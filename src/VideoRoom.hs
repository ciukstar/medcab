{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module VideoRoom
  ( module VideoRoom.Data
  , YesodVideo (getRtcPeerConnectionConfig, getAppHttpManager)
  , wsApp
  , getWebSoketR
  , postPushMessageR
  , widgetOutgoingCall
  ) where

import VideoRoom.Data
    ( resourcesVideoRoom, channelMapTVar
    , VideoRoom (VideoRoom), ChanId (ChanId)
    , Route (WebSoketR, PushMessageR)
    , VideoRoomMessage (VideoRoomOutgoingCall, VideoRoomIncomingCall)
    )

import Conduit ((.|), mapM_C, runConduit, MonadIO (liftIO))

import Control.Applicative ((<|>))
import Control.Lens ((.~))
import Control.Monad (forever, forM_)

import Database.Esqueleto.Experimental
    ( selectOne, Value (unValue), from, table, where_, val
    , (^.), (==.)
    , Entity (entityVal), select, toSqlKey, SqlBackend, just
    )
import Database.Persist (Entity (Entity))

import Data.Aeson (object, (.=))
import qualified Data.Aeson as A (Value)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import qualified Data.Map as M ( lookup, insert, alter )
import Data.Text (Text, unpack)

import Model
    ( User (userEmail, userName)
    , PushSubscription (PushSubscription), Token
    , StoreType (StoreTypeGoogleSecretManager, StoreTypeDatabase, StoreTypeSession)
    , Store, apiInfoVapid, secretVolumeVapid
    , PushMsgType
      ( PushMsgTypeCall, PushMsgTypeAccept, PushMsgTypeDecline, PushMsgTypeEnd
      )
    , EntityField
      ( UserId, PushSubscriptionUser
      , TokenApi, TokenId, TokenStore, StoreToken, StoreVal
      )
    )
    
import Network.HTTP.Client (Manager)

import UnliftIO.Exception (try, SomeException)
import UnliftIO.STM
    (atomically, readTVarIO, writeTVar, newTQueue, readTQueue, writeTQueue)

import Settings (widgetFile)

import System.IO (readFile')

import Text.Read (readMaybe)

import Web.WebPush
    ( VAPIDKeysMinDetails(VAPIDKeysMinDetails), readVAPIDKeys, mkPushNotification
    , pushMessage, pushSenderEmail, pushExpireInSeconds, sendPushNotification
    )

import Yesod
    ( Yesod, YesodSubDispatch, yesodSubDispatch , mkYesodSubDispatch
    , SubHandlerFor, MonadHandler (liftHandler) , getSubYesod
    , Application, newIdent , YesodPersist (YesodPersistBackend)
    , RenderMessage , FormMessage, HandlerFor, lookupPostParam
    )
import Yesod.Core.Types (YesodSubRunnerEnv)
import Yesod.Core.Widget (WidgetFor)
import Yesod.Persist.Core (runDB)
import Yesod.WebSockets
    ( WebSocketsT, sendTextData, race_, sourceWS, webSockets)


class YesodVideo m where
    getRtcPeerConnectionConfig :: HandlerFor m (Maybe A.Value)
    getAppHttpManager :: HandlerFor m Manager


postPushMessageR :: (Yesod m, YesodVideo m)
                 => (YesodPersist m, YesodPersistBackend m ~ SqlBackend)
                 => (RenderMessage m FormMessage)
                 => SubHandlerFor VideoRoom m ()
postPushMessageR = do
    
    messageType <- (\x -> x <|> Just PushMsgTypeCall) . (readMaybe . unpack =<<)
        <$> lookupPostParam "messageType"
    icon <- lookupPostParam "icon"
    channelId <- ((ChanId <$>) . readMaybe . unpack =<<) <$> lookupPostParam "channelId"
    polite <- (readMaybe . unpack =<<) <$> lookupPostParam "polite"
    ws <- lookupPostParam "ws"
    sid <- ((toSqlKey <$>) . readMaybe . unpack =<<) <$> lookupPostParam "senderId"
    senderPhoto <- lookupPostParam "senderPhoto"
    rid <- ((toSqlKey <$>) . readMaybe . unpack =<<) <$> lookupPostParam "recipientId"

    sender <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ just (x ^. UserId) ==. val sid
        return x

    subscriptions <- liftHandler $ runDB $ select $ do
        x <- from $ table @PushSubscription
        where_ $ just (x ^. PushSubscriptionUser) ==. val rid
        return x

    manager <- liftHandler getAppHttpManager

    storeType <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val apiInfoVapid
        return (x ^. TokenId, x ^. TokenStore) )

    let readTriple (s,x,y) = VAPIDKeysMinDetails s x y

    details <- case storeType of
      Just (_, StoreTypeGoogleSecretManager) -> do
          liftIO $ (readTriple <$>) . readMaybe <$> readFile' secretVolumeVapid

      Just (tid, StoreTypeDatabase) -> do
          liftHandler $ ((readTriple <$>) . readMaybe . unpack . unValue =<<) <$> runDB ( selectOne $ do
              x <-from $ table @Store
              where_ $ x ^. StoreToken ==. val tid
              return $ x ^. StoreVal )

      Just (_,StoreTypeSession) -> return Nothing
      Nothing -> return Nothing

    case details of
      Just vapidKeysMinDetails -> do
          let vapidKeys = readVAPIDKeys vapidKeysMinDetails
          
          forM_ subscriptions $ \(Entity _ (PushSubscription _ endpoint p256dh auth)) -> do
                let notification = mkPushNotification endpoint p256dh auth
                        & pushMessage .~ object [ "messageType" .= messageType
                                                , "icon" .= icon
                                                , "channelId" .= channelId
                                                , "ws" .= ws
                                                , "polite" .= (polite :: Maybe Bool)
                                                , "senderId" .= sid
                                                , "senderName" .= (userName . entityVal <$> sender)
                                                , "senderEmail" .= (userEmail . entityVal <$> sender)
                                                , "senderPhoto" .= senderPhoto
                                                , "recipientId" .= rid
                                                ]
                        & pushSenderEmail .~ ("ciukstar@gmail.com" :: Text)
                        & pushExpireInSeconds .~ 60 * 60

                result <- sendPushNotification vapidKeys manager notification

                case result of
                  Left ex -> do
                      liftIO $ print ex
                  Right () -> return ()

      Nothing -> return () -- liftHandler $ invalidArgsI [MsgNotGeneratedVAPID]


userJoinedChannel :: Num b => Maybe (a,b) -> Maybe (a,b)
userJoinedChannel Nothing = Nothing
userJoinedChannel (Just (writeChan,numUsers)) = Just (writeChan,numUsers + 1)


userLeftChannel :: Num b => Maybe (a,b) -> Maybe (a,b)
userLeftChannel Nothing = Nothing
userLeftChannel (Just (writeChan,numUsers)) = Just (writeChan,numUsers - 1)


wsApp :: ChanId -> Bool -> WebSocketsT (SubHandlerFor VideoRoom m) ()
wsApp channelId polite = do 

    VideoRoom {..} <- getSubYesod

    channelMap <- readTVarIO channelMapTVar

    let maybeChan = M.lookup channelId channelMap

    (chan,peer) <- atomically $ case maybeChan of
      Nothing -> do
          chan <- newTQueue
          peer <- newTQueue
          writeTVar channelMapTVar $ M.insert channelId ((chan,peer),1) channelMap
          return (chan,peer)
      Just (chan,_) -> do
          writeTVar channelMapTVar $ M.alter userJoinedChannel channelId channelMap
          return chan

    (e :: Either SomeException ()) <- try $ race_
        (forever $ atomically (readTQueue (if polite then chan else peer)) >>= sendTextData)
        (runConduit (sourceWS .| mapM_C (
                          \msg -> do
                              liftIO $ print msg
                              atomically $ writeTQueue (if not polite then chan else peer) msg
                          )
                    ))
    case e of
      Left _ -> do
          m <- readTVarIO channelMapTVar
          let newChannelMap = M.alter userLeftChannel channelId m
          atomically $ writeTVar channelMapTVar newChannelMap
      Right () -> return ()


widgetOutgoingCall :: (YesodVideo m, RenderMessage m VideoRoomMessage)
                   => ChanId -- ^ Channel id
                   -> Text -- ^ Dialog id Outgoing
                   -> Text -- ^ Button id for cancelig outgoing call
                   -> Text -- ^ Button id for ending call
                   -> (Route VideoRoom -> Route m)
                   -> WidgetFor m ()
widgetOutgoingCall channelId idDialogOutgoingCall idButtonOutgoingCallCancel idButtonEndVideoSession toParent = do

    let polite = True
    
    config <- liftHandler $ fromMaybe (object []) <$> getRtcPeerConnectionConfig

    idDialogVideoSession <- newIdent
    idVideoRemote <- newIdent
    idVideoSelf <- newIdent

    idDialogCallDeclined <- newIdent
    idDialogVideoSessionEnded <- newIdent
    
    $(widgetFile "video/outgoing")


getWebSoketR :: (Yesod m, YesodPersist m, YesodPersistBackend m ~ SqlBackend)
             => ChanId -> Bool -> SubHandlerFor VideoRoom m ()
getWebSoketR channelId polite = webSockets (wsApp channelId polite)


instance ( Yesod m, YesodVideo m, RenderMessage m VideoRoomMessage
         , YesodPersist m, YesodPersistBackend m ~ SqlBackend
         , RenderMessage m FormMessage
         ) => YesodSubDispatch VideoRoom m where
    yesodSubDispatch :: YesodSubRunnerEnv VideoRoom m -> Application
    yesodSubDispatch = $(mkYesodSubDispatch resourcesVideoRoom)
