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
  ) where

import Conduit ((.|), mapM_C, runConduit, MonadIO (liftIO))

import Control.Applicative ((<|>))
import Control.Lens ((.~))
import Control.Monad (forever, forM_)

import Database.Esqueleto.Experimental
    ( selectOne, Value (unValue), from, table, where_, val
    , (^.), (==.)
    , select, just
    )
import Database.Persist (Entity (Entity, entityVal))
import Database.Persist.Sql (SqlBackend, fromSqlKey, toSqlKey)

import Data.Aeson (object, (.=))
import qualified Data.Aeson as A (Value)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import qualified Data.Map as M ( lookup, insert, alter )
import Data.Text (Text, unpack)

import Model
    ( UserId, User (userEmail, userName)
    , PushSubscription (PushSubscription), Token
    , StoreType (StoreTypeGoogleSecretManager, StoreTypeDatabase, StoreTypeSession)
    , Store, apiInfoVapid, secretVolumeVapid
    , PushMsgType
      ( PushMsgTypeCall, PushMsgTypeAccept, PushMsgTypeDecline, PushMsgTypeEnd
      )
    , EntityField
      ( UserId, PushSubscriptionUser
      , TokenApi, TokenId, TokenStore, StoreToken, StoreVal, UserPhotoUser
      ), UserPhoto (UserPhoto), PatientId, paramBacklink
    )

import Network.HTTP.Client (Manager)

import UnliftIO.Exception (try, SomeException)
import UnliftIO.STM
    (atomically, readTVarIO, writeTVar, newTQueue, readTQueue, writeTQueue)

import Settings (widgetFile)

import System.IO (readFile')

import Text.Hamlet (Html)
import Text.Julius (RawJS(rawJS))
import Text.Read (readMaybe)
import Text.Shakespeare.Text (st)

import VideoRoom.Data
    ( resourcesVideoRoom, channelMapTVar
    , VideoRoom (VideoRoom), ChanId (ChanId)
    , Route (WebSoketR, PushMessageR, PhotoR, RoomR)
    , VideoRoomMessage
      ( VideoRoomOutgoingCall, VideoRoomIncomingCall, VideoRoomClose
      , VideoRoomCallEnded, VideoRoomVideoSession, VideoRoomNotGeneratedVAPID
      )
    )

import Web.WebPush
    ( VAPIDKeysMinDetails(VAPIDKeysMinDetails), readVAPIDKeys, mkPushNotification
    , pushMessage, pushSenderEmail, pushExpireInSeconds, sendPushNotification
    )

import Yesod
    ( Yesod, YesodSubDispatch, yesodSubDispatch , mkYesodSubDispatch
    , SubHandlerFor, MonadHandler (liftHandler) , getSubYesod
    , Application, newIdent , YesodPersist (YesodPersistBackend)
    , RenderMessage , FormMessage, HandlerFor, lookupPostParam, getRouteToParent
    , getCurrentRoute
    )
import Yesod.Core (defaultLayout)
import Yesod.Core.Content (TypedContent (TypedContent), toContent)
import Yesod.Core.Handler (invalidArgsI, getUrlRender)
import Yesod.Core.Types (YesodSubRunnerEnv)
import Yesod.Core.Widget (WidgetFor)
import Yesod.Form.Input (runInputGet, ireq)
import Yesod.Form.Fields (intField, urlField)
import Yesod.Persist.Core (runDB)
import Yesod.WebSockets
    ( WebSocketsT, sendTextData, race_, sourceWS, webSockets)
import Data.Text.Encoding (encodeUtf8)


class YesodVideo m where
    getRtcPeerConnectionConfig :: HandlerFor m (Maybe A.Value)
    getAppHttpManager :: HandlerFor m Manager


getRoomR :: (Yesod m, YesodVideo m)
         => (RenderMessage m VideoRoomMessage, RenderMessage m FormMessage)
         => UserId -> PatientId -> UserId -> Bool -> SubHandlerFor VideoRoom m Html
getRoomR sid pid rid polite = do
    
    -- let polite = True

    channelId@(ChanId channel) <- ChanId <$> runInputGet (ireq intField "channel")
    backlink <- runInputGet (ireq urlField paramBacklink)

    toParent <- getRouteToParent

    config <- liftHandler $ fromMaybe (object []) <$> getRtcPeerConnectionConfig

    liftHandler $ defaultLayout $ do
        idButtonExitVideoSession <- newIdent
        idVideoRemote <- newIdent
        idVideoSelf <- newIdent
        idButtonEndVideoSession <- newIdent
        idDialogCallEnded <- newIdent
        $(widgetFile "video/session")


getOutgoingR :: (Yesod m, YesodVideo m)
             => (RenderMessage m VideoRoomMessage, RenderMessage m FormMessage)
             => UserId -> UserId -> SubHandlerFor VideoRoom m Html
getOutgoingR sid rid = do

    let polite = True

    channelId@(ChanId channel) <- ChanId <$> runInputGet (ireq intField "channel")
    backlink <- runInputGet (ireq urlField "backlink")

    toParent <- getRouteToParent

    config <- liftHandler $ fromMaybe (object []) <$> getRtcPeerConnectionConfig

    liftHandler $ defaultLayout $ do
        idButtonExitVideoSession <- newIdent
        idVideoRemote <- newIdent
        idVideoSelf <- newIdent
        idButtonEndVideoSession <- newIdent
        idDialogCallEnded <- newIdent
        $(widgetFile "video/session")


getIncomingR :: (Yesod m, YesodVideo m)
             => (YesodPersist m, YesodPersistBackend m ~ SqlBackend)
             => (RenderMessage m VideoRoomMessage, RenderMessage m FormMessage)
             => SubHandlerFor VideoRoom m Html
getIncomingR = do

    let polite = False

    (sid :: UserId) <- toSqlKey <$> runInputGet (ireq intField "senderId")
    (rid :: UserId) <- toSqlKey <$> runInputGet (ireq intField "recipientId")

    channelId@(ChanId channel) <- ChanId <$> runInputGet (ireq intField "channel")
    backlink <- runInputGet (ireq urlField "backlink")

    toParent <- getRouteToParent

    config <- liftHandler $ fromMaybe (object []) <$> getRtcPeerConnectionConfig

    liftHandler $ defaultLayout $ do
        idButtonExitVideoSession <- newIdent
        idVideoRemote <- newIdent
        idVideoSelf <- newIdent
        idButtonEndVideoSession <- newIdent
        idDialogCallEnded <- newIdent
        $(widgetFile "video/session")


getWebSoketR :: (Yesod m, YesodPersist m, YesodPersistBackend m ~ SqlBackend)
             => ChanId -> Bool -> SubHandlerFor VideoRoom m ()
getWebSoketR channelId polite = webSockets (wsApp channelId polite)


postPushMessageR :: (Yesod m, YesodVideo m)
                 => (YesodPersist m, YesodPersistBackend m ~ SqlBackend)
                 => (RenderMessage m FormMessage, RenderMessage m VideoRoomMessage)
                 => SubHandlerFor VideoRoom m ()
postPushMessageR = do

    messageType <- (\x -> x <|> Just PushMsgTypeCall) . (readMaybe . unpack =<<)
        <$> lookupPostParam "messageType"
    icon <- lookupPostParam "icon"
    targetRoom <- lookupPostParam "targetRoom"
    channelId <- ((ChanId <$>) . readMaybe . unpack =<<) <$> lookupPostParam "channelId"
    sid <- ((toSqlKey <$>) . readMaybe . unpack =<<) <$> lookupPostParam "senderId"
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

    toParent <- getRouteToParent
    urlRender <- getUrlRender

    case details of
      Just vapidKeysMinDetails -> do
          let vapidKeys = readVAPIDKeys vapidKeysMinDetails

          forM_ subscriptions $ \(Entity _ (PushSubscription _ endpoint p256dh auth)) -> do
                let notification = mkPushNotification endpoint p256dh auth
                        & pushMessage .~ object [ "messageType" .= messageType
                                                , "topic" .= messageType
                                                , "icon" .= icon
                                                , "targetRoom" .= targetRoom
                                                , "channelId" .= channelId
                                                , "senderId" .= sid
                                                , "senderName" .= ( (userName . entityVal <$> sender)
                                                                    <|> (Just . userEmail . entityVal <$> sender)
                                                                  )
                                                , "senderPhoto" .= (urlRender . toParent . PhotoR <$> sid)
                                                , "recipientId" .= rid
                                                ]
                        & pushSenderEmail .~ ("ciukstar@gmail.com" :: Text)
                        & pushExpireInSeconds .~ 60 * 60

                result <- sendPushNotification vapidKeys manager notification

                case result of
                  Left ex -> do
                      liftIO $ print ex
                  Right () -> return ()

      Nothing -> liftHandler $ invalidArgsI [VideoRoomNotGeneratedVAPID]


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


getPhotoR :: (YesodPersist m, YesodPersistBackend m ~ SqlBackend)
          => UserId -> SubHandlerFor VideoRoom m TypedContent
getPhotoR uid = do
    photo <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @UserPhoto
        where_ $ x ^. UserPhotoUser ==. val uid
        return x
    return $ case photo of
      Just (Entity _ (UserPhoto _ mime bs _)) -> TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> TypedContent "image/svg+xml" $ toContent [st|<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 -960 960 960" width="24"><path d="M480-480q-66 0-113-47t-47-113q0-66 47-113t113-47q66 0 113 47t47 113q0 66-47 113t-113 47ZM160-160v-112q0-34 17.5-62.5T224-378q62-31 126-46.5T480-440q66 0 130 15.5T736-378q29 15 46.5 43.5T800-272v112H160Zm80-80h480v-32q0-11-5.5-20T700-306q-54-27-109-40.5T480-360q-56 0-111 13.5T260-306q-9 5-14.5 14t-5.5 20v32Zm240-320q33 0 56.5-23.5T560-640q0-33-23.5-56.5T480-720q-33 0-56.5 23.5T400-640q0 33 23.5 56.5T480-560Zm0-80Zm0 400Z"/></svg>|]


instance ( Yesod m, YesodVideo m, RenderMessage m VideoRoomMessage
         , YesodPersist m, YesodPersistBackend m ~ SqlBackend
         , RenderMessage m FormMessage
         ) => YesodSubDispatch VideoRoom m where
    yesodSubDispatch :: YesodSubRunnerEnv VideoRoom m -> Application
    yesodSubDispatch = $(mkYesodSubDispatch resourcesVideoRoom)
