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
  , getDoctorVideoRoomR
  , getPatientVideoRoomR
  , postPushMessageR
  ) where

import VideoRoom.Data
    ( resourcesVideoRoom, channelMapTVar
    , VideoRoom (VideoRoom, rtcPeerConnectionConfig, httpManager)
    , Route (DoctorVideoRoomR, PatientVideoRoomR, PushMessageR)
    )

import Conduit ((.|), mapM_C, runConduit, MonadIO (liftIO))

import Control.Applicative ((<|>))
import Control.Lens ((.~))
import Control.Monad (forever, forM_)

import Database.Esqueleto.Experimental
    ( selectOne, Value (unValue), from, table, where_, val
    , (^.), (==.), Entity (entityVal), select
    )
import Database.Persist (Entity (Entity))
import Database.Persist.Sql (fromSqlKey)

import Data.Aeson (object, (.=), ToJSON (toJSON), Value (String))
import Data.Bifunctor (Bifunctor(bimap))
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import qualified Data.Map as M ( lookup, insert, alter )
import Data.Text (Text, pack, unpack)

import Foundation
    ( App, Route (MyDoctorR, MyPatientR, MyDoctorPhotoR)
    , AppMessage
      ( MsgBack, MsgVideoCall, MsgPhoto, MsgOutgoingCall, MsgNotGeneratedVAPID
      )
    )

import Model
    ( DoctorId, PatientId, UserId, DoctorPhoto, User (userEmail, userName)
    , PushSubscription (PushSubscription), Token
    , StoreType (StoreTypeGoogleSecretManager, StoreTypeDatabase, StoreTypeSession)
    , Store, apiInfoVapid, secretVolumeVapid
    , EntityField
      ( DoctorPhotoDoctor, DoctorPhotoAttribution, UserId, PushSubscriptionUser
      , TokenApi, TokenId, TokenStore, StoreToken, StoreVal
      )
    )

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
    ( Yesod (defaultLayout), YesodSubDispatch, yesodSubDispatch
    , mkYesodSubDispatch, SubHandlerFor, Html, MonadHandler (liftHandler)
    , getSubYesod, setTitleI, Application, newIdent, invalidArgsI
    )
import Yesod.Core.Types (YesodSubRunnerEnv)
import Yesod.Form.Input (iopt, runInputPost)
import Yesod.Form.Fields (textField)
import Yesod.Persist.Core (runDB)
import Yesod.WebSockets
    ( WebSocketsT, sendTextData, race_, sourceWS, webSockets)


data MessageType = MessageTypeCall | MessageTypeAccept | MessageTypeDecline 
    deriving (Eq, Show, Read)


instance ToJSON MessageType where
    toJSON :: MessageType -> Data.Aeson.Value
    toJSON MessageTypeCall = String "Call"
    toJSON MessageTypeAccept = String "Accept"
    toJSON MessageTypeDecline = String "Decline"


postPushMessageR :: UserId -> UserId -> SubHandlerFor VideoRoom App ()
postPushMessageR sid rid = do

    messageType <- (\x -> x <|> Just MessageTypeCall) . (readMaybe . unpack =<<)
        <$> runInputPost (iopt textField "messageType")
    
    sender <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val sid
        return x

    subscriptions <- liftHandler $ runDB $ select $ do
        x <- from $ table @PushSubscription
        where_ $ x ^. PushSubscriptionUser ==. val rid
        return x

    manager <- httpManager <$> getSubYesod

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
                                                , "senderId" .= sid
                                                , "recipientId" .= rid
                                                , "senderName" .= (userName . entityVal <$> sender)
                                                , "senderEmail" .= (userEmail . entityVal <$> sender)
                                                , "senderEndpoint" .= endpoint
                                                , "senderP256dh" .= p256dh
                                                , "senderAuth" .= auth
                                                ]
                        & pushSenderEmail .~ ("ciukstar@gmail.com" :: Text)
                        & pushExpireInSeconds .~ 60 * 60

                result <- sendPushNotification vapidKeys manager notification

                case result of
                  Left ex -> do
                      liftIO $ print ex
                  Right () -> return ()

      Nothing -> liftHandler $ invalidArgsI [MsgNotGeneratedVAPID]


userJoinedChannel :: Num b => Maybe (a,b) -> Maybe (a,b)
userJoinedChannel Nothing = Nothing
userJoinedChannel (Just (writeChan,numUsers)) = Just (writeChan,numUsers + 1)


userLeftChannel :: Num b => Maybe (a,b) -> Maybe (a,b)
userLeftChannel Nothing = Nothing
userLeftChannel (Just (writeChan,numUsers)) = Just (writeChan,numUsers - 1)


videoApp :: PatientId -> Bool -> WebSocketsT (SubHandlerFor VideoRoom App) ()
videoApp pid polite = do

    let channelId = pack $ show $ fromSqlKey pid

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



getDoctorVideoRoomR :: PatientId -> UserId -> DoctorId -> SubHandlerFor VideoRoom App Html
getDoctorVideoRoomR pid uid did = do
    let polite = True

    webSockets (videoApp pid polite)

    config <- fromMaybe (object []) . rtcPeerConnectionConfig <$> getSubYesod

    attrib <- liftHandler $ (unValue =<<) <$> runDB ( selectOne $ do
        x <- from $ table @DoctorPhoto
        where_ $ x ^. DoctorPhotoDoctor ==. val did
        return (x ^. DoctorPhotoAttribution) )

    liftHandler $ defaultLayout $ do
        setTitleI MsgVideoCall
        idOutgoingCall <- newIdent
        idVideoRemote <- newIdent
        idVideoSelf <- newIdent
        $(widgetFile "my/doctors/video/video")



getPatientVideoRoomR :: PatientId -> DoctorId -> UserId -> SubHandlerFor VideoRoom App Html
getPatientVideoRoomR pid did uid = do
    let polite = False

    webSockets (videoApp pid polite)

    config <- fromMaybe (object []) . rtcPeerConnectionConfig <$> getSubYesod

    liftHandler $ defaultLayout $ do
        setTitleI MsgVideoCall
        $(widgetFile "my/patients/video/video")
        


instance YesodSubDispatch VideoRoom App where
    yesodSubDispatch :: YesodSubRunnerEnv VideoRoom App -> Application
    yesodSubDispatch = $(mkYesodSubDispatch resourcesVideoRoom)
