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
  , YesodVideo
    ( getAppSettings, getRtcPeerConnectionConfig, getAppHttpManager
    , getVapidKeys, getStaticRoute
    )
  , wsApp
  , getWebSoketR
  , postPushMessageR
  , getHomeRoute
  ) where

import Conduit ((.|), mapM_C, runConduit, MonadIO (liftIO))

import Control.Lens ((.~), (?~))
import Control.Monad (forever, forM_)

import Database.Esqueleto.Experimental
    ( selectOne, select, from, table, where_, val
    , (^.), (==.)
    )
import Database.Persist (Entity (Entity))
import Database.Persist.Sql (SqlBackend, fromSqlKey)

import Data.Aeson (object, (.=))
import qualified Data.Aeson as A (Value)
import Data.Function ((&))
import qualified Data.Map as M ( lookup, insert, alter )
import Data.Maybe (fromMaybe)
import Data.Text (unpack, pack)
import Data.Text.Encoding (encodeUtf8)

import Model
    ( paramBacklink
    , UserId, User (User), UserPhoto (UserPhoto)
    , PatientId, PushSubscription (PushSubscription)
    , PushMsgType
      ( PushMsgTypeEnd
      )
    , EntityField
      ( PushSubscriptionSubscriber, UserPhotoUser, UserId
      )
    )

import Network.HTTP.Client (Manager)

import UnliftIO.Exception (try, SomeException)
import UnliftIO.STM
    (atomically, readTVarIO, writeTVar, newTQueue, readTQueue, writeTQueue)

import Settings
    ( widgetFile, AppSettings (appSuperuser)
    , Superuser (Superuser, superuserUsername)
    )
import Settings.StaticFiles
    ( img_call_end_24dp_FILL0_wght400_GRAD0_opsz24_svg )

import Text.Hamlet (Html)
import Text.Julius (RawJS(rawJS))
import Text.Read (readMaybe)
import Text.Shakespeare.Text (st)

import VideoRoom.Data
    ( resourcesVideoRoom, channelMapTVar
    , VideoRoom (VideoRoom), ChanId (ChanId)
    , Route (WebSoketR, PushMessageR, PhotoR, RoomR, AudioR)
    , VideoRoomMessage
      ( MsgNotGeneratedVAPID, MsgVideoSession, MsgAudioSession, MsgClose
      , MsgCallEnded, MsgAppName, MsgUserCallIsOver, MsgBack, MsgUnknown
      , MsgUserOnCall
      )
    )

import Web.WebPush
    ( VAPIDKeys, PushTopic (PushTopic), PushUrgency (PushUrgencyHigh)
    , mkPushNotification, pushMessage, pushSenderEmail, pushExpireInSeconds
    , sendPushNotification, pushUrgency, pushTopic
    )

import Yesod
    ( Yesod, YesodSubDispatch, yesodSubDispatch , mkYesodSubDispatch
    , SubHandlerFor, MonadHandler (liftHandler) , getSubYesod
    , Application, newIdent , YesodPersist (YesodPersistBackend)
    , RenderMessage , FormMessage, HandlerFor, lookupPostParam, getRouteToParent
    , getMessageRender
    )
import Yesod.Core (defaultLayout)
import Yesod.Core.Content (TypedContent (TypedContent), toContent)
import Yesod.Core.Handler (invalidArgsI, getUrlRender)
import Yesod.Core.Types (YesodSubRunnerEnv)
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Fields (urlField)
import Yesod.Persist.Core (runDB)
import Yesod.WebSockets
    ( WebSocketsT, sendTextData, race_, sourceWS, webSockets)
import Yesod.Static (StaticRoute)



class YesodVideo m where
    getHomeRoute :: HandlerFor m (Route m)
    getAppSettings :: HandlerFor m AppSettings
    getStaticRoute :: StaticRoute -> HandlerFor m (Route m)
    getRtcPeerConnectionConfig :: HandlerFor m (Maybe A.Value)
    getAppHttpManager :: HandlerFor m Manager
    getVapidKeys :: HandlerFor m (Maybe VAPIDKeys)


getAudioR :: (Yesod m, YesodVideo m)
          => (YesodPersist m, YesodPersistBackend m ~ SqlBackend)
          => (RenderMessage m VideoRoomMessage, RenderMessage m FormMessage)
          => UserId -> PatientId -> UserId -> Bool -> SubHandlerFor VideoRoom m Html
getAudioR sid pid rid polite = do

    let channelId = ChanId (fromIntegral $ fromSqlKey pid)
    backlink <- do
        link <- runInputGet (iopt urlField paramBacklink)
        home <- liftHandler getHomeRoute
        rndr <- getUrlRender
        return $ fromMaybe (rndr home) link

    toParent <- getRouteToParent

    config <- liftHandler $ fromMaybe (object []) <$> getRtcPeerConnectionConfig

    iconCallEnd <- liftHandler $ getStaticRoute img_call_end_24dp_FILL0_wght400_GRAD0_opsz24_svg

    let extractName (Entity _ (User email _ _ _ _ name _ _)) = fromMaybe email name

    terminator <- liftHandler $ (extractName <$>) <$> runDB ( selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val sid
        return x )
    
    callerName <- liftHandler $ (extractName <$>) <$> runDB ( selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val rid
        return x )

    msgr <- liftHandler getMessageRender
    liftHandler $ defaultLayout $ do
        idButtonExitAudioSession <- newIdent
        idImgPosterRemote <- newIdent
        idAudioRemote <- newIdent
        idImgPosterSelf <- newIdent
        idAudioSelf <- newIdent
        idButtonEndAudioSession <- newIdent
        idDialogCallEnded <- newIdent
        $(widgetFile "audio/session")


getRoomR :: (Yesod m, YesodVideo m)
         => (YesodPersist m, YesodPersistBackend m ~ SqlBackend)
         => (RenderMessage m VideoRoomMessage, RenderMessage m FormMessage)
         => UserId -> PatientId -> UserId -> Bool -> SubHandlerFor VideoRoom m Html
getRoomR sid pid rid polite = do

    let channelId = ChanId (fromIntegral $ fromSqlKey pid)
    backlink <- do
        link <- runInputGet (iopt urlField paramBacklink)
        home <- liftHandler getHomeRoute
        rndr <- getUrlRender
        return $ fromMaybe (rndr home) link

    toParent <- getRouteToParent

    config <- liftHandler $ fromMaybe (object []) <$> getRtcPeerConnectionConfig

    iconCallEnd <- liftHandler $ getStaticRoute img_call_end_24dp_FILL0_wght400_GRAD0_opsz24_svg

    let extractName (Entity _ (User email _ _ _ _ name _ _)) = fromMaybe email name

    terminator <- liftHandler $ (extractName <$>) <$> runDB ( selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val sid
        return x )

    msgr <- liftHandler getMessageRender
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
                 => (RenderMessage m VideoRoomMessage, RenderMessage m FormMessage)
                 => UserId -> UserId -> SubHandlerFor VideoRoom m ()
postPushMessageR sid rid = do

    messageType <- (readMaybe @PushMsgType . unpack =<<) <$> lookupPostParam "messageType"
    icon <- lookupPostParam "icon"
    image <- lookupPostParam "image"
    body <- lookupPostParam "body"
    targetRoom <- lookupPostParam "targetRoom"
    targetPush <- lookupPostParam "targetPush"

    subscriptions <- liftHandler $ runDB $ select $ do
        x <- from $ table @PushSubscription
        where_ $ x ^. PushSubscriptionSubscriber ==. val rid
        return x

    manager <- liftHandler getAppHttpManager
    vapidKeys <- liftHandler getVapidKeys

    msgr <- getMessageRender

    case vapidKeys of
      Just vapid -> do
          Superuser {..} <- liftHandler $ appSuperuser <$> getAppSettings

          forM_ subscriptions $ \(Entity _ (PushSubscription _ _ endpoint p256dh auth)) -> do
                let notification = mkPushNotification endpoint p256dh auth
                        & pushMessage .~ object [ "messageType" .= messageType
                                                , "title" .= msgr MsgAppName
                                                , "icon" .= icon
                                                , "image" .= image
                                                , "body" .= body
                                                , "targetRoom" .= targetRoom
                                                , "targetPush" .= targetPush
                                                ]
                        & pushSenderEmail .~ superuserUsername
                        & pushExpireInSeconds .~ 60 * 60
                        & pushUrgency ?~ PushUrgencyHigh
                        & pushTopic .~ (PushTopic . pack . show <$> messageType)

                result <- sendPushNotification vapid manager notification

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

instance ( Yesod m, YesodVideo m
         , YesodPersist m, YesodPersistBackend m ~ SqlBackend
         , RenderMessage m VideoRoomMessage, RenderMessage m FormMessage
         ) => YesodSubDispatch VideoRoom m where
    yesodSubDispatch :: YesodSubRunnerEnv VideoRoom m -> Application
    yesodSubDispatch = $(mkYesodSubDispatch resourcesVideoRoom)
