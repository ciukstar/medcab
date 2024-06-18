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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}


module ChatRoom
  ( module ChatRoom.Data
  , getDoctorChatRoomR
  , YesodChat
    ( getStaticRoute, getMyDoctorRoute, getMyPatientRoute, getDoctorPhotoRoute
    , getAccountPhotoRoute, getVapidKeys, getAppHttpManager, getAppSettings
    , getChatRoute
    )
  ) where

import ChatRoom.Data
    ( ChatRoom (ChatRoom), resourcesChatRoom
    , Route (DoctorChatRoomR, PatientChatRoomR)
    , ChatRoomMessage 
      ( MsgBack, MsgChat, MsgPhoto, MsgMessage, MsgChatParticipantsNotDefined
      , MsgAppName, MsgPushNotificationExcception
      )
    )

import Conduit ((.|), mapM_C, runConduit, MonadIO (liftIO))

import Control.Concurrent.STM.TChan
    ( writeTChan, dupTChan, readTChan, newBroadcastTChan )
import Control.Lens ((.~), (?~))
import Control.Monad (forever, forM_)

import Data.Aeson (object, (.=))
import Data.Function ((&))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val, innerJoin, on, update, set
    , (^.), (==.), (:&) ((:&)), (||.), (&&.), (=.)
    , just, SqlBackend, select, orderBy, subSelect, desc
    )
import Database.Persist
    ( Entity (Entity), PersistStoreWrite (insert), entityVal )
import Database.Persist.Sql (fromSqlKey)

import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Map as M ( Map, lookup, insert, alter, fromListWith, toList )
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)

import Model
    ( AvatarColor (AvatarColorDark), statusError
    , ChatMessageStatus (ChatMessageStatusRead, ChatMessageStatusUnread)
    , DoctorId, Doctor (Doctor)
    , PatientId, Patient
    , UserId, User (User), userEmail, userName
    , Chat (Chat)
    , PushSubscription (PushSubscription)
    , PushMsgType (PushMsgTypeChat)
    , EntityField
      ( DoctorId, PatientDoctor, PatientId, PatientUser, UserId, DoctorUser
      , ChatTimemark, ChatUser, ChatInterlocutor, ChatStatus, ChatNotified
      , PushSubscriptionPublisher, PushSubscriptionSubscriber, ChatId
      )
    )

import Network.HTTP.Client (Manager)
import Network.HTTP.Types (extractPath)

import UnliftIO.Exception (try, SomeException)
import UnliftIO.STM (atomically, readTVarIO, writeTVar)

import Settings
    ( widgetFile, AppSettings (appSuperuser)
    , Superuser (Superuser, superuserUsername)
    )
import Settings.StaticFiles
    ( ringtones_outgoing_chat_mp3, ringtones_incoming_chat_mp3
    , img_chat_FILL0_wght400_GRAD0_opsz24_svg
    )

import Text.Shakespeare.I18N (RenderMessage)

import Web.WebPush
    ( mkPushNotification
    , pushMessage, pushSenderEmail, pushExpireInSeconds
    , sendPushNotification, pushTopic, PushTopic (PushTopic), pushUrgency
    , PushUrgency (PushUrgencyHigh), VAPIDKeys
    )

import Yesod
    ( Yesod (defaultLayout), YesodSubDispatch, yesodSubDispatch
    , mkYesodSubDispatch, SubHandlerFor, Html, MonadHandler (liftHandler)
    , getSubYesod, setTitleI, Application, YesodPersist (YesodPersistBackend)
    , invalidArgsI
    )
import Yesod.Core.Handler
    ( newIdent, HandlerFor, getUrlRender, getMessageRender
    , addMessageI
    )
import Yesod.Core.Types (YesodSubRunnerEnv)
import Yesod.Form.Types (FormMessage)
import Yesod.Persist (YesodPersist(runDB))
import Yesod.Static (StaticRoute)
import Yesod.WebSockets
    ( WebSocketsT, sendTextData, race_, sourceWS, webSockets)


class YesodChat m where
    getChatRoute :: UserId -> Route ChatRoom -> HandlerFor m (Route m)
    getAppSettings :: HandlerFor m AppSettings
    getStaticRoute :: StaticRoute -> HandlerFor m (Route m)
    getMyDoctorRoute :: PatientId -> UserId -> DoctorId -> HandlerFor m (Route m)
    getMyPatientRoute :: UserId -> DoctorId -> PatientId -> HandlerFor m (Route m)
    getDoctorPhotoRoute :: DoctorId -> HandlerFor m (Route m)
    getAccountPhotoRoute :: UserId -> AvatarColor -> HandlerFor m (Route m)
    getAppHttpManager :: HandlerFor m Manager
    getVapidKeys :: HandlerFor m (Maybe VAPIDKeys)


userJoinedChannel :: Num b => Maybe (a,b) -> Maybe (a,b)
userJoinedChannel Nothing = Nothing
userJoinedChannel (Just (writeChan,numUsers)) = Just (writeChan,numUsers + 1)


userLeftChannel :: Num b => Maybe (a,b) -> Maybe (a,b)
userLeftChannel Nothing = Nothing
userLeftChannel (Just (writeChan,numUsers)) = Just (writeChan,numUsers - 1)


chatApp :: YesodChat m
        => (YesodPersist m, YesodPersistBackend m ~ SqlBackend)
        => RenderMessage m ChatRoomMessage
        => PatientId
        -> Entity User -- ^ user
        -> Entity User -- ^ interlocutor
        -> Route m
        -> WebSocketsT (SubHandlerFor ChatRoom m) ()
chatApp pid (Entity uid _) (Entity iid _) targetRoute = do

    let channelId = pack $ show $ fromSqlKey pid

    ChatRoom channelMapTVar <- getSubYesod

    channelMap <- readTVarIO channelMapTVar

    let maybeChan = M.lookup channelId channelMap

    writeChan <- atomically $ case maybeChan of
      Nothing -> do
          chan <- newBroadcastTChan
          writeTVar channelMapTVar $ M.insert channelId (chan,1) channelMap
          return chan
      Just (writeChan,_) -> do
          writeTVar channelMapTVar $ M.alter userJoinedChannel channelId channelMap
          return writeChan

    readChan <- atomically $ dupTChan writeChan

    (e :: Either SomeException ()) <- try $ race_
        (forever $ atomically (readTChan readChan) >>= sendTextData)
        (runConduit (sourceWS .| mapM_C (
                          \msg -> do
                              
                              m <- readTVarIO channelMapTVar

                              let n = maybe 0 snd (M.lookup channelId m)
                              let status = if n > 1 then ChatMessageStatusRead else ChatMessageStatusUnread
                              
                              now <- liftIO getCurrentTime
                              let chat = Chat uid iid now msg status False
                              atomically $ writeTChan writeChan $ toStrict $ encodeToLazyText chat
                              cid <- liftHandler (runDB $ insert chat)

                              mVapidKeys <- liftHandler getVapidKeys

                              case mVapidKeys of
                                Just vapidKeys -> do

                                    subscriptions <- liftHandler $ runDB $ select $ do
                                        x <- from $ table @PushSubscription
                                        where_ $ x ^. PushSubscriptionPublisher ==. val uid
                                        where_ $ x ^. PushSubscriptionSubscriber ==. val iid
                                        return x

                                    sender <- liftHandler $ runDB $ selectOne $ do
                                        x <- from $ table @User
                                        where_ $ x ^. UserId ==. val uid
                                        return x


                                    iconr <- liftHandler $ getStaticRoute img_chat_FILL0_wght400_GRAD0_opsz24_svg
                                    urlr <- liftHandler getUrlRender
                                    msgr <- liftHandler getMessageRender
                                    Superuser {..} <- liftHandler $ appSuperuser <$> getAppSettings

                                    let expath = decodeUtf8 . extractPath . encodeUtf8 . urlr
                                    
                                    forM_ subscriptions $ \(Entity _ (PushSubscription sid pid' endpoint p256dh auth)) -> do
                                        photor <- liftHandler $ getAccountPhotoRoute pid' AvatarColorDark
                                        let notification = mkPushNotification endpoint p256dh auth
                                                & pushMessage .~ object
                                                    [ "title" .= msgr MsgAppName
                                                    , "icon" .= expath iconr
                                                    , "image" .= expath photor
                                                    , "body" .= msg
                                                    , "messageType" .= PushMsgTypeChat
                                                    , "targetRoom" .= expath targetRoute
                                                    , "senderId" .= pid'
                                                    , "senderName" .= (
                                                          (\u -> fromMaybe (userEmail u) (userName u)) . entityVal <$> sender
                                                                      )
                                                    , "recipientId" .= sid
                                                    ]
                                                & pushSenderEmail .~ superuserUsername
                                                & pushExpireInSeconds .~ 30 * 60
                                                & pushTopic ?~ (PushTopic . pack . show $ PushMsgTypeChat)
                                                & pushUrgency ?~ PushUrgencyHigh

                                        manager <- liftHandler getAppHttpManager

                                        result <- sendPushNotification vapidKeys manager notification

                                        case result of
                                          Left ex -> do
                                              liftIO $ print ex
                                              liftHandler $ addMessageI statusError MsgPushNotificationExcception
                                          Right () -> liftHandler $ runDB $ update $ \x -> do
                                              set x [ChatNotified =. val True]
                                              where_ $ x ^. ChatId ==. val cid
                                Nothing -> do
                                    liftIO $ print @Text "No VAPID"
                                    liftHandler $ addMessageI statusError MsgPushNotificationExcception
                                  
                                    
                          )
                    ))


    case e of
      Left _ -> do
          m <- readTVarIO channelMapTVar
          let newChannelMap = M.alter userLeftChannel channelId m
          atomically $ writeTVar channelMapTVar newChannelMap
      Right () -> return ()


getDoctorChatRoomR :: (Yesod m, YesodChat m)
                   => RenderMessage m ChatRoomMessage
                   => (YesodPersist m, YesodPersistBackend m ~ SqlBackend)
                   => PatientId -> UserId -> SubHandlerFor ChatRoom m Html
getDoctorChatRoomR pid uid' = do

    patient <- liftHandler $ runDB $ selectOne $ do
        x :& u :& d :& l <- from $ table @Patient
            `innerJoin` table @User `on` (\(x :& u) -> x ^. PatientUser ==. u ^. UserId)
            `innerJoin` table @Doctor `on` (\(x :& _ :& d) -> x ^. PatientDoctor ==. d ^. DoctorId)
            `innerJoin` table @User `on` (\(_ :& _ :& d :& l) -> d ^. DoctorUser ==. just (l ^. UserId))
        where_ $ x ^. PatientId ==. val pid
        return (x,u,l,d)

    liftHandler $ runDB $ update $ \x -> do
        set x [ChatStatus =. val ChatMessageStatusRead]
        where_ $ x ^. ChatInterlocutor ==. val uid'
        where_ $ just ( just (x ^. ChatUser)) ==. subSelect
            ( do
                  y :& d <- from $ table @Patient `innerJoin` table @Doctor
                      `on` (\(y :& d) -> y ^. PatientDoctor ==. d ^. DoctorId)
                  where_ $ y ^. PatientId ==. val pid
                  return $ d ^. DoctorUser
            )
        where_ $ x ^. ChatStatus ==. val ChatMessageStatusUnread

    case patient of
      Just (_,user,interlocutor@(Entity iid _),_) -> do
          chatr <- liftHandler $ getChatRoute iid (PatientChatRoomR pid iid)
          webSockets (chatApp pid user interlocutor chatr)
      _otherwise -> invalidArgsI [MsgChatParticipantsNotDefined]


    chats <- liftHandler $ M.toList . groupByKey (\(Entity _ (Chat _ _ t _ _ _)) -> utctDay t) <$> runDB ( select $ do
        x <- from $ table @Chat
        where_ $ ( (x ^. ChatUser ==. val uid')
                   &&. ( case patient of
                           Just (_,_,Entity iid _,_) -> x ^. ChatInterlocutor ==. val iid
                           Nothing -> val False
                       )
                 ) ||. ( (x ^. ChatInterlocutor ==. val uid')
                         &&. ( case patient of
                                 Just (_,_,Entity iid _,_) -> x ^. ChatUser ==. val iid
                                 Nothing -> val False
                             )
                       )
        orderBy [desc (x ^. ChatTimemark)]
        return x )

    case patient of
      Just (_,Entity uid _,_,Entity did (Doctor name _ _ _ _)) -> do
          doctorPhotoR <- liftHandler $ getDoctorPhotoRoute did
          myDoctorR <- liftHandler $ getMyDoctorRoute pid uid did

          ringtoneOutgoing <- liftHandler $ getStaticRoute ringtones_outgoing_chat_mp3
          ringtoneIncoming <- liftHandler $ getStaticRoute ringtones_incoming_chat_mp3

          liftHandler $ defaultLayout $ do
              setTitleI MsgChat
              idChatOutput <- newIdent
              idMessageForm <- newIdent
              idMessageInput <- newIdent
              idAudioOutgoingChat <- newIdent
              idAudioIncomingChat <- newIdent
              $(widgetFile "my/doctors/chat/chat")
              
      Nothing -> invalidArgsI [MsgChatParticipantsNotDefined]


getPatientChatRoomR :: (Yesod m, YesodChat m)
                    => RenderMessage m ChatRoomMessage
                    => (YesodPersist m, YesodPersistBackend m ~ SqlBackend)
                    => PatientId -> UserId -> SubHandlerFor ChatRoom m Html
getPatientChatRoomR pid uid = do

    patient <- liftHandler $ runDB $ selectOne $ do
        x :& u :& d :& l <- from $ table @Patient
            `innerJoin` table @User `on` (\(x :& u) -> x ^. PatientUser ==. u ^. UserId)
            `innerJoin` table @Doctor `on` (\(x :& _ :& d) -> x ^. PatientDoctor ==. d ^. DoctorId)
            `innerJoin` table @User `on` (\(_ :& _ :& d :& l) -> d ^. DoctorUser ==. just (l ^. UserId))
        where_ $ x ^. PatientId ==. val pid
        return (x,u,l,d)

    liftHandler $ runDB $ update $ \x -> do
        set x [ChatStatus =. val ChatMessageStatusRead]
        where_ $ x ^. ChatInterlocutor ==. val uid
        where_ $ just (x ^. ChatUser) ==. subSelect
            ( do
                  y <- from $ table @Patient
                  where_ $ y ^. PatientId ==. val pid
                  return $ y ^. PatientUser
            )
        where_ $ x ^. ChatStatus ==. val ChatMessageStatusUnread

    case patient of
      Just (_,interlocutor@(Entity iid _),user,_) -> do
          chatr <- liftHandler $ getChatRoute iid (DoctorChatRoomR pid iid)
          webSockets (chatApp pid user interlocutor chatr)
      _otherwise -> invalidArgsI [MsgChatParticipantsNotDefined]

    chats <- liftHandler $ M.toList . groupByKey (\(Entity _ (Chat _ _ t _ _ _)) -> utctDay t) <$> runDB ( select $ do
        x <- from $ table @Chat
        where_ $ ( (x ^. ChatUser ==. val uid)
                   &&. ( case patient of
                           Just (_,Entity iid _,_,_) -> x ^. ChatInterlocutor ==. val iid
                           Nothing -> val False
                       )
                 ) ||. ( (x ^. ChatInterlocutor ==. val uid)
                         &&. ( case patient of
                                 Just (_,Entity iid _,_,_) -> x ^. ChatUser ==. val iid
                                 Nothing -> val False
                             )
                       )
        orderBy [desc (x ^. ChatTimemark)]
        return x )

    case patient of
      Just (_,Entity iid (User email _ _ _ _ name _ _),_,Entity did _) -> do
          myPatientR <- liftHandler $ getMyPatientRoute uid did pid
          accountPhotoR <- liftHandler $ getAccountPhotoRoute iid AvatarColorDark

          ringtoneOutgoing <- liftHandler $ getStaticRoute ringtones_outgoing_chat_mp3
          ringtoneIncoming <- liftHandler $ getStaticRoute ringtones_incoming_chat_mp3

          liftHandler $ defaultLayout $ do
              setTitleI MsgChat
              idChatOutput <- newIdent
              idMessageForm <- newIdent
              idMessageInput <- newIdent
              idAudioOutgoingChat <- newIdent
              idAudioIncomingChat <- newIdent
              $(widgetFile "my/patients/chat/chat")
              
      Nothing -> invalidArgsI [MsgChatParticipantsNotDefined]


groupByKey :: Ord k => (v -> k) -> [v] -> M.Map k [v]
groupByKey key = M.fromListWith (<>) . fmap (\x -> (key x,[x]))


instance ( Yesod m, YesodChat m
         , YesodPersist m, YesodPersistBackend m ~ SqlBackend
         , RenderMessage m ChatRoomMessage, RenderMessage m FormMessage
         ) =>  YesodSubDispatch ChatRoom m where
    yesodSubDispatch :: YesodSubRunnerEnv ChatRoom m -> Application
    yesodSubDispatch = $(mkYesodSubDispatch resourcesChatRoom)
