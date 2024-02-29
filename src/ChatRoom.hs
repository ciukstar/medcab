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

module ChatRoom
  ( module ChatRoom.Data
  , getDoctorChatRoomR
  ) where

import ChatRoom.Data
    ( ChatRoom (ChatRoom), resourcesChatRoom
    , Route (DoctorChatRoomR, PatientChatRoomR)
    )

import Conduit ((.|), mapM_C, runConduit, MonadIO (liftIO))

import Control.Monad (forever)
import Control.Concurrent.STM.TChan
    ( writeTChan, dupTChan, readTChan, newBroadcastTChan )

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val, innerJoin, on, update, set
    , (^.), (==.), (:&) ((:&)), (||.), (&&.), (=.)
    , just, SqlBackend, select, orderBy, asc, subSelect
    )
import Database.Persist
    ( Entity (Entity), PersistStoreWrite (insert_) )
import Database.Persist.Sql (fromSqlKey)

import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Map as M (Map,lookup, insert, alter, fromListWith, toList)
import Data.Text (pack)
import Data.Text.Lazy (toStrict)

import Foundation
    ( App, Route (MyDoctorR, DoctorPhotoR, MyPatientR, AccountPhotoR)
    , AppMessage(MsgBack, MsgChat, MsgPhoto, MsgMessage)
    )

import Model
    ( AvatarColor (AvatarColorDark)
    , ChatMessageStatus (ChatMessageStatusRead, ChatMessageStatusUnread)
    , Doctor (Doctor), PatientId, Patient, UserId, User (User)
    , Chat (Chat)
    , EntityField
      ( DoctorId, PatientDoctor, PatientId, PatientUser, UserId, DoctorUser
      , ChatTimemark, ChatUser, ChatInterlocutor, ChatStatus
      )
    )

import UnliftIO.Exception (try, SomeException)
import UnliftIO.STM (atomically, readTVarIO, writeTVar)

import Settings (widgetFile)

import Yesod
    ( Yesod (defaultLayout), YesodSubDispatch, yesodSubDispatch
    , mkYesodSubDispatch, SubHandlerFor, Html, MonadHandler (liftHandler)
    , getSubYesod, setTitleI, Application, YesodPersist (YesodPersistBackend)
    )
import Yesod.Auth (maybeAuth)
import Yesod.Core.Handler (newIdent)
import Yesod.Core.Types (YesodSubRunnerEnv)
import Yesod.Persist (YesodPersist(runDB))
import Yesod.WebSockets
    ( WebSocketsT, sendTextData, receiveData, race_, sourceWS, webSockets)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))


userJoinedChannel :: Num b => Maybe (a,b) -> Maybe (a,b)
userJoinedChannel Nothing = Nothing
userJoinedChannel (Just (writeChan,numUsers)) = Just (writeChan,numUsers + 1)

userLeftChannel :: Num b => Maybe (a,b) -> Maybe (a,b)
userLeftChannel Nothing = Nothing
userLeftChannel (Just (writeChan,numUsers)) = Just (writeChan,numUsers - 1)


cleanupChannel :: (Eq a1, Num a1) => Maybe (a2,a1) -> Maybe (a2,a1)
cleanupChannel Nothing = Nothing
cleanupChannel (Just (_writeChan, 1)) = Nothing
cleanupChannel (Just c) = Just c


chatApp :: (YesodPersist master, YesodPersistBackend master ~ SqlBackend)
        => PatientId -> Entity User -> Entity User -> WebSocketsT (SubHandlerFor ChatRoom master) ()
chatApp pid user@(Entity uid _) interlocutor@(Entity iid _) = do
    -- let name = fromMaybe (userEmail $ entityVal user) (userName $ entityVal user)

    let channelId = pack $ show $ fromSqlKey pid
    -- sendTextData $ toStrict $ encodeToLazyText (Msg uid iid (name <> " just joined " <> channelId))

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

    readChan <- atomically $ do
        -- writeTChan writeChan $ name <> " has joined the chat"
        dupTChan writeChan

    (e :: Either SomeException ()) <- try $ race_
        (forever $ atomically (readTChan readChan) >>= sendTextData)
        (runConduit (sourceWS .| mapM_C (
                          \msg -> do
                              
                              m <- readTVarIO channelMapTVar

                              let n = maybe 0 snd (M.lookup channelId m)
                              let status = if n > 1 then ChatMessageStatusRead else ChatMessageStatusUnread
                              
                              now <- liftIO getCurrentTime
                              let chat = Chat uid iid now msg status
                              atomically $ writeTChan writeChan $ toStrict $ encodeToLazyText chat
                              liftHandler (runDB $ insert_ chat)
                          )
                    ))


    case e of
      Left _ -> do
          m <- readTVarIO channelMapTVar
          let newChannelMap = M.alter userLeftChannel channelId m
          atomically $ writeTVar channelMapTVar newChannelMap
          {--
          let newChannelMap = M.alter cleanupChannel channelId channelMap
          writeTVar channelMapTVar newChannelMap
          writeTChan writeChan $ toStrict $ encodeToLazyText (Msg uid iid (name <> " has left the chat"))
          --}
      Right () -> do
          return ()



getDoctorChatRoomR :: PatientId -> UserId -> SubHandlerFor ChatRoom App Html
getDoctorChatRoomR pid uid = do

    user <- maybeAuth

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
        where_ $ just ( just (x ^. ChatUser)) ==. subSelect
            ( do
                  y :& d <- from $ table @Patient `innerJoin` table @Doctor
                      `on` (\(y :& d) -> y ^. PatientDoctor ==. d ^. DoctorId)
                  where_ $ y ^. PatientId ==. val pid
                  return $ d ^. DoctorUser
            )
        where_ $ x ^. ChatStatus ==. val ChatMessageStatusUnread

    case (user,patient) of
      (Just u, Just (_,_,interlocutor,_)) -> webSockets (chatApp pid u interlocutor)
      _ -> return ()


    chats <- liftHandler $ M.toList . groupByKey (\(Entity _ (Chat _ _ t _ _)) -> utctDay t) <$> runDB ( select $ do
        x <- from $ table @Chat
        where_ $ ( (x ^. ChatUser ==. val uid)
                   &&. ( case patient of
                           Just (_,_,Entity iid _,_) -> x ^. ChatInterlocutor ==. val iid
                           Nothing -> val False
                       )
                 ) ||. ( (x ^. ChatInterlocutor ==. val uid)
                         &&. ( case patient of
                                 Just (_,_,Entity iid _,_) -> x ^. ChatUser ==. val iid
                                 Nothing -> val False
                             )
                       )
        orderBy [asc (x ^. ChatTimemark)]
        return x )

    liftHandler $ defaultLayout $ do
        setTitleI MsgChat
        idChatOutput <- newIdent
        idMessageForm <- newIdent
        idMessageInput <- newIdent
        $(widgetFile "my/doctors/chat/chat")


getPatientChatRoomR :: PatientId -> UserId -> SubHandlerFor ChatRoom App Html
getPatientChatRoomR pid uid = do

    user <- maybeAuth

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

    case (user,patient) of
      (Just u, Just (_,interlocutor,_,_)) -> webSockets (chatApp pid u interlocutor)
      _ -> return ()

    chats <- liftHandler $ M.toList . groupByKey (\(Entity _ (Chat _ _ t _ _)) -> utctDay t) <$> runDB ( select $ do
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
        orderBy [asc (x ^. ChatTimemark)]
        return x )

    liftHandler $ defaultLayout $ do
        setTitleI MsgChat
        idChatOutput <- newIdent
        idMessageForm <- newIdent
        idMessageInput <- newIdent
        $(widgetFile "my/patients/chat/chat")


groupByKey :: Ord k => (v -> k) -> [v] -> M.Map k [v]
groupByKey key = M.fromListWith (<>) . fmap (\x -> (key x,[x]))


instance YesodSubDispatch ChatRoom App where
    yesodSubDispatch :: YesodSubRunnerEnv ChatRoom App -> Application
    yesodSubDispatch = $(mkYesodSubDispatch resourcesChatRoom)
