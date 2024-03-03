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

module VideoRoom
  ( module VideoRoom.Data
  , getDoctorVideoRoomR
  , getPatientVideoRoomR
  ) where

import VideoRoom.Data
    ( VideoRoom (VideoRoom), resourcesVideoRoom
    , Route (DoctorVideoRoomR, PatientVideoRoomR)
    )

import Conduit ((.|), mapM_C, runConduit, MonadIO (liftIO))

import Control.Monad (forever)

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val, innerJoin, on
    , (^.), (==.), (:&) ((:&))
    , just, SqlBackend
    )
import Database.Persist ( Entity (Entity) )
import Database.Persist.Sql (fromSqlKey)

import qualified Data.Map as M ( lookup, insert, alter )
import Data.Text (pack)

import Foundation
    ( App, Route (MyDoctorR, MyPatientR)
    , AppMessage
      ( MsgBack, MsgChatParticipantsNotDefined, MsgVideoCall
      )
    )

import Model
    ( AvatarColor (AvatarColorDark)
    , ChatMessageStatus (ChatMessageStatusRead, ChatMessageStatusUnread)
    , DoctorId, Doctor (Doctor), PatientId, Patient, UserId, User (User)
    , Chat (Chat)
    , EntityField
      ( DoctorId, PatientDoctor, PatientId, PatientUser, UserId, DoctorUser
      , ChatTimemark, ChatUser, ChatInterlocutor, ChatStatus
      )
    )

import UnliftIO.Exception (try, SomeException)
import UnliftIO.STM
    (atomically, readTVarIO, writeTVar, newTQueue, readTQueue, writeTQueue)

import Settings (widgetFile)

import Yesod
    ( Yesod (defaultLayout), YesodSubDispatch, yesodSubDispatch
    , mkYesodSubDispatch, SubHandlerFor, Html, MonadHandler (liftHandler)
    , getSubYesod, setTitleI, Application, YesodPersist (YesodPersistBackend)
    )
import Yesod.Auth (maybeAuth)
import Yesod.Core.Types (YesodSubRunnerEnv)
import Yesod.Persist (YesodPersist(runDB))
import Yesod.WebSockets
    ( WebSocketsT, sendTextData, race_, sourceWS, webSockets)


userJoinedChannel :: Num b => Maybe (a,b) -> Maybe (a,b)
userJoinedChannel Nothing = Nothing
userJoinedChannel (Just (writeChan,numUsers)) = Just (writeChan,numUsers + 1)


userLeftChannel :: Num b => Maybe (a,b) -> Maybe (a,b)
userLeftChannel Nothing = Nothing
userLeftChannel (Just (writeChan,numUsers)) = Just (writeChan,numUsers - 1)


chatApp :: (YesodPersist master, YesodPersistBackend master ~ SqlBackend)
        => PatientId
        -> Bool
        -> WebSocketsT (SubHandlerFor VideoRoom master) ()
chatApp pid polite = do

    let channelId = pack $ show $ fromSqlKey pid

    VideoRoom channelMapTVar <- getSubYesod

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

    user <- maybeAuth

    patient <- liftHandler $ runDB $ selectOne $ do
        x :& u :& d :& l <- from $ table @Patient
            `innerJoin` table @User `on` (\(x :& u) -> x ^. PatientUser ==. u ^. UserId)
            `innerJoin` table @Doctor `on` (\(x :& _ :& d) -> x ^. PatientDoctor ==. d ^. DoctorId)
            `innerJoin` table @User `on` (\(_ :& _ :& d :& l) -> d ^. DoctorUser ==. just (l ^. UserId))
        where_ $ x ^. PatientId ==. val pid
        return (x,u,l,d)

    webSockets (chatApp pid polite)

    liftHandler $ defaultLayout $ do
        setTitleI MsgVideoCall
        $(widgetFile "my/doctors/video/video")



getPatientVideoRoomR :: PatientId -> DoctorId -> UserId -> SubHandlerFor VideoRoom App Html
getPatientVideoRoomR pid did uid = do
    let polite = False

    user <- maybeAuth

    patient <- liftHandler $ runDB $ selectOne $ do
        x :& u :& d :& l <- from $ table @Patient
            `innerJoin` table @User `on` (\(x :& u) -> x ^. PatientUser ==. u ^. UserId)
            `innerJoin` table @Doctor `on` (\(x :& _ :& d) -> x ^. PatientDoctor ==. d ^. DoctorId)
            `innerJoin` table @User `on` (\(_ :& _ :& d :& l) -> d ^. DoctorUser ==. just (l ^. UserId))
        where_ $ x ^. PatientId ==. val pid
        return (x,u,l,d)

    webSockets (chatApp pid polite)

    liftHandler $ defaultLayout $ do
        setTitleI MsgVideoCall
        $(widgetFile "my/patients/video/video")


instance YesodSubDispatch VideoRoom App where
    yesodSubDispatch :: YesodSubRunnerEnv VideoRoom App -> Application
    yesodSubDispatch = $(mkYesodSubDispatch resourcesVideoRoom)
