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

module VideoRoom
  ( module VideoRoom.Data
  , getDoctorVideoRoomR
  , getPatientVideoRoomR
  ) where

import VideoRoom.Data
    ( VideoRoom (VideoRoom, rtcPeerConnectionConfig), resourcesVideoRoom
    , channelMapTVar
    , Route (DoctorVideoRoomR, PatientVideoRoomR)
    )

import Conduit ((.|), mapM_C, runConduit, MonadIO (liftIO))

import Control.Monad (forever)

import Database.Esqueleto.Experimental
    ( SqlBackend
    )
import Database.Persist.Sql (fromSqlKey)

import Data.Aeson (object )
import Data.Maybe (fromMaybe)
import qualified Data.Map as M ( lookup, insert, alter )
import Data.Text (pack)

import Foundation
    ( App, Route (MyDoctorR, MyPatientR)
    , AppMessage
      ( MsgBack, MsgVideoCall
      )
    )

import Model
    ( DoctorId, PatientId, UserId
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
import Yesod.Core.Types (YesodSubRunnerEnv)
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

    webSockets (chatApp pid polite) 

    config <- fromMaybe (object []) . rtcPeerConnectionConfig <$> getSubYesod

    liftHandler $ defaultLayout $ do
        setTitleI MsgVideoCall
        $(widgetFile "my/doctors/video/video")



getPatientVideoRoomR :: PatientId -> DoctorId -> UserId -> SubHandlerFor VideoRoom App Html
getPatientVideoRoomR pid did uid = do
    let polite = False

    webSockets (chatApp pid polite)    

    config <- fromMaybe (object []) . rtcPeerConnectionConfig <$> getSubYesod

    liftHandler $ defaultLayout $ do
        setTitleI MsgVideoCall
        $(widgetFile "my/patients/video/video")


instance YesodSubDispatch VideoRoom App where
    yesodSubDispatch :: YesodSubRunnerEnv VideoRoom App -> Application
    yesodSubDispatch = $(mkYesodSubDispatch resourcesVideoRoom)
