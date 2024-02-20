{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}

module ChatRoom (module ChatRoom.Data, module ChatRoom) where

import ChatRoom.Data (ChatRoom (ChatRoom), resourcesChatRoom, Route (ChatRoomR))
import Conduit ((.|), mapM_C, runConduit)
import Control.Monad (forever)
import Control.Concurrent.STM.TChan (writeTChan, dupTChan, readTChan, newBroadcastTChan)

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val
    , (^.), (==.)
    )
import Database.Persist (Entity (Entity))

import qualified Data.Map as M (lookup, insert, alter)
import Data.Text (Text)

import Foundation
    ( App, Route (DoctorR, DoctorPhotoR)
    , AppMessage(MsgBack, MsgChat, MsgPhoto)
    )

import Model (DoctorId, Doctor (Doctor), EntityField (DoctorId))

import UnliftIO.Exception (try, SomeException)
import UnliftIO.STM (atomically, readTVarIO, writeTVar)

import Settings (widgetFile)

import Yesod
    ( Yesod (defaultLayout), YesodSubDispatch, yesodSubDispatch
    , mkYesodSubDispatch, SubHandlerFor, Html, MonadHandler (liftHandler)
    , getSubYesod, setTitleI, Application
    )
import Yesod.Core.Types (YesodSubRunnerEnv)
import Yesod.Persist (YesodPersist(runDB))
import Yesod.WebSockets
    ( WebSocketsT, sendTextData, receiveData, race_, sourceWS, webSockets)


userJoinedChannel :: Num b => Maybe (a,b) -> Maybe (a,b)
userJoinedChannel Nothing = Nothing
userJoinedChannel (Just (writeCan,numUsers)) = Just (writeCan,numUsers + 1)


cleanupChannel :: (Eq a1, Num a1) => Maybe (a2,a1) -> Maybe (a2,a1)
cleanupChannel Nothing = Nothing
cleanupChannel (Just (writeChan, 1)) = Nothing
cleanupChannel (Just c) = Just c


chatApp :: WebSocketsT (SubHandlerFor ChatRoom mater) ()
chatApp = do
    sendTextData ("Welcome to the chat server, please enter your name." :: Text)
    name <- receiveData
    
    sendTextData $ "Welcome, " <> name <> ". Plsease enter your cannel ID"
    channelId <- receiveData
    sendTextData $  name <> " just joined " <> channelId

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
        writeTChan writeChan $ name <> " has joined the chat"
        dupTChan writeChan

    (e :: Either SomeException ()) <- try $ race_
        (forever $ atomically (readTChan readChan) >>= sendTextData)
        (runConduit (sourceWS .| mapM_C (\msg -> atomically $ writeTChan writeChan $ name <> ": " <> msg)))

    atomically $ case e of
      Left _ -> do
          let newChannelMap = M.alter cleanupChannel channelId channelMap
          writeTVar channelMapTVar newChannelMap
          writeTChan writeChan $ name <> " has left the chat"
      Right () -> return ()
    

getChatRoomR :: DoctorId -> SubHandlerFor ChatRoom App Html
getChatRoomR did = do

    doctor <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @Doctor
        where_ $ x ^. DoctorId ==. val did
        return x
    
    webSockets chatApp
    liftHandler $ defaultLayout $ do
        setTitleI MsgChat
        $(widgetFile "doctors/chat/chat")




instance YesodSubDispatch ChatRoom App where
    yesodSubDispatch :: YesodSubRunnerEnv ChatRoom App -> Application
    yesodSubDispatch = $(mkYesodSubDispatch resourcesChatRoom)
