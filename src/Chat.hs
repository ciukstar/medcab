{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}

module Chat (module Chat.Data, module Chat) where

import Chat.Data (Chat (Chat), resourcesChat, Route (ChatR))
import Conduit (($$), mapM_C)
import Control.Monad (forever)
import Control.Concurrent.STM.TChan (writeTChan, dupTChan, readTChan)

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val
    , (^.), (==.)
    )
import Database.Persist (Entity (Entity))
    
import Data.Text (Text)

import Foundation
    ( App, Route (DoctorR, DoctorPhotoR)
    , AppMessage(MsgBack, MsgChat, MsgPhoto)
    )

import Model (DoctorId, Doctor (Doctor), EntityField (DoctorId))

import UnliftIO.Exception (try, SomeException)
import UnliftIO.STM (atomically)

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


chatApp :: WebSocketsT (SubHandlerFor Chat mater) ()
chatApp = do
    sendTextData ("Welcome to the chat server, please enter your name." :: Text)
    name <- receiveData
    sendTextData $ "Welcome, " <> name
    Chat writeChan <- getSubYesod
    readChan <- atomically $ do
        writeTChan writeChan $ name <> " has joined the chat"
        dupTChan writeChan
    (e :: Either SomeException ()) <- try $ race_
        (forever $ atomically (readTChan readChan) >>= sendTextData)
        (sourceWS $$ mapM_C (\msg ->
            atomically $ writeTChan writeChan $ name <> ": " <> msg))

    atomically $ case e of
        Left _ -> writeTChan writeChan $ name <> " has left the chat"
        Right () -> return ()


getChatR :: DoctorId -> SubHandlerFor Chat App Html
getChatR did = do

    doctor <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @Doctor
        where_ $ x ^. DoctorId ==. val did
        return x
    
    webSockets chatApp
    liftHandler $ defaultLayout $ do
        setTitleI MsgChat
        $(widgetFile "doctors/chat/chat")




instance YesodSubDispatch Chat App where
    yesodSubDispatch :: YesodSubRunnerEnv Chat App -> Application
    yesodSubDispatch = $(mkYesodSubDispatch resourcesChat)
