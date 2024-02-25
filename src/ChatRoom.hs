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
import ChatRoom.Data
    ( ChatRoom (ChatRoom), resourcesChatRoom
    , Route (DoctorChatRoomR, PatientChatRoomR)
    )
    
import Conduit ((.|), mapM_C, runConduit)

import Control.Monad (forever)
import Control.Concurrent.STM.TChan
    ( writeTChan, dupTChan, readTChan, newBroadcastTChan )

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val, innerJoin, on
    , (^.), (==.), (:&) ((:&)), just
    )
import Database.Persist (Entity (Entity), entityVal)
import Database.Persist.Sql (fromSqlKey)

import Data.Aeson (ToJSON, Value, toJSON, object, (.=))
import Data.Aeson.Text (encodeToLazyText)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M (lookup, insert, alter)
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)

import Foundation
    ( App, Route (MyDoctorR, DoctorPhotoR, MyPatientR, AccountPhotoR)
    , AppMessage(MsgBack, MsgChat, MsgPhoto, MsgMessage)
    )

import Model
    ( AvatarColor (AvatarColorDark)
    , Doctor (Doctor), PatientId, Patient
    , UserId, User (User, userEmail, userName)
    , EntityField
      ( DoctorId, PatientDoctor, PatientId, PatientUser, UserId, DoctorUser
      )
    )

import UnliftIO.Exception (try, SomeException)
import UnliftIO.STM (atomically, readTVarIO, writeTVar)

import Settings (widgetFile)

import Yesod
    ( Yesod (defaultLayout), YesodSubDispatch, yesodSubDispatch
    , mkYesodSubDispatch, SubHandlerFor, Html, MonadHandler (liftHandler)
    , getSubYesod, setTitleI, Application, newIdent
    )
import Yesod.Auth (maybeAuth)
import Yesod.Core.Types (YesodSubRunnerEnv)
import Yesod.Persist (YesodPersist(runDB))
import Yesod.WebSockets
    ( WebSocketsT, sendTextData, receiveData, race_, sourceWS, webSockets)


userJoinedChannel :: Num b => Maybe (a,b) -> Maybe (a,b)
userJoinedChannel Nothing = Nothing
userJoinedChannel (Just (writeCan,numUsers)) = Just (writeCan,numUsers + 1)


cleanupChannel :: (Eq a1, Num a1) => Maybe (a2,a1) -> Maybe (a2,a1)
cleanupChannel Nothing = Nothing
cleanupChannel (Just (_writeChan, 1)) = Nothing
cleanupChannel (Just c) = Just c


chatApp :: PatientId -> Entity User -> Entity User -> WebSocketsT (SubHandlerFor ChatRoom mater) ()
chatApp pid user@(Entity uid _) interlocutor@(Entity iid _) = do
    let name = fromMaybe (userEmail $ entityVal user) (userName $ entityVal user) 
    
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
        (runConduit (sourceWS .| mapM_C
                     (\msg -> atomically $ writeTChan writeChan $ toStrict $ encodeToLazyText (Msg uid iid msg))
                    ))
{--
    atomically $ case e of
      Left _ -> do
          let newChannelMap = M.alter cleanupChannel channelId channelMap
          writeTVar channelMapTVar newChannelMap
          writeTChan writeChan $ toStrict $ encodeToLazyText (Msg uid iid (name <> " has left the chat"))
      Right () -> return ()
    --}  
    return ()
    

data Msg = Msg !UserId !UserId !Text

instance ToJSON Msg where
   
    toJSON :: Msg -> Value
    toJSON (Msg uid iid msg) = object [ "user" .= pack (show $ fromSqlKey uid)
                                      , "interlocautor" .= pack (show $ fromSqlKey iid)
                                      , "message" .= msg
                                      ]


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
        
    case (user,patient) of
      (Just u, Just (_,_,iterlocutor,_)) -> webSockets (chatApp pid u iterlocutor)
      _ -> return ()
      
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
        
    case (user,patient) of
      (Just u, Just (_,_,iterlocutor,_)) -> webSockets (chatApp pid u iterlocutor)
      _ -> return ()
      
    liftHandler $ defaultLayout $ do
        setTitleI MsgChat
        idChatOutput <- newIdent
        idMessageForm <- newIdent
        idMessageInput <- newIdent
        $(widgetFile "my/patients/chat/chat")




instance YesodSubDispatch ChatRoom App where
    yesodSubDispatch :: YesodSubRunnerEnv ChatRoom App -> Application
    yesodSubDispatch = $(mkYesodSubDispatch resourcesChatRoom)
