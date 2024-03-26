{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VideoRoom.Data where

import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent.STM.TVar (TVar)

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Map as M
import Data.Text (Text)

import Yesod.Core (renderRoute, PathPiece)
import Yesod.Core.Dispatch (mkYesodSubData, parseRoutes)

newtype ChanId = ChanId Int
    deriving (Eq, Ord, Show, Read, PathPiece, ToJSON, FromJSON)


newtype VideoRoom = VideoRoom
    { channelMapTVar :: TVar (M.Map ChanId ((TQueue Text,TQueue Text), Int))
    }


data VideoRoomMessage = VideoRoomOutgoingCall | VideoRoomIncomingCall

englishVideoRoomMessage :: VideoRoomMessage -> Text
englishVideoRoomMessage VideoRoomOutgoingCall = "Outgoing call"
englishVideoRoomMessage VideoRoomIncomingCall = "Incoming call"

frenchVideoRoomMessage :: VideoRoomMessage -> Text
frenchVideoRoomMessage VideoRoomOutgoingCall = "Appel sortant"
frenchVideoRoomMessage VideoRoomIncomingCall = "Appel entrant"

romanianVideoRoomMessage :: VideoRoomMessage -> Text
romanianVideoRoomMessage VideoRoomOutgoingCall = "Apel de ieșire"
romanianVideoRoomMessage VideoRoomIncomingCall = "Apel de intrare"

russianVideoRoomMessage :: VideoRoomMessage -> Text
russianVideoRoomMessage VideoRoomOutgoingCall = "Исходящий звонок"
russianVideoRoomMessage VideoRoomIncomingCall = "Входящий звонок"

defaultVideoRoomMessage :: VideoRoomMessage -> Text
defaultVideoRoomMessage = englishVideoRoomMessage


mkYesodSubData "VideoRoom" [parseRoutes|
/#ChanId/#Bool/ws WebSoketR    GET
/api/push         PushMessageR POST
/incoming         IncomingR    GET
|]
