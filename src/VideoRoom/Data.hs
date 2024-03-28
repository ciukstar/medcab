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

import Model (UserId)

import Yesod.Core (renderRoute, PathPiece)
import Yesod.Core.Dispatch (mkYesodSubData, parseRoutes)

newtype ChanId = ChanId Int
    deriving (Eq, Ord, Show, Read, PathPiece, ToJSON, FromJSON)


newtype VideoRoom = VideoRoom
    { channelMapTVar :: TVar (M.Map ChanId ((TQueue Text,TQueue Text), Int))
    }


data VideoRoomMessage = VideoRoomOutgoingCall
                      | VideoRoomIncomingCall
                      | VideoRoomCallEnded
                      | VideoRoomVideoSession
                      | VideoRoomClose
                      | VideoRoomNotGeneratedVAPID

englishVideoRoomMessage :: VideoRoomMessage -> Text
englishVideoRoomMessage VideoRoomOutgoingCall = "Outgoing call"
englishVideoRoomMessage VideoRoomIncomingCall = "Incoming call"
englishVideoRoomMessage VideoRoomCallEnded = "Call ended"
englishVideoRoomMessage VideoRoomVideoSession = "Video Session"
englishVideoRoomMessage VideoRoomClose = "Close"
englishVideoRoomMessage VideoRoomNotGeneratedVAPID = "VAPID not generated"

frenchVideoRoomMessage :: VideoRoomMessage -> Text
frenchVideoRoomMessage VideoRoomOutgoingCall = "Appel sortant"
frenchVideoRoomMessage VideoRoomIncomingCall = "Appel entrant"
frenchVideoRoomMessage VideoRoomCallEnded = "Appel terminé"
frenchVideoRoomMessage VideoRoomVideoSession = "Séance vidéo"
frenchVideoRoomMessage VideoRoomClose = "Fermer"
frenchVideoRoomMessage VideoRoomNotGeneratedVAPID = "Le VAPID n'a pas été généré"

romanianVideoRoomMessage :: VideoRoomMessage -> Text
romanianVideoRoomMessage VideoRoomOutgoingCall = "Apel de ieșire"
romanianVideoRoomMessage VideoRoomIncomingCall = "Apel de intrare"
romanianVideoRoomMessage VideoRoomCallEnded = "Apel terminat"
romanianVideoRoomMessage VideoRoomVideoSession = "Sesiune video"
romanianVideoRoomMessage VideoRoomClose = "Închide"
romanianVideoRoomMessage VideoRoomNotGeneratedVAPID = "VAPID nu a fost generat"

russianVideoRoomMessage :: VideoRoomMessage -> Text
russianVideoRoomMessage VideoRoomOutgoingCall = "Исходящий звонок"
russianVideoRoomMessage VideoRoomIncomingCall = "Входящий звонок"
russianVideoRoomMessage VideoRoomCallEnded = "Звонок окончен"
russianVideoRoomMessage VideoRoomVideoSession = "Видеосессия"
russianVideoRoomMessage VideoRoomClose = "Закрыть"
russianVideoRoomMessage VideoRoomNotGeneratedVAPID = "VAPID не был создан"

defaultVideoRoomMessage :: VideoRoomMessage -> Text
defaultVideoRoomMessage = englishVideoRoomMessage


mkYesodSubData "VideoRoom" [parseRoutes|
/photo/#UserId            PhotoR       GET
/ws/#ChanId/#Bool         WebSoketR    GET
/api/push                 PushMessageR POST
/outgoing/#UserId/#UserId OutgoingR    GET
/incoming                 IncomingR    GET
|]
