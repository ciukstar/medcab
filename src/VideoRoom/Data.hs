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

import Model (UserId, PatientId)

import Text.Shakespeare.Text (st)

import Yesod.Core (renderRoute, PathPiece)
import Yesod.Core.Dispatch (mkYesodSubData, parseRoutes)

newtype ChanId = ChanId Int
    deriving (Eq, Ord, Show, Read, PathPiece, ToJSON, FromJSON)


newtype VideoRoom = VideoRoom
    { channelMapTVar :: TVar (M.Map ChanId ((TQueue Text,TQueue Text), Int))
    }


mkYesodSubData "VideoRoom" [parseRoutes|
/photo/#UserId                          PhotoR       GET
/ws/#ChanId/#Bool                       WebSoketR    GET
/push/#UserId/#UserId                   PushMessageR POST
/video/#UserId/#PatientId/#UserId/#Bool RoomR        GET
/audio/#UserId/#PatientId/#UserId/#Bool AudioR       GET
|]

data VideoRoomMessage = MsgAppName
                      | MsgBack
                      | MsgVideoSession
                      | MsgAudioSession
                      | MsgCallEnded
                      | MsgClose
                      | MsgUserCallIsOver Text
                      | MsgNotGeneratedVAPID

englishVideoRoomMessage :: VideoRoomMessage -> Text
englishVideoRoomMessage MsgAppName = "MedCab"
englishVideoRoomMessage MsgBack = "Back"
englishVideoRoomMessage MsgVideoSession = "Video Session"
englishVideoRoomMessage MsgAudioSession = "Audio session"
englishVideoRoomMessage MsgCallEnded = "Call ended"
englishVideoRoomMessage MsgClose = "Close"
englishVideoRoomMessage (MsgUserCallIsOver user) = [st|The call is over. #{user} hung up.|]
englishVideoRoomMessage MsgNotGeneratedVAPID = "VAPID not generated"

frenchVideoRoomMessage :: VideoRoomMessage -> Text
frenchVideoRoomMessage MsgAppName = "MedCab"
frenchVideoRoomMessage MsgBack = "Retour"
frenchVideoRoomMessage MsgVideoSession = "Séance vidéo"
frenchVideoRoomMessage MsgAudioSession = "Séance audio"
frenchVideoRoomMessage MsgCallEnded = "Appel terminé"
frenchVideoRoomMessage MsgClose = "Fermer"
frenchVideoRoomMessage (MsgUserCallIsOver user) = [st|L'appel est terminé. #{user} a raccroché.|]
frenchVideoRoomMessage MsgNotGeneratedVAPID = "Le VAPID n'a pas été généré"

romanianVideoRoomMessage :: VideoRoomMessage -> Text
romanianVideoRoomMessage MsgAppName = "MedCab"
romanianVideoRoomMessage MsgBack = "Înapoi"
romanianVideoRoomMessage MsgVideoSession = "Sesiune video"
romanianVideoRoomMessage MsgAudioSession = "Sesiune audio"
romanianVideoRoomMessage MsgCallEnded = "Apel terminat"
romanianVideoRoomMessage MsgClose = "Închide"
romanianVideoRoomMessage (MsgUserCallIsOver user) = [st|Apelul sa încheiat. #{user} a închis.|]
romanianVideoRoomMessage MsgNotGeneratedVAPID = "VAPID nu a fost generat"

russianVideoRoomMessage :: VideoRoomMessage -> Text
russianVideoRoomMessage MsgAppName = "MedCab"
russianVideoRoomMessage MsgBack = "Вернуться"
russianVideoRoomMessage MsgVideoSession = "Видеосессия"
russianVideoRoomMessage MsgAudioSession = "Аудиосессия"
russianVideoRoomMessage MsgCallEnded = "Звонок окончен"
russianVideoRoomMessage MsgClose = "Закрыть"
russianVideoRoomMessage (MsgUserCallIsOver user) = [st|Звонок окончен. #{user} повесил(а) трубку.|]
russianVideoRoomMessage MsgNotGeneratedVAPID = "VAPID не был создан"

defaultVideoRoomMessage :: VideoRoomMessage -> Text
defaultVideoRoomMessage = englishVideoRoomMessage
