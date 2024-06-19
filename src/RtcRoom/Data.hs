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

module RtcRoom.Data where

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


newtype RtcRoom = RtcRoom
    { channelMapTVar :: TVar (M.Map ChanId ((TQueue Text,TQueue Text), Int))
    }


mkYesodSubData "RtcRoom" [parseRoutes|
/photo/#UserId                          PhotoR       GET
/ws/#ChanId/#Bool                       WebSoketR    GET
/push/#UserId/#UserId                   PushMessageR POST
/video/#UserId/#PatientId/#UserId/#Bool VideoR        GET
/audio/#UserId/#PatientId/#UserId/#Bool AudioR       GET
|]

data RtcRoomMessage = MsgAppName
                      | MsgBack
                      | MsgVideoSession
                      | MsgAudioSession
                      | MsgCallEnded
                      | MsgClose
                      | MsgUserCallIsOver Text
                      | MsgNotGeneratedVAPID
                      | MsgUnknown
                      | MsgUserOnCall Text

englishRtcRoomMessage :: RtcRoomMessage -> Text
englishRtcRoomMessage MsgAppName = "MedCab"
englishRtcRoomMessage MsgBack = "Back"
englishRtcRoomMessage MsgVideoSession = "Video Session"
englishRtcRoomMessage MsgAudioSession = "Audio session"
englishRtcRoomMessage MsgCallEnded = "Call ended"
englishRtcRoomMessage MsgClose = "Close"
englishRtcRoomMessage (MsgUserCallIsOver user) = [st|The call is over. #{user} hung up.|]
englishRtcRoomMessage MsgNotGeneratedVAPID = "VAPID not generated"
englishRtcRoomMessage MsgUnknown = "Unknown"
englishRtcRoomMessage (MsgUserOnCall user) = [st|#{user} on call|]

frenchRtcRoomMessage :: RtcRoomMessage -> Text
frenchRtcRoomMessage MsgAppName = "MedCab"
frenchRtcRoomMessage MsgBack = "Retour"
frenchRtcRoomMessage MsgVideoSession = "Séance vidéo"
frenchRtcRoomMessage MsgAudioSession = "Séance audio"
frenchRtcRoomMessage MsgCallEnded = "Appel terminé"
frenchRtcRoomMessage MsgClose = "Fermer"
frenchRtcRoomMessage (MsgUserCallIsOver user) = [st|L'appel est terminé. #{user} a raccroché.|]
frenchRtcRoomMessage MsgNotGeneratedVAPID = "Le VAPID n'a pas été généré"
frenchRtcRoomMessage MsgUnknown = "Inconnu"
frenchRtcRoomMessage (MsgUserOnCall user) = [st|#{user} sur appel|]

romanianRtcRoomMessage :: RtcRoomMessage -> Text
romanianRtcRoomMessage MsgAppName = "MedCab"
romanianRtcRoomMessage MsgBack = "Înapoi"
romanianRtcRoomMessage MsgVideoSession = "Sesiune video"
romanianRtcRoomMessage MsgAudioSession = "Sesiune audio"
romanianRtcRoomMessage MsgCallEnded = "Apel terminat"
romanianRtcRoomMessage MsgClose = "Închide"
romanianRtcRoomMessage (MsgUserCallIsOver user) = [st|Apelul sa încheiat. #{user} a închis.|]
romanianRtcRoomMessage MsgNotGeneratedVAPID = "VAPID nu a fost generat"
romanianRtcRoomMessage MsgUnknown = "Necunoscut"
romanianRtcRoomMessage (MsgUserOnCall user) = [st|#{user} la telefon|]

russianRtcRoomMessage :: RtcRoomMessage -> Text
russianRtcRoomMessage MsgAppName = "MedCab"
russianRtcRoomMessage MsgBack = "Вернуться"
russianRtcRoomMessage MsgVideoSession = "Видеосессия"
russianRtcRoomMessage MsgAudioSession = "Аудиосессия"
russianRtcRoomMessage MsgCallEnded = "Звонок окончен"
russianRtcRoomMessage MsgClose = "Закрыть"
russianRtcRoomMessage (MsgUserCallIsOver user) = [st|Звонок окончен. #{user} повесил(а) трубку.|]
russianRtcRoomMessage MsgNotGeneratedVAPID = "VAPID не был создан"
russianRtcRoomMessage MsgUnknown = "Неизвестный"
russianRtcRoomMessage (MsgUserOnCall user) = [st|#{user} на связи|]

defaultRtcRoomMessage :: RtcRoomMessage -> Text
defaultRtcRoomMessage = englishRtcRoomMessage
