{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ChatRoom.Data where

import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM.TVar (TVar)

import qualified Data.Map as M
import Data.Text (Text)

import Model (PatientId, UserId)

import Yesod.Core (renderRoute)
import Yesod.Core.Dispatch (mkYesodSubData, parseRoutes)


newtype ChatRoom = ChatRoom (TVar (M.Map Text (TChan Text, Int)))

mkYesodSubData "ChatRoom" [parseRoutes|
/#PatientId/#UserId/doctor/chat  DoctorChatRoomR  GET
/#PatientId/#UserId/patient/chat PatientChatRoomR GET
|]

data ChatRoomMessage = MsgBack
                     | MsgChat
                     | MsgPhoto
                     | MsgMessage
                     | MsgChatParticipantsNotDefined


englishChatRoomMessage :: ChatRoomMessage -> Text
englishChatRoomMessage MsgBack = "Back"
englishChatRoomMessage MsgChat = "Chat"
englishChatRoomMessage MsgPhoto = "Photo"
englishChatRoomMessage MsgMessage = "Message"
englishChatRoomMessage MsgChatParticipantsNotDefined = "Chat participants are not defined"


frenchChatRoomMessage :: ChatRoomMessage -> Text
frenchChatRoomMessage MsgBack = "Retour"
frenchChatRoomMessage MsgChat = "Chat"
frenchChatRoomMessage MsgPhoto = "Photo"
frenchChatRoomMessage MsgMessage = "Message"
frenchChatRoomMessage MsgChatParticipantsNotDefined = "Les participants au chat ne sont pas définis"


romanianChatRoomMessage :: ChatRoomMessage -> Text
romanianChatRoomMessage MsgBack = "Înapoi"
romanianChatRoomMessage MsgChat = "Chat"
romanianChatRoomMessage MsgPhoto = "Foto"
romanianChatRoomMessage MsgMessage = "Mesaj"
romanianChatRoomMessage MsgChatParticipantsNotDefined = "Participanții la chat nu sunt definiți"


russianChatRoomMessage :: ChatRoomMessage -> Text
russianChatRoomMessage MsgBack = "Вернуться"
russianChatRoomMessage MsgChat = "Chat"
russianChatRoomMessage MsgPhoto = "Фото"
russianChatRoomMessage MsgMessage = "Сообщение"
russianChatRoomMessage MsgChatParticipantsNotDefined = "Участники чата не определены"


defaultChatRoomMessage :: ChatRoomMessage -> Text
defaultChatRoomMessage = englishChatRoomMessage
