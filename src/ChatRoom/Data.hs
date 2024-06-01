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

import Text.Shakespeare.I18N (renderMessage)

import Yesod.Core (renderRoute, mkMessage)
import Yesod.Core.Dispatch (mkYesodSubData, parseRoutes)


newtype ChatRoom = ChatRoom (TVar (M.Map Text (TChan Text, Int)))

mkMessage "ChatRoom" "messages" "en"

mkYesodSubData "ChatRoom" [parseRoutes|
/#PatientId/#UserId/doctor/chat  DoctorChatRoomR  GET
/#PatientId/#UserId/patient/chat PatientChatRoomR GET
|]

