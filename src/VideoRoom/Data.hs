{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module VideoRoom.Data where

import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent.STM.TVar (TVar)

import Data.Aeson.Types (Value)
import qualified Data.Map as M
import Data.Text (Text)

import Model (PatientId, DoctorId, UserId)

import Yesod.Core (renderRoute)
import Yesod.Core.Dispatch (mkYesodSubData, parseRoutes)

data VideoRoom = VideoRoom
    { channelMapTVar :: TVar (M.Map Text ((TQueue Text,TQueue Text), Int))
    , rtcPeerConnectionConfig :: Maybe Value
    }

mkYesodSubData "VideoRoom" [parseRoutes|
/#PatientId/users/#UserId/doctors/#DoctorId DoctorVideoRoomR GET
/#PatientId/doctors/#DoctorId/users/#UserId PatientVideoRoomR GET
|]

