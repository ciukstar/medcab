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

import Yesod.Core (renderRoute, PathPiece)
import Yesod.Core.Dispatch (mkYesodSubData, parseRoutes)

newtype ChanId = ChanId Int
    deriving (Eq, Ord, Show, Read, PathPiece, ToJSON, FromJSON)


newtype VideoRoom = VideoRoom
    { channelMapTVar :: TVar (M.Map ChanId ((TQueue Text,TQueue Text), Int))
    }


mkYesodSubData "VideoRoom" [parseRoutes|
/photo/#UserId                         PhotoR       GET
/ws/#ChanId/#Bool                      WebSoketR    GET
/api/push                              PushMessageR POST
/room/#UserId/#PatientId/#UserId/#Bool RoomR        GET
|]
