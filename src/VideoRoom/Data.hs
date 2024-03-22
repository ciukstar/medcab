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

module VideoRoom.Data where

import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent.STM.TVar (TVar)

import qualified Data.Map as M
import Data.Text (Text)

import Database.Persist.Sql (fromSqlKey)

import Model (UserId)

import Yesod.Core (renderRoute)
import Yesod.Core.Dispatch (mkYesodSubData, parseRoutes)

newtype ChanId = ChanId (UserId, UserId)


instance Ord ChanId where
    (<=) :: ChanId -> ChanId -> Bool
    ChanId (uid1,uid2) <= ChanId (uid1',uid2') =
        fromSqlKey uid1 + fromSqlKey uid2 <= fromSqlKey uid1' + fromSqlKey uid2'


instance Eq ChanId where
    (==) :: ChanId -> ChanId -> Bool
    ChanId (uid1,uid2) == ChanId (uid1',uid2') =
        (uid1 == uid1' && uid2 == uid2') || (uid1 == uid2' && uid2 == uid1')


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
/#UserId/#UserId/#Bool/x DoctorVideoRoomR  GET
/#UserId/#UserId/#Bool/y PatientVideoRoomR GET
/api/push                PushMessageR      POST
|]
