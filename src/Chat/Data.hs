{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Chat.Data where

import Control.Concurrent.STM.TChan (TChan)
import Data.Text (Text)

import Yesod.Core (renderRoute)
import Yesod.Core.Dispatch (mkYesodSubData, parseRoutes)
import Model (DoctorId)


newtype Chat = Chat (TChan Text)

mkYesodSubData "Chat" [parseRoutes|
/#DoctorId/chat ChatR GET
|]

