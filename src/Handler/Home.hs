{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home (getHomeR) where

import Foundation
    ( Handler
    , Route (RecordsR, DoctorsR)
    , AppMessage
      ( MsgWelcome, MsgAppName, MsgFindDoctor, MsgRecordVitalSigns
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetUser, widgetBanner, widgetSnackbar)

import Yesod.Core (Yesod(defaultLayout), getMessages, setUltDestCurrent)
import Yesod.Core.Widget (setTitleI)


getHomeR :: Handler Html
getHomeR = do
    msgs <- getMessages
    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgWelcome
        $(widgetFile "homepage")
