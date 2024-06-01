{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Home (getHomeR) where

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val
    , (^.), (==.)
    )
import Database.Persist (Entity (Entity))

import Foundation
    ( Handler
    , Route (RecordsR, MyPatientsR, MyDoctorsR)
    , AppMessage
      ( MsgWelcome, MsgAppName, MsgRecordVitalSigns
      , MsgMyPatients, MsgMyDoctors
      )
    )
    
import Model (Doctor, EntityField (DoctorUser))

import Settings (widgetFile)

import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetUser, widgetBanner, widgetSnackbar)

import Yesod.Auth (maybeAuth)
import Yesod.Core (Yesod(defaultLayout), getMessages, setUltDestCurrent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Persist (YesodPersist(runDB), Entity (entityKey))


getHomeR :: Handler Html
getHomeR = do

    user <- maybeAuth

    doctor <- runDB $ selectOne $ do
        x <- from $ table @Doctor
        where_ $ x ^. DoctorUser ==. val (entityKey <$> user)
        return x
    
    msgs <- getMessages
    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgWelcome
        $(widgetFile "homepage")
