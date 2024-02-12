{-# LANGUAGE TemplateHaskell #-}

module Menu (menu) where

import Database.Persist (Entity(Entity))

import Foundation
    ( Widget
    , Route (HomeR, DataR, DocsR, AuthR, RecordsR)
    , DataR (UsersR, TokensR, DoctorsR, SpecialtiesR, UnitsR, MedSignsR)
    , AppMessage
      ( MsgWelcome, MsgTokens, MsgMainMenu, MsgData, MsgDoctors, MsgUsers
      , MsgDocumentation, MsgSourceCode, MsgResources, MsgSpecialties
      , MsgMeasurementUnits, MsgMedicalSigns, MsgRecords
      )
    )
import Model (Specialties (Specialties))

import Settings (widgetFile)

import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core.Handler (getCurrentRoute)

menu :: Widget
menu = do
    user <- maybeAuth
    curr <- getCurrentRoute
    $(widgetFile "menu")
