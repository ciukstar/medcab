{-# LANGUAGE TemplateHaskell #-}

module Menu (menu) where

import Foundation
    ( Widget
    , Route (HomeR, DataR, DocsR, DoctorsR, RecordsR, PatientsR)
    , DataR (UsersR, TokensR, StaffR, SpecialtiesR, UnitsR, MedSignsR)
    , AppMessage
      ( MsgWelcome, MsgTokens, MsgMainMenu, MsgData, MsgDoctors, MsgUsers
      , MsgDocumentation, MsgSourceCode, MsgResources, MsgSpecialties
      , MsgMeasurementUnits, MsgMedicalSigns, MsgRecords, MsgPatients
      )
    )
import Model (Specialties (Specialties))

import Settings (widgetFile)

import Yesod.Core.Handler (getCurrentRoute)

menu :: Widget
menu = do
    curr <- getCurrentRoute
    $(widgetFile "menu")
