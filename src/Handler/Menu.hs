{-# LANGUAGE TemplateHaskell #-}

module Handler.Menu (menu) where

import Foundation
    ( Widget
    , Route (HomeR, DataR, DocsR)
    , DataR (UsersR, TokensR, DoctorsR, SpecialtiesR, UnitsR, MedSignsR)
    , AppMessage
      ( MsgWelcome, MsgTokens, MsgMainMenu, MsgData, MsgDoctors, MsgUsers
      , MsgDocumentation, MsgSourceCode, MsgResources, MsgSpecialties
      , MsgMeasurementUnits, MsgMedicalSigns
      )
    )
import Model (Specialties (Specialties))
import Settings (widgetFile)
import Yesod.Core.Handler (getCurrentRoute)

menu :: Widget
menu = do
    curr <- getCurrentRoute
    $(widgetFile "menu")
