{-# LANGUAGE TemplateHaskell #-}

module Handler.Menu (menu) where

import Foundation
    ( Widget
    , Route (HomeR, DataR, DocsR)
    , DataR (TokensR, DoctorsR, SpecialtiesR)
    , AppMessage
      ( MsgWelcome, MsgTokens, MsgMainMenu, MsgData, MsgDoctors
      , MsgDocumentation, MsgSourceCode, MsgResources, MsgSpecialties
      )
    )
import Model (Specialties (Specialties))
import Settings (widgetFile)
import Yesod.Core.Handler (getCurrentRoute)

menu :: Widget
menu = do
    curr <- getCurrentRoute
    $(widgetFile "menu")
