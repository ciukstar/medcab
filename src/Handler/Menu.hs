{-# LANGUAGE TemplateHaskell #-}

module Handler.Menu (menu) where

import Foundation
    ( Widget
    , Route (HomeR, AdminR, DocsR)
    , AdminR (TokensR, DoctorsR)
    , AppMessage
      ( MsgWelcome, MsgTokens, MsgMainMenu, MsgData, MsgDoctors
      , MsgDocumentation, MsgSourceCode, MsgResources
      )
    )
import Settings (widgetFile)
import Yesod.Core.Handler (getCurrentRoute)

menu :: Widget
menu = do
    curr <- getCurrentRoute
    $(widgetFile "menu")
