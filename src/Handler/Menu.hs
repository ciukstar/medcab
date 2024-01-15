{-# LANGUAGE TemplateHaskell #-}

module Handler.Menu (menu) where

import Foundation
    ( Widget
    , Route (HomeR, AdminR, DocsR)
    , AdminR (TokensR)
    , AppMessage
      ( MsgWelcome, MsgTokens, MsgMainMenu, MsgData, MsgDoctors
      , MsgDocumentation, MsgSourceCode, MsgResources
      )
    )
import Settings (widgetFile)

menu :: Widget
menu = $(widgetFile "menu")
