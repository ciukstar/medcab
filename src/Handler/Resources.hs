{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Resources (getDocsR) where

import Foundation
    ( Handler
    , Route (HomeR, AuthR, StaticR, DataR)
    , DataR (UnitsR)
    , AppMessage
      ( MsgAppDocumentation, MsgDocumentation, MsgAppName, MsgAppDescription
      , MsgErDiagram, MsgEmail
      , MsgSuperuser, MsgUsername, MsgPassword, MsgClientId, MsgClientSecret
      , MsgBasicEntities, MsgUser, MsgDoctor, MsgSpecialty, MsgSourceCode
      , MsgIssueTracking, MsgUnitOfMeasure, MsgSearchEngineOptimization
      , MsgConfiguration
      , MsgDoc001, MsgDoc002, MsgDoc003, MsgDoc004, MsgDoc005, MsgDoc006
      , MsgDoc007, MsgDoc008, MsgDoc009, MsgDoc010
      )
    )
    
import Settings (widgetFile)
import Settings.StaticFiles (img_ERD_MedCab_svg)

import Text.Blaze.Html (preEscapedToHtml)
import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetUser, widgetBanner, widgetSnackbar)

import Yesod.Auth (Route (LoginR))
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getMessageRender, getUrlRender
    , getMessages
    )
import Yesod.Core.Widget (setTitleI)


getDocsR :: Handler Html
getDocsR = do
    rndr <- getUrlRender
    translate <- (preEscapedToHtml .) <$> getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgAppDocumentation
        $(widgetFile "resources/docs")
