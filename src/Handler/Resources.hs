{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Resources (getDocsR) where

import Foundation
    ( Handler
    , Route (AuthR, StaticR, HomeR, DataR)
    , DataR (UnitsR, StaffR, SpecialtiesR)
    , AppMessage
      ( MsgAppDocumentation, MsgDocumentation, MsgAppName, MsgAppDescription
      , MsgErDiagram, MsgEmail, MsgOverview
      , MsgSuperuser, MsgUsername, MsgPassword, MsgClientId, MsgClientSecret
      , MsgBasicEntities, MsgUser, MsgDoctor, MsgSpecialty, MsgSourceCode
      , MsgIssueTracking, MsgUnitOfMeasure, MsgSearchEngineOptimization
      , MsgConfiguration, MsgPatient
      , MsgDoc001, MsgDoc002, MsgDoc003, MsgDoc004, MsgDoc005, MsgDoc006
      , MsgDoc007, MsgDoc008, MsgDoc009, MsgDoc010, MsgDoc011, MsgDoc012
      , MsgDoc013, MsgDoc014, MsgDoc015, MsgDoc016, MsgDoc017, MsgDoc018
      , MsgDoc019, MsgDoc020, MsgDoc021
      )
    )

import Model (Specialties (Specialties))
    
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
