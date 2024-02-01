{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Resources (getDocsR) where

import Database.Persist (Entity (Entity))
import Handler.Menu (menu)
import Foundation
    ( Handler
    , Route (HomeR, AuthR, AccountR, AccountPhotoR, StaticR)
    , AppMessage
      ( MsgDocumentation, MsgAppName, MsgAppDescription, MsgSignIn, MsgSignOut
      , MsgUserAccount, MsgPhoto, MsgErDiagram, MsgEmail, MsgSuperuser
      , MsgUsername, MsgPassword, MsgClientId, MsgClientSecret, MsgBasicEntities
      , MsgUser, MsgDoctor, MsgSpecialty, MsgSearchEngineOptimization
      , MsgSourceCode, MsgIssueTracking
      , MsgDoc001, MsgDoc002, MsgDoc003, MsgDoc004, MsgDoc005, MsgDoc006
      , MsgDoc007
      )
    )
import Model (AvatarColor (AvatarColorLight))

import Settings (widgetFile)
import Settings.StaticFiles (img_ERD_MedCab_svg)

import Text.Blaze.Html (preEscapedToHtml)
import Text.Hamlet (Html)

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getMessageRender, getUrlRender )
import Yesod.Core.Widget (setTitleI)


getDocsR :: Handler Html
getDocsR = do
    user <- maybeAuth
    rndr <- getUrlRender
    translate <- (preEscapedToHtml .) <$> getMessageRender
    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgDocumentation
        $(widgetFile "resources/docs")
