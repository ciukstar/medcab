{-# LANGUAGE TemplateHaskell #-}

module Handler.Resources (getDocsR) where

import Database.Persist (Entity (Entity))
import Handler.Menu (menu)
import Foundation
    ( Handler
    , Route (AuthR, AccountR, AccountPhotoR, StaticR)
    , AppMessage
      ( MsgDocumentation, MsgSignIn, MsgSignOut, MsgUserAccount, MsgPhoto
      , MsgErDiagram
      )
    )
import Model (AvatarColor (AvatarColorLight))
import Settings (widgetFile)
import Settings.StaticFiles (js_resources_min_js, img_ERD_MedCab_svg)
import Text.Hamlet (Html)
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core (Yesod(defaultLayout), setUltDestCurrent)
import Yesod.Core.Widget (setTitleI, addScript)


getDocsR :: Handler Html
getDocsR = do
    user <- maybeAuth
    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgDocumentation
        addScript (StaticR js_resources_min_js)
        $(widgetFile "resources/docs")
