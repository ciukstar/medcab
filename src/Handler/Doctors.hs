{-# LANGUAGE TemplateHaskell #-}

module Handler.Doctors (getDoctorsR) where

import Handler.Menu (menu)
import Foundation
    (Handler
    , Route (StaticR, AuthR, AccountR, AccountPhotoR)
    , AppMessage
      ( MsgDoctors, MsgUserAccount, MsgSignOut, MsgSignIn, MsgPhoto, MsgEdit
      )
    )
import Model (AvatarColor (AvatarColorLight))
import Settings (widgetFile)
import Settings.StaticFiles (js_doctors_min_js)
import Text.Hamlet (Html)
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core (Yesod(defaultLayout), setTitleI, addScript, newIdent)
import Yesod.Persist (Entity (Entity))


getDoctorsR :: Handler Html
getDoctorsR = do
    user <- maybeAuth
    defaultLayout $ do
        setTitleI MsgDoctors
        addScript (StaticR js_doctors_min_js)
        idFabAdd <- newIdent
        $(widgetFile "admin/doctors/doctors")
