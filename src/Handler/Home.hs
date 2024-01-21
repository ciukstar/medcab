{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home (getHomeR) where

import Database.Persist (Entity(Entity))
import Handler.Menu (menu)
import Foundation
    ( Handler
    , Route (StaticR, AuthR, HomeR, VideoR, AccountPhotoR, AccountR)
    , AppMessage
      ( MsgWelcome, MsgSignIn, MsgPhoto, MsgSignOut, MsgUserAccount
      )
    )
import Model (statusError, AvatarColor(AvatarColorLight))
import Settings (widgetFile)
import Settings.StaticFiles ( js_homepage_min_js )
import Text.Hamlet (Html)
import Yesod.Auth ( Route (LoginR, LogoutR), maybeAuth )
import Yesod.Core.Widget (addScript, setTitleI)
import Yesod.Core (Yesod(defaultLayout), getMessages, setUltDestCurrent)


getHomeR :: Handler Html
getHomeR = do
    user <- maybeAuth
    msgs <- getMessages
    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgWelcome
        addScript (StaticR js_homepage_min_js)
        $(widgetFile "homepage")
