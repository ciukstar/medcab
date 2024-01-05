{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home (getHomeR) where

import Database.Persist (Entity(Entity))
import Foundation
    ( Handler
    , Route (StaticR, AuthR, HomeR, VideoR, AccountPhotoR)
    , AppMessage
      ( MsgWelcome, MsgMainMenu, MsgSignIn, MsgPhoto
      )
    )
import Text.Hamlet (Html)
import Settings (widgetFile)
import Settings.StaticFiles ( js_homepage_min_js )
import Yesod.Auth ( Route (LoginR), maybeAuth )
import Yesod.Core.Widget (addScript, setTitleI)
import Yesod.Core (Yesod(defaultLayout))


getHomeR :: Handler Html
getHomeR = do
    user <- maybeAuth
    defaultLayout $ do
        setTitleI MsgWelcome
        addScript (StaticR js_homepage_min_js)
        $(widgetFile "homepage")
