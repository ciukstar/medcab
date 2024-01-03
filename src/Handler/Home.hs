{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home (getHomeR) where

import Foundation
    ( Handler
    , Route (StaticR, AuthR, HomeR, VideoR)
    , AppMessage
      ( MsgWelcome, MsgMainMenu, MsgSignIn
      )
    )
import Text.Hamlet (Html)
import Settings (widgetFile)
import Settings.StaticFiles ( js_homepage_min_js )
import Yesod.Auth ( Route (LoginR) )
import Yesod.Core.Widget (addScript, setTitleI)
import Yesod.Core (Yesod(defaultLayout))


getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitleI MsgWelcome
        addScript (StaticR js_homepage_min_js)
        $(widgetFile "homepage")
