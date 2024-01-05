{-# LANGUAGE TemplateHaskell #-}

module Handler.Video (getVideoR) where

import Foundation
    ( Handler
    , Route (StaticR, HomeR, VideoR, AuthR)
    , AppMessage (MsgVideoConference, MsgWelcome, MsgSignIn, MsgMainMenu)
    )
import Text.Hamlet (Html)
import Settings (widgetFile)
import Settings.StaticFiles (js_homepage_min_js)
import Yesod.Auth ( Route(LoginR) ) 
import Yesod.Core (Yesod(defaultLayout), addScript)
import Yesod.Core.Widget (setTitleI)

getVideoR :: Handler Html
getVideoR = do
    defaultLayout $ do
        setTitleI MsgVideoConference
        addScript (StaticR js_homepage_min_js)
        $(widgetFile "video/video")
