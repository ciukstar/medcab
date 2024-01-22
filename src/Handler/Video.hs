{-# LANGUAGE TemplateHaskell #-}

module Handler.Video (getVideoR) where

import Handler.Menu (menu)
import Foundation
    ( Handler
    , Route (HomeR, VideoR, AuthR)
    , AppMessage (MsgVideoConference, MsgWelcome, MsgSignIn)
    )
import Text.Hamlet (Html)
import Settings (widgetFile)
import Yesod.Auth ( Route(LoginR) ) 
import Yesod.Core (Yesod(defaultLayout))
import Yesod.Core.Widget (setTitleI)

getVideoR :: Handler Html
getVideoR = do
    defaultLayout $ do
        setTitleI MsgVideoConference
        $(widgetFile "video/video")
