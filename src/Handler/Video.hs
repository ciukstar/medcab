{-# LANGUAGE TemplateHaskell #-}

module Handler.Video (getVideoR) where

import Database.Persist (Entity (Entity))

import Handler.Menu (menu)

import Foundation
    ( Handler
    , Route (HomeR, VideoR, AuthR, AccountR, AccountPhotoR)
    , AppMessage
      ( MsgVideoConference, MsgWelcome, MsgSignIn, MsgUserAccount, MsgPhoto
      , MsgSignOut
      )
    )

import Model (AvatarColor (AvatarColorLight))

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Auth ( Route(LoginR, LogoutR), maybeAuth )
import Yesod.Core (Yesod(defaultLayout))
import Yesod.Core.Widget (setTitleI)

getVideoR :: Handler Html
getVideoR = do
    user <- maybeAuth
    defaultLayout $ do
        setTitleI MsgVideoConference
        $(widgetFile "video/video")
