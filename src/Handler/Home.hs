{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home (getHomeR) where

import Database.Persist (Entity(Entity))
import Handler.Menu (menu)
import Foundation
    ( Handler
    , Route (AuthR, HomeR, VideoR, AccountPhotoR, AccountR, RecordsR)
    , AppMessage
      ( MsgWelcome, MsgSignIn, MsgPhoto, MsgSignOut, MsgUserAccount
      , MsgElectronicHealthRecord
      )
    )
import Model (statusError, AvatarColor(AvatarColorLight))
import Settings (widgetFile)
import Text.Hamlet (Html)
import Yesod.Auth ( Route (LoginR, LogoutR), maybeAuth )
import Yesod.Core.Widget (setTitleI)
import Yesod.Core (Yesod(defaultLayout), getMessages, setUltDestCurrent)


getHomeR :: Handler Html
getHomeR = do
    user <- maybeAuth
    msgs <- getMessages
    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgWelcome
        $(widgetFile "homepage")
