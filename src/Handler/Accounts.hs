{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Accounts
  ( getAccountPhotoR
  , getAccountEditR
  , getAccountsR
  , getAccountR
  , postAccountR
  ) where

import Database.Persist (Entity(Entity, entityVal))
import Database.Esqueleto.Experimental
    (selectOne, from, table, where_, (^.), (==.), val)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Model
    ( UserId, UserPhoto (UserPhoto)
    , EntityField (UserPhotoUser)
    , User (User, userName)
    )
import Foundation
    ( Handler, Widget
    , Route
      ( HomeR, AccountsR, StaticR, AuthR, AccountPhotoR, AccountEditR, AccountR)
    , AppMessage
      ( MsgUserAccount, MsgBack, MsgCancel
      , MsgFullName, MsgSignOut, MsgPhoto
      , MsgSave
      )
    )
import Settings (widgetFile)
import Settings.StaticFiles
    ( js_account_min_js, img_person_FILL0_wght400_GRAD0_opsz24_white_svg )
import Text.Hamlet (Html)
import Yesod.Auth (Route (LogoutR), maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), SomeMessage (SomeMessage), getMessageRender
    , MonadHandler (liftHandler), redirect
    )
import Yesod.Core.Content
    (TypedContent (TypedContent), ToContent (toContent))
import Yesod.Core.Widget (setTitleI, whamlet, addScript)
import Yesod.Form.Functions (generateFormPost, mopt)
import Yesod.Form.Types
    ( MForm, FormResult, FieldView (fvInput)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Persist (YesodPersist(runDB))

import Handler.Material3
    ( md3textField )


postAccountR :: UserId -> Handler Html
postAccountR uid = undefined


getAccountR :: UserId -> Handler Html
getAccountR uid = do
    user <- maybeAuth
    defaultLayout $ do
        setTitleI MsgUserAccount
        addScript (StaticR js_account_min_js)
        $(widgetFile "accounts/account")


getAccountsR :: Handler Html
getAccountsR = undefined


getAccountEditR :: UserId -> Handler Html
getAccountEditR uid = do
    user <- maybeAuth
    (fw,et) <- generateFormPost $ formAccount user
    defaultLayout $ do
        setTitleI MsgUserAccount
        addScript (StaticR js_account_min_js)
        $(widgetFile "accounts/edit")


formAccount :: Maybe (Entity User) -> Html -> MForm Handler (FormResult (Maybe Text), Widget)
formAccount user extra = do
    msgRender <- liftHandler getMessageRender
    (nameR,nameV) <- mopt md3textField FieldSettings
        { fsLabel = SomeMessage MsgFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgRender MsgFullName)]
        } (userName . entityVal <$> user)
    return ( nameR
           , [whamlet|
                     #{extra}
                     $forall v <- [nameV]
                       ^{fvInput v}
                     |]
           )
      

getAccountPhotoR :: UserId -> Handler TypedContent
getAccountPhotoR uid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @UserPhoto
        where_ $ x ^. UserPhotoUser ==. val uid
        return x
    case photo of
      Just (Entity _ (UserPhoto _ mime bs)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_person_FILL0_wght400_GRAD0_opsz24_white_svg

