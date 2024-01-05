{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Accounts
  ( getAccountPhotoR
  , getAccountCreateR
  , postAccountsR
  ) where

import Database.Persist (Entity(Entity), PersistStoreWrite (insert_))
import Data.FileEmbed (embedFile)
import Database.Esqueleto.Experimental
    (selectOne, from, table, where_, (^.), (==.), val)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Model
    ( UserId, UserPhoto (UserPhoto), EntityField (UserPhotoUser)
    , User (User), AuthenticationType (UserAuthTypePassword)
    )
import Foundation
    ( Handler, Widget
    , Route (HomeR, AccountsR, StaticR)
    , AppMessage
      ( MsgUserAccount, MsgBack, MsgCancel, MsgEmailAddress
      , MsgPassword, MsgFullName, MsgUserRegistered, MsgSignUp
      )
    )
import Settings (widgetFile)
import Settings.StaticFiles (js_account_min_js)
import Text.Hamlet (Html)
import Yesod.Core
    ( Yesod(defaultLayout), SomeMessage (SomeMessage), getMessageRender
    , MonadHandler (liftHandler), addMessageI, redirect
    )
import Yesod.Core.Content
    (TypedContent (TypedContent), typeSvg, ToContent (toContent))
import Yesod.Core.Widget (setTitleI, whamlet, addScript)
import Yesod.Form.Functions (runFormPost, generateFormPost, mreq)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), FieldView (fvInput)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Persist (YesodPersist(runDB))

import Handler.Material3
    ( m3textField, m3passwordField, m3emailField )


postAccountsR :: Handler Html
postAccountsR = do
    ((fr,fw),et) <- runFormPost formAccount
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI info MsgUserRegistered
          redirect HomeR
      _otherwise ->  defaultLayout $ do
          setTitleI MsgUserAccount
          addScript (StaticR js_account_min_js)
          $(widgetFile "account/create")


getAccountCreateR :: Handler Html
getAccountCreateR = do
    (fw,et) <- generateFormPost $ formAccount
    defaultLayout $ do
        setTitleI MsgUserAccount
        addScript (StaticR js_account_min_js)
        $(widgetFile "account/create")


formAccount :: Html -> MForm Handler (FormResult User, Widget)
formAccount extra = do
    msgRender <- liftHandler getMessageRender
    (emailR,emailV) <- mreq m3emailField FieldSettings
        { fsLabel = SomeMessage MsgEmailAddress
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgRender MsgEmailAddress)]
        } Nothing
    (passR,passV) <- mreq m3passwordField FieldSettings
        { fsLabel = SomeMessage MsgPassword
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgRender MsgPassword)]
        } Nothing
    (nameR,nameV) <- mreq m3textField FieldSettings
        { fsLabel = SomeMessage MsgFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgRender MsgFullName)]
        } Nothing
    return ( User <$> emailR <*> (pure <$> passR) <*> pure UserAuthTypePassword <*> (pure <$> nameR)
           , [whamlet|
#{extra}
$forall v <- [emailV,passV,nameV]
  ^{fvInput v}
|]
           )
      

getAccountPhotoR :: UserId -> Handler TypedContent
getAccountPhotoR uid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @UserPhoto
        where_ $ x ^. UserPhotoUser ==. val uid
        return x
    return $ case photo of
      Just (Entity _ (UserPhoto _ mime bs)) -> TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> TypedContent typeSvg
        $ toContent $(embedFile "static/img/person_FILL0_wght400_GRAD0_opsz24.svg")


info :: Text
info = "info"
