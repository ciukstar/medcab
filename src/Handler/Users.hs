{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Users
  ( getUsersR
  , getUserR
  , getUserCreateR
  , postUserDeleR
  , getUserEditR
  ) where

import Database.Esqueleto.Experimental
    (select, from, table, orderBy
    , (^.), (==.), asc, Entity (entityVal), selectOne, where_, val
    )
import Database.Persist (Entity (Entity), PersistStoreWrite (delete))
import Foundation
    ( Handler, Widget, Form
    , Route (AuthR, DataR, AccountPhotoR, AccountR, StaticR)
    , DataR (UserR, UserCreateR, UsersR, UserDeleR, UserEditR)
    , AppMessage
      ( MsgUsers, MsgAdd, MsgNoUsersYet, MsgSignIn, MsgSignOut, MsgUserAccount
      , MsgPhoto, MsgUser, MsgSave, MsgBack, MsgCancel, MsgEmailAddress, MsgYes
      , MsgAuthentication, MsgPassword, MsgVerificationKey, MsgVerified, MsgNo
      , MsgFullName, MsgGoogle, MsgEmail, MsgEdit, MsgDele, MsgConfirmPlease
      , MsgDeleteAreYouSure, MsgInvalidFormData, MsgRecordDeleted
      )
    )
import Handler.Material3
    ( md3emailField, md3passwordField, md3textField, md3selectField, md3switchField )
import Handler.Menu (menu)
import Model
    ( AvatarColor (AvatarColorLight, AvatarColorDark), statusError, statusSuccess
    , UserId
    , User
      ( User, userEmail, userAuthType, userPassword, userVerkey, userVerified, userName )
    , EntityField (UserId)
    , AuthenticationType (UserAuthTypeGoogle, UserAuthTypeEmail, UserAuthTypePassword)
    )
import Settings (widgetFile)
import Settings.StaticFiles (img_person_FILL0_wght400_GRAD0_opsz24_svg)
import Text.Hamlet (Html)
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Core
    ( defaultLayout, newIdent, getMessages, setUltDestCurrent
    , SomeMessage (SomeMessage), getMessageRender, FileInfo, addMessageI, redirect
    )
import Yesod.Form.Fields (optionsPairs, fileField)
import Yesod.Form.Functions (generateFormPost, mreq, mopt, runFormPost)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsName, fsAttrs, fsId)
    , FieldView (fvInput, fvLabel, fvId)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postUserDeleR :: UserId -> Handler Html
postUserDeleR uid = do
    ((fr,fw),et) <- runFormPost formDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete uid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR UsersR
      _otherwise -> do

          user <- runDB $ selectOne $ do
              x <- from $ table @User
              where_ $ x ^. UserId ==. val uid
              return x
              
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgUser
              $(widgetFile "data/users/user")
          


getUserEditR :: UserId -> Handler Html
getUserEditR uid = do

    user <- runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x
    (fw,et) <- generateFormPost $ formUser user
    defaultLayout $ do
        setTitleI MsgUser
        $(widgetFile "data/users/edit")


getUserR :: UserId -> Handler Html
getUserR uid = do

    user <- runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x
    
    (fw,et) <- generateFormPost formDelete
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUser
        $(widgetFile "data/users/user")


formDelete :: Form ()
formDelete extra = return (FormSuccess (),[whamlet|#{extra}|])


getUserCreateR :: Handler Html
getUserCreateR = do
    (fw,et) <- generateFormPost $ formUser Nothing
    defaultLayout $ do
        setTitleI MsgUser
        $(widgetFile "data/users/create")


formUser :: Maybe (Entity User)
         -> Html -> MForm Handler (FormResult (User,Maybe FileInfo),Widget)
formUser user extra = do

    rndr <- getMessageRender

    let opts = [ (MsgGoogle,UserAuthTypeGoogle)
               , (MsgEmail,UserAuthTypeEmail)
               , (MsgPassword,UserAuthTypePassword)
               ]

    (emailR,emailV) <- mreq md3emailField FieldSettings
        { fsLabel = SomeMessage MsgEmailAddress
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgEmailAddress)]
        } (userEmail . entityVal <$> user)
    (authR,authV) <- mreq (md3selectField (optionsPairs opts)) FieldSettings
        { fsLabel = SomeMessage MsgAuthentication
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgAuthentication)]
        } (userAuthType . entityVal <$> user)
    (passR,passV) <- mopt md3passwordField FieldSettings
        { fsLabel = SomeMessage MsgPassword
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgPassword)]
        } (userPassword . entityVal <$> user)
    (verkeyR,verkeyV) <- mopt md3textField FieldSettings
        { fsLabel = SomeMessage MsgVerificationKey
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgVerificationKey)]
        } (userVerkey . entityVal <$> user)
    (verifiedR,verifiedV) <- mreq md3switchField FieldSettings
        { fsLabel = SomeMessage MsgVerified
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("icons","")]
        } (userVerified . entityVal <$> user)
    (nameR,nameV) <- mopt md3textField FieldSettings
        { fsLabel = SomeMessage MsgFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgFullName)]
        } (userName . entityVal <$> user)

    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    let r = (,) <$> (User <$> emailR <*> authR <*> passR <*> verkeyR <*> verifiedR <*> nameR) <*> photoR

    idLabelPhoto <- newIdent
    idFigurePhoto <- newIdent
    idImgPhoto <- newIdent
    let w = $(widgetFile "data/users/form")

    return (r,w)


getUsersR :: Handler Html
getUsersR = do

    user <- maybeAuth

    users <- runDB $ select $ do
        x <- from $ table @User
        orderBy [asc (x ^. UserId)]
        return x

    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgUsers
        idFabAdd <- newIdent
        $(widgetFile "data/users/users")
