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
  , getUserPhotoR
  , postUserR
  ) where

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc, Entity (entityVal), selectOne, where_
    , (^.), (==.), (=.)
    , val, update, set, Value (unValue)
    )
import Database.Persist
    ( Entity (Entity), PersistStoreWrite (delete), PersistUniqueWrite (upsert) )
import qualified Database.Persist as P ((=.))
import Foundation
    ( Handler, Widget, Form
    , Route (AuthR, DataR, AccountPhotoR, AccountR, StaticR)
    , DataR (UserR, UserCreateR, UsersR, UserDeleR, UserEditR, UserPhotoR)
    , AppMessage
      ( MsgUsers, MsgAdd, MsgNoUsersYet, MsgSignIn, MsgSignOut, MsgUserAccount
      , MsgPhoto, MsgUser, MsgSave, MsgBack, MsgCancel, MsgEmailAddress, MsgYes
      , MsgAuthentication, MsgPassword, MsgVerificationKey, MsgVerified, MsgNo
      , MsgFullName, MsgGoogle, MsgEmail, MsgEdit, MsgDele, MsgConfirmPlease
      , MsgDeleteAreYouSure, MsgInvalidFormData, MsgRecordDeleted, MsgNotSpecified
      , MsgRecordEdited, MsgSuperuser, MsgAdministrator, MsgSuperuserCannotBeDeleted
      )
    )
import Handler.Material3
    ( md3textField, md3switchField )
import Handler.Menu (menu)
import Model
    ( AvatarColor (AvatarColorLight, AvatarColorDark), statusError, statusSuccess
    , UserId
    , User
      ( User, userEmail, userAuthType, userPassword, userVerkey, userVerified, userName )
    , EntityField
      ( UserId, UserPhotoUser, UserEmail, UserAuthType, UserVerkey, UserVerified
      , UserName, UserPhotoMime, UserPhotoPhoto, UserAdmin, UserSuperuser
      )
    , AuthenticationType
      ( UserAuthTypeGoogle, UserAuthTypeEmail, UserAuthTypePassword)
    , UserPhoto (UserPhoto)
    )
import Settings (widgetFile)
import Settings.StaticFiles
    ( img_person_FILL0_wght400_GRAD0_opsz24_svg
    , img_shield_person_FILL0_wght400_GRAD0_opsz24_svg
    )
import Text.Hamlet (Html)
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Core
    ( defaultLayout, newIdent, getMessages, setUltDestCurrent
    , SomeMessage (SomeMessage), getMessageRender, FileInfo (fileContentType), addMessageI
    , redirect, TypedContent (TypedContent), ToContent (toContent), fileSourceByteString
    )
import Yesod.Form.Fields (fileField)
import Yesod.Form.Functions (generateFormPost, mreq, mopt, runFormPost)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsName, fsAttrs, fsId)
    , FieldView (fvInput, fvLabel, fvId)
    )
import Yesod.Persist.Core (YesodPersist(runDB))
import Data.Text.Encoding (encodeUtf8)
import Control.Monad (void)
import Data.Text (Text)


postUserDeleR :: UserId -> Handler Html
postUserDeleR uid = do
    superuser <- maybe False unValue <$> runDB ( selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        where_ $ x ^. UserSuperuser ==. val True
        return $ x ^. UserSuperuser )
        
    ((fr,fw),et) <- runFormPost formDelete
    case (fr,superuser) of
      (FormSuccess (),False) -> do
          runDB $ delete uid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR UsersR
      (FormSuccess (),True) -> do
          addMessageI statusError MsgSuperuserCannotBeDeleted
          redirect $ DataR $ UserR uid
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


postUserR :: UserId -> Handler Html
postUserR uid = do
    ((fr,fw),et) <- runFormPost $ formUser Nothing
    case fr of
      FormSuccess (UserData name admin mfi) -> do
          runDB $ update $ \x -> do
              set x [ UserName =. val name, UserAdmin =. val admin ]
              where_ $ x ^. UserId ==. val uid

          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                void $ runDB $ upsert (UserPhoto uid (fileContentType fi) bs)
                    [UserPhotoMime P.=. fileContentType fi, UserPhotoPhoto P.=. bs]
            Nothing -> return ()
              
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ UserR uid
      _otherwise -> do
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


data UserData = UserData
    { userDataName :: Maybe Text
    , userDataAdmin :: Bool
    , userDataPhoto :: Maybe FileInfo
    }


formUser :: Maybe (Entity User)
         -> Html -> MForm Handler (FormResult UserData, Widget)
formUser user extra = do

    rndr <- getMessageRender
    
    (nameR,nameV) <- mopt md3textField FieldSettings
        { fsLabel = SomeMessage MsgFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgFullName)]
        } (userName . entityVal <$> user)
    (adminR,adminV) <- mreq md3switchField FieldSettings
        { fsLabel = SomeMessage MsgAdministrator
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("icons","")]
        } (userVerified . entityVal <$> user)

    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    let r = UserData <$> nameR <*> adminR <*> photoR

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


getUserPhotoR :: UserId -> Handler TypedContent
getUserPhotoR uid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @UserPhoto
        where_ $ x ^. UserPhotoUser ==. val uid
        return x
    case photo of
      Just (Entity _ (UserPhoto _ mime bs)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> do
          superuser <- maybe False unValue <$> runDB ( selectOne $ do
              x <- from $ table @User
              where_ $ x ^. UserId ==. val uid
              return $ x ^. UserSuperuser )
          redirect $ if superuser
              then StaticR img_shield_person_FILL0_wght400_GRAD0_opsz24_svg
              else StaticR img_person_FILL0_wght400_GRAD0_opsz24_svg
