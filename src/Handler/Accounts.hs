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

import Database.Persist
    ( Entity(Entity, entityVal), PersistUniqueWrite (upsert))
import qualified Database.Persist as P ((=.))
import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val, update, set
    , (^.), (==.), (=.)
    )
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Model
    ( UserId, UserPhoto (UserPhoto), statusSuccess
    , EntityField (UserPhotoUser, UserPhotoPhoto, UserPhotoMime, UserName, UserId)
    , User (User, userName), AvatarColor (AvatarColorDark, AvatarColorLight)
    )
import Foundation
    ( Handler, Widget
    , Route
      ( HomeR, StaticR, AuthR, AccountPhotoR, AccountEditR, AccountR)
    , AppMessage
      ( MsgUserAccount, MsgBack, MsgCancel, MsgFullName, MsgSignOut, MsgPhoto
      , MsgSave, MsgRecordEdited
      )
    )
import Settings (widgetFile)
import Settings.StaticFiles
    ( js_account_min_js, img_person_FILL0_wght400_GRAD0_opsz24_white_svg
    , img_person_FILL0_wght400_GRAD0_opsz24_svg
    )
import Text.Hamlet (Html)
import Yesod.Auth (Route (LogoutR), maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), SomeMessage (SomeMessage), getMessageRender
    , MonadHandler (liftHandler), redirect, FileInfo (fileContentType)
    , newIdent, fileSourceByteString, addMessageI
    )
import Yesod.Core.Content
    (TypedContent (TypedContent), ToContent (toContent))
import Yesod.Core.Widget (setTitleI, addScript)
import Yesod.Form.Functions (generateFormPost, mopt, runFormPost)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), FieldView (fvInput, fvId)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Persist (YesodPersist(runDB))

import Handler.Material3
    ( md3textField )
import Yesod.Form.Fields (fileField)
import Control.Monad (void)


postAccountR :: UserId -> Handler Html
postAccountR uid = do
    ((fr,fw),et) <- runFormPost $ formAccount Nothing
    case fr of
      FormSuccess (mname,mfi) -> do
          runDB $ update $ \x -> do
              set x [ UserName =. val mname ]
              where_ $ x ^. UserId ==. val uid
          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                void $ runDB $ upsert (UserPhoto uid (fileContentType fi) bs)
                    [UserPhotoMime P.=. fileContentType fi, UserPhotoPhoto P.=. bs]
            Nothing -> return ()
          addMessageI statusSuccess MsgRecordEdited
          redirect $ AccountR uid
      _otherwise -> defaultLayout $ do
          setTitleI MsgUserAccount
          $(widgetFile "accounts/edit")


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


formAccount :: Maybe (Entity User)
            -> Html -> MForm Handler (FormResult (Maybe Text,Maybe FileInfo), Widget)
formAccount user extra = do
    rndr <- liftHandler getMessageRender
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

    idUserIdent <- newIdent
    idLabelPhotoUser <- newIdent
    idFigurePhotoUser <- newIdent
    idImgPhotoUser <- newIdent

    return ( (,) <$> nameR <*> photoR
           , $(widgetFile "accounts/form")
           )
      

getAccountPhotoR :: UserId -> AvatarColor -> Handler TypedContent
getAccountPhotoR uid color = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @UserPhoto
        where_ $ x ^. UserPhotoUser ==. val uid
        return x
    case photo of
      Just (Entity _ (UserPhoto _ mime bs)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ case color of
        AvatarColorDark -> StaticR img_person_FILL0_wght400_GRAD0_opsz24_svg
        AvatarColorLight -> StaticR img_person_FILL0_wght400_GRAD0_opsz24_white_svg
        

