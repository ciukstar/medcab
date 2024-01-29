{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Specialties
  ( getSpecialtiesR
  , postSpecialtiesR
  , getSpecialtyCreateR
  , getSpecialtyR
  , getSpecialtyEditR
  , postSpecialtyR
  , postSpecialtyDeleR
  ) where

import Control.Monad (when, unless)
import qualified Data.List.Safe as LS (last)
import Data.Text (Text)

import Database.Esqueleto.Experimental
    ( Entity (entityVal), select, from, table, orderBy, asc, val, where_
    , (^.), (==.)
    , selectOne, isNothing_, just
    )
import Database.Persist (Entity (Entity), PersistStoreWrite (replace, delete))

import Handler.Menu (menu)

import Material3 (md3textField, md3textareaField, md3mreq, md3mopt)
import Model
    ( Specialty
      (Specialty, specialtyName, specialtyCode, specialtyDescr, specialtyGroup)
    , EntityField (SpecialtyName, SpecialtyId, SpecialtyGroup)
    , AvatarColor (AvatarColorLight), statusSuccess, SpecialtyId, statusError
    , Specialties (Specialties)
    )

import Foundation
    ( Handler, Widget
    , Route (DataR, AuthR, AccountR, AccountPhotoR)
    , DataR
      ( SpecialtiesR, SpecialtyCreateR, SpecialtyR, SpecialtyEditR
      , SpecialtyDeleR
      )
    , AppMessage
      ( MsgSpecialties, MsgUserAccount, MsgNoSpecialtiesYet, MsgSignIn
      , MsgSignOut, MsgPhoto, MsgEdit, MsgSpecialty, MsgDescription
      , MsgCode, MsgName, MsgSave, MsgCancel, MsgRecordCreated, MsgTabs
      , MsgBack, MsgDele, MsgRecordEdited, MsgInvalidFormData, MsgRecordDeleted
      , MsgDeleteAreYouSure, MsgConfirmPlease, MsgSubspecialties, MsgDetails
      , MsgNoSubspecialtiesYet, MsgAlreadyExists
      ), Form
    )
import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Auth (Route (LoginR, LogoutR), maybeAuth)
import Yesod.Core
    ( Yesod (defaultLayout), newIdent, SomeMessage (SomeMessage)
    , getMessageRender, addMessageI, redirect, getMessages, whamlet, setUltDestCurrent
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Persist (YesodPersist (runDB), PersistStoreWrite (insert_))
import Yesod.Form.Functions (generateFormPost, runFormPost, checkM)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput), Field
    )


postSpecialtyDeleR :: SpecialtyId -> Specialties -> Handler Html
postSpecialtyDeleR sid ps@(Specialties sids) = do

    specialty <- runDB $ selectOne $ do
        x <- from $ table @Specialty
        where_ $ x ^. SpecialtyId ==. val sid
        return x

    ((fr,fw),et) <- runFormPost formDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete sid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ SpecialtiesR ps
      _otherwise -> defaultLayout $ do
          setTitleI MsgSpecialty
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          idPanelDetails <- newIdent
          $(widgetFile "data/specialties/specialty")


formDelete :: Html -> MForm Handler (FormResult (), Widget)
formDelete extra = return (pure (),[whamlet|#{extra}|])


postSpecialtyR :: SpecialtyId -> Specialties -> Handler Html
postSpecialtyR sid ps@(Specialties sids) = do

    specialty <- runDB $ selectOne $ do
        x <- from $ table @Specialty
        where_ $ x ^. SpecialtyId ==. val sid
        return x

    ((fr,fw),et) <- runFormPost $ formSpecialty Nothing specialty

    case fr of
      FormSuccess r -> do
          runDB $ replace sid r { specialtyGroup = LS.last sids }
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR (SpecialtyR sid ps)
      _otherwise -> defaultLayout $ do
          setTitleI MsgSpecialty
          $(widgetFile "data/specialties/edit")


getSpecialtyEditR :: SpecialtyId -> Specialties -> Handler Html
getSpecialtyEditR sid ps@(Specialties sids) = do

    specialty <- runDB $ selectOne $ do
        x <- from $ table @Specialty
        where_ $ x ^. SpecialtyId ==. val sid
        return x

    (fw,et) <- generateFormPost $ formSpecialty Nothing specialty
    defaultLayout $ do
        setTitleI MsgSpecialty
        $(widgetFile "data/specialties/edit")


getSpecialtyCreateR :: Specialties -> Handler Html
getSpecialtyCreateR ps@(Specialties sids) = do
    (fw,et) <- generateFormPost $ formSpecialty Nothing Nothing
    defaultLayout $ do
        setTitleI MsgSpecialty
        $(widgetFile "data/specialties/create")


formSpecialty :: Maybe SpecialtyId -> Maybe (Entity Specialty) -> Form Specialty
formSpecialty group specialty extra = do
    rndr <- getMessageRender
    
    (nameR,nameV) <- md3mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgName)]
        } (specialtyName . entityVal <$> specialty)
        
    (codeR,codeV) <- md3mopt md3textField FieldSettings
        { fsLabel = SomeMessage MsgCode
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgCode)]
        } (specialtyCode . entityVal <$> specialty)
        
    (descrR,descrV) <- md3mopt md3textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgDescription)]
        } (specialtyDescr . entityVal <$> specialty)

    let r = Specialty <$> nameR <*> codeR <*> descrR <*> pure group
    let w = $(widgetFile "data/specialties/form")
    return (r,w)
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName md3textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Specialty
              where_ $ x ^. SpecialtyName ==. val name
              return x
          return $ case mx of
            Nothing -> Right name
            Just (Entity sid _) -> case specialty of
              Nothing -> Left MsgAlreadyExists
              Just (Entity sid' _) | sid == sid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


getSpecialtyR :: SpecialtyId -> Specialties -> Handler Html
getSpecialtyR sid ps@(Specialties sids) = do

    specialty <- runDB $ selectOne $ do
        x <- from $ table @Specialty
        where_ $ x ^. SpecialtyId ==. val sid
        return x

    (fw,et) <- generateFormPost formDelete

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSpecialty
        idPanelDetails <- newIdent
        $(widgetFile "data/specialties/specialty")


postSpecialtiesR :: Specialties -> Handler Html
postSpecialtiesR ps@(Specialties sids) = do
    ((fr,fw),et) <- runFormPost $ formSpecialty Nothing Nothing
    case fr of
      FormSuccess r -> do
          when (null sids) $ runDB $ insert_ r
          unless (null sids) $ runDB $ insert_ r { specialtyGroup = Just (last sids) }
          addMessageI statusSuccess MsgRecordCreated
          redirect $ DataR $ SpecialtiesR ps
      _otherwise -> defaultLayout $ do
          setTitleI MsgSpecialty
          $(widgetFile "data/specialties/create")


getSpecialtiesR :: Specialties -> Handler Html
getSpecialtiesR ps@(Specialties sids) = do
    user <- maybeAuth
    specialties <- runDB $ select $ do
        x <- from $ table @Specialty
        unless (null sids) $ where_ $ x ^. SpecialtyGroup ==. just (val (last sids))
        when (null sids) $ where_ $ isNothing_ $ x ^. SpecialtyGroup
        orderBy [asc (x ^. SpecialtyName)]
        return x
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSpecialties
        idFabAdd <- newIdent
        setUltDestCurrent
        when (null sids) $ do
            $(widgetFile "data/specialties/specialties")
        unless (null sids) $ do
            setTitleI MsgSpecialties
            idPanelSubspecialties <- newIdent
            $(widgetFile "data/specialties/subspecialties")
