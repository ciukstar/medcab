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

import Database.Esqueleto.Experimental
    ( Entity (entityVal), select, from, table, orderBy, asc, val, where_
    , (^.), (==.)
    , selectOne
    )
import Database.Persist (Entity (Entity), PersistStoreWrite (replace, delete))
import Handler.Material3 (md3textField, md3textareaField)
import Handler.Menu (menu) 
import Model
    ( Specialty(Specialty, specialtyName, specialtyCode, specialtyDescr)
    , EntityField (SpecialtyName, SpecialtyId)
    , AvatarColor (AvatarColorLight), statusSuccess, SpecialtyId, statusError
    )
import Foundation
    ( Handler, Widget
    , Route (DataR, AuthR, AccountR, AccountPhotoR, StaticR)
    , DataR
      ( SpecialtiesR, SpecialtyCreateR, SpecialtyR, SpecialtyEditR
      , SpecialtyDeleR
      )
    , AppMessage
      ( MsgSpecialties, MsgUserAccount, MsgNoSpecialtiesYet, MsgSignIn
      , MsgSignOut, MsgPhoto, MsgEdit, MsgSpecialty, MsgDescription
      , MsgCode, MsgName, MsgSave, MsgCancel, MsgRecordCreated
      , MsgBack, MsgDele, MsgRecordEdited, MsgInvalidFormData, MsgRecordDeleted
      , MsgDeleteAreYouSure, MsgConfirmPlease
      )
    )
import Settings (widgetFile)
import Settings.StaticFiles
    ( js_specialties_specialties_min_js, js_specialties_create_min_js
    , js_specialties_specialty_min_js
    )
import Text.Hamlet (Html)
import Yesod.Auth (Route (LoginR, LogoutR), maybeAuth) 
import Yesod.Core
    ( Yesod (defaultLayout), newIdent, addScript, SomeMessage (SomeMessage)
    , getMessageRender, addMessageI, redirect, getMessages, whamlet
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Persist (YesodPersist (runDB), PersistStoreWrite (insert_))
import Yesod.Form.Functions (generateFormPost, mreq, mopt, runFormPost)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput)
    )


postSpecialtyDeleR :: SpecialtyId -> Handler Html
postSpecialtyDeleR sid = do
    
    specialty <- runDB $ selectOne $ do
        x <- from $ table @Specialty
        where_ $ x ^. SpecialtyId ==. val sid
        return x
        
    ((fr,fw),et) <- runFormPost formDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete sid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR SpecialtiesR
      _otherwise -> defaultLayout $ do
          setTitleI MsgSpecialty
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          addScript (StaticR js_specialties_specialty_min_js)
          $(widgetFile "data/specialties/specialty")


formDelete :: Html -> MForm Handler (FormResult (), Widget)
formDelete extra = return (pure (),[whamlet|#{extra}|])


postSpecialtyR :: SpecialtyId -> Handler Html
postSpecialtyR sid = do
    
    specialty <- runDB $ selectOne $ do
        x <- from $ table @Specialty
        where_ $ x ^. SpecialtyId ==. val sid
        return x
        
    ((fr,fw),et) <- runFormPost $ formSpecialty specialty
    case fr of
      FormSuccess r -> do
          runDB $ replace sid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR (SpecialtyR sid)
      _otherwise -> defaultLayout $ do
          setTitleI MsgSpecialty
          addScript (StaticR js_specialties_create_min_js)
          $(widgetFile "data/specialties/edit")
    

getSpecialtyEditR :: SpecialtyId -> Handler Html
getSpecialtyEditR sid = do
    
    specialty <- runDB $ selectOne $ do
        x <- from $ table @Specialty
        where_ $ x ^. SpecialtyId ==. val sid
        return x

    (fw,et) <- generateFormPost $ formSpecialty specialty
    defaultLayout $ do
        setTitleI MsgSpecialty
        addScript (StaticR js_specialties_create_min_js)
        $(widgetFile "data/specialties/edit")


getSpecialtyCreateR :: Handler Html
getSpecialtyCreateR = do
    (fw,et) <- generateFormPost $ formSpecialty Nothing
    defaultLayout $ do
        setTitleI MsgSpecialty
        addScript (StaticR js_specialties_create_min_js)
        $(widgetFile "data/specialties/create")


formSpecialty :: Maybe (Entity Specialty)
              -> Html -> MForm Handler (FormResult Specialty, Widget)
formSpecialty specialty extra = do
    rndr <- getMessageRender
    (nameR,nameV) <- mreq md3textField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgName)]
        } (specialtyName . entityVal <$> specialty)
    (codeR,codeV) <- mopt md3textField FieldSettings
        { fsLabel = SomeMessage MsgCode
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgCode)]
        } (specialtyCode . entityVal <$> specialty)
    (descrR,descrV) <- mopt md3textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgDescription)]
        } (specialtyDescr . entityVal <$> specialty)

    let r = Specialty <$> nameR <*> codeR <*> descrR <*> pure Nothing
    let w = $(widgetFile "data/specialties/form")
    return (r,w)


getSpecialtyR :: SpecialtyId -> Handler Html
getSpecialtyR sid = do
    
    specialty <- runDB $ selectOne $ do
        x <- from $ table @Specialty
        where_ $ x ^. SpecialtyId ==. val sid
        return x

    (fw,et) <- generateFormPost formDelete
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSpecialty
        addScript (StaticR js_specialties_specialty_min_js)
        $(widgetFile "data/specialties/specialty")


postSpecialtiesR :: Handler Html
postSpecialtiesR = do
    ((fr,fw),et) <- runFormPost $ formSpecialty Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordCreated
          redirect $ DataR SpecialtiesR 
      _otherwise -> defaultLayout $ do
          setTitleI MsgSpecialty
          $(widgetFile "data/specialties/create")


getSpecialtiesR :: Handler Html
getSpecialtiesR = do
    user <- maybeAuth
    specialties <- runDB $ select $ do
        x <- from $ table @Specialty
        orderBy [asc (x ^. SpecialtyName)]
        return x
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSpecialties
        idFabAdd <- newIdent
        addScript (StaticR js_specialties_specialties_min_js)
        $(widgetFile "data/specialties/specialties")
