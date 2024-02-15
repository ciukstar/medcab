{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Specialties
  ( getSpecialtiesR
  , postSpecialtiesR
  , getSpecialtyCreateR
  , getSpecialtyR
  , getSpecialtyEditR
  , postSpecialtyR
  , postSpecialtyDeleR
  , getSpecialtyDoctorsR
  , getSpecialtyDoctorR
  , postSpecialtyDoctorDeleR
  , getSpecialtyDoctorEditR
  , getSpecialtyDoctorCreateR
  , postSpecialtyDoctorsR
  , postSpecialtyDoctorR
  ) where

import Control.Monad (when, unless, join)
import qualified Data.List.Safe as LS (last)
import Data.Bifunctor (Bifunctor(bimap, second))
import Data.Text (Text)

import Database.Esqueleto.Experimental
    ( Entity (entityVal), select, from, table, orderBy, asc, val, where_
    , (^.), (?.), (==.), (:&)((:&))
    , selectOne, isNothing_, just, innerJoin, on, Value (unValue)
    , leftJoin
    )
import Database.Persist
    ( Entity (Entity), PersistStoreWrite (replace, delete, insert_, replace)
    )

import Material3 (md3textField, md3textareaField, md3mreq, md3mopt, md3dayField)
import Menu (menu)
import Model
    ( Specialty
      ( Specialty, specialtyName, specialtyCode, specialtyDescr, specialtyGroup )
    , EntityField
      ( SpecialtyName, SpecialtyId, SpecialtyGroup, SpecialistDoctor
      , SpecialistSpecialty, DoctorId, DoctorPhotoDoctor, DoctorPhotoAttribution
      , SpecialistTitle, DoctorName, SpecialistId
      )
    , AvatarColor (AvatarColorLight), statusSuccess, SpecialtyId, statusError
    , Specialties (Specialties), Doctor (Doctor)
    , Specialist (Specialist, specialistDoctor, specialistTitle, specialistCertDate)
    , DoctorId, DoctorPhoto, SpecialistId
    )

import Foundation
    ( Handler, Widget, Form
    , Route (DataR, AuthR, AccountR, AccountPhotoR)
    , DataR
      ( SpecialtiesR, SpecialtyCreateR, SpecialtyR, SpecialtyEditR
      , SpecialtyDeleR, SpecialtyDoctorsR, StaffPhotoR, SpecialtyDoctorR
      , SpecialtyDoctorDeleR, SpecialtyDoctorEditR, SpecialtyDoctorCreateR
      )
    , AppMessage
      ( MsgSpecialties, MsgUserAccount, MsgNoSpecialtiesYet, MsgSignIn
      , MsgSignOut, MsgPhoto, MsgEdit, MsgSpecialty, MsgDescription
      , MsgCode, MsgName, MsgSave, MsgCancel, MsgRecordCreated, MsgTabs
      , MsgBack, MsgDele, MsgRecordEdited, MsgInvalidFormData, MsgRecordDeleted
      , MsgDeleteAreYouSure, MsgConfirmPlease, MsgSubspecialties, MsgDetails
      , MsgNoSubspecialtiesYet, MsgAlreadyExists, MsgDoctors, MsgNoDoctorsYet
      , MsgSinceDate, MsgDoctor, MsgFullName, MsgMobile, MsgEmailAddress
      , MsgSpecialtyTitle, MsgCertificateDate
      )
    )
import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Auth (Route (LoginR, LogoutR), maybeAuth)
import Yesod.Core
    ( Yesod (defaultLayout), newIdent, SomeMessage (SomeMessage)
    , getMessageRender, addMessageI, redirect, getMessages, whamlet
    , setUltDestCurrent, MonadHandler (liftHandler), handlerToWidget
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Persist (YesodPersist (runDB))
import Yesod.Form.Fields
    ( optionsPairs, selectField, OptionList (olOptions)
    , Option (optionInternalValue, optionExternalValue, optionDisplay)
    )
import Yesod.Form.Functions (generateFormPost, runFormPost, checkM)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput), Field (fieldView)
    )


postSpecialtyDoctorDeleR :: SpecialistId -> SpecialtyId -> DoctorId -> Specialties -> Handler Html
postSpecialtyDoctorDeleR xid sid did ps = do
    ((fr,_),_) <- runFormPost formSpecialtyDoctorDele
    case fr of
      FormSuccess () -> do
          runDB $ delete xid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ SpecialtyDoctorsR sid ps
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ SpecialtyDoctorR xid sid did ps


postSpecialtyDoctorR :: SpecialistId -> SpecialtyId -> DoctorId -> Specialties -> Handler Html
postSpecialtyDoctorR xid sid did ps = do
    specialist <- runDB $ selectOne $ do
        x <- from $ table @Specialist
        where_ $ x ^. SpecialistId ==. val xid
        return x
    ((fr,fw),et) <- runFormPost $ formSpecialist sid specialist
    case fr of
      FormSuccess r -> do
          runDB $ replace xid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ SpecialtyDoctorR xid sid did ps
      _otherwise -> defaultLayout $ do
          setTitleI MsgDoctor
          $(widgetFile "data/specialties/doctors/edit")


getSpecialtyDoctorEditR :: SpecialistId -> SpecialtyId -> DoctorId -> Specialties -> Handler Html
getSpecialtyDoctorEditR xid sid did ps = do
    specialist <- runDB $ selectOne $ do
        x <- from $ table @Specialist
        where_ $ x ^. SpecialistId ==. val xid
        return x
    (fw,et) <- generateFormPost $ formSpecialist sid specialist
    defaultLayout $ do
        setTitleI MsgDoctor
        $(widgetFile "data/specialties/doctors/edit")


postSpecialtyDoctorsR :: SpecialtyId -> Specialties -> Handler Html
postSpecialtyDoctorsR sid ps = do
    ((fr,fw),et) <- runFormPost $ formSpecialist sid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordCreated
          redirect $ DataR $ SpecialtyDoctorsR sid ps
      _otherwise -> defaultLayout $ do
          setTitleI MsgDoctor
          $(widgetFile "data/specialties/doctors/create")          


getSpecialtyDoctorCreateR :: SpecialtyId -> Specialties -> Handler Html
getSpecialtyDoctorCreateR sid ps = do
    (fw,et) <- generateFormPost $ formSpecialist sid Nothing
    defaultLayout $ do
        setTitleI MsgDoctor
        $(widgetFile "data/specialties/doctors/create")


formSpecialist :: SpecialtyId -> Maybe (Entity Specialist) -> Form Specialist
formSpecialist sid specialist extra = do
    rndr <- getMessageRender

    docs <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Doctor
        orderBy [asc (x ^. DoctorName)]
        return (x ^. DoctorName, x ^. DoctorId) )

    (docR,docV) <- md3mreq (md3selectImgField (optionsPairs docs)) FieldSettings
        { fsLabel = SomeMessage MsgDoctor
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgDoctor)]
        } (specialistDoctor . entityVal <$> specialist)

    let doc = case docR of
          FormSuccess r -> Just r
          _oterwise -> Nothing

    (titleR,titleV) <- md3mreq (uniqueTitleField doc) FieldSettings
        { fsLabel = SomeMessage MsgSpecialtyTitle
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgSpecialtyTitle)]
        } (specialistTitle . entityVal <$> specialist)

    (certR,certV) <- md3mreq md3dayField FieldSettings
        { fsLabel = SomeMessage MsgCertificateDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgCertificateDate)]
        } (specialistCertDate . entityVal <$> specialist)

    let r = Specialist <$> docR <*> pure sid <*> titleR <*> certR
    let w = $(widgetFile "data/specialties/doctors/form")
    return (r,w)
    
  where
      
      md3selectImgField options = (selectField options)
          { fieldView = \theId name attrs x req -> do
                opts <- olOptions <$> handlerToWidget options
                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y
                [whamlet|
                  <md-filled-select ##{theId} *{attrs} :req:required name=#{name}>
                    $forall opt<- opts
                      <md-select-option value=#{optionExternalValue opt} :sel x opt:selected>
                        <img slot=start src=@{DataR $ StaffPhotoR (optionInternalValue opt)}
                          width=56 height=56 alt=_{MsgPhoto} loading=lazy style="clip-path:circle(50%)">
                        <div slot=headline>#{optionDisplay opt}
                    $if elem "error" (fst <$> attrs)
                      <md-icon slot=trailing-icon>error
                        |]
            }
                            
      uniqueTitleField :: Maybe DoctorId -> Field Handler Text
      uniqueTitleField mdid = checkM (uniqueTitle mdid) md3textField

      uniqueTitle :: Maybe DoctorId -> Text -> Handler (Either AppMessage Text)
      uniqueTitle mdid title = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Specialist
              where_ $ x ^. SpecialistTitle ==. val title
              where_ $ x ^. SpecialistSpecialty ==. val sid
              case mdid of
                Just did -> where_ $ x ^. SpecialistDoctor ==. val did
                Nothing -> return ()
              return x
          return $ case (mx,specialist) of
            (Nothing,_) -> Right title
            (Just (Entity xid' _), Just (Entity xid _)) | xid' == xid -> Right title
                                                        | otherwise -> Left MsgAlreadyExists
            _otherwise -> Left MsgAlreadyExists


getSpecialtyDoctorR :: SpecialistId -> SpecialtyId -> DoctorId -> Specialties -> Handler Html
getSpecialtyDoctorR xid sid did ps = do
    doctor <- runDB $ selectOne $ do
        x :& s <- from $ table @Doctor
            `innerJoin` table @Specialist `on` (\(x :& s) -> x ^. DoctorId ==. s ^. SpecialistDoctor)
        where_ $ s ^. SpecialistId ==. val xid
        return (x,s)

    attrib <- (unValue =<<) <$> runDB ( selectOne $ do
        x <- from $ table @DoctorPhoto
        where_ $ x ^. DoctorPhotoDoctor ==. val did
        return (x ^. DoctorPhotoAttribution) )
        
    (fw,et) <- generateFormPost formSpecialtyDoctorDele
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgDoctor
        $(widgetFile "data/specialties/doctors/doctor")


formSpecialtyDoctorDele :: Form ()
formSpecialtyDoctorDele extra = return (FormSuccess (), [whamlet|#{extra}|])


getSpecialtyDoctorsR :: SpecialtyId -> Specialties -> Handler Html
getSpecialtyDoctorsR sid ps@(Specialties sids) = do

    doctors <- (second (join . unValue) <$>) <$> runDB ( select $ do
        x :& s :& h <- from $ table @Doctor
            `innerJoin` table @Specialist `on` (\(x :& s) -> x ^. DoctorId ==. s ^. SpecialistDoctor)
            `leftJoin` table @DoctorPhoto `on` (\(x :& _ :& h) -> just (x ^. DoctorId) ==. h ?. DoctorPhotoDoctor)
        where_ $ s ^. SpecialistSpecialty ==. val sid
        return ((x,s),h ?. DoctorPhotoAttribution) )

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgDoctors
        idTabs <- newIdent
        idPanelDoctors <- newIdent
        idFabAdd <- newIdent
        $(widgetFile "data/specialties/doctors/doctors")


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
          idTabs <- newIdent
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
getSpecialtyEditR sid ps = do

    specialty <- runDB $ selectOne $ do
        x <- from $ table @Specialty
        where_ $ x ^. SpecialtyId ==. val sid
        return x

    (fw,et) <- generateFormPost $ formSpecialty Nothing specialty
    defaultLayout $ do
        setTitleI MsgSpecialty
        $(widgetFile "data/specialties/edit")


getSpecialtyCreateR :: Specialties -> Handler Html
getSpecialtyCreateR ps = do
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
        idTabs <- newIdent
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
            idTabs <- newIdent
            idPanelSubspecialties <- newIdent
            $(widgetFile "data/specialties/subspecialties")
