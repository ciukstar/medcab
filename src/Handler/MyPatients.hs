{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.MyPatients
  ( getMyPatientsR
  , getMyPatientR
  , getMyPatientNewR
  , postMyPatientsR
  , postMyPatientRemoveR
  ) where


import ChatRoom.Data ( Route(PatientChatRoomR) )

import Control.Monad (join, forM_)
import Control.Monad.IO.Class (liftIO)

import Data.Bifunctor (Bifunctor(second))
import Data.Time.Clock (getCurrentTime)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val, innerJoin, on, just
    , (^.), (?.), (==.), (:&) ((:&))
    , leftJoin, Value (unValue), not_, exists, Entity (entityKey, entityVal)
    )
import Database.Persist (Entity (Entity), PersistStoreWrite (insert_, delete))

import Foundation
    ( Handler, Form, App
    , Route
      ( AuthR, AccountPhotoR, MyPatientR, AccountR, MyPatientNewR, MyPatientsR
      , MyPatientRemoveR, ChatR
      )
    , AppMessage
      ( MsgPatients, MsgUserAccount, MsgSignIn, MsgSignOut, MsgNoPatientsYet
      , MsgPhoto, MsgEdit, MsgSinceDate, MsgCancel, MsgSave, MsgBack, MsgPatient
      , MsgRecordCreated, MsgFullName, MsgDele, MsgConfirmPlease, MsgChat
      , MsgEmailAddress, MsgRemoveAreYouSure, MsgAudioCall, MsgInvalidFormData
      , MsgVideoCall, MsgRecordDeleted, MsgRemove
      )
    )

import Menu (menu)
import Model
    ( statusError, statusSuccess, AvatarColor (AvatarColorLight, AvatarColorDark)
    , User (User, userName), UserPhoto
    , DoctorId, Doctor, PatientId, Patient(Patient)
    , EntityField
      ( PatientUser, UserId, PatientDoctor, UserPhotoUser, PatientId
      , UserPhotoAttribution, UserSuperuser, DoctorId
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Shakespeare.I18N (RenderMessage)

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, getMessages, newIdent, addMessageI
    , redirect, whamlet, handlerToWidget
    )
import Yesod.Form
    ( FieldView (fvInput), FormResult (FormSuccess), runFormPost
    , Field (fieldView), OptionList (olOptions)
    , multiSelectField, optionsPairs
    , Option (optionInternalValue, optionExternalValue, optionDisplay)
    )
import Yesod.Form.Functions (generateFormPost, mreq)
import Yesod.Persist (YesodPersist(runDB))


postMyPatientRemoveR :: DoctorId -> PatientId -> Handler Html
postMyPatientRemoveR did pid = do
    ((fr,_),_) <- runFormPost formPatientRemove
    case fr of
      FormSuccess () -> do
          runDB $ delete pid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ MyPatientsR did
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ MyPatientR did pid


postMyPatientsR :: DoctorId -> Handler Html
postMyPatientsR did = do

    patients <- runDB $ select $ do
        x <- from $ table @User
        where_ $ not_ $ x ^. UserSuperuser
        where_ $ not_ $ exists $ do
            y <- from $ table @Patient
            where_ $ y ^. PatientUser ==. x ^. UserId
        return x

    ((fr,fw),et) <- runFormPost $ formPatients did patients
    case fr of
      FormSuccess r -> do
          forM_ r $ \x -> runDB $ insert_ x
          addMessageI statusSuccess MsgRecordCreated
          redirect $ MyPatientsR did
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgPatients
          $(widgetFile "my/patients/new")


getMyPatientNewR :: DoctorId -> Handler Html
getMyPatientNewR did = do

    patients <- runDB $ select $ do
        x <- from $ table @User
        where_ $ not_ $ x ^. UserSuperuser
        where_ $ not_ $ exists $ do
            y <- from $ table @Patient
            where_ $ y ^. PatientUser ==. x ^. UserId
        return x

    (fw,et) <- generateFormPost $ formPatients did patients

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPatients
        $(widgetFile "my/patients/new")


formPatients :: DoctorId -> [Entity User] -> Form [Patient]
formPatients did options extra = do

    now <- liftIO getCurrentTime

    (usersR,usersV) <- mreq (usersFieldList (option <$> options)) "" Nothing

    let r = (<*>) <$> ((<*>) . (Patient <$>) <$> ((entityKey <$>) <$> usersR) <*> pure (pure did)) <*> pure (pure now)
    let w = $(widgetFile "my/patients/form")
    return (r,w)
  where
      option e@(Entity _ (User email _ _ _ _ _ _ _)) = (email,e)

      usersFieldList :: RenderMessage App msg => [(msg, Entity User)] -> Field Handler [Entity User]
      usersFieldList = usersField . optionsPairs

      usersField :: Handler (OptionList (Entity User)) -> Field Handler [Entity User]
      usersField ioptlist = (multiSelectField ioptlist)
          { fieldView = \theId name attrs eval _idReq -> do
              opts <- olOptions <$> handlerToWidget ioptlist
              let optselected (Left _) _ = False
                  optselected (Right vals) opt = optionInternalValue opt `elem` vals
              [whamlet|
                <md-list ##{theId}>
                  $forall opt <- opts
                    <md-list-item type=button onclick="this.querySelector('md-checkbox').click()">
                      <img slot=start src=@{AccountPhotoR (entityKey $ optionInternalValue opt) AvatarColorDark}
                        width=56 height=56 loading=lazy style="clip-path:circle(50%)">
                        
                      <div slot=headline>
                        $maybe name <- userName $ entityVal $ optionInternalValue opt
                          #{name}
                      <div slot=supporting-text>
                        #{optionDisplay opt}

                      <md-checkbox touch-target=wrapper slot=end onclick="event.stopPropagation()"
                        name=#{name} value=#{optionExternalValue opt} *{attrs} :optselected eval opt:checked>
              |]
          }


getMyPatientR :: DoctorId -> PatientId -> Handler Html
getMyPatientR did pid = do
    patient <- (second (second (join . unValue)) <$>) <$> runDB ( selectOne $ do
        x :& u :& h <- from $ table @Patient
            `innerJoin` table @User `on` (\(x :& u) -> x ^. PatientUser ==. u ^. UserId)
            `leftJoin` table @UserPhoto `on` (\(_ :& u :& h) -> just (u ^. UserId) ==. h ?. UserPhotoUser)
        where_ $ x ^. PatientId ==. val pid
        return (x,(u,h ?. UserPhotoAttribution)) )

    (fw,et) <- generateFormPost formPatientRemove
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPatient
        $(widgetFile "my/patients/patient")


formPatientRemove :: Form ()
formPatientRemove extra = return (pure (), [whamlet|#{extra}|])


getMyPatientsR :: DoctorId -> Handler Html
getMyPatientsR did = do
    user <- maybeAuth

    doctor <- runDB $ selectOne $ do
        x <- from $ table @Doctor
        where_ $ x ^. DoctorId ==. val did
        return x

    patients <- (second (second (join . unValue)) <$>) <$> runDB ( select $ do
        x :& u :& h <- from $ table @Patient
            `innerJoin` table @User `on` (\(x :& u) -> x ^. PatientUser ==. u ^. UserId)
            `leftJoin` table @UserPhoto `on` (\(_ :& u :& h) -> just (u ^. UserId) ==. h ?. UserPhotoUser)
        where_ $ x ^. PatientDoctor ==. val did
        return (x,(u,h ?. UserPhotoAttribution)) )

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPatients
        idFabAdd <- newIdent
        $(widgetFile "my/patients/patients")
