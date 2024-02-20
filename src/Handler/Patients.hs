{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Patients
  ( getPatientsR
  , getPatientR
  , getPatientNewR
  ) where

import Control.Monad (join)

import Data.Bifunctor (Bifunctor(second))

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val, innerJoin, on, just
    , (^.), (?.), (==.), (:&) ((:&))
    , leftJoin, Value (unValue)
    )
import Database.Persist (Entity (Entity))

import Foundation
    ( Handler
    , Route (AuthR, AccountPhotoR, PatientR, AccountR, PatientNewR)
    , AppMessage
      ( MsgPatients, MsgUserAccount, MsgSignIn, MsgSignOut, MsgNoPatientsYet
      , MsgPhoto, MsgEdit, MsgSinceDate
      )
    )

import Menu (menu)
import Model
    ( statusError, AvatarColor (AvatarColorLight, AvatarColorDark)
    , User (User), UserPhoto (UserPhoto)
    , DoctorId, Doctor (Doctor)
    , EntityField
      ( PatientUser, UserId, PatientDoctor, DoctorId, DoctorUser, UserPhotoUser
      , UserPhotoAttribution
      )
    , PatientId, Patient(Patient)
    )
    
import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core (Yesod(defaultLayout), setTitleI, getMessages, newIdent)
import Yesod.Persist (YesodPersist(runDB))


getPatientNewR :: DoctorId -> Handler Html
getPatientNewR did = undefined


getPatientR :: PatientId -> Handler Html
getPatientR pid = undefined


getPatientsR :: Handler Html
getPatientsR = do
    user <- maybeAuth
    
    doctor <- case user of
      Just (Entity uid _) -> runDB $ selectOne $ do
        x <- from $ table @Doctor
        where_ $ x ^. DoctorUser ==. just (val uid)
        return x
      Nothing -> return Nothing
        
    patients <- (second (second (join . unValue)) <$>) <$> case doctor of
      Just (Entity did _) -> runDB $ select $ do
          x :& u :& h <- from $ table @Patient
              `innerJoin` table @User `on` (\(x :& u) -> x ^. PatientUser ==. u ^. UserId)
              `leftJoin` table @UserPhoto `on` (\(_ :& u :& h) -> just (u ^. UserId) ==. h ?. UserPhotoUser)
          where_ $ x ^. PatientDoctor ==. val did
          return (x,(u,h ?. UserPhotoAttribution))
      Nothing -> return []

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPatients
        idFabAdd <- newIdent
        $(widgetFile "patients/patients")
