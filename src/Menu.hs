{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Menu (menu) where

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, just, val, exists
    , (^.), (==.)
    )
import Database.Persist (Entity (Entity))

import Foundation
    ( Widget
    , Route (HomeR, DataR, DocsR, RecordsR, MyDoctorsR, MyPatientsR)
    , DataR (UsersR, TokensR, StaffR, SpecialtiesR, UnitsR, MedSignsR)
    , AppMessage
      ( MsgWelcome, MsgTokens, MsgMainMenu, MsgData, MsgDoctors, MsgUsers
      , MsgDocumentation, MsgSourceCode, MsgResources, MsgSpecialties
      , MsgMeasurementUnits, MsgMedicalSigns, MsgRecords, MsgPatients
      )
    )
    
import Model
    ( Specialties (Specialties), Doctor, User, Patient
    , EntityField(DoctorUser, PatientUser, UserId)
    )

import Settings (widgetFile)

import Yesod.Auth (maybeAuth)
import Yesod.Core (MonadHandler(liftHandler))
import Yesod.Core.Handler (getCurrentRoute)
import Yesod.Persist (YesodPersist(runDB))

menu :: Widget
menu = do
    curr <- getCurrentRoute

    user <- maybeAuth

    doctor <- liftHandler $ case user of
      Just (Entity uid _) -> runDB $ selectOne $ do
        x <- from $ table @Doctor
        where_ $ x ^. DoctorUser ==. just (val uid)
        return x
      Nothing -> return Nothing

    patient <- liftHandler $ case user of
      Just (Entity uid _) -> runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ exists $ do
            y <- from $ table @Patient
            where_ $ y ^. PatientUser ==. x ^. UserId
            where_ $ y ^. PatientUser ==. val uid
        return x
      Nothing -> return Nothing
    
    $(widgetFile "menu")
