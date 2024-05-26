{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExplicitForAll #-}

module Widgets
  ( widgetBanner
  , widgetSnackbar
  , widgetMenu
  , widgetUser
  ) where

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, just, val, exists
    , (^.), (==.)
    )
import Database.Persist (Entity (Entity))

import Foundation ()
import Foundation.Data
    ( Widget
    , Route
      ( HomeR, DataR, DocsR, RecordsR, MyDoctorsR, MyPatientsR, AuthR
      , AccountR, AccountPhotoR
      )
    , DataR (UsersR, SubscriptionsR, TokensR, StaffR, SpecialtiesR, UnitsR, MedSignsR)
    , AppMessage
      ( MsgWelcome, MsgTokens, MsgMainMenu, MsgData, MsgDoctors, MsgUsers
      , MsgDocumentation, MsgSourceCode, MsgResources, MsgSpecialties
      , MsgMeasurementUnits, MsgMedicalSigns, MsgRecords, MsgPatients
      , MsgSignIn, MsgSignOut, MsgUserAccount, MsgPhoto, MsgSubscriptions
      )
    )
    
import Model
    ( Specialties (Specialties), Doctor, User, Patient
    , AvatarColor (AvatarColorLight)
    , EntityField(DoctorUser, PatientUser, UserId)
    , statusError, statusSuccess
    )

import Settings (widgetFile)

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core (MonadHandler(liftHandler), Html, WidgetFor, Yesod)
import Yesod.Core.Handler (getCurrentRoute)
import Yesod.Persist (YesodPersist(runDB))
import Data.Text (Text)


widgetBanner :: Yesod  m => [(Text, Html)] -> WidgetFor m ()
widgetBanner msgs = do
    $(widgetFile "widgets/banner")


widgetSnackbar :: Yesod m => [(Text, Html)] -> WidgetFor m ()
widgetSnackbar msgs = do
    $(widgetFile "widgets/snackbar")


widgetMenu :: Widget
widgetMenu = do
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
    
    $(widgetFile "widgets/menu")


widgetUser :: Widget
widgetUser = do
    user <- maybeAuth
    $(widgetFile "widgets/user")
