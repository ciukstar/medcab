{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Doctors
  ( getDoctorsR
  , getDoctorPhotoR
  , getDoctorR
  , getDoctorSpecialtiesR
  ) where

import Control.Monad (join)

import Data.Bifunctor (Bifunctor(second))
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    (select, from, table, where_, leftJoin, on, just, orderBy, desc, selectOne
    , (^.), (?.), (==.), (:&)((:&))
    , Value (unValue), val, innerJoin
    )
import Database.Persist (Entity (Entity))

import Foundation
    ( Handler
    , Route
      ( DoctorPhotoR, StaticR, DoctorR, DoctorsR
      , DoctorSpecialtiesR
      )
    , AppMessage
      ( MsgDoctors, MsgPhoto, MsgTabs
      , MsgNoDoctorsYet, MsgDoctor, MsgSpecializations, MsgMobile, MsgFullName
      , MsgEmailAddress, MsgDetails, MsgBack, MsgBookAppointment, MsgAudioCall
      , MsgVideoCall, MsgNoSpecialtiesYet, MsgSpecialty, MsgCertificateDate
      , MsgPhone, MsgChat
      )
    )

import Model
    ( EntityField
      ( DoctorPhotoDoctor, DoctorPhotoAttribution, DoctorId, SpecialistSpecialty
      , SpecialtyId, SpecialistDoctor
      )
    , Doctor(Doctor), DoctorPhoto (DoctorPhoto), DoctorId
    , Specialist (Specialist), Specialty (Specialty)
    )

import Settings (widgetFile)
import Settings.StaticFiles (img_person_FILL0_wght400_GRAD0_opsz24_svg)

import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetUser, widgetBanner, widgetSnackbar)

import Yesod.Core
    ( Yesod(defaultLayout), ToContent (toContent), redirect, newIdent )
import Yesod.Core.Content (TypedContent (TypedContent))
import Yesod.Core.Handler (getMessages, setUltDestCurrent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Persist.Core (YesodPersist(runDB))


getDoctorSpecialtiesR :: DoctorId -> Handler Html
getDoctorSpecialtiesR did = do

    attrib <- (unValue =<<) <$> runDB ( selectOne $ do
        x <- from $ table @DoctorPhoto
        where_ $ x ^. DoctorPhotoDoctor ==. val did
        return (x ^. DoctorPhotoAttribution) )

    specialties <- runDB $ select $ do
        x :& s <- from $ table @Specialist
            `innerJoin` table @Specialty `on` (\(x :& s) -> x ^. SpecialistSpecialty ==. s ^. SpecialtyId)
        where_ $ x ^. SpecialistDoctor ==. val did
        return (x,s)

    defaultLayout $ do
        setTitleI MsgDoctor
        idPanelSpecialties <- newIdent
        $(widgetFile "doctors/specialties")


getDoctorR :: DoctorId -> Handler Html
getDoctorR did = do

    doctor <- (second (join . unValue) <$>) <$> runDB ( selectOne $ do
        x :& h <- from $ table @Doctor
            `leftJoin` table @DoctorPhoto `on` (\(x :& h) -> just (x ^. DoctorId) ==. h ?. DoctorPhotoDoctor)
        where_ $ x ^. DoctorId ==. val did
        return (x,h ?. DoctorPhotoAttribution) )

    defaultLayout $ do
        setTitleI MsgDoctor
        idPanelDetails <- newIdent
        $(widgetFile "doctors/doctor")


getDoctorsR :: Handler Html
getDoctorsR = do

    doctors <- (second (join . unValue) <$>) <$> runDB ( select $ do
        x :& h <- from $ table @Doctor
            `leftJoin` table @DoctorPhoto `on` (\(x :& h) -> just (x ^. DoctorId) ==. h ?. DoctorPhotoDoctor)
        orderBy [desc (x ^. DoctorId)]
        return (x, h ?. DoctorPhotoAttribution) )

    msgs <- getMessages
    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgDoctors
        $(widgetFile "doctors/doctors")


getDoctorPhotoR :: DoctorId -> Handler TypedContent
getDoctorPhotoR did = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @DoctorPhoto
        where_ $ x ^. DoctorPhotoDoctor ==. val did
        return x
    case photo of
      Just (Entity _ (DoctorPhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_person_FILL0_wght400_GRAD0_opsz24_svg
