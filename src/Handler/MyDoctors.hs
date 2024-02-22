{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.MyDoctors
  ( getMyDoctorsR
  , getMyDoctorPhotoR
  , getMyDoctorR
  , getMyDoctorSpecialtiesR
  ) where

import Control.Monad (join)

import Data.Bifunctor (Bifunctor(second))
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    (select, from, table, where_, leftJoin, on, just, orderBy, desc, selectOne
    , (^.), (?.), (==.), (:&)((:&))
    , Value (unValue), val, innerJoin, exists
    )
import Database.Persist (Entity (Entity))

import Foundation
    ( Handler
    , Route
      ( AuthR, AccountR, AccountPhotoR, MyDoctorPhotoR, StaticR, MyDoctorR, MyDoctorsR
      , MyDoctorSpecialtiesR, DoctorChatR
      )
    , AppMessage
      ( MsgDoctors, MsgUserAccount, MsgSignOut, MsgSignIn, MsgPhoto, MsgTabs
      , MsgNoDoctorsYet, MsgDoctor, MsgSpecializations, MsgMobile, MsgFullName
      , MsgEmailAddress, MsgDetails, MsgBack, MsgBookAppointment, MsgAudioCall
      , MsgVideoCall, MsgNoSpecialtiesYet, MsgSpecialty, MsgCertificateDate
      , MsgPhone, MsgChat
      )
    )

import ChatRoom.Data ( Route(ChatRoomR) )

import Menu (menu)
import Model
    ( statusError, AvatarColor (AvatarColorLight)
    , EntityField
      ( DoctorPhotoDoctor, DoctorPhotoAttribution, DoctorId, SpecialistSpecialty
      , SpecialtyId, SpecialistDoctor, PatientDoctor, PatientId, PatientUser
      )
    , Doctor(Doctor), DoctorPhoto (DoctorPhoto), DoctorId
    , Specialist (Specialist), Specialty (Specialty)
    , PatientId, Patient (Patient), UserId
    )

import Settings (widgetFile)
import Settings.StaticFiles (img_person_FILL0_wght400_GRAD0_opsz24_svg)

import Text.Hamlet (Html)

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core
    ( Yesod(defaultLayout), ToContent (toContent), redirect, newIdent )
import Yesod.Core.Content (TypedContent (TypedContent))
import Yesod.Core.Handler (getMessages, setUltDestCurrent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Persist.Core (YesodPersist(runDB))


getMyDoctorSpecialtiesR :: UserId -> DoctorId -> Handler Html
getMyDoctorSpecialtiesR uid did = do

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
        $(widgetFile "my/doctors/specialties")


getMyDoctorR :: UserId -> DoctorId -> Handler Html
getMyDoctorR uid did = do

    doctor <- (second (join . unValue) <$>) <$> runDB ( selectOne $ do
        x :& h <- from $ table @Doctor
            `leftJoin` table @DoctorPhoto `on` (\(x :& h) -> just (x ^. DoctorId) ==. h ?. DoctorPhotoDoctor)
        where_ $ x ^. DoctorId ==. val did
        return (x,h ?. DoctorPhotoAttribution) )

    defaultLayout $ do
        setTitleI MsgDoctor
        idPanelDetails <- newIdent
        $(widgetFile "my/doctors/doctor")



getMyDoctorsR :: UserId -> Handler Html
getMyDoctorsR uid = do
    
    user <- maybeAuth

    doctors <- (second (join . unValue) <$>) <$> runDB ( select $ do
        x :& h <- from $ table @Doctor
            `leftJoin` table @DoctorPhoto `on` (\(x :& h) -> just (x ^. DoctorId) ==. h ?. DoctorPhotoDoctor)
        where_ $ exists $ do
            y <- from $ table @Patient
            where_ $ y ^. PatientDoctor ==. x ^. DoctorId
            where_ $ y ^. PatientUser ==. val uid
        orderBy [desc (x ^. DoctorId)]
        return (x, h ?. DoctorPhotoAttribution) )

    msgs <- getMessages
    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgDoctors
        $(widgetFile "my/doctors/doctors")


getMyDoctorPhotoR :: UserId -> DoctorId -> Handler TypedContent
getMyDoctorPhotoR uid did = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @DoctorPhoto
        where_ $ x ^. DoctorPhotoDoctor ==. val did
        return x
    case photo of
      Just (Entity _ (DoctorPhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_person_FILL0_wght400_GRAD0_opsz24_svg

