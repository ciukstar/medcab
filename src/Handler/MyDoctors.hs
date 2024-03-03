{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.MyDoctors
  ( getMyDoctorsR
  , getMyDoctorPhotoR
  , getMyDoctorR
  , getMyDoctorSpecialtiesR
  ) where

import ChatRoom.Data ( Route(DoctorChatRoomR) )
import VideoRoom.Data ( Route(DoctorVideoRoomR) )

import Control.Monad (join)

import Data.Bifunctor (Bifunctor(second, bimap))
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    ( select, from, table, where_, leftJoin, on, just, orderBy, desc, selectOne
    , (^.), (?.), (==.), (:&)((:&))
    , SqlExpr, Value (unValue), val, innerJoin, countRows, subSelect
    , subSelectCount
    )
import Database.Persist (Entity (Entity))

import Foundation
    ( Handler
    , Route
      ( AuthR, AccountR, AccountPhotoR, MyDoctorPhotoR, StaticR, MyDoctorR, ChatR
      , MyDoctorsR, MyDoctorSpecialtiesR, VideoR
      )
    , AppMessage
      ( MsgDoctors, MsgUserAccount, MsgSignOut, MsgSignIn, MsgPhoto, MsgTabs
      , MsgNoDoctorsYet, MsgDoctor, MsgSpecializations, MsgMobile, MsgFullName
      , MsgEmailAddress, MsgDetails, MsgBack, MsgBookAppointment, MsgAudioCall
      , MsgVideoCall, MsgNoSpecialtiesYet, MsgSpecialty, MsgCertificateDate
      , MsgPhone, MsgChat
      )
    )

import Menu (menu)
import Model
    ( statusError, AvatarColor (AvatarColorLight)
    , ChatMessageStatus (ChatMessageStatusUnread)
    , Doctor(Doctor), DoctorPhoto (DoctorPhoto), DoctorId
    , Specialist (Specialist), Specialty (Specialty)
    , PatientId, Patient, UserId, Chat
    , EntityField
      ( DoctorPhotoDoctor, DoctorPhotoAttribution, DoctorId, SpecialistSpecialty
      , SpecialtyId, SpecialistDoctor, PatientDoctor, PatientUser, DoctorUser
      , ChatInterlocutor, ChatStatus, ChatUser
      )
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


getMyDoctorSpecialtiesR :: PatientId -> UserId -> DoctorId -> Handler Html
getMyDoctorSpecialtiesR pid uid did = do

    attrib <- (unValue =<<) <$> runDB ( selectOne $ do
        x <- from $ table @DoctorPhoto
        where_ $ x ^. DoctorPhotoDoctor ==. val did
        return (x ^. DoctorPhotoAttribution) )

    specialties <- runDB $ select $ do
        x :& s <- from $ table @Specialist `innerJoin` table @Specialty
            `on` (\(x :& s) -> x ^. SpecialistSpecialty ==. s ^. SpecialtyId)
        where_ $ x ^. SpecialistDoctor ==. val did
        return (x,s)

    defaultLayout $ do
        setTitleI MsgDoctor
        idPanelSpecialties <- newIdent
        $(widgetFile "my/doctors/specialties")


getMyDoctorR :: PatientId -> UserId -> DoctorId -> Handler Html
getMyDoctorR pid uid did = do

    doctor <- (second (join . unValue) <$>) <$> runDB ( selectOne $ do
        x :& h <- from $ table @Doctor `leftJoin` table @DoctorPhoto
            `on` (\(x :& h) -> just (x ^. DoctorId) ==. h ?. DoctorPhotoDoctor)
        where_ $ x ^. DoctorId ==. val did
        return (x,h ?. DoctorPhotoAttribution) )

    unread <- maybe 0 unValue <$> runDB ( selectOne $ do
        x <- from $ table @Chat
        where_ $ x ^. ChatInterlocutor ==. val uid
        where_ $ just (just (x ^. ChatUser)) ==. subSelect
            ( do
                  y <- from $ table @Doctor
                  where_ $ y ^. DoctorId ==. val did
                  return $ y ^. DoctorUser
            )
        where_ $ x ^. ChatStatus ==. val ChatMessageStatusUnread
        return (countRows :: SqlExpr (Value Int)) )

    defaultLayout $ do
        setTitleI MsgDoctor
        idPanelDetails <- newIdent
        $(widgetFile "my/doctors/doctor")



getMyDoctorsR :: UserId -> Handler Html
getMyDoctorsR uid = do
    
    user <- maybeAuth

    patients <- (second (second (bimap (join . unValue) unValue)) <$>) <$> runDB ( select $ do
        x :& d :& h <- from $ table @Patient `innerJoin` table @Doctor
            `on` (\(x :& d) -> x ^. PatientDoctor ==. d ^. DoctorId)
            `leftJoin` table @DoctorPhoto
            `on` (\(_ :& d :& h) -> just (d ^. DoctorId) ==. h ?. DoctorPhotoDoctor)
        where_ $ x ^. PatientUser ==. val uid
        
        let unread :: SqlExpr (Value Int)
            unread = subSelectCount $ do
              c <- from $ table @Chat
              where_ $ c ^. ChatInterlocutor ==. val uid
              where_ $ just (just (c ^. ChatUser)) ==. subSelect
                  ( do
                        d' <- from $ table @Doctor
                        where_ $ d' ^. DoctorId ==. d ^. DoctorId
                        return $ d' ^. DoctorUser
                  )
              where_ $ c ^. ChatStatus ==. val ChatMessageStatusUnread
        
        orderBy [desc (d ^. DoctorId)]
        return (x, (d, (h ?. DoctorPhotoAttribution,unread))) )

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

