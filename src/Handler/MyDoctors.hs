{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Handler.MyDoctors
  ( getMyDoctorsR
  , getMyDoctorPhotoR
  , getMyDoctorR
  , getMyDoctorSpecialtiesR
  , getMyDoctorNotificationsR
  , postMyDoctorNotificationsR
  ) where

import ChatRoom.Data ( Route(DoctorChatRoomR) )

import Control.Monad (join)

import qualified Data.Aeson as A ( Value )
import Data.Bifunctor (Bifunctor(second, bimap))
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    ( select, from, table, where_, leftJoin, on, just, orderBy, desc, selectOne
    , (^.), (?.), (==.), (!=.), (:&)((:&))
    , SqlExpr, Value (unValue), val, innerJoin, countRows, subSelect, delete
    , subSelectCount, subSelectMaybe, Entity (entityVal)
    )
import Database.Persist (Entity (Entity), upsertBy, (=.))
import Database.Persist.Sql (fromSqlKey)

import Foundation (Form, getVAPIDKeys)
import Foundation.Data
    ( Handler
    , Route
      ( MyDoctorPhotoR, StaticR, ChatR, VideoR
      , MyDoctorsR, MyDoctorSpecialtiesR, MyDoctorNotificationsR, MyDoctorR, HomeR
      )
    , AppMessage
      ( MsgDoctors, MsgPhoto, MsgTabs
      , MsgNoDoctorsYet, MsgDoctor, MsgSpecializations, MsgMobile, MsgFullName
      , MsgEmailAddress, MsgDetails, MsgBack, MsgBookAppointment, MsgAudioCall
      , MsgVideoCall, MsgNoSpecialtiesYet, MsgSpecialty, MsgCertificateDate
      , MsgPhone, MsgChat, MsgNotifications, MsgSubscribeToNotifications
      , MsgNotGeneratedVAPID, MsgNoRecipient, MsgAllowUserToSendYouNotifications
      , MsgRecordDeleted, MsgInvalidFormData, MsgDoctorDoesNotHaveUserAccount
      , MsgOutgoingCall, MsgNoDataFound
      )
    )

import Material3 (md3mreq, md3switchField)

import Model
    ( paramEndpoint, User (User), statusSuccess, statusError
    , ChatMessageStatus (ChatMessageStatusUnread)
    , PushMsgType (PushMsgTypeCall, PushMsgTypeCancel, PushMsgTypeDecline, PushMsgTypeAccept)
    , PatientId, Patient, Chat
    , UserId, Doctor(Doctor, doctorUser), DoctorPhoto (DoctorPhoto), DoctorId
    , Specialist (Specialist), Specialty (Specialty)
    , PushSubscription
      ( PushSubscription, pushSubscriptionEndpoint, pushSubscriptionP256dh
      , pushSubscriptionAuth
      )
    , Unique (UniquePushSubscription)
    , EntityField
      ( DoctorPhotoDoctor, DoctorPhotoAttribution, DoctorId, SpecialistSpecialty
      , SpecialtyId, SpecialistDoctor, PatientDoctor, PatientUser, DoctorUser
      , ChatInterlocutor, ChatStatus, ChatUser, PushSubscriptionEndpoint
      , PushSubscriptionUser, PushSubscriptionP256dh, PushSubscriptionAuth
      , UserId
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_person_FILL0_wght400_GRAD0_opsz24_svg
    , img_call_FILL0_wght400_GRAD0_opsz24_svg
    )

import Text.Hamlet (Html)

import VideoRoom.Data (ChanId (ChanId), Route(PushMessageR, RoomR) )

import Web.WebPush (vapidPublicKeyBytes, VAPIDKeys)
    
import Widgets (widgetMenu, widgetUser, widgetBanner, widgetSnackbar)

import Yesod.Core
    ( Yesod(defaultLayout), ToContent (toContent), redirect, newIdent
    , SomeMessage (SomeMessage), ToJSON (toJSON), lookupGetParam, invalidArgsI
    , getCurrentRoute, MonadHandler (liftHandler), addMessageI
    )
import Yesod.Core.Content (TypedContent (TypedContent))
import Yesod.Core.Handler (getMessages, setUltDestCurrent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput, fvLabel, fvId), FormResult (FormSuccess)
    )
import Yesod.Persist.Core (YesodPersist(runDB))
import Data.Maybe (fromMaybe, isJust)
import Yesod.Form.Fields (hiddenField)


postMyDoctorNotificationsR :: PatientId -> UserId -> DoctorId -> Handler A.Value
postMyDoctorNotificationsR pid uid did = do
    vapidKeys <- getVAPIDKeys
    case vapidKeys of
      Just vapid -> do
          
          endpoint <- lookupGetParam paramEndpoint

          subscription <- runDB ( selectOne $ do
              x <- from $ table @PushSubscription
              where_ $ x ^. PushSubscriptionUser ==. val uid
              where_ $ just (x ^. PushSubscriptionEndpoint) ==. val endpoint
              return x )
              
          ((fr,_),_) <- runFormPost $ formNotifications vapid uid subscription

          case fr of
            FormSuccess (True, ps@(PushSubscription uid' endpoint' keyP256dh' keyAuth')) -> do
                _ <- runDB $ upsertBy (UniquePushSubscription endpoint') ps [ PushSubscriptionUser =. uid'
                                                                            , PushSubscriptionP256dh =. keyP256dh'
                                                                            , PushSubscriptionAuth =. keyAuth'
                                                                            ]
                redirect $ MyDoctorNotificationsR pid uid did
                
            FormSuccess (False, PushSubscription uid' endpoint' _ _) -> do
                _ <- runDB $ delete $ do
                    y <- from $ table @PushSubscription
                    where_ $ y ^. PushSubscriptionUser ==. val uid'
                    where_ $ y ^. PushSubscriptionEndpoint ==. val endpoint'
                addMessageI statusSuccess MsgRecordDeleted
                redirect $ MyDoctorNotificationsR pid uid did
                
            _otherwise -> do
                addMessageI statusError MsgInvalidFormData
                redirect $ MyDoctorNotificationsR pid uid did
                
      Nothing -> invalidArgsI [MsgNotGeneratedVAPID]


getMyDoctorNotificationsR :: PatientId -> UserId -> DoctorId -> Handler Html
getMyDoctorNotificationsR pid uid did = do

    endpoint <- lookupGetParam paramEndpoint
    
    doctor <- (unwrap <$>) <$> runDB ( selectOne $ do
        x :& h <- from $ table @Doctor `leftJoin` table @DoctorPhoto
            `on` (\(x :& h) -> just (x ^. DoctorId) ==. h ?. DoctorPhotoDoctor)

        let subscriptions :: SqlExpr (Value Int)
            subscriptions = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionUser ==. val uid
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint
                return y

        let loops :: SqlExpr (Value Int)
            loops = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ just (y ^. PushSubscriptionUser) ==. x ^. DoctorUser
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint
                return y

        let accessible :: SqlExpr (Value Int)
            accessible = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ just (y ^. PushSubscriptionUser) ==. x ^. DoctorUser
                where_ $ just (y ^. PushSubscriptionEndpoint) !=. val endpoint
                return y
                
        where_ $ x ^. DoctorId ==. val did
        return (x, (h ?. DoctorPhotoAttribution, (subscriptions, (loops, accessible)))) )

    vapidKeys <- getVAPIDKeys

    case (doctor, vapidKeys) of
      (Just (Entity _ (Doctor _ _ _ _ (Just duid)),_), Just vapid) -> do

          subscription <- runDB ( selectOne $ do
              x <- from $ table @PushSubscription
              where_ $ x ^. PushSubscriptionUser ==. val uid
              where_ $ just (x ^. PushSubscriptionEndpoint) ==. val endpoint
              return x )

          (fw,et) <- generateFormPost $ formNotifications vapid duid subscription

          defaultLayout $ do
              setTitleI MsgDoctor
              idPanelNotifications <- newIdent
              $(widgetFile "my/doctors/notifications/notifications")

      (Just (Entity _ (Doctor name _ _ _ Nothing),_), _) -> invalidArgsI [MsgDoctorDoesNotHaveUserAccount name]

      (Nothing, _) -> invalidArgsI [MsgNoDataFound]

      (_, Nothing) -> invalidArgsI [MsgNotGeneratedVAPID]
  where
      unwrap = second (bimap (join . unValue) (bimap ((> 0) . unValue) (bimap ((> 0) . unValue) ((> 0) . unValue))))


formNotifications :: VAPIDKeys -> UserId -> Maybe (Entity PushSubscription)
                  -> Form (Bool, PushSubscription)
formNotifications vapidKeys uid subscription extra = do

    let applicationServerKey = vapidPublicKeyBytes vapidKeys

    user <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x

    (subscribedR,subscribedV) <- md3mreq md3switchField FieldSettings
        { fsLabel = SomeMessage MsgSubscribeToNotifications
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("icons","")]
        } ( pure (isJust subscription) )

    (endpointR,endpointV) <- mreq hiddenField "" (pushSubscriptionEndpoint . entityVal <$> subscription)
    (p256dhR,p256dhV) <- mreq hiddenField "" (pushSubscriptionP256dh . entityVal <$> subscription)
    (authR,authV) <- mreq hiddenField "" (pushSubscriptionAuth . entityVal <$> subscription)    

    let r = (,) <$> subscribedR <*> (PushSubscription uid <$> endpointR <*> p256dhR <*> authR)
    idFormContentWrapper <- newIdent
    return (r, $(widgetFile "my/doctors/notifications/form"))


getMyDoctorSpecialtiesR :: PatientId -> UserId -> DoctorId -> Handler Html
getMyDoctorSpecialtiesR pid uid did = do

    endpoint <- lookupGetParam paramEndpoint
    
    doctor <- (unwrap <$>) <$> runDB ( selectOne $ do
        x :& h <- from $ table @Doctor
            `leftJoin` table @DoctorPhoto `on` (\(x :& h) -> just (x ^. DoctorId) ==. h ?. DoctorPhotoDoctor)

        let subscriptions :: SqlExpr (Value Int)
            subscriptions = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionUser ==. val uid
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint
                return y

        let loops :: SqlExpr (Value Int)
            loops = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ just (y ^. PushSubscriptionUser) ==. x ^. DoctorUser
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint
                return y

        let accessible :: SqlExpr (Value Int)
            accessible = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ just (y ^. PushSubscriptionUser) ==. x ^. DoctorUser
                where_ $ just (y ^. PushSubscriptionEndpoint) !=. val endpoint
                return y
                
        where_ $ x ^. DoctorId ==. val did
        return (x, (h ?. DoctorPhotoAttribution, (subscriptions, (loops, accessible)))) )

    specialties <- runDB $ select $ do
        x :& s <- from $ table @Specialist `innerJoin` table @Specialty
            `on` (\(x :& s) -> x ^. SpecialistSpecialty ==. s ^. SpecialtyId)
        where_ $ x ^. SpecialistDoctor ==. val did
        return (x,s)

    defaultLayout $ do
        setTitleI MsgDoctor
        idPanelSpecialties <- newIdent
        $(widgetFile "my/doctors/specialties")
  where
      unwrap = second (bimap (join . unValue) (bimap ((> 0) . unValue) (bimap ((> 0) . unValue) ((> 0) . unValue))))


getMyDoctorR :: PatientId -> UserId -> DoctorId -> Handler Html
getMyDoctorR pid uid did = do

    let polite = True

    endpoint <- lookupGetParam paramEndpoint
    
    doctor <- (unwrap <$>) <$> runDB ( selectOne $ do
        x :& h <- from $ table @Doctor
            `leftJoin` table @DoctorPhoto `on` (\(x :& h) -> just (x ^. DoctorId) ==. h ?. DoctorPhotoDoctor)

        let subscriptions :: SqlExpr (Value Int)
            subscriptions = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionUser ==. val uid
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint
                return y

        let loops :: SqlExpr (Value Int)
            loops = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ just (y ^. PushSubscriptionUser) ==. x ^. DoctorUser
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint
                return y

        let accessible :: SqlExpr (Value Int)
            accessible = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ just (y ^. PushSubscriptionUser) ==. x ^. DoctorUser
                where_ $ just (y ^. PushSubscriptionEndpoint) !=. val endpoint
                return y
                
        where_ $ x ^. DoctorId ==. val did
        return (x, (h ?. DoctorPhotoAttribution, (subscriptions, (loops, accessible)))) )

    unread <- maybe 0 unValue <$> runDB ( selectOne $ do
        x <- from $ table @Chat
        where_ $ x ^. ChatInterlocutor ==. val uid
        where_ $ just (x ^. ChatUser) ==. subSelectMaybe
            ( do
                  y <- from $ table @Doctor
                  where_ $ y ^. DoctorId ==. val did
                  return $ y ^. DoctorUser
            )
        where_ $ x ^. ChatStatus ==. val ChatMessageStatusUnread
        return (countRows :: SqlExpr (Value Int)) )

    backlink <- fromMaybe HomeR <$> getCurrentRoute

    let sid = uid
    case doctor >>= doctorUser . entityVal . fst of
      Just rid -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgDoctor

              idPanelDetails <- newIdent
              idButtonVideoCall <- newIdent
              idDialogOutgoingCall <- newIdent
              idButtonOutgoingCallCancel <- newIdent
              idDialogVideoSessionEnded <- newIdent
              idDialogCallDeclined <- newIdent

              let ChanId channel = ChanId (fromIntegral (fromSqlKey pid))
              
              $(widgetFile "my/doctors/doctor")
              $(widgetFile "video/outgoing")

      Nothing -> invalidArgsI [MsgNoRecipient]
  where
      unwrap = second (bimap (join . unValue) (bimap ((> 0) . unValue) (bimap ((> 0) . unValue) ((> 0) . unValue))))


getMyDoctorsR :: UserId -> Handler Html
getMyDoctorsR uid = do

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
getMyDoctorPhotoR _ did = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @DoctorPhoto
        where_ $ x ^. DoctorPhotoDoctor ==. val did
        return x
    case photo of
      Just (Entity _ (DoctorPhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_person_FILL0_wght400_GRAD0_opsz24_svg
