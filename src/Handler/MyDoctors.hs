{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.MyDoctors
  ( getMyDoctorsR
  , getMyDoctorPhotoR
  , getMyDoctorR
  , getMyDoctorSpecialtiesR
  , postPushSubscriptionsR
  , deletePushSubscriptionR
  , getMyDoctorNotificationsR
  , postMyDoctorNotificationsR
  , postPushMessageR
  ) where

import ChatRoom.Data ( Route(DoctorChatRoomR) )
import VideoRoom.Data ( Route(DoctorVideoRoomR) )

import Control.Lens ((.~))
import qualified Control.Lens as L
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Aeson as A (Value (Bool), Result (Success, Error), object, (.=))
import Data.Bifunctor (Bifunctor(second, bimap))
import Data.Function ((&))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    ( select, from, table, where_, leftJoin, on, just, orderBy, desc, selectOne
    , (^.), (?.), (==.), (:&)((:&))
    , SqlExpr, Value (unValue), val, innerJoin, countRows, subSelect, delete
    , subSelectCount
    )
import Database.Persist (Entity (Entity), entityVal, upsertBy, (=.))
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler
    , Route
      ( AuthR, AccountR, AccountPhotoR, MyDoctorPhotoR, StaticR, ChatR, VideoR
      , MyDoctorsR, MyDoctorSpecialtiesR, MyDoctorNotificationsR, MyDoctorR
      , PushMessageR, PushSubscriptionsR, PushSubscriptionR
      )
    , AppMessage
      ( MsgDoctors, MsgUserAccount, MsgSignOut, MsgSignIn, MsgPhoto, MsgTabs
      , MsgNoDoctorsYet, MsgDoctor, MsgSpecializations, MsgMobile, MsgFullName
      , MsgEmailAddress, MsgDetails, MsgBack, MsgBookAppointment, MsgAudioCall
      , MsgVideoCall, MsgNoSpecialtiesYet, MsgSpecialty, MsgCertificateDate
      , MsgPhone, MsgChat, MsgNotifications, MsgAcceptNotifyMeForChat
      , MsgAcceptNotifyMeForVideo, MsgAcceptNotifyMeForAudio
      ), Form, App (getVAPID, appHttpManager)
    )

import Material3 (md3mreq, md3switchField)
import Menu (menu)
import Model
    ( statusError, AvatarColor (AvatarColorLight)
    , ChatMessageStatus (ChatMessageStatusUnread)
    , Doctor(Doctor, doctorName), DoctorPhoto (DoctorPhoto), DoctorId
    , Specialist (Specialist), Specialty (Specialty)
    , PatientId, Patient, UserId, Chat
    , EntityField
      ( DoctorPhotoDoctor, DoctorPhotoAttribution, DoctorId, SpecialistSpecialty
      , SpecialtyId, SpecialistDoctor, PatientDoctor, PatientUser, DoctorUser
      , ChatInterlocutor, ChatStatus, ChatUser, PushSubscriptionEndpoint
      , PushSubscriptionUser, PushSubscriptionP256dh, PushSubscriptionAuth
      )
    , webPushEndpoint, webPushP256dh, webPushAuth
    , PushSubscription (PushSubscription), Unique (UniquePushSubscription)
    , PushSubscriptionId
    )
    
import Network.HTTP.Types.Status (status400)

import Settings (widgetFile)
import Settings.StaticFiles (img_person_FILL0_wght400_GRAD0_opsz24_svg)

import Text.Hamlet (Html)
import Text.Julius (RawJS(rawJS))

import Web.WebPush
    ( mkPushNotification, pushMessage, readVAPIDKeys, sendPushNotification
    , vapidPublicKeyBytes
    )

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core
    ( Yesod(defaultLayout), ToContent (toContent), redirect, newIdent
    , SomeMessage (SomeMessage), lookupSession, getYesod, addMessage, toHtml
    , parseCheckJsonBody, returnJson, sendStatusJSON, ToJSON (toJSON), lookupGetParam
    )
import Yesod.Core.Content (TypedContent (TypedContent))
import Yesod.Core.Handler (getMessages, setUltDestCurrent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Persist.Core (YesodPersist(runDB))
import Yesod.Form.Fields (textField)
import Yesod.Form.Functions (generateFormPost, runFormPost)
import Yesod.Form.Input (runInputPost, ireq)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput, fvLabel, fvId), FormResult (FormSuccess)
    )


postPushMessageR :: Handler ()
postPushMessageR = do
    vapidKeys <- readVAPIDKeys . getVAPID <$> getYesod
    manager <- appHttpManager <$> getYesod

    endpoint <- runInputPost $ ireq textField "endpoint"
    
    subscription <- runDB $ selectOne $ do
        x <- from $ table @PushSubscription
        where_ $ x ^. PushSubscriptionEndpoint ==. val endpoint
        return x

    case subscription of
      Just (Entity _ (PushSubscription _ x y z)) -> do
          let notification = mkPushNotification x y z & pushMessage .~ ("My first message" :: Text)
          result <- sendPushNotification vapidKeys manager notification
          case result of
            Left ex -> do
                liftIO $ print ex
            Right () -> return ()
      Nothing -> do
          let msg = "No subscription found" :: Text
          liftIO $ print msg


deletePushSubscriptionR :: Handler ()
deletePushSubscriptionR = do
    endpoint <- lookupGetParam "endpoint"
    case endpoint of
      Just x -> runDB $ delete $ do
          y <- from $ table @PushSubscription
          where_ $ y ^. PushSubscriptionEndpoint ==. val x
      Nothing -> return ()


postPushSubscriptionsR :: Handler A.Value
postPushSubscriptionsR = do
    result <- parseCheckJsonBody
    case result of

      A.Success ps@(PushSubscription uid psEndpoint psKeyP256dh psKeyAuth) -> do
          _ <- runDB $ upsertBy (UniquePushSubscription psEndpoint) ps [ PushSubscriptionUser =. uid
                                                                       , PushSubscriptionP256dh =. psKeyP256dh
                                                                       , PushSubscriptionAuth =. psKeyAuth
                                                                       ]
          returnJson $ A.object [ "data" A..= A.object [ "success" A..= A.Bool True ] ]

      A.Error msg -> sendStatusJSON status400 (A.object [ "msg" A..= msg ])


postMyDoctorNotificationsR :: PatientId -> UserId -> DoctorId -> Handler Html
postMyDoctorNotificationsR pid uid did = do

    doctor <- runDB $ selectOne $ do
        x <- from $ table @Doctor
        where_ $ x ^. DoctorId ==. val did
        return x
    
    ((fr,fw),et) <- runFormPost $ formNotifications uid (maybe "" (doctorName . entityVal) doctor) Nothing
    case fr of
      FormSuccess (_,allowPushNotifications,_) -> undefined
      _otherwise -> undefined


getMyDoctorNotificationsR :: PatientId -> UserId -> DoctorId -> Handler Html
getMyDoctorNotificationsR pid uid did = do

    doctor <- (second (join . unValue) <$>) <$> runDB ( selectOne $ do
        x :& h <- from $ table @Doctor `leftJoin` table @DoctorPhoto
            `on` (\(x :& h) -> just (x ^. DoctorId) ==. h ?. DoctorPhotoDoctor)
        where_ $ x ^. DoctorId ==. val did
        return (x,h ?. DoctorPhotoAttribution) )

    (fw,et) <- generateFormPost $ formNotifications uid (maybe "" (doctorName . entityVal . fst) doctor) Nothing

    
    vapidKeys <- readVAPIDKeys . getVAPID <$> getYesod

    defaultLayout $ do
        setTitleI MsgDoctor
        idPanelNotifications <- newIdent
        $(widgetFile "my/doctors/notifications/notifications")


formNotifications :: UserId -> Text -> Maybe (Bool, Bool, Bool) -> Form (Bool, Bool, Bool)
formNotifications uid name notifs extra = do

    let userId = pack $ show (fromSqlKey uid)
    
    vapidKeys <- readVAPIDKeys . getVAPID <$> getYesod

    (chatR,chatV) <- md3mreq md3switchField FieldSettings
        { fsLabel = SomeMessage (MsgAcceptNotifyMeForChat name)
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("icons","")]
        } ( notifs L.^? L._Just . L._1 )

    (videoR,videoV) <- md3mreq md3switchField FieldSettings
        { fsLabel = SomeMessage (MsgAcceptNotifyMeForVideo name)
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("icons","")]
        } ( notifs L.^? L._Just . L._2 )

    (audioR,audioV) <- md3mreq md3switchField FieldSettings
        { fsLabel = SomeMessage (MsgAcceptNotifyMeForAudio name)
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("icons","")]
        } ( notifs L.^? L._Just . L._3 )

    let r = (,,) <$> chatR <*> videoR <*> audioR
    let w = $(widgetFile "my/doctors/notifications/form")
    return (r,w)


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

    msgs <- getMessages
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
