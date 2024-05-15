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
  , deleteMyDoctorNotificationsR
  ) where

import ChatRoom.Data ( Route(DoctorChatRoomR) )

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Aeson as A
    ( Value (Bool), Result (Success, Error), object, (.=) )
import Data.Bifunctor (Bifunctor(second, bimap))
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    ( select, from, table, where_, leftJoin, on, just, orderBy, desc, selectOne
    , (^.), (?.), (==.), (:&)((:&))
    , SqlExpr, Value (unValue), val, innerJoin, countRows, subSelect, delete
    , subSelectCount, subSelectMaybe, Entity (entityVal)
    )
import Database.Persist (Entity (Entity), upsertBy, (=.))
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler, Form
    , Route
      ( MyDoctorPhotoR, StaticR, ChatR, VideoR
      , MyDoctorsR, MyDoctorSpecialtiesR, MyDoctorNotificationsR, MyDoctorR
      )
    , AppMessage
      ( MsgDoctors, MsgPhoto, MsgTabs
      , MsgNoDoctorsYet, MsgDoctor, MsgSpecializations, MsgMobile, MsgFullName
      , MsgEmailAddress, MsgDetails, MsgBack, MsgBookAppointment, MsgAudioCall
      , MsgVideoCall, MsgNoSpecialtiesYet, MsgSpecialty, MsgCertificateDate
      , MsgPhone, MsgChat, MsgNotifications, MsgSubscribeToNotifications
      , MsgNotGeneratedVAPID, MsgNoRecipient
      )
    )

import Material3 (md3mreq, md3switchField)
import Model
    ( statusError, secretVolumeVapid, apiInfoVapid
    , ChatMessageStatus (ChatMessageStatusUnread)
    , PushMsgType (PushMsgTypeCall, PushMsgTypeCancel)
    , PatientId, Patient, Chat, Token
    , UserId, Doctor(Doctor, doctorUser), DoctorPhoto (DoctorPhoto), DoctorId, Store
    , Specialist (Specialist), Specialty (Specialty)
    , PushSubscription (PushSubscription), Unique (UniquePushSubscription)
    , StoreType (StoreTypeGoogleSecretManager, StoreTypeDatabase, StoreTypeSession)
    , EntityField
      ( DoctorPhotoDoctor, DoctorPhotoAttribution, DoctorId, SpecialistSpecialty
      , SpecialtyId, SpecialistDoctor, PatientDoctor, PatientUser, DoctorUser
      , ChatInterlocutor, ChatStatus, ChatUser, PushSubscriptionEndpoint
      , PushSubscriptionUser, PushSubscriptionP256dh, PushSubscriptionAuth
      , TokenApi, StoreToken, TokenId, StoreVal, TokenStore
      )
    )

import Network.HTTP.Types.Status (status400)

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_person_FILL0_wght400_GRAD0_opsz24_svg
    , img_call_FILL0_wght400_GRAD0_opsz24_svg
    )

import System.IO (readFile')

import Text.Hamlet (Html)
import Text.Julius (RawJS(rawJS))
import Text.Read (readMaybe)

import VideoRoom (widgetOutgoingCall, ChanId (ChanId))
import VideoRoom.Data ( Route(PushMessageR) )

import Web.WebPush
    ( readVAPIDKeys, vapidPublicKeyBytes, VAPIDKeys
    , VAPIDKeysMinDetails (VAPIDKeysMinDetails)
    )
    
import Widgets (widgetMenu, widgetUser, widgetBanner, widgetSnackbar)

import Yesod.Core
    ( Yesod(defaultLayout), ToContent (toContent), redirect, newIdent
    , SomeMessage (SomeMessage), parseCheckJsonBody, returnJson
    , sendStatusJSON, ToJSON (toJSON), lookupGetParam, invalidArgsI
    )
import Yesod.Core.Content (TypedContent (TypedContent))
import Yesod.Core.Handler (getMessages, setUltDestCurrent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Functions (generateFormPost)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput, fvLabel, fvId)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


deleteMyDoctorNotificationsR :: PatientId -> UserId -> DoctorId -> Handler ()
deleteMyDoctorNotificationsR _pid uid _did = do
    endpoint <- lookupGetParam "endpoint"
    case endpoint of
      Just x -> runDB $ delete $ do
          y <- from $ table @PushSubscription
          where_ $ y ^. PushSubscriptionUser ==. val uid
          where_ $ y ^. PushSubscriptionEndpoint ==. val x
      Nothing -> return ()


postMyDoctorNotificationsR :: PatientId -> UserId -> DoctorId -> Handler A.Value
postMyDoctorNotificationsR _pid _uid _did = do
    result <- parseCheckJsonBody
    case result of

      A.Success ps@(PushSubscription uid' psEndpoint psKeyP256dh psKeyAuth) -> do
          _ <- runDB $ upsertBy (UniquePushSubscription psEndpoint) ps [ PushSubscriptionUser =. uid'
                                                                       , PushSubscriptionP256dh =. psKeyP256dh
                                                                       , PushSubscriptionAuth =. psKeyAuth
                                                                       ]
          returnJson $ A.object [ "data" A..= A.object [ "success" A..= A.Bool True ] ]

      A.Error msg -> sendStatusJSON status400 (A.object [ "msg" A..= msg ])


getMyDoctorNotificationsR :: PatientId -> UserId -> DoctorId -> Handler Html
getMyDoctorNotificationsR pid uid did = do

    storeType <- (bimap unValue unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val apiInfoVapid
        return (x ^. TokenId, x ^. TokenStore) )

    let readTriple (s,x,y) = VAPIDKeysMinDetails s x y

    details <- case storeType of
      Just (_, StoreTypeGoogleSecretManager) -> do
          liftIO $ (readTriple <$>) . readMaybe <$> readFile' secretVolumeVapid

      Just (tid, StoreTypeDatabase) -> do
          ((readTriple <$>) . readMaybe . unpack . unValue =<<) <$> runDB ( selectOne $ do
              x <-from $ table @Store
              where_ $ x ^. StoreToken ==. val tid
              return $ x ^. StoreVal )

      Just (_,StoreTypeSession) -> return Nothing
      Nothing -> return Nothing

    case details of
      Just vapidKeysMinDetails -> do

          doctor <- (second (join . unValue) <$>) <$> runDB ( selectOne $ do
              x :& h <- from $ table @Doctor `leftJoin` table @DoctorPhoto
                  `on` (\(x :& h) -> just (x ^. DoctorId) ==. h ?. DoctorPhotoDoctor)
              where_ $ x ^. DoctorId ==. val did
              return (x,h ?. DoctorPhotoAttribution) )

          endpoint <- lookupGetParam "endpoint"

          permission <- (\case Just _ -> True; Nothing -> False) <$> runDB ( selectOne $ do
              x <- from $ table @PushSubscription
              where_ $ x ^. PushSubscriptionUser ==. val uid
              where_ $ just (x ^. PushSubscriptionEndpoint) ==. val endpoint
              return x )

          let vapidKeys = readVAPIDKeys vapidKeysMinDetails
          (fw,et) <- generateFormPost $ formNotifications vapidKeys pid uid did permission

          defaultLayout $ do
              setTitleI MsgDoctor
              idPanelNotifications <- newIdent
              $(widgetFile "my/doctors/notifications/notifications")

      Nothing -> invalidArgsI [MsgNotGeneratedVAPID]


formNotifications :: VAPIDKeys -> PatientId -> UserId -> DoctorId -> Bool -> Form Bool
formNotifications vapidKeys pid uid did notif extra = do

    let userId = pack $ show (fromSqlKey uid)

    (r,v) <- md3mreq md3switchField FieldSettings
        { fsLabel = SomeMessage MsgSubscribeToNotifications
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("icons","")]
        } ( pure notif )

    let applicationServerKey = vapidPublicKeyBytes vapidKeys
    return (r, $(widgetFile "my/doctors/notifications/form"))


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
        where_ $ just (x ^. ChatUser) ==. subSelectMaybe
            ( do
                  y <- from $ table @Doctor
                  where_ $ y ^. DoctorId ==. val did
                  return $ y ^. DoctorUser
            )
        where_ $ x ^. ChatStatus ==. val ChatMessageStatusUnread
        return (countRows :: SqlExpr (Value Int)) )

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

              let ChanId channel = ChanId (fromIntegral (fromSqlKey pid))

              $(widgetFile "my/doctors/doctor")
              widgetOutgoingCall idDialogOutgoingCall idButtonOutgoingCallCancel sid rid VideoR

      Nothing -> invalidArgsI [MsgNoRecipient]


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
