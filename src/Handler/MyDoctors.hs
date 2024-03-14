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
  , postPushSubscriptionsR
  , deletePushSubscriptionR
  , getMyDoctorNotificationsR
  , postMyDoctorNotificationsR
  , postPushMessageR
  ) where

import ChatRoom.Data ( Route(DoctorChatRoomR) )
import VideoRoom.Data ( Route(DoctorVideoRoomR) )

import Control.Lens ((.~))
import Control.Monad (join, forM_)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Aeson as A
    ( Value (Bool), Result (Success, Error), object, (.=) )
import Data.Bifunctor (Bifunctor(second, bimap))
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    ( select, from, table, where_, leftJoin, on, just, orderBy, desc, selectOne
    , (^.), (?.), (==.), (:&)((:&))
    , SqlExpr, Value (unValue), val, innerJoin, countRows, subSelect, delete
    , subSelectCount, subSelectMaybe
    )
import Database.Persist (Entity (Entity), entityVal, upsertBy, (=.))
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler, Form, App (appHttpManager)
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
      , MsgPhone, MsgChat, MsgNotifications, MsgSubscribeToNotifications
      , MsgNoRecipient, MsgNotGeneratedVAPID
      )
    )

import Material3 (md3mreq, md3switchField)
import Menu (menu)
import Model
    ( statusError, secretVolumeVapid, apiInfoVapid, AvatarColor (AvatarColorLight)
    , ChatMessageStatus (ChatMessageStatusUnread), User (userName, userEmail)
    , Doctor(Doctor, doctorUser), DoctorPhoto (DoctorPhoto), DoctorId
    , Specialist (Specialist), Specialty (Specialty)
    , PatientId, Patient, UserId, Chat, Token, Store
    , PushSubscription (PushSubscription), Unique (UniquePushSubscription)
    , StoreType (StoreTypeGoogleSecretManager, StoreTypeDatabase, StoreTypeSession)
    , EntityField
      ( DoctorPhotoDoctor, DoctorPhotoAttribution, DoctorId, SpecialistSpecialty
      , SpecialtyId, SpecialistDoctor, PatientDoctor, PatientUser, DoctorUser
      , ChatInterlocutor, ChatStatus, ChatUser, PushSubscriptionEndpoint
      , PushSubscriptionUser, PushSubscriptionP256dh, PushSubscriptionAuth
      , UserId, TokenApi, StoreToken, TokenId, StoreVal, TokenStore
      )
    )

import Network.HTTP.Types.Status (status400)

import Settings (widgetFile)
import Settings.StaticFiles (img_person_FILL0_wght400_GRAD0_opsz24_svg)

import System.IO (readFile')

import Text.Hamlet (Html)
import Text.Julius (RawJS(rawJS))
import Text.Read (readMaybe)

import Web.WebPush
    ( mkPushNotification, pushMessage, readVAPIDKeys, sendPushNotification
    , vapidPublicKeyBytes, pushSenderEmail, pushExpireInSeconds
    , VAPIDKeys, VAPIDKeysMinDetails (VAPIDKeysMinDetails)
    )

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core
    ( Yesod(defaultLayout), ToContent (toContent), redirect, newIdent
    , SomeMessage (SomeMessage), getYesod, parseCheckJsonBody, returnJson
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


postPushMessageR :: UserId -> UserId -> Handler ()
postPushMessageR sid rid = do

    sender <- do
        s <- runDB $ selectOne $ do
            x <- from $ table @User
            where_ $ x ^. UserId ==. val sid
            return x
        return $ fromMaybe (maybe "" (userEmail . entityVal) s) (userName . entityVal =<< s)

    subscriptions <- runDB $ select $ do
        x <- from $ table @PushSubscription
        where_ $ x ^. PushSubscriptionUser ==. val rid
        return x

    manager <- appHttpManager <$> getYesod

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
          let vapidKeys = readVAPIDKeys vapidKeysMinDetails

          forM_ subscriptions $ \(Entity _ (PushSubscription _ x y z)) -> do
                let notification = mkPushNotification x y z
                        & pushMessage .~ ("Call from " <> sender :: Text)
                        & pushSenderEmail .~ ("ciukstar@gmail.com" :: Text)
                        & pushExpireInSeconds .~ 60 * 60

                result <- sendPushNotification vapidKeys manager notification

                case result of
                  Left ex -> do
                      liftIO $ print ex
                  Right () -> return ()
                  
      Nothing -> invalidArgsI [MsgNotGeneratedVAPID]


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
postMyDoctorNotificationsR _pid _uid _did = undefined


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

          permission <- (\case Just _ -> True; Nothing -> False) <$> runDB ( selectOne $ do
              x <- from $ table @PushSubscription
              where_ $ x ^. PushSubscriptionUser ==. val uid
              return x )

          let vapidKeys = readVAPIDKeys vapidKeysMinDetails
          (fw,et) <- generateFormPost $ formNotifications vapidKeys uid permission

          defaultLayout $ do
              setTitleI MsgDoctor
              idPanelNotifications <- newIdent
              $(widgetFile "my/doctors/notifications/notifications")

      Nothing -> invalidArgsI [MsgNotGeneratedVAPID]


formNotifications :: VAPIDKeys -> UserId -> Bool -> Form Bool
formNotifications vapidKeys uid notif extra = do

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
              $(widgetFile "my/doctors/doctor")
      Nothing -> invalidArgsI [MsgNoRecipient]



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
getMyDoctorPhotoR _ did = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @DoctorPhoto
        where_ $ x ^. DoctorPhotoDoctor ==. val did
        return x
    case photo of
      Just (Entity _ (DoctorPhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_person_FILL0_wght400_GRAD0_opsz24_svg
