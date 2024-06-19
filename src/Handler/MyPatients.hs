{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.MyPatients
  ( getMyPatientsR
  , getMyPatientR
  , postMyPatientR
  , getMyPatientNewR
  , postMyPatientsR
  , postMyPatientRemoveR
  , getMyPatientSubscriptionsR
  , postMyPatientSubscriptionsR
  , postMyPatientUnsubscribeR
  , getMyPatientEditR
  ) where


import ChatRoom.Data ( Route(PatientChatRoomR) )

import Control.Lens ((.~),(?~))
import Control.Monad (join, forM_, void)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (object, (.=))
import Data.Bifunctor (Bifunctor(second, bimap))
import Data.Function ((&))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text, pack)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (localTimeToUTC, utc, utcToLocalTime)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val, innerJoin, on, just
    , (^.), (?.), (==.), (!=.), (:&) ((:&))
    , SqlExpr, Value (unValue), leftJoin, not_, exists, countRows, subSelect
    , subSelectCount, delete
    )
import Database.Persist
    ( Entity (Entity, entityKey, entityVal)
    , PersistStoreWrite (insert_, replace)
    , PersistUniqueWrite (upsertBy), (=.)
    )
import qualified Database.Persist as P (delete)

import Foundation
    ( Handler, Form, App (appHttpManager, appSettings), getVAPIDKeys
    , Route
      ( AccountPhotoR, MyPatientR, MyPatientNewR, MyPatientsR, MyDoctorPhotoR
      , MyPatientRemoveR, ChatR, RtcR, MyPatientSubscriptionsR, StaticR, HomeR
      , MyPatientUnsubscribeR, MyPatientEditR
      )
    , AppMessage
      ( MsgPatients, MsgNoPatientsYet, MsgClose, MsgCallEnded, MsgCallDeclined
      , MsgPhoto, MsgEdit, MsgSinceDate, MsgCancel, MsgSave, MsgBack, MsgPatient
      , MsgRecordCreated, MsgFullName, MsgDele, MsgConfirmPlease, MsgChat
      , MsgEmailAddress, MsgRemoveAreYouSure, MsgAudioCall, MsgInvalidFormData
      , MsgVideoCall, MsgRecordDeleted, MsgRemove, MsgDetails, MsgTabs
      , MsgSubscription, MsgSubscribeToNotifications, MsgNotGeneratedVAPID
      , MsgNoRecipient, MsgNotSubscribedToNotificationsFromUser, MsgSendEmail
      , MsgYouAndUserSubscribedOnSameDevice, MsgAllowUserToSendYouNotifications
      , MsgUserUnavailable, MsgNoPublisherFound, MsgCalleeDeclinedTheCall
      , MsgUnsubscribe, MsgAppName, MsgUserIsNowAvailable, MsgUserIsNoLongerAvailable
      , MsgIncomingVideoCallFrom, MsgIncomingAudioCallFrom, MsgOutgoingVideoCall
      , MsgOutgoingAudioCall, MsgUserCanceledVideoCall, MsgUserCanceledAudioCall
      , MsgMobile, MsgPhone, MsgInvalidPatient, MsgMakeACall
      )
    )

import Material3
    ( md3switchField, md3mreq, md3mopt, md3datetimeLocalField, md3telField )

import Model
    ( statusError, statusSuccess, paramEndpoint
    , AvatarColor (AvatarColorDark)
    , ChatMessageStatus (ChatMessageStatusUnread)
    , PushMsgType
      ( PushMsgTypeVideoCall, PushMsgTypeAudioCall
      , PushMsgTypeAcceptVideoCall, PushMsgTypeAcceptAudioCall
      , PushMsgTypeDeclineVideoCall, PushMsgTypeDeclineAudioCall
      , PushMsgTypeCancel, PushMsgTypeRefresh
      )
    , UserId, User (User, userName), UserPhoto, DoctorId, Doctor (Doctor)
    , PatientId
    , Patient(Patient, patientUser, patientSince, patientMobile, patientPhone)
    , Chat, DoctorPhoto
    , PushSubscription
      ( PushSubscription, pushSubscriptionEndpoint, pushSubscriptionP256dh
      , pushSubscriptionAuth
      )
    , Unique (UniquePushSubscription)
    , EntityField
      ( PatientUser, UserId, PatientDoctor, UserPhotoUser, PatientId
      , UserPhotoAttribution, UserSuperuser, DoctorId, ChatInterlocutor
      , ChatStatus, ChatUser, PushSubscriptionSubscriber, PushSubscriptionP256dh
      , PushSubscriptionAuth, PushSubscriptionEndpoint, PushSubscriptionPublisher
      , DoctorPhotoDoctor, DoctorPhotoAttribution, UserInfoUser
      ), UserInfo (UserInfo)
    )

import Settings
    ( widgetFile, Superuser (Superuser, superuserUsername)
    , AppSettings (appSuperuser)
    )
import Settings.StaticFiles
    ( img_call_FILL0_wght400_GRAD0_opsz24_svg
    , img_notifications_off_24dp_FILL0_wght400_GRAD0_opsz24_svg
    , img_notifications_24dp_FILL0_wght400_GRAD0_opsz24_svg, ringtones_outgoing_call_mp3
    )

import Text.Hamlet (Html)
import Text.Shakespeare.I18N (RenderMessage, SomeMessage (SomeMessage))

import RtcRoom.Data (Route (PushMessageR, VideoR, AudioR))

import Web.WebPush
    ( PushUrgency (PushUrgencyHigh), PushTopic (PushTopic), VAPIDKeys
    , pushMessage, sendPushNotification, vapidPublicKeyBytes, pushSenderEmail
    , pushExpireInSeconds, pushTopic, mkPushNotification, pushUrgency
    )

import Widgets (widgetMenu, widgetUser, widgetBanner, widgetSnackbar)

import Yesod.Auth (maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), ToJSON (toJSON)
    , setTitleI, getMessages, newIdent, addMessageI, redirect, whamlet
    , handlerToWidget, invalidArgsI, lookupGetParam, getCurrentRoute
    , lookupGetParam, getMessageRender, getYesod, getUrlRender
    )
import Yesod.Form
    ( FieldView (fvInput, fvLabel, fvId), FormResult (FormSuccess, FormFailure), runFormPost
    , Field (fieldView), OptionList (olOptions)
    , multiSelectField, optionsPairs, hiddenField
    , Option (optionInternalValue, optionExternalValue, optionDisplay)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Form.Functions (generateFormPost, mreq)
import Yesod.Persist (YesodPersist(runDB))


postMyPatientUnsubscribeR :: UserId -> DoctorId -> PatientId -> Handler ()
postMyPatientUnsubscribeR uid did pid = do

    ((fr2,_),_) <- runFormPost formUnsubscribePatient

    patient <- runDB $ selectOne $ do
        x <- from $ table @Patient
        where_ $ x ^. PatientId ==. val pid
        return x

    case (fr2,patient) of
      (FormSuccess endpoint,Just (Entity _ (Patient publisher _ _ _ _))) -> do

          runDB $ delete $ do
              x <- from $ table @PushSubscription
              where_ $ x ^. PushSubscriptionSubscriber ==. val publisher
              where_ $ x ^. PushSubscriptionPublisher ==. val uid
              where_ $ x ^. PushSubscriptionEndpoint ==. val endpoint

          addMessageI statusSuccess MsgRecordDeleted
          redirect $ MyPatientSubscriptionsR uid did pid
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ MyPatientSubscriptionsR uid did pid


postMyPatientSubscriptionsR :: UserId -> DoctorId -> PatientId -> Handler Html
postMyPatientSubscriptionsR uid did pid = do

    vapidKeys <- getVAPIDKeys

    patient <- runDB $ selectOne $ do
        x :& u <- from $ table @Patient
            `innerJoin` table @User `on` (\(x :& u) -> x ^. PatientUser ==. u ^. UserId)
        where_ $ x ^. PatientId ==. val pid
        return u

    case (patient, vapidKeys) of
      (Just (Entity publisher (User email _ _ _ _ name _ _)), Just vapid) -> do

          ((fr,_),_) <- runFormPost $ formNotifications vapid uid publisher patient Nothing

          case fr of
            FormSuccess (True, ps@(PushSubscription uid' pid' endpoint' keyP256dh' keyAuth')) -> do
                void $ runDB $ upsertBy (UniquePushSubscription uid' pid') ps [ PushSubscriptionEndpoint =. endpoint'
                                                                              , PushSubscriptionP256dh =. keyP256dh'
                                                                              , PushSubscriptionAuth =. keyAuth'
                                                                              ]

                publishers <- runDB $ select $ do
                    x <- from $ table @PushSubscription
                    where_ $ x ^. PushSubscriptionSubscriber ==. val pid'
                    where_ $ x ^. PushSubscriptionPublisher ==. val uid'
                    return x

                msgr <- getMessageRender
                rndr <- getUrlRender
                manager <- appHttpManager <$> getYesod
                Superuser {..} <- appSuperuser . appSettings <$> getYesod

                forM_ publishers $ \(Entity _ (PushSubscription _ _ endpoint'' p256dh'' auth'')) -> do
                    let notification = mkPushNotification endpoint'' p256dh'' auth''
                            & pushMessage .~ object [ "messageType" .= PushMsgTypeRefresh
                                                    , "title" .= msgr MsgAppName
                                                    , "icon" .= rndr
                                                      (StaticR img_notifications_24dp_FILL0_wght400_GRAD0_opsz24_svg)
                                                    , "body" .= msgr (MsgUserIsNowAvailable (fromMaybe email name))
                                                    ]
                            & pushSenderEmail .~ superuserUsername
                            & pushExpireInSeconds .~ 60
                            & pushUrgency ?~ PushUrgencyHigh
                            & pushTopic .~ (pure . PushTopic . pack . show $ PushMsgTypeRefresh)

                    void $ sendPushNotification vapid manager notification

                redirect $ MyPatientSubscriptionsR uid did pid

            FormSuccess (False, PushSubscription uid' pid' endpoint' _ _) -> do

                void $ runDB $ delete $ do
                    y <- from $ table @PushSubscription
                    where_ $ y ^. PushSubscriptionSubscriber ==. val uid'
                    where_ $ y ^. PushSubscriptionPublisher ==. val pid'
                    where_ $ y ^. PushSubscriptionEndpoint ==. val endpoint'

                addMessageI statusSuccess MsgRecordDeleted

                publishers <- runDB $ select $ do
                    x <- from $ table @PushSubscription
                    where_ $ x ^. PushSubscriptionSubscriber ==. val pid'
                    where_ $ x ^. PushSubscriptionPublisher ==. val uid'
                    return x

                msgr <- getMessageRender
                rndr <- getUrlRender
                manager <- appHttpManager <$> getYesod
                Superuser {..} <- appSuperuser . appSettings <$> getYesod

                forM_ publishers $ \(Entity _ (PushSubscription _ _ endpoint'' p256dh'' auth'')) -> do
                    let notification = mkPushNotification endpoint'' p256dh'' auth''
                            & pushMessage .~ object [ "messageType" .= PushMsgTypeRefresh
                                                    , "title" .= msgr MsgAppName
                                                    , "icon" .= rndr
                                                      (StaticR img_notifications_off_24dp_FILL0_wght400_GRAD0_opsz24_svg)
                                                    , "body" .= msgr (MsgUserIsNoLongerAvailable (fromMaybe email name))
                                                    ]
                            & pushSenderEmail .~ superuserUsername
                            & pushExpireInSeconds .~ 60
                            & pushUrgency ?~ PushUrgencyHigh
                            & pushTopic .~ (pure . PushTopic . pack . show $ PushMsgTypeRefresh)

                    void $ sendPushNotification vapid manager notification

                redirect $ MyPatientSubscriptionsR uid did pid

            _otherwise -> do
                addMessageI statusError MsgInvalidFormData
                redirect $ MyPatientSubscriptionsR uid did pid

      (Nothing, _) -> invalidArgsI [MsgNoPublisherFound]

      (_, Nothing) -> invalidArgsI [MsgNotGeneratedVAPID]


getMyPatientSubscriptionsR :: UserId -> DoctorId -> PatientId -> Handler Html
getMyPatientSubscriptionsR uid did pid = do

    vapidKeys <- getVAPIDKeys

    endpoint <- lookupGetParam paramEndpoint

    patient <- (unwrap <$>) <$> runDB ( selectOne $ do
        x :& u :& h <- from $ table @Patient
            `innerJoin` table @User `on` (\(x :& u) -> x ^. PatientUser ==. u ^. UserId)
            `leftJoin` table @UserPhoto `on` (\(_ :& u :& h) -> just (u ^. UserId) ==. h ?. UserPhotoUser)

        let subscriptions :: SqlExpr (Value Int)
            subscriptions = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. val uid
                where_ $ y ^. PushSubscriptionPublisher ==. x ^. PatientUser
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint
                return y

        let loops :: SqlExpr (Value Int)
            loops = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. PatientUser
                where_ $ y ^. PushSubscriptionPublisher ==. val uid
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint
                return y

        let accessible :: SqlExpr (Value Int)
            accessible = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. PatientUser
                where_ $ y ^. PushSubscriptionPublisher ==. val uid
                where_ $ just (y ^. PushSubscriptionEndpoint) !=. val endpoint
                return y

        where_ $ x ^. PatientId ==. val pid
        return (x, (u, (h ?. UserPhotoAttribution, (subscriptions, (loops, accessible))))) )

    case (patient, vapidKeys) of
      (Just (_, (Entity publisher _, _)), Just vapid) -> do

          subscription <- runDB ( selectOne $ do
              x <- from $ table @PushSubscription
              where_ $ x ^. PushSubscriptionSubscriber ==. val uid
              where_ $ x ^. PushSubscriptionPublisher ==. val publisher
              where_ $ just (x ^. PushSubscriptionEndpoint) ==. val endpoint
              return x )

          (fw,et) <- generateFormPost $ formNotifications vapid uid publisher (fst . snd <$> patient) subscription
          (fw2,et2) <- generateFormPost formUnsubscribePatient
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPatient
              idPanelNotifications <- newIdent
              idFormSubscription <- newIdent
              $(widgetFile "my/patients/subscriptions/subscriptions")

      (Nothing, _) -> invalidArgsI [MsgNoPublisherFound]

      (_, Nothing) -> invalidArgsI [MsgNotGeneratedVAPID]

  where
      unwrap = second (second (bimap (join . unValue) (bimap ((> 0) . unValue) (bimap ((> 0) . unValue) ((> 0) . unValue)))))


formUnsubscribePatient :: Form Text
formUnsubscribePatient extra = do
    endpoint <- lookupGetParam paramEndpoint
    (endpointR,endpointV) <- mreq hiddenField "" endpoint
    return (endpointR, [whamlet|^{extra} ^{fvInput endpointV}|])


formNotifications :: VAPIDKeys -> UserId -> UserId -> Maybe (Entity User) -> Maybe (Entity PushSubscription)
                  -> Form (Bool, PushSubscription)
formNotifications vapidKeys uid pid patient subscription extra = do

    let applicationServerKey = vapidPublicKeyBytes vapidKeys

    (subscribedR,subscribedV) <- md3mreq md3switchField FieldSettings
        { fsLabel = SomeMessage MsgSubscribeToNotifications
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("icons","")]
        } ( pure (isJust subscription) )

    (endpointR,endpointV) <- mreq hiddenField "" (pushSubscriptionEndpoint . entityVal <$> subscription)
    (p256dhR,p256dhV) <- mreq hiddenField "" (pushSubscriptionP256dh . entityVal <$> subscription)
    (authR,authV) <- mreq hiddenField "" (pushSubscriptionAuth . entityVal <$> subscription)

    let r = (,) <$> subscribedR <*> (PushSubscription uid pid <$> endpointR <*> p256dhR <*> authR)
    idFormContentWrapper <- newIdent
    return (r, $(widgetFile "my/patients/subscriptions/form"))


postMyPatientRemoveR :: UserId -> DoctorId -> PatientId -> Handler Html
postMyPatientRemoveR uid did pid = do
    ((fr,_),_) <- runFormPost formPatientRemove
    case fr of
      FormSuccess () -> do
          runDB $ P.delete pid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ MyPatientsR did
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ MyPatientR uid did pid


postMyPatientsR :: DoctorId -> Handler Html
postMyPatientsR did = do

    patients <- runDB $ select $ do
        x <- from $ table @User
        where_ $ not_ $ x ^. UserSuperuser
        where_ $ not_ $ exists $ do
            y <- from $ table @Patient
            where_ $ y ^. PatientUser ==. x ^. UserId
        return x

    ((fr,fw),et) <- runFormPost $ formPatients did patients
    case fr of
      FormSuccess r -> do
          forM_ r $ \x -> runDB $ insert_ x
          addMessageI statusSuccess MsgRecordCreated
          redirect $ MyPatientsR did
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgPatients
          $(widgetFile "my/patients/new")


getMyPatientNewR :: DoctorId -> Handler Html
getMyPatientNewR did = do

    patients <- runDB $ select $ do
        x <- from $ table @User
        where_ $ not_ $ x ^. UserSuperuser
        where_ $ not_ $ exists $ do
            y <- from $ table @Patient
            where_ $ y ^. PatientUser ==. x ^. UserId
        return x

    (fw,et) <- generateFormPost $ formPatients did patients

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPatients
        $(widgetFile "my/patients/new")


formPatients :: DoctorId -> [Entity User] -> Form [Patient]
formPatients did options extra = do

    now <- liftIO getCurrentTime

    (usersR,usersV) <- mreq (usersFieldList (option <$> options)) "" Nothing

    let r = (<*>) <$> ((<*>) <$> ((<*>) <$> ((<*>) . (Patient <$>) <$> ((entityKey <$>) <$> usersR) <*> pure (pure did))
            <*> pure (pure now) )
            <*> pure (pure Nothing) )
            <*> pure (pure Nothing)
    let w = $(widgetFile "my/patients/form")
    return (r,w)
  where
      option e@(Entity _ (User email _ _ _ _ _ _ _)) = (email,e)

      usersFieldList :: RenderMessage App msg => [(msg, Entity User)] -> Field Handler [Entity User]
      usersFieldList = usersField . optionsPairs

      usersField :: Handler (OptionList (Entity User)) -> Field Handler [Entity User]
      usersField ioptlist = (multiSelectField ioptlist)
          { fieldView = \theId name attrs eval _idReq -> do
              opts <- olOptions <$> handlerToWidget ioptlist
              let optselected (Left _) _ = False
                  optselected (Right vals) opt = optionInternalValue opt `elem` vals
              [whamlet|
                <md-list ##{theId}>
                  $forall opt <- opts
                    <md-list-item type=button onclick="this.querySelector('md-checkbox').click()">
                      <img slot=start src=@{AccountPhotoR (entityKey $ optionInternalValue opt) AvatarColorDark}
                        width=56 height=56 loading=lazy style="clip-path:circle(50%)">

                      <div slot=headline>
                        $maybe name <- userName $ entityVal $ optionInternalValue opt
                          #{name}
                      <div slot=supporting-text>
                        #{optionDisplay opt}

                      <md-checkbox touch-target=wrapper slot=end onclick="event.stopPropagation()"
                        name=#{name} value=#{optionExternalValue opt} *{attrs} :optselected eval opt:checked>
              |]
          }


postMyPatientR :: UserId -> DoctorId -> PatientId -> Handler Html
postMyPatientR uid did pid = do
    
    patient <- runDB $ selectOne $ do
        x <- from $ table @Patient
        where_ $ x ^. PatientId ==. val pid
        return x

    ((fr,fw),et) <- runFormPost $ formPatient patient
    case fr of
      FormSuccess r -> do
          runDB $ replace pid r
          redirect $ MyPatientR uid did pid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPatient
              $(widgetFile "my/patients/edit")
    


getMyPatientEditR :: UserId -> DoctorId -> PatientId -> Handler Html
getMyPatientEditR uid did pid = do
    
    patient <- runDB $ selectOne $ do
        x <- from $ table @Patient
        where_ $ x ^. PatientId ==. val pid
        return x
        
    (fw,et) <- generateFormPost $ formPatient patient
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPatient
        $(widgetFile "my/patients/edit")


formPatient :: Maybe (Entity Patient) -> Form Patient
formPatient patient extra = case patient of
  Just (Entity _ (Patient uid did _ _ _)) -> do
    
    msgr <- getMessageRender
    
    (sinceR,sinceV) <- md3mreq md3datetimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgSinceDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgSinceDate)]
        } (utcToLocalTime utc . patientSince . entityVal <$> patient)
        
    (mobileR,mobileV) <- md3mopt md3telField FieldSettings
        { fsLabel = SomeMessage MsgMobile
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgMobile)]
        } (patientMobile . entityVal <$> patient)
        
    (phoneR,phoneV) <- md3mopt md3telField FieldSettings
        { fsLabel = SomeMessage MsgPhone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgPhone)]
        } (patientPhone . entityVal <$> patient)
    
    let r = Patient uid did <$> (localTimeToUTC utc <$> sinceR) <*> mobileR <*> phoneR
    let w = [whamlet|#{extra} ^{fvInput sinceV} ^{fvInput mobileV} ^{fvInput phoneV}|]
    return (r,w)
    
  Nothing -> do
      msg <- getMessageRender >>= \r -> return $ r MsgInvalidPatient
      return (FormFailure [msg], [whamlet|<p>#{msg}|])


getMyPatientR :: UserId -> DoctorId -> PatientId -> Handler Html
getMyPatientR uid did pid = do

    let polite = False

    endpoint <- lookupGetParam paramEndpoint

    doctor <- (second (join . unValue) <$>) <$> runDB ( selectOne $ do
        x :& h <- from $ table @Doctor
            `leftJoin` table @DoctorPhoto `on` (\(x :& h) -> just (x ^. DoctorId) ==. h ?. DoctorPhotoDoctor)
        where_ $ x ^. DoctorId ==. val did
        return (x, h ?. DoctorPhotoAttribution) )

    let callerName = case doctor of
          Just (Entity _ (Doctor name _ _ _ _),_) -> name
          Nothing -> "???"

    patient <- (unwrap <$>) <$> runDB ( selectOne $ do
        x :& u :& h <- from $ table @Patient
            `innerJoin` table @User `on` (\(x :& u) -> x ^. PatientUser ==. u ^. UserId)
            `leftJoin` table @UserPhoto `on` (\(_ :& u :& h) -> just (u ^. UserId) ==. h ?. UserPhotoUser)

        let subscriptions :: SqlExpr (Value Int)
            subscriptions = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. val uid
                where_ $ y ^. PushSubscriptionPublisher ==. x ^. PatientUser
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint
                return y

        let loops :: SqlExpr (Value Int)
            loops = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. PatientUser
                where_ $ y ^. PushSubscriptionPublisher ==. val uid
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint
                return y

        let accessible :: SqlExpr (Value Int)
            accessible = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. PatientUser
                where_ $ y ^. PushSubscriptionPublisher ==. val uid
                where_ $ just (y ^. PushSubscriptionEndpoint) !=. val endpoint
                return y

        where_ $ x ^. PatientId ==. val pid
        return (x, (u, (h ?. UserPhotoAttribution, (subscriptions, (loops, accessible))))) )

    patientInfo <- runDB $ selectOne $ do
        x :& i <- from $ table @Patient
            `innerJoin` table @UserInfo `on` (\(x :& i) -> x ^. PatientUser ==. i ^. UserInfoUser)
        where_ $ x ^. PatientId ==. val pid
        return i

    unread <- maybe 0 unValue <$> runDB ( selectOne $ do
        x <- from $ table @Chat
        where_ $ x ^. ChatInterlocutor ==. val uid
        where_ $ just (x ^. ChatUser) ==. subSelect
            ( do
                  y <- from $ table @Patient
                  where_ $ y ^. PatientId ==. val pid
                  return $ y ^. PatientUser
            )
        where_ $ x ^. ChatStatus ==. val ChatMessageStatusUnread
        return (countRows :: SqlExpr (Value Int)) )

    (fw,et) <- generateFormPost formPatientRemove

    backlink <- fromMaybe HomeR <$> getCurrentRoute

    let sid = uid
    msgr <- getMessageRender
    msgs <- getMessages
    case patientUser . entityVal . fst <$> patient of
      Just rid ->  defaultLayout $ do
          setTitleI MsgPatient

          idPanelDetails <- newIdent

          idButtonVideoCall <- newIdent
          idButtonAudioCall <- newIdent

          idDialogOutgoingVideoCall <- newIdent
          idAudioOutgoingVideoCallRingtone <- newIdent
          idButtonOutgoingVideoCallCancel <- newIdent

          idDialogOutgoingAudioCall <- newIdent
          idAudioOutgoingAudioCallRingtone <- newIdent
          idButtonOutgoingAudioCallCancel <- newIdent

          idDialogVideoSessionEnded <- newIdent
          idDialogCallDeclined <- newIdent
          
          $(widgetFile "my/patients/patient")

      Nothing -> invalidArgsI [MsgNoRecipient]
  where
      unwrap = second (second (bimap (join . unValue) (bimap ((> 0) . unValue) (bimap ((> 0) . unValue) ((> 0) . unValue)))))



formPatientRemove :: Form ()
formPatientRemove extra = return (pure (), [whamlet|#{extra}|])


getMyPatientsR :: DoctorId -> Handler Html
getMyPatientsR did = do
    user <- maybeAuth

    doctor <- runDB $ selectOne $ do
        x <- from $ table @Doctor
        where_ $ x ^. DoctorId ==. val did
        return x

    patients <- (second (second (bimap (join . unValue) unValue)) <$>) <$> runDB ( select $ do
        x :& u :& h <- from $ table @Patient
            `innerJoin` table @User `on` (\(x :& u) -> x ^. PatientUser ==. u ^. UserId)
            `leftJoin` table @UserPhoto `on` (\(_ :& u :& h) -> just (u ^. UserId) ==. h ?. UserPhotoUser)
        where_ $ x ^. PatientDoctor ==. val did

        let unread :: SqlExpr (Value Int)
            unread = subSelectCount $ do
                c <- from $ table @Chat
                where_ $ just (c ^. ChatInterlocutor) ==. val (entityKey <$> user)
                where_ $ just (c ^. ChatUser) ==. subSelect
                    ( do
                           y <- from $ table @Patient
                           where_ $ y ^. PatientId ==. x ^. PatientId
                           return $ y ^. PatientUser
                     )
                where_ $ c ^. ChatStatus ==. val ChatMessageStatusUnread

        return (x,(u,(h ?. UserPhotoAttribution,unread))) )


    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPatients
        idFabAdd <- newIdent
        $(widgetFile "my/patients/patients")
