{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Handler.MyPatients
  ( getMyPatientsR
  , getMyPatientR
  , getMyPatientNewR
  , postMyPatientsR
  , postMyPatientRemoveR
  , getMyPatientNotificationsR
  , postMyPatientNotificationsR
  ) where


import ChatRoom.Data ( Route(PatientChatRoomR) )

import Control.Monad (join, forM_)
import Control.Monad.IO.Class (liftIO)

import Data.Bifunctor (Bifunctor(second, bimap))
import Data.Maybe (fromMaybe, isJust)
import Data.Time.Clock (getCurrentTime)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val, innerJoin, on, just
    , (^.), (?.), (==.), (!=.), (:&) ((:&))
    , SqlExpr, Value (unValue), leftJoin, not_, exists, countRows, subSelect
    , subSelectCount, delete
    )
import Database.Persist
    ( Entity (Entity, entityKey, entityVal), PersistStoreWrite (insert_)
    , PersistUniqueWrite (upsertBy), (=.)
    )
import qualified Database.Persist as P (delete)
import Database.Persist.Sql (fromSqlKey)

import Foundation (Form, getVAPIDKeys)
import Foundation.Data
    ( Handler, App
    , Route
      ( AccountPhotoR, MyPatientR, MyPatientNewR, MyPatientsR
      , MyPatientRemoveR, ChatR, VideoR, MyPatientNotificationsR, StaticR, HomeR
      )
    , AppMessage
      ( MsgPatients, MsgNoPatientsYet
      , MsgPhoto, MsgEdit, MsgSinceDate, MsgCancel, MsgSave, MsgBack, MsgPatient
      , MsgRecordCreated, MsgFullName, MsgDele, MsgConfirmPlease, MsgChat
      , MsgEmailAddress, MsgRemoveAreYouSure, MsgAudioCall, MsgInvalidFormData
      , MsgVideoCall, MsgRecordDeleted, MsgRemove, MsgDetails, MsgTabs
      , MsgNotifications, MsgSubscribeToNotifications, MsgNotGeneratedVAPID
      , MsgNoRecipient, MsgOutgoingCall, MsgNotSubscribedToNotificationsFromPatient
      , MsgYouAndUserSubscribedOnSameDevice, MsgAllowUserToSendYouNotifications
      , MsgUserUnavailable
      )
    )

import Material3 (md3switchField, md3mreq)

import Model
    ( statusError, statusSuccess, paramEndpoint
    , AvatarColor (AvatarColorDark)
    , ChatMessageStatus (ChatMessageStatusUnread)
    , PushMsgType
      ( PushMsgTypeCall, PushMsgTypeCancel, PushMsgTypeDecline
      , PushMsgTypeAccept
      )
    , UserId, User (User, userName), UserPhoto, DoctorId, Doctor
    , PatientId, Patient(Patient, patientUser), Chat
    , PushSubscription
      ( PushSubscription, pushSubscriptionEndpoint, pushSubscriptionP256dh
      , pushSubscriptionAuth
      )
    , Unique (UniquePushSubscription)
    , EntityField
      ( PatientUser, UserId, PatientDoctor, UserPhotoUser, PatientId
      , UserPhotoAttribution, UserSuperuser, DoctorId, ChatInterlocutor
      , ChatStatus, ChatUser, PushSubscriptionUser
      , PushSubscriptionP256dh
      , PushSubscriptionAuth, PushSubscriptionEndpoint, DoctorUser
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles (img_call_FILL0_wght400_GRAD0_opsz24_svg)

import Text.Hamlet (Html)
import Text.Shakespeare.I18N (RenderMessage, SomeMessage (SomeMessage))

import VideoRoom.Data (ChanId (ChanId), Route (PushMessageR, RoomR))

import Web.WebPush ( vapidPublicKeyBytes, VAPIDKeys )
    
import Widgets (widgetMenu, widgetUser, widgetBanner, widgetSnackbar)

import Yesod.Auth (maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), MonadHandler (liftHandler), ToJSON (toJSON)
    , setTitleI, getMessages, newIdent, addMessageI, redirect, whamlet
    , handlerToWidget, invalidArgsI, lookupGetParam, getCurrentRoute
    , lookupGetParam
    )
import Yesod.Form
    ( FieldView (fvInput, fvLabel, fvId), FormResult (FormSuccess), runFormPost
    , Field (fieldView), OptionList (olOptions)
    , multiSelectField, optionsPairs, hiddenField
    , Option (optionInternalValue, optionExternalValue, optionDisplay)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Form.Functions (generateFormPost, mreq)
import Yesod.Persist (YesodPersist(runDB))


postMyPatientNotificationsR :: UserId -> DoctorId -> PatientId -> Handler Html
postMyPatientNotificationsR uid did pid = do
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
                redirect $ MyPatientNotificationsR uid did pid
                
            FormSuccess (False, PushSubscription uid' endpoint' _ _) -> do
                _ <- runDB $ delete $ do
                    y <- from $ table @PushSubscription
                    where_ $ y ^. PushSubscriptionUser ==. val uid'
                    where_ $ y ^. PushSubscriptionEndpoint ==. val endpoint'
                addMessageI statusSuccess MsgRecordDeleted
                redirect $ MyPatientNotificationsR uid did pid
                
            _otherwise -> do
                addMessageI statusError MsgInvalidFormData
                redirect $ MyPatientNotificationsR uid did pid
                
      Nothing -> invalidArgsI [MsgNotGeneratedVAPID]


getMyPatientNotificationsR :: UserId -> DoctorId -> PatientId -> Handler Html
getMyPatientNotificationsR uid did pid = do

    vapidKeys <- getVAPIDKeys

    case vapidKeys of
      Just vapid -> do

          endpoint <- lookupGetParam paramEndpoint
          
          patient <- (unwrap <$>) <$> runDB ( selectOne $ do
              x :& u :& h :& d <- from $ table @Patient
                  `innerJoin` table @User `on` (\(x :& u) -> x ^. PatientUser ==. u ^. UserId)
                  `leftJoin` table @UserPhoto `on` (\(_ :& u :& h) -> just (u ^. UserId) ==. h ?. UserPhotoUser)
                  `innerJoin` table @Doctor `on` (\(x :& _ :& _ :& d) -> x ^. PatientDoctor ==. d ^. DoctorId)

              let subscriptions :: SqlExpr (Value Int)
                  subscriptions = subSelectCount $ do
                      y <- from $ table @PushSubscription
                      where_ $ just (y ^. PushSubscriptionUser) ==. d ^. DoctorUser
                      where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint
                      return y

              let loops :: SqlExpr (Value Int)
                  loops = subSelectCount $ do
                      y <- from $ table @PushSubscription
                      where_ $ y ^. PushSubscriptionUser ==. x ^. PatientUser
                      where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint
                      return y

              let accessible :: SqlExpr (Value Int)
                  accessible = subSelectCount $ do
                      y <- from $ table @PushSubscription
                      where_ $ y ^. PushSubscriptionUser ==. x ^. PatientUser
                      where_ $ just (y ^. PushSubscriptionEndpoint) !=. val endpoint
                      return y
                  
              where_ $ x ^. PatientId ==. val pid
              return (x, (u, (h ?. UserPhotoAttribution, (subscriptions, (loops, accessible))))) )

          subscription <- runDB ( selectOne $ do
              x <- from $ table @PushSubscription
              where_ $ x ^. PushSubscriptionUser ==. val uid
              where_ $ just (x ^. PushSubscriptionEndpoint) ==. val endpoint
              return x )

          (fw,et) <- generateFormPost $ formNotifications vapid uid subscription
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPatient
              idPanelNotifications <- newIdent
              idFormSubscription <- newIdent
              $(widgetFile "my/patients/notifications/notifications")

      Nothing -> invalidArgsI [MsgNotGeneratedVAPID]
  where
      unwrap = second (second (bimap (join . unValue) (bimap ((> 0) . unValue) (bimap ((> 0) . unValue) ((> 0) . unValue)))))


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
    return (r, $(widgetFile "my/patients/notifications/form"))


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

    let r = (<*>) <$> ((<*>) . (Patient <$>) <$> ((entityKey <$>) <$> usersR) <*> pure (pure did)) <*> pure (pure now)
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


getMyPatientR :: UserId -> DoctorId -> PatientId -> Handler Html
getMyPatientR uid did pid = do

    let polite = False

    endpoint <- lookupGetParam paramEndpoint
    
    patient <- (unwrap <$>) <$> runDB ( selectOne $ do
        x :& u :& h :& d <- from $ table @Patient
            `innerJoin` table @User `on` (\(x :& u) -> x ^. PatientUser ==. u ^. UserId)
            `leftJoin` table @UserPhoto `on` (\(_ :& u :& h) -> just (u ^. UserId) ==. h ?. UserPhotoUser)
            `innerJoin` table @Doctor `on` (\(x :& _ :& _ :& d) -> x ^. PatientDoctor ==. d ^. DoctorId)

        let subscriptions :: SqlExpr (Value Int)
            subscriptions = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ just (y ^. PushSubscriptionUser) ==. d ^. DoctorUser
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint
                return y

        let loops :: SqlExpr (Value Int)
            loops = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionUser ==. x ^. PatientUser
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint
                return y

        let accessible :: SqlExpr (Value Int)
            accessible = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionUser ==. x ^. PatientUser
                where_ $ just (y ^. PushSubscriptionEndpoint) !=. val endpoint
                return y
            
        where_ $ x ^. PatientId ==. val pid
        return (x, (u, (h ?. UserPhotoAttribution, (subscriptions, (loops, accessible))))) )

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
    msgs <- getMessages

    backlink <- fromMaybe HomeR <$> getCurrentRoute

    let sid = uid
    case patientUser . entityVal . fst <$> patient of
      Just rid ->  defaultLayout $ do
          setTitleI MsgPatient

          idPanelDetails <- newIdent
          idButtonVideoCall <- newIdent
          idDialogOutgoingCall <- newIdent
          idButtonOutgoingCallCancel <- newIdent
          idDialogVideoSessionEnded <- newIdent
          idDialogCallDeclined <- newIdent

          let ChanId channel = ChanId (fromIntegral (fromSqlKey pid))
          
          $(widgetFile "my/patients/patient")
          $(widgetFile "video/outgoing")

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
