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
import VideoRoom.Data ( Route(PatientVideoRoomR) )

import Control.Monad (join, forM_)
import Control.Monad.IO.Class (liftIO)

import Data.Bifunctor (Bifunctor(second, bimap))
import Data.Text (pack)
import Data.Time.Clock (getCurrentTime)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val, innerJoin, on, just
    , (^.), (?.), (==.), (:&) ((:&))
    , SqlExpr, Value (unValue), leftJoin, not_, exists, countRows, subSelect
    , subSelectCount
    )
import Database.Persist
    ( Entity (Entity, entityKey, entityVal), PersistStoreWrite (insert_, delete)
    )
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler, Form, App
    , Route
      ( AuthR, AccountPhotoR, MyPatientR, AccountR, MyPatientNewR, MyPatientsR
      , MyPatientRemoveR, ChatR, VideoR, PushSubscriptionR, PushSubscriptionsR
      , MyPatientNotificationsR, PushMessageR
      )
    , AppMessage
      ( MsgPatients, MsgUserAccount, MsgSignIn, MsgSignOut, MsgNoPatientsYet
      , MsgPhoto, MsgEdit, MsgSinceDate, MsgCancel, MsgSave, MsgBack, MsgPatient
      , MsgRecordCreated, MsgFullName, MsgDele, MsgConfirmPlease, MsgChat
      , MsgEmailAddress, MsgRemoveAreYouSure, MsgAudioCall, MsgInvalidFormData
      , MsgVideoCall, MsgRecordDeleted, MsgRemove, MsgDetails, MsgTabs
      , MsgNotifications, MsgSubscribeToNotifications, MsgNoRecipient, MsgInvalidVAPID
      )
    )

import Material3 (md3switchField, md3mreq)
import Menu (menu)
import Model
    ( statusError, statusSuccess, secretVolumeVapid
    , AvatarColor (AvatarColorLight, AvatarColorDark)
    , ChatMessageStatus (ChatMessageStatusUnread), UserId, User (User, userName)
    , UserPhoto, DoctorId, Doctor, PatientId, Patient(Patient), Chat
    , PushSubscription
    , EntityField
      ( PatientUser, UserId, PatientDoctor, UserPhotoUser, PatientId
      , UserPhotoAttribution, UserSuperuser, DoctorId, ChatInterlocutor
      , ChatStatus, ChatUser, PushSubscriptionUser
      )
    )

import Settings (widgetFile)

import System.IO (readFile')

import Text.Read (readMaybe)
import Text.Hamlet (Html)
import Text.Julius (RawJS(rawJS))
import Text.Shakespeare.I18N (RenderMessage, SomeMessage (SomeMessage))

import Web.WebPush
    ( readVAPIDKeys, vapidPublicKeyBytes, VAPIDKeys
    , VAPIDKeysMinDetails (VAPIDKeysMinDetails)
    )

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, getMessages, newIdent, addMessageI
    , redirect, whamlet, handlerToWidget, ToJSON (toJSON), invalidArgsI
    )
import Yesod.Form
    ( FieldView (fvInput, fvLabel, fvId), FormResult (FormSuccess), runFormPost
    , Field (fieldView), OptionList (olOptions)
    , multiSelectField, optionsPairs
    , Option (optionInternalValue, optionExternalValue, optionDisplay)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Form.Functions (generateFormPost, mreq)
import Yesod.Persist (YesodPersist(runDB))


postMyPatientNotificationsR :: UserId -> DoctorId -> PatientId -> Handler Html
postMyPatientNotificationsR _uid _did _pid = undefined


getMyPatientNotificationsR :: UserId -> DoctorId -> PatientId -> Handler Html
getMyPatientNotificationsR uid did pid = do

    details <- liftIO $ ((\(s,x,y) -> VAPIDKeysMinDetails s x y) <$>) . readMaybe <$> readFile' secretVolumeVapid
    
    case details of
      Just vapidKeysMinDetails -> do
          patient <- (second (second (join . unValue)) <$>) <$> runDB ( selectOne $ do
              x :& u :& h <- from $ table @Patient
                  `innerJoin` table @User `on` (\(x :& u) -> x ^. PatientUser ==. u ^. UserId)
                  `leftJoin` table @UserPhoto `on` (\(_ :& u :& h) -> just (u ^. UserId) ==. h ?. UserPhotoUser)
              where_ $ x ^. PatientId ==. val pid
              return (x,(u,h ?. UserPhotoAttribution)) )

          permission <- (\case Just _ -> True; Nothing -> False) <$> runDB ( selectOne $ do
              x <- from $ table @PushSubscription
              where_ $ x ^. PushSubscriptionUser ==. val uid
              return x )
              
          let vapidKeys = readVAPIDKeys vapidKeysMinDetails
          (fw,et) <- generateFormPost $ formNotifications vapidKeys uid permission

          defaultLayout $ do
              setTitleI MsgPatient
              idPanelNotifications <- newIdent
              $(widgetFile "my/patients/notifications/notifications")
              
      Nothing -> invalidArgsI [MsgInvalidVAPID]


formNotifications :: VAPIDKeys -> UserId -> Bool -> Form Bool
formNotifications vapidKeys uid permission extra = do

    let userId = pack $ show (fromSqlKey uid)
    let applicationServerKey = vapidPublicKeyBytes vapidKeys

    (r,v) <- md3mreq md3switchField FieldSettings
        { fsLabel = SomeMessage MsgSubscribeToNotifications
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("icons","")]
        } ( pure permission )

    return (r,$(widgetFile "my/patients/notifications/form"))


postMyPatientRemoveR :: UserId -> DoctorId -> PatientId -> Handler Html
postMyPatientRemoveR uid did pid = do
    ((fr,_),_) <- runFormPost formPatientRemove
    case fr of
      FormSuccess () -> do
          runDB $ delete pid
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
    
    patient <- (second (second (join . unValue)) <$>) <$> runDB ( selectOne $ do
        x :& u :& h <- from $ table @Patient
            `innerJoin` table @User `on` (\(x :& u) -> x ^. PatientUser ==. u ^. UserId)
            `leftJoin` table @UserPhoto `on` (\(_ :& u :& h) -> just (u ^. UserId) ==. h ?. UserPhotoUser)
        where_ $ x ^. PatientId ==. val pid
        return (x,(u,h ?. UserPhotoAttribution)) )

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

    

    let sid = uid
    case entityKey . fst . snd <$> patient of
      Just rid -> do
          (fw,et) <- generateFormPost formPatientRemove
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPatient
              idPanelDetails <- newIdent
              $(widgetFile "my/patients/patient")
      Nothing -> invalidArgsI [MsgNoRecipient]



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
