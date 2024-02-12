{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Records
  ( getRecordsR
  , getRecordR
  , getRecordNewR
  , postRecordsR
  , getRecordEditR
  , postRecordDeleR
  , postRecordR
  ) where

import Data.Bifunctor (Bifunctor(bimap))
import Database.Esqueleto.Experimental
    ( select, from, table, where_, val, innerJoin, on, leftJoin
    , (^.), (?.), (==.), (:&)((:&))
    , orderBy, desc, asc, Value (unValue), Entity (entityVal), selectOne
    )
import Database.Persist
    (Entity (Entity), PersistStoreWrite (insert_, delete, replace))

import Foundation
    ( Handler, Form
    , Route
      ( AuthR, AccountR, AccountPhotoR, RecordsR, RecordR, RecordNewR
      , RecordEditR, RecordDeleR
      )
    , AppMessage
      ( MsgElectronicHealthRecord, MsgUserAccount, MsgSignIn, MsgSignOut
      , MsgPhoto, MsgRecords, MsgNoDataYet, MsgAdd, MsgRecord, MsgMedicalSign
      , MsgDate, MsgTime, MsgValue, MsgUnitOfMeasure, MsgRemarks, MsgSave
      , MsgCancel, MsgBack, MsgRecordCreated, MsgDele, MsgDeleteAreYouSure
      , MsgEdit, MsgConfirmPlease, MsgInvalidFormData, MsgRecordDeleted
      )
    )

import Material3
    ( md3mreq, md3selectField, md3textareaField, md3mopt, md3doubleField
    , md3datetimeLocalField
    )
import Menu (menu)
import Model
    ( AvatarColor (AvatarColorLight), statusError
    , EntityField
      ( RecordUser, RecordSign, MedSignId, RecordUnit, UnitId
      , RecordTime, MedSignName, UnitSymbol, RecordId
      )
    , UserId
    , Record
      ( Record, recordSign, recordTime, recordValue, recordUnit
      , recordRemarks
      )
    , MedSign (MedSign), Unit (Unit), RecordId, statusSuccess
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Auth (Route (LoginR, LogoutR), maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), getMessages, newIdent, SomeMessage (SomeMessage)
    , getMessageRender, MonadHandler (liftHandler), addMessageI, redirect
    , whamlet
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Fields (optionsPairs)
import Yesod.Form.Functions (generateFormPost, runFormPost)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput), FormResult (FormSuccess)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postRecordDeleR :: UserId -> RecordId -> Handler Html
postRecordDeleR uid rid = do
    ((fr,_),_) <- runFormPost formRecordDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete rid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ RecordsR uid
      _otherwise -> defaultLayout $ do
          addMessageI statusError MsgInvalidFormData
          redirect $ RecordR uid rid


postRecordR :: UserId -> RecordId -> Handler Html
postRecordR uid rid = do

    ((fr,fw),et) <- runFormPost $ formRecord uid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ replace rid r
          addMessageI statusSuccess MsgRecordCreated
          redirect $ RecordR uid rid
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgRecord
          $(widgetFile "records/edit")


getRecordEditR :: UserId -> RecordId -> Handler Html
getRecordEditR uid rid = do

    record <- runDB $ selectOne $ do
        x <- from $ table @Record
        where_ $ x ^. RecordId ==. val rid
        return x

    (fw,et) <- generateFormPost $ formRecord uid record
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRecord
        $(widgetFile "records/edit")


postRecordsR :: UserId -> Handler Html
postRecordsR uid = do
    ((fr,fw),et) <- runFormPost $ formRecord uid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordCreated
          redirect $ RecordsR uid
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgRecord
          $(widgetFile "records/new")


getRecordNewR :: UserId -> Handler Html
getRecordNewR uid = do
    (fw,et) <- generateFormPost $ formRecord uid Nothing
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRecord
        $(widgetFile "records/new")


formRecord :: UserId -> Maybe (Entity Record) -> Form Record
formRecord uid record extra = do

    rndr <- getMessageRender

    signs <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @MedSign
        orderBy [asc (x ^. MedSignName)]
        return (x ^. MedSignName, x ^. MedSignId) )

    (signR,signV) <- md3mreq (md3selectField (optionsPairs signs)) FieldSettings
        { fsLabel = SomeMessage MsgMedicalSign
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgMedicalSign)]
        } (recordSign . entityVal <$> record)

    (timeR,timeV) <- md3mreq md3datetimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgTime)]
        } (recordTime . entityVal <$> record)

    (valueR,valueV) <- md3mreq md3doubleField FieldSettings
        { fsLabel = SomeMessage MsgValue
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgValue)]
        } (recordValue . entityVal <$> record)

    units <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Unit
        orderBy [asc (x ^. UnitSymbol)]
        return (x ^. UnitSymbol, x ^. UnitId) )

    (unitR,unitV) <- md3mopt (md3selectField (optionsPairs units)) FieldSettings
        { fsLabel = SomeMessage MsgUnitOfMeasure
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgUnitOfMeasure)]
        } (recordUnit . entityVal <$> record)

    (remarksR,remarksV) <- md3mopt md3textareaField FieldSettings
        { fsLabel = SomeMessage MsgRemarks
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgRemarks)]
        } (recordRemarks . entityVal <$> record)

    let r = Record uid <$> signR <*> timeR <*> valueR <*> unitR <*> remarksR
    let w = $(widgetFile "records/form")
    return (r,w)


getRecordR :: UserId -> RecordId -> Handler Html
getRecordR uid rid = do

    record <- runDB $ selectOne $ do
        x :& s :& u <- from $ table @Record
            `innerJoin` table @MedSign `on` (\(x :& s) -> x ^. RecordSign ==. s ^. MedSignId)
            `leftJoin` table @Unit `on` (\(x :& _ :& u) -> x ^. RecordUnit ==. u ?. UnitId)
        where_ $ x ^. RecordId ==. val rid
        return (x,s,u)

    (fw,et) <- generateFormPost formRecordDelete
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRecord
        $(widgetFile "records/record")


formRecordDelete :: Form ()
formRecordDelete extra = return (pure (), [whamlet|#{extra}|])


getRecordsR :: UserId -> Handler Html
getRecordsR uid = do

    records <- runDB $ select $ do
        x :& s :& u <- from $ table @Record
            `innerJoin` table @MedSign `on` (\(x :& s) -> x ^. RecordSign ==. s ^. MedSignId)
            `leftJoin` table @Unit `on` (\(x :& _ :& u) -> x ^. RecordUnit ==. u ?. UnitId)
        where_ $ x ^. RecordUser ==. val uid
        orderBy [desc (x ^. RecordTime)]
        return (x,s,u)

    user <- maybeAuth
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgElectronicHealthRecord
        idFabAdd <- newIdent
        $(widgetFile "records/records")
