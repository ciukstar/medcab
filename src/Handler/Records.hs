{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Handler.Records
  ( getRecordsR
  , getRecordR
  , getRecordNewR
  , postRecordsR
  , getRecordEditR
  , postRecordDeleR
  , postRecordR
  , getRecordMeasurementsR
  , getRecordMeasurementR
  , getRecordMeasurementNewR
  , postRecordMeasurementsR
  , getRecordMeasurementEditR
  , postRecordMeasurementDeleR
  , postRecordMeasurementR
  ) where

import Control.Monad (forM)
import Control.Monad.Trans.Reader (ReaderT)

import Data.Bifunctor (Bifunctor(bimap))
import qualified Data.Map as M (Map, fromListWith, toDescList)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (LocalTime(LocalTime, localTimeOfDay, localDay))

import Database.Esqueleto.Experimental
    ( select, from, table, where_, val, innerJoin, on, leftJoin
    , (^.), (?.), (==.), (:&)((:&))
    , orderBy, desc, asc, Value (unValue), Entity (entityVal), selectOne
    )
import Database.Persist
    (Entity (Entity), PersistStoreWrite (insert_, delete, replace))
import Database.Persist.Sql ( SqlBackend )

import Foundation
    ( Handler, Form
    , Route
      ( AuthR, AccountR, AccountPhotoR, RecordsR, RecordR, RecordNewR
      , RecordEditR, RecordDeleR, RecordMeasurementsR, RecordMeasurementR
      , RecordMeasurementNewR, RecordMeasurementEditR, RecordMeasurementDeleR
      )
    , AppMessage
      ( MsgElectronicHealthRecord, MsgUserAccount, MsgSignIn, MsgSignOut
      , MsgPhoto, MsgRecords, MsgNoDataYet, MsgAdd, MsgRecord, MsgMedicalSign
      , MsgDate, MsgTime, MsgValue, MsgUnitOfMeasure, MsgRemarks, MsgSave
      , MsgCancel, MsgBack, MsgRecordCreated, MsgDele, MsgDeleteAreYouSure
      , MsgEdit, MsgConfirmPlease, MsgInvalidFormData, MsgRecordDeleted, MsgTabs
      , MsgDetails, MsgMeasurements, MsgMeasurement, MsgName, MsgRecordEdited
      , MsgAlreadyExists
      )
    )

import Material3
    ( md3mreq, md3selectField, md3textareaField, md3mopt, md3doubleField
    , md3datetimeLocalField, md3textField, tsep
    )
import Menu (menu)
import Model
    ( AvatarColor (AvatarColorLight), statusError
    , EntityField
      ( RecordUser, RecordSign, MedSignId, UnitId, RecordTime, MedSignName
      , UnitSymbol, RecordId, MeasurementRecord, MeasurementUnit, UnitName
      , MeasurementId, MeasurementName, RecordDay
      )
    , UserId
    , Record (Record, recordSign, recordTime, recordRemarks, recordDay)
    , MedSign (MedSign), Unit (Unit), RecordId, statusSuccess
    , Measurement
      ( Measurement, measurementValue, measurementName, measurementUnit )
    , MeasurementId
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
import Yesod.Form.Functions (generateFormPost, runFormPost, checkM)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput), FormResult (FormSuccess), Field
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postRecordMeasurementDeleR :: UserId -> RecordId -> MeasurementId -> Handler Html
postRecordMeasurementDeleR uid rid mid = do
    ((fr,_),_) <- runFormPost formRecordMeasurementDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete mid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ RecordMeasurementsR uid rid
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ RecordMeasurementR uid rid mid


postRecordMeasurementR :: UserId -> RecordId -> MeasurementId -> Handler Html
postRecordMeasurementR uid rid mid = do

    measurement <- runDB $ selectOne $ do
        x <- from $ table @Measurement
        where_ $ x ^. MeasurementId ==. val mid
        return x

    ((fr,fw),et) <- runFormPost $ formRecordMeasurement rid measurement
    case fr of
      FormSuccess r -> do
          runDB $ replace mid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ RecordMeasurementR uid rid mid
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgMeasurement
          $(widgetFile "records/measurements/edit")


getRecordMeasurementEditR :: UserId -> RecordId -> MeasurementId -> Handler Html
getRecordMeasurementEditR uid rid mid = do

    measurement <- runDB $ selectOne $ do
        x <- from $ table @Measurement
        where_ $ x ^. MeasurementId ==. val mid
        return x

    (fw,et) <- generateFormPost $ formRecordMeasurement rid measurement

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgMeasurement
        $(widgetFile "records/measurements/edit")


postRecordMeasurementsR :: UserId -> RecordId -> Handler Html
postRecordMeasurementsR uid rid = do

    ((fr,fw),et) <- runFormPost $ formRecordMeasurement rid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordCreated
          redirect $ RecordMeasurementsR uid rid
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgMeasurement
          $(widgetFile "records/measurements/new")


getRecordMeasurementNewR :: UserId -> RecordId -> Handler Html
getRecordMeasurementNewR uid rid = do

    (fw,et) <- generateFormPost $ formRecordMeasurement rid Nothing

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgMeasurement
        $(widgetFile "records/measurements/new")


formRecordMeasurement :: RecordId -> Maybe (Entity Measurement) -> Form Measurement
formRecordMeasurement rid measurement extra = do

    rndr <- getMessageRender

    (valueR,valueV) <- md3mreq md3doubleField FieldSettings
        { fsLabel = SomeMessage MsgValue
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgValue)]
        } (measurementValue . entityVal <$> measurement)

    (nameR,nameV) <- md3mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgName)]
        } (measurementName . entityVal <$> measurement)

    units <- liftHandler $ (bimap ((\(a, b) -> a <> tsep <> b) . bimap unValue unValue) unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Unit
        orderBy [asc (x ^. UnitName)]
        return ((x ^. UnitSymbol, x ^. UnitName),x ^. UnitId) )

    (unitR,unitV) <- md3mopt (md3selectField (optionsPairs units)) FieldSettings
        { fsLabel = SomeMessage MsgUnitOfMeasure
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgUnitOfMeasure)]
        } (measurementUnit . entityVal <$> measurement)


    return ( Measurement rid <$> nameR <*> valueR <*> unitR
           , $(widgetFile "records/measurements/form")
           )
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName md3textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Measurement
              where_ $ x ^. MeasurementRecord ==. val rid
              where_ $ x ^. MeasurementName ==. val name
              return x
          return $ case mx of
            Nothing -> Right name
            Just (Entity mid _) -> case measurement of
              Nothing -> Left MsgAlreadyExists
              Just (Entity mid' _) | mid == mid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


getRecordMeasurementR :: UserId -> RecordId -> MeasurementId -> Handler Html
getRecordMeasurementR uid rid mid = do

    measurement <- runDB $ selectOne $ do
        x :& r :& s :& u <- from $ table @Measurement
            `innerJoin` table @Record `on` (\(x :& r) -> x ^. MeasurementRecord ==. r ^. RecordId)
            `innerJoin` table @MedSign `on` (\(_ :& r :& s) -> r ^. RecordSign ==. s ^. MedSignId)
            `leftJoin` table @Unit `on` (\(x :& _ :& _ :& u) -> x ^. MeasurementUnit ==. u ?. UnitId)

        where_ $ x ^. MeasurementId ==. val mid
        return (x,r,s,u)

    (fw,et) <- generateFormPost formRecordMeasurementDelete

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgMeasurement
        $(widgetFile "records/measurements/measurement")


formRecordMeasurementDelete :: Form ()
formRecordMeasurementDelete extra = return (FormSuccess (), [whamlet|#{extra}|])


getRecordMeasurementsR :: UserId -> RecordId -> Handler Html
getRecordMeasurementsR uid rid = do

    measurements <- runDB $ select $ do
        x :& u <- from $ table @Measurement
            `leftJoin` table @Unit `on` (\(x :& u) -> x ^. MeasurementUnit ==. u ?. UnitId)
        where_ $ x ^. MeasurementRecord ==. val rid
        orderBy [asc (x ^. MeasurementId)]
        return (x,u)

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgMeasurements
        idPanelMeasurements <- newIdent
        idFabAdd <- newIdent
        $(widgetFile "records/measurements/measurements")


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
        } (LocalTime <$> (recordDay . entityVal <$> record) <*> (recordTime . entityVal <$> record))

    (remarksR,remarksV) <- md3mopt md3textareaField FieldSettings
        { fsLabel = SomeMessage MsgRemarks
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgRemarks)]
        } (recordRemarks . entityVal <$> record)

    let r = Record uid <$> signR <*> (localDay <$> timeR) <*> (localTimeOfDay <$> timeR) <*> remarksR
    let w = $(widgetFile "records/form")
    return (r,w)


getRecordR :: UserId -> RecordId -> Handler Html
getRecordR uid rid = do

    r <- runDB $ selectOne $ do
        x :& s <- from $ table @Record
            `innerJoin` table @MedSign `on` (\(x :& s) -> x ^. RecordSign ==. s ^. MedSignId)
        where_ $ x ^. RecordId ==. val rid
        return (x,s)

    record <- mapM (\e@(Entity xid _,_) -> (e,) <$> runDB (measurements xid)) r

    (fw,et) <- generateFormPost formRecordDelete
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRecord
        idPanelDetails <- newIdent
        $(widgetFile "records/record")

  where

      measurements :: RecordId -> ReaderT SqlBackend Handler [(Entity Measurement,Maybe (Entity Unit))]
      measurements xid = select $ do
          x :& u <- from $ table @Measurement
              `leftJoin` table @Unit `on` (\(x :& u) -> x ^. MeasurementUnit ==. u ?. UnitId)
          where_ $ x ^. MeasurementRecord ==. val xid
          orderBy [asc (x ^. MeasurementId)]
          return (x,u)


formRecordDelete :: Form ()
formRecordDelete extra = return (pure (), [whamlet|#{extra}|])


getRecordsR :: UserId -> Handler Html
getRecordsR uid = do

    xs <- runDB ( select $ do
        x :& s <- from $ table @Record
            `innerJoin` table @MedSign `on` (\(x :& s) -> x ^. RecordSign ==. s ^. MedSignId)
        where_ $ x ^. RecordUser ==. val uid
        orderBy [desc (x ^. RecordDay), desc (x ^. RecordTime)]
        return (x,s) )

    groups <- groupByDay <$> forM xs ( \r@(Entity rid _,_) -> (r,) <$> runDB ( select $ do
        x :& u <- from $ table @Measurement
            `leftJoin` table @Unit `on` (\(x :& u) -> x ^. MeasurementUnit ==. u ?. UnitId)
        orderBy [asc (x ^. MeasurementId)]
        where_ $ x ^. MeasurementRecord ==. val rid
        return (x,u) ) )


    user <- maybeAuth
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgElectronicHealthRecord
        idFabAdd <- newIdent
        $(widgetFile "records/records")

  where

      groupByDay :: [((Entity Record,Entity MedSign),[(Entity Measurement,Maybe (Entity Unit))])]
                 -> [(Day,[((Entity Record,Entity MedSign),[(Entity Measurement,Maybe (Entity Unit))])])]
      groupByDay = M.toDescList . groupByKey (recordDay . entityVal . fst . fst)

      groupByKey :: (Ord k) => (v -> k) -> [v] -> M.Map k [v]
      groupByKey key = M.fromListWith (<>) . fmap (\x -> (key x,[x]))

      irange = [1 :: Int ..]
