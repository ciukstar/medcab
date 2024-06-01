{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Units
  ( getUnitsR
  , getUnitAddR
  , getUnitR
  , postUnitsR
  , getUnitEditR
  , postUnitR
  , postUnitDeleR
  , getQuantitiesR
  , getQuantityR
  , getQuantityAddR
  , postQuantitiesR
  , getQuantityEditR
  , postQuantityDeleR
  , postQuantityR
  , getQuantityUnitsR
  , getQuantityUnitCreateR
  , postQuantityUnitsR
  , getQuantityUnitR
  , getQuantityUnitEditR
  , postQuantityUnitDeleR
  , postQuantityUnitR
  ) where

import Control.Applicative ((<|>))
import Control.Monad (unless)

import Data.Maybe (mapMaybe)
import Data.Text (Text, unpack, pack)
import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, justList, valList, in_
    , (^.), (?.), (==.), (:&) ((:&))
    , asc, selectOne, where_, val, Value (unValue), leftJoin, on, just
    )
import Database.Persist
    ( Entity (Entity), entityVal, PersistStoreWrite (insert_, delete, replace)
    )
import Database.Persist.Sql (toSqlKey, fromSqlKey)

import Foundation
    ( Handler, Form
    , Route (DataR)
    , DataR
      ( UnitsR, UnitR, UnitAddR, UnitEditR, UnitDeleR, QuantitiesR, QuantityR
      , QuantityAddR, QuantityEditR, QuantityDeleR, QuantityUnitsR
      , QuantityUnitCreateR, QuantityUnitR, QuantityUnitDeleR, QuantityUnitEditR
      )
    , AppMessage
      ( MsgMeasurementUnits, MsgAdd, MsgName
      , MsgUnitsOfMeasure, MsgNoDataYet, MsgMeasurementUnit
      , MsgDescription, MsgSymbol, MsgBack, MsgCancel, MsgSave, MsgRecordCreated
      , MsgInvalidFormData, MsgRecordEdited, MsgConfirmPlease, MsgDescription
      , MsgDeleteAreYouSure, MsgEdit, MsgDele, MsgAlreadyExists, MsgQuantities
      , MsgConfigure, MsgQuantity, MsgRecordDeleted, MsgTabs, MsgDetails
      , MsgUnitOfMeasure
      )
    )

import Material3 (md3mreq, md3textField, md3textareaField, md3mopt, md3selectField)

import Model
    ( statusError, statusSuccess
    , EntityField (UnitName, UnitId, QuantityName, QuantityId, UnitQuantity)
    , UnitId, Unit(Unit, unitName, unitSymbol, unitDescr, unitQuantity)
    , QuantityId, Quantity (Quantity, quantityName, quantityDescr)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Read (readMaybe)

import Widgets (widgetMenu, widgetUser, widgetSnackbar, widgetBanner)

import Yesod.Core
    ( Yesod(defaultLayout), newIdent, getMessages, redirect
    , SomeMessage (SomeMessage), getMessageRender, addMessageI
    , YesodRequest (reqGetParams), getRequest, MonadHandler (liftHandler)
    )
import Yesod.Core.Widget (setTitleI, whamlet)

import Yesod.Form.Functions (generateFormPost, runFormPost, checkM)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput), FormResult (FormSuccess), Field
    )
import Yesod.Persist.Core (YesodPersist(runDB))
import Yesod.Form.Fields (optionsPairs)
import ClassyPrelude (Bifunctor(bimap))


postQuantityUnitDeleR :: QuantityId -> UnitId -> Handler Html
postQuantityUnitDeleR qid uid = do
    ((fr,_),_) <- runFormPost formQuantityUnitDele
    case fr of
      FormSuccess () -> do
          runDB $ delete uid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ QuantityUnitsR qid
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ QuantityUnitR qid uid


postQuantityUnitR :: QuantityId -> UnitId -> Handler Html
postQuantityUnitR qid uid = do
    
    unit <- runDB $ selectOne $ do
        x <- from $ table @Unit
        where_ $ x ^. UnitId ==. val uid
        return x
        
    ((fr,fw),et) <- runFormPost $ formQuantityUnit qid unit
    case fr of
      FormSuccess r -> do
          runDB $ replace uid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ QuantityUnitR qid uid
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgUnitOfMeasure
          $(widgetFile "data/units/quantities/units/edit")


getQuantityUnitEditR :: QuantityId -> UnitId -> Handler Html
getQuantityUnitEditR qid uid = do
    
    unit <- runDB $ selectOne $ do
        x <- from $ table @Unit
        where_ $ x ^. UnitId ==. val uid
        return x
        
    (fw,et) <- generateFormPost $ formQuantityUnit qid unit
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUnitOfMeasure
        $(widgetFile "data/units/quantities/units/edit")


getQuantityUnitR :: QuantityId -> UnitId -> Handler Html
getQuantityUnitR qid uid = do
    
    unit <- runDB $ selectOne $ do
        x <- from $ table @Unit
        where_ $ x ^. UnitId ==. val uid
        return x
        
    (fw,et) <- generateFormPost formQuantityUnitDele
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUnitOfMeasure
        $(widgetFile "data/units/quantities/units/unit")


formQuantityUnitDele :: Form ()
formQuantityUnitDele extra = return (pure (),[whamlet|#{extra}|])


postQuantityUnitsR :: QuantityId -> Handler Html
postQuantityUnitsR qid = do
    ((fr,fw),et) <- runFormPost $ formQuantityUnit qid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordCreated
          redirect $ DataR $ QuantityUnitsR qid
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgUnitOfMeasure
          $(widgetFile "data/units/quantities/units/create")


getQuantityUnitCreateR :: QuantityId -> Handler Html
getQuantityUnitCreateR qid = do
    (fw,et) <- generateFormPost $ formQuantityUnit qid Nothing
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUnitOfMeasure
        $(widgetFile "data/units/quantities/units/create")


formQuantityUnit :: QuantityId -> Maybe (Entity Unit) -> Form Unit
formQuantityUnit qid unit extra = do

    rndr <- getMessageRender
    
    (nameR,nameV) <- md3mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgName)]
        } (unitName . entityVal <$> unit)
        
    (symbolR,symbolV) <- md3mreq md3textField FieldSettings
        { fsLabel = SomeMessage MsgSymbol
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgSymbol)]
        } (unitSymbol . entityVal <$> unit)
        
    (descrR,descrV) <- md3mopt md3textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgDescription)]
        } (unitDescr . entityVal <$> unit)

    quantities <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Quantity
        orderBy [asc (x ^. QuantityName)]
        return (x ^. QuantityName, x ^. QuantityId) )

    (quantityR,quantityV) <- md3mopt (md3selectField (optionsPairs quantities)) FieldSettings
        { fsLabel = SomeMessage MsgQuantity
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgQuantity)]
        } (unitQuantity . entityVal <$> unit <|> pure (Just qid))

    let r = Unit <$> nameR <*> symbolR <*> descrR <*> quantityR
    let w = $(widgetFile "data/units/form")
    return (r,w)
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName md3textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Unit
              where_ $ x ^. UnitName ==. val name
              return x
          return $ case mx of
            Nothing -> Right name
            Just (Entity uid _) -> case unit of
              Nothing -> Left MsgAlreadyExists
              Just (Entity uid' _) | uid == uid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


getQuantityUnitsR :: QuantityId -> Handler Html
getQuantityUnitsR qid = do

    units <- runDB $ select $ do
        x <- from $ table @Unit
        where_ $ x ^. UnitQuantity ==. just (val qid)
        orderBy [asc (x ^. UnitName)]
        return x
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgMeasurementUnits
        idPanelUnits <- newIdent
        idFabAdd <- newIdent
        $(widgetFile "data/units/quantities/units/units")


postQuantityDeleR :: QuantityId -> Handler Html
postQuantityDeleR qid = do
    ((fr,_),_) <- runFormPost formQuantityDele
    case fr of
      FormSuccess () -> do
          runDB $ delete qid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR QuantitiesR
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ QuantityR qid


formQuantityDele :: Form ()
formQuantityDele extra = return (FormSuccess (), [whamlet|#{extra}|])


postQuantityR :: QuantityId -> Handler Html
postQuantityR qid = do

    quantity <- runDB $ selectOne $ do
        x <- from $ table @Quantity
        where_ $ x ^. QuantityId ==. val qid
        return x

    ((fr,fw),et) <- runFormPost $ formQuantity quantity
    case fr of
      FormSuccess r -> do
          runDB $ replace qid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ QuantityR qid
      _otherwise -> defaultLayout $ do
          setTitleI MsgQuantity
          msgs <- getMessages
          $(widgetFile "data/units/quantities/edit")


getQuantityEditR :: QuantityId -> Handler Html
getQuantityEditR qid = do

    quantity <- runDB $ selectOne $ do
        x <- from $ table @Quantity
        where_ $ x ^. QuantityId ==. val qid
        return x

    (fw,et) <- generateFormPost $ formQuantity quantity
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgQuantity
        $(widgetFile "data/units/quantities/edit")


getQuantityR :: QuantityId -> Handler Html
getQuantityR qid = do
    
    quantity <- runDB $ selectOne $ do
        x <- from $ table @Quantity
        where_ $ x ^. QuantityId ==. val qid
        return x

    (fw,et) <- generateFormPost formQuantityDele
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgQuantity
        idPanelDetails <- newIdent
        $(widgetFile "data/units/quantities/quantity")


postQuantitiesR :: Handler Html
postQuantitiesR = do
    ((fr,fw),et) <- runFormPost $ formQuantity Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordCreated
          redirect $ DataR QuantitiesR
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgQuantity
          $(widgetFile "data/units/quantities/create")


getQuantityAddR :: Handler Html
getQuantityAddR = do
    (fw,et) <- generateFormPost $ formQuantity Nothing
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgQuantity
        $(widgetFile "data/units/quantities/create")


formQuantity :: Maybe (Entity Quantity) -> Form Quantity
formQuantity quantity extra = do
    rndr <- getMessageRender
    
    (nameR,nameV) <- md3mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgName)]
        } ( quantityName . entityVal <$> quantity)
        
    (descrR,descrV) <- md3mopt md3textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgDescription)]
        } ( quantityDescr . entityVal <$> quantity)

    let r = Quantity <$> nameR <*> descrR
    let w = $(widgetFile "data/units/quantities/form")
    return (r,w)
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName md3textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Quantity
              where_ $ x ^. QuantityName ==. val name
              return x
          return $ case mx of
            Nothing -> Right name
            Just (Entity qid _) -> case quantity of
              Nothing -> Left MsgAlreadyExists
              Just (Entity qid' _) | qid == qid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


getQuantitiesR :: Handler Html
getQuantitiesR = do

    stati <- reqGetParams <$> getRequest
    
    quantities <- runDB $ select $ do
        x <- from $ table @Quantity
        orderBy [asc (x ^. QuantityName)]
        return x

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgQuantities
        idFabAdd <- newIdent
        $(widgetFile "data/units/quantities/quantities")


postUnitDeleR :: UnitId -> Handler Html
postUnitDeleR uid = do
    ((fr,_),_) <- runFormPost formDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete uid
          redirect $ DataR UnitsR
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ UnitR uid


formDelete :: Form ()
formDelete extra = return (FormSuccess (), [whamlet|#{extra}|])


postUnitR :: UnitId -> Handler Html
postUnitR uid = do
    unit <- runDB $ selectOne $ do
        x <- from $ table @Unit
        where_ $ x ^. UnitId ==. val uid
        return x
    ((fr,fw),et) <- runFormPost $ formUnit unit
    case fr of
      FormSuccess r -> do
          runDB $ replace uid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ UnitR uid
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgMeasurementUnit
          $(widgetFile "data/units/edit")    


getUnitEditR :: UnitId -> Handler Html
getUnitEditR uid = do
    unit <- runDB $ selectOne $ do
        x <- from $ table @Unit
        where_ $ x ^. UnitId ==. val uid
        return x
    (fw,et) <- generateFormPost $ formUnit unit
    defaultLayout $ do
        msgs <- getMessages
        setTitleI MsgMeasurementUnit
        $(widgetFile "data/units/edit")


postUnitsR :: Handler Html
postUnitsR = do
    ((fr,fw),et) <- runFormPost $ formUnit Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordCreated
          redirect $ DataR UnitsR
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgMeasurementUnit
          $(widgetFile "data/units/create")


getUnitAddR :: Handler Html
getUnitAddR = do
    (fw,et) <- generateFormPost $ formUnit Nothing
    defaultLayout $ do
        msgs <- getMessages
        setTitleI MsgMeasurementUnit
        $(widgetFile "data/units/create")


formUnit :: Maybe (Entity Unit) -> Form Unit
formUnit unit extra = do

    rndr <- getMessageRender
    
    (nameR,nameV) <- md3mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgName)]
        } (unitName . entityVal <$> unit)
        
    (symbolR,symbolV) <- md3mreq md3textField FieldSettings
        { fsLabel = SomeMessage MsgSymbol
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgSymbol)]
        } (unitSymbol . entityVal <$> unit)
        
    (descrR,descrV) <- md3mopt md3textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgDescription)]
        } (unitDescr . entityVal <$> unit)

    quantities <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Quantity
        orderBy [asc (x ^. QuantityName)]
        return (x ^. QuantityName, x ^. QuantityId) )

    (quantityR,quantityV) <- md3mopt (md3selectField (optionsPairs quantities)) FieldSettings
        { fsLabel = SomeMessage MsgQuantity
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgQuantity)]
        } (unitQuantity . entityVal <$> unit)

    let r = Unit <$> nameR <*> symbolR <*> descrR <*> quantityR
    let w = $(widgetFile "data/units/form")
    return (r,w)
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName md3textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Unit
              where_ $ x ^. UnitName ==. val name
              return x
          return $ case mx of
            Nothing -> Right name
            Just (Entity uid _) -> case unit of
              Nothing -> Left MsgAlreadyExists
              Just (Entity uid' _) | uid == uid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


getUnitR :: UnitId -> Handler Html
getUnitR uid = do
    unit <- runDB $ selectOne $ do
        x :& q <- from $ table @Unit
            `leftJoin` table @Quantity `on` (\(u :& q) -> u ^. UnitQuantity ==. q ?. QuantityId)
        where_ $ x ^. UnitId ==. val uid
        return (x,q)
    (fw,et) <- generateFormPost formDelete
    defaultLayout $ do
        msgs <- getMessages
        setTitleI MsgMeasurementUnit
        $(widgetFile "data/units/unit")


getUnitsR :: Handler Html
getUnitsR = do

    stati <- reqGetParams <$> getRequest
    let iquantities = filter ((== "quantity") . fst) stati
    let selected = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) iquantities
    
    quantities <- runDB $ select $ do
        x <- from $ table @Quantity
        orderBy [asc (x ^. QuantityName)]
        return x

    units <- runDB $ select $ do
        x <- from $ table @Unit
        unless (null selected) $ where_ $ x ^. UnitQuantity `in_` justList (valList selected)
        orderBy [asc (x ^. UnitName)]
        return x
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgMeasurementUnits
        idFabAdd <- newIdent
        $(widgetFile "data/units/units")
