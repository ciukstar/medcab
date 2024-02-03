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
  ) where

import Data.Text (Text)
import Database.Esqueleto.Experimental
    ( select, from, table, orderBy
    , (^.), (==.)
    , asc, selectOne, where_, val
    )
import Database.Persist
    ( Entity (Entity), entityVal, PersistStoreWrite (insert_, delete, replace)
    )

import Foundation
    ( Handler, Form
    , Route (DataR, AuthR, AccountR, AccountPhotoR)
    , DataR (UnitsR, UnitR, UnitAddR, UnitEditR, UnitDeleR)
    , AppMessage
      ( MsgMeasurementUnits, MsgAdd, MsgSignIn, MsgSignOut, MsgPhoto, MsgName
      , MsgUserAccount, MsgUnitsOfMeasure, MsgNoDataYet, MsgMeasurementUnit
      , MsgDescription, MsgSymbol, MsgBack, MsgCancel, MsgSave, MsgRecordCreated
      , MsgInvalidFormData, MsgRecordEdited, MsgConfirmPlease, MsgDescription
      , MsgDeleteAreYouSure, MsgEdit, MsgDele, MsgAlreadyExists
      )
    )

import Handler.Menu (menu)

import Material3 (md3mreq, md3textField, md3textareaField, md3mopt)
import Model
    ( AvatarColor(AvatarColorLight), Unit(Unit, unitName, unitSymbol, unitDescr)
    , statusError, EntityField (UnitName, UnitId), UnitId, statusSuccess
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessages, redirect
    , SomeMessage (SomeMessage), getMessageRender, addMessageI, whamlet
    )
import Yesod.Form.Functions (generateFormPost, runFormPost, checkM)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput), FormResult (FormSuccess), Field
    )
import Yesod.Persist.Core (YesodPersist(runDB))


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

    let r = Unit <$> nameR <*> symbolR <*> descrR
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
        x <- from $ table @Unit
        where_ $ x ^. UnitId ==. val uid
        return x
    (fw,et) <- generateFormPost formDelete
    defaultLayout $ do
        msgs <- getMessages
        setTitleI MsgMeasurementUnit
        $(widgetFile "data/units/unit")


getUnitsR :: Handler Html
getUnitsR = do
    user <- maybeAuth

    units <- runDB $ select $ do
        x <- from $ table @Unit
        orderBy [asc (x ^. UnitName)]
        return x
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgMeasurementUnits
        idFabAdd <- newIdent
        $(widgetFile "data/units/units")
