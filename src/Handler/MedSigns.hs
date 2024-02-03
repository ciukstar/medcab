{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.MedSigns
  ( getMedSignsR
  , getMedSignR
  , getMedSignAddR
  , getMedSignEditR
  , postMedSignDeleR
  , postMedSignsR
  , postMedSignR
  ) where


import Data.Bifunctor (Bifunctor(bimap))
import Data.Text (Text)
import Database.Esqueleto.Experimental
    (select, selectOne, from, table, orderBy, asc, leftJoin, on, val, where_
    , (^.), (?.), (==.), (:&)((:&))
    , Value (unValue)
    )
import Database.Persist
    ( Entity (Entity), PersistStoreWrite (replace, delete, insert_), entityVal )

import Handler.Menu (menu)

import Material3
    (md3mreq, md3textField, md3selectField, md3mopt, md3textareaField, tsep)
import Model
    ( AvatarColor (AvatarColorLight)
    , MedSign
      ( MedSign, medSignName, medSignCode, medSignUnit, medSignGroup
      , medSignDescr
      )
    , EntityField
      ( MedSignName, MedSignId, MedSignUnit, UnitId, MedSignGroup, UnitName
      , UnitSymbol
      )
    , MedSignId, statusError, Unit (Unit), statusSuccess
    )

import Foundation
    ( Handler, Form
    , Route (DataR, AuthR, AccountR, AccountPhotoR)
    , DataR (MedSignsR, MedSignR, MedSignAddR, MedSignEditR, MedSignDeleR)
    , AppMessage
      ( MsgMedicalSigns, MsgNoDataYet, MsgAdd, MsgSignIn, MsgSignOut
      , MsgUserAccount, MsgPhoto, MsgName, MsgDele, MsgDeleteAreYouSure
      , MsgCancel, MsgConfirmPlease, MsgEdit, MsgMedicalSign, MsgBack
      , MsgGroup, MsgDescription, MsgUnitOfMeasure, MsgCode, MsgInvalidFormData
      , MsgRecordDeleted, MsgSave, MsgRecordCreated, MsgRecordEdited, MsgAlreadyExists
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Auth (Route (LoginR, LogoutR), maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), addMessageI, redirect, SomeMessage (SomeMessage)
    , getMessageRender, MonadHandler (liftHandler)
    )
import Yesod.Core.Handler (getMessages, newIdent)
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Persist.Core (YesodPersist(runDB))
import Yesod.Form.Fields (optionsPairs)
import Yesod.Form.Functions (generateFormPost, runFormPost, checkM)
import Yesod.Form.Types
    ( FormResult(FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput), Field
    )


postMedSignDeleR :: MedSignId -> Handler Html
postMedSignDeleR sid = do
    ((fr,_),_) <- runFormPost formDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete sid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR MedSignsR
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ MedSignR sid


postMedSignR :: MedSignId -> Handler Html
postMedSignR sid = do
    
    sign <- runDB $ selectOne $ do
        x <- from $ table @MedSign
        where_ $ x ^. MedSignId ==. val sid
        return x

    ((fr,fw),et) <- runFormPost $ formMedSign sign

    case fr of
      FormSuccess r -> do
          runDB $ replace sid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ MedSignR sid
      _otherwise -> defaultLayout $ do
              msgs <- getMessages
              setTitleI MsgMedicalSign
              $(widgetFile "data/signs/edit")


getMedSignEditR :: MedSignId -> Handler Html
getMedSignEditR sid = do
    
    sign <- runDB $ selectOne $ do
        x <- from $ table @MedSign
        where_ $ x ^. MedSignId ==. val sid
        return x

    (fw,et) <- generateFormPost $ formMedSign sign
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgMedicalSign
        $(widgetFile "data/signs/edit")


postMedSignsR :: Handler Html
postMedSignsR = do
    ((fr,fw),et) <- runFormPost $ formMedSign Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordCreated
          redirect $ DataR MedSignsR
      _otherwise -> defaultLayout $ do
          setTitleI MsgMedicalSign
          msgs <- getMessages
          $(widgetFile "data/signs/create")
    

getMedSignAddR :: Handler Html
getMedSignAddR = do
    (fw,et) <- generateFormPost $ formMedSign Nothing
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgMedicalSign
        $(widgetFile "data/signs/create")


formMedSign :: Maybe (Entity MedSign) -> Form MedSign
formMedSign sign extra = do
    
    rndr <- getMessageRender

    (nameR,nameV) <- md3mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgName)]
        } (medSignName . entityVal <$> sign)
        
    (codeR,codeV) <- md3mopt md3textField FieldSettings
        { fsLabel = SomeMessage MsgCode
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgCode)]
        } (medSignCode . entityVal <$> sign)

    units <- liftHandler $ (bimap ((\(a, b) -> a <> tsep <> b) . bimap unValue unValue) unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Unit
        orderBy [asc (x ^. UnitName)]
        return ((x ^. UnitSymbol, x ^. UnitName),x ^. UnitId) )
        
    (unitR,unitV) <- md3mopt (md3selectField (optionsPairs units)) FieldSettings
        { fsLabel = SomeMessage MsgUnitOfMeasure
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgUnitOfMeasure)]
        } (medSignUnit . entityVal <$> sign)
        
    (descrR,descrV) <- md3mopt md3textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgDescription)]
        } (medSignDescr . entityVal <$> sign)

    let r = MedSign <$> nameR <*> codeR <*> unitR <*> descrR <*> pure (medSignGroup . entityVal =<< sign)
    let w = $(widgetFile "data/signs/form")
    return (r,w)
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName md3textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @MedSign
              where_ $ x ^. MedSignName ==. val name
              return x
          return $ case mx of
            Nothing -> Right name
            Just (Entity sid _) -> case sign of
              Nothing -> Left MsgAlreadyExists
              Just (Entity sid' _) | sid == sid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


getMedSignR :: MedSignId -> Handler Html
getMedSignR sid = do
    sign <- runDB $ selectOne $ do
        x :& u :& s <- from $ table @MedSign
            `leftJoin` table @Unit `on` (\(x :& u) -> x ^. MedSignUnit ==. u ?. UnitId)
            `leftJoin` table @MedSign `on` (\(x :& _ :& s) -> x ^. MedSignGroup ==. s ?. MedSignId)
        where_ $ x ^. MedSignId ==. val sid
        return (x,u,s)
    (fw,et) <- generateFormPost formDelete
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgMedicalSign
        $(widgetFile "data/signs/sign")


formDelete :: Form ()
formDelete extra = return (FormSuccess (), [whamlet|#{extra}|])


getMedSignsR :: Handler Html
getMedSignsR = do

    user <- maybeAuth

    signs <- runDB $ select $ do
        x <- from $ table @MedSign
        orderBy [asc (x ^. MedSignName)]
        return x

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgMedicalSigns
        idFabAdd <- newIdent
        $(widgetFile "data/signs/signs")
