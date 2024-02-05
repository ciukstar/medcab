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
  , getSignTagsR
  , getSignTagR
  , getSignTagAddR
  , getSignTagEditR
  , postSignTagDeleR
  , postSignTagsR
  , postSignTagR
  ) where


import Control.Monad (unless)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Maybe (mapMaybe)
import Data.Text (Text, pack, unpack)
import Database.Esqueleto.Experimental
    (select, selectOne, from, table, orderBy, asc, leftJoin, on, val, where_
    , (^.), (?.), (==.), (:&)((:&))
    , Value (unValue), in_, valList, justList
    )
import Database.Persist
    ( Entity (Entity), PersistStoreWrite (replace, delete, insert_), entityVal )
import Database.Persist.Sql (fromSqlKey, toSqlKey)

import Handler.Menu (menu)

import Material3
    (md3mreq, md3textField, md3selectField, md3mopt, md3textareaField, tsep)
import Model
    ( AvatarColor (AvatarColorLight), statusError, statusSuccess
    , MedSign
      ( MedSign, medSignName, medSignCode, medSignUnit, medSignDescr, medSignTag
      )
    , EntityField
      ( MedSignName, MedSignId, MedSignUnit, UnitId, UnitName, UnitSymbol
      , SignTagName, SignTagId, MedSignTag, SignTagGroup
      )
    , MedSignId, Unit (Unit)
    , SignTagId, SignTag (SignTag, signTagName, signTagDescr, signTagGroup)
    )

import Foundation
    ( Handler, Form
    , Route (DataR, AuthR, AccountR, AccountPhotoR)
    , DataR
      ( MedSignsR, MedSignR, MedSignAddR, MedSignEditR, MedSignDeleR, SignTagsR
      , SignTagR, SignTagAddR, SignTagEditR, SignTagDeleR
      )
    , AppMessage
      ( MsgMedicalSigns, MsgNoDataYet, MsgAdd, MsgSignIn, MsgSignOut
      , MsgUserAccount, MsgPhoto, MsgName, MsgDele, MsgDeleteAreYouSure
      , MsgCancel, MsgConfirmPlease, MsgEdit, MsgMedicalSign, MsgBack
      , MsgGroup, MsgDescription, MsgUnitOfMeasure, MsgCode, MsgInvalidFormData
      , MsgRecordDeleted, MsgSave, MsgRecordCreated, MsgRecordEdited, MsgTag
      , MsgAlreadyExists, MsgTags, MsgConfigure
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Read (readMaybe)

import Yesod.Auth (Route (LoginR, LogoutR), maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), addMessageI, redirect, SomeMessage (SomeMessage)
    , getMessageRender, MonadHandler (liftHandler), getRequest
    , YesodRequest (reqGetParams)
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


postSignTagDeleR :: SignTagId -> Handler Html
postSignTagDeleR tid = do
    ((fr,_),_) <- runFormPost formDeleSignTag
    case fr of
      FormSuccess () -> do
          runDB $ delete tid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR SignTagsR
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ SignTagR tid


postSignTagR :: SignTagId -> Handler Html
postSignTagR tid = do

    tag <- runDB $ selectOne $ do
        x <- from $ table @SignTag
        where_ $ x ^. SignTagId ==. val tid
        return x

    ((fr,fw),et) <- runFormPost $ formSignTag tag
    case fr of
      FormSuccess r -> do
          runDB $ replace tid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ SignTagR tid
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgTag
          $(widgetFile "data/signs/tags/edit")


getSignTagEditR :: SignTagId -> Handler Html
getSignTagEditR tid = do

    tag <- runDB $ selectOne $ do
        x <- from $ table @SignTag
        where_ $ x ^. SignTagId ==. val tid
        return x

    (fw,et) <- generateFormPost $ formSignTag tag
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTag
        $(widgetFile "data/signs/tags/edit")



postSignTagsR :: Handler Html
postSignTagsR = do
    ((fr,fw),et) <- runFormPost $ formSignTag Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordCreated
          redirect $ DataR SignTagsR
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgTag
          $(widgetFile "data/signs/tags/create")


getSignTagAddR :: Handler Html
getSignTagAddR = do
    (fw,et) <- generateFormPost $ formSignTag Nothing
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTag
        $(widgetFile "data/signs/tags/create")


formSignTag :: Maybe (Entity SignTag) -> Form SignTag
formSignTag tag extra = do

    rndr <- getMessageRender

    (nameR,nameV) <- md3mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgName)]
        } (signTagName . entityVal <$> tag)

    (descrR,descrV) <- md3mopt md3textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgDescription)]
        } (signTagDescr . entityVal <$> tag)

    groups <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @SignTag
        orderBy [asc (x ^. SignTagName)]
        return (x ^. SignTagName, x ^. SignTagId) )

    (groupR,groupV) <- md3mopt (md3selectField (optionsPairs groups)) FieldSettings
        { fsLabel = SomeMessage MsgGroup
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgGroup)]
        } (signTagGroup . entityVal <$> tag)

    let r = SignTag <$> nameR <*> descrR <*> groupR
    let w = $(widgetFile "data/signs/tags/form")
    return (r,w)
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName md3textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @SignTag
              where_ $ x ^. SignTagName ==. val name
              return x
          return $ case mx of
            Nothing -> Right name
            Just (Entity tid _) -> case tag of
              Nothing -> Left MsgAlreadyExists
              Just (Entity tid' _) | tid == tid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


getSignTagR :: SignTagId -> Handler Html
getSignTagR tid = do

    tag <- runDB $ selectOne $ do
        x :& g <- from $ table @SignTag
            `leftJoin` table @SignTag `on` (\(x :& g) -> x ^. SignTagGroup ==. g ?. SignTagId)
        where_ $ x ^. SignTagId ==. val tid
        return (x,g)
        
    (fw,et) <- generateFormPost formDeleSignTag
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTags
        $(widgetFile "data/signs/tags/tag")


formDeleSignTag :: Form ()
formDeleSignTag extra = return (FormSuccess (), [whamlet|#{extra}|])


getSignTagsR :: Handler Html
getSignTagsR = do

    stati <- reqGetParams <$> getRequest

    tags <- runDB $ select $ from $ table @SignTag

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTags
        idFabAdd <- newIdent
        $(widgetFile "data/signs/tags/tags")


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

    (descrR,descrV) <- md3mopt md3textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgDescription)]
        } (medSignDescr . entityVal <$> sign)

    units <- liftHandler $ (bimap ((\(a, b) -> a <> tsep <> b) . bimap unValue unValue) unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Unit
        orderBy [asc (x ^. UnitName)]
        return ((x ^. UnitSymbol, x ^. UnitName),x ^. UnitId) )

    (unitR,unitV) <- md3mopt (md3selectField (optionsPairs units)) FieldSettings
        { fsLabel = SomeMessage MsgUnitOfMeasure
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgUnitOfMeasure)]
        } (medSignUnit . entityVal <$> sign)

    tags <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @SignTag
        orderBy [asc (x ^. SignTagName)]
        return (x ^. SignTagName,x ^. SignTagId) )

    (tagR,tagV) <- md3mopt (md3selectField (optionsPairs tags)) FieldSettings
        { fsLabel = SomeMessage MsgTag
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgTag)]
        } (medSignTag . entityVal <$> sign)

    let r = MedSign <$> nameR <*> codeR <*> descrR <*> unitR <*> tagR
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

    stati <- reqGetParams <$> getRequest
    
    sign <- runDB $ selectOne $ do
        x :& u :& t <- from $ table @MedSign
            `leftJoin` table @Unit `on` (\(x :& u) -> x ^. MedSignUnit ==. u ?. UnitId)
            `leftJoin` table @SignTag `on` (\(x :& _ :& t) -> x ^. MedSignTag ==. t ?. SignTagId)
        where_ $ x ^. MedSignId ==. val sid
        return (x,u,t)
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

    stati <- reqGetParams <$> getRequest
    let itags = filter ((== "tag") . fst) stati  
    let selected = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) itags

    tags <- runDB $ select $ do
        x <- from $ table @SignTag
        orderBy [asc (x ^. SignTagName)]
        return x

    signs <- runDB $ select $ do
        x <- from $ table @MedSign
        unless (null selected) $ where_ $ (x ^. MedSignTag) `in_` justList (valList selected)
        orderBy [asc (x ^. MedSignName)]
        return x

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgMedicalSigns
        idFabAdd <- newIdent
        $(widgetFile "data/signs/signs")
