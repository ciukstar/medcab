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
  , getMedSignNormalsR
  , getMedSignNormalR
  , getMedSignNormalAddR
  , postMedSignNormalsR
  , getMedSignNormalEditR
  , postMedSignNormalDeleR
  , postMedSignNormalR
  ) where


import Control.Applicative ((<|>))
import Control.Monad (unless, when)

import Data.Bifunctor (Bifunctor(bimap))
import qualified Data.List.Safe as LS (last)
import Data.Maybe (mapMaybe)
import Data.Text (Text, pack, unpack)

import Database.Esqueleto.Experimental
    (select, selectOne, from, table, orderBy, asc, leftJoin, on, val, where_
    , (^.), (?.), (==.), (:&)((:&)), (+.)
    , Value (unValue, Value), in_, valList, justList, withRecursive, unionAll_
    , just, isNothing_, innerJoin, subSelectList, not_
    )
import Database.Persist
    ( Entity (Entity), PersistStoreWrite (replace, delete, insert_)
    , entityVal
    )
import Database.Persist.Sql (fromSqlKey, toSqlKey)

import Material3
    ( md3mreq, md3textField, md3selectField, md3mopt, md3textareaField, tsep
    , md3doubleField
    )
import Menu (menu)
import Model
    ( AvatarColor (AvatarColorLight), statusError, statusSuccess
    , MedSign
      ( MedSign, medSignName, medSignCode, medSignDescr, medSignTag, medSignIcon
      )
    , EntityField
      ( MedSignName, MedSignId, UnitId, UnitName, UnitSymbol, SignTagName
      , SignTagId, MedSignTag, SignTagGroup, NormalSign, NormalName
      , NormalUnit, NormalId
      )
    , MedSignId, Unit (Unit)
    , SignTagId, SignTag (SignTag, signTagName, signTagDescr, signTagGroup)
    , SignTags (SignTags), NormalId
    , Normal (Normal, normalName, normalLowerBound, normalUnit, normalUpperBound)
    )

import Foundation
    ( Handler, Form
    , Route (DataR, AuthR, AccountR, AccountPhotoR)
    , DataR
      ( MedSignsR, MedSignR, MedSignAddR, MedSignEditR, MedSignDeleR, SignTagsR
      , SignTagR, SignTagAddR, SignTagEditR, SignTagDeleR, MedSignNormalsR
      , MedSignNormalR, MedSignNormalAddR, MedSignNormalEditR
      , MedSignNormalDeleR
      )
    , AppMessage
      ( MsgMedicalSigns, MsgNoDataYet, MsgAdd, MsgSignIn, MsgSignOut, MsgSubtags
      , MsgUserAccount, MsgPhoto, MsgName, MsgDele, MsgDeleteAreYouSure
      , MsgCancel, MsgConfirmPlease, MsgEdit, MsgMedicalSign, MsgBack, MsgTabs
      , MsgGroup, MsgDescription, MsgUnitOfMeasure, MsgCode, MsgInvalidFormData
      , MsgRecordDeleted, MsgSave, MsgRecordCreated, MsgRecordEdited, MsgTag
      , MsgAlreadyExists, MsgTags, MsgConfigure, MsgDetails, MsgNone, MsgIcon
      , MsgNormalValues, MsgNormalValue, MsgLowerBound, MsgUpperBound
      , MsgInvalidRangeBoundaries
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
import Yesod.Form.Functions (generateFormPost, runFormPost, checkM, check)
import Yesod.Form.Types
    ( FormResult(FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput), Field
    )


postMedSignNormalDeleR :: MedSignId -> NormalId -> Handler Html
postMedSignNormalDeleR sid nid = do
    ((fr,_),_) <- runFormPost formMedSignNormalDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete nid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ MedSignNormalsR sid
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ MedSignNormalR sid nid


formMedSignNormalDelete :: Form ()
formMedSignNormalDelete extra = return (FormSuccess (), [whamlet|#{extra}|])


postMedSignNormalR :: MedSignId -> NormalId -> Handler Html
postMedSignNormalR sid nid = do

    normal <- runDB $ selectOne $ do
        x <- from $ table @Normal
        where_ $ x ^. NormalId ==. val nid
        return x

    ((fr,fw),et) <- runFormPost $ formMedSignNormal sid normal
    case fr of
      FormSuccess r -> do
          runDB $ replace nid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ MedSignNormalR sid nid
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgNormalValue
          $(widgetFile "data/signs/normals/edit")


getMedSignNormalEditR :: MedSignId -> NormalId -> Handler Html
getMedSignNormalEditR sid nid = do

    normal <- runDB $ selectOne $ do
        x <- from $ table @Normal
        where_ $ x ^. NormalId ==. val nid
        return x

    (fw,et) <- generateFormPost $ formMedSignNormal sid normal
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgNormalValue
        $(widgetFile "data/signs/normals/edit")


postMedSignNormalsR :: MedSignId -> Handler Html
postMedSignNormalsR sid = do
    ((fr,fw),et) <- runFormPost $ formMedSignNormal sid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordCreated
          redirect $ DataR $ MedSignNormalsR sid
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgNormalValue
          $(widgetFile "data/signs/normals/create")



getMedSignNormalAddR :: MedSignId -> Handler Html
getMedSignNormalAddR sid = do
    (fw,et) <- generateFormPost $ formMedSignNormal sid Nothing
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgNormalValue
        $(widgetFile "data/signs/normals/create")


formMedSignNormal :: MedSignId -> Maybe (Entity Normal) -> Form Normal
formMedSignNormal sid normal extra = do

    rndr <- getMessageRender

    (nameR,nameV) <- md3mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgName)]
        } (normalName . entityVal <$> normal)

    (lowerR,lowerV) <- md3mreq md3doubleField FieldSettings
        { fsLabel = SomeMessage MsgLowerBound
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgLowerBound)]
        } (normalLowerBound . entityVal <$> normal)

    (upperR,upperV) <- md3mreq (gteField lowerR) FieldSettings
        { fsLabel = SomeMessage MsgUpperBound
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgUpperBound)]
        } (normalUpperBound . entityVal <$> normal)

    units <- liftHandler $ (unval <$>) <$> runDB ( select $ do
        x <- from $ table @Unit
        orderBy [asc (x ^. UnitName)]
        return ((x ^. UnitSymbol, x ^. UnitName),x ^. UnitId) )

    (unitR,unitV) <- md3mopt (md3selectField (optionsPairs units)) FieldSettings
        { fsLabel = SomeMessage MsgUnitOfMeasure
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgUnitOfMeasure)]
        } (normalUnit . entityVal <$> normal)

    let r = Normal sid <$> nameR <*> lowerR <*> upperR <*> unitR
    let w = $(widgetFile "data/signs/normals/form")
    return (r,w)
  where

      gteField :: FormResult Double -> Field Handler Double
      gteField lowerR = check (gte lowerR) md3doubleField

      gte :: FormResult Double -> Double -> Either AppMessage Double
      gte (FormSuccess x) y | y < x = Left MsgInvalidRangeBoundaries
      gte _ y = Right y
      
      unval = bimap ((\(a, b) -> a <> tsep <> b) . bimap unValue unValue) unValue
      
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName md3textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Normal
              where_ $ x ^. NormalSign ==. val sid
              where_ $ x ^. NormalName ==. val name
              return x
          return $ case mx of
            Nothing -> Right name
            Just (Entity nid _) -> case normal of
              Nothing -> Left MsgAlreadyExists
              Just (Entity nid' _) | nid == nid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


getMedSignNormalR :: MedSignId -> NormalId -> Handler Html
getMedSignNormalR sid nid = do
    stati <- reqGetParams <$> getRequest

    normal <- runDB $ selectOne $ do
        x :& u <- from $ table @Normal
            `leftJoin` table @Unit `on` (\(x :& u) -> x ^. NormalUnit ==. u ?. UnitId)
        where_ $ x ^. NormalId ==. val nid
        return (x,u)

    (fw,et) <- generateFormPost formMedSignNormalDelete
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgNormalValue
        $(widgetFile "data/signs/normals/normal")


getMedSignNormalsR :: MedSignId -> Handler Html
getMedSignNormalsR sid = do

    stati <- reqGetParams <$> getRequest

    normals <- runDB $ select $ do
        x :& u <- from $ table @Normal
            `leftJoin` table @Unit `on` (\(x :& u) -> x ^. NormalUnit ==. u ?. UnitId)
        where_ $ x ^. NormalSign ==. val sid
        orderBy [asc (x ^. NormalId)]
        return (x,u)

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgNormalValues
        idPanelNormals <- newIdent
        idFabAdd <- newIdent
        $(widgetFile "data/signs/normals/normals")


postSignTagDeleR :: SignTagId -> SignTags -> Handler Html
postSignTagDeleR tid ps = do
    ((fr,_),_) <- runFormPost formDeleSignTag
    case fr of
      FormSuccess () -> do
          runDB $ delete tid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ SignTagsR ps
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ SignTagR tid ps


postSignTagR :: SignTagId -> SignTags -> Handler Html
postSignTagR tid ps = do

    tag <- runDB $ selectOne $ do
        x <- from $ table @SignTag
        where_ $ x ^. SignTagId ==. val tid
        return x

    ((fr,fw),et) <- runFormPost $ formSignTag Nothing tag
    case fr of
      FormSuccess r -> do
          runDB $ replace tid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ SignTagR tid ps
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgTag
          $(widgetFile "data/signs/tags/edit")


getSignTagEditR :: SignTagId -> SignTags -> Handler Html
getSignTagEditR tid ps = do

    tag <- runDB $ selectOne $ do
        x <- from $ table @SignTag
        where_ $ x ^. SignTagId ==. val tid
        return x

    (fw,et) <- generateFormPost $ formSignTag Nothing tag
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTag
        $(widgetFile "data/signs/tags/edit")



postSignTagsR :: SignTags -> Handler Html
postSignTagsR ps = do
    ((fr,fw),et) <- runFormPost $ formSignTag Nothing Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordCreated
          redirect $ DataR $ SignTagsR ps
      _otherwise -> defaultLayout $ do
          msgs <- getMessages
          setTitleI MsgTag
          $(widgetFile "data/signs/tags/create")


getSignTagAddR :: SignTags -> Handler Html
getSignTagAddR ps@(SignTags tids) = do

    (fw,et) <- generateFormPost $ formSignTag (LS.last tids) Nothing
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTag
        $(widgetFile "data/signs/tags/create")


formSignTag :: Maybe SignTagId -> Maybe (Entity SignTag) -> Form SignTag
formSignTag gid tag extra = do

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
        } ((signTagGroup . entityVal <$> tag) <|> pure gid)

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


getSignTagR :: SignTagId -> SignTags -> Handler Html
getSignTagR tid ps@(SignTags tids) = do

    tag <- runDB $ selectOne $ do
        x :& g <- from $ table @SignTag
            `leftJoin` table @SignTag `on` (\(x :& g) -> x ^. SignTagGroup ==. g ?. SignTagId)
        where_ $ x ^. SignTagId ==. val tid
        return (x,g)

    (fw,et) <- generateFormPost formDeleSignTag
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTags
        idPanelDetails <- newIdent
        $(widgetFile "data/signs/tags/tag")


formDeleSignTag :: Form ()
formDeleSignTag extra = return (FormSuccess (), [whamlet|#{extra}|])


getSignTagsR :: SignTags -> Handler Html
getSignTagsR ps@(SignTags tids) = do

    stati <- reqGetParams <$> getRequest

    tags <- runDB $ select $ do
        cte <- withRecursive
            (do
                  x <- from $ table @SignTag

                  unless (null tids) $ where_ $ x ^. SignTagGroup ==. just (val (last tids))
                  when (null tids) $ where_ $ isNothing_ $ x ^. SignTagGroup

                  let level = val (length tids)
                  return (level,x)
            )
            unionAll_
            (\parent -> do
                  (l,_) :& x <- from $ parent
                      `innerJoin` table @SignTag `on` (\((_, p) :& x) -> just (p ^. SignTagId) ==. x ^. SignTagGroup)
                  let level = l +. val 1
                  -- orderBy [desc level]
                  return (level,x)
            )
        from cte

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTags
        idTabs <- newIdent
        idPanelSubtags <- newIdent
        idFabAdd <- newIdent
        when (null tids) $ do
            $(widgetFile "data/signs/tags/tags")
        unless (null tids) $ do
            $(widgetFile "data/signs/tags/subtags")


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

    (iconR,iconV) <- md3mopt md3textField FieldSettings
        { fsLabel = SomeMessage MsgIcon
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgIcon)]
        } (medSignIcon . entityVal <$> sign)

    (descrR,descrV) <- md3mopt md3textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgDescription)]
        } (medSignDescr . entityVal <$> sign)

    tags <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @SignTag
        orderBy [asc (x ^. SignTagName)]
        return (x ^. SignTagName,x ^. SignTagId) )

    (tagR,tagV) <- md3mopt (md3selectField (optionsPairs tags)) FieldSettings
        { fsLabel = SomeMessage MsgTag
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgTag)]
        } (medSignTag . entityVal <$> sign)

    let r = MedSign <$> nameR <*> codeR <*> iconR <*> descrR <*> tagR
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
        x :& t <- from $ table @MedSign
            `leftJoin` table @SignTag `on` (\(x :& t) -> x ^. MedSignTag ==. t ?. SignTagId)
        where_ $ x ^. MedSignId ==. val sid
        return (x,t)
    (fw,et) <- generateFormPost formDelete
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgMedicalSign
        idPanelDetails <- newIdent
        $(widgetFile "data/signs/sign")


formDelete :: Form ()
formDelete extra = return (FormSuccess (), [whamlet|#{extra}|])


getMedSignsR :: Handler Html
getMedSignsR = do

    user <- maybeAuth

    stati <- reqGetParams <$> getRequest
    let itags = filter ((== "tag") . fst) stati
    let selected = mapMaybe ((toSqlKey <$>) . readMaybe . unpack . snd) itags

    tags0 <- runDB $ select $ do
        x <- from $ table @SignTag
        where_ $ isNothing_ $ x ^. SignTagGroup
        orderBy [asc (x ^. SignTagName)]
        return x

    tags1 <- runDB $ select $ do
        x <- from $ table @SignTag
        where_ $ not_ $ isNothing_ $ x ^. SignTagGroup
        unless (null selected) $ where_ $ x ^. SignTagGroup `in_` justList (valList selected)
        when (null selected) $ where_ (val False)
        orderBy [asc (x ^. SignTagName)]
        return x

    parents <- mapMaybe unValue <$> runDB ( select $ do
        x <- from $ table @SignTag
        where_ $ not_ $ isNothing_ $ x ^. SignTagGroup
        where_ $ (x ^. SignTagId) `in_` valList selected
        where_ $ x ^. SignTagGroup `in_` justList ( valList selected )
        return $ x ^. SignTagGroup )



    signs <- runDB $ select $ do
        x <- from $ table @MedSign
        unless (null selected) $ where_ $ (x ^. MedSignTag) `in_` justList ( subSelectList
            ( do
                  cte <- withRecursive
                      ( do
                            p <- from $ table @SignTag
                            where_ $ p ^. SignTagId `in_` valList (filter (`notElem` parents) selected)
                            return p
                      )
                      unionAll_
                      (\parent -> do
                        (c :& _) <- from $ table @SignTag `innerJoin` parent
                            `on` (\(c :& p) -> c ^. SignTagGroup ==. just (p ^. SignTagId))
                        return c
                      )
                  ( ^. SignTagId) <$> from cte
            ) )
        orderBy [asc (x ^. MedSignName)]
        return x

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgMedicalSigns
        idFabAdd <- newIdent
        $(widgetFile "data/signs/signs")
