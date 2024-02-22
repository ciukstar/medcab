{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Material3
  ( md3mopt
  , md3mreq
  , md3emailField
  , md3passwordField
  , md3radioField
  , md3telField
  , md3textField
  , md3textareaField
  , md3selectField
  , md3switchField
  , md3htmlField
  , md3doubleField
  , md3dayField
  , md3timeField
  , md3datetimeLocalField
  , md3checkboxesField
  , md3checkboxesFieldList
  , tsep
  ) where


import qualified Data.List.Safe as LS (head, tail)
import Data.Text (Text, pack, splitOn)
import Data.Text.Lazy (toStrict)

import qualified Text.Blaze.Html.Renderer.String as S (renderHtml)
import qualified Text.Blaze.Html.Renderer.Text as T (renderHtml)
import Text.Hamlet (Html)
import Text.Shakespeare.I18N (RenderMessage)

import Yesod.Core (MonadHandler(HandlerSite), newIdent)
import Yesod.Core.Handler (HandlerFor)
import Yesod.Core.Widget (whamlet, handlerToWidget)
import Yesod.Form.Fields
    ( emailField, passwordField, textField, OptionList (olOptions), radioField
    , Option (optionExternalValue, optionDisplay, optionInternalValue)
    , textareaField, Textarea (Textarea), selectField, checkBoxField, htmlField
    , FormMessage, doubleField, dayField, timeField, datetimeLocalField
    , optionsPairs, multiSelectField
    )
import Yesod.Form.Functions (mopt, mreq)
import Yesod.Form.Types
    ( Field (fieldView)
    , FieldSettings (fsId, fsName, fsAttrs)
    , FieldView (fvErrors), MForm, FormResult
    )
import Data.Time.Calendar (Day)
import Data.Time (TimeOfDay, LocalTime)
import Data.Time.Format.ISO8601 (iso8601Show)


md3checkboxesFieldList :: (Eq a, RenderMessage m msg) => [(msg,a)] -> Field (HandlerFor m) [a]
md3checkboxesFieldList = md3checkboxesField . optionsPairs


md3checkboxesField :: Eq a => HandlerFor m (OptionList a) -> Field (HandlerFor m) [a]
md3checkboxesField ioptlist = (multiSelectField ioptlist)
    { fieldView = \theId name attrs val _idReq -> do
          opts <- olOptions <$> handlerToWidget ioptlist
          let optselected (Left _) _ = False
              optselected (Right vals) opt = optionInternalValue opt `elem` vals
          [whamlet|
            <span ##{theId}>
              $forall opt <- opts
                <label>
                  <md-checkbox touch-target=wrapper
                    name=#{name} value=#{optionExternalValue opt} *{attrs} :optselected val opt:checked>
                  #{optionDisplay opt}
          |]
    }


md3switchField :: Monad m => Field m Bool
md3switchField = checkBoxField
    { fieldView = \theId name attrs x _ -> [whamlet|
<md-switch ##{theId} *{attrs} name=#{name} :showVal id x:value=yes :showVal id x:selected>
|] }
    where
      showVal = either (const False)


md3selectField :: (Eq a, RenderMessage m FormMessage) => HandlerFor m (OptionList a) -> Field (HandlerFor m) a
md3selectField options = (selectField options)
    { fieldView = \theId name attrs x req -> do
          opts <- olOptions <$> handlerToWidget options
          let sel (Left _) _ = False
              sel (Right y) opt = optionInternalValue opt == y
          [whamlet|
<md-filled-select ##{theId} *{attrs} :req:required name=#{name}>
  $forall opt <- opts
    $with parts <- splitOn tsep $ optionDisplay opt
      <md-select-option value=#{optionExternalValue opt} :sel x opt:selected>
        $maybe h <- LS.head parts
          <div slot=headline>#{h}
        $maybe ts <- LS.tail parts
          $forall t <- ts
            <div slot=supporting-text>#{t}
  $if elem "error" (fst <$> attrs)
    <md-icon slot=trailing-icon>error
|] }


md3radioField :: (RenderMessage m FormMessage, Eq a) => HandlerFor m (OptionList a) -> Field (HandlerFor m) a
md3radioField options = (radioField options)
    { fieldView = \theId name attrs x isReq -> do
          opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget options
          let sel (Left _) _ = False
              sel (Right y) opt = optionInternalValue opt == y
          [whamlet|
<div ##{theId} *{attrs}>
  $forall (i,opt) <- opts
    <div>
      <md-radio ##{theId}-#{i} name=#{name} :isReq:required=true value=#{optionExternalValue opt} :sel x opt:checked>
      <label.label-large for=#{theId}-#{i}>#{optionDisplay opt}
|] }


md3telField :: RenderMessage m FormMessage => Field (HandlerFor m) Text
md3telField = textField { fieldView = \theId name attrs x req -> [whamlet|
<md-filled-text-field ##{theId} type=tel name=#{name} :req:required value=#{either id id x} *{attrs}>
  $if elem "error" (fst <$> attrs)
    <md-icon slot=trailing-icon>error
|] }


md3passwordField :: RenderMessage m FormMessage => Field (HandlerFor m) Text
md3passwordField = passwordField { fieldView = \theId name attrs x req -> [whamlet|
<md-filled-text-field ##{theId} type=password name=#{name} :req:required value=#{either id id x} *{attrs}>
  $if elem "error" (fst <$> attrs)
    <md-icon slot=trailing-icon>error
|] }


md3emailField :: RenderMessage m FormMessage => Field (HandlerFor m) Text
md3emailField = emailField { fieldView = \theId name attrs x req -> [whamlet|
<md-filled-text-field ##{theId} type=email name=#{name} :req:required value=#{either id id x} *{attrs}>
  $if elem "error" (fst <$> attrs)
    <md-icon slot=trailing-icon>error
|] }


md3textField :: RenderMessage m FormMessage => Field (HandlerFor m) Text
md3textField = textField { fieldView = \theId name attrs ex req -> [whamlet|
<md-filled-text-field ##{theId} type=text name=#{name} :req:required value=#{either id id ex} *{attrs}>
  $if elem "error" (fst <$> attrs)
    <md-icon slot=trailing-icon>error
|] }


md3dayField :: RenderMessage m FormMessage => Field (HandlerFor m) Day
md3dayField = dayField { fieldView = \theId name attrs ex req -> [whamlet|
<md-filled-text-field ##{theId} type=date name=#{name} :req:required value=#{either id (pack . show) ex} *{attrs}>
  <span slot=trailing-icon style="display:flex;flex-direction:row;align-items:center">
    $if elem "error" (fst <$> attrs)
      <md-icon>error
    <md-icon-button type=button
      onclick="document.getElementById('#{theId}').shadowRoot.querySelector('input').showPicker()">
      <md-icon>today
|] }


md3timeField :: RenderMessage m FormMessage => Field (HandlerFor m) TimeOfDay
md3timeField = timeField { fieldView = \theId name attrs ex req -> [whamlet|
<md-filled-text-field ##{theId} type=time name=#{name} :req:required value=#{either id (pack . show) ex} *{attrs}>
  <span slot=trailing-icon style="display:flex;flex-direction:row;align-items:center">
    $if elem "error" (fst <$> attrs)
      <md-icon>error
    <md-icon-button type=button slot=trailing-icon
      onclick="document.getElementById('#{theId}').shadowRoot.querySelector('input').showPicker()">
        <md-icon>schedule
|] }


md3datetimeLocalField :: RenderMessage m FormMessage => Field (HandlerFor m) LocalTime
md3datetimeLocalField = datetimeLocalField { fieldView = \theId name attrs ex req -> [whamlet|
<md-filled-text-field ##{theId} type=datetime-local name=#{name} :req:required value=#{either id showVal ex} *{attrs}>
  <span slot=trailing-icon style="display:flex;flex-direction:row;align-items:center">
    $if elem "error" (fst <$> attrs)
      <md-icon>error
    <md-icon-button type=button slot=trailing-icon
      onclick="document.getElementById('#{theId}').shadowRoot.querySelector('input').showPicker()">
        <md-icon>schedule
|] }
  where
    showVal = pack . iso8601Show


md3textareaField :: RenderMessage m FormMessage => Field (HandlerFor m) Textarea
md3textareaField = textareaField { fieldView = \theId name attrs x req -> [whamlet|
<md-filled-text-field ##{theId} type=textarea name=#{name} :req:required value=#{either Textarea id x} *{attrs}>
  $if elem "error" (fst <$> attrs)
    <md-icon slot=trailing-icon>error
|] }


md3htmlField ::  Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Html
md3htmlField = htmlField { fieldView = \theId name attrs x req -> [whamlet|
<md-filled-text-field ##{theId} type=textarea name=#{name} :req:required value=#{showVal x} *{attrs}>
  $if elem "error" (fst <$> attrs)
    <md-icon slot=trailing-icon>error
|] }
    where
      showVal = either id (pack . S.renderHtml)


md3doubleField :: RenderMessage m FormMessage => Field (HandlerFor m) Double
md3doubleField = doubleField { fieldView = \theId name attrs ex req -> [whamlet|
<md-filled-text-field ##{theId} type=number name=#{name} :req:required value=#{either id (pack . show) ex} *{attrs}>
  $if elem "error" (fst <$> attrs)
    <md-icon slot=trailing-icon>error
|] }


md3mopt :: (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m)
        => Field m a           -- ^ form field
        -> FieldSettings site  -- ^ settings for this field
        -> Maybe (Maybe a)     -- ^ optional default value
        -> MForm m (FormResult (Maybe a), FieldView site)
md3mopt field fs mdef = do
    identName <- newIdent
    identId <- newIdent
    let nameFs = fs { fsId = Just identId, fsName = Just identName}
    (r,v) <- mopt field nameFs Nothing

    let attributes = case fvErrors v of
          Nothing -> []
          Just errs -> [("error",""),("error-text",toStrict $ T.renderHtml errs)]

    (_,v') <- mopt field (nameFs { fsAttrs = fsAttrs nameFs <> attributes }) mdef

    return (r,v')


md3mreq :: (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m)
        => Field m a           -- ^ form field
        -> FieldSettings site  -- ^ settings for this field
        -> Maybe a             -- ^ optional default value
        -> MForm m (FormResult a, FieldView site)
md3mreq field fs mdef = do
    identName <- newIdent
    identId <- newIdent
    let nameFs = fs { fsId = Just identId, fsName = Just identName}
    (r,v) <- mreq field nameFs Nothing

    let attributes = case fvErrors v of
          Nothing -> []
          Just errs -> [("error",""),("error-text",toStrict $ T.renderHtml errs)]

    (_,v') <- mreq field (nameFs { fsAttrs = fsAttrs nameFs <> attributes }) mdef

    return (r,v')


tsep :: Text
tsep = "<>"
