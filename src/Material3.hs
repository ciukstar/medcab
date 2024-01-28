{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Material3
  ( md3emailField
  , md3passwordField
  , md3radioField
  , md3telField
  , md3textField
  , md3textareaField
  , md3selectField
  , md3switchField
  , md3htmlField
  ) where

import Data.Text (Text, pack)

import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Hamlet (Html)
import Text.Shakespeare.I18N (RenderMessage)

import Yesod.Core (MonadHandler(HandlerSite))
import Yesod.Core.Handler (HandlerFor)
import Yesod.Core.Widget (whamlet, handlerToWidget)
import Yesod.Form.Fields
    ( emailField, passwordField, textField, OptionList (olOptions), radioField
    , Option (optionExternalValue, optionDisplay, optionInternalValue)
    , textareaField, Textarea (Textarea), selectField, checkBoxField, htmlField
    , FormMessage
    )
import Yesod.Form.Types (Field (fieldView))


md3switchField :: Monad m => Field m Bool
md3switchField = checkBoxField
    { fieldView = \theId name attrs x _ -> [whamlet|
<md-switch ##{theId} *{attrs} name=#{name} :showVal id x:value=yes :showVal id x:selected>
|] }
    where
      showVal = either (const False)


md3selectField :: (Eq a, RenderMessage m FormMessage) => HandlerFor m (OptionList a) -> Field (HandlerFor m) a
md3selectField options = (selectField options)
    { fieldView = \theId name attrs x isReq -> do
          opts <- olOptions <$> handlerToWidget options
          let sel (Left _) _ = False
              sel (Right y) opt = optionInternalValue opt == y
          [whamlet|
<md-filled-select ##{theId} *{attrs} :isReq:required name=#{name}>
  $forall opt<- opts
    <md-select-option value=#{optionExternalValue opt} :sel x opt:selected>
      <div slot="headline">#{optionDisplay opt}
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
md3telField = textField { fieldView = \theId name attrs eval isReq -> [whamlet|
<md-filled-text-field ##{theId} type=tel name=#{name} :isReq:required=true value=#{either id id eval} *{attrs}>
|] }


md3passwordField :: RenderMessage m FormMessage => Field (HandlerFor m) Text
md3passwordField = passwordField { fieldView = \theId name attrs eval isReq -> [whamlet|
<md-filled-text-field ##{theId} type=password name=#{name} :isReq:required=true value=#{either id id eval} *{attrs}>
|] }


md3emailField :: RenderMessage m FormMessage => Field (HandlerFor m) Text
md3emailField = emailField { fieldView = \theId name attrs eval isReq -> [whamlet|
<md-filled-text-field ##{theId} type=email name=#{name} :isReq:required=true value=#{either id id eval} *{attrs}>
|] }


md3textField :: RenderMessage m FormMessage => Field (HandlerFor m) Text
md3textField = textField { fieldView = \theId name attrs ex req -> [whamlet|
<md-filled-text-field ##{theId} type=text name=#{name} :req:required value=#{either id id ex} *{attrs}>
|] }


md3textareaField :: RenderMessage m FormMessage => Field (HandlerFor m) Textarea
md3textareaField = textareaField { fieldView = \theId name attrs eval isReq -> [whamlet|
<md-filled-text-field ##{theId} type=textarea name=#{name} :isReq:required=true value=#{either Textarea id eval} *{attrs}>
|] }


md3htmlField ::  Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Html
md3htmlField = htmlField { fieldView = \theId name attrs x isReq -> [whamlet|
<md-filled-text-field ##{theId} type=textarea name=#{name} :isReq:required=true value=#{showVal x} *{attrs}>
|] }
    where
      showVal = either id (pack . renderHtml)
