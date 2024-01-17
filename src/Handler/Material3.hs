{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Material3
  ( md3emailField
  , md3passwordField
  , md3radioField
  , md3telField
  , md3textField
  ) where

import Data.Text (Text)
import Yesod.Core.Widget (whamlet, handlerToWidget)
import Yesod.Form.Fields
    ( emailField, passwordField, textField, OptionList (olOptions)
    , radioField, Option (optionExternalValue, optionDisplay, optionInternalValue)
    )
import Yesod.Form.Types (Field (fieldView), FormMessage)
import Yesod.Core.Handler (HandlerFor)
import Text.Shakespeare.I18N (RenderMessage)


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


md3textField :: RenderMessage m FormMessage => Field (HandlerFor m) Text
md3textField = textField { fieldView = \theId name attrs eval isReq -> [whamlet|
<md-filled-text-field ##{theId} type=text name=#{name} :isReq:required=true value=#{either id id eval} *{attrs}>
|] }


md3passwordField :: RenderMessage m FormMessage => Field (HandlerFor m) Text
md3passwordField = passwordField { fieldView = \theId name attrs eval isReq -> [whamlet|
<md-filled-text-field ##{theId} type=password name=#{name} :isReq:required=true value=#{either id id eval} *{attrs}>
|] }


md3emailField :: RenderMessage m FormMessage => Field (HandlerFor m) Text
md3emailField = emailField { fieldView = \theId name attrs eval isReq -> [whamlet|
<md-filled-text-field ##{theId} type=email name=#{name} :isReq:required=true value=#{either id id eval} *{attrs}>
|] }
