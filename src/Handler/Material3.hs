{-# LANGUAGE QuasiQuotes #-}

module Handler.Material3
  ( m3emailField
  , m3passwordField
  , m3textField
  , m3radioField
  ) where

import Data.Text (Text)
import Foundation (Handler)
import Yesod.Core.Widget (whamlet, handlerToWidget)
import Yesod.Form.Fields
    ( emailField, passwordField, textField, OptionList (olOptions)
    , radioField, Option (optionExternalValue, optionDisplay, optionInternalValue)
    )
import Yesod.Form.Types (Field (fieldView))


m3radioField :: Eq a => Handler (OptionList a) -> Field Handler a
m3radioField options = (radioField options)
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


m3textField :: Field Handler Text
m3textField = textField { fieldView = \theId name attrs eval isReq -> [whamlet|
<md-filled-text-field ##{theId} type=text name=#{name} :isReq:required=true value=#{either id id eval} *{attrs}>
|] }


m3passwordField :: Field Handler Text
m3passwordField = passwordField { fieldView = \theId name attrs eval isReq -> [whamlet|
<md-filled-text-field ##{theId} type=password name=#{name} :isReq:required=true value=#{either id id eval} *{attrs}>
|] }


m3emailField :: Field Handler Text
m3emailField = emailField { fieldView = \theId name attrs eval isReq -> [whamlet|
<md-filled-text-field ##{theId} type=email name=#{name} :isReq:required=true value=#{either id id eval} *{attrs}>
|] }
