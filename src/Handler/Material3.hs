{-# LANGUAGE QuasiQuotes #-}

module Handler.Material3
  ( m3emailField
  , m3passwordField
  , m3textField
  ) where

import Data.Text (Text)
import Foundation (Handler)
import Yesod.Core.Widget (whamlet)
import Yesod.Form.Fields (emailField, passwordField, textField)
import Yesod.Form.Types (Field (fieldView))

      
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
