{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Tokens
  ( getTokensR
  , postTokensR
  , getTokensHookR
  , postTokensClearR
  ) where

import qualified Control.Lens as L ((^.), (^?))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Lens (key, AsValue (_String), AsNumber (_Integer))
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val
    , (^.), (==.)
    )
import Database.Persist
    ( Entity(Entity, entityVal)
    , PersistStoreWrite (delete)
    , PersistUniqueWrite (upsert)
    )
import qualified Database.Persist as P ((=.))
import Foundation
    ( Handler, Widget
    , Route (HomeR, StaticR, AdminR)
    , AdminR (TokensR, TokensHookR, TokensClearR)
    , AppMessage
      ( MsgTokens, MsgBack, MsgInitialize, MsgUserSession, MsgDatabase
      , MsgStoreType, MsgInvalidStoreType, MsgRecordEdited, MsgClearSettings
      , MsgRecordDeleted, MsgInvalidFormData, MsgCleared
      ), App (appSettings)
    )
import Model
    ( gmailAccessToken, gmailRefreshToken, gmail
    , SaveOption (SaveOptionUserSession, SaveOptionDatabase)
    , Store (Store), Token (Token, tokenStore)
    , EntityField (StoreVal, TokenStore, TokenApi)
    )
import Network.Wreq (post, FormParam ((:=)), responseBody)
import Network.Wreq.Lens (statusCode, responseStatus)
import Settings
    ( widgetFile, AppSettings (appGoogleClientId, appGoogleClientSecret) )
import Settings.StaticFiles (js_tokens_min_js)
import Text.Blaze.Html (preEscapedToHtml)
import Text.Hamlet (Html)
import Text.Read (readMaybe)
import Yesod.Core
    ( Yesod(defaultLayout), whamlet, SomeMessage (SomeMessage), getYesod
    , getUrlRender, deleteSession
    )
import Yesod.Core.Handler (redirect, addMessageI, setSession)
import Yesod.Core.Widget (setTitleI, addScript)
import Yesod.Persist (YesodPersist(runDB))
import Yesod.Form.Input (ireq, runInputGet)
import Yesod.Form.Fields (optionsPairs, textField)
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), FieldView (fvInput)
    , FieldSettings (FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    )

import Handler.Material3 (m3radioField)


getTokensHookR :: Handler Html
getTokensHookR = do
    rndr <- getUrlRender
    app <- appSettings <$> getYesod
    let googleClientId = appGoogleClientId app
    let googleClientSecret = appGoogleClientSecret app
    
    code <- runInputGet $ ireq textField "code"
    store <- readMaybe .  unpack <$> runInputGet (ireq textField "state")

    r <- liftIO $ post "https://oauth2.googleapis.com/token"
         [ "code" := code
         , "redirect_uri" := rndr (AdminR TokensHookR)
         , "client_id" := googleClientId
         , "client_secret" := googleClientSecret
         , "grant_type" := ("authorization_code" :: Text)
         ]

    let _status = r L.^. responseStatus . statusCode
    let _tokenType = r L.^. responseBody . key "token_type" . _String
    let _scope = r L.^. responseBody . key "scope" . _String
    let _expiresIn = r L.^? responseBody . key "expires_in" . _Integer

    let accessToken = r L.^. responseBody . key "access_token" . _String
    let refreshToken = r L.^. responseBody . key "refresh_token" . _String
    
    case store of
      Just x@SaveOptionUserSession -> do
          setSession gmailAccessToken accessToken
          setSession gmailRefreshToken refreshToken
          _ <- runDB $ upsert (Token gmail x) [TokenStore P.=. x]
          addMessageI info MsgRecordEdited
          redirect $ AdminR TokensR
      Just x@SaveOptionDatabase -> do
          Entity tid _ <- runDB $ upsert (Token gmail x) [TokenStore P.=. x]
          _ <- runDB $ upsert (Store tid gmailAccessToken accessToken) [StoreVal P.=. accessToken]
          _ <- runDB $ upsert (Store tid gmailRefreshToken refreshToken) [StoreVal P.=. refreshToken]
          addMessageI info MsgRecordEdited
          redirect $ AdminR TokensR
      Nothing -> do
          addMessageI warn MsgInvalidStoreType
          redirect $ AdminR TokensR


postTokensClearR :: Handler Html
postTokensClearR = do

    token <- runDB $ selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val gmail
        return x
        
    ((fr2,fw2),et2) <- runFormPost formTokensClear
    case (fr2,token) of
      (FormSuccess (),Just (Entity tid (Token _ SaveOptionUserSession))) -> do
          deleteSession gmailAccessToken
          deleteSession gmailRefreshToken
          runDB $ delete tid
          addMessageI info MsgRecordDeleted
          redirect $ AdminR TokensR
      (FormSuccess (),Just (Entity tid (Token _ SaveOptionDatabase))) -> do
          runDB $ delete tid
          addMessageI info MsgRecordDeleted
          redirect $ AdminR TokensR
      (FormSuccess (),Nothing) -> do
          addMessageI info MsgCleared
          redirect $ AdminR TokensR
      _otherwise -> do
          (fw,et) <- generateFormPost $ formStoreOptions token
          addMessageI warn MsgInvalidFormData
          defaultLayout $ do
              setTitleI MsgTokens
              $(widgetFile "admin/tokens/tokens")


formTokensClear :: Html -> MForm Handler (FormResult (),Widget)
formTokensClear extra = return (FormSuccess (),[whamlet|#{extra}|])


postTokensR :: Handler Html
postTokensR = do
    
    token <- runDB $ selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val gmail
        return x
        
    ((fr,fw),et) <- runFormPost $ formStoreOptions token
    case fr of
      FormSuccess x -> do
          app <- appSettings <$> getYesod
          urlRender <- getUrlRender
          
          let scope :: Text
              scope = "https://www.googleapis.com/auth/gmail.send"

          r <- liftIO $ post "https://accounts.google.com/o/oauth2/v2/auth"
              [ "redirect_uri" := urlRender (AdminR TokensHookR)
              , "response_type" := ("code" :: Text)
              , "prompt" := ("consent" :: Text)
              , "client_id" := appGoogleClientId app
              , "scope" := scope
              , "access_type" := ("offline" :: Text)
              , "state" := pack (show x)
              ]

          return $ preEscapedToHtml $ decodeUtf8 $ toStrict (r L.^. responseBody)
          
      _otherwise -> do
          (fw2,et2) <- generateFormPost formTokensClear
          defaultLayout $ do
              setTitleI MsgTokens
              addScript (StaticR js_tokens_min_js)
              $(widgetFile "admin/tokens/tokens")


getTokensR :: Handler Html
getTokensR = do
    
    token <- runDB $ selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val gmail
        return x

    (fw2,et2) <- generateFormPost formTokensClear
    (fw,et) <- generateFormPost $ formStoreOptions token
    defaultLayout $ do
        setTitleI MsgTokens
        addScript (StaticR js_tokens_min_js)
        $(widgetFile "admin/tokens/tokens")


formStoreOptions :: Maybe (Entity Token) -> Html -> MForm Handler (FormResult SaveOption, Widget)
formStoreOptions token extra = do
    let storeOptions = [ (MsgUserSession, SaveOptionUserSession)
                       , (MsgDatabase, SaveOptionDatabase)
                       ]
    (r,v) <- mreq (m3radioField (optionsPairs storeOptions)) FieldSettings
        { fsLabel = SomeMessage MsgStoreType
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing
        , fsAttrs = [("class","app-options-store-type")]
        } (tokenStore . entityVal <$> token)
    return (r,[whamlet|
#{extra}
^{fvInput v}
|])


info :: Text
info = "info"


warn :: Text
warn = "warn"
