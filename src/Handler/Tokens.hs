{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Tokens
  ( getTokensR
  , postTokensR
  , getTokensHookR
  , postTokensClearR
  , getGoogleSecretManagerReadR
  ) where

import Control.Exception.Safe (tryAny)
import qualified Control.Lens as L ((^.), (?~))
import Control.Monad ((<=<), void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Aeson.Lens (key, AsValue (_String))
import qualified Data.ByteString.Base64 as B64 (encode)
import Data.ByteString.Lazy (toStrict)
import qualified Data.List.Safe as LS (last)
import Data.Text (Text, pack, unpack, splitOn)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
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
    ( Handler, Widget, App (appSettings)
    , Route (HomeR, StaticR, AdminR)
    , AdminR (TokensR, TokensHookR, TokensClearR)
    , AppMessage
      ( MsgTokens, MsgBack, MsgInitialize, MsgUserSession, MsgDatabase
      , MsgStoreType, MsgInvalidStoreType, MsgRecordEdited, MsgClearSettings
      , MsgRecordDeleted, MsgInvalidFormData, MsgCleared, MsgEmailAddress
      , MsgGmailAccount, MsgGoogleSecretManager
      )
    )
import Data.Function ((&))
import Model
    ( gmailAccessToken, gmailRefreshToken, gmail
    , StoreType
      ( StoreTypeDatabase, StoreTypeSession, StoreTypeGoogleSecretManager )
    , Store (Store), Token (Token, tokenStore)
    , EntityField (StoreVal, TokenStore, TokenApi)
    , gmailSender, statusSuccess, statusError, gmailAccessTokenExpiresIn
    )
import Network.Wreq
    ( post, FormParam ((:=)), responseBody, defaults, auth, oauth2Bearer
    , postWith, getWith
    )
import Network.Wreq.Lens (statusCode, responseStatus)
import Settings
    ( widgetFile, AppSettings (appGoogleClientId, appGoogleClientSecret) )
import Settings.StaticFiles (js_tokens_min_js)
import System.IO (readFile')
import Text.Blaze.Html (preEscapedToHtml, toHtml)
import Text.Hamlet (Html)
import Text.Read (readMaybe)
import Text.Shakespeare.Text (st)
import Yesod.Core
    ( Yesod(defaultLayout), whamlet, SomeMessage (SomeMessage), getYesod
    , getUrlRender, deleteSession, getMessageRender, getMessages, logWarn
    , addMessage
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

import Handler.Material3 (md3radioField, md3emailField)


getGoogleSecretManagerReadR :: Handler Html
getGoogleSecretManagerReadR = do
    secret <- liftIO $ readFile' "/grt/gmail_refresh_token"
    defaultLayout [whamlet|<p>Secret: #{secret}|]


projects :: Text
projects = "https://secretmanager.googleapis.com/v1/projects" :: Text


project :: Text
project = "medcab-410214" :: Text        
        

getTokensHookR :: Handler Html
getTokensHookR = do
    rndr <- getUrlRender
    app <- appSettings <$> getYesod
    let googleClientId = appGoogleClientId app
    let googleClientSecret = appGoogleClientSecret app

    code <- runInputGet $ ireq textField "code"
    state <- readMaybe .  unpack <$> runInputGet (ireq textField "state")

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

    let accessToken = r L.^. responseBody . key "access_token" . _String
    let refreshToken = r L.^. responseBody . key "refresh_token" . _String
    let expiresIn = r L.^. responseBody . key "expires_in" . _String

    setSession gmailAccessToken accessToken
    setSession gmailRefreshToken refreshToken
    setSession gmailAccessTokenExpiresIn expiresIn

    case state of
      Just (email,x@StoreTypeSession) -> do
          setSession gmailSender email
          _ <- runDB $ upsert (Token gmail x) [TokenStore P.=. x]
          addMessageI statusSuccess MsgRecordEdited
          redirect $ AdminR TokensR
      Just (email,x@StoreTypeDatabase) -> do
          setSession gmailSender email
          Entity tid _ <- runDB $ upsert (Token gmail x) [TokenStore P.=. x]
          _ <- runDB $ upsert (Store tid gmailAccessToken accessToken) [StoreVal P.=. accessToken]
          _ <- runDB $ upsert (Store tid gmailRefreshToken refreshToken) [StoreVal P.=. refreshToken]
          _ <- runDB $ upsert (Store tid gmailSender email) [StoreVal P.=. email]
          addMessageI statusSuccess MsgRecordEdited
          redirect $ AdminR TokensR
      Just (email,x@StoreTypeGoogleSecretManager) -> do
          setSession gmailSender email

          let opts = defaults & auth L.?~ oauth2Bearer (encodeUtf8 accessToken)
          response <- liftIO $ tryAny $ postWith opts
              (unpack [st|#{projects}/#{project}/secrets/#{gmailRefreshToken}:addVersion|])
              (object [ "payload" .= object [ "data" .= decodeUtf8 (B64.encode (encodeUtf8 refreshToken)) ]])

          -- destroy previous version
          case response of
            Right res -> do
                
                let prev :: Maybe Int
                    prev = (fmap (\y -> y - 1) . readMaybe . unpack) <=< (LS.last . splitOn "/")
                        $ res L.^. responseBody . key "name" . _String

                case prev of
                  Just v | v > 0 -> do
                               void $ liftIO $ tryAny $ postWith opts
                                   (unpack [st|#{projects}/#{project}/secrets/#{gmailRefreshToken}/versions/#{v}:destroy|])
                                   (object [])
                         | otherwise -> return ()
                  Nothing -> return ()
            
            Left e -> do
                let msg = pack $ show e
                addMessage statusError (toHtml msg)
                $(logWarn) msg
                    
          Entity tid _ <- runDB $ upsert (Token gmail x) [TokenStore P.=. x]
          _ <- runDB $ upsert (Store tid gmailSender email) [StoreVal P.=. email]

          addMessageI statusSuccess MsgRecordEdited
          redirect $ AdminR TokensR
      Nothing -> do
          addMessageI statusError MsgInvalidStoreType
          redirect $ AdminR TokensR


postTokensClearR :: Handler Html
postTokensClearR = do

    token <- runDB $ selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val gmail
        return x

    ((fr2,fw2),et2) <- runFormPost formTokensClear
    case (fr2,token) of
      (FormSuccess (),Just (Entity tid (Token _ StoreTypeSession))) -> do
          deleteSession gmailAccessToken
          deleteSession gmailRefreshToken
          deleteSession gmailAccessTokenExpiresIn
          deleteSession gmailSender
          runDB $ delete tid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ AdminR TokensR
      (FormSuccess (),Just (Entity tid (Token _ StoreTypeDatabase))) -> do
          deleteSession gmailAccessToken
          deleteSession gmailRefreshToken
          deleteSession gmailAccessTokenExpiresIn
          deleteSession gmailSender
          runDB $ delete tid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ AdminR TokensR
      (FormSuccess (),Just (Entity tid (Token _ StoreTypeGoogleSecretManager))) -> do
          app <- appSettings <$> getYesod
          -- 1. read refresh token from mounted volume
          refreshToken <- liftIO $ readFile' "/grt/gmail_refresh_token"
          
          -- 2. get access token from googleapi
          refreshResponse <- liftIO $ post "https://oauth2.googleapis.com/token"
              [ "refresh_token" := refreshToken
              , "client_id" := appGoogleClientId app
              , "client_secret" := appGoogleClientSecret app
              , "grant_type" := ("refresh_token" :: Text)
              ]

          let newAccessToken = refreshResponse L.^. responseBody . key "access_token" . _String
          
          let opts = defaults & auth L.?~ oauth2Bearer (encodeUtf8 newAccessToken)

          res <- liftIO $ getWith opts
              (unpack [st|#{projects}/#{project}/secrets/#{gmailRefreshToken}/versions/latest|])

          let ver :: Maybe Int
              ver = (readMaybe . unpack) <=< (LS.last . splitOn "/") $ res L.^. responseBody . key "name" . _String
              
          case ver of
            Just v -> do
                
                void $ liftIO $ tryAny $ postWith opts
                    (unpack [st|#{projects}/#{project}/secrets/#{gmailRefreshToken}/versions/#{v}:destroy|])
                    (object [])

            Nothing -> return ()
          
          deleteSession gmailAccessToken
          deleteSession gmailRefreshToken
          deleteSession gmailAccessTokenExpiresIn
          deleteSession gmailSender
          
          runDB $ delete tid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ AdminR TokensR
          
      (FormSuccess (),Nothing) -> do
          deleteSession gmailAccessToken
          deleteSession gmailRefreshToken
          deleteSession gmailAccessTokenExpiresIn
          deleteSession gmailSender
          addMessageI statusSuccess MsgCleared
          redirect $ AdminR TokensR
      _otherwise -> do
          (fw,et) <- generateFormPost $ formStoreOptions token
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
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
              scope = "https://www.googleapis.com/auth/gmail.send https://www.googleapis.com/auth/cloud-platform"

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
          msgs <- getMessages
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
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTokens
        addScript (StaticR js_tokens_min_js)
        $(widgetFile "admin/tokens/tokens")


formStoreOptions :: Maybe (Entity Token) -> Html -> MForm Handler (FormResult (Text,StoreType), Widget)
formStoreOptions token extra = do
    msg <- getMessageRender
    let storeOptions = [ (MsgUserSession, StoreTypeSession)
                       , (MsgDatabase, StoreTypeDatabase)
                       , (MsgGoogleSecretManager, StoreTypeGoogleSecretManager)
                       ]
    (emailR,emailV) <- mreq md3emailField FieldSettings
        { fsLabel = SomeMessage MsgEmailAddress
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing
        , fsAttrs = [("label", msg MsgGmailAccount)]
        } (Just "ciukstar@gmail.com")
    (storeR,storeV) <- mreq (md3radioField (optionsPairs storeOptions)) FieldSettings
        { fsLabel = SomeMessage MsgStoreType
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing
        , fsAttrs = [("class","app-options-store-type")]
        } (tokenStore . entityVal <$> token)
    return ( (,) <$> emailR <*> storeR
           , [whamlet|
               #{extra}
               ^{fvInput emailV}
               <fieldset.shape-medium>
                 <legend.body-medium>_{MsgStoreType}<sup>*
                 ^{fvInput storeV}
             |]
           )
