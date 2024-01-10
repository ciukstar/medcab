{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Accounts
  ( getAccountPhotoR
  , getAccountCreateR
  , postAccountsR
  , getAccountR
  ) where

import Control.Exception.Safe
    ( tryAny, SomeException (SomeException), Exception (fromException) )
import Control.Lens ((?~))
import qualified Control.Lens as L ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Database.Persist (Entity(Entity), PersistStoreWrite (insert_))
import Data.Function ((&))
import Database.Esqueleto.Experimental
    (selectOne, from, table, where_, (^.), (==.), val, Value (unValue))
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Model
    ( UserId, UserPhoto (UserPhoto)
    , EntityField (UserPhotoUser, TokenApi, StoreToken, StoreVal, StoreKey)
    , User (User), AuthenticationType (UserAuthTypePassword), Token (Token)
    , StoreType (StoreTypeDatabase, StoreTypeSession)
    , gmail, gmailAccessToken, gmailRefreshToken, Store, gmailSender
    )
import Network.HTTP.Client
    ( HttpExceptionContent(StatusCodeException)
    , HttpException (HttpExceptionRequest)
    )
import Network.Wreq (defaults, auth, oauth2Bearer, postWith)
import Foundation
    ( Handler, Widget, App
    , Route (HomeR, AccountsR, StaticR, AuthR)
    , AppMessage
      ( MsgUserAccount, MsgBack, MsgCancel, MsgEmailAddress
      , MsgPassword, MsgFullName, MsgUserRegistered, MsgSignUp, MsgSignOut
      )
    )
import Settings (widgetFile)
import Settings.StaticFiles
    ( js_account_min_js, img_person_FILL0_wght400_GRAD0_opsz24_white_svg )
import Text.Hamlet (Html, HtmlUrlI18n, ihamlet)
import Yesod.Auth (Route (LogoutR))
import Yesod.Core
    ( Yesod(defaultLayout), SomeMessage (SomeMessage), getMessageRender
    , MonadHandler (liftHandler), addMessageI, redirect, lookupSession
    , toHtml, getUrlRenderParams
    )
import Yesod.Core.Content
    (TypedContent (TypedContent), ToContent (toContent))
import Yesod.Core.Widget (setTitleI, whamlet, addScript)
import Yesod.Form.Functions (runFormPost, generateFormPost, mreq)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), FieldView (fvInput)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Persist (YesodPersist(runDB))

import Handler.Material3
    ( md3textField, md3passwordField, md3emailField )
import Text.Printf (printf)
import Network.Mail.Mime
    ( Mail, simpleMailInMemory, Address (Address), renderMail' )
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text.Lazy as TL (empty)
import Data.ByteString.Lazy as BSL (toStrict)
import qualified Data.ByteString.Base64.Lazy as B64L (encode)
import Network.Wreq.Lens (responseStatus, statusCode)


getAccountR :: UserId -> Handler Html
getAccountR uid = do
    defaultLayout $ do
        setTitleI MsgUserAccount
        addScript (StaticR js_account_min_js)
        $(widgetFile "accounts/account")


postAccountsR :: Handler Html
postAccountsR = do
    ((fr,fw),et) <- runFormPost formAccount
    case fr of
      FormSuccess r@(User email _ _ _ _ name) -> do
          runDB $ insert_ r

          accessToken <- runDB $ selectOne $ do
              x <- from $ table @Token
              where_ $ x ^. TokenApi ==. val gmail
              return x

          (atoken,rtoken,sender) <- case accessToken of
            Just (Entity tid (Token _ StoreTypeDatabase)) -> do
                access <- (unValue <$>) <$> runDB ( selectOne $ do
                    x <- from $ table @Store
                    where_ $ x ^. StoreToken ==. val tid
                    where_ $ x ^. StoreKey ==. val gmailAccessToken
                    return $ x ^. StoreVal )
                refresh <- (unValue <$>) <$> runDB ( selectOne $ do
                    x <- from $ table @Store
                    where_ $ x ^. StoreToken ==. val tid
                    where_ $ x ^. StoreKey ==. val gmailRefreshToken
                    return $ x ^. StoreVal )
                sender <- (unValue <$>) <$> runDB ( selectOne $ do
                    x <- from $ table @Store
                    where_ $ x ^. StoreToken ==. val tid
                    where_ $ x ^. StoreKey ==. val gmailSender
                    return $ x ^. StoreVal )
                return (access,refresh,sender)
            Just (Entity _ (Token _ StoreTypeSession)) -> do
                access <- lookupSession gmailAccessToken
                refresh <- lookupSession gmailRefreshToken
                sender <- lookupSession gmailSender
                return (access,refresh,sender)
            Nothing -> return (Nothing, Nothing, Nothing)

          case (atoken,rtoken,sender) of
            (Just at,Just rt,Just proxy) -> do
                msgRender <- getMessageRender
                urlRender <- getUrlRenderParams
                let mail = buildMail r (buildHtml (toHtml . msgRender) urlRender)
                
                raw <- liftIO $ decodeUtf8 . toStrict . B64L.encode <$> renderMail' mail
                
                let opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 at)
                response <- liftIO $ tryAny $ postWith
                    opts (gmailApi $ unpack proxy) (object ["raw" .= raw])

                case response of
                  Left e@(SomeException _) -> case fromException e of
                    Just (HttpExceptionRequest _ (StatusCodeException r' bs)) -> do
                        case r' L.^. responseStatus . statusCode of
                          401 -> undefined
                          403 -> do
                              liftIO $ print response
                          _   -> undefined
                    _other -> undefined
                  Right _ok -> redirect HomeR
                  
            _otherwise -> undefined
          
          addMessageI info MsgUserRegistered
          redirect HomeR
      _otherwise ->  defaultLayout $ do
          setTitleI MsgUserAccount
          addScript (StaticR js_account_min_js)
          $(widgetFile "accounts/create")


buildHtml :: HtmlUrlI18n AppMessage (Route App)
buildHtml = [ihamlet|
<div>
  <a href=#>Please click to validate
|]


buildMail :: User -> Html -> Mail
buildMail (User email _ _ _ _ name) html = simpleMailInMemory
    (Address name email)
    (Address (Just "Sergiu Starciuc") "ciukstar@gmail.com")
    "Validation email"
    TL.empty
    (renderHtml html)
    []


gmailApi :: String -> String
gmailApi = printf "https://gmail.googleapis.com/gmail/v1/users/%s/messages/send"


getAccountCreateR :: Handler Html
getAccountCreateR = do
    (fw,et) <- generateFormPost formAccount
    defaultLayout $ do
        setTitleI MsgUserAccount
        addScript (StaticR js_account_min_js)
        $(widgetFile "accounts/create")


formAccount :: Html -> MForm Handler (FormResult User, Widget)
formAccount extra = do
    msgRender <- liftHandler getMessageRender
    (emailR,emailV) <- mreq md3emailField FieldSettings
        { fsLabel = SomeMessage MsgEmailAddress
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgRender MsgEmailAddress)]
        } Nothing
    (passR,passV) <- mreq md3passwordField FieldSettings
        { fsLabel = SomeMessage MsgPassword
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgRender MsgPassword)]
        } Nothing
    (nameR,nameV) <- mreq md3textField FieldSettings
        { fsLabel = SomeMessage MsgFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgRender MsgFullName)]
        } Nothing
    return ( User <$> emailR <*> pure UserAuthTypePassword
             <*> (pure <$> passR) <*> pure Nothing <*> pure True <*> (pure <$> nameR)
           , [whamlet|
                     #{extra}
                     $forall v <- [emailV,passV,nameV]
                       ^{fvInput v}
                     |]
           )
      

getAccountPhotoR :: UserId -> Handler TypedContent
getAccountPhotoR uid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @UserPhoto
        where_ $ x ^. UserPhotoUser ==. val uid
        return x
    case photo of
      Just (Entity _ (UserPhoto _ mime bs)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_person_FILL0_wght400_GRAD0_opsz24_white_svg


info :: Text
info = "info"
