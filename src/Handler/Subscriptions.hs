{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Subscriptions
  ( getSubscriptionsR
  , getUserSubscriptionsR
  , getUserSubscriptionR
  , postUserSubscriptionR
  ) where

import Control.Monad (join)

import Data.Bifunctor (Bifunctor(second, bimap))

import Database.Esqueleto.Experimental
    ( SqlExpr, Value (unValue), select, from, table, where_, leftJoin
    , (^.), (?.), (==.), (:&)((:&))
    , on, just, val, selectOne, subSelectCount, exists, innerJoin
    )
import Database.Persist (Entity (Entity))
import Database.Persist as P (PersistStoreWrite (delete))

import Foundation (Form)
import Foundation.Data
    ( Handler
    , Route (DataR)
    , DataR (UserPhotoR, UserSubscriptionsR, SubscriptionsR, UserSubscriptionR)
    , AppMessage
      ( MsgSubscriptions, MsgNoSubscriptionsYet, MsgBack, MsgPhoto
      , MsgNoUserHasSubscribedYet, MsgEndpoint, MsgSubscription, MsgInvalidFormData
      , MsgRecordDeleted, MsgDele, MsgDeleteAreYouSure, MsgConfirmPlease, MsgCancel
      )
    )

import Model
    ( statusError, statusSuccess
    , PushSubscriptionId, PushSubscription(PushSubscription)
    , UserId, User (User), UserPhoto
    , EntityField
      ( PushSubscriptionSubscriber, UserId, UserPhotoUser, UserPhotoAttribution
      , PushSubscriptionId, PushSubscriptionPublisher
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetUser, widgetBanner, widgetSnackbar)

import Yesod.Core (Yesod(defaultLayout), getMessages, addMessageI, redirect, whamlet)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form (FormResult(FormSuccess))
import Yesod.Form.Functions (runFormPost, generateFormPost)
import Yesod.Persist.Core (YesodPersist(runDB))


postUserSubscriptionR :: UserId -> PushSubscriptionId -> Handler Html
postUserSubscriptionR uid sid = do
    ((fr,_),_) <- runFormPost formSubscriptionDelete
    case fr of
      FormSuccess () -> do
          runDB $ P.delete sid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ UserSubscriptionsR uid
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ UserSubscriptionR uid sid


formSubscriptionDelete :: Form ()
formSubscriptionDelete extra = return (pure (), [whamlet|^{extra}|])


getUserSubscriptionR :: UserId -> PushSubscriptionId -> Handler Html
getUserSubscriptionR uid sid = do

    subscription <- runDB $ selectOne $ do
        x <- from $ table @PushSubscription
        where_ $ x ^. PushSubscriptionId ==. val sid
        return x

    (fw,et) <- generateFormPost formSubscriptionDelete
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSubscriptions
        $(widgetFile "data/subscriptions/user/subscription")


getUserSubscriptionsR :: UserId -> Handler Html
getUserSubscriptionsR uid = do

    subscriber <- (second (join . unValue) <$>) <$> runDB ( selectOne $ do
        x :& h <- from $ table @User
            `leftJoin` table @UserPhoto `on` (\(u :& h) -> just (u ^. UserId) ==. h ?. UserPhotoUser)
            
        where_ $ x ^. UserId ==. val uid
        return (x, h ?. UserPhotoAttribution) )

    subscriptions <- (second (second (join . unValue)) <$>) <$> runDB ( select $ do
        x :& u :& h <- from $ table @PushSubscription
            `innerJoin` table @User `on` (\(x :& u) -> x ^. PushSubscriptionPublisher ==. u ^. UserId)
            `leftJoin` table @UserPhoto `on` (\(_ :& u :& h) -> just (u ^. UserId) ==. h ?. UserPhotoUser)
        where_ $ x ^. PushSubscriptionSubscriber ==. val uid
        return (x, (u, h ?. UserPhotoAttribution)) )
    
    msgs <- getMessages
    
    defaultLayout $ do
        setTitleI MsgSubscriptions
        $(widgetFile "data/subscriptions/user/subscriptions")


getSubscriptionsR :: Handler Html
getSubscriptionsR = do

    subscriptions <- (second (bimap (join . unValue) unValue) <$>) <$> runDB ( select $ do
        x :& h <- from $ table @User
            `leftJoin` table @UserPhoto `on` (\(u :& h) -> just (u ^. UserId) ==. h ?. UserPhotoUser)

        let subscriptions :: SqlExpr (Value Int)
            subscriptions = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. UserId

        where_ $ exists $ do
            y <- from $ table @PushSubscription
            where_ $ y ^. PushSubscriptionSubscriber ==. x ^. UserId
        
        return (x, (h ?. UserPhotoAttribution, subscriptions)) )
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSubscriptions
        $(widgetFile "data/subscriptions/subscriptions")
        
