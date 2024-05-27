{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}

module Model where

import Control.Applicative (pure)
import Control.Monad (mapM, return)
import ClassyPrelude.Yesod
    ( Ord, Read, Typeable, Bool, ByteString, Text, Double, derivePersistField
    , mkMigrate, mkPersist, persistFileWith, share, sqlSettings
    )

import Data.Aeson
    ( Value (String), ToJSON, toJSON, FromJSON, parseJSON, withObject, (.:)
    )
import Data.Aeson.Types (Parser, prependFailure, typeMismatch)
import Data.Eq (Eq ((==)))
import Data.Function ((.), ($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.String (String)
import Data.Text (unpack, pack)
import Data.Time (UTCTime)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay)

import Database.Persist.Quasi ( lowerCaseSettings )
import Database.Persist.Sql (fromSqlKey, toSqlKey)

import Text.Hamlet (Html)
import Text.Show (Show (show))
import Text.Read (readMaybe)

import Yesod.Core.Dispatch
    ( PathPiece (fromPathPiece, toPathPiece)
    , PathMultiPiece (fromPathMultiPiece, toPathMultiPiece)
    )
import Yesod.Form (Textarea)


data PushMsgType = PushMsgTypeCall | PushMsgTypeAccept | PushMsgTypeDecline
                 | PushMsgTypeCancel | PushMsgTypeEnd
    deriving (Eq, Show, Read)


instance ToJSON PushMsgType where
    toJSON :: PushMsgType -> Data.Aeson.Value
    toJSON PushMsgTypeCall = String "PushMsgTypeCall"
    toJSON PushMsgTypeAccept = String "PushMsgTypeAccept"
    toJSON PushMsgTypeDecline = String "PushMsgTypeDecline"
    toJSON PushMsgTypeCancel = String "PushMsgTypeCancel"
    toJSON PushMsgTypeEnd = String "PushMsgTypeEnd"


data ChatMessageStatus = ChatMessageStatusRead | ChatMessageStatusUnread
    deriving (Show, Read, Eq, Ord)
derivePersistField "ChatMessageStatus"

instance ToJSON ChatMessageStatus where
    toJSON :: ChatMessageStatus -> Value
    toJSON ChatMessageStatusRead = "Read"
    toJSON ChatMessageStatusUnread = "Unread"

instance FromJSON ChatMessageStatus where
    parseJSON :: Value -> Parser ChatMessageStatus
    parseJSON (String "Read") = pure ChatMessageStatusRead
    parseJSON (String "Unread") = pure ChatMessageStatusUnread
    parseJSON invalid = prependFailure "parsing ChatMessageStatus failed" (typeMismatch "String" invalid)


data Gender = GenderFemale | GenderMale | GenderOther
    deriving (Show, Read, Eq, Ord)
derivePersistField "Gender"


data AvatarColor = AvatarColorLight | AvatarColorDark
    deriving (Show, Read, Eq)

instance PathPiece AvatarColor where
    fromPathPiece :: Text -> Maybe AvatarColor
    fromPathPiece = readMaybe . unpack

    toPathPiece :: AvatarColor -> Text
    toPathPiece = pack . show


data StoreType = StoreTypeDatabase | StoreTypeSession | StoreTypeGoogleSecretManager
    deriving (Show, Read, Eq, Ord)
derivePersistField "StoreType"


data AuthenticationType = UserAuthTypePassword
                        | UserAuthTypeEmail
                        | UserAuthTypeGoogle
    deriving (Show, Read, Eq, Ord)
derivePersistField "AuthenticationType"

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")


webPushEndpoint :: Text
webPushEndpoint = "webPushEndpoint"

webPushP256dh :: Text
webPushP256dh = "webPushP256dh"

webPushAuth :: Text
webPushAuth = "webPushAuth"


newtype SignTags = SignTags { unTags :: [SignTagId] }
    deriving (Show, Read, Eq)

instance PathMultiPiece SignTags where
    toPathMultiPiece :: SignTags -> [Text]
    toPathMultiPiece (SignTags xs) = pack . show . fromSqlKey <$> xs

    fromPathMultiPiece :: [Text] -> Maybe SignTags
    fromPathMultiPiece xs = SignTags <$> mapM ((toSqlKey <$>) . readMaybe . unpack) xs


newtype Specialties = Specialties { unSpecialties :: [SpecialtyId] }
    deriving (Show, Read, Eq)

instance PathMultiPiece Specialties where
    toPathMultiPiece :: Specialties -> [Text]
    toPathMultiPiece (Specialties xs) = pack . show . fromSqlKey <$> xs

    fromPathMultiPiece :: [Text] -> Maybe Specialties
    fromPathMultiPiece xs = Specialties <$> mapM ((toSqlKey <$>) . readMaybe . unpack) xs


instance Eq User where
    (==) :: User -> User -> Bool
    a == b = userEmail a == userEmail b


gmailSender :: Text
gmailSender = "gmail_sender"

gmailAccessTokenExpiresIn :: Text
gmailAccessTokenExpiresIn = "gmail_access_token_expires_in"

gmailAccessToken :: Text
gmailAccessToken = "gmail_access_token"

gmailRefreshToken :: Text
gmailRefreshToken = "gmail_refresh_token"

apiInfoGoogle :: Text
apiInfoGoogle = "GOOGLE_API"

secretVapid :: Text
secretVapid = "vapid_min_details"

apiInfoVapid :: Text
apiInfoVapid = "VAPID"



statusSuccess :: Text
statusSuccess = "success"

statusError :: Text
statusError = "error"

ultDestKey :: Text
ultDestKey = "_ULT"

secretVolumeVapid :: String
secretVolumeVapid = "/vapid/vapid_min_details"


secretVolumeGmail :: String
secretVolumeGmail = "/grt/gmail_refresh_token"

paramBacklink :: Text
paramBacklink = "backlink"

paramEndpoint :: Text
paramEndpoint = "endpoint"
