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

import ClassyPrelude.Yesod
    ( Eq, Ord, Read, Typeable, Bool, ByteString, Text, derivePersistField
    , mkMigrate, mkPersist, persistFileWith, share, sqlSettings
    )
import Data.Function ((.))
import Data.Maybe (Maybe)
import Data.Text (unpack, pack)
import Data.Time.Calendar (Day)
import Database.Persist.Quasi ( lowerCaseSettings )
import Text.Read (readMaybe)
import Text.Show (Show (show))
import Yesod.Core.Dispatch (PathPiece (fromPathPiece, toPathPiece))
import Yesod.Form (Textarea)


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


gmailSender :: Text
gmailSender = "gmail_sender"

gmailAccessTokenExpiresIn :: Text
gmailAccessTokenExpiresIn = "gmail_access_token_expires_in"

gmailAccessToken :: Text
gmailAccessToken = "gmail_access_token"

gmailRefreshToken :: Text
gmailRefreshToken = "gmail_refresh_token"

gmail :: Text
gmail = "GMAIL_API"


statusSuccess :: Text
statusSuccess = "success"

statusError :: Text
statusError = "error"
