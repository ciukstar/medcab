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
    ( Eq, Ord, Read, Show, Typeable, Bool, ByteString, Text, derivePersistField
    , mkMigrate, mkPersist, persistFileWith, share, sqlSettings
    )
import Database.Persist.Quasi ( lowerCaseSettings )


data StoreType = StoreTypeDatabase | StoreTypeSession
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
