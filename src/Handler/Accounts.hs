{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Accounts (getAccountPhotoR) where

import Database.Persist (Entity(Entity))
import Data.FileEmbed (embedFile)
import Database.Esqueleto.Experimental
    (selectOne, from, table, where_, (^.), (==.), val)
import Data.Text.Encoding (encodeUtf8)
import Model (UserId, UserPhoto (UserPhoto), EntityField (UserPhotoUser))
import Foundation (Handler)
import Yesod.Core.Content (TypedContent (TypedContent), typeSvg, ToContent (toContent))
import Yesod.Persist (YesodPersist(runDB))


getAccountPhotoR :: UserId -> Handler TypedContent
getAccountPhotoR uid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @UserPhoto
        where_ $ x ^. UserPhotoUser ==. val uid
        return x
    return $ case photo of
      Just (Entity _ (UserPhoto _ mime bs)) -> TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> TypedContent typeSvg
        $ toContent $(embedFile "static/img/person_FILL0_wght400_GRAD0_opsz24.svg")
