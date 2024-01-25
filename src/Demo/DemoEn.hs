{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo.DemoEn (fillDemoEn) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.SqlBackend (SqlBackend)
import Model
    ( AuthenticationType (UserAuthTypeEmail)
    , Specialty
      ( Specialty, specialtyName, specialtyCode, specialtyDescr, specialtyGroup )
    , User
      ( User, userEmail, userAuthType, userPassword, userVerkey, userVerified
      , userName, userSuperuser, userAdmin
      )
    , UserPhoto
      ( UserPhoto, userPhotoMime, userPhotoPhoto, userPhotoUser
      , userPhotoAttribution
      )
    )

import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (st)

import Yesod.Auth.Email (saltPass)
import Yesod.Form.Fields (Textarea(Textarea))
import Yesod.Persist(PersistStoreWrite (insert, insert_))
import Data.FileEmbed (embedFile)


fillDemoEn :: MonadIO m => ReaderT SqlBackend m ()
fillDemoEn = do

    pass1 <- liftIO $ saltPass "marylopez"
    let user1 = User { userEmail = "marylopez@gmail.com"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass1
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Mary Lopez"
                     , userSuperuser = False
                     , userAdmin = True
                     }

    u1 <- insert user1

    insert_ UserPhoto { userPhotoUser = u1
                      , userPhotoMime = "image/avif"
                      , userPhotoPhoto = $(embedFile "demo/happy-woman-gray-polo-shirt-with-pink-pin-button_53876-102858.avif")
                      , userPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]
                      }

    pass2 <- liftIO $ saltPass "jjohnson"
    let user2 = User { userEmail = "jjohnson@gmail.com"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass2
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "John Johnson"
                     , userSuperuser = False
                     , userAdmin = False
                     }

    u2 <- insert user2

    insert_ UserPhoto { userPhotoUser = u2
                      , userPhotoMime = "image/avif"
                      , userPhotoPhoto = $(embedFile "demo/smiley-handsome-man-posing_23-2148911841.avif")
                      , userPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]

                      }

    let specialty1 = Specialty { specialtyName = "Allergy and immunology"
                               , specialtyCode = Just "ALLI"
                               , specialtyDescr = Just $ Textarea [st|Immunology is a branch of medicine that covers the study of immune systems in all organisms.|]
                               , specialtyGroup = Nothing
                               }

    s1 <- insert specialty1

    let specialty2 = Specialty { specialtyName = "Anesthesiology"
                               , specialtyCode = Just "ANES"
                               , specialtyDescr = Just $ Textarea [st|Anesthesiology, anaesthesiology, or anaesthesia is the medical specialty concerned with the total perioperative care of patients before, during and after surgery.|]
                               , specialtyGroup = Nothing
                               }

    s2 <- insert specialty2

    let specialty3 = Specialty { specialtyName = "Cardiology"
                               , specialtyCode = Just "CARD"
                               , specialtyDescr = Just $ Textarea [st|Cardiology is a branch of medicine that deals with disorders of the heart and the cardiovascular system.|]
                               , specialtyGroup = Nothing
                               }

    s3 <- insert specialty3

    let specialty4 = Specialty { specialtyName = "Dermatology"
                               , specialtyCode = Just "DERMA"
                               , specialtyDescr = Just $ Textarea [st|Dermatology is the branch of medicine dealing with the skin. It is a speciality with both medical and surgical aspects. A dermatologist is a specialist medical doctor who manages diseases related to skin, hair, nails, and some cosmetic problems.|]
                               , specialtyGroup = Nothing
                               }

    s4 <- insert specialty4

    return ()
