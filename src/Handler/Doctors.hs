{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Doctors
  ( getDoctorsR
  , getDoctorCreateR
  , getDoctorPhotoR
  , postDoctorsR
  , getDoctorR
  ) where

import Control.Monad (void)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist
    ( Entity (Entity), PersistStoreWrite(insert), PersistUniqueWrite (upsert)
    )
import qualified Database.Persist as P ((=.))
import Database.Esqueleto.Experimental
    ( select, from, table, selectOne, Entity (entityVal), val
    , (^.), (==.)
    , where_
    )
import Handler.Material3 (md3textField, md3telField, md3emailField)
import Handler.Menu (menu)
import Foundation
    (Handler, Widget
    , Route (StaticR, AuthR, AccountR, AccountPhotoR, DataR)
    , DataR (DoctorCreateR, DoctorsR, DoctorPhotoR, DoctorR)
    , AppMessage
      ( MsgDoctors, MsgUserAccount, MsgSignOut, MsgSignIn, MsgPhoto, MsgEdit
      , MsgNoDoctorsYet, MsgSave, MsgCancel, MsgFullName, MsgMobile
      , MsgEmailAddress, MsgSpecialization, MsgDoctor, MsgBack, MsgRecordCreated
      , MsgEdit, MsgDele
      )
    )
import Model
    ( AvatarColor (AvatarColorLight)
    , Doctor (Doctor, doctorName, doctorMobile, doctorEmail, doctorSpecialization)
    , DoctorId, DoctorPhoto (DoctorPhoto)
    , EntityField (DoctorPhotoDoctor, DoctorPhotoMime, DoctorPhotoPhoto, DoctorId)
    , statusSuccess
    )
import Settings (widgetFile)
import Settings.StaticFiles
    ( js_doctors_doctors_min_js, js_doctors_create_min_js, js_doctors_doctor_min_js
    , img_person_FILL0_wght400_GRAD0_opsz24_svg
    )
import Text.Hamlet (Html)
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, addScript, newIdent, FileInfo (fileContentType)
    , SomeMessage (SomeMessage), getMessageRender, addMessageI, fileSourceByteString
    )
import Yesod.Form.Functions (generateFormPost, mreq, mopt, runFormPost)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput, fvId)
    )
import Yesod.Persist (YesodPersist (runDB))
import Yesod.Core.Handler (redirect)
import Yesod.Core.Content (TypedContent (TypedContent), ToContent (toContent))
import Yesod.Form.Fields (fileField)


getDoctorR :: DoctorId -> Handler Html
getDoctorR did = do
    
    doctor <- runDB $ selectOne $ do
        x <- from $ table @Doctor
        where_ $ x ^. DoctorId ==. val did
        return x
        
    defaultLayout $ do
        setTitleI MsgDoctor
        addScript (StaticR js_doctors_doctor_min_js)
        $(widgetFile "data/doctors/doctor")


postDoctorsR :: Handler Html
postDoctorsR = do
    ((fr,fw),et) <- runFormPost $ formDoctor Nothing
    case fr of
      FormSuccess (r,mfi) -> do
          did <- runDB $ insert r
          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                void $ runDB $ upsert (DoctorPhoto did (fileContentType fi) bs)
                    [DoctorPhotoMime P.=. fileContentType fi, DoctorPhotoPhoto P.=. bs]
            Nothing -> return ()
          addMessageI statusSuccess MsgRecordCreated
          redirect $ DataR DoctorsR
      _otherwise -> defaultLayout $ do
          setTitleI MsgDoctor
          $(widgetFile "data/doctors/create")


getDoctorCreateR :: Handler Html
getDoctorCreateR = do
    (fw,et) <- generateFormPost $ formDoctor Nothing
    defaultLayout $ do
        setTitleI MsgDoctor
        addScript (StaticR js_doctors_create_min_js)
        $(widgetFile "data/doctors/create")


formDoctor :: Maybe (Entity Doctor)
           -> Html -> MForm Handler (FormResult (Doctor,Maybe FileInfo), Widget)
formDoctor doctor extra = do
    rndr <- getMessageRender
    (nameR,nameV) <- mreq md3textField FieldSettings
        { fsLabel = SomeMessage MsgFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgFullName)]
        } (doctorName . entityVal <$> doctor)
    (mobileR,mobileV) <- mreq md3telField FieldSettings
        { fsLabel = SomeMessage MsgMobile
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgMobile)]
        } (doctorMobile . entityVal <$> doctor)
    (emailR,emailV) <- mreq md3emailField FieldSettings
        { fsLabel = SomeMessage MsgEmailAddress
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgEmailAddress)]
        } (doctorEmail . entityVal <$> doctor)
    (specR,specV) <- mreq md3textField FieldSettings
        { fsLabel = SomeMessage MsgSpecialization
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgSpecialization)]
        } (doctorSpecialization . entityVal <$> doctor)
    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    let r = (,) <$> (Doctor <$> nameR <*> mobileR <*> emailR <*> specR) <*> photoR

    idLabelPhoto <- newIdent
    idFigurePhoto <- newIdent
    idImgPhoto <- newIdent
    let w = $(widgetFile "data/doctors/form")
    
    return (r,w)


getDoctorsR :: Handler Html
getDoctorsR = do
    user <- maybeAuth

    doctors <- runDB $ select $ from $ table @Doctor
    
    defaultLayout $ do
        setTitleI MsgDoctors
        addScript (StaticR js_doctors_doctors_min_js)
        idFabAdd <- newIdent
        $(widgetFile "data/doctors/doctors")


getDoctorPhotoR :: DoctorId -> Handler TypedContent
getDoctorPhotoR did = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @DoctorPhoto
        where_ $ x ^. DoctorPhotoDoctor ==. val did
        return x
    case photo of
      Just (Entity _ (DoctorPhoto _ mime bs)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_person_FILL0_wght400_GRAD0_opsz24_svg
