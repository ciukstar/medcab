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
  , postDoctorDeleR
  , getDoctorEditR
  , postDoctorR
  ) where

import Control.Monad (void)
import Data.Bifunctor (Bifunctor(second))
import Data.Text.Encoding (encodeUtf8)
import Database.Persist
    ( Entity (Entity, entityKey), PersistStoreWrite (insert, delete, replace)
    , PersistUniqueWrite (upsert)
    )
import qualified Database.Persist as P ((=.))
import Database.Esqueleto.Experimental
    ( select, from, table, selectOne, Entity (entityVal), val, innerJoin, on
    , (^.), (==.), (:&)((:&))
    , where_, orderBy, asc, Value (unValue)
    )
import Material3
    ( md3textField, md3telField, md3emailField, md3selectField )
import Handler.Menu (menu)
import Foundation
    (Handler, Widget
    , Route (StaticR, AuthR, AccountR, AccountPhotoR, DataR)
    , DataR
      ( DoctorCreateR, DoctorsR, DoctorPhotoR, DoctorR, DoctorDeleR
      , DoctorEditR
      )
    , AppMessage
      ( MsgDoctors, MsgUserAccount, MsgSignOut, MsgSignIn, MsgPhoto, MsgEdit
      , MsgNoDoctorsYet, MsgSave, MsgCancel, MsgFullName, MsgMobile
      , MsgEmailAddress, MsgSpecialization, MsgDoctor, MsgBack, MsgRecordCreated
      , MsgEdit, MsgDele, MsgRecordDeleted, MsgDeleteAreYouSure, MsgConfirmPlease
      , MsgRecordEdited
      )
    )
import Model
    ( AvatarColor (AvatarColorLight)
    , Doctor (Doctor, doctorName, doctorMobile, doctorEmail, doctorSpecialty)
    , DoctorId, DoctorPhoto (DoctorPhoto)
    , EntityField
      ( DoctorPhotoDoctor, DoctorPhotoMime, DoctorPhotoPhoto, DoctorId
      , SpecialtyName, DoctorSpecialty, SpecialtyId
      )
    , statusSuccess, Specialty (specialtyName), statusError
    )
import Settings (widgetFile)
import Settings.StaticFiles
    ( img_person_FILL0_wght400_GRAD0_opsz24_svg )
import Text.Hamlet (Html)
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, fileSourceByteString
    , FileInfo (fileContentType), SomeMessage (SomeMessage), getMessageRender
    , addMessageI, setUltDestCurrent, MonadHandler (liftHandler), getMessages
    , whamlet
    )
import Yesod.Core.Handler (redirect)
import Yesod.Core.Content (TypedContent (TypedContent), ToContent (toContent))
import Yesod.Form.Fields (fileField, optionsPairs)
import Yesod.Form.Functions (generateFormPost, mreq, mopt, runFormPost)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput, fvId)
    )
import Yesod.Persist (YesodPersist (runDB))


postDoctorDeleR :: DoctorId -> Handler Html
postDoctorDeleR did = do
    ((fr,fw),et) <- runFormPost formDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete did
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR DoctorsR
      _otherwise -> do

          doctor <- (second unValue <$>) <$> runDB ( selectOne $ do
              x :& s <- from $ table @Doctor
                  `innerJoin` table @Specialty `on` (\(x :& s) -> x ^. DoctorSpecialty ==. s ^. SpecialtyId)
              where_ $ x ^. DoctorId ==. val did
              return (x, s ^. SpecialtyName) )
              
          msgs <- getMessages              
          defaultLayout $ do
              setTitleI MsgDoctor
              $(widgetFile "data/doctors/doctor")


formDelete :: Html -> MForm Handler (FormResult (),Widget)
formDelete extra = return (pure (), [whamlet|#{extra}|])


getDoctorEditR :: DoctorId -> Handler Html
getDoctorEditR did = do

    doctor <- runDB $ selectOne $ do
        x <- from $ table @Doctor
        where_ $ x ^. DoctorId ==. val did
        return x
        
    (fw,et) <- generateFormPost $ formDoctor doctor

    defaultLayout $ do
          setTitleI MsgDoctor
          $(widgetFile "data/doctors/edit")   


postDoctorR :: DoctorId -> Handler Html
postDoctorR did = do
    ((fr,fw),et) <- runFormPost $ formDoctor Nothing
    case fr of
      FormSuccess (r,mfi) -> do
          runDB $ replace did r
          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                void $ runDB $ upsert (DoctorPhoto did (fileContentType fi) bs)
                    [DoctorPhotoMime P.=. fileContentType fi, DoctorPhotoPhoto P.=. bs]
            Nothing -> return ()
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ DoctorR did
      _otherwise -> do
          defaultLayout $ do
              setTitleI MsgDoctor
              $(widgetFile "data/doctors/create")


getDoctorR :: DoctorId -> Handler Html
getDoctorR did = do
    
    doctor <- (second unValue <$>) <$> runDB ( selectOne $ do
        x :& s <- from $ table @Doctor
            `innerJoin` table @Specialty `on` (\(x :& s) -> x ^. DoctorSpecialty ==. s ^. SpecialtyId)
        where_ $ x ^. DoctorId ==. val did
        return (x, s ^. SpecialtyName) )

    (fw,et) <- generateFormPost formDelete
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgDoctor
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
        $(widgetFile "data/doctors/create")


formDoctor :: Maybe (Entity Doctor)
           -> Html -> MForm Handler (FormResult (Doctor,Maybe FileInfo), Widget)
formDoctor doctor extra = do
    rndr <- getMessageRender

    specs <- liftHandler $ ((\s -> (specialtyName . entityVal $ s,entityKey s)) <$>) <$> runDB ( select $ do
        x <- from $ table @Specialty
        orderBy [asc (x ^. SpecialtyName)]
        return x )
    
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
        
    (specR,specV) <- mreq (md3selectField (optionsPairs specs)) FieldSettings
        { fsLabel = SomeMessage MsgSpecialization
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgSpecialization)]
        } (doctorSpecialty . entityVal <$> doctor)
        
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

    msgs <- getMessages
    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgDoctors
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
