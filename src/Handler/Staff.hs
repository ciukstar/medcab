{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Staff
  ( getStaffR
  , getDoctorCreateR
  , getStaffPhotoR
  , postStaffR
  , getMemberR
  , postDoctorDeleR
  , getDoctorEditR
  , postMemberR
  , getStaffSpecialtiesR
  , getDoctorSpecialtyCreateR
  , postStaffSpecialtiesR
  , getSpecialistR
  , postSpecialistDeleR
  , getSpecialistEditR
  , postSpecialistR
  ) where

import Control.Monad (void, join)

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Database.Persist
    ( Entity (Entity, entityKey)
    , PersistStoreWrite (insert, delete, replace, insert_)
    , PersistUniqueWrite (upsert)
    )
import qualified Database.Persist as P ((=.))
import Database.Esqueleto.Experimental
    ( select, from, table, selectOne, Entity (entityVal), val, innerJoin, on
    , (^.), (?.), (==.), (:&)((:&)), (=.)
    , where_, orderBy, asc, Value (unValue), update, set, just, leftJoin, desc
    )

import Foundation
    ( Handler, Form, Widget
    , Route (StaticR, DataR)
    , DataR
      ( DoctorCreateR, StaffR, StaffPhotoR, MemberR, DoctorDeleR, DoctorEditR
      , StaffSpecialtiesR, DoctorSpecialtyCreateR, SpecialistR
      , SpecialistDeleR, SpecialistEditR
      )
    , AppMessage
      ( MsgDoctors, MsgPhoto, MsgEdit
      , MsgNoDoctorsYet, MsgSave, MsgCancel, MsgFullName, MsgMobile, MsgDetails
      , MsgEmailAddress, MsgSpecialization, MsgDoctor, MsgBack, MsgRecordCreated
      , MsgEdit, MsgDele, MsgRecordDeleted, MsgDeleteAreYouSure, MsgConfirmPlease
      , MsgRecordEdited, MsgSpecialties, MsgTabs, MsgSpecialty, MsgNoSpecialtiesYet
      , MsgAttribution, MsgSpecialtyTitle, MsgSpecializations, MsgCertificateDate
      , MsgInvalidFormData, MsgAlreadyExists, MsgPhone, MsgUser
      )
    )

import Material3
    ( md3textField, md3telField, md3emailField, md3selectField, md3htmlField
    , md3mreq, md3mopt, md3dayField
    )
    
import Model
    ( Doctor (Doctor, doctorName, doctorMobile, doctorEmail, doctorPhone, doctorUser)
    , DoctorId, DoctorPhoto (DoctorPhoto), User
    , EntityField
      ( DoctorPhotoDoctor, DoctorPhotoMime, DoctorPhotoPhoto, DoctorId
      , SpecialtyName, SpecialtyId, SpecialistDoctor, SpecialistSpecialty
      , DoctorPhotoAttribution, SpecialistId, SpecialistTitle, UserId, UserEmail
      )
    , statusSuccess, statusError
    , SpecialtyId, Specialty (specialtyName, Specialty)
    , SpecialistId
    , Specialist
      (Specialist, specialistCertDate, specialistSpecialty, specialistTitle)
    )
    
import Settings (widgetFile)
import Settings.StaticFiles
    ( img_person_FILL0_wght400_GRAD0_opsz24_svg )

import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetUser, widgetBanner, widgetSnackbar)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, fileSourceByteString
    , FileInfo (fileContentType), SomeMessage (SomeMessage), getMessageRender
    , addMessageI, setUltDestCurrent, MonadHandler (liftHandler), getMessages
    , whamlet
    )
import Yesod.Core.Handler (redirect)
import Yesod.Core.Content (TypedContent (TypedContent), ToContent (toContent))
import Yesod.Form.Fields (fileField, optionsPairs)
import Yesod.Form.Functions (generateFormPost, runFormPost, checkM)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput, fvId), Field
    )
import Yesod.Persist (YesodPersist (runDB))
import Data.Bifunctor (Bifunctor(second, bimap))


postSpecialistDeleR :: DoctorId -> SpecialtyId -> SpecialistId -> Handler Html
postSpecialistDeleR did sid xid = do
    ((fr,_),_) <- runFormPost formSpecialistDele
    case fr of
      FormSuccess () -> do
          runDB $ delete xid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ StaffSpecialtiesR did
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ SpecialistR did sid xid


formSpecialistDele :: Form ()
formSpecialistDele extra = return (FormSuccess (),[whamlet|#{extra}|])


postSpecialistR :: DoctorId -> SpecialtyId -> SpecialistId -> Handler Html
postSpecialistR did sid xid = do
    specialist <- runDB $ selectOne $ do
        x <- from $ table @Specialist
        where_ $ x ^. SpecialistId ==. val xid
        return x
    ((fr,fw),et) <- runFormPost $ formSpecialty did specialist
    case fr of
      FormSuccess r -> do
          runDB $ replace xid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ StaffSpecialtiesR did
      _otherwise -> defaultLayout $ do
              setTitleI MsgSpecialization
              $(widgetFile "data/staff/specialties/edit")



getSpecialistEditR :: DoctorId -> SpecialtyId -> SpecialistId -> Handler Html
getSpecialistEditR did sid xid = do
    specialist <- runDB $ selectOne $ do
        x <- from $ table @Specialist
        where_ $ x ^. SpecialistId ==. val xid
        return x
    (fw,et) <- generateFormPost $ formSpecialty did specialist
    defaultLayout $ do
        setTitleI MsgSpecialization
        $(widgetFile "data/staff/specialties/edit")


getSpecialistR :: DoctorId -> SpecialtyId -> SpecialistId -> Handler Html
getSpecialistR did sid xid = do

    attrib <- (unValue =<<) <$> runDB ( selectOne $ do
        x <- from $ table @DoctorPhoto
        where_ $ x ^. DoctorPhotoDoctor ==. val did
        return (x ^. DoctorPhotoAttribution) )

    specialst <- runDB $ selectOne $ do
        x :& d :& s <- from $ table @Specialist
            `innerJoin` table @Doctor `on` (\(x :& d) -> x ^. SpecialistDoctor ==. d ^. DoctorId)
            `innerJoin` table @Specialty `on` (\(x :& _ :& s) -> x ^. SpecialistSpecialty ==. s ^. SpecialtyId)
        where_ $ x ^. SpecialistId ==. val xid
        return (x,d,s)

    (fw,et) <- generateFormPost formSpecialistDele
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSpecialization
        $(widgetFile "data/staff/specialties/specialty")


postStaffSpecialtiesR :: DoctorId -> Handler Html
postStaffSpecialtiesR did = do
    ((fr,fw),et) <- runFormPost $ formSpecialty did Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordCreated
          redirect $ DataR $ StaffSpecialtiesR did
      _otherwise -> do
          defaultLayout $ do
              setTitleI MsgSpecialty
              $(widgetFile "data/staff/specialties/create")


getDoctorSpecialtyCreateR :: DoctorId -> Handler Html
getDoctorSpecialtyCreateR did = do
    (fw,et) <- generateFormPost $ formSpecialty did Nothing
    defaultLayout $ do
        setTitleI MsgSpecialty
        $(widgetFile "data/staff/specialties/create")


formSpecialty :: DoctorId -> Maybe (Entity Specialist) -> Form Specialist
formSpecialty did specialist extra = do
    rndr <- getMessageRender

    specs <- liftHandler $ ((\s -> (specialtyName . entityVal $ s,entityKey s)) <$>) <$> runDB ( select $ do
        x <- from $ table @Specialty
        orderBy [asc (x ^. SpecialtyName)]
        return x )

    (specR,specV) <- md3mreq (md3selectField (optionsPairs specs)) FieldSettings
        { fsLabel = SomeMessage MsgSpecialty
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgSpecialty)]
        } (specialistSpecialty . entityVal <$> specialist)

    let spec = case specR of
          FormSuccess r -> Just r
          _oterwise -> Nothing

    (titleR,titleV) <- md3mreq (uniqueTitleField spec) FieldSettings
        { fsLabel = SomeMessage MsgSpecialtyTitle
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgSpecialtyTitle)]
        } (specialistTitle . entityVal <$> specialist)

    (certR,certV) <- md3mreq md3dayField FieldSettings
        { fsLabel = SomeMessage MsgCertificateDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgCertificateDate)]
        } (specialistCertDate . entityVal <$> specialist)

    let r = Specialist did <$> specR <*> titleR <*> certR
    let w = $(widgetFile "data/staff/specialties/form")
    return (r,w)
  where
      uniqueTitleField :: Maybe SpecialtyId -> Field Handler Text
      uniqueTitleField msid = checkM (uniqueTitle msid) md3textField

      uniqueTitle :: Maybe SpecialtyId -> Text -> Handler (Either AppMessage Text)
      uniqueTitle msid title = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Specialist
              where_ $ x ^. SpecialistTitle ==. val title
              where_ $ x ^. SpecialistDoctor ==. val did
              case msid of
                Just sid -> where_ $ x ^. SpecialistSpecialty ==. val sid
                Nothing -> return ()
              return x
          return $ case (mx,specialist) of
            (Nothing,_) -> Right title
            (Just (Entity xid' _), Just (Entity xid _)) | xid' == xid -> Right title
                                                        | otherwise -> Left MsgAlreadyExists
            _otherwise -> Left MsgAlreadyExists


getStaffSpecialtiesR :: DoctorId -> Handler Html
getStaffSpecialtiesR did = do

    attrib <- (unValue =<<) <$> runDB ( selectOne $ do
        x <- from $ table @DoctorPhoto
        where_ $ x ^. DoctorPhotoDoctor ==. val did
        return (x ^. DoctorPhotoAttribution) )

    specialties <- runDB $ select $ do
        x :& s <- from $ table @Specialist
            `innerJoin` table @Specialty `on` (\(x :& s) -> x ^. SpecialistSpecialty ==. s ^. SpecialtyId)
        where_ $ x ^. SpecialistDoctor ==. val did
        return (x,s)

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSpecialties
        idTabSpecialties <- newIdent
        idPanelSpecialties <- newIdent
        idFabAdd <- newIdent
        $(widgetFile "data/staff/specialties/specialties")


postDoctorDeleR :: DoctorId -> Handler Html
postDoctorDeleR did = do
    ((fr,fw),et) <- runFormPost formDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete did
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR StaffR
      _otherwise -> do

          doctor <- (second (join . unValue) <$>) <$> runDB ( selectOne $ do
              x :& h <- from $ table @Doctor
                  `leftJoin` table @DoctorPhoto `on` (\(x :& h) -> just (x ^. DoctorId) ==. h ?. DoctorPhotoDoctor)
              where_ $ x ^. DoctorId ==. val did
              return (x,h ?. DoctorPhotoAttribution) )

          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgDoctor
              idPanelDetails <- newIdent
              $(widgetFile "data/staff/doctor")


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
          $(widgetFile "data/staff/edit")


postMemberR :: DoctorId -> Handler Html
postMemberR did = do
    ((fr,fw),et) <- runFormPost $ formDoctor Nothing
    case fr of
      FormSuccess (r,mfi,attrib) -> do
          runDB $ replace did r
          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                void $ runDB $ upsert (DoctorPhoto did (fileContentType fi) bs attrib)
                    [ DoctorPhotoMime P.=. fileContentType fi
                    , DoctorPhotoPhoto P.=. bs
                    , DoctorPhotoAttribution P.=. attrib
                    ]
            Nothing -> runDB $ update $ \x -> do
                set x [DoctorPhotoAttribution =. val attrib]
                where_ $ x ^. DoctorPhotoDoctor ==. val did
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ MemberR did
      _otherwise -> do
          defaultLayout $ do
              setTitleI MsgDoctor
              $(widgetFile "data/staff/create")


getMemberR :: DoctorId -> Handler Html
getMemberR did = do

    doctor <- (second (join . unValue) <$>) <$> runDB ( selectOne $ do
        x :& h <- from $ table @Doctor
            `leftJoin` table @DoctorPhoto `on` (\(x :& h) -> just (x ^. DoctorId) ==. h ?. DoctorPhotoDoctor)
        where_ $ x ^. DoctorId ==. val did
        return (x,h ?. DoctorPhotoAttribution) )

    (fw,et) <- generateFormPost formDelete
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgDoctor
        idPanelDetails <- newIdent
        $(widgetFile "data/staff/doctor")


postStaffR :: Handler Html
postStaffR = do
    ((fr,fw),et) <- runFormPost $ formDoctor Nothing
    case fr of
      FormSuccess (r,mfi,attrib) -> do
          did <- runDB $ insert r

          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                void $ runDB $ upsert (DoctorPhoto did (fileContentType fi) bs attrib)
                    [ DoctorPhotoMime P.=. fileContentType fi
                    , DoctorPhotoPhoto P.=. bs
                    , DoctorPhotoAttribution P.=. attrib
                    ]
            Nothing -> runDB $ update $ \x -> do
                set x [DoctorPhotoAttribution =. val attrib]
                where_ $ x ^. DoctorPhotoDoctor ==. val did

          addMessageI statusSuccess MsgRecordCreated
          redirect $ DataR StaffR
      _otherwise -> defaultLayout $ do
          setTitleI MsgDoctor
          $(widgetFile "data/staff/create")


getDoctorCreateR :: Handler Html
getDoctorCreateR = do
    (fw,et) <- generateFormPost $ formDoctor Nothing
    defaultLayout $ do
        setTitleI MsgDoctor
        $(widgetFile "data/staff/create")


formDoctor :: Maybe (Entity Doctor)
           -> Html -> MForm Handler (FormResult (Doctor,Maybe FileInfo, Maybe Html), Widget)
formDoctor doctor extra = do
    rndr <- getMessageRender

    (nameR,nameV) <- md3mreq md3textField FieldSettings
        { fsLabel = SomeMessage MsgFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgFullName)]
        } (doctorName . entityVal <$> doctor)
    (mobileR,mobileV) <- md3mreq md3telField FieldSettings
        { fsLabel = SomeMessage MsgMobile
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgMobile)]
        } (doctorMobile . entityVal <$> doctor)
    (emailR,emailV) <- md3mreq md3emailField FieldSettings
        { fsLabel = SomeMessage MsgEmailAddress
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgEmailAddress)]
        } (doctorEmail . entityVal <$> doctor)
    (phoneR,phoneV) <- md3mopt md3telField FieldSettings
        { fsLabel = SomeMessage MsgPhone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgPhone)]
        } (doctorPhone . entityVal <$> doctor)

    users <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @User
        orderBy [asc (x ^. UserEmail)]
        return (x ^. UserEmail, x ^. UserId) )
        
    (userR,userV) <- md3mopt (md3selectField (optionsPairs users)) FieldSettings
        { fsLabel = SomeMessage MsgUser
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgUser)]
        } (doctorUser . entityVal <$> doctor)

    (photoR,photoV) <- md3mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    attrib <- (unValue <$>) <$> case doctor of
      Just (Entity did _) -> liftHandler $ runDB $ selectOne $ do
        x <- from $ table @DoctorPhoto
        where_ $ x ^. DoctorPhotoDoctor ==. val did
        return $ x ^. DoctorPhotoAttribution
      Nothing -> return Nothing

    (attribR,attribV) <- md3mopt md3htmlField FieldSettings
        { fsLabel = SomeMessage MsgAttribution
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgAttribution)]
        } attrib

    let r = (,,) <$> (Doctor <$> nameR <*> mobileR <*> emailR <*> phoneR <*> userR) <*> photoR <*> attribR

    idLabelPhoto <- newIdent
    idFigurePhoto <- newIdent
    idImgPhoto <- newIdent
    let w = $(widgetFile "data/staff/form")

    return (r,w)


getStaffR :: Handler Html
getStaffR = do

    doctors <- (second (join . unValue) <$>) <$> runDB ( select $ do
        x :& h <- from $ table @Doctor
            `leftJoin` table @DoctorPhoto `on` (\(x :& h) -> just (x ^. DoctorId) ==. h ?. DoctorPhotoDoctor)
        orderBy [desc (x ^. DoctorId)]
        return (x, h ?. DoctorPhotoAttribution) )

    msgs <- getMessages
    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgDoctors
        idFabAdd <- newIdent
        $(widgetFile "data/staff/doctors")


getStaffPhotoR :: DoctorId -> Handler TypedContent
getStaffPhotoR did = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @DoctorPhoto
        where_ $ x ^. DoctorPhotoDoctor ==. val did
        return x
    case photo of
      Just (Entity _ (DoctorPhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_person_FILL0_wght400_GRAD0_opsz24_svg
