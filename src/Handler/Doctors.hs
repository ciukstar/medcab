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
  , getDoctorSpecialtiesR
  , getDoctorSpecialtyCreateR
  , postDoctorSpecialtiesR
  , getSpecialistR
  , postSpecialistDeleR
  , getSpecialistEditR
  , postSpecialistR
  ) where

import Control.Monad (void, join)
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
import Material3
    ( md3textField, md3telField, md3emailField, md3selectField, md3htmlField )
import Handler.Menu (menu)
import Foundation
    ( Handler, Widget, Form
    , Route (StaticR, AuthR, AccountR, AccountPhotoR, DataR)
    , DataR
      ( DoctorCreateR, DoctorsR, DoctorPhotoR, DoctorR, DoctorDeleR, DoctorEditR
      , DoctorSpecialtiesR, DoctorSpecialtyCreateR, SpecialistR
      , SpecialistDeleR, SpecialistEditR
      )
    , AppMessage
      ( MsgDoctors, MsgUserAccount, MsgSignOut, MsgSignIn, MsgPhoto, MsgEdit
      , MsgNoDoctorsYet, MsgSave, MsgCancel, MsgFullName, MsgMobile, MsgDetails
      , MsgEmailAddress, MsgSpecialization, MsgDoctor, MsgBack, MsgRecordCreated
      , MsgEdit, MsgDele, MsgRecordDeleted, MsgDeleteAreYouSure, MsgConfirmPlease
      , MsgRecordEdited, MsgSpecialties, MsgTabs, MsgSpecialty, MsgNoSpecialtiesYet
      , MsgAttribution, MsgSpecialtyTitle, MsgSpecializations, MsgCertificateDate
      , MsgInvalidFormData
      )
    )
import Model
    ( AvatarColor (AvatarColorLight)
    , Doctor (Doctor, doctorName, doctorMobile, doctorEmail)
    , DoctorId, DoctorPhoto (DoctorPhoto)
    , EntityField
      ( DoctorPhotoDoctor, DoctorPhotoMime, DoctorPhotoPhoto, DoctorId
      , SpecialtyName, SpecialtyId, SpecialistDoctor, SpecialistSpecialty
      , DoctorPhotoAttribution, SpecialistId
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
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, fileSourceByteString
    , FileInfo (fileContentType), SomeMessage (SomeMessage), getMessageRender
    , addMessageI, setUltDestCurrent, MonadHandler (liftHandler), getMessages
    , whamlet
    )
import Yesod.Core.Handler (redirect)
import Yesod.Core.Content (TypedContent (TypedContent), ToContent (toContent))
import Yesod.Form.Fields (fileField, optionsPairs, dayField)
import Yesod.Form.Functions (generateFormPost, mreq, mopt, runFormPost)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput, fvId)
    )
import Yesod.Persist (YesodPersist (runDB))
import Data.Bifunctor (Bifunctor(second))


postSpecialistDeleR :: DoctorId -> SpecialtyId -> SpecialistId -> Handler Html
postSpecialistDeleR did sid xid = do
    ((fr,_),_) <- runFormPost formSpecialistDele
    case fr of
      FormSuccess () -> do
          runDB $ delete xid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ DoctorSpecialtiesR did
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ SpecialistR did sid xid


formSpecialistDele :: Form ()
formSpecialistDele extra = return (FormSuccess (),[whamlet|#{extra}|])


postSpecialistR :: DoctorId -> SpecialtyId -> SpecialistId -> Handler Html
postSpecialistR did sid xid = do
    ((fr,fw),et) <- runFormPost $ formSpecialty did Nothing
    case fr of
      FormSuccess r -> do
          runDB $ replace xid r
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ DoctorSpecialtiesR did
      _otherwise -> defaultLayout $ do
              setTitleI MsgSpecialization
              $(widgetFile "data/doctors/specialties/edit")



getSpecialistEditR :: DoctorId -> SpecialtyId -> SpecialistId -> Handler Html
getSpecialistEditR did sid xid = do
    specialist <- runDB $ selectOne $ do
        x <- from $ table @Specialist
        where_ $ x ^. SpecialistId ==. val xid
        return x
    (fw,et) <- generateFormPost $ formSpecialty did specialist
    defaultLayout $ do
        setTitleI MsgSpecialization
        $(widgetFile "data/doctors/specialties/edit")


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
        $(widgetFile "data/doctors/specialties/specialty")


postDoctorSpecialtiesR :: DoctorId -> Handler Html
postDoctorSpecialtiesR did = do
    ((fr,fw),et) <- runFormPost $ formSpecialty did Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordCreated
          redirect $ DataR $ DoctorSpecialtiesR did
      _otherwise -> do
          defaultLayout $ do
              setTitleI MsgSpecialty
              $(widgetFile "data/doctors/specialties/create")


getDoctorSpecialtyCreateR :: DoctorId -> Handler Html
getDoctorSpecialtyCreateR did = do
    (fw,et) <- generateFormPost $ formSpecialty did Nothing
    defaultLayout $ do
        setTitleI MsgSpecialty
        $(widgetFile "data/doctors/specialties/create")
        

formSpecialty :: DoctorId -> Maybe (Entity Specialist)
              -> Form Specialist
formSpecialty did specialist extra = do
    rndr <- getMessageRender
    specs <- liftHandler $ ((\s -> (specialtyName . entityVal $ s,entityKey s)) <$>) <$> runDB ( select $ do
        x <- from $ table @Specialty
        orderBy [asc (x ^. SpecialtyName)]
        return x )
        
    (specR,specV) <- mreq (md3selectField (optionsPairs specs)) FieldSettings
        { fsLabel = SomeMessage MsgSpecialty
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgSpecialty)]
        } (specialistSpecialty . entityVal <$> specialist)

    (titleR,titleV) <- mreq md3textField FieldSettings
        { fsLabel = SomeMessage MsgSpecialtyTitle
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgSpecialtyTitle)]
        } (specialistTitle . entityVal <$> specialist)

    (certR,certV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgSpecialty
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgSpecialty)]
        } (specialistCertDate . entityVal <$> specialist)

    let r = Specialist did <$> specR <*> titleR <*> certR
    let w = $(widgetFile "data/doctors/specialties/form")
    return (r,w)


getDoctorSpecialtiesR :: DoctorId -> Handler Html
getDoctorSpecialtiesR did = do
    
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
        $(widgetFile "data/doctors/specialties/specialties")


postDoctorDeleR :: DoctorId -> Handler Html
postDoctorDeleR did = do
    ((fr,fw),et) <- runFormPost formDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete did
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR DoctorsR
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
          redirect $ DataR $ DoctorR did
      _otherwise -> do
          defaultLayout $ do
              setTitleI MsgDoctor
              $(widgetFile "data/doctors/create")


getDoctorR :: DoctorId -> Handler Html
getDoctorR did = do
    
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
        $(widgetFile "data/doctors/doctor")


postDoctorsR :: Handler Html
postDoctorsR = do
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
           -> Html -> MForm Handler (FormResult (Doctor,Maybe FileInfo, Maybe Html), Widget)
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
                
    (photoR,photoV) <- mopt fileField FieldSettings
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
        
    (attribR,attribV) <- mopt md3htmlField FieldSettings
        { fsLabel = SomeMessage MsgAttribution
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",rndr MsgAttribution)]
        } attrib

    let r = (,,) <$> (Doctor <$> nameR <*> mobileR <*> emailR) <*> photoR <*> attribR

    idLabelPhoto <- newIdent
    idFigurePhoto <- newIdent
    idImgPhoto <- newIdent
    let w = $(widgetFile "data/doctors/form")
    
    return (r,w)


getDoctorsR :: Handler Html
getDoctorsR = do
    user <- maybeAuth

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
        $(widgetFile "data/doctors/doctors")


getDoctorPhotoR :: DoctorId -> Handler TypedContent
getDoctorPhotoR did = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @DoctorPhoto
        where_ $ x ^. DoctorPhotoDoctor ==. val did
        return x
    case photo of
      Just (Entity _ (DoctorPhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_person_FILL0_wght400_GRAD0_opsz24_svg
