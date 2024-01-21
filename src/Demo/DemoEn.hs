{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Demo.DemoEn (fillDemoEn) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.SqlBackend (SqlBackend)
import Model
    ( Specialty
      ( Specialty, specialtyName, specialtyCode, specialtyDescr, specialtyGroup )
    )
import Text.Shakespeare.Text (st)
import Yesod.Form.Fields (Textarea(Textarea))
import Yesod.Persist(PersistStoreWrite (insert))


fillDemoEn :: MonadIO m => ReaderT SqlBackend m ()
fillDemoEn = do
    let specialty1 = Specialty { specialtyName = "Allergists/Immunologists"
                               , specialtyCode = Just "ALLI"
                               , specialtyDescr = Just $ Textarea [st|
They treat immune system disorders such as asthma, eczema, food allergies, insect sting allergies, and some autoimmune diseases.
                                                                     |]
                               , specialtyGroup = Nothing
                               }

    s1 <- insert specialty1
    
    let specialty2 = Specialty { specialtyName = "Anesthesiologists"
                               , specialtyCode = Just "ANES"
                               , specialtyDescr = Just $ Textarea [st|
These doctors give you drugs to numb your pain or to put you under during surgery, childbirth, or other procedures. They monitor your vital signs while you’re under anesthesia.
                                                                     |]
                               , specialtyGroup = Nothing
                               }

    s2 <- insert specialty2
             
    return ()
