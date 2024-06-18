{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo.DemoRo (fillDemoRo) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import Data.FileEmbed (embedFile)
import Data.Time
    ( utcToLocalTime, addLocalTime, nominalDay, getCurrentTimeZone, addUTCTime
    )
import Data.Time.Calendar (addGregorianYearsClip)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))
import Data.Time.LocalTime (localDay, localTimeOfDay)

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
      ), Doctor (doctorName, Doctor, doctorMobile, doctorEmail, doctorPhone, doctorUser)
    , DoctorPhoto
      ( DoctorPhoto, doctorPhotoDoctor, doctorPhotoMime, doctorPhotoPhoto
      , doctorPhotoAttribution
      )
    , Specialist
      ( Specialist, specialistDoctor, specialistSpecialty, specialistCertDate
      , specialistTitle
      ), Unit (Unit, unitName, unitSymbol, unitDescr, unitQuantity)
    , MedSign
      ( MedSign, medSignName, medSignCode, medSignDescr, medSignTag, medSignIcon
      )
    , SignTag (SignTag, signTagName, signTagGroup, signTagDescr)
    , Quantity (Quantity, quantityName, quantityDescr)
    , Record
      ( recordUser, recordSign, Record, recordTime, recordRemarks, recordDay )
    , Measurement
      ( Measurement, measurementRecord, measurementValue, measurementUnit
      , measurementName
      )
    , Normal
      ( Normal, normalName, normalLowerBound, normalUpperBound, normalUnit
      , normalSign
      )
    , Patient
      ( Patient, patientUser, patientDoctor, patientSince, patientMobile
      , patientPhone
      )
    , Chat
      ( Chat, chatUser, chatInterlocutor, chatTimemark, chatMessage, chatStatus
      , chatNotified
      )
    , ChatMessageStatus (ChatMessageStatusUnread, ChatMessageStatusRead)
    , Token (Token, tokenApi, tokenStore), apiInfoVapid, apiInfoGoogle
    , StoreType (StoreTypeGoogleSecretManager, StoreTypeDatabase)
    , Store (Store, storeKey, storeToken, storeVal)
    , UserInfo
      ( UserInfo, userInfoUser, userInfoBirthDate, userInfoGender, userInfoMobile
      , userInfoPhone
      )
    )

import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (st)

import Yesod.Auth.Email (saltPass)
import Yesod.Form.Fields (Textarea(Textarea))
import Yesod.Persist(PersistStoreWrite (insert, insert_))
import Settings (AppSettings (appDevelopment))


fillDemoRo :: MonadIO m => AppSettings -> ReaderT SqlBackend m ()
fillDemoRo appSettings = do

    tz <- liftIO getCurrentTimeZone
    now <- liftIO getCurrentTime
    let today = utctDay now
    let localNow = utcToLocalTime tz now

    if appDevelopment appSettings
        then do    
        tid <- insert Token { tokenApi = apiInfoVapid
                            , tokenStore = StoreTypeDatabase
                            }
        insert_ Store { storeToken = tid
                      , storeKey = "VAPID triple"
                      , storeVal = "(22879504107471320671126810649052900428463951865620953018730582802067053764751,106963013057532433758118925351970093795784080248924999494142080730165234056952,106312979860921952110239664650581548265214157776781431141487974369128419274671)"
                      }
        else do
        insert_ Token { tokenApi = apiInfoGoogle
                      , tokenStore = StoreTypeGoogleSecretManager
                      }
    
        insert_ Token { tokenApi = apiInfoVapid
                      , tokenStore = StoreTypeGoogleSecretManager
                      }
    
    pass1 <- liftIO $ saltPass "raduam"
    let user1 = User { userEmail = "raduam@xmail.ro"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass1
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Radu Ana-Maria"
                     , userSuperuser = False
                     , userAdmin = True
                     }

    usr1 <- insert user1

    insert_ UserPhoto { userPhotoUser = usr1
                      , userPhotoMime = "image/avif"
                      , userPhotoPhoto = $(embedFile "demo/happy-woman-gray-polo-shirt-with-pink-pin-button_53876-102858.avif")
                      , userPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]
                      }

    insert_ UserInfo { userInfoUser = usr1
                     , userInfoBirthDate = Nothing
                     , userInfoGender = Nothing
                     , userInfoMobile = Just "+18056594960"
                     , userInfoPhone = Just "+18056594960"
                     }

    pass2 <- liftIO $ saltPass "ionescuav"
    let user2 = User { userEmail = "ionescuav@xmail.ro"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass2
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Ionescu Alexandru Victor"
                     , userSuperuser = False
                     , userAdmin = False
                     }

    usr2 <- insert user2

    insert_ UserPhoto { userPhotoUser = usr2
                      , userPhotoMime = "image/avif"
                      , userPhotoPhoto = $(embedFile "demo/smiley-handsome-man-posing_23-2148911841.avif")
                      , userPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]

                      }

    pass3 <- liftIO $ saltPass "rususa"
    let user3 = User { userEmail = "rususa@xmail.ro"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass3
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Rusu Ştefan Alexandru"
                     , userSuperuser = False
                     , userAdmin = False
                     }

    usr3 <- insert user3

    insert_ UserPhoto { userPhotoUser = usr3
                      , userPhotoMime = "image/avif"
                      , userPhotoPhoto = $(embedFile "demo/doctor-smiling-offering-hand_23-2148075683.avif")
                      , userPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]

                      }

    pass4 <- liftIO $ saltPass "marini"
    let user4 = User { userEmail = "marini@xmail.ro"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass4
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Marin Ioana"
                     , userSuperuser = False
                     , userAdmin = False
                     }

    usr4 <- insert user4

    insert_ UserPhoto { userPhotoUser = usr4
                      , userPhotoMime = "image/avif"
                      , userPhotoPhoto = $(embedFile "demo/doctor-office-looking-camera_23-2147796576.avif")
                      , userPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]

                      }

    let specialty1 = Specialty { specialtyName = "Alergie și imunologie"
                               , specialtyCode = Just "ALLI"
                               , specialtyDescr = Just $ Textarea [st|Imunologia este o ramură a medicinei care acoperă studiul sistemelor imunitare din toate organismele.|]
                               , specialtyGroup = Nothing
                               }

    s1 <- insert specialty1

    let specialty2 = Specialty { specialtyName = "Anestezie"
                               , specialtyCode = Just "ANES"
                               , specialtyDescr = Just $ Textarea [st|Anestezia, anestezia sau anestezia este specialitatea medicală care se ocupă de îngrijirea perioperatorie totală a pacienților înainte, în timpul și după intervenția chirurgicală.|]
                               , specialtyGroup = Nothing
                               }

    s2 <- insert specialty2

    let specialty3 = Specialty { specialtyName = "Cardiologie"
                               , specialtyCode = Just "CARD"
                               , specialtyDescr = Just $ Textarea [st|Cardiologia este o ramură a medicinei care se ocupă de tulburările inimii și ale sistemului cardiovascular.|]
                               , specialtyGroup = Nothing
                               }

    s3 <- insert specialty3

    let specialty4 = Specialty { specialtyName = "Dermatologie"
                               , specialtyCode = Just "DERMA"
                               , specialtyDescr = Just $ Textarea [st|Dermatologia este ramura medicinei care se ocupă cu pielea. Este o specialitate cu aspecte atât medicale cât și chirurgicale. Un dermatolog este un medic specialist care gestionează boli legate de piele, păr, unghii și unele probleme cosmetice.|]
                               , specialtyGroup = Nothing
                               }

    s4 <- insert specialty4

    let doctor1 = Doctor { doctorName = "Dr. Rusu Ştefan Alexandru"
                         , doctorMobile = "+18056594960"
                         , doctorEmail = "rususa@xmail.ru"
                         , doctorPhone = Just "+18056594960"
                         , doctorUser = Just usr3
                         }
    d1 <- insert doctor1

    insert_ DoctorPhoto { doctorPhotoDoctor = d1
                        , doctorPhotoMime = "image/avif"
                        , doctorPhotoPhoto = $(embedFile "demo/doctor-smiling-offering-hand_23-2148075683.avif")
                        , doctorPhotoAttribution = Just [shamlet|
                                                                Designed by #
                                                                <a href="https://www.freepik.com/" target=_blank>
                                                                  Freepik
                                                                |]
                        }
    insert_ Specialist { specialistDoctor = d1
                       , specialistSpecialty = s1
                       , specialistTitle = "Alergologii"
                       , specialistCertDate = addGregorianYearsClip (-11) today
                       }
    insert_ Specialist { specialistDoctor = d1
                       , specialistSpecialty = s1
                       , specialistTitle = "Imunolog"
                       , specialistCertDate = addGregorianYearsClip (-10) today
                       }

    let doctor2 = Doctor { doctorName = "Dr. Marin Ioana"
                         , doctorMobile = "+12258813837"
                         , doctorEmail = "marini@xmail.ro"
                         , doctorPhone = Just "+12258813837"
                         , doctorUser = Just usr4
                         }
    d2 <- insert doctor2

    insert_ DoctorPhoto { doctorPhotoDoctor = d2
                        , doctorPhotoMime = "image/avif"
                        , doctorPhotoPhoto = $(embedFile "demo/doctor-office-looking-camera_23-2147796576.avif")
                        , doctorPhotoAttribution = Just [shamlet|
                                                                Designed by #
                                                                <a href="https://www.freepik.com/" target=_blank>
                                                                  Freepik
                                                                |]
                        }
    insert_ Specialist { specialistDoctor = d2
                       , specialistSpecialty = s2
                       , specialistTitle = "Anestezist"
                       , specialistCertDate = addGregorianYearsClip (-9) today
                       }

    let doctor3 = Doctor { doctorName = "Dr. Munteanu David"
                         , doctorMobile = "+13029222541"
                         , doctorEmail = "munteanud@xmail.ro"
                         , doctorPhone = Just "+13029222541"
                         , doctorUser = Nothing
                         }
    d3 <- insert doctor3

    insert_ DoctorPhoto { doctorPhotoDoctor = d3
                        , doctorPhotoMime = "image/avif"
                        , doctorPhotoPhoto = $(embedFile "demo/cheerful-medic-standing-with-arms-crossed_23-2147767252.avif")
                        , doctorPhotoAttribution = Just [shamlet|
                                                                Designed by #
                                                                <a href="https://www.freepik.com/" target=_blank>
                                                                  Freepik
                                                                |]
                        }
    insert_ Specialist { specialistDoctor = d3
                       , specialistSpecialty = s3
                       , specialistTitle = "Cardiolog"
                       , specialistCertDate = addGregorianYearsClip (-5) today
                       }

    let doctor4 = Doctor { doctorName = "Dr. Stoica Maria Alexandra"
                         , doctorMobile = "+17743753179"
                         , doctorEmail = "stoicama@xmail.ro"
                         , doctorPhone = Just "+17743753179"
                         , doctorUser = Nothing
                         }
    d4 <- insert doctor4

    insert_ DoctorPhoto { doctorPhotoDoctor = d4
                        , doctorPhotoMime = "image/avif"
                        , doctorPhotoPhoto = $(embedFile "demo/cheerful-pretty-doctor_23-2147648677.avif")
                        , doctorPhotoAttribution = Just [shamlet|
                                                                Designed by #
                                                                <a href="https://www.freepik.com/" target=_blank>
                                                                  Freepik
                                                                |]
                        }
    insert_ Specialist { specialistDoctor = d4
                       , specialistSpecialty = s4
                       , specialistTitle = "Dermatolog"
                       , specialistCertDate = addGregorianYearsClip (-5) today
                       }

    let quantity1 = Quantity { quantityName = "Lungime"
                             , quantityDescr = Just $ Textarea "Lungimea este o măsură a distanței"
                             }

    q1 <- insert quantity1

    let quantity2 = Quantity { quantityName = "Timp"
                             , quantityDescr = Just $ Textarea "Timpul este succesiunea continuă a existenței și a evenimentelor care au loc într-o succesiune aparent ireversibilă din trecut, prin prezent și în viitor."
                             }

    q2 <- insert quantity2

    let quantity3 = Quantity { quantityName = "Masa"
                             , quantityDescr = Just $ Textarea "Masa este o proprietate intrinsecă a unui corp"
                             }

    q3 <- insert quantity3

    let quantity4 = Quantity { quantityName = "Temperatura"
                             , quantityDescr = Just $ Textarea "Temperatura este o mărime fizică care exprimă cantitativ atributul de căldură sau răceală"
                             }

    q4 <- insert quantity4

    let quantity5 = Quantity { quantityName = "Frecvență"
                             , quantityDescr = Just $ Textarea "Frecvența este numărul de apariții ale unui eveniment care se repetă pe unitatea de timp"
                             }

    q5 <- insert quantity5

    let unit1 = Unit { unitName = "Milimetri de mercur"
                     , unitSymbol = "mmHg"
                     , unitDescr = Just $ Textarea "Un milimetru de mercur este o unitate manometrică de presiune, definită anterior ca presiunea suplimentară generată de o coloană de mercur înaltă de un milimetru și definită în prezent ca exact 133,322387415 pascali sau exact 133,322 pascali"
                     , unitQuantity = Nothing
                     }

    u1 <- insert unit1

    let unit2 = Unit { unitName = "Bătăi pe minut"
                     , unitSymbol = "bpm"
                     , unitDescr = Just $ Textarea "Frecvența cardiacă (sau pulsul) este frecvența bătăilor inimii măsurată prin numărul de contracții ale inimii pe minut"
                     , unitQuantity = Just q5
                     }

    u2 <- insert unit2

    let unit3 = Unit { unitName = "Centimetru"
                     , unitSymbol = "cm"
                     , unitDescr = Just $ Textarea "Un centimetru este o unitate de lungime în Sistemul Internațional de Unități egală cu o sutime de metru"
                     , unitQuantity = Just q1
                     }

    u3 <- insert unit3

    let unit4 = Unit { unitName = "Piciorul"
                     , unitSymbol = "ft"
                     , unitDescr = Just $ Textarea "Piciorul este o unitate de măsurare a lungimii. Este una dintre unitățile imperiale și unitățile obișnuite din SUA. Un picior conține 12 inci. Aceasta este egală cu 30,48 centimetri. Se numește picior, deoarece inițial se baza pe lungimea unui picior."
                     , unitQuantity = Just q1
                     }

    u4 <- insert unit4

    let unit5 = Unit { unitName = "Celsius"
                     , unitSymbol = "°C"
                     , unitDescr = Just $ Textarea "Gradul Celsius este unitatea de temperatură pe scara Celsius, una dintre cele două scări de temperatură utilizate în Sistemul Internațional de Unități (SI), cealaltă fiind scala Kelvin strâns legată"
                     , unitQuantity = Just q4
                     }

    u5 <- insert unit5

    let unit6 = Unit { unitName = "Fahrenheit"
                     , unitSymbol = "°F"
                     , unitDescr = Just $ Textarea "Scara Fahrenheit este o scară de temperatură bazată pe cea propusă în 1724 de fizicianul european Daniel Gabriel Fahrenheit. Folosește gradul Fahrenheit ca unitate"
                     , unitQuantity = Just q4
                     }

    u6 <- insert unit6

    let unit7 = Unit { unitName = "Respirații pe minut"
                     , unitSymbol = "r/min"
                     , unitDescr = Just $ Textarea "Frecvența respiratorie a unei persoane este de obicei măsurată în respirații pe minut"
                     , unitQuantity = Just q5
                     }

    u7 <- insert unit7

    let unit8 = Unit { unitName = "Lira"
                     , unitSymbol = "lb"
                     , unitDescr = Just $ Textarea "Lira sau lira-masa este o unitate de masă folosită atât în sistemele obișnuite de măsurare imperiale britanice, cât și în cele ale Statelor Unite."
                     , unitQuantity = Just q3
                     }

    u8 <- insert unit8

    let unit9 = Unit { unitName = "Kilogram"
                     , unitSymbol = "kg"
                     , unitDescr = Just $ Textarea "Kilogramul este unitatea de bază de masă în Sistemul Internațional de Unități (SI)"
                     , unitQuantity = Just q3
                     }

    u9 <- insert unit9

    let signTag1 = SignTag { signTagName = "Semnele vitale"
                           , signTagDescr = Just $ Textarea "Semnele vitale (cunoscute și sub numele de elemente vitale) sunt un grup de patru până la șase semne medicale cele mai importante care indică starea funcțiilor vitale (de susținere a vieții) ale corpului."
                           , signTagGroup = Nothing
                           }

    st1 <- insert signTag1

    let signTag2 = SignTag { signTagName = "Simptome"
                           , signTagDescr = Just $ Textarea "Un simptom este ceva simțit sau experimentat, cum ar fi durerea sau amețelile"
                           , signTagGroup = Nothing
                           }

    st2 <- insert signTag2

    let signTag21 = SignTag { signTagName = "Simptome specifice"
                            , signTagDescr = Just $ Textarea "Unele simptome sunt specifice, adică sunt asociate cu o singură afecțiune medicală specifică"
                            , signTagGroup = Just st2
                            }

    st21 <- insert_ signTag21

    let signTag22 = SignTag { signTagName = "Simptome nespecifice"
                            , signTagDescr = Just $ Textarea "Simptomele nespecifice, uneori numite și simptome echivoce, nu sunt specifice unei anumite afecțiuni"
                            , signTagGroup = Just st2
                            }

    st22 <- insert signTag22

    let sign1 = MedSign { medSignName = "Temperatura corpului"
                        , medSignCode = Just "TC"
                        , medSignIcon = Just "thermometer"
                        , medSignDescr = Just $ Textarea "Termoregularea este capacitatea unui organism de a-și menține temperatura corpului în anumite limite, chiar și atunci când temperatura din jur este foarte diferită."
                        , medSignTag = Just st1
                        }

    sgn1 <- insert sign1

    let normal11 = Normal { normalSign = sgn1
                          , normalName = "Valoare"
                          , normalLowerBound = 36.1
                          , normalUpperBound = 37.2
                          , normalUnit = Just u5
                          }

    insert_ normal11

    let sign2 = MedSign { medSignName = "Tensiune arteriala"
                        , medSignCode = Just "TA"
                        , medSignIcon = Just "blood_pressure"
                        , medSignDescr = Just $ Textarea "Tensiunea arterială este presiunea sângelui circulant împotriva pereților vaselor de sânge. Cea mai mare parte a acestei presiuni rezultă din inima care pompează sânge prin sistemul circulator"
                        , medSignTag = Just st1
                        }

    sgn2 <- insert sign2

    let normal21 = Normal { normalSign = sgn2
                          , normalName = "Sistolică"
                          , normalLowerBound = 90
                          , normalUpperBound = 120
                          , normalUnit = Just u1
                          }

    insert_ normal21

    let normal22 = Normal { normalSign = sgn2
                          , normalName = "Diastolică"
                          , normalLowerBound = 50
                          , normalUpperBound = 80
                          , normalUnit = Just u1
                          }

    insert_ normal22

    let normal23 = Normal { normalSign = sgn2
                          , normalName = "Puls"
                          , normalLowerBound = 60
                          , normalUpperBound = 100
                          , normalUnit = Just u1
                          }

    insert_ normal23

    let sign3 = MedSign { medSignName = "Ritmul cardiac"
                        , medSignCode = Just "RC"
                        , medSignIcon = Just "cardiology"
                        , medSignDescr = Just $ Textarea "În medicină, un puls reprezintă palparea arterială tactilă a ciclului cardiac (bătăile inimii) de către vârfurile degetelor antrenate."
                        , medSignTag = Just st1
                        }

    sgn3 <- insert sign3

    let normal31 = Normal { normalSign = sgn3
                          , normalName = "Valoare"
                          , normalLowerBound = 60
                          , normalUpperBound = 100
                          , normalUnit = Just u2
                          }

    insert_ normal31

    let sign4 = MedSign { medSignName = "Frecvența respiratorie"
                        , medSignCode = Just "FR"
                        , medSignIcon = Just "respiratory_rate"
                        , medSignDescr = Just $ Textarea "Frecvența respiratorie este ritmul cu care are loc respirația; este setat și controlat de centrul respirator al creierului. Frecvența respiratorie a unei persoane este de obicei măsurată în respirații pe minut"
                        , medSignTag = Just st1
                        }

    sgn4 <- insert sign4

    let normal41 = Normal { normalSign = sgn4
                          , normalName = "Valoare"
                          , normalLowerBound = 12
                          , normalUpperBound = 18
                          , normalUnit = Just u7
                          }

    insert_ normal41

    let sign5 = MedSign { medSignName = "Pierdere în greutate"
                        , medSignCode = Nothing
                        , medSignIcon = Just "monitor_weight_loss"
                        , medSignDescr = Just $ Textarea "Pierderea în greutate, în contextul medicinei, sănătății sau aptitudinii fizice, se referă la o reducere a masei corporale totale, printr-o pierdere medie de lichide, grăsime corporală (țesut adipos) sau masă slabă (și anume depozite minerale osoase, mușchi, tendon și alt țesut conjunctiv)"
                        , medSignTag = Just st22
                        }

    sgn5 <- insert sign5

    let normal51 = Normal { normalSign = sgn5
                          , normalName = "Valoare"
                          , normalLowerBound = 3
                          , normalUpperBound = 80
                          , normalUnit = Just u9
                          }

    insert_ normal51

    let sign6 = MedSign { medSignName = "Cefaleea"
                        , medSignCode = Nothing
                        , medSignIcon = Nothing
                        , medSignDescr = Just $ Textarea "Cefaleea, cunoscută și sub numele de cefalalgie, este simptomul durerii la nivelul feței, capului sau gâtului"
                        , medSignTag = Just st22
                        }

    sgn6 <- insert sign6

    let sign7 = MedSign { medSignName = "Durere"
                        , medSignCode = Nothing
                        , medSignIcon = Just "sick"
                        , medSignDescr = Just $ Textarea "Durerea este un sentiment supărător cauzat adesea de stimuli intensi sau dăunători"
                        , medSignTag = Just st22
                        }

    sgn7 <- insert sign7

    let sign8 = MedSign { medSignName = "Oboseală"
                        , medSignCode = Nothing
                        , medSignIcon = Nothing
                        , medSignDescr = Just $ Textarea "Oboseala descrie o stare de oboseală (care nu este somnolență) sau epuizare"
                        , medSignTag = Just st22
                        }

    sgn8 <- insert sign8

    let sign9 = MedSign { medSignName = "Anorexia"
                        , medSignCode = Nothing
                        , medSignIcon = Nothing
                        , medSignDescr = Just $ Textarea "Anorexia este un termen medical pentru pierderea poftei de mâncare"
                        , medSignTag = Just st22
                        }

    sgn9 <- insert sign9

    let sign10 = MedSign { medSignName = "Transpirații nocturne"
                         , medSignCode = Nothing
                         , medSignIcon = Nothing
                         , medSignDescr = Just $ Textarea "Transpirațiile nocturne sau hiperhidroza nocturnă reprezintă apariția repetată a transpirației excesive în timpul somnului"
                         , medSignTag = Just st22
                         }

    sgn10 <- insert sign10

    let sign11 = MedSign { medSignName = "Starea de rău"
                         , medSignCode = Nothing
                         , medSignIcon = Nothing
                         , medSignDescr = Just $ Textarea "Ca termen medical, starea de rău este un sentiment de disconfort general, neliniște sau lipsă de bunăstare și adesea primul semn al unei infecții sau al unei alte boli."
                         , medSignTag = Just st22
                         }

    sgn11 <- insert sign11

    let twoDaysBefore = addLocalTime ((-2) * nominalDay) localNow

    let patient11 = Patient { patientUser = usr1
                            , patientDoctor = d1
                            , patientSince = addUTCTime ((-7) * nominalDay) now
                            , patientMobile = Nothing
                            , patientPhone = Nothing
                            }

    insert_ patient11

    let patient12 = Patient { patientUser = usr1
                            , patientDoctor = d2
                            , patientSince = addUTCTime ((-6) * nominalDay) now
                            , patientMobile = Just "+12258813837"
                            , patientPhone = Just "+13029222541"
                            }

    insert_ patient12

    let patient21 = Patient { patientUser = usr2
                            , patientDoctor = d1
                            , patientSince = addUTCTime ((-3) * nominalDay) now
                            , patientMobile = Nothing
                            , patientPhone = Nothing
                            }

    insert_ patient21

    let patient22 = Patient { patientUser = usr2
                            , patientDoctor = d2
                            , patientSince = addUTCTime ((-2) * nominalDay) now
                            , patientMobile = Nothing
                            , patientPhone = Nothing
                            }

    insert_ patient22
    
    let record121 = Record { recordUser = usr1
                           , recordSign = sgn2
                           , recordDay = localDay twoDaysBefore
                           , recordTime = localTimeOfDay twoDaysBefore
                           , recordRemarks = Nothing
                           }

    r121 <- insert record121

    let measurement1211 = Measurement { measurementRecord = r121
                                      , measurementValue = 128
                                      , measurementName = "Sistolică"
                                      , measurementUnit = Just u1
                                      }

    m1211 <- insert measurement1211

    let measurement1212 = Measurement { measurementRecord = r121
                                      , measurementValue = 70
                                      , measurementName = "Diastolică"
                                      , measurementUnit = Just u1
                                      }

    m1212 <- insert measurement1212

    let measurement1213 = Measurement { measurementRecord = r121
                                      , measurementValue = 60
                                      , measurementName = "Puls"
                                      , measurementUnit = Just u2
                                      }

    m1213 <- insert measurement1213

    let record14 = Record { recordUser = usr1
                          , recordSign = sgn4
                          , recordDay = localDay twoDaysBefore
                          , recordTime = localTimeOfDay twoDaysBefore
                          , recordRemarks = Nothing
                          }

    r14 <- insert record14

    let measurement141 = Measurement { measurementRecord = r14
                                     , measurementValue = 15
                                     , measurementName = "Valoare"
                                     , measurementUnit = Just u7
                                     }

    m141 <- insert measurement141

    let record13 = Record { recordUser = usr1
                          , recordSign = sgn3
                          , recordDay = localDay twoDaysBefore
                          , recordTime = localTimeOfDay twoDaysBefore
                          , recordRemarks = Nothing
                          }

    r13 <- insert record13

    let measurement131 = Measurement { measurementRecord = r13
                                     , measurementValue = 72
                                     , measurementName = "Valoare"
                                     , measurementUnit = Just u2
                                     }

    m131 <- insert measurement131

    let record221 = Record { recordUser = usr2
                           , recordSign = sgn2
                           , recordDay = localDay twoDaysBefore
                           , recordTime = localTimeOfDay twoDaysBefore
                           , recordRemarks = Nothing
                           }

    r221 <- insert record221

    let measurement2211 = Measurement { measurementRecord = r221
                                      , measurementValue = 128
                                      , measurementName = "Sistolică"
                                      , measurementUnit = Just u1
                                      }

    m2211 <- insert measurement2211

    let measurement2212 = Measurement { measurementRecord = r221
                                      , measurementValue = 70
                                      , measurementName = "Diastolică"
                                      , measurementUnit = Just u1
                                      }

    m2212 <- insert measurement2212

    let measurement2213 = Measurement { measurementRecord = r221
                                      , measurementValue = 60
                                      , measurementName = "Puls"
                                      , measurementUnit = Just u2
                                      }

    m2213 <- insert measurement2213

    let record24 = Record { recordUser = usr2
                          , recordSign = sgn4
                          , recordDay = localDay twoDaysBefore
                          , recordTime = localTimeOfDay twoDaysBefore
                          , recordRemarks = Nothing
                          }

    r24 <- insert record24

    let measurement241 = Measurement { measurementRecord = r24
                                     , measurementValue = 15
                                     , measurementName = "Valoare"
                                     , measurementUnit = Just u7
                                     }

    m241 <- insert measurement241

    let record23 = Record { recordUser = usr2
                          , recordSign = sgn3
                          , recordDay = localDay twoDaysBefore
                          , recordTime = localTimeOfDay twoDaysBefore
                          , recordRemarks = Nothing
                          }

    r23 <- insert record23

    let measurement231 = Measurement { measurementRecord = r23
                                     , measurementValue = 72
                                     , measurementName = "Valoare"
                                     , measurementUnit = Just u2
                                     }

    m231 <- insert measurement231



    
    let yesterday = addLocalTime ((-1) * nominalDay) localNow

    let record12 = Record { recordUser = usr1
                          , recordSign = sgn2
                          , recordDay = localDay yesterday
                          , recordTime = localTimeOfDay yesterday
                          , recordRemarks = Nothing
                          }

    r12 <- insert record12

    let measurement121 = Measurement { measurementRecord = r12
                                     , measurementValue = 127
                                     , measurementName = "Sistolică"
                                     , measurementUnit = Just u1
                                     }

    m121 <- insert measurement121

    let measurement122 = Measurement { measurementRecord = r12
                                     , measurementValue = 71
                                     , measurementName = "Diastolică"
                                     , measurementUnit = Just u1
                                     }

    m122 <- insert measurement122

    let measurement123 = Measurement { measurementRecord = r12
                                     , measurementValue = 64
                                     , measurementName = "Puls"
                                     , measurementUnit = Just u2
                                     }

    m123 <- insert measurement123

    let record14 = Record { recordUser = usr1
                          , recordSign = sgn4
                          , recordDay = localDay yesterday
                          , recordTime = localTimeOfDay yesterday
                          , recordRemarks = Nothing
                          }

    r14 <- insert record14

    let measurement141 = Measurement { measurementRecord = r14
                                     , measurementValue = 17
                                     , measurementName = "Valoare"
                                     , measurementUnit = Just u7
                                     }

    m141 <- insert measurement141

    let record13 = Record { recordUser = usr1
                          , recordSign = sgn3
                          , recordDay = localDay yesterday
                          , recordTime = localTimeOfDay yesterday
                          , recordRemarks = Nothing
                          }

    r13 <- insert record13

    let measurement131 = Measurement { measurementRecord = r13
                                     , measurementValue = 69
                                     , measurementName = "Valoare"
                                     , measurementUnit = Just u2
                                     }

    m131 <- insert measurement131

    let record22 = Record { recordUser = usr2
                          , recordSign = sgn2
                          , recordDay = localDay yesterday
                          , recordTime = localTimeOfDay yesterday
                          , recordRemarks = Nothing
                          }

    r22 <- insert record22

    let measurement221 = Measurement { measurementRecord = r22
                                     , measurementValue = 127
                                     , measurementName = "Sistolică"
                                     , measurementUnit = Just u1
                                     }

    m221 <- insert measurement221

    let measurement222 = Measurement { measurementRecord = r22
                                     , measurementValue = 71
                                     , measurementName = "Diastolică"
                                     , measurementUnit = Just u1
                                     }

    m222 <- insert measurement222

    let measurement223 = Measurement { measurementRecord = r22
                                     , measurementValue = 64
                                     , measurementName = "Puls"
                                     , measurementUnit = Just u2
                                     }

    m223 <- insert measurement223

    let record24 = Record { recordUser = usr2
                          , recordSign = sgn4
                          , recordDay = localDay yesterday
                          , recordTime = localTimeOfDay yesterday
                          , recordRemarks = Nothing
                          }

    r24 <- insert record24

    let measurement241 = Measurement { measurementRecord = r24
                                     , measurementValue = 17
                                     , measurementName = "Valoare"
                                     , measurementUnit = Just u7
                                     }

    m241 <- insert measurement241

    let record23 = Record { recordUser = usr2
                          , recordSign = sgn3
                          , recordDay = localDay yesterday
                          , recordTime = localTimeOfDay yesterday
                          , recordRemarks = Nothing
                          }

    r23 <- insert record23

    let measurement231 = Measurement { measurementRecord = r23
                                     , measurementValue = 69
                                     , measurementName = "Valoare"
                                     , measurementUnit = Just u2
                                     }

    m231 <- insert measurement231

    let chat131 = Chat { chatUser = usr1
                       , chatInterlocutor = usr3
                       , chatTimemark = addUTCTime ((-1) * nominalDay) now
                       , chatMessage = "Bună, Dr. Rusu Ştefan Alexandru"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat131

    let chat311 = Chat { chatUser = usr3
                       , chatInterlocutor = usr1
                       , chatTimemark = addUTCTime 30 (chatTimemark chat131)
                       , chatMessage = "Salut, Radu Ana-Maria"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat311

    let chat312 = Chat { chatUser = usr3
                       , chatInterlocutor = usr1
                       , chatTimemark = addUTCTime 60 (chatTimemark chat311)
                       , chatMessage = "Ce mai faci?"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat312

    let chat132 = Chat { chatUser = usr1
                       , chatInterlocutor = usr3
                       , chatTimemark = addUTCTime 65 (chatTimemark chat312)
                       , chatMessage = "În general, este în regulă, dar sunt îngrijorată de tensiunea arterială."
                       , chatStatus = ChatMessageStatusUnread
                       , chatNotified = True
                       }

    insert_ chat132

    let chat321 = Chat { chatUser = usr3
                       , chatInterlocutor = usr2
                       , chatTimemark = addUTCTime ((-2) * nominalDay) now
                       , chatMessage = "Bună, Alexandru."
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat321

    let chat231 = Chat { chatUser = usr2
                       , chatInterlocutor = usr3
                       , chatTimemark = addUTCTime 60 (chatTimemark chat321)
                       , chatMessage = "Bună ziua, Dr. Rusu Ştefan Alexandru"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat231

    let chat322 = Chat { chatUser = usr3
                       , chatInterlocutor = usr2
                       , chatTimemark = addUTCTime 10 (chatTimemark chat231)
                       , chatMessage = "Totul e în regulă?"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat322

    let chat232 = Chat { chatUser = usr2
                       , chatInterlocutor = usr3
                       , chatTimemark = addUTCTime 30 (chatTimemark chat322)
                       , chatMessage = "Da totul este bine."
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat232

    let chat233 = Chat { chatUser = usr2
                       , chatInterlocutor = usr3
                       , chatTimemark = addUTCTime ((-1) * nominalDay) now
                       , chatMessage = "Bună ziua, Dr. Rusu Ştefan Alexandru"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }
    insert_ chat233

    let chat323 = Chat { chatUser = usr3
                       , chatInterlocutor = usr2
                       , chatTimemark = addUTCTime 65 (chatTimemark chat233)
                       , chatMessage = "Buna Alexandru"
                       , chatStatus = ChatMessageStatusUnread
                       , chatNotified = True
                       }

    insert_ chat323

    let chat141 = Chat { chatUser = usr1
                       , chatInterlocutor = usr4
                       , chatTimemark = addUTCTime ((-1) * nominalDay) now
                       , chatMessage = "Bună, dr. Marin Ioana"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat141

    let chat411 = Chat { chatUser = usr4
                       , chatInterlocutor = usr1
                       , chatTimemark = addUTCTime 30 (chatTimemark chat141)
                       , chatMessage = "Salut, Radu Ana-Maria"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat411

    let chat412 = Chat { chatUser = usr4
                       , chatInterlocutor = usr1
                       , chatTimemark = addUTCTime 60 (chatTimemark chat411)
                       , chatMessage = "Ce mai faci?"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat412

    let chat142 = Chat { chatUser = usr1
                       , chatInterlocutor = usr4
                       , chatTimemark = addUTCTime 65 (chatTimemark chat412)
                       , chatMessage = "În general, este în regulă, dar sunt îngrijorat de tensiunea arterială."
                       , chatStatus = ChatMessageStatusUnread
                       , chatNotified = True
                       }

    insert_ chat142

    let chat421 = Chat { chatUser = usr4
                       , chatInterlocutor = usr2
                       , chatTimemark = addUTCTime ((-2) * nominalDay) now
                       , chatMessage = "Bună, Alexandru."
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat421

    let chat241 = Chat { chatUser = usr2
                       , chatInterlocutor = usr4
                       , chatTimemark = addUTCTime 60 (chatTimemark chat421)
                       , chatMessage = "Bună ziua, dr. Marin Ioana"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat241

    let chat422 = Chat { chatUser = usr4
                       , chatInterlocutor = usr2
                       , chatTimemark = addUTCTime 10 (chatTimemark chat241)
                       , chatMessage = "Totul este în regulă?"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat422

    let chat242 = Chat { chatUser = usr2
                       , chatInterlocutor = usr4
                       , chatTimemark = addUTCTime 30 (chatTimemark chat422)
                       , chatMessage = "Da totul este bine."
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat242

    let chat243 = Chat { chatUser = usr2
                       , chatInterlocutor = usr4
                       , chatTimemark = addUTCTime ((-1) * nominalDay) now
                       , chatMessage = "Bună ziua, dr. Marin Ioana"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }
    insert_ chat243

    let chat423 = Chat { chatUser = usr4
                       , chatInterlocutor = usr2
                       , chatTimemark = addUTCTime 65 (chatTimemark chat243)
                       , chatMessage = "Buna Alexandru"
                       , chatStatus = ChatMessageStatusUnread
                       , chatNotified = True
                       }

    insert_ chat423

    return ()
