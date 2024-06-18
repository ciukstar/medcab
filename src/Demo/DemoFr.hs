{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo.DemoFr (fillDemoFr) where

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


fillDemoFr :: MonadIO m => AppSettings -> ReaderT SqlBackend m ()
fillDemoFr appSettings = do

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
    
    pass1 <- liftIO $ saltPass "bernardj"
    let user1 = User { userEmail = "bernardj@xmail.fr"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass1
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Bernard Jade"
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

    pass2 <- liftIO $ saltPass "thomasgr"
    let user2 = User { userEmail = "thomasgr@xmail.fr"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass2
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Thomas Gabriel Raphaël"
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

    pass3 <- liftIO $ saltPass "richardal"
    let user3 = User { userEmail = "richardal@xmail.fr"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass3
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Richard Arthur Louis"
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

    pass4 <- liftIO $ saltPass "duboisaa"
    let user4 = User { userEmail = "duboisaa@xmail.fr"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass4
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Dubois Alice Ambre"
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

    let specialty1 = Specialty { specialtyName = "Allergie et immunologie"
                               , specialtyCode = Just "ALLI"
                               , specialtyDescr = Just $ Textarea [st|L'immunologie est une branche de la médecine qui couvre l'étude du système immunitaire de tous les organismes.|]
                               , specialtyGroup = Nothing
                               }

    s1 <- insert specialty1

    let specialty2 = Specialty { specialtyName = "Anesthésiologie"
                               , specialtyCode = Just "ANES"
                               , specialtyDescr = Just $ Textarea [st|L'anesthésiologie ou anesthésie est la spécialité médicale concernée par la prise en charge périopératoire totale des patients avant, pendant et après la chirurgie.|]
                               , specialtyGroup = Nothing
                               }

    s2 <- insert specialty2

    let specialty3 = Specialty { specialtyName = "Cardiologie"
                               , specialtyCode = Just "CARD"
                               , specialtyDescr = Just $ Textarea [st|La cardiologie est une branche de la médecine qui traite des troubles du cœur et du système cardiovasculaire.|]
                               , specialtyGroup = Nothing
                               }

    s3 <- insert specialty3

    let specialty4 = Specialty { specialtyName = "Dermatologie"
                               , specialtyCode = Just "DERMA"
                               , specialtyDescr = Just $ Textarea [st|La dermatologie est la branche de la médecine qui s'occupe de la peau. C'est une spécialité avec des aspects à la fois médicaux et chirurgicaux. Un dermatologue est un médecin spécialiste qui prend en charge les maladies liées à la peau, aux cheveux, aux ongles et certains problèmes esthétiques.|]
                               , specialtyGroup = Nothing
                               }

    s4 <- insert specialty4

    let doctor1 = Doctor { doctorName = "Dr Richard Arthur Louis"
                         , doctorMobile = "+18056594960"
                         , doctorEmail = "richardal@xmail.fr"
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
                       , specialistTitle = "Allergologues"
                       , specialistCertDate = addGregorianYearsClip (-11) today
                       }
    insert_ Specialist { specialistDoctor = d1
                       , specialistSpecialty = s1
                       , specialistTitle = "Immunologue"
                       , specialistCertDate = addGregorianYearsClip (-10) today
                       }

    let doctor2 = Doctor { doctorName = "Dr Dubois Alice Ambre"
                         , doctorMobile = "+12258813837"
                         , doctorEmail = "duboisaa@xmail.fr"
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
                       , specialistTitle = "Anesthésiste"
                       , specialistCertDate = addGregorianYearsClip (-9) today
                       }

    let doctor3 = Doctor { doctorName = "Dr Laurent Adam"
                         , doctorMobile = "+13029222541"
                         , doctorEmail = "laurenta@xmail.fr"
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
                       , specialistTitle = "Cardiologue"
                       , specialistCertDate = addGregorianYearsClip (-5) today
                       }

    let doctor4 = Doctor { doctorName = "Dr Durand Jules"
                         , doctorMobile = "+17743753179"
                         , doctorEmail = "durandj@xmail.fr"
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
                       , specialistTitle = "Dermatologue"
                       , specialistCertDate = addGregorianYearsClip (-5) today
                       }

    let quantity1 = Quantity { quantityName = "Longueur"
                             , quantityDescr = Just $ Textarea "La longueur est une mesure de distance"
                             }

    q1 <- insert quantity1

    let quantity2 = Quantity { quantityName = "Temps"
                             , quantityDescr = Just $ Textarea "Le temps est la séquence continue d'existences et d'événements qui se produisent dans une succession apparemment irréversible du passé, à travers le présent et dans le futur."
                             }

    q2 <- insert quantity2

    let quantity3 = Quantity { quantityName = "Masse"
                             , quantityDescr = Just $ Textarea "La masse est une propriété intrinsèque d'un corps"
                             }

    q3 <- insert quantity3

    let quantity4 = Quantity { quantityName = "Température"
                             , quantityDescr = Just $ Textarea "La température est une grandeur physique qui exprime quantitativement l'attribut de chaleur ou de froid."
                             }

    q4 <- insert quantity4

    let quantity5 = Quantity { quantityName = "Fréquence"
                             , quantityDescr = Just $ Textarea "La fréquence est le nombre d'occurrences d'un événement répétitif par unité de temps"
                             }

    q5 <- insert quantity5

    let unit1 = Unit { unitName = "Millimètres de mercure"
                     , unitSymbol = "mmHg"
                     , unitDescr = Just $ Textarea "Un millimètre de mercure est une unité manométrique de pression, autrefois définie comme la pression supplémentaire générée par une colonne de mercure d'un millimètre de haut, et actuellement définie comme exactement 133,322387415 pascals ou exactement 133,322 pascals."
                     , unitQuantity = Nothing
                     }

    u1 <- insert unit1

    let unit2 = Unit { unitName = "Battements par minute"
                     , unitSymbol = "b/min"
                     , unitDescr = Just $ Textarea "La fréquence cardiaque (ou fréquence du pouls) est la fréquence des battements cardiaques mesurée par le nombre de contractions du cœur par minute."
                     , unitQuantity = Just q5
                     }

    u2 <- insert unit2

    let unit3 = Unit { unitName = "Centimètre"
                     , unitSymbol = "cm"
                     , unitDescr = Just $ Textarea "Un centimètre est une unité de longueur dans le Système international d'unités égale à un centième de mètre."
                     , unitQuantity = Just q1
                     }

    u3 <- insert unit3

    let unit4 = Unit { unitName = "Pied"
                     , unitSymbol = "pi"
                     , unitDescr = Just $ Textarea "Le pied est une unité de mesure de longueur. C'est l'une des unités impériales et des unités usuelles américaines. Un pied contient 12 pouces. Cela équivaut à 30,48 centimètres. On l'appelle pied, car à l'origine, il était basé sur la longueur d'un pied."
                     , unitQuantity = Just q1
                     }

    u4 <- insert unit4

    let unit5 = Unit { unitName = "Celsius"
                     , unitSymbol = "°C"
                     , unitDescr = Just $ Textarea "Le degré Celsius est l'unité de température sur l'échelle Celsius, l'une des deux échelles de température utilisées dans le Système international d'unités (SI), l'autre étant l'échelle Kelvin étroitement liée."
                     , unitQuantity = Just q4
                     }

    u5 <- insert unit5

    let unit6 = Unit { unitName = "Fahrenheit"
                     , unitSymbol = "°F"
                     , unitDescr = Just $ Textarea "L'échelle Fahrenheit est une échelle de température basée sur celle proposée en 1724 par le physicien européen Daniel Gabriel Fahrenheit. Il utilise le degré Fahrenheit comme unité"
                     , unitQuantity = Just q4
                     }

    u6 <- insert unit6

    let unit7 = Unit { unitName = "Respirations par minute"
                     , unitSymbol = "respirations/minute"
                     , unitDescr = Just $ Textarea "La fréquence respiratoire d'une personne est généralement mesurée en respirations par minute"
                     , unitQuantity = Just q5
                     }

    u7 <- insert unit7

    let unit8 = Unit { unitName = "Livre"
                     , unitSymbol = "lb"
                     , unitDescr = Just $ Textarea "La livre ou livre-masse est une unité de masse utilisée à la fois dans les systèmes de mesure impériaux britanniques et américains."
                     , unitQuantity = Just q3
                     }

    u8 <- insert unit8

    let unit9 = Unit { unitName = "Kilogramme"
                     , unitSymbol = "kg"
                     , unitDescr = Just $ Textarea "Le kilogramme est l'unité de masse de base dans le Système international d'unités (SI)"
                     , unitQuantity = Just q3
                     }

    u9 <- insert unit9

    let signTag1 = SignTag { signTagName = "Signes vitaux"
                           , signTagDescr = Just $ Textarea "Les signes vitaux sont un groupe de quatre à six signes médicaux les plus cruciaux qui indiquent l'état des fonctions vitales (de maintien de la vie) du corps."
                           , signTagGroup = Nothing
                           }

    st1 <- insert signTag1

    let signTag2 = SignTag { signTagName = "Symptômes"
                           , signTagDescr = Just $ Textarea "Un symptôme est quelque chose de ressenti ou d’expérimenté, comme une douleur ou des étourdissements"
                           , signTagGroup = Nothing
                           }

    st2 <- insert signTag2

    let signTag21 = SignTag { signTagName = "Symptômes spécifiques"
                            , signTagDescr = Just $ Textarea "Certains symptômes sont spécifiques, c'est-à-dire qu'ils sont associés à une seule condition médicale spécifique."
                            , signTagGroup = Just st2
                            }

    st21 <- insert_ signTag21

    let signTag22 = SignTag { signTagName = "Symptômes non spécifiques"
                            , signTagDescr = Just $ Textarea "Les symptômes non spécifiques, parfois également appelés symptômes équivoques, ne sont pas spécifiques à une affection particulière."
                            , signTagGroup = Just st2
                            }

    st22 <- insert signTag22

    let sign1 = MedSign { medSignName = "Température corporelle"
                        , medSignCode = Just "TC"
                        , medSignIcon = Just "thermometer"
                        , medSignDescr = Just $ Textarea "La thermorégulation est la capacité d'un organisme à maintenir sa température corporelle dans certaines limites, même lorsque la température ambiante est très différente."
                        , medSignTag = Just st1
                        }

    sgn1 <- insert sign1

    let normal11 = Normal { normalSign = sgn1
                          , normalName = "Value"
                          , normalLowerBound = 36.1
                          , normalUpperBound = 37.2
                          , normalUnit = Just u5
                          }

    insert_ normal11

    let sign2 = MedSign { medSignName = "Pression artérielle"
                        , medSignCode = Just "PA"
                        , medSignIcon = Just "blood_pressure"
                        , medSignDescr = Just $ Textarea "La pression artérielle est la pression du sang circulant contre les parois des vaisseaux sanguins. La majeure partie de cette pression résulte du fait que le cœur pompe le sang dans le système circulatoire."
                        , medSignTag = Just st1
                        }

    sgn2 <- insert sign2

    let normal21 = Normal { normalSign = sgn2
                          , normalName = "Systolique"
                          , normalLowerBound = 90
                          , normalUpperBound = 120
                          , normalUnit = Just u1
                          }

    insert_ normal21

    let normal22 = Normal { normalSign = sgn2
                          , normalName = "Diastolique"
                          , normalLowerBound = 50
                          , normalUpperBound = 80
                          , normalUnit = Just u1
                          }

    insert_ normal22

    let normal23 = Normal { normalSign = sgn2
                          , normalName = "Pouls"
                          , normalLowerBound = 60
                          , normalUpperBound = 100
                          , normalUnit = Just u1
                          }

    insert_ normal23

    let sign3 = MedSign { medSignName = "Rythme cardiaque"
                        , medSignCode = Just "RC"
                        , medSignIcon = Just "cardiology"
                        , medSignDescr = Just $ Textarea "En médecine, un pouls représente la palpation artérielle tactile du cycle cardiaque (battement cardiaque) par le bout des doigts entraînés."
                        , medSignTag = Just st1
                        }

    sgn3 <- insert sign3

    let normal31 = Normal { normalSign = sgn3
                          , normalName = "Valeur"
                          , normalLowerBound = 60
                          , normalUpperBound = 100
                          , normalUnit = Just u2
                          }

    insert_ normal31

    let sign4 = MedSign { medSignName = "Fréquence respiratoire"
                        , medSignCode = Just "FR"
                        , medSignIcon = Just "respiratory_rate"
                        , medSignDescr = Just $ Textarea "La fréquence respiratoire est la vitesse à laquelle la respiration se produit ; il est défini et contrôlé par le centre respiratoire du cerveau. La fréquence respiratoire d'une personne est généralement mesurée en respirations par minute"
                        , medSignTag = Just st1
                        }

    sgn4 <- insert sign4

    let normal41 = Normal { normalSign = sgn4
                          , normalName = "Valeur"
                          , normalLowerBound = 12
                          , normalUpperBound = 18
                          , normalUnit = Just u7
                          }

    insert_ normal41

    let sign5 = MedSign { medSignName = "Perte de poids"
                        , medSignCode = Nothing
                        , medSignIcon = Just "monitor_weight_loss"
                        , medSignDescr = Just $ Textarea "La perte de poids, dans le contexte de la médecine, de la santé ou de la forme physique, fait référence à une réduction de la masse corporelle totale, par une perte moyenne de liquide, de graisse corporelle (tissu adipeux) ou de masse maigre (à savoir les dépôts minéraux osseux, musculaires, tendon et autres tissus conjonctifs)"
                        , medSignTag = Just st22
                        }

    sgn5 <- insert sign5

    let normal51 = Normal { normalSign = sgn5
                          , normalName = "Valeur"
                          , normalLowerBound = 3
                          , normalUpperBound = 80
                          , normalUnit = Just u9
                          }

    insert_ normal51

    let sign6 = MedSign { medSignName = "Mal de tête"
                        , medSignCode = Nothing
                        , medSignIcon = Nothing
                        , medSignDescr = Just $ Textarea "Les maux de tête, également appelés céphalées, sont le symptôme d'une douleur au visage, à la tête ou au cou."
                        , medSignTag = Just st22
                        }

    sgn6 <- insert sign6

    let sign7 = MedSign { medSignName = "Douleur"
                        , medSignCode = Nothing
                        , medSignIcon = Just "sick"
                        , medSignDescr = Just $ Textarea "La douleur est une sensation pénible souvent provoquée par des stimuli intenses ou dommageables."
                        , medSignTag = Just st22
                        }

    sgn7 <- insert sign7

    let sign8 = MedSign { medSignName = "Fatigue"
                        , medSignCode = Nothing
                        , medSignIcon = Nothing
                        , medSignDescr = Just $ Textarea "La fatigue décrit un état de lassitude (qui n'est pas de la somnolence) ou d'épuisement"
                        , medSignTag = Just st22
                        }

    sgn8 <- insert sign8

    let sign9 = MedSign { medSignName = "Anorexie"
                        , medSignCode = Nothing
                        , medSignIcon = Nothing
                        , medSignDescr = Just $ Textarea "L'anorexie est un terme médical désignant une perte d'appétit"
                        , medSignTag = Just st22
                        }

    sgn9 <- insert sign9

    let sign10 = MedSign { medSignName = "Sueurs nocturnes"
                         , medSignCode = Nothing
                         , medSignIcon = Nothing
                         , medSignDescr = Just $ Textarea "Les sueurs nocturnes ou hyperhidrose nocturne sont la survenue répétée d'une transpiration excessive pendant le sommeil."
                         , medSignTag = Just st22
                         }

    sgn10 <- insert sign10

    let sign11 = MedSign { medSignName = "Malaise"
                         , medSignCode = Nothing
                         , medSignIcon = Nothing
                         , medSignDescr = Just $ Textarea "En tant que terme médical, le malaise est une sensation générale d'inconfort, de malaise ou de manque de bien-être et est souvent le premier signe d'une infection ou d'une autre maladie."
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
                                      , measurementName = "Systolique"
                                      , measurementUnit = Just u1
                                      }

    m1211 <- insert measurement1211

    let measurement1212 = Measurement { measurementRecord = r121
                                      , measurementValue = 70
                                      , measurementName = "Diastolique"
                                      , measurementUnit = Just u1
                                      }

    m1212 <- insert measurement1212

    let measurement1213 = Measurement { measurementRecord = r121
                                      , measurementValue = 60
                                      , measurementName = "Pouls"
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
                                     , measurementName = "Valeur"
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
                                     , measurementName = "Valeur"
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
                                      , measurementName = "Systolique"
                                      , measurementUnit = Just u1
                                      }

    m2211 <- insert measurement2211

    let measurement2212 = Measurement { measurementRecord = r221
                                      , measurementValue = 70
                                      , measurementName = "Diastolique"
                                      , measurementUnit = Just u1
                                      }

    m2212 <- insert measurement2212

    let measurement2213 = Measurement { measurementRecord = r221
                                      , measurementValue = 60
                                      , measurementName = "Pouls"
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
                                     , measurementName = "Valeur"
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
                                     , measurementName = "Valeur"
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
                                     , measurementName = "Systolique"
                                     , measurementUnit = Just u1
                                     }

    m121 <- insert measurement121

    let measurement122 = Measurement { measurementRecord = r12
                                     , measurementValue = 71
                                     , measurementName = "Diastolique"
                                     , measurementUnit = Just u1
                                     }

    m122 <- insert measurement122

    let measurement123 = Measurement { measurementRecord = r12
                                     , measurementValue = 64
                                     , measurementName = "Pouls"
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
                                     , measurementName = "Valeur"
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
                                     , measurementName = "Valeur"
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
                                     , measurementName = "Systolique"
                                     , measurementUnit = Just u1
                                     }

    m221 <- insert measurement221

    let measurement222 = Measurement { measurementRecord = r22
                                     , measurementValue = 71
                                     , measurementName = "Diastolique"
                                     , measurementUnit = Just u1
                                     }

    m222 <- insert measurement222

    let measurement223 = Measurement { measurementRecord = r22
                                     , measurementValue = 64
                                     , measurementName = "Pouls"
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
                                     , measurementName = "Valeur"
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
                                     , measurementName = "Valeur"
                                     , measurementUnit = Just u2
                                     }

    m231 <- insert measurement231

    let chat131 = Chat { chatUser = usr1
                       , chatInterlocutor = usr3
                       , chatTimemark = addUTCTime ((-1) * nominalDay) now
                       , chatMessage = "Bonjour, Dr Richard Arthur Louis"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat131

    let chat311 = Chat { chatUser = usr3
                       , chatInterlocutor = usr1
                       , chatTimemark = addUTCTime 30 (chatTimemark chat131)
                       , chatMessage = "Bonjour Bernard Jade"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat311

    let chat312 = Chat { chatUser = usr3
                       , chatInterlocutor = usr1
                       , chatTimemark = addUTCTime 60 (chatTimemark chat311)
                       , chatMessage = "Comment allez-vous?"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat312

    let chat132 = Chat { chatUser = usr1
                       , chatInterlocutor = usr3
                       , chatTimemark = addUTCTime 65 (chatTimemark chat312)
                       , chatMessage = "Dans l'ensemble, ça va, mais je m'inquiète pour la tension artérielle."
                       , chatStatus = ChatMessageStatusUnread
                       , chatNotified = True
                       }

    insert_ chat132

    let chat321 = Chat { chatUser = usr3
                       , chatInterlocutor = usr2
                       , chatTimemark = addUTCTime ((-2) * nominalDay) now
                       , chatMessage = "Bonjour Gabriel."
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat321

    let chat231 = Chat { chatUser = usr2
                       , chatInterlocutor = usr3
                       , chatTimemark = addUTCTime 60 (chatTimemark chat321)
                       , chatMessage = "Bonjour, Dr Richard Arthur Louis"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat231

    let chat322 = Chat { chatUser = usr3
                       , chatInterlocutor = usr2
                       , chatTimemark = addUTCTime 10 (chatTimemark chat231)
                       , chatMessage = "Tout va bien?"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat322

    let chat232 = Chat { chatUser = usr2
                       , chatInterlocutor = usr3
                       , chatTimemark = addUTCTime 30 (chatTimemark chat322)
                       , chatMessage = "Oui tout va bien."
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat232

    let chat233 = Chat { chatUser = usr2
                       , chatInterlocutor = usr3
                       , chatTimemark = addUTCTime ((-1) * nominalDay) now
                       , chatMessage = "Bonjour, Dr Richard Arthur Louis"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }
    insert_ chat233

    let chat323 = Chat { chatUser = usr3
                       , chatInterlocutor = usr2
                       , chatTimemark = addUTCTime 65 (chatTimemark chat233)
                       , chatMessage = "Salut Gabriel"
                       , chatStatus = ChatMessageStatusUnread
                       , chatNotified = True
                       }

    insert_ chat323

    let chat141 = Chat { chatUser = usr1
                       , chatInterlocutor = usr4
                       , chatTimemark = addUTCTime ((-1) * nominalDay) now
                       , chatMessage = "Bonjour, Dr Dubois Alice Ambre"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat141

    let chat411 = Chat { chatUser = usr4
                       , chatInterlocutor = usr1
                       , chatTimemark = addUTCTime 30 (chatTimemark chat141)
                       , chatMessage = "Bonjour Bernard Jade"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat411

    let chat412 = Chat { chatUser = usr4
                       , chatInterlocutor = usr1
                       , chatTimemark = addUTCTime 60 (chatTimemark chat411)
                       , chatMessage = "Comment allez-vous?"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat412

    let chat142 = Chat { chatUser = usr1
                       , chatInterlocutor = usr4
                       , chatTimemark = addUTCTime 65 (chatTimemark chat412)
                       , chatMessage = "Dans l'ensemble, ça va, mais je m'inquiète pour la tension artérielle."
                       , chatStatus = ChatMessageStatusUnread
                       , chatNotified = True
                       }

    insert_ chat142

    let chat421 = Chat { chatUser = usr4
                       , chatInterlocutor = usr2
                       , chatTimemark = addUTCTime ((-2) * nominalDay) now
                       , chatMessage = "Bonjour Gabriel."
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat421

    let chat241 = Chat { chatUser = usr2
                       , chatInterlocutor = usr4
                       , chatTimemark = addUTCTime 60 (chatTimemark chat421)
                       , chatMessage = "Bonjour, Dr Dubois Alice Ambre"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat241

    let chat422 = Chat { chatUser = usr4
                       , chatInterlocutor = usr2
                       , chatTimemark = addUTCTime 10 (chatTimemark chat241)
                       , chatMessage = "Tout va bien?"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat422

    let chat242 = Chat { chatUser = usr2
                       , chatInterlocutor = usr4
                       , chatTimemark = addUTCTime 30 (chatTimemark chat422)
                       , chatMessage = "Oui tout va bien."
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }

    insert_ chat242

    let chat243 = Chat { chatUser = usr2
                       , chatInterlocutor = usr4
                       , chatTimemark = addUTCTime ((-1) * nominalDay) now
                       , chatMessage = "Bonjour, Dr Dubois Alice Ambre"
                       , chatStatus = ChatMessageStatusRead
                       , chatNotified = True
                       }
    insert_ chat243

    let chat423 = Chat { chatUser = usr4
                       , chatInterlocutor = usr2
                       , chatTimemark = addUTCTime 65 (chatTimemark chat243)
                       , chatMessage = "Salut Gabriel"
                       , chatStatus = ChatMessageStatusUnread
                       , chatNotified = True
                       }

    insert_ chat423

    return ()
