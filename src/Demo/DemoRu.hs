{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo.DemoRu (fillDemoRu) where

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
    , Chat (Chat, chatUser, chatInterlocutor, chatTimemark, chatMessage, chatStatus)
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


fillDemoRu :: MonadIO m => AppSettings -> ReaderT SqlBackend m ()
fillDemoRu appSettings = do

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
    
    pass1 <- liftIO $ saltPass "bulanovalm"
    let user1 = User { userEmail = "bulanovalm@xmail.ru"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass1
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Буланова Любовь Михайловна"
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

    pass2 <- liftIO $ saltPass "petrovia"
    let user2 = User { userEmail = "petrovia@xmail.ru"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass2
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Петров Иван Александрович"
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

    pass3 <- liftIO $ saltPass "smirnovav"
    let user3 = User { userEmail = "smirnovav@xmail.ru"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass3
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Смирнов Андрей Васильевич"
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

    pass4 <- liftIO $ saltPass "sergeevaav"
    let user4 = User { userEmail = "sergeevaav@xmail.ru"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass4
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Сергеева Александра Владимировна"
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

    let specialty1 = Specialty { specialtyName = "Аллергия и иммунология"
                               , specialtyCode = Just "ALLI"
                               , specialtyDescr = Just $ Textarea [st|Иммунология – это раздел медицины, который занимается изучением иммунной системы всех организмов.|]
                               , specialtyGroup = Nothing
                               }

    s1 <- insert specialty1

    let specialty2 = Specialty { specialtyName = "Анестезиология"
                               , specialtyCode = Just "ANES"
                               , specialtyDescr = Just $ Textarea [st|Анестезиология, анестезиология или анестезия — это медицинская специальность, занимающаяся полным периоперационным уходом за пациентами до, во время и после операции.|]
                               , specialtyGroup = Nothing
                               }

    s2 <- insert specialty2

    let specialty3 = Specialty { specialtyName = "Кардиология"
                               , specialtyCode = Just "CARD"
                               , specialtyDescr = Just $ Textarea [st|Кардиология – раздел медицины, занимающийся заболеваниями сердца и сердечно-сосудистой системы.|]
                               , specialtyGroup = Nothing
                               }

    s3 <- insert specialty3

    let specialty4 = Specialty { specialtyName = "Дерматология"
                               , specialtyCode = Just "DERMA"
                               , specialtyDescr = Just $ Textarea [st|Дерматология – это раздел медицины, занимающийся проблемами кожи. Это специальность, включающая как медицинские, так и хирургические аспекты. Дерматолог – это врач-специалист, который лечит заболевания кожи, волос, ногтей и некоторые косметические проблемы.|]
                               , specialtyGroup = Nothing
                               }

    s4 <- insert specialty4

    let doctor1 = Doctor { doctorName = "Д-р Смирнов Андрей Васильевич"
                         , doctorMobile = "+18056594960"
                         , doctorEmail = "smirnovav@xmail.ru"
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
                       , specialistTitle = "Аллергологи"
                       , specialistCertDate = addGregorianYearsClip (-11) today
                       }
    insert_ Specialist { specialistDoctor = d1
                       , specialistSpecialty = s1
                       , specialistTitle = "Иммунолог"
                       , specialistCertDate = addGregorianYearsClip (-10) today
                       }

    let doctor2 = Doctor { doctorName = "Д-р Сергеева Александра Владимировна"
                         , doctorMobile = "+12258813837"
                         , doctorEmail = "sergeevaav@xmail.ru"
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
                       , specialistTitle = "Анестезиолог"
                       , specialistCertDate = addGregorianYearsClip (-9) today
                       }

    let doctor3 = Doctor { doctorName = "Д-р Кузнецов Артем Сергеевич"
                         , doctorMobile = "+13029222541"
                         , doctorEmail = "kuznetsovas@xmail.ru"
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
                       , specialistTitle = "Кардиолог"
                       , specialistCertDate = addGregorianYearsClip (-5) today
                       }

    let doctor4 = Doctor { doctorName = "Д-р Степанова Татьяна Николаевна"
                         , doctorMobile = "+17743753179"
                         , doctorEmail = "stepanovatn@xmail.ru"
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
                       , specialistTitle = "Дерматолог"
                       , specialistCertDate = addGregorianYearsClip (-5) today
                       }

    let quantity1 = Quantity { quantityName = "Длина"
                             , quantityDescr = Just $ Textarea "Длина – это мера расстояния"
                             }

    q1 <- insert quantity1

    let quantity2 = Quantity { quantityName = "Время"
                             , quantityDescr = Just $ Textarea "Время — это непрерывная последовательность существования и событий, которая происходит в очевидно необратимой последовательности из прошлого, через настоящее и в будущее."
                             }

    q2 <- insert quantity2

    let quantity3 = Quantity { quantityName = "Масса"
                             , quantityDescr = Just $ Textarea "Масса – это внутреннее свойство тела"
                             }

    q3 <- insert quantity3

    let quantity4 = Quantity { quantityName = "Температура"
                             , quantityDescr = Just $ Textarea "Температура – это физическая величина, количественно выражающая признак жары или холода."
                             }

    q4 <- insert quantity4

    let quantity5 = Quantity { quantityName = "Частота"
                             , quantityDescr = Just $ Textarea "Частота – это количество повторений повторяющегося события в единицу времени."
                             }

    q5 <- insert quantity5

    let unit1 = Unit { unitName = "Миллиметры ртутного столба"
                     , unitSymbol = "mmHg"
                     , unitDescr = Just $ Textarea "Миллиметр ртутного столба — это манометрическая единица давления, ранее определяемая как дополнительное давление, создаваемое столбом ртути высотой в один миллиметр, а в настоящее время определяемая как ровно 133,322387415 паскалей или ровно 133,322 паскаля."
                     , unitQuantity = Nothing
                     }

    u1 <- insert unit1

    let unit2 = Unit { unitName = "Частота пульса"
                     , unitSymbol = "bpm"
                     , unitDescr = Just $ Textarea "Частота сердечных сокращений (или частота пульса) — это частота сердечных сокращений, измеряемая количеством сокращений сердца в минуту."
                     , unitQuantity = Just q5
                     }

    u2 <- insert unit2

    let unit3 = Unit { unitName = "Сантиметр"
                     , unitSymbol = "см"
                     , unitDescr = Just $ Textarea "Сантиметр — единица длины в Международной системе единиц, равная одной сотой метра."
                     , unitQuantity = Just q1
                     }

    u3 <- insert unit3

    let unit4 = Unit { unitName = "Фут"
                     , unitSymbol = "фут"
                     , unitDescr = Just $ Textarea "Фут – это единица измерения длины. Это одна из имперских единиц и обычных единиц США. В одном футе содержится 12 дюймов. Это равно 30,48 сантиметра. Его называют футом, потому что изначально оно основывалось на длине ступни."
                     , unitQuantity = Just q1
                     }

    u4 <- insert unit4

    let unit5 = Unit { unitName = "Цельсия"
                     , unitSymbol = "°C"
                     , unitDescr = Just $ Textarea "Градус Цельсия — это единица температуры по шкале Цельсия, одной из двух температурных шкал, используемых в Международной системе единиц (СИ), а другая — тесно связанной шкале Кельвина."
                     , unitQuantity = Just q4
                     }

    u5 <- insert unit5

    let unit6 = Unit { unitName = "Фаренгейт"
                     , unitSymbol = "°F"
                     , unitDescr = Just $ Textarea "Шкала Фаренгейта — это температурная шкала, основанная на шкале, предложенной в 1724 году европейским физиком Даниэлем Габриэлем Фаренгейтом. В качестве единицы измерения используется градус Фаренгейта."
                     , unitQuantity = Just q4
                     }

    u6 <- insert unit6

    let unit7 = Unit { unitName = "Дыханий в минуту"
                     , unitSymbol = "д/мин"
                     , unitDescr = Just $ Textarea "Частота дыхания человека обычно измеряется в вдохах в минуту."
                     , unitQuantity = Just q5
                     }

    u7 <- insert unit7

    let unit8 = Unit { unitName = "Фунт"
                     , unitSymbol = "lb"
                     , unitDescr = Just $ Textarea "Фунт или фунт-масса — это единица массы, используемая как в британской имперской системе измерения, так и в общепринятой системе измерения США."
                     , unitQuantity = Just q3
                     }

    u8 <- insert unit8

    let unit9 = Unit { unitName = "Килограмм"
                     , unitSymbol = "кг"
                     , unitDescr = Just $ Textarea "Килограмм — основная единица массы в Международной системе единиц (СИ)."
                     , unitQuantity = Just q3
                     }

    u9 <- insert unit9

    let signTag1 = SignTag { signTagName = "Жизненные показатели"
                           , signTagDescr = Just $ Textarea "Жизненно показатели (также известные как жизненно важные показатели) представляют собой группу из четырех-шести наиболее важных медицинских признаков, которые указывают на состояние жизненно важных (поддерживающих жизнь) функций организма."
                           , signTagGroup = Nothing
                           }

    st1 <- insert signTag1

    let signTag2 = SignTag { signTagName = "Симптомы"
                           , signTagDescr = Just $ Textarea "Симптом – это что-то ощущаемое или пережитое, например боль или головокружение."
                           , signTagGroup = Nothing
                           }

    st2 <- insert signTag2

    let signTag21 = SignTag { signTagName = "Специфические симптомы"
                            , signTagDescr = Just $ Textarea "Некоторые симптомы специфичны, то есть связаны с одним конкретным заболеванием."
                            , signTagGroup = Just st2
                            }

    st21 <- insert_ signTag21

    let signTag22 = SignTag { signTagName = "Неспецифические симптомы"
                            , signTagDescr = Just $ Textarea "Неспецифические симптомы, иногда также называемые сомнительными симптомами, не являются специфичными для конкретного состояния."
                            , signTagGroup = Just st2
                            }

    st22 <- insert signTag22

    let sign1 = MedSign { medSignName = "Температура тела"
                        , medSignCode = Just "ТТ"
                        , medSignIcon = Just "thermometer"
                        , medSignDescr = Just $ Textarea "Терморегуляция – это способность организма поддерживать температуру своего тела в определенных пределах, даже если температура окружающей среды сильно отличается."
                        , medSignTag = Just st1
                        }

    sgn1 <- insert sign1

    let normal11 = Normal { normalSign = sgn1
                          , normalName = "Значение"
                          , normalLowerBound = 36.1
                          , normalUpperBound = 37.2
                          , normalUnit = Just u5
                          }

    insert_ normal11

    let sign2 = MedSign { medSignName = "Артериальное давление"
                        , medSignCode = Just "АД"
                        , medSignIcon = Just "blood_pressure"
                        , medSignDescr = Just $ Textarea "Артериальное давление – это давление циркулирующей крови на стенки кровеносных сосудов. Большая часть этого давления возникает из-за того, что сердце перекачивает кровь через систему кровообращения."
                        , medSignTag = Just st1
                        }

    sgn2 <- insert sign2

    let normal21 = Normal { normalSign = sgn2
                          , normalName = "Систолический"
                          , normalLowerBound = 90
                          , normalUpperBound = 120
                          , normalUnit = Just u1
                          }

    insert_ normal21

    let normal22 = Normal { normalSign = sgn2
                          , normalName = "Диастолический"
                          , normalLowerBound = 50
                          , normalUpperBound = 80
                          , normalUnit = Just u1
                          }

    insert_ normal22

    let normal23 = Normal { normalSign = sgn2
                          , normalName = "Пульс"
                          , normalLowerBound = 60
                          , normalUpperBound = 100
                          , normalUnit = Just u1
                          }

    insert_ normal23

    let sign3 = MedSign { medSignName = "Частота сердцебиения"
                        , medSignCode = Just "ЧС"
                        , medSignIcon = Just "cardiology"
                        , medSignDescr = Just $ Textarea "В медицине пульс представляет собой тактильную пальпацию артерий сердечного цикла (сердцебиения) тренированными кончиками пальцев."
                        , medSignTag = Just st1
                        }

    sgn3 <- insert sign3

    let normal31 = Normal { normalSign = sgn3
                          , normalName = "Значение"
                          , normalLowerBound = 60
                          , normalUpperBound = 100
                          , normalUnit = Just u2
                          }

    insert_ normal31

    let sign4 = MedSign { medSignName = "Частота дыхания"
                        , medSignCode = Just "ЧД"
                        , medSignIcon = Just "respiratory_rate"
                        , medSignDescr = Just $ Textarea "Частота дыхания — это скорость дыхания; оно задается и контролируется дыхательным центром мозга. Частота дыхания человека обычно измеряется в вдохах в минуту."
                        , medSignTag = Just st1
                        }

    sgn4 <- insert sign4

    let normal41 = Normal { normalSign = sgn4
                          , normalName = "Значение"
                          , normalLowerBound = 12
                          , normalUpperBound = 18
                          , normalUnit = Just u7
                          }

    insert_ normal41

    let sign5 = MedSign { medSignName = "Потеря веса"
                        , medSignCode = Nothing
                        , medSignIcon = Just "monitor_weight_loss"
                        , medSignDescr = Just $ Textarea "Снижение веса в контексте медицины, здоровья или физической подготовки означает уменьшение общей массы тела за счет средней потери жидкости, жира (жировой ткани) или мышечной массы (а именно минеральных отложений в костях, мышцах, сухожилия и другие соединительные ткани)."
                        , medSignTag = Just st22
                        }

    sgn5 <- insert sign5

    let normal51 = Normal { normalSign = sgn5
                          , normalName = "Значение"
                          , normalLowerBound = 3
                          , normalUpperBound = 80
                          , normalUnit = Just u9
                          }

    insert_ normal51

    let sign6 = MedSign { medSignName = "Головная боль"
                        , medSignCode = Nothing
                        , medSignIcon = Nothing
                        , medSignDescr = Just $ Textarea "Головная боль, также известная как цефалгия, является симптомом боли в лице, голове или шее."
                        , medSignTag = Just st22
                        }

    sgn6 <- insert sign6

    let sign7 = MedSign { medSignName = "Боль"
                        , medSignCode = Nothing
                        , medSignIcon = Just "sick"
                        , medSignDescr = Just $ Textarea "Боль – это мучительное чувство, часто вызываемое интенсивными или повреждающими раздражителями."
                        , medSignTag = Just st22
                        }

    sgn7 <- insert sign7

    let sign8 = MedSign { medSignName = "Усталость"
                        , medSignCode = Nothing
                        , medSignIcon = Nothing
                        , medSignDescr = Just $ Textarea "Усталость описывает состояние усталости (которое не является сонливостью) или истощения."
                        , medSignTag = Just st22
                        }

    sgn8 <- insert sign8

    let sign9 = MedSign { medSignName = "Анорексия"
                        , medSignCode = Nothing
                        , medSignIcon = Nothing
                        , medSignDescr = Just $ Textarea "Анорексия – медицинский термин, обозначающий потерю аппетита."
                        , medSignTag = Just st22
                        }

    sgn9 <- insert sign9

    let sign10 = MedSign { medSignName = "Ночная потливость"
                         , medSignCode = Nothing
                         , medSignIcon = Nothing
                         , medSignDescr = Just $ Textarea "Ночная потливость или ночной гипергидроз – это неоднократное возникновение чрезмерного потоотделения во время сна."
                         , medSignTag = Just st22
                         }

    sgn10 <- insert sign10

    let sign11 = MedSign { medSignName = "Недомогание"
                         , medSignCode = Nothing
                         , medSignIcon = Nothing
                         , medSignDescr = Just $ Textarea "В медицинском термине недомогание — это чувство общего дискомфорта, беспокойства или плохого самочувствия, которое часто является первым признаком инфекции или другого заболевания."
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
                                      , measurementName = "Систолический"
                                      , measurementUnit = Just u1
                                      }

    m1211 <- insert measurement1211

    let measurement1212 = Measurement { measurementRecord = r121
                                      , measurementValue = 70
                                      , measurementName = "Диастолический"
                                      , measurementUnit = Just u1
                                      }

    m1212 <- insert measurement1212

    let measurement1213 = Measurement { measurementRecord = r121
                                      , measurementValue = 60
                                      , measurementName = "Пульс"
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
                                     , measurementName = "Значение"
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
                                     , measurementName = "Значение"
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
                                      , measurementName = "Систолический"
                                      , measurementUnit = Just u1
                                      }

    m2211 <- insert measurement2211

    let measurement2212 = Measurement { measurementRecord = r221
                                      , measurementValue = 70
                                      , measurementName = "Диастолический"
                                      , measurementUnit = Just u1
                                      }

    m2212 <- insert measurement2212

    let measurement2213 = Measurement { measurementRecord = r221
                                      , measurementValue = 60
                                      , measurementName = "Пульс"
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
                                     , measurementName = "Значение"
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
                                     , measurementName = "Значение"
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
                                     , measurementName = "Систолический"
                                     , measurementUnit = Just u1
                                     }

    m121 <- insert measurement121

    let measurement122 = Measurement { measurementRecord = r12
                                     , measurementValue = 71
                                     , measurementName = "Диастолический"
                                     , measurementUnit = Just u1
                                     }

    m122 <- insert measurement122

    let measurement123 = Measurement { measurementRecord = r12
                                     , measurementValue = 64
                                     , measurementName = "Пульс"
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
                                     , measurementName = "Значение"
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
                                     , measurementName = "Значение"
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
                                     , measurementName = "Систолический"
                                     , measurementUnit = Just u1
                                     }

    m221 <- insert measurement221

    let measurement222 = Measurement { measurementRecord = r22
                                     , measurementValue = 71
                                     , measurementName = "Диастолический"
                                     , measurementUnit = Just u1
                                     }

    m222 <- insert measurement222

    let measurement223 = Measurement { measurementRecord = r22
                                     , measurementValue = 64
                                     , measurementName = "Пульс"
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
                                     , measurementName = "Значение"
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
                                     , measurementName = "Значение"
                                     , measurementUnit = Just u2
                                     }

    m231 <- insert measurement231

    let chat131 = Chat { chatUser = usr1
                       , chatInterlocutor = usr3
                       , chatTimemark = addUTCTime ((-1) * nominalDay) now
                       , chatMessage = "Здравствуйте, доктор Смирнов Андрей Васильевич."
                       , chatStatus = ChatMessageStatusRead
                       }

    insert_ chat131

    let chat311 = Chat { chatUser = usr3
                       , chatInterlocutor = usr1
                       , chatTimemark = addUTCTime 30 (chatTimemark chat131)
                       , chatMessage = "Здравствуйте, Буланова Любовь Михайловна."
                       , chatStatus = ChatMessageStatusRead
                       }

    insert_ chat311

    let chat312 = Chat { chatUser = usr3
                       , chatInterlocutor = usr1
                       , chatTimemark = addUTCTime 60 (chatTimemark chat311)
                       , chatMessage = "Как дела?"
                       , chatStatus = ChatMessageStatusRead
                       }

    insert_ chat312

    let chat132 = Chat { chatUser = usr1
                       , chatInterlocutor = usr3
                       , chatTimemark = addUTCTime 65 (chatTimemark chat312)
                       , chatMessage = "В целом все в порядке, но меня беспокоит артериальное давление."
                       , chatStatus = ChatMessageStatusUnread
                       }

    insert_ chat132

    let chat321 = Chat { chatUser = usr3
                       , chatInterlocutor = usr2
                       , chatTimemark = addUTCTime ((-2) * nominalDay) now
                       , chatMessage = "Здравствуйте, Иван."
                       , chatStatus = ChatMessageStatusRead
                       }

    insert_ chat321

    let chat231 = Chat { chatUser = usr2
                       , chatInterlocutor = usr3
                       , chatTimemark = addUTCTime 60 (chatTimemark chat321)
                       , chatMessage = "Здравствуйте, доктор Смирнов Андрей Васильевич."
                       , chatStatus = ChatMessageStatusRead
                       }

    insert_ chat231

    let chat322 = Chat { chatUser = usr3
                       , chatInterlocutor = usr2
                       , chatTimemark = addUTCTime 10 (chatTimemark chat231)
                       , chatMessage = "Все в порядке?"
                       , chatStatus = ChatMessageStatusRead
                       }

    insert_ chat322

    let chat232 = Chat { chatUser = usr2
                       , chatInterlocutor = usr3
                       , chatTimemark = addUTCTime 30 (chatTimemark chat322)
                       , chatMessage = "Да, все в порядке."
                       , chatStatus = ChatMessageStatusRead
                       }

    insert_ chat232

    let chat233 = Chat { chatUser = usr2
                       , chatInterlocutor = usr3
                       , chatTimemark = addUTCTime ((-1) * nominalDay) now
                       , chatMessage = "Здравствуйте, доктор Смирнов Андрей Васильевич."
                       , chatStatus = ChatMessageStatusRead
                       }
    insert_ chat233

    let chat323 = Chat { chatUser = usr3
                       , chatInterlocutor = usr2
                       , chatTimemark = addUTCTime 65 (chatTimemark chat233)
                       , chatMessage = "Привет, Иван"
                       , chatStatus = ChatMessageStatusUnread
                       }

    insert_ chat323

    let chat141 = Chat { chatUser = usr1
                       , chatInterlocutor = usr4
                       , chatTimemark = addUTCTime ((-1) * nominalDay) now
                       , chatMessage = "Здравствуйте, доктор Сергеева Александра Владимировна."
                       , chatStatus = ChatMessageStatusRead
                       }

    insert_ chat141

    let chat411 = Chat { chatUser = usr4
                       , chatInterlocutor = usr1
                       , chatTimemark = addUTCTime 30 (chatTimemark chat141)
                       , chatMessage = "Здравствуйте, Буланова Любовь Михайловна."
                       , chatStatus = ChatMessageStatusRead
                       }

    insert_ chat411

    let chat412 = Chat { chatUser = usr4
                       , chatInterlocutor = usr1
                       , chatTimemark = addUTCTime 60 (chatTimemark chat411)
                       , chatMessage = "Как дела?"
                       , chatStatus = ChatMessageStatusRead
                       }

    insert_ chat412

    let chat142 = Chat { chatUser = usr1
                       , chatInterlocutor = usr4
                       , chatTimemark = addUTCTime 65 (chatTimemark chat412)
                       , chatMessage = "В целом все в порядке, но меня беспокоит артериальное давление."
                       , chatStatus = ChatMessageStatusUnread
                       }

    insert_ chat142

    let chat421 = Chat { chatUser = usr4
                       , chatInterlocutor = usr2
                       , chatTimemark = addUTCTime ((-2) * nominalDay) now
                       , chatMessage = "Здравствуйте, Иван."
                       , chatStatus = ChatMessageStatusRead
                       }

    insert_ chat421

    let chat241 = Chat { chatUser = usr2
                       , chatInterlocutor = usr4
                       , chatTimemark = addUTCTime 60 (chatTimemark chat421)
                       , chatMessage = "Здравствуйте, доктор Сергеева Александра Владимировна."
                       , chatStatus = ChatMessageStatusRead
                       }

    insert_ chat241

    let chat422 = Chat { chatUser = usr4
                       , chatInterlocutor = usr2
                       , chatTimemark = addUTCTime 10 (chatTimemark chat241)
                       , chatMessage = "Все в порядке?"
                       , chatStatus = ChatMessageStatusRead
                       }

    insert_ chat422

    let chat242 = Chat { chatUser = usr2
                       , chatInterlocutor = usr4
                       , chatTimemark = addUTCTime 30 (chatTimemark chat422)
                       , chatMessage = "Да, все в порядке."
                       , chatStatus = ChatMessageStatusRead
                       }

    insert_ chat242

    let chat243 = Chat { chatUser = usr2
                       , chatInterlocutor = usr4
                       , chatTimemark = addUTCTime ((-1) * nominalDay) now
                       , chatMessage = "Здравствуйте, доктор Сергеева Александра Владимировна."
                       , chatStatus = ChatMessageStatusRead
                       }
    insert_ chat243

    let chat423 = Chat { chatUser = usr4
                       , chatInterlocutor = usr2
                       , chatTimemark = addUTCTime 65 (chatTimemark chat243)
                       , chatMessage = "Привет, Иван"
                       , chatStatus = ChatMessageStatusUnread
                       }

    insert_ chat423

    return ()
