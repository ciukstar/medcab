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
      ), Doctor (doctorName, Doctor, doctorMobile, doctorEmail)
    , DoctorPhoto
      ( DoctorPhoto, doctorPhotoDoctor, doctorPhotoMime, doctorPhotoPhoto
      , doctorPhotoAttribution
      )
    , Specialist
      ( Specialist, specialistDoctor, specialistSpecialty, specialistCertDate
      , specialistTitle
      ), Unit (Unit, unitName, unitSymbol, unitDescr)
    , MedSign
      ( MedSign, medSignName, medSignCode, medSignUnit, medSignDescr, medSignTag )
    , SignTag (SignTag, signTagName, signTagGroup, signTagDescr), Quantity (Quantity, quantityName, quantityDescr)
    )

import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (st)

import Yesod.Auth.Email (saltPass)
import Yesod.Form.Fields (Textarea(Textarea))
import Yesod.Persist(PersistStoreWrite (insert, insert_))
import Data.FileEmbed (embedFile)
import Data.Time.Calendar (addGregorianYearsClip)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))


fillDemoEn :: MonadIO m => ReaderT SqlBackend m ()
fillDemoEn = do

    today <- liftIO $ utctDay <$> getCurrentTime


    pass1 <- liftIO $ saltPass "marylopez"
    let user1 = User { userEmail = "marylopez@xmail.edu"
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
    let user2 = User { userEmail = "jjohnson@xmail.edu"
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

    let doctor1 = Doctor { doctorName = "Dr. Julian Maulsby"
                         , doctorMobile = "+18056594960"
                         , doctorEmail = "jmaulsby@xmail.edu"
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
                       , specialistTitle = "Allergists"
                       , specialistCertDate = addGregorianYearsClip (-11) today
                       }
    insert_ Specialist { specialistDoctor = d1
                       , specialistSpecialty = s1
                       , specialistTitle = "Immunologist"
                       , specialistCertDate = addGregorianYearsClip (-10) today
                       }

    let doctor2 = Doctor { doctorName = "Dr. Valentina Schoen"
                         , doctorMobile = "+12258813837"
                         , doctorEmail = "vschoen@xmail.edu"
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
                       , specialistTitle = "Anesthesiologist"
                       , specialistCertDate = addGregorianYearsClip (-9) today
                       }

    let doctor3 = Doctor { doctorName = "Dr. Steve Stefano"
                         , doctorMobile = "+13029222541"
                         , doctorEmail = "sstefano@xmail.edu"
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
                       , specialistTitle = "Cardiologist"
                       , specialistCertDate = addGregorianYearsClip (-5) today
                       }

    let doctor4 = Doctor { doctorName = "Dr. Jocelyn Frascone"
                         , doctorMobile = "+17743753179"
                         , doctorEmail = "jfrascone@xmail.edu"
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
                       , specialistTitle = "Dermatologist"
                       , specialistCertDate = addGregorianYearsClip (-5) today
                       }

    let quantity1 = Quantity { quantityName = "Length"
                             , quantityDescr = Just $ Textarea "Length is a measure of distance"
                             }

    q1 <- insert quantity1

    let quantity2 = Quantity { quantityName = "Time"
                             , quantityDescr = Just $ Textarea "Time is the continued sequence of existence and events that occurs in an apparently irreversible succession from the past, through the present, and into the future"
                             }

    q2 <- insert quantity2

    let quantity3 = Quantity { quantityName = "Mass"
                             , quantityDescr = Just $ Textarea "Mass is an intrinsic property of a body"
                             }

    q3 <- insert quantity3

    let quantity4 = Quantity { quantityName = "Temperature"
                             , quantityDescr = Just $ Textarea "Temperature is a physical quantity that quantitatively expresses the attribute of hotness or coldness"
                             }

    q4 <- insert quantity4

    let unit1 = Unit { unitName = "Millimeters of mercury"
                     , unitSymbol = "mmHg"
                     , unitDescr = Just $ Textarea "A millimetre of mercury is a manometric unit of pressure, formerly defined as the extra pressure generated by a column of mercury one millimetre high, and currently defined as exactly 133.322387415 pascals or exactly 133.322 pascals"
                     }

    u1 <- insert unit1

    let unit2 = Unit { unitName = "Beats per minute"
                     , unitSymbol = "bpm"
                     , unitDescr = Just $ Textarea "Heart rate (or pulse rate) is the frequency of the heartbeat measured by the number of contractions of the heart per minute"
                     }

    u2 <- insert unit2

    let unit3 = Unit { unitName = "Centimetre"
                     , unitSymbol = "cm"
                     , unitDescr = Just $ Textarea "A centimetre is a unit of length in the International System of Units equal to one hundredth of a metre"
                     }

    u3 <- insert unit3

    let unit4 = Unit { unitName = "Foot"
                     , unitSymbol = "ft"
                     , unitDescr = Just $ Textarea "The foot is a unit for measuring length. It is one of the Imperial units and US customary units. One foot contains 12 inches. This is equal to 30.48 centimeters. It is called a foot, because it was originally based on the length of a foot."
                     }

    u4 <- insert unit4

    let unit5 = Unit { unitName = "Celsius"
                     , unitSymbol = "°C"
                     , unitDescr = Just $ Textarea "The degree Celsius is the unit of temperature on the Celsius scale, one of two temperature scales used in the International System of Units (SI), the other being the closely related Kelvin scale"
                     }

    u5 <- insert unit5

    let unit6 = Unit { unitName = "Fahrenheit"
                     , unitSymbol = "°F"
                     , unitDescr = Just $ Textarea "The degree Celsius is the unit of temperature on the Celsius scale, one of two temperature scales used in the International System of Units (SI), the other being the closely related Kelvin scale"
                     }

    u6 <- insert unit6

    let unit7 = Unit { unitName = "Breaths per minute"
                     , unitSymbol = "br/min"
                     , unitDescr = Just $ Textarea "A person's respiratory rate is usually measured in breaths per minute"
                     }

    u7 <- insert unit7

    let unit8 = Unit { unitName = "Pound"
                     , unitSymbol = "lb"
                     , unitDescr = Just $ Textarea "The pound or pound-mass is a unit of mass used in both the British imperial and United States customary systems of measurement"
                     }

    u8 <- insert unit8

    let unit9 = Unit { unitName = "Kilogram"
                     , unitSymbol = "kg"
                     , unitDescr = Just $ Textarea "The kilogram is the base unit of mass in the International System of Units (SI)"
                     }

    u9 <- insert unit9

    let signTag1 = SignTag { signTagName = "Vital signs"
                           , signTagDescr = Just $ Textarea "Vital signs (also known as vitals) are a group of the four to six most crucial medical signs that indicate the status of the body's vital (life-sustaining) functions"
                           , signTagGroup = Nothing
                           }

    st1 <- insert signTag1

    let signTag2 = SignTag { signTagName = "Symptoms"
                           , signTagDescr = Just $ Textarea "A symptom is something felt or experienced, such as pain or dizziness"
                           , signTagGroup = Nothing
                           }

    st2 <- insert signTag2

    let signTag21 = SignTag { signTagName = "Specific symptoms"
                            , signTagDescr = Just $ Textarea "Some symptoms are specific, that is, they are associated with a single, specific medical condition"
                            , signTagGroup = Just st2
                            }

    st21 <- insert_ signTag21

    let signTag22 = SignTag { signTagName = "Nonspecific symptoms"
                            , signTagDescr = Just $ Textarea "Nonspecific symptoms, sometimes also called equivocal symptoms, are not specific to a particular condition"
                            , signTagGroup = Just st2
                            }

    st22 <- insert signTag22

    let sign1 = MedSign { medSignName = "Body temperature"
                        , medSignCode = Just "BT"
                        , medSignDescr = Just $ Textarea "Thermoregulation is the ability of an organism to keep its body temperature within certain boundaries, even when the surrounding temperature is very different"
                        , medSignUnit = Just u5
                        , medSignTag = Just st1
                        }

    sgn1 <- insert_ sign1

    let sign2 = MedSign { medSignName = "Blood pressure"
                        , medSignCode = Just "BP"
                        , medSignDescr = Just $ Textarea "Blood pressure is the pressure of circulating blood against the walls of blood vessels. Most of this pressure results from the heart pumping blood through the circulatory system"
                        , medSignUnit = Just u1
                        , medSignTag = Just st1
                        }

    sgn2 <- insert_ sign2

    let sign3 = MedSign { medSignName = "Heart rate"
                        , medSignCode = Just "HR"
                        , medSignDescr = Just $ Textarea "In medicine, a pulse represents the tactile arterial palpation of the cardiac cycle (heartbeat) by trained fingertips"
                        , medSignUnit = Just u2
                        , medSignTag = Just st1
                        }

    sgn3 <- insert_ sign3

    let sign4 = MedSign { medSignName = "Respiratory rate"
                        , medSignCode = Just "RR"
                        , medSignDescr = Just $ Textarea "The respiratory rate is the rate at which breathing occurs; it is set and controlled by the respiratory center of the brain. A person's respiratory rate is usually measured in breaths per minute"
                        , medSignUnit = Just u7
                        , medSignTag = Just st1
                        }

    sgn4 <- insert sign4

    let sign5 = MedSign { medSignName = "Weight loss"
                        , medSignCode = Nothing
                        , medSignDescr = Just $ Textarea "Weight loss, in the context of medicine, health, or physical fitness, refers to a reduction of the total body mass, by a mean loss of fluid, body fat (adipose tissue), or lean mass (namely bone mineral deposits, muscle, tendon, and other connective tissue)"
                        , medSignUnit = Just u9
                        , medSignTag = Just st22
                        }

    sgn5 <- insert sign5

    let sign6 = MedSign { medSignName = "Headache"
                        , medSignCode = Nothing
                        , medSignDescr = Just $ Textarea "Headache, also known as cephalalgia, is the symptom of pain in the face, head, or neck"
                        , medSignUnit = Nothing
                        , medSignTag = Just st22
                        }

    sgn6 <- insert sign6

    let sign7 = MedSign { medSignName = "Pain"
                        , medSignCode = Nothing
                        , medSignDescr = Just $ Textarea "Pain is a distressing feeling often caused by intense or damaging stimuli"
                        , medSignUnit = Nothing
                        , medSignTag = Just st22
                        }

    sgn7 <- insert sign7

    let sign8 = MedSign { medSignName = "Fatigue"
                        , medSignCode = Nothing
                        , medSignDescr = Just $ Textarea "Fatigue describes a state of tiredness (which is not sleepiness) or exhaustion"
                        , medSignUnit = Nothing
                        , medSignTag = Just st22
                        }

    sgn8 <- insert sign8

    let sign9 = MedSign { medSignName = "Anorexia"
                        , medSignCode = Nothing
                        , medSignDescr = Just $ Textarea "Anorexia is a medical term for a loss of appetite"
                        , medSignUnit = Nothing
                        , medSignTag = Just st22
                        }

    sgn9 <- insert sign9

    let sign10 = MedSign { medSignName = "Night sweats"
                         , medSignCode = Nothing
                         , medSignDescr = Just $ Textarea "Night sweats or nocturnal hyperhidrosis is the repeated occurrence of excessive sweating during sleep"
                         , medSignUnit = Nothing
                         , medSignTag = Just st22
                         }

    sgn10 <- insert sign10

    let sign11 = MedSign { medSignName = "Malaise"
                         , medSignCode = Nothing
                         , medSignDescr = Just $ Textarea "As a medical term, malaise is a feeling of general discomfort, uneasiness or lack of wellbeing and often the first sign of an infection or other disease"
                         , medSignUnit = Nothing
                         , medSignTag = Just st22
                         }

    sgn11 <- insert sign11

    return ()
