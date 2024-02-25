{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo.DemoEn (fillDemoEn) where

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
      ), Patient (Patient, patientUser, patientDoctor, patientSince)
    )

import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (st)

import Yesod.Auth.Email (saltPass)
import Yesod.Form.Fields (Textarea(Textarea))
import Yesod.Persist(PersistStoreWrite (insert, insert_))


fillDemoEn :: MonadIO m => ReaderT SqlBackend m ()
fillDemoEn = do

    tz <- liftIO getCurrentTimeZone
    now <- liftIO getCurrentTime
    let today = utctDay now
    let localNow = utcToLocalTime tz now

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

    pass3 <- liftIO $ saltPass "jmaulsby"
    let user3 = User { userEmail = "jmaulsby@xmail.edu"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass3
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Julian Maulsby"
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

    pass4 <- liftIO $ saltPass "vschoen"
    let user4 = User { userEmail = "vschoen@xmail.edu"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass4
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Valentina Schoen"
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
                       , specialistTitle = "Anesthesiologist"
                       , specialistCertDate = addGregorianYearsClip (-9) today
                       }

    let doctor3 = Doctor { doctorName = "Dr. Steve Stefano"
                         , doctorMobile = "+13029222541"
                         , doctorEmail = "sstefano@xmail.edu"
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
                       , specialistTitle = "Cardiologist"
                       , specialistCertDate = addGregorianYearsClip (-5) today
                       }

    let doctor4 = Doctor { doctorName = "Dr. Jocelyn Frascone"
                         , doctorMobile = "+17743753179"
                         , doctorEmail = "jfrascone@xmail.edu"
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

    let quantity5 = Quantity { quantityName = "Frequency"
                             , quantityDescr = Just $ Textarea "Frequency is the number of occurrences of a repeating event per unit of time"
                             }

    q5 <- insert quantity5

    let unit1 = Unit { unitName = "Millimeters of mercury"
                     , unitSymbol = "mmHg"
                     , unitDescr = Just $ Textarea "A millimetre of mercury is a manometric unit of pressure, formerly defined as the extra pressure generated by a column of mercury one millimetre high, and currently defined as exactly 133.322387415 pascals or exactly 133.322 pascals"
                     , unitQuantity = Nothing
                     }

    u1 <- insert unit1

    let unit2 = Unit { unitName = "Beats per minute"
                     , unitSymbol = "bpm"
                     , unitDescr = Just $ Textarea "Heart rate (or pulse rate) is the frequency of the heartbeat measured by the number of contractions of the heart per minute"
                     , unitQuantity = Just q5
                     }

    u2 <- insert unit2

    let unit3 = Unit { unitName = "Centimetre"
                     , unitSymbol = "cm"
                     , unitDescr = Just $ Textarea "A centimetre is a unit of length in the International System of Units equal to one hundredth of a metre"
                     , unitQuantity = Just q1
                     }

    u3 <- insert unit3

    let unit4 = Unit { unitName = "Foot"
                     , unitSymbol = "ft"
                     , unitDescr = Just $ Textarea "The foot is a unit for measuring length. It is one of the Imperial units and US customary units. One foot contains 12 inches. This is equal to 30.48 centimeters. It is called a foot, because it was originally based on the length of a foot."
                     , unitQuantity = Just q1
                     }

    u4 <- insert unit4

    let unit5 = Unit { unitName = "Celsius"
                     , unitSymbol = "°C"
                     , unitDescr = Just $ Textarea "The degree Celsius is the unit of temperature on the Celsius scale, one of two temperature scales used in the International System of Units (SI), the other being the closely related Kelvin scale"
                     , unitQuantity = Just q4
                     }

    u5 <- insert unit5

    let unit6 = Unit { unitName = "Fahrenheit"
                     , unitSymbol = "°F"
                     , unitDescr = Just $ Textarea "The degree Celsius is the unit of temperature on the Celsius scale, one of two temperature scales used in the International System of Units (SI), the other being the closely related Kelvin scale"
                     , unitQuantity = Just q4
                     }

    u6 <- insert unit6

    let unit7 = Unit { unitName = "Breaths per minute"
                     , unitSymbol = "br/min"
                     , unitDescr = Just $ Textarea "A person's respiratory rate is usually measured in breaths per minute"
                     , unitQuantity = Just q5
                     }

    u7 <- insert unit7

    let unit8 = Unit { unitName = "Pound"
                     , unitSymbol = "lb"
                     , unitDescr = Just $ Textarea "The pound or pound-mass is a unit of mass used in both the British imperial and United States customary systems of measurement"
                     , unitQuantity = Just q3
                     }

    u8 <- insert unit8

    let unit9 = Unit { unitName = "Kilogram"
                     , unitSymbol = "kg"
                     , unitDescr = Just $ Textarea "The kilogram is the base unit of mass in the International System of Units (SI)"
                     , unitQuantity = Just q3
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
                        , medSignIcon = Just "thermometer"
                        , medSignDescr = Just $ Textarea "Thermoregulation is the ability of an organism to keep its body temperature within certain boundaries, even when the surrounding temperature is very different"
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

    let sign2 = MedSign { medSignName = "Blood pressure"
                        , medSignCode = Just "BP"
                        , medSignIcon = Just "blood_pressure"
                        , medSignDescr = Just $ Textarea "Blood pressure is the pressure of circulating blood against the walls of blood vessels. Most of this pressure results from the heart pumping blood through the circulatory system"
                        , medSignTag = Just st1
                        }

    sgn2 <- insert sign2

    let normal21 = Normal { normalSign = sgn2
                          , normalName = "Systolic"
                          , normalLowerBound = 90
                          , normalUpperBound = 120
                          , normalUnit = Just u1
                          }

    insert_ normal21

    let normal22 = Normal { normalSign = sgn2
                          , normalName = "Diastolic"
                          , normalLowerBound = 50
                          , normalUpperBound = 80
                          , normalUnit = Just u1
                          }

    insert_ normal22

    let normal23 = Normal { normalSign = sgn2
                          , normalName = "Pulse"
                          , normalLowerBound = 60
                          , normalUpperBound = 100
                          , normalUnit = Just u1
                          }

    insert_ normal23

    let sign3 = MedSign { medSignName = "Heart rate"
                        , medSignCode = Just "HR"
                        , medSignIcon = Just "cardiology"
                        , medSignDescr = Just $ Textarea "In medicine, a pulse represents the tactile arterial palpation of the cardiac cycle (heartbeat) by trained fingertips"
                        , medSignTag = Just st1
                        }

    sgn3 <- insert sign3

    let normal31 = Normal { normalSign = sgn3
                          , normalName = "Value"
                          , normalLowerBound = 60
                          , normalUpperBound = 100
                          , normalUnit = Just u2
                          }

    insert_ normal31

    let sign4 = MedSign { medSignName = "Respiratory rate"
                        , medSignCode = Just "RR"
                        , medSignIcon = Just "respiratory_rate"
                        , medSignDescr = Just $ Textarea "The respiratory rate is the rate at which breathing occurs; it is set and controlled by the respiratory center of the brain. A person's respiratory rate is usually measured in breaths per minute"
                        , medSignTag = Just st1
                        }

    sgn4 <- insert sign4

    let normal41 = Normal { normalSign = sgn4
                          , normalName = "Value"
                          , normalLowerBound = 12
                          , normalUpperBound = 18
                          , normalUnit = Just u7
                          }

    insert_ normal41

    let sign5 = MedSign { medSignName = "Weight loss"
                        , medSignCode = Nothing
                        , medSignIcon = Just "monitor_weight_loss"
                        , medSignDescr = Just $ Textarea "Weight loss, in the context of medicine, health, or physical fitness, refers to a reduction of the total body mass, by a mean loss of fluid, body fat (adipose tissue), or lean mass (namely bone mineral deposits, muscle, tendon, and other connective tissue)"
                        , medSignTag = Just st22
                        }

    sgn5 <- insert sign5

    let normal51 = Normal { normalSign = sgn5
                          , normalName = "Value"
                          , normalLowerBound = 3
                          , normalUpperBound = 80
                          , normalUnit = Just u9
                          }

    insert_ normal51

    let sign6 = MedSign { medSignName = "Headache"
                        , medSignCode = Nothing
                        , medSignIcon = Nothing
                        , medSignDescr = Just $ Textarea "Headache, also known as cephalalgia, is the symptom of pain in the face, head, or neck"
                        , medSignTag = Just st22
                        }

    sgn6 <- insert sign6

    let sign7 = MedSign { medSignName = "Pain"
                        , medSignCode = Nothing
                        , medSignIcon = Just "sick"
                        , medSignDescr = Just $ Textarea "Pain is a distressing feeling often caused by intense or damaging stimuli"
                        , medSignTag = Just st22
                        }

    sgn7 <- insert sign7

    let sign8 = MedSign { medSignName = "Fatigue"
                        , medSignCode = Nothing
                        , medSignIcon = Nothing
                        , medSignDescr = Just $ Textarea "Fatigue describes a state of tiredness (which is not sleepiness) or exhaustion"
                        , medSignTag = Just st22
                        }

    sgn8 <- insert sign8

    let sign9 = MedSign { medSignName = "Anorexia"
                        , medSignCode = Nothing
                        , medSignIcon = Nothing
                        , medSignDescr = Just $ Textarea "Anorexia is a medical term for a loss of appetite"
                        , medSignTag = Just st22
                        }

    sgn9 <- insert sign9

    let sign10 = MedSign { medSignName = "Night sweats"
                         , medSignCode = Nothing
                         , medSignIcon = Nothing
                         , medSignDescr = Just $ Textarea "Night sweats or nocturnal hyperhidrosis is the repeated occurrence of excessive sweating during sleep"
                         , medSignTag = Just st22
                         }

    sgn10 <- insert sign10

    let sign11 = MedSign { medSignName = "Malaise"
                         , medSignCode = Nothing
                         , medSignIcon = Nothing
                         , medSignDescr = Just $ Textarea "As a medical term, malaise is a feeling of general discomfort, uneasiness or lack of wellbeing and often the first sign of an infection or other disease"
                         , medSignTag = Just st22
                         }

    sgn11 <- insert sign11

    let twoDaysBefore = addLocalTime ((-2) * nominalDay) localNow

    let patient11 = Patient { patientUser = usr1
                            , patientDoctor = d1
                            , patientSince = addUTCTime ((-7) * nominalDay) now
                            }

    insert_ patient11

    let patient12 = Patient { patientUser = usr1
                            , patientDoctor = d2
                            , patientSince = addUTCTime ((-6) * nominalDay) now
                            }

    insert_ patient12

    let patient21 = Patient { patientUser = usr2
                            , patientDoctor = d1
                            , patientSince = addUTCTime ((-3) * nominalDay) now
                            }

    insert_ patient21

    let patient22 = Patient { patientUser = usr2
                            , patientDoctor = d2
                            , patientSince = addUTCTime ((-2) * nominalDay) now
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
                                      , measurementName = "Systolic"
                                      , measurementUnit = Just u1
                                      }

    m1211 <- insert measurement1211

    let measurement1212 = Measurement { measurementRecord = r121
                                      , measurementValue = 70
                                      , measurementName = "Diastolic"
                                      , measurementUnit = Just u1
                                      }

    m1212 <- insert measurement1212

    let measurement1213 = Measurement { measurementRecord = r121
                                      , measurementValue = 60
                                      , measurementName = "Pulse"
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
                                     , measurementName = "Value"
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
                                     , measurementName = "Value"
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
                                      , measurementName = "Systolic"
                                      , measurementUnit = Just u1
                                      }

    m2211 <- insert measurement2211

    let measurement2212 = Measurement { measurementRecord = r221
                                      , measurementValue = 70
                                      , measurementName = "Diastolic"
                                      , measurementUnit = Just u1
                                      }

    m2212 <- insert measurement2212

    let measurement2213 = Measurement { measurementRecord = r221
                                      , measurementValue = 60
                                      , measurementName = "Pulse"
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
                                     , measurementName = "Value"
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
                                     , measurementName = "Value"
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
                                     , measurementName = "Systolic"
                                     , measurementUnit = Just u1
                                     }

    m121 <- insert measurement121

    let measurement122 = Measurement { measurementRecord = r12
                                     , measurementValue = 71
                                     , measurementName = "Diastolic"
                                     , measurementUnit = Just u1
                                     }

    m122 <- insert measurement122

    let measurement123 = Measurement { measurementRecord = r12
                                     , measurementValue = 64
                                     , measurementName = "Pulse"
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
                                     , measurementName = "Value"
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
                                     , measurementName = "Value"
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
                                     , measurementName = "Systolic"
                                     , measurementUnit = Just u1
                                     }

    m221 <- insert measurement221

    let measurement222 = Measurement { measurementRecord = r22
                                     , measurementValue = 71
                                     , measurementName = "Diastolic"
                                     , measurementUnit = Just u1
                                     }

    m222 <- insert measurement222

    let measurement223 = Measurement { measurementRecord = r22
                                     , measurementValue = 64
                                     , measurementName = "Pulse"
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
                                     , measurementName = "Value"
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
                                     , measurementName = "Value"
                                     , measurementUnit = Just u2
                                     }

    m231 <- insert measurement231

    return ()
