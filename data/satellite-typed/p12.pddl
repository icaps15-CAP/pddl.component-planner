(DEFINE (PROBLEM SATELLITE-TYPED-12) (:DOMAIN SATELLITE-TYPED)
 (:OBJECTS PHENOMENON24 - DIRECTION STAR23 - DIRECTION STAR22 - DIRECTION
  PHENOMENON21 - DIRECTION PLANET20 - DIRECTION STAR19 - DIRECTION STAR18 -
  DIRECTION PHENOMENON17 - DIRECTION PLANET16 - DIRECTION STAR15 - DIRECTION
  STAR14 - DIRECTION PHENOMENON13 - DIRECTION STAR12 - DIRECTION PLANET11 -
  DIRECTION PLANET10 - DIRECTION STAR9 - DIRECTION PLANET8 - DIRECTION STAR7 -
  DIRECTION PHENOMENON6 - DIRECTION PLANET5 - DIRECTION STAR4 - DIRECTION STAR2
  - DIRECTION GROUNDSTATION1 - DIRECTION STAR3 - DIRECTION STAR0 - DIRECTION
  INFRARED3 - MODE SPECTROGRAPH4 - MODE INFRARED1 - MODE INFRARED0 - MODE
  THERMOGRAPH2 - MODE INSTRUMENT9 - INSTRUMENT INSTRUMENT8 - INSTRUMENT
  INSTRUMENT7 - INSTRUMENT SATELLITE4 - SATELLITE INSTRUMENT6 - INSTRUMENT
  SATELLITE3 - SATELLITE INSTRUMENT5 - INSTRUMENT SATELLITE2 - SATELLITE
  INSTRUMENT4 - INSTRUMENT INSTRUMENT3 - INSTRUMENT SATELLITE1 - SATELLITE
  INSTRUMENT2 - INSTRUMENT INSTRUMENT1 - INSTRUMENT INSTRUMENT0 - INSTRUMENT
  SATELLITE0 - SATELLITE)
 (:INIT (POINTING SATELLITE4 STAR14) (POWER_AVAIL SATELLITE4)
  (ON_BOARD INSTRUMENT9 SATELLITE4) (ON_BOARD INSTRUMENT8 SATELLITE4)
  (ON_BOARD INSTRUMENT7 SATELLITE4) (CALIBRATION_TARGET INSTRUMENT9 STAR4)
  (SUPPORTS INSTRUMENT9 INFRARED1) (SUPPORTS INSTRUMENT9 SPECTROGRAPH4)
  (SUPPORTS INSTRUMENT9 INFRARED3) (CALIBRATION_TARGET INSTRUMENT8 STAR2)
  (SUPPORTS INSTRUMENT8 SPECTROGRAPH4) (SUPPORTS INSTRUMENT8 INFRARED3)
  (SUPPORTS INSTRUMENT8 INFRARED0) (CALIBRATION_TARGET INSTRUMENT7 STAR2)
  (SUPPORTS INSTRUMENT7 INFRARED3) (SUPPORTS INSTRUMENT7 INFRARED1)
  (POINTING SATELLITE3 PHENOMENON6) (POWER_AVAIL SATELLITE3)
  (ON_BOARD INSTRUMENT6 SATELLITE3) (CALIBRATION_TARGET INSTRUMENT6 STAR4)
  (SUPPORTS INSTRUMENT6 INFRARED1) (POINTING SATELLITE2 STAR15)
  (POWER_AVAIL SATELLITE2) (ON_BOARD INSTRUMENT5 SATELLITE2)
  (CALIBRATION_TARGET INSTRUMENT5 GROUNDSTATION1)
  (SUPPORTS INSTRUMENT5 INFRARED1) (POINTING SATELLITE1 STAR4)
  (POWER_AVAIL SATELLITE1) (ON_BOARD INSTRUMENT4 SATELLITE1)
  (ON_BOARD INSTRUMENT3 SATELLITE1) (CALIBRATION_TARGET INSTRUMENT4 STAR4)
  (SUPPORTS INSTRUMENT4 THERMOGRAPH2) (SUPPORTS INSTRUMENT4 INFRARED3)
  (SUPPORTS INSTRUMENT4 INFRARED0) (CALIBRATION_TARGET INSTRUMENT3 STAR4)
  (SUPPORTS INSTRUMENT3 SPECTROGRAPH4) (SUPPORTS INSTRUMENT3 INFRARED0)
  (POINTING SATELLITE0 PLANET16) (POWER_AVAIL SATELLITE0)
  (ON_BOARD INSTRUMENT2 SATELLITE0) (ON_BOARD INSTRUMENT1 SATELLITE0)
  (ON_BOARD INSTRUMENT0 SATELLITE0) (CALIBRATION_TARGET INSTRUMENT2 STAR3)
  (SUPPORTS INSTRUMENT2 INFRARED0) (SUPPORTS INSTRUMENT2 INFRARED1)
  (CALIBRATION_TARGET INSTRUMENT1 STAR2) (SUPPORTS INSTRUMENT1 INFRARED0)
  (SUPPORTS INSTRUMENT1 INFRARED1) (CALIBRATION_TARGET INSTRUMENT0 STAR0)
  (SUPPORTS INSTRUMENT0 SPECTROGRAPH4) (SUPPORTS INSTRUMENT0 INFRARED1))
 (:GOAL
  (AND (HAVE_IMAGE PLANET5 INFRARED0) (HAVE_IMAGE PHENOMENON6 SPECTROGRAPH4)
       (HAVE_IMAGE STAR7 INFRARED0) (HAVE_IMAGE PLANET8 INFRARED1)
       (HAVE_IMAGE STAR9 SPECTROGRAPH4) (HAVE_IMAGE PLANET10 THERMOGRAPH2)
       (HAVE_IMAGE PLANET11 INFRARED3) (HAVE_IMAGE PHENOMENON13 SPECTROGRAPH4)
       (HAVE_IMAGE STAR14 THERMOGRAPH2) (HAVE_IMAGE STAR15 INFRARED3)
       (HAVE_IMAGE PLANET16 INFRARED1) (HAVE_IMAGE PHENOMENON17 SPECTROGRAPH4)
       (HAVE_IMAGE STAR18 SPECTROGRAPH4) (HAVE_IMAGE STAR19 THERMOGRAPH2)
       (HAVE_IMAGE PLANET20 THERMOGRAPH2)
       (HAVE_IMAGE PHENOMENON21 THERMOGRAPH2) (HAVE_IMAGE STAR22 INFRARED1)
       (HAVE_IMAGE STAR23 SPECTROGRAPH4) (HAVE_IMAGE PHENOMENON24 INFRARED0))))