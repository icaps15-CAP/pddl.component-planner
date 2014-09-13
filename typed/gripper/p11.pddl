(DEFINE (PROBLEM STRIPS-GRIPPER-X-11)
 (:DOMAIN GRIPPER-STRIPS)
 (:OBJECTS ROOMA - ROOM
           ROOMB - ROOM
           BALL24 - BALL
           BALL23 - BALL
           BALL22 - BALL
           BALL21 - BALL
           BALL20 - BALL
           BALL19 - BALL
           BALL18 - BALL
           BALL17 - BALL
           BALL16 - BALL
           BALL15 - BALL
           BALL14 - BALL
           BALL13 - BALL
           BALL12 - BALL
           BALL11 - BALL
           BALL10 - BALL
           BALL9 - BALL
           BALL8 - BALL
           BALL7 - BALL
           BALL6 - BALL
           BALL5 - BALL
           BALL4 - BALL
           BALL3 - BALL
           BALL2 - BALL
           BALL1 - BALL
           LEFT - GRIPPER
           RIGHT - GRIPPER)

 (:INIT
  (AT-ROBBY ROOMA)
  (FREE LEFT)
  (FREE RIGHT)
  (AT BALL24 ROOMA)
  (AT BALL23 ROOMA)
  (AT BALL22 ROOMA)
  (AT BALL21 ROOMA)
  (AT BALL20 ROOMA)
  (AT BALL19 ROOMA)
  (AT BALL18 ROOMA)
  (AT BALL17 ROOMA)
  (AT BALL16 ROOMA)
  (AT BALL15 ROOMA)
  (AT BALL14 ROOMA)
  (AT BALL13 ROOMA)
  (AT BALL12 ROOMA)
  (AT BALL11 ROOMA)
  (AT BALL10 ROOMA)
  (AT BALL9 ROOMA)
  (AT BALL8 ROOMA)
  (AT BALL7 ROOMA)
  (AT BALL6 ROOMA)
  (AT BALL5 ROOMA)
  (AT BALL4 ROOMA)
  (AT BALL3 ROOMA)
  (AT BALL2 ROOMA)
  (AT BALL1 ROOMA))
 (:GOAL
  (AND (AT BALL24 ROOMB)
       (AT BALL23 ROOMB)
       (AT BALL22 ROOMB)
       (AT BALL21 ROOMB)
       (AT BALL20 ROOMB)
       (AT BALL19 ROOMB)
       (AT BALL18 ROOMB)
       (AT BALL17 ROOMB)
       (AT BALL16 ROOMB)
       (AT BALL15 ROOMB)
       (AT BALL14 ROOMB)
       (AT BALL13 ROOMB)
       (AT BALL12 ROOMB)
       (AT BALL11 ROOMB)
       (AT BALL10 ROOMB)
       (AT BALL9 ROOMB)
       (AT BALL8 ROOMB)
       (AT BALL7 ROOMB)
       (AT BALL6 ROOMB)
       (AT BALL5 ROOMB)
       (AT BALL4 ROOMB)
       (AT BALL3 ROOMB)
       (AT BALL2 ROOMB)
       (AT BALL1 ROOMB))))