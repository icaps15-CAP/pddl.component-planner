(DEFINE (PROBLEM STRIPS-GRIPPER-X-5)
 (:DOMAIN GRIPPER-STRIPS)
 (:OBJECTS ROOMA - ROOM
           ROOMB - ROOM
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
  (AND (AT BALL12 ROOMB)
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