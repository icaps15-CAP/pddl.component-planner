(DEFINE (PROBLEM DLOG-2-2-2)
 (:DOMAIN DRIVERLOG)
 (:OBJECTS DRIVER1 - DRIVER
           DRIVER2 - DRIVER
           TRUCK1 - TRUCK
           TRUCK2 - TRUCK
           PACKAGE1 - OBJ
           PACKAGE2 - OBJ
           S0 - LOCATION
           S1 - LOCATION
           S2 - LOCATION
           P1-0 - LOCATION
           P1-2 - LOCATION)

 (:INIT
  (AT DRIVER1 S2)
  (AT DRIVER2 S2)
  (AT TRUCK1 S0)
  (EMPTY TRUCK1)
  (AT TRUCK2 S0)
  (EMPTY TRUCK2)
  (AT PACKAGE1 S0)
  (AT PACKAGE2 S0)
  (PATH S1 P1-0)
  (PATH P1-0 S1)
  (PATH S0 P1-0)
  (PATH P1-0 S0)
  (PATH S1 P1-2)
  (PATH P1-2 S1)
  (PATH S2 P1-2)
  (PATH P1-2 S2)
  (LINK S0 S1)
  (LINK S1 S0)
  (LINK S0 S2)
  (LINK S2 S0)
  (LINK S2 S1)
  (LINK S1 S2))
 (:GOAL
  (AND (AT DRIVER1 S1)
       (AT TRUCK1 S1)
       (AT PACKAGE1 S0)
       (AT PACKAGE2 S0))))