(DEFINE (PROBLEM DLOG-3-3-6)
 (:DOMAIN DRIVERLOG)
 (:OBJECTS DRIVER1 - DRIVER
           DRIVER2 - DRIVER
           DRIVER3 - DRIVER
           TRUCK1 - TRUCK
           TRUCK2 - TRUCK
           TRUCK3 - TRUCK
           PACKAGE1 - OBJ
           PACKAGE2 - OBJ
           PACKAGE3 - OBJ
           PACKAGE4 - OBJ
           PACKAGE5 - OBJ
           PACKAGE6 - OBJ
           S0 - LOCATION
           S1 - LOCATION
           S2 - LOCATION
           P0-1 - LOCATION
           P0-2 - LOCATION
           P2-1 - LOCATION)

 (:INIT
  (AT DRIVER1 S1)
  (AT DRIVER2 S2)
  (AT DRIVER3 S2)
  (AT TRUCK1 S1)
  (EMPTY TRUCK1)
  (AT TRUCK2 S1)
  (EMPTY TRUCK2)
  (AT TRUCK3 S1)
  (EMPTY TRUCK3)
  (AT PACKAGE1 S0)
  (AT PACKAGE2 S2)
  (AT PACKAGE3 S1)
  (AT PACKAGE4 S1)
  (AT PACKAGE5 S1)
  (AT PACKAGE6 S0)
  (PATH S0 P0-1)
  (PATH P0-1 S0)
  (PATH S1 P0-1)
  (PATH P0-1 S1)
  (PATH S0 P0-2)
  (PATH P0-2 S0)
  (PATH S2 P0-2)
  (PATH P0-2 S2)
  (PATH S2 P2-1)
  (PATH P2-1 S2)
  (PATH S1 P2-1)
  (PATH P2-1 S1)
  (LINK S1 S0)
  (LINK S0 S1)
  (LINK S1 S2)
  (LINK S2 S1)
  (LINK S2 S0)
  (LINK S0 S2))
 (:GOAL
  (AND (AT DRIVER1 S0)
       (AT DRIVER2 S0)
       (AT TRUCK1 S1)
       (AT TRUCK2 S0)
       (AT PACKAGE1 S0)
       (AT PACKAGE2 S1)
       (AT PACKAGE3 S1)
       (AT PACKAGE4 S2)
       (AT PACKAGE5 S0)
       (AT PACKAGE6 S0))))