(DEFINE (PROBLEM LOGISTICS-4-2)
 (:DOMAIN LOGISTICS)
 (:OBJECTS APN1 - AIRPLANE
           APT2 - AIRPORT
           POS2 - LOCATION
           APT1 - AIRPORT
           POS1 - LOCATION
           CIT2 - CITY
           CIT1 - CITY
           TRU2 - TRUCK
           TRU1 - TRUCK
           OBJ23 - PACKAGE
           OBJ22 - PACKAGE
           OBJ21 - PACKAGE
           OBJ13 - PACKAGE
           OBJ12 - PACKAGE
           OBJ11 - PACKAGE)

 (:INIT
  (LOCATION APT1)
  (LOCATION APT2)
  (AT APN1 APT1)
  (AT TRU1 POS1)
  (AT OBJ11 POS1)
  (AT OBJ12 POS1)
  (AT OBJ13 POS1)
  (AT TRU2 POS2)
  (AT OBJ21 POS2)
  (AT OBJ22 POS2)
  (AT OBJ23 POS2)
  (IN-CITY POS1 CIT1)
  (IN-CITY APT1 CIT1)
  (IN-CITY POS2 CIT2)
  (IN-CITY APT2 CIT2))
 (:GOAL
  (AND (AT OBJ21 APT1)
       (AT OBJ11 POS2)
       (AT OBJ23 POS2)
       (AT OBJ12 POS1))))