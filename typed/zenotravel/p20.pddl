(DEFINE (PROBLEM ZTRAVEL-5-25)
 (:DOMAIN ZENO-TRAVEL)
 (:OBJECTS PLANE1 - AIRCRAFT
           PLANE2 - AIRCRAFT
           PLANE3 - AIRCRAFT
           PLANE4 - AIRCRAFT
           PLANE5 - AIRCRAFT
           PERSON1 - PERSON
           PERSON2 - PERSON
           PERSON3 - PERSON
           PERSON4 - PERSON
           PERSON5 - PERSON
           PERSON6 - PERSON
           PERSON7 - PERSON
           PERSON8 - PERSON
           PERSON9 - PERSON
           PERSON10 - PERSON
           PERSON11 - PERSON
           PERSON12 - PERSON
           PERSON13 - PERSON
           PERSON14 - PERSON
           PERSON15 - PERSON
           PERSON16 - PERSON
           PERSON17 - PERSON
           PERSON18 - PERSON
           PERSON19 - PERSON
           PERSON20 - PERSON
           PERSON21 - PERSON
           PERSON22 - PERSON
           PERSON23 - PERSON
           PERSON24 - PERSON
           PERSON25 - PERSON
           CITY0 - CITY
           CITY1 - CITY
           CITY2 - CITY
           CITY3 - CITY
           CITY4 - CITY
           CITY5 - CITY
           CITY6 - CITY
           CITY7 - CITY
           CITY8 - CITY
           CITY9 - CITY
           CITY10 - CITY
           CITY11 - CITY
           CITY12 - CITY
           CITY13 - CITY
           CITY14 - CITY
           CITY15 - CITY
           CITY16 - CITY
           CITY17 - CITY
           CITY18 - CITY
           CITY19 - CITY
           CITY20 - CITY
           CITY21 - CITY
           FL0 - FLEVEL
           FL1 - FLEVEL
           FL2 - FLEVEL
           FL3 - FLEVEL
           FL4 - FLEVEL
           FL5 - FLEVEL
           FL6 - FLEVEL)

 (:INIT
  (AT PLANE1 CITY0)
  (FUEL-LEVEL PLANE1 FL6)
  (AT PLANE2 CITY6)
  (FUEL-LEVEL PLANE2 FL0)
  (AT PLANE3 CITY18)
  (FUEL-LEVEL PLANE3 FL0)
  (AT PLANE4 CITY11)
  (FUEL-LEVEL PLANE4 FL6)
  (AT PLANE5 CITY9)
  (FUEL-LEVEL PLANE5 FL4)
  (AT PERSON1 CITY12)
  (AT PERSON2 CITY13)
  (AT PERSON3 CITY12)
  (AT PERSON4 CITY1)
  (AT PERSON5 CITY20)
  (AT PERSON6 CITY13)
  (AT PERSON7 CITY13)
  (AT PERSON8 CITY4)
  (AT PERSON9 CITY7)
  (AT PERSON10 CITY7)
  (AT PERSON11 CITY8)
  (AT PERSON12 CITY14)
  (AT PERSON13 CITY1)
  (AT PERSON14 CITY14)
  (AT PERSON15 CITY2)
  (AT PERSON16 CITY21)
  (AT PERSON17 CITY8)
  (AT PERSON18 CITY4)
  (AT PERSON19 CITY8)
  (AT PERSON20 CITY17)
  (AT PERSON21 CITY5)
  (AT PERSON22 CITY21)
  (AT PERSON23 CITY15)
  (AT PERSON24 CITY6)
  (AT PERSON25 CITY5)
  (NEXT FL0 FL1)
  (NEXT FL1 FL2)
  (NEXT FL2 FL3)
  (NEXT FL3 FL4)
  (NEXT FL4 FL5)
  (NEXT FL5 FL6))
 (:GOAL
  (AND (AT PERSON1 CITY5)
       (AT PERSON2 CITY0)
       (AT PERSON3 CITY18)
       (AT PERSON4 CITY7)
       (AT PERSON5 CITY8)
       (AT PERSON6 CITY4)
       (AT PERSON7 CITY12)
       (AT PERSON8 CITY16)
       (AT PERSON9 CITY20)
       (AT PERSON10 CITY5)
       (AT PERSON11 CITY18)
       (AT PERSON12 CITY10)
       (AT PERSON13 CITY0)
       (AT PERSON14 CITY1)
       (AT PERSON15 CITY0)
       (AT PERSON16 CITY13)
       (AT PERSON17 CITY4)
       (AT PERSON18 CITY9)
       (AT PERSON19 CITY16)
       (AT PERSON20 CITY1)
       (AT PERSON21 CITY10)
       (AT PERSON22 CITY2)
       (AT PERSON23 CITY4)
       (AT PERSON24 CITY18)
       (AT PERSON25 CITY21))))