(DEFINE (PROBLEM ZTRAVEL-5-15)
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
           FL0 - FLEVEL
           FL1 - FLEVEL
           FL2 - FLEVEL
           FL3 - FLEVEL
           FL4 - FLEVEL
           FL5 - FLEVEL
           FL6 - FLEVEL)

 (:INIT
  (AT PLANE1 CITY6)
  (FUEL-LEVEL PLANE1 FL2)
  (AT PLANE2 CITY0)
  (FUEL-LEVEL PLANE2 FL3)
  (AT PLANE3 CITY10)
  (FUEL-LEVEL PLANE3 FL5)
  (AT PLANE4 CITY4)
  (FUEL-LEVEL PLANE4 FL4)
  (AT PLANE5 CITY1)
  (FUEL-LEVEL PLANE5 FL6)
  (AT PERSON1 CITY8)
  (AT PERSON2 CITY12)
  (AT PERSON3 CITY0)
  (AT PERSON4 CITY4)
  (AT PERSON5 CITY13)
  (AT PERSON6 CITY7)
  (AT PERSON7 CITY1)
  (AT PERSON8 CITY2)
  (AT PERSON9 CITY1)
  (AT PERSON10 CITY2)
  (AT PERSON11 CITY10)
  (AT PERSON12 CITY7)
  (AT PERSON13 CITY6)
  (AT PERSON14 CITY1)
  (AT PERSON15 CITY13)
  (NEXT FL0 FL1)
  (NEXT FL1 FL2)
  (NEXT FL2 FL3)
  (NEXT FL3 FL4)
  (NEXT FL4 FL5)
  (NEXT FL5 FL6))
 (:GOAL
  (AND (AT PLANE2 CITY12)
       (AT PLANE3 CITY6)
       (AT PERSON1 CITY3)
       (AT PERSON2 CITY4)
       (AT PERSON3 CITY11)
       (AT PERSON4 CITY13)
       (AT PERSON5 CITY11)
       (AT PERSON6 CITY7)
       (AT PERSON7 CITY1)
       (AT PERSON8 CITY11)
       (AT PERSON9 CITY2)
       (AT PERSON10 CITY6)
       (AT PERSON11 CITY0)
       (AT PERSON12 CITY12)
       (AT PERSON13 CITY13)
       (AT PERSON14 CITY4)
       (AT PERSON15 CITY4))))