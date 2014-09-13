(DEFINE (PROBLEM STRIPS-MYSTY-X-25)
 (:DOMAIN MYSTERY-STRIPS)
 (:OBJECTS WURST - FOOD
           TUNA - FOOD
           PISTACHIO - FOOD
           CHICKEN - FOOD
           EXPECTATION - PLEASURE
           REST - PLEASURE
           DEPRESSION - PAIN
           ANGINA - PAIN
           BOSNIA - OBJECT
           KENTUCKY - OBJECT
           BAVARIA - OBJECT
           PENNSYLVANIA - OBJECT
           SURREY - OBJECT
           MORAVIA - OBJECT
           JUPITER - PLANET
           URANUS - PLANET
           NEPTUNE - PLANET
           EARTH - PLANET)

 (:INIT
  (PROVINCE BOSNIA)
  (PROVINCE KENTUCKY)
  (PROVINCE BAVARIA)
  (PROVINCE PENNSYLVANIA)
  (PROVINCE SURREY)
  (PROVINCE MORAVIA)
  (EATS WURST CHICKEN)
  (EATS TUNA PISTACHIO)
  (CRAVES ANGINA CHICKEN)
  (EATS CHICKEN PISTACHIO)
  (CRAVES REST PISTACHIO)
  (LOCALE TUNA BAVARIA)
  (EATS CHICKEN WURST)
  (HARMONY EXPECTATION URANUS)
  (ORBITS JUPITER URANUS)
  (CRAVES EXPECTATION TUNA)
  (ATTACKS KENTUCKY BAVARIA)
  (CRAVES DEPRESSION WURST)
  (EATS PISTACHIO WURST)
  (ATTACKS BOSNIA KENTUCKY)
  (ORBITS NEPTUNE EARTH)
  (EATS TUNA WURST)
  (LOCALE WURST BAVARIA)
  (EATS PISTACHIO TUNA)
  (ATTACKS PENNSYLVANIA SURREY)
  (EATS WURST TUNA)
  (HARMONY REST EARTH)
  (ORBITS URANUS NEPTUNE)
  (EATS WURST PISTACHIO)
  (EATS PISTACHIO CHICKEN)
  (ATTACKS SURREY MORAVIA)
  (ATTACKS BAVARIA PENNSYLVANIA)
  (LOCALE CHICKEN BAVARIA)
  (LOCALE PISTACHIO MORAVIA))
 (:GOAL (AND (CRAVES DEPRESSION CHICKEN))))