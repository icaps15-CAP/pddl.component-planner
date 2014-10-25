(DEFINE (PROBLEM STRIPS-MYSTY-X-4)
 (:DOMAIN MYSTERY-STRIPS)
 (:OBJECTS MUFFIN - FOOD
           HAM - FOOD
           SCALLION - FOOD
           SHRIMP - FOOD
           CHERRY - FOOD
           GRAPEFRUIT - FOOD
           BACON - FOOD
           ARUGULA - FOOD
           SCALLOP - FOOD
           WURST - FOOD
           AESTHETICS - PLEASURE
           HANGOVER - PAIN
           DREAD - PAIN
           SCIATICA - PAIN
           JEALOUSY - PAIN
           LONELINESS - PAIN
           ABRASION - PAIN
           ANGER - PAIN
           SURREY - OBJECT
           QUEBEC - OBJECT
           BOSNIA - OBJECT
           OREGON - OBJECT
           KENTUCKY - OBJECT
           MARS - PLANET
           VULCAN - PLANET)

 (:INIT
  (PROVINCE SURREY)
  (PROVINCE QUEBEC)
  (PROVINCE BOSNIA)
  (PROVINCE OREGON)
  (PROVINCE KENTUCKY)
  (LOCALE CHERRY KENTUCKY)
  (EATS HAM MUFFIN)
  (EATS CHERRY SHRIMP)
  (LOCALE SCALLION QUEBEC)
  (CRAVES DREAD HAM)
  (EATS CHERRY HAM)
  (EATS GRAPEFRUIT SCALLOP)
  (CRAVES SCIATICA GRAPEFRUIT)
  (EATS WURST BACON)
  (EATS MUFFIN HAM)
  (ATTACKS OREGON KENTUCKY)
  (EATS ARUGULA SCALLOP)
  (EATS ARUGULA BACON)
  (EATS BACON WURST)
  (EATS ARUGULA MUFFIN)
  (CRAVES ANGER WURST)
  (EATS SCALLION SHRIMP)
  (EATS ARUGULA WURST)
  (LOCALE ARUGULA KENTUCKY)
  (EATS GRAPEFRUIT WURST)
  (CRAVES LONELINESS ARUGULA)
  (HARMONY AESTHETICS VULCAN)
  (EATS MUFFIN CHERRY)
  (EATS SCALLOP ARUGULA)
  (LOCALE MUFFIN KENTUCKY)
  (LOCALE GRAPEFRUIT SURREY)
  (CRAVES HANGOVER MUFFIN)
  (EATS CHERRY ARUGULA)
  (EATS SHRIMP SCALLION)
  (LOCALE HAM BOSNIA)
  (EATS MUFFIN SCALLION)
  (EATS ARUGULA CHERRY)
  (EATS SCALLOP GRAPEFRUIT)
  (CRAVES ABRASION SCALLOP)
  (EATS BACON ARUGULA)
  (EATS HAM CHERRY)
  (EATS CHERRY MUFFIN)
  (LOCALE BACON QUEBEC)
  (LOCALE WURST SURREY)
  (ATTACKS BOSNIA OREGON)
  (LOCALE SCALLOP OREGON)
  (EATS SHRIMP CHERRY)
  (EATS WURST ARUGULA)
  (ATTACKS QUEBEC BOSNIA)
  (EATS MUFFIN ARUGULA)
  (ATTACKS SURREY QUEBEC)
  (CRAVES AESTHETICS SHRIMP)
  (EATS SCALLION MUFFIN)
  (ORBITS MARS VULCAN)
  (LOCALE SHRIMP BOSNIA)
  (CRAVES JEALOUSY BACON)
  (EATS WURST GRAPEFRUIT))
 (:GOAL (AND (CRAVES SCIATICA WURST))))