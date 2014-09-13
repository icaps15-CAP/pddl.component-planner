(DEFINE (PROBLEM STRIPS-MYSTY-X-2)
 (:DOMAIN MYSTERY-STRIPS)
 (:OBJECTS BEEF - FOOD
           ONION - FOOD
           TUNA - FOOD
           FLOUNDER - FOOD
           CHERRY - FOOD
           MUFFIN - FOOD
           HAM - FOOD
           SATIETY - PLEASURE
           STIMULATION - PLEASURE
           CURIOSITY - PLEASURE
           ENTERTAINMENT - PLEASURE
           ANGER - PAIN
           DEPRESSION - PAIN
           PROSTATITIS - PAIN
           GRIEF - PAIN
           ABRASION - PAIN
           LONELINESS - PAIN
           DREAD - PAIN
           ANGINA - PAIN
           BOILS - PAIN
           LACERATION - PAIN
           SCIATICA - PAIN
           HANGOVER - PAIN
           ANXIETY - PAIN
           JEALOUSY - PAIN
           JEALOUSY-2 - PAIN
           DEPRESSION-1 - PAIN
           GRIEF-7 - PAIN
           DREAD-8 - PAIN
           PROSTATITIS-3 - PAIN
           BOILS-4 - PAIN
           ALSACE - OBJECT
           ARIZONA - OBJECT
           KENTUCKY - OBJECT
           BOSNIA - OBJECT
           SURREY - OBJECT
           MERCURY - PLANET
           VULCAN - PLANET
           PLUTO - PLANET
           JUPITER - PLANET)

 (:INIT
  (PROVINCE ALSACE)
  (PROVINCE ARIZONA)
  (PROVINCE KENTUCKY)
  (PROVINCE BOSNIA)
  (PROVINCE SURREY)
  (ORBITS MERCURY VULCAN)
  (EATS ONION MUFFIN)
  (EATS TUNA MUFFIN)
  (EATS MUFFIN HAM)
  (ATTACKS ARIZONA KENTUCKY)
  (LOCALE BEEF SURREY)
  (CRAVES LACERATION TUNA)
  (CRAVES DREAD ONION)
  (LOCALE ONION BOSNIA)
  (LOCALE TUNA KENTUCKY)
  (LOCALE HAM SURREY)
  (EATS FLOUNDER TUNA)
  (CRAVES PROSTATITIS BEEF)
  (CRAVES BOILS-4 HAM)
  (CRAVES ENTERTAINMENT HAM)
  (HARMONY SATIETY VULCAN)
  (CRAVES SATIETY ONION)
  (EATS TUNA HAM)
  (CRAVES STIMULATION FLOUNDER)
  (EATS ONION CHERRY)
  (EATS TUNA FLOUNDER)
  (EATS BEEF TUNA)
  (EATS CHERRY FLOUNDER)
  (CRAVES CURIOSITY CHERRY)
  (ORBITS VULCAN PLUTO)
  (LOCALE FLOUNDER KENTUCKY)
  (HARMONY ENTERTAINMENT JUPITER)
  (ATTACKS ALSACE ARIZONA)
  (CRAVES JEALOUSY FLOUNDER)
  (EATS HAM TUNA)
  (CRAVES LONELINESS ONION)
  (EATS ONION BEEF)
  (EATS CHERRY ONION)
  (HARMONY STIMULATION PLUTO)
  (CRAVES ANXIETY FLOUNDER)
  (ATTACKS KENTUCKY BOSNIA)
  (LOCALE MUFFIN KENTUCKY)
  (CRAVES SCIATICA TUNA)
  (CRAVES GRIEF BEEF)
  (CRAVES GRIEF-7 HAM)
  (HARMONY CURIOSITY PLUTO)
  (CRAVES JEALOUSY-2 CHERRY)
  (CRAVES ANGINA ONION)
  (CRAVES DEPRESSION-1 MUFFIN)
  (EATS MUFFIN TUNA)
  (CRAVES ANGER BEEF)
  (LOCALE CHERRY SURREY)
  (EATS BEEF ONION)
  (CRAVES ABRASION BEEF)
  (ATTACKS BOSNIA SURREY)
  (EATS CHERRY BEEF)
  (EATS BEEF CHERRY)
  (CRAVES HANGOVER TUNA)
  (EATS MUFFIN ONION)
  (ORBITS PLUTO JUPITER)
  (EATS FLOUNDER CHERRY)
  (CRAVES BOILS TUNA)
  (EATS TUNA BEEF)
  (CRAVES PROSTATITIS-3 HAM)
  (EATS HAM MUFFIN)
  (CRAVES DREAD-8 HAM)
  (CRAVES DEPRESSION BEEF))
 (:GOAL
  (AND (CRAVES GRIEF-7 BEEF)
       (CRAVES DEPRESSION-1 BEEF))))