(DEFINE (PROBLEM STRIPS-MYSTY-X-19)
 (:DOMAIN MYSTERY-STRIPS)
 (:OBJECTS BACON - FOOD
           TOFU - FOOD
           LAMB - FOOD
           LOBSTER - FOOD
           BEEF - FOOD
           CANTELOPE - FOOD
           ONION - FOOD
           WONDERBREAD - FOOD
           LEMON - FOOD
           HAROSET - FOOD
           MARZIPAN - FOOD
           FLOUNDER - FOOD
           OKRA - FOOD
           SNICKERS - FOOD
           HAM - FOOD
           AESTHETICS - PLEASURE
           STIMULATION - PLEASURE
           ENTERTAINMENT - PLEASURE
           ACHIEVEMENT - PLEASURE
           LUBRICITY - PLEASURE
           LOVE - PLEASURE
           EXPECTATION - PLEASURE
           LEARNING - PLEASURE
           SCIATICA - PAIN
           LONELINESS - PAIN
           LACERATION - PAIN
           GRIEF - PAIN
           ABRASION - PAIN
           ANXIETY - PAIN
           DREAD - PAIN
           BOILS - PAIN
           JEALOUSY - PAIN
           PROSTATITIS - PAIN
           ANGER - PAIN
           HANGOVER - PAIN
           DEPRESSION - PAIN
           ANGINA - PAIN
           BAVARIA - OBJECT
           MANITOBA - OBJECT
           PENNSYLVANIA - OBJECT
           OREGON - OBJECT
           GOIAS - OBJECT
           BOSNIA - OBJECT
           VULCAN - PLANET
           URANUS - PLANET
           SATURN - PLANET
           EARTH - PLANET)

 (:INIT
  (PROVINCE BAVARIA)
  (PROVINCE MANITOBA)
  (PROVINCE PENNSYLVANIA)
  (PROVINCE OREGON)
  (PROVINCE GOIAS)
  (PROVINCE BOSNIA)
  (LOCALE CANTELOPE BAVARIA)
  (HARMONY ACHIEVEMENT SATURN)
  (HARMONY LOVE URANUS)
  (LOCALE ONION BAVARIA)
  (EATS LOBSTER BACON)
  (CRAVES JEALOUSY HAROSET)
  (CRAVES LOVE HAROSET)
  (CRAVES SCIATICA BACON)
  (CRAVES ABRASION BEEF)
  (EATS LOBSTER HAROSET)
  (EATS LEMON HAM)
  (HARMONY STIMULATION EARTH)
  (EATS WONDERBREAD BACON)
  (HARMONY LUBRICITY EARTH)
  (CRAVES HANGOVER OKRA)
  (CRAVES STIMULATION LAMB)
  (EATS OKRA SNICKERS)
  (LOCALE MARZIPAN OREGON)
  (EATS BEEF LEMON)
  (EATS HAROSET FLOUNDER)
  (EATS LEMON CANTELOPE)
  (EATS MARZIPAN TOFU)
  (HARMONY EXPECTATION SATURN)
  (CRAVES DREAD WONDERBREAD)
  (ATTACKS PENNSYLVANIA OREGON)
  (EATS WONDERBREAD OKRA)
  (EATS ONION TOFU)
  (EATS MARZIPAN CANTELOPE)
  (LOCALE LEMON MANITOBA)
  (LOCALE OKRA MANITOBA)
  (EATS LEMON BEEF)
  (CRAVES ANGINA HAM)
  (EATS BACON WONDERBREAD)
  (EATS OKRA WONDERBREAD)
  (EATS BEEF LAMB)
  (CRAVES LACERATION LAMB)
  (EATS LAMB BEEF)
  (EATS WONDERBREAD MARZIPAN)
  (EATS SNICKERS WONDERBREAD)
  (LOCALE LOBSTER MANITOBA)
  (EATS HAROSET LAMB)
  (CRAVES LONELINESS TOFU)
  (LOCALE BACON MANITOBA)
  (EATS TOFU MARZIPAN)
  (EATS MARZIPAN WONDERBREAD)
  (CRAVES LUBRICITY LEMON)
  (EATS CANTELOPE LEMON)
  (EATS BEEF MARZIPAN)
  (LOCALE HAROSET MANITOBA)
  (ATTACKS GOIAS BOSNIA)
  (CRAVES DEPRESSION SNICKERS)
  (EATS BACON LOBSTER)
  (ATTACKS MANITOBA PENNSYLVANIA)
  (EATS LAMB HAROSET)
  (EATS SNICKERS OKRA)
  (EATS FLOUNDER HAM)
  (ATTACKS BAVARIA MANITOBA)
  (LOCALE BEEF OREGON)
  (CRAVES ANGER FLOUNDER)
  (LOCALE LAMB BOSNIA)
  (HARMONY ENTERTAINMENT URANUS)
  (ORBITS URANUS SATURN)
  (HARMONY LEARNING SATURN)
  (ORBITS VULCAN URANUS)
  (CRAVES ANXIETY ONION)
  (EATS CANTELOPE MARZIPAN)
  (CRAVES LEARNING SNICKERS)
  (HARMONY AESTHETICS EARTH)
  (LOCALE FLOUNDER OREGON)
  (EATS HAM LEMON)
  (CRAVES GRIEF LOBSTER)
  (EATS MARZIPAN BEEF)
  (LOCALE HAM BOSNIA)
  (LOCALE WONDERBREAD BAVARIA)
  (LOCALE TOFU MANITOBA)
  (CRAVES EXPECTATION OKRA)
  (CRAVES ACHIEVEMENT WONDERBREAD)
  (EATS HAROSET LOBSTER)
  (LOCALE SNICKERS MANITOBA)
  (ORBITS SATURN EARTH)
  (ATTACKS OREGON GOIAS)
  (EATS ONION SNICKERS)
  (EATS FLOUNDER HAROSET)
  (CRAVES BOILS LEMON)
  (EATS WONDERBREAD SNICKERS)
  (EATS TOFU ONION)
  (EATS SNICKERS ONION)
  (CRAVES PROSTATITIS MARZIPAN)
  (CRAVES AESTHETICS BACON)
  (EATS HAM FLOUNDER)
  (CRAVES ENTERTAINMENT LOBSTER))
 (:GOAL (AND (CRAVES ABRASION LOBSTER))))