(DEFINE (PROBLEM STRIPS-MYSTY-X-9)
 (:DOMAIN MYSTERY-STRIPS)
 (:OBJECTS APPLE - FOOD
           FLOUNDER - FOOD
           HAROSET - FOOD
           HAMBURGER - FOOD
           WURST - FOOD
           HOTDOG - FOOD
           GUAVA - FOOD
           ENTERTAINMENT - PLEASURE
           INTOXICATION - PLEASURE
           CURIOSITY - PLEASURE
           EXCITEMENT - PLEASURE
           DREAD - PAIN
           SCIATICA - PAIN
           ABRASION - PAIN
           PROSTATITIS - PAIN
           LONELINESS - PAIN
           ANGER - PAIN
           HANGOVER - PAIN
           ANXIETY - PAIN
           LACERATION - PAIN
           BOILS - PAIN
           JEALOUSY - PAIN
           ANGINA - PAIN
           GRIEF - PAIN
           BOSNIA - OBJECT
           BAVARIA - OBJECT
           ARIZONA - OBJECT
           MANITOBA - OBJECT
           KENTUCKY - OBJECT
           EARTH - PLANET
           URANUS - PLANET
           SATURN - PLANET
           VENUS - PLANET)

 (:INIT
  (PROVINCE BOSNIA)
  (PROVINCE BAVARIA)
  (PROVINCE ARIZONA)
  (PROVINCE MANITOBA)
  (PROVINCE KENTUCKY)
  (EATS APPLE GUAVA)
  (EATS GUAVA APPLE)
  (LOCALE HOTDOG BAVARIA)
  (LOCALE WURST BAVARIA)
  (CRAVES HANGOVER HAROSET)
  (CRAVES LONELINESS HAROSET)
  (EATS HAMBURGER HAROSET)
  (HARMONY INTOXICATION URANUS)
  (CRAVES LACERATION WURST)
  (CRAVES ANXIETY WURST)
  (EATS FLOUNDER HAMBURGER)
  (LOCALE HAMBURGER BOSNIA)
  (HARMONY EXCITEMENT URANUS)
  (CRAVES ANGER HAROSET)
  (CRAVES CURIOSITY HOTDOG)
  (CRAVES DREAD FLOUNDER)
  (ATTACKS BAVARIA ARIZONA)
  (HARMONY ENTERTAINMENT VENUS)
  (CRAVES INTOXICATION HAROSET)
  (EATS HAMBURGER FLOUNDER)
  (CRAVES ANGINA GUAVA)
  (EATS HAROSET GUAVA)
  (ATTACKS BOSNIA BAVARIA)
  (EATS HOTDOG WURST)
  (EATS GUAVA HAROSET)
  (EATS HOTDOG APPLE)
  (CRAVES EXCITEMENT GUAVA)
  (CRAVES JEALOUSY GUAVA)
  (CRAVES SCIATICA FLOUNDER)
  (CRAVES GRIEF GUAVA)
  (EATS FLOUNDER WURST)
  (CRAVES BOILS GUAVA)
  (EATS APPLE HOTDOG)
  (EATS WURST FLOUNDER)
  (CRAVES ENTERTAINMENT FLOUNDER)
  (ATTACKS ARIZONA MANITOBA)
  (ORBITS EARTH URANUS)
  (LOCALE HAROSET ARIZONA)
  (EATS WURST HOTDOG)
  (HARMONY CURIOSITY URANUS)
  (ORBITS URANUS SATURN)
  (CRAVES PROSTATITIS HAROSET)
  (ORBITS SATURN VENUS)
  (CRAVES ABRASION HAROSET)
  (LOCALE FLOUNDER ARIZONA)
  (LOCALE GUAVA KENTUCKY)
  (ATTACKS MANITOBA KENTUCKY)
  (LOCALE APPLE ARIZONA)
  (EATS HAROSET HAMBURGER))
 (:GOAL
  (AND (CRAVES SCIATICA HAMBURGER)
       (CRAVES JEALOUSY WURST))))