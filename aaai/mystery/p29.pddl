(DEFINE (PROBLEM STRIPS-MYSTY-X-13)
 (:DOMAIN MYSTERY-STRIPS)
 (:OBJECTS LETTUCE - FOOD
           CHOCOLATE - FOOD
           MUTTON - FOOD
           MARZIPAN - FOOD
           SCALLOP - FOOD
           YOGURT - FOOD
           RICE - FOOD
           HAROSET - FOOD
           MUFFIN - FOOD
           POTATO - FOOD
           TURKEY - FOOD
           HAM - FOOD
           ONION - FOOD
           MELON - FOOD
           WURST - FOOD
           FLOUNDER - FOOD
           BAGUETTE - FOOD
           SCALLION - FOOD
           HAMBURGER - FOOD
           PAPAYA - FOOD
           LOBSTER - FOOD
           LEMON - FOOD
           ENTERTAINMENT - PLEASURE
           INTOXICATION - PLEASURE
           SATISFACTION - PLEASURE
           ACHIEVEMENT - PLEASURE
           CURIOSITY - PLEASURE
           LOVE - PLEASURE
           AESTHETICS - PLEASURE
           ANGER - PAIN
           JEALOUSY - PAIN
           HANGOVER - PAIN
           GRIEF - PAIN
           ABRASION - PAIN
           LONELINESS - PAIN
           ANXIETY - PAIN
           BOILS - PAIN
           SCIATICA - PAIN
           DREAD - PAIN
           ANGINA - PAIN
           LACERATION - PAIN
           DEPRESSION - PAIN
           GRIEF-1 - PAIN
           ANXIETY-2 - PAIN
           PROSTATITIS - PAIN
           LACERATION-7 - PAIN
           JEALOUSY-8 - PAIN
           BOILS-3 - PAIN
           DREAD-4 - PAIN
           ANGINA-14 - PAIN
           HANGOVER-15 - PAIN
           SCIATICA-16 - PAIN
           DEPRESSION-5 - PAIN
           ANGER-6 - PAIN
           LONELINESS-11 - PAIN
           ABRASION-12 - PAIN
           PROSTATITIS-13 - PAIN
           ANXIETY-9 - PAIN
           SCIATICA-10 - PAIN
           LONELINESS-32 - PAIN
           PROSTATITIS-29 - PAIN
           LACERATION-30 - PAIN
           HANGOVER-31 - PAIN
           DREAD-24 - PAIN
           GRIEF-25 - PAIN
           ABRASION-26 - PAIN
           BOILS-27 - PAIN
           DEPRESSION-28 - PAIN
           ANGER-21 - PAIN
           JEALOUSY-22 - PAIN
           ANGINA-23 - PAIN
           LONELINESS-17 - PAIN
           PROSTATITIS-18 - PAIN
           DEPRESSION-19 - PAIN
           LACERATION-20 - PAIN
           OREGON - OBJECT
           ALSACE - OBJECT
           BAVARIA - OBJECT
           QUEBEC - OBJECT
           MARS - PLANET
           NEPTUNE - PLANET
           VULCAN - PLANET
           VENUS - PLANET)

 (:INIT
  (PROVINCE OREGON)
  (PROVINCE ALSACE)
  (PROVINCE BAVARIA)
  (PROVINCE QUEBEC)
  (CRAVES DEPRESSION RICE)
  (CRAVES DREAD RICE)
  (CRAVES PROSTATITIS-18 LEMON)
  (LOCALE MELON ALSACE)
  (EATS LOBSTER MARZIPAN)
  (CRAVES LACERATION-7 POTATO)
  (CRAVES LONELINESS-32 FLOUNDER)
  (EATS MARZIPAN LOBSTER)
  (LOCALE HAM OREGON)
  (CRAVES ANGER MUTTON)
  (CRAVES SATISFACTION RICE)
  (EATS RICE LETTUCE)
  (LOCALE MARZIPAN OREGON)
  (EATS LOBSTER BAGUETTE)
  (EATS TURKEY POTATO)
  (EATS WURST HAM)
  (EATS MUTTON MARZIPAN)
  (CRAVES ACHIEVEMENT TURKEY)
  (EATS BAGUETTE LOBSTER)
  (EATS PAPAYA HAMBURGER)
  (LOCALE CHOCOLATE BAVARIA)
  (CRAVES SCIATICA-16 TURKEY)
  (EATS BAGUETTE LEMON)
  (CRAVES ABRASION-26 SCALLION)
  (LOCALE BAGUETTE BAVARIA)
  (EATS HAMBURGER PAPAYA)
  (EATS SCALLOP TURKEY)
  (ORBITS VULCAN VENUS)
  (EATS MARZIPAN MUTTON)
  (LOCALE YOGURT ALSACE)
  (CRAVES PROSTATITIS-29 BAGUETTE)
  (CRAVES GRIEF-1 MUFFIN)
  (CRAVES LACERATION-30 BAGUETTE)
  (CRAVES JEALOUSY-8 POTATO)
  (HARMONY LOVE VENUS)
  (EATS FLOUNDER SCALLION)
  (CRAVES DREAD-24 SCALLION)
  (CRAVES INTOXICATION YOGURT)
  (CRAVES BOILS-27 SCALLION)
  (LOCALE LEMON QUEBEC)
  (ORBITS NEPTUNE VULCAN)
  (CRAVES LONELINESS SCALLOP)
  (EATS HAMBURGER SCALLION)
  (LOCALE MUFFIN BAVARIA)
  (LOCALE SCALLOP OREGON)
  (EATS HAROSET WURST)
  (EATS TURKEY ONION)
  (HARMONY CURIOSITY VULCAN)
  (CRAVES GRIEF-25 SCALLION)
  (EATS MELON HAROSET)
  (EATS FLOUNDER LEMON)
  (HARMONY SATISFACTION NEPTUNE)
  (CRAVES ENTERTAINMENT MUTTON)
  (CRAVES ABRASION SCALLOP)
  (EATS POTATO MUFFIN)
  (CRAVES HANGOVER-31 BAGUETTE)
  (EATS LETTUCE RICE)
  (EATS TURKEY SCALLOP)
  (EATS LETTUCE CHOCOLATE)
  (CRAVES GRIEF SCALLOP)
  (CRAVES AESTHETICS HAMBURGER)
  (EATS LEMON ONION)
  (EATS YOGURT SCALLOP)
  (EATS LEMON FLOUNDER)
  (EATS CHOCOLATE LETTUCE)
  (LOCALE ONION ALSACE)
  (EATS ONION LEMON)
  (EATS HAROSET MELON)
  (CRAVES LOVE BAGUETTE)
  (HARMONY INTOXICATION VULCAN)
  (LOCALE FLOUNDER QUEBEC)
  (CRAVES CURIOSITY HAM)
  (LOCALE WURST BAVARIA)
  (CRAVES ABRASION-12 HAM)
  (CRAVES DREAD-4 POTATO)
  (EATS MUTTON SCALLOP)
  (CRAVES HANGOVER SCALLOP)
  (LOCALE TURKEY BAVARIA)
  (EATS RICE MARZIPAN)
  (CRAVES ANXIETY-9 MELON)
  (CRAVES LACERATION RICE)
  (CRAVES ANGER-6 TURKEY)
  (EATS SCALLION FLOUNDER)
  (LOCALE POTATO ALSACE)
  (HARMONY ACHIEVEMENT VENUS)
  (CRAVES JEALOUSY MARZIPAN)
  (CRAVES PROSTATITIS-13 HAM)
  (LOCALE LOBSTER ALSACE)
  (EATS MUFFIN HAM)
  (CRAVES DEPRESSION-28 SCALLION)
  (CRAVES ANGINA-23 HAMBURGER)
  (CRAVES SCIATICA RICE)
  (CRAVES BOILS YOGURT)
  (EATS SCALLOP YOGURT)
  (LOCALE RICE QUEBEC)
  (EATS PAPAYA LOBSTER)
  (ATTACKS BAVARIA QUEBEC)
  (ATTACKS ALSACE BAVARIA)
  (ATTACKS OREGON ALSACE)
  (CRAVES LACERATION-20 LEMON)
  (CRAVES BOILS-3 POTATO)
  (CRAVES LONELINESS-11 HAM)
  (CRAVES JEALOUSY-22 HAMBURGER)
  (CRAVES ANXIETY YOGURT)
  (LOCALE MUTTON QUEBEC)
  (CRAVES HANGOVER-15 TURKEY)
  (EATS YOGURT CHOCOLATE)
  (CRAVES ANGINA RICE)
  (EATS HAM MUFFIN)
  (EATS SCALLOP MUTTON)
  (LOCALE LETTUCE ALSACE)
  (EATS ONION MELON)
  (EATS HAM WURST)
  (EATS CHOCOLATE YOGURT)
  (EATS WURST HAROSET)
  (EATS POTATO TURKEY)
  (LOCALE SCALLION ALSACE)
  (CRAVES DEPRESSION-5 TURKEY)
  (CRAVES PROSTATITIS MUFFIN)
  (HARMONY ENTERTAINMENT VENUS)
  (CRAVES DEPRESSION-19 LEMON)
  (LOCALE PAPAYA BAVARIA)
  (EATS MELON ONION)
  (CRAVES ANGER-21 HAMBURGER)
  (EATS ONION TURKEY)
  (EATS MUFFIN POTATO)
  (LOCALE HAMBURGER BAVARIA)
  (CRAVES ANGINA-14 TURKEY)
  (EATS LOBSTER PAPAYA)
  (EATS MARZIPAN RICE)
  (LOCALE HAROSET OREGON)
  (CRAVES LONELINESS-17 LEMON)
  (HARMONY AESTHETICS NEPTUNE)
  (ORBITS MARS NEPTUNE)
  (CRAVES ANXIETY-2 MUFFIN)
  (EATS SCALLION HAMBURGER)
  (CRAVES SCIATICA-10 MELON)
  (EATS LEMON BAGUETTE))
 (:GOAL
  (AND (CRAVES PROSTATITIS-18 LOBSTER)
       (CRAVES LACERATION-20 HAM))))