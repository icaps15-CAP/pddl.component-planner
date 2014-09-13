(DEFINE (PROBLEM STRIPS-MYSTY-X-24)
 (:DOMAIN MYSTERY-STRIPS)
 (:OBJECTS TURKEY - FOOD
           CHERRY - FOOD
           TOFU - FOOD
           LETTUCE - FOOD
           BACON - FOOD
           CHICKEN - FOOD
           MUFFIN - FOOD
           CHOCOLATE - FOOD
           PEPPER - FOOD
           SHRIMP - FOOD
           ORANGE - FOOD
           APPLE - FOOD
           WURST - FOOD
           POTATO - FOOD
           MELON - FOOD
           ACHIEVEMENT - PLEASURE
           SATIETY - PLEASURE
           CURIOSITY - PLEASURE
           GRIEF - PAIN
           ANGINA - PAIN
           DEPRESSION - PAIN
           SCIATICA - PAIN
           LONELINESS - PAIN
           JEALOUSY - PAIN
           ANXIETY - PAIN
           BOILS - PAIN
           DREAD - PAIN
           HANGOVER - PAIN
           LACERATION - PAIN
           PROSTATITIS - PAIN
           ABRASION - PAIN
           ANGER - PAIN
           ABRASION-2 - PAIN
           JEALOUSY-8 - PAIN
           PROSTATITIS-3 - PAIN
           ANXIETY-4 - PAIN
           LONELINESS-1 - PAIN
           ANGINA-16 - PAIN
           BOILS-5 - PAIN
           DREAD-6 - PAIN
           LACERATION-7 - PAIN
           HANGOVER-13 - PAIN
           DEPRESSION-14 - PAIN
           GRIEF-15 - PAIN
           SCIATICA-11 - PAIN
           ANGER-12 - PAIN
           ANGINA-32 - PAIN
           ANGER-9 - PAIN
           HANGOVER-10 - PAIN
           LONELINESS-31 - PAIN
           ANXIETY-26 - PAIN
           ABRASION-27 - PAIN
           DREAD-28 - PAIN
           SCIATICA-29 - PAIN
           DEPRESSION-30 - PAIN
           SURREY - OBJECT
           ARIZONA - OBJECT
           ALSACE - OBJECT
           GOIAS - OBJECT
           MANITOBA - OBJECT
           BOSNIA - OBJECT
           OREGON - OBJECT
           MORAVIA - OBJECT
           GUANABARA - OBJECT
           BAVARIA - OBJECT
           VULCAN - PLANET
           URANUS - PLANET
           SATURN - PLANET
           EARTH - PLANET)

 (:INIT
  (PROVINCE SURREY)
  (PROVINCE ARIZONA)
  (PROVINCE ALSACE)
  (PROVINCE GOIAS)
  (PROVINCE MANITOBA)
  (PROVINCE BOSNIA)
  (PROVINCE OREGON)
  (PROVINCE MORAVIA)
  (PROVINCE GUANABARA)
  (PROVINCE BAVARIA)
  (HARMONY SATIETY SATURN)
  (CRAVES ANGER-9 WURST)
  (EATS POTATO WURST)
  (CRAVES CURIOSITY MUFFIN)
  (CRAVES ABRASION-27 MELON)
  (CRAVES LACERATION-7 SHRIMP)
  (EATS LETTUCE BACON)
  (EATS MELON POTATO)
  (EATS MUFFIN TOFU)
  (EATS TURKEY BACON)
  (ORBITS URANUS SATURN)
  (ATTACKS GOIAS MANITOBA)
  (LOCALE SHRIMP ARIZONA)
  (EATS ORANGE SHRIMP)
  (EATS CHERRY CHICKEN)
  (LOCALE TOFU SURREY)
  (HARMONY CURIOSITY EARTH)
  (EATS SHRIMP APPLE)
  (ATTACKS BOSNIA OREGON)
  (CRAVES SATIETY BACON)
  (LOCALE LETTUCE MANITOBA)
  (EATS PEPPER POTATO)
  (ATTACKS OREGON MORAVIA)
  (CRAVES HANGOVER CHICKEN)
  (EATS WURST POTATO)
  (CRAVES ANGER MUFFIN)
  (ATTACKS ARIZONA ALSACE)
  (EATS APPLE SHRIMP)
  (CRAVES DEPRESSION-30 MELON)
  (EATS CHOCOLATE APPLE)
  (EATS CHERRY TOFU)
  (CRAVES ABRASION CHICKEN)
  (ATTACKS SURREY ARIZONA)
  (CRAVES BOILS LETTUCE)
  (EATS TOFU MUFFIN)
  (HARMONY ACHIEVEMENT SATURN)
  (CRAVES JEALOUSY-8 PEPPER)
  (CRAVES GRIEF TURKEY)
  (LOCALE CHOCOLATE MANITOBA)
  (LOCALE CHICKEN ALSACE)
  (CRAVES DREAD LETTUCE)
  (LOCALE APPLE SURREY)
  (CRAVES SCIATICA-11 APPLE)
  (CRAVES ACHIEVEMENT CHERRY)
  (CRAVES DEPRESSION CHERRY)
  (CRAVES DREAD-28 MELON)
  (EATS POTATO PEPPER)
  (LOCALE MUFFIN GOIAS)
  (ORBITS VULCAN URANUS)
  (EATS APPLE CHOCOLATE)
  (CRAVES JEALOUSY LETTUCE)
  (CRAVES BOILS-5 SHRIMP)
  (CRAVES ANXIETY LETTUCE)
  (CRAVES ANXIETY-4 PEPPER)
  (EATS WURST ORANGE)
  (EATS TURKEY CHICKEN)
  (CRAVES ANGINA TURKEY)
  (EATS TOFU CHICKEN)
  (EATS MUFFIN MELON)
  (LOCALE WURST ARIZONA)
  (EATS POTATO MELON)
  (EATS CHOCOLATE PEPPER)
  (EATS CHERRY LETTUCE)
  (LOCALE POTATO BAVARIA)
  (EATS PEPPER MELON)
  (EATS MELON PEPPER)
  (LOCALE ORANGE GOIAS)
  (CRAVES ANGER-12 APPLE)
  (EATS CHICKEN TOFU)
  (EATS ORANGE WURST)
  (EATS MUFFIN APPLE)
  (CRAVES LACERATION CHICKEN)
  (ATTACKS ALSACE GOIAS)
  (LOCALE MELON BAVARIA)
  (EATS SHRIMP ORANGE)
  (LOCALE BACON BOSNIA)
  (EATS MELON MUFFIN)
  (EATS APPLE MUFFIN)
  (CRAVES HANGOVER-10 WURST)
  (CRAVES GRIEF-15 ORANGE)
  (EATS CHICKEN TURKEY)
  (ORBITS SATURN EARTH)
  (ATTACKS MORAVIA GUANABARA)
  (CRAVES LONELINESS TOFU)
  (CRAVES LONELINESS-31 POTATO)
  (CRAVES SCIATICA-29 MELON)
  (CRAVES PROSTATITIS-3 PEPPER)
  (LOCALE TURKEY ALSACE)
  (LOCALE PEPPER BAVARIA)
  (CRAVES HANGOVER-13 ORANGE)
  (CRAVES ABRASION-2 CHOCOLATE)
  (CRAVES SCIATICA TOFU)
  (CRAVES DEPRESSION-14 ORANGE)
  (EATS PEPPER CHOCOLATE)
  (EATS LETTUCE CHERRY)
  (EATS BACON LETTUCE)
  (ATTACKS MANITOBA BOSNIA)
  (LOCALE CHERRY SURREY)
  (CRAVES DREAD-6 SHRIMP)
  (CRAVES ANGINA-32 WURST)
  (CRAVES ANGINA-16 SHRIMP)
  (ATTACKS GUANABARA BAVARIA)
  (CRAVES ANXIETY-26 MELON)
  (EATS BACON TURKEY)
  (CRAVES PROSTATITIS CHICKEN)
  (EATS TOFU CHERRY)
  (EATS CHICKEN CHERRY)
  (CRAVES LONELINESS-1 PEPPER))
 (:GOAL
  (AND (CRAVES JEALOUSY-8 PEPPER)
       (CRAVES ANXIETY-4 PEPPER)
       (CRAVES ANGER-12 CHERRY))))