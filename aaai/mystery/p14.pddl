(DEFINE (PROBLEM STRIPS-MYSTY-X-16)
 (:DOMAIN MYSTERY-STRIPS)
 (:OBJECTS CHICKEN - FOOD
           PEPPER - FOOD
           GUAVA - FOOD
           MARZIPAN - FOOD
           BACON - FOOD
           ARUGULA - FOOD
           FLOUNDER - FOOD
           WONDERBREAD - FOOD
           BAGUETTE - FOOD
           RICE - FOOD
           CHOCOLATE - FOOD
           REST - PLEASURE
           STIMULATION - PLEASURE
           EXCITEMENT - PLEASURE
           SATIETY - PLEASURE
           LEARNING - PLEASURE
           ACHIEVEMENT - PLEASURE
           TRIUMPH - PLEASURE
           PROSTATITIS - PAIN
           BOILS - PAIN
           LONELINESS - PAIN
           DEPRESSION - PAIN
           ANGINA - PAIN
           JEALOUSY - PAIN
           ABRASION - PAIN
           SCIATICA - PAIN
           GRIEF - PAIN
           MANITOBA - OBJECT
           ALSACE - OBJECT
           BOSNIA - OBJECT
           KENTUCKY - OBJECT
           PENNSYLVANIA - OBJECT
           GOIAS - OBJECT
           MERCURY - PLANET
           NEPTUNE - PLANET
           VULCAN - PLANET
           EARTH - PLANET)

 (:INIT
  (PROVINCE MANITOBA)
  (PROVINCE ALSACE)
  (PROVINCE BOSNIA)
  (PROVINCE KENTUCKY)
  (PROVINCE PENNSYLVANIA)
  (PROVINCE GOIAS)
  (EATS WONDERBREAD FLOUNDER)
  (LOCALE FLOUNDER ALSACE)
  (HARMONY STIMULATION EARTH)
  (HARMONY LEARNING VULCAN)
  (LOCALE BAGUETTE MANITOBA)
  (EATS CHICKEN BACON)
  (ATTACKS PENNSYLVANIA GOIAS)
  (HARMONY SATIETY NEPTUNE)
  (CRAVES EXCITEMENT ARUGULA)
  (CRAVES ANGINA WONDERBREAD)
  (LOCALE BACON PENNSYLVANIA)
  (CRAVES REST CHICKEN)
  (CRAVES TRIUMPH CHOCOLATE)
  (EATS ARUGULA BAGUETTE)
  (EATS CHOCOLATE FLOUNDER)
  (EATS PEPPER CHOCOLATE)
  (CRAVES DEPRESSION BACON)
  (LOCALE CHICKEN MANITOBA)
  (ORBITS MERCURY NEPTUNE)
  (EATS ARUGULA WONDERBREAD)
  (LOCALE RICE ALSACE)
  (LOCALE CHOCOLATE GOIAS)
  (ATTACKS ALSACE BOSNIA)
  (CRAVES LEARNING BAGUETTE)
  (EATS GUAVA BAGUETTE)
  (ORBITS NEPTUNE VULCAN)
  (ATTACKS MANITOBA ALSACE)
  (ORBITS VULCAN EARTH)
  (ATTACKS KENTUCKY PENNSYLVANIA)
  (EATS BAGUETTE ARUGULA)
  (CRAVES STIMULATION PEPPER)
  (CRAVES SCIATICA BAGUETTE)
  (CRAVES BOILS PEPPER)
  (LOCALE GUAVA PENNSYLVANIA)
  (LOCALE MARZIPAN MANITOBA)
  (CRAVES GRIEF BAGUETTE)
  (EATS PEPPER GUAVA)
  (EATS WONDERBREAD ARUGULA)
  (EATS BACON CHICKEN)
  (EATS RICE BACON)
  (CRAVES PROSTATITIS CHICKEN)
  (EATS CHICKEN MARZIPAN)
  (EATS CHOCOLATE MARZIPAN)
  (CRAVES JEALOUSY WONDERBREAD)
  (EATS MARZIPAN CHICKEN)
  (CRAVES LONELINESS BACON)
  (EATS RICE BAGUETTE)
  (EATS FLOUNDER CHOCOLATE)
  (HARMONY ACHIEVEMENT VULCAN)
  (CRAVES SATIETY WONDERBREAD)
  (CRAVES ABRASION BAGUETTE)
  (HARMONY REST NEPTUNE)
  (EATS GUAVA PEPPER)
  (HARMONY EXCITEMENT EARTH)
  (EATS BAGUETTE GUAVA)
  (EATS CHOCOLATE PEPPER)
  (EATS FLOUNDER WONDERBREAD)
  (LOCALE ARUGULA KENTUCKY)
  (CRAVES ACHIEVEMENT RICE)
  (EATS MARZIPAN CHOCOLATE)
  (EATS BACON RICE)
  (ATTACKS BOSNIA KENTUCKY)
  (EATS BAGUETTE RICE)
  (HARMONY TRIUMPH VULCAN)
  (LOCALE WONDERBREAD BOSNIA)
  (LOCALE PEPPER KENTUCKY))
 (:GOAL
  (AND (CRAVES ABRASION RICE)
       (CRAVES SCIATICA RICE))))