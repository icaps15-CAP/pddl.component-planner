(DEFINE (DOMAIN MYSTERY-STRIPS)
 (:TYPES PLANET - OBJECT FOOD - OBJECT PLEASURE - OBJECT PAIN - OBJECT)
 (:PREDICATES
  (ORBITS ?I - OBJECT ?J - OBJECT)
  (ATTACKS ?I - OBJECT ?J - OBJECT)
  (HARMONY ?V - OBJECT ?S - OBJECT)
  (LOCALE ?N - OBJECT ?A - OBJECT)
  (FEARS ?C - OBJECT ?V - OBJECT)
  (CRAVES ?V - OBJECT ?N - OBJECT)
  (EATS ?N1 - OBJECT ?N2 - OBJECT)
  (PROVINCE ?X - OBJECT))

 (:ACTION OVERCOME
  :PARAMETERS (?C - PAIN ?V - PLEASURE ?N - FOOD ?S1 - PLANET ?S2 - PLANET)
  :PRECONDITION (AND (CRAVES ?C ?N)
                     (CRAVES ?V ?N)
                     (HARMONY ?V ?S2)
                     (ORBITS ?S1 ?S2))
  :EFFECT (AND (NOT (CRAVES ?C ?N))
               (FEARS ?C ?V)
               (NOT (HARMONY ?V ?S2))
               (HARMONY ?V ?S1)))

 (:ACTION FEAST
  :PARAMETERS (?V - PLEASURE ?N1 - FOOD ?N2 - FOOD ?L1 - OBJECT ?L2 - OBJECT)
  :PRECONDITION (AND (CRAVES ?V ?N1)
                     (EATS ?N1 ?N2)
                     (LOCALE ?N1 ?L2)
                     (ATTACKS ?L1 ?L2))
  :EFFECT (AND (NOT (CRAVES ?V ?N1))
               (CRAVES ?V ?N2)
               (NOT (LOCALE ?N1 ?L2))
               (LOCALE ?N1 ?L1)))

 (:ACTION SUCCUMB
  :PARAMETERS (?C - PAIN ?V - PLEASURE ?N - FOOD ?S1 - OBJECT ?S2 - OBJECT)
  :PRECONDITION (AND (FEARS ?C ?V)
                     (CRAVES ?V ?N)
                     (HARMONY ?V ?S1)
                     (ORBITS ?S1 ?S2))
  :EFFECT (AND (NOT (FEARS ?C ?V))
               (CRAVES ?C ?N)
               (NOT (HARMONY ?V ?S1))
               (HARMONY ?V ?S2))))
