(DEFINE (DOMAIN ZENO-TRAVEL)
 (:TYPES AIRCRAFT - OBJECT PERSON - OBJECT CITY - OBJECT FLEVEL - OBJECT)
 (:PREDICATES
  (NEXT ?L1 - OBJECT ?L2 - OBJECT)
  (FUEL-LEVEL ?A - OBJECT ?L - OBJECT)
  (IN ?P - OBJECT ?A - OBJECT)
  (AT ?X - OBJECT ?C - OBJECT))

 (:ACTION BOARD
  :PARAMETERS (?P - PERSON ?A - AIRCRAFT ?C - CITY)
  :PRECONDITION (AND (AT ?P ?C)
                     (AT ?A ?C))
  :EFFECT (AND (IN ?P ?A)
               (NOT (AT ?P ?C))))

 (:ACTION DEBARK
  :PARAMETERS (?P - PERSON ?A - AIRCRAFT ?C - CITY)
  :PRECONDITION (AND (IN ?P ?A)
                     (AT ?A ?C))
  :EFFECT (AND (AT ?P ?C)
               (NOT (IN ?P ?A))))

 (:ACTION FLY
  :PARAMETERS (?A - AIRCRAFT ?C1 - CITY ?C2 - CITY ?L1 - FLEVEL ?L2 - FLEVEL)
  :PRECONDITION (AND (AT ?A ?C1)
                     (FUEL-LEVEL ?A ?L1)
                     (NEXT ?L2 ?L1))
  :EFFECT (AND (AT ?A ?C2)
               (FUEL-LEVEL ?A ?L2)
               (NOT (AT ?A ?C1))
               (NOT (FUEL-LEVEL ?A ?L1))))

 (:ACTION ZOOM
  :PARAMETERS (?A - AIRCRAFT ?C1 - CITY ?C2 - CITY ?L1 - FLEVEL ?L2 - FLEVEL
               ?L3 - FLEVEL)
  :PRECONDITION (AND (AT ?A ?C1)
                     (FUEL-LEVEL ?A ?L1)
                     (NEXT ?L2 ?L1)
                     (NEXT ?L3 ?L2))
  :EFFECT (AND (AT ?A ?C2)
               (FUEL-LEVEL ?A ?L3)
               (NOT (AT ?A ?C1))
               (NOT (FUEL-LEVEL ?A ?L1))))

 (:ACTION REFUEL
  :PARAMETERS (?A - AIRCRAFT ?C - CITY ?L - FLEVEL ?L1 - FLEVEL)
  :PRECONDITION (AND (FUEL-LEVEL ?A ?L)
                     (NEXT ?L ?L1)
                     (AT ?A ?C))
  :EFFECT (AND (FUEL-LEVEL ?A ?L1)
               (NOT (FUEL-LEVEL ?A ?L)))))
