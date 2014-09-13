(DEFINE (DOMAIN DEPOT)
 (:TYPES PLACE - OBJECT
         TRUCK - OBJECT
         HOIST - OBJECT
         SURFACE - OBJECT)

 (:PREDICATES
  (CRATE ?Y - OBJECT)
  (PALLET ?X - OBJECT)
  (DISTRIBUTOR ?X - OBJECT)
  (DEPOT ?X - OBJECT)
  (LOCATABLE ?X - OBJECT)
  (CLEAR ?X - OBJECT)
  (AVAILABLE ?X - OBJECT)
  (LIFTING ?X - OBJECT ?Y - OBJECT)
  (IN ?X - OBJECT ?Y - OBJECT)
  (ON ?X - OBJECT ?Y - OBJECT)
  (AT ?X - OBJECT ?Y - OBJECT))

 (:ACTION DRIVE
  :PARAMETERS (?X - TRUCK ?Y - PLACE ?Z - PLACE)
  :PRECONDITION (AND (AT ?X ?Y))
  :EFFECT (AND (AT ?X ?Z)
               (NOT (AT ?X ?Y))))

 (:ACTION LIFT
  :PARAMETERS (?X - HOIST ?Y - OBJECT ?Z - SURFACE ?P - PLACE)
  :PRECONDITION (AND (CRATE ?Y)
                     (AT ?X ?P)
                     (AVAILABLE ?X)
                     (AT ?Y ?P)
                     (ON ?Y ?Z)
                     (CLEAR ?Y))
  :EFFECT (AND (LIFTING ?X ?Y)
               (CLEAR ?Z)
               (NOT (AT ?Y ?P))
               (NOT (CLEAR ?Y))
               (NOT (AVAILABLE ?X))
               (NOT (ON ?Y ?Z))))

 (:ACTION DROP
  :PARAMETERS (?X - HOIST ?Y - OBJECT ?Z - SURFACE ?P - PLACE)
  :PRECONDITION (AND (CRATE ?Y)
                     (AT ?X ?P)
                     (AT ?Z ?P)
                     (CLEAR ?Z)
                     (LIFTING ?X ?Y))
  :EFFECT (AND (AVAILABLE ?X)
               (AT ?Y ?P)
               (CLEAR ?Y)
               (ON ?Y ?Z)
               (NOT (LIFTING ?X ?Y))
               (NOT (CLEAR ?Z))))

 (:ACTION LOAD
  :PARAMETERS (?X - HOIST ?Y - OBJECT ?Z - TRUCK ?P - PLACE)
  :PRECONDITION (AND (CRATE ?Y)
                     (AT ?X ?P)
                     (AT ?Z ?P)
                     (LIFTING ?X ?Y))
  :EFFECT (AND (IN ?Y ?Z)
               (AVAILABLE ?X)
               (NOT (LIFTING ?X ?Y))))

 (:ACTION UNLOAD
  :PARAMETERS (?X - HOIST ?Y - OBJECT ?Z - TRUCK ?P - PLACE)
  :PRECONDITION (AND (CRATE ?Y)
                     (AT ?X ?P)
                     (AT ?Z ?P)
                     (AVAILABLE ?X)
                     (IN ?Y ?Z))
  :EFFECT (AND (LIFTING ?X ?Y)
               (NOT (IN ?Y ?Z))
               (NOT (AVAILABLE ?X)))))
