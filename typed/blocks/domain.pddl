(DEFINE (DOMAIN BLOCKS)
 (:REQUIREMENTS :STRIPS)
 (:PREDICATES
  (ON ?X - OBJECT ?Y - OBJECT)
  (ONTABLE ?X - OBJECT)
  (CLEAR ?X - OBJECT)
  (HANDEMPTY)
  (HOLDING ?X - OBJECT))

 (:ACTION PICK-UP
  :PARAMETERS (?X - OBJECT)
  :PRECONDITION (AND (HANDEMPTY)
                     (ONTABLE ?X)
                     (CLEAR ?X))
  :EFFECT (AND (NOT (ONTABLE ?X))
               (NOT (CLEAR ?X))
               (NOT (HANDEMPTY))
               (HOLDING ?X)))

 (:ACTION PUT-DOWN
  :PARAMETERS (?X - OBJECT)
  :PRECONDITION (AND (HOLDING ?X))
  :EFFECT (AND (NOT (HOLDING ?X))
               (CLEAR ?X)
               (HANDEMPTY)
               (ONTABLE ?X)))

 (:ACTION STACK
  :PARAMETERS (?X - OBJECT ?Y - OBJECT)
  :PRECONDITION (AND (CLEAR ?Y)
                     (HOLDING ?X))
  :EFFECT (AND (NOT (HOLDING ?X))
               (NOT (CLEAR ?Y))
               (CLEAR ?X)
               (HANDEMPTY)
               (ON ?X ?Y)))

 (:ACTION UNSTACK
  :PARAMETERS (?X - OBJECT ?Y - OBJECT)
  :PRECONDITION (AND (HANDEMPTY)
                     (CLEAR ?X)
                     (ON ?X ?Y))
  :EFFECT (AND (HOLDING ?X)
               (CLEAR ?Y)
               (NOT (CLEAR ?X))
               (NOT (HANDEMPTY))
               (NOT (ON ?X ?Y)))))
