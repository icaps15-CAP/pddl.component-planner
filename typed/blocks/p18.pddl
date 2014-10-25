(DEFINE (PROBLEM BLOCKS-8-2)
 (:DOMAIN BLOCKS)
 (:OBJECTS F - OBJECT
           B - OBJECT
           G - OBJECT
           C - OBJECT
           H - OBJECT
           E - OBJECT
           A - OBJECT
           D - OBJECT)

 (:INIT
  (CLEAR D)
  (CLEAR A)
  (CLEAR E)
  (CLEAR H)
  (CLEAR C)
  (ONTABLE G)
  (ONTABLE A)
  (ONTABLE E)
  (ONTABLE H)
  (ONTABLE C)
  (ON D B)
  (ON B F)
  (ON F G)
  (HANDEMPTY))
 (:GOAL
  (AND (ON C B)
       (ON B E)
       (ON E G)
       (ON G F)
       (ON F A)
       (ON A D)
       (ON D H))))