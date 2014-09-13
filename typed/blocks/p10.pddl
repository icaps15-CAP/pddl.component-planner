(DEFINE (PROBLEM BLOCKS-7-2)
 (:DOMAIN BLOCKS)
 (:OBJECTS E - OBJECT
           G - OBJECT
           C - OBJECT
           D - OBJECT
           F - OBJECT
           A - OBJECT
           B - OBJECT)

 (:INIT
  (CLEAR B)
  (CLEAR A)
  (ONTABLE F)
  (ONTABLE D)
  (ON B C)
  (ON C G)
  (ON G E)
  (ON E F)
  (ON A D)
  (HANDEMPTY))
 (:GOAL
  (AND (ON E B)
       (ON B F)
       (ON F D)
       (ON D A)
       (ON A C)
       (ON C G))))