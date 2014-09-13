(DEFINE (PROBLEM BLOCKS-7-1)
 (:DOMAIN BLOCKS)
 (:OBJECTS E - OBJECT
           B - OBJECT
           D - OBJECT
           F - OBJECT
           G - OBJECT
           C - OBJECT
           A - OBJECT)

 (:INIT
  (CLEAR A)
  (CLEAR C)
  (ONTABLE G)
  (ONTABLE F)
  (ON A G)
  (ON C D)
  (ON D B)
  (ON B E)
  (ON E F)
  (HANDEMPTY))
 (:GOAL
  (AND (ON A E)
       (ON E B)
       (ON B F)
       (ON F G)
       (ON G C)
       (ON C D))))