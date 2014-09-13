(DEFINE (PROBLEM BLOCKS-12-0)
 (:DOMAIN BLOCKS)
 (:OBJECTS I - OBJECT
           D - OBJECT
           B - OBJECT
           E - OBJECT
           K - OBJECT
           G - OBJECT
           A - OBJECT
           F - OBJECT
           C - OBJECT
           J - OBJECT
           L - OBJECT
           H - OBJECT)

 (:INIT
  (CLEAR H)
  (CLEAR L)
  (CLEAR J)
  (ONTABLE C)
  (ONTABLE F)
  (ONTABLE J)
  (ON H A)
  (ON A G)
  (ON G K)
  (ON K E)
  (ON E B)
  (ON B D)
  (ON D I)
  (ON I C)
  (ON L F)
  (HANDEMPTY))
 (:GOAL
  (AND (ON I C)
       (ON C B)
       (ON B L)
       (ON L D)
       (ON D J)
       (ON J E)
       (ON E K)
       (ON K F)
       (ON F A)
       (ON A H)
       (ON H G))))