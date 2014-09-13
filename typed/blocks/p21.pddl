(DEFINE (PROBLEM BLOCKS-10-0)
 (:DOMAIN BLOCKS)
 (:OBJECTS D - OBJECT
           A - OBJECT
           H - OBJECT
           G - OBJECT
           B - OBJECT
           J - OBJECT
           E - OBJECT
           I - OBJECT
           F - OBJECT
           C - OBJECT)

 (:INIT
  (CLEAR C)
  (CLEAR F)
  (ONTABLE I)
  (ONTABLE F)
  (ON C E)
  (ON E J)
  (ON J B)
  (ON B G)
  (ON G H)
  (ON H A)
  (ON A D)
  (ON D I)
  (HANDEMPTY))
 (:GOAL
  (AND (ON D C)
       (ON C F)
       (ON F J)
       (ON J E)
       (ON E H)
       (ON H B)
       (ON B A)
       (ON A G)
       (ON G I))))