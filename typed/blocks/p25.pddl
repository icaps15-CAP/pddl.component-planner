(DEFINE (PROBLEM BLOCKS-11-1)
 (:DOMAIN BLOCKS)
 (:OBJECTS B - OBJECT
           C - OBJECT
           E - OBJECT
           A - OBJECT
           H - OBJECT
           K - OBJECT
           I - OBJECT
           G - OBJECT
           D - OBJECT
           F - OBJECT
           J - OBJECT)

 (:INIT
  (CLEAR J)
  (CLEAR F)
  (CLEAR D)
  (CLEAR G)
  (ONTABLE I)
  (ONTABLE K)
  (ONTABLE H)
  (ONTABLE A)
  (ON J I)
  (ON F E)
  (ON E K)
  (ON D C)
  (ON C H)
  (ON G B)
  (ON B A)
  (HANDEMPTY))
 (:GOAL
  (AND (ON B D)
       (ON D J)
       (ON J K)
       (ON K H)
       (ON H A)
       (ON A C)
       (ON C F)
       (ON F G)
       (ON G I)
       (ON I E))))