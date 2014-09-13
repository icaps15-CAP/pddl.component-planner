(DEFINE (PROBLEM BLOCKS-9-2)
 (:DOMAIN BLOCKS)
 (:OBJECTS B - OBJECT
           I - OBJECT
           C - OBJECT
           E - OBJECT
           D - OBJECT
           A - OBJECT
           G - OBJECT
           F - OBJECT
           H - OBJECT)

 (:INIT
  (CLEAR H)
  (CLEAR F)
  (ONTABLE G)
  (ONTABLE F)
  (ON H A)
  (ON A D)
  (ON D E)
  (ON E C)
  (ON C I)
  (ON I B)
  (ON B G)
  (HANDEMPTY))
 (:GOAL
  (AND (ON F G)
       (ON G H)
       (ON H D)
       (ON D I)
       (ON I E)
       (ON E B)
       (ON B C)
       (ON C A))))