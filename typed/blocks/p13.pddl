(DEFINE (PROBLEM BLOCKS-9-1)
 (:DOMAIN BLOCKS)
 (:OBJECTS H - OBJECT
           G - OBJECT
           I - OBJECT
           C - OBJECT
           D - OBJECT
           B - OBJECT
           E - OBJECT
           A - OBJECT
           F - OBJECT)

 (:INIT
  (CLEAR F)
  (ONTABLE A)
  (ON F E)
  (ON E B)
  (ON B D)
  (ON D C)
  (ON C I)
  (ON I G)
  (ON G H)
  (ON H A)
  (HANDEMPTY))
 (:GOAL
  (AND (ON D I)
       (ON I A)
       (ON A B)
       (ON B H)
       (ON H G)
       (ON G F)
       (ON F E)
       (ON E C))))