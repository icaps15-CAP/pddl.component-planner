(DEFINE (PROBLEM BLOCKS-8-1)
 (:DOMAIN BLOCKS)
 (:OBJECTS B - OBJECT
           A - OBJECT
           G - OBJECT
           C - OBJECT
           F - OBJECT
           D - OBJECT
           H - OBJECT
           E - OBJECT)

 (:INIT
  (CLEAR E)
  (CLEAR H)
  (CLEAR D)
  (CLEAR F)
  (ONTABLE C)
  (ONTABLE G)
  (ONTABLE D)
  (ONTABLE F)
  (ON E C)
  (ON H A)
  (ON A B)
  (ON B G)
  (HANDEMPTY))
 (:GOAL
  (AND (ON C D)
       (ON D B)
       (ON B G)
       (ON G F)
       (ON F H)
       (ON H A)
       (ON A E))))