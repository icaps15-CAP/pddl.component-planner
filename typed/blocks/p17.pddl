(DEFINE (PROBLEM BLOCKS-9-0)
 (:DOMAIN BLOCKS)
 (:OBJECTS H - OBJECT
           D - OBJECT
           I - OBJECT
           A - OBJECT
           E - OBJECT
           G - OBJECT
           B - OBJECT
           F - OBJECT
           C - OBJECT)

 (:INIT
  (CLEAR C)
  (CLEAR F)
  (ONTABLE C)
  (ONTABLE B)
  (ON F G)
  (ON G E)
  (ON E A)
  (ON A I)
  (ON I D)
  (ON D H)
  (ON H B)
  (HANDEMPTY))
 (:GOAL
  (AND (ON G D)
       (ON D B)
       (ON B C)
       (ON C A)
       (ON A I)
       (ON I F)
       (ON F E)
       (ON E H))))