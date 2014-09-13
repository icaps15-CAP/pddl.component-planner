(DEFINE (PROBLEM BLOCKS-10-1)
 (:DOMAIN BLOCKS)
 (:OBJECTS D - OBJECT
           A - OBJECT
           J - OBJECT
           I - OBJECT
           E - OBJECT
           G - OBJECT
           H - OBJECT
           B - OBJECT
           F - OBJECT
           C - OBJECT)

 (:INIT
  (CLEAR C)
  (CLEAR F)
  (ONTABLE B)
  (ONTABLE H)
  (ON C G)
  (ON G E)
  (ON E I)
  (ON I J)
  (ON J A)
  (ON A B)
  (ON F D)
  (ON D H)
  (HANDEMPTY))
 (:GOAL
  (AND (ON C B)
       (ON B D)
       (ON D F)
       (ON F I)
       (ON I A)
       (ON A E)
       (ON E H)
       (ON H G)
       (ON G J))))