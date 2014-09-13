(DEFINE (PROBLEM BLOCKS-12-1)
 (:DOMAIN BLOCKS)
 (:OBJECTS E - OBJECT
           L - OBJECT
           A - OBJECT
           B - OBJECT
           F - OBJECT
           I - OBJECT
           H - OBJECT
           G - OBJECT
           D - OBJECT
           J - OBJECT
           K - OBJECT
           C - OBJECT)

 (:INIT
  (CLEAR C)
  (CLEAR K)
  (ONTABLE J)
  (ONTABLE D)
  (ON C G)
  (ON G H)
  (ON H I)
  (ON I F)
  (ON F B)
  (ON B A)
  (ON A L)
  (ON L E)
  (ON E J)
  (ON K D)
  (HANDEMPTY))
 (:GOAL
  (AND (ON J C)
       (ON C E)
       (ON E K)
       (ON K H)
       (ON H A)
       (ON A F)
       (ON F L)
       (ON L G)
       (ON G B)
       (ON B I)
       (ON I D))))