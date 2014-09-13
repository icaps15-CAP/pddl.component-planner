(DEFINE (PROBLEM BLOCKS-11-2)
 (:DOMAIN BLOCKS)
 (:OBJECTS E - OBJECT
           J - OBJECT
           D - OBJECT
           C - OBJECT
           F - OBJECT
           K - OBJECT
           H - OBJECT
           G - OBJECT
           A - OBJECT
           I - OBJECT
           B - OBJECT)

 (:INIT
  (CLEAR B)
  (CLEAR I)
  (ONTABLE A)
  (ONTABLE G)
  (ON B H)
  (ON H K)
  (ON K F)
  (ON F C)
  (ON C D)
  (ON D J)
  (ON J A)
  (ON I E)
  (ON E G)
  (HANDEMPTY))
 (:GOAL
  (AND (ON I G)
       (ON G C)
       (ON C D)
       (ON D E)
       (ON E J)
       (ON J B)
       (ON B H)
       (ON H A)
       (ON A F)
       (ON F K))))