(DEFINE (PROBLEM BLOCKS-14-1)
 (:DOMAIN BLOCKS)
 (:OBJECTS K - OBJECT
           A - OBJECT
           F - OBJECT
           L - OBJECT
           D - OBJECT
           B - OBJECT
           M - OBJECT
           E - OBJECT
           J - OBJECT
           N - OBJECT
           H - OBJECT
           I - OBJECT
           C - OBJECT
           G - OBJECT)

 (:INIT
  (CLEAR G)
  (CLEAR C)
  (CLEAR I)
  (CLEAR H)
  (CLEAR N)
  (ONTABLE J)
  (ONTABLE E)
  (ONTABLE M)
  (ONTABLE B)
  (ONTABLE N)
  (ON G J)
  (ON C E)
  (ON I D)
  (ON D L)
  (ON L M)
  (ON H F)
  (ON F A)
  (ON A K)
  (ON K B)
  (HANDEMPTY))
 (:GOAL
  (AND (ON J D)
       (ON D B)
       (ON B H)
       (ON H M)
       (ON M K)
       (ON K F)
       (ON F G)
       (ON G A)
       (ON A I)
       (ON I E)
       (ON E L)
       (ON L N)
       (ON N C))))