(DEFINE (PROBLEM BLOCKS-14-0)
 (:DOMAIN BLOCKS)
 (:OBJECTS I - OBJECT
           D - OBJECT
           B - OBJECT
           L - OBJECT
           C - OBJECT
           K - OBJECT
           M - OBJECT
           H - OBJECT
           J - OBJECT
           N - OBJECT
           E - OBJECT
           F - OBJECT
           G - OBJECT
           A - OBJECT)

 (:INIT
  (CLEAR A)
  (CLEAR G)
  (CLEAR F)
  (ONTABLE E)
  (ONTABLE N)
  (ONTABLE F)
  (ON A J)
  (ON J H)
  (ON H M)
  (ON M K)
  (ON K C)
  (ON C L)
  (ON L B)
  (ON B E)
  (ON G D)
  (ON D I)
  (ON I N)
  (HANDEMPTY))
 (:GOAL
  (AND (ON E L)
       (ON L F)
       (ON F B)
       (ON B J)
       (ON J I)
       (ON I N)
       (ON N C)
       (ON C K)
       (ON K G)
       (ON G D)
       (ON D M)
       (ON M A)
       (ON A H))))