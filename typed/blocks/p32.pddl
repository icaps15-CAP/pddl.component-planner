(DEFINE (PROBLEM BLOCKS-16-2)
 (:DOMAIN BLOCKS)
 (:OBJECTS K - OBJECT
           I - OBJECT
           G - OBJECT
           N - OBJECT
           P - OBJECT
           A - OBJECT
           D - OBJECT
           M - OBJECT
           C - OBJECT
           B - OBJECT
           H - OBJECT
           F - OBJECT
           O - OBJECT
           J - OBJECT
           L - OBJECT
           E - OBJECT)

 (:INIT
  (CLEAR E)
  (CLEAR L)
  (ONTABLE J)
  (ONTABLE O)
  (ON E F)
  (ON F H)
  (ON H B)
  (ON B C)
  (ON C M)
  (ON M D)
  (ON D A)
  (ON A P)
  (ON P N)
  (ON N G)
  (ON G I)
  (ON I K)
  (ON K J)
  (ON L O)
  (HANDEMPTY))
 (:GOAL
  (AND (ON I D)
       (ON D H)
       (ON H F)
       (ON F B)
       (ON B K)
       (ON K J)
       (ON J G)
       (ON G E)
       (ON E C)
       (ON C L)
       (ON L M)
       (ON M N)
       (ON N A)
       (ON A P)
       (ON P O))))