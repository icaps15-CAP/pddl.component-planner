(DEFINE (PROBLEM BLOCKS-16-1)
 (:DOMAIN BLOCKS)
 (:OBJECTS K - OBJECT
           C - OBJECT
           D - OBJECT
           B - OBJECT
           I - OBJECT
           N - OBJECT
           P - OBJECT
           J - OBJECT
           M - OBJECT
           L - OBJECT
           G - OBJECT
           E - OBJECT
           A - OBJECT
           O - OBJECT
           H - OBJECT
           F - OBJECT)

 (:INIT
  (CLEAR F)
  (CLEAR H)
  (CLEAR O)
  (ONTABLE A)
  (ONTABLE E)
  (ONTABLE G)
  (ON F L)
  (ON L M)
  (ON M J)
  (ON J P)
  (ON P N)
  (ON N I)
  (ON I B)
  (ON B D)
  (ON D C)
  (ON C K)
  (ON K A)
  (ON H E)
  (ON O G)
  (HANDEMPTY))
 (:GOAL
  (AND (ON D B)
       (ON B P)
       (ON P F)
       (ON F G)
       (ON G K)
       (ON K I)
       (ON I L)
       (ON L J)
       (ON J H)
       (ON H A)
       (ON A N)
       (ON N E)
       (ON E M)
       (ON M C)
       (ON C O))))