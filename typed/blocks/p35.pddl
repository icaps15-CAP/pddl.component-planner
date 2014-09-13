(DEFINE (PROBLEM BLOCKS-17-0)
 (:DOMAIN BLOCKS)
 (:OBJECTS C - OBJECT
           D - OBJECT
           E - OBJECT
           F - OBJECT
           B - OBJECT
           I - OBJECT
           J - OBJECT
           A - OBJECT
           N - OBJECT
           O - OBJECT
           K - OBJECT
           M - OBJECT
           P - OBJECT
           H - OBJECT
           G - OBJECT
           L - OBJECT
           Q - OBJECT)

 (:INIT
  (CLEAR Q)
  (CLEAR L)
  (CLEAR G)
  (CLEAR H)
  (CLEAR P)
  (ONTABLE M)
  (ONTABLE K)
  (ONTABLE O)
  (ONTABLE N)
  (ONTABLE P)
  (ON Q A)
  (ON A J)
  (ON J I)
  (ON I B)
  (ON B M)
  (ON L F)
  (ON F E)
  (ON E K)
  (ON G D)
  (ON D C)
  (ON C O)
  (ON H N)
  (HANDEMPTY))
 (:GOAL
  (AND (ON Q N)
       (ON N L)
       (ON L O)
       (ON O J)
       (ON J H)
       (ON H C)
       (ON C E)
       (ON E M)
       (ON M P)
       (ON P A)
       (ON A G)
       (ON G B)
       (ON B I)
       (ON I K)
       (ON K F)
       (ON F D))))