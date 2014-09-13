(DEFINE (PROBLEM BLOCKS-15-1)
 (:DOMAIN BLOCKS)
 (:OBJECTS J - OBJECT
           B - OBJECT
           K - OBJECT
           A - OBJECT
           D - OBJECT
           H - OBJECT
           E - OBJECT
           N - OBJECT
           C - OBJECT
           F - OBJECT
           L - OBJECT
           M - OBJECT
           I - OBJECT
           O - OBJECT
           G - OBJECT)

 (:INIT
  (CLEAR G)
  (CLEAR O)
  (ONTABLE I)
  (ONTABLE M)
  (ON G L)
  (ON L F)
  (ON F C)
  (ON C N)
  (ON N E)
  (ON E H)
  (ON H D)
  (ON D A)
  (ON A K)
  (ON K B)
  (ON B J)
  (ON J I)
  (ON O M)
  (HANDEMPTY))
 (:GOAL
  (AND (ON D G)
       (ON G F)
       (ON F K)
       (ON K J)
       (ON J E)
       (ON E M)
       (ON M A)
       (ON A B)
       (ON B C)
       (ON C N)
       (ON N O)
       (ON O I)
       (ON I L)
       (ON L H))))