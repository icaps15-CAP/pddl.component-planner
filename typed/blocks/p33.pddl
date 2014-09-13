(DEFINE (PROBLEM BLOCKS-15-0)
 (:DOMAIN BLOCKS)
 (:OBJECTS A - OBJECT
           C - OBJECT
           L - OBJECT
           D - OBJECT
           J - OBJECT
           H - OBJECT
           K - OBJECT
           O - OBJECT
           N - OBJECT
           G - OBJECT
           I - OBJECT
           F - OBJECT
           B - OBJECT
           M - OBJECT
           E - OBJECT)

 (:INIT
  (CLEAR E)
  (CLEAR M)
  (CLEAR B)
  (CLEAR F)
  (CLEAR I)
  (ONTABLE G)
  (ONTABLE N)
  (ONTABLE O)
  (ONTABLE K)
  (ONTABLE H)
  (ON E J)
  (ON J D)
  (ON D L)
  (ON L C)
  (ON C G)
  (ON M N)
  (ON B A)
  (ON A O)
  (ON F K)
  (ON I H)
  (HANDEMPTY))
 (:GOAL
  (AND (ON G O)
       (ON O H)
       (ON H K)
       (ON K M)
       (ON M F)
       (ON F E)
       (ON E A)
       (ON A B)
       (ON B L)
       (ON L J)
       (ON J D)
       (ON D N)
       (ON N I)
       (ON I C))))