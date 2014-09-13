(DEFINE (PROBLEM BLOCKS-13-0)
 (:DOMAIN BLOCKS)
 (:OBJECTS L - OBJECT
           H - OBJECT
           E - OBJECT
           A - OBJECT
           J - OBJECT
           C - OBJECT
           D - OBJECT
           F - OBJECT
           G - OBJECT
           K - OBJECT
           M - OBJECT
           I - OBJECT
           B - OBJECT)

 (:INIT
  (CLEAR B)
  (CLEAR I)
  (CLEAR M)
  (ONTABLE K)
  (ONTABLE G)
  (ONTABLE M)
  (ON B F)
  (ON F D)
  (ON D C)
  (ON C J)
  (ON J A)
  (ON A E)
  (ON E H)
  (ON H L)
  (ON L K)
  (ON I G)
  (HANDEMPTY))
 (:GOAL
  (AND (ON G I)
       (ON I C)
       (ON C D)
       (ON D F)
       (ON F A)
       (ON A M)
       (ON M H)
       (ON H E)
       (ON E L)
       (ON L J)
       (ON J B)
       (ON B K))))