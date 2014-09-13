(DEFINE (PROBLEM BLOCKS-10-2)
 (:DOMAIN BLOCKS)
 (:OBJECTS B - OBJECT
           G - OBJECT
           E - OBJECT
           D - OBJECT
           F - OBJECT
           H - OBJECT
           I - OBJECT
           A - OBJECT
           C - OBJECT
           J - OBJECT)

 (:INIT
  (CLEAR J)
  (CLEAR C)
  (ONTABLE A)
  (ONTABLE C)
  (ON J I)
  (ON I H)
  (ON H F)
  (ON F D)
  (ON D E)
  (ON E G)
  (ON G B)
  (ON B A)
  (HANDEMPTY))
 (:GOAL
  (AND (ON B E)
       (ON E I)
       (ON I G)
       (ON G H)
       (ON H C)
       (ON C A)
       (ON A F)
       (ON F J)
       (ON J D))))