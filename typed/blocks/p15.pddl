(DEFINE (PROBLEM BLOCKS-8-0)
 (:DOMAIN BLOCKS)
 (:OBJECTS H - OBJECT
           G - OBJECT
           F - OBJECT
           E - OBJECT
           C - OBJECT
           B - OBJECT
           D - OBJECT
           A - OBJECT)

 (:INIT
  (CLEAR A)
  (CLEAR D)
  (CLEAR B)
  (CLEAR C)
  (ONTABLE E)
  (ONTABLE F)
  (ONTABLE B)
  (ONTABLE C)
  (ON A G)
  (ON G E)
  (ON D H)
  (ON H F)
  (HANDEMPTY))
 (:GOAL
  (AND (ON D F)
       (ON F E)
       (ON E H)
       (ON H C)
       (ON C A)
       (ON A G)
       (ON G B))))