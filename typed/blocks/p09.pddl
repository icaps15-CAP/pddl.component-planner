(DEFINE (PROBLEM BLOCKS-7-0)
 (:DOMAIN BLOCKS)
 (:OBJECTS C - OBJECT
           F - OBJECT
           A - OBJECT
           B - OBJECT
           G - OBJECT
           D - OBJECT
           E - OBJECT)

 (:INIT
  (CLEAR E)
  (ONTABLE D)
  (ON E G)
  (ON G B)
  (ON B A)
  (ON A F)
  (ON F C)
  (ON C D)
  (HANDEMPTY))
 (:GOAL
  (AND (ON A G)
       (ON G D)
       (ON D B)
       (ON B C)
       (ON C F)
       (ON F E))))