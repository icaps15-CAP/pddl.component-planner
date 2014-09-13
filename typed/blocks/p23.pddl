(DEFINE (PROBLEM BLOCKS-11-0)
 (:DOMAIN BLOCKS)
 (:OBJECTS F - OBJECT
           A - OBJECT
           K - OBJECT
           H - OBJECT
           G - OBJECT
           E - OBJECT
           D - OBJECT
           I - OBJECT
           C - OBJECT
           J - OBJECT
           B - OBJECT)

 (:INIT
  (CLEAR B)
  (CLEAR J)
  (CLEAR C)
  (ONTABLE I)
  (ONTABLE D)
  (ONTABLE E)
  (ON B G)
  (ON G H)
  (ON H K)
  (ON K A)
  (ON A F)
  (ON F I)
  (ON J D)
  (ON C E)
  (HANDEMPTY))
 (:GOAL
  (AND (ON A J)
       (ON J D)
       (ON D B)
       (ON B H)
       (ON H K)
       (ON K I)
       (ON I F)
       (ON F E)
       (ON E G)
       (ON G C))))