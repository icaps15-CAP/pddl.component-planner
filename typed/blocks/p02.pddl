(DEFINE (PROBLEM BLOCKS-5-2)
 (:DOMAIN BLOCKS)
 (:OBJECTS A - OBJECT C - OBJECT E - OBJECT B - OBJECT D - OBJECT)
 (:INIT
  (CLEAR D)
  (ONTABLE B)
  (ON D E)
  (ON E C)
  (ON C A)
  (ON A B)
  (HANDEMPTY))
 (:GOAL
  (AND (ON D C)
       (ON C B)
       (ON B E)
       (ON E A))))