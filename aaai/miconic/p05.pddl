(DEFINE (PROBLEM MIXED-F2-P1-U0-V0-G0-A0-N0-A0-B0-N0-F0-R1)
 (:DOMAIN MICONIC)
 (:OBJECTS P0 - PASSENGER F0 - FLOOR F1 - FLOOR)
 (:INIT
  (ABOVE F0 F1)
  (ORIGIN P0 F0)
  (DESTIN P0 F1)
  (LIFT-AT F0))
 (:GOAL (AND (SERVED P0))))