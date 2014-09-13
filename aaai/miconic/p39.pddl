(DEFINE (PROBLEM MIXED-F16-P8-U0-V0-G0-A0-N0-A0-B0-N0-F0-R2)
 (:DOMAIN MICONIC)
 (:OBJECTS P0 - PASSENGER
           P1 - PASSENGER
           P2 - PASSENGER
           P3 - PASSENGER
           P4 - PASSENGER
           P5 - PASSENGER
           P6 - PASSENGER
           P7 - PASSENGER
           F0 - FLOOR
           F1 - FLOOR
           F2 - FLOOR
           F3 - FLOOR
           F4 - FLOOR
           F5 - FLOOR
           F6 - FLOOR
           F7 - FLOOR
           F8 - FLOOR
           F9 - FLOOR
           F10 - FLOOR
           F11 - FLOOR
           F12 - FLOOR
           F13 - FLOOR
           F14 - FLOOR
           F15 - FLOOR)

 (:INIT
  (ABOVE F0 F1)
  (ABOVE F0 F2)
  (ABOVE F0 F3)
  (ABOVE F0 F4)
  (ABOVE F0 F5)
  (ABOVE F0 F6)
  (ABOVE F0 F7)
  (ABOVE F0 F8)
  (ABOVE F0 F9)
  (ABOVE F0 F10)
  (ABOVE F0 F11)
  (ABOVE F0 F12)
  (ABOVE F0 F13)
  (ABOVE F0 F14)
  (ABOVE F0 F15)
  (ABOVE F1 F2)
  (ABOVE F1 F3)
  (ABOVE F1 F4)
  (ABOVE F1 F5)
  (ABOVE F1 F6)
  (ABOVE F1 F7)
  (ABOVE F1 F8)
  (ABOVE F1 F9)
  (ABOVE F1 F10)
  (ABOVE F1 F11)
  (ABOVE F1 F12)
  (ABOVE F1 F13)
  (ABOVE F1 F14)
  (ABOVE F1 F15)
  (ABOVE F2 F3)
  (ABOVE F2 F4)
  (ABOVE F2 F5)
  (ABOVE F2 F6)
  (ABOVE F2 F7)
  (ABOVE F2 F8)
  (ABOVE F2 F9)
  (ABOVE F2 F10)
  (ABOVE F2 F11)
  (ABOVE F2 F12)
  (ABOVE F2 F13)
  (ABOVE F2 F14)
  (ABOVE F2 F15)
  (ABOVE F3 F4)
  (ABOVE F3 F5)
  (ABOVE F3 F6)
  (ABOVE F3 F7)
  (ABOVE F3 F8)
  (ABOVE F3 F9)
  (ABOVE F3 F10)
  (ABOVE F3 F11)
  (ABOVE F3 F12)
  (ABOVE F3 F13)
  (ABOVE F3 F14)
  (ABOVE F3 F15)
  (ABOVE F4 F5)
  (ABOVE F4 F6)
  (ABOVE F4 F7)
  (ABOVE F4 F8)
  (ABOVE F4 F9)
  (ABOVE F4 F10)
  (ABOVE F4 F11)
  (ABOVE F4 F12)
  (ABOVE F4 F13)
  (ABOVE F4 F14)
  (ABOVE F4 F15)
  (ABOVE F5 F6)
  (ABOVE F5 F7)
  (ABOVE F5 F8)
  (ABOVE F5 F9)
  (ABOVE F5 F10)
  (ABOVE F5 F11)
  (ABOVE F5 F12)
  (ABOVE F5 F13)
  (ABOVE F5 F14)
  (ABOVE F5 F15)
  (ABOVE F6 F7)
  (ABOVE F6 F8)
  (ABOVE F6 F9)
  (ABOVE F6 F10)
  (ABOVE F6 F11)
  (ABOVE F6 F12)
  (ABOVE F6 F13)
  (ABOVE F6 F14)
  (ABOVE F6 F15)
  (ABOVE F7 F8)
  (ABOVE F7 F9)
  (ABOVE F7 F10)
  (ABOVE F7 F11)
  (ABOVE F7 F12)
  (ABOVE F7 F13)
  (ABOVE F7 F14)
  (ABOVE F7 F15)
  (ABOVE F8 F9)
  (ABOVE F8 F10)
  (ABOVE F8 F11)
  (ABOVE F8 F12)
  (ABOVE F8 F13)
  (ABOVE F8 F14)
  (ABOVE F8 F15)
  (ABOVE F9 F10)
  (ABOVE F9 F11)
  (ABOVE F9 F12)
  (ABOVE F9 F13)
  (ABOVE F9 F14)
  (ABOVE F9 F15)
  (ABOVE F10 F11)
  (ABOVE F10 F12)
  (ABOVE F10 F13)
  (ABOVE F10 F14)
  (ABOVE F10 F15)
  (ABOVE F11 F12)
  (ABOVE F11 F13)
  (ABOVE F11 F14)
  (ABOVE F11 F15)
  (ABOVE F12 F13)
  (ABOVE F12 F14)
  (ABOVE F12 F15)
  (ABOVE F13 F14)
  (ABOVE F13 F15)
  (ABOVE F14 F15)
  (ORIGIN P0 F11)
  (DESTIN P0 F0)
  (ORIGIN P1 F4)
  (DESTIN P1 F10)
  (ORIGIN P2 F9)
  (DESTIN P2 F14)
  (ORIGIN P3 F6)
  (DESTIN P3 F14)
  (ORIGIN P4 F15)
  (DESTIN P4 F7)
  (ORIGIN P5 F10)
  (DESTIN P5 F11)
  (ORIGIN P6 F15)
  (DESTIN P6 F2)
  (ORIGIN P7 F2)
  (DESTIN P7 F14)
  (LIFT-AT F0))
 (:GOAL
  (AND (SERVED P0)
       (SERVED P1)
       (SERVED P2)
       (SERVED P3)
       (SERVED P4)
       (SERVED P5)
       (SERVED P6)
       (SERVED P7))))