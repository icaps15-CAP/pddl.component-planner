(DEFINE (PROBLEM MIXED-F18-P9-U0-V0-G0-A0-N0-A0-B0-N0-F0-R2)
 (:DOMAIN MICONIC)
 (:OBJECTS P0 - PASSENGER
           P1 - PASSENGER
           P2 - PASSENGER
           P3 - PASSENGER
           P4 - PASSENGER
           P5 - PASSENGER
           P6 - PASSENGER
           P7 - PASSENGER
           P8 - PASSENGER
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
           F15 - FLOOR
           F16 - FLOOR
           F17 - FLOOR)

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
  (ABOVE F0 F16)
  (ABOVE F0 F17)
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
  (ABOVE F1 F16)
  (ABOVE F1 F17)
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
  (ABOVE F2 F16)
  (ABOVE F2 F17)
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
  (ABOVE F3 F16)
  (ABOVE F3 F17)
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
  (ABOVE F4 F16)
  (ABOVE F4 F17)
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
  (ABOVE F5 F16)
  (ABOVE F5 F17)
  (ABOVE F6 F7)
  (ABOVE F6 F8)
  (ABOVE F6 F9)
  (ABOVE F6 F10)
  (ABOVE F6 F11)
  (ABOVE F6 F12)
  (ABOVE F6 F13)
  (ABOVE F6 F14)
  (ABOVE F6 F15)
  (ABOVE F6 F16)
  (ABOVE F6 F17)
  (ABOVE F7 F8)
  (ABOVE F7 F9)
  (ABOVE F7 F10)
  (ABOVE F7 F11)
  (ABOVE F7 F12)
  (ABOVE F7 F13)
  (ABOVE F7 F14)
  (ABOVE F7 F15)
  (ABOVE F7 F16)
  (ABOVE F7 F17)
  (ABOVE F8 F9)
  (ABOVE F8 F10)
  (ABOVE F8 F11)
  (ABOVE F8 F12)
  (ABOVE F8 F13)
  (ABOVE F8 F14)
  (ABOVE F8 F15)
  (ABOVE F8 F16)
  (ABOVE F8 F17)
  (ABOVE F9 F10)
  (ABOVE F9 F11)
  (ABOVE F9 F12)
  (ABOVE F9 F13)
  (ABOVE F9 F14)
  (ABOVE F9 F15)
  (ABOVE F9 F16)
  (ABOVE F9 F17)
  (ABOVE F10 F11)
  (ABOVE F10 F12)
  (ABOVE F10 F13)
  (ABOVE F10 F14)
  (ABOVE F10 F15)
  (ABOVE F10 F16)
  (ABOVE F10 F17)
  (ABOVE F11 F12)
  (ABOVE F11 F13)
  (ABOVE F11 F14)
  (ABOVE F11 F15)
  (ABOVE F11 F16)
  (ABOVE F11 F17)
  (ABOVE F12 F13)
  (ABOVE F12 F14)
  (ABOVE F12 F15)
  (ABOVE F12 F16)
  (ABOVE F12 F17)
  (ABOVE F13 F14)
  (ABOVE F13 F15)
  (ABOVE F13 F16)
  (ABOVE F13 F17)
  (ABOVE F14 F15)
  (ABOVE F14 F16)
  (ABOVE F14 F17)
  (ABOVE F15 F16)
  (ABOVE F15 F17)
  (ABOVE F16 F17)
  (ORIGIN P0 F13)
  (DESTIN P0 F12)
  (ORIGIN P1 F16)
  (DESTIN P1 F0)
  (ORIGIN P2 F3)
  (DESTIN P2 F12)
  (ORIGIN P3 F2)
  (DESTIN P3 F14)
  (ORIGIN P4 F7)
  (DESTIN P4 F3)
  (ORIGIN P5 F8)
  (DESTIN P5 F5)
  (ORIGIN P6 F13)
  (DESTIN P6 F8)
  (ORIGIN P7 F0)
  (DESTIN P7 F12)
  (ORIGIN P8 F1)
  (DESTIN P8 F9)
  (LIFT-AT F0))
 (:GOAL
  (AND (SERVED P0)
       (SERVED P1)
       (SERVED P2)
       (SERVED P3)
       (SERVED P4)
       (SERVED P5)
       (SERVED P6)
       (SERVED P7)
       (SERVED P8))))