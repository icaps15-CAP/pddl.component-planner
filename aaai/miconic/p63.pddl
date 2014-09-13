(DEFINE (PROBLEM MIXED-F26-P13-U0-V0-G0-A0-N0-A0-B0-N0-F0-R0)
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
           P9 - PASSENGER
           P10 - PASSENGER
           P11 - PASSENGER
           P12 - PASSENGER
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
           F17 - FLOOR
           F18 - FLOOR
           F19 - FLOOR
           F20 - FLOOR
           F21 - FLOOR
           F22 - FLOOR
           F23 - FLOOR
           F24 - FLOOR
           F25 - FLOOR)

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
  (ABOVE F0 F18)
  (ABOVE F0 F19)
  (ABOVE F0 F20)
  (ABOVE F0 F21)
  (ABOVE F0 F22)
  (ABOVE F0 F23)
  (ABOVE F0 F24)
  (ABOVE F0 F25)
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
  (ABOVE F1 F18)
  (ABOVE F1 F19)
  (ABOVE F1 F20)
  (ABOVE F1 F21)
  (ABOVE F1 F22)
  (ABOVE F1 F23)
  (ABOVE F1 F24)
  (ABOVE F1 F25)
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
  (ABOVE F2 F18)
  (ABOVE F2 F19)
  (ABOVE F2 F20)
  (ABOVE F2 F21)
  (ABOVE F2 F22)
  (ABOVE F2 F23)
  (ABOVE F2 F24)
  (ABOVE F2 F25)
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
  (ABOVE F3 F18)
  (ABOVE F3 F19)
  (ABOVE F3 F20)
  (ABOVE F3 F21)
  (ABOVE F3 F22)
  (ABOVE F3 F23)
  (ABOVE F3 F24)
  (ABOVE F3 F25)
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
  (ABOVE F4 F18)
  (ABOVE F4 F19)
  (ABOVE F4 F20)
  (ABOVE F4 F21)
  (ABOVE F4 F22)
  (ABOVE F4 F23)
  (ABOVE F4 F24)
  (ABOVE F4 F25)
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
  (ABOVE F5 F18)
  (ABOVE F5 F19)
  (ABOVE F5 F20)
  (ABOVE F5 F21)
  (ABOVE F5 F22)
  (ABOVE F5 F23)
  (ABOVE F5 F24)
  (ABOVE F5 F25)
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
  (ABOVE F6 F18)
  (ABOVE F6 F19)
  (ABOVE F6 F20)
  (ABOVE F6 F21)
  (ABOVE F6 F22)
  (ABOVE F6 F23)
  (ABOVE F6 F24)
  (ABOVE F6 F25)
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
  (ABOVE F7 F18)
  (ABOVE F7 F19)
  (ABOVE F7 F20)
  (ABOVE F7 F21)
  (ABOVE F7 F22)
  (ABOVE F7 F23)
  (ABOVE F7 F24)
  (ABOVE F7 F25)
  (ABOVE F8 F9)
  (ABOVE F8 F10)
  (ABOVE F8 F11)
  (ABOVE F8 F12)
  (ABOVE F8 F13)
  (ABOVE F8 F14)
  (ABOVE F8 F15)
  (ABOVE F8 F16)
  (ABOVE F8 F17)
  (ABOVE F8 F18)
  (ABOVE F8 F19)
  (ABOVE F8 F20)
  (ABOVE F8 F21)
  (ABOVE F8 F22)
  (ABOVE F8 F23)
  (ABOVE F8 F24)
  (ABOVE F8 F25)
  (ABOVE F9 F10)
  (ABOVE F9 F11)
  (ABOVE F9 F12)
  (ABOVE F9 F13)
  (ABOVE F9 F14)
  (ABOVE F9 F15)
  (ABOVE F9 F16)
  (ABOVE F9 F17)
  (ABOVE F9 F18)
  (ABOVE F9 F19)
  (ABOVE F9 F20)
  (ABOVE F9 F21)
  (ABOVE F9 F22)
  (ABOVE F9 F23)
  (ABOVE F9 F24)
  (ABOVE F9 F25)
  (ABOVE F10 F11)
  (ABOVE F10 F12)
  (ABOVE F10 F13)
  (ABOVE F10 F14)
  (ABOVE F10 F15)
  (ABOVE F10 F16)
  (ABOVE F10 F17)
  (ABOVE F10 F18)
  (ABOVE F10 F19)
  (ABOVE F10 F20)
  (ABOVE F10 F21)
  (ABOVE F10 F22)
  (ABOVE F10 F23)
  (ABOVE F10 F24)
  (ABOVE F10 F25)
  (ABOVE F11 F12)
  (ABOVE F11 F13)
  (ABOVE F11 F14)
  (ABOVE F11 F15)
  (ABOVE F11 F16)
  (ABOVE F11 F17)
  (ABOVE F11 F18)
  (ABOVE F11 F19)
  (ABOVE F11 F20)
  (ABOVE F11 F21)
  (ABOVE F11 F22)
  (ABOVE F11 F23)
  (ABOVE F11 F24)
  (ABOVE F11 F25)
  (ABOVE F12 F13)
  (ABOVE F12 F14)
  (ABOVE F12 F15)
  (ABOVE F12 F16)
  (ABOVE F12 F17)
  (ABOVE F12 F18)
  (ABOVE F12 F19)
  (ABOVE F12 F20)
  (ABOVE F12 F21)
  (ABOVE F12 F22)
  (ABOVE F12 F23)
  (ABOVE F12 F24)
  (ABOVE F12 F25)
  (ABOVE F13 F14)
  (ABOVE F13 F15)
  (ABOVE F13 F16)
  (ABOVE F13 F17)
  (ABOVE F13 F18)
  (ABOVE F13 F19)
  (ABOVE F13 F20)
  (ABOVE F13 F21)
  (ABOVE F13 F22)
  (ABOVE F13 F23)
  (ABOVE F13 F24)
  (ABOVE F13 F25)
  (ABOVE F14 F15)
  (ABOVE F14 F16)
  (ABOVE F14 F17)
  (ABOVE F14 F18)
  (ABOVE F14 F19)
  (ABOVE F14 F20)
  (ABOVE F14 F21)
  (ABOVE F14 F22)
  (ABOVE F14 F23)
  (ABOVE F14 F24)
  (ABOVE F14 F25)
  (ABOVE F15 F16)
  (ABOVE F15 F17)
  (ABOVE F15 F18)
  (ABOVE F15 F19)
  (ABOVE F15 F20)
  (ABOVE F15 F21)
  (ABOVE F15 F22)
  (ABOVE F15 F23)
  (ABOVE F15 F24)
  (ABOVE F15 F25)
  (ABOVE F16 F17)
  (ABOVE F16 F18)
  (ABOVE F16 F19)
  (ABOVE F16 F20)
  (ABOVE F16 F21)
  (ABOVE F16 F22)
  (ABOVE F16 F23)
  (ABOVE F16 F24)
  (ABOVE F16 F25)
  (ABOVE F17 F18)
  (ABOVE F17 F19)
  (ABOVE F17 F20)
  (ABOVE F17 F21)
  (ABOVE F17 F22)
  (ABOVE F17 F23)
  (ABOVE F17 F24)
  (ABOVE F17 F25)
  (ABOVE F18 F19)
  (ABOVE F18 F20)
  (ABOVE F18 F21)
  (ABOVE F18 F22)
  (ABOVE F18 F23)
  (ABOVE F18 F24)
  (ABOVE F18 F25)
  (ABOVE F19 F20)
  (ABOVE F19 F21)
  (ABOVE F19 F22)
  (ABOVE F19 F23)
  (ABOVE F19 F24)
  (ABOVE F19 F25)
  (ABOVE F20 F21)
  (ABOVE F20 F22)
  (ABOVE F20 F23)
  (ABOVE F20 F24)
  (ABOVE F20 F25)
  (ABOVE F21 F22)
  (ABOVE F21 F23)
  (ABOVE F21 F24)
  (ABOVE F21 F25)
  (ABOVE F22 F23)
  (ABOVE F22 F24)
  (ABOVE F22 F25)
  (ABOVE F23 F24)
  (ABOVE F23 F25)
  (ABOVE F24 F25)
  (ORIGIN P0 F13)
  (DESTIN P0 F22)
  (ORIGIN P1 F11)
  (DESTIN P1 F17)
  (ORIGIN P2 F1)
  (DESTIN P2 F12)
  (ORIGIN P3 F16)
  (DESTIN P3 F1)
  (ORIGIN P4 F7)
  (DESTIN P4 F2)
  (ORIGIN P5 F3)
  (DESTIN P5 F0)
  (ORIGIN P6 F17)
  (DESTIN P6 F25)
  (ORIGIN P7 F14)
  (DESTIN P7 F22)
  (ORIGIN P8 F10)
  (DESTIN P8 F24)
  (ORIGIN P9 F7)
  (DESTIN P9 F8)
  (ORIGIN P10 F3)
  (DESTIN P10 F16)
  (ORIGIN P11 F18)
  (DESTIN P11 F2)
  (ORIGIN P12 F3)
  (DESTIN P12 F23)
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
       (SERVED P8)
       (SERVED P9)
       (SERVED P10)
       (SERVED P11)
       (SERVED P12))))