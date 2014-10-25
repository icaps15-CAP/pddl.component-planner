(DEFINE (PROBLEM ELEVATORS-9-200)
 (:DOMAIN ELEVATORS-SEQUENCEDSTRIPS)
 (:OBJECTS N-0 N-1 N-2 N-3 N-4 N-5 N-6 N-7 N-8 - COUNT
           P-0 P-1 P-2 P-3 P-4 P-5 P-6 P-7 P-8 P-9 P-10 P-11 P-12 P-13 P-14 P-15 P-16 P-17 P-18 P-19 P-20 P-21 P-22 P-23 P-24 P-25 P-26 P-27 P-28 P-29 P-30 P-31 P-32 P-33 P-34 P-35 P-36 P-37 P-38 P-39 P-40 P-41 P-42 P-43 P-44 P-45 P-46 P-47 P-48 P-49 P-50 P-51 P-52 P-53 P-54 P-55 P-56 P-57 P-58 P-59 P-60 P-61 P-62 P-63 P-64 P-65 P-66 P-67 P-68 P-69 P-70 P-71 P-72 P-73 P-74 P-75 P-76 P-77 P-78 P-79 P-80 P-81 P-82 P-83 P-84 P-85 P-86 P-87 P-88 P-89 P-90 P-91 P-92 P-93 P-94 P-95 P-96 P-97 P-98 P-99 P-100 P-101 P-102 P-103 P-104 P-105 P-106 P-107 P-108 P-109 P-110 P-111 P-112 P-113 P-114 P-115 P-116 P-117 P-118 P-119 P-120 P-121 P-122 P-123 P-124 P-125 P-126 P-127 P-128 P-129 P-130 P-131 P-132 P-133 P-134 P-135 P-136 P-137 P-138 P-139 P-140 P-141 P-142 P-143 P-144 P-145 P-146 P-147 P-148 P-149 P-150 P-151 P-152 P-153 P-154 P-155 P-156 P-157 P-158 P-159 P-160 P-161 P-162 P-163 P-164 P-165 P-166 P-167 P-168 P-169 P-170 P-171 P-172 P-173 P-174 P-175 P-176 P-177 P-178 P-179 P-180 P-181 P-182 P-183 P-184 P-185 P-186 P-187 P-188 P-189 P-190 P-191 P-192 P-193 P-194 P-195 P-196 P-197 P-198 P-199 - PASSENGER
           FAST-0 FAST-1 FAST-2 FAST-3 - FAST-ELEVATOR
           SLOW-0 SLOW-1 SLOW-2 SLOW-3 - SLOW-ELEVATOR)

 (:INIT
  (NEXT N-0 N-1)
  (NEXT N-1 N-2)
  (NEXT N-2 N-3)
  (NEXT N-3 N-4)
  (NEXT N-4 N-5)
  (NEXT N-5 N-6)
  (NEXT N-6 N-7)
  (NEXT N-7 N-8)
  (ABOVE N-0 N-1)
  (ABOVE N-0 N-2)
  (ABOVE N-0 N-3)
  (ABOVE N-0 N-4)
  (ABOVE N-0 N-5)
  (ABOVE N-0 N-6)
  (ABOVE N-0 N-7)
  (ABOVE N-0 N-8)
  (ABOVE N-1 N-2)
  (ABOVE N-1 N-3)
  (ABOVE N-1 N-4)
  (ABOVE N-1 N-5)
  (ABOVE N-1 N-6)
  (ABOVE N-1 N-7)
  (ABOVE N-1 N-8)
  (ABOVE N-2 N-3)
  (ABOVE N-2 N-4)
  (ABOVE N-2 N-5)
  (ABOVE N-2 N-6)
  (ABOVE N-2 N-7)
  (ABOVE N-2 N-8)
  (ABOVE N-3 N-4)
  (ABOVE N-3 N-5)
  (ABOVE N-3 N-6)
  (ABOVE N-3 N-7)
  (ABOVE N-3 N-8)
  (ABOVE N-4 N-5)
  (ABOVE N-4 N-6)
  (ABOVE N-4 N-7)
  (ABOVE N-4 N-8)
  (ABOVE N-5 N-6)
  (ABOVE N-5 N-7)
  (ABOVE N-5 N-8)
  (ABOVE N-6 N-7)
  (ABOVE N-6 N-8)
  (ABOVE N-7 N-8)
  (REACHABLE-FLOOR SLOW-0 N-0)
  (REACHABLE-FLOOR SLOW-0 N-1)
  (REACHABLE-FLOOR SLOW-0 N-2)
  (LIFT-AT SLOW-0 N-0)
  (PASSENGERS SLOW-0 N-0)
  (CAN-HOLD SLOW-0 N-1)
  (CAN-HOLD SLOW-0 N-2)
  (CAN-HOLD SLOW-0 N-3)
  (CAN-HOLD SLOW-0 N-4)
  (REACHABLE-FLOOR SLOW-1 N-2)
  (REACHABLE-FLOOR SLOW-1 N-3)
  (REACHABLE-FLOOR SLOW-1 N-4)
  (LIFT-AT SLOW-1 N-2)
  (PASSENGERS SLOW-1 N-0)
  (CAN-HOLD SLOW-1 N-1)
  (CAN-HOLD SLOW-1 N-2)
  (CAN-HOLD SLOW-1 N-3)
  (CAN-HOLD SLOW-1 N-4)
  (REACHABLE-FLOOR SLOW-2 N-4)
  (REACHABLE-FLOOR SLOW-2 N-5)
  (REACHABLE-FLOOR SLOW-2 N-6)
  (LIFT-AT SLOW-2 N-4)
  (PASSENGERS SLOW-2 N-0)
  (CAN-HOLD SLOW-2 N-1)
  (CAN-HOLD SLOW-2 N-2)
  (CAN-HOLD SLOW-2 N-3)
  (CAN-HOLD SLOW-2 N-4)
  (REACHABLE-FLOOR SLOW-3 N-6)
  (REACHABLE-FLOOR SLOW-3 N-7)
  (REACHABLE-FLOOR SLOW-3 N-8)
  (LIFT-AT SLOW-3 N-7)
  (PASSENGERS SLOW-3 N-0)
  (CAN-HOLD SLOW-3 N-1)
  (CAN-HOLD SLOW-3 N-2)
  (CAN-HOLD SLOW-3 N-3)
  (CAN-HOLD SLOW-3 N-4)
  (REACHABLE-FLOOR FAST-0 N-0)
  (REACHABLE-FLOOR FAST-0 N-2)
  (REACHABLE-FLOOR FAST-0 N-4)
  (REACHABLE-FLOOR FAST-0 N-6)
  (REACHABLE-FLOOR FAST-0 N-8)
  (LIFT-AT FAST-0 N-4)
  (PASSENGERS FAST-0 N-0)
  (CAN-HOLD FAST-0 N-1)
  (CAN-HOLD FAST-0 N-2)
  (CAN-HOLD FAST-0 N-3)
  (CAN-HOLD FAST-0 N-4)
  (CAN-HOLD FAST-0 N-5)
  (CAN-HOLD FAST-0 N-6)
  (REACHABLE-FLOOR FAST-1 N-0)
  (REACHABLE-FLOOR FAST-1 N-2)
  (REACHABLE-FLOOR FAST-1 N-4)
  (REACHABLE-FLOOR FAST-1 N-6)
  (REACHABLE-FLOOR FAST-1 N-8)
  (LIFT-AT FAST-1 N-6)
  (PASSENGERS FAST-1 N-0)
  (CAN-HOLD FAST-1 N-1)
  (CAN-HOLD FAST-1 N-2)
  (CAN-HOLD FAST-1 N-3)
  (CAN-HOLD FAST-1 N-4)
  (CAN-HOLD FAST-1 N-5)
  (CAN-HOLD FAST-1 N-6)
  (REACHABLE-FLOOR FAST-2 N-0)
  (REACHABLE-FLOOR FAST-2 N-2)
  (REACHABLE-FLOOR FAST-2 N-4)
  (REACHABLE-FLOOR FAST-2 N-6)
  (REACHABLE-FLOOR FAST-2 N-8)
  (LIFT-AT FAST-2 N-6)
  (PASSENGERS FAST-2 N-0)
  (CAN-HOLD FAST-2 N-1)
  (CAN-HOLD FAST-2 N-2)
  (CAN-HOLD FAST-2 N-3)
  (CAN-HOLD FAST-2 N-4)
  (CAN-HOLD FAST-2 N-5)
  (CAN-HOLD FAST-2 N-6)
  (REACHABLE-FLOOR FAST-3 N-0)
  (REACHABLE-FLOOR FAST-3 N-2)
  (REACHABLE-FLOOR FAST-3 N-4)
  (REACHABLE-FLOOR FAST-3 N-6)
  (REACHABLE-FLOOR FAST-3 N-8)
  (LIFT-AT FAST-3 N-4)
  (PASSENGERS FAST-3 N-0)
  (CAN-HOLD FAST-3 N-1)
  (CAN-HOLD FAST-3 N-2)
  (CAN-HOLD FAST-3 N-3)
  (CAN-HOLD FAST-3 N-4)
  (CAN-HOLD FAST-3 N-5)
  (CAN-HOLD FAST-3 N-6)
  (PASSENGER-AT P-0 N-7)
  (PASSENGER-AT P-1 N-7)
  (PASSENGER-AT P-2 N-3)
  (PASSENGER-AT P-3 N-2)
  (PASSENGER-AT P-4 N-1)
  (PASSENGER-AT P-5 N-2)
  (PASSENGER-AT P-6 N-6)
  (PASSENGER-AT P-7 N-2)
  (PASSENGER-AT P-8 N-2)
  (PASSENGER-AT P-9 N-2)
  (PASSENGER-AT P-10 N-7)
  (PASSENGER-AT P-11 N-4)
  (PASSENGER-AT P-12 N-7)
  (PASSENGER-AT P-13 N-3)
  (PASSENGER-AT P-14 N-6)
  (PASSENGER-AT P-15 N-0)
  (PASSENGER-AT P-16 N-3)
  (PASSENGER-AT P-17 N-1)
  (PASSENGER-AT P-18 N-5)
  (PASSENGER-AT P-19 N-6)
  (PASSENGER-AT P-20 N-5)
  (PASSENGER-AT P-21 N-5)
  (PASSENGER-AT P-22 N-4)
  (PASSENGER-AT P-23 N-5)
  (PASSENGER-AT P-24 N-6)
  (PASSENGER-AT P-25 N-2)
  (PASSENGER-AT P-26 N-5)
  (PASSENGER-AT P-27 N-8)
  (PASSENGER-AT P-28 N-5)
  (PASSENGER-AT P-29 N-2)
  (PASSENGER-AT P-30 N-0)
  (PASSENGER-AT P-31 N-6)
  (PASSENGER-AT P-32 N-3)
  (PASSENGER-AT P-33 N-7)
  (PASSENGER-AT P-34 N-2)
  (PASSENGER-AT P-35 N-1)
  (PASSENGER-AT P-36 N-1)
  (PASSENGER-AT P-37 N-2)
  (PASSENGER-AT P-38 N-0)
  (PASSENGER-AT P-39 N-7)
  (PASSENGER-AT P-40 N-7)
  (PASSENGER-AT P-41 N-4)
  (PASSENGER-AT P-42 N-6)
  (PASSENGER-AT P-43 N-3)
  (PASSENGER-AT P-44 N-6)
  (PASSENGER-AT P-45 N-6)
  (PASSENGER-AT P-46 N-3)
  (PASSENGER-AT P-47 N-4)
  (PASSENGER-AT P-48 N-6)
  (PASSENGER-AT P-49 N-4)
  (PASSENGER-AT P-50 N-1)
  (PASSENGER-AT P-51 N-4)
  (PASSENGER-AT P-52 N-8)
  (PASSENGER-AT P-53 N-5)
  (PASSENGER-AT P-54 N-0)
  (PASSENGER-AT P-55 N-7)
  (PASSENGER-AT P-56 N-5)
  (PASSENGER-AT P-57 N-4)
  (PASSENGER-AT P-58 N-8)
  (PASSENGER-AT P-59 N-8)
  (PASSENGER-AT P-60 N-8)
  (PASSENGER-AT P-61 N-7)
  (PASSENGER-AT P-62 N-8)
  (PASSENGER-AT P-63 N-4)
  (PASSENGER-AT P-64 N-1)
  (PASSENGER-AT P-65 N-0)
  (PASSENGER-AT P-66 N-4)
  (PASSENGER-AT P-67 N-4)
  (PASSENGER-AT P-68 N-4)
  (PASSENGER-AT P-69 N-0)
  (PASSENGER-AT P-70 N-6)
  (PASSENGER-AT P-71 N-4)
  (PASSENGER-AT P-72 N-3)
  (PASSENGER-AT P-73 N-6)
  (PASSENGER-AT P-74 N-8)
  (PASSENGER-AT P-75 N-0)
  (PASSENGER-AT P-76 N-5)
  (PASSENGER-AT P-77 N-6)
  (PASSENGER-AT P-78 N-5)
  (PASSENGER-AT P-79 N-0)
  (PASSENGER-AT P-80 N-3)
  (PASSENGER-AT P-81 N-0)
  (PASSENGER-AT P-82 N-4)
  (PASSENGER-AT P-83 N-6)
  (PASSENGER-AT P-84 N-1)
  (PASSENGER-AT P-85 N-2)
  (PASSENGER-AT P-86 N-2)
  (PASSENGER-AT P-87 N-7)
  (PASSENGER-AT P-88 N-7)
  (PASSENGER-AT P-89 N-3)
  (PASSENGER-AT P-90 N-7)
  (PASSENGER-AT P-91 N-2)
  (PASSENGER-AT P-92 N-2)
  (PASSENGER-AT P-93 N-7)
  (PASSENGER-AT P-94 N-8)
  (PASSENGER-AT P-95 N-7)
  (PASSENGER-AT P-96 N-8)
  (PASSENGER-AT P-97 N-1)
  (PASSENGER-AT P-98 N-0)
  (PASSENGER-AT P-99 N-8)
  (PASSENGER-AT P-100 N-7)
  (PASSENGER-AT P-101 N-5)
  (PASSENGER-AT P-102 N-5)
  (PASSENGER-AT P-103 N-1)
  (PASSENGER-AT P-104 N-3)
  (PASSENGER-AT P-105 N-2)
  (PASSENGER-AT P-106 N-8)
  (PASSENGER-AT P-107 N-8)
  (PASSENGER-AT P-108 N-1)
  (PASSENGER-AT P-109 N-4)
  (PASSENGER-AT P-110 N-8)
  (PASSENGER-AT P-111 N-8)
  (PASSENGER-AT P-112 N-0)
  (PASSENGER-AT P-113 N-2)
  (PASSENGER-AT P-114 N-1)
  (PASSENGER-AT P-115 N-8)
  (PASSENGER-AT P-116 N-8)
  (PASSENGER-AT P-117 N-2)
  (PASSENGER-AT P-118 N-6)
  (PASSENGER-AT P-119 N-0)
  (PASSENGER-AT P-120 N-6)
  (PASSENGER-AT P-121 N-7)
  (PASSENGER-AT P-122 N-8)
  (PASSENGER-AT P-123 N-6)
  (PASSENGER-AT P-124 N-6)
  (PASSENGER-AT P-125 N-8)
  (PASSENGER-AT P-126 N-5)
  (PASSENGER-AT P-127 N-1)
  (PASSENGER-AT P-128 N-0)
  (PASSENGER-AT P-129 N-8)
  (PASSENGER-AT P-130 N-5)
  (PASSENGER-AT P-131 N-6)
  (PASSENGER-AT P-132 N-0)
  (PASSENGER-AT P-133 N-1)
  (PASSENGER-AT P-134 N-4)
  (PASSENGER-AT P-135 N-6)
  (PASSENGER-AT P-136 N-6)
  (PASSENGER-AT P-137 N-4)
  (PASSENGER-AT P-138 N-1)
  (PASSENGER-AT P-139 N-7)
  (PASSENGER-AT P-140 N-5)
  (PASSENGER-AT P-141 N-0)
  (PASSENGER-AT P-142 N-4)
  (PASSENGER-AT P-143 N-5)
  (PASSENGER-AT P-144 N-0)
  (PASSENGER-AT P-145 N-3)
  (PASSENGER-AT P-146 N-2)
  (PASSENGER-AT P-147 N-3)
  (PASSENGER-AT P-148 N-2)
  (PASSENGER-AT P-149 N-2)
  (PASSENGER-AT P-150 N-2)
  (PASSENGER-AT P-151 N-0)
  (PASSENGER-AT P-152 N-0)
  (PASSENGER-AT P-153 N-2)
  (PASSENGER-AT P-154 N-5)
  (PASSENGER-AT P-155 N-8)
  (PASSENGER-AT P-156 N-1)
  (PASSENGER-AT P-157 N-4)
  (PASSENGER-AT P-158 N-7)
  (PASSENGER-AT P-159 N-4)
  (PASSENGER-AT P-160 N-7)
  (PASSENGER-AT P-161 N-1)
  (PASSENGER-AT P-162 N-6)
  (PASSENGER-AT P-163 N-5)
  (PASSENGER-AT P-164 N-2)
  (PASSENGER-AT P-165 N-5)
  (PASSENGER-AT P-166 N-0)
  (PASSENGER-AT P-167 N-2)
  (PASSENGER-AT P-168 N-5)
  (PASSENGER-AT P-169 N-7)
  (PASSENGER-AT P-170 N-4)
  (PASSENGER-AT P-171 N-3)
  (PASSENGER-AT P-172 N-5)
  (PASSENGER-AT P-173 N-2)
  (PASSENGER-AT P-174 N-7)
  (PASSENGER-AT P-175 N-5)
  (PASSENGER-AT P-176 N-4)
  (PASSENGER-AT P-177 N-8)
  (PASSENGER-AT P-178 N-4)
  (PASSENGER-AT P-179 N-7)
  (PASSENGER-AT P-180 N-0)
  (PASSENGER-AT P-181 N-4)
  (PASSENGER-AT P-182 N-7)
  (PASSENGER-AT P-183 N-5)
  (PASSENGER-AT P-184 N-2)
  (PASSENGER-AT P-185 N-1)
  (PASSENGER-AT P-186 N-4)
  (PASSENGER-AT P-187 N-7)
  (PASSENGER-AT P-188 N-4)
  (PASSENGER-AT P-189 N-4)
  (PASSENGER-AT P-190 N-5)
  (PASSENGER-AT P-191 N-0)
  (PASSENGER-AT P-192 N-2)
  (PASSENGER-AT P-193 N-4)
  (PASSENGER-AT P-194 N-6)
  (PASSENGER-AT P-195 N-1)
  (PASSENGER-AT P-196 N-3)
  (PASSENGER-AT P-197 N-5)
  (PASSENGER-AT P-198 N-4)
  (PASSENGER-AT P-199 N-1))
 (:GOAL
  (AND (PASSENGER-AT P-0 N-5) (PASSENGER-AT P-1 N-0) (PASSENGER-AT P-2 N-8)
   (PASSENGER-AT P-3 N-4) (PASSENGER-AT P-4 N-3) (PASSENGER-AT P-5 N-4)
   (PASSENGER-AT P-6 N-1) (PASSENGER-AT P-7 N-2) (PASSENGER-AT P-8 N-0)
   (PASSENGER-AT P-9 N-1) (PASSENGER-AT P-10 N-4) (PASSENGER-AT P-11 N-0)
   (PASSENGER-AT P-12 N-2) (PASSENGER-AT P-13 N-6) (PASSENGER-AT P-14 N-0)
   (PASSENGER-AT P-15 N-8) (PASSENGER-AT P-16 N-0) (PASSENGER-AT P-17 N-8)
   (PASSENGER-AT P-18 N-7) (PASSENGER-AT P-19 N-6) (PASSENGER-AT P-20 N-4)
   (PASSENGER-AT P-21 N-0) (PASSENGER-AT P-22 N-1) (PASSENGER-AT P-23 N-8)
   (PASSENGER-AT P-24 N-7) (PASSENGER-AT P-25 N-7) (PASSENGER-AT P-26 N-1)
   (PASSENGER-AT P-27 N-4) (PASSENGER-AT P-28 N-2) (PASSENGER-AT P-29 N-8)
   (PASSENGER-AT P-30 N-7) (PASSENGER-AT P-31 N-3) (PASSENGER-AT P-32 N-4)
   (PASSENGER-AT P-33 N-2) (PASSENGER-AT P-34 N-4) (PASSENGER-AT P-35 N-0)
   (PASSENGER-AT P-36 N-0) (PASSENGER-AT P-37 N-6) (PASSENGER-AT P-38 N-4)
   (PASSENGER-AT P-39 N-6) (PASSENGER-AT P-40 N-4) (PASSENGER-AT P-41 N-7)
   (PASSENGER-AT P-42 N-8) (PASSENGER-AT P-43 N-4) (PASSENGER-AT P-44 N-5)
   (PASSENGER-AT P-45 N-0) (PASSENGER-AT P-46 N-4) (PASSENGER-AT P-47 N-8)
   (PASSENGER-AT P-48 N-8) (PASSENGER-AT P-49 N-1) (PASSENGER-AT P-50 N-2)
   (PASSENGER-AT P-51 N-7) (PASSENGER-AT P-52 N-0) (PASSENGER-AT P-53 N-2)
   (PASSENGER-AT P-54 N-1) (PASSENGER-AT P-55 N-5) (PASSENGER-AT P-56 N-7)
   (PASSENGER-AT P-57 N-1) (PASSENGER-AT P-58 N-5) (PASSENGER-AT P-59 N-6)
   (PASSENGER-AT P-60 N-8) (PASSENGER-AT P-61 N-3) (PASSENGER-AT P-62 N-4)
   (PASSENGER-AT P-63 N-4) (PASSENGER-AT P-64 N-7) (PASSENGER-AT P-65 N-7)
   (PASSENGER-AT P-66 N-8) (PASSENGER-AT P-67 N-1) (PASSENGER-AT P-68 N-2)
   (PASSENGER-AT P-69 N-8) (PASSENGER-AT P-70 N-1) (PASSENGER-AT P-71 N-3)
   (PASSENGER-AT P-72 N-5) (PASSENGER-AT P-73 N-3) (PASSENGER-AT P-74 N-2)
   (PASSENGER-AT P-75 N-1) (PASSENGER-AT P-76 N-5) (PASSENGER-AT P-77 N-8)
   (PASSENGER-AT P-78 N-4) (PASSENGER-AT P-79 N-6) (PASSENGER-AT P-80 N-3)
   (PASSENGER-AT P-81 N-0) (PASSENGER-AT P-82 N-4) (PASSENGER-AT P-83 N-7)
   (PASSENGER-AT P-84 N-5) (PASSENGER-AT P-85 N-2) (PASSENGER-AT P-86 N-3)
   (PASSENGER-AT P-87 N-6) (PASSENGER-AT P-88 N-0) (PASSENGER-AT P-89 N-6)
   (PASSENGER-AT P-90 N-8) (PASSENGER-AT P-91 N-1) (PASSENGER-AT P-92 N-6)
   (PASSENGER-AT P-93 N-5) (PASSENGER-AT P-94 N-0) (PASSENGER-AT P-95 N-5)
   (PASSENGER-AT P-96 N-5) (PASSENGER-AT P-97 N-2) (PASSENGER-AT P-98 N-8)
   (PASSENGER-AT P-99 N-2) (PASSENGER-AT P-100 N-3) (PASSENGER-AT P-101 N-4)
   (PASSENGER-AT P-102 N-8) (PASSENGER-AT P-103 N-8) (PASSENGER-AT P-104 N-5)
   (PASSENGER-AT P-105 N-8) (PASSENGER-AT P-106 N-6) (PASSENGER-AT P-107 N-2)
   (PASSENGER-AT P-108 N-7) (PASSENGER-AT P-109 N-3) (PASSENGER-AT P-110 N-6)
   (PASSENGER-AT P-111 N-8) (PASSENGER-AT P-112 N-5) (PASSENGER-AT P-113 N-7)
   (PASSENGER-AT P-114 N-0) (PASSENGER-AT P-115 N-8) (PASSENGER-AT P-116 N-1)
   (PASSENGER-AT P-117 N-4) (PASSENGER-AT P-118 N-1) (PASSENGER-AT P-119 N-2)
   (PASSENGER-AT P-120 N-1) (PASSENGER-AT P-121 N-6) (PASSENGER-AT P-122 N-0)
   (PASSENGER-AT P-123 N-8) (PASSENGER-AT P-124 N-3) (PASSENGER-AT P-125 N-2)
   (PASSENGER-AT P-126 N-4) (PASSENGER-AT P-127 N-6) (PASSENGER-AT P-128 N-6)
   (PASSENGER-AT P-129 N-8) (PASSENGER-AT P-130 N-3) (PASSENGER-AT P-131 N-8)
   (PASSENGER-AT P-132 N-1) (PASSENGER-AT P-133 N-1) (PASSENGER-AT P-134 N-8)
   (PASSENGER-AT P-135 N-8) (PASSENGER-AT P-136 N-7) (PASSENGER-AT P-137 N-3)
   (PASSENGER-AT P-138 N-2) (PASSENGER-AT P-139 N-4) (PASSENGER-AT P-140 N-7)
   (PASSENGER-AT P-141 N-7) (PASSENGER-AT P-142 N-8) (PASSENGER-AT P-143 N-1)
   (PASSENGER-AT P-144 N-3) (PASSENGER-AT P-145 N-3) (PASSENGER-AT P-146 N-5)
   (PASSENGER-AT P-147 N-0) (PASSENGER-AT P-148 N-0) (PASSENGER-AT P-149 N-1)
   (PASSENGER-AT P-150 N-7) (PASSENGER-AT P-151 N-8) (PASSENGER-AT P-152 N-1)
   (PASSENGER-AT P-153 N-0) (PASSENGER-AT P-154 N-4) (PASSENGER-AT P-155 N-5)
   (PASSENGER-AT P-156 N-2) (PASSENGER-AT P-157 N-0) (PASSENGER-AT P-158 N-3)
   (PASSENGER-AT P-159 N-6) (PASSENGER-AT P-160 N-6) (PASSENGER-AT P-161 N-3)
   (PASSENGER-AT P-162 N-0) (PASSENGER-AT P-163 N-1) (PASSENGER-AT P-164 N-8)
   (PASSENGER-AT P-165 N-5) (PASSENGER-AT P-166 N-2) (PASSENGER-AT P-167 N-2)
   (PASSENGER-AT P-168 N-8) (PASSENGER-AT P-169 N-1) (PASSENGER-AT P-170 N-3)
   (PASSENGER-AT P-171 N-3) (PASSENGER-AT P-172 N-7) (PASSENGER-AT P-173 N-5)
   (PASSENGER-AT P-174 N-2) (PASSENGER-AT P-175 N-3) (PASSENGER-AT P-176 N-0)
   (PASSENGER-AT P-177 N-6) (PASSENGER-AT P-178 N-3) (PASSENGER-AT P-179 N-2)
   (PASSENGER-AT P-180 N-7) (PASSENGER-AT P-181 N-2) (PASSENGER-AT P-182 N-7)
   (PASSENGER-AT P-183 N-0) (PASSENGER-AT P-184 N-5) (PASSENGER-AT P-185 N-7)
   (PASSENGER-AT P-186 N-1) (PASSENGER-AT P-187 N-8) (PASSENGER-AT P-188 N-2)
   (PASSENGER-AT P-189 N-4) (PASSENGER-AT P-190 N-7) (PASSENGER-AT P-191 N-2)
   (PASSENGER-AT P-192 N-7) (PASSENGER-AT P-193 N-7) (PASSENGER-AT P-194 N-4)
   (PASSENGER-AT P-195 N-8) (PASSENGER-AT P-196 N-0) (PASSENGER-AT P-197 N-2)
   (PASSENGER-AT P-198 N-6) (PASSENGER-AT P-199 N-8))))