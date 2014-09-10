(DEFINE (PROBLEM ELEVATORS-9-300)
 (:DOMAIN ELEVATORS-SEQUENCEDSTRIPS)
 (:OBJECTS N-0 N-1 N-2 N-3 N-4 N-5 N-6 N-7 N-8 - COUNT
           P-0 P-1 P-2 P-3 P-4 P-5 P-6 P-7 P-8 P-9 P-10 P-11 P-12 P-13 P-14 P-15 P-16 P-17 P-18 P-19 P-20 P-21 P-22 P-23 P-24 P-25 P-26 P-27 P-28 P-29 P-30 P-31 P-32 P-33 P-34 P-35 P-36 P-37 P-38 P-39 P-40 P-41 P-42 P-43 P-44 P-45 P-46 P-47 P-48 P-49 P-50 P-51 P-52 P-53 P-54 P-55 P-56 P-57 P-58 P-59 P-60 P-61 P-62 P-63 P-64 P-65 P-66 P-67 P-68 P-69 P-70 P-71 P-72 P-73 P-74 P-75 P-76 P-77 P-78 P-79 P-80 P-81 P-82 P-83 P-84 P-85 P-86 P-87 P-88 P-89 P-90 P-91 P-92 P-93 P-94 P-95 P-96 P-97 P-98 P-99 P-100 P-101 P-102 P-103 P-104 P-105 P-106 P-107 P-108 P-109 P-110 P-111 P-112 P-113 P-114 P-115 P-116 P-117 P-118 P-119 P-120 P-121 P-122 P-123 P-124 P-125 P-126 P-127 P-128 P-129 P-130 P-131 P-132 P-133 P-134 P-135 P-136 P-137 P-138 P-139 P-140 P-141 P-142 P-143 P-144 P-145 P-146 P-147 P-148 P-149 P-150 P-151 P-152 P-153 P-154 P-155 P-156 P-157 P-158 P-159 P-160 P-161 P-162 P-163 P-164 P-165 P-166 P-167 P-168 P-169 P-170 P-171 P-172 P-173 P-174 P-175 P-176 P-177 P-178 P-179 P-180 P-181 P-182 P-183 P-184 P-185 P-186 P-187 P-188 P-189 P-190 P-191 P-192 P-193 P-194 P-195 P-196 P-197 P-198 P-199 P-200 P-201 P-202 P-203 P-204 P-205 P-206 P-207 P-208 P-209 P-210 P-211 P-212 P-213 P-214 P-215 P-216 P-217 P-218 P-219 P-220 P-221 P-222 P-223 P-224 P-225 P-226 P-227 P-228 P-229 P-230 P-231 P-232 P-233 P-234 P-235 P-236 P-237 P-238 P-239 P-240 P-241 P-242 P-243 P-244 P-245 P-246 P-247 P-248 P-249 P-250 P-251 P-252 P-253 P-254 P-255 P-256 P-257 P-258 P-259 P-260 P-261 P-262 P-263 P-264 P-265 P-266 P-267 P-268 P-269 P-270 P-271 P-272 P-273 P-274 P-275 P-276 P-277 P-278 P-279 P-280 P-281 P-282 P-283 P-284 P-285 P-286 P-287 P-288 P-289 P-290 P-291 P-292 P-293 P-294 P-295 P-296 P-297 P-298 P-299 - PASSENGER
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
  (LIFT-AT SLOW-1 N-4)
  (PASSENGERS SLOW-1 N-0)
  (CAN-HOLD SLOW-1 N-1)
  (CAN-HOLD SLOW-1 N-2)
  (CAN-HOLD SLOW-1 N-3)
  (CAN-HOLD SLOW-1 N-4)
  (REACHABLE-FLOOR SLOW-2 N-4)
  (REACHABLE-FLOOR SLOW-2 N-5)
  (REACHABLE-FLOOR SLOW-2 N-6)
  (LIFT-AT SLOW-2 N-5)
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
  (LIFT-AT FAST-0 N-2)
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
  (LIFT-AT FAST-1 N-4)
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
  (LIFT-AT FAST-2 N-2)
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
  (LIFT-AT FAST-3 N-0)
  (PASSENGERS FAST-3 N-0)
  (CAN-HOLD FAST-3 N-1)
  (CAN-HOLD FAST-3 N-2)
  (CAN-HOLD FAST-3 N-3)
  (CAN-HOLD FAST-3 N-4)
  (CAN-HOLD FAST-3 N-5)
  (CAN-HOLD FAST-3 N-6)
  (PASSENGER-AT P-0 N-6)
  (PASSENGER-AT P-1 N-6)
  (PASSENGER-AT P-2 N-0)
  (PASSENGER-AT P-3 N-1)
  (PASSENGER-AT P-4 N-5)
  (PASSENGER-AT P-5 N-6)
  (PASSENGER-AT P-6 N-4)
  (PASSENGER-AT P-7 N-1)
  (PASSENGER-AT P-8 N-0)
  (PASSENGER-AT P-9 N-1)
  (PASSENGER-AT P-10 N-1)
  (PASSENGER-AT P-11 N-4)
  (PASSENGER-AT P-12 N-0)
  (PASSENGER-AT P-13 N-0)
  (PASSENGER-AT P-14 N-3)
  (PASSENGER-AT P-15 N-7)
  (PASSENGER-AT P-16 N-0)
  (PASSENGER-AT P-17 N-0)
  (PASSENGER-AT P-18 N-4)
  (PASSENGER-AT P-19 N-1)
  (PASSENGER-AT P-20 N-7)
  (PASSENGER-AT P-21 N-7)
  (PASSENGER-AT P-22 N-0)
  (PASSENGER-AT P-23 N-6)
  (PASSENGER-AT P-24 N-7)
  (PASSENGER-AT P-25 N-6)
  (PASSENGER-AT P-26 N-6)
  (PASSENGER-AT P-27 N-1)
  (PASSENGER-AT P-28 N-0)
  (PASSENGER-AT P-29 N-3)
  (PASSENGER-AT P-30 N-7)
  (PASSENGER-AT P-31 N-2)
  (PASSENGER-AT P-32 N-4)
  (PASSENGER-AT P-33 N-2)
  (PASSENGER-AT P-34 N-5)
  (PASSENGER-AT P-35 N-6)
  (PASSENGER-AT P-36 N-6)
  (PASSENGER-AT P-37 N-2)
  (PASSENGER-AT P-38 N-3)
  (PASSENGER-AT P-39 N-3)
  (PASSENGER-AT P-40 N-5)
  (PASSENGER-AT P-41 N-2)
  (PASSENGER-AT P-42 N-7)
  (PASSENGER-AT P-43 N-0)
  (PASSENGER-AT P-44 N-3)
  (PASSENGER-AT P-45 N-4)
  (PASSENGER-AT P-46 N-7)
  (PASSENGER-AT P-47 N-3)
  (PASSENGER-AT P-48 N-4)
  (PASSENGER-AT P-49 N-4)
  (PASSENGER-AT P-50 N-1)
  (PASSENGER-AT P-51 N-8)
  (PASSENGER-AT P-52 N-3)
  (PASSENGER-AT P-53 N-5)
  (PASSENGER-AT P-54 N-3)
  (PASSENGER-AT P-55 N-5)
  (PASSENGER-AT P-56 N-4)
  (PASSENGER-AT P-57 N-2)
  (PASSENGER-AT P-58 N-7)
  (PASSENGER-AT P-59 N-2)
  (PASSENGER-AT P-60 N-0)
  (PASSENGER-AT P-61 N-5)
  (PASSENGER-AT P-62 N-0)
  (PASSENGER-AT P-63 N-1)
  (PASSENGER-AT P-64 N-6)
  (PASSENGER-AT P-65 N-5)
  (PASSENGER-AT P-66 N-1)
  (PASSENGER-AT P-67 N-0)
  (PASSENGER-AT P-68 N-6)
  (PASSENGER-AT P-69 N-3)
  (PASSENGER-AT P-70 N-2)
  (PASSENGER-AT P-71 N-0)
  (PASSENGER-AT P-72 N-6)
  (PASSENGER-AT P-73 N-1)
  (PASSENGER-AT P-74 N-2)
  (PASSENGER-AT P-75 N-0)
  (PASSENGER-AT P-76 N-1)
  (PASSENGER-AT P-77 N-2)
  (PASSENGER-AT P-78 N-5)
  (PASSENGER-AT P-79 N-0)
  (PASSENGER-AT P-80 N-2)
  (PASSENGER-AT P-81 N-1)
  (PASSENGER-AT P-82 N-3)
  (PASSENGER-AT P-83 N-7)
  (PASSENGER-AT P-84 N-1)
  (PASSENGER-AT P-85 N-7)
  (PASSENGER-AT P-86 N-3)
  (PASSENGER-AT P-87 N-6)
  (PASSENGER-AT P-88 N-3)
  (PASSENGER-AT P-89 N-8)
  (PASSENGER-AT P-90 N-2)
  (PASSENGER-AT P-91 N-7)
  (PASSENGER-AT P-92 N-0)
  (PASSENGER-AT P-93 N-2)
  (PASSENGER-AT P-94 N-6)
  (PASSENGER-AT P-95 N-1)
  (PASSENGER-AT P-96 N-2)
  (PASSENGER-AT P-97 N-6)
  (PASSENGER-AT P-98 N-3)
  (PASSENGER-AT P-99 N-4)
  (PASSENGER-AT P-100 N-7)
  (PASSENGER-AT P-101 N-4)
  (PASSENGER-AT P-102 N-3)
  (PASSENGER-AT P-103 N-8)
  (PASSENGER-AT P-104 N-8)
  (PASSENGER-AT P-105 N-6)
  (PASSENGER-AT P-106 N-3)
  (PASSENGER-AT P-107 N-5)
  (PASSENGER-AT P-108 N-4)
  (PASSENGER-AT P-109 N-0)
  (PASSENGER-AT P-110 N-8)
  (PASSENGER-AT P-111 N-7)
  (PASSENGER-AT P-112 N-6)
  (PASSENGER-AT P-113 N-3)
  (PASSENGER-AT P-114 N-7)
  (PASSENGER-AT P-115 N-7)
  (PASSENGER-AT P-116 N-4)
  (PASSENGER-AT P-117 N-5)
  (PASSENGER-AT P-118 N-1)
  (PASSENGER-AT P-119 N-0)
  (PASSENGER-AT P-120 N-3)
  (PASSENGER-AT P-121 N-2)
  (PASSENGER-AT P-122 N-5)
  (PASSENGER-AT P-123 N-5)
  (PASSENGER-AT P-124 N-5)
  (PASSENGER-AT P-125 N-7)
  (PASSENGER-AT P-126 N-3)
  (PASSENGER-AT P-127 N-1)
  (PASSENGER-AT P-128 N-5)
  (PASSENGER-AT P-129 N-8)
  (PASSENGER-AT P-130 N-4)
  (PASSENGER-AT P-131 N-5)
  (PASSENGER-AT P-132 N-0)
  (PASSENGER-AT P-133 N-7)
  (PASSENGER-AT P-134 N-0)
  (PASSENGER-AT P-135 N-7)
  (PASSENGER-AT P-136 N-3)
  (PASSENGER-AT P-137 N-8)
  (PASSENGER-AT P-138 N-2)
  (PASSENGER-AT P-139 N-8)
  (PASSENGER-AT P-140 N-6)
  (PASSENGER-AT P-141 N-7)
  (PASSENGER-AT P-142 N-6)
  (PASSENGER-AT P-143 N-1)
  (PASSENGER-AT P-144 N-2)
  (PASSENGER-AT P-145 N-6)
  (PASSENGER-AT P-146 N-2)
  (PASSENGER-AT P-147 N-4)
  (PASSENGER-AT P-148 N-0)
  (PASSENGER-AT P-149 N-5)
  (PASSENGER-AT P-150 N-5)
  (PASSENGER-AT P-151 N-2)
  (PASSENGER-AT P-152 N-8)
  (PASSENGER-AT P-153 N-7)
  (PASSENGER-AT P-154 N-5)
  (PASSENGER-AT P-155 N-1)
  (PASSENGER-AT P-156 N-3)
  (PASSENGER-AT P-157 N-3)
  (PASSENGER-AT P-158 N-0)
  (PASSENGER-AT P-159 N-5)
  (PASSENGER-AT P-160 N-3)
  (PASSENGER-AT P-161 N-6)
  (PASSENGER-AT P-162 N-8)
  (PASSENGER-AT P-163 N-2)
  (PASSENGER-AT P-164 N-6)
  (PASSENGER-AT P-165 N-8)
  (PASSENGER-AT P-166 N-7)
  (PASSENGER-AT P-167 N-5)
  (PASSENGER-AT P-168 N-5)
  (PASSENGER-AT P-169 N-4)
  (PASSENGER-AT P-170 N-4)
  (PASSENGER-AT P-171 N-4)
  (PASSENGER-AT P-172 N-8)
  (PASSENGER-AT P-173 N-3)
  (PASSENGER-AT P-174 N-8)
  (PASSENGER-AT P-175 N-8)
  (PASSENGER-AT P-176 N-5)
  (PASSENGER-AT P-177 N-7)
  (PASSENGER-AT P-178 N-7)
  (PASSENGER-AT P-179 N-2)
  (PASSENGER-AT P-180 N-1)
  (PASSENGER-AT P-181 N-1)
  (PASSENGER-AT P-182 N-2)
  (PASSENGER-AT P-183 N-6)
  (PASSENGER-AT P-184 N-7)
  (PASSENGER-AT P-185 N-7)
  (PASSENGER-AT P-186 N-0)
  (PASSENGER-AT P-187 N-3)
  (PASSENGER-AT P-188 N-6)
  (PASSENGER-AT P-189 N-5)
  (PASSENGER-AT P-190 N-7)
  (PASSENGER-AT P-191 N-7)
  (PASSENGER-AT P-192 N-1)
  (PASSENGER-AT P-193 N-4)
  (PASSENGER-AT P-194 N-7)
  (PASSENGER-AT P-195 N-5)
  (PASSENGER-AT P-196 N-2)
  (PASSENGER-AT P-197 N-8)
  (PASSENGER-AT P-198 N-1)
  (PASSENGER-AT P-199 N-0)
  (PASSENGER-AT P-200 N-4)
  (PASSENGER-AT P-201 N-8)
  (PASSENGER-AT P-202 N-8)
  (PASSENGER-AT P-203 N-1)
  (PASSENGER-AT P-204 N-5)
  (PASSENGER-AT P-205 N-5)
  (PASSENGER-AT P-206 N-1)
  (PASSENGER-AT P-207 N-3)
  (PASSENGER-AT P-208 N-8)
  (PASSENGER-AT P-209 N-7)
  (PASSENGER-AT P-210 N-0)
  (PASSENGER-AT P-211 N-1)
  (PASSENGER-AT P-212 N-4)
  (PASSENGER-AT P-213 N-0)
  (PASSENGER-AT P-214 N-5)
  (PASSENGER-AT P-215 N-3)
  (PASSENGER-AT P-216 N-2)
  (PASSENGER-AT P-217 N-6)
  (PASSENGER-AT P-218 N-5)
  (PASSENGER-AT P-219 N-8)
  (PASSENGER-AT P-220 N-7)
  (PASSENGER-AT P-221 N-2)
  (PASSENGER-AT P-222 N-3)
  (PASSENGER-AT P-223 N-4)
  (PASSENGER-AT P-224 N-1)
  (PASSENGER-AT P-225 N-3)
  (PASSENGER-AT P-226 N-8)
  (PASSENGER-AT P-227 N-6)
  (PASSENGER-AT P-228 N-6)
  (PASSENGER-AT P-229 N-7)
  (PASSENGER-AT P-230 N-0)
  (PASSENGER-AT P-231 N-2)
  (PASSENGER-AT P-232 N-2)
  (PASSENGER-AT P-233 N-4)
  (PASSENGER-AT P-234 N-8)
  (PASSENGER-AT P-235 N-8)
  (PASSENGER-AT P-236 N-3)
  (PASSENGER-AT P-237 N-3)
  (PASSENGER-AT P-238 N-5)
  (PASSENGER-AT P-239 N-1)
  (PASSENGER-AT P-240 N-5)
  (PASSENGER-AT P-241 N-8)
  (PASSENGER-AT P-242 N-2)
  (PASSENGER-AT P-243 N-6)
  (PASSENGER-AT P-244 N-4)
  (PASSENGER-AT P-245 N-6)
  (PASSENGER-AT P-246 N-3)
  (PASSENGER-AT P-247 N-5)
  (PASSENGER-AT P-248 N-7)
  (PASSENGER-AT P-249 N-6)
  (PASSENGER-AT P-250 N-1)
  (PASSENGER-AT P-251 N-6)
  (PASSENGER-AT P-252 N-2)
  (PASSENGER-AT P-253 N-8)
  (PASSENGER-AT P-254 N-4)
  (PASSENGER-AT P-255 N-2)
  (PASSENGER-AT P-256 N-2)
  (PASSENGER-AT P-257 N-8)
  (PASSENGER-AT P-258 N-0)
  (PASSENGER-AT P-259 N-2)
  (PASSENGER-AT P-260 N-0)
  (PASSENGER-AT P-261 N-4)
  (PASSENGER-AT P-262 N-8)
  (PASSENGER-AT P-263 N-4)
  (PASSENGER-AT P-264 N-6)
  (PASSENGER-AT P-265 N-0)
  (PASSENGER-AT P-266 N-1)
  (PASSENGER-AT P-267 N-3)
  (PASSENGER-AT P-268 N-3)
  (PASSENGER-AT P-269 N-6)
  (PASSENGER-AT P-270 N-4)
  (PASSENGER-AT P-271 N-4)
  (PASSENGER-AT P-272 N-0)
  (PASSENGER-AT P-273 N-3)
  (PASSENGER-AT P-274 N-0)
  (PASSENGER-AT P-275 N-1)
  (PASSENGER-AT P-276 N-2)
  (PASSENGER-AT P-277 N-4)
  (PASSENGER-AT P-278 N-8)
  (PASSENGER-AT P-279 N-2)
  (PASSENGER-AT P-280 N-6)
  (PASSENGER-AT P-281 N-2)
  (PASSENGER-AT P-282 N-6)
  (PASSENGER-AT P-283 N-4)
  (PASSENGER-AT P-284 N-0)
  (PASSENGER-AT P-285 N-6)
  (PASSENGER-AT P-286 N-0)
  (PASSENGER-AT P-287 N-1)
  (PASSENGER-AT P-288 N-1)
  (PASSENGER-AT P-289 N-5)
  (PASSENGER-AT P-290 N-7)
  (PASSENGER-AT P-291 N-0)
  (PASSENGER-AT P-292 N-8)
  (PASSENGER-AT P-293 N-2)
  (PASSENGER-AT P-294 N-5)
  (PASSENGER-AT P-295 N-8)
  (PASSENGER-AT P-296 N-8)
  (PASSENGER-AT P-297 N-3)
  (PASSENGER-AT P-298 N-8)
  (PASSENGER-AT P-299 N-2))
 (:GOAL
  (AND (PASSENGER-AT P-0 N-5) (PASSENGER-AT P-1 N-2) (PASSENGER-AT P-2 N-6)
   (PASSENGER-AT P-3 N-8) (PASSENGER-AT P-4 N-7) (PASSENGER-AT P-5 N-0)
   (PASSENGER-AT P-6 N-5) (PASSENGER-AT P-7 N-7) (PASSENGER-AT P-8 N-6)
   (PASSENGER-AT P-9 N-3) (PASSENGER-AT P-10 N-1) (PASSENGER-AT P-11 N-0)
   (PASSENGER-AT P-12 N-5) (PASSENGER-AT P-13 N-8) (PASSENGER-AT P-14 N-6)
   (PASSENGER-AT P-15 N-8) (PASSENGER-AT P-16 N-5) (PASSENGER-AT P-17 N-3)
   (PASSENGER-AT P-18 N-0) (PASSENGER-AT P-19 N-0) (PASSENGER-AT P-20 N-1)
   (PASSENGER-AT P-21 N-4) (PASSENGER-AT P-22 N-8) (PASSENGER-AT P-23 N-5)
   (PASSENGER-AT P-24 N-5) (PASSENGER-AT P-25 N-6) (PASSENGER-AT P-26 N-2)
   (PASSENGER-AT P-27 N-8) (PASSENGER-AT P-28 N-1) (PASSENGER-AT P-29 N-8)
   (PASSENGER-AT P-30 N-0) (PASSENGER-AT P-31 N-6) (PASSENGER-AT P-32 N-8)
   (PASSENGER-AT P-33 N-5) (PASSENGER-AT P-34 N-6) (PASSENGER-AT P-35 N-1)
   (PASSENGER-AT P-36 N-3) (PASSENGER-AT P-37 N-3) (PASSENGER-AT P-38 N-7)
   (PASSENGER-AT P-39 N-8) (PASSENGER-AT P-40 N-5) (PASSENGER-AT P-41 N-7)
   (PASSENGER-AT P-42 N-0) (PASSENGER-AT P-43 N-4) (PASSENGER-AT P-44 N-8)
   (PASSENGER-AT P-45 N-2) (PASSENGER-AT P-46 N-6) (PASSENGER-AT P-47 N-4)
   (PASSENGER-AT P-48 N-4) (PASSENGER-AT P-49 N-0) (PASSENGER-AT P-50 N-5)
   (PASSENGER-AT P-51 N-7) (PASSENGER-AT P-52 N-1) (PASSENGER-AT P-53 N-1)
   (PASSENGER-AT P-54 N-4) (PASSENGER-AT P-55 N-1) (PASSENGER-AT P-56 N-6)
   (PASSENGER-AT P-57 N-5) (PASSENGER-AT P-58 N-2) (PASSENGER-AT P-59 N-7)
   (PASSENGER-AT P-60 N-2) (PASSENGER-AT P-61 N-5) (PASSENGER-AT P-62 N-1)
   (PASSENGER-AT P-63 N-1) (PASSENGER-AT P-64 N-7) (PASSENGER-AT P-65 N-4)
   (PASSENGER-AT P-66 N-6) (PASSENGER-AT P-67 N-0) (PASSENGER-AT P-68 N-4)
   (PASSENGER-AT P-69 N-6) (PASSENGER-AT P-70 N-4) (PASSENGER-AT P-71 N-5)
   (PASSENGER-AT P-72 N-5) (PASSENGER-AT P-73 N-1) (PASSENGER-AT P-74 N-4)
   (PASSENGER-AT P-75 N-4) (PASSENGER-AT P-76 N-4) (PASSENGER-AT P-77 N-6)
   (PASSENGER-AT P-78 N-2) (PASSENGER-AT P-79 N-0) (PASSENGER-AT P-80 N-5)
   (PASSENGER-AT P-81 N-5) (PASSENGER-AT P-82 N-1) (PASSENGER-AT P-83 N-6)
   (PASSENGER-AT P-84 N-8) (PASSENGER-AT P-85 N-2) (PASSENGER-AT P-86 N-8)
   (PASSENGER-AT P-87 N-2) (PASSENGER-AT P-88 N-5) (PASSENGER-AT P-89 N-7)
   (PASSENGER-AT P-90 N-7) (PASSENGER-AT P-91 N-2) (PASSENGER-AT P-92 N-4)
   (PASSENGER-AT P-93 N-8) (PASSENGER-AT P-94 N-3) (PASSENGER-AT P-95 N-6)
   (PASSENGER-AT P-96 N-5) (PASSENGER-AT P-97 N-2) (PASSENGER-AT P-98 N-1)
   (PASSENGER-AT P-99 N-7) (PASSENGER-AT P-100 N-7) (PASSENGER-AT P-101 N-0)
   (PASSENGER-AT P-102 N-5) (PASSENGER-AT P-103 N-8) (PASSENGER-AT P-104 N-1)
   (PASSENGER-AT P-105 N-2) (PASSENGER-AT P-106 N-3) (PASSENGER-AT P-107 N-1)
   (PASSENGER-AT P-108 N-7) (PASSENGER-AT P-109 N-1) (PASSENGER-AT P-110 N-1)
   (PASSENGER-AT P-111 N-6) (PASSENGER-AT P-112 N-0) (PASSENGER-AT P-113 N-3)
   (PASSENGER-AT P-114 N-6) (PASSENGER-AT P-115 N-8) (PASSENGER-AT P-116 N-7)
   (PASSENGER-AT P-117 N-2) (PASSENGER-AT P-118 N-3) (PASSENGER-AT P-119 N-8)
   (PASSENGER-AT P-120 N-6) (PASSENGER-AT P-121 N-1) (PASSENGER-AT P-122 N-6)
   (PASSENGER-AT P-123 N-4) (PASSENGER-AT P-124 N-8) (PASSENGER-AT P-125 N-1)
   (PASSENGER-AT P-126 N-4) (PASSENGER-AT P-127 N-4) (PASSENGER-AT P-128 N-7)
   (PASSENGER-AT P-129 N-1) (PASSENGER-AT P-130 N-2) (PASSENGER-AT P-131 N-5)
   (PASSENGER-AT P-132 N-3) (PASSENGER-AT P-133 N-7) (PASSENGER-AT P-134 N-3)
   (PASSENGER-AT P-135 N-1) (PASSENGER-AT P-136 N-6) (PASSENGER-AT P-137 N-3)
   (PASSENGER-AT P-138 N-2) (PASSENGER-AT P-139 N-1) (PASSENGER-AT P-140 N-7)
   (PASSENGER-AT P-141 N-6) (PASSENGER-AT P-142 N-2) (PASSENGER-AT P-143 N-1)
   (PASSENGER-AT P-144 N-2) (PASSENGER-AT P-145 N-3) (PASSENGER-AT P-146 N-6)
   (PASSENGER-AT P-147 N-3) (PASSENGER-AT P-148 N-3) (PASSENGER-AT P-149 N-6)
   (PASSENGER-AT P-150 N-4) (PASSENGER-AT P-151 N-6) (PASSENGER-AT P-152 N-1)
   (PASSENGER-AT P-153 N-8) (PASSENGER-AT P-154 N-1) (PASSENGER-AT P-155 N-2)
   (PASSENGER-AT P-156 N-6) (PASSENGER-AT P-157 N-3) (PASSENGER-AT P-158 N-5)
   (PASSENGER-AT P-159 N-2) (PASSENGER-AT P-160 N-5) (PASSENGER-AT P-161 N-6)
   (PASSENGER-AT P-162 N-6) (PASSENGER-AT P-163 N-8) (PASSENGER-AT P-164 N-6)
   (PASSENGER-AT P-165 N-3) (PASSENGER-AT P-166 N-5) (PASSENGER-AT P-167 N-8)
   (PASSENGER-AT P-168 N-4) (PASSENGER-AT P-169 N-5) (PASSENGER-AT P-170 N-6)
   (PASSENGER-AT P-171 N-2) (PASSENGER-AT P-172 N-5) (PASSENGER-AT P-173 N-3)
   (PASSENGER-AT P-174 N-7) (PASSENGER-AT P-175 N-4) (PASSENGER-AT P-176 N-5)
   (PASSENGER-AT P-177 N-0) (PASSENGER-AT P-178 N-2) (PASSENGER-AT P-179 N-0)
   (PASSENGER-AT P-180 N-5) (PASSENGER-AT P-181 N-0) (PASSENGER-AT P-182 N-8)
   (PASSENGER-AT P-183 N-6) (PASSENGER-AT P-184 N-8) (PASSENGER-AT P-185 N-2)
   (PASSENGER-AT P-186 N-2) (PASSENGER-AT P-187 N-8) (PASSENGER-AT P-188 N-4)
   (PASSENGER-AT P-189 N-8) (PASSENGER-AT P-190 N-4) (PASSENGER-AT P-191 N-3)
   (PASSENGER-AT P-192 N-6) (PASSENGER-AT P-193 N-8) (PASSENGER-AT P-194 N-8)
   (PASSENGER-AT P-195 N-2) (PASSENGER-AT P-196 N-8) (PASSENGER-AT P-197 N-2)
   (PASSENGER-AT P-198 N-8) (PASSENGER-AT P-199 N-7) (PASSENGER-AT P-200 N-6)
   (PASSENGER-AT P-201 N-1) (PASSENGER-AT P-202 N-2) (PASSENGER-AT P-203 N-6)
   (PASSENGER-AT P-204 N-0) (PASSENGER-AT P-205 N-8) (PASSENGER-AT P-206 N-7)
   (PASSENGER-AT P-207 N-1) (PASSENGER-AT P-208 N-8) (PASSENGER-AT P-209 N-5)
   (PASSENGER-AT P-210 N-3) (PASSENGER-AT P-211 N-7) (PASSENGER-AT P-212 N-6)
   (PASSENGER-AT P-213 N-0) (PASSENGER-AT P-214 N-3) (PASSENGER-AT P-215 N-8)
   (PASSENGER-AT P-216 N-2) (PASSENGER-AT P-217 N-6) (PASSENGER-AT P-218 N-2)
   (PASSENGER-AT P-219 N-8) (PASSENGER-AT P-220 N-2) (PASSENGER-AT P-221 N-0)
   (PASSENGER-AT P-222 N-8) (PASSENGER-AT P-223 N-8) (PASSENGER-AT P-224 N-1)
   (PASSENGER-AT P-225 N-4) (PASSENGER-AT P-226 N-8) (PASSENGER-AT P-227 N-0)
   (PASSENGER-AT P-228 N-6) (PASSENGER-AT P-229 N-8) (PASSENGER-AT P-230 N-4)
   (PASSENGER-AT P-231 N-3) (PASSENGER-AT P-232 N-7) (PASSENGER-AT P-233 N-3)
   (PASSENGER-AT P-234 N-2) (PASSENGER-AT P-235 N-2) (PASSENGER-AT P-236 N-3)
   (PASSENGER-AT P-237 N-7) (PASSENGER-AT P-238 N-3) (PASSENGER-AT P-239 N-1)
   (PASSENGER-AT P-240 N-7) (PASSENGER-AT P-241 N-0) (PASSENGER-AT P-242 N-5)
   (PASSENGER-AT P-243 N-5) (PASSENGER-AT P-244 N-0) (PASSENGER-AT P-245 N-7)
   (PASSENGER-AT P-246 N-5) (PASSENGER-AT P-247 N-0) (PASSENGER-AT P-248 N-3)
   (PASSENGER-AT P-249 N-4) (PASSENGER-AT P-250 N-4) (PASSENGER-AT P-251 N-7)
   (PASSENGER-AT P-252 N-3) (PASSENGER-AT P-253 N-5) (PASSENGER-AT P-254 N-2)
   (PASSENGER-AT P-255 N-8) (PASSENGER-AT P-256 N-0) (PASSENGER-AT P-257 N-0)
   (PASSENGER-AT P-258 N-3) (PASSENGER-AT P-259 N-8) (PASSENGER-AT P-260 N-4)
   (PASSENGER-AT P-261 N-2) (PASSENGER-AT P-262 N-7) (PASSENGER-AT P-263 N-4)
   (PASSENGER-AT P-264 N-6) (PASSENGER-AT P-265 N-1) (PASSENGER-AT P-266 N-3)
   (PASSENGER-AT P-267 N-5) (PASSENGER-AT P-268 N-8) (PASSENGER-AT P-269 N-2)
   (PASSENGER-AT P-270 N-8) (PASSENGER-AT P-271 N-0) (PASSENGER-AT P-272 N-8)
   (PASSENGER-AT P-273 N-3) (PASSENGER-AT P-274 N-4) (PASSENGER-AT P-275 N-0)
   (PASSENGER-AT P-276 N-7) (PASSENGER-AT P-277 N-5) (PASSENGER-AT P-278 N-6)
   (PASSENGER-AT P-279 N-6) (PASSENGER-AT P-280 N-0) (PASSENGER-AT P-281 N-8)
   (PASSENGER-AT P-282 N-6) (PASSENGER-AT P-283 N-1) (PASSENGER-AT P-284 N-4)
   (PASSENGER-AT P-285 N-2) (PASSENGER-AT P-286 N-8) (PASSENGER-AT P-287 N-5)
   (PASSENGER-AT P-288 N-0) (PASSENGER-AT P-289 N-2) (PASSENGER-AT P-290 N-3)
   (PASSENGER-AT P-291 N-8) (PASSENGER-AT P-292 N-5) (PASSENGER-AT P-293 N-8)
   (PASSENGER-AT P-294 N-3) (PASSENGER-AT P-295 N-6) (PASSENGER-AT P-296 N-3)
   (PASSENGER-AT P-297 N-8) (PASSENGER-AT P-298 N-5) (PASSENGER-AT P-299 N-3))))