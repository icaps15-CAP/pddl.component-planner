

(ql:quickload :pddl.component-planner.experiment)
;;(sb-profile:profile "PDDL" "PDDL.COMPONENT-PLANNER" "PDDL.COMPONENT-ABSTRACTION" "GUICHO-UTILITIES" "EAZYLAZY")
(in-package "PDDL.COMPONENT-PLANNER.EXPERIMENT")
(swank:set-default-directory "/mnt/video/guicho/repos/lisp/pddl.component-planner/aaai/")

(time (test1-1))
(time (test1-2))
(time (test1-3))
;;(time (test1-4))
(time (test2-1))
(time (test2-2))
(time (test2-3))
;;(time (test2-4))
(sb-profile:report :print-no-call-list nil)


(in-package "PDDL.COMPONENT-PLANNER.EXPERIMENT")
(time (test2-2))
(sb-profile:report :print-no-call-list nil)

(time (test2-3))


;; testing
(progn
  (time (test1-1))
  (time (test2-1))
  (time (test3-1))
  (time (test4-1))
  (time (test5-1))
  (time (test6-1))
  (time (test7-1))
  (time (test8-1)))

(let ((*preprocess-only* t))
  (time (test1-1))
  (time (test2-1))
  (time (test3-1))
  (time (test4-1))
  (time (test5-1))
  (time (test6-1))
  (time (test7-1))
  (time (test8-1)))

(let ((*preprocess-only* t))
  (time (test8-1)))

(time (test8-1))
