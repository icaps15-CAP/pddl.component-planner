
(in-package :pddl.component-planner)

(defparameter *component-plan-time-limit* 300)
(defvar *start* nil)
(defun within-time-limit ()
  (if (numberp *start*)
      (or (< (- (get-universal-time) *start*)
             *component-plan-time-limit*)
          (format t "~&Reached the time limit!"))
      t))
