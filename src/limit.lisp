
(in-package :pddl.component-planner)

(defparameter *preprocess-time-limit* 300)
(defparameter *start* nil)
(defun within-time-limit ()
  (if (numberp *start*)
      (or (< (- (get-universal-time) *start*)
             *preprocess-time-limit*)
          (format t "~&Reached the time limit!"))
      t))
