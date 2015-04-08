
(in-package :pddl.component-planner)

(defparameter *preprocess-time-limit* MOST-POSITIVE-FIXNUM)
(defparameter *start* nil)
(defun within-time-limit ()
  (if (numberp *start*)
      (or (< (- (get-universal-time) *start*)
             *preprocess-time-limit*)
          (format t "~&Reached the time limit!"))
      t))
(defun elapsed-time ()
  (- (get-universal-time) *start*))
