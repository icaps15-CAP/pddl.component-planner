(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

#|

Condition classes for logging and performance measurement.

|#

(define-condition evaluation-signal (simple-condition)
  ((usage :initarg :usage :reader resource-usage)))

(define-condition restored-evaluation-signal (simple-condition) ())

(define-condition comparison-signal (simple-condition) ())

