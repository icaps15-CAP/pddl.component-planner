(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

#|

Condition classes for logging and performance measurement.

|#

@export
(define-condition evaluation-signal (simple-condition)
  ((usage :initarg :usage :reader resource-usage)))
@export
(define-condition restored-evaluation-signal (simple-condition) ())
@export
(define-condition comparison-signal (simple-condition) ())

