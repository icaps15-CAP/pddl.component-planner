(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

@export
(define-condition evaluation-signal (simple-condition)
  ((time :initarg :time :reader elapsed-time)))
@export
(define-condition restored-evaluation-signal (simple-condition) ())
@export
(define-condition comparison-signal (simple-condition) ())

@export 'elapsed-time
