(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

(defmacro format<> (&rest args)
  `(progn (format ,@args) <>))
