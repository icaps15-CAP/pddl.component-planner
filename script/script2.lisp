

(asdf:load-system :pddl.component-planner-test)
(in-package :pddl.component-planner-test)

(defun run (i)
  (handler-bind ((error (lambda (c)
                          (sb-ext:exit)))) 
    (benchmark i)))

