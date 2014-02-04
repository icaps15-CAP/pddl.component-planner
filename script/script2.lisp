
(print "This is script2.lisp")

(asdf:load-system :pddl.component-planner-test)
(in-package :pddl.component-planner-test)

(handler-bind ((error (lambda (c)
                          (sb-ext:exit))))
  (benchmark (parse-integer (second sb-ext:*posix-argv*))))

