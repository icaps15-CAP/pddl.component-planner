
(print "This is script2.lisp")

(asdf:load-system :pddl.component-planner-test)
(in-package :pddl.component-planner-test)

(defun run-benchmark (i)
  (format t "running ~a th domain in script2.lisp" i)
  (handler-bind ((error (lambda (c)
                          (sb-ext:exit))))
    (benchmark i)
    (print "finished successfully.")))

