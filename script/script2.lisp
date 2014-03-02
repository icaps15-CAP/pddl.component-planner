
(print "This is script2.lisp")

(asdf:load-system :pddl.component-planner-test)
(in-package :pddl.component-planner-test)

(defparameter *delayed-problems*
              (list (delay (load-and-collect-problems
                            '(:pddl.instances.barman-x1.3)
                            '(:cocktail :shot)
                            ".*BARMAN-.*"))
                    (delay (load-and-collect-problems
                            '(:pddl.instances.cell-assembly-eachparts)
                            '(:base)
                            "CELL-ASSEMBLY-(2A2B-MIXED-EACH-.*|MODEL2A-EACH-[12][0-9])"))
                    (delay (load-and-collect-problems
                            '(:pddl.instances.elevators)
                            '(:passenger)
                            ".*ELEVATORS.*"))
                    (delay (load-and-collect-problems
                            '(:pddl.instances.openstacks)
                            '(:order :product)
                            ".*OPENSTACKS.*"))
                    (delay (load-and-collect-problems
                            '(:pddl.instances.rover)
                            '(:objective)
                            "ROVERPROB[0-9]*"))
                    (delay (load-and-collect-problems
                            '(:pddl.instances.satellite-typed)
                            '(:direction)
                            "SATELLITE-TYPED-.*"))
                    (delay (load-and-collect-problems
                            '(:pddl.instances.woodworking-large
                              :pddl.instances.woodworking-xlarge)
                            '(:part)
                            "WOOD-PROB-SAT-[0-9]*"))))

(defun run-benchmark (i)
  (format t "running ~a th domain in script2.lisp" i)
  (handler-bind ((storage-condition (lambda (c)
                                  (sb-ext:exit :code 1)))
                 (error (lambda (c) (continue c))))
    (benchmark i)
    (print "finished successfully.")))

