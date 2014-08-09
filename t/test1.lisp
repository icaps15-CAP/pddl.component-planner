(in-package :pddl.component-planner-test)
(in-suite :pddl.component-planner)


(test component
  (let (tasks)
    (finishes
      (setf tasks (abstract-tasks assemblep :product)))
    (finishes
      (ematch tasks
        ((list t1 t2)
         (is-false (task-plan-equal t1 t2))))))) ; since the problem is not
                                        ; binarized and the
                                        ; abstraction fails

(test component-after-binarization
  (let (tasks)
    (finishes
      (setf tasks
            (abstract-tasks
             (binarize assemblep assemble)
             :product)))
    (finishes
      (ematch tasks
        ((list t1 t2)
         (is (task-plan-equal t1 t2)))))))


(test component-macro
  (finishes
    (print
     (component-macro assemblep :product))))

