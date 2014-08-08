(in-package :pddl.component-planner-test)
(in-suite :pddl.component-planner)


(test component
  (let (task-groups)
    (finishes
      (setf task-groups (abstract-tasks assemblep :product)))
    (finishes
      (ematch task-groups
        ((list (list t1 t2)) task-groups
         (is-false (task-plan-equal t1 t2))))))) ; since the problem is not
                                               ; binarized and the
                                               ; abstraction fails

(test component-after-binarization
  (let (task-groups)
    (finishes
      (setf task-groups (abstract-tasks (binarize assemblep assemble) :product)))
    (finishes
      (ematch task-groups
        ((list (list t1 t2)) task-groups
         (is (task-plan-equal t1 t2)))))))

