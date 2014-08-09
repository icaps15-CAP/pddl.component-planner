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


(test enhance-domain
  (finishes
    (multiple-value-bind (problem domain)
        (enhance-problem assemblep :product)
      (print-pddl-object problem *standard-output*)
      (terpri *standard-output*)
      (print-pddl-object domain *standard-output*))))

(test solve-enhanced-domain
  (finishes
    (multiple-value-bind (problem domain)
        (enhance-problem assemblep :product)
      (let* ((dir (mktemp "enhanced"))
             (pp (write-pddl problem "problem.pddl" dir))
             (dp (write-pddl domain "domain.pddl" dir))
             (results (multiple-value-list (test-problem pp dp :verbose t))))
        (is-true (validate-plan dp pp (first (first results))
                                :verbose t))))))
        
        
       
