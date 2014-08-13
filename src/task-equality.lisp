(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

@export
(defun task-plan-equal (t1 t2)
  "Computes plan-wise compatibility. It returns true if any of the component plan
mapped from t1 to t2 is a valid plan of t2."
  ;; (assert (abstract-component-task-strict= t1 t2))
  (signal 'comparison-signal)
  (let* ((dir (mktemp "task-equal" t))
         (problem2 (build-component-problem t2))
         (problem-path2 (write-pddl problem2 "mapped-problem.pddl" dir))
         (domain-path (write-pddl (domain problem2) "domain.pddl" dir)))
    (some
     (let ((i 0))
       (lambda (plan1)
         (validate-plan
          domain-path
          problem-path2
          (write-plan
           (map-component-plan plan1 (mapping-between-tasks t1 t2))
           (prog1 (format nil "mapped.plan.~a" i) (incf i))
           dir
           (ask-for verbose t)))))
     (plan-task-with-retry t1))))


