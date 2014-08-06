(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

@export
(defun task-plan-equal (t1 t2)
  "Computes plan-wise compatibility. It returns true if any of the component plan
mapped from t1 to t2 is a valid plan of t2."
  ;; (assert (abstract-component-task-strict= t1 t2))
  (signal 'comparison-signal)
  (let* ((problem2 (build-component-problem t2))
         (problem-path2 (write-problem problem2)))
    (some 
     (lambda (plan1)
       (validate-plan (path (domain problem2))
                      problem-path2
                      (write-plan
                       (apply-mapping
                        plan1
                        (mapping-between-tasks t1 t2)
                        problem2))))
     (plan-task-with-retry t1))))


