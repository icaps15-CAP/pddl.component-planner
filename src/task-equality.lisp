(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

@export
(defun task-plan-equal (t1 t2)
  "Computes plan-wise compatibility. It returns true if any of the component plan
mapped from t1 to t2 is a valid plan of t2.

The secondary value is the component plans for t1, which might be nil due
to the internal time limit. If the secondary value is nil, the primary
value is also nil.

This function guarantees the compatibility between the tasks. However, IT
DOES NOT DUARANTEE the INcompatibility between the tasks. If the primary
value is NIL, there are 3 possibilities: there are no plans of t1 found in
the time limit, all plans of t1 was not compatible with t2, or, finally,
only part of possible plans for t1 were found and all of them were
incompatible. We cannot tell between the second and the third case, so this
function basically cannot negatively prove the compatibility.
"
  ;; (assert (abstract-component-task-strict= t1 t2))
  (signal 'comparison-signal)
  (let* ((dir (mktemp "task-equal" t))
         (problem2 (build-component-problem t2))
         (problem-path2 (write-pddl problem2 "mapped-problem.pddl" dir))
         (domain-path (write-pddl (domain problem2) "domain.pddl" dir)))
    (multiple-value-bind (plans complete) (plan-task-with-retry t1)
      (cond
        (plans
         (values
          (some
           (let ((i 0))
             (lambda (plan1)
               (validate-plan
                domain-path
                problem-path2
                (write-plan
                 (map-component-plan plan1 (mapping-between-tasks t1 t2))
                 (prog1 (format nil "mapped.plan.~a" i) (incf i))
                 dir))))
           plans)
          :proven))
        ((and (null plans) complete)
         (values nil :proven))
        ((and (null plans) (not complete))
         (values nil nil))))))


