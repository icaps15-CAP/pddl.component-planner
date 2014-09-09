
(in-package :pddl.component-planner)

#|

given a set of grounded macros -> 

find the shortest path to the entrance of next macro

then add it as another macro

|#

(defun reverse-problem (gmacro task &optional
                                      (*problem* *problem*)
                                      (*domain* *domain*))
  (match task
    ((abstract-component-task init goal)
     (pddl-problem :name (symbolicate 'rev- (name gmacro))
                   :domain *domain*
                   :objects (objects/const *problem*)
                   :metric (metric *problem*)
                   :init (apply-ground-action gmacro (init *problem*))
                   :goal `(and ,@goal
                               ,@(set-difference
                                  (remove-if
                                   (rcurry #'typep 'pddl-function-state)
                                   (init *problem*))
                                  init
                                  :test #'eqstate))))))

(defun %solve-rev (*problem* *domain*)
  (let* ((dir (mktemp "reverse-problem" t)))
    (when (within-time-limit)
      (multiple-value-match
          (funcall #'test-problem-common
                   (write-pddl *problem* "problem.pddl" dir)
                   (write-pddl *domain* "domain.pddl" dir)
                   :name *preprocessor*
                   :options *preprocessor-options*
                   :time-limit 1
                   :hard-time-limit *component-plan-time-limit*
                   :memory *memory-limit*
                   :verbose *debug-preprocessing*)
        ((plans time memory _)
         (signal 'evaluation-signal :usage (list time memory))
         (when plans
           (pddl-plan
            :name (concatenate-symbols
                   (name *problem*) 'plan)
            :path (first plans))))))))


(defun reverse-macro (pair &optional
                             (*problem* *problem*)
                             (*domain* *domain*))
  "deprecated. the precondition does not contain restriction --> bloat"
  (multiple-value-bind (gms tasks) (get-actions-grounded pair)
    (when-let ((plan (%solve-rev
                      (reverse-problem (first gms) (first tasks))
                      *domain*)))
      ;; reuse this function
      (match (component-macro/bpvector (vector tasks plan)) ;; -> (vector tasks macro)
       ((and it (vector _ (macro-action (name (place name)))))
        (setf name (symbolicate 'rev- name))
        it)))))
