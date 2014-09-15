
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
                   :goal `(and ,@goal ;; maintain the previously achieved goal is true
                               ,@(set-difference
                                  (remove-if
                                   (rcurry #'typep 'pddl-function-state)
                                   (init *problem*))
                                  init ;; remove the fluents regarding this task
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

#+nil
(defun reverse-macro (pair &optional
                             (*problem* *problem*)
                             (*domain* *domain*))
  "deprecated. the precondition does not contain restriction --> bloat"
  (multiple-value-bind (gms tasks) (get-actions-grounded pair)
    (when-let ((plan (%solve-rev
                      (reverse-problem (first gms) (first tasks))
                      *domain*)))
      ;; reuse this function
      (ematch (component-macro/bpvector (vector tasks plan)) ;; -> (vector tasks macro)
        ((and it (vector _ (macro-action (name (place name)))))
         (setf name (symbolicate 'rev- name))
         it)))))


(defun cyclic-macro (pair &optional (*problem* *problem*) (*domain* *domain*))
  "grounded by default.
 Assume *problem*,*domain* is the original problem/domain."
  (handler-bind ((warning #'muffle-warning))
    (multiple-value-bind (gms tasks) (get-actions-grounded pair)
      (if-let ((plan (%solve-rev
                      (reverse-problem (first gms) (first tasks))
                      *domain*)))
        ;; reuse this function
        (let ((reverse-gms (get-actions-grounded
                            (component-macro/bpvector (vector tasks plan)))))
          (values
           (mapcar
            (lambda (gm rgm)
              (change-class
               (merge-ground-actions
                (change-class gm 'ground-macro-action :problem *problem*)
                (change-class rgm 'ground-macro-action :problem *problem*))
               'macro-action
               :parameters nil
               :actions (concatenate 'vector (actions gm) (actions rgm))
               :name
               (let ((str (symbol-name
                           (symbolicate
                            'cycle- (symbol-name (name gm))))))
                 (if (< 30 (length str))
                     (gensym (subseq str 0 29))
                     (gensym str)))))
            gms reverse-gms)
           tasks))
        (progn (format t "~& No reverse macro found in this task. Grounding..")
               (values gms tasks))))))




