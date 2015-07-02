(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

;; variable factoring

(defun variable-factoring-bpvectors (bproblem *problem* domain)
  (declare (ignorable domain))
  (ematch *problem*
    ((pddl-problem positive-goals)
     ;; same reporter as in task-bags
     (format t "~&Using Factor=Variable strategy")
     (format t "~&Tasks found : ~a" (length positive-goals))
     (format t "~&Removing tasks w/o goals : ~a" (length positive-goals))
     (format t "~&TASKS/g/i/attr : ~a" (mapcar (constantly 1) positive-goals))
     (iter (for goal in positive-goals)
           (for task = (make-abstract-component-task
                        :problem bproblem
                        ;; NOTE: these facts may contain environment objects
                        ;; when they are more than 3 arg predicates.
                        :init nil
                        :goal (list (debinarize-predicate goal))
                        :ac (make-abstract-component
                             :seed nil
                             :facts nil
                             :components nil
                             :attributes nil)))
           (when-let ((plans-for-a-task (plan-task task)))
             (collect (vector (list task)
                              (first plans-for-a-task))))))))
