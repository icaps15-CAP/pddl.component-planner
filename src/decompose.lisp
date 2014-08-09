
(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

;;; categorize each problem

(defun component-plans (problem seed &aux (domain (domain problem)))
  ;; -> (list (vector (list task) plan))
  (multiple-value-bind (problem domain) (binarize problem domain)
    @ignorable domain
    (format t "~&Categorizing problem ~a with seed ~a" (name problem) seed)
    (let* (;; tasks of the same component but the different init/goals
           (tasks (abstract-tasks problem seed))
           ;; categorize tasks into buckets, based on init/goal/attribute.
           (tasks/same-goal-inits-attr (categorize-tasks tasks :strict)))
      ;; list pf bags. each bag contains tasks of the same structure
      (let ((tasks/plan
             (reduce #'append tasks/same-goal-inits-attr
                     :key (lambda (bucket)
                            (coerce (categorize-by-equality
                                     bucket #'task-plan-equal
                                     :transitive t)
                                    'list))
                     :initial-value nil)))
        ;; list of bags. each bag contains tasks whose plans are interchangeable
        (mapcar (lambda (task-bucket)
                  ;; assume the cached value of plan-task
                  (vector task-bucket
                          (first (plan-task (first task-bucket)))))
                tasks/plan)))))

(defun component-macro (problem seed &aux (domain (domain problem)))
  (multiple-value-bind (problem *domain*) (binarize problem domain)
    (mapcar #'component-macro/bucket
            (component-plans problem seed))))

(defun component-macro/bucket (v)
  (match v
    ((vector _ (pddl-plan actions))
     (macro-action actions))))

(defun enhance-problem (problem seed &aux (domain (domain problem)))
  (multiple-value-bind (problem domain) (binarize problem domain)
    (format t "~&Enhancing domain ~a" domain)
    (ematch domain
      ((pddl-domain name actions)
       (let ((*domain*
              (shallow-copy
               domain
               :name (symbolicate name '-enhanced)
               :actions (append actions
                                (component-macro problem seed)))))
         (values (shallow-copy problem
                               :name (symbolicate (name problem)
                                                  '-enhanced)
                               :domain *domain*)
                 *domain*))))))
  
  

