
(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

;;; categorize each problem

(defun component-plans (problem seed) ;; -> (list (vector (list task) plan))
  (format t "~&Categorizing problem ~a with seed ~a" (name problem) seed)
  (let* (;; tasks of the same component but the different init/goals
         (tasks (abstract-tasks (binarize problem (domain problem)) seed))
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
              tasks/plan))))


(defun component-macro (problem seed)
  (mapcar #'component-macro/bucket (component-plans problem seed)))

(defun component-macro/bucket (v)
  (match v
    ((vector _ (pddl-plan actions))
     (reduce #'merge-ground-actions actions))))


