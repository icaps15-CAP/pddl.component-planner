
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
       (let* ((macros (component-macro problem seed))
              (*domain*
               (shallow-copy
                domain
                :name (symbolicate name '-enhanced)
                :actions (append actions macros))))
         (values (shallow-copy problem
                               :name (symbolicate (name problem)
                                                  '-enhanced)
                               :domain *domain*)
                 *domain*
                 macros))))))
  
(defun solve-problem-enhancing (problem seed &rest test-problem-args)
  (multiple-value-bind (eproblem edomain macros) (enhance-problem problem seed)
    (let* ((dir (mktemp "enhanced")))
      (debinarize-plan
       (domain problem)
       problem
       edomain
       eproblem
       (let ((*domain* edomain) (*problem* eproblem))
         (reduce #'decode-plan
                 macros
                 :from-end t
                 :initial-value
                 (pddl-plan :path
                            (first (apply #'test-problem
                                          (write-pddl *problem* "eproblem.pddl" dir)
                                          (write-pddl *domain* "edomain.pddl" dir)
                                          test-problem-args)))))))))

(define-local-function debinarize-action (ga)
  (let ((a (find ga (actions bdomain) :test #'eqname)))
    (ematch a
      ((macro-action binarization-origin)
       (ematch binarization-origin
         ((pddl-action name)
          (shallow-copy ga
                        :domain domain
                        :problem problem
                        :name name))))  ; change the name
      ((pddl-action) ; the action might not have been binarized (because 
       ga))))        ; there is no need for it)

(defun debinarize-plan (domain problem bdomain bproblem plan)
  (declare (ignorable bproblem))
  (more-labels () (debinarize-action)
    (ematch plan
      ((pddl-plan :actions actions)
       (pddl-plan :actions (map 'vector #'debinarize-action actions))))))



;; in order to set (domain/problem plan)
;; during the initialization
  

