
(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

;;; categorize each problem

(defun trivial-component-p (ac)
  (ematch ac
    ((abstract-component components)
     (= 1 (length components)))))

(defun component-plans (problem seed &aux (domain (domain problem)))
  ;; -> (list (vector (list task) plan))
  (multiple-value-bind (problem domain) (binarize problem domain)
    @ignorable domain
    (format t "~2&Categorizing PROBLEM ~a with seed ~a" (name problem) seed)
    (let* (;; tasks of the same component but the different init/goals
           (tasks (abstract-tasks problem seed))
           ;; remove tasks of the trivial component = components of single object
           (tasks/non-trivial (remove-if #'trivial-component-p tasks :key #'abstract-component-task-ac))
           ;; remove tasks without goals
           (tasks/with-goals (remove-if-not #'abstract-component-task-goal tasks/non-trivial))
           ;; categorize tasks into buckets, based on init/goal/attribute.
           (tasks/same-goal-inits-attr (categorize-tasks tasks/with-goals :strict)))
      ;; list pf bags. each bag contains tasks of the same structure
      (format t "~&GROUPS OF TASKS with same goals/init/attributes : ~a"
              (mapcar #'length tasks/same-goal-inits-attr))
      (format t "~&Categorizing TASKS by equality -- calling FD")
      (let ((tasks/plan
             (reduce #'append tasks/same-goal-inits-attr
                     :key (lambda (bucket)
                            (in-reply-to ((verbosity t)
                                          (time-limit 100)
                                          (hard-time-limit 200))
                              (coerce (categorize-by-equality
                                       bucket #'task-plan-equal
                                       :transitive t)
                                      'list)))
                     :initial-value nil)))
        (format t " ... finished.")
        (format t "~&Clustered into ~a groups." (length tasks/plan))
        ;; list of bags. each bag contains tasks whose plans are interchangeable
        (iter (for task-bucket in tasks/plan)
              ;; assume the cached value of plan-task
              (when-let ((plans-for-a-task (some #'plan-task task-bucket)))
                (collect
                    (vector task-bucket
                            ;; TODO: what if the component-plan does not exists?
                            (first plans-for-a-task)))))))))

(defun component-macro (problem seed &aux (domain (domain problem)))
  (multiple-value-bind (problem *domain*) (binarize problem domain)
    (remove nil
            (mapcar #'component-macro/bucket
                    (component-plans problem seed)))))

(defun component-macro/bucket (v)
  (ematch v
    ((vector _ (pddl-plan actions))
     (macro-action actions)))) ; might return nil

(defun enhance-problem (problem &optional seed &aux (domain (domain problem)))
  (declare (ignore seed))
  (format t "~&Binarizing domain ~a" domain)
  (multiple-value-bind (problem domain) (binarize problem domain)
    (format t "~&Enhancing domain ~a" domain)
    (ematch domain
      ((pddl-domain name actions)
       (let* ((macros (iter (for seed in (types-in-goal problem))
                            (appending (component-macro problem seed))))
              (*domain*
               (shallow-copy
                domain
                :name (symbolicate name '-enhanced)
                :actions (append actions macros))))
         (if macros
             (format t "~&~a macros found." (length macros))
             (warn "No component macros are found!"))
         (values (shallow-copy problem
                               :name (symbolicate (name problem)
                                                  '-enhanced)
                               :domain *domain*)
                 *domain*
                 macros))))))

(defun types-in-goal (problem)
  (ematch problem
    ((pddl-problem positive-goals)
     (let (types)
       (dolist (type (mapcar #'type (mappend #'parameters positive-goals)) types)
         (pushnew type types))))))
  
(defun solve-problem-enhancing (problem &rest test-problem-args)
  (clear-plan-task-cache)
  (format t "~&Enhancing the problem with macros.")
  (multiple-value-bind (eproblem edomain macros) (enhance-problem problem)
    (format t "~&Enhancement finished.~&Solving the enhanced problem with FD.")
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
                            (first
                             (prog1 (apply #'test-problem
                                           (write-pddl *problem* "eproblem.pddl" dir)
                                           (write-pddl *domain* "edomain.pddl" dir)
                                           test-problem-args)
                               (format t "~&Decoding the result plan."))))))))))

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
  

