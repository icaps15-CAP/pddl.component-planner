
(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

;;; categorize each problem

(defun trivial-component-p (ac)
  (ematch ac
    ((abstract-component components)
     (= 1 (length components)))))

(defun categorize-bag (bag)
  (coerce (categorize-by-equality bag #'maybe-task-plan-equal :transitive t)
          'list))

(defun maybe-task-plan-equal (x y)
  (multiple-value-bind (result plans) (task-plan-equal x y)
    (if plans
        (progn (format t "~&Compatibility~@[ not~] proven" result) result)
        (progn (format t "~&Compatibility not proven, assuming true") t))))


(defun component-plans (problem seed &aux (domain (domain problem)))
  ;; -> (list (vector (list task) plan))
  (multiple-value-bind (problem domain) (binarize problem domain)
    @ignorable domain
    (let (tasks tasks-bag)
      (format t "~2&Categorizing PROBLEM ~a with seed ~a" (name problem) seed)
      (setf tasks (abstract-tasks problem seed))
      ;; remove tasks of the trivial component = components of single object
      ;; (setf tasks (remove-if #'trivial-component-p tasks :key #'abstract-component-task-ac))
      ;; remove tasks without goals
      (setf tasks (remove-if-not #'abstract-component-task-goal tasks))
      ;; categorize tasks into buckets, based on init/goal/attribute.
      (setf tasks-bag (categorize-tasks tasks :strict))
      ;; list pf bags. each bag contains tasks of the same structure
      (format t "~&TASKS/g/i/attr : ~a" (mapcar #'length tasks-bag))
      (format t "~&Categorizing TASKS by equality -- calling FD")
      (setf tasks-bag (mappend #'categorize-bag tasks-bag))
      (format t "~&TASKS/plan : ~a" (mapcar #'length tasks-bag))
      ;; list of bags. each bag contains tasks whose plans are interchangeable
      (iter (for bag in tasks-bag)
            ;; assume the cached value of plan-task
            (when-let ((plans-for-a-task (some #'plan-task bag)))
              (collect ; TODO: what if the component-plan does not exists?
                  (vector bag (first plans-for-a-task))))))))

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
  

