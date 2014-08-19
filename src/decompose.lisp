
(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

;;; enhance domain and problem
;;;; categorize components

(defun trivial-component-p (ac)
  (ematch ac
    ((abstract-component components)
     (= 1 (length components)))))

(defun categorize-bag (bag)
  (coerce (categorize-by-equality bag #'maybe-task-plan-equal :transitive t)
          'list))

(defun maybe-task-plan-equal (x y)
  (multiple-value-bind (result proven?) (task-plan-equal x y)
    (if proven?
        (progn (format t "~&Compatibility~@[ negatively~] proven" (not result)) result)
        (progn (format t "~&Compatibility not proven, assuming true") t))))

(defun component-plans (problem seed &aux (domain (domain problem)))
  ;; -> (list (vector (list task) plan))
  (multiple-value-bind (problem domain) (binarize problem domain)
    @ignorable domain
    (let (tasks tasks-bag)
      (format t "~2&Categorizing PROBLEM ~a with seed ~a" (name problem) seed)
      (setf tasks (abstract-tasks-seed-only problem seed))
      ;; remove tasks of the trivial component = components of single object
      ;; (setf tasks (remove-if #'trivial-component-p tasks :key #'abstract-component-task-ac))
      ;; remove tasks without goals
      (format t "~&Tasks found : ~a" (length tasks))
      (format t "~&Removing tasks w/o goals.")
      (setf tasks (remove-if-not #'abstract-component-task-goal tasks))
      (format t "~&Tasks : ~a" (length tasks))
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

;;;; creates macros from plans

(defun component-macro (problem seed &aux (domain (domain problem)))
  (multiple-value-bind (problem *domain*) (binarize problem domain)
    (mapcar #'component-macro/bucket
            (component-plans problem seed))))

(defun component-macro/bucket (v)
  (ematch v
    ((vector bag (pddl-plan actions))
     (vector bag (macro-action actions))))) ; macro-action might return nil

;;;; score, sort and filter macros

(defun get-action (x)
  (match x ((vector _ m) m)))

(defun remove-null-macros (pairs)
  (format t "~&Filtering null macros.")
  (remove-if (lambda-match ((vector _ nil) t)) pairs))

(defun score-pair (pair)
  (ematch pair
    ((vector (and bag (list* (abstract-component-task-
                              (ac (abstract-component components))) _))
             (macro-action actions))
     (let ((sample (vector (length bag)
                           (length components)
                           (length actions))))
       (- (mean sample)
          (standard-deviation sample))))))

(defun print-pair-status (pair)
  (ematch pair
    ((vector (and bag (list* (abstract-component-task-
                              (ac (abstract-component components))) _))
             (macro-action actions))
     (format t "~&(:tasks ~a :objs ~a :macro-length ~a :score ~a)"
             (length bag) (length components) (length actions)
             (score-pair pair)))))

(defun sort-and-filter-macros (pairs)
  (when (< 2 (length pairs))
    (format t "~&~a macros are filtered down to 2 (fixed number)." (length pairs)))
  (setf pairs
        (sort pairs #'> :key #'score-pair))
  (format t "~&Macro status:")
  (mapc #'print-pair-status pairs)
  (subseq pairs 0 (min 2 (length pairs))))

(defvar *threshold* (z 0.8))

(defun filter-macros-normdist (pairs)
  (format t "~&~a macros, status:" (length pairs))
  (when pairs
    (setf pairs (sort pairs #'> :key #'score-pair))
    (mapc #'print-pair-status pairs)
    (let* ((scores (map 'vector #'score-pair pairs))
           (threshold (+ (mean scores) (* (standard-deviation scores) *threshold*)))
           (results (remove-if (lambda (pair) (< (score-pair pair) threshold)) pairs)))
      (format t "~&Pruning threshold is ~a." threshold)
      (when (< (length results) 2)
        (format t "~&This threshold value prunes too many macros. Recovering at least 2.")
        (setf results (subseq pairs 0 (min 2 (length pairs)))))
      (format t "~&~a macros are filtered down to ~a." (length pairs) (length results))
      results)))

;;;; enhance the given problem

(defun identity2 (x y) (values x y))

(defun enhance-problem (problem
                        &key
                          (filters (list #'remove-null-macros
                                         ;; #'sort-and-filter-macros
                                         #'filter-macros-normdist
                                         ))
                          (modify-domain-problem #'identity2)
                        &aux (domain (domain problem)))
  (format t "~&Binarizing domain ~a" domain)
  (multiple-value-bind (problem domain) (binarize problem domain)
    (format t "~&Enhancing domain ~a" domain)
    (ematch domain
      ((pddl-domain name actions)
       (let* ((*domain*
               (shallow-copy domain :name (symbolicate name '-enhanced)))
              macro-pairs macros)
         (setf macro-pairs
               (iter (for seed in (types-in-goal problem))
                     (appending (component-macro problem seed))))
         (format t "~&~a macros found in total." (length macro-pairs))
         (setf macro-pairs
               (funcall (apply #'compose (reverse filters)) macro-pairs))
         (format t "~&~a macros after filtering." (length macro-pairs))
         (setf macros (mapcar #'get-action macro-pairs))
         (setf (actions *domain*) (append actions macros))
         (let* ((*problem* (shallow-copy problem
                                         :name (symbolicate (name problem) '-enhanced)
                                         :domain *domain*)))
           (multiple-value-bind (*domain* *problem*)
               (funcall modify-domain-problem *domain* *problem*)
             (values *problem* *domain* macros))))))))

(defun types-in-goal (problem)
  (ematch problem
    ((pddl-problem positive-goals)
     (remove-duplicates (mapcar #'type (mappend #'parameters positive-goals))))))

;;; enhance and solve problems & domain

(defun decode-plan-all (macros plan)
  (reduce #'decode-plan macros
          :from-end t
          :initial-value (pddl-plan :path plan)))

(defun enhancement-method (problem)
  (enhance-problem problem))

(defun solve-problem-enhancing (problem &rest test-problem-args)
  (clear-plan-task-cache)
  (format t "~&Enhancing the problem with macros.")
  (multiple-value-bind (eproblem edomain macros)
      (time
       (enhancement-method problem))
    (format t "~&Enhancement finished on:~%   ~a~%-> ~a"
            (name problem) (name eproblem))
    (format t "~&Solving the enhanced problem with FD.")
    (let* ((dir (mktemp "enhanced"))
           (*domain* edomain)
           (*problem* eproblem)
           (plans (prog1
                      (handler-bind ((unix-signal
                                      (lambda (c)
                                        (invoke-restart
                                         (find-restart 'finish c)))))
                        (apply #'test-problem
                               (write-pddl *problem* "eproblem.pddl" dir)
                               (write-pddl *domain* "edomain.pddl" dir)
                               test-problem-args))
                    (format t "~&Decoding the result plan.")))
           (plans (mapcar (curry #'decode-plan-all macros) plans)))
      (iter (for plan in plans)
            (collect
                (debinarize-plan
                 (domain problem) problem
                 edomain eproblem plan))))))

;;;; debinarize the result

(define-local-function debinarize-action (ga)
  (let ((a (find ga (actions bdomain) :test #'eqname)))
    (ematch a
      ((binarized-action binarization-origin)
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
  

