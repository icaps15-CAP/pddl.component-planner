
(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

(defun component-factoring-bpvectors (bproblem *problem* domain)
  (declare (ignorable domain))
  (-<>>
      (iter (for seed in (types-in-goal *problem*))
            (appending
             (task-bags *problem* bproblem seed)))
    (sort <> #'> :key #'length)
    (format<> t "~&Categorizing TASKS by plan compatibility.")
    (format<> t "~&Calling the preprocessing planner ~a" *preprocessor*)
    (format<> t "~&Total Tasks /g/i/attr: ~a" (reduce #'+ (mapcar #'length <>)))
    (format<> t "~&Task cardinalities: ~a" (mapcar #'length <>))
    (block nil
      (let ((*print-pretty* t))
        (pprint-logical-block (*standard-output* nil)
          (return
            (mappend #'categorize-by-compatibility <>)))))
    (format<> t "~&Finished the categorization based on plan compatibility.")
    (format<> t "~&TASKS/plan : ~a" (mapcar #'length <>))
    (iter (for bag in <>)
          ;; assume the cached value of plan-task
          (when-let ((plans-for-a-task (some #'plan-task bag)))
            (collect (vector bag (first plans-for-a-task)))))))

(defun types-in-goal (problem)
  (ematch problem
    ((pddl-problem positive-goals)
     (remove-duplicates
      (mapcar #'type (mappend #'parameters positive-goals))))))

;;; structure-level categorization

(defvar *precategorization* t)
(defvar *single-node-components* nil)
(defun task-bags (problem bproblem seed &aux (domain (domain problem)))
  ;; -> (list (vector (list task) plan))
  (declare (ignorable domain))
  (format t "~2&Categorizing PROBLEM ~a with seed ~a" (name bproblem) seed)
  (-<>>
      (if *single-node-components*
          (abstract-tasks-single-node bproblem seed)
          (abstract-tasks-seed-only bproblem seed))
    ;; remove tasks of the trivial component = components of single object
    ;; (remove-if #'trivial-component-p <> :key #'abstract-component-task-ac)
    ;; remove tasks without goals
    (format<> t "~&Tasks found : ~a" (length <>))
    (remove-if-not #'abstract-component-task-goal <>)
    (format<> t "~&Removing tasks w/o goals : ~a" (length <>))
    ;; categorize tasks into buckets, based on init/goal/attribute.
    (if *precategorization*
        (coerce (categorize-tasks <>) 'list)
        (list <>))
    (format<> t "~&TASKS/g/i/attr : ~a" (mapcar #'length <>))
    (format<> t "~&Debinarizing Tasks...")
    (mapcar (curry #'mapcar (curry #'debinarize-task problem)) <>)))

;;; debinarize

(defun debinarize-task (problem task)
  (ematch task
    ((abstract-component-task
      ;; problem
      init goal
      (ac (abstract-component
           seed facts
           components attributes)))
     (make-abstract-component-task
      :problem problem
      ;; NOTE: these facts may contain environment objects
      ;; when they are more than 3 arg predicates.
      :init (debinarize-predicates init)
      :goal (debinarize-predicates goal)
      :ac (make-abstract-component
           :seed seed
           :facts (debinarize-predicates facts)
           :components components
           :attributes attributes)))))

;;; plan-level categorization

(defvar *compatibility* nil)

(defun categorize-by-compatibility (bag)
  (format t "~&Categorizing bag/g/i/attr of length ~a~%" (length bag))
  (coerce (categorize-by-equality
           bag #'maybe-task-plan-equal :transitive t)
          'list))

(defun maybe-task-plan-equal (x y)
  (when *compatibility*
    (multiple-value-bind (result proven?) (task-plan-equal x y)
      (if proven?
          ;; using pprint-newline
          (progn (format t "~:[F~;.~]~:_" result) result)
          (progn (format t "?~:_")
                 (ecase *compatibility*
                   (:strict nil)
                   (:loose t)))))))



;;; generate bmvectors

(defun bmvector (bpvector) ;; bag plan vector
  (ematch bpvector
    ((vector (and bag (list* t1 _)) (pddl-plan actions))
     (handler-bind ((warning #'muffle-warning))
       (handler-case
           (vector bag (ground-macro-action
                        actions (mapcar #'car (mapping-between-tasks t1 t1))))
         (zero-length-plan ()
           (format t "~&ignoring macros of length zero")))))))

