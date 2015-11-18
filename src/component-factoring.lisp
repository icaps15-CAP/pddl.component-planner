
(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

(defvar *iterative-resource* nil)
(defun component-factoring-bpvectors (problem)
  (-<>>
      (iter (for seed in (types-in-goal problem))
            (appending
             (task-bags problem seed)))
    (sort <> #'> :key #'length)
    (format<> t "~&Categorizing TASKS by plan compatibility.")
    (format<> t "~&Calling the preprocessing planner ~a" *preprocessor*)
    (format<> t "~&Total Tasks /g/i/attr: ~a" (reduce #'+ (mapcar #'length <>)))
    (format<> t "~&Task cardinalities: ~a" (mapcar #'length <>))
    (if *iterative-resource*
        (progn (format t "~&Iterative resource mode, compatibility option is ignored!")
               (mappend (lambda (list)
                          (mapcar #'list list)) <>))
        (block nil
          (let ((*print-pretty* t))
            (pprint-logical-block (*standard-output* nil)
              (return
                (mappend #'categorize-by-compatibility <>))))))
    ;; list of lists of tasks
    (format<> t "~&Finished the categorization based on plan compatibility.")
    (format<> t "~&TASKS/plan : ~a" (mapcar #'length <>))
    (if *iterative-resource*
        (let (#+nil
              (max-component-time-limit *component-plan-time-limit*)
              (results (make-array (length <>) :initial-element nil)))
          ;; we know that each bag is just a list of 1 element
          (iter (while (within-time-limit))
                (until (every #'identity results))
                (for i from 0)
                (for *component-plan-time-limit* = (expt 2 i))
                #+nil (while (< *component-plan-time-limit* max-component-time-limit))
                (format t "~&Start iteration with time limit ~a!" *component-plan-time-limit*)
                (iter (for bag in <>)
                      (for j from 0)
                      (assert (= 1 (length bag)))
                      (unless (aref results j)
                        (handler-case
                            (setf (aref results j) (vector bag (first (plan-task (first bag)))))
                          (plan-not-found ()
                            (format t "~&Failed with time limit ~a!" *component-plan-time-limit*))))))
          (remove-if (lambda (x)
                       (ematch x
                         (nil t)
                         ((vector _ nil) t)
                         ((vector _ _) nil)))
                     (coerce results 'list)))
        (iter (for bag in <>)
              ;; assume the cached value of plan-task
              (when-let ((plans-for-a-task (some #'plan-task bag)))
                (collect (vector bag (first plans-for-a-task))))))))

(defun types-in-goal (problem)
  (ematch problem
    ((pddl-problem positive-goals)
     (remove-duplicates
      (mapcar #'type (mappend #'parameters positive-goals))))))

;;; structure-level categorization

(defvar *precategorization* t)
(defvar *component-abstraction* nil)
(defun task-bags (problem seed)
  (format t "~2&Categorizing PROBLEM ~a with seed ~a" (name problem) seed)
  (-<>>
      (let ((bproblem (if *binarization*
                          (progn
                            (format t "~&Binarizing ~a" problem)
                            (binarize problem (domain problem)))
                          problem)))
        (-<>>
            (if *component-abstraction*
                (abstract-tasks-seed-only bproblem seed)
                (abstract-tasks-single-node bproblem seed))
          (format<> t "~&Debinarizing Tasks...")
          (mapcar (curry #'debinarize-task problem))))
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
    (format<> t "~&TASKS/g/i/attr : ~a" (mapcar #'length <>))))

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
    ((vector bag (pddl-plan actions))
     (handler-bind ((warning #'muffle-warning))
       (vector bag (ground-macro-action actions))))))


