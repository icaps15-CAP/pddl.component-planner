
(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

(defvar *iterative-resource* nil)
(defun component-factoring-bpvectors (problem)
  (-<>>
      (types-in-goal problem)
    (let ((types <>))
      (format t "~&types in goal: ~a" types)
      types)
    (iter (for seed in <>)
      (appending
       (task-bags problem seed)))
    (sort <> #'> :key #'length)
    (let ((bags <>))
      (format t "~&Categorizing TASKS by plan compatibility.")
      (format t "~&Calling the preprocessing planner ~a" *preprocessor*)
      (format t "~&Total Tasks /g/i/attr: ~a" (reduce #'+ (mapcar #'length bags)))
      (format t "~&Task cardinalities: ~a" (mapcar #'length bags))
      (if *iterative-resource*
          (progn (format t "~&Iterative resource mode, compatibility option is ignored!")
                 (mappend (lambda (list)
                            (mapcar #'list list))
                          bags))
          (block nil
            (let ((*print-pretty* t))
              (pprint-logical-block (*standard-output* nil)
                (return
                  (mappend #'categorize-by-compatibility bags)))))))
    ;; list of lists of tasks
    (let ((bags <>))
      (format t "~&Finished the categorization based on plan compatibility.")
      (format t "~&TASKS/plan : ~a" (mapcar #'length bags))
      (unwind-protect
           (progn
             (setf *kernel* (make-kernel (if *rely-on-cfs* (length bags) *num-threads*)))
             (cond
               (*iterative-resource*
                (assert (every (lambda (bag) (= 1 (length bag))) bags))
                (let ((initial-time-limit 1))
                  ;; we know that each bag is just a list of 1 element
                  (labels ((rec (bag limit)
                             (handler-case
                                 (vector bag
                                         (let ((*component-plan-time-limit* limit))
                                           (first (plan-task (first bag)))))
                               (plan-not-found ()
                                 (format t "~&Failed with time limit ~a" limit)
                                 (lambda (max-component-time-limit)
                                   (let ((limit (* 2 limit)))
                                     (if (< limit max-component-time-limit)
                                         (future (rec bag limit))
                                         (format t "~&Iteration stopped: ~a = ~a"
                                                 `(< limit max-component-time-limit)
                                                 `(< ,limit ,max-component-time-limit)))))))))
                    (iter (for futures =
                               (mapcar #'force
                                       (if (first-time-p)
                                           (mapcar (lambda (bag) (future (rec bag initial-time-limit))) bags)
                                           futures)))
                      (for count = (count-if #'functionp futures))
                      (if (zerop count)
                          (return
                            (remove-if (lambda (x)
                                         (ematch x
                                           (nil t)
                                           ((vector _ nil) t)
                                           ((vector _ _) nil)))
                                       futures))
                          (let ((new-max (* *num-threads* (/ *preprocess-time-limit* count))))
                            (setf futures
                                  (mapcar (lambda (res)
                                            (ematch res
                                              ((type vector) res)
                                              ((type function) (funcall res new-max))))
                                          futures))))))))
               (t ; use lparallel even under 1 thread
                (remove nil
                        (mapcar #'force
                                (mapcar (lambda (bag)
                                          (future
                                            (when-let ((plans-for-a-task (some #'plan-task bag)))
                                              (vector bag (first plans-for-a-task)))))
                                        (shuffle bags)))))))
        (when (check-kernel)
          (end-kernel))))))


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
          (let ((tasks <>))
            (format t "~&Debinarizing Tasks...")
            (mapcar (curry #'debinarize-task problem) tasks))))
    ;; remove tasks of the trivial component = components of single object
    ;; (remove-if #'trivial-component-p <> :key #'abstract-component-task-ac)
    ;; remove tasks without goals
    (let ((tasks <>))
      (format t "~&Tasks found : ~a" (length tasks))
      (remove-if-not #'goal tasks))
    (let ((tasks <>))
      (format t "~&Removing tasks w/o goals : ~a" (length tasks))
      ;; categorize tasks into buckets, based on init/goal/attribute.
      (if *precategorization*
          (coerce (categorize-tasks tasks) 'list)
          (list tasks)))
    (let ((tasks <>))
      (format t "~&TASKS/g/i/attr : ~a" (mapcar #'length tasks))
      tasks)))

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


