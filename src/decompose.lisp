
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
     (let ((env-objs
            (remove-task-component
             (objects *problem*) (first bag))))
       (vector bag (macro-action actions env-objs)))))) ; macro-action might return nil

(defun remove-task-component (objs task)
  (iter (for o in objs)
        (when (filter-object
               (make-instance 'filtering-strategy) o
               :components (abstract-component-components
                            (abstract-component-task-ac task)))
          (collect o))))

;;;; score, sort and filter macros

(defun get-action (x)
  (match x ((vector _ m) m)))

(defun remove-null-macros (pairs)
  (format t "~&Filtering null macros.")
  (remove-if (lambda-match ((vector _ nil) t)) pairs))

;;;;; normalized score

(defun basedata (pair)
  (ematch pair
    ((vector (and bag (list* (abstract-component-task-
                              (ac (abstract-component components))) _))
             (macro-action actions))
     (vector (length bag)
             (length components)
             (length actions)))))


(defun map-column (data fn index)
  (flet ((v (datum) (aref datum index)))
    (funcall fn (map 'vector #'v data))))

(defun norminfo (data)
  (iter (for i below 3)
        (collect
            (vector (map-column data #'mean i)
                    (map-column data #'standard-deviation i)))))
(defun normalize (mean stdev x)
  (/ (- x mean) (if (zerop stdev) 1 stdev)))
(defun normalize-data (data)
  (let ((norminfo (norminfo data)))
    (map 'vector
         (lambda (datum)
           (iter (for x in-vector datum)
                 (for v in norminfo)
                 (ematch v
                   ((vector mean stdev)
                    (collect (normalize mean stdev x)
                      result-type vector)))))
         data)))

(defun ^2 (x) (* x x))

(defvar *weights* (vector 1.0 0.5 0.1))
(defun ms-diff-score (d)
  (- (mean d) (standard-deviation d)))
(defun ms-rate-score (d)
  (let ((stdev (standard-deviation d)))
    (if (zerop stdev)
        (if (plusp (reduce #'+ d))
            MOST-POSITIVE-SINGLE-FLOAT
            MOST-NEGATIVE-SINGLE-FLOAT)
        (/ (mean d) stdev))))

(defun score (datum)
  (ms-diff-score
   (map 'vector #'* datum *weights*)))

(defun filter-macros-normalized (pairs)
  (when pairs
    (let* ((data (map 'vector #'basedata pairs))
           (ndata (normalize-data data))
           (scores (map 'vector #'score ndata))
           (threshold (+ (mean scores)
                         (* (standard-deviation scores) *threshold*)))
           (all (iter (for pair in pairs)
                      (for datum in-vector data)
                      (for ndatum in-vector ndata)
                      (for score in-vector scores)
                      (collect (list pair datum ndatum score))))
           (all (sort all #'> :key #'fourth))
           (results (remove-if-not
                     (curry #'< threshold) all
                     :key #'fourth)))
      (print-results all threshold)
      (format t "~&Pruning threshold is ~a." threshold)
      (when (< (length results) 2)
        (format t "~&This threshold value prunes too many macros. ~
                     Recovering at least 2.")
        (setf results (subseq all 0 (min 2 (length all)))))
      (format t "~&~a macros are filtered down to ~a."
              (length pairs) (length results))
      (mapcar #'first results))))

(defvar *sep*
    "----------------------------------------------------------------")
(defun print-results (results th)
  (format t "~&~a macros, status:" (length results))
  (when results
    (iter (for r in results)
          (for s = (print-result r))
          (for ps previous s)
          (when (and ps (< s th ps))
            (format t "~&~a" *sep*)))))
(defun print-result (result)
  (ematch result
    ((list (vector (list* (abstract-component-task-
                           (ac (abstract-component seed))) _) _)
           (vector bag components actions)
           _ ; (vector nbag ncomponents nactions)
           score)
     (format t "~&(:tasks ~a :objs ~a :macro-length ~a ~
                   :score ~,4f :seed ~a)"
             ;; :tasks ~,4f :objs ~,4f :macro-length ~,4f ~
             bag components actions
             ;; nbag ncomponents nactions
             score
             (name (type seed)))
     score)))

;;;;; old implementation of fixed and normdist filtering

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
                              (ac (abstract-component seed components))) _))
             (macro-action actions))
     (format t "~&(:tasks ~a :objs ~a :macro-length ~a :score ~a :seed ~a)"
             (length bag) (length components) (length actions)
             (score-pair pair)
             (name (type seed))))))

(defun filter-macros-fixed (pairs)
  (when (< 2 (length pairs))
    (format t "~&~a macros are filtered down to 2 (fixed number)."
            (length pairs)))
  (subseq pairs 0 (min 2 (length pairs))))

(defvar *threshold* (z 0.8))

(defun sort-and-print-macros (pairs)
  (format t "~&~a macros, status:" (length pairs))
  (when pairs
    (setf pairs (sort pairs #'> :key #'score-pair))
    (mapc #'print-pair-status pairs)
    pairs))

(defun filter-macros-normdist (pairs)
  (when pairs
    (let* ((scores (map 'vector #'score-pair pairs))
           (threshold (+ (mean scores)
                         (* (standard-deviation scores) *threshold*)))
           (results (remove-if-not (curry #'< threshold) pairs
                                   :key #'score-pair)))
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
                                         ;; #'sort-and-print-macros
                                         ;; #'filter-macros-normdist
                                         #'filter-macros-normalized
                                         ))
                          (modify-domain-problem #'identity2)
                        &aux (domain (domain problem)))
  (format t "~&Binarizing domain ~a" domain)
  (multiple-value-bind (problem domain) (binarize problem domain)
    (format t "~&Enhancing domain ~a" domain)
    (ematch domain
      ((pddl-domain name)
       (let* ((*domain*
               (shallow-copy domain :name (symbolicate name '-enhanced)))
              macro-pairs macros)
         (setf macro-pairs
               (let ((*problem* problem))
                 (iter (for seed in (types-in-goal problem))
                       (appending (component-macro problem seed)))))
         (format t "~&~a macros found in total." (length macro-pairs))
         (setf macro-pairs
               (funcall (apply #'compose (reverse filters)) macro-pairs))
         (format t "~&~a macros after filtering." (length macro-pairs))
         (setf macros (mapcar #'get-action macro-pairs))
         (appendf (actions *domain*) macros)
         (let ((consts (remove-duplicates (mappend #'constants macros)
                                          :test #'eqname)))
           (format t "~& ~a constants added" (length consts))
           (unionf (constants *domain*) consts :test #'eqname))
         (let* ((*problem*
                 (shallow-copy
                  problem
                  :name (symbolicate (name problem) '-enhanced)
                  :domain *domain*
                  :objects
                  (set-difference
                   (set-difference
                    (objects problem)
                    (remove-duplicates
                     (mappend #'originals macros)
                     :test #'eqname)
                    :test #'eqname)
                   (constants *domain*)
                   :test #'eqname))))
           (multiple-value-bind (domain problem)
               (funcall modify-domain-problem *domain* *problem*)
             (values problem domain macros))))))))

(defun types-in-goal (problem)
  (ematch problem
    ((pddl-problem positive-goals)
     (remove-duplicates
      (mapcar #'type (mappend #'parameters positive-goals))))))

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
  

