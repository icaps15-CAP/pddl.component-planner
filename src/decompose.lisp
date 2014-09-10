
(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

;;; enhance domain and problem
;;;; binarize, extract and debinarize components

(defun tasks-bag/aig/seed (problem bproblem seed  &aux (domain (domain problem)))
  ;; -> (list (vector (list task) plan))
  (let (tasks tasks-bag)
    (format t "~2&Categorizing PROBLEM ~a with seed ~a" (name bproblem) seed)
    (setf tasks (abstract-tasks-seed-only bproblem seed))
    ;; remove tasks of the trivial component = components of single object
    ;; (setf tasks (remove-if #'trivial-component-p tasks :key #'abstract-component-task-ac))
    ;; remove tasks without goals
    (format t "~&Tasks found : ~a" (length tasks))
    (format t "~&Removing tasks w/o goals.")
    (setf tasks (remove-if-not #'abstract-component-task-goal tasks))
    (format t "~&Tasks : ~a" (length tasks))
    ;; categorize tasks into buckets, based on init/goal/attribute.
    (setf tasks-bag (coerce (categorize-tasks tasks) 'list))
    ;; list pf bags. each bag contains tasks of the same structure
    (format t "~&TASKS/g/i/attr : ~a" (mapcar #'length tasks-bag))
    (format t "~&Debinarizing Tasks...")
    (mapcar (curry #'mapcar (curry #'debinarize-task problem))
            tasks-bag)))

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

(defun debinarize-predicates (predicates)
  (remove-duplicates
   (mapcar (lambda (g)
             (match g
               ((binarized-predicate binarization-origin)
                binarization-origin)
               ((pddl-predicate) g)))
           predicates)
   :test #'eqstate))
  
          

;; task: ac, init, goal
;; ac: seed, objects, facts, attr, attr-facts

;;;; compute component-plans

(let ((i 0))
(defun categorize-by-compatibility (bag)
  (format t "~&Categorizing bag/g/i/attr of length ~a" (length bag))
  (coerce (categorize-by-equality
           bag #'maybe-task-plan-equal :transitive t)
          'list))

(defun maybe-task-plan-equal (x y)
  (multiple-value-bind (result proven?) (task-plan-equal x y)
    (incf i) (when (< 60 i) (setf i 0) (terpri))
    (if proven?
        (progn (format t "~:[F~;.~]" result) result)
        (progn (format t "?") nil))))
)

(defun component-plans (tasks-bag)
  (setf tasks-bag (sort tasks-bag #'> :key #'length)) ;; sort by c_i
  (format t "~&Categorizing TASKS by plan compatibility")
  (format t "~&Calling the preprocessing planner ~a" *preprocessor*)
  (format t "~&Total Tasks /g/i/attr: ~a" (mapcar #'length tasks-bag))
  (setf tasks-bag (mappend #'categorize-by-compatibility tasks-bag))
  (format t "~&Finished the categorization based on plan compatibility.")
  (format t "~&TASKS/plan : ~a" (mapcar #'length tasks-bag))
  ;; list of bags. each bag contains tasks whose plans are interchangeable
  (iter (for bag in tasks-bag)
        ;; assume the cached value of plan-task
        (when-let ((plans-for-a-task (some #'plan-task bag)))
          (collect ; TODO: what if the component-plan does not exists?
              (vector bag (first plans-for-a-task))))))

;;;; creates macros from the obtained component-plans

(defun component-macro/bpvector (v)
  (ematch v
    ((vector bag (pddl-plan actions))
     (let ((env-objs
            (environment-objects
             (objects *problem*) (first bag))))
       (vector bag (macro-action actions env-objs)))))) ; macro-action might return nil

(defun environment-objects (objs task)
  (iter (for o in objs)
        (when (filter-object
               (make-instance 'filtering-strategy) o
               :components (abstract-component-components
                            (abstract-component-task-ac task)))
          (collect o))))



;;;; generate-macro-pairs

(defun generate-macro-pairs (*problem* domain)
  (format t "~&Binarizing domain ~a" domain)
  (let* ((bproblem (binarize *problem* domain))
         (tasks-bag (iter (for seed in (types-in-goal *problem*))
                          (appending
                           (tasks-bag/aig/seed
                            *problem* bproblem seed)))))
    (mapcar #'component-macro/bpvector
            (component-plans tasks-bag))))

;;;; score, sort and filter macros

;; no grounding
(defun get-actions (bpvector)
  (match bpvector ((vector _ m) (list m))))
(defun get-actions-grounded (bpvector)
  (match bpvector
    ((vector (and tasks (list* (abstract-component-task
                                (ac (abstract-component
                                     (components components1)))) _))
             (and m (macro-action :alist alist)))
     (values
      (mapcar
       (lambda-match
         ((abstract-component-task
           (ac (abstract-component components)))
          (labels ((ground-p (param)
                     (handler-bind ((error (lambda (c)
                                             (declare (ignorable c))
                                             (let ((*standard-output* *error-output*))
                                               (break+ components1
                                                       components
                                                       param
                                                       (rassoc param alist)
                                                       alist)))))
                       (ematch (rassoc param alist)
                         ((cons _ (and enh (type pddl-constant)))
                          enh)
                         ((cons (and org (type pddl-constant)) (and (type pddl-variable)))
                          org)
                         ((cons (and org (type pddl-object)) (and (type pddl-variable)))
                          (elt components (position org components1))))))
                   (ground-a (a)
                     (handler-bind ((warning #'muffle-warning)) 
                       (ground-action
                        a (mapcar #'ground-p (parameters a))))))
            (change-class
             (ground-a m)
             'macro-action
             :parameters nil
             :actions (mapcar #'ground-a (actions m))
             :name (gensym (symbol-name (name m)))
             :alist (mapcar (lambda-match
                              ;; of (object . variable) or (constant
                              ;; . variable) if objects are not in the
                              ;; ignore list, and (object . constant) or
                              ;; (constant . constant) if they are in the
                              ;; ignore list.
                              ((cons (and const (type pddl-constant)) enh)
                               (cons const enh))
                              ((cons (and obj (type pddl-object)) enh)
                               (if-let ((pos (position obj components1)))
                                 (cons (elt components pos) enh)
                                 (cons obj enh))))
                            alist)))))
       ;; :alist do not matter now;
       tasks)
      tasks))))

(defun remove-null-macros (pairs)
  (format t "~&~40@<Filtering null macros.~>")
  (let ((filtered (remove-if (lambda-match ((vector _ nil) t)) pairs)))
    (format t "... ~a remaining." (length filtered))
    filtered))
(defun remove-single-macros (pairs)
  (format t "~&~40@<Filtering macros with length 1.~>")
  (let ((filtered (remove-if (lambda-match ((vector _ (macro-action actions))
                                            (= 1 (length actions))))
                             pairs)))
    (format t "... ~a remaining." (length filtered))
    filtered))

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

(defun sum-score (d) (reduce #'+ d))
(defun score (datum)
  (sum-score
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
                   ~&Recovering at least 2.")
        (setf results (subseq all 0 (min 2 (length all)))))
      (format t "~&~40@<~>... ~a remaining." (length results))
      (mapcar #'first results))))

(defvar *sep*
    "----------------------------------------------------------------")
(defun print-results (results th)
  (format t "~&~a macros, status:" (length results))
  (when results
    (iter (for r in results)
          (for s = (get-score r))
          (for ps previous s)
          (when (and ps (< s th ps))
            (format t "~&~a" *sep*))
          (print-result r))))
(defun get-score (result)
  (ematch result
    ((list _ _ _ score) score)))
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
             (name (type seed))))))

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

;; This function is not used anymore since the action name is made by
;; gensym now
;; (defun check-macro-sanity (macros)
;;   ;; ensure the name of the macros are unique
;;   ;; (mapc #'print (mapcar #'name macros))
;;   (prog1 (iter (for (name mm) in-hashtable (categorize macros :test #'eq :key #'name))
;;                (when (<= 2 (length mm))
;;                  (iter (for m in mm)
;;                        (for i from 0)
;;                        (setf (name m) (symbolicate name '_ (princ-to-string i)))))
;;                (appending mm))
;;          (mapc #'print (mapcar #'name macros))))


(defvar *disable-filtering* nil)
(defvar *use-grounded-macros* nil)
(defvar *use-reverse-macros* nil)
(defvar *use-grounded-reverse-macros* nil)

(defun enhance-problem (problem
                        &key
                          (filters
                           (list #'remove-null-macros
                                 #'remove-single-macros
                                 ;; #'sort-and-print-macros
                                 ;; #'filter-macros-normdist
                                 (if *disable-filtering*
                                     (lambda (x)
                                       (format t "~&Rank-based filtering is disabled.") x)
                                     #'filter-macros-normalized)))
                          (modify-domain-problem #'identity2)
                        &aux (domain (domain problem)))
  (format t "~&Enhancing domain ~a" domain)
  (ematch domain
    ((pddl-domain name)
     (let* ((*domain*
             (shallow-copy domain :name (symbolicate name '-enhanced)))
            macro-pairs macros)
       (setf macro-pairs (generate-macro-pairs problem domain))
       (format t "~&~a macros found in total." (length macro-pairs))
       (setf macro-pairs
             (funcall (apply #'compose (reverse filters)) macro-pairs))
       (format t "~&~a macros after filtering." (length macro-pairs))
       (format t "~&Instantiating cyclic macro.")
       (let ((*domain* domain) (*problem* problem))
         (setf macros (mappend #'cyclic-macro macro-pairs)))
       (iter (for m in macros)
             (format t "~%(~50@<~a~>:length ~a)"
                     (name m) (length (actions m))))
       (appendf (actions *domain*) macros)
       (appendf (constants *domain*) (objects/const problem))
       (let* ((*problem*
               (shallow-copy
                problem
                :name (symbolicate (name problem) '-enhanced)
                :domain *domain*
                :objects nil)))
         (multiple-value-bind (domain problem)
             (funcall modify-domain-problem *domain* *problem*)
           (values problem domain macros)))))))

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

(defvar *preprocess-only* nil)
(defvar *validation* nil)
(defvar *main-search* "fd-clean")
(defvar *main-options* *lama-options*)

(defun solve-problem-enhancing (problem &rest test-problem-args)
  (clear-plan-task-cache)
  (format t "~&Enhancing the problem with macros.")
  (let ((*start* (get-universal-time)))
    (format t "~&Start measuring the maximam preprocessing time ~a."
            *preprocess-time-limit*)
    (multiple-value-bind (eproblem edomain macros)
        (time
         (enhancement-method problem))
      (format t "~&Enhancement finished on:~%   ~a~%-> ~a"
              (name problem) (name eproblem))
      (format t "~&Solving the enhanced problem with the main planner ~a."
              *main-search*)
      (unless *preprocess-only*
        (let* ((dir (mktemp "enhanced"))
               (*domain* edomain)
               (*problem* eproblem)
               (plans (prog1
                        (handler-bind ((unix-signal
                                        (lambda (c)
                                          (invoke-restart
                                           (find-restart 'finish c)))))
                          (apply #'test-problem-common
                                 (write-pddl *problem* "eproblem.pddl" dir)
                                 (write-pddl *domain* "edomain.pddl" dir)
                                 test-problem-args)))))
          (when *validation*
            (dolist (p plans)
              (validate-plan (pathname (format nil "~a/edomain.pddl" dir))
                             (pathname (format nil "~a/eproblem.pddl" dir)) p
                             :verbose t)))
          (format t "~&~a plans found, decoding the result plan." (length plans))
          (mapcar (curry #'decode-plan-all macros) plans))))))

;; in order to set (domain/problem plan)
;; during the initialization
  

