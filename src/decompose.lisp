
(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)
;;; factoring-bmvectors
(defvar *binarization* nil)
(defvar *variable-factoring* nil)
(defun factoring-bmvectors (*problem* domain)
  (let ((bproblem (if *binarization*
                      (progn
                        (format t "~&Binarizing domain ~a" domain)
                        (binarize *problem* domain))
                      *problem*)))
    (-<>>
        (if *variable-factoring*
            (variable-factoring-bpvectors bproblem *problem* domain)
            (component-factoring-bpvectors bproblem *problem* domain))
      (mapcar #'bmvector)
      (remove nil))))

;;;; bmvector -> macro action instances
(defun get-actions (bmvector)
  (ematch bmvector ((vector _ m) (list m))))

(defun get-actions-grounded (bmvector)
  (ematch bmvector
    ((vector (and tasks (list* t1 _))
             (and m (ground-macro-action))) ;; (original . variable)
     (values
      (mapcar
       (lambda (t2)
         (let ((mapping (%remove-nochange (mapping-between-tasks t1 t2)))) ;; (original1 . original2)
           (handler-bind ((unspecified-parameter
                           (lambda (c)
                             ;; FIXME almost ignore it. is it correct?
                             (use-value (parameter c))))
                          (warning #'muffle-warning))
             (change-class
              (map-action m mapping)
              'ground-macro-action
              :problem *problem*
              :parameters nil
              :actions (map 'vector (rcurry #'map-action mapping)
                            (actions m))
              :name (gensym (symbol-name (name m)))
              :alist ;; (iter (for (o1 . o2) in mapping)
                     ;;       (collect (cons o2 (cdr (assoc o1 alist)))))
              (mapping-between-tasks t2 t2)))))
       tasks)
      tasks))))

;;; process macros

(defvar *pddl3.1-multiple-action-costs* nil)
(defvar *cyclic-macros* nil)
(defvar *ground-macros* t)
(defun postprocess-macros (domain problem bmvectors)
  (-> (if *ground-macros*
          (let ((*domain* domain) (*problem* problem))
            (format t "~&Instantiating ~:[forward~;cyclic~] macros." *cyclic-macros*)
            (-<>> bmvectors
              (mappend (if *cyclic-macros*
                           #'cyclic-macro
                           #'get-actions-grounded))
              (mapcar (if *pddl3.1-multiple-action-costs*
                          #'identity
                          #'ground-cost))
              ;; make them appropriate for printing as actions in a domain description
              (mapcar (lambda (x) (change-class x 'macro-action)))))
          (mappend #'get-actions bmvectors))
    (report-macros bmvectors)))

(defun report-macros (macros bmvectors)
  "report the current state of macros"
  (iter (for m in macros)
        (for bmvector in bmvectors)
        (format t "~%(~50@<~a~>:length ~a :first-comp ~a)"
                (name m) (length (actions m))
                (ematch bmvector
                  ((vector (list* (abstract-component-task
                                   (ac
                                    (abstract-component
                                     (components (list* o _))))) _) _)
                   (name o))
                  ;; for factor=variable method
                  ((vector (list* (abstract-component-task
                                   (ac
                                    (abstract-component
                                     (components nil)))) _) _)
                   :no-component))))
  macros)

(defun compute-macros (domain problem filters)
  (let ((comparison-count 0)
        (evaluation-count 0))
    (-<>>
        (handler-bind
            ((comparison-signal (lambda (c) (declare (ignore c)) (incf comparison-count)))
             (evaluation-signal (lambda (c) (declare (ignore c)) (incf evaluation-count))))
          (factoring-bmvectors problem domain))
      (format<> t "~&Forward-macro computation: ~a sec" (elapsed-time))
      (format<> t "~&~a macros found in total." (length <>))
      (format<> t "~&Number of component plan evaluation: ~a" evaluation-count)
      (format<> t "~&Number of comparison: ~a" comparison-count)
      ;; macro filtering
      (funcall (apply #'compose (reverse filters)))
      (format<> t "~&~a macros after filtering." (length <>))
      (postprocess-macros domain problem <>))))

;;; enhance the given problem

(defun enhance-problem (problem
                        &key
                          (filters
                           (list #'remove-null-macros
                                 #'remove-single-macros
                                 ;; #'sort-and-print-macros
                                 ;; #'filter-macros-normdist
                                 #'filter-macros-normalized))
                        &aux (domain (domain problem)))
  (format t "~&Enhancing domain ~a" domain)
  (ematch domain
    ((pddl-domain name)
     (let* ((*domain* (shallow-copy domain :name (symbolicate name '-enhanced)))
            (macros (compute-macros domain problem filters)))
       (format t "~&Cyclic-macro computation: ~a sec" (elapsed-time))
       (appendf (actions *domain*) macros)
       (appendf (constants *domain*) (objects/const problem))
       (let* ((*problem*
               (shallow-copy
                problem
                :name (symbolicate (name problem) '-enhanced)
                :domain *domain*
                :objects nil)))
         (multiple-value-bind (domain problem)
             (if *remove-main-problem-cost*
                 (remove-cost *domain* *problem*)
                 (if *add-macro-cost*
                     (add-macro-cost *domain* *problem*)
                     (values *domain* *problem*)))
           #+nil
           (ematch* (*add-macro-cost* *remove-main-problem-cost*)
             ((_ t)   (remove-cost *domain* *problem*))
             ((t nil) (add-macro-cost *domain* *problem*))
             ((_ _)   (values *domain* *problem*)))
           (values problem domain macros)))))))


;;; enhance and solve problems & domain

(defvar *preprocess-only* nil)
(defvar *validation* nil)
(defvar *main-search* "lama-clean")
(defvar *main-options* "")

(defun solve-problem-enhancing (problem &rest test-problem-args)
  (clear-plan-task-cache)
  (format t "~&Enhancing the problem with macros.")
  (format t "~&Start measuring the maximam preprocessing time ~a at ~a"
          *preprocess-time-limit* *start*)
  (multiple-value-bind (eproblem edomain macros)
      (unwind-protect
          (handler-bind ((unix-signal
                          (lambda (c)
                            (format t "~&Reached the limit during preprocessing")
                            (return-from solve-problem-enhancing))))
            (enhance-problem problem))
        (let ((preprocessing-end (get-universal-time)))
          (format t "~&Preprocessing time: ~a sec"
                  (- preprocessing-end *start*))))
    (format t "~&Enhancement finished on:~%   ~a~%-> ~a"
            (name problem) (name eproblem))
    (format t "~&Solving the enhanced problem with the main planner ~a."
            *main-search*)
    (unless *preprocess-only*
      (when *debug-preprocessing*
        (let ((*package* (find-package :pddl)))
          (print-pddl-object edomain *standard-output*)
          (print-pddl-object eproblem *standard-output*)))
      (let* ((dir (mktemp "enhanced"))
             (*domain* edomain)
             (*problem* eproblem)
             (plans (prog1
                      (handler-bind ((unix-signal
                                      (lambda (c)
                                        (format t "~&main search terminated")
                                        (invoke-restart
                                         (find-restart 'pddl:finish c)))))
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
        (mapcar (lambda (plan i)
                  (terpri)
                  (block nil
                    (pprint-logical-block
                        (*standard-output*
                         nil
                         :per-line-prefix
                         (format nil "Plan ~a " i))
                      (return 
                        (decode-plan-all macros plan)))))
                plans (iota (length plans)))))))

(defun decode-plan-all (macros plan)
  (handler-bind ((warning #'muffle-warning))
    (reduce #'decode-plan macros
            :from-end t
            :initial-value (pddl-plan :path plan))))
