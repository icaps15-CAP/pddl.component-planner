
(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)
;;; factoring-bmvectors

(defvar *variable-factoring* nil)
(defun factoring-bmvectors (problem domain)
  (-<>>
      (if *variable-factoring*
          (variable-factoring-bpvectors problem)
          (component-factoring-bpvectors problem))
    (mapcar (lambda (bpvector)
              (handler-case
                  (bmvector bpvector)
                (zero-length-plan ()
                  (format t "~&ignoring macros of length zero")))))
    (remove nil)))

;;;; bmvector -> macro action instances
(defun get-actions (bmvector)
  (ematch bmvector
    ((vector _ (and m (ground-macro-action)))
     (handler-bind ((parameter-not-found
                       (lambda (c) (invoke-restart 'lift c))))
       (list (lift-action m))))))

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
             (let* ((*problem* (abstract-component-task-problem t1))
                    (*domain* (domain *problem*)))
               (change-class
                (map-action m mapping)
                'ground-macro-action
                :problem *problem*
                :parameters nil
                :actions (map 'vector (lambda (a) (map-action a mapping))
                              (actions m))
                :name (gensym (symbol-name (name m)))
                :alist ;; (iter (for (o1 . o2) in mapping)
                ;;       (collect (cons o2 (cdr (assoc o1 alist)))))
                (mapping-between-tasks t2 t2))))))
       tasks)
      tasks))))

;;; process macros

(defvar *pddl3.1-multiple-action-costs* nil)
(defvar *cyclic-macros* nil)
(defvar *ground-macros* t)
(defun postprocess-macros (domain problem bmvectors)
  (format t "~&Instantiating ~:[forward~;cyclic~] macros." *cyclic-macros*)
  (-> (if *ground-macros*
          (-<>> bmvectors
            (mappend (if *cyclic-macros*
                         #'cyclic-macro
                         #'get-actions-grounded))
            (mapcar (if *pddl3.1-multiple-action-costs*
                        #'identity
                        #'ground-cost))
            ;; make them appropriate for printing as actions in a domain description
            (mapcar (lambda (x) (change-class x 'macro-action))))
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
    ((pddl-domain name actions constants predicates)
     (let* ((macros (compute-macros domain problem filters))
            (edomain (shallow-copy domain
                                   :name (symbolicate name '-enhanced)
                                   :actions (append actions macros)
                                   :constants (append constants (objects/const problem))
                                   :predicates
                                   (if *ground-macros*
                                       predicates
                                       (cons (pddl-predicate
                                              :domain domain
                                              :name 'equal
                                              :parameters (list (pddl-variable :domain domain :name '?p1)
                                                                (pddl-variable :domain domain :name '?p2)))
                                             predicates))))
            (eproblem (shallow-copy problem
                                    :name (symbolicate (name problem) '-enhanced)
                                    :domain edomain
                                    :objects nil
                                    :init
                                    (if *ground-macros*
                                        (init problem)
                                        (append (mapcar (lambda (o)
                                                          (pddl-atomic-state
                                                           :domain domain
                                                           :problem problem
                                                           :name 'equal
                                                           :parameters (list o o)))
                                                        (objects problem))
                                                (init problem))))))
       (format t "~&Max_number_of_parameters: ~a"
               (reduce #'max (mapcar (lambda (a) (length (parameters a)))
                                     (actions edomain))))
       (format t "~&Cyclic-macro computation: ~a sec" (elapsed-time))
       (values eproblem edomain macros)))))

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
                            (format t "~&Received signal ~a: Reached the limit during preprocessing"
                                    (signo c))
                            (return-from solve-problem-enhancing))))
            (enhance-problem problem))
        (let ((preprocessing-end (get-universal-time)))
          (format t "~&Preprocessing time: ~a sec"
                  (- preprocessing-end *start*))))
    (format t "~&Enhancement finished on:~%   ~a~%-> ~a" (name problem) (name eproblem))
    (format t "~&Solving the enhanced problem with the main planner ~a." *main-search*)
    (when *preprocess-only* (return-from solve-problem-enhancing))
    (multiple-value-bind (edomain-mod eproblem-mod macros-mod)
        (if *remove-main-problem-cost*
            (remove-cost edomain eproblem macros)
            (if *add-macro-cost*
                (add-macro-cost edomain eproblem macros)
                (values edomain eproblem macros)))
      (when *debug-preprocessing*
        (let ((*package* (find-package :pddl)))
          (terpri)
          (print-pddl-object edomain *standard-output*)
          (print-pddl-object eproblem *standard-output*)
          (print-pddl-object edomain-mod *standard-output*)
          (print eproblem-mod)
          (print-pddl-object eproblem-mod *standard-output*)))
      (let* ((dir (mktemp "enhanced"))
             (plans (handler-bind ((unix-signal
                                    (lambda (c)
                                      (format t "~&main search terminated")
                                      (invoke-restart
                                       (find-restart 'pddl:finish c)))))
                      (apply #'test-problem-common
                             (write-pddl eproblem-mod "eproblem.pddl" dir)
                             (write-pddl edomain-mod "edomain.pddl" dir)
                             test-problem-args))))
        (when *validation*
          (dolist (p plans)
            (validate-plan (pathname (format nil "~a/edomain.pddl" dir))
                           (pathname (format nil "~a/eproblem.pddl" dir)) p
                           :verbose t)))
        (format t "~&~a plans found, decoding the result plan." (length plans))
        (mapcar (lambda (plan i)
                  (terpri)
                  (block nil
                    (pprint-logical-block (*standard-output* nil :per-line-prefix (format nil "Plan ~a " i))
                      (return 
                        (decode-plan-all macros-mod plan edomain-mod eproblem-mod)))))
                plans (iota (length plans)))))))

(defun decode-plan-all (macros plan edomain-mod eproblem-mod)
  (handler-bind ((warning #'muffle-warning)
                 (undefined-predicate
                  (lambda (c)
                    (when (and (not *ground-macros*)
                               (eq 'equal (name c)))
                      (invoke-restart 'ignore)))))
    (reduce #'decode-plan macros
            ;;            ^^^^^^
            ;; based on edomain/eproblem: incompatible with edomain/eproblem-mod
            :from-end t
            :initial-value (pddl-plan :path plan :domain edomain-mod :problem eproblem-mod))))
