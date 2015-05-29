(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

;; this file contains functions that builds a component-problem and compute
;; the component-plan.

;;;; basic plan-task functionality

(defun clear-plan-task-cache ()
  "Clear the cache for plan-task."
  (clear-cache *PLAN-TASK-CACHE*))

(defvar *preprocessor* "lama-clean")
(defvar *preprocessor-options* "")
(defvar *debug-preprocessing* nil)
(defvar *component-plan-time-limit* 30)
(defvar *remove-component-problem-cost* nil
  "The problem and the domain solved by
the external planner could be modified so that it does not have
any :action-costs, so that any pure STRIPS-based planners can be used. It
depends on the special variable.")

(defcached plan-task (task)
  "Calls build-component-problem, make a plan with FD, then parse the results.
returns a list of PDDL-PLAN. The call to this function is cached and memoized, so be
careful if you measure the elapsed time. When you measure the time, run
 (clear-plan-task-cache) to clear the cache.

The behavior of this function can be tweaked:
`*remove-component-problem-cost*',
`*preprocessor*',`*preprocessor-options*', `*debug-preprocessing*',
`*component-plan-time-limit*'.

It signals 'evaluation-signal each time in order to count the actual
invocation of underlying planner easiy. "
  (when (and (abstract-component-task-goal task) ;; filter if the task has no goal
             (within-time-limit)) ;; do not compute plans when the time limit is reached
    (let* ((*problem* (build-component-problem task))
           (*domain* (domain *problem*))
           (dir (mktemp "plan-task" t)))
      (when *debug-preprocessing*
        (let ((*package* (find-package :pddl)))
          (terpri)
          (print-pddl-object *problem* *standard-output*)))
      (multiple-value-match
          (with-open-file (s "/dev/null" :direction :output :if-exists :overwrite)
            (funcall #'test-problem-common
                     (write-pddl (if *remove-component-problem-cost*
                                     (remove-costs *problem*)
                                     *problem*)
                                 "problem.pddl" dir)
                     (write-pddl (if *remove-component-problem-cost*
                                     (remove-costs *domain*)
                                     *domain*)
                                 "domain.pddl" dir)
                     :name *preprocessor*
                     :options *preprocessor-options*
                     :time-limit 1
                     :hard-time-limit *component-plan-time-limit*
                     :memory *memory-limit*
                     :stream (if *debug-preprocessing* *error-output* s)
                     :error (if *debug-preprocessing* *error-output* s)
                     :verbose *debug-preprocessing*))
        ((plans time memory complete)
         (when *debug-preprocessing*
           (print plans) (print time) (print memory) (print complete)
           (clear-output))
         (signal 'evaluation-signal :usage (list time memory))
         (values
          (mapcar (let ((i 0))
                    (curry #'pddl-plan
                           :name (concatenate-symbols
                                  (name *problem*) 'plan (incf i))
                           :path))
                  plans)
          complete))))))

;;;; retry wrapper for plan-task

(defun plan-task-with-retry (t1)
  "It computes a component plan of t1 and return the result
plans.  If it fails, it signals plan-not-found. If the signal is not
handled, return nil. It also provides `retry' restart, which signals
`restored-evaluation-signal' for the logging purpose and recompute the
component-plan. "
  (handler-bind
      ((plan-not-found
        (lambda (c)
          (signal c) ; default handler
          (return-from plan-task-with-retry nil))))
    (do-restart ((retry
                  (lambda ()
                    (signal 'restored-evaluation-signal)
                    (warn "Retrying, restoring objects in the other tasks."))))
      (plan-task t1))))

;;;; build-component-problem

(defun build-component-problem
    (abstract-task
     &optional
       (keeping-strategy
        (ask-for keeping-strategy
                 (make-instance 'filtering-strategy))))
  "Build a small problem which contains only the relevant objects in a
abstract task. objects and initial state is filtered according to the
given strategy."
  ;(format t "~&Building a component problem of~&~a ..." abstract-task)
  (ematch abstract-task
    ((abstract-component-task :problem *problem*
                              :ac (abstract-component components)
                              :goal task-goal)
     ;; NOTE: abstract-component-fact is not used !
     (ematch *problem*
       ((pddl-problem :name total-name
                      :domain *domain*
                      :objects objs
                      :init init
                      :metric metric)
        (multiple-value-bind
            (active-objects removed-objects)
            (filter-objects keeping-strategy
                            :objs objs
                            :components components
                            :init init)
          (multiple-value-bind
              (active-init removed-init)
              (filter-inits keeping-strategy
                            :objs objs
                            :components components
                            :active-objects active-objects
                            :removed-objects removed-objects
                            :init init)
            ;; (let ((*print-length* 10))
            ;;   (format t "~&Component Obj: ~w" components)
            ;;   (format t "~&Removed Obj  : ~w" removed-objects)
            ;;   (format t "~&Active Init  : ~w" active-init)
            ;;   (format t "~&Removed Init : ~w" removed-init))
            (pddl-problem
             :domain *domain*
             :name (apply #'concatenate-symbols
                          total-name (mapcar #'name components))
             :objects (set-difference active-objects
                                      (constants *domain*))
             :init active-init
             :goal (list* 'and task-goal)
             :metric metric))))))))


