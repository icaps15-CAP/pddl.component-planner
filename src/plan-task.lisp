(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

;; this file contains functions that builds a component-problem and compute
;; the component-plan.

;;;; basic plan-task functionality

@export
(defun clear-plan-task-cache ()
  "Clear the cache for plan-task."
  (clear-cache *PLAN-TASK-CACHE*))

@export
(defcached plan-task (task)
  "Calls build-component-problem, make a plan with FD, then parse the results.
returns a PDDL-PLAN. The call to this function is cached and memoized, so be
careful if you measure the elapsed time. When you measure the time, run
 (clear-plan-task-cache) to clear the cache."
  (let* ((*problem* (build-component-problem task))
         (*domain* (domain *problem*))
         (dir (mktemp "plan-task" t)))
    (mapcar (let ((i 0))
              (curry #'pddl-plan
                     :name (concatenate-symbols
                            (name *problem*) 'plan (incf i))
                     :path))
            (multiple-value-match
                (test-problem
                 (write-pddl *problem* "problem.pddl" dir)
                 (write-pddl *domain* "domain.pddl" dir)
                 :time-limit (ask-for time-limit 100)
                 :hard-time-limit (ask-for hard-time-limit (* 60 5))
                 :memory (ask-for memory (floor (/ (sb-ext:dynamic-space-size) 1000)))
                 :verbose t
                 ;; 15GB * 0.8 = 120
                 ;; :options "--search astar(lmcut())"
                 )
              ((plans t-time p-time s-time
                      t-memory p-memory s-memory)
               (signal 'evaluation-signal
                       :usage (list t-time p-time s-time
                                    t-memory p-memory s-memory))
               plans)))))

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

@export
(defun build-component-problem
    (abstract-task
     &optional
       (keeping-strategy
        (ask-for keeping-strategy
                 (make-instance 'filtering-strategy))))
  "Build a small problem which contains only the relevant objects in a
abstract task. objects and initial state is filtered according to the
given strategy."
  (format t "~&Building a component problem of~&~a ..." abstract-task)
  (ematch abstract-task
    ((abstract-component-task :problem *problem*)
     (ematch *problem*
       ((pddl-problem :name total-name
                      :domain *domain*
                      :objects objs
                      :init init
                      :metric metric)
        (match abstract-task
          ((abstract-component-task- (ac (abstract-component components))
                                     (goal task-goal))
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
               (format t "~&~<Component Obj: ~;~@{~a~^, ~:_~}~:>" components)
               (format t "~&~<Removed Obj  : ~;~@{~a~^, ~:_~}~:>" removed-objects)
               (format t "~&~<Active Init  : ~;~@{~a~^, ~:_~}~:>" active-init)
               (format t "~&~<Removed Init : ~;~@{~a~^, ~:_~}~:>" removed-init)
               (pddl-problem
                :domain *domain*
                :name (apply #'concatenate-symbols
                             total-name (mapcar #'name components))
                :objects active-objects
                :init active-init
                :goal (list* 'and task-goal)
                :metric metric))))))))))


