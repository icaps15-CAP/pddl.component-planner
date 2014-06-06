(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

@export
(defcached plan-task (task)
  "Calls build-component-problem, make a plan with FD, then parse the results.
returns a PDDL-PLAN. The call to this function is cached and memoized, so be
careful if you measure the elapsed time. When you measure the time, run
 (clear-plan-task-cache) to clear the cache."
  (let* ((*problem* (build-component-problem task))
         (*domain* (domain *problem*)))
    (mapcar (let ((i 0))
              (curry #'pddl-plan
                     :name (concatenate-symbols (name *problem*) 'plan (incf i))
                     :path))
            (multiple-value-match (test-problem
                                   (write-problem *problem*)
                                   (path *domain*)
                                   :time-limit 1
                                   :hard-time-limit (* 60 5)
                                   :memory (floor (/ (sb-ext:dynamic-space-size) 1000)) ;; 15GB * 0.8 = 120
                                   ;; :options "--search astar(lmcut())"
                                   )
              ((plans t-time p-time s-time t-memory p-memory s-memory)
               (signal 'evaluation-signal :time (list t-time p-time s-time t-memory p-memory s-memory))
               plans)))))

@export
(defun clear-plan-task-cache ()
  "Clear the cache for plan-task."
  (clear-cache *PLAN-TASK-CACHE*))

@export
(defun task-plan-equal (t1 t2)
  "Computes plan-wise compatibility. It returns true if any of the component plan
mapped from t1 to t2 is a valid plan of t2."
  ;; (assert (abstract-component-task-strict= t1 t2))
  (let ((*default-keep-init* nil)
        (*default-keep-objects* nil)
        (*validator-verbosity* nil))
    (signal 'comparison-signal)
    (let* ((problem2 (build-component-problem t2))
           (problem-path2 (write-problem problem2)))
      (some 
       (lambda (plan1)
         (validate-plan (path (domain problem2))
                        problem-path2
                        (write-plan
                         (apply-mapping
                          plan1
                          (mapping-between-tasks t1 t2)
                          problem2))))
       (plan-task-with-full-restoration t1)))))

(defun plan-task-with-full-restoration (t1)
  "It first tries to compute a
component plan of t1 and return the result plans.
If it fails, it restore the objects in the problem."
  (let ((first-time t))
    (handler-bind ((plan-not-found
                    (lambda (c)
                      @ignore c
                      (if first-time
                          (invoke-restart (find-restart 'retry))
                          (return-from plan-task-with-full-restoration nil)))))
      (do-restart ((retry
                    (lambda ()
                      (setf *validator-verbosity* nil
                            *default-keep-init* t
                            *default-keep-objects* t
                            first-time nil)
                      (signal 'restored-evaluation-signal)
                      (warn "Retrying, restoring objects in the other tasks."))))
        (plan-task t1)))))

;; (defun plan-task-with-iterative-restoration (t1)
;;   "It first tries to compute a
;; component plan of t1 and return the result plans.
;; If it fails, it restore the objects in the problem."
;;   (let ((first-time t))
;;     (handler-bind ((plan-not-found
;;                     (lambda (c)
;;                       @ignore c
;;                       (if first-time
;;                           (invoke-restart (find-restart 'retry))
;;                           (return-from plan-task-with-full-restoration nil)))))
;;       (do-restart ((retry
;;                     (lambda ()
;;                       (setf *validator-verbosity* nil
;;                             *default-keep-init* t
;;                             *default-keep-objects* t
;;                             first-time nil)
;;                       (signal 'restored-evaluation-signal)
;;                       (warn "Retrying, restoring objects in the other tasks."))))
;;         (plan-task t1)))))
