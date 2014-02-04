#|
  This file is a part of pddl.component-planner project.
  Copyright (c) 2013 guicho ()
|#

(in-package :cl-user)
(defpackage pddl.component-planner-test
  (:use :cl
        :alexandria
        :eazylazy
        :pddl
        :pddl.component-abstraction
        :pddl.component-planner
        :pddl.loop-planner
        :pddl.loop-detection
        :pddl.instances
        :guicho-utilities
        :guicho-utilities.threading
        :repl-utilities
        :iterate
        :log4cl
        :function-cache
        :optima
        :fiveam
        :lparallel)
  (:shadow :fail :maximize :minimize :delay :force))
(in-package :pddl.component-planner-test)
(cl-syntax:use-syntax :annot)

(def-suite :pddl.component-planner)
(in-suite :pddl.component-planner)

(defun collect-problems (regex)
  (mapcar #'symbol-value
          (sort (iter (for problem in-package *package*)
                      (when (and (ppcre:scan regex (symbol-name problem))
                                 (boundp problem)
                                 (typep (symbol-value problem)
                                        'pddl-problem))
                        (collect problem)))
                #'string< :key #'symbol-name)))

(defparameter *rover-problems*
  (mapcar (lambda (x) (list x :objective))
          (collect-problems "ROVERPROB3[0-9]")))

(defparameter *eachparts-problems*
              (mapcar (lambda (x) (list x :base))
                      (collect-problems "CELL-ASSEMBLY-MODEL2A-EACH-[12][0-9]")))

(defparameter *woodworking-problems*
  (mapcar (lambda (x) (list x :part))
          (collect-problems "WOOD-PROB-SAT-[0-9]*")))

(defparameter *elevators-problems*
  (mapcar (lambda (x) (list x :passenger))
          (collect-problems ".*ELEVATORS.*")))

(defparameter *barman-sat11-problems*
  (mappend (lambda (x) (list (list x :cocktail)
                             (list x :shot)))
           (collect-problems ".*BARMAN.*")))

(defparameter *openstacks-problems*
  (mappend (lambda (x) (list (list x :order)
                             (list x :product)))
           (collect-problems ".*OPENSTACKS.*")))


(defparameter *satellite-problems*
  (mappend (lambda (x) (list (list x :direction)))
           (collect-problems "SATELLITE-TYPED-.*")))


(defvar *seed*)

(defparameter *problem-sets-orig*
 (append *rover-problems*
         *eachparts-problems*
         *woodworking-problems*
         *elevators-problems*
         *barman-sat11-problems*
         *openstacks-problems*
         *satellite-problems*))

(defparameter *problem-sets* 
  (shuffle (copy-list *problem-sets-orig*)))

(defun until-exhaust (howmany list)
  (let ((i 0))
    (lambda ()
      (incf i)
      (when (<= i howmany)
        (random-elt list)))))

(def-fixture problem ()
  (for-all ((p-s (until-exhaust 10 *problem-sets*)
                 p-s))
    (destructuring-bind (*problem* *seed*) p-s
      (&body))))

(test (:plan-component-problems :fixture problem)
  (finishes
    (mapc (compose (lambda (results)
                     (iter (for r in results)
                           (is (probe-file r))))
                   (lambda (path-pair)
                     (pass)
                     (apply #'test-problem
                            (append path-pair
                                    '(:time-limit 5
                                      :hard-time-limit 20
                                      ;; :options "--search astar(lmcut())"
                                      ))))
                   (lambda (problem)
                     (is (typep problem 'pddl-problem))
                     (list (write-problem problem)
                           (path (domain problem))))
                   (lambda (task)
                     (pass)
                     (build-component-problem task))
                   #'first)
          (mappend (rcurry #'categorize-tasks :strict)
                   (abstract-tasks *problem* *seed*))))
  (finishes
    (mapcar (compose (lambda (results)
                       (iter (for r in results)
                             (is (typep r 'pddl-plan))))
                     #'plan-task #'first)
            (mappend (rcurry #'categorize-tasks :strict)
                     (abstract-tasks *problem* *seed*)))))

(def-fixture tasks ()
  (let ((tasks (mappend (rcurry #'categorize-tasks :strict)
                        (abstract-tasks *problem* *seed*))))
    (&body)))

(def-fixture bucket ()
  (for-all ((bucket (until-exhaust 3 tasks) bucket))
    (&body)))

(def-fixture task ()
  (for-all ((task (until-exhaust 3 bucket) task))
    (&body)))

;; next: see what happens if some part of a component is replaced

(test :fluently-connected-object
  (with-fixture problem ()
    (with-fixture tasks ()
      (with-fixture bucket ()
        (with-fixture task ()
          (ematch task
            ((abstract-component-task (ac (abstract-component
                                           (components c1)
                                           (attributes a1))))
             (let ((goal (goal-object task))
                   (init (init-object task)))
               (is (null (intersection init c1)))
               (is (null (intersection goal c1)))
               (is (null (intersection init a1)))
               (is (null (intersection goal a1)))))))
        (unless (= (length bucket) 1)
          (for-all ((t1 (curry #'random-elt bucket))
                    (t2 (curry #'random-elt bucket) (not (eq t1 t2))))
            (let ((iobj1 (init-object t1))
                  (gobj1 (goal-object t1))
                  (iobj2 (init-object t2))
                  (gobj2 (goal-object t2)))
              (is (= (length iobj1) (length iobj2)))
              (is (= (length gobj1) (length gobj2)))
              (let ((eqtype
                     (lambda (o1 o2)
                       (is (eq (type o1) (type o2))))))
                (mapcar eqtype iobj1 iobj2)
                (mapcar eqtype gobj1 gobj2)))))))))

(test :mapping-between-tasks
  (with-fixture problem ()
    (with-fixture tasks ()
      (with-fixture bucket ()
        (if (>= (length bucket) 2)
            (for-all ((t1 (curry #'random-elt bucket) t1)
                      (t2 (curry #'random-elt bucket) (not (eq t1 t2))))
              (finishes
                (print (mapping-between-tasks t1 t2))))
            (pass))))))


(defvar problem)
(defvar seed)
(defvar tasks)

(defun set-tasks (%problem %seed)
  (setf problem %problem)
  (setf seed %seed)
  (setf tasks (categorize-tasks
               (flatten
                (abstract-tasks problem seed)) :strict)))

(test (:categorize-by-plan-conversion1)
    (finishes
      (set-tasks cell-assembly-model2a-each-3 :base))
    (is (= 1 (length tasks)))
    (is (= 3 (length (first tasks))))
    (is (= 1 (length (categorize-tasks (first tasks) :strict))))
    (is (= 1 (length (categorize-by-equality
                      (first tasks)
                      #'task-plan-equal
                      :transitive nil)))))

(test (:categorize-by-plan-conversion2
       :fixture problem
       :depends-on :categorize-by-plan-conversion1)
  (finishes
    (mapcar (lambda (tasks)
              (categorize-by-equality
               tasks
               #'task-plan-equal
               :transitive nil))
            (mappend (rcurry #'categorize-tasks :strict)
                     (abstract-tasks *problem* *seed*)))))


(test (:categorize-by-plan-conversion3)
  (finishes
    (set-tasks roverprob20 :objective)
    (print (mappend (lambda (bucket)
                      (categorize-by-equality
                       bucket
                       #'task-plan-equal
                       :transitive nil))
                    tasks))))


(test (:categorization-not-bloat)
  (flet ((set-big-wood-problem (problem)
           (handler-bind ((storage-condition
                           (lambda (c)
                             (abort c))))
             (set-tasks problem :part)
             (print (mapcar #'length tasks))
             (sb-ext:gc :full t))))
    (finishes (set-big-wood-problem wood-prob-sat-80))
    (finishes (set-big-wood-problem wood-prob-sat-81))
    (finishes (set-big-wood-problem wood-prob-sat-82))
    (finishes (set-big-wood-problem wood-prob-sat-83))
    (finishes (set-big-wood-problem wood-prob-sat-84))
    (finishes (set-big-wood-problem wood-prob-sat-85))
    (finishes (set-big-wood-problem wood-prob-sat-86))))

(test (:categorize-by-plan-conversion4)
  (finishes
    (set-tasks wood-prob-sat-86 :part)
    ;; (clear-plan-task-cache)  
    (let* ((*validator-verbosity* nil)
           (result
            (time
             (mapcar (lambda (bucket)
                       (let ((result (categorize-by-equality
                                      bucket
                                      #'task-plan-equal
                                      :transitive nil)))
                         result))
                     tasks)))
           (result2
            (time
             (mapcar (lambda (bucket)
                       (let ((result (categorize-by-equality
                                      bucket
                                      #'task-plan-equal)))
                         result))
                     tasks))))
      (print result)
      (print (mapcar #'length result))
      (print (mapcar #'length result2)))))

(defvar result)
;; (defvar result2)


(test (:categorize-by-plan-conversion-parallel)
  (finishes
    (set-tasks wood-prob-sat-86 :part)
    ;; (clear-plan-task-cache)
    (setf result (pmap-reduce (lambda (bucket)
                                (categorize-by-equality
                                 bucket
                                 #'task-plan-equal
                                 :transitive nil))
                              #'append
                              tasks
                              :initial-value nil))
    ;;効果なし
    ;; (setf result2 (categorize-by-equality
    ;;                result
    ;;                (lambda (bucket1 bucket2)
    ;;                  (or (task-plan-equal (car bucket1) (car bucket2))
    ;;                      (task-plan-equal (car bucket2) (car bucket1))))
    ;;                :transitive nil))
    (print (mapcar #'length result))
    ;; (print (mapcar #'length result2))
    ;; result2
    ))
 
(defun categorize-all (problem-sets)
  (iter (for (problem seed) in problem-sets)
        (sb-ext:gc :full t)
        (collect (categorize-problem problem seed))))

(defun categorize-problem (problem seed)
  (log:info "~&Categorizing problem ~a with seed ~a"
            (name problem) seed)
  (let* ((tasks/type
          (flatten
           (abstract-tasks problem seed)))
         (tasks/structure
          (categorize-tasks tasks/type :strict)))
    ;; list pf bags. each bag contains tasks of the same structure
    (log:info (length tasks/type))
    (log:info (mapcar #'length tasks/structure))
    (let ((tasks/plan
           (mappend (lambda (bucket)
                      (categorize-by-equality
                       bucket
                       #'task-plan-equal
                       :transitive nil))
                    tasks/structure)))
      (log:info (mapcar #'length tasks/plan))
      (log:info (length tasks/plan))
      ;; list of bags. each bag contains tasks whose plans are interchangeable
      (list (name problem)
            seed
            (mapcar #'length tasks/structure)
            (mapcar #'length tasks/plan)))))

(defparameter *log-dir*
  (merge-pathnames
   #p"Dropbox/component-planner/"
   (user-homedir-pathname)))
(ensure-directories-exist *log-dir*)

(defparameter *log-name*
  (merge-pathnames #p"logfile" *log-dir*))

(defun benchmark ()
  (log:config :daily *log-name*)
  (log:info "start categorization")
  (categorize-all *problem-sets-orig*))