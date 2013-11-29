#|
  This file is a part of pddl.component-planner project.
  Copyright (c) 2013 guicho ()
|#

(in-package :cl-user)
(defpackage pddl.component-planner-test
  (:use :cl
        :alexandria
        :pddl
        :pddl.component-abstraction
        :pddl.component-planner
        :pddl.loop-planner
        :pddl.loop-detection
        :pddl.instances
        :guicho-utilities
        :repl-utilities
        :iterate
        :optima
        :fiveam)
  (:shadow :fail :maximize :minimize))
(in-package :pddl.component-planner-test)

(def-suite :pddl.component-planner)
(in-suite :pddl.component-planner)

(defparameter *rover-problems*
  (mapcar (lambda (x) (list x :objective))
          (list roverprob14
                roverprob15
                roverprob16
                roverprob17
                roverprob18
                roverprob19
                roverprob20)))

(defparameter *eachparts-problems*
    (mapcar (lambda (x) (list x :base))
            (list cell-assembly-model2a-each-1
                  cell-assembly-model2a-each-2
                  cell-assembly-model2a-each-3
                  cell-assembly-model2a-each-4
                  cell-assembly-model2a-each-5)))

(defvar *seed*)

(defparameter *problem-sets* 
  (shuffle (append *rover-problems*
                   *eachparts-problems*)))

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
                                      :hard-time-limit 10
                                      :options "--search astar(lmcut())"))))
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
        (for-all ((t1 (curry #'random-elt bucket) t1)
                  (t2 (curry #'random-elt bucket) (not (eq t1 t2))))
          (finishes
            (print (mapping-between-tasks t1 t2))))))))

(test (:categorize-by-plan-conversion :fixture problem)
  (finishes
    (mapcar (lambda (tasks)
              (categorize-by-equality
               tasks
               #'task-plan-equal
               :transitive nil))
            (mappend (rcurry #'categorize-tasks :strict)
                     (abstract-tasks *problem* *seed*)))))

