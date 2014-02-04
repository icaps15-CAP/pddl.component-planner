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

(defparameter *problem-sets-orig*
 (append *rover-problems*
         *eachparts-problems*
         *woodworking-problems*
         *elevators-problems*
         *barman-sat11-problems*
         *openstacks-problems*
         *satellite-problems*))

(defun categorize-all (problem-sets)
  (iter (for (problem seed) in problem-sets)
        (sb-ext:gc :full t)
        (collect (categorize-problem problem seed))))

(defun categorize-problem (problem seed)
  (format t "~&Categorizing problem ~a with seed ~a"
          (name problem) seed)
  (let* ((tasks/type
          (flatten
           (abstract-tasks problem seed)))
         (tasks/structure
          (categorize-tasks tasks/type :strict)))
    ;; list pf bags. each bag contains tasks of the same structure
    (format t "~&Tasks: ~a in total" (length tasks/type))
    (print (mapcar #'length tasks/structure))
    (let ((tasks/plan
           (pmap-reduce (lambda (bucket)
                          (categorize-by-equality
                           bucket
                           #'task-plan-equal
                           :transitive nil))
                        #'append
                        tasks/structure
                        :initial-value nil)))
      ;; list of bags. each bag contains tasks whose plans are interchangeable
      (list (name problem)
            seed
            (mapcar #'length tasks/structure)
            (mapcar #'length tasks/plan)))))
