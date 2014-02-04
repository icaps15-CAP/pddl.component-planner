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

(defun load-and-collect-problems (systems seeds regexp)
  (mapc #'asdf:load-system systems)
  (mappend (lambda (x)
             (mapcar (curry #'list x) seeds))
           (collect-problems regexp)))

(defvar *delayed-problems*
    (list (delay (load-and-collect-problems
                  '(:pddl.instances.rover)
                  '(:objective)
                  "ROVERPROB3[0-9]"))
          (delay (load-and-collect-problems
                  '(:pddl.instances.cell-assembly-eachparts)
                  '(:base)
                  "CELL-ASSEMBLY-MODEL2A-EACH-[12][0-9]"))
          (delay (load-and-collect-problems
                  '(:pddl.instances.woodworking-xlarge)
                  '(:part)
                  "WOOD-PROB-SAT-[0-9]*"))
          (delay (load-and-collect-problems
                  '(:pddl.instances.cell-assembly-eachparts)
                  '(:base)
                  "CELL-ASSEMBLY-MODEL2A-EACH-[12][0-9]"))
          (delay (load-and-collect-problems
                  '(:pddl.instances.elevators)
                  '(:passenger)
                  ".*ELEVATORS.*"))
          (delay (load-and-collect-problems
                  '(:pddl.instances.barman-sat11)
                  '(:cocktail :shot)
                  ".*BARMAN.*"))
          (delay (load-and-collect-problems
                  '(:pddl.instances.openstacks)
                  '(:order :product)
                  ".*OPENSTACKS.*"))
          (delay (load-and-collect-problems
                  '(:pddl.instances.openstacks)
                  '(:direction)
                  "SATELLITE-TYPED-.*"))))

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
