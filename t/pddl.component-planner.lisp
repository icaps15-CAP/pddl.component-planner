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

(defparameter *woodworking-problems*
  (mapcar (lambda (x) (list x :part))
          (list wood-prob-sat-1
                wood-prob-sat-2
                wood-prob-sat-3
                wood-prob-sat-4
                wood-prob-sat-5
                wood-prob-sat-6
                wood-prob-sat-7
                wood-prob-sat-8
                wood-prob-sat-9
                wood-prob-sat-10
                wood-prob-sat-11
                wood-prob-sat-12
                wood-prob-sat-13
                wood-prob-sat-14
                wood-prob-sat-15
                wood-prob-sat-16
                wood-prob-sat-17
                wood-prob-sat-18
                wood-prob-sat-19)))

(defparameter *elevators-problems*
  (mapcar (lambda (x) (list x :passenger))
          (list elevators-sequencedstrips-p16_14_1
                elevators-sequencedstrips-p16_22_1
                elevators-sequencedstrips-p16_26_1
                elevators-sequencedstrips-p24_21_1
                elevators-sequencedstrips-p24_24_1
                elevators-sequencedstrips-p24_27_1
                elevators-sequencedstrips-p24_30_1
                elevators-sequencedstrips-p24_33_1
                elevators-sequencedstrips-p24_36_1
                elevators-sequencedstrips-p24_39_1
                elevators-sequencedstrips-p24_40_1
                elevators-sequencedstrips-p24_43_1
                elevators-sequencedstrips-p24_46_1
                elevators-sequencedstrips-p24_49_1
                elevators-sequencedstrips-p24_52_1
                elevators-sequencedstrips-p40_40_1
                elevators-sequencedstrips-p40_45_1
                elevators-sequencedstrips-p40_50_1
                elevators-sequencedstrips-p40_55_1
                elevators-sequencedstrips-p40_60_1)))

(defparameter *barman-sat11-problems*
  (mappend (lambda (x) (list (list x :cocktail)
                             (list x :shot)))
           (list barman-06-021 barman-06-022 barman-06-023
                 barman-06-024 barman-07-025 barman-07-026
                 barman-07-027 barman-07-028 barman-08-029
                 barman-08-030 barman-08-031 barman-08-032
                 barman-09-033 barman-09-034 barman-09-035
                 barman-09-036 barman-10-037 barman-10-038
                 barman-10-039 barman-10-040)))

(defparameter *openstacks-problems*
  (mappend (lambda (x) (list (list x :order)
                             (list x :product)))
           (list openstacks-1 openstacks-2 openstacks-3 openstacks-4
                 openstacks-5 openstacks-6 openstacks-7 openstacks-8
                 openstacks-9)))


(defparameter *satellite-problems*
  (mappend (lambda (x) (list (list x :direction)))
           (list satellite-typed-1 satellite-typed-2 satellite-typed-3
                 satellite-typed-4 satellite-typed-5 satellite-typed-6
                 satellite-typed-7 satellite-typed-8 satellite-typed-9
                 ;; satellite-typed-10 satellite-typed-11
                 ;; satellite-typed-12 satellite-typed-13
                 ;; satellite-typed-14 satellite-typed-15
                 ;; satellite-typed-16 satellite-typed-17
                 ;; satellite-typed-18 satellite-typed-19
                 ;; satellite-typed-20 satellite-typed-21
                 ;; satellite-typed-22 satellite-typed-23
                 ;; satellite-typed-24 satellite-typed-25
                 ;; satellite-typed-26 satellite-typed-27
                 ;; satellite-typed-28 satellite-typed-29
                 ;; satellite-typed-30 satellite-typed-31
                 ;; satellite-typed-32 satellite-typed-33
                 ;; satellite-typed-34 satellite-typed-35
                 satellite-typed-36)))


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
