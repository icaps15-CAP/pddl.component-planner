#|
  This file is a part of pddl.component-planner project.
  Copyright (c) 2013 guicho ()
|#

(in-package :cl-user)
(defpackage pddl.component-planner-test
  (:use :cl
        :alexandria
        :eazylazy
        :trivial-lazy
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
        :local-time
        :optima
        :fiveam
        :lparallel)
  (:shadow :fail :maximize :minimize)
  (:shadowing-import-from :eazylazy :delay :force))
(in-package :pddl.component-planner-test)
(cl-syntax:use-syntax :annot)

(def-suite :pddl.component-planner)
(in-suite :pddl.component-planner)

(defun collect-problems (regex)
  (mapcar #'symbol-value
          (sort (iter (for problem in-package (find-package :pddl.instances))
                      (when (and (ppcre:scan regex (symbol-name problem))
                                 (boundp problem)
                                 (typep (symbol-value problem)
                                        'pddl-problem))
                        (collect problem)))
                #'string< :key #'symbol-name)))

(defun load-and-collect-problems (systems seeds regexp)
  (mapc #'asdf:load-system systems)
  (mappend (lambda (x)
             (print (name x))
             (mapcar (curry #'list x) seeds))
           (collect-problems regexp)))

(defparameter *delayed-problems*
              (list (delay (load-and-collect-problems
                            '(:pddl.instances.cell-assembly-eachparts)
                            '(:base)
                            "CELL-ASSEMBLY-MODEL2A-EACH-[1-5]$"))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro map-reduce (mapper reducer sequence &rest reduce-args)
    `(reduce ,reducer
             (map 'vector ,mapper ,sequence)
             ,@reduce-args)))

(defun categorize-problem (problem seed)
  (log:info "~&Categorizing problem ~a with seed ~a" (name problem) seed)
  (let* ((tasks/type
          (flatten
           (abstract-tasks problem seed)))
         (tasks/structure
          (categorize-tasks tasks/type :strict)))
    ;; list pf bags. each bag contains tasks of the same structure
    (log:info (name problem) seed (length tasks/type))
    (log:info (name problem) seed (mapcar #'length tasks/structure))
    (let ((tasks/plan
           (map-reduce (lambda (bucket)
                         (categorize-by-equality
                          bucket
                          #'task-plan-equal
                          :transitive nil))
                       #'append
                       tasks/structure
                       :initial-value nil)))
      (log:info (name problem) seed (mapcar #'length tasks/plan))
      (log:info (name problem) seed (length tasks/plan))
      ;; list of bags. each bag contains tasks whose plans are interchangeable
      (list (name problem)
            seed
            (length tasks/type)
            (mapcar #'length tasks/structure)
            (mapcar #'length tasks/plan)))))

(defparameter *log-dir*
  (merge-pathnames
   (format nil
           "Dropbox/lisp-output/~a/"
           (machine-instance))
   (user-homedir-pathname)))
(ensure-directories-exist *log-dir*)

;; generic version
;; (defun histogram (list)
;;   (iter (with h = nil)
        ;; (for n in list)
        ;; (incf (getf h n 0))
;;         (finally (return (plist-alist h)))))

;; assumes list of numbers
(defun histogram (list)
  (iter (with v = (make-array 10
                              :initial-element 0
                              :adjustable t))
        (for n in list)
        (when (>= n (array-dimension v 0))
          (adjust-array v (1+ n)))
        (incf (aref v n))
        (finally (return v))))

(defun categorize-problem-csv (problem seed)
  (let ((*default-pathname-defaults*
         (ensure-directories-exist
          (merge-pathnames
           (format nil "~a/~a/~a/"
                   (name (domain problem))
                   seed
                   (name problem))
           *log-dir*)))
        start end)
    (match (prog2
             (setf start (now))
             (categorize-problem problem seed)
             (setf end (now)))
      ((list _ _ length/type length/structure length/plan)
       (with-output-to-file (s "total" :if-exists :supersede)
         (print length/type s))
       (with-output-to-file (s "hist-structure" :if-exists :supersede)
         (cl-csv:write-csv (coerce (histogram length/structure) 'list)
                           :stream s))
       (with-output-to-file (s "hist-plan" :if-exists :supersede)
         (cl-csv:write-csv (coerce (histogram length/plan) 'list)
                           :stream s))
       (with-output-to-file (s "time" :if-exists :supersede)
         (format s "~f" (timestamp-difference end start)))))))

(defparameter *log-name*
  (merge-pathnames #p"logfile" *log-dir*))

(defun benchmark (domain-num)
  (log:config :daily *log-name*)
  (log:info "start categorization" domain-num)
  (print *delayed-problems*)
  (let ((forced (force (nth domain-num *delayed-problems*))))
    (print forced)
    (clear-plan-task-cache)
    (sb-ext:gc :full t)
    (map nil (lambda (pair)
               (print pair)
               (sb-ext:gc)
               (restart-return ((continue (lambda (c) c)))
                 (apply #'categorize-problem-csv pair)))
         forced)))
