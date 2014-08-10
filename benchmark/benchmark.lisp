(defpackage :pddl.component-planner.benchmark
  (:use :cl
        :guicho-utilities
        :pddl
        :pddl.component-abstraction
        :pddl.component-planner
        :log4cl
        :alexandria
        :iterate)
  (:shadow :minimize :maximize))

(in-package :pddl.component-planner.benchmark)

(defparameter *log-dir*
  (merge-pathnames
   (format nil "Dropbox/lisp-output/~a/" (machine-instance))
   (user-homedir-pathname)))
(ensure-directories-exist *log-dir*)

;;; collecting problem data

(defun collect-problems (regex)
  (mapcar #'symbol-value
          (sort (iter (for problem in-package (find-package :pddl.instances))
                      (when (and (ppcre:scan regex (symbol-name problem))
                                 (boundp problem)
                                 (typep (symbol-value problem) 'pddl-problem))
                        (collect problem)))
                #'string< :key #'symbol-name)))

(defun load-and-collect-problems (systems seeds regexp)
  (mapc #'asdf:load-system systems)
  (mappend (lambda (x)
             (print (name x))
             (mapcar (curry #'list x) seeds))
           (collect-problems regexp)))

(defparameter *delayed-problems*
              (load-and-collect-problems
               '(:pddl.instances.cell-assembly-eachparts)
               '(:base)
               "CELL-ASSEMBLY-MODEL2A-.*"))

(defun run ()
  (mapcar (lambda (list)
            (destructuring-bind (problem seed) list
              (solve-problem-enhancing problem seed :verbose t)))
          *delayed-problems*))
