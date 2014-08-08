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

;;; categorize each problem

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro map-reduce (mapper reducer sequence &rest reduce-args)
    `(reduce ,reducer
             (map 'vector ,mapper ,sequence)
             ,@reduce-args)))

(defun categorize-problem (problem seed)
  (log:info "~&Categorizing problem ~a with seed ~a" (name problem) seed)
  (let* ((tasks/type (flatten (abstract-tasks (binarize problem (domain problem)) seed)))
         (tasks/structure (categorize-tasks tasks/type :strict)))
    ;; list pf bags. each bag contains tasks of the same structure
    (log:info (name problem) seed (length tasks/type))
    (log:info (name problem) seed (mapcar #'length tasks/structure))
    (let ((tasks/plan
           (map-reduce (lambda (bucket)
                         (coerce (categorize-by-equality
                                  bucket #'task-plan-equal
                                  :transitive t)
                                 'list))
                       #'append tasks/structure
                       :initial-value nil)))
      (log:info (name problem) seed (mapcar #'length tasks/plan))
      (log:info (name problem) seed (length tasks/plan))
      ;; list of bags. each bag contains tasks whose plans are interchangeable
      (list (name problem)
            seed
            (length tasks/type)
            (mapcar #'length tasks/structure)
            (mapcar #'length tasks/plan)))))



