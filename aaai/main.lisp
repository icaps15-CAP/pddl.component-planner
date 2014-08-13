#! /usr/local/bin/sbcl --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (format t "~%loading Quicklisp...~%")
  (when (probe-file quicklisp-init)
    (load quicklisp-init))
  (eval (read-from-string
         "(push (merge-pathnames
         \"repos/lisp/\"
         (user-homedir-pathname))
        quicklisp-client:*local-project-directories*)")))

(unless (asdf:version-satisfies (asdf:asdf-version) "3.0.3")
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames
           "Dropbox/asdf/build/asdf.lisp"
           (user-homedir-pathname)))))

(ql:quickload :pddl.component-planner)
(defpackage :pddl.component-planner.experiment
  (:use :cl :cl-rlimit :pddl :pddl.component-planner :optima))
(in-package :pddl.component-planner.experiment)

(defmacro suppress (&body body)
  `(handler-bind ((warning #'muffle-warning))
     ,@body))

(defvar *verbose* nil)

(defun solve (ppath dpath)
  (multiple-value-bind (dname domain) (suppress (parse-file dpath nil t))
    (multiple-value-bind (pname problem) (suppress (parse-file ppath nil t))
      (print dname)
      (print domain)
      (print pname)
      (print problem)
      (let ((plp (make-pathname :defaults ppath :type "plan")))
        (let ((plan (solve-problem-enhancing problem :verbose *verbose*)))
          (write-plan plan plp *default-pathname-defaults* t))))))

(defun main (&optional (argv (cdr sb-ext:*posix-argv*)))
  (let ((*package* (find-package :pddl.component-planner.experiment)))
    (ematch argv
      ((list* "-v" rest)
       (setf *verbose* t)
       (main rest))
      ((list* "-t" time rest)
       (let ((parsed (parse-integer time)))
         (setf (rlimit +rlimit-cpu-time+) parsed))
       (main rest))
      ((list* "-m" memory rest)
       (let ((parsed (parse-integer memory)))
         (setf (rlimit +rlimit-address-space+) parsed))
       (main rest))
      ((list ppath)
       (solve ppath (make-pathname :defaults ppath :name "domain")))
      ((list ppath dpath)
       (solve ppath dpath)))))

(defun save (name)
  (sb-ext:gc :full t)
  (sb-ext:save-lisp-and-die
   name
   :toplevel #'main
   :executable t
   :purify t))

(save "component-planner")
