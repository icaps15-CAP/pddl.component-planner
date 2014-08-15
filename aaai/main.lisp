
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

(ql:register-local-projects)
#-add-cost
(ql:quickload :pddl.component-planner)
#+add-cost
(ql:quickload :pddl.component-planner.add-cost)
(defpackage :pddl.component-planner.experiment
  (:use :cl :cl-rlimit :pddl :pddl.component-planner :optima))
(in-package :pddl.component-planner.experiment)

(defmacro suppress (&body body)
  `(handler-bind ((warning #'muffle-warning))
     ,@body))

(defvar *verbose* nil)

(defun solve (ppath dpath plp)
  (multiple-value-bind (dname domain) (suppress (parse-file dpath nil t))
    (multiple-value-bind (pname problem) (suppress (parse-file ppath nil t))
      (print dname)
      (print domain)
      (print pname)
      (print problem)
      (let ((plan (solve-problem-enhancing problem
                                           :options *opt-options*
                                           :time-limit 1 ; satisficing
                                           :verbose *verbose*)))
        (write-plan plan plp *default-pathname-defaults* t)))))

(defun main (&optional (argv (cdr (print sb-ext:*posix-argv*))))
  (let ((*package* (find-package :pddl.component-planner.experiment)))
     (ematch argv
       ((list* "-v" rest)
        (setf *verbose* t)
        (main rest))
       ((list* "-t" time rest)
        (setf *hard-time-limit* (parse-integer time))
        (main rest))
       ((list* "-m" memory rest)
        (setf *memory-limit* (parse-integer memory))
        (main rest))
       ((list ppath)
        (let ((ppath (merge-pathnames ppath)))
          (main (list ppath (make-pathname :defaults ppath
                                           :name "domain")))))
       ((list ppath dpath)
        (let ((ppath (merge-pathnames ppath))
              (dpath (merge-pathnames dpath)))
          (let ((plp (make-pathname :defaults ppath :type "plan")))
            (when (probe-file plp)
              (delete-file plp))
            (solve ppath dpath plp)))))))

(defun save (name)
  (sb-ext:gc :full t)
  (sb-ext:save-lisp-and-die
   name
   :toplevel #'main
   :executable t
   :purify t
   :save-runtime-options t))

(defun test ()
  (main (list "-v" "-m" "2000000" "elevators-sat11/p01.pddl")))
(defun fdtest ()
  (test-problem "elevators-sat11/p01.pddl" "elevators-sat11/domain.pddl"
                :verbose t :options *opt-options*))
(defun test2 ()
  (main (list "-v" "-m" "2000000" "elevators-sat11/p04.pddl")))


#-add-cost
(save "component-planner")
#+add-cost
(save "component-planner-cost")
