
(defpackage :pddl.component-planner.experiment
  (:use :cl :cl-rlimit :pddl :pddl.component-planner :optima
        :alexandria :iterate)
  (:shadowing-import-from :pddl :minimize :maximize))
(in-package :pddl.component-planner.experiment)

(defmacro suppress (&body body)
  `(handler-bind ((warning #'muffle-warning))
     ,@body))

(defvar *verbose* nil)
(defvar *validation* nil)

(defparameter *lmcut-lazy-gbfs* (wrap-option "--search lazy_greedy(lmcut())"))
(defparameter *lmcut-eager-gbfs* (wrap-option "--search eager_greedy(lmcut())"))


(defun solve (ppath dpath)
  (multiple-value-bind (dname domain) (suppress (parse-file dpath nil t))
    (multiple-value-bind (pname problem) (suppress (parse-file ppath nil t))
      (print dname)
      (print domain)
      (print pname)
      (print problem)
      (let ((plans (solve-problem-enhancing problem
                                            ;; :options *lmcut-lazy-gbfs* ; not effective
                                            ;; :options *lmcut-eager-gbfs* ; not effective
                                            ;; :options *opt-options*
                                            :time-limit 1 ; satisficing
                                            :verbose *verbose*)))
        (iter (for plan in plans)
              (for i from 1)
              (for plp =
                   (merge-pathnames
                    (format nil "~a.plan.~a"
                            (pathname-name ppath) i)))
              (when (probe-file plp) (delete-file plp))
              (write-plan plan plp
                          *default-pathname-defaults* t)
              (when *validation*
                (validate-plan dpath ppath plp :verbose *verbose*)))))))

(defun main (&optional (argv (cdr sb-ext:*posix-argv*)))
  (print argv)
  (let ((*package* (find-package :pddl.component-planner.experiment)))
    (match argv
      ((list* "-v" rest)
       (let ((*verbose* t))
         (main rest)))
      ((list* "--validation" rest)
       (let ((*validation* t))
         (main rest)))
      ((list* "--preprocess-only" rest)
       (let ((*preprocess-only* t))
         (main rest)))
      ;; underlying planner
      ((list* "--preprocess-ff" rest)
       (let ((*preprocess-ff* t))
         (main rest)))
      ((list* "--main-search-ff" rest)
       (let ((*main-search-ff* t))
         (main rest)))
      ((list* "--use-ff" rest)
       (let ((*preprocess-ff* t)
             (*main-search-ff* t))
         (main rest)))
      ((list* "--main-options" string rest)
       (let ((*main-options* string))
         (main rest)))
      ;; time limit and resource
      ((list* "--preprocess-limit" time rest)
       (let ((*preprocess-time-limit* (parse-integer time)))
         (main rest)))
      ((list* "--component-plan-limit" time rest)
       (let ((*component-plan-time-limit* (parse-integer time)))
         (main rest)))
      ((list* "-t" time rest)
       (let ((*hard-time-limit* (parse-integer time)))
         (main rest)))
      ((list* "-m" memory rest)
       (let ((*memory-limit* (parse-integer memory)))
         (main rest)))
      ((list ppath)
       (let ((ppath (merge-pathnames ppath)))
         (main (list ppath (make-pathname :defaults ppath
                                          :name "domain")))))
      ((list ppath dpath)
       (let ((ppath (merge-pathnames ppath))
             (dpath (merge-pathnames dpath)))
         (solve ppath dpath)))
      (_
       (format *error-output* "~&Invalid Arguments!")
       (format *error-output* "~&Usage: component-planner PROBLEM [DOMAIN]~
               ~%~@{~4t~25a ~:[          ~;~:*~10a~] : ~a~%~}"
               "-v" nil "specify verbosity"
               "--validation" nil "run the validator after the planning"
               "--preprocess-only" nil "stops immediately when preprocess finishes"
               "--preprocess-ff" nil "use FF during preprocesssing (otherwise LAMA ipc 2011)"
               "--main-search-ff" nil "use FF during main search (otherwise LAMA ipc 2011)"
               "--use-ff" nil "both --preprocess-ff and --main-search-ff"
               "--main-options" 'string "specify the additional options given to macroff/fastdownward."
               "--preprocess-limit" 'time "specify the approximated sum of maxmimum preprocessing time in integer"
               "--component-plan-limit" 'time "specify the time limit of component planning in integer"
               "-t" 'time "time limit for the main search. NOT the total limit"
               "-m" 'memory "memory limit for the main search. NOT the total limit")
       (format *error-output* "~&DOMAIN is by default domain.pddl in the same directory")
       (error "~&Invalid Arguments!~2%")))))

(defun toplevel ()
  (sb-ext:disable-debugger)
  (main))

(defun save (name)
  (sb-ext:gc :full t)
  (sb-ext:save-lisp-and-die
   name
   :toplevel #'toplevel
   :executable t
   :purify t
   ;; :save-runtime-options t
   ))

;;;; elevators

(defun test1-1 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "elevators-sat11/p01.pddl")))
(defun test1-2 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "elevators-sat11/p04.pddl")))
(defun test1-3 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "elevators-sat11/p08.pddl")))
(defun test1-4 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "elevators-sat11/p12.pddl")))

;;;; fd

(defun fdtest ()
  (clear-plan-task-cache)
  (test-problem "elevators-sat11/p01.pddl" "elevators-sat11/domain.pddl"
                :verbose t :options *opt-options*))
(defun fdtest-sat ()
  (clear-plan-task-cache)
  (test-problem "depot/p01.pddl" "depot/domain.pddl"
                :verbose t))

;;;; cell-assembly-noneg-nocost

(defun test2-1 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "cell-assembly-noneg-nocost/p01.pddl")))
(defun test2-2 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "cell-assembly-noneg-nocost/p04.pddl")))
(defun test2-3 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "cell-assembly-noneg-nocost/p08.pddl")))
(defun test2-4 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "cell-assembly-noneg-nocost/p12.pddl")))

;;;; pipesworld
(defun test3-1 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "pipesworld-notankage/p01.pddl")))
(defun test3-2 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "pipesworld-notankage/p04.pddl")))

(defun test4-1 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "pipesworld-tankage/p01.pddl")))
(defun test4-2 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "pipesworld-tankage/p04.pddl")))

;;;; barman

(defun test5-1 ()
  (clear-plan-task-cache) ;; かなり難しい
  (main (list "--validation" "-v" "-m" "2000000" "barman/p01.pddl")))
(defun test5-2 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "barman/p04.pddl")))
(defun test5-3 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "barman/p08.pddl")))

;;;;

(defun test6-1 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "childsnack/p01.pddl")))
(defun test6-1-fd ()
  (clear-plan-task-cache)
  (test-problem "childsnack/p01.pddl" "childsnack/domain.pddl" :verbose t))
(defun test6-2 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "childsnack/p04.pddl")))
(defun test6-2-fd ()
  (clear-plan-task-cache)
  (test-problem "childsnack/p04.pddl" "childsnack/domain.pddl" :verbose t))


(defun test7-1 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "woodworking-sat11-nocost/p01.pddl")))
(defun test7-1-fd ()
  (clear-plan-task-cache)
  (test-problem "woodworking-sat11-nocost/p01.pddl" "woodworking-sat11-nocost/domain.pddl" :verbose t))
(defun test7-2 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "woodworking-sat11-nocost/p04.pddl")))
(defun test7-2-fd ()
  (clear-plan-task-cache)
  (test-problem "woodworking-sat11-nocost/p04.pddl" "woodworking-sat11-nocost/domain.pddl" :verbose t))

(defun test8-1 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "satellite-typed2/p01.pddl")))
(defun test8-1-fd ()
  (clear-plan-task-cache)
  (test-problem "satellite-typed2/p01.pddl" "satellite-typed2/domain.pddl" :verbose t))
(defun test8-2 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "satellite-typed2/p04.pddl")))
(defun test8-2-fd ()
  (clear-plan-task-cache)
  (test-problem "satellite-typed2/p04.pddl" "satellite-typed2/domain.pddl" :verbose t))
(defun test8-3 ()
  (clear-plan-task-cache)
  (main (list "--validation" "-v" "-m" "2000000" "satellite-typed2/p01-hard.pddl")))
