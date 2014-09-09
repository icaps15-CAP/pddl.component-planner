
(defpackage :pddl.component-planner.experiment
  (:use :cl :cl-rlimit :pddl :pddl.component-planner :optima
        :alexandria :iterate)
  (:shadowing-import-from :pddl :minimize :maximize))
(in-package :pddl.component-planner.experiment)

(defmacro suppress (&body body)
  `(handler-bind ((warning #'muffle-warning))
     ,@body))

(defvar *verbose* nil)

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
                                            :time-limit 1 ; satisficing
                                            :name *main-search*
                                            :options *main-options*
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
                (always
                 (validate-plan dpath ppath plp :verbose *verbose*))))))))

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
      ((list* "--debug-preprocessing" rest)
       (let ((*debug-preprocessing* t))
         (main rest)))
      ((list* "--use-grounded-macros" rest)
       (let ((*use-grounded-macros* t))
         (main rest)))
      ((list* "--use-reverse-macros" rest)
       (let ((*use-reverse-macros* t))
         (main rest)))
      ((list* "--use-grounded-reverse-macros" rest)
       (let ((*use-grounded-reverse-macros* t)
             (*use-reverse-macros* t))
         (main rest)))
      ((list* "--disable-filtering" rest)
       (let ((*disable-filtering* t))
         (main rest)))
      ;; underlying planner
      ((list* "--preprocessor"
              *preprocessor*
              *preprocessor-options* rest) (main rest))
      ((list* "--main-search"
              *main-search*
              *main-options* rest) (main rest))
      ;; shortcuts
      ((list* "--both-search"
              (and *preprocessor* *main-search*)
              (and *preprocessor-options* *main-options*) rest) (main rest))
      ((list* "--use-ff" rest)
       (main (list* "--preprocess-ff" "--main-search-ff" rest)))
      ((list* "--preprocess-ff" rest)
       (main (list* "--preprocessor" "macroff-clean" "" rest)))
      ((list* "--main-search-ff" rest)
       (main (list* "--main-search" "macroff-clean" "" rest)))
      ((list* "--use-marvin" rest)
       (main (list* "--preprocess-marvin" "--main-search-marvin" rest)))
      ((list* "--preprocess-marvin" rest)
       (main (list* "--preprocessor" "marvin-clean" "" rest)))
      ((list* "--main-search-marvin" rest)
       (main (list* "--main-search" "marvin-clean" "" rest)))
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
      (nil
       (format *error-output* "~&Usage: component-planner PROBLEM [DOMAIN]~
               ~%~@{~4t~40<~(~a~)~;~{~a ~}~> : ~@(~a~)~%~}"
               '-v nil "specify verbosity"
               '--validation nil "run the validator after the planning"
               '--debug-preprocessing nil "enable the verbosity of the preprocessing planner"
               '--preprocess-only nil "stops immediately when preprocess finishes"
               '--disable-filtering nil "disable ranking-based macro filtering"
               '--use-grounded-macros nil "use non-parametric fully grounded macro actions"
               '--use-reverse-macros nil "use reverse macros"
               '--use-grounded-reverse-macros nil "use non-parametric reverse macros. implies --use-reverse-macros."
               '--preprocess-limit '(time) "specify the approximated sum of maxmimum preprocessing time in integer"
               '--component-plan-limit '(time) "specify the time limit of component planning in integer"
               '-t '(time) "time limit for the main search. NOT the total limit"
               '-m '(memory) "memory limit for the main search. NOT the total limit"
               '--------underlying-planner-options------ nil "-------------------------------"
               '--main-search '(string string) "specify the additional options given to the underlying planner."
               '--preprocessor '(string string) "specify the additional options given to the underlying planner."
               '-----------------shortcuts-------------- nil "-------------------------------"
               '--both-search '(string string) "specify the same config for --main-search and --preprocessor."
               '--preprocess-ff nil "use FF during preprocesssing (otherwise LAMA ipc 2011)"
               '--main-search-ff nil "use FF during main search (otherwise LAMA ipc 2011)"
               '--use-ff nil "both --preprocess-ff and --main-search-ff"
               '--preprocess-marvin nil "use MARVIN during preprocesssing (otherwise LAMA ipc 2011)"
               '--main-search-marvin nil "use MARVIN during main search (otherwise LAMA ipc 2011)"
               '--use-marvin nil "both --preprocess-marvin and --main-search-marvin")
       (format *error-output* "~%DOMAIN is by default domain.pddl in the same directory~%"))
      (_
       (format *error-output* "~%Invalid Arguments!")
       (main nil)
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

(defun fdtest-sat ()
  (clear-plan-task-cache)
  (test-problem "depot/p01.pddl" "depot/domain.pddl"
                :verbose t))

;;;; elevators

(defun test1-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "elevators-sat11/p01.pddl")))
(defun test1-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "elevators-sat11/p04.pddl")))
(defun test1-3 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "elevators-sat11/p08.pddl")))
(defun test1-4 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "elevators-sat11/p12.pddl")))

;;;; cell-assembly-noneg-nocost

(defun test2-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "cell-assembly-noneg-nocost/p01.pddl")))
(defun test2-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "cell-assembly-noneg-nocost/p04.pddl")))
(defun test2-3 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "cell-assembly-noneg-nocost/p08.pddl")))
(defun test2-4 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "cell-assembly-noneg-nocost/p12.pddl")))

;;;; pipesworld
(defun test3-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "pipesworld-notankage/p01.pddl")))
(defun test3-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "pipesworld-notankage/p04.pddl")))

(defun test4-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "pipesworld-tankage/p01.pddl")))
(defun test4-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "pipesworld-tankage/p04.pddl")))

;;;; barman

(defun test5-1 (&rest args)
  (clear-plan-task-cache) ;; かなり難しい
  (main `(,@args "--validation" "-v" "-m" "2000000" "barman/p01.pddl")))
(defun test5-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "barman/p04.pddl")))
(defun test5-3 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "barman/p08.pddl")))

;;;; childsnack

(defun test6-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "childsnack/p01.pddl")))
(defun test6-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "childsnack/p04.pddl")))


;;;; woodworking

(defun test7-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "woodworking-sat11-nocost/p01.pddl")))

(defun test7-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "woodworking-sat11-nocost/p04.pddl")))

;;;; satellite

(defun test8-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "satellite-typed2/p01.pddl")))

(defun test8-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "satellite-typed2/p04.pddl")))

(defun test8-3 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "satellite-typed2/p01-hard.pddl")))

;;;; rovers

(defun test9-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "rovers/p01.pddl")))

(defun test9-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "rovers/p04.pddl")))

(defun test9-3 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "rovers/p41.pddl")))

(defun test9-1* (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "rovers/p01.pddl" "rovers/domain-fixed.pddl")))

(defun test9-2* (&rest args)
  (clear-plan-task-cache)
  (main `(,@args "--validation" "-v" "-m" "2000000" "rovers/p04.pddl" "rovers/domain-fixed.pddl")))

