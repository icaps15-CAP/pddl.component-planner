
(defpackage :pddl.component-planner.experiment
  (:use :cl :cl-rlimit :pddl :pddl.component-planner :optima
        :alexandria :iterate :guicho-utilities)
  (:shadowing-import-from :pddl :minimize :maximize))
(in-package :pddl.component-planner.experiment)

(defvar *build-date*
    (multiple-value-bind (second minute hour date month year) (get-decoded-time)
      (format nil "~2,,,'0@a:~2,,,'0@a:~2,,,'0@a ~2,,,'0@a/~2,,,'0@a, ~a"
              hour minute second month date year)))

(defun toplevel ()
  (sb-ext:disable-debugger)
  (main))

(defun main (&optional (argv (cdr sb-ext:*posix-argv*)))
  (when *verbose*
    (print argv))
  (let ((*package* (find-package :pddl.component-planner.experiment)))
    (match argv
      ;; debug options
      ((list* "-v" rest)
       (let ((*verbose* t))
         (main rest)))
      ((list* "--validation" rest)
       (let ((*validation* t))
         (main rest)))
      ((list* "--debug-preprocessing" rest)
       (let ((*debug-preprocessing* t))
         (main rest)))

      ;; run mode options
      ((list* "--preprocess-only" rest)
       (format t "~&; Preprocessing-only mode was activated")
       (format t "~&; CAP does not run the main planner.")
       (let ((*preprocess-only* t))
         (main rest)))
      ((list* "--plain" rest)
       (format t "~&; Plain mode was activated, CAP runs only the main planner.")
       (let ((*use-plain-planner* t)) (main rest)))
      ((list "--reformat" path)
       (format t "~&; Loading the pddl file and reformatting the result to stdout")
       (reformat-pddl path))
      ((list* "--training" path rest)
       (format t "~&; Copy the training instances ~a (effective only under --plain)" path)
       (let ((*training-instances*
              (cons (pathname path) *training-instances*)))
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

      ;; CAP search options
      ((list* "--compatibility-type" string rest)
       (let ((*compatibility-type* (read-from-string string)))
         (main rest)))
      ((list* "--disable-precategorization" rest)
       (let ((*disable-precategorization* t))
         (main rest)))
      ((list* "--disable-binarization" rest)
       (let ((*disable-binarization* t))
         (main rest)))
      ((list* "--disable-cyclic-macros" rest)
       (let ((*disable-cyclic-macros* t))
         (main rest)))
      ((list* "--iterated" rest)
       (let ((*iterated* t))
         (main rest)))
      ((list* "--remove-component-problem-cost" rest)
       (let ((*remove-component-problem-cost* t))
         (main rest)))
      ((list* "--remove-main-problem-cost" rest)
       (let ((*remove-main-problem-cost* t))
         (main rest)))
      ((list* "--filtering-threashold" th rest)
       (let ((*threshold* (read-from-string th)))
         (if (numberp *threshold*)
             (if (<= 0 *threshold* 0.99999995)
                 (main rest)
                 (error "--filtering-threashold should be 0 <= x < 0.99999995 ~~ 1-eps! "))
             (error "--filtering-threashold should be a lisp-readable number! ex) 0, 0.0, 1/2, 0.5d0, 0.7"))))
      ((list* "--disable-filtering" rest)
       (let ((*threshold* 0))
         (main rest)))
      ((list* "--preprocessor" *preprocessor* "-" rest)
       (main (list* "--preprocessor" *preprocessor* "" rest)))
      ((list* "--preprocessor"
              *preprocessor*
              *preprocessor-options* rest) (main rest))
      ((list* "--main-search" *main-search* "-" rest)
       (main (list* "--main-search" *main-search* "" rest)))
      ((list* "--main-search"
              *main-search*
              *main-options* rest) (main rest))

      ;; aliases
      ((list* "--both-search" searcher option rest)
       (main (list* "--preprocessor" searcher option
                    "--main-search" searcher option rest)))
      ((list* "--default" rest)
       (main (list* "--disable-filtering"
                    "--preprocess-limit"
                    (princ-to-string MOST-POSITIVE-FIXNUM)
                    "-v"
                    rest)))
      ((list* "--fffd" rest)
       (main (list* "--default" "--preprocess-ff" rest)))
      ;;;; ff
      ((list* "--plain-ff" rest)
       (main (list* "--plain" "--main-search-ff" rest)))
      ((list* "--use-ff" rest)
       (main (list* "--preprocess-ff" "--main-search-ff" rest)))
      ((list* "--preprocess-ff" rest)
       (main (list* "--remove-component-problem-cost"
                    "--preprocessor" "ff-clean" "" rest)))
      ((list* "--main-search-ff" rest)
       (main (list* "--remove-main-problem-cost"
                    "--main-search" "ff-clean" "" rest)))

      ;; find the problem files
      ((list ppath)
       (let* ((ppath (merge-pathnames ppath)))
         (main (list ppath (find-domain ppath)))))
      ((list ppath dpath)
       (let ((ppath (merge-pathnames ppath))
             (dpath (merge-pathnames dpath)))
         (format t "~%; Build date : ~a~%" *build-date*)
         (solve ppath dpath)))
      (nil
       (format *error-output* "~&Usage: component-planner PROBLEM [DOMAIN]~
               ~%~@{~4t~40<~(~a~)~;~{~a ~}~> : ~@(~a~)~%~}"
               '-----------------debug-options---------- nil "-------------------------------"
               '-v nil "specify verbosity"
               '--validation nil "run the validator after the planning"
               '--debug-preprocessing nil "enable the verbosity of the preprocessing planner"
               '--------------run-mode-options---------- nil "-------------------------------"
               '--plain nil "Use the plain underlying planner specified by --main-search."
               '--preprocess-only nil "stops immediately when preprocess finishes"
               '--reformat '(path) "Loading the pddl file and reformatting the result to stdout"
               '--training '(path) "Also copy the training instance (effective only under --plain, for SOL-EP)"
               '----------computational-resource-------- nil "-------------------------------"
               '-t '(time) "time limit for the main search. NOT the total limit"
               '-m '(memory) "memory limit for the main search. NOT the total limit"
               '--preprocess-limit '(time) "specify the approximated sum of maxmimum preprocessing time in integer"
               '--component-plan-limit '(time) "specify the time limit of component planning in integer (default 30sec)"
               '----------------CAP-options------------- nil "-------------------------------"
               '--filtering-threashold '(threashold)
               "set the threashold in macro filtering, 0.8 by default. Should be a number in [0,0.99)"
               '--remove-component-problem-cost nil "Remove :action-costs during component planning"
               '--remove-main-problem-cost nil "Remove :action-costs during main search"
               '--disable-binarization nil "Do not use binarized domain for component abstraction."
               '--disable-cyclic-macros nil "Disable computing cyclic macros, always use forward-macros"
               '--disable-precategorization nil "Do not apply precategorization before compatibility checking."
               '--compatibility-type '(symbol) "specify the result of combatibility when no component plan exists. One of: strict(default), loose, always-false(=disabling compat-check)."
               '--iterated nil "Specify if the main search should run an iterated search (in case of FD/LAMA)."
               '--------underlying-planner-options------ nil "-------------------------------"
               '--main-search '(string string) "specify the options given to the MainPlanner. \"-\" means \"no options\"."
               '--preprocessor '(string string) "specify the options given to the ComponentPlanner. \"-\" means \"no options\"."
               '-------------shortcuts/aliases---------- nil "-------------------------------"
               '--disable-filtering nil "Same as specifying --filtering-threashold 0 ."
               '--plain-ff nil "Use plain FF."
               '--fffd nil "Use FF + LAMA combination."
               '--default nil "same as --disable-filtering, --preprocess-limit MOST-POSITIVE-FIXNUM -v"
               '--both-search '(string string) "specify the same config for --main-search and --preprocessor."
               '--preprocess-ff nil "use FF during preprocesssing (otherwise LAMA ipc 2011)"
               '--main-search-ff nil "use FF during main search (otherwise LAMA ipc 2011)"
               '--use-ff nil "both --preprocess-ff and --main-search-ff")
       (format *error-output* "~%DOMAIN is by default domain.pddl in the same directory")
       (format *error-output* "~%Build date : ~a~%" *build-date*))
      (_
       (format *error-output* "~%Invalid Arguments!~%")
       (main nil)
       (error "~&Invalid Arguments!~2%")))))

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

(defun reformat-test (&rest args)
  (main `(,@args "--reformat" "transport-sat11-strips/domain.pddl"))
  (main `(,@args "--reformat" "transport-sat11-strips/p01.pddl"))
  (main `(,@args "--remove-main-problem-cost" "--reformat" "transport-sat11-strips/domain.pddl"))
  (main `(,@args "--remove-main-problem-cost" "--reformat" "transport-sat11-strips/p01.pddl")))

;;;; elevators

;; (defun test1-1 (&rest args)
;;   (clear-plan-task-cache)
;;   (main `(,@args  "--default" "--fffd" "-m" "2000000" "elevators-sat11/p01.pddl")))
;; (defun test1-2 (&rest args)
;;   (clear-plan-task-cache)
;;   (main `(,@args  "--default" "--fffd" "-m" "2000000" "elevators-sat11/p04.pddl")))
;; (defun test1-3 (&rest args)
;;   (clear-plan-task-cache)
;;   (main `(,@args  "--default" "--fffd" "-m" "2000000" "elevators-sat11/p08.pddl")))
;; (defun test1-4 (&rest args)
;;   (clear-plan-task-cache)
;;   (main `(,@args  "--default" "--fffd" "-m" "2000000" "elevators-sat11/p12.pddl")))

(defun test1-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "elev-lessfloors/p01.pddl")))
(defun test1-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "elev-lessfloors/p02.pddl")))
(defun test1-3 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "elev-lessfloors/p03.pddl")))
(defun test1-4 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "elev-lessfloors/p04.pddl")))

;;;; cell-assembly-noneg-nocost

(defun test2-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "cell-assembly-noneg-nocost/p01.pddl")))
(defun test2-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "cell-assembly-noneg-nocost/p04.pddl")))
(defun test2-3 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "cell-assembly-noneg-nocost/p08.pddl")))
(defun test2-4 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "cell-assembly-noneg-nocost/p12.pddl")))

;;;; pipesworld
(defun test3-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "pipesworld-notankage/p01.pddl")))
(defun test3-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "pipesworld-notankage/p04.pddl")))

(defun test4-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "pipesworld-tankage/p01.pddl")))
(defun test4-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "pipesworld-tankage/p04.pddl")))

;;;; barman

(defun test5-1 (&rest args)
  (clear-plan-task-cache) ;; かなり難しい
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "barman/p01.pddl")))
(defun test5-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "barman/p04.pddl")))
(defun test5-3 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "barman/p08.pddl")))

;;;; childsnack

(defun test6-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "childsnack/p01.pddl")))
(defun test6-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "childsnack/p04.pddl")))


;;;; woodworking

(defun test7-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "woodworking-sat11-nocost/p01.pddl")))
(defun test7-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "woodworking-sat11-nocost/p02.pddl")))
(defun test7-3 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "woodworking-sat11-nocost/p04.pddl")))

;;;; satellite

(defun test8-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "satellite-typed2/p01.pddl")))

(defun test8-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "satellite-typed2/p04.pddl")))

(defun test8-3 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "satellite-typed2/p01-hard.pddl")))

;;;; rovers

(defun test9-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "rovers/p01.pddl")))

(defun test9-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "rovers/p04.pddl")))

(defun test9-3 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "rovers/p41.pddl")))

(defun test9-1* (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "rovers/p01.pddl" "rovers/domain-fixed.pddl")))

(defun test9-2* (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "rovers/p04.pddl" "rovers/domain-fixed.pddl")))


;;;; transport-sat11-strips

(defun test10-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "transport-sat11-strips/p01.pddl")))

(defun test10-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "transport-sat11-strips/p10.pddl")))

(defun test10-3 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "--default" "--fffd" "-m" "2000000" "transport-sat11-strips/p20.pddl")))


