(defpackage :pddl.component-planner.experiment
  (:nicknames :experiment)
  (:use :cl :cl-rlimit :pddl :pddl.component-planner :trivia
        :alexandria :iterate :guicho-utilities)
  (:shadowing-import-from :pddl :minimize :maximize))
(in-package :pddl.component-planner.experiment)

(defvar *build-date*
    (multiple-value-bind (second minute hour date month year) (get-decoded-time)
      (format nil "~2,,,'0@a:~2,,,'0@a:~2,,,'0@a ~2,,,'0@a/~2,,,'0@a, ~a"
              hour minute second month date year)))

(defun main (&rest argv)
  (uiop:quit
   (if (parse argv) 0 1)))

(defun consume-until-hyphen (list next)
  (labels ((rec (acc list)
             (ematch list
               ((list* "-" rest)
                (funcall next (format nil "'~{~a~^ ~}'" (nreverse acc)) rest))
               ((list* string rest)
                (rec (cons string acc) rest)))))
    (rec nil list)))

(defun parse (argv)
  (when *verbose*
    (print argv))
  (let ((*package* (find-package :pddl.component-planner.experiment)))
    (match argv
      ;; debug options
      ((list* "-v" rest)
       (setf *verbose* t)
       (parse rest))
      ((list* "--validation" rest)
       (setf *validation* t)
       (parse rest))
      ((list* "--debug-preprocessing" rest)
       (setf *debug-preprocessing* t)
       (parse rest))

      ;; run mode options
      ((list* "--preprocess-only" rest)
       (format t "~&; Preprocessing-only mode was activated")
       (format t "~&; CAP does not run the main planner.")
       (setf *preprocess-only* t)
       (parse rest))
      ((list* "--plain" rest)
       (format t "~&; Plain mode was activated, CAP runs only the main planner.")
       (setf *use-plain-planner* t)
       (parse rest))
      ((list "--reformat" path)
       (format t "~&; Loading the pddl file and reformatting the result to stdout")
       (reformat-pddl path))
      ((list* "--training" path rest)
       (format t "~&; Copy the training instances ~a (effective only under --plain)" path)
       (setf *training-instances* (cons (pathname path) *training-instances*))
       (parse rest))
      
      ;; time limit and resource
      ((list* "--preprocess-limit" time rest)
       (setf *preprocess-time-limit* (parse-integer time))
       (parse rest))
      ((list* "--component-plan-limit" time rest)
       (setf *component-plan-time-limit* (parse-integer time))
       (parse rest))
      ((list* "--component-plan-memory-limit" memory rest)
       (setf *component-plan-memory-limit* (parse-integer memory))
       (parse rest))

      ((list* "--iterative-resource" rest)
       (setf *iterative-resource* t)
       (parse rest))

      ((list* "-t" time rest)
       (setf *hard-time-limit* (parse-integer time))
       (parse rest))
      ((list* "-m" memory rest)
       (setf *memory-limit* (parse-integer memory))
       (setf *component-plan-memory-limit* *memory-limit*)
       (parse rest))

      ;; CAP search options
      ((list* "--compatibility" rest)
       (setf *compatibility* :strict)
       (parse rest))
      ((list* "--force-lifted" rest)
       (setf *ground-macros* nil)
       (parse rest))
      #+nil
      ((list* "--precategorization" rest)
       (setf *precategorization* t)
       (parse rest))
      ((list* "--binarization" rest)
       (setf *binarization* t)
       (parse rest))
      ((list* "--component-abstraction" rest)
       (setf *component-abstraction* t)
       (parse rest))
      ((list* "--force-variable-factoring" rest)
       (setf *variable-factoring* t)
       (parse rest))
      ((list* "--cyclic-macros" rest)
       (setf *cyclic-macros* t)
       (parse rest))
      ((list* "--iterated" rest)
       (setf *iterated* t)
       (parse rest))

      ;; concurrency option
      ((list* "--threads" num rest)
       (setf *num-threads* (parse-integer num))
       (parse rest))

      ((list* "--ipc-threads" rest)
       (parse (list* "--threads" "4" rest)))

      ((list* "--cfs" rest)
       (setf *rely-on-cfs* t)
       (parse rest))

      ;; cost options
      ((list* "--add-macro-cost" rest)
       (setf *add-macro-cost* t)
       (parse rest))
      ((list* "--remove-main-problem-cost" rest)
       (setf *remove-main-problem-cost* t)
       (parse rest))
      ((list* "--remove-component-problem-cost" rest)
       (setf *remove-component-problem-cost* t)
       (parse rest))

      ;; not used at all now
      #+nil
      ((list* "--filtering-threashold" th rest)
       (let ((*threshold* (read-from-string th)))
         (if (numberp *threshold*)
             (if (<= 0 *threshold* 0.99999995)
                 (parse rest)
                 (error "--filtering-threashold should be 0 <= x < 0.99999995 ~~ 1-eps! "))
             (error "--filtering-threashold should be a lisp-readable number! ex) 0, 0.0, 1/2, 0.5d0, 0.7"))))

      ((list* "--preprocessor" planner rest)
       (setf *preprocessor* planner)
       (consume-until-hyphen
        rest
        (lambda (options rest)
          (setf *preprocessor-options* options)
          (parse rest))))
      ((list* "--main-search" planner rest)
       (setf *main-search* planner)
       (consume-until-hyphen
        rest
        (lambda (options rest)
          (setf *main-options* options)
          (parse rest))))

      ;; aliases
      ((list* "--both-search" searcher rest)
       (consume-until-hyphen
        rest
        (lambda (options rest)
          (setf *main-search* searcher
                *preprocessor* searcher
                *main-options* options
                *preprocessor-options* options)
          (parse rest))))

      ;; find the problem files
      ((list ppath)
       (solve (merge-pathnames ppath)))
      ((list ppath dpath)
         (format t "~%; Build date : ~a~%" *build-date*)
       (solve (merge-pathnames ppath)
              (merge-pathnames dpath)))
      (nil
       (format *error-output* "~&Usage: component-planner PROBLEM [DOMAIN]~
               ~%~@{~4t~40<~(~a~)~;~{~a ~}~> : ~@(~a~)~%~}"
               '-----------------debug-options---------- nil "-------------------------------"
               '-v nil "Become more verbose"
               '--validation nil "run the validator after the planning"
               '--debug-preprocessing nil "enable the verbosity of the preprocessing planner"
               '--------------run-mode-options---------- nil "-------------------------------"
               '--plain nil "Use the plain underlying planner specified by --main-search."
               '--preprocess-only nil "stops immediately when preprocess finishes"
               '--reformat '(path) "Loading the pddl file and reformatting the result to stdout"
               '--training '(path) "Also copy the training instance (effective only under --plain, for SOL-EP)"
               '----------computational-resource-------- nil "-------------------------------"
               '-t '(sec) "time limit for the main search. NOT the total limit"
               '-m '(memory-in-kb) "memory limit for main search and subproblems. NOT the total limit"
               '--preprocess-limit '(sec) "max total preprocessing time."
               '--component-plan-limit '(sec) "time limit for solving subproblems (default: 30)"
               '--component-plan-memory-limit '(memory-in-kb) "Override the memory limit for subproblems."
               '--iterative-resource nil "Enable iterative resource limit. This ignores --compatibility and --component-plan-limit"
               '----------------CAP-options------------- nil "-------------------------------"
               ;; not used at all now
               ;; '--filtering-threashold '(threashold)
               ;; "set the threashold in macro filtering, 0 by default. Should be a number in [0,0.99)"
               '--component-abstraction nil "Enables component abstraction."
               '--binarization nil "Decompose multiary predicates into binary predicates during component abstraction."
               '--cyclic-macros nil "Enables cyclic macros: additional preprocessing time"
               '--compatibility nil "Enables compatibility-based pruning."
               '--force-variable-factoring nil "Emulates Domshrak's Factor=Variable method"
               '--force-lifted nil "Disables the macro-action grounding."
               ;; true by default 
               ;; '--precategorization nil "Do not apply precategorization before compatibility checking."
               ;; now on/off only. once enabled, it uses "loose"
               ;; '--compatibility-type '(symbol) "specify the result of combatibility when no component plan exists. One of: strict(default), loose, always-false(=disabling compat-check)."
               '--iterated nil "Specify if the main search should run an iterated search (in case of FD/LAMA)."
               '------------concurrency-options--------- nil "-------------------------------"
               '--threads '(num) "specify the number of threads to solve subproblems. Default: 1"
               '--ipc-threads nil "alias to --threads 4, for IPC MultiCore track."
               '--cfs '() "Spawn as many threads and use Linux's Completely Fair Scheduler to balance threaded executions. Default: disabled"
               '--------underlying-planner-options------ nil "-------------------------------"
               '--main-search '(planner strings... -) "Specify MainPlanner. Options end with a \"-\"."
               '--preprocessor '(planner string... -) "Specify ComponentPlanner. Options end with a \"-\"."
               "" nil "Where PLANNER is one of: fd-clean,ff-clean,probe-clean,"
               "" nil "marvin1-clean,marvin2-clean,lpg-clean,mp-clean."
               '-------planner-compatibility-options---- nil "-------------------------------"
               '--add-macro-cost nil "Unit-cost domains are converted into action-cost domains. In those domains macro actions are then given a cost same as its length."
               '--remove-component-problem-cost nil "Remove :action-costs during component planning."
               '--remove-main-problem-cost nil "Remove :action-costs during main search. This option supersedes --add-macro-cost."
               '-------------shortcuts/aliases---------- nil "-------------------------------"
               '--both-search '(string string) "specify the same config for --main-search and --preprocessor.")
       (format *error-output* "~%DOMAIN is by default domain.pddl, or [problemname]-domain.pddl in the same directory")
       (format *error-output* "~%Build date : ~a" *build-date*)
       (format *error-output* "~%Foreign library directories : ~a" cffi:*foreign-library-directories*)
       (terpri *error-output*))
      (_
       (format *error-output* "~%Invalid Arguments!~%")
       (parse nil)
       (error "~&Invalid Arguments!~2%")))))

