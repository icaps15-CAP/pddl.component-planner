
(defpackage :pddl.component-planner.experiment
  (:nicknames :experiment)
  (:use :cl :cl-rlimit :pddl :pddl.component-planner :optima
        :alexandria :iterate :guicho-utilities)
  (:shadowing-import-from :pddl :minimize :maximize)
  (:export
   #:main))
(in-package :pddl.component-planner.experiment)

(defvar *build-date*
    (multiple-value-bind (second minute hour date month year) (get-decoded-time)
      (format nil "~2,,,'0@a:~2,,,'0@a:~2,,,'0@a ~2,,,'0@a/~2,,,'0@a, ~a"
              hour minute second month date year)))

(defun toplevel ()
  (sb-ext:disable-debugger)
  (uiop:quit
   (if (main) 0 1)))

(defun consume-until-hyphen (list next)
  (labels ((rec (acc list)
             (ematch list
               ((list* "-" rest)
                (funcall next (format nil "~{~a~^ ~}" (nreverse acc)) rest))
               ((list* string rest)
                (rec (cons string acc) rest)))))
    (rec nil list)))

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
      ((list* "--component-plan-memory-limit" memory rest)
       (let ((*component-plan-memory-limit* (parse-integer memory)))
         (main rest)))
      ((list* "--iterative-resource" rest)
       (let ((*iterative-resource* t))
         (main rest)))
      ((list* "-t" time rest)
       (let ((*hard-time-limit* (parse-integer time)))
         (main rest)))
      ((list* "-m" memory rest)
       (let* ((*memory-limit* (parse-integer memory))
              (*component-plan-memory-limit* *memory-limit*))
         (main rest)))

      ;; CAP search options
      ((list* "--compatibility" rest)
       (let ((*compatibility* :strict))
         (main rest)))
      ((list* "--force-lifted" rest)
       (let ((*ground-macros* nil))
         (main rest)))
      #+nil
      ((list* "--precategorization" rest)
       (let ((*precategorization* t))
         (main rest)))
      ((list* "--binarization" rest)
       (let ((*binarization* t))
         (main rest)))
      ((list* "--component-abstraction" rest)
       (let ((*component-abstraction* t))
         (main rest)))
      ((list* "--force-variable-factoring" rest)
       (let ((*variable-factoring* t))
         (main rest)))
      ((list* "--cyclic-macros" rest)
       (let ((*cyclic-macros* t))
         (main rest)))
      ((list* "--iterated" rest)
       (let ((*iterated* t))
         (main rest)))

      ;; cost options
      ((list* "--add-macro-cost" rest)
       (let ((*add-macro-cost* t))
         (main rest)))
      ((list* "--remove-main-problem-cost" rest)
       (let ((*remove-main-problem-cost* t))
         (main rest)))
      ((list* "--remove-component-problem-cost" rest)
       (let ((*remove-component-problem-cost* t))
         (main rest)))

      ;; not used at all now
      #+nil
      ((list* "--filtering-threashold" th rest)
       (let ((*threshold* (read-from-string th)))
         (if (numberp *threshold*)
             (if (<= 0 *threshold* 0.99999995)
                 (main rest)
                 (error "--filtering-threashold should be 0 <= x < 0.99999995 ~~ 1-eps! "))
             (error "--filtering-threashold should be a lisp-readable number! ex) 0, 0.0, 1/2, 0.5d0, 0.7"))))

      ((list* "--preprocessor" *preprocessor* rest)
       (consume-until-hyphen
        rest
        (lambda (*preprocessor-options* rest)
          (main rest))))
      ((list* "--main-search" *main-search* rest)
       (consume-until-hyphen
        rest
        (lambda (*main-options* rest)
          (main rest))))

      ;; aliases
      ((list* "--both-search" searcher rest)
       (consume-until-hyphen
        rest
        (lambda (options rest)
          (let ((*main-search* searcher)
                (*preprocessor* searcher)
                (*main-options* options)
                (*preprocessor-options* options))
            (main rest)))))

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

