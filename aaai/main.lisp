
(defpackage :pddl.component-planner.experiment
  (:use :cl :cl-rlimit :pddl :pddl.component-planner :optima
        :alexandria :iterate)
  (:shadowing-import-from :pddl :minimize :maximize))
(in-package :pddl.component-planner.experiment)

(defmacro suppress (&body body)
  `(handler-bind ((warning #'muffle-warning))
     ,@body))

(defvar *verbose* nil)
(defvar *build-date*
    (multiple-value-bind (second minute hour date month year) (get-decoded-time)
      (format nil "~2,,,'0@a:~2,,,'0@a:~2,,,'0@a ~2,,,'0@a/~2,,,'0@a, ~a"
              hour minute second month date year)))

(defparameter *lmcut-lazy-gbfs* (wrap-option "--search lazy_greedy(lmcut())"))
(defparameter *lmcut-eager-gbfs* (wrap-option "--search eager_greedy(lmcut())"))
(defvar *use-plain-planner* nil)
(defun solve (ppath dpath)
  (format t "~%Build date : ~a~%" *build-date*)
  (multiple-value-bind (dname domain) (suppress (parse-file dpath nil t))
    (multiple-value-bind (pname problem) (suppress (parse-file ppath nil t))
      (print dname)
      (print domain)
      (print pname)
      (print problem)
      (let ((plans
             (if *use-plain-planner*
                 (mapcar (curry #'pddl-plan
                                :domain domain :problem problem :path)
                         (test-problem-common ppath dpath
                                              :name *main-search*
                                              :options *main-options*
                                              :verbose *verbose*))
                 (solve-problem-enhancing problem
                                          :time-limit 1 ; satisficing
                                          :name *main-search*
                                          :options *main-options*
                                          :verbose *verbose*))))
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

(defun toplevel ()
  (sb-ext:disable-debugger)
  (main))

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
      ;; underlying planner
      ((list* "--plain" rest)
       (let ((*use-plain-planner* t)) (main rest)))
      ((list* "--preprocessor"
              *preprocessor*
              *preprocessor-options* rest) (main rest))
      ((list* "--main-search"
              *main-search*
              *main-options* rest) (main rest))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; aliases ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ((list* "--both-search"
              (and *preprocessor* *main-search*)
              (and *preprocessor-options* *main-options*) rest)
       (main rest))
      ((list* "--default" rest)
       (main (list* "--disable-filtering"
                    "--preprocess-limit"
                    (princ-to-string MOST-POSITIVE-FIXNUM)
                    "-v"
                    rest)))
      ((list* "--fffd" rest)
       (main (list* "--default" "--preprocess-ff" rest)))
      ;;;; ff
      ((list* "--use-ff" rest)
       (main (list* "--preprocess-ff" "--main-search-ff" rest)))
      ((list* "--preprocess-ff" rest)
       (main (list* "--remove-component-problem-cost"
                    "--preprocessor" "ff-clean" "" rest)))
      ((list* "--main-search-ff" rest)
       (main (list* "--remove-main-problem-cost"
                    "--main-search" "ff-clean" "" rest)))
      ;;;; marvin
      ((list* "--use-marvin" rest)
       (main (list* "--preprocess-marvin" "--main-search-marvin" rest)))
      ((list* "--preprocess-marvin" rest)
       (main (list* "--remove-component-problem-cost"
                    "--preprocessor" "marvin-clean" "" rest)))
      ((list* "--main-search-marvin" rest)
       (main (list* "--remove-main-problem-cost"
                    "--main-search" "marvin-clean" "" rest)))
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
      ;; prosessing the problem files
      ((list ppath)
       (let* ((ppath (merge-pathnames ppath))
              (dpath (make-pathname :defaults ppath :name "domain")))
         (if (probe-file dpath)
             (main (list ppath dpath))
             (let ((dpath (make-pathname
                           :defaults ppath :name
                           (format nil "~a-domain" (pathname-name ppath)))))
               (main (list ppath dpath))))))
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
               '--filtering-threashold '(threashold)
               "set the threashold in macro filtering, 0.8 by default. Should be a number in [0,0.99)"
               '--remove-component-problem-cost nil "Remove :action-costs during component planning"
               '--remove-main-problem-cost nil "Remove :action-costs during main search"
               '--disable-filtering nil "Same as specifying --filtering-threashold 0 ."
               '--disable-binarization nil "Do not use binarized domain for component abstraction."
               '--disable-cyclic-macros nil "Disable computing cyclic macros, always use forward-macros"
               '--disable-precategorization nil "Do not apply precategorization before compatibility checking."
               '--preprocess-limit '(time) "specify the approximated sum of maxmimum preprocessing time in integer"
               '--component-plan-limit '(time) "specify the time limit of component planning in integer (default 30sec)"
               '--compatibility-type '(symbol) "specify the result of combatibility when no component plan exists. One of: strict(default), loose, always-false(=disabling compat-check)."
               '-t '(time) "time limit for the main search. NOT the total limit"
               '-m '(memory) "memory limit for the main search. NOT the total limit"
               '--------underlying-planner-options------ nil "-------------------------------"
               '--plain nil "Use the plain underlying planner specified by --main-search."
               '--main-search '(string string) "specify the additional options given to the underlying planner."
               '--preprocessor '(string string) "specify the additional options given to the underlying planner."
               '-----------------shortcuts-------------- nil "-------------------------------"
               '--plain-ff nil "Use plain FF."
               '--both-search '(string string) "specify the same config for --main-search and --preprocessor."
               '--preprocess-ff nil "use FF during preprocesssing (otherwise LAMA ipc 2011)"
               '--main-search-ff nil "use FF during main search (otherwise LAMA ipc 2011)"
               '--use-ff nil "both --preprocess-ff and --main-search-ff"
               '--preprocess-marvin nil "use MARVIN during preprocesssing (otherwise LAMA ipc 2011)"
               '--main-search-marvin nil "use MARVIN during main search (otherwise LAMA ipc 2011)"
               '--use-marvin nil "both --preprocess-marvin and --main-search-marvin")
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

;;;; elevators

;; (defun test1-1 (&rest args)
;;   (clear-plan-task-cache)
;;   (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "elevators-sat11/p01.pddl")))
;; (defun test1-2 (&rest args)
;;   (clear-plan-task-cache)
;;   (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "elevators-sat11/p04.pddl")))
;; (defun test1-3 (&rest args)
;;   (clear-plan-task-cache)
;;   (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "elevators-sat11/p08.pddl")))
;; (defun test1-4 (&rest args)
;;   (clear-plan-task-cache)
;;   (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "elevators-sat11/p12.pddl")))

(defun test1-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "elev-lessfloors/p01.pddl")))
(defun test1-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "elev-lessfloors/p02.pddl")))
(defun test1-3 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "elev-lessfloors/p03.pddl")))
(defun test1-4 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "elev-lessfloors/p04.pddl")))

;;;; cell-assembly-noneg-nocost

(defun test2-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "cell-assembly-noneg-nocost/p01.pddl")))
(defun test2-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "cell-assembly-noneg-nocost/p04.pddl")))
(defun test2-3 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "cell-assembly-noneg-nocost/p08.pddl")))
(defun test2-4 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "cell-assembly-noneg-nocost/p12.pddl")))

;;;; pipesworld
(defun test3-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "pipesworld-notankage/p01.pddl")))
(defun test3-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "pipesworld-notankage/p04.pddl")))

(defun test4-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "pipesworld-tankage/p01.pddl")))
(defun test4-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "pipesworld-tankage/p04.pddl")))

;;;; barman

(defun test5-1 (&rest args)
  (clear-plan-task-cache) ;; かなり難しい
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "barman/p01.pddl")))
(defun test5-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "barman/p04.pddl")))
(defun test5-3 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "barman/p08.pddl")))

;;;; childsnack

(defun test6-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "childsnack/p01.pddl")))
(defun test6-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "childsnack/p04.pddl")))


;;;; woodworking

(defun test7-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "woodworking-sat11-nocost/p01.pddl")))
(defun test7-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "woodworking-sat11-nocost/p02.pddl")))
(defun test7-3 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "woodworking-sat11-nocost/p04.pddl")))

;;;; satellite

(defun test8-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "satellite-typed2/p01.pddl")))

(defun test8-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "satellite-typed2/p04.pddl")))

(defun test8-3 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "satellite-typed2/p01-hard.pddl")))

;;;; rovers

(defun test9-1 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "rovers/p01.pddl")))

(defun test9-2 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "rovers/p04.pddl")))

(defun test9-3 (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "rovers/p41.pddl")))

(defun test9-1* (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "rovers/p01.pddl" "rovers/domain-fixed.pddl")))

(defun test9-2* (&rest args)
  (clear-plan-task-cache)
  (main `(,@args  "-v" "--disable-filtering" "--preprocess-ff" "-m" "2000000" "rovers/p04.pddl" "rovers/domain-fixed.pddl")))

