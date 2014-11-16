
(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

(defmacro suppress (&body body)
  `(handler-bind ((warning #'muffle-warning))
     ,@body))
@export
(defvar *verbose* nil)
@export
(defvar *iterated* nil)
@export
(defvar *use-plain-planner* nil)
@export
(defun solve (ppath dpath)
  (let ((*start* (get-universal-time)))
    (unwind-protect
        (multiple-value-bind (dname domain) (suppress (parse-file dpath nil t))
          (multiple-value-bind (pname problem) (suppress (parse-file ppath nil t))
            (print dname)
            (print domain)
            (print pname)
            (print problem)
            (if *use-plain-planner*
                (plan-plain ppath domain problem)
                (let ((plans
                       (solve-problem-enhancing problem
                                                :time-limit 1 ; satisficing
                                                :name *main-search*
                                                :options *main-options*
                                                :verbose *verbose*
                                                :iterated *iterated*)))
                  (iter (for plan in plans)
                        (for i from 1)
                        (for plp =
                             (merge-pathnames
                              (format nil "~a.plan.~a"
                                      (pathname-name ppath) i)))
                        (when (probe-file plp) (delete-file plp))
                        (write-plan plan plp *default-pathname-defaults* t)
                        (when *validation*
                          (always
                           (validate-plan dpath ppath plp :verbose *verbose*))))))))
      (format t "~&Wall time: ~a sec"
              (- (get-universal-time) *start*)))))

(defun plan-plain (ppath *domain* *problem*)
  (let ((dir (mktemp "plain")))
    (let ((plans
           (test-problem-common
            (write-pddl (if *remove-main-problem-cost*
                            (remove-costs *problem*)
                            *problem*)
                        "problem.pddl" dir)
            (write-pddl (if *remove-main-problem-cost*
                            (remove-costs *domain*)
                            *domain*)
                        "domain.pddl" dir)
            :name *main-search*
            :options *main-options*
            :verbose *verbose*
            :iterated *iterated*)))
      (iter (for path in plans)
            (for i from 1)
            (for new-path =
                 (merge-pathnames
                  (format nil "~a.plan.~a"
                          (pathname-name ppath) i)))
            (when (probe-file new-path) (delete-file new-path))
            (sb-ext:run-program
             "/bin/cp" (list (namestring path)
                             (namestring new-path)))))))

