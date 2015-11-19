
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

(defvar *num-threads* 1)

@export
(defun solve (ppath &optional (dpath (find-domain ppath)))
  (setf *start* (get-universal-time)
        *kernel* (make-kernel *num-threads*))
    (unwind-protect 
        (if *use-plain-planner*
            (plan-plain dpath ppath)
            (multiple-value-bind (dname domain) (suppress (parse-file dpath nil t))
              (multiple-value-bind (pname problem) (suppress (parse-file ppath nil t))
                (print dname)
                (print domain)
                (print pname)
                (print problem)
                (let ((plans
                       (solve-problem-enhancing problem
                                                :time-limit 1 ; satisficing
                                                :name *main-search*
                                                :options *main-options*
                                                :verbose *verbose*
                                                :iterated *iterated*)))
                  (and plans
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
                                (validate-plan dpath ppath plp :verbose *verbose*)))))))))
      (format t "~&Wall time: ~a sec~%"
              (- (get-universal-time) *start*))
    (end-kernel)))

@export
(defvar *training-instances* nil)

(defun just-copy-file (src dest)
  (ensure-directories-exist dest)
  (eazy-process:shell-command
   (format nil "cp ~a ~a" (namestring src) (namestring dest)) :verbose t)
  (namestring dest))

(defun plan-plain (dpath ppath)
  (let ((dir (mktemp "plain")))
    (let ((plans
           (handler-bind ((unix-signal
                           (lambda (c)
                             (format t "~&main search terminated")
                             (invoke-restart
                              (find-restart 'pddl:finish c)))))
             (when *training-instances*
               (iter (for tppath in *training-instances*)
                     (setf tppath (pathname tppath))
                     (format t "~&Copying the training instance ~a " tppath)
                     (unless (probe-file tppath)
                       (format t "~&  ~a does not exist, ignored!" tppath)
                       (next-iteration))
                     (if *remove-main-problem-cost*
                         (let ((tprob (nth-value 1 (suppress (parse-file tppath nil t)))))
                           (write-pddl (remove-costs tprob)
                                       (file-namestring tppath)
                                       dir *verbose*))
                         (just-copy-file
                          ppath
                          (format nil "~a/~a" dir
                                  (file-namestring tppath))))))
             (if *remove-main-problem-cost*
                 (multiple-value-bind (dname *domain*) (suppress (parse-file dpath nil t))
                   (declare (ignorable dname))
                     (multiple-value-bind (pname *problem*) (suppress (parse-file ppath nil t))
                     (declare (ignorable pname))
                     (test-problem-common
                      (write-pddl (remove-costs *problem*) "problem.pddl" dir)
                      (write-pddl (remove-costs *domain*) "domain.pddl" dir)
                      :name *main-search*
                      :options *main-options*
                      :verbose *verbose*
                      :iterated *iterated*)))
                 (test-problem-common
                  (just-copy-file ppath (format nil "~a/problem.pddl" dir))
                  (just-copy-file dpath (format nil "~a/domain.pddl" dir))
                  :name *main-search*
                  :options *main-options*
                  :verbose *verbose*
                  :iterated *iterated*)))))
      (and plans
           (iter (for path in plans)
                 (for i from 1)
                 (for new-path =
                      (merge-pathnames
                       (format nil "~a.plan.~a"
                               (pathname-name ppath) i)))
                 (when (probe-file new-path) (delete-file new-path))
                 (sb-ext:run-program
                  "/bin/cp" (list (namestring path)
                                  (namestring new-path)))
                 (when *validation*
                   (always
                    (validate-plan dpath ppath new-path :verbose *verbose*))))))))

@export
(defun find-domain (problem-path)
  (let ((dpath (make-pathname
                :defaults problem-path :name "domain")))
    (when (probe-file dpath) (return-from find-domain dpath)))
  (let ((dpath (make-pathname
                :defaults problem-path :name
                (format nil "~a-domain" (pathname-name problem-path)))))
    (when (probe-file dpath) (return-from find-domain dpath)))
  (error "~& Failed to infer the domain file pathname!~% ~a~% ~a"
         (make-pathname :defaults problem-path :name "domain")
         (make-pathname :defaults problem-path :name (format nil "~a-domain" (pathname-name problem-path)))))

@export
(defun reformat-pddl (path)
  (unwind-protect
       (print-pddl-object
        (if *remove-main-problem-cost*
            (remove-costs 
             (%load-pddl-for-reformatting path))
            (%load-pddl-for-reformatting path))
        *standard-output*)
    (terpri)))

(defun %load-pddl-for-reformatting (path)
  (unwind-protect
       (handler-case
           (%try-load-pddl-for-reformatting path)
         (domain-not-found (c)
           @ignore c
           (format t "~&; loading the corresponding domain file...")
           (suppress (parse-file (find-domain path) nil t))
           (%try-load-pddl-for-reformatting path)))
    (terpri)))

(defun %try-load-pddl-for-reformatting (path)
  (nth-value 1 (suppress (parse-file path nil t))))

    
