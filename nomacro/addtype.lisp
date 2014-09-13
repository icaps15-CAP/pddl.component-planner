

(ql:quickload :pddl.component-planner.experiment)
;;(sb-profile:profile "PDDL" "PDDL.COMPONENT-PLANNER" "PDDL.COMPONENT-ABSTRACTION" "GUICHO-UTILITIES" "EAZYLAZY")
(in-package "PDDL.COMPONENT-PLANNER.EXPERIMENT")
(swank:set-default-directory
 "/mnt/video/guicho/repos/lisp/pddl.component-planner/aaai/nomacro/")

(in-package :pddl.component-planner.experiment)

(ql:quickload :osicat)

(use-package :osicat)
(use-package :pddl.component-abstraction)

(defvar files
    (mapcar (lambda (path)
              (when (directory-pathname-p path)
                (list-directory path)))
            (list-directory ".")))

(defun addtype-files ()
  (iter (for flist in files)
        (restart-case
            (progn
              (iter (for file in flist)
                    (restart-case
                        (when (and (string= "pddl" (pathname-type file))
                                   (search "domain" (pathname-name file)))
                          (handler-bind ((warning #'muffle-warning))
                            (multiple-value-bind (name obj) (parse-file file)
                              (format t "~&~a ~a" name file)
                              (let ((path (make-pathname :defaults file :type "typed-pddl")))
                                (format t "~&Writing to ~a ..." path)
                                (write-pddl (add-types obj) path *default-pathname-defaults*)))))
                      (skip-this-file ()
                        (warn "skipped file ~A" file))))
              (iter (for file in flist)
                    (format t "~&~a" file)
                    (restart-case
                        (when (and (string= "pddl" (pathname-type file))
                                   (not (search "domain" (pathname-name file))))
                          (handler-bind ((warning #'muffle-warning))
                            (multiple-value-bind (name obj) (parse-file file)
                              (format t "~&~a ~a" name file)
                              (let ((path (make-pathname :defaults file :type "typed-pddl")))
                                (format t "~&Writing to ~a ..." path)
                                (write-pddl (add-types obj) path *default-pathname-defaults*)))))
                      (skip-this-file ()
                        (warn "skipped file ~A" file)))))
          (skip-this-domain ()
            (warn "skipped dir ~A" flist)))))
      

(parse-file "gripper/domain.pddl")
(parse-file "gripper/domain.pddl")

(print-pddl-object
 (add-types-to-problem logistics-4-0 logistics)
 *standard-output*)

