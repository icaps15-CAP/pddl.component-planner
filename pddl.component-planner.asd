#|
  This file is a part of pddl.component-planner project.
  Copyright (c) 2013 guicho ()
|#

#|
  

  Author: guicho ()
|#

(in-package :cl-user)
(defpackage pddl.component-planner-asd
  (:use :cl :asdf))
(in-package :pddl.component-planner-asd)

(defsystem pddl.component-planner
  :version "0.1"
  :author "guicho"
  :license "LLGPL"
  :depends-on (:optima
               :function-cache
               :guicho-utilities
               :pddl.component-abstraction
               :pddl.macro-action
               :pddl.planner-scripts
               :pddl
               :iterate
               :alexandria
               :cl-annot
               :arrow-macros)
  :components ((:module "src"
                :components
                ((:file :cl-statistics)
                 (:file :package)
                 (:file :limit)
                 (:file :binarization)
                 (:file :map-component-plan)
                 (:file :conditions)
                 (:file :restoration)
                 (:file :plan-task)
                 (:file :task-equality)
                 (:file :reverse)
                 (:file :modifiers)
                 (:file :filter)
                 (:file :decompose)
                 (:file :plan)
                 ;; (:file :delete-effect-minimization)
                 )
                :serial t))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op pddl.component-planner-test))))


(defmethod asdf:perform ((op asdf:test-op)
			 (system (eql (asdf:find-system :pddl.component-planner))))
  (funcall (find-symbol "RUN!" (find-package :fiveam)) :pddl.component-planner)
  t)
