#|
  This file is a part of pddl.component-planner project.
  Copyright (c) 2013 guicho ()
|#

(in-package :cl-user)
(defpackage pddl.component-planner-test-asd
  (:use :cl :asdf))
(in-package :pddl.component-planner-test-asd)

(defsystem pddl.component-planner-test
  :author "guicho"
  :license "LLGPL"
  :depends-on (:pddl.component-abstraction
               :pddl.component-planner
               :pddl.instances
               :fiveam
               :local-time
               :cl-csv
               :guicho-utilities
               :guicho-utilities.threading
               :lparallel
               :repl-utilities
               :log4cl)
  :components ((:module "t"
                :components
                ((:file "pddl.component-planner")
                 (:file "test1"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
