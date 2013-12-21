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
  :depends-on (:pddl.component-planner
               :pddl.instances
               :pddl.instances.cell-assembly-eachparts
               :pddl.instances.rover
               :pddl.instances.woodworking-small
               :fiveam
               :repl-utilities)
  :components ((:module "t"
                :components
                ((:file "pddl.component-planner"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
