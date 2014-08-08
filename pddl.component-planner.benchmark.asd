#|
  This file is a part of pddl.component-planner project.
  Copyright (c) 2013 guicho ()
|#

#|
  Author: guicho ()
|#

(defsystem pddl.component-planner.benchmark
  :version "0.1"
  :author "guicho"
  :license "LLGPL"
  :depends-on (:pddl.component-planner
               :log4cl)
  :components ((:module :benchmark
                :components
                ((:file :benchmark))
                :serial t)))

