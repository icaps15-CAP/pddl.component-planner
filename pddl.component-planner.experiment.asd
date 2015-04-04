#|
  This file is a part of pddl.component-planner project.
  Copyright (c) 2013 guicho ()
|#

#|
  Author: guicho ()
|#

(defsystem pddl.component-planner.experiment
  :version "0.1"
  :author "guicho"
  :license "LLGPL"
  :depends-on (#-add-cost
               :pddl.component-planner
               #+add-cost
               :pddl.component-planner.add-cost)
  :components ((:file :main)))

