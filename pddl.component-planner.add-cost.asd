#|
  This file is a part of pddl.component-planner project.
  Copyright (c) 2013 guicho ()
|#

#|
  Loads additional file for appending action-costs in the enhanced domain
  Author: guicho ()
|#


(defsystem pddl.component-planner.add-cost
  :version "0.1"
  :author "guicho"
  :license "LLGPL"
  :depends-on (:pddl.component-planner)
  :components ((:module "src"
                :components
                ((:file :enhance-with-cost)))))

