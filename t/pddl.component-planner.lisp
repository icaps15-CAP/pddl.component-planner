#|
  This file is a part of pddl.component-planner project.
  Copyright (c) 2013 guicho ()
|#

(in-package :cl-user)
(defpackage pddl.component-planner-test
  (:use :cl
        :alexandria
        :pddl
        :pddl.component-abstraction
        :pddl.component-planner
        :guicho-utilities
        :iterate
        :optima
        :fiveam)
  (:shadow :fail :maximize :minimize))
