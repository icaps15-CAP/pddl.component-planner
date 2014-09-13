(in-package :cl-user)
(defpackage pddl.component-planner
  (:use :cl
        :statistics
        :optima
        :function-cache
        :guicho-utilities
        :pddl.component-abstraction
        :pddl.macro-action
        :pddl
        :iterate
        :alexandria
        :cl-annot
        :anaphora)
  (:export :resource-usage
           :categorize-problem
           :component-plans
           :component-macro
           :enhance-problem
           :solve-problem-enhancing
           :clear-plan-task-cache
           :plan-task
           :types-in-goal
           :add-cost-domain
           :add-cost-problem
           :*preprocess-only*
           :*component-plan-time-limit*
           :*preprocess-time-limit*
           :*validation*
           :*disable-filtering*
           :*preprocessor*
           :*preprocessor-options*
           :*debug-preprocessing*
           :*main-search*
           :*main-options*
           :reverse-problem
           :get-actions-grounded
           :solve-rev
           :*use-grounded-macros*
           :*use-reverse-macros*
           :*use-grounded-reverse-macros*
           :reverse-macro
           :cyclic-macro
           :*threshold*
           :*compatibility-type*
           :strict
           :always-false
           :loose)
  (:shadowing-import-from :guicho-utilities
                          :permutations)
  (:shadowing-import-from :statistics
                          :variance
                          :median
                          :mean
                          :standard-deviation)
  (:shadowing-import-from :pddl :minimize :maximize))
