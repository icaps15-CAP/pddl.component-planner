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
        :trivial-signal
        :arrow-macros)
  (:export :reformat-pddl
           :solve
           :*preprocess-only*
           :*component-plan-time-limit*
           :*preprocess-time-limit*
           :*validation*
           :*preprocessor*
           :*preprocessor-options*
           :*debug-preprocessing*
           :*main-search*
           :*main-options*
           :*threshold*
           :*compatibility*
           :*binarization*
           :*cyclic-macros*
           :*ground-macros*
           :*remove-component-problem-cost*
           :*remove-main-problem-cost*
           :*precategorization*)
  (:shadowing-import-from :guicho-utilities
                          :permutations)
  (:shadowing-import-from :statistics
                          :variance
                          :median
                          :mean
                          :standard-deviation)
  (:shadowing-import-from :pddl :minimize :maximize))
