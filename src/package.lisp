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
        :arrow-macros
        :lparallel)
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
           :*add-macro-cost*
           :*remove-component-problem-cost*
           :*remove-main-problem-cost*
           :*precategorization*
           :*component-abstraction*
           :*variable-factoring*
           :enhance-problem
           :*start*
           :*component-plan-memory-limit*
           :*iterative-resource*
           :*num-threads*
           :*use-plain-planner*
           :*iterated*
           :*verbose*
           :*training-instances*
           :find-domain
           :map-component-plan
           :task-plan-equal
           :evaluation-signal
           :restored-evaluation-signal
           :comparison-signal
           :*rely-on-cfs*)
  (:shadowing-import-from :guicho-utilities
                          :permutations)
  (:shadowing-import-from :statistics
                          :variance
                          :median
                          :mean
                          :standard-deviation)
  (:shadowing-import-from :pddl :minimize :maximize))
