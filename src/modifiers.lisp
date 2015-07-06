
(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

(defmethod pddl::%add-costs ((a macro-action))
  (shallow-copy
   a
   :domain *domain*
   :actions (map 'vector #'remove-costs (actions a))
   :effect `(and ,@(add-list a)
                 ,@(mapcar (lambda (x) `(not ,x)) (delete-list a))
                 ,(parse-numeric-effect
                   `(increase (total-cost) ,(length (actions a)))))
   'add-list +unbound+
   'delete-list +unbound+
   'assign-ops +unbound+))

(defmethod pddl::%remove-costs ((a macro-action))
  (shallow-copy
   a
   :domain *domain*
   :actions (map 'vector #'remove-costs (actions a))
   :effect `(and ,@(add-list a)
                 ,@(mapcar (lambda (x) `(not ,x)) (delete-list a)))
   'assign-ops +unbound+
   'add-list +unbound+
   'delete-list +unbound+))


(defun add-macro-cost (domain problem macros)
  "Add the action costs to the domain if it is a unit-cost domain.
Primitive actions are given a cost of 1. Macro actions are given a cost same as its length."
  (let ((*domain* (add-costs domain))
        (*problem* (add-costs problem)))
    (values *domain* *problem*
            ;; FIXME: due to caching, evaluation order is important... 
            (mapcar #'add-costs macros))))

(defvar *add-macro-cost* nil
  "Add the action costs to the domain if it is a unit-cost domain.
Primitive actions are given a cost of 1. Macro actions are given a cost same as its length.
Ignored when *remove-main-problem-cost* is T.")

(defun remove-cost (domain problem macros)
  (let ((*domain* (remove-costs domain))
        (*problem* (remove-costs problem)))
    (values *domain* *problem*
            ;; FIXME: due to caching, evaluation order is important... 
            (mapcar #'remove-costs macros))))

(defvar *remove-main-problem-cost* nil
  "The problem and the domain solved by
the external planner could be modified so that it does not have
any :action-costs, so that any pure STRIPS-based planners can be used. It
depends on the special variable.

Supercedes *add-macro-cost*.")

#+nil
(defvar *action-cost-plusone* nil
  "Count the cost of each action in a macro plus one. This is a similar
  behavior to that of FD's strategy.")
