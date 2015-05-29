
(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

;; (union '(:action-cost) requirements)

;; (defun add-numeric-effect/pair (pairs)
;;   (mapcar (lambda (pair)
;;             (ematch pair
;;               ((vector bag m)
;;                (vector
;;                 bag (%add-numeric-effect/macro m)))))
;;           pairs))

(defun add-numeric-effect (a)
  (ematch a
    ((macro-action add-list delete-list actions)
     (shallow-copy a
                   :domain *domain*
                   'add-list +unbound+
                   'delete-list +unbound+
                   'assign-ops +unbound+
                   :effect `(and ,@add-list
                                 ,@(mapcar (lambda (x) `(not ,x)) delete-list)
                                 ,(parse-numeric-effect
                                   `(increase (total-cost) ,(length actions))))))
    ((pddl-action add-list delete-list)
     (shallow-copy a  ; it might be a binarized-action, so it should be a
                      ; shallow copy
                   :domain *domain*
                   'add-list +unbound+
                   'delete-list +unbound+
                   'assign-ops +unbound+
                   :effect `(and ,@add-list
                                 ,@(mapcar (lambda (x) `(not ,x)) delete-list)
                                 ,(parse-numeric-effect
                                   `(increase (total-cost) 1)))))))

(defun add-macro-cost (*domain* *problem*)
  "Add the action costs to the domain if it is a unit-cost domain.
Primitive actions are given a cost of 1. Macro actions are given a cost same as its length."
  (unless (member :action-costs (requirements *domain*))
    ;; domain
    (push :action-costs (requirements *domain*))
    (push (pddl-function :name 'total-cost
                         :parameters nil)
          (functions *domain*))
    (setf (actions *domain*)
          (mapcar #'add-numeric-effect (actions *domain*)))
    ;; problem
    (push (ground-function (query-function *domain* 'total-cost)
                           nil 0 *problem*)
          (init *problem*))
    (setf (metric *problem*)
          (parse-metric-body '(minimize (total-cost)))))
  (values *domain* *problem*))

(defvar *add-macro-cost* nil
  "Add the action costs to the domain if it is a unit-cost domain.
Primitive actions are given a cost of 1. Macro actions are given a cost same as its length.")

(defun remove-cost (*domain* *problem*)
  (values (remove-costs *domain*) (remove-costs *problem*)))

(defvar *remove-main-problem-cost* nil
  "The problem and the domain solved by
the external planner could be modified so that it does not have
any :action-costs, so that any pure STRIPS-based planners can be used. It
depends on the special variable.")

