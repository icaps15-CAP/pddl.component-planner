
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

(defun add-cost (*domain* *problem*)
  (unless (member :action-costs (requirements *domain*))
    (push :action-costs (requirements *domain*))
    (push (pddl-function :name 'total-cost
                         :parameters nil)
          (functions *domain*))
    (setf (actions *domain*)
          (mapcar #'add-numeric-effect (actions *domain*)))
    ;(break+ (mapcar #'assign-ops (actions *domain*)))
    (push (ground-function (query-function *domain* 'total-cost)
                           nil 0 *problem*)
          (init *problem*))
    (setf (metric *problem*)
          (parse-metric-body '(minimize (total-cost)))))
  (values *domain* *problem*))

(defun enhancement-method (problem)
  (enhance-problem problem
                   :modify-domain-problem #'add-cost))
