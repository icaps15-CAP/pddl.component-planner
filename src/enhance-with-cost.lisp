
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

(defun add-cost-domain (*domain*)
  (unless (member :action-costs (requirements *domain*))
    (push :action-costs (requirements *domain*))
    (push (pddl-function :name 'total-cost
                         :parameters nil)
          (functions *domain*))
    (setf (actions *domain*)
          (mapcar #'add-numeric-effect (actions *domain*)))
    ;(break+ (mapcar #'assign-ops (actions *domain*)))
    )
  *domain*)

(defun add-cost-problem (*problem*)
  (unless (member :action-costs (requirements *domain*))
    (push (ground-function (query-function *domain* 'total-cost)
                           nil 0 *problem*)
          (init *problem*)))
  *problem*)

(defun solve-problem-enhancing (problem &rest test-problem-args)
  (clear-plan-task-cache)
  (format t "~&Enhancing the problem with macros.")
  (multiple-value-bind (eproblem edomain macros)
      (enhance-problem problem
                       :modify-domain #'add-cost-domain
                       :modify-problem #'add-cost-problem)
    (format t "~&Enhancement finished.~&Solving the enhanced problem with FD.")
    (let* ((dir (mktemp "enhanced")))
      (debinarize-plan
       (domain problem)
       problem
       edomain
       eproblem
       (let ((*domain* edomain) (*problem* eproblem))
         (reduce #'decode-plan
                 macros
                 :from-end t
                 :initial-value
                 (pddl-plan :path
                            (first
                             (prog1 (apply #'test-problem
                                           (write-pddl *problem* "eproblem.pddl" dir)
                                           (write-pddl *domain* "edomain.pddl" dir)
                                           test-problem-args)
                               (format t "~&Decoding the result plan."))))))))))
