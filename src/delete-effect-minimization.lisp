(in-package :pddl.component-planner)

(cl-syntax:use-syntax :annot)
;; blah blah blah.

@export
(defun build-delete-effect-minimization-domain (domain)
  (let* ((*package* (find-package :pddl.instances))
         (total-cost-sym (intern "TOTAL-COST" *package*)))
    (ematch domain
      ((pddl-domain :name name :actions actions :functions functions)
       (let ((*domain*
              (shallow-copy
               domain
               :name (concatenate-symbols name 'delete-effect-minimization)
               :functions
               (if (member total-cost-sym functions :key #'name)
                   functions
                   (cons (pddl-function :name total-cost-sym
                                        :parameters nil)
                         functions)))))
         (setf (actions *domain*)
               (mapcar
                (lambda (a)
                  (let ((deletion-cost (length (delete-list a))))
                    (shallow-copy
                     a
                     :assign-ops
                     (list
                      (let ((source `(increase ,total-cost-sym ,deletion-cost)))
                        (ematch (transform-numeric-to-assign source)
                          ((list 'assign place _)
                           (pddl-assign-op
                            :source source
                            :place-function (compile-place-function place)
                            :value-function (compile-value-function deletion-cost)))))))))
                actions))
         *domain*)))))

@export
(defun build-delete-effect-minimization-problem
    (problem &optional (*domain*
                        (build-delete-effect-minimization-domain
                         (domain problem))))
  (let* ((*package* (find-package :pddl.instances))
         (total-cost-sym (intern "TOTAL-COST" *package*))
         (metric-f-exp (list total-cost-sym)))
    (shallow-copy problem
                  :name (concatenate-symbols
                         (name problem)
                         'delete-effect-minimization)
                  :domain *domain*
                  :metric
                  (pddl-metric
                   :optimization 'minimize
                   :metric-spec metric-f-exp
                   :metric-function (compile-metric-function metric-f-exp)))))


@export
(defcached plan-delete-effect-minimization (domain task)
  (let* ((*domain* (build-delete-effect-minimization-domain domain))
         (*problem* (build-delete-effect-minimization-problem
                     (build-component-problem task) *domain*)))
    (mapcar (curry #'pddl-plan :path)
            (test-problem
             (write-pddl *problem*)
             (write-pddl *domain*)
             :time-limit 5
             :hard-time-limit 10
             :options "--search astar(lmcut())"))))

