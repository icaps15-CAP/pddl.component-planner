
(in-package :pddl.component-planner-test)
(in-suite :pddl.component-planner)

;; assemble-resource
;; assemblep-resource


;; (write-plan (car
;;               (solve-problem-enhancing
;;                pddl.component-planner-test::assemblep-resource
;;                :name *main-search*
;;                :options *main-options*
;;                :verbose t)) "myplan.pddl" *default-pathname-defaults*)


(defvar *pairs*)
(setf *pairs*
      (let ((*problem* pddl.component-planner-test::assemblep-resource)
            (*domain* pddl.component-planner-test::assemble-resource))
        (pddl.component-planner::generate-macro-pairs *problem*)))

;; (defvar *x*)
;; (setf *x*
;;       (let ((*problem* pddl.component-planner-test::assemblep-resource)
;;             (*domain* pddl.component-planner-test::assemble-resource))
;;         (first (mappend #'pddl.component-planner::get-actions-grounded
;;                         *pairs*))))

;; (defvar *macro-goal*)
;; (setf *macro-goal*
;;       (let ((*problem* pddl.component-planner-test::assemblep-resource)
;;             (*domain* pddl.component-planner-test::assemble-resource))
;;         (apply-ground-action
;;          *x* (init *problem*))))


;; mvb
#+nil
(let ((*problem* pddl.component-planner-test::assemblep-resource)
      (*domain* pddl.component-planner-test::assemble-resource))
  (get-actions-grounded (first *pairs*)))

(defvar *reverse-problems*)
(setf *reverse-problems*
      (let ((*problem* pddl.component-planner-test::assemblep-resource)
            (*domain* pddl.component-planner-test::assemble-resource))
        (multiple-value-bind (gms tasks) 
            (get-actions-grounded (first *pairs*))
          (iter (for gm in gms)
                (for task in tasks)
                (collect (reverse-problem gm task))))))
apply-ground-action
(let ((*debug-preprocessing* t))
  (solve-rev (first *reverse-problems*) assemble-resource))

