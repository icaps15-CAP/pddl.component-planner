(in-package :pddl.component-planner.test)
(in-suite :pddl.component-planner)


(test component
  (let (tasks)
    (finishes
      (setf tasks (abstract-tasks-seed-only assemblep :product)))
    (finishes
      (ematch tasks
        ((list t1 t2)
         (is-false (task-plan-equal t1 t2))))))) ; since the problem is not
                                        ; binarized and the
                                        ; abstraction fails

;; (test component-after-binarization
;;   (let (tasks)
;;     (finishes
;;       (setf tasks
;;             (abstract-tasks
;;              (binarize assemblep assemble)
;;              :product)))
;;     (finishes
;;       (ematch tasks
;;         ((list t1 t2)
;;          (is (task-plan-equal t1 t2)))))))


;; (test component-macro
;;   (finishes
;;     (print
;;      (component-macro assemblep :product))))


(test enhance-domain
  (finishes
    (let ((*start* (get-universal-time)))
      (multiple-value-bind (*problem* *domain*)
          (enhance-problem assemblep)
        (print-pddl-object *problem* *standard-output*)
        (terpri *standard-output*)
        (print-pddl-object *domain* *standard-output*)
        (let* ((dir (mktemp "enhanced"))
               (pp (write-pddl *problem* "problem.pddl" dir))
               (dp (write-pddl *domain* "domain.pddl" dir))
               (results (multiple-value-list (test-problem-common pp dp
                                                                  :name "probe-clean"
                                                                  :options ""
                                                                  :verbose t))))
          (is-true (validate-plan dp pp (first (first results))
                                  :verbose t)))))))

;; (test solve-problem-enhancing
;;   (let* ((plan (solve-problem-enhancing assemblep :verbose t))
;;          (dir (mktemp "validate" t))
;;          (dp (write-pddl assemble "domain.pddl" dir t))
;;          (pp (write-pddl assemblep "problem.pddl" dir t))
;;          (plp (write-plan plan "decoded.plan" dir t)))
;;     (is-true (validate-plan dp pp plp :verbose t))))
