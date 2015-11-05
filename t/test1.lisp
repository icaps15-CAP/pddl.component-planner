(in-package :pddl.component-planner.test)
(in-suite :pddl.component-planner)


(test component
  (let (tasks)
    (finishes
      (setf tasks (abstract-tasks-seed-only assemblep :product)))
    ;; no longer hold true
    ;; ;; since the problem is not binarized and the abstraction fails
    ;; (ematch tasks
    ;;   ((list t1 t2)
    ;;    (is-false (task-plan-equal t1 t2))))
    ))


#+nil
(test component-after-binarization
  (let (tasks)
    (finishes
      (setf tasks
            (abstract-tasks
             (binarize assemblep assemble)
             :product)))
    (finishes
      (ematch tasks
        ((list t1 t2)
         (is (task-plan-equal t1 t2)))))))

#+nil
(test component-macro
  (finishes
    (print
     (component-macro assemblep :product))))


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

#+nil
(test solve-problem-enhancing
  (let* ((plan (solve-problem-enhancing assemblep :verbose t))
         (dir (mktemp "validate" t))
         (dp (write-pddl assemble "domain.pddl" dir t))
         (pp (write-pddl assemblep "problem.pddl" dir t))
         (plp (write-plan plan "decoded.plan" dir t)))
    (is-true (validate-plan dp pp plp :verbose t))))


(defun solve-tests (rel-ppath)
  (solve (asdf:system-relative-pathname :pddl.component-planner rel-ppath)))

(test cap-normal
  (finishes (solve-tests "t/domains/assembly-acp-ll/p01.pddl"))
  (finishes (solve-tests "t/domains/assembly-acp-ll/p02.pddl"))
  (finishes (solve-tests "t/domains/assembly-cap-ll/p01.pddl"))
  (finishes (solve-tests "t/domains/assembly-cap-ll/p02.pddl"))
  (finishes (solve-tests "t/domains/elevators-ll/p01.pddl"))
  (finishes (solve-tests "t/domains/elevators-ll/p02.pddl"))
  (finishes (solve-tests "t/domains/woodworking-ipc11/p01.pddl"))
  (finishes (solve-tests "t/domains/woodworking-ipc11/p02.pddl")))

(test compatibility
  (finishes
    (let ((*compatibility* :strict))
      (finishes (solve-tests "t/domains/assembly-acp-ll/p01.pddl"))
      (finishes (solve-tests "t/domains/assembly-acp-ll/p02.pddl"))
      (finishes (solve-tests "t/domains/assembly-cap-ll/p01.pddl"))
      (finishes (solve-tests "t/domains/assembly-cap-ll/p02.pddl"))
      (finishes (solve-tests "t/domains/elevators-ll/p01.pddl"))
      (finishes (solve-tests "t/domains/elevators-ll/p02.pddl"))
      (finishes (solve-tests "t/domains/woodworking-ipc11/p01.pddl"))
      (finishes (solve-tests "t/domains/woodworking-ipc11/p02.pddl"))
      )))
