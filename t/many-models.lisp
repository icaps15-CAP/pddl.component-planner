(in-package :pddl.component-planner-test)

(in-suite :pddl.component-planner)

;; test 5 and 6 -- in roverprob, there is no re-usable same tasks
;; so we have to use result2 (interchangeability between different tasks)

(test :categorize-by-plan-conversion5
  (finishes
    (set-tasks roverprob40 :objective)
    ;; (clear-plan-task-cache)
    (setf result (pmap-reduce (lambda (bucket)
                                (categorize-by-equality
                                 bucket
                                 #'task-plan-equal
                                 :transitive nil))
                              #'append
                              tasks
                              :initial-value nil))

    (setf result2 (categorize-by-equality
                   result
                   (lambda (bucket1 bucket2)
                     (or (task-plan-equal (car bucket1) (car bucket2))
                         (task-plan-equal (car bucket2) (car bucket1))))
                   :transitive nil))
    (print (mapcar #'length result))
    (print (mapcar #'length result2))
    ;; result2
    ))

(test :categorize-by-plan-conversion6
  (finishes
    (set-tasks roverprob40 :rover)
    ;; (clear-plan-task-cache)
    (setf result (pmap-reduce (lambda (bucket)
                                (categorize-by-equality
                                 bucket
                                 #'task-plan-equal
                                 :transitive nil))
                              #'append
                              tasks
                              :initial-value nil))
    (setf result2 (categorize-by-equality
                   result
                   (lambda (bucket1 bucket2)
                     (or (task-plan-equal (car bucket1) (car bucket2))
                         (task-plan-equal (car bucket2) (car bucket1))))
                   :transitive nil))
    (print (mapcar #'length result))
    (print (mapcar #'length result2))
    ;; result2
    ))
