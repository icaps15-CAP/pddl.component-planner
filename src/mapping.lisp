(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

@export
(defun fluently-connected-objects (components attributes f)
  (let ((ps (parameters f)))
    (setf ps (set-difference ps components))
    (setf ps (set-difference ps attributes))
    ps))

@export
(defun goal-object (task)
  (ematch task
    ((abstract-component-task
      goal
      (ac (abstract-component
           components
           attributes)))
     (mappend 
      (curry #'fluently-connected-objects components attributes)
      goal))))

@export
(defun init-object (task)
  (ematch task
    ((abstract-component-task
      init
      (ac (abstract-component
           components
           attributes)))
     (mappend 
      (curry #'fluently-connected-objects components attributes)
      init))))

@export
(defun mapping-between-tasks (t1 t2)
  (list (two-list-mapping (abstract-component-components
                           (abstract-component-task-ac t1))
                          (abstract-component-components
                           (abstract-component-task-ac t2)))
        (two-list-mapping (init-object t1) (init-object t2))
        (two-list-mapping (goal-object t1) (goal-object t2))))

@export
(defun two-list-mapping (os1 os2)
  (mapcar (lambda (o1 o2)
            (cons o1 o2))
          os1 os2))


@export
(defun apply-mapping (plan mapping mapped-problem)
  (ematch plan
    ((pddl-plan actions problem)
     (let ((%mapping 
            ;; @break+
            (%remove-nochange mapping)))
       (shallow-copy
        plan
        :problem mapped-problem
        :name (concatenate-symbols
               'MP (name mapped-problem))
        :actions
        ;;@break+
        (map 'vector
             (lambda (action)
               (ematch action
                 ((or (pddl-initial-action)
                      (pddl-goal-action))
                  action)
                 ((pddl-intermediate-action parameters)
                  (shallow-copy
                   action
                   :parameters (%apply parameters %mapping)))))
             actions))))))

(defun %apply (parameters mappings)
  (reduce (lambda (parameters mapping)
            (ematch mapping
              ((cons from to)
               (substitute to from parameters))))
          mappings :initial-value parameters))

(defun %remove-nochange (mapping)
  (ematch mapping
    ((list component-mapping init-mapping goal-mapping)
     (flet ((fn (mapping)
              (ematch mapping
                ((cons from to)
                 (eq from to)))))
       (append component-mapping
               (remove-if #'fn init-mapping)
               (remove-if #'fn goal-mapping))))))
