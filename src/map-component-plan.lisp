(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

;;; primary function : apply-mapping

@export
(defun map-component-plan (pddl-plan mapping)
  (ematch pddl-plan
    ((pddl-plan actions problem domain)
     (let ((%mapping (%remove-nochange mapping)))
       (shallow-copy
        pddl-plan
        :problem problem
        :name (concatenate-symbols 'MP (name problem))
        :actions
        (map 'vector
             (lambda (ga)
               (ematch ga
                 ((pddl-ground-action parameters)
                  (ground-action (action domain (name ga))
                                 (%apply parameters %mapping) ))))
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

;;;; mapping-between-tasks

(defun mapping-between-tasks (t1 t2)
  "Returns a list of 3 elements.
The first element is a mapping between the objects in the tasks,
assuming the given two tasks are of the same abstract-type.
e.g. if AC1 = (A B C) and AC2 = (D E F), it
returns ((A . D) (B . E) (C . F)).

The second and the third element is a mapping between the initial and the
goal states, respectively.
"
  (list (two-list-mapping (abstract-component-components
                           (abstract-component-task-ac t1))
                          (abstract-component-components
                           (abstract-component-task-ac t2)))
        (two-list-mapping (init-object t1) (init-object t2))
        (two-list-mapping (goal-object t1) (goal-object t2))))

;;;; utilities for mapping-between-tasks

(defun fluently-connected-objects (components attributes f)
  (let ((ps (parameters f)))
    (setf ps (set-difference ps components))
    (setf ps (set-difference ps attributes))
    ps))

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

(defun two-list-mapping (os1 os2)
  (mapcar (lambda (o1 o2)
            (cons o1 o2))
          os1 os2))

