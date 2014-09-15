(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

;;; primary function : apply-mapping

@export
(defun map-component-plan (pddl-plan mapping)
  (ematch pddl-plan
    ((pddl-plan actions problem)
     (shallow-copy
      pddl-plan
      :problem problem
      :name (concatenate-symbols 'MP (name problem))
      :actions (map-actions actions mapping)))))

(defun map-actions (actions mapping)
  (map 'vector
       (rcurry #'map-action
               (%remove-nochange mapping))
       actions))

(defun map-action (action alist)
  (ematch action
    ((pddl-action parameters)
     (ground-action action (%apply parameters alist)))))

(defun %apply (parameters mapping)
  (mapcar
   #'car
   (reduce (lambda (parameters map1)
             (ematch map1
               ((cons from to)
                (iter (for pair in parameters)
                      (collecting
                       (ematch pair
                         ((cons (guard old (eqname old from)) nil)
                          (cons to old))
                         ((cons _ nil)
                          pair)
                         ((cons _ old)
                          (when (eqname old from)
                            (warn "Duplicated mapping! ~a ~a" parameters mapping))
                          pair)))))))
           mapping :initial-value
           (mapcar (lambda (p) (cons p nil)) parameters))))

(defun %remove-nochange (alist)
  (remove-if (lambda (cons)
               (ematch cons
                 ((cons from to)
                  (eqname from to))))
             alist))

;;;; mapping-between-tasks

(defun mapping-between-tasks (t1 t2)
  "Returns an alist.
The alist is a mapping between the objects in the tasks,
assuming the given two tasks are of the same abstract-type/init-goal interface.
e.g. if AC1 = (A B C) and AC2 = (D E F), it
contains ((A . D) (B . E) (C . F)).
if the initial state of AC1 contains (pred A P) and AC2 contains (pred D Q),
it also contains ((P . Q)). Same thing applies to the goal condition.

May contain duplicated mapping: e.g. (A . A).
"
  (append (two-list-mapping (abstract-component-components
                             (abstract-component-task-ac t1))
                            (abstract-component-components
                             (abstract-component-task-ac t2)))
          (two-list-mapping (abstract-component-attributes
                             (abstract-component-task-ac t1))
                            (abstract-component-attributes
                             (abstract-component-task-ac t2)))
          (two-list-mapping (init-object t1) (init-object t2))
          (two-list-mapping (goal-object t1) (goal-object t2))))

;;;; utilities for mapping-between-tasks

(defun fluently-connected-objects (components attributes f)
  (let ((ps (parameters f)))
    (setf ps (set-difference ps components :test #'eqname))
    (setf ps (set-difference ps attributes :test #'eqname))
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

