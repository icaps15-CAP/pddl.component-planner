(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)


@export
(defun build-component-problem (abstract-task
                                &optional
                                  (keeping-strategy
                                   (ask-for
                                    keeping-strategy
                                    (make-instance 'full-restoration-strategy))))
  "build a small problem which contains only the relevant objects in a
abstract task. objects and initial state is filtered according to the functions
given in *def"
  (ematch abstract-task
    ((abstract-component-task :problem *problem*)
     (ematch *problem*
       ((pddl-problem :name total-name
                      :domain *domain*
                      :objects objs
                      :init init
                      :metric metric)
        (match abstract-task
          ((abstract-component-task- (ac (abstract-component components))
                                     (goal task-goal))
           (multiple-value-bind
                 (active-objects removed-objects)
               (filter-objects keeping-strategy
                               :objs objs
                               :components components
                               :init init)
             (multiple-value-bind
                   (active-init removed-init)
                 (filter-inits keeping-strategy
                               :objs objs
                               :components components
                               :active-objects active-objects
                               :removed-objects removed-objects
                               :init init)
               (format t "~&Component: ~{~a~^, ~_~}"
                       (mapcar #'name components))
               (format t "~&Removed  : ~{~a~^, ~_~}"
                       (mapcar #'name removed-objects))
               (pddl-problem
                :domain *domain*
                :name (apply #'concatenate-symbols
                             total-name (mapcar #'name components))
                :objects active-objects
                :init active-init
                :goal (list* 'and task-goal)
                :metric metric))))))))))
