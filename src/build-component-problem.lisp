(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)


@export
(defvar *default-keep-objects* nil)
@export
(defvar *default-keep-init* nil)

@export
(defun build-component-problem (abstract-task &optional
                                (keep-objects *default-keep-objects*)
                                (keep-init *default-keep-init*))
  "build a small problem which contains only the relevant objects in a
abstract task."
  (assert (not (and (null keep-objects) keep-init))
          (keep-objects keep-init)
          "If you remove the objects in other tasks, then it's not
possible to keep the initial states which depends on those objects.
Change the value of keep-objects or keep-init.")
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
           (let* ((removed nil)
                  (other-objects (set-difference objs components))
                  (environment-objects
                   (if keep-objects
                       other-objects
                       (remove-if
                        (lambda (o)
                          (when (some (lambda (comp)
                                        (pddl-supertype-p (type o) (type comp)))
                                      components)
                            (push o removed) t))
                        other-objects))))
             (format t "~&Component: ~{~a~^, ~_~}" (mapcar #'name components))
             (format t "~&Removed  : ~{~a~^, ~_~}" (mapcar #'name removed))
             (pddl-problem
              :domain *domain*
              :name (apply #'concatenate-symbols
                           total-name (mapcar #'name components))
              :objects (append components environment-objects)
              :init (if keep-init
                        init
                        (remove-if
                         (lambda (f)
                           (some (lambda (p) (find p removed))
                                 (parameters f)))
                         init))
              :goal (list* 'and task-goal)
              :metric metric)))))))))
