
(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

(defun enhance-with-cost (problem &optional seed &aux (domain (domain problem)))
  (declare (ignore seed))
  (format t "~&Binarizing domain ~a" domain)
  (multiple-value-bind (problem domain) (binarize problem domain)
    (format t "~&Enhancing domain ~a" domain)
    (ematch domain
      ((pddl-domain requirements name actions)
       (let ((macros (iter (for seed in (types-in-goal problem))
                           (appending (component-macro problem seed)))))
         (setf macros (remove-if (lambda-match ((vector _ nil) t)) macros))
         (if macros
             (format t "~&~a macros found~@[, filtered down to 2~]."
                     (length macros) (< 2 (length macros)))
             (warn "No component macros are found!"))
         (let* ((macros/filtered (filter-macros macros))
                (*domain*
                 (shallow-copy domain
                               :requirements (union '(:action-cost) requirements)
                               :name (symbolicate name '-enhanced)
                               :actions (append actions macros/filtered))))
           (values (shallow-copy problem
                                 :name (symbolicate (name problem) '-enhanced)
                                 :domain *domain*)
                   *domain* macros)))))))

(defun add-costs (macros)
  (iter (for m in macros)
        (match m
          ((macro-action (domain (pddl-domain
                                  :requirements
                                  (guard req (member :action-costs req)))))
           m)
          ((macro-action precondition add-list delete-list actions)
           (make-instance 'macro-action 
                          :actions actions
                          :precondition precondition
                          :effect `(and ,@add-list
                                        ,@(mapcar (lambda (x) `(not ,x)) delete-list)
                                        ,(parse-numeric-effect
                                          `(increase (total-cost) ,(length actions)))))))))

           
           
