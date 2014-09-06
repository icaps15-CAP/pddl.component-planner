
(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

;;; class definitions

(define-pddl-class binarized-object ()
  (binarization-origin))

(define-pddl-class binarized-action (pddl-action binarized-object) ())
(define-pddl-class binarized-predicate (pddl-predicate binarized-object) ())
(define-pddl-class binarized-atomic-state (pddl-atomic-state binarized-object) ())

;;; binarizing domain & problem

;; not exported
(defun ensure-binarized (domain)
  "Ensuress the domain is binarized"
  (if (every (lambda (p) (<= (length (parameters p)) 2))
             (predicates domain))
      (values domain nil)
      (values (binarize-domain domain) t)))

(defun binarize (problem domain)
  "Ensure both the problem and the domain is binarized."
  (multiple-value-bind (*domain* changed?) (ensure-binarized domain)
    (values (if changed? (binarize-problem problem) problem)
            *domain*)))

(defun binarize-domain (domain)
  (ematch domain
    ((pddl-domain name actions predicates)
     (let ((*domain* (shallow-copy domain)))
       (reinitialize-instance
        *domain*
        :name (symbolicate name "2")
        :predicates (mappend #'binarize-predicate predicates)
        :actions (mapcar #'binarize-action actions))))))

(defun binarize-problem (problem &optional (*domain* *domain*))
  (match problem
    ((pddl-problem name objects init positive-goals metric)
     (let* ((*problem* (shallow-copy problem))
            (init2 (mappend #'binarize-predicate init))
            (goal2 (mappend #'binarize-predicate positive-goals)))
       (pddl-problem :domain *domain*
                     :name (symbolicate name "2")
                     :objects objects
                     :init (remove-duplicates init2 :test #'eqstate)
                     :goal `(and ,@(remove-duplicates goal2 :test #'eqstate))
                     :metric metric)))))

;;; converting a predicate into a set of binary predicates

(defun binarize-predicate (predicate)
  "returns an object of type `binarized-predicate' or
`binarized-atomic-state', depending on the given object.
Returns itself if numeric-fluents are given."
  (match predicate
    ;; pddl predicate (in domain) /atomic state (in problem)
    ((pddl-atomic-state :name name
                        :parameters (guard parameters
                                           (< 2 (length parameters))))
     (iter outer
           (for (p1 . rest) on parameters)
           (for i from 1)
           (iter (for p2 in rest)
                 (for j from (1+ i))
                 (in outer
                     (collect 
                         (binarized-atomic-state
                          :binarization-origin predicate
                          :name (intern
                                 (format nil "~a-~a~a-~a~a"
                                         name
                                         (name (type p1)) (princ-to-string i)
                                         (name (type p2)) (princ-to-string j)))
                          :parameters (list p1 p2)))))))
    ((pddl-predicate :name name
                     :parameters (guard parameters
                                        (< 2 (length parameters))))
     (iter outer
           (for (p1 . rest) on parameters)
           (for i from 1)
           (iter (for p2 in rest)
                 (for j from (1+ i))
                 (in outer
                     (collect 
                         (binarized-predicate
                          :binarization-origin predicate
                          :name (intern
                                 (format nil "~a-~a~a-~a~a"
                                         name
                                         (name (type p1)) (princ-to-string i)
                                         (name (type p2)) (princ-to-string j)))
                          :parameters (list p1 p2)))))))
    ;; pddl function (in domain) / function state (in problem)
    ;; false idea --- at least, currently not supported because
    ;; FF is not able to handle numeric fluents
    ;; ((pddl-function :name name
    ;;                 :parameters (guard parameters (< 2 (length parameters))))
    ;;  (iter outer
    ;;        (for (p1 . rest) on parameters)
    ;;        (iter (for p2 in rest)
    ;;              (in outer
    ;;                  (collect 
    ;;                      (shallow-copy predicate
    ;;                                    :name (symbolicate name
    ;;                                                       '- (name (type p1))
    ;;                                                       '- (name (type p2)))
    ;;                                    :parameters (list p1 p2)))))))
    (_ (list predicate))))

;;; binarize actions

(defun binarize-action (action)
  (ematch action
    ((pddl-action name parameters add-list delete-list
                  positive-preconditions assign-ops)
     (binarized-action
      :name (symbolicate name "2")
      :binarization-origin action
      :parameters parameters
      :precondition `(and ,@(mapcar
                             #'binarize-predicate
                             positive-preconditions))
      :effect `(and ,@(mapcar #'binarize-predicate add-list)
                    ,@(mapcar (compose #'wrap-not #'binarize-predicate)
                              delete-list)
                    ,@assign-ops)))))

;; apply binarization to the predicates in the action definition.

(defun wrap-not (x) `(not ,x))

;; ;;; assign-ops

;; (defun binarize-assign-ops (assign-ops)
;;   (iter (for op in assign-ops)
        
