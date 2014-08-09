
(in-package :pddl.component-abstraction)
(cl-syntax:use-syntax :annot)

(defun binarize (problem domain)
  (let ((*domain* (binarize-domain domain)))
    (values (binarize-problem problem)
            *domain*)))

(defun binarize-domain (domain)
  (ematch domain
    ((pddl-domain name actions predicates)
     (let ((*domain* (shallow-copy domain)))
       (let ((binarizations (mapcar #'binarization-of predicates)))
         (reinitialize-instance
          *domain*
          :name (symbolicate name "2")
          :predicates (reduce #'append binarizations :key #'binarization-results)
          :actions (binarize-actions actions binarizations)))))))

(defun binarize-problem (problem &optional (*domain* *domain*))
  (match problem
    ((pddl-problem name objects init positive-goals metric)
     (let* ((*problem* (shallow-copy problem))
            (init2 (mappend (compose #'binarization-results #'binarization-of) init))
            (goal2 (mappend (compose #'binarization-results #'binarization-of) positive-goals)))
       (pddl-problem :domain *domain*
                     :name (symbolicate name "2")
                     :objects objects
                     :init init2
                     :goal `(and ,@goal2)
                     :metric metric)))))

(defstruct binarization results source)

(defun binarization-of (predicate)
  (make-binarization
   :source predicate
   :results 
   (match predicate
     ;; pddl predicate (in domain) /atomic state (in problem)
     ((pddl-predicate :name name
                      :parameters (guard parameters (< 2 (length parameters))))
      (iter outer
            (for (p1 . rest) on parameters)
            (iter (for p2 in rest)
                  (in outer
                      (collect 
                          (shallow-copy predicate
                                        :name (symbolicate name
                                                           '- (name (type p1))
                                                           '- (name (type p2)))
                                        :parameters (list p1 p2)))))))
     
     ;; pddl function (in domain) / function state (in problem)
     ;; false idea
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
     (_ (list predicate)))))

(defun binarize-actions (actions binarizations)
  (iter (for a in actions)
        (collect (binarize-action a binarizations))))

(defun binarize-action (action binarizations)
  (ematch action
    ((pddl-action name parameters add-list delete-list positive-preconditions assign-ops)
     (pddl-action :name (symbolicate name "2")
                  :parameters parameters
                  :precondition `(and ,@(apply-binarizations binarizations positive-preconditions))
                  :effect `(and ,@(apply-binarizations binarizations add-list)
                                ,@(mapcar #'wrap-not (apply-binarizations binarizations delete-list))
                                ,@assign-ops)))))

(defun apply-binarizations (binarizations predicates)
  (mappend (lambda (s) (apply-binarization binarizations s)) predicates))

(defun find-binarization (binarizations predicate)
  (find predicate binarizations :test #'eqname :key #'binarization-source))

(defun apply-binarization (binarizations predicate)
  (let ((bin (find-binarization binarizations predicate)))
    (ematch predicate
      ((pddl-predicate :parameters ps1)
       (ematch (binarization-source bin)
         ((pddl-predicate :parameters ps2)
          (iter (for pred in (binarization-results bin))
                (collect
                    (match pred
                      ((pddl-predicate name parameters)
                       (pddl-predicate
                        :name name
                        :parameters
                        (iter (for p in parameters)
                              (collecting (elt ps1 (position p ps2)))))))))))))))

(defun wrap-not (x) `(not ,x))

;; ;;; assign-ops

;; (defun binarize-assign-ops (assign-ops)
;;   (iter (for op in assign-ops)
        
