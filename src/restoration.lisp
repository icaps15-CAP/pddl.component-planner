(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

(defgeneric filter-objects (strategy &key &allow-other-keys))
(defgeneric filter-inits (strategy &key &allow-other-keys))
(defgeneric filter-object (strategy o &key &allow-other-keys))
(defgeneric filter-init (strategy i &key &allow-other-keys))

;;; filtering-strategy -- basic filtering

(defclass filtering-strategy () ()
  (:documentation
   "Removes all objects that is a part of the other components.
Remaining objects are those who do not belongs to any components,
and the objects in the target component itself."))

(defmethod filter-objects ((strategy filtering-strategy)
                           &rest keys &key objs components)
  (iter (for o in (set-difference objs components)) ;; external objects
        (if (apply #'filter-object strategy o keys)
            ;; when t, it is kept. when nil, it is removed.
            (collect o into environment-objects)
            (collect o into removed-objects))
        (finally (return (values (append components environment-objects)
                                 removed-objects)))))

(defmethod filter-inits ((strategy filtering-strategy)
                         &rest keys &key init)
  (iter (for i in init)
        (if (apply #'filter-init strategy i keys)
            ;; when t, it is kept. when nil, it is removed.
            (collect i into active-inits)
            (collect i into removed-inits))
        (finally (return (values active-inits
                                 removed-inits)))))

(defmethod filter-object ((strategy filtering-strategy)
                          o &key components &allow-other-keys)
  (notany (lambda (comp)
            (pddl-supertype-p (type o) (type comp)))
          components))

(defmethod filter-init ((strategy filtering-strategy)
                        i &key removed-objects &allow-other-keys)
  (notany (lambda (p) (find p removed-objects))
          (parameters i)))

;;; full restoration strategy -- proposed in KEPS14

;; (defclass full-restoration-strategy (filtering-strategy)
;;   ((obj-restored :type boolean :initform nil :accessor obj-restored)
;;    (init-restored :type boolean :initform nil :accessor init-restored))
;;   (:documentation
;;    "Same as filtering-strategy, but it returns a full list of objects
;; when it is called more than twice."))
;; 
;; (defmethod filter-objects
;;     ((strategy full-restoration-strategy) &key objs)
;;   (if (obj-restored strategy)
;;       (values objs nil)
;;       (prog1
;;           (call-next-method)
;;         (setf (obj-restored strategy) t))))
;; 
;; (defmethod filter-inits ((strategy full-restoration-strategy)
;;                          &rest keys &key init)
;;   (if (init-restored strategy)
;;       (values init nil)
;;       (prog1
;;           (call-next-method)
;;         (setf (init-restored strategy) t))))

;;; gradual restoration strategy

;; (defclass gradual-restoration-strategy (filtering-strategy)
;;   ((kernel :initform nil :accessor kernel)
;;    (active-objects :initform nil :accessor active-objects)
;;    (removed-objects :initform nil :accessor removed-objects)
;;    (active-inits :initform nil :accessor active-inits)
;;    (removed-inits :initform nil :accessor removed-inits))
;;   (:documentation
;;    "In the first call, it behaves just the same way as filtering-strategy
;;  does.  After that, the number of returned objects grows gradually.  It
;;  incrementally grows its kernel, which is defined as: Initially, the kernel
;;  is the component itself.  After that, the kernel grows:

;; If any components are statically connected to the kernel,


;; "))

;; (defmethod filter-objects ((strategy gradual-restoration-strategy)
;;                            &key objs components init)
;;   (if (null (active-objects strategy))
;;       (setf (kernel strategy)
;;             components
;;             (values (active-objects strategy)
;;                     (removed-objects strategy))
;;             (call-next-method))
;;       ;; else, grow from the kernel
;;       (iter (for ok in kernel)
;;             (iter (for or in removed-objects)

;; (defun restore-objects-gradually (o &key components strategy &allow-other-keys)
;;   (some (lambda (comp)
;;           (pddl-supertype-p (type o) (type comp)))
;;         components))

