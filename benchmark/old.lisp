



;; generic version
;; (defun histogram (list)
;;   (iter (with h = nil)
        ;; (for n in list)
        ;; (incf (getf h n 0))
;;         (finally (return (plist-alist h)))))

;; assumes list of numbers
(defun histogram (list)
  (iter (with v = (make-array 10
                              :initial-element 0
                              :adjustable t))
        (for n in list)
        (when (>= n (array-dimension v 0))
          (adjust-array v (1+ n)))
        (incf (aref v n))
        (finally (return v))))

(defun categorize-problem-csv (problem seed)
  (let ((*default-pathname-defaults*
         (ensure-directories-exist
          (merge-pathnames
           (format nil "~a/~a/~a/"
                   (name (domain problem))
                   seed
                   (name problem))
           *log-dir*)))
        start end
        (comparison 0)
        (evaluation 0)
        (restored-evaluation 0)
        usage)
    (match (prog2
             (progn (clear-plan-task-cache)
                    (setf start (now)))
             (handler-bind ((comparison-signal
                             (lambda (c)
                               (declare (ignorable c))
                               (incf comparison)))
                            (evaluation-signal
                             (lambda (c)
                               (incf evaluation)
                               (push (resource-usage c) usage)))
                            (restored-evaluation-signal
                             (lambda (c)
                               (declare (ignorable c))
                               (incf restored-evaluation))))
               (categorize-problem problem seed))
             (setf end (now)))
      ((list _ _ length/type length/structure length/plan)
       ;; (break+ length/type length/structure length/plan
       ;;         comparison evaluation restored-evaluation
       ;;         usage)
       (with-output-to-file (s "total" :if-exists :supersede)
         (print length/type s))
       (with-output-to-file (s "hist-structure" :if-exists :supersede)
         (cl-csv:write-csv (coerce (histogram length/structure) 'list)
                           :stream s))
       (with-output-to-file (s "hist-plan" :if-exists :supersede)
         (cl-csv:write-csv (coerce (histogram length/plan) 'list)
                           :stream s))
       (with-output-to-file (s "time" :if-exists :supersede)
         (format s "~f" (timestamp-difference end start)))
       (with-output-to-file (s "evaluation" :if-exists :supersede)
         (print evaluation s))
       (with-output-to-file (s "restored-evaluation" :if-exists :supersede)
         (print restored-evaluation s))
       (with-output-to-file (s "comparison" :if-exists :supersede)
         (print comparison s))
       (with-output-to-file (s "usage" :if-exists :supersede)
         (cl-csv:write-csv usage :stream s))))))

(defparameter *log-name*
  (merge-pathnames #p"logfile" *log-dir*))

(defun benchmark (problem-num)
  (log:config :daily *log-name*)
  (log:info "start categorization" problem-num)
  (let ((forced (nth problem-num *delayed-problems*)))
    (print forced)
    (sb-ext:gc :full t)
    (restart-return ((continue (lambda (c) c)))
      (apply #'categorize-problem-csv forced))))
