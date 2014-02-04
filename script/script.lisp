
(print "This is script.lisp")

(defun run (i)
  (sb-ext:run-program
   "sbcl"
   (list " --load script2.lisp"
         (princ-to-string i))
   :search t
   :output t))

(handler-bind ((error (lambda (c)
                        (exit :code 1))))
  (let ((m (machine-instance)))
    (cond
      ((string= m "fun003")
       (loop for i from 0 to 3 do (run i))
      ((string= m "fun007")
       (loop for i from 4 to 7 do (run i)))))))
