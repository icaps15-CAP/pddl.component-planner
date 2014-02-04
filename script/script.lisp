



(let ((m (machine-instance)))
  (cond
    ((string= m "fun003")
     (loop for i from 0 to 3
           do (sb-ext:run-program
                 (format nil "sbcl --load script2.lisp --eval \"(run ~a)\"" i))))
    ((string= m "fun007")
     (loop for i from 4 to 7
           do (sb-ext:run-program
                 (format nil "sbcl --load script2.lisp --eval \"(run ~a)\"" i))))))
