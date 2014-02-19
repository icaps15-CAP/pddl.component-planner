
(print "This is script.lisp")

(defun run (i)
  (let ((process (sb-ext:run-program
                  "sbcl"
                  (list "--core"
                        (format nil "~a/repos/core.original" (user-homedir-pathname))
                        "--dynamic-space-size"
                        "15500"
                        "--non-interactive"
                        "--load"
                        "script2.lisp"
                        "--eval"
                        (format nil "(pddl.component-planner-test::run-benchmark ~a)" i))
                  :search t
                  :output *standard-output*)))
    (when (not (= 0 (process-exit-code process)))
      (format t "The child sbcl process has stopped with some error!"))))


(let* ((ms '("fun003"
             "fun004"
             "fun005"
             "fun007"))
       (all 8)
       (n (position (machine-instance) ms :test #'string=))
       (per-machine (floor all (length ms))))
  (dotimes (i per-machine)
    (run (+ i (* n per-machine)))))

(exit)