;;; LOAD-TIME-VALUE under COMPILE from several threads at once. The slot id
;;; each LOAD-TIME-VALUE form gets was taken with a plain INCF of a global
;;; counter, so two threads could take the same id, and a compiled lambda
;;; returned the value of another thread's form.

(require "dotcl-thread")

(deftest ltv-compile-threads.distinct-slots
  (let* ((nthreads 4) (per 200) (go nil)
         (bad (make-array nthreads :initial-element 0))
         (threads
           (loop for i below nthreads
                 collect (let ((i i))
                           (dotcl-thread:make-thread
                            (lambda ()
                              (loop until go do (dotcl-thread:thread-yield))
                              (dotimes (j per)
                                (let* ((k (+ (* 100000 i) j))
                                       (f (compile nil `(lambda () (load-time-value (list ,k))))))
                                  (unless (equal (funcall f) (list k))
                                    (incf (aref bad i)))))))))))
    (setf go t)
    (mapc #'dotcl-thread:thread-join threads)
    (reduce #'+ bad))
  0)
