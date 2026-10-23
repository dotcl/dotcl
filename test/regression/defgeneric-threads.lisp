;;; DEFGENERIC and ENSURE-GENERIC-FUNCTION of a name that has no generic function
;;; yet, from several threads at once. Each made its own generic function and
;;; registered it over whatever another thread had registered meanwhile, so the
;;; threads went on with different generic functions, and the methods added to
;;; all but the last one were lost. Every thread must get the one that stays.

(require "dotcl-thread")

(defun %dgt-run (makers)
  "MAKERS: one function per name, which defines it and returns the generic
   function. Run each from 4 threads at once; count the names where a thread got
   a generic function other than the one finally defined."
  (let* ((nthreads 4) (go nil)
         (got (make-array nthreads))
         (threads (loop for i below nthreads
                        collect (let ((i i))
                                  (dotcl-thread:make-thread
                                   (lambda ()
                                     (loop until go do (dotcl-thread:thread-yield))
                                     (setf (aref got i)
                                           (loop for (nil . fn) in makers collect (funcall fn)))))))))
    (setf go t)
    (mapc #'dotcl-thread:thread-join threads)
    (loop for (name) in makers
          for k from 0
          count (loop for i below nthreads
                      thereis (not (eq (nth k (aref got i)) (fdefinition name)))))))

(deftest-compiled-only defgeneric-threads.defgeneric
  (%dgt-run (loop for r below 200
                  collect (let ((g (intern (format nil "%DGT-DG-~D" r))))
                            (cons g (compile nil `(lambda () (defgeneric ,g (x))))))))
  0)

(deftest defgeneric-threads.ensure-generic-function
  (%dgt-run (loop for r below 200
                  collect (let ((g (intern (format nil "%DGT-EGF-~D" r))))
                            (cons g (lambda () (ensure-generic-function g :lambda-list '(x)))))))
  0)
