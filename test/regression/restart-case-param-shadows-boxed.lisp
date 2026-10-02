;;; A RESTART-CASE clause parameter that has the name of an enclosing variable
;;; which is assigned (and so may live in a box) inherited the boxed
;;; representation: a reference to the parameter read the restart argument as
;;; if it were a box. COMPILE failed with a .NET NullReferenceException, or the
;;; reference read garbage ("Not a number: "). HANDLER-CASE clause variables had
;;; the same problem and were fixed earlier. Found by the random integer form
;;; test with its extra shapes (make test-random-forms RANDOM_EXTRA=1).

(defun %rcpsb-setq ()
  (let ((xv 100))
    (setq xv 3)
    (list (restart-case (invoke-restart 'use-value 4) (use-value (xv) xv)) xv)))

(defun %rcpsb-closure ()
  (let ((xv 100))
    (funcall (lambda () (incf xv)))
    (list (restart-case (invoke-restart 'use-value 4 5)
            (use-value (xv &optional (y 0)) (+ xv y)))
          xv)))

(defun %rcpsb-random-form (c)
  (let ((xv (make-array 2 :element-type '(signed-byte 32) :initial-element 0)))
    (handler-bind ((error (lambda (xc) (declare (ignore xc)) (invoke-restart 'use-value 7))))
      (restart-case (error "e ~A" (let ((xh (make-hash-table)))
                                    (setf (gethash 1 xh) 1994 (gethash 2 xh) c)
                                    (loop for xk being the hash-keys of xh using (hash-value xv)
                                          sum xk)))
        (use-value (xv) (+ xv (length (the vector (make-array 1)))))))))

(deftest restart-case-param-shadows-boxed
  (list (%rcpsb-setq) (%rcpsb-closure) (%rcpsb-random-form 5))
  ((4 3) (9 101) 8))
