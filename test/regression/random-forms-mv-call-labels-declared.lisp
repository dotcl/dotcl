;;; MULTIPLE-VALUE-CALL of #'+ over NOTINLINE local functions, one returning
;;; two values of a closed-over variable with a declared integer range. SBCL
;;; 2.6.8's COMPILE signals a TYPE-ERROR on this shape, so the random
;;; differential test cannot compare it; the value is checked here instead.

(defun %rfmc (c d)
  (declare (type (integer -2 1) c))
  (labels ((f2 () 24) (f3 (p) (declare (ignore p)) (values c d)))
    (declare (notinline f2 f3))
    (multiple-value-call #'+ (f2) (multiple-value-call #'f3 0))))

(deftest random-forms-mv-call-labels-declared.values
  (list (%rfmc -2 10) (%rfmc 1 395994098631) (%rfmc 0 -24))
  (32 395994098656 0))
