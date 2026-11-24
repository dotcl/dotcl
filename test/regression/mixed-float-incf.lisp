;;; INCF / DECF of a double-float accumulator by a single-float or rational
;;; delta keeps the accumulator in a raw double slot.

(defun %mfi-sum ()
  (let ((f 0d0))
    (dotimes (i 100000)
      (multiple-value-bind (q r) (truncate (* i 17) 13)
        (incf f (/ (float r) (1+ q)))))
    (* f 1d0)))

(defun %mfi-small ()
  (let ((f 0d0))
    (dotimes (i 1000)
      (multiple-value-bind (q r) (truncate (* i 17) 13)
        (incf f (/ (float r) (1+ q)))
        (decf f (float q))))
    (* f 1d0)))

(defun %mfi-ratio ()
  (let ((f 0d0))
    (dotimes (i 10) (incf f (/ i 3)))
    (* f 1d0)))

(defun %mfi-complex ()
  (let ((f 0d0))
    (dotimes (i 3) (incf f (complex i 1)))
    f))

(defun %mfi-int-first ()
  (let ((f 0d0))
    (dotimes (i 5) (incf f i))
    (* f 1d0)))

(deftest-emitting-only mixed-float-incf.no-allocation
  (progn
    (%mfi-sum)
    (< (min-allocated-bytes (lambda () (%mfi-sum))) 10000))
  t)

(deftest mixed-float-incf.values
  (list (%mfi-sum) (%mfi-small) (%mfi-ratio) (%mfi-complex) (%mfi-int-first)
        (type-of (%mfi-sum)))
  (54.074764680090084d0 -652698.0566403933d0 15.0d0 #C(3.0d0 3.0d0) 10.0d0
   double-float))
