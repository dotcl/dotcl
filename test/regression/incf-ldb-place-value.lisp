;;; INCF and DECF return the new value of the place; for an LDB or MASK-FIELD place
;;; that is the field (the byte stored), not the integer around it. SETF of the
;;; place already did so.

(deftest incf-ldb-place-value.ldb
  (list (let ((x 0)) (list (incf (ldb (byte 2 1) x) 5) x))
        (let ((x 0)) (list (decf (ldb (byte 2 1) x) 5) x))
        (let ((x 0)) (list (incf (ldb (byte 0 0) x) 5) x))
        (let ((x 0)) (list (incf (ldb (byte 2 100) x) 1) x))
        (let ((x 5)) (list (incf (ldb (byte 4 0) x) 3) x)))
  ((5 2) (-5 6) (5 0) (1 #.(expt 2 100)) (8 8)))

(deftest incf-ldb-place-value.mask-field
  (list (let ((x 0)) (list (incf (mask-field (byte 2 1) x) 5) x))
        (let ((x 0)) (list (incf (mask-field (byte 8 0) x) 300) x))
        (let ((x 0)) (list (decf (mask-field (byte 2 1) x) 1) x)))
  ((5 4) (300 44) (-1 6)))

(deftest incf-ldb-place-value.compound-place
  (let ((v (vector 0 7)) (i 0))
    (list (incf (ldb (byte 3 2) (aref v (incf i 0))) 9) (aref v 0) i
          (decf (mask-field (byte 3 0) (svref v 1)) 2) (svref v 1)))
  (9 4 0 5 5))

(deftest incf-ldb-place-value.setf-and-others-unchanged
  (let ((x 0))
    (list (setf (ldb (byte 2 1) x) 5) x
          (let ((y 255)) (list (incf (ldb (byte 8 0) y) 1) y))))
  (5 2 (256 0)))
