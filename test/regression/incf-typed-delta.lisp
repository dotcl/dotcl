;;; INCF / DECF of a typed variable by a typed delta stays on the typed path.
;;; The delta is bound to a temporary (it is evaluated after the place's
;;; subforms and before the place is read), and that temporary was not
;;; declared, so (incf acc (aref v i)) on a DOUBLE-FLOAT ACC boxed the element
;;; and added generically -- 72 B an element -- where
;;; (setq acc (+ acc (aref v i))) was 0 B.

(defun %itd-sum (v n)
  (declare (type (simple-array double-float (*)) v) (fixnum n))
  (let ((acc 0d0))
    (declare (double-float acc))
    (dotimes (k n)
      (dotimes (i (length v))
        (incf acc (aref v i))
        (decf acc (* 0.5d0 (aref v i)))))
    acc))

(defun %itd-sum-single (v)
  (declare (type (simple-array single-float (*)) v))
  (let ((acc 0.0))
    (declare (single-float acc))
    (dotimes (i (length v)) (incf acc (aref v i)))
    acc))

;; The delta is still evaluated after the place's subforms and before the
;; place is read.
(defun %itd-order ()
  (let ((x 1) (log '()))
    (declare (fixnum x))
    (incf x (progn (push :delta log) (setq x 10) 1))
    (list x log)))

(deftest incf-typed-delta.values
  (list (%itd-sum (make-array 4 :element-type 'double-float :initial-contents '(1d0 2d0 3d0 4d0)) 2)
        (%itd-sum-single (make-array 3 :element-type 'single-float :initial-contents '(1.0 2.0 3.0)))
        (%itd-order))
  (10d0 6.0 (11 (:delta))))

(deftest-emitting-only incf-typed-delta.no-allocation
  (let ((v (make-array 16 :element-type 'double-float :initial-element 1d0)))
    (%itd-sum v 100)
    (let ((best nil))
      (dotimes (k 3)
        (let ((b0 (nth 4 (dotcl:gc-stats))))
          (%itd-sum v 1000)
          (let ((d (- (nth 4 (dotcl:gc-stats)) b0)))
            (setf best (if best (min best d) d)))))
      ;; 16,000 INCFs and as many DECFs: 2,304,000 B when the delta was boxed.
      (< best 4000)))
  t)
