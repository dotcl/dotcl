;;; (dpb V (byte SIZE POS) X) with literal SIZE and POS is compiled as shifts
;;; and masks instead of a call to DPB. These check that the open-coded form
;;; gives the same value as the function, that V is still evaluated before X,
;;; and that a non-integer is still rejected.

(defun %dpbc-inline (v x)
  (list (dpb v (byte 8 0) x)
        (dpb v (byte 8 8) x)
        (dpb v (byte 16 16) x)
        (dpb v (byte 32 0) x)
        (dpb v (byte 0 5) x)
        (dpb v (byte 64 3) x)
        (dpb v (byte 100 7) x)))

(defun %dpbc-generic (v x)
  (let ((f #'dpb))
    (list (funcall f v (byte 8 0) x)
          (funcall f v (byte 8 8) x)
          (funcall f v (byte 16 16) x)
          (funcall f v (byte 32 0) x)
          (funcall f v (byte 0 5) x)
          (funcall f v (byte 64 3) x)
          (funcall f v (byte 100 7) x))))

(deftest dpb-constant-bytespec.matches-function
  (let ((vals (list 0 1 -1 255 #xABCD #x12345678 -12345
                    most-positive-fixnum most-negative-fixnum
                    (expt 2 70) (- (expt 3 50)))))
    (loop for v in vals
          nconc (loop for x in vals
                      unless (equal (%dpbc-inline v x) (%dpbc-generic v x))
                        collect (list v x))))
  nil)

;; The shape ironclad's UB32REF/BE uses.
(defun %dpbc-ub32 (hi lo)
  (dpb hi (byte 16 16) lo))

(deftest dpb-constant-bytespec.ub32
  (list (%dpbc-ub32 #xDEAD #xBEEF) (%dpbc-ub32 0 #xFFFF) (%dpbc-ub32 #x1FFFF 0))
  (#xDEADBEEF #xFFFF #xFFFF0000))

(deftest dpb-constant-bytespec.evaluation-order
  (let ((log '()))
    (dpb (progn (push :v log) 1) (byte 8 8) (progn (push :x log) 0))
    (nreverse log))
  (:v :x))

(deftest dpb-constant-bytespec.not-an-integer
  (list (handler-case (progn (dpb (the t (identity 1.5)) (byte 8 0) 0) :no-error)
          (type-error () :type-error))
        (handler-case (progn (dpb 1 (byte 8 0) (the t (identity "x"))) :no-error)
          (type-error () :type-error)))
  (:type-error :type-error))
