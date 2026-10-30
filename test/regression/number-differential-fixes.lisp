;;; Number fixes found by differential tests: COERCE of a complex to a float,
;;; RATIONAL and mixed comparisons at 2^63, EXPT of zero with a complex power,
;;; PARSE-INTEGER argument validation, and FLOAT of a large integer.

(defun %ndf-type-error-p (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error () :type-error)
    (error (c) (type-of c))))

(defun %ndf-division-by-zero-p (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (division-by-zero () :division-by-zero)
    (error (c) (type-of c))))

(deftest number-differential-fixes.coerce-complex-to-float
  (list (%ndf-type-error-p (lambda () (coerce #C(1 2) 'float)))
        (%ndf-type-error-p (lambda () (coerce #C(1 2) 'short-float)))
        (%ndf-type-error-p (lambda () (coerce #C(1 2) 'single-float)))
        (%ndf-type-error-p (lambda () (coerce #C(1 2) 'double-float)))
        (%ndf-type-error-p (lambda () (coerce #C(1 2) 'long-float)))
        (%ndf-type-error-p (lambda () (coerce #C(1.0d0 2.0d0) 'single-float))))
  (:type-error :type-error :type-error :type-error :type-error :type-error))

(deftest number-differential-fixes.rational-at-2^63
  (let ((f 9.223372036854776d18)
        (n 9223372036854775808))
    (list (rational f)
          (rational (- f))
          (= n f) (> n f) (<= n f) (/= n f) (>= n f) (< n f)))
  (9223372036854775808 -9223372036854775808 t nil t nil t nil))

(deftest number-differential-fixes.expt-zero-complex-power
  (list (%ndf-division-by-zero-p (lambda () (expt 0 #C(0 1))))
        (%ndf-division-by-zero-p (lambda () (expt 0.0d0 #C(0 1))))
        (%ndf-division-by-zero-p (lambda () (expt -0.0 #C(0 1))))
        (%ndf-division-by-zero-p (lambda () (expt 0 #C(0 -1))))
        (%ndf-division-by-zero-p (lambda () (expt 0 #C(-1 1))))
        (expt 0 #C(0 0))
        (expt 0 #C(1 1)))
  (:division-by-zero :division-by-zero :division-by-zero :division-by-zero
   :division-by-zero 1 0))

(deftest number-differential-fixes.parse-integer-validation
  (list (%ndf-type-error-p (lambda () (parse-integer "12" :radix 37)))
        (%ndf-type-error-p (lambda () (parse-integer "12" :radix 1)))
        (%ndf-type-error-p (lambda () (parse-integer "12" :radix 2.0)))
        (%ndf-type-error-p (lambda () (parse-integer "123" :start 2 :end 1)))
        (%ndf-type-error-p (lambda () (parse-integer "123" :start 4)))
        (parse-integer "101" :radix 2)
        (parse-integer "z" :radix 36)
        (parse-integer "123" :start 1 :end 2))
  (:type-error :type-error :type-error :type-error :type-error 5 35 2))

(deftest number-differential-fixes.float-bignum-rounding
  (list (float (1- (expt 2 64)) 0.0d0)
        (float (expt 10 30) 0.0d0)
        (float -98765432109876543210 0.0d0)
        (= (float (1- (expt 2 64)) 0.0d0)
           (coerce (1- (expt 2 64)) 'double-float)))
  (1.8446744073709552d19 1.0d30 -9.876543210987654d19 t))
