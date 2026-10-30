;;; The generic * and ZEROP test for two fixnums inline and leave every other
;;; case, the fixnum product that overflows included, to the out-of-line path.

(defun %gffp-mul (a b) (* a b))
(defun %gffp-zero (a) (if (zerop a) :zero :nonzero))
(defun %gffp-caddr (x) (caddr x))

(deftest generic-fixnum-fast-paths.multiply
  (list (%gffp-mul 3 -7) (%gffp-mul most-positive-fixnum 2) (%gffp-mul most-negative-fixnum -1)
        (%gffp-mul most-negative-fixnum 1) (%gffp-mul 4294967296 4294967296)
        (%gffp-mul -4294967296 4294967296) (%gffp-mul 2 1.5) (%gffp-mul 1/2 4) (%gffp-mul (expt 2 70) 0))
  (-21 #.(* most-positive-fixnum 2) #.(- most-negative-fixnum) #.most-negative-fixnum
   18446744073709551616 -18446744073709551616 3.0 2 0))

(deftest generic-fixnum-fast-paths.zerop
  (list (%gffp-zero 0) (%gffp-zero 5) (%gffp-zero 0.0) (%gffp-zero -0.0d0) (%gffp-zero (expt 2 70))
        (%gffp-zero 1/3) (%gffp-zero #c(0 1)) (%gffp-zero #c(0.0 0.0)))
  (:zero :nonzero :zero :zero :nonzero :nonzero :nonzero :zero))

(deftest generic-fixnum-fast-paths.caddr
  (list (%gffp-caddr '(1 2 3)) (%gffp-caddr '(1 2)) (%gffp-caddr nil)
        (handler-case (%gffp-caddr '(1 2 . 3)) (type-error () :type-error)))
  (3 nil nil :type-error))
