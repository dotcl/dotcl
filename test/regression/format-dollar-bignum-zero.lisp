;;; ~$ must handle bignums and n = 0.
;;;
;;; CLHS 22.3.3.4 says a rational argument is coerced to a single float and
;;; printed. dotcl's conversion had no bignum case, so a large integer printed
;;; "0.00". The same section says exactly n digits precede the decimal point,
;;; so n = 0 leaves no integer digit even when the integer part is zero.
;;;
;;; Expected values are SBCL 2.6.8's.

(deftest format-dollar-bignum-zero.large-integer
  (list (format nil "~$" (expt 10 20))
        (format nil "~$" (expt 2 63))
        (format nil "~$" (expt 10 30))
        (format nil "~$" (- (expt 10 20))))
  ("100000000000000000000.00"
   "9223372000000000000.00"
   "1000000000000000000000000000000.00"
   "-100000000000000000000.00"))

(deftest format-dollar-bignum-zero.no-integer-digits
  (list (format nil "~0,0$" 0)
        (format nil "~0,0$" 1/3)
        (format nil "~0,0$" 1.0d-5)
        (format nil "~0,0$" 1)
        (format nil "~2,0$" 0))
  ("." "." "." "1." ".00"))
