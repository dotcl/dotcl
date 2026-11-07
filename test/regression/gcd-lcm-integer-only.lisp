;;; GCD and LCM take integers only: a ratio is a TYPE-ERROR (it was silently truncated:
;;; (gcd 1/2 3) was 1).

(defmacro %glio-type-error-p (form) `(handler-case (progn (eval ',form) nil) (type-error () t)))

(deftest gcd-lcm-integer-only.ratio-rejected
  (list (%glio-type-error-p (gcd 1/2 3)) (%glio-type-error-p (lcm 1/2 3))
        (%glio-type-error-p (gcd 4 3/2)) (%glio-type-error-p (gcd 1.0 2))
        (let ((a (/ 1 2))) (handler-case (progn (gcd a 3) nil) (type-error () t))))
  (t t t t t))

(deftest gcd-lcm-integer-only.integers-unchanged
  (list (gcd) (gcd -4) (gcd 4 6) (gcd 12 18 24) (lcm) (lcm -4) (lcm 4 6) (lcm 0 5)
        (gcd (expt 10 30) (expt 10 20)) (lcm (expt 2 70) 3))
  (0 4 2 6 1 4 12 0 100000000000000000000 3541774862152233910272))
