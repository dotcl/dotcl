;;; The remainder of FLOOR / CEILING / TRUNCATE / ROUND of a ratio by an integer
;;; is already in lowest terms and is built without a GCD; a ratio by a ratio
;;; still reduces. GCDs of large operands with common powers of two strip them
;;; by shifts first.

(defun %rtr-check (op a b)
  (multiple-value-bind (q r) (funcall op a b)
    (list (integerp q) (= a (+ (* q b) r))
          (or (integerp r) (= 1 (gcd (numerator r) (denominator r)))))))

(deftest rational-truncate-remainder-reduced.ratio-by-integer
  (let ((ok t))
    (dolist (op (list #'floor #'ceiling #'truncate #'round) ok)
      (dolist (a (list 7/3 -7/3 (/ (1+ (expt 3 200)) (expt 2 300))
                       (- (/ (1+ (expt 3 200)) (expt 2 300))) 1/7))
        (dolist (b (list 10 -10 3 1 (expt 7 50)))
          (unless (equal (%rtr-check op a b) '(t t t)) (setq ok (list op a b)))))))
  t)

(deftest rational-truncate-remainder-reduced.values
  (list (multiple-value-list (truncate 7/3 2))
        (multiple-value-list (floor -7/3 2))
        (multiple-value-list (ceiling 7/3 2))
        (multiple-value-list (round 7/2 3))
        (multiple-value-list (truncate 7/2 1/4))
        (multiple-value-list (floor 9 3/2)))
  ((1 1/3) (-2 5/3) (2 -5/3) (1 1/2) (14 0) (6 0)))

(deftest rational-truncate-remainder-reduced.gcd-powers-of-two
  (let* ((a (* (expt 2 400) (expt 3 90) 5))
         (b (* (expt 2 333) (expt 3 70) 7))
         (s (+ (/ 1 a) (/ 1 b))))
    (list (gcd a b)
          (= (gcd a b) (* (expt 2 333) (expt 3 70)))
          (= s (/ (+ a b) (* a b)))
          (= 1 (gcd (numerator s) (denominator s)))
          (+ (/ 3 (expt 2 300)) (/ 5 (expt 2 299)))))
  (#.(* (expt 2 333) (expt 3 70)) t t t #.(/ 13 (expt 2 300))))
