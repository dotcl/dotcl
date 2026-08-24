;;; FLOOR / CEILING / TRUNCATE / ROUND give the remainder the division produced.
;;;
;;; All four computed it as (- a (* q b)) instead. That is a multiplication by a
;;; quotient which, for the bignums these operations exist to handle, can be
;;; thousands of digits wide -- more work than the division itself, and the
;;; division had already produced the remainder and thrown it away. A rounding
;;; adjustment of one moves that remainder by exactly one divisor, so an addition
;;; finishes it.
;;;
;;; Float operands still take the old path: their remainder has to be a float.
;;;
;;; Every expected value here was read off SBCL.

(defvar *err-cases*
  '((100000000000000000000000000000000000000000 7)
    (-100000000000000000000000000000000000000000 7)
    (100000000000000000000000000000000000000000 -7)
    (-100000000000000000000000000000000000000000 -7)
    (17 5) (-17 5) (17 -5) (-17 -5)
    (7/2 1/3) (-7/2 1/3) (7/2 -1/3)
    (10 5) (-10 5) (10 2/5)))

(defun %err-apply (f)
  (mapcar (lambda (c) (multiple-value-list (funcall f (first c) (second c)))) *err-cases*))

(deftest exact-rounding-remainder.floor
  (%err-apply #'floor)
  ((14285714285714285714285714285714285714285 5)
   (-14285714285714285714285714285714285714286 2)
   (-14285714285714285714285714285714285714286 -2)
   (14285714285714285714285714285714285714285 -5)
   (3 2) (-4 3) (-4 -3) (3 -2)
   (10 1/6) (-11 1/6) (-11 -1/6)
   (2 0) (-2 0) (25 0)))

(deftest exact-rounding-remainder.ceiling
  (%err-apply #'ceiling)
  ((14285714285714285714285714285714285714286 -2)
   (-14285714285714285714285714285714285714285 -5)
   (-14285714285714285714285714285714285714285 5)
   (14285714285714285714285714285714285714286 2)
   (4 -3) (-3 -2) (-3 2) (4 3)
   (11 -1/6) (-10 -1/6) (-10 1/6)
   (2 0) (-2 0) (25 0)))

(deftest exact-rounding-remainder.truncate
  (%err-apply #'truncate)
  ((14285714285714285714285714285714285714285 5)
   (-14285714285714285714285714285714285714285 -5)
   (-14285714285714285714285714285714285714285 5)
   (14285714285714285714285714285714285714285 -5)
   (3 2) (-3 -2) (-3 2) (3 -2)
   (10 1/6) (-10 -1/6) (-10 1/6)
   (2 0) (-2 0) (25 0)))

(deftest exact-rounding-remainder.round
  (%err-apply #'round)
  ((14285714285714285714285714285714285714286 -2)
   (-14285714285714285714285714285714285714286 2)
   (-14285714285714285714285714285714285714286 -2)
   (14285714285714285714285714285714285714286 2)
   (3 2) (-3 -2) (-3 2) (3 -2)
   (10 1/6) (-10 -1/6) (-10 1/6)
   (2 0) (-2 0) (25 0)))

;;; The property the remainder has to satisfy, over a wider set than the tables
;;; above: a is q*b + r, exactly.
(deftest exact-rounding-remainder.identity-holds
  (let ((vals (list 1 -1 7 -7 (expt 10 40) (- (expt 10 40)) (1+ (expt 2 62))
                    1/3 -1/3 7/2 (/ (expt 10 30) 7)))
        (bad '()))
    (dolist (a vals (or bad t))
      (dolist (b vals)
        (dolist (f (list #'floor #'ceiling #'truncate #'round))
          (multiple-value-bind (q r) (funcall f a b)
            (unless (= a (+ (* q b) r)) (push (list a b q r) bad)))))))
  t)

;;; A float operand keeps a float remainder.
(deftest exact-rounding-remainder.float-operands-unchanged
  (list (multiple-value-list (floor 7.5 2))
        (multiple-value-list (truncate -7.5 2))
        (multiple-value-list (ceiling 7.5d0 2))
        (multiple-value-list (round 7.5 2)))
  ((3 1.5) (-3 -1.5) (4 -0.5d0) (4 -0.5)))

;;; The point: the remainder no longer costs a multiplication by the quotient, so
;;; a bignum division allocates a bounded amount rather than a second big number.
(defun %err-truncate-loop (n a b)
  (declare (fixnum n))
  (let ((s 0))
    (dotimes (i n s) (setq s (truncate a b)))))

(defun %err-bytes-for (n)
  (let ((a (expt 7 4000)) (b (expt 3 2000)) (best nil))
    (dotimes (r 5 best)
      (let ((before (nth 4 (dotcl:gc-stats))))
        (%err-truncate-loop n a b)
        (let ((used (- (nth 4 (dotcl:gc-stats)) before)))
          (when (or (null best) (< used best)) (setq best used)))))))

;; Compiled-only: an allocation measurement of a compiled loop.
(deftest-compiled-only exact-rounding-remainder.does-not-rebuild-the-remainder
  (progn (%err-bytes-for 20)                ; warm
         (let ((small (%err-bytes-for 100))
               (large (%err-bytes-for 1100)))
           ;; Measured per call on these operands: 5292 bytes when the
           ;; remainder was rebuilt as (- a (* q b)), 3413 when it comes from
           ;; the division. The bound sits between them, for 1000 extra calls.
           (< (- large small) 4500000)))
  t)
