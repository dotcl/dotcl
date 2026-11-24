;;; (MULTIPLE-VALUE-BIND (Q R) (TRUNCATE form form) ...) with operands that are
;;; compound forms keeps Q and R as raw integers.

(defun %mff-sum ()
  (let ((acc 0))
    (declare (fixnum acc))
    (dotimes (i 100000)
      (multiple-value-bind (q r) (truncate (* i 17) 13)
        (setq acc (mod (+ acc q (* r 3)) 1000003))))
    acc))

(defun %mff-floor (a b)
  (multiple-value-bind (q r) (floor (+ a 1) (- b 0)) (list q r)))

(defun %mff-trunc (a b)
  (multiple-value-bind (q r) (truncate (* a 3) b) (list q r)))

(defun %mff-order ()
  (let ((log nil))
    (multiple-value-bind (q r) (truncate (progn (push :a log) 17) (progn (push :b log) 5))
      (list q r (reverse log)))))

(deftest-emitting-only mv-floor-forms.no-allocation
  (progn
    (%mff-sum)
    (< (min-allocated-bytes (lambda () (%mff-sum))) 10000))
  t)

(deftest mv-floor-forms.values
  (list (%mff-sum)
        (%mff-floor -7 2) (%mff-floor 0 5) (%mff-floor -1 -5)
        (%mff-floor (1- (expt 2 62)) 1)
        (%mff-floor (- (expt 2 62)) -1)
        (%mff-trunc -7 2) (%mff-trunc 0 5) (%mff-trunc -1 -5)
        (%mff-trunc (expt 2 70) 5)
        (%mff-trunc (- (expt 2 62)) -1)
        (%mff-floor 1.5 2)
        (handler-case (%mff-trunc 3 0) (division-by-zero () :dbz))
        (handler-case (%mff-floor 3 0) (division-by-zero () :dbz))
        (%mff-order))
  (130380 (-3 0) (0 1) (0 0) (4611686018427387904 0) (4611686018427387903 0)
   (-10 -1) (0 0) (0 -3) (708354972430446782054 2) (13835058055282163712 0)
   (1 0.5) :dbz :dbz (3 2 (:a :b))))
