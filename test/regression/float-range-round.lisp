;;; FLOOR / TRUNCATE / CEILING / ROUND of a float whose range is known (a
;;; ranged float declaration, arithmetic on it, an unassigned LET of it) give
;;; an integer computed raw. The values must be those of the generic entry, and
;;; where the range is not known (too wide, assigned, shadowed) nothing changes.

(setf dotcl:*save-sil* t)

(defun %frg-s (x w)
  (declare (type (single-float 0f0 1f0) x) (type (unsigned-byte 32) w))
  (let ((y (- (* x w) 0.5f0)))
    (list (floor y) (truncate y) (ceiling y) (round y)
          (multiple-value-bind (q r) (truncate y) (list q r))
          (multiple-value-bind (q r) (floor y) (list (+ q 1) r)))))

(defun %frg-d (x)
  (declare (type (double-float -1d0 1d0) x))
  (let* ((y (* x 1d15))
         (z (+ y 0.5d0)))
    (list (floor z) (truncate z) (ceiling z) (round z) (- (round y) 1))))

(defun %frg-generic (y)
  (let ((fl #'floor) (tr #'truncate) (ce #'ceiling) (ro #'round))
    (list (values (funcall fl y)) (values (funcall tr y))
          (values (funcall ce y)) (values (funcall ro y)))))

;; Too wide for an int64: the quotient is a bignum.
(defun %frg-wide (x)
  (declare (type (double-float 0d0 1d0) x))
  (let ((y (* x 1d30)))
    (truncate y)))

;; The LET rebinds X: the init's X is the parameter, the body's the new one.
(defun %frg-shadow (x)
  (declare (type (double-float 0d0 1d0) x))
  (let ((x (* x 1d30)))
    (let ((x (truncate x)))
      (+ x 1))))

;; In a LET* a later init that names an earlier binding sees that binding.
(defun %frg-let* (a)
  (declare (type (double-float 0d0 1d0) a) (ignorable a))
  (let* ((a 1d20) (b (truncate a)))
    (+ b 1)))

;; In a parallel LET it sees the outer one.
(defun %frg-let (a)
  (declare (type (double-float 0d0 1d0) a))
  (let ((a 1d20) (b (truncate (* a 4))))
    (list (truncate a) b)))

;; An assigned variable is not given its init's range.
(defun %frg-assigned (x)
  (declare (type (double-float 0d0 1d0) x))
  (let ((y (* x 10)))
    (setq y (* y 1d20))
    (truncate y)))

;; Integer ranges of a same-name rebinding.
(defun %frg-int-shadow (n)
  (declare (type (integer 0 10) n))
  (let ((n (* n 1000000000)))
    (let ((n (* n n)))
      (* n 1000))))

(deftest float-range-round.single
  (list (%frg-s 0f0 100) (%frg-s 1f0 100) (%frg-s 0.5f0 3) (%frg-s 1f0 #xffffffff)
        (%frg-s 0.37f0 1000))
  ((-1 0 0 0 (0 -0.5) (0 0.5)) (99 99 100 100 (99 0.5) (100 0.5))
   (1 1 1 1 (1 0.0) (2 0.0)) (4294967296 4294967296 4294967296 4294967296 (4294967296 0.0) (4294967297 0.0))
   (369 369 370 370 (369 0.5) (370 0.5))))

(deftest float-range-round.double
  (loop for x in '(-1d0 -0.3d0 0d0 0.123456789d0 1d0)
        for y = (+ (* x 1d15) 0.5d0)
        unless (equal (butlast (%frg-d x)) (%frg-generic y))
          collect x)
  nil)

(deftest float-range-round.unknown-range
  (list (%frg-wide 1d0) (%frg-shadow 1d0) (%frg-let* 0.5d0) (%frg-let 0.5d0)
        (%frg-assigned 1d0) (%frg-int-shadow 10))
  (1000000000000000019884624838656 1000000000000000019884624838657
   100000000000000000001 (100000000000000000000 2) 1000000000000000000000
   100000000000000000000000))

(deftest-compiled-only float-range-round.raw
  (let ((s (%flat-text (dotcl:function-sil #'%frg-s))))
    (list (search "FloatIntegerS" s) (search "TruncateOp" s)))
  (nil nil))

(setf dotcl:*save-sil* nil)
