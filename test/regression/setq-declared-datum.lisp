;;; A SETQ (or init) of a declared FIXNUM / DOUBLE-FLOAT / SINGLE-FLOAT variable held in a native
;;; slot signals a TYPE-ERROR that names the value and the declared type.

(defun %sdd-datum (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error (c) (list (type-error-datum c) (type-error-expected-type c)))))

(deftest-emitting-only setq-datum.fixnum-float
  (%sdd-datum (lambda () (let ((x 1)) (declare (optimize (safety 3)) (fixnum x)) (setq x (car (list 2.5))) x)))
  (2.5 fixnum))

(deftest-emitting-only setq-datum.fixnum-string
  (%sdd-datum (lambda () (let ((x 1)) (declare (optimize (safety 3)) (fixnum x)) (setq x (car (list "s"))) x)))
  ("s" fixnum))

(deftest-emitting-only setq-datum.fixnum-bignum
  (%sdd-datum (lambda () (let ((x 1)) (declare (optimize (safety 3)) (fixnum x)) (setq x (car (list (expt 10 30))) ) x)))
  (1000000000000000000000000000000 fixnum))

(deftest-emitting-only setq-datum.fixnum-incf
  (%sdd-datum (lambda () (let ((x 5)) (declare (optimize (safety 3)) (fixnum x)) (incf x (car (list 1.5))) x)))
  (6.5 fixnum))

(deftest-emitting-only setq-datum.fixnum-aux
  (%sdd-datum (lambda () (funcall (lambda (&aux (x 1)) (declare (optimize (safety 3)) (fixnum x)) (setq x (car (list 2.5))) x))))
  (2.5 fixnum))

(deftest-emitting-only setq-datum.double-from-integer
  (%sdd-datum (lambda () (let ((x 1.5d0)) (declare (optimize (safety 3)) (double-float x)) (setq x (car (list 1))) x)))
  (1 double-float))

(deftest-emitting-only setq-datum.double-from-symbol
  (%sdd-datum (lambda () (let ((x 1.5d0)) (declare (optimize (safety 3)) (double-float x)) (setq x (car (list 'a))) x)))
  (a double-float))

(deftest-emitting-only setq-datum.single-from-double
  (%sdd-datum (lambda () (let ((x 1.5f0)) (declare (optimize (safety 3)) (single-float x)) (setq x (car (list 1.5d0))) x)))
  (1.5d0 single-float))

(deftest setq-datum.valid-stores-unchanged
  (let ((x 1) (d 1.5d0) (s 1.5f0))
    (declare (optimize (safety 3)) (fixnum x) (double-float d) (single-float s))
    (setq x 7 d 2.5d0 s 3.5f0)
    (list x d s))
  (7 2.5d0 3.5f0))
