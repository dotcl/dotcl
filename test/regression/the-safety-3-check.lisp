;;; Under (optimize (safety 3)) THE checks the value against the type it asserts and keeps the
;;; multiple values; at the default safety THE is trusted as before.

(deftest-emitting-only the-safety-3.checks
  (values (handler-case (funcall (compile nil '(lambda (x) (declare (optimize (safety 3))) (the string x))) 1) (type-error () :type-error))
          (handler-case (funcall (compile nil '(lambda (x) (declare (optimize (safety 3))) (the fixnum x))) 'a) (type-error () :type-error))
          (handler-case (funcall (compile nil '(lambda (x) (declare (optimize (safety 3))) (the fixnum (car x)))) '(a)) (type-error () :type-error))
          (handler-case (funcall (compile nil '(lambda (x) (declare (optimize safety)) (the (integer 0 5) x))) 9) (type-error () :type-error)))
  :type-error :type-error :type-error :type-error)

(deftest-emitting-only the-safety-3.accepts-and-keeps-values
  (values (funcall (compile nil '(lambda (x) (declare (optimize (safety 3))) (the string x))) "ok")
          (funcall (compile nil '(lambda (x) (declare (optimize (safety 3))) (the (or null string) x))) nil)
          (multiple-value-list (funcall (compile nil '(lambda (x) (declare (optimize (safety 3))) (the integer (floor x 2)))) 7))
          (multiple-value-list (funcall (compile nil '(lambda (x) (declare (optimize (safety 3))) (the (values integer integer) (values x 2)))) 1)))
  "ok" nil (3 1) (1 2))

(deftest-emitting-only the-safety-3.default-safety-trusts
  (funcall (compile nil '(lambda (x) (the string x))) 1)
  1)

;;; A LET / LET* variable declared with a type is checked when it is bound, at (safety 3) only.
(deftest-emitting-only let-declared-type-safety-3.checks
  (values (handler-case (funcall (compile nil '(lambda (x) (declare (optimize (safety 3))) (let ((y x)) (declare (type string y)) y))) 1) (type-error () :type-error))
          (handler-case (funcall (compile nil '(lambda (x) (declare (optimize (safety 3))) (let* ((y x)) (declare (fixnum y)) y))) 'a) (type-error () :type-error))
          (handler-case (funcall (compile nil '(lambda (x) (declare (optimize (safety 3))) (let* ((a 1) (y (car x))) (declare (type (integer 0 3) y)) (+ a y)))) '(9)) (type-error () :type-error)))
  :type-error :type-error :type-error)

(deftest-emitting-only let-declared-type-safety-3.accepts
  (values (funcall (compile nil '(lambda (x) (declare (optimize (safety 3))) (let ((y x)) (declare (type string y)) y))) "s")
          (funcall (compile nil '(lambda (x) (declare (optimize (safety 3))) (let* ((a 1) (y (car x))) (declare (type (integer 0 3) y)) (+ a y)))) '(2))
          (funcall (compile nil '(lambda (x) (let ((y x)) (declare (type string y)) y))) 1))
  "s" 3 1)

;;; A declaimed FTYPE that is wrong about a parameter (as in real libraries) does not make a function
;;; compiled under (safety 0) reject the argument; at the default safety it still does.
(deftest-emitting-only ftype-arg-check-safety-0.skipped
  (progn
    (eval '(declaim (ftype (function ((unsigned-byte 32) (unsigned-byte 5)) t) ftype-wrong-s0 ftype-wrong-s1)))
    (eval '(defun ftype-wrong-s0 (a s) (declare (optimize (safety 0)) (type (integer 0 32) s)) (+ a s)))
    (funcall 'ftype-wrong-s0 1 32))
  33)

(deftest-emitting-only ftype-arg-check-default-safety.signals
  (progn
    (eval '(declaim (ftype (function ((unsigned-byte 32) (unsigned-byte 5)) t) ftype-wrong-s1)))
    (eval '(defun ftype-wrong-s1 (a s) (+ a s)))
    (handler-case (funcall 'ftype-wrong-s1 1 32) (type-error () :type-error)))
  :type-error)
