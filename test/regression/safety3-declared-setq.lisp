;;; At (safety 3) a store into a variable with a declared type is checked, whatever slot the variable
;;; lives in: a LET* variable whose init the compiler could not prove, a variable a closure captures,
;;; a variable declared a type that is not FIXNUM. The init of a LET whose own declaration asks for
;;; safety 3 is checked too. The default safety checks none of these (docs/deviations.md).

(defmacro s3d-err (form)
  `(handler-case ,form (type-error (c) (list :type-error (type-error-datum c)))))

(defun s3d-let*-setq ()
  (let* ((x 1) (y (+ x 1))) (declare (optimize (safety 3)) (fixnum x y)) (setq y 1.5) (list x y)))
(defun s3d-flet-setq ()
  (let ((x 5)) (declare (optimize (safety 3)) (fixnum x)) (flet ((f () (setq x 'closure))) (f)) x))
(defun s3d-lambda-setq ()
  (let ((x 5)) (declare (optimize (safety 3)) (fixnum x)) (let ((f (lambda () (setq x 2.5)))) (funcall f)) x))
(defun s3d-range-setq ()
  (let ((x 5)) (declare (optimize (safety 3)) (type (integer 0 10) x)) (let ((f (lambda () (setq x 11)))) (funcall f)) x))
(defun s3d-string-setq (v)
  (let ((x "a")) (declare (optimize (safety 3)) (type string x)) (let ((f (lambda () (setq x v)))) (funcall f)) x))
(defun s3d-init-let ()
  (let ((x (car (list 1.5)))) (declare (optimize (safety 3)) (fixnum x)) x))
(defun s3d-init-let* ()
  (let* ((x 1) (y (car (list 2.5)))) (declare (optimize (safety 3)) (fixnum y)) (list x y)))
(defun s3d-init-double ()
  (let ((x (car (list 1)))) (declare (optimize (safety 3)) (double-float x)) x))
(defun s3d-valid ()
  (let* ((x 1) (y (+ x 1)) (z "a") (w 5))
    (declare (optimize (safety 3)) (fixnum x y) (type string z) (type (integer 0 10) w))
    (let ((f (lambda () (setq w 10) (setq z "b"))))
      (funcall f))
    (setq y 7)
    (incf y 2)
    (list x y z w)))
;; The same forms at the default safety: the declaration is trusted, as docs/deviations.md says.
(defun s3d-default-let*-setq ()
  (let* ((x 1) (y (+ x 1))) (declare (fixnum x y)) (setq y 1.5) (list x y)))
(defun s3d-default-lambda-setq ()
  (let ((x 5)) (declare (type (integer 0 10) x)) (let ((f (lambda () (setq x 11)))) (funcall f)) x))
(defun s3d-default-init ()
  (let ((x (car (list 1.5)))) (declare (fixnum x)) x))

(deftest-emitting-only safety3-declared-setq.setq-of-a-wrong-type-is-a-type-error
  (list (s3d-err (s3d-let*-setq)) (s3d-err (s3d-flet-setq)) (s3d-err (s3d-lambda-setq))
        (s3d-err (s3d-range-setq)) (s3d-err (s3d-string-setq 5)))
  ((:type-error 1.5) (:type-error closure) (:type-error 2.5) (:type-error 11) (:type-error 5)))

(deftest-emitting-only safety3-declared-setq.init-with-its-own-safety-declaration
  (list (s3d-err (s3d-init-let)) (s3d-err (s3d-init-let*)) (s3d-err (s3d-init-double)))
  ((:type-error 1.5) (:type-error 2.5) (:type-error 1)))

(deftest-emitting-only safety3-declared-setq.valid-stores-are-unchanged
  (list (s3d-valid) (s3d-string-setq "ok"))
  ((1 9 "b" 10) "ok"))

(deftest-emitting-only safety3-declared-setq.default-safety-trusts-the-declaration
  (list (s3d-default-let*-setq) (s3d-default-lambda-setq) (s3d-default-init))
  ((1 1.5) 11 1.5))
