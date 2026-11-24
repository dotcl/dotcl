;;; At (safety 3) a variable declared FIXNUM whose init is provably a fixnum is an int64 slot, so a later
;;; SETQ of a non-fixnum is a TYPE-ERROR, in a LET, a LET* and an &AUX variable alike. The init of
;;; the slot used to be wrapped in a checking call, which made it a boxed slot nothing checked again.

(defun s3f-let () (declare (optimize (safety 3))) (let ((x 1)) (declare (fixnum x)) (setq x (identity 2.5)) x))
(defun s3f-let* () (declare (optimize (safety 3))) (let* ((x 1)) (declare (fixnum x)) (setq x (identity "s")) x))
(defun s3f-aux (&aux (x 1)) (declare (fixnum x) (optimize (safety 3))) (setq x (identity 2.5)) x)
(defun s3f-aux-ok (&aux (x 1)) (declare (fixnum x) (optimize (safety 3))) (setq x (+ x 41)) x)
(defun s3f-init (v) (declare (optimize (safety 3))) (let ((x (identity v))) (declare (fixnum x)) x))
(defun s3f-param (x) (declare (fixnum x) (optimize (safety 3))) (setq x (identity 2.5)) x)

(deftest-emitting-only safety3-fixnum-setq.non-fixnum-setq-is-a-type-error
  (flet ((err (thunk) (handler-case (funcall thunk) (type-error (c) (progn c :type-error)))))
    (list (err #'s3f-let) (err #'s3f-let*) (err #'s3f-aux) (err (lambda () (s3f-param 1)))))
  (:type-error :type-error :type-error :type-error))

(deftest-emitting-only safety3-fixnum-setq.fixnum-values-are-unchanged
  (list (s3f-aux-ok)
        (s3f-init 7)
        (handler-case (s3f-init "s") (type-error () :type-error))
        (handler-case (s3f-init 1.5d0) (type-error () :type-error)))
  (42 7 :type-error :type-error))
