;;; Malformed declarations are a PROGRAM-ERROR in the body of LAMBDA, DEFUN, DOLIST, DOTIMES, DO,
;;; DESTRUCTURING-BIND and the FLET / LABELS body, as they already are in LET.
(deftest-emitting-only declare-syntax-more.errors
  (flet ((bad (form) (handler-case (progn (funcall (compile nil form)) :no-error) (program-error () :program-error))))
    (list (bad '(lambda (x) (declare (ignore 1)) x))
          (bad '(lambda (x) (declare (special "x")) x))
          (bad '(lambda (x) (declare (dynamic-extent (foo 1))) x))
          (bad '(lambda (x) (declare (ftype)) x))
          (bad '(lambda () (flet ((f (a) a)) (declare (inline 1)) (f 1))))
          (bad '(lambda () (labels ((f (a) a)) (declare (ignore "x")) (f 1))))
          (bad '(lambda () (dolist (x '(1)) (declare (ignore 1)))))
          (bad '(lambda () (dotimes (x 1) (declare (special "x")))))
          (bad '(lambda (l) (destructuring-bind (x y) l (declare (ignore 1)) (list x y))))
          (bad '(lambda (x) (declare (type fixnum nil)) x))
          (bad '(lambda (x) (declare (fixnum t x)) x))
          (bad '(lambda (x) (declare (special nil)) x))))
  (:program-error :program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error :program-error :program-error))

(deftest-emitting-only declare-syntax-more.valid
  (list (funcall (compile nil '(lambda (x) (declare (fixnum x) (ignorable x) (optimize (speed 1))) x)) 5)
        (funcall (compile nil '(lambda () (flet ((f (a) (declare (fixnum a)) a)) (declare (inline f)) (f 1)))))
        (funcall (compile nil '(lambda () (dolist (x '(1 2) x) (declare (fixnum x))))))
        (funcall (compile nil '(lambda (l) (destructuring-bind (x y) l (declare (ignore y)) x))) '(1 2))
        (funcall (compile nil '(lambda (x) "doc" (declare (fixnum x)) x)) 1))
  (5 1 nil 1 1))

(deftest-emitting-only declaim-type-without-arguments.errors
  (handler-case (progn (eval '(declaim (type))) :no-error) (program-error () :program-error))
  :program-error)
