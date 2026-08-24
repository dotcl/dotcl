;;; A direct-params closure holds its body delegate and environment as they are;
;;; the per-arity entry points bind them at call time and the args-array entry
;;; (apply, restart-bind's RawFunction, funcall through a symbol) spreads. The
;;; wrapper lambdas it used to build per closure are gone, so every one of those
;;; paths needs to keep working.

(defun %cd-mk2 (a) (lambda (x y) (list a x y)))

(deftest closure-direct.funcall-and-apply
  (list (funcall (%cd-mk2 1) 2 3) (apply (%cd-mk2 1) '(2 3)))
  ((1 2 3) (1 2 3)))

(deftest closure-direct.arity-error-is-program-error
  (handler-case (funcall (%cd-mk2 1) 2)
    (program-error () :program-error))
  :program-error)

(deftest closure-direct.arities-0-to-6
  (list (funcall (let ((a 1)) (lambda () a)))
        (funcall (let ((a 1)) (lambda (x) (+ a x))) 1)
        (funcall (let ((a 1)) (lambda (x y) (+ a x y))) 1 2)
        (funcall (let ((a 1)) (lambda (x y z) (+ a x y z))) 1 2 3)
        (funcall (let ((a 1)) (lambda (x y z w) (+ a x y z w))) 1 2 3 4)
        (funcall (let ((a 1)) (lambda (x y z w v) (+ a x y z w v))) 1 2 3 4 5)
        (funcall (let ((a 1)) (lambda (x y z w v u) (+ a x y z w v u))) 1 2 3 4 5 6))
  (1 2 4 7 11 16 22))

(deftest closure-direct.apply-at-each-arity
  (list (apply (let ((a 1)) (lambda (x) (+ a x))) '(1))
        (apply (let ((a 1)) (lambda (x y z) (+ a x y z))) '(1 2 3))
        (apply (let ((a 1)) (lambda (x y z w v) (+ a x y z w v))) '(1 2 3 4 5)))
  (2 7 16))

(deftest closure-direct.rest-optional-key
  ;; These take the args-array closure path, not the direct one.
  (list (funcall (let ((a 1)) (lambda (&rest r) (cons a r))) 2 3)
        (funcall (let ((a 1)) (lambda (&optional (x 9)) (list a x))))
        (funcall (let ((a 1)) (lambda (&key b) (list a b))) :b 5))
  ((1 2 3) (1 9) (1 5)))

(deftest closure-direct.captured-variable-is-shared
  (let ((n 0))
    (let ((f (lambda () (incf n))))
      (funcall f) (funcall f)
      (list n (funcall f))))
  (2 3))

(deftest closure-direct.nested-closures
  (funcall (funcall (let ((a 1)) (lambda (b) (lambda (c) (list a b c)))) 2) 3)
  (1 2 3))

(deftest closure-direct.as-sort-predicate
  (let ((k 1)) (sort (list 3 1 2) (lambda (a b) (< (* k a) (* k b)))))
  (1 2 3))

(deftest closure-direct.through-symbol-function
  (let ((a 5))
    (setf (symbol-function 'closure-direct-tmp) (lambda (x) (+ a x)))
    (funcall 'closure-direct-tmp 1))
  6)

(deftest closure-direct.restart-bind-handler
  ;; restart-bind takes the closure's args-array delegate (RawFunction).
  (let ((v 42))
    (catch 'closure-direct-done
      (restart-bind ((r (lambda () (throw 'closure-direct-done (list :restart v)))))
        (invoke-restart 'r))))
  (:restart 42))

(deftest closure-direct.restart-case-handler
  (let ((v 7))
    (handler-bind ((error (lambda (c) (declare (ignore c)) (invoke-restart 'closure-direct-r2))))
      (restart-case (error "boom") (closure-direct-r2 () (list :handled v)))))
  (:handled 7))

(deftest closure-direct.labels-recursive
  (labels ((g (x) (if (> x 0) (g (1- x)) :done))) (funcall #'g 3))
  :done)

(deftest closure-direct.mapcar
  (let ((n 10)) (mapcar (lambda (x) (+ n x)) '(1 2 3)))
  (11 12 13))
