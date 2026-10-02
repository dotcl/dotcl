;;; COMPILE of a lambda expression (and EVAL of #'(LAMBDA ...)) returns the
;;; function without building and running a one-shot method around it. The
;;; function object is made while its code is assembled; the method only
;;; loaded it and returned it, and JIT-compiling that method with full
;;; optimization cost as much as compiling a small function.

(deftest-emitting-only compile-lambda-no-thunk.no-code-until-called
  (flet ((jitted ()
           (dotnet:static "System.Runtime.JitInfo" "GetCompiledMethodCount" t)))
    (compile nil '(lambda (x) (+ x 1)))
    (let ((before (jitted)))
      (dotimes (i 50) (compile nil '(lambda (x) (+ x 1))))
      (< (- (jitted) before) 25)))
  t)

(deftest-emitting-only compile-lambda-no-thunk.results
  (let ((f (compile nil '(lambda (x &optional (y 10)) (list x y))))
        (g (compile nil '(lambda (a b c d e f g h i) (list a b c d e f g h i))))
        (h (eval '(function (lambda (&rest r) (reverse r)))))
        (k (compile nil (let ((n 3)) (declare (ignorable n)) '(lambda () (values 1 2))))))
    (list (funcall f 1) (funcall f 1 2)
          (funcall g 1 2 3 4 5 6 7 8 9)
          (funcall h 1 2 3)
          (multiple-value-list (funcall k))
          (compiled-function-p f)))
  ((1 10) (1 2) (1 2 3 4 5 6 7 8 9) (3 2 1) (1 2) t))

;; A lambda that closes over nothing but builds closures when called.
(deftest-emitting-only compile-lambda-no-thunk.inner-closures
  (let ((f (compile nil '(lambda (n) (let ((acc '())) (dotimes (i n) (push (lambda () i) acc)) (mapcar #'funcall acc))))))
    (length (funcall f 5)))
  5)
