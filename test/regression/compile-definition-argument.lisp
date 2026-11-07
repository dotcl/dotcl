;;; (COMPILE name NIL) and (COMPILE NIL NIL) are errors: an explicit second argument must be a
;;; lambda expression or a function. (COMPILE name) alone, and a lambda expression, still work.

(defun %cda-target (x) (1+ x))

(deftest-emitting-only compile-definition-argument.errors
  (values (handler-case (progn (compile '%cda-target nil) :no-error) (error () :error))
          (handler-case (progn (compile nil nil) :no-error) (error () :error))
          (handler-case (progn (compile '%cda-target 1) :no-error) (error () :error))
          (handler-case (progn (compile '%cda-target '(foo)) :no-error) (error () :error)))
  :error :error :error :error)

(deftest-emitting-only compile-definition-argument.valid
  (values (funcall (compile nil '(lambda (x) (* 2 x))) 4)
          (progn (compile '%cda-new '(lambda (y) (+ y 10))) (funcall '%cda-new 1))
          (progn (compile '%cda-new2 #'identity) (funcall '%cda-new2 7)))
  8 11 7)
