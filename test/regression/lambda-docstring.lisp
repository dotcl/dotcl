;;; A docstring in a LAMBDA body is the documentation of the function it makes,
;;; for (lambda ...), #'(lambda ...), COMPILE and the interpreter alike.

(deftest lambda-docstring.closure
  (documentation (let ((y 1)) (lambda (x) "inner" (+ x y))) 'function)
  "inner")

(deftest lambda-docstring.function-type-t
  (documentation (lambda (x) "doc" x) t)
  "doc")

(deftest lambda-docstring.sharp-quote
  (documentation #'(lambda (x) "sharp" x) t)
  "sharp")

(deftest lambda-docstring.after-declare
  (documentation (lambda (x) (declare (ignore x)) "after decl" 5) t)
  "after decl")

(deftest lambda-docstring.only-a-string-is-the-value
  (list (documentation (lambda () "only string") t)
        (funcall (lambda () "only string")))
  (nil "only string"))

(deftest lambda-docstring.second-string-is-not-doc
  (list (documentation (lambda (x) "a" "b" x) t)
        (funcall (lambda (x) "a" "b" x) 7))
  ("a" 7))

(deftest lambda-docstring.none
  (documentation (lambda (x) x) t)
  nil)

(deftest-emitting-only lambda-docstring.compile-nil
  (documentation (compile nil '(lambda (x) "compiled" x)) t)
  "compiled")

(deftest-emitting-only lambda-docstring.compile-named
  (progn (compile 'lambda-docstring-f '(lambda (x) "named" x))
         (documentation 'lambda-docstring-f 'function))
  "named")

(deftest-emitting-only lambda-docstring.interpret
  (let ((dotcl:*evaluator-mode* :interpret))
    (documentation (eval '(lambda (x) "interp" x)) t))
  "interp")
