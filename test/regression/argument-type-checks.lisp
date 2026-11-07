;;; COMPLEMENT, FUNCTION-LAMBDA-EXPRESSION, COMPILER-MACRO-FUNCTION, TYPE-ERROR-DATUM
;;; and the TYPE-ERROR accessors check the type of their argument; DEFSETF,
;;; DEFINE-MODIFY-MACRO, (SETF (THE ...)) and LAMBDA check their shape; a condition or
;;; CLOS instance is not a type specifier.

(defmacro %atc (form)
  `(handler-case (progn (eval ',form) :no-error) (error () :error)))

(deftest argument-type-checks.errors
  (values (%atc (complement 1)) (%atc (complement nil)) (%atc (complement 'car))
          (%atc (function-lambda-expression 1)) (%atc (function-lambda-expression nil))
          (%atc (compiler-macro-function 1)) (%atc (compiler-macro-function "x")) (%atc (compiler-macro-function '(setf)))
          (%atc (type-error-datum 1)) (%atc (type-error-datum nil)) (%atc (type-error-expected-type 2))
          (%atc (defsetf %atc-a %atc-b 5)) (%atc (defsetf %atc-a %atc-b "doc" x)) (%atc (defsetf %atc-a %atc-b "doc" . x))
          (%atc (define-modify-macro %atc-m nil %atc-f "doc" 5)) (%atc (define-modify-macro %atc-m nil %atc-f 5))
          (%atc (let (x y) (setf (the t x y) nil)))
          (%atc (lambda)) (%atc (lambda 1))
          (%atc (typep 1 (make-condition 'simple-error))))
  :error :error :error :error :error :error :error :error :error :error :error :error :error :error :error :error
  :error :error :error :error)

(deftest argument-type-checks.valid
  (values (funcall (complement #'null) 1) (funcall (complement #'null) nil)
          (functionp (nth-value 0 (function-lambda-expression #'car)))
          (compiler-macro-function 'argument-type-checks-no-such)
          (compiler-macro-function '(setf argument-type-checks-no-such))
          (type-error-datum (make-condition 'type-error :datum 5 :expected-type 'string))
          (type-error-expected-type (make-condition 'type-error :datum 5 :expected-type 'string))
          (eval '(progn (defsetf %atc-ok1 %atc-ok2 "doc") 1))
          (eval '(progn (defsetf %atc-ok3 %atc-ok2) 2))
          (eval '(progn (defsetf %atc-ok4 (a) (v) (list a v)) 3))
          (eval '(progn (define-modify-macro %atc-ok5 (&optional (d 1)) + "doc") 4))
          (let (x) (setf (the t x) 9))
          (funcall (eval '(lambda (a) a)) 8))
  t nil nil nil nil 5 string 1 2 3 4 9 8)
