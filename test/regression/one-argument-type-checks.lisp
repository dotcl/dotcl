;;; The one-argument calls of + * = < > <= >= LOGIOR LOGAND LOGXOR CHAR= CHAR< ... check their argument;
;;; DECODE-FLOAT and INTEGER-DECODE-FLOAT want a float; condition accessors want a condition.

(defmacro %otc-error-p (form)
  `(handler-case (progn ,form nil) (type-error () t)))

(deftest-compiled-only one-arg-checks.compiled-forms
  (list (%otc-error-p (funcall (compile nil '(lambda (a) (+ a))) 'sym))
        (%otc-error-p (funcall (compile nil '(lambda (a) (* a))) "x"))
        (%otc-error-p (funcall (compile nil '(lambda (a) (logior a))) 'sym))
        (%otc-error-p (funcall (compile nil '(lambda (a) (logand a))) 1.5))
        (%otc-error-p (funcall (compile nil '(lambda (a) (logxor a))) 'sym))
        (%otc-error-p (funcall (compile nil '(lambda (a) (< a))) 'sym))
        (%otc-error-p (funcall (compile nil '(lambda (a) (>= a))) 'sym))
        (%otc-error-p (funcall (compile nil '(lambda (a) (= a))) 'sym))
        (%otc-error-p (funcall (compile nil '(lambda (a) (< a))) #c(1 2))))
  (t t t t t t t t t))

(deftest one-arg-checks.function-objects
  (list (%otc-error-p (funcall #'< 'sym))
        (%otc-error-p (funcall #'<= 'sym))
        (%otc-error-p (apply #'= '(sym)))
        (%otc-error-p (funcall #'char= 1))
        (%otc-error-p (funcall #'char< 1))
        (%otc-error-p (funcall #'char/= 1)))
  (t t t t t t))

(deftest-compiled-only one-arg-checks.valid-calls-unchanged
  (list (funcall (compile nil '(lambda (a) (+ a))) 5)
        (funcall (compile nil '(lambda (a) (* a))) 2.5)
        (funcall (compile nil '(lambda (a) (< a))) 1)
        (funcall (compile nil '(lambda (a) (= a))) #c(1 2))
        (funcall (compile nil '(lambda (a) (logior a))) 3)
        (funcall #'char= #\a))
  (5 2.5 t t 3 t))

(deftest one-arg-checks.decode-float-wants-a-float
  (list (%otc-error-p (decode-float 5))
        (%otc-error-p (integer-decode-float 5))
        (%otc-error-p (decode-float 1/2))
        (multiple-value-list (decode-float 0.5d0))
        (multiple-value-list (integer-decode-float 1.0d0)))
  (t t t (0.5d0 0 1.0d0) (4503599627370496 -52 1)))

(deftest one-arg-checks.condition-accessors-want-a-condition
  (list (%otc-error-p (cell-error-name 5))
        (%otc-error-p (simple-condition-format-control "a"))
        (%otc-error-p (simple-condition-format-arguments 5))
        (%otc-error-p (file-error-pathname (vector)))
        (%otc-error-p (stream-error-stream 5))
        (%otc-error-p (arithmetic-error-operands 5))
        (%otc-error-p (package-error-package 5))
        (%otc-error-p (print-not-readable-object 5))
        (cell-error-name (make-condition 'unbound-variable :name 'zzz))
        (simple-condition-format-control (make-condition 'simple-error :format-control "x ~a" :format-arguments '(1))))
  (t t t t t t t t zzz "x ~a"))
