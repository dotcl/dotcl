;;; (compile nil) without a definition, a method object as a type specifier,
;;; (function formatter) and a LET that binds one variable twice are errors.

(defmacro %bas-err (form)
  `(handler-case (progn (eval ',form) :no-error)
     (error () :error)))

(deftest beyond-ansi-small-checks.errors
  (list (let ((m (car (generic-function-methods #'print-object))))
          (list (handler-case (typep nil m) (error () :error))
                (handler-case (subtypep m t) (error () :error))))
        (%bas-err (function formatter))
        (%bas-err (let ((x 1) (x 2)) x))
        (%bas-err (let ((x 0) (x 1)) x)))
  ((:error :error) :error :error :error))

(deftest beyond-ansi-small-checks.valid
  (list (functionp (formatter "~A"))
        (let ((s (make-string-output-stream)))
          (funcall (formatter "<~A>") s 5)
          (get-output-stream-string s))
        (let* ((x 1) (x (+ x 1))) x)
        (let ((x 1)) (let ((x 2)) x))
        (functionp (macro-function 'formatter)))
  (t "<5>" 2 2 t))

(deftest-emitting-only beyond-ansi-small-checks.compile
  (list (functionp (compile nil '(lambda () 1)))
        (handler-case (progn (compile nil) :no-error) (error () :error))
        (%bas-err (macrolet ((%m () :foo)) (declare (ftype (function (&rest t) t) %m)) (%m)))
        (macrolet ((%m () :foo)) (declare (ftype (function (&rest t) t) other-fn)) (%m)))
  (t :error :error :foo))
