;;; A failed internal cast reports a Lisp type name, not a .NET type name.

(defun cast-msg (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error (c) (princ-to-string c))))

(deftest cast-message.get-macro-character
  (let ((m (cast-msg (lambda () (get-macro-character "x")))))
    (and (search "CHARACTER" m) (not (search "DotCL" m)) (not (search "System." m)) t))
  t)

(deftest cast-message.decode-float
  (let ((m (cast-msg (lambda () (decode-float "x")))))
    (and (not (search "System." m)) (not (search "DotCL" m)) (or (search "FLOAT" m) (search "NUMBER" m)) t))
  t)

(deftest cast-message.stays-type-error
  (list (typep (nth-value 1 (ignore-errors (integer-decode-float "x"))) 'type-error)
        (typep (nth-value 1 (ignore-errors (get-dispatch-macro-character "x" #\a))) 'type-error))
  (t t))
