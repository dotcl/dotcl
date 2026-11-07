;;; A type error raised by the runtime with a message of the form "OP: not a THING"
;;; also says which value it was, as the standard-library errors that already did
;;; ("CAR: not a list: 5") and as SBCL's do. A message that already names the
;;; value is left as it is.

(defun %temv-message (thunk)
  (handler-case (progn (funcall thunk) nil)
    (type-error (e) (princ-to-string e))))

(defmacro %temv (form) `(%temv-message (lambda () ,form)))

(deftest type-error-message-has-value.aref
  (let ((m (%temv (aref 42 0)))) (and (search "42" m) t))
  t)

(deftest type-error-message-has-value.gethash
  (let ((m (%temv (gethash 'x 42)))) (and (search "42" m) t))
  t)

(deftest type-error-message-has-value.funcall
  (let ((m (%temv (funcall 42)))) (and (search "42" m) t))
  t)

(deftest type-error-message-has-value.mapcar
  (let ((m (%temv (mapcar #'1+ 42)))) (and (search "42" m) t))
  t)

(deftest type-error-message-has-value.sequence
  (let ((m (%temv (find 1 42)))) (and (search "42" m) t))
  t)

(deftest type-error-message-has-value.coerce-reads-naturally
  (search "cannot coerce FOO to" (%temv (coerce 'foo 'integer)))
  8)

(deftest type-error-message-has-value.not-doubled
  (let ((m (%temv (car 5))))
    (and (search "5" m) (not (search "5: 5" m)) (not (search "5, 5" m)) t))
  t)

(deftest type-error-message-has-value.datum-slot-unchanged
  (handler-case (aref 42 0) (type-error (e) (type-error-datum e)))
  42)

(deftest type-error-message-has-value.valid-input-no-error
  (list (aref #(1 2) 1) (gethash 'a (make-hash-table)) (mapcar #'1+ '(1 2)) (find 1 '(0 1)))
  (2 nil (2 3) 1))
