;;; The STORE-VALUE restart of CCASE and CTYPECASE stores into the keyplace.

(defun ccsv-store (value)
  (lambda (condition)
    (declare (ignore condition))
    (invoke-restart 'store-value value)))

(deftest ccase-store-value.updates-variable
  (let ((x 'foo))
    (list (handler-bind ((type-error (ccsv-store 'bar)))
            (ccase x (bar :got-bar) (baz :got-baz)))
          x))
  (:got-bar bar))

(deftest ccase-store-value.updates-compound-place-once
  (let ((v (vector 'foo 'foo)) (i 0))
    (list (handler-bind ((type-error (ccsv-store 'bar)))
            (ccase (aref v (incf i)) (bar :got-bar) (baz :got-baz)))
          i (coerce v 'list)))
  (:got-bar 1 (foo bar)))

(deftest ctypecase-store-value.updates-variable
  (let ((x "foo"))
    (list (handler-bind ((type-error (ccsv-store 5)))
            (ctypecase x (integer :got-int) (symbol :got-sym)))
          x))
  (:got-int 5))

(deftest ctypecase-store-value.updates-compound-place-once
  (let ((v (vector "foo" "foo")) (i 0))
    (list (handler-bind ((type-error (ccsv-store 5)))
            (ctypecase (aref v (incf i)) (integer :got-int) (symbol :got-sym)))
          i (coerce v 'list)))
  (:got-int 1 ("foo" 5)))
