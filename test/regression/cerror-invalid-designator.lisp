;;; CERROR's invalid condition designators signal TYPE-ERROR, as ERROR does.

(defun %cerror-invalid-kind (form)
  (handler-case
      (list :returned (eval form))
    (type-error (c)
      (list :type-error (type-error-datum c)))
    (error (c)
      (list :other (type-of c)))))

(deftest cerror-invalid-designator.symbol
  (%cerror-invalid-kind '(cerror "Continue" 'no-such-cerror-condition))
  (:type-error no-such-cerror-condition))

(deftest cerror-invalid-designator.object
  (%cerror-invalid-kind '(cerror "Continue" 42))
  (:type-error 42))

(deftest cerror-invalid-designator.valid-condition
  (handler-bind ((error (lambda (c) (declare (ignore c)) (invoke-restart 'continue))))
    (cerror "Continue" 'simple-error)
    :continued)
  :continued)
