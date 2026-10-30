;;; ERROR's invalid condition designators must signal TYPE-ERROR, not the
;;; generic ERROR condition (CLHS 9.1.2.1).

(defun %eid-kind (form)
  (handler-case
      (list :returned (eval form))
    (type-error (c)
      (list :type-error (type-error-datum c)))
    (error (c)
      (list :other (type-of c)))))

(deftest error-invalid-designator.symbol
  (%eid-kind '(error 'no-such-condition-class))
  (:type-error no-such-condition-class))

(deftest error-invalid-designator.object
  (%eid-kind '(error 42))
  (:type-error 42))

(deftest error-invalid-designator.cons
  (%eid-kind '(error '(or)))
  (:type-error (or)))

(deftest error-invalid-designator.condition-still-works
  (let ((c (make-condition 'simple-error :format-control "x")))
    (handler-case (error c)
      (simple-error (e) (eq e c))))
  t)
