;;; COERCE of a lambda form to FUNCTION needs a lambda list: (lambda) is not a
;;; lambda expression.

(deftest coerce-lambda-form.valid
  (funcall (coerce '(lambda (x) (* x 2)) 'function) 4)
  8)

(deftest coerce-lambda-form.no-params
  (funcall (coerce '(lambda ()  7) 'function))
  7)

(deftest coerce-lambda-form.bare-lambda-is-type-error
  (handler-case (functionp (coerce '(lambda) 'function)) (type-error () :type-error))
  :type-error)

(deftest coerce-lambda-form.non-list-params-is-type-error
  (handler-case (functionp (coerce '(lambda 5 6) 'function)) (type-error () :type-error))
  :type-error)
