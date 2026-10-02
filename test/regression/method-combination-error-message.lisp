;;; METHOD-COMBINATION-ERROR and INVALID-METHOD-ERROR signal an error whose
;;; report is the caller's format control applied to its arguments. They used
;;; to throw away both and always report "METHOD-COMBINATION-ERROR called",
;;; which hid generic-cl's "No method for type ..." message.

(define-method-combination mce-strict ()
  ((all *))
  (method-combination-error "No method for type ~S in ~A" 'integer "mce-g"))

(defgeneric mce-g (x) (:method-combination mce-strict))
(defmethod mce-g (x) x)

(deftest method-combination-error-report
  (handler-case (mce-g 1)
    (error (e) (princ-to-string e)))
  "No method for type INTEGER in mce-g")

(deftest method-combination-error-simple-error
  (handler-case (mce-g 1)
    (simple-error (e)
      (values (simple-condition-format-control e)
              (simple-condition-format-arguments e))))
  "No method for type ~S in ~A" (integer "mce-g"))

(deftest invalid-method-error-report
  (handler-case (invalid-method-error 'some-method "qualifier ~S is not allowed" :bogus)
    (simple-error (e)
      (values (princ-to-string e)
              (simple-condition-format-arguments e))))
  "qualifier :BOGUS is not allowed" (:bogus))
