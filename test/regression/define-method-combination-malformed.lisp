;;; A malformed DEFINE-METHOD-COMBINATION is a program error: a short form whose
;;; options are not a property list of :DOCUMENTATION (a string),
;;; :IDENTITY-WITH-ONE-ARGUMENT and :OPERATOR, or a :GENERIC-FUNCTION option that
;;; is not exactly one symbol.

(defmacro %dmc-error-p (form)
  `(handler-case (progn (macroexpand-1 ',form) nil) (program-error () t)))

(deftest dmc-malformed.short-form
  (list (%dmc-error-p (define-method-combination dmc-a1 :documentation :operator))
        (%dmc-error-p (define-method-combination dmc-a2 :documentation nil))
        (%dmc-error-p (define-method-combination dmc-a3 nil))
        (%dmc-error-p (define-method-combination dmc-a4 :operator + :bogus 1)))
  (t t t t))

(deftest dmc-malformed.generic-function-option
  (list (%dmc-error-p (define-method-combination dmc-b1 nil nil (:generic-function)))
        (%dmc-error-p (define-method-combination dmc-b2 nil nil (:generic-function bar baz)))
        (%dmc-error-p (define-method-combination dmc-b3 nil nil (:generic-function (bar)))))
  (t t t))

(deftest dmc-malformed.valid-forms-unchanged
  (progn
    (eval '(define-method-combination dmc-sum-mc :operator + :identity-with-one-argument t :documentation "sum"))
    (eval '(defgeneric dmc-sum-gf (x) (:method-combination dmc-sum-mc)))
    (eval '(defmethod dmc-sum-gf dmc-sum-mc ((x integer)) x))
    (eval '(defmethod dmc-sum-gf dmc-sum-mc ((x number)) 10))
    (list (eval '(dmc-sum-gf 3))
          (%dmc-error-p (define-method-combination dmc-ok :operator + :documentation "d"))
          (%dmc-error-p (define-method-combination dmc-ok2 nil ((all *)) (:generic-function gf) `(list ,@all)))))
  (13 nil nil))
