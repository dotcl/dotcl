;;; TYPEP of a type whose DEFTYPE expander signals an error signals it, as it does when the
;;; type is not a constant in the code. A constant (TYPEP x 'name) was rewritten inline, and the
;;; failed expansion left the rewrite with the type NIL: the answer was NIL.

(deftype tde-bad () (error "tde-bad expander"))
(deftype tde-ok () 'integer)

(defun %tde-error-p (thunk) (handler-case (progn (funcall thunk) nil) (error () t)))

(deftest typep-deftype-error.constant-type
  (list (%tde-error-p (lambda () (typep 3 'tde-bad)))
        (%tde-error-p (lambda () (eval '(typep 3 'tde-bad))))
        (%tde-error-p (lambda () (typecase 3 (tde-bad :a) (t :b)))))
  (t t t))

(deftest typep-deftype-error.working-expander-unchanged
  (list (typep 3 'tde-ok) (typep 'a 'tde-ok) (typecase 3 (tde-ok :int) (t :no)))
  (t nil :int))

(deftest-emitting-only typep-deftype-error.compiled
  (list (%tde-error-p (lambda () (funcall (compile nil '(lambda (x) (typep x 'tde-bad))) 3)))
        (funcall (compile nil '(lambda (x) (typep x 'tde-ok))) 3))
  (t t))
