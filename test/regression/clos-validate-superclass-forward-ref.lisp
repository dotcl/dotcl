;;; VALIDATE-SUPERCLASS is consulted with the real metaclass of both classes: a user metaclass
;;; that inherits from STANDARD-CLASS is not compatible with an ordinary class (and the other
;;; way round) until a method says so. And a class with a superclass that was never defined
;;; cannot be instantiated; defining the superclass makes it work.

(defclass vsc-mc (standard-class) ())
(defclass vsc-plain () ())
(defclass vsc-mc-parent () () (:metaclass vsc-mc))

(defun %vsc-outcome (thunk)
  (handler-case (progn (funcall thunk) :accepted) (error () :error)))

(deftest clos-validate-superclass.custom-child-of-standard
  (%vsc-outcome (lambda () (eval '(defclass vsc-bad-1 (vsc-plain) () (:metaclass vsc-mc)))))
  :error)

(deftest clos-validate-superclass.standard-child-of-custom
  (%vsc-outcome (lambda () (eval '(defclass vsc-bad-2 (vsc-mc-parent) ()))))
  :error)

(deftest clos-validate-superclass.same-metaclass-accepted
  (list (%vsc-outcome (lambda () (eval '(defclass vsc-ok-1 (vsc-mc-parent) () (:metaclass vsc-mc)))))
        (%vsc-outcome (lambda () (eval '(defclass vsc-ok-2 (vsc-plain) ())))))
  (:accepted :accepted))

(deftest clos-validate-superclass.method-makes-it-compatible
  (progn
    (defmethod dotcl-mop:validate-superclass ((c vsc-mc) (s standard-class)) t)
    (%vsc-outcome (lambda () (eval '(defclass vsc-ok-3 (vsc-plain) () (:metaclass vsc-mc))))))
  :accepted)

(deftest clos-validate-superclass.default-method-direct-call
  (list (dotcl-mop:validate-superclass (find-class 'vsc-plain) (find-class 'vsc-plain))
        (dotcl-mop:validate-superclass (find-class 'standard-class) (find-class 'standard-class)))
  (t t))

(defclass fwd-k (fwd-missing-super) ())

(deftest clos-forward-referenced.make-instance-is-an-error
  (%vsc-outcome (lambda () (make-instance 'fwd-k)))
  :error)

(deftest clos-forward-referenced.defined-later-works
  (progn
    (defclass fwd-missing-super () ())
    (let ((x (make-instance 'fwd-k)))
      (list (typep x 'fwd-k) (typep x 'fwd-missing-super))))
  (t t))
