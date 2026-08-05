;;; Regression: CLHS TYPE — "A symbol cannot be both the name of a type and the
;;; name of a declaration. Defining a symbol as the name of a class, structure,
;;; condition, or type, when the symbol has been declared as a declaration name,
;;; or vice versa, signals an error."
;;;
;;; (proclaim '(declaration foo)) used to be accepted and thrown away, so both
;;; directions went unnoticed: a later deftype/defclass/defstruct/define-condition
;;; on the same symbol succeeded, and so did proclaiming a declaration over an
;;; existing type name.
;;;
;;; The flag also feeds the unknown-type warning: a proclaimed declaration names
;;; no type by definition, so (declare (foo x)) on one must not be reported as a
;;; declared type that resolves to nothing.

(defun dte-errors-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

;;; --- declaration first, type definer second ------------------------------

(proclaim '(declaration dte-a))
(deftest dte-deftype-over-declaration
  (dte-errors-p (lambda () (eval '(deftype dte-a () t))))
  t)

(proclaim '(declaration dte-b))
(deftest dte-defclass-over-declaration
  (dte-errors-p (lambda () (eval '(defclass dte-b () ()))))
  t)

(proclaim '(declaration dte-c))
(deftest dte-defstruct-over-declaration
  (dte-errors-p (lambda () (eval '(defstruct dte-c))))
  t)

(proclaim '(declaration dte-d))
(deftest dte-define-condition-over-declaration
  (dte-errors-p (lambda () (eval '(define-condition dte-d (error) ()))))
  t)

;;; --- type first, declaration second --------------------------------------

(deftype dte-e () t)
(deftest dte-declaration-over-deftype
  (dte-errors-p (lambda () (proclaim '(declaration dte-e))))
  t)

(defclass dte-f () ())
(deftest dte-declaration-over-defclass
  (dte-errors-p (lambda () (proclaim '(declaration dte-f))))
  t)

(deftest dte-declaration-over-builtin-type
  (dte-errors-p (lambda () (proclaim '(declaration integer))))
  t)

;;; --- the ordinary cases still work ---------------------------------------

(deftest dte-plain-deftype        (dte-errors-p (lambda () (eval '(deftype dte-ok () t)))) nil)
(deftest dte-plain-defclass       (dte-errors-p (lambda () (eval '(defclass dte-ok2 () ())))) nil)
(deftest dte-plain-declaration    (dte-errors-p (lambda () (proclaim '(declaration dte-ok3)))) nil)
;; Proclaiming the same declaration twice is not a redefinition conflict.
(deftest dte-declaration-twice    (dte-errors-p (lambda () (proclaim '(declaration dte-ok3)))) nil)
;; CLHS 4.3.6: redefining a class under its own name still updates it in place.
(deftest dte-class-redefinition   (dte-errors-p (lambda () (eval '(defclass dte-ok2 () ())))) nil)

;;; A proclaimed declaration used as a declaration works, and (this is the part
;;; the flag fixes) is not reported as an unknown declared type.
(deftest dte-declaration-is-usable
  (eval '(let ((x 1)) (declare (dte-ok3 x)) x))
  1)

(deftest dte-declaration-does-not-warn
  (let ((warned nil))
    (handler-bind ((warning (lambda (c) (declare (ignore c)) (setf warned t))))
      (eval '(let ((x 1)) (declare (dte-ok3 x)) x)))
    warned)
  nil)
