;;; Redefining an EQL-specialized method must replace it, not add a second one.
;;; Method identity is (specializers, qualifiers), and a class specializer is an
;;; interned object -- but an EQL specializer is a fresh (EQL value) cons on every
;;; DEFMETHOD, so comparing specializers by identity never matched one. The
;;; redefinition was appended instead, and the original kept winning: a second
;;; (defmethod f ((x (eql :v))) ...) had no visible effect at all.

(defgeneric emr-late (x))
(defmethod emr-late ((x (eql :v))) :first)

(deftest eql-method-redefinition.takes-effect
  (let ((warm (list (emr-late :v) (emr-late :v))))
    (eval '(defmethod emr-late ((x (eql :v))) :second))
    (append warm (list (emr-late :v))))
  (:first :first :second))

(deftest eql-method-redefinition.method-count
  (length (dotcl-mop:generic-function-methods #'emr-late))
  1)

;;; Different EQL values stay different methods.
(defgeneric emr-two (x))
(defmethod emr-two ((x (eql :a))) :a)
(defmethod emr-two ((x (eql :b))) :b)

(deftest eql-method-redefinition.distinct-values-are-distinct-methods
  (list (emr-two :a) (emr-two :b) (length (dotcl-mop:generic-function-methods #'emr-two)))
  (:a :b 2))

;;; EQL values that are equal numbers but not the same object (a bignum, a float)
;;; are EQL, so they name the same method.
(defgeneric emr-num (x))
(defmethod emr-num ((x (eql 1099511627776))) :first)

(deftest eql-method-redefinition.eql-not-identity
  (progn (eval '(defmethod emr-num ((x (eql 1099511627776))) :second))
         (list (emr-num 1099511627776) (length (dotcl-mop:generic-function-methods #'emr-num))))
  (:second 1))

;;; A qualifier still separates methods with the same specializer.
(defvar *emr-trace* '())
(defgeneric emr-qual (x))
(defmethod emr-qual ((x (eql :q))) (push :primary *emr-trace*) :primary)
(defmethod emr-qual :after ((x (eql :q))) (push :after *emr-trace*))

(deftest eql-method-redefinition.qualifier-separates
  (progn (setf *emr-trace* '())
         (list (emr-qual :q) (reverse *emr-trace*) (length (dotcl-mop:generic-function-methods #'emr-qual))))
  (:primary (:primary :after) 2))
