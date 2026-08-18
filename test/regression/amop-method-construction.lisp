;;; Building a method through the AMOP (MAKE-INSTANCE 'STANDARD-METHOD) and
;;; attaching it with ADD-METHOD.
;;;
;;; The ledger for this said the AMOP initargs were "accepted and dropped",
;;; because the :before methods in cil-stdlib.lisp declare them ignore. They are
;;; not dropped: an initialize-instance :after on the C# side applies
;;; qualifiers / specializers / function / lambda-list. What WAS missing is that
;;; the :lambda-list was parsed for its arity and then discarded, so
;;; METHOD-LAMBDA-LIST answered with a rebuilt placeholder (#:R0) instead of the
;;; list that was passed.

(defclass amc-thing () ())
(defgeneric amc-gf (x))

(defparameter *amc-method*
  (make-instance 'standard-method
                 :qualifiers '()
                 :lambda-list '(x)
                 :specializers (list (find-class 'amc-thing))
                 :function (lambda (x) (declare (ignore x)) :from-amop)))

(deftest amop-method-construction.specializers
  (mapcar #'class-name (dotcl-mop:method-specializers *amc-method*))
  (amc-thing))

(deftest amop-method-construction.lambda-list-is-what-was-given
  (dotcl-mop:method-lambda-list *amc-method*)
  (x))

(deftest amop-method-construction.add-and-call
  (progn
    (add-method #'amc-gf *amc-method*)
    (list (amc-gf (make-instance 'amc-thing))
          (eq (dotcl-mop:method-generic-function *amc-method*) #'amc-gf)
          (length (dotcl-mop:generic-function-methods #'amc-gf))))
  (:from-amop t 1))

;;; A DEFMETHOD-built method has no stored lambda list; it still reports one of
;;; the right shape, rebuilt from the recorded arity.

(defgeneric amc-two (a b))
(defmethod amc-two ((a amc-thing) b) (list a b))

(deftest amop-method-construction.defmethod-lambda-list-arity
  (length (dotcl-mop:method-lambda-list
           (first (dotcl-mop:generic-function-methods #'amc-two))))
  2)

;;; REMOVE-METHOD detaches it again.

(deftest amop-method-construction.remove-method
  (progn
    (remove-method #'amc-gf *amc-method*)
    (list (length (dotcl-mop:generic-function-methods #'amc-gf))
          (dotcl-mop:method-generic-function *amc-method*)))
  (0 nil))
