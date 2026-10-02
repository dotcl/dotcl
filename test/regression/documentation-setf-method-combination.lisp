;;; DEFSETF (both forms), DEFINE-SETF-EXPANDER and DEFINE-METHOD-COMBINATION
;;; (both forms) record their docstring, so (DOCUMENTATION name 'SETF) and
;;; (DOCUMENTATION name 'METHOD-COMBINATION) return it. They used to drop it.
;;; The expected values are SBCL 2.6.8's.

(defun dsmc-setter (x v) (declare (ignore x)) v)
(defsetf dsmc-short dsmc-setter "short doc")
(defsetf dsmc-long (x) (v) "long doc" `(dsmc-setter ,x ,v))
(defsetf dsmc-nodoc dsmc-setter)
(define-setf-expander dsmc-expander (x) "expander doc"
  (declare (ignore x))
  (values nil nil nil nil nil))
(define-method-combination dsmc-short-mc :identity-with-one-argument t
  :documentation "short mc doc")
(define-method-combination dsmc-long-mc () ((primary () :required t))
  "long mc doc"
  `(call-method ,(first primary)))

(defgeneric dsmc-gf (x) (:method-combination dsmc-long-mc))
(defmethod dsmc-gf (x) (list :gf x))

(deftest documentation-setf-method-combination.setf
  (list (documentation 'dsmc-short 'setf) (documentation 'dsmc-long 'setf)
        (documentation 'dsmc-expander 'setf) (documentation 'dsmc-nodoc 'setf))
  ("short doc" "long doc" "expander doc" nil))

(deftest documentation-setf-method-combination.method-combination
  (list (documentation 'dsmc-short-mc 'method-combination)
        (documentation 'dsmc-long-mc 'method-combination)
        (dsmc-gf 1))
  ("short mc doc" "long mc doc" (:gf 1)))
