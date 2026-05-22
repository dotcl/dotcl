;;; CLOS regression tests

;;; Basic defclass / make-instance / slot-value
(defclass clos-point ()
  ((x :initarg :x :initform 0 :accessor point-x)
   (y :initarg :y :initform 0 :accessor point-y)))

(deftest clos-basic-make-instance
  (let ((p (make-instance 'clos-point :x 3 :y 4)))
    (list (point-x p) (point-y p)))
  (3 4))

;;; defmethod dispatch
(defgeneric clos-area (shape))
(defclass clos-square () ((side :initarg :side)))
(defmethod clos-area ((s clos-square))
  (let ((side (slot-value s 'side)))
    (* side side)))

(deftest clos-method-dispatch
  (clos-area (make-instance 'clos-square :side 5))
  25)

;;; Method combination — standard :around/:before/:after
(defvar *clos-log* nil)
(defgeneric clos-logged (x))
(defclass clos-base-obj ())
(defmethod clos-logged :before ((x clos-base-obj))
  (push :before *clos-log*))
(defmethod clos-logged ((x clos-base-obj))
  (push :primary *clos-log*)
  42)
(defmethod clos-logged :after ((x clos-base-obj))
  (push :after *clos-log*))

(deftest clos-before-after
  (let ((*clos-log* nil))
    (let ((result (clos-logged (make-instance 'clos-base-obj))))
      (list result (reverse *clos-log*))))
  (42 (:before :primary :after)))

;;; call-next-method
(defclass clos-child () ())
(defmethod clos-area ((s clos-child))
  (+ (call-next-method) 1))

;;; Initarg validation — invalid initarg should error (D174)
(deftest clos-invalid-initarg-error
  (signals-error (make-instance 'clos-point :z 99) error)
  t)

;;; defstruct
(defstruct clos-person name age)

(deftest clos-defstruct-basic
  (let ((p (make-clos-person :name "Alice" :age 30)))
    (list (clos-person-name p) (clos-person-age p)))
  ("Alice" 30))

;;; defstruct :read-only (D545) — accessor works
(defstruct clos-ro-struct
  (value 0 :read-only t))

(deftest clos-defstruct-read-only-access
  (clos-ro-struct-value (make-clos-ro-struct :value 7))
  7)

;;; typep and class-of
(deftest clos-typep
  (typep (make-instance 'clos-point) 'clos-point)
  t)

(deftest clos-class-of-name
  (class-name (class-of (make-instance 'clos-point)))
  clos-point)

;;; Issue #29: ValidateInitargs should apply to condition classes too
(define-condition clos-test-condition (error)
  ((msg :initarg :message :reader condition-msg)))

(deftest clos-condition-valid-initarg
  ;; :message is a valid slot initarg for clos-test-condition
  (let ((c (make-condition 'clos-test-condition :message "hello")))
    (condition-msg c))
  "hello")

(deftest clos-condition-invalid-initarg
  ;; :no-such-slot is NOT a valid initarg — should signal an error
  (signals-error (make-condition 'clos-test-condition :no-such-slot 42) error)
  t)

;;; Issue #30: reinitialize-instance validates initargs when no custom methods exist
;;; (Full &key-param collection from methods is deferred — see issue #30)
(defclass clos-ri-obj ()
  ((val :initarg :val :accessor ri-val :initform 0)))

(deftest clos-reinitialize-instance-invalid-initarg
  ;; :no-such-key is not a valid initarg — should signal an error (no custom methods on this class)
  (signals-error (reinitialize-instance (make-instance 'clos-ri-obj) :no-such-key 42) error)
  t)

;;; Cross-package typep: pa:widget and pb:widget are distinct classes (#204)
(defpackage :typep-test-pa (:use :cl) (:export :widget))
(defpackage :typep-test-pb (:use :cl) (:export :widget))
(defclass typep-test-pa:widget () ())
(defclass typep-test-pb:widget () ())

(deftest clos-typep-cross-package-no-false-positive
  ;; pb:widget instance must NOT satisfy pa:widget
  (typep (make-instance 'typep-test-pb:widget) 'typep-test-pa:widget)
  nil)

(deftest clos-typep-cross-package-positive
  ;; pa:widget instance must satisfy pa:widget
  (typep (make-instance 'typep-test-pa:widget) 'typep-test-pa:widget)
  t)

;;; D1085: next-method-p fast path — funcall #'next-method-p must agree with
;;; compiled (next-method-p) even when an :around method for the same GF was
;;; dispatched previously (which set nmpSym.Function to a stale closure).

(defgeneric nmp-gf (x))
(defclass nmp-base ())
(defclass nmp-derived (nmp-base) ())

;; Primary only for nmp-base (fast path: 1 primary, no before/after)
(defmethod nmp-gf ((x nmp-base))
  (list (next-method-p) (funcall #'next-method-p)))

;; :around + primary for nmp-derived — exercises InvokeWithNextMethods which
;; sets nmpSym.Function; after this dispatch nmpSym.Function must be restored
(defmethod nmp-gf :around ((x nmp-derived))
  (call-next-method))
(defmethod nmp-gf ((x nmp-derived))
  :derived-primary)

;; Compiled (next-method-p) and (funcall #'next-method-p) must both be NIL
;; when there is no next method (fast path).
(deftest nmp-fast-path-both-nil
  (nmp-gf (make-instance 'nmp-base))
  (nil nil))

;; After dispatching via :around (sets nmpSym.Function stale), the fast path
;; must reset nmpSym.Function so (funcall #'next-method-p) still returns NIL.
;; This was the D1085 regression.
(deftest nmp-fast-path-nil-after-around
  (progn
    (nmp-gf (make-instance 'nmp-derived))   ; pollutes nmpSym.Function
    (nmp-gf (make-instance 'nmp-base)))     ; fast path must reset it
  (nil nil))

;; next-method-p must return T when there IS a next method (via call-next-method
;; inside :around calling the primary for nmp-base).
(defgeneric nmp-has-next (x))
(defclass nmp-hn-base ())
(defclass nmp-hn-sub (nmp-hn-base) ())

(defvar *nmp-has-next-result* nil)
(defmethod nmp-has-next ((x nmp-hn-base)) :base-primary)
(defmethod nmp-has-next :around ((x nmp-hn-sub))
  (setf *nmp-has-next-result* (list (next-method-p) (funcall #'next-method-p)))
  (call-next-method))

(deftest nmp-has-next-method-both-t
  (progn
    (nmp-has-next (make-instance 'nmp-hn-sub))
    *nmp-has-next-result*)
  (t t))

;;; CNM capture: (call-next-method) inside a continuation passed to a GF.
;;; Without the capture fix, the inner GF dispatch overwrites _nextMethodChain,
;;; so (call-next-method) inside the thunk errors with "no next method".

(defgeneric cnm-call-thunk (fn))
(defmethod cnm-call-thunk (fn) (funcall fn))

(defgeneric cnm-capture-gf (x))
(defclass cnm-capture-base ())
(defclass cnm-capture-derived (cnm-capture-base) ())

(defmethod cnm-capture-gf ((x cnm-capture-base)) :base)
(defmethod cnm-capture-gf :around ((x cnm-capture-derived))
  ;; (call-next-method) inside the lambda must call the base primary,
  ;; even though cnm-call-thunk dispatch happens in between.
  (cnm-call-thunk (lambda () (call-next-method))))

(deftest cnm-capture-via-thunk
  (cnm-capture-gf (make-instance 'cnm-capture-derived))
  :base)

;;; Same but using #'call-next-method as a first-class value.
(defgeneric cnm-capture-gf2 (x))
(defclass cnm-capture-base2 ())
(defclass cnm-capture-derived2 (cnm-capture-base2) ())

(defmethod cnm-capture-gf2 ((x cnm-capture-base2)) :base2)
(defmethod cnm-capture-gf2 :around ((x cnm-capture-derived2))
  (cnm-call-thunk #'call-next-method))

(deftest cnm-capture-funcref-thunk
  (cnm-capture-gf2 (make-instance 'cnm-capture-derived2))
  :base2)

;;; D1097: next-method-p with args must signal program-error
(defgeneric nmp-arity-gf (x))
(defmethod nmp-arity-gf ((x t)) nil)

(deftest nmp-error-with-args
  (handler-case
    (progn (eval '(defmethod nmp-arity-gf ((x t)) (next-method-p nil)))
           (nmp-arity-gf nil)
           :no-error)
    (program-error () :ok))
  :ok)

;;; D1097: call-next-method with args that change applicable method set must error
(defgeneric cnm-applicability-gf (x))
(defmethod cnm-applicability-gf ((x (eql 0))) (call-next-method 1))
(defmethod cnm-applicability-gf ((x t)) :base)

(deftest cnm-applicability-check
  (handler-case
    (cnm-applicability-gf 0)
    (error () :ok))
  :ok)
