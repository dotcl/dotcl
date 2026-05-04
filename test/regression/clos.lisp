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
