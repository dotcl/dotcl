;;; :DEFAULT-INITARGS under FUNCALLABLE-STANDARD-CLASS.
;;;
;;; MAKE-INSTANCE allocates an instance of a funcallable class through a
;;; specialized allocator, because the object has to be callable. That branch
;;; called INITIALIZE-INSTANCE with the initargs it was handed and returned,
;;; passing over the place where defaulted initargs are added, so a default
;;; initarg was silently ignored and an inherited :INITFORM ran instead. A
;;; validation library whose base class has (:initform (error "...")) on a slot
;;; that each concrete subclass fills through (:default-initargs ...) could then
;;; not construct a single object.
;;;
;;; The class-level computation was never at fault: CLASS-DEFAULT-INITARGS
;;; reported the initarg all along. Each test below is written twice, once per
;;; metaclass, so the STANDARD-CLASS control travels with the funcallable case.
;;;
;;; CLHS 7.1.4 fixes the precedence: initargs given to MAKE-INSTANCE beat
;;; defaulted initargs, which beat slot initforms.

;;; ---- a default initarg supplies a slot ----

(defclass fdi-func ()
  ((slot :initarg :slot :reader fdi-func-slot))
  (:metaclass dotcl-mop:funcallable-standard-class)
  (:default-initargs :slot :from-default))

(defclass fdi-std ()
  ((slot :initarg :slot :reader fdi-std-slot))
  (:default-initargs :slot :from-default))

(deftest funcallable-default-initargs.supplies-slot
  (fdi-func-slot (make-instance 'fdi-func))
  :from-default)

(deftest funcallable-default-initargs.supplies-slot-standard-control
  (fdi-std-slot (make-instance 'fdi-std))
  :from-default)

;;; ---- an explicit initarg still overrides the default one ----

(deftest funcallable-default-initargs.explicit-overrides-default
  (fdi-func-slot (make-instance 'fdi-func :slot :explicit))
  :explicit)

(deftest funcallable-default-initargs.explicit-overrides-default-standard-control
  (fdi-std-slot (make-instance 'fdi-std :slot :explicit))
  :explicit)

;;; ---- a default initarg still beats an :INITFORM ----

(defclass fdi-func-initform ()
  ((slot :initarg :slot :reader fdi-func-initform-slot :initform :from-initform))
  (:metaclass dotcl-mop:funcallable-standard-class)
  (:default-initargs :slot :from-default))

(defclass fdi-std-initform ()
  ((slot :initarg :slot :reader fdi-std-initform-slot :initform :from-initform))
  (:default-initargs :slot :from-default))

(deftest funcallable-default-initargs.default-beats-initform
  (fdi-func-initform-slot (make-instance 'fdi-func-initform))
  :from-default)

(deftest funcallable-default-initargs.default-beats-initform-standard-control
  (fdi-std-initform-slot (make-instance 'fdi-std-initform))
  :from-default)

(defclass fdi-func-no-default ()
  ((slot :initarg :slot :initform :from-initform))
  (:metaclass dotcl-mop:funcallable-standard-class))

;; All three ranks at once, the order CLHS 7.1.4 gives.
(deftest funcallable-default-initargs.explicit-beats-default-beats-initform
  (list (fdi-func-initform-slot (make-instance 'fdi-func-initform :slot :explicit))
        (fdi-func-initform-slot (make-instance 'fdi-func-initform))
        (slot-value (make-instance 'fdi-func-no-default) 'slot))
  (:explicit :from-default :from-initform))

;;; ---- an inherited default initarg ----
;;;
;;; The subclass supplies the initarg for a slot the superclass declared with an
;;; erroring initform. This is the shape the validation library uses.

(defclass fdi-base ()
  ((m :initarg :m :reader fdi-m :initform (error "Provide M")))
  (:metaclass dotcl-mop:funcallable-standard-class))

(defclass fdi-sub (fdi-base)
  ()
  (:metaclass dotcl-mop:funcallable-standard-class)
  (:default-initargs :m 42))

;; Inherited two levels down, to show the default comes off the whole precedence
;; list and not just the direct superclass.
(defclass fdi-sub-sub (fdi-sub)
  ()
  (:metaclass dotcl-mop:funcallable-standard-class))

(deftest funcallable-default-initargs.inherited-from-superclass
  (list (fdi-m (make-instance 'fdi-sub))
        (fdi-m (make-instance 'fdi-sub-sub))
        (fdi-m (make-instance 'fdi-sub :m 7)))
  (42 42 7))

;; A more specific default initarg shadows the inherited one (CLHS 7.1.4: the
;; effective list keeps the most specific entry per key).
(defclass fdi-sub-override (fdi-sub)
  ()
  (:metaclass dotcl-mop:funcallable-standard-class)
  (:default-initargs :m 99))

(deftest funcallable-default-initargs.subclass-shadows-inherited-default
  (fdi-m (make-instance 'fdi-sub-override))
  99)

;;; ---- the value form is a function, so it runs per instance ----

(defvar *fdi-func-counter* 0)
(defvar *fdi-std-counter* 0)

(defclass fdi-func-counted ()
  ((slot :initarg :slot :reader fdi-func-counted-slot))
  (:metaclass dotcl-mop:funcallable-standard-class)
  (:default-initargs :slot (incf *fdi-func-counter*)))

(defclass fdi-std-counted ()
  ((slot :initarg :slot :reader fdi-std-counted-slot))
  (:default-initargs :slot (incf *fdi-std-counter*)))

(deftest funcallable-default-initargs.value-form-evaluated-per-instance
  (let ((*fdi-func-counter* 0))
    (let* ((a (fdi-func-counted-slot (make-instance 'fdi-func-counted)))
           (b (fdi-func-counted-slot (make-instance 'fdi-func-counted)))
           (c (fdi-func-counted-slot (make-instance 'fdi-func-counted))))
      (list (list a b c) *fdi-func-counter*)))
  ((1 2 3) 3))

(deftest funcallable-default-initargs.value-form-evaluated-per-instance-standard-control
  (let ((*fdi-std-counter* 0))
    (let* ((a (fdi-std-counted-slot (make-instance 'fdi-std-counted)))
           (b (fdi-std-counted-slot (make-instance 'fdi-std-counted))))
      (list (list a b) *fdi-std-counter*)))
  ((1 2) 2))

;; An explicitly supplied initarg must not evaluate the default's form at all.
(deftest funcallable-default-initargs.explicit-does-not-evaluate-default-form
  (let ((*fdi-func-counter* 0))
    (let ((v (fdi-func-counted-slot (make-instance 'fdi-func-counted :slot :explicit))))
      (list v *fdi-func-counter*)))
  (:explicit 0))

;;; ---- the MOP reports what it reported before ----
;;;
;;; The entries are (key form function); compare the first two, since the third
;;; is a fresh function object.

(defun %fdi-key-and-form (entries)
  (mapcar (lambda (e) (list (first e) (second e))) entries))

(deftest funcallable-default-initargs.class-default-initargs-unchanged
  (%fdi-key-and-form
   (dotcl-mop:class-default-initargs (find-class 'fdi-func)))
  ((:slot :from-default)))

(deftest funcallable-default-initargs.class-direct-default-initargs-unchanged
  (%fdi-key-and-form
   (dotcl-mop:class-direct-default-initargs (find-class 'fdi-func)))
  ((:slot :from-default)))

(deftest funcallable-default-initargs.class-default-initargs-inherited
  (%fdi-key-and-form
   (dotcl-mop:class-default-initargs (find-class 'fdi-sub-sub)))
  ((:m 42)))

(deftest funcallable-default-initargs.class-default-initargs-standard-control
  (%fdi-key-and-form
   (dotcl-mop:class-default-initargs (find-class 'fdi-std)))
  ((:slot :from-default)))

;;; ---- the other classes allocated the same way ----
;;;
;;; Generic function and method classes are allocated by the same specialized
;;; branch, for the same reason, so they lost their default initargs too.

(defclass fdi-gf (standard-generic-function)
  ((tag :initarg :tag :reader fdi-gf-tag :initform :from-initform))
  (:metaclass dotcl-mop:funcallable-standard-class)
  (:default-initargs :tag :from-default))

(deftest funcallable-default-initargs.generic-function-class
  (list (fdi-gf-tag (make-instance 'fdi-gf))
        (fdi-gf-tag (make-instance 'fdi-gf :tag :explicit)))
  (:from-default :explicit))

;; DEFGENERIC with that class still produces a working generic function.
(defgeneric fdi-gf-op (x)
  (:generic-function-class fdi-gf))

(defmethod fdi-gf-op ((x integer)) (* 2 x))

(deftest funcallable-default-initargs.defgeneric-with-that-class-still-works
  (list (fdi-gf-op 21) (class-name (class-of #'fdi-gf-op)))
  (42 fdi-gf))

(defclass fdi-method (standard-method)
  ((tag :initarg :tag :reader fdi-method-tag :initform :from-initform))
  (:default-initargs :tag :from-default))

(deftest funcallable-default-initargs.method-class
  (list (fdi-method-tag (make-instance 'fdi-method))
        (fdi-method-tag (make-instance 'fdi-method :tag :explicit)))
  (:from-default :explicit))

;;; ---- the instance is still callable ----
;;;
;;; The point of the metaclass. A default initarg must not cost that.

(deftest funcallable-default-initargs.instance-is-still-funcallable
  (let ((inst (make-instance 'fdi-func)))
    (dotcl-mop:set-funcallable-instance-function inst (lambda (x) (list :called x)))
    (list (funcall inst 1) (fdi-func-slot inst)))
  ((:called 1) :from-default))
