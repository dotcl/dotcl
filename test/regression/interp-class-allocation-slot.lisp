;;; EVAL must be able to run a DEFCLASS with an :ALLOCATION :CLASS slot.
;;;
;;; The DEFCLASS expansion lowers slot definitions to %MAKE-SLOT-DEF (ordinary)
;;; and %MAKE-SLOT-DEF-WITH-ALLOCATION (with :allocation). The former had a
;;; function entity registered in the runtime; the latter was the one member of
;;; that family that did not. The compiler emits Runtime.MakeSlotDefWithAllocation
;;; inline from its own table and never looks the name up, so nothing showed on
;;; the compiled path. The tree-walk evaluator runs the expansion as real code, so
;;; it died with "Undefined function: %MAKE-SLOT-DEF-WITH-ALLOCATION" as soon as a
;;; slot carried :allocation (ansi-test CLASS-REDEFINITION.1/2/3).
;;;
;;; Investigation note: these symbols are interned in DOTCL-INTERNAL. Measuring
;;; FBOUNDP on names read in CL-USER makes ALL of them look missing, which hides
;;; the fact that only one actually is. Check the package in the macroexpand-1
;;; output before measuring.

(defun %cas (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (type-of e) (princ-to-string e))))))

;;; --- a class with an :allocation :class slot can be defined through EVAL

(deftest interp-class-allocation.defclass-compile
  (%cas :compile '(progn (eval '(defclass %cas-c1 () ((a :allocation :class :initform 'x))))
                   (slot-value (make-instance '%cas-c1) 'a)))
  x)

(deftest interp-class-allocation.defclass-interpret
  (%cas :interpret '(progn (eval '(defclass %cas-i1 () ((a :allocation :class :initform 'x))))
                     (slot-value (make-instance '%cas-i1) 'a)))
  x)

;;; --- redefinition returns the same class object (ansi CLASS-REDEFINITION.1)

(deftest interp-class-allocation.redefinition-eq-interpret
  (%cas :interpret '(let* ((c1 (eval '(defclass %cas-i2 () ((a :allocation :class :initform 'x)))))
                           (c2 (eval '(defclass %cas-i2 () ((a :allocation :class :initform 'x))))))
                     (list (eq c1 c2) (class-name c1))))
  (t %cas-i2))

;;; --- a subclass overriding with :allocation :instance (the second half of
;;; ansi CLASS-REDEFINITION.1)

(deftest interp-class-allocation.subclass-redefinition-interpret
  (%cas :interpret '(let* ((p1 (eval '(defclass %cas-i3 () ((a :allocation :class :initform 'x)))))
                           (s1 (eval '(defclass %cas-i4 (%cas-i3) ((a :allocation :instance)))))
                           (p2 (eval '(defclass %cas-i3 () ((a :allocation :class :initform 'x)))))
                           (s2 (eval '(defclass %cas-i4 (%cas-i3) ((a :allocation :instance))))))
                     (list (eq p1 p2) (eq s1 s2)
                           (slot-value (make-instance '%cas-i3) 'a)
                           (slot-value (make-instance '%cas-i4) 'a))))
  (t t x x))

;;; --- the class slot must really be shared between instances. This rejects a
;;; fix that resolves the name but drops the :allocation.

(deftest interp-class-allocation.shared-across-instances-interpret
  (%cas :interpret '(progn
                     (eval '(defclass %cas-i5 () ((a :allocation :class :initform 0))))
                     (let ((x (make-instance '%cas-i5))
                           (y (make-instance '%cas-i5)))
                       (setf (slot-value x 'a) 42)
                       (list (slot-value y 'a) (eql (slot-value x 'a) (slot-value y 'a))))))
  (42 t))

;;; --- over-fix guard: an ordinary slot without :allocation is not shared

(deftest interp-class-allocation.instance-slot-not-shared-interpret
  (%cas :interpret '(progn
                     (eval '(defclass %cas-i6 () ((a :initform 0))))
                     (let ((x (make-instance '%cas-i6))
                           (y (make-instance '%cas-i6)))
                       (setf (slot-value x 'a) 42)
                       (list (slot-value y 'a) (slot-value x 'a)))))
  (0 42))

;;; --- works together with :initarg / :initform

(deftest interp-class-allocation.initarg-interpret
  (%cas :interpret '(progn
                     (eval '(defclass %cas-i7 () ((a :allocation :class :initarg :a :initform 'd))))
                     (list (slot-value (make-instance '%cas-i7) 'a)
                           (slot-value (make-instance '%cas-i7 :a 'given) 'a))))
  (d given))
