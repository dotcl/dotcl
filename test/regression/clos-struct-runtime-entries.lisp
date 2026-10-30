;;; CLOS and structure intrinsic targets that no ordinary expansion reaches;
;;; values agree with SBCL where the standard functions exist.

(setf dotcl:*save-sil* t)

(defclass %clr-a () ((x :initform 42) (y :initarg :y :initform 7)))
(defclass %clr-b () ((x :initform 0) (z :initarg :z :initform 9)))
(defstruct %clr-s a b)
(defgeneric %clr-g (x))
(defmethod %clr-g ((x integer)) (+ x 1))
(defmethod %clr-g :before ((x integer)) (declare (ignore x)) nil)
(defmethod %clr-g :after ((x integer)) (declare (ignore x)) nil)

(defun %clr-find (name) (dotcl-internal::%find-class-or-nil name))
(defun %clr-alloc (class) (dotcl-internal::%allocate-instance class))
(defun %clr-slot-exists (object name) (dotcl-internal::%slot-exists-p object name))
(defun %clr-change (object) (dotcl-internal::%change-class object '%clr-b))
(defun %clr-change-z (object) (dotcl-internal::%change-class object '%clr-b :z 99))
(defun %clr-change-odd (object) (dotcl-internal::%change-class object '%clr-b :z))
(defun %clr-change-bad (object) (dotcl-internal::%change-class object 5))
(defun %clr-change-missing (object) (dotcl-internal::%change-class object '%clr-no-such-class))
(defun %clr-gf-methods (gf) (dotcl-internal::%gf-methods gf))
(defun %clr-specializers (method) (dotcl-internal::%method-specializers method))
(defun %clr-qualifiers (method) (dotcl-internal::%method-qualifiers method))
(defun %clr-method-function (method) (dotcl-internal::%method-function method))
(defun %clr-make-class (name)
  (dotcl-internal::%make-class-full
   name (list (find-class 'standard-object)) nil (find-class 'standard-class)))
(defun %clr-struct-ref (structure index) (dotcl-internal::%struct-ref structure index))
(defun %clr-struct-set (structure index value)
  (dotcl-internal::%struct-set structure index value))

(defun %clr-error (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (program-error () :program-error)
    (type-error () :type-error)
    (error () :error)))

(deftest clos-struct-runtime-entries.find-class
  (let ((class (find-class '%clr-a)))
    (list (eq (%clr-find '%clr-a) class) (%clr-find 'clr-no-such-class)))
  (t nil))

(deftest clos-struct-runtime-entries.allocate-instance
  (let ((instance (%clr-alloc (find-class '%clr-a))))
    (list (typep instance '%clr-a)
          (slot-boundp instance 'x)
          (slot-exists-p instance 'x)
          (%clr-error (lambda () (%clr-alloc 5)))))
  (t nil t :type-error))

(deftest clos-struct-runtime-entries.slot-exists
  (let ((instance (make-instance '%clr-a)))
    (list (%clr-slot-exists instance 'x)
          (%clr-slot-exists instance 'clr-no-such-slot)
          (%clr-slot-exists 5 'x)))
  (t nil nil))

(deftest clos-struct-runtime-entries.change-class
  (let ((object (make-instance '%clr-a :y 10)))
    (let ((same (%clr-change object)))
      (let ((other (%clr-change-z (make-instance '%clr-a))))
        (list (eq same object)
              (class-name (class-of same))
              (slot-value same 'x)
              (slot-value same 'z)
              (slot-value other 'z)))))
  (t %clr-b 42 9 99))

(deftest clos-struct-runtime-entries.change-class-errors
  (let ((object (make-instance '%clr-a)))
    (list (%clr-error (lambda () (%clr-change-odd object)))
          (%clr-error (lambda () (%clr-change-bad object)))
          (%clr-error (lambda () (%clr-change-missing object)))))
  (:program-error :type-error :error))

(deftest clos-struct-runtime-entries.generic-function-methods
  (let* ((methods (%clr-gf-methods #'%clr-g))
         (qualifiers (mapcar #'%clr-qualifiers methods)))
    (list (length methods)
          (count-if #'null qualifiers)
          (count-if (lambda (q) (equal q '(:before))) qualifiers)
          (count-if (lambda (q) (equal q '(:after))) qualifiers)
          (mapcar (lambda (method)
                    (mapcar #'class-name (%clr-specializers method)))
                  methods)))
  (3 1 1 1 ((integer) (integer) (integer))))

(deftest clos-struct-runtime-entries.method-function
  (let* ((methods (%clr-gf-methods #'%clr-g))
         (primary (find-if (lambda (method) (null (%clr-qualifiers method)))
                           methods))
         (function (%clr-method-function primary)))
    (list (functionp function)
          (funcall function (list 4) nil)
          (eq function (%clr-method-function primary))))
  (t 5 t))

(deftest clos-struct-runtime-entries.method-errors
  (list (%clr-error (lambda () (%clr-gf-methods 5)))
        (%clr-error (lambda () (%clr-specializers 5)))
        (%clr-error (lambda () (%clr-qualifiers 5)))
        (%clr-error (lambda () (%clr-method-function 5))))
  (:type-error :type-error :type-error :type-error))

(deftest clos-struct-runtime-entries.make-class-full
  (let ((class (%clr-make-class '%clr-made)))
    (list (eq (class-name class) '%clr-made)
          (typep (make-instance class) class)
          (%clr-error (lambda () (%clr-make-class 5)))))
  (t t :type-error))

(deftest clos-struct-runtime-entries.struct-ref-set
  (let ((structure (make-%clr-s :a 1 :b 2))
        (index 1))
    (list (%clr-struct-ref structure index)
          (%clr-struct-set structure index 5)
          (%clr-struct-ref structure index)
          (%clr-struct-ref structure 0)))
  (2 5 5 1))

(deftest clos-struct-runtime-entries.struct-errors
  (let ((structure (make-%clr-s :a 1 :b 2))
        (index 1))
    (list (%clr-error (lambda () (%clr-struct-ref structure 'x)))
          (%clr-error (lambda () (%clr-struct-ref structure 9)))
          (%clr-error (lambda () (%clr-struct-ref 5 index)))
          (%clr-error (lambda () (%clr-struct-set structure 'x 1)))
          (%clr-error (lambda () (%clr-struct-set 5 index 1)))))
  (:type-error :type-error :type-error :type-error :type-error))

(defun %clr-has (needle fn)
  (and (search needle (%flat-text (dotcl:function-sil fn))) t))

(deftest-emitting-only clos-struct-runtime-entries.sil
  (list (%clr-has "Runtime.FindClassOrNil" #'%clr-find)
        (%clr-has "Runtime.MakeInstanceRaw" #'%clr-alloc)
        (%clr-has "Runtime.SlotExists" #'%clr-slot-exists)
        (%clr-has "Runtime.ChangeClass" #'%clr-change)
        (%clr-has "Runtime.GetGFMethods" #'%clr-gf-methods)
        (%clr-has "Runtime.MethodSpecializers" #'%clr-specializers)
        (%clr-has "Runtime.MethodQualifiers" #'%clr-qualifiers)
        (%clr-has "Runtime.MethodFunction" #'%clr-method-function)
        (%clr-has "Runtime.MakeClassFull" #'%clr-make-class)
        (%clr-has "Runtime.StructRef" #'%clr-struct-ref)
        (%clr-has "Runtime.StructSet" #'%clr-struct-set))
  (t t t t t t t t t t t))
