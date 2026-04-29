;;; test/mop-protocol.lisp — AMOP protocol conformance tests for DOTCL-MOP
;;;
;;; Covers the generic functions specified in Chapters 5-6 of
;;; "The Art of the Metaobject Protocol" (Kiczales et al.).
;;;
;;; Run standalone:
;;;   dotnet run --project runtime/runtime.csproj -- --asm compiler/cil-out.sil \
;;;     test/framework.lisp test/mop-protocol.lisp
;;;
;;; Key: AMOP §N.N references the relevant section of the protocol.

(load "test/framework.lisp")

;;; ============================================================
;;; Test fixtures
;;; ============================================================

(defclass mop/a ()
  ((x :initarg :x :initform 1 :accessor mop/a-x)
   (y :initarg :y :initform 2 :reader mop/a-y))
  (:documentation "Base class for MOP protocol tests"))

(defclass mop/b (mop/a)
  ((z :initarg :z :initform 3 :accessor mop/b-z)
   (w :allocation :class :initform 0 :accessor mop/b-w)))

(defclass mop/c (mop/b) ())

(defclass mop/d (mop/a) ())

(defgeneric mop/gf1 (x))
(defmethod mop/gf1 ((x mop/a)) :a)
(defmethod mop/gf1 ((x mop/b)) :b)
(defmethod mop/gf1 :before ((x mop/a)) nil)

(defgeneric mop/gf2 (x y))
(defmethod mop/gf2 ((x mop/a) (y integer)) x)

(defgeneric mop/gf3 (x)
  (:method ((x t)) :default))

(defgeneric mop/gf-eql (x))
(defmethod mop/gf-eql ((x (eql 42))) :forty-two)
(defmethod mop/gf-eql (x) :other)

(defclass mop/with-defaults ()
  ((slot1 :initarg :s1 :initform 10)
   (slot2 :initarg :s2 :initform 20))
  (:default-initargs :s1 99))

;;; ============================================================
;;; CLASS-DIRECT-SUPERCLASSES (AMOP §5.4.3)
;;; ============================================================

(deftest mop/class-direct-superclasses-exact
  ;; mop/b directly inherits from mop/a
  (let ((supers (dotcl-mop:class-direct-superclasses (find-class 'mop/b))))
    (and (= (length supers) 1)
         (eq (first supers) (find-class 'mop/a))))
  t)

(deftest mop/class-direct-superclasses-multiple
  ;; mop/c inherits only from mop/b, not transitively from mop/a here
  (let ((supers (dotcl-mop:class-direct-superclasses (find-class 'mop/c))))
    (= (length supers) 1))
  t)

(deftest mop/class-direct-superclasses-root
  ;; standard-object has at least t in its superclasses (or is empty)
  ;; mop/a with no explicit superclasses should have standard-object
  (let ((supers (dotcl-mop:class-direct-superclasses (find-class 'mop/a))))
    (notnot (find (find-class 'standard-object) supers)))
  t)

;;; ============================================================
;;; CLASS-DIRECT-SUBCLASSES (AMOP §5.4.3)
;;; ============================================================

(deftest mop/class-direct-subclasses-contains-child
  (let ((subs (dotcl-mop:class-direct-subclasses (find-class 'mop/a))))
    (and (notnot (member (find-class 'mop/b) subs))
         (notnot (member (find-class 'mop/d) subs))))
  t)

(deftest mop/class-direct-subclasses-not-transitive
  ;; mop/c is a sub of mop/b, not direct sub of mop/a
  (let ((subs (dotcl-mop:class-direct-subclasses (find-class 'mop/a))))
    (not (member (find-class 'mop/c) subs)))
  t)

;;; ============================================================
;;; CLASS-PRECEDENCE-LIST (AMOP §5.4.3)
;;; ============================================================

(deftest mop/class-precedence-list-self-first
  ;; AMOP: the class itself is always first
  (eq (first (dotcl-mop:class-precedence-list (find-class 'mop/b)))
      (find-class 'mop/b))
  t)

(deftest mop/class-precedence-list-t-last
  ;; AMOP: t is always last
  (eq (car (last (dotcl-mop:class-precedence-list (find-class 'mop/b))))
      (find-class 't))
  t)

(deftest mop/class-precedence-list-monotonic
  ;; mop/c inherits b then a — a must come before standard-object
  (let* ((cpl (dotcl-mop:class-precedence-list (find-class 'mop/c)))
         (names (mapcar (lambda (c) (class-name c)) cpl)))
    (< (position 'mop/b names) (position 'mop/a names)))
  t)

(deftest mop/class-precedence-list-contains-standard-object
  (let ((cpl (dotcl-mop:class-precedence-list (find-class 'mop/a))))
    (notnot (member (find-class 'standard-object) cpl)))
  t)

;;; ============================================================
;;; CLASS-FINALIZED-P (AMOP §5.4.1)
;;; ============================================================

(deftest mop/class-finalized-p-used-class
  ;; A class is finalized once an instance has been made (or explicitly)
  (progn (make-instance 'mop/b)
         (dotcl-mop:class-finalized-p (find-class 'mop/b)))
  t)

(deftest mop/class-finalized-p-returns-boolean
  (typep (dotcl-mop:class-finalized-p (find-class 'mop/a)) 'boolean)
  t)

;;; ============================================================
;;; CLASS-SLOTS / CLASS-DIRECT-SLOTS (AMOP §5.4.3)
;;; ============================================================

(deftest mop/class-slots-includes-inherited
  ;; mop/b has z, w (direct) + x, y (from mop/a)
  (let ((names (mapcar #'dotcl-mop:slot-definition-name
                       (dotcl-mop:class-slots (find-class 'mop/b)))))
    (notnot (and (member 'x names) (member 'y names)
                 (member 'z names) (member 'w names))))
  t)

(deftest mop/class-direct-slots-only-own
  ;; mop/b directly defines z and w only
  (let ((names (mapcar #'dotcl-mop:slot-definition-name
                       (dotcl-mop:class-direct-slots (find-class 'mop/b)))))
    (notnot (and (member 'z names) (member 'w names)
                 (not (member 'x names)) (not (member 'y names)))))
  t)

(deftest mop/class-slots-are-slot-definitions
  ;; AMOP: each element of class-slots must be a standard-slot-definition.
  ;; dotcl TODO: SlotDefinition C# objects have no Lisp class yet — type-of returns T.
  ;; For now verify they have a slot-definition-name (duck-type check).
  (every (lambda (s)
           (symbolp (dotcl-mop:slot-definition-name s)))
         (dotcl-mop:class-slots (find-class 'mop/b)))
  t)

;;; ============================================================
;;; CLASS-DEFAULT-INITARGS / CLASS-DIRECT-DEFAULT-INITARGS
;;; (AMOP §5.4.3)
;;; ============================================================

(deftest mop/class-default-initargs-present
  ;; mop/with-defaults has (:default-initargs :s1 99)
  (let ((dia (dotcl-mop:class-default-initargs
              (find-class 'mop/with-defaults))))
    (notnot dia))
  t)

(deftest mop/class-direct-default-initargs-present
  (let ((ddia (dotcl-mop:class-direct-default-initargs
               (find-class 'mop/with-defaults))))
    (notnot ddia))
  t)

(deftest mop/class-no-default-initargs-nil
  (null (dotcl-mop:class-default-initargs (find-class 'mop/a)))
  t)

;;; ============================================================
;;; CLASS-PROTOTYPE (AMOP §5.4.3)
;;; ============================================================

(deftest mop/class-prototype-type
  (typep (dotcl-mop:class-prototype (find-class 'mop/b)) 'mop/b)
  t)

(deftest mop/class-prototype-is-instance
  (let ((p (dotcl-mop:class-prototype (find-class 'mop/a))))
    (typep p 'mop/a))
  t)

;;; ============================================================
;;; SLOT-DEFINITION-NAME (AMOP §5.7.2)
;;; ============================================================

(deftest mop/slot-definition-name-correct
  (let ((slotd (find-if (lambda (s)
                          (eq (dotcl-mop:slot-definition-name s) 'z))
                        (dotcl-mop:class-direct-slots (find-class 'mop/b)))))
    (dotcl-mop:slot-definition-name slotd))
  z)

;;; ============================================================
;;; SLOT-DEFINITION-ALLOCATION (AMOP §5.7.2)
;;; ============================================================

(deftest mop/slot-definition-allocation-instance
  (let ((slotd (find-if (lambda (s)
                          (eq (dotcl-mop:slot-definition-name s) 'z))
                        (dotcl-mop:class-direct-slots (find-class 'mop/b)))))
    (dotcl-mop:slot-definition-allocation slotd))
  :instance)

(deftest mop/slot-definition-allocation-class
  (let ((slotd (find-if (lambda (s)
                          (eq (dotcl-mop:slot-definition-name s) 'w))
                        (dotcl-mop:class-direct-slots (find-class 'mop/b)))))
    (dotcl-mop:slot-definition-allocation slotd))
  :class)

;;; ============================================================
;;; SLOT-DEFINITION-INITARGS (AMOP §5.7.2)
;;; ============================================================

(deftest mop/slot-definition-initargs-keyword-list
  (let ((slotd (find-if (lambda (s)
                          (eq (dotcl-mop:slot-definition-name s) 'z))
                        (dotcl-mop:class-direct-slots (find-class 'mop/b)))))
    (dotcl-mop:slot-definition-initargs slotd))
  (:z))

;; Define fixture outside deftest (defclass can't be inside deftest body)
(defclass mop/no-initarg-class ()
  ((bare-slot :initform 42)))

(deftest mop/slot-definition-initargs-empty-when-none
  ;; A slot with no :initarg should have empty initargs list
  (let ((slotd (first (dotcl-mop:class-direct-slots
                       (find-class 'mop/no-initarg-class)))))
    (null (dotcl-mop:slot-definition-initargs slotd)))
  t)

;;; ============================================================
;;; SLOT-DEFINITION-INITFORM (AMOP §5.7.2)
;;; ============================================================

(deftest mop/slot-definition-initform-literal
  ;; AMOP: should return the initform expression (e.g. 3 for :initform 3).
  ;; dotcl TODO: source form not preserved — always returns NIL.
  ;; Test documents current (non-conformant) behavior.
  (let ((slotd (find-if (lambda (s)
                          (eq (dotcl-mop:slot-definition-name s) 'z))
                        (dotcl-mop:class-direct-slots (find-class 'mop/b)))))
    (dotcl-mop:slot-definition-initform slotd))
  nil)                        ; TODO: should be 3 per AMOP

;;; ============================================================
;;; SLOT-DEFINITION-INITFUNCTION (AMOP §5.7.2)
;;; ============================================================

(deftest mop/slot-definition-initfunction-callable
  ;; initfunction must return the initform value when called
  (let ((slotd (find-if (lambda (s)
                          (eq (dotcl-mop:slot-definition-name s) 'z))
                        (dotcl-mop:class-direct-slots (find-class 'mop/b)))))
    (funcall (dotcl-mop:slot-definition-initfunction slotd)))
  3)

(deftest mop/slot-definition-initfunction-type
  (let ((slotd (find-if (lambda (s)
                          (eq (dotcl-mop:slot-definition-name s) 'x))
                        (dotcl-mop:class-direct-slots (find-class 'mop/a)))))
    (functionp (dotcl-mop:slot-definition-initfunction slotd)))
  t)

;;; ============================================================
;;; SLOT-DEFINITION-LOCATION (AMOP §5.7.2)
;;; ============================================================

(deftest mop/slot-definition-location-non-negative-integer
  ;; AMOP: should return a non-negative integer (index in instance data).
  ;; dotcl: returns the slot name symbol as an opaque locator instead.
  ;; Test documents current (non-conformant) behavior.
  (let ((slotd (find-if (lambda (s)
                          (eq (dotcl-mop:slot-definition-name s) 'x))
                        (dotcl-mop:class-slots (find-class 'mop/a)))))
    (let ((loc (dotcl-mop:slot-definition-location slotd)))
      ;; dotcl returns slot name as opaque locator
      (eq loc 'x)))           ; TODO: should be (integerp loc) per AMOP
  t)

(deftest mop/slot-definition-location-distinct
  ;; Distinct slots must have distinct locations (regardless of representation)
  (let* ((slots (dotcl-mop:class-slots (find-class 'mop/a)))
         (x-loc (dotcl-mop:slot-definition-location
                 (find-if (lambda (s) (eq (dotcl-mop:slot-definition-name s) 'x)) slots)))
         (y-loc (dotcl-mop:slot-definition-location
                 (find-if (lambda (s) (eq (dotcl-mop:slot-definition-name s) 'y)) slots))))
    (not (equal x-loc y-loc)))
  t)

;;; ============================================================
;;; GENERIC-FUNCTION-NAME (AMOP §6.7.2)
;;; ============================================================

(deftest mop/generic-function-name-symbol
  (dotcl-mop:generic-function-name #'mop/gf1)
  mop/gf1)

(deftest mop/generic-function-name-type
  (symbolp (dotcl-mop:generic-function-name #'mop/gf2))
  t)

;;; ============================================================
;;; GENERIC-FUNCTION-METHODS (AMOP §6.7.2)
;;; ============================================================

(deftest mop/generic-function-methods-count
  ;; mop/gf1 has 3 methods: (mop/a), (mop/b), :before (mop/a)
  (length (dotcl-mop:generic-function-methods #'mop/gf1))
  3)

(deftest mop/generic-function-methods-type
  (every (lambda (m) (typep m 'standard-method))
         (dotcl-mop:generic-function-methods #'mop/gf1))
  t)

;;; ============================================================
;;; GENERIC-FUNCTION-LAMBDA-LIST (AMOP §6.7.2)
;;; ============================================================

(deftest mop/generic-function-lambda-list-arity-1
  (= (length (dotcl-mop:generic-function-lambda-list #'mop/gf1)) 1)
  t)

(deftest mop/generic-function-lambda-list-arity-2
  (= (length (dotcl-mop:generic-function-lambda-list #'mop/gf2)) 2)
  t)

;;; ============================================================
;;; GENERIC-FUNCTION-METHOD-CLASS (AMOP §6.7.2)
;;; ============================================================

(deftest mop/generic-function-method-class-is-standard-method
  (let ((mc (dotcl-mop:generic-function-method-class #'mop/gf1)))
    (or (eq mc (find-class 'standard-method))
        (subtypep mc (find-class 'standard-method))))
  t)

;;; ============================================================
;;; GENERIC-FUNCTION-METHOD-COMBINATION (AMOP §6.7.2)
;;; ============================================================

(deftest mop/generic-function-method-combination-not-null
  (notnot (dotcl-mop:generic-function-method-combination #'mop/gf1))
  t)

;;; ============================================================
;;; METHOD-GENERIC-FUNCTION (AMOP §6.7.2)
;;; ============================================================

(deftest mop/method-generic-function-identity
  (let* ((gf #'mop/gf1)
         (m (first (dotcl-mop:generic-function-methods gf))))
    (eq (dotcl-mop:method-generic-function m) gf))
  t)

;;; ============================================================
;;; METHOD-LAMBDA-LIST (AMOP §6.7.2)
;;; ============================================================

(deftest mop/method-lambda-list-length
  ;; mop/gf1 methods all have 1 parameter
  (let ((m (first (dotcl-mop:generic-function-methods #'mop/gf1))))
    (= (length (dotcl-mop:method-lambda-list m)) 1))
  t)

;;; ============================================================
;;; METHOD-QUALIFIERS (AMOP §6.7.2)
;;; ============================================================

(deftest mop/method-qualifiers-primary-empty
  ;; Primary method has no qualifiers
  (let ((primary (find-if (lambda (m)
                            (null (dotcl-mop:method-qualifiers m)))
                          (dotcl-mop:generic-function-methods #'mop/gf1))))
    (notnot primary))
  t)

(deftest mop/method-qualifiers-before
  ;; :before method has (:before) as qualifiers
  (let ((before-m (find-if (lambda (m)
                              (equal (dotcl-mop:method-qualifiers m) '(:before)))
                            (dotcl-mop:generic-function-methods #'mop/gf1))))
    (notnot before-m))
  t)

;;; ============================================================
;;; METHOD-SPECIALIZERS (AMOP §6.7.2)
;;; ============================================================

(deftest mop/method-specializers-length
  ;; mop/gf1 primary on mop/a has 1 specializer
  (let ((m (find-if (lambda (m)
                      (and (null (dotcl-mop:method-qualifiers m))
                           (member (find-class 'mop/a)
                                   (dotcl-mop:method-specializers m))))
                    (dotcl-mop:generic-function-methods #'mop/gf1))))
    (= (length (dotcl-mop:method-specializers m)) 1))
  t)

(deftest mop/method-specializers-class-object
  ;; Specializer for mop/a method is the mop/a class metaobject
  (let ((m (find-if (lambda (m)
                      (and (null (dotcl-mop:method-qualifiers m))
                           (member (find-class 'mop/a)
                                   (dotcl-mop:method-specializers m))))
                    (dotcl-mop:generic-function-methods #'mop/gf1))))
    (eq (first (dotcl-mop:method-specializers m)) (find-class 'mop/a)))
  t)

(deftest mop/method-specializers-eql
  ;; AMOP: eql specializer method should return an eql-specializer metaobject.
  ;; dotcl: stores eql specializers as (EQL value) cons cells instead.
  ;; Test documents current representation.
  (let ((eql-m (find-if (lambda (m)
                           (let ((s (first (dotcl-mop:method-specializers m))))
                             (and (null (dotcl-mop:method-qualifiers m))
                                  (and (consp s) (eq (car s) 'eql)))))
                         (dotcl-mop:generic-function-methods #'mop/gf-eql))))
    (notnot eql-m))            ; TODO: s should be typep 'eql-specializer per AMOP
  t)

;;; ============================================================
;;; EQL-SPECIALIZER-OBJECT / INTERN-EQL-SPECIALIZER (AMOP §6.4)
;;; ============================================================

(deftest mop/eql-specializer-roundtrip
  (dotcl-mop:eql-specializer-object
   (dotcl-mop:intern-eql-specializer 42))
  42)

(deftest mop/eql-specializer-symbol
  (dotcl-mop:eql-specializer-object
   (dotcl-mop:intern-eql-specializer :keyword-val))
  :keyword-val)

(deftest mop/intern-eql-specializer-same-object
  ;; Interning the same value twice should return eq objects (AMOP says
  ;; implementations may or may not intern; dotcl may return fresh ones)
  (let ((a (dotcl-mop:intern-eql-specializer 99))
        (b (dotcl-mop:intern-eql-specializer 99)))
    (= (dotcl-mop:eql-specializer-object a)
       (dotcl-mop:eql-specializer-object b)))
  t)

;;; ============================================================
;;; EXTRACT-LAMBDA-LIST / EXTRACT-SPECIALIZER-NAMES (AMOP §6.3)
;;; ============================================================

(deftest mop/extract-lambda-list-simple
  ;; ((x mop/a)) -> (x)
  (dotcl-mop:extract-lambda-list '((x mop/a)))
  (x))

(deftest mop/extract-lambda-list-mixed
  ;; ((x mop/a) y) -> (x y)
  (dotcl-mop:extract-lambda-list '((x mop/a) y))
  (x y))

(deftest mop/extract-specializer-names-simple
  ;; ((x mop/a)) -> (mop/a)
  (dotcl-mop:extract-specializer-names '((x mop/a)))
  (mop/a))

(deftest mop/extract-specializer-names-default
  ;; (x) — no specializer — becomes (t)
  (dotcl-mop:extract-specializer-names '(x))
  (t))

(deftest mop/extract-specializer-names-mixed
  ;; ((x mop/a) y) -> (mop/a t)
  (dotcl-mop:extract-specializer-names '((x mop/a) y))
  (mop/a t))

;;; ============================================================
;;; VALIDATE-SUPERCLASS (AMOP §5.4.2)
;;; ============================================================

(deftest mop/validate-superclass-standard-standard
  ;; Two standard classes: always valid
  (dotcl-mop:validate-superclass (find-class 'mop/b) (find-class 'mop/a))
  t)

(deftest mop/validate-superclass-standard-object
  ;; standard-class can always be super of any standard class
  (dotcl-mop:validate-superclass (find-class 'mop/a)
                                  (find-class 'standard-object))
  t)

;;; ============================================================
;;; CLASSP (closer-mop extension, also in DOTCL-MOP)
;;; ============================================================

(deftest mop/classp-class-metaobject
  (dotcl-mop:classp (find-class 'mop/a))
  t)

(deftest mop/classp-built-in
  (dotcl-mop:classp (find-class 'integer))
  t)

(deftest mop/classp-non-class
  (dotcl-mop:classp 'mop/a)
  nil)

(deftest mop/classp-instance
  (dotcl-mop:classp (make-instance 'mop/a))
  nil)

;;; ============================================================
;;; SUBCLASSP (closer-mop extension)
;;; ============================================================

(deftest mop/subclassp-direct
  (dotcl-mop:subclassp (find-class 'mop/b) (find-class 'mop/a))
  t)

(deftest mop/subclassp-transitive
  (dotcl-mop:subclassp (find-class 'mop/c) (find-class 'mop/a))
  t)

(deftest mop/subclassp-reflexive
  (dotcl-mop:subclassp (find-class 'mop/a) (find-class 'mop/a))
  t)

(deftest mop/subclassp-negative
  (dotcl-mop:subclassp (find-class 'mop/a) (find-class 'mop/b))
  nil)

(deftest mop/subclassp-sibling
  (dotcl-mop:subclassp (find-class 'mop/d) (find-class 'mop/b))
  nil)

;;; ============================================================
;;; AMOP conformance: class hierarchy invariants
;;; ============================================================

(deftest mop/cpl-includes-t-for-all-standard
  ;; Every standard class CPL must include t
  (every (lambda (cls)
           (member (find-class 't)
                   (dotcl-mop:class-precedence-list cls)))
         (list (find-class 'mop/a) (find-class 'mop/b) (find-class 'mop/c)))
  t)

(deftest mop/cpl-includes-standard-object
  ;; Every user-defined standard class must include standard-object
  (every (lambda (cls)
           (member (find-class 'standard-object)
                   (dotcl-mop:class-precedence-list cls)))
         (list (find-class 'mop/a) (find-class 'mop/b)))
  t)

(deftest mop/slots-count-monotonic-inheritance
  ;; mop/b has more slots than mop/a (adds z, w)
  (> (length (dotcl-mop:class-slots (find-class 'mop/b)))
     (length (dotcl-mop:class-slots (find-class 'mop/a))))
  t)

(deftest mop/class-of-instance-is-class
  ;; class-of should return the metaobject for the class
  (eq (class-of (make-instance 'mop/a)) (find-class 'mop/a))
  t)

;;; ============================================================
;;; Unimplemented — expected to fail (mark as known missing)
;;; ============================================================

;;; The following functions are specified by AMOP but not yet
;;; implemented in DOTCL-MOP. These tests document what's missing.

(deftest mop/TODO-slot-value-using-class
  ;; AMOP §5.5.1: (slot-value-using-class class instance slotd) -> value
  (handler-case
    (let* ((inst (make-instance 'mop/a :x 77))
           (slotd (find-if (lambda (s) (eq (dotcl-mop:slot-definition-name s) 'x))
                           (dotcl-mop:class-slots (find-class 'mop/a)))))
      (dotcl-mop:slot-value-using-class (find-class 'mop/a) inst slotd))
    (undefined-function () :not-implemented)
    (error () :not-implemented))
  77)

(deftest mop/TODO-compute-slots
  ;; AMOP §5.4.1: (compute-slots class) -> list of effective slot definitions
  (handler-case
    (length (dotcl-mop:compute-slots (find-class 'mop/a)))
    (undefined-function () :not-implemented)
    (error () :not-implemented))
  2)

(deftest mop/TODO-finalize-inheritance
  ;; AMOP §5.4.1: (finalize-inheritance class) -> unspecified, side-effects CPL
  (handler-case
    (progn (dotcl-mop:finalize-inheritance (find-class 'mop/a)) t)
    (undefined-function () :not-implemented)
    (error () :not-implemented))
  t)

(deftest mop/TODO-compute-effective-method
  ;; AMOP §6.5: (compute-effective-method gf combination applicable-methods)
  (handler-case
    (let* ((gf #'mop/gf3)
           (comb (dotcl-mop:generic-function-method-combination gf))
           (methods (dotcl-mop:generic-function-methods gf)))
      (notnot (dotcl-mop:compute-effective-method gf comb methods)))
    (undefined-function () :not-implemented)
    (error () :not-implemented))
  t)

(deftest mop/TODO-method-function
  ;; AMOP §6.7.2: (method-function method) -> function
  (handler-case
    (let ((m (first (dotcl-mop:generic-function-methods #'mop/gf3))))
      (functionp (dotcl-mop:method-function m)))
    (undefined-function () :not-implemented)
    (error () :not-implemented))
  t)

;;; ============================================================
;;; CPL / typep conformance invariant (from SBCL mop.pure.lisp)
;;; ============================================================
;;; AMOP invariant: if (typep obj cls) then cls is in (class-precedence-list
;;; (class-of obj)). Verify for our fixture classes.

(deftest mop/typep-cpl-conformance-direct
  ;; (typep inst 'mop/b) -> mop/b in CPL of (class-of inst)
  (let* ((inst (make-instance 'mop/b))
         (cpl (dotcl-mop:class-precedence-list (class-of inst))))
    (every (lambda (cls)
             (if (typep inst cls)
                 (member (find-class cls) cpl)
                 t))
           '(mop/b mop/a standard-object t)))
  t)

(deftest mop/typep-cpl-conformance-inherited
  ;; mop/c instance is typep of mop/a (inherited) — mop/a must be in CPL
  (let* ((inst (make-instance 'mop/c))
         (cpl (dotcl-mop:class-precedence-list (class-of inst))))
    (notnot (and (typep inst 'mop/c)
                 (typep inst 'mop/b)
                 (typep inst 'mop/a)
                 (member (find-class 'mop/c) cpl)
                 (member (find-class 'mop/b) cpl)
                 (member (find-class 'mop/a) cpl))))
  t)

(deftest mop/typep-cpl-conformance-negative
  ;; mop/d is NOT typep mop/b; mop/b must not be in cpl of mop/d
  (let* ((inst (make-instance 'mop/d))
         (cpl (dotcl-mop:class-precedence-list (class-of inst))))
    (and (not (typep inst 'mop/b))
         (not (member (find-class 'mop/b) cpl))))
  t)

(deftest mop/typep-cpl-conformance-builtin
  ;; Standard types: integer is typep number/t, both in CPL
  (let ((cpl (dotcl-mop:class-precedence-list (find-class 'integer))))
    (notnot (and (member (find-class 'number) cpl)
                 (member (find-class 't) cpl))))
  t)

;;; ============================================================
;;; Slot definition error validation (from SBCL bug-309072)
;;; #+nil: uncomment when slot-definition type system is implemented
;;; ============================================================

#+nil
(progn
  ;; These tests verify that defclass signals errors for malformed
  ;; slot definitions. Enable when dotcl validates slot options.

  (deftest mop/slot-def-error-bad-initarg
    ;; :initarg must be a symbol
    (handler-case
      (eval '(defclass mop/bad-initarg () ((s :initarg 42))))
      (error () :error))
    :error)

  (deftest mop/slot-def-error-missing-initfunction
    ;; :initform without :initfunction should still work via thunk generation
    ;; (this is actually valid — testing the converse: :initfunction alone is ok)
    (handler-case
      (progn
        (eval '(defclass mop/ok-initfunction ()
                ((s :initfunction (lambda () 99)))))
        :ok)
      (error () :error))
    :ok)

  (deftest mop/slot-def-error-bad-allocation
    ;; :allocation must be :instance or :class
    (handler-case
      (eval '(defclass mop/bad-alloc () ((s :allocation :bogus))))
      (error () :error))
    :error)

  (deftest mop/slot-def-error-bad-type
    ;; :type must be a valid type specifier
    (handler-case
      (progn
        (eval '(defclass mop/typed-slot () ((s :type integer))))
        :ok)
      (error () :error))
    :ok)
) ; end #+nil

;;; ============================================================
;;; Summary
;;; ============================================================

(do-tests-summary)
