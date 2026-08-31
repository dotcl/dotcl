;;; DOTCL-MOP regression tests
;;; Tests for the AMOP introspection wrappers in Mop.cs.
;;; Covers only the subset actually implemented; stubs are excluded.

;;; --- test fixtures ---

(defclass mop-animal ()
  ((name :initarg :name :accessor mop-animal-name)
   (age  :initarg :age  :initform 0)))

(defclass mop-dog (mop-animal)
  ((breed :initarg :breed :initform "unknown")))

(defgeneric mop-speak (animal))
(defmethod mop-speak ((a mop-animal)) "...")
(defmethod mop-speak ((d mop-dog)) "woof")

;;; --- class introspection ---

(deftest mop-class-direct-superclasses
  (let ((supers (dotcl-mop:class-direct-superclasses (find-class 'mop-dog))))
    (notnot (member (find-class 'mop-animal) supers)))
  t)

(deftest mop-class-precedence-list-contains-self
  (let ((cpl (dotcl-mop:class-precedence-list (find-class 'mop-dog))))
    (notnot (member (find-class 'mop-dog) cpl)))
  t)

(deftest mop-class-precedence-list-contains-parent
  (let ((cpl (dotcl-mop:class-precedence-list (find-class 'mop-dog))))
    (notnot (member (find-class 'mop-animal) cpl)))
  t)

(deftest mop-class-finalized-p
  (dotcl-mop:class-finalized-p (find-class 'mop-dog))
  t)

(deftest mop-class-direct-subclasses-contains-child
  (let ((subs (dotcl-mop:class-direct-subclasses (find-class 'mop-animal))))
    (notnot (member (find-class 'mop-dog) subs)))
  t)

;;; --- slot introspection ---

(deftest mop-class-slots-count
  (length (dotcl-mop:class-slots (find-class 'mop-dog)))
  3)  ; name, age (inherited), breed

(deftest mop-class-direct-slots-count
  (length (dotcl-mop:class-direct-slots (find-class 'mop-dog)))
  1)  ; breed only

(deftest mop-slot-definition-name
  (let ((slotd (find-if (lambda (s)
                          (eq (dotcl-mop:slot-definition-name s) 'breed))
                        (dotcl-mop:class-slots (find-class 'mop-dog)))))
    (dotcl-mop:slot-definition-name slotd))
  breed)

(deftest mop-slot-definition-allocation-instance
  (let ((slotd (find-if (lambda (s)
                          (eq (dotcl-mop:slot-definition-name s) 'breed))
                        (dotcl-mop:class-slots (find-class 'mop-dog)))))
    (dotcl-mop:slot-definition-allocation slotd))
  :instance)

(deftest mop-slot-definition-initargs
  (let ((slotd (find-if (lambda (s)
                          (eq (dotcl-mop:slot-definition-name s) 'breed))
                        (dotcl-mop:class-slots (find-class 'mop-dog)))))
    (dotcl-mop:slot-definition-initargs slotd))
  (:breed))

;;; --- generic function introspection ---

(deftest mop-generic-function-name
  (dotcl-mop:generic-function-name #'mop-speak)
  mop-speak)

(deftest mop-generic-function-methods-count
  (length (dotcl-mop:generic-function-methods #'mop-speak))
  2)

(deftest mop-generic-function-lambda-list-arity
  (length (dotcl-mop:generic-function-lambda-list #'mop-speak))
  1)

;;; --- method introspection ---

(deftest mop-method-generic-function
  (let* ((gf #'mop-speak)
         (m  (first (dotcl-mop:generic-function-methods gf))))
    (eq gf (dotcl-mop:method-generic-function m)))
  t)

(deftest mop-method-specializers-not-empty
  (let* ((m (first (dotcl-mop:generic-function-methods #'mop-speak))))
    (notnot (dotcl-mop:method-specializers m)))
  t)

;;; --- utility functions ---

(deftest mop-classp-true
  (dotcl-mop:classp (find-class 'mop-dog))
  t)

(deftest mop-classp-false
  (dotcl-mop:classp 42)
  nil)

(deftest mop-subclassp-true
  (dotcl-mop:subclassp (find-class 'mop-dog) (find-class 'mop-animal))
  t)

(deftest mop-subclassp-false
  (dotcl-mop:subclassp (find-class 'mop-animal) (find-class 'mop-dog))
  nil)

(deftest mop-validate-superclass-returns-t
  (dotcl-mop:validate-superclass (find-class 'mop-dog) (find-class 'mop-animal))
  t)

;;; --- class-default-initargs ---

(defclass mop-with-defaults ()
  ((x :initarg :x :initform 10))
  (:default-initargs :x 99))

(deftest mop-class-default-initargs-not-empty
  (notnot (dotcl-mop:class-default-initargs (find-class 'mop-with-defaults)))
  t)

;;; --- class-prototype ---

(deftest mop-class-prototype-type
  (typep (dotcl-mop:class-prototype (find-class 'mop-dog)) 'mop-dog)
  t)

;;; class-prototype must return the SAME instance every call (AMOP). McCLIM's
;;; define-presentation-method dispatches via (eql (class-prototype <ptype-class>)),
;;; which only works if the prototype identity is stable across method definition
;;; and call. A fresh instance each call made every presentation method fall through
;;; to its dumb default (e.g. presentation-subtypep on `command' with mixed
;;; symbol/object :command-table params wrongly returned (nil nil)).
(deftest mop-class-prototype-stable
  (eq (dotcl-mop:class-prototype (find-class 'mop-dog))
      (dotcl-mop:class-prototype (find-class 'mop-dog)))
  t)

;;; The behaviour McCLIM relies on: an (eql class-prototype) method actually
;;; dispatches when called with that prototype.
(defgeneric proto-dispatch (x))
(defmethod proto-dispatch ((x t)) :default)
(eval-when (:load-toplevel :execute)
  (let ((proto (dotcl-mop:class-prototype (find-class 'mop-dog))))
    (eval `(defmethod proto-dispatch ((x (eql ,proto))) :prototype))))

(deftest mop-class-prototype-eql-dispatch
  (proto-dispatch (dotcl-mop:class-prototype (find-class 'mop-dog)))
  :prototype)

;;; --- eql-specializer ---

(deftest mop-intern-eql-specializer
  (let ((spec (dotcl-mop:intern-eql-specializer 42)))
    (dotcl-mop:eql-specializer-object spec))
  42)

;;; --- slot-value-using-class dispatch (AMOP §5.4) ---

(defclass svuc-meta (standard-class) ())
(defmethod validate-superclass ((c svuc-meta) (s standard-class)) t)

(let ((svuc-log nil))
  (defmethod slot-value-using-class ((c svuc-meta) obj slotd)
    (push :read svuc-log)
    (call-next-method))
  (defmethod (setf slot-value-using-class) (v (c svuc-meta) obj slotd)
    (push :write svuc-log)
    (call-next-method))
  (defclass svuc-obj ()
    ((x :initarg :x :accessor svuc-x))
    (:metaclass svuc-meta))

  ;; Clear the log AFTER make-instance: per AMOP, shared-initialize sets slots
  ;; from initargs via (setf slot-value-using-class), so construction itself
  ;; dispatches a :write (required for McCLIM dynamic slots). These tests
  ;; isolate the read / write dispatch of the operation under test.
  (deftest mop-svuc-read-dispatch
    (let ((o (make-instance 'svuc-obj :x 1)))
      (setq svuc-log nil)
      (svuc-x o)
      (member :read svuc-log))
    (:read))

  (deftest mop-svuc-write-dispatch
    (let ((o (make-instance 'svuc-obj :x 1)))
      (setq svuc-log nil)
      (setf (svuc-x o) 2)
      (member :write svuc-log))
    (:write))

  ;; Construction dispatches (setf slot-value-using-class) for the initarg slot.
  (deftest mop-svuc-init-dispatch
    (progn (setq svuc-log nil)
           (make-instance 'svuc-obj :x 1)
           (member :write svuc-log))
    (:write)))

;;; --- setf GF cross-package bleed guard ---
;;; A (setf pkg:documentation) GF with a different arity must NOT mutate
;;; the existing (setf cl:documentation) GF lambda-list.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "DOCBLEED-TEST")
    (make-package "DOCBLEED-TEST" :use '())))

(deftest mop-setf-gf-no-cross-package-bleed
  (let* ((before (length (generic-function-lambda-list #'(setf documentation))))
         (other-doc (intern "DOCUMENTATION" (find-package "DOCBLEED-TEST"))))
    ;; Define a 4-arg (setf docbleed-test:documentation) GF - must not affect CL's
    (ensure-generic-function (list 'setf other-doc)
                             :lambda-list '(new-value name doc-type language))
    (length (generic-function-lambda-list #'(setf documentation))))
  3)

;;; standard GF stores its lambda-list; MOP readers return the
;;; actual parameter names, not gensym placeholders (#:R0 ...).
(defgeneric mop-llnames (alpha beta)
  (:argument-precedence-order beta alpha))
(defmethod mop-llnames ((alpha t) (beta t)) nil)

(deftest d1110-gf-lambda-list-real-names
  (generic-function-lambda-list #'mop-llnames)
  (alpha beta))

(deftest d1110-gf-apo-real-names
  (generic-function-argument-precedence-order #'mop-llnames)
  (beta alpha))

(defgeneric mop-ll-opt (x y &optional z))
(defmethod mop-ll-opt ((x t) (y t) &optional z) z)

(deftest d1110-gf-lambda-list-with-optional
  (generic-function-lambda-list #'mop-ll-opt)
  (x y &optional z))

;;; --- custom metaclass identity: defclass and ensure-class ---
;; ensure-class must honor :metaclass (it used to ignore it), and a class
;; defined with a custom metaclass must satisfy TYPEP against that metaclass
;; consistently with CLASS-OF.
(defclass mop-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mop-meta) (s standard-class)) t)

(defclass mop-with-meta () () (:metaclass mop-meta))

(deftest mop-defclass-metaclass-class-of
  (eq (class-of (find-class 'mop-with-meta)) (find-class 'mop-meta))
  t)

(deftest mop-defclass-metaclass-typep
  (notnot (typep (find-class 'mop-with-meta) 'mop-meta))
  t)

(dotcl-mop:ensure-class 'mop-ec-meta
                        :metaclass 'mop-meta
                        :direct-superclasses (list (find-class 'standard-object)))

(deftest mop-ensure-class-honors-metaclass-class-of
  (eq (class-of (find-class 'mop-ec-meta)) (find-class 'mop-meta))
  t)

(deftest mop-ensure-class-honors-metaclass-typep
  (notnot (typep (find-class 'mop-ec-meta) 'mop-meta))
  t)

(deftest mop-ensure-class-superclass
  (notnot (member (find-class 'standard-object)
                  (dotcl-mop:class-precedence-list (find-class 'mop-ec-meta))))
  t)

;;; --- metaclass-added slots on the class metaobject ---
;; A metaclass that subclasses standard-class AND adds a slot: the classes it
;; creates must hold that slot on their class metaobject (slot-value works).
(defclass mop-meta-mixin () ((tag :initarg :tag :initform :none)))
(defclass mop-meta-slotted (mop-meta-mixin standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mop-meta-slotted) (s standard-class)) t)
(defclass mop-foo-slotted () () (:metaclass mop-meta-slotted))

(deftest mop-metaclass-slot-initform
  (slot-value (find-class 'mop-foo-slotted) 'tag)
  :none)

(deftest mop-metaclass-slot-setf-boundp
  (let ((c (find-class 'mop-foo-slotted)))
    (setf (slot-value c 'tag) :hi)
    (list (slot-value c 'tag) (notnot (slot-boundp c 'tag))))
  (:hi t))

;;; A class metaobject runs the metaclass's inherited initialize-instance
;;; :after — a slot computed by :after (no initform/initarg) is bound on the class.
(defclass mop-meta-mixin2 () ((computed :accessor mop-computed)))
(defmethod initialize-instance :after ((o mop-meta-mixin2) &key)
  (unless (slot-boundp o 'computed) (setf (slot-value o 'computed) :by-after)))
(defclass mop-meta-after (mop-meta-mixin2 standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mop-meta-after) (s standard-class)) t)
(defclass mop-foo-after () () (:metaclass mop-meta-after))

(deftest mop-metaclass-initialize-instance-after
  (slot-value (find-class 'mop-foo-after) 'computed)
  :by-after)

;;; re-ensure-class an existing class under a different metaclass switches the
;;; metaclass and applies metaclass-slot initargs (McCLIM forward-ref -> real class).
(defclass mop-meta-tn () ((type-name :initarg :type-name :accessor mop-type-name)))
(defclass mop-meta-switch (mop-meta-tn standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mop-meta-switch) (s standard-class)) t)
(dotcl-mop:ensure-class 'mop-reensure :direct-superclasses (list (find-class 'standard-object)))
(dotcl-mop:ensure-class 'mop-reensure :metaclass 'mop-meta-switch :type-name 'mop-reensure
                        :direct-superclasses (list (find-class 'standard-object)))

(deftest mop-reensure-metaclass-switch
  (notnot (typep (find-class 'mop-reensure) 'mop-meta-switch))
  t)
(deftest mop-reensure-initarg-applied
  (slot-value (find-class 'mop-reensure) 'type-name)
  mop-reensure)

;;; On a class metaobject built via ensure-class, initialize-instance :after
;;; runs AFTER the metaclass-slot initargs are applied (ordinary instance order), so an
;;; :after that reads an initarg-filled slot sees it BOUND (used to be UNBOUND because
;;; the :after fired during class creation before the initargs were applied).
(defclass mop-meta-tn2 ()
  ((type-name :initarg :type-name :accessor mop-type-name2)
   (spec :accessor mop-spec2)))
(defmethod initialize-instance :after ((o mop-meta-tn2) &key)
  (unless (slot-boundp o 'spec)
    (setf (slot-value o 'spec) (list :derived-from (slot-value o 'type-name)))))
(defclass mop-meta-order (mop-meta-tn2 standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mop-meta-order) (s standard-class)) t)
(dotcl-mop:ensure-class 'mop-ec-order :metaclass 'mop-meta-order :type-name 'mop-ot
                        :direct-superclasses (list (find-class 'standard-object)))

(deftest mop-ec-after-sees-initarg
  (slot-value (find-class 'mop-ec-order) 'spec)
  (:derived-from mop-ot))

;;; --- newly added AMOP protocol functions ---
;;; These were interned/exported but UNDEFINED, breaking closer-mop callers.

(deftest mop-compute-slots
  (length (dotcl-mop:compute-slots (find-class 'mop-dog)))
  3)  ; name age breed

(deftest mop-compute-class-precedence-list
  (mapcar #'class-name
          (dotcl-mop:compute-class-precedence-list (find-class 'mop-dog)))
  (mop-dog mop-animal standard-object t))

(deftest mop-compute-default-initargs
  (dotcl-mop:compute-default-initargs (find-class 'mop-dog))
  nil)

(deftest mop-method-function-is-function
  (functionp (dotcl-mop:method-function
              (first (dotcl-mop:generic-function-methods #'mop-speak))))
  t)

(deftest mop-reader-method-class
  (class-name (dotcl-mop:reader-method-class (find-class 'mop-dog) nil))
  standard-method)

(deftest mop-writer-method-class
  (class-name (dotcl-mop:writer-method-class (find-class 'mop-dog) nil))
  standard-method)

;;; specializer-direct-methods: every method specialized on mop-animal.
;;; mop-speak has one (the (mop-animal) method); accessor mop-animal-name adds a
;;; reader + writer = 3 total on the mop-animal class specializer.
(deftest mop-specializer-direct-methods-count
  (>= (length (dotcl-mop:specializer-direct-methods (find-class 'mop-animal))) 1)
  t)

(deftest mop-specializer-direct-generic-functions-has-speak
  (if (member #'mop-speak
              (dotcl-mop:specializer-direct-generic-functions (find-class 'mop-animal)))
      t nil)
  t)

;;; compute-applicable-methods-using-classes + compute-discriminating-function
(defgeneric mop-camuc (a))
(defmethod mop-camuc ((a mop-animal)) 'a)
(defmethod mop-camuc ((a mop-dog)) 'd)
(defmethod mop-camuc ((a (eql 99))) 'e)

(deftest mop-camuc-dog-definitive
  ;; (eql 99) cannot apply to a dog, so the class-only result is definitive.
  (multiple-value-bind (methods def)
      (dotcl-mop:compute-applicable-methods-using-classes #'mop-camuc (list (find-class 'mop-dog)))
    (list (length methods) (not (null def))))
  (2 t))

(deftest mop-camuc-integer-nondefinitive
  ;; 99 is an integer, so for class INTEGER the (eql 99) method might apply.
  (multiple-value-bind (methods def)
      (dotcl-mop:compute-applicable-methods-using-classes #'mop-camuc (list (find-class 'integer)))
    (list (length methods) (null def)))
  (1 t))

(deftest mop-compute-discriminating-function-callable
  (functionp (dotcl-mop:compute-discriminating-function #'mop-camuc))
  t)

;;; ensure-class-using-class / ensure-generic-function-using-class
(deftest mop-ensure-gf-using-class-creates
  (typep (dotcl-mop:ensure-generic-function-using-class nil 'mop-egfuc-new)
         'generic-function)
  t)

(deftest mop-ensure-class-using-class-creates
  (class-name (dotcl-mop:ensure-class-using-class nil 'mop-ecuc-new
                :direct-superclasses (list (find-class 'standard-object))))
  mop-ecuc-new)

(defclass mop-ecuc-existing () ((a :initarg :a)))
(deftest mop-ensure-class-using-class-reensures
  (eq (dotcl-mop:ensure-class-using-class (find-class 'mop-ecuc-existing) 'mop-ecuc-existing
        :direct-superclasses (list (find-class 'standard-object)))
      (find-class 'mop-ecuc-existing))
  t)

;;; compute-effective-method (standard combination)
(defgeneric mop-cem (x))
(defmethod mop-cem ((x mop-animal)) 'p)
(defmethod mop-cem :before ((x mop-animal)) 'b)
(defmethod mop-cem :after ((x mop-animal)) 'a)

(deftest mop-compute-effective-method-primary-only
  (let* ((ms (dotcl-mop:compute-applicable-methods-using-classes #'mop-speak
               (list (find-class 'mop-animal))))
         (em (dotcl-mop:compute-effective-method #'mop-speak 'standard ms)))
    (car em))
  call-method)

(deftest mop-compute-effective-method-before-after
  ;; with :before/:after the form wraps the primary call (progn / multiple-value-prog1)
  (let* ((ms (dotcl-mop:compute-applicable-methods-using-classes #'mop-cem
               (list (find-class 'mop-animal))))
         (em (dotcl-mop:compute-effective-method #'mop-cem 'standard ms)))
    (and (consp em) (symbolp (car em)) t))
  t)

;;; accessor-method-slot-definition: DEFCLASS tags reader/writer/accessor
;;; methods with their slot-definition; ordinary methods return NIL.
(defclass mop-ams () ((a :accessor mop-ams-a) (b :reader mop-ams-b)))
(defgeneric mop-ams-plain (x))
(defmethod mop-ams-plain ((x mop-ams)) 0)

(deftest mop-accessor-method-slot-definition-reader
  (slot-definition-name
   (dotcl-mop:accessor-method-slot-definition
    (first (dotcl-mop:generic-function-methods #'mop-ams-a))))
  a)

(deftest mop-accessor-method-slot-definition-setf
  (slot-definition-name
   (dotcl-mop:accessor-method-slot-definition
    (first (dotcl-mop:generic-function-methods #'(setf mop-ams-a)))))
  a)

(deftest mop-accessor-method-slot-definition-ordinary-nil
  (dotcl-mop:accessor-method-slot-definition
   (first (dotcl-mop:generic-function-methods #'mop-ams-plain)))
  nil)

;;; standard-instance-access / funcallable-standard-instance-access: read and
;;; write a slot by its layout index, bypassing slot-value-using-class. On dotcl
;;; an instance of a funcallable-standard-class class is an ordinary instance
;;; with the same slot vector, so the two accessors share one implementation.

(defclass mop-sia () ((a :initform 11) (b :initform 22)))
(defclass mop-fsia () ((a :initform 11) (b :initform 22))
  (:metaclass dotcl-mop:funcallable-standard-class))

(defun mop-slot-location (class-name slot-name)
  (let ((class (find-class class-name)))
    (unless (dotcl-mop:class-finalized-p class)
      (dotcl-mop:finalize-inheritance class))
    (dotcl-mop:slot-definition-location
     (find slot-name (dotcl-mop:class-slots class)
           :key #'dotcl-mop:slot-definition-name))))

(deftest mop-standard-instance-access
  (dotcl-mop:standard-instance-access (make-instance 'mop-sia)
                                      (mop-slot-location 'mop-sia 'b))
  22)

(deftest mop-standard-instance-access-setf
  (let ((object (make-instance 'mop-sia))
        (location (mop-slot-location 'mop-sia 'a)))
    (setf (dotcl-mop:standard-instance-access object location) 7)
    (list (dotcl-mop:standard-instance-access object location)
          (slot-value object 'a)))
  (7 7))

(deftest mop-funcallable-standard-instance-access
  (dotcl-mop:funcallable-standard-instance-access
   (make-instance 'mop-fsia) (mop-slot-location 'mop-fsia 'b))
  22)

(deftest mop-funcallable-standard-instance-access-setf
  (let ((object (make-instance 'mop-fsia))
        (location (mop-slot-location 'mop-fsia 'a)))
    (setf (dotcl-mop:funcallable-standard-instance-access object location) 7)
    (list (dotcl-mop:funcallable-standard-instance-access object location)
          (slot-value object 'a)))
  (7 7))

(deftest mop-funcallable-standard-instance-access-not-an-instance
  (notnot (nth-value 1 (ignore-errors
                        (dotcl-mop:funcallable-standard-instance-access 42 0))))
  t)

(deftest mop-funcallable-standard-instance-access-out-of-range
  (notnot (nth-value 1 (ignore-errors
                        (dotcl-mop:funcallable-standard-instance-access
                         (make-instance 'mop-fsia) 99))))
  t)

;;; generic-function-declarations: AMOP passes declarations with the
;;; :declarations initarg, ANSI defgeneric spells the same thing (declare ...),
;;; and both are read back here.

(defgeneric mop-gfd (x)
  (:documentation "declarations fixture")
  (declare (optimize (speed 3))))

(defgeneric mop-gfd-none (x))

(deftest mop-generic-function-declarations-defgeneric
  (dotcl-mop:generic-function-declarations #'mop-gfd)
  ((optimize (speed 3))))

(deftest mop-generic-function-declarations-initarg
  (dotcl-mop:generic-function-declarations
   (make-instance 'standard-generic-function
                  :lambda-list '(x)
                  :declarations '((optimize (speed 3)))))
  ((optimize (speed 3))))

(deftest mop-generic-function-declarations-default-nil
  (dotcl-mop:generic-function-declarations #'mop-gfd-none)
  nil)

;;; find-method-combination: AMOP hands back a method combination metaobject.
;;; dotcl decides the combination from a symbol plus its arguments, so the
;;; object records those two; portable code tests its type and passes it on.

(defgeneric mop-fmc (x))

(deftest mop-find-method-combination-typep
  (notnot (typep (dotcl-mop:find-method-combination #'mop-fmc 'standard '())
                 'method-combination))
  t)

(deftest mop-find-method-combination-type-of
  (type-of (dotcl-mop:find-method-combination #'mop-fmc 'standard '()))
  method-combination)

(deftest mop-find-method-combination-class-of
  (eq (find-class 'method-combination)
      (class-of (dotcl-mop:find-method-combination #'mop-fmc 'progn '())))
  t)

(deftest mop-find-method-combination-is-generic
  (notnot (typep #'dotcl-mop:find-method-combination 'generic-function))
  t)

;;; add-direct-method / remove-direct-method: AMOP has add-method and
;;; remove-method call these once per specializer. dotcl keeps no specializer to
;;; method back-link (specializer-direct-methods scans), so the default methods
;;; have nothing to record; what the protocol buys is the call, which a portable
;;; metaobject class can specialize to keep its own registry.

(defvar *mop-direct-seen* nil)

(defclass mop-direct-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mop-direct-meta) (s standard-class)) t)
(defmethod dotcl-mop:add-direct-method ((spec mop-direct-meta) method)
  (push :add *mop-direct-seen*)
  (call-next-method))
(defmethod dotcl-mop:remove-direct-method ((spec mop-direct-meta) method)
  (push :remove *mop-direct-seen*)
  (call-next-method))

(defclass mop-direct-thing () () (:metaclass mop-direct-meta))
(defgeneric mop-direct-touch (x))

(deftest mop-add-method-calls-add-direct-method
  (let ((*mop-direct-seen* nil))
    (eval '(defmethod mop-direct-touch ((x mop-direct-thing)) x))
    *mop-direct-seen*)
  (:add))

(deftest mop-remove-method-calls-remove-direct-method
  (let ((*mop-direct-seen* nil))
    (let ((method (find-method #'mop-direct-touch '()
                               (list (find-class 'mop-direct-thing)) nil)))
      (when method (remove-method #'mop-direct-touch method)))
    *mop-direct-seen*)
  (:remove))

(deftest mop-direct-method-protocol-is-generic
  (list (notnot (typep #'dotcl-mop:add-direct-method 'generic-function))
        (notnot (typep #'dotcl-mop:remove-direct-method 'generic-function)))
  (t t))

;;; add-direct-subclass / remove-direct-subclass: same shape as the direct-method
;;; pair. class-direct-subclasses is derived by scanning the class registry, so
;;; the default methods have nothing to record; the call is what a metaobject
;;; class specializes. A redefinition that drops a superclass reports the drop.

(defvar *mop-subclass-seen* nil)

(defclass mop-sub-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mop-sub-meta) (s standard-class)) t)
(defmethod dotcl-mop:add-direct-subclass ((super mop-sub-meta) sub)
  (push (list :add (class-name super)) *mop-subclass-seen*)
  (call-next-method))
(defmethod dotcl-mop:remove-direct-subclass ((super mop-sub-meta) sub)
  (push (list :remove (class-name super)) *mop-subclass-seen*)
  (call-next-method))

(defclass mop-sub-base-a () () (:metaclass mop-sub-meta))
(defclass mop-sub-base-b () () (:metaclass mop-sub-meta))

(deftest mop-defclass-calls-add-direct-subclass
  (let ((*mop-subclass-seen* nil))
    (eval '(defclass mop-sub-child (mop-sub-base-a) () (:metaclass mop-sub-meta)))
    (reverse *mop-subclass-seen*))
  ((:add mop-sub-base-a)))

(deftest mop-redefinition-reports-dropped-superclass
  (let ((*mop-subclass-seen* nil))
    (eval '(defclass mop-sub-child (mop-sub-base-b) () (:metaclass mop-sub-meta)))
    (reverse *mop-subclass-seen*))
  ((:remove mop-sub-base-a) (:add mop-sub-base-b)))

(deftest mop-direct-subclass-protocol-is-generic
  (list (notnot (typep #'dotcl-mop:add-direct-subclass 'generic-function))
        (notnot (typep #'dotcl-mop:remove-direct-subclass 'generic-function)))
  (t t))

;;; compute-effective-method-function: closer-mop's extension, not AMOP. It hands
;;; back a function of the generic function's arguments that runs an effective
;;; method form. The standard combination's form nests a (make-method ...) in the
;;; around chain, so the function has to reach call-next-method through it.

(defgeneric mop-cemf (x))
(defmethod mop-cemf ((x integer)) (* 2 x))
(defmethod mop-cemf :around ((x integer)) (1+ (call-next-method)))

(defun mop-cemf-function ()
  (let* ((methods (dotcl-mop:compute-applicable-methods-using-classes
                   #'mop-cemf (list (find-class 'integer))))
         (form (dotcl-mop:compute-effective-method #'mop-cemf 'standard methods)))
    (dotcl-mop:compute-effective-method-function #'mop-cemf form '())))

(deftest mop-compute-effective-method-function-is-a-function
  (notnot (functionp (mop-cemf-function)))
  t)

(deftest mop-compute-effective-method-function-agrees-with-dispatch
  (let ((result (funcall (mop-cemf-function) 5)))
    (list result (eql result (mop-cemf 5))))
  (11 t))

(deftest mop-compute-effective-method-function-rejects-options
  (notnot (nth-value 1 (ignore-errors
                        (dotcl-mop:compute-effective-method-function
                         #'mop-cemf nil '((:arguments))))))
  t)

;;; set-funcallable-instance-function: what a funcallable instance does when
;;; called. On dotcl the funcallable instances are generic functions, so this
;;; installs the function the generic function calls, which is what a
;;; discriminating function is. Dispatch has arity-specialised fast paths, so the
;;; installed function has to be reached through those too.

(defgeneric mop-sfif (x))
(defmethod mop-sfif ((x integer)) (list :method x))

(defgeneric mop-sfif-2 (x y))
(defmethod mop-sfif-2 ((x integer) (y integer)) :method)

(deftest mop-set-funcallable-instance-function-replaces-dispatch
  (let ((before (mop-sfif 1)))
    (dotcl-mop:set-funcallable-instance-function
     #'mop-sfif (lambda (&rest args) (cons :discriminating args)))
    (list before (mop-sfif 1)))
  ((:method 1) (:discriminating 1)))

(deftest mop-set-funcallable-instance-function-reaches-arity-fast-path
  (progn
    (dotcl-mop:set-funcallable-instance-function
     #'mop-sfif-2 (lambda (a b) (list :discriminating a b)))
    (mop-sfif-2 3 4))
  (:discriminating 3 4))

(deftest mop-set-funcallable-instance-function-keeps-closure-state
  (let ((calls 0))
    (dotcl-mop:set-funcallable-instance-function
     #'mop-sfif (lambda (&rest args) (declare (ignore args)) (incf calls)))
    (mop-sfif 1)
    (list (mop-sfif 1) calls))
  (2 2))

(deftest mop-set-funcallable-instance-function-rejects-plain-instance
  (notnot (nth-value 1 (ignore-errors
                        (dotcl-mop:set-funcallable-instance-function
                         (make-instance 'standard-object) #'identity))))
  t)

;;; compute-discriminating-function: AMOP has the generic function call whatever
;;; this returns. The default method hands back dotcl's own dispatch as a function
;;; object, so a user method can wrap it with call-next-method. The result is
;;; installed on the generic function; plain generic functions get nothing
;;; installed and keep the arity fast paths.

(defclass mop-counting-gf (standard-generic-function) ()
  (:metaclass dotcl-mop:funcallable-standard-class))

(defvar *mop-discriminating-calls* 0)

(defmethod dotcl-mop:compute-discriminating-function ((gf mop-counting-gf))
  (let ((standard (call-next-method)))
    (lambda (&rest args)
      (incf *mop-discriminating-calls*)
      (apply standard args))))

(defgeneric mop-counted (x) (:generic-function-class mop-counting-gf))
(defmethod mop-counted ((x integer)) (* 10 x))

(deftest mop-discriminating-function-is-installed
  (let ((*mop-discriminating-calls* 0))
    (list (mop-counted 4) *mop-discriminating-calls*))
  (40 1))

(deftest mop-discriminating-function-runs-every-call
  (let ((*mop-discriminating-calls* 0))
    (mop-counted 1)
    (mop-counted 2)
    *mop-discriminating-calls*)
  2)

(defgeneric mop-plain-dispatch (x))
(defmethod mop-plain-dispatch ((x integer)) (* 3 x))

(deftest mop-plain-generic-function-is-untouched
  (mop-plain-dispatch 7)
  21)

(deftest mop-compute-discriminating-function-default-is-a-function
  (notnot (functionp (dotcl-mop:compute-discriminating-function #'mop-plain-dispatch)))
  t)

;;; The AMOP protocol functions have to be generic functions, or they cannot be
;;; specialised at all. closer-mop asks this directly: only-standard-methods
;;; calls generic-function-methods on each of them to decide whether a host has
;;; been customised, and a plain function makes that signal.

(deftest mop-protocol-functions-are-generic
  (mapcar (lambda (name)
            (let ((fn (fdefinition (find-symbol (symbol-name name) '#:dotcl-mop))))
              (notnot (typep fn 'generic-function))))
          '(#:compute-slots #:compute-applicable-methods-using-classes
            #:generic-function-method-class #:compute-effective-method
            #:make-method-lambda #:compute-discriminating-function
            #:compute-effective-slot-definition #:validate-superclass))
  (t t t t t t t t))

;;; make-method-lambda also had a flat registration on the DOTCL-INTERNAL symbol
;;; of the same name. One name, one behaviour: both spellings are the same object.

(deftest mop-make-method-lambda-one-behaviour
  (eq (fdefinition (find-symbol "MAKE-METHOD-LAMBDA" '#:dotcl-mop))
      (fdefinition (find-symbol "MAKE-METHOD-LAMBDA" '#:dotcl-internal)))
  t)

;;; Specialising one of them reaches the default through call-next-method.

(defclass mop-slots-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mop-slots-meta) (s standard-class)) t)
(defvar *mop-compute-slots-seen* nil)
(defmethod dotcl-mop:compute-slots ((class mop-slots-meta))
  (setf *mop-compute-slots-seen* t)
  (call-next-method))

(deftest mop-compute-slots-is-specialisable
  (let ((*mop-compute-slots-seen* nil))
    (eval '(defclass mop-slots-probe () ((a)) (:metaclass mop-slots-meta)))
    (let ((slots (dotcl-mop:compute-slots (find-class 'mop-slots-probe))))
      (list *mop-compute-slots-seen*
            (mapcar #'dotcl-mop:slot-definition-name slots))))
  (t (a)))

;;; A generic function class may add slots. The object is callable, so it cannot
;;; also be a LispInstance with a slot vector; the slots live in the same escape
;;; hatch class metaobjects use. Before this the class reported slots its
;;; instances could not hold, which is what closer-mop's own generic function
;;; class ran into.

(defclass mop-slotted-gf (standard-generic-function)
  ((tag :initarg :tag :accessor mop-gf-tag :initform :none))
  (:metaclass dotcl-mop:funcallable-standard-class))

(deftest mop-generic-function-slot-from-initarg
  (slot-value (make-instance 'mop-slotted-gf :lambda-list '(x) :tag :hello) 'tag)
  :hello)

(deftest mop-generic-function-slot-from-initform
  (slot-value (make-instance 'mop-slotted-gf :lambda-list '(x)) 'tag)
  :none)

(deftest mop-generic-function-slot-setf
  (let ((gf (make-instance 'mop-slotted-gf :lambda-list '(x))))
    (setf (slot-value gf 'tag) :written)
    (list (slot-value gf 'tag) (mop-gf-tag gf)))
  (:written :written))

(deftest mop-generic-function-slot-stays-callable
  (let ((gf (make-instance 'mop-slotted-gf :lambda-list '(x) :tag :t)))
    (list (notnot (functionp gf))
          (notnot (typep gf 'standard-generic-function))
          (slot-value gf 'tag)))
  (t t :t))

;;; (setf generic-function-name) is defined by AMOP as reinitialization, not a
;;; field write, so a reinitialize-instance method sees it. Reinitialising a
;;; generic function applies the same initargs initialization does.

(defgeneric mop-rename-me (x))
(defmethod mop-rename-me ((x integer)) :original)

(deftest mop-setf-generic-function-name
  (let ((gf #'mop-rename-me))
    (setf (dotcl-mop:generic-function-name gf) 'mop-renamed)
    (list (dotcl-mop:generic-function-name gf)
          (mop-renamed 1)))
  (mop-renamed :original))

(defclass mop-reinit-gf (standard-generic-function)
  ((seen :initform nil :accessor mop-reinit-seen))
  (:metaclass dotcl-mop:funcallable-standard-class))

(defmethod reinitialize-instance :after ((gf mop-reinit-gf) &rest initargs)
  (declare (ignore initargs))
  (setf (mop-reinit-seen gf) t))

(deftest mop-setf-generic-function-name-goes-through-reinitialize
  (let ((gf (make-instance 'mop-reinit-gf :lambda-list '(x))))
    (setf (dotcl-mop:generic-function-name gf) 'mop-reinit-named)
    (list (mop-reinit-seen gf) (dotcl-mop:generic-function-name gf)))
  (t mop-reinit-named))

(deftest mop-reinitialize-generic-function-lambda-list
  (let ((gf (make-instance 'standard-generic-function :lambda-list '(a b))))
    (reinitialize-instance gf :lambda-list '(x y z))
    (dotcl-mop:generic-function-argument-precedence-order gf))
  (x y z))

;;; :allocation other than :instance / :class. CLHS 7.1.2 allows only those two,
;;; but that rule is about standard-class: AMOP has the metaclass decide what an
;;; allocation means, through effective-slot-definition-class and
;;; slot-value-using-class. Under a custom metaclass dotcl now carries the keyword
;;; instead of rejecting it, and the slot is kept out of the instance vector.

(defclass mop-alloc-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mop-alloc-meta) (s standard-class)) t)

(defclass mop-alloc-slotd (dotcl-mop:standard-effective-slot-definition)
  ((store :initform (make-hash-table :test 'eq) :reader mop-alloc-store)))

(defmethod dotcl-mop:effective-slot-definition-class ((class mop-alloc-meta) &rest initargs)
  (if (eq :elsewhere (getf initargs :allocation))
      (find-class 'mop-alloc-slotd)
      (call-next-method)))

(defmethod dotcl-mop:slot-value-using-class ((class mop-alloc-meta) object
                                             (slotd mop-alloc-slotd))
  (gethash object (mop-alloc-store slotd)))
(defmethod (setf dotcl-mop:slot-value-using-class) (value (class mop-alloc-meta) object
                                                    (slotd mop-alloc-slotd))
  (setf (gethash object (mop-alloc-store slotd)) value))

(defclass mop-alloc-user () ((a :allocation :elsewhere)) (:metaclass mop-alloc-meta))

(deftest mop-extensible-allocation-slotd-class
  (notnot (typep (first (dotcl-mop:class-slots (find-class 'mop-alloc-user)))
                 'mop-alloc-slotd))
  t)

(deftest mop-extensible-allocation-keyword-survives
  (dotcl-mop:slot-definition-allocation
   (first (dotcl-mop:class-slots (find-class 'mop-alloc-user))))
  :elsewhere)

(deftest mop-extensible-allocation-not-in-the-instance-vector
  (dotcl-mop:slot-definition-location
   (first (dotcl-mop:class-slots (find-class 'mop-alloc-user))))
  nil)

(deftest mop-extensible-allocation-round-trip
  (let ((object (make-instance 'mop-alloc-user))
        (slotd (first (dotcl-mop:class-slots (find-class 'mop-alloc-user)))))
    (setf (slot-value object 'a) 5)
    (list (slot-value object 'a)
          (gethash object (mop-alloc-store slotd))))
  (5 5))

(deftest mop-standard-allocation-still-rejected-without-a-metaclass
  (notnot (nth-value 1 (ignore-errors
                        (eval '(defclass mop-alloc-plain () ((a :allocation :bogus)))))))
  t)

;;; AMOP: make-instance on a class metaobject class makes a class. The result is
;;; not registered under a name -- an anonymous class is reachable only through
;;; the object, which is the point of it. Naming is what ensure-class is for.

(deftest mop-anonymous-class-is-a-class
  (let ((class (make-instance 'standard-class
                              :direct-superclasses (list (find-class 'standard-object))
                              :direct-slots '((:name a :initargs (:a) :readers () :writers ())))))
    (list (notnot (typep class 'class)) (class-name class)))
  (t nil))

(deftest mop-anonymous-class-instantiable
  (let* ((class (make-instance 'standard-class
                               :direct-superclasses (list (find-class 'standard-object))
                               :direct-slots '((:name a :initargs (:a) :readers () :writers ()))))
         (object (progn (dotcl-mop:finalize-inheritance class)
                        (make-instance class :a 3))))
    (list (eq (class-of object) class) (slot-value object 'a)))
  (t 3))

(deftest mop-anonymous-classes-are-distinct
  (flet ((anon () (make-instance 'standard-class
                                 :direct-superclasses (list (find-class 'standard-object)))))
    (eq (anon) (anon)))
  nil)

(deftest mop-anonymous-class-slots
  (let ((class (make-instance 'standard-class
                              :direct-superclasses (list (find-class 'standard-object))
                              :direct-slots '((:name a :initargs (:a) :readers () :writers ())
                                              (:name b :initargs (:b) :readers () :writers ())))))
    (dotcl-mop:finalize-inheritance class)
    (mapcar #'dotcl-mop:slot-definition-name (dotcl-mop:class-slots class)))
  (a b))

;;; A class with :metaclass funcallable-standard-class makes instances that are
;;; callable. On dotcl the callable object that carries a class and slots is the
;;; generic function, so that is the representation -- but such an instance is not
;;; a generic function, and its class says so.

(defclass mop-fi ()
  ((a :initarg :a :initform 11 :accessor mop-fi-a))
  (:metaclass dotcl-mop:funcallable-standard-class))

(deftest mop-funcallable-instance-is-callable-with-slots
  (let ((object (make-instance 'mop-fi :a 7)))
    (dotcl-mop:set-funcallable-instance-function object (lambda (x) (* 3 x)))
    (list (notnot (functionp object))
          (slot-value object 'a)
          (funcall object 2)))
  (t 7 6))

(deftest mop-funcallable-instance-is-not-a-generic-function
  (let ((object (make-instance 'mop-fi)))
    (list (notnot (typep object 'mop-fi))
          (typep object 'standard-generic-function)
          (typep object 'generic-function)))
  (t nil nil))

(deftest mop-funcallable-instance-function-can-be-a-closure
  (let ((object (make-instance 'mop-fi))
        (calls 0))
    (dotcl-mop:set-funcallable-instance-function
     object (lambda () (incf calls)))
    (funcall object)
    (list (funcall object) calls))
  (2 2))

(deftest mop-funcallable-standard-instance-access
  (let* ((object (make-instance 'mop-fi :a 4))
         (class (find-class 'mop-fi))
         (location (dotcl-mop:slot-definition-location
                    (find 'a (dotcl-mop:class-slots class)
                          :key #'dotcl-mop:slot-definition-name))))
    (list (dotcl-mop:funcallable-standard-instance-access object location)
          (progn (setf (dotcl-mop:funcallable-standard-instance-access object location) 9)
                 (slot-value object 'a))))
  (4 9))

;;; AMOP: a method function is called with a list of the generic function's
;;; arguments and a list of the next methods. dotcl's own method functions take
;;; the arguments spread, which is what dispatch calls, so method-function hands
;;; out a view over that -- and a function passed with the :function initarg,
;;; which is AMOP-shaped already, is handed back as it was given.

(defgeneric mop-mf (x))
(defmethod mop-mf ((x integer)) (* 2 x))

(deftest mop-method-function-takes-processed-parameters
  (let ((method (first (dotcl-mop:generic-function-methods #'mop-mf))))
    (funcall (dotcl-mop:method-function method) (list 2) '()))
  4)

(deftest mop-method-function-is-the-same-object-each-time
  (let ((method (first (dotcl-mop:generic-function-methods #'mop-mf))))
    (eq (dotcl-mop:method-function method) (dotcl-mop:method-function method)))
  t)

(deftest mop-method-initialized-with-function-keeps-identity
  (let* ((function (lambda (args next-methods)
                     (declare (ignore next-methods))
                     (* 10 (first args))))
         (method (make-instance 'standard-method
                                :lambda-list '(x)
                                :specializers (list (find-class 'integer))
                                :qualifiers '()
                                :function function)))
    (eq function (dotcl-mop:method-function method)))
  t)

(deftest mop-dispatch-calls-a-user-method-function-the-amop-way
  (let* ((function (lambda (args next-methods)
                     (declare (ignore next-methods))
                     (* 10 (first args))))
         (method (make-instance 'standard-method
                                :lambda-list '(x)
                                :specializers (list (find-class 'integer))
                                :qualifiers '()
                                :function function)))
    (defgeneric mop-mf-user (x))
    (add-method #'mop-mf-user method)
    (mop-mf-user 3))
  30)

;;; A slot definition carries its :documentation. AMOP passes it to
;;; effective-slot-definition-class and DOCUMENTATION reads it back, and an
;;; effective slot definition is built rather than defined, so nothing would have
;;; registered it in the global documentation table.

(defclass mop-doc-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mop-doc-meta) (s standard-class)) t)
(defvar *mop-esd-initargs* nil)
(defmethod dotcl-mop:effective-slot-definition-class ((class mop-doc-meta) &rest initargs)
  (setf *mop-esd-initargs* (copy-list initargs))
  (call-next-method))

(defclass mop-doc-user () ((a :documentation "the doc")) (:metaclass mop-doc-meta))
(defclass mop-doc-plain () ((a :documentation "plain doc") (b)))

(deftest mop-slot-definition-documentation-direct
  (documentation (first (dotcl-mop:class-direct-slots (find-class 'mop-doc-plain))) t)
  "plain doc")

(deftest mop-slot-definition-documentation-effective
  (documentation (first (dotcl-mop:class-slots (find-class 'mop-doc-plain))) t)
  "plain doc")

(deftest mop-slot-without-documentation-has-none
  (documentation (second (dotcl-mop:class-slots (find-class 'mop-doc-plain))) t)
  nil)

(deftest mop-documentation-passed-to-effective-slot-definition-class
  (progn (dotcl-mop:finalize-inheritance (find-class 'mop-doc-user))
         (getf *mop-esd-initargs* :documentation))
  "the doc")

;;; AMOP's canonicalized default initarg is (name form function): the form has to
;;; be carried because a thunk cannot be turned back into it.

(defclass mop-di-base () ((a :initarg :a) (b :initarg :b)) (:default-initargs :a 1))
(defclass mop-di-child (mop-di-base) () (:default-initargs :b (+ 1 1)))

(deftest mop-class-direct-default-initargs-canonical
  (let ((entry (first (dotcl-mop:class-direct-default-initargs (find-class 'mop-di-base)))))
    (list (first entry) (second entry) (notnot (functionp (third entry)))
          (funcall (third entry))))
  (:a 1 t 1))

(deftest mop-class-default-initargs-inherited
  (sort (mapcar #'first (dotcl-mop:class-default-initargs (find-class 'mop-di-child)))
        #'string< :key #'symbol-name)
  (:a :b))

(deftest mop-default-initarg-form-is-the-source
  (let ((entry (find :b (dotcl-mop:class-default-initargs (find-class 'mop-di-child))
                     :key #'first)))
    (list (second entry) (funcall (third entry))))
  ((+ 1 1) 2))

(deftest mop-default-initargs-still-apply
  (let ((object (make-instance 'mop-di-child)))
    (list (slot-value object 'a) (slot-value object 'b)))
  (1 2))

;;; Class-side protocols: AMOP has class initialization ask the metaclass for the
;;; accessor method classes, finalization go through compute-default-initargs, and
;;; (setf class-name) be a reinitialization rather than a field write. dotcl does
;;; not use the accessor method class for anything -- it builds plain standard
;;; methods -- so what the protocol buys there is the call.

(defvar *mop-class-protocol-seen* nil)

(defclass mop-cp-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mop-cp-meta) (s standard-class)) t)
(defmethod dotcl-mop:reader-method-class ((c mop-cp-meta) slotd &rest initargs)
  (declare (ignore initargs))
  (push :reader *mop-class-protocol-seen*)
  (call-next-method))
(defmethod dotcl-mop:writer-method-class ((c mop-cp-meta) slotd &rest initargs)
  (declare (ignore initargs))
  (push :writer *mop-class-protocol-seen*)
  (call-next-method))
(defmethod dotcl-mop:compute-default-initargs ((c mop-cp-meta))
  (push :default-initargs *mop-class-protocol-seen*)
  (call-next-method))
(defmethod reinitialize-instance :after ((c mop-cp-meta) &rest initargs)
  (declare (ignore initargs))
  (push :reinitialize *mop-class-protocol-seen*))

(deftest mop-class-initialization-calls-the-accessor-method-classes
  (let ((*mop-class-protocol-seen* nil))
    (eval '(defclass mop-cp-probe () ((a :accessor mop-cp-a)) (:metaclass mop-cp-meta)))
    (dotcl-mop:finalize-inheritance (find-class 'mop-cp-probe))
    (list (notnot (member :reader *mop-class-protocol-seen*))
          (notnot (member :writer *mop-class-protocol-seen*))
          (notnot (member :default-initargs *mop-class-protocol-seen*))))
  (t t t))

(deftest mop-setf-class-name-calls-reinitialize-instance
  (let ((*mop-class-protocol-seen* nil))
    (eval '(defclass mop-cp-named () ((a)) (:metaclass mop-cp-meta)))
    (setf (class-name (find-class 'mop-cp-named)) 'mop-cp-renamed)
    (list (notnot (member :reinitialize *mop-class-protocol-seen*))
          (class-name (find-class 'mop-cp-named))))
  (t mop-cp-renamed))

(deftest mop-setf-class-name-nil-clears-the-proper-name
  (progn
    (eval '(defclass mop-cp-cleared () ((a))))
    (let ((class (find-class 'mop-cp-cleared)))
      (setf (class-name class) nil)
      (class-name class)))
  nil)

;;; The dependent protocol. add-/remove-/map-dependents keep and walk the list;
;;; update-dependent is what a user specialises, so its default method does
;;; nothing. The runtime tells the dependents after a metaobject is reinitialized
;;; and after a method is added or removed, and only when something is registered.

(defclass mop-listener () ((seen :initform nil :accessor mop-listener-seen)))
(defmethod dotcl-mop:update-dependent (metaobject (d mop-listener) &rest initargs)
  (push initargs (mop-listener-seen d)))

(defclass mop-watched () ((a)))
(defgeneric mop-watched-gf (x))

(deftest mop-dependent-protocol-for-classes
  (let ((listener (make-instance 'mop-listener))
        (class (find-class 'mop-watched)))
    (dotcl-mop:add-dependent class listener)
    (reinitialize-instance class :name 'mop-watched)
    (prog1 (first (mop-listener-seen listener))
      (dotcl-mop:remove-dependent class listener)))
  (:name mop-watched))

(deftest mop-dependent-protocol-for-generic-functions
  (let ((listener (make-instance 'mop-listener)))
    (dotcl-mop:add-dependent #'mop-watched-gf listener)
    (eval '(defmethod mop-watched-gf ((x integer)) x))
    (prog1 (notnot (mop-listener-seen listener))
      (dotcl-mop:remove-dependent #'mop-watched-gf listener)))
  t)

(deftest mop-map-dependents-walks-them
  (let ((listener (make-instance 'mop-listener))
        (class (find-class 'mop-watched))
        (walked nil))
    (dotcl-mop:add-dependent class listener)
    (dotcl-mop:map-dependents class (lambda (d) (push d walked)))
    (prog1 (eq (first walked) listener)
      (dotcl-mop:remove-dependent class listener)))
  t)

(deftest mop-removed-dependent-is-not-told
  (let ((listener (make-instance 'mop-listener))
        (class (find-class 'mop-watched)))
    (dotcl-mop:add-dependent class listener)
    (dotcl-mop:remove-dependent class listener)
    (reinitialize-instance class :name 'mop-watched)
    (mop-listener-seen listener))
  nil)

;;; Three class-side gaps AMOP names, unrelated to each other but each small.

;;; 1. Reinitializing a class finalizes it again, through the generic function.
(defvar *mop-refinalize-seen* nil)
(defclass mop-refinalize-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mop-refinalize-meta) (s standard-class)) t)
(defmethod dotcl-mop:finalize-inheritance ((c mop-refinalize-meta))
  (push :finalize *mop-refinalize-seen*)
  (call-next-method))
(defclass mop-refinalize () ((a)) (:metaclass mop-refinalize-meta))

(deftest mop-reinitialize-instance-calls-finalize-inheritance
  (progn
    (dotcl-mop:finalize-inheritance (find-class 'mop-refinalize))
    (let ((*mop-refinalize-seen* nil))
      (reinitialize-instance (find-class 'mop-refinalize)
                             :direct-slots '((:name b :initargs (:b) :readers () :writers ())))
      *mop-refinalize-seen*))
  (:finalize))

;;; 2. A funcallable-standard-class class defaults to funcallable-standard-object,
;;;    not standard-object: its instances are callable.
(defclass mop-fso () () (:metaclass dotcl-mop:funcallable-standard-class))

(deftest mop-default-superclass-for-funcallable-standard-class
  ;; Compared by name: the class is interned in dotcl's own package and DOTCL-MOP
  ;; exports a same-named symbol for it, so the two spellings are not EQ.
  (list (mapcar (lambda (c) (symbol-name (class-name c)))
                (dotcl-mop:class-direct-superclasses (find-class 'mop-fso)))
        (notnot (subtypep 'mop-fso 'dotcl-mop:funcallable-standard-object)))
  (("FUNCALLABLE-STANDARD-OBJECT") t))

;;; 3. A slot option given more than once reaches direct-slot-definition-class as a
;;;    list of the values; one occurrence stays the value itself.
(defvar *mop-dsd-initargs* nil)
(defclass mop-dsd-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mop-dsd-meta) (s standard-class)) t)
(defclass mop-dsd-slotd (dotcl-mop:standard-direct-slot-definition)
  ((extra :initarg :extra :initform nil)))
(defmethod dotcl-mop:direct-slot-definition-class ((c mop-dsd-meta) &rest initargs)
  (setf *mop-dsd-initargs* (copy-list initargs))
  (find-class 'mop-dsd-slotd))

(deftest mop-multiple-slot-options-passed-as-a-list
  (progn (eval '(defclass mop-dsd-twice () ((a :extra 1 :extra 2))
                  (:metaclass mop-dsd-meta)))
         (getf *mop-dsd-initargs* :extra))
  (1 2))

(deftest mop-single-slot-option-stays-a-value
  (progn (eval '(defclass mop-dsd-once () ((a :extra 1)) (:metaclass mop-dsd-meta)))
         (getf *mop-dsd-initargs* :extra))
  1)

;;; Which way an accessor goes is decided by where the class sits in the method's
;;; specializers -- a reader takes the object first, a writer takes the value
;;; first. The generic function's name does not say: a :writer slot option names
;;; one like any other function, and only an :accessor writer is (SETF x).

(defvar *mop-accessor-kind-seen* nil)
(defclass mop-ak-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mop-ak-meta) (s standard-class)) t)
(defmethod dotcl-mop:reader-method-class ((c mop-ak-meta) slotd &rest initargs)
  (declare (ignore initargs))
  (push :reader *mop-accessor-kind-seen*)
  (call-next-method))
(defmethod dotcl-mop:writer-method-class ((c mop-ak-meta) slotd &rest initargs)
  (declare (ignore initargs))
  (push :writer *mop-accessor-kind-seen*)
  (call-next-method))

(deftest mop-plain-writer-reaches-writer-method-class
  (let ((*mop-accessor-kind-seen* nil))
    (eval '(defclass mop-ak-w () ((a :writer mop-ak-set-a)) (:metaclass mop-ak-meta)))
    *mop-accessor-kind-seen*)
  (:writer))

(deftest mop-plain-reader-reaches-reader-method-class
  (let ((*mop-accessor-kind-seen* nil))
    (eval '(defclass mop-ak-r () ((a :reader mop-ak-get-a)) (:metaclass mop-ak-meta)))
    *mop-accessor-kind-seen*)
  (:reader))

(deftest mop-accessor-reaches-both
  (let ((*mop-accessor-kind-seen* nil))
    (eval '(defclass mop-ak-rw () ((a :accessor mop-ak-a)) (:metaclass mop-ak-meta)))
    (list (notnot (member :reader *mop-accessor-kind-seen*))
          (notnot (member :writer *mop-accessor-kind-seen*))))
  (t t))

;;; A generic function whose invocation protocol someone specialised is dispatched
;;; through it: compute-applicable-methods-using-classes (falling back to
;;; compute-applicable-methods when the class answer is not definitive), then
;;; compute-effective-method. The answer is cached per argument-class vector like
;;; every other dispatch, so the protocol runs on a miss, not on every call.
;;; Generic functions nobody specialised keep the ordinary path.

(defclass mop-ip-base () ())
(defclass mop-ip-mid (mop-ip-base) ())

(defvar *mop-ip-cam* 0)
(defvar *mop-ip-cem* 0)

(defclass mop-ip-gf (standard-generic-function) ()
  (:metaclass dotcl-mop:funcallable-standard-class))
(defmethod compute-applicable-methods ((gf mop-ip-gf) args)
  (incf *mop-ip-cam*)
  (call-next-method))
(defmethod dotcl-mop:compute-effective-method ((gf mop-ip-gf) combination methods)
  (incf *mop-ip-cem*)
  (call-next-method))

(defgeneric mop-ip-through (x) (:generic-function-class mop-ip-gf))
(defmethod mop-ip-through ((x mop-ip-base)) (list :base))
(defmethod mop-ip-through ((x mop-ip-mid)) (cons :mid (call-next-method)))

(defgeneric mop-ip-plain (x))
(defmethod mop-ip-plain ((x mop-ip-base)) (list :base))
(defmethod mop-ip-plain ((x mop-ip-mid)) (cons :mid (call-next-method)))

(deftest mop-invocation-through-the-protocol-agrees-with-dispatch
  (let ((object (make-instance 'mop-ip-mid)))
    (list (mop-ip-through object) (mop-ip-plain object)))
  ((:mid :base) (:mid :base)))

(deftest mop-invocation-protocol-is-consulted
  (let ((object (make-instance 'mop-ip-mid)))
    (mop-ip-through object)
    (let ((*mop-ip-cam* 0) (*mop-ip-cem* 0))
      (mop-ip-through object)
      ;; warm: the effective method for this argument class is cached
      (list (> (+ *mop-ip-cam* *mop-ip-cem*) -1) *mop-ip-cam* *mop-ip-cem*)))
  (t 0 0))

(deftest mop-invocation-protocol-runs-on-a-new-argument-class
  (let ((*mop-ip-cam* 0) (*mop-ip-cem* 0))
    (mop-ip-through (make-instance 'mop-ip-base))
    (notnot (plusp (+ *mop-ip-cam* *mop-ip-cem*))))
  t)

(deftest mop-uncustomised-generic-function-keeps-the-ordinary-path
  (let ((*mop-ip-cam* 0) (*mop-ip-cem* 0))
    (mop-ip-plain (make-instance 'mop-ip-mid))
    (list *mop-ip-cam* *mop-ip-cem*))
  (0 0))

;;; The emit side of the same protocol: defmethod asks the generic function what
;;; class its methods are, and defgeneric asks for the method combination
;;; metaobject named by its option. Both calls used to be skipped entirely. The
;;; class defmethod is told about becomes the method's class; a generic function
;;; nobody customised still gets STANDARD-METHOD.

(defclass mop-emit-method (standard-method) ())

(defvar *mop-emit-method-class-calls* 0)
(defvar *mop-emit-combination-seen* nil)

(defclass mop-emit-mc-gf (standard-generic-function) ()
  (:metaclass dotcl-mop:funcallable-standard-class))
(defmethod dotcl-mop:generic-function-method-class ((gf mop-emit-mc-gf))
  (incf *mop-emit-method-class-calls*)
  (find-class 'mop-emit-method))

(defclass mop-emit-fmc-gf (standard-generic-function) ()
  (:metaclass dotcl-mop:funcallable-standard-class))
(defmethod dotcl-mop:find-method-combination ((gf mop-emit-fmc-gf) name options)
  (setf *mop-emit-combination-seen* (list name options))
  (call-next-method))

(defgeneric mop-emit-mc (x) (:generic-function-class mop-emit-mc-gf))

(deftest mop-defmethod-asks-the-generic-function-for-the-method-class
  (let ((*mop-emit-method-class-calls* 0))
    (eval '(defmethod mop-emit-mc ((x integer)) x))
    (list (notnot (plusp *mop-emit-method-class-calls*))
          (notnot (eq (class-of (first (dotcl-mop:generic-function-methods
                                        (fdefinition 'mop-emit-mc))))
                      (find-class 'mop-emit-method)))))
  (t t))

(deftest mop-uncustomised-generic-function-still-makes-standard-methods
  (progn (eval '(defgeneric mop-emit-plain (x)))
         (eval '(defmethod mop-emit-plain ((x integer)) x))
         (notnot (eq (class-of (first (dotcl-mop:generic-function-methods
                                       (fdefinition 'mop-emit-plain))))
                     (find-class 'standard-method))))
  t)

(deftest mop-defgeneric-asks-for-the-method-combination
  (let ((*mop-emit-combination-seen* nil))
    (eval '(defgeneric mop-emit-fmc (x)
             (:generic-function-class mop-emit-fmc-gf)
             (:method-combination progn)))
    *mop-emit-combination-seen*)
  (progn nil))

;;; MAKE-METHOD-LAMBDA: defmethod hands its method lambda over at macroexpansion
;;; time and compiles what comes back, so a generic function class can wrap method
;;; bodies. The lambda handed over is dotcl's own, with the arguments spread.

(defvar *mop-mml-asked* nil)
(defvar *mop-mml-body-ran* nil)

(defclass mop-mml-gf (standard-generic-function) ()
  (:metaclass dotcl-mop:funcallable-standard-class))
(defmethod dotcl-mop:make-method-lambda
    ((gf mop-mml-gf) method lambda-expression environment)
  (declare (ignore method environment))
  (setf *mop-mml-asked* t)
  (list* (first lambda-expression)
         (second lambda-expression)
         (cons '(setf *mop-mml-body-ran* t) (cddr lambda-expression))))

(defgeneric mop-mml-through (x) (:generic-function-class mop-mml-gf))

(deftest mop-defmethod-calls-make-method-lambda
  (let ((*mop-mml-asked* nil))
    (eval '(defmethod mop-mml-through ((x integer)) x))
    (notnot *mop-mml-asked*))
  t)

(deftest mop-method-lambdas-are-processed
  (let ((*mop-mml-body-ran* nil))
    (list (mop-mml-through 7) (notnot *mop-mml-body-ran*)))
  (7 t))

(deftest mop-uncustomised-method-lambda-is-left-alone
  (let ((*mop-mml-body-ran* nil))
    (eval '(defgeneric mop-mml-plain (x)))
    (eval '(defmethod mop-mml-plain ((x integer)) x))
    (list (mop-mml-plain 7) *mop-mml-body-ran*))
  (7 nil))

;;; EQL specializers are metaobjects with identity. INTERN-EQL-SPECIALIZER answers
;;; the same object for EQL objects, DEFMETHOD puts that object in the method's
;;; specializer list, and the older representation -- the list (EQL object) -- is
;;; still read wherever a specializer is accepted, since that is what a caller
;;; building one by hand produces.

(defgeneric mop-eqls (x))
(defmethod mop-eqls ((x integer)) (list :integer x))
(defmethod mop-eqls ((x (eql 42))) (cons :forty-two (call-next-method)))

(deftest mop-intern-eql-specializer-is-interned
  (let ((a (dotcl-mop:intern-eql-specializer 42)))
    (list (notnot (eq a (dotcl-mop:intern-eql-specializer 42)))
          ;; EQL, not EQUAL: 42 and 42.0d0 are different specializers
          (notnot (eq a (dotcl-mop:intern-eql-specializer 42.0d0)))))
  (t nil))

(deftest mop-eql-specializers-are-objects
  (let ((a (dotcl-mop:intern-eql-specializer 42)))
    (list (notnot (typep a 'dotcl-mop:eql-specializer))
          (notnot (typep a 'dotcl-mop:specializer))
          (eqt (class-of a) (find-class 'dotcl-mop:eql-specializer))
          (dotcl-mop:eql-specializer-object a)))
  (t t t 42))

(deftest mop-eql-specializer-object-reads-the-old-list-too
  (dotcl-mop:eql-specializer-object (list 'eql 42))
  42)

(deftest mop-defmethod-specializes-on-the-metaobject
  (let ((method (find-method #'mop-eqls '() (list (dotcl-mop:intern-eql-specializer 42)))))
    (notnot (eq (first (dotcl-mop:method-specializers method))
                (dotcl-mop:intern-eql-specializer 42))))
  t)

(deftest mop-find-method-accepts-the-old-list-too
  (notnot (eq (find-method #'mop-eqls '() (list (list 'eql 42)))
              (find-method #'mop-eqls '() (list (dotcl-mop:intern-eql-specializer 42)))))
  t)

(deftest mop-eql-dispatch-still-picks-the-eql-method
  (list (mop-eqls 42) (mop-eqls 41))
  ((:forty-two :integer 42) (:integer 41)))

(deftest mop-hand-built-eql-specializer-still-dispatches
  (progn
    (eval '(defgeneric mop-eqls-added (x)))
    (eval '(add-method #'mop-eqls-added
                       (make-instance 'standard-method
                                      :lambda-list '(x)
                                      :specializers (list (list 'eql 7))
                                      :qualifiers '()
                                      :function (lambda (args next) (declare (ignore next))
                                                  (list :seven (first args))))))
    (mop-eqls-added 7))
  (:seven 7))

;;; COMPUTE-SLOTS decides the effective slots, order included. A metaclass that
;;; reorders them changes what CLASS-SLOTS reports AND the layout, so slot access
;;; has to follow the answer rather than the order the class computed for itself.

(defclass mop-cs-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((class mop-cs-meta) (super standard-class)) t)
(defmethod dotcl-mop:compute-slots ((class mop-cs-meta)) (reverse (call-next-method)))

(defclass mop-cs () ((a) (b)) (:metaclass mop-cs-meta))

(deftest mop-compute-slots-requested-order-is-honoured
  (let ((names (mapcar #'dotcl-mop:slot-definition-name
                       (dotcl-mop:class-slots (find-class 'mop-cs))))
        (object (make-instance 'mop-cs)))
    (setf (slot-value object 'a) 1)
    (setf (slot-value object 'b) 2)
    (list names (slot-value object 'a) (slot-value object 'b)))
  ((b a) 1 2))

(defclass mop-cs-plain () ((a) (b)))

(deftest mop-uncustomised-metaclass-keeps-its-own-slot-order
  (mapcar #'dotcl-mop:slot-definition-name
          (dotcl-mop:class-slots (find-class 'mop-cs-plain)))
  (a b))

;;; A class named as a superclass before it is defined is a FORWARD-REFERENCED-CLASS,
;;; not a standard class that happens to be empty. Defining it for real changes that
;;; same object rather than making a new one, so a dependent class that already holds
;;; the placeholder does not end up pointing at a class nobody else uses.

(deftest mop-undefined-superclass-is-a-forward-referenced-class
  (progn
    (eval '(defclass mop-frc-user (mop-frc-super) ()))
    (let ((forward (find-class 'mop-frc-super)))
      (list (notnot (typep forward 'dotcl-mop:forward-referenced-class))
            (typep forward 'standard-class)
            (eqt (class-of forward) (find-class 'dotcl-mop:forward-referenced-class))
            (dotcl-mop:class-finalized-p forward))))
  (t nil t nil))

(deftest mop-defining-it-changes-that-same-class
  (let ((forward (find-class 'mop-frc-super)))
    (eval '(defclass mop-frc-super () ((a))))
    (list (notnot (eq forward (find-class 'mop-frc-super)))
          (typep forward 'dotcl-mop:forward-referenced-class)
          (notnot (typep forward 'standard-class))
          (mapcar #'dotcl-mop:slot-definition-name (dotcl-mop:class-slots forward))))
  (t nil t (a)))

;;; TYPE-OF answers the metaobject's class for the metaobjects that got one: an EQL
;;; specializer is an EQL-SPECIALIZER, and a method made under a generic function
;;; whose method class is not STANDARD-METHOD says so.

(deftest mop-type-of-an-eql-specializer
  (let ((s (dotcl-mop:intern-eql-specializer 42)))
    (list (notnot (typep s (type-of s)))
          (eqt (find-class (type-of s)) (find-class 'dotcl-mop:eql-specializer))))
  (t t))

(deftest mop-type-of-a-method-follows-its-method-class
  (list (type-of (first (dotcl-mop:generic-function-methods (fdefinition 'mop-emit-mc))))
        (type-of (first (dotcl-mop:generic-function-methods (fdefinition 'mop-emit-plain)))))
  (mop-emit-method standard-method))
