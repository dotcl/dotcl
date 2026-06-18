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

;;; --- slot-value-using-class dispatch (issue #259, AMOP §5.4) ---

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
  ;; dispatches a :write (required for #264 / McCLIM dynamic slots). These tests
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

;;; --- setf GF cross-package bleed guard (issue #261, D1082) ---
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

;;; D1110/#270: standard GF stores its lambda-list; MOP readers return the
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

;;; --- custom metaclass identity: defclass and ensure-class (#287) ---
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

;;; --- metaclass-added slots on the class metaobject (#291) ---
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

;;; #295: a class metaobject runs the metaclass's inherited initialize-instance
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

;;; #296: re-ensure-class an existing class under a different metaclass switches the
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

;;; #297: on a class metaobject built via ensure-class, initialize-instance :after
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
