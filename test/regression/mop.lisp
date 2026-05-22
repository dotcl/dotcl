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

  (deftest mop-svuc-read-dispatch
    (progn (setq svuc-log nil)
           (let ((o (make-instance 'svuc-obj :x 1)))
             (svuc-x o))
           (member :read svuc-log))
    (:read))

  (deftest mop-svuc-write-dispatch
    (progn (setq svuc-log nil)
           (let ((o (make-instance 'svuc-obj :x 1)))
             (setf (svuc-x o) 2))
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
