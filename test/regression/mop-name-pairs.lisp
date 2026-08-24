;;; Nineteen MOP introspection names are registered twice: once in DOTCL-MOP and
;;; once in the CL / DOTCL-INTERNAL side. Which one a caller reaches depends on
;;; how it wrote the name, so the two have to answer identically.
;;;
;;; They did not, and the failure mode is invisible: CLASS-DIRECT-SUBCLASSES was
;;; a stub returning NIL for every class on one side while the other walked the
;;; registry, and GENERIC-FUNCTION-LAMBDA-LIST had the same split and returned
;;; NIL only on Linux. Both were fixed by pointing each pair at one shared
;;; implementation — but they remain two separate function objects wrapping it,
;;; so nothing stops a later edit from touching one side alone.
;;;
;;; This pins the property instead of the mechanism: for every name registered on
;;; both sides, both answer the same. The last test is the one that keeps the
;;; list honest — a NEW double registration fails until someone adds it here and
;;; checks that the two agree.

(defclass mnp-base () ((a :initform 1 :initarg :a :accessor mnp-a :type integer)))
(defclass mnp-derived (mnp-base) ((b :initform 2 :initarg :b :reader mnp-b)))

(defgeneric mnp-gf (x))
(defmethod mnp-gf ((x mnp-base)) (list :base x))

;;; Name -> what kind of object it takes. The pairs, as they stand today.
(defparameter *mnp-pairs*
  '(("CLASS-DIRECT-SLOTS"           :class)
    ("CLASS-DIRECT-SUBCLASSES"      :class)
    ("CLASS-DIRECT-SUPERCLASSES"    :class)
    ("CLASS-FINALIZED-P"            :class)
    ("CLASS-PRECEDENCE-LIST"        :class)
    ("CLASS-PROTOTYPE"              :class)
    ("CLASS-SLOTS"                  :class)
    ("GENERIC-FUNCTION-METHODS"     :gf)
    ("GENERIC-FUNCTION-NAME"        :gf)
    ("METHOD-GENERIC-FUNCTION"      :method)
    ("METHOD-LAMBDA-LIST"           :method)
    ("SLOT-DEFINITION-ALLOCATION"   :slotd)
    ("SLOT-DEFINITION-INITARGS"     :slotd)
    ("SLOT-DEFINITION-INITFORM"     :slotd)
    ("SLOT-DEFINITION-INITFUNCTION" :slotd)
    ("SLOT-DEFINITION-NAME"         :slotd)
    ("SLOT-DEFINITION-READERS"      :slotd)
    ("SLOT-DEFINITION-TYPE"         :slotd)
    ("SLOT-DEFINITION-WRITERS"      :slotd)))

;;; Names registered on both sides as ONE shared function object. Nothing to
;;; compare — sameness is stronger than agreement — but they still have to be
;;; listed, so the completeness check below stays exhaustive.
(defparameter *mnp-shared-object-pairs*
  '("ENSURE-CLASS" "MAKE-METHOD-LAMBDA" "METHOD-QUALIFIERS" "METHOD-SPECIALIZERS"
    "SLOT-BOUNDP-USING-CLASS" "SLOT-MAKUNBOUND-USING-CLASS" "SLOT-VALUE-USING-CLASS"))

(defun %mnp-arg (kind)
  (let ((class (find-class 'mnp-derived)))
    (ecase kind
      (:class class)
      (:gf #'mnp-gf)
      (:method (first (dotcl-mop:generic-function-methods #'mnp-gf)))
      (:slotd (first (dotcl-mop:class-direct-slots class))))))

(defun %mnp-other-symbol (name)
  "The non-DOTCL-MOP symbol of NAME that is fbound, if any."
  (dolist (pkg '("COMMON-LISP" "DOTCL-INTERNAL"))
    (let ((s (find-symbol name (find-package pkg))))
      (when (and s (fboundp s)) (return s)))))

(defun %mnp-disagreements ()
  "Names whose two registrations answer differently (or where one errors)."
  (let ((bad '()))
    (dolist (entry *mnp-pairs* (nreverse bad))
      (destructuring-bind (name kind) entry
        (let ((mop-sym (find-symbol name (find-package "DOTCL-MOP")))
              (other (%mnp-other-symbol name))
              (arg (%mnp-arg kind)))
          (when (and mop-sym other (fboundp mop-sym))
            (let ((r1 (multiple-value-list
                       (ignore-errors (funcall (symbol-function mop-sym) arg))))
                  (r2 (multiple-value-list
                       (ignore-errors (funcall (symbol-function other) arg)))))
              (unless (equal r1 r2)
                (push (list name (first r1) (first r2)) bad)))))))))

(deftest mop-name-pairs-agree
  (%mnp-disagreements)
  nil)

;;; Every name that IS registered on both sides has to be in the table above.
;;; Without this, a new double registration lands unchecked — which is how the
;;; two CLASS-DIRECT-SUBCLASSES implementations drifted apart in the first place.

;;; The shared-object ones really are one object, not two that happen to agree.

(defun %mnp-not-shared ()
  (remove-if (lambda (name)
               (let ((m (find-symbol name (find-package "DOTCL-MOP")))
                     (o (%mnp-other-symbol name)))
                 (or (null m) (null o) (not (fboundp m))
                     (eq (symbol-function m) (symbol-function o)))))
             *mnp-shared-object-pairs*))

(deftest mop-name-pairs-shared-objects-are-shared
  (%mnp-not-shared)
  nil)

;;; A lambda list rebuilt from arity uses fresh uninterned parameter names, which
;;; is fine — but it was rebuilt on EVERY call, so asking the same method twice
;;; gave two lists that were not even EQUAL. Callers that cache or compare a
;;; lambda list saw it change under them.

(deftest mop-method-lambda-list-is-stable
  (let ((m (first (dotcl-mop:generic-function-methods #'mnp-gf))))
    (list (equal (dotcl-mop:method-lambda-list m) (dotcl-mop:method-lambda-list m))
          (eq (dotcl-mop:method-lambda-list m) (dotcl-mop:method-lambda-list m))))
  (t t))

(deftest mop-generic-function-lambda-list-is-stable
  (list (equal (dotcl-mop:generic-function-lambda-list #'mnp-gf)
               (dotcl-mop:generic-function-lambda-list #'mnp-gf))
        (eq (dotcl-mop:generic-function-lambda-list #'mnp-gf)
            (dotcl-mop:generic-function-lambda-list #'mnp-gf)))
  (t t))

(defun %mnp-unlisted-pairs ()
  (let ((mop (find-package "DOTCL-MOP"))
        (known (append (mapcar #'first *mnp-pairs*) *mnp-shared-object-pairs*))
        (found '()))
    (do-symbols (s mop (sort found #'string<))
      (when (and (eq (symbol-package s) mop) (fboundp s))
        (let ((name (symbol-name s)))
          (when (and (%mnp-other-symbol name)
                     (not (member name known :test #'string=))
                     (not (member name found :test #'string=)))
            (push name found)))))))

(deftest mop-name-pairs-table-is-complete
  (%mnp-unlisted-pairs)
  nil)
