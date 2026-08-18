;;; SLOT-DEFINITION-READERS / -WRITERS / -TYPE report what DEFCLASS was given.
;;;
;;; They used to be stubs: TYPE was always T, READERS and WRITERS always NIL --
;;; the values were parsed by DEFCLASS (it defines the accessor methods from
;;; them) but never reached the slot definition. A library walking the MOP saw
;;; a class whose slots had no accessors and no types, with nothing signalled.

(defclass sda-base ()
  ((a :initarg :a :accessor sda-a :type integer :initform 1)
   (b :reader sda-b :writer sda-set-b :type (or null string))
   (plain :initform 0)))

(defclass sda-derived (sda-base)
  ((a :initform 2)))

(defun %sda-direct (class-name slot-name)
  (find slot-name (dotcl-mop:class-direct-slots (find-class class-name))
        :key #'slot-definition-name))

(defun %sda-effective (class-name slot-name)
  (find slot-name (class-slots (find-class class-name))
        :key #'slot-definition-name))

;;; :accessor contributes a reader NAME and the writer (SETF NAME).

(deftest slot-definition-attrs.accessor
  (let ((s (%sda-direct 'sda-base 'a)))
    (list (slot-definition-readers s) (slot-definition-writers s)))
  ((sda-a) ((setf sda-a))))

(deftest slot-definition-attrs.reader-and-writer
  (let ((s (%sda-direct 'sda-base 'b)))
    (list (slot-definition-readers s) (slot-definition-writers s)))
  ((sda-b) (sda-set-b)))

(deftest slot-definition-attrs.plain-slot-has-none
  (let ((s (%sda-direct 'sda-base 'plain)))
    (list (slot-definition-readers s) (slot-definition-writers s)
          (slot-definition-type s)))
  (nil nil t))

;;; :type is reported, and an unspecified type is T.

(deftest slot-definition-attrs.type
  (list (slot-definition-type (%sda-direct 'sda-base 'a))
        (slot-definition-type (%sda-direct 'sda-base 'b)))
  (integer (or null string)))

;;; An effective slot inherits the type, and per AMOP carries no readers/writers.

(deftest slot-definition-attrs.effective-inherits-type
  (list (slot-definition-type (%sda-effective 'sda-derived 'a))
        (slot-definition-readers (%sda-effective 'sda-derived 'a)))
  (integer nil))

;;; Both symbols answer the same thing.

(deftest slot-definition-attrs.symbols-agree
  (let ((s (%sda-direct 'sda-base 'a)))
    (list (equal (slot-definition-readers s) (dotcl-mop:slot-definition-readers s))
          (equal (slot-definition-writers s) (dotcl-mop:slot-definition-writers s))
          (equal (slot-definition-type s) (dotcl-mop:slot-definition-type s))
          (eq (slot-definition-name s) (dotcl-mop:slot-definition-name s))))
  (t t t t))

;;; A non-slot-definition argument is a TYPE-ERROR from either symbol, not NIL.

(deftest slot-definition-attrs.non-slotd-is-an-error
  (list (handler-case (progn (slot-definition-readers 42) :no-error)
          (type-error () :type-error) (error () :other))
        (handler-case (progn (dotcl-mop:slot-definition-type 42) :no-error)
          (type-error () :type-error) (error () :other)))
  (:type-error :type-error))

;;; The accessors DEFCLASS defined still work (the attributes are recorded in
;;; addition to, not instead of, the method definitions).

(deftest slot-definition-attrs.accessors-still-work
  (let ((o (make-instance 'sda-base :a 7)))
    (setf (sda-a o) 9)
    (sda-set-b "x" o)
    (list (sda-a o) (sda-b o)))
  (9 "x"))

;;; SLOT-DEFINITION-INITFORM returns the source form. It used to be NIL for
;;; every slot: only the compiled thunk was kept, and a thunk cannot be turned
;;; back into the form it came from. The form now rides the same channel as the
;;; reader/writer/type attributes.

(defclass sda-if-base ()
  ((a :initform (list 1 2))
   (b :initform 42)
   (plain)))

(defclass sda-if-derived (sda-if-base) ((b :initform (* 6 7))))

(deftest slot-definition-attrs.initform-source
  (mapcar (lambda (s) (list (slot-definition-name s) (slot-definition-initform s)))
          (dotcl-mop:class-direct-slots (find-class 'sda-if-base)))
  ((a (list 1 2)) (b 42) (plain nil)))

;; An effective slot reports the most specific initform, matching the thunk that
;; actually runs.
(deftest slot-definition-attrs.initform-effective-most-specific
  (slot-definition-initform (%sda-effective 'sda-if-derived 'b))
  (* 6 7))

(deftest slot-definition-attrs.initform-effective-inherited
  (slot-definition-initform (%sda-effective 'sda-if-derived 'a))
  (list 1 2))

(deftest slot-definition-attrs.initform-agrees-across-symbols
  (let ((s (%sda-direct 'sda-if-base 'a)))
    (equal (slot-definition-initform s) (dotcl-mop:slot-definition-initform s)))
  t)

;; The initform still initializes the slot (the form is recorded in addition to
;; the thunk, not instead of it).
(deftest slot-definition-attrs.initform-still-runs
  (list (slot-value (make-instance 'sda-if-base) 'a)
        (slot-value (make-instance 'sda-if-derived) 'b))
  ((1 2) 42))

;;; GENERIC-FUNCTION-METHODS / -NAME answer the same through either symbol.

(defgeneric sda-gf (x))
(defmethod sda-gf ((x sda-base)) x)

(deftest slot-definition-attrs.generic-function-accessors-agree
  (list (eq (generic-function-name #'sda-gf) (dotcl-mop:generic-function-name #'sda-gf))
        (= (length (generic-function-methods #'sda-gf))
           (length (dotcl-mop:generic-function-methods #'sda-gf)))
        (handler-case (progn (generic-function-name 42) :no-error)
          (type-error () :type-error) (error () :other)))
  (t t :type-error))
