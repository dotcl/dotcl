;;; A metaclass that defines no method on SLOT-VALUE-USING-CLASS, its SETF,
;;; SLOT-BOUNDP-USING-CLASS or SLOT-MAKUNBOUND-USING-CLASS (one that only adds
;;; slots or class options) sent every slot access on its instances through
;;; those generic functions anyway, where only dotcl's defaults could run: an
;;; accessor cost about 30 times the same accessor on a STANDARD-CLASS class.
;;; Such a class now reads the slot vector directly, and is asked the same
;;; question as a STANDARD-CLASS class: does a program method apply to its
;;; instances, under its metaclass, for one of its slot definitions?  These
;;; tests check that the answer follows methods added and removed later, and
;;; that a method specialized on a custom slot-definition class still runs.

(defclass mwsm-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mwsm-meta) (s standard-class)) t)

(defclass mwsm-obj ()
  ((a :initarg :a :initform 1 :accessor mwsm-a)
   (b :initarg :b :accessor mwsm-b))
  (:metaclass mwsm-meta))

(defclass mwsm-plain () ((a :initform 1 :accessor mwsm-plain-a)))

(defvar *mwsm-log* nil)

;; Accessors, SLOT-VALUE (constant and variable name), their SETFs, SLOT-BOUNDP,
;; SLOT-MAKUNBOUND and the unbound-slot error, from a compiled function so the
;; call-site caches are exercised.
(defun %mwsm-exercise (o)
  (let ((name 'a) (r '()))
    (push (mwsm-a o) r)
    (setf (mwsm-a o) 2)
    (push (slot-value o 'a) r)
    (setf (slot-value o name) 3)
    (push (slot-value o name) r)
    (push (slot-boundp o 'b) r)
    (setf (mwsm-b o) 4)
    (push (list (slot-boundp o 'b) (mwsm-b o)) r)
    (slot-makunbound o 'b)
    (push (slot-boundp o 'b) r)
    (push (handler-case (mwsm-b o) (unbound-slot (c) (cell-error-name c))) r)
    (nreverse r)))

(deftest metaclass-without-slot-methods.semantics
  (%mwsm-exercise (make-instance 'mwsm-obj))
  (1 2 3 nil (t 4) nil b))

(deftest metaclass-without-slot-methods.initargs
  (let ((o (make-instance 'mwsm-obj :a 10 :b 20)))
    (list (mwsm-a o) (mwsm-b o)))
  (10 20))

;; A method on another metaclass leaves this one's instances alone.
(defclass mwsm-other-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mwsm-other-meta) (s standard-class)) t)
(defmethod dotcl-mop:slot-value-using-class :around ((c mwsm-other-meta) o slotd)
  (declare (ignore o slotd))
  (push :other *mwsm-log*)
  (call-next-method))

(deftest metaclass-without-slot-methods.other-metaclass-method
  (let ((*mwsm-log* nil) (o (make-instance 'mwsm-obj)))
    (list (mwsm-a o) (slot-value o 'a) *mwsm-log*))
  (1 1 nil))

;; A method added on this metaclass after instances and call sites exist runs
;; from the next access on, and stops running once removed.
(defun %mwsm-read (o) (list (mwsm-a o) (slot-value o 'a)))

(deftest metaclass-without-slot-methods.method-added-later
  (let ((o (make-instance 'mwsm-obj)))
    (%mwsm-read o)                      ; fill the call-site caches
    (let* ((*mwsm-log* nil)
           (m (defmethod dotcl-mop:slot-value-using-class :around ((c mwsm-meta) o slotd)
                (declare (ignore o slotd))
                (push :read *mwsm-log*)
                (call-next-method)))
           (with (%mwsm-read o))
           (log-with (copy-list *mwsm-log*)))
      (remove-method #'dotcl-mop:slot-value-using-class m)
      (setq *mwsm-log* nil)
      (list with log-with (%mwsm-read o) *mwsm-log*)))
  ((1 1) (:read :read) (1 1) nil))

(deftest metaclass-without-slot-methods.setf-method-added-later
  (let ((o (make-instance 'mwsm-obj)))
    (setf (mwsm-a o) 5)
    (let* ((*mwsm-log* nil)
           (m (defmethod (setf dotcl-mop:slot-value-using-class) :after (v (c mwsm-meta) o slotd)
                (declare (ignore o))
                (push (list v (dotcl-mop:slot-definition-name slotd)) *mwsm-log*)))
           (ignore (setf (mwsm-a o) 6 (slot-value o 'a) 7))
           (log-with (reverse *mwsm-log*)))
      (declare (ignore ignore))
      (remove-method #'(setf dotcl-mop:slot-value-using-class) m)
      (setq *mwsm-log* nil)
      (setf (mwsm-a o) 8)
      (list log-with (mwsm-a o) *mwsm-log*)))
  (((6 a) (7 a)) 8 nil))

;; A metaclass that gives its classes their own effective slot definition
;; class, and a method specialized only on that slot class: it applies, though
;; neither the class nor the instance specializer names the metaclass.
(defclass mwsm-slotd (dotcl-mop:standard-effective-slot-definition) ())
(defclass mwsm-slotd-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mwsm-slotd-meta) (s standard-class)) t)
(defmethod dotcl-mop:effective-slot-definition-class ((c mwsm-slotd-meta) &rest initargs)
  (declare (ignore initargs))
  (find-class 'mwsm-slotd))
(defmethod dotcl-mop:slot-value-using-class :around (c o (slotd mwsm-slotd))
  (declare (ignore c o))
  (push :slotd *mwsm-log*)
  (call-next-method))
(defclass mwsm-slotd-obj () ((a :initform 1 :accessor mwsm-slotd-a)) (:metaclass mwsm-slotd-meta))

(deftest metaclass-without-slot-methods.slot-definition-class-method
  (let ((*mwsm-log* nil) (o (make-instance 'mwsm-slotd-obj)) (p (make-instance 'mwsm-plain)))
    (list (mwsm-slotd-a o) (slot-value o 'a) (mwsm-plain-a p) (mwsm-a (make-instance 'mwsm-obj))
          *mwsm-log*))
  (1 1 1 1 (:slotd :slotd)))
