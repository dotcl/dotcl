;;; Slots are named by symbols (CLHS 7.5.3): two slots whose names are
;;; different symbols with the same name, e.g. CSP-A:MY-SLOT and
;;; CSP-B:MY-SLOT, are two distinct slots. The runtime used to key slots by
;;; the name string, so such a class ended up with one slot that both names
;;; read and wrote. The same held for non-keyword initargs of the same name.

(defpackage :csp-a (:use :cl) (:export #:my-slot))
(defpackage :csp-b (:use :cl) (:export #:my-slot))
(defpackage :csp-c (:use :cl) (:export #:my-slot))

(defclass csp-both ()
  ((csp-a:my-slot :initarg :my-slot :initarg csp-a:my-slot
                  :accessor csp-a-slot :initform 1)
   (csp-b:my-slot :initarg csp-b:my-slot :accessor csp-b-slot :initform 2)))

(deftest slot-name-packages.class-slots
  (mapcar #'dotcl-mop:slot-definition-name
          (dotcl-mop:class-slots (find-class 'csp-both)))
  (csp-a:my-slot csp-b:my-slot))

(deftest slot-name-packages.slot-value
  (let ((o (make-instance 'csp-both)))
    (list (slot-value o 'csp-a:my-slot) (slot-value o 'csp-b:my-slot)))
  (1 2))

(deftest slot-name-packages.setf-slot-value
  (let ((o (make-instance 'csp-both)))
    (setf (slot-value o 'csp-b:my-slot) 9)
    (list (slot-value o 'csp-a:my-slot) (slot-value o 'csp-b:my-slot)
          (csp-a-slot o) (csp-b-slot o)))
  (1 9 1 9))

(deftest slot-name-packages.accessors
  (let ((o (make-instance 'csp-both)))
    (setf (csp-a-slot o) 10)
    (let ((r (list (csp-a-slot o) (csp-b-slot o))))
      (setf (csp-b-slot o) 20)
      (append r (list (csp-a-slot o) (csp-b-slot o)))))
  (10 2 10 20))

(defun csp-read-a (o) (csp-a-slot o))
(defun csp-read-b (o) (csp-b-slot o))

;; The accessor call sites cache (class, index); run each a few times so the
;; cached path is the one answering.
(deftest slot-name-packages.accessor-cache
  (let ((o (make-instance 'csp-both 'csp-b:my-slot 5))
        (r nil))
    (dotimes (i 3) (push (list (csp-read-a o) (csp-read-b o)) r))
    r)
  ((1 5) (1 5) (1 5)))

(deftest slot-name-packages.non-keyword-initargs
  (list (let ((o (make-instance 'csp-both 'csp-a:my-slot 7)))
          (list (csp-a-slot o) (csp-b-slot o)))
        (let ((o (make-instance 'csp-both 'csp-b:my-slot 8)))
          (list (csp-a-slot o) (csp-b-slot o)))
        (let ((o (make-instance 'csp-both :my-slot 9)))
          (list (csp-a-slot o) (csp-b-slot o))))
  ((7 2) (1 8) (9 2)))

(defclass csp-shared-initarg ()
  ((csp-a:my-slot :initarg :v :reader csp-sia)
   (csp-b:my-slot :initarg :v :reader csp-sib)))

;; One initarg naming two slots initializes both (CLHS 7.1.4).
(deftest slot-name-packages.one-initarg-two-slots
  (let ((o (make-instance 'csp-shared-initarg :v 3)))
    (list (csp-sia o) (csp-sib o)))
  (3 3))

(defclass csp-defaults ()
  ((csp-a:my-slot :initarg csp-a:my-slot :reader csp-da)
   (csp-b:my-slot :initarg csp-b:my-slot :reader csp-db))
  (:default-initargs csp-a:my-slot 11 csp-b:my-slot 22))

(deftest slot-name-packages.default-initargs
  (list (let ((o (make-instance 'csp-defaults)))
          (list (csp-da o) (csp-db o)))
        (let ((o (make-instance 'csp-defaults 'csp-a:my-slot 5)))
          (list (csp-da o) (csp-db o))))
  ((11 22) (5 22)))

(deftest slot-name-packages.boundp-makunbound
  (let ((o (make-instance 'csp-both)))
    (slot-makunbound o 'csp-b:my-slot)
    (list (slot-boundp o 'csp-a:my-slot) (slot-boundp o 'csp-b:my-slot)))
  (t nil))

(deftest slot-name-packages.exists-p
  (let ((o (make-instance 'csp-both)))
    (list (slot-exists-p o 'csp-a:my-slot) (slot-exists-p o 'csp-b:my-slot)
          (slot-exists-p o 'csp-c:my-slot)))
  (t t nil))

(deftest slot-name-packages.missing-other-package
  (let ((o (make-instance 'csp-both)))
    (handler-case (progn (slot-value o 'csp-c:my-slot) :no-error)
      (error () :error)))
  :error)

(deftest slot-name-packages.shared-initialize-slot-names
  (let ((o (make-instance 'csp-both)))
    (slot-makunbound o 'csp-a:my-slot)
    (slot-makunbound o 'csp-b:my-slot)
    (shared-initialize o '(csp-b:my-slot))
    (list (slot-boundp o 'csp-a:my-slot) (slot-value o 'csp-b:my-slot)))
  (nil 2))

(deftest slot-name-packages.reinitialize-instance
  (let ((o (make-instance 'csp-both)))
    (reinitialize-instance o 'csp-b:my-slot 30)
    (reinitialize-instance o :my-slot 40)
    (list (csp-a-slot o) (csp-b-slot o)))
  (40 30))

(defclass csp-only-a ()
  ((csp-a:my-slot :initarg :a :initform 100)))

(deftest slot-name-packages.change-class
  (let ((o (make-instance 'csp-only-a :a 5)))
    (change-class o 'csp-both)
    (list (slot-value o 'csp-a:my-slot) (slot-value o 'csp-b:my-slot)))
  (5 2))

;; A class without a collision still tells the symbols apart.
(deftest slot-name-packages.no-collision-other-package
  (let ((o (make-instance 'csp-only-a)))
    (list (slot-exists-p o 'csp-a:my-slot) (slot-exists-p o 'csp-b:my-slot)
          (handler-case (progn (slot-value o 'csp-b:my-slot) :no-error)
            (error () :error))))
  (t nil :error))

(defclass csp-only-b ()
  ((csp-b:my-slot :initform 200)))

;; The value of CSP-A:MY-SLOT is not carried into CSP-B:MY-SLOT: the new
;; slot is a different one and gets its initform.
(deftest slot-name-packages.change-class-no-crossover
  (let ((o (make-instance 'csp-only-a :a 5)))
    (change-class o 'csp-only-b)
    (slot-value o 'csp-b:my-slot))
  200)

(defvar *csp-added-slots* nil)
(defmethod update-instance-for-different-class :before
    ((previous csp-only-a) (current csp-both) &rest initargs)
  (declare (ignore initargs))
  (setf *csp-added-slots*
        (loop for sd in (dotcl-mop:class-slots (class-of current))
              for n = (dotcl-mop:slot-definition-name sd)
              unless (slot-exists-p previous n) collect n)))

(deftest slot-name-packages.added-slots
  (progn (change-class (make-instance 'csp-only-a) 'csp-both)
         *csp-added-slots*)
  (csp-b:my-slot))

;; Inheritance: a subclass adding a same-named slot from another package gets
;; a second slot instead of merging with the inherited one.
(defclass csp-base () ((csp-a:my-slot :initarg :a :initform :base)))
(defclass csp-derived (csp-base) ((csp-b:my-slot :initarg :b :initform :derived)))

(deftest slot-name-packages.inheritance
  (let ((o (make-instance 'csp-derived :a 1)))
    (list (length (dotcl-mop:class-slots (find-class 'csp-derived)))
          (slot-value o 'csp-a:my-slot) (slot-value o 'csp-b:my-slot)))
  (2 1 :derived))

;; Redefining a class to add the colliding slot.
(defclass csp-redef () ((csp-a:my-slot :initarg :a)))
(defclass csp-redef () ((csp-a:my-slot :initarg :a) (csp-b:my-slot :initarg :b)))

(deftest slot-name-packages.redefinition
  (let ((o (make-instance 'csp-redef :a 1 :b 2)))
    (list (length (dotcl-mop:class-slots (find-class 'csp-redef)))
          (slot-value o 'csp-a:my-slot) (slot-value o 'csp-b:my-slot)))
  (2 1 2))

(deftest slot-name-packages.describe
  (let* ((o (make-instance 'csp-both 'csp-b:my-slot 'csp-marker))
         (s (with-output-to-string (*standard-output*) (describe o))))
    (list (not (null (search "CSP-MARKER" s)))
          (not (null (search "1" s)))))
  (t t))

(define-condition csp-cond (error)
  ((csp-a:my-slot :initarg :a :reader csp-cond-a)
   (csp-b:my-slot :initarg :b :reader csp-cond-b)))

(deftest slot-name-packages.condition
  (let ((c (make-condition 'csp-cond :a 1 :b 2)))
    (list (csp-cond-a c) (csp-cond-b c)))
  (1 2))

;; Standard condition slots keep working when a user condition class uses a
;; same-named symbol for its own slot.
(define-condition csp-simple (simple-error)
  ((csp-a:my-slot :initarg :a :reader csp-simple-a)))

(deftest slot-name-packages.simple-condition
  (let ((c (make-condition 'csp-simple :a 1 :format-control "x~a"
                                       :format-arguments '(2))))
    (list (csp-simple-a c) (format nil "~a" c)))
  (1 "x2"))
