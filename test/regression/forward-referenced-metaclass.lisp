;;; ENSURE-CLASS with a subclass of FORWARD-REFERENCED-CLASS as the :METACLASS
;;; makes a placeholder class. It used to get STANDARD-OBJECT as its default
;;; superclass, and VALIDATE-SUPERCLASS then rejected that pair. McCLIM makes
;;; such placeholders for presentation types named in :INHERIT-FROM before they
;;; are defined; a later DEFCLASS of the name replaces the placeholder.

(defpackage :frmeta (:use :cl))
(in-package :frmeta)

(defclass placeholder-class (dotcl-mop:forward-referenced-class) ())

(defun make-placeholder (name)
  (dotcl-mop:ensure-class name :name name :metaclass 'placeholder-class
                               :direct-superclasses nil))
(in-package :cl-user)

(deftest forward-referenced-metaclass.placeholder
  (let ((c (frmeta::make-placeholder 'frmeta::p1)))
    (list (class-name (class-of c))
          (typep c 'dotcl-mop:forward-referenced-class)
          (dotcl-mop:class-direct-superclasses c)
          (dotcl-mop:class-finalized-p c)
          (eq c (find-class 'frmeta::p1))))
  (frmeta::placeholder-class t nil nil t))

;;; A subclass defined while the placeholder stands is not finalized, and it is
;;; once the placeholder's name is defined for real, here with T as the only
;;; superclass (the shape of McCLIM's test).
(deftest forward-referenced-metaclass.defined-later
  (progn
    (frmeta::make-placeholder 'frmeta::p2)
    (defclass frmeta::p2-leaf (frmeta::p2) ())
    (let ((before (dotcl-mop:class-finalized-p (find-class 'frmeta::p2-leaf))))
      (defclass frmeta::p2 (t) ())
      (make-instance 'frmeta::p2-leaf)
      (list before
            (class-name (class-of (find-class 'frmeta::p2)))
            (mapcar #'class-name
                    (dotcl-mop:class-precedence-list (find-class 'frmeta::p2-leaf))))))
  (nil standard-class (frmeta::p2-leaf frmeta::p2 t)))

;;; Defining the name with no superclasses gives the usual STANDARD-OBJECT.
(deftest forward-referenced-metaclass.defined-with-slots
  (progn
    (frmeta::make-placeholder 'frmeta::p3)
    (defclass frmeta::p3 () ((a :initarg :a :reader frmeta::p3-a)))
    (list (frmeta::p3-a (make-instance 'frmeta::p3 :a 3))
          (mapcar #'class-name
                  (dotcl-mop:class-precedence-list (find-class 'frmeta::p3)))))
  (3 (frmeta::p3 standard-object t)))

;;; Plain FORWARD-REFERENCED-CLASS placeholders are unchanged.
(deftest forward-referenced-metaclass.plain-forward-reference
  (progn
    (defclass frmeta::p4-leaf (frmeta::p4) ())
    (list (class-name (class-of (find-class 'frmeta::p4)))
          (dotcl-mop:class-finalized-p (find-class 'frmeta::p4-leaf))))
  (dotcl-mop:forward-referenced-class nil))

;;; FINALIZE-INHERITANCE of a forward-referenced class, or of a class with a
;;; forward-referenced superclass, signals an error. It returned quietly and
;;; left the class unfinalized. McCLIM relies on the error to report a
;;; presentation type whose supertype is not defined yet.
(deftest forward-referenced-metaclass.finalize-signals
  (progn
    (frmeta::make-placeholder 'frmeta::p5)
    (defclass frmeta::p5-leaf (frmeta::p5) ())
    (defclass frmeta::p6-leaf (frmeta::p6) ())
    (defclass frmeta::p6-leaf2 (frmeta::p6-leaf) ())
    (flet ((errs (name)
             (handler-case (progn (dotcl-mop:finalize-inheritance (find-class name)) :no-error)
               (error () :error))))
      (list (errs 'frmeta::p5) (errs 'frmeta::p5-leaf)
            (errs 'frmeta::p6) (errs 'frmeta::p6-leaf) (errs 'frmeta::p6-leaf2)
            (progn (defclass frmeta::p6 () ())
                   (errs 'frmeta::p6-leaf2))
            (dotcl-mop:class-finalized-p (find-class 'frmeta::p6-leaf2)))))
  (:error :error :error :error :error :no-error t))
