;;; Redefining a class so that it gains slots updates its existing instances
;;; (CLHS 4.3.6). A class that had no slots at all kept its instances as they were,
;;; with an empty slot vector, and the first slot access on one failed with an
;;; index error: the redefinition was taken for the first definition because the
;;; old class had no slots.

(defpackage :crs (:use :cl))
(in-package :crs)

(defclass a () ((x :initarg :x) (w :initform 7)))
(defclass b () ())
(defvar *i* (make-instance 'b))
(defclass b (a) ())

(defclass c () ())
(defvar *k* (make-instance 'c))
(defclass c () ((y :initform 3) (z)))
(in-package :cl-user)

(deftest class-redefine-add-superclass.new-slots
  (list (class-name (class-of crs::*i*))
        (slot-boundp crs::*i* 'crs::x)
        (slot-boundp crs::*i* 'crs::w)
        (slot-value crs::*i* 'crs::w)
        (typep crs::*i* 'crs::a))
  (crs::b nil t 7 t))

(deftest class-redefine-add-superclass.write-and-read
  (progn (setf (slot-value crs::*i* 'crs::x) 5)
         (slot-value crs::*i* 'crs::x))
  5)

(deftest class-redefine-add-superclass.own-slots-of-an-empty-class
  (list (slot-value crs::*k* 'crs::y) (slot-boundp crs::*k* 'crs::z))
  (3 nil))

(deftest class-redefine-add-superclass.instances-made-after
  (let ((o (make-instance 'crs::b :x 1)))
    (list (slot-value o 'crs::x) (slot-value o 'crs::w)))
  (1 7))
