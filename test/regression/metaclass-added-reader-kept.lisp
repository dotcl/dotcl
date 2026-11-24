;;; A reader method that a metaclass's INITIALIZE-INSTANCE :AROUND adds for a slot's reader (mito's
;;; relational readers) stays: the DEFCLASS accessor definition, which runs once the class exists,
;;; does not replace it. Class initialization defines the accessors first in SBCL, so the method
;;; added after CALL-NEXT-METHOD wins there.

(defpackage :mar (:use :cl :dotcl-mop))
(in-package :mar)

(defclass rel-meta (standard-class) ())
(defmethod validate-superclass ((c rel-meta) (s standard-class)) t)
(defmethod initialize-instance :around ((class rel-meta) &rest initargs &key &allow-other-keys)
  (let ((c (apply #'call-next-method class initargs)))
    (dolist (s (class-direct-slots c) c)
      (when (eq (slot-definition-name s) 'owner)
        (dolist (reader (slot-definition-readers s))
          (let ((gf (ensure-generic-function reader :lambda-list '(object))))
            (add-method gf (make-instance 'standard-method
                                          :lambda-list '(object) :qualifiers ()
                                          :specializers (list c)
                                          :function (lambda (object &rest ignore)
                                                      (declare (ignore ignore))
                                                      (when (consp object) (setf object (first object)))
                                                      (list :relational
                                                            (if (slot-boundp object 'owner)
                                                                (slot-value object 'owner)
                                                                :loaded-lazily)))))))))))

(defclass row ()
  ((owner :initarg :owner :reader row-owner)
   (title :initarg :title :reader row-title :accessor row-title-acc))
  (:metaclass rel-meta))

;; a class with the standard metaclass: the accessors are still defined, as always
(defclass plain-row () ((a :initarg :a :reader plain-a :accessor plain-a-acc)))
(in-package :cl-user)

(deftest metaclass-added-reader.kept
  (list (mar::row-owner (make-instance 'mar::row :owner 5))
        (mar::row-owner (make-instance 'mar::row))
        (mar::row-title (make-instance 'mar::row :title "t"))
        (mar::row-title-acc (make-instance 'mar::row :title "u")))
  ((:relational 5) (:relational :loaded-lazily) "t" "u"))

(deftest metaclass-added-reader.other-accessors-unchanged
  (let ((o (make-instance 'mar::row :title "x")))
    (setf (mar::row-title-acc o) "y")
    (list (mar::row-title o)
          (let ((p (make-instance 'mar::plain-row :a 1)))
            (setf (mar::plain-a-acc p) 2)
            (list (mar::plain-a p) (mar::plain-a-acc p)))))
  ("y" (2 2)))
