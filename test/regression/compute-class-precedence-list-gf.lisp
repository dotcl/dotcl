;;; COMPUTE-CLASS-PRECEDENCE-LIST is a generic function that finalization calls,
;;; so a method specialised on a metaclass decides the class's precedence list.

(defpackage :ccpl (:use :cl :dotcl-mop))
(in-package :ccpl)

(defclass reorder-class (standard-class) ())
(defmethod validate-superclass ((c reorder-class) (s standard-class)) t)

(defclass ccpl-a () ())
(defclass ccpl-b () ())

;; Put the direct superclasses in reverse order after the class itself.
(defmethod compute-class-precedence-list ((c reorder-class))
  (let ((default (call-next-method)))
    (if (member (find-class 'ccpl-b) default)
        (list* c (find-class 'ccpl-b) (find-class 'ccpl-a)
               (remove-if (lambda (k) (member k (list c (find-class 'ccpl-a)
                                                      (find-class 'ccpl-b))))
                          default))
        default)))

(defclass ccpl-sub (ccpl-a ccpl-b) () (:metaclass reorder-class))
(defclass ccpl-plain (ccpl-a ccpl-b) ())
(in-package :cl-user)

(deftest ccpl-gf.user-method-takes-effect
  (mapcar #'class-name (class-precedence-list (find-class 'ccpl::ccpl-sub)))
  (ccpl::ccpl-sub ccpl::ccpl-b ccpl::ccpl-a standard-object t))

(deftest ccpl-gf.plain-class-unaffected
  (mapcar #'class-name (class-precedence-list (find-class 'ccpl::ccpl-plain)))
  (ccpl::ccpl-plain ccpl::ccpl-a ccpl::ccpl-b standard-object t))

(deftest ccpl-gf.is-generic
  (typep #'dotcl-mop:compute-class-precedence-list 'generic-function)
  t)
