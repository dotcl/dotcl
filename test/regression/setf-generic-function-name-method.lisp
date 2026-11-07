;;; (SETF GENERIC-FUNCTION-NAME) is a generic function whose primary method does
;;; the renaming (AMOP). A method of the program on it, even one with only an
;;; :BEFORE qualifier, runs around it and the rename still happens; it used to
;;; leave the generic function without a primary method.

(defpackage :sgfn (:use :cl :dotcl-mop))
(in-package :sgfn)

(defvar *log* nil)
(defclass named-gf (standard-generic-function) () (:metaclass funcallable-standard-class))
(defgeneric target (x) (:generic-function-class named-gf))

(defmethod (setf generic-function-name) :before (new (gf named-gf))
  (push (list :renaming new) *log*))
(in-package :cl-user)

(deftest setf-generic-function-name-method.before-runs-and-rename-happens
  (progn
    (setq sgfn::*log* nil)
    (list (setf (dotcl-mop:generic-function-name #'sgfn::target) 'sgfn::renamed)
          (dotcl-mop:generic-function-name #'sgfn::target)
          sgfn::*log*))
  (sgfn::renamed sgfn::renamed ((:renaming sgfn::renamed))))

(deftest setf-generic-function-name-method.other-generic-functions
  (progn
    (setq sgfn::*log* nil)
    (defgeneric sgfn::other (x))
    (setf (dotcl-mop:generic-function-name #'sgfn::other) 'sgfn::other2)
    (list (dotcl-mop:generic-function-name #'sgfn::other) sgfn::*log*))
  (sgfn::other2 nil))
