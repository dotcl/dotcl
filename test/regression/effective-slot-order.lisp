;;; The effective slots of a class come in the order AMOP gives COMPUTE-SLOTS and
;;; SBCL produces: the classes of the precedence list from the least specific to
;;; the most specific, each one's direct slots in the order written, a repeated
;;; slot staying where it first appeared. Inherited slots come first. The order
;;; is the order MAKE-INSTANCE evaluates the initforms in.

(defpackage :eso (:use :cl :dotcl-mop))
(in-package :eso)

(defclass a () ((x :initarg :x) (y :initarg :y)))
(defclass b (a) ((z :initarg :z) (x :initarg :x)))
(defclass c (a) ((w :initarg :w)))
(defclass d (b c) ((v :initarg :v)))

(defvar *log* nil)
(defclass la () ((p :initform (push :p *log*)) (q :initform (push :q *log*))))
(defclass lb (la) ((r :initform (push :r *log*)) (p :initform (push :p2 *log*))))
(defclass lc (la) ((s :initform (push :s *log*))))
(defclass ld (lb lc) ((t1 :initform (push :t *log*))))

(defun slot-names (name)
  (let ((c (find-class name)))
    (finalize-inheritance c)
    (mapcar #'slot-definition-name (class-slots c))))
(in-package :cl-user)

(deftest effective-slot-order.inherited-first
  (mapcar #'eso::slot-names '(eso::a eso::b eso::c eso::d))
  ((eso::x eso::y) (eso::x eso::y eso::z) (eso::x eso::y eso::w)
   (eso::x eso::y eso::w eso::z eso::v)))

(deftest effective-slot-order.initform-evaluation
  (progn
    (setq eso::*log* nil)
    (make-instance 'eso::ld)
    (reverse eso::*log*))
  (:p2 :q :s :r :t))

(deftest effective-slot-order.redefinition-keeps-order
  (progn
    (eval '(defclass eso::re-a () ((m) (n))))
    (eval '(defclass eso::re-b (eso::re-a) ((o) (m))))
    (let ((first (eso::slot-names 'eso::re-b)))
      (eval '(defclass eso::re-b (eso::re-a) ((o) (m) (extra))))
      (list first (eso::slot-names 'eso::re-b))))
  ((m n o) (m n o extra)))
