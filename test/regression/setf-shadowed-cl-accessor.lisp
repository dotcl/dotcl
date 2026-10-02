;;; A symbol that shadows a CL accessor (a package with its own ELT, GET or
;;; FIRST) names a different place: SETF of it calls its own (SETF name)
;;; function, not the built-in expander of the CL accessor with that name.

(defpackage :setf-shadow-test (:use :cl) (:shadow #:elt #:get #:first #:second))
(in-package :setf-shadow-test)

(defun elt (s i) (list :elt s i))
(defun (setf elt) (v s i) (list :set-elt v s i))
(defgeneric (setf get) (v k m))
(defmethod (setf get) (v k m) (list :set-get v k m))
(defstruct (sst-node (:conc-name nil)) first)

(cl-user::deftest setf-shadowed-elt
  (setf (elt 'a 1) 2)
  (:set-elt 2 a 1))

(cl-user::deftest setf-shadowed-get-generic
  (setf (get 'k 'm) 3)
  (:set-get 3 k m))

(cl-user::deftest setf-shadowed-first-struct-accessor
  (let ((n (make-sst-node :first 1)))
    (setf (first n) 5)
    (first n))
  5)

;;; The CL accessors keep their own expanders.
(cl-user::deftest setf-cl-accessors-unaffected
  (let ((l (list 1 2 3)) (v (vector 1 2)))
    (setf (cl:first l) :a (cl:second l) :b (cl:elt v 1) :c)
    (list l (coerce v (quote list))))
  ((:a :b 3) (1 :c)))

(in-package :cl-user)
