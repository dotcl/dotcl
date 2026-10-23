;;; Two structures whose names are the same string in different packages are
;;; different types: TYPEP, the predicate and TYPECASE tell them apart, and an
;;; accessor of one rejects the other.

(defpackage :ssnop-a (:use :cl))
(defpackage :ssnop-b (:use :cl))

(defstruct (ssnop-a::node (:conc-name ssnop-an-) (:constructor make-ssnop-an)
                          (:predicate ssnop-an-p))
  x)
(defstruct (ssnop-b::node (:conc-name ssnop-bn-) (:constructor make-ssnop-bn)
                          (:predicate ssnop-bn-p))
  y)
(defstruct (ssnop-b::child (:include ssnop-b::node) (:constructor make-ssnop-bc)))

(defun %ssnop-which (n)
  (typecase n
    (ssnop-a::node (list :a (ssnop-an-x n)))
    (ssnop-b::node (list :b (ssnop-bn-y n)))
    (t :other)))

(deftest-compiled-only struct-same-name-other-package.typep
  (let ((a (make-ssnop-an :x 1)) (b (make-ssnop-bn :y 2)) (c (make-ssnop-bc :y 3)))
    (list (typep a 'ssnop-a::node) (typep b 'ssnop-a::node) (typep c 'ssnop-a::node)
          (typep b 'ssnop-b::node) (typep c 'ssnop-b::node)
          (ssnop-an-p b) (ssnop-bn-p a) (ssnop-bn-p c)
          (funcall (compile nil '(lambda (o) (typep o 'ssnop-a::node))) b)))
  (t nil nil t t nil nil t nil))

(deftest struct-same-name-other-package.typecase
  (list (%ssnop-which (make-ssnop-an :x 1)) (%ssnop-which (make-ssnop-bn :y 2))
        (%ssnop-which (make-ssnop-bc :y 3)))
  ((:a 1) (:b 2) (:b 3)))

(deftest struct-same-name-other-package.accessor
  (handler-case (ssnop-an-x (make-ssnop-bn :y 2))
    (type-error (e) (type-error-expected-type e)))
  ssnop-a::node)
