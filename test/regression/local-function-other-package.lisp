;;; An FLET/LABELS binding shadows its own function name, not every symbol with
;;; the same name. magicl-tests binds MAGICL-TESTS::MULT with LABELS and calls
;;; MAGICL:MULT with keyword arguments inside it; the call went to the local
;;; function ("wrong number of arguments: 4 (expected 2)").

(defpackage :lfop-a (:use :cl) (:export #:mult))
(defpackage :lfop-b (:use :cl))
(in-package :lfop-a)
(defun mult (a b &key (x 0)) (list :global a b x))
(in-package :cl-user)

(deftest local-function-other-package.flet
  (flet ((lfop-b::mult (a b) (list :local a b)))
    (list (lfop-b::mult 1 2) (lfop-a:mult 1 2 :x 3)))
  ((:local 1 2) (:global 1 2 3)))

(deftest local-function-other-package.labels
  (labels ((lfop-b::mult (a b) (if (> a 0) (lfop-b::mult (1- a) b) (list :local a b))))
    (list (lfop-b::mult 2 5) (lfop-a:mult 1 2 :x 3)))
  ((:local 0 5) (:global 1 2 3)))

(deftest local-function-other-package.closure
  (labels ((lfop-b::mult (a b) (list :local a b)))
    (mapcar (lambda (n) (list (lfop-b::mult n n) (lfop-a:mult n n :x n))) '(1 2)))
  (((:local 1 1) (:global 1 1 1)) ((:local 2 2) (:global 2 2 2))))

(deftest local-function-other-package.function
  (flet ((lfop-b::mult (a b) (list :local a b)))
    (list (funcall #'lfop-b::mult 1 2) (funcall #'lfop-a:mult 1 2 :x 3)))
  ((:local 1 2) (:global 1 2 3)))

(deftest local-function-other-package.setf
  (let ((cell (list 0)))
    (flet (((setf lfop-b::mult) (v c) (setf (car c) (list :local v))))
      (setf (lfop-b::mult cell) 1)
      (car cell)))
  (:local 1))
