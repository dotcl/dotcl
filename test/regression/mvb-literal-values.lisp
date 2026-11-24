;;; (MULTIPLE-VALUE-BIND (A B C) (VALUES X Y) ...) is the pair of LETs it means: the forms of VALUES
;;; are evaluated in order, the variables take them in order, the ones beyond are NIL and the forms
;;; beyond the variables are evaluated and dropped.

(defun %mlv-three (x) (multiple-value-bind (a b c) (values x (1+ x) (+ x 2)) (list a b c)))
(defun %mlv-fewer (x) (multiple-value-bind (a b c) (values x (1+ x)) (list a b c)))
(defun %mlv-more (x) (multiple-value-bind (a) (values x (1+ x) (+ x 2)) (list a)))
(defun %mlv-none () (multiple-value-bind (a b) (values) (list a b)))
(defun %mlv-order (log)
  (multiple-value-bind (a b) (values (progn (push :one log) 1) (progn (push :two log) 2) (progn (push :three log) 3))
    (list a b (reverse log))))
(defun %mlv-scope (x)
  (let ((a 10))
    (multiple-value-bind (a b) (values (+ a x) a) (list a b))))
(defvar *mlv-s* :global)
(defun %mlv-special (x)
  (multiple-value-bind (*mlv-s* b) (values x 2) (list (symbol-value '*mlv-s*) b)))
(defun %mlv-nested (x)
  (multiple-value-bind (a b) (values x (1+ x))
    (multiple-value-bind (c d) (values b a) (list a b c d))))
(defun %mlv-with-declare (x)
  (multiple-value-bind (a b) (values x (* 2 x)) (declare (fixnum a b)) (+ a b)))
(defun %mlv-values-multi (x)
  (multiple-value-bind (a b) (values (floor x 3)) (list a b)))
(defun %mlv-rebound (x)
  (flet ((values (&rest r) (list :mine r))) (multiple-value-bind (a b) (values x 2) (list a b))))

(deftest mvb-literal-values.counts
  (list (%mlv-three 5) (%mlv-fewer 5) (%mlv-more 5) (%mlv-none) (%mlv-values-multi 10))
  ((5 6 7) (5 6 nil) (5) (nil nil) (3 nil)))

(deftest mvb-literal-values.order-and-scope
  (list (%mlv-order nil) (%mlv-scope 5) (%mlv-nested 1) (%mlv-with-declare 4))
  ((1 2 (:one :two :three)) (15 10) (1 2 2 1) 12))

(deftest mvb-literal-values.special-and-rebound
  (list (%mlv-special 7) *mlv-s* (%mlv-rebound 5))
  ((7 2) :global ((:mine (5 2)) nil)))

(deftest mvb-literal-values.no-multiple-values-left-over
  (list (multiple-value-list (multiple-value-bind (a b) (values 1 2) (values a b 3)))
        (multiple-value-list (multiple-value-bind (a b) (values 1 2) (declare (ignore a b)) (values)))
        (let ((n 0)) (dotimes (i 5) (multiple-value-bind (a b) (values i (* i i)) (incf n (+ a b)))) n))
  ((1 2 3) nil 40))
