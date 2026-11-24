;;; (VECTOR ...) is built where it is written, without the call. The arguments are evaluated left
;;; to right, one value each; the result is a fresh simple-vector; NOTINLINE, #'VECTOR, APPLY,
;;; FUNCALL and a local function named VECTOR keep the function.

(defvar *vi-log* nil)
(defun %vi-note (x) (push x *vi-log*) x)

(defun %vi-order ()
  (setq *vi-log* nil)
  (let ((v (vector (%vi-note 1) (%vi-note 2) (%vi-note 3))))
    (list v (reverse *vi-log*))))

(defun %vi-values () (vector (values 1 2 3) (floor 7 2) (values) 5))
(defun %vi-none () (vector))
(defun %vi-one (x) (vector x))
(defun %vi-nine (a) (vector a 1 2 3 4 5 6 7 8))
(defun %vi-twelve () (vector 1 2 3 4 5 6 7 8 9 10 11 12))
(defun %vi-fresh () (vector 1 2))
(defun %vi-notinline (a b)
  (declare (notinline vector))
  (vector a b))
(defun %vi-flet (a)
  (flet ((vector (&rest xs) (list :local xs)))
    (vector a 2)))
(defun %vi-labels-arg (n)
  (labels ((vector (&rest xs) (length xs)))
    (vector n n n)))
(defun %vi-nested (a) (vector (vector a) (vector) (vector (vector a a))))

(defun %vi-l (x) (if (and (vectorp x) (not (stringp x))) (map 'list #'%vi-l x) x))

(deftest vector-inline.order-of-evaluation
  (let ((r (%vi-order))) (list (%vi-l (first r)) (second r)))
  ((1 2 3) (1 2 3)))

(deftest vector-inline.one-value-per-argument
  (list (%vi-l (%vi-values)) (length (%vi-values)))
  ((1 3 nil 5) 4))

(deftest vector-inline.sizes
  (list (%vi-l (%vi-none)) (%vi-l (%vi-one :a)) (%vi-l (%vi-nine 0)) (%vi-l (%vi-twelve))
        (length (%vi-twelve)))
  (nil (:a) (0 1 2 3 4 5 6 7 8) (1 2 3 4 5 6 7 8 9 10 11 12) 12))

(deftest vector-inline.the-result-is-a-plain-simple-vector
  (let ((v (%vi-one 7)) (e (%vi-none)))
    (list (simple-vector-p v) (typep v '(simple-array t (1))) (array-has-fill-pointer-p v)
          (adjustable-array-p v) (array-element-type v) (array-rank v) (length v)
          (simple-vector-p e) (length e)
          (not (eq (%vi-fresh) (%vi-fresh)))
          (equalp (%vi-fresh) (funcall #'vector 1 2))))
  (t t nil nil t 1 1 t 0 t t))

(deftest vector-inline.same-as-the-function
  (list (equalp (%vi-nine 0) (apply #'vector (list 0 1 2 3 4 5 6 7 8)))
        (equalp (%vi-nine 0) (funcall 'vector 0 1 2 3 4 5 6 7 8))
        (equalp (%vi-nested 1) (funcall #'vector (vector 1) (vector) (vector (vector 1 1)))))
  (t t t))

(deftest vector-inline.notinline-and-local-function
  (list (%vi-l (%vi-notinline 1 2)) (%vi-flet 9) (%vi-labels-arg 4))
  ((1 2) (:local (9 2)) 3))

(deftest vector-inline.writes-do-not-alias
  (let ((a (%vi-fresh)) (b (%vi-fresh)))
    (setf (aref a 0) :x)
    (list (%vi-l a) (%vi-l b)))
  ((:x 2) (1 2)))

(deftest vector-inline.argument-error-still-signals
  (handler-case (%vi-one (car 5)) (error (e) (type-of e)))
  type-error)
