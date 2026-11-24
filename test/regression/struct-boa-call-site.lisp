;;; A BOA constructor with only required parameters is built at the call site, and an
;;; inline function's double result takes a native slot in a LET variable.

(defstruct (bv (:constructor bv (z x y)))
  (x 0d0 :type double-float) (y 0d0 :type double-float) (z 0d0 :type double-float))

(defun %bv-parts (v) (list (bv-x v) (bv-y v) (bv-z v)))

(deftest struct-boa-call-site.slots-by-parameter
  (%bv-parts (bv 3d0 1d0 2d0))
  (1d0 2d0 3d0))

(deftest struct-boa-call-site.evaluation-order
  (let ((log '()))
    (bv (progn (push :z log) 3d0) (progn (push :x log) 1d0) (progn (push :y log) 2d0))
    (reverse log))
  (:z :x :y))

(defun %bv-make (a b c)
  (declare (double-float a b c))
  (bv c a b))

(deftest struct-boa-call-site.typed-arguments
  (%bv-parts (%bv-make 1d0 2d0 3d0))
  (1d0 2d0 3d0))

(deftest struct-boa-call-site.typed-slot-checks
  ;; An argument not of the slot's type is still a type error.
  (handler-case (progn (bv 1 2d0 3d0) :no-error)
    (type-error () :type-error))
  :type-error)

(defstruct (bw (:constructor bw (a))) (a 0 :type fixnum) (b 7))

(deftest struct-boa-call-site.omitted-slot-takes-initform
  (let ((o (bw 5))) (list (bw-a o) (bw-b o)))
  (5 7))

(declaim (inline %dd))
(defun %dd (a b) (+ (bv-x a) (bv-x b)))

(defun %dd-user (a b)
  (let ((s (%dd a b)))
    (declare (double-float s))
    (* s 2d0)))

(deftest struct-boa-call-site.inline-double-result-in-let
  (%dd-user (bv 0d0 1.5d0 0d0) (bv 0d0 2.5d0 0d0))
  8d0)

(declaim (inline %ident))
(defun %ident (a) a)

(defun %ident-user (x)
  (let ((s (%ident x)))
    (declare (double-float s))
    (+ s 1d0)))

(deftest-emitting-only struct-boa-call-site.inline-untyped-result-still-checked
  (list (%ident-user 1d0)
        (handler-case (%ident-user 3) (type-error () :type-error)))
  (2d0 :type-error))
