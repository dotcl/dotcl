;;; Structures whose raw slots live in fields come in two shapes: raw slots only (up to four, LispStructR) and
;;; up to four slots with raw and boxed ones (LispStructRB). An access compiled for a structure carries the
;;; shape the compiler knows, so reading, writing, TYPEP and COPY-STRUCTURE must give the same answers
;;; in every shape, through a function of the structure's own type and through a general one.

(defstruct (srm-raw (:constructor make-srm-raw (a b))) (a 0 :type fixnum) (b 0.0d0 :type double-float))
(defstruct (srm-mix (:constructor make-srm-mix (x a y))) x (a 0 :type fixnum) y)
(defstruct (srm-mix4 (:constructor make-srm-mix4 (x a y b))) x (a 0 :type fixnum) y (b 0.0d0 :type double-float))
(defstruct (srm-big (:constructor make-srm-big (x a y b z))) x (a 0 :type fixnum) y (b 0.0d0 :type double-float) z)
(defstruct (srm-child (:include srm-mix) (:constructor make-srm-child (x a y z))) z)

(defun srm-read (s) (list (srm-raw-a s) (srm-raw-b s)))
(defun srm-write (s) (setf (srm-raw-a s) 7 (srm-raw-b s) 2.5d0) (srm-read s))
(defun srm-mix-read (s) (list (srm-mix-x s) (srm-mix-a s) (srm-mix-y s)))
(defun srm-mix-write (s) (setf (srm-mix-x s) :x2 (srm-mix-a s) 9 (srm-mix-y s) :y2) (srm-mix-read s))

(deftest struct-rb-representation-mask.raw-only
  (let ((s (make-srm-raw 1 1.5d0)))
    (list (srm-read s) (srm-write s) (typep s 'srm-raw) (typep s 'srm-mix) (srm-raw-a (copy-structure s))))
  ((1 1.5d0) (7 2.5d0) t nil 7))

(deftest struct-rb-representation-mask.mixed
  (let ((s (make-srm-mix :x 1 :y)))
    (list (srm-mix-read s) (srm-mix-write s) (typep s 'srm-mix) (typep s 'srm-raw)
          (srm-mix-a (copy-structure s))))
  ((:x 1 :y) (:x2 9 :y2) t nil 9))

(deftest struct-rb-representation-mask.mixed-four-slots
  (let ((s (make-srm-mix4 :x 3 :y 4.5d0)))
    (list (srm-mix4-x s) (srm-mix4-a s) (srm-mix4-y s) (srm-mix4-b s)
          (progn (setf (srm-mix4-b s) 0.25d0 (srm-mix4-a s) 8) (list (srm-mix4-a s) (srm-mix4-b s)))
          (typep s 'srm-mix4)))
  (:x 3 :y 4.5d0 (8 0.25d0) t))

(deftest struct-rb-representation-mask.five-slots-array-form
  (let ((s (make-srm-big :x 1 :y 2.5d0 :z)))
    (list (srm-big-x s) (srm-big-a s) (srm-big-y s) (srm-big-b s) (srm-big-z s)
          (progn (setf (srm-big-a s) 5) (srm-big-a s)) (typep s 'srm-big)))
  (:x 1 :y 2.5d0 :z 5 t))

(deftest struct-rb-representation-mask.include-child
  (let ((c (make-srm-child :x 1 :y :z)))
    (list (srm-mix-x c) (srm-mix-a c) (srm-mix-y c) (srm-child-z c)
          (typep c 'srm-mix) (typep c 'srm-child) (typep (make-srm-mix :x 1 :y) 'srm-child)
          (srm-mix-read c)))
  (:x 1 :y :z t t nil (:x 1 :y)))

(deftest struct-rb-representation-mask.generic-accessor
  (let ((s (make-srm-mix :x 1 :y)))
    (list (funcall (fdefinition 'srm-mix-a) s) (funcall (fdefinition '(setf srm-mix-a)) 11 s) (srm-mix-a s)
          (slot-value s 'x) (type-of s)))
  (1 11 11 :x srm-mix))
