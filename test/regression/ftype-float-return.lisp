;;; A declaimed float return type reaches the native float arithmetic.
;;;
;;; (declaim (ftype (function (...) double-float) f)) is a promise that every
;;; call to F answers a DOUBLE-FLOAT. The fixnum form of the same promise has
;;; long made (f x) an int64 operand; the float forms were recorded and then
;;; never read, so (+ (f a) (f b)) over two declared doubles went through the
;;; generic Runtime.Add. Now such a call is an r8 (or, for SINGLE-FLOAT, r4)
;;; operand: the call, then an unbox of the returned float.
;;;
;;; Only a DECLAIMED return type counts. The compiler also infers a return type
;;; from a DEFUN body, but that guess is not revisited when the function is
;;; redefined, so a caller compiled after the redefinition would unbox the new
;;; return value as the old type. The last tests pin that down.

(setf dotcl:*save-sil* t)

(defun %ffr-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %ffr-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;;; ---- declaimed ----

(declaim (ftype (function (double-float) double-float) %ffr-d))
(defun %ffr-d (x) (declare (double-float x)) (* x 2d0))
(defun %ffr-dsum (a b) (declare (double-float a b)) (+ (%ffr-d a) (%ffr-d b)))

(declaim (ftype (function (single-float) single-float) %ffr-s))
(defun %ffr-s (x) (declare (single-float x)) (* x 2f0))
(defun %ffr-ssum (a b) (declare (single-float a b)) (+ (%ffr-s a) (%ffr-s b)))

;; LONG-FLOAT is the double format here, and a range is still that format.
(declaim (ftype (function (double-float) long-float) %ffr-l))
(defun %ffr-l (x) (declare (double-float x)) (- x))
(declaim (ftype (function (double-float) (double-float 0d0)) %ffr-r))
(defun %ffr-r (x) (declare (double-float x)) (abs x))
(defun %ffr-lr (a) (declare (double-float a)) (* (%ffr-l a) (%ffr-r a)))

;; A declamation that comes after the DEFUN it describes replaces the inferred
;; entry that DEFUN left, and is honored like any other.
(defun %ffr-late (x) (declare (double-float x)) x)
(declaim (ftype (function (double-float) double-float) %ffr-late))
(defun %ffr-late-use (a) (declare (double-float a)) (+ (%ffr-late a) a))

;; A local function under the declaimed name is a call to that function.
(defun %ffr-flet (a)
  (declare (double-float a))
  (flet ((%ffr-d (x) (declare (ignore x)) 7))
    (+ (%ffr-d a) a)))

;;; ---- inferred only ----

;; No declamation: the return type is inferred from the declared parameter,
;; and then the function is redefined to answer an integer.
(defun %ffr-inf (x) (declare (double-float x)) x)
(defun %ffr-inf (x) (declare (ignore x)) 3)
(defun %ffr-inf-use (a) (declare (double-float a)) (+ (%ffr-inf a) a))

;;; ---- values ----

(deftest ftype-float-return.double
  (%ffr-dsum 1d0 2.5d0)
  7d0)

(deftest ftype-float-return.single
  (%ffr-ssum 1f0 2.5f0)
  7f0)

(deftest ftype-float-return.long-and-range
  (%ffr-lr 3d0)
  -9d0)

(deftest ftype-float-return.declaim-after-defun
  (%ffr-late-use 1.5d0)
  3d0)

(deftest ftype-float-return.flet-shadow
  (%ffr-flet 1d0)
  8d0)

(deftest ftype-float-return.redefined-inferred
  (%ffr-inf-use 1d0)
  4d0)

;;; ---- emitted code ----

(deftest-emitting-only ftype-float-return.double-is-r8
  (let ((r (%ffr-sil #'%ffr-dsum)))
    (list (%ffr-count "Runtime.Add" r) (%ffr-count "UNBOX-DOUBLE" r)))
  (0 2))

(deftest-emitting-only ftype-float-return.single-is-r4
  (let ((r (%ffr-sil #'%ffr-ssum)))
    (list (%ffr-count "Runtime.Add" r) (%ffr-count "UNBOX-SINGLE" r)))
  (0 2))

(deftest-emitting-only ftype-float-return.long-and-range-are-r8
  (let ((r (%ffr-sil #'%ffr-lr)))
    (list (%ffr-count "Runtime.Mul" r) (%ffr-count "UNBOX-DOUBLE" r)))
  (0 2))

(deftest-emitting-only ftype-float-return.declaim-after-defun-is-r8
  (%ffr-count "Runtime.Add" (%ffr-sil #'%ffr-late-use))
  0)

(deftest-emitting-only ftype-float-return.flet-shadow-stays-generic
  (let ((r (%ffr-sil #'%ffr-flet)))
    (list (%ffr-count "UNBOX-DOUBLE" r) (plusp (%ffr-count "Runtime.Add" r))))
  (0 t))

(deftest-emitting-only ftype-float-return.inferred-stays-generic
  (let ((r (%ffr-sil #'%ffr-inf-use)))
    (list (%ffr-count "UNBOX-DOUBLE" r) (plusp (%ffr-count "Runtime.Add" r))))
  (0 t))
