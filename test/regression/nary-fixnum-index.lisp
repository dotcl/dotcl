;;; An n-ary integer expression in an array index is the left-associated form of
;;; the two-argument one, so it belongs on the same unboxed path. The three
;;; places that decide "this can be computed in raw int64" each matched a
;;; two-argument call, so (- n i 1) was not fixnum-typed -- which disqualified
;;; the whole AREF from the native path and boxed the element it read (24 B a
;;; read; the fft benchmark copies elements with exactly this index shape).
;;;
;;; These tests pin the arithmetic, not the representation: the point is that the
;;; folded form computes what the n-ary call means, including overflow into
;;; bignums and left-to-right evaluation.

(defvar *nfi-single* (make-array 8 :element-type 'single-float :initial-element 0.0))
(defvar *nfi-double* (make-array 8 :element-type 'double-float :initial-element 0.0d0))
(defvar *nfi-fix* (make-array 8 :element-type 'fixnum :initial-element 0))

(defun nfi-copy-single (dst src n)
  "Reverse SRC into DST. Separate arrays: copying in place over the whole array
   would read elements this loop has already overwritten."
  (declare (type (simple-array single-float (8)) dst src) (fixnum n))
  (dotimes (i n dst) (setf (aref dst i) (aref src (- n i 1)))))

(defun nfi-sum-at (a n)
  (declare (type (simple-array fixnum (8)) a) (fixnum n))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (i n s) (setq s (+ s (aref a (- n i 1)))))))

(deftest nary-fixnum-index.single-float-copy
  (let ((dst (make-array 8 :element-type (quote single-float) :initial-element 0.0)))
    (dotimes (i 8) (setf (aref *nfi-single* i) (float i 1.0)))
    (nfi-copy-single dst *nfi-single* 8)
    (coerce dst (quote list)))
  (7.0 6.0 5.0 4.0 3.0 2.0 1.0 0.0))

(deftest nary-fixnum-index.fixnum-read
  (progn (dotimes (i 8) (setf (aref *nfi-fix* i) (* i 10)))
         (nfi-sum-at *nfi-fix* 8))
  280)

(deftest nary-fixnum-index.double-float-read
  (progn (dotimes (i 8) (setf (aref *nfi-double* i) (float i 1.0d0)))
         (let ((n 8))
           (declare (fixnum n))
           (list (aref *nfi-double* (- n 1 1)) (aref *nfi-double* (+ 1 2 3)))))
  (6.0d0 6.0d0))

;;; The folded form must mean what the n-ary call means.
(defun nfi-arith (a b c)
  (declare (fixnum a b c))
  (list (- a b c) (+ a b c) (* a b c) (- a b c 1) (+ a b c 1)))

(deftest nary-fixnum-index.arithmetic-agrees
  (nfi-arith 100 7 3)
  (90 110 2100 89 111))

;;; Left to right, and each intermediate is what CL says it is: an n-ary + that
;;; leaves the fixnum range mid-way still gives the exact answer.
(defun nfi-overflow (a b c)
  (declare (integer a b c))
  (+ a b c))

(deftest nary-fixnum-index.intermediate-overflow-is-exact
  (list (nfi-overflow 4611686018427387903 4611686018427387903 2)
        (nfi-overflow most-positive-fixnum 1 (- most-positive-fixnum))
        (* 4611686018427387903 2 2))
  (9223372036854775808 1 18446744073709551612))

(defvar *nfi-order* '())
(defun nfi-note (x) (push x *nfi-order*) x)

(deftest nary-fixnum-index.evaluation-order-left-to-right
  (progn (setf *nfi-order* '())
         (let ((v (- (nfi-note 10) (nfi-note 3) (nfi-note 2))))
           (list v (reverse *nfi-order*))))
  (5 (10 3 2)))
