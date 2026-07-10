;;; Regression tests for NATIVE FLOAT LOCAL SLOTS: a double/single-float
;;; declared, non-special, non-captured lexical with a float-typed init gets a
;;; raw r8/r4 slot instead of a boxed DoubleFloat/SingleFloat. setq stores the
;;; raw float (no per-assignment box), native-float contexts read the slot
;;; directly, and generic reads box on demand. Contract: results identical to
;;; the boxed path, and the per-setq box vanishes from numeric accumulator loops
;;; (fft/mandelbrot's tr/ti/ur/ui). The float analog of *long-locals*.

(setf dotcl:*save-sil* t)

;; Accumulator loop over a double-float local — the box-free proof. The whole
;; float flow is statement-position (returns a fixnum), so the peephole (P6/P7)
;; deletes every DoubleFloat box: no newobj DoubleFloat survives.
(defun %nfl-acc (n)
  (declare (fixnum n))
  (let ((acc 0d0))
    (declare (double-float acc))
    (dotimes (i n) (setq acc (+ acc 1.5d0)))
    (if (> acc 0d0) n 0)))

(deftest nfl-acc-boxfree-sil
  (let ((sil (princ-to-string (dotcl:function-sil #'%nfl-acc))))
    (search "DoubleFloat" sil))
  nil)

(deftest nfl-acc-result
  (%nfl-acc 4)
  4)

;; The value is still correct when read out generically (boxed on read).
(defun %nfl-acc-value (n)
  (declare (fixnum n))
  (let ((acc 0d0))
    (declare (double-float acc))
    (dotimes (i n) (setq acc (+ acc 2d0)))
    acc))

(deftest nfl-acc-value
  (%nfl-acc-value 5)
  10.0d0)

;; single-float native local.
(defun %nfl-single (n)
  (declare (fixnum n))
  (let ((s 0.0))
    (declare (single-float s))
    (dotimes (i n) (setq s (+ s 0.5)))
    s))

(deftest nfl-single-result
  (%nfl-single 6)
  3.0)

;; Native double local passed to a generic function (must box on the read).
(defun %nfl-generic-read (n)
  (declare (fixnum n))
  (let ((acc 0d0))
    (declare (double-float acc))
    (dotimes (i n) (setq acc (+ acc 1d0)))
    ;; list boxes acc; format consumes it — exercises the generic read path
    (car (list acc))))

(deftest nfl-generic-read
  (%nfl-generic-read 3)
  3.0d0)

;; let* sequential native float locals referring to earlier siblings.
(defun %nfl-let-star (x)
  (declare (double-float x))
  (let* ((a (* x 2d0))
         (b (+ a 1d0)))
    (declare (double-float a b))
    (+ a b)))

(deftest nfl-let-star
  (%nfl-let-star 3d0)
  13.0d0)

;; A captured double-float local must stay BOXED (native slot can't be captured
;; by an Object[] env) and remain correct.
(defun %nfl-captured ()
  (let ((acc 1.5d0))
    (declare (double-float acc))
    (let ((f (lambda () (setq acc (+ acc 1d0)) acc)))
      (funcall f)
      (funcall f)
      acc)))

(deftest nfl-captured
  (%nfl-captured)
  3.5d0)

;; Native double local used natively in an array store (composes with):
;; the accumulator is native AND feeds a native float aref store, box-free.
(defun %nfl-array-store (n)
  (declare (fixnum n))
  (let ((c (make-array n :element-type 'double-float))
        (acc 0d0))
    (declare (double-float acc))
    (dotimes (i n)
      (setq acc (+ acc 1d0))
      (setf (aref c i) acc))
    (aref c (1- n))))

(deftest nfl-array-store
  (%nfl-array-store 4)
  4.0d0)

;; Mixed: a double-float local reassigned via a comparison-driven branch stays
;; correct (mutation + native read in a compare).
(defun %nfl-branch (n)
  (declare (fixnum n))
  (let ((m 0d0))
    (declare (double-float m))
    (dotimes (i n)
      (let ((v (* (float i 0d0) 1.5d0)))
        (declare (double-float v))
        (when (> v m) (setq m v))))
    m))

(deftest nfl-branch
  (%nfl-branch 5)
  6.0d0)

(setf dotcl:*save-sil* nil)
