;;; Regression tests for numeric-array LOCAL INFERENCE: a let binding whose
;;; init is (make-array ... :element-type 'BOUNDED-INTEGER) is proven
;;; numeric-backed, so aref on it with fixnum-typed indices reads/writes the
;;; element as a raw int64 (Runtime.ArefNum*L) and contributes the storage
;;; range to the arithmetic range prover. Contract: results identical to the
;;; boxed path, loud range errors, graceful fallback when the array's backing
;;; changes at runtime (adjust-array :displaced-to), and NO inference for
;;; mutated locals.

(setf dotcl:*save-sil* t)

;; The cl-bench 2d-arrays inner-loop shape, &optional included.
(defun %nai2-bench-2d (&optional (size 8) (runs 2))
  (declare (fixnum size))
  (let ((ones (make-array (list size size) :element-type '(integer 0 1000) :initial-element 1))
        (twos (make-array (list size size) :element-type '(integer 0 1000) :initial-element 2))
        (threes (make-array (list size size) :element-type '(integer 0 2000))))
    (dotimes (runs runs)
      (dotimes (i size)
        (dotimes (j size)
          (setf (aref threes i j)
                (+ (aref ones i j) (aref twos i j))))))
    (aref threes 3 3)))

(deftest nai2-bench-2d-result
  (%nai2-bench-2d)
  3)

;; The whole inner loop must be on the raw-long entries: no boxed Aref2D,
;; no generic Runtime.Add.
(deftest nai2-bench-2d-sil
  (let ((sil (princ-to-string (dotcl:function-sil #'%nai2-bench-2d))))
    (list (notnot (search "ArefNum2DL" sil))
          (notnot (search "ArefSetNum2DL" sil))
          (notnot (search "Runtime.Add" sil))))
  (t t nil))

;; 1D and 3D shapes.
(defun %nai2-1d (n)
  (declare (fixnum n))
  (let ((v (make-array n :element-type '(unsigned-byte 8) :initial-element 3))
        (s 0))
    (dotimes (i n)
      (setf (aref v i) (+ (aref v i) 1))
      (setq s (+ s (aref v i))))
    s))

(deftest nai2-1d
  (%nai2-1d 10)
  40)

(defun %nai2-3d (n)
  (declare (fixnum n))
  (let ((a (make-array (list n n n) :element-type 'fixnum)))
    (dotimes (i n)
      (dotimes (j n)
        (dotimes (k n)
          (setf (aref a i j k) (+ (* i 100) (* j 10) k)))))
    (aref a 2 1 3)))

(deftest nai2-3d
  (%nai2-3d 4)
  213)

;; Range violation through the inferred raw-long store still signals.
(deftest nai2-range-error
  (let ((v (make-array 4 :element-type '(integer 0 2000) :initial-element 0)))
    (handler-case
        (progn (dotimes (i 4) (setf (aref v i) 70000)) :no-error)
      (error () :err)))
  :err)

;; adjust-array to displaced mid-flight: the raw-long entries must fall back
;; to the boxed path and stay correct.
(defun %nai2-displace-flip (flip)
  (let ((base (make-array 8 :element-type '(unsigned-byte 16) :initial-element 5))
        (v (make-array 4 :element-type '(unsigned-byte 16) :initial-element 1 :adjustable t))
        (s 0))
    (when flip
      (adjust-array v 4 :displaced-to base :displaced-index-offset 2))
    (dotimes (i 4)
      (setq s (+ s (aref v i))))
    s))

(deftest nai2-displaced-fallback
  (list (%nai2-displace-flip nil) (%nai2-displace-flip t))
  (4 20))

;; A mutated (setq'd) array local is NOT inferred — and stays correct.
(defun %nai2-mutated (n)
  (declare (fixnum n))
  (let ((v (make-array n :element-type '(unsigned-byte 8) :initial-element 1)))
    (setq v (make-array n :initial-element 10))
    (let ((s 0))
      (dotimes (i n) (setq s (+ s (aref v i))))
      s)))

(deftest nai2-mutated-not-inferred
  (%nai2-mutated 5)
  50)

;;; ---- float-backed inference (single-float→float[], double-float→double[]):
;;; aref reads/writes the element as a native r8 (Runtime.ArefNum*D), and the
;;; inner arithmetic stays on the double-float native path (no DoubleFloat box,
;;; no generic Runtime.Add). phase 2.

;; The fft/daxpy inner-loop shape on double-float arrays.
(defun %naf-daxpy (a b c n)
  (declare (type (simple-array double-float (*)) a b c) (fixnum n))
  (dotimes (i n)
    (setf (aref c i) (+ (aref a i) (* 2d0 (aref b i)))))
  (aref c 0))

(deftest naf-daxpy-result
  (let ((a (make-array 3 :element-type 'double-float :initial-contents '(1d0 2d0 3d0)))
        (b (make-array 3 :element-type 'double-float :initial-contents '(10d0 20d0 30d0)))
        (c (make-array 3 :element-type 'double-float)))
    (%naf-daxpy a b c 3)
    (coerce c 'list))
  (21.0d0 42.0d0 63.0d0))

;; Inner loop must ride the raw-r8 entries: no boxed Aref, no DoubleFloat box,
;; no generic Runtime.Add. The array locals here are the PARAMETERS declared
;; (simple-array double-float (*)), so the read/write route through ArefNum*D
;; via the double-float-typed leaf path.
(defun %naf-daxpy-local (n)
  (declare (fixnum n))
  (let ((a (make-array n :element-type 'double-float :initial-element 2d0))
        (c (make-array n :element-type 'double-float)))
    (dotimes (i n)
      (setf (aref c i) (+ (aref a i) (aref a i))))
    (aref c 0)))

;; ArefNum*D on both sides and NO generic Runtime.Add (the inner-loop add runs
;; on the native r8 path). A newobj DoubleFloat may still appear for the boxed
;; RETURN value (aref c 0) and the literal — only the arithmetic must be raw.
(deftest naf-daxpy-local-sil
  (let ((sil (princ-to-string (dotcl:function-sil #'%naf-daxpy-local))))
    (list (notnot (search "ArefNumD" sil))
          (notnot (search "ArefSetNumD" sil))
          (search "Runtime.Add" sil)))
  (t t nil))

(deftest naf-daxpy-local-result
  (%naf-daxpy-local 4)
  4.0d0)

;; Box-free proof: when the float flow is entirely statement-position (the
;; function returns a fixnum, not a boxed aref), the peephole (P6/P7) deletes
;; every DoubleFloat box — the inner loop is pure native r8, zero heap boxing.
(defun %naf-boxfree (n)
  (declare (fixnum n))
  (let ((a (make-array n :element-type 'double-float :initial-element 2d0))
        (c (make-array n :element-type 'double-float)))
    (dotimes (i n)
      (setf (aref c i) (+ (aref a i) (* 3d0 (aref a i)))))
    n))

(deftest naf-boxfree-sil
  (let ((sil (princ-to-string (dotcl:function-sil #'%naf-boxfree))))
    (list (notnot (search "ArefSetNumD" sil))
          ;; no surviving DoubleFloat box anywhere in the body
          (search "DoubleFloat" sil)))
  (t nil))

(deftest naf-boxfree-result
  (%naf-boxfree 5)
  5)

;; single-float local: raw r8 read narrowed with conv.r4, stored via ArefSetNumD.
(defun %naf-single (n)
  (declare (fixnum n))
  (let ((a (make-array n :element-type 'single-float :initial-element 1.5))
        (c (make-array n :element-type 'single-float)))
    (dotimes (i n)
      (setf (aref c i) (+ (aref a i) (aref a i))))
    (aref c 0)))

(deftest naf-single-sil
  (let ((sil (princ-to-string (dotcl:function-sil #'%naf-single))))
    (list (notnot (search "ArefNumD" sil))
          (notnot (search "ArefSetNumD" sil))))
  (t t))

(deftest naf-single-result
  (%naf-single 3)
  3.0)

;; 2D double-float.
(defun %naf-2d (n)
  (declare (fixnum n))
  (let ((a (make-array (list n n) :element-type 'double-float :initial-element 3d0))
        (b (make-array (list n n) :element-type 'double-float)))
    (dotimes (i n)
      (dotimes (j n)
        (setf (aref b i j) (* (aref a i j) (aref a i j)))))
    (aref b 1 1)))

(deftest naf-2d-sil
  (let ((sil (princ-to-string (dotcl:function-sil #'%naf-2d))))
    (list (notnot (search "ArefNum2DD" sil))
          (notnot (search "ArefSetNum2DD" sil))
          (search "Runtime.Multiply" sil)))
  (t t nil))

(deftest naf-2d-result
  (%naf-2d 3)
  9.0d0)

;; adjust-array to displaced mid-flight: float raw-r8 entries fall back to the
;; boxed path and stay correct.
(defun %naf-displace-flip (flip)
  (let ((base (make-array 8 :element-type 'double-float :initial-element 5d0))
        (v (make-array 4 :element-type 'double-float :initial-element 1d0 :adjustable t))
        (s 0d0))
    (when flip
      (adjust-array v 4 :displaced-to base :displaced-index-offset 2))
    (dotimes (i 4)
      (setf s (+ s (aref v i))))
    s))

(deftest naf-displaced-fallback
  (list (%naf-displace-flip nil) (%naf-displace-flip t))
  (4.0d0 20.0d0))

;;; ---- float-array TYPE DECLARATIONS (params / declared locals), not just
;;; make-array let-locals: (declare (type (simple-array double-float (*)) a))
;;; makes aref on the PARAMETER ride the native r8 path (the fft/mandelbrot
;;; shape). Copy-propagation carries the backing through a bare alias binding
;;; (the Gabriel fft's (prog ((ar areal)) ...)). phase 3.

;; Typed double-float array parameters — array-to-array store, box-free.
(defun %naf-param-daxpy (a b c n)
  (declare (type (simple-array double-float (*)) a b c) (fixnum n))
  (dotimes (i n)
    (setf (aref c i) (+ (aref a i) (aref b i)))))

(deftest naf-param-daxpy-sil
  (let ((sil (princ-to-string (dotcl:function-sil #'%naf-param-daxpy))))
    (list (notnot (search "ArefNumD" sil))
          (notnot (search "ArefSetNumD" sil))
          (search "Runtime.Add" sil)
          ;; no surviving DoubleFloat box (whole flow is statement-position)
          (search "DoubleFloat" sil)))
  (t t nil nil))

(deftest naf-param-daxpy-result
  (let ((a (make-array 3 :element-type 'double-float :initial-contents '(1d0 2d0 3d0)))
        (b (make-array 3 :element-type 'double-float :initial-contents '(10d0 20d0 30d0)))
        (c (make-array 3 :element-type 'double-float)))
    (%naf-param-daxpy a b c 3)
    (coerce c 'list))
  (11.0d0 22.0d0 33.0d0))

;; single-float typed param with explicit dims (the fft (1025) shape).
(defun %naf-param-single (a n)
  (declare (type (simple-array single-float (1025)) a) (fixnum n))
  (dotimes (i n)
    (setf (aref a i) (* 2.0 (aref a i)))))

(deftest naf-param-single-sil
  (let ((sil (princ-to-string (dotcl:function-sil #'%naf-param-single))))
    (list (notnot (search "ArefNumD" sil))
          (notnot (search "ArefSetNumD" sil))))
  (t t))

(deftest naf-param-single-result
  (let ((a (make-array 1025 :element-type 'single-float :initial-element 3.0)))
    (%naf-param-single a 4)
    (list (aref a 0) (aref a 3) (aref a 4)))
  (6.0 6.0 3.0))

;; copy propagation: (prog ((ar a)) ...) aliases the typed param — aref on the
;; alias still routes to the native path (the Gabriel fft shape).
(defun %naf-alias (a n)
  (declare (type (simple-array double-float (*)) a) (fixnum n))
  (prog ((ar a) (i 0))
    (declare (fixnum i))
 loop (when (< i n)
        (setf (aref ar i) (+ (aref ar i) 1d0))
        (setq i (1+ i))
        (go loop))
    (return (aref ar 0))))

(deftest naf-alias-sil
  (let ((sil (princ-to-string (dotcl:function-sil #'%naf-alias))))
    (list (notnot (search "ArefNumD" sil))
          (notnot (search "ArefSetNumD" sil))))
  (t t))

(deftest naf-alias-result
  (let ((a (make-array 3 :element-type 'double-float :initial-contents '(5d0 6d0 7d0))))
    (%naf-alias a 3)
    (coerce a 'list))
  (6.0d0 7.0d0 8.0d0))

;; a caller passing a NON-float-backed array to a float-typed param must not
;; corrupt — the runtime fast path re-checks kind and coerces via the boxed path.
(defun %naf-param-read (a i)
  (declare (type (simple-array double-float (*)) a) (fixnum i))
  (aref a i))

(deftest naf-param-type-mismatch-safe
  ;; general (element-type t) vector holding a double — ArefNumD fast path
  ;; misses (_numKind 0), falls back to boxed coerce.
  (let ((v (make-array 2 :initial-element 0)))
    (setf (aref v 1) 3.5d0)
    (%naf-param-read v 1))
  3.5d0)

(setf dotcl:*save-sil* nil)
