;;; ABS of a declared float computes the magnitude in place.
;;;
;;; The general entry takes a boxed number and returns a fresh box, so
;;; (abs x) on a DOUBLE-FLOAT local boxed its argument, allocated a result, and
;;; then had to be unboxed again by whatever consumed it -- two allocations for
;;; an operation that is one machine instruction. ABS of a float is a float of
;;; the same format, which is exactly the fact that makes the raw form safe.
;;;
;;; Expected values from SBCL, including the signed zero and the extremes.

(defun %nab-double (x) (declare (double-float x)) (abs x))
(defun %nab-single (x) (declare (single-float x)) (abs x))

(defun %nab-in-expression (a b)
  (declare (double-float a b))
  (abs (+ (* a a) (* b b) 1.0d0)))

(defun %nab-loop (n a)
  (declare (fixnum n) (double-float a))
  (let ((s 0.0d0))
    (declare (double-float s))
    (dotimes (i n s) (setq s (+ (abs (- s a)) 0.5d0)))))

(deftest native-float-abs.values
  (list (%nab-double 1.5d0) (%nab-double -1.5d0) (%nab-double 0.0d0)
        (%nab-double most-negative-double-float)
        (%nab-single 1.5) (%nab-single -1.5)
        (%nab-single most-negative-single-float))
  (1.5d0 1.5d0 0.0d0 1.7976931348623157d308
   1.5 1.5 3.4028235e38))

;;; A negative zero comes back as a positive zero, in the argument's format.
(deftest native-float-abs.signed-zero
  (list (float-sign (%nab-double -0.0d0)) (%nab-double -0.0d0)
        (%nab-single -0.0) (type-of (%nab-single -0.0)))
  (1.0d0 0.0d0 0.0 single-float))

;;; Infinity keeps its magnitude; NaN stays unordered with itself.
(deftest native-float-abs.non-finite
  (let* ((inf (/ 1.0d0 0.0d0)) (nan (- inf inf)))
    (list (= (%nab-double inf) inf) (= (%nab-double (- inf)) inf)
          (= (%nab-double nan) (%nab-double nan))))
  (t t nil))

(deftest native-float-abs.inside-an-expression
  (list (%nab-in-expression 1.5d0 -2.0d0) (%nab-in-expression -0.5d0 0.25d0)
        (%nab-loop 0 0.25d0) (%nab-loop 1 0.25d0)
        (%nab-loop 5 0.25d0) (%nab-loop 9 1.5d0))
  (7.25d0 1.3125d0 0.0d0 0.75d0 1.75d0 1.0d0))

;;; No box survives an iteration of a loop whose body takes an ABS.
(defun %nab-bytes-for (n)
  (let ((best nil))
    (dotimes (r 5 best)
      (let ((before (nth 4 (dotcl:gc-stats))))
        (%nab-loop n 0.25d0)
        (let ((used (- (nth 4 (dotcl:gc-stats)) before)))
          (when (or (null best) (< used best)) (setq best used)))))))

;; Compiled-only: this is a statement about the code the compiler emits. An
;; emit-free build interprets the loop, where the value has no native slot to
;; stay in and the allocation is expected.
(deftest-compiled-only native-float-abs.loop-allocates-nothing
  (progn (%nab-bytes-for 1000)              ; warm
         ;; Boxing the argument and the result was 48 bytes an iteration, so
         ;; 300k extra iterations would show over 14 MB here.
         (< (- (%nab-bytes-for 400000) (%nab-bytes-for 100000)) 100000))
  t)
