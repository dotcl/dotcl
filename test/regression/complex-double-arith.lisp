;;; Complex arithmetic over double floats computes in the machine's doubles.
;;;
;;; The general complex path builds every intermediate as a boxed number: a
;;; multiply produced four products, a difference and a sum, seven allocations
;;; for one result. When both operands are complex doubles the same formula runs
;;; on raw doubles, rounded at the same points, so the answers below are the ones
;;; the general path gave -- exactly, not approximately. They are written out in
;;; halves and quarters, which are exact in binary, so a mistake shows as a wrong
;;; value rather than a last-digit difference.

(deftest complex-double-arith.multiply
  (list (* #c(1.5d0 -2.25d0) #c(0.5d0 0.25d0))
        (* #c(1.5d0 -2.25d0) #c(1.5d0 -2.25d0))
        (* #c(0.0d0 1.0d0) #c(0.0d0 1.0d0)))
  (#c(1.3125d0 -0.75d0) #c(-2.8125d0 -6.75d0) #c(-1.0d0 0.0d0)))

(deftest complex-double-arith.add-subtract
  (list (+ #c(1.5d0 -2.25d0) #c(0.25d0 0.5d0))
        (- #c(1.5d0 -2.25d0) #c(0.25d0 0.5d0))
        (+ #c(1.5d0 -2.25d0) 0.5d0)
        (- 0.5d0 #c(1.5d0 -2.25d0)))
  (#c(1.75d0 -1.75d0) #c(1.25d0 -2.75d0) #c(2.0d0 -2.25d0) #c(-1.0d0 2.25d0)))

;;; A real double keeps its zero imaginary part as a float, and the result stays
;;; complex: CLHS 12.1.5.3 collapses a zero imaginary part only when it is exact.
(deftest complex-double-arith.real-operand-stays-complex
  (list (complexp (+ #c(1.0d0 0.0d0) 1.0d0))
        (imagpart (+ #c(1.0d0 0.0d0) 1.0d0))
        (complexp (* #c(1.0d0 0.0d0) 2.0d0)))
  (t 0.0d0 t))

;;; Infinities and NaN propagate through the raw path as IEEE says, the same as
;;; through the boxed one.
(deftest complex-double-arith.non-finite
  (let* ((inf (/ 1.0d0 0.0d0))
         (r (* (complex inf 0.0d0) #c(1.0d0 1.0d0))))
    (list (= (realpart r) inf) (= (imagpart r) inf)))
  (t t))

;;; The float comparisons take mixed formats. A literal like 4.0 is a single
;;; float, so comparing a computed double against one is the ordinary case;
;;; it used to fall past the fast path into the general compare.
(deftest complex-double-arith.mixed-format-comparison
  (list (> 4.5d0 4.0) (< 4.5d0 4.0) (>= 4.0d0 4.0) (<= 4.0d0 4.0) (= 4.0d0 4.0)
        (> 4.0 4.5d0) (< 4.0 4.5d0) (= 0.5 0.5d0) (= 0.1 0.1d0))
  (t nil t t t nil t t nil))

;;; NaN is unordered: every comparison with it is false, whichever format it is
;;; compared against.
(deftest complex-double-arith.nan-is-unordered
  (let ((nan (- (/ 1.0d0 0.0d0) (/ 1.0d0 0.0d0))))
    (list (> nan 1.0) (< nan 1.0) (>= nan 1.0) (<= nan 1.0) (= nan 1.0)
          (> 1.0d0 nan) (= nan nan)))
  (nil nil nil nil nil nil nil))

;;; The allocation the raw path removes. Measured as the difference between two
;;; loop lengths so the harness around it cancels out, and as the minimum of
;;; several runs because the counter is process-wide.
(defun %cda-mul (n z)
  (declare (fixnum n))
  (let ((acc z))
    (dotimes (i n acc) (setq acc (* acc #c(1.0000001d0 0.0000001d0))))))

(defun %cda-bytes-for (n)
  (let ((best nil) (z #c(1.0d0 1.0d0)))
    (dotimes (r 5 best)
      (let ((before (nth 4 (dotcl:gc-stats))))
        (%cda-mul n z)
        (let ((used (- (nth 4 (dotcl:gc-stats)) before)))
          (when (or (null best) (< used best)) (setq best used)))))))

;; Compiled-only: this is a statement about the code the compiler emits. An
;; emit-free build interprets the loop, where the value has no native slot to
;; stay in and the allocation is expected.
(deftest-compiled-only complex-double-arith.multiply-allocates-three-objects
  (progn (%cda-bytes-for 1000)              ; warm
         (let ((small (%cda-bytes-for 10000))
               (large (%cda-bytes-for 110000)))
           ;; 100k extra multiplies: one complex and two doubles is 80 bytes
           ;; each, the four discarded products were another 96.
           (< (- large small) 12000000)))
  t)

;;; A real factor scales each part of the complex.
;;;
;;; Running it through the (a+bi)(c+di) form with an imaginary part of zero
;;; multiplies that zero by the other part instead, and 0 * infinity is NaN:
;;; (* 1 #C(inf 0.0)) answered #C(inf NaN). The same cross terms lose a signed
;;; zero, because 0*x + 0*y is +0.0 whatever the signs were. Expected values
;;; from SBCL.
(defun %cda-inf () (/ 1.0d0 0.0d0))
(defun %cda-nan-p (x) (/= x x))

(deftest complex-double-arith.real-factor-keeps-infinity
  (let* ((inf (%cda-inf)) (z (complex inf 0.0d0)))
    (mapcar (lambda (r) (list (= (realpart r) inf) (imagpart r)))
            (list (* 1 z) (* z 1) (* 2 z) (* z 2) (* 1.0d0 z) (* z 1.0d0))))
  ((t 0.0d0) (t 0.0d0) (t 0.0d0) (t 0.0d0) (t 0.0d0) (t 0.0d0)))

;;; Only the part the NaN belongs to becomes NaN.
(deftest complex-double-arith.real-factor-nan-is-local
  (let* ((z (complex (%cda-inf) 0.0d0)) (r (* 0.0d0 z)))
    (list (%cda-nan-p (realpart r)) (imagpart r)))
  (t 0.0d0))

;;; The sign of a zero part survives the multiplication.
(deftest complex-double-arith.real-factor-keeps-signed-zero
  (list (float-sign (imagpart (* 1 #c(1.0d0 -0.0d0))))
        (float-sign (imagpart (* -1 #c(1.0d0 0.0d0))))
        (realpart (* -1 #c(1.0d0 0.0d0))))
  (-1.0d0 -1.0d0 -1.0d0))

;;; Exact arithmetic is unchanged, including the collapse to a real.
(deftest complex-double-arith.real-factor-exact
  (list (* 2 #c(1 2)) (* #c(1 2) 3) (* 0 #c(1 2)) (* 1/2 #c(2 4)))
  (#c(2 4) #c(3 6) 0 #c(1 2)))
