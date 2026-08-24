;;; Three-argument float arithmetic stays on the native path.
;;;
;;; (+ a b c) means (+ (+ a b) c) (CLHS 12.2), but the tests that decide whether
;;; a float expression can be computed in raw r8/r4 each matched a two-argument
;;; call only. So (* 2.0d0 z2 aux) was not recognized as double-typed, and the
;;; enclosing + fell off the native path with it: the product was computed in raw
;;; doubles, boxed, handed to the general ADD, and unboxed again. The integer
;;; side already left-associates before asking the same question.
;;;
;;; Values checked against SBCL, including the last-digit-sensitive ones: the
;;; native path rounds at the same points the general one does.

(defun %nfa-double (n a b)
  (declare (fixnum n) (double-float a b))
  (let ((s 0.0d0))
    (declare (double-float s))
    (dotimes (i n s) (setq s (+ (* 2.0d0 s a) b 1.0d0)))))

(defun %nfa-single (n a b)
  (declare (fixnum n) (single-float a b))
  (let ((s 0.0))
    (declare (single-float s))
    (dotimes (i n s) (setq s (+ (* 2.0 s a) b 1.0)))))

(defun %nfa-forms (a b c)
  (declare (double-float a b c))
  (list (- a b c) (* a b c) (+ a b c) (- (* a b c))))

(deftest nary-float-arith.values-are-unchanged
  (list (%nfa-double 0 0.5d0 0.25d0) (%nfa-double 1 0.5d0 0.25d0)
        (%nfa-double 3 0.5d0 0.25d0) (%nfa-double 7 0.3d0 0.1d0)
        (%nfa-single 3 0.5 0.25) (%nfa-single 7 0.3 0.1))
  (0.0d0 1.25d0 3.75d0 2.6730176d0 3.75 2.6730177))

(deftest nary-float-arith.three-argument-forms
  (list (%nfa-forms 1.5d0 0.25d0 3.0d0) (%nfa-forms -1.5d0 7.0d0 0.125d0))
  ((-1.75d0 1.125d0 4.75d0 -1.125d0) (-8.625d0 -1.3125d0 5.625d0 1.3125d0)))

;;; The point: no box survives a loop iteration. Difference of two loop lengths,
;;; smallest of several runs (the counter is process-wide).
(defun %nfa-bytes-for (f n a b)
  (let ((best nil))
    (dotimes (r 5 best)
      (let ((before (nth 4 (dotcl:gc-stats))))
        (funcall f n a b)
        (let ((used (- (nth 4 (dotcl:gc-stats)) before)))
          (when (or (null best) (< used best)) (setq best used)))))))

;; Compiled-only: this is a statement about the code the compiler emits. An
;; emit-free build interprets the loop, where the value has no native slot to
;; stay in and the allocation is expected.
(deftest-compiled-only nary-float-arith.loop-allocates-nothing
  (progn (%nfa-bytes-for #'%nfa-double 1000 0.5d0 0.25d0)   ; warm
         (%nfa-bytes-for #'%nfa-single 1000 0.5 0.25)
         (let ((d (- (%nfa-bytes-for #'%nfa-double 400000 0.5d0 0.25d0)
                     (%nfa-bytes-for #'%nfa-double 100000 0.5d0 0.25d0)))
               (s (- (%nfa-bytes-for #'%nfa-single 400000 0.5 0.25)
                     (%nfa-bytes-for #'%nfa-single 100000 0.5 0.25))))
           ;; Boxing the product and the sum was 48 bytes an iteration, so
           ;; 300k extra iterations would show over 14 MB on each.
           (list (< d 100000) (< s 100000))))
  (t t))
