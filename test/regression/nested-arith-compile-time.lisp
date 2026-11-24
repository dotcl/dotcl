;;; regression: skip-under-interp
;;; Whether an integer expression can be computed in raw int64 is decided by
;;; FIXNUM-TYPED-P and EXPR-INT-RANGE, which remember their answers per operand
;;; form for the duration of one query. The table was keyed by EQUAL, and an
;;; EQUAL hash looks only a few levels into a list: in a deeply nested
;;; (+ 1 (+ 1 ...)) every operand had the same hash, so each lookup compared
;;; whole trees against each other. Compiling 1,000 levels took 33 seconds.
;;; (Over a variable the operations go through COMPILE-FORM one level each, so
;;; that variant stays under its 500-level limit.)

(defun %nact-nest (op depth leaf)
  (let ((f leaf))
    (dotimes (i depth f)
      (setf f (if (eq op '1+) `(1+ ,f) `(,op 1 ,f))))))

(defmacro %nact (op depth leaf) (%nact-nest op depth leaf))

(defun %nact-plus () (%nact + 1000 0))
(defun %nact-plus-fixnum (x) (declare (fixnum x)) (%nact + 450 x))
(defun %nact-1+ () (%nact 1+ 1000 0))

(deftest nested-arith-compile-time
  (list (%nact-plus) (%nact-plus-fixnum 5) (%nact-1+))
  (1000 455 1000))

;;; Deciding whether an operand of a native int64 operation is straight-line
;;; code asks, at every two-operand LOGAND / LOGIOR / LOGXOR in it, whether the
;;; operands are fixnums. Each of those was a query of its own that walked the
;;; operand's whole tree again, and the decision is made at every level of the
;;; nest: 900 levels took 75 seconds.
(defmacro %nact-mixed (depth)
  (let ((f 'x))
    (dotimes (i depth f)
      (setf f (if (zerop (mod i 3))
                  `(+ 1 (logand ,f 1048575))
                  `(+ 1 ,f))))))

(defun %nact-mixed-fn (x)
  (declare (type (integer 0 1000) x))
  (%nact-mixed 900))

(deftest nested-arith-mixed-logand
  (list (%nact-mixed-fn 0) (%nact-mixed-fn 1000))
  (900 1900))

;;; The answers about operands are kept across the levels of one native int64
;;; chain, but not across a form that is compiled on its own: here the same
;;; operand cons (+ v 1) appears under a FIXNUM V and under a bignum V, and only
;;; the first may be computed in raw int64.
(defmacro %nact-twice-env (form)
  `(+ (let ((v 1)) (declare (fixnum v)) (logand ,form 255))
      (let ((v (expt 2 70))) (logand ,form 255))))

(defun %nact-twice-env-fn () (%nact-twice-env (+ v 1)))

(deftest nested-arith-shared-operand-two-envs
  (%nact-twice-env-fn)
  3)

;;; The tests above only finish; they do not fail when the compile gets slow. This one checks
;;; the growth: compiling a nest of N levels and one of 2N levels should take about twice as
;;; long (linear); a quadratic walk makes it about four times. Absolute times vary with the
;;; machine, a ratio of the minimum of three runs of each does not (the ratio is not
;;; compared with 4 but with 3.5, and 2 is what a linear compile gives). A run is judged by up
;;; to three attempts, as in cond-many-clauses: a quadratic walk gives a ratio near 4 on every
;;; attempt, a stall of the machine in one measurement only spoils that attempt.
(defun %nact-mixed-form (depth)
  (let ((f 'x))
    (dotimes (i depth f)
      (setf f (if (zerop (mod i 3))
                  `(+ 1 (logand ,f 1048575))
                  `(+ 1 ,f))))))

(defun %nact-compile-ms (depth)
  "The shortest of three compiles of a DEPTH-level mixed nest, in internal time units."
  (let ((form `(lambda (x) (declare (type (integer 0 1000) x)) ,(%nact-mixed-form depth)))
        (best nil))
    (dotimes (i 3 best)
      (let ((t0 (get-internal-real-time)))
        (compile nil form)
        (let ((dt (- (get-internal-real-time) t0)))
          (when (or (null best) (< dt best)) (setf best dt)))))))

(defun %nact-growth-ok-p ()
  (%nact-compile-ms 100)                ; the first compile pays for loading the compiler paths
  (dotimes (attempt 3 nil)
    (let* ((small (max 1 (%nact-compile-ms 450)))
           (large (%nact-compile-ms 900)))
      (when (< (/ large small) 3.5) (return t)))))

(deftest-emitting-only nested-arith-mixed-logand-growth
  (%nact-growth-ok-p)
  t)
