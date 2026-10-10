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
