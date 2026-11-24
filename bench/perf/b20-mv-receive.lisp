(load "bench/perf/pcommon.lisp")
(setf *bench-group* "20-mv-receive")
;; Receiving multiple values: MULTIPLE-VALUE-BIND on a user function that returns 1, 2 or 3 values,
;; on standard functions that return two values (FLOOR, GETHASH, PARSE-INTEGER), and the other
;; consumers (MULTIPLE-VALUE-LIST, MULTIPLE-VALUE-CALL, MULTIPLE-VALUE-PROG1). The loop bodies
;; are compiled into the one function BENCH-MV, so the whole file is measured the way a library's
;; compiled code is.
(declaim (notinline mv2 mv3 mv1 mv0))
(defun mv2 (a b) (values (+ a b) a))
(defun mv3 (a b) (values a b (+ a b)))
(defun mv1 (a b) (+ a b))
(defun mv0 (a b) (declare (ignore a b)) (values))
(defvar *ht* (let ((h (make-hash-table))) (setf (gethash 5 h) 6) h))
(defvar *sink* 0)
(defmacro mv-bench (name &body body)
  `(bench ,name 200000
     (let ((x 0))
       (declare (fixnum x))
       (dotimes (i 50) ,@body)
       (setq *sink* x))))
(mv-bench "mvb 2 vars, function returning 2 values"
  (setq x (logand (multiple-value-bind (a b) (mv2 x i) (+ a b)) 1023)))
(mv-bench "mvb 1 var, function returning 2 values"
  (setq x (logand (multiple-value-bind (a) (mv2 x i) a) 1023)))
(mv-bench "mvb 3 vars, function returning 2 values (one missing)"
  (setq x (logand (multiple-value-bind (a b c) (mv2 x i) (+ a b (or c 0))) 1023)))
(mv-bench "mvb 3 vars, function returning 3 values"
  (setq x (logand (multiple-value-bind (a b c) (mv3 x i) (+ a b c)) 1023)))
(mv-bench "mvb 2 vars, function returning 1 value"
  (setq x (logand (multiple-value-bind (a b) (mv1 x i) (+ a (or b 0))) 1023)))
(mv-bench "mvb 2 vars, function returning 0 values"
  (setq x (logand (multiple-value-bind (a b) (mv0 x i) (+ x (or a 0) (or b 0))) 1023)))
(mv-bench "primary of a function returning 2 values"
  (setq x (logand (mv2 x i) 1023)))
(mv-bench "mvb floor"
  (setq x (logand (multiple-value-bind (q r) (floor (+ x i 100) 7) (+ q r)) 1023)))
(mv-bench "floor, primary only"
  (setq x (logand (floor (+ x i 100) 7) 1023)))
(mv-bench "mvb gethash"
  (setq x (logand (multiple-value-bind (v p) (gethash 5 *ht*) (if p v 0)) 1023)))
(mv-bench "mvb parse-integer"
  (setq x (logand (multiple-value-bind (v p) (parse-integer "123") (+ v p)) 1023)))
(mv-bench "multiple-value-list of a 2-value function"
  (setq x (logand (length (multiple-value-list (mv2 x i))) 1023)))
(mv-bench "multiple-value-call + on a 2-value function"
  (setq x (logand (multiple-value-call #'+ (mv2 x i)) 1023)))
(mv-bench "multiple-value-prog1 of a 2-value function"
  (setq x (logand (multiple-value-prog1 (mv2 x i) (setq x 0)) 1023)))
