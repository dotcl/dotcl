(load "bench/perf/pcommon.lisp")
(setf *bench-group* "3-calls")
(defvar *sink* 0)
(defun f-gen (x) x)
(defun f-fix (x) (declare (fixnum x)) x)
(defun f-fix2 (x) (declare (fixnum x)) (the fixnum (1+ x)))
(defun r1 (x) (declare (fixnum x)) (values x))
(defun r3 (x) (declare (fixnum x)) (values x (1+ x) (+ x 2)))
(declaim (notinline f-gen f-fix f-fix2 r1 r3))
(defmacro loop1000 (name &body body) `(bench ,name 50000 (setq *sink* (let ((s 0)) (declare (fixnum s)) (dotimes (i 1000) (setq s (logand (+ s (the fixnum ,@body)) 1023))) s))))
(loop1000 "loop only (no call) x1000" i)
(loop1000 "call untyped defun x1000" (f-gen i))
(loop1000 "call fixnum-declared defun x1000" (f-fix i))
(loop1000 "call fixnum -> fixnum defun x1000" (f-fix2 i))
(loop1000 "call returning 1 value x1000" (r1 i))
(loop1000 "call returning 3 values, first used x1000" (r3 i))
(bench "multiple-value-bind 1 of 3 values x1000" 50000
  (setq *sink* (let ((s 0)) (declare (fixnum s)) (dotimes (i 1000) (multiple-value-bind (a) (r3 i) (setq s (logand (+ s a) 1023)))) s)))
(bench "multiple-value-bind 2 of 3 values x1000" 50000
  (setq *sink* (let ((s 0)) (declare (fixnum s)) (dotimes (i 1000) (multiple-value-bind (a b) (r3 i) (setq s (logand (+ s a b) 1023)))) s)))
(bench "multiple-value-bind 3 of 3 values x1000" 50000
  (setq *sink* (let ((s 0)) (declare (fixnum s)) (dotimes (i 1000) (multiple-value-bind (a b c) (r3 i) (setq s (logand (+ s a b c) 1023)))) s)))
(bench "multiple-value-bind (floor i 7) x1000" 50000
  (setq *sink* (let ((s 0)) (declare (fixnum s)) (dotimes (i 1000) (multiple-value-bind (q r) (floor i 7) (setq s (logand (+ s q r) 1023)))) s)))
