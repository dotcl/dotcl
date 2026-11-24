(load "bench/perf/pcommon.lisp")
(setf *bench-group* "9-hash")
(load-ql-system :alexandria)
;; MAPHASH over a 50-entry EQUAL table: a function object (not a literal lambda), a literal lambda (the compiler opens
;; it into a loop), a closure that pushes, and the alexandria wrappers (MAPHASH-KEYS puts a second closure around
;; the user function). mac (dotcl / SBCL): 628 / 211 ns for an empty lambda, hash-table-keys 1472 / 117 ns.
;; Decision: dotcl / SBCL >= 2 on this machine -> work on the per-entry cost of the walk and of the closure call.
(defvar *sink* nil)
(defvar *h* (let ((h (make-hash-table :test 'equal))) (dotimes (i 50) (setf (gethash (format nil "k~D" i) h) i)) h))
(defun f2 (k v) (declare (ignore k v)) nil)
(bench "maphash 50, #'f2 (function object)" 20000 (maphash #'f2 *h*))
(bench "maphash 50, literal lambda, empty" 20000 (maphash (lambda (k v) (declare (ignore k v)) nil) *h*))
(bench "maphash 50, push keys (closure)" 20000
  (setq *sink* (let (keys) (maphash (lambda (k v) (declare (ignore v)) (push k keys)) *h*) keys)))
(bench "loop being the hash-keys, collect" 20000 (setq *sink* (loop for k being the hash-keys of *h* collect k)))
(bench "alexandria:hash-table-keys" 20000 (setq *sink* (alexandria:hash-table-keys *h*)))
(bench "alexandria:hash-table-values" 20000 (setq *sink* (alexandria:hash-table-values *h*)))
(bench "alexandria:hash-table-alist" 20000 (setq *sink* (alexandria:hash-table-alist *h*)))
