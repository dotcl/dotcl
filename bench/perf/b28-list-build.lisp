(load "bench/perf/pcommon.lisp")
(setf *bench-group* "28-list-build")
;; Building lists: PUSH of a counter and of a constant, CONS dropped at once, COPY-LIST,
;; PUSH / POP, NREVERSE, and a counter boxed and dropped. Each body runs 200000 operations,
;; in one compiled function, so a Fixnum above the small-integer cache is a fresh box in
;; some of them and a shared one in others.
(defvar *sink* nil)
(defvar *list* (let ((r nil)) (dotimes (i 200000) (push i r)) r))
(bench "push j onto a live list x200000" 20
  (let ((acc nil)) (dotimes (j 200000) (push j acc)) (setq *sink* acc)))
(bench "push a constant onto a live list x200000" 20
  (let ((acc nil)) (dotimes (j 200000) (push 'a acc)) (setq *sink* acc)))
(bench "cons dropped at once x200000" 20
  (let ((acc nil)) (dotimes (j 200000) (setq acc (cons 'a nil))) (setq *sink* acc)))
(bench "fixnum j + 100000 boxed and dropped x200000" 20
  (let ((x nil)) (dotimes (j 200000) (setq x (+ j 100000)) (setq *sink* x))))
(bench "copy-list of 200000" 20
  (setq *sink* (copy-list *list*)))
(bench "push (pop l) onto a second list x200000" 20
  (let ((acc nil) (l *list*)) (loop (when (null l) (return)) (push (pop l) acc)) (setq *sink* acc)))
(bench "nreverse of a copy, 200000" 20
  (setq *sink* (nreverse (copy-list *list*))))
