(load "bench/perf/pcommon.lisp")
(setf *bench-group* "17-serapeum-lists")
;; serapeum list building on a 200000-element list: PARTITION, ASSORT and BATCHES (the rows
;; where dotcl is 6-10 times SBCL in the report this comes from).
;; Libraries to install on the runner: serapeum  (perf.yml: libs=serapeum)
(load-ql-system :serapeum)
(defvar *list* (loop for i below 200000 collect i))
(defvar *sink* nil)
(bench "serapeum partition evenp, 200k list" 20 (setq *sink* (multiple-value-list (serapeum:partition #'evenp *list*))))
(bench "serapeum assort by (mod x 10), 200k list" 20 (setq *sink* (serapeum:assort *list* :key (lambda (x) (mod x 10)))))
(bench "serapeum batches of 100, 200k list" 20 (setq *sink* (serapeum:batches *list* 100)))
