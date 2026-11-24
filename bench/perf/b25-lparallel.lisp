(load "bench/perf/pcommon.lisp")
(setf *bench-group* "25-lparallel")
;; lparallel: pmapcar / preduce / futures with 4 workers over small work items.
;; Libraries to install on the runner: lparallel bordeaux-threads
(load-ql-system :lparallel)
(defvar *sink* nil)
(setf lparallel:*kernel* (lparallel:make-kernel 4))
(defvar *list* (loop for i below 2000 collect i))
(defun work (x) (let ((s 0)) (dotimes (i 200 s) (incf s (logand (* x i) 255)))))
(bench "lparallel:pmapcar 2000 items" 20 (setq *sink* (lparallel:pmapcar #'work *list*)))
(bench "lparallel:preduce + 2000" 20 (setq *sink* (lparallel:preduce #'+ *list*)))
(bench "lparallel future x200" 20
  (setq *sink* (mapcar #'lparallel:force (loop for i below 200 collect (let ((i i)) (lparallel:future (work i)))))))
(bench "lparallel:pmap-into / psort 2000" 20 (setq *sink* (lparallel:psort (copy-seq *list*) #'>)))
(lparallel:end-kernel :wait t)
