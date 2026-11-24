(load "bench/perf/pcommon.lisp")
(setf *bench-group* "8-restart")
;; non-local exits through the condition system: the shape cl-json's encode-json-list-guessing-encoder uses
;; (handler-bind whose handler invokes a restart), a return-from out of a handler-bind handler, and handler-case.
;; mac: dotcl 5.2 / 5.4 / 3.9 us, SBCL 0.13 / 0.07 / 0.07 us. The question is how much of that is mac-specific
;; (dyld image lookups during unwinding) and how much is the .NET exception floor on Linux.
(defvar *sink* 0)
(define-condition b8-err (error) ())
(defun r-restart () (restart-case (handler-bind ((b8-err (lambda (e) (declare (ignore e)) (invoke-restart 'r)))) (error 'b8-err)) (r () 1)))
(defun r-return-from () (block b (handler-bind ((b8-err (lambda (e) (declare (ignore e)) (return-from b 1)))) (error 'b8-err))))
(defun r-handler-case () (handler-case (error 'b8-err) (b8-err () 1)))
(defun r-catch-throw () (catch 'b8 (throw 'b8 1)))
(bench "restart-case + invoke-restart from handler-bind" 20000 (setq *sink* (r-restart)))
(bench "return-from out of a handler-bind handler" 20000 (setq *sink* (r-return-from)))
(bench "handler-case" 20000 (setq *sink* (r-handler-case)))
(bench "catch / throw (no condition)" 20000 (setq *sink* (r-catch-throw)))
