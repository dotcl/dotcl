;;; Per kernel: the minimum over the processes, how many processes, and the split
;;; (bench/gate/modes.lisp) as the JSON object bench-state.json keeps.
;;;
;;; Usage: dotcl pairs.lisp FILE
;;;
;;; FILE is the benchmark's stdout: one "name<TAB>milliseconds" line per kernel per
;;; process, the processes concatenated. Output, one line per name:
;;;
;;;     name<TAB>min<TAB>processes<TAB>split-json
;;;
;;; where split-json is {"fast_n": 3, "fast_ms": 39.453, "slow_n": 4, "slow_ms": 57.608} when
;;; the values fall into two groups, and null otherwise. Used by
;;; bench/csharp-parity/make-parity-state.sh.

(load (merge-pathnames "modes.lisp" (or *load-truename* *default-pathname-defaults*)))

(let ((file (first (dotcl:script-arguments))))
  (unless file
    (format *error-output* "Usage: pairs.lisp FILE~%")
    (dotcl:quit 2))
  (dolist (entry (read-name-values file))
    (let* ((m (split-modes (cdr entry))))
      (format t "~A~C~A~C~D~C~A~%"
              (car entry) #\Tab (fixed (modes-min m) 3) #\Tab (modes-n m) #\Tab
              (if (modes-split m)
                  (format nil "{\"fast_n\": ~D, \"fast_ms\": ~A, \"slow_n\": ~D, \"slow_ms\": ~A}"
                          (modes-fast-n m) (fixed (modes-fast-med m) 3)
                          (modes-slow-n m) (fixed (modes-slow-med m) 3))
                  "null")))))
