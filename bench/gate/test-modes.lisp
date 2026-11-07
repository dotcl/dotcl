;;; Self-test for the split rule in bench/gate/modes.lisp, on fixed per-process values.
;;;
;;; Usage: dotcl test-modes.lisp
;;;
;;; The first six cases are per-process times (ms, one value per process) of one
;;; kernel, taken from an A/B on a Linux x64 machine (Intel i5-8350U) where the
;;; same binary ran some kernels at two speeds depending on the process. They are
;;; here so the rule is held to the data it was written against: the three split
;;; cases must stay split and the three noisy one-group cases must stay one group.
;;; Runs in milliseconds, so `make bench-parity` runs it first: a detector that has
;;; stopped working would otherwise just print "one group".

(load (merge-pathnames "modes.lisp" (or *load-truename* *default-pathname-defaults*)))

;;; (expected label values...)
(defparameter *cases*
  '(("split 1/6" "array-walk, one fast process" 57.839 58.000 57.916 40.097 57.790 57.190 57.837)
    ("split 3/4" "array-walk, three fast processes" 57.516 39.422 57.473 39.585 57.845 57.700 39.453)
    ("split 1/6" "string-walk, one fast process" 175.795 176.078 176.120 175.988 103.283 175.908 177.105)
    ("one" "array-walk, one group with a 6% high value" 38.778 38.667 38.194 39.370 38.598 41.161 38.576)
    ("one" "struct-slots, one slow process (interference)" 113.612 129.409 113.507 113.195 113.636 113.492 113.980)
    ("one" "fib, wide spread without a gap" 255.327 272.114 282.445 255.577 276.052 256.695 256.074)
    ("one" "identical values" 10.000 10.000 10.000 10.000)
    ("split 2/2" "two clean groups of two" 10.0 10.1 15.0 15.1)
    ("one" "one slow process of four is not a group" 10.0 10.1 10.2 15.0)
    ("unchecked" "two values are not checked" 10.0 20.0)))

(let ((bad 0))
  (format t "=== split detection self-test (bench/gate/modes.lisp) ===~%")
  (dolist (case *cases*)
    (destructuring-bind (expected label &rest values) case
      (let* ((m (split-modes values))
             (got (cond ((not (modes-checked m)) "unchecked")
                        ((modes-split m) (format nil "split ~D/~D" (modes-fast-n m) (modes-slow-n m)))
                        (t "one"))))
        (unless (string= got expected) (incf bad))
        (format t "  ~A ~A expected ~A got ~A~%"
                (if (string= got expected) "ok  " "FAIL")
                (pad-right label 48) (pad-right expected 10) got))))
  (when (plusp bad)
    (format *error-output* "test-modes: the split rule no longer classifies the recorded cases as expected.~%")
    (dotcl:quit 1))
  (format t "test-modes: ok~%"))
