;;; The method EVAL builds for a top level form with no loop calls functions
;;; through LispFunction.InvokeOnceN, which the JIT does not inline. The method
;;; runs once and is JIT-compiled with full optimization; inlining InvokeN
;;; (frame push, try/finally, the direct-delegate arms) into it made a form
;;; like (FOO 1) cost about 0.6 ms of JIT against 0.1 ms without. Compiling a
;;; file evaluates thousands of such forms (DEFMACRO, EVAL-WHEN, the
;;; compile-time side of DEFCLASS and DEFGENERIC).

(defun %eroc-id (&rest xs) xs)

(deftest-emitting-only eval-run-once-calls.no-inlining
  (let* ((type (dotnet:static "System.Type" "GetType" "DotCL.LispFunction, DotCL.Runtime")))
    (loop for n below 9
          always (search "NoInlining"
                         (princ-to-string
                          (dotnet:invoke (dotnet:invoke type "GetMethod" (format nil "InvokeOnce~d" n))
                                         "GetMethodImplementationFlags")))))
  t)

;; Every arity, in statement and tail position.
(deftest eval-run-once-calls.results
  (list (eval '(%eroc-id))
        (eval '(progn (%eroc-id 1) (%eroc-id 1 2)))
        (eval '(%eroc-id 1 2 3))
        (eval '(%eroc-id 1 2 3 4))
        (eval '(%eroc-id 1 2 3 4 5))
        (eval '(%eroc-id 1 2 3 4 5 6))
        (eval '(%eroc-id 1 2 3 4 5 6 7))
        (eval '(%eroc-id 1 2 3 4 5 6 7 8))
        (eval '(%eroc-id 1 2 3 4 5 6 7 8 9))
        (eval '(multiple-value-list (values-list (%eroc-id 1 2)))))
  (nil (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5) (1 2 3 4 5 6) (1 2 3 4 5 6 7)
   (1 2 3 4 5 6 7 8) (1 2 3 4 5 6 7 8 9) (1 2)))

;; A form with a loop keeps the ordinary call: it can be hot.
(deftest eval-run-once-calls.loop
  (eval '(let ((n 0)) (dotimes (i 1000) (setq n (+ n (car (%eroc-id i))))) n))
  499500)
