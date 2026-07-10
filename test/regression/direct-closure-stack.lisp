;;; Direct-closure stack check regression tests.
;;;
;;; The per-arity direct delegates (_funcN) of closures must include the
;;; periodic stack check: a closure recursing through its own captured box
;;; (funcall of a self-reference) otherwise never passes a checked entry
;;; point and runaway recursion kills the process with an uncatchable .NET
;;; StackOverflowException instead of the catchable PROGRAM-ERROR.

;; Sanity first: moderate-depth closure self-recursion works normally.
(deftest direct-closure.deep-recursion-sanity
  (let ((f nil))
    (setq f (lambda (n) (if (zerop n) 0 (+ 1 (funcall f (- n 1))))))
    (funcall f 1000))
  1000)

;; Overflow depth: must surface as a catchable PROGRAM-ERROR, not process death.
(deftest direct-closure.stack-overflow-catchable
  (let ((f nil))
    (setq f (lambda (n) (if (zerop n) 0 (+ 1 (funcall f (- n 1))))))
    (handler-case (progn (funcall f 10000000) :no-overflow)
      (program-error () :caught-program-error)))
  :caught-program-error)
