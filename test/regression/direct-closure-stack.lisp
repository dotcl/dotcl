;;; Direct-delegate stack check regression tests.
;;;
;;; Every per-arity direct-delegate call (_funcN) goes through the InvokeN fast
;;; path, which must run the periodic stack check before dispatching. Both a
;;; closure recursing through its own captured box AND a named simple function
;;; recursing non-tail otherwise never pass a checked entry point, and runaway
;;; recursion kills the process with an uncatchable .NET StackOverflowException
;;; instead of the catchable PROGRAM-ERROR. The check lives at the single
;;; InvokeN choke point so both call shapes are covered.

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

;; Named simple function (assembler-installed _funcN): moderate-depth non-tail
;; self-recursion works normally.
(defun %named-deep-rec (n) (if (zerop n) 0 (+ 1 (%named-deep-rec (- n 1)))))

(deftest direct-named.deep-recursion-sanity
  (%named-deep-rec 1000)
  1000)

;; Named simple function overflow: deep non-TCO recursion must surface as a
;; catchable PROGRAM-ERROR, not process death via raw .NET StackOverflowException.
(deftest direct-named.stack-overflow-catchable
  (handler-case (progn (%named-deep-rec 10000000) :no-overflow)
    (program-error () :caught-program-error))
  :caught-program-error)
