;;; Direct-delegate stack check regression tests.
;;;
;;; Every per-arity direct-delegate call (_funcN) goes through the InvokeN fast
;;; path, which must run the periodic stack check before dispatching. Both a
;;; closure recursing through its own captured box AND a named simple function
;;; recursing non-tail otherwise never pass a checked entry point, and runaway
;;; recursion kills the process with an uncatchable .NET StackOverflowException
;;; instead of the catchable STORAGE-CONDITION. The check lives at the single
;;; InvokeN choke point so both call shapes are covered.

;; Sanity first: moderate-depth closure self-recursion works normally.
(deftest direct-closure.deep-recursion-sanity
  (let ((f nil))
    (setq f (lambda (n) (if (zerop n) 0 (+ 1 (funcall f (- n 1))))))
    (funcall f 1000))
  1000)

;; Overflow depth: must surface as a catchable STORAGE-CONDITION, not process
;; death. It must NOT be an ERROR: the spec puts storage-condition under
;; serious-condition but outside error (SBCL's control-stack-exhausted behaves
;; the same), so (error ...) clauses are transparent to it.
(deftest direct-closure.stack-overflow-catchable
  (let ((f nil))
    (setq f (lambda (n) (if (zerop n) 0 (+ 1 (funcall f (- n 1))))))
    (handler-case (progn (funcall f 10000000) :no-overflow)
      (error () :caught-error)
      (storage-condition () :caught-storage-condition)))
  :caught-storage-condition)

;; Named simple function (assembler-installed _funcN): moderate-depth non-tail
;; self-recursion works normally.
(defun %named-deep-rec (n) (if (zerop n) 0 (+ 1 (%named-deep-rec (- n 1)))))

(deftest direct-named.deep-recursion-sanity
  (%named-deep-rec 1000)
  1000)

;; Named simple function overflow: deep non-TCO recursion must surface as a
;; catchable STORAGE-CONDITION, not process death via raw StackOverflowException.
(deftest direct-named.stack-overflow-catchable
  (handler-case (progn (%named-deep-rec 10000000) :no-overflow)
    (storage-condition () :caught-storage-condition))
  :caught-storage-condition)

;;; apply chain: Runtime.Apply frames are fatter than plain InvokeN calls, and
;;; the signal machinery (Signal -> Typep handler matching) runs ON TOP of the
;;; exhausted stack. With only the fixed ~64KB probe headroom the signal path
;;; itself died as a fatal StackOverflowException (4/4 process death). The
;;; periodic check now probes with an extra padded margin so the condition
;;; system has room to run.
(defun %apply-chain-rec (n) (+ 1 (apply #'%apply-chain-rec (list (- n 1)))))

(deftest apply-chain.stack-overflow-catchable
  (handler-case (progn (%apply-chain-rec 10000000) :no-overflow)
    (error () :caught-error)
    (storage-condition () :caught-storage-condition))
  :caught-storage-condition)
