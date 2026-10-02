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

;;; Recursion that goes Lisp -> .NET delegate -> Lisp at every level. Reflection
;;; wraps a faulting call's exception by catching and throwing a new one, and the
;;; handler that undid that wrap re-threw with ExceptionDispatchInfo: two restarted
;;; exception dispatches per level, each leaving a live handler funclet behind
;;; while the exception kept travelling. The STORAGE-CONDITION raised at the bottom
;;; therefore ran out of stack on its way out and became a fatal .NET
;;; StackOverflowException. Invoking without the wrap, and converting a genuine
;;; .NET failure only at the level where it originated, leaves the crossing free.
(defvar *cb-chain-fn* nil)

(defun %cb-chain-rec (n)
  (if (zerop n) 0 (+ 1 (dotnet:invoke *cb-chain-fn* "Invoke" (- n 1)))))

(setq *cb-chain-fn*
      (dotnet:make-delegate "System.Func`2[System.Int32,System.Int32]"
                            (lambda (x) (%cb-chain-rec x))))

(deftest callback-chain.moderate-depth-sanity
  (%cb-chain-rec 100)
  100)

(deftest callback-chain.stack-overflow-catchable
  (handler-case (progn (%cb-chain-rec 10000000) :no-overflow)
    (error () :caught-error)
    (storage-condition () :caught-storage-condition))
  :caught-storage-condition)

;;; Stack exhaustion inside a callback reaches the code that called into .NET.
;;; The callback boundary keeps Lisp ERRORs out of the host, answering it with the
;;; return type's default. It also kept in a STORAGE-CONDITION, which is not an
;;; error: the delegate returned 0 and whatever called it carried on as if nothing
;;; had happened. A Lisp handler for the condition hides this -- it is found when
;;; the condition is signalled, before the unwind reaches the boundary -- unless
;;; the signalling itself runs out of stack, which is what made CALLBACK-CHAIN.
;;; STACK-OVERFLOW-CATCHABLE answer :NO-OVERFLOW now and then (emit-free, where
;;; the handler matching is interpreted and its frames are larger: about 1 in 120
;;; when the same test is started at 120 different stack depths).
;;;
;;; Here the handler for STORAGE-CONDITION is made invisible to the signalling on
;;; purpose: the callback runs inside a handler of another condition, and while a
;;; handler runs only the handlers established outside it are active. So the
;;; condition goes unhandled at the signal and travels as an unwind -- the same
;;; path the flaky case took -- and the HANDLER-CASE meets it on the way out.
(defun %in-callback-deep-rec (n) (if (zerop n) 0 (+ 1 (%in-callback-deep-rec (- n 1)))))

(define-condition %run-callback (condition) ())

(deftest callback-boundary.storage-condition-passes
  (let ((d (dotnet:make-delegate "System.Func`1[System.Int32]"
                                 (lambda () (%in-callback-deep-rec 10000000)))))
    (handler-bind ((%run-callback (lambda (c) (declare (ignore c)) (dotnet:invoke d "Invoke"))))
      (handler-case (progn (signal '%run-callback) :no-overflow)
        (storage-condition () :caught-storage-condition))))
  :caught-storage-condition)

;;; An ERROR in a callback is still contained, as before.
(deftest callback-boundary.error-still-contained
  (let ((d (dotnet:make-delegate "System.Func`1[System.Int32]"
                                 (lambda () (error "boom in callback"))))
        (*error-output* (make-broadcast-stream)))
    (handler-case (dotnet:invoke d "Invoke")
      (error () :escaped)))
  0)
