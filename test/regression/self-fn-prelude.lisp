;;; Regression tests for the SELF-FN PRELUDE being emitted only when it is read.
;;;
;;; Every compiled function used to open with
;;;   load-sym-pkg NAME PKG / castclass Symbol / GetFunctionBySymbol / stloc
;;; so that a NON-TAIL self-call had a LispFunction to invoke. A tail self-call
;;; becomes a TCO branch and never reads it, and most functions do not call
;;; themselves at all — so for most functions that was a symbol lookup plus a
;;; function lookup on every single entry, and the JIT cannot remove it because
;;; GetFunctionBySymbol is an opaque call.
;;;
;;; Contract: the prelude disappears when the body never loads it, and every
;;; shape that DOES need self (non-tail recursion, tail recursion, special
;;; params, (setf name), mutual recursion) keeps working.

(setf dotcl:*save-sil* t)

(defun %sfp-prelude-p (fn)
  "True when FN's SIL still declares the self-fn local, i.e. still looks the self
   LispFunction up on entry. Keyed on the SELF-FN local rather than on
   GetFunctionBySymbol, which any ordinary call to another function also emits."
  (and (search "SELF-FN" (princ-to-string (dotcl:function-sil fn))) t))

;;; ---- the prelude is gone where nothing reads it ----

(defun %sfp-plain (a b) (+ a b))

(deftest sfp-plain-no-prelude
  (%sfp-prelude-p #'%sfp-plain)
  nil)

(deftest sfp-plain-value
  (%sfp-plain 3 4)
  7)

;; A tail self-call compiles to a TCO branch, not a funcall — still no prelude.
(defun %sfp-tail (n acc)
  (if (<= n 0) acc (%sfp-tail (- n 1) (+ acc n))))

(deftest sfp-tail-no-prelude
  (%sfp-prelude-p #'%sfp-tail)
  nil)

(deftest sfp-tail-value
  (%sfp-tail 100 0)
  5050)

;; Deep enough that a lost TCO would blow the stack.
(deftest sfp-tail-deep
  (%sfp-tail 200000 0)
  20000100000)

;; Calls another function, not itself.
(defun %sfp-caller (n) (%sfp-tail n 0))

(deftest sfp-caller-no-prelude
  (%sfp-prelude-p #'%sfp-caller)
  nil)

(deftest sfp-caller-value
  (%sfp-caller 10)
  55)

;;; ---- every shape that does need self still works ----

;; Non-tail self-call: the self LispFunction is threaded in as a hidden arg0.
(defun %sfp-fact (n)
  (if (= n 0) 1 (* n (%sfp-fact (- n 1)))))

(deftest sfp-fact-value
  (%sfp-fact 10)
  3628800)

(deftest sfp-fact-bignum
  (%sfp-fact 25)
  15511210043330985984000000)

;; Two non-tail self-calls in one body (tree recursion).
(defun %sfp-fib (n)
  (if (< n 2) n (+ (%sfp-fib (- n 1)) (%sfp-fib (- n 2)))))

(deftest sfp-fib-value
  (%sfp-fib 20)
  6765)

;; Non-tail AND tail self-call in the same body — the mix the arg0 threading
;; has to keep straight.
(defun %sfp-mixed (n acc)
  (cond ((<= n 0) acc)
        ((= n 5) (+ 1 (%sfp-mixed (- n 1) acc)))
        (t (%sfp-mixed (- n 1) (+ acc 1)))))

(deftest sfp-mixed-value
  (%sfp-mixed 10 0)
  10)

;; Special-declared parameter: the body goes through the dynamic-binding path,
;; where self cannot be threaded as arg0, so the prelude must survive.
(defvar *sfp-dyn* 0)

(defun %sfp-special (*sfp-dyn*)
  (declare (special *sfp-dyn*))
  (if (= *sfp-dyn* 0) 1 (* *sfp-dyn* (%sfp-special (- *sfp-dyn* 1)))))

(deftest sfp-special-value
  (%sfp-special 6)
  720)

;; (SETF NAME) function that calls itself non-tail: the prelude form that looks
;; up SetfFunction on the target symbol.
(defvar *sfp-place* nil)

(defun (setf %sfp-acc) (v n)
  (if (<= n 0)
      (setq *sfp-place* v)
      (progn (setf (%sfp-acc (- n 1)) (+ v 1)) v)))

(deftest sfp-setf-value
  (progn (setf (%sfp-acc 3) 10) *sfp-place*)
  13)

;; Mutual recursion: neither function reads its own self LispFunction.
(defun %sfp-even (n) (if (= n 0) t (%sfp-odd (- n 1))))
(defun %sfp-odd (n) (if (= n 0) nil (%sfp-even (- n 1))))

(deftest sfp-mutual-no-prelude
  (list (%sfp-prelude-p #'%sfp-even) (%sfp-prelude-p #'%sfp-odd))
  (nil nil))

(deftest sfp-mutual-value
  (list (%sfp-even 10) (%sfp-odd 10))
  (t nil))

;; Redefinition still resolves: a non-tail self-call must reach the CURRENT
;; definition of the function it names, not a stale one captured at entry.
(defun %sfp-redef (n) (if (= n 0) 0 (+ 1 (%sfp-redef (- n 1)))))

(deftest sfp-redef-before
  (%sfp-redef 3)
  3)

(defun %sfp-redef (n) (if (= n 0) 100 (+ 1 (%sfp-redef (- n 1)))))

(deftest sfp-redef-after
  (%sfp-redef 3)
  103)

;; A self-call from INSIDE a closure. The closure compiles to its own method, so
;; it cannot reach a self LispFunction threaded into the outer one as arg0 — it
;; has to resolve the name itself.
(defun %sfp-closure (n)
  (if (= n 0)
      0
      (funcall (lambda (k) (+ 1 (%sfp-closure k))) (- n 1))))

(deftest sfp-closure-value
  (%sfp-closure 3)
  3)

(setf dotcl:*save-sil* nil)
