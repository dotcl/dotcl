;;; Regression: a function body compiled inside another body is a separate CLR
;;; method, so it must not inherit the enclosing body's compile context — the
;;; same rule the closure boundary follows.
;;;
;;; Most per-compilation state now travels in the *CSTATE* pack: the closure
;;; boundary resets it to the empty pack via the registry, and the function-body
;;; paths build theirs from CSTATE-FRESH-FUNCTION-BODY (fresh except the
;;; tco-self-symbol / tco-local-fn-key handoff the caller passes in — the
;;; self-call fast path reads those on purpose). A new pack slot therefore
;;; participates in every reset by construction. The behaviour tests below pin
;;; the rule; the registry test at the bottom pins the few remaining
;;; non-pack specials.

(defpackage :fbr (:use))

;;; --- what gets compiled inside what -------------------------------------
;;; Each outer function has an inner DEFUN in its body, so the inner body is
;;; compiled while the outer body's state is live.

(defun fbr-outer-handler-case ()
  (handler-case
      (progn (defun fbr-inner-hc (x) (if (> x 0) (fbr-inner-hc (- x 1)) :done))
             :defined)
    (error (e) (list :err e))))

(defun fbr-outer-unwind ()
  (unwind-protect
       (progn (defun fbr-inner-uw (x) (if (> x 0) (fbr-inner-uw (- x 1)) :done))
              :defined)
    nil))

(defun fbr-outer-typed (n)
  (declare (fixnum n))
  (defun fbr-inner-typed (n) (list n))
  (+ n 1))

(defun fbr-outer-native (d)
  (declare (double-float d))
  (defun fbr-inner-native (d) (list d))
  (* d 2))

(defun fbr-outer-labels (n)
  (labels ((helper (x) (* x 2)))
    (defun fbr-inner-labels (x) (list x))
    (helper n)))

(defun fbr-outer-block (n)
  (block outer
    (defun fbr-inner-block (x) (list x))
    (return-from outer (* n 3))))

;;; The outer forms must still work, and each inner function must be compiled
;;; from its own context (its parameter is undeclared, so a non-fixnum /
;;; non-number argument must survive).
(deftest fbr-defun-in-handler-case      (fbr-outer-handler-case) :defined)
(deftest fbr-defun-in-handler-case-runs (fbr-inner-hc 3)         :done)
(deftest fbr-defun-in-unwind-protect      (fbr-outer-unwind) :defined)
(deftest fbr-defun-in-unwind-protect-runs (fbr-inner-uw 3)   :done)

(deftest fbr-defun-in-fixnum-scope       (fbr-outer-typed 5)          6)
(deftest fbr-defun-in-fixnum-scope-inner (fbr-inner-typed 2.5d0)      (2.5d0))
(deftest fbr-defun-in-double-scope       (fbr-outer-native 2.5d0)     5.0d0)
(deftest fbr-defun-in-double-scope-inner (fbr-inner-native "x")       ("x"))

(deftest fbr-defun-in-labels       (fbr-outer-labels 4)  8)
(deftest fbr-defun-in-labels-inner (fbr-inner-labels :s) (:S))
(deftest fbr-defun-in-block        (fbr-outer-block 3)   9)
(deftest fbr-defun-in-block-inner  (fbr-inner-block :t)  (:T))

;;; Self-recursion in a function defined inside a try region must still be
;;; tail-optimized: the enclosing method's try context is not this method's, so
;;; it must not suppress the TCO branch. Without the reset this recurses for
;;; real and exhausts the stack.
(deftest fbr-tco-inside-try-region (fbr-inner-hc 200000) :done)

;;; --- drift tripwire ------------------------------------------------------
;;; The closure boundary picks up new state automatically; the two function-body
;;; paths do not. If this list changes, decide what compile-function-body-inner
;;; and compile-function-body-direct (cil-forms.lisp) must do with the new
;;; variable, then update this list.
;;; The registry-mechanism test registers dummy state of its own, so those are
;;; filtered out here — this list is about the compiler's real state.
(defun fbr-registered-state-names ()
  (let ((s (find-symbol "*CLOSURE-FRESH-STATE*" "DOTCL-INTERNAL")))
    (when (and s (boundp s))
      (sort (remove-if (lambda (name) (search "DUMMY" name))
                       (mapcar (lambda (e) (symbol-name (car e))) (symbol-value s)))
            #'string<))))

(deftest fbr-compile-state-registry-unchanged
  (fbr-registered-state-names)
  ("*CSTATE*" "*IN-FINALLY-BLOCK*" "*IN-TAIL-POSITION*" "*IN-TRY-BLOCK*"))
