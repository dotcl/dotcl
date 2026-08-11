;;; MACROLET shadows a global macro, and NIL / T are legal in operator position.
;;;
;;; %MINI-EVAL's MACROLET registered its expanders only in *MACROS* (the
;;; compiler's macro table). But operator dispatch goes through MACROEXPAND-1
;;; first, and that consults the RUNTIME macro table before *MACROS*. So whenever
;;; a global macro of the same name existed it always won and the MACROLET had no
;;; effect (ansi-test MACROLET.50, and the MACROLET.16 entries naming CL macros).
;;; For a name with no global definition the lookup fell through to *MACROS* and
;;; worked — so this broke ONLY for names that were already defined, which is
;;; what made it hard to see.
;;;
;;; The same path also lost NIL and T as operators: neither is an instance of the
;;; Symbol class here, so MACROEXPAND-1 does not treat the form as a macro call,
;;; and the fall-through called (SYMBOL-FUNCTION NIL), a type-error
;;; (ansi-test MACROLET.15).
;;;
;;; The fix also records MACROLET bindings in their own namespace in ENV
;;; (%MINI-MACROS) and has operator dispatch consult it BEFORE MACROEXPAND-1 —
;;; the same shape FLET bindings (%MINI-FDEFN) and GO tags (%MINI-GO-TAGS) use.

(defmacro %mls-global () :bad)
(defun %mls-global-fn () :bad)

(defun %mls (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (type-of e) (princ-to-string e))))))

;;; --- shadowing a global macro (ansi MACROLET.50)

(deftest interp-macrolet-shadowing.shadows-global-macro-compile
  (%mls :compile '(macrolet ((%mls-global () :good)) (%mls-global)))
  :good)

(deftest interp-macrolet-shadowing.shadows-global-macro-interpret
  (%mls :interpret '(macrolet ((%mls-global () :good)) (%mls-global)))
  :good)

;;; shadowing a global FUNCTION already worked (different path); asserted as a
;;; pair so a half-fixed state is visible
(deftest interp-macrolet-shadowing.shadows-global-function-interpret
  (%mls :interpret '(macrolet ((%mls-global-fn () :good)) (%mls-global-fn)))
  :good)

;;; --- NIL / T in operator position (ansi MACROLET.15)

(deftest interp-macrolet-shadowing.nil-operator-compile
  (%mls :compile '(macrolet ((nil () ''a)) (nil)))
  a)

(deftest interp-macrolet-shadowing.nil-operator-interpret
  (%mls :interpret '(macrolet ((nil () ''a)) (nil)))
  a)

(deftest interp-macrolet-shadowing.t-operator-interpret
  (%mls :interpret '(macrolet ((t () ''a)) (t)))
  a)

;;; --- over-fix guards ---------------------------------------------------

;;; a MACROLET's scope is its body: the global comes back on the way out
(deftest interp-macrolet-shadowing.scope-ends-interpret
  (%mls :interpret '(list (macrolet ((%mls-global () :good)) (%mls-global))
                     (%mls-global)))
  (:good :bad))

;;; an inner MACROLET shadows an outer one
(deftest interp-macrolet-shadowing.nested-inner-wins-interpret
  (%mls :interpret '(macrolet ((%m () :outer))
                     (list (macrolet ((%m () :inner)) (%m))
                           (%m))))
  (:inner :outer))

;;; an inner FLET shadows an outer MACROLET (CLHS 3.1.2.1.2.4) — this rejects a
;;; fix that merely swaps the precedence
(deftest interp-macrolet-shadowing.inner-flet-wins-interpret
  (%mls :interpret '(macrolet ((%m () :macro))
                     (flet ((%m () :function))
                       (%m))))
  :function)

;;; the expander receives the whole form; rejects one that returns a constant
(deftest interp-macrolet-shadowing.expander-receives-form-interpret
  (%mls :interpret '(macrolet ((%m (a b) `(list ,b ,a))) (%m 1 2)))
  (2 1))

;;; expanders using &whole / &environment still work
(deftest interp-macrolet-shadowing.whole-and-env-interpret
  (%mls :interpret '(macrolet ((%m (&whole w x &environment e)
                                 (declare (ignore e))
                                 `(list ',(car w) ,x)))
                     (%m 5)))
  (%m 5))

;;; an ordinary global macro call with no MACROLET around it still works
(deftest interp-macrolet-shadowing.plain-global-macro-interpret
  (%mls :interpret '(%mls-global))
  :bad)

;;; --- the bindings must be LEXICAL, not global for the dynamic extent --------
;;;
;;; The MACROLET case ALSO registered each expander in the global *MACROS* table
;;; for the dynamic extent of the body, and restored it on the way out. That is
;;; how an expander's &ENVIRONMENT could reach them: the environment object was
;;; built from the globals, so a lexical entry would have been invisible to
;;; (MACROEXPAND x env).
;;;
;;; The cost was that EVERY form interpreted during that extent saw them —
;;; including the body of a separately defined function the body happened to
;;; call. That body has nothing to do with the MACROLET's scope.
;;;
;;; The expander is now handed the environment instead, so the bindings can stay
;;; in ENV where they belong. Both halves are asserted below: the leak is gone,
;;; and &ENVIRONMENT still sees the binding.

(defun %mls (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (type-of e))))))

(defparameter %mls-leak
  '(progn (defun %mls-h () :global)
          (defun %mls-c () (%mls-h))
          (macrolet ((%mls-h () :macro)) (list (%mls-h) (%mls-c)))))

;;; (:MACRO :GLOBAL): the MACROLET applies to the form it encloses, and %MLS-C's
;;; body is not that form — it was defined elsewhere and only called from here.
(deftest interp-macrolet-scope.no-leak-into-callee-interpret
  (%mls :interpret %mls-leak)
  (:macro :global))

(deftest-compiled-only interp-macrolet-scope.no-leak-into-callee-compile
  (%mls :compile %mls-leak)
  (:macro :global))

;;; the callee is unchanged afterwards either way — the old code restored the
;;; global entry on exit, so this passed even while the leak existed
(deftest interp-macrolet-scope.callee-unchanged-after-interpret
  (%mls :interpret '(%mls-c))
  :global)

;;; --- &ENVIRONMENT still reaches the binding (what the global entry bought) ---

(deftest interp-macrolet-scope.environment-macroexpand-interpret
  (%mls :interpret '(macrolet ((%mls-m () :expanded))
                     (macrolet ((%mls-probe (&environment e)
                                  (list 'quote (macroexpand '(%mls-m) e))))
                       (%mls-probe))))
  :expanded)

(deftest interp-macrolet-scope.environment-macroexpand-1-flag-interpret
  (%mls :interpret '(macrolet ((%mls-m2 () :e2))
                     (macrolet ((%mls-probe2 (&environment e)
                                  (list 'quote (nth-value 1 (macroexpand-1 '(%mls-m2) e)))))
                       (%mls-probe2))))
  t)

;;; an expander's &ENVIRONMENT reaches SYMBOL-MACROLET bindings too — the
;;; environment now carries both halves
(deftest interp-macrolet-scope.environment-sees-symbol-macro-interpret
  (%mls :interpret '(symbol-macrolet ((%mls-s (list :sm)))
                     (macrolet ((%mls-probe3 (&environment e)
                                  (list 'quote (macroexpand '%mls-s e))))
                       (%mls-probe3))))
  (list :sm))

;;; a GLOBAL macro is still expandable through that environment: the environment
;;; table is consulted first, then the runtime one
(defmacro %mls-global-macro () :from-global)

(deftest interp-macrolet-scope.environment-still-sees-global-interpret
  (%mls :interpret '(macrolet ((%mls-probe4 (&environment e)
                                 (list 'quote (macroexpand '(%mls-global-macro) e))))
                     (%mls-probe4)))
  :from-global)

;;; --- over-fix guards ------------------------------------------------------

;;; nesting still resolves innermost-first
(deftest interp-macrolet-scope.nested-innermost-wins-interpret
  (%mls :interpret '(macrolet ((%mls-n () :outer))
                     (list (macrolet ((%mls-n () :inner)) (%mls-n)) (%mls-n))))
  (:inner :outer))

;;; the binding is gone once the form is left
(deftest interp-macrolet-scope.out-of-scope-is-undefined-interpret
  (%mls :interpret '(progn (macrolet ((%mls-gone () :in-scope)) (%mls-gone))
                           (%mls-gone)))
  (:error undefined-function))

;;; a closure created inside the scope keeps it (the bindings are captured in
;;; ENV, so this is where a purely dynamic registration would have differed)
(deftest interp-macrolet-scope.closure-keeps-binding-interpret
  (%mls :interpret '(funcall (macrolet ((%mls-cl () :captured))
                               (lambda () (%mls-cl)))))
  :captured)
