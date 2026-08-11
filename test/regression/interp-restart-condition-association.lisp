;;; RESTART-CASE over a signaling form must associate its restarts with the
;;; condition that form signals — on BOTH evaluator paths.
;;;
;;; CLHS RESTART-CASE: when the protected form is a call to SIGNAL / ERROR /
;;; CERROR / WARN, "or a macro form which macroexpands into such a list",
;;; WITH-CONDITION-RESTARTS is used implicitly. The restarts then belong to that
;;; one condition: (FIND-RESTART name c) and (COMPUTE-RESTARTS c) must not offer
;;; them for any other condition.
;;;
;;; COMPILE-RESTART-CASE did this; the RESTART-CASE macro expansion — the route
;;; the tree-walk interpreter actually takes — did not. So a handler that
;;; resignalled reached the INNER restart-case's clause through
;;; (FIND-RESTART 'FOO c) even though that restart belonged to a different
;;; condition (ansi-test RESTART-CASE.25-31, COMPUTE-RESTARTS.9).
;;;
;;; Each case asserts both paths by binding dotcl:*evaluator-mode* around the
;;; EVAL, so this runs under the ordinary compiled harness.

(defun %rca (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (princ-to-string e))))))

;;; --- the outer restart wins because the inner one is associated with the
;;; condition the inner ERROR signalled, not with the resignalled one
;;; (ansi RESTART-CASE.25)

(defparameter %rca-error-form
  '(handler-bind ((error (lambda (c2) (invoke-restart (find-restart 'foo c2)))))
    (handler-bind ((error (lambda (c) (declare (ignore c)) (error "Blah"))))
      (restart-case
          (restart-case (error "Boo!") (foo () 'bad))
        (foo () 'good)))))

(deftest interp-restart-association.error-compile
  (%rca :compile %rca-error-form)
  good)

(deftest interp-restart-association.error-interpret
  (%rca :interpret %rca-error-form)
  good)

;;; --- SIGNAL, CERROR and WARN are signaling forms too
;;; (ansi RESTART-CASE.26 / .27 / .28)

(defparameter %rca-signal-form
  '(handler-bind ((error (lambda (c2) (invoke-restart (find-restart 'foo c2)))))
    (handler-bind ((simple-condition (lambda (c) (declare (ignore c)) (error "Blah"))))
      (restart-case
          (restart-case (signal "Boo!") (foo () 'bad))
        (foo () 'good)))))

(deftest interp-restart-association.signal-compile
  (%rca :compile %rca-signal-form)
  good)

(deftest interp-restart-association.signal-interpret
  (%rca :interpret %rca-signal-form)
  good)

(defparameter %rca-cerror-form
  '(handler-bind ((error (lambda (c2) (invoke-restart (find-restart 'foo c2)))))
    (handler-bind ((error (lambda (c) (declare (ignore c)) (error "Blah"))))
      (restart-case
          (restart-case (cerror "" "") (foo () 'bad))
        (foo () 'good)))))

(deftest interp-restart-association.cerror-interpret
  (%rca :interpret %rca-cerror-form)
  good)

(defparameter %rca-warn-form
  '(handler-bind ((error (lambda (c2) (invoke-restart (find-restart 'foo c2)))))
    (handler-bind ((warning (lambda (c) (declare (ignore c)) (error "Blah"))))
      (restart-case
          (restart-case (warn "Boo!") (foo () 'bad))
        (foo () 'good)))))

(deftest interp-restart-association.warn-interpret
  (%rca :interpret %rca-warn-form)
  good)

;;; --- a MACRO form that expands into one counts, so the test needs the
;;; expansion environment: %M is only visible through the enclosing MACROLET
;;; (ansi RESTART-CASE.29, and .31 for an expander that re-expands via
;;; &ENVIRONMENT)

(defparameter %rca-macrolet-form
  '(macrolet ((%m (&rest args) (cons 'error args)))
    (handler-bind ((error (lambda (c2) (invoke-restart (find-restart 'foo c2)))))
      (handler-bind ((error (lambda (c) (declare (ignore c)) (error "Blah"))))
        (restart-case
            (restart-case (%m "Boo!") (foo () 'bad))
          (foo () 'good))))))

(deftest interp-restart-association.macrolet-compile
  (%rca :compile %rca-macrolet-form)
  good)

(deftest interp-restart-association.macrolet-interpret
  (%rca :interpret %rca-macrolet-form)
  good)

(defparameter %rca-nested-macrolet-form
  '(macrolet ((%m2 (&rest args) (cons 'error args)))
    (macrolet ((%m (&rest args &environment env) (macroexpand (cons '%m2 args) env)))
      (handler-bind ((error (lambda (c2) (invoke-restart (find-restart 'foo c2)))))
        (handler-bind ((error (lambda (c) (declare (ignore c)) (error "Blah"))))
          (restart-case
              (restart-case (%m "Boo!") (foo () 'bad))
            (foo () 'good)))))))

(deftest interp-restart-association.nested-macrolet-interpret
  (%rca :interpret %rca-nested-macrolet-form)
  good)

;;; --- the test must also see through a SYMBOL-MACROLET (ansi RESTART-CASE.30)
;;;
;;; This one stayed broken the longest. %MINI-EVAL called MACROEXPAND-1 with NO
;;; environment, so the &ENVIRONMENT an expander received was always empty.
;;; MACROLET worked anyway because the %MINI-EVAL MACROLET case registers its
;;; expanders globally in *MACROS*, not through the environment; SYMBOL-MACROLET
;;; bindings live only in *SYMBOL-MACROS*.

(defparameter %rca-symbol-macrolet-form
  '(symbol-macrolet ((%s (error "Boo!")))
    (handler-bind ((error (lambda (c2) (invoke-restart (find-restart 'foo c2)))))
      (handler-bind ((error (lambda (c) (declare (ignore c)) (error "Blah"))))
        (restart-case
            (restart-case %s (foo () 'bad))
          (foo () 'good))))))

(deftest interp-restart-association.symbol-macrolet-compile
  (%rca :compile %rca-symbol-macrolet-form)
  good)

(deftest interp-restart-association.symbol-macrolet-interpret
  (%rca :interpret %rca-symbol-macrolet-form)
  good)

;;; A MACROLET expander's (macroexpand x env) can see SYMBOL-MACROLET bindings.
;;; This ALREADY WORKED: %MACROLET-EXPANDER-FORM builds the env from
;;; *SYMBOL-MACROS* inside the expander itself. Only the RESTART-CASE case above
;;; failed, because that expander is on the C# side and takes whatever env
;;; MACROEXPAND-1 was handed. Both are kept so the difference stays visible.
(deftest interp-restart-association.expander-sees-symbol-macro-interpret
  (%rca :interpret '(symbol-macrolet ((%q 42))
                     (macrolet ((%g (x &environment e) (list 'quote (macroexpand x e))))
                       (%g %q))))
  42)

;;; --- COMPUTE-RESTARTS on an unrelated condition must not list them
;;; (ansi COMPUTE-RESTARTS.9)

(defparameter %rca-compute-form
  '(let ((c2 (make-condition 'error)))
    (block done
      (handler-bind ((error (lambda (c)
                              (declare (ignore c))
                              (return-from done
                                (mapcar #'restart-name
                                        (remove 'foo (compute-restarts c2)
                                                :test-not #'eq
                                                :key #'restart-name))))))
        (restart-case (error "an error")
          (foo () 'bad)
          (foo () 'also-bad))))))

(deftest interp-restart-association.compute-restarts-other-condition-compile
  (%rca :compile %rca-compute-form)
  nil)

(deftest interp-restart-association.compute-restarts-other-condition-interpret
  (%rca :interpret %rca-compute-form)
  nil)

;;; --- over-fix guards ---------------------------------------------------
;;; The restarts must still be reachable from the condition they DO belong to,
;;; and a restart-case whose body is not a signaling form must associate with
;;; nothing at all (an unassociated restart is visible to every condition).

(deftest interp-restart-association.own-condition-still-found-interpret
  (%rca :interpret
        '(handler-bind ((error (lambda (c) (invoke-restart (find-restart 'foo c)))))
          (restart-case (error "Boo!") (foo () 'good))))
  good)

(deftest interp-restart-association.non-signaling-body-stays-unassociated-interpret
  (%rca :interpret
        '(let ((c2 (make-condition 'error)))
          (restart-case (if (find-restart 'foo c2) 'good 'bad)
            (foo () 'not-taken))))
  good)

(deftest interp-restart-association.plain-restart-case-interpret
  (%rca :interpret '(restart-case (invoke-restart 'foo) (foo () :ok)))
  :ok)
