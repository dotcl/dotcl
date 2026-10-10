;;; An ERROR from a macro's expander while EVAL or COMPILE compiles a form is
;;; deferred to when the macro form is evaluated, as a PROGRAM-ERROR.
;;;
;;; (eval '(ignore-errors (bad) 'x)) with BAD's expander signalling: the error
;;; came out of EVAL, past the IGNORE-ERRORS around the form, and took the script
;;; down. SBCL compiles the form with the macro form replaced by one that signals
;;; a COMPILED-PROGRAM-ERROR (a PROGRAM-ERROR), so IGNORE-ERRORS returns NIL and
;;; the condition. iterate's test BUG/PREVIOUSLY-INITIALLY.1 expects that.
;;; SBCL's evaluator expands a macro form at the top of what EVAL is given itself,
;;; so there the expander's own error comes out of EVAL; dotcl does the same.
;;; Expected values are SBCL 2.6.8's.

(defmacro eme-bad () (error "eme boom"))

(defun eme-eval (form)
  (let ((*error-output* (make-broadcast-stream)))
    (eval form)))

(defun eme-describe (c)
  (list (and (typep c 'program-error) t)
        (and (search "eme boom" (princ-to-string c)) t)))

(deftest eval-macroexpansion-error.ignore-errors
  (multiple-value-bind (v c) (eme-eval '(ignore-errors (eme-bad) 'x))
    (list v (and (search "eme boom" (princ-to-string c)) t)))
  (nil t))

;; Compiled, the condition is the PROGRAM-ERROR that stands in for the failed
;; expansion. (The tree-walk interpreter expands the macro form only when it
;; evaluates it, so there IGNORE-ERRORS sees the expander's own error.)
(deftest-compiled-only eval-macroexpansion-error.program-error
  (multiple-value-bind (v c) (eme-eval '(ignore-errors (eme-bad) 'x))
    (list v (eme-describe c)))
  (nil (t t)))

;; Only the branch that is taken signals.
(deftest eval-macroexpansion-error.not-reached
  (eme-eval '(if t 1 (eme-bad)))
  1)

;; At the top of EVAL's form (also inside a top-level PROGN): the expander's own
;; error, not a PROGRAM-ERROR.
(deftest eval-macroexpansion-error.top-level
  (list (handler-case (eme-eval '(eme-bad)) (error (e) (type-of e)))
        (handler-case (eme-eval '(progn (eme-bad))) (error (e) (type-of e))))
  (simple-error simple-error))

;; COMPILE returns a function; calling it signals. (An emit-free build has no
;; COMPILE.)
(deftest-emitting-only eval-macroexpansion-error.compile
  (let ((f (let ((*error-output* (make-broadcast-stream)))
             (compile nil '(lambda () (eme-bad))))))
    (list (functionp f)
          (handler-case (progn (funcall f) :no-error)
            (error (e) (eme-describe e)))))
  (t (t t)))
