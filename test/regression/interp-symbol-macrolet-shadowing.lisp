;;; A reified &ENVIRONMENT must expand a shadowed symbol-macro to the INNERMOST
;;; binding (CLHS 5.1.2.1).
;;;
;;; The environment object handed to an expander carries its symbol macros as a
;;; hash table built from *SYMBOL-MACROS*, which SYMBOL-MACROLET pushes onto — so
;;; an outer binding of a name sits BEHIND the inner one in that alist. The
;;; builder wrote every entry into the table in order, letting the outer binding
;;; overwrite the inner one, so shadowing came out backwards and the OUTERMOST
;;; expansion won. The macrolet table three lines above it already skipped
;;; already-present keys for exactly this reason.
;;;
;;; Only the tree-walk evaluator reached the broken builder — the compiler hands
;;; expanders an environment it builds itself — so the compiled path was right and
;;; the emit-free suite was where it showed.

(defmacro %ism-probe (&environment env) `(list ',(macroexpand-1 '%ism-foo env)))

(defmacro %ism-probe2 (&environment env)
  `(list ',(macroexpand-1 '%ism-a env) ',(macroexpand-1 '%ism-b env)))

(defmacro %ism-pr (&environment env) `(list :tb ',(macroexpand-1 '%ism-tb env)))

(defmacro %ism-wb (branches &body body &environment env)
  (if branches
      (let ((cur (macroexpand-1 '%ism-tb env)))
        `(if ,(car branches)
             (symbol-macrolet ((%ism-tb (,(car branches) . ,cur)))
               (%ism-wb ,(cdr branches) ,@body))
             (%ism-wb ,(cdr branches) ,@body)))
      (if (= 1 (length body)) (car body) `(progn ,@body))))

(defun %ism (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (eval form)))

(defparameter %ism-nested
  '(symbol-macrolet ((%ism-foo :outer))
    (symbol-macrolet ((%ism-foo :inner))
      (%ism-probe))))

(deftest interp-symbol-macrolet-shadowing.innermost-wins-interpret
  (%ism :interpret %ism-nested)
  (:inner))

(deftest interp-symbol-macrolet-shadowing.innermost-wins-compile
  (%ism :compile %ism-nested)
  (:inner))

;;; Unshadowed lookup still works (rejects a fix that simply drops later entries).

(deftest interp-symbol-macrolet-shadowing.distinct-names-interpret
  (%ism :interpret
        '(symbol-macrolet ((%ism-a 1))
          (symbol-macrolet ((%ism-b 2))
            (%ism-probe2))))
  (1 2))

;;; The same body cons spliced into both arms of a recursive macro must expand
;;; against each arm's own symbol-macro scope, not reuse the other arm's.

(deftest interp-symbol-macrolet-shadowing.per-arm-scope-interpret
  (%ism :interpret
        '(funcall (lambda (x) (symbol-macrolet ((%ism-tb nil)) (%ism-wb (x) (%ism-pr)))) t))
  (:tb (x)))
