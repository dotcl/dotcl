;;; cil-host.lisp: what the compiler calls that the dotcl runtime provides as a
;;; builtin, defined for the host Lisp that runs the cross-compile.
;;;
;;; Loaded by cil-compile.lisp on the host only, after cil-compiler*.lisp and
;;; before the rest of the compiler. It is not part of the core: a definition
;;; in the compiler's own sources is compiled into the core with everything
;;; else, where it would stand in for the runtime's builtin of the same name.

(in-package :dotcl.cil-compiler)

(defun %package-physical-name (package) (package-name package))

;; Locks and compare-and-swap on a special variable. The host compiles on one
;; thread.
(defun %make-lock (name) name)
(defun %cas-symbol-value (lock symbol old new)
  (declare (ignore lock))
  (let ((cur (symbol-value symbol)))
    (when (eql cur old) (setf (symbol-value symbol) new))
    cur))

;; LOAD-TIME-VALUE slot ids. The runtime's increments atomically.
(defvar *ltv-counter* 0)
(defun %next-ltv-id () (incf *ltv-counter*))

;; The tree-walk evaluator's single-value MACROEXPAND-1 (COMPILE-MACROLET runs
;; %MINI-EVAL on the host too): the expansion, or FORM itself when it does not
;; expand.
(defun %macroexpand-1-or-self (form &optional (env nil env-p))
  (multiple-value-bind (exp expandedp)
      (if env-p (macroexpand-1 form env) (macroexpand-1 form))
    (if expandedp exp form)))

;; The tree-walk evaluator's per-list memo (see %MINI-SHAPE in cil-forms-interp.lisp).
(defvar *mini-memo* (make-hash-table :test 'eq))
(defun %mini-memo (key) (values (gethash key *mini-memo*)))
(defun %set-mini-memo (key value)
  (setf (gethash key *mini-memo*) value))

;; The tree-walk evaluator's expansion cache (see %MINI-MACROEXPAND-1-CACHED in
;; the runtime). The host keeps nothing: it expands every time, here, or, with
;; a global symbol macro defined, in the caller's environment.
(defvar *%host-expansion-miss* (list '%mini-expansion-miss))
(defun %mini-expansion-miss () *%host-expansion-miss*)
(defun %mini-macroexpand-1-cached (form)
  (if (eql (hash-table-count *global-symbol-macros*) 0)
      (%macroexpand-1-or-self form)
      *%host-expansion-miss*))
(defun %mini-store-expansion (form expansion)
  (declare (ignore form))
  expansion)
