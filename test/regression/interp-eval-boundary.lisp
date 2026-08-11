;;; Behaviour EVAL must exhibit identically on both evaluator paths.
;;;
;;; These bind dotcl:*evaluator-mode* around the EVAL under test, so the
;;; interpreter is exercised from the ordinary compiled harness — no separate
;;; interpreted run needed to catch a regression here. That matters because
;;; the tree-walk interpreter is the ONLY evaluator on emit-free
;;; (netstandard2.0) builds, where nothing else would notice.

;;; An unmatched THROW leaving EVAL is a CONTROL-ERROR (CLHS; ansi-test
;;; THROW-ERROR). The conversion used to wrap the compiled branch of
;;; EvalCompound only, so under the interpreter the raw .NET CatchThrowException
;;; escaped to Program.Main and killed the process outright — running ansi-test
;;; with :interpret died at THROW-ERROR instead of reporting a failure.

(defun %ieb-throw-unmatched (mode)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (progn (eval (list 'throw (list 'quote (gensym)) nil)) :no-error)
      (control-error () :control-error)
      (error () :other-error))))

(deftest interp-eval-boundary.unmatched-throw-compile
  (%ieb-throw-unmatched :compile)
  :control-error)

(deftest interp-eval-boundary.unmatched-throw-interpret
  (%ieb-throw-unmatched :interpret)
  :control-error)

;;; A THROW whose CATCH is outside the EVAL must still reach it — the
;;; control-error conversion must not swallow a legitimate non-local exit.

(defun %ieb-throw-to-outer (mode)
  (catch 'ieb-outer
    (let ((dotcl:*evaluator-mode* mode))
      (eval '(throw 'ieb-outer :thrown)))
    :not-thrown))

(deftest interp-eval-boundary.outer-catch-compile
  (%ieb-throw-to-outer :compile)
  :thrown)

(deftest interp-eval-boundary.outer-catch-interpret
  (%ieb-throw-to-outer :interpret)
  :thrown)

;;; A CATCH established inside the evaluated form catches its own THROW on
;;; either path (the ordinary case, guarding against over-eager conversion).

(defun %ieb-self-contained (mode)
  (let ((dotcl:*evaluator-mode* mode))
    (eval '(catch 'ieb-inner (throw 'ieb-inner :ok) :unreached))))

(deftest interp-eval-boundary.self-contained-catch-compile
  (%ieb-self-contained :compile)
  :ok)

(deftest interp-eval-boundary.self-contained-catch-interpret
  (%ieb-self-contained :interpret)
  :ok)
