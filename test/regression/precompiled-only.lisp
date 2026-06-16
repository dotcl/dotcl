;;; Precompiled-only mode (dotcl:precompiled-only / DotclHost.PrecompiledOnly).
;;; When enabled, any attempt to generate code at runtime (eval/compile of a
;;; compound form, dotnet:define-class, native FFI) signals an error; running
;;; already-compiled code is unaffected. This is the in-process enforcement of
;;; the "ship = precompiled only" contract (the same constraint an AOT/IL2CPP
;;; target imposes). Loaded last so the global flag can't affect other tests.

(defun ship-sq (x) (* x x))   ; precompiled while emit is still allowed

(deftest precompiled-only-blocks-eval-not-funcall
  ;; With precompiled-only mode on: calling already-compiled code still works,
  ;; but eval of a compound form (which would JIT) is blocked. unwind-protect
  ;; guarantees the global flag is cleared even though the eval errors.
  (progn
    (dotcl:precompiled-only t)
    (unwind-protect
        (list (ship-sq 6)
              (handler-case (progn (eval '(+ 1 2)) :no-error)
                (error () :blocked)))
      (dotcl:precompiled-only nil)))
  (36 :blocked))

(deftest precompiled-only-off-allows-eval-again
  ;; After clearing the flag, eval works again (no lingering state).
  (progn (dotcl:precompiled-only nil) (eval '(+ 40 2)))
  42)
