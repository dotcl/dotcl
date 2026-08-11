;;; A non-default *MACROEXPAND-HOOK* must not re-enter itself.
;;;
;;; An INTERPRETED hook used to recurse forever. The tree-walk evaluator
;;; macroexpands the hook's own body on every call, so a hook whose body contains
;;; any macro — (incf count) is enough — expands it, which calls MACROEXPAND-1,
;;; which calls the hook, and so on. The process died on a .NET stack overflow,
;;; which cannot be caught, so the failure took the whole test run with it.
;;;
;;; A COMPILED hook never showed this: its body was expanded once at compile time,
;;; so nothing expands while it runs. That is why the ordinary harness stayed
;;; green — LOAD compiles the DEFTEST form, so the hook lambda is compiled — and
;;; why this only surfaced on an emit-free build, where everything is interpreted.
;;;
;;; MACROEXPAND-1 now calls the expander directly while a hook is running. A hook
;;; therefore does not observe expansions performed by its OWN body, which is the
;;; only terminating reading for an evaluator that expands at eval time.
;;;
;;; These go through (eval ...) under :interpret so the hook really is an
;;; interpreted closure; written as plain code the harness would compile it and
;;; exercise nothing.

(defmacro %mhr-double (x) `(* 2 ,x))
(defmacro %mhr-once (x) `(%mhr-double ,x))

(defun %mhr (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (type-of e))))))

;;; --- a counting hook terminates, and counts the one expansion it was asked for

(defparameter %mhr-count-form
  '(let ((count 0))
    (let ((*macroexpand-hook*
            (lambda (expander form env)
              (incf count)
              (funcall expander form env))))
      (macroexpand-1 '(%mhr-double 3)))
    count))

(deftest interp-macroexpand-hook-reentry.counted-compile
  (%mhr :compile %mhr-count-form)
  1)

(deftest interp-macroexpand-hook-reentry.counted-interpret
  (%mhr :interpret %mhr-count-form)
  1)

;;; MACROEXPAND's fixpoint loop consults the hook on each step
(defparameter %mhr-fixpoint-form
  '(let ((count 0))
    (let ((*macroexpand-hook*
            (lambda (expander form env)
              (incf count)
              (funcall expander form env))))
      (macroexpand '(%mhr-once 3)))
    count))

(deftest interp-macroexpand-hook-reentry.fixpoint-compile
  (%mhr :compile %mhr-fixpoint-form)
  2)

(deftest interp-macroexpand-hook-reentry.fixpoint-interpret
  (%mhr :interpret %mhr-fixpoint-form)
  2)

;;; --- over-fix guards ---------------------------------------------------
;;; The guard must suppress only the hook's own nested expansions. If it disabled
;;; hooks altogether the cases below would fail.

;;; the hook still runs at all: it can REPLACE the expansion
(deftest interp-macroexpand-hook-reentry.hook-can-replace-interpret
  (%mhr :interpret '(let ((*macroexpand-hook*
                            (lambda (expander form env)
                              (declare (ignore expander form env))
                              :replaced)))
                     (values (macroexpand-1 '(%mhr-double 3)))))
  :replaced)

;;; the hook sees the form and the expander it was given
(deftest interp-macroexpand-hook-reentry.hook-receives-form-interpret
  (%mhr :interpret '(let ((seen nil))
                     (let ((*macroexpand-hook*
                             (lambda (expander form env)
                               (setq seen form)
                               (funcall expander form env))))
                       (macroexpand-1 '(%mhr-double 3)))
                     seen))
  (%mhr-double 3))

;;; a hook whose body contains NO macro is unaffected either way
(deftest interp-macroexpand-hook-reentry.macro-free-hook-interpret
  (%mhr :interpret '(let ((*macroexpand-hook*
                            (lambda (expander form env) (funcall expander form env))))
                     (values (macroexpand-1 '(%mhr-double 3)))))
  (* 2 3))

;;; the default hook path (no rebinding) still expands normally.
;;; MACROEXPAND-1 returns two values; only the expansion is asserted here.
(deftest interp-macroexpand-hook-reentry.default-hook-interpret
  (%mhr :interpret '(values (macroexpand-1 '(%mhr-double 3))))
  (* 2 3))

;;; the binding is restored: a later expansion outside the LET is not suppressed
(deftest interp-macroexpand-hook-reentry.restored-after-interpret
  (%mhr :interpret '(let ((count 0))
                     (let ((*macroexpand-hook*
                             (lambda (expander form env)
                               (incf count)
                               (funcall expander form env))))
                       (macroexpand-1 '(%mhr-double 1))
                       (macroexpand-1 '(%mhr-double 2)))
                     count))
  2)
