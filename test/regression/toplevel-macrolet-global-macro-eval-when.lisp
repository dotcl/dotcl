;;; An EVAL-WHEN (:COMPILE-TOPLEVEL ...) DEFUN that a global macro produces inside a top
;;; level MACROLET is a real compile-time definition, as it is outside the MACROLET: it is
;;; still there after COMPILE-FILE returns. It used to be undone with the early
;;; evaluations of plain DEFUNs, so loading the compiled file found the function unbound
;;; while its memoization record was still on the symbol (lisp-interface-library's
;;; BUILTIN macro wrapped DEFINE-INTERFACE, which expands into DEFINE-MEMO-FUNCTION).

(defmacro %cfm-def (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute) (defun ,name () 1)))

(defun %cfm-compile (forms)
  (let* ((src (regression-temp-file (format nil "cfm-~D.lisp" (random 1000000))))
         (out (regression-temp-file (format nil "cfm-~D.fasl" (random 1000000)))))
    (with-open-file (o src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (dolist (f forms) (prin1 f o) (terpri o))))
    (unwind-protect (compile-file src :output-file out)
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file out)))))

(deftest-emitting-only toplevel-macrolet-global-macro-eval-when.global-macro-in-macrolet
  (progn
    (%cfm-compile '((macrolet ((b (name) `(progn (%cfm-def ,name))))
                      (b cfm-in-macrolet))))
    (fboundp 'cfm-in-macrolet))
  t)

(deftest-emitting-only toplevel-macrolet-global-macro-eval-when.local-macro-shadows-global
  (progn
    (%cfm-compile '((macrolet ((%cfm-def (name) `(eval-when (:compile-toplevel) (defun ,name () 2))))
                      (%cfm-def cfm-shadowed))))
    (and (fboundp 'cfm-shadowed) (funcall 'cfm-shadowed)))
  2)

(deftest-emitting-only toplevel-macrolet-global-macro-eval-when.plain-defun-is-still-undone
  (progn
    (%cfm-compile '((macrolet ((b (name) `(defun ,name () 3)))
                      (b cfm-plain))))
    (fboundp 'cfm-plain))
  nil)
