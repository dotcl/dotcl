;;; A macro's expander can call EVAL while the compiler is in the middle of a
;;; function body (Coalton's code generator evaluates the symbol macros TRUE and
;;; FALSE that way). The body that takes its caller's value mode keeps that mode
;;; in a local of its own method, and EVAL compiled its form with that local
;;; still current: a tail call in the evaluated form passed the mode through
;;; it, and assembling the form failed with "Undeclared local: MVMODE_n".
;;; The value-mode state is now reset for the form EVAL compiles.
;;;
;;; The compiler's state is bound here directly, as it is while a body is
;;; compiled; reaching the same moment through a macro depends on which pass
;;; first expands the macro.

(defun eicb-two (x) (values x (1+ x)))
(eicb-two 1)

(deftest eval-inside-compiled-body.tail-call
  (let ((s (find-symbol "*MV-MODE-LOCAL*" "DOTCL-INTERNAL")))
    (progv (if s (list s) '()) (if s (list (intern "MVMODE_999" "DOTCL-INTERNAL")) '())
      (multiple-value-list (eval '(let ((y 5)) (eicb-two y))))))
  (5 6))

(deftest eval-inside-compiled-body.primary
  (let ((s (find-symbol "*MV-MODE-LOCAL*" "DOTCL-INTERNAL")))
    (progv (if s (list s) '()) (if s (list (intern "MVMODE_999" "DOTCL-INTERNAL")) '())
      (list (eval '(let ((y 7)) (car (list (eicb-two y))))))))
  (7))
