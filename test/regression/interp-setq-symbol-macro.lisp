;;; SETQ of a name bound by SYMBOL-MACROLET is SETF of that name's expansion
;;; (CLHS setq), not a variable assignment. The tree-walk interpreter stored the
;;; value straight into the env entry — which not only assigned the wrong place
;;; but OVERWROTE the (name SYMBOL-MACRO expansion) binding with the value,
;;; destroying the symbol macro for the rest of the body.
;;;
;;; PSETQ, PSETF and ROTATEF all macroexpand into SETQ, so the whole cluster
;;; failed with it (ansi-test SETF-SYMBOL-MACRO.1-3, PSETQ.4/5/7, PSETF.*,
;;; ROTATEF.*).
;;;
;;; Both evaluator paths are asserted per case by binding dotcl:*evaluator-mode*
;;; around the EVAL, so this runs under the ordinary compiled harness.

(defun %issm-eval (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (eval form)))

;;; The ansi-test PSETQ.4 shape: X is a symbol macro for Y, so (psetq x 2)
;;; must assign Y and leave the outer X alone.
(defparameter %issm-psetq
  '(let ((x 0))
     (values (symbol-macrolet ((x y)) (let ((y 1)) (psetq x 2) y)) x)))

(deftest interp-setq-symbol-macro.psetq-compile
  (multiple-value-list (%issm-eval :compile %issm-psetq))
  (2 0))

(deftest interp-setq-symbol-macro.psetq-interpret
  (multiple-value-list (%issm-eval :interpret %issm-psetq))
  (2 0))

;;; Plain SETQ through a symbol macro.
(defparameter %issm-setq
  '(let ((place (list 10 20)))
     (symbol-macrolet ((head (car place)))
       (setq head 99))
     place))

(deftest interp-setq-symbol-macro.setq-place-compile
  (%issm-eval :compile %issm-setq)
  (99 20))

(deftest interp-setq-symbol-macro.setq-place-interpret
  (%issm-eval :interpret %issm-setq)
  (99 20))

;;; The binding must SURVIVE the assignment: reading the symbol macro again
;;; afterwards still goes through the expansion. This is the part the old code
;;; broke outright by clobbering the env entry.
(defparameter %issm-survives
  '(let ((y 1))
     (symbol-macrolet ((x y))
       (setq x 2)
       (list x y))))

(deftest interp-setq-symbol-macro.binding-survives-compile
  (%issm-eval :compile %issm-survives)
  (2 2))

(deftest interp-setq-symbol-macro.binding-survives-interpret
  (%issm-eval :interpret %issm-survives)
  (2 2))

;;; The value form is evaluated exactly once.
(defparameter %issm-once
  '(let ((n 0) (y 0))
     (symbol-macrolet ((x y))
       (setq x (progn (incf n) 5))
       (list n y))))

(deftest interp-setq-symbol-macro.value-evaluated-once-interpret
  (%issm-eval :interpret %issm-once)
  (1 5))

;;; An ordinary lexical variable is still assigned as a variable.
(deftest interp-setq-symbol-macro.plain-variable-interpret
  (%issm-eval :interpret '(let ((a 1)) (setq a 7) a))
  7)
