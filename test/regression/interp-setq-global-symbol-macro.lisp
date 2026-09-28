;;; SETQ of a name that DEFINE-SYMBOL-MACRO defined, and that no LET shadows,
;;; is SETF of the expansion (CLHS 5.1.2.4 / SETQ). The tree-walk evaluator only
;;; looked for SYMBOL-MACROLET entries in its own environment and otherwise SET
;;; the symbol's value, so the place was left alone and the symbol became bound.
;;; Expected values are SBCL's.

(defvar *isgsm-c* (list 0))
(define-symbol-macro isgsm-a (car *isgsm-c*))
(define-symbol-macro isgsm-b isgsm-a)

(defun %isgsm (mode form)
  (setf *isgsm-c* (list 0))
  (let ((dotcl:*evaluator-mode* mode))
    (eval form)))

(deftest interp-setq-global-symbol-macro.interpret
  (%isgsm :interpret '(progn (setq isgsm-a 5) (list *isgsm-c* (boundp 'isgsm-a))))
  ((5) nil))

(deftest interp-setq-global-symbol-macro.compile
  (%isgsm :compile '(progn (setq isgsm-a 5) (list *isgsm-c* (boundp 'isgsm-a))))
  ((5) nil))

;; A symbol macro expanding to another one; a LET still shadows the name.
(deftest interp-setq-global-symbol-macro.chain-and-shadow
  (%isgsm :interpret '(list (setq isgsm-b 7) *isgsm-c*
                       (let ((isgsm-a 1)) (setq isgsm-a 2) isgsm-a) *isgsm-c*))
  (7 (7) 2 (7)))

;; PSETQ expands into SETQ.
(deftest interp-setq-global-symbol-macro.psetq
  (%isgsm :interpret '(progn (psetq isgsm-a 9) *isgsm-c*))
  (9))

;; An ordinary special variable is still assigned directly.
(defvar *isgsm-plain* 1)
(deftest interp-setq-global-symbol-macro.plain-variable
  (%isgsm :interpret '(progn (setq *isgsm-plain* 3) *isgsm-plain*))
  3)
