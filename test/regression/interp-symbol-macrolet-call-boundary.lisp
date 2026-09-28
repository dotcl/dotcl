;;; SYMBOL-MACROLET is lexical: it must not reach code that is merely CALLED
;;; from inside its body, nor a form handed to EVAL there.
;;;
;;; The tree-walk evaluator keeps the enclosing SYMBOL-MACROLET bindings in the
;;; special *SYMBOL-MACROS* as well as in its lexical environment (the reified
;;; &ENVIRONMENT and the SETF-family expanders read the special). Nothing reset
;;; that special at a function call or at EVAL, so a separately defined function
;;; that read a global symbol macro got the caller's local expansion instead.
;;; The compiled path was always right; these guard the interpreted ones.

(define-symbol-macro %smcb-gsm :global-sm)

(defun %smcb-reads () %smcb-gsm)

(deftest smcb-called-function-sees-global
  (symbol-macrolet ((%smcb-gsm :local-sm))
    (list %smcb-gsm (%smcb-reads)))
  (:local-sm :global-sm))

(deftest smcb-eval-sees-global
  (symbol-macrolet ((%smcb-gsm :local-sm))
    (list %smcb-gsm (eval '%smcb-gsm) (eval '(identity %smcb-gsm))))
  (:local-sm :global-sm :global-sm))

;; An &ENVIRONMENT handed to a macro expanded inside the called function must
;; not carry the caller's symbol macros either.
(defmacro %smcb-expand (x &environment env) `',(macroexpand x env))

(defun %smcb-expands () (%smcb-expand %smcb-gsm))

(deftest smcb-called-function-macro-env
  (symbol-macrolet ((%smcb-gsm :local-sm))
    (list (%smcb-expand %smcb-gsm) (%smcb-expands)))
  (:local-sm :global-sm))

;; A closure made inside a SYMBOL-MACROLET keeps seeing it when called outside,
;; also through an &ENVIRONMENT.
(deftest smcb-closure-keeps-its-own
  (list (funcall (symbol-macrolet ((%smcb-gsm :captured)) (lambda () %smcb-gsm)))
        (funcall (symbol-macrolet ((%smcb-gsm :captured))
                   (lambda () (%smcb-expand %smcb-gsm)))))
  (:captured :captured))

;; SETQ of a global symbol macro in a called function writes the global
;; expansion's place, not the caller's local one.
(defvar *smcb-cell* (list :old))
(defvar *smcb-local* :untouched)
(define-symbol-macro %smcb-place (car *smcb-cell*))

(defun %smcb-set () (setq %smcb-place :new))

(deftest smcb-called-function-setq
  (progn
    (setf *smcb-cell* (list :old) *smcb-local* :untouched)
    (symbol-macrolet ((%smcb-place *smcb-local*))
      (%smcb-set))
    (list (car *smcb-cell*) *smcb-local*))
  (:new :untouched))
