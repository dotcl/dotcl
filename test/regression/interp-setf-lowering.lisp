;;; SETF of SYMBOL-VALUE and of GET expand into the lowering targets
;;; %SET-SYMBOL-VALUE and PUT-PROP. The compiler recognises both in its
;;; special-form emit table and calls Runtime.SetSymbolValue / Runtime.PutProp
;;; inline, so neither name ever got a function binding — and the tree-walk
;;; interpreter resolves an operator through SYMBOL-FUNCTION. An interpreted
;;;   (setf (symbol-value s) v)   died with "Undefined function: %SET-SYMBOL-VALUE"
;;;   (setf (get s k) v)          died with "Undefined function: PUT-PROP"
;;; This is the same class as the package/setf lowering targets that were given
;;; function entities earlier — the compiler's name test being the only
;;; definition — and the fix follows the PUTHASH precedent already in
;;; cil-forms.lisp.
;;;
;;; Both evaluator paths are asserted by binding dotcl:*evaluator-mode* around
;;; the EVAL, so this runs under the ordinary compiled harness.

(defun %sl (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (princ-to-string e))))))

(deftest interp-setf-lowering.symbol-value-compile
  (%sl :compile '(let ((s (gensym))) (setf (symbol-value s) 1) (symbol-value s)))
  1)

(deftest interp-setf-lowering.symbol-value-interpret
  (%sl :interpret '(let ((s (gensym))) (setf (symbol-value s) 1) (symbol-value s)))
  1)

(deftest interp-setf-lowering.get-compile
  (%sl :compile '(let ((s (gensym))) (setf (get s :foo) 1) (get s :foo)))
  1)

(deftest interp-setf-lowering.get-interpret
  (%sl :interpret '(let ((s (gensym))) (setf (get s :foo) 1) (get s :foo)))
  1)

;;; MAKUNBOUND / the read-back path go through the same lowering.
(deftest interp-setf-lowering.makunbound-interpret
  (%sl :interpret '(let ((s (gensym)))
                     (setf (symbol-value s) 1)
                     (makunbound s)
                     (boundp s)))
  nil)

;;; GET with a default, and overwriting an existing indicator.
(deftest interp-setf-lowering.get-overwrite-interpret
  (%sl :interpret '(let ((s (gensym)))
                     (setf (get s :a) 1)
                     (setf (get s :a) 2)
                     (list (get s :a) (get s :missing :fallback))))
  (2 :fallback))

;;; PSETF / ROTATEF expand into the same targets.
(deftest interp-setf-lowering.psetf-symbol-value-interpret
  (%sl :interpret '(let ((a (gensym)) (b (gensym)))
                     (setf (symbol-value a) 1 (symbol-value b) 2)
                     (psetf (symbol-value a) (symbol-value b)
                            (symbol-value b) (symbol-value a))
                     (list (symbol-value a) (symbol-value b))))
  (2 1))
