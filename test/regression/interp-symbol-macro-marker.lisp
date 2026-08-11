;;; The interpreter's SYMBOL-MACROLET marker must never reach user code.
;;;
;;; %MINI-EVAL treats an ENV value shaped (<marker> expansion) as a
;;; symbol-macrolet binding and EVALUATES ITS SECOND ELEMENT AS A VARIABLE. That
;;; marker used to be the interned symbol DOTCL-INTERNAL::SYMBOL-MACRO.
;;;
;;; The SIL loader materialises that symbol LAZILY — it appears in the package the
;;; moment the interpreter first evaluates a SYMBOL-MACROLET. From then on
;;; DO-SYMBOLS / DO-ALL-SYMBOLS hand the interpreter's own sentinel back to user
;;; code, so any interpreted variable holding a list (the DOLIST variable walking
;;; a package's symbol list is the obvious one) is misread as a symbol-macro
;;; binding as soon as the marker lands at its head:
;;;
;;;   (do-symbols (sym (find-package "DOTCL-INTERNAL")) (symbol-name sym))
;;;   => #<UNBOUND-VARIABLE: %THREADP>   ; the symbol after the marker
;;;
;;; ansi-test DO-ALL-SYMBOLS.1-13 / FIND-ALL-SYMBOLS.1 / PRINT.SYMBOL.RANDOM.3-4.
;;;
;;; The lazy interning is what made this look state-dependent and unreproducible:
;;; the same form passes until something, somewhere, interprets a SYMBOL-MACROLET.
;;; The reported variable name changes between runs for the same reason — what
;;; sits next to the marker depends on the package's symbol order.
;;;
;;; The fix makes the marker uninterned with MAKE-SYMBOL. It belongs to no
;;; package, so no iteration can produce it.
;;;
;;; ORDER MATTERS IN THE TESTS BELOW: without first evaluating one SYMBOL-MACROLET
;;; through the interpreter, the marker does not exist yet and even the broken
;;; version passes.

;;; --- the marker is not visible from any package (the fix, stated directly)

(deftest interp-symbol-macro-marker.not-reachable-after-symbol-macrolet
  (let ((dotcl:*evaluator-mode* :interpret))
    ;; in the broken version this is what interns the marker
    (eval '(symbol-macrolet ((%smm-a 1)) %smm-a))
    (let ((hits '()))
      (do-all-symbols (s)
        (when (string= (symbol-name s) "SYMBOL-MACRO") (pushnew s hits)))
      hits))
  nil)

;;; --- a package's symbols can be walked on the interpreted path

(deftest interp-symbol-macro-marker.do-symbols-interpret
  (let ((dotcl:*evaluator-mode* :interpret))
    (eval '(symbol-macrolet ((%smm-b 1)) %smm-b))
    (handler-case
        (progn (eval `(do-symbols (s ,(find-package "DOTCL-INTERNAL")) (symbol-name s)))
               :ok)
      (error (e) (list :error (princ-to-string e)))))
  :ok)

(deftest interp-symbol-macro-marker.do-all-symbols-interpret
  (let ((dotcl:*evaluator-mode* :interpret))
    (eval '(symbol-macrolet ((%smm-c 1)) %smm-c))
    (handler-case
        (progn (eval '(let ((n 0)) (do-all-symbols (s) (when (symbol-name s) (incf n))) n))
               :ok)
      (error (e) (list :error (princ-to-string e)))))
  :ok)

;;; the shape of ansi FIND-ALL-SYMBOLS.1: DO-SYMBOLS plus accumulation
(deftest interp-symbol-macro-marker.accumulate-symbols-interpret
  (let ((dotcl:*evaluator-mode* :interpret))
    (eval '(symbol-macrolet ((%smm-d 1)) %smm-d))
    (handler-case
        (eval `(let ((acc nil))
                 (do-symbols (s ,(find-package "DOTCL-INTERNAL"))
                   (push s acc))
                 (if (> (length acc) 100) :many :few)))
      (error (e) (list :error (princ-to-string e)))))
  :many)

;;; --- over-fix guards: SYMBOL-MACROLET itself still works

(defun %smm (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (type-of e))))))

(deftest interp-symbol-macro-marker.symbol-macrolet-read-interpret
  (%smm :interpret '(symbol-macrolet ((x 10)) (+ x 1)))
  11)

(deftest interp-symbol-macro-marker.symbol-macrolet-read-compile
  (%smm :compile '(symbol-macrolet ((x 10)) (+ x 1)))
  11)

;;; SETQ becomes a SETF of the expansion (the marker is read on that path too)
(deftest interp-symbol-macro-marker.symbol-macrolet-setq-interpret
  (%smm :interpret '(let ((c (list 1 2)))
                     (symbol-macrolet ((h (car c)))
                       (setq h 99)
                       (list h c))))
  (99 (99 2)))

;;; a LET shadows a symbol macro of the same name
(deftest interp-symbol-macro-marker.let-shadows-interpret
  (%smm :interpret '(symbol-macrolet ((y 10)) (let ((y 20)) y)))
  20)

;;; nested: the inner one wins
(deftest interp-symbol-macro-marker.nested-interpret
  (%smm :interpret '(symbol-macrolet ((z 1))
                     (list (symbol-macrolet ((z 2)) z) z)))
  (2 1))

;;; a symbol macro whose expansion is NIL: a marker test written on truthiness
;;; would break here
(deftest interp-symbol-macro-marker.nil-expansion-interpret
  (%smm :interpret '(symbol-macrolet ((n nil)) (list n (null n))))
  (nil t))
