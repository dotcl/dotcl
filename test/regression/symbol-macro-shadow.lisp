;;; Regression: SYMBOL-MACROLET binds one SYMBOL, not a name.
;;;
;;; A symbol-macro shadows the enclosing lexical variable of the same variable
;;; (CLHS 5.1.2.1), which the compiler implements by dropping that variable from
;;; *LOCALS* for the body. The drop matched by NAME STRING, so a symbol-macro for
;;; SM-B::X also hid the unrelated lexical SM-A::X and the reference expanded to
;;; the macro — silently, with the wrong value. The mirror image lived in the
;;; closure-body path, which drops symbol-macros shadowed by captured locals:
;;; there a captured SM-A::Y removed the SM-B::Y symbol-macro, and the reference
;;; failed with "Unbound variable".

(defpackage :smsh-a (:use))
(defpackage :smsh-b (:use))

(deftest symbol-macro-other-package
  (let ((smsh-a::x 1))
    (symbol-macrolet ((smsh-b::x :expanded))
      (list smsh-a::x smsh-b::x)))
  (1 :expanded))

(deftest symbol-macro-other-package-in-closure
  (let ((smsh-a::y 1))
    (symbol-macrolet ((smsh-b::y :expanded))
      (funcall (lambda () (list smsh-a::y smsh-b::y)))))
  (1 :expanded))

;;; Same symbol: the symbol-macro must still lose to an inner lexical binding.
(deftest symbol-macro-same-symbol-shadowed
  (symbol-macrolet ((smsh-a::z :expanded))
    (let ((smsh-a::z 1)) smsh-a::z))
  1)

;;; Same symbol, no inner binding: the symbol-macro wins over the OUTER lexical.
(deftest symbol-macro-same-symbol-wins
  (let ((smsh-a::w 1))
    (declare (ignorable smsh-a::w))
    (symbol-macrolet ((smsh-a::w :expanded))
      smsh-a::w))
  :expanded)

;;; A free (declare (special ...)) inside symbol-macrolet de-lexicalizes only the
;;; variable it names.
(defvar smsh-a::sv :dynamic)
(deftest symbol-macro-special-decl-other-package
  (let ((smsh-a::sv 1) (smsh-b::sv 2))
    (declare (ignorable smsh-a::sv smsh-b::sv))
    (symbol-macrolet ()
      (locally (declare (special smsh-a::sv))
        smsh-b::sv)))
  2)
