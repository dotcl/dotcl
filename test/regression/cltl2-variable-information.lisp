;;; DOTCL-CLTL2:VARIABLE-INFORMATION answers for the global environment:
;;; :SPECIAL, :CONSTANT and :SYMBOL-MACRO, NIL for a symbol with no global
;;; variable definition. It used to answer NIL for everything, so a library had
;;; no public way to ask whether a symbol is globally special.

(defvar *vi-special* 1)
(defconstant +vi-constant+ 2)
(define-symbol-macro vi-symbol-macro (car '(3)))

(deftest variable-information-special
  (multiple-value-list (dotcl-cltl2:variable-information '*vi-special*))
  (:special nil nil))

(deftest variable-information-standard-special
  (values (dotcl-cltl2:variable-information '*package*))
  :special)

(deftest variable-information-constant
  (list (dotcl-cltl2:variable-information '+vi-constant+)
        (dotcl-cltl2:variable-information 'pi)
        (dotcl-cltl2:variable-information :key)
        (dotcl-cltl2:variable-information t)
        (dotcl-cltl2:variable-information nil))
  (:constant :constant :constant :constant :constant))

(deftest variable-information-symbol-macro
  (values (dotcl-cltl2:variable-information 'vi-symbol-macro))
  :symbol-macro)

(deftest variable-information-unknown
  (multiple-value-list (dotcl-cltl2:variable-information (make-symbol "VI-FRESH") nil))
  (nil nil nil))
