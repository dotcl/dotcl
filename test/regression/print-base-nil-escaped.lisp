;;; With *PRINT-BASE* 24 or more, NIL could be read as a number, so the symbol is
;;; printed escaped, |NIL|, as any symbol that looks like a number is (CLHS 22.1.3.3;
;;; SBCL). It was printed as (), which reads back as NIL but is not the symbol's name.

(deftest print-base-nil-escaped.high-bases
  (list (let ((*print-base* 24)) (prin1-to-string nil))
        (let ((*print-base* 36)) (prin1-to-string '(nil)))
        (let ((*print-base* 36) (*print-pretty* t)) (prin1-to-string nil)))
  ("|NIL|" "(|NIL|)" "|NIL|"))

(deftest print-base-nil-escaped.reads-back
  (let ((*print-base* 30)) (eq nil (let ((*read-base* 30)) (read-from-string (prin1-to-string nil)))))
  t)

(deftest print-base-nil-escaped.low-bases-unchanged
  (list (let ((*print-base* 10)) (prin1-to-string nil))
        (let ((*print-base* 23)) (prin1-to-string nil))
        (let ((*print-base* 36)) (princ-to-string nil)))
  ("NIL" "NIL" "NIL"))
