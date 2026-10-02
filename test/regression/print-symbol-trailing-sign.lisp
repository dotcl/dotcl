;;; A symbol whose name ends with a sign (1+, 1-) prints without escapes.
;;;
;;; A potential number never ends with a sign (CLHS 2.3.1.1), so such a name
;;; reads back as the same symbol. The printer escaped every name that starts
;;; with a digit, and printed (1+ x) as (|1+| X), which try's reports and
;;; anything comparing printed code against SBCL's output tripped over.
;;; Expected values are SBCL's.

(deftest print-symbol-trailing-sign.unescaped
  (mapcar #'prin1-to-string (list '1+ '1- (intern "1B+")))
  ("1+" "1-" "1B+"))

(deftest print-symbol-trailing-sign.form
  (let ((*package* (find-package "CL-USER")))
    (prin1-to-string '(1+ (1- x))))
  "(1+ (1- X))")

(deftest print-symbol-trailing-sign.potential-numbers-still-escaped
  (mapcar #'prin1-to-string
          (list (intern "1A") (intern "+1") (intern "1E5") (intern "1/2") (intern "1^")))
  ("|1A|" "|+1|" "|1E5|" "|1/2|" "|1^|"))

(deftest print-symbol-trailing-sign.reads-back
  (eq (read-from-string (prin1-to-string '1+)) '1+)
  t)
