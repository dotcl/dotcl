;;; Regression: SPECIAL-OPERATOR-P, FBOUNDP, FDEFINITION and SYMBOL-FUNCTION
;;; looked a special operator up by symbol NAME (case-insensitively), so a
;;; symbol that only shared the name -- a SHADOWed LABELS -- counted as a
;;; special operator, and DEFGENERIC on it signalled "names a special
;;; operator". cl-opendaq shadows LABELS and defines a generic function on it.

(defpackage :sosn-pkg (:use :cl) (:shadow #:labels #:flet))

(deftest special-operator-shadowed-name-predicate
  (list (special-operator-p 'cl:labels)
        (special-operator-p 'sosn-pkg::labels)
        (special-operator-p (make-symbol "IF"))
        (special-operator-p (intern "if" :sosn-pkg))
        (fboundp 'sosn-pkg::flet))
  (t nil nil nil nil))

(deftest special-operator-shadowed-name-defgeneric
  (progn
    (eval '(defgeneric sosn-pkg::labels (o)))
    (eval '(defmethod sosn-pkg::labels ((o integer)) (list :labels o)))
    (funcall 'sosn-pkg::labels 7))
  (:labels 7))

(deftest special-operator-shadowed-name-defun-call
  (progn
    (eval '(defun sosn-pkg::flet (x) (* x 2)))
    (list (funcall (eval '(lambda () (sosn-pkg::flet 21))))
          (eval '(sosn-pkg::flet 4))))
  (42 8))
