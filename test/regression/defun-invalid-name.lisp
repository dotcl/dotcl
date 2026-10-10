;;; DEFUN signals an error when the name is not a function name (a symbol or
;;; (SETF symbol)).
;;;
;;; A string was accepted: (defun "invalid" () ...) defined nothing anyone could
;;; call and returned "invalid". Other non-names failed by accident inside the
;;; compiler. SBCL 2.6.8 signals an error for each of these.

(defun %defun-name-errors-p (form)
  (handler-case (progn (eval form) nil)
    (error () t)))

(deftest defun-invalid-name.string
  (%defun-name-errors-p '(defun "invalid" () (princ :hoge)))
  t)

(deftest defun-invalid-name.number
  (%defun-name-errors-p '(defun 42 () 1))
  t)

(deftest defun-invalid-name.setf-of-non-symbol
  (%defun-name-errors-p '(defun (setf 3) (v) v))
  t)

(deftest defun-invalid-name.other-list
  (%defun-name-errors-p '(defun (foo bar) () 1))
  t)

;; Symbols and (SETF symbol) still define functions.
(deftest defun-invalid-name.valid-names
  (list (%defun-name-errors-p '(defun %defun-name-ok () 1))
        (%defun-name-errors-p '(defun (setf %defun-name-ok) (v) v)))
  (nil nil))
