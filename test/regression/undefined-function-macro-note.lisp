;;; Calling a macro or a special operator as a function (funcall 'when, #'if through a symbol, mapcar
;;; over a macro name) says so in the message, instead of the plain "Undefined function".

(defun %ufmn-message (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (undefined-function (c) (princ-to-string c))))

(deftest undefined-function-macro-note.macro
  (%ufmn-message (lambda () (funcall 'when 1 2)))
  "Undefined function: WHEN (it is a macro, not a function)")

(deftest undefined-function-macro-note.special-operator
  (%ufmn-message (lambda () (funcall 'if 1 2 3)))
  "Undefined function: IF (it is a special operator, not a function)")

(deftest undefined-function-macro-note.plain-undefined
  (%ufmn-message (lambda () (funcall 'no-such-function-ufmn 1)))
  "Undefined function: NO-SUCH-FUNCTION-UFMN")
