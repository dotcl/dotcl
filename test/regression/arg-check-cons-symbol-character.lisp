;;; Declared CONS, SYMBOL and CHARACTER parameters signal TYPE-ERROR.

(setf dotcl:*save-sil* t)

(defun %acsc-cons (x) (declare (type cons x)) (car x))
(defun %acsc-symbol (x) (declare (type symbol x)) x)
(defun %acsc-character (x) (declare (type character x)) x)

(defun %acsc-outcome (thunk)
  (handler-case (list :value (funcall thunk))
    (type-error (e) (list :type-error (type-error-datum e)))))

(defun %acsc-has (needle fn)
  (and (search needle (%flat-text (dotcl:function-sil fn))) t))

(deftest arg-check-cons-symbol-character.values
  (list (%acsc-cons '(1 2)) (%acsc-symbol 'foo) (%acsc-symbol nil)
        (%acsc-symbol t) (%acsc-character #\a))
  (1 foo nil t #\a))

(deftest arg-check-cons-symbol-character.wrong-type
  (list (%acsc-outcome (lambda () (%acsc-cons 3)))
        (%acsc-outcome (lambda () (%acsc-cons nil)))
        (%acsc-outcome (lambda () (%acsc-symbol 3)))
        (%acsc-outcome (lambda () (%acsc-character 3))))
  ((:type-error 3) (:type-error nil) (:type-error 3) (:type-error 3)))

(deftest-emitting-only arg-check-cons-symbol-character.uses-the-checks
  (list (%acsc-has "ISINST Cons" #'%acsc-cons)
        (%acsc-has "Runtime.CheckArgCons" #'%acsc-cons)
        (%acsc-has "Runtime.CheckArgSymbol" #'%acsc-symbol)
        (%acsc-has "Runtime.CheckArgCharacter" #'%acsc-character))
  (t nil t t))
