;;; (FUNCALL 'name ...) where the symbol has no function.
;;;
;;; A macro name or a special-operator name is FBOUNDP but has no function to
;;; CALL. CLHS reports that as UNDEFINED-FUNCTION, and the compiled path did.
;;;
;;; The registered C# FUNCALL resolved the symbol with Runtime.Fdefinition, which
;;; is deliberately permissive: it answers macros with their macro-function and
;;; special operators with a stub that dies when called. So on the interpreted
;;; path
;;;   (funcall 'progn 1)       → ERROR "Cannot call special operator PROGN"
;;;   (funcall 'defconstant x) => PROGRAM-ERROR (the MACRO's arity mismatch)
;;; and neither the condition type nor the CELL-ERROR-NAME matched
;;; (ansi-test FUNCALL.ERROR.1 / .2 / .3).
;;;
;;; The fix routes it through Runtime.CoerceToFunction, what the compiled path
;;; already used.
;;;
;;; These assert the CELL-ERROR-NAME as well as the condition type. ansi checks it
;;; too, and the type alone is easy to confuse with the PROGRAM-ERROR the broken
;;; version raised.

(defun %fd (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (list :value (eval form))
      (undefined-function (c) (list :undefined-function (cell-error-name c)))
      (error (e) (list :other (type-of e))))))

;;; --- special operators

(deftest interp-funcall-designator.special-operator-quote-compile
  (%fd :compile '(funcall 'quote 1))
  (:undefined-function quote))

(deftest interp-funcall-designator.special-operator-quote-interpret
  (%fd :interpret '(funcall 'quote 1))
  (:undefined-function quote))

(deftest interp-funcall-designator.special-operator-progn-compile
  (%fd :compile '(funcall 'progn 1))
  (:undefined-function progn))

(deftest interp-funcall-designator.special-operator-progn-interpret
  (%fd :interpret '(funcall 'progn 1))
  (:undefined-function progn))

;;; --- macros

(deftest interp-funcall-designator.macro-compile
  (%fd :compile '(funcall 'defconstant '(defconstant x 10)))
  (:undefined-function defconstant))

(deftest interp-funcall-designator.macro-interpret
  (%fd :interpret '(funcall 'defconstant '(defconstant x 10)))
  (:undefined-function defconstant))

;;; --- an undefined symbol, which was already UNDEFINED-FUNCTION

(deftest interp-funcall-designator.undefined-interpret
  (%fd :interpret '(funcall 'no-such-function-here-4711 1))
  (:undefined-function no-such-function-here-4711))

;;; --- over-fix guard: ordinary function designators are still callable
;;; (a symbol, #'name, a lambda, and the function object an FLET bound)

(deftest interp-funcall-designator.ordinary-interpret
  (%fd :interpret '(list (funcall '+ 1 2)
                    (funcall #'car '(a b))
                    (funcall (lambda (x) (* x 2)) 21)
                    (flet ((%g (x) (list :flet x))) (funcall #'%g 1))))
  (:value (3 a 42 (:flet 1))))

(deftest interp-funcall-designator.ordinary-compile
  (%fd :compile '(list (funcall '+ 1 2)
                  (funcall #'car '(a b))
                  (funcall (lambda (x) (* x 2)) 21)))
  (:value (3 a 42)))
