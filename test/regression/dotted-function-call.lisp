;;; A function call form whose argument list is dotted is a PROGRAM-ERROR that
;;; names the call, not a LENGTH type error from inside the compiler.

(deftest-emitting-only dotted-function-call.eval
  (handler-case (eval '(car . 1)) (program-error () :program-error))
  :program-error)

(deftest-emitting-only dotted-function-call.eval-args
  (handler-case (eval '(list 1 2 . 3)) (program-error () :program-error))
  :program-error)

(deftest-emitting-only dotted-function-call.message
  (handler-case (eval '(no-such-fn-dotted . 1))
    (program-error (e) (and (search "malformed function call" (princ-to-string e)) t)))
  t)

(deftest-emitting-only dotted-function-call.macro-unaffected
  (progn
    (defmacro dotted-call-macro-probe (a . b) `(quote (,a ,b)))
    (eval '(dotted-call-macro-probe 1 . 2)))
  (1 2))
