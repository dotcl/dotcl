;;; MACROEXPAND-1 of a LAMBDA form gives (FUNCTION (LAMBDA ...)), as CLHS
;;; specifies for the LAMBDA macro.
;;;
;;; MACRO-FUNCTION of LAMBDA was non-NIL, but the expander returned the form
;;; unchanged. A code walker that expands a form and compares it with the
;;; original (iterate does) then saw "a macro that won't expand" and did not
;;; walk into the lambda body.

(deftest lambda-macroexpand-1-expands
  (multiple-value-bind (exp expanded-p) (macroexpand-1 '(lambda (c) c nil))
    (list exp (and expanded-p t)))
  ((function (lambda (c) c nil)) t))

(deftest lambda-macroexpand-reaches-function
  (car (macroexpand '(lambda (x) x)))
  function)

(deftest lambda-macro-function-agrees-with-macroexpand
  (let ((form '(lambda () 1)))
    (equal (funcall (macro-function 'lambda) form nil)
           (macroexpand-1 form)))
  t)

(deftest lambda-eval-still-a-function
  (funcall (eval '(lambda (x) (* x 2))) 21)
  42)

;;; A code walker still keeps the LAMBDA form itself (SBCL's macroexpand-all
;;; does too), and a lambda-expression operator must stay one.
(deftest lambda-macroexpand-all-keeps-lambda
  (dotcl-cltl2:macroexpand-all '(lambda (a) a))
  (lambda (a) a))

(deftest lambda-macroexpand-all-operator-kept
  (dotcl-cltl2:macroexpand-all '((lambda (a) a) 1))
  ((lambda (a) a) 1))
