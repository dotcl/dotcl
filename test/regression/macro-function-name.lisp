;;; FUNCTION-LAMBDA-EXPRESSION names a macro's expander (MACRO-FUNCTION name),
;;; as SBCL does, instead of NIL. Code that sorts a package's fbound symbols
;;; into functions and macros by this name (mnas-package) counted every macro
;;; as a function. Expected values are SBCL 2.6.8's answers.

(defmacro %mfn-mac (x) x)
(defun %mfn-fun (x) x)

(deftest macro-function-name.user-macro
  (nth-value 2 (function-lambda-expression (macro-function '%mfn-mac)))
  (macro-function %mfn-mac))

(deftest macro-function-name.standard-macro
  (nth-value 2 (function-lambda-expression (macro-function 'when)))
  (macro-function when))

(deftest macro-function-name.function-unchanged
  (nth-value 2 (function-lambda-expression #'%mfn-fun))
  %mfn-fun)
