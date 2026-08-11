;;; The interpreted path must also reject SYMBOL-MACROLET's PROGRAM-ERROR cases.
;;;
;;; CLHS SYMBOL-MACROLET makes three of them a program error:
;;;   * the name is a constant
;;;   * the name is a globally special variable
;;;   * the body declares that name SPECIAL
;;;
;;; COMPILE-SYMBOL-MACROLET rejected all three, but the %MINI-EVAL case had no
;;; check at all, so through EVAL a symbol macro could shadow a special variable
;;; (ansi-test SYMBOL-MACROLET.ERROR.1/2/3).
;;;
;;; The decision now lives in a single predicate, SYMBOL-MACROLET-VIOLATION-P.
;;; Written twice, the two paths drift apart again.

(defvar *smv-special* :outer)

(defun %smv (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (list :value (eval form))
      (program-error () :program-error)
      (error (e) (list :other (type-of e))))))

;;; --- the body declares one of the bound names SPECIAL

(defparameter %smv-special-decl
  '(symbol-macrolet ((x 10)) (declare (special x)) 20))

(deftest interp-symbol-macrolet-violation.special-decl-compile
  (%smv :compile %smv-special-decl)
  :program-error)

(deftest interp-symbol-macrolet-violation.special-decl-interpret
  (%smv :interpret %smv-special-decl)
  :program-error)

;;; --- the name is a constant

(defparameter %smv-constant
  '(symbol-macrolet ((most-positive-fixnum 'a)) most-positive-fixnum))

(deftest interp-symbol-macrolet-violation.constant-compile
  (%smv :compile %smv-constant)
  :program-error)

(deftest interp-symbol-macrolet-violation.constant-interpret
  (%smv :interpret %smv-constant)
  :program-error)

;;; --- the name is globally special

(defparameter %smv-special-var
  '(symbol-macrolet ((*smv-special* 19)) *smv-special*))

(deftest interp-symbol-macrolet-violation.special-var-compile
  (%smv :compile %smv-special-var)
  :program-error)

(deftest interp-symbol-macrolet-violation.special-var-interpret
  (%smv :interpret %smv-special-var)
  :program-error)

;;; --- over-fix guards: a legitimate SYMBOL-MACROLET still works. This rejects
;;; an implementation that simply always signals a program-error.

(deftest interp-symbol-macrolet-violation.ordinary-interpret
  (%smv :interpret '(symbol-macrolet ((%smv-x 10)) (+ %smv-x 1)))
  (:value 11))

(deftest interp-symbol-macrolet-violation.ordinary-compile
  (%smv :compile '(symbol-macrolet ((%smv-x 10)) (+ %smv-x 1)))
  (:value 11))

;;; a SPECIAL declaration unrelated to the bound names is allowed
(deftest interp-symbol-macrolet-violation.unrelated-special-decl-interpret
  (%smv :interpret '(symbol-macrolet ((%smv-y 10))
                     (declare (special *smv-special*))
                     (list %smv-y *smv-special*)))
  (:value (10 :outer)))
