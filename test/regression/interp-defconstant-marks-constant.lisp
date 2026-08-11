;;; An interpreted DEFCONSTANT must mark the symbol constant.
;;;
;;; COMPILE-DEFVAR emits Runtime.SetSymbolConstant for the defconstant case; the
;;; %MINI-EVAL case only did the SET and the PROCLAIM, so a constant defined
;;; through EVAL was an ordinary special variable and CONSTANTP answered NIL.
;;;
;;; That reaches past introspection — SYMBOL-MACROLET's program-error check and
;;; anything else asking CONSTANTP saw an ordinary variable. No test here gates
;;; that consequence, though: DEFCONSTANT also proclaims the name SPECIAL, and
;;; SYMBOL-MACROLET-VIOLATION-P refuses on EITHER condition, so such a case
;;; passes with or without the constant mark. Asserting it would look like a gate
;;; and be none.
;;;
;;; On an emit-free build every DEFCONSTANT takes this path.
;;;
;;; Each mode uses its OWN symbol: a shared one is marked constant by whichever
;;; run goes first, and the other passes without doing anything.

(defun %dck (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (type-of e))))))

(deftest interp-defconstant-marks.constantp-compile
  (%dck :compile '(progn (defconstant +dck-c1+ 7) (list (constantp '+dck-c1+) +dck-c1+)))
  (t 7))

(deftest interp-defconstant-marks.constantp-interpret
  (%dck :interpret '(progn (defconstant +dck-i1+ 7) (list (constantp '+dck-i1+) +dck-i1+)))
  (t 7))

;;; --- over-fix guards ---------------------------------------------------

;;; DEFVAR / DEFPARAMETER must NOT become constants
(deftest interp-defconstant-marks.defvar-not-constant-interpret
  (%dck :interpret '(progn (defvar *dck-i2* 1) (defparameter *dck-i3* 2)
                     (list (constantp '*dck-i2*) (constantp '*dck-i3*))))
  (nil nil))

;;; the variable still works as a special
(deftest interp-defconstant-marks.still-special-interpret
  (%dck :interpret '(progn (defconstant +dck-i4+ 5)
                     (list +dck-i4+ (symbol-value '+dck-i4+))))
  (5 5))

;;; the docstring path added earlier is unaffected
(deftest interp-defconstant-marks.docstring-interpret
  (%dck :interpret '(progn (defconstant +dck-i5+ 1 "doc-k")
                     (documentation '+dck-i5+ 'variable)))
  "doc-k")
