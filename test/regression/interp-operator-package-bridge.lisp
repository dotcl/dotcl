;;; A symbol in OPERATOR position must resolve the way a compiled call does.
;;;
;;; dotcl registers a good deal of its runtime on symbols in its own packages —
;;; DOTCL-MOP, DOTCL-INTERNAL, DOTCL — and CilAssembler bridges a bare name across
;;; those packages when it resolves a named call. So compiled code can write
;;;
;;;   (class-precedence-list (find-class 'foo))
;;;
;;; from CL-USER even though CL-USER::CLASS-PRECEDENCE-LIST has no function of its
;;; own: the compiler finds DOTCL-MOP's. FUNCALL does too, because it coerces with
;;; Runtime.CoerceToFunction, which uses the same bridge.
;;;
;;; %MINI-EVAL's operator branch called SYMBOL-FUNCTION directly, which has no
;;; bridge, so the plain form failed under the interpreter while both of the other
;;; two routes worked:
;;;
;;;   (class-precedence-list c)          ; interpreted => Undefined function
;;;   (funcall 'class-precedence-list c) ; => works
;;;   compiled (class-precedence-list c) ; => works
;;;
;;; Routing operator position through the same coercion removes the whole class,
;;; instead of giving each name its own binding one emit-free run at a time.
;;;
;;; The bridge only searches dotcl's own packages (Package.IsBridgeSource), so an
;;; arbitrary library's package still cannot answer for an undefined function.

(defclass %opb-a () ())
(defclass %opb-b (%opb-a) ())

(defun %opb (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (type-of e) (princ-to-string e))))))

;;; --- a MOP function named from CL-USER, called as a plain form

(defparameter %opb-cpl
  '(mapcar #'class-name (class-precedence-list (find-class '%opb-b))))

(deftest interp-operator-bridge.mop-cpl-compile
  (%opb :compile %opb-cpl)
  (%opb-b %opb-a standard-object t))

(deftest interp-operator-bridge.mop-cpl-interpret
  (%opb :interpret %opb-cpl)
  (%opb-b %opb-a standard-object t))

(deftest interp-operator-bridge.mop-class-slots-interpret
  (%opb :interpret '(listp (class-slots (find-class '%opb-a))))
  t)

;;; the three routes must agree — that disagreement was the bug
(deftest interp-operator-bridge.routes-agree-interpret
  (%opb :interpret '(let ((c (find-class '%opb-b)))
                     (list (length (class-precedence-list c))
                           (length (funcall 'class-precedence-list c))
                           (length (funcall #'class-precedence-list c)))))
  (4 4 4))

;;; --- over-fix guards ---------------------------------------------------

;;; a genuinely undefined name is still UNDEFINED-FUNCTION, not silently bridged
(deftest interp-operator-bridge.undefined-still-errors-interpret
  (%opb :interpret '(no-such-operator-here-8842 1 2))
  (:error undefined-function "Undefined function: NO-SUCH-OPERATOR-HERE-8842"))

;;; an ordinary CL function in operator position is unaffected
(deftest interp-operator-bridge.ordinary-call-interpret
  (%opb :interpret '(list (length (list 1 2 3)) (car '(a b)) (+ 1 2)))
  (3 a 3))

;;; a user DEFUN still wins for its own name
(deftest interp-operator-bridge.user-defun-interpret
  (%opb :interpret '(progn (defun %opb-user () :mine) (%opb-user)))
  :mine)

;;; a lexical FLET still shadows the global of the same name
(deftest interp-operator-bridge.flet-shadows-interpret
  (%opb :interpret '(flet ((class-slots (x) (list :shadowed x))) (class-slots :arg)))
  (:shadowed :arg))

;;; --- the bridge must NOT alias a package-qualified call ------------------
;;;
;;; cross-package-fn-aliasing.lisp asserts this for COMPILED calls: a qualified
;;; call to an unbound symbol signals UNDEFINED-FUNCTION rather than resolving to
;;; a same-named bound function in another package. Those tests go through
;;; COMPILE, so they cannot see the interpreted path — and routing operator
;;; position through the bridge is exactly the change that could have broken it.
;;;
;;; It holds because Package.IsBridgeSource admits only dotcl's own packages, so
;;; a user package never answers. Asserted here so the interpreted path has its
;;; own guard.

(defpackage #:%opb-lib (:use #:cl) (:export #:foo))
(defpackage #:%opb-usr (:use #:cl) (:export #:foo))
(defun %opb-usr:foo () :usr-impl)          ; bound; %opb-lib:foo deliberately is not

(deftest interp-operator-bridge.qualified-unbound-not-aliased-interpret
  (%opb :interpret '(%opb-lib:foo))
  (:error undefined-function "Undefined function: FOO"))

(deftest interp-operator-bridge.qualified-unbound-not-aliased-compile
  (%opb :compile '(%opb-lib:foo))
  (:error undefined-function "Undefined function: FOO"))
