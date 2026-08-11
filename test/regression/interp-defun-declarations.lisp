;;; An interpreted DEFUN must keep declarations OUTSIDE the implicit block.
;;;
;;; The %MINI-EVAL DEFUN case built the closure as
;;;
;;;   `(lambda ,params (block ,name ,@body))
;;;
;;; which buries (declare (special x)) inside the block. %MINI-EVAL-PROGN does not
;;; know which names the enclosing form bound when it walks the block's body, so a
;;; parameter's SPECIAL declaration looks FREE. The free rule is "read it
;;; dynamically, do not rebind", so the lexical entry is dropped and no dynamic
;;; binding is made — the body then reads an unbound variable:
;;;
;;;   (defun f (x &aux (y 10)) (declare (special x)) (+ x y))
;;;   (f 5)   ;; => Unbound variable: X
;;;
;;; %MINI-FN-LAMBDA exists to avoid exactly this and says so in its docstring.
;;; FLET / LABELS already went through it; DEFUN did not.
;;;
;;; This only shows when the DEFUN ITSELF is interpreted. The ordinary harness has
;;; LOAD compile every top-level form, so the tests below go through
;;; (eval '(defun ...)) to reach the same path an emit-free build always takes.

(defun %idd (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (type-of e) (princ-to-string e))))))

;;; --- a SPECIAL-declared parameter, with &aux

(defparameter %idd-special-aux
  '(progn (defun %idd-f1 (x &aux (y 10)) (declare (special x)) (+ x y))
    (%idd-f1 5)))

(deftest interp-defun-declarations.special-param-aux-compile
  (%idd :compile %idd-special-aux)
  15)

(deftest interp-defun-declarations.special-param-aux-interpret
  (%idd :interpret %idd-special-aux)
  15)

;;; --- and without &aux: this is about where the declaration sits, not about &aux

(defparameter %idd-special-only
  '(progn (defun %idd-f2 (x) (declare (special x)) (1+ x))
    (%idd-f2 5)))

(deftest interp-defun-declarations.special-param-compile
  (%idd :compile %idd-special-only)
  6)

(deftest interp-defun-declarations.special-param-interpret
  (%idd :interpret %idd-special-only)
  6)

;;; --- the binding must really be DYNAMIC, not merely readable lexically:
;;; another function reads the name with no lexical binding in sight

(deftest interp-defun-declarations.dynamic-binding-visible-interpret
  (%idd :interpret '(progn
                     (defun %idd-reader () (declare (special %idd-v)) %idd-v)
                     (defun %idd-f3 (%idd-v) (declare (special %idd-v)) (%idd-reader))
                     (%idd-f3 :from-param)))
  :from-param)

;;; --- a FREE special declaration must still not rebind: the reference has to
;;; reach the value an OUTER special binding established, not the shadowing one

(deftest interp-defun-declarations.free-special-not-rebound-interpret
  (%idd :interpret '(let ((x 1))
                     (declare (special x))
                     (let ((x 2) (w 5))
                       (defun %idd-f4 (&aux (q w)) (declare (special x)) (values q x))
                       (multiple-value-list (%idd-f4)))))
  (5 1))

;;; --- over-fix guards ---------------------------------------------------
;;; Hoisting declarations out must not damage the implicit block.

(deftest interp-defun-declarations.implicit-block-interpret
  (%idd :interpret '(progn (defun %idd-f5 (x) (when x (return-from %idd-f5 :early)) :late)
                     (list (%idd-f5 t) (%idd-f5 nil))))
  (:early :late))

;;; A docstring followed by several DECLARE forms: the hoisting must step over the
;;; string. Whether the docstring is RECORDED is asserted separately, in
;;; interp-defun-docstring.lisp; this case is only about the hoisting.
(deftest interp-defun-declarations.docstring-and-decls-interpret
  (%idd :interpret '(progn
                     (defun %idd-f6 (x y)
                       "doc"
                       (declare (ignorable y))
                       (declare (special x))
                       (+ x 1))
                     (%idd-f6 5 9)))
  6)

;;; An ordinary DEFUN with no declarations, exercising every lambda-list part
(deftest interp-defun-declarations.plain-interpret
  (%idd :interpret '(progn (defun %idd-f7 (a &optional (b 2) &key (c 3) &aux (d (+ a b c)))
                            (list a b c d))
                     (%idd-f7 1)))
  (1 2 3 6))

;;; (setf name) function names
(deftest interp-defun-declarations.setf-name-interpret
  (%idd :interpret '(progn
                     (defvar %idd-cell (list nil))
                     (defun (setf %idd-slot) (v obj) (setf (car obj) v))
                     (setf (%idd-slot %idd-cell) :set)
                     (car %idd-cell)))
  :set)
