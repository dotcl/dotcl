;;; (dotcl:function-lambda-list fn-or-name) -> (values lambda-list foundp)
;;;
;;; What an editor needs and CL does not provide: FUNCTION-LAMBDA-EXPRESSION is
;;; allowed to return NIL and does, and the recorded arity counts required
;;; parameters only, so "what are this function's arguments?" had no answer at
;;; all. A swank/slynk backend can only call what the implementation offers --
;;; SBCL has sb-introspect:function-lambda-list, ECL/Clasp ext:function-lambda-list,
;;; CLISP ext:arglist -- which is why SLIME's arglist was the one thing that
;;; stayed empty against dotcl.
;;;
;;; The second value is the contract that matters: it separates "takes no
;;; arguments" from "unknown". A caller that cannot tell those apart shows a
;;; function as taking nothing instead of falling back to :not-available.
;;;
;;; These pin the sources that exist today. A COMPILED function still answers
;;; "unknown": recording its lambda list means emitting a per-definition constant,
;;; which the current emitter can only put in the global pool -- measured at one
;;; leaked pool entry per redefinition, which
;;; DEFUN-REDEFINE-DOES-NOT-LEAK-CONSTANTS exists to prevent. Deliberately not
;;; asserted either way here, so that filling that gap does not fail this file.

(defgeneric fll-gf (a b))
(defmethod fll-gf ((a integer) (b integer)) (+ a b))

(defgeneric fll-gf-opt (a &optional b))
(defmethod fll-gf-opt ((a integer) &optional b) (list a b))

(defun %fll (name) (multiple-value-list (dotcl:function-lambda-list name)))

;;; A generic function keeps the lambda list it was defined with.

(deftest function-lambda-list.generic-function
  (%fll 'fll-gf)
  ((a b) t))

;;; Including the shape past the required parameters, which is the part ARITY
;;; cannot express.

(deftest function-lambda-list.generic-function-optional
  (%fll 'fll-gf-opt)
  ((a &optional b) t))

;;; A function object works as well as a name.

(deftest function-lambda-list.accepts-a-function-object
  (%fll #'fll-gf)
  ((a b) t))

;;; Nothing recorded is two values, both NIL -- never one value that looks like
;;; "no arguments".

(deftest function-lambda-list.unbound-name-is-two-nils
  (%fll 'fll-no-such-function-anywhere)
  (nil nil))

;;; An interpreted closure carries its lambda list in the information the
;;; tree-walk evaluator already keeps, so it answers wherever EVAL interprets
;;; rather than compiles.

(deftest function-lambda-list.interpreted-closure-contract
  (let* ((f (eval '(lambda (a &optional b &key c) (list a b c))))
         (r (%fll f)))
    ;; Which evaluator ran this depends on the build, so assert the contract that
    ;; holds either way: found means the real lambda list, not found means two
    ;; NILs. A wrong list, or a found flag with nothing behind it, fails.
    (if (second r)
        (equal (first r) '(a &optional b &key c))
        (equal r '(nil nil))))
  t)
