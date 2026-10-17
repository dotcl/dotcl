;;; cil-compiler-float.lisp: Double-float, decimal and single-float native arithmetic
;;; Part of the CIL compiler (A2 instruction list architecture)

(in-package :dotcl.cil-compiler)

;;; ============================================================
;;; Double-float native arithmetic
;;; Parallel to the fixnum path above, but emits native r8 (IEEE 754
;;; double) arithmetic with a final newobj DoubleFloat to box the result.
;;; ============================================================

(defun %double-float-type-spec-p (ty)
  "T for a type specifier that names the double-float format: DOUBLE-FLOAT,
   LONG-FLOAT (the same format here, as the structure slot clause of
   DOUBLE-FLOAT-TYPED-P already assumes), or a range of either."
  (or (member ty '(double-float long-float))
      (and (consp ty) (member (car ty) '(double-float long-float)))))

(defun %single-float-type-spec-p (ty)
  "T for SINGLE-FLOAT or a range of it."
  (or (eq ty 'single-float)
      (and (consp ty) (eq (car ty) 'single-float))))

(defparameter +float-round-ops+
  '((ffloor floor "Math.FloorDouble") (ftruncate truncate "Math.TruncateDouble")
    (fceiling ceiling "Math.CeilingDouble") (fround round "Math.RoundDouble"))
  "The one-argument rounding functions of a float, the integer one of each, and
   the System.Math entry that rounds a double the same way.")

(defun %float-round-arg-kind (op args)
  "When ARGS is one float of a statically known format and OP one of
   +FLOAT-ROUND-OPS+ (either name) not locally rebound, that format: :SINGLE or
   :DOUBLE."
  (and (consp args) (null (cdr args))
       (or (assoc op +float-round-ops+) (find op +float-round-ops+ :key #'second))
       (not (local-function-entry op))
       (cond ((single-float-typed-p (car args)) :single)
             ((double-float-typed-p (car args)) :double))))

(defun %float-round-entry (op)
  (third (or (assoc op +float-round-ops+) (find op +float-round-ops+ :key #'second))))

(defun compile-float-round-raw (op arg kind)
  "ARG rounded by OP, left on the stack as a raw float of format KIND."
  (if (eq kind :single)
      `(,@(compile-as-single arg) (:conv-r8) (:call ,(%float-round-entry op)) (:conv-r4))
      `(,@(compile-as-double arg) (:call ,(%float-round-entry op)))))

(defun double-float-typed-p (expr)
  "Return T if EXPR is statically known to produce a DoubleFloat value.
   Recognizes: (the double-float E), local vars declared double-float
   (via *double-float-locals*), recursive binary arithmetic (+, -, *, /)
   and unary negate (- x) whose operands are themselves double-float-typed.
   A literal whose value IS a double-float (e.g. 2.0d0) is recognized: the
   read object already has a definite type, so there is no single/double
   ambiguity; only the reader's choice for an unsuffixed 2.0 was ambiguous,
   and that produces a single-float object here, not a double."
  ;; A three-argument call means the same thing as nested two-argument ones
  ;; (CLHS 12.2), but every clause below matches a two-argument call, so
  ;; (* 2.0d0 z2 aux) was not recognized as double-typed and the enclosing
  ;; + fell off the native path with it. FOLD-NARY-ARITH is what the integer
  ;; side already does for the same reason.
  (let ((expr (fold-nary-arith expr)))
  (cond
    ;; Literal double-float constant: unambiguous (the object is a double).
    ((typep expr 'double-float) t)
    ;; aref on a proven double-float-backed array local: the element is a
    ;; double, readable as a native r8 (Runtime.ArefNum*D).
    ((eq (numeric-array-aref-float-kind expr) :double) t)
    ((and (symbolp expr)
          (boundp '*double-float-locals*)
          (member (var-name expr) *double-float-locals* :test #'string=)
          (not (boxed-var-p expr))
          (lookup-local expr))
     t)
    ((and (consp expr) (eq (car expr) 'the)
          (let ((ty (cadr expr)))
            (or (eq ty 'double-float)
                (and (consp ty) (eq (car ty) 'double-float)))))
     t)
    ;; A structure slot declared DOUBLE-FLOAT. The integer sibling of this
    ;; clause is in FIXNUM-TYPED-P and exists for the same reason: the slot's
    ;; storage is a raw double, so a read that is not known to be one goes
    ;; through the generic path and boxes the value on every read.
    ((and (consp expr) (= (length expr) 2) (symbolp (car expr))
          (not (local-function-entry (car expr)))
          (boundp '*struct-accessor-types*)
          (member (gethash (car expr) *struct-accessor-types*)
                  '(double-float long-float)))
     t)
    ;; Declared double-float function return: (name ...) where name has a
    ;; return type promising a double (see FUNCTION-RETURN-TYPE for which
    ;; inferred ones count). The fixnum sibling is the last clause of
    ;; FIXNUM-TYPED-P. COMPILE-AS-DOUBLE lowers it through its generic
    ;; fallback: the call, then an unbox of the returned DoubleFloat.
    ((and (consp expr) (symbolp (car expr))
          (%double-float-type-spec-p (function-return-type (car expr)))
          ;; Must not be shadowed by a local flet/labels function.
          (not (local-function-entry (car expr))))
     t)
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(+ - * /))
          (double-float-typed-p (cadr expr))
          (double-float-typed-p (caddr expr)))
     t)
    ;; (abs x) of a double is a double -- same format, no contagion to consider.
    ((and (consp expr) (= (length expr) 2) (eq (car expr) (quote abs))
          (double-float-typed-p (cadr expr)))
     t)
    ;; Unary negate: (- x) where x is double-typed -> double.
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '-)
          (double-float-typed-p (cadr expr)))
     t)
    ;; (sin x), (exp x), ... of a double, and (float i 1d0) / (coerce i
    ;; 'double-float) of a fixnum or a float: see the predicates.
    ((double-real-unary-form-p expr) t)
    ((double-convert-form-p expr) t)
    ((and (consp expr) (assoc (car expr) +float-round-ops+)
          (eq (%float-round-arg-kind (car expr) (cdr expr)) :double))
     t)
    (t nil))))

(defparameter +double-real-unary-entries+
  '((sin . "Math.SinDouble") (cos . "Math.CosDouble") (tan . "Math.TanDouble")
    (atan . "Math.AtanDouble") (sinh . "Math.SinhDouble")
    (cosh . "Math.CoshDouble") (tanh . "Math.TanhDouble")
    (exp . "Runtime.ExpDouble"))
  "One-argument functions whose value for any double-float argument is a
   double-float (never a complex), with the raw r8 -> r8 entry each lowers to.
   LOG, SQRT, ASIN, ACOS, ACOSH and ATANH are not here: a double outside their
   real domain gives a complex. EXP signals on overflow as the generic entry
   does, so it goes through a runtime entry rather than Math.Exp.")

(defun double-real-unary-form-p (expr)
  "T when EXPR is (F X), F one of +DOUBLE-REAL-UNARY-ENTRIES+ and X statically a
   DOUBLE-FLOAT. The value is then a double-float, computed by the same
   System.Math function the generic entry calls."
  (and (consp expr) (consp (cdr expr)) (null (cddr expr))
       (symbolp (car expr))
       (assoc (car expr) +double-real-unary-entries+)
       (not (local-function-entry (car expr)))
       (double-float-typed-p (cadr expr))))

(defun double-convertible-arg-p (x)
  "T when X can become a raw double exactly as FLOAT would convert it: a
   statically FIXNUM value (conv.r8 rounds to nearest, as the generic entry's
   (double)long does) or a float of either format (widening is exact).
   FIXNUM-TYPED-P only: the overflow-checked arithmetic forms would signal where
   FLOAT of the bignum they would otherwise make is a fine double."
  (or (fixnum-typed-p x) (float-typed-p x)))

(defun double-convert-form-p (expr)
  "T when EXPR converts to a double-float a value DOUBLE-CONVERTIBLE-ARG-P
   accepts: (FLOAT X P) with P a double-float literal, (COERCE X 'DOUBLE-FLOAT)
   or 'LONG-FLOAT, or (FLOAT X) of a double (which returns X)."
  (and (consp expr) (consp (cdr expr))
       (cond
         ((eq (car expr) 'float)
          (and (not (local-function-entry 'float))
               (if (null (cddr expr))
                   (double-float-typed-p (cadr expr))
                   (and (null (cdddr expr))
                        (typep (caddr expr) 'double-float)
                        (double-convertible-arg-p (cadr expr))))))
         ((eq (car expr) 'coerce)
          (and (not (local-function-entry 'coerce))
               (consp (cddr expr)) (null (cdddr expr))
               (let ((ty (caddr expr)))
                 (and (consp ty) (eq (car ty) 'quote) (consp (cdr ty))
                      (member (cadr ty) '(double-float long-float))))
               (double-convertible-arg-p (cadr expr))))
         (t nil))))

(defun compile-double-convert (x)
  "X (DOUBLE-CONVERTIBLE-ARG-P) as a raw r8."
  (cond ((double-float-typed-p x) (compile-as-double x))
        ((single-float-typed-p x) `(,@(compile-as-single x) (:conv-r8)))
        (t `(,@(compile-expr-to-long x) (:conv-r8)))))

(defun float-native-local-kind (sym)
  "If SYM is a lexical local whose slot holds a native float (r8/r4) directly
   rather than a boxed DoubleFloat/SingleFloat, return :double / :single; else
   NIL. Gated (like the long-locals table) on a live lexical binding and non-boxed
   (captured vars keep the boxed slot, since env capture stores an object)."
  (cond
    ((and (boundp '*cstate*)
          (native-slot-p sym (cstate-native-double-locals)))
     :double)
    ((and (boundp '*cstate*)
          (native-slot-p sym (cstate-native-single-locals)))
     :single)
    (t nil)))

(defun compile-float-native-value (expr kind)
  "Compile EXPR leaving a native r8 (KIND :double) or r4 (KIND :single) on the
   stack, for storing into a native float slot. A float-typed EXPR lowers via
   compile-as-double/single (no box); anything else is compiled generically and
   unboxed; the slot's declaration promises a float, so a non-float value
   surfaces as a loud InvalidCast (matching the long-locals store contract)."
  (ecase kind
    (:double (if (double-float-typed-p expr)
                 (compile-as-double expr)
                 `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                       (compile-expr expr))
                   (:unbox-double))))
    (:single (if (single-float-typed-p expr)
                 (compile-as-single expr)
                 `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                       (compile-expr expr))
                   (:unbox-single))))))

(defun double-sqrt-form-p (expr)
  "T when EXPR is (SQRT X) or (LOG X) with X statically a DOUBLE-FLOAT.

   LOG follows exactly the rule SQRT does, for the same reason: the logarithm
   of a negative double is a COMPLEX.

   Deliberately NOT a clause of DOUBLE-FLOAT-TYPED-P, which is where every other
   shape of this kind lives: the square root of a negative double is a COMPLEX,
   so (SQRT X) alone does not have a known type even when X does. The type is
   known only where the program has ALSO declared the result -- (THE
   DOUBLE-FLOAT (SQRT X)), a double-declared binding, a double-float array
   element. Those are the callers, and each of them supplies the half this
   predicate leaves out."
  (and (consp expr) (= (length expr) 2) (member (car expr) '(sqrt log))
       (not (local-function-entry (car expr)))
       (double-float-typed-p (cadr expr))))

(defun double-sqrt-instr (&optional (op 'sqrt))
  "The call a lowered (SQRT X) emits, which is where the declaration is either
   trusted or checked. At (safety 0) a declaration is a license, as it is for
   THE FIXNUM arithmetic, and a negative argument gives the NaN Math.Sqrt
   returns. From (safety 1) up it is an assertion, so a negative argument -- the
   one input whose true answer is a COMPLEX -- signals a TYPE-ERROR."
  (if (eq op 'log)
      (if (compiling-at-safety-0-p)
          '(:call "Runtime.LogDouble")
          '(:call "Runtime.LogDoubleChecked"))
      (if (compiling-at-safety-0-p)
          '(:call "Math.SqrtDouble")
          '(:call "Runtime.SqrtDoubleChecked"))))

(defun compile-as-double (expr)
  "Compile EXPR leaving a native r8 (double) on the stack.
   Caller must have verified double-float-typed-p."
  ;; Folded the same way DOUBLE-FLOAT-TYPED-P folds it, so what this walks is
  ;; the shape that was approved.
  (let ((expr (fold-nary-arith expr)))
  (cond
    ;; A double-float literal is an IL immediate (ldc.r8). Falling through to
    ;; the generic path instead compiles it as a constant-pool reference and
    ;; unboxes that: ldc.i4 idx; call GetConstant; castclass LispObject;
    ;; castclass DoubleFloat; call get_Value. Inside a loop that whole
    ;; sequence is paid per iteration, which is why declared double-float
    ;; arithmetic lagged the same loop written over fixnums, where a literal
    ;; already lowered to ldc.i8.
    ((typep expr 'double-float)
     `((:ldc-r8 ,expr)))
    ;; A call from a typed body to itself whose result is a DOUBLE-FLOAT.
    ((and (typed-self-call-p expr)
          (eq (car (third (cstate-typed-self))) :double))
     (compile-typed-self-call expr))
    ;; Native r8 slot (double-rep local): the raw double is already in the slot.
    ((eq (float-native-local-kind expr) :double)
     `((:ldloc ,(lookup-local expr))))
    ((and (symbolp expr)
          (boundp '*double-float-locals*)
          (member (var-name expr) *double-float-locals* :test #'string=)
          (lookup-local expr))
     `((:ldloc ,(lookup-local expr))
       (:unbox-double)))
    ;; A structure slot declared DOUBLE-FLOAT -> raw r8 read, no box. The
    ;; integer sibling is COMPILE-STRUCT-SLOT-AS-LONG.
    ((and (consp expr) (= (length expr) 2) (symbolp (car expr))
          (boundp '*struct-accessor-types*)
          (member (gethash (car expr) *struct-accessor-types*)
                  '(double-float long-float))
          (struct-slot-ref expr))
     (let* ((ref (struct-slot-ref expr))
            (hit (struct-backing-entry (car ref) (cdr ref))))
       (if (and hit (eql (cddr hit) 1))
           (%struct-backing-read hit (car ref) (cdr ref))
           `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                 (compile-expr (car ref)))
             (:ldc-i4 ,(cdr ref))
             (:call "Runtime.StructRefD")))))
    ;; aref on a double-float-backed array local -> raw r8 read, no box.
    ((eq (numeric-array-aref-float-kind expr) :double)
     (compile-numeric-aref-float (cadr expr) (cddr expr)
                                 (car (numeric-array-aref-entry expr))))
    ;; (the double-float E) where E is itself provably a double: lower E the
    ;; native way instead of compiling it generically and unboxing the result.
    ;; The declaration is the only thing that made the SQRT shape reachable, and
    ;; a boxed argument to it is exactly what this path exists to avoid. Gated on
    ;; DOUBLE-FLOAT-TYPED-P of the inner form, not on the declaration: THE
    ;; promises the VALUE's type, and (the double-float (+ i j)) over two fixnums
    ;; must still go through the generic add that produces that double.
    ((and (consp expr) (eq (car expr) 'the)
          (or (double-sqrt-form-p (caddr expr))
              (double-float-typed-p (caddr expr))))
     (compile-as-double (caddr expr)))
    ((and (consp expr) (eq (car expr) 'the))
     `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
           (compile-expr (caddr expr)))
       (:unbox-double)))
    ((and (consp expr) (= (length expr) 3) (member (car expr) '(+ - * /)))
     (let ((op (ecase (car expr) (+ :add) (- :sub) (* :mul) (/ :div))))
       `(,@(compile-as-double (cadr expr))
         ,@(compile-as-double (caddr expr))
         (,op))))
    ;; (abs x) -> raw magnitude, no box on the way in or out.
    ((and (consp expr) (= (length expr) 2) (eq (car expr) (quote abs)))
     `(,@(compile-as-double (cadr expr)) (:call "Math.AbsDouble")))
    ;; (sqrt x) -> raw root. Reached only from a context that has already
    ;; declared the result a double-float; see DOUBLE-SQRT-FORM-P.
    ((double-sqrt-form-p expr)
     `(,@(compile-as-double (cadr expr)) ,(double-sqrt-instr (car expr))))
    ;; Unary negate: (- x) -> native r8 neg.
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '-))
     `(,@(compile-as-double (cadr expr)) (:neg)))
    ((double-real-unary-form-p expr)
     `(,@(compile-as-double (cadr expr))
       (:call ,(cdr (assoc (car expr) +double-real-unary-entries+)))))
    ((double-convert-form-p expr)
     (compile-double-convert (cadr expr)))
    ((and (consp expr) (assoc (car expr) +float-round-ops+)
          (eq (%float-round-arg-kind (car expr) (cdr expr)) :double))
     (compile-float-round-raw (car expr) (cadr expr) :double))
    (t
     `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
           (compile-expr expr))
       (:unbox-double))))))

(defun float-typed-p (expr)
  "T if EXPR is statically known to produce a float, of either format."
  (or (double-float-typed-p expr) (single-float-typed-p expr)))

(defun compile-as-double-widened (expr)
  "Compile a float-typed EXPR leaving a native r8, widening a single-float one.
   CLHS 12.1.4.1 compares a mixed pair in the longer format, and widening a
   single to a double is exact, so a comparison can take either format this way.
   COMPILE-AS-DOUBLE alone cannot: on a single-typed expression it falls through
   to the generic path and unboxes a SingleFloat as a DoubleFloat."
  (if (double-float-typed-p expr)
      (compile-as-double expr)
      `(,@(compile-as-single expr) (:conv-r8))))

(defun compile-double-binop (args op)
  "Emit: compile-as-double a, compile-as-double b, <op>, newobj DoubleFloat."
  `(,@(compile-as-double (first args))
    ,@(compile-as-double (second args))
    (,op)
    (:newobj "DoubleFloat")))

(defun compile-double-cmp (args op)
  "Emit native r8 comparison. OP is :lt :le :gt :ge :eq :ne.
   Leaves an i4 (0 or 1) on stack.

   NaN is unordered: <, >, <= and >= are all false against it, and only /= is
   true. <= and >= therefore negate the UNORDERED comparison (cgt.un/clt.un),
   which is true when the operands are unordered, so the negation is false --
   negating the ordered form answered true for a NaN, which is how a declared
   DOUBLE-FLOAT comparison came to disagree with the same comparison undeclared."
  (let ((body (ecase op
                (:lt (quote ((:clt))))
                (:gt (quote ((:cgt))))
                (:eq (quote ((:ceq))))
                (:le (quote ((:cgt-un) (:ldc-i4 0) (:ceq))))
                (:ge (quote ((:clt-un) (:ldc-i4 0) (:ceq))))
                (:ne '((:ceq) (:ldc-i4 0) (:ceq))))))
    `(,@(compile-as-double-widened (first args))
      ,@(compile-as-double-widened (second args))
      ,@body)))

;;; ============================================================
;;; Decimal native arithmetic
;;; Parallel to the double-float path, but emits native System.Decimal ops
;;; (decimal.op_Addition etc.) and boxes with newobj LispDecimal. The point is
;;; SEMANTIC as well as speed: System.Decimal arithmetic preserves scale, so in a
;;; (declare (type decimal ...)) scope 1.50m+2.25m stays 3.75m: the undeclared
;;; tower path instead treats the decimal by its rational value (scale lost).
;;; ============================================================

;; Whether the running host knows the DECIMAL type. Evaluated at load time in the
;; TARGET (dotcl -> T): not baked by a #+dotcl read-conditional, which would freeze
;; the cross-compile host's answer (SBCL -> NIL) into the shipped compiler. ignore-errors
;; makes the probe safe on the XC host, where DECIMAL is an unknown type specifier.
(defvar *decimal-type-available*
  (ignore-errors (progn (typep nil 'decimal) t)))

(defun decimal-literal-p (expr)
  "T if EXPR is a first-class decimal object (a #m literal). NIL (never a type error)
   on a host without the DECIMAL type, e.g. the SBCL cross-compile host."
  (and *decimal-type-available* (typep expr 'decimal)))

;; Same load-time probe shape as *decimal-type-available*: the predicate lives in
;; the DOTCL package, which the SBCL cross-compile host does not have, so it is
;; looked up by name at load time rather than read as a package-qualified symbol.
(defvar *known-type-name-p-fn*
  (ignore-errors
   (let ((sym (find-symbol "KNOWN-TYPE-NAME-P" "DOTCL")))
     (and sym (fboundp sym) (symbol-function sym)))))

;; Same shape, same reason: proclamations live on the runtime's symbols, and the
;; host has neither the package nor the flags.
(defvar *declaration-name-p-fn*
  (ignore-errors
   (let ((sym (find-symbol "DECLARATION-NAME-P" "DOTCL")))
     (and sym (fboundp sym) (symbol-function sym)))))

(defvar *warned-unknown-types* (make-hash-table :test #'equal :synchronized t)
  "Names already reported by WARN-UNKNOWN-DECLARED-TYPES, so a type that is
   declared in many places is mentioned once rather than once per binding form.")

(defun %declared-type-known-p (spec)
  "How SPEC is known as a type (:DEFTYPE / :BUILTIN / :CLASS), or NIL.
   Only bare symbols are judged: a compound specifier's head is a separate
   question, and NIL/T reach here as their own objects rather than symbols."
  (cond ((null *known-type-name-p-fn*) :builtin)   ; cross-compile host: never warn
        ((not (symbolp spec)) :builtin)
        (t (funcall *known-type-name-p-fn* spec))))

(defun warn-unknown-declared-types (body)
  "Style-warn for each type named in BODY's leading declarations that nothing
   defines as a type. Such a declaration is silently dropped, the compiler has
   nothing to attach it to, so a typo, or DECIMAL where DOTCL:DECIMAL was meant,
   costs the declaration (and any optimization or check it implied) without a
   word. Reported once per name per session."
  (when *known-type-name-p-fn*
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare))
        (return))
      (dolist (decl (cdr form))
        (when (consp decl)
          (let ((head (car decl)))
            (cond
              ;; (type SPEC var...)
              ((and (symbolp head) (string= (var-name head) "TYPE"))
               (%warn-if-unknown-type (cadr decl) nil t))
              ;; (SPEC var...) shorthand, and the variable-less (SPEC) that is
              ;; the shape of a user-proclaimed DECLARATION identifier. Both are
              ;; reported: neither names anything the compiler can attach, and a
              ;; proclaimed declaration name is excluded by the warner itself.
              ((and (symbolp head)
                    (every #'symbolp (cdr decl))
                    (not (member (var-name head)
                                 '("IGNORE" "IGNORABLE" "OPTIMIZE" "SPECIAL"
                                   "DYNAMIC-EXTENT" "INLINE" "NOTINLINE"
                                   "DECLARATION" "FTYPE" "VALUES")
                                 :test #'string=)))
               (%warn-if-unknown-type head (null (cdr decl)))))))))))

(defun %warn-if-unknown-type (spec lone-p &optional type-form-p)
  "Warn when a declared type name resolves to nothing. LONE-P is true for a
   variable-less (declare (spec)), where the likely intent is a declaration
   identifier rather than a type, so the message names the way to say that.
   TYPE-FORM-P is true for the explicit (type spec var...) form, which is a
   STYLE-WARNING as in SBCL (\"undefined type\"): the type may be defined
   later, and a full WARNING sets COMPILE-FILE's failure-p. The shorthand
   (spec var...) and the lone form stay a WARNING, as SBCL's \"unrecognized
   declaration\" is.
   Three ways to get here and they look identical in source:

   1. nothing defines it; a typo
   2. it is a dotcl extension type named through the WRONG symbol, e.g. a bare
      DECIMAL where DOTCL:DECIMAL was meant. Those are gated on symbol identity
      (KNOWN-TYPE-NAME-P answers NIL for a foreign DECIMAL), because the
      declaration-driven native paths key on the DOTCL symbol; so the
      declaration buys nothing and the arithmetic quietly degrades
   3. it is meant to be a declaration identifier that was never proclaimed
      (LONE-P). CLHS leaves an unproclaimed one undefined, and every other
      implementation warns

   Either way the compiler drops the declaration, so say so. Once per name."
  (when (and (symbolp spec)
             (null (%declared-type-known-p spec))
             ;; A symbol proclaimed (declaration foo) names no type by
             ;; definition, CLHS forbids it from naming both, so it is not an
             ;; unknown type, it is a declaration the user told us about.
             (null (and *declaration-name-p-fn*
                        (funcall *declaration-name-p-fn* spec))))
    (let ((pkg (let ((p (symbol-package spec)))
                 (if p (package-name p) "#")))
          (suggestion (%known-type-in-other-package spec)))
      (let ((key (format nil "~a::~a" pkg (symbol-name spec))))
        (unless (gethash key *warned-unknown-types*)
          (setf (gethash key *warned-unknown-types*) t)
          (if lone-p
              (warn "~a::~a is neither a known type nor a proclaimed declaration; ~
                     the declaration is ignored~@[ (did you mean ~a?)~] ~;
                     proclaim it with (declaim (declaration ~a))"
                    pkg (symbol-name spec) suggestion (symbol-name spec))
              (warn (if type-form-p 'style-warning 'simple-warning)
                    :format-control "declared type ~a::~a names no known type; the declaration is ignored~@[ (did you mean ~a?)~]"
                    :format-arguments (list pkg (symbol-name spec) suggestion))))))))

(defun %known-type-in-other-package (spec)
  "A same-named symbol from another package that IS known as a type, rendered
   as PKG:NAME. The DOTCL one wins when several exist; that is the case worth
   pointing at (a bare DECIMAL meaning DOTCL:DECIMAL)."
  (when *known-type-name-p-fn*
    (let ((hits '()))
      (dolist (p (list-all-packages))
        (multiple-value-bind (sym status) (find-symbol (symbol-name spec) p)
          (when (and status (not (eq sym spec)) (symbol-package sym)
                     (funcall *known-type-name-p-fn* sym))
            (pushnew sym hits :test #'eq))))
      (let ((best (or (find-if (lambda (s)
                                 (string= (package-name (symbol-package s)) "DOTCL"))
                               hits)
                      (and (= (length hits) 1) (first hits)))))
        (when best
          (format nil "~a:~a" (package-name (symbol-package best))
                  (symbol-name best)))))))

(defun decimal-typed-p (expr)
  "T if EXPR is statically known to produce a decimal (LispDecimal) value:
   a #m literal, a decimal-declared local (via *decimal-locals*), (the decimal E),
   or recursive binary +,-,*,/ / unary - whose operands are all decimal-typed."
  (cond
    ((decimal-literal-p expr) t)
    ((and (symbolp expr)
          (boundp '*decimal-locals*)
          (member (var-name expr) *decimal-locals* :test #'string=)
          (not (boxed-var-p expr))
          (lookup-local expr))
     t)
    ((and (consp expr) (eq (car expr) 'the)
          (let ((ty (cadr expr)))
            (or (decimal-type-name-p ty)
                (and (consp ty) (decimal-type-name-p (car ty))))))
     t)
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(+ - * /))
          (decimal-typed-p (cadr expr))
          (decimal-typed-p (caddr expr)))
     t)
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '-)
          (decimal-typed-p (cadr expr)))
     t)
    (t nil)))

(defun decimal-strong-typed-p (expr)
  "Like DECIMAL-TYPED-P, but a bare #m literal does NOT count on its own. A native
   decimal op fires only when a declared decimal or (the decimal E) is involved, so
   undeclared literal arithmetic, (+ #m1.5 #m1.5), stays on the standard tower and
   degrades to a rational (conservative-extension invariant #1: standard ops/literals
   do not spontaneously yield extended-type values). Literals are still valid OPERANDS
   of a strong decimal expression; compile-as-decimal loads them fine."
  (cond
    ((and (symbolp expr)
          (boundp '*decimal-locals*)
          (member (var-name expr) *decimal-locals* :test #'string=)
          (not (boxed-var-p expr))
          (lookup-local expr))
     t)
    ((and (consp expr) (eq (car expr) 'the)
          (let ((ty (cadr expr)))
            (or (decimal-type-name-p ty)
                (and (consp ty) (decimal-type-name-p (car ty))))))
     t)
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(+ - * /))
          (decimal-typed-p (cadr expr))
          (decimal-typed-p (caddr expr))
          (or (decimal-strong-typed-p (cadr expr))
              (decimal-strong-typed-p (caddr expr))))
     t)
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '-)
          (decimal-strong-typed-p (cadr expr)))
     t)
    (t nil)))

(defun decimal-float-mix-p (args)
  "T if the two ARGS are a declared/`the` decimal on one side and a statically
   float-typed value on the other. Such a mix has no lossless meaning (decimal is
   base-10 exact, float is binary approximate; .NET itself forbids implicit
   decimal<->double), so a declared scope rejects it and requires an explicit coerce
   ((float d) / (rational d)) rather than silently widening to double."
  (and (= (length args) 2)
       (let ((a (first args)) (b (second args)))
         (flet ((floatp* (e) (or (double-float-typed-p e) (single-float-typed-p e))))
           (or (and (decimal-strong-typed-p a) (floatp* b))
               (and (decimal-strong-typed-p b) (floatp* a)))))))

(defparameter *decimal-float-mix-error*
  "cannot mix a decimal with a float in one arithmetic op; .NET forbids implicit
 decimal<->double; coerce explicitly, e.g. (+ (rational d) x) or (float d)")

(defun decimal-native-local-p (sym)
  "T if SYM's slot holds a raw System.Decimal rather than a boxed LispDecimal.
   Gated on a live lexical binding and non-boxed, like the float slots: a
   captured variable keeps the boxed slot because env capture stores an object."
  (and (boundp '*cstate*)
       (native-slot-p sym (cstate-native-decimal-locals))))

(defun char-native-local-p (sym)
  "T if SYM's slot holds a raw character code rather than a boxed LispChar.
   Gated on a live lexical binding and non-boxed, like the float slots."
  (and (boundp '*cstate*)
       (native-slot-p sym (cstate-native-char-locals))))

(defun character-typed-p (expr)
  "T if EXPR is statically known to produce a CHARACTER **and** its code is
   reachable without building one. The second half is why this is narrower than
   the type question: it decides whether a binding may take a raw-code slot, so
   an expression whose code only exists inside a LispChar buys nothing.

   Two shapes qualify:
   - a character literal
   - (SCHAR S I), which COMPILE-STRING-CHAR-CODE-AS-LONG already reads as a code

   (CHAR S I) is deliberately NOT here even though its value is just as much a
   character: CHAR accepts strings SCHAR does not, and while the typed entry
   falls back correctly for them, a raw slot is worth having only where the init
   path is typed end to end. Widening this to CHAR later is one symbol.

   (CODE-CHAR N) is excluded for a harder reason: CLHS lets it answer NIL for a
   code at or above CHAR-CODE-LIMIT, and NIL is not a value a raw code slot can
   hold. The integer and float slots have no equivalent of that."
  (setf expr (fold-constant-symbol expr))
  (or (characterp expr)
      (and (consp expr) (= (length expr) 3) (eq (car expr) 'schar)
           (not (local-function-entry 'schar)))))

(defun compile-char-native-value (expr)
  "Compile EXPR leaving a raw character code on the stack as an int64, for
   storing into a native character slot. CHARACTER-TYPED-P has already accepted
   EXPR.

   int64 rather than the int32 the code actually needs, because that is the width
   every other character code on this path already has: CHAR-CODE-OPERAND's other
   two shapes leave an i8, and the comparison that consumes them cannot mix
   widths. Keeping the slot at i8 makes the hot read -- the comparison -- a bare
   LDLOC, and puts the one narrowing on the generic read, which is the rare one."
  (setf expr (fold-constant-symbol expr))
  (cond
    ((characterp expr) `((:ldc-i8 ,(char-code expr))))
    ((character-typed-p expr)
     (compile-string-char-code-as-long (cadr expr) (caddr expr)))
    ;; Anything else is compiled generically and reduced to its code. Reached
    ;; from SETQ, not from the binding: the binding only takes a raw slot when
    ;; CHARACTER-TYPED-P accepted its init, but a later assignment may be any
    ;; expression. The slot's declaration promises a character, and
    ;; CHAR-CODE-OF-L reports a value that is not one as the TYPE-ERROR the
    ;; broken promise is, rather than as a .NET cast failure.
    (t `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
             (compile-expr expr))
         (:call "Runtime.CharCodeOfL")))))

(defun compile-as-decimal (expr)
  "Compile EXPR leaving a native System.Decimal (valuetype) on the stack.
   Caller must have verified decimal-typed-p."
  (cond
    ;; Native decimal slot: the raw value is already there.
    ((decimal-native-local-p expr)
     `((:ldloc ,(lookup-local expr))))
    ((and (symbolp expr)
          (boundp '*decimal-locals*)
          (member (var-name expr) *decimal-locals* :test #'string=)
          (lookup-local expr))
     `((:ldloc ,(lookup-local expr))
       (:unbox-decimal)))
    ((and (consp expr) (eq (car expr) 'the))
     `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
           (compile-expr (caddr expr)))
       (:unbox-decimal)))
    ((and (consp expr) (= (length expr) 3) (member (car expr) '(+ - * /)))
     (let ((op (ecase (car expr) (+ :decimal-add) (- :decimal-sub)
                                 (* :decimal-mul) (/ :decimal-div))))
       `(,@(compile-as-decimal (cadr expr))
         ,@(compile-as-decimal (caddr expr))
         (,op))))
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '-))
     `(,@(compile-as-decimal (cadr expr)) (:decimal-neg)))
    (t
     `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
           (compile-expr expr))
       (:unbox-decimal)))))

(defun compile-decimal-native-value (expr)
  "Compile EXPR leaving a raw System.Decimal on the stack, for storing into a
   native decimal slot. A decimal-typed EXPR lowers through compile-as-decimal
   (no box); anything else is compiled generically and unboxed; the slot's
   declaration promises a decimal, so a non-decimal value surfaces as a loud
   InvalidCast, matching the native float slots' store contract."
  (if (decimal-typed-p expr)
      (compile-as-decimal expr)
      `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
            (compile-expr expr))
        (:unbox-decimal))))

(defun compile-decimal-binop (args op)
  "Emit: compile-as-decimal a, compile-as-decimal b, <decimal op>, newobj LispDecimal."
  `(,@(compile-as-decimal (first args))
    ,@(compile-as-decimal (second args))
    (,op)
    (:newobj "LispDecimal")))

;;; ============================================================
;;; Single-float native arithmetic
;;; Parallel to the double-float path above, but emits native r4 (IEEE 754
;;; single) arithmetic with a final conv.r4 + newobj SingleFloat to box.
;;; ============================================================

(defun single-float-typed-p (expr)
  "Return T if EXPR is statically known to produce a SingleFloat value.
   Recognizes: (the single-float E), local vars declared single-float
   (via *single-float-locals*), and recursive arithmetic (+, -, *, /)
   whose operands are themselves single-float-typed. A literal single-float
   object (e.g. 2.0f0, or 2.0 under the default read format) is recognized;
   its type is already definite."
  ;; See DOUBLE-FLOAT-TYPED-P: a three-argument call means nested two-argument
  ;; ones, and every clause below matches only the two-argument shape.
  (let ((expr (fold-nary-arith expr)))
  (cond
    ;; Literal single-float constant: unambiguous (the object is a single).
    ((typep expr 'single-float) t)
    ((and (consp expr) (member (car expr) *macrolet-shadowed* :test #'eq)) nil)
    ((and (symbolp expr) (assoc expr *single-assumed* :test #'eq))
     (cdr (assoc expr *single-assumed* :test #'eq)))
    ;; aref on a proven single-float-backed array local: element is a single,
    ;; readable via Runtime.ArefNum*D (double) narrowed back with conv.r4.
    ((and (not (%mentions-assumed-p expr))
          (eq (numeric-array-aref-float-kind expr) :single))
     t)
    ((and (symbolp expr)
          (boundp '*single-float-locals*)
          (member (var-name expr) *single-float-locals* :test #'string=)
          (not (boxed-var-p expr))
          (lookup-local expr))
     t)
    ((and (consp expr) (eq (car expr) 'the)
          (let ((ty (cadr expr)))
            (or (eq ty 'single-float)
                (and (consp ty) (eq (car ty) 'single-float)))))
     t)
    ;; A structure slot declared SINGLE-FLOAT; see the same clause in
    ;; DOUBLE-FLOAT-TYPED-P.
    ((and (consp expr) (= (length expr) 2) (symbolp (car expr))
          (not (local-function-entry (car expr)))
          (boundp '*struct-accessor-types*)
          (member (gethash (car expr) *struct-accessor-types*)
                  '(single-float short-float)))
     t)
    ;; Declared single-float function return; see the same clause in
    ;; DOUBLE-FLOAT-TYPED-P.
    ((and (consp expr) (symbolp (car expr))
          (%single-float-type-spec-p (function-return-type (car expr)))
          (not (local-function-entry (car expr))))
     t)
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(+ - * /))
          (not (local-function-entry (car expr)))
          (let ((a (single-float-typed-p (cadr expr)))
                (b (single-float-typed-p (caddr expr))))
            (or (and a b)
                (and a (%single-fixnum-operand-p (caddr expr)))
                (and b (%single-fixnum-operand-p (cadr expr))))))
     t)
    ;; (abs x) of a single is a single.
    ((and (consp expr) (= (length expr) 2) (eq (car expr) (quote abs))
          (single-float-typed-p (cadr expr)))
     t)
    ;; Unary negate: (- x) where x is single-typed -> single.
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '-)
          (single-float-typed-p (cadr expr)))
     t)
    ((single-convert-form-p expr) t)
    ((and (consp expr) (assoc (car expr) +float-round-ops+)
          (eq (%float-round-arg-kind (car expr) (cdr expr)) :single))
     t)
    ((and (consp expr) (member (car expr) '(let let*)) (consp (cdr expr)))
     (%single-let-value-p expr))
    ;; MACROLET: the value of its last form, the local macro names shadowing
    ;; whatever they name outside.
    ((and (consp expr) (eq (car expr) 'macrolet) (consp (cdr expr)) (listp (cadr expr))
          (consp (cddr expr)))
     (let ((*macrolet-shadowed* (append (mapcar (lambda (d) (and (consp d) (car d)))
                                                (cadr expr))
                                        *macrolet-shadowed*)))
       (single-float-typed-p (car (last (cddr expr))))))
    ((and (consp expr) (symbolp (car expr)) (local-function-entry (car expr)))
     (%local-inline-call-single-p expr))
    (t nil))))

(defun %mentions-assumed-p (x)
  "True when X names a variable of *SINGLE-ASSUMED* outside QUOTE: the typing
   predicates that read bindings would see the outer one instead."
  (and *single-assumed*
       (labels ((walk (y)
                  (cond ((symbolp y) (and y (assoc y *single-assumed* :test #'eq)))
                        ((not (consp y)) nil)
                        ((eq (car y) 'quote) nil)
                        (t (do ((r y (cdr r)))
                               ((not (consp r)) (walk r))
                             (when (walk (car r)) (return t)))))))
         (walk x))))

(defun %single-fixnum-operand-p (x)
  ;; (THE <integer type> E) is typed by the declaration alone, whatever E reads.
  (and (or (not (%mentions-assumed-p x))
           (and (consp x) (eq (car x) 'the) (consp (cdr x))))
       (fixnum-typed-p x)))

(defun %single-convertible-p (x)
  (or (single-float-typed-p x)
      (%single-fixnum-operand-p x)
      (and (not (%mentions-assumed-p x)) (double-convertible-arg-p x))))

(defun single-convert-form-p (expr)
  "T when EXPR converts to a single-float a fixnum or a float: (FLOAT X P) with
   P a single-float literal, (FLOAT X) of a fixnum or a single, (COERCE X
   'SINGLE-FLOAT)."
  (and (consp expr) (consp (cdr expr))
       (cond
         ((eq (car expr) 'float)
          (and (not (local-function-entry 'float))
               (if (null (cddr expr))
                   (or (single-float-typed-p (cadr expr))
                       (%single-fixnum-operand-p (cadr expr)))
                   (and (null (cdddr expr))
                        (typep (caddr expr) 'single-float)
                        (%single-convertible-p (cadr expr))))))
         ((eq (car expr) 'coerce)
          (and (not (local-function-entry 'coerce))
               (consp (cddr expr)) (null (cdddr expr))
               (let ((ty (caddr expr)))
                 (and (consp ty) (eq (car ty) 'quote) (consp (cdr ty))
                      (member (cadr ty) '(single-float short-float))))
               (%single-convertible-p (cadr expr))))
         (t nil))))

(defun %single-declared-p (var decls)
  (dolist (d decls nil)
    (when (and (consp d) (eq (car d) 'declare))
      (dolist (spec (cdr d))
        (when (and (consp spec)
                   (if (eq (car spec) 'type)
                       (and (%single-float-type-spec-p (cadr spec))
                            (member var (cddr spec) :test #'eq))
                       (and (%single-float-type-spec-p (car spec))
                            (member var (cdr spec) :test #'eq))))
          (return-from %single-declared-p t))))))

(defun %single-let-value-p (expr)
  "SINGLE-FLOAT-TYPED-P of (LET / LET* BINDINGS DECL... FORM... ): its last
   form, each variable single when declared so or, unassigned, bound to a single."
  (let* ((bindings (cadr expr))
         (body (cddr expr))
         (decls (loop for f in body while (and (consp f) (eq (car f) 'declare)) collect f))
         (forms (nthcdr (length decls) body)))
    (when (and forms (listp bindings))
      (let* ((vars (mapcar (lambda (b) (if (consp b) (car b) b)) bindings))
             (mutated (and (every #'symbolp vars)
                           (values (find-mutated-and-captured-vars
                                    forms (mapcar #'var-name vars)))))
             (env *single-assumed*)
             (new '()))
        (dolist (b bindings)
          (let* ((v (if (consp b) (car b) b))
                 (init (and (consp b) (cadr b))))
            (unless (and (symbolp v) v (not (global-special-p v))) (return-from %single-let-value-p nil))
            (let ((typed (or (%single-declared-p v decls)
                             (and init
                                  (not (member (var-name v) mutated :test #'string=))
                                  (let ((*single-assumed* (if (eq (car expr) 'let) env
                                                              (append new env))))
                                    (single-float-typed-p init))))))
              (push (cons v (and typed t)) new))))
        (let ((*single-assumed* (append new env)))
          (single-float-typed-p (car (last forms))))))))

(defun %local-inline-call-single-p (expr)
  "T when EXPR is a call of a local function substituted here whose one body
   form is single-float typed, each parameter single when declared so or when
   its argument is."
  (let ((plan (nth 5 (local-function-entry (car expr)))))
    (when (and plan
               (let ((*local-inline-report* nil)
                     (*local-inline-probe* t))
                 (maybe-expand-local-inline (car expr) expr)))
      (destructuring-bind (name params decls body blockp &rest more) plan
        (declare (ignore more))
        (when (and (not (member name *inlining-stack*))
                   (consp body) (null (cdr body))
                   (not (and blockp (form-has-return-from-p name (car body))))
                   (= (length params) (length (cdr expr))))
          (let ((*single-assumed*
                  (append (loop for p in params
                                for a in (cdr expr)
                                collect (cons p (and (or (%single-declared-p p decls)
                                                         (single-float-typed-p a))
                                                     t)))
                          *single-assumed*))
                (*inlining-stack* (inline-guard-stack name)))
            (single-float-typed-p (car body))))))))

(defun compile-as-single (expr)
  "Compile EXPR leaving a native r4 (float) on the stack.
   Caller must have verified single-float-typed-p."
  ;; Folded the same way SINGLE-FLOAT-TYPED-P folds it.
  (let ((expr (fold-nary-arith expr)))
  (cond
    ;; Literal, as an IL immediate (ldc.r4) -- see the note in
    ;; COMPILE-AS-DOUBLE for what the generic constant-pool path costs.
    ((typep expr 'single-float)
     `((:ldc-r4 ,expr)))
    ;; Native r4 slot (single-rep local): the raw float is already in the slot.
    ((eq (float-native-local-kind expr) :single)
     `((:ldloc ,(lookup-local expr))))
    ((and (symbolp expr)
          (boundp '*single-float-locals*)
          (member (var-name expr) *single-float-locals* :test #'string=)
          (lookup-local expr))
     `((:ldloc ,(lookup-local expr))
       (:unbox-single)))
    ((and (consp expr) (member (car expr) '(let let*)) (consp (cdr expr))
          (listp (cadr expr))
          (%boolean-branchable-let-p (cadr expr) (cddr expr)))
     (let ((*in-tail-position* nil) (*in-mv-context* nil))
       (compile-let (cadr expr) (cddr expr) (eq (car expr) 'let*)
                    (lambda (forms)
                      `(,@(%compile-statements (butlast forms))
                        ,@(compile-as-single (car (last forms))))))))
    ((and (consp expr) (symbolp (car expr)) (car expr)
          (local-function-entry (car expr))
          (not (%lexical-macro-p (car expr)))
          (%local-inline-call-single-p expr))
     (let ((inl (maybe-expand-local-inline (car expr) expr)))
       (if inl
           (let ((*inlining-stack* (cons (car expr) *inlining-stack*)))
             (compile-as-single inl))
           `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                 (compile-expr expr))
             (:unbox-single)))))
    ;; The integer operand of a mixed single-float operation.
    ((and (not (single-float-typed-p expr)) (fixnum-typed-p expr))
     `(,@(compile-expr-to-long expr) (:conv-r4)))
    ((single-convert-form-p expr)
     (let ((x (cadr expr)))
       (cond ((single-float-typed-p x) (compile-as-single x))
             ((double-float-typed-p x) `(,@(compile-as-double x) (:conv-r4)))
             (t `(,@(compile-expr-to-long x) (:conv-r4))))))
    ;; A structure slot declared SINGLE-FLOAT -> raw r4 read, no box.
    ((and (consp expr) (= (length expr) 2) (symbolp (car expr))
          (boundp '*struct-accessor-types*)
          (member (gethash (car expr) *struct-accessor-types*)
                  '(single-float short-float))
          (struct-slot-ref expr))
     (let* ((ref (struct-slot-ref expr))
            (hit (struct-backing-entry (car ref) (cdr ref))))
       (if (and hit (eql (cddr hit) 2))
           (%struct-backing-read hit (car ref) (cdr ref))
           `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                 (compile-expr (car ref)))
             (:ldc-i4 ,(cdr ref))
             (:call "Runtime.StructRefF")))))
    ;; aref on a single-float-backed array local -> raw r8 read (widened float),
    ;; narrowed back to r4 with conv.r4 (exact for single-float values).
    ((eq (numeric-array-aref-float-kind expr) :single)
     `(,@(compile-numeric-aref-float (cadr expr) (cddr expr)
                                     (car (numeric-array-aref-entry expr)))
       (:conv-r4)))
    ((and (consp expr) (assoc (car expr) +float-round-ops+)
          (eq (%float-round-arg-kind (car expr) (cdr expr)) :single))
     (compile-float-round-raw (car expr) (cadr expr) :single))
    ((and (consp expr) (eq (car expr) 'the))
     `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
           (compile-expr (caddr expr)))
       (:unbox-single)))
    ((and (consp expr) (= (length expr) 3) (member (car expr) '(+ - * /)))
     (let ((op (ecase (car expr) (+ :add) (- :sub) (* :mul) (/ :div))))
       `(,@(compile-as-single (cadr expr))
         ,@(compile-as-single (caddr expr))
         (,op))))
    ;; (abs x) -> raw magnitude, no box on the way in or out.
    ((and (consp expr) (= (length expr) 2) (eq (car expr) (quote abs)))
     `(,@(compile-as-single (cadr expr)) (:conv-r4) (:call "Math.AbsSingle")))
    ;; Unary negate: (- x) -> native r4 neg.
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '-))
     `(,@(compile-as-single (cadr expr)) (:neg)))
    (t
     `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
           (compile-expr expr))
       (:unbox-single))))))

(defun compile-single-binop (args op)
  "Emit: compile-as-single a, compile-as-single b, <op>, conv.r4, newobj SingleFloat."
  `(,@(compile-as-single (first args))
    ,@(compile-as-single (second args))
    (,op)
    (:conv-r4)
    (:newobj "SingleFloat")))

(defun compile-single-cmp (args op)
  "Emit native r4 comparison. OP is :lt :le :gt :ge :eq :ne.
   Leaves an i4 (0 or 1) on stack.

   See COMPILE-DOUBLE-CMP for why <= and >= negate the unordered comparison."
  (let ((body (ecase op
                (:lt (quote ((:clt))))
                (:gt (quote ((:cgt))))
                (:eq (quote ((:ceq))))
                (:le (quote ((:cgt-un) (:ldc-i4 0) (:ceq))))
                (:ge (quote ((:clt-un) (:ldc-i4 0) (:ceq))))
                (:ne '((:ceq) (:ldc-i4 0) (:ceq))))))
    `(,@(compile-as-single (first args))
      ,@(compile-as-single (second args))
      ,@body)))

(defun %nary-fixnum-foldable-p (op args)
  "True when (OP . ARGS) should keep folding into binary calls past the arity
   cutoff below, because every step will take the native fixnum path.

   The cutoff exists to stop a long argument list from expanding into a chain of
   binary calls; the array form is one call instead. But it is keyed on the
   count alone, so a fixnum-declared (+ x x ... x) of nine arguments falls to
   Runtime.AddN, which takes LispObject[] -- and building that array costs MORE
   than the chain it replaced. A fixnum declaration then made the code allocate
   where the undeclared form allocated nothing, which is backwards.

   Every argument has to be fixnum-typed for this: the fold re-tests the pair at
   each step, and one generic argument puts the accumulator back on the boxed
   path, where the chain has no advantage over the array.

   Overflow is NOT asked about here, and must not be. Whether the whole sum fits
   int64 is exactly what the range prover cannot show for an unbounded fixnum,
   so requiring it would decline every case this exists for. The fold does not
   need it: each binary step tests FIXNUM-ARITH-UNBOXED-SAFE-P for itself and
   takes the promoting Runtime.Add when the proof fails, so an overflowing
   intermediate still becomes a bignum. All this predicate decides is chain
   versus array, and the chain is never less correct than the array."
  (declare (ignore op))
  (and (fixnum-typed-p (first args))
       (fixnum-typed-p (second args))
       (every #'fixnum-typed-p (cddr args))))

(defun compile-add (args)
  (case (length args)
    (0 (emit-fixnum 0))
    (1 (let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args))))
    (2
     (cond
       ;; Both args known decimal -> native System.Decimal add (scale preserved).
       ((and (decimal-typed-p (first args)) (decimal-typed-p (second args))
             (or (decimal-strong-typed-p (first args)) (decimal-strong-typed-p (second args))))
        (compile-decimal-binop args :decimal-add))
       ((decimal-float-mix-p args)
        (compile-static-program-error *decimal-float-mix-error*))
       ;; Fast path: both args known double-float -> native r8 add
       ((and (double-float-typed-p (first args)) (double-float-typed-p (second args)))
        (compile-double-binop args :add))
       ;; Fast path: both args known single-float -> native r4 add
       ((and (single-float-typed-p (first args)) (single-float-typed-p (second args)))
        (compile-single-binop args :add))
       ;; Fast path: both args known fixnum AND no intermediate int64 overflow
       ;; provable -> native int64 add. Otherwise fall to the promoting Runtime.Add
       ;; so an overflowing result becomes a bignum.
       ((fixnum-binop-native-p '+ args)
        (compile-fixnum-binop args :add))
       ;; Optimize (+ x 1) and (+ 1 x) to Increment
       ((eql (second args) 1)
        (compile-unary-call (list (first args)) "Runtime.Increment" "1+"))
       ((eql (first args) 1)
        (compile-unary-call (list (second args)) "Runtime.Increment" "1+"))
       (t (compile-binary-call args "Runtime.Add"))))
    (t (if (or (<= (length args) 8) (%nary-fixnum-foldable-p '+ args))
           (compile-expr (cons '+ (cons (list '+ (first args) (second args)) (cddr args))))
           `(,@(compile-args-array args) (:call "Runtime.AddN"))))))

(defun compile-sub (args)
  (case (length args)
    (0 (compile-static-program-error "-: too few arguments: 0 (expected at least 1)"))
    (1 (cond
         ;; Native unary negate for a strongly-decimal operand (scale preserved). A bare
         ;; #m literal is NOT strong, so (- #m1.5) degrades to a rational like other
         ;; undeclared literal arithmetic.
         ((decimal-strong-typed-p (first args))
          `(,@(compile-as-decimal (first args)) (:decimal-neg) (:newobj "LispDecimal")))
         ;; Native unary negate for float-typed operand.
         ((double-float-typed-p (first args))
          `(,@(compile-as-double (first args)) (:neg) (:newobj "DoubleFloat")))
         ((single-float-typed-p (first args))
          `(,@(compile-as-single (first args)) (:neg) (:conv-r4) (:newobj "SingleFloat")))
         ;; Unary (- x): Subtract(0, x) would collapse -0.0 (0.0-0.0=+0.0), so use Negate
         ;; (IEEE sign flip). Needed for signed-zero correctness of conjugate/eql.
         (t (compile-unary-call (list (first args)) "Runtime.Negate" "-"))))
    (2
     (cond
       ((and (decimal-typed-p (first args)) (decimal-typed-p (second args))
             (or (decimal-strong-typed-p (first args)) (decimal-strong-typed-p (second args))))
        (compile-decimal-binop args :decimal-sub))
       ((decimal-float-mix-p args)
        (compile-static-program-error *decimal-float-mix-error*))
       ((and (double-float-typed-p (first args)) (double-float-typed-p (second args)))
        (compile-double-binop args :sub))
       ((and (single-float-typed-p (first args)) (single-float-typed-p (second args)))
        (compile-single-binop args :sub))
       ((fixnum-binop-native-p '- args)
        (compile-fixnum-binop args :sub))
       ((eql (second args) 1)
        (compile-unary-call (list (first args)) "Runtime.Decrement" "1-"))
       (t (compile-binary-call args "Runtime.Subtract"))))
    (t (if (or (<= (length args) 8) (%nary-fixnum-foldable-p '- args))
           (compile-expr (cons '- (cons (list '- (first args) (second args)) (cddr args))))
           `(,@(compile-args-array args) (:call "Runtime.SubtractN"))))))

(defun compile-mul (args)
  (case (length args)
    (0 (emit-fixnum 1))
    (1 (let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args))))
    (2
     (cond
       ((and (decimal-typed-p (first args)) (decimal-typed-p (second args))
             (or (decimal-strong-typed-p (first args)) (decimal-strong-typed-p (second args))))
        (compile-decimal-binop args :decimal-mul))
       ((decimal-float-mix-p args)
        (compile-static-program-error *decimal-float-mix-error*))
       ((and (double-float-typed-p (first args)) (double-float-typed-p (second args)))
        (compile-double-binop args :mul))
       ((and (single-float-typed-p (first args)) (single-float-typed-p (second args)))
        (compile-single-binop args :mul))
       ((fixnum-binop-native-p '* args)
        (compile-fixnum-binop args :mul))
       (t (compile-binary-call args "Runtime.Multiply"))))
    (t (if (or (<= (length args) 8) (%nary-fixnum-foldable-p '* args))
           (compile-expr (cons '* (cons (list '* (first args) (second args)) (cddr args))))
           `(,@(compile-args-array args) (:call "Runtime.MultiplyN"))))))

(defun compile-div (args)
  (case (length args)
    (0 (compile-static-program-error "/: too few arguments: 0 (expected at least 1)"))
    (1 (compile-binary-call (list 1 (first args)) "Runtime.Divide"))
    (2
     (cond
       ;; Both decimal -> native decimal divide (System.Decimal semantics: rounds
       ;; to 28 significant digits, unlike CL's exact rational division).
       ((and (decimal-typed-p (first args)) (decimal-typed-p (second args))
             (or (decimal-strong-typed-p (first args)) (decimal-strong-typed-p (second args))))
        (compile-decimal-binop args :decimal-div))
       ((decimal-float-mix-p args)
        (compile-static-program-error *decimal-float-mix-error*))
       ((and (double-float-typed-p (first args)) (double-float-typed-p (second args)))
        (compile-double-binop args :div))
       ((and (single-float-typed-p (first args)) (single-float-typed-p (second args)))
        (compile-single-binop args :div))
       (t (compile-binary-call args "Runtime.Divide"))))
    (t (if (or (<= (length args) 8) (%nary-fixnum-foldable-p '/ args))
           (compile-expr (cons '/ (cons (list '/ (first args) (second args)) (cddr args))))
           `(,@(compile-args-array args) (:call "Runtime.DivideN"))))))

;;; ============================================================
;;; DOTCL-SIMD calls as raw Vector128<T>
;;;
;;; A nest of DOTCL-SIMD operations is compiled to Vector128<T> values on the
;;; evaluation stack, through the closed generic methods of SimdLower. A pack
;;; is boxed only where the value leaves the nest. Any argument that is not
;;; such an operation is evaluated as usual and cast as the function would.
;;; ============================================================

(defvar *fasl-corlib* nil
  "The corlib COMPILE-FILE retargets the fasl to (\"netstandard\"), or NIL. A
   netstandard fasl cannot refer to Vector128, so no DOTCL-SIMD call is lowered.")

(defparameter +simd-kinds+
  '(("F64.2" "Double" t :f64) ("F32.4" "Single" t :f32)
    ("S64.2" "Int64" nil 4 (-9223372036854775808 . 9223372036854775807))
    ("U64.2" "UInt64" nil :u64 (0 . 18446744073709551615))
    ("S32.4" "Int32" nil 3 (-2147483648 . 2147483647))
    ("U32.4" "UInt32" nil nil) ("S16.8" "Int16" nil nil)
    ("U16.8" "UInt16" nil 2 (0 . 65535))
    ("S8.16" "SByte" nil nil)
    ("U8.16" "Byte" nil 1 (0 . 255)))
  "(NAME LANE-TYPE FLOATP BACKING-KIND ELEMENT-RANGE) for each pack type. A type
   with a BACKING-KIND has a buffer the array reader can use directly.")

(defparameter +simd-ops+
  '(("+" "Add" 2) ("-" "Sub" 2) ("*" "Mul" 2) ("/" "Div" 2 t)
    ("-MIN" "Min" 2) ("-MAX" "Max" 2)
    ("-AND" "And" 2) ("-OR" "Or" 2) ("-XOR" "Xor" 2) ("-ANDC1" "AndC1" 2)
    ("-NOT" "Not" 1) ("-SQRT" "Sqrt" 1 t)
    ("=" "Eq" 2 nil :cmp) ("/=" "Ne" 2 nil :cmp) ("<" "Lt" 2 nil :cmp)
    ("<=" "Le" 2 nil :cmp) (">" "Gt" 2 nil :cmp) (">=" "Ge" 2 nil :cmp)
    ("-IF" "If" 3 nil :if)
    ("-SHIFTL" "Shl" 2 :int :shift) ("-SHIFTR" "Shr" 2 :int :shift)
    ("-HORIZONTAL+" "HAdd" 1 nil :horizontal) ("-HORIZONTAL-MIN" "HMin" 1 nil :horizontal)
    ("-HORIZONTAL-MAX" "HMax" 1 nil :horizontal)
    ("-AREF" :aref 2) ("" :cast 1))
  "(SUFFIX METHOD ARITY ONLY CLASS) for each lowered operation. ONLY is T for a
   float type only and :INT for an integer type only.")

(defparameter +simd-conversions+
  '(("F32.4" "S32.4") ("F32.4" "U32.4") ("S32.4" "F32.4") ("U32.4" "F32.4")
    ("F64.2" "S64.2") ("F64.2" "U64.2") ("S64.2" "F64.2") ("U64.2" "F64.2"))
  "(TO FROM) of the conversions lowered. The ones between f64.2 and f32.4 stay calls.")

(defun %simd-lowering-p ()
  (and (not *cross-compiling*)
       (not (equalp *fasl-corlib* "netstandard"))))

(defun %simd-kind-named (name)
  (find name +simd-kinds+ :key #'first :test #'string=))

(defun %simd-op-for (kind rest)
  "The +SIMD-OPS+ entry of KIND's operation REST, or for a conversion a fresh
   entry (SUFFIX \"Cvt\" 1 NIL :CONVERT FROM-KIND)."
  (let ((op (find rest +simd-ops+ :key #'first :test #'string=)))
    (cond (op (and (case (fourth op)
                     ((nil) t)
                     ((t) (third kind))
                     (t (not (third kind))))
                   op))
          ((and (> (length rest) 6) (string= "-FROM-" rest :end2 6))
           (let ((from (subseq rest 6)))
             (and (member (list (first kind) from) +simd-conversions+ :test #'equal)
                  (list rest "Cvt" 1 nil :convert (%simd-kind-named from))))))))

(defun %simd-name-info (sym)
  "(KIND OP) for a DOTCL-SIMD function name SYM that is lowered, else NIL."
  (let ((pkg (and (symbolp sym) (symbol-package sym))))
    (when (and pkg (string= (package-name pkg) "DOTCL-SIMD"))
      (let ((name (symbol-name sym)))
        (dolist (k +simd-kinds+)
          (let ((kn (first k)))
            (when (and (>= (length name) (length kn))
                       (string= kn name :end2 (length kn)))
              (let ((op (%simd-op-for k (subseq name (length kn)))))
                (return (and op (list k op)))))))))))

(defun %simd-lane-bits (kind)
  (let ((name (first kind)))
    (parse-integer name :start 1 :end (position #\. name))))

(defun %simd-mask-kind (kind)
  (%simd-kind-named (format nil "U~D.~D" (%simd-lane-bits kind) (/ 128 (%simd-lane-bits kind)))))

(defun %simd-op-class (node) (fifth (second node)))

(defun %simd-result-kind (node)
  "The kind of the pack NODE computes, or NIL for a horizontal reduction."
  (case (%simd-op-class node)
    (:cmp (%simd-mask-kind (first node)))
    (:horizontal nil)
    (t (first node))))

(defun %simd-arg-kinds (node)
  "The kind each argument of NODE is cast to; :COUNT for a literal shift count."
  (let ((kind (first node)) (op (second node)))
    (case (fifth op)
      (:if (list (%simd-mask-kind kind) kind kind))
      (:shift (list kind :count))
      (:convert (list (sixth op)))
      (t (make-list (third op) :initial-element kind)))))

(defun %simd-callable-p (name)
  (not (or (local-function-entry name)
           (macrolet-shadowed-p name)
           (%lexical-macro-p name)
           (member name *notinline-functions* :test #'equal)
           (%global-notinline-p name))))

(defun %simd-node (expr)
  "(KIND OP) when EXPR is a call of a DOTCL-SIMD operation this compiler lowers here."
  (and (consp expr)
       (symbolp (car expr))
       (%simd-lowering-p)
       (let ((info (%simd-name-info (car expr))))
         (and info
              (eql (length (cdr expr)) (third (second info)))
              (%simd-callable-p (car expr))
              (or (not (eq (fifth (second info)) :shift))
                  (typep (third expr) `(integer 0 ,(1- (%simd-lane-bits (first info))))))
              info))))

(defun %simd-lane (kind) (second kind))

(defun %simd-call (method kind)
  `(:call ,(concatenate 'string "Simd." method "." (%simd-lane kind))))

(defun %simd-aref-backing (kind arr idx)
  "ARR's hoisted rank 1 buffer of KIND's lane type, or NIL."
  (and (fourth kind)
       (symbolp arr)
       (let ((e (array-backing-entry (list 'aref arr idx))))
         (and e
              (eq (fourth e) (fourth kind))
              (or (third kind)
                  (and (null (fifth e))
                       (equal (and (eighth e) (integer-type-range (eighth e)))
                              (fifth kind))))
              (null (backing-entry-dims e))
              (not (eq (%backing-arm-for e) :helper))
              e))))

(defun %simd-const-p (expr kind)
  "True when EXPR is a number COMPILE-AS-SIMD repeats in every lane of KIND."
  (cond ((typep expr 'double-float) (equal (first kind) "F64.2"))
        ((typep expr 'single-float) (equal (first kind) "F32.4"))
        ((integerp expr) (and (not (third kind)) (fifth kind)
                              (<= (car (fifth kind)) expr (cdr (fifth kind)))))))

(defun %simd-the-inner (expr kind)
  "INNER when EXPR is (THE TYPE INNER), TYPE names KIND and INNER is known to be
   a pack of KIND, so the check of THE always passes."
  (and (consp expr) (eq (car expr) 'the)
       (consp (cdr expr)) (consp (cddr expr)) (null (cdddr expr))
       (eq (%simd-type-kind (second expr)) kind)
       (simd-typed-init-p (third expr) kind)
       (third expr)))

(defun %simd-stack-safe-p (expr kind)
  "True when EXPR, compiled with COMPILE-AS-SIMD for KIND, opens no try block and
   has no effect, so the values before it may stay on the stack."
  (or (simple-expr-p expr)
      (let ((in (%simd-the-inner expr kind)))
        (and in (%simd-stack-safe-p in kind)))
      (let ((node (%simd-node expr)))
        (and node
             (eq (%simd-result-kind node) kind)
             (let ((op (second (second node))))
               (if (eq op :aref)
                   (and (simple-expr-p (third expr))
                        (%simd-aref-backing kind (second expr) (third expr)))
                   (every (lambda (a k) (or (eq k :count) (%simd-stack-safe-p a k)))
                          (cdr expr) (%simd-arg-kinds node))))))))

(defun %simd-raw-arg-p (expr kind)
  "True when COMPILE-AS-SIMD computes EXPR itself rather than casting its value."
  (or (let ((node (%simd-node expr))) (and node (eq (%simd-result-kind node) kind)))
      (%simd-local-of-kind-p expr kind)
      (%simd-the-inner expr kind)
      (%simd-const-p expr kind)))

(defun %simd-local-of-kind-p (expr kind)
  (let ((e (simd-native-local expr)))
    (and e (eq (cdr e) kind))))

(defun %simd-local-type (kind)
  (concatenate 'string "V128." (%simd-lane kind)))

(defun %simd-type-kind (type)
  (and (symbolp type)
       type
       (symbol-package type)
       (string= (package-name (symbol-package type)) "DOTCL-SIMD")
       (%simd-kind-named (symbol-name type))))

(defun extract-simd-locals (body)
  "Alist (NAME . KIND) of the locals the head declarations of BODY declare a
   DOTCL-SIMD pack type, when calls are lowered at all."
  (when (%simd-lowering-p)
    (let ((result '()))
      (dolist (form body)
        (unless (and (consp form) (eq (car form) 'declare))
          (return))
        (dolist (decl (cdr form))
          (when (consp decl)
            (multiple-value-bind (type vars)
                (if (eq (car decl) 'type)
                    (values (cadr decl) (cddr decl))
                    (values (car decl) (cdr decl)))
              (let ((kind (%simd-type-kind type)))
                (when kind
                  (dolist (v vars)
                    (when (and (symbolp v)
                               (not (assoc (var-name v) result :test #'string=)))
                      (push (cons (var-name v) kind) result)))))))))
      result)))

(defun %simd-param-rebind (body required)
  "BODY with the parameters among REQUIRED it declares a pack type rebound,
   after the declarations and the checks of the parameter types, by a LET to
   the cast of the parameter, so that they are held raw like such a local."
  (let ((declared (and (not *emit-frame-locals*) (extract-simd-locals body))))
    (if (null declared)
        body
        (let* ((decls (loop for f in body while (and (consp f) (eq (car f) 'declare)) collect f))
               (rest (nthcdr (length decls) body))
               (checks (loop for f in rest
                             while (and (consp f) (member (car f) '(%check-simple-decl %check-arg-decl)))
                             collect f))
               (rest (nthcdr (length checks) rest))
               (excluded (loop for d in decls
                               append (loop for spec in (cdr d)
                                            when (and (consp spec) (member (car spec) '(special ignore)))
                                              append (mapcar #'var-name (remove-if-not #'symbolp (cdr spec))))))
               (params (loop for p in required
                             for e = (and (symbolp p) (assoc (var-name p) declared :test #'string=))
                             when (and e
                                       (not (member (var-name p) excluded :test #'string=))
                                       (not (global-special-p p)))
                               collect (cons p (find-symbol (first (cdr e)) "DOTCL-SIMD")))))
          (if (or (null params) (notany #'cdr params))
              body
              `(,@decls ,@checks
                (let ,(loop for (p . type) in params collect `(,p (,type ,p)))
                  (declare ,@(loop for (p . type) in params collect `(type ,type ,p)))
                  ,@rest)))))))

(defun simd-typed-init-p (expr kind)
  "True when EXPR is known to be a pack of KIND: a lowered call returning one,
   or a local holding one raw."
  (or (let ((node (%simd-node expr))) (and node (eq (%simd-result-kind node) kind)))
      (%simd-local-of-kind-p expr kind)
      (%simd-the-inner expr kind)))

(defun compile-simd-local-value (expr kind)
  "Code leaving EXPR, stored into a local declared KIND, as a raw Vector128. A
   value that is not a pack of KIND is a type error."
  (if (simd-typed-init-p expr kind)
      (compile-as-simd expr kind (first kind))
      `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
            (compile-for-single-value expr))
        (:ldstr "SETQ")
        ,(%simd-call "Check" kind))))

(defun %simd-args-on-stack-p (args kinds)
  "True when ARGS can be compiled one after another onto the stack: none opens a
   try block or has an effect, and no cast of a value comes before an argument
   that can signal on its own."
  (and (every #'%simd-stack-safe-p args kinds)
       (let ((cast-seen nil))
         (loop for a in args for k in kinds
               do (if (%simd-raw-arg-p a k)
                      (when (and cast-seen (or (%simd-node a) (%simd-the-inner a k)))
                        (return nil))
                      (setf cast-seen t))
               finally (return t)))))

(defun %simd-leaf (expr kind fn)
  `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
        (compile-for-single-value expr))
    (:ldstr ,fn)
    ,(%simd-call "Unbox" kind)))

(defun %simd-access-method (entry store)
  "The SimdLower entry for a read (or a STORE) through ENTRY's buffer, and
   whether it takes the array and the function name for its slow path."
  (cond ((not (compiling-at-safety-0-p)) (values (if store "Store" "Load") t))
        ((eq (%backing-arm-for entry) :fast) (values (if store "StoreRaw" "LoadRaw") nil))
        (t (values (if store "StoreNoCheck" "LoadNoCheck") t))))

(defun %simd-slow-args (slow arr fn)
  (and slow `(,@(compile-var-ref arr) (:ldstr ,fn))))

(defun %simd-index-offset (idx var)
  "K when IDX is VAR, (+ VAR K) or (+ K VAR) with K a non-negative integer."
  (cond ((eq idx var) 0)
        ((and (consp idx) (eq (car idx) '+) (= (length idx) 3))
         (let ((a (second idx)) (b (third idx)))
           (cond ((and (eq a var) (typep b '(integer 0 1024))) b)
                 ((and (eq b var) (typep a '(integer 0 1024))) a))))))

(defun %simd-bounded-ref (kind entry idx)
  "The element reference local of a counting loop that proves all of KIND's
   lanes from IDX on are in ENTRY's buffer, or NIL."
  (let* ((var (if (consp idx) (if (symbolp (second idx)) (second idx) (third idx)) idx))
         (k (and (symbolp var) (%simd-index-offset idx var))))
    (and k (%bounded-backing-ref entry (list var) (+ k (/ 128 (%simd-lane-bits kind)))))))

(defun %simd-load-code (kind entry arr idx fn)
  (multiple-value-bind (method slow) (%simd-access-method entry nil)
    (let ((idx-code (compile-index-to-long idx))
          (ref (%simd-bounded-ref kind entry idx)))
      (cond
        (ref `((:ldloc ,ref) ,@idx-code ,(%simd-call "LoadAt" kind)))
        ((simple-expr-p idx)
         `((:ldloc ,(third entry)) ,@idx-code
           ,@(%simd-slow-args slow arr fn) ,(%simd-call method kind)))
        (t
         (let ((tk (gen-local "SIMDI")))
           `((:declare-local ,tk "Int64") ,@idx-code (:stloc ,tk)
             (:ldloc ,(third entry)) (:ldloc ,tk)
             ,@(%simd-slow-args slow arr fn) ,(%simd-call method kind))))))))

(defun compile-as-simd (expr kind fn)
  "Code leaving EXPR as a raw Vector128 of KIND's lane type. FN names the
   function whose argument EXPR is, for the type error of a wrong value."
  (let ((node (%simd-node expr)))
    (cond
      ((%simd-local-of-kind-p expr kind)
       `((:ldloc ,(car (simd-native-local expr)))))
      ((%simd-the-inner expr kind)
       (compile-as-simd (third expr) kind fn))
      ((and node (eq (%simd-result-kind node) kind))
       (let ((op (second (second node)))
             (class (%simd-op-class node))
             (name (symbol-name (car expr)))
             (args (cdr expr))
             (kinds (%simd-arg-kinds node)))
         (cond
           ((eq op :cast) (compile-as-simd (first args) kind name))
           ((eq op :aref)
            (let ((e (%simd-aref-backing kind (first args) (second args))))
              (if e
                  (%simd-load-code kind e (first args) (second args) name)
                  `(,@(compile-named-call (car expr) args)
                    (:ldstr ,fn) ,(%simd-call "Unbox" kind)))))
           ((eq class :shift)
            `(,@(compile-as-simd (first args) (first kinds) name)
              (:ldc-i4 ,(second args))
              ,(%simd-call op (first node))))
           ((eq class :convert)
            `(,@(compile-as-simd (first args) (first kinds) name)
              (:call ,(concatenate 'string "Simd.Cvt." (%simd-lane (first kinds)) "."
                                   (%simd-lane (first node))))))
           ((%simd-args-on-stack-p args kinds)
            `(,@(loop for a in args for k in kinds append (compile-as-simd a k name))
              ,(%simd-call op (first node))))
           (t
            ;; Every argument is evaluated before any of them is cast, as
            ;; for the ordinary call.
            (let ((tmps (loop for a in args collect (gen-local "SIMDV"))))
              `(,@(loop for a in args for k in kinds for tmp in tmps
                        append (if (%simd-raw-arg-p a k)
                                   `((:declare-local ,tmp ,(concatenate 'string "V128." (%simd-lane k)))
                                     ,@(compile-as-simd a k name)
                                     (:stloc ,tmp))
                                   `((:declare-local ,tmp "LispObject")
                                     ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                                         (compile-for-single-value a))
                                     (:stloc ,tmp))))
                ,@(loop for a in args for k in kinds for tmp in tmps
                        append (if (%simd-raw-arg-p a k)
                                   `((:ldloc ,tmp))
                                   `((:ldloc ,tmp) (:ldstr ,name) ,(%simd-call "Unbox" k))))
                ,(%simd-call op (first node))))))))
      ((and (typep expr 'double-float) (equal (first kind) "F64.2"))
       `((:ldc-r8 ,expr) ,(%simd-call "Broadcast" kind)))
      ((and (typep expr 'single-float) (equal (first kind) "F32.4"))
       `((:ldc-r4 ,expr) ,(%simd-call "Broadcast" kind)))
      ((%simd-const-p expr kind)
       (let* ((bits (%simd-lane-bits kind))
              (v (if (and (member bits '(32 64)) (>= expr (ash 1 (1- bits))))
                     (- expr (ash 1 bits))
                     expr)))
         `((,(if (= bits 64) :ldc-i8 :ldc-i4) ,v)
           ,(%simd-call "Broadcast" kind))))
      (t (%simd-leaf expr kind fn)))))

(defun compile-simd-root (expr)
  "EXPR, a call %SIMD-NODE accepts, at a place that wants a Lisp object. A cast
   keeps its identity semantics, and a read without a hoisted buffer gains
   nothing, so both stay ordinary calls."
  (let* ((node (%simd-node expr))
         (kind (%simd-result-kind node))
         (op (second (second node))))
    (cond
      ((null kind)
       `(,@(compile-as-simd (second expr) (first node) (symbol-name (car expr)))
         ,(%simd-call op (first node))))
      ((or (eq op :cast)
           (and (eq op :aref)
                (not (%simd-aref-backing kind (second expr) (third expr)))))
       (compile-named-call (car expr) (cdr expr)))
      (t
       `(,@(compile-as-simd expr kind (symbol-name (car expr)))
         ,(%simd-call "Box" kind))))))

(defun compile-simd-store-form (expr)
  "(%SIMD-STORE NAME ARRAY INDEX... VALUE), the SETF of a DOTCL-SIMD array
   reader NAME. The subforms are evaluated in that order."
  (let* ((name (second expr))
         (arr (third expr))
         (idxs (butlast (cdddr expr)))
         (val (car (last expr)))
         (info (and (%simd-lowering-p) (%simd-name-info name)))
         (kind (first info))
         (e (and info
                 (eq (second (second info)) :aref)
                 (= (length idxs) 1)
                 (%simd-callable-p (list 'setf name))
                 (%simd-aref-backing kind arr (first idxs)))))
    (if (null e)
        (compile-expr (%simd-store-expansion expr))
        (let* ((tk (gen-local "SIMDI"))
               (vk (gen-local "SIMDV"))
               (fn (symbol-name name))
               (setf-name (concatenate 'string "(SETF " fn ")"))
               (obj (and (not (%simd-raw-arg-p val kind)) (gen-local "SIMDO"))))
          `((:declare-local ,tk "Int64")
            ,@(compile-index-to-long (first idxs))
            (:stloc ,tk)
            (:declare-local ,vk ,(concatenate 'string "V128." (%simd-lane kind)))
            ,@(if obj
                  `((:declare-local ,obj "LispObject")
                    ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                        (compile-for-single-value val))
                    (:stloc ,obj)
                    (:ldloc ,obj) (:ldstr ,setf-name) ,(%simd-call "Unbox" kind))
                  (compile-as-simd val kind setf-name))
            (:stloc ,vk)
            ,@(let ((ref (%simd-bounded-ref kind e (first idxs))))
                (if ref
                    `((:ldloc ,ref) (:ldloc ,tk) (:ldloc ,vk) ,(%simd-call "StoreAt" kind))
                    `((:ldloc ,(third e)) (:ldloc ,tk) (:ldloc ,vk)
                      ,@(multiple-value-bind (method slow) (%simd-access-method e t)
                          `(,@(%simd-slow-args slow arr fn) ,(%simd-call method kind))))))
            ,@(if obj
                  `((:ldloc ,obj))
                  `((:ldloc ,vk) ,(%simd-call "Box" kind))))))))

(defun %simd-store-expansion (form)
  "The ordinary SETF expansion of a %SIMD-STORE form: the (SETF NAME) function
   called on the place's subforms, bound left to right, and the value."
  (let* ((name (second form))
         (place-args (butlast (cddr form)))
         (val (car (last form)))
         (temps (mapcar (lambda (a) (if (%setf-arg-constant-p a) a (gensym "SETF-ARG")))
                        place-args))
         (bindings (loop for tmp in temps for a in place-args
                         unless (eq tmp a) collect (list tmp a)))
         (call `(funcall (function (setf ,name)) ,val ,@temps)))
    (if bindings `(let* ,bindings ,call) call)))
