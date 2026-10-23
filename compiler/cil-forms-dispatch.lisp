;;; cil-forms-dispatch.lisp: aref / typep / declaim / inline-cs / map forms, compile-form handler registrations
;;; Part of the CIL compiler (A2 instruction list architecture)

(in-package :dotcl.cil-compiler)

;;; ============================================================
;;; compile-form handler registrations
;;; Must be after all compile-* helpers are defined (the cil-compiler*.lisp
;;; and cil-forms*.lisp files).  Populates *compile-form-handlers* for O(1) dispatch.
;;; ============================================================

(defun compile-value-arg (arg)
  "Compile ARG as a known-function intrinsic argument: never in tail position (the
   consuming intrinsic call is), never in MV context (a single value is taken).
   Without this, a self-tail-call in an intrinsic argument inherits the tail
   context, fires TCO (a br/leave to the TCO loop), and the intrinsic call that
   follows becomes unreachable dead code; its side effect silently lost
   (e.g. (defun f (n) (if (zerop n) 0 (print (f (1- n))))) never printing). Mirrors
   compile-unary-call / compile-args-array, which already bind both. NOT for
   tail/MV-transparent intrinsics (the/when/unless/min/max/values/...): those use a
   bare (compile-expr ...) as their whole result and must keep the caller's context."
  (let ((*in-tail-position* nil) (*in-mv-context* nil))
    (compile-expr arg)))

(defun %typep-spec-expansion (v spec depth)
  "A form testing V against the constant type SPEC by its parts, or NIL when
   SPEC is best left to TYPEP whole. DEFTYPEs known now are expanded (CLHS
   3.2.2.3 lets compiled code assume they keep their definition) when the
   expansion can itself be taken apart or is a type name."
  (labels ((part (s)
             (or (%typep-spec-expansion v s (1+ depth)) `(typep ,v ',s))))
    (cond
      ((> depth 16) nil)
      ((and (symbolp spec) spec (not (eq spec t))
            (not (eq (symbol-package spec) (find-package "COMMON-LISP")))
            (eq (funcall *known-type-name-p-fn* spec) :deftype)
            (null (find-class spec nil)))
       (multiple-value-bind (e expanded)
           (ignore-errors (funcall *typexpand-1-fn* spec))
         (and expanded (not (eq e spec))
              (or (%typep-spec-expansion v e (1+ depth))
                  (and (symbolp e) `(typep ,v ',e))))))
      ((not (and (consp spec) (listp (cdr spec)))) nil)
      ((and (member (car spec) '(or and)) (cdr spec))
       (cons (car spec) (mapcar #'part (cdr spec))))
      ((and (eq (car spec) 'not) (consp (cdr spec)) (null (cddr spec)))
       `(not ,(part (cadr spec))))
      ((and (eq (car spec) 'eql) (consp (cdr spec)) (null (cddr spec)))
       `(,(if (symbolp (cadr spec)) 'eq 'eql) ,v ',(cadr spec)))
      ((and (eq (car spec) 'member) (cdr spec) (<= (length (cdr spec)) 8))
       `(or ,@(mapcar (lambda (o) `(,(if (symbolp o) 'eq 'eql) ,v ',o)) (cdr spec))))
      (t nil))))

(defun %typep-constant-rewrite (expr)
  "(TYPEP x 'SPEC) rewritten by %TYPEP-SPEC-EXPANSION, or NIL."
  (when (and (not *cross-compiling*) *known-type-name-p-fn* *typexpand-1-fn*
             (consp expr) (eq (car expr) 'typep)
             (consp (cdr expr)) (consp (cddr expr)) (null (cdddr expr))
             (not (local-function-entry 'typep)))
    (let ((type-arg (caddr expr)))
      (when (and (consp type-arg) (eq (car type-arg) 'quote)
                 (consp (cdr type-arg)) (null (cddr type-arg)))
        (let* ((v (gensym "TYPEP"))
               (form (%typep-spec-expansion v (cadr type-arg) 0)))
          (when form
            `(let ((,v ,(cadr expr))) ,form)))))))

(defun compile-struct-typep-value (obj type)
  "COMPILE-STRUCT-TYPEP as a Lisp value: T or NIL, not the branch flag."
  (let ((yes (gen-label "STVYES")) (end-label (gen-label "STVEND")))
    `(,@(compile-struct-typep obj type)
      (:brtrue ,yes)
      ,@(emit-nil)
      (:br ,end-label)
      (:label ,yes)
      ,@(emit-t)
      (:label ,end-label))))

(defun compile-typep-form (expr)
  "The TYPEP form handler: constant quoted type names with a dedicated runtime
   predicate lower to a unary call; everything else goes through Runtime.Typep."
  (let* ((a (cdr expr)) (nargs (length a)))
    (cond
      ((< nargs 2)
       (compile-static-program-error
        (format nil "TYPEP: too few arguments: ~D (expected 2-3)" nargs)))
      ((> nargs 3)
       (compile-static-program-error
        (format nil "TYPEP: too many arguments: ~D (expected 2-3)" nargs)))
      ((%typep-constant-rewrite expr)
       (compile-expr (%typep-constant-rewrite expr)))
      (t
       (let ((type-arg (cadr a)))
         (cond
           ((and (= nargs 2)
                 (consp type-arg) (eq (car type-arg) 'quote)
                 (%struct-type-test-type-p (cadr type-arg)))
            (compile-struct-typep-value (car a) (cadr type-arg)))
           ((and (consp type-arg) (eq (car type-arg) 'quote) (symbolp (cadr type-arg)))
             (let* ((type-name (cadr type-arg))
                    (predicate (cdr (assoc type-name
                                     '((cons . "Runtime.Consp")
                                       (list . "Runtime.Listp")
                                       (null . "Runtime.Not")
                                       (number . "Runtime.Numberp")
                                       (integer . "Runtime.Integerp")
                                       (rational . "Runtime.Rationalp")
                                       (float . "Runtime.Floatp")
                                       (complex . "Runtime.Complexp")
                                       (symbol . "Runtime.Symbolp")
                                       (string . "Runtime.Stringp")
                                       (character . "Runtime.Characterp")
                                       (function . "Runtime.Functionp")
                                       (atom . "Runtime.Atom")
                                       (vector . "Runtime.Vectorp")
                                       (hash-table . "Runtime.Hash_table_p")
                                       (package . "Runtime.Packagep"))))))
               (if predicate
                   (compile-unary-call (list (car a)) predicate "TYPEP")
                   (compile-binary-call (list (car a) type-arg) "Runtime.Typep"))))
           (t (compile-binary-call (list (car a) type-arg) "Runtime.Typep"))))))))

(defun compile-aref-form (expr)
  "The AREF form handler. Numeric/float-backed array locals read the element
   raw (long / r8) and box once here for the value context; statically
   fixnum-typed indices ride the *L runtime variants with no index boxing."
  (let* ((args (cdr expr))
         (idxs (cdr args))
         (num-info (numeric-array-aref-info expr))
         (float-kind (numeric-array-aref-float-kind expr))
         (native (and (<= 2 (length args) 4)
                      (every #'fixnum-long-lowerable-p idxs))))
    (cond
      (num-info
       `(,@(compile-numeric-aref-as-long (first args) idxs (car num-info))
         (:call "Fixnum.Make")))
      ((numeric-array-aref-u64-p expr)
       `(,@(compile-u64-aref-bits (first args) idxs)
         (:call "Runtime.BoxU64")))
      (float-kind
       `(,@(compile-numeric-aref-float
            (first args) idxs (car (numeric-array-aref-entry expr)))
         ,@(ecase float-kind
             (:double '((:newobj "DoubleFloat")))
             (:single '((:conv-r4) (:newobj "SingleFloat"))))))
      (t
       (case (length args)
         (2 (if native
                (compile-aref-native-index-call (first args) idxs "Runtime.ArefL")
                (compile-binary-call args "Runtime.Aref")))
         (3 (if native
                (compile-aref-native-index-call (first args) idxs "Runtime.Aref2DL")
                (compile-ternary-call args "Runtime.Aref2D")))
         (4 (if native
                (compile-aref-native-index-call (first args) idxs "Runtime.Aref3DL")
                (compile-quaternary-call args "Runtime.Aref3D")))
         (t `(,@(compile-args-array args) (:call "Runtime.ArefMulti"))))))))

(defun %aref-set-let-sunk (expr)
  "(%AREF-SET A I... (LET BINDINGS DECL... FORM... LAST)) as
   (LET BINDINGS DECL... FORM... (%AREF-SET A I... LAST)), when that evaluates
   the same: A and each I are literals or plain lexical variables that the LET
   neither binds nor assigns, and it declares nothing special. The store then
   sees LAST, whose type the LET's bindings make known. Else NIL."
  (let* ((args (cdr expr))
         (val (car (last args)))
         (operands (butlast args)))
    (when (and (consp val) (member (car val) '(let let*))
               (consp (cdr val)) (listp (cadr val))
               (consp (cddr val)))
      (let* ((bindings (cadr val))
             (names (mapcar (lambda (b) (if (consp b) (car b) b)) bindings))
             (body (cddr val))
             (decls (loop while (and (consp (car body)) (eq (caar body) 'declare))
                          collect (pop body))))
        (when (and body
                   (every (lambda (o)
                            (or (integerp o)
                                (and (symbolp o) o (not (constantp o))
                                     (simple-expr-p o)
                                     (not (special-var-p o))
                                     (not (member o names :test #'eq)))))
                          operands)
                   (notany (lambda (d)
                             (some (lambda (s) (and (consp s) (eq (car s) 'special)))
                                   (cdr d)))
                           decls)
                   (let ((vars (remove-if-not #'symbolp operands)))
                     (or (null vars)
                         (null (find-mutated-and-captured-vars
                                (list val) (mapcar #'var-name vars))))))
          `(,(car val) ,bindings ,@decls ,@(butlast body)
            (,(car expr) ,@operands ,(car (last body)))))))))

(defun compile-%aref-set-form (expr)
  "The %AREF-SET form handler (the setf expansion of AREF). Mirrors
   COMPILE-AREF-FORM: numeric/float-backed locals store raw with matching
   value types; a mismatched value falls to the general coercing path."
  (let ((sunk (%aref-set-let-sunk expr)))
    (when sunk
      ;; What the LET was proven to store still holds for its last form.
      (let* ((last (car (last (car (last sunk)))))
             (r (and (consp last) (%store-value-range (car (last expr)))))
             (*sunk-store-ranges* (if r
                                      (acons last r *sunk-store-ranges*)
                                      *sunk-store-ranges*)))
        (return-from compile-%aref-set-form (compile-expr sunk)))))
  (let* ((args (cdr expr))
         (idxs (butlast (cdr args)))
         ;; A constant folded here, not only inside the typed predicates, so
         ;; the store below also treats it as the literal operand it is.
         (val (fold-constant-symbol (car (last args))))
         (aref-form `(aref ,(first args) ,@idxs))
         ;; An integer element cannot hold a bignum, so a value whose top-level
         ;; +, -, * is computed with an overflow check is as good as an exact
         ;; one (COMPILE-NUMERIC-AREF-SET lowers it with COMPILE-EXPR-TO-LONG).
         (num-info (and (fixnum-long-lowerable-p val)
                        (numeric-array-aref-info aref-form)))
         (float-kind (numeric-array-aref-float-kind aref-form))
         (native (and (<= 3 (length args) 5)
                      (every #'fixnum-long-lowerable-p idxs))))
    (cond
      (num-info
       (compile-numeric-aref-set (first args) idxs val (car num-info)))
      ((numeric-array-aref-u64-p aref-form)
       (compile-u64-aref-set (first args) idxs val))
      ((and float-kind
            (ecase float-kind
              (:double (double-float-typed-p val))
              (:single (single-float-typed-p val))))
       (compile-numeric-aref-set-float
        (first args) idxs val
        (car (numeric-array-aref-entry aref-form)) float-kind))
      (t
       (case (length args)
         (3 (if native
                (compile-aref-native-index-call (first args) idxs "Runtime.ArefSetL" val)
                (compile-ternary-call args "Runtime.ArefSet")))
         (4 (if native
                (compile-aref-native-index-call (first args) idxs "Runtime.ArefSet2DL" val)
                (compile-quaternary-call args "Runtime.ArefSet2D")))
         (5 (if native
                (compile-aref-native-index-call (first args) idxs "Runtime.ArefSet3DL" val)
                (compile-quinary-call args "Runtime.ArefSet3D")))
         (t `(,@(compile-args-array args) (:call "Runtime.ArefSetMulti"))))))))

(defun compile-declaim-form (expr)
  "The DECLAIM form handler. Each spec has a compile-time half (this file /
   the rest of the compilation must already see it) and a load-time half
   (a PROCLAIM form compiled into the output); ftype feeds the return-type
   table that fixnum-typed-p consults."
  (let ((proclaim-forms nil))
    (dolist (spec (cdr expr))
      (cond
        ((and (consp spec) (eq (car spec) 'special))
         (dolist (sym (cdr spec))
           (note-global-special sym))
         (push `(proclaim ',spec) proclaim-forms))
        ;; (declaration name...). Skipped while cross-compiling so the SBCL
        ;; host's own proclamations stay untouched.
        ((and (consp spec) (eq (car spec) 'declaration))
         (unless *cross-compiling* (proclaim spec))
         (push `(proclaim ',spec) proclaim-forms))
        ;; (notinline name...) / (inline name...): same two halves.
        ((and (consp spec) (member (car spec) '(notinline inline)))
         (unless *cross-compiling* (proclaim spec))
         (push `(proclaim ',spec) proclaim-forms))
        ;; (optimize ...): the qualities decide what we emit for the functions
        ;; compiled after this point -- a debugger frame per call, an interrupt
        ;; poll per loop back-edge. A body declaring the quality itself still
        ;; overrides this (CLHS 3.3.4).
        ((and (consp spec) (eq (car spec) 'optimize))
         (%note-global-proclamation spec)
         (push `(proclaim ',spec) proclaim-forms))
        ((%type-declaration-spec-p spec)
         (%note-global-proclamation spec)
         (unless *cross-compiling*
           (push `(proclaim ',spec) proclaim-forms)))
        ;; (ftype (function (arg-types...) return-type) name...)
        ((and (consp spec) (eq (car spec) 'ftype)
              (consp (cadr spec))
              (eq (car (cadr spec)) 'function)
              (cddr (cadr spec)))
         (let ((ret (car (last (cadr spec)))))
           (dolist (name (cddr spec))
             (when (symbolp name)
               (setf (gethash name *function-return-types*) ret)
               (setf (gethash name *function-arg-types*) (cadr (cadr spec)))
               (remhash name *inferred-return-type-names*))))
         ;; The load-time half, so an image that loads the fasl compiles
         ;; calls against the declared type too (%PROCLAIM-COMPILER-INFO).
         (unless *cross-compiling*
           (push `(proclaim ',spec) proclaim-forms)))))
    (if proclaim-forms
        (compile-progn (append (nreverse proclaim-forms) (list nil)))
        (emit-nil))))

;; The type check of a structure slot store when the value is already known to
;; be of the slot's type. A typed slot's store was preceded by
;; Runtime.CheckSlotType on the boxed value even when the value was an int64
;; local of a fixnum slot or a double of a double-float slot, so the store
;; boxed the value (24 B) only to check it, and the store itself then took the
;; raw value anyway.
(defun %slot-check-inline-type-p (val ty)
  "True when the slot check of VAL against TY can first test TYPEP inline: TY
   is a standard structure or a type whose TYPEP compiles to a direct test, and
   VAL is a variable, which the test and the call may both read."
  (and (not *cross-compiling*)
       (symbolp val) val (not (eq val t)) (not (keywordp val))
       (symbolp ty) ty
       (or (member ty '(integer fixnum symbol cons list character function string))
           (let ((info (gethash ty *struct-info*)))
             (and info (null (getf info :type-option)))))))

(defun %slot-store-value-of-type-p (val ty)
  "True when the value of VAL, a variable or a constant, is statically of TY,
   the declared type of a structure slot, so the check of the store has nothing
   to find. Integer types by range (EXPR-INT-RANGE), DOUBLE-FLOAT and
   SINGLE-FLOAT by the float paths' own predicates."
  (cond ((eq ty t) t)
        ((eq ty 'fixnum) (and (fixnum-typed-p val) t))
        ((integer-type-range ty)
         (let ((want (integer-type-range ty))
               (have (expr-int-range val)))
           (and have (<= (car want) (car have)) (<= (cdr have) (cdr want)))))
        ((member ty '(double-float long-float)) (and (double-float-typed-p val) t))
        ((eq ty 'single-float) (and (single-float-typed-p val) t))
        ((and (symbolp ty) ty)
         (let ((have (%static-value-type val)))
           (and have (%proven-subtype-p have ty))))
        (t nil)))

(defun %static-value-type (expr)
  "A type symbol EXPR's value is known to be of, because a check already made
   sure of it, or NIL: a constant, a variable in CSTATE-PROVEN-TYPE-LOCALS, or a
   read of a structure slot with a declared type (the store of the slot was
   checked)."
  (cond ((or (and (consp expr) (eq (car expr) 'quote) (consp (cdr expr)))
             (integerp expr) (characterp expr) (stringp expr) (keywordp expr)
             (eq expr t) (null expr))
         (let ((v (if (consp expr) (cadr expr) expr)))
           (typecase v
             (null 'null)
             (symbol 'symbol)
             (fixnum 'fixnum)
             (integer 'integer)
             (character 'character)
             (string 'string)
             (cons 'cons)
             (t nil))))
        ((and (symbolp expr) (boundp '*cstate*))
         (let ((key (lookup-local expr)))
           (and key (cdr (assoc key (cstate-proven-type-locals) :test #'equal)))))
        ((and (consp expr) (symbolp (car expr)) (consp (cdr expr)) (null (cddr expr))
              (not *cross-compiling*)
              (not (local-function-entry (car expr))))
         (let ((ty (gethash (car expr) *struct-accessor-types*)))
           (and ty (symbolp ty) (not (eq ty t)) ty)))
        (t nil)))

(defun %proven-subtype-p (have want)
  "True when every value of type HAVE is of type WANT, both symbols: the same
   name, a structure that includes WANT, or standard CL types SUBTYPEP orders."
  (or (eq have want)
      (and (gethash have *struct-info*)
           (loop for s = (getf (gethash have *struct-info*) :parent)
                   then (getf (gethash s *struct-info*) :parent)
                 while s
                 thereis (eq s want)))
      (and (eq (symbol-package have) (find-package "COMMON-LISP"))
           (eq (symbol-package want) (find-package "COMMON-LISP"))
           (member have '(null fixnum integer bit character base-char standard-char
                          symbol cons list string simple-string function
                          double-float single-float))
           (member want '(null fixnum integer bit character base-char standard-char
                          symbol cons list string simple-string function
                          double-float single-float real number atom))
           (values (subtypep have want)))))

(defun %tco-args-checked-p (args)
  "True when a self tail call with ARGS may loop past the entry's parameter
   checks: the label after them was emitted, and each checked parameter gets an
   argument known to be of its checked type."
  (let ((e (cstate-tco-checked)))
    (and e (eq (car e) (cstate-tco-loop-label)) (second e)
         (every (lambda (c)
                  (let ((pos (position (car c) (cstate-tco-param-entries)
                                       :key #'car :test #'equal))
                        (spec (cdr c)))
                    (and pos (< pos (length args)) (symbolp spec) spec
                         (let ((ty (%static-value-type (nth pos args))))
                           (and ty (%proven-subtype-p ty spec))))))
                (cddr e)))))

(defun %proven-arg-type (var spec)
  "SPEC, when the entry check of parameter VAR declared SPEC leaves VAR known
   to be of it (a type symbol); else NIL."
  (and (symbolp spec) spec (not (eq spec t))
       (or (gethash spec *struct-info*)
           (member spec '(fixnum integer character symbol cons list string function
                          double-float single-float)))
       (symbolp var) var
       spec))

;; A structure with raw slots built from values already raw. %MAKE-STRUCT boxed
;; every value into the slot array and the structure's constructor unboxed the
;; raw ones again, keeping the box in the array too.
(defun %make-struct-raw-plan (expr)
  "For (%MAKE-STRUCT 'NAME V...) of a structure NAME with raw slot storage
   (*STRUCT-RAW-LAYOUTS*) where every value going to a raw slot is statically
   of that slot's raw kind: (VERSION . KINDS), KINDS one (KIND . RAW-POSITION)
   per value, KIND :BOXED :LONG :DOUBLE or :SINGLE. Else NIL."
  (let ((q (cadr expr)) (vals (cddr expr)))
    (when (and (consp q) (eq (car q) 'quote) (symbolp (cadr q)) (cadr q)
               (not *cross-compiling*))
      (let ((entry (gethash (cadr q) *struct-raw-layouts*)))
        (when (and entry (<= 1 (length vals) 31) (= (length vals) (length (cdr entry))))
          (let ((kinds (loop for (pos . kind) in (cdr entry)
                             for v in vals
                             collect (cons (cond ((< pos 0) :boxed)
                                                 ((and (eql kind 1) (double-float-typed-p v)) :double)
                                                 ((and (eql kind 2) (single-float-typed-p v)) :single)
                                                 ((and (eql kind 0) (fixnum-typed-p v)) :long)
                                                 (t (return-from %make-struct-raw-plan nil)))
                                           pos))))
            (when (find :boxed kinds :key #'car :test-not #'eq)
              (cons (car entry) kinds))))))))

(defun %compile-make-struct-raw (expr plan)
  "Instructions for EXPR as %MAKE-STRUCT-RAW-PLAN planned it: each value into a
   temporary of its kind, in order, then the boxed ones into a LispObject[]
   (none when every slot is raw), the raw bits into an Int64[] and
   Runtime.MakeStructRaw."
  (let* ((version (car plan)) (kinds (mapcar #'car (cdr plan)))
         (positions (mapcar #'cdr (cdr plan))) (n (length kinds))
         (name-tmp (gen-local "MKSNAME"))
         (temps (loop for k in kinds
                      collect (gen-local (if (eq k :boxed) "MKSV" "MKSR"))))
         (nraw (count :boxed kinds :test-not #'eq))
         (all-raw (= nraw n))
         (mask (loop for k in kinds for i from 0
                     sum (ash (ecase k (:boxed 0) (:long 1) (:double 2) (:single 3)) (* 2 i))))
         (*in-tail-position* nil) (*in-mv-context* nil))
    `((:declare-local ,name-tmp "LispObject")
      ,@(compile-value-arg (cadr expr)) (:stloc ,name-tmp)
      ,@(loop for v in (cddr expr) for k in kinds for tmp in temps
              append `((:declare-local ,tmp ,(if (eq k :boxed) "LispObject" "Int64"))
                       ,@(ecase k
                           (:boxed (compile-value-arg v))
                           (:long (compile-expr-to-long v))
                           (:double `(,@(compile-as-double v) (:call "Runtime.DoubleBits")))
                           (:single `(,@(compile-as-single v) (:call "Runtime.SingleBits"))))
                       (:stloc ,tmp)))
      (:ldloc ,name-tmp)
      ,@(if all-raw
            '((:ldnull))
            `((:ldc-i4 ,n) (:newarr "LispObject")
              ,@(loop for k in kinds for tmp in temps for i from 0
                      when (eq k :boxed)
                        append `((:dup) (:ldc-i4 ,i) (:ldloc ,tmp) (:stelem-ref)))))
      (:ldc-i4 ,nraw) (:newarr "Int64")
      ,@(loop for k in kinds for tmp in temps for pos in positions
              unless (eq k :boxed)
                append `((:dup) (:ldc-i4 ,pos) (:ldloc ,tmp) (:stelem-i8)))
      (:ldc-i4 ,version) (:ldc-i8 ,mask) (:ldc-i4 ,n)
      (:call "Runtime.MakeStructRaw"))))

;;; The IL of a C# method, as DOTNET-CS:DISASSEMBLE-CS returns it, rewritten
;;; into instructions of this compiler for splicing into the function being
;;; compiled (the %INLINE-CS-SPLICED form below).
;;;
;;; Roslyn writes the compact encodings: an argument, local or small constant
;;; in the opcode itself (ldarg.1, stloc.0, ldc.i4.3, ldc.i4.m1), one-byte
;;; operands (ldarg.s, ldc.i4.s) and one-byte branch offsets (br.s, blt.s,
;;; bne.un.s ...). Each is turned into the general instruction here. The method's
;;; arguments become ARG-LOCALS, its own locals (listed by the leading (:LOCALS
;;; ...)) become fresh locals of the enclosing function, its IL_xxxx labels
;;; become fresh labels, and a RET leaves the value on the stack: the last one
;;; falls through, an earlier one branches to the end. Returns the list.
(defun %inline-cs-translate (instrs arg-locals)
  (let ((label-map (make-hash-table :test #'equal))
        (cs-locals #())
        (end-label nil)
        (out '()))
    (labels ((label (k)
               (let ((name (if (symbolp k) (symbol-name k) (princ-to-string k))))
                 (or (gethash name label-map)
                     (setf (gethash name label-map) (gen-label "INLINECS")))))
             (arg (n)
               (or (and (< -1 n (length arg-locals)) (nth n arg-locals))
                   (error "inline-cs: the C# body reads argument ~D of ~D" n (length arg-locals))))
             (loc (n)
               (if (< -1 n (length cs-locals))
                   (aref cs-locals n)
                   (error "inline-cs: the C# body uses local ~D, not declared" n)))
             (suffix (op start)
               ;; The N of LDARG-N / LDLOC-N / STLOC-N / LDC-I4-N.
               (parse-integer (symbol-name op) :start start))
             (emit (ins) (push ins out))
             (local-type (cs-type)
               ;; A slot's type as the IL stack sees it. Unsigned and signed
               ;; slots of one width load and store alike; a narrower slot
               ;; (byte, short, char) truncates on store and has no equal here.
               (cond ((member cs-type '("System.Int64" "System.UInt64") :test #'string=) "Int64")
                     ((member cs-type '("System.Int32" "System.UInt32" "System.Boolean")
                              :test #'string=)
                      "Int32")
                     ((string= cs-type "System.Double") "Double")
                     ((string= cs-type "System.Single") "Single")
                     (t (error "inline-cs: a C# local of type ~A is not supported" cs-type)))))
      (loop for (ins . rest) on instrs
            for op = (car ins)
            do (case op
                 (:locals
                  (setq cs-locals
                        (coerce (loop for ty in (cadr ins)
                                      collect (let ((l (gen-local "INLINECS_LOC")))
                                                (emit `(:declare-local ,l ,(local-type ty)))
                                                l))
                                'vector)))
                 (:ldc-i4-m1 (emit '(:ldc-i4 -1)))
                 ((:ldc-i4-0 :ldc-i4-1 :ldc-i4-2 :ldc-i4-3 :ldc-i4-4
                   :ldc-i4-5 :ldc-i4-6 :ldc-i4-7 :ldc-i4-8)
                  (emit `(:ldc-i4 ,(suffix op 7))))
                 (:ldc-i4-s (emit `(:ldc-i4 ,(cadr ins))))
                 ((:ldarg-0 :ldarg-1 :ldarg-2 :ldarg-3)
                  (emit `(:ldloc ,(arg (suffix op 6)))))
                 ((:ldarg-s :ldarg) (emit `(:ldloc ,(arg (cadr ins)))))
                 ((:starg-s :starg) (emit `(:stloc ,(arg (cadr ins)))))
                 ((:ldloc-0 :ldloc-1 :ldloc-2 :ldloc-3)
                  (emit `(:ldloc ,(loc (suffix op 6)))))
                 ((:ldloc-s :ldloc) (emit `(:ldloc ,(loc (cadr ins)))))
                 ((:stloc-0 :stloc-1 :stloc-2 :stloc-3)
                  (emit `(:stloc ,(loc (suffix op 6)))))
                 ((:stloc-s :stloc) (emit `(:stloc ,(loc (cadr ins)))))
                 ((:br-s :br) (emit `(:br ,(label (cadr ins)))))
                 ((:brfalse-s :brfalse) (emit `(:brfalse ,(label (cadr ins)))))
                 ((:brtrue-s :brtrue) (emit `(:brtrue ,(label (cadr ins)))))
                 ((:beq-s :beq) (emit `(:beq ,(label (cadr ins)))))
                 ((:bge-s :bge) (emit `(:bge ,(label (cadr ins)))))
                 ((:bgt-s :bgt) (emit `(:bgt ,(label (cadr ins)))))
                 ((:ble-s :ble) (emit `(:ble ,(label (cadr ins)))))
                 ((:blt-s :blt) (emit `(:blt ,(label (cadr ins)))))
                 ((:bne-un-s :bne-un) (emit `(:bne ,(label (cadr ins)))))
                 ((:bge-un-s :bge-un) (emit `(:bge-un ,(label (cadr ins)))))
                 ((:bgt-un-s :bgt-un) (emit `(:bgt-un ,(label (cadr ins)))))
                 ((:ble-un-s :ble-un) (emit `(:ble-un ,(label (cadr ins)))))
                 ((:blt-un-s :blt-un) (emit `(:blt-un ,(label (cadr ins)))))
                 (:switch (emit `(:switch ,(mapcar #'label (cadr ins)))))
                 (:label (emit `(:label ,(label (cadr ins)))))
                 (:nop nil)
                 (:ret
                  ;; The value stays on the stack. The last RET falls through.
                  (when rest
                    (unless end-label (setq end-label (gen-label "INLINECS_END")))
                    (emit `(:br ,end-label))))
                 ((:ldarga :ldarga-s :ldloca :ldloca-s)
                  (error "inline-cs: the C# body takes an address (~A), which is not supported"
                         op))
                 (t (emit ins))))
      (when end-label (emit `(:label ,end-label)))
      (nreverse out))))

;;; Mapping functions called with a literal LAMBDA.
;;;
;;; (MAPCAR (LAMBDA (X) ...) L) used to build a closure per call (a LispFunction,
;;; its body delegate and the captured-variable array) only to hand it to the
;;; runtime walker. When the function argument is a lambda expression written in
;;; place, the call is opened into a loop instead, and the lambda becomes an FLET
;;; function that is only ever called directly, which the compiler lowers without
;;; making a closure object. The lambda's body sits in the FLET definition,
;;; lexically outside every binding, block and tag of the loop, so it sees exactly
;;; the environment the lambda saw. The list forms are evaluated in order, after
;;; the (side-effect free) function form, as before.

(defun %map-literal-lambda (form)
  "The lambda expression FORM is, as (LAMBDA ...) or #'(LAMBDA ...), else NIL."
  (let ((lam (cond ((and (consp form) (eq (car form) 'lambda)
                         (not (macrolet-shadowed-p 'lambda)))
                    form)
                   ((and (consp form) (eq (car form) 'function)
                         (consp (cdr form)) (null (cddr form))
                         (consp (cadr form)) (eq (car (cadr form)) 'lambda))
                    (cadr form)))))
    (and lam (consp (cdr lam)) (listp (cadr lam)) lam)))

(defun %map-lambda-accepts-p (lambda-list nargs)
  "True when LAMBDA-LIST is an ordinary lambda list of required, &OPTIONAL, &REST
   and &AUX parameters only, and accepts NARGS arguments."
  (let ((required 0) (optional 0) (rest nil) (state :required))
    (do ((p lambda-list (cdr p)))
        ((atom p) (and (null p)
                       (<= required nargs)
                       (or rest (<= nargs (+ required optional)))))
      (let ((x (car p)))
        (cond ((eq x '&optional)
               (unless (eq state :required) (return nil))
               (setq state :optional))
              ((eq x '&rest)
               (unless (member state '(:required :optional)) (return nil))
               (setq state :rest))
              ((eq x '&aux) (setq state :aux))
              ((member x lambda-list-keywords) (return nil))
              ((eq state :required)
               (unless (and (symbolp x) x (not (keywordp x))) (return nil))
               (incf required))
              ((eq state :optional) (incf optional))
              ((eq state :rest) (setq rest t) (setq state :after-rest))
              ((eq state :after-rest) (return nil)))))))

(defun %map-lambda-expansion (op args)
  "The form (MAPCAR / MAPC / MAPCAN / SOME / EVERY / NOTANY / NOTEVERY OP) with
   arguments ARGS opens into, when the function argument is a literal lambda that
   accepts one argument per list and OP is not declared NOTINLINE. Else NIL.
   The lambda becomes an FLET function and the loop a %MAP-OPEN form in its body
   (COMPILE-%MAP-OPEN), whose call template names the cursor variables."
  (let ((lam (and (consp args) (consp (cdr args))
                  (%map-literal-lambda (car args))))
        (lists (cdr args)))
    (when (and lam
               (%map-lambda-accepts-p (cadr lam) (length lists))
               (not (member op *notinline-functions*))
               (not (%global-notinline-p op))
               ;; SOME and friends also walk vectors; only one sequence is opened.
               (or (member op '(mapcar mapc mapcan)) (null (cdr lists))))
      (let ((fn (gensym "MAPFN"))
            (curs (mapcar (lambda (l) (declare (ignore l)) (gensym "MAPC")) lists)))
        `(flet ((,fn ,@(cdr lam)))
           ,@(when (%map-body-exits-p (cddr lam))
               `((declare (inline ,fn))))
           (%map-open ',op (,fn ,@(mapcar (lambda (c) `(car ,c)) curs)) ,@lists))))))

(defun %map-body-exits-p (body)
  "True when BODY is written with RETURN-FROM, RETURN or GO. Out of the FLET
   function such an exit is a non-local one; inlined into the loop it is a jump."
  (let ((budget 5000))
    (labels ((walk (x)
               (cond ((<= (decf budget) 0) nil)
                     ((atom x) (member x '(return-from return go)))
                     (t (or (walk (car x)) (walk (cdr x)))))))
      (walk body))))

(defun compile-%map-open (expr)
  "Compile (%MAP-OPEN 'OP (FN (CAR C1) ...) LIST1 ...), the loop a mapping call
   with a literal lambda opens into (%MAP-LAMBDA-EXPANSION). C1 ... are the
   cursors over the lists; the call is the only place the lambda's function is
   reached.

   The loop is put together here rather than written as a LET around a DO. Its
   variables are the compiler's own: the lambda's body is in the FLET
   definition, so nothing can capture or even see them. The analyses a LET and a
   TAGBODY run on their bodies (capture and mutation scans, type inference on
   the bindings) would find nothing, and they cost several times what compiling
   the call does. The steps of the loop are still ordinary forms over these
   variables, compiled as such.

   MAPCAR: the results are collected through a tail pointer. With one list the
   list must be proper (checked when the walk is done), as the runtime MAPCAR;
   several stop at the first that runs out. MAPC returns the first list. MAPCAN
   follows NCONC's rules, as the library MAPCAN: a NIL piece adds nothing, a
   non-list piece is allowed only last. SOME / EVERY / NOTANY / NOTEVERY take one
   sequence: a list is walked with ENDP, anything else with LENGTH and AREF.
   Every value is the primary value only."
  (destructuring-bind (qop call &rest lists) (cdr expr)
    (let* ((op (cadr qop))
           (curs (mapcar #'cadr (cdr call)))
           (vars '())
           (decls '())
           (inits '()))
      (flet ((slot (sym type)
               (let ((key (gen-local (var-name sym))))
                 (push (cons sym key) vars)
                 (push `(:declare-local ,key ,type) decls)
                 key)))
        ;; The list forms, in order, in the enclosing scope.
        (loop for c in curs
              for l in lists
              do (let ((key (slot c "LispObject")))
                   (setf inits
                         (append inits
                                 (let ((*in-tail-position* nil) (*in-mv-context* nil))
                                   (compile-expr l))
                                 `((:stloc ,key))))))
        (let* ((r (gensym "MAPR"))
               (rkey (slot r "LispObject"))
               (poll (unless (cstate-no-safepoint)
                       '((:call "ConditionSystem.PollInterrupt"))))
               (top (gen-label "MAPTOP"))
               (end (gen-label "MAPEND"))
               (body
                 (ecase op
                   ((mapcar mapcan)
                    (let* ((tl (gensym "MAPT"))
                           (tkey (slot tl "LispObject"))
                           (x (gensym "MAPX"))
                           (xkey (slot x "LispObject"))
                           (*cstate* (cstate-with *cstate* +cs-locals+
                                                  (append vars (cstate-locals))))
                           (*in-tail-position* nil)
                           (*in-mv-context* nil))
                      (declare (ignore xkey))
                      `(,@(emit-nil) (:stloc ,rkey)
                        ,@(emit-nil) (:stloc ,tkey)
                        (:label ,top)
                        ,@poll
                        ,@(compile-boolean-branch
                           (if (eq op 'mapcar)
                               (if (cdr curs)
                                   `(or ,@(mapcar (lambda (c) `(atom ,c)) curs))
                                   `(atom ,(car curs)))
                               (if (cdr curs)
                                   `(or ,@(mapcar (lambda (c) `(endp ,c)) curs))
                                   `(endp ,(car curs))))
                           end t)
                        ,@(%compile-statements
                           (if (eq op 'mapcar)
                               `((setq ,x (cons ,call nil))
                                 (if ,tl (rplacd ,tl ,x) (setq ,r ,x))
                                 (setq ,tl ,x))
                               `((setq ,x ,call)
                                 (cond ((null ,x))
                                       ((null ,r)
                                        (setq ,r ,x)
                                        (setq ,tl (and (consp ,x) (last ,x))))
                                       ((null ,tl)
                                        (error 'type-error :datum ,r :expected-type 'list))
                                       (t
                                        (rplacd ,tl ,x)
                                        (setq ,tl (and (consp ,x) (last ,x))))))))
                        ,@(%compile-statements
                           `((setq ,@(mapcan (lambda (c) (list c `(cdr ,c))) curs))))
                        (:br ,top)
                        (:label ,end)
                        ,@(when (and (eq op 'mapcar) (null (cdr curs)))
                            (%compile-statements
                             `((when ,(car curs)
                                 (error 'type-error :datum ,(car curs)
                                                    :expected-type 'list))))))))
                   (mapc
                    (let ((*cstate* (cstate-with *cstate* +cs-locals+
                                                 (append vars (cstate-locals))))
                          (*in-tail-position* nil)
                          (*in-mv-context* nil))
                      `((:ldloc ,(cdr (assoc (car curs) vars))) (:stloc ,rkey)
                        (:label ,top)
                        ,@poll
                        ,@(compile-boolean-branch
                           (if (cdr curs)
                               `(or ,@(mapcar (lambda (c) `(endp ,c)) curs))
                               `(endp ,(car curs)))
                           end t)
                        ,@(%compile-statements
                           `(,call
                             (setq ,@(mapcan (lambda (c) (list c `(cdr ,c))) curs))))
                        (:br ,top)
                        (:label ,end))))
                   ((some every notany notevery)
                    (let* ((c (car curs))
                           (ckey (cdr (assoc c vars)))
                           (s (gensym "MAPS"))
                           (skey (slot s "LispObject"))
                           (i (gensym "MAPI"))
                           (ikey (slot i "Int64"))
                           (nkey (gen-local "MAPN"))
                           (vtop (gen-label "MAPVTOP"))
                           (vec (gen-label "MAPVEC"))
                           (hit (gen-label "MAPHIT"))
                           (done (gen-label "MAPDONE"))
                           (*cstate* (cstate-with *cstate*
                                       +cs-locals+ (append vars (cstate-locals))
                                       +cs-long-locals+ (cons ikey (cstate-long-locals))))
                           (*in-tail-position* nil)
                           (*in-mv-context* nil))
                      (flet ((step (elt)
                               ;; The call on one element, and the branch out of
                               ;; the loop when it decides the answer.
                               (let ((call (list (car call) elt)))
                                 (if (eq op 'some)
                                     `(,@(%compile-statements `((setq ,r ,call)))
                                       ,@(compile-boolean-branch r done t))
                                     (compile-boolean-branch
                                      call hit (eq op 'notany))))))
                        (push `(:declare-local ,nkey "Int64") decls)
                        `((:ldloc ,ckey) (:stloc ,skey)
                          ,@(compile-boolean-branch `(listp ,s) vec nil)
                          (:label ,top)
                          ,@poll
                          ,@(compile-boolean-branch `(endp ,c) end t)
                          ,@(step `(car ,c))
                          ,@(%compile-statements `((setq ,c (cdr ,c))))
                          (:br ,top)
                          (:label ,vec)
                          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                              (compile-expr `(length ,s)))
                          (:unbox-fixnum)
                          (:stloc ,nkey)
                          (:ldc-i8 0)
                          (:stloc ,ikey)
                          (:label ,vtop)
                          ,@poll
                          (:ldloc ,ikey) (:ldloc ,nkey) (:clt) (:brfalse ,end)
                          ,@(step `(aref ,s ,i))
                          (:ldloc ,ikey) (:ldc-i8 1) (:add) (:stloc ,ikey)
                          (:br ,vtop)
                          ,@(unless (eq op 'some)
                              `((:label ,hit)
                                ,@(if (eq op 'notevery) (emit-t) (emit-nil))
                                (:stloc ,rkey)
                                (:br ,done)))
                          (:label ,end)
                          ,@(if (member op '(every notany)) (emit-t) (emit-nil))
                          (:stloc ,rkey)
                          (:label ,done))))))))
          `(,@(reverse decls)
            ,@inits
            ,@body
            (:call "MultipleValues.Reset")
            (:ldloc ,rkey)))))))

(let ((h *compile-form-handlers*))

  ;; The shared body of a &key function and its entries (%BUILD-KEY-SHARED-SPECS).
  (setf (gethash '%absent-key h)
        (lambda (expr) (declare (ignore expr)) '((:ldsfld "AbsentKey.Instance"))))
  (setf (gethash '%key-absent-p h)
        (lambda (expr) (compile-expr `(if ,expr t nil))))
  ;; (%key-value IN DEFAULT): IN, or DEFAULT when IN is the absent marker. The
  ;; same as (IF (%KEY-ABSENT-P IN) DEFAULT IN), but known to be single-valued,
  ;; so the binding does not pay an UnwrapMv on every call.
  (setf (gethash '%key-value h)
        (lambda (expr)
          (let ((in (cadr expr)) (default (caddr expr))
                (use-in (gen-label "KEYIN")) (done (gen-label "KEYDONE")))
            (let ((*in-tail-position* nil) (*in-mv-context* nil))
              `(,@(compile-expr in)
                (:call "AbsentKey.Is")
                (:brfalse ,use-in)
                ,@(compile-for-single-value default)
                (:br ,done)
                (:label ,use-in)
                ,@(compile-expr in)
                (:label ,done))))))
  (setf (gethash '%call-key-shared h)
        (lambda (expr)
          (let ((da (compile-direct-call-args (cdr expr))))
            `(,@(cdr da)
              ,@(loop for tmp in (car da) collect `(:ldloc ,tmp))
              ;; A typed entry hands its caller's value mode on to the shared
              ;; body, whose first instruction takes it; nothing runs between.
              ,@(when (and *key-shared-takes-mode* *in-tail-position* (%mv-mode-local))
                  `(,@(%mv-mode-push) (:call "MultipleValues.PassMode")))
              (:call-key-shared)))))

  ;; Arithmetic
  (setf (gethash '+ h) (lambda (expr) (compile-add (cdr expr))))
  (setf (gethash '- h) (lambda (expr) (compile-sub (cdr expr))))
  (setf (gethash '* h) (lambda (expr) (compile-mul (cdr expr))))
  (setf (gethash '/ h) (lambda (expr) (compile-div (cdr expr))))

  ;; Comparison (N-arg: (< a b c) means (and (< a b) (< b c)))
  (setf (gethash '> h) (lambda (expr) (compile-nary-comparison (cdr expr) '> "Runtime.GreaterThan")))
  (setf (gethash '< h) (lambda (expr) (compile-nary-comparison (cdr expr) '< "Runtime.LessThan")))
  (setf (gethash '>= h) (lambda (expr) (compile-nary-comparison (cdr expr) '>= "Runtime.GreaterEqual")))
  ;; (%fixnum-ge-object counter limit) -- the DOTIMES loop test. The counter side
  ;; rides the raw int64 path (it is a declared-fixnum slot), the limit side stays
  ;; boxed, so a bignum count still compares correctly and no Fixnum is made per
  ;; iteration.
  (setf (gethash '%fixnum-ge-object h)
        (lambda (expr)
          `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                (compile-as-long (cadr expr)))
            ,@(compile-value-arg (caddr expr))
            (:call "Runtime.FixnumGeObject"))))
  (setf (gethash '<= h) (lambda (expr) (compile-nary-comparison (cdr expr) '<= "Runtime.LessEqual")))
  (setf (gethash '= h) (lambda (expr) (compile-nary-comparison (cdr expr) '= "Runtime.NumEqual")))
  ;; /= is the one comparison that is not transitive: (/= a b c) asks that all
  ;; three be pairwise distinct, so it cannot be chained the way < and = are, and
  ;; the N-ary entry takes the arguments as an array. Two arguments have nothing
  ;; pairwise about them -- (/= a b) is exactly (not (= a b)) -- so the binary
  ;; entry that has been sitting in the runtime unused takes them directly. The
  ;; array path cost 80 bytes on every call of the commonest arity: one
  ;; LispObject[] for the arguments and one Number[] inside the callee.
  (setf (gethash '/= h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (if (= (length args) 2)
                (compile-binary-call args "Runtime.NumNotEqual" "/=")
                `(,@(compile-args-array args) (:call "Runtime.NumNotEqualN"))))))

  ;; Equality
  (setf (gethash 'eq h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Eq")))
  (setf (gethash 'eql h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Eql")))
  (setf (gethash 'equal h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Equal")))
  (setf (gethash 'equalp h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Equalp" "EQUALP")))

  ;; List ops
  (setf (gethash 'cons h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.MakeCons" "CONS")))
  (setf (gethash 'car h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Car" "CAR")))
  (setf (gethash 'cdr h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Cdr" "CDR")))
  (setf (gethash 'list h) (lambda (expr) (compile-list-call (cdr expr))))
  (setf (gethash 'list* h) (lambda (expr) (compile-list-star-call (cdr expr))))
  (setf (gethash 'append h) (lambda (expr) (compile-append (cdr expr))))
  (setf (gethash 'length h)
        (lambda (expr)
          (let ((e (and (= (length expr) 2)
                        (array-backing-entry (list 'aref (second expr) 0)))))
            (if (%raw-array-entry-p e)
                `((:ldloc ,(third e)) (:ldlen) (:conv-i8) (:call "Fixnum.Make"))
                (compile-unary-call (cdr expr) "Runtime.Length" "LENGTH")))))
  (setf (gethash 'rplaca h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Rplaca" "RPLACA")))
  (setf (gethash 'rplacd h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Rplacd" "RPLACD")))
  (setf (gethash 'nreverse h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Nreverse" "NREVERSE")))
  (setf (gethash 'nth h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Nth" "NTH")))
  (setf (gethash 'nthcdr h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Nthcdr" "NTHCDR")))
  (setf (gethash 'last h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              (compile-unary-call (cdr expr) "Runtime.Last" "LAST")
              (compile-named-call 'last (cdr expr)))))
  (setf (gethash 'nconc h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (case (length args)
              (0 (emit-nil))
              (1 (let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args))))
              ;; 2-arg: direct Runtime.Nconc2 (semantics match the stdlib &rest
              ;; defun exactly): the dominant call shape, e.g. the liveness
              ;; analysis' (nconc writes reads). Unconditional inline, same
              ;; precedent as the rplacd/append handlers here (builtin handlers
              ;; don't consult notinline; only compiler macros do, CLHS 3.2.2.1.1).
              ;; NOTE: Runtime.Nconc2 is a different, COPYING helper (CLOS
              ;; method combination): NconcDestructive2 is the CL-semantics one.
              (2 (compile-binary-call args "Runtime.NconcDestructive2" "NCONC"))
              (t (compile-named-call 'nconc args))))))
  (setf (gethash 'copy-list h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.CopyList" "COPY-LIST")))
  (setf (gethash 'member h)
        (lambda (expr)
          (let ((n-args (length (cdr expr))) (args (cdr expr)))
            (cond
              ((< n-args 2)
               (compile-static-program-error
                (format nil "MEMBER: wrong number of arguments: ~a (expected at least 2)" n-args)))
              ((and (= n-args 4) (eq (third args) :test)
                    (let ((tf (fourth args)))
                      (and (consp tf) (eq (car tf) 'function) (eq (cadr tf) 'eq))))
               (compile-binary-call (list (first args) (second args)) "Runtime.MemberEq"))
              ((cddr args) (compile-named-call 'member args))
              (t (compile-binary-call args "Runtime.Member" "MEMBER"))))))
  (setf (gethash 'assoc h)
        (lambda (expr)
          (let ((n-args (length (cdr expr))) (args (cdr expr)))
            (cond
              ((< n-args 2)
               (compile-static-program-error
                (format nil "ASSOC: wrong number of arguments: ~a (expected at least 2)" n-args)))
              ((and (= n-args 4) (eq (third args) :test)
                    (let ((tf (fourth args)))
                      (and (consp tf) (eq (car tf) 'function) (eq (cadr tf) 'eq))))
               (compile-binary-call (list (first args) (second args)) "Runtime.AssocEq"))
              ((cddr args) (compile-named-call 'assoc args))
              (t (compile-binary-call args "Runtime.Assoc" "ASSOC"))))))
  (setf (gethash 'cadr h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Cadr" "CADR")))
  (setf (gethash 'cddr h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Cddr" "CDDR")))
  (setf (gethash 'caar h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Caar" "CAAR")))
  (setf (gethash 'cdar h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Cdar" "CDAR")))
  (setf (gethash 'caddr h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Caddr" "CADDR")))

  ;; List accessor aliases
  (setf (gethash 'first h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Car" "FIRST")))
  (setf (gethash 'rest h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Cdr" "REST")))
  (setf (gethash 'second h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Cadr" "SECOND")))
  (setf (gethash 'third h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Caddr" "THIRD")))

  ;; Type predicates
  (setf (gethash 'not h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Not" "NOT")))
  (setf (gethash 'null h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Not" "NULL")))
  (setf (gethash 'identity h)
        (lambda (expr)
          (if (and (consp (cdr expr)) (null (cddr expr))
                   (not (member 'identity *notinline-functions*))
                   (not (%global-notinline-p 'identity)))
              (let ((*in-tail-position* nil) (*in-mv-context* nil))
                (compile-for-single-value (cadr expr)))
              (compile-named-call 'identity (cdr expr)))))
  (setf (gethash 'atom h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Atom" "ATOM")))
  (setf (gethash 'consp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Consp" "CONSP")))
  (setf (gethash 'endp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Endp" "ENDP")))
  (setf (gethash 'listp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Listp" "LISTP")))
  (setf (gethash 'numberp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Numberp")))
  (setf (gethash 'integerp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Integerp")))
  (setf (gethash 'symbolp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Symbolp")))
  (setf (gethash 'stringp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Stringp")))
  (setf (gethash 'characterp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Characterp")))
  (setf (gethash 'functionp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Functionp")))
  (setf (gethash 'rationalp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Rationalp")))
  (setf (gethash 'floatp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Floatp")))
  (setf (gethash 'complexp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Complexp")))
  (setf (gethash 'type-of h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.TypeOf")))
  (setf (gethash 'typep h) #'compile-typep-form)
  (setf (gethash 'vectorp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Vectorp")))
  (setf (gethash 'keywordp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Keywordp")))
  (setf (gethash 'hash-table-p h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Hash_table_p")))
  (setf (gethash 'packagep h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Packagep")))
  (setf (gethash 'subtypep h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (if (= (length args) 3)
                (compile-binary-call (list (first args) (second args)) "Runtime.SubtypepMv")
                (compile-binary-call args "Runtime.SubtypepMv")))))

  ;; Array ops. When every index is statically fixnum-typed (e.g. an
  ;; Int64-slot loop counter), lower the indices as raw longs and call the
  ;; *L runtime variants: no per-access Fixnum boxing on either side.
  (setf (gethash 'svref h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (if (and (= (length args) 2) (fixnum-long-lowerable-p (second args)))
                (compile-aref-native-index-call
                 (first args) (list (second args)) "Runtime.ArefL")
                (compile-binary-call args "Runtime.Aref")))))
  (setf (gethash 'aref h) #'compile-aref-form)
  (setf (gethash '%aref-set h) #'compile-%aref-set-form)
  ;; ROW-MAJOR-AREF on a rank-1 array the compiler already knows is
  ;; numeric-backed (a declared local, or a structure accessor whose slot says
  ;; so) is that array's AREF, so it takes AREF's element path. Anything else
  ;; keeps the full call, which handles every rank.
  (setf (gethash 'row-major-aref h)
        (lambda (expr)
          (if (numeric-array-aref-entry expr)
              (compile-aref-form (cons 'aref (cdr expr)))
              (compile-named-call 'row-major-aref (cdr expr)))))
  (setf (gethash '%set-row-major-aref h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (if (and (= (length args) 3)
                     (numeric-array-aref-entry
                      (list 'row-major-aref (first args) (second args))))
                (compile-%aref-set-form (cons '%aref-set args))
                (compile-named-call '%set-row-major-aref args)))))
  (setf (gethash '%char-set h) (lambda (expr) (compile-ternary-call (cdr expr) "Runtime.CharSet")))
  (setf (gethash 'vector-push-extend h)
        (lambda (expr)
          (if (= (length (cdr expr)) 2)
              (compile-binary-call (cdr expr) "Runtime.VectorPushExtend2")
              (compile-named-call 'vector-push-extend (cdr expr)))))
  (setf (gethash 'vector-push h)
        (lambda (expr)
          (if (= (length (cdr expr)) 2)
              (compile-binary-call (cdr expr) "Runtime.VectorPush2")
              (compile-named-call 'vector-push (cdr expr)))))
  (setf (gethash '%set-symbol-value h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.SetSymbolValue")))

  ;; error
  (setf (gethash 'error h)
        (lambda (expr)
          `(,@(compile-args-array (cdr expr))
            (:call "Runtime.LispErrorFormat"))))

  ;; locally: preserves top-level-ness (uses *compile-was-toplevel*)
  (setf (gethash 'locally h)
        (lambda (expr)
          (let ((*at-toplevel* *compile-was-toplevel*)
                (*lexical-decl-frames* (%push-decl-frame :locally nil (cdr expr)))
                ;; (locally (declare (notinline f)) ...) extends the notinline set
                ;; for the body (CLHS 3.2.2.1.1).
                (*notinline-functions* (append (extract-notinline (cdr expr)) *notinline-functions*)))
            (multiple-value-bind (declared-specials real-body) (extract-specials (cdr expr))
              (if (null declared-specials)
                  (compile-progn real-body)
                  (let* ((*specials* (append declared-specials *specials*))
                         (*cstate* (cstate-with *cstate* +cs-locals+
                                     (remove-locals-shadowed-by declared-specials
                                                                (cstate-locals)))))
                    (compile-progn real-body)))))))

  ;; handler-case
  (setf (gethash 'handler-case h)
        (lambda (expr)
          (let* ((body-form (cadr expr))
                 (clauses (cddr expr))
                 (no-error-clause (find :no-error clauses :key #'car))
                 (error-clauses (remove :no-error clauses :key #'car)))
            (if no-error-clause
                (let* ((ne-lambda-list (cadr no-error-clause))
                       (ne-body (cddr no-error-clause))
                       (block-tag (gensym "HC-NO-ERROR"))
                       (wrapped-clauses
                         (mapcar (lambda (clause)
                                   (let ((type (car clause))
                                         (var-list (cadr clause))
                                         (clause-body (cddr clause)))
                                     `(,type ,var-list
                                       (return-from ,block-tag (progn ,@clause-body)))))
                                 error-clauses)))
                  (compile-expr
                   `(block ,block-tag
                      (multiple-value-call (lambda ,ne-lambda-list ,@ne-body)
                        (handler-case ,body-form ,@wrapped-clauses)))))
                (compile-handler-case body-form clauses)))))
  (setf (gethash 'handler-bind h) (lambda (expr) (compile-handler-bind (cadr expr) (cddr expr))))

  ;; restart-case / restart-bind / invoke-restart / find-restart / compute-restarts
  (setf (gethash 'restart-case h) (lambda (expr) (compile-restart-case (cadr expr) (cddr expr))))
  (setf (gethash 'restart-bind h) (lambda (expr) (compile-restart-bind (cadr expr) (cddr expr))))
  ;; Temps for the same reason as %MAKE-STRUCT below: an argument containing a
  ;; branch would otherwise run with the restart designator pending on the stack.
  (setf (gethash 'invoke-restart h)
        (lambda (expr)
          (let ((r-tmp (gen-local "IRRESTART")) (args-tmp (gen-local "IRARGS")))
            `((:declare-local ,r-tmp "LispObject")
              (:declare-local ,args-tmp "LispObject[]")
              ,@(compile-value-arg (cadr expr)) (:stloc ,r-tmp)
              ,@(compile-args-array (cddr expr)) (:stloc ,args-tmp)
              (:ldloc ,r-tmp) (:ldloc ,args-tmp)
              (:call "Runtime.InvokeRestart")))))
  (setf (gethash 'find-restart h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              (compile-unary-call (cdr expr) "Runtime.FindRestart")
              `(,@(compile-args-array (cdr expr)) (:call "Runtime.FindRestartN")))))
  (setf (gethash 'compute-restarts h)
        (lambda (expr)
          (if (null (cdr expr))
              '((:call "Runtime.ComputeRestarts"))
              `(,@(compile-args-array (cdr expr)) (:call "Runtime.ComputeRestartsN")))))

  ;; signal / warn
  (setf (gethash 'signal h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              (compile-unary-call (cdr expr) "Runtime.LispSignal")
              `(,@(compile-args-array (cdr expr)) (:call "Runtime.LispSignalFormat")))))
  (setf (gethash 'warn h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              (compile-unary-call (cdr expr) "Runtime.LispWarn")
              `(,@(compile-args-array (cdr expr)) (:call "Runtime.LispWarnFormat")))))

  ;; Struct primitives
  ;; Both operands go through temps, like FORMAT and OPEN above and for the same
  ;; reason: the slot values are compiled between the type name being pushed and
  ;; the call, and a slot value is arbitrary user code. One containing a branch
  ;; (a LOOP, a COND, an inlined call) makes the pushed name pending across a
  ;; join label, which is IL the verifier rejects -- coalton hit it with
  ;; (make-node-body :nodes (loop ...) :last-node ...), and the failure surfaced
  ;; only when the method was first CALLED, as "invalid program".
  (setf (gethash '%make-struct h)
        (lambda (expr)
          (let ((name-tmp (gen-local "MKSNAME")) (slots-tmp (gen-local "MKSSLOTS"))
                (raw-plan (%make-struct-raw-plan expr)))
           (if raw-plan
            (%compile-make-struct-raw expr raw-plan)
            `((:declare-local ,name-tmp "LispObject")
              (:declare-local ,slots-tmp "LispObject[]")
              ,@(compile-value-arg (cadr expr)) (:stloc ,name-tmp)
              ,@(compile-args-array (cddr expr)) (:stloc ,slots-tmp)
              (:ldloc ,name-tmp) (:ldloc ,slots-tmp)
              (:call "Runtime.MakeStruct"))))))
  ;; (%check-slot-type VALUE TYPE STRUCT SLOT) -- the declared type of a
  ;; structure slot, checked where the value is stored. Compiles to just the
  ;; value under (safety 0): a violated declaration is undefined (CLHS 3.3.1),
  ;; and safety 0 is where the writer asked not to be checked. The interpreter
  ;; has no per-function policy and always checks, which is the same licence
  ;; read the other way. Elided too when the value is statically of the slot's
  ;; type (%SLOT-STORE-VALUE-OF-TYPE-P): the check could not fail.
  (setf (gethash (quote %check-slot-type) h)
        (lambda (expr)
          (let ((val (first (cdr expr))))
            (if (or (compiling-at-safety-0-p)
                    (let ((ty (second (cdr expr))))
                      (and (consp ty) (eq (car ty) 'quote)
                           (%slot-store-value-of-type-p val (cadr ty)))))
                ;; Nil, not the value: this form is only ever emitted as a
                ;; STATEMENT (see %SLOT-CHECK-STMT), so a pop follows it, and
                ;; (ldsfld Nil.Instance) (pop) is peephole P4 -- the pair
                ;; disappears and the elided check costs nothing at all.
                ;; Compiling the value here instead left a load and a pop
                ;; behind, two instructions per typed store, which is what
                ;; make il-parity caught.
                (emit-nil)
                (let ((call `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                                  `(,@(compile-value-arg val)
                                    ,@(compile-value-arg (second (cdr expr)))
                                    ,@(compile-value-arg (third (cdr expr)))
                                    ,@(compile-value-arg (fourth (cdr expr)))))
                              (:call "Runtime.CheckSlotType")))
                      (ty (let ((q (second (cdr expr))))
                            (and (consp q) (eq (car q) 'quote) (cadr q)))))
                  (if (%slot-check-inline-type-p val ty)
                      ;; TYPEP says yes for every value of the type, and
                      ;; CheckSlotType then could not fail; a no goes to it.
                      (let ((ok (gen-label "SCOK")))
                        `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                              (compile-boolean-branch `(typep ,val ',ty) ok t))
                          ,@call (:pop)
                          (:label ,ok)
                          ,@(emit-nil)))
                      call))))))
  ;; (%check-simple-decl VAR 'SPEC) -- inserted by INSERT-SIMPLE-DECL-CHECKS
  ;; after the declarations of a binding form: VAR was declared SPEC, a SIMPLE-*
  ;; array type, so a non-simple array in it is a TYPE-ERROR. Only ever a
  ;; statement, so the elided cases compile to NIL, whose load and pop peephole
  ;; P4 removes. Elided under (safety 0), where the declaration is trusted, and
  ;; when VAR's storage is hoisted, whose fetch makes the same test itself.
  (setf (gethash '%check-simple-decl h)
        (lambda (expr)
          (let ((var (second expr))
                (spec (second (third expr))))
            (if (or (compiling-at-safety-0-p)
                    (%hoisted-backing-entry-p var))
                (emit-nil)
                (let ((ok (gen-label "SDOK"))
                      (*in-tail-position* nil)
                      (*in-mv-context* nil))
                  `(,@(compile-expr var)
                    (:call "Runtime.IsNonSimpleArray")
                    (:brfalse ,ok)
                    ,@(compile-expr var)
                    ,@(compile-quoted-cached spec)
                    (:ldstr ,(symbol-name var))
                    (:call "Runtime.SignalDeclaredNotSimple")
                    (:label ,ok)
                    ,@(emit-nil)))))))
;; (%tco-checks-done): follows the entry's parameter checks. A label here, where
;; a self tail call whose arguments are known to be of the checked types loops
;; to, so it does not check them again.
  (setf (gethash '%tco-checks-done h)
        (lambda (expr)
          (declare (ignore expr))
          (let ((tl (cstate-tco-loop-label)) (e (cstate-tco-checked)))
            (if (and tl (not (cstate-tco-in-try-catch)))
                (let ((label (gen-label "TCOCHK")))
                  (setf *cstate*
                        (cstate-with *cstate* +cs-tco-checked+
                                     (list* tl label (and e (eq (car e) tl) (cddr e)))))
                  `((:label ,label) ,@(emit-nil)))
                (emit-nil)))))
;; (%check-arg-decl VAR 'SPEC [PROVEN]): VAR was declared SPEC on entry. PROVEN:
;; the body never assigns VAR, so after the check VAR is known to be of SPEC.
  (setf (gethash '%check-arg-decl h)
        (lambda (expr)
          (let ((var (second expr))
                (spec (second (third expr))))
            (if (compiling-at-safety-0-p)
                (emit-nil)
                (let ((*in-tail-position* nil) (*in-mv-context* nil))
                  ;; The rest of the body is compiled in the extent of the
                  ;; binding of *CSTATE* this check is compiled in.
                  (let ((key (lookup-local var)))
                    (when (and key (fourth expr))
                      (setf *cstate*
                            (cstate-with *cstate* +cs-proven-type-locals+
                                         (acons key spec (cstate-proven-type-locals)))))
                    (let ((tl (cstate-tco-loop-label)) (e (cstate-tco-checked)))
                      (when tl
                        (setf *cstate*
                              (cstate-with *cstate* +cs-tco-checked+
                                           (if (and e (eq (car e) tl))
                                               (list* tl nil (acons key spec (cddr e)))
                                               (list* tl nil (acons key spec nil))))))))
                  `(,@(%arg-check-instrs var spec) ,@(emit-nil)))))))
  (setf (gethash '%struct-ref h)
        (lambda (expr)
          (let ((obj (first (cdr expr))) (idx (second (cdr expr))))
            (if (integerp idx)
                (%compile-struct-read obj idx (%struct-check-name (fourth expr)))
                (compile-binary-call (list obj idx) "Runtime.StructRef")))))
  (setf (gethash '%struct-set h)
        (lambda (expr)
          (let ((obj (first (cdr expr))) (idx (second (cdr expr))) (val (third (cdr expr)))
                (type (%struct-check-name (fifth expr))))
            (if (integerp idx)
                (if (and (double-float-typed-p val) (not (fixnum-typed-p val)))
                    ;; A double-typed value into a slot whose storage is a raw
                    ;; double: no box either way. StructSetD falls back when the
                    ;; slot turns out not to be raw, so this is safe to emit
                    ;; whenever the VALUE is a double.
                    (let ((hit (struct-backing-entry obj idx type)))
                      (if (and hit (eql (cddr hit) 1))
                          `(,@(%struct-backing-write
                               hit obj idx
                               (let ((*in-tail-position* nil) (*in-mv-context* nil))
                                 (compile-as-double val))
                               type)
                            (:newobj "DoubleFloat"))
                          `(,@(compile-struct-object obj type)
                            (:ldc-i4 ,idx)
                            ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                                (compile-as-double val))
                            (:call "Runtime.StructSetD")
                            (:newobj "DoubleFloat"))))
                (if (and (single-float-typed-p val) (not (fixnum-typed-p val)))
                    ;; A single-typed value: StructSetF stores its bits into a
                    ;; raw single-float slot and falls back to the boxed store
                    ;; otherwise, like StructSetD.
                    (let ((hit (struct-backing-entry obj idx type)))
                      (if (and hit (eql (cddr hit) 2))
                          `(,@(%struct-backing-write
                               hit obj idx
                               (let ((*in-tail-position* nil) (*in-mv-context* nil))
                                 (compile-as-single val))
                               type)
                            (:newobj "SingleFloat"))
                          `(,@(compile-struct-object obj type)
                            (:ldc-i4 ,idx)
                            ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                                (compile-as-single val))
                            (:call "Runtime.StructSetF")
                            (:newobj "SingleFloat"))))
                (if (u64-native-local-p val)
                    `(,@(compile-struct-object obj type)
                      (:ldc-i4 ,idx)
                      (:ldloc ,(u64-native-local-p val))
                      (:call "Runtime.StructSetU64")
                      (:call "Runtime.BoxU64"))
                (if (fixnum-typed-p val)
                    ;; Fixnum value: store the raw long. The box the ordinary path
                    ;; makes for the call was the entire allocation of a
                    ;; struct-writing loop. Fixnum.Make here re-boxes for the SETF
                    ;; value, and in statement position the peephole removes it.
                    (let ((hit (struct-backing-entry obj idx type)))
                      (if (and hit (eql (cddr hit) 0))
                          `(,@(%struct-backing-write
                               hit obj idx
                               (let ((*in-tail-position* nil) (*in-mv-context* nil))
                                 (compile-as-long val))
                               type)
                            (:call "Fixnum.Make"))
                          `(,@(compile-struct-object obj type)
                            (:ldc-i4 ,idx)
                            ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                                (compile-as-long val))
                            (:call "Runtime.StructSetL")
                            (:call "Fixnum.Make"))))
                `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                      (compile-expr obj))
                  (:ldc-i4 ,idx)
                  ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                      (compile-expr val))
                  ,@(if type
                        `(,@(compile-quoted type) (:call "Runtime.StructSetIT"))
                        '((:call "Runtime.StructSetI"))))))))
                `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                      `(,@(compile-value-arg obj)
                        ,@(compile-value-arg idx)
                        ,@(compile-value-arg val)))
                  (:call "Runtime.StructSet"))))))
  (setf (gethash '%struct-typep h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.StructTypep")))
  (setf (gethash '%copy-struct h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.CopyStruct")))

  ;; CLOS primitives
  (setf (gethash '%make-class h)
        (lambda (expr)
          `(,@(compile-value-arg (second expr))
            ,@(compile-value-arg (third expr))
            ,@(compile-value-arg (fourth expr))
            (:call "Runtime.MakeClass"))))
  (setf (gethash '%make-class-full h)
        (lambda (expr)
          `(,@(compile-value-arg (second expr))
            ,@(compile-value-arg (third expr))
            ,@(compile-value-arg (fourth expr))
            ,@(compile-value-arg (fifth expr))
            (:call "Runtime.MakeClassFull"))))
  (setf (gethash '%make-class-full-options h)
        (lambda (expr)
          `(,@(compile-value-arg (second expr))
            ,@(compile-value-arg (third expr))
            ,@(compile-value-arg (fourth expr))
            ,@(compile-value-arg (fifth expr))
            ,@(compile-value-arg (sixth expr))
            (:call "Runtime.MakeClassFullOptions"))))
  (setf (gethash '%make-slot-def h)
        (lambda (expr)
          `(,@(compile-value-arg (second expr))
            ,@(compile-value-arg (third expr))
            ,@(compile-value-arg (fourth expr))
            (:call "Runtime.MakeSlotDef"))))
  (setf (gethash '%make-slot-def-with-allocation h)
        (lambda (expr)
          `(,@(compile-value-arg (second expr))
            ,@(compile-value-arg (third expr))
            ,@(compile-value-arg (fourth expr))
            ,@(compile-value-arg (fifth expr))
            (:call "Runtime.MakeSlotDefWithAllocation"))))
  ;; (%slot-def-raw-options slotd options-plist) -> slotd
  ;; Attaches the canonical slot-option plist for DIRECT-SLOT-DEFINITION-CLASS dispatch
  ;; under a custom metaclass; returns the slotd so it wraps %make-slot-def transparently.
  (setf (gethash '%slot-def-raw-options h)
        (lambda (expr)
          `(,@(compile-value-arg (second expr))
            ,@(compile-value-arg (third expr))
            (:call "Runtime.SetSlotDefRawOptions"))))
  ;; (%slot-def-attrs slotd readers writers type initform) -> slotd
  ;; Carries the introspectable slot attributes DEFCLASS parses (reader/writer
  ;; names, declared type, the initform as source, the documentation) onto the
  ;; slot definition.
  (setf (gethash '%slot-def-attrs h)
        (lambda (expr)
          `(,@(compile-value-arg (second expr))
            ,@(compile-value-arg (third expr))
            ,@(compile-value-arg (fourth expr))
            ,@(compile-value-arg (fifth expr))
            ,@(compile-value-arg (sixth expr))
            (:call "Runtime.SetSlotDefAttrs"))))
  ;; (%slot-def-doc slotd documentation) -> slotd
  ;; Its own call rather than a sixth argument above: a compiled FASL names the
  ;; runtime method it calls, so widening that signature stops every FASL built
  ;; before the change from loading.
  (setf (gethash '%slot-def-doc h)
        (lambda (expr)
          `(,@(compile-value-arg (second expr))
            ,@(compile-value-arg (third expr))
            (:call "Runtime.SetSlotDefDocumentation"))))
  (setf (gethash '%register-class h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.RegisterClass")))
  (setf (gethash '%set-class-default-initargs h)
        (lambda (expr)
          `(,@(compile-value-arg (second expr))
            ,@(compile-value-arg (third expr))
            (:call "Runtime.SetClassDefaultInitargs"))))
  (setf (gethash 'find-class h)
        (lambda (expr)
          (if (null (cddr expr))
              (compile-unary-call (cdr expr) "Runtime.FindClass")
              (compile-named-call 'find-class (cdr expr)))))
  (setf (gethash '%find-class-or-nil h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.FindClassOrNil")))
  (setf (gethash 'class-of h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.ClassOf")))
  (setf (gethash 'boundp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Boundp")))
  (setf (gethash 'symbol-value h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.SymbolValue")))
  (setf (gethash 'fdefinition h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Fdefinition")))
  (setf (gethash '%getenv h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Getenv")))
  (setf (gethash 'slot-value h)
        (lambda (expr)
          (let ((name (caddr expr)))
            (if (and (not *cross-compiling*)
                     (= (length expr) 3)
                     (consp name) (eq (car name) 'quote) (consp (cdr name)) (null (cddr name))
                     (symbolp (cadr name)) (symbol-package (cadr name)))
                `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                      (compile-expr (cadr expr)))
                  (:slot-value-ic ,(symbol-name (cadr name))
                                  ,(%package-physical-name (symbol-package (cadr name)))))
                (compile-binary-call (cdr expr) "Runtime.SlotValue")))))
  (setf (gethash 'slot-boundp h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.SlotBoundp")))
  (setf (gethash '%set-slot-value h)
        (lambda (expr)
          ;; Pre-evaluate obj/slot/value into temps so the stack is empty while
          ;; each is compiled. Pushing them inline would leave obj+slot on the
          ;; stack during the value's evaluation, which is invalid CIL if the
          ;; value contains a try block (e.g. (setf (slot-value o 's)
          ;; (loop ... being each hash-value ...)) or a handler-case value).
          (let* ((da (compile-direct-call-args (cdr expr)))
                 (temps (car da))
                 (eval-instrs (cdr da)))
            `(,@eval-instrs
              ,@(loop for tmp in temps append `((:ldloc ,tmp)))
              (:call "Runtime.SetSlotValue")))))
  (setf (gethash '%allocate-instance h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.MakeInstanceRaw")))
  (setf (gethash '%slot-exists-p h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.SlotExists")))
  (setf (gethash '%change-class h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.ChangeClass"))))

  ;; GF primitives
  (setf (gethash '%make-gf h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.MakeGF")))
  (setf (gethash '%register-gf h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.RegisterGF")))
  (setf (gethash '%set-method-combination h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.SetMethodCombination")))
  (setf (gethash '%set-method-combination-order h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.SetMethodCombinationOrder")))
  (setf (gethash '%set-method-combination-args h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.SetMethodCombinationArgs")))
  (setf (gethash '%set-gf-lambda-list-info h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.SetGFLambdaListInfo"))))
  (setf (gethash '%set-method-lambda-list-info h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.SetMethodLambdaListInfo"))))
  (setf (gethash '%find-gf h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.FindGF")))
  (setf (gethash '%clear-defgeneric-inline-methods h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.ClearDefgenericInlineMethods")))
  (setf (gethash '%mark-defgeneric-inline-method h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.MarkDefgenericInlineMethod")))
  (setf (gethash '%make-method h)
        (lambda (expr)
          `(,@(compile-value-arg (second expr))
            ,@(compile-value-arg (third expr))
            ,@(compile-value-arg (fourth expr))
            (:call "Runtime.MakeMethod"))))
  (setf (gethash '%add-method h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.AddMethod")))
  (setf (gethash '%gf-methods h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.GetGFMethods")))
  (setf (gethash '%method-specializers h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.MethodSpecializers")))
  (setf (gethash '%method-qualifiers h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.MethodQualifiers")))
  (setf (gethash '%method-function h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.MethodFunction")))
  (setf (gethash (quote call-next-method) h)
        (lambda (expr)
          ;; No arguments means "the same arguments", which the loose entry can
          ;; pass along without materialising them into an array.
          (if (cdr expr)
              `(,@(compile-args-array (cdr expr)) (:call "Runtime.CallNextMethod"))
              (quote ((:call "Runtime.CallNextMethodLoose"))))))
  (setf (gethash 'next-method-p h)
        (lambda (expr)
          (if (cdr expr)
              `((:ldstr ,(format nil "NEXT-METHOD-P was called with ~D argument~:P; accepts exactly 0."
                                 (length (cdr expr))))
                (:call "Runtime.ProgramError"))
              '((:call "Runtime.NextMethodP")))))
  ;; defmethod-body capture intrinsics: return THIS invocation's cnm/nmp closures
  ;; from thread-local storage. Used in place of (symbol-function 'call-next-method).
  (setf (gethash '%captured-call-next-method h)
        (lambda (expr) (declare (ignore expr)) '((:call "Runtime.CapturedCnm"))))
  (setf (gethash '%captured-next-method-p h)
        (lambda (expr) (declare (ignore expr)) '((:call "Runtime.CapturedNmp"))))
  (setf (gethash '%make-instance-with-initargs h)
        (lambda (expr)
          ;; Evaluate the class into a temp FIRST so the stack is empty while
          ;; compile-args-array pre-evaluates the initarg values. An initarg value
          ;; containing a tagbody (loop/dolist: which emit :LEAVE and labels that
          ;; require an empty CIL stack) was otherwise compiled with the class still
          ;; on the stack, producing an unbalanced (stack-underflow) method; invalid
          ;; CIL, e.g. cl-ppcre's (make-instance 'alternation :choices (loop ... collect
          ;; ...)). A plain call (list/mapcar) never hit this because it loads nothing
          ;; before its args.
          (let ((class-tmp (gen-local "MI-CLASS"))
                (args-tmp (gen-local "MI-ARGS")))
            `((:declare-local ,class-tmp "LispObject")
              ,@(compile-value-arg (second expr))
              (:stloc ,class-tmp)
              (:declare-local ,args-tmp "LispObject[]")
              ,@(compile-args-array (cddr expr))
              (:stloc ,args-tmp)
              (:ldloc ,class-tmp)
              (:ldloc ,args-tmp)
              (:call "Runtime.MakeInstanceWithInitargs")))))

  ;; Derived predicates
  (setf (gethash 'zerop h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              `(,@(compile-value-arg (cadr expr)) (:call "Runtime.Zerop"))
              (compile-named-call 'zerop (cdr expr)))))
  (setf (gethash 'plusp h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              `(,@(compile-value-arg (cadr expr)) ,@(emit-fixnum 0) (:call "Runtime.GreaterThan"))
              (compile-named-call 'plusp (cdr expr)))))
  (setf (gethash 'minusp h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              `(,@(compile-value-arg (cadr expr)) ,@(emit-fixnum 0) (:call "Runtime.LessThan"))
              (compile-named-call 'minusp (cdr expr)))))
  (setf (gethash 'evenp h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              (let ((tmp (gensym "EVENP")))
                (compile-expr
                  `(let ((,tmp ,(cadr expr)))
                     (if (integerp ,tmp) (= (mod ,tmp 2) 0)
                         (error 'type-error :datum ,tmp :expected-type 'integer)))))
              (compile-named-call 'evenp (cdr expr)))))
  (setf (gethash 'oddp h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              (let ((tmp (gensym "ODDP")))
                (compile-expr
                  `(let ((,tmp ,(cadr expr)))
                     (if (integerp ,tmp) (not (= (mod ,tmp 2) 0))
                         (error 'type-error :datum ,tmp :expected-type 'integer)))))
              (compile-named-call 'oddp (cdr expr)))))

  ;; Math
  ;; A declared-float ABS computes the magnitude in place: the general entry
  ;; needs its argument boxed and hands back a fresh box, both of which the
  ;; declaration makes unnecessary.
  (setf (gethash (quote abs) h)
        (lambda (expr)
          (cond
            ((and (= (length (cdr expr)) 1) (double-float-typed-p (cadr expr)))
             `(,@(compile-as-double (cadr expr)) (:call "Math.AbsDouble")
               (:newobj "DoubleFloat")))
            ((and (= (length (cdr expr)) 1) (single-float-typed-p (cadr expr)))
             `(,@(compile-as-single (cadr expr)) (:conv-r4) (:call "Math.AbsSingle")
               (:newobj "SingleFloat")))
            (t (compile-unary-call (cdr expr) "Runtime.Abs")))))
  ;; Fixnum-typed operands take the raw int64 path: (mod i 128) in a loop over an
  ;; Int64-slot counter otherwise boxes the counter on every iteration just to
  ;; call the generic entry.
  (dolist (op '(mod rem))
    (let ((op op)
          (generic (if (eq op 'mod) "Runtime.Mod" "Runtime.Rem")))
      (setf (gethash op h)
            (lambda (expr)
              (let ((args (cdr expr)))
                (cond ((and (= (length args) 2)
                            (fixnum-typed-p (first args))
                            (fixnum-typed-p (second args)))
                       (compile-fixmod args op))
                      ((and (eq op 'mod) (= (length args) 2) (try-compile-u64 expr)))
                      (t (compile-binary-call args generic))))))))
  (dolist (op-method '((floor . "Runtime.FloorOp") (truncate . "Runtime.TruncateOp")
                        (ceiling . "Runtime.CeilingOp") (round . "Runtime.RoundOp")))
    (let ((op (car op-method)) (method (cdr op-method)))
      (setf (gethash op h)
            (lambda (expr)
              (let ((args (cdr expr)))
                (cond ((and (not *in-mv-context*) (not *in-tail-position*)
                            (float-round-int-range expr))
                       `(,@(compile-as-long expr) (:call "Fixnum.Make")))
                      ((and (not *in-mv-context*) (not *in-tail-position*)
                            (%float-round-arg-kind op args))
                       (let ((kind (%float-round-arg-kind op args)))
                         `(,@(compile-float-round-raw op (car args) kind)
                           (:call ,(if (eq kind :single)
                                       "Runtime.FloatIntegerS" "Runtime.FloatIntegerD")))))
                      ((= (length args) 1) (compile-binary-call (list (car args) 1) method))
                      ;; One value wanted of two fixnums: the quotient in int64,
                      ;; without building the second value.
                      ((and (= (length args) 2) (member op '(floor truncate))
                            (not *in-mv-context*) (not *in-tail-position*)
                            (fixnum-exact-p (first args)) (fixnum-exact-p (second args)))
                       `(,@(compile-long-binop-spilled (first args) (second args))
                         (:call ,(if (eq op 'floor) "Runtime.FloorFixnumBoxed"
                                     "Runtime.TruncateFixnumBoxed"))))
                      ((= (length args) 2) (compile-binary-call args method))
                      (t (compile-named-call op args))))))))
  (dolist (op '(ffloor ftruncate fceiling fround))
    (let ((op op))
      (setf (gethash op h)
            (lambda (expr)
              (let ((kind (and (not *in-mv-context*) (not *in-tail-position*)
                               (%float-round-arg-kind op (cdr expr)))))
                (case kind
                  (:single `(,@(compile-float-round-raw op (cadr expr) kind)
                             (:newobj "SingleFloat")))
                  (:double `(,@(compile-float-round-raw op (cadr expr) kind)
                             (:newobj "DoubleFloat")))
                  (t (compile-named-call op (cdr expr)))))))))
  (setf (gethash 'min h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (cond ((%fixnum-min-max-p expr)
                   `(,@(compile-as-long expr) (:call "Fixnum.Make")))
                  ((%double-min-max-p expr)
                   `(,@(compile-as-double expr) (:newobj "DoubleFloat")))
                  ((= (length args) 0) (compile-named-call 'min args))
                  ((= (length args) 1) (compile-named-call 'min args))
                  ((= (length args) 2) (compile-binary-call args "Runtime.Min"))
                  (t (compile-expr (reduce (lambda (a b) `(min ,a ,b)) args)))))))
  (setf (gethash 'max h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (cond ((%fixnum-min-max-p expr)
                   `(,@(compile-as-long expr) (:call "Fixnum.Make")))
                  ((%double-min-max-p expr)
                   `(,@(compile-as-double expr) (:newobj "DoubleFloat")))
                  ((= (length args) 0) (compile-named-call 'max args))
                  ((= (length args) 1) (compile-named-call 'max args))
                  ((= (length args) 2) (compile-binary-call args "Runtime.Max"))
                  (t (compile-expr (reduce (lambda (a b) `(max ,a ,b)) args)))))))
;; (POSITION-IF (LAMBDA (X) ...) SEQ) and its relatives, with a literal
  ;; one-parameter lambda and no keyword arguments: the lambda body is
  ;; substituted into a loop over the sequence instead of being made into a
  ;; function object. A lambda with a free variable is a closure, and building
  ;; one costs a LispFunction (about 240 B) on every call of the enclosing code.
  (dolist (op '(position-if position-if-not find-if find-if-not count-if count-if-not))
    (let ((op op))
      (setf (gethash op h)
            (lambda (expr)
              (let ((inline (inline-sequence-predicate-call op (cdr expr))))
                (if inline
                    (compile-expr inline)
                    (compile-named-call op (cdr expr))))))))
  (setf (gethash 'gcd h)
        (lambda (expr)
          (if (= (length (cdr expr)) 2)
              (compile-binary-call (cdr expr) "Runtime.Gcd")
              (compile-named-call 'gcd (cdr expr)))))
  (setf (gethash 'lcm h)
        (lambda (expr)
          (if (= (length (cdr expr)) 2)
              (compile-binary-call (cdr expr) "Runtime.Lcm")
              (compile-named-call 'lcm (cdr expr)))))
  (setf (gethash 'expt h)
        (lambda (expr)
          (if (= (length (cdr expr)) 2)
              (compile-binary-call (cdr expr) "Runtime.Expt")
              (compile-named-call 'expt (cdr expr)))))
  (setf (gethash 'ash h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (if (= (length args) 2)
                (or (compile-ash-fast args)
                    (and (not (integerp (second args))) (fixnum-typed-p expr)
                         `(,@(compile-as-long expr) (:call "Fixnum.Make")))
                    (try-compile-u64 expr)
                    (compile-binary-call args "Runtime.Ash"))
                (compile-named-call 'ash args)))))
  ;; (ldb (byte SIZE POS) X) with literal SIZE and POS is a shift and a mask.
  ;; Open-code it instead of calling LDB, which conses the byte specifier and
  ;; calls BYTE-SIZE / BYTE-POSITION at run time. The rewritten form goes back
  ;; through COMPILE-EXPR, so LOGAND / ASH get their own fast paths.
  (setf (gethash 'ldb h)
        (lambda (expr)
          (let ((spec (and (= (length (cdr expr)) 2) (%literal-byte-spec (cadr expr)))))
            (if (and spec (not (local-function-entry 'byte)))
                (let* ((size (car spec)) (pos (cdr spec))
                       (mask (1- (ash 1 size)))
                       (x (caddr expr)))
                  (compile-expr (if (zerop pos)
                                    `(logand ,x ,mask)
                                    `(logand (ash ,x ,(- pos)) ,mask))))
                (if (= (length (cdr expr)) 2)
                    (compile-binary-call (cdr expr) "Runtime.Ldb")
                    (compile-named-call 'ldb (cdr expr)))))))
  ;; A byte specifier is a cons (SIZE . POSITION), as BYTE builds it.
  (setf (gethash 'byte h)
        (lambda (expr)
          (if (= (length (cdr expr)) 2)
              (compile-expr `(cons ,@(cdr expr)))
              (compile-named-call 'byte (cdr expr)))))
  (setf (gethash 'byte-size h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              (compile-expr `(car ,(cadr expr)))
              (compile-named-call 'byte-size (cdr expr)))))
  (setf (gethash 'byte-position h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              (compile-expr `(cdr ,(cadr expr)))
              (compile-named-call 'byte-position (cdr expr)))))
  ;; (dpb V (byte SIZE POS) X) with literal SIZE and POS, likewise. V is
  ;; evaluated before X in the rewritten form too, as the argument order says.
  (setf (gethash 'dpb h)
        (lambda (expr)
          (let ((spec (and (= (length (cdr expr)) 3) (%literal-byte-spec (caddr expr)))))
            (if (and spec (not (local-function-entry 'byte)))
                (let* ((size (car spec)) (pos (cdr spec))
                       (mask (1- (ash 1 size)))
                       (v (cadr expr)) (x (cadddr expr)))
                  (compile-expr
                   `(logior ,(if (zerop pos)
                                 `(logand ,v ,mask)
                                 `(ash (logand ,v ,mask) ,pos))
                            (logand ,x ,(lognot (ash mask pos))))))
                (if (= (length (cdr expr)) 3)
                    (compile-ternary-call (cdr expr) "Runtime.Dpb")
                    (compile-named-call 'dpb (cdr expr)))))))
  (dolist (op '(mask-field deposit-field))
    (let ((op op))
      (setf (gethash op h)
            (lambda (expr)
              (if (%literal-byte-field-form-p expr)
                  (compile-expr (%literal-byte-field-as-logand expr))
                  (compile-named-call op (cdr expr)))))))
  (dolist (op '(logandc1 logandc2 logorc1 logorc2 lognand lognor logeqv))
    (let ((op op))
      (setf (gethash op h)
            (lambda (expr)
              (let ((form (%logical-complement-form expr)))
                (if form
                    (compile-expr form)
                    (compile-named-call op (cdr expr))))))))
  (setf (gethash 'lognot h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (cond
              ((and (= (length args) 1) (fixnum-typed-p (first args)))
               (compile-fixbit-not args))
              ((= (length args) 1)
               (compile-unary-call args "Runtime.Lognot"))
              (t (compile-named-call 'lognot args))))))
  (setf (gethash 'logcount h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              (compile-unary-call (cdr expr) "Runtime.Logcount")
              (compile-named-call 'logcount (cdr expr)))))
  (setf (gethash 'logtest h)
        (lambda (expr)
          (if (= (length (cdr expr)) 2)
              (compile-binary-call (cdr expr) "Runtime.Logtest")
              (compile-named-call 'logtest (cdr expr)))))
  (setf (gethash 'integer-length h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              (compile-unary-call (cdr expr) "Runtime.IntegerLength")
              (compile-named-call 'integer-length (cdr expr)))))
  (setf (gethash 'logbitp h)
        (lambda (expr)
          (if (= (length (cdr expr)) 2)
              (compile-binary-call (cdr expr) "Runtime.Logbitp")
              (compile-named-call 'logbitp (cdr expr)))))
  (dolist (item (list (list 'logior "Runtime.Logior2" "Runtime.Logior" 0 :or)
                      (list 'logand "Runtime.Logand2" "Runtime.Logand" -1 :and)
                      (list 'logxor "Runtime.Logxor2" "Runtime.Logxor" 0 :xor)))
    (let ((op (first item)) (method2 (second item)) (methodN (third item))
          (zero-val (fourth item)) (cil-op (fifth item)))
      (setf (gethash op h)
            (lambda (expr)
              (let ((args (cdr expr)))
                (case (length args)
                  (0 (emit-fixnum zero-val))
                  (1 (let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args))))
                  ;; Asked of the whole form, not the operands: (logand X M)
                  ;; with a non-negative M is exact even when X is not.
                  (2 (cond ((fixnum-typed-p expr)
                            (compile-fixbit-binop args cil-op))
                           ((try-compile-u64 expr))
                           (t (compile-binary-call args method2))))
                  (t (if (or (<= (length args) 8) (%nary-fixnum-foldable-p op args))
                         (compile-expr (list* op (list op (first args) (second args)) (cddr args)))
                         `(,@(compile-args-array args) (:call ,methodN))))))))))

  ;; I/O
  (setf (gethash 'print h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond ((= nargs 1) `(,@(compile-value-arg (cadr expr)) (:call "Runtime.Print")))
                  ((= nargs 2) `(,@(compile-value-args (list (cadr expr) (caddr expr))) (:call "Runtime.Print2")))
                  (t (compile-static-program-error (format nil "PRINT: wrong number of arguments: ~a (expected 1-2)" nargs)))))))
  (setf (gethash 'prin1 h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond ((= nargs 1) `(,@(compile-value-arg (cadr expr)) (:call "Runtime.Prin1")))
                  ((= nargs 2) `(,@(compile-value-args (list (cadr expr) (caddr expr))) (:call "Runtime.Prin12")))
                  (t (compile-static-program-error (format nil "PRIN1: wrong number of arguments: ~a (expected 1-2)" nargs)))))))
  (setf (gethash 'princ h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond ((= nargs 1) `(,@(compile-value-arg (cadr expr)) (:call "Runtime.Princ")))
                  ((= nargs 2) `(,@(compile-value-args (list (cadr expr) (caddr expr))) (:call "Runtime.Princ2")))
                  (t (compile-static-program-error (format nil "PRINC: wrong number of arguments: ~a (expected 1-2)" nargs)))))))
  (setf (gethash 'terpri h)
        (lambda (expr)
          (if (> (length (cdr expr)) 1)
              (compile-static-program-error (format nil "TERPRI: wrong number of arguments: ~a (expected 0-1)" (length (cdr expr))))
              `(,@(compile-args-array (cdr expr)) (:call "Runtime.Terpri")))))
  (setf (gethash 'fresh-line h)
        (lambda (expr)
          (if (> (length (cdr expr)) 1)
              (compile-static-program-error (format nil "FRESH-LINE: wrong number of arguments: ~a (expected 0-1)" (length (cdr expr))))
              `(,@(compile-args-array (cdr expr)) (:call "Runtime.FreshLine")))))
  (setf (gethash 'format h)
        (lambda (expr)
          (let ((stream-tmp (gen-local "FMTDST")) (args-tmp (gen-local "FMTARGS")))
            `((:declare-local ,stream-tmp "LispObject")
              (:declare-local ,args-tmp "LispObject[]")
              ,@(compile-value-arg (cadr expr)) (:stloc ,stream-tmp)
              ,@(compile-args-array (cddr expr)) (:stloc ,args-tmp)
              (:ldloc ,stream-tmp) (:ldloc ,args-tmp)
              (:call "Runtime.Format")))))
  (setf (gethash 'open h)
        (lambda (expr)
          (if (null (cdr expr))
              (compile-static-program-error "OPEN: wrong number of arguments: 0 (expected at least 1)")
              (let ((path-tmp (gen-local "OPNDST")) (args-tmp (gen-local "OPNARGS")))
                `((:declare-local ,path-tmp "LispObject")
                  (:declare-local ,args-tmp "LispObject[]")
                  ,@(compile-value-arg (cadr expr)) (:stloc ,path-tmp)
                  ,@(compile-args-array (cddr expr)) (:stloc ,args-tmp)
                  (:ldloc ,path-tmp) (:ldloc ,args-tmp)
                  (:call "Runtime.OpenFile"))))))
  ;; open-stream-p goes through the named call (symbol-function), not a direct
  ;; Runtime.OpenStreamP inline, so a user-defined method: e.g. flexi-streams,
  ;; whose open-stream-p delegates to the underlying stream: is dispatched
  ;; instead of the builtin gray-stream default (which always returns T). When no
  ;; user method exists the symbol-function is still the builtin, so the default
  ;; behavior is unchanged.
  (setf (gethash 'open-stream-p h) (lambda (expr) (compile-named-call 'open-stream-p (cdr expr))))
  (setf (gethash 'read-line h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((= nargs 4) (compile-named-call 'read-line (cdr expr)))
              ((= nargs 0) `(,@(compile-value-args (list '*standard-input* t nil)) (:call "Runtime.ReadLineMv")))
              ((= nargs 1) `(,@(compile-value-args (list (cadr expr) t nil)) (:call "Runtime.ReadLineMv")))
              ((= nargs 2) `(,@(compile-value-args (list (cadr expr) (caddr expr) nil)) (:call "Runtime.ReadLineMv")))
              ((= nargs 3) `(,@(compile-value-args (list (cadr expr) (caddr expr) (cadddr expr))) (:call "Runtime.ReadLineMv")))
              (t (compile-static-program-error (format nil "READ-LINE: wrong number of arguments: ~a (expected 0-4)" nargs)))))))
  (setf (gethash 'read-char h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((= nargs 4) (compile-named-call 'read-char (cdr expr)))
              ((= nargs 0) `(,@(compile-value-args (list '*standard-input* t nil)) (:call "Runtime.ReadChar")))
              ((= nargs 1) `(,@(compile-value-args (list (cadr expr) t nil)) (:call "Runtime.ReadChar")))
              ((= nargs 2) `(,@(compile-value-args (list (cadr expr) (caddr expr) nil)) (:call "Runtime.ReadChar")))
              ((= nargs 3) `(,@(compile-value-args (list (cadr expr) (caddr expr) (cadddr expr))) (:call "Runtime.ReadChar")))
              (t (compile-static-program-error (format nil "READ-CHAR: wrong number of arguments: ~a (expected 0-4)" nargs)))))))
  (setf (gethash 'read-char-no-hang h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((= nargs 4) (compile-named-call 'read-char-no-hang (cdr expr)))
              ((= nargs 0) `(,@(compile-value-args (list '*standard-input* t nil)) (:call "Runtime.ReadCharNoHang")))
              ((= nargs 1) `(,@(compile-value-args (list (cadr expr) t nil)) (:call "Runtime.ReadCharNoHang")))
              ((= nargs 2) `(,@(compile-value-args (list (cadr expr) (caddr expr) nil)) (:call "Runtime.ReadCharNoHang")))
              ((= nargs 3) `(,@(compile-value-args (list (cadr expr) (caddr expr) (cadddr expr))) (:call "Runtime.ReadCharNoHang")))
              (t (compile-static-program-error (format nil "READ-CHAR-NO-HANG: wrong number of arguments: ~a (expected 0-4)" nargs)))))))
  (setf (gethash 'listen h)
        (lambda (expr)
          (if (null (cdr expr))
              `(,@(compile-value-arg '*standard-input*) (:call "Runtime.Listen"))
              (compile-unary-call (cdr expr) "Runtime.Listen"))))
  (setf (gethash 'clear-input h)
        (lambda (expr)
          (if (null (cdr expr))
              `(,@(compile-value-arg '*standard-input*) (:call "Runtime.ClearInput"))
              (compile-unary-call (cdr expr) "Runtime.ClearInput"))))
  (setf (gethash 'write-byte h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.WriteByte")))
  (setf (gethash 'peek-char h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((= nargs 5) (compile-named-call 'peek-char (cdr expr)))
              ((= nargs 0) `(,@(compile-value-args (list nil '*standard-input* t nil)) (:call "Runtime.PeekChar")))
              ((= nargs 1) `(,@(compile-value-args (list (cadr expr) '*standard-input* t nil)) (:call "Runtime.PeekChar")))
              ((= nargs 2) `(,@(compile-value-args (list (cadr expr) (caddr expr) t nil)) (:call "Runtime.PeekChar")))
              ((= nargs 3) `(,@(compile-value-args (list (cadr expr) (caddr expr) (cadddr expr) nil)) (:call "Runtime.PeekChar")))
              ((= nargs 4) `(,@(compile-value-args (list (cadr expr) (caddr expr) (cadddr expr) (car (cddddr expr)))) (:call "Runtime.PeekChar")))
              (t (compile-static-program-error (format nil "PEEK-CHAR: wrong number of arguments: ~a (expected 0-5)" nargs)))))))
  (setf (gethash 'unread-char h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg '*standard-input*) (:call "Runtime.UnreadChar"))
              (compile-binary-call (cdr expr) "Runtime.UnreadChar"))))
  (setf (gethash 'write-char h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg '*standard-output*) (:call "Runtime.WriteChar"))
              (compile-binary-call (cdr expr) "Runtime.WriteChar"))))
  ;; No :start/:end: call the two/one-argument entry points directly. The
  ;; args-array form allocates a LispObject[] per call, which output-heavy code
  ;; pays on every string it writes.
  (setf (gethash 'write-string h)
        (lambda (expr)
          (case (length (cdr expr))
            (1 (compile-unary-call (cdr expr) "Runtime.WriteString1"))
            (2 (compile-binary-call (cdr expr) "Runtime.WriteString2"))
            (t `(,@(compile-args-array (cdr expr)) (:call "Runtime.WriteString"))))))
  (setf (gethash 'write-line h)
        (lambda (expr)
          (case (length (cdr expr))
            (1 (compile-unary-call (cdr expr) "Runtime.WriteLine1"))
            (2 (compile-binary-call (cdr expr) "Runtime.WriteLine2"))
            (t `(,@(compile-args-array (cdr expr)) (:call "Runtime.WriteLine"))))))
  (setf (gethash 'directory h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.Directory"))))
  (setf (gethash 'probe-file h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.ProbeFile")))
  (setf (gethash 'truename h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Truename")))
  (setf (gethash 'delete-file h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.DeleteFile")))
  (setf (gethash 'file-author h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.FileAuthor")))
  (setf (gethash 'file-error-pathname h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.FileErrorPathname")))
  (setf (gethash 'rename-file h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.RenameFile")))
  (setf (gethash 'file-write-date h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.FileWriteDate")))

  ;; String streams / pathnames
  (setf (gethash 'make-string-output-stream h)
        (lambda (expr)
          (if (null (cdr expr))
              '((:call "Runtime.MakeStringOutputStream"))
              (compile-named-call 'make-string-output-stream (cdr expr)))))
  (setf (gethash 'get-output-stream-string h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.GetOutputStreamString")))
  (setf (gethash 'make-string-input-stream h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.MakeStringInputStream"))))
  (setf (gethash 'make-pathname h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.MakePathname"))))
  (setf (gethash 'pathname h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Pathname")))
  (setf (gethash 'namestring h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Namestring")))

  ;; Read
  (setf (gethash 'read h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((> nargs 4) (compile-static-program-error (format nil "READ: too many arguments: ~D (expected at most 4)" nargs)))
              ;; With recursive-p: fall through to generic dispatch (registered LispFunction handles it)
              ((= nargs 4) (compile-named-call 'read (cdr expr)))
              ((= nargs 0) `(,@(compile-value-args (list '*standard-input* t nil)) (:call "Runtime.ReadFromStream")))
              ((= nargs 1) `(,@(compile-value-args (list (cadr expr) t nil)) (:call "Runtime.ReadFromStream")))
              ((= nargs 3) `(,@(compile-value-args (list (cadr expr) (caddr expr) (cadddr expr))) (:call "Runtime.ReadFromStream")))
              (t `(,@(compile-value-args (list (cadr expr) (caddr expr) nil)) (:call "Runtime.ReadFromStream")))))))
  (setf (gethash 'read-from-string h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.ReadFromString"))))
  (setf (gethash 'read-preserving-whitespace h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((> nargs 4) (compile-static-program-error (format nil "READ-PRESERVING-WHITESPACE: too many arguments: ~D (expected at most 4)" nargs)))
              ;; With recursive-p: fall through to generic dispatch
              ((= nargs 4) (compile-named-call 'read-preserving-whitespace (cdr expr)))
              ((= nargs 0) `(,@(compile-value-args (list '*standard-input* t nil)) (:call "Runtime.ReadPreservingWhitespace")))
              ((= nargs 1) `(,@(compile-value-args (list (cadr expr) t nil)) (:call "Runtime.ReadPreservingWhitespace")))
              ((= nargs 3) `(,@(compile-value-args (list (cadr expr) (caddr expr) (cadddr expr))) (:call "Runtime.ReadPreservingWhitespace")))
              (t `(,@(compile-value-args (list (cadr expr) (caddr expr) nil)) (:call "Runtime.ReadPreservingWhitespace")))))))

  ;; Eval / gensym / misc
  (setf (gethash 'eval h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Eval")))
  (setf (gethash 'gensym h)
        (lambda (expr)
          (if (cdr expr)
              (compile-unary-call (cdr expr) "Runtime.Gensym")
              '((:call "Runtime.Gensym0")))))
  (setf (gethash 'streamp h)
        (lambda (expr)
          (if (and (cdr expr) (null (cddr expr)))
              `(,@(compile-value-arg (cadr expr)) ,@(compile-quoted 'stream) (:call "Runtime.Typep"))
              (compile-named-call 'streamp (cdr expr)))))
  (setf (gethash 'lisp-implementation-type h)
        (lambda (expr)
          (if (null (cdr expr))
              '((:ldstr "dotcl") (:newobj "LispString"))
              (compile-named-call 'lisp-implementation-type (cdr expr)))))

  ;; String / char ops
  (setf (gethash 'random h)
        (lambda (expr)
          (let ((n (length (cdr expr))))
            (cond
              ((= n 1) (compile-unary-call (cdr expr) "Runtime.Random" "RANDOM"))
              ((= n 2) (compile-binary-call (cdr expr) "Runtime.Random2" "RANDOM"))
              (t (compile-static-program-error (format nil "RANDOM: wrong number of arguments: ~a (expected 1 or 2)" n)))))))
  (setf (gethash 'string-trim h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.StringTrim")))
  (setf (gethash 'string-left-trim h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.StringLeftTrim")))
  (setf (gethash 'string-right-trim h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.StringRightTrim")))
  (setf (gethash 'char-code h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.CharCode")))
  (setf (gethash 'code-char h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.CodeChar")))
  (setf (gethash 'digit-char-p h)
        (lambda (expr)
          (let ((args (cdr expr)) (nargs (length (cdr expr))))
            (cond
              ((= nargs 0) (compile-static-program-error "DIGIT-CHAR-P: too few arguments: 0 (expected 1-2)"))
              ((> nargs 2) (compile-static-program-error (format nil "DIGIT-CHAR-P: too many arguments: ~D (expected 1-2)" nargs)))
              ;; The default radix has to be pushed as an i8: Fixnum.Make takes a
              ;; long, and an i4 there is an unverifiable stack-type mismatch.
              ((= nargs 1) `(,@(compile-value-arg (car args)) ,@(emit-fixnum 10) (:call "Runtime.DigitCharP")))
              (t (compile-binary-call args "Runtime.DigitCharP"))))))
  (setf (gethash 'string h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.String")))
  ;; CHAR / SCHAR in value position. SCHAR had no entry at all, so every bare
  ;; element read went through the generic call; CHAR had one that still boxed
  ;; the subscript. Both are the same two-argument element read, and the typed
  ;; entry takes the subscript raw.
  (setf (gethash 'char h)
        (lambda (expr)
          (if (= (length (cdr expr)) 2)
              (compile-string-char-as-object (cadr expr) (caddr expr))
              (compile-named-call 'char (cdr expr)))))
  (setf (gethash 'schar h)
        (lambda (expr)
          (if (= (length (cdr expr)) 2)
              (compile-string-char-as-object (cadr expr) (caddr expr))
              (compile-named-call 'schar (cdr expr)))))
  (setf (gethash 'search h)
        (lambda (expr)
          (let ((n-args (length (cdr expr))))
            (cond
              ((< n-args 2) (compile-static-program-error (format nil "SEARCH: wrong number of arguments: ~a (expected at least 2)" n-args)))
              ((cddr (cdr expr)) (compile-named-call 'search (cdr expr)))
              (t (compile-binary-call (cdr expr) "Runtime.Search" "SEARCH"))))))

  ;; Symbol ops
  (setf (gethash 'symbol-name h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.SymbolName")))
  (setf (gethash 'symbol-package h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.SymbolPackage")))

  ;; Package ops (eq-accessible versions; string= versions remain in compile-form fallback)
  (setf (gethash 'hash-table-pairs h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.HashTablePairs")))
  (setf (gethash 'find-package h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.FindPackage")))
  (setf (gethash 'package-name h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.PackageName")))
  (setf (gethash 'package-error-package h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.PackageErrorPackage")))
  (setf (gethash 'delete-package h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.DeletePackage")))
  (setf (gethash 'find-symbol h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.FindSymbolMv"))))
  (setf (gethash 'unintern h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.UninternSymbol"))))
  (setf (gethash 'package-used-by-list h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.PackageUsedByList")))
  (setf (gethash 'package-use-list h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.PackageUseListL")))
  (setf (gethash 'package-shadowing-symbols h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.PackageShadowingSymbols")))
  (setf (gethash 'package-nicknames h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.PackageNicknamesList")))
  (setf (gethash 'list-all-packages h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.ListAllPackagesV"))))
  (setf (gethash 'make-package h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.MakePackageK"))))
  (setf (gethash 'rename-package h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.RenamePackage"))))

  ;; Sequence ops
  (setf (gethash 'elt h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Elt")))
  (setf (gethash 'subseq h) (lambda (expr) (compile-subseq (cdr expr))))
  (setf (gethash 'make-array h) (lambda (expr) (compile-make-array (cdr expr))))
  (setf (gethash 'replace h) (lambda (expr) (compile-replace (cdr expr))))
  (setf (gethash 'concatenate h) (lambda (expr) (compile-concatenate (cdr expr))))
  (setf (gethash 'sort h)
        (lambda (expr)
          (if (cddr (cdr expr))
              (compile-named-call 'sort (cdr expr))
              (compile-binary-call (cdr expr) "Runtime.Sort"))))
  (setf (gethash 'stable-sort h)
        (lambda (expr)
          (if (cddr (cdr expr))
              (compile-named-call 'stable-sort (cdr expr))
              ;; Runtime.StableSort, not Runtime.Sort: the 2-arg fast path must
              ;; keep the order of elements the predicate calls equal.
              (compile-binary-call (cdr expr) "Runtime.StableSort"))))
  (setf (gethash 'reverse h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Reverse" "REVERSE")))
  (setf (gethash 'coerce h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Coerce")))

  ;; (APPLY f a b ... list). One or two fixed arguments ahead of the list go to a
  ;; runtime entry that takes them positionally. The general rewrite below conses a
  ;; LIST* prefix per call, for a list APPLY then walks straight back apart.
  (setf (gethash 'apply h)
        (lambda (expr &aux (last-arg (car (last expr))))
          (if (and (cddr expr) (consp last-arg) (eq (car last-arg) 'list)
                   (not (local-function-entry 'list))
                   (not (member 'list *notinline-functions* :test #'equal)))
              (compile-expr `(funcall ,@(butlast (cdr expr)) ,@(cdr last-arg)))
          (let* ((args (cdr expr))
                 (mode (and (<= 2 (length args) 4) (%call-site-mv-mode args)))
                 (code (cond
                         ((null (cddr args))
                          (compile-binary-call args (if mode "Runtime.ApplyM" "Runtime.Apply")))
                         ((null (cdddr args))
                          (compile-ternary-call args (if mode "Runtime.ApplySpread1M" "Runtime.ApplySpread1")))
                         ((null (cddddr args))
                          (compile-quaternary-call args (if mode "Runtime.ApplySpread2M" "Runtime.ApplySpread2")))
                         (t (compile-expr `(apply ,(car args) (list* ,@(cdr args))))))))
            ;; The value mode goes after the arguments, before the call.
            (if mode
                (append (butlast code) mode (last code))
                code)))))
  (setf (gethash 'maphash h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Maphash")))
  ;; A literal LAMBDA opens into a loop (%MAP-LAMBDA-EXPANSION).
  (setf (gethash '%map-open h) #'compile-%map-open)
  (dolist (op '(mapc mapcan some every notany notevery))
    (setf (gethash op h)
          (lambda (expr)
            (let ((opened (%map-lambda-expansion (car expr) (cdr expr))))
              (if opened
                  (compile-expr opened)
                  (compile-named-call (car expr) (cdr expr)))))))
  (setf (gethash 'mapcar h)
        (lambda (expr)
          (let ((args (cdr expr))
                (opened (%map-lambda-expansion (quote mapcar) (cdr expr))))
            (cond
             (opened (compile-expr opened))
             ((cddr args)
                ;; Two lists is the common multi-list shape, and Runtime.Mapcar2
                ;; takes them as separate arguments: no array for the lists, and
                ;; none for the cursors MapcarN keeps. The same stack rule as below
                ;; applies, so the function and both lists go to temps first.
                (if (null (cdddr args))
                    (let ((fn-tmp (gen-local "MAPFN"))
                          (l1-tmp (gen-local "MAPL1"))
                          (l2-tmp (gen-local "MAPL2")))
                      `((:declare-local ,fn-tmp "LispObject")
                        (:declare-local ,l1-tmp "LispObject")
                        (:declare-local ,l2-tmp "LispObject")
                        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                            `(,@(compile-expr (first args)) (:stloc ,fn-tmp)
                              ,@(compile-expr (second args)) (:stloc ,l1-tmp)
                              ,@(compile-expr (third args)) (:stloc ,l2-tmp)))
                        (:ldloc ,fn-tmp) (:ldloc ,l1-tmp) (:ldloc ,l2-tmp)
                        (:call "Runtime.Mapcar2")))
                ;; Multi-list MAPCAR. Evaluate the function into a temp FIRST so the
                ;; stack is empty before compile-args-array compiles the list args.
                ;; Leaving the function value on the stack while a list arg expands
                ;; to code that opens a try-block (e.g. a LOOP ... COLLECT) violated
                ;; CIL's "stack must be empty at try-block entry" rule and produced
                ;; invalid IL ("CLR detected an invalid program").
                (let ((fn-tmp (gen-local "MAPFN"))
                      (arr-tmp (gen-local "MAPARR")))
                  `((:declare-local ,fn-tmp "LispObject")
                    ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                        (compile-expr (car args)))
                    (:stloc ,fn-tmp)
                    ;; Build the list-args array with an empty operand stack
                    ;; (compile-args-array already pre-evaluates each arg to a temp),
                    ;; then stash it so neither the function value nor a partial array
                    ;; sits on the stack while a LOOP arg opens its try-block.
                    (:declare-local ,arr-tmp "LispObject[]")
                    ,@(compile-args-array (cdr args))
                    (:stloc ,arr-tmp)
                    (:ldloc ,fn-tmp)
                    (:ldloc ,arr-tmp)
                    (:call "Runtime.MapcarN")))))
             (t (compile-binary-call args "Runtime.Mapcar"))))))

  ;; Property list
  (setf (gethash 'get h)
        (lambda (expr)
          (let* ((args (cdr expr)) (nargs (length args)))
            (cond
              ((< nargs 2) (compile-static-program-error (format nil "GET: too few arguments: ~D (expected at least 2)" nargs)))
              ((> nargs 3) (compile-static-program-error (format nil "GET: too many arguments: ~D (expected at most 3)" nargs)))
              (t `(,@(compile-value-args (list (first args) (second args) (third args)))
                   (:call "Runtime.GetProp")))))))
  (setf (gethash 'put-prop h)
        (lambda (expr)
          `(,@(compile-value-arg (cadr expr))
            ,@(compile-value-arg (caddr expr))
            ,@(compile-value-arg (cadddr expr))
            (:call "Runtime.PutProp"))))
  (setf (gethash 'remprop h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Remprop")))
  (setf (gethash 'copy-symbol h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (cond ((= (length args) 1) (compile-unary-call args "Runtime.CopySymbol"))
                  ((= (length args) 2) (compile-binary-call args "Runtime.CopySymbolFull"))
                  (t (compile-static-program-error (format nil "COPY-SYMBOL: wrong number of arguments: ~a" (length args))))))))

  ;; Hash table
  (setf (gethash 'make-hash-table h)
        (lambda (expr)
          ;; Open-coded only for () and (:TEST x). Anything else, including a
          ;; key that is computed rather than written as a keyword, goes
          ;; through the function, which evaluates every argument and parses
          ;; the keywords at run time.
          (let* ((args (cdr expr))
                 (test-only (and (= (length args) 2) (eq (first args) :test)))
                 (test-expr (and test-only (second args)))
                 (has-other-kw (and args (not test-only))))
            (cond
              ;; Any keyword other than :test -> use variadic function path
              ;; (keeps SYNCHRONIZED handling in the registered LispFunction)
              (has-other-kw
               (compile-named-call 'make-hash-table (cdr expr)))
              (test-expr
               (let ((literal-name
                      (cond
                        ((and (consp test-expr) (eq (car test-expr) 'function)) (symbol-name (cadr test-expr)))
                        ((and (consp test-expr) (eq (car test-expr) 'quote)) (symbol-name (cadr test-expr)))
                        (t nil))))
                 (if literal-name
                     `((:load-sym-keyword ,literal-name) (:call "Runtime.MakeHashTable"))
                     `(,@(compile-value-arg test-expr) (:call "Runtime.MakeHashTable")))))
              (t '((:call "Runtime.MakeHashTable0")))))))
  (setf (gethash 'gethash h) (lambda (expr) (compile-gethash (cdr expr))))
  (setf (gethash 'puthash h) (lambda (expr) (compile-puthash (cdr expr))))
  (setf (gethash 'remhash h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Remhash")))

  ;; Values
  (setf (gethash 'values h) (lambda (expr) (compile-values-call (cdr expr))))
  ;; (%mv-capture FORM): evaluate FORM keeping its values, stash them in the
  ;; per-thread bind snapshot and leave the primary value. (%mv-nth I) then reads
  ;; the I-th. MULTIPLE-VALUE-BIND uses the pair instead of building a list.
  (setf (gethash '%mv-capture h)
        (lambda (expr)
          (let ((form (cadr expr)))
            `((:call "MultipleValues.Reset")
              ,@(%compile-for-capture form)
              (:call "MultipleValues.CaptureForBind")))))
  (setf (gethash '%mv-nth h)
        (lambda (expr)
          (if (and (= (length expr) 2) (typep (cadr expr) '(integer 0 1000)))
              `((:ldc-i4 ,(cadr expr)) (:call "MultipleValues.BindNthI"))
              (compile-unary-call (cdr expr) "MultipleValues.BindNth"))))
  ;; (NTH-VALUE n form) with a literal N reads the value straight out of what
  ;; FORM returned. The macro expansion it otherwise takes is
  ;; (NTH n (MULTIPLE-VALUE-LIST form)), which builds the whole list of values to
  ;; hand back one of them -- three conses to read value 1 of three. A computed
  ;; index still goes through the macro.
  (setf (gethash 'nth-value h)
        (lambda (expr)
          (let ((n (cadr expr))
                (form (caddr expr)))
            (if (and (integerp n) (<= 0 n))
                `((:call "MultipleValues.Reset")
                  ;; FORM's values are read the moment it returns.
                  ,@(%compile-for-capture form)
                  (:ldc-i4 ,n)
                  (:call "Runtime.NthValueOf"))
                (compile-expr `(nth ,n (multiple-value-list ,form)))))))

  (setf (gethash 'multiple-value-list h)
        (lambda (expr)
          `((:call "MultipleValues.Reset")
            ;; Keep MV context (multiple-value-list needs its arg's values) but
            ;; block tail: a self-tail-call arg would otherwise fire TCO and
            ;; dead-code the MultipleValuesList1 call.
            ,@(let ((*in-tail-position* nil) (*in-mv-context* t))
                ;; No capture mode here: consing the list out of the bind
                ;; snapshot measured slower than out of an MvReturn, and the
                ;; list conses anyway.
                (compile-expr (cadr expr)))
            (:call "Runtime.MultipleValuesList1"))))

  ;; Shorthand
  (setf (gethash '1+ h)
        (lambda (expr)
          (cond
            ((and (= (length (cdr expr)) 1)
                  (fixnum-typed-p (cadr expr))
                  (fixnum-arith-unboxed-safe-p expr))
             ;; Native: unbox arg, add 1, box
             `(,@(compile-as-long (cadr expr))
               (:ldc-i8 1) (:add)
               (:call "Fixnum.Make")))
            ((= (length (cdr expr)) 1)
             (compile-unary-call (cdr expr) "Runtime.Increment" "1+"))
            (t (compile-named-call '1+ (cdr expr))))))
  (setf (gethash '1- h)
        (lambda (expr)
          (cond
            ((and (= (length (cdr expr)) 1)
                  (fixnum-typed-p (cadr expr))
                  (fixnum-arith-unboxed-safe-p expr))
             `(,@(compile-as-long (cadr expr))
               (:ldc-i8 1) (:sub)
               (:call "Fixnum.Make")))
            ((= (length (cdr expr)) 1)
             (compile-unary-call (cdr expr) "Runtime.Decrement" "1-"))
            (t (compile-named-call '1- (cdr expr))))))
  ;; %dotimes-1+: increment emitted by the dotimes expansion for a
  ;; fixnum-declared counter. The macro asserts (from loop structure: the
  ;; increment site is only reached while counter < limit <= int64-max) that
  ;; the result fits int64, so a fixnum-typed counter takes the raw add path
  ;; without needing a +1-widened range proof. A counter that is NOT
  ;; fixnum-typed here (e.g. captured by a closure, hence boxed) falls back to
  ;; the generic promoting increment.
  (setf (gethash '%dotimes-1+ h)
        (lambda (expr)
          (if (and (= (length (cdr expr)) 1)
                   (fixnum-typed-p (cadr expr)))
              `(,@(compile-as-long (cadr expr))
                (:ldc-i8 1) (:add)
                (:call "Fixnum.Make"))
              (compile-unary-call (cdr expr) "Runtime.Increment" "1+"))))
  ;; %dotimes-limit: the DOTIMES count, evaluated once before the loop and
  ;; clamped into int64 (Runtime.DotimesLimit), so the loop test is a raw
  ;; compare. As an object only when the limit slot is not native.
  (setf (gethash '%dotimes-limit h)
        (lambda (expr)
          `(,@(compile-value-arg (cadr expr))
            (:call "Runtime.DotimesLimit")
            (:call "Fixnum.Make"))))
  ;; %loop-limit-floor: the same for a LOOP limit whose test changes at the
  ;; floor of a non-integer (up inclusive, down exclusive).
  (setf (gethash '%loop-limit-floor h)
        (lambda (expr)
          `(,@(compile-value-arg (cadr expr))
            (:call "Runtime.LoopLimitFloor")
            (:call "Fixnum.Make"))))

  ;; %loop-by: LOOP's BY value, checked to be a positive real.
  (setf (gethash '%loop-by h)
        (lambda (expr)
          (compile-unary-call (cdr expr) "Runtime.LoopBy" "%LOOP-BY")))

  ;; Special forms
  (setf (gethash 'if h) (lambda (expr) (compile-if (cdr expr))))
  (setf (gethash 'when h) (lambda (expr) (compile-expr `(if ,(cadr expr) (progn ,@(cddr expr))))))
  (setf (gethash 'unless h) (lambda (expr) (compile-expr `(if ,(cadr expr) nil (progn ,@(cddr expr))))))
  (setf (gethash 'cond h) (lambda (expr) (compile-cond (cdr expr))))
  (setf (gethash 'and h) (lambda (expr) (compile-and (cdr expr))))
  (setf (gethash 'or h) (lambda (expr) (compile-or (cdr expr))))
  ;; progn preserves top-level-ness per CLHS 3.2.3.1
  (setf (gethash 'progn h) (lambda (expr) (let ((*at-toplevel* *compile-was-toplevel*)) (compile-progn (cdr expr)))))
  (setf (gethash 'let h)
        (lambda (expr)
          (let* ((*let-subst-size-memo* (or *let-subst-size-memo* (list nil)))
                 (form (or (let-scalar-replace-struct (cadr expr) (cddr expr))
                           (let-substitute-single-reads (cadr expr) (cddr expr)))))
            (if form
                (compile-expr form)
                (compile-let (cadr expr) (cddr expr) nil)))))
  (setf (gethash 'let* h)
        (lambda (expr)
          (let* ((*let-subst-size-memo* (or *let-subst-size-memo* (list nil)))
                 (form (and (consp (cadr expr)) (null (cdr (cadr expr)))
                            (or (let-scalar-replace-struct (cadr expr) (cddr expr))
                                (let-substitute-single-reads (cadr expr) (cddr expr))))))
            (if form
                (compile-expr form)
                (compile-let-star (cadr expr) (cddr expr))))))
  (setf (gethash 'setq h)
        (lambda (expr)
          (let ((pairs (cdr expr)))
            (if (null pairs)
                (emit-nil)
                (if (<= (length pairs) 2)
                    (compile-setq (first pairs) (second pairs))
                    (let ((forms '()))
                      (loop while pairs
                            do (push `(setq ,(pop pairs) ,(pop pairs)) forms))
                      (compile-progn (nreverse forms))))))))
  (setf (gethash 'lambda h) (lambda (expr) (compile-lambda (cadr expr) (cddr expr))))
  (setf (gethash 'funcall h) (lambda (expr) (compile-funcall (cdr expr))))
  (setf (gethash 'defun h) #'compile-defun-toplevel)
  ;; %inline-cs-spliced: dotnet-cs:inline-cs macro expansion target.
  ;; Form: (%inline-cs-spliced ((arg1 arg2 ...)) :returns long ((:LDARG-0) ...))
  ;;   ARGN are Lisp value forms (compiled to LispObject Fixnum).
  ;;   The instr-list comes from (dotnet-cs:disassemble-cs ...) at macro
  ;;   expansion time: opcodes like (:LDARG-0) refer to the corresponding
  ;;   ARGN. We unbox each arg to int64, store in a fresh local, rewrite
  ;;   the C# IL with %INLINE-CS-TRANSLATE, and box the final stack-top via
  ;;   Fixnum.Make. Only fixnum bindings + fixnum
  ;;   return are supported for the MVP. Other types throw.
  ;; Use the DOTCL-INTERNAL symbol so Startup.Sym at runtime returns
  ;; the same Symbol instance that the macro-expanded user form refers to.
  (setf (gethash (intern "%INLINE-CS-SPLICED" "DOTCL-INTERNAL") h)
        (lambda (expr)
          (let* ((args (cadr expr))
                 (returns-kw (caddr expr))      ; :returns
                 (returns-type (cadddr expr))   ; long (must be :returns long)
                 (instrs (car (cddddr expr))))  ; the SIL instruction list
            (unless (eq returns-kw :returns)
              (error "%inline-cs-spliced: missing :returns keyword"))
            (unless (or (eq returns-type 'long) (string= (symbol-name returns-type) "LONG"))
              (error "%inline-cs-spliced: only :returns long supported (got ~S)" returns-type))
            (let* ((arg-locals (loop for i from 0 below (length args)
                                     collect (gen-local (format nil "INLINE_~A" i))))
                   (prelude '()))
              ;; Prelude: compile each arg, unbox to int64, stloc to local
              (loop for a in args
                    for tk in arg-locals
                    do (setf prelude
                             (append prelude
                                     `((:declare-local ,tk "Int64")
                                       ,@(let ((*in-tail-position* nil)
                                               (*in-mv-context* nil))
                                           (compile-expr a))
                                       (:unbox-fixnum)
                                       (:stloc ,tk)))))
              (append prelude
                      (%inline-cs-translate instrs arg-locals)
                      ;; Box the final int64 to LispObject Fixnum
                      `((:call "Fixnum.Make")))))))
  (setf (gethash 'block h) (lambda (expr) (compile-block (cadr expr) (cddr expr))))
  (setf (gethash 'return-from h) (lambda (expr) (compile-return-from (cadr expr) (if (cddr expr) (caddr expr) nil))))
  (setf (gethash 'tagbody h) (lambda (expr) (compile-tagbody (cdr expr))))
  (setf (gethash 'go h) (lambda (expr) (compile-go (cadr expr))))
  (setf (gethash 'unwind-protect h) (lambda (expr) (compile-unwind-protect (cadr expr) (cddr expr))))
  (setf (gethash 'catch h) (lambda (expr) (compile-catch (cadr expr) (cddr expr))))
  (setf (gethash 'throw h) (lambda (expr) (compile-throw (cadr expr) (caddr expr))))
  (setf (gethash 'function h) (lambda (expr) (compile-function-special (cadr expr))))
  (setf (gethash 'flet h) (lambda (expr) (compile-flet (cadr expr) (cddr expr))))
  (setf (gethash 'labels h) (lambda (expr) (compile-labels (cadr expr) (cddr expr))))
  ;; macrolet and symbol-macrolet preserve top-level-ness per CLHS 3.2.3.1
  (setf (gethash 'macrolet h)
        (lambda (expr) (let ((*at-toplevel* *compile-was-toplevel*)) (compile-macrolet (cadr expr) (cddr expr)))))
  (setf (gethash 'symbol-macrolet h)
        (lambda (expr) (let ((*at-toplevel* *compile-was-toplevel*)) (compile-symbol-macrolet (cadr expr) (cddr expr)))))
  (setf (gethash 'define-symbol-macro h)
        (lambda (expr)
          (let ((name (cadr expr)) (expansion (caddr expr)))
            (setf (gethash name *global-symbol-macros*) expansion)
            (incf *global-symbol-macros-epoch*)
            (compile-expr `(%register-symbol-macro-rt ',name ',expansion)))))
  (dolist (sym '(defvar defparameter defconstant))
    (let ((sym sym))
      (setf (gethash sym h)
            (lambda (expr)
              (compile-defvar (cadr expr) (caddr expr) (not (null (cddr expr)))
                              (eq sym 'defvar) (eq sym 'defconstant) (cadddr expr))))))
  ;; (THE FIXNUM (+ A B)) and the other two-operand fixnum ops.
  ;;
  ;; The range prover cannot show that FIXNUM + FIXNUM fits int64 -- the sum
  ;; spans one bit more -- so adding two values known only to be fixnums always
  ;; fell to the generic promoting RUNTIME.ADD, boxing both operands and the
  ;; result. That is the right answer for a bare (+ A B), whose result may
  ;; legitimately be a bignum.
  ;;
  ;; THE is different from a variable declaration: it is the writer's assertion
  ;; about THIS result, so it licenses the native path the proof cannot reach.
  ;; SAFETY 0 believes it and emits the raw int64 op -- a violated assertion
  ;; wraps, which is what the standard leaves undefined. SAFETY 1 and up
  ;; computes the same op with an overflow check and signals a TYPE-ERROR when
  ;; the result the writer called a FIXNUM turns out not to be one, so the
  ;; assertion is never silently wrong.
  ;;
  ;; Declines when the range proof already succeeds: that case has its own
  ;; native lowering through COMPILE-ADD, and taking it over here would change
  ;; code generation for expressions this is not about.
  (setf (gethash 'the h)
        (lambda (expr)
          (let ((form (caddr expr)))
            (cond
              ((and (fixnum-typed-p expr)
                    (checked-long-arith-p form)
                    (not (fixnum-arith-unboxed-safe-p form)))
               `(,@(if (compiling-at-safety-0-p)
                       (compile-as-long form)
                       (compile-checked-long-arith form))
                 (:call "Fixnum.Make")))
              ;; (the double-float (sqrt x)) over a double argument. The
              ;; declaration is what makes the result's type known -- SQRT of a
              ;; negative double is a COMPLEX -- so this is the one place that
              ;; can lower the call at all. Without it the call was generic and
              ;; its argument had to be boxed to make it, which is what a
              ;; declared double computation pays for a square root.
              ((and (double-float-typed-p expr) (double-sqrt-form-p form))
               `(,@(compile-as-double form) (:newobj "DoubleFloat")))
              (t (let ((*mv-expect-args* (%mv-expect-for expr form)))
                   (compile-expr form)))))))
  (setf (gethash 'load-time-value h)
        (lambda (expr)
          (let ((ltv-id (%next-ltv-id))
                (mod-id *current-module-id*))
            (cond
              ;; (LOAD-TIME-VALUE C) of a constant variable C: evaluating C at
              ;; load time or here gives the same object, so read C itself and
              ;; skip the slot. ironclad's INLINE STREEBOG-AX holds its table this
              ;; way, and every one of its 640 expansions in STREEBOG-G read the
              ;; slot through a full call.
              ((%ltv-constant-symbol-p (cadr expr))
               (compile-expr (cadr expr)))
              ;; COMPILE and a compiling EVAL run the form now and treat the
              ;; result as a literal.
              (*ltv-eval-now*
               (compile-expr `(quote ,(eval (cadr expr)))))
              ;; In a fasl, CLHS LOAD-TIME-VALUE: the form is evaluated when the
              ;; file is loaded, every time it is loaded. COMPILE-TOPLEVEL runs
              ;; the evaluation ahead of the top level form, and the code here
              ;; reads the slot once and keeps it in a field of the loaded fasl.
              ((and mod-id *ltv-hoisted*)
               (push `(%set-ltv-slot-in ,mod-id ,ltv-id ,(cadr expr)) (car *ltv-hoisted*))
               `((:load-const-cached ,(compile-expr `(%get-ltv-slot-in ,mod-id ,ltv-id)))))
              (mod-id
                ;; Per-module namespaced LTV: prevents cross-run slot ID collisions
                (compile-expr `(if (%has-ltv-slot-in ,mod-id ,ltv-id)
                                   (%get-ltv-slot-in ,mod-id ,ltv-id)
                                   (%set-ltv-slot-in ,mod-id ,ltv-id ,(cadr expr)))))
              (t
                ;; Legacy path (eval, compile without compile-file context)
                (compile-expr `(if (%has-ltv-slot ,ltv-id)
                                   (%get-ltv-slot ,ltv-id)
                                   (%set-ltv-slot ,ltv-id ,(cadr expr)))))))))
  (setf (gethash 'declare h) (lambda (expr) (declare (ignore expr)) (emit-nil)))
  (setf (gethash 'declaim h) #'compile-declaim-form)
  ;; eval-when preserves top-level-ness per CLHS 3.2.3.1
  (setf (gethash 'eval-when h)
        (lambda (expr) (let ((*at-toplevel* *compile-was-toplevel*)) (compile-eval-when (cadr expr) (cddr expr)))))
  (setf (gethash 'multiple-value-call h)
        (lambda (expr)
          (let ((bind (%mv-call-lambda-bind expr)))
            (cond
              ;; The portable MULTIPLE-VALUE-BIND shape: bind straight from the
              ;; values, as the MULTIPLE-VALUE-BIND macro does.
              (bind (compile-expr bind))
              ;; One values form: call the function on the bind snapshot.
              ((and (cddr expr) (null (cdddr expr)))
               (let ((mode (%call-site-mv-mode (cdr expr)))
                     (fn-key (gen-local "MVCF")))
                 `(,@(let ((*in-tail-position* nil)) (compile-for-single-value (cadr expr)))
                   (:declare-local ,fn-key "LispObject")
                   (:stloc ,fn-key)
                   (:call "MultipleValues.Reset")
                   ,@(%compile-for-capture (caddr expr))
                   (:call "MultipleValues.CaptureForBind")
                   (:pop)
                   (:ldloc ,fn-key)
                   ,@(or mode '((:ldc-i4 0)))
                   (:call "MultipleValues.CallBound"))))
              (t
                (let* ((fn-form (cadr expr)) (arg-forms (cddr expr)) (fn-var (gensym "MVC-FN")))
                  (compile-expr
                   `(let ((,fn-var ,fn-form))
                      (apply ,fn-var
                             (append ,@(mapcar (lambda (a) `(multiple-value-list ,a)) arg-forms)))))))))))
  (setf (gethash 'multiple-value-prog1 h)
        (lambda (expr) (compile-multiple-value-prog1 (cadr expr) (cddr expr))))

  nil) ; end compile-form handler registration
