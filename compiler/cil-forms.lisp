;;; cil-forms.lisp: Special forms, functions, control flow
;;; Part of the CIL compiler (A2 instruction list architecture)

(in-package :dotcl.cil-compiler)

;;; Per-compilation dynamic state (TCO state, typed-local tracking, etc.) is
;;; declared in cil-compiler.lisp: see the
;;; "--- Per-compilation dynamic state ---" section there.

(defun %local-var-marker (key var &optional rep)
  "Debug annotations for the binding of source variable VAR in slot KEY, to be
   placed right after the slot is stored:
   - under *EMIT-SOURCE-LINES*, (:local-var KEY sourcename) names the slot for the
     PDB, so an out-of-process debugger's Locals window shows the source name;
   - under *EMIT-FRAME-LOCALS*, (:frame-set sourcename KEY) stores the value into
     the body's runtime DebugFrame, so the in-process debugger can read it by name.
   Both are NIL by default. Skipped for uninterned symbols (gensyms introduced by
   macroexpansion, e.g. dotimes's limit) so only user-written variables show up.
   REP describes the slot's representation: NIL for a plain LispObject slot,
   otherwise :BOX (a boxed cell) or :LONG/:DOUBLE/:SINGLE/:DECIMAL/:CHAR (a
   native-rep slot)."
  (when (and (symbolp var) (symbol-package var))
    (append
     (when *emit-source-lines*
       `((:local-var ,key ,(var-name var))))
     (%frame-set-instrs key var rep))))

(defun %frame-set-instrs (key var &optional rep)
  "Under *EMIT-FRAME-LOCALS*, the store recording slot KEY's current value in the
   body's DebugFrame under source variable VAR's name; otherwise NIL. Emitted right
   after every write to the slot, the binding (via %LOCAL-VAR-MARKER) and each
   later assignment, because the frame holds values, not slots: without the
   assignment stores a mutated variable would keep showing what it was bound to.
   Same REP restriction and uninterned-symbol skip as %LOCAL-VAR-MARKER."
  (when (and *emit-frame-locals*
             (symbolp var) (symbol-package var))
    ;; A box slot records the CELL, not its contents: every closure that shares the
    ;; variable mutates that one cell, so the frame stays current with no store of
    ;; its own at each mutation site. The read side dereferences it. A native-rep
    ;; slot holds a raw long/double/single, which the assembler boxes into the
    ;; corresponding Lisp object on the way into the frame.
    `((,(ecase rep
         ((nil) :frame-set)
         (:box :frame-set-box)
         (:long :frame-set-long)
         (:double :frame-set-double)
         (:single :frame-set-single)
         (:decimal :frame-set-decimal)
         (:char :frame-set-char))
       ,(var-name var) ,key))))

(defun %frame-set-instrs-for-key (key &optional rep)
  "Like %FRAME-SET-INSTRS but for a writer that knows only the slot: the owning
   variable is recovered from *LOCALS*. NIL when no user variable owns KEY (a
   compiler temp, or a slot from another body's scope)."
  (when *emit-frame-locals*
    (let ((entry (rassoc key (cstate-locals) :test #'equal)))
      (when entry (%frame-set-instrs key (car entry) rep)))))

(defun %frame-enter-instrs (&optional fn-name)
  "Under *EMIT-FRAME-LOCALS*, the body-prologue instruction opening this body's
   runtime DebugFrame, which the (:frame-set ...) stores then write into. It must
   dominate every store in the method; hence the head of the body, before the TCO
   loop label (one frame per invocation; tail iterations overwrite its variables)
   and outside any branch. A body with no (:frame-enter) simply records no locals:
   the assembler treats (:frame-set ...) as a no-op there.
   FN-NAME is the body's runtime function name, passed to the runtime so it can
   tell whether this invocation pushed a call-stack frame of its own. An
   empty/absent one means the body runs anonymously (lambda / closure), which never
   does: it shares the caller's depth and must not evict the caller's own locals."
  (when *emit-frame-locals*
    `((:frame-enter ,(if (and fn-name (string/= fn-name "")) fn-name nil)))))

(defun emit-box-create (key value-instrs var)
  "Emit IL creating a boxed-variable cell in local slot KEY, initialized to the
   value pushed by VALUE-INSTRS, and naming it for the PDB. Boxed vars (mutated
   AND captured) live in a heap cell so a closure and its enclosing frame share
   the mutation. Normally the cell is a LispObject[1]; under debug info emission
   it is a LispBox class instead, whose Value field the VS Locals window shows
   directly (a LispObject[1] would display as a one-element array). Paired with
   compile-var-ref's box read and compile-setq's box write, which branch on the
   same *emit-source-lines* flag."
  (if *emit-source-lines*
      `((:declare-local ,key "LispBox")
        ,@value-instrs
        (:newobj "LispBox") (:stloc ,key)
        ,@(%local-var-marker key var :box))
      (if (%instrs-need-empty-stack-p value-instrs)
          ;; VALUE-INSTRS leave or enter a protected region (a TAGBODY's
          ;; dispatch loop, a BLOCK with a non-local exit, ...): LEAVE empties
          ;; the evaluation stack, so the array and index cannot sit under it.
          ;; Compute the value into a temporary first.
          (let ((tmp (gen-local "BOXINIT")))
            `((:declare-local ,key "LispObject[]")
              (:declare-local ,tmp "LispObject")
              ,@value-instrs (:stloc ,tmp)
              (:ldc-i4 1) (:newarr "LispObject") (:dup)
              (:ldc-i4 0) (:ldloc ,tmp)
              (:stelem-ref) (:stloc ,key)
              ,@(%local-var-marker key var :box)))
          `((:declare-local ,key "LispObject[]")
            (:ldc-i4 1) (:newarr "LispObject") (:dup)
            (:ldc-i4 0) ,@value-instrs
            (:stelem-ref) (:stloc ,key)
            ,@(%local-var-marker key var :box)))))

(defun %instrs-need-empty-stack-p (instrs)
  "True if INSTRS contain a LEAVE or open a protected region, either of which
   requires the evaluation stack to hold nothing of the caller's."
  (some (lambda (ins)
          (and (consp ins)
               (member (car ins) '(:leave :begin-exception-block))))
        instrs))

(defun %synthetic-capture-name-p (name)
  "True if NAME is a compiler-synthesized captured slot, a labels/flet function
   cell (__LABELFN_), a block tag (%BTAG-), or a tagbody id (%TBID-), rather
   than a user variable. Such slots are excluded from the debugger's Locals; only
   real captured variables are named there."
  (flet ((pfx (p) (let ((n (length p)))
                    (and (>= (length name) n) (string= name p :end1 n)))))
    (or (labels-cell-var-p name)
        (pfx "%BTAG-")
        (pfx "%TBID-"))))

(defvar *tail-prefix-max-instrs* 1000
  "Largest body, in SIL instructions, whose calls in tail position are made as
   tail calls. The CLR JIT compiles a method that contains an explicit tail call
   with full optimization on its first call instead of at tier 0, and full
   optimization costs several times as much JIT time per IL byte. For a small
   function that is cheap and the call stays a real tail call. For a large one it
   is paid once, up front, whether or not the function recurses: a test suite or
   a top-level lambda of a few thousand instructions spends most of its first
   call in the JIT. Past this size every call is an ordinary call.")

(defun tail-calls-off-p (body)
  "T when the function compiled from BODY makes no tail calls: (debug 3), from
   its own declaration or else from DECLAIM, as SBCL does, so every caller stays
   in the backtrace."
  (let ((declared (body-declared-debug body)))
    (eql (or declared *optimize-debug*) 3)))

(defun %tail-self-instrs (fn-name body body-instrs &optional anonymous)
  "The (:tail-self NAME) marker that lets the assembler make the calls in tail
   position of this function body tail calls, or NIL for none. The assembler
   finds those calls itself (a call whose value reaches the method's RET through
   nothing but branches and a local it is stored to and loaded back from, outside
   any protected region), so a call inside a special binding, UNWIND-PROTECT,
   HANDLER-CASE or the argument of THROW is never one, and neither is one whose
   value LET binds (that keeps only the first value). NAME is the name of the debugger frame the body runs under when
   called normally (FN-NAME), or NIL for a body that records none of its own: a
   closure (ANONYMOUS), a lambda, or a (debug 0) function. See the tail-call
   section of Function.cs for what the name is used for."
  (unless (or (tail-calls-off-p body)
              (> (length body-instrs) *tail-prefix-max-instrs*))
    `((:tail-self ,(if (or anonymous
                           (null fn-name) (equal fn-name "")
                           (debug-frames-off-p body))
                       nil
                       fn-name)))))

(defun %conditional-arms (form)
  "(test then [else]) for FORM when it is an IF / WHEN / UNLESS, else NIL.
   WHEN and UNLESS are lowered here the same way their handlers lower them."
  (case (car form)
    (if (when (<= 3 (length form) 4) (cdr form)))
    (when (when (>= (length form) 2)
            (list (cadr form) (cons 'progn (cddr form)))))
    (unless (when (>= (length form) 2)
              (list (cadr form) nil (cons 'progn (cddr form)))))))

(defun compile-and-pop (form)
  "Compile FORM for effect. Uses void call variants when available to avoid allocation.
   Returns CIL instructions without (:pop) if a void variant was used."
  (let ((prefix (%form-prefix-code form)))
    (when prefix
      (return-from compile-and-pop
        (let ((*form-prefixed* form)) (append prefix (compile-and-pop form))))))
  (let ((op (and (consp form) (symbolp (car form)) (car form))))
    (cond
      ;; Direct vector-push-extend in for-effect position: use void variant (no Fixnum.Make)
      ((and (eq op 'vector-push-extend) (= (length (cdr form)) 2))
       (compile-binary-call (cdr form) "Runtime.VectorPushExtendVoid2"))
      ;; (FUNCALL #'(SETF F) ...) storing a word of adjacent octets, its value
      ;; discarded: the wide store only.
      ((and (eq op 'funcall) (not *at-toplevel*) (octet-word-store-info form))
       (compile-octet-word-store (octet-word-store-info form) nil))
      ;; A conditional whose value is discarded compiles its arms for effect too.
      ;; Otherwise both arms must leave a value at the branch merge, and an arm
      ;; that assigns to a native-slot variable boxes it to do so -- for a value
      ;; nobody reads. (SETQ inside a WHEN in a loop body is the shape where this
      ;; costs the most: one box an iteration.)
      ((and op (member op '(if when unless))
            (not (macrolet-shadowed-p op))
            (not (local-function-entry op))
            (%conditional-arms form))
       (compile-if (%conditional-arms form) t))
      ;; Every form of a discarded PROGN is discarded, the last one included.
      ;; Without this the arms above stop here, since WHEN's body arrives wrapped
      ;; in one.
      ((and (eq op 'progn)
            (not (macrolet-shadowed-p op))
            (not (local-function-entry op)))
       (loop for sub in (cdr form) append (compile-and-pop sub)))
      ((and (eq op '%aref-set) (%aref-copy-plan form))
       (compile-aref-copy (%aref-copy-plan form)))
      ((and (eq op '%aref-set) (%aref-set-long-plan form))
       (compile-aref-set-long (%aref-set-long-plan form)))
      ;; A SETF whose value is discarded: its expansion is discarded, so the
      ;; LET* and the substituted INLINE (SETF F) body it is made of are too.
      ;; The new value the body returns would otherwise be boxed for nobody.
      ((and (eq op 'setf)
            (not *at-toplevel*)
            (not (local-function-entry op))
            (find-macro-expander op))
       (let ((expanded (%macroexpand-for-compile form (find-macro-expander op))))
         (if (%discard-transparent-form-p expanded)
             `(,@(%source-line-marker form) ,@(compile-and-pop expanded))
             `(,@(compile-expr form) (:pop)))))
      ;; A LET / LET* whose value is discarded discards its last form too.
      ;; Not with a SPECIAL binding: that body runs inside a try/finally whose
      ;; shape expects the value.
      ((and (member op '(let let*))
            (not *at-toplevel*)
            (consp (cdr form)) (listp (cadr form))
            (%boolean-branchable-let-p (cadr form) (cddr form)))
       (compile-let (cadr form) (cddr form) (eq op 'let*)
                    #'%compile-discarded-body))
      ;; A BLOCK nothing visibly returns from, its value discarded: the body is
      ;; compiled as statements without the block's result slot, the same
      ;; transparent treatment a test position gives it.
      ((and (eq op 'block)
            (not *at-toplevel*)
            (consp (cdr form)) (symbolp (cadr form)) (cddr form))
       (%compile-discarded-block form))
      ;; A call substituted by an INLINE proclamation, its value discarded: the
      ;; expansion (a LET around the BLOCK) is discarded as a whole.
      ((and (not *at-toplevel*) (%discard-inline-expansion form))
       (let ((*at-toplevel* nil))
         (compile-and-pop (%discard-inline-expansion form))))
      (t `(,@(compile-expr form) (:pop))))))

(defun %source-line-marker (form)
  "The source-span marker COMPILE-EXPR puts in front of FORM's code, as a list
   of zero or one instruction."
  (when (and *emit-source-lines* (consp form))
    (let ((span (funcall *emit-source-lines* form)))
      (when span (list (cons :line span))))))

(defun %discard-inline-expansion (form)
  "The INLINE expansion of FORM when FORM is a call the compiler would
   substitute: (F args...) for a function F, or (FUNCALL #'(SETF F) args...).
   NIL otherwise, including whenever something else would claim the call
   first (a handler, a macro, a compiler macro, a local function)."
  (when (consp form)
    (let ((op (car form)))
      (cond
        ((and (eq op 'funcall)
              (not (local-function-entry op))
              (consp (cdr form))
              (let ((fe (cadr form)))
                (and (consp fe) (eq (car fe) 'function)
                     (consp (cdr fe)) (null (cddr fe))
                     (%setf-function-name-p (cadr fe)))))
         (let ((name (cadr (cadr form))))
           (maybe-expand-inline name (cons name (cddr form)))))
        ((and (symbolp op) op
              (not (special-operator-name-p op))
              (not (gethash op *compile-form-handlers*))
              (not (local-function-entry op))
              (not (macrolet-shadowed-p op))
              (not (find-macro-expander op))
              (gethash op *inline-defs*)
              (not (and *labels-demoted* (assoc op *labels-demoted* :test #'eq)))
              (not (maybe-expand-compiler-macro op form)))
         (maybe-expand-inline op form))))))

(defun %discard-transparent-form-p (form)
  "True when COMPILE-AND-POP compiles FORM differently from its value: a form
   whose value it never builds."
  (and (consp form)
       (or (member (car form) '(let let* block progn))
           (and (%discard-inline-expansion form) t)
           (and (eq (car form) '%aref-set)
                (or (%aref-copy-plan form) (%aref-set-long-plan form))
                t))))

(defun %aref-copy-plan (form)
  "For FORM = (%AREF-SET DST D... (AREF SRC S...)), its value discarded, with
   rank 1 or 2 on each side, every subscript lowered to an int64, and the
   element storage of one side not known here (the other's unknown or integer;
   a known integer SRC only when read without a hoisted buffer):
   (DST D-SUBSCRIPTS SRC S-SUBSCRIPTS).
   Such a store would box the element only to unbox it again."
  (let* ((args (cdr form))
         (val (car (last args)))
         (didx (butlast (cdr args))))
    (and (consp val) (eq (car val) 'aref)
         (not (local-function-entry 'aref))
         (not (macrolet-shadowed-p 'aref))
         (not (local-function-entry '%aref-set))
         (<= 1 (length didx) 2)
         (<= 1 (length (cddr val)) 2)
         (every #'fixnum-long-lowerable-p didx)
         (every #'fixnum-long-lowerable-p (cddr val))
         (let ((dst-known (numeric-array-aref-entry `(aref ,(car args) ,@didx)))
               (src-known (numeric-array-aref-entry val)))
           (if dst-known
               (and (not src-known) (numeric-array-aref-info `(aref ,(car args) ,@didx)))
               (or (not src-known)
                   (and (numeric-array-aref-info val)
                        (not (array-backing-entry-for-read val))))))
         (list (car args) didx (cadr val) (cddr val)))))

(defun compile-aref-copy (plan)
  "The element move of an %AREF-COPY-PLAN, as a void Runtime.ArefCopy<D><S>L
   call. Arrays and subscripts are evaluated in source order."
  (destructuring-bind (dst didx src sidx) plan
    (let* ((method (format nil "Runtime.ArefCopy~D~DL" (length didx) (length sidx)))
           (parts (append (list (cons dst nil))
                          (mapcar (lambda (i) (cons i t)) didx)
                          (list (cons src nil))
                          (mapcar (lambda (i) (cons i t)) sidx))))
      (flet ((emit (part)
               (if (cdr part)
                   (compile-expr-to-long (car part))
                   (let ((*in-tail-position* nil) (*in-mv-context* nil))
                     (compile-expr (car part))))))
        (if (every (lambda (p) (simple-expr-p (car p))) parts)
            `(,@(loop for p in parts append (emit p))
              (:call ,method))
            (let ((tmps (mapcar (lambda (p) (gen-local (if (cdr p) "ACI" "ACA"))) parts)))
              `(,@(loop for p in parts for tk in tmps
                        collect `(:declare-local ,tk ,(if (cdr p) "Int64" "LispObject")))
                ,@(loop for p in parts for tk in tmps
                        append `(,@(emit p) (:stloc ,tk)))
                ,@(mapcar (lambda (tk) `(:ldloc ,tk)) tmps)
                (:call ,method))))))))

(defun %aref-set-long-plan (form)
  "For FORM = (%AREF-SET DST D... X), its value discarded, DST of rank 1 or 2
   with its element storage not known here, every subscript lowered to an
   int64, and X an element of an array whose integer storage is known here:
   (DST D-SUBSCRIPTS X). X is stored without being boxed when DST can hold it."
  (let* ((args (cdr form))
         (val (car (last args)))
         (didx (butlast (cdr args))))
    (and (not (local-function-entry '%aref-set))
         (<= 1 (length didx) 2)
         (every #'fixnum-long-lowerable-p didx)
         (not (numeric-array-aref-entry `(aref ,(car args) ,@didx)))
         (numeric-array-aref-info val)
         (list (car args) didx val))))

(defun compile-aref-set-long (plan)
  "The store of an %AREF-SET-LONG-PLAN, as a void Runtime.ArefSetLong<D>L call.
   The array, the subscripts and the value are evaluated in source order."
  (destructuring-bind (dst didx val) plan
    (let ((a-tmp (gen-local "ASA"))
          (i-tmps (mapcar (lambda (i) (declare (ignore i)) (gen-local "ASI")) didx))
          (v-tmp (gen-local "ASV")))
      `((:declare-local ,a-tmp "LispObject")
        ,@(mapcar (lambda (tk) `(:declare-local ,tk "Int64")) i-tmps)
        (:declare-local ,v-tmp "Int64")
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
            (compile-expr dst))
        (:stloc ,a-tmp)
        ,@(loop for i in didx for tk in i-tmps
                append `(,@(compile-expr-to-long i) (:stloc ,tk)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
            (compile-as-long val))
        (:stloc ,v-tmp)
        (:ldloc ,a-tmp)
        ,@(mapcar (lambda (tk) `(:ldloc ,tk)) i-tmps)
        (:ldloc ,v-tmp)
        (:call ,(format nil "Runtime.ArefSetLong~DL" (length didx)))))))

;;; Statements and the multiple-values channel.
;;;
;;; A body's forms before the last are compiled for effect, and the channel
;;; (MultipleValues) was reset after each of them, so that values one of them
;;; published cannot be read by the next one or by whoever reads the body's
;;; values. A statement that cannot touch the channel at all (raw arithmetic,
;;; an array or slot store) needs no reset after it: the channel is then what
;;; it was before the statement, and dropping such statements from the body
;;; would leave the same channel for the forms after them. The reset is placed
;;; once, before the first form after a statement that can touch the channel.

(defun %mv-neutral-call-p (method)
  "True for a method a statement may call and still leave the multiple-values
   channel untouched: none of these runs Lisp code that can return normally,
   and none publishes values. MultipleValues.Reset only empties the channel."
  (member method
          '("Fixnum.Make" "MultipleValues.Reset" "MultipleValues.ResetOnExit"
            "Runtime.ArefNumL" "Runtime.ArefSetNumL"
            "Runtime.BoxU64" "Runtime.ShlU64" "Runtime.ShrU64"
            "Runtime.RotateLeft32" "Runtime.RotateLeft64"
            "Runtime.BackingU64" "Runtime.ArefU64Bits" "Runtime.ArefSetU64Bits"
            "Runtime.CheckStoreU8" "Runtime.CheckStoreU16" "Runtime.CheckStoreI32"
            "Runtime.CheckStoreUnsigned" "Runtime.CheckStoreSigned"
            "Runtime.StructRefL" "Runtime.StructSetL" "Runtime.StructOf"
            "Runtime.ReadU16LE" "Runtime.ReadU16BE" "Runtime.ReadU32LE"
            "Runtime.ReadU32BE" "Runtime.ReadU64LE" "Runtime.ReadU64BE"
            "Runtime.WriteU16LE" "Runtime.WriteU16BE" "Runtime.WriteU32LE"
            "Runtime.WriteU32BE" "Runtime.WriteU64LE" "Runtime.WriteU64BE")
          :test #'equal))

(defun %mv-neutral-op-p (op)
  (or (member op '(:declare-local :label :pop :dup :nop
                   :add :sub :mul :div :rem :div-un :rem-un :neg :not
                   :and :or :xor :shl :shr :shr-un
                   :ceq :clt :cgt :clt-un :cgt-un
                   :leave :go-local :unbox-fixnum :load-const :ldnull))
      (let ((name (symbol-name op)))
        (or (and (>= (length name) 2)
                 (member (subseq name 0 2) '("LD" "ST") :test #'string=)
                 (not (member op '(:ldftn :ldvirtftn))))
            (and (>= (length name) 4) (string= (subseq name 0 4) "CONV"))
            (member op '(:br :brtrue :brfalse :beq :bne-un :blt :ble :bgt :bge
                         :blt-un :ble-un :bgt-un :bge-un))))))

(defun %mv-neutral-instrs-p (instrs)
  "True when INSTRS, a statement compiled for effect, can neither publish
   values to the multiple-values channel nor read it. Runtime.UnwrapMv
   publishes only when handed an MvReturn, so it is allowed right after an
   instruction whose result is never one: a fixnum just boxed, a static field,
   or another unwrap."
  (let ((prev nil))
    (dolist (ins instrs t)
      (unless (and (consp ins)
                   (let ((op (car ins)))
                     (cond ((not (eq op :call)) (%mv-neutral-op-p op))
                           ((equal (second ins) "Runtime.UnwrapMv")
                            (and (consp prev)
                                 (or (eq (car prev) :ldsfld)
                                     (and (eq (car prev) :call)
                                          (member (second prev)
                                                  '("Fixnum.Make" "Runtime.BoxU64" "Runtime.UnwrapMv")
                                                  :test #'equal)))))
                           (t (%mv-neutral-call-p (second ins))))))
        (return nil))
      (setq prev ins))))

(defun %join-statements (compiled)
  "The instructions of statements, each compiled for effect, in order, with
   MultipleValues.Reset where the channel has to be emptied: before every
   statement that can touch the channel and before the form that follows them
   all, whenever a statement since the last reset may have published values.
   COMPILED is a list of (INSTRS . NEUTRAL-P); NEUTRAL-P :DEFAULT asks
   %MV-NEUTRAL-INSTRS-P."
  (let ((neutral (mapcar (lambda (entry)
                           (if (eq (cdr entry) :default)
                               (%mv-neutral-instrs-p (car entry))
                               (cdr entry)))
                         compiled))
        (dirty nil))
    (loop for (entry . rest) on compiled
          for (n . next) on neutral
          append (car entry)
          do (unless n (setq dirty t))
          when (and dirty (or (null rest) (not (car next))))
            append (progn (setq dirty nil)
                          (list (list :call "MultipleValues.Reset"))))))

(defun %compile-statements (forms)
  "FORMS compiled for effect and joined by %JOIN-STATEMENTS, for a body whose
   last form follows them."
  (%join-statements (mapcar (lambda (f) (cons (compile-and-pop f) :default)) forms)))

;;; ============================================================
;;; Named function call (user-defined)
;;; ============================================================

(defun compile-direct-call-args (args)
  "Pre-evaluate args to individual temp locals (not array).
   Returns (temps . eval-instrs) where temps is list of local names.
   Args are never in tail position and receive a single value each."
  (let ((temps (loop for a in args collect (gen-local "DA"))))
    (cons temps
          (loop for arg in args
                for tmp in temps
                append `((:declare-local ,tmp "LispObject")
                         ,@(let ((*in-tail-position* nil)
                                 (*in-mv-context* nil))
                             (compile-expr arg)) (:stloc ,tmp))))))

(defun compile-direct-call-args-long (args)
  "Like compile-direct-call-args but evaluates each arg via compile-as-long -> Int64.
   Used in native function bodies for TCO self-call argument evaluation."
  (let ((temps (loop for a in args collect (gen-local "DA"))))
    (cons temps
          (loop for arg in args
                for tmp in temps
                append `((:declare-local ,tmp "Int64")
                         ,@(let ((*in-tail-position* nil)
                                 (*in-mv-context* nil))
                             (compile-as-long arg))
                         (:stloc ,tmp))))))

(defun %struct-ctor-key-args (keywords args)
  "ARGS parsed as a keyword argument list for KEYWORDS: an alist of
   (KEYWORD . VALUE-FORM) in the order written, or :DECLINE when the call is not
   statically resolvable; an odd or dotted argument list, a key that is not a
   literal keyword or not a slot of this structure, or a key given twice (CLHS
   says the leftmost wins, but every value form is still evaluated, so a rewrite
   would have to keep the discarded ones)."
  (let ((rest args) (supplied '()) (ok t))
    (loop while (and ok rest)
          do (let ((k (car rest)))
               (if (and (consp (cdr rest))
                        (keywordp k)
                        (member k keywords)
                        (not (assoc k supplied)))
                   (progn (push (cons k (cadr rest)) supplied)
                          (setf rest (cddr rest)))
                   (setf ok nil))))
    (if ok (nreverse supplied) :decline)))

(defun %struct-ctor-direct-form (entry args)
  "The form a keyword-constructor call rewrites to, or NIL to leave the call alone.
   ENTRY is a *STRUCT-KEYWORD-CTORS* value: (STRUCT-NAME KEYWORDS INITFORMS).

   An omitted slot contributes its initform, so it is only usable when that form
   is constant: the constructor evaluates initforms in its own scope, and moving a
   non-constant one to the call site would evaluate it in the caller's instead.

   Argument forms are evaluated left to right as written (CLHS 3.1.2.1.2.3). When
   the keywords are written in slot order the rewrite preserves that by itself;
   otherwise the values are bound to temporaries in the order written first."
  (let* ((struct-name (first entry))
         (keywords (second entry))
         (initforms (third entry))
         (supplied (%struct-ctor-key-args keywords args)))
    (unless (eq supplied :decline)
      (let ((written (mapcar #'car supplied))
            (usable t))
        (loop for k in keywords
              for init in initforms
              do (unless (or (assoc k supplied) (constantp init))
                   (setf usable nil)))
        (when usable
          (let* ((slots (fourth entry))
                 ;; The constructor's own DEFUN checks every initial value
                 ;; against the slot's declared :TYPE. Rewriting the call has to
                 ;; carry those checks along, or the SAME call means one thing
                 ;; interpreted (which calls the DEFUN) and another compiled.
                 ;;
                 ;; Not emitted under (safety 0): the check would compile away
                 ;; there anyway, and asking here as well keeps the fast shape
                 ;; -- and the temporaries it does not need -- byte for byte
                 ;; what it was.
                 (checked (and slots
                               (not (compiling-at-safety-0-p))
                               (some (lambda (s) (getf (cddr s) :type)) slots)))
                 (reordered (not (equal written
                                        (remove-if-not (lambda (k) (member k written))
                                                       keywords))))
                 ;; A checked value is read twice (once by the check, once by
                 ;; the store), so it goes through a temporary whatever the
                 ;; keyword order was. The bindings are in the order the
                 ;; arguments were written, which is the order they must be
                 ;; evaluated in; the checks then run in slot order, as they do
                 ;; inside the DEFUN.
                 (temps (when (or reordered checked)
                          (loop for s in supplied
                                collect (cons (car s) (gensym "SV")))))
                 (values (loop for k in keywords
                               for init in initforms
                               collect (let ((cell (assoc k supplied)))
                                         (cond ((null cell) init)
                                               (temps (cdr (assoc k temps)))
                                               (t (cdr cell))))))
                 (call `(%make-struct ',struct-name ,@values))
                 (body (if checked
                           `(progn
                              ,@(remove nil
                                        (loop for s in slots
                                              for v in values
                                              collect (%slot-check-stmt v s struct-name)))
                              ,call)
                           call)))
            (if temps
                `(let* ,(loop for s in supplied
                              collect (list (cdr (assoc (car s) temps)) (cdr s)))
                   ,body)
                body)))))))

;; Value modes a call can pass (MultipleValues.ModePrimary / ModeCapture).
(defparameter +mv-mode-primary+ 1)
(defparameter +mv-mode-capture+ 2)

(defun %call-site-mv-mode (args &optional name self-call-p)
  "Instructions that push the value mode for a call whose argument forms are
   ARGS, or NIL for a plain call. The result is consumed at once when the call
   is the form of *MV-EXPECT-ARGS*: as its primary value (for a global callee
   only when it is known to take a mode, see *MV-TAIL-FUNCTIONS*) or captured
   into the thread state. A call in the tail of a body that took a mode passes that mode on,
   since its values are that body's own."
  (let ((ex *mv-expect-args*))
    (cond ((not *mv-mode-elision*) nil)
          ((and ex (eq (car ex) args))
           ;; This call's result is consumed right here; it is not the body's
           ;; return value even where *IN-TAIL-POSITION* is still set (an
           ;; argument compiled by COMPILE-FOR-SINGLE-VALUE).
           (case (cdr ex)
             (:primary
              ;; A body's call to itself: whether it takes a mode is not known
              ;; until it is compiled, and the callee decides anyway.
              (when (or self-call-p
                        ;; A local function or a FUNCALL: no name to look up,
                        ;; and the callee decides anyway.
                        (null name)
                        (%mv-tail-function-p name))
                `((:ldc-i4 ,+mv-mode-primary+))))
             (:capture
              (when *in-mv-context*
                `((:ldc-i4 ,+mv-mode-capture+))))))
          ((and *in-tail-position* (%mv-mode-local))
           (%mv-mode-push))
          (t nil))))

(defun %compile-for-capture (form)
  "FORM compiled for a consumer that reads its values out of the thread state
   the moment it returns (CAPTUREFORBIND, NTH-VALUE). MV
   context keeps the values; no tail position, so a self-call in FORM is not
   turned into a loop. With value modes on, FORM's own tail answers the capture
   mode: a (VALUES ...) there, or a call there, puts the values where the
   consumer reads them without an MvReturn, as at the tail of a body whose
   caller asked for a capture."
  (let ((*in-mv-context* t)
        ;; A named call written directly here is consumed the moment it returns.
        (*mv-expect-args* (if (consp form) (cons (cdr form) :capture) nil)))
    (if *mv-mode-elision*
        (let ((*in-tail-position* :value)
              (*mv-mode-local* +mv-mode-capture+))
          (compile-expr form))
        (let ((*in-tail-position* nil))
          (compile-expr form)))))

(defun %gf-ic-call-p (name n-args)
  "Whether a call of the global function NAME with N-ARGS arguments goes through a
   call site cache: a one-argument call, not in tail position, of a name
   DEFGENERIC / DEFMETHOD has named."
  (and (= n-args 1)
       (symbolp name)
       (not *cross-compiling*)
       (not (eq *in-tail-position* t))
       (symbol-package name)
       (not (eq (symbol-package name) (find-package "COMMON-LISP")))
       (gethash name *gf-call-names*)))

(defun %invoke-instrs (n-args mode-instrs)
  "The call of a LispFunction whose N-ARGS arguments are on the stack: InvokeN,
   or InvokeNM with the value mode MODE-INSTRS push (see %CALL-SITE-MV-MODE)."
  (if (and mode-instrs (<= n-args 6))
      `(,@mode-instrs (:callvirt ,(invoke-mode-name n-args)))
      `((:callvirt ,(invoke-name n-args)))))

(defun %float-slot-kind (key)
  "Whether the slot KEY holds a raw double (:DOUBLE) or single (:SINGLE)."
  (cond ((member key (cstate-native-double-locals) :test #'equal) :double)
        ((member key (cstate-native-single-locals) :test #'equal) :single)))

(defun %compile-to-long-through-lets (expr)
  "COMPILE-EXPR-TO-LONG of the value of EXPR, looking through value wrappers and
   binding forms to their last form, for a value stored into an Int64 slot."
  (cond
    ((%locally-wrapper-p expr)
     (let ((*lexical-decl-frames* (%push-decl-frame :locally nil (cdr expr))))
       (%compile-to-long-through-lets (car (last expr)))))
    ((not (eq (strip-value-wrappers expr nil) expr))
     (%compile-to-long-through-lets (strip-value-wrappers expr nil)))
    ;; (THE <integer type> X): X as a raw int64, then checked against the type's
    ;; range as the declaration asks.
    ((and (consp expr) (eq (car expr) 'the) (consp (cdr expr)) (consp (cddr expr))
          (null (cdddr expr))
          (let ((r (declared-integer-range (cadr expr))))
            (and r (range-fits-int64-p r) (%exact-integer-type-p (cadr expr)))))
     (let ((r (declared-integer-range (cadr expr))))
       `(,@(%compile-to-long-through-lets (caddr expr))
         ,@(%long-range-check (caddr expr) (list :long (car r) (cdr r))))))
    ((and (consp expr) (member (car expr) '(let let*)) (consp (cdr expr))
          (listp (cadr expr)) (cddr expr)
          (%boolean-branchable-let-p (cadr expr) (cddr expr)))
     (compile-let (cadr expr) (cddr expr) (eq (car expr) 'let*)
                  (lambda (forms)
                    `(,@(%compile-statements (butlast forms))
                      ,@(%compile-to-long-through-lets (car (last forms)))))))
    (t (compile-expr-to-long expr))))

(defun %raw-slot-kind (key)
  "Whether the slot KEY holds a raw double, single or Int64 (:DOUBLE / :SINGLE / :LONG)."
  (or (%float-slot-kind key)
      (and (member key (cstate-long-locals) :test #'equal) :long)))

(defun %tco-raw-slot-args (args entries)
  "COMPILE-DIRECT-CALL-ARGS for a tail self-call whose parameter slots ENTRIES
   include raw ones: the argument for such a slot is computed as that raw value,
   checked to be one when it is not statically."
  (let ((temps (loop for nil in args collect (gen-local "DA"))))
    (cons temps
          (loop for a in args
                for tmp in temps
                for e in entries
                for kind = (%raw-slot-kind (car e))
                append (let ((*in-tail-position* nil) (*in-mv-context* nil))
                         (case kind
                           (:long
                            `((:declare-local ,tmp "Int64")
                              ,@(%compile-to-long-through-lets a)
                              (:stloc ,tmp)))
                           (:double
                            `((:declare-local ,tmp "Double")
                              ,@(if (double-float-typed-p a)
                                    (compile-as-double a)
                                    `(,@(compile-for-single-value a)
                                      (:call "Runtime.TypedArgD")))
                              (:stloc ,tmp)))
                           (:single
                            (if (single-float-typed-p a)
                                `((:declare-local ,tmp "Single")
                                  ,@(compile-as-single a)
                                  (:stloc ,tmp))
                                (let ((obj (gen-local "DAO")))
                                  `((:declare-local ,obj "LispObject")
                                    ,@(compile-for-single-value a)
                                    (:stloc ,obj)
                                    (:ldloc ,obj) (:call "Runtime.CheckArgSingle")
                                    (:declare-local ,tmp "Single")
                                    (:ldloc ,obj) (:unbox-single)
                                    (:stloc ,tmp)))))
                           (t
                            `((:declare-local ,tmp "LispObject")
                              ,@(compile-expr a)
                              (:stloc ,tmp)))))))))

(defun compile-named-call (name args)
  ;; xref: single choke point for every named call: all the fast-path
  ;; return-froms below (self-call, CLOS reader/writer IC, struct accessor,
  ;; local flet/labels) are branches INSIDE this function, so recording at
  ;; entry catches them all.
  (xref-record-call name)
  (block compile-named-call
    ;; Self-TCO: if in tail position and calling current function, emit loop
    ;; Use symbol identity (eq) not just name string to avoid cross-package false matches
    ;; (e.g. uiop/os:getenv calling dotcl:getenv must not be treated as self-recursion)
    ;; Skip TCO when a local function (flet/labels) shadows the defun name
    (let* ((name-str (mangle-name name))
           (n-args (length args))
           ;; (ARGS) when a tail self-call could loop with these arguments, as
           ;; the parameters receive them. A &key function's shared body takes
           ;; its keys positionally, so there the call is spread into that shape
           ;; and a call that merely has the same count does not match.
           (tco-match
             (let ((ks (cstate-tco-key-shared)))
               (if (and ks (eq (car ks) (cstate-tco-param-entries)))
                   (let ((spread (%key-shared-tco-args args (cadr ks) (cddr ks))))
                     (and spread (list spread)))
                   (and (= n-args (length (cstate-tco-param-entries)))
                        (list args))))))
      (when (and (eq *in-tail-position* t)
                 ;; try/finally (special-var LET) suppresses TCO, but handler-case's
                 ;; try/catch allows it via the tco-in-try-catch slot (`leave`, not `br`)
                 (or (cstate-tco-in-try-catch) (not *in-try-block*))
                 (cstate-tco-self-name)
                 ;; Skip TCO when a different local function shadows the name;
                 ;; allow when the shadow IS the labels fn being compiled.
                 (let ((lf (local-function-entry name-str)))
                   (or (null lf)
                       (and (cstate-tco-local-fn-key)
                            (string= (second lf) (cstate-tco-local-fn-key)))))
                 (if (cstate-tco-self-symbol)
                     (eq name (cstate-tco-self-symbol))
                     (string= name-str (cstate-tco-self-name)))
                 (self-call-shortcut-p name)
                 tco-match)
        (let* ((args (car tco-match))
               (typed-self (cstate-typed-self))
               (raw-args nil)
               (use-native-tco (and (not typed-self)
                                    (cstate-native-self-name)
                                    ;; A native self's parameters are all
                                    ;; declared FIXNUM.
                                    (every #'fixnum-wrap-typed-p args)))
               (da (cond
                     ;; Typed body: each argument as the raw value its
                     ;; parameter's slot holds, checked as the entry would.
                     (typed-self
                      (let ((temps (loop for nil in args collect (gen-local "DA"))))
                        (cons temps
                              (loop for a in args
                                    for spec in (second typed-self)
                                    for tmp in temps
                                    append `((:declare-local ,tmp ,(%typed-slot-type spec))
                                             ,@(compile-typed-value a spec)
                                             (:stloc ,tmp))))))
                     (use-native-tco (compile-direct-call-args-long args))
                     ((some (lambda (e) (%raw-slot-kind (car e)))
                            (cstate-tco-param-entries))
                      (setq raw-args t)
                      (%tco-raw-slot-args args (cstate-tco-param-entries)))
                     (t (compile-direct-call-args args))))
               (temps (car da))
               (eval-instrs (cdr da))
               (store-instrs
                 (if (or use-native-tco typed-self)
                     ;; Native body: all params are Int64, temps are Int64 -> direct store
                     (loop for tmp in temps
                           for (key . boxed-p) in (cstate-tco-param-entries)
                           for i from 0
                           append `((:ldloc ,tmp) (:stloc ,key)
                                    ,@(%frame-set-instrs-for-key
                                       key (if typed-self
                                               (case (car (nth i (second typed-self)))
                                                 (:double :double) (:object nil) (t :long))
                                               :long))))
                     ;; Normal body: LispObject temps -> LispObject or boxed params
                     (loop for tmp in temps
                           for (key . boxed-p) in (cstate-tco-param-entries)
                           if boxed-p
                             append (if *emit-source-lines*
                                        `((:ldloc ,key) (:ldloc ,tmp) (:stfld "LispBox.Value"))
                                        `((:ldloc ,key) (:ldc-i4 0) (:ldloc ,tmp) (:stelem-ref)))
                           else if (%float-slot-kind key)
                             append `((:ldloc ,tmp) (:stloc ,key)
                                      ,@(%frame-set-instrs-for-key key (%float-slot-kind key)))
                           else if (and raw-args (member key (cstate-long-locals) :test #'equal))
                             append `((:ldloc ,tmp) (:stloc ,key)
                                      ,@(%frame-set-instrs-for-key key :long))
                           else if (member key (cstate-long-locals) :test #'equal)
                             ;; A FIXNUM-declared parameter promoted to a raw
                             ;; Int64 slot: the temp is still a LispObject
                             ;; (the arguments are evaluated generically here),
                             ;; so the slot's representation is restored on the
                             ;; way in.
                             append `((:ldloc ,tmp) (:unbox-fixnum) (:stloc ,key)
                                      ,@(%frame-set-instrs-for-key key :long))
                           else
                             ;; A tail self-call rebinds the parameters in place and
                             ;; loops, staying in one frame: so the debug frame has
                             ;; to follow, or it would show the arguments of the
                             ;; first iteration forever.
                             append `((:ldloc ,tmp) (:stloc ,key)
                                      ,@(%frame-set-instrs-for-key key))))))
          (return-from compile-named-call
            `(,@eval-instrs
              ,@store-instrs
              ,@(cstate-tco-leave-instrs)
              ;; Back-edge safepoint: a TCO'd self-call never passes a checked
              ;; Invoke entry, so poll here or the loop is unstoppable.
              ,@(unless (cstate-no-safepoint)
                  '((:call "ConditionSystem.PollInterrupt")))
              ;; handler-case try/catch: use `leave` to exit cleanly.
              ;; try/finally (special-var LET) already suppressed above via *in-try-block*.
              ,(cond ((cstate-tco-in-try-catch)
                      `(:leave ,(cstate-tco-loop-label)))
                     ((and (not typed-self) (not use-native-tco)
                           (%tco-args-checked-p args))
                      `(:br ,(second (cstate-tco-checked))))
                     (t `(:br ,(cstate-tco-loop-label))))))))
      ;; Mutual-TCO: tail call to a labels sibling -> update shared params + br TCOLOOP
      (when (and (eq *in-tail-position* t)
                 (or (cstate-tco-in-try-catch) (not *in-try-block*))
                 (cstate-labels-mutual-tco))
        (let ((mtco (assoc name-str (cstate-labels-mutual-tco) :test #'string=)))
          (when mtco
            (let* ((fn-index (second mtco))
                   (which-fn-key (third mtco))
                   (tcoloop-label (fourth mtco))
                   (shared-param-keys (fifth mtco)))
              (when (= n-args (length shared-param-keys))
                (let* ((da (compile-direct-call-args args))
                       (temps (car da))
                       (eval-instrs (cdr da)))
                  (return-from compile-named-call
                    `(,@eval-instrs
                      ,@(loop for tmp in temps
                              for key in shared-param-keys
                              append `((:ldloc ,tmp) (:stloc ,key)))
                      (:ldc-i4 ,fn-index)
                      (:stloc ,which-fn-key)
                      ,@(cstate-tco-leave-instrs)
                      ,@(unless (cstate-no-safepoint)
                          '((:call "ConditionSystem.PollInterrupt")))
                      ,(if (cstate-tco-in-try-catch)
                           `(:leave ,tcoloop-label)
                           `(:br ,tcoloop-label))))))))))
      ;; Non-tail self-call in a typed body: straight to the body method with
      ;; raw arguments, and the raw result boxed for this generic context.
      (when (and (cstate-typed-self) (typed-self-call-p (cons name args)))
        (return-from compile-named-call
          `(,@(compile-typed-self-call (cons name args))
            ,@(%box-typed-result (third (cstate-typed-self))))))
      ;; Non-tail self-call fast path: reuse LispFunction cached at body entry.
      (when (and (cstate-self-fn-local)
                 (not (local-function-entry name-str))
                 (if (cstate-tco-self-symbol)
                     (eq name (cstate-tco-self-symbol))
                     (string= name-str (cstate-tco-self-name)))
                 (self-call-shortcut-p name)
                 (<= n-args 8))
        (let* ((skip-reset (single-value-form-p (cons name args)))
               (da (compile-direct-call-args args))
               (temps (car da))
               (eval-instrs (cdr da)))
          (let ((mode (unless skip-reset (%call-site-mv-mode args name t)))
                (tr (and *throw-return-forward* (not *in-finally-block*)
                         (<= n-args 4) (cstate-throw-return))))
            (return-from compile-named-call
              `(,@eval-instrs
                ,(if (eq (cstate-self-fn-local) :arg0) '(:ldarg 0)
                     `(:ldloc ,(cstate-self-fn-local)))
                ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                ,@(loop for tmp in temps append `((:ldloc ,tmp)))
                ,@(if tr
                      ;; Pass this function's claimed CATCH on to the callee; a
                      ;; THROW it returned is returned on from here.
                      (destructuring-bind (claim-key exit-label value-key used) tr
                        (setf (car used) t)
                        (let ((r (gen-local "TRFWD")) (cont (gen-label "TRCONT")))
                          `(,@mode
                            (:ldloc ,claim-key)
                            (:call ,(format nil "ThrowReturn.Forward~D~:[~;M~]" n-args mode))
                            (:declare-local ,r "LispObject")
                            (:stloc ,r) (:ldloc ,r)
                            (:call "ThrowReturn.IsReturned")
                            (:brfalse ,cont)
                            (:call "ThrowReturn.TakePending")
                            (:stloc ,value-key)
                            (:leave ,exit-label)
                            (:label ,cont)
                            (:ldloc ,r))))
                      (%invoke-instrs n-args mode))))))))
    ;; The reader of a (:TYPE VECTOR) structure is the AREF its DEFUN computes.
    (let ((a (%typed-vector-struct-read (cons name args))))
      (when a
        (return-from compile-named-call (compile-expr a))))
    ;; Inline struct accessor: (accessor-name obj) -> StructRefI with raw int index
    ;; Only when not shadowed by a local function (flet/labels)
    (when (and (symbolp name)
               (= (length args) 1)
               (not (local-function-entry name)))
      (let ((slot-idx (gethash name *struct-accessors*)))
        (when slot-idx
          (return-from compile-named-call
            (%compile-struct-read (car args) slot-idx
                                  (struct-slot-ref-type (cons name args)))))))
    ;; Keyword struct constructor called with constant keywords: build the instance
    ;; here instead of calling it. The &key call is what costs, the argument array
    ;; and the keyword scan, while the constructor body is one %MAKE-STRUCT of the
    ;; slot values in slot order, which is what this rewrites to. Declines (leaving
    ;; the ordinary call) whenever the keywords are not statically resolvable.
    (when (and (symbolp name)
               (not *cross-compiling*)
               (not (local-function-entry name))
               (not (member name *notinline-functions*)))
      (let ((entry (gethash name *struct-keyword-ctors*)))
        (when (and entry (not (%global-notinline-p name)))
          (let ((form (%struct-ctor-direct-form entry args)))
            (when form
              (return-from compile-named-call (compile-expr form)))))))
    ;; Inline CLOS simple-reader accessor: (reader obj) -> Runtime.ReaderIC(obj, cell),
    ;; a per-call-site monomorphic inline cache. On a warm hit it reads the slot straight
    ;; from the instance vector (no GF resolution, no dispatch, no name->index lookup); any
    ;; miss re-resolves and anything that isn't a plain instance-slot simple reader falls
    ;; back to full dispatch. The DEFCLASS-populated registry is only a
    ;; compile-time hint that NAME is likely a reader; ReaderIC re-validates at run time
    ;; (SimpleReaderSlot flag + epoch), so a redefined/extended accessor stays correct.
    (when (and (symbolp name)
               (symbol-package name)
               (not *cross-compiling*)
               (= (length args) 1)
               (not (local-function-entry name))
               (gethash name *clos-accessor-readers*))
      (return-from compile-named-call
        `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
              (compile-expr (car args)))
          (:reader-ic ,(symbol-name name)
                      ,(%package-physical-name (symbol-package name))))))
    ;; Inline CLOS simple-writer accessor: ((setf writer) newval obj) ->
    ;; Runtime.WriterIC(newval, obj, cell). Twin of :reader-ic above, same hint/re-validate
    ;; split. The two argument forms are spilled to locals first (CIL forbids entering a
    ;; try region with a non-empty stack, and either form may be a block/loop), then loaded
    ;; in the (SETF name) GF's own order so evaluation order is unchanged.
    (when (and (consp name)
               (symbolp (car name)) (string= (symbol-name (car name)) "SETF")
               (consp (cdr name)) (symbolp (cadr name)) (symbol-package (cadr name))
               (not *cross-compiling*)
               (= (length args) 2)
               (not (local-function-entry name))
               (gethash (cadr name) *clos-accessor-writers*))
      (let ((v-tmp (gen-local "WICV"))
            (o-tmp (gen-local "WICO"))
            (accessor (cadr name)))
        (return-from compile-named-call
          `((:declare-local ,v-tmp "LispObject")
            ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                (compile-expr (first args)))
            (:stloc ,v-tmp)
            (:declare-local ,o-tmp "LispObject")
            ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                (compile-expr (second args)))
            (:stloc ,o-tmp)
            (:ldloc ,v-tmp) (:ldloc ,o-tmp)
            (:writer-ic ,(symbol-name accessor)
                        ,(%package-physical-name (symbol-package accessor)))))))
    ;; A global function whose declaimed FTYPE gives it a typed entry: raw
    ;; arguments straight into that entry, the raw result boxed for this generic
    ;; context. Not in tail position of an ordinary body, where the ordinary call
    ;; is a tail call.
    (when (and (symbolp name)
               ;; In a typed body the tail value is the raw result, so a call
               ;; there is never the ordinary tail call: through the typed entry
               ;; it can be a typed one (FindTypedTailSites).
               (or (not (eq *in-tail-position* t)) (cstate-typed-self))
               (not (local-function-entry name)))
      (let ((specs (typed-call-specs name args)))
        (when specs
          (return-from compile-named-call
            (if (and (consp *typed-call-raw-request*)
                     (eq args (cdr (car *typed-call-raw-request*)))
                     (eq (car (cdr specs)) (cdr *typed-call-raw-request*)))
                (progn (setq *typed-call-raw-request* :done)
                       (compile-typed-call name args specs))
                `(,@(compile-typed-call name args specs)
                  ,@(%box-typed-result (cdr specs))))))))
    ;; --- original compile-named-call body (unchanged) ---
    ;; Compile args first (into temp), then load function and invoke.
    ;; This ensures the stack is empty during arg evaluation, which is
    ;; required by CIL when args contain try blocks (e.g. loop with block).
    (let ((args-tmp (gen-local "NCARGS"))
          (name-str (mangle-name name))
          (n-args (length args))
          (local-fn (local-function-entry name))
          (skip-reset (single-value-form-p (cons name args))))
    (if local-fn
        ;; Local function (flet/labels): load from local or box, cast, invoke
        (let ((key (second local-fn))
              (boxed-p (third local-fn))
              (caps (fourth local-fn)))
          ;; Capture-lifted function (see %LIFT-CAPTURES): the variables its body
          ;; used to close over are passed as trailing arguments. Only legal while
          ;; each one still resolves to the SLOT it had where the FLET was written
          ;; -- a shadowing binding, or a call from inside a nested closure that
          ;; captured its own copy, gives a different slot, and then the lifted
          ;; compile is abandoned for the ordinary closure path.
          (when caps
            (dolist (c caps)
              (unless (eq (lookup-local (car c)) (cdr c))
                (throw (%lift-capture-tag local-fn) :lift-aborted)))
            (setq args (append args (mapcar #'car caps))
                  n-args (length args)))
          (if (<= n-args 8)
              ;; Direct invoke for small arg count (0-8 args, no array allocation)
              (let* ((da (compile-direct-call-args args))
                     (temps (car da))
                     (eval-instrs (cdr da)))
                `(,@eval-instrs
                  ,@(if boxed-p
                        `((:ldloc ,key) (:ldc-i4 0) (:ldelem-ref))
                        `((:ldloc ,key)))
                  (:castclass "LispFunction")
                  ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                  ,@(loop for tmp in temps append `((:ldloc ,tmp)))
                  ,@(%invoke-instrs n-args (unless skip-reset (%call-site-mv-mode args)))))
              `((:declare-local ,args-tmp "LispObject[]")
                ,@(compile-args-array args) (:stloc ,args-tmp)
                ,@(if boxed-p
                      `((:ldloc ,key) (:ldc-i4 0) (:ldelem-ref))
                      `((:ldloc ,key)))
                (:castclass "LispFunction")
                ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                (:ldloc ,args-tmp)
                (:callvirt "LispFunction.Invoke"))))
        ;; Not a local function. In a Lisp-2 the operator position never names a
        ;; lexical variable, so a symbol that is a boxed local *variable*
        ;; (mutated + captured, hence living in a LispObject[1] cell) and happens
        ;; to share a global function's name must still call the GLOBAL function
        ;; here: it must NOT funcall the variable's cell. Boxed local *functions*
        ;; (flet/labels, including ones captured into a closure and re-established
        ;; in *local-functions*) are handled by the local-fn branch above. So
        ;; always take the global-function path.
        ;; Global function: use symbol-based lookup (fixes flat namespace
        ;; collision). A (setf SYM) name resolves via the TARGET symbol's
        ;; SetfFunction (symbol identity), NOT via the "(SETF NAME)" string path:
        ;; CilAssembler.GetFunction does a cross-package name search for (setf ...)
        ;; names, so a same-named accessor in another package (e.g. clump's
        ;; (setf parent) vs spatial-trees' (setf parent)) could be picked up by
        ;; iteration order, dispatching to the wrong GF.
        (let* ((setf-sym-p (and (consp name) (eq (car name) 'setf) (symbolp (cadr name))))
                     (load-fn (cond (setf-sym-p
                                     `(,@(compile-sym-lookup (cadr name))
                                       (:castclass "Symbol")
                                       (:call "CilAssembler.GetSetfFunctionBySymbol")))
                                    ((symbolp name)
                                     `(,@(compile-fn-sym-lookup name)
                                       (:castclass "Symbol")
                                       (:call "CilAssembler.GetFunctionBySymbol")))
                                    (t
                                     `((:ldstr ,(mangle-name name))
                                       (:call "CilAssembler.GetFunction"))))))
                (if (<= n-args 8)
                    (let* ((mode-instrs (unless skip-reset (%call-site-mv-mode args name)))
                           (same-file (%same-file-call-p name n-args))
                           (call-instrs
                             (cond
                               (same-file
                                `(,@mode-instrs
                                  (:call-same-file ,(%package-physical-name (symbol-package name))
                                                   ,(mangle-name name) ,n-args
                                                   ,@(when mode-instrs '(:mode)))))
                               ((%gf-ic-call-p name n-args)
                                `(,@mode-instrs
                                  (:gf-ic1 ,(symbol-name name)
                                           ,(%package-physical-name (symbol-package name))
                                           ,@(when mode-instrs '(:mode)))))
                               (t (%invoke-instrs n-args mode-instrs)))))
                      (if (and (symbolp name) (every #'simple-expr-p args))
                          ;; Fast path: simple args -> skip temps, push directly
                          `(,@load-fn
                            ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                            ,@(loop for arg in args
                                    append (let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr arg)))
                            ,@call-instrs)
                          (let* ((da (compile-direct-call-args args))
                                 (temps (car da))
                                 (eval-instrs (cdr da)))
                            `(,@eval-instrs
                              ,@load-fn
                              ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                              ,@(loop for tmp in temps append `((:ldloc ,tmp)))
                              ,@call-instrs))))
                    `((:declare-local ,args-tmp "LispObject[]")
                      ,@(compile-args-array args) (:stloc ,args-tmp)
                      ,@load-fn
                      ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                      (:ldloc ,args-tmp)
                      (:callvirt "LispFunction.Invoke"))))))))


;;; ============================================================
;;; if
;;; ============================================================

(defun %struct-type-test-type-p (type)
  "TYPE when it names a standard structure DEFSTRUCT recorded, else NIL."
  (and (not *cross-compiling*)
       (symbolp type)
       (let ((info (gethash type *struct-info*)))
         (and info (null (getf info :type-option))))))

(defun compile-struct-typep (obj type)
  "Inline structure TYPEP; follows the instance's type name and :INCLUDE parents."
  (let ((not-struct (gen-label "STNOT"))
        (no-match (gen-label "STNO"))
        (found (gen-label "STYES"))
        (loop-label (gen-label "STLOOP"))
        (end-label (gen-label "STEND")))
    `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
          (compile-expr obj))
      (:isinst "LispStruct")
      (:dup)
      (:brfalse ,not-struct)
      (:call "LispStruct.get_TypeName")
      (:castclass "LispObject")
      (:label ,loop-label)
      (:dup)
      ,@(compile-quoted type)
      (:ceq)
      (:brtrue ,found)
      (:call "Symbol.get_StructParent")
      (:castclass "LispObject")
      (:dup)
      (:brtrue ,loop-label)
      (:pop)
      (:label ,no-match)
      (:ldc-i4 0)
      (:br ,end-label)
      (:label ,found)
      (:pop)
      (:ldc-i4 1)
      (:br ,end-label)
      (:label ,not-struct)
      (:pop)
      (:ldc-i4 0)
      (:label ,end-label))))

(defun compile-if-fused-comparison-p (cond-expr)
  "If cond-expr is a binary comparison suitable for fused comparison+branch,
   return (:binary method-name) or (:unary method-name).
   Returns nil otherwise."
  (when (and (consp cond-expr)
             (symbolp (car cond-expr))
             ;; Don't fuse if the comparison is a local function (flet/labels shadowing)
             (not (local-function-entry (car cond-expr))))
    (let ((op (car cond-expr))
          (nargs (length (cdr cond-expr)))
          (args (cdr cond-expr)))
      (cond
        ;; The absent-key test of a &key function's shared body.
        ((and (eq op '%key-absent-p) (= nargs 1))
         (list :unary "AbsentKey.Is" args))
        ;; Special case: (= x 0) or (= 0 x) -> zerop optimization. An X the
        ;; typed paths can compute raw is compared raw: the zerop entry takes an
        ;; object, so an Int64 or double X was boxed only to be compared.
        ((and (eq op '=) (= nargs 2)
              (or (eql (second cond-expr) 0) (eql (third cond-expr) 0)))
         (let ((non-zero-arg (if (eql (second cond-expr) 0) (third cond-expr) (second cond-expr))))
           (cond ((fixnum-typed-p non-zero-arg)
                  (list :fixnum-cmp :eq (list non-zero-arg 0)))
                 ((float-typed-p non-zero-arg)
                  (list :double-cmp :eq (list non-zero-arg 0d0)))
                 (t (list :unary "Runtime.IsTrueZerop" (list non-zero-arg))))))
        ;; Float-typed fast path: both args statically float -> native r8 compare.
        ;; Emitted as compile-double-cmp, which widens a single-float operand.
        ;;
        ;; Mixing formats is the ordinary case, not an exotic one: a literal like
        ;; 4.0 reads as a single float, so (> (abs z) 4.0) on a double had one
        ;; operand of each format and fell off this path entirely -- the double
        ;; got boxed and the comparison went through the generic entry.
        ((and (= nargs 2)
              (member op '(< > <= >= = /=))
              (float-typed-p (first args))
              (float-typed-p (second args)))
         (list :double-cmp
               (ecase op (< :lt) (> :gt) (<= :le) (>= :ge) (= :eq) (/= :ne))
               args))
        ;; Fixnum-typed fast path: both args statically fixnum -> native i8 compare.
        ;; Emitted as compile-fixnum-cmp which leaves an i4 (0/1) on stack.
        ((and (= nargs 2)
              (member op '(< > <= >= = /=))
              (fixnum-typed-p (first args))
              (fixnum-typed-p (second args)))
         (list :fixnum-cmp
               (ecase op (< :lt) (> :gt) (<= :le) (>= :ge) (= :eq) (/= :ne))
               args))
        ;; EQL / EQUAL / EQUALP of two fixnum-typed operands is =: on integers
        ;; all three compare by value. The generic entries take objects, so a
        ;; raw slot or an Int64 local was boxed only to be compared; for EQUALP
        ;; it was also a full call through the symbol.
        ((and (= nargs 2)
              (member op '(eql equal equalp))
              (fixnum-typed-p (first args))
              (fixnum-typed-p (second args)))
         (list :fixnum-cmp :eq args))
        ;; Character comparison whose operands' codes are reachable without
        ;; building a character: a literal, a local declared CHARACTER, or a
        ;; string reference. Comparing characters is comparing codes, so this
        ;; lowers to the same integer compare the fixnum path uses -- and the
        ;; boolean object the generic entry returns, plus the IsTruthy that
        ;; tests it, both disappear.
        ((and (= nargs 2)
              (member op '(char= char/= char< char> char<= char>=))
              (char-code-operand (first args))
              (char-code-operand (second args)))
         (list :char-cmp
               (ecase op
                 (char< :lt) (char> :gt) (char<= :le)
                 (char>= :ge) (char= :eq) (char/= :ne))
               args))
        ;; Binary numeric comparisons (generic path)
        ((and (= nargs 2)
              (cdr (assoc op '((> . "Runtime.IsTrueGt")
                               (< . "Runtime.IsTrueLt")
                               (>= . "Runtime.IsTrueGe")
                               (<= . "Runtime.IsTrueLe")
                               (= . "Runtime.IsTrueNumEq")))))
         (list :binary (cdr (assoc op '((> . "Runtime.IsTrueGt")
                                        (< . "Runtime.IsTrueLt")
                                        (>= . "Runtime.IsTrueGe")
                                        (<= . "Runtime.IsTrueLe")
                                        (= . "Runtime.IsTrueNumEq")))) args))
        ;; Fixnum-typed unary sign predicate -> native i8 compare against 0.
        ;; (zerop (logand ...)) etc. lower the arg via compile-as-long, so a
        ;; native logand feeds ceq directly with no Fixnum.Make round-trip.
        ((and (= nargs 1)
              (member op '(zerop minusp plusp))
              (fixnum-typed-p (first args)))
         (list :fixnum-cmp
               (ecase op (zerop :eq) (minusp :lt) (plusp :gt))
               (list (first args) 0)))
        ;; Unary predicates: zerop, minusp, plusp
        ((and (= nargs 1)
              (member op '(zerop minusp plusp)))
         (list :unary (cdr (assoc op '((zerop . "Runtime.IsTrueZerop")
                                       (minusp . "Runtime.IsTrueMinusp")
                                       (plusp . "Runtime.IsTruePlusp")))) args))
        ;; (eq x 'sym) / (eql x 'sym) against a symbol literal -> inline
        ;; reference comparison instead of a call. CASE expands to a linear
        ;; COND of these, so a dispatch pays one call per clause it walks past
        ;; (measured ~25 ns each; %MINI-EVAL's 32-key dispatch pays ~0.77 us
        ;; before it reaches the function-call default).
        ;; (eq x nil): EQ bridges the NIL object and the NIL symbol, as IsNilObj does.
        ((and (= nargs 2)
              (member op '(eq eql))
              (or (%nil-operand-p (first args)) (%nil-operand-p (second args))))
         (list :unary "Runtime.IsNilObj"
               (list (if (%nil-operand-p (first args)) (second args) (first args)))))
        ((and (= nargs 2)
              (member op '(eq eql))
              (or (literal-symbol-operand (first args))
                  (literal-symbol-operand (second args))))
         (list :sym-eq nil args))
        ((and (= nargs 2)
              (member op '(eq eql equal equalp)))
         (list :binary (cdr (assoc op '((eq . "Runtime.IsTrueEq")
                                        (eql . "Runtime.IsTrueEql")
                                        (equal . "Runtime.IsTrueEqual")
                                        (equalp . "Runtime.IsTrueEqualp")))) args))
        ;; Unary type predicates: consp, atom, null (as predicate, not as not)
        ((and (= nargs 1)
              (member op '(consp atom listp numberp integerp symbolp stringp
                           characterp functionp endp)))
         (list :unary (cdr (assoc op '((consp . "Runtime.IsTrueConsp")
                                       (endp . "Runtime.IsTrueEndp")
                                       (atom . "Runtime.IsTrueAtom")
                                       (listp . "Runtime.IsTrueListp")
                                       (numberp . "Runtime.IsTrueNumberp")
                                       (integerp . "Runtime.IsTrueIntegerp")
                                       (symbolp . "Runtime.IsTrueSymbolp")
                                       (stringp . "Runtime.IsTrueStringp")
                                       (characterp . "Runtime.IsTrueCharacterp")
                                       (functionp . "Runtime.IsTrueFunctionp")))) args))
        ;; (typep x 'struct) -> inline structure name / :INCLUDE test
        ((and (eq op 'typep) (= nargs 2)
              (let ((type-arg (second args)))
                (and (consp type-arg) (eq (car type-arg) 'quote)
                     (%struct-type-test-type-p (cadr type-arg)))))
         (list :struct-typep (cadr (second args)) (list (first args))))
        ;; (typep x 'known-type) -> IsTrueXxx fused predicate
        ((and (eq op 'typep) (= nargs 2)
              (let ((type-arg (second args)))
                (and (consp type-arg) (eq (car type-arg) 'quote)
                     (symbolp (cadr type-arg)))))
         (let* ((type-name (cadr (second args)))
                (predicate (cdr (assoc type-name
                                  '((cons . "Runtime.IsTrueConsp")
                                    (list . "Runtime.IsTrueListp")
                                    (number . "Runtime.IsTrueNumberp")
                                    (integer . "Runtime.IsTrueIntegerp")
                                    (symbol . "Runtime.IsTrueSymbolp")
                                    (string . "Runtime.IsTrueStringp")
                                    (character . "Runtime.IsTrueCharacterp")
                                    (function . "Runtime.IsTrueFunctionp")
                                    (atom . "Runtime.IsTrueAtom"))))))
           (if predicate
               (list :unary predicate (list (first args)))
               ;; Non-standard type: use IsTrueTypep for fused branch
               (list :binary "Runtime.IsTrueTypep" args))))
        (t nil)))))

(defun struct-slot-ref (expr)
  "(OBJ . SLOT-INDEX) when EXPR reads a structure slot at a constant index --
   a DEFSTRUCT accessor call, or the %STRUCT-REF it is lowered to. NIL otherwise.
   A local flet/labels binding the accessor name means the form is a call to
   that, not a slot read."
  (and (consp expr)
       (cond
         ((and (symbolp (car expr)) (= (length expr) 2)
               (not (local-function-entry (car expr))))
          (let ((idx (gethash (car expr) *struct-accessors*)))
            (and idx (cons (cadr expr) idx))))
         ((and (eq (car expr) '%struct-ref) (<= 3 (length expr) 4)
               (integerp (caddr expr)))
          (cons (cadr expr) (caddr expr))))))

(defun %struct-check-name (type-form)
  "The structure name a slot access checks its object against, from the quoted
   name an accessor's expansion carries; NIL when no check is emitted."
  (and (consp type-form) (eq (car type-form) 'quote)
       (symbolp (cadr type-form)) (cadr type-form)
       (not *cross-compiling*)
       (not (compiling-at-safety-0-p))
       (cadr type-form)))

(defun struct-slot-ref-type (expr)
  "For an EXPR STRUCT-SLOT-REF accepts, the structure name its object is
   checked against, or NIL."
  (%struct-check-name
   (if (eq (car expr) '%struct-ref)
       (fourth expr)
       (let ((o (and (boundp '*struct-accessor-owners*)
                     (gethash (car expr) *struct-accessor-owners*))))
         (and o (list 'quote o))))))

(defun %compile-struct-read (obj idx type)
  "A boxed read of the slot at the packed index IDX of OBJ, checking OBJ is a
   TYPE structure when TYPE."
  `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
        (compile-expr obj))
    (:ldc-i4 ,idx)
    ,@(if type
          `(,@(compile-quoted type) (:call "Runtime.StructRefIT"))
          '((:call "Runtime.StructRefI")))))

(defun compile-struct-object (obj type)
  "OBJ compiled for a slot access, checked to be a TYPE structure when TYPE."
  `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
        (compile-expr obj))
    ,@(when type
        `(,@(compile-quoted type) (:call "Runtime.StructOf")))))

(defun %strip-the (expr)
  "EXPR with any THE wrappers removed. A THE says what the value is, not how it
   is produced, so the form underneath is the one to identify."
  (loop while (and (consp expr) (eq (car expr) 'the) (= (length expr) 3))
        do (setq expr (caddr expr)))
  expr)

(defun struct-slot-read-emitted-p (expr code)
  "True when EXPR names a structure slot read AND the code just emitted for it
   is that read: CODE ends in the Runtime.StructRefI that fetches the slot.

   A slot read is single-valued by construction, so the Runtime.UnwrapMv a
   caller would append to CODE can only be a no-op. A slot holds an ordinary
   value: every store into one evaluates its value in a single-value position,
   which is the same reason CAR and AREF are already taken as single-valued.

   Both halves are required. The name alone does not settle it: a macro, a
   compiler macro, an INLINE proclamation or a local FLET binding of the
   accessor name is compiled in place of the slot read and may answer anything
   at all, and a slot read in a fixnum or double context is lowered to a
   different instruction entirely. Asking what was emitted rather than
   predicting it from the source form keeps the two in step by construction.
   The name is tested first so that the walk to the end of CODE only runs for
   the handful of forms that could pass. THE wrappers are stepped through:
   (THE SIMPLE-STRING (ACC S)) is the same read, and the emitted code decides
   whether it still ends in one."
  (and (struct-slot-ref (%strip-the expr))
       (let ((tail (car (last code))))
         (and (consp tail)
              (eq (car tail) :call)
              (member (cadr tail) '("Runtime.StructRefI" "Runtime.StructRefIT")
                      :test #'equal)))))

(defun code-ends-unwrapped-p (code)
  "True when CODE ends in a Runtime.UnwrapMv, so its value is already a single
   value: a LET or PROGN whose last form was compiled in this same position."
  (let ((tail (car (last code))))
    (and (consp tail)
         (eq (car tail) :call)
         (equal (cadr tail) "Runtime.UnwrapMv")
         (null (cddr tail)))))

(defun %struct-backing-read (hit obj packed &optional type)
  "A raw slot read at a constant position against OBJ's hoisted raw array, with
   the arm that runs when there is no such array.

   HIT is what STRUCT-BACKING-ENTRY answered: the binding entry and the slot's
   (POSITION . KIND). The fast arm is a bare ldelem; the null arm is exactly
   the call this replaces, which is what keeps every answer and every error the
   same for an object that could not be hoisted. Both arms leave the same raw
   value, an int64 or, for a double slot, an r8."
  (let* ((back (struct-backing-slot (car hit)))
         (pos (cadr hit))
         (kind (cddr hit))
         (fall (gen-label "SBFALL"))
         (done (gen-label "SBDONE")))
    (when (struct-backing-known-p (car hit))
      (return-from %struct-backing-read
        `((:ldloc ,back)
          (:ldc-i4 ,pos)
          (:ldelem-i8)
          ,@(case kind
              (1 '((:call "Runtime.RawBitsToDouble")))
              (2 '((:call "Runtime.RawBitsToSingle")))))))
    `((:ldloc ,back)
      (:brfalse ,fall)
      (:ldloc ,back)
      (:ldc-i4 ,pos)
      (:ldelem-i8)
      ,@(case kind
          (1 '((:call "Runtime.RawBitsToDouble")))
          (2 '((:call "Runtime.RawBitsToSingle"))))
      (:br ,done)
      (:label ,fall)
      ,@(compile-struct-object obj type)
      (:ldc-i4 ,packed)
      (:call ,(case kind
                (1 "Runtime.StructRefD")
                (2 "Runtime.StructRefF")
                (t "Runtime.StructRefL")))
      (:label ,done))))

(defun %struct-backing-write (hit obj packed val-instrs &optional type)
  "A raw slot store at a constant position against OBJ's hoisted raw array,
   leaving the stored raw value on the stack for the caller to box.

   VAL-INSTRS must leave the value already lowered to the slot's raw
   representation. It is evaluated once, into a temporary, ahead of the branch:
   both arms need it, and a store must not evaluate its value twice."
  (let* ((back (struct-backing-slot (car hit)))
         (pos (cadr hit))
         (kind (cddr hit))
         (vtmp (gen-local "SBV"))
         (fall (gen-label "SBFALL"))
         (done (gen-label "SBDONE")))
    (when (struct-backing-known-p (car hit))
      (return-from %struct-backing-write
        `((:declare-local ,vtmp ,(case kind (1 "Double") (2 "Single") (t "Int64")))
          ,@val-instrs
          (:stloc ,vtmp)
          (:ldloc ,back)
          (:ldc-i4 ,pos)
          (:ldloc ,vtmp)
          ,@(case kind
              (1 '((:call "Runtime.RawDoubleToBits")))
              (2 '((:call "Runtime.RawSingleToBits"))))
          (:stelem-i8)
          (:ldloc ,vtmp))))
    `((:declare-local ,vtmp ,(case kind (1 "Double") (2 "Single") (t "Int64")))
      ,@val-instrs
      (:stloc ,vtmp)
      (:ldloc ,back)
      (:brfalse ,fall)
      (:ldloc ,back)
      (:ldc-i4 ,pos)
      (:ldloc ,vtmp)
      ,@(case kind
          (1 '((:call "Runtime.RawDoubleToBits")))
          (2 '((:call "Runtime.RawSingleToBits"))))
      (:stelem-i8)
      (:br ,done)
      (:label ,fall)
      ,@(compile-struct-object obj type)
      (:ldc-i4 ,packed)
      (:ldloc ,vtmp)
      (:call ,(case kind
                (1 "Runtime.StructSetD")
                (2 "Runtime.StructSetF")
                (t "Runtime.StructSetL")))
      (:pop)
      (:label ,done)
      (:ldloc ,vtmp))))

(defun compile-struct-slot-as-long (obj idx &optional type)
  "Emit a structure slot read as a raw int64 (Runtime.StructRefL). Reached only
   from COMPILE-AS-LONG, i.e. where the context already reads the slot as a
   fixnum; a slot holding something else fails the same cast it fails today.

   When OBJ is a local whose raw slot array was hoisted at its binding, the
   call becomes a bare element load against that array."
  (let ((hit (struct-backing-entry obj idx type)))
    (if (and hit (eql (cddr hit) 0))
        (%struct-backing-read hit obj idx type)
        `(,@(compile-struct-object obj type)
          (:ldc-i4 ,idx)
          (:call "Runtime.StructRefL")))))

(defun compile-progn-boolean-tail (forms label branch-on-true)
  "FORMS as statements except the last, which is compiled as a branch to LABEL.
   The shape a binding form's body takes when the binding form itself sits in a
   test position: the value of the body is never built, only branched on."
  (if (null forms)
      ;; An empty body is NIL, which is false.
      (if branch-on-true '() `((:br ,label)))
      `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
            (%compile-statements (butlast forms)))
        ,@(compile-boolean-branch (car (last forms)) label branch-on-true))))

(defun compile-progn-long-tail (forms)
  "FORMS as statements except the last, whose value is left as a raw int64.
   The shape a binding form's body takes when the binding form itself is wanted
   as an int64 (COMPILE-AS-LONG-STRUCTURED): the value of the body is never
   boxed. The caller has established that the value is an integer that fits
   int64; a last form the typed paths cannot see into is compiled as a value and
   unboxed, as COMPILE-AS-LONG does for any form it has no clause for."
  (let ((*in-tail-position* nil) (*in-mv-context* nil))
    `(,@(%compile-statements (butlast forms))
      ,@(compile-long-tail-form (car (last forms))))))

(defun compile-long-tail-form (x)
  "X compiled leaving a raw int64: natively when its value is known exactly
   (FIXNUM-EXACT-P), through COMPILE-AS-LONG-STRUCTURED when X is itself a
   binding form or an inline call, else as a value and unboxed."
  (let ((*in-tail-position* nil) (*in-mv-context* nil))
    (cond ((fixnum-exact-p x) (compile-as-long x))
          ((compile-as-long-structured x))
          (t `(,@(compile-expr x) (:unbox-fixnum))))))

(defun compile-as-long-structured (expr)
  "Instructions leaving EXPR's value as a raw int64 when EXPR is a binding form
   or an inline call whose value is wanted that way, else NIL. The caller has
   established that the value is an integer that fits int64 -- for an inline
   call, the function's declared return type.

   An inline call is a LET around a BLOCK (MAYBE-EXPAND-INLINE). Compiled as a
   value, the BLOCK's result is a boxed object, so (mod32+ a b) declared to
   return (UNSIGNED-BYTE 32) allocated a Fixnum for every call only for the
   caller to unbox it again. Here the same expansion is compiled with its last
   form left as an int64, the way COMPILE-BOOLEAN-BRANCH compiles it with its
   last form left as a branch, and under the same conditions: no SPECIAL
   binding in the LET, and no RETURN-FROM to the BLOCK (a macro that expands
   into one still finds the BLOCK: *TRANSPARENT-BLOCKS* aborts and the form is
   compiled as a value)."
  (let ((*in-tail-position* nil) (*in-mv-context* nil))
    (cond
      ((not (consp expr)) nil)
      ((and (member (car expr) '(let let*)) (consp (cdr expr)) (listp (cadr expr))
            (%boolean-branchable-let-p (cadr expr) (cddr expr)))
       (compile-let (cadr expr) (cddr expr) (eq (car expr) 'let*) :long))
      ((and (eq (car expr) 'block) (consp (cdr expr)) (symbolp (cadr expr)) (cddr expr)
            (not (%block-returns-to-p (cadr expr) (cddr expr))))
       (let* ((name (cadr expr))
              (tag (list '#:transparent-block))
              (result (let ((*transparent-blocks*
                              (cons (list* name tag (%block-identity name))
                                    *transparent-blocks*))
                            (*inlining-stack* (inline-guard-stack name)))
                        (catch tag
                          (compile-progn-long-tail (cddr expr))))))
         (if (eq result :transparent-block-aborted)
             `(,@(compile-expr expr) (:unbox-fixnum))
             result)))
      ((and (eq (car expr) 'progn) (cdr expr))
       (compile-progn-long-tail (cdr expr)))
      ;; An inline body recorded with the MACROLETs around its DEFUN.
      ((and (eq (car expr) 'macrolet) (consp (cdr expr)) (listp (cadr expr)) (cddr expr))
       (compile-macrolet (cadr expr) (cddr expr) #'compile-progn-long-tail))
      ((and (symbolp (car expr)) (car expr)
            (gethash (car expr) *inline-defs*)
            (not (special-operator-name-p (car expr)))
            (not (gethash (car expr) *compile-form-handlers*))
            (not (local-function-entry (car expr)))
            (not (macrolet-shadowed-p (car expr)))
            (not (find-macro-expander (car expr)))
            (not (maybe-expand-compiler-macro (car expr) expr)))
       ;; The recursion guard is pushed by the expansion's BLOCK
       ;; (INLINE-GUARD-STACK).
       (let ((inl (maybe-expand-inline (car expr) expr)))
         (and inl
              (let ((*at-toplevel* nil))
                (compile-long-tail-form inl)))))
      (t nil))))

;; An inline call whose value is an (unsigned-byte 64) built inside its body
;; (ironclad's UB64REF/LE: two 32-bit reads deposited into one word) cannot be
;; planned through by the 64-bit lane, whose operands are evaluated ahead of
;; the expression, outside the body's bindings. Compiled as a value, the word
;; is boxed (a bignum at or above 2^63) for the lane to take apart again.
;; COMPILE-AS-U64-STRUCTURED compiles the expansion with its last form left as
;; the word's bits instead, when that form is exact in the lane in the body's
;; own scope. Anything else aborts, and the call is compiled as a value.

(defun compile-progn-u64-tail (forms tag)
  "FORMS as statements except the last, whose value is left as the bits of an
   (unsigned-byte 64), an int64. Throws to TAG when the last form is not known
   to be one."
  (let ((*in-tail-position* nil) (*in-mv-context* nil))
    `(,@(%compile-statements (butlast forms))
      ,@(compile-u64-tail-form (car (last forms)) tag))))

(defun compile-u64-tail-form (x tag)
  (let ((*in-tail-position* nil) (*in-mv-context* nil)
        (*u64-structured-nontail* nil))
    (cond
      ((numeric-array-aref-u64-p x)
       (compile-u64-aref-bits (cadr x) (cddr x)))
      ((and (symbolp x) (u64-native-local-p x))
       `((:ldloc ,(u64-native-local-p x))))
      ((let ((ow (octet-word-ref-info x))) (and ow (= (fourth ow) 8)))
       (compile-octet-word-bits (octet-word-ref-info x)))
      ((and (fixnum-exact-p x)
            (let ((r (expr-int-range x))) (and r (>= (car r) 0))))
       (compile-as-long x))
      ((%u64-structured-candidate-p x)
       (%compile-as-u64-structured x tag))
      (t
       ;; Exact in the lane, with no operand that has to be checked: no path
       ;; can leave [0, 2^64) or need the generic arithmetic.
       (let* ((*u64-leaves* nil)
              (plan (u64-plan-compound x :exact)))
         (if (and plan
                  (not (eq (car plan) :leaf))
                  (notany #'%u64-guarded-leaf-p *u64-leaves*))
             (%u64-alias-leaf-copies
              `(,@(%u64-leaf-code (reverse *u64-leaves*))
                ,@(%u64-fast plan))
              (reverse *u64-leaves*))
             (throw tag nil)))))))

(defun %u64-structured-candidate-p (expr)
  (and (consp expr)
       (or (and (member (car expr) '(let let*)) (consp (cdr expr)) (listp (cadr expr))
                (%boolean-branchable-let-p (cadr expr) (cddr expr)))
           (and (eq (car expr) 'block) (consp (cdr expr)) (symbolp (cadr expr)) (cddr expr)
                (not (%block-returns-to-p (cadr expr) (cddr expr))))
           (and (eq (car expr) 'progn) (cdr expr))
           (and (eq (car expr) 'symbol-macrolet) (consp (cdr expr)) (listp (cadr expr))
                (cddr expr))
           (%u64-inline-call-p expr))))

(defun %u64-local-inline-call-p (expr)
  "True when EXPR calls a local function that is substituted at this call."
  (let ((op (car expr)))
    (and (symbolp op) op
         (nth 5 (local-function-entry op))
         (let ((*local-inline-probe* t))
           (and (maybe-expand-local-inline op expr) t)))))

(defun %u64-inline-call-p (expr)
  (let ((op (car expr)))
    (and (symbolp op) op
         (or (%u64-local-inline-call-p expr)
             (and (gethash op *inline-defs*)
                  (not (special-operator-name-p op))
                  (not (gethash op *compile-form-handlers*))
                  (not (local-function-entry op))
                  (not (macrolet-shadowed-p op))
                  (not (find-macro-expander op))
                  (not (maybe-expand-compiler-macro op expr)))))))

(defun %compile-as-u64-structured (expr tag)
  (let ((*in-tail-position* nil) (*in-mv-context* nil)
        (*u64-structured-nontail* *u64-structured-probing*))
    (case (car expr)
      ((let let*)
       (compile-let (cadr expr) (cddr expr) (eq (car expr) 'let*)
                    (lambda (real-body) (compile-progn-u64-tail real-body tag))))
      (block
       (let* ((name (cadr expr))
              (btag (list '#:transparent-block))
              (result (let ((*transparent-blocks*
                              (cons (list* name btag (%block-identity name))
                                    *transparent-blocks*))
                            (*inlining-stack* (inline-guard-stack name)))
                        (catch btag
                          (compile-progn-u64-tail (cddr expr) tag)))))
         (if (eq result :transparent-block-aborted)
             (throw tag nil)
             result)))
      (progn (compile-progn-u64-tail (cdr expr) tag))
      (symbol-macrolet
       (compile-symbol-macrolet (cadr expr) (cddr expr)
                                (lambda (body) (compile-progn-u64-tail body tag))))
      (t (let ((inl (if (local-function-entry (car expr))
                        (maybe-expand-local-inline (car expr) expr)
                        (maybe-expand-inline (car expr) expr))))
           (if inl
               (let ((*at-toplevel* nil))
                 (compile-u64-tail-form inl tag))
               (throw tag nil)))))))

(defun compile-as-u64-structured (expr)
  "Instructions leaving the value of EXPR, an inline call, as the bits of an
   (unsigned-byte 64) when its expansion computes one exactly; else NIL. The
   code compiled before an abort is dropped, as for a transparent BLOCK."
  (and (consp expr)
       (%u64-inline-call-p expr)
       (flet ((try (probing)
                (let ((tag (list '#:u64-structured))
                      (*u64-structured-probing* probing))
                  (catch tag
                    (%compile-as-u64-structured expr tag)))))
         ;; Most expansions do not end in a word. Find that out first without
         ;; trying the operands before the last form as structured ones, and
         ;; compile the expansion again only when it does and the probe passed
         ;; over such an operand. Inside another probe only the answer counts.
         (if *u64-structured-probing*
             (try t)
             (let* ((*u64-structured-skipped* nil)
                    (code (try t)))
               (cond ((null code) nil)
                     (*u64-structured-skipped* (try nil))
                     (t code)))))))

(defun %boolean-branchable-let-p (bindings body)
  "True when a LET/LET* in a test position may have its body branched on
   directly instead of producing a value.

   Refused for a SPECIAL binding: that one needs the dynamic stack and the
   try/finally that pops it, and a branch leaving the body from inside the
   protected region is a different shape than this emits. Refused for an empty
   body, which has nothing to branch on."
  (and body
       (every (lambda (b)
                (let ((name (if (consp b) (car b) b)))
                  (and (symbolp name) name (not (global-special-p name)))))
              bindings)
       (notany (lambda (f)
                 (and (consp f) (eq (car f) 'declare)
                      (some (lambda (d) (and (consp d) (eq (car d) 'special)))
                            (cdr f))))
               body)))

(defun %block-returns-to-p (name forms)
  "True when FORMS contain a RETURN-FROM naming NAME. Conservative: a nested
   BLOCK of the same name shadows the outer one, but this does not model that --
   a hit anywhere refuses the transparent treatment, which only costs speed."
  (let ((hit nil))
    (labels ((walk (x)
               (when (and (consp x) (not hit))
                 (cond
                   ((eq (car x) 'quote))
                   ((and (eq (car x) 'return-from) (eq (cadr x) name))
                    (setf hit t))
                   (t (walk (car x)) (walk (cdr x)))))))
      (walk forms))
    hit))

;; A BLOCK in a test position is compiled without its binding when no
;; RETURN-FROM naming it is visible in the source (%BLOCK-RETURNS-TO-P). A
;; macro can still expand into one: (RETURN X) is (RETURN-FROM NIL X). Each
;; entry here is (NAME TAG . OUTER), OUTER being the %BLOCK-IDENTITY of NAME
;; where the BLOCK was dropped. A RETURN-FROM that still resolves to OUTER was
;; aimed at the dropped block; it throws to TAG and the BLOCK is compiled with
;; its binding.
(defvar *transparent-blocks* nil)

(defun %block-identity (name)
  "What identifies the block NAME resolves to here, or NIL for none: its
   needs-catch cell. Not the entry or its tag key: a closure rebuilds the
   entries it inherits under its own keys, and keeps only that cell."
  (nth 6 (assoc name (cstate-block-tags))))

(defun %transparent-block-check (name)
  (let ((hit (assoc name *transparent-blocks* :test #'eq)))
    (when (and hit (eq (cddr hit) (%block-identity name)))
      (throw (cadr hit) :transparent-block-aborted))))

(defun %truth-test-instrs (expr)
  "EXPR's value and the test of it for not being NIL. A lexical variable never
   holds a multiple-values return, so its test is the reference comparison."
  (let ((code (let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr expr))))
    `(,@code
      ,(if (%plain-local-load-p expr code)
           '(:call "Runtime.IsNotNil")
           '(:call "Runtime.IsTruthy")))))

(defun %boolean-branch-default (expr label branch-on-true)
  "EXPR's value, asked IsTruthy, then the branch."
  `(,@(%truth-test-instrs expr)
    (:call "MultipleValues.Reset")
    (,(if branch-on-true :brtrue :brfalse) ,label)))

(defun %test-value-form (expr)
  "EXPR without the wrappers a test can branch through: those of
   STRIP-VALUE-WRAPPERS other than LOCALLY, and (THE TYPE X)."
  (loop
    (let ((s (strip-value-wrappers expr nil)))
      (cond ((not (eq s expr)) (setq expr s))
            ((and (consp expr) (eq (car expr) 'the) (consp (cdr expr)) (consp (cddr expr))
                  (null (cdddr expr)))
             (setq expr (caddr expr)))
            (t (return expr))))))

(defun %boolean-branch-macro-p (expr)
  "True when EXPR is a call of a macro the compiler has no handler for, which a
   test position can branch on the expansion of."
  (and (consp expr) (symbolp (car expr)) (car expr)
       (not (gethash (car expr) *compile-form-handlers*))
       (find-macro-expander (car expr))
       (or (not (local-function-entry (car expr)))
           (%lexical-macro-p (car expr)))))

(defun compile-boolean-branch (expr label branch-on-true)
  "Compile expr as a boolean condition and emit a branch to label.
   If branch-on-true, branch when condition is true (brtrue).
   Otherwise, branch when condition is false (brfalse).
   Handles fused comparisons, (not x), nested (and ...) / (or ...) recursively."
  (let ((fused (compile-if-fused-comparison-p
                (setq expr (or (%typep-constant-rewrite expr) expr)))))
    (cond
      ;; A constant: the branch is decided now.
      ((or (eq expr t) (eq expr nil) (keywordp expr) (numberp expr) (characterp expr)
           (and (consp expr) (eq (car expr) 'quote) (consp (cdr expr)) (null (cddr expr))))
       (let ((true (if (consp expr) (not (null (cadr expr))) (not (null expr)))))
         (if (eq true branch-on-true) `((:br ,label)) '())))
      ;; Fused comparison: IsTrueXxx -> bool -> branch
      (fused
       `(,@(let ((*in-tail-position* nil))
             (ecase (first fused)
               (:binary (compile-binary-call (third fused) (second fused)))
               (:unary (compile-unary-call (third fused) (second fused)))
               (:fixnum-cmp (compile-fixnum-cmp (third fused) (second fused)))
               (:char-cmp (compile-char-cmp (third fused) (second fused)))
               (:sym-eq (compile-sym-eq (third fused)))
               (:double-cmp (compile-double-cmp (third fused) (second fused)))
               (:struct-typep (compile-struct-typep (first (third fused)) (second fused)))))
         (,(if branch-on-true :brtrue :brfalse) ,label)))
      ;; The value passed through unchanged: branch on it.
      ((%locally-wrapper-p expr)
       (let ((*lexical-decl-frames* (%push-decl-frame :locally nil (cdr expr))))
         (compile-boolean-branch (car (last expr)) label branch-on-true)))
      ((not (eq (%test-value-form expr) expr))
       (compile-boolean-branch (%test-value-form expr) label branch-on-true))
      ;; (not x) / (null x): negate direction and recurse
      ((and (consp expr)
            (symbolp (car expr))
            (member (car expr) '(not null))
            (= (length (cdr expr)) 1)
            (not (local-function-entry (car expr))))
       (compile-boolean-branch (cadr expr) label (not branch-on-true)))
      ;; A binding form in a test position. Without these the value of the body
      ;; is built as an object and then asked IsTruthy about, which is what an
      ;; inlined predicate turns every comparison into: the expansion is a LET
      ;; around a BLOCK, and the test context used to stop at the LET.
      ((and (consp expr) (member (car expr) '(let let*))
            (%boolean-branchable-let-p (cadr expr) (cddr expr)))
       (compile-let (cadr expr) (cddr expr) (eq (car expr) 'let*)
                    (cons label branch-on-true)))
      ;; A BLOCK nothing returns from is transparent here.
      ((and (consp expr) (eq (car expr) 'block) (cddr expr)
            (not (%block-returns-to-p (cadr expr) (cddr expr))))
       (let* ((name (cadr expr))
              (tag (list '#:transparent-block))
              (result (let ((*transparent-blocks*
                              (cons (list* name tag (%block-identity name))
                                    *transparent-blocks*))
                            (*inlining-stack* (inline-guard-stack name)))
                        (catch tag
                          (compile-progn-boolean-tail (cddr expr) label branch-on-true)))))
         (if (eq result :transparent-block-aborted)
             (%boolean-branch-default expr label branch-on-true)
             result)))
      ;; A call to a predicate proclaimed INLINE. The substitution normally
      ;; happens inside COMPILE-EXPR, which is already past this decision, so
      ;; the expansion's comparisons were compiled as values however hard the
      ;; clauses above tried. Expanding here instead puts the LET and the BLOCK
      ;; the expansion is made of in front of those clauses.
      ((and (consp expr) (symbolp (car expr))
            (maybe-expand-inline (car expr) expr))
       ;; The expansion is taken once, here: the clause test above only asks
       ;; whether there is one. The recursion guard is pushed by the
       ;; expansion's BLOCK (INLINE-GUARD-STACK).
       (let ((inl (maybe-expand-inline (car expr) expr)))
         (let ((*at-toplevel* nil))
           (compile-boolean-branch inl label branch-on-true))))
      ;; (and ...) in boolean context
      ((and (consp expr) (eq (car expr) 'and) (cdr expr))
       (if branch-on-true
           ;; branch-on-true: all must be true -> chain brfalse to fail, then br to label
           (let ((fail-label (gen-label "ANDFAIL")))
             `(,@(loop for sub in (cdr expr)
                       append (compile-boolean-branch sub fail-label nil))
               (:br ,label)
               (:label ,fail-label)))
           ;; branch-on-false: any false -> branch to label
           `(,@(loop for sub in (cdr expr)
                     append (compile-boolean-branch sub label nil)))))
      ;; (or ...) in boolean context
      ((and (consp expr) (eq (car expr) 'or) (cdr expr))
       (if branch-on-true
           ;; branch-on-true: any true -> branch to label
           `(,@(loop for sub in (cdr expr)
                     append (compile-boolean-branch sub label t)))
           ;; branch-on-false: all must be false -> chain brtrue to pass, then br to label
           (let ((pass-label (gen-label "ORPASS")))
             `(,@(loop for sub in (cdr expr)
                       append (compile-boolean-branch sub pass-label t))
               (:br ,label)
               (:label ,pass-label)))))
      ;; A macro call the compiler has no handler for: branch on its expansion,
      ;; which may be one of the shapes above ((AND X T) and the like).
      ((%boolean-branch-macro-p expr)
       (let ((expanded (%macroexpand-for-compile expr (find-macro-expander (car expr)))))
         (if (eq expanded expr)
             (%boolean-branch-default expr label branch-on-true)
             (compile-boolean-branch expanded label branch-on-true))))
      ;; Default: IsTruthy
      (t (%boolean-branch-default expr label branch-on-true)))))

(defun compile-if (args &optional for-effect)
  "Compile (IF test then [else]).
   FOR-EFFECT: the value is discarded, so compile each arm for effect too and
   emit nothing where a missing ELSE would have produced NIL. Otherwise both arms
   have to leave a value at the branch merge, and an arm that assigns to a
   native-slot variable boxes it to do so -- for a value nobody reads. A SETQ
   inside a WHEN in a loop body is where that costs the most."
  (let ((else-label (gen-label "ELSE"))
        (end-label (gen-label "END"))
        (cond-expr (let ((c (%test-value-form (first args))))
                     (or (%typep-constant-rewrite c) c)))
        (fused-method nil))
   (flet ((then-arm ()
            (if for-effect
                (let ((*in-tail-position* nil) (*in-mv-context* nil))
                  (compile-and-pop (second args)))
                (compile-expr (second args))))
          (else-arm ()
            (cond ((third args)
                   (if for-effect
                       (let ((*in-tail-position* nil) (*in-mv-context* nil))
                         (compile-and-pop (third args)))
                       (compile-expr (third args))))
                  (for-effect (quote ()))
                  (t (let ((*in-tail-position* nil)) (emit-nil))))))
    ;; Check for fused comparison+branch optimization
    (cond
      ;; Fused comparison: skip IsTruthy (still reset MV)
      ((setq fused-method (compile-if-fused-comparison-p cond-expr))
       `(,@(let ((*in-tail-position* nil))
             (ecase (first fused-method)
               (:binary (compile-binary-call (third fused-method) (second fused-method)))
               (:unary (compile-unary-call (third fused-method) (second fused-method)))
               (:fixnum-cmp (compile-fixnum-cmp (third fused-method) (second fused-method)))
               (:char-cmp (compile-char-cmp (third fused-method) (second fused-method)))
               (:sym-eq (compile-sym-eq (third fused-method)))
               (:double-cmp (compile-double-cmp (third fused-method) (second fused-method)))
               (:struct-typep (compile-struct-typep
                               (first (third fused-method)) (second fused-method)))))
         (:brfalse ,else-label)
         ,@(then-arm)
         (:br ,end-label)
         (:label ,else-label)
         ,@(else-arm)
         (:label ,end-label)))
      ;; (if (and ...) then else): chain brfalse to else for each condition
      ((and (consp cond-expr) (eq (car cond-expr) 'and) (cdr cond-expr))
       `(,@(loop for sub in (cdr cond-expr)
                 append (compile-boolean-branch sub else-label nil))
         ,@(then-arm)
         (:br ,end-label)
         (:label ,else-label)
         ,@(else-arm)
         (:label ,end-label)))
      ;; (if (or ...) then else): chain brtrue to then for each, fall through to else
      ((and (consp cond-expr) (eq (car cond-expr) 'or) (cdr cond-expr))
       (let ((or-true (gen-label "ORTRUE")))
         `(,@(loop for (sub . rest) on (cdr cond-expr)
                   if rest
                     append (compile-boolean-branch sub or-true t)
                   else
                     append (compile-boolean-branch sub else-label nil))
           (:label ,or-true)
           ,@(then-arm)
           (:br ,end-label)
           (:label ,else-label)
           ,@(else-arm)
           (:label ,end-label))))
      ;; A binding form or a transparent BLOCK as the test. COMPILE-BOOLEAN-BRANCH
      ;; knows how to push the test inward; without this clause the test's value
      ;; is built and handed to IsTruthy, which is what an inlined predicate
      ;; (a LET around a BLOCK) made of every comparison.
      ((and (consp cond-expr)
            (or (and (member (car cond-expr) '(let let*))
                     (%boolean-branchable-let-p (cadr cond-expr) (cddr cond-expr)))
                (%locally-wrapper-p cond-expr)
                (and (eq (car cond-expr) 'block) (cddr cond-expr)
                     (not (%block-returns-to-p (cadr cond-expr) (cddr cond-expr))))
                ;; ...or a call the INLINE proclamation turns into one of those.
                (and (symbolp (car cond-expr))
                     (maybe-expand-inline (car cond-expr) cond-expr))
                (%boolean-branch-macro-p cond-expr)))
       `(,@(compile-boolean-branch cond-expr else-label nil)
         ,@(then-arm)
         (:br ,end-label)
         (:label ,else-label)
         ,@(else-arm)
         (:label ,end-label)))
      ;; Fused (not x) / (null x): negate the branch
      ((and (consp cond-expr)
            (symbolp (car cond-expr))
            (member (car cond-expr) '(not null))
            (= (length (cdr cond-expr)) 1))
       (let ((inner-fused (compile-if-fused-comparison-p (cadr cond-expr))))
         (cond
          ((and (not inner-fused) (%boolean-branch-macro-p (cadr cond-expr)))
           `(,@(compile-boolean-branch (cadr cond-expr) else-label t)
             ,@(then-arm)
             (:br ,end-label)
             (:label ,else-label)
             ,@(else-arm)
             (:label ,end-label)))
          (inner-fused
             ;; Inner expression is fusable: use fused comparison with negated branch
             `(,@(let ((*in-tail-position* nil))
                   (ecase (first inner-fused)
                     (:binary (compile-binary-call (third inner-fused) (second inner-fused)))
                     (:unary (compile-unary-call (third inner-fused) (second inner-fused)))
                     (:fixnum-cmp (compile-fixnum-cmp (third inner-fused) (second inner-fused)))
                     (:char-cmp (compile-char-cmp (third inner-fused) (second inner-fused)))
                     (:sym-eq (compile-sym-eq (third inner-fused)))
                     (:double-cmp (compile-double-cmp (third inner-fused) (second inner-fused)))
                     (:struct-typep (compile-struct-typep
                                     (first (third inner-fused)) (second inner-fused)))))
               (:brtrue ,else-label)  ;; Negate: branch to else when TRUE
               ,@(then-arm)
               (:br ,end-label)
               (:label ,else-label)
               ,@(else-arm)
               (:label ,end-label)))
          ;; Default: IsTruthy + negate
          (t
             `(,@(%truth-test-instrs (cadr cond-expr))
               (:call "MultipleValues.Reset")
               (:brtrue ,else-label)  ;; Negate: branch to else when TRUE (meaning (not x) is false)
               ,@(then-arm)
               (:br ,end-label)
               (:label ,else-label)
               ,@(else-arm)
               (:label ,end-label))))))
      ;; Default: general condition
      (t
       `(,@(%truth-test-instrs cond-expr)
         (:call "MultipleValues.Reset")
         (:brfalse ,else-label)
         ,@(then-arm)
         (:br ,end-label)
         (:label ,else-label)
         ,@(else-arm)
         (:label ,end-label)))))))

;;; ============================================================
;;; progn
;;; ============================================================

;;; --- Oversized-progn chunking ---
;;;
;;; The CLR imposes a per-method IL size limit (a few MB for a DynamicMethod). A
;;; progn/implicit-progn body with very many forms: e.g. a data-driven test
;;; suite that bundles 1000+ assertions into one (progn (is ...) ...); emits one
;;; oversized method that JITs to InvalidProgramException. Split such a body into
;;; groups, each wrapped in an immediately-called closure ((lambda () group...)),
;;; so every group compiles to its own method. Closures reuse the existing
;;; capture / boxing / non-local-exit machinery, so semantics are preserved.
;;;
;;; Only the leading forms are grouped; the last form stays inline so its tail
;;; position (self-TCO, multiple-value return) is unchanged.
;;;
;;; Refuse to split when a wrapped form could observe a semantic or codegen change:
;;;  - an enclosing lexical variable captured *by value* (non-boxed) and mutated
;;;    inside the progn: the closure would capture a stale copy; and
;;;  - any enclosing native raw-slot local (Int64 / native float / numeric array);
;;;    a closure capture loads a raw value where an object is required (invalid
;;;    IL). Refusing here is cheap: a progn large enough to need splitting is never
;;;    a hot native numeric loop.

(defvar *progn-chunk-threshold* 256
  "Split a progn body into closures when it has more than this many forms.")
(defvar *progn-chunk-size* 64
  "Leading forms per closure chunk when splitting an oversized progn body.")

(defun %list-longer-than-p (list n)
  "T if LIST has more than N proper elements. Tolerant of an improper (dotted)
   tail; compile-progn is occasionally handed one (e.g. tagbody segments), and
   LENGTH would signal on it where the LAST/BUTLAST path below does not."
  (do ((tail list (cdr tail))
       (i 0 (1+ i)))
      ((not (consp tail)) nil)
    (when (> i n) (return t))))

(defun %group-forms (forms n)
  "Partition FORMS into consecutive groups of at most N."
  (loop for tail = forms then (nthcdr n tail)
        while tail
        collect (loop repeat n for x in tail collect x)))

(defun %wrap-chunk (group)
  "Wrap GROUP (a list of forms) as an immediately-applied nullary lambda
   ((lambda () form...)); compiled to its own method, so the group's CIL lands
   outside the enclosing method's IL budget."
  (list (list* 'lambda nil group)))

(defun %chunk-progn-forms (forms)
  "Rewrite oversized FORMS into leading closure chunks plus the inline last form."
  (let ((head (butlast forms))
        (last-form (car (last forms))))
    (append (mapcar #'%wrap-chunk (%group-forms head *progn-chunk-size*))
            (list last-form))))

(defun %progn-chunk-safe-p (forms)
  "T if the enclosing scope permits wrapping FORMS in closures without changing
   semantics or emitting invalid IL; see the section comment."
  (and (null (cstate-long-locals))
       (null (cstate-native-double-locals))
       (null (cstate-native-single-locals))
       (null (cstate-native-decimal-locals))
       (null (cstate-native-char-locals))
       (null (cstate-u64-locals))
       (null (cstate-simd-locals))
       (null (cstate-numeric-array-locals))
       (let ((value-captured
               (loop for pair in (cstate-locals)
                     for sym = (car pair)
                     unless (boxed-var-p sym) collect (var-name sym))))
         (or (null value-captured)
             (null (intersection
                    (nth-value 0 (find-mutated-and-captured-vars forms value-captured))
                    value-captured :test #'string=))))))

;;; A LET body gets its chunking decision earlier than a bare progn does, and
;;; that timing is the whole point. By the time COMPILE-PROGN runs on a LET body,
;;; the LET has already decided which of its own variables are boxed; so a body
;;; that assigns to one of them is stuck: the variable is a plain slot, a closure
;;; would capture a stale copy, and %PROGN-CHUNK-SAFE-P correctly refuses. That
;;; refusal is what left coalton's (let ((env ..)) (setf ..) x1575) as a single
;;; 17 MB method.
;;;
;;; Rewriting the body into closure chunks BEFORE %COMPILE-LET runs its
;;; capture/mutation scan removes the obstacle instead of overriding it: the scan
;;; then sees the lambdas, finds the variable both captured and mutated, and
;;; boxes it through the existing machinery. No new sharing rules: the chunks
;;; get the same boxed cell any other closure would.
;;;
;;; The enclosing scope still has to pass %PROGN-CHUNK-SAFE-P: variables bound
;;; further out already have their boxing fixed, so a chunk that mutates one of
;;; those would still capture a stale copy. At this point CSTATE-LOCALS holds
;;; exactly those outer variables, this LET's own are not registered yet, so
;;; the existing predicate asks precisely the right question here.

(defun %let-own-native-locals-p (body)
  "T if BODY's declarations give this LET a raw native slot (Int64, native
   float, decimal, numeric array). Those are invisible to CSTATE-LOCALS at
   %COMPILE-LET time because the LET has not established them yet, and a closure
   capturing a raw slot loads a value where an object is required (invalid IL)."
  (or (extract-fixnum-locals body)
      (extract-small-int-locals body)
      (extract-double-float-locals body)
      (extract-single-float-locals body)
      (extract-decimal-locals body)
      (extract-simd-locals body)
      (extract-numeric-array-locals body)))

(defun %maybe-chunk-let-body (real-body body)
  "Rewrite an oversized LET body into leading closure chunks, ahead of the
   capture/mutation analysis. REAL-BODY is BODY minus its declarations."
  (if (and (not *at-toplevel*)
           (%list-longer-than-p real-body *progn-chunk-threshold*)
           (%progn-chunk-safe-p real-body)
           (not (%let-own-native-locals-p body)))
      (%chunk-progn-forms real-body)
      real-body))

;;; A TOPLEVEL progn cannot be chunked into closures (see COMPILE-PROGN), but
;;; under EVAL it does not have to be one method either: its forms share no
;;; lexical variables, so they can run as consecutive methods. A macro that
;;; expands, at top level, into thousands of forms (April's test suite is
;;; (progn (plan 1140) (with-april-context ... <2672 forms>) ...), and
;;; WITH-APRIL-CONTEXT expands to a PROGN) is cut into groups separated by
;;; (:TOPLEVEL-BOUNDARY). COMPILE-TOPLEVEL-EVAL closes each group into a method
;;; of its own. The groups are compiled before any of them runs, exactly as the
;;; one method was, and each form stays a top-level form.

(defun %compile-segmented-toplevel-progn (forms)
  (let ((groups (%group-forms forms *progn-chunk-size*)))
    (append
     (loop for group in (butlast groups)
           append `(,@(let ((*in-tail-position* nil)
                            (*in-mv-context* nil))
                        (compile-progn group))
                    (:pop)
                    (:call "MultipleValues.Reset")
                    (:toplevel-boundary)))
     (compile-progn (car (last groups))))))

(defun %close-toplevel-segments (instrs)
  "Split INSTRS at (:TOPLEVEL-BOUNDARY) and end each piece as a method body:
   the leading pieces return NIL (their value is discarded), the last one returns
   its own value. Without a boundary this is (,@INSTRS (:RET))."
  (let ((segments '()) (current '()))
    (dolist (instr instrs)
      (if (and (consp instr) (eq (car instr) :toplevel-boundary))
          (progn (push (nreverse current) segments) (setf current '()))
          (push instr current)))
    (push (nreverse current) segments)
    (loop for (segment . more) on (nreverse segments)
          append `(,@(peephole-optimize segment)
                   ,@(when more (emit-nil))
                   (:ret)
                   ,@(when more '((:toplevel-boundary)))))))

(defun %compile-discarded-body (forms)
  "FORMS, a body whose value is discarded: every form for effect. The
   multiple-values channel is reset where COMPILE-PROGN would reset it (before
   the last form, after a statement that can touch it); the last form leaves
   the channel as it would have as the body's value."
  (when forms
    (let ((*in-tail-position* nil)
          (*in-mv-context* nil))
      `(,@(%join-statements
           (loop for form in (butlast forms)
                 collect (cons (compile-and-pop form)
                               (if (and (consp form) (member (car form) '(%check-simple-decl %check-arg-decl %tco-checks-done)))
                                   t
                                   :default))))
        ,@(compile-and-pop (car (last forms)))))))

(defun %compile-discarded-block (form)
  "(BLOCK name . body) for effect (see COMPILE-AND-POP): the body as statements
   with no result slot, when no RETURN-FROM naming the block is visible and the
   body is short enough not to be chunked. A RETURN-FROM a macro produces aborts
   to the ordinary BLOCK, as in a test position (*TRANSPARENT-BLOCKS*)."
  (let ((name (cadr form)) (body (cddr form)))
    (if (or (%list-longer-than-p body *progn-chunk-threshold*)
            (%block-returns-to-p name body))
        `(,@(compile-expr form) (:pop))
        (let* ((tag (list '#:transparent-block))
               (result (let ((*transparent-blocks*
                               (cons (list* name tag (%block-identity name))
                                     *transparent-blocks*))
                             (*inlining-stack* (inline-guard-stack name)))
                         (catch tag
                           (%compile-discarded-body body)))))
          (if (eq result :transparent-block-aborted)
              `(,@(compile-expr form) (:pop))
              result)))))

(defun compile-progn (forms)
  (cond
    ((null forms) (emit-nil))
    ;; Single form: inherits *in-tail-position* from caller
    ((null (cdr forms)) (compile-expr (car forms)))
    ;; Oversized toplevel body under EVAL: consecutive methods (above).
    ((and *at-toplevel*
          *toplevel-segments-p*
          (%list-longer-than-p forms *progn-chunk-threshold*))
     (%compile-segmented-toplevel-progn forms))
    ;; Oversized non-toplevel body: split into closure chunks (below). A TOPLEVEL
    ;; progn must NOT be chunked: its forms are definitions (defun/defvar/
    ;; defmacro/eval-when) whose top-level processing (compile-time side effects,
    ;; :toplevel-boundary, macro registration) would be lost inside a lambda. The
    ;; whole dotcl core is cross-compiled as one giant toplevel progn, so chunking
    ;; it corrupts everything (e.g. the loop keyword universe). Toplevel progns
    ;; already have their own :toplevel-boundary splitting.
    ((and (not *at-toplevel*)
          (%list-longer-than-p forms *progn-chunk-threshold*)
          (%progn-chunk-safe-p forms))
     (compile-progn (%chunk-progn-forms forms)))
    (t (append
        ;; Non-last forms: never in tail position, and their values are
        ;; discarded so an MV-propagating context (e.g. block value position)
        ;; must not leak into them: subforms would skip UnwrapMv and leak
        ;; raw MvReturn objects into single-value positions.
        (let ((*in-tail-position* nil)
              (*in-mv-context* nil)
              ;; Whether an earlier form's compile-time side effect has to reach the
              ;; compilation of the later ones. Read before COMPILE-AND-POP rebinds
              ;; *AT-TOPLEVEL* for the subform.
              (ct-order-p (and *at-toplevel* (not *cross-compiling*))))
          (%join-statements
           (loop for form in (butlast forms)
                collect (let ((instrs
                               (cons (compile-and-pop form)
                                     ;; Produces no values to reset, and when it is
                                     ;; elided it must leave nothing behind at all.
                                     (if (and (consp form) (member (car form) '(%check-simple-decl %check-arg-decl %tco-checks-done)))
                                         t
                                         :default))))
                         ;; After this form is compiled, before the next one is:
                         ;; the next form's macroexpansion has to see the effect.
                         ;; Cross-compilation is excluded: the core is compiled as
                         ;; one giant toplevel progn under a driver that manages the
                         ;; host package itself, and perturbing that is a separate
                         ;; question from CLHS conformance for user code.
                         (when (and ct-order-p (%toplevel-ct-eval-form-p form))
                           (try-eval form))
                         instrs))))
        ;; Last form: inherits outer *in-tail-position*
        (compile-expr (car (last forms)))))))
