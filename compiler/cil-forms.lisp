;;; cil-forms.lisp — Special forms, functions, control flow
;;; Part of the CIL compiler (A2 instruction list architecture)

(in-package :dotcl.cil-compiler)

;;; Per-compilation dynamic state (TCO state, typed-local tracking, etc.) is
;;; declared in cil-compiler.lisp — see the
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
   otherwise :BOX (a boxed cell) or :LONG/:DOUBLE/:SINGLE/:DECIMAL (a native-rep slot)."
  (when (and (symbolp var) (symbol-package var))
    (append
     (when *emit-source-lines*
       `((:local-var ,key ,(var-name var))))
     (%frame-set-instrs key var rep))))

(defun %frame-set-instrs (key var &optional rep)
  "Under *EMIT-FRAME-LOCALS*, the store recording slot KEY's current value in the
   body's DebugFrame under source variable VAR's name; otherwise NIL. Emitted right
   after every write to the slot — the binding (via %LOCAL-VAR-MARKER) and each
   later assignment — because the frame holds values, not slots: without the
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
         (:decimal :frame-set-decimal))
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
   dominate every store in the method — hence the head of the body, before the TCO
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
      `((:declare-local ,key "LispObject[]")
        (:ldc-i4 1) (:newarr "LispObject") (:dup)
        (:ldc-i4 0) ,@value-instrs
        (:stelem-ref) (:stloc ,key)
        ,@(%local-var-marker key var :box))))

(defun %synthetic-capture-name-p (name)
  "True if NAME is a compiler-synthesized captured slot — a labels/flet function
   cell (__LABELFN_), a block tag (%BTAG-), or a tagbody id (%TBID-) — rather
   than a user variable. Such slots are excluded from the debugger's Locals; only
   real captured variables are named there."
  (flet ((pfx (p) (let ((n (length p)))
                    (and (>= (length name) n) (string= name p :end1 n)))))
    (or (labels-cell-var-p name)
        (pfx "%BTAG-")
        (pfx "%TBID-"))))

(defun maybe-tail-callvirt (instrs)
  "Post-pass for compile-function-body-direct: if INSTRS ends with (:callvirt ...),
  insert (:tail-prefix) immediately before it. Only called when there is no
  try/finally wrapping the body (special-param-syms is nil), so the sequence
  (:tail-prefix) (:callvirt ...) (:ret) is valid CIL."
  (let ((last (and (consp instrs) (car (last instrs)))))
    (if (and (consp last) (eq (car last) :callvirt))
        (append (butlast instrs 1) '((:tail-prefix)) (list last))
        instrs)))

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
  (let ((op (and (consp form) (symbolp (car form)) (car form))))
    (cond
      ;; Direct vector-push-extend in for-effect position: use void variant (no Fixnum.Make)
      ((and (eq op 'vector-push-extend) (= (length (cdr form)) 2))
       (compile-binary-call (cdr form) "Runtime.VectorPushExtendVoid2"))
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
      (t `(,@(compile-expr form) (:pop))))))

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
  "Like compile-direct-call-args but evaluates each arg via compile-as-long → Int64.
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
   statically resolvable — an odd or dotted argument list, a key that is not a
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
          (let* ((reordered (not (equal written
                                        (remove-if-not (lambda (k) (member k written))
                                                       keywords))))
                 (temps (when reordered
                          (loop for s in supplied
                                collect (cons (car s) (gensym "SV")))))
                 (values (loop for k in keywords
                               for init in initforms
                               collect (let ((cell (assoc k supplied)))
                                         (cond ((null cell) init)
                                               (reordered (cdr (assoc k temps)))
                                               (t (cdr cell))))))
                 (call `(%make-struct ',struct-name ,@values)))
            (if reordered
                `(let* ,(loop for s in supplied
                              collect (list (cdr (assoc (car s) temps)) (cdr s)))
                   ,call)
                call)))))))

(defun compile-named-call (name args)
  ;; xref: single choke point for every named call — all the fast-path
  ;; return-froms below (self-call, CLOS reader/writer IC, struct accessor,
  ;; local flet/labels) are branches INSIDE this function, so recording at
  ;; entry catches them all.
  (xref-record-call name)
  (block compile-named-call
    ;; Self-TCO: if in tail position and calling current function, emit loop
    ;; Use symbol identity (eq) not just name string to avoid cross-package false matches
    ;; (e.g. uiop/os:getenv calling dotcl:getenv must not be treated as self-recursion)
    ;; Skip TCO when a local function (flet/labels) shadows the defun name
    (let ((name-str (mangle-name name))
          (n-args (length args)))
      (when (and *in-tail-position*
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
                 (= n-args (length (cstate-tco-param-entries))))
        (let* ((use-native-tco (and (cstate-native-self-name)
                                    (every #'fixnum-typed-p args)))
               (da (if use-native-tco
                       (compile-direct-call-args-long args)
                       (compile-direct-call-args args)))
               (temps (car da))
               (eval-instrs (cdr da))
               (store-instrs
                 (if use-native-tco
                     ;; Native body: all params are Int64, temps are Int64 → direct store
                     (loop for tmp in temps
                           for (key . boxed-p) in (cstate-tco-param-entries)
                           append `((:ldloc ,tmp) (:stloc ,key)
                                    ,@(%frame-set-instrs-for-key key :long)))
                     ;; Normal body: LispObject temps → LispObject or boxed params
                     (loop for tmp in temps
                           for (key . boxed-p) in (cstate-tco-param-entries)
                           if boxed-p
                             append (if *emit-source-lines*
                                        `((:ldloc ,key) (:ldloc ,tmp) (:stfld "LispBox.Value"))
                                        `((:ldloc ,key) (:ldc-i4 0) (:ldloc ,tmp) (:stelem-ref)))
                           else
                             ;; A tail self-call rebinds the parameters in place and
                             ;; loops, staying in one frame — so the debug frame has
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
              ,(if (cstate-tco-in-try-catch)
                   `(:leave ,(cstate-tco-loop-label))
                   `(:br ,(cstate-tco-loop-label)))))))
      ;; Mutual-TCO: tail call to a labels sibling → update shared params + br TCOLOOP
      (when (and *in-tail-position*
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
      ;; Non-tail self-call fast path: reuse LispFunction cached at body entry.
      (when (and (cstate-self-fn-local)
                 (not (local-function-entry name-str))
                 (if (cstate-tco-self-symbol)
                     (eq name (cstate-tco-self-symbol))
                     (string= name-str (cstate-tco-self-name)))
                 (<= n-args 8))
        (let* ((skip-reset (single-value-form-p (cons name args)))
               (da (compile-direct-call-args args))
               (temps (car da))
               (eval-instrs (cdr da)))
          (return-from compile-named-call
            `(,@eval-instrs
              ,(if (eq (cstate-self-fn-local) :arg0) '(:ldarg 0)
                   `(:ldloc ,(cstate-self-fn-local)))
              ,@(unless skip-reset '((:call "MultipleValues.Reset")))
              ,@(loop for tmp in temps append `((:ldloc ,tmp)))
              (:callvirt ,(invoke-name n-args)))))))
    ;; Inline struct accessor: (accessor-name obj) → StructRefI with raw int index
    ;; Only when not shadowed by a local function (flet/labels)
    (when (and (symbolp name)
               (= (length args) 1)
               (not (local-function-entry name)))
      (let ((slot-idx (gethash name *struct-accessors*)))
        (when slot-idx
          (return-from compile-named-call
            `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                  (compile-expr (car args)))
              (:ldc-i4 ,slot-idx)
              (:call "Runtime.StructRefI"))))))
    ;; Keyword struct constructor called with constant keywords: build the instance
    ;; here instead of calling it. The &key call is what costs — the argument array
    ;; and the keyword scan — while the constructor body is one %MAKE-STRUCT of the
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
    ;; Inline CLOS simple-reader accessor: (reader obj) → Runtime.ReaderIC(obj, cell),
    ;; a per-call-site monomorphic inline cache. On a warm hit it reads the slot straight
    ;; from the instance vector (no GF resolution, no dispatch, no name→index lookup); any
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
                      ,(package-name (symbol-package name))))))
    ;; Inline CLOS simple-writer accessor: ((setf writer) newval obj) →
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
                        ,(package-name (symbol-package accessor)))))))
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
                  (:callvirt ,(invoke-name n-args))))
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
        ;; here — it must NOT funcall the variable's cell. Boxed local *functions*
        ;; (flet/labels, including ones captured into a closure and re-established
        ;; in *local-functions*) are handled by the local-fn branch above. So
        ;; always take the global-function path.
        ;; Global function — use symbol-based lookup (fixes flat namespace
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
                    (if (and (symbolp name) (every #'simple-expr-p args))
                        ;; Fast path: simple args → skip temps, push directly
                        `(,@load-fn
                          ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                          ,@(loop for arg in args
                                  append (let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr arg)))
                          (:callvirt ,(invoke-name n-args)))
                        (let* ((da (compile-direct-call-args args))
                               (temps (car da))
                               (eval-instrs (cdr da)))
                          `(,@eval-instrs
                            ,@load-fn
                            ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                            ,@(loop for tmp in temps append `((:ldloc ,tmp)))
                            (:callvirt ,(invoke-name n-args)))))
                    `((:declare-local ,args-tmp "LispObject[]")
                      ,@(compile-args-array args) (:stloc ,args-tmp)
                      ,@load-fn
                      ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                      (:ldloc ,args-tmp)
                      (:callvirt "LispFunction.Invoke"))))))))


;;; ============================================================
;;; if
;;; ============================================================

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
        ;; Special case: (= x 0) or (= 0 x) → zerop optimization
        ((and (eq op '=) (= nargs 2)
              (or (eql (second cond-expr) 0) (eql (third cond-expr) 0)))
         (let ((non-zero-arg (if (eql (second cond-expr) 0) (third cond-expr) (second cond-expr))))
           (list :unary "Runtime.IsTrueZerop" (list non-zero-arg))))
        ;; Float-typed fast path: both args statically float → native r8 compare.
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
        ;; Fixnum-typed fast path: both args statically fixnum → native i8 compare.
        ;; Emitted as compile-fixnum-cmp which leaves an i4 (0/1) on stack.
        ((and (= nargs 2)
              (member op '(< > <= >= = /=))
              (fixnum-typed-p (first args))
              (fixnum-typed-p (second args)))
         (list :fixnum-cmp
               (ecase op (< :lt) (> :gt) (<= :le) (>= :ge) (= :eq) (/= :ne))
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
        ;; Fixnum-typed unary sign predicate → native i8 compare against 0.
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
        ;; (eq x 'sym) / (eql x 'sym) against a symbol literal → inline
        ;; reference comparison instead of a call. CASE expands to a linear
        ;; COND of these, so a dispatch pays one call per clause it walks past
        ;; (measured ~25 ns each; %MINI-EVAL's 32-key dispatch pays ~0.77 us
        ;; before it reaches the function-call default).
        ((and (= nargs 2)
              (member op '(eq eql))
              (or (literal-symbol-operand (first args))
                  (literal-symbol-operand (second args))))
         (list :sym-eq nil args))
        ;; Binary equality: eq, eql, equal
        ((and (= nargs 2)
              (member op '(eq eql equal)))
         (list :binary (cdr (assoc op '((eq . "Runtime.IsTrueEq")
                                        (eql . "Runtime.IsTrueEql")
                                        (equal . "Runtime.IsTrueEqual")))) args))
        ;; Unary type predicates: consp, atom, null (as predicate, not as not)
        ((and (= nargs 1)
              (member op '(consp atom listp numberp integerp symbolp stringp
                           characterp functionp)))
         (list :unary (cdr (assoc op '((consp . "Runtime.IsTrueConsp")
                                       (atom . "Runtime.IsTrueAtom")
                                       (listp . "Runtime.IsTrueListp")
                                       (numberp . "Runtime.IsTrueNumberp")
                                       (integerp . "Runtime.IsTrueIntegerp")
                                       (symbolp . "Runtime.IsTrueSymbolp")
                                       (stringp . "Runtime.IsTrueStringp")
                                       (characterp . "Runtime.IsTrueCharacterp")
                                       (functionp . "Runtime.IsTrueFunctionp")))) args))
        ;; (typep x 'known-type) → IsTrueXxx fused predicate
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

(defun compile-boolean-branch (expr label branch-on-true)
  "Compile expr as a boolean condition and emit a branch to label.
   If branch-on-true, branch when condition is true (brtrue).
   Otherwise, branch when condition is false (brfalse).
   Handles fused comparisons, (not x), nested (and ...) / (or ...) recursively."
  (let ((fused (compile-if-fused-comparison-p expr)))
    (cond
      ;; Fused comparison: IsTrueXxx → bool → branch
      (fused
       `(,@(let ((*in-tail-position* nil))
             (ecase (first fused)
               (:binary (compile-binary-call (third fused) (second fused)))
               (:unary (compile-unary-call (third fused) (second fused)))
               (:fixnum-cmp (compile-fixnum-cmp (third fused) (second fused)))
               (:sym-eq (compile-sym-eq (third fused)))
               (:double-cmp (compile-double-cmp (third fused) (second fused)))))
         (,(if branch-on-true :brtrue :brfalse) ,label)))
      ;; (not x) / (null x): negate direction and recurse
      ((and (consp expr)
            (symbolp (car expr))
            (member (car expr) '(not null))
            (= (length (cdr expr)) 1)
            (not (local-function-entry (car expr))))
       (compile-boolean-branch (cadr expr) label (not branch-on-true)))
      ;; (and ...) in boolean context
      ((and (consp expr) (eq (car expr) 'and) (cdr expr))
       (if branch-on-true
           ;; branch-on-true: all must be true → chain brfalse to fail, then br to label
           (let ((fail-label (gen-label "ANDFAIL")))
             `(,@(loop for sub in (cdr expr)
                       append (compile-boolean-branch sub fail-label nil))
               (:br ,label)
               (:label ,fail-label)))
           ;; branch-on-false: any false → branch to label
           `(,@(loop for sub in (cdr expr)
                     append (compile-boolean-branch sub label nil)))))
      ;; (or ...) in boolean context
      ((and (consp expr) (eq (car expr) 'or) (cdr expr))
       (if branch-on-true
           ;; branch-on-true: any true → branch to label
           `(,@(loop for sub in (cdr expr)
                     append (compile-boolean-branch sub label t)))
           ;; branch-on-false: all must be false → chain brtrue to pass, then br to label
           (let ((pass-label (gen-label "ORPASS")))
             `(,@(loop for sub in (cdr expr)
                       append (compile-boolean-branch sub pass-label t))
               (:br ,label)
               (:label ,pass-label)))))
      ;; Default: IsTruthy
      (t
       `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr expr))
         (:call "Runtime.IsTruthy")
         (:call "MultipleValues.Reset")
         (,(if branch-on-true :brtrue :brfalse) ,label))))))

(defun compile-if (args &optional for-effect)
  "Compile (IF test then [else]).
   FOR-EFFECT: the value is discarded, so compile each arm for effect too and
   emit nothing where a missing ELSE would have produced NIL. Otherwise both arms
   have to leave a value at the branch merge, and an arm that assigns to a
   native-slot variable boxes it to do so -- for a value nobody reads. A SETQ
   inside a WHEN in a loop body is where that costs the most."
  (let ((else-label (gen-label "ELSE"))
        (end-label (gen-label "END"))
        (cond-expr (first args))
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
               (:sym-eq (compile-sym-eq (third fused-method)))
               (:double-cmp (compile-double-cmp (third fused-method) (second fused-method)))))
         (:brfalse ,else-label)
         ,@(then-arm)
         (:br ,end-label)
         (:label ,else-label)
         ,@(else-arm)
         (:label ,end-label)))
      ;; (if (and ...) then else): chain brfalse to else for each condition
      ((and (consp cond-expr) (eq (car cond-expr) 'and) (cddr cond-expr))
       `(,@(loop for sub in (cdr cond-expr)
                 append (compile-boolean-branch sub else-label nil))
         ,@(then-arm)
         (:br ,end-label)
         (:label ,else-label)
         ,@(else-arm)
         (:label ,end-label)))
      ;; (if (or ...) then else): chain brtrue to then for each, fall through to else
      ((and (consp cond-expr) (eq (car cond-expr) 'or) (cddr cond-expr))
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
      ;; Fused (not x) / (null x): negate the branch
      ((and (consp cond-expr)
            (symbolp (car cond-expr))
            (member (car cond-expr) '(not null))
            (= (length (cdr cond-expr)) 1))
       (let ((inner-fused (compile-if-fused-comparison-p (cadr cond-expr))))
         (if inner-fused
             ;; Inner expression is fusable: use fused comparison with negated branch
             `(,@(let ((*in-tail-position* nil))
                   (ecase (first inner-fused)
                     (:binary (compile-binary-call (third inner-fused) (second inner-fused)))
                     (:unary (compile-unary-call (third inner-fused) (second inner-fused)))
                     (:fixnum-cmp (compile-fixnum-cmp (third inner-fused) (second inner-fused)))
                     (:sym-eq (compile-sym-eq (third inner-fused)))
                     (:double-cmp (compile-double-cmp (third inner-fused) (second inner-fused)))))
               (:brtrue ,else-label)  ;; Negate: branch to else when TRUE
               ,@(then-arm)
               (:br ,end-label)
               (:label ,else-label)
               ,@(else-arm)
               (:label ,end-label))
             ;; Default: IsTruthy + negate
             `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (cadr cond-expr)))
               (:call "Runtime.IsTruthy")
               (:call "MultipleValues.Reset")
               (:brtrue ,else-label)  ;; Negate: branch to else when TRUE (meaning (not x) is false)
               ,@(then-arm)
               (:br ,end-label)
               (:label ,else-label)
               ,@(else-arm)
               (:label ,end-label)))))
      ;; Default: general condition
      (t
       `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr cond-expr))
         (:call "Runtime.IsTruthy")
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
;;; progn/implicit-progn body with very many forms — e.g. a data-driven test
;;; suite that bundles 1000+ assertions into one (progn (is ...) ...) — emits one
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
;;;    inside the progn — the closure would capture a stale copy; and
;;;  - any enclosing native raw-slot local (Int64 / native float / numeric array)
;;;    — a closure capture loads a raw value where an object is required (invalid
;;;    IL). Refusing here is cheap: a progn large enough to need splitting is never
;;;    a hot native numeric loop.

(defvar *progn-chunk-threshold* 256
  "Split a progn body into closures when it has more than this many forms.")
(defvar *progn-chunk-size* 64
  "Leading forms per closure chunk when splitting an oversized progn body.")

(defun %list-longer-than-p (list n)
  "T if LIST has more than N proper elements. Tolerant of an improper (dotted)
   tail — compile-progn is occasionally handed one (e.g. tagbody segments), and
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
   ((lambda () form...)) — compiled to its own method, so the group's CIL lands
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
   semantics or emitting invalid IL — see the section comment."
  (and (null (cstate-long-locals))
       (null (cstate-native-double-locals))
       (null (cstate-native-single-locals))
       (null (cstate-native-decimal-locals))
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
;;; the LET has already decided which of its own variables are boxed — so a body
;;; that assigns to one of them is stuck: the variable is a plain slot, a closure
;;; would capture a stale copy, and %PROGN-CHUNK-SAFE-P correctly refuses. That
;;; refusal is what left coalton's (let ((env ..)) (setf ..) x1575) as a single
;;; 17 MB method.
;;;
;;; Rewriting the body into closure chunks BEFORE %COMPILE-LET runs its
;;; capture/mutation scan removes the obstacle instead of overriding it: the scan
;;; then sees the lambdas, finds the variable both captured and mutated, and
;;; boxes it through the existing machinery. No new sharing rules — the chunks
;;; get the same boxed cell any other closure would.
;;;
;;; The enclosing scope still has to pass %PROGN-CHUNK-SAFE-P: variables bound
;;; further out already have their boxing fixed, so a chunk that mutates one of
;;; those would still capture a stale copy. At this point CSTATE-LOCALS holds
;;; exactly those outer variables — this LET's own are not registered yet — so
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
      (extract-float-array-locals body)))

(defun %maybe-chunk-let-body (real-body body)
  "Rewrite an oversized LET body into leading closure chunks, ahead of the
   capture/mutation analysis. REAL-BODY is BODY minus its declarations."
  (if (and (not *at-toplevel*)
           (%list-longer-than-p real-body *progn-chunk-threshold*)
           (%progn-chunk-safe-p real-body)
           (not (%let-own-native-locals-p body)))
      (%chunk-progn-forms real-body)
      real-body))

(defun compile-progn (forms)
  (cond
    ((null forms) (emit-nil))
    ;; Single form — inherits *in-tail-position* from caller
    ((null (cdr forms)) (compile-expr (car forms)))
    ;; Oversized non-toplevel body: split into closure chunks (below). A TOPLEVEL
    ;; progn must NOT be chunked — its forms are definitions (defun/defvar/
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
        ;; must not leak into them — subforms would skip UnwrapMv and leak
        ;; raw MvReturn objects into single-value positions.
        (let ((*in-tail-position* nil)
              (*in-mv-context* nil)
              ;; Whether an earlier form's compile-time side effect has to reach the
              ;; compilation of the later ones. Read before COMPILE-AND-POP rebinds
              ;; *AT-TOPLEVEL* for the subform.
              (ct-order-p (and *at-toplevel* (not *cross-compiling*))))
          (loop for form in (butlast forms)
                append (let ((instrs `(,@(compile-and-pop form)
                                        (:call "MultipleValues.Reset"))))
                         ;; After this form is compiled, before the next one is:
                         ;; the next form's macroexpansion has to see the effect.
                         ;; Cross-compilation is excluded — the core is compiled as
                         ;; one giant toplevel progn under a driver that manages the
                         ;; host package itself, and perturbing that is a separate
                         ;; question from CLHS conformance for user code.
                         (when (and ct-order-p (%toplevel-ct-eval-form-p form))
                           (try-eval form))
                         instrs)))
        ;; Last form: inherits outer *in-tail-position*
        (compile-expr (car (last forms)))))))

;;; ============================================================
;;; defvar / defparameter
;;; ============================================================

(defun compile-defvar (name init-form has-init-p &optional (is-defvar t) (is-defconstant nil) docstring)
  "Compile (defvar name value [doc]) or (defparameter name value [doc]) or (defconstant name value [doc]).
   Returns the symbol name. Stores docstring if provided."
  ;; Register as globally special at compile time
  (pushnew name *specials*)
  (pushnew name *global-specials*)
  (let ((val-local (gen-local "DEFVAR-VAL"))
        (sym-local (gen-local "DEFVAR-SYM")))
    `((:declare-local ,val-local "LispObject")
      ;; Symbol (not LispObject): the symbol is castclass'd to Symbol below, and
      ;; DynamicBindings.Set's first parameter is Symbol. Declaring the local as
      ;; LispObject would widen it back, emitting an unverifiable covariant call
      ;; (LispObject passed where Symbol is required) that CoreCLR/NativeAOT JIT
      ;; tolerate but Unity IL2CPP's strict C++ codegen rejects. All other uses of
      ;; this local pass it to LispObject parameters, where Symbol upcasts cleanly.
      (:declare-local ,sym-local "Symbol")
      ,@(compile-sym-lookup name)
      (:castclass "Symbol")
      (:stloc ,sym-local)
      ,@(if has-init-p
            (let ((skip-label (gen-label "DEFVAR-SKIP")))
              `(,@(if is-defvar
                      ;; defvar: only evaluate and set init-form if unbound
                      `((:ldloc ,sym-local)
                        (:call "Runtime.Boundp")
                        (:call "Runtime.IsTruthy")
                        (:brtrue ,skip-label)
                        ,@(compile-expr init-form)
                        (:stloc ,val-local)
                        (:ldloc ,sym-local)
                        (:ldloc ,val-local)
                        (:call "DynamicBindings.Set")
                        (:pop)
                        (:label ,skip-label))
                      ;; defparameter: always set. defconstant goes through
                      ;; DefineConstant, which sets AND marks the symbol, and
                      ;; refuses a re-definition to a non-EQL value (CLHS
                      ;; 11.1.2.1.2 -- code compiled against the old value would
                      ;; disagree with the new one).
                      `(,@(compile-expr init-form)
                        (:stloc ,val-local)
                        (:ldloc ,sym-local)
                        (:ldloc ,val-local)
                        (:call ,(if is-defconstant "Runtime.DefineConstant" "DynamicBindings.Set"))
                        (:pop)))))
            '())
      ;; Mark the symbol as special at runtime (needed for self-hosting)
      (:ldloc ,sym-local)
      (:call "Runtime.MarkSpecial")
      (:pop)
      ,@(if (and docstring (stringp docstring))
            `((:ldloc ,sym-local)
              (:ldstr ,docstring)
              (:newobj "LispString")
              (:call "Runtime.SetVariableDocumentation")
              (:pop))
            '())
      (:ldloc ,sym-local))))

;;; ============================================================
;;; defun
;;; ============================================================

(defun form-has-return-from-p (name form)
  "Check if FORM contains (return-from NAME ...) anywhere in the tree.
   Stops descending into nested blocks/defuns that shadow NAME."
  (cond
    ((atom form) nil)
    ;; (return-from X ...) — match if X eq NAME
    ((and (eq (car form) 'return-from)
          (consp (cdr form))
          (eq (cadr form) name))
     t)
    ;; (block NAME ...) shadows our block name — don't descend
    ((and (eq (car form) 'block)
          (consp (cdr form))
          (eq (cadr form) name))
     nil)
    ;; (defun NAME ...) also creates an implicit block — don't descend
    ((and (eq (car form) 'defun)
          (consp (cdr form))
          (eq (cadr form) name))
     nil)
    ;; (quote X) is data, not code — never descend. X may be a circular literal
    ;; (e.g. '#1=(1 2 3 . #1#)); walking it would loop forever.
    ((eq (car form) 'quote) nil)
    ;; Recurse into subforms (handle dotted pairs safely)
    (t (do ((cur form (cdr cur)))
           ((atom cur) (when cur (form-has-return-from-p name cur)))
         (when (form-has-return-from-p name (car cur))
           (return t))))))

(defun %lambda-list-init-forms (lambda-list)
  "The default-value initializer forms in an ordinary LAMBDA-LIST — the only code
   positions it contains. Parameter names (required, &rest, and supplied-p names)
   are binding occurrences, not code, so they are excluded. Returns NIL for a
   non-list. Used by the return-from scanners so they never macroexpand a
   parameter name, which would fire a same-named macro's compile-time side
   effects (e.g. a required param named INST under SBCL's assembler)."
  (let ((state :required) (forms '()))
    (do ((cur lambda-list (cdr cur)))
        ((not (consp cur)) (nreverse forms))
      (let ((p (car cur)))
        (cond
          ((member p '(&optional &rest &body &key &aux &allow-other-keys
                       &whole &environment))
           (setf state p))
          ((and (consp p) (member state '(&optional &key &aux)) (cadr p))
           (push (cadr p) forms)))))))

(defun %scan-code-forms (fn forms)
  "Apply FN to each element of FORMS, returning the first true result. Unlike
   SOME, tolerates an improper or non-list FORMS: the scanners walk unevaluated
   subforms too, so a binding special form's binding-list or body position is
   not guaranteed to hold a proper list. A DOLIST spec whose variable is named
   LET or LABELS, for instance, reads as (LET <list-form>) — indistinguishable
   by shape from a LET whose binding list is a symbol."
  (do ((cur forms (cdr cur)))
      ((not (consp cur)) nil)
    (let ((r (funcall fn (car cur))))
      (when r (return r)))))

(defun form-macroexpands-to-return-from-p (name form depth)
  "Like FORM-HAS-RETURN-FROM-P, but also expands global macro calls (up to
   *macro-expand-depth-limit*) so a (return-from NAME) produced by a macro is
   found even when the macro call is nested inside a special form such as PROGN
   (dotcl/dotcl issue 51: the top-level-only scan missed `(progn (some-macro))`).
   Scans BOTH the expansion and the original subforms, so it never misses a
   return-from and only ever over-keeps the implicit block (safe). Conservative:
   answers T if a macro expander errors. Uses cached-macroexpand, so the pre-pass
   expansion is shared with code-gen (identical gensyms).
   DEPTH is the current macro-expansion depth; entry callers pass 0. It is a
   required parameter (not &optional) so the compiler gives this hot recursive
   tree-walker a required-only lambda list, which enables the direct-delegate
   fast path instead of the args-array entry."
  (cond
    ((atom form) nil)
    ((and (eq (car form) 'return-from) (consp (cdr form)) (eq (cadr form) name)) t)
    ;; nested block/defun shadowing NAME — its own return-from is not ours
    ((and (eq (car form) 'block) (consp (cdr form)) (eq (cadr form) name)) nil)
    ((and (eq (car form) 'defun) (consp (cdr form)) (eq (cadr form) name)) nil)
    ((eq (car form) 'quote) nil)
    ;; Binding special forms: recurse only into their code positions (default
    ;; initializer forms + bodies), never into lambda-list parameter names or
    ;; LET variable names. Blindly recursing into a lambda list macroexpands a
    ;; form like (INST) — a required param named after a macro — firing that
    ;; macro's compile-time side effects. A binding name can never hold a
    ;; RETURN-FROM, so skipping it loses no detection.
    ((member (car form) '(lambda named-lambda))
     (let ((namedp (eq (car form) 'named-lambda)))
       (or (%scan-code-forms (lambda (f) (form-macroexpands-to-return-from-p name f depth))
                             (%lambda-list-init-forms (if namedp (caddr form) (cadr form))))
           (%scan-code-forms (lambda (f) (form-macroexpands-to-return-from-p name f depth))
                             (if namedp (cdddr form) (cddr form))))))
    ((member (car form) '(flet labels))
     (or (%scan-code-forms
          (lambda (fdef)
            (and (consp fdef)
                 (or (%scan-code-forms (lambda (f) (form-macroexpands-to-return-from-p name f depth))
                                       (%lambda-list-init-forms (cadr fdef)))
                     (%scan-code-forms (lambda (f) (form-macroexpands-to-return-from-p name f depth))
                                       (cddr fdef)))))
          (cadr form))
         (%scan-code-forms (lambda (f) (form-macroexpands-to-return-from-p name f depth))
                           (cddr form))))
    ((member (car form) '(let let*))
     (or (%scan-code-forms
          (lambda (b)
            (and (consp b) (cadr b)
                 (form-macroexpands-to-return-from-p name (cadr b) depth)))
          (cadr form))
         (%scan-code-forms (lambda (f) (form-macroexpands-to-return-from-p name f depth))
                           (cddr form))))
    (t
     (or
      ;; A global macro call may expand to a return-from (possibly after more
      ;; expansions). Local macrolet macros are handled separately by
      ;; form-has-macrolet-p (they are not in the global *macros* table).
      (and (symbolp (car form))
           (< depth *macro-expand-depth-limit*)
           (gethash (car form) *macros*)
           (handler-case
               (let ((expanded (cached-macroexpand form (gethash (car form) *macros*))))
                 (and (not (equal expanded form))
                      (form-macroexpands-to-return-from-p name expanded (1+ depth))))
             (error () t)))
      ;; Structural recursion into the original subforms (macro arguments may
      ;; themselves contain a return-from).
      (do ((cur form (cdr cur)))
          ((atom cur) (when cur (form-macroexpands-to-return-from-p name cur depth)))
        (when (form-macroexpands-to-return-from-p name (car cur) depth)
          (return t)))))))

(defun form-has-macrolet-p (form)
  "T if FORM contains a MACROLET / SYMBOL-MACROLET anywhere in the tree. Such a
   local macro can expand to (return-from <enclosing-defun> …) — which
   form-has-return-from-p cannot see, because the expander body is a quasiquoted
   template, not a literal return-from. When present we conservatively keep the
   defun's implicit block wrapper (disable the use-direct fast path) so the
   generated return-from resolves."
  (cond
    ((atom form) nil)
    ((eq (car form) 'quote) nil)
    ((member (car form) '(macrolet symbol-macrolet)) t)
    (t (do ((cur form (cdr cur)))
           ((atom cur) nil)
         (when (form-has-macrolet-p (car cur)) (return t))))))

(defun defun-pkg-spec (name)
  "Return (:pkg \"PKG\") if name refers to a symbol with a home package, nil otherwise.
   For (setf NAME) forms, uses the home package of NAME (the setf-target symbol).
   Used to pass package info to :defmethod so the assembler registers the function
   on the correct symbol (not just *package*'s inherited symbol, which at FASL load
   time is often CL-USER rather than the compile-time *package*)."
  (let ((target (cond ((symbolp name) name)
                      ((and (consp name) (eq (car name) 'setf) (symbolp (cadr name)))
                       (cadr name)))))
    (when (and target (symbol-package target))
      `(:pkg ,(package-name (symbol-package target))))))

(defvar *sil-dump-pattern* :unread
  "Cached upcased DOTCL_DUMP_SIL_FOR value (or NIL if unset); :UNREAD before the
   first compiled defun reads the env var. See %maybe-dump-defun-sil.")

(defun %maybe-dump-defun-sil (name result)
  "Diagnostic: if the env var DOTCL_DUMP_SIL_FOR is set to a substring that
   NAME contains (case-insensitive), print RESULT — the emitted instruction list,
   which carries the :body SIL — to *error-output*, bracketed by ;;DUMP-SIL /
   ;;END-DUMP-SIL markers. Lets a state-dependent miscompile be inspected as
   actually emitted during a full build (make-host-1), where the compiled fasl
   retains no SIL. Gated by an env var (not a special var) so it is stable against
   cross-compiled-vs-reader symbol identity and needs no in-image setter: run e.g.
   DOTCL_DUMP_SIL_FOR=%DEFKNOWN before the build. Returns RESULT."
  ;; The pattern is read from the env var once and cached (both write and read are
  ;; compiler-internal, so *sil-dump-pattern* has stable identity). Skipped during
  ;; cross-compile: this fn runs on the SBCL host then, where %getenv is not a host
  ;; function; the (%getenv ...) call is only reached at runtime, where it compiles
  ;; to the Runtime.Getenv intrinsic. Caching keeps the common (unset) case to one
  ;; env read for the whole process instead of one per compiled defun.
  (when (eq *sil-dump-pattern* :unread)
    (setf *sil-dump-pattern*
          (unless *cross-compiling*
            (let ((pat (%getenv "DOTCL_DUMP_SIL_FOR")))
              (and (stringp pat) (> (length pat) 0) (string-upcase pat))))))
  (when *sil-dump-pattern*
    (let ((nm (cond ((consp name) (format nil "~S" name))
                    ((symbolp name) (symbol-name name))
                    (t nil))))
      (when (and nm (search *sil-dump-pattern* (string-upcase nm)))
        (format *error-output* "~&;;DUMP-SIL ~A~%~S~%;;END-DUMP-SIL~%" nm result)
        (finish-output *error-output*))))
  result)

(defun %declares-special-p (body names)
  "T when BODY declares any of NAMES special.

   A parameter the direct entry binds with a LET* instead of receiving as a real
   parameter loses a SPECIAL declaration: the declaration sits at the head of the
   body, inside the implicit block, so the LET* never sees it and binds the name
   lexically. The array XEP binds it dynamically. Rather than reconstruct the
   declaration in the right place, such a function simply does not get a direct
   entry -- it is rare, and the XEP is correct.

   BODY may be the implicit-block wrapper, so a leading BLOCK is looked through."
  (let ((forms (if (and (consp body) (null (cdr body))
                        (consp (car body)) (eq (caar body) 'block))
                   (cddr (car body))
                   body)))
    (dolist (f forms nil)
      (unless (and (consp f) (eq (car f) 'declare)) (return nil))
      (dolist (d (cdr f))
        (when (and (consp d) (eq (car d) 'special))
          (dolist (v (cdr d))
            (when (member (var-name v) names :test #'string=)
              (return-from %declares-special-p t))))))))

(defun %optional-direct-eligible-p (required optional has-key-p rest-param aux)
  "T if a required+&optional function can carry typed direct delegates for its
   concrete arities in addition to the array XEP. Requires: at least one optional,
   no &key/&rest/&aux, no supplied-p var, and total params <= 8 (the
   direct-delegate arity ceiling). The default forms themselves are unrestricted:
   an absent optional is bound by a LET* in the direct body, which evaluates the
   form at call time exactly where the array XEP would — so (pkg *package*) and
   defaults that read earlier params are fine."
  (and optional (not has-key-p) (null rest-param) (null aux)
       (<= (+ (length required) (length optional)) 8)
       (every (lambda (o) (null (third o))) optional)))   ; no supplied-p var

(defun %key-direct-eligible-p (required optional key rest-param aux)
  "T if a required+&key function can carry a typed direct delegate for its
   required-only arity in addition to the array XEP.

   A variadic LispFunction has one entry, taking LispObject[], so a call builds
   an array even when it passes no keywords at all -- and that is most calls to
   a &key function. Requires: at least one key param, no &optional/&rest/&aux,
   no supplied-p var, and required <= 8 (the direct-delegate arity ceiling).
   The default forms are unrestricted: an absent key is bound by a LET* in the
   direct body, which evaluates the form at call time exactly where the array
   XEP would. A supplied-p var is fine too: on this arity none of them are
   supplied, so each is bound to NIL."
  (and key (null optional) (null rest-param) (null aux)
       (<= (length required) 8)))

(defun %key-binding-forms (key kw-var val-var)
  "LET* bindings for the key params. When KW-VAR is NIL none are supplied and
   each takes its default; otherwise the one keyword KW-VAR names takes VAL-VAR
   and the rest take their defaults. LET* rather than LET: CL binds key defaults
   in order and a later one may read an earlier parameter."
  (loop for k in key
        for kw = (intern (first k) "KEYWORD")
        append (if kw-var
                   (if (fourth k)
                       (list (list (second k) `(if (eq ,kw-var ',kw) ,val-var ,(third k)))
                             (list (fourth k) `(if (eq ,kw-var ',kw) t nil)))
                       (list (list (second k) `(if (eq ,kw-var ',kw) ,val-var ,(third k)))))
                   (if (fourth k)
                       (list (list (second k) (third k)) (list (fourth k) nil))
                       (list (list (second k) (third k)))))))

(defun %build-key-direct-body (required key wrapped-body kw-var val-var
                               &optional allow-other-keys-p fn-name)
  "The body for one typed arity. With KW-VAR, an unrecognized keyword is
   signalled before any default runs -- the array entry signals before entering
   the body, and a default may have a side effect."
  (declare (ignore required))
  (let ((bindings (%key-binding-forms key kw-var val-var)))
    (if kw-var
        `((unless (or ,@(loop for k in key
                              collect `(eq ,kw-var ',(intern (first k) "KEYWORD")))
                      (eq ,kw-var :allow-other-keys)
                      ,@(when allow-other-keys-p '(t)))
            (error 'program-error
                   :format-control "~a: unrecognized keyword argument ~s"
                   :format-arguments (list ,fn-name ,kw-var)))
          (let* ,bindings ,@wrapped-body))
        `((let* ,bindings ,@wrapped-body)))))

(defun %build-key-direct-specs (required key wrapped-body fn-name fn-pkg fn-symbol
                                allow-other-keys-p implicit-keys-p)
  "((ARITY SELF-P DIRECT-BODY) ...) for the arities of a &key function that can
   be typed:

     required            -- no keywords supplied, each key takes its default
     required + 2        -- exactly one keyword pair, which is the shape most
                            calls that DO pass a keyword have ((position x l
                            :test #'eq), (sort l #'< :key #'car), ...)

   The second one keeps a single copy of the body: which key was named is a
   per-binding IF, not a copy of the body per key. It is only built when every
   key is an implicit keyword -- an explicit ((:kw var) default) names a symbol
   whose package the generated comparison would have to reconstruct."
  (let* ((specs '())
         (base (%build-key-direct-body required key wrapped-body nil nil)))
    (multiple-value-bind (body-instrs self-p)
        (compile-function-body-direct required base fn-name fn-pkg fn-symbol)
      (push (list (length required) (if self-p t nil) body-instrs) specs))
    (when (and implicit-keys-p (<= (+ (length required) 2) 8))
      (let* ((kw-var (gensym "KW")) (val-var (gensym "KVAL"))
             (params (append required (list kw-var val-var)))
             (body (%build-key-direct-body required key wrapped-body kw-var val-var
                                           allow-other-keys-p fn-name)))
        (multiple-value-bind (body-instrs self-p)
            (compile-function-body-direct params body fn-name fn-pkg fn-symbol)
          (push (list (+ (length required) 2) (if self-p t nil) body-instrs) specs))))
    (nreverse specs)))


(defun %build-optional-direct-specs (required optional wrapped-body fn-name fn-pkg fn-symbol)
  "Build ((ARITY DIRECT-BODY) ...) for each concrete arity N in
   [len(required) .. len(required)+len(optional)]. For arity N the first
   (N - len(required)) optionals are real direct params; the rest are bound to
   their defaults by a wrapping LET*, so the shared body runs identically to the
   array XEP with those optionals defaulted. LET* rather than LET: CL binds
   optional defaults in order and a later one may read an earlier parameter."
  (let ((rn (length required))
        (specs '()))
    (loop for present from 0 to (length optional)
          for n = (+ rn present)
          for present-opts = (subseq optional 0 present)
          for absent-opts = (subseq optional present)
          for direct-params = (append required (mapcar #'car present-opts))
          for direct-body = (if absent-opts
                                `((let* ,(mapcar (lambda (o) (list (car o) (second o))) absent-opts)
                                    ,@wrapped-body))
                                wrapped-body)
          do (multiple-value-bind (body-instrs self-p)
                 (compile-function-body-direct direct-params direct-body fn-name fn-pkg fn-symbol)
               ;; A non-tail self-call makes compile-function-body-direct thread
               ;; the function itself as arg0 (params shift to ldarg 1+); the
               ;; install path then builds a method with a leading LispFunction
               ;; self param and binds the fn into the _funcN delegate. SELF-P
               ;; per arity tells HandleDefmethod which signature to emit.
               (push (list n (if self-p t nil) body-instrs) specs)))
    (nreverse specs)))

(defun sil-portable-lambda-list (params)
  "PARAMS with each variable NAME reduced to a package-independent symbol, for
   embedding in a :LAMBDA-LIST directive.

   The SIL must be byte-identical across self-host generations, and a raw lambda
   list is not: a DEFSTRUCT accessor's parameter is interned in DOTCL-INTERNAL
   when the cross-compile host is SBCL but in the current package when dotcl
   compiles itself, so the same DEFUN printed (DOTCL-INTERNAL::OBJ) in one
   generation and (OBJ) in the next -- SELFHOST-CHECK compares the two byte for
   byte and failed. Every other package-sensitive slot in the SIL (:PARAMS,
   :LOAD-SYM) carries strings for exactly this reason.

   The identity is not lost information here: the SIL carries no IN-PACKAGE, so
   these symbols are re-interned by whatever package the loader runs in either
   way. This is development-only display data (SLIME/SLY autodoc, DESCRIBE), and
   what a tool shows is the NAME.

   Only variable-name positions are rewritten. A default form is arbitrary code
   ((PREFIX \"G\"), (TEST #'EQL)) and is left exactly as written."
  (labels ((var (x) (if (and (symbolp x) x (not (keywordp x)))
                        (make-symbol (symbol-name x))
                        x))
           (spec (x)
             ;; (name default [supplied-p]) or ((:key name) default [supplied-p])
             (if (consp x)
                 (cons (if (consp (car x))
                           (list (caar x) (var (cadar x)))
                           (var (car x)))
                       (cdr x))
                 (var x)))
           (walk (ps)
             (cond ((null ps) nil)
                   ((atom ps) (var ps))          ; dotted tail
                   ((and (symbolp (car ps)) (car ps)
                         (char= (char (symbol-name (car ps)) 0) #\&))
                    ;; a lambda-list keyword: keep it, it is part of the syntax
                    (cons (car ps) (walk (cdr ps))))
                   (t (cons (spec (car ps)) (walk (cdr ps)))))))
    (walk params)))

(defun defun-runtime-registration-instrs (name params wrapped-body uninterned-fixup)
  "The DEFUN emission for the runtime-registration path: a closure defun (free
   vars captured) or a NON-top-level defun (nested inside a conditional /
   other form). Registers via compile-lambda + RegisterFunctionOnSymbol when
   the form is actually reached — the :defmethod paths register at ASSEMBLY
   time, which would define a guarded (unless (fboundp 'x) (defun x ...))
   unconditionally. Four name shapes: interned symbol / gensym (register on
   the actual symbol object) / (setf interned) (SetfFunction, package-aware) /
   anything else (mangled-string fallback)."
  (if (symbolp name)
      (if (symbol-package name)
          ;; compile-sym-lookup leaves the symbol widened to LispObject, and
          ;; RegisterFunctionOnSymbol takes a Symbol: without the narrowing
          ;; castclass the call is covariant, which the JIT tolerates but
          ;; verifiable IL (and AOT codegens like IL2CPP) does not.
          `(,@(compile-sym-lookup name)
            (:castclass "Symbol")
            ,@(compile-lambda params wrapped-body "" (mangle-name name))
            (:castclass "LispFunction")
            (:call "CilAssembler.RegisterFunctionOnSymbol")
            ,@(compile-sym-lookup name))
          ;; Uninterned (gensym) name: compile-sym-lookup would intern a
          ;; FRESH symbol by name (registering the fn on the wrong object,
          ;; and the mangled-name uninterned-fixup does not apply on the
          ;; runtime-registration path). Load the actual gensym object and
          ;; register directly on it. (fix fallout; ANSI DEFUN.ERROR.4
          ;; eval's a prog2-nested (defun #:g ...).)
          `((:load-const ,name) (:castclass "Symbol")
            ,@(compile-lambda params wrapped-body "" (mangle-name name))
            (:castclass "LispFunction")
            (:call "CilAssembler.RegisterFunctionOnSymbol")
            (:load-const ,name)))
      ;; (setf target): register on the target symbol's SetfFunction
      ;; slot, package-aware. RegisterFunction resolved the name via
      ;; Startup.Sym on the mangled string, which interns TARGET in the
      ;; wrong package (not the reader's symbol), so #'(setf target) /
      ;; (fboundp '(setf target)) could not find it — e.g. a non-top-level
      ;; (defun (setf acc) ...) inside report-and-ignore-errors (fix
      ;; fallout; ANSI FBOUNDP.6 / FUNCTION.7). An uninterned (setf gensym)
      ;; target still goes through UNINTERNED-FIXUP.
      (if (and (consp name) (eq (car name) 'setf) (symbolp (cadr name))
               (symbol-package (cadr name)))
          `(,@(compile-sym-lookup (cadr name))
            (:castclass "Symbol")
            ,@(compile-lambda params wrapped-body "" (mangle-name name))
            (:castclass "LispFunction")
            (:call "CilAssembler.RegisterSetfFunctionOnSymbol")
            ,@uninterned-fixup
            ,@(compile-quoted name))
          `((:ldstr ,(mangle-name name))
            ,@(compile-lambda params wrapped-body "" (mangle-name name))
            (:castclass "LispFunction")
            (:call "CilAssembler.RegisterFunction")
            ,@uninterned-fixup
            (:ldstr ,(mangle-name name)) (:call "Startup.Sym")))))

(defun defun-direct-instrs (name params required direct-wrapped-body wrapped-body
                            pkg-spec uninterned-fixup)
  "The DEFUN emission for the direct-params path (simple required-only
   functions): :defmethod-native when the whole function proves fixnum-native,
   else :defmethod-direct (with :self t when the body does a non-tail
   self-call, so the backend threads the LispFunction in as arg0)."
  (let* ((mangled (mangle-name name))
         (param-names (mapcar #'var-name required))
         (pkg-name (cadr pkg-spec))
         ;; Native eligibility: all fixnum params, fixnum return, ≤4 params,
         ;; no special-declared params
         (native-eligible
           (and (symbolp name)
                (<= (length required) 4)
                (all-params-fixnum-p params wrapped-body)
                (eq 'fixnum (gethash name *function-return-types*))
                (null (fn-body-special-params wrapped-body required))
                (null (remove-if-not #'global-special-p required)))))
    (multiple-value-bind (direct-body direct-self-p)
        (compile-function-body-direct params direct-wrapped-body mangled pkg-name name)
      `(,(if native-eligible
             `(:defmethod-native ,mangled
                ,@pkg-spec
                ,@(when (debug-frames-off-p wrapped-body) '(:no-frame t))
                :lambda-list ,(sil-portable-lambda-list params)
                :params ,param-names
                :body ,direct-body)
             ;; :self t — self-recursive non-native direct fn: the
             ;; backend gives the direct method a leading LispFunction self
             ;; param (threaded for non-tail self-calls, no per-entry lookup).
             `(:defmethod-direct ,mangled
                ,@pkg-spec
                ,@(when direct-self-p '(:self t))
                ,@(when (debug-frames-off-p wrapped-body) '(:no-frame t))
                :lambda-list ,(sil-portable-lambda-list params)
                :params ,param-names
                :body ,direct-body))
        ,@uninterned-fixup
        ,@(if (symbolp name)
              (compile-sym-lookup name)
              ;; A non-symbol function name — e.g. (SETF foo) — is
              ;; returned by DEFUN as the name itself (the list
              ;; (SETF foo)), NOT a symbol interned from its mangled
              ;; string. Emitting the mangled symbol broke e.g.
              ;; (multiple-value-list (eval `(defun (setf ,g) ...)))
              ;; (ANSI DEFINE-COMPILER-MACRO.4).
              (compile-quoted name))))))

(defun compile-defun (name params body)
  "Compile (defun name (params) body...) → :defmethod directive + return symbol."
  ;; xref: collect CALLER→CALLEE edges while this body (and any nested lambda)
  ;; compiles, and prepend a load-time %xref-note registration. The caller must
  ;; be a nameable global — an uninterned gensym defun records nothing.
  (let ((*xref-caller* (and (not *cross-compiling*)
                            (or (and (symbolp name) (symbol-package name) name)
                                (and (consp name) (eq (car name) 'setf) name))))
        (*xref-edges* '()))
    (let ((instrs (%compile-defun-1 name params body)))
      (let ((note (xref-note-instrs)))
        (if note (append note instrs) instrs)))))

(defun %compile-defun-1 (name params body)
  ;; Pre-pass: infer return type before body compilation so self-calls inside the
  ;; body benefit from the single-value elision path.
  (when (and (symbolp name) (not (gethash name *function-return-types*)))
    (let ((inferred (infer-body-return-type body (mangle-name name))))
      (when inferred
        (setf (gethash name *function-return-types*) inferred))))
  ;; Keep the definition for substitution at call sites when the name is
  ;; proclaimed INLINE. Recorded here, at the point the DEFUN is compiled,
  ;; which is what makes the proclamation have to come first (CLHS 3.2.2.1.3).
  ;; A redefinition overwrites the entry; call sites compiled against the old
  ;; body keep it, which is the licensed behaviour for an inlined function.
  (when (and (symbolp name) (not *cross-compiling*))
    (if (%global-inline-p name)
        (setf (gethash name *inline-defs*) (cons params body))
        ;; NOTINLINE (or a plain redefinition after the proclamation was taken
        ;; back) must not leave a stale body behind for later call sites.
        (remhash name *inline-defs*)))
  (multiple-value-bind (required optional key rest-param aux allow-other-keys-p has-key-p)
      (parse-lambda-list params)

    (let* ((param-names (mapcar #'var-name required))
           ;; Every variable the lambda list binds in the body. The analysis
           ;; context below must report ALL of them as bound — not just the
           ;; required ones — or a (declare (fixnum X)) on an &optional/&key
           ;; param never satisfies fixnum-typed-p during the analysis pass and
           ;; the cached dotimes expansion misses its fixnum declaration (the
           ;; cl-bench array functions take (&optional (size 2000)) and lost
           ;; the entire native loop/aref path this way).
           (all-param-vars (append required
                                   (mapcar #'car optional)
                                   (remove nil (mapcar #'third optional))
                                   (mapcar #'second key)
                                   (remove nil (mapcar #'fourth key))
                                   (when rest-param (list rest-param))))
           (block-name (cond ((and (consp name) (eq (car name) 'setf)) (cadr name))
                             ((consp name) (cadr name)) ; (cas foo) → block named foo
                             (t name)))
           ;; Establish the enclosing function's numeric type-declaration context
           ;; for the macro expansions performed during this analysis pass. The
           ;; *macroexpand-cache* shares ONE expansion between the analysis and
           ;; the later code-gen pass (so gensyms stay identical). A context-
           ;; dependent macro such as dotimes consults fixnum-typed-p on its
           ;; count-form to decide whether to inject (declare (fixnum ...)); if
           ;; the analysis expansion runs with these vars unbound, the cached
           ;; form lacks the declaration and the native int64 loop path can never
           ;; fire at code-gen. Bind them (and a dummy *locals* so lookup-local
           ;; reports params as bound) only around the analysis computations —
           ;; the dummy keys never reach emission. Code-gen re-validates each
           ;; declaration against the real boxed/captured-var context, so an
           ;; over-eager declaration on a captured var is harmlessly ignored.
           (analysis-context-vals
            (let* ((param-names (mapcar #'var-name all-param-vars))
                  (*fixnum-locals* (append (extract-fixnum-locals body)
                                           (drop-shadowed-type-locals
                                            param-names *fixnum-locals*)))
                  ;; Analysis context only: *LOCALS* here is a dummy whose keys
                  ;; ARE the var-name strings, so declaration entries key
                  ;; themselves. Real (symbol-keyed) outer entries simply never
                  ;; match here, which is the conservative direction.
                  (*cstate* (cstate-with *cstate* +cs-small-int-locals+
                                         (append (extract-small-int-locals body)
                                                 (cstate-small-int-locals))))
                  (*double-float-locals* (append (extract-double-float-locals body)
                                                 (drop-shadowed-type-locals
                                                  param-names *double-float-locals*)))
                  (*single-float-locals* (append (extract-single-float-locals body)
                                                 (drop-shadowed-type-locals
                                                  param-names *single-float-locals*)))
                  (*decimal-locals* (append (extract-decimal-locals body)
                                            (drop-shadowed-type-locals
                                             param-names *decimal-locals*)))
                  (*cstate* (cstate-with *cstate* +cs-locals+
                                         (append (mapcar (lambda (p) (cons p (var-name p)))
                                                         all-param-vars)
                                                 (cstate-locals)))))
              (cons
               (or ;; Detect (return-from block-name …) anywhere in the body,
                   ;; expanding global macro calls at any depth (a macro nested in
                   ;; a progn/let/etc. can expand to one — dotcl/dotcl issue 51).
                   (some (lambda (f) (form-macroexpands-to-return-from-p block-name f 0)) body)
                   ;; A MACROLET / SYMBOL-MACROLET local macro can also expand to
                   ;; (return-from block-name …) via its quasiquoted template, and
                   ;; its expander is lexical (not in the global *macros* table), so
                   ;; the scan above cannot expand it. Keep the implicit block
                   ;; wrapper conservatively when present.
                   (some #'form-has-macrolet-p body))
               (with-phase "analysis" (find-free-vars-with-defaults params body)))))
           (has-literal-return-from (car analysis-context-vals))
           ;; Check for free variables from original body (block wrapper doesn't add free vars)
           (free-vars (cdr analysis-context-vals))
           ;; Use direct params for simple required-only functions. A literal
           ;; return-from (or a macrolet that may expand to one) no longer forces
           ;; the array path: the direct body is wrapped in the implicit block
           ;; below, so an early (return-from name ...) still resolves while the
           ;; function keeps its typed direct delegate (was: every early-return
           ;; function fell back to InvokeSlow on every call).
           (use-direct (and (null free-vars)
                            (simple-required-only-p params)))
           ;; use-direct with no return-from: literal body, no block (preserves TCO).
           ;; Otherwise wrap in the implicit block so return-from resolves.
           (wrapped-body (if (and use-direct (not has-literal-return-from))
                             body
                             `((block ,block-name ,@body))))
           ;; The direct path binds &aux itself. The array XEP gets them from the
           ;; lambda list, but compile-function-body-direct only knows about required
           ;; params, so a LET* around the body — which is what &aux means — puts them
           ;; back. Inside the implicit block, so a (return-from name ...) in an init
           ;; form still resolves. Only this path may see it: adding the LET* to
           ;; WRAPPED-BODY would make the array XEP evaluate each init form twice.
           (direct-wrapped-body
             (let ((aux-bindings (%aux-bindings params)))
               (if (null aux-bindings)
                   wrapped-body
                   (multiple-value-bind (decls rest) (%split-leading-declares body)
                     (if (and use-direct (not has-literal-return-from))
                         `(,@decls (let* ,aux-bindings ,@rest))
                         `(,@decls (block ,block-name (let* ,aux-bindings ,@rest))))))))
           (pkg-spec (defun-pkg-spec name)))
      ;; For uninterned symbols (gensyms), emit extra code to set .Function
      ;; on the actual symbol object after defmethod registers the function.
      ;; Also handle (setf gensym): set SetfFunction on the uninterned gensym.
      (let ((uninterned-fixup
              (cond
                ;; Regular gensym: set .Function on the original symbol
                ((and (symbolp name) (null (symbol-package name)))
                 `((:load-const ,name)
                   (:castclass "Symbol")
                   (:ldstr ,(mangle-name name))
                   (:call "CilAssembler.GetFunction")
                   (:castclass "LispFunction")
                   (:call "CilAssembler.RegisterFunctionOnSymbol")))
                ;; (setf gensym): set .SetfFunction on the uninterned gensym target
                ((and (consp name) (eq (car name) 'setf)
                      (symbolp (cadr name)) (null (symbol-package (cadr name))))
                 `((:load-const ,(cadr name))
                   (:castclass "Symbol")
                   (:ldstr ,(mangle-name name))
                   (:call "CilAssembler.GetFunction")
                   (:castclass "LispFunction")
                   (:call "CilAssembler.RegisterSetfFunctionOnSymbol"))))))
        (let ((*cstate* (cstate-with *cstate* +cs-tco-self-symbol+
                                     (if (symbolp name) name nil))))
          (%maybe-dump-defun-sil name
          (cond
            ;; Closure defun (free vars captured), OR a NON-top-level defun (nested
            ;; inside a conditional / other form): register the function at RUNTIME
            ;; via compile-lambda + RegisterFunctionOnSymbol, executed only when the
            ;; form is actually reached. The :defmethod path below registers at
            ;; ASSEMBLY time, even inside an untaken if-branch, so a
            ;; guarded defun like (unless (fboundp 'x) (defun x …)) or (if nil
            ;; (defun x …)) would define X unconditionally, breaking cross-file
            ;; defdfun defaults on fresh compile.
            ((or free-vars (not *compile-was-toplevel*))
             (defun-runtime-registration-instrs name params wrapped-body
                                                uninterned-fixup))
            ;; Direct params: simple required-only functions
            (use-direct
             (defun-direct-instrs name params required direct-wrapped-body
                                  wrapped-body pkg-spec uninterned-fixup))
            ;; Standard defmethod (array XEP). A required+&optional function with
            ;; constant optional defaults additionally carries typed direct
            ;; delegates for each concrete arity, so calls at a fixed arity skip
            ;; the args-array InvokeSlow detour (the array XEP still backs apply /
            ;; other arities). Only the non-fasl (--asm) load path installs these;
            ;; the fasl path ignores :direct-delegates and stays array-only.
            (t
              (let ((direct-specs
                      (when (symbolp name)
                        (cond
                          ((and (%optional-direct-eligible-p required optional has-key-p rest-param aux)
                                (not (%declares-special-p wrapped-body (mapcar #'car optional))))
                           (%build-optional-direct-specs
                            required optional wrapped-body
                            (mangle-name name) (cadr pkg-spec) name))
                          ;; Same idea for &key: the required-only call is the
                          ;; one that can be typed, and it is the common one.
                          ((and (%key-direct-eligible-p required optional key rest-param aux)
                                (not (%declares-special-p wrapped-body (mapcar #'second key))))
                           (%build-key-direct-specs
                            required key wrapped-body
                            (mangle-name name) (cadr pkg-spec) name
                            allow-other-keys-p
                            (every (lambda (k) (null (fifth k))) key)))))))
                `((:defmethod ,(mangle-name name)
                   ,@pkg-spec
                   ,@(when (debug-frames-off-p wrapped-body) '(:no-frame t))
                   ;; Development information for SLIME/SLY autodoc and DESCRIBE.
                   ;; Carried by the directive, not by emitted code: a
                   ;; (%set-function-lambda-list ...) call after the definition
                   ;; would put the list in the constant pool once per
                   ;; redefinition, which is exactly the leak the pool work
                   ;; removed.
                   :lambda-list ,(sil-portable-lambda-list params)
                   :params ,param-names
                   ,@(when direct-specs `(:direct-delegates ,direct-specs))
                   :body ,(compile-function-body params wrapped-body (mangle-name name)))
                  ,@uninterned-fixup
                  ,@(if (symbolp name)
                        (compile-sym-lookup name)
                        (compile-quoted name))))))))))))

(defun compile-defun-toplevel (expr)
  "The DEFUN form handler: the staging AROUND compile-defun.
   In compile-file mode, ALSO evaluate the defun so the function is callable
   during subsequent compile-time macro expansion in the same file. SBCL does
   this; relying on it is the de-facto convention for libraries like
   alexandria, where a macro body uses a sibling defun (e.g., once-only calls
   make-gensym-list). *at-toplevel* is reset to NIL inside compile-form, so we
   test *compile-was-toplevel* (captured prior). try-eval prevents the failure
   of one defun (e.g., references not-yet-defined fn) from aborting
   compile-file as a whole. Runtime.TryEval (the C# entry that the `try-eval`
   call resolves to via cil-compiler.lisp's STRING= shortcut) binds
   *compile-file-mode* to NIL during eval so the recursive compile-form does
   NOT re-enter this handler and infinitely recurse on the same defun."
  (when (and *compile-was-toplevel* *compile-file-mode*
             (not *cross-compiling*))
    (try-eval expr))
  ;; Redefining a name that DEFSTRUCT registered as a keyword constructor drops
  ;; the call-site construction: the new definition decides what the call means.
  ;; The structure's own constructor DEFUN precedes its registration, so this
  ;; never clears the entry the same expansion is about to make.
  (when (symbolp (cadr expr))
    (remhash (cadr expr) *struct-keyword-ctors*))
  ;; CLHS 3.4.11: extract docstring (first form if string AND more forms follow).
  ;; Skip during cross-compile because cil-stdlib's own defuns may precede
  ;; (setf documentation) GF definition; runtime user defuns are fine.
  (let* ((name (cadr expr))
         (params (caddr expr))
         (body (cdddr expr))
         ;; Only attach docstring for plain symbol names; (setf foo) /
         ;; other function-name forms are skipped because compile-sym-lookup
         ;; expects a symbol (would error: SYMBOL-PACKAGE on cons).
         (has-docstring (and (not *cross-compiling*)
                             (symbolp name)
                             (consp body) (stringp (car body)) (cdr body)))
         (docstring (when has-docstring (car body)))
         (real-body (if has-docstring (cdr body) body))
         (defun-instrs (compile-defun name params real-body)))
    (if has-docstring
        ;; defun-instrs ends with sym on stack. Set documentation
        ;; via (funcall #'(setf documentation) docstring 'name 'function),
        ;; then leave sym on stack as the form's value.
        `(,@defun-instrs
          (:pop)
          ,@(compile-and-pop
              `(funcall #'(setf documentation) ,docstring ',name 'function))
          ,@(compile-sym-lookup name))
        defun-instrs)))

(defun compile-defmacro (name lambda-list body)
  "Compile (defmacro name lambda-list body...).
   Uses SBCL's eval to create macro function at compile time.
   At runtime, just returns the macro name as a symbol."
  ;; Package lock check: only fire when the macro isn't already defined.
  ;; If *macros* already has this key (e.g. bootstrap CL macros like DEFMETHOD),
  ;; the registration below is a no-op — checking the lock would cause false errors
  ;; for guard patterns like (unless (fboundp 'defmethod) (defmacro defmethod ...)).
  (when (and (not *cross-compiling*) (symbolp name)
             (not (gethash (macro-key-for-symbol name) *macros*)))
    (%check-package-lock name "DEFMACRO"))
  (let* ((whole-sym (gensym "WHOLE"))
         ;; If lambda-list starts with &whole var, bind var to whole form
         (whole-var (when (and (consp lambda-list) (eq (car lambda-list) '&whole))
                      (cadr lambda-list)))
         (rest-ll (if whole-var (cddr lambda-list) lambda-list))
         ;; Strip &environment var from lambda-list (not valid in destructuring-bind)
         (env-var nil)
         (clean-ll (let ((result nil) (ll rest-ll))
                     (loop while ll do
                       (cond ((atom ll)
                              ;; Dotted tail (a b . rest) = (a b &rest rest)
                              (push '&rest result)
                              (push ll result)
                              (setq ll nil))
                             ((eq (car ll) '&environment)
                              (setq env-var (cadr ll))
                              (setq ll (cddr ll)))
                             (t (push (car ll) result)
                                (setq ll (cdr ll)))))
                     (nreverse result)))
         ;; 1-arg (compile-time eval): the compiler expands via this expander, so
         ;; bind &environment to a REIFIED environment reflecting the current
         ;; lexical macro / symbol-macro scope — a (macros-ht . symbol-macros-ht)
         ;; cons that macroexpand-1/macroexpand read (mirrors the macrolet env
         ;; builder, cil-compiler.lisp). Previously this was NIL, so a macro doing
         ;; (macroexpand-1 'sym env) inside a symbol-macrolet saw no binding —
         ;; e.g. serapeum with-boolean's %all-branches% channel. *macros*/
         ;; *symbol-macros* are read at EXPANSION time, so they carry the scope
         ;; active when the macro call is compiled.
         (wrapped-body-1arg (if env-var
                                `((let ((,env-var (%reify-macro-environment)))
                                    ,@body))
                                body))
         ;; defmacro creates an implicit block named after the macro (1-arg form)
         (block-wrapped-body
           (if (some (lambda (f) (form-has-return-from-p name f)) wrapped-body-1arg)
               `((block ,name ,@wrapped-body-1arg))
               wrapped-body-1arg))
         ;; expander-form for compile-time eval (1-arg: form only)
         (expander-form-1arg
           (if whole-var
               `(lambda (,whole-sym)
                  (let ((,whole-var ,whole-sym))
                    (destructuring-bind ,clean-ll (cdr ,whole-sym)
                      ,@block-wrapped-body)))
               `(lambda (,whole-sym)
                  (destructuring-bind ,clean-ll (cdr ,whole-sym)
                    ,@block-wrapped-body))))
         ;; 2-arg (runtime): env-var bound to the actual env argument (CLHS 3.4.4).
         ;; %register-macro-function-rt overwrites the *macros* entry with a wrapper
         ;; that calls this 2-arg expander with a NIL env (Runtime.Misc.cs), so at
         ;; same-session compile the macro would see no lexical scope. When the
         ;; passed env is NIL, reify the compiler's current macro / symbol-macro
         ;; scope so (macroexpand-1 'sym env) inside a symbol-macrolet expands —
         ;; e.g. serapeum with-boolean's %all-branches% channel.
         (env-sym (gensym "ENV"))
         (wrapped-body-2arg (if env-var
                                `((let ((,env-var (or ,env-sym (%reify-macro-environment))))
                                    ,@body))
                                body))
         (block-wrapped-body-2arg
           (if (some (lambda (f) (form-has-return-from-p name f)) wrapped-body-2arg)
               `((block ,name ,@wrapped-body-2arg))
               wrapped-body-2arg))
         (expander-form-2arg
           (if whole-var
               `(lambda (,whole-sym ,env-sym)
                  ,@(unless env-var `((declare (ignore ,env-sym))))
                  (let ((,whole-var ,whole-sym))
                    (destructuring-bind ,clean-ll (cdr ,whole-sym)
                      ,@block-wrapped-body-2arg)))
               `(lambda (,whole-sym ,env-sym)
                  ,@(unless env-var `((declare (ignore ,env-sym))))
                  (destructuring-bind ,clean-ll (cdr ,whole-sym)
                    ,@block-wrapped-body-2arg))))
         ;; When inside a flet scope, wrap the eval form with the flet definitions
         ;; so that flet-local functions (e.g. PROGNIFY in SBCL's macros.lisp) are
         ;; available during compile-time macro registration.
         (eval-form (if *compile-time-flet-defs*
                        `(flet ,*compile-time-flet-defs*
                           ,expander-form-1arg)
                        expander-form-1arg))
         (expander-fn (handler-case (eval eval-form)
                        (error () nil))))
    ;; Only register if eval succeeded and no existing entry
    (when expander-fn
      (let ((mkey (macro-key-for-symbol name)))
        (unless (gethash mkey *macros*)
          (setf (gethash mkey *macros*) expander-fn)))
      ;; In cross-compilation context (dotcl as SBCL XC host), also update
      ;; SBCL's info db so ir1-convert-global-functoid recognizes this as a macro.
      ;; Without this, SBCL XC treats macros defined by dotcl's defmacro as functions.
      ;; Gated by #+sbcl so dotcl's reader doesn't try to intern SB-C symbols.
      #+sbcl
      (when (and *cross-compiling* (symbolp name))
        (ignore-errors
          (let ((fn2 (let ((fn expander-fn))
                       (lambda (form env) (declare (ignore env)) (funcall fn form)))))
            (setf (sb-c::info :function :kind name) :macro)
            (setf (sb-c::info :function :macro-function name) fn2)))))
    ;; Runtime: compile the expander lambda and register as macro function,
    ;; then return macro name symbol.
    ;; This ensures FASL loads also register the macro.
    ;; %REGISTER-MACRO-FUNCTION-RT returns the function it registered, so the
    ;; lambda list goes on straight from that value -- asking for
    ;; (MACRO-FUNCTION 'name) again would put a second copy of the name in the
    ;; constant pool, and a redefinition loop keeps every one of them.
    ;;
    ;; The lambda list a tool should show for a macro is the one the user wrote,
    ;; not the (form env) pair the expander actually takes.
    (compile-expr `(progn (%set-function-lambda-list
                           (%register-macro-function-rt ',name ,expander-form-2arg)
                           ',lambda-list)
                          ',name))))

(defun try-eval (form)
  "Eval FORM, ignoring errors. Used during compile-file to establish
   compile-time values for defvar/defparameter/defconstant.
   Note: callers usually hit Runtime.TryEval instead (cil-compiler.lisp
   has a (string= op \"TRY-EVAL\") shortcut that emits a direct call to
   the C# entry); the C# side is responsible for binding
   *compile-file-mode* to NIL during the inner eval. This Lisp
   definition is the fallback for non-shortcut callers."
  (handler-case (eval form)
    (error (c)
      (warn "compile-time eval failed: ~A" c)
      nil)))

(defun %toplevel-ct-eval-form-p (form)
  "T for a toplevel form whose compile-time side effect has to be visible while the
   REST of the enclosing toplevel progn is compiled.

   Only IN-PACKAGE for now, because that is the one measured to matter. A macrolet
   whose expansion is (progn (in-package X) (defstruct ...) (in-package Y) ...) —
   the shape SBCL's force-delayed-defbangstructs uses to replay delayed DEF!STRUCTs —
   interned every accessor name in the package that was current when the macrolet
   form was read, not in X and Y. Accessor names are built at macroexpansion time and
   interned in *PACKAGE*, so the IN-PACKAGE has to have run by then.

   A file's toplevel forms get this from the loader, which splits a toplevel PROGN and
   compiles+runs each piece in turn. That split does not reach inside MACROLET, and
   EVAL of a whole (progn (in-package X) ...) never gets it at all — so the ordering
   belongs here as well, not only in the loader.

   IN-PACKAGE is idempotent, so evaluating it here and again at load time is harmless.
   Other forms in CLHS 3.2.3.1's compile-time-side-effect set are deliberately left
   out until there is a measured case: several of them (DEFCLASS, DEFMACRO) are not
   idempotent in the same cheap way, and this is a hot path.

   Compared by SYMBOL-NAME, not EQ: symbols baked into this list at cross-compile
   time are not EQ to the ones the runtime reader interns (same idiom as
   COMPILE-EVAL-WHEN's defvar list)."
  (and (consp form)
       (symbolp (car form))
       (string= (symbol-name (car form)) "IN-PACKAGE")))

(defun compile-eval-when (situations body)
  "Compile (eval-when (situations...) body...).
   During cross-compilation: :compile-toplevel/:execute → eval in SBCL.
   During compile-file: :compile-toplevel → eval at compile time (CLHS 3.2.3.1),
     :load-toplevel → emit CIL for load.
   During load/eval: only :execute → emit CIL.
   When :compile-toplevel at runtime, eval each form individually first
   (so defvar values are available for subsequent macro expansion)."
  (let ((ct-p (or (member :compile-toplevel situations)
                  (member 'compile-toplevel situations)
                  (member 'compile situations)))
        (lt-p (or (member :load-toplevel situations)
                  (member 'load-toplevel situations)
                  (member 'load situations)))
        (ex-p (or (member :execute situations)
                  (member 'execute situations)
                  (member 'eval situations))))
    ;; Compile-time evaluation at top level.
    ;; Cross-compile: SBCL eval (all forms).
    ;; Compile-file: eval all :compile-toplevel forms per CLHS 3.2.3.1
    ;;   (defmacro, defvar, etc. must take effect before subsequent forms).
    ;; Load/eval: best-effort try-eval of defvar/defparameter only.
    (when *at-toplevel*
      (cond
        (*cross-compiling*
         (when (or ct-p ex-p)
           (dolist (form body)
             ;; Skip %register-struct-class: dotcl runtime function absent in SBCL.
             ;; During compile-file, the cross-package bridge resolves it correctly.
             (unless (and (consp form)
                          (symbolp (car form))
                          (string= (symbol-name (car form)) "%REGISTER-STRUCT-CLASS"))
               (eval form)))))
        (*compile-file-mode*
         (when ct-p
           (let ((*compile-file-mode* nil))
             (dolist (form body)
               (eval form)))))
        (t
         (when (or ct-p ex-p)
           (dolist (form body)
             (when (and (consp form)
                        (symbolp (car form))
                        (member (symbol-name (car form))
                                '("DEFVAR" "DEFPARAMETER" "DEFCONSTANT"
                                  "DEFTYPE" "DEFSTRUCT" "DEFCLASS"
                                  "DEFINE-CONDITION" "DEFMACRO")
                                :test #'string=))
               (try-eval form)))))))
    ;; Emit CIL for runtime/load.
    ;; Per CLHS 3.2.3.1 (Figure 3-7):
    ;; - compile-file at top level: emit for load iff :load-toplevel is present.
    ;;   A bare {:execute} eval-when at top level of a compiled file is DISCARDED
    ;;   (its body is neither evaluated at compile time nor placed in the fasl),
    ;;   so emitting on ex-p here was wrong (leaked :execute-only bodies into the
    ;;   fasl load code — see EVAL-WHEN.1).
    ;; - Cross-compiling (dotcl bootstrap convention): :load-toplevel OR :execute
    ;;   at top level → emit CIL. The core build relies on this; keep it.
    ;; - Load/eval (not cross-compiling, not compile-file): only :execute → emit CIL.
    ;; - Not at top level: only :execute → emit body as implicit progn.
    (if (cond
          (*cross-compiling*
           (if *at-toplevel* (or lt-p ex-p) ex-p))
          (*compile-file-mode*
           (if *at-toplevel* lt-p ex-p))
          (t ex-p))
        (compile-progn body)
        (emit-nil))))

(defun unwrap-body-wrappers (body)
  "Peel nested single-form (block name ...) and (let* bindings ...) wrappers to
   reach the leading declarations. &aux + a defun block produce
   (block name (let* aux ...)), so both levels must be unwrapped."
  (loop while (and (= (length body) 1)
                   (consp (car body))
                   (member (caar body) '(block let*)))
        do (setf body (cddar body)))
  body)

(defun fn-body-special-params (body all-params)
  "Find function parameters declared special in the function body.
   Handles body possibly wrapped in (block name ...).
   ALL-PARAMS is a list of parameter SYMBOLS; a declaration matches a parameter
   under the same rule references resolve by (SHADOWS-VAR-P), so a declaration
   naming another package's symbol of the same name matches nothing here."
  (let ((inner-forms (unwrap-body-wrappers body)))
    (multiple-value-bind (specials _rest) (extract-specials inner-forms)
      (declare (ignore _rest))
      (remove-if-not (lambda (s)
                       (some (lambda (p) (shadows-var-p p s)) all-params))
                     specials))))

(defun special-param-name-set (body all-params)
  "Names (strings) of ALL-PARAMS that are special — either declared special in
   BODY or globally proclaimed (defvar/defparameter/proclaim/defmvar).
   Special variables are accessed through the dynamic-binding stack, never
   through a lexical box, so they must be excluded from capture-driven boxing
   (a boxed special would store the LispObject[] box where a value is expected)."
  (mapcar #'var-name
          (%union-eq (fn-body-special-params body all-params)
                 (remove-if-not #'global-special-p all-params))))

(defun compile-function-body (params body &optional (fn-name ""))
  "Compile a function body. Params are bound from args array (arg 0).
   Handles &rest/&optional/&key parameters."
  (merge-disjoint-locals (compile-function-body-inner params body 0 fn-name)))

(defun simple-required-only-p (params)
  "Return T if params is a required-only lambda list with <= 8 params. &aux is
   allowed: it is not an argument-passing feature but sequential binding, which the
   direct path reproduces with a LET* around the body (see compile-defun)."
  (multiple-value-bind (required optional key rest-param aux allow-other-keys-p has-key-p) (parse-lambda-list params)
    (declare (ignore allow-other-keys-p aux))
    (and (<= (length required) 8)
         (null optional) (null key) (not has-key-p) (null rest-param))))

(defun %aux-bindings (params)
  "&aux entries of PARAMS as LET* bindings, or NIL when there are none."
  (let ((aux (nth-value 4 (parse-lambda-list params))))
    (mapcar (lambda (a) (if (consp a) (list (car a) (second a)) (list a nil))) aux)))

(defun %direct-body-with-aux (params body)
  "BODY prepared for the direct-parameter path: &aux bound by a LET* placed under
   any leading declarations (those are about the parameters). compile-function-body
   binds &aux from the lambda list, but compile-function-body-direct only knows the
   required params, so the binding has to be in the body. Returns BODY unchanged
   when there is no &aux."
  (let ((aux-bindings (%aux-bindings params)))
    (if (null aux-bindings)
        body
        (multiple-value-bind (decls rest) (%split-leading-declares body)
          `(,@decls (let* ,aux-bindings ,@rest))))))

(defun %split-leading-declares (body)
  "Return (values declare-forms rest). Declarations at the head of a function body
   are about its PARAMETERS — (declare (special x)) on a parameter binds it
   dynamically — so a wrapper inserted around the body must go under them, not
   over them."
  (let ((decls '()) (rest body))
    (loop while (and rest (consp (car rest)) (eq (caar rest) 'declare))
          do (push (pop rest) decls))
    (values (nreverse decls) rest)))

(defun all-params-fixnum-p (params body)
  "Return T if PARAMS is all required fixnum-declared args with no optional/key/rest.
   Used to determine native-eligibility for the native self-call path."
  (multiple-value-bind (required optional key rest-param) (parse-lambda-list params)
    (and (null optional) (null key) (null rest-param)
         (not (null required))
         (let ((fxlocals (extract-fixnum-locals body)))
           (every (lambda (p) (member (var-name p) fxlocals :test #'string=))
                  required)))))

(defun %sil-references-local-p (tree key)
  "Return T if the SIL TREE contains an instruction (:ldloc KEY).
   Used to detect whether a non-native direct body actually performs a
   non-tail self-call (the only producer of (:ldloc *self-fn-local*)).
   Walks via a local recursion that visits car and cdr as separate
   statements (NOT `(or (f car) (f cdr))`): the latter mixes a non-tail
   self-call with a tail self-call in one form and the tail call's TCO
   loop drops the non-tail call's result on the long body spine."
  (labels ((walk (x)
             (when (consp x)
               (cond
                 ((and (eq (car x) :ldloc) (consp (cdr x)) (eq (cadr x) key))
                  (return-from %sil-references-local-p t))
                 ;; (:load-const OBJ) embeds a quoted data literal that may be
                 ;; circular (e.g. '#1=(1 2 3 . #1#)); it is data, never SIL,
                 ;; so it can't contain (:ldloc KEY). Don't descend — walking a
                 ;; cyclic constant loops forever.
                 ((eq (car x) :load-const) nil)
                 (t (walk (car x)) (walk (cdr x)))))))
    (walk tree)
    nil))

(defun %sil-subst-self-arg0 (tree key)
  "Replace every (:ldloc KEY) leaf in SIL TREE with (:ldarg 0). KEY is the
   unique self-fn gen-local, so this rewrites exactly the non-tail self-call
   receivers to read the self LispFunction threaded in as arg0."
  (cond ((atom tree) tree)
        ((and (eq (car tree) :ldloc) (consp (cdr tree))
              (eq (cadr tree) key) (null (cddr tree)))
         '(:ldarg 0))
        ;; (:load-const OBJ) holds an opaque (possibly circular) data literal —
        ;; never a self-call receiver. Leave it untouched, don't recurse.
        ((eq (car tree) :load-const) tree)
        (t (let ((a (%sil-subst-self-arg0 (car tree) key))
                 (d (%sil-subst-self-arg0 (cdr tree) key)))
             (if (and (eq a (car tree)) (eq d (cdr tree))) tree (cons a d))))))

;;; --- Shared context-construction helpers for the function-body compilers ---
;;; (verbatim extractions of blocks that were identical in
;;; compile-function-body-direct and compile-function-body-inner)

(defun gen-param-local-keys (all-params)
  "Fresh gen-local key per param: alist (param . key)."
  (mapcar (lambda (p) (cons p (gen-local (var-name p)))) all-params))

(defun %string-member-p (x list)
  "T if string X is STRING= to some element of LIST. Required-only (typed direct
   delegate) and internally uses only STRING= (which has a 2-arg direct delegate),
   so it never re-enters the variadic MEMBER XEP."
  (dolist (y list nil) (when (string= x y) (return t))))

(defun %eq-member-p (x list)
  "T if X is EQ to some element of LIST (EQL membership for symbols)."
  (dolist (y list nil) (when (eq x y) (return t))))

(defun %union-eq (a b)
  "UNION of two symbol lists by EQ (= EQL for symbols): B plus every element of A
   not already present. Required-only helper — a typed direct delegate that skips
   the variadic UNION :test XEP (InvokeSlow) on the special-param analysis hot
   path. Callers use the result as a set, so order/dup match UNION well enough."
  (let ((r b))
    (dolist (x a r) (unless (%eq-member-p x r) (push x r)))))

(defun %strings-in-both (a b)
  "Elements of string list A that are STRING= to some element of B (INTERSECTION
   by STRING=, keeping A's elements/order). A boxing-analysis hot path per compile
   — a required-only helper skips the variadic INTERSECTION :test XEP (InvokeSlow)."
  (let ((r '()))
    (dolist (x a (nreverse r)) (when (%string-member-p x b) (push x r)))))

(defun %strings-minus (a b)
  "Elements of string list A not STRING= to any element of B (SET-DIFFERENCE by
   STRING=). Companion to %strings-in-both, same rationale."
  (let ((r '()))
    (dolist (x a (nreverse r)) (unless (%string-member-p x b) (push x r)))))

(defun params-needs-boxing (body all-params)
  "Names of params that must be boxed: mutated AND captured in BODY (single
   walk), minus special params — those are bound on the dynamic
   stack, not in a box."
  (let* ((mc (multiple-value-list
              (find-mutated-and-captured-vars body (mapcar #'var-name all-params))))
         (mutated (first mc))
         (captured (second mc)))
    (%strings-minus
     (%strings-in-both mutated captured)
     (special-param-name-set body all-params))))

(defun params-shadowed-symbol-macros (all-params)
  "Value for rebinding *symbol-macros* in a function body: lambda-list
   parameters shadow an enclosing symbol-macro of the same name (CLHS 3.4.2),
   including inside nested lambdas. Without this a self-referential
   symbol-macro whose name is a param — e.g. serapeum define-env-method's
   (self (slot-value self 'self)) — is still live when a NESTED lambda's
   free-var scan reaches the name, expanding it forever (compile hang)."
  (let ((param-names (mapcar #'var-name all-params)))
    (remove-if (lambda (entry)
                 (let ((k (car entry)))
                   (member (if (symbolp k) (var-name k) "")
                           param-names :test #'string=)))
               *symbol-macros*)))

(defun params-boxed-vars (all-params needs-boxing)
  "Value for rebinding *boxed-vars*: the param objects (from ALL-PARAMS)
   whose names are in NEEDS-BOXING."
  (mapcar (lambda (name) (find name all-params :key #'var-name :test #'string=))
          needs-boxing))

(defun compile-args-param-instrs (required optional key-specs rest-param
                                  locals-alist n-required key-start
                                  arg-elem-fn args-array-instrs
                                  defaults-locals-fn)
  "Parameter-binding instructions for an args-array function body. Shared by
   compile-function-body-inner and compile-closure-body, which differ only in
   three context points:
   ARG-ELEM-FN — (lambda (i) instrs) pushing args[i] (inner: ldarg
     args-arg-idx + ldelem; closure: :load-arg, which the assembler maps per
     closure mode).
   ARGS-ARRAY-INSTRS — instrs pushing the args array itself (for ldlen /
     FindKeyArg / CollectRestArgs).
   DEFAULTS-LOCALS-FN — (lambda (remaining-param-names) locals-value): the
     *locals* in effect while compiling an &optional/&key default form. Per
     CL, the current and all not-yet-initialized params must not be visible
     (a default referencing a later param name as a special variable must
     read the dynamic binding); the closure variant also keeps env captures
     visible so defaults see the outer scope."
  (append
   ;; Required params: load from args[i]
   (loop for p in required
         for key = (cdr (assoc p locals-alist))
         for i from 0
         if (boxed-var-p p)
           append (emit-box-create key (funcall arg-elem-fn i) p)
         else
           append `((:declare-local ,key "LispObject")
                    ,@(funcall arg-elem-fn i)
                    (:stloc ,key)
                    ,@(%local-var-marker key p)))
   ;; Optional params: check args.Length (with boxing & supplied-p support)
   (let ((opt-instrs nil)
         (remaining-opt-names (mapcar #'car optional)))
     (loop for (opt-name opt-default sp-var) in optional
           for i from n-required
           for key = (cdr (assoc opt-name locals-alist))
           do ;; Mask current+later opt params while compiling the default
              (let* ((*cstate* (cstate-with *cstate* +cs-locals+
                                            (funcall defaults-locals-fn
                                                     (mapcar #'var-name remaining-opt-names))))
                     (default-label (gen-label "OPTDEF"))
                     (done-label (gen-label "OPTDONE")))
                (setq opt-instrs
                      (append opt-instrs
                              (if (boxed-var-p opt-name)
                                  (let ((tmp (gen-local "OPTTMP")))
                                    `((:declare-local ,tmp "LispObject")
                                      ,@args-array-instrs (:ldlen) (:conv-i4)
                                      (:ldc-i4 ,(1+ i))
                                      (:blt ,default-label)
                                      ,@(funcall arg-elem-fn i)
                                      (:br ,done-label)
                                      (:label ,default-label)
                                      ,@(if opt-default
                                            (compile-expr opt-default)
                                            (emit-nil))
                                      (:label ,done-label)
                                      (:stloc ,tmp)
                                      ,@(emit-box-create key (list (list :ldloc tmp)) opt-name)))
                                  `((:declare-local ,key "LispObject")
                                    ,@args-array-instrs (:ldlen) (:conv-i4)
                                    (:ldc-i4 ,(1+ i))
                                    (:blt ,default-label)
                                    ,@(funcall arg-elem-fn i)
                                    (:br ,done-label)
                                    (:label ,default-label)
                                    ,@(if opt-default
                                          (compile-expr opt-default)
                                          (emit-nil))
                                    (:label ,done-label)
                                    (:stloc ,key)
                                    ,@(%local-var-marker key opt-name)))))
              ;; After initializing, this param is now visible to subsequent defaults
              (pop remaining-opt-names)
              ;; supplied-p variable right after its optional param
              (when sp-var
                  (let ((sp-key (cdr (assoc sp-var locals-alist)))
                        (sp-found-label (gen-label "OPTSPF"))
                        (sp-done-label (gen-label "OPTSPD")))
                    (setq opt-instrs
                          (append opt-instrs
                                  `((:declare-local ,sp-key "LispObject")
                                    ,@args-array-instrs (:ldlen) (:conv-i4)
                                    (:ldc-i4 ,(1+ i))
                                    (:blt ,sp-found-label)
                                    ,@(emit-t)
                                    (:br ,sp-done-label)
                                    (:label ,sp-found-label)
                                    ,@(emit-nil)
                                    (:label ,sp-done-label)
                                    (:stloc ,sp-key))))))))
     opt-instrs)
   ;; &key params: search from key-start (with boxing support)
   ;; supplied-p variable is emitted right after its key param
   ;; (CL requires left-to-right init, so later defaults can reference earlier supplied-p)
   (let ((key-instrs nil)
         (remaining-key-vars (mapcar #'second key-specs)))
     (dolist (key-spec key-specs)
       (let* ((key-name (first key-spec))
              (var-name (second key-spec))
              (key-default (third key-spec))
              (sp-var (fourth key-spec))
              (explicit-key-pkg (fifth key-spec))
              (find-key-fn (if explicit-key-pkg "Runtime.FindKeyArgByName" "Runtime.FindKeyArg"))
              (key (cdr (assoc var-name locals-alist)))
              (found-label (gen-label "KEYFOUND"))
              (done-label (gen-label "KEYDONE")))
         ;; Key param binding: mask current+later key params while compiling
         ;; the default (see DEFAULTS-LOCALS-FN above)
         (let ((default-instrs
                 (if key-default
                     (let ((*cstate* (cstate-with *cstate* +cs-locals+
                                                  (funcall defaults-locals-fn
                                                           (mapcar #'var-name remaining-key-vars)))))
                       (compile-expr key-default))
                     (emit-nil))))
           (setq key-instrs
                 (append key-instrs
                         (if (boxed-var-p var-name)
                             (let ((tmp (gen-local "KEYTMP")))
                               `((:declare-local ,tmp "LispObject")
                                 ,@args-array-instrs (:ldc-i4 ,key-start)
                                 (:ldstr ,key-name)
                                 ,@(when explicit-key-pkg `((:ldstr ,explicit-key-pkg)))
                                 (:call ,find-key-fn)
                                 (:dup) (:brtrue ,found-label)
                                 (:pop)
                                 ,@default-instrs
                                 (:br ,done-label)
                                 (:label ,found-label)
                                 (:label ,done-label)
                                 (:stloc ,tmp)
                                 ,@(emit-box-create key (list (list :ldloc tmp)) var-name)))
                             `((:declare-local ,key "LispObject")
                               ,@args-array-instrs (:ldc-i4 ,key-start)
                               (:ldstr ,key-name)
                               ,@(when explicit-key-pkg `((:ldstr ,explicit-key-pkg)))
                               (:call ,find-key-fn)
                               (:dup) (:brtrue ,found-label)
                               (:pop)
                               ,@default-instrs
                               (:br ,done-label)
                               (:label ,found-label)
                               (:label ,done-label)
                               (:stloc ,key)
                               ,@(%local-var-marker key var-name))))))
         ;; supplied-p variable right after its key param
         (when sp-var
           (let ((sp-key (cdr (assoc sp-var locals-alist)))
                 (sp-found-label (gen-label "SPFOUND"))
                 (sp-done-label (gen-label "SPDONE")))
             (setq key-instrs
                   (append key-instrs
                           `((:declare-local ,sp-key "LispObject")
                             ,@args-array-instrs (:ldc-i4 ,key-start)
                             (:ldstr ,key-name)
                             ,@(when explicit-key-pkg `((:ldstr ,explicit-key-pkg)))
                             (:call ,find-key-fn)
                             (:brtrue ,sp-found-label)
                             ,@(emit-nil)
                             (:br ,sp-done-label)
                             (:label ,sp-found-label)
                             ,@(emit-t)
                             (:label ,sp-done-label)
                             (:stloc ,sp-key)))))))
       ;; After initializing, this key param is now visible to subsequent defaults
       (pop remaining-key-vars))
     key-instrs)
   ;; Rest param: collect args[N..] (after required+optional, before &key)
   (when rest-param
     (let ((key (cdr (assoc rest-param locals-alist)))
           (n key-start))
       (if (boxed-var-p rest-param)
           (emit-box-create key (append args-array-instrs
                                        (list (list :ldc-i4 n)
                                              (list :call "Runtime.CollectRestArgs")))
                            rest-param)
           `((:declare-local ,key "LispObject")
             ,@args-array-instrs (:ldc-i4 ,n) (:call "Runtime.CollectRestArgs")
             (:stloc ,key)
             ,@(%local-var-marker key rest-param)))))))

(defun compile-args-arity-instrs (fn-name optional key-specs rest-param has-key-p
                                  n-required args-array-instrs)
  "Arity-check prefix for an args-array function body (shared inner/closure).
   The :direct closure case (arity check omitted) is handled by the caller."
  (cond
    ((and (null optional) (null key-specs) (null rest-param) (not has-key-p))
     ;; Required-only: exact arity check
     `((:ldstr ,fn-name) ,@args-array-instrs (:ldc-i4 ,n-required)
       (:call "Runtime.CheckArityExact")))
    ((and optional (null key-specs) (null rest-param) (not has-key-p))
     ;; Optional-only: min + max check
     (let ((n-max (+ n-required (length optional))))
       `((:ldstr ,fn-name) ,@args-array-instrs (:ldc-i4 ,n-required)
         (:call "Runtime.CheckArityMin")
         (:ldstr ,fn-name) ,@args-array-instrs (:ldc-i4 ,n-max)
         (:call "Runtime.CheckArityMax"))))
    (t
     ;; Has key/rest: minimum arity check only
     `((:ldstr ,fn-name) ,@args-array-instrs (:ldc-i4 ,n-required)
       (:call "Runtime.CheckArityMin")))))

(defun compile-args-key-check-instrs (fn-name key-specs has-key-p allow-other-keys-p
                                      key-start args-array-instrs)
  "Unknown-keyword check for an args-array function body (shared inner/
   closure). Fires for any &key lambda list (even a bare &key with no key
   params — hence HAS-KEY-P) without &allow-other-keys; explicit-package
   keyword names compare via the package-aware CheckNoUnknownKeys2."
  (when (and has-key-p (not allow-other-keys-p))
    (let ((has-explicit (some #'fifth key-specs)))
      ;; The name and package vectors are the same for every call of this
      ;; function, and CheckNoUnknownKeys only reads them, so they are emitted as
      ;; one cached array per site rather than being rebuilt in the prologue.
      ;; Building them per call put a string[] on every &key call -- including
      ;; the calls that pass no keyword at all, where the check returns on its
      ;; first line.
      `((:ldstr ,fn-name)
        ,@args-array-instrs (:ldc-i4 ,key-start)
        (:const-str-array ,(mapcar #'first key-specs))
        ,@(if has-explicit
              ;; Pass package array for explicit key matching. NIL entries are
              ;; nulls in the array: only keys written with an explicit package
              ;; have one, and the runtime compares by name for the rest.
              `((:const-str-array ,(mapcar #'fifth key-specs))
                (:call "Runtime.CheckNoUnknownKeys2"))
              `((:call "Runtime.CheckNoUnknownKeys")))))))

(defun direct-param-instrs (required local-keys arg0-offset native-p)
  "Parameter-binding instructions for the direct calling convention (params
   arrive as real .NET arguments). ARG0-OFFSET is 1 when arg0 carries the
   self LispFunction (native bodies, and the self-as-arg0 rewrite), else 0.
   NATIVE-P stores Int64 params into raw long slots — native bodies never
   box, so that variant has no boxed-var branch."
  (if native-p
      (loop for p in required
            for key = (cdr (assoc p local-keys))
            for i from arg0-offset
            append `((:declare-local ,key "Int64")
                     (:ldarg ,i) (:stloc ,key)
                     ,@(%local-var-marker key p :long)))
      (loop for p in required
            for key = (cdr (assoc p local-keys))
            for i from arg0-offset
            if (boxed-var-p p)
              append (emit-box-create key (list (list :ldarg i)) p)
            else
              append `((:declare-local ,key "LispObject")
                       (:ldarg ,i) (:stloc ,key)
                       ,@(%local-var-marker key p)))))

(defun direct-self-fn-prelude (self-fn-local fn-name fn-pkg setf-fn-p setf-target-name)
  "Prologue caching the current function's LispFunction in SELF-FN-LOCAL for
   non-tail self-calls. (SETF NAME) functions look up SetfFunction on the
   target NAME symbol; plain functions look up Function on their own symbol."
  (if setf-fn-p
      `((:declare-local ,self-fn-local "LispFunction")
        ,@(if fn-pkg
              `((:load-sym-pkg ,setf-target-name ,fn-pkg))
              `((:load-sym-fn ,setf-target-name ,(package-name *package*))))
        (:castclass "Symbol")
        (:call "CilAssembler.GetSetfFunctionBySymbol")
        (:stloc ,self-fn-local))
      `((:declare-local ,self-fn-local "LispFunction")
        ,@(if fn-pkg
              `((:load-sym-pkg ,fn-name ,fn-pkg))
              `((:load-sym-fn ,fn-name ,(package-name *package*))))
        (:castclass "Symbol")
        (:call "CilAssembler.GetFunctionBySymbol")
        (:stloc ,self-fn-local))))

(defvar *lift-block-tags* nil)

(defun %lift-param-block-tags (params local-keys)
  "The *BLOCK-TAGS* entries a lifted function gets from its own parameters."
  (loop for outer in *lift-block-tags*
        for bname = (car outer)
        for tag-var = (block-tag-var-name bname)
        for hit = (find-if (lambda (p) (string= (var-name p) tag-var)) params)
        when hit
          collect (cons bname (list (cdr (assoc hit local-keys :test #'eq))
                                    nil nil nil nil (sixth (cdr outer))))))

(defun compile-function-body-direct (params body &optional (fn-name "") fn-pkg fn-symbol)
  "Compile function body with direct parameter passing (no args array).
   Only for functions with exactly required params, no optional/key/rest.
   Params are accessed via (:ldarg 0), (:ldarg 1), ... directly.
   FN-PKG, if given, is the defining package name — used by the self-call
   symbol-lookup cache.
   FN-SYMBOL, if given, is the defun symbol — used for native eligibility check."
  (warn-unknown-declared-types body)
  (multiple-value-bind (required optional key rest-param aux) (parse-lambda-list params)
    (declare (ignore optional key rest-param aux))
    (let* ((all-params required)
           ;; Fresh scope tables. local-functions is normally empty (a defun body
           ;; sees no lexical local functions) — but during speculative
           ;; labels-self-TCO compilation, inject THIS function's own box as a
           ;; local-function: a self-reference that becomes a TCO branch never
           ;; consults it, but one that does NOT (non-tail, arity mismatch, inside a
           ;; try region, #'g, an optimizer gate) falls to the local-fn path and emits
           ;; a (:ldloc box-key) the acceptance scan detects — forcing the safe
           ;; closure-path fallback. Keyed off fn-name so nested inner lambdas
           ;; (fn-name "") never pick it up.
           (*cstate* (cstate-with *cstate*
                       +cs-locals+ '() +cs-boxed-vars+ '()
                       +cs-block-tags+ '() +cs-go-tags+ '()
                       +cs-local-functions+
                       (let ((spec (cstate-labels-direct-speculation)))
                         (if (and spec (string= fn-name (car spec)))
                             (list (list (car spec) (cdr spec) t))
                             '()))))
           ;; NOTINLINE in this body disables matching compiler macros (CLHS 3.2.2.1.1).
           (*notinline-functions* (extract-notinline body))
           ;; The rest of the closure-boundary reset set. Bound AFTER
           ;; *local-functions* above, which is the one deliberate reader of
           ;; the labels-direct-speculation slot — the speculation is consulted
           ;; at the boundary and must not stay visible inside the body.
           ;; NOT reset: the tco-self-symbol and tco-local-fn-key slots are
           ;; parameters the CALLER hands in through its *CSTATE* binding —
           ;; compile-defun passes the defun symbol, the labels path passes the
           ;; self-TCO key — and the self-call fast path in this body reads them.
           (*cstate* (cstate-with *cstate*
                                  +cs-labels-direct-speculation+ nil
                                  +cs-native-double-locals+ nil
                                  +cs-native-single-locals+ nil
                                  +cs-native-decimal-locals+ nil
                                  +cs-dotnet-typed-locals+ nil
                                  +cs-tco-in-try-catch+ nil
                                  +cs-tco-leave-instrs+ nil))
           ;; Try-region context belongs to the enclosing method.
           (*in-finally-block* nil)
           (local-keys (gen-param-local-keys all-params))
           (needs-boxing (params-needs-boxing body all-params))
           ;; Pre-check native eligibility: all fixnum params, fixnum return, no captures
           ;; Full check (including no specials) happens after special-param-syms is computed,
           ;; but we need this early for param-instrs type selection.
           (pre-special-syms
             (when (and fn-symbol (null needs-boxing) (all-params-fixnum-p params body))
               (%union-eq (fn-body-special-params body all-params)
                      (remove-if-not #'global-special-p all-params))))
           (pre-native-eligible
             (and fn-symbol
                  (null needs-boxing)
                  (all-params-fixnum-p params body)
                  (null pre-special-syms)
                  (eq 'fixnum (gethash fn-symbol *function-return-types*)))))
      (let ((*cstate* (cstate-with *cstate*
                        +cs-locals+ local-keys
                        +cs-boxed-vars+ (params-boxed-vars all-params needs-boxing)
                        +cs-block-tags+ (%lift-param-block-tags all-params local-keys)
                        +cs-no-safepoint+ (body-declares-safety-0-p body)))
            (*lift-block-tags* nil)
            (*symbol-macros* (params-shadowed-symbol-macros all-params)))
        (let ((param-instrs
                ;; Native body: arg0 is the self LispFunction (threaded for
                ;; self-calls), so the long params start at ldarg 1.
                (if pre-native-eligible
                    (direct-param-instrs required local-keys 1 t)
                    (direct-param-instrs required local-keys 0 nil))))
          (let* ((special-param-syms
                   (%union-eq (fn-body-special-params body all-params)
                          (remove-if-not #'global-special-p all-params)))
                 (special-push-instrs
                   (loop for p in special-param-syms
                         for pkey = (cdr (assoc p local-keys))
                         append `(,@(compile-sym-lookup p)
                                  (:castclass "Symbol")
                                  (:ldloc ,pkey)
                                  (:call "DynamicBindings.Push"))))
                 (*cstate* (cstate-with *cstate* +cs-locals+
                                        (remove-locals-shadowed-by
                                         special-param-syms (cstate-locals)))))
            ;; TCO scope: enabled for named functions without special params.
            ;; Special params require a try/finally block (DynamicBindings.Pop) and
            ;; CIL's plain Br can't escape it (needs Leave). Boxed params are fine —
            ;; compile-named-call's TCO branch writes through the box via stelem-ref
            ;; (relaxed the prior (null needs-boxing) restriction; *tco-param-entries*
            ;; now carries per-param boxed-p so the self-call code chooses the right path).
            ;; Always reset TCO state so inner lambdas don't inherit outer TCO context.
            (let* ((use-tco (and (string/= fn-name "")
                                 (null special-param-syms)))
                   (tco-loop-label (when use-tco (gen-label "TCOLOOP")))
                   ;; Reset mutual-TCO alongside: closures compiled within a labels
                   ;; group must not emit br-to-outer-TCOLOOP.
                   ;; Self-fn local: holds the LispFunction used by non-tail self-calls.
                   ;; - Native bodies: the self LispFunction arrives as arg0, so use
                   ;;   the sentinel :ARG0 — self-call sites load (:ldarg 0) and NO prelude
                   ;;   symbol-lookup runs per recursive entry.
                   ;; - Labels functions are stored in boxes, not symbols — skip.
                   (*cstate* (cstate-with *cstate*
                               +cs-tco-self-name+ (if use-tco fn-name nil)
                               +cs-tco-loop-label+ (if use-tco tco-loop-label nil)
                               +cs-labels-mutual-tco+ nil
                               +cs-self-fn-local+
                               (cond ((and pre-native-eligible use-tco) :arg0)
                                     ((and use-tco (null (cstate-tco-local-fn-key)))
                                      (gen-local "SELF-FN")))))
                   ;; Self-fn caching: for (SETF NAME) functions, look up SetfFunction
                   ;; on the target NAME symbol rather than Function on "(SETF NAME)"
                   ;; (fix broken GetFunctionBySymbol call for setf functions).
                   (setf-fn-p (and (cstate-self-fn-local)
                                   (not (eq (cstate-self-fn-local) :arg0))
                                   (> (length fn-name) 7)
                                   (string= fn-name "(SETF " :end1 6)))
                   (setf-target-name (when setf-fn-p
                                       (subseq fn-name 6 (1- (length fn-name)))))
                   (self-fn-prelude
                     (when (and (cstate-self-fn-local)
                                (not (eq (cstate-self-fn-local) :arg0)))
                       (direct-self-fn-prelude (cstate-self-fn-local) fn-name fn-pkg
                                               setf-fn-p setf-target-name)))
                   (*cstate* (cstate-with *cstate* +cs-tco-param-entries+
                                          (if use-tco
                                              (loop for p in required
                                                    for key = (cdr (assoc p local-keys))
                                                    collect (cons key (boxed-var-p p)))
                                              nil)))
                   ;; Function body last form is in tail position:
                   ;; - TCO rewrite applies only when the tco-self-name slot is set
                   ;; - MV return propagation: tail form doesn't unwrap MvReturn
                   ;; No reset at function ENTRY is needed on this path: nothing
                   ;; compiles before this binding, so the value inherited from
                   ;; the caller is never observed (see inline-tail-probe.lisp —
                   ;; at inline call sites that inheritance is what makes an
                   ;; inlined tail call ride the caller's TCO loop).
                   (*in-tail-position* t)
                   ;; Fixnum type declarations on params — consulted by fixnum-typed-p
                   ;; and compile-as-long for native int64 paths.
                   (*fixnum-locals* (append (extract-fixnum-locals body)
                                            (drop-shadowed-type-locals
                                             (mapcar #'var-name all-params)
                                             *fixnum-locals*)))
                   ;; Bounded-integer type declarations on params (signed-byte/
                   ;; unsigned-byte/bit) → tight range gating native int64 arith.
                   (*cstate* (cstate-with *cstate* +cs-small-int-locals+
                                          (append (rekey-by-param
                                                   (extract-small-int-locals body)
                                                   all-params local-keys)
                                                  (cstate-small-int-locals))))
                   ;; Double-float type declarations on params.
                   (*double-float-locals* (append (extract-double-float-locals body)
                                                  (drop-shadowed-type-locals
                                                   (mapcar #'var-name all-params)
                                                   *double-float-locals*)))
                   (*single-float-locals* (append (extract-single-float-locals body)
                                                  (drop-shadowed-type-locals
                                                   (mapcar #'var-name all-params)
                                                   *single-float-locals*)))
                   ;; Decimal type declarations on params: native
                   ;; System.Decimal arithmetic in the body, scale preserved.
                   (*decimal-locals* (append (extract-decimal-locals body)
                                             (drop-shadowed-type-locals
                                              (mapcar #'var-name all-params)
                                              *decimal-locals*)))
                   ;; Float-array type declarations on params/locals (e.g.
                   ;; (simple-array double-float (*))) → aref rides native r8.
                   (*cstate* (cstate-with *cstate* +cs-numeric-array-locals+
                                          (append (extract-float-array-locals body)
                                                  (cstate-numeric-array-locals))))
                   ;; Native body: params are Int64, enabling compile-as-long without
                   ;; unbox and native self-calls via InvokeNativeN.
                   (*cstate* (cstate-with *cstate*
                               +cs-long-locals+
                               (if (and pre-native-eligible use-tco)
                                   (mapcar (lambda (p) (cdr (assoc p local-keys)))
                                           all-params)
                                   (cstate-long-locals))
                               +cs-native-self-name+
                               (if (and pre-native-eligible use-tco)
                                   fn-name
                                   (cstate-native-self-name))))
                   ;; If we'll wrap in try/finally for DynamicBindings.Pop, body
                   ;; compilation must know — so tail-position calls don't emit
                   ;; the `.tail` prefix (illegal in CIL inside try) and TCO
                   ;; branches don't try to cross the try boundary.
                   (*in-try-block* (or *in-try-block* (not (null special-param-syms))))
                   (body-instrs (compile-progn body))
                   ;; Extend the native self-as-arg0 threading to NON-native direct
                   ;; functions. The per-entry self-fn-prelude (load-sym→castclass→
                   ;; GetFunctionBySymbol) only survives JIT DCE when the body does a
                   ;; non-tail self-call (= references *self-fn-local*). For exactly those
                   ;; functions, thread the self LispFunction in as a hidden arg0 (shift
                   ;; the LispObject params to ldarg 1..n, rewrite the self-call receiver
                   ;; to ldarg 0, drop the prelude). The caller emits :self t so the
                   ;; backend gives the direct method a leading LispFunction param and
                   ;; binds _funcN's target to fn. Non-recursive direct functions keep
                   ;; arg0 = first param so their apply/array path needs no symbol lookup.
                   ;; Does the body actually consume the self LispFunction? Only a
                   ;; NON-TAIL self-call does (tail ones become a TCO branch), and
                   ;; most functions have none — so for most functions the prelude
                   ;; is dead. It cannot be dropped by the JIT (GetFunctionBySymbol
                   ;; is an opaque call), so it has to be dropped here.
                   (self-fn-used-p (and (cstate-self-fn-local)
                                        (not (eq (cstate-self-fn-local) :arg0))
                                        (%sil-references-local-p body-instrs
                                                                 (cstate-self-fn-local))))
                   (self-arg0-p (and (not pre-native-eligible)
                                     self-fn-used-p
                                     (null special-param-syms)))
                   (eff-param-instrs
                     (if self-arg0-p
                         (direct-param-instrs required local-keys 1 nil)
                         param-instrs))
                   ;; Threaded in as arg0, or never read: either way no prelude.
                   (eff-self-fn-prelude (if self-fn-used-p
                                            (if self-arg0-p '() self-fn-prelude)
                                            '()))
                   (eff-body-instrs (if self-arg0-p
                                        (%sil-subst-self-arg0 body-instrs
                                                              (cstate-self-fn-local))
                                        body-instrs)))
              (values
               (merge-disjoint-locals
                (if special-param-syms
                    `(,@(%frame-enter-instrs fn-name)
                      ,@param-instrs
                      ,@(if self-fn-used-p self-fn-prelude '())
                      ,@(when use-tco `((:label ,tco-loop-label)))
                      ,@(compile-let-with-specials '() special-push-instrs body-instrs special-param-syms)
                      (:ret))
                    `(,@(%frame-enter-instrs fn-name)
                      ,@eff-param-instrs
                      ,@eff-self-fn-prelude
                      ,@(when use-tco `((:label ,tco-loop-label)))
                      ,@(maybe-tail-callvirt eff-body-instrs)
                      (:ret))))
               self-arg0-p))))))))

(defun wrap-aux-body (aux body)
  "Wrap BODY in (let* AUX ...) for &aux parameters. If BODY is a single
   (block name . forms), push the let* INSIDE the block so leading
   (declare (special x)) stays directly in the let* body — otherwise it gets
   buried under the block and compile-let* can't see it, so a body-level
   special declaration for a captured free var is lost."
  (if (null aux)
      body
      (if (and (= (length body) 1)
               (consp (car body))
               (eq (caar body) 'block))
          (let ((block-name (cadar body))
                (block-forms (cddar body)))
            `((block ,block-name (let* ,aux ,@block-forms))))
          `((let* ,aux ,@body)))))

(defun compile-function-body-inner (params body args-arg-idx &optional (fn-name ""))
  "Compile function body, loading args from (:ldarg ARGS-ARG-IDX).
   For normal functions args-arg-idx=0, for closures args-arg-idx=1."
  (warn-unknown-declared-types body)
  (multiple-value-bind (required optional key rest-param aux allow-other-keys-p has-key-p) (parse-lambda-list params)
    (let ((body (wrap-aux-body aux body)))
    (let* ((key-supplied-p-vars (remove nil (mapcar #'fourth key)))
           (opt-supplied-p-vars (remove nil (mapcar #'third optional)))
           (all-params (append required
                               (mapcar #'car optional)
                               opt-supplied-p-vars
                               (mapcar #'second key)
                               key-supplied-p-vars
                               (if rest-param (list rest-param) nil)))
           ;; NOTINLINE in this body disables matching compiler macros for calls
           ;; within it (CLHS 3.2.2.1.1). Fresh function scope → this body only.
           (*notinline-functions* (extract-notinline body))
           (*cstate* (cstate-fresh-function-body))
           (*in-tail-position* nil)
           ;; Try-region context belongs to the enclosing method: this body has
           ;; its own regions, so a TCO branch in it needs no suppression.
           (*in-try-block* nil)
           (*in-finally-block* nil)
           (local-keys (gen-param-local-keys all-params))
           (needs-boxing (params-needs-boxing body all-params))
           (n-required (length required))
           (key-start (+ n-required (length optional))))
      (let ((*cstate* (cstate-with *cstate*
                        +cs-locals+ local-keys
                        +cs-boxed-vars+ (params-boxed-vars all-params needs-boxing)
                        +cs-no-safepoint+ (body-declares-safety-0-p body)))
            (*symbol-macros* (params-shadowed-symbol-macros all-params)))
        ;; Generate parameter binding instructions via the shared args-array
        ;; machinery (compile-args-param-instrs); the args array lives at
        ;; ldarg ARGS-ARG-IDX here, and defaults mask current+later params
        ;; by filtering the full *locals*.
        (let ((param-instrs
                (compile-args-param-instrs
                 required optional key rest-param local-keys n-required key-start
                 (lambda (i) `((:ldarg ,args-arg-idx) (:ldc-i4 ,i) (:ldelem-ref)))
                 `((:ldarg ,args-arg-idx))
                 (lambda (names)
                   (remove-if (lambda (entry)
                                (member (var-name (car entry)) names :test #'string=))
                              (cstate-locals))))))
          (let* ((arity-instrs
                  (compile-args-arity-instrs fn-name optional key rest-param has-key-p
                                             n-required `((:ldarg ,args-arg-idx))))
                (key-check-instrs
                  (compile-args-key-check-instrs fn-name key has-key-p allow-other-keys-p
                                                 key-start `((:ldarg ,args-arg-idx)))))
           ;; Handle special params: both (declare (special param)) and
           ;; globally special params (defvar/*name* convention) bind dynamically
           (let* ((special-param-syms
                    (%union-eq (fn-body-special-params body all-params)
                           (remove-if-not #'global-special-p all-params)))
                  (special-push-instrs
                    (loop for p in special-param-syms
                          for pkey = (cdr (assoc p local-keys))
                          append `(,@(compile-sym-lookup p)
                                   (:castclass "Symbol")
                                   (:ldloc ,pkey)
                                   (:call "DynamicBindings.Push"))))
                  (*cstate* (cstate-with *cstate* +cs-locals+
                                         (remove-locals-shadowed-by
                                          special-param-syms (cstate-locals))))
                  ;; Tail position preserves MvReturn for multi-value callers
                  (*in-tail-position* t)
                  (body-instrs (compile-progn body)))
             (merge-disjoint-locals
              (if special-param-syms
                  `(,@arity-instrs
                    ,@key-check-instrs
                    ,@(%frame-enter-instrs fn-name)
                    ,@param-instrs
                    ,@(compile-let-with-specials '() special-push-instrs body-instrs special-param-syms)
                    (:ret))
                  `(,@arity-instrs
                    ,@key-check-instrs
                    ,@(%frame-enter-instrs fn-name)
                    ,@param-instrs
                    ,@body-instrs
                    (:ret))))))))))))

;;; ============================================================
;;; let / let*
;;; ============================================================

(defun extract-specials (body)
  "Extract (declare (special ...)) from the beginning of body.
   Strips ALL declarations (ignore, type, etc.) from remaining-body.
   Returns (values specials-list remaining-body)."
  (let ((specials '())
        (rest-body body))
    (loop while (and rest-body
                     (consp (car rest-body))
                     (eq (caar rest-body) 'declare))
          do (dolist (decl (cdar rest-body))
               (when (and (consp decl) (eq (car decl) 'special))
                 (dolist (s (cdr decl))
                   (push s specials))))
             (pop rest-body))
    (values specials rest-body)))

(defun extract-fixnum-locals (body)
  "Scan (declare ...) forms at the head of BODY for fixnum type hints
   on locals. Recognizes:
     (declare (fixnum x y z))
     (declare (type fixnum x y))
   NOTE: `(type (integer LO HI) x)` is NOT treated as fixnum even when
   bounds fit — multiplication on such a var could produce a result outside
   fixnum range that we'd silently wrap (CL mandates bignum promotion).
   Users who want the optimization must declare `fixnum` explicitly.
   Returns a list of symbol-names (strings) declared fixnum."
  (let ((result '()))
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare))
        (return))
      (dolist (decl (cdr form))
        (when (consp decl)
          (cond
            ;; (fixnum x y ...)
            ((eq (car decl) 'fixnum)
             (dolist (v (cdr decl))
               (when (symbolp v) (pushnew (var-name v) result :test #'string=))))
            ;; (type fixnum x y ...)
            ((and (eq (car decl) 'type) (eq (cadr decl) 'fixnum))
             (dolist (v (cddr decl))
               (when (symbolp v) (pushnew (var-name v) result :test #'string=))))))))
    result))

(defun extract-small-int-locals (body)
  "Scan head (declare ...) forms for bounded integer type hints —
   (signed-byte N) / (unsigned-byte N) / bit / (integer LO HI) — on locals.
   Returns an alist (name-string . (LO . HI)) for those whose range fits int64.
   Unlike extract-fixnum-locals these carry a TIGHT range, so expr-int-range can
   prove a product stays in int64 (native) or, when it can't, fall back to the
   promoting path (bignum). This is the CL-compliant generalization of the case
   extract-fixnum-locals deliberately punted on before the range gate."
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
            (let ((range (integer-type-range type)))
              (when (and range (range-fits-int64-p range))
                (dolist (v vars)
                  (when (symbolp v)
                    (pushnew (cons (var-name v) range) result
                             :key #'car :test #'string=)))))))))
    result))

(defparameter +native-int-ops+
  '(+ - * 1+ 1- logand logior logxor lognot ash logbitp
    zerop plusp minusp evenp oddp = /= < > <= >= min max
    aref svref elt char schar the)
  "Operators whose compiler support reads an integer argument as a raw int64
   (compile-as-long), so a reference in argument position costs no unbox when
   the variable's slot is already native.")

(defun %native-int-use-p (var body)
  "True when BODY syntactically reads VAR as an argument of a +NATIVE-INT-OPS+
   form. Gates the range-proven Int64 slot promotion in COMPILE-LET: a raw slot
   pays for itself only when some read skips an unbox, and a variable used purely
   in generic positions would just gain a Fixnum.Make on every read.

   Deliberately syntactic and under-approximating — BODY is unexpanded source
   here, so a use that only appears after macroexpansion is not seen and the
   binding simply stays boxed. Over-approximating in the other direction is
   harmless too: a wrong guess costs performance, never correctness, since both
   representations of the slot are fully compiled for."
  (labels ((arg-p (args)
             (do ((a args (cdr a)))
                 ((not (consp a)) nil)
               (when (eq (car a) var) (return t))))
           (walk (form)
             (and (consp form)
                  (let ((head (car form)))
                    (cond
                      ((eq head 'quote) nil)
                      ((and (symbolp head)
                            (member head +native-int-ops+ :test #'eq)
                            (arg-p (cdr form)))
                       t)
                      (t (do ((r form (cdr r)))
                             ((not (consp r)) nil)
                           (when (walk (car r)) (return t)))))))))
    (do ((r body (cdr r)))
        ((not (consp r)) nil)
      (when (walk (car r)) (return t)))))

(defun infer-small-int-bindings (binding-info needs-boxing mutated)
  "For plain lexical (non-special, non-boxed, non-mutated) let bindings whose
   init has a statically provable int64 range (expr-int-range), return an alist
   (SLOT-KEY . (LO . HI)) to extend *small-int-locals* for the body. This is
   what makes an undeclared let var like crc's new-rmdr = (logior bit (* rmdr 2))
   participate in native int64 arithmetic. Mutated bindings are excluded: a setf
   could move the value outside the init range, breaking the range gate's
   soundness (and the inline unbox-fixnum, which assumes a Fixnum). Init ranges
   are evaluated against the enclosing *small-int-locals* (callers leave the
   prior binding in effect), so the result is sound for both let and let*."
  (let ((result '()))
    (dolist (b binding-info)
      (let ((var (first b)) (init (second b)) (is-special (third b)))
        (when (and (not is-special)
                   init
                   (not (member (var-name var) needs-boxing :test #'string=))
                   (not (member (var-name var) mutated :test #'string=)))
          (let ((r (expr-int-range init)))
            (when (and r (range-fits-int64-p r))
              ;; Keyed by slot (fourth b), matching SMALL-INT-LOCAL-RANGE.
              (push (cons (fourth b) r) result))))))
    result))

(defun %dotnet-sym-p (x name)
  "True when X is a symbol named NAME in the DOTNET package. Cross-compile safe:
   on the SBCL host there is no DOTNET package, so this is simply always NIL and
   no .NET type inference happens during cross-compile."
  (and (symbolp x) (string= (symbol-name x) name)
       (let ((p (symbol-package x)))
         (and p (string= (package-name p) "DOTNET")))))

(defun %dotnet-init-type (init)
  "If the let/let* init form INIT statically denotes a typed .NET value, return
   its .NET type-name string, else NIL. Mirrors the receiver/arg type sources the
   DOTNET:INVOKE compiler macro already understands:
     (the (dotnet \"T\") E)  -> \"T\"   (E is asserted to be a .NET object of T)
     (dotnet:new \"T\" ...)   -> \"T\"
     (dotnet:box E \"T\")     -> \"T\"
   The variable bound to such an init holds a .NET object of T, so a later
   (dotnet:invoke var ...) can be lowered to a typed direct callvirt."
  (and (consp init)
       (let ((head (car init)))
         (cond
           ((and (symbolp head) (string= (symbol-name head) "THE")
                 (consp (cdr init)) (consp (cadr init)) (consp (cddr init)))
            (let ((spec (cadr init)))
              (and (symbolp (car spec)) (string= (symbol-name (car spec)) "DOTNET")
                   (consp (cdr spec)) (stringp (cadr spec))
                   (cadr spec))))
           ((%dotnet-sym-p head "NEW")
            (and (consp (cdr init)) (stringp (cadr init)) (cadr init)))
           ((%dotnet-sym-p head "BOX")
            (and (consp (cdr init)) (consp (cddr init)) (stringp (caddr init))
                 (caddr init)))
           (t nil)))))

(defun infer-dotnet-typed-bindings (binding-info mutated outer)
  "Compute the .NET-typed-locals environment for a let/let* body. Start from OUTER
   (the enclosing environment), drop every name this let binds (so a rebinding
   shadows an outer typed local even when the new init is untyped), then add an
   entry for each non-special, non-mutated binding whose init denotes a typed .NET
   value. Each new entry is (name-string type-string . local-key); the key lets
   the consumer verify the binding is still the live one (see
   DOTNET-VALID-TYPED-LOCALS). Mutated bindings are excluded because a setf could
   replace the value with a different type, which would miscompile the direct
   callvirt."
  (let* ((bound-names (mapcar (lambda (b) (var-name (first b))) binding-info))
         (result (remove-if (lambda (e) (member (car e) bound-names :test #'string=))
                            outer)))
    (dolist (b binding-info)
      (let ((var (first b)) (init (second b)) (is-special (third b)) (key (fourth b)))
        (unless (or is-special
                    (member (var-name var) mutated :test #'string=))
          (let ((type (%dotnet-init-type init)))
            (when type
              (push (list* (var-name var) type key) result))))))
    result))

(defun extract-double-float-locals (body)
  "Parallel to extract-fixnum-locals for double-float."
  (let ((result '()))
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare))
        (return))
      (dolist (decl (cdr form))
        (when (consp decl)
          (cond
            ((eq (car decl) 'double-float)
             (dolist (v (cdr decl))
               (when (symbolp v) (pushnew (var-name v) result :test #'string=))))
            ((and (eq (car decl) 'type) (eq (cadr decl) 'double-float))
             (dolist (v (cddr decl))
               (when (symbolp v) (pushnew (var-name v) result :test #'string=))))))))
    result))

(defun decimal-type-name-p (sym)
  "T iff SYM is specifically DOTCL:DECIMAL, keyed on symbol-NAME *and* home
   PACKAGE-NAME. Both are cross-compile-safe strings (dotcl:decimal need not exist on
   the SBCL host, so EQ is unusable), yet the package check keeps a bare or foreign
   DECIMAL (CL-USER::DECIMAL, my-lib::DECIMAL) from being mistaken for the CLR decimal
   type and mis-triggering native decimal codegen. Under (use-package :dotcl) an
   unqualified DECIMAL resolves to dotcl:decimal and still matches."
  (and (symbolp sym)
       (string= (symbol-name sym) "DECIMAL")
       (let ((p (symbol-package sym)))
         (and p (string= (package-name p) "DOTCL")))))

(defun extract-decimal-locals (body)
  "Parallel to extract-double-float-locals for decimal. Only the
   (type decimal ...) / (decimal ...) forms — decimal is non-standard, so there is
   no bare shorthand class name to also accept beyond these."
  (let ((result '()))
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare))
        (return))
      (dolist (decl (cdr form))
        (when (consp decl)
          (cond
            ((decimal-type-name-p (car decl))
             (dolist (v (cdr decl))
               (when (symbolp v) (pushnew (var-name v) result :test #'string=))))
            ((and (symbolp (car decl)) (string= (symbol-name (car decl)) "TYPE")
                  (decimal-type-name-p (cadr decl)))
             (dolist (v (cddr decl))
               (when (symbolp v) (pushnew (var-name v) result :test #'string=))))))))
    result))

(defun extract-single-float-locals (body)
  "Parallel to extract-double-float-locals for single-float."
  (let ((result '()))
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare))
        (return))
      (dolist (decl (cdr form))
        (when (consp decl)
          (cond
            ((eq (car decl) 'single-float)
             (dolist (v (cdr decl))
               (when (symbolp v) (pushnew (var-name v) result :test #'string=))))
            ((and (eq (car decl) 'type) (eq (cadr decl) 'single-float))
             (dolist (v (cddr decl))
               (when (symbolp v) (pushnew (var-name v) result :test #'string=))))))))
    result))

(defun extract-float-array-locals (body)
  "Scan the head declare forms of BODY for variables declared with a float
   simple-array/array/vector type of statically-known rank 1-3, returning
   *numeric-array-locals* entries (NAME KEY RANK . :single/:double). This lets
   aref on a float-array-typed PARAMETER or declared local (e.g. the fft/
   mandelbrot kernels' (simple-array single-float (1025)) / (simple-array
   double-float (6))) ride the native r8 path (Runtime.ArefNum*D), not just
   make-array let-locals. Must be called with *locals* bound (KEY = the var's
   slot via lookup-local, which numeric-array-aref-entry re-checks so a shadow
   or a re-key self-invalidates). The runtime fast path re-checks _numKind, so a
   caller that passes a non-float-backed array falls back to the boxed coerce
   path rather than corrupting data — the declaration is a soundness-safe hint."
  (let ((result '()))
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare)) (return))
      (dolist (decl (cdr form))
        (when (and (consp decl) (eq (car decl) 'type))
          (let ((kr (%array-type-float-kind-and-rank (cadr decl))))
            (when kr
              (dolist (v (cddr decl))
                (when (and (symbolp v) (lookup-local v) (not (boxed-var-p v)))
                  (pushnew (list* (var-name v) (lookup-local v) (cdr kr) (car kr))
                           result :test #'string= :key #'car))))))))
    result))

;;; ============================================================
;;; Return type inference
;;; ============================================================

(defun extract-declared-var-types (body)
  "Return alist of (var-symbol . type) from declare forms at head of BODY.
  Recognizes (fixnum x y), (double-float x), (type fixnum x y)."
  (let ((types '()))
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare)) (return))
      (dolist (decl (cdr form))
        (when (consp decl)
          (let ((dtype
                  (cond
                    ((member (car decl) '(fixnum double-float single-float
                                                 character boolean bit))
                     (car decl))
                    ((and (eq (car decl) 'type) (symbolp (cadr decl)))
                     (cadr decl))
                    (t nil))))
            (when dtype
              (dolist (v (if (eq (car decl) 'type) (cddr decl) (cdr decl)))
                (when (symbolp v) (push (cons v dtype) types))))))))
    types))

(defun infer-expr-return-type (expr var-types self-name)
  "Conservatively infer the return type of EXPR.
  VAR-TYPES is an alist (sym . type). SELF-NAME is the mangled fn name for
  self-recursion detection. Returns 'fixnum, 'double-float, :self-recursive,
  or NIL (unknown). :self-recursive means 'same as the enclosing function',
  handled by meet-inferred-types."
  (cond
    ((integerp expr)
     (when (<= most-negative-fixnum expr most-positive-fixnum) 'fixnum))
    ((symbolp expr)
     (cdr (assoc expr var-types)))
    ((and (consp expr) (eq (car expr) 'the) (symbolp (cadr expr)))
     (cadr expr))
    ((and (consp expr) (eq (car expr) 'progn))
     (when (cdr expr)
       (infer-expr-return-type (car (last expr)) var-types self-name)))
    ((and (consp expr) (eq (car expr) 'if))
     (let* ((then-t (infer-expr-return-type (caddr expr) var-types self-name))
            (else-t (if (cdddr expr)
                        (infer-expr-return-type (cadddr expr) var-types self-name)
                        nil)))
       (meet-inferred-types then-t else-t)))
    ;; Fixnum arithmetic on fixnum-typed operands
    ((and (consp expr) (symbolp (car expr))
          (member (car expr) '(+ - * 1+ 1- logand logior logxor min max abs))
          (every (lambda (a)
                   (eq 'fixnum (infer-expr-return-type a var-types self-name)))
                 (cdr expr)))
     'fixnum)
    ;; ash: a non-negative shift can overflow int64, so only infer fixnum when
    ;; the value provably fits — negative shift (right shift), or a constant base
    ;; and shift whose folded result is in int64 range. Must match fixnum-typed-p
    ;; so the native-long return ABI never compiles an overflowing SHL.
    ((and (consp expr) (= (length expr) 3) (eq (car expr) 'ash)
          (eq 'fixnum (infer-expr-return-type (cadr expr) var-types self-name))
          (integerp (caddr expr))
          (let ((n (caddr expr)))
            (or (< n 0)
                (and (integerp (cadr expr))
                     (typep (ash (cadr expr) n) '(signed-byte 64))))))
     'fixnum)
    ;; Self-recursive call
    ((and (consp expr) (symbolp (car expr)) self-name
          (string= (mangle-name (car expr)) self-name))
     :self-recursive)
    (t nil)))

(defun meet-inferred-types (t1 t2)
  "Merge two inferred types. :self-recursive is compatible with any concrete type."
  (cond
    ((equal t1 t2) t1)
    ((eq t1 :self-recursive) t2)
    ((eq t2 :self-recursive) t1)
    (t nil)))

(defun infer-body-return-type (body self-name)
  "Infer the return type of a function with BODY and mangled name SELF-NAME.
  Returns a concrete type symbol (e.g. 'fixnum) or NIL if unknown."
  (let* ((var-types (extract-declared-var-types body))
         (last-form (and body (car (last body)))))
    (when last-form
      (let ((ty (infer-expr-return-type last-form var-types self-name)))
        (when (and ty (not (eq ty :self-recursive)))
          ty)))))

(defun compile-let (bindings body sequential-p)
  "Compile (let/let* ...), wrapping the result in a debug scope so its lexical
   locals are named only within this binding form. Nested lets nest their scopes,
   giving the debugger correct shadowing (a method-wide scope can't disambiguate
   two same-named vars). No-op wrapper when debug info is off."
  (let ((result (%compile-let bindings body sequential-p)))
    (if *emit-source-lines*
        `((:scope-begin) ,@result (:scope-end))
        result)))

(defun compile-let-body-instrs (real-body body binding-info let-bound-names
                                needs-boxing mutated special-syms declared-specials)
  "Compile REAL-BODY in the scope a LET/LET* body sees, and return its
   instruction list. Establishes the body's own type declarations (name-keyed
   tables shadow-filtered by LET-BOUND-NAMES, slot-keyed tables rekeyed via
   BINDING-INFO), the inferred native/typed bindings, and the try-region flag
   when SPECIAL-SYMS forces a try/finally. DECLARED-SPECIALS non-NIL additionally
   drops those declared-special variables from the locals table (the sequential
   branch defers that removal to here; the parallel branch has already done it).
   Shared by both %COMPILE-LET branches — the scope rules of a LET body have
   one definition."
  (let ((*in-try-block* (or *in-try-block* special-syms))
        (*fixnum-locals*
         (append (extract-fixnum-locals body)
                 (drop-shadowed-type-locals let-bound-names *fixnum-locals*)))
        (*cstate*
         (let ((cs (cstate-with *cstate*
                     +cs-small-int-locals+
                     (append (rekey-by-binding
                              (extract-small-int-locals body)
                              binding-info)
                             (infer-small-int-bindings
                              binding-info needs-boxing mutated)
                             (cstate-small-int-locals))
                     +cs-numeric-array-locals+
                     (append
                      (extract-float-array-locals body)
                      (infer-numeric-array-bindings
                       binding-info mutated (cstate-numeric-array-locals)))
                     +cs-dotnet-typed-locals+
                     (infer-dotnet-typed-bindings
                      binding-info mutated (cstate-dotnet-typed-locals)))))
           (if declared-specials
               (cstate-with cs +cs-locals+
                            (remove-locals-shadowed-by declared-specials
                                                       (cstate-locals)))
               cs)))
        (*double-float-locals*
         (append (extract-double-float-locals body)
                 (drop-shadowed-type-locals let-bound-names *double-float-locals*)))
        (*single-float-locals*
         (append (extract-single-float-locals body)
                 (drop-shadowed-type-locals let-bound-names *single-float-locals*)))
        (*decimal-locals*
         (append (extract-decimal-locals body)
                 (drop-shadowed-type-locals let-bound-names *decimal-locals*))))
    (compile-progn real-body)))

(defun %compile-let (bindings body sequential-p)
  "Compile (let bindings body...) or (let* bindings body...)."
  (warn-unknown-declared-types body)
  (multiple-value-bind (declared-specials real-body) (extract-specials body)
    ;; Before the capture/mutation scan below, so that a variable the chunks
    ;; assign to is boxed like any other captured-and-mutated variable.
    (setf real-body (%maybe-chunk-let-body real-body body))
    (let* ((all-specials (append declared-specials *specials*))
           ;; Parse bindings
           (parsed (mapcar (lambda (b)
                             (if (consp b)
                                 (list (check-binding-name (car b) "LET") (cadr b))
                                 (list (check-binding-name b "LET") nil)))
                           bindings))
           ;; Classify into lexical and special
           ;; Only globally special vars (defvar/proclaim) or vars in this let's own declares
           ;; force dynamic binding. Outer locally-declared specials do NOT propagate to bindings.
           (binding-info
             (mapcar (lambda (p)
                       (let* ((var (first p))
                              (init (second p))
                              (is-special (or (member var declared-specials)
                                              (global-special-p var))))
                         (list var init is-special (gen-local (var-name var)))))
                     parsed))
           ;; Pre-scan for mutable captures in body (and init forms for let*)
           (var-names (mapcar #'first parsed))
           ;; For let*, lambdas in init forms can capture+mutate earlier vars
           (scan-forms (if sequential-p
                           (append (remove nil (mapcar #'second parsed)) real-body)
                           real-body))
           (mc (multiple-value-list
                (find-mutated-and-captured-vars scan-forms (mapcar #'var-name var-names))))
           (mutated (first mc))
           (captured (second mc))
           (needs-boxing (%strings-in-both mutated captured))
           ;; Separate
           (special-bindings (remove-if-not #'third binding-info))
           (lexical-bindings (remove-if #'third binding-info))
           ;; Long-rep candidates: fixnum-declared plain lexicals whose slot can
           ;; hold a raw Int64 instead of a boxed Fixnum. Requires: not special,
           ;; not captured by any closure (env capture loads the slot as an
           ;; object reference), and a fixnum-typed init so the initial store is
           ;; well-typed. Mutation is fine — compile-setq has an Int64-slot
           ;; path. References in boxed contexts re-box via Fixnum.Make; native
           ;; contexts (compare/arith/aref index) read the slot directly, which
           ;; is what removes the per-iteration box/unbox from loop counters.
           (fx-decl-names (extract-fixnum-locals body))
           (fx-rep-names
             (when fx-decl-names
               (loop for b in binding-info
                     for nm = (var-name (first b))
                     when (and (not (third b))
                               (second b)
                               (member nm fx-decl-names :test #'string=)
                               (not (member nm captured :test #'string=))
                               (fixnum-typed-p (second b)))
                       collect nm)))
           ;; The same slot promotion WITHOUT a fixnum declaration: a binding
           ;; whose init has a statically proven int64 range (EXPR-INT-RANGE).
           ;; crc-division-step's (let ((new-rmdr (logior bit (* rmdr 2)))) ...)
           ;; is the motivating case — its init is already computed with native
           ;; int64 ops and then boxed only to be unboxed again by the body.
           ;;
           ;; The proven range is also recorded in *small-int-locals* for the
           ;; body (INFER-SMALL-INT-BINDINGS uses the same predicate), and
           ;; EXPR-INT-RANGE consults that TIGHT range before the full-int64
           ;; *long-locals* clause, so promoting the slot never widens the
           ;; bounds a surrounding (* x y) is proven against.
           (range-rep-names
             (unless sequential-p
               ;; Parallel LET only. Its inits are evaluated in the enclosing
               ;; scope, which is exactly the environment EXPR-INT-RANGE sees
               ;; here. A LET*'s later inits are not: they see the earlier
               ;; siblings' bindings, so a range proved here could belong to an
               ;; outer variable of the same name — the same trap the native
               ;; decimal slots hit, where a LET*'s sibling init was declared
               ;; Decimal on the strength of an enclosing binding's type.
               (loop for b in binding-info
                     for nm = (var-name (first b))
                     when (and (not (third b))
                               ;; Compound init only. An atom (literal, another
                               ;; variable) costs nothing to store boxed, so
                               ;; promoting it only adds Fixnum.Make to every
                               ;; generic read.
                               (consp (second b))
                               (not (member nm fx-rep-names :test #'string=))
                               (not (member nm captured :test #'string=))
                               (not (member nm needs-boxing :test #'string=))
                               ;; Mutation excluded: an Int64 slot cannot hold a
                               ;; bignum, and proving that every SETQ stays in
                               ;; range needs the environment at each setq site,
                               ;; which is not available here. (This is also why
                               ;; INFER-SMALL-INT-BINDINGS drops mutated vars.)
                               (not (member nm mutated :test #'string=))
                               ;; Pay only where it pays: the body must read the
                               ;; variable in a native integer position, where
                               ;; the raw slot avoids an unbox. Otherwise the
                               ;; promotion is a pure loss (every generic read
                               ;; re-boxes).
                               (%native-int-use-p (first b) real-body)
                               (let ((r (expr-int-range (second b))))
                                 (and r (range-fits-int64-p r))))
                       collect nm)))
           (long-rep-names (append fx-rep-names range-rep-names))
           ;; Native-float-slot candidates: the r8/r4 analog of long-rep-names.
           ;; A double/single-float-declared plain lexical, not special, not
           ;; captured, with a float-typed init (so the initial native store is
           ;; well-typed). Its slot holds a raw double/float; the per-setq box in
           ;; numeric loops is eliminated (fft/mandelbrot accumulators).
           (dbl-decl-names (extract-double-float-locals body))
           (sgl-decl-names (extract-single-float-locals body))
           (double-rep-names
             (when dbl-decl-names
               (loop for b in binding-info
                     for nm = (var-name (first b))
                     when (and (not (third b)) (second b)
                               (member nm dbl-decl-names :test #'string=)
                               (not (member nm captured :test #'string=))
                               (double-float-typed-p (second b)))
                       collect nm)))
           (single-rep-names
             (when sgl-decl-names
               (loop for b in binding-info
                     for nm = (var-name (first b))
                     when (and (not (third b)) (second b)
                               (member nm sgl-decl-names :test #'string=)
                               (not (member nm captured :test #'string=))
                               (single-float-typed-p (second b)))
                       collect nm)))
           ;; Decimal-rep candidates: same rule as the float slots — declared
           ;; decimal, plain lexical, not captured, decimal-typed init.
           (dec-decl-names (extract-decimal-locals body))
           (decimal-rep-names
             (when dec-decl-names
               (loop for b in binding-info
                     for nm = (var-name (first b))
                     when (and (not (third b)) (second b)
                               (member nm dec-decl-names :test #'string=)
                               (not (member nm captured :test #'string=))
                               ;; Strong trigger only: a bare #m literal init is
                               ;; still on the standard tower, and a sibling init
                               ;; in a LET* does not see this LET's declarations,
                               ;; so a weak test would declare a Decimal slot for
                               ;; a value that arrives as a rational.
                               (or (decimal-literal-p (second b))
                                   (decimal-strong-typed-p (second b))))
                       collect nm))))
      ;; Build new locals alist
      (let* ((new-local-entries
               (mapcar (lambda (b)
                         (cons (first b) (fourth b)))
                       lexical-bindings))
             (init-instrs '())
             (bind-instrs '())
             (special-syms '()))
        ;; For let (not let*): evaluate all inits in old scope first
        (if (not sequential-p)
            ;; Let: evaluate all inits, store to temp locals (only when the
            ;; binding routes through a special-var Push or a boxed array).
            ;; Plain lexical, non-boxed bindings store the init result directly
            ;; into the final binding key — no temp needed.
            (let ((temp-keys
                    (mapcar (lambda (b)
                              (let ((var (first b))
                                    (is-special (third b)))
                                (if (or is-special
                                        (member (var-name var) needs-boxing
                                                :test #'string=))
                                    (gen-local "INIT")
                                    nil)))
                            binding-info)))
              ;; Evaluate inits in old scope — inits bind a single value:
              ;; never in tail position and never in MV context.
              (loop for b in binding-info
                    for tk in temp-keys
                    do (let* ((init-form (second b))
                              (key (fourth b))
                              (nm (var-name (first b)))
                              (nfk (cond ((member nm double-rep-names :test #'string=) :double)
                                         ((member nm single-rep-names :test #'string=) :single)
                                         (t nil))))
                         (cond
                           ((member nm long-rep-names :test #'string=)
                             ;; Long-rep lexical: raw Int64 slot, init lowered
                             ;; to a raw long (still in the OLD scope).
                             (setf init-instrs
                                   (append init-instrs
                                           `((:declare-local ,key "Int64")
                                             ,@(compile-expr-to-long init-form)
                                             (:stloc ,key)
                                             ,@(%local-var-marker key (first b) :long)))))
                           (nfk
                             ;; Native float rep: raw r8/r4 slot, init lowered
                             ;; to a native float (still in the OLD scope).
                             (setf init-instrs
                                   (append init-instrs
                                           `((:declare-local ,key ,(ecase nfk (:double "Double") (:single "Single")))
                                             ,@(compile-float-native-value init-form nfk)
                                             (:stloc ,key)
                                             ,@(%local-var-marker key (first b) nfk)))))
                           ((member nm decimal-rep-names :test #'string=)
                             ;; Native decimal rep: raw System.Decimal slot.
                             (setf init-instrs
                                   (append init-instrs
                                           `((:declare-local ,key "Decimal")
                                             ,@(compile-decimal-native-value init-form)
                                             (:stloc ,key)
                                             ,@(%local-var-marker key (first b) :decimal)))))
                           (t
                             (let ((init-code
                                     (let ((*in-tail-position* nil)
                                           (*in-mv-context* nil))
                                       (if init-form
                                           (compile-expr init-form)
                                           (emit-nil)))))
                               (if tk
                                   ;; Boxed / special: route via temp; bind-instrs
                                   ;; will consume it in the new scope.
                                   (setf init-instrs
                                         (append init-instrs
                                                 `((:declare-local ,tk "LispObject")
                                                   ,@init-code
                                                   (:stloc ,tk))))
                                   ;; Plain lexical: declare the final key here and
                                   ;; stloc directly. The new scope's *locals*
                                   ;; entry already points at this key, so no
                                   ;; further bind-instr is needed for this var.
                                   ;; Under debug, name this user lexical for the PDB.
                                   (setf init-instrs
                                         (append init-instrs
                                                 `((:declare-local ,key "LispObject")
                                                   ,@init-code
                                                   (:stloc ,key)
                                                   ,@(%local-var-marker key (first b)))))))))))
              ;; Now bind in new scope
              ;; Filter *boxed-vars* to remove names being rebound as non-boxed
              ;; %strings-minus, not SET-DIFFERENCE :test #'string= — both
              ;; operands are name strings and the result is only used as a
              ;; membership set below, so the required-only helper (typed direct
              ;; delegate) gives the same answer without the variadic :test XEP
              ;; on this per-LET path. Same rationale as params-needs-boxing.
              (let* ((non-boxed-names (%strings-minus
                                       (mapcar #'var-name var-names)
                                       needs-boxing))
                     (filtered-boxed (remove-if
                                      (lambda (x)
                                        (member (if (symbolp x) (var-name x) x)
                                                non-boxed-names :test #'string=))
                                      (cstate-boxed-vars)))
                     ;; Remove declared-specials from *locals* so references use dynamic binding
                     (*cstate* (cstate-with *cstate*
                                 +cs-locals+
                                 (remove-locals-shadowed-by
                                  declared-specials
                                  (append new-local-entries (cstate-locals)))
                                 +cs-boxed-vars+
                                 (append
                                  (mapcar (lambda (name)
                                            (find name var-names
                                                  :key #'var-name :test #'string=))
                                          needs-boxing)
                                  filtered-boxed)))
                     (*specials* all-specials)
                     ;; Shadow symbol-macros for variables being bound by this let
                     (*symbol-macros* (remove-if
                                       (lambda (entry)
                                         (member (var-name (car entry))
                                                 (mapcar #'var-name var-names)
                                                 :test #'string=))
                                       *symbol-macros*))
                     ;; Native-representation slots for the body, recorded by SLOT
                     ;; KEY. Nothing has to be dropped for the names this LET
                     ;; rebinds: those bindings own different slots, so a
                     ;; reference resolving to one of them is simply not in the
                     ;; table (see NATIVE-SLOT-P).
                     (*cstate*
                       (cstate-with *cstate*
                         +cs-long-locals+
                         (append (binding-keys-named binding-info long-rep-names)
                                 (cstate-long-locals))
                         +cs-native-double-locals+
                         (append (binding-keys-named binding-info double-rep-names)
                                 (cstate-native-double-locals))
                         +cs-native-single-locals+
                         (append (binding-keys-named binding-info single-rep-names)
                                 (cstate-native-single-locals))
                         +cs-native-decimal-locals+
                         (append (binding-keys-named binding-info decimal-rep-names)
                                 (cstate-native-decimal-locals)))))
                ;; Declare and store each binding. Plain lexical bindings (tk
                ;; is nil) were already finalized in init-instrs and need no
                ;; bind-instr here.
                (loop for b in binding-info
                      for tk in temp-keys
                      do (let ((var (first b))
                               (is-special (third b))
                               (key (fourth b)))
                           (cond
                             (is-special
                              (push var special-syms)
                              (setf bind-instrs
                                    (append bind-instrs
                                            `(,@(compile-sym-lookup var)
                                              (:castclass "Symbol")
                                              (:ldloc ,tk)
                                              (:call "DynamicBindings.Push")))))
                             ((member (var-name var) needs-boxing :test #'string=)
                              (setf bind-instrs
                                    (append bind-instrs
                                            (emit-box-create key (list (list :ldloc tk)) var))))
                             (t
                              ;; Plain lexical: already initialized in
                              ;; init-instrs via direct stloc to key.
                              nil))))
                ;; Compile body — inherits outer *in-tail-position*.
                ;; But when we have special bindings, the body is wrapped in a
                ;; try/finally (to Pop on exit), so self-TCO's raw `br` to the
                ;; loop label outside the try would produce invalid IL. Flag
                ;; *in-try-block* suppresses TCO branch generation in that case
                ;; while still allowing MV propagation through tail position.
                (let* ((let-bound-names (mapcar (lambda (b) (var-name (first b)))
                                                binding-info))
                       (body-instrs (compile-let-body-instrs
                                      real-body body binding-info let-bound-names
                                      needs-boxing mutated special-syms nil)))
                  (compile-let-with-specials
                   init-instrs bind-instrs body-instrs
                   (reverse special-syms)))))
            ;; Let*: sequential binding — incrementally extend *locals*
            (let ((*specials* all-specials)
                  (*symbol-macros* *symbol-macros*)
                  ;; Extended/shadowed incrementally as bindings are processed
                  ;; so each init sees exactly the earlier siblings' slots.
                  (*cstate* (cstate-with *cstate*
                              +cs-boxed-vars+
                              (append
                               (mapcar (lambda (name)
                                         (find name var-names
                                               :key #'var-name :test #'string=))
                                       needs-boxing)
                               (cstate-boxed-vars)))))
              ;; Bind one at a time; compile init THEN extend scope
              (loop for b in binding-info
                    do (let ((var (first b))
                             (init-form (second b))
                             (is-special (third b))
                             (key (fourth b)))
                           (if is-special
                               (let ((tmp (gen-local "SPLTMP")))
                                 (push var special-syms)
                                 ;; Shadow any symbol-macro with this name
                                 (setf *symbol-macros*
                                       (remove var *symbol-macros*
                                               :key #'car :test #'eq))
                                 (setf bind-instrs
                                       (append bind-instrs
                                               `((:declare-local ,tmp "LispObject")
                                                 ;; Init forms bind a single value
                                                 ,@(let ((*in-tail-position* nil)
                                                         (*in-mv-context* nil))
                                                     (if init-form
                                                         (compile-expr init-form)
                                                         (emit-nil)))
                                                 (:stloc ,tmp)
                                                 ,@(compile-sym-lookup var)
                                                 (:castclass "Symbol")
                                                 (:ldloc ,tmp)
                                                 (:call "DynamicBindings.Push")))))
                               ;; Lexical: compile init in current scope (not tail pos), then extend
                               (let* ((long-rep-p (member (var-name var) long-rep-names
                                                          :test #'string=))
                                      (nfk (cond ((member (var-name var) double-rep-names :test #'string=) :double)
                                                 ((member (var-name var) single-rep-names :test #'string=) :single)
                                                 (t nil)))
                                      (dec-rep-p (member (var-name var) decimal-rep-names
                                                         :test #'string=))
                                      (init-code (cond
                                                   (long-rep-p (compile-expr-to-long init-form))
                                                   (nfk (compile-float-native-value init-form nfk))
                                                   (dec-rep-p (compile-decimal-native-value init-form))
                                                   (t (let ((*in-tail-position* nil)
                                                            (*in-mv-context* nil))
                                                        (if init-form
                                                            (compile-expr init-form)
                                                            (emit-nil)))))))
                                 ;; Extend scope for subsequent bindings and body
                                 (setf *cstate*
                                       (cstate-with *cstate* +cs-locals+
                                                    (acons var key (cstate-locals))))
                                 ;; Native-representation slots for subsequent
                                 ;; inits/body, recorded by slot key. A non-native
                                 ;; rebinding needs no removal: it owns a
                                 ;; different key, and references to it resolve
                                 ;; there (see NATIVE-SLOT-P).
                                 (when long-rep-p
                                   (setf *cstate*
                                         (cstate-with *cstate* +cs-long-locals+
                                                      (cons key (cstate-long-locals)))))
                                 (when (eq nfk :double)
                                   (setf *cstate*
                                         (cstate-with *cstate* +cs-native-double-locals+
                                                      (cons key (cstate-native-double-locals)))))
                                 (when (eq nfk :single)
                                   (setf *cstate*
                                         (cstate-with *cstate* +cs-native-single-locals+
                                                      (cons key (cstate-native-single-locals)))))
                                 (when dec-rep-p
                                   (setf *cstate*
                                         (cstate-with *cstate* +cs-native-decimal-locals+
                                                      (cons key (cstate-native-decimal-locals)))))
                                 ;; Shadow any symbol-macro with this name
                                 (setf *symbol-macros*
                                       (remove var *symbol-macros*
                                               :key #'car :test #'eq))
                                 (if (member (var-name var) needs-boxing :test #'string=)
                                     (setf bind-instrs
                                           (append bind-instrs
                                                   (emit-box-create key init-code var)))
                                     (progn
                                       ;; Remove from boxed-vars to shadow outer boxed binding (e.g. labels)
                                       (when (boxed-var-p var)
                                         (setf *cstate*
                                               (cstate-with *cstate* +cs-boxed-vars+
                                                            (remove (var-name var) (cstate-boxed-vars)
                                                                    :key (lambda (x) (if (symbolp x) (var-name x) x))
                                                                    :test #'string=))))
                                       (setf bind-instrs
                                             (append bind-instrs
                                                     `((:declare-local ,key
                                                        ,(cond (long-rep-p "Int64")
                                                               ((eq nfk :double) "Double")
                                                               ((eq nfk :single) "Single")
                                                               (dec-rep-p "Decimal")
                                                               (t "LispObject")))
                                                       ,@init-code
                                                       (:stloc ,key)
                                                       ,@(%local-var-marker
                                                          key var
                                                          (cond (long-rep-p :long)
                                                                (dec-rep-p :decimal)
                                                                (t nfk))))))))))))
              ;; Remove declared-specials from *locals* so body references use dynamic binding
              (let* ((let-bound-names (mapcar (lambda (b) (var-name (first b)))
                                             binding-info))
                     ;; Body inherits outer *in-tail-position*; TCO branches
                     ;; are suppressed via *in-try-block* when special-syms
                     ;; introduce a try/finally (see parallel-let branch).
                     (body-instrs (compile-let-body-instrs
                                    real-body body binding-info let-bound-names
                                    needs-boxing mutated special-syms declared-specials)))
                (compile-let-with-specials
                 '() bind-instrs body-instrs
                 (reverse special-syms)))))))))

(defun rekey-by-binding (name-alist binding-info)
  "Re-key a (NAME . INFO) alist to (SLOT-KEY . INFO) using BINDING-INFO. Entries
   naming nothing this form binds are dropped — the declaration scanners run over
   a body's own head declarations, which name that body's own variables."
  (loop for e in name-alist
        for b = (find (car e) binding-info
                      :key (lambda (b) (var-name (first b))) :test #'string=)
        when b collect (cons (fourth b) (cdr e))))

(defun rekey-by-param (name-alist params local-keys)
  "REKEY-BY-BINDING for a function body, where the slots belong to PARAMS and
   LOCAL-KEYS maps each param symbol to its slot key."
  (loop for e in name-alist
        for p = (find (car e) params :key #'var-name :test #'string=)
        when p collect (cons (cdr (assoc p local-keys)) (cdr e))))

(defun binding-keys-named (binding-info names)
  "The slot keys of the BINDING-INFO entries whose variable name is in NAMES.
   Bridges the name-keyed representation analyses (long-rep-names etc., which are
   computed from declarations before slots exist) to the slot-keyed native
   tables."
  (loop for b in binding-info
        when (member (var-name (first b)) names :test #'string=)
          collect (fourth b)))

(defun compile-let-with-specials (init-instrs bind-instrs body-instrs special-syms)
  "Wrap body with try/finally for special variable cleanup if needed."
  (if (null special-syms)
      (eliminate-single-ref-locals `(,@init-instrs ,@bind-instrs ,@body-instrs))
      (let ((result-key (gen-local "RESULT")))
        `(,@init-instrs
          ,@bind-instrs
          (:declare-local ,result-key "LispObject")
          (:begin-exception-block)
          ,@body-instrs
          (:stloc ,result-key)
          (:begin-finally-block)
          ;; Pop in REVERSE of push order (LIFO). All call sites pass SPECIAL-SYMS
          ;; in push (source) order, so the naive in-order loop emitted FIFO Pops
          ;; for a multi-special frame. The current by-symbol DynamicBindings.Pop
          ;; tolerates any order, so this is behavior-neutral today — but it lets a
          ;; shallow-binding Pop take its O(1) top-of-stack fast path instead of the
          ;; out-of-order search fallback.
          ,@(loop for sym in (reverse special-syms)
                  append `(,@(compile-sym-lookup sym)
                           (:castclass "Symbol") (:call "DynamicBindings.Pop")))
          (:end-exception-block)
          (:ldloc ,result-key)))))

(defun compile-let-star (bindings body)
  "Compile let* by delegating to compile-let with sequential flag."
  (compile-let bindings body t))

;;; ============================================================
;;; setq
;;; ============================================================

(defun single-value-form-p (expr)
  "Return T if expr is guaranteed to produce a single value (no MV).
   Used to skip MultipleValues.Primary in setq."
  (cond
    ((null expr) t)
    ((eq expr t) t)
    ((numberp expr) t)
    ((characterp expr) t)
    ((stringp expr) t)
    ((keywordp expr) t)
    ;; Variable reference (any symbol evaluates to a single value)
    ((symbolp expr) t)
    ;; (quote ...) or (the type expr)
    ((and (consp expr) (eq (car expr) 'quote)) t)
    ((and (consp expr) (eq (car expr) 'the))
     (single-value-form-p (caddr expr)))
    ;; Non-local exits (return-from, throw, go) transfer control and
    ;; leave the stack unchanged from the compiler's perspective — adding
    ;; an UnwrapMv after would try to pop a non-existent value.
    ((and (consp expr)
          (member (car expr) '(return-from return throw go)))
     t)
    ;; Known single-value operators (direct calls, never produce MV)
    ((and (consp expr) (symbolp (car expr))
          (not (local-function-entry (car expr)))
          (member (car expr)
                  '(car cdr cons list list* first rest second third
                    cadr cddr caar cdar caddr
                    + - * / 1+ 1- mod rem
                    < > <= >= = /=
                    eq eql equal
                    not null atom consp listp numberp integerp symbolp
                    stringp characterp functionp
                    length last nconc nreverse reverse
                    rplaca rplacd nth nthcdr
                    char-code code-char char
                    ash logand logior logxor lognot
                    typep type-of
                    get
                    aref vector-push-extend
                    setq)))
     t)
    ;; Functions with a declaimed ftype return type are single-value: atomic
    ;; types like fixnum / double-float / etc. cannot appear as multiple values.
    ;; Honors user `(declaim (ftype (function (...) fixnum) NAME))` so calls to
    ;; such NAME elide Runtime.UnwrapMv.
    ((and (consp expr) (symbolp (car expr))
          (not (local-function-entry (car expr)))
          (let ((ret (gethash (car expr) *function-return-types*)))
            (and ret
                 (member ret '(fixnum bit double-float single-float
                                      short-float long-float
                                      integer float real rational number
                                      character boolean)))))
     t)
    (t nil)))

(defun compile-setq (var val-expr)
  "Compile (setq var value). Returns the assigned value on stack (single value)."
  ;; If var is a symbol-macro, delegate to SETF of the expansion (branch on found-p,
  ;; not the expansion's truth value, so a NIL-expanding symbol-macro still delegates )
  (multiple-value-bind (sm-exp found) (lookup-symbol-macro var)
    (when found
      (return-from compile-setq (compile-expr `(setf ,sm-exp ,val-expr)))))
  ;; Int64-slot local (long-rep let binding or native body param): store the
  ;; raw long, then box once for the expression value. In statement position
  ;; the peephole pass deletes the box+pop, leaving a pure native store.
  ;; Checked before the generic path because the slot type differs.
  (let ((key (and (boundp '*cstate*)
                  (native-slot-p var (cstate-long-locals)))))
    (when key
      (return-from compile-setq
        `(,@(compile-expr-to-long val-expr)
          (:dup)
          (:stloc ,key)
          ,@(%frame-set-instrs key var :long)
          (:call "Fixnum.Make")))))
  ;; Native float slot (double/single-rep local): store the raw r8/r4, then box
  ;; once for the expression value. The peephole (P6/P7) deletes the box+pop in
  ;; statement position, leaving a pure native store — this is what removes the
  ;; per-setq DoubleFloat/SingleFloat box from numeric accumulator loops.
  (let ((key (lookup-local var))
        (nk (float-native-local-kind var)))
    (when (and key nk)
      (return-from compile-setq
        `(,@(compile-float-native-value val-expr nk)
          (:dup)
          (:stloc ,key)
          ,@(%frame-set-instrs key var nk)
          (:newobj ,(ecase nk (:double "DoubleFloat") (:single "SingleFloat")))))))
  ;; Native decimal slot: same shape as the float slots — store raw, box once
  ;; for the expression value (the peephole drops the box in statement position).
  (let ((key (lookup-local var)))
    (when (and key (decimal-native-local-p var))
      (return-from compile-setq
        `(,@(compile-decimal-native-value val-expr)
          (:dup)
          (:stloc ,key)
          ,@(%frame-set-instrs key var :decimal)
          (:newobj "LispDecimal")))))
  ;; Variable assignment binds a single value. Force *in-mv-context* nil
  ;; and *in-tail-position* nil when compiling val-expr so MvReturn is
  ;; unwrapped before storage (mirrors compile-let).
  (let ((val-instrs (let ((*in-tail-position* nil)
                          (*in-mv-context* nil))
                      (compile-expr val-expr))))
    ;; Check lexical binding BEFORE special — mirrors compile-var-ref ordering
    (let ((key (lookup-local var)))
      (if key
          (if (boxed-var-p var)
              ;; Boxed: box[0] = value, return value
              (let ((tmp (gen-local "SETQ")))
                `((:declare-local ,tmp "LispObject")
                  ,@val-instrs
                  (:stloc ,tmp)
                  ,@(if (and *emit-source-lines* (not (labels-cell-var-p var)))
                        `((:ldloc ,key) (:ldloc ,tmp) (:stfld "LispBox.Value"))
                        `((:ldloc ,key) (:ldc-i4 0) (:ldloc ,tmp) (:stelem-ref)))
                  (:ldloc ,tmp)))
              ;; Simple local: store and return value
              `(,@val-instrs
                (:dup)
                (:stloc ,key)
                ,@(%frame-set-instrs key var)))
          ;; No lexical binding — use special/dynamic assignment.
          ;; SetChecked, not Set: assigning a constant variable is an error
          ;; (CLHS 3.1.2.1.1.3), and DEFCONSTANT is the only thing that may write
          ;; one. The check is at run time because the symbol may not be a
          ;; constant yet when this form is compiled.
          (let ((tmp (gen-local "SETQSPL")))
            `((:declare-local ,tmp "LispObject")
              ,@val-instrs
              (:stloc ,tmp)
              ,@(compile-sym-lookup var)
              (:castclass "Symbol")
              (:ldloc ,tmp)
              (:call "DynamicBindings.SetChecked")))))))

;;; ============================================================
;;; lambda / closure
;;; ============================================================

(defun find-free-vars-with-defaults (params body)
  "The free variables of BODY and of the &optional/&key default forms, as the
   SYMBOLS that were referenced — not their names. FREE-HT is keyed by VAR-NAME
   (so same-named references collapse to one capture) but holds the symbol, and
   returning that keeps capture on symbol identity all the way to the closure
   body's *LOCALS*. Names are re-derived (VAR-NAME) only where a string is
   actually required: the emitted :env-map, and the string-keyed mutation/boxing
   analyses."
  (multiple-value-bind (required optional key rest-param aux) (parse-lambda-list params)
    (let* ((all-params (append required
                               (mapcar #'car optional)
                               (remove nil (mapcar #'third optional))
                               (mapcar #'second key)
                               (remove nil (mapcar #'fourth key))
                               ;; The &rest var is bound by the lambda list like
                               ;; every other parameter. Leaving it out made a body
                               ;; reference to it look free, which pushed DEFUN off
                               ;; the :defmethod path onto runtime registration and
                               ;; made a bare (lambda (&rest r) r) build an env array
                               ;; capturing an outer R that the parameter shadows.
                               (if rest-param (list rest-param) nil)
                               ;; &aux vars are bound in the function body scope,
                               ;; so body references to them are not free (D-fix).
                               (mapcar (lambda (a) (if (consp a) (car a) a)) aux)))
           (bound-names (mapcar #'var-name all-params))
           (free-ht (make-hash-table :test #'eq)))
      ;; Scan body
      (dolist (form body)
        (find-free-vars-expr form bound-names free-ht))
      ;; Scan default forms in &optional/&key — pass nil for bound
      ;; since scan-lambda-list-defaults handles progressive scoping
      ;; (each default only sees params to its left, not all params)
      (scan-lambda-list-defaults params nil free-ht)
      (let ((syms '()))
        (maphash (lambda (k v) (declare (ignore k)) (push v syms)) free-ht)
        syms))))

(defun lambda-list-shape-tag (params)
  "Digit-free lambda-list shape tag for the make-function DM name (diagnostic
   only — InvokeSlow statistics can then break \"<anon:lambda>\" down by
   shape). Counts encode as repeated letters, capped at 9, because the
   statistics origin tag strips digits: \"oo\" = 2 &optional, \"kkk\" = 3
   &key, \"r\" = &rest, \"a\" = &aux; a bare &key with no key params = \"k\"."
  (multiple-value-bind (required optional key rest-param aux allow-other-keys-p has-key-p)
      (parse-lambda-list params)
    (declare (ignore required allow-other-keys-p))
    (concatenate 'string
                 (make-string (min (length optional) 9) :initial-element #\o)
                 (if (and has-key-p (null key))
                     "k"
                     (make-string (min (length key) 9) :initial-element #\k))
                 (if rest-param "r" "")
                 (if aux "a" ""))))

(defun compile-lambda (params body &optional (fn-name "") (fn-label ""))
  "Compile (lambda (params) body...). FN-NAME enables self-TCO when non-empty.
   FN-LABEL, when non-empty, is wired to the :name plist of
   :make-function/-direct so the runtime LispFunction carries the name
   (backtrace / statistics / print) WITHOUT enabling TCO — used by the
   runtime-registration defun paths, whose defuns were previously anonymous
   (e.g. &rest stdlib defuns in chunked progn compiles)."
  (multiple-value-bind (required optional key rest-param) (parse-lambda-list params)
    (declare (ignore optional key))
    (let* ((free-vars (remove-if #'global-special-p
                                 (find-free-vars-with-defaults params body)))
           ;; Speculative labels-self-TCO: when this named, required-only fn's
           ;; ONLY free var is its own labels box, compile it through the
           ;; direct+TCO path instead of a closure. compile-labels-boxed scans the
           ;; result and falls back to the closure path unless it is provably
           ;; self-contained (no box-key reference, no orphan post-TCO code), so a
           ;; wrong guess only forgoes the optimization — it never miscompiles.
           (spec (cstate-labels-direct-speculation))
           (speculate-direct
             (and spec
                  (string= fn-name (car spec))
                  (simple-required-only-p params)
                  free-vars (null (cdr free-vars))
                  (string= (var-name (first free-vars))
                           (concatenate 'string "__LABELFN_" (car spec))))))
      (if (or (null free-vars) speculate-direct)
          ;; No captures (or speculating self-TCO) — :make-function or :make-function-direct
          (if (simple-required-only-p params)
              `((:make-function-direct
                 :param-count ,(length required)
                 ,@(when (string/= fn-label "") `(:name ,fn-label))
                 ,@(when (debug-frames-off-p body) '(:no-frame t))
                 :body ,(compile-function-body-direct
                         params (%direct-body-with-aux params body) fn-name)))
              `((:make-function
                 :param-count ,(length required)
                 :ll-shape ,(lambda-list-shape-tag params)
                 ,@(when (string/= fn-label "") `(:name ,fn-label))
                 ,@(when (debug-frames-off-p body) '(:no-frame t))
                 ;; FN-LABEL, not FN-NAME: this is the name the arity error and
                 ;; the debug frame report, and the runtime-registration DEFUN
                 ;; paths carry the name there (FN-NAME stays "" so self-TCO,
                 ;; which only the direct path implements, is not enabled).
                 :body ,(compile-function-body params body
                                               (if (string/= fn-label "")
                                                   fn-label
                                                   fn-name)))))
          ;; Closure: build env array, then :make-closure
          (compile-closure params body free-vars)))))

(defun compile-closure (params body free-vars)
  "Compile a closure lambda with captured variables."
  (multiple-value-bind (required optional key rest-param) (parse-lambda-list params)
    (declare (ignore optional key))
  (let* ((n-free (length free-vars))
         ;; Direct-params eligibility: required-only lambda list (no &optional/
         ;; &key/&rest/&aux — simple-required-only-p) with <= 6 params. Such a
         ;; body reads params exclusively via :load-arg, so the assembler can
         ;; emit it with real per-arity CLR params (:load-arg i -> ldarg i+1).
         ;; Its arity-check prefix is omitted (see compile-closure-body): the
         ;; runtime wrapper performs the identical CheckArityExact for
         ;; args-array callers, and the delegate signature enforces argc
         ;; structurally on the direct path.
         (direct-p (and (simple-required-only-p params)
                        (<= (length required) 6)))
         ;; Build env array on stack
         (env-build-instrs
           `((:ldc-i4 ,n-free)
             (:newarr "Object")
             ,@(loop for fv in free-vars
                     for i from 0
                     append (compile-env-capture fv i))))
         ;; Determine which free vars are boxed in outer scope
         (outer-boxed-fvs
           (remove-if-not (lambda (fv)
                            (let ((entry (local-entry fv)))
                              (if entry
                                  (boxed-var-p (car entry))
                                  ;; Only check *local-functions* when not in *locals*
                                  (let ((lf (local-function-entry (var-name fv))))
                                    (and lf (third lf))))))
                          free-vars))
         ;; Under debug, the subset of boxed captures that are DATA variables
         ;; (a boxed lexical in *locals*, not a captured labels function cell)
         ;; use the LispBox representation and so must be loaded/declared as
         ;; LispBox in the body. Labels function cells stay LispObject[1].
         (outer-lispbox-fvs
           (when *emit-source-lines*
             (remove-if-not (lambda (fv)
                              (let ((entry (local-entry fv)))
                                (and entry (boxed-var-p (car entry))
                                     (not (labels-cell-var-p (car entry))))))
                            free-vars)))
         ;; Compile inner body with env slots. The enclosing context — the
         ;; tables the body may still consult plus the two classifications
         ;; above — is snapshotted here, where we are still outside the
         ;; closure-boundary reset.
         (inner-body (compile-closure-body params body free-vars
                                           (capture-compile-env outer-boxed-fvs
                                                                outer-lispbox-fvs)
                                           "" direct-p)))
    `(,@env-build-instrs
      (:make-closure
       :param-count ,(length required)
       ;; fn-name "" matches the name the omitted arity check would have
       ;; carried (compile-closure never passes a name to compile-closure-body)
       ,@(when direct-p '(:direct t :fn-name ""))
       :env-size ,n-free
       ;; The map crosses into the assembler, so slots are named by string here.
       :env-map ,(loop for fv in free-vars
                       for name = (var-name fv)
                       for i from 0
                       collect (let ((entry (local-entry fv)))
                                 (if entry
                                     (if (boxed-var-p (car entry))
                                         ;; Data var: LispBox under debug (so the
                                         ;; body loads env[i] as LispBox), else
                                         ;; LispObject[1]. Labels function cells
                                         ;; stay LispObject[1] even under debug.
                                         (list name i
                                               (if (and *emit-source-lines*
                                                        (not (labels-cell-var-p (car entry))))
                                                   "lispbox" "boxed"))
                                         (list name i "value"))
                                     ;; Only check *local-functions* when not in *locals*
                                     (let ((lf (local-function-entry name)))
                                       (if (and lf (third lf))
                                           ;; Labels function cell: always LispObject[1].
                                           (list name i "boxed")
                                           (list name i "value"))))))
       :body ,inner-body)))))

(defun compile-env-capture (fv idx)
  "Generate instructions to capture free variable FV (the referenced symbol)
   into env[idx]."
  (let ((entry (local-entry fv)))
    (if entry
        (let ((key (cdr entry))
              (is-boxed (boxed-var-p (car entry))))
          `((:dup) (:ldc-i4 ,idx)
            ,@(if is-boxed
                  ;; Capture the box (LispObject[]) itself
                  `((:ldloc ,key))
                  ;; Capture the value
                  `((:ldloc ,key)))
            (:stelem-ref)))
        ;; Not in *locals* — check *local-functions* for boxed labels functions
        (let ((lf-entry (local-function-entry (var-name fv))))
          (if (and lf-entry (third lf-entry))  ;; boxed-p
              `((:dup) (:ldc-i4 ,idx)
                (:ldloc ,(second lf-entry))  ;; the box key
                (:stelem-ref))
              ;; Not found — nil
              `((:dup) (:ldc-i4 ,idx) (:ldsfld "Nil.Instance") (:stelem-ref)))))))

;;; ------------------------------------------------------------
;;; compile-env — the enclosing compile context at a boundary
;;; ------------------------------------------------------------
;;; The per-compilation state variables describe the body being compiled. A
;;; closure body gets a fresh set of them (the closure-boundary reset, see
;;; DEFINE-COMPILE-STATE), but a few pieces of the ENCLOSING body stay relevant
;;; inside it: a captured flet/labels function, a return-from naming an outer
;;; block, a non-local go. Those were saved one variable at a time just before
;;; the reset. COMPILE-ENV names the concept and puts the capture in one place,
;;; so cross-boundary context is added here instead of as another ad-hoc save at
;;; the boundary.
;;;
;;; It is deliberately NOT the whole registry. Reset is an obligation — missing
;;; one variable there is a silent miscompile, which is why that side is
;;; registry-driven. Capture is a permission: state absent here simply cannot be
;;; consulted from inside, and that fails loudly at the call site.
;;;
;;; It lives in this file rather than beside the state variables because the
;;; cross-compile compiles cil-compiler.lisp first, before DEFSTRUCT's support
;;; functions exist.

(defstruct (compile-env
             (:constructor %make-compile-env (local-functions block-tags go-tags
                                              boxed-fvs lispbox-fvs))
             (:copier nil)
             (:predicate nil))
  local-functions       ; as *local-functions*, from the enclosing body
  block-tags            ; as *block-tags*
  go-tags               ; as *go-tags*
  boxed-fvs             ; captured free vars that are boxed in the enclosing body
  lispbox-fvs)          ; under debug, the subset of those held in a LispBox
;; NB: COMPILE-ENV-CAPTURE (below) is NOT an accessor of this struct — it emits
;; the closure env-array capture for one free var. Do not name a slot CAPTURE.

(defun capture-compile-env (boxed-fvs lispbox-fvs)
  "Snapshot the enclosing compile context — the state an inner body may still
   consult after the closure-boundary reset. Call it BEFORE the reset.
   BOXED-FVS and LISPBOX-FVS are the caller's classification of the captured
   free variables; they are facts about the ENCLOSING bindings, so they belong
   to the same snapshot as the tables above."
  (%make-compile-env (cstate-local-functions) (cstate-block-tags) (cstate-go-tags)
                     boxed-fvs lispbox-fvs))

(defun compile-closure-body (params body free-vars outer-env &optional (fn-name "")
                                                                      direct-p)
  "Compile function body for closure. Free vars access env (arg 0), params from args (arg 1).
   FREE-VARS are the captured SYMBOLS (see FIND-FREE-VARS-WITH-DEFAULTS); each one
   keys its env slot in *LOCALS*, so body references resolve by identity.
   OUTER-ENV is the enclosing context (CAPTURE-COMPILE-ENV), taken by the caller
   before this body's closure-boundary reset: the tables a captured
   flet/labels / return-from / go still resolve against, and which captured free
   variables were boxed (and under debug, held in a LispBox) out there.
   Handles &rest/&optional/&key parameters.
   DIRECT-P: the caller will mark this closure :direct (required-only, <= 6
   params, assembled with per-arity CLR params) — omit the arity-check prefix;
   the runtime-side args-array wrapper performs the identical CheckArityExact."
  (multiple-value-bind (required optional key rest-param aux allow-other-keys-p has-key-p) (parse-lambda-list params)
    (let ((body (wrap-aux-body aux body)))
    (let* ((all-params (append required
                               (mapcar #'car optional)
                               (remove nil (mapcar #'third optional))
                               (mapcar #'second key)
                               (remove nil (mapcar #'fourth key))
                               (if rest-param (list rest-param) nil))))
      ;; Closure-boundary reset: rebind every registered per-compilation state
      ;; variable (define-compile-state, cil-compiler.lisp) to its fresh value
      ;; so inner closures don't inherit the enclosing body's compile context
      ;; (TCO state / *self-fn-local* especially — it refers to a local
      ;; declared in the OUTER method). Overrides carry the entries whose
      ;; fresh value is not a registry constant: *notinline-functions* is
      ;; computed from this body (CLHS 3.2.2.1.1); *in-tail-position* T
      ;; (closure body's last form is in tail position: MV propagation)
      ;; duplicates the registered fresh-init for explicitness. Everything
      ;; that was previously bound AFTER the reset bindings in the old let*
      ;; is evaluated inside the thunk — the original evaluation order and
      ;; environment are preserved exactly.
      (call-with-fresh-closure-state
       (list (cons '*notinline-functions* (extract-notinline body))
             (cons '*in-tail-position* t))
       (lambda ()
    (let* ((n-required (length required))
           (key-start (+ n-required (length optional)))
           ;; Compute boxing needs for params within this closure (single walk)
           (all-var-names (append (mapcar #'var-name all-params)
                                  (mapcar #'var-name free-vars)))
           (mc (multiple-value-list
                (find-mutated-and-captured-vars body all-var-names)))
           (mutated (first mc))
           (captured-inner (second mc))
           (needs-boxing (%strings-in-both mutated captured-inner))
           ;; Env slot locals
           (env-instrs '())
           (env-locals '()))
      ;; Set up free var locals from env
      (loop for fv in free-vars
            for i from 0
            do (let* ((key (gen-local (var-name fv)))
                      ;; FV itself keys the env local — the closure body's
                      ;; references resolve to the very symbol the enclosing
                      ;; scope bound, by identity. (Interning the name into
                      ;; DOTCL.CIL-COMPILER here used to break that, leaving the
                      ;; name fallback to reconnect them.)
                      (fv-sym fv)
                      (is-lispbox (member fv (compile-env-lispbox-fvs outer-env)))
                      (is-outer-boxed (member fv (compile-env-boxed-fvs outer-env)))
                      ;; Name the captured slot in the closure body's Locals, but
                      ;; only for real user variables (not labels cells / block /
                      ;; tagbody machinery) — so a variable closed over shows by
                      ;; name when stopped inside the lambda. The box branches pass
                      ;; :BOX: the slot holds the cell, not the value, so it takes
                      ;; the PDB marker but no frame-locals store.
                      (user-var-p (not (%synthetic-capture-name-p (var-name fv))))
                      (marker (when user-var-p
                                (%local-var-marker key fv-sym)))
                      (box-marker (when user-var-p
                                    (%local-var-marker key fv-sym :box))))
                 (push (cons fv-sym key) env-locals)
                 (setf env-instrs
                       (append env-instrs
                               (cond
                                 ;; Debug data-var box: LispBox cell from env.
                                 (is-lispbox
                                  `((:declare-local ,key "LispBox")
                                    (:load-env ,i)
                                    (:stloc ,key)
                                    ,@box-marker))
                                 ;; LispObject[1] box (labels cell, or non-debug).
                                 (is-outer-boxed
                                  `((:declare-local ,key "LispObject[]")
                                    (:load-env ,i)
                                    (:stloc ,key)
                                    ,@box-marker))
                                 (t
                                  `((:declare-local ,key "LispObject")
                                    (:load-env ,i)
                                    (:stloc ,key)
                                    ,@marker)))))))
      ;; Set up params
      (let* ((param-locals
               (loop for p in all-params
                     collect (let ((key (gen-local (var-name p))))
                               (cons p key))))
             ;; Shadow symbol-macros whose names match captured env variables or params.
             ;; When a variable like X is both a symbol-macro (x → (svref #:inst 0)) AND
             ;; captured in the env, accessing X inside the closure must use the local
             ;; variable, not the expansion. Without this shadow, the expansion would be
             ;; compiled but its free variables (e.g. #:inst) wouldn't be in *locals*.
             (*symbol-macros*
              (let ((local-syms (append all-params free-vars)))
                (remove-if (lambda (entry)
                             (let ((k (car entry)))
                               (some (lambda (l) (shadows-var-p l k)) local-syms)))
                           *symbol-macros*)))
             ;; One pack update: params+env into locals; boxed-vars maps
             ;; NEEDS-BOXING (a name list — the mutation/capture analysis is
             ;; string-keyed) back to the symbol that owns each name — a param or
             ;; a captured free var — interning only as a last resort, plus the
             ;; already-symbol OUTER-BOXED-FVS; and the captured flet/labels
             ;; functions, block tags, and non-local go tags are re-established
             ;; from the OUTER-ENV snapshot against the env slot locals.
             (*cstate*
              (cstate-with *cstate*
                +cs-locals+ (append param-locals env-locals)
                +cs-boxed-vars+
                (append
                 (mapcar (lambda (name)
                           (or (find name all-params :key #'var-name :test #'string=)
                               (find name free-vars :key #'var-name :test #'string=)
                               (intern name :dotcl.cil-compiler)))
                         needs-boxing)
                 (or (compile-env-boxed-fvs outer-env) '()))
                +cs-local-functions+
                (loop for outer-fn in (compile-env-local-functions outer-env)
                      for fn-name = (first outer-fn)
                      for fn-key = (second outer-fn)
                      for fn-boxed-p = (third outer-fn)
                      for mangled = (concatenate 'string "__LABELFN_" fn-name)
                      for captured = (or (find mangled free-vars :key #'var-name :test #'string=)
                                         (find fn-name free-vars :key #'var-name :test #'string=))
                      when captured
                      collect (let ((env-entry (assoc captured env-locals :test #'eq)))
                                ;; A capture-lifted function cannot be reached from
                                ;; here: its extra parameters name slots of the
                                ;; ENCLOSING frame, which this closure has not
                                ;; captured, so a call from inside would pass the
                                ;; wrong count. Unwind and recompile the FLET the
                                ;; ordinary way.
                                (when (fourth outer-fn)
                                  (throw (%lift-capture-tag outer-fn) :lift-aborted))
                                (list fn-name (cdr env-entry) fn-boxed-p)))
                +cs-block-tags+
                (loop for (bname . binfo) in (compile-env-block-tags outer-env)
                      for tag-var = (block-tag-var-name bname)
                      for env-entry = (local-entry-by-name tag-var env-locals)
                      when env-entry
                      collect (cons bname (list (cdr env-entry) nil nil nil nil (sixth binfo))))
                +cs-go-tags+
                ;; Format: (tag-name tb-var-name tb-id-key label-idx); after
                ;; capture tb-id-key becomes the env local key.
                (loop for gt-entry in (compile-env-go-tags outer-env)
                      for tag-name = (first gt-entry)
                      for tb-var-name = (second gt-entry)
                      for label-idx = (fourth gt-entry)
                      for env-entry = (local-entry-by-name tb-var-name env-locals)
                      when env-entry
                      ;; 5th/6th nil (closure go → throw path); 7th/8th preserve
                      ;; the outer tagbody's needs-catch / has-go cells so the
                      ;; throw flags them (has-go: a go that exists only inside
                      ;; the closure still makes the outer tagbody a loop).
                      collect (list tag-name tb-var-name (cdr env-entry) label-idx
                                    nil nil (seventh gt-entry) (eighth gt-entry)))))
             (param-instrs
               ;; Shared args-array machinery (compile-args-param-instrs):
               ;; args elements load via :load-arg (mapped per closure mode by
               ;; the assembler), the args array itself is ldarg 1, and
               ;; defaults mask current+later params while keeping env
               ;; captures visible (so defaults see the outer scope).
               (compile-args-param-instrs
                required optional key rest-param param-locals n-required key-start
                (lambda (i) `((:load-arg ,i)))
                '((:ldarg 1))
                (lambda (names)
                  (append (remove-if (lambda (entry)
                                       (member (var-name (car entry)) names
                                               :test #'string=))
                                     param-locals)
                          env-locals)))))
        (let* ((arity-instrs
                 (if direct-p
                     ;; :direct closure: arity check moves to the runtime-side
                     ;; args-array wrapper (MakeDirectClosure); the per-arity
                     ;; delegate signature enforces argc on the direct path.
                     ;; Structural guard: :direct must imply the required-only
                     ;; shape — a mismatch means the ignition predicate in
                     ;; compile-closure diverged from this one.
                     (progn
                       (unless (and (null optional) (null key) (null rest-param)
                                    (not has-key-p))
                         (error ":direct closure with non-required-only lambda list: ~s"
                                params))
                       '())
                     (compile-args-arity-instrs fn-name optional key rest-param
                                                has-key-p n-required '((:ldarg 1)))))
               (key-check-instrs
                 ;; Shared with compile-function-body-inner (semantics unified
                 ;; in the fix): bare &key rejects unknown keywords,
                 ;; explicit-package keys use CheckNoUnknownKeys2.
                 (compile-args-key-check-instrs fn-name key has-key-p allow-other-keys-p
                                                key-start '((:ldarg 1)))))
          ;; Handle special params in closure body (declare + globally special)
          (let* ((special-param-syms
                   (%union-eq (fn-body-special-params body all-params)
                          (remove-if-not #'global-special-p all-params)))
                 (special-push-instrs
                   (loop for p in special-param-syms
                         for pkey = (cdr (assoc p param-locals))
                         append `(,@(compile-sym-lookup p)
                                  (:castclass "Symbol")
                                  (:ldloc ,pkey)
                                  (:call "DynamicBindings.Push"))))
                 ;; Also find free vars (non-params) declared special in body.
                 ;; These must be removed from *locals* so body references use
                 ;; DynamicBindings.Get instead of the closure-captured lexical value.
                 ;; Unwrap optional (block name ...) wrapper to find leading declares.
                 ;; Do NOT unwrap let*: a special declared inside an &aux-introduced
                 ;; (let* aux ...) only governs the let* BODY, not its init forms, and
                 ;; compile-let* already handles that scoping. Removing it here
                 ;; would wrongly make the aux init forms read the dynamic value.
                 (body-inner-forms
                   (if (and (= (length body) 1)
                            (consp (car body))
                            (eq (caar body) 'block))
                       (cddar body)
                       body))
                 (body-declared-specials
                   (multiple-value-bind (specials _rest) (extract-specials body-inner-forms)
                     (declare (ignore _rest))
                     specials))
                 (free-special-syms
                   (remove-if (lambda (s)
                                (some (lambda (p) (shadows-var-p p s)) all-params))
                              body-declared-specials))
                 (*cstate* (cstate-with *cstate* +cs-locals+
                                        (remove-locals-shadowed-by
                                         (append special-param-syms free-special-syms)
                                         (cstate-locals))))
                 (body-instrs (compile-progn body)))
            (merge-disjoint-locals
             (if special-param-syms
                 `(,@arity-instrs
                   ,@key-check-instrs
                   ,@(%frame-enter-instrs fn-name)
                   ,@env-instrs ,@param-instrs
                   ,@(compile-let-with-specials '() special-push-instrs body-instrs special-param-syms)
                   (:ret))
                 `(,@arity-instrs
                   ,@key-check-instrs
                   ,@(%frame-enter-instrs fn-name)
                   ,@env-instrs ,@param-instrs
                   ,@body-instrs
                   (:ret))))))))))))))



;;; ============================================================
;;; funcall
;;; ============================================================

(defun compile-funcall (args)
  "Compile (funcall fn arg1 arg2 ...).
   Per CL spec, if fn is a symbol, calls symbol-function on it first."
  (when (null args)
    (return-from compile-funcall
      (compile-static-program-error "FUNCALL: too few arguments (expected function designator)")))
  (let ((fn-expr (car args)))
    (if (and (consp fn-expr)
             (or (eq (car fn-expr) 'quote) (eq (car fn-expr) 'function))
             (symbolp (cadr fn-expr))
             ;; Only optimize if sym is NOT shadowed by a local flet/labels.
             (not (local-function-entry (cadr fn-expr))))
        ;; (funcall 'sym ...) or (funcall #'sym ...) → compile as named call
        (compile-named-call (cadr fn-expr) (cdr args))
        ;; General case: evaluate fn, coerce symbol to function if needed
        (let ((call-args (cdr args))
              (n-call-args (length (cdr args))))
          (if (<= n-call-args 8)
              (let* ((fn-tmp (gen-local "FNTMP"))
                     (da (compile-direct-call-args call-args))
                     (temps (car da))
                     (eval-instrs (cdr da)))
                `((:declare-local ,fn-tmp "LispFunction")
                  ,@(let ((*in-mv-context* nil) (*in-tail-position* nil))
                      (compile-expr fn-expr))
                  (:call "Runtime.CoerceToFunction")
                  (:stloc ,fn-tmp)
                  ,@eval-instrs
                  (:ldloc ,fn-tmp)
                  ,@(loop for tmp in temps append `((:ldloc ,tmp)))
                  (:callvirt ,(invoke-name n-call-args))))
              `(,@(let ((*in-mv-context* nil) (*in-tail-position* nil))
                    (compile-expr fn-expr))
                (:call "Runtime.CoerceToFunction")
                ,@(compile-args-array call-args)
                (:callvirt "LispFunction.Invoke")))))))

;;; ============================================================
;;; function special form + flet / labels
;;; ============================================================

;;; (TCO defvars moved to top of file for proper SBCL special-var recognition)

(defun compile-function-special (thing)
  "Compile (function name) or (function (lambda ...))."
  (cond
    ((and (consp thing) (eq (car thing) 'lambda))
     (compile-lambda (cadr thing) (cddr thing)))
    ;; (function (named-lambda name lambda-list . body)) — SBCL extension
    ;; Treat as plain lambda, discarding the name
    ((and (consp thing) (symbolp (car thing))
          (string= (symbol-name (car thing)) "NAMED-LAMBDA"))
     (compile-lambda (caddr thing) (cdddr thing)))
    ;; (function (setf name)) — a lexical (flet/labels) (setf name) shadows the
    ;; global one (CLHS 5.1.2.9 / the FUNCTION special form consults the lexical
    ;; environment), so check *local-functions* first, exactly like the symbol
    ;; case below. Without this, #'(setf f) always took the global SetfFunction
    ;; path and the setf-function fallback expansion — (funcall #'(setf f) …) —
    ;; failed with "Undefined function: (SETF F)" inside an flet that binds it
    ;; (eclector's set-standard-syntax-types).
    ((and (consp thing) (eq (car thing) 'setf) (symbolp (cadr thing)))
     (let ((local-fn (local-function-entry thing)))
       (if local-fn
           (let ((key (second local-fn))
                 (boxed-p (third local-fn)))
             (if boxed-p
                 `((:ldloc ,key) (:ldc-i4 0) (:ldelem-ref))
                 `((:ldloc ,key))))
           (progn
             ;; xref: #'(setf f) is an indirect-call reference, same edge as a call
             (xref-record-call thing)
             `(,@(compile-sym-lookup (cadr thing))
               (:castclass "Symbol")
               (:call "CilAssembler.GetSetfFunctionBySymbol"))))))
    ((symbolp thing)
     ;; Check local functions first
     (let ((local-fn (local-function-entry thing)))
       (if local-fn
           (let ((key (second local-fn))
                 (boxed-p (third local-fn)))
             (if boxed-p
                 `((:ldloc ,key) (:ldc-i4 0) (:ldelem-ref))
                 `((:ldloc ,key))))
           ;; Global function (built-in or user-defined) — use symbol-based lookup
           ;; so that #'sym returns the SAME stable object as (symbol-function sym),
           ;; i.e. (eq #'car #'car) and (eq #'car (symbol-function 'car)) are T.
           ;; A fresh arity-checking wrapper per #' would break eq-on-builtin code
           ;; (memoization, function tables, cl-store's fdefinition round-trip).
           ;; Package-qualified names (e.g. #'bt2:current-thread) resolve to the
           ;; correct package's symbol via compile-fn-sym-lookup. GetFunctionBySymbol
           ;; is authoritative (no cross-package fallback): unqualified #'sym resolves
           ;; via Startup.Sym's bare-name bridge at symbol-resolution time.
           (progn
             ;; xref: #'f is an indirect-call reference (funcall/mapcar target),
             ;; recorded as the same caller→callee edge as a direct call.
             (xref-record-call thing)
             `(,@(compile-fn-sym-lookup thing)
               (:castclass "Symbol")
               (:call "CilAssembler.GetFunctionBySymbol"))))))
    (t (error "FUNCTION: unsupported argument ~s" thing))))

;;;; Mini S-expression interpreter for compile-macrolet
;;;; Interprets the expander lambda without Reflection.Emit (NativeAOT/IL2CPP safe).

;; puthash is a compiler intrinsic (no CL standard equivalent). Define a Lisp
;; wrapper here (in dotcl.cil-compiler package) so %mini-eval can call it via
;; symbol-function. The body compiles to (:call "Runtime.Puthash") directly.
(defun puthash (key ht val) (puthash key ht val))

;; Same rationale as PUTHASH: these are the lowering targets SETF of SYMBOL-VALUE
;; and of GET expand into (cil-macros.lisp), emitted inline by the compiler
;; through the special-form table and therefore never given a function binding.
;; The tree-walk evaluator resolves an operator through SYMBOL-FUNCTION, so an
;; interpreted (setf (symbol-value s) v) died with "Undefined function:
;; %SET-SYMBOL-VALUE" and (setf (get s k) v) with "PUT-PROP". Each body compiles
;; to the corresponding (:call "Runtime.X"), so the compiled path is unchanged —
;; the name test in the emit table still wins there.
(defun %set-symbol-value (sym val) (%set-symbol-value sym val))
(defun put-prop (sym ind val) (put-prop sym ind val))

(defun %mini-lambda-list-shape (params)
  "Describe PARAMS for argument checking:
   (values nreq nopt restp keyp allow-other-keys-p keywords checkablep).
   CHECKABLEP is NIL when the list contains a lambda-list keyword this does not
   model — the caller then skips checking rather than risk rejecting a valid
   call."
  (let ((state :required) (nreq 0) (nopt 0) (restp nil) (keyp nil)
        (aok nil) (kws '()) (checkable t))
    (dolist (p params)
      (cond
        ((eq p '&optional) (setq state :optional))
        ;; &body is &rest with a different name (it reaches here only from a
        ;; lambda list that was not routed through DESTRUCTURING-BIND).
        ((or (eq p '&rest) (eq p '&body)) (setq state :rest restp t))
        ((eq p '&key) (setq state :key keyp t))
        ((eq p '&aux) (setq state :aux))
        ((eq p '&allow-other-keys) (setq aok t))
        ;; Anything else spelled like a lambda-list keyword (&whole, &environment,
        ;; an implementation's own) is not modelled here: stop claiming to know
        ;; the shape instead of counting it as a required parameter, which would
        ;; reject calls that are perfectly legal.
        ((and (symbolp p) p (char= (char (symbol-name p) 0) #\&))
         (setq checkable nil))
        ((eq state :required) (incf nreq))
        ((eq state :optional) (incf nopt))
        ((eq state :key)
         (let* ((head (if (consp p) (car p) p))
                (var (if (consp head) (cadr head) head))
                (kw (if (consp head) (car head)
                        (intern (symbol-name var) :keyword))))
           (push kw kws)))
        (t nil)))                       ; :rest / :aux consume no arguments
    (values nreq nopt restp keyp aok (nreverse kws) checkable)))

(defvar *mini-shape-cache*
  (make-hash-table :test 'eq :weakness :key :synchronized t)
  "PARAMS list -> #(nreq nopt restp keyp aok kws checkable).
   A lambda list does not change, but describing it returns seven values, and
   returning seven values conses a list — on every interpreted call. Keyed by
   the params list itself, which an interpreted function holds for its lifetime;
   weak so a lambda list built by EVAL and dropped does not pin the entry.

   Synchronized because every interpreted call reads and, on a miss, writes it,
   and interpreted code runs on whatever threads the program makes. Without it
   two threads interpreting at once corrupted the table — the .NET dictionary
   raises \"Operations that change non-concurrent collections must have
   exclusive access\" from inside an unrelated user function, with nothing in
   the message pointing at a cache. On an emit-free build the interpreter is the
   only evaluator, so this is every concurrent program there.")

(defun %mini-shape (params)
  "The cached shape vector for PARAMS (see *MINI-SHAPE-CACHE*)."
  (or (gethash params *mini-shape-cache*)
      (setf (gethash params *mini-shape-cache*)
            (multiple-value-bind (nreq nopt restp keyp aok kws checkable)
                (%mini-lambda-list-shape params)
              (vector nreq nopt restp keyp aok kws checkable)))))

(defun %mini-callee-name ()
  "Name of the interpreted function whose arguments are being checked, as a
   string, or \"anonymous\". The caller pushed the callee's frame before binding
   its parameters, so the innermost backtrace entry is that callee. Read only
   when an argument check has already failed, so the backtrace walk costs
   nothing on a call that is about to succeed."
  (let ((frames (ignore-errors (backtrace))))
    (if (and (consp frames) (stringp (first frames)))
        (first frames)
        "anonymous")))

(defun %mini-arg-error (control &rest arguments)
  "Signal the PROGRAM-ERROR an interpreted call's argument check raises, naming
   the callee and what was wrong. Compiled calls report through
   Runtime.CheckArityExact and read like

     FOO: wrong number of arguments: 3 (expected 1)

   while the interpreter used to signal a bare PROGRAM-ERROR that printed as
   #<PROGRAM-ERROR>. Same fault, no way to tell which call it came from — and on
   an emit-free build, where the interpreter is the only evaluator, that is every
   argument mistake in the program."
  (error 'program-error
         :format-control (concatenate 'string "~a: " control)
         :format-arguments (cons (%mini-callee-name) arguments)))

(defun %mini-check-args (params args)
  "Signal PROGRAM-ERROR if ARGS does not match the lambda list PARAMS.
   Interpreted closures previously accepted any argument count, so a missing
   required argument silently bound NIL and an unknown :KEY was ignored. That is
   the largest single source of ansi-test failures under the interpreter — every
   structure whose DEFSTRUCT was created through EVAL gets its accessors as
   interpreted closures, so (FOO-P) with no argument answered NIL instead of
   signalling (CLHS 3.5.1.2 / 3.5.1.4 / 3.5.1.6)."
  (let* ((shape (%mini-shape params))
         (nreq (svref shape 0))
         (nopt (svref shape 1))
         (restp (svref shape 2))
         (keyp (svref shape 3))
         (aok (svref shape 4))
         (kws (svref shape 5))
         (checkable (svref shape 6)))
    (when checkable
      (let ((n (length args)))
        ;; Wording follows the compiled path (Runtime.CheckArityExact / -Min /
        ;; -Max and the keyword check), so the same mistake reads the same way
        ;; whichever evaluator ran it: a required-only function reports an exact
        ;; count, anything else reports a bound.
        (let ((exactp (and (zerop nopt) (not restp) (not keyp))))
          (when (< n nreq)
            (if exactp
                (%mini-arg-error "wrong number of arguments: ~d (expected ~d)" n nreq)
                (%mini-arg-error "too few arguments: ~d (expected at least ~d)" n nreq)))
          (when (and (not restp) (not keyp) (> n (+ nreq nopt)))
            (if exactp
                (%mini-arg-error "wrong number of arguments: ~d (expected ~d)" n nreq)
                (%mini-arg-error "too many arguments: ~d (expected at most ~d)"
                                 n (+ nreq nopt)))))
        (when keyp
          (let ((tail (nthcdr (+ nreq nopt) args)))
            ;; The keyword portion must be an even-length list of keyword/value
            ;; pairs, and every keyword must be accepted unless other keys are
            ;; allowed — by &allow-other-keys or by :ALLOW-OTHER-KEYS non-NIL in
            ;; the call itself (CLHS 3.5.1.4).
            (when (oddp (length tail))
              (%mini-arg-error "odd number of keyword arguments"))
            ;; CLHS 3.4.1.4.1: when :ALLOW-OTHER-KEYS appears more than once, the
            ;; value of the FIRST pair is the one that counts. Scanning for "any
            ;; pair with a true value" made
            ;;   (:allow-other-keys nil :allow-other-keys t :foo t)
            ;; accept :FOO, where the leftmost NIL should have rejected it
            ;; (ansi-test COMPLEMENT.ERROR.6). Stop at the first pair, whatever
            ;; its value.
            (let ((caller-aok (do ((a tail (cddr a))) ((null a) nil)
                                (when (eq (car a) :allow-other-keys)
                                  (return (and (cadr a) t))))))
              (unless (or aok caller-aok)
                (do ((a tail (cddr a))) ((null a) nil)
                  (unless (or (member (car a) kws)
                              (eq (car a) :allow-other-keys))
                    (%mini-arg-error "unrecognized keyword argument ~s" (car a))))))))))))

(defun %mini-body-special-decls (body)
  "The names BODY's (DECLARE (SPECIAL ...)) forms mention. Scans every form, the
   same way %MINI-EVAL-PROGN does, so the two agree on what counts as declared."
  (let ((r '()))
    (dolist (f body r)
      (when (and (consp f) (eq (car f) 'declare))
        (dolist (d (cdr f))
          (when (and (consp d) (eq (car d) 'special))
            (dolist (v (cdr d)) (push v r))))))))

(defun %mini-bind-supp (supp supp-val ps state env rest-args specials k)
  "Bind a supplied-p variable, then continue the lambda-list walk. Split out of
   %MINI-BIND-WALK (rather than being a LABELS inside it) so neither function
   closes over anything: both are on the per-call path of every interpreted
   call."
  (if (member supp specials)
      (progv (list supp) (list supp-val)
        (%mini-bind-walk ps state env rest-args specials k))
      (%mini-bind-walk ps state (acons supp supp-val env) rest-args specials k)))

(defun %mini-bind-walk (ps state env rest-args specials k)
  "Walk the remaining lambda list PS, extending ENV, and finally call K with it.
   See %MINI-BIND-PARAMS-CALL for what SPECIALS and K are for.

   The walk is ITERATIVE and recurses only where it must — at a special binding,
   whose PROGV has to wrap the remainder."
  (loop
    (when (null ps) (return-from %mini-bind-walk (funcall k env)))
    (let ((p (car ps)))
      (cond
        ((eq p '&optional) (setq state :optional ps (cdr ps)))
        ((or (eq p '&rest) (eq p '&body)) (setq state :rest ps (cdr ps)))
        ((eq p '&key) (setq state :key ps (cdr ps)))
        ((eq p '&aux) (setq state :aux ps (cdr ps)))
        ((eq p '&allow-other-keys) (setq ps (cdr ps)))
        (t
         (let ((var nil) (val nil) (supp nil) (supp-val nil) (next rest-args))
           (cond
             ((eq state :required)
              (setq var p val (car rest-args) next (cdr rest-args)))
             ((eq state :optional)
              (let* ((v (if (consp p) (car p) p))
                     (default (when (consp p) (cadr p)))
                     ;; An explicit NIL supplied-p is a binding of the constant
                     ;; NIL; an ABSENT one is just a short clause. (caddr p)
                     ;; cannot tell them apart, so test for the cell.
                     (sp (when (and (consp p) (cddr p))
                           (check-binding-name (caddr p) "lambda list")))
                     (present rest-args))
                (setq var v
                      val (if present (car rest-args)
                              (when default (%mini-eval default env)))
                      supp sp
                      supp-val (if present t nil)
                      next (if present (cdr rest-args) rest-args))))
             ((eq state :rest)
              (setq var p val rest-args))
             ((eq state :key)
              ;; p may be VAR, (VAR DEFAULT), (VAR DEFAULT SUPPLIED-P), or
              ;; ((:KEYWORD VAR) DEFAULT [SUPPLIED-P]).
              (let* ((head (if (consp p) (car p) p))
                     (v (if (consp head) (cadr head) head))
                     (kw (if (consp head) (car head)
                             (intern (symbol-name v) :keyword)))
                     (default (when (consp p) (cadr p)))
                     (sp (when (and (consp p) (cddr p))
                           (check-binding-name (caddr p) "lambda list")))
                     (cell (do ((a rest-args (cddr a))) ((null a) nil)
                             (when (eq (car a) kw) (return a)))))
                (setq var v
                      val (if cell (cadr cell)
                              (when default (%mini-eval default env)))
                      supp sp
                      supp-val (if cell t nil))))
             ((eq state :aux)
              (let* ((v (if (consp p) (car p) p))
                     (init (when (consp p) (cadr p))))
                (setq var v val (when init (%mini-eval init env))))))
           (check-binding-name var "lambda list")
           (setq ps (cdr ps))
           (cond
             ((member var specials)
              (return-from %mini-bind-walk
                (progv (list var) (list val)
                  (if supp
                      (%mini-bind-supp supp supp-val ps state env next specials k)
                      (%mini-bind-walk ps state env next specials k)))))
             (supp
              (setq env (acons var val env))
              (return-from %mini-bind-walk
                (%mini-bind-supp supp supp-val ps state env next specials k)))
             (t (setq env (acons var val env)
                      rest-args next)))))))))

(defun %mini-bind-params-call (params args env specials k)
  "Bind lambda list PARAMS to ARGS extending ENV, then call K with the result.
   Handles required, &optional (default + supplied-p), &rest, &key (default,
   supplied-p, ((:keyword var) ...)) and &aux. Init forms are evaluated
   left-to-right in the partial environment.

   A parameter named in SPECIALS is bound DYNAMICALLY (PROGV) instead of being
   put in the alist, and that binding is in effect for every LATER init form as
   well as for K. **That is why this takes a continuation instead of returning
   the env**: a PROGV scope cannot be handed back as a value.

   The previous shape built the whole env first and left the dynamic binding to
   %MINI-EVAL-PROGN, so an &AUX init ran BEFORE the parameter it reads was bound:

     (let ((x :bad)) (declare (special x))
       (flet ((%f () x))
         ((lambda (x &aux (y (%f))) (declare (special x)) y) :good)))   ; => :BAD

   (ansi-test LAMBDA.64.) Leaving the special parameter OUT of the alist is also
   what makes %MINI-EVAL-PROGN do the right thing afterwards: it only re-binds a
   declared name it can still find in ENV, so it now sees nothing to do and body
   references fall through to the dynamic value.

   This is the hottest path in the evaluator, so nothing here allocates for the
   common case (SPECIALS NIL): the walk is a plain loop in two top-level
   functions that close over nothing, and K is built once per closure rather than
   once per call (see %MINI-MAKE-CLOSURE). Written as a straightforward CPS walk
   with LABELS it cost ~10% on every interpreted call."
  (%mini-check-args params args)
  (%mini-bind-walk params :required env args specials k))

(defun %mini-bind-params (params args env)
  "Bind PARAMS to ARGS and RETURN the extended ENV. No parameter is treated as
   special — callers that need that use %MINI-BIND-PARAMS-CALL, which can keep a
   PROGV scope open across the rest of the binding and the body."
  (%mini-bind-params-call params args env '() #'identity))

(defvar *%mini-symbol-macro-marker* (make-symbol "SYMBOL-MACRO")
  "Head cons marker for a SYMBOL-MACROLET binding in %MINI-EVAL's ENV alist.

   It must be UNINTERNED. It used to be the interned symbol
   DOTCL-INTERNAL::SYMBOL-MACRO, which the SIL loader materialises lazily — the
   first interpreted SYMBOL-MACROLET brings it into the package, and from then on
   DO-SYMBOLS / DO-ALL-SYMBOLS hand the interpreter's own sentinel back to user
   code. Any interpreted variable then holding a list that starts with it (the
   DOLIST variable walking a package's symbol list is the obvious one) was read as
   a symbol-macro binding, and its SECOND element was evaluated as a variable:

     (do-symbols (sym (find-package \"DOTCL-INTERNAL\")) (symbol-name sym))
     => #<UNBOUND-VARIABLE: %THREADP>   ; the symbol after the marker

   That is ansi-test DO-ALL-SYMBOLS.1-13 / FIND-ALL-SYMBOLS.1 / PRINT.SYMBOL.RANDOM.3-4.
   The lazy interning is what made it look state-dependent: the same form passes
   until something interprets a SYMBOL-MACROLET, and the reported variable name
   changes with the package's symbol order. MAKE-SYMBOL puts the marker out of
   every package, so no iteration can ever produce it.")

(defun %mini-macroexpand-env (&optional lex-macros)
  "&ENVIRONMENT object to hand MACROEXPAND-1 from %MINI-EVAL: the MACROLET and
   SYMBOL-MACROLET bindings currently in scope, and nothing else. NIL when there
   are none. LEX-MACROS is the enclosing %MINI-MACROS alist (name . expander).

   %MINI-EVAL used to call (MACROEXPAND-1 FORM) with no environment at all, so an
   expander's &ENVIRONMENT was always empty. SYMBOL-MACROLET bindings live only in
   *SYMBOL-MACROS*, which the runtime MACROEXPAND-1 does not read — so
   (MACROEXPAND x env) inside an expander could not see them (ansi-test
   RESTART-CASE.30, where RESTART-CASE must decide whether its body is a signaling
   form and the body is a symbol macro).

   MACROLET used to reach an expander by a different route: the MACROLET case
   registered its expanders GLOBALLY in *MACROS* for the dynamic extent of the
   body. That made them visible with an empty environment — and visible to every
   other form interpreted during that extent, including the body of a separately
   defined function the body happened to call. Putting the bindings here instead
   is what makes them lexical: MACROEXPAND-1 consults an environment's macro table
   before the runtime one, which is the shadowing CL asks for, and nothing outside
   this scope is handed the table.

   The CAR still must not be *MACROS* itself — that would reorder macro lookup for
   every interpreted form. Only the lexically established bindings go in."
  (when (or *symbol-macros* lex-macros)
    (cons (when lex-macros
            (let ((ht (make-hash-table :test #'equal)))
              (dolist (entry lex-macros ht)
                ;; innermost first: the alist is already in that order, so an
                ;; outer binding must not overwrite an inner one
                (let ((key (if (null (car entry)) "NIL" (symbol-name (car entry)))))
                  (unless (nth-value 1 (gethash key ht))
                    (setf (gethash key ht) (cdr entry)))))))
          (when *symbol-macros*
            (let ((ht (make-hash-table :test #'equal)))
              (dolist (entry *symbol-macros* ht)
                ;; innermost first, exactly as above: *SYMBOL-MACROS* is pushed
                ;; onto by SYMBOL-MACROLET, so an outer binding sits behind an
                ;; inner one of the same name and must not overwrite it. Letting
                ;; it win made the reified &ENVIRONMENT expand a shadowed name to
                ;; the OUTERMOST expansion (CLHS 5.1.2.1 asks for the innermost).
                (let ((key (symbol-name (car entry))))
                  (unless (nth-value 1 (gethash key ht))
                    (setf (gethash key ht) (cdr entry))))))))))

(defun %mini-macro-fn (name env)
  "The lexical MACROLET binding of NAME in ENV, or NIL.

   Macros get their own key in ENV for the reasons FLET bindings (%MINI-FDEFN)
   and GO tags (%MINI-GO-TAGS) do: they are looked up before MACROEXPAND-1, which
   cannot see ENV and therefore always answered with the GLOBAL macro of the same
   name. ASSOC's EQL test is right here — a macro name is always a symbol (and
   NIL / T reach this too, which is the point: they are not SYMBOL instances, so
   nothing else in this evaluator recognises them in operator position)."
  (%mini-macro-fn-in name (cdr (assoc '%mini-macros env))))

(defun %mini-macro-fn-in (name lex-macros)
  "%MINI-MACRO-FN against an already-extracted %MINI-MACROS alist. The operator
   dispatch needs that alist twice per compound form — once to look the operator
   up, once to build the &ENVIRONMENT it hands MACROEXPAND-1 — and this runs on
   every interpreted form, so it is extracted once and passed in."
  (and (or (symbolp name) (null name))
       (cdr (assoc name lex-macros))))

(defun symbol-macrolet-violation-p (bindings body)
  "True when (symbol-macrolet BINDINGS . BODY) is a program error.
   CLHS SYMBOL-MACROLET: the consequences are undefined (and it is required to
   signal PROGRAM-ERROR) if a name is a constant or names a globally special
   variable, or if the body declares one of the names SPECIAL.
   Shared by COMPILE-SYMBOL-MACROLET and the %MINI-EVAL case so the two
   evaluators cannot drift — the interpreter used to accept all three."
  (dolist (b bindings)
    (let ((name (car b)))
      (when (or (constantp name) (global-special-p name))
        (return-from symbol-macrolet-violation-p t))))
  (let ((binding-names (mapcar #'car bindings)))
    (dolist (form body)
      (when (and (consp form) (eq (car form) 'declare))
        (dolist (decl (cdr form))
          (when (and (consp decl) (eq (car decl) 'special))
            (dolist (sname (cdr decl))
              (when (member sname binding-names)
                (return-from symbol-macrolet-violation-p t))))))))
  nil)

;;; Tail calls in the tree-walk evaluator.
;;;
;;; A call in tail position is not made; it is RETURNED, as
;;; (marker fn . args), to the trampoline that %MINI-MAKE-CLOSURE wraps around
;;; every interpreted function body. The trampoline runs the callee's body in its
;;; own loop when the callee is itself interpreted, so an interpreted tail
;;; recursion costs constant stack instead of one .NET frame per iteration —
;;; without which a tail-recursive loop, the ordinary CL idiom, could not be
;;; written at all on a build with no compiler (netstandard2.0 / WASM / AOT).
;;;
;;; The marker is a fresh cons held in a variable, so nothing a user program can
;;; construct is EQ to it. Only positions that are genuinely tail pass TAILP on:
;;; the last form of a body, both arms of IF, a BLOCK's body. A body running
;;; inside PROGV (a special binding, including a special parameter), UNWIND-PROTECT
;;; or a handler cluster is NOT tail — the binding has to outlive the call — and
;;; those simply never pass the flag.
(defvar *%mini-tail-marker* (list '%mini-tail-call))

(defun %mini-tail-p (x)
  (and (consp x) (eq (car x) *%mini-tail-marker*)))

;; %MACROEXPAND-1-OR-SELF is a runtime builtin, but COMPILE-MACROLET runs
;; %MINI-EVAL on the cross-compile host as well, where there is no such function.
;; The host version is MACROEXPAND-1 with its second value folded into the
;; identity of the result, which is exactly what the builtin returns.
#-dotcl
(defun %macroexpand-1-or-self (form &optional (env nil env-p))
  (multiple-value-bind (exp expandedp)
      (if env-p (macroexpand-1 form env) (macroexpand-1 form))
    (if expandedp exp form)))

(defun %mini-eval-args (forms env)
  "Evaluate FORMS left to right in ENV and return the list of their values.
   A loop, not (MAPCAR (LAMBDA (A) (%MINI-EVAL A ENV)) FORMS): that lambda
   captures ENV, and creating a capturing closure costs a flat ~512 B whatever
   it captures. Every interpreted function call evaluates its arguments here."
  (let ((head nil) (tail nil))
    (dolist (f forms head)
      (let ((cell (cons (%mini-eval f env) nil)))
        (if tail (setf (cdr tail) cell) (setq head cell))
        (setq tail cell)))))

(defvar *%mini-go-marker* (list '%mini-go)
  "Head cons of the value (GO tag) returns instead of throwing, when the caller
   asked for it. See %MINI-EVAL's GOP parameter.")

(defun %mini-go-p (x)
  (and (consp x) (eq (car x) *%mini-go-marker*)))

(defun %mini-eval-progn (forms env &optional bound-vars tailp gop)
  ;; (declare (special V)) in a body makes references to V within that body
  ;; DYNAMIC (CLHS 3.3.4). Which binding that refers to depends on whether the
  ;; enclosing form bound V, so BOUND-VARS names what it just bound:
  ;;
  ;;   V is in BOUND-VARS — the enclosing form's own binding becomes dynamic.
  ;;     PROGV it with the value that form computed.
  ;;       (let ((x 5)) (declare (special x)) ...)      ; x is dynamically 5
  ;;
  ;;   V is free here — the declaration only says "read V dynamically". Do NOT
  ;;     rebind: the reference must reach the value an OUTER special binding
  ;;     established.
  ;;       (let ((x :good)) (declare (special x))
  ;;         (let ((x :bad)) (locally (declare (special x)) x)))   ; => :GOOD
  ;;
  ;; Either way the lexical entry must be dropped for the body, or reads keep
  ;; hitting the alist and never consult the dynamic value at all. Taking the
  ;; value out of ENV unconditionally — what this did before — got the free case
  ;; exactly backwards: it re-bound V to the *shadowing lexical* value, so the
  ;; example above answered :BAD (ansi-test DO.14/17/19 and the LOCALLY tests).
  (let ((dyn-vars '()) (dyn-vals '()) (shadow '()))
    (dolist (f forms)
      (when (and (consp f) (eq (car f) 'declare))
        (dolist (d (cdr f))
          (when (and (consp d) (eq (car d) 'special))
            (dolist (v (cdr d))
              (push v shadow)
              (when (member v bound-vars)
                (let ((b (assoc v env)))
                  (when b
                    (push v dyn-vars)
                    (push (cdr b) dyn-vals)))))))))
    (when shadow
      (setq env (remove-if (lambda (e) (member (car e) shadow)) env)))
    ;; Evaluate all-but-last for effect; return the LAST form's values via a
    ;; tail %mini-eval so multiple values propagate (CL progn semantics).
    ;; ENV travels as an argument rather than being closed over. A local
    ;; function that captures anything costs a flat ~512 B to create, paid on
    ;; every entry here — and this runs for the body of every PROGN, LET,
    ;; LAMBDA and DEFUN the interpreter evaluates. Taking it as a parameter
    ;; leaves RUN capturing nothing, which allocates nothing.
    ;; GOP is passed straight through to every form: this body's value goes to a
    ;; caller that checks for the marker, and a non-last form's value is dropped
    ;; here, so a GO in either position can travel as a return value. If a
    ;; non-last form produces one, the rest of the body must not run.
    (labels ((run (fs e tail g)
               (cond ((null fs) nil)
                     ((null (cdr fs)) (%mini-eval (car fs) e tail g))
                     (t (let ((v (%mini-eval (car fs) e nil g)))
                          (if (and g (%mini-go-p v))
                              v
                              (run (cdr fs) e tail g)))))))
      (if dyn-vars
          ;; The PROGV must outlive the body, so its last form is not tail.
          (progv dyn-vars dyn-vals (run forms env nil gop))
          (run forms env tailp gop)))))

(defun %mini-lambda-list-var-names (params)
  "The variable names PARAMS binds, ignoring lambda-list keywords, default forms
   and supplied-p vars' spelling. Used to tell a (declare (special p)) that
   rebinds a parameter from one that merely reads an outer special."
  (let ((names '()))
    (dolist (p params)
      (cond ((and (symbolp p) p (char= (char (symbol-name p) 0) #\&)) nil)
            ((symbolp p) (push p names))
            ((consp p)
             (let ((head (car p)))
               (push (if (consp head) (cadr head) head) names))
             (when (caddr p) (push (caddr p) names)))))
    (nreverse names)))

(defun %mini-fdefn (name env)
  "The lexical FLET / LABELS binding of NAME in ENV, or NIL.

   Function names get their own key, for the same two reasons go tags did
   (%MINI-GO-TAGS): they are a separate namespace from variables (CLHS 3.1.1),
   and — unlike tags — a name can be the CONS (SETF F), which ASSOC's default
   EQL test can never match. Storing them in the variable alist therefore made
   every (setf f) local function invisible no matter how it was referenced, and
   made a VARIABLE whose value happened to be a function answer in operator
   position: (let ((list #'car)) (list 1 2)) called CAR.

   EQUAL is only needed for the (SETF F) names; a symbol name is settled by
   ASSOC's default EQL, which never matches one of those conses. Taking that
   branch keeps the keyword argument (and the args vector it allocates) out of
   a lookup every interpreted compound form makes."
  (let ((fns (cdr (assoc '%mini-functions env))))
    (if (symbolp name)
        (cdr (assoc name fns))
        (cdr (assoc name fns :test #'equal)))))

(defun %mini-fn-lambda (params bname body)
  "(LAMBDA PARAMS decls... (BLOCK BNAME body...)) for a named local function.
   The declarations must stay OUTSIDE the BLOCK: %MINI-MAKE-CLOSURE is the only
   caller that knows which names the lambda list bound, and %MINI-EVAL-PROGN only
   scans the forms handed to it. Burying them in the BLOCK made a parameter's
   (declare (special p)) look like a FREE declaration, so the parameter was never
   bound dynamically."
  (let ((decls '()) (rest body))
    (when (and (stringp (car rest)) (cdr rest)) (push (pop rest) decls))
    (loop while (and (consp (car rest)) (eq (caar rest) 'declare))
          do (push (pop rest) decls))
    `(lambda ,params ,@(nreverse decls) (block ,bname ,@rest))))

(defun %mini-check-lambda-list (params)
  "Signal PROGRAM-ERROR if PARAMS binds a constant, and return PARAMS.
   Checked when the closure is MADE, not when it is called: SBCL rejects
   (lambda (t) t) at that point, and a lambda that is never called would
   otherwise slip through the per-binding check in %MINI-BIND-WALK.
   %MINI-LAMBDA-LIST-VAR-NAMES cannot serve here -- it drops a bare NIL
   parameter, which is exactly one of the cases to catch."
  (dolist (p params)
    (unless (and (symbolp p) p (char= (char (symbol-name p) 0) #\&))
      (cond ((consp p)
             (let ((head (car p)))
               (check-binding-name (if (consp head) (cadr head) head)
                                   "lambda list"))
             ;; An explicit NIL supplied-p is a binding; an absent one is not.
             (when (cddr p) (check-binding-name (caddr p) "lambda list")))
            (t (check-binding-name p "lambda list")))))
  params)

(defun %mini-make-closure (lambda-form env &optional name)
  "Return a Lisp function that interprets LAMBDA-FORM in captured ENV.

   NAME, when given, is the string the function reports on the debugger call
   stack. Only a globally named definition passes one: a compiled backtrace lists
   DEFUN and DEFMETHOD frames and nothing else — FLET, LABELS and plain LAMBDA
   stay anonymous there — so the interpreted path names exactly the same set."
  (let* ((params (%mini-check-lambda-list (cadr lambda-form)))
         (body   (cddr lambda-form))
         (names  (%mini-lambda-list-var-names params))
         ;; The parameters are what THIS form binds, so a (declare (special p))
         ;; in the body rebinds the parameter rather than reading an outer one.
         ;; Those are bound during the lambda-list walk, not after it, so a later
         ;; &OPTIONAL / &KEY default or &AUX init sees the dynamic binding.
         (specials (intersection (%mini-body-special-decls body) names))
         ;; Built once per closure, not once per call: it depends only on the
         ;; lambda form, and this runs on every interpreted call.
         ;; A special parameter binds through PROGV around the body, which must
         ;; outlive any call the body ends in — so such a body is not tail.
         (body-tailp (null specials))
         ;; Everything %MINI-BIND-PARAMS-CALL needs to run this body again, so the
         ;; trampoline can re-enter it without calling the closure. Its identity
         ;; also stands in for this closure's: it is a fresh list per closure, so
         ;; EQ on it answers "is this callee me?" — which the closure cannot ask
         ;; about itself, having no name inside its own LET* init form. The
         ;; continuation is filled in below, once it exists.
         (info (list params env specials nil))
         ;; INFO under its own key in ENV is how a call in tail position tells
         ;; "this is me again" from "this is some other function" — the same
         ;; namespace trick %MINI-BLOCKS and %MINI-FUNCTIONS use.
         (k (lambda (new-env)
              (%mini-eval-progn body
                                (if body-tailp
                                    (cons (cons '%mini-self-info info) new-env)
                                    new-env)
                                names body-tailp)))
         ;; MULTIPLE-VALUE-CALL, not (let ((r ...))): binding the body's result
         ;; would truncate it to one value, and an interpreted function that ends
         ;; in (values a b) must still return both. A tail request is always
         ;; exactly one value, so only that shape is intercepted; everything else
         ;; is passed straight back out, including no values at all.
         ;; Built once per closure, like K. It closes over INFO, and a closure
         ;; that captures anything costs a flat ~450 B to create — written inline
         ;; as MULTIPLE-VALUE-CALL's function it was built again on every call,
         ;; which was most of what entering an interpreted function allocated.
         (tail-filter
          (lambda (&optional (v1 nil v1p) &rest more)
            (cond ((and v1p (null more) (%mini-tail-p v1)) (%mini-trampoline v1 info))
                  (v1p (apply #'values v1 more))
                  (t (values)))))
         (fn (lambda (&rest call-args)
               (multiple-value-call tail-filter
                 (%mini-bind-params-call params call-args env specials k)))))
    (setf (fourth info) k)
    (when name (%set-function-name fn name))
    ;; Every interpreted closure is variadic, so it would otherwise report zero
    ;; required parameters whatever the user wrote. Anything that reads a
    ;; function's arity then sees the evaluator's shape instead of the user's —
    ;; selecting the .NET overload for a delegate parameter, for one, which is why
    ;; a Lisp lambda handed to Enumerable.Select matched no overload at all.
    ;; Compiled code records the same count when it emits the function.
    ;; FBOUNDP because the cross-compiling host runs this while loading the
    ;; compiler, and %SET-FUNCTION-ARITY is a dotcl runtime primitive.
    (when (fboundp '%set-function-arity)
      (%set-function-arity fn (%mini-required-param-count params)))
    ;; What the trampoline needs to continue a tail call INTO this function
    ;; without calling it: everything %MINI-BIND-PARAMS-CALL takes.
    (when (fboundp '%set-fn-interp-info)
      (%set-fn-interp-info fn info))
    fn))

(defun %mini-call (fn args tailp env)
  "Call FN on ARGS, or — for a tail call back into the function being interpreted
   — hand it to that function's trampoline.

   Only a SELF call is handed over, and the restriction is what makes the
   handover safe as well as compiler-faithful. Returning the request unwinds
   everything between here and the trampoline, including any BLOCK whose CATCH
   the body established. For a self call that is right: re-entering the body
   builds a fresh block, exactly as the compiler's self-TCO jump does. For a call
   to some OTHER function it is not: a closure that function receives may
   RETURN-FROM one of those blocks, which would then no longer exist —
   (block lvl (let ((esc (lambda () (return-from lvl ...)))) (funcall esc)))
   throws to a dead tag. It would also drop a frame DOTCL:BACKTRACE still lists
   on the compiled path, which optimizes self calls only."
  (if (and tailp
           (fboundp '%fn-interp-info)
           (let ((self (cdr (assoc '%mini-self-info env))))
             (and self (eq (%fn-interp-info fn) self))))
      (list* *%mini-tail-marker* fn args)
      (apply fn args)))

(defun %mini-trampoline (r self-info)
  "Run out the tail calls R stands for, re-entering the body in THIS frame so a
   self-recursive tail loop costs constant stack instead of a .NET frame per
   iteration.

   Only a tail call back into the SAME function is looped. That is deliberate:
   it is exactly what the compiler optimizes (self-TCO), so the two evaluators
   keep agreeing — including on what DOTCL:BACKTRACE shows, since eliding a call
   to a DIFFERENT function would drop a frame the compiled path still lists.
   Every other tail call is an ordinary call made here, in tail position of this
   loop, so its values reach the caller intact."
  (loop
    (let* ((fn (cadr r))
           (args (cddr r))
           (info (and (fboundp '%fn-interp-info) (%fn-interp-info fn))))
      (unless (eq info self-info)
        ;; End of the chain: an ordinary call, in tail position of this loop so
        ;; its values reach the caller intact.
        (return (apply fn args)))
      (let ((next (multiple-value-call
                   (lambda (&optional (v1 nil v1p) &rest more)
                     (if (and v1p (null more) (%mini-tail-p v1))
                         v1
                         (list* *%mini-tail-values* (if v1p (cons v1 more) '()))))
                   (%mini-bind-params-call (first info) args (second info)
                                           (third info) (fourth info)))))
        (if (%mini-tail-p next)
            (setq r next)
            (return (values-list (cdr next))))))))

;;; Marks "the chain ended with these values" inside the trampoline. Distinct
;;; from the tail marker so the loop can tell a further tail call from a result
;;; that merely happens to be a list.
(defvar *%mini-tail-values* (list '%mini-tail-values))

(defun %mini-required-param-count (params)
  "Number of parameters before the first lambda-list keyword — what a compiled
   function records as its arity."
  (let ((n 0))
    (dolist (p params n)
      (if (and (symbolp p) p (char= (char (symbol-name p) 0) #\&))
          (return n)
          (incf n)))))

(defun %mini-expand-handler-case (form)
  "Rewrite (handler-case body (type (var) h...) ... [(:no-error ll forms...)])
   into block + handler-bind (+ multiple-value-call for :no-error), reusing
   primitives %mini-eval already supports. The condition clauses become
   handler-bind handlers that non-locally exit the block with their result; a
   :no-error clause receives the body's values when no condition is signalled."
  (let* ((body (cadr form))
         (all-clauses (cddr form))
         (no-error (find :no-error all-clauses :key (lambda (c) (and (consp c) (car c)))))
         (clauses (remove no-error all-clauses))
         (blk (gensym "HC-BLK"))
         (handler-bindings
          (mapcar (lambda (clause)
                    (let* ((type (car clause))
                           (vars (cadr clause))
                           (hbody (cddr clause))
                           ;; (error (nil) ...) names the constant NIL, which
                           ;; cannot be bound; an EMPTY list is the "no variable"
                           ;; spelling and gets a gensym.
                           (cvar (if vars
                                     (check-binding-name (car vars) "HANDLER-CASE")
                                     (gensym "HC-C"))))
                      (list type
                            `(lambda (,cvar)
                               ,@(unless vars `((declare (ignore ,cvar))))
                               (return-from ,blk (progn ,@hbody))))))
                  clauses)))
    (if no-error
        `(block ,blk
           (multiple-value-call
               (lambda ,(cadr no-error) ,@(cddr no-error))
             (handler-bind ,handler-bindings ,body)))
        `(block ,blk
           (handler-bind ,handler-bindings ,body)))))

(defun %mini-eval-handler-bind (form env)
  "Interpret (handler-bind ((type handler-form)...) body...).
   Establishes the cluster through the runtime primitive, which runs the body
   under a .NET try/catch: a raw .NET exception is converted to a condition and
   signalled through the cluster, so an interpreted handler sees the same
   conditions a compiled one does."
  (%call-with-handler-cluster
   (mapcar (lambda (b) (cons (car b) (%mini-eval (cadr b) env)))
           (cadr form))
   (%mini-make-closure (list* 'lambda '() (cddr form)) env)))

(defun %mini-eval (form env &optional tailp gop)
  "Interpret FORM in ENV (alist of (sym . val)). No Reflection.Emit needed.

   TAILP says FORM's value is returned straight to the enclosing function body,
   so a call it ends in may be handed to the trampoline instead of made here
   (see *%MINI-TAIL-MARKER*). Callers that do anything with the value — bind it,
   test it, unwind past it — leave it NIL, which is the default.

   GOP says the caller inspects this form's value for a GO marker and will act
   on it, so a (GO tag) reached from here may return one instead of throwing.
   THROW out of the interpreter is expensive far beyond the throw itself: the
   .NET unwind walks every interpreter frame between the GO and its TAGBODY, and
   the interpreter stacks a dozen frames per level of Lisp nesting. The same
   throw costs about 1.0 us from compiled code and 4.8 us from here, which made
   one GO 3.1 us — 45% of an interpreted DOTIMES iteration.

   Only positions whose caller actually checks pass GOP on: the forms of a
   TAGBODY body, the forms of a PROGN/LET/LAMBDA body under one, and the arms of
   an IF. Everywhere else it stays NIL and GO throws exactly as before, so a GO
   from a place the marker could not travel through — an argument, an init form,
   a test — is unaffected."
  (cond
    ;; Self-evaluating
    ((null form) nil)
    ((or (numberp form) (stringp form) (characterp form)) form)
    ((keywordp form) form)
    ;; Variable lookup (also check symbol-macrolet bindings via lookup-symbol-macro)
    ((symbolp form)
     (let ((b (assoc form env)))
       (if b
           ;; Could be (SYMBOL-MACRO expansion) from symbol-macrolet
           (let ((v (cdr b)))
             (if (and (consp v) (eq (car v) *%mini-symbol-macro-marker*))
                 (%mini-eval (cadr v) env)
                 v))
           (multiple-value-bind (sm found) (lookup-symbol-macro form)
             (if found
                 (%mini-eval sm env)
                 ;; Not lexically bound: a global symbol macro
                 ;; (define-symbol-macro) expands via macroexpand-1; otherwise
                 ;; treat as a dynamic/global variable reference.
                 (multiple-value-bind (exp expandedp) (macroexpand-1 form)
                   (if expandedp
                       (%mini-eval exp env)
                       (symbol-value form))))))))
    ((consp form)
     ;; A lexical FLET/LABELS binding shadows a GLOBAL MACRO of the same name
     ;; (CLHS 3.1.2.1.2.4). MACROEXPAND-1 cannot see ENV, so expanding first
     ;; turned (flet ((f () :good)) (f)) into the global macro's expansion.
     (let ((lex-fn (%mini-fdefn (car form) env)))
     (if lex-fn
         (%mini-call lex-fn
                     (%mini-eval-args (cdr form) env)
                     tailp env)
     ;; A MACROLET binding shadows a global macro for the same reason, and needs
     ;; its own lookup for the same reason: MACROEXPAND-1 consults the runtime
     ;; macro table before it ever reaches the *MACROS* entry the MACROLET case
     ;; writes, so (macrolet ((m () :good)) (m)) took the GLOBAL M's expansion
     ;; whenever one existed (ansi-test MACROLET.50, and the entries of
     ;; MACROLET.16 that name a CL macro).
     ;;
     ;; It also settles the operator NIL and T. Those are not SYMBOL instances
     ;; here, so MACROEXPAND-1 does not treat them as macro calls and the
     ;; fall-through called (SYMBOL-FUNCTION NIL), which type-errors —
     ;; (macrolet ((nil () ''a)) (nil)) died there (ansi-test MACROLET.15).
     ;; Looking the operator up in ENV first never reaches that path.
     (let* ((lex-macros (cdr (assoc '%mini-macros env)))
            (lex-macro (%mini-macro-fn-in (car form) lex-macros)))
     (if lex-macro
         ;; The expander is handed the same environment MACROEXPAND-1 would get,
         ;; so an &ENVIRONMENT parameter can expand the MACROLET bindings in scope
         ;; (they are lexical here, so an environment built from the globals would
         ;; not show them).
         (%mini-eval (funcall lex-macro form (%mini-macroexpand-env lex-macros)) env tailp gop)
     (if (eq (car form) 'multiple-value-bind)
         ;; Handled before macroexpansion, like HANDLER-BIND just below. The
         ;; compiled expansion takes the values out of a per-thread snapshot
         ;; immediately after producing them -- sound in compiled code, where
         ;; nothing runs in between, but here the "in between" IS the
         ;; interpreter, whose own MULTIPLE-VALUE-BINDs overwrite that snapshot
         ;; (binding (floor 17 5) produced FIND-SYMBOL's two values). Bind from
         ;; a list instead, which is what this form did before the snapshot.
         (let* ((mvb-vars (mapcar (lambda (v)
                                    (check-binding-name v "MULTIPLE-VALUE-BIND"))
                                  (cadr form)))
                (mvb-vals (multiple-value-list (%mini-eval (caddr form) env))))
           (labels ((bind-seq (vs vls benv tail)
                      (if (null vs)
                          (%mini-eval-progn (cdddr form) benv mvb-vars tail gop)
                          (let ((var (car vs)) (val (car vls)))
                            (if (%runtime-special-p var)
                                (progv (list var) (list val)
                                  (bind-seq (cdr vs) (cdr vls) benv nil))
                                (bind-seq (cdr vs) (cdr vls)
                                          (cons (cons var val) benv) tail))))))
             (bind-seq mvb-vars mvb-vals env tailp)))
     (if (eq (car form) 'handler-bind)
         ;; HANDLER-BIND is implemented here rather than through its macro, the
         ;; way COMPILE-FORM consults its handler table before macroexpanding.
         ;; The macro exists so a code walker can see the body inline, and its
         ;; expansion (%PUSH-HANDLER-CLUSTER + UNWIND-PROTECT) establishes the
         ;; cluster without wrapping the body in a .NET try/catch — which is
         ;; correct for a walker and wrong for an evaluator, because a raw .NET
         ;; throw does not go through SIGNAL and so flew straight past the
         ;; handlers. %CALL-WITH-HANDLER-CLUSTER runs the body under a catch that
         ;; converts and signals. HANDLER-CASE expands into HANDLER-BIND, so it
         ;; arrives here too.
         (%mini-eval-handler-bind form env)
     ;; First try macroexpand-1: handles destructuring-bind, when, cond, etc.
     ;; The environment carries the enclosing SYMBOL-MACROLET bindings (and only
     ;; those) so an expander's (MACROEXPAND x env) can see them. The *SYMBOL-MACROS*
     ;; test is inline rather than inside %MINI-MACROEXPAND-ENV: this runs for every
     ;; interpreted compound form, and the overwhelmingly common case is "no symbol
     ;; macros in scope", which must not cost a call.
     ;; %MACROEXPAND-1-OR-SELF, not MACROEXPAND-1: the two-value version decides
     ;; "expanded" by comparing the result against the input anyway, so the same
     ;; test here loses nothing — and returning two values allocates ~96 B on
     ;; every compound form the evaluator looks at, expanding or not.
     (let ((expanded
            (if (or *symbol-macros* lex-macros)
                (%macroexpand-1-or-self form (%mini-macroexpand-env lex-macros))
                (%macroexpand-1-or-self form))))
       (if (not (eq expanded form))
           (%mini-eval expanded env tailp gop)
           ;; Dispatch on special form operators
           (let ((op (car form)))
             (case op
               (quote   (cadr form))
               ;; The test's value is used here, so it never carries a marker;
               ;; the arms' values are this form's value, so they can.
               (if      (if (%mini-eval (cadr form) env)
                            (%mini-eval (caddr form) env tailp gop)
                            (when (cdddr form) (%mini-eval (cadddr form) env tailp gop))))
               (progn   (%mini-eval-progn (cdr form) env nil tailp gop))
               (let
                ;; Special (dynamic) vars bind via progv; lexical vars via alist.
                ;; All init forms are evaluated in the outer env (parallel).
                (let ((spec-vars '()) (spec-vals '()) (lex '()))
                  (dolist (b (cadr form))
                    (let ((var (check-binding-name
                                (if (consp b) (car b) b) "LET"))
                          (val (when (consp b) (%mini-eval (cadr b) env))))
                      (if (%runtime-special-p var)
                          (progn (push var spec-vars) (push val spec-vals))
                          (push (cons var val) lex))))
                  (let ((new-env (append lex env))
                        (bound (mapcar (lambda (b) (if (consp b) (car b) b))
                                       (cadr form))))
                    (if spec-vars
                        ;; PROGV must outlive the body: not a tail position.
                        ;; A GO marker still travels — returning it unwinds the
                        ;; PROGV, which is what leaving the LET has to do.
                        (progv (nreverse spec-vars) (nreverse spec-vals)
                          (%mini-eval-progn (cddr form) new-env bound nil gop))
                        (%mini-eval-progn (cddr form) new-env bound tailp gop)))))
               (let*
                ;; Sequential (CLHS 3.1.2.1.1.2): each binding is in effect while
                ;; the REMAINING init forms are evaluated. Collecting the specials
                ;; and PROGV-ing them all at the end got that backwards: a later
                ;; init form ran with the special still at its outer value, and
                ;; then PROGV installed the saved value over whatever that form
                ;; had done to it. So
                ;;   (let* ((*ctr* 0) (s (make-s2))) ... *ctr* ...)
                ;; where the struct's slot default is (incf *ctr*) saw *ctr* = 0
                ;; in the body although the counter had been incremented — the
                ;; increment was applied to the outer binding and then masked.
                ;; (ansi-test structures: 370 of that category's failures.)
                ;; Bind one at a time instead, so each PROGV is established before
                ;; the next init form is evaluated and the body runs inside all of
                ;; them. Lexicals keep accumulating in ENV as before.
                (labels ((bind-seq (bindings benv tail)
                           (if (null bindings)
                               (%mini-eval-progn
                                (cddr form) benv
                                (mapcar (lambda (b) (if (consp b) (car b) b))
                                        (cadr form))
                                tail gop)
                               (let* ((b (car bindings))
                                      (var (check-binding-name
                                            (if (consp b) (car b) b) "LET*"))
                                      (val (when (consp b)
                                             (%mini-eval (cadr b) benv))))
                                 (if (%runtime-special-p var)
                                     ;; PROGV must outlive the body: from here in,
                                     ;; nothing under it is a tail position.
                                     (progv (list var) (list val)
                                       (bind-seq (cdr bindings) benv nil))
                                     (bind-seq (cdr bindings)
                                               (cons (cons var val) benv) tail))))))
                  (bind-seq (cadr form) env tailp)))
               (setq
                ;; CLHS 5.1.2.4 / setq: assigning a name that is a symbol macro
                ;; is SETF of its expansion, not a variable assignment. The env
                ;; alist holds a symbol-macrolet binding as
                ;; (name SYMBOL-MACRO expansion) — the same shape the variable
                ;; read branch above already unwraps — so writing to (cdr b)
                ;; here did not just assign the wrong place, it OVERWROTE the
                ;; binding with the value and destroyed the symbol macro for the
                ;; rest of the body. (ansi-test SETF-SYMBOL-MACRO.1-3, and
                ;; PSETQ/PSETF/ROTATEF, which macroexpand into SETQ.)
                (let (result)
                  (let ((pairs (cdr form)))
                    (loop while pairs do
                      (let* ((var (car pairs))
                             (vform (cadr pairs))
                             (b   (assoc var env)))
                        (if (and b (consp (cdr b)) (eq (cadr b) *%mini-symbol-macro-marker*))
                            ;; Hand the whole assignment back to the evaluator as
                            ;; a SETF of the expansion, so the value form is
                            ;; evaluated exactly once and any place form works.
                            (setq result
                                  (%mini-eval (list 'setf (caddr b) vform) env))
                            (let ((val (%mini-eval vform env)))
                              (if b (setf (cdr b) val) (set var val))
                              (setq result val)))
                        (setq pairs (cddr pairs)))))
                  result))
               (function
                ;; A lexical FLET/LABELS binding shadows the global definition, so
                ;; the function namespace is consulted first (ansi-test BLOCK.5 /
                ;; BLOCK.10, where #'%f is handed to MAPCAR). This used to look in
                ;; the variable alist for an entry whose value happened to be a
                ;; function, because that was where FLET put its bindings; with a
                ;; real namespace the guess is unnecessary, and a (SETF F) name —
                ;; a CONS, which ASSOC/EQL never matched — now resolves too.
                ;; %COERCE-TO-FUNCTION for the same reason the operator branch
                ;; uses it: SYMBOL-FUNCTION has no cross-package bare-name bridge,
                ;; so #'class-precedence-list failed under the interpreter while
                ;; the compiled #' and (funcall 'name ...) both worked.
                (let ((fn (cadr form)))
                  (cond ((%mini-fdefn fn env))
                        ((symbolp fn) (%coerce-to-function fn))
                        ((and (consp fn) (eq (car fn) 'setf)) (fdefinition fn))
                        (t (%mini-make-closure fn env)))))
               (lambda
                (%mini-make-closure form env))
               (flet
                ;; Named local functions get an implicit block named after the
                ;; function ((setf f) -> block f), so (return-from f ...) works.
                ;; Bindings go in the FUNCTION namespace (see %MINI-FDEFN).
                (let ((fns '()) (outer (cdr (assoc '%mini-functions env))))
                  (dolist (def (cadr form))
                    (let ((bname (if (consp (car def)) (cadr (car def)) (car def))))
                      (push (cons (car def)
                                  (%mini-make-closure
                                   (%mini-fn-lambda (cadr def) bname (cddr def)) env))
                            fns)))
                  (%mini-eval-progn
                   (cddr form)
                   (cons (cons '%mini-functions (append (nreverse fns) outer)) env))))
               (labels
                ;; Pre-allocate cells so closures can mutually reference each other,
                ;; then fill them in against an env that already has every name.
                (let* ((cells (mapcar (lambda (def) (cons (car def) nil)) (cadr form)))
                       (outer (cdr (assoc '%mini-functions env)))
                       (new-env (cons (cons '%mini-functions (append cells outer)) env)))
                  (dolist (def (cadr form))
                    (let ((cell (assoc (car def) cells :test #'equal))
                          (bname (if (consp (car def)) (cadr (car def)) (car def))))
                      (setf (cdr cell)
                            (%mini-make-closure
                             (%mini-fn-lambda (cadr def) bname (cddr def)) new-env))))
                  (%mini-eval-progn (cddr form) new-env)))
               (block
                ;; BLOCK names are LEXICAL (CLHS 3.1.2.1.2.4), so each entry gets a
                ;; fresh tag object recorded in its own namespace in ENV — the same
                ;; shape %MINI-GO-TAGS uses, and for the same reason.
                ;;
                ;; The block NAME itself used to be the CATCH tag, which made the
                ;; scope DYNAMIC: a same-named inner block hid the outer one for
                ;; everything running inside it, including a closure whose text is
                ;; lexically outside. In
                ;;   (block done
                ;;     (flet ((%f (x) (return-from done x)))
                ;;       (block done (mapcar #'%f '(good bad bad))))
                ;;     'bad)
                ;; %f's RETURN-FROM refers to the OUTER DONE, but the throw landed
                ;; on the inner one, so MAPCAR simply took its next element and the
                ;; form returned BAD (ansi-test BLOCK.10). A closure carries the ENV
                ;; it was made in, so looking the tag up there is exactly the
                ;; lexical rule.
                (let* ((name (cadr form))
                       (tag (list '%mini-block name))
                       (new-env (cons (cons '%mini-blocks
                                            (cons (cons name tag)
                                                  (cdr (assoc '%mini-blocks env))))
                                      env)))
                  (catch tag (%mini-eval-progn (cddr form) new-env nil tailp))))
               (return-from
                ;; No lexically visible block of that name: fall back to throwing on
                ;; the name, which is what every established block used as its tag
                ;; before. Keeps a RETURN-FROM that crosses an evaluator boundary
                ;; behaving as it did rather than turning into a new failure mode.
                (let ((b (assoc (cadr form) (cdr (assoc '%mini-blocks env)))))
                  (throw (if b (cdr b) (cadr form))
                         (when (cddr form) (%mini-eval (caddr form) env)))))
               (the    (%mini-eval (caddr form) env))
               (locally (%mini-eval-progn (cdr form) env nil tailp))
               (eval-when
                ;; In an evaluator (not compile-file), eval the body iff :execute
                ;; is among the situations (CLHS 3.2.3.1).
                (let ((situations (cadr form)))
                  (when (or (member :execute situations)
                            (member 'cl:eval situations)) ; legacy EVAL keyword
                    (%mini-eval-progn (cddr form) env))))
               (symbol-macrolet
                ;; Extend env with symbol macro bindings, AND bind *SYMBOL-MACROS*.
                ;; That dynamically-scoped alist is what %MACROLET-EXPANDER-FORM
                ;; turns into the &ENVIRONMENT object it hands a macrolet expander:
                ;;   (cons *macros* <hash of *symbol-macros*>)
                ;; so an expander's (macroexpand x env) can only see an enclosing
                ;; SYMBOL-MACROLET if that special was bound. The interpreter pushed
                ;; onto its own alist only, leaving *SYMBOL-MACROS* empty, so the
                ;; expander was handed an environment with no symbol macros in it
                ;; and MACROEXPAND returned the symbol unchanged (ansi-test
                ;; MACROLET.13/14/15). COMPILE-SYMBOL-MACROLET binds the same
                ;; special, which is exactly why the compiled path was right — the
                ;; expander builder is deliberately shared between the two paths,
                ;; so the divergence was in who populates what it reads.
                ;;
                ;; The same split applied to the CLHS program-error cases (a
                ;; constant / globally special name, or a body that declares one
                ;; of the names special): COMPILE-SYMBOL-MACROLET rejected them
                ;; and the interpreter silently accepted, so a symbol-macro could
                ;; shadow a special variable under EVAL only (ansi-test
                ;; SYMBOL-MACROLET.ERROR.1/2/3). The predicate is shared.
                (when (symbol-macrolet-violation-p (cadr form) (cddr form))
                  (error 'program-error))
                (let ((new-env env) (new-sm *symbol-macros*))
                  (dolist (binding (cadr form))
                    (push (cons (car binding)
                                (list *%mini-symbol-macro-marker* (cadr binding)))
                          new-env)
                    (push (cons (car binding) (cadr binding)) new-sm))
                  (let ((*symbol-macros* new-sm))
                    (%mini-eval-progn (cddr form) new-env))))
               (macrolet
                ;; The bindings go into a lexical %MINI-MACROS namespace in ENV. The
                ;; operator dispatch consults it (%MINI-MACRO-FN), and it is what the
                ;; &ENVIRONMENT handed to MACROEXPAND-1 is built from
                ;; (%MINI-MACROEXPAND-ENV) — an environment's macro table is consulted
                ;; before the runtime one, so a MACROLET over a name that already had
                ;; a global macro shadows it, which is what CL asks for.
                ;;
                ;; The expanders used to ALSO be registered in the global *MACROS*
                ;; table for the dynamic extent of the body. That is how they used to
                ;; become visible to MACROEXPAND-1, which cannot see ENV — and it made
                ;; them visible far too widely: any form interpreted during that
                ;; extent saw them, including the body of a separately defined
                ;; function that the body called, so
                ;;   (defun h () :global) (defun c () (h))
                ;;   (macrolet ((h () :macro)) (list (h) (c)))
                ;; answered (:MACRO :MACRO) interpreted and (:MACRO :GLOBAL) compiled.
                (let ((lex-macros '()))
                  (dolist (def (cadr form))
                    (push (cons (car def)
                                (%mini-eval (%macrolet-expander-form (cadr def) (cddr def))
                                            env))
                          lex-macros))
                  (%mini-eval-progn
                   (cddr form)
                   (cons (cons '%mini-macros
                               (append (nreverse lex-macros)
                                       (cdr (assoc '%mini-macros env))))
                         env))))
               (tagbody
                (let* ((tb-id (list 'tagbody))
                       ;; Parse body into segments: list of (tag . forms)
                       (segs
                        (let ((cur-tag nil) (cur-forms '()) (result '()))
                          (dolist (item (cdr form))
                            (if (or (symbolp item) (integerp item))
                                (progn
                                  (push (cons cur-tag (nreverse cur-forms)) result)
                                  (setq cur-tag item cur-forms '()))
                                (push item cur-forms)))
                          (push (cons cur-tag (nreverse cur-forms)) result)
                          (nreverse result)))
                       ;; Go tags live in their OWN namespace (CLHS 3.1.1), so they
                       ;; are collected under one private key instead of being
                       ;; pushed onto ENV under the tag symbol itself. Sharing the
                       ;; symbol key with variables was actively destructive: in
                       ;;   (let ((even nil)) (dotimes (i 8) ... (go even) ...
                       ;;                       even (push i even) ...))
                       ;; the tag EVEN and the variable EVEN landed on the same
                       ;; alist entry, so (push i even) — a SETQ — overwrote the
                       ;; GO-TARGET with a list and the NEXT (go even) reported
                       ;; "tag not found" (ansi-test DOTIMES.12 and the DO / DO* /
                       ;; DOLIST / TAGBODY tests of the same shape). Inner tagbodies
                       ;; shadow outer ones by consing in front.
                       (outer-tags (cdr (assoc '%mini-go-tags env)))
                       (tagged-env
                        (let ((tl '()) (idx 0))
                          (dolist (seg segs)
                            (when (car seg)
                              (push (cons (car seg) (cons tb-id idx)) tl))
                            (incf idx))
                          (cons (cons '%mini-go-tags (append (nreverse tl) outer-tags))
                                env)))
                       (done-marker (list 'done))
                       (start-idx 0))
                  ;; Body forms are evaluated with GOP on, so a GO that can reach
                  ;; here as a return value does that instead of throwing. The
                  ;; CATCH stays for the ones that cannot — a GO inside an
                  ;; argument, an init form, a handler, an interpreted function
                  ;; called from here — which still throw.
                  (loop
                    (let ((result
                           (catch tb-id
                             (let ((idx 0) (jump nil))
                               (dolist (seg segs)
                                 (when (and (null jump) (>= idx start-idx))
                                   (dolist (f (cdr seg))
                                     (let ((v (%mini-eval f tagged-env nil t)))
                                       (when (%mini-go-p v)
                                         (setq jump v)
                                         (return)))))
                                 (incf idx))
                               (cond ((null jump) done-marker)
                                     ;; Ours: loop again from the tag's segment.
                                     ((eq (cadr jump) tb-id) (cddr jump))
                                     ;; An enclosing TAGBODY's tag. Pass it on as
                                     ;; a marker if our own caller checks, else
                                     ;; put it back on the throw path.
                                     (gop (return-from %mini-eval jump))
                                     (t (throw (cadr jump) (cddr jump))))))))
                      (if (eq result done-marker)
                          (return nil)
                          (setq start-idx result))))))
               (go
                ;; Tags are looked up in the tag namespace only (see TAGBODY), so a
                ;; variable of the same name can neither hide a tag nor be clobbered
                ;; by one. Entry is (tag tb-id . index).
                (let* ((tag (cadr form))
                       (b (assoc tag (cdr (assoc '%mini-go-tags env)))))
                  (cond ((null b) (error "%mini-eval: go tag ~S not found" tag))
                        ;; (cdr b) is (tb-id . index), which is what the marker
                        ;; carries and what the CATCH would have thrown.
                        (gop (cons *%mini-go-marker* (cdr b)))
                        (t (throw (cadr b) (cddr b))))))
               (declare nil)
               (catch
                (catch (%mini-eval (cadr form) env)
                  (%mini-eval-progn (cddr form) env)))
               (throw
                (throw (%mini-eval (cadr form) env)
                       (%mini-eval (caddr form) env)))
               (unwind-protect
                (unwind-protect (%mini-eval (cadr form) env)
                  (%mini-eval-progn (cddr form) env)))
               (multiple-value-call
                ;; (m-v-call fn form*) — splice all values of each form as args.
                (apply (%mini-eval (cadr form) env)
                       (mapcan (lambda (a)
                                 (multiple-value-list (%mini-eval a env)))
                               (cddr form))))
               (multiple-value-prog1
                (multiple-value-prog1 (%mini-eval (cadr form) env)
                  (%mini-eval-progn (cddr form) env)))
               (progv
                (progv (%mini-eval (cadr form) env)
                       (%mini-eval (caddr form) env)
                  (%mini-eval-progn (cdddr form) env)))
               (defun
                ;; Interpreted defun: install an interpreted closure (with the
                ;; implicit block) as the function. No compilation — works on
                ;; emit-free targets.
                ;;
                ;; %MINI-FN-LAMBDA, not a hand-built (lambda params (block ...)):
                ;; the declarations have to stay OUTSIDE the block. Buried inside
                ;; it, a parameter's (declare (special p)) looks like a FREE
                ;; declaration to %MINI-EVAL-PROGN — which then drops P's lexical
                ;; entry without establishing a dynamic binding, so the body reads
                ;; an unbound variable:
                ;;   (defun f (x &aux (y 10)) (declare (special x)) (+ x y))
                ;;   (f 5)   =>  Unbound variable: X
                ;; FLET / LABELS already went through %MINI-FN-LAMBDA for exactly
                ;; this reason; DEFUN was the one that did not. It only shows when
                ;; the DEFUN ITSELF is interpreted — LOAD compiles top-level forms
                ;; on an emit build, so it took running the suite on an emit-free
                ;; build to surface it.
                ;; The docstring is registered the same way COMPILE-DEFUN does
                ;; it, with (setf documentation). %MINI-FN-LAMBDA hoists it out of
                ;; the implicit block, but hoisting only moves it — as a form in
                ;; the lambda body it is evaluated and discarded, so an
                ;; interpreted DEFUN recorded nothing and (documentation 'f
                ;; 'function) answered NIL. Same shape as the DEFVAR family.
                ;; A docstring needs a body after it; "foo" alone IS the body.
                (let* ((name (cadr form))
                       (params (caddr form))
                       (fbody (cdddr form))
                       (bname (if (consp name) (cadr name) name))
                       (doc (when (and (stringp (car fbody)) (cdr fbody))
                              (car fbody)))
                       (fn (%mini-make-closure
                            (%mini-fn-lambda params bname fbody)
                            env (mangle-name name))))
                  (setf (fdefinition name) fn)
                  (when doc (funcall #'(setf documentation) doc name 'function))
                  name))
               ;; The docstring is the 4th element and is stored INDEPENDENTLY of the
               ;; value: DEFVAR skips the init form when the variable is already
               ;; bound, but its documentation is still updated (CLHS defvar —
               ;; ansi-test DEFVAR.5 turns exactly on that). COMPILE-DEFVAR emitted
               ;; the SetVariableDocumentation call; these cases dropped the string
               ;; on the floor, so (documentation name 'variable) was NIL under EVAL
               ;; (DEFVAR.4/5, DEFPARAMETER.4/5).
               (defvar
                (let ((name (cadr form)) (doc (cadddr form)))
                  (proclaim (list 'special name))
                  (when (and (cddr form) (not (boundp name)))
                    (set name (%mini-eval (caddr form) env)))
                  (when (stringp doc) (setf (documentation name 'variable) doc))
                  name))
               (defparameter
                (let ((name (cadr form)) (doc (cadddr form)))
                  (proclaim (list 'special name))
                  (set name (%mini-eval (caddr form) env))
                  (when (stringp doc) (setf (documentation name 'variable) doc))
                  name))
               (defconstant
                ;; SET-SYMBOL-CONSTANT is what makes CONSTANTP answer T; without it
                ;; a constant defined through EVAL was an ordinary special. That
                ;; matters beyond introspection: SYMBOL-MACROLET's program-error
                ;; check consults CONSTANTP, and nothing could refuse to rebind
                ;; such a name. COMPILE-DEFVAR emitted the call for defconstant;
                ;; this case only did the SET and the PROCLAIM.
                (let ((name (cadr form)) (doc (cadddr form)))
                  (proclaim (list 'special name))
                  ;; %DEFINE-CONSTANT, not SET + SET-SYMBOL-CONSTANT: SET now
                  ;; refuses to assign a symbol that is already constant, and the
                  ;; same primitive is what refuses a redefinition to a non-EQL
                  ;; value. The compiled path emits a call to it too.
                  (%define-constant name (%mini-eval (caddr form) env))
                  (when (stringp doc) (setf (documentation name 'variable) doc))
                  name))
               (define-symbol-macro
                ;; Register into the global symbol-macro registry that
                ;; macroexpand-1 (and the symbol-read branch above) consults.
                (let ((name (cadr form)) (expansion (caddr form)))
                  (%register-symbol-macro-rt name expansion)
                  name))
               (defmacro
                ;; Interpreted defmacro: build a 2-arg (form env) interpreted
                ;; expander and register it via (setf (macro-function name) ...),
                ;; which feeds the runtime macro registry that macroexpand-1 reads.
                ;; Handles &whole / &environment / dotted tail; &body via
                ;; destructuring-bind. No compilation.
                (let* ((name (cadr form))
                       (ll0 (caddr form))
                       (mbody (cdddr form))
                       (whole-var (when (and (consp ll0) (eq (car ll0) '&whole))
                                    (cadr ll0)))
                       (rest-ll (if whole-var (cddr ll0) ll0))
                       (env-var nil)
                       (clean-ll
                        (let ((res '()) (p rest-ll))
                          (loop while (consp p) do
                            (cond ((eq (car p) '&environment)
                                   (setq env-var (cadr p) p (cddr p)))
                                  (t (push (car p) res) (setq p (cdr p)))))
                          (when p (push '&rest res) (push p res)) ; dotted tail
                          (nreverse res)))
                       (wv (or whole-var (gensym "MWHOLE")))
                       (ev (or env-var (gensym "MENV")))
                       (expander
                        (%mini-make-closure
                         `(lambda (,wv ,ev)
                            (declare (ignorable ,wv ,ev))
                            (destructuring-bind ,clean-ll (cdr ,wv)
                              (block ,name ,@mbody)))
                         env)))
                  (setf (macro-function name) expander)
                  ;; What a tool should show for a macro is the lambda list the
                  ;; user wrote, not the (whole env) pair this expander takes --
                  ;; %MINI-MAKE-CLOSURE has just recorded the latter, and on an
                  ;; emit-free build this is the only DEFMACRO path there is, so
                  ;; DOTCL:FUNCTION-LAMBDA-LIST answered (#:MWHOLE #:MENV).
                  (when (fboundp '%set-function-lambda-list)
                    (%set-function-lambda-list expander ll0))
                  name))
               (handler-case
                (%mini-eval (%mini-expand-handler-case form) env))
               (restart-case
                ;; (restart-case body (name (params) [:report/:interactive/:test x]* h...) ...)
                (%call-with-restart-cluster
                 (mapcar (lambda (clause)
                           (let ((name (car clause))
                                 (params (cadr clause))
                                 (rest (cddr clause)))
                             (loop while (and rest (member (car rest)
                                                           '(:report :interactive :test)))
                                   do (setq rest (cddr rest)))
                             (cons name
                                   (%mini-make-closure `(lambda ,params ,@rest) env))))
                         (cddr form))
                 (%mini-make-closure (list 'lambda '() (cadr form)) env)))
               (restart-bind
                ;; (restart-bind ((name function [:report/:interactive/:test x]*) ...) body...)
                (%call-with-restart-bind
                 (mapcar (lambda (binding)
                           (cons (car binding) (%mini-eval (cadr binding) env)))
                         (cadr form))
                 (%mini-make-closure (list* 'lambda '() (cddr form)) env)))
               (multiple-value-list
                (multiple-value-list (%mini-eval (cadr form) env)))
               (%make-instance-with-initargs
                ;; Compiler intrinsic for (make-instance 'class ...): delegate to GF.
                (apply (symbol-function 'make-instance)
                       (%mini-eval-args (cdr form) env)))
               (t
                ;; %DOTIMES-1+ is a compiler intrinsic emitted by dotcl's DOTIMES
                ;; for a fixnum counter (an increment asserted to fit int64).
                ;; compile-expr lowers it to a raw add, but the interpreter never
                ;; sees a handler for it — so a macro body that uses DOTIMES and is
                ;; expanded through %MINI-EVAL (e.g. the DO-FPRS assembler macro)
                ;; would call %DOTIMES-1+ as an undefined function. The interpreter
                ;; has no int64 assertion, so it is simply 1+. Match by name: the
                ;; symbol may be DOTCL-INTERNAL:: or DOTCL.CIL-COMPILER:: depending
                ;; on how it was interned at emit time.
                (if (and (symbolp op) (string= (symbol-name op) "%DOTIMES-1+"))
                    (return-from %mini-eval (1+ (%mini-eval (cadr form) env)))
                    nil)
                ;; %MV-CAPTURE is MULTIPLE-VALUE-BIND's capture intrinsic. The
                ;; compiled form reads the thread values right after the call;
                ;; the interpreter cannot, because its own evaluation steps (a
                ;; symbol lookup is itself a two-value FIND-SYMBOL) sit in
                ;; between and overwrite them. So take the values as a list here.
                ;; Matched by name for the same reason as %DOTIMES-1+.
                (if (and (symbolp op) (string= (symbol-name op) "%MV-CAPTURE"))
                    (return-from %mini-eval
                      (%mv-capture-list
                       (multiple-value-list (%mini-eval (cadr form) env))))
                    nil)
                ;; %FIXNUM-GE-OBJECT is the other DOTIMES intrinsic: the loop test
                ;; for a counter in an Int64 slot against a boxed (possibly bignum)
                ;; limit. The interpreter has no unboxed counter, so it is >=.
                ;; Matched by name for the same reason as %DOTIMES-1+.
                (if (and (symbolp op) (string= (symbol-name op) "%FIXNUM-GE-OBJECT"))
                    (return-from %mini-eval
                      (>= (%mini-eval (cadr form) env) (%mini-eval (caddr form) env)))
                    nil)
                ;; %DOTNET-CALL-DIRECT is the one compiler intrinsic on this path
                ;; that cannot be given a function binding: its third argument is
                ;; a literal list of parameter type names, which ordinary argument
                ;; evaluation would try to call. It is a special form, so the
                ;; interpreter needs its own case.
                ;;   (%dotnet-call-direct "Type" "Method" (param-types...) recv arg...)
                ;; The type and parameter-type strings only select an overload for
                ;; the direct callvirt; dropping them and going through the dynamic
                ;; DOTNET:INVOKE path is the same call, which is exactly what the
                ;; assembler itself falls back to when it cannot resolve the
                ;; overload. Matched by name for the same reason as %DOTIMES-1+.
                (if (and (symbolp op) (string= (symbol-name op) "%DOTNET-CALL-DIRECT"))
                    (return-from %mini-eval
                      (apply (symbol-function (find-symbol "INVOKE" "DOTNET"))
                             (%mini-eval (nth 4 form) env)
                             (caddr form)
                             (%mini-eval-args (nthcdr 5 form) env)))
                    nil)
                ;; Function call: a local binding, a symbol's function, a
                ;; (setf name) function designator in operator position (e.g.
                ;; the ((setf foo) v place) form setf expands to), or a
                ;; ((lambda ...) ...) / computed-function operator.
                (let* ((fn (cond
                             ;; A lexical FLET/LABELS binding of OP was already
                             ;; taken at the top of this branch (LEX-FN), where it
                             ;; has to be looked up before macroexpansion to shadow
                             ;; a global macro of the same name (CLHS 3.1.2.1.2.4).
                             ;; Reaching here means there is none.
                             ;;
                             ;; %COERCE-TO-FUNCTION, not SYMBOL-FUNCTION: the
                             ;; compiled named-call path resolves through
                             ;; CilAssembler, which bridges a bare name across
                             ;; dotcl's own packages. SYMBOL-FUNCTION does not, so
                             ;; (class-precedence-list c) — registered on a
                             ;; DOTCL-MOP symbol — worked compiled and through
                             ;; FUNCALL but not as a plain form under the
                             ;; interpreter.
                             ((symbolp op) (%coerce-to-function op))
                             ((and (consp op) (eq (car op) 'setf))
                              (fdefinition op))
                             (t (%mini-eval op env))))
                       (args (%mini-eval-args (cdr form) env)))
                  (%mini-call fn args tailp env))))))))))))))
    (t form)))

(defun compile-macrolet (macro-defs body)
  "Compile (macrolet ((name (params) body...) ...) body...).
   Temporarily registers local macros in *macros*, compiles body, then restores."
  (let ((saved '()))
    ;; Save existing macro entries and register local macros
    (dolist (def macro-defs)
      (let* ((name (car def))
             (params (cadr def))
             (mbody (cddr def))
             (old-entry (gethash name *macros*)))
        (push (cons name old-entry) saved)
        ;; Build a macro expander lambda and eval it. &whole / &environment are
        ;; handled by the shared %macrolet-expander-form (same builder the
        ;; %mini-eval MACROLET case uses) so the compiled and interpreted paths
        ;; never diverge.
        (progn
          (progn
            (let* ((expander-form (%macrolet-expander-form params mbody))
                   ;; Wrap with surrounding flet so macrolet body can call
                   ;; enclosing locally-defined functions at expansion time.
                   ;; Use %mini-eval instead of eval: no Reflection.Emit needed.
                   (eval-form (if *compile-time-flet-defs*
                                  `(flet ,*compile-time-flet-defs* ,expander-form)
                                  expander-form))
                   (expander-fn (%mini-eval eval-form nil)))
              (setf (gethash name *macros*) expander-fn))))))
    ;; Compile body with local macros active
    ;; Handle (declare (special ...)) in body — remove those vars from *locals*
    ;; so they use dynamic binding (like locally does)
    (multiple-value-bind (declared-specials real-body) (extract-specials body)
    (let* ((*cstate* (if declared-specials
                         (cstate-with *cstate* +cs-locals+
                                      (remove-locals-shadowed-by declared-specials
                                                                 (cstate-locals)))
                         *cstate*)))
      (unwind-protect
          ;; Push this macrolet onto *macroexpand-scope* so a form shared (by a
          ;; splicing macro) between this shadowing scope and an outer scope is
          ;; cached separately per scope. MACRO-DEFS is the source
          ;; cons the analysis walk pushes too, keeping the two passes in sync.
          ;; *macrolet-shadowed* carries the same names to COMPILE-FORM's
          ;; dispatcher, which would otherwise reach the built-in handler for a
          ;; name like WHEN before ever looking for this binding.
          (let ((*macroexpand-scope* (cons macro-defs *macroexpand-scope*))
                (*macrolet-shadowed* (append (mapcar #'car macro-defs)
                                             *macrolet-shadowed*)))
            (compile-progn real-body))
        (dolist (entry saved)
          (if (cdr entry)
              (setf (gethash (car entry) *macros*) (cdr entry))
              (remhash (car entry) *macros*))))))))

(defun compile-symbol-macrolet (bindings body)
  "Compile (symbol-macrolet ((sym expansion)...) body...).
   Temporarily extends *symbol-macros* with new bindings."
  ;; Validate (constant / globally special name, or a body (declare (special n))
  ;; naming one of them). The predicate is shared with the %MINI-EVAL case so the
  ;; two evaluators cannot drift apart on what is a program error.
  (when (symbol-macrolet-violation-p bindings body)
    (return-from compile-symbol-macrolet
      (compile-expr `(error 'program-error))))
  (let* ((*symbol-macros* (append (mapcar (lambda (b) (cons (car b) (cadr b)))
                                          bindings)
                                  *symbol-macros*))
         ;; A symbol-macro shadows an enclosing lexical variable of the same name
         ;; inside the body (CLHS 5.1.2.1: inner binding wins). Drop such locals so
         ;; compile-var-ref falls through lookup-local to the symbol-macro instead of
         ;; resolving the outer variable. Without this, e.g. (with-accessors ((x acc))
         ;; x x) — where the accessor var name equals the instance-form variable —
         ;; read the instance itself, not (acc instance).
         (*cstate* (cstate-with *cstate* +cs-locals+
                                (remove-locals-shadowed-by (mapcar #'car bindings)
                                                           (cstate-locals))))
         ;; Push this symbol-macrolet onto *macroexpand-scope* (like compile-macrolet)
         ;; so a macro call form shared by a splicing macro between this scope and an
         ;; outer/sibling scope is cached separately per scope. Without this, a
         ;; recursive macro that nests symbol-macrolet and splices its body into both
         ;; if-arms (serapeum %with-boolean) reuses the outer-scope expansion for the
         ;; inner-scope form — the inner symbol-macro binding is lost. BINDINGS
         ;; is the source cons the analysis walk pushes too, keeping the passes in sync.
         (*macroexpand-scope* (cons bindings *macroexpand-scope*)))
    ;; Process free special declarations (like locally does)
    (multiple-value-bind (declared-specials real-body) (extract-specials body)
      (if (null declared-specials)
          (compile-progn real-body)
          (let* ((*specials* (append declared-specials *specials*))
                 (*cstate* (cstate-with *cstate* +cs-locals+
                                        (remove-locals-shadowed-by declared-specials
                                                                   (cstate-locals)))))
            (compile-progn real-body))))))

;;; --- FLET/LABELS capture lifting ---
;;;
;;; A local function that closes over anything builds a LispFunction every time
;;; control enters its FLET, and a LispFunction is 248 bytes. A local function
;;; that closes over NOTHING is compiled once as a constant and costs nothing. So
;;; when the captured variables can be passed as extra arguments instead, the
;;; local function drops into the constant path and the per-entry allocation goes
;;; to zero. The captured variable keeps its own name as the extra parameter, so
;;; the body needs no rewriting -- references to it resolve to the parameter.
;;;
;;; This is only valid while every call site can still reach the same BINDING the
;;; body would have closed over. That is checked at each call site by slot
;;; identity, not by name: if the variable no longer resolves to the slot it had
;;; where the FLET was written -- an inner binding shadows it, or the call sits
;;; inside a nested closure that captured a copy -- the whole lifted compile is
;;; thrown away and the form is recompiled the ordinary way.

(defun %lift-capture-tag (entry)
  "The abort tag of a lifted local-function ENTRY, or NIL. Each lifted FLET has
   its own tag: a call site must unwind to the FLET whose entry it consulted, not
   to whichever lifted FLET happens to be innermost."
  (fifth entry))

(defun %liftable-block-tag-p (name)
  "T when NAME is the synthetic variable of a NAMED block that is in scope here,
   so passing it as an argument gives the lifted function a working RETURN-FROM.

   BLOCK NIL is excluded. Every LOOP, DO and DOLIST establishes one, so several
   are in scope at once and the innermost is not the one the local function was
   written against -- the slot-identity check at the call site then aborts the
   lift from inside a block whose own compilation is already under way."
  (and (cstate-block-tags)
       (find-if (lambda (b)
                  (and (car b) (string= (block-tag-var-name (car b)) name)))
                (cstate-block-tags))
       t))

(defun %lift-captures (params body)
  "The lexical locals BODY closes over that can be passed as extra required
   parameters instead, as a list of (SYMBOL . SLOT-KEY), or :NONE when some
   capture cannot be.

   A capture is liftable only when reading it at a call site is the same as
   reading it in the closure would have been. Boxed variables are excluded
   because boxing means the variable is assigned somewhere, so its value at call
   time and at FLET-entry time can differ. Natively-typed slots (Int64, r8, r4,
   Decimal) are excluded because passing one as an argument would box it, moving
   the allocation rather than removing it. Synthetic slots (block tags, tagbody
   ids, labels cells) are excluded because they are machinery the closure
   protocol re-establishes from the env, not values the body reads."
  (let ((caps '()))
    (dolist (sym (find-free-vars-with-defaults params body) (nreverse caps))
      (let ((entry (local-entry sym)))
        (when entry
          (when (or (boxed-var-p sym)
                    ;; A synthetic slot -- a block tag, a tagbody id, a labels
                    ;; cell -- is not a value the body reads. COMPILE-LAMBDA
                    ;; rebuilds the block/go tables from the captured ENV, and a
                    ;; lifted function has no env, so a RETURN-FROM out of one
                    ;; would find no block at all.
                    (and (%synthetic-capture-name-p (var-name (car entry)))
                         (not (%liftable-block-tag-p (var-name (car entry)))))
                    (native-slot-p sym (cstate-long-locals))
                    (native-slot-p sym (cstate-native-double-locals))
                    (native-slot-p sym (cstate-native-single-locals))
                    (native-slot-p sym (cstate-native-decimal-locals)))
            (return-from %lift-captures :none))
          (pushnew (cons (car entry) (cdr entry)) caps :key #'cdr))))))

(defun %symbol-only-in-operator-position-p (name forms)
  "T when the symbol NAME occurs in FORMS only as the operator of a call.
   Anything else -- #'NAME, NAME as an argument, NAME inside a quoted form's
   neighbours -- means the function is used as a VALUE, which a lifted function
   cannot be: its arity no longer matches what it was written with."
  (labels ((walk (x)
             (cond ((eq x name) nil)
                   ((not (consp x)) t)
                   ((eq (car x) 'quote) t)
                   ;; A declaration names the function without using its value:
                   ;; (declare (inline f)) is a hint about calls to F, and
                   ;; treating it as a value reference would decline every local
                   ;; function anyone bothered to declare.
                   ((eq (car x) 'declare) t)
                   (t (let ((rest (if (eq (car x) name) (cdr x) x)))
                        (loop
                          (cond ((null rest) (return t))
                                ((not (consp rest)) (return (walk rest)))
                                ((not (walk (car rest))) (return nil))
                                (t (setq rest (cdr rest)))))))))) 
    (every #'walk forms)))

(defun %labels-self-free-p (name forms)
  "T when FORMS -- one LABELS definition.s own body -- never names itself.
   Such a definition is an FLET: nothing can see the binding the box exists for.

   (RETURN-FROM NAME ...) does not count. It names the implicit BLOCK, not the
   function, and FLET establishes that block too. Excluding it would decline
   every local function that returns early, which is most of them."
  (labels ((walk (x)
             (cond ((eq x name) nil)
                   ((not (consp x)) t)
                   ((and (eq (car x) 'return-from) (eq (cadr x) name))
                    (walk (cddr x)))
                   (t (and (walk (car x)) (walk (cdr x)))))))
    (every #'walk forms)))

(defun %lift-plan (fdef body)
  "The lift plan for one FLET definition FDEF as (PARAMS CAPS), or NIL.
   BODY is the FLET body, which decides whether the function is ever used as a
   value. Only symbol-named, required-only functions qualify, and the lifted
   arity has to stay inside the direct-call range."
  (let ((name (car fdef))
        (params (cadr fdef))
        (fn-body (cddr fdef)))
    (and (symbolp name)
         (labels-required-only-params-p params)
         (%symbol-only-in-operator-position-p name body)
         (let ((caps (%lift-captures params fn-body)))
           (and (consp caps)
                ;; 8 is where the direct-call entries stop. It used to have to be
                ;; 6: INVOKE7 and INVOKE8 built the debugger's frame array on
                ;; every call, which turned lifting into a per-ENTRY saving paid
                ;; for by a per-CALL cost, and a local function is normally called
                ;; more than once per entry. Those two entries now skip the array
                ;; for an anonymous callee, exactly as INVOKE5/INVOKE6 do, so the
                ;; cost is gone and the trade needs no counting again. Arity 7,
                ;; eight calls per entry: 2528 B unlifted, 2848 lifted before that
                ;; fix, 2208 after.
                (<= (+ (length params) (length caps)) 8)
                (notany (lambda (c) (member (car c) params :test #'eq)) caps)
                (list params caps))))))

(defun compile-flet (fn-defs body)
  "Compile (flet ((name (params) body...) ...) body...).
   Tries the capture-lifting path first (see %LIFT-CAPTURES); a call site that
   cannot reach the original binding throws back here and the ordinary closure
   path runs instead."
  (let ((plans (mapcar (lambda (f) (%lift-plan f body)) fn-defs)))
    (if (notany #'identity plans)
        (%compile-flet-1 fn-defs body nil)
        (let* ((tag (list '#:flet-lift))
               (result (catch tag (%compile-flet-1 fn-defs body (cons tag plans)))))
          (if (eq result :lift-aborted)
              (%compile-flet-1 fn-defs body nil)
              result)))))

(defun %compile-flet-1 (fn-defs body lift)
  "One compile of an FLET. LIFT is NIL for the ordinary closure path, or
   (TAG . PLANS) with one plan per definition (NIL where that definition is not
   being lifted)."
  (let ((fn-instrs '())
        (new-local-fns '())
        (new-locals '())
        (tag (car lift))
        (plans (cdr lift)))
    ;; Compile each function definition in OUTER scope (flet functions can't see each other)
    (dolist (fdef fn-defs)
      (let* ((name (car fdef))
             (plan (pop plans))
             (caps (second plan))
             ;; The captured variables become extra required parameters, keeping
             ;; their own names so the body reads them without being rewritten.
             (params (if plan (append (cadr fdef) (mapcar #'car caps)) (cadr fdef)))
             (fn-body (cddr fdef))
             (name-str (mangle-name name))
             (key (gen-local name-str)))
        ;; Compile the lambda (in current scope, not extended)
        ;; CL spec: flet creates an implicit block named after the function
        ;; For (setf sym) names, use progn instead of block (block requires a symbol)
        (let ((lambda-instrs
                (let ((*lift-block-tags* (if plan (cstate-block-tags) nil)))
                  (if (and (symbolp name)
                           (some (lambda (f) (form-has-return-from-p name f)) fn-body))
                      (compile-lambda params `((block ,name ,@fn-body)))
                      (compile-lambda params fn-body)))))
          (setf fn-instrs
                (append fn-instrs
                        `((:declare-local ,key "LispObject")
                          ,@lambda-instrs
                          (:stloc ,key))))
          (push (list name-str key nil (and plan caps) (and plan tag)) new-local-fns)
          ;; Track in *locals* so closures can capture flet functions.
          ;; Use BOTH the plain name and the __LABELFN_ prefix (same key):
          ;; - Plain name: backward compat (#'flet-fn value capture)
          ;; - __LABELFN_ prefix: capture in function call position without
          ;;   conflicting with a same-named let/let* variable (Lisp-2 separation).
          ;;   The free var analysis detects __LABELFN_ entries in function position
          ;;   (see find-free-vars-expr), and compile-closure-body prefers it.
          (when (symbolp name)
            (push (cons (intern (symbol-name name) :dotcl.cil-compiler) key) new-locals)
            (push (cons (intern (concatenate 'string "__LABELFN_" (symbol-name name))
                                :dotcl.cil-compiler)
                        key)
                  new-locals)))))
    ;; Compile body with extended local-functions AND locals.
    ;; Also track flet source defs so compile-defmacro can wrap its eval
    ;; with flet bindings (e.g. SBCL macros.lisp wraps defmacro in flet).
    (let ((*cstate* (cstate-with *cstate*
                      +cs-local-functions+ (append (nreverse new-local-fns)
                                                   (cstate-local-functions))
                      +cs-locals+ (append (nreverse new-locals) (cstate-locals))))
          (*compile-time-flet-defs* (append fn-defs *compile-time-flet-defs*)))
      `(,@fn-instrs
        ,@(compile-progn body)))))

(defun labels-required-only-params-p (params)
  "Return T if PARAMS is a required-only lambda list with no &optional/&key/&rest/&aux."
  (not (some (lambda (p) (member p '(&optional &key &rest &aux &allow-other-keys)))
             params)))

;;; --- Speculative labels-self-TCO acceptance scan ---
;;; compile-labels-boxed speculatively compiles a single self-recursive labels
;;; function through the direct+TCO path (see *labels-direct-speculation*). The
;;; generated instruction tree is accepted ONLY when both scans pass, so the
;;; failure mode is always "fall back to the closure path", never a miscompile.

(defun %instr-tree-contains-symbol-p (tree sym)
  "T if the interned symbol SYM (a labels box-key from gen-local) appears
   anywhere in the instruction TREE. Any occurrence means the box was actually
   referenced (as a value / non-tail call / #'g), so the direct compile is not
   self-contained — scan (a)."
  (cond ((eq tree sym) t)
        ((consp tree)
         (or (%instr-tree-contains-symbol-p (car tree) sym)
             (%instr-tree-contains-symbol-p (cdr tree) sym)))
        (t nil)))

(defun %tco-loop-target (instr)
  "If INSTR is a (:br L) / (:leave L) to a TCO loop label (TCOLOOP* or LMTCO*),
   return L, else NIL."
  (and (consp instr)
       (member (car instr) '(:br :leave))
       (let ((tgt (cadr instr)))
         (and (stringp tgt)
              (or (and (>= (length tgt) 7) (string= tgt "TCOLOOP" :end1 7))
                  (and (>= (length tgt) 5) (string= tgt "LMTCO" :end1 5)))
              tgt))))

(defun %instr-list-tco-orphan-free-p (instrs)
  "Scan ONE instruction list: after a TCO-loop branch, until the next
   (:label ...), only :br / :leave / :ret may appear. Anything else is dead code
   stranded by a TCO rewrite (e.g. an intrinsic call whose result was discarded
   because its argument tail-recursed) — reject. Scan (b)."
  (let ((in-dead nil))
    (dolist (instr instrs t)
      (cond
        ((and (consp instr) (eq (car instr) :label))
         (setq in-dead nil))
        (in-dead
         (unless (and (consp instr) (member (car instr) '(:br :leave :ret)))
           (return nil)))
        ((%tco-loop-target instr)
         (setq in-dead t))))))

(defun %instr-nested-body (instr)
  "The nested :body instruction list of a make-function/-direct/closure op, or
   NIL. Restricted to those ops so GETF never touches a non-plist instruction
   like (:ldc-i4 0)."
  (when (and (consp instr)
             (member (car instr)
                     '(:make-function :make-function-direct :make-closure)))
    (getf (cdr instr) :body)))

(defun %instr-tree-tco-orphan-free-p (instrs)
  "Apply %instr-list-tco-orphan-free-p to INSTRS and recursively to every nested
   :body instruction list (each nested make-function/-direct/closure is its own
   method)."
  (and (%instr-list-tco-orphan-free-p instrs)
       (every (lambda (instr)
                (let ((body (%instr-nested-body instr)))
                  (or (not (consp body))
                      (%instr-tree-tco-orphan-free-p body))))
              instrs)))

(defun labels-direct-speculation-acceptable-p (instrs box-key)
  "T iff INSTRS is a single :make-function-direct whose body is provably safe for
   labels self-TCO: it took the direct path, references BOX-KEY nowhere (scan a),
   and strands no orphan code after a TCO branch (scan b)."
  (and (consp instrs)
       (null (cdr instrs))
       (consp (car instrs))
       (eq (caar instrs) :make-function-direct)
       (not (%instr-tree-contains-symbol-p instrs box-key))
       (%instr-tree-tco-orphan-free-p instrs)))

(defun compile-labels (fn-defs body)
  "Compile (labels ((name (params) body...) ...) body...).
   When all functions have the same required-only arity (>= 2 fns), uses a dispatch
   loop for mutual tail call optimization. Otherwise uses boxed closures."
  (let* ((n-fns (length fn-defs))
         (first-arity (and fn-defs (length (cadr (first fn-defs))))))
    (cond
      ;; One function that never names itself is an FLET: nothing can see the
      ;; binding the box exists for. Routing it there is what lets capture
      ;; lifting reach it, and the box it drops is an allocation of its own.
      ((and (= n-fns 1)
            (symbolp (car (first fn-defs)))
            (%labels-self-free-p (car (first fn-defs)) (cddr (first fn-defs))))
       (compile-flet fn-defs body))
      ((and (>= n-fns 2)
            (every (lambda (f) (labels-required-only-params-p (cadr f))) fn-defs)
            (every (lambda (f) (= (length (cadr f)) first-arity)) fn-defs))
       (compile-labels-mutual-tco fn-defs body first-arity))
      (t (compile-labels-boxed fn-defs body)))))

(defun compile-labels-boxed (fn-defs body)
  "Compile (labels ...) using boxed closures (no mutual-TCO optimization)."
  (let ((box-instrs '())
        (new-local-fns '())
        (fn-compile-list '()))
    ;; Phase 1: Allocate boxes for all function names
    (dolist (fdef fn-defs)
      (let* ((name (car fdef))
             (name-str (mangle-name name))
             (key (gen-local name-str)))
        (setf box-instrs
              (append box-instrs
                      `((:declare-local ,key "LispObject[]")
                        (:ldc-i4 1) (:newarr "LispObject")
                        (:stloc ,key))))
        (push (list name-str key t) new-local-fns)
        (push (list name (cadr fdef) (cddr fdef) key) fn-compile-list)))
    (setf new-local-fns (nreverse new-local-fns))
    (setf fn-compile-list (nreverse fn-compile-list))
    ;; Phase 2: Compile each function body in scope of ALL labels names
    (let* (;; Also make boxes available as locals for capture (only symbol names).
           ;; CL is a Lisp-2: function and variable namespaces are separate.
           ;; Always use __LABELFN_ prefix so labels boxes and let variables with
           ;; the same name can coexist in *locals*. find-free-vars-expr detects the
           ;; __LABELFN_ entry when a labels function appears in function position.
           (new-locals (compile-labels-build-new-locals new-local-fns))
           (*cstate* (cstate-with *cstate*
                       +cs-local-functions+ (append new-local-fns
                                                    (cstate-local-functions))
                       +cs-locals+ (append new-locals (cstate-locals))
                       +cs-boxed-vars+ (append (mapcar #'car new-locals)
                                               (cstate-boxed-vars))))
           (store-instrs '()))
      ;; Compile each function and store into its box.
      ;; CL spec: labels creates an implicit block named after the function.
      ;; For (setf sym) names, use progn instead of block (block requires a symbol).
      ;; Pass name-str + set *tco-local-fn-key* so compile-lambda enables self-TCO.
      (dolist (entry fn-compile-list)
        (let ((name (first entry))
              (params (second entry))
              (fn-body (third entry))
              (key (fourth entry)))
          (let* ((name-str (mangle-name name))
                 ;; Rebind *tco-self-symbol* to THIS labels function's symbol:
                 ;; the self-call identity check must match g, not a stale
                 ;; enclosing defun (whose symbol would make a tail call to
                 ;; that outer defun falsely TCO-branch to g's loop).
                 (compile-thunk
                   (lambda ()
                     (let ((*cstate* (cstate-with *cstate*
                                       +cs-tco-local-fn-key+ key
                                       +cs-tco-self-symbol+ (if (symbolp name) name nil))))
                       (if (and (symbolp name)
                                (some (lambda (f) (form-has-return-from-p name f)) fn-body))
                           (compile-lambda params `((block ,name ,@fn-body)) name-str)
                           (compile-lambda params fn-body name-str)))))
                 ;; Speculative direct+TCO: try compiling this labels fn
                 ;; through the direct path so a single self-tail-recursion is
                 ;; TCO'd instead of overflowing. Accept only when the generated
                 ;; body is provably self-contained; otherwise recompile via the
                 ;; unchanged closure path. Symbol-named fns only (the box key is
                 ;; matched by name in compile-lambda / -function-body-direct).
                 (lambda-instrs
                   (if (symbolp name)
                       (let ((spec-instrs
                               (let ((*cstate* (cstate-with *cstate*
                                                 +cs-labels-direct-speculation+
                                                 (cons name-str key))))
                                 (funcall compile-thunk))))
                         (if (labels-direct-speculation-acceptable-p spec-instrs key)
                             spec-instrs
                             (funcall compile-thunk)))
                       (funcall compile-thunk))))
            (setf store-instrs
                  (append store-instrs
                          `((:ldloc ,key) (:ldc-i4 0)
                            ,@lambda-instrs
                            (:stelem-ref)))))))
      ;; Compile body
      `(,@box-instrs
        ,@store-instrs
        ,@(compile-progn body)))))

(defun compile-labels-build-new-locals (new-local-fns)
  "Shared helper: build the *locals* additions for a labels group (box-as-variable bindings)."
  (remove nil
    (mapcar (lambda (lf)
              (let* ((name-str (first lf)))
                (unless (char= (char name-str 0) #\()
                  (let* ((local-sym (intern (concatenate 'string "__LABELFN_" name-str)
                                            :dotcl.cil-compiler)))
                    (cons local-sym (second lf))))))
            new-local-fns)))

(defun compile-labels-mutual-tco (fn-defs body arity)
  "Compile (labels ...) where all functions have the same required ARITY.
   Emits closures in boxes for non-tail calls, plus a dispatch loop for
   mutual tail call optimization.
   Dispatch loop structure:
     TCOLOOP: dispatch by WHICH_FN index → fn body sections
     Each fn body section: tail calls to siblings → br TCOLOOP; normal return → br TCOEND.
   The labels body is compiled with *labels-mutual-tco* active so its own
   tail calls to labels fns also enter the dispatch loop directly."
  ;; Phase 1: allocate boxes (for non-tail calls), same as compile-labels-boxed
  (let ((box-instrs '())
        (new-local-fns '())
        (fn-compile-list '()))
    (dolist (fdef fn-defs)
      (let* ((name (car fdef))
             (name-str (mangle-name name))
             (key (gen-local name-str)))
        (setf box-instrs
              (append box-instrs
                      `((:declare-local ,key "LispObject[]")
                        (:ldc-i4 1) (:newarr "LispObject")
                        (:stloc ,key))))
        (push (list name-str key t) new-local-fns)
        (push (list name (cadr fdef) (cddr fdef) key) fn-compile-list)))
    (setf new-local-fns (nreverse new-local-fns))
    (setf fn-compile-list (nreverse fn-compile-list))
    ;; Phase 2: compile closures into boxes; *labels-mutual-tco* is NIL here
    ;; (compile-function-body-* resets it) so closures don't emit br-to-outer-TCOLOOP
    (let* ((new-locals (compile-labels-build-new-locals new-local-fns))
           (*cstate* (cstate-with *cstate*
                       +cs-local-functions+ (append new-local-fns
                                                    (cstate-local-functions))
                       +cs-locals+ (append new-locals (cstate-locals))
                       +cs-boxed-vars+ (append (mapcar #'car new-locals)
                                               (cstate-boxed-vars))))
           (store-instrs '()))
      (dolist (entry fn-compile-list)
        (let ((name (first entry))
              (params (second entry))
              (fn-body (third entry))
              (key (fourth entry)))
          (let* ((name-str (mangle-name name))
                 ;; Rebind *tco-self-symbol* per labels function (see
                 ;; compile-labels-boxed) — a stale enclosing defun symbol
                 ;; falsely matches tail calls to that outer defun.
                 (lambda-instrs (let ((*cstate* (cstate-with *cstate*
                                                  +cs-tco-local-fn-key+ key
                                                  +cs-tco-self-symbol+
                                                  (if (symbolp name) name nil))))
                                  (if (and (symbolp name)
                                           (some (lambda (f) (form-has-return-from-p name f)) fn-body))
                                      (compile-lambda params `((block ,name ,@fn-body)) name-str)
                                      (compile-lambda params fn-body name-str)))))
            (setf store-instrs
                  (append store-instrs
                          `((:ldloc ,key) (:ldc-i4 0)
                            ,@lambda-instrs
                            (:stelem-ref)))))))
      ;; Phase 3: dispatch loop infrastructure
      ;; shared-param-keys: one LispObject local per param position, shared across all fn bodies
      ;; which-fn-key: System.Int32 local that selects which fn to dispatch to
      (let* ((shared-param-keys
               (loop for i from 0 below arity
                     collect (gen-local (format nil "LMARG~D" i))))
             (which-fn-key (gen-local "LMWFN"))
             (tcoloop-label (gen-label "LMTCO"))
             (tcoend-label (gen-label "LMEND"))
             ;; mtco-table: (name-str fn-index which-fn-key tcoloop-label shared-param-keys)
             (mtco-table
               (loop for fdef in fn-defs
                     for i from 0
                     collect (list (mangle-name (car fdef))
                                   i
                                   which-fn-key
                                   tcoloop-label
                                   shared-param-keys)))
             ;; Int32 type string recognized by CilAssembler
             (int32-type "Int32")
             ;; Per-fn CIL labels for dispatch targets
             (fn-labels
               (loop for i from 0 below (length fn-defs)
                     collect (gen-label (format nil "LMF~D" i)))))
        ;; Phase 4: compile labels body with *labels-mutual-tco* active
        ;; Tail calls to labels fns emit: store args, set which-fn, br tcoloop-label
        (let* ((*cstate* (cstate-with *cstate* +cs-labels-mutual-tco+
                                      (append mtco-table (cstate-labels-mutual-tco))))
               (body-instrs (compile-progn body)))
          ;; Phase 5: dispatch instructions (beq for fns 0..N-2; fn N-1 falls through)
          (let ((dispatch-instrs
                  (loop for lbl in (butlast fn-labels)
                        for i from 0
                        append `((:ldloc ,which-fn-key)
                                 (:ldc-i4 ,i)
                                 (:beq ,lbl)))))
            ;; Phase 6: compile fn body sections inline (no compile-lambda wrapper)
            ;; Each section: fn's params bound to shared-param-keys, *labels-mutual-tco* active
            ;; Sections emitted in order: fn[N-1] (fall-through target), fn[0], ..., fn[N-2]
            (let* ((fn-body-sections
                     (loop for fdef in fn-defs
                           for lbl in fn-labels
                           collect
                           (let* ((params (cadr fdef))
                                  (fn-body (cddr fdef))
                                  ;; The section binds each param to a plain shared
                                  ;; LispObject local. An outer variable of the SAME
                                  ;; name may be boxed or numerically typed; those
                                  ;; per-name context lists must be shadowed here or
                                  ;; a param reference compiles as a box-deref /
                                  ;; native unbox of a raw value.
                                  (param-names (mapcar #'var-name params))
                                  (shadowed-p (lambda (x)
                                                (member (if (symbolp x) (var-name x) x)
                                                        param-names :test #'string=)))
                                  (shadowed-key-p (lambda (e)
                                                    (member (if (consp e) (car e) e)
                                                            param-names :test #'string=)))
                                  (fn-instrs
                                    (let ((*fixnum-locals* (remove-if shadowed-key-p *fixnum-locals*))
                                          ;; *small-int-locals* is slot-keyed —
                                          ;; a param shadowing the name owns a
                                          ;; different slot (SMALL-INT-LOCAL-RANGE).

                                          (*double-float-locals* (remove-if shadowed-key-p *double-float-locals*))
                                          (*single-float-locals* (remove-if shadowed-key-p *single-float-locals*))
                                          (*decimal-locals* (remove-if shadowed-key-p *decimal-locals*))
                                          ;; *long-locals* is slot-keyed, so a
                                          ;; param shadowing the name is not a
                                          ;; match to begin with (NATIVE-SLOT-P).

                                          ;; One pack update: params into locals,
                                          ;; boxed/numeric-array shadowing, reset of
                                          ;; outer self-TCO (dispatch bodies have
                                          ;; their own context), and the active
                                          ;; mutual-TCO table.
                                          (*cstate* (cstate-with *cstate*
                                                      +cs-locals+
                                                      (append (loop for p in params
                                                                    for key in shared-param-keys
                                                                    collect (cons p key))
                                                              (cstate-locals))
                                                      +cs-boxed-vars+
                                                      (remove-if shadowed-p (cstate-boxed-vars))
                                                      +cs-numeric-array-locals+
                                                      (remove-if shadowed-key-p (cstate-numeric-array-locals))
                                                      +cs-tco-self-name+ nil
                                                      +cs-tco-self-symbol+ nil
                                                      +cs-tco-loop-label+ nil
                                                      +cs-tco-param-entries+ nil
                                                      +cs-self-fn-local+ nil
                                                      +cs-tco-local-fn-key+ nil
                                                      +cs-labels-mutual-tco+
                                                      (append mtco-table (cstate-labels-mutual-tco))))
                                          ;; Dispatch body is always in tail position;
                                          ;; its result IS the result of the labels form
                                          (*in-tail-position* t))
                                      (let ((name (car fdef)))
                                        (if (and (symbolp name)
                                                 (some (lambda (f) (form-has-return-from-p name f))
                                                       fn-body))
                                            (compile-progn `((block ,name ,@fn-body)))
                                            (compile-progn fn-body))))))
                             (cons lbl fn-instrs))))
                   ;; Emit order: fn[N-1] first (fall-through from dispatch), then fn[0]..fn[N-2]
                   (sections-in-emit-order
                     (append (last fn-body-sections) (butlast fn-body-sections)))
                   ;; All section code: label + body + br to TCOEND
                   (all-section-code
                     (loop for (lbl . body-code) in sections-in-emit-order
                           append `((:label ,lbl)
                                    ,@body-code
                                    (:br ,tcoend-label)))))
              ;; Assemble complete instruction list
              `(,@(loop for key in shared-param-keys
                        collect `(:declare-local ,key "LispObject"))
                (:declare-local ,which-fn-key ,int32-type)
                ,@box-instrs
                ,@store-instrs
                ,@body-instrs
                (:br ,tcoend-label)
                (:label ,tcoloop-label)
                ,@dispatch-instrs
                ,@all-section-code
                (:label ,tcoend-label)))))))))

;;; ============================================================
;;; block / return-from
;;; ============================================================

(defun compile-block (name body)
  "Compile (block name body...).
   If no non-local return-from is used, skip the try/catch overhead entirely."
  (let* ((tag-key (gen-local "BTAG"))
         (tag-var-sym (intern (block-tag-var-name name) :dotcl.cil-compiler))
         (result-key (gen-local "BRES"))
         (end-label (gen-label "BEND"))
         (ex-key (gen-local "BEX"))
         (needs-catch (list nil))
         ;; Entry format: (tag-key result-key end-label local-result-key local-end-label needs-catch)
         (*cstate* (cstate-with *cstate*
                     +cs-block-tags+
                     (acons name (list tag-key result-key end-label result-key end-label needs-catch)
                            (cstate-block-tags))
                     +cs-locals+ (acons tag-var-sym tag-key (cstate-locals))))
         (body-instrs (let ((*in-tail-position* nil)
                            (*in-mv-context* t))
                        (compile-progn body))))
    (if (car needs-catch)
        ;; Non-local return detected: need try/catch
        `((:declare-local ,tag-key "LispObject")
          (:ldsfld "Nil.Instance") (:ldsfld "Nil.Instance") (:call "Runtime.MakeCons")
          (:stloc ,tag-key)
          (:declare-local ,result-key "LispObject")
          (:begin-exception-block)
          ,@body-instrs
          (:stloc ,result-key)
          (:leave ,end-label)
          ;; Filter: exception on the stack, answer 1 (mine) / 0 (keep unwinding).
          ;; A catch that rethrew what it did not own left a live handler funclet at
          ;; every block a non-local return crossed, so crossing N of them cost N
          ;; stacked dispatches (see compile-catch).
          (:begin-filter-block)
          (:ldloc ,tag-key)
          (:call "ControlFlowFilters.BlockTagMatches")
          (:begin-filter-handler)
          (:castclass "BlockReturnException")
          (:declare-local ,ex-key "BlockReturnException")
          (:stloc ,ex-key)
          (:ldloc ,ex-key) (:callvirt "BlockReturnException.get_Value")
          (:stloc ,result-key)
          (:leave ,end-label)
          (:end-exception-block)
          (:label ,end-label)
          (:ldloc ,result-key))
        ;; All returns are local: no try/catch needed
        `((:declare-local ,result-key "LispObject")
          ,@body-instrs
          (:stloc ,result-key)
          (:label ,end-label)
          (:ldloc ,result-key)))))

(defun compile-return-from (name value-expr)
  "Compile (return-from name value).
   Local return (same compilation unit, not in finally block) uses leave.
   Non-local return (from closure or finally block) throws BlockReturnException."
  (let ((entry (assoc name (cstate-block-tags))))
    (unless entry (error "return-from: no block named ~s" name))
    (let ((tag-key (first (cdr entry)))
          (local-result-key (fourth (cdr entry)))
          (local-end-label (fifth (cdr entry))))
      (let ((needs-catch (sixth (cdr entry))))
        ;; value-expr's multiple values become the block's return values;
        ;; preserve MvReturn wrapping so the block's caller sees MV.
        (if (and local-result-key (not *in-finally-block*))
            ;; Local return: store result and leave to block end
            `(,@(if value-expr
                    (let ((*in-mv-context* t)) (compile-expr value-expr))
                    (emit-nil))
              (:stloc ,local-result-key)
              (:leave ,local-end-label))
            ;; Non-local return: throw BlockReturnException
            (progn
              (when needs-catch (setf (car needs-catch) t))
              (let ((val-key (gen-local "RVAL")))
                `((:declare-local ,val-key "LispObject")
                  ,@(if value-expr
                        (let ((*in-mv-context* t)) (compile-expr value-expr))
                        (emit-nil))
                  (:stloc ,val-key)
                  (:ldloc ,tag-key)
                  (:ldloc ,val-key)
                  (:newobj "BlockReturnException")
                  (:throw)))))))))

;;; ============================================================
;;; catch / throw
;;; ============================================================

(defun compile-catch (tag-expr body)
  "Compile (catch tag body...).
   Uses try/filter/finally for CatchThrowException with EQ tag matching.
   Pushes tag to CatchTagStack so (throw ...) inside (eval ...) can propagate
   correctly to an outer (catch ...) even across the eval boundary.

   The tag test is a CIL exception FILTER, not a catch that rethrows what it does
   not own. A rethrow restarts dispatch from inside the handler funclet, and that
   funclet stays live for the rest of the throw's journey, so N nested catches
   cost N stacked dispatches' worth of stack. The tree-walk evaluator wraps every
   interpreted call in a BLOCK, which is one CATCH per call, so a THROW out of
   deep interpreted recursion needed stack proportional to the depth it crossed
   and blew the .NET stack fatally. A filter answers \"not mine\" without touching
   the frame."
  (let ((tag-key (gen-local "CTAG"))
        (result-key (gen-local "CRES"))
        (end-label (gen-label "CEND"))
        (ex-key (gen-local "CEX")))
    `((:declare-local ,tag-key "LispObject")
      ,@(compile-expr tag-expr)
      (:stloc ,tag-key)
      ;; Register tag so (throw ...) inside (eval ...) can propagate to this catch
      (:ldloc ,tag-key)
      (:call "CatchTagStack.Push")
      (:declare-local ,result-key "LispObject")
      (:begin-exception-block)
      ;; Body in MV-propagating position: catch returns body's values (CL spec).
      ,@(let ((*in-tail-position* nil) (*in-mv-context* t)) (compile-progn body))
      (:stloc ,result-key)
      (:leave ,end-label)
      ;; Filter: exception on the stack, answer 1 (mine) / 0 (keep unwinding).
      (:begin-filter-block)
      (:ldloc ,tag-key)
      (:call "ControlFlowFilters.CatchTagMatches")
      (:begin-filter-handler)
      (:castclass "CatchThrowException")
      (:declare-local ,ex-key "CatchThrowException")
      (:stloc ,ex-key)
      (:ldloc ,ex-key) (:callvirt "CatchThrowException.get_Value")
      (:stloc ,result-key)
      (:leave ,end-label)
      (:begin-finally-block)
      (:call "CatchTagStack.Pop")
      (:end-exception-block)
      (:label ,end-label)
      (:ldloc ,result-key))))

(defun compile-throw (tag-expr result-expr)
  "Compile (throw tag result).
   Stores tag and result in locals before constructing the exception,
   so that non-local exits within result-expr don't leave tag on stack.
   Unmatched throws are caught at the Eval boundary and converted to CONTROL-ERROR."
  (let ((tag-key (gen-local "TTAG"))
        (res-key (gen-local "TRES")))
    `((:declare-local ,tag-key "LispObject")
      (:declare-local ,res-key "LispObject")
      ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
          (compile-expr tag-expr))
      (:stloc ,tag-key)
      ;; result-expr's multiple values become the catch's return values
      ;; regardless of where the throw form sits (CLHS 5.2); preserve
      ;; MvReturn wrapping like return-from does. This must not depend on
      ;; the ambient MV context of the position the throw appears in.
      ,@(let ((*in-tail-position* nil) (*in-mv-context* t))
          (compile-expr result-expr))
      (:stloc ,res-key)
      (:ldloc ,tag-key) (:ldloc ,res-key)
      (:newobj "CatchThrowException")
      (:throw))))

;;; ============================================================
;;; tagbody / go
;;; ============================================================

(defun parse-tagbody-forms (forms)
  "Parse tagbody forms into segments: ((label . forms-list) ...).
   The first segment has label nil."
  (let ((segments '())
        (current-label nil)
        (current-forms '()))
    (dolist (form forms)
      (if (or (and (symbolp form) (not (null form))) (integerp form))
          (progn
            (push (cons current-label (reverse current-forms)) segments)
            (setf current-label form)
            (setf current-forms '()))
          (push form current-forms)))
    (push (cons current-label (reverse current-forms)) segments)
    (reverse segments)))

(defun sil-references-symbol-p (instrs sym)
  "Deep scan: does SYM appear anywhere in the SIL tree INSTRS (including nested
   :body lists for hoisted closures)? Used by compile-tagbody to decide whether
   the tagbody id local is referenced — by a non-local go's GoException throw
   OR by a closure capturing the id into its env — in which case the GoException
   try/catch must be kept. A structural check, immune to compile ordering."
  (labels ((walk (x)
             (cond ((eq x sym) t)
                   ((consp x) (or (walk (car x)) (walk (cdr x))))
                   (t nil))))
    (walk instrs)))

(defun compile-tagbody (forms)
  "Compile (tagbody forms...).
   Local go (same tagbody, not in closure) uses index + leave instead of exceptions.
   Non-local go (from closures) still uses GoException throw/catch."
  (let* ((segments (parse-tagbody-forms forms))
         (tb-id-key (gen-local "TBID"))
         (index-key (gen-local "TBIDX"))
         (done-key (gen-local "TBDONE"))
         (end-label (gen-label "TBEND"))
         (loop-label (gen-label "TBLOOP"))
         (leave-label (gen-label "TBLEAVE"))
         (ex-key (gen-local "TBEX"))
         ;; Build tag→index map (tags start from index 0)
         (tag-counter 0)
         (tag-indices
           (let ((result '()))
             (dolist (seg segments)
               (when (car seg)
                 (push (cons (car seg) tag-counter) result))
               (incf tag-counter))
             (reverse result)))
         ;; Segment labels
         (seg-labels (loop for i from 0 below (length segments)
                           collect (gen-label (format nil "SEG~d" i))))
         ;; Synthetic variable for the tagbody ID (for non-local go from closures)
         (tb-var-name (concatenate 'string "%TBID-" (symbol-name tb-id-key) "%"))
         (tb-var-sym (intern tb-var-name :dotcl.cil-compiler))
         (*cstate* (cstate-with *cstate* +cs-locals+
                                (acons tb-var-sym tb-id-key (cstate-locals))))
         ;; needs-catch: shared cell flagged by compile-go when it emits a
         ;; NON-LOCAL (GoException throw) go targeting this tagbody. If none is
         ;; emitted while compiling the segments, the catch is dead and omitted
         ;; (mirrors compile-block). Eliding the per-iteration try is a large
         ;; win for hot loops (dotimes/do/loop → tagbody).
         (needs-catch (list nil))
         ;; has-go: shared cell flagged by compile-go when ANY go targets this
         ;; tagbody. Every go re-enters through the dispatch label below, so a
         ;; flagged tagbody is a (potential) loop and gets an interrupt
         ;; safepoint there; a straight-line tagbody (no go at all) does not.
         (has-go (list nil))
         ;; Extended format: (tag-name tb-var-name tb-id-key label-idx index-key
         ;;                    leave-label needs-catch has-go). 5th/6th enable
         ;; local go; 7th lets a captured non-local go flag this tagbody's
         ;; needs-catch; 8th records that a go exists at all.
         (*cstate* (cstate-with *cstate* +cs-go-tags+
                     (append
                      (mapcar (lambda (ti) (list (car ti) tb-var-name tb-id-key (cdr ti)
                                                 index-key loop-label needs-catch has-go))
                              tag-indices)
                      (cstate-go-tags))))
         ;; Compile segments NOW so compile-go runs (and may flag needs-catch)
         ;; before we decide whether the GoException try/catch is needed.
         (seg-instrs (let ((*in-tail-position* nil) (*in-mv-context* nil))
                       (loop for seg in segments
                             for label in seg-labels
                             append `((:label ,label)
                                      ,@(loop for form in (cdr seg)
                                              append (compile-and-pop form))))))
         ;; Interrupt safepoint on the dispatch label = the shared back-edge of
         ;; every loop this tagbody expresses. Only when a go exists (otherwise
         ;; the tagbody runs straight through once) and the body is not
         ;; declared (optimize (safety 0)).
         (poll-instrs (when (and (car has-go) (not (cstate-no-safepoint)))
                        '((:call "ConditionSystem.PollInterrupt")))))
    (if (or (car needs-catch)
            ;; tb-id-key referenced in the body ⇒ a non-local go's throw or a
            ;; closure capturing the tagbody id ⇒ the catch is required. This
            ;; structural check backs up the needs-catch side-effect flag.
            (sil-references-symbol-p seg-instrs tb-id-key))
        ;; A non-local go can land here: keep the GoException try/catch.
        `((:declare-local ,tb-id-key "LispObject")
          (:ldsfld "Nil.Instance") (:ldsfld "Nil.Instance") (:call "Runtime.MakeCons")
          (:stloc ,tb-id-key)
          (:declare-local ,index-key "Int32")
          (:ldc-i4 0) (:stloc ,index-key)
          (:declare-local ,done-key "Boolean")
          (:ldc-i4 0) (:stloc ,done-key)
          (:label ,loop-label)
          ,@poll-instrs
          (:ldloc ,done-key) (:brtrue ,end-label)
          (:begin-exception-block)
          (:ldloc ,index-key)
          (:switch ,seg-labels)
          (:br ,leave-label)
          ,@seg-instrs
          (:label ,leave-label)
          (:ldc-i4 1) (:stloc ,done-key)
          (:leave ,loop-label)
          ;; Filter: exception on the stack, answer 1 (mine) / 0 (keep unwinding).
          ;; A catch that rethrew another tagbody's GO left a live handler funclet at
          ;; every level it crossed: a GO out of 20000 nested tagbodies took 4.1s and
          ;; 50000 killed the process (see compile-catch).
          (:begin-filter-block)
          (:ldloc ,tb-id-key)
          (:call "ControlFlowFilters.GoTagbodyMatches")
          (:begin-filter-handler)
          (:castclass "GoException")
          (:declare-local ,ex-key "GoException")
          (:stloc ,ex-key)
          (:ldloc ,ex-key) (:callvirt "GoException.get_TargetLabel")
          (:stloc ,index-key)
          (:leave ,loop-label)
          (:end-exception-block)
          (:br ,loop-label)
          (:label ,end-label)
          (:call "MultipleValues.Reset")  ; tagbody return is always single NIL
          ,@(emit-nil))
        ;; All go's are local: no GoException can target this tagbody. Skip the
        ;; try/catch entirely. tb-id-key is unreferenced and needs no slot.
        `((:declare-local ,index-key "Int32")
          (:ldc-i4 0) (:stloc ,index-key)
          (:declare-local ,done-key "Boolean")
          (:ldc-i4 0) (:stloc ,done-key)
          (:label ,loop-label)
          ,@poll-instrs
          (:ldloc ,done-key) (:brtrue ,end-label)
          (:ldloc ,index-key)
          (:switch ,seg-labels)
          (:br ,leave-label)
          ,@seg-instrs
          (:label ,leave-label)
          (:ldc-i4 1) (:stloc ,done-key)
          (:leave ,loop-label)
          (:label ,end-label)
          (:call "MultipleValues.Reset")
          ,@(emit-nil)))))

(defun compile-go (tag)
  "Compile (go tag).
   Local go (5th element in *go-tags* entry present) sets index + leave.
   Non-local go (from closures, no 5th element) uses GoException throw."
  (let ((entry (assoc tag (cstate-go-tags))))
    (unless entry (error "go: no tagbody tag named ~s" tag))
    ;; Format: (tag-name tb-var-name tb-id-key label-idx [index-key loop-label])
    (let ((tb-id-key (third entry))
          (label-idx (fourth entry))
          (local-index-key (fifth entry))
          (local-loop-label (sixth entry))
          (needs-catch (seventh entry))
          (has-go (eighth entry)))
      ;; Any go makes the target tagbody a potential loop → it places an
      ;; interrupt safepoint on its dispatch label (see compile-tagbody).
      (when has-go (setf (car has-go) t))
      (if (and local-index-key (not *in-finally-block*))
          ;; Local go: set index and leave try block — no exception
          `((:ldc-i4 ,label-idx)
            (:stloc ,local-index-key)
            (:leave ,local-loop-label))
          ;; Non-local go (or go from finally block): throw GoException. Flag the
          ;; target tagbody so it keeps its GoException catch.
          (progn
            (when needs-catch (setf (car needs-catch) t))
            `((:ldloc ,tb-id-key)
              (:ldc-i4 ,label-idx)
              (:newobj "GoException")
              (:throw)))))))

;;; ============================================================
;;; unwind-protect
;;; ============================================================

(defun compile-unwind-protect (protected-form cleanup-forms)
  "Compile (unwind-protect protected cleanup...).
   Saves/restores MultipleValues so body's secondary values are preserved
   (e.g. (progv ... (values x y z))). The restore is done AFTER the exception
   block (not in finally) so it only runs on normal return, not during
   non-local exits (return-from, throw)."
  (let ((result-key (gen-local "UPRES"))
        (mv-count-key (gen-local "UPMVC"))
        (mv-vals-key (gen-local "UPMVV"))
        (normal-label (gen-label "UPNORM")))
    `((:declare-local ,result-key "LispObject")
      (:declare-local ,mv-count-key "Int32")
      (:declare-local ,mv-vals-key "LispObject[]")
      ,@(emit-nil) (:stloc ,result-key)
      (:begin-exception-block)
      ;; Protected form in MV-propagating position: unwind-protect returns body's values.
      ;; *in-tail-position* nil (TCO illegal across try boundary).
      ,@(let ((*in-tail-position* nil) (*in-mv-context* t)) (compile-expr protected-form))
      ;; If protected-form did a non-local exit (leave/throw),
      ;; the stloc below is unreachable. Use a label so dead-code
      ;; elimination in the assembler can handle it cleanly.
      (:label ,normal-label)
      (:stloc ,result-key)
      ;; Save MultipleValues state before cleanup runs
      (:call "MultipleValues.SaveCount") (:stloc ,mv-count-key)
      (:call "MultipleValues.SaveValues") (:stloc ,mv-vals-key)
      (:begin-finally-block)
      ,@(let ((*in-tail-position* nil)
              (*in-finally-block* t))
           (loop for form in cleanup-forms
                 append (compile-and-pop form)))
      (:end-exception-block)
      ;; Restore MultipleValues AFTER exception block (only on normal return)
      (:ldloc ,mv-count-key) (:ldloc ,mv-vals-key)
      (:call "MultipleValues.RestoreSaved")
      (:ldloc ,result-key))))

;;; ============================================================
;;; handler-case
;;; ============================================================

(defun %handler-type-spec-load (type-spec)
  "Instructions that push the handler clause's type specifier as a LispObject for
   Runtime.Typep. A bare symbol / class metaobject loads its name symbol (Typep
   matches CL condition types by name); a compound specifier — (or ...), (and ...),
   (member ...), a deftype, etc. — is loaded as its literal list so Typep evaluates
   it (was previously stringified into one bogus symbol, so (or ...) never matched)."
  (cond
    ((symbolp type-spec) `((:ldstr ,(symbol-name type-spec)) (:call "Startup.Sym")))
    ((and (typep type-spec 'standard-object) (class-name type-spec))
     `((:ldstr ,(symbol-name (class-name type-spec))) (:call "Startup.Sym")))
    (t (compile-quoted type-spec))))

(defun compile-handler-case (body-form clauses)
  "Compile (handler-case body (type1 (var) handler1...) (type2 () handler2...) ...).
   Uses HandlerClusterStack so handler-case takes priority over enclosing handler-bind
   handlers (innermost wins, per CL spec).

   Key CIL constraint: brfalse/brtrue inside a catch block can only branch WITHIN that
   catch block; use 'leave' to exit the catch block to a label in an enclosing try."
  (let* ((result-key (gen-local "HCRES"))
         (outer-end-label (gen-label "HCOUTEREND"))
         (inner-end-label (gen-label "HCINNEREND"))
         (hc-tag-key (gen-local "HCTAG"))
         (cond-key (gen-local "HCCOND"))
         (exobj-key (gen-local "HCEXOBJ"))
         (specs-key (gen-local "HCSPECS"))
         (ci-key (gen-local "HCCIKEY"))
         ;; Parse clauses: each is (type-name var handler-body)
         (parsed (mapcar (lambda (clause)
                           (let* ((type-spec (car clause))
                                  (lambda-list (cadr clause))
                                  ;; A clause lambda list is (var) or (); when it
                                  ;; is (nil) the name is the constant NIL, which
                                  ;; cannot be bound -- that used to read as "no
                                  ;; variable" and pass silently.
                                  (var (if lambda-list
                                           (check-binding-name (car lambda-list)
                                                               "HANDLER-CASE")
                                           nil))
                                  (handler-body (cddr clause)))
                             (list type-spec var handler-body)))
                         clauses))
         (n (length parsed))
         ;; Generate unified clause body labels (shared across all catch dispatches)
         (clause-labels (loop for i from 0 below n
                              collect (gen-label (format nil "HCCLAUSE~d" i))))
         ;; Skip labels for the ceq+brfalse dispatch, which must stay WITHIN the
         ;; filter's handler block (a brfalse cannot leave a funclet).
         (ci-skip-labels (loop for i from 0 below n
                               collect (gen-label (format nil "HCCISKIP~d" i)))))
    `(;; Create unique tag for this handler-case instance
      (:declare-local ,hc-tag-key "Object")
      (:newobj "Object") (:stloc ,hc-tag-key)
      ;; Shared condition local (set before dispatching to clause body)
      (:declare-local ,cond-key "LispObject")
      ,@(emit-nil) (:stloc ,cond-key)
      ;; Matched clause index, written by the filter and read by its handler.
      (:declare-local ,ci-key "Int32")
      ;; The in-flight exception, kept by the filter for the handler.
      (:declare-local ,exobj-key "Object")
      ;; Clause type specifiers in clause order, for the filter's type match. Built
      ;; once here rather than re-emitted per handler: they are literals, and the
      ;; filter must stay a straight predicate.
      (:declare-local ,specs-key "LispObject[]")
      (:ldc-i4 ,n)
      (:newarr "LispObject")
      ,@(loop for (type-spec var handler-body) in parsed
              for i from 0
              append `((:dup) (:ldc-i4 ,i)
                       ,@(%handler-type-spec-load type-spec)
                       (:stelem-ref)))
      (:stloc ,specs-key)
      ;; Build HandlerBinding[] for our handler-case cluster
      (:ldc-i4 ,n)
      (:newarr "HandlerBinding")
      ,@(loop for (type-spec var handler-body) in parsed
              for i from 0
              append `((:dup) (:ldc-i4 ,i)
                        ;; Type specifier (symbol/class name, or compound list literal)
                        ,@(%handler-type-spec-load type-spec)
                        ;; The clause is identified by (tag, index); the binding
                        ;; carries them itself, so entering a handler-case builds
                        ;; no handler function per clause.
                        (:ldloc ,hc-tag-key)
                        (:ldc-i4 ,i)
                        (:newobj "HandlerBindingHc")
                        (:stelem-ref)))
      (:call "HandlerClusterStack.PushCluster")
      ;; Result local
      (:declare-local ,result-key "LispObject")
      ,@(emit-nil) (:stloc ,result-key)
      ;; try-catch-FINALLY: body + exception dispatch. PopCluster lives in the finally
      ;; so it runs on EVERY exit from the try (normal, matched-clause :leave, rethrow,
      ;; and non-local :leave out of the body). Handler bodies still run with the
      ;; cluster removed (per CL spec) because the :leave to a clause label runs the
      ;; finally before the clause body executes.
      (:begin-exception-block)
      ;; Body in MV-propagating position: handler-case returns body's values (CL spec).
      ;; Self-TCO inside handler-case: use `leave` to exit the try block and prepend
      ;; PopCluster so each iteration has a clean handler stack.
      ,@(let ((*in-try-block* t)         ; protect: :ret invalid in try/catch region
              (*in-mv-context* t)
              ;; When TCO is active, mark the try/catch context so the self-call uses
              ;; `leave TCOLOOP` instead of `ret`. The cluster PopCluster on that leave
              ;; is now handled by the finally block (see below), so we no longer
              ;; prepend it to the leave-instrs (doing so would double-pop).
              (*cstate* (cstate-with *cstate*
                          +cs-tco-in-try-catch+
                          (if (cstate-tco-self-name) t (cstate-tco-in-try-catch)))))
          (compile-expr body-form))
      (:stloc ,result-key)
      (:leave ,inner-end-label)
      ;; One filter for every exception shape this handler-case can take: its own
      ;; HandlerCaseInvocationException (the main path, thrown by the handler
      ;; function HandlerClusterStack.Signal calls), a LispErrorException whose
      ;; condition matches a clause type, and a raw .NET exception wrapped into a
      ;; condition that does. The filter answers "which clause" (or -1) without
      ;; entering the frame, so a signal that crosses N nested handler-cases costs
      ;; one dispatch instead of N stacked ones. Catching everything and rethrowing
      ;; what did not match — the shape this replaces — left a live handler funclet
      ;; at every level, and deep recursion with a handler-case per level died as an
      ;; uncatchable .NET StackOverflowException.
      (:begin-filter-block)
      (:dup) (:stloc ,exobj-key)
      (:ldloc ,hc-tag-key)
      (:ldloc ,specs-key)
      (:call "ControlFlowFilters.HandlerCaseClause")
      (:stloc ,ci-key)
      (:ldloc ,ci-key) (:ldc-i4 -1) (:cgt)
      (:begin-filter-handler)
      (:pop)
      ;; The clause variable binds the condition the filter matched on.
      (:ldloc ,exobj-key)
      (:call "ControlFlowFilters.HandlerCaseCondition")
      (:stloc ,cond-key)
      ,@(loop for label in clause-labels
              for ci-skip in ci-skip-labels
              for i from 0
              append `((:ldloc ,ci-key) (:ldc-i4 ,i) (:ceq) (:brfalse ,ci-skip) ;; within handler
                       (:leave ,label) ;; exit handler to clause body after the block
                       (:label ,ci-skip)))
      ;; Unreachable: the filter only answered 1 for an index in range.
      (:leave ,inner-end-label)
      ;; Finally: pop the cluster on EVERY exit from the try — normal completion, a
      ;; matched-clause :leave, a no-match rethrow, AND a non-local :leave
      ;; (return-from / go) out of the body. The old code popped only on the catch
      ;; and normal-exit paths, so a :leave *through the body* leaked the handler
      ;; cluster; a later signal then fired the stale handler and threw
      ;; HandlerCaseInvocationException past this frame (the make-host-2 irrat crash).
      ;; Clauses still run outside the cluster: the :leave to a clause label runs the
      ;; finally (pop) before the clause body executes.
      (:begin-finally-block)
      (:call "HandlerClusterStack.PopCluster")
      (:end-exception-block) ;; end try-catch-finally
      ;; Normal exit: finally already popped; jump to end
      (:label ,inner-end-label)
      (:br ,outer-end-label)
      ;; Clause bodies (AFTER try-catch, outside exception block). The cluster was
      ;; already popped by the finally when the catch did :leave to the clause label.
      ,@(loop for (type-spec var handler-body) in parsed
              for label in clause-labels
              append (multiple-value-bind (declared-specials real-body)
                         (extract-specials handler-body)
                       (let* ((var-is-special (and var
                                                   (or (member var declared-specials)
                                                       (global-special-p var))))
                              (*specials* (append declared-specials *specials*))
                              ;; The clause var binds the condition into a plain
                              ;; LispObject slot; it must SHADOW an enclosing boxed
                              ;; var of the same name (e.g. a boxed LOOP variable
                              ;; captured by this same-named handler), else its
                              ;; reference compiles to a boxed `slot[0]` ldelem-ref
                              ;; on the condition object → ArrayTypeMismatch.
                              (*cstate*
                               (if (and var (not var-is-special))
                                   (cstate-with *cstate*
                                     +cs-locals+
                                     (let ((var-key (gen-local "HCV")))
                                       (acons var var-key (cstate-locals)))
                                     +cs-boxed-vars+
                                     (remove-if (lambda (x)
                                                  (string= (if (symbolp x) (var-name x) x)
                                                           (var-name var)))
                                                (cstate-boxed-vars)))
                                   *cstate*)))
                         (let ((var-key (if (and var (not var-is-special))
                                            (lookup-local var) nil)))
                           `((:label ,label)
                             ;; cluster already popped by the finally on the catch's
                             ;; :leave to this clause label (was a manual pop here)
                             ,@(cond
                                 (var-is-special
                                  ;; Bind as special (dynamic) variable with try/finally.
                                  ;; Set *in-try-block* before compiling body so TCO hooks
                                  ;; emit `leave` instead of `br` (mirrors compile-let).
                                  (let ((tmp-key (gen-local "HCTMP")))
                                    `((:declare-local ,tmp-key "LispObject")
                                      (:ldloc ,cond-key) (:stloc ,tmp-key)
                                      ,@(compile-let-with-specials
                                         '()
                                         `(,@(compile-sym-lookup var)
                                           (:castclass "Symbol")
                                           (:ldloc ,tmp-key)
                                           (:call "DynamicBindings.Push"))
                                         (let ((*in-try-block* (or *in-try-block* (list var))))
                                           (compile-progn real-body))
                                         (list var)))))
                                 (var
                                  `((:declare-local ,var-key "LispObject")
                                    (:ldloc ,cond-key) (:stloc ,var-key)
                                    ,@(compile-progn real-body)))
                                 (t (compile-progn real-body)))
                             (:stloc ,result-key)
                             (:br ,outer-end-label))))))
      (:label ,outer-end-label)
      (:ldloc ,result-key))))

;;; ============================================================
;;; restart-case
;;; ============================================================

(defun compile-restart-case (body-form clauses)
  "Compile (restart-case body (name (params...) body...) ...).
   Uses RestartClusterStack + RestartInvocationException matching on tag.
   Handler bodies execute AFTER try-catch with PopCluster first,
   so they run outside the restart-case's dynamic scope."
  (let* ((is-signaling-body (labels ((check-signaling (form)
                                        (cond ((not (consp form)) nil)
                                              ((member (car form) '(error signal cerror warn)) t)
                                              ;; Macro form: expand and check (CLHS 9.2.2.5)
                                              ((and (symbolp (car form))
                                                    (find-macro-expander (car form))
                                                    (not (local-function-entry (car form))))
                                               (check-signaling (cached-macroexpand form (find-macro-expander (car form)))))
                                              (t nil))))
                                (or (check-signaling body-form)
                                    ;; Also check symbol-macrolet expansion
                                    (and (symbolp body-form)
                                         (lookup-symbol-macro body-form)
                                         (check-signaling (lookup-symbol-macro body-form))))))
         (result-key (gen-local "RCRES"))
         (try-end-label (gen-label "RCTRYEND"))
         (done-label (gen-label "RCDONE"))
         ;; The in-flight exception, kept by the filter for its handler.
         (exobj-key (gen-local "RCEXOBJ"))
         ;; Matched clause index, written by the filter and read by its handler.
         (ri-key (gen-local "RCIDX"))
         (args-key (gen-local "RCARGS"))
         ;; One unique tag object per clause, in clause order. The LispRestart
         ;; entries and the filter both read them from here.
         (tags-key (gen-local "RCTAGS"))
         ;; Parse clauses: ((name params body...) ...)
         ;; Extract :report, :interactive, :test keyword options before body
         (parsed (mapcar (lambda (clause)
                           (let ((name (symbol-name (car clause)))
                                 (params (cadr clause))
                                 (rest (cddr clause))
                                 (report nil)
                                 (interactive nil)
                                 (test-fn nil))
                             ;; Parse keyword options from the beginning of rest
                             (loop while rest
                                   do (cond ((eq (car rest) :report)
                                             (setq report (cadr rest))
                                             (setq rest (cddr rest)))
                                            ((eq (car rest) :interactive)
                                             (setq interactive (cadr rest))
                                             (setq rest (cddr rest)))
                                            ((eq (car rest) :test)
                                             (setq test-fn (cadr rest))
                                             (setq rest (cddr rest)))
                                            (t (return))))
                             (list name params rest report interactive (car clause) test-fn)))
                         clauses))
         ;; Generate labels for each clause
         (clause-labels (loop for i from 0 below (length parsed)
                              collect (gen-label (format nil "RC~d" i))))
         ;; Skip labels for the ceq+brfalse dispatch, which must stay WITHIN the
         ;; filter's handler block (a brfalse cannot leave a funclet).
         (skip-labels (loop for i from 0 below (length parsed)
                            collect (gen-label (format nil "RCSKIP~d" i)))))
    `((:declare-local ,result-key "LispObject")
      ,@(emit-nil) (:stloc ,result-key)
      (:declare-local ,args-key "LispObject[]")
      (:declare-local ,exobj-key "Object")
      (:declare-local ,ri-key "Int32")
      ;; Create a tag object for each restart
      (:declare-local ,tags-key "Object[]")
      (:ldc-i4 ,(length parsed))
      (:newarr "Object")
      ,@(loop for i from 0 below (length parsed)
              append `((:dup) (:ldc-i4 ,i) (:newobj "Object") (:stelem-ref)))
      (:stloc ,tags-key)
      ;; Build LispRestart[] array and push cluster
      (:ldc-i4 ,(length parsed))
      (:newarr "LispRestart")
      ,@(loop for (name params handler-body report interactive name-sym test-fn) in parsed
              for i from 0
              append `((:dup) (:ldc-i4 ,i)
                       (:ldstr ,name)   ;; restart name
                       (:ldnull)        ;; handler (unused, dispatch is via tag)
                       ,@(if (stringp report)
                             `((:ldstr ,report))
                             `((:ldnull)))  ;; description
                       (:ldloc ,tags-key) (:ldc-i4 ,i) (:ldelem-ref)  ;; tag
                       (:ldc-i4 0)      ;; isBindRestart = false
                       (:newobj "LispRestart")
                       ;; Set NameSymbol to original symbol
                       ,@(when name-sym
                           `((:dup)
                             ,@(compile-expr (list 'quote name-sym))
                             (:castclass "Symbol")
                             (:callvirt "LispRestart.set_NameSymbol")))
                       ;; Set ReportFunction if report is a lambda/function form
                       ,@(if (and report (not (stringp report)))
                             `((:dup)
                               ,@(compile-expr report)
                               (:callvirt "LispRestart.set_ReportFunction"))
                             nil)
                       ;; Set InteractiveFunction if provided
                       ;; CLHS: :interactive takes a function designator (symbol → #'name)
                       ,@(if interactive
                             `((:dup)
                               ,@(compile-expr (if (symbolp interactive)
                                                   `(function ,interactive)
                                                   interactive))
                               (:callvirt "LispRestart.set_InteractiveFunction"))
                             nil)
                       ;; Set TestFunction if provided (symbol → #'name)
                       ,@(if test-fn
                             `((:dup)
                               ,@(compile-expr (if (symbolp test-fn)
                                                   `(function ,test-fn)
                                                   test-fn))
                               (:callvirt "LispRestart.set_TestFunction"))
                             nil)
                       (:stelem-ref)))
      (:call "RestartClusterStack.PushCluster")
      ;; try-catch for body + RestartInvocationException
      (:begin-exception-block)
      ;; Body in MV-propagating position: restart-case returns body's values.
      ,@(let ((*in-tail-position* nil) (*in-mv-context* t))
           (compile-expr (if is-signaling-body
                             `(let ((%rc-restarts (%top-cluster-restarts)))
                                (handler-bind ((condition
                                               (lambda (%rc-cond)
                                                 (%associate-condition-restarts
                                                  %rc-cond
                                                  %rc-restarts))))
                                  ,body-form))
                             body-form)))
      (:stloc ,result-key)
      (:leave ,try-end-label)
      ;; A CIL exception FILTER answers "which of MY clauses does this invocation
      ;; target" (or -1) without entering the frame. The shape this replaces caught
      ;; every RestartInvocationException and rethrew the ones it did not own, and a
      ;; rethrow restarts dispatch from inside the handler funclet, which then stays
      ;; live for the rest of the exception's journey. Invoking a restart established
      ;; N levels out therefore stacked N dispatches: quadratic time, and past ~20000
      ;; levels an uncatchable .NET StackOverflowException, at a depth the same
      ;; recursion descends ten times over.
      (:begin-filter-block)
      (:dup) (:stloc ,exobj-key)
      (:ldloc ,tags-key)
      (:call "ControlFlowFilters.RestartCaseTag")
      (:stloc ,ri-key)
      (:ldloc ,ri-key) (:ldc-i4 -1) (:cgt)
      (:begin-filter-handler)
      (:pop)
      ;; Save arguments array
      (:ldloc ,exobj-key)
      (:castclass "RestartInvocationException")
      (:callvirt "RestartInvocationException.get_Arguments")
      (:stloc ,args-key)
      ;; Dispatch on the clause index the filter picked
      ,@(loop for label in clause-labels
              for skip in skip-labels
              for i from 0
              append `((:ldloc ,ri-key) (:ldc-i4 ,i) (:ceq)
                       (:brfalse ,skip)  ;; skip if not this clause (within handler)
                       (:leave ,label)   ;; exit handler to clause body
                       (:label ,skip)))  ;; within handler
      ;; Unreachable: the filter only answered 1 for an index in range.
      (:leave ,try-end-label)
      ;; Finally: pop the cluster on EVERY exit from the try — normal completion,
      ;; a matched-restart :leave to a clause, an invocation the filter declined,
      ;; AND a non-local transfer (return-from / throw / go) out of the body. The
      ;; old code popped only on the normal-exit and rethrow paths, so a non-local exit *through the
      ;; body* leaked the restart cluster. SBCL's compile-file body does exactly
      ;; that, leaking one RECOMPILE cluster per stem in the make-host-2 XC build.
      (:begin-finally-block)
      (:call "RestartClusterStack.PopCluster")
      (:end-exception-block)
      ;; Normal exit: finally already popped; jump to done (skip clause bodies).
      (:label ,try-end-label)
      (:br ,done-label)
      ;; Clause bodies (AFTER try-catch, with PopCluster before handler body)
      ,@(loop for (name params handler-body report interactive name-sym test-fn) in parsed
              for label in clause-labels
              append (if (and params (car params))
                         ;; Has parameters: bind args via Runtime.RestartArg
                         (let ((*cstate* *cstate*)
                               (param-bindings nil))
                           ;; Process declare forms at start of handler-body
                           (let ((effective-body handler-body))
                             (loop while (and effective-body
                                             (consp (car effective-body))
                                             (eq (caar effective-body) 'declare))
                                   do (pop effective-body))
                             ;; Parse lambda list: split into positional, optional, rest, key, aux
                             (let ((positional nil)
                                   (optional nil)
                                   (rest-var nil)
                                   (key-params nil)
                                   (aux-params nil)
                                   (mode :positional))
                               (dolist (p params)
                                 (cond ((eq p '&rest)  (setq mode :rest))
                                       ((eq p '&optional) (setq mode :optional))
                                       ((eq p '&key) (setq mode :key))
                                       ((eq p '&aux) (setq mode :aux))
                                       ((eq p '&allow-other-keys) nil)
                                       ((eq mode :positional) (push p positional))
                                       ((eq mode :optional) (push p optional))
                                       ((eq mode :rest) (setq rest-var p))
                                       ((eq mode :key) (push p key-params))
                                       ((eq mode :aux) (push p aux-params))))
                               (setq positional (nreverse positional))
                               (setq optional (nreverse optional))
                               (setq key-params (nreverse key-params))
                               (setq aux-params (nreverse aux-params))
                               ;; Bind positional params
                               (let ((idx 0))
                                 (dolist (var positional)
                                   (let ((var-key (gen-local "RCV")))
                                     (setf *cstate* (cstate-with *cstate* +cs-locals+
                                                                 (acons var var-key (cstate-locals))))
                                     (push `((:declare-local ,var-key "LispObject")
                                             (:ldloc ,args-key)
                                             (:ldc-i4 ,idx)
                                             (:call "Runtime.RestartArg")
                                             (:stloc ,var-key))
                                           param-bindings))
                                   (incf idx))
                                 ;; Bind optional params (RestartArg returns NIL for out-of-bounds)
                                 (dolist (var optional)
                                   (let* ((var-name (if (consp var) (car var) var))
                                          (var-key (gen-local "RCV")))
                                     (setf *cstate* (cstate-with *cstate* +cs-locals+
                                                                 (acons var-name var-key (cstate-locals))))
                                     (push `((:declare-local ,var-key "LispObject")
                                             (:ldloc ,args-key)
                                             (:ldc-i4 ,idx)
                                             (:call "Runtime.RestartArg")
                                             (:stloc ,var-key))
                                           param-bindings))
                                   (incf idx))
                                 ;; Bind &rest param
                                 (when rest-var
                                   (let ((var-key (gen-local "RCV")))
                                     (setf *cstate* (cstate-with *cstate* +cs-locals+
                                                                 (acons rest-var var-key (cstate-locals))))
                                     (push `((:declare-local ,var-key "LispObject")
                                             (:ldloc ,args-key)
                                             (:ldc-i4 ,idx)
                                             (:call "Runtime.RestartArgsAsList")
                                             (:stloc ,var-key))
                                           param-bindings)))
                                 ;; Bind &key params via Runtime.RestartKeyArg
                                 (dolist (kp key-params)
                                   (let* ((var-name (if (consp kp) (car kp) kp))
                                          (keyword-name (intern (symbol-name var-name) "KEYWORD"))
                                          (var-key (gen-local "RCV")))
                                     (setf *cstate* (cstate-with *cstate* +cs-locals+
                                                                 (acons var-name var-key (cstate-locals))))
                                     (push `((:declare-local ,var-key "LispObject")
                                             (:ldloc ,args-key)
                                             ,@(compile-expr (list 'quote keyword-name))
                                             (:ldc-i4 ,idx)
                                             (:call "Runtime.RestartKeyArg")
                                             (:stloc ,var-key))
                                           param-bindings)))
                                 ;; Bind &aux params via compile-expr of init form
                                 (dolist (ap aux-params)
                                   (let* ((var-name (if (consp ap) (car ap) ap))
                                          (init-form (if (consp ap) (cadr ap) nil))
                                          (var-key (gen-local "RCV")))
                                     (setf *cstate* (cstate-with *cstate* +cs-locals+
                                                                 (acons var-name var-key (cstate-locals))))
                                     (push `((:declare-local ,var-key "LispObject")
                                             ,@(compile-expr init-form)
                                             (:stloc ,var-key))
                                           param-bindings)))))
                             `((:label ,label)
                               ;; cluster already popped by the finally on the
                               ;; catch's :leave to this clause label
                               ,@(apply #'append (nreverse param-bindings))
                               ,@(compile-progn effective-body)
                               (:stloc ,result-key)
                               (:br ,done-label))))
                         ;; No parameter
                         `((:label ,label)
                           ;; cluster already popped by the finally (see above)
                           ,@(compile-progn handler-body)
                           (:stloc ,result-key)
                           (:br ,done-label))))
      (:label ,done-label)
      (:ldloc ,result-key))))

;;; ============================================================
;;; restart-bind
;;; ============================================================

(defun compile-restart-bind (bindings body)
  "Compile (restart-bind ((name function &key ...) ...) body...).
   Creates LispRestart[] with isBindRestart=true, pushes cluster,
   body in try/finally with PopCluster. Unlike restart-case, there
   is no catch block - invoke-restart calls the handler directly."
  (let* ((n (length bindings))
         (result-key (gen-local "RBRES")))
    `(;; Build LispRestart[] array
      (:ldc-i4 ,n)
      (:newarr "LispRestart")
      ,@(loop for binding in bindings
              for i from 0
              append (let* ((name (car binding))
                            (fn-form (cadr binding))
                            (name-str (if name (symbol-name name) "NIL"))
                            (rest-args (cddr binding))
                            (test-fn nil)
                            (report-fn nil)
                            (interactive-fn nil))
                       ;; Parse keyword args
                       (loop while rest-args
                             do (cond ((eq (car rest-args) :test-function)
                                       (setq test-fn (cadr rest-args))
                                       (setq rest-args (cddr rest-args)))
                                      ((eq (car rest-args) :report-function)
                                       (setq report-fn (cadr rest-args))
                                       (setq rest-args (cddr rest-args)))
                                      ((eq (car rest-args) :interactive-function)
                                       (setq interactive-fn (cadr rest-args))
                                       (setq rest-args (cddr rest-args)))
                                      (t (setq rest-args (cddr rest-args)))))
                       `((:dup) (:ldc-i4 ,i)
                         (:ldstr ,name-str)         ;; restart name
                         ;; handler function: compile the function form
                         ,@(compile-expr fn-form)
                         (:castclass "LispFunction")
                         (:callvirt "LispFunction.get_RawFunction")
                         (:ldnull)                  ;; description
                         (:ldnull)                  ;; tag (unused for bind restarts)
                         (:ldc-i4 1)                ;; isBindRestart = true
                         (:newobj "LispRestart")
                         ;; Set NameSymbol
                         ,@(when name
                             `((:dup)
                               ,@(compile-expr (list 'quote name))
                               (:castclass "Symbol")
                               (:callvirt "LispRestart.set_NameSymbol")))
                         ;; Set TestFunction if provided
                         ,@(when test-fn
                             `((:dup)
                               ,@(compile-expr test-fn)
                               (:callvirt "LispRestart.set_TestFunction")))
                         ;; Set ReportFunction if provided
                         ,@(when report-fn
                             `((:dup)
                               ,@(compile-expr report-fn)
                               (:callvirt "LispRestart.set_ReportFunction")))
                         ;; Set InteractiveFunction if provided
                         ,@(when interactive-fn
                             `((:dup)
                               ,@(compile-expr interactive-fn)
                               (:callvirt "LispRestart.set_InteractiveFunction")))
                         (:stelem-ref))))
      ;; PushCluster
      (:call "RestartClusterStack.PushCluster")
      ;; try/finally to ensure PopCluster
      (:declare-local ,result-key "LispObject")
      ,@(emit-nil) (:stloc ,result-key)
      (:begin-exception-block)
      ;; Body in MV-propagating position: restart-bind returns body's values.
      ,@(let ((*in-tail-position* nil) (*in-mv-context* t)) (compile-progn body))
      (:stloc ,result-key)
      (:begin-finally-block)
      (:call "RestartClusterStack.PopCluster")
      (:end-exception-block)
      (:ldloc ,result-key))))

;;; ============================================================
;;; handler-bind
;;; ============================================================

(defun compile-handler-bind (bindings body)
  "Compile (handler-bind ((type handler-fn) ...) body...).
   Creates HandlerBinding[] array, pushes cluster, body in try/finally with PopCluster."
  (let* ((n (length bindings))
         (result-key (gen-local "HBRES")))
    `(;; Build HandlerBinding[] array
      (:ldc-i4 ,n)
      (:newarr "HandlerBinding")
      ,@(loop for binding in bindings
              for i from 0
              append (let ((type-spec (car binding))
                           (handler-form (cadr binding)))
                       `((:dup) (:ldc-i4 ,i)
                         ;; type specifier as symbol
                         ,@(compile-quoted type-spec)
                         ;; handler function
                         ,@(compile-expr handler-form)
                         (:call "Runtime.CoerceToFunction")
                         (:newobj "HandlerBinding")
                         (:stelem-ref))))
      ;; PushCluster
      (:call "HandlerClusterStack.PushCluster")
      ;; try { try { body } catch(System.Exception) { wrap+signal+rethrow } } finally { PopCluster }
      (:declare-local ,result-key "LispObject")
      ,@(emit-nil) (:stloc ,result-key)
      ,@(let ((inner-end-label (gen-label "HBINNER")))
          `((:begin-exception-block)  ;; outer try (for finally)
            (:begin-exception-block)  ;; inner try (for catch)
            ;; Body in MV-propagating position: handler-bind returns body's values.
            ,@(let ((*in-tail-position* nil) (*in-mv-context* t)) (compile-progn body))
            (:stloc ,result-key)
            (:leave ,inner-end-label)
            ;; Catch raw .NET exceptions: wrap as LispCondition, signal through handlers,
            ;; then throw as LispErrorException (so handler-bind handlers can
            ;; do non-local exits via return-from/throw).
            ;; Lisp control exceptions (BlockReturn, CatchThrow, Go, etc.) are rethrown.
            (:begin-catch-block "System.Exception")
            (:call "Runtime.RewrapNonLispException")
            (:end-exception-block)  ;; end inner try-catch
            (:label ,inner-end-label)
            (:begin-finally-block)
            (:call "HandlerClusterStack.PopCluster")
            (:end-exception-block)  ;; end outer try-finally
            (:ldloc ,result-key))))))

;;; ============================================================
;;; and / or / cond
;;; ============================================================

(defun compile-and (args)
  "Compile (and a b c) with short-circuit evaluation."
  (cond
    ((null args) (emit-t))
    ((null (cdr args)) (compile-expr (car args)))
    (t (let ((end-label (gen-label "ANDEND"))
             (result-key (gen-local "AND")))
         `((:declare-local ,result-key "LispObject")
           ,@(loop for (arg . rest) on args
                   if rest
                     ;; Intermediate arg: NOT in tail position — its value is
                     ;; consumed by the IsTruthy test, not returned. Binding
                     ;; *in-tail-position* nil prevents a self-call here from
                     ;; being TCO-rewritten into a value-discarding loop-back
                     ;; (symmetric with compile-or).
                     append `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr arg))
                              (:call "MultipleValues.Primary")
                              (:stloc ,result-key)
                              (:ldloc ,result-key)
                              (:call "Runtime.IsTruthy")
                              (:brfalse ,end-label))
                   else
                     ;; Last arg: pass through all values
                     append `(,@(compile-expr arg)
                              (:stloc ,result-key)))
           (:label ,end-label)
           (:ldloc ,result-key))))))

(defun compile-or (args)
  "Compile (or a b c) with short-circuit evaluation."
  (cond
    ((null args) (emit-nil))
    ((null (cdr args)) (compile-expr (car args)))
    (t (let ((end-label (gen-label "OREND"))
             (result-key (gen-local "OR")))
         `((:declare-local ,result-key "LispObject")
           ,@(loop for (arg . rest) on args
                   if rest
                     ;; Intermediate arg: NOT in tail position — its value is
                     ;; consumed by the IsTruthy test, not returned. Binding
                     ;; *in-tail-position* nil prevents a self-call here from
                     ;; being TCO-rewritten into a value-discarding loop-back,
                     ;; which made e.g. (or (f (car x)) (f (cdr x))) return the
                     ;; wrong value on deep inputs. The last arg
                     ;; below keeps tail position so genuine tail self-calls
                     ;; still TCO.
                     append `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr arg))
                              (:call "MultipleValues.Primary")
                              (:stloc ,result-key)
                              (:ldloc ,result-key)
                              (:call "Runtime.IsTruthy")
                              (:brtrue ,end-label))
                   else
                     ;; Last arg: pass through all values (tail position kept)
                     append `(,@(compile-expr arg)
                              (:stloc ,result-key)))
           (:label ,end-label)
           (:ldloc ,result-key))))))

(defun compile-cond (clauses &optional shared-tmp)
  "Compile (cond (test1 body1...) ...) to nested conditional.
   shared-tmp: shared LispObject local for no-body arms."
  (if (null clauses)
      (emit-nil)
      (let* ((clause (car clauses))
             (test (car clause))
             (body (cdr clause)))
        (if (eq test t)
            ;; Default clause
            (if body (compile-progn body) (emit-t))
            ;; Normal clause
            (let ((else-label (gen-label "CELSE"))
                  (end-label (gen-label "CEND")))
              (let ((fused (compile-if-fused-comparison-p test)))
                (if body
                    ;; With body: evaluate test, if true evaluate body
                    (cond
                      (fused
                        ;; Fused comparison: skip IsTruthy
                        `(,@(let ((*in-tail-position* nil))
                              (ecase (first fused)
                                (:binary (compile-binary-call (third fused) (second fused)))
                                (:unary (compile-unary-call (third fused) (second fused)))
                                (:fixnum-cmp (compile-fixnum-cmp (third fused) (second fused)))
                                (:sym-eq (compile-sym-eq (third fused)))
               (:double-cmp (compile-double-cmp (third fused) (second fused)))))
                          (:brfalse ,else-label)
                          ,@(compile-progn body)
                          (:br ,end-label)
                          (:label ,else-label)
                          ,@(compile-cond (cdr clauses) shared-tmp)
                          (:label ,end-label)))
                      ;; (cond ((and ...) body) ...): chain boolean branches
                      ((and (consp test) (member (car test) '(and or)) (cddr test))
                       `(,@(compile-boolean-branch test else-label nil)
                         ,@(compile-progn body)
                         (:br ,end-label)
                         (:label ,else-label)
                         ,@(compile-cond (cdr clauses) shared-tmp)
                         (:label ,end-label)))
                      (t
                        ;; Default: normalize MV state, then IsTruthy.
                        ;; The test is never in tail position (only the body is);
                        ;; binding *in-tail-position* nil prevents a self-recursive
                        ;; call in the test from being TCO'd into a jump.
                        `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr test))
                          (:call "MultipleValues.Primary")
                          (:call "Runtime.IsTruthy")
                          (:brfalse ,else-label)
                          ,@(compile-progn body)
                          (:br ,end-label)
                          (:label ,else-label)
                          ,@(compile-cond (cdr clauses) shared-tmp)
                          (:label ,end-label))))
                    ;; No body: all no-body arms share one CTMP slot
                    (let ((tmp (or shared-tmp (gen-local "CTMP"))))
                      ;; Test-only arm: the test value becomes the result, but the
                      ;; test itself is not in tail position — a self-recursive call
                      ;; here must compute a value, not TCO-jump.
                      `(,@(unless shared-tmp `((:declare-local ,tmp "LispObject")))
                        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr test))
                        (:call "MultipleValues.Primary")
                        (:stloc ,tmp)
                        (:ldloc ,tmp)
                        (:call "Runtime.IsTruthy")
                        (:brfalse ,else-label)
                        (:ldloc ,tmp)
                        (:br ,end-label)
                        (:label ,else-label)
                        ,@(compile-cond (cdr clauses) tmp)
                        (:label ,end-label))))))))))

;;; ============================================================
;;; compile-form handler registrations
;;; Must be after all compile-* helpers are defined (both cil-compiler.lisp
;;; and cil-forms.lisp).  Populates *compile-form-handlers* for O(1) dispatch.
;;; ============================================================

(defun compile-value-arg (arg)
  "Compile ARG as a known-function intrinsic argument: never in tail position (the
   consuming intrinsic call is), never in MV context (a single value is taken).
   Without this, a self-tail-call in an intrinsic argument inherits the tail
   context, fires TCO (a br/leave to the TCO loop), and the intrinsic call that
   follows becomes unreachable dead code — its side effect silently lost
   (e.g. (defun f (n) (if (zerop n) 0 (print (f (1- n))))) never printing). Mirrors
   compile-unary-call / compile-args-array, which already bind both. NOT for
   tail/MV-transparent intrinsics (the/when/unless/min/max/values/…): those use a
   bare (compile-expr …) as their whole result and must keep the caller's context."
  (let ((*in-tail-position* nil) (*in-mv-context* nil))
    (compile-expr arg)))

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
      (t
       (let ((type-arg (cadr a)))
         (if (and (consp type-arg) (eq (car type-arg) 'quote) (symbolp (cadr type-arg)))
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
                   (compile-binary-call (list (car a) type-arg) "Runtime.Typep")))
             (compile-binary-call (list (car a) type-arg) "Runtime.Typep")))))))

(defun compile-aref-form (expr)
  "The AREF form handler. Numeric/float-backed array locals read the element
   raw (long / r8) and box once here for the value context; statically
   fixnum-typed indices ride the *L runtime variants with no index boxing."
  (let* ((args (cdr expr))
         (idxs (cdr args))
         (num-info (numeric-array-aref-info expr))
         (float-kind (numeric-array-aref-float-kind expr))
         (native (and (<= 2 (length args) 4)
                      (every #'fixnum-typed-p idxs))))
    (cond
      (num-info
       `(,@(compile-numeric-aref-as-long (first args) idxs (car num-info))
         (:call "Fixnum.Make")))
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

(defun compile-%aref-set-form (expr)
  "The %AREF-SET form handler (the setf expansion of AREF). Mirrors
   COMPILE-AREF-FORM: numeric/float-backed locals store raw with matching
   value types; a mismatched value falls to the general coercing path."
  (let* ((args (cdr expr))
         (idxs (butlast (cdr args)))
         (val (car (last args)))
         (aref-form `(aref ,(first args) ,@idxs))
         (num-info (and (fixnum-typed-p val)
                        (numeric-array-aref-info aref-form)))
         (float-kind (numeric-array-aref-float-kind aref-form))
         (native (and (<= 3 (length args) 5)
                      (every #'fixnum-typed-p idxs))))
    (cond
      (num-info
       (compile-numeric-aref-set (first args) idxs val (car num-info)))
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
           (pushnew sym *specials*)
           (pushnew sym *global-specials*))
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
        ;; (optimize ... (debug N) ...). Compile-time only: the DEBUG quality
        ;; decides whether the functions compiled after it record a debugger
        ;; frame, which is a property of the code we emit, not of the image.
        ((and (consp spec) (eq (car spec) 'optimize))
         (dolist (q (cdr spec))
           (cond ((and (consp q) (eq (car q) 'debug) (integerp (cadr q)))
                  (setq *optimize-debug* (cadr q)))
                 ((eq q 'debug) (setq *optimize-debug* 3))))
         (push `(proclaim ',spec) proclaim-forms))
        ;; (ftype (function (arg-types...) return-type) name...)
        ((and (consp spec) (eq (car spec) 'ftype)
              (consp (cadr spec))
              (eq (car (cadr spec)) 'function)
              (cddr (cadr spec)))
         (let ((ret (car (last (cadr spec)))))
           (dolist (name (cddr spec))
             (when (symbolp name)
               (setf (gethash name *function-return-types*) ret)))))))
    (if proclaim-forms
        (compile-progn (append (nreverse proclaim-forms) (list nil)))
        (emit-nil))))

(let ((h *compile-form-handlers*))

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
  (setf (gethash '/= h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.NumNotEqualN"))))

  ;; Equality
  (setf (gethash 'eq h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Eq")))
  (setf (gethash 'eql h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Eql")))
  (setf (gethash 'equal h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Equal")))

  ;; List ops
  (setf (gethash 'cons h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.MakeCons" "CONS")))
  (setf (gethash 'car h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Car" "CAR")))
  (setf (gethash 'cdr h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Cdr" "CDR")))
  (setf (gethash 'list h) (lambda (expr) (compile-list-call (cdr expr))))
  (setf (gethash 'list* h) (lambda (expr) (compile-list-star-call (cdr expr))))
  (setf (gethash 'append h) (lambda (expr) (compile-append (cdr expr))))
  (setf (gethash 'length h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Length" "LENGTH")))
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
              ;; defun exactly) — the dominant call shape, e.g. the liveness
              ;; analysis' (nconc writes reads). Unconditional inline, same
              ;; precedent as the rplacd/append handlers here (builtin handlers
              ;; don't consult notinline; only compiler macros do, CLHS 3.2.2.1.1).
              ;; NOTE: Runtime.Nconc2 is a different, COPYING helper (CLOS
              ;; method combination) — NconcDestructive2 is the CL-semantics one.
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
  (setf (gethash 'atom h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Atom" "ATOM")))
  (setf (gethash 'consp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Consp" "CONSP")))
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
                (compile-binary-call (list (first args) (second args)) "Runtime.Subtypep")
                (compile-binary-call args "Runtime.Subtypep")))))

  ;; Array ops. When every index is statically fixnum-typed (e.g. an
  ;; Int64-slot loop counter), lower the indices as raw longs and call the
  ;; *L runtime variants — no per-access Fixnum boxing on either side.
  (setf (gethash 'svref h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (if (and (= (length args) 2) (fixnum-typed-p (second args)))
                (compile-aref-native-index-call
                 (first args) (list (second args)) "Runtime.ArefL")
                (compile-binary-call args "Runtime.Aref")))))
  (setf (gethash 'aref h) #'compile-aref-form)
  (setf (gethash '%aref-set h) #'compile-%aref-set-form)
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
          (let ((name-tmp (gen-local "MKSNAME")) (slots-tmp (gen-local "MKSSLOTS")))
            `((:declare-local ,name-tmp "LispObject")
              (:declare-local ,slots-tmp "LispObject[]")
              ,@(compile-value-arg (cadr expr)) (:stloc ,name-tmp)
              ,@(compile-args-array (cddr expr)) (:stloc ,slots-tmp)
              (:ldloc ,name-tmp) (:ldloc ,slots-tmp)
              (:call "Runtime.MakeStruct")))))
  (setf (gethash '%struct-ref h)
        (lambda (expr)
          (let ((obj (first (cdr expr))) (idx (second (cdr expr))))
            (if (integerp idx)
                `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                      (compile-expr obj))
                  (:ldc-i4 ,idx) (:call "Runtime.StructRefI"))
                (compile-binary-call (cdr expr) "Runtime.StructRef")))))
  (setf (gethash '%struct-set h)
        (lambda (expr)
          (let ((obj (first (cdr expr))) (idx (second (cdr expr))) (val (third (cdr expr))))
            (if (integerp idx)
                `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                      (compile-expr obj))
                  (:ldc-i4 ,idx)
                  ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                      (compile-expr val))
                  (:call "Runtime.StructSetI"))
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
  ;; (%slot-def-raw-options slotd options-plist) → slotd
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
  (setf (gethash 'slot-value h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.SlotValue")))
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
          ;; containing a tagbody (loop/dolist — which emit :LEAVE and labels that
          ;; require an empty CIL stack) was otherwise compiled with the class still
          ;; on the stack, producing an unbalanced (stack-underflow) method — invalid
          ;; CIL, e.g. cl-ppcre's (make-instance 'alternation :choices (loop … collect
          ;; …)). A plain call (list/mapcar) never hit this because it loads nothing
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
              `(,@(compile-value-arg (cadr expr)) ,@(emit-fixnum 0) (:call "Runtime.NumEqual"))
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
                (if (and (= (length args) 2)
                         (fixnum-typed-p (first args))
                         (fixnum-typed-p (second args)))
                    (compile-fixmod args op)
                    (compile-binary-call args generic)))))))
  (dolist (op-method '((floor . "Runtime.FloorOp") (truncate . "Runtime.TruncateOp")
                        (ceiling . "Runtime.CeilingOp") (round . "Runtime.RoundOp")))
    (let ((op (car op-method)) (method (cdr op-method)))
      (setf (gethash op h)
            (lambda (expr)
              (let ((args (cdr expr)))
                (cond ((= (length args) 1) (compile-binary-call (list (car args) 1) method))
                      ((= (length args) 2) (compile-binary-call args method))
                      (t (compile-named-call op args))))))))
  (setf (gethash 'min h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (cond ((= (length args) 0) (compile-named-call 'min args))
                  ((= (length args) 1) (compile-named-call 'min args))
                  ((= (length args) 2) (compile-binary-call args "Runtime.Min"))
                  (t (compile-expr (reduce (lambda (a b) `(min ,a ,b)) args)))))))
  (setf (gethash 'max h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (cond ((= (length args) 0) (compile-named-call 'max args))
                  ((= (length args) 1) (compile-named-call 'max args))
                  ((= (length args) 2) (compile-binary-call args "Runtime.Max"))
                  (t (compile-expr (reduce (lambda (a b) `(max ,a ,b)) args)))))))
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
                    (compile-binary-call args "Runtime.Ash"))
                (compile-named-call 'ash args)))))
  (setf (gethash 'lognot h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (cond
              ((and (= (length args) 1) (fixnum-typed-p (first args)))
               (compile-fixbit-not args))
              ((= (length args) 1)
               (compile-unary-call args "Runtime.Lognot"))
              (t (compile-named-call 'lognot args))))))
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
                  (2 (if (and (fixnum-typed-p (first args))
                              (fixnum-typed-p (second args)))
                         (compile-fixbit-binop args cil-op)
                         (compile-binary-call args method2)))
                  (t `(,@(compile-args-array args) (:call ,methodN)))))))))

  ;; I/O
  (setf (gethash 'print h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond ((= nargs 1) `(,@(compile-value-arg (cadr expr)) (:call "Runtime.Print")))
                  ((= nargs 2) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) (:call "Runtime.Print2")))
                  (t (compile-static-program-error (format nil "PRINT: wrong number of arguments: ~a (expected 1-2)" nargs)))))))
  (setf (gethash 'prin1 h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond ((= nargs 1) `(,@(compile-value-arg (cadr expr)) (:call "Runtime.Prin1")))
                  ((= nargs 2) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) (:call "Runtime.Prin12")))
                  (t (compile-static-program-error (format nil "PRIN1: wrong number of arguments: ~a (expected 1-2)" nargs)))))))
  (setf (gethash 'princ h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond ((= nargs 1) `(,@(compile-value-arg (cadr expr)) (:call "Runtime.Princ")))
                  ((= nargs 2) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) (:call "Runtime.Princ2")))
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
  ;; Runtime.OpenStreamP inline, so a user-defined method — e.g. flexi-streams,
  ;; whose open-stream-p delegates to the underlying stream — is dispatched
  ;; instead of the builtin gray-stream default (which always returns T). When no
  ;; user method exists the symbol-function is still the builtin, so the default
  ;; behavior is unchanged.
  (setf (gethash 'open-stream-p h) (lambda (expr) (compile-named-call 'open-stream-p (cdr expr))))
  (setf (gethash 'read-line h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((= nargs 4) (compile-named-call 'read-line (cdr expr)))
              ((= nargs 0) `(,@(compile-value-arg '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadLine")))
              ((= nargs 1) `(,@(compile-value-arg (cadr expr)) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadLine")))
              ((= nargs 2) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(emit-nil) (:call "Runtime.ReadLine")))
              ((= nargs 3) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(compile-value-arg (cadddr expr)) (:call "Runtime.ReadLine")))
              (t (compile-static-program-error (format nil "READ-LINE: wrong number of arguments: ~a (expected 0-4)" nargs)))))))
  (setf (gethash 'read-char h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((= nargs 4) (compile-named-call 'read-char (cdr expr)))
              ((= nargs 0) `(,@(compile-value-arg '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadChar")))
              ((= nargs 1) `(,@(compile-value-arg (cadr expr)) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadChar")))
              ((= nargs 2) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(emit-nil) (:call "Runtime.ReadChar")))
              ((= nargs 3) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(compile-value-arg (cadddr expr)) (:call "Runtime.ReadChar")))
              (t (compile-static-program-error (format nil "READ-CHAR: wrong number of arguments: ~a (expected 0-4)" nargs)))))))
  (setf (gethash 'read-char-no-hang h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((= nargs 4) (compile-named-call 'read-char-no-hang (cdr expr)))
              ((= nargs 0) `(,@(compile-value-arg '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadCharNoHang")))
              ((= nargs 1) `(,@(compile-value-arg (cadr expr)) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadCharNoHang")))
              ((= nargs 2) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(emit-nil) (:call "Runtime.ReadCharNoHang")))
              ((= nargs 3) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(compile-value-arg (cadddr expr)) (:call "Runtime.ReadCharNoHang")))
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
              ((= nargs 0) `(,@(emit-nil) ,@(compile-value-arg '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.PeekChar")))
              ((= nargs 1) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.PeekChar")))
              ((= nargs 2) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(emit-t) ,@(emit-nil) (:call "Runtime.PeekChar")))
              ((= nargs 3) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(compile-value-arg (cadddr expr)) ,@(emit-nil) (:call "Runtime.PeekChar")))
              ((= nargs 4) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(compile-value-arg (cadddr expr)) ,@(compile-value-arg (car (cddddr expr))) (:call "Runtime.PeekChar")))
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
              ((= nargs 0) `(,@(compile-value-arg '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadFromStream")))
              ((= nargs 1) `(,@(compile-value-arg (cadr expr)) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadFromStream")))
              ((= nargs 3) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(compile-value-arg (cadddr expr)) (:call "Runtime.ReadFromStream")))
              (t `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(emit-nil) (:call "Runtime.ReadFromStream")))))))
  (setf (gethash 'read-from-string h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.ReadFromString"))))
  (setf (gethash 'read-preserving-whitespace h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((> nargs 4) (compile-static-program-error (format nil "READ-PRESERVING-WHITESPACE: too many arguments: ~D (expected at most 4)" nargs)))
              ;; With recursive-p: fall through to generic dispatch
              ((= nargs 4) (compile-named-call 'read-preserving-whitespace (cdr expr)))
              ((= nargs 0) `(,@(compile-value-arg '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadPreservingWhitespace")))
              ((= nargs 1) `(,@(compile-value-arg (cadr expr)) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadPreservingWhitespace")))
              ((= nargs 3) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(compile-value-arg (cadddr expr)) (:call "Runtime.ReadPreservingWhitespace")))
              (t `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(emit-nil) (:call "Runtime.ReadPreservingWhitespace")))))))

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
  (setf (gethash 'char h)
        (lambda (expr)
          (if (= (length (cdr expr)) 2)
              (compile-binary-call (cdr expr) "Runtime.CharAccess")
              (compile-named-call 'char (cdr expr)))))
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
  (setf (gethash 'find-symbol h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.FindSymbolL"))))
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

  ;; Higher-order
  (setf (gethash 'apply h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (if (cddr args)
                (compile-expr `(apply ,(car args) (list* ,@(cdr args))))
                (compile-binary-call args "Runtime.Apply")))))
  (setf (gethash 'maphash h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Maphash")))
  (setf (gethash 'mapcar h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (if (cddr args)
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
                    (:call "Runtime.MapcarN"))))
                (compile-binary-call args "Runtime.Mapcar")))))

  ;; Property list
  (setf (gethash 'get h)
        (lambda (expr)
          (let* ((args (cdr expr)) (nargs (length args)))
            (cond
              ((< nargs 2) (compile-static-program-error (format nil "GET: too few arguments: ~D (expected at least 2)" nargs)))
              ((> nargs 3) (compile-static-program-error (format nil "GET: too many arguments: ~D (expected at most 3)" nargs)))
              (t `(,@(compile-value-arg (first args))
                   ,@(compile-value-arg (second args))
                   ,@(if (third args) (compile-value-arg (third args)) (emit-nil))
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
          (let ((test-expr nil)
                (has-other-kw nil))
            (loop for (k v) on (cdr expr) by #'cddr
                  when (and (keywordp k) (string= (symbol-name k) "TEST"))
                    do (setf test-expr v)
                  else when (keywordp k)
                    ;; :SYNCHRONIZED or other keywords → fall back to variadic call
                    do (setf has-other-kw t))
            (cond
              ;; Any keyword other than :test → use variadic function path
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
                     `((:ldstr ,literal-name) (:call "Startup.Keyword") (:call "Runtime.MakeHashTable"))
                     `(,@(compile-value-arg test-expr) (:call "Runtime.MakeHashTable")))))
              (t '((:call "Runtime.MakeHashTable0")))))))
  (setf (gethash 'gethash h) (lambda (expr) (compile-gethash (cdr expr))))
  (setf (gethash 'puthash h) (lambda (expr) (compile-puthash (cdr expr))))
  (setf (gethash 'remhash h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Remhash")))

  ;; Values
  (setf (gethash 'values h) (lambda (expr) (compile-values-call (cdr expr))))
  ;; (%mv-capture FORM) — evaluate FORM keeping its values, stash them in the
  ;; per-thread bind snapshot and leave the primary value. (%mv-nth I) then reads
  ;; the I-th. MULTIPLE-VALUE-BIND uses the pair instead of building a list.
  (setf (gethash '%mv-capture h)
        (lambda (expr)
          `((:call "MultipleValues.Reset")
            ,@(let ((*in-tail-position* nil) (*in-mv-context* t))
                (compile-expr (cadr expr)))
            (:call "MultipleValues.CaptureForBind"))))
  (setf (gethash '%mv-nth h)
        (lambda (expr) (compile-unary-call (cdr expr) "MultipleValues.BindNth")))
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
                  ;; Same shape as MULTIPLE-VALUE-LIST below: keep MV context so
                  ;; the values survive, block tail so a self-tail-call argument
                  ;; does not TCO past the read.
                  ,@(let ((*in-tail-position* nil) (*in-mv-context* t))
                      (compile-expr form))
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
  ;; %dotimes-1+ — increment emitted by the dotimes expansion for a
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

  ;; Special forms
  (setf (gethash 'if h) (lambda (expr) (compile-if (cdr expr))))
  (setf (gethash 'when h) (lambda (expr) (compile-expr `(if ,(cadr expr) (progn ,@(cddr expr))))))
  (setf (gethash 'unless h) (lambda (expr) (compile-expr `(if ,(cadr expr) nil (progn ,@(cddr expr))))))
  (setf (gethash 'cond h) (lambda (expr) (compile-cond (cdr expr))))
  (setf (gethash 'and h) (lambda (expr) (compile-and (cdr expr))))
  (setf (gethash 'or h) (lambda (expr) (compile-or (cdr expr))))
  ;; progn preserves top-level-ness per CLHS 3.2.3.1
  (setf (gethash 'progn h) (lambda (expr) (let ((*at-toplevel* *compile-was-toplevel*)) (compile-progn (cdr expr)))))
  (setf (gethash 'let h) (lambda (expr) (compile-let (cadr expr) (cddr expr) nil)))
  (setf (gethash 'let* h) (lambda (expr) (compile-let-star (cadr expr) (cddr expr))))
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
  ;; %inline-cs-spliced: dotcl-cs:inline-cs macro expansion target.
  ;; Form: (%inline-cs-spliced ((arg1 arg2 ...)) :returns long ((:LDARG-0) ...))
  ;;   ARGN are Lisp value forms (compiled to LispObject Fixnum).
  ;;   The instr-list comes from (dotcl-cs:disassemble-cs ...) at macro
  ;;   expansion time — opcodes like (:LDARG-0) refer to the corresponding
  ;;   ARGN. We unbox each arg to int64, store in a fresh local, translate
  ;;   LDARG-N to ldloc that local, drop the trailing :RET, and box the
  ;;   final stack-top via Fixnum.Make. Only fixnum bindings + fixnum
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
                   (prelude '())
                   (translated '()))
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
              ;; Translate the SIL: LDARG-N → ldloc local-N, drop RET
              (dolist (ins instrs)
                (let ((op (car ins)))
                  (cond
                    ((member op '(:ldarg.0 :ldarg-0))
                     (push `(:ldloc ,(nth 0 arg-locals)) translated))
                    ((member op '(:ldarg.1 :ldarg-1))
                     (push `(:ldloc ,(nth 1 arg-locals)) translated))
                    ((member op '(:ldarg.2 :ldarg-2))
                     (push `(:ldloc ,(nth 2 arg-locals)) translated))
                    ((member op '(:ldarg.3 :ldarg-3))
                     (push `(:ldloc ,(nth 3 arg-locals)) translated))
                    ((eq op :ldarg)
                     (push `(:ldloc ,(nth (cadr ins) arg-locals)) translated))
                    ((member op '(:ldarg.s))
                     (push `(:ldloc ,(nth (cadr ins) arg-locals)) translated))
                    ((eq op :ret)
                     ;; Drop — we splice the result, caller boxes.
                     nil)
                    (t
                     (push ins translated)))))
              (append prelude
                      (nreverse translated)
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
            (compile-expr `(%register-symbol-macro-rt ',name ',expansion)))))
  (dolist (sym '(defvar defparameter defconstant))
    (let ((sym sym))
      (setf (gethash sym h)
            (lambda (expr)
              (compile-defvar (cadr expr) (caddr expr) (not (null (cddr expr)))
                              (eq sym 'defvar) (eq sym 'defconstant) (cadddr expr))))))
  (setf (gethash 'the h) (lambda (expr) (compile-expr (caddr expr))))
  (setf (gethash 'load-time-value h)
        (lambda (expr)
          (let ((ltv-id (incf *ltv-counter*))
                (mod-id *current-module-id*))
            (if mod-id
                ;; Per-module namespaced LTV — prevents cross-run slot ID collisions
                (compile-expr `(if (%has-ltv-slot-in ,mod-id ,ltv-id)
                                   (%get-ltv-slot-in ,mod-id ,ltv-id)
                                   (%set-ltv-slot-in ,mod-id ,ltv-id ,(cadr expr))))
                ;; Legacy path (eval, compile without compile-file context)
                (compile-expr `(if (%has-ltv-slot ,ltv-id)
                                   (%get-ltv-slot ,ltv-id)
                                   (%set-ltv-slot ,ltv-id ,(cadr expr))))))))
  (setf (gethash 'declare h) (lambda (expr) (declare (ignore expr)) (emit-nil)))
  (setf (gethash 'declaim h) #'compile-declaim-form)
  ;; eval-when preserves top-level-ness per CLHS 3.2.3.1
  (setf (gethash 'eval-when h)
        (lambda (expr) (let ((*at-toplevel* *compile-was-toplevel*)) (compile-eval-when (cadr expr) (cddr expr)))))
  (setf (gethash 'multiple-value-call h)
        (lambda (expr)
          (let* ((fn-form (cadr expr)) (arg-forms (cddr expr)) (fn-var (gensym "MVC-FN")))
            (compile-expr
              `(let ((,fn-var ,fn-form))
                 (apply ,fn-var
                        (append ,@(mapcar (lambda (a) `(multiple-value-list ,a)) arg-forms))))))))
  (setf (gethash 'multiple-value-prog1 h)
        (lambda (expr)
          (let* ((first-form (cadr expr)) (rest-forms (cddr expr)) (mv-var (gensym "MVP1")))
            (compile-expr
              `(let ((,mv-var (multiple-value-list ,first-form)))
                 ,@rest-forms
                 (values-list ,mv-var))))))

  nil) ; end compile-form handler registration
