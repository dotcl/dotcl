;;; cil-forms-defun.lisp: defvar / defparameter, defun, typed entries, function bodies
;;; Part of the CIL compiler (A2 instruction list architecture)

(in-package :dotcl.cil-compiler)

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
    ;; (return-from X ...): match if X eq NAME
    ((and (eq (car form) 'return-from)
          (consp (cdr form))
          (eq (cadr form) name))
     t)
    ;; (block NAME ...) shadows our block name: don't descend
    ((and (eq (car form) 'block)
          (consp (cdr form))
          (eq (cadr form) name))
     nil)
    ;; (defun NAME ...) also creates an implicit block: don't descend
    ((and (eq (car form) 'defun)
          (consp (cdr form))
          (eq (cadr form) name))
     nil)
    ;; (quote X) is data, not code: never descend. X may be a circular literal
    ;; (e.g. '#1=(1 2 3 . #1#)); walking it would loop forever.
    ((eq (car form) 'quote) nil)
    ;; Recurse into subforms (handle dotted pairs safely)
    (t (do ((cur form (cdr cur)))
           ((atom cur) (when cur (form-has-return-from-p name cur)))
         (when (form-has-return-from-p name (car cur))
           (return t))))))

(defun %lambda-list-init-forms (lambda-list)
  "The default-value initializer forms in an ordinary LAMBDA-LIST; the only code
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
   LET or LABELS, for instance, reads as (LET <list-form>); indistinguishable
   by shape from a LET whose binding list is a symbol."
  (do ((cur forms (cdr cur)))
      ((not (consp cur)) nil)
    (let ((r (funcall fn (car cur))))
      (when r (return r)))))

(defvar *return-from-scan-memo* nil
  "NIL, or an EQ hash table mapping a form to the smallest macro-expansion
   DEPTH at which FORM-MACROEXPANDS-TO-RETURN-FROM-P already scanned it and found
   no return-from. Bound fresh by the entry caller for one scan (one NAME).")

(defvar *return-from-scan-expansions* nil
  "NIL, or an EQ hash table mapping a macro form to its expansion for one scan.
   Outside compile-file *MACROEXPAND-CACHE* is NIL, so without this every visit
   of a macro form would expand it again into fresh conses and the EQ memo in
   *RETURN-FROM-SCAN-MEMO* could never hit on the expansion's subforms.")

(defun %return-from-scan-macroexpand (form expander)
  ;; The scan does not track the bindings it walks into (the function's own
  ;; parameters included), so an expansion that asks its &ENVIRONMENT about a
  ;; symbol macro is made as an analysis walk's is: kept only for the places
  ;; where the symbol macro names resolve the same way.
  (let ((table *return-from-scan-expansions*)
        (*sm-walker-context* t)
        (*sm-walker-bound* :none))
    (if (null table)
        (cached-macroexpand form expander)
        (multiple-value-bind (expansion found) (gethash form table)
          (if found
              expansion
              (setf (gethash form table) (cached-macroexpand form expander)))))))

(defun form-macroexpands-to-return-from-p (name form depth)
  "Memoizing front of %FORM-MACROEXPANDS-TO-RETURN-FROM-P. The scan looks at both
   a macro call's expansion and its original subforms, and an expansion usually
   embeds those same subforms (EQ), so without memoization every level of nested
   macro calls doubles the work: exponential in macro nesting depth. A negative
   result at depth D stays negative at any depth >= D (less expansion budget
   left), so it is reused; a positive result ends the whole scan anyway."
  (let ((memo *return-from-scan-memo*))
    (if (or (null memo) (atom form))
        (%form-macroexpands-to-return-from-p name form depth)
        (let ((seen (gethash form memo)))
          (if (and seen (>= depth seen))
              nil
              ;; Marked before the scan: an expansion that contains FORM again
              ;; finds it here, and the scan already under way covers it.
              (progn
                (setf (gethash form memo) depth)
                (%form-macroexpands-to-return-from-p name form depth)))))))

(defun %compiler-let-symbol-p (sym)
  "T for DOTCL-CLTL2:COMPILER-LET, matched by name so the compiler needs no
   reference to a package that is created after it loads."
  (and (string= (symbol-name sym) "COMPILER-LET")
       (let ((pkg (symbol-package sym)))
         (and pkg (string= (package-name pkg) "DOTCL-CLTL2")))))

(defun %form-macroexpands-to-return-from-p (name form depth)
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
    ;; nested block/defun shadowing NAME: its own return-from is not ours
    ((and (eq (car form) 'block) (consp (cdr form)) (eq (cadr form) name)) nil)
    ((and (eq (car form) 'defun) (consp (cdr form)) (eq (cadr form) name)) nil)
    ((eq (car form) 'quote) nil)
    ;; Binding special forms: recurse only into their code positions (default
    ;; initializer forms + bodies), never into lambda-list parameter names or
    ;; LET variable names. Blindly recursing into a lambda list macroexpands a
    ;; form like (INST), a required param named after a macro, firing that
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
    ;; COMPILER-LET: its body means something only while its bindings are up,
    ;; and its expansion is the body already fully expanded under them. Scan
    ;; that expansion alone; the structural pass below would expand the body's
    ;; macros a second time with the bindings gone, firing their compile-time
    ;; side effects (series records a restriction warning there) against the
    ;; wrong values.
    ((and (symbolp (car form))
          (%compiler-let-symbol-p (car form))
          (< depth *macro-expand-depth-limit*)
          (gethash (car form) *macros*))
     (handler-case
         (let ((expanded (%return-from-scan-macroexpand form (gethash (car form) *macros*))))
           (form-macroexpands-to-return-from-p name expanded (1+ depth)))
       (error () t)))
    ;; A global macro call may expand to a return-from (possibly after more
    ;; expansions). Local macrolet macros are handled separately by
    ;; form-has-macrolet-p (they are not in the global *macros* table).
    ;; Once the call has expanded, only the expansion is code: an argument the
    ;; macro drops is never compiled, so it is not scanned either. Scanning it
    ;; would run the expanders of macro calls inside it, with their compile-time
    ;; side effects (warnings), which the compilation itself never does.
    ((and (symbolp (car form))
          (< depth *macro-expand-depth-limit*)
          (gethash (car form) *macros*))
     (handler-case
         (let ((expanded (%return-from-scan-macroexpand form (gethash (car form) *macros*))))
           (if (equal expanded form)
               (%scan-code-forms (lambda (f) (form-macroexpands-to-return-from-p name f depth))
                                 (cdr form))
               (form-macroexpands-to-return-from-p name expanded (1+ depth))))
       (error () t)))
    (t
     ;; Structural recursion into the subforms.
     (do ((cur form (cdr cur)))
         ((atom cur) (when cur (form-macroexpands-to-return-from-p name cur depth)))
       (when (form-macroexpands-to-return-from-p name (car cur) depth)
         (return t))))))

(defun form-has-macrolet-p (form)
  "T if FORM contains a MACROLET / SYMBOL-MACROLET anywhere in the tree. Such a
   local macro can expand to (return-from <enclosing-defun> ...); which
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
   NAME contains (case-insensitive), print RESULT, the emitted instruction list,
   which carries the :body SIL, to *error-output*, bracketed by ;;DUMP-SIL /
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
   form at call time exactly where the array XEP would; so (pkg *package*) and
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

(defvar *key-shared-tco* nil
  "(N-REQUIRED . KEYWORDS) while %BUILD-KEY-DIRECT-SPECS compiles a &key
   function's shared body. COMPILE-FUNCTION-BODY-DIRECT takes it on entry (and
   rebinds it to NIL for anything compiled inside), so the body's tail
   self-calls can be spread into the body's positional shape.")

(defvar *key-entry-copy-max-instrs* 150
  "A &key function whose required-only body is at most this many SIL
   instructions keeps a copy of the body in that entry instead of calling the
   shared body (%BUILD-KEY-SHARED-SPECS). The point of sharing is to JIT the body
   once; for a small body that JIT is cheap, and the copy keeps what a call
   loses: a constant default folded into the code.")

(defun %key-shared-tco-args (args n-req keywords)
  "The argument list of the shared body for a tail self-call with ARGS, or NIL
   when the call cannot loop.

   The shared body takes the required arguments and then one argument per key,
   in the order the keys were declared; a key the call does not supply gets the
   absent marker, and the body puts its default in. So (F X) loops, and so does
   (F X :B 1) -- but only when every keyword is a literal naming a key, none
   repeats, and they come in declaration order: the values are then evaluated
   in the order the call wrote them. Anything else (a computed keyword,
   :ALLOW-OTHER-KEYS, another order) is left to the ordinary call."
  (let ((n (length args)))
    ;; The same machinery serves the shared body of an &optional or &rest
    ;; function (%BUILD-POSITIONAL-SHARED-SPECS); KEYWORDS then starts with a
    ;; marker that no keyword list can (it holds keywords only).
    (case (car keywords)
      (%optional
       ;; Optionals are positional: pad the missing ones with the marker.
       (return-from %key-shared-tco-args
         (and (<= n-req n (+ n-req (cdr keywords)))
              (append args (make-list (- (+ n-req (cdr keywords)) n)
                                      :initial-element '(%absent-key))))))
      (%rest
       ;; The rest list is one argument, consed from the extra arguments.
       (return-from %key-shared-tco-args
         (and (>= n n-req)
              (append (subseq args 0 n-req)
                      (list `(list ,@(nthcdr n-req args))))))))
    (when (and (>= n n-req) (evenp (- n n-req)))
      (let ((vals (make-list (length keywords) :initial-element '(%absent-key)))
            (last -1))
        (loop for (kw val) on (nthcdr n-req args) by #'cddr
              for pos = (and (keywordp kw) (position kw keywords))
              do (unless (and pos (> pos last))
                   (return-from %key-shared-tco-args nil))
                 (setf last pos)
                 (setf (nth pos vals) val))
        (append (subseq args 0 n-req) vals)))))

(defun %compile-key-shared-xep (required key allow-other-keys-p fn-name)
  "The array entry of a &key function whose body is shared
   (%BUILD-KEY-SHARED-SPECS): it parses the arguments as the lambda list says,
   with the absent marker as every key's default, and calls the shared body. So
   the body exists once, and a call with several keywords (or through APPLY)
   runs the same code the typed entries do."
  ;; Each key variable is a fresh symbol with the key's own name, so the lambda
  ;; list names the same keyword in the plain form, whose parse looks keywords
  ;; up by name alone.
  (let ((rs (loop repeat (length required) collect (gensym "REQ")))
        (ins (loop for k in key collect (make-symbol (first k)))))
    (compile-function-body
     `(,@rs &key ,@(loop for in in ins collect `(,in (%absent-key)))
            ,@(when allow-other-keys-p '(&allow-other-keys)))
     `((%call-key-shared ,@rs ,@ins))
     fn-name)))

(defun %compile-positional-shared-xep (required optional rest-param fn-name)
  "The array entry of an &optional or &rest function whose body is shared
   (%BUILD-POSITIONAL-SHARED-SPECS): it parses the arguments as the lambda list
   says, with the absent marker as every optional's default, and calls the
   shared body with the optionals, or with the rest list."
  (let ((rs (loop repeat (length required) collect (gensym "REQ")))
        (ins (loop repeat (length optional) collect (gensym "OPT")))
        (rin (gensym "REST")))
    (compile-function-body
     (if rest-param
         `(,@rs &rest ,rin)
         `(,@rs &optional ,@(loop for in in ins collect `(,in (%absent-key)))))
     (if rest-param
         `((%call-key-shared ,@rs ,rin))
         `((%call-key-shared ,@rs ,@ins)))
     fn-name)))

(defun %build-positional-shared-specs (required optional rest-param wrapped-body
                                       fn-name fn-pkg fn-symbol)
  "The typed entries of an &optional function (OPTIONAL non-NIL) or a &rest
   function (REST-PARAM non-NIL) as thin callers of one shared body, or NIL when
   the body is small or cannot be shared (the shared body would call itself
   other than in tail position). With NIL, the second value is the compiled
   required-only arity when it does not call itself (else NIL), for the
   unshared entries to use as is (ARITY0-INSTRS of %BUILD-OPTIONAL-DIRECT-SPECS
   and %BUILD-REST-DIRECT-SPECS). A third value, when non-NIL, is a
   (:SHARED NPARAMS BODY) spec to put in front of those entries: the array
   entry then calls that body instead of carrying a full copy of its own.

   The shape is the one %BUILD-KEY-SHARED-SPECS gives a &key function. The
   shared body takes the required arguments and then one argument per optional,
   the absent marker for an optional the call did not supply; it binds each
   optional in order to its argument or its default. For &rest it takes the rest
   list as one argument, and each typed entry conses that list from exactly its
   extra arguments. Each arity used to carry its own copy of the body, and each
   is JIT-compiled on its first call.

   A small body keeps the old entries: when the copy for the required-only
   arity is at most *KEY-ENTRY-COPY-MAX-INSTRS* SIL instructions, the JIT that
   sharing saves is cheap, and a call to the shared body would cost more than
   it saves. Otherwise the required-only entry carries that copy as a fourth
   element for the in-memory backend, as for &key. A copy that calls itself
   (typically a tail self-call passing an optional, which that arity cannot
   loop on) is not kept, and the body is shared whatever its size: the shared
   body can loop on it."
  (let* ((n (length required))
         (ins (loop repeat (length optional) collect (gensym "OPT")))
         (rin (gensym "REST"))
         (shared-params (if rest-param (append required (list rin)) (append required ins)))
         (shared-body
           (if rest-param
               `((let ((,rest-param ,rin)) ,@wrapped-body))
               `((let* ,(loop for o in optional
                              for in in ins
                              collect (list (car o) `(%key-value ,in ,(second o))))
                   ,@wrapped-body))))
         (copy :none))
    (flet ((compile-copy ()
             ;; The required-only arity, bound exactly as the old entry bound it.
             (multiple-value-bind (instrs copy-self-p)
                 (compile-function-body-direct
                  required
                  (if rest-param
                      `((let ((,rest-param (list))) ,@wrapped-body))
                      `((let* ,(mapcar (lambda (o) (list (car o) (second o))) optional)
                          ,@wrapped-body)))
                  fn-name fn-pkg fn-symbol)
               (unless copy-self-p instrs)))
           (compile-shared ()
             (let ((*key-shared-tco*
                     (if rest-param
                         (list n '%rest)
                         (list* n '%optional (length optional)))))
               (compile-function-body-direct shared-params shared-body
                                             fn-name fn-pkg fn-symbol))))
    (multiple-value-bind (shared-instrs self-p)
        (if (and *compile-file-mode* (not *cross-compiling*))
            ;; A fasl never uses the copy of a shared body (its entry calls
            ;; the shared body and leaves inlining to the JIT), so under
            ;; COMPILE-FILE the shared body comes first and decides by its own
            ;; size: a large one is shared with no copy compiled at all. A
            ;; small one still gets the copy, for the unshared entries, and the
            ;; shared body is not wasted: it backs the array entry (third
            ;; value) in place of a full body.
            (multiple-value-bind (instrs shared-self-p) (compile-shared)
              (cond (shared-self-p
                     (return-from %build-positional-shared-specs
                       (values nil (compile-copy))))
                    ((<= (length instrs) *key-entry-copy-max-instrs*)
                     (setf copy (compile-copy))
                     (when copy
                       (return-from %build-positional-shared-specs
                         (values nil copy
                                 (list :shared (length shared-params) instrs))))))
              (values instrs nil))
            (progn
              (setf copy (compile-copy))
              ;; No copy means the copy calls itself other than in tail
              ;; position (a tail call with more arguments than the arity has
              ;; is one): share then too. When the body is not shared, the
              ;; copy is handed back as the second value: it is the
              ;; required-only arity of the unshared entries, and compiling it
              ;; again there was a third of the compile time of a small
              ;; &optional DEFUN.
              (unless (or (null copy) (> (length copy) *key-entry-copy-max-instrs*))
                (return-from %build-positional-shared-specs (values nil copy)))
              (compile-shared)))
      (when (eq copy :none) (setf copy nil))
      (if self-p
          (values nil copy)
          (cons
           (list :shared (length shared-params) shared-instrs)
           (loop for extra from 0 to (if rest-param 2 (length optional))
                 for arity = (+ n extra)
                 while (<= arity 8)
                 collect
                 (let* ((*key-shared-takes-mode* (%sil-takes-mode-p shared-instrs))
                        (rs (loop repeat arity collect (gensym "ARG")))
                        (passed (if rest-param
                                    `((list ,@(nthcdr n rs)))
                                    (append (nthcdr n rs)
                                            (make-list (- (length optional) extra)
                                                       :initial-element '(%absent-key)))))
                        (thin (compile-function-body-direct
                               rs `((%call-key-shared ,@(subseq rs 0 n) ,@passed)))))
                   (if (and (= extra 0) copy)
                       (list arity nil thin copy)
                       (list arity nil thin))))))))))

(defun %build-key-shared-specs (required key wrapped-body fn-name fn-pkg fn-symbol
                                allow-other-keys-p)
  "The typed entries of a &key function as thin callers of one shared body, or
   NIL when the body cannot be shared.

   The shared body takes the required arguments and then one argument per key.
   Each entry only decides those values: the required-only entry passes the
   absent marker for every key, the one-pair entry passes the value for the key
   it names. The body binds each key to its argument, or to its default when it
   got the marker, in declaration order, so a default still runs at call time
   and still sees the earlier parameters. The entries were each a full copy of
   the body before, and every entry is JIT-compiled on its first call, so a
   function called both ways paid the JIT for its body twice.

   Not shared when the body makes a non-tail call to itself: such a body takes
   the function as a leading argument, which the entries do not have."
  (let* ((n (length required))
         (kws (loop for k in key collect (intern (first k) "KEYWORD")))
         (ins (loop for k in key collect (gensym "KEY")))
         (bindings
           (loop for k in key
                 for in in ins
                 collect (list (second k) `(%key-value ,in ,(third k)))
                 when (fourth k)
                   collect (list (fourth k) `(if (%key-absent-p ,in) nil t)))))
    (multiple-value-bind (shared-instrs self-p)
        (let ((*key-shared-tco* (cons n kws)))
          (compile-function-body-direct (append required ins)
                                        `((let* ,bindings ,@wrapped-body))
                                        fn-name fn-pkg fn-symbol))
      (unless self-p
        (let* ((*key-shared-takes-mode* (%sil-takes-mode-p shared-instrs))
               (rs (loop repeat n collect (gensym "REQ")))
               (kw-var (gensym "KW"))
               (val-var (gensym "KVAL"))
               ;; A full copy of the body for the required-only shape, with each
               ;; key bound straight to its default (so a constant default still
               ;; folds into the code, a NIL tested by IF for one).
               (copy (multiple-value-bind (instrs copy-self-p)
                         (compile-function-body-direct
                          required (%build-key-direct-body required key wrapped-body nil nil)
                          fn-name fn-pkg fn-symbol)
                       (unless copy-self-p instrs))))
          (list
           (list :shared (+ n (length key)) shared-instrs)
           (if (and copy (<= (length copy) *key-entry-copy-max-instrs*))
               ;; Small: the copy is the entry. Its JIT is cheap, and a call to
               ;; the shared body would cost more than the body saves.
               (list n nil copy)
               ;; Otherwise the entry calls the shared body. The copy rides along
               ;; as a fourth element for the in-memory backend (DynamicMethods):
               ;; it never inlines one method into another, so there the call
               ;; would stay a real call on the commonest shape. A fasl ignores
               ;; it; the JIT can inline the call there.
               (list* n nil
                      (compile-function-body-direct
                       rs `((%call-key-shared ,@rs ,@(loop repeat (length key)
                                                           collect '(%absent-key)))))
                      (and copy (list copy))))
           (list (+ n 2) nil
                 (compile-function-body-direct
                  (append rs (list kw-var val-var))
                  `((unless (or ,@(loop for kw in kws collect `(eq ,kw-var ',kw))
                                (eq ,kw-var :allow-other-keys)
                                ,@(when allow-other-keys-p '(t)))
                      (error 'program-error
                             :format-control "~a: unrecognized keyword argument ~s"
                             :format-arguments (list ,fn-name ,kw-var)))
                    (%call-key-shared
                     ,@rs
                     ,@(loop for kw in kws
                             collect `(if (eq ,kw-var ',kw) ,val-var (%absent-key)))))))))))))

(defun %build-key-direct-specs (required key wrapped-body fn-name fn-pkg fn-symbol
                                allow-other-keys-p implicit-keys-p
                                &optional (shared-body wrapped-body))
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
  (when (and implicit-keys-p (<= (+ (length required) 2) 8)
             (<= (+ (length required) (length key)) 32))
    (let ((shared (%build-key-shared-specs required key shared-body fn-name fn-pkg
                                           fn-symbol allow-other-keys-p)))
      (when shared
        (return-from %build-key-direct-specs shared))))
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



(defun %rest-direct-eligible-p (required optional key rest-param aux)
  "T if a required+&rest function can carry typed direct delegates for the call
   shapes that pass a small, fixed number of extra arguments, in addition to the
   array XEP.

   A variadic LispFunction has one entry, taking LispObject[], so every call to a
   &rest function builds an array -- including (F 1), where the rest list is NIL
   and nothing needs collecting at all. On a typed arity the extra arguments
   arrive as real parameters and a LET* binds the rest parameter to a fresh list
   of exactly them, which is what the array entry would have consed. (LIST) with
   no arguments is NIL, so the required-only arity allocates nothing.

   Requires: a rest param, no &optional/&key/&aux (those compose with &rest in
   ways the array entry handles and this shape does not), and required <= 8."
  (and rest-param (null optional) (null key) (null aux)
       (<= (length required) 8)))

(defun %build-rest-direct-specs (required rest-param wrapped-body fn-name fn-pkg fn-symbol
                                 &optional arity0-instrs)
  "((ARITY SELF-P DIRECT-BODY) ...) for the arities of a &rest function that can
   be typed: required + 0, +1 and +2 extra arguments. ARITY0-INSTRS, if given, is
   the required-only arity already compiled (and not calling itself); it is used
   instead of compiling that arity again.

   The cut at 2 is where the shapes stop being common: (F A) and (F A X) are what
   callers write, and each further arity is another copy of the body in the image.
   Anything longer, and APPLY, still go through the array XEP."
  (let ((rn (length required))
        (specs '()))
    (loop for extra from 0 to 2
          for n = (+ rn extra)
          when (<= n 8)
            do (let* ((xs (loop repeat extra collect (gensym "REST")))
                      (direct-params (append required xs))
                      (direct-body `((let ((,rest-param (list ,@xs))) ,@wrapped-body))))
                 (if (and (= extra 0) arity0-instrs)
                     (push (list n nil arity0-instrs) specs)
                     (multiple-value-bind (body-instrs self-p)
                         (compile-function-body-direct direct-params direct-body
                                                       fn-name fn-pkg fn-symbol)
                       (push (list n (if self-p t nil) body-instrs) specs)))))
    (nreverse specs)))
(defun %build-optional-direct-specs (required optional wrapped-body fn-name fn-pkg fn-symbol
                                     &optional arity0-instrs)
  "Build ((ARITY DIRECT-BODY) ...) for each concrete arity N in
   [len(required) .. len(required)+len(optional)]. For arity N the first
   (N - len(required)) optionals are real direct params; the rest are bound to
   their defaults by a wrapping LET*, so the shared body runs identically to the
   array XEP with those optionals defaulted. LET* rather than LET: CL binds
   optional defaults in order and a later one may read an earlier parameter.
   ARITY0-INSTRS, if given, is the required-only arity already compiled (and not
   calling itself); it is used instead of compiling that arity again."
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
                 (if (and (= present 0) arity0-instrs)
                     (values arity0-instrs nil)
                     (compile-function-body-direct direct-params direct-body fn-name fn-pkg fn-symbol))
               ;; A non-tail self-call makes compile-function-body-direct thread
               ;; the function itself as arg0 (params shift to ldarg 1+); the
               ;; install path then builds a method with a leading LispFunction
               ;; self param and binds the fn into the _funcN delegate. SELF-P
               ;; per arity tells HandleDefmethod which signature to emit.
               (push (list n (if self-p t nil) body-instrs) specs)))
    (nreverse specs)))

(defun %sil-portable-var (x)
  (if (and (symbolp x) x (not (keywordp x)))
      (make-symbol (symbol-name x))
      x))

(defun %sil-portable-spec (x)
  ;; (name default [supplied-p]) or ((:key name) default [supplied-p])
  (if (consp x)
      (cons (if (consp (car x))
                (list (caar x) (%sil-portable-var (cadar x)))
                (%sil-portable-var (car x)))
            (cdr x))
      (%sil-portable-var x)))

(defun %sil-portable-walk (ps)
  (cond ((null ps) nil)
        ((atom ps) (%sil-portable-var ps))          ; dotted tail
        ((and (symbolp (car ps)) (car ps)
              (char= (char (symbol-name (car ps)) 0) #\&))
         ;; a lambda-list keyword: keep it, it is part of the syntax
         (cons (car ps) (%sil-portable-walk (cdr ps))))
        (t (cons (%sil-portable-spec (car ps)) (%sil-portable-walk (cdr ps))))))

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
  (%sil-portable-walk params))

(defun defun-runtime-registration-instrs (name params wrapped-body uninterned-fixup)
  "The DEFUN emission for the runtime-registration path: a closure defun (free
   vars captured) or a NON-top-level defun (nested inside a conditional /
   other form). Registers via compile-lambda + RegisterFunctionOnSymbol when
   the form is actually reached; the :defmethod paths register at ASSEMBLY
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
      ;; (fboundp '(setf target)) could not find it: e.g. a non-top-level
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

;;; ------------------------------------------------------------
;;; Typed entries
;;; ------------------------------------------------------------
;;; A global function whose DECLAIM FTYPE gives every argument and the result a
;;; numeric type that has a raw representation -- an integer type that fits
;;; int64 (a Fixnum holds any of them) or DOUBLE-FLOAT -- is compiled into a
;;; method taking and returning the raw values, (:DEFMETHOD-TYPED ...). Its
;;; ordinary entry is a shell the assembler builds: convert the arguments, call
;;; the method, box the result. From (safety 1) up the declaration is an
;;; assertion: the shell checks each argument and the body checks its result.
;;; At (safety 0) both are trusted.
;;;
;;; A kind spec is (:LONG LO HI), LO / HI NIL for the whole int64 range, or
;;; (:DOUBLE).

(defvar *typed-entry-request* nil
  "The (ARG-SPECS . RET-SPEC) DEFUN-DIRECT-INSTRS asks the next
   COMPILE-FUNCTION-BODY-DIRECT to compile a typed body for, or :BOXED-ONLY for
   a body taking and returning objects even where a native body would do (see
   DEFUN-DIRECT-INSTRS). That body takes it and rebinds this to NIL, so nothing
   compiled inside sees it.")

(defun %typed-kind-spec (type)
  "The kind spec of TYPE, the declared type of an argument or result, or NIL
   when values of TYPE have no raw representation here."
  (let ((ty (%typexpand-for-range type)))
    (cond ((member ty '(double-float long-float)) (list :double))
          ((eq ty 'fixnum) (list :long nil nil))
          (t (let ((r (declared-integer-range type)))
               (cond ((not (and r (range-fits-int64-p r))) nil)
                     ((and (= (car r) +int64-min+) (= (cdr r) +int64-max+))
                      (list :long nil nil))
                     (t (list :long (car r) (cdr r)))))))))

(defun %single-value-type (type)
  "The type of the only value a function declared to return TYPE returns:
   TYPE itself, or X of (VALUES X) / (VALUES X &OPTIONAL). NIL for any other
   VALUES type."
  (if (and (consp type) (eq (car type) 'values))
      (let ((rest (cdr type)))
        (and (consp rest) (not (member (car rest) '(&optional &rest)))
             (or (null (cdr rest)) (equal (cdr rest) '(&optional)))
             (car rest)))
      type))

(defun typed-entry-specs (name required)
  "(ARG-SPECS . RET-SPEC) when a DEFUN of NAME with the required parameters
   REQUIRED can have a typed entry: NAME has a declaimed (not inferred) FTYPE
   whose arguments are exactly as many plain types as REQUIRED, at most four, and
   every one of them and the single result type has a kind spec. Else NIL."
  (when (and name (symbolp name) (not *cross-compiling*)
             required (<= (length required) 4))
    (multiple-value-bind (args found) (gethash name *function-arg-types*)
      (multiple-value-bind (ret rfound) (gethash name *function-return-types*)
        (when (and found rfound
                   (not (nth-value 1 (gethash name *inferred-return-type-names*)))
                   (listp args)
                   (= (length args) (length required))
                   (notany (lambda (a) (member a lambda-list-keywords)) args))
          (let ((arg-specs (mapcar #'%typed-kind-spec args))
                (ret-spec (let ((ty (%single-value-type ret)))
                            (and ty (%typed-kind-spec ty)))))
            (when (and ret-spec (every #'identity arg-specs))
              (cons arg-specs ret-spec))))))))

(defun %declared-param-type (param body)
  "The type the leading declarations of BODY give the parameter PARAM: from
   (FIXNUM ..), (DOUBLE-FLOAT ..) or (TYPE T ..), or NIL when none does."
  (let ((n (var-name param)))
    (dolist (form body nil)
      (unless (and (consp form) (eq (car form) 'declare)) (return nil))
      (dolist (decl (cdr form))
        (when (consp decl)
          (multiple-value-bind (ty vars)
              (cond ((member (car decl) '(fixnum double-float)) (values (car decl) (cdr decl)))
                    ((and (eq (car decl) 'type) (consp (cdr decl))) (values (cadr decl) (cddr decl)))
                    (t (values nil nil)))
            (when (and ty (member n vars :test (lambda (a b) (and (symbolp b) (string= a (var-name b))))))
              (return-from %declared-param-type ty))))))))

(defun %calls-self-p (x name)
  "Does the form X contain a form whose operator is NAME?"
  (and (consp x)
       (or (eq (car x) name)
           (loop for tail on x
                 thereis (and (consp tail) (consp (car tail)) (%calls-self-p (car tail) name))))))

(defun inferred-typed-entry-specs (name required body)
  "(ARG-SPECS . RET-SPEC) for a self-recursive DEFUN of NAME without a
   declaimed FTYPE: every required parameter is declared a type with a kind spec
   in BODY, and the return type inferred from BODY (INFER-BODY-RETURN-TYPE,
   recorded before the body is compiled) has one too. The typed entry makes the
   self-calls direct calls of the body, which is allowed unless NAME is NOTINLINE
   (CLHS 3.2.2.3). Else NIL."
  (when (and name (symbolp name) (not *cross-compiling*)
             required (<= (length required) 4)
             (nth-value 1 (gethash name *inferred-return-type-names*))
             (not (member name (extract-notinline body)))
             (%calls-self-p (nth-value 1 (%split-leading-declares body)) name))
    (let ((ret (function-return-type name)))
      (when ret
        (let ((arg-specs (mapcar (lambda (p)
                                   (let ((ty (%declared-param-type p body)))
                                     (and ty (%typed-kind-spec ty))))
                                 required))
              (ret-spec (%typed-kind-spec ret)))
          (when (and ret-spec (every #'identity arg-specs))
            (cons arg-specs ret-spec)))))))

(defun %typed-sig-string (arg-specs ret-spec)
  "The signature the assembler reads (see TypedEntrySig): \"LD>L\"."
  (flet ((ch (s) (if (eq (car s) :double) #\D #\L)))
    (coerce (append (mapcar #'ch arg-specs) (list #\> (ch ret-spec))) 'string)))

(defun %typed-param-conflict-p (required arg-specs body)
  "True when BODY declares a parameter a type whose slot representation differs
   from the one its ftype kind gives it, e.g. DOUBLE-FLOAT for a parameter the
   ftype makes an integer. Such a body is compiled the ordinary way."
  (let ((fx (extract-fixnum-locals body))
        (sml (mapcar #'car (extract-small-int-locals body)))
        (dbl (extract-double-float-locals body))
        (sgl (extract-single-float-locals body))
        (dec (extract-decimal-locals body))
        (chr (extract-character-locals body)))
    (loop for p in required
          for spec in arg-specs
          for n = (var-name p)
          thereis (if (eq (car spec) :double)
                      (or (member n fx :test #'string=) (member n sml :test #'string=)
                          (member n sgl :test #'string=) (member n dec :test #'string=)
                          (member n chr :test #'string=))
                      (or (member n dbl :test #'string=) (member n sgl :test #'string=)
                          (member n dec :test #'string=) (member n chr :test #'string=))))))

(defun %typed-checks-p ()
  "Are typed-entry declarations checked in the body being compiled: (safety 1)
   and up."
  (not (compiling-at-safety-0-p)))

(defun %long-range-check (expr spec)
  "After EXPR's value is on the stack as a raw long: the check that it lies in
   the range SPEC declares, or NIL when SPEC is the whole int64 range, the range
   of EXPR is proved to lie inside it, or declarations are trusted."
  (let ((lo (second spec)) (hi (third spec)))
    (when (and lo (%typed-checks-p)
               (not (let ((r (and expr (expr-int-range expr))))
                      (and r (<= lo (car r)) (<= (cdr r) hi)))))
      `((:ldc-i8 ,lo) (:ldc-i8 ,hi) (:call "Runtime.CheckLongRange")))))

(defun compile-typed-value (expr spec &optional (range-check t))
  "EXPR compiled leaving the raw value SPEC describes, for an argument of a
   typed entry. Native when the typed paths can produce it; otherwise as an
   object, converted with the check the declaration asks for. RANGE-CHECK NIL
   leaves the range of an integer to the callee's body, which checks it on entry
   when it was compiled to (a call from another function: the callee's safety
   decides, as it does for a boxed caller)."
  (let ((*in-tail-position* nil) (*in-mv-context* nil))
    (if (eq (car spec) :double)
        (cond ((typed-self-call-p expr) (compile-typed-self-call expr))
              ((double-float-typed-p expr) (compile-as-double expr))
              (t `(,@(compile-expr expr)
                   ,(if (%typed-checks-p) '(:call "Runtime.TypedArgD") '(:unbox-double)))))
        (if (typed-self-call-p expr)
            (compile-typed-self-call expr)
            `(,@(cond ((fixnum-arith-unboxed-safe-p expr) (compile-as-long expr))
                      ((checked-long-arith-p expr) (compile-checked-long-arith expr))
                      (t `(,@(compile-expr expr)
                           ,(if (%typed-checks-p) '(:call "Runtime.TypedArgL") '(:unbox-fixnum)))))
              ,@(when range-check (%long-range-check expr spec)))))))

(defun %self-call-in-tail-p (x self)
  "Might X reach a call to SELF in its tail position? Looks through the control
   and binding forms; a macro other than those is answered by whether SELF occurs
   in it at all. Errs toward T, which only costs the native lowering of X."
  (and (consp x)
       (let ((op (car x)))
         (cond ((eq op self) t)
               ((not (symbolp op))
                (some (lambda (s) (%self-call-in-tail-p s self)) x))
               ((member op '(progn locally block let let* the if cond when unless case ecase
                             typecase etypecase and or flet labels macrolet symbol-macrolet
                             tagbody return-from return))
                (some (lambda (s) (%self-call-in-tail-p s self)) (cdr x)))
               ((and (find-macro-expander op) (not (special-operator-name-p op)))
                (labels ((occurs (y) (or (eq y self) (and (consp y) (or (occurs (car y)) (occurs (cdr y)))))))
                  (occurs (cdr x))))
               (t nil)))))

(defun compile-typed-result (x spec)
  "X, the value a typed entry returns, leaving the raw result. Native when the
   typed paths can produce it. Otherwise X is compiled as an object in tail
   position, so a self-call in its tail is still the loop back to the top of
   the body, and the value is converted: taken out of a multiple-value return
   and checked to be of the declared kind (and range, from (safety 1) up)."
  (let ((self (first (cstate-typed-self))))
    (if (and (not (typed-self-call-p x)) (%self-call-in-tail-p x self))
        (%typed-result-generic x spec)
        (let ((*in-tail-position* nil) (*in-mv-context* nil))
          (if (eq (car spec) :double)
              (cond ((typed-self-call-p x) (compile-typed-self-call x))
                    ((double-float-typed-p x) (compile-as-double x))
                    (t (%typed-result-generic x spec)))
              (cond ((typed-self-call-p x) (compile-typed-self-call x))
                    ((fixnum-arith-unboxed-safe-p x)
                     `(,@(compile-as-long x) ,@(%long-range-check x spec)))
                    ((checked-long-arith-p x)
                     `(,@(compile-checked-long-arith x) ,@(%long-range-check x spec)))
                    (t (%typed-result-generic x spec))))))))

(defun %typed-result-generic (x spec)
  (let ((*in-tail-position* t) (*in-mv-context* nil))
    `(,@(compile-expr x)
      ,@(if (eq (car spec) :double)
            '((:call "Runtime.TypedRetD"))
            `((:call "Runtime.TypedRetL") ,@(%long-range-check nil spec))))))

(defun typed-self-call-p (expr)
  "True for a call, from the body of a typed entry, to that same function with
   as many arguments as it takes."
  (let ((ts (and (boundp '*cstate*) (cstate-typed-self))))
    (and ts (consp expr) (symbolp (car expr))
         (eq (car expr) (first ts))
         (not (local-function-entry (car expr)))
         (= (length (cdr expr)) (length (second ts))))))

(defun compile-typed-self-call (expr)
  "A call to the typed entry being compiled, leaving its raw result: the
   arguments go into typed temporaries first, so nothing is pending on the stack
   while any of them is computed."
  (let* ((ts (cstate-typed-self))
         (temps (loop for nil in (cdr expr) collect (gen-local "TA"))))
    `(,@(loop for a in (cdr expr)
              for spec in (second ts)
              for tmp in temps
              append `((:declare-local ,tmp ,(if (eq (car spec) :double) "Double" "Int64"))
                       ,@(compile-typed-value a spec)
                       (:stloc ,tmp)))
      (:ldarg 0)
      ,@(loop for tmp in temps collect `(:ldloc ,tmp))
      (:call-typed-self))))

(defun typed-call-specs (name args)
  "(ARG-SPECS . RET-SPEC) when a call of the global function NAME with the
   argument forms ARGS can go through its typed entry: the entry a DEFUN of NAME
   gets under the same declaimed FTYPE (TYPED-ENTRY-SPECS). Else NIL."
  (and args (typed-entry-specs name args)))

(defun compile-typed-call (name args specs)
  "A call of the global function NAME through its typed entry of SPECS, leaving
   the raw result: the arguments go into typed temporaries first (checked
   against the declaration from (safety 1) up, as the boxed entry checks them),
   then the function is fetched from the name on every call. A function there
   without that entry is called the ordinary way by the runtime."
  (let ((temps (loop for nil in args collect (gen-local "TCA"))))
    `(,@(loop for a in args
              for spec in (car specs)
              for tmp in temps
              append `((:declare-local ,tmp ,(if (eq (car spec) :double) "Double" "Int64"))
                       ,@(compile-typed-value a spec nil)
                       (:stloc ,tmp)))
      ,@(compile-fn-sym-lookup name)
      (:castclass "Symbol")
      (:call "CilAssembler.GetFunctionBySymbol")
      ,@(loop for tmp in temps collect `(:ldloc ,tmp))
      (:call-typed-entry ,(%typed-sig-string (car specs) (cdr specs))))))

(defun %box-typed-result (spec)
  "The instruction boxing a raw value of kind SPEC."
  (if (eq (car spec) :double) '((:newobj "DoubleFloat")) '((:call "Fixnum.Make"))))

(defun compile-typed-progn (forms spec)
  "FORMS (leading declarations skipped) as the body of a typed entry: all but
   the last for effect, the last through COMPILE-TYPED-TAIL."
  (let ((forms (nth-value 1 (%split-leading-declares forms))))
    (if (null forms)
        (compile-typed-tail nil spec)
        `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
              (%join-statements
               (loop for form in (butlast forms)
                     collect (cons (compile-and-pop form)
                                   (if (and (consp form) (member (car form) '(%check-simple-decl %check-arg-decl)))
                                       t
                                       :default)))))
          ,@(compile-typed-tail (car (last forms)) spec)))))

(defun %typed-body-boxed-tail-call-p (instrs)
  "True when the typed body INSTRS makes an ordinary call (LispFunction.InvokeN
   or InvokeNM) whose value goes to the body's result only through the
   conversion to the raw kind (Runtime.TypedRetL / TypedRetD). Without the
   typed entry that call would be a tail call (the assembler's FindTailSites
   follows the value the same way: labels, branches, a local stored and loaded
   right back, outside every protected region). Taking the value out of its box
   needs the call to return, so here it is not one, and a mutual recursion
   through it grows the stack. A body without the (:typed-tail-self) mark makes
   no tail calls anyway."
  (when (find :typed-tail-self instrs :key (lambda (i) (and (consp i) (car i))))
    (let* ((cells (coerce instrs 'vector))
           (n (length cells))
           (depth (make-array n))
           (label-at (make-hash-table :test #'equal))
           (d 0))
      (dotimes (i n)
        (let ((ins (aref cells i)))
          (when (consp ins)
            (case (car ins)
              (:label (setf (gethash (string (cadr ins)) label-at) i))
              (:begin-exception-block (incf d))
              (:end-exception-block (decf d)))))
        (setf (aref depth i) d))
      (flet ((invoke-p (ins)
               (and (consp ins) (eq (car ins) :callvirt) (stringp (cadr ins))
                    (let ((s (cadr ins)))
                      (and (> (length s) 19) (string= "LispFunction.Invoke" s :end2 19)
                           (digit-char-p (char s 19))
                           (or (= (length s) 20)
                               (and (= (length s) 21) (char= (char s 20) #\M)))))))
             (follows-to-typed-ret-p (i)
               (let ((j (1+ i)) (steps 0) (held nil))
                 (loop
                   (when (or (>= j n) (> (incf steps) 64)) (return nil))
                   (let* ((ins (aref cells j)) (op (and (consp ins) (car ins))))
                     (cond
                       ((member op '(:label :line :declare-local)) (incf j))
                       ((and (or (eq op :br)
                                 (and (eq op :leave) held (= (aref depth j) 0)))
                             (gethash (string (cadr ins)) label-at))
                        (setq j (gethash (string (cadr ins)) label-at)))
                       ((null held)
                        (cond ((and (eq op :call)
                                    (member (cadr ins) '("Runtime.TypedRetL" "Runtime.TypedRetD")
                                            :test #'equal))
                               (return (= (aref depth j) 0)))
                              ((eq op :stloc) (setq held (cadr ins)) (incf j))
                              (t (return nil))))
                       ((and (eq op :ldloc) (eq (cadr ins) held))
                        (setq held nil) (incf j))
                       (t (return nil))))))))
        (loop for i below n
              thereis (and (= (aref depth i) 0)
                           (invoke-p (aref cells i))
                           (follows-to-typed-ret-p i)))))))

(defun compile-typed-tail (x spec)
  "X, in tail position of a typed entry's body, leaving the raw result SPEC
   describes. The conditionals and binding forms are walked so that each arm
   produces the raw value itself, and a self-call in tail position stays the
   loop back to the top of the body it is anywhere else."
  (let ((op (and (consp x) (symbolp (car x)) (car x))))
    (cond
      ((or (null op) (local-function-entry op) (macrolet-shadowed-p op))
       (compile-typed-result x spec))
      ((eq op 'progn) (compile-typed-progn (cdr x) spec))
      ((and (eq op 'if) (<= 3 (length x) 4))
       (let ((else-label (gen-label "TELSE")) (end-label (gen-label "TEND")))
         `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
               (compile-boolean-branch (cadr x) else-label nil))
           ,@(compile-typed-tail (caddr x) spec)
           (:br ,end-label)
           (:label ,else-label)
           ,@(compile-typed-tail (cadddr x) spec)
           (:label ,end-label))))
      ((and (eq op 'the) (= (length x) 3))
       (compile-typed-tail (caddr x) spec))
      ((and (member op '(let let*)) (consp (cdr x)) (listp (cadr x))
            (%boolean-branchable-let-p (cadr x) (cddr x)))
       (compile-let (cadr x) (cddr x) (eq op 'let*)
                    (lambda (real-body) (compile-typed-progn real-body spec))))
      ((and (eq op 'block) (consp (cdr x)) (symbolp (cadr x)) (cddr x)
            (not (%block-returns-to-p (cadr x) (cddr x))))
       (let* ((name (cadr x))
              (tag (list '#:transparent-block))
              (result (let ((*transparent-blocks*
                              (cons (list* name tag (%block-identity name))
                                    *transparent-blocks*))
                            (*inlining-stack* (inline-guard-stack name)))
                        (catch tag
                          (compile-typed-progn (cddr x) spec)))))
         (if (eq result :transparent-block-aborted)
             (compile-typed-result x spec)
             result)))
      ((typed-self-call-p x)
       ;; In tail position this is normally the loop back to the top of the
       ;; body. When the loop is not available here (inside a handler, say),
       ;; it is an ordinary typed self-call.
       (let ((instrs (let ((*in-tail-position* t) (*in-mv-context* nil))
                       (compile-expr x))))
         (if (and (consp instrs)
                  (member (car (car (last instrs))) '(:br :leave))
                  (eq (cadr (car (last instrs))) (cstate-tco-loop-label)))
             instrs
             (compile-typed-result x spec))))
      ;; COND / WHEN / UNLESS have compile handlers of their own, which would
      ;; stop the walk at the most common shape of a recursive body: they are
      ;; walked as the IFs they mean.
      ((and (member op '(when unless)) (consp (cdr x)))
       (compile-typed-tail (if (eq op 'when)
                               `(if ,(cadr x) (progn ,@(cddr x)) nil)
                               `(if ,(cadr x) nil (progn ,@(cddr x))))
                           spec))
      ((and (eq op 'cond) (every (lambda (c) (and (consp c) (cdr c))) (cdr x)))
       (compile-typed-tail (if (null (cdr x))
                               nil
                               (let ((c (cadr x)))
                                 (if (eq (car c) t)
                                     `(progn ,@(cdr c))
                                     `(if ,(car c) (progn ,@(cdr c)) (cond ,@(cddr x))))))
                           spec))
      ;; Other macros are expanded so their conditionals and bindings are
      ;; walked too.
      ((and (not (special-operator-name-p op))
            (find-macro-expander op)
            (not (gethash op *compile-form-handlers*)))
       (compile-typed-tail (%macroexpand-for-compile x (find-macro-expander op)) spec))
      (t (compile-typed-result x spec)))))

(defun defun-direct-instrs (name params required direct-wrapped-body wrapped-body
                            pkg-spec uninterned-fixup)
  "The DEFUN emission for the direct-params path (simple required-only
   functions): :defmethod-native when the whole function proves fixnum-native,
   else :defmethod-direct (with :self t when the body does a non-tail
   self-call, so the backend threads the LispFunction in as arg0)."
  (let* ((mangled (mangle-name name))
         (param-names (mapcar #'var-name required))
         (pkg-name (cadr pkg-spec))
         ;; Native eligibility: all fixnum params, fixnum return, <=4 params,
         ;; no special-declared params
         (native-eligible
           (and (symbolp name)
                (<= (length required) 4)
                (all-params-fixnum-p params wrapped-body)
                ;; Asked the way COMPILE-FUNCTION-BODY-DIRECT asks, with the
                ;; body's own NOTINLINE declarations in effect: an inferred
                ;; return type does not answer for a NOTINLINE name, the body
                ;; is then compiled as an ordinary one, and the native method
                ;; around it would read its raw arguments as objects.
                (eq 'fixnum (let ((*notinline-functions*
                                    (append (extract-notinline wrapped-body)
                                            *notinline-functions*)))
                              (function-return-type name)))
                (null (fn-body-special-params wrapped-body required))
                (null (remove-if-not #'global-special-p required))
                (null (nth-value 1 (params-needs-boxing direct-wrapped-body required))))))
    (multiple-value-bind (direct-body direct-self-p typed)
        (let ((*typed-entry-request*
                (or (typed-entry-specs name required)
                    (inferred-typed-entry-specs name required wrapped-body))))
          (compile-function-body-direct params direct-wrapped-body mangled pkg-name name))
      ;; A typed body returns the raw value, so a call to an ordinary function
      ;; whose value is the result has to return to it, to be unboxed: it is not
      ;; a tail call, and a recursion through it (this function calling another
      ;; that calls this one back through its boxed entry) grows the stack where
      ;; the function without a typed entry runs in constant space. Such a
      ;; function keeps its tail calls and goes without the typed entry: its
      ;; body is compiled again, taking and returning objects (not the native
      ;; body either, which returns a raw value too). Callers that know the
      ;; FTYPE then reach it through the boxed entry, and check the result.
      (when (and typed (%typed-body-boxed-tail-call-p direct-body))
        (setq native-eligible nil)
        (multiple-value-setq (direct-body direct-self-p typed)
          (let ((*typed-entry-request* :boxed-only))
            (compile-function-body-direct params direct-wrapped-body mangled pkg-name name))))
      `(,(cond
           (typed
            `(:defmethod-typed ,mangled
               ,@pkg-spec
               ,@(when (debug-frames-off-p wrapped-body) '(:no-frame t))
               :lambda-list ,(sil-portable-lambda-list params)
               :params ,param-names
               :sig ,(%typed-sig-string (car typed) (cdr typed))
               :arg-ranges ,(mapcar (lambda (s) (and (eq (car s) :long) (second s)
                                                     (list (second s) (third s))))
                                    (car typed))
               :checks ,(not (safepoints-off-p direct-wrapped-body))
               :body ,direct-body))
           (t
           (if native-eligible
             `(:defmethod-native ,mangled
                ,@pkg-spec
                ,@(when (debug-frames-off-p wrapped-body) '(:no-frame t))
                :lambda-list ,(sil-portable-lambda-list params)
                :params ,param-names
                :body ,direct-body)
             ;; :self t: self-recursive non-native direct fn: the
             ;; backend gives the direct method a leading LispFunction self
             ;; param (threaded for non-tail self-calls, no per-entry lookup).
             (progn
              (%note-same-file-function name required
                                        (and (<= (length required) 4)
                                             (not (debug-frames-off-p wrapped-body))
                                             (not (tail-calls-off-p wrapped-body))))
             `(:defmethod-direct ,mangled
                ,@pkg-spec
                ,@(when direct-self-p '(:self t))
                ,@(when (debug-frames-off-p wrapped-body) '(:no-frame t))
                :lambda-list ,(sil-portable-lambda-list params)
                :params ,param-names
                :body ,direct-body)))))
        ,@uninterned-fixup
        ,@(if (symbolp name)
              (compile-sym-lookup name)
              ;; A non-symbol function name, e.g. (SETF foo), is
              ;; returned by DEFUN as the name itself (the list
              ;; (SETF foo)), NOT a symbol interned from its mangled
              ;; string. Emitting the mangled symbol broke e.g.
              ;; (multiple-value-list (eval `(defun (setf ,g) ...)))
              ;; (ANSI DEFINE-COMPILER-MACRO.4).
              (compile-quoted name))))))

(defun %note-same-file-function (name required eligible)
  (when (and *same-file-functions* (symbolp name))
    (if eligible
        (setf (gethash name *same-file-functions*) (length required))
        (remhash name *same-file-functions*))))

(defun %same-file-call-p (name n-args)
  "Whether a call to NAME with N-ARGS arguments may go straight to the body that
   DEFUN compiled earlier in this file (:CALL-SAME-FILE), when the name still
   holds that function."
  (and *same-file-functions* *compile-file-mode*
       (symbolp name) (symbol-package name)
       (not (eq *in-tail-position* t))
       (eql (gethash name *same-file-functions*) n-args)
       (not (member name *notinline-functions*))
       (not (%global-notinline-p name))
       (not (eql *optimize-debug* 3))))

(defun compile-defun (name params body)
  "Compile (defun name (params) body...) -> :defmethod directive + return symbol."
  ;; xref: collect CALLER->CALLEE edges while this body (and any nested lambda)
  ;; compiles, and prepend a load-time %xref-note registration. The caller must
  ;; be a nameable global: an uninterned gensym defun records nothing.
  (let ((*xref-caller* (and (not *cross-compiling*)
                            (or (and (symbolp name) (symbol-package name) name)
                                (and (consp name) (eq (car name) 'setf) name))))
        (*xref-edges* '()))
    (let* ((instrs (let ((*return-type-self* (and (symbolp name) name)))
                     (%compile-defun-1 name params body)))
           ;; An inline definition recorded by this DEFUN is also written into
           ;; the fasl, so an image that loads it can substitute calls too.
           (inline-note (let ((form (%inline-expansion-proclamation name)))
                          (when form
                            (let ((*xref-caller* nil)
                                  (*in-tail-position* nil)
                                  (*in-mv-context* nil))
                              `(,@(compile-expr form) (:pop))))))
           (instrs (if inline-note (append inline-note instrs) instrs)))
      (let ((note (xref-note-instrs)))
        (if note (append note instrs) instrs)))))

(defun %ftype-arg-declarations (name params body)
  "BODY with (TYPE spec param) declarations for the required parameters NAME's
   declaimed FTYPE gives a type."
  (if (or *cross-compiling* (not (symbolp name)))
      body
      (multiple-value-bind (args found) (gethash name *function-arg-types*)
        (if (not (and found (listp args)))
            body
            (let ((decls (loop for p in (parse-lambda-list params)
                               for a in args
                               until (member a lambda-list-keywords)
                               unless (or (member a '(t *))
                                          (and (symbolp a) (not (%declared-type-known-p a))))
                                 collect `(type ,a ,p))))
              (if decls `((declare ,@decls) ,@body) body))))))

(defun %compile-defun-1 (name params body)
  ;; Pre-pass: infer return type before body compilation so self-calls inside the
  ;; body benefit from the single-value elision path. Every definition is
  ;; inferred afresh: an inferred type describes the definition it came from,
  ;; and a redefinition must not inherit its predecessor's.
  (when (symbolp name)
    (record-inferred-return-type
     name (infer-body-return-type body (mangle-name name))))
  ;; Keep the definition for substitution at call sites when the name is
  ;; proclaimed INLINE. Recorded here, at the point the DEFUN is compiled,
  ;; which is what makes the proclamation have to come first (CLHS 3.2.2.1.3).
  ;; A redefinition overwrites the entry; call sites compiled against the old
  ;; body keep it, which is the licensed behaviour for an inlined function.
  (when (and (or (symbolp name) (%setf-function-name-p name))
             (not *cross-compiling*))
    ;; A body that uses a name bound by an enclosing MACROLET, SYMBOL-MACROLET
    ;; or local function is not recorded: outside that scope it would mean
    ;; something else (see %INLINE-BODY-CAPTURABLE-P).
    ;; A body that uses only MACROLET names is recorded with those MACROLETs
    ;; around it (%INLINE-BODY-WITH-MACROLETS).
    (let ((recorded (and (%global-inline-p name)
                         (if (%inline-body-capturable-p body params)
                             (%inline-body-with-macrolets body params)
                             body))))
      (if recorded
          (setf (gethash name *inline-defs*) (cons params recorded))
          ;; NOTINLINE (or a plain redefinition after the proclamation was taken
          ;; back) must not leave a stale body behind for later call sites.
          (remhash name *inline-defs*))))
  (setq body (%ftype-arg-declarations name params body))
  (multiple-value-bind (required optional key) (parse-lambda-list params)
    (declare (ignore required))
    (when (or optional key)
      (setq body (%unswitch-index-params
                  (append (mapcar #'car optional) (mapcar #'second key)) body))))
  (multiple-value-bind (required optional key rest-param aux allow-other-keys-p has-key-p)
      (parse-lambda-list params)

    (let* ((param-names (mapcar #'var-name required))
           ;; Every variable the lambda list binds in the body. The analysis
           ;; context below must report ALL of them as bound, not just the
           ;; required ones, or a (declare (fixnum X)) on an &optional/&key
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
                             ((consp name) (cadr name)) ; (cas foo) -> block named foo
                             (t name)))
           ;; Establish the enclosing function's parameters and their numeric
           ;; type declarations for the macro expansions performed during this
           ;; analysis pass (and a dummy *locals* so lookup-local reports params
           ;; as bound); the dummy keys never reach emission. Only the top level
           ;; of the body is in this scope: a binding inside the body is not
           ;; established here, so an expansion that depends on the declarations
           ;; (DOTIMES) is not reused at code-gen (*EXPANSION-UNREUSABLE*).
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
               (or ;; Detect (return-from block-name ...) anywhere in the body,
                   ;; expanding global macro calls at any depth (a macro nested in
                   ;; a progn/let/etc. can expand to one: dotcl/dotcl issue 51).
                   (let ((*return-from-scan-memo* (make-hash-table :test 'eq))
                         (*return-from-scan-expansions* (make-hash-table :test 'eq)))
                     (some (lambda (f) (form-macroexpands-to-return-from-p block-name f 0)) body))
                   ;; A MACROLET / SYMBOL-MACROLET local macro can also expand to
                   ;; (return-from block-name ...) via its quasiquoted template, and
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
           ;;
           ;; The declarations stay OUTSIDE the block. They belong to the
           ;; function, not to the block -- CLHS 3.4.11 puts a DEFUN's
           ;; declarations in the lambda, and a BLOCK body takes no declarations
           ;; at all -- and everything downstream that reads the function's
           ;; leading declarations reads this list. Sweeping them inside made
           ;; (optimize (safety 0)) invisible to BODY-DECLARED-SAFETY for every
           ;; function containing a RETURN-FROM. The &aux path below already
           ;; split them out for the same reason.
           (wrapped-body (if (and use-direct (not has-literal-return-from))
                             body
                             (multiple-value-bind (decls rest)
                                 (%split-leading-declares body)
                               `(,@decls (block ,block-name ,@rest)))))
           ;; The direct path binds &aux itself. The array XEP gets them from the
           ;; lambda list, but compile-function-body-direct only knows about required
           ;; params, so a LET* around the body, which is what &aux means, puts them
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
            ;; guarded defun like (unless (fboundp 'x) (defun x ...)) or (if nil
            ;; (defun x ...)) would define X unconditionally, breaking cross-file
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
              (let* ((check-vars (append required (mapcar #'car optional) (mapcar #'second key)
                                         (if rest-param (list rest-param) nil)))
                     (wrapped-body (insert-simple-decl-checks wrapped-body check-vars))
                     (body (insert-simple-decl-checks body check-vars))
                     (direct-specs
                      (when (symbolp name)
                        (cond
                          ((and (%optional-direct-eligible-p required optional has-key-p rest-param aux)
                                (not (%declares-special-p wrapped-body (mapcar #'car optional))))
                           (multiple-value-bind (shared arity0 backing)
                               (%build-positional-shared-specs
                                required optional nil
                                (if has-literal-return-from wrapped-body body)
                                (mangle-name name) (cadr pkg-spec) name)
                             (or shared
                                 (append
                                  (and backing (list backing))
                                  (%build-optional-direct-specs
                                   required optional wrapped-body
                                   (mangle-name name) (cadr pkg-spec) name arity0)))))
                          ;; Same idea for &key: the required-only call is the
                          ;; one that can be typed, and it is the common one.
                          ((and (%key-direct-eligible-p required optional key rest-param aux)
                                (not (%declares-special-p wrapped-body (mapcar #'second key))))
                           (%build-key-direct-specs
                            required key wrapped-body
                            (mangle-name name) (cadr pkg-spec) name
                            allow-other-keys-p
                            (every (lambda (k) (null (fifth k))) key)
                            ;; Without a RETURN-FROM the implicit block has
                            ;; nothing to catch, and leaving it out keeps the
                            ;; body's last form in tail position, so a tail
                            ;; self-call can loop (as on the required-only path).
                            (if has-literal-return-from wrapped-body body)))
                          ;; Same idea for &rest: the extra arguments become real
                          ;; parameters and the rest list is consed from exactly
                          ;; them -- (F 1), where the list is empty, then costs
                          ;; nothing at all.
                          ((and (%rest-direct-eligible-p required optional key rest-param aux)
                                (not (%declares-special-p wrapped-body (list rest-param))))
                           (multiple-value-bind (shared arity0 backing)
                               (%build-positional-shared-specs
                                required nil rest-param
                                (if has-literal-return-from wrapped-body body)
                                (mangle-name name) (cadr pkg-spec) name)
                             (or shared
                                 (append
                                  (and backing (list backing))
                                  (%build-rest-direct-specs
                                   required rest-param wrapped-body
                                   (mangle-name name) (cadr pkg-spec) name arity0)))))))))
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
                   :body ,(cond
                            ((not (and direct-specs (eq (car (first direct-specs)) :shared)))
                             (compile-function-body params wrapped-body (mangle-name name)))
                            (key
                             (%compile-key-shared-xep required key allow-other-keys-p
                                                      (mangle-name name)))
                            (t
                             (%compile-positional-shared-xep required optional rest-param
                                                             (mangle-name name)))))
                  ,@uninterned-fixup
                  ,@(if (symbolp name)
                        (compile-sym-lookup name)
                        (compile-quoted name))))))))))))

(defun %check-defun-name (name)
  "Signal TYPE-ERROR unless NAME is a function name: a symbol or (SETF symbol).
  Anything else (a string, for one) used to be accepted by DEFUN, or fail later
  by accident inside the compiler. Shared by the compiled and interpreted DEFUN."
  (unless (or (symbolp name)
              (and (consp name) (eq (car name) 'setf)
                   (consp (cdr name)) (symbolp (cadr name)) (null (cddr name))))
    (error 'type-error :datum name
                       :expected-type '(or symbol (cons (eql setf) (cons symbol null))))))

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
  (%check-defun-name (cadr expr))
  (unless *cross-compiling*
    (let ((note (find-symbol "%NOTE-DEFINITION" "DOTCL")))
      (when (and note (fboundp note))
        (funcall note 'function (cadr expr)))))
  (when (and *compile-was-toplevel* *compile-file-mode*
             (not *cross-compiling*))
    (try-eval expr))
  ;; Redefining a name that DEFSTRUCT registered as a keyword constructor drops
  ;; the call-site construction: the new definition decides what the call means.
  ;; The structure's own constructor DEFUN precedes its registration, so this
  ;; never clears the entry the same expansion is about to make.
  (when (symbolp (cadr expr))
    (remhash (cadr expr) *struct-keyword-ctors*)
    (remhash (cadr expr) *typed-vector-struct-accessors*))
  ;; CLHS 3.4.11: extract docstring (first form if string AND more forms follow).
  ;; Skip during cross-compile because cil-stdlib's own defuns may precede
  ;; (setf documentation) GF definition; runtime user defuns are fine.
  (let* ((name (cadr expr))
         (params (caddr expr))
         (body (cdddr expr))
         ;; A (SETF FOO) name carries its docstring too; the form's value is
         ;; then the quoted name, as COMPILE-DEFUN leaves it.
         (has-docstring (and (not *cross-compiling*)
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
          ,@(if (symbolp name)
                (compile-sym-lookup name)
                (compile-quoted name)))
        defun-instrs)))

(defun compile-defmacro (name lambda-list body)
  "Compile (defmacro name lambda-list body...).
   Uses SBCL's eval to create macro function at compile time.
   At runtime, just returns the macro name as a symbol."
  ;; Package lock check: only fire when the macro isn't already defined.
  ;; If *macros* already has this key (e.g. bootstrap CL macros like DEFMETHOD),
  ;; the registration below is a no-op: checking the lock would cause false errors
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
         ;; lexical macro / symbol-macro scope: a (macros-ht . symbol-macros-ht)
         ;; cons that macroexpand-1/macroexpand read (mirrors the macrolet env
         ;; builder, cil-compiler.lisp). Previously this was NIL, so a macro doing
         ;; (macroexpand-1 'sym env) inside a symbol-macrolet saw no binding;
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
         ;; scope so (macroexpand-1 'sym env) inside a symbol-macrolet expands;
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
         ;; CLHS 3.2.3.1: under COMPILE-FILE only a top-level DEFMACRO has a
         ;; compile-time effect. One nested in UNLESS, LET, a function body and
         ;; so on defines the macro when it runs, not when it is compiled.
         ;; Registering it here made a guard like
         ;; (unless (fboundp 'm) (defmacro m ...) (defun helper ...)) see M
         ;; already defined when the compile-time evaluation of that very
         ;; form tested it, so the body was skipped and HELPER never defined.
         ;;
         ;; The expander is compiled the first time something expands a call.
         ;; Most of these registrations are replaced before any expansion: a
         ;; top level DEFMACRO in COMPILE-FILE is also run at compile time, and
         ;; LOAD runs the form right after compiling it, and both install the
         ;; 2-arg expander below. Compiling the expander here as well cost as
         ;; much again as compiling the DEFMACRO. While cross-compiling the host
         ;; needs the function at once (see below), so it is made right away.
         ;; Inside a MACROLET or SYMBOL-MACROLET the expander is made at once, as
         ;; before: the local macros it may use are gone by the first expansion.
         (expander-fn (when (or *compile-was-toplevel* (not *compile-file-mode*))
                        (if (or *cross-compiling* *macroexpand-scope*)
                            (handler-case (if *cross-compiling*
                                              (eval eval-form)
                                              (%eval-at-compile-time eval-form))
                              (error () nil))
                            (%deferred-macro-expander eval-form)))))
    ;; Only register if eval succeeded and no existing entry
    (when expander-fn
      (let ((mkey (macro-key-for-symbol name)))
        (if (gethash mkey *macros*)
            ;; Defined again in this file: a top level DEFUN still waiting to be
            ;; evaluated (see Runtime.TryEval) that uses this macro is evaluated
            ;; now, with the definition in effect where it was written.
            (when (and *compile-file-mode* (not *cross-compiling*) (symbolp name))
              (%force-deferred-defuns-mentioning name))
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

(defun %deferred-macro-expander (eval-form)
  "A 1-arg macro expander that makes the real one by evaluating EVAL-FORM the
   first time it is called."
  (let ((fn nil) (making nil))
    (lambda (form)
      (unless fn
        ;; Making the expander compiles the macro's own body, which can ask to
        ;; expand a call of the macro again.
        (when making
          (error "The expander of ~s is used while it is being made." (car form)))
        (setq making t)
        (unwind-protect (setq fn (eval eval-form))
          (setq making nil)))
      (funcall fn form))))

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
   whose expansion is (progn (in-package X) (defstruct ...) (in-package Y) ...),
   the shape SBCL's force-delayed-defbangstructs uses to replay delayed DEF!STRUCTs,
   interned every accessor name in the package that was current when the macrolet
   form was read, not in X and Y. Accessor names are built at macroexpansion time and
   interned in *PACKAGE*, so the IN-PACKAGE has to have run by then.

   A file's toplevel forms get this from the loader, which splits a toplevel PROGN and
   compiles+runs each piece in turn. That split does not reach inside MACROLET, and
   EVAL of a whole (progn (in-package X) ...) never gets it at all; so the ordering
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
   During cross-compilation: :compile-toplevel/:execute -> eval in SBCL.
   During compile-file: :compile-toplevel -> eval at compile time (CLHS 3.2.3.1),
     :load-toplevel -> emit CIL for load.
   During load/eval: only :execute -> emit CIL.
   When :execute at runtime, eval the defining forms individually first
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
    ;; Load/eval: best-effort try-eval of the defining forms, only with :execute.
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
         ;; LOAD of source and EVAL process only :EXECUTE (CLHS 3.8): without
         ;; it the body is discarded, so it must not define anything. With
         ;; it, the definitions are made before the body is compiled as one
         ;; unit, so that later forms in the body can expand macros that read
         ;; them.
         (when ex-p
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
    ;;   fasl load code: see EVAL-WHEN.1).
    ;; - Cross-compiling (dotcl bootstrap convention): :load-toplevel OR :execute
    ;;   at top level -> emit CIL. The core build relies on this; keep it.
    ;; - Load/eval (not cross-compiling, not compile-file): only :execute -> emit CIL.
    ;; - Not at top level: only :execute -> emit body as implicit progn.
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
  "Names (strings) of ALL-PARAMS that are special; either declared special in
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

(defun inline-sequence-predicate-call (op args)
  "For (OP (LAMBDA (X) BODY...) SEQ), OP one of POSITION-IF, FIND-IF,
   COUNT-IF and their -IF-NOT forms, a loop computing the same value with
   BODY substituted for each element, or NIL when the call is not that shape.
   The loop is written with its own block and tags, so a RETURN in BODY still
   means what it meant inside the lambda."
  (when (= (length args) 2)
    (let ((fn (first args)) (seq (second args)))
      (when (and (consp fn) (eq (car fn) 'function) (consp (cdr fn)) (null (cddr fn)))
        (setf fn (cadr fn)))
      (when (and (consp fn) (eq (car fn) 'lambda)
                 (not (local-function-entry 'lambda))
                 (consp (cdr fn))
                 (let ((ll (cadr fn)))
                   (and (consp ll) (null (cdr ll)) (symbolp (car ll)) (car ll)
                        (not (member (car ll) lambda-list-keywords))
                        (not (constantp (car ll))))))
        (let* ((var (car (cadr fn)))
               (body (cddr fn))
               (s (gensym "SEQ")) (l (gensym "L")) (i (gensym "I")) (n (gensym "N"))
               (e (gensym "E")) (c (gensym "C")) (blk (gensym "SEQBLK"))
               (top (gensym "TOP")) (top2 (gensym "TOP"))
               (negate (member op '(position-if-not find-if-not count-if-not)))
               (kind (case op
                       ((position-if position-if-not) :position)
                       ((find-if find-if-not) :find)
                       (t :count)))
               (test (let ((call `(let ((,var ,e)) ,@body)))
                       (if negate `(not ,call) call)))
               (hit (ecase kind
                      (:position `(return-from ,blk ,i))
                      (:find `(return-from ,blk ,e))
                      (:count `(setq ,c (+ ,c 1)))))
               (done (if (eq kind :count) c nil)))
          `(let ((,s ,seq) (,i 0) (,c 0))
             (declare (fixnum ,i ,c) (ignorable ,c))
             (block ,blk
               (if (listp ,s)
                   (let ((,l ,s) (,e nil))
                     (tagbody
                        ,top
                        (when (endp ,l) (return-from ,blk ,done))
                        (setq ,e (car ,l) ,l (cdr ,l))
                        (when ,test ,hit)
                        (setq ,i (+ ,i 1))
                        (go ,top)))
                   (let ((,n (length ,s)) (,e nil))
                     (declare (fixnum ,n))
                     (tagbody
                        ,top2
                        (when (>= ,i ,n) (return-from ,blk ,done))
                        (setq ,e (aref ,s ,i))
                        (when ,test ,hit)
                        (setq ,i (+ ,i 1))
                        (go ,top2)))))))))))

(defun %split-leading-declares (body)
  "Return (values declare-forms rest). Declarations at the head of a function body
   are about its PARAMETERS, (declare (special x)) on a parameter binds it
   dynamically, so a wrapper inserted around the body must go under them, not
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
                 ;; so it can't contain (:ldloc KEY). Don't descend: walking a
                 ;; cyclic constant loops forever.
                 ((eq (car x) :load-const) nil)
                 (t (walk (car x)) (walk (cdr x)))))))
    (walk tree)
    nil))

(defun %sil-subst-self-arg0 (tree key)
  "Replace every (:ldloc KEY) leaf in SIL TREE with (:ldarg 0). KEY is the
   unique self-fn gen-local, so this rewrites exactly the non-tail self-call
   receivers to read the self LispFunction threaded in as arg0."
  ;; Recurses on the CAR only and walks the CDR chain in a loop: an
  ;; instruction list is long, and recursing down it made the depth grow with
  ;; the size of the function, which overflowed the stack of a thread smaller
  ;; than the main one. The result is the same as the plain recursive walk,
  ;; including sharing: an unchanged tail is returned as is.
  (flet ((stop-p (x)
           ;; A tail the recursive walk would not descend into by CAR/CDR.
           (or (atom x)
               (and (eq (car x) :ldloc) (consp (cdr x))
                    (eq (cadr x) key) (null (cddr x)))
               ;; (:load-const OBJ) holds an opaque (possibly circular) data
               ;; literal; never a self-call receiver. Leave it untouched.
               (eq (car x) :load-const)))
         (finish (x)
           (if (and (consp x) (eq (car x) :ldloc)) '(:ldarg 0) x)))
    (let ((spine '()) (tail tree))
      (loop until (stop-p tail)
            do (push tail spine)
               (setq tail (cdr tail)))
      (let ((result (finish tail)))
        (dolist (cell spine result)
          (let ((a (%sil-subst-self-arg0 (car cell) key)))
            (setq result
                  (if (and (eq a (car cell)) (eq result (cdr cell)))
                      cell
                      (cons a result)))))))))

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
   not already present. Required-only helper; a typed direct delegate that skips
   the variadic UNION :test XEP (InvokeSlow) on the special-param analysis hot
   path. Callers use the result as a set, so order/dup match UNION well enough."
  (let ((r b))
    (dolist (x a r) (unless (%eq-member-p x r) (push x r)))))

(defun %strings-in-both (a b)
  "Elements of string list A that are STRING= to some element of B (INTERSECTION
   by STRING=, keeping A's elements/order). A boxing-analysis hot path per compile;
   a required-only helper skips the variadic INTERSECTION :test XEP (InvokeSlow)."
  (let ((r '()))
    (dolist (x a (nreverse r)) (when (%string-member-p x b) (push x r)))))

(defun %strings-minus (a b)
  "Elements of string list A not STRING= to any element of B (SET-DIFFERENCE by
   STRING=). Companion to %strings-in-both, same rationale."
  (let ((r '()))
    (dolist (x a (nreverse r)) (unless (%string-member-p x b) (push x r)))))

(defun params-needs-boxing (body all-params)
  "Names of params that must be boxed: mutated AND captured in BODY (single
   walk), minus special params; those are bound on the dynamic
   stack, not in a box.

   Second value: the names captured by a closure at all, boxed or not. A
   captured parameter cannot take a native slot representation even when it is
   never assigned, because the environment a closure captures stores objects.
   Third value: the names assigned anywhere in BODY."
  (let* ((mc (multiple-value-list
              (find-mutated-and-captured-vars body (mapcar #'var-name all-params))))
         (mutated (first mc))
         (captured (second mc)))
    (values
     (%strings-minus
      (%strings-in-both mutated captured)
      (special-param-name-set body all-params))
     captured
     mutated)))

(defun %symbol-macros-without (names)
  "*SYMBOL-MACROS* without the entries a new variable binding of one of NAMES
   shadows (CLHS 3.4.2), compared by name as the LET case does."
  (let ((ns (mapcar #'var-name names)))
    (if (some (lambda (entry)
                (let ((k (car entry)))
                  (and (symbolp k) (member (var-name k) ns :test #'string=))))
              *symbol-macros*)
        (remove-if (lambda (entry)
                     (let ((k (car entry)))
                       (and (symbolp k) (member (var-name k) ns :test #'string=))))
                   *symbol-macros*)
        *symbol-macros*)))

(defun params-shadowed-symbol-macros (all-params)
  "Value for rebinding *symbol-macros* in a function body: lambda-list
   parameters shadow an enclosing symbol-macro of the same name (CLHS 3.4.2),
   including inside nested lambdas. Without this a self-referential
   symbol-macro whose name is a param; e.g. serapeum define-env-method's
   (self (slot-value self 'self)); is still live when a NESTED lambda's
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

(defun %bind-supplied-p-instrs (sp-var sp-key value-instrs)
  "Store the supplied-p value VALUE-INSTRS pushes into SP-VAR's local. A
   supplied-p variable that is both assigned and captured by a closure lives in
   a box like any other such parameter, so it has to be created as one: storing
   the bare T/NIL where the closure expects the box made the closure fail with
   an invalid cast (series' LATCH does (SETQ POST-P T) and then closes over it)."
  (if (boxed-var-p sp-var)
      (let ((tmp (gen-local "SPTMP")))
        `((:declare-local ,tmp "LispObject")
          ,@value-instrs
          (:stloc ,tmp)
          ,@(emit-box-create sp-key (list (list :ldloc tmp)) sp-var)))
      `((:declare-local ,sp-key "LispObject")
        ,@value-instrs
        (:stloc ,sp-key))))

(defun %defaults-cstate (defaults-locals-fn outer-boxed-vars names)
  (flet ((masked-p (x)
           (member (if (symbolp x) (var-name x) x) names :test #'string=)))
    (cstate-with *cstate*
      +cs-locals+ (funcall defaults-locals-fn names)
      +cs-boxed-vars+ (append (remove-if #'masked-p (cstate-boxed-vars))
                              (remove-if-not #'masked-p outer-boxed-vars)))))

(defun compile-args-param-instrs (required optional key-specs rest-param
                                  locals-alist n-required key-start
                                  arg-elem-fn args-array-instrs
                                  defaults-locals-fn &optional outer-boxed-vars)
  "Parameter-binding instructions for an args-array function body. Shared by
   compile-function-body-inner and compile-closure-body, which differ only in
   three context points:
   ARG-ELEM-FN; (lambda (i) instrs) pushing args[i] (inner: ldarg
     args-arg-idx + ldelem; closure: :load-arg, which the assembler maps per
     closure mode).
   ARGS-ARRAY-INSTRS; instrs pushing the args array itself (for ldlen /
     FindKeyArg / CollectRestArgs).
   DEFAULTS-LOCALS-FN; (lambda (remaining-param-names) locals-value): the
     *locals* in effect while compiling an &optional/&key default form. Per
     CL, the current and all not-yet-initialized params must not be visible
     (a default referencing a later param name as a special variable must
     read the dynamic binding); the closure variant also keeps env captures
     visible so defaults see the outer scope.
   OUTER-BOXED-VARS; the boxed variables of the enclosing scope, which a
     default sees under the names of the params it masks."
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
         (remaining-opt-names (mapcar #'car optional))
         ;; The &rest and &key variables are bound after every optional, so an
         ;; optional's default must not see them either: it refers to the
         ;; enclosing binding of the same name.
         (later-param-names (append (when rest-param (list rest-param))
                                    (mapcar #'second key-specs))))
     (loop for (opt-name opt-default sp-var) in optional
           for i from n-required
           for key = (cdr (assoc opt-name locals-alist))
           do ;; Mask current+later opt params while compiling the default
              (let* ((boxed-p (boxed-var-p opt-name))
                     (*cstate* (%defaults-cstate defaults-locals-fn outer-boxed-vars
                                                 (mapcar #'var-name
                                                         (append remaining-opt-names
                                                                 later-param-names))))
                     (default-label (gen-label "OPTDEF"))
                     (done-label (gen-label "OPTDONE")))
                (setq opt-instrs
                      (append opt-instrs
                              (if boxed-p
                                  (let ((tmp (gen-local "OPTTMP")))
                                    `((:declare-local ,tmp "LispObject")
                                      ,@args-array-instrs (:ldlen) (:conv-i4)
                                      (:ldc-i4 ,(1+ i))
                                      (:blt ,default-label)
                                      ,@(funcall arg-elem-fn i)
                                      (:br ,done-label)
                                      (:label ,default-label)
                                      ,@(if opt-default
                                            (compile-for-single-value opt-default)
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
                                          (compile-for-single-value opt-default)
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
                                  (%bind-supplied-p-instrs
                                   sp-var sp-key
                                   `(,@args-array-instrs (:ldlen) (:conv-i4)
                                     (:ldc-i4 ,(1+ i))
                                     (:blt ,sp-found-label)
                                     ,@(emit-t)
                                     (:br ,sp-done-label)
                                     (:label ,sp-found-label)
                                     ,@(emit-nil)
                                     (:label ,sp-done-label)))))))))
     opt-instrs)
   ;; Rest param: collect args[N..], bound after required+optional and before
   ;; &key (CLHS 3.4.1), so a &key default form can refer to it
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
             ,@(%local-var-marker key rest-param)))))
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
                     (let ((*cstate* (%defaults-cstate defaults-locals-fn outer-boxed-vars
                                                       (mapcar #'var-name remaining-key-vars))))
                       (compile-for-single-value key-default))
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
                           (%bind-supplied-p-instrs
                            sp-var sp-key
                            `(,@args-array-instrs (:ldc-i4 ,key-start)
                              (:ldstr ,key-name)
                              ,@(when explicit-key-pkg `((:ldstr ,explicit-key-pkg)))
                              (:call ,find-key-fn)
                              (:brtrue ,sp-found-label)
                              ,@(emit-nil)
                              (:br ,sp-done-label)
                              (:label ,sp-found-label)
                              ,@(emit-t)
                              (:label ,sp-done-label))))))))
       ;; After initializing, this key param is now visible to subsequent defaults
       (pop remaining-key-vars))
     key-instrs)))

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
   params; hence HAS-KEY-P) without &allow-other-keys; explicit-package
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

(defun direct-param-instrs (required local-keys arg0-offset native-p &optional long-keys)
  "Parameter-binding instructions for the direct calling convention (params
   arrive as real .NET arguments). ARG0-OFFSET is 1 when arg0 carries the
   self LispFunction (native bodies, and the self-as-arg0 rewrite), else 0.
   NATIVE-P stores Int64 params into raw long slots; native bodies never
   box, so that variant has no boxed-var branch.

   LONG-KEYS (only for the non-native variant) are slots that hold a raw Int64
   although the argument still arrives as a LispObject: the argument is unboxed
   once here and every read in the body is raw. See the long-param selection in
   COMPILE-FUNCTION-BODY-DIRECT for which parameters qualify."
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
            else if (member key long-keys :test #'equal)
              append `((:declare-local ,key "Int64")
                       (:ldarg ,i)
                       ,(if (%typed-checks-p) '(:call "Runtime.TypedArgL") '(:unbox-fixnum))
                       (:stloc ,key)
                       ,@(%local-var-marker key p :long))
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

(defun %labelfn-local-sym (name-str)
  "The *LOCALS* symbol a LABELS function's box is bound to. CL is a Lisp-2, so
   the cell is kept under a __LABELFN_ prefix and a variable of the same name
   can coexist with it."
  (compiler-name-sym (concatenate 'string "__LABELFN_" name-str)))

(defun %sil-takes-mode-p (instrs)
  "True when INSTRS (a compiled body) take a value mode as their first
   instruction, the shape %MV-MODE-PROLOGUE emits."
  (loop for i in instrs
        unless (and (consp i) (member (car i) '(:declare-local :line)))
          return (and (consp i) (eq (car i) :call)
                      (equal (cadr i) "MultipleValues.TakeMode"))))

(defun %mv-mode-prologue (mode-key body-instrs fn-symbol)
  "The entry that takes the caller's value mode into MODE-KEY, or NIL when
   BODY-INSTRS never consult it. It must be the body's first instruction: the
   assembler marks only such a body as an entry that takes a mode
   (CilAssembler.BodyReadsMvMode). Records FN-SYMBOL as a function a call can
   pass the primary-value mode to."
  (when (and mode-key (%sil-references-local-p body-instrs mode-key))
    (when (and fn-symbol (symbolp fn-symbol))
      (setf (gethash fn-symbol *mv-tail-functions*) t))
    `((:declare-local ,mode-key "Int32")
      (:call "MultipleValues.TakeMode")
      (:stloc ,mode-key))))

(defun %record-inline-return-range (name body)
  "Called where the DEFUN of NAME compiles BODY, with the body's declarations in
   force. When NAME's definition was recorded for inline substitution, record in
   *INLINE-RETURN-RANGES* the integer range of the form that gives the result:
   the last form, looked for inside the implicit BLOCK when nothing returns from
   it. Otherwise remove any earlier entry. ironclad's byte-order readers
   (UB16REF/LE, UB32REF/BE ...) are declared inline but not their result type;
   with the range, a word read from octets is a known integer where it is used."
  (remhash name *inline-return-ranges*)
  (when (and (symbolp name) name (gethash name *inline-defs*))
    (let* ((forms (loop for r on body
                        unless (and (consp (car r)) (eq (caar r) 'declare))
                          return r))
           (last-form (car (last forms))))
      (when (and (consp last-form) (eq (car last-form) 'block)
                 (consp (cdr last-form)) (eq (cadr last-form) name)
                 (null (cdr forms)))
        (setq last-form
              (and (cddr last-form)
                   (notany (lambda (f) (form-has-return-from-p name f)) (cddr last-form))
                   (car (last (cddr last-form))))))
      (when last-form
        (let ((r (let ((*int-type-memo* nil)) (expr-int-range last-form))))
          (when r
            (setf (gethash name *inline-return-ranges*) r)))))))

(defun compile-function-body-direct (params body &optional (fn-name "") fn-pkg fn-symbol)
  "Compile function body with direct parameter passing (no args array).
   Only for functions with exactly required params, no optional/key/rest.
   Params are accessed via (:ldarg 0), (:ldarg 1), ... directly.
   FN-PKG, if given, is the defining package name; used by the self-call
   symbol-lookup cache.
   FN-SYMBOL, if given, is the defun symbol; used for native eligibility check."
  (let ((*lexical-decl-frames* (%push-decl-frame :lambda params body)))
    (%compile-function-body-direct-1 params body fn-name fn-pkg fn-symbol)))

(defun %compile-function-body-direct-1 (params body fn-name fn-pkg fn-symbol)
  (warn-unknown-declared-types body)
  (multiple-value-bind (required optional key rest-param aux) (parse-lambda-list params)
    (declare (ignore optional key rest-param aux))
    (setq body (%simd-param-rebind (insert-simple-decl-checks body required) required))
    (let* ((all-params required)
           ;; Set by %BUILD-KEY-DIRECT-SPECS for exactly this body; a function
           ;; compiled inside it must not see it.
           (key-shared *key-shared-tco*)
           (*key-shared-tco* nil)
           ;; Set by DEFUN-DIRECT-INSTRS for exactly this body, like the above.
           (typed-req *typed-entry-request*)
           (*typed-entry-request* nil)
           ;; Fresh scope tables. local-functions is normally empty (a defun body
           ;; sees no lexical local functions): but during speculative
           ;; labels-self-TCO compilation, inject THIS function's own box as a
           ;; local-function: a self-reference that becomes a TCO branch never
           ;; consults it, but one that does NOT (non-tail, arity mismatch, inside a
           ;; try region, #'g, an optimizer gate) falls to the local-fn path and emits
           ;; a (:ldloc box-key) the acceptance scan detects: forcing the safe
           ;; closure-path fallback. Keyed off fn-name so nested inner lambdas
           ;; (fn-name "") never pick it up.
           (spec-entry (let ((spec (cstate-labels-direct-speculation)))
                         (and spec (string= fn-name (car spec)) spec)))
           (*cstate* (cstate-with *cstate*
                       +cs-locals+ '() +cs-boxed-vars+ '()
                       +cs-block-tags+ '() +cs-go-tags+ '()
                       +cs-local-functions+
                       (if spec-entry
                           (list (list (car spec-entry) (cdr spec-entry) t))
                           '())))
           ;; NOTINLINE in this body disables matching compiler macros (CLHS 3.2.2.1.1).
           ;; One made around this function (LOCALLY, an enclosing body) still holds.
           (*notinline-functions* (append (extract-notinline body) *notinline-functions*))
           ;; The rest of the closure-boundary reset set. Bound AFTER
           ;; *local-functions* above, which is the one deliberate reader of
           ;; the labels-direct-speculation slot: the speculation is consulted
           ;; at the boundary and must not stay visible inside the body.
           ;; NOT reset: the tco-self-symbol and tco-local-fn-key slots are
           ;; parameters the CALLER hands in through its *CSTATE* binding;
           ;; compile-defun passes the defun symbol, the labels path passes the
           ;; self-TCO key: and the self-call fast path in this body reads them.
           (*cstate* (cstate-with *cstate*
                                  +cs-labels-direct-speculation+ nil
                                  +cs-native-double-locals+ nil
                                  +cs-native-single-locals+ nil
                                  +cs-native-decimal-locals+ nil
                                  +cs-native-char-locals+ nil
                                  +cs-u64-locals+ nil
                                  +cs-simd-locals+ nil
                                  +cs-dotnet-typed-locals+ nil
                                  +cs-tco-in-try-catch+ nil
                                  +cs-tco-leave-instrs+ nil))
           ;; Try-region context belongs to the enclosing method.
           (*in-finally-block* nil)
           (local-keys (gen-param-local-keys all-params))
           (param-box-info (multiple-value-list (params-needs-boxing body all-params)))
           (needs-boxing (first param-box-info))
           (captured-params (second param-box-info))
           (mutated-params (third param-box-info))
           ;; A typed entry (TYPED-ENTRY-SPECS): every parameter arrives raw and
           ;; lives in a raw slot, so none may be captured or special, and the
           ;; body's own declarations must not give one a different representation.
           (typed (and (consp typed-req) fn-symbol
                       (null needs-boxing) (null captured-params)
                       (= (length required) (length (car typed-req)))
                       (null (fn-body-special-params body all-params))
                       (null (remove-if-not #'global-special-p all-params))
                       (not (%typed-param-conflict-p required (car typed-req) body))
                       typed-req))
           ;; Pre-check native eligibility: all fixnum params, fixnum return, no captures
           ;; Full check (including no specials) happens after special-param-syms is computed,
           ;; but we need this early for param-instrs type selection.
           (param-fx-decl (extract-fixnum-locals body))
           ;; Parameters declared a bounded integer type inside int64 ((MOD N),
           ;; (UNSIGNED-BYTE 32), a DEFTYPE such as an INDEX): the declaration
           ;; covers every value the parameter ever holds, assignments included,
           ;; so the raw slot is as sound for them as for FIXNUM. COMPILE-LET
           ;; promotes its bindings on the same two lists.
           (param-int-decl (append param-fx-decl
                                   (mapcar #'car (extract-small-int-locals body))))
           ;; Only asked for by the two selections below, and each of them asks
           ;; only after a cheaper test has passed; most bodies pass neither.
           (pre-special-syms
             (when (or (and fn-symbol (null needs-boxing)
                            (all-params-fixnum-p params body))
                       param-int-decl)
               (%union-eq (fn-body-special-params body all-params)
                          (remove-if-not #'global-special-p all-params))))
           (pre-native-eligible
             (and fn-symbol
                  (not typed)
                  (not (eq typed-req :boxed-only))
                  (null needs-boxing)
                  (null captured-params)
                  (all-params-fixnum-p params body)
                  (null pre-special-syms)
                  (eq 'fixnum (function-return-type fn-symbol))))
           ;; Required parameters whose SLOT holds a raw Int64 while the method
           ;; still receives its arguments as LispObject. A FIXNUM declaration on
           ;; a parameter is the same promise a FIXNUM declaration on a LET
           ;; binding makes, so the parameter gets the same representation the
           ;; long-rep LET bindings get (%COMPILE-LET's FX-REP-NAMES): unbox once
           ;; on entry, read raw everywhere after. Without it a declared
           ;; parameter is unboxed at every native read -- once per iteration
           ;; when it is a loop bound that the loop cannot hoist, which is what a
           ;; DO end test does with (>= i n).
           ;;
           ;; Excluded: special parameters (bound on the dynamic stack), boxed or
           ;; captured ones (a closure's environment stores objects, so the slot
           ;; must hold the box), and -- the point of %NATIVE-INT-USE-P --
           ;; parameters the body never reads in a native integer position, where
           ;; a raw slot would only add a Fixnum.Make to every generic read.
           ;; A native body needs none of this: its arguments already arrive raw.
           (long-param-keys
             (unless (or pre-native-eligible typed)
               (let ((fx-decl param-int-decl))
                 (when fx-decl
                   (loop for p in required
                         for key = (cdr (assoc p local-keys))
                         when (and key
                                   (member (var-name p) fx-decl :test #'string=)
                                   (not (member (var-name p) needs-boxing :test #'string=))
                                   (not (member (var-name p) captured-params :test #'string=))
                                   (not (member p pre-special-syms))
                                   ;; An assigned parameter pays too: its SETQ
                                   ;; and INCF lower to a raw store. Asked of
                                   ;; the analysis, which sees through macros
                                   ;; (MACROLET included) where the syntactic
                                   ;; scan does not.
                                   (or (member (var-name p) mutated-params
                                               :test #'string=)
                                       (%native-int-use-p p body)))
                           collect key))))))
      ;; During labels self-TCO speculation the box is also a *locals* entry,
      ;; under the same __LABELFN_ name the enclosing body used, after the
      ;; parameters so a same-named parameter still shadows it. Free-variable
      ;; analysis reads *locals*, not *local-functions*, so without the entry a
      ;; self-call from inside a NESTED closure is not free: the closure
      ;; captures nothing, resolves the name globally at run time, and the
      ;; acceptance scan finds no (:ldloc box-key) to reject. With it the
      ;; capture emits that load here and the speculation is refused, which is
      ;; the only safe answer -- the lifted self-TCO loop is a branch in THIS
      ;; method and a closure cannot reach it.
      (let ((*cstate* (cstate-with *cstate*
                        +cs-locals+ (if spec-entry
                                        (append local-keys
                                                (list (cons (%labelfn-local-sym (car spec-entry))
                                                            (cdr spec-entry))))
                                        local-keys)
                        +cs-boxed-vars+ (let ((pb (params-boxed-vars all-params needs-boxing)))
                                          (if spec-entry
                                              (append pb (list (%labelfn-local-sym
                                                                (car spec-entry))))
                                              pb))
                        +cs-block-tags+ (%lift-param-block-tags all-params local-keys)
                        +cs-no-safepoint+ (safepoints-off-p body)))
            (*lift-block-tags* nil)
            (*symbol-macros* (params-shadowed-symbol-macros all-params)))
        (let ((param-instrs
                ;; Native body: arg0 is the self LispFunction (threaded for
                ;; self-calls), so the long params start at ldarg 1.
                (cond
                  (typed
                   ;; Typed body: arg0 is the self LispFunction, then the raw
                   ;; arguments, each into a slot of its own representation.
                   (loop for p in required
                         for spec in (car typed)
                         for key = (cdr (assoc p local-keys))
                         for i from 1
                         append (if (eq (car spec) :double)
                                    `((:declare-local ,key "Double")
                                      (:ldarg ,i) (:stloc ,key)
                                      ,@(%local-var-marker key p :double))
                                    `((:declare-local ,key "Int64")
                                      (:ldarg ,i)
                                      ;; The declared range, checked here from
                                      ;; (safety 1) up: a caller that knows the
                                      ;; FTYPE passes the raw argument straight
                                      ;; in (the shell checks it too, for its
                                      ;; error message).
                                      ,@(%long-range-check nil spec)
                                      (:stloc ,key)
                                      ,@(%local-var-marker key p :long)))))
                  (pre-native-eligible
                   (direct-param-instrs required local-keys 1 t))
                  (t (direct-param-instrs required local-keys 0 nil long-param-keys)))))
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
            ;; CIL's plain Br can't escape it (needs Leave). Boxed params are fine;
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
                   ;;   the sentinel :ARG0: self-call sites load (:ldarg 0) and NO prelude
                   ;;   symbol-lookup runs per recursive entry.
                   ;; - Labels functions are stored in boxes, not symbols: skip.
                   (*cstate* (cstate-with *cstate*
                               +cs-tco-self-name+ (if use-tco fn-name nil)
                               +cs-tco-loop-label+ (if use-tco tco-loop-label nil)
                               +cs-labels-mutual-tco+ nil
                               +cs-self-fn-local+
                               (cond ((and (or pre-native-eligible typed) use-tco) :arg0)
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
                   (*cstate* (if (and use-tco key-shared)
                                 (cstate-with *cstate* +cs-tco-key-shared+
                                              (cons (cstate-tco-param-entries) key-shared))
                                 *cstate*))
                   ;; Function body last form is in tail position:
                   ;; - TCO rewrite applies only when the tco-self-name slot is set
                   ;; - MV return propagation: tail form doesn't unwrap MvReturn
                   ;; No reset at function ENTRY is needed on this path: nothing
                   ;; compiles before this binding, so the value inherited from
                   ;; the caller is never observed (see inline-tail-probe.lisp;
                   ;; at inline call sites that inheritance is what makes an
                   ;; inlined tail call ride the caller's TCO loop).
                   (*in-tail-position* t)
                   ;; The value mode the caller passed (see MultipleValues.TakeMode),
                   ;; taken on entry into an Int32 local that a tail (VALUES ...)
                   ;; consults. Not for a native body (long arguments) or one that
                   ;; binds special parameters (its body runs inside a try).
                   (mv-mode-key (when (and *mv-mode-elision*
                                           (not pre-native-eligible)
                                           (not typed)
                                           (null special-param-syms))
                                  (gen-local "MVMODE")))
                   (*mv-mode-local* mv-mode-key)
                   (*mv-expect-args* nil)
                   ;; Fixnum type declarations on params: consulted by fixnum-typed-p
                   ;; and compile-as-long for native int64 paths.
                   (*fixnum-locals* (append (extract-fixnum-locals body)
                                            (drop-shadowed-type-locals
                                             (mapcar #'var-name all-params)
                                             *fixnum-locals*)))
                   (*character-locals* (append (extract-character-locals body)
                                               (drop-shadowed-type-locals
                                                (mapcar #'var-name all-params)
                                                *character-locals*)))
                   ;; Bounded-integer type declarations on params (signed-byte/
                   ;; unsigned-byte/bit) -> tight range gating native int64 arith.
                   (*cstate* (cstate-with *cstate* +cs-small-int-locals+
                                          (append (rekey-by-param
                                                   (extract-small-int-locals body)
                                                   all-params local-keys)
                                                  (cstate-small-int-locals))))
                   ;; Double-float type declarations on params, and the
                   ;; DOUBLE-FLOAT parameters of a typed body.
                   (*double-float-locals* (append (and typed
                                                       (loop for p in required
                                                             for spec in (car typed)
                                                             when (eq (car spec) :double)
                                                               collect (var-name p)))
                                                  (extract-double-float-locals body)
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
                   ;; Numeric-array type declarations on params/locals (e.g.
                   ;; (simple-array double-float (*)) / (simple-array fixnum (*)))
                   ;; -> aref rides the raw r8 / int64 element path.
                   (*cstate* (cstate-with *cstate* +cs-numeric-array-locals+
                                          (append (extract-numeric-array-locals body)
                                                  (cstate-numeric-array-locals))))
                   ;; (simple-array <integer type> (*)) declarations whose element
                   ;; buffer can be fetched once here instead of per element.
                   (backing-entries (extract-array-backing-locals body))
                   (*cstate* (cstate-with *cstate* +cs-array-backing-locals+
                                          (append backing-entries
                                                  (cstate-array-backing-locals))))
                   ;; Structure-typed declarations whose raw slot array can be
                   ;; fetched once here instead of re-derived per slot access.
                   (struct-backing-entries (extract-struct-backing-locals body))
                   (*cstate* (cstate-with *cstate* +cs-struct-backing-locals+
                                          (append struct-backing-entries
                                                  (cstate-struct-backing-locals))))
                   ;; Native body: params are Int64, enabling compile-as-long without
                   ;; unbox and native self-calls via InvokeNativeN.
                   (*cstate* (cstate-with *cstate*
                               +cs-long-locals+
                               (cond
                                 (typed
                                  (append (loop for p in required
                                                for spec in (car typed)
                                                when (eq (car spec) :long)
                                                  collect (cdr (assoc p local-keys)))
                                          (cstate-long-locals)))
                                 ((and pre-native-eligible use-tco)
                                  (mapcar (lambda (p) (cdr (assoc p local-keys)))
                                          all-params))
                                 (t (append long-param-keys (cstate-long-locals))))
                               +cs-native-self-name+
                               (cond (typed nil)
                                     ((and pre-native-eligible use-tco) fn-name)
                                     (t (cstate-native-self-name)))
                               +cs-typed-self+
                               (and typed (list fn-symbol (car typed) (cdr typed)))))
                   ;; A typed body's DOUBLE-FLOAT parameters sit in raw r8 slots,
                   ;; and an integer parameter of a narrower type than FIXNUM
                   ;; has that range for as long as nothing assigns it.
                   (*cstate* (if typed
                                 (cstate-with *cstate*
                                   +cs-native-double-locals+
                                   (append (loop for p in required
                                                 for spec in (car typed)
                                                 when (eq (car spec) :double)
                                                   collect (cdr (assoc p local-keys)))
                                           (cstate-native-double-locals))
                                   +cs-small-int-locals+
                                   (append (loop for p in required
                                                 for spec in (car typed)
                                                 when (and (eq (car spec) :long) (second spec)
                                                           (not (member (var-name p) mutated-params
                                                                        :test #'string=)))
                                                   collect (cons (cdr (assoc p local-keys))
                                                                 (cons (second spec) (third spec))))
                                           (cstate-small-int-locals)))
                                 *cstate*))
                   ;; If we'll wrap in try/finally for DynamicBindings.Pop, body
                   ;; compilation must know: so tail-position calls don't emit
                   ;; the `.tail` prefix (illegal in CIL inside try) and TCO
                   ;; branches don't try to cross the try boundary.
                   (*in-try-block* (or *in-try-block* (not (null special-param-syms))))
                   (body-instrs (progn
                                  (when (and fn-symbol (not *cross-compiling*))
                                    (%record-inline-return-range fn-symbol body))
                                  (unswitch-array-backing backing-entries body
                                    (if typed
                                        (lambda () (compile-typed-progn body (cdr typed)))
                                        (lambda () (compile-progn body)))
                                    struct-backing-entries)))
                   ;; Extend the native self-as-arg0 threading to NON-native direct
                   ;; functions. The per-entry self-fn-prelude (load-sym->castclass->
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
                   ;; most functions have none: so for most functions the prelude
                   ;; is dead. It cannot be dropped by the JIT (GetFunctionBySymbol
                   ;; is an opaque call), so it has to be dropped here.
                   (mv-prologue (%mv-mode-prologue mv-mode-key body-instrs fn-symbol))
                   (self-fn-used-p (and (cstate-self-fn-local)
                                        (not (eq (cstate-self-fn-local) :arg0))
                                        (%sil-references-local-p body-instrs
                                                                 (cstate-self-fn-local))))
                   (self-arg0-p (and (not pre-native-eligible)
                                     self-fn-used-p
                                     (null special-param-syms)))
                   (eff-param-instrs
                     (if self-arg0-p
                         (direct-param-instrs required local-keys 1 nil long-param-keys)
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
                      ,@(array-backing-prologue backing-entries)
                      ,@(struct-backing-prologue struct-backing-entries)
                      ,@(compile-let-with-specials '() special-push-instrs body-instrs special-param-syms)
                      (:ret))
                    `(,@mv-prologue
                      ,@(%frame-enter-instrs fn-name)
                      ,@eff-param-instrs
                      ,@eff-self-fn-prelude
                      ,@(when use-tco `((:label ,tco-loop-label)))
                      ,@(array-backing-prologue backing-entries)
                      ,@(struct-backing-prologue struct-backing-entries)
                      ,@(cond ((not typed) (%tail-self-instrs fn-name body eff-body-instrs))
                              ;; A self-call whose value is the result is made a
                              ;; tail call to the body method by the assembler.
                              ;; NAME: the frame the body runs under, as
                              ;; for (:tail-self ...); its calls to other
                              ;; typed entries in tail position are tail
                              ;; calls too.
                              ((not (tail-calls-off-p body))
                               `((:typed-tail-self
                                  ,(if (or (equal fn-name "") (debug-frames-off-p body))
                                       nil
                                       fn-name)))))
                      ,@eff-body-instrs
                      (:ret))))
               self-arg0-p
               typed))))))))

(defun wrap-aux-body (aux body)
  "Wrap BODY in (let* AUX ...) for &aux parameters. If BODY is a single
   (block name . forms), push the let* INSIDE the block so leading
   (declare (special x)) stays directly in the let* body; otherwise it gets
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
  (let ((*lexical-decl-frames* (%push-decl-frame :lambda params body)))
    (%compile-function-body-inner-1 params body args-arg-idx fn-name)))

(defun %compile-function-body-inner-1 (params body args-arg-idx fn-name)
  (warn-unknown-declared-types body)
  (multiple-value-bind (required optional key rest-param aux allow-other-keys-p has-key-p) (parse-lambda-list params)
    (let ((body (wrap-aux-body
                 aux
                 (insert-simple-decl-checks
                  body (append required (mapcar #'car optional) (mapcar #'second key)
                               (if rest-param (list rest-param) nil))))))
    (let* ((key-supplied-p-vars (remove nil (mapcar #'fourth key)))
           (opt-supplied-p-vars (remove nil (mapcar #'third optional)))
           (all-params (append required
                               (mapcar #'car optional)
                               opt-supplied-p-vars
                               (mapcar #'second key)
                               key-supplied-p-vars
                               (if rest-param (list rest-param) nil)))
           ;; NOTINLINE in this body disables matching compiler macros for calls
           ;; within it (CLHS 3.2.2.1.1). The declaration is lexical, so one made
           ;; around this function (an enclosing body or LOCALLY) stays in force.
           (*notinline-functions* (append (extract-notinline body) *notinline-functions*))
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
                        +cs-no-safepoint+ (safepoints-off-p body)))
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
                  ;; An args-array body is no direct entry: it takes no value mode.
                  (*mv-mode-local* nil)
                  (*mv-expect-args* nil)
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
                    ,@(%tail-self-instrs fn-name body body-instrs)
                    ,@body-instrs
                    (:ret))))))))))))
