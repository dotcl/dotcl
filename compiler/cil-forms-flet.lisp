;;; cil-forms-flet.lisp: macrolet, symbol-macrolet, flet / labels
;;; Part of the CIL compiler (A2 instruction list architecture)

(in-package :dotcl.cil-compiler)

(defun %macrolet-lexical-operators (macro-defs)
  "*LEXICAL-OPERATORS* extended with the MACROLET bindings of MACRO-DEFS, whose
   expanders have just been registered in *MACROS*."
  (let ((new '()))
    (dolist (def macro-defs)
      (when (symbolp (car def))
        (push (cons (car def) (gethash (car def) *macros*)) new)))
    (nreconc new *lexical-operators*)))

(defun %flet-lexical-operators (names)
  "*LEXICAL-OPERATORS* extended with the FLET / LABELS names NAMES that hide a
   macro (the list %FLET-MACRO-SHADOWS returns)."
  (append (mapcar #'list names) *lexical-operators*))

(defun %compile-scope-mini-env ()
  "The %MINI-EVAL environment a MACROLET expander is made in: the enclosing
   MACROLET and SYMBOL-MACROLET bindings of the code being compiled.
   CLHS MACROLET: the expander is defined in the lexical environment of the
   MACROLET form, so those local macros are visible in its body. The compiler
   keeps them in *MACROS* / *SYMBOL-MACROS* only while it compiles the body, so
   an expander that expanded them when it ran already worked; but a closure the
   expander makes and returns (it ends up in the expansion as a literal) runs
   later, after they are gone, and saw a local macro call as a call to an
   undefined function. Put in the environment, the closure keeps them."
  (let ((env '()) (seen '()))
    (dolist (sb *symbol-macros*)
      (let ((name (car sb)))
        (unless (or (member name seen :test #'eq) (lookup-local name))
          (push name seen)
          (push (cons name (list *%mini-symbol-macro-marker* (cdr sb))) env))))
    (let ((macros '()) (seen-ops '()))
      (dolist (op *lexical-operators*)
        (let ((name (car op)))
          (unless (member name seen-ops :test #'eq)
            (push name seen-ops)
            ;; (NAME) is an FLET binding hiding a macro: nothing to expand.
            (when (cdr op)
              (push (cons name (cdr op)) macros)))))
      (when macros
        (push (cons '%mini-macros (nreverse macros)) env)))
    (nreverse env)))

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
                   (expander-fn (%expander-with-caller-env
                                 (%mini-eval eval-form (%compile-scope-mini-env))
                                 params)))
              (setf (gethash name *macros*) expander-fn))))))
    ;; Compile body with local macros active
    ;; Handle (declare (special ...)) in body: remove those vars from *locals*
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
                (*lexical-decl-frames* (%push-decl-frame :macrolet macro-defs body))
                (*macrolet-source-defs* (cons macro-defs *macrolet-source-defs*))
                (*macrolet-shadowed* (append (mapcar #'car macro-defs)
                                             *macrolet-shadowed*))
                (*lexical-operators* (%macrolet-lexical-operators macro-defs)))
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
         ;; x x), where the accessor var name equals the instance-form variable,
         ;; read the instance itself, not (acc instance).
         (*cstate* (cstate-with *cstate* +cs-locals+
                                (remove-locals-shadowed-by (mapcar #'car bindings)
                                                           (cstate-locals))))
         ;; Push this symbol-macrolet onto *macroexpand-scope* (like compile-macrolet)
         ;; so a macro call form shared by a splicing macro between this scope and an
         ;; outer/sibling scope is cached separately per scope. Without this, a
         ;; recursive macro that nests symbol-macrolet and splices its body into both
         ;; if-arms (serapeum %with-boolean) reuses the outer-scope expansion for the
         ;; inner-scope form: the inner symbol-macro binding is lost. BINDINGS
         ;; is the source cons the analysis walk pushes too, keeping the passes in sync.
         (*macroexpand-scope* (cons bindings *macroexpand-scope*))
         (*lexical-decl-frames* (%push-decl-frame :symbol-macrolet bindings body)))
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
                    (native-slot-p sym (cstate-native-decimal-locals))
                    (native-slot-p sym (cstate-native-char-locals))
                    (native-slot-p sym (cstate-u64-locals))
                    (simd-native-local sym))
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
                   ;; A quoted constant is data: a symbol inside it names
                   ;; nothing, so descending is both pointless and unsafe. A
                   ;; literal read with #n= can be circular, and this walk has
                   ;; no cycle detection -- it ran off the stack instead.
                   ;; %SYMBOL-ONLY-IN-OPERATOR-POSITION-P, the FLET-side walker,
                   ;; already stops here, which is why FLET never showed it.
                   ((eq (car x) 'quote) t)
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

(defun make-local-inline-plan (name params fn-body)
  "The plan MAYBE-EXPAND-LOCAL-INLINE substitutes at a call site, or NIL when
   this local function cannot be substituted at all. Built where the FLET or
   LABELS is compiled, so the snapshot it takes is the definition's own
   environment -- the whole point of the plan.

   Refused for a lambda list that is not all-required (an &optional/&key/&rest
   expansion would have to replicate defaulting rules; the ordinary call already
   does), for a body past *LOCAL-INLINE-BODY-SIZE-LIMIT*, and for a body that
   names itself -- a substituted copy would call the binding it is inside."
  (when (and name
             (symbolp name)
             (every #'symbolp params)
             (notany (lambda (p) (member p lambda-list-keywords)) params)
             (%labels-self-free-p name fn-body)
             (< (%form-size fn-body (1+ *local-inline-body-size-limit*))
                (1+ *local-inline-body-size-limit*)))
    (let ((decls '())
          (rest fn-body)
          ;; NAME is excluded along with the parameters. A self-free body can
          ;; still name itself in a RETURN-FROM, which names the implicit
          ;; block -- and the expansion rebuilds that block itself, so the
          ;; name means there exactly what it meant here. Left in, it would
          ;; refuse every early-returning local function, which is most of
          ;; them: at the call site the name is bound to the very binding
          ;; being substituted, and where the body was written it was not.
          (snapshot (%local-inline-snapshot fn-body (cons name params))))
      (when (eq snapshot :refuse)
        (return-from make-local-inline-plan nil))
      ;; The leading declarations stay OUTSIDE the block: they belong to the
      ;; binding form's body, and (block name (declare ...)) is not a
      ;; declaration position (same reason as MAYBE-EXPAND-INLINE).
      (loop while (and (consp rest) (consp (car rest)) (eq (caar rest) 'declare))
            do (push (pop rest) decls))
      (list name params (nreverse decls) rest
            ;; The implicit block only has to be built where something returns
            ;; from it -- the same test the closure path uses to decide.
            (some (lambda (f) (form-has-return-from-p name f)) rest)
            snapshot
            ;; How many call sites took a copy.
            (list 0)))))


(defun %nlx-local-fn-p (name fn-body &optional (depth 0))
  "T when FN-BODY, the body of local function NAME, returns from a named block
   outside itself, or calls a substitutable local function that does. Called as
   its own method, such a RETURN-FROM can only leave by throwing."
  (let ((self (local-fn-block-name name)))
    (labels ((walk (x inner)
               (cond ((atom x) nil)
                     ((eq (car x) 'quote) nil)
                     ((and (eq (car x) 'return-from) (consp (cdr x))
                           (cadr x) (symbolp (cadr x))
                           (not (eq (cadr x) self))
                           (not (member (cadr x) inner :test #'eq))
                           (assoc (cadr x) (cstate-block-tags)))
                      t)
                     ((and (eq (car x) 'block) (consp (cdr x)))
                      (walk-list (cddr x) (cons (cadr x) inner)))
                     ((and (symbolp (car x)) (car x) (< depth 4)
                           (not (eq (car x) name))
                           (let ((plan (nth 5 (local-function-entry (car x)))))
                             (and plan (%nlx-local-fn-p (first plan) (fourth plan)
                                                        (1+ depth))))))
                     (t (walk-list x inner))))
             (walk-list (l inner)
               (do ((r l (cdr r)))
                   ((atom r) nil)
                 (when (walk (car r) inner) (return t)))))
      (walk-list fn-body '()))))

(defvar *nlx-inline-body-size-limit* 200
  "Largest local function body, in conses, substituted only because it returns
   from an outer block.")

(defun %implicit-inline-names (fn-defs body)
  "The names of FN-DEFS to substitute at their call sites: those BODY declares
   INLINE, and those %NLX-LOCAL-FN-P picks unless BODY declares them NOTINLINE."
  (let ((names (extract-inline body))
        (notinline (extract-notinline body)))
    (dolist (d fn-defs names)
      (when (and (symbolp (car d)) (car d)
                 (not (member (car d) names :test #'eq))
                 (not (member (car d) notinline :test #'eq))
                 (< (%form-size (cddr d) *nlx-inline-body-size-limit*)
                    *nlx-inline-body-size-limit*)
                 (%nlx-local-fn-p (car d) (cddr d)))
        (push (car d) names)))))

(defun local-fn-block-name (name)
  "The name of the implicit BLOCK around the body of a local function named
   NAME: NAME itself, or G for (SETF G)."
  (cond ((symbolp name) name)
        ((and (consp name) (eq (car name) 'setf) (symbolp (cadr name))) (cadr name))
        (t nil)))

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
        (plans (cdr lift))
        ;; The names this body declared INLINE. Read here rather than at the
        ;; call site: the declaration is about the binding, and the plan it
        ;; enables has to snapshot the environment the definition sits in.
        (inline-names (%implicit-inline-names fn-defs body)))
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
             (key (gen-local name-str))
             ;; The 6th element of the entry is the inline plan: present only
             ;; where this body declared this function INLINE and the definition
             ;; can actually be substituted (MAKE-LOCAL-INLINE-PLAN). Built from
             ;; the SOURCE lambda list and body, not the lifted ones: capture
             ;; lifting is about the closure this may end up not needing.
             (inl-plan (and (member name inline-names :test #'eq)
                            (make-local-inline-plan name (cadr fdef) (cddr fdef)))))
        ;; Compile the lambda (in current scope, not extended)
        ;; CL spec: flet creates an implicit block named after the function
        ;; ((setf sym) names: block sym).
        (flet ((compile-the-lambda ()
                 (let ((*lift-block-tags* (if plan (cstate-block-tags) nil)))
                   (let ((bname (local-fn-block-name name)))
                     (if (and bname
                              (some (lambda (f) (form-has-return-from-p bname f)) fn-body))
                         (compile-lambda params `((block ,bname ,@fn-body)))
                         (compile-lambda params fn-body))))))
          ;; A definition that may be inlined away is compiled LATER, and only if
          ;; the body turns out to still reach the binding. Not merely dropped
          ;; after the fact: compiling a closure that contains a RETURN-FROM out
          ;; of an enclosing block is what tells that block it needs a tag and a
          ;; try/filter, and throwing the instructions away afterwards does not
          ;; take the flag back. Compiled and discarded, the inlined escape still
          ;; costs its block the 32-byte tag it no longer needs.
          (push (if inl-plan
                    (list key #'compile-the-lambda nil)
                    (list key nil `((:declare-local ,key "LispObject")
                                    ,@(compile-the-lambda)
                                    (:stloc ,key))))
                fn-instrs)
          (push (list name-str key nil (and plan caps) (and plan tag) inl-plan name)
                new-local-fns)
          ;; Track in *locals* so closures can capture flet functions, under
          ;; the __LABELFN_ prefix only, as LABELS does: CL is a Lisp-2, and an
          ;; entry under the plain name would be read as the variable of that
          ;; name. The free var analysis detects __LABELFN_ entries in function
          ;; position and in #'name (see find-free-vars-expr), and
          ;; compile-closure-body prefers it.
          (when (local-fn-block-name name)
            (push (cons (%labelfn-local-sym (mangle-name name)) key)
                  new-locals)))))
    ;; Compile body with extended local-functions AND locals.
    ;; Also track flet source defs so compile-defmacro can wrap its eval
    ;; with flet bindings (e.g. SBCL macros.lisp wraps defmacro in flet).
    (let* ((new-local-fns (nreverse new-local-fns))
           (new-locals (nreverse new-locals))
           (plans (remove nil (mapcar (lambda (e) (nth 5 e)) new-local-fns)))
           (body-instrs
             (flet ((compile-body ()
                      (let ((*cstate* (cstate-with *cstate*
                                        +cs-local-functions+ (append new-local-fns
                                                                     (cstate-local-functions))
                                        +cs-locals+ (append new-locals (cstate-locals))))
                            (*lexical-decl-frames* (%push-decl-frame :flet fn-defs body))
                            (*compile-time-flet-defs* (append fn-defs *compile-time-flet-defs*)))
                        ;; A name that is a macro out here is a function in the body, which
                        ;; a macro's &ENVIRONMENT has to show (see *LEXICAL-OPERATORS*).
                        ;; LABELS binds this around its definitions too, in COMPILE-LABELS.
                        (let ((shadows (%flet-macro-shadows fn-defs)))
                          (if shadows
                              (let ((*lexical-operators* (%flet-lexical-operators shadows))
                                    (*macroexpand-scope* (cons fn-defs *macroexpand-scope*)))
                                (compile-progn body))
                              (compile-progn body))))))
               (let ((instrs (compile-body)))
                 (if (and (not *local-inline-capped*)
                          (some (lambda (p) (> (car (nth 6 p)) *local-inline-call-limit*))
                                plans)
                          (%sil-past-jit-limits-p instrs))
                     (let ((*local-inline-capped* t))
                       (dolist (p plans) (setf (car (nth 6 p)) 0))
                       (compile-body))
                     instrs)))))
      ;; Back in the outer environment, which is where a deferred definition has
      ;; to be compiled: an FLET function cannot see the bindings this form makes.
      (append
       ;; An inline-declared function whose local never appears in the compiled
       ;; body was substituted at every call site, and nothing else reached the
       ;; binding -- #'f as a value, a closure capturing it, a call the
       ;; substitution refused all put the local back in. Never compiling it is
       ;; the difference between an inlined escape that still pays for its
       ;; closure and one that costs nothing.
       (loop for entry in (nreverse fn-instrs)
             for key = (first entry)
             for deferred = (second entry)
             append (cond ((null deferred) (third entry))
                          ((%instr-tree-contains-symbol-p body-instrs key)
                           `((:declare-local ,key "LispObject")
                             ,@(funcall deferred)
                             (:stloc ,key)))
                          (t nil)))
       body-instrs))))

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
   self-contained; scan (a)."
  (cond ((eq tree sym) t)
        ;; A (:load-const OBJ) literal is data and may be circular.
        ((and (consp tree) (eq (car tree) :load-const)) nil)
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
   because its argument tail-recursed); reject. Scan (b)."
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
  (let ((shadows (%flet-macro-shadows fn-defs))
        (*lexical-decl-frames* (%push-decl-frame :flet fn-defs body)))
    (if shadows
        ;; A LABELS name that is a macro out here is a function both in the body
        ;; and in the definitions (see *LEXICAL-OPERATORS*). Not routed through
        ;; COMPILE-FLET, whose definitions are outside the binding's scope.
        (let ((*lexical-operators* (%flet-lexical-operators shadows))
              (*macroexpand-scope* (cons fn-defs *macroexpand-scope*)))
          (compile-labels-boxed fn-defs body))
        (compile-labels-1 fn-defs body))))

(defun compile-labels-1 (fn-defs body)
  "COMPILE-LABELS when no definition hides a macro."
  (let ((n-fns (length fn-defs)))
    (cond
      ;; One function that never names itself is an FLET: nothing can see the
      ;; binding the box exists for. Routing it there is what lets capture
      ;; lifting reach it, and the box it drops is an allocation of its own.
      ((and (= n-fns 1)
            (symbolp (car (first fn-defs)))
            (%labels-self-free-p (car (first fn-defs)) (cddr (first fn-defs))))
       ;; The walk sees the source only; a macro in the definition can still
       ;; expand into a self-call (see *LABELS-DEMOTED*). Such a call throws
       ;; back here and the binding is built after all.
       (let* ((tag (list '#:labels-demote))
              (result (let ((*labels-demoted* (acons (car (first fn-defs)) tag
                                                     *labels-demoted*)))
                        (catch tag (compile-flet fn-defs body)))))
         (if (eq result :labels-demote-aborted)
             (compile-labels-boxed fn-defs body)
             result)))
      ((and (>= n-fns 2) (%labels-inline-nest-order fn-defs body))
       (let* ((tag (list '#:labels-demote))
              (result (let ((*labels-demoted*
                              (append (mapcar (lambda (f) (cons (car f) tag)) fn-defs)
                                      *labels-demoted*)))
                        (catch tag
                          (compile-expr
                           (%labels-as-nested-flets
                            (%labels-inline-nest-order fn-defs body) body))))))
         (if (eq result :labels-demote-aborted)
             (compile-labels-1-closures fn-defs body)
             result)))
      (t (compile-labels-1-closures fn-defs body)))))

(defun compile-labels-1-closures (fn-defs body)
  (let ((first-arity (length (cadr (first fn-defs)))))
    (if (and (>= (length fn-defs) 2)
             (every (lambda (f) (labels-required-only-params-p (cadr f))) fn-defs)
             (every (lambda (f) (= (length (cadr f)) first-arity)) fn-defs))
        (compile-labels-mutual-tco fn-defs body first-arity)
        (compile-labels-boxed fn-defs body))))

(defun %labels-group-refs (fdef names)
  "The members of NAMES the definition FDEF names anywhere in its lambda list or
   body, outside QUOTE; its own name only where it is not a RETURN-FROM's block."
  (let ((self (car fdef)) (found '()))
    (labels ((walk (x)
               (cond ((and (symbolp x) (member x names :test #'eq))
                      (pushnew x found))
                     ((not (consp x)) nil)
                     ((eq (car x) 'quote) nil)
                     ((and (eq (car x) 'return-from) (eq (cadr x) self)
                           (consp (cdr x)))
                      (walk (cddr x)))
                     (t (do ((r x (cdr r)))
                            ((not (consp r)) (walk r))
                          (walk (car r)))))))
      (walk (cdr fdef)))
    found))

(defun %labels-inline-nest-order (fn-defs body)
  "FN-DEFS ordered so that each definition names only the ones before it, when
   the body declares one of them INLINE, all are symbol-named, none shadows a
   local function, and none reaches itself. Else NIL. In that order the LABELS
   is a nest of FLETs, which can substitute a call."
  (let ((names (mapcar #'car fn-defs)))
    (when (and (every #'symbolp names)
               (= (length names) (length (remove-duplicates names)))
               (intersection names (%implicit-inline-names fn-defs body))
               (notany #'local-function-entry names))
      (let ((deps (mapcar (lambda (f) (cons f (%labels-group-refs f names))) fn-defs))
            (placed '())
            (order '()))
        (loop while deps
              do (let ((next (find-if (lambda (d)
                                        (every (lambda (n) (member n placed :test #'eq))
                                               (cdr d)))
                                      deps)))
                   (unless next (return-from %labels-inline-nest-order nil))
                   (push (car (car next)) placed)
                   (push (car next) order)
                   (setf deps (remove next deps :test #'eq))))
        (nreverse order)))))

(defun %labels-as-nested-flets (order body)
  "The FLET nest for the LABELS body BODY with definitions ORDER. INLINE,
   NOTINLINE and FTYPE declarations of a member go right after its own FLET, so
   they also cover the definitions after it, as they cover all of them in a
   LABELS; the other declarations stay with BODY."
  (let ((names (mapcar #'car order))
        (per-name (make-hash-table :test #'eq))
        (rest-decls '())
        (forms body))
    (loop while (and (consp forms) (consp (car forms)) (eq (caar forms) 'declare))
          do (dolist (spec (cdr (pop forms)))
               (if (and (consp spec) (member (car spec) '(inline notinline ftype)))
                   (let* ((ftype-p (eq (car spec) 'ftype))
                          (fns (if ftype-p (cddr spec) (cdr spec)))
                          (head (if ftype-p (list 'ftype (cadr spec)) (list (car spec))))
                          (others (remove-if (lambda (f) (member f names :test #'eq)) fns)))
                     (dolist (f fns)
                       (when (member f names :test #'eq)
                         (push (append head (list f)) (gethash f per-name))))
                     (when others (push (append head others) rest-decls)))
                   (push spec rest-decls))))
    (let ((inner (if rest-decls
                     `((declare ,@(nreverse rest-decls)) ,@forms)
                     forms)))
      (dolist (fdef (reverse order) (car inner))
        (let ((own (gethash (car fdef) per-name)))
          (setf inner
                `((flet (,fdef)
                    ,@(when own `((declare ,@(reverse own))))
                    ,@inner))))))))

(defun %labels-group-closed-p (fn-compile-list)
  "T when no function of a LABELS group captures anything but the group's own
   boxes: the closures the group builds are then the same on every entry, and
   COMPILE-LABELS-BOXED can build them once. Uses the free-variable analysis
   that decides whether a LAMBDA needs an environment at all, so it answers
   what that decision answers. Called with the group's boxes in scope."
  (let ((own (mapcar (lambda (e) (%labelfn-local-sym (mangle-name (first e))))
                     fn-compile-list)))
    (every (lambda (e)
             (let* ((name (first e)) (params (second e)) (fn-body (third e))
                    (bname (local-fn-block-name name))
                    (body (if (and bname
                                   (some (lambda (f) (form-has-return-from-p bname f)) fn-body))
                              `((block ,bname ,@fn-body))
                              fn-body)))
               (every (lambda (v) (member v own :test #'eq))
                      (remove-if #'global-special-p
                                 (find-free-vars-with-defaults params body)))))
           fn-compile-list)))

(defun compile-labels-boxed (fn-defs body)
  "Compile (labels ...) using boxed closures (no mutual-TCO optimization).
   A group whose functions capture nothing but each other builds its closures
   once, the first time the form runs (the :ONCE instruction), and every entry
   reuses them: they are the same closures each time, and building them was an
   allocation of a function object, a delegate and an environment per function
   per entry."
  (let ((outer-cstate *cstate*)
        (box-instrs '())
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
        (push (list name-str key t nil nil nil name) new-local-fns)
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
      (when (and (not (eq fn-defs *labels-once-inner*))
                 (every (lambda (e) (symbolp (first e))) fn-compile-list)
                 (%labels-group-closed-p fn-compile-list))
        (let ((vec (gen-local "LABELSONCE")))
          (return-from compile-labels-boxed
            `(,@box-instrs
              (:declare-local ,vec "LispObject")
              (:once ,@(let ((*cstate* outer-cstate)
                             (*labels-once-inner* fn-defs))
                         (compile-expr
                          `(function
                            (lambda ()
                             (labels ,fn-defs
                               (vector ,@(mapcar (lambda (e) `(function ,(first e)))
                                                 fn-compile-list)))))))
                     (:castclass "LispFunction")
                     (:callvirt "LispFunction.Invoke0"))
              (:stloc ,vec)
              ,@(loop for e in fn-compile-list
                      for i from 0
                      append `((:ldloc ,(fourth e)) (:ldc-i4 0)
                               (:ldloc ,vec) ,@(compile-expr i) (:call "Runtime.Aref")
                               (:stelem-ref)))
              ,@(compile-progn body)))))
      ;; Compile each function and store into its box.
      ;; CL spec: labels creates an implicit block named after the function.
      ;; ((setf sym) names: block sym).
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
                       (let ((bname (local-fn-block-name name)))
                         (if (and bname
                                  (some (lambda (f) (form-has-return-from-p bname f)) fn-body))
                             (compile-lambda params `((block ,bname ,@fn-body)) name-str)
                             (compile-lambda params fn-body name-str))))))
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
  (mapcar (lambda (lf) (cons (%labelfn-local-sym (first lf)) (second lf)))
          new-local-fns))

(defun compile-labels-mutual-tco (fn-defs body arity)
  "Compile (labels ...) where all functions have the same required ARITY.
   Emits closures in boxes for non-tail calls, plus a dispatch loop for
   mutual tail call optimization.
   Dispatch loop structure:
     TCOLOOP: dispatch by WHICH_FN index -> fn body sections
     Each fn body section: tail calls to siblings -> br TCOLOOP; normal return -> br TCOEND.
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
        (push (list name-str key t nil nil nil name) new-local-fns)
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
                 ;; compile-labels-boxed): a stale enclosing defun symbol
                 ;; falsely matches tail calls to that outer defun.
                 (lambda-instrs (let ((*cstate* (cstate-with *cstate*
                                                  +cs-tco-local-fn-key+ key
                                                  +cs-tco-self-symbol+
                                                  (if (symbolp name) name nil))))
                                  (let ((bname (local-fn-block-name name)))
                                    (if (and bname
                                             (some (lambda (f) (form-has-return-from-p bname f)) fn-body))
                                        (compile-lambda params `((block ,bname ,@fn-body)) name-str)
                                        (compile-lambda params fn-body name-str))))))
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
                                          ;; *small-int-locals* is slot-keyed;
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
                                          (*in-tail-position* t)
                                          ;; ... not of the enclosing body: its tail
                                          ;; VALUES is no answer to that body's caller.
                                          (*mv-mode-local* nil))
                                      (let ((bname (local-fn-block-name (car fdef))))
                                        (if (and bname
                                                 (some (lambda (f) (form-has-return-from-p bname f))
                                                       fn-body))
                                            (compile-progn `((block ,bname ,@fn-body)))
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
