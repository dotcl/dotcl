;;; cil-analysis.lisp: Free variable, mutation, and capture analysis
;;; Part of the CIL compiler (A2 instruction list architecture)

(in-package :dotcl.cil-compiler)

;;; ============================================================
;;; Safe list iteration (handles dotted pairs)
;;; ============================================================

(defmacro do-list-safe ((var list) &body body)
  "Like dolist but handles dotted pairs without error.
   Iterates over car elements; stops at non-cons cdr."
  (let ((cur (gensym "CUR")))
    `(let ((,cur ,list))
       (loop while (consp ,cur)
             do (let ((,var (car ,cur)))
                  ,@body)
                (setf ,cur (cdr ,cur))))))

;;; ============================================================
;;; Block tag variable names (for non-local return-from capture)
;;; ============================================================

(defun block-tag-var-name (block-name)
  "Return the synthetic variable name for a block's tag.
   Used to track block tags as capturable variables for closures."
  (concatenate 'string "%BTAG-" (symbol-name block-name) "%"))

;;; ============================================================
;;; Free variable analysis (for lambda/closure)
;;; ============================================================

;; Nesting of macro expansions an analysis walk follows before it gives up and
;; walks the unexpanded form instead. Giving up is NOT conservative: a closure
;; that only appears in a deeper expansion is missed, so a variable it assigns
;; is not boxed and the assignment is lost. Keep this at the depth guard in
;; COMPILE-FORM (500), which counts every nested form including each
;; expansion: any form the compiler accepts is then walked completely. Code
;; walkers that wrap each subform in their own macro (cl-environments'
;; %WALK-FORM) reach more than 50 in ordinary code.
(defvar *macro-expand-depth-limit* 500)

;; Stub for cross-compilation: always returns T (no stack limit during self-compile)
(unless (fboundp '%stack-space-available-p)
  (defun %stack-space-available-p () t))

;;; find-free-vars-expr: iterative version using explicit worklist.
;;; Each worklist entry is (expr bound . mdepth).
;;; Macrolet restore sentinels: (:restore-macro name . old-entry-or-nil).
;;; A unique private object marks the "pop *macroexpand-scope*" sentinel; using an
;;; uninterned cons (never EQ to any analyzed source form) avoids the keyword-vs-
;;; source-form collision the other sentinels have to guard against.
(defvar *mscope-restore-sentinel* (list '#:restore-macroexpand-scope))

;; Marks the "restore *lexical-operators*" sentinel, the same way. The walks
;; extend *LEXICAL-OPERATORS* at a MACROLET, and at an FLET / LABELS that hides
;; a macro, exactly where compile does, so a macro's &ENVIRONMENT (and the
;; expansion cached for code-gen) is the same in both passes.
(defvar *lexops-restore-sentinel* (list '#:restore-lexical-operators))

;; Per-top-level-form EQ memo: lambda form -> list of free-variable CANDIDATE
;; names (structurally free w.r.t. the lambda's own params, collected under
;; *ffv-assume-bound* so the set is *locals*-independent = a pure function of the
;; form). find-free-vars-expr descends into every nested lambda and compile-lambda
;; re-runs find-free-vars per lambda, so without this an inner body is walked once
;; per enclosing lambda = O(depth^2) on nested closures. The real
;; local-bound-p filter is applied at each enclosing merge, not baked into the memo.
(defvar *ffv-free-cache* nil)

;; Marker heading a (MARKER . TAG) free-var candidate: a GO to TAG seen while
;; collecting candidates, resolved to the target tagbody's id variable at merge.
(defvar *ffv-go-candidate* (list '#:go-candidate))

(defun %ffv-note-go (tag bnd free-ht)
  "A GO to TAG seen by the free-variable walk: when TAG names a tagbody outside
   the lambda being analysed, that tagbody's id variable is captured."
  (let ((entry (assoc tag (cstate-go-tags))))
    (when entry
      (let* ((tb-var-name (second entry))
             (tb-sym (intern tb-var-name :dotcl.cil-compiler)))
        (when (and (not (member tb-var-name bnd :test #'string=))
                   (not (gethash tb-sym free-ht))
                   (local-bound-p tb-sym))
          (setf (gethash tb-sym free-ht) tb-sym))))))

(defun %walker-macroexpand (form expander bound)
  "Macroexpand FORM for an analysis walk. Returns (values EXPANSION HITS):
   EXPANSION is NIL when the expander signals, and HITS lists the global symbol
   macros the expander resolved without knowing whether a binding in the
   walked code shadows them (see *SM-GLOBAL-HITS*). BOUND is the free-variable
   walk's BND, or :NONE for a walk that does not track bound names."
  (let ((*sm-walker-context* t)
        (*sm-walker-bound* bound)
        (*sm-global-hits* nil))
    (values (handler-case (cached-macroexpand form expander)
              (error () nil))
            *sm-global-hits*)))

(defun %walker-macroexpand-for-mutation (form expander mutated-ht in-lambda mdepth push-fn)
  "%WALKER-MACROEXPAND for the mutation / capture walk. Each global symbol macro
   the expansion resolved is only ever looked up as a place (setf-family
   expanders are the only macro-time callers of LOOKUP-SYMBOL-MACRO), so if the
   name is in fact a shadowing variable it is being assigned: record it as
   mutated, and hand it to PUSH-FN as a reference for the capture side.
   Over-recording is safe (it can only box a variable that did not need it)."
  (multiple-value-bind (exp hits) (%walker-macroexpand form expander :none)
    (dolist (h hits)
      (setf (gethash (var-name h) mutated-ht) t)
      (funcall push-fn (cons h (cons in-lambda mdepth))))
    exp))

(defun find-free-vars-expr (expr bound free-ht)
  "Walk expr finding free variable references. Results accumulated in free-ht.
   Iterative worklist version; no recursion depth limit."
  (let ((worklist (list (cons expr (cons bound 0))))
        (*lexical-operators* *lexical-operators*))
    (loop while worklist do
      (let* ((item (pop worklist))
             (e (car item)))
        (cond
          ;; Restore-scope sentinel: pop *macroexpand-scope* after a macrolet body.
          ;; The marker is a unique private object, so no source form collides.
          ((eq e *mscope-restore-sentinel*)
           (setf *macroexpand-scope* (cdr item)))
          ((eq e *lexops-restore-sentinel*)
           (setf *lexical-operators* (cdr item)))
          ;; Restore-macro sentinel: restore *macros* entry after macrolet body.
          ;; Guard symbolp name so that a bare :restore-macro keyword from analyzed
          ;; source code (where cadr item is a bnd-list, not a symbol) is ignored.
          ((and (eq e :restore-macro) (symbolp (cadr item)))
           (let ((name (cadr item))
                 (old-entry (cddr item)))
             (if old-entry
                 (setf (gethash name *macros*) old-entry)
                 (remhash name *macros*))))
          ;; Restore-symbol-macros sentinel: restore *symbol-macros* after symbol-macrolet body.
          ;; Guard: real sentinels have (cdr item) = old-*symbol-macros* = nil or proper alist,
          ;; so (cddr item) is nil or a list. Collisions (when analyzed code contains the literal
          ;; keyword :restore-symbol-macros) have (cdr item) = (bnd . mdepth) so (cddr item) = integer.
          ((and (eq e :restore-symbol-macros) (not (integerp (cddr item))))
           (setf *symbol-macros* (cdr item)))
          (t
           (let ((bnd (cadr item))
                 (mdepth (cddr item)))
             (cond
               ;; Symbol: check if it's a free variable reference.
               ;; If the symbol is a symbol-macro (and not shadowed by a local binding),
               ;; walk the expansion instead of treating it as a variable reference.
               ((symbolp e)
                (let ((sm (and e
                               (not (bnd-member-p e bnd))
                               (assoc e *symbol-macros* :test #'eq))))
                  (if sm
                      (push (cons (cdr sm) (cons bnd mdepth)) worklist)
                      (when (and e
                                 (or (not (eq e t)) (local-bound-p e))
                                 ;; A keyword is a self-evaluating constant, never a
                                 ;; lexical variable, so it is never a free-var
                                 ;; candidate. Excluding it unconditionally matters
                                 ;; under *ffv-assume-bound* (candidate collection),
                                 ;; where LOCAL-BOUND-P is forced T: otherwise e.g.
                                 ;; the :input keyword in (apply f :input input ...)
                                 ;; would grab the "INPUT" var-name slot in FREE-HT
                                 ;; (string-keyed) and shadow the real INPUT variable,
                                 ;; losing its closure capture.
                                 (not (keywordp e))
                                 (not (bnd-member-p e bnd))
                                 (not (gethash e free-ht))
                                 (local-bound-p e))
                        (setf (gethash e free-ht) e)))))
               ;; Cons: dispatch on head
               ((consp e)
                (let ((head (car e)))
                  (cond
                    ((and (symbolp head) (eq head 'quote)) nil)
                    ((and (symbolp head) (eq head 'defun)) nil)
                    ;; The portable MULTIPLE-VALUE-BIND shape compiles as a LET*
                    ;; (%MV-CALL-LAMBDA-BIND), not as a closure: walk it that way.
                    ((let ((bind (and (eq head 'multiple-value-call)
                                      (%mv-call-lambda-bind e))))
                       (when bind
                         (push (cons bind (cons bnd mdepth)) worklist)
                         t)))
                    ;; cond: each clause is (test . body) and EVERY element is an
                    ;; evaluated expression. The generic walk below would treat a
                    ;; clause whose test is a symbol, e.g. (cond (start-anchored-p ...)),
                    ;; as a function call (car = function name), dropping the test as a
                    ;; free-variable reference. When that name is also a captured local
                    ;; AND a global function (Lisp-2), the variable then isn't captured
                    ;; into the closure env and reads "Unbound variable" at run time.
                    ;; Scan all elements of each clause as expressions. (case/typecase
                    ;; differ: their clause cars are unevaluated keys: handled generically.)
                    ((and (symbolp head) (eq head 'cond))
                     (do-list-safe (clause (cdr e))
                       (when (consp clause)
                         (do-list-safe (sub clause)
                           (push (cons sub (cons bnd mdepth)) worklist)))))
                    ;; Lambda.
                    ((and (symbolp head) (eq head 'lambda) (listp (cadr e)))
                     (if *symbol-macros*
                         ;; Under an active symbol-macrolet, an enclosing binding
                         ;; (e.g. a lambda parameter) may shadow a symbol-macro of
                         ;; the same name (CLHS 3.4.2). The candidate memo drops the
                         ;; enclosing BND, so it would lose that shadow and re-expand
                         ;; the symbol-macro: infinitely if it is self-referential
                         ;; (regression: symbol-macro-param-shadow-nested-lambda).
                         ;; Fall back to the exact inline descent, which carries the
                         ;; full enclosing BND. Rare, so the O(depth^2) is acceptable.
                         (let* ((params (cadr e))
                                (lbody (cddr e))
                                (inner-bound (append (extract-param-names params) bnd)))
                           (map-lambda-list-vars
                            params
                            (lambda (init scope)
                              (push (cons init (cons scope mdepth)) worklist))
                            bnd)
                           (dolist (form lbody)
                             (push (cons form (cons inner-bound mdepth)) worklist)))
                         ;; No active symbol-macro: merge the memoized free-var
                         ;; CANDIDATES (names free w.r.t. E's own params,
                         ;; *locals*-independent) re-scoped by the enclosing BND,
                         ;; applying the real local-bound-p here. O(1) per enclosing
                         ;; level instead of re-walking the inner body each time
                         ;; level. Under *ffv-assume-bound* (we are collecting
                         ;; candidates for an outer lambda) local-bound-p is T, so
                         ;; this collects; otherwise it filters.
                        (dolist (sym (%lambda-free-candidates e))
                          (cond ((not (and (consp sym) (eq (car sym) *ffv-go-candidate*)))
                                 (when (and (not (bnd-member-p sym bnd))
                                            (not (gethash sym free-ht))
                                            (or *ffv-assume-bound* (local-bound-p sym)))
                                   (setf (gethash sym free-ht) sym)))
                                (*ffv-assume-bound* (setf (gethash sym free-ht) sym))
                                (t (%ffv-note-go (cdr sym) bnd free-ht))))))
                    ;; Let/Let* introduces bindings
                    ((and (symbolp head) (member head '(let let*)) (listp (cadr e)))
                     (let* ((bindings (cadr e))
                            (lbody (cddr e))
                            (inner-bound (copy-list bnd))
                            (is-star (eq head 'let*)))
                       (dolist (b bindings)
                         (let ((init (if (consp b) (cadr b) nil))
                               (bind-sym (if (consp b) (car b) b)))
                           (when init
                             (push (cons init (cons (if is-star inner-bound bnd) mdepth)) worklist))
                           (push bind-sym inner-bound)))
                       (dolist (form lbody)
                         (push (cons form (cons inner-bound mdepth)) worklist))))
                    ;; setq: analyze all target/value pairs
                    ((and (symbolp head) (eq head 'setq))
                     (loop for (var val) on (cdr e) by #'cddr
                           do (when var (push (cons var (cons bnd mdepth)) worklist))
                              (when val (push (cons val (cons bnd mdepth)) worklist))))
                    ;; go: check if tagbody ID needs capture
                    ((and (symbolp head) (eq head 'go))
                     (if *ffv-assume-bound*
                         ;; Candidate collection: which tagbody the tag names
                         ;; depends on the go tags in scope where the enclosing
                         ;; lambda is compiled, and the candidate memo outlives
                         ;; that scope (a walk made before the TAGBODY's tags
                         ;; were known would cache "captures nothing"). Record
                         ;; the tag itself; the merge resolves it.
                         (let ((m (cons *ffv-go-candidate* (cadr e))))
                           (setf (gethash m free-ht) m))
                         (%ffv-note-go (cadr e) bnd free-ht)))
                    ;; Block introduces a synthetic block-tag variable
                    ((and (symbolp head) (eq head 'block))
                     (let* ((bname (cadr e))
                            (tag-var (block-tag-var-name bname))
                            (inner-bound (cons tag-var bnd)))
                       (dolist (form (cddr e))
                         (push (cons form (cons inner-bound mdepth)) worklist))))
                    ;; return-from: check block tag capture + scan value
                    ((and (symbolp head) (eq head 'return-from))
                     (let* ((bname (cadr e))
                            (tag-var (block-tag-var-name bname))
                            (tag-sym (intern tag-var :dotcl.cil-compiler)))
                       (when (and (not (member tag-var bnd :test #'string=))
                                  (not (gethash tag-sym free-ht))
                                  (local-bound-p tag-sym))
                        (setf (gethash tag-sym free-ht) tag-sym)))
                     (when (caddr e)
                       (push (cons (caddr e) (cons bnd mdepth)) worklist)))
                    ;; return: (return expr) = (return-from nil expr)
                    ((and (symbolp head) (eq head 'return))
                     (let* ((tag-var (block-tag-var-name nil))
                            (tag-sym (intern tag-var :dotcl.cil-compiler)))
                       (when (and (not (member tag-var bnd :test #'string=))
                                  (not (gethash tag-sym free-ht))
                                  (local-bound-p tag-sym))
                        (setf (gethash tag-sym free-ht) tag-sym)))
                     (when (cadr e)
                       (push (cons (cadr e) (cons bnd mdepth)) worklist)))
                    ;; (function sym) or (function (lambda ...))
                    ((and (symbolp head) (eq head 'function))
                     (let ((arg (cadr e)))
                       (cond
                         ((and (consp arg) (eq (car arg) 'lambda))
                          (push (cons arg (cons bnd mdepth)) worklist))
                         ;; #'(setf g): a LABELS (setf g) lives in a box like any
                         ;; other LABELS function, under the mangled name.
                         ((and (consp arg) (eq (car arg) 'setf) (symbolp (cadr arg)))
                          (let* ((nm (concatenate 'string "__LABELFN_" (mangle-name arg)))
                                 (nm-sym (intern nm :dotcl.cil-compiler)))
                            (when (and (not (member nm bnd :test #'string=))
                                       (not (gethash nm-sym free-ht))
                                       (or *ffv-assume-bound* (local-bound-p nm-sym)))
                              (setf (gethash nm-sym free-ht) nm-sym))))
                         ((symbolp arg)
                          (if *ffv-assume-bound*
                              ;; Candidate collection: local-bound-p is T for all,
                              ;; so the mangled-vs-plain choice below can't be made
                              ;; yet. Emit BOTH names as candidates; the enclosing
                              ;; merge's real local-bound-p keeps the labels-fn
                              ;; (mangled) and/or the variable (plain) that is
                              ;; actually bound. Over-collecting is harmless: merge
                              ;; drops names that are not local-bound.
                              (when (and arg (not (special-var-p arg)))
                                (let ((plain-name (symbol-name arg))
                                      (mangled-name (concatenate 'string "__LABELFN_"
                                                                 (symbol-name arg))))
                                  (dolist (nm (list mangled-name plain-name))
                                    (let ((nm-sym (intern nm :dotcl.cil-compiler)))
                                      (when (and (not (member nm bnd :test #'string=))
                                                 (not (gethash nm-sym free-ht)))
                                        (setf (gethash nm-sym free-ht) nm-sym))))))
                              (when (and arg (or (not (eq arg t)) (local-bound-p arg))
                                         (not (special-var-p arg))
                                         (local-bound-p arg))
                                (let* ((plain-name (symbol-name arg))
                                       (mangled-name (concatenate 'string "__LABELFN_" plain-name))
                                       (capture-name (cond
                                                       ((local-bound-p (intern mangled-name :dotcl.cil-compiler))
                                                        mangled-name)
                                                       (t plain-name))))
                                  (let ((capture-sym (intern capture-name :dotcl.cil-compiler)))
                                    (when (and (not (member capture-name bnd :test #'string=))
                                               (not (gethash capture-sym free-ht)))
                                      (setf (gethash capture-sym free-ht) capture-sym))))))))))
                    ;; handler-case: body + clauses with optional var binding
                    ((and (symbolp head) (eq head 'handler-case))
                     (let ((body-form (cadr e))
                           (hc-clauses (cddr e)))
                       (push (cons body-form (cons bnd mdepth)) worklist)
                       (dolist (clause hc-clauses)
                         (let* ((lambda-list (cadr clause))
                                (var (if (and lambda-list (car lambda-list))
                                         (car lambda-list) nil))
                                (handler-body (cddr clause))
                                (inner-bound (if var
                                                 (cons (var-name var) bnd)
                                                 bnd)))
                           (dolist (form handler-body)
                             (push (cons form (cons inner-bound mdepth)) worklist))))))
                    ;; handler-bind: bindings + body
                    ((and (symbolp head) (eq head 'handler-bind))
                     (let ((hb-bindings (cadr e))
                           (hb-body (cddr e)))
                       (dolist (binding hb-bindings)
                         (when (cadr binding)
                           (push (cons (cadr binding) (cons bnd mdepth)) worklist)))
                       (dolist (form hb-body)
                         (push (cons form (cons bnd mdepth)) worklist))))
                    ;; restart-case: body + clauses with params
                    ((and (symbolp head) (eq head 'restart-case))
                     (push (cons (cadr e) (cons bnd mdepth)) worklist)
                     (dolist (clause (cddr e))
                       (let* ((params (cadr clause))
                              (handler-body (cddr clause))
                              (param-names
                                (let ((names nil))
                                  (dolist (p params)
                                    (cond ((member p '(&optional &rest &key &aux &allow-other-keys)) nil)
                                          ((consp p) (push (var-name (car p)) names))
                                          ((symbolp p) (push (var-name p) names))))
                                  (nreverse names)))
                              (inner-bound (append param-names bnd)))
                         ;; Default value forms in optional/key params
                         (dolist (p params)
                           (when (and (consp p) (cdr p))
                             (push (cons (cadr p) (cons bnd mdepth)) worklist)))
                         (dolist (form handler-body)
                           (push (cons form (cons inner-bound mdepth)) worklist)))))
                    ;; macrolet: register macros, push body, push restore sentinels
                    ((and (symbolp head) (eq head 'macrolet))
                     (let ((macro-defs (cadr e))
                           (mlbody (cddr e)))
                       ;; Push restore sentinels FIRST (LIFO: processed LAST, after body)
                       (dolist (def macro-defs)
                         (let* ((mname (car def))
                                (old-entry (gethash mname *macros*)))
                           (push (cons :restore-macro (cons mname old-entry)) worklist)))
                       ;; Pop-scope sentinel + push this macrolet's scope marker, so a
                       ;; form shared between this shadowing scope and an outer scope is
                       ;; cached per scope and the analysis walk agrees with code-gen.
                       ;; Same source MACRO-DEFS cons as compile-macrolet.
                       (push (cons *mscope-restore-sentinel* *macroexpand-scope*) worklist)
                       (push (cons *lexops-restore-sentinel* *lexical-operators*) worklist)
                       (setf *macroexpand-scope* (cons macro-defs *macroexpand-scope*))
                       ;; Register macros immediately (same as compile-macrolet)
                       (dolist (def macro-defs)
                         (let* ((mname (car def))
                                (mparams (cadr def))
                                (mbody (cddr def)))
                           (setf (gethash mname *macros*)
                                 (%eval-macrolet-expander mparams mbody))))
                       (setf *lexical-operators* (%macrolet-lexical-operators macro-defs))
                       ;; Push body forms (LIFO: processed BEFORE restore sentinels)
                       (dolist (form mlbody)
                         (push (cons form (cons bnd mdepth)) worklist))))
                    ;; symbol-macrolet: extend *symbol-macros* during body walk
                    ((and (symbolp head) (eq head 'symbol-macrolet))
                     (let* ((sm-bindings (cadr e))
                            (sm-body (cddr e))
                            ;; A symbol-macro shadows an enclosing lexical variable of
                            ;; the same name in the body, so drop those names from the
                            ;; bound set: a reference to them is the symbol-macro, not
                            ;; a variable. Must match compile-symbol-macrolet, else the
                            ;; free/mutation analysis and codegen disagree on boxing.
                            (body-bnd (remove-if
                                       (lambda (n)
                                         (member n sm-bindings
                                                 :key (lambda (b) (var-name (car b)))
                                                 :test #'string=))
                                       bnd)))
                       ;; Push restore sentinels FIRST (LIFO: processed LAST, after body):
                       ;; pop *macroexpand-scope* and restore *symbol-macros*.
                       (push (cons *mscope-restore-sentinel* *macroexpand-scope*) worklist)
                       (push (cons :restore-symbol-macros *symbol-macros*) worklist)
                       ;; Push a scope marker so cached macro expansions inside the body
                       ;; are keyed per symbol-macrolet scope (mirrors compile-symbol-macrolet).
                       ;; SM-BINDINGS is the same source cons compile pushes.
                       (setf *macroexpand-scope* (cons sm-bindings *macroexpand-scope*))
                       ;; Extend *symbol-macros* immediately so macro expansions inside
                       ;; the body see the correct symbol-macro bindings
                       (setf *symbol-macros*
                             (append (mapcar (lambda (b) (cons (car b) (cadr b))) sm-bindings)
                                     *symbol-macros*))
                       ;; Push body forms (LIFO: processed BEFORE restore sentinels)
                       (dolist (form sm-body)
                         (push (cons form (cons body-bnd mdepth)) worklist))))
                    ;; flet/labels: function definitions + body
                    ((and (symbolp head) (member head '(flet labels)) (listp (cadr e)))
                     (let* ((fn-defs (cadr e))
                            (lbody (cddr e))
                            (fn-names (loop for fd in fn-defs
                                            for name = (car fd)
                                            when (symbolp name) collect (symbol-name name)))
                            (fn-boxes (loop for fd in fn-defs
                                            for name = (car fd)
                                            when (local-fn-block-name name)
                                              collect (concatenate 'string "__LABELFN_"
                                                                   (mangle-name name))))
                            (shadows (%flet-macro-shadows fn-defs)))
                       ;; Names that hide a macro: in scope for the body, and for
                       ;; LABELS also for the definitions (as in compile-flet /
                       ;; compile-labels). LIFO: an FLET enters after its
                       ;; definitions are pushed, so they are walked after the
                       ;; restore sentinels, outside the scope.
                       (flet ((enter ()
                                (push (cons *mscope-restore-sentinel* *macroexpand-scope*) worklist)
                                (push (cons *lexops-restore-sentinel* *lexical-operators*) worklist)
                                (setf *macroexpand-scope* (cons fn-defs *macroexpand-scope*)
                                      *lexical-operators* (%flet-lexical-operators shadows))))
                         (when (and shadows (eq head 'labels)) (enter))
                       ;; Function bodies see outer scope (flet) or same scope (labels)
                       ;; Labels fn-names are NOT added to fn body bound: they are captured
                       ;; as free vars via boxed variables in *locals*
                       ;; The box of each function, as a closure would capture it
                       ;; (see COMPILE-FLET / COMPILE-LABELS-BOXED). A call to the
                       ;; function inside the form refers to this binding, not to
                       ;; an outer local function of the same name, so for the
                       ;; lambda being analyzed it is bound here: in the body, and
                       ;; for LABELS also in the definitions.
                       (let ((fn-body-bound (if (eq head 'labels)
                                                (append fn-boxes bnd)
                                                bnd)))
                         (dolist (fd fn-defs)
                           (let* ((params (cadr fd))
                                  (fn-body (cddr fd))
                                  (fname (car fd))
                                  (block-name (if (consp fname) (cadr fname) fname))
                                  ;; The body is inside the function's implicit
                                  ;; BLOCK, so a RETURN-FROM of that name there
                                  ;; targets it, not an outer block of the same name.
                                  (inner-bound (append (extract-param-names params)
                                                       (if (symbolp block-name)
                                                           (cons (block-tag-var-name block-name)
                                                                 fn-body-bound)
                                                           fn-body-bound))))
                             ;; Init forms of the fn's own params: same scoping
                             ;; rule as a lambda's (see MAP-LAMBDA-LIST-VARS).
                             (map-lambda-list-vars
                              params
                              (lambda (init scope)
                                (push (cons init (cons scope mdepth)) worklist))
                              fn-body-bound)
                             ;; Push fn body forms
                             (dolist (form fn-body)
                               (push (cons form (cons inner-bound mdepth)) worklist)))))
                       (when (and shadows (eq head 'flet)) (enter))
                       ;; Body sees all fn-names as bound
                       (let ((body-bound (append fn-names fn-boxes bnd)))
                         (dolist (form lbody)
                           (push (cons form (cons body-bound mdepth)) worklist))))))
                    ;; CLOS primitives: analyze sub-expressions normally
                    ((and (symbolp head) (member head '(%make-class %make-slot-def %register-class %set-class-default-initargs
                                                        find-class %find-class-or-nil class-of class-name
                                                        slot-value slot-boundp %set-slot-value
                                                        %allocate-instance %slot-exists-p
                                                        make-instance %make-instance-with-initargs
                                                        %make-gf %register-gf %set-method-combination %set-method-combination-order %set-method-combination-args %find-gf
                                                        %clear-defgeneric-inline-methods %mark-defgeneric-inline-method
                                                        %make-method %add-method
                                                        %gf-methods %method-specializers
                                                        %method-qualifiers %method-function
                                                        call-next-method next-method-p
                                                        %captured-call-next-method %captured-next-method-p
                                                        %change-class)))
                     (dolist (sub (cdr e))
                       (push (cons sub (cons bnd mdepth)) worklist)))
                    ;; Default: try macro expansion, then generic walk
                    (t
                     (let ((expanded nil))
                       (when (and (symbolp head) head
                                  (< mdepth *macro-expand-depth-limit*)
                                  (%stack-space-available-p)
                                  (not (%lexical-function-p head))
                                  (find-macro-expander head))
                         (let ((expander (find-macro-expander head)))
                           (multiple-value-bind (exp hits)
                               (%walker-macroexpand e expander bnd)
                             (setf expanded exp)
                             ;; A place that is a global symbol macro may be a
                             ;; LET-bound variable here: walk the name as a
                             ;; reference so a binding outside this form is still
                             ;; captured.
                             (dolist (h hits)
                               (push (cons h (cons bnd mdepth)) worklist)))))
                       (if expanded
                           (push (cons expanded (cons bnd (1+ mdepth))) worklist)
                           (progn
                             ;; Labels function mangled name capture.
                             ;; head=NIL is admitted too: a local function may
                             ;; be named NIL (ANSI LABELS.24) and its box must
                             ;; be captured: the local-bound-p check below
                             ;; gates this to scopes where such a fn exists.
                             (when (symbolp head)
                               (let* ((name (symbol-name head))
                                      (mangled (concatenate 'string "__LABELFN_" name))
                                      (mangled-sym (intern mangled :dotcl.cil-compiler)))
                                 (when (and (local-bound-p mangled-sym)
                                            (not (member mangled bnd :test #'string=))
                                            (not (gethash mangled-sym free-ht)))
                                   (setf (gethash mangled-sym free-ht) mangled-sym))))
                             ;; Generic walk. The car is in function position only when
                             ;; it is a SYMBOL (function name) or a (setf sym) / (lambda ...)
                             ;; compound form. Symbols in function position must NOT be pushed
                             ;; as variable references: doing so would loop on symbol-macros
                             ;; from with-accessors (e.g. (disabled-commands #:OBJ) ->
                             ;; push disabled-commands symbol -> expand to (disabled-commands
                             ;; #:OBJ) -> repeat). Lambda-car means immediate application;
                             ;; scan it. (setf sym) is a compound function name: skip it.
                             ;; Any OTHER cons in car position means the form is NOT a function
                             ;; call (e.g. a cond clause ((test-form ...) result)), so we push
                             ;; all sub-expressions including the car.
                             (let ((car-e (car e)))
                               (cond
                                 ((symbolp car-e)
                                  ;; Symbol car: function name. Push args only.
                                  (do-list-safe (sub (cdr e))
                                    (push (cons sub (cons bnd mdepth)) worklist)))
                                 ((and (consp car-e) (eq (car car-e) 'lambda))
                                  ;; Immediate application: scan lambda and args.
                                  (push (cons car-e (cons bnd mdepth)) worklist)
                                  (do-list-safe (sub (cdr e))
                                    (push (cons sub (cons bnd mdepth)) worklist)))
                                 ((and (consp car-e) (eq (car car-e) 'setf))
                                  ;; Compound function name: push args only.
                                  (do-list-safe (sub (cdr e))
                                    (push (cons sub (cons bnd mdepth)) worklist)))
                                 (t
                                  ;; Non-function-call form (e.g. cond clause, case clause):
                                  ;; push all sub-expressions including the car.
                                  (do-list-safe (sub e)
                                    (push (cons sub (cons bnd mdepth)) worklist))))))))))))))))))))

(defun %compute-free-candidates (e)
  "Free-variable CANDIDATE names of lambda form E, relative to E's OWN params.
   Runs the same walk as the inline lambda case but under *ffv-assume-bound* (so
   local-bound-p is T and every structurally-free name is collected) and with an
   empty enclosing scope; the caller re-scopes by subtracting its BND and applies
   the real local-bound-p. Because *locals* is not consulted, the result is a pure
   function of E and can be memoized."
  (let* ((params (cadr e))
         (lbody (cddr e))
         (inner-bound (extract-param-names params))
         (free-ht (make-hash-table :test #'eq))
         (*ffv-assume-bound* t))
    ;; &optional/&key/&aux default forms with progressive left-to-right scoping,
    ;; starting from the empty scope (a default referencing an enclosing-bound var
    ;; is reported here and removed by the caller's BND subtraction).
    (map-lambda-list-vars params
                          (lambda (init scope)
                            (find-free-vars-expr init scope free-ht))
                          '())
    (dolist (form lbody)
      (find-free-vars-expr form inner-bound free-ht))
    (let ((keys '()))
      (maphash (lambda (k v) (declare (ignore k)) (push v keys)) free-ht)
      keys)))

(defun %lambda-free-candidates (e)
  "Memoized wrapper over %compute-free-candidates, keyed by EQ(E) in
   *ffv-free-cache*. Within one top-level form EQ identity of E implies an
   identical lexical macro/symbol-macro scope, and the candidate set is
   *locals*-independent, so caching is a behaviour-preserving performance
   transform."
  (if *ffv-free-cache*
      (multiple-value-bind (cached present) (gethash e *ffv-free-cache*)
        (if present
            cached
            (setf (gethash e *ffv-free-cache*) (%compute-free-candidates e))))
      (%compute-free-candidates e)))

;;; ============================================================
;;; Mutated/captured variable analysis (for boxing)
;;; ============================================================

;;; find-mutated-and-captured-vars: single worklist walk that computes BOTH the
;;; mutated-var set and the captured-var set in one pass over the expression
;;; tree, avoiding two separate O(tree) walks per lambda/let.
;;;
;;; Merges find-mutated-vars-expr + find-captured-vars-expr. The two passes are
;;; walk-compatible: identical mdepth-incrementing symbol-macro expansion,
;;; identical macrolet/symbol-macrolet register-restore discipline, identical
;;; let shadow-narrowing, and the same cached-macroexpand / *macroexpand-scope*
;;; contract. Worklist entry shape is captured's `(expr inside-lambda . mdepth)`.
;;;
;;; MUTATED side records setq/setf/incf/... targets (independent of inside-lambda,
;;; matching find-mutated-vars). CAPTURED side marks a var-names reference seen
;;; while inside-lambda. Crucially, a mutation target symbol is BOTH recorded
;;; (mutated) AND pushed onto the worklist (so it can be capture-marked); the old
;;; captured pass relied on its generic walk pushing the target; dropping that
;;; push would lose the capture mark and silently skip boxing (the
;;; mutation-loss class). Returns (values mutated-names captured-names).
;;;
;;; VAR-NAMES may be the sentinel :ALL, meaning "every referenced symbol is a
;;; capture candidate": used by %boundary-mut-ref to collect the full mutated
;;; and referenced sets of a nested lambda once, so the enclosing walks reuse
;;; them instead of re-descending (O(depth^2) -> O(depth) on nested
;;; closures). Sound to memoize because this walk never consults *locals*:
;;; captures key off explicit references and mutations off setq/place targets,
;;; both pure functions of the form within a top-level compile (unlike the
;;; free-var walk, whose local-bound-p filter is *locals*-dependent).
(defvar *bmr-cache* nil
  "Per-top-level-form EQ memo: lambda form -> (mutated-names . ref-names).")

(defun find-mutated-and-captured-vars-expr (expr var-names mutated-ht captured-ht inside-lambda)
  (let ((worklist (list (cons expr (cons inside-lambda 0))))
        (*lexical-operators* *lexical-operators*))
    (loop while worklist do
      (let* ((item (pop worklist))
             (e (car item))
             (in-lambda (cadr item))
             (mdepth (cddr item)))
        (cond
          ((eq e *mscope-restore-sentinel*)
           (setf *macroexpand-scope* in-lambda))
          ((eq e *lexops-restore-sentinel*)
           (setf *lexical-operators* in-lambda))
          ((and (eq e :restore-symbol-macros) (not (eq in-lambda t)))
           (setf *symbol-macros* in-lambda))
          ((and (eq e :restore-macro) (consp in-lambda))
           (let ((name (car in-lambda))
                 (old-entry (cdr in-lambda)))
             (if old-entry
                 (setf (gethash name *macros*) old-entry)
                 (remhash name *macros*))))
          ;; Bare symbol that is a symbol-macro: mark captured if applicable, and
          ;; ALSO expand it so a mutation hidden in the expansion is still seen
          ;; (fable pitfall #2: do both actions, not either).
          ((and (symbolp e) e
                (not (eq e :restore-symbol-macros)) (not (eq e :restore-macro))
                (< mdepth *macro-expand-depth-limit*)
                (assoc e *symbol-macros* :test #'eq))
           ;; Capture side only expands when the symbol isn't a bound var-name.
           ;; Mutation side always needs the expansion. So: if it IS a var-name,
           ;; mark captured (when in-lambda) but still expand for the mutation walk;
           ;; if it is NOT a var-name, just expand (matches both old passes).
           (when (and in-lambda (or (eq var-names :all)
                                    (member (var-name e) var-names :test #'string=)))
             (setf (gethash (var-name e) captured-ht) t))
           (push (cons (cdr (assoc e *symbol-macros* :test #'eq))
                       (cons in-lambda (1+ mdepth)))
                 worklist))
          ((and (symbolp e) in-lambda)
           (when (or (eq var-names :all)
                     (member (var-name e) var-names :test #'string=))
             (setf (gethash (var-name e) captured-ht) t)))
          ((consp e)
           (let ((head (car e)))
             (cond
               ;; The portable MULTIPLE-VALUE-BIND shape compiles as a LET*
               ;; (%MV-CALL-LAMBDA-BIND), not as a closure: walk it that way.
               ((let ((bind (and (eq head 'multiple-value-call)
                                 (%mv-call-lambda-bind e))))
                  (when bind
                    (push (cons bind (cons in-lambda mdepth)) worklist)
                    t)))
               ;; --- mutation-recording place forms (from find-mutated-vars-expr) ---
               ;; Each records the target into mutated-ht AND pushes subforms/targets
               ;; so the capture walk still sees them.
               ((and (symbolp head) (or (eq head 'setq) (eq head 'setf)
                                        (eq head 'psetq) (eq head 'psetf)))
                (loop for (var val) on (cdr e) by #'cddr
                      do (cond
                           ((and var (symbolp var))
                            (setf (gethash (var-name var) mutated-ht) t)
                            ;; push the target symbol so capture side can mark it
                            (push (cons var (cons in-lambda mdepth)) worklist))
                           ((and (consp var) (eq (car var) 'the) (symbolp (caddr var)))
                            (setf (gethash (var-name (caddr var)) mutated-ht) t)
                            (push (cons var (cons in-lambda mdepth)) worklist))
                           ((consp var)
                            (when (and (or (eq head 'setf) (eq head 'psetf))
                                       (< mdepth *macro-expand-depth-limit*)
                                       (%stack-space-available-p)
                                       (find-macro-expander head))
                              (let* ((single-form `(,head ,var ,val))
                                     (expander (find-macro-expander head))
                                     (expanded (%walker-macroexpand-for-mutation
                                                single-form expander mutated-ht
                                                in-lambda mdepth
                                                (lambda (item) (push item worklist)))))
                                (when expanded
                                  (push (cons expanded (cons in-lambda (1+ mdepth))) worklist))))
                            (push (cons var (cons in-lambda mdepth)) worklist)))
                         (when val (push (cons val (cons in-lambda mdepth)) worklist))))
               ((and (symbolp head) (string= (symbol-name head) "MULTIPLE-VALUE-SETQ"))
                (let ((vars (cadr e)))
                  (when (listp vars)
                    (dolist (v vars)
                      (when (symbolp v)
                        (setf (gethash (var-name v) mutated-ht) t)
                        (push (cons v (cons in-lambda mdepth)) worklist)))))
                (when (caddr e)
                  (push (cons (caddr e) (cons in-lambda mdepth)) worklist)))
               ((and (symbolp head) (member (symbol-name head) '("PUSH" "PUSHNEW") :test #'string=))
                (let ((place (caddr e)))
                  (let ((sym (if (symbolp place) place
                                 (and (consp place) (eq (car place) 'the) (caddr place)))))
                    (when (and sym (symbolp sym) (not (eq sym t)))
                      (setf (gethash (var-name sym) mutated-ht) t))))
                (do-list-safe (sub (cdr e))
                  (push (cons sub (cons in-lambda mdepth)) worklist)))
               ((and (symbolp head) (member (symbol-name head) '("POP" "INCF" "DECF") :test #'string=))
                (let ((place (cadr e)))
                  (let ((sym (if (symbolp place) place
                                 (and (consp place) (eq (car place) 'the) (caddr place)))))
                    (when (and sym (symbolp sym) (not (eq sym t)))
                      (setf (gethash (var-name sym) mutated-ht) t))))
                (do-list-safe (sub (cdr e))
                  (push (cons sub (cons in-lambda mdepth)) worklist)))
               ((and (symbolp head) (member (symbol-name head) '("ROTATEF" "SHIFTF") :test #'string=))
                (dolist (arg (cdr e))
                  (when (and (symbolp arg) arg (not (eq arg t)))
                    (setf (gethash (var-name arg) mutated-ht) t)
                    (push (cons arg (cons in-lambda mdepth)) worklist))
                  (when (consp arg)
                    (push (cons arg (cons in-lambda mdepth)) worklist))))
               ;; --- structural forms (from find-captured-vars-expr) ---
               ((and (symbolp head) (eq head 'quote)) nil)
               ((and (symbolp head) (eq head 'defun))
                (dolist (form (cdddr e))
                  (push (cons form (cons t mdepth)) worklist)))
               ;; Nested lambda: its whole content is inside-lambda, so every
               ;; mutation is a mutation and every reference is a capture
               ;; candidate. Compute both sets once (memoized) and merge, instead
               ;; of re-walking the body once per enclosing lambda.
               ((and (symbolp head) (eq head 'lambda) (listp (cadr e)))
                (multiple-value-bind (mut ref) (%boundary-mut-ref e)
                  (dolist (n mut) (setf (gethash n mutated-ht) t))
                  (dolist (n ref)
                    (when (or (eq var-names :all)
                              (member n var-names :test #'string=))
                      (setf (gethash n captured-ht) t)))))
               ((and (symbolp head) (member head '(let let*)) (listp (cadr e)))
                (let ((bindings (cadr e))
                      (lbody (cddr e))
                      (shadowed nil))
                  (when *symbol-macros*
                    (do-list-safe (b bindings)
                      (let ((name (if (consp b) (car b) b)))
                        (when (and name (symbolp name)
                                   (assoc name *symbol-macros* :test #'eq))
                          (push name shadowed)))))
                  (when shadowed
                    (push (cons :restore-symbol-macros (cons *symbol-macros* mdepth)) worklist))
                  (dolist (form lbody)
                    (push (cons form (cons in-lambda mdepth)) worklist))
                  (when shadowed
                    (push (cons :restore-symbol-macros
                                (cons (remove-if (lambda (sm)
                                                   (member (car sm) shadowed :test #'eq))
                                                 *symbol-macros*)
                                      mdepth))
                          worklist))
                  (dolist (b bindings)
                    (when (and (consp b) (cadr b))
                      (push (cons (cadr b) (cons in-lambda mdepth)) worklist)))))
               ((and (symbolp head) (or (eq head 'flet) (eq head 'labels)) (listp (cadr e)))
                (let ((shadows (%flet-macro-shadows (cadr e))))
                ;; Names that hide a macro are in scope for the body, and for
                ;; LABELS also for the definitions: as in the free-variable walk.
                (flet ((enter ()
                         (push (cons *mscope-restore-sentinel* (cons *macroexpand-scope* mdepth)) worklist)
                         (push (cons *lexops-restore-sentinel* (cons *lexical-operators* mdepth)) worklist)
                         (setf *macroexpand-scope* (cons (cadr e) *macroexpand-scope*)
                               *lexical-operators* (%flet-lexical-operators shadows))))
                (when (and shadows (eq head 'labels)) (enter))
                (dolist (fdef (cadr e))
                  ;; Walk only the initializer forms of the lambda list (the
                  ;; default-value / supplied-p expressions of &optional/&key/&aux),
                  ;; not the raw lambda list. Pushing the lambda list itself as a
                  ;; form macroexpands a param whose name happens to be a macro
                  ;; (e.g. a required param named INST, which is a macro under
                  ;; SBCL's assembler), firing that macro's compile-time side
                  ;; effects. Param names are binding occurrences, not code.
                  ;; Walked with a CONSP loop, not DOLIST: the walker also visits
                  ;; unevaluated subforms, so the lambda-list position is not
                  ;; guaranteed to hold a proper list.
                  (do ((pl (cadr fdef) (cdr pl)))
                      ((not (consp pl)))
                    (let ((p (car pl)))
                      (when (and (consp p) (cadr p))
                        (push (cons (cadr p) (cons t mdepth)) worklist))))
                  (dolist (form (cddr fdef))
                    (push (cons form (cons t mdepth)) worklist)))
                (when (and shadows (eq head 'flet)) (enter))
                (dolist (form (cddr e))
                  (push (cons form (cons in-lambda mdepth)) worklist)))))
               ((and (symbolp head) (eq head 'handler-case))
                (when (cadr e)
                  (push (cons (cadr e) (cons in-lambda mdepth)) worklist))
                (dolist (clause (cddr e))
                  (dolist (form (cddr clause))
                    (push (cons form (cons t mdepth)) worklist))))
               ((and (symbolp head) (eq head 'handler-bind))
                (dolist (binding (cadr e))
                  (when (cadr binding)
                    (push (cons (cadr binding) (cons t mdepth)) worklist)))
                (dolist (form (cddr e))
                  (push (cons form (cons in-lambda mdepth)) worklist)))
               ((and (symbolp head) (eq head 'restart-case))
                (when (cadr e)
                  (push (cons (cadr e) (cons in-lambda mdepth)) worklist))
                (dolist (clause (cddr e))
                  (dolist (form (cddr clause))
                    (push (cons form (cons t mdepth)) worklist))))
               ((and (symbolp head) (eq head 'macrolet))
                (let ((macro-defs (cadr e))
                      (mlbody (cddr e)))
                  (dolist (def macro-defs)
                    (let* ((mname (car def))
                           (old-entry (gethash mname *macros*)))
                      (push (cons :restore-macro (cons (cons mname old-entry) mdepth)) worklist)))
                  (push (cons *mscope-restore-sentinel* (cons *macroexpand-scope* mdepth)) worklist)
                  (push (cons *lexops-restore-sentinel* (cons *lexical-operators* mdepth)) worklist)
                  (setf *macroexpand-scope* (cons macro-defs *macroexpand-scope*))
                  (dolist (def macro-defs)
                    (let* ((mname (car def))
                           (mparams (cadr def))
                           (mbody (cddr def)))
                      (setf (gethash mname *macros*)
                            (%eval-macrolet-expander mparams mbody))))
                  (setf *lexical-operators* (%macrolet-lexical-operators macro-defs))
                  (dolist (form mlbody)
                    (push (cons form (cons in-lambda mdepth)) worklist))))
               ((and (symbolp head) (eq head 'symbol-macrolet))
                (let ((sm-bindings (cadr e))
                      (sm-body (cddr e)))
                  (push (cons :restore-symbol-macros (cons *symbol-macros* mdepth)) worklist)
                  (setf *symbol-macros*
                        (append (mapcar (lambda (b) (cons (car b) (cadr b))) sm-bindings)
                                *symbol-macros*))
                  (dolist (form sm-body)
                    (push (cons form (cons in-lambda mdepth)) worklist))))
               (t
                (let ((expanded nil))
                  (when (and (symbolp head) head
                             (< mdepth *macro-expand-depth-limit*)
                             (%stack-space-available-p)
                             (not (%lexical-function-p head))
                             (find-macro-expander head))
                    (let ((expander (find-macro-expander head)))
                      (setf expanded (%walker-macroexpand-for-mutation
                                      e expander mutated-ht in-lambda mdepth
                                      (lambda (item) (push item worklist))))))
                  (if expanded
                      (push (cons expanded (cons in-lambda (1+ mdepth))) worklist)
                      (do-list-safe (sub e)
                        (push (cons sub (cons in-lambda mdepth)) worklist)))))))))))))

(defun %boundary-mut-ref (lam)
  "Return (values MUT-NAMES REF-NAMES) for lambda form LAM: every symbol name
   mutated anywhere in LAM, and every symbol name referenced anywhere in LAM
   (all inside-lambda, hence all capture candidates). Both are independent of the
   caller's var-names, so this is computed once and memoized by EQ(LAM) in
   *bmr-cache*; the caller intersects REF with its own var-names. Walks (CDR LAM)
   rather than LAM so a nested lambda re-enters through the memoized lambda case
   instead of recursing on itself. NIL cache => uncached (identical result)."
  (flet ((compute ()
           (let ((mut (make-hash-table :test #'equal))
                 (ref (make-hash-table :test #'equal)))
             (dolist (sub (cdr lam))
               (find-mutated-and-captured-vars-expr sub :all mut ref t))
             (let ((ml '()) (rl '()))
               (maphash (lambda (k v) (declare (ignore v)) (push k ml)) mut)
               (maphash (lambda (k v) (declare (ignore v)) (push k rl)) ref)
               (cons ml rl)))))
    (let ((cell (if *bmr-cache*
                    (multiple-value-bind (c present) (gethash lam *bmr-cache*)
                      (if present c (setf (gethash lam *bmr-cache*) (compute))))
                    (compute))))
      (values (car cell) (cdr cell)))))

(defun find-mutated-and-captured-vars (body var-names)
  "One walk computing both sets. Returns (values mutated-names captured-names),
   each a list of variable-name strings. Replaces adjacent find-mutated-vars +
   find-captured-vars calls on the same BODY."
  (let ((mutated-ht (make-hash-table :test #'equal))
        (captured-ht (make-hash-table :test #'equal)))
    (dolist (form body)
      (find-mutated-and-captured-vars-expr form var-names mutated-ht captured-ht nil))
    (let ((mut '()) (cap '()))
      (maphash (lambda (k v) (declare (ignore v)) (push k mut)) mutated-ht)
      (maphash (lambda (k v) (declare (ignore v)) (push k cap)) captured-ht)
      (values mut cap))))

;;; ============================================================
;;; SIL local-reference enumeration (single source of truth)
;;; ============================================================
;;; Every analysis pass that ENUMERATES or REWRITES locals must go through these
;;; helpers. A local-bearing SIL op is described in exactly one place here, so a
;;; new op that carries local KEYs, whether as a direct operand or embedded in a
;;; nested operand list, is taught to every pass by editing only this section.
;;; Authoritative op set (CilAssembler.Emit.cs): :declare-local, :ldloc, :stloc,
;;; :dotnet-call-direct-locals (RECV + ARG list). Background: a missed
;;; nested-operand local once let slot-sharing orphan a merged local into an
;;; "Undeclared local" at assembly time, because each pass scanned for top-level
;;; :ldloc/:stloc independently and none knew the new op carried locals.
;;;
;;; (peephole-optimize is intentionally NOT a client: it pattern-matches fixed
;;;  adjacent op sequences rather than enumerating locals, so an unknown op simply
;;;  fails to match and passes through untouched: safe by construction.)

;;; The DO- macros below are the authoritative description; the list-returning
;;; functions are derived from them. A pass that walks every instruction of a
;;; function body should use the macro: the list form conses a fresh one-element
;;; list per instruction, which at one call per instruction per pass was a
;;; visible share of the compiler's total allocation.

(defmacro do-instr-local-reads ((var instr) &body body)
  "Run BODY with VAR bound to each local KEY INSTR reads, including locals
   carried in nested operand lists. Allocates nothing."
  (let ((i (gensym "INSTR")) (k (gensym "KEY")))
    `(let ((,i ,instr))
       (when (consp ,i)
         (case (car ,i)
           (:ldloc (let ((,var (cadr ,i))) ,@body))
           ;; The debug frame stores (:frame-set NAME KEY) and its box /
           ;; native-rep variants all read KEY.
           ((:frame-set :frame-set-box :frame-set-long :frame-set-double :frame-set-single)
            (let ((,var (caddr ,i))) ,@body))
           ;; (:dotnet-call-direct-locals TYPE METHOD RECV (ARG...) (PARAM...))
           ;; RECV and each ARG are locals read by the call.
           (:dotnet-call-direct-locals
            (let ((,var (nth 3 ,i))) ,@body)
            (dolist (,k (nth 4 ,i)) (let ((,var ,k)) ,@body)))
           (t nil))))))

(defmacro do-instr-local-writes ((var instr) &body body)
  "Run BODY with VAR bound to each local KEY INSTR writes. Allocates nothing."
  (let ((i (gensym "INSTR")))
    `(let ((,i ,instr))
       (when (and (consp ,i) (eq (car ,i) :stloc))
         (let ((,var (cadr ,i))) ,@body)))))

(defmacro do-instr-local-refs ((var instr) &body body)
  "Run BODY with VAR bound to each local KEY INSTR reads or writes (for liveness
   ranges / use counting). Allocates nothing."
  (let ((i (gensym "INSTR")))
    `(let ((,i ,instr))
       (do-instr-local-writes (,var ,i) ,@body)
       (do-instr-local-reads (,var ,i) ,@body))))

(defun instr-local-reads (instr)
  "Local KEYs INSTR reads, including locals carried in nested operand lists."
  (let ((acc '()))
    (do-instr-local-reads (k instr) (push k acc))
    (nreverse acc)))

(defun instr-local-writes (instr)
  "Local KEYs INSTR writes."
  (let ((acc '()))
    (do-instr-local-writes (k instr) (push k acc))
    (nreverse acc)))

(defun instr-local-refs (instr)
  "All local KEYs INSTR reads or writes (for liveness ranges / use counting)."
  (nconc (instr-local-writes instr) (instr-local-reads instr)))

(defun instr-declared-local (instr)
  "If INSTR declares a local, return (KEY . TYPE-STRING); else NIL."
  (when (and (consp instr) (eq (car instr) :declare-local))
    (cons (cadr instr) (caddr instr))))

(defun rewrite-instr-locals (instr rename)
  "Return INSTR with every local KEY mapped through RENAME (a hash-table; a key
   absent from RENAME is left unchanged). Covers :declare-local/:ldloc/:stloc,
   the debug (:frame-set[-box] NAME KEY) stores, and the RECV + ARG locals of
   :dotnet-call-direct-locals. Other instrs are returned unchanged."
  ;; RN is a macrolet, not an flet: this runs once per instruction of every
  ;; function body compiled, and a local function that is also referenced as
  ;; #'RN materializes a closure object on each entry.
  (if (not (consp instr))
      instr
      (macrolet ((rn (k) `(let ((key ,k)) (or (gethash key rename) key))))
        (case (car instr)
          ;; Each case answers INSTR itself when its key is not renamed. Most
          ;; instructions of a body mention a local, and most of those locals are
          ;; not the ones being merged, so rebuilding every one of them was the
          ;; bulk of what this pass allocated.
          (:declare-local (let ((n (rn (cadr instr))))
                            (if (eq n (cadr instr)) instr
                                `(:declare-local ,n ,(caddr instr)))))
          (:ldloc (let ((n (rn (cadr instr))))
                    (if (eq n (cadr instr)) instr `(:ldloc ,n))))
          (:stloc (let ((n (rn (cadr instr))))
                    (if (eq n (cadr instr)) instr `(:stloc ,n))))
          ((:frame-set :frame-set-box :frame-set-long :frame-set-double :frame-set-single)
           (let ((n (rn (caddr instr))))
             (if (eq n (caddr instr)) instr
                 `(,(car instr) ,(cadr instr) ,n))))
          (:dotnet-call-direct-locals
           `(:dotnet-call-direct-locals
             ,(nth 1 instr) ,(nth 2 instr)
             ,(rn (nth 3 instr))
             ,(loop for k in (nth 4 instr) collect (rn k))
             ,(nth 5 instr)))
          (t instr)))))

;;; ============================================================
;;; Copy propagation: eliminate single-reference let locals
;;; ============================================================

(defun eliminate-single-ref-locals (instrs)
  "Peephole: remove :declare-local / :stloc / :ldloc for single-reference locals.
   A local KEY is eligible when:
     - exactly 1 (:stloc KEY) and 1 (:ldloc KEY) appear in INSTRS
     - type in the corresponding :declare-local is \"LispObject\" (not a box array)
     - (:stloc KEY) and (:ldloc KEY) are consecutive in the non-:declare-local
       instruction subsequence (only :declare-local instructions may appear between)
   Preserves CIL stack semantics and evaluation order."
  (let ((stloc-count (make-hash-table :test #'equal))
        (ldloc-count (make-hash-table :test #'equal))
        (local-type  (make-hash-table :test #'equal)))
    (dolist (instr instrs)
      (let ((decl (instr-declared-local instr)))
        (when decl (setf (gethash (car decl) local-type) (cdr decl))))
      ;; Count writes and reads via the central enumerator so locals embedded in
      ;; nested operands (e.g. :dotnet-call-direct-locals) raise the read count and
      ;; correctly disqualify a key from single-ref removal.
      (do-instr-local-writes (k instr) (incf (gethash k stloc-count 0)))
      (do-instr-local-reads (k instr)  (incf (gethash k ldloc-count 0))))
    ;; Eligible keys: single stloc, single ldloc, LispObject type
    (let ((single-ref (make-hash-table :test #'equal)))
      (maphash (lambda (key sc)
                 (when (and (= sc 1)
                            (= (gethash key ldloc-count 0) 1)
                            (string= (gethash key local-type "") "LispObject"))
                   (setf (gethash key single-ref) t)))
               stloc-count)
      (when (zerop (hash-table-count single-ref))
        (return-from eliminate-single-ref-locals instrs))
      ;; Find consecutive (stloc KEY)(ldloc KEY) pairs skipping :declare-local
      (let ((removable (make-hash-table :test #'equal))
            (prev nil))
        (dolist (instr instrs)
          (when (consp instr)
            (let ((op (car instr)) (key (cadr instr)))
              (cond
                ((eq op :declare-local))        ; transparent: don't reset prev
                ((eq op :stloc)
                 (setf prev (and (gethash key single-ref) key)))
                ((eq op :ldloc)
                 (cond
                   ((and prev (equal prev key))
                    (setf (gethash key removable) t)
                    (setf prev nil))
                   (t (setf prev nil))))
                (t (setf prev nil))))))
        (if (zerop (hash-table-count removable))
            instrs
            (remove-if (lambda (instr)
                         (and (consp instr)
                              (gethash (cadr instr) removable)
                              (member (car instr) '(:declare-local :stloc :ldloc))))
                       instrs))))))

;;; ============================================================
;;; A &REST list nobody reads
;;; ============================================================

(defun elide-unused-rest-list (instrs)
  "Delete the construction of a &REST list that the body never reads.

   A &REST parameter binds a freshly consed list of the remaining arguments, and
   the emitter builds it whether or not the body looks at it. It very often does
   not -- (defmethod initialize-instance :after ((x c) &rest initargs) (declare
   (ignore initargs)) ...) is the standard way to write a method that wants the
   protocol but not the arguments, and every such method conses one list per call,
   per level of the class hierarchy.

   The test is on the emitted instructions, not the source: a local that no
   instruction reads cannot be read, whatever a macro in the body expanded to.
   That also makes this self-disabling under debug info emission -- (:frame-set
   NAME KEY) counts as a read, so a build that wants the in-process debugger to
   show the variable keeps the list.

   The shape matched is exactly what COMPILE-ARGS-PARAM-INSTRS emits for a rest
   parameter, ending in the store; anything else is left alone."
  (let ((reads (make-hash-table :test #'equal))
        (dropped (make-hash-table :test #'equal))
        (out '()))
    (dolist (i instrs)
      (do-instr-local-reads (k i) (setf (gethash k reads) t)))
    (dolist (i instrs)
      (let ((prev (first out)) (prev2 (second out)) (prev3 (third out)))
        (if (and (consp i) (eq (car i) :stloc)
                 (not (gethash (cadr i) reads))
                 (consp prev) (eq (car prev) :call)
                 (equal (cadr prev) "Runtime.CollectRestArgs")
                 (consp prev2) (eq (car prev2) :ldc-i4)
                 (consp prev3) (eq (car prev3) :ldarg))
            (progn (setf (gethash (cadr i) dropped) t)
                   (setq out (cdddr out)))
            (push i out))))
    (if (zerop (hash-table-count dropped))
        instrs
        (remove-if (lambda (i)
                     (and (consp i) (eq (car i) :declare-local)
                          (gethash (cadr i) dropped)))
                   (nreverse out)))))

;;; ============================================================
;;; Slot sharing: merge LispObject locals with disjoint flat ranges
;;; ============================================================

(defparameter +mv-primary-twins+
  '(("Runtime.Gethash"      . "Runtime.GethashPrimary")
    ("Runtime.FloorOp"      . "Runtime.FloorOpPrimary")
    ("Runtime.TruncateOp"   . "Runtime.TruncateOpPrimary")
    ("Runtime.CeilingOp"    . "Runtime.CeilingOpPrimary")
    ("Runtime.RoundOp"      . "Runtime.RoundOpPrimary")
    ;; A written (VALUES A B), which is what the tail of every two-value function
    ;; is. In single-value position it is the same call pair as the entries above.
    ("Runtime.Values2"      . "Runtime.Values2Primary"))
  "Runtime entries that return two values, paired with an entry that returns only
   the primary. The peephole (P12) swaps in the second when the call is followed
   by Runtime.UnwrapMv, i.e. when the call site is in single-value position.

   A pair belongs here only when `TWIN(x)` is observably identical to
   `UnwrapMv(ORIGINAL(x))` -- same computation, same thread value state (one
   value, the primary), same conditions signalled in the same order. The twins
   are written as two entries over one core in the runtime so that stays true.

   Not a DEFCONSTANT: a list literal under DEFCONSTANT is re-evaluated to a
   fresh, non-EQL list when the file is reloaded, which SBCL rejects.")

(defun peephole-optimize (instrs)
  "Local peephole pass over a finalized SIL instruction list. Removes
   instruction sequences that codegen emits but that are semantically
   no-ops, iterating to a fixpoint so cascades collapse.

   Patterns:
     P1  (:ldloc X) (:stloc X)        ->  {}          ; dead self-copy (n-ary
                                                       ; arithmetic lowering)
     P2  (:dup) (:stloc X) (:pop)     ->  (:stloc X)   ; discarded-assignment
                                                       ; idiom (setq/dolist/loop
                                                       ; in statement position)
     P3  (:ldsfld \"Nil.Instance\")
           (:call \"Runtime.UnwrapMv\") -> (:ldsfld \"Nil.Instance\")  ; UnwrapMv of a
                                          ; statically-Nil value is identity with no
                                          ; side effect (Nil is not an MvReturn)
     P4  (:ldsfld \"Nil.Instance\") (:pop) -> {}        ; push-Nil-then-discard is dead
     P5  (:call \"Fixnum.Make\") (:pop)   ->  (:pop)    ; boxing a value only to
                                          ; discard it: Fixnum.Make is pure, so
                                          ; pop the raw long instead. (Int64-slot
                                          ; setq in statement position; composes
                                          ; with P2 to a bare native store.)
     P6  (:newobj \"DoubleFloat\") (:pop) ->  (:pop)    ; float sibling of P5: the
     P6  (:newobj \"SingleFloat\") (:pop) ->  (:pop)    ; DoubleFloat/SingleFloat
     P6  (:newobj \"LispDecimal\") (:pop) ->  (:pop)   ; and the decimal slot's box
                                          ; ctor is pure (value + alloc counter),
                                          ; so boxing a discarded native float
                                          ; store result is dead: pop the raw r8
                                          ; instead. (Float-array setf in
                                          ; statement position.)
     P8  (:newobj \"LispString\") (:pop)  ->  (:pop)    ; string sibling of P6
     P9  (:ldstr S) (:pop)              -> {}          ; dead string constant.
                                          ; P8+P9 compose to delete a string
                                          ; literal in statement position: a
                                          ; documentation string compiles to
                                          ; exactly that, and without this
                                          ; allocates a LispString per call.
     P10 (:newobj \"DoubleFloat\") (:unbox-double) -> {}  ; box a native value and
     P10 (:call \"Fixnum.Make\") (:unbox-fixnum)   -> {}  ; take it straight back
                                          ; out. The unbox casts to the type the
                                          ; box was just made as and reads the
                                          ; field the ctor just wrote, so the
                                          ; pair is identity. Every assignment
                                          ; into a native slot whose value came
                                          ; from a Lisp-object expression had
                                          ; this shape.
     P11 UnwrapMv UnwrapMv              -> UnwrapMv    ; the second cannot do
                                          ; anything: values never nest, so the
                                          ; first call's result is never an
                                          ; MvReturn. Every LOOP body iteration
                                          ; carried the pair.
     P12 (:call F) (:call \"Runtime.UnwrapMv\")  ->  (:call F-primary)
                                          ; F returns two values and the unwrap
                                          ; throws the second away. The twin
                                          ; entry publishes the primary and
                                          ; returns it without building the
                                          ; MvReturn: 40 B off every GETHASH /
                                          ; FLOOR / TRUNCATE / CEILING / ROUND
                                          ; in single-value position. Pairs are
                                          ; in +mv-primary-twins+.
   (P3+P4 compose across the fixpoint to delete the dead nil/unwrap/pop preamble
    that codegen emits at the top of every TCO loop body.)

   Matches only strictly-adjacent instructions. A :label (the only branch
   target form in SIL) between instructions breaks adjacency in the list, so
   it naturally blocks a match; no control-flow analysis needed, and the
   rewrites are valid even inside loops/TCO."
  (let ((changed t))
    (loop while changed do
      (setf changed nil)
      (let ((out '())
            (cur instrs))
        (loop while cur do
          (let ((i1 (first cur))
                (i2 (second cur))
                (i3 (third cur)))
            (cond
              ;; P1: load a local then immediately store it back to itself.
              ((and (consp i1) (eq (car i1) :ldloc)
                    (consp i2) (eq (car i2) :stloc)
                    (equal (cadr i1) (cadr i2)))
               (setf changed t)
               (setf cur (cddr cur)))
              ;; P2: dup a value, store it, discard the duplicate. The dup/pop
              ;; bracket cancels: stack-equivalent to a bare store. (Assignment
              ;; forms leave their value on the stack; in statement position it
              ;; is then popped, so codegen emits dup;stloc;pop.)
              ((and (consp i1) (eq (car i1) :dup)
                    (consp i2) (eq (car i2) :stloc)
                    (consp i3) (eq (car i3) :pop))
               (setf changed t)
               (push i2 out)
               (setf cur (cdddr cur)))
              ;; P3: UnwrapMv of a statically-Nil value is a no-op identity call.
              ((and (consp i1) (eq (car i1) :ldsfld) (equal (cadr i1) "Nil.Instance")
                    (consp i2) (eq (car i2) :call) (equal (cadr i2) "Runtime.UnwrapMv"))
               (setf changed t)
               (push i1 out)
               (setf cur (cddr cur)))
              ;; P4: push a Nil constant then immediately discard it: dead.
              ((and (consp i1) (eq (car i1) :ldsfld) (equal (cadr i1) "Nil.Instance")
                    (consp i2) (eq (car i2) :pop))
               (setf changed t)
               (setf cur (cddr cur)))
              ;; P5: box a raw long only to discard it. Fixnum.Make is pure;
              ;; drop the call and pop the operand instead.
              ((and (consp i1) (eq (car i1) :call) (equal (cadr i1) "Fixnum.Make")
                    (consp i2) (eq (car i2) :pop))
               (setf changed t)
               (push i2 out)
               (setf cur (cddr cur)))
              ;; P6: box a native float only to discard it. The DoubleFloat /
              ;; SingleFloat ctor is pure (stores value, bumps the alloc counter),
              ;; so drop the newobj and pop the raw r8/r4 operand instead.
              ((and (consp i1) (eq (car i1) :newobj)
                    (member (cadr i1) '("DoubleFloat" "SingleFloat" "LispDecimal") :test #'equal)
                    (consp i2) (eq (car i2) :pop))
               (setf changed t)
               (push i2 out)
               (setf cur (cddr cur)))
              ;; P7: UnwrapMv of a freshly-boxed float is identity: a DoubleFloat
              ;; / SingleFloat is never an MvReturn. Codegen wraps a setf result
              ;; in UnwrapMv (twice, in a statement-position dotimes body); drop it
              ;; so the newobj becomes adjacent to the pop and P6 can then delete
              ;; the whole dead box.
              ((and (consp i1) (eq (car i1) :newobj)
                    (member (cadr i1) '("DoubleFloat" "SingleFloat" "LispDecimal") :test #'equal)
                    (consp i2) (eq (car i2) :call) (equal (cadr i2) "Runtime.UnwrapMv"))
               (setf changed t)
               (push i1 out)
               (setf cur (cddr cur)))
              ;; P7 for a boxed fixnum: same identity, same reason. This was left
              ;; out on the reading that Fixnum.Make's small-integer cache makes
              ;; the surviving box free, and that is only true below 65536 -- a
              ;; loop counter stored into a struct slot allocated one box per
              ;; iteration past that (measured: 934,467 Fixnums in 1,000,000
              ;; iterations). With the UnwrapMv gone, P5 reaches the pop and the
              ;; box disappears in statement position.
              ((and (consp i1) (eq (car i1) :call) (equal (cadr i1) "Fixnum.Make")
                    (consp i2) (eq (car i2) :call) (equal (cadr i2) "Runtime.UnwrapMv"))
               (setf changed t)
               (push i1 out)
               (setf cur (cddr cur)))
              ;; P8: wrap a raw string only to discard it. The LispString ctor is
              ;; pure, so drop the newobj and pop the raw string instead.
              ((and (consp i1) (eq (car i1) :newobj) (equal (cadr i1) "LispString")
                    (consp i2) (eq (car i2) :pop))
               (setf changed t)
               (push i2 out)
               (setf cur (cddr cur)))
              ;; P10: box a native value and immediately take it back out. The
              ;; unbox is a castclass to the type the box was just made as, then
              ;; a read of the field the ctor just wrote, so the pair returns the
              ;; operand unchanged and the box is garbage the moment it exists.
              ;; This is the shape every assignment into a native slot has when
              ;; the value came from an expression that produces a Lisp object:
              ;; (setq z1 (+ ...)) on a DOUBLE-FLOAT local emitted
              ;; newobj DoubleFloat / unbox-double / stloc, one allocation per
              ;; assignment in the loop it was declared to make allocation-free.
              ((and (consp i1) (consp i2)
                    (or (and (eq (car i1) :newobj) (eq (car i2) :unbox-double)
                             (equal (cadr i1) "DoubleFloat"))
                        (and (eq (car i1) :newobj) (eq (car i2) :unbox-single)
                             (equal (cadr i1) "SingleFloat"))
                        (and (eq (car i1) :newobj) (eq (car i2) :unbox-decimal)
                             (equal (cadr i1) "LispDecimal"))
                        (and (eq (car i1) :call) (eq (car i2) :unbox-fixnum)
                             (equal (cadr i1) "Fixnum.Make"))))
               (setf changed t)
               (setf cur (cddr cur)))
              ;; P11: unwrap a multiple-value return twice in a row. The second
              ;; call cannot do anything: values never nest, so what the first
              ;; call returns is never an MvReturn and the second one hands it
              ;; straight back, side effect included (there is one -- it narrows
              ;; the thread's value state -- and the first call already did it).
              ;; The pair appears where a form that unwraps its own result sits in
              ;; statement position, which is every iteration of a LOOP body.
              ((and (consp i1) (eq (car i1) :call) (equal (cadr i1) "Runtime.UnwrapMv")
                    (consp i2) (eq (car i2) :call) (equal (cadr i2) "Runtime.UnwrapMv"))
               (setf changed t)
               (push i1 out)
               (setf cur (cddr cur)))
              ;; P12: a call that returns two values, followed immediately by the
              ;; unwrap that throws the second one away. Both halves are then
              ;; equivalent to the single-value twin entry, which publishes the
              ;; primary and returns it without building the MvReturn -- 40 B on
              ;; every such call. The table is the whole list of pairs; a name
              ;; that is not in it is left alone.
              ((and (consp i1) (eq (car i1) :call)
                    (consp i2) (eq (car i2) :call) (equal (cadr i2) "Runtime.UnwrapMv")
                    (assoc (cadr i1) +mv-primary-twins+ :test #'equal))
               (setf changed t)
               (push (list :call (cdr (assoc (cadr i1) +mv-primary-twins+ :test #'equal)))
                     out)
               (setf cur (cddr cur)))
              ;; P9: push a string constant then immediately discard it: dead.
              ;; Composes with P8 to delete a string literal in statement
              ;; position, which is what a documentation string compiles to: it
              ;; would otherwise allocate a fresh LispString on every call.
              ((and (consp i1) (eq (car i1) :ldstr)
                    (consp i2) (eq (car i2) :pop))
               (setf changed t)
               (setf cur (cddr cur)))
              (t
               (push i1 out)
               (setf cur (cdr cur))))))
        (setf instrs (nreverse out))))
    (box-native-float-locals-out-of-line instrs)))

(defun box-native-float-locals-out-of-line (instrs)
  "Rewrite (:ldloc K) (:newobj \"DoubleFloat\") to (:ldloc K) (:call
   \"DoubleFloat.Box\"), and the same for SingleFloat. Runs once, after the
   peephole fixpoint, so P6 / P7 / P10 have already seen the newobj form.

   The pair is a generic read of a native float local, typically the value a
   float loop returns. Under an inline newobj the local is live across the
   allocation helper call; the System V x64 ABI has no callee-saved XMM
   registers, so the JIT gives the local a stack home for its whole lifetime
   and stores and reloads it on every loop iteration, on the loop-carried
   chain. Passing it to an out-of-line helper ends the lifetime at the call."
  (let ((out '()))
    (loop with prev = nil
          for i in instrs
          do (push (if (and (consp i) (eq (car i) :newobj)
                            (consp prev) (eq (car prev) :ldloc))
                       (cond ((equal (cadr i) "DoubleFloat")
                              (list :call "DoubleFloat.Box"))
                             ((equal (cadr i) "SingleFloat")
                              (list :call "SingleFloat.Box"))
                             (t i))
                       i)
                   out)
             (setf prev i))
    (nreverse out)))

(defun merge-disjoint-locals (instrs)
  "Linear-scan slot-share locals, then peephole-optimize. Thin wrapper so all
   callers get the peephole pass; the slot-merge logic lives in
   %merge-disjoint-locals. Peephole runs AFTER slot merging: %merge dedups
   :declare-local entries, which can bring an (:ldloc X)(:stloc X) pair
   (separated by a declare in the raw stream) into adjacency where the
   peephole can collapse it."
  ;; Under debug info emission, skip slot sharing so each source variable keeps
  ;; its own physical slot (a coalesced slot would host several source vars over
  ;; its lifetime, which a method-wide PDB LocalVariable name can't represent).
  ;; The classic "debug builds don't reuse slots" tradeoff. Peephole still runs.
  (peephole-optimize
   (if *emit-source-lines*
       (elide-unused-rest-list instrs)
       (%merge-disjoint-locals (elide-unused-rest-list instrs)))))

(defconstant +slot-merge-min-locals+ 32
  "Fewest LispObject locals a body must declare before slot sharing runs at all.")

(defvar *slot-merge-off* :unread
  "Diagnostic: when DOTCL_NO_SLOT_MERGE is set, %MERGE-DISJOINT-LOCALS becomes the
   identity. Read from the environment once and cached, NOT a special variable a
   script can SETQ: the compiled compiler's own specials are different symbol
   objects from the ones the reader interns, so a (setq dotcl.cil-compiler::*x* nil)
   in a loaded file silently does nothing (same reason %MAYBE-DUMP-DEFUN-SIL is
   gated by an env var).

   It exists because measuring this pass by editing it is confounded: the compiler
   compiles itself, so an edited pass also changes the code doing the measuring.")

(defun %slot-merge-off-p ()
  (when (eq *slot-merge-off* :unread)
    (setf *slot-merge-off*
          (unless *cross-compiling*
            (let ((v (%getenv "DOTCL_NO_SLOT_MERGE")))
              (and (stringp v) (> (length v) 0))))))
  *slot-merge-off*)

(defvar *slot-merge-stage* :unread
  "Diagnostic: DOTCL_SLOT_MERGE_STAGE=1 stops after the pre-scan, =2 after the
   live-range analysis, unset runs the whole pass. Lets the cost of each half be
   measured from ONE build, which is what the self-compiling compiler requires.")

(defun %slot-merge-stage ()
  (when (eq *slot-merge-stage* :unread)
    (setf *slot-merge-stage*
          (unless *cross-compiling*
            (let ((v (%getenv "DOTCL_SLOT_MERGE_STAGE")))
              (and (stringp v) (> (length v) 0) (parse-integer v :junk-allowed t))))))
  *slot-merge-stage*)

(defun %merge-disjoint-locals (instrs)
  (when (%slot-merge-off-p)
    (return-from %merge-disjoint-locals instrs))
  "Linear-scan slot sharing: merge LispObject locals whose flat live ranges
   do not overlap. When last-use(K1) < first-def(K2) in flat instruction order,
   K2 can reuse K1's slot. Reduces local variable count across exclusive cond arms.
   Applied once per function body. Does NOT recurse into nested :body lists.
   Skipped entirely when any backward branch is present (loops, TCO)."
  ;; Cheap pre-pass, allocating nothing: a body with fewer than two LispObject
  ;; locals has nothing to merge, and one with no branch instruction cannot have a
  ;; backward branch. Both are the common case (every small function), and the work
  ;; below builds six hash tables and rebuilds the instruction list twice before it
  ;; can discover that it has nothing to do. Slot merging was a quarter of all the
  ;; conses a trivial COMPILE allocated.
  (let ((lispobj-declares 0)
        (any-branch nil))
    (dolist (instr instrs)
      (when (consp instr)
        (cond ((eq (car instr) :declare-local)
               (when (and (stringp (caddr instr)) (string= (caddr instr) "LispObject"))
                 (incf lispobj-declares)))
              ((member (car instr) '(:br :brtrue :brfalse :leave))
               (setq any-branch t)))))
    ;; Below the threshold the pass is not worth what it costs. Measured on 400
    ;; DEFUNs with LET*/COND bodies: slot sharing is 27% of everything COMPILE-FILE
    ;; allocates (372 -> 272 MB) and buys 0.5% off the fasl, with no measurable
    ;; difference in how fast the compiled code runs (takl / deriv / richards, both
    ;; orders, differences inside the noise). What it IS for is the body with
    ;; hundreds of locals, where the IL local count is a real limit -- so keep it
    ;; there and skip it for the small functions that make up almost every file.
    (when (or (< lispobj-declares +slot-merge-min-locals+)
              (eql (%slot-merge-stage) 1))
      (return-from %merge-disjoint-locals instrs))
    ;; Bail out if any backward branch is present. A backward branch targets a label
    ;; whose position <= the branch's own position. :leave counts: a tagbody that
    ;; elides its GoException try/catch (compile-tagbody no-catch path) uses
    ;; (:leave loop-label) for its backward loop edge and has no trailing
    ;; (:br loop-label), so :leave is the only backward-branch signal. Missing it lets
    ;; the linear scan treat a loop as straight-line code and wrongly merge
    ;; live-overlapping slots. Forward :leave (block / handler-case exit) has
    ;; target > position and does not trip this.
    (when any-branch
      (let ((label-pos (make-hash-table :test #'equal))
        (scan-pos 0))
    (dolist (instr instrs)
      (when (and (consp instr) (eq (car instr) :label))
        (setf (gethash (cadr instr) label-pos) scan-pos))
      (incf scan-pos))
    (let ((fwd-pos 0))
      (dolist (instr instrs)
        (when (and (consp instr)
                   (member (car instr) '(:br :brtrue :brfalse :leave))
                   (let ((tgt (gethash (cadr instr) label-pos)))
                     (and tgt (<= tgt fwd-pos))))
          (return-from %merge-disjoint-locals instrs))
        (incf fwd-pos))))))
  (let ((first-pos  (make-hash-table :test #'equal))
        (last-pos   (make-hash-table :test #'equal))
        (local-type (make-hash-table :test #'equal))
        (pos 0))
    ;; Pass 1: collect types and compute [first-pos, last-pos] for each key.
    ;; do-instr-local-refs visits every local a key reads/writes, including
    ;; those embedded in nested operand lists (:dotnet-call-direct-locals), so
    ;; their live ranges extend to the using op and the slot-share scan won't
    ;; merge another local over a still-live nested reference.
    (dolist (instr instrs)
      (let ((decl (instr-declared-local instr)))
        (when decl (setf (gethash (car decl) local-type) (cdr decl))))
      (do-instr-local-refs (key instr)
        (unless (gethash key first-pos)
          (setf (gethash key first-pos) pos))
        (setf (gethash key last-pos) pos))
      (incf pos))
    ;; Collect eligible candidates: LispObject type with at least one use
    (let ((candidates nil))
      (maphash (lambda (key type)
                 (when (and (string= type "LispObject")
                            (gethash key first-pos)
                            (gethash key last-pos))
                   (push (list (gethash key first-pos)
                               (gethash key last-pos)
                               key)
                         candidates)))
               local-type)
      (when (< (length candidates) 2)
        (return-from %merge-disjoint-locals instrs))
      ;; Sort by first-pos ascending
      (setf candidates (sort candidates #'< :key #'first))
      ;; Linear scan: for each key in order, find an expired free slot to reuse
      ;; free-slots: list of (last-pos . canonical-key) cons cells
      (let ((rename (make-hash-table :test #'equal))
            (free-slots nil))
        (dolist (cand candidates)
          (let* ((fp (first cand))
                 (lp (second cand))
                 (key (third cand))
                 (slot (find-if (lambda (s) (< (car s) fp)) free-slots)))
            (if slot
                (let ((canonical (cdr slot)))
                  (setf (gethash key rename) canonical)
                  (setf free-slots (delete slot free-slots :test #'eq))
                  (push (cons lp canonical) free-slots))
                (push (cons lp key) free-slots))))
        (when (or (zerop (hash-table-count rename)) (eql (%slot-merge-stage) 2))
          (return-from %merge-disjoint-locals instrs))
        ;; Pass 2: apply RENAME to every local (central rewriter handles stloc/
        ;; ldloc/declare-local and nested :dotnet-call-direct-locals locals), then
        ;; drop duplicate :declare-local entries that collapsed onto a shared slot.
        ;; One pass, not MAPCAR followed by REMOVE: the pair rebuilt the whole
        ;; instruction list twice.
        (let ((seen-declare (make-hash-table :test #'equal))
              (out (quote ())))
          (dolist (instr instrs (nreverse out))
            (let* ((new (rewrite-instr-locals instr rename))
                   (decl (instr-declared-local new)))
              (cond ((null decl) (push new out))
                    ((gethash (car decl) seen-declare))   ; duplicate declare: drop
                    (t (setf (gethash (car decl) seen-declare) t)
                       (push new out))))))))))

;;; ============================================================
;;; Top-level compilation
;;; ============================================================

(defun compile-toplevel (expr)
  "Compile a top-level expression. Returns instruction list.

   The peephole runs here for the same reason it runs on every function body:
   codegen emits the boxed form of a native-slot store and lets the pass delete
   the box when the value is discarded. Without it, a declared FIXNUM loop
   written at top level -- a script, or the REPL -- boxed its counter once per
   iteration and threw the box away, 16 B/iteration that the same loop inside a
   DEFUN did not pay. Slot sharing is deliberately not run alongside it: it saves
   slots rather than allocation, and top-level forms rarely reach its threshold."
  (let ((*cstate* (cstate-with *cstate*
                               +cs-locals+ '() +cs-block-tags+ '() +cs-go-tags+ '()
                               +cs-boxed-vars+ '() +cs-local-functions+ '()))
        (*var-counter* 0)
        (*label-counter* 0)
        (*specials* '())
        (*at-toplevel* t)
        (*toplevel-segments-p* nil)
        (*macroexpand-scope* '())
        (*macroexpand-cache* (make-hash-table :test #'eq))
        (*bmr-cache* (make-hash-table :test #'eq))
        (*ffv-free-cache* (make-hash-table :test #'eq))
        (*ltv-hoisted* (and *compile-file-mode* *current-module-id* (list '()))))
    (let ((body (compile-expr expr)))
      `(,@(peephole-optimize (append (%compile-hoisted-ltvs) body))
        (:ret)))))

(defun %compile-hoisted-ltvs ()
  "The instructions that run the LOAD-TIME-VALUE forms collected in
   *LTV-HOISTED*, in the order they were met. A form compiled here may contain
   LOAD-TIME-VALUE itself; those run before it."
  (let ((out '()))
    (loop
      (let ((sets (and *ltv-hoisted* (reverse (car *ltv-hoisted*)))))
        (when (null sets) (return out))
        (setf (car *ltv-hoisted*) '())
        (let ((instrs `(,@(compile-expr `(progn ,@sets nil)) (:pop))))
          (setf out (append instrs out)))))))

(defun compile-toplevel-eval (expr)
  "Compile a top-level expression for EVAL.
   Like compile-toplevel but preserves MvReturn at the tail so EVAL's
   caller can observe the form's multiple values. An oversized top-level progn
   comes back as several method bodies joined by (:TOPLEVEL-BOUNDARY), which
   EVAL's assembler runs in sequence."
  (let ((*cstate* (cstate-with *cstate*
                               +cs-locals+ '() +cs-block-tags+ '() +cs-go-tags+ '()
                               +cs-boxed-vars+ '() +cs-local-functions+ '()))
        (*var-counter* 0)
        (*label-counter* 0)
        (*specials* '())
        (*at-toplevel* t)
        (*toplevel-segments-p* t)
        (*in-tail-position* t)
        (*macroexpand-scope* '())
        (*macroexpand-cache* (make-hash-table :test #'eq))
        (*bmr-cache* (make-hash-table :test #'eq))
        (*ffv-free-cache* (make-hash-table :test #'eq))
        ;; Code EVAL (or COMPILE) runs now, also when a macro calls it while
        ;; COMPILE-FILE compiles a top level form: its LOAD-TIME-VALUE forms
        ;; belong to it, not to the fasl being written.
        (*ltv-hoisted* nil))
    (%close-toplevel-segments (compile-expr expr))))
