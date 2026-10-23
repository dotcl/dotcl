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
;; COMPILE-FORM (*COMPILE-DEPTH-LIMIT*), which counts every nested form
;; including each expansion: any form the compiler accepts is then walked
;; completely. Code walkers that wrap each subform in their own macro
;; (cl-environments' %WALK-FORM) reach more than 50 in ordinary code.
(defvar *macro-expand-depth-limit* *compile-depth-limit*)

;; Stub for cross-compilation: always returns T (no stack limit during self-compile)
(unless (fboundp '%stack-space-available-p)
  (defun %stack-space-available-p () t))

;;; find-free-vars-expr: iterative version using explicit worklist.
;;; Each worklist entry is (expr bound . mdepth).
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
;; Bound to a cons whose car is the table, made on first use.
(defvar *ffv-free-cache* nil)

;; Marker heading a (MARKER . TAG) free-var candidate: a GO to TAG seen while
;; collecting candidates, resolved to the target tagbody's id variable at merge.
(defvar *ffv-go-candidate* (list '#:go-candidate))

(defun %ffv-seen (sym free)
  (let ((table (car free)))
    (and table (gethash sym table))))

(defun %ffv-add (sym free)
  (setf (gethash sym (or (car free)
                         (setf (car free) (make-hash-table :test #'eq))))
        sym))

(defun %ffv-collected (free)
  (let ((syms '()))
    (when (car free)
      (maphash (lambda (k v) (declare (ignore k)) (push v syms)) (car free)))
    syms))

(defun %ffv-note-go (tag bnd free-ht)
  "A GO to TAG seen by the free-variable walk: when TAG names a tagbody outside
   the lambda being analysed, that tagbody's id variable is captured."
  (let ((entry (assoc tag (cstate-go-tags))))
    (when entry
      (let* ((tb-var-name (second entry))
             (tb-sym (compiler-name-sym tb-var-name)))
        (when (and (not (member tb-var-name bnd :test #'string=))
                   (not (%ffv-seen tb-sym free-ht))
                   (local-bound-p tb-sym))
          (%ffv-add tb-sym free-ht))))))

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

(defun %walk-macrolet-lexical-operators (defs)
  "*LEXICAL-OPERATORS* with the MACROLET definitions DEFS bound, for a walk."
  (%macrolet-lexical-operators
   defs
   (mapcar (lambda (d)
             (and (consp d) (symbolp (car d)) (consp (cdr d))
                  (%eval-macrolet-expander (cadr d) (cddr d))))
           defs)))

(defun find-free-vars-expr (expr bound free-ht)
  "Walk expr finding free variable references. Results accumulated in free-ht.
   Iterative worklist version; no recursion depth limit."
  (let ((worklist (list (cons expr (cons bound 0))))
        (inline-plans-seen '())
        ;; The walk assigns these as it enters and leaves scopes: bound here,
        ;; so the assignments stay on this thread.
        (*lexical-operators* *lexical-operators*)
        (*macroexpand-scope* *macroexpand-scope*)
        (*symbol-macros* *symbol-macros*))
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
                                 (not (%ffv-seen e free-ht))
                                 (local-bound-p e))
                        (%ffv-add e free-ht)))))
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
                                            (not (%ffv-seen sym free-ht))
                                            (or *ffv-assume-bound* (local-bound-p sym)))
                                   (%ffv-add sym free-ht)))
                                (*ffv-assume-bound* (%ffv-add sym free-ht))
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
                           (%ffv-add m free-ht))
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
                            (tag-sym (compiler-name-sym tag-var)))
                       (when (and (not (member tag-var bnd :test #'string=))
                                  (not (%ffv-seen tag-sym free-ht))
                                  (local-bound-p tag-sym))
                        (%ffv-add tag-sym free-ht)))
                     (when (caddr e)
                       (push (cons (caddr e) (cons bnd mdepth)) worklist)))
                    ;; return: (return expr) = (return-from nil expr)
                    ((and (symbolp head) (eq head 'return))
                     (let* ((tag-var (block-tag-var-name nil))
                            (tag-sym (compiler-name-sym tag-var)))
                       (when (and (not (member tag-var bnd :test #'string=))
                                  (not (%ffv-seen tag-sym free-ht))
                                  (local-bound-p tag-sym))
                        (%ffv-add tag-sym free-ht)))
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
                                 (nm-sym (compiler-name-sym nm)))
                            (when (and (not (member nm bnd :test #'string=))
                                       (not (%ffv-seen nm-sym free-ht))
                                       (or *ffv-assume-bound* (local-bound-p nm-sym)))
                              (%ffv-add nm-sym free-ht))))
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
                                    (let ((nm-sym (compiler-name-sym nm)))
                                      (when (and (not (member nm bnd :test #'string=))
                                                 (not (%ffv-seen nm-sym free-ht)))
                                        (%ffv-add nm-sym free-ht))))))
                              (when (and arg (or (not (eq arg t)) (local-bound-p arg))
                                         (not (special-var-p arg))
                                         (local-bound-p arg))
                                (let* ((plain-name (symbol-name arg))
                                       (mangled-name (concatenate 'string "__LABELFN_" plain-name))
                                       (capture-name (cond
                                                       ((local-bound-name-p mangled-name)
                                                        mangled-name)
                                                       (t plain-name))))
                                  (let ((capture-sym (compiler-name-sym capture-name)))
                                    (when (and (not (member capture-name bnd :test #'string=))
                                               (not (%ffv-seen capture-sym free-ht)))
                                      (%ffv-add capture-sym free-ht))))))))))
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
                    ;; macrolet: bind the macros, push body, push restore sentinels
                    ((and (symbolp head) (eq head 'macrolet))
                     (let ((macro-defs (cadr e))
                           (mlbody (cddr e)))
                       ;; Restore sentinels FIRST (LIFO: processed LAST, after body).
                       ;; Pop-scope sentinel + push this macrolet's scope marker, so a
                       ;; form shared between this shadowing scope and an outer scope is
                       ;; cached per scope and the analysis walk agrees with code-gen.
                       ;; Same source MACRO-DEFS cons as compile-macrolet.
                       (push (cons *mscope-restore-sentinel* *macroexpand-scope*) worklist)
                       (push (cons *lexops-restore-sentinel* *lexical-operators*) worklist)
                       (setf *macroexpand-scope* (cons macro-defs *macroexpand-scope*))
                       (setf *lexical-operators* (%walk-macrolet-lexical-operators macro-defs))
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
                                      (mangled (concatenate 'string "__LABELFN_" name)))
                                 (when (and (or *ffv-assume-bound* (local-bound-name-p mangled))
                                            (not (member mangled bnd :test #'string=)))
                                   (let ((mangled-sym (compiler-name-sym mangled)))
                                     (unless (%ffv-seen mangled-sym free-ht)
                                       (%ffv-add mangled-sym free-ht)))
                                   ;; A substituted copy of an INLINE local
                                   ;; function reads what its body names.
                                   (let ((plan (and (boundp '*cstate*)
                                                    (nth 5 (local-function-entry head)))))
                                     (when (and plan (not (member plan inline-plans-seen :test #'eq)))
                                       (push plan inline-plans-seen)
                                       (destructuring-bind (pname pparams pdecls pbody pblockp &rest more)
                                           plan
                                         (declare (ignore more))
                                         (push (cons `(let ,(mapcar (lambda (v) (list v nil)) pparams)
                                                        ,@pdecls
                                                        ,@(if pblockp `((block ,pname ,@pbody)) pbody))
                                                     (cons nil mdepth))
                                               worklist)))))))
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
         (free-ht (list nil))
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
    (%ffv-collected free-ht)))

(defun %lambda-free-candidates (e)
  "Memoized wrapper over %compute-free-candidates, keyed by EQ(E) in
   *ffv-free-cache*. Within one top-level form EQ identity of E implies an
   identical lexical macro/symbol-macro scope, and the candidate set is
   *locals*-independent, so caching is a behaviour-preserving performance
   transform."
  (if *ffv-free-cache*
      (let ((table (or (car *ffv-free-cache*)
                       (setf (car *ffv-free-cache*) (make-hash-table :test #'eq)))))
        (multiple-value-bind (cached present) (gethash e table)
          (if present
              cached
              (setf (gethash e table) (%compute-free-candidates e)))))
      (%compute-free-candidates e)))

;;; ============================================================
;;; Mutated/captured variable analysis (for boxing)
;;; ============================================================

;;; find-mutated-and-captured-vars: which of a set of variables a body assigns,
;;; and which it references from inside a closure. The two together decide
;;; boxing.
;;;
;;; The walk does not depend on the variables asked about, so it computes a
;;; SUMMARY of a form once and keeps it: three lists of symbols, every symbol
;;; assigned anywhere in the form (MUTATED), every symbol referenced under a
;;; closure boundary inside the form (REF-IN), and every symbol referenced
;;; outside one (REF-OUT). A query intersects a summary with its own names: a
;;; form placed inside a closure contributes REF-IN and REF-OUT as captures, a
;;; form outside one only REF-IN.
;;;
;;; The forms a later query asks about are the bodies of LET, LAMBDA, FLET and
;;; the like, so those are where summaries are kept (in *BMR-CACHE*, keyed by
;;; EQ). An outer LET is compiled first; its walk leaves a summary for every
;;; body form nested in it, and the inner LETs, compiled afterwards, look them
;;; up instead of walking their bodies again. Without the memo the work was the
;;; size of a body times the depth of the LETs around it.
;;;
;;; An EQ form is not always in one lexical macro environment: a macro that
;;; splices its body into two MACROLETs (proc-parse's WITH-VECTOR-PARSING does,
;;; one per vector type) puts the same forms under two definitions of a local
;;; macro, each assigning a different variable. So a summary is kept per macro
;;; environment (*MACROEXPAND-SCOPE* and *SYMBOL-MACROS*), the key the macro
;;; expansion cache uses. The walk never consults *LOCALS*. A summary whose walk met a macro
;;; expansion that could not be cached (it depends on more than the lexical
;;; macro environment) is not kept, and neither is any summary containing it.
;;;
;;; A mutation target is recorded AND walked as a reference, so a closure that
;;; assigns a variable also marks it captured: dropping that would skip the
;;; box and lose the assignment.
;;;
;;; The walk takes the forms in the same order as before the memo, and gives an
;;; uninterned symbol its effective name (VAR-NAME, which carries a counter)
;;; at the same points: when it is assigned or referenced inside a closure.
;;; Those names show up in the generated code, so their order is part of the
;;; output.
(defvar *bmr-cache* nil
  "Per-top-level-form EQ memo of the mutation / capture walk. A form (a cons)
   maps to its summaries, a list of (SCOPE SYMBOL-MACROS SIGNATURE . SUMMARY) with
   SUMMARY (MUTATED REF-IN . REF-OUT) taken under that *MACROEXPAND-SCOPE* and
   *SYMBOL-MACROS* (see %FMC-CACHED-SUMMARY); a symbol maps to the stamp
   of the last de-duplication pass that saw it.")

(defvar *fmc-set-step* (list '#:fmc-set-step)
  "Heads a walk step that changes the walk's macro environment instead of
   walking a form: (MARKER KIND . VALUE).")

(defun %fmc-apply-set (kind-value)
  (let ((v (cdr kind-value)))
    (case (car kind-value)
      (:symbol-macros (setf *symbol-macros* v))
      (:mscope (setf *macroexpand-scope* v))
      (:lexops (setf *lexical-operators* v)))))

(defun %fmc-set-step (kind value)
  (cons *fmc-set-step* (cons kind value)))

(defun %fmc-dedupe (list)
  "LIST without repeated symbols (order not kept). A short list is searched;
   a longer one stamps each symbol in *BMR-CACHE*."
  (cond
    ((or (null list) (null (cdr list))) list)
    ((null (nthcdr 12 list))
     (let ((out '()))
       (dolist (s list out)
         (unless (member s out :test #'eq)
           (push s out)))))
    (t
     (let ((cache *bmr-cache*)
           (stamp (list nil))
           (out '()))
       (dolist (s list out)
         (unless (eq (gethash s cache) stamp)
           (setf (gethash s cache) stamp)
           (push s out)))))))

(defun %fmc-place-op (head)
  "How the walk treats an operator named like a place modifier, matched by
   SYMBOL-NAME across packages: :PUSH, :POP (also INCF / DECF), :ROTATE (also
   SHIFTF), :MV-SETQ, or NIL. The length and first character rule out most
   operators before any string comparison."
  (let* ((n (symbol-name head))
         (len (length n)))
    (when (and (<= 3 len 19)
               (member (char n 0) '(#\P #\I #\D #\R #\S #\M)))
      (cond
        ((or (string= n "PUSH") (string= n "PUSHNEW")) :push)
        ((or (string= n "POP") (string= n "INCF") (string= n "DECF")) :pop)
        ((or (string= n "ROTATEF") (string= n "SHIFTF")) :rotate)
        ((string= n "MULTIPLE-VALUE-SETQ") :mv-setq)))))

;; A form is kept once per macro environment it was walked in: the same (EQ)
;; form can sit in two of them when a macro splices its body into two MACROLET
;; or SYMBOL-MACROLET forms that give a local macro different expansions (one
;; per branch, each assigning its own variable), and a summary taken in one is
;; wrong in the other. A summary whose walk gave an expander an environment the
;; walk could not be sure of (*EXPANSION-SM-DEPENDENT*) also keeps the
;; resolution of the symbol macro names there, and is reused only where they
;; resolve the same way.
(defun %fmc-lambda-list-vars (lambda-list)
  "The variables LAMBDA-LIST binds: references to them in the function body are
   the body's own."
  (let ((vars '()))
    (do ((pl lambda-list (cdr pl)))
        ((not (consp pl)))
      (let ((p (car pl)))
        (cond ((and p (symbolp p))
               (unless (member p lambda-list-keywords :test #'eq) (push p vars)))
              ((consp p)
               (let ((v (car p)))
                 (cond ((and v (symbolp v)) (push v vars))
                       ((and (consp v) (consp (cdr v)) (cadr v) (symbolp (cadr v)))
                        (push (cadr v) vars))))
               (when (and (consp (cdr p)) (consp (cddr p)) (caddr p) (symbolp (caddr p)))
                 (push (caddr p) vars))))))
    vars))

(defun %fmc-cached-summary (entries)
  "The summary in ENTRIES, a list of (SCOPE SYMBOL-MACROS SIGNATURE . SUMMARY),
   taken in the current macro environment, or NIL. SIGNATURE is :ANY, or an
   %SM-SNAPSHOT the current position has to match."
  (let ((sig :unset))
    (dolist (e entries nil)
      (when (and (%macroexpand-scope= (car e) *macroexpand-scope*)
                 (let ((sm (cadr e)))
                   (or (eq sm *symbol-macros*) (equal sm *symbol-macros*)))
                 (let ((esig (caddr e)))
                   (or (eq esig :any)
                       (progn
                         (when (eq sig :unset) (setf sig (%sm-snapshot)))
                         (when (%sm-snapshot-match-p sig esig)
                           (setf *expansion-sm-dependent* t)
                           t)))))
        (return (cdddr e))))))

(defun %fmc-summary (form ctx mdepth)
  "Summary (MUTATED REF-IN . REF-OUT) of FORM. CTX is true when FORM sits
   inside a closure where it is met, and only decides when uninterned symbols
   get their names."
  (if (consp form)
      (let* ((entries (gethash form *bmr-cache*))
             (hit (%fmc-cached-summary entries)))
        (cond
          (hit
           (when ctx
             (dolist (s (cddr hit))
               (unless (symbol-package s) (var-name s))))
           hit)
          (t
           (let ((s nil) (unc nil) (dep nil))
             (let ((*expansion-uncacheable* nil)
                   (*expansion-sm-dependent* nil))
               (setf s (%fmc-walk form ctx mdepth)
                     unc *expansion-uncacheable*
                     dep *expansion-sm-dependent*))
             (when dep (setf *expansion-sm-dependent* t))
             (if unc
                 (setf *expansion-uncacheable* t)
                 (setf (gethash form *bmr-cache*)
                       (cons (list* *macroexpand-scope* *symbol-macros*
                                    (if dep (%sm-snapshot) :any) s)
                             entries)))
             s))))
      (%fmc-walk form ctx mdepth)))

(defun %fmc-symbol-macro-variable (s)
  "When S names a local symbol macro whose expansion is a variable, possibly
   through further symbol macros or THE, that variable; else NIL. SETQ, SETF,
   INCF and the like of S store into it."
  (let ((seen '()))
    (loop
      (let ((sm (and s (symbolp s)
                     (not (member s seen :test #'eq))
                     (assoc s *symbol-macros* :test #'eq))))
        (unless sm
          (return (and seen s (symbolp s) s)))
        (push s seen)
        (let ((e (cdr sm)))
          (loop while (and (consp e) (eq (car e) 'the)
                           (consp (cdr e)) (consp (cddr e)))
                do (setf e (caddr e)))
          (if (and e (symbolp e))
              (setf s e)
              (return nil)))))))

(defun %fmc-walk (expr ctx mdepth0)
  "Walk EXPR and return its summary. Worklist entries are (FORM IN-LAMBDA .
   MDEPTH), IN-LAMBDA relative to EXPR, or a set step."
  (let ((worklist (list (cons expr mdepth0)))
        (*lexical-operators* *lexical-operators*)
        (*macroexpand-scope* *macroexpand-scope*)
        (*symbol-macros* *symbol-macros*)
        (mut '())
        (rin '())
        (rout '())
        (marker *fmc-set-step*)
        (limit *macro-expand-depth-limit*))
    ;; Local macros, not local functions: this runs once per kept form, and
    ;; closures over the accumulators would box every one of them. No expansion
    ;; below calls another of these local macros: the released dotcl that
    ;; seeds the build leaves a local macro met inside another local macro's
    ;; expansion as a call to an undefined function, so EXPAND and RUN spell
    ;; out what MUTATE, WALK and REF would have written.
    (macrolet ((ref (s in-lambda)
                 `(let ((s ,s))
                    (cond (,in-lambda
                           (unless (symbol-package s) (var-name s))
                           (push s rin))
                          (t
                           (when (and ctx (not (symbol-package s))) (var-name s))
                           (push s rout)))))
               ;; Assigning a local symbol macro assigns the variable it
               ;; stands for: record that variable too.
               (mutate (s)
                 `(let ((s ,s))
                    (unless (symbol-package s) (var-name s))
                    (push s mut)
                    (when *symbol-macros*
                      (let ((v (%fmc-symbol-macro-variable s)))
                        (when v
                          (unless (symbol-package v) (var-name v))
                          (push v mut))))))
               ;; One cons per entry: MDEPTH, negated (less one) inside a closure.
               (walk (form in-lambda mdepth)
                 `(push (cons ,form (if ,in-lambda (- -1 ,mdepth) ,mdepth)) worklist))
               ;; A global symbol macro the expansion resolved is only looked
               ;; up as a place (setf-family expanders are the only macro-time
               ;; callers of LOOKUP-SYMBOL-MACRO), so if the name is in fact a
               ;; shadowing variable it is being assigned. Over-recording is
               ;; safe: it can only box a variable that did not need it.
               (expand (form expander in-lambda mdepth)
                 `(multiple-value-bind (exp hits) (%walker-macroexpand ,form ,expander :none)
                    (dolist (h hits)
                      (unless (symbol-package h) (var-name h))
                      (push h mut)
                      (push (cons h (if ,in-lambda (- -1 ,mdepth) ,mdepth)) worklist))
                    exp))
               ;; STEPS, in the order the walk takes them: (FORM . IN-LAMBDA)
               ;; or a set step. A kept form is summarised (and remembered)
               ;; now, a plain symbol is just a reference; without the stack
               ;; for that, they go on the worklist.
               (run (steps mdepth force)
                 `(let ((steps ,steps) (mdepth ,mdepth))
                    (if (or ,force (%stack-space-available-p))
                        (dolist (st steps)
                          (let ((f (car st)))
                            (cond
                              ((eq f marker) (%fmc-apply-set (cdr st)))
                              ((and (symbolp f)
                                    (not (and f (< mdepth limit)
                                              (assoc f *symbol-macros* :test #'eq))))
                               (cond ((and (consp (cdr st)) (member f (cddr st) :test #'eq)))
                                     ((if (consp (cdr st)) (cadr st) (cdr st))
                                      (unless (symbol-package f) (var-name f))
                                      (push f rin))
                                     (t
                                      (when (and ctx (not (symbol-package f))) (var-name f))
                                      (push f rout))))
                              ((and (atom f) (not (symbolp f))) nil)
                              (t
                               (let* ((own (and (consp (cdr st)) (cddr st)))
                                      (in-lambda (if (consp (cdr st)) (cadr st) (cdr st)))
                                      (s (%fmc-summary f (or ctx in-lambda) mdepth)))
                                 (dolist (x (car s))
                                   (unless (and own (member x own :test #'eq)) (push x mut)))
                                 (dolist (x (cadr s))
                                   (unless (and own (member x own :test #'eq)) (push x rin)))
                                 (if in-lambda
                                     (dolist (x (cddr s))
                                       (unless (and own (member x own :test #'eq)) (push x rin)))
                                     (dolist (x (cddr s))
                                       (unless (and own (member x own :test #'eq)) (push x rout)))))))))
                        (dolist (st (reverse steps))
                          (if (eq (car st) marker)
                              (push st worklist)
                              (push (cons (car st) (if (if (consp (cdr st)) (cadr st) (cdr st))
                                                       (- -1 mdepth) mdepth))
                                    worklist)))))))
      (loop while worklist do
        (let ((item (pop worklist)))
          (if (eq (car item) marker)
              (%fmc-apply-set (cdr item))
              (let* ((e (car item))
                     (code (cdr item))
                     (in-lambda (< code 0))
                     (mdepth (if in-lambda (- -1 code) code)))
                (cond
                  ;; A symbol macro is both a reference and an expansion to
                  ;; walk: a mutation can hide in the expansion.
                  ((symbolp e)
                   (ref e in-lambda)
                   (when (and e (< mdepth limit))
                     (let ((sm (assoc e *symbol-macros* :test #'eq)))
                       (when sm
                         (walk (cdr sm) in-lambda (1+ mdepth))))))
                  ((consp e)
                   (let* ((head (car e))
                          (place-op (and (symbolp head) (%fmc-place-op head))))
                     (cond
                       ;; The portable MULTIPLE-VALUE-BIND shape compiles as a
                       ;; LET* (%MV-CALL-LAMBDA-BIND), not as a closure: walk
                       ;; it that way.
                       ((let ((bind (and (eq head 'multiple-value-call)
                                         (%mv-call-lambda-bind e))))
                          (when bind
                            (walk bind in-lambda mdepth)
                            t)))
                       ;; --- assignments: record the target and walk it ---
                       ((and (symbolp head) (or (eq head 'setq) (eq head 'setf)
                                                (eq head 'psetq) (eq head 'psetf)))
                        (loop for (var val) on (cdr e) by #'cddr
                              do (cond
                                   ((and var (symbolp var))
                                    (mutate var)
                                    (walk var in-lambda mdepth))
                                   ((and (consp var) (eq (car var) 'the) (symbolp (caddr var)))
                                    (mutate (caddr var))
                                    (walk var in-lambda mdepth))
                                   ((consp var)
                                    (let ((expander nil))
                                      (when (and (or (eq head 'setf) (eq head 'psetf))
                                                 (< mdepth limit)
                                                 (%stack-space-available-p)
                                                 (setf expander (find-macro-expander head)))
                                        (let ((expanded (expand `(,head ,var ,val)
                                                                expander
                                                                in-lambda mdepth)))
                                          (when expanded
                                            (walk expanded in-lambda (1+ mdepth))))))
                                    (walk var in-lambda mdepth)))
                                 (when val (walk val in-lambda mdepth))))
                       ((eq place-op :mv-setq)
                        (let ((vars (cadr e)))
                          (when (listp vars)
                            (dolist (v vars)
                              (when (symbolp v)
                                (mutate v)
                                (walk v in-lambda mdepth)))))
                        (when (caddr e)
                          (walk (caddr e) in-lambda mdepth)))
                       ((eq place-op :push)
                        (let* ((place (caddr e))
                               (sym (if (symbolp place) place
                                        (and (consp place) (eq (car place) 'the) (caddr place)))))
                          (when (and sym (symbolp sym) (not (eq sym t)))
                            (mutate sym)))
                        (do-list-safe (sub (cdr e))
                          (walk sub in-lambda mdepth)))
                       ((eq place-op :pop)
                        (let* ((place (cadr e))
                               (sym (if (symbolp place) place
                                        (and (consp place) (eq (car place) 'the) (caddr place)))))
                          (when (and sym (symbolp sym) (not (eq sym t)))
                            (mutate sym)))
                        (do-list-safe (sub (cdr e))
                          (walk sub in-lambda mdepth)))
                       ((eq place-op :rotate)
                        (dolist (arg (cdr e))
                          (when (and (symbolp arg) arg (not (eq arg t)))
                            (mutate arg)
                            (walk arg in-lambda mdepth))
                          (when (consp arg)
                            (walk arg in-lambda mdepth))))
                       ;; --- structure ---
                       ((and (symbolp head) (eq head 'quote)) nil)
                       ((and (symbolp head) (eq head 'defun))
                        (run (mapcar (lambda (f) (cons f t)) (reverse (cdddr e))) mdepth nil))
                       ;; A nested lambda: all of it is inside a closure.
                       ;; Only the initializer forms of the lambda list, as for
                       ;; FLET: walking the list itself as a form expands
                       ;; (PARAM ...) when a parameter is named after a macro,
                       ;; firing that macro's compile-time side effects.
                       ((and (symbolp head) (eq head 'lambda) (listp (cadr e)))
                        (let ((steps '()))
                          (do ((pl (cadr e) (cdr pl)))
                              ((not (consp pl)))
                            (let ((p (car pl)))
                              (when (and (consp p) (cadr p))
                                (push (cons (cadr p) t) steps))))
                          (let ((own (%fmc-lambda-list-vars (cadr e))))
                            (dolist (form (cddr e))
                              (push (list* form t own) steps)))
                          (run (nreverse steps) 0 t)))
                       ((and (or (eq head 'let) (eq head 'let*)) (listp (cadr e)))
                        (let ((bindings (cadr e))
                              (steps '())
                              (shadowed nil))
                          (when *symbol-macros*
                            (do-list-safe (b bindings)
                              (let ((name (if (consp b) (car b) b)))
                                (when (and name (symbolp name)
                                           (assoc name *symbol-macros* :test #'eq))
                                  (push name shadowed)))))
                          ;; Built back to front: the init forms come first,
                          ;; then the body with the bound names unshadowed.
                          (when shadowed
                            (push (%fmc-set-step :symbol-macros *symbol-macros*) steps))
                          (let ((own '()))
                            (do-list-safe (b bindings)
                              (let ((name (if (consp b) (car b) b)))
                                (when (and name (symbolp name)) (push name own))))
                            (dolist (form (cddr e))
                              (push (list* form in-lambda own) steps)))
                          (when shadowed
                            (push (%fmc-set-step
                                   :symbol-macros
                                   (remove-if (lambda (sm) (member (car sm) shadowed :test #'eq))
                                              *symbol-macros*))
                                  steps))
                          (dolist (b bindings)
                            (when (and (consp b) (cadr b))
                              (push (cons (cadr b) in-lambda) steps)))
                          (run steps mdepth nil)))
                       ((and (symbolp head) (or (eq head 'flet) (eq head 'labels)) (listp (cadr e)))
                        (let ((shadows (%flet-macro-shadows (cadr e)))
                              (steps '())
                              (enter nil))
                          ;; Names that hide a macro are in scope for the body,
                          ;; and for LABELS also for the definitions: as in the
                          ;; free-variable walk.
                          (flet ((enter ()
                                   (push (%fmc-set-step :mscope *macroexpand-scope*) steps)
                                   (push (%fmc-set-step :lexops *lexical-operators*) steps)
                                   (setf enter t)))
                            (when (and shadows (eq head 'labels)) (enter))
                            (dolist (fdef (cadr e))
                              ;; Only the initializer forms of the lambda list,
                              ;; not the list itself: a parameter whose name is
                              ;; a macro must not be expanded. A CONSP loop: the
                              ;; position is not guaranteed to be a proper list.
                              (do ((pl (cadr fdef) (cdr pl)))
                                  ((not (consp pl)))
                                (let ((p (car pl)))
                                  (when (and (consp p) (cadr p))
                                    (push (cons (cadr p) t) steps))))
                              (let ((own (%fmc-lambda-list-vars (cadr fdef))))
                                (dolist (form (cddr fdef))
                                  (push (list* form t own) steps))))
                            (when (and shadows (eq head 'flet)) (enter))
                            (dolist (form (cddr e))
                              (push (cons form in-lambda) steps)))
                          (when enter
                            (setf *macroexpand-scope* (cons (cadr e) *macroexpand-scope*)
                                  *lexical-operators* (%flet-lexical-operators shadows)))
                          (run steps mdepth nil)))
                       ((and (symbolp head) (eq head 'handler-case))
                        (let ((steps '()))
                          (when (cadr e)
                            (push (cons (cadr e) in-lambda) steps))
                          (dolist (clause (cddr e))
                            (dolist (form (cddr clause))
                              (push (cons form t) steps)))
                          (run steps mdepth nil)))
                       ((and (symbolp head) (eq head 'handler-bind))
                        (let ((steps '()))
                          (dolist (binding (cadr e))
                            (when (cadr binding)
                              (push (cons (cadr binding) t) steps)))
                          (dolist (form (cddr e))
                            (push (cons form in-lambda) steps))
                          (run steps mdepth nil)))
                       ((and (symbolp head) (eq head 'restart-case))
                        (let ((steps '()))
                          (when (cadr e)
                            (push (cons (cadr e) in-lambda) steps))
                          (dolist (clause (cddr e))
                            (dolist (form (cddr clause))
                              (push (cons form t) steps)))
                          (run steps mdepth nil)))
                       ((and (symbolp head) (eq head 'macrolet))
                        (let ((macro-defs (cadr e))
                              (steps '()))
                          (push (%fmc-set-step :mscope *macroexpand-scope*) steps)
                          (push (%fmc-set-step :lexops *lexical-operators*) steps)
                          (setf *macroexpand-scope* (cons macro-defs *macroexpand-scope*))
                          (setf *lexical-operators* (%walk-macrolet-lexical-operators macro-defs))
                          (dolist (form (cddr e))
                            (push (cons form in-lambda) steps))
                          (run steps mdepth nil)))
                       ((and (symbolp head) (eq head 'symbol-macrolet))
                        (let ((steps (list (%fmc-set-step :mscope *macroexpand-scope*)
                                           (%fmc-set-step :symbol-macros *symbol-macros*))))
                          (setf *macroexpand-scope* (cons (cadr e) *macroexpand-scope*))
                          (setf *symbol-macros*
                                (append (mapcar (lambda (b) (cons (car b) (cadr b))) (cadr e))
                                        *symbol-macros*))
                          (dolist (form (cddr e))
                            (push (cons form in-lambda) steps))
                          (run steps mdepth nil)))
                       (t
                        (let ((expanded nil)
                              (expander nil))
                          (when (and (symbolp head) head
                                     (< mdepth limit)
                                     (%stack-space-available-p)
                                     (not (%lexical-function-p head))
                                     (setf expander (find-macro-expander head)))
                            (setf expanded (expand e expander in-lambda mdepth)))
                          (if expanded
                              (walk expanded in-lambda (1+ mdepth))
                              (do-list-safe (sub e)
                                (walk sub in-lambda mdepth)))))))))))))
      (cons (%fmc-dedupe mut) (cons (%fmc-dedupe rin) (%fmc-dedupe rout))))))

(defun find-mutated-and-captured-vars (body var-names)
  "Returns (values MUTATED CAPTURED): the names in VAR-NAMES (variable-name
   strings) that BODY assigns, and those it references from inside a closure."
  (let ((*bmr-cache* (or *bmr-cache* (make-hash-table :test #'eq)))
        (mutated '())
        (captured '()))
    (dolist (form body)
      (let ((s (%fmc-summary form nil 0)))
        (dolist (sym (car s))
          (let ((n (var-name sym)))
            (when (and (member n var-names :test #'string=)
                       (not (member n mutated :test #'string=)))
              (push n mutated))))
        (dolist (sym (cadr s))
          (let ((n (var-name sym)))
            (when (and (member n var-names :test #'string=)
                       (not (member n captured :test #'string=)))
              (push n captured))))))
    (values mutated captured)))

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
   Preserves CIL stack semantics and evaluation order.

   Every LET runs this over its whole instruction list, its nested LETs'
   instructions included, so the work is the size of the body times its nesting
   depth. The pairs are found first, which takes no table; only the few keys that
   form one are then counted and checked."
  (let ((cands '())
        (prev nil))
    ;; A (:stloc KEY) whose next instruction other than a :declare-local is
    ;; (:ldloc KEY). Anything that is not a list (none is emitted today) is
    ;; skipped without breaking the pair, as before.
    (dolist (instr instrs)
      (when (consp instr)
        (let ((op (car instr)))
          (cond
            ((eq op :declare-local))
            ((eq op :stloc) (setf prev (cadr instr)))
            ((eq op :ldloc)
             (when (and prev (equal prev (cadr instr)))
               (push (list prev 0 0 nil) cands))
             (setf prev nil))
            (t (setf prev nil))))))
    (when (null cands)
      (return-from eliminate-single-ref-locals instrs))
    ;; Each candidate: (KEY STLOC-COUNT LDLOC-COUNT DECLARED-TYPE). Counted via
    ;; the central enumerator so locals embedded in nested operands (e.g.
    ;; :dotnet-call-direct-locals) raise the read count and correctly disqualify
    ;; a key.
    (let ((table (when (> (length cands) 8)
                   (let ((h (make-hash-table :test #'equal)))
                     (dolist (c cands h) (setf (gethash (car c) h) c))))))
      (flet ((cand (k) (if table
                           (gethash k table)
                           (assoc k cands :test #'equal))))
        (dolist (instr instrs)
          (when (consp instr)
            (when (eq (car instr) :declare-local)
              (let ((c (cand (cadr instr))))
                (when c (setf (fourth c) (caddr instr)))))
            (do-instr-local-writes (k instr)
              (let ((c (cand k))) (when c (incf (second c)))))
            (do-instr-local-reads (k instr)
              (let ((c (cand k))) (when c (incf (third c)))))))))
    (let ((removable (loop for c in cands
                           when (and (= (second c) 1)
                                     (= (third c) 1)
                                     (string= (or (fourth c) "") "LispObject"))
                             collect (car c))))
      (if (null removable)
          instrs
          (remove-if (lambda (instr)
                       (and (consp instr)
                            (member (car instr) '(:declare-local :stloc :ldloc))
                            (member (cadr instr) removable :test #'equal)))
                     instrs)))))

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
  (unless (dolist (i instrs nil)
            (when (and (consp i) (eq (car i) :call)
                       (equal (cadr i) "Runtime.CollectRestArgs"))
              (return t)))
    (return-from elide-unused-rest-list instrs))
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
    ("Runtime.Values2"      . "Runtime.Values2Primary")
    ("Runtime.FindSymbolMv" . "Runtime.FindSymbolPrimary")
    ("Runtime.InternMv"     . "Runtime.InternPrimary")
    ("Runtime.ReadLineMv"   . "Runtime.ReadLinePrimary")
    ("Runtime.SubtypepMv"   . "Runtime.SubtypepPrimary"))
  "Runtime entries that return two values, paired with an entry that returns only
   the primary. The peephole (P12) swaps in the second when the call is followed
   by Runtime.UnwrapMv, i.e. when the call site is in single-value position.

   A pair belongs here only when `TWIN(x)` is observably identical to
   `UnwrapMv(ORIGINAL(x))` -- same computation, same thread value state (one
   value, the primary), same conditions signalled in the same order. The twins
   are written as two entries over one core in the runtime so that stays true.

   Not a DEFCONSTANT: a list literal under DEFCONSTANT is re-evaluated to a
   fresh, non-EQL list when the file is reloaded, which SBCL rejects.")

(defparameter +mv-pair-twins+
  '(("Runtime.Gethash"      . "Runtime.GethashPair")
    ("Runtime.FloorOp"      . "Runtime.FloorOpPair")
    ("Runtime.TruncateOp"   . "Runtime.TruncateOpPair")
    ("Runtime.CeilingOp"    . "Runtime.CeilingOpPair")
    ("Runtime.RoundOp"      . "Runtime.RoundOpPair")
    ("Runtime.Values2"      . "Runtime.Values2Pair")
    ("Runtime.FindSymbolMv" . "Runtime.FindSymbolL")
    ("Runtime.InternMv"     . "Runtime.InternSymbolV")
    ("Runtime.ReadLineMv"   . "Runtime.ReadLine")
    ("Runtime.SubtypepMv"   . "Runtime.SubtypepPair"))
  "The same entries as +MV-PRIMARY-TWINS+, paired with an entry that publishes
   both values in the thread state and returns the primary without an MvReturn.
   P13 swaps it in when the next instruction reads the values out of the thread
   state (CaptureForBind, NthValueOf), together with a reader that knows the
   state is that pair.")

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
     P7c (:ldc-i8 K) (:call \"Fixnum.Make\") -> (:load-const K)  ; K outside
                                          ; the small-integer cache
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
     P13 (:call F) (:call \"MultipleValues.CaptureForBind\")
           -> (:call F-pair) (:call \"MultipleValues.CapturePair\")
     P13 (:call F) (:ldc-i4 N) (:call \"Runtime.NthValueOf\")
           -> (:call F-pair) (:ldc-i4 N) (:call \"MultipleValues.PairNth\")
                                          ; the values are read out of the
                                          ; thread state at once, so the MvReturn
                                          ; is not needed (+mv-pair-twins+).
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
              ;; P5 for a SIMD pack: the box is pure and never an MvReturn.
              ((and (consp i1) (eq (car i1) :call) (stringp (cadr i1))
                    (> (length (cadr i1)) 9) (string= "Simd.Box." (cadr i1) :end2 9)
                    (consp i2) (or (eq (car i2) :pop)
                                   (and (eq (car i2) :call)
                                        (equal (cadr i2) "Runtime.UnwrapMv"))))
               (setf changed t)
               (if (eq (car i2) :pop)
                   (push i2 out)
                   (push i1 out))
               (setf cur (cddr cur)))
              ;; P5 for an (unsigned-byte 64): BoxU64 is pure too (a Fixnum or a fresh
              ;; Bignum from the bits).
              ((and (consp i1) (eq (car i1) :call) (equal (cadr i1) "Runtime.BoxU64")
                    (consp i2) (or (eq (car i2) :pop)
                                   (and (eq (car i2) :call)
                                        (equal (cadr i2) "Runtime.UnwrapMv"))))
               (setf changed t)
               (if (eq (car i2) :pop)
                   (push i2 out)
                   (push i1 out))
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
              ;; P14: an int64 masked to its low 8 / 16 / 32 bits by zero
              ;; extension: 2 bytes of IL where ldc.i8 and AND take 10.
              ((and (consp i1) (eq (car i1) :ldc-i8)
                    (member (cadr i1) '(255 65535 4294967295))
                    (consp i2) (eq (car i2) :and) (null (cdr i2)))
               (setf changed t)
               (push (list (case (cadr i1) (255 :conv-u1) (65535 :conv-u2) (t :conv-u4))) out)
               (push (list :conv-u8) out)
               (setf cur (cddr cur)))
              ;; P7c: box a literal outside Fixnum.Make's cache: a load of the
              ;; constant instead of an allocation on every execution. Not when
              ;; the next instruction lets P5 / P7 / P10 drop the box.
              ((and (consp i1) (eq (car i1) :ldc-i8) (integerp (cadr i1))
                    (not (<= -128 (cadr i1) 65535))
                    (consp i2) (eq (car i2) :call) (equal (cadr i2) "Fixnum.Make")
                    (not (and (consp i3)
                              (or (member (car i3) '(:pop :unbox-fixnum))
                                  (and (eq (car i3) :call)
                                       (equal (cadr i3) "Runtime.UnwrapMv"))))))
               (setf changed t)
               (push (list :load-const (cadr i1)) out)
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
              ;; P13: the same entries where the next instruction reads the
              ;; values out of the thread state.
              ((and (consp i1) (eq (car i1) :call)
                    (assoc (cadr i1) +mv-pair-twins+ :test #'equal)
                    (or (and (consp i2) (eq (car i2) :call)
                             (equal (cadr i2) "MultipleValues.CaptureForBind"))
                        (and (consp i2) (eq (car i2) :ldc-i4)
                             (consp i3) (eq (car i3) :call)
                             (equal (cadr i3) "Runtime.NthValueOf"))))
               (setf changed t)
               (push (list :call (cdr (assoc (cadr i1) +mv-pair-twins+ :test #'equal)))
                     out)
               (cond ((eq (car i2) :call)
                      (push '(:call "MultipleValues.CapturePair") out)
                      (setf cur (cddr cur)))
                     (t
                      (push i2 out)
                      (push '(:call "MultipleValues.PairNth") out)
                      (setf cur (cdddr cur)))))
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
  (%thread-binding-stack
   (peephole-optimize
    (%hoist-loop-mv-resets
     (if *emit-source-lines*
         (elide-unused-rest-list instrs)
         (%merge-disjoint-locals (elide-unused-rest-list instrs)))))))

(defparameter +binding-stack-calls+
  '(("DynamicBindings.Get" . "DynamicBindings.GetOn")
    ("DynamicBindings.SetChecked" . "DynamicBindings.SetCheckedOn")
    ("DynamicBindings.Push" . "DynamicBindings.PushOn")
    ("DynamicBindings.Pop" . "DynamicBindings.PopOn")
    ("DynamicBindings.BindDepth" . "DynamicBindings.BindDepthOn")
    ("DynamicBindings.UnbindTo" . "DynamicBindings.UnbindToOn"))
  "Binding stack operations, each paired with the entry that takes the thread's
   stack as its last argument.")

(defvar *bound-special-loads* (make-hash-table :test #'equal :synchronized t)
  "The symbol loads (SIL forms) of the special variables that a body compiled
   so far binds.")

(defun %sym-load-p (i)
  (and (consp i) (member (car i) '(:load-sym :load-sym-pkg :load-const))))

(defun %note-bound-specials (instrs)
  "Record the variables bound by the body: each bind is the symbol load, a
   castclass, the value from a local and the push."
  (let (p1 p2 p3)
    (dolist (i instrs)
      (when (and (consp i) (eq (car i) :call) (equal (cadr i) "DynamicBindings.Push")
                 (consp p1) (eq (car p1) :ldloc)
                 (equal p2 '(:castclass "Symbol"))
                 (%sym-load-p p3))
        (setf (gethash p3 *bound-special-loads*) t))
      (setq p3 p2 p2 p1 p1 i))))

(defun %lazy-bound-special-access (instrs)
  "Read or SETQ of a variable some compiled body binds: through the stack in a
   new local, fetched at the first such access. Returns the new instructions
   and the local, or NIL when there is no such access."
  (let (p1 p2 p3 out st)
    (dolist (i instrs)
      (let ((lazy (and (consp i) (eq (car i) :call)
                       (cond ((and (equal (cadr i) "DynamicBindings.Get")
                                   (equal p1 '(:castclass "Symbol"))
                                   (gethash p2 *bound-special-loads*))
                              "DynamicBindings.GetLazy")
                             ((and (equal (cadr i) "DynamicBindings.SetChecked")
                                   (consp p1) (eq (car p1) :ldloc)
                                   (equal p2 '(:castclass "Symbol"))
                                   (gethash p3 *bound-special-loads*))
                              "DynamicBindings.SetCheckedLazy")))))
        (cond (lazy
               (unless st (setq st (gen-local "DSTACK")))
               (push `(:ldloca ,st) out)
               (push `(:call ,lazy) out))
              (t (push i out))))
      (setq p3 p2 p2 p1 p1 i))
    (if st (values (nreverse out) st) (values instrs nil))))

(defun %binding-stack-body-start (body)
  "Position after the parameter stores and the value-mode read, which must stay
   the body's first call (CilAssembler.BodyReadsMvMode)."
  (let ((k 0))
    (loop
      (let ((i (nth k body)) (next (nth (1+ k) body)))
        (cond ((and (consp i) (member (car i) '(:declare-local :line)))
               (incf k))
              ((and (consp i) (consp next) (eq (car next) :stloc)
                    (or (eq (car i) :ldarg)
                        (and (eq (car i) :call)
                             (equal (cadr i) "MultipleValues.TakeMode"))))
               (incf k 2))
              (t (return k)))))))

(defun %thread-binding-stack (instrs)
  "When a function body binds a special variable, fetch the thread's binding
   stack once at its start and hand it to every binding operation in the body,
   which otherwise reads the thread-static at each one. A body that binds
   nothing reads and SETQs the variables some compiled body binds through a
   stack fetched at the first such access; other variables keep the global
   value's inline path. Only the body's own instructions: a closure body
   nested in it is another method."
  (if (find-if (lambda (i)
                 (and (consp i) (eq (car i) :call)
                      (equal (cadr i) "DynamicBindings.Push")))
               instrs)
      (let* ((st (gen-local "DSTACK"))
             (body (loop for i in instrs
                         for twin = (and (consp i) (eq (car i) :call) (stringp (cadr i))
                                         (cdr (assoc (cadr i) +binding-stack-calls+
                                                     :test #'string=)))
                         if twin append `((:ldloc ,st) (:call ,twin))
                         else collect i))
             (n (%binding-stack-body-start body)))
        (%note-bound-specials instrs)
        `(,@(subseq body 0 n)
          (:declare-local ,st "BindingStack")
          (:call "DynamicBindings.Here")
          (:stloc ,st)
          ,@(nthcdr n body)))
      (multiple-value-bind (body st) (%lazy-bound-special-access instrs)
        (if (not st)
            instrs
            (let ((n (%binding-stack-body-start body)))
              `(,@(subseq body 0 n)
                (:declare-local ,st "BindingStack")
                ,@(nthcdr n body)))))))

(defparameter +sil-branch-ops+
  '(:br :brtrue :brfalse :leave :beq :bne :blt :bgt :ble :bge
    :bne-un :bge-un :bgt-un :ble-un :blt-un))

(defparameter +mv-quiet-ops+
  '(:label :ldloc :stloc :ldarg :starg :ldc-i4 :ldc-i8 :ldc-r8 :ldsfld
    :load-const :load-const-cached :load-sym :load-sym-keyword :load-sym-pkg
    :ldnull :pop :dup :ceq :clt :cgt :add :sub :and :or :xor :declare-local
    :unbox-fixnum :castclass :isinst :line :nop
    :mul :div :rem :div-un :rem-un :neg :not :shl :shr :shr-un :cgt-un :clt-un
    :ldc-r4 :conv-i :conv-i4 :conv-i8 :conv-u :conv-u8 :conv-r8 :conv-r4 :conv-r-un
    :br :brtrue :brfalse :leave :beq :bne :blt :bgt :ble :bge
    :bne-un :bge-un :bgt-un :ble-un :blt-un))

(defparameter +mv-quiet-calls+
  '("ConditionSystem.PollInterrupt" "MultipleValues.Reset" "MultipleValues.ResetOnExit"
    "Runtime.IsTruthy"
    "Runtime.Car" "Runtime.Cdr" "Runtime.Primary" "Runtime.UnwrapMv"
    "Fixnum.Make" "Runtime.AddFixnumChecked" "Runtime.IsTrueEq" "Runtime.IsTrueEql"
    "Runtime.IsTrueConsp" "Runtime.IsTrueAtom" "Runtime.IsTrueListp"
    "Runtime.IsTrueSymbolp" "Runtime.Endp" "Runtime.IsTrueEndp" "Runtime.IsNotNil"
    "Runtime.IsNilObj" "Runtime.IsConsObject" "Runtime.IsAtomObject" "Runtime.IsListObject"))

(defun %mv-quiet-instr-p (x)
  (and (consp x)
       (or (member (car x) +mv-quiet-ops+)
           (and (eq (car x) :newobj)
                (member (cadr x) '("DoubleFloat" "SingleFloat") :test #'equal))
           (and (eq (car x) :call)
                (or (member (cadr x) +mv-quiet-calls+ :test #'equal)
                    (let ((name (cadr x)))
                      (and (stringp name) (> (length name) 5)
                           (string= name "Math." :end1 5))))))))

(defun %hoist-loop-mv-resets (instrs)
  "A loop whose instructions can neither publish values nor run user code
   leaves the values channel as it found it, so the resets inside it (one per
   test of a variable) are replaced by one in front of its head. The loop is a
   backward branch's target to the branch; nothing outside may jump into it."
  (unless (member '(:call "MultipleValues.Reset") instrs :test #'equal)
    (return-from %hoist-loop-mv-resets instrs))
  (let* ((v (coerce instrs 'simple-vector))
         (n (length v))
         (label-pos (make-hash-table :test #'equal))
         (ends (make-hash-table))
         (good '()))
    (dotimes (i n)
      (let ((x (svref v i)))
        (when (and (consp x) (eq (car x) :label))
          (setf (gethash (cadr x) label-pos) i))))
    (dotimes (i n)
      (let ((x (svref v i)))
        (when (and (consp x) (member (car x) +sil-branch-ops+))
          (let ((tp (gethash (cadr x) label-pos)))
            (when (and tp (<= tp i) (> i (gethash tp ends -1)))
              (setf (gethash tp ends) i))))))
    (maphash
     (lambda (s e)
       (when (and (loop for i from s to e always (%mv-quiet-instr-p (svref v i)))
                  (loop for i from s to e
                        thereis (equal (svref v i) '(:call "MultipleValues.Reset")))
                  (loop for i below n
                        never (and (or (< i s) (> i e))
                                   (let ((x (svref v i)))
                                     (and (consp x)
                                          (flet ((inside (l)
                                                   (let ((p (gethash l label-pos)))
                                                     (and p (<= s p e)))))
                                            (cond ((member (car x) +sil-branch-ops+)
                                                   (inside (cadr x)))
                                                  ((eq (car x) :switch)
                                                   (some #'inside (cadr x))))))))))
         (push (cons s e) good)))
     ends)
    (when (null good)
      (return-from %hoist-loop-mv-resets instrs))
    (setq good
          (remove-if (lambda (r)
                       (some (lambda (o)
                               (and (not (eq o r))
                                    (<= (car o) (cdr r)) (<= (car r) (cdr o))
                                    (not (and (<= (car r) (car o)) (<= (cdr o) (cdr r))))))
                             good))
                     good))
    (let ((out '()))
      (dotimes (i n (nreverse out))
        (let ((x (svref v i)))
          (cond ((assoc i good) (push '(:call "MultipleValues.Reset") out) (push x out))
                ((and (equal x '(:call "MultipleValues.Reset"))
                      (some (lambda (r) (<= (car r) i (cdr r))) good)))
                (t (push x out))))))))

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
;;; Unused lexical variables
;;; ============================================================
;;; SBCL's style warnings for a lexical variable that is never read, or is
;;; declared IGNORE and used. One walk per top-level form, before it is
;;; compiled. Uninterned variables are left alone: macros make them.

(defvar *uv-warned* nil
  "EQ table: binding form -> the variables of it already reported.")

(defvar *uv-warnings* nil)

(defvar *uv-sm-depth* 0)

(defvar *uv-depth* 0)

(defvar *uv-level* 0)

(defun %uv-decls (body doc-p)
  "(values SPECIALS IGNORES IGNORABLES REST) for the declarations heading BODY."
  (let ((specials '()) (ignores '()) (ignorables '()))
    (loop
      (cond ((and doc-p (consp body) (stringp (car body)) (consp (cdr body)))
             (setf body (cdr body) doc-p nil))
            ((and (consp body) (consp (car body)) (eq (caar body) 'declare))
             (dolist (spec (cdar body))
               (when (and (consp spec) (listp (cdr spec)))
                 (case (car spec)
                   (special (setf specials (append (cdr spec) specials)))
                   (ignore (setf ignores (append (cdr spec) ignores)))
                   (ignorable (setf ignorables (append (cdr spec) ignorables))))))
             (setf body (cdr body)))
            (t (return))))
    (values specials ignores ignorables body)))

(defun %uv-entry (sym site specials)
  (if (member sym specials :test #'eq)
      (list* sym :special nil)
      (list* sym :var (vector nil nil site *uv-depth*))))

(defun %uv-free-specials (specials entries env)
  (dolist (s specials env)
    (when (and (symbolp s) (not (assoc s entries :test #'eq)))
      (push (list* s :special nil) env))))

(defun %uv-report (entries ignores ignorables)
  (dolist (e entries)
    (when (eq (cadr e) :var)
      (let* ((sym (car e))
             (cell (cddr e))
             (read (svref cell 0))
             (set (svref cell 1))
             (site (svref cell 2)))
        (when (and (symbol-package sym)
                   (not (member sym (gethash site *uv-warned*) :test #'eq))
                   (not (global-special-p sym))
                   (not (constantp sym)))
          (let ((msgs
                  (cond ((member sym ignores :test #'eq)
                         (append (and read '("reading an ignored variable: ~S"))
                                 (and set '("~S is being set even though it was declared to be ignored."))))
                        ((member sym ignorables :test #'eq) nil)
                        (read nil)
                        (set '("The variable ~S is assigned but never read."))
                        (t '("The variable ~S is defined but never used.")))))
            (when msgs
              (push sym (gethash site *uv-warned*))
              (dolist (m msgs)
                (push (cons m sym) *uv-warnings*)))))))))

(defun %uv-note (sym env slot)
  "SYM is read (SLOT 0) or assigned (SLOT 1) where ENV is visible."
  (let ((e (assoc sym env :test #'eq)))
    (when e
      (case (cadr e)
        ;; An assignment from a closure counts as a use, as in SBCL.
        (:var (setf (svref (cddr e) (if (> *uv-depth* (svref (cddr e) 3)) 0 slot)) t))
        (:sm (when (< *uv-sm-depth* 20)
               (let ((*uv-sm-depth* (1+ *uv-sm-depth*)))
                 (%uv-walk (cddr e) env))))))))

(defun %uv-walk-list (forms env)
  (loop while (consp forms)
        do (%uv-walk (pop forms) env)))

(defun %uv-body (body env entries doc-p)
  "Walk BODY, whose leading declarations apply to ENTRIES (just bound, in
   binding order, already on ENV), and report them."
  (multiple-value-bind (specials ignores ignorables rest) (%uv-decls body doc-p)
    (dolist (e entries)
      (when (and (eq (cadr e) :var) (member (car e) specials :test #'eq))
        (setf (cadr e) :special)))
    (%uv-walk-list rest (%uv-free-specials specials entries env))
    (%uv-report entries ignores ignorables)))

(defun %uv-let (form env sequential)
  (let* ((bindings (cadr form))
         (specials (%uv-decls (cddr form) nil))
         (inner env)
         (entries '()))
    (dolist (b (if (listp bindings) bindings '()))
      (let ((name (if (consp b) (car b) b)))
        (when (consp b) (%uv-walk (cadr b) (if sequential inner env)))
        (when (and name (symbolp name))
          (let ((e (%uv-entry name form specials)))
            (push e entries)
            (when sequential (push e inner))))))
    (unless sequential
      (setf inner (append entries env)))
    (let ((shadowed (and *symbol-macros*
                         (remove-if-not (lambda (e) (assoc (car e) *symbol-macros* :test #'eq))
                                        entries))))
      (let ((*symbol-macros* (if shadowed
                                 (remove-if (lambda (sm) (assoc (car sm) shadowed :test #'eq))
                                            *symbol-macros*)
                                 *symbol-macros*)))
        (%uv-body (cddr form) inner (reverse entries) nil)))))

(defun %uv-lambda (ll body env site macro-p)
  "Walk a function with lambda list LL (a macro lambda list when MACRO-P) and
   BODY, defined where ENV is visible."
  (let ((specials (%uv-decls body t))
        (inner env)
        (entries '())
        (*uv-depth* (1+ *uv-depth*)))
    (labels ((bind (sym)
               (when (and sym (symbolp sym) (not (keywordp sym))
                          (not (member sym lambda-list-keywords :test #'eq)))
                 (let ((e (%uv-entry sym site specials)))
                   (push e entries)
                   (push e inner))))
             (var (p)
               (if (and macro-p (consp p)) (walk-ll p) (bind p)))
             (walk-ll (l)
               (let ((state :req) (resume :req))
                 (loop
                   (cond
                     ((null l) (return))
                     ((atom l) (bind l) (return))
                     (t
                      (let ((p (pop l)))
                        (cond
                          ((eq p '&optional) (setf state :opt))
                          ((or (eq p '&rest) (eq p '&body)) (setf state :rest))
                          ((eq p '&key) (setf state :key))
                          ((eq p '&aux) (setf state :aux))
                          ((eq p '&allow-other-keys))
                          ((or (eq p '&whole) (eq p '&environment))
                           (unless (eq state :one) (setf resume state))
                           (setf state :one))
                          ((eq state :one) (var p) (setf state resume))
                          ((or (eq state :req) (eq state :rest)) (var p))
                          ((atom p) (bind p))
                          ((eq state :opt)
                           (when (consp (cdr p)) (%uv-walk (cadr p) inner))
                           (var (car p))
                           (when (and (consp (cdr p)) (consp (cddr p))) (bind (caddr p))))
                          ((eq state :key)
                           (when (consp (cdr p)) (%uv-walk (cadr p) inner))
                           (if (consp (car p)) (var (cadr (car p))) (bind (car p)))
                           (when (and (consp (cdr p)) (consp (cddr p))) (bind (caddr p))))
                          ((eq state :aux)
                           (when (consp (cdr p)) (%uv-walk (cadr p) inner))
                           (bind (car p)))))))))))
      (walk-ll ll))
    (%uv-body body inner (reverse entries) t)))

(defun %uv-flet (form env labels-p)
  (let* ((defs (if (listp (cadr form)) (cadr form) '()))
         (shadows (%flet-macro-shadows defs))
         (*macroexpand-scope* *macroexpand-scope*)
         (*lexical-operators* *lexical-operators*))
    (flet ((enter ()
             (when shadows
               (setf *macroexpand-scope* (cons defs *macroexpand-scope*)
                     *lexical-operators* (%flet-lexical-operators shadows)))))
      (when labels-p (enter))
      (dolist (d defs)
        (when (and (consp d) (consp (cdr d)))
          (%uv-lambda (cadr d) (cddr d) env d nil)))
      (unless labels-p (enter))
      (%uv-body (cddr form) env '() nil))))

(defun %uv-macrolet (form env)
  (let* ((defs (if (listp (cadr form)) (cadr form) '()))
         (*macroexpand-scope* *macroexpand-scope*)
         (*lexical-operators* *lexical-operators*))
    (dolist (d defs)
      (when (and (consp d) (consp (cdr d)))
        (%uv-lambda (cadr d) (cddr d) '() d t)))
    (setf *macroexpand-scope* (cons defs *macroexpand-scope*))
    (setf *lexical-operators* (%walk-macrolet-lexical-operators defs))
    (%uv-body (cddr form) env '() nil)))

(defun %uv-symbol-macrolet (form env)
  (let ((inner env) (sms '()))
    (dolist (b (if (listp (cadr form)) (cadr form) '()))
      (when (and (consp b) (symbolp (car b)) (consp (cdr b)))
        (push (cons (car b) (cadr b)) sms)
        (push (list* (car b) :sm (cadr b)) inner)))
    (let ((*symbol-macros* (append (nreverse sms) *symbol-macros*))
          (*macroexpand-scope* (cons (cadr form) *macroexpand-scope*)))
      (%uv-body (cddr form) inner '() nil))))

(defun %uv-handler-case (form env)
  (%uv-walk (cadr form) env)
  (dolist (clause (cddr form))
    (when (and (consp clause) (consp (cdr clause)) (listp (cadr clause)))
      (if (eq (car clause) :no-error)
          (%uv-lambda (cadr clause) (cddr clause) env clause nil)
          (let ((v (car (cadr clause))))
            (if (and v (symbolp v))
                (let ((e (%uv-entry v clause '())))
                  (%uv-body (cddr clause) (cons e env) (list e) nil))
                (%uv-body (cddr clause) env '() nil)))))))

(defun %uv-restart-case (form env)
  (%uv-walk (cadr form) env)
  (dolist (clause (cddr form))
    (when (and (consp clause) (consp (cdr clause)) (listp (cadr clause)))
      (let ((body (cddr clause)))
        (loop while (and (consp body) (member (car body) '(:report :interactive :test))
                         (consp (cdr body)))
              do (when (consp (cadr body)) (%uv-walk (cadr body) env))
                 (setf body (cddr body)))
        (%uv-lambda (cadr clause) body env clause nil)))))

(defun %uv-walk (form env)
  (cond
    ((symbolp form) (when form (%uv-note form env 0)))
    ((atom form) nil)
    ((or (> *uv-level* 2000) (not (%stack-space-available-p)))
     (throw '%uv-give-up nil))
    (t
     (let ((head (car form))
           (*uv-level* (1+ *uv-level*)))
       (cond
         ((not (listp (cdr form))) nil)
         ((consp head)
          (when (and (eq (car head) 'lambda) (consp (cdr head)))
            (%uv-lambda (cadr head) (cddr head) env head nil))
          (%uv-walk-list (cdr form) env))
         ((not (symbolp head)) (%uv-walk-list (cdr form) env))
         ((%lexical-function-p head) (%uv-walk-list (cdr form) env))
         ((eq head 'quote) nil)
         ((eq head 'function)
          (let ((f (cadr form)))
            (when (and (consp f) (eq (car f) 'lambda) (consp (cdr f)))
              (%uv-lambda (cadr f) (cddr f) env f nil))))
         ((or (eq head 'lambda) (eq head 'defun) (eq head 'defmacro))
          (let ((f (if (eq head 'lambda) form (cdr form))))
            (when (consp (cdr f))
              (%uv-lambda (cadr f) (cddr f) env form (eq head 'defmacro)))))
         ((eq head 'setq)
          (loop for (var val) on (cdr form) by #'cddr
                do (when (symbolp var) (%uv-note var env 1))
                   (%uv-walk val env)))
         ((eq head 'multiple-value-setq)
          (when (listp (cadr form))
            (dolist (v (cadr form))
              (when (symbolp v) (%uv-note v env 0))))
          (%uv-walk-list (cddr form) env))
         ((eq head 'let) (%uv-let form env nil))
         ((eq head 'let*) (%uv-let form env t))
         ((eq head 'flet) (%uv-flet form env nil))
         ((eq head 'labels) (%uv-flet form env t))
         ((eq head 'macrolet) (%uv-macrolet form env))
         ((eq head 'symbol-macrolet) (%uv-symbol-macrolet form env))
         ((eq head 'locally) (%uv-body (cdr form) env '() nil))
         ((eq head 'handler-case) (%uv-handler-case form env))
         ((eq head 'restart-case) (%uv-restart-case form env))
         ((or (eq head 'handler-bind) (eq head 'restart-bind))
          (when (listp (cadr form))
            (dolist (b (cadr form))
              (when (consp b) (%uv-walk-list (cdr b) env))))
          (%uv-walk-list (cddr form) env))
         ((eq head 'cond)
          (dolist (clause (cdr form))
            (when (consp clause) (%uv-walk-list clause env))))
         ((or (eq head 'block) (eq head 'the) (eq head 'eval-when))
          (%uv-walk-list (cddr form) env))
         ((eq head 'return-from) (%uv-walk (caddr form) env))
         ((eq head 'tagbody)
          (dolist (f (cdr form)) (when (consp f) (%uv-walk f env))))
         ((or (eq head 'go) (eq head 'declare) (eq head 'define-symbol-macro)) nil)
         ((eq head 'load-time-value) (%uv-walk (cadr form) '()))
         (t
          (let ((expander (find-macro-expander head)))
            (let ((expanded (and expander (%walker-macroexpand form expander :none))))
              (if (and expanded (not (eq expanded form)))
                  (%uv-walk expanded env)
                  (%uv-walk-list (cdr form) env))))))))))

(defun note-unused-variables (form)
  "Signal the unused-variable style warnings for FORM, a top-level form that
   is about to be compiled."
  (unless *uv-warned*
    (setf *uv-warned* (make-hash-table :test #'eq)))
  (let ((*uv-warnings* '()))
    (catch '%uv-give-up
      (handler-case (%uv-walk form '())
        (error () (setf *uv-warnings* '()))))
    (dolist (w (nreverse *uv-warnings*))
      (warn 'style-warning :format-control (car w) :format-arguments (list (cdr w))))))

(defun maybe-note-unused-variables (form)
  "NOTE-UNUSED-VARIABLES for top-level FORM, unless COMPILE-FORM passes its
   top-level-ness on to subforms or an expansion, which are walked instead."
  (let ((op (car form)))
    (unless (and (symbolp op)
                 (or (member op '(progn locally macrolet symbol-macrolet eval-when) :test #'eq)
                     (and (not (gethash op *compile-form-handlers*))
                          (not (string= (symbol-name op) "DEFMACRO"))
                          (find-macro-expander op))))
      (note-unused-variables form))))

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
        (*compiler-name-syms* (or *compiler-name-syms* (list nil)))
        (*uninterned-var-names* (if *compiler-name-syms*
                                    *uninterned-var-names*
                                    (list nil)))
        (*specials* '())
        (*lexical-decl-frames* '())
        (*at-toplevel* t)
        (*toplevel-segments-p* nil)
        (*mv-mode-local* nil)
        (*mv-expect-args* nil)
        (*mv-exit-cleanup* nil)
        (*macroexpand-scope* '())
        (*macroexpand-cache* (list nil))
        (*macrolet-expanders* (list nil))
        (*fold-call-memo* (list nil))
        (*bmr-cache* (make-hash-table :test #'eq))
        (*ffv-free-cache* (list nil))
        (*uv-warned* nil)
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
        (*compiler-name-syms* (or *compiler-name-syms* (list nil)))
        (*uninterned-var-names* (if *compiler-name-syms*
                                    *uninterned-var-names*
                                    (list nil)))
        (*specials* '())
        (*lexical-decl-frames* '())
        (*at-toplevel* t)
        (*toplevel-segments-p* t)
        (*in-tail-position* t)
        ;; A macro's expander can call EVAL while a function body is being
        ;; compiled. The value mode of that body is a local of its method, not
        ;; of the method compiled here.
        (*mv-mode-local* nil)
        (*mv-expect-args* nil)
        (*mv-exit-cleanup* nil)
        (*macroexpand-scope* '())
        (*macroexpand-cache* (list nil))
        (*macrolet-expanders* (list nil))
        (*fold-call-memo* (list nil))
        (*bmr-cache* (make-hash-table :test #'eq))
        (*ffv-free-cache* (list nil))
        (*uv-warned* nil)
        ;; Code EVAL (or COMPILE) runs now, also when a macro calls it while
        ;; COMPILE-FILE compiles a top level form: its LOAD-TIME-VALUE forms
        ;; belong to it, not to the fasl being written.
        (*ltv-hoisted* nil)
        (*ltv-eval-now* t))
    (%close-toplevel-segments (compile-expr expr))))
