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
                            (inner-bound bnd)
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
                                                        make-instance %make-instance-with-initargs %make-instance-dyn
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
                             ;; An expansion EQUAL to its input is a fixed point.
                             (setf expanded (and exp (not (equal exp e)) exp))
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
;;; :ldloca, :dotnet-call-direct-locals (RECV + ARG list),
;;; :dotnet-static-direct-locals (ARG list). Background: a missed
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
           ;; An address taken may be read through, and written: it counts
           ;; as a read.
           ((:ldloc :ldloca) (let ((,var (cadr ,i))) ,@body))
           ;; The debug frame stores (:frame-set NAME KEY) and its box /
           ;; native-rep variants all read KEY.
           ((:frame-set :frame-set-box :frame-set-long :frame-set-double :frame-set-single)
            (let ((,var (caddr ,i))) ,@body))
           ;; (:dotnet-call-direct-locals TYPE METHOD RECV (ARG...) (PARAM...))
           ;; RECV and each ARG are locals read by the call.
           (:dotnet-call-direct-locals
            (let ((,var (nth 3 ,i))) ,@body)
            (dolist (,k (nth 4 ,i)) (let ((,var ,k)) ,@body)))
           ;; (:dotnet-static-direct-locals TYPE MEMBER (ARG...) (PARAM...))
           (:dotnet-static-direct-locals
            (dolist (,k (nth 3 ,i)) (let ((,var ,k)) ,@body)))
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
   absent from RENAME is left unchanged). Covers :declare-local/:ldloc/:stloc/:ldloca,
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
          (:ldloca (let ((n (rn (cadr instr))))
                     (if (eq n (cadr instr)) instr `(:ldloca ,n))))
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
          (:dotnet-static-direct-locals
           `(:dotnet-static-direct-locals
             ,(nth 1 instr) ,(nth 2 instr)
             ,(loop for k in (nth 3 instr) collect (rn k))
             ,(nth 4 instr)))
          (t instr)))))

;;; ============================================================
;;; Copy propagation: eliminate single-reference let locals
;;; ============================================================

(defconstant +instr-memo-min-length+ 256
  "Instruction lists shorter than this are not recorded by %CODE-CONCAT or
   ELIMINATE-SINGLE-REF-LOCALS: walking or copying one costs less than keeping
   the entry, and almost every list is short.")

(defvar *instr-summaries* nil
  "NIL, or a cons whose car is NIL or a %MAKE-INSTR-MEMO from an instruction
   list that ELIMINATE-SINGLE-REF-LOCALS returned to its %ESRL summary. Bound per
   top-level compile. An enclosing LET's list ends in its body's list, so the
   pass, and CODE-LAST-INSTR, stop walking where a summarized list begins.")

(defconstant +instr-memo-size+ 4)

(defun %make-instr-memo ()
  "A small table of the instruction lists recorded last, searched by EQ. Few
   lists are long enough to be recorded, and the one asked for was almost
   always recorded just before, so a bounded table keeps the nest linear
   without hashing every list or keeping all of them."
  (let ((v (make-array (1+ +instr-memo-size+) :initial-element nil)))
    (setf (svref v +instr-memo-size+) 0)
    v))

(defun %memo-get (memo key)
  (dotimes (i +instr-memo-size+ nil)
    (let ((e (svref memo i)))
      (when (and e (eq (car e) key)) (return (cdr e))))))

(defun %memo-rem (memo key)
  (dotimes (i +instr-memo-size+ nil)
    (let ((e (svref memo i)))
      (when (and e (eq (car e) key)) (setf (svref memo i) nil) (return)))))

(defun %memo-put (memo key val)
  (%memo-rem memo key)
  (let ((i (svref memo +instr-memo-size+)))
    (setf (svref memo i) (cons key val)
          (svref memo +instr-memo-size+) (mod (1+ i) +instr-memo-size+))
    val))

(defun %instr-summaries ()
  (and *instr-summaries*
       (or (car *instr-summaries*)
           (setf (car *instr-summaries*) (%make-instr-memo)))))

;;; A summary is (LAST ENTRY EXIT CANDS TABLE): the last cons of the list; its
;;; first instruction that is a list and not a :DECLARE-LOCAL (NIL if none);
;;; :PASS when there is no such instruction, else the key of a final
;;; (:STLOC KEY) or NIL; the keys of the (:STLOC KEY) (:LDLOC KEY) pairs in it;
;;; and NIL or an EQUAL table of key -> #(WRITES READS DECLARES TYPE) over the
;;; whole list.

(defvar *instr-walk-steps* nil
  "NIL, or a cons whose car counts the conses CODE-LAST-INSTR and
   ELIMINATE-SINGLE-REF-LOCALS walk and %CODE-CONCAT copies (for tests of how
   the work grows).")

(defvar *owned-code* nil
  "NIL, or a cons whose car is NIL or a %MAKE-INSTR-MEMO from an instruction
   list %CODE-CONCAT returned to (LAST-CONS OWNED-P). OWNED-P: every cons of the
   list was made for its caller alone, so the next %CODE-CONCAT that takes it
   as a part links it in place instead of copying it. Bound per top-level
   compile.")

(defun %owned-code ()
  (and *owned-code*
       (or (car *owned-code*)
           (setf (car *owned-code*) (%make-instr-memo)))))

(defun %forget-instr-memos ()
  "Drop the recorded instruction lists once a function's body is complete: the
   passes over the whole body copy it, and a recorded list would outlive them."
  (when *instr-summaries* (setf (car *instr-summaries*) nil))
  (when *owned-code* (setf (car *owned-code*) nil)))

(defun %code-concat (&rest parts)
  "(APPEND PARTS...) for instruction lists, sharing a long last part as APPEND
   does. A part %CODE-CONCAT returned with all its conses fresh is linked in
   place rather than copied. Only a result of +INSTR-MEMO-MIN-LENGTH+ or more
   is recorded."
  (when (block short
          (let ((n 0))
            (declare (fixnum n))
            (dolist (part parts t)
              (loop while (consp part)
                    do (when (>= (incf n) +instr-memo-min-length+)
                         (return-from short nil))
                       (setf part (cdr part))))))
    (return-from %code-concat (apply #'append parts)))
  (let ((table (%owned-code))
        (head nil) (tail nil) (owned t))
    (macrolet ((link (list last)
                 `(let ((l ,list))
                    (if tail (setf (cdr tail) l) (setf head l))
                    (setf tail ,last))))
      (loop for (part . more) on parts
            when (consp part)
              do (let ((e (and table (%memo-get table part))))
                   (cond ((and e (second e))
                          (%memo-rem table part)
                          (link part (%code-last-cons-from (first e))))
                         ((and (notany #'consp more)
                               (or e (nthcdr +instr-memo-min-length+ part)))
                          (setf owned nil)
                          (link part (if e
                                         (%code-last-cons-from (first e))
                                         (%code-last-cons part))))
                         (t
                          (let ((copy (copy-list part)))
                            (link copy (last copy)))
                          (when *instr-walk-steps*
                            (incf (car *instr-walk-steps*) (length part))))))))
    (when (and table (nthcdr (1- +instr-memo-min-length+) head))
      (%memo-put table head (list tail owned)))
    head))

(defun %code-append-instr (code instr)
  "(APPEND CODE (LIST INSTR)), as %CODE-CONCAT would make it."
  (if (nthcdr (- +instr-memo-min-length+ 2) code)
      (%code-concat code (list instr))
      (append code (list instr))))

(defun %code-last-cons-from (cons)
  "The last cons of the list CONS is in."
  (loop while (consp (cdr cons)) do (setf cons (cdr cons)))
  cons)

(defun %code-last-cons (code)
  "(LAST CODE), starting from what a summary or %CODE-CONCAT recorded."
  (let* ((memo (and *instr-summaries* (car *instr-summaries*)))
         (owned (and *owned-code* (car *owned-code*)))
         (s (and memo (%memo-get memo code)))
         (l (cond ((and (consp s) (not (eq (car s) :depth))) (first s))
                  ((and owned (car (%memo-get owned code))))
                  (t code))))
    (if (and *instr-walk-steps* (nthcdr (1- +instr-memo-min-length+) code))
        (let ((n 0))
          (loop while (consp (cdr l)) do (setf l (cdr l)) (incf n))
          (incf (car *instr-walk-steps*) n)
          l)
        (last l))))

(defun code-last-instr (code)
  "(CAR (LAST CODE)), from what a summary or %CODE-CONCAT recorded."
  (let ((l (%code-last-cons code)))
    (and (consp l) (car l))))

(defun %esrl-count (table list end)
  "Add the local references of LIST's instructions, up to the cons END, to TABLE."
  (flet ((entry (k)
           (or (gethash k table)
               (setf (gethash k table) (vector 0 0 0 nil)))))
    (loop for x on list
          until (eq x end)
          do (when *instr-walk-steps* (incf (car *instr-walk-steps*)))
             (let ((instr (car x)))
               (when (consp instr)
                 (when (eq (car instr) :declare-local)
                   (let ((e (entry (cadr instr))))
                     (incf (svref e 2))
                     (setf (svref e 3) (caddr instr))))
                 (do-instr-local-writes (k instr) (incf (svref (entry k) 0)))
                 (do-instr-local-reads (k instr) (incf (svref (entry k) 1))))))
    table))

(defun %esrl-count-before (table list end)
  "Add the references of LIST's instructions up to END to TABLE, the counts of
   what follows them: a declared type already in TABLE stays, being later."
  (maphash (lambda (k e)
             (let ((b (gethash k table)))
               (if b
                   (progn (incf (svref b 0) (svref e 0))
                          (incf (svref b 1) (svref e 1))
                          (when (zerop (svref b 2)) (setf (svref b 3) (svref e 3)))
                          (incf (svref b 2) (svref e 2)))
                   (setf (gethash k table) e))))
           (%esrl-count (make-hash-table :test #'equal) list end))
  table)

(defun %esrl-scan (list end prev)
  "Scan LIST up to the cons END for pairs, starting with PREV pending.
   Values: entry, final prev (:PASS when no pairing instruction), cands
   (reversed), last cons before END."
  (let ((entry nil) (state :pass) (cands '()) (last nil) (n 0))
    (loop for x on list
          until (eq x end)
          do (setf last x)
             (incf n)
             (let ((instr (car x)))
               (when (and (consp instr) (not (eq (car instr) :declare-local)))
                 (unless entry (setf entry instr))
                 (let ((p (if (eq state :pass) prev state)))
                   (case (car instr)
                     (:stloc (setf state (cadr instr)))
                     (:ldloc
                      (when (and p (equal p (cadr instr)))
                        (push p cands))
                      (setf state nil))
                     (t (setf state nil)))))))
    (when *instr-walk-steps* (incf (car *instr-walk-steps*) n))
    (values entry state cands last)))

(defun eliminate-single-ref-locals (instrs &optional body)
  "See %ESRL-PLAIN, which this computes the same result as. Inside a top-level
   compile the walk stops at the first cons that is a list this pass returned
   before, and continues from that list's summary, so a chain of nested LETs is
   not rescanned once per level."
  (let ((memo (%instr-summaries)))
    (if (or (null memo) (null (nthcdr (1- +instr-memo-min-length+) instrs)))
        (%esrl-plain instrs)
        (%esrl-incremental instrs memo body))))

(defconstant +esrl-plain-depth+ 8)

(defun %esrl-incremental (instrs memo body)
  ;; A summarized list can only be a tail of INSTRS: the tail at BODY, or a
  ;; few conses into it (a LET body may start with a MultipleValues.Reset).
  (let* ((r (and body
                 (loop for x on instrs
                       when (eq x body)
                         return (loop for y on x
                                      repeat 3
                                      when (%memo-get memo y) return y))))
         (rs (and r (%memo-get memo r))))
    ;; A nest only a few levels deep costs less rescanned than summarized: the
    ;; plain pass until the chain of LETs ending here is +ESRL-PLAIN-DEPTH+ deep.
    ;; Such a result records only its depth, and is summarized if a deeper LET
    ;; continues from it.
    (when (or (null rs)
              (and (eq (car rs) :depth) (< (cdr rs) +esrl-plain-depth+)))
      (let ((out (%esrl-plain instrs)))
        (when (consp out)
          (%memo-put memo out (cons :depth (if rs (1+ (cdr rs)) 1))))
        (return-from %esrl-incremental out)))
    (when (eq (car rs) :depth)
      (setf rs (multiple-value-bind (o-entry o-state o-cands o-last) (%esrl-scan r nil nil)
                 (list o-last o-entry o-state (remove-duplicates o-cands :test #'equal) nil))))
    (multiple-value-bind (entry state cands last) (%esrl-scan instrs r nil)
      (when rs
        (destructuring-bind (r-last r-entry r-exit r-cands r-table) rs
          (declare (ignore r-table))
          (when (and (not (eq state :pass)) state
                     (consp r-entry) (eq (car r-entry) :ldloc)
                     (equal state (cadr r-entry)))
            (push state cands))
          (setf cands (append r-cands cands))
          (unless entry (setf entry r-entry))
          (unless (eq r-exit :pass) (setf state r-exit))
          (setf last r-last)))
      (when (null cands)
        (let ((table (and rs (fifth rs))))
          (when table
            (%memo-rem memo r)
            (%esrl-count-before table instrs r))
          (%memo-put memo instrs (list last entry state '() table)))
        (return-from %esrl-incremental instrs))
      ;; The counts over the whole list: R's table, made now if R has none,
      ;; plus the instructions before R.
      (let* ((table (or (and rs (fifth rs))
                        (%esrl-count (make-hash-table :test #'equal) (or r instrs) nil)))
             (r-only (and r (loop for k in cands
                                  for e = (gethash k table)
                                  collect (cons k (if e (copy-seq e) (vector 0 0 0 nil)))))))
        (when r (%memo-rem memo r) (%esrl-count-before table instrs r))
        (let ((removable (loop for k in cands
                               for e = (gethash k table)
                               when (and e (= (svref e 0) 1) (= (svref e 1) 1)
                                         (string= (or (svref e 3) "") "LispObject"))
                                 collect k)))
          (setf removable (remove-duplicates removable :test #'equal))
          (when (null removable)
            (%memo-put memo instrs (list last entry state (remove-duplicates cands :test #'equal) table))
            (return-from %esrl-incremental instrs))
          (flet ((drop-p (instr)
                   (and (consp instr)
                        (member (car instr) '(:declare-local :stloc :ldloc))
                        (member (cadr instr) removable :test #'equal))))
            ;; Inside R, a removed key may only be read by R's first pairing
            ;; instruction; otherwise copy the whole list as %ESRL-PLAIN does.
            (let* ((r-entry (and rs (second rs)))
                   (entry-key (and (consp r-entry) (eq (car r-entry) :ldloc)
                                   (member (cadr r-entry) removable :test #'equal)
                                   (cadr r-entry)))
                   (fast (and r
                              (loop for k in removable
                                    for e = (cdr (assoc k r-only :test #'equal))
                                    always (or (null e)
                                               (and (= (svref e 0) 0) (= (svref e 1) 0)
                                                    (= (svref e 2) 0))
                                               (and (equal k entry-key)
                                                    (= (svref e 0) 0) (= (svref e 1) 1)
                                                    (= (svref e 2) 0)))))))
              (if (not fast)
                  (let ((out (remove-if #'drop-p instrs)))
                    (when (consp out)
                      (dolist (k removable) (remhash k table))
                      (multiple-value-bind (o-entry o-state o-cands o-last) (%esrl-scan out nil nil)
                        (%memo-put memo out
                                   (list o-last o-entry o-state
                                         (remove-duplicates o-cands :test #'equal) table))))
                    out)
                  (let* ((head '())
                         (tail r))
                    (loop for x on instrs until (eq x r)
                          unless (drop-p (car x)) do (push (car x) head))
                    (when entry-key
                      (loop until (eq (car tail) r-entry)
                            do (push (car tail) head) (setf tail (cdr tail)))
                      (setf tail (cdr tail)))
                    (dolist (k removable) (remhash k table))
                    (let ((out (nreconc head tail)))
                      (when (consp out)
                        (multiple-value-bind (t-entry t-state t-cands t-last)
                            (if (eq tail r)
                                (values (second rs) (third rs) nil (first rs))
                                (let ((e (loop for i in tail
                                               when (and (consp i) (not (eq (car i) :declare-local)))
                                                 return i)))
                                  (values e (if e (third rs) :pass) nil
                                          (and (consp tail) (first rs)))))
                          (declare (ignore t-cands))
                          (multiple-value-bind (h-entry h-state h-cands h-last)
                              (%esrl-scan out tail nil)
                            (let ((c (append (remove-if (lambda (k) (member k removable :test #'equal))
                                                        (fourth rs))
                                             h-cands)))
                              (when (and (not (eq h-state :pass)) h-state
                                         (consp t-entry) (eq (car t-entry) :ldloc)
                                         (equal h-state (cadr t-entry)))
                                (push h-state c))
                              (%memo-put memo out
                                    (list (or t-last h-last)
                                          (or h-entry t-entry)
                                          (if (eq t-state :pass) h-state t-state)
                                          (remove-duplicates c :test #'equal)
                                          table))))))
                      out))))))))))

(defun %esrl-plain (instrs)
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
      (return-from %esrl-plain instrs))
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
              ;; P15: a checked fixnum add of a constant that is not negative
              ;; needs one compare for its overflow test instead of the
              ;; sign arithmetic for an operand of either sign.
              ((and (consp i1) (eq (car i1) :ldc-i8) (integerp (cadr i1)) (>= (cadr i1) 0)
                    (consp i2) (equal i2 '(:call "Runtime.AddFixnumChecked")))
               (setf changed t)
               (push i1 out)
               (push (list :call "Runtime.AddFixnumCheckedNonNeg") out)
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

(defconstant +poll-save-max-locals+ 16)

(defun %poll-loop-region (v i labels)
  "The loop a back-edge poll at index I of V closes, as (START . END) indexes:
   a poll followed by a branch back to an earlier label, or a poll right after
   a label that a later branch jumps back to. NIL for any other shape."
  (let ((next (and (< (1+ i) (length v)) (svref v (1+ i))))
        (prev (and (> i 0) (svref v (1- i)))))
    (cond ((and (consp next) (eq (car next) :br))
           (let ((start (gethash (cadr next) labels)))
             (and start (< start i) (cons start i))))
          ((and (consp prev) (eq (car prev) :label))
           (let ((end nil))
             (loop for k from (1+ i) below (length v)
                   for x = (svref v k)
                   when (and (consp x) (member (car x) '(:br :brtrue :brfalse :beq :bne :blt :bgt :ble :bge
                                                     :bne-un :bge-un :bgt-un :ble-un :blt-un))
                             (eq (cadr x) (cadr prev)))
                     do (setf end k))
             (and end (cons (1- i) end)))))))

(defun %poll-upward-float-reads (v start end types)
  "The raw float locals of TYPES read in V between START and END before the
   region writes them (in instruction order): the values the loop carries
   round its back-edge."
  (let ((written '()) (read '()))
    (loop for k from start to end
          for x = (svref v k)
          when (and (consp x) (member (car x) '(:ldloc :stloc)) (gethash (cadr x) types))
            do (if (eq (car x) :stloc)
                   (pushnew (cadr x) written)
                   (unless (or (member (cadr x) written) (member (cadr x) read))
                     (push (cadr x) read))))
    (let ((r (nreverse read)))
      (if (> (length r) +poll-save-max-locals+)
          (subseq r 0 +poll-save-max-locals+)
          r))))

(defun %poll-instr-p (x)
  (and (consp x) (eq (car x) :call) (equal (cadr x) "ConditionSystem.PollInterrupt")))

(defun %poll-saving-instrs (locals types)
  (let* ((first (subseq locals 0 (min 4 (length locals))))
         (rest (nthcdr (length first) locals))
         (buf (gen-local "PSAVE"))
         (save (gen-label "PSAVE"))
         (skip (gen-label "PSKIP")))
    `((:call "ConditionSystem.PollPending")
      (:brfalse ,skip)
      (:call "ConditionSystem.PollSavesFloats")
      (:brtrue ,save)
      (:call "ConditionSystem.PollInterruptSaved")
      (:br ,skip)
      (:label ,save)
      (:declare-local ,buf "Double[]")
      (:ldc-i4 ,(length locals))
      ,@(loop for x in first
              append `((:ldloc ,x)
                       ,@(when (equal (gethash x types) "Single") '((:conv-r8)))))
      (:call ,(format nil "ConditionSystem.PollSave~D" (length first)))
      (:stloc ,buf)
      ,@(loop for x in rest for k from (length first)
              append `((:ldloc ,buf) (:ldc-i4 ,k) (:ldloc ,x)
                       ,@(when (equal (gethash x types) "Single") '((:conv-r8)))
                       (:call "ConditionSystem.PollSaveD")))
      (:call "ConditionSystem.PollInterruptSaved")
      ,@(loop for x in locals for k from 0
              append `((:ldloc ,buf) (:ldc-i4 ,k) (:call "ConditionSystem.PollLoadD")
                       ,@(when (equal (gethash x types) "Single") '((:conv-r4)))
                       (:stloc ,x)))
      (:label ,skip))))

(defun %poll-save-raw-floats (instrs)
  "A loop's back-edge poll whose loop carries raw DOUBLE-FLOAT or SINGLE-FLOAT
   locals saves them to a heap buffer around its slow call and reads them
   back. On x64 Unix no XMM register survives a call, and a float local live
   across the slow call was stored and reloaded on every iteration, on the
   path of the loop's own dependency chain. Saved this way, no float local
   is live across the call. Only the body's own instructions: a nested
   closure body is another method."
  (if (not (find-if #'%poll-instr-p instrs))
      instrs
      (let ((types (make-hash-table :test #'eq))
            (labels (make-hash-table :test #'eq)))
        (dolist (x instrs)
          (when (consp x)
            (case (car x)
              (:declare-local
               (when (member (caddr x) '("Double" "Single") :test #'equal)
                 (setf (gethash (cadr x) types) (caddr x)))))))
        (if (zerop (hash-table-count types))
            instrs
            (let* ((v (coerce instrs 'simple-vector))
                   (n (length v)))
              (dotimes (k n)
                (let ((x (svref v k)))
                  (when (and (consp x) (eq (car x) :label))
                    (setf (gethash (cadr x) labels) k))))
              (loop for k from 0 below n
                    for x = (svref v k)
                    append (let* ((region (and (%poll-instr-p x) (%poll-loop-region v k labels)))
                                  (locals (and region
                                               (%poll-upward-float-reads
                                                v (car region) (cdr region) types))))
                             (if locals
                                 (%poll-saving-instrs locals types)
                                 (list x)))))))))

(defun %fold-constant-fixnum-locals (instrs)
  "A LispObject local stored once, from a boxed integer constant or from such a
   local, and read only to be unboxed or copied into such a local, is replaced
   by the constant at each unboxing read. Coalton's arithmetic binds each
   operand with a LET, so (+ i 1) on a raw counter boxed the 1 into a temporary
   and unboxed it on every iteration."
  (unless (loop for (a b c) on instrs
                thereis (and (consp a) (eq (car a) :ldc-i8)
                             (equal b '(:call "Fixnum.Make"))
                             (consp c) (eq (car c) :stloc)))
    (return-from %fold-constant-fixnum-locals instrs))
  (let ((stores (make-hash-table :test #'eq))
        (src (make-hash-table :test #'eq))
        (reads (make-hash-table :test #'eq))
        (bad (make-hash-table :test #'eq))
        (others '()))
    (loop for (y z a b) on (list* nil nil instrs)
          when (consp a)
            do (case (car a)
                 (:stloc
                  (let ((l (cadr a)))
                    (incf (gethash l stores 0))
                    (setf (gethash l src)
                          (cond ((and (consp z) (equal z '(:call "Fixnum.Make"))
                                      (consp y) (eq (car y) :ldc-i8))
                                 (list :const (cadr y)))
                                ((and (consp z) (eq (car z) :ldloc))
                                 (list :copy (cadr z)))
                                (t (list :other))))))
                 (:ldloc
                  (cond ((and (consp b) (eq (car b) :unbox-fixnum)))
                        ((and (consp b) (eq (car b) :stloc))
                         (push (cadr b) (gethash (cadr a) reads)))
                        (t (setf (gethash (cadr a) bad) t))))
                 ((:declare-local))
                 (t (push a others))))
    ;; Only a local stored once from a constant or a copy can be folded, so the
    ;; other instructions are searched for those locals alone.
    (let ((cands (make-hash-table :test #'eq))
          (names (make-hash-table :test #'equal))
          (seen (make-hash-table :test #'eq)))
      (maphash (lambda (l n)
                 (when (and (eql n 1) (not (gethash l bad))
                            (member (car (gethash l src)) '(:const :copy)))
                   (setf (gethash l cands) t)
                   (push l (gethash (symbol-name l) names))))
               stores)
      (when (zerop (hash-table-count cands))
        (return-from %fold-constant-fixnum-locals instrs))
      (labels ((walk (x)
                 (cond ((consp x)
                        (unless (gethash x seen)
                          (setf (gethash x seen) t)
                          (walk (car x)) (walk (cdr x))))
                       ((and x (symbolp x) (not (keywordp x)))
                        (when (gethash x cands) (setf (gethash x bad) t)))
                       ((stringp x)
                        (dolist (l (gethash x names)) (setf (gethash l bad) t))))))
        (dolist (a others) (walk (cdr a)))))
    (let ((value (make-hash-table :test #'eq))
          (changed t))
      (maphash (lambda (l n)
                 (when (and (eql n 1) (not (gethash l bad))
                            (member (car (gethash l src)) '(:const :copy)))
                   (setf (gethash l value) :unknown)))
               stores)
      (loop while changed
            do (setf changed nil)
               (maphash (lambda (l v)
                          (let* ((s (gethash l src))
                                 (nv (if (eq (car s) :const)
                                         (cadr s)
                                         (multiple-value-bind (xv found) (gethash (cadr s) value)
                                           (if (and found (not (eq xv :unknown))) xv v)))))
                            (cond ((or (and (eq (car s) :copy)
                                            (not (nth-value 1 (gethash (cadr s) value))))
                                       (some (lambda (to) (not (nth-value 1 (gethash to value))))
                                             (gethash l reads)))
                                   (remhash l value) (setf changed t))
                                  ((not (eql nv v))
                                   (setf (gethash l value) nv changed t)))))
                        value))
      (maphash (lambda (l v) (when (eq v :unknown) (remhash l value))) value)
      (if (zerop (hash-table-count value))
          instrs
          (let ((out '()))
            (loop while instrs
                  do (let ((a (pop instrs)))
                       (cond ((and (consp a) (eq (car a) :ldc-i8)
                                   (equal (first instrs) '(:call "Fixnum.Make"))
                                   (consp (second instrs)) (eq (car (second instrs)) :stloc)
                                   (gethash (cadr (second instrs)) value))
                              (pop instrs) (pop instrs))
                             ((and (consp a) (eq (car a) :ldloc)
                                   (consp (first instrs)) (eq (car (first instrs)) :stloc)
                                   (gethash (cadr (first instrs)) value))
                              (pop instrs))
                             ((and (consp a) (eq (car a) :ldloc) (gethash (cadr a) value))
                              (pop instrs)
                              (push (list :ldc-i8 (gethash (cadr a) value)) out))
                             (t (push a out)))))
            (nreverse out))))))

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
  (%forget-instr-memos)
  (%poll-save-raw-floats
   (%thread-binding-stack
    (peephole-optimize
     (%fold-constant-fixnum-locals
     (%hoist-loop-mv-resets
      (if *emit-source-lines*
          (elide-unused-rest-list instrs)
          (%merge-disjoint-locals (elide-unused-rest-list instrs)))))))))

(defconstant +max-special-group+ 8
  "Most special bindings one grouped runtime push takes.")

(defconstant +min-special-group+ 3
  "Fewest special bindings worth a grouped runtime push.")

(defparameter +group-push-arities+
  (loop for n from 2 to +max-special-group+
        collect (cons (format nil "DynamicBindings.PushGroup~D" n) n)))

(defun %group-push-arity (i)
  (and (consp i) (eq (car i) :call)
       (cdr (assoc (cadr i) +group-push-arities+ :test #'equal))))

(defparameter +binding-stack-calls+
  (append
   '(("DynamicBindings.Get" . "DynamicBindings.GetOn")
     ("DynamicBindings.SetChecked" . "DynamicBindings.SetCheckedOn")
     ("DynamicBindings.Push" . "DynamicBindings.PushOn")
     ("DynamicBindings.Pop" . "DynamicBindings.PopOn")
     ("DynamicBindings.BindDepth" . "DynamicBindings.BindDepthOn")
     ("DynamicBindings.BindGroupDepth" . "DynamicBindings.BindGroupDepthOn")
     ("DynamicBindings.UnbindTo" . "DynamicBindings.UnbindToOn"))
   (loop for n from 2 to +max-special-group+
         collect (cons (format nil "DynamicBindings.PushGroup~D" n)
                       (format nil "DynamicBindings.PushGroup~DOn" n))))
  "Binding stack operations, each paired with the entry that takes the thread's
   stack as its last argument.")

(defvar *bound-special-loads* (make-hash-table :test #'equal :synchronized t)
  "The symbol loads (SIL forms) of the special variables that a body compiled
   so far binds.")

(defun %sym-load-p (i)
  (and (consp i) (member (car i) '(:load-sym :load-sym-pkg :load-const))))

(defun %note-bound-specials (instrs)
  "Record the variables bound by the body: each bind is the symbol load, a
   castclass, the value from a local and a push."
  (let ((v (coerce instrs 'vector)))
    (dotimes (idx (length v))
      (let ((i (aref v idx)))
        (cond
          ((and (consp i) (eq (car i) :call)
                (equal (cadr i) "DynamicBindings.Push") (>= idx 3)
                (consp (aref v (1- idx))) (eq (car (aref v (1- idx))) :ldloc)
                (equal (aref v (- idx 2)) '(:castclass "Symbol"))
                (%sym-load-p (aref v (- idx 3))))
           (setf (gethash (aref v (- idx 3)) *bound-special-loads*) t))
          ((%group-push-arity i)
           (dotimes (k (%group-push-arity i))
             (let ((j (- idx (* 3 (1+ k)))))
               (when (and (>= j 0)
                          (%sym-load-p (aref v j))
                          (equal (aref v (1+ j)) '(:castclass "Symbol"))
                          (consp (aref v (+ j 2)))
                          (eq (car (aref v (+ j 2))) :ldloc))
                 (setf (gethash (aref v j) *bound-special-loads*) t))))))))))

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
                      (or (equal (cadr i) "DynamicBindings.Push")
                          (%group-push-arity i))))
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
    "Runtime.IsNilObj" "Runtime.IsConsObject" "Runtime.IsAtomObject" "Runtime.IsListObject"
    "FloatLane.AddD" "FloatLane.SubtractD"
    "FloatLane.MultiplyD" "FloatLane.DivideD"
    "FloatLane.AddS" "FloatLane.SubtractS"
    "FloatLane.MultiplyS" "FloatLane.DivideS"
    "FloatLane.NarrowD" "FloatLane.SinhD"
    "FloatLane.CoshD" "Runtime.ExpDouble" "FloatLane.NonFiniteD" "FloatLane.NonFiniteS" "FloatLane.SinD" "FloatLane.CosD" "FloatLane.TanD" "FloatLane.LogD"))

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
    ;; A local whose address is taken keeps its own slot: the callee may write
    ;; it at any time, and its first reference may read the null it starts with.
    (let ((candidates nil)
          (addressed (let ((h nil))
                       (dolist (instr instrs h)
                         (when (and (consp instr) (eq (car instr) :ldloca))
                           (push (cadr instr) h))))))
      (maphash (lambda (key type)
                 (when (and (string= type "LispObject")
                            (not (member key addressed :test #'equal))
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
  "Lazy EQ table (%LAZY-TABLE-GET): binding form -> the variables of it already reported.")

(defvar *uv-warnings* nil)

(defvar *uv-sm-depth* 0)

(defvar *uv-depth* 0)

(defvar *uv-level* 0)

(defvar *uv-current-form* nil
  "The nearest enclosing cons form of the symbol being walked.")

(defvar *uv-local-functions* nil
  "Function names bound by enclosing FLET / LABELS forms.")

(defvar *uv-defining* nil
  "Names being defined by the DEFVAR / DEFPARAMETER / DEFCONSTANT form the walk is in.
   Their expansion assigns the name before the compile-time half of the definition
   has made it special, which is not a use of an undefined variable.")

;;; --- Argument counts of the COMMON-LISP functions ---
;;; (NAME MIN MAX): MAX is NIL when the function takes &rest or &key. Used to warn about a call with
;;; the wrong number of arguments at compile time.
(defvar *cl-function-arities*
  (let ((h (make-hash-table :test #'eq)))
    (dolist (row '(
   ("*" 0 NIL) ("+" 0 NIL) ("-" 1 NIL) ("/" 1 NIL) ("/=" 1 NIL) ("1+" 1 1) ("1-" 1 1) ("<" 1 NIL)
   ("<=" 1 NIL) ("=" 1 NIL) (">" 1 NIL) (">=" 1 NIL) ("ABORT" 0 1) ("ABS" 1 1) ("ACONS" 3 3)
   ("ACOS" 1 1) ("ACOSH" 1 1) ("ADJOIN" 2 NIL) ("ADJUST-ARRAY" 2 NIL) ("ADJUSTABLE-ARRAY-P" 1 1)
   ("ALPHA-CHAR-P" 1 1) ("ALPHANUMERICP" 1 1) ("APPEND" 0 NIL) ("APPLY" 2 NIL) ("APROPOS" 1 3)
   ("APROPOS-LIST" 1 5) ("AREF" 1 NIL) ("ARITHMETIC-ERROR-OPERANDS" 1 1)
   ("ARITHMETIC-ERROR-OPERATION" 1 1) ("ARRAY-DIMENSION" 2 2) ("ARRAY-DIMENSIONS" 1 1)
   ("ARRAY-DISPLACEMENT" 1 1) ("ARRAY-ELEMENT-TYPE" 1 1) ("ARRAY-HAS-FILL-POINTER-P" 1 1)
   ("ARRAY-IN-BOUNDS-P" 1 NIL) ("ARRAY-RANK" 1 1) ("ARRAY-ROW-MAJOR-INDEX" 1 NIL)
   ("ARRAY-TOTAL-SIZE" 1 1) ("ARRAYP" 1 1) ("ASH" 2 2) ("ASIN" 1 1) ("ASINH" 1 1) ("ASSOC" 2 NIL)
   ("ASSOC-IF" 2 NIL) ("ASSOC-IF-NOT" 2 NIL) ("ATAN" 1 2) ("ATANH" 1 1) ("ATOM" 1 1) ("BIT" 1 NIL)
   ("BIT-AND" 2 3) ("BIT-ANDC1" 2 3) ("BIT-ANDC2" 2 3) ("BIT-EQV" 2 3) ("BIT-IOR" 2 3)
   ("BIT-NAND" 2 3) ("BIT-NOR" 2 3) ("BIT-NOT" 1 2) ("BIT-ORC1" 2 3) ("BIT-ORC2" 2 3)
   ("BIT-VECTOR-P" 1 1) ("BIT-XOR" 2 3) ("BOOLE" 3 3) ("BOTH-CASE-P" 1 1) ("BOUNDP" 1 1)
   ("BREAK" 0 NIL) ("BROADCAST-STREAM-STREAMS" 1 1) ("BUTLAST" 1 2) ("BYTE" 2 2)
   ("BYTE-POSITION" 1 1) ("BYTE-SIZE" 1 1) ("CAAAAR" 1 1) ("CAAADR" 1 1) ("CAAAR" 1 1)
   ("CAADAR" 1 1) ("CAADDR" 1 1) ("CAADR" 1 1) ("CAAR" 1 1) ("CADAAR" 1 1) ("CADADR" 1 1)
   ("CADAR" 1 1) ("CADDAR" 1 1) ("CADDDR" 1 1) ("CADDR" 1 1) ("CADR" 1 1) ("CAR" 1 1)
   ("CDAAAR" 1 1) ("CDAADR" 1 1) ("CDAAR" 1 1) ("CDADAR" 1 1) ("CDADDR" 1 1) ("CDADR" 1 1)
   ("CDAR" 1 1) ("CDDAAR" 1 1) ("CDDADR" 1 1) ("CDDAR" 1 1) ("CDDDAR" 1 1) ("CDDDDR" 1 1)
   ("CDDDR" 1 1) ("CDDR" 1 1) ("CDR" 1 1) ("CEILING" 1 2) ("CELL-ERROR-NAME" 1 1) ("CERROR" 2 NIL)
   ("CHAR" 2 2) ("CHAR-CODE" 1 1) ("CHAR-DOWNCASE" 1 1) ("CHAR-EQUAL" 1 NIL)
   ("CHAR-GREATERP" 1 NIL) ("CHAR-INT" 1 1) ("CHAR-LESSP" 1 NIL) ("CHAR-NAME" 1 1)
   ("CHAR-NOT-EQUAL" 1 NIL) ("CHAR-NOT-GREATERP" 1 NIL) ("CHAR-NOT-LESSP" 1 NIL)
   ("CHAR-UPCASE" 1 1) ("CHAR/=" 1 NIL) ("CHAR<" 1 NIL) ("CHAR<=" 1 NIL) ("CHAR=" 1 NIL)
   ("CHAR>" 1 NIL) ("CHAR>=" 1 NIL) ("CHARACTER" 1 1) ("CHARACTERP" 1 1) ("CIS" 1 1)
   ("CLASS-OF" 1 1) ("CLEAR-INPUT" 0 1) ("CLEAR-OUTPUT" 0 1) ("CLRHASH" 1 1) ("CODE-CHAR" 1 1)
   ("COERCE" 2 2) ("COMPILE" 1 2) ("COMPILE-FILE" 1 NIL) ("COMPILE-FILE-PATHNAME" 1 NIL)
   ("COMPILED-FUNCTION-P" 1 1) ("COMPILER-MACRO-FUNCTION" 1 2) ("COMPLEMENT" 1 1) ("COMPLEX" 1 2)
   ("COMPLEXP" 1 1) ("COMPUTE-RESTARTS" 0 1) ("CONCATENATE" 1 NIL)
   ("CONCATENATED-STREAM-STREAMS" 1 1) ("CONJUGATE" 1 1) ("CONS" 2 2) ("CONSP" 1 1)
   ("CONSTANTLY" 1 1) ("CONSTANTP" 1 2) ("CONTINUE" 0 1) ("COPY-ALIST" 1 1) ("COPY-LIST" 1 1)
   ("COPY-PPRINT-DISPATCH" 0 1) ("COPY-READTABLE" 0 2) ("COPY-SEQ" 1 1) ("COPY-STRUCTURE" 1 1)
   ("COPY-SYMBOL" 1 2) ("COPY-TREE" 1 1) ("COS" 1 1) ("COSH" 1 1) ("COUNT" 2 NIL)
   ("COUNT-IF" 2 NIL) ("COUNT-IF-NOT" 2 NIL) ("DECODE-FLOAT" 1 1) ("DECODE-UNIVERSAL-TIME" 1 2)
   ("DELETE" 2 NIL) ("DELETE-DUPLICATES" 1 NIL) ("DELETE-FILE" 1 1) ("DELETE-IF" 2 NIL)
   ("DELETE-IF-NOT" 2 NIL) ("DELETE-PACKAGE" 1 1) ("DENOMINATOR" 1 1) ("DEPOSIT-FIELD" 3 3)
   ("DESCRIBE" 1 2) ("DIGIT-CHAR" 1 2) ("DIGIT-CHAR-P" 1 2) ("DIRECTORY" 1 NIL)
   ("DIRECTORY-NAMESTRING" 1 1) ("DISASSEMBLE" 1 NIL) ("DPB" 3 3) ("DRIBBLE" 0 NIL)
   ("ECHO-STREAM-INPUT-STREAM" 1 1) ("ECHO-STREAM-OUTPUT-STREAM" 1 1) ("ED" 0 1) ("EIGHTH" 1 1)
   ("ELT" 2 2) ("ENCODE-UNIVERSAL-TIME" 6 7) ("ENDP" 1 1) ("ENOUGH-NAMESTRING" 1 2)
   ("ENSURE-DIRECTORIES-EXIST" 1 NIL) ("ENSURE-GENERIC-FUNCTION" 1 NIL) ("EQ" 2 2) ("EQL" 2 2)
   ("EQUAL" 2 2) ("EQUALP" 2 2) ("ERROR" 1 NIL) ("EVAL" 1 1) ("EVENP" 1 1) ("EVERY" 2 NIL)
   ("EXP" 1 1) ("EXPORT" 1 2) ("EXPT" 2 2) ("FBOUNDP" 1 1) ("FCEILING" 1 2) ("FDEFINITION" 1 1)
   ("FFLOOR" 1 2) ("FIFTH" 1 1) ("FILE-AUTHOR" 1 1) ("FILE-ERROR-PATHNAME" 1 1) ("FILE-LENGTH" 1 1)
   ("FILE-NAMESTRING" 1 1) ("FILE-POSITION" 1 2) ("FILE-STRING-LENGTH" 2 2) ("FILE-WRITE-DATE" 1 1)
   ("FILL" 2 NIL) ("FILL-POINTER" 1 1) ("FIND" 2 NIL) ("FIND-ALL-SYMBOLS" 1 1) ("FIND-CLASS" 1 3)
   ("FIND-IF" 2 NIL) ("FIND-IF-NOT" 2 NIL) ("FIND-PACKAGE" 1 1) ("FIND-RESTART" 1 2)
   ("FIND-SYMBOL" 1 2) ("FINISH-OUTPUT" 0 1) ("FIRST" 1 1) ("FLOAT" 1 2) ("FLOAT-DIGITS" 1 1)
   ("FLOAT-PRECISION" 1 1) ("FLOAT-RADIX" 1 1) ("FLOAT-SIGN" 1 2) ("FLOATP" 1 1) ("FLOOR" 1 2)
   ("FMAKUNBOUND" 1 1) ("FORCE-OUTPUT" 0 1) ("FORMAT" 2 NIL) ("FOURTH" 1 1) ("FRESH-LINE" 0 1)
   ("FROUND" 1 2) ("FTRUNCATE" 1 2) ("FUNCALL" 1 NIL) ("FUNCTION-LAMBDA-EXPRESSION" 1 1)
   ("FUNCTIONP" 1 1) ("GCD" 0 NIL) ("GENSYM" 0 1) ("GENTEMP" 0 2) ("GET" 2 3)
   ("GET-DECODED-TIME" 0 0) ("GET-DISPATCH-MACRO-CHARACTER" 2 3) ("GET-INTERNAL-REAL-TIME" 0 0)
   ("GET-INTERNAL-RUN-TIME" 0 0) ("GET-MACRO-CHARACTER" 1 2) ("GET-OUTPUT-STREAM-STRING" 1 1)
   ("GET-PROPERTIES" 2 2) ("GET-SETF-EXPANSION" 1 2) ("GET-UNIVERSAL-TIME" 0 0) ("GETF" 2 3)
   ("GETHASH" 2 3) ("GRAPHIC-CHAR-P" 1 1) ("HASH-TABLE-COUNT" 1 1) ("HASH-TABLE-P" 1 1)
   ("HASH-TABLE-REHASH-SIZE" 1 1) ("HASH-TABLE-REHASH-THRESHOLD" 1 1) ("HASH-TABLE-SIZE" 1 1)
   ("HASH-TABLE-TEST" 1 1) ("HOST-NAMESTRING" 1 1) ("IDENTITY" 1 1) ("IMAGPART" 1 1) ("IMPORT" 1 2)
   ("INSPECT" 1 1) ("INTEGER-DECODE-FLOAT" 1 1) ("INTEGER-LENGTH" 1 1) ("INTEGERP" 1 1)
   ("INTERN" 1 2) ("INTERSECTION" 2 NIL) ("INVALID-METHOD-ERROR" 2 NIL) ("INVOKE-DEBUGGER" 1 1)
   ("INVOKE-RESTART" 1 NIL) ("INVOKE-RESTART-INTERACTIVELY" 1 1) ("ISQRT" 1 1) ("KEYWORDP" 1 1)
   ("LAST" 1 2) ("LCM" 0 NIL) ("LDB" 2 2) ("LDB-TEST" 2 2) ("LDIFF" 2 2) ("LENGTH" 1 1)
   ("LISP-IMPLEMENTATION-TYPE" 0 0) ("LISP-IMPLEMENTATION-VERSION" 0 0) ("LIST" 0 NIL)
   ("LIST*" 1 NIL) ("LIST-ALL-PACKAGES" 0 0) ("LIST-LENGTH" 1 1) ("LISTEN" 0 1) ("LISTP" 1 1)
   ("LOAD" 1 NIL) ("LOAD-LOGICAL-PATHNAME-TRANSLATIONS" 1 1) ("LOG" 1 2) ("LOGAND" 0 NIL)
   ("LOGANDC1" 2 2) ("LOGANDC2" 2 2) ("LOGBITP" 2 2) ("LOGCOUNT" 1 1) ("LOGEQV" 0 NIL)
   ("LOGICAL-PATHNAME" 1 1) ("LOGICAL-PATHNAME-TRANSLATIONS" 1 1) ("LOGIOR" 0 NIL) ("LOGNAND" 2 2)
   ("LOGNOR" 2 2) ("LOGNOT" 1 1) ("LOGORC1" 2 2) ("LOGORC2" 2 2) ("LOGTEST" 2 2) ("LOGXOR" 0 NIL)
   ("LONG-SITE-NAME" 0 0) ("LOWER-CASE-P" 1 1) ("MACHINE-INSTANCE" 0 0) ("MACHINE-TYPE" 0 0)
   ("MACHINE-VERSION" 0 0) ("MACRO-FUNCTION" 1 2) ("MACROEXPAND" 1 2) ("MACROEXPAND-1" 1 2)
   ("MAKE-ARRAY" 1 NIL) ("MAKE-BROADCAST-STREAM" 0 NIL) ("MAKE-CONCATENATED-STREAM" 0 NIL)
   ("MAKE-CONDITION" 1 NIL) ("MAKE-DISPATCH-MACRO-CHARACTER" 1 3) ("MAKE-ECHO-STREAM" 2 2)
   ("MAKE-HASH-TABLE" 0 NIL) ("MAKE-LIST" 1 NIL) ("MAKE-LOAD-FORM-SAVING-SLOTS" 1 NIL)
   ("MAKE-PACKAGE" 1 NIL) ("MAKE-PATHNAME" 0 NIL) ("MAKE-RANDOM-STATE" 0 1) ("MAKE-SEQUENCE" 2 NIL)
   ("MAKE-STRING" 1 NIL) ("MAKE-STRING-INPUT-STREAM" 1 3) ("MAKE-STRING-OUTPUT-STREAM" 0 NIL)
   ("MAKE-SYMBOL" 1 1) ("MAKE-SYNONYM-STREAM" 1 1) ("MAKE-TWO-WAY-STREAM" 2 2) ("MAKUNBOUND" 1 1)
   ("MAP" 3 NIL) ("MAP-INTO" 2 NIL) ("MAPC" 2 NIL) ("MAPCAN" 2 NIL) ("MAPCAR" 2 NIL)
   ("MAPCON" 2 NIL) ("MAPHASH" 2 2) ("MAPL" 2 NIL) ("MAPLIST" 2 NIL) ("MASK-FIELD" 2 2)
   ("MAX" 1 NIL) ("MEMBER" 2 NIL) ("MEMBER-IF" 2 NIL) ("MEMBER-IF-NOT" 2 NIL) ("MERGE" 4 NIL)
   ("MERGE-PATHNAMES" 1 3) ("METHOD-COMBINATION-ERROR" 1 NIL) ("MIN" 1 NIL) ("MINUSP" 1 1)
   ("MISMATCH" 2 NIL) ("MOD" 2 2) ("MUFFLE-WARNING" 0 1) ("NAME-CHAR" 1 1) ("NAMESTRING" 1 1)
   ("NBUTLAST" 1 2) ("NCONC" 0 NIL) ("NINTERSECTION" 2 NIL) ("NINTH" 1 1) ("NOT" 1 1)
   ("NOTANY" 2 NIL) ("NOTEVERY" 2 NIL) ("NRECONC" 2 2) ("NREVERSE" 1 1) ("NSET-DIFFERENCE" 2 NIL)
   ("NSET-EXCLUSIVE-OR" 2 NIL) ("NSTRING-CAPITALIZE" 1 NIL) ("NSTRING-DOWNCASE" 1 NIL)
   ("NSTRING-UPCASE" 1 NIL) ("NSUBLIS" 2 NIL) ("NSUBST" 3 NIL) ("NSUBST-IF" 3 NIL)
   ("NSUBST-IF-NOT" 3 NIL) ("NSUBSTITUTE" 3 NIL) ("NSUBSTITUTE-IF" 3 NIL)
   ("NSUBSTITUTE-IF-NOT" 3 NIL) ("NTH" 2 2) ("NTHCDR" 2 2) ("NULL" 1 1) ("NUMBERP" 1 1)
   ("NUMERATOR" 1 1) ("NUNION" 2 NIL) ("ODDP" 1 1) ("OPEN" 1 NIL) ("PACKAGE-ERROR-PACKAGE" 1 1)
   ("PACKAGE-NAME" 1 1) ("PACKAGE-NICKNAMES" 1 1) ("PACKAGE-SHADOWING-SYMBOLS" 1 1)
   ("PACKAGE-USE-LIST" 1 1) ("PACKAGE-USED-BY-LIST" 1 1) ("PACKAGEP" 1 1) ("PAIRLIS" 2 3)
   ("PARSE-INTEGER" 1 NIL) ("PARSE-NAMESTRING" 1 NIL) ("PATHNAME" 1 1) ("PATHNAME-DEVICE" 1 NIL)
   ("PATHNAME-DIRECTORY" 1 NIL) ("PATHNAME-HOST" 1 NIL) ("PATHNAME-MATCH-P" 2 2)
   ("PATHNAME-NAME" 1 NIL) ("PATHNAME-TYPE" 1 NIL) ("PATHNAME-VERSION" 1 1) ("PATHNAMEP" 1 1)
   ("PEEK-CHAR" 0 5) ("PHASE" 1 1) ("PLUSP" 1 1) ("POSITION" 2 NIL) ("POSITION-IF" 2 NIL)
   ("POSITION-IF-NOT" 2 NIL) ("PPRINT" 1 2) ("PPRINT-DISPATCH" 1 2) ("PPRINT-FILL" 2 4)
   ("PPRINT-INDENT" 2 3) ("PPRINT-LINEAR" 2 4) ("PPRINT-NEWLINE" 1 2) ("PPRINT-TAB" 3 4)
   ("PPRINT-TABULAR" 2 5) ("PRIN1" 1 2) ("PRIN1-TO-STRING" 1 1) ("PRINC" 1 2)
   ("PRINC-TO-STRING" 1 1) ("PRINT" 1 2) ("PRINT-NOT-READABLE-OBJECT" 1 1) ("PROBE-FILE" 1 1)
   ("PROCLAIM" 1 1) ("PROVIDE" 1 1) ("RANDOM" 1 2) ("RANDOM-STATE-P" 1 1) ("RASSOC" 2 NIL)
   ("RASSOC-IF" 2 NIL) ("RASSOC-IF-NOT" 2 NIL) ("RATIONAL" 1 1) ("RATIONALIZE" 1 1)
   ("RATIONALP" 1 1) ("READ" 0 4) ("READ-BYTE" 1 3) ("READ-CHAR" 0 4) ("READ-CHAR-NO-HANG" 0 4)
   ("READ-DELIMITED-LIST" 1 3) ("READ-FROM-STRING" 1 NIL) ("READ-LINE" 0 4)
   ("READ-PRESERVING-WHITESPACE" 0 4) ("READ-SEQUENCE" 2 NIL) ("READTABLE-CASE" 1 1)
   ("READTABLEP" 1 1) ("REALP" 1 1) ("REALPART" 1 1) ("REDUCE" 2 NIL) ("REM" 2 2) ("REMHASH" 2 2)
   ("REMOVE" 2 NIL) ("REMOVE-DUPLICATES" 1 NIL) ("REMOVE-IF" 2 NIL) ("REMOVE-IF-NOT" 2 NIL)
   ("REMPROP" 2 2) ("RENAME-FILE" 2 2) ("RENAME-PACKAGE" 2 3) ("REPLACE" 2 NIL) ("REQUIRE" 1 2)
   ("REST" 1 1) ("RESTART-NAME" 1 1) ("REVAPPEND" 2 2) ("REVERSE" 1 1) ("ROOM" 0 1) ("ROUND" 1 2)
   ("ROW-MAJOR-AREF" 2 2) ("RPLACA" 2 2) ("RPLACD" 2 2) ("SBIT" 1 NIL) ("SCALE-FLOAT" 2 2)
   ("SCHAR" 2 2) ("SEARCH" 2 NIL) ("SECOND" 1 1) ("SET" 2 2) ("SET-DIFFERENCE" 2 NIL)
   ("SET-DISPATCH-MACRO-CHARACTER" 3 4) ("SET-EXCLUSIVE-OR" 2 NIL) ("SET-MACRO-CHARACTER" 2 4)
   ("SET-PPRINT-DISPATCH" 2 4) ("SET-SYNTAX-FROM-CHAR" 2 4) ("SEVENTH" 1 1) ("SHADOW" 1 2)
   ("SHADOWING-IMPORT" 1 2) ("SHORT-SITE-NAME" 0 0) ("SIGNAL" 1 NIL) ("SIGNUM" 1 1)
   ("SIMPLE-BIT-VECTOR-P" 1 1) ("SIMPLE-CONDITION-FORMAT-ARGUMENTS" 1 1)
   ("SIMPLE-CONDITION-FORMAT-CONTROL" 1 1) ("SIMPLE-STRING-P" 1 1) ("SIMPLE-VECTOR-P" 1 1)
   ("SIN" 1 1) ("SINH" 1 1) ("SIXTH" 1 1) ("SLEEP" 1 1) ("SLOT-BOUNDP" 2 2) ("SLOT-EXISTS-P" 2 2)
   ("SLOT-MAKUNBOUND" 2 2) ("SLOT-VALUE" 2 2) ("SOFTWARE-TYPE" 0 0) ("SOFTWARE-VERSION" 0 0)
   ("SOME" 2 NIL) ("SORT" 2 NIL) ("SPECIAL-OPERATOR-P" 1 1) ("SQRT" 1 1) ("STABLE-SORT" 2 NIL)
   ("STANDARD-CHAR-P" 1 1) ("STORE-VALUE" 1 2) ("STREAM-ERROR-STREAM" 1 1)
   ("STREAM-EXTERNAL-FORMAT" 1 1) ("STREAMP" 1 1) ("STRING" 1 1) ("STRING-CAPITALIZE" 1 NIL)
   ("STRING-DOWNCASE" 1 NIL) ("STRING-EQUAL" 2 NIL) ("STRING-GREATERP" 2 NIL)
   ("STRING-LEFT-TRIM" 2 2) ("STRING-LESSP" 2 NIL) ("STRING-NOT-EQUAL" 2 NIL)
   ("STRING-NOT-GREATERP" 2 NIL) ("STRING-NOT-LESSP" 2 NIL) ("STRING-RIGHT-TRIM" 2 2)
   ("STRING-TRIM" 2 2) ("STRING-UPCASE" 1 NIL) ("STRING/=" 2 NIL) ("STRING<" 2 NIL)
   ("STRING<=" 2 NIL) ("STRING=" 2 NIL) ("STRING>" 2 NIL) ("STRING>=" 2 NIL) ("STRINGP" 1 1)
   ("SUBLIS" 2 NIL) ("SUBSEQ" 2 3) ("SUBSETP" 2 NIL) ("SUBST" 3 NIL) ("SUBST-IF" 3 NIL)
   ("SUBST-IF-NOT" 3 NIL) ("SUBSTITUTE" 3 NIL) ("SUBSTITUTE-IF" 3 NIL) ("SUBSTITUTE-IF-NOT" 3 NIL)
   ("SUBTYPEP" 2 3) ("SVREF" 2 2) ("SXHASH" 1 1) ("SYMBOL-FUNCTION" 1 1) ("SYMBOL-NAME" 1 1)
   ("SYMBOL-PACKAGE" 1 1) ("SYMBOL-PLIST" 1 1) ("SYMBOL-VALUE" 1 1) ("SYMBOLP" 1 1)
   ("SYNONYM-STREAM-SYMBOL" 1 1) ("TAILP" 2 2) ("TAN" 1 1) ("TANH" 1 1) ("TENTH" 1 1)
   ("TERPRI" 0 1) ("THIRD" 1 1) ("TRANSLATE-LOGICAL-PATHNAME" 1 NIL) ("TRANSLATE-PATHNAME" 3 NIL)
   ("TREE-EQUAL" 2 NIL) ("TRUENAME" 1 1) ("TRUNCATE" 1 2) ("TWO-WAY-STREAM-INPUT-STREAM" 1 1)
   ("TWO-WAY-STREAM-OUTPUT-STREAM" 1 1) ("TYPE-ERROR-DATUM" 1 1) ("TYPE-ERROR-EXPECTED-TYPE" 1 1)
   ("TYPE-OF" 1 1) ("TYPEP" 2 3) ("UNBOUND-SLOT-INSTANCE" 1 1) ("UNEXPORT" 1 2) ("UNINTERN" 1 2)
   ("UNION" 2 NIL) ("UNREAD-CHAR" 1 2) ("UNUSE-PACKAGE" 1 2) ("UPGRADED-ARRAY-ELEMENT-TYPE" 1 2)
   ("UPGRADED-COMPLEX-PART-TYPE" 1 2) ("UPPER-CASE-P" 1 1) ("USE-PACKAGE" 1 2) ("USE-VALUE" 1 2)
   ("USER-HOMEDIR-PATHNAME" 0 1) ("VALUES" 0 NIL) ("VALUES-LIST" 1 1) ("VECTOR" 0 NIL)
   ("VECTOR-POP" 1 1) ("VECTOR-PUSH" 2 2) ("VECTOR-PUSH-EXTEND" 2 3) ("VECTORP" 1 1) ("WARN" 1 NIL)
   ("WILD-PATHNAME-P" 1 2) ("WRITE" 1 NIL) ("WRITE-BYTE" 2 2) ("WRITE-CHAR" 1 2)
   ("WRITE-LINE" 1 NIL) ("WRITE-SEQUENCE" 2 NIL) ("WRITE-STRING" 1 NIL) ("WRITE-TO-STRING" 1 NIL)
   ("Y-OR-N-P" 0 NIL) ("YES-OR-NO-P" 0 NIL) ("ZEROP" 1 1)
                   ) h)
      (let ((sym (find-symbol (first row) "COMMON-LISP")))
        (when sym (setf (gethash sym h) (cons (second row) (third row))))))))

(defun %uv-note-arity (name form)
  "Warn when FORM calls the COMMON-LISP function NAME with too few or too many arguments."
  (let ((entry (and (symbolp name) (eq (symbol-package name) (find-package "COMMON-LISP"))
                    (consp form) (listp (cdr form))
                    (gethash name *cl-function-arities*))))
    (when (and entry
               (not (%lexical-function-p name))
               (not (member name *uv-local-functions* :test #'eq))
               (not (find-macro-expander name))
               (not (special-operator-name-p name)))
      (let ((n (length (cdr form))) (min (car entry)) (max (cdr entry)))
        (when (or (< n min) (and max (> n max)))
          (warn 'simple-warning
                :format-control "The function ~S is called with ~D argument~:P, but wants ~A."
                :format-arguments
                (list name n
                      (cond ((null max) (format nil "at least ~D" min))
                            ((= min max) (format nil "exactly ~D" min))
                            (t (format nil "between ~D and ~D" min max))))))))))

(defun %uv-note-function (name form)
  "Record NAME, called in FORM, when it is not a local operator, macro or
   function. The end of the compilation unit decides whether it stayed undefined."
  (%uv-note-arity name form)
  (when (and *compiler-note-undefined*
             (symbolp name) name (symbol-package name)
             (not (keywordp name))
             (not (special-operator-name-p name))
             (not (%lexical-function-p name))
             (not (member name *uv-local-functions* :test #'eq))
             (not (gethash name *compile-form-handlers*))
             (not (find-macro-expander name))
             (not (fboundp name)))
    (funcall *compiler-note-undefined* :function name form)))

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
                   (not (member sym (%lazy-table-get *uv-warned* site) :test #'eq))
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
              (%lazy-table-put *uv-warned* site (cons sym (%lazy-table-get *uv-warned* site)))
              (dolist (m msgs)
                (push (cons m sym) *uv-warnings*)))))))))

(defun %uv-note (sym env slot)
  "SYM is read (SLOT 0) or assigned (SLOT 1) where ENV is visible."
  (let ((e (assoc sym env :test #'eq)))
    (cond
      (e
       (case (cadr e)
         ;; An assignment from a closure counts as a use, as in SBCL.
         (:var (setf (svref (cddr e) (if (> *uv-depth* (svref (cddr e) 3)) 0 slot)) t))
         (:sm (when (< *uv-sm-depth* 20)
                (let ((*uv-sm-depth* (1+ *uv-sm-depth*)))
                  (%uv-walk (cddr e) env))))))
      ((and *compiler-note-undefined*
            (symbolp sym) sym (symbol-package sym)
            (not (keywordp sym))
            (not (constantp sym))
            (not (global-special-p sym))
            (not (boundp sym))
            (not (member sym *uv-defining* :test #'eq))
            (not (nth-value 1 (gethash sym *global-symbol-macros*)))
            ;; A SYMBOL-MACROLET around a top level form being compiled by itself
            ;; (a DEFUN in its body): the binding is in *SYMBOL-MACROS*, not in ENV.
            (not (assoc sym *symbol-macros* :test #'eq)))
       (funcall *compiler-note-undefined* :variable sym
                (or *uv-current-form* sym))))))

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
         (*lexical-operators* *lexical-operators*)
         (*uv-local-functions*
           (append (remove-if-not #'symbolp (mapcar #'car defs))
                   *uv-local-functions*)))
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
  (when (and *compiler-source-note* (consp form))
    (funcall *compiler-source-note* form))
  (let ((*uv-current-form* (if (consp form) form *uv-current-form*)))
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
         ((and (member head '(defvar defparameter defconstant)) (consp (cdr form))
               (symbolp (cadr form)) (cadr form))
          (let ((*uv-defining* (cons (cadr form) *uv-defining*))
                (expander (find-macro-expander head)))
            (let ((expanded (and expander (%walker-macroexpand form expander :none))))
              (if (and expanded (not (equal expanded form)))
                  (%uv-walk expanded env)
                  (%uv-walk-list (cdr form) env)))))
         (t
          (let ((expander (find-macro-expander head)))
            (let ((expanded (and expander (%walker-macroexpand form expander :none))))
              (if (and expanded (not (equal expanded form)))
                  (%uv-walk expanded env)
                  (progn
                    (%uv-note-function head form)
                    (%uv-walk-list (cdr form) env))))))))))))

(defun %compiler-warn-undefined (kind name)
  "Signal the warning for NAME, first used in the form whose source position is
   the current compiler context."
  (cond ((eq kind :variable)
         (warn 'style-warning :format-control "undefined variable: ~S"
               :format-arguments (list name)))
        ;; A global FTYPE proclamation says the function exists.
        ((gethash name *declaimed-ftype-names*) nil)
        ;; A function DEFCLASS (reader, writer, accessor), DEFGENERIC or DEFMETHOD
        ;; defines in this image or file, noted when the form was macroexpanded.
        ((or (gethash name *gf-call-names*)
             (gethash name *clos-accessor-readers*)
             (gethash name *clos-accessor-writers*))
         nil)
        (t
         (warn 'style-warning :format-control "undefined function: ~S"
               :format-arguments (list name)))))

(defun note-unused-variables (form)
  "Signal the unused-variable style warnings for FORM, a top-level form that
   is about to be compiled."
  (unless *uv-warned*
    (setf *uv-warned* (list nil)))
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
        (*instr-summaries* (list nil))
        (*owned-code* (list nil))
        (*ffv-free-cache* (list nil))
        (*uv-warned* nil)
        (*ltv-hoisted* (and *compile-file-mode* *current-module-id* (list '())))
        (*xref-caller* (%xref-top-level-caller expr))
        (*xref-top-level-conses* nil)
        (*xref-walked* nil)
        (*xref-edges* '())
        (*xref-uses* '()))
    (when (and (consp *xref-caller*) (eq (car *xref-caller*) :top-level-form))
      (setq *xref-top-level-conses* (%form-cons-set (fourth *xref-caller*))
            *xref-caller* (list :top-level-form (second *xref-caller*) (third *xref-caller*))))
    (let* ((body (compile-expr expr))
           (ltvs (%compile-hoisted-ltvs))
           ;; Code compiled to run now registers now: a registration in the
           ;; code would be the only code of a COMPILEd lambda's thunk.
           (note (and *xref-caller*
                      (if *compile-file-mode*
                          (xref-note-instrs)
                          (progn (xref-register-now) nil)))))
      `(,@(peephole-optimize (append note ltvs body))
        (:ret)))))

(defun %xref-top-level-caller (expr)
  "The xref name for the top level form EXPR, from *XREF-TOP-LEVEL-NAME*, which
   is cleared so that a compile nested in this one does not take it too."
  (let ((spec *xref-top-level-name*))
    (when spec
      (setq *xref-top-level-name* nil)
      (when (and *record-xref* (not *xref-sink*) (not *cross-compiling*) (consp spec))
        (case (car spec)
          (:top-level-form spec)
          (:compile (or (second spec)
                        (and (consp expr) (eq (car expr) 'lambda)
                             (let ((file (or *load-truename* *compile-file-truename*)))
                               (if file
                                   `(lambda ,(second expr) :in ,(namestring file))
                                   `(lambda ,(second expr))))))))))))

(defun %form-cons-set (form)
  (let ((set (make-hash-table :test #'eq)))
    (labels ((walk (x)
               (loop while (and (consp x) (not (gethash x set)))
                     do (setf (gethash x set) t)
                        (walk (car x))
                        (setq x (cdr x)))))
      (walk form))
    set))

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
        (*instr-summaries* (list nil))
        (*owned-code* (list nil))
        (*ffv-free-cache* (list nil))
        (*uv-warned* nil)
        ;; Code EVAL (or COMPILE) runs now, also when a macro calls it while
        ;; COMPILE-FILE compiles a top level form: its LOAD-TIME-VALUE forms
        ;; belong to it, not to the fasl being written.
        (*ltv-hoisted* nil)
        (*ltv-eval-now* t)
        (*xref-caller* nil)
        (*xref-top-level-conses* nil)
        (*xref-walked* nil))
    (%close-toplevel-segments (compile-expr expr))))
