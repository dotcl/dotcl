;;; cil-analysis.lisp — Free variable, mutation, and capture analysis
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

(defvar *macro-expand-depth-limit* 50)

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

;; Per-top-level-form EQ memo: lambda form -> list of free-variable CANDIDATE
;; names (structurally free w.r.t. the lambda's own params, collected under
;; *ffv-assume-bound* so the set is *locals*-independent = a pure function of the
;; form). find-free-vars-expr descends into every nested lambda and compile-lambda
;; re-runs find-free-vars per lambda, so without this an inner body is walked once
;; per enclosing lambda = O(depth^2) on nested closures. The real
;; local-bound-p filter is applied at each enclosing merge, not baked into the memo.
(defvar *ffv-free-cache* nil)

(defun find-free-vars-expr (expr bound free-ht)
  "Walk expr finding free variable references. Results accumulated in free-ht.
   Iterative worklist version — no recursion depth limit."
  (let ((worklist (list (cons expr (cons bound 0)))))
    (loop while worklist do
      (let* ((item (pop worklist))
             (e (car item)))
        (cond
          ;; Restore-scope sentinel: pop *macroexpand-scope* after a macrolet body.
          ;; The marker is a unique private object, so no source form collides.
          ((eq e *mscope-restore-sentinel*)
           (setf *macroexpand-scope* (cdr item)))
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
                    ;; cond: each clause is (test . body) and EVERY element is an
                    ;; evaluated expression. The generic walk below would treat a
                    ;; clause whose test is a symbol — e.g. (cond (start-anchored-p ...)) —
                    ;; as a function call (car = function name), dropping the test as a
                    ;; free-variable reference. When that name is also a captured local
                    ;; AND a global function (Lisp-2), the variable then isn't captured
                    ;; into the closure env and reads "Unbound variable" at run time.
                    ;; Scan all elements of each clause as expressions. (case/typecase
                    ;; differ: their clause cars are unevaluated keys — handled generically.)
                    ((and (symbolp head) (eq head 'cond))
                     (do-list-safe (clause (cdr e))
                       (when (consp clause)
                         (do-list-safe (sub clause)
                           (push (cons sub (cons bnd mdepth)) worklist)))))
                    ;; Lambda.
                    ((and (symbolp head) (eq head 'lambda))
                     (if *symbol-macros*
                         ;; Under an active symbol-macrolet, an enclosing binding
                         ;; (e.g. a lambda parameter) may shadow a symbol-macro of
                         ;; the same name (CLHS 3.4.2). The candidate memo drops the
                         ;; enclosing BND, so it would lose that shadow and re-expand
                         ;; the symbol-macro — infinitely if it is self-referential
                         ;; (regression: symbol-macro-param-shadow-nested-lambda).
                         ;; Fall back to the exact inline descent, which carries the
                         ;; full enclosing BND. Rare, so the O(depth^2) is acceptable.
                         (let* ((params (cadr e))
                                (lbody (cddr e))
                                (inner-bound (append (extract-param-names params) bnd)))
                           (let ((state :required)
                                 (progressive-bound bnd))
                             (dolist (p params)
                               (cond
                                 ((lambda-list-keyword-p p)
                                  (case p
                                    ((&rest &body) (setf state :rest))
                                    (&optional (setf state :optional))
                                    (&key (setf state :key))
                                    (&aux (setf state :aux))
                                    (t nil)))
                                 ((eq state :required)
                                  (push (var-name p) progressive-bound))
                                 ((member state '(:optional :key :aux))
                                  (when (and (consp p) (cadr p))
                                    (push (cons (cadr p) (cons progressive-bound mdepth)) worklist))
                                  (let ((name (if (consp p) (car p) p)))
                                    (when (consp name) (setf name (cadr name)))
                                    (push (var-name name) progressive-bound))
                                  (when (and (consp p) (caddr p))
                                    (push (var-name (caddr p)) progressive-bound)))
                                 ((eq state :rest)
                                  (push (var-name p) progressive-bound)))))
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
                          (when (and (not (bnd-member-p sym bnd))
                                     (not (gethash sym free-ht))
                                     (or *ffv-assume-bound* (local-bound-p sym)))
                            (setf (gethash sym free-ht) sym)))))
                    ;; Let/Let* introduces bindings
                    ((and (symbolp head) (member head '(let let*)))
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
                     (let* ((tag (cadr e))
                            (entry (assoc tag (cstate-go-tags))))
                       (when entry
                         (let* ((tb-var-name (second entry))
                                (tb-sym (intern tb-var-name :dotcl.cil-compiler)))
                           (when (and (not (member tb-var-name bnd :test #'string=))
                                      (not (gethash tb-sym free-ht))
                                      (local-bound-p tb-sym))
                             (setf (gethash tb-sym free-ht) tb-sym))))))
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
                         ((symbolp arg)
                          (if *ffv-assume-bound*
                              ;; Candidate collection: local-bound-p is T for all,
                              ;; so the mangled-vs-plain choice below can't be made
                              ;; yet. Emit BOTH names as candidates; the enclosing
                              ;; merge's real local-bound-p keeps the labels-fn
                              ;; (mangled) and/or the variable (plain) that is
                              ;; actually bound. Over-collecting is harmless — merge
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
                       (setf *macroexpand-scope* (cons macro-defs *macroexpand-scope*))
                       ;; Register macros immediately (same as compile-macrolet)
                       (dolist (def macro-defs)
                         (let* ((mname (car def))
                                (mparams (cadr def))
                                (mbody (cddr def)))
                           (setf (gethash mname *macros*)
                                 (eval (%macrolet-expander-form mparams mbody)))))
                       ;; Push body forms (LIFO: processed BEFORE restore sentinels)
                       (dolist (form mlbody)
                         (push (cons form (cons bnd mdepth)) worklist))))
                    ;; symbol-macrolet: extend *symbol-macros* during body walk
                    ((and (symbolp head) (eq head 'symbol-macrolet))
                     (let* ((sm-bindings (cadr e))
                            (sm-body (cddr e))
                            ;; A symbol-macro shadows an enclosing lexical variable of
                            ;; the same name in the body, so drop those names from the
                            ;; bound set — a reference to them is the symbol-macro, not
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
                    ((and (symbolp head) (member head '(flet labels)))
                     (let* ((fn-defs (cadr e))
                            (lbody (cddr e))
                            (fn-names (loop for fd in fn-defs
                                            for name = (car fd)
                                            when (symbolp name) collect (symbol-name name))))
                       ;; Function bodies see outer scope (flet) or same scope (labels)
                       ;; Labels fn-names are NOT added to fn body bound — they are captured
                       ;; as free vars via boxed variables in *locals*
                       (let ((fn-body-bound bnd))
                         (dolist (fd fn-defs)
                           (let* ((params (cadr fd))
                                  (fn-body (cddr fd))
                                  (inner-bound (append (extract-param-names params)
                                                       fn-body-bound)))
                             ;; Inline scan-lambda-list-defaults for fn params
                             (let ((state :required)
                                   (progressive-bound fn-body-bound))
                               (dolist (p params)
                                 (cond
                                   ((lambda-list-keyword-p p)
                                    (case p
                                      ((&rest &body) (setf state :rest))
                                      (&optional (setf state :optional))
                                      (&key (setf state :key))
                                      (&aux (setf state :aux))
                                      (t nil)))
                                   ((eq state :required)
                                    (push (var-name p) progressive-bound))
                                   ((member state '(:optional :key :aux))
                                    (when (and (consp p) (cadr p))
                                      (push (cons (cadr p) (cons progressive-bound mdepth)) worklist))
                                    (let ((name (if (consp p) (car p) p)))
                                      (when (consp name) (setf name (cadr name)))
                                      (push (var-name name) progressive-bound))
                                    (when (and (consp p) (caddr p))
                                      (push (var-name (caddr p)) progressive-bound)))
                                   ((eq state :rest)
                                    (push (var-name p) progressive-bound)))))
                             ;; Push fn body forms
                             (dolist (form fn-body)
                               (push (cons form (cons inner-bound mdepth)) worklist)))))
                       ;; Body sees all fn-names as bound
                       (let ((body-bound (append fn-names bnd)))
                         (dolist (form lbody)
                           (push (cons form (cons body-bound mdepth)) worklist)))))
                    ;; CLOS primitives — analyze sub-expressions normally
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
                                  (find-macro-expander head))
                         (let ((expander (find-macro-expander head)))
                           (setf expanded (handler-case (cached-macroexpand e expander)
                                            (error () nil)))))
                       (if expanded
                           (push (cons expanded (cons bnd (1+ mdepth))) worklist)
                           (progn
                             ;; Labels function mangled name capture.
                             ;; head=NIL is admitted too: a local function may
                             ;; be named NIL (ANSI LABELS.24) and its box must
                             ;; be captured — the local-bound-p check below
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
                             ;; as variable references — doing so would loop on symbol-macros
                             ;; from with-accessors (e.g. (disabled-commands #:OBJ) →
                             ;; push disabled-commands symbol → expand to (disabled-commands
                             ;; #:OBJ) → repeat). Lambda-car means immediate application —
                             ;; scan it. (setf sym) is a compound function name — skip it.
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
    (let ((state :required) (progressive-bound '()))
      (dolist (p params)
        (cond
          ((lambda-list-keyword-p p)
           (case p
             ((&rest &body) (setf state :rest))
             (&optional (setf state :optional))
             (&key (setf state :key))
             (&aux (setf state :aux))
             (t nil)))
          ((eq state :required)
           (push (var-name p) progressive-bound))
          ((member state '(:optional :key :aux))
           (when (and (consp p) (cadr p))
             (find-free-vars-expr (cadr p) progressive-bound free-ht))
           (let ((name (if (consp p) (car p) p)))
             (when (consp name) (setf name (cadr name)))
             (push (var-name name) progressive-bound))
           (when (and (consp p) (caddr p))
             (push (var-name (caddr p)) progressive-bound)))
          ((eq state :rest)
           (push (var-name p) progressive-bound)))))
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
;;; (mutated) AND pushed onto the worklist (so it can be capture-marked) — the old
;;; captured pass relied on its generic walk pushing the target; dropping that
;;; push would lose the capture mark and silently skip boxing (the
;;; mutation-loss class). Returns (values mutated-names captured-names).
;;;
;;; VAR-NAMES may be the sentinel :ALL, meaning "every referenced symbol is a
;;; capture candidate" — used by %boundary-mut-ref to collect the full mutated
;;; and referenced sets of a nested lambda once, so the enclosing walks reuse
;;; them instead of re-descending (O(depth^2) -> O(depth) on nested
;;; closures). Sound to memoize because this walk never consults *locals*:
;;; captures key off explicit references and mutations off setq/place targets,
;;; both pure functions of the form within a top-level compile (unlike the
;;; free-var walk, whose local-bound-p filter is *locals*-dependent).
(defvar *bmr-cache* nil
  "Per-top-level-form EQ memo: lambda form -> (mutated-names . ref-names).")

(defun find-mutated-and-captured-vars-expr (expr var-names mutated-ht captured-ht inside-lambda)
  (let ((worklist (list (cons expr (cons inside-lambda 0)))))
    (loop while worklist do
      (let* ((item (pop worklist))
             (e (car item))
             (in-lambda (cadr item))
             (mdepth (cddr item)))
        (cond
          ((eq e *mscope-restore-sentinel*)
           (setf *macroexpand-scope* in-lambda))
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
          ;; (fable pitfall #2 — do both actions, not either).
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
                                     (expanded (handler-case
                                                   (cached-macroexpand single-form expander)
                                                 (error () nil))))
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
               ((and (symbolp head) (eq head 'lambda))
                (multiple-value-bind (mut ref) (%boundary-mut-ref e)
                  (dolist (n mut) (setf (gethash n mutated-ht) t))
                  (dolist (n ref)
                    (when (or (eq var-names :all)
                              (member n var-names :test #'string=))
                      (setf (gethash n captured-ht) t)))))
               ((and (symbolp head) (member head '(let let*)))
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
               ((and (symbolp head) (or (eq head 'flet) (eq head 'labels)))
                (dolist (fdef (cadr e))
                  ;; Walk only the initializer forms of the lambda list (the
                  ;; default-value / supplied-p expressions of &optional/&key/&aux),
                  ;; not the raw lambda list. Pushing the lambda list itself as a
                  ;; form macroexpands a param whose name happens to be a macro
                  ;; (e.g. a required param named INST, which is a macro under
                  ;; SBCL's assembler), firing that macro's compile-time side
                  ;; effects. Param names are binding occurrences, not code.
                  (dolist (p (cadr fdef))
                    (when (and (consp p) (cadr p))
                      (push (cons (cadr p) (cons t mdepth)) worklist)))
                  (dolist (form (cddr fdef))
                    (push (cons form (cons t mdepth)) worklist)))
                (dolist (form (cddr e))
                  (push (cons form (cons in-lambda mdepth)) worklist)))
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
                  (setf *macroexpand-scope* (cons macro-defs *macroexpand-scope*))
                  (dolist (def macro-defs)
                    (let* ((mname (car def))
                           (mparams (cadr def))
                           (mbody (cddr def)))
                      (setf (gethash mname *macros*)
                            (eval (%macrolet-expander-form mparams mbody)))))
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
                             (find-macro-expander head))
                    (let ((expander (find-macro-expander head)))
                      (setf expanded (handler-case (cached-macroexpand e expander)
                                       (error () nil)))))
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
;;; new op that carries local KEYs — whether as a direct operand or embedded in a
;;; nested operand list — is taught to every pass by editing only this section.
;;; Authoritative op set (CilAssembler.Emit.cs): :declare-local, :ldloc, :stloc,
;;; :dotnet-call-direct-locals (RECV + ARG list). Background: a missed
;;; nested-operand local once let slot-sharing orphan a merged local into an
;;; "Undeclared local" at assembly time, because each pass scanned for top-level
;;; :ldloc/:stloc independently and none knew the new op carried locals.
;;;
;;; (peephole-optimize is intentionally NOT a client: it pattern-matches fixed
;;;  adjacent op sequences rather than enumerating locals, so an unknown op simply
;;;  fails to match and passes through untouched — safe by construction.)

(defun instr-local-reads (instr)
  "Local KEYs INSTR reads, including locals carried in nested operand lists."
  (when (consp instr)
    (case (car instr)
      (:ldloc (list (cadr instr)))
      ;; The debug frame stores (:frame-set NAME KEY) and its box / native-rep
      ;; variants all read KEY.
      ((:frame-set :frame-set-box :frame-set-long :frame-set-double :frame-set-single)
       (list (caddr instr)))
      ;; (:dotnet-call-direct-locals TYPE METHOD RECV (ARG...) (PARAM...))
      ;; RECV and each ARG are locals read by the call.
      (:dotnet-call-direct-locals (cons (nth 3 instr) (nth 4 instr)))
      (t nil))))

(defun instr-local-writes (instr)
  "Local KEYs INSTR writes."
  (when (consp instr)
    (case (car instr)
      (:stloc (list (cadr instr)))
      (t nil))))

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
  (if (not (consp instr))
      instr
      (flet ((rn (k) (or (gethash k rename) k)))
        (case (car instr)
          (:declare-local `(:declare-local ,(rn (cadr instr)) ,(caddr instr)))
          (:ldloc `(:ldloc ,(rn (cadr instr))))
          (:stloc `(:stloc ,(rn (cadr instr))))
          ((:frame-set :frame-set-box :frame-set-long :frame-set-double :frame-set-single)
           `(,(car instr) ,(cadr instr) ,(rn (caddr instr))))
          (:dotnet-call-direct-locals
           `(:dotnet-call-direct-locals
             ,(nth 1 instr) ,(nth 2 instr)
             ,(rn (nth 3 instr))
             ,(mapcar #'rn (nth 4 instr))
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
      (dolist (k (instr-local-writes instr)) (incf (gethash k stloc-count 0)))
      (dolist (k (instr-local-reads instr))  (incf (gethash k ldloc-count 0))))
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
;;; Slot sharing: merge LispObject locals with disjoint flat ranges
;;; ============================================================

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
                                          ; discard it — Fixnum.Make is pure, so
                                          ; pop the raw long instead. (Int64-slot
                                          ; setq in statement position; composes
                                          ; with P2 to a bare native store.)
     P6  (:newobj \"DoubleFloat\") (:pop) ->  (:pop)    ; float sibling of P5: the
     P6  (:newobj \"SingleFloat\") (:pop) ->  (:pop)    ; DoubleFloat/SingleFloat
     P6  (:newobj \"LispDecimal\") (:pop) ->  (:pop)   ; and the decimal slot's box
                                          ; ctor is pure (value + alloc counter),
                                          ; so boxing a discarded native float
                                          ; store result is dead — pop the raw r8
                                          ; instead. (Float-array setf in
                                          ; statement position.)
   (P3+P4 compose across the fixpoint to delete the dead nil/unwrap/pop preamble
    that codegen emits at the top of every TCO loop body.)

   Matches only strictly-adjacent instructions. A :label (the only branch
   target form in SIL) between instructions breaks adjacency in the list, so
   it naturally blocks a match — no control-flow analysis needed, and the
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
              ;; bracket cancels — stack-equivalent to a bare store. (Assignment
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
              ;; P4: push a Nil constant then immediately discard it — dead.
              ((and (consp i1) (eq (car i1) :ldsfld) (equal (cadr i1) "Nil.Instance")
                    (consp i2) (eq (car i2) :pop))
               (setf changed t)
               (setf cur (cddr cur)))
              ;; P5: box a raw long only to discard it. Fixnum.Make is pure —
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
              ;; P7: UnwrapMv of a freshly-boxed float is identity — a DoubleFloat
              ;; / SingleFloat is never an MvReturn. Codegen wraps a setf result
              ;; in UnwrapMv (twice, in a statement-position dotimes body); drop it
              ;; so the newobj becomes adjacent to the pop and P6 can then delete
              ;; the whole dead box. (Fixnum.Make's surviving box is invisible in
              ;; alloc profiles thanks to the small-int cache; a float box is not.)
              ((and (consp i1) (eq (car i1) :newobj)
                    (member (cadr i1) '("DoubleFloat" "SingleFloat" "LispDecimal") :test #'equal)
                    (consp i2) (eq (car i2) :call) (equal (cadr i2) "Runtime.UnwrapMv"))
               (setf changed t)
               (push i1 out)
               (setf cur (cddr cur)))
              (t
               (push i1 out)
               (setf cur (cdr cur))))))
        (setf instrs (nreverse out))))
    instrs))

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
       instrs
       (%merge-disjoint-locals instrs))))

(defun %merge-disjoint-locals (instrs)
  "Linear-scan slot sharing: merge LispObject locals whose flat live ranges
   do not overlap. When last-use(K1) < first-def(K2) in flat instruction order,
   K2 can reuse K1's slot. Reduces local variable count across exclusive cond arms.
   Applied once per function body. Does NOT recurse into nested :body lists.
   Skipped entirely when any backward branch is present (loops, TCO)."
  ;; Pre-scan: bail out if any backward branch is present.
  ;; A backward branch targets a label whose position <= the branch's own position.
  ;; :leave counts: a tagbody that elides its GoException try/catch (compile-tagbody
  ;; no-catch path) uses (:leave loop-label) for its backward loop edge and has no
  ;; trailing (:br loop-label), so :leave is the only backward-branch signal. Missing
  ;; it lets the linear scan treat a loop as straight-line code and wrongly merge
  ;; live-overlapping slots. Forward :leave (block / handler-case exit) has target >
  ;; position and does not trip this.
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
        (incf fwd-pos))))
  (let ((first-pos  (make-hash-table :test #'equal))
        (last-pos   (make-hash-table :test #'equal))
        (local-type (make-hash-table :test #'equal))
        (pos 0))
    ;; Pass 1: collect types and compute [first-pos, last-pos] for each key.
    ;; instr-local-refs returns every local a key reads/writes — including those
    ;; embedded in nested operand lists (:dotnet-call-direct-locals) — so their
    ;; live ranges extend to the using op and the slot-share scan won't merge
    ;; another local over a still-live nested reference.
    (dolist (instr instrs)
      (let ((decl (instr-declared-local instr)))
        (when decl (setf (gethash (car decl) local-type) (cdr decl))))
      (dolist (key (instr-local-refs instr))
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
        (when (zerop (hash-table-count rename))
          (return-from %merge-disjoint-locals instrs))
        ;; Pass 2: apply RENAME to every local (central rewriter handles stloc/
        ;; ldloc/declare-local and nested :dotnet-call-direct-locals locals), then
        ;; drop duplicate :declare-local entries that collapsed onto a shared slot.
        (let ((seen-declare (make-hash-table :test #'equal)))
          (remove nil
                  (mapcar (lambda (instr)
                            (let* ((new (rewrite-instr-locals instr rename))
                                   (decl (instr-declared-local new)))
                              (if decl
                                  (if (gethash (car decl) seen-declare)
                                      nil
                                      (progn (setf (gethash (car decl) seen-declare) t)
                                             new))
                                  new)))
                          instrs)))))))

;;; ============================================================
;;; Top-level compilation
;;; ============================================================

(defun compile-toplevel (expr)
  "Compile a top-level expression. Returns instruction list."
  (let ((*cstate* (cstate-with *cstate*
                               +cs-locals+ '() +cs-block-tags+ '() +cs-go-tags+ '()
                               +cs-boxed-vars+ '() +cs-local-functions+ '()))
        (*var-counter* 0)
        (*label-counter* 0)
        (*specials* '())
        (*at-toplevel* t)
        (*macroexpand-scope* '())
        (*macroexpand-cache* (make-hash-table :test #'eq))
        (*bmr-cache* (make-hash-table :test #'eq))
        (*ffv-free-cache* (make-hash-table :test #'eq)))
    `(,@(compile-expr expr)
      (:ret))))

(defun compile-toplevel-eval (expr)
  "Compile a top-level expression for EVAL.
   Like compile-toplevel but preserves MvReturn at the tail so EVAL's
   caller can observe the form's multiple values."
  (let ((*cstate* (cstate-with *cstate*
                               +cs-locals+ '() +cs-block-tags+ '() +cs-go-tags+ '()
                               +cs-boxed-vars+ '() +cs-local-functions+ '()))
        (*var-counter* 0)
        (*label-counter* 0)
        (*specials* '())
        (*at-toplevel* t)
        (*in-tail-position* t)
        (*macroexpand-scope* '())
        (*macroexpand-cache* (make-hash-table :test #'eq))
        (*bmr-cache* (make-hash-table :test #'eq))
        (*ffv-free-cache* (make-hash-table :test #'eq)))
    `(,@(compile-expr expr)
      (:ret))))
