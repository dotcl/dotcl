;;; cil-compiler.lisp — Lisp CIL compiler (A2 instruction list architecture)
;;;
;;; Pure-functional compiler: S-expression in → instruction list out.
;;; Each compile-* function returns a flat list of CIL instructions.
;;; No .NET API calls. The C# CilAssembler walks the list and calls ILGenerator.

(defpackage :dotcl.cil-compiler
  (:use :cl)
  (:export #:compile-toplevel #:compile-toplevel-eval
           #:*cross-compiling* #:*compile-file-mode*))

;; %INLINE-CS-SPLICED is the dispatch symbol for the dotcl-cs:inline-cs
;; macro (#122 Phase 2; D903 で contrib 統合). It needs to be reachable
;; via Startup.Sym (which
;; checks CL → DOTCL-INTERNAL → cross-package bridge) so that the
;; runtime LOAD-SYM resolution and the cross-compiled handler
;; registration converge on the same Symbol instance. CL is locked,
;; so we use DOTCL-INTERNAL (also universally searched by Startup.Sym).
#-dotcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "DOTCL-INTERNAL")
    (make-package "DOTCL-INTERNAL"))
  (intern "%INLINE-CS-SPLICED" "DOTCL-INTERNAL"))
(in-package :dotcl.cil-compiler)

;;; ============================================================
;;; Compilation context (dynamic variables)
;;; ============================================================

(defvar *cross-compiling* nil
  "T when running in SBCL as cross-compiler; NIL in self-hosted mode.")

(defvar *compile-file-mode* nil
  "T when compiling via compile-file. Controls eval-when behavior per CLHS 3.2.3.1:
   :compile-toplevel → eval at compile time, :load-toplevel → emit CIL for load.")

(defvar *locals* '()
  "Alist of (symbol . local-key). local-key is a keyword like :v1.")

(defvar *var-counter* 0)

(defvar *label-counter* 0)

(defvar *block-tags* '()
  "Alist of (block-name . (tag-key . result-key)).
   tag-key is a keyword for the tag object, result-key for the result local.")

(defvar *go-tags* '()
  "Alist of (tag-symbol . (tagbody-id-key . integer-label)).")

(defvar *specials* '()
  "List of symbols known to be special (both global and locally declared).")

(defvar *global-specials* '()
  "List of symbols that are GLOBALLY special (via defvar/defparameter/proclaim).
   Used to determine binding classification: only global specials force nested let bindings
   to be dynamic. Locally-declared specials (declare (special x)) only affect references.")

(defvar *boxed-vars* '()
  "Set of variable names (symbols) that need boxing (mutated + captured).")


(defvar *macros* (make-hash-table :test #'eq)
  "Global macro table: symbol → macro-expander-function.
   Keyed by symbol identity (not name string).
   Not reset by compile-toplevel (defmacro has global effect).")

(defvar *function-return-types* (make-hash-table :test #'eq)
  "symbol → return-type (currently only 'fixnum is honored).
   Populated by (declaim (ftype (function (...) ret) name...)).
   Used by fixnum-typed-p to recognize `(name args...)` as statically
   fixnum-typed, which then enables native int64 paths in arithmetic.")

(defvar *local-functions* '()
  "Alist of (name-string local-key boxed-p).
   For flet: local-key is a keyword for a LispObject local holding the function.
   For labels: boxed-p is T, local-key is a keyword for a LispObject[] box.")

(defvar *symbol-macros* '()
  "Alist of (symbol . expansion) for symbol-macrolet. Dynamically scoped.")

(defvar *global-symbol-macros* (make-hash-table :test #'eq)
  "Hash table of global symbol macros defined by DEFINE-SYMBOL-MACRO.")

(defvar *ltv-counter* 0
  "Counter for load-time-value slot IDs. Incremented for each load-time-value form.")

(defvar *at-toplevel* nil
  "T when compiling a form at top level (per CLHS 3.2.3.1).
   Only progn, eval-when, locally, macrolet, and symbol-macrolet preserve
   top-level-ness for their body forms.  All other forms set this to NIL.")

(defvar *compile-was-toplevel* nil
  "Captures *at-toplevel* at the start of compile-form, before it is reset to NIL.
   Handlers in *compile-form-handlers* that need to propagate top-level-ness
   (progn, locally, macrolet, symbol-macrolet, eval-when per CLHS 3.2.3.1)
   bind *at-toplevel* to this value.")

(defvar *compile-form-handlers* (make-hash-table :test #'eq :size 400)
  "Hash table: operator symbol → (lambda (expr) ...) handler.
   Populated at the bottom of cil-forms.lisp after all compile-* helpers are defined.
   Provides O(1) dispatch for ~250+ common operators in compile-form.")

(defvar *macroexpand-cache* nil
  "Hash table (form → expansion) for memoizing macro expansions within one
   top-level compile.  Keyed by eq (cons-cell identity) so that the same
   source form object is never expanded twice even when it appears in both
   the analysis pass and the code-gen pass.  NIL outside a compile-toplevel
   call, which disables caching (eval-time macro calls are not cached).")

(defun cached-macroexpand (form expander)
  "Expand FORM using EXPANDER, memoizing the result in *macroexpand-cache*.
   If the cache is NIL (outside a compile-toplevel) falls through to a plain
   funcall so that eval / macrolet restore paths are unaffected."
  (if *macroexpand-cache*
      (let ((cached (gethash form *macroexpand-cache* :miss)))
        (if (eq cached :miss)
            (let ((result (funcall expander form)))
              (setf (gethash form *macroexpand-cache*) result)
              result)
            cached))
      (funcall expander form)))

;;; TCO (Tail Call Optimization) state — declared here so all compiler files see them as special.
;;; (cil-forms.lisp re-declares them with documentation strings; that is fine in CL.)
(defvar *tco-self-name* nil)
(defvar *tco-loop-label* nil)
(defvar *tco-param-entries* nil)
(defvar *in-tail-position* nil)
(defvar *in-finally-block* nil)
(defvar *self-fn-local* nil)

;;; ============================================================
;;; Utilities
;;; ============================================================

(defun gen-local (&optional (prefix "V"))
  "Generate a unique local variable symbol in the compiler package."
  (intern (format nil "~a_~d" prefix (incf *var-counter*)) "DOTCL.CIL-COMPILER"))

(defun gen-label (&optional (prefix "L"))
  "Generate a unique label symbol in the compiler package."
  (intern (format nil "~a_~d" prefix (incf *label-counter*)) "DOTCL.CIL-COMPILER"))

(defun compile-sym-lookup (sym)
  "Generate CIL instructions to load a Symbol object for SYM.
Uses LOAD-SYM instructions to resolve symbols at assembly time
(constant pool), avoiding runtime dictionary lookups."
  (cond ((keywordp sym)
         `((:load-sym-keyword ,(symbol-name sym))))
        ;; Uninterned symbols (gensyms with nil package): use load-const to preserve identity
        ((null (symbol-package sym))
         `((:load-const ,sym)))
        ((and (not *cross-compiling*)
              (not (string= (package-name (symbol-package sym)) "COMMON-LISP")))
         `((:load-sym-pkg ,(symbol-name sym) ,(package-name (symbol-package sym)))))
        (t `((:load-sym ,(symbol-name sym))))))

(defun %runtime-special-p (sym)
  "Check if the runtime marks SYM as special (via IsSpecial flag).
   Returns NIL during cross-compilation or if %SYMBOL-SPECIAL-P is unavailable."
  (when (not *cross-compiling*)
    (let ((fn (multiple-value-bind (s status)
                 (find-symbol "%SYMBOL-SPECIAL-P" "DOTCL-INTERNAL")
               (when (and s status (fboundp s))
                 (symbol-function s)))))
      (when fn (funcall fn sym)))))

(defun special-var-p (sym)
  "Check if a symbol is a special (dynamic) variable (includes locally declared specials)."
  (unless (symbolp sym) (return-from special-var-p nil))
  (or (member sym *specials*)
      (member (symbol-name sym) *specials* :key #'symbol-name :test #'string=)
      (%runtime-special-p sym)))

(defun global-special-p (sym)
  "Check if a symbol is GLOBALLY special (via defvar/defparameter/proclaim).
   Used for binding classification: only globally special vars force nested let bindings dynamic."
  (unless (symbolp sym) (return-from global-special-p nil))
  (or (member sym *global-specials*)
      (member (symbol-name sym) *global-specials* :key #'symbol-name :test #'string=)
      (%runtime-special-p sym)))

(defun lookup-local (sym)
  "Look up a variable in *locals* by symbol identity first, then name fallback."
  ;; Use eq for identity first.
  ;; Fall back to string= for both interned (cross-package CL/CL-USER compat)
  ;; and uninterned gensyms (closure env-locals use interned version of gensym name).
  (or (cdr (assoc sym *locals* :test #'eq))
      (let ((name (symbol-name sym)))
        (cdr (assoc name *locals*
                    :key (lambda (k) (if (and (symbolp k) (symbol-package k))
                                         (symbol-name k) nil))
                    :test #'string=)))))

(defun local-bound-p (sym)
  "Check if symbol is bound in *locals* or has a boxed entry in *local-functions*."
  (or (assoc sym *locals* :test #'eq)
      (let ((name (symbol-name sym)))
        (assoc name *locals*
               :key (lambda (k) (if (and (symbolp k) (symbol-package k))
                                    (symbol-name k) nil))
               :test #'string=))
      ;; Also check *local-functions* for boxed labels functions
      ;; (supports closure capture of labels functions whose name clashes with a variable)
      (let ((name (symbol-name sym)))
        (find name *local-functions*
              :key #'first :test #'string=))))

(defun boxed-var-p (sym)
  "Check if a variable needs boxing (by name, cross-package safe)."
  (member (symbol-name sym) *boxed-vars*
          :key (lambda (x) (if (symbolp x) (symbol-name x) x))
          :test #'string=))

(defun mangle-name (symbol)
  "Convert a Lisp symbol/name to a display string (for defmethod names).
   Handles (setf foo), (cas foo), string names, and (\"c-name\" lisp-name) pairs."
  (cond
    ;; String: use directly (e.g. C name strings from define-alien-routine)
    ((stringp symbol) (string-upcase symbol))
    ((and (consp symbol) (eq (car symbol) 'setf))
     (format nil "(SETF ~A)" (string-upcase (symbol-name (cadr symbol)))))
    ((and (consp symbol) (stringp (car symbol)))
     ;; ("c-name" lisp-name) pair from define-alien-routine → use the Lisp name
     (if (symbolp (cadr symbol))
         (string-upcase (symbol-name (cadr symbol)))
         (string-upcase (car symbol))))
    ((consp symbol)
     ;; Generic compound function name: (OP NAME) e.g. (cas car)
     (format nil "(~A~{ ~A~})"
             (if (symbolp (car symbol))
                 (string-upcase (symbol-name (car symbol)))
                 (prin1-to-string (car symbol)))
             (mapcar (lambda (s) (if (symbolp s) (string-upcase (symbol-name s))
                                     (prin1-to-string s)))
                     (cdr symbol))))
    (t (string-upcase (symbol-name symbol)))))

(defun parse-lambda-list (params)
  "Parse lambda list → (values required optional key rest-param aux allow-other-keys-p has-key-p).
   &body is treated as &rest. rest-param is NIL if no variadic.
   optional: ((name default-form) ...)
   key: ((keyword-name var-name default-form) ...)
   aux: ((name init-form) ...)
   allow-other-keys-p: T if &allow-other-keys was present
   has-key-p: T if &key was present (even if no key params listed)"
  (let ((required '()) (optional '()) (key '()) (rest-param nil) (aux '())
        (allow-other-keys-p nil) (has-key-p nil)
        (state :required))
    (dolist (p params)
      (cond
        ((member p '(&rest &body)) (setf state :rest))
        ((eq p '&optional) (setf state :optional))
        ((eq p '&key) (setf state :key) (setf has-key-p t))
        ((eq p '&allow-other-keys) (setf allow-other-keys-p t))
        ((eq p '&aux) (setf state :aux))
        ((eq state :aux)
         (if (consp p)
             (push (list (car p) (cadr p)) aux)
             (push (list p nil) aux)))
        ((eq state :required) (push p required))
        ((eq state :optional)
         (if (consp p)
             (push (list (car p) (cadr p) (caddr p)) optional)
             (push (list p nil nil) optional)))
        ((eq state :key)
         (if (consp p)
             (let* ((spec (car p))
                    (explicit-p (consp spec))
                    (var-name (if explicit-p (cadr spec) spec))
                    (key-sym (if explicit-p (car spec) spec))
                    (key-name (symbol-name key-sym))
                    (key-pkg (when explicit-p
                               (let ((pkg (symbol-package key-sym)))
                                 (if pkg (package-name pkg) ""))))
                    (default (cadr p))
                    (supplied-p (caddr p)))
               (push (list key-name var-name default supplied-p key-pkg) key))
             (push (list (symbol-name p) p nil nil nil) key)))
        ((eq state :rest) (setf rest-param p) (setf state :done))))
    (values (nreverse required) (nreverse optional) (nreverse key) rest-param (nreverse aux) allow-other-keys-p has-key-p)))

(defun lambda-list-keyword-p (sym)
  "Check if symbol is a lambda list keyword."
  (and (symbolp sym)
       (member sym '(&rest &body &optional &key &allow-other-keys &aux &whole &environment))))

(defun extract-param-names (params)
  "Extract variable names (as strings) from a lambda list, handling &optional/&key specs."
  (let ((names '()) (state :required))
    (dolist (p params)
      (cond
        ((lambda-list-keyword-p p)
         (case p
           ((&rest &body) (setf state :rest))
           (&optional (setf state :optional))
           (&key (setf state :key))
           (&aux (setf state :aux))
           (t nil)))
        ((eq state :done) nil)
        ((eq state :required)
         (push (symbol-name p) names))
        ((eq state :rest)
         (push (symbol-name p) names) (setf state :done))
        ((eq state :optional)
         (push (symbol-name (if (consp p) (car p) p)) names)
         ;; supplied-p variable (third element of optional spec)
         (when (and (consp p) (caddr p))
           (push (symbol-name (caddr p)) names)))
        ((eq state :key)
         (if (consp p)
             (let ((spec (car p)))
               (push (symbol-name (if (consp spec) (cadr spec) spec)) names)
               ;; supplied-p variable (third element of key spec)
               (when (caddr p)
                 (push (symbol-name (caddr p)) names)))
             (push (symbol-name p) names)))
        ((eq state :aux)
         (push (symbol-name (if (consp p) (car p) p)) names))))
    (nreverse names)))

(defun scan-lambda-list-defaults (params bound free-ht)
  "Scan default value forms in &optional/&key parameters for free variable references.
   Per CLHS 3.4.1.5, each init-form may only refer to params to its left,
   so we progressively add param names to bound as we process each default."
  (let ((state :required)
        (progressive-bound bound))
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
         (push (symbol-name p) progressive-bound))
        ((member state '(:optional :key :aux))
         (when (consp p)
           (let ((default-form (cadr p)))
             (when default-form
               (find-free-vars-expr default-form progressive-bound free-ht))))
         ;; After evaluating default, this param is now visible to subsequent defaults
         (let ((name (if (consp p) (car p) p)))
           (when (consp name) (setf name (cadr name))) ;; &key ((keyword var) default)
           (push (symbol-name name) progressive-bound))
         ;; Also add supplied-p var if present
         (when (and (consp p) (caddr p))
           (push (symbol-name (caddr p)) progressive-bound)))
        ((eq state :rest)
         (push (symbol-name p) progressive-bound))))))

;;; ============================================================
;;; Instruction builders (thin wrappers for readability)
;;; ============================================================

(defun emit-fixnum (n)
  `((:ldc-i8 ,n) (:call "Fixnum.Make")))

(defun emit-nil ()
  '((:ldsfld "Nil.Instance")))

(defun emit-t ()
  '((:ldsfld "T.Instance")))

;;; ============================================================
;;; Quote / literal compilation
;;; ============================================================

(defun compile-quoted (obj)
  "Compile a quoted datum to instruction list."
  (cond
    ((null obj) (emit-nil))
    ((eq obj t) (emit-t))
    ((integerp obj)
     (if (typep obj '(integer #.(- (expt 2 63)) #.(1- (expt 2 63))))
         (emit-fixnum obj)
         `((:load-const ,obj))))
    ((and (stringp obj) (= (array-rank obj) 1))
     (if *cross-compiling*
         `((:ldstr ,obj) (:newobj "LispString"))
         `((:load-const ,obj))))
    ((characterp obj)
     `((:ldc-i4 ,(char-code obj)) (:call "LispChar.Make")))
    ((symbolp obj)
     (if *cross-compiling*
         (compile-sym-lookup obj)
         `((:load-const ,obj))))
    ((consp obj)
     ;; At runtime (not cross-compiling), use load-const to preserve EQL identity
     ;; of cons objects (important for CASE with dynamic list keys).
     ;; During cross-compile, use MakeCons to avoid sharing mutable constants.
     (if *cross-compiling*
         `(,@(compile-quoted (car obj))
           ,@(compile-quoted (cdr obj))
           (:call "Runtime.MakeCons"))
         `((:load-const ,obj))))
    ((typep obj 'single-float)
     `((:load-const ,obj)))
    ((typep obj 'double-float)
     `((:load-const ,obj)))
    ((typep obj 'ratio)
     `((:load-const ,obj)))
    ((bit-vector-p obj)
     ;; Preserve bit-vector element type via load-const (newobj "LispVector" would lose BIT type)
     `((:load-const ,obj)))
    ((vectorp obj)
     (if *cross-compiling*
         (if (simple-vector-p obj)
             ;; 1D simple vector: reconstruct element-by-element during cross-compile
             `((:ldc-i4 ,(length obj))
               (:newarr "LispObject")
               ,@(loop for i from 0 below (length obj)
                       append `((:dup) (:ldc-i4 ,i) ,@(compile-quoted (aref obj i)) (:stelem-ref)))
               (:newobj "LispVector"))
             ;; Multi-dimensional or specialized vector: use load-const
             `((:load-const ,obj)))
         ;; At runtime, use load-const to preserve EQL identity
         `((:load-const ,obj))))
    ((pathnamep obj)
     (if *cross-compiling*
         ;; Emit explicit make-pathname to preserve all components (esp. version)
         ;; through text serialization in compile-file output
         (let ((parts (list (pathname-host obj) (pathname-device obj)
                            (pathname-directory obj) (pathname-name obj)
                            (pathname-type obj) (pathname-version obj))))
           `(,@(compile-args-array (mapcar (lambda (p) `(quote ,p)) parts))
             (:call "Runtime.MakePathnameFromParts")))
         ;; At runtime, use load-const to preserve EQL identity
         `((:load-const ,obj))))
    ((complexp obj)
     `((:load-const ,obj)))
    ;; General fallback for runtime objects (packages, hash-tables, functions, etc.)
    (t `((:load-const ,obj)))))

;;; ============================================================
;;; Variable reference
;;; ============================================================

(defun lookup-symbol-macro (sym)
  "Return the expansion form if SYM is a symbol-macro, else NIL."
  (or (cdr (assoc sym *symbol-macros* :test #'eq))
      (and (symbol-package sym)
           (cdr (assoc (symbol-name sym) *symbol-macros*
                       :key (lambda (k) (if (and (symbolp k) (symbol-package k))
                                            (symbol-name k) nil))
                       :test #'string=)))
      ;; Check global symbol macros from DEFINE-SYMBOL-MACRO
      (multiple-value-bind (val found) (gethash sym *global-symbol-macros*)
        (if found val nil))))

(defun compile-var-ref (sym)
  "Compile a variable reference."
  ;; Check symbol-macro first
  (let ((sm-exp (lookup-symbol-macro sym)))
    (if sm-exp
        (compile-expr sm-exp)
        ;; Check lexical binding BEFORE special — allows inner let to shadow outer declare
        (let ((key (lookup-local sym)))
          (if key
              (if (boxed-var-p sym)
                  ;; Boxed: load box, then element 0
                  `((:ldloc ,key) (:ldc-i4 0) (:ldelem-ref))
                  ;; Native Int64 param: box back to Fixnum for LispObject context (#130)
                  (if (and (boundp '*long-locals*) *long-locals*
                           (member (symbol-name sym) *long-locals* :test #'string=))
                      `((:ldloc ,key) (:call "Fixnum.Make"))
                      `((:ldloc ,key))))
              ;; No lexical binding — check special (includes locally declared specials)
              `(,@(compile-sym-lookup sym)
                (:castclass "Symbol") (:call "DynamicBindings.Get")))))))

;;; ============================================================
;;; Main expression compiler
;;; ============================================================

(defvar *in-mv-context* nil
  "T when compiling an expression whose multiple values should propagate
   (e.g. the form inside multiple-value-list). Default nil = unwrap MvReturn.")

(defun compile-expr-raw (expr)
  "Compile expression without MvReturn unwrapping."
  ;; SBCL cross-compile: expand SB-INT:QUASIQUOTE at compile time
  #+sbcl
  (when (and (consp expr)
             (symbolp (car expr))
             (string= (symbol-name (car expr)) "QUASIQUOTE")
             (let ((pkg (symbol-package (car expr))))
               (and pkg (member (package-name pkg) '("SB-INT" "SB-IMPL") :test #'string=))))
    (setf expr (macroexpand-1 expr)))
  (cond
    ((integerp expr)
     (if (typep expr '(integer #.(- (expt 2 63)) #.(1- (expt 2 63))))
         (emit-fixnum expr)
         ;; Bignum: use constant pool
         `((:load-const ,expr))))
    ((typep expr 'single-float) `((:load-const ,expr)))
    ((typep expr 'double-float) `((:load-const ,expr)))
    ((typep expr 'ratio) `((:load-const ,expr)))
    ((and (stringp expr) (= (array-rank expr) 1))
     (if *cross-compiling*
         `((:ldstr ,expr) (:newobj "LispString"))
         `((:load-const ,expr))))
    ((characterp expr) `((:ldc-i4 ,(char-code expr)) (:call "LispChar.Make")))
    ((null expr) (emit-nil))
    ((eq expr t) (emit-t))
    ((keywordp expr) `((:ldstr ,(symbol-name expr)) (:call "Startup.Keyword")))
    ((symbolp expr) (compile-var-ref expr))
    ((consp expr) (compile-form expr))
    ((vectorp expr)
     ;; Vector literals are self-evaluating in CL — quote each element
     (compile-quoted expr))
    ;; Other self-evaluating objects (pathnames, etc.) → load as constant
    (t `((:load-const ,expr)))))

(defun compile-expr (expr)
  "Compile expression. Unwraps MvReturn unless in MV-propagating position.
   Tail positions (*in-tail-position* t) propagate MV to the caller.
   MV-context positions (*in-mv-context* t, e.g. inside multiple-value-list)
   also propagate. Single-value forms never produce MvReturn so no unwrap."
  (let ((code (compile-expr-raw expr)))
    (if (or *in-mv-context* *in-tail-position* (single-value-form-p expr))
        code
        `(,@code (:call "Runtime.UnwrapMv")))))

(defun compile-for-single-value (expr)
  "Compile expr and ensure result is a single value (unwrap MvReturn).
   Forces unwrap even in MV context. Used at positions that must always be single-valued."
  (if (single-value-form-p expr)
      (compile-expr-raw expr)
      `(,@(compile-expr-raw expr)
        (:call "Runtime.UnwrapMv"))))

;;; ============================================================
;;; Form dispatch
;;; ============================================================

(defvar *compile-depth* 0)


(defun find-macro-expander (sym)
  "Find macro expander for SYM by symbol identity.
   Checks *macros* first, then (at runtime only) the runtime macro table
   (MACRO-FUNCTION) for C#-registered macros such as DOTCL:WITHOUT-PACKAGE-LOCKS.
   Runtime-table entries take (form env); we wrap them to the 1-arg convention.
   Skipped during cross-compile to avoid picking up host (SBCL) macro definitions."
  (or (gethash sym *macros*)
      ;; Runtime fallback: pick up macros registered from C# (e.g.
      ;; DOTCL:WITHOUT-PACKAGE-LOCKS) that the Lisp-side *macros* table doesn't
      ;; know about. Limit to symbols in the DOTCL package to avoid the cost of
      ;; a per-form macro-function lookup for every operator. Skipped during
      ;; cross-compile so we never pick up host (SBCL) macro definitions.
      (and (not *cross-compiling*)
           (symbolp sym)
           (let ((pkg (symbol-package sym)))
             (and pkg (string= (package-name pkg) "DOTCL")))
           (let ((mf (macro-function sym)))
             (and mf (lambda (form) (funcall mf form nil)))))))

(defun compile-form (expr)
  "Compile a list form (op args...).
   Dispatch: quote fast-path → flet-override check → hash table (O(1), ~250 ops)
   → cons-op cases → string=-based ops → macro expansion → named-call."
  ;; Guard to catch runaway macro expansion loops (D412). Raised to 500 from
  ;; the original 200 because legitimate large literal quasiquote forms
  ;; (e.g. cl-durian's 63-entry entity-map alist) can exceed 200 naturally,
  ;; while real infinite expansion loops still trip well before 500.
  ;; With the 256MB runtime stack (Program.Main) this is safe.
  (when (> *compile-depth* 500)
    (error (format nil "Compile depth limit exceeded at depth ~D, form head: ~S"
                   *compile-depth* (if (consp expr) (car expr) expr))))
  (let ((*compile-depth* (1+ *compile-depth*))
        (op (car expr))
        (*compile-was-toplevel* *at-toplevel*)
        (*at-toplevel* nil))
    ;; quote fast path
    (if (eq op 'quote)
        (compile-quoted (cadr expr))
      ;; Local flet/labels function override (shadowing built-ins):
      ;; Must come before hash dispatch so flet can shadow built-in functions.
      ;; CL special operators must never be shadowed by flet — explicitly excluded.
      (if (and (symbolp op)
               (not (member op '(quote function if progn let let* setq block return-from
                                 tagbody go unwind-protect catch throw flet labels macrolet
                                 symbol-macrolet the locally load-time-value eval-when
                                 multiple-value-call multiple-value-prog1 progv)))
               (assoc (mangle-name op) *local-functions* :test #'string=))
          (compile-named-call op (cdr expr))
        ;; Hash table dispatch — O(1) for all registered ops (~250 cases).
        ;; *compile-was-toplevel* is already bound above; handlers use it directly.
        (let ((handler (and (symbolp op) (gethash op *compile-form-handlers*))))
          (if handler
              (funcall handler expr)
            ;; Fallback: cons-op cases, string=-based ops, macro expansion, named-call
            (cond
              ;; (setf name) function call: ((setf foo) args...) → named call
              ((and (consp op) (symbolp (car op)) (string= (symbol-name (car op)) "SETF"))
               (compile-named-call op (cdr expr)))
              ;; ((lambda ...) args) — immediate lambda application
              ;; Store the function to a local before evaluating args so the stack is
              ;; empty during arg evaluation. CIL requires empty stack at try-block entry;
              ;; loop/return in args would fail if the function is on the stack (D767).
              ((and (consp op) (eq (car op) 'lambda))
               (let ((fn-tmp (gen-local "FN")) (arr-tmp (gen-local "FNARR")))
                 `(,@(compile-expr op)
                   (:castclass "LispFunction")
                   (:declare-local ,fn-tmp "LispFunction") (:stloc ,fn-tmp)
                   ,@(compile-args-array (cdr expr))
                   (:declare-local ,arr-tmp "LispObject[]") (:stloc ,arr-tmp)
                   (:ldloc ,fn-tmp) (:ldloc ,arr-tmp)
                   (:callvirt "LispFunction.Invoke"))))
              ;; ((declare ...) body...) — treat as (locally (declare ...) body...)
              ((and (consp op) (eq (car op) 'declare))
               (compile-expr `(locally ,op ,@(cdr expr))))
              ;; Any other Cons op: compile the op expression (expects it to
              ;; evaluate to a function designator) and funcall it with args.
              ;; CLHS is strict here but SBCL accepts forms like ((quote =) nil)
              ;; by evaluating `(quote =)` as a function designator. binfix /
              ;; series / similar generate such forms; deferring to runtime
              ;; (where the type error will surface if the designator is wrong)
              ;; matches SBCL's behavior better than rejecting at compile time.
              ((consp op)
               (let ((fn-tmp (gen-local "FN")) (arr-tmp (gen-local "FNARR")))
                 `(,@(compile-expr op)
                   (:call "Runtime.CoerceToFunction")
                   (:declare-local ,fn-tmp "LispFunction") (:stloc ,fn-tmp)
                   ,@(compile-args-array (cdr expr))
                   (:declare-local ,arr-tmp "LispObject[]") (:stloc ,arr-tmp)
                   (:ldloc ,fn-tmp) (:ldloc ,arr-tmp)
                   (:callvirt "LispFunction.Invoke"))))

              ;; String=-based dispatch for ops that may arrive from different packages.
              ;; These are rare (internal/cross-package ops) — not in the eq hash table.
              ((string= (symbol-name op) "TRY-EVAL") (compile-unary-call (cdr expr) "Runtime.TryEval"))
              ((string= (symbol-name op) "%SET-CHAR")
               (let ((args (cdr expr)))
                 `(,@(compile-expr (first args))
                   ,@(compile-expr (second args))
                   ,@(compile-expr (third args))
                   (:call "Runtime.SetChar"))))
              ((string= (symbol-name op) "%SET-ELT")
               (let ((args (cdr expr)))
                 `(,@(compile-expr (first args))
                   ,@(compile-expr (second args))
                   ,@(compile-expr (third args))
                   (:call "Runtime.SetElt"))))
              ((string= (symbol-name op) "%SET-SUBSEQ")
               (compile-named-call '%set-subseq (cdr expr)))
              ((string= (symbol-name op) "%PUTF")
               (compile-ternary-call (cdr expr) "Runtime.Putf"))
              ((and (string= (symbol-name op) "CHAR=") (= (length (cdr expr)) 2))
               (compile-binary-call (cdr expr) "Runtime.CharEqual"))
              ((string= (symbol-name op) "%MAKE-PACKAGE") (compile-unary-call (cdr expr) "Runtime.MakePackage"))
              ((string= (symbol-name op) "%PACKAGE-USE") (compile-binary-call (cdr expr) "Runtime.PackageUse"))
              ((string= (symbol-name op) "%PACKAGE-EXPORT") (compile-binary-call (cdr expr) "Runtime.PackageExport"))
              ((string= (symbol-name op) "%PACKAGE-IMPORT") (compile-binary-call (cdr expr) "Runtime.PackageImport"))
              ((string= (symbol-name op) "%PACKAGE-SHADOW") (compile-binary-call (cdr expr) "Runtime.PackageShadow"))
              ((string= (symbol-name op) "%PACKAGE-NICKNAME") (compile-binary-call (cdr expr) "Runtime.PackageNickname"))
              ((string= (symbol-name op) "%UNEXPORT") (compile-binary-call (cdr expr) "Runtime.UnexportSymbol"))
              ((string= (symbol-name op) "%UNUSE-PACKAGE") (compile-binary-call (cdr expr) "Runtime.UnusePackage"))
              ((string= (symbol-name op) "%SHADOWING-IMPORT") (compile-binary-call (cdr expr) "Runtime.ShadowingImport"))
              ((string= (symbol-name op) "%PACKAGE-EXTERNAL-SYMBOLS")
               (compile-unary-call (cdr expr) "Runtime.PackageExternalSymbolsList"))
              ((string= (symbol-name op) "%PACKAGE-ALL-SYMBOLS")
               (compile-unary-call (cdr expr) "Runtime.PackageAllSymbolsList"))
              ((string= (symbol-name op) "INTERN")
               `(,@(compile-args-array (cdr expr))
                 (:call "Runtime.InternSymbolV")))
              ;; defmacro: string= match to catch cross-package variants (e.g. SB-XC:DEFMACRO)
              ((and (symbolp op) (string= (symbol-name op) "DEFMACRO"))
               (compile-defmacro (cadr expr) (caddr expr) (cdddr expr)))

              ;; Macro expansion (after string= checks, before named-call).
              ;; Preserves top-level-ness per CLHS 3.2.3.1.
              ((and (symbolp op)
                    (find-macro-expander op)
                    (not (assoc (symbol-name op) *local-functions* :test #'string=)))
               (let* ((expander (find-macro-expander op))
                      (expanded (cached-macroexpand expr expander))
                      (*at-toplevel* *compile-was-toplevel*))
                 (compile-expr expanded)))

              ;; General function call (user-defined)
              ((symbolp op) (compile-named-call op (cdr expr)))

              (t (error "Cannot compile form: ~s" expr)))))))))



;;; REMOVED: The ~250 (eq op 'foo) cond branches that previously followed here
;;; have been moved to *compile-form-handlers* hash table in cil-forms.lisp.
;;; See the handler registration block at the bottom of cil-forms.lisp.

;;; ============================================================
;;; Arithmetic
;;; ============================================================

;;; ============================================================
;;; Fixnum-typed expression detection & unboxed long arithmetic
;;;
;;; When the compiler can statically prove an expression produces a fixnum
;;; (via (the fixnum E) wrappers or recursive fixnum ops), it emits native
;;; int64 arithmetic instead of Runtime.Add / Runtime.Subtract method calls.
;;; Result is boxed via Fixnum.Make only at the outermost boundary, so
;;; intermediate values stay on the evaluation stack as raw longs.
;;;
;;; Trigger: fixnum-typed-p => all operands statically fixnum => long path.
;;; Unsafe for overflow (behaves like SBCL at (optimize (safety 0) (speed 3)));
;;; two adjacent long values that overflow produce a wrapped result rather
;;; than a bignum. Acceptable for the opt-in (the fixnum ...) contract.
;;; ============================================================

(defun fixnum-typed-p (expr)
  "Return T if EXPR is statically known to produce a Fixnum value.
   Recognizes: literal integers in fixnum range, (the fixnum E),
   references to lexical locals declared fixnum (via *fixnum-locals*),
   calls to functions declared (declaim (ftype (function (...) fixnum) NAME)),
   and fixnum-typed arithmetic (+, -, *, 1+, 1-) whose operands are
   themselves fixnum-typed."
  (cond
    ((integerp expr) (and (<= -4611686018427387904 expr 4611686018427387903)))
    ;; Direct Int64 local in native function body — already long, no unbox needed (#130)
    ((and (symbolp expr)
          (boundp '*long-locals*)
          *long-locals*
          (member (symbol-name expr) *long-locals* :test #'string=)
          (lookup-local expr))
     t)
    ;; Local var declared fixnum — must be a non-captured simple local
    ((and (symbolp expr)
          (boundp '*fixnum-locals*)
          (member (symbol-name expr) *fixnum-locals* :test #'string=)
          ;; Boxed (captured) vars need indirection — stick with generic path
          (not (boxed-var-p expr))
          (lookup-local expr))
     t)
    ((and (consp expr) (eq (car expr) 'the)
          (let ((ty (cadr expr)))
            (or (eq ty 'fixnum)
                (and (consp ty) (eq (car ty) 'integer)))))
     t)
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(+ - *))
          (fixnum-typed-p (cadr expr))
          (fixnum-typed-p (caddr expr)))
     t)
    ((and (consp expr) (= (length expr) 2)
          (member (car expr) '(1+ 1-))
          (fixnum-typed-p (cadr expr)))
     t)
    ;; Declared-fixnum function return: (name ...) where name has an
    ;; ftype declaration promising a fixnum result.
    ((and (consp expr) (symbolp (car expr))
          (boundp '*function-return-types*)
          (eq (gethash (car expr) *function-return-types*) 'fixnum)
          ;; Must not be shadowed by a local flet/labels function.
          (not (assoc (mangle-name (car expr)) *local-functions* :test #'string=)))
     t)
    (t nil)))

(defun compile-as-long (expr)
  "Compile EXPR leaving an int64 on the stack. Caller must have verified
   fixnum-typed-p; this routine assumes the invariant."
  (cond
    ((integerp expr)
     `((:ldc-i8 ,expr)))
    ;; Direct Int64 local in native body — already long, no unbox (#130)
    ((and (symbolp expr)
          (boundp '*long-locals*)
          *long-locals*
          (member (symbol-name expr) *long-locals* :test #'string=)
          (lookup-local expr))
     `((:ldloc ,(lookup-local expr))))
    ;; Native self-call: long args avoid boxing; InvokeNativeN returns LispObject,
    ;; so unbox-fixnum extracts the long back for the caller (#130).
    ((and (consp expr) (symbolp (car expr))
          (boundp '*native-self-name*) *native-self-name*
          (boundp '*self-fn-local*) *self-fn-local*
          (string= (mangle-name (car expr)) *native-self-name*)
          (not (assoc (mangle-name (car expr)) *local-functions* :test #'string=))
          (let ((n (length (cdr expr)))) (and (>= n 1) (<= n 4)))
          (every #'fixnum-typed-p (cdr expr)))
     (let ((n-args (length (cdr expr))))
       `((:ldloc ,*self-fn-local*)
         ,@(mapcan (lambda (arg)
                     (let ((*in-tail-position* nil) (*in-mv-context* nil))
                       (compile-as-long arg)))
                   (cdr expr))
         (:callvirt ,(format nil "LispFunction.InvokeNative~D" n-args))
         (:unbox-fixnum))))
    ;; Declared-fixnum local: load slot (LispObject) then unbox.
    ((and (symbolp expr)
          (boundp '*fixnum-locals*)
          (member (symbol-name expr) *fixnum-locals* :test #'string=)
          (lookup-local expr))
     `((:ldloc ,(lookup-local expr))
       (:unbox-fixnum)))
    ((and (consp expr) (eq (car expr) 'the))
     ;; (the fixnum E): compile E as LispObject, then unbox.
     `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
           (compile-expr (caddr expr)))
       (:unbox-fixnum)))
    ((and (consp expr) (= (length expr) 3) (member (car expr) '(+ - *)))
     (let ((op (ecase (car expr) (+ :add) (- :sub) (* :mul))))
       `(,@(compile-as-long (cadr expr))
         ,@(compile-as-long (caddr expr))
         (,op))))
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '1+))
     `(,@(compile-as-long (cadr expr))
       (:ldc-i8 1)
       (:add)))
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '1-))
     `(,@(compile-as-long (cadr expr))
       (:ldc-i8 1)
       (:sub)))
    (t
     ;; Fallback — compile as LispObject and unbox. Shouldn't hit this
     ;; if fixnum-typed-p was checked first.
     `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
           (compile-expr expr))
       (:unbox-fixnum)))))

(defun compile-fixnum-binop (args op)
  "Emit: compile-as-long a, compile-as-long b, <op>, Fixnum.Make."
  `(,@(compile-as-long (first args))
    ,@(compile-as-long (second args))
    (,op)
    (:call "Fixnum.Make")))

(defun compile-fixnum-cmp (args op)
  "Emit native int64 comparison. OP is :lt :le :gt :ge :eq :ne.
   Leaves an i4 (0 or 1) on stack — suitable for :brfalse / :brtrue.
   Callers: compile-if-fused-comparison dispatch."
  (let ((body (ecase op
                (:lt '((:clt)))
                (:gt '((:cgt)))
                (:eq '((:ceq)))
                (:le '((:cgt) (:ldc-i4 0) (:ceq)))   ; not greater
                (:ge '((:clt) (:ldc-i4 0) (:ceq)))   ; not less
                (:ne '((:ceq) (:ldc-i4 0) (:ceq)))))) ; not equal
    `(,@(compile-as-long (first args))
      ,@(compile-as-long (second args))
      ,@body)))

;;; ============================================================
;;; Double-float native arithmetic (D672)
;;; Parallel to the fixnum path above, but emits native r8 (IEEE 754
;;; double) arithmetic with a final newobj DoubleFloat to box the result.
;;; ============================================================

(defun double-float-typed-p (expr)
  "Return T if EXPR is statically known to produce a DoubleFloat value.
   Recognizes: (the double-float E), local vars declared double-float
   (via *double-float-locals*), and recursive arithmetic (+, -, *, /)
   whose operands are themselves double-float-typed. Literal floats are
   NOT auto-detected to avoid single/double ambiguity from the reader."
  (cond
    ((and (symbolp expr)
          (boundp '*double-float-locals*)
          (member (symbol-name expr) *double-float-locals* :test #'string=)
          (not (boxed-var-p expr))
          (lookup-local expr))
     t)
    ((and (consp expr) (eq (car expr) 'the)
          (let ((ty (cadr expr)))
            (or (eq ty 'double-float)
                (and (consp ty) (eq (car ty) 'double-float)))))
     t)
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(+ - * /))
          (double-float-typed-p (cadr expr))
          (double-float-typed-p (caddr expr)))
     t)
    (t nil)))

(defun compile-as-double (expr)
  "Compile EXPR leaving a native r8 (double) on the stack.
   Caller must have verified double-float-typed-p."
  (cond
    ((and (symbolp expr)
          (boundp '*double-float-locals*)
          (member (symbol-name expr) *double-float-locals* :test #'string=)
          (lookup-local expr))
     `((:ldloc ,(lookup-local expr))
       (:unbox-double)))
    ((and (consp expr) (eq (car expr) 'the))
     `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
           (compile-expr (caddr expr)))
       (:unbox-double)))
    ((and (consp expr) (= (length expr) 3) (member (car expr) '(+ - * /)))
     (let ((op (ecase (car expr) (+ :add) (- :sub) (* :mul) (/ :div))))
       `(,@(compile-as-double (cadr expr))
         ,@(compile-as-double (caddr expr))
         (,op))))
    (t
     `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
           (compile-expr expr))
       (:unbox-double)))))

(defun compile-double-binop (args op)
  "Emit: compile-as-double a, compile-as-double b, <op>, newobj DoubleFloat."
  `(,@(compile-as-double (first args))
    ,@(compile-as-double (second args))
    (,op)
    (:newobj "DoubleFloat")))

(defun compile-double-cmp (args op)
  "Emit native r8 comparison. OP is :lt :le :gt :ge :eq :ne.
   Leaves an i4 (0 or 1) on stack."
  (let ((body (ecase op
                (:lt '((:clt)))
                (:gt '((:cgt)))
                (:eq '((:ceq)))
                (:le '((:cgt) (:ldc-i4 0) (:ceq)))
                (:ge '((:clt) (:ldc-i4 0) (:ceq)))
                (:ne '((:ceq) (:ldc-i4 0) (:ceq))))))
    `(,@(compile-as-double (first args))
      ,@(compile-as-double (second args))
      ,@body)))

;;; ============================================================
;;; Single-float native arithmetic (D736)
;;; Parallel to the double-float path above, but emits native r4 (IEEE 754
;;; single) arithmetic with a final conv.r4 + newobj SingleFloat to box.
;;; ============================================================

(defun single-float-typed-p (expr)
  "Return T if EXPR is statically known to produce a SingleFloat value.
   Recognizes: (the single-float E), local vars declared single-float
   (via *single-float-locals*), and recursive arithmetic (+, -, *, /)
   whose operands are themselves single-float-typed."
  (cond
    ((and (symbolp expr)
          (boundp '*single-float-locals*)
          (member (symbol-name expr) *single-float-locals* :test #'string=)
          (not (boxed-var-p expr))
          (lookup-local expr))
     t)
    ((and (consp expr) (eq (car expr) 'the)
          (let ((ty (cadr expr)))
            (or (eq ty 'single-float)
                (and (consp ty) (eq (car ty) 'single-float)))))
     t)
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(+ - * /))
          (single-float-typed-p (cadr expr))
          (single-float-typed-p (caddr expr)))
     t)
    (t nil)))

(defun compile-as-single (expr)
  "Compile EXPR leaving a native r4 (float) on the stack.
   Caller must have verified single-float-typed-p."
  (cond
    ((and (symbolp expr)
          (boundp '*single-float-locals*)
          (member (symbol-name expr) *single-float-locals* :test #'string=)
          (lookup-local expr))
     `((:ldloc ,(lookup-local expr))
       (:unbox-single)))
    ((and (consp expr) (eq (car expr) 'the))
     `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
           (compile-expr (caddr expr)))
       (:unbox-single)))
    ((and (consp expr) (= (length expr) 3) (member (car expr) '(+ - * /)))
     (let ((op (ecase (car expr) (+ :add) (- :sub) (* :mul) (/ :div))))
       `(,@(compile-as-single (cadr expr))
         ,@(compile-as-single (caddr expr))
         (,op))))
    (t
     `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
           (compile-expr expr))
       (:unbox-single)))))

(defun compile-single-binop (args op)
  "Emit: compile-as-single a, compile-as-single b, <op>, conv.r4, newobj SingleFloat."
  `(,@(compile-as-single (first args))
    ,@(compile-as-single (second args))
    (,op)
    (:conv-r4)
    (:newobj "SingleFloat")))

(defun compile-single-cmp (args op)
  "Emit native r4 comparison. OP is :lt :le :gt :ge :eq :ne.
   Leaves an i4 (0 or 1) on stack."
  (let ((body (ecase op
                (:lt '((:clt)))
                (:gt '((:cgt)))
                (:eq '((:ceq)))
                (:le '((:cgt) (:ldc-i4 0) (:ceq)))
                (:ge '((:clt) (:ldc-i4 0) (:ceq)))
                (:ne '((:ceq) (:ldc-i4 0) (:ceq))))))
    `(,@(compile-as-single (first args))
      ,@(compile-as-single (second args))
      ,@body)))

(defun compile-add (args)
  (case (length args)
    (0 (emit-fixnum 0))
    (1 (let ((*in-tail-position* nil)) (compile-expr (first args))))
    (2
     (cond
       ;; Fast path: both args known double-float → native r8 add (D672)
       ((and (double-float-typed-p (first args)) (double-float-typed-p (second args)))
        (compile-double-binop args :add))
       ;; Fast path: both args known single-float → native r4 add (D736)
       ((and (single-float-typed-p (first args)) (single-float-typed-p (second args)))
        (compile-single-binop args :add))
       ;; Fast path: both args known fixnum → native int64 add
       ((and (fixnum-typed-p (first args)) (fixnum-typed-p (second args)))
        (compile-fixnum-binop args :add))
       ;; Optimize (+ x 1) and (+ 1 x) to Increment
       ((eql (second args) 1)
        (compile-unary-call (list (first args)) "Runtime.Increment" "1+"))
       ((eql (first args) 1)
        (compile-unary-call (list (second args)) "Runtime.Increment" "1+"))
       (t (compile-binary-call args "Runtime.Add"))))
    (t (if (<= (length args) 8)
           (compile-expr (cons '+ (cons (list '+ (first args) (second args)) (cddr args))))
           `(,@(compile-args-array args) (:call "Runtime.AddN"))))))

(defun compile-sub (args)
  (case (length args)
    (0 (compile-static-program-error "-: too few arguments: 0 (expected at least 1)"))
    (1 (compile-binary-call (list 0 (first args)) "Runtime.Subtract"))
    (2
     (cond
       ((and (double-float-typed-p (first args)) (double-float-typed-p (second args)))
        (compile-double-binop args :sub))
       ((and (single-float-typed-p (first args)) (single-float-typed-p (second args)))
        (compile-single-binop args :sub))
       ((and (fixnum-typed-p (first args)) (fixnum-typed-p (second args)))
        (compile-fixnum-binop args :sub))
       ((eql (second args) 1)
        (compile-unary-call (list (first args)) "Runtime.Decrement" "1-"))
       (t (compile-binary-call args "Runtime.Subtract"))))
    (t (if (<= (length args) 8)
           (compile-expr (cons '- (cons (list '- (first args) (second args)) (cddr args))))
           `(,@(compile-args-array args) (:call "Runtime.SubtractN"))))))

(defun compile-mul (args)
  (case (length args)
    (0 (emit-fixnum 1))
    (1 (let ((*in-tail-position* nil)) (compile-expr (first args))))
    (2
     (cond
       ((and (double-float-typed-p (first args)) (double-float-typed-p (second args)))
        (compile-double-binop args :mul))
       ((and (single-float-typed-p (first args)) (single-float-typed-p (second args)))
        (compile-single-binop args :mul))
       ((and (fixnum-typed-p (first args)) (fixnum-typed-p (second args)))
        (compile-fixnum-binop args :mul))
       (t (compile-binary-call args "Runtime.Multiply"))))
    (t (if (<= (length args) 8)
           (compile-expr (cons '* (cons (list '* (first args) (second args)) (cddr args))))
           `(,@(compile-args-array args) (:call "Runtime.MultiplyN"))))))

(defun compile-div (args)
  (case (length args)
    (0 (compile-static-program-error "/: too few arguments: 0 (expected at least 1)"))
    (1 (compile-binary-call (list 1 (first args)) "Runtime.Divide"))
    (2
     (cond
       ((and (double-float-typed-p (first args)) (double-float-typed-p (second args)))
        (compile-double-binop args :div))
       ((and (single-float-typed-p (first args)) (single-float-typed-p (second args)))
        (compile-single-binop args :div))
       (t (compile-binary-call args "Runtime.Divide"))))
    (t (if (<= (length args) 8)
           (compile-expr (cons '/ (cons (list '/ (first args) (second args)) (cddr args))))
           `(,@(compile-args-array args) (:call "Runtime.DivideN"))))))

;;; ============================================================
;;; Call helpers
;;; ============================================================

(defun compile-static-program-error (msg)
  "Emit CIL that unconditionally signals PROGRAM-ERROR with MSG."
  `((:ldstr ,msg)
    (:newobj "LispProgramError")
    (:newobj "LispErrorException")
    (:throw)))

(defun compile-nary-comparison (args op method)
  "Compile N-arg comparison (op a b c ...) as (and (op a b) (op b c) ...).
   For N=2, directly emit binary call. For N>2, expand to let* + and."
  (let ((n (length args)))
    (cond
      ((= n 0) (compile-static-program-error
                 (format nil "~A: wrong number of arguments: 0 (expected >= 1)" op)))
      ((= n 1)
       ;; (< a) evaluates arg for side effects, returns T
       `(,@(compile-expr (car args)) (:pop) ,@(emit-t)))
      ((= n 2) (compile-binary-call args method))
      (t
       ;; Expand (op a b c ...) → (let* ((t0 a) (t1 b) (t2 c) ...) (and (op t0 t1) (op t1 t2) ...))
       (let* ((tmps (loop for i below n collect (intern (format nil "%%NARGT~A" i))))
              (bindings (loop for tmp in tmps for arg in args collect `(,tmp ,arg)))
              (pairs (loop for (t1 t2) on tmps while t2 collect `(,op ,t1 ,t2))))
         (compile-expr `(let* ,bindings (and ,@pairs))))))))

(defun simple-expr-p (expr)
  "Return T if EXPR compiles to simple stack ops without try blocks.
   Safe to leave on the evaluation stack while compiling subsequent exprs."
  (cond ((null expr) t)
        ((eq expr t) t)
        ((numberp expr) t)
        ((characterp expr) t)
        ((stringp expr) t)
        ((symbolp expr)
         ;; Simple if it's a lexical variable (not boxed, not symbol-macro)
         (and (not (lookup-symbol-macro expr))
              (lookup-local expr)
              (not (boxed-var-p expr))))
        ((and (consp expr) (eq (car expr) 'the))
         (simple-expr-p (caddr expr)))
        ((and (consp expr) (eq (car expr) 'quote)) t)
        (t nil)))

(defun compile-binary-call (args method &optional (fn-name ""))
  ;; Pre-evaluate both args to temps so the stack is empty
  ;; when each is compiled (CIL requires empty stack at try-block entry).
  ;; Args are single-valued: never in tail position, never in MV context.
  (unless (= (length args) 2)
    (return-from compile-binary-call
      (compile-static-program-error
       (format nil "~a: wrong number of arguments: ~a (expected 2)" fn-name (length args)))))
  ;; Fast path: if both args are simple, push directly without temp locals
  (if (and (simple-expr-p (first args)) (simple-expr-p (second args)))
      `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (second args)))
        (:call ,method))
      (let ((t1 (gen-local "BA")) (t2 (gen-local "BB")))
        `((:declare-local ,t1 "LispObject")
          (:declare-local ,t2 "LispObject")
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))  (:stloc ,t1)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (second args))) (:stloc ,t2)
          (:ldloc ,t1) (:ldloc ,t2)
          (:call ,method)))))

(defun compile-ternary-call (args method &optional (fn-name ""))
  (unless (= (length args) 3)
    (return-from compile-ternary-call
      (compile-static-program-error
       (format nil "~a: wrong number of arguments: ~a (expected 3)" fn-name (length args)))))
  (if (every #'simple-expr-p args)
      `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (second args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (third args)))
        (:call ,method))
      (let ((t1 (gen-local "TA")) (t2 (gen-local "TB")) (t3 (gen-local "TC")))
        `((:declare-local ,t1 "LispObject")
          (:declare-local ,t2 "LispObject")
          (:declare-local ,t3 "LispObject")
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))  (:stloc ,t1)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (second args))) (:stloc ,t2)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (third args)))  (:stloc ,t3)
          (:ldloc ,t1) (:ldloc ,t2) (:ldloc ,t3)
          (:call ,method)))))

(defun compile-quaternary-call (args method &optional (fn-name ""))
  (unless (= (length args) 4)
    (return-from compile-quaternary-call
      (compile-static-program-error
       (format nil "~a: wrong number of arguments: ~a (expected 4)" fn-name (length args)))))
  (if (every #'simple-expr-p args)
      `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (second args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (third args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (fourth args)))
        (:call ,method))
      (let ((t1 (gen-local "QA")) (t2 (gen-local "QB")) (t3 (gen-local "QC")) (t4 (gen-local "QD")))
        `((:declare-local ,t1 "LispObject")
          (:declare-local ,t2 "LispObject")
          (:declare-local ,t3 "LispObject")
          (:declare-local ,t4 "LispObject")
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))  (:stloc ,t1)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (second args))) (:stloc ,t2)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (third args)))  (:stloc ,t3)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (fourth args))) (:stloc ,t4)
          (:ldloc ,t1) (:ldloc ,t2) (:ldloc ,t3) (:ldloc ,t4)
          (:call ,method)))))

(defun compile-quinary-call (args method &optional (fn-name ""))
  (unless (= (length args) 5)
    (return-from compile-quinary-call
      (compile-static-program-error
       (format nil "~a: wrong number of arguments: ~a (expected 5)" fn-name (length args)))))
  (if (every #'simple-expr-p args)
      `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (second args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (third args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (fourth args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (fifth args)))
        (:call ,method))
      (let ((t1 (gen-local "QA")) (t2 (gen-local "QB")) (t3 (gen-local "QC"))
            (t4 (gen-local "QD")) (t5 (gen-local "QE")))
        `((:declare-local ,t1 "LispObject")
          (:declare-local ,t2 "LispObject")
          (:declare-local ,t3 "LispObject")
          (:declare-local ,t4 "LispObject")
          (:declare-local ,t5 "LispObject")
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))  (:stloc ,t1)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (second args))) (:stloc ,t2)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (third args)))  (:stloc ,t3)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (fourth args))) (:stloc ,t4)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (fifth args)))  (:stloc ,t5)
          (:ldloc ,t1) (:ldloc ,t2) (:ldloc ,t3) (:ldloc ,t4) (:ldloc ,t5)
          (:call ,method)))))

(defun compile-unary-call (args method &optional (fn-name ""))
  (unless (= (length args) 1)
    (let ((errmsg (format nil "~a: wrong number of arguments: ~a (expected 1)" fn-name (length args))))
      (return-from compile-unary-call
        (compile-static-program-error errmsg))))
  ;; Arg is single-valued: never in tail position, never in MV context
  `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))
    (:call ,method)))

(defun compile-append (args)
  "Compile (append ...) with 0+ arguments.
   Reduces multi-arg append to nested binary Runtime.Append calls."
  (cond
    ((null args) (emit-nil))
    ((null (cdr args)) (compile-expr (car args)))
    ((null (cddr args)) (compile-binary-call args "Runtime.Append"))
    (t ;; 3+ args: fold left into nested binary appends
       (let ((acc (car args)))
         (dolist (a (cdr args))
           (setq acc (list 'append acc a)))
         (compile-form acc)))))

(defun compile-list-call (args)
  (if (null args)
      (emit-nil)
      `(,@(compile-args-array args)
        (:call "Runtime.List")
        (:call "MultipleValues.Primary"))))

(defun compile-list-star-call (args)
  (if (null args)
      (error "LIST* requires at least one argument")
      `(,@(compile-args-array args)
        (:call "Runtime.ListStar"))))

(defun compile-gethash (args)
  (let ((nargs (length args)))
    (cond
      ((< nargs 2) (compile-static-program-error "GETHASH: too few arguments (expected 2-3)"))
      ((> nargs 3) (compile-static-program-error "GETHASH: too many arguments (expected 2-3)"))
      (t
       `(,@(compile-expr (first args))
         ,@(compile-expr (second args))
         ,@(if (= nargs 3)
                (compile-expr (third args))
                '((:ldnull)))
         (:call "Runtime.Gethash"))))))

(defun compile-puthash (args)
  "Compile (puthash key table value)."
  `(,@(compile-expr (first args))
    ,@(compile-expr (second args))
    ,@(compile-expr (third args))
    (:call "Runtime.Puthash")))

(defun compile-values-call (args)
  `(,@(compile-args-array args)
    (:call "Runtime.Values")))

(defun compile-subseq (args)
  "Compile (subseq seq start &optional end)."
  (let ((nargs (length args)))
    (cond
      ((or (< nargs 2) (> nargs 3))
       ;; Wrong number of args → PROGRAM-ERROR
       (compile-expr '(error 'program-error)))
      (t
       `(,@(compile-expr (first args))
         ,@(compile-expr (second args))
         ,@(if (= nargs 3)
               (compile-expr (third args))
               (emit-nil))
         (:call "Runtime.Subseq"))))))

(defun compile-concatenate (args)
  "Compile (concatenate result-type seq1 seq2 ...)."
  (if (null args)
      (compile-expr '(error 'program-error))
      (let ((rt-tmp (gen-local "RTTYP"))
            (arr-tmp (gen-local "CATARR")))
        `((:declare-local ,rt-tmp "LispObject")
          (:declare-local ,arr-tmp "LispObject[]")
          ,@(let ((*in-tail-position* nil)) (compile-expr (first args)))
          (:stloc ,rt-tmp)
          ,@(compile-args-array (cdr args))
          (:stloc ,arr-tmp)
          (:ldloc ,rt-tmp)
          (:ldloc ,arr-tmp)
          (:call "Runtime.Concatenate")))))

(defun compile-args-array (args)
  "Compile arguments into a LispObject[] array on the stack.
   Pre-evaluates all args to temps so the stack is empty during
   each compile-expr (CIL requires empty stack at try-block entry).
   Args are never in tail position and receive a single value each."
  (let ((n (length args)))
    (if (zerop n)
        `((:ldc-i4 0) (:newarr "LispObject"))
        (let ((temps (loop for a in args collect (gen-local "AA"))))
          `(,@(loop for arg in args
                    for tmp in temps
                    append `((:declare-local ,tmp "LispObject")
                             ,@(let ((*in-tail-position* nil)
                                     (*in-mv-context* nil))
                                 (compile-expr arg)) (:stloc ,tmp)))
            (:ldc-i4 ,n)
            (:newarr "LispObject")
            ,@(loop for tmp in temps
                    for i from 0
                    append `((:dup) (:ldc-i4 ,i) (:ldloc ,tmp) (:stelem-ref))))))))
