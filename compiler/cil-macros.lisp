;;; cil-macros.lisp — Standard macro definitions
;;; Part of the CIL compiler (A2 instruction list architecture)

(in-package :dotcl.cil-compiler)

;;; ============================================================
;;; Standard macros (installed at compile-time load)
;;; ============================================================

;;; --- destructuring-bind ---

(defun %normalize-db-pattern (pattern)
  "Normalize dotted tails to &rest: (a b . c) → (a b &rest c)"
  (cond
    ((null pattern) nil)
    ((atom pattern) (list '&rest pattern))
    (t (cons (car pattern) (%normalize-db-pattern (cdr pattern))))))

(defun %db-bindings (pattern form-var)
  "Generate let* bindings for destructuring PATTERN against FORM-VAR."
  (let ((bindings nil)
        (rest-var form-var)
        (mode :required))
    ;; Handle &whole
    (when (and (consp pattern) (eq (car pattern) '&whole))
      (let ((whole-var (cadr pattern)))
        (if (symbolp whole-var)
            (push (list whole-var form-var) bindings)
            ;; &whole (a . b) — recursively destructure the pattern
            (dolist (b (%db-bindings whole-var form-var))
              (push b bindings))))
      (setq pattern (cddr pattern)))
    ;; Normalize dotted pairs
    (setq pattern (%normalize-db-pattern pattern))
    ;; Process elements
    (dolist (elem pattern)
      (cond
        ((eq elem '&optional) (setq mode :optional))
        ((or (eq elem '&rest) (eq elem '&body)) (setq mode :rest))
        ((eq elem '&key) (setq mode :key))
        ((eq elem '&allow-other-keys) nil)
        ((eq elem '&aux) (setq mode :aux))
        (t
         (case mode
           (:required
            (let ((new-rest (gensym "REST")))
              (if (consp elem)
                  ;; Nested destructuring
                  (let ((sub-var (gensym "SUB")))
                    (push (list sub-var `(car ,rest-var)) bindings)
                    (dolist (b (%db-bindings elem sub-var))
                      (push b bindings))
                    (push (list new-rest `(cdr ,rest-var)) bindings))
                  (if (null elem)
                      ;; NIL means empty-list sub-pattern — value must be NIL
                      (let ((check-var (gensym "NILCHK")))
                        (push (list check-var `(car ,rest-var)) bindings)
                        (push (list (gensym "IGNORE")
                                    `(if ,check-var (error 'program-error) nil)) bindings)
                        (push (list new-rest `(cdr ,rest-var)) bindings))
                      (progn
                        (push (list elem `(car ,rest-var)) bindings)
                        (push (list new-rest `(cdr ,rest-var)) bindings))))
              (setq rest-var new-rest)))
           (:optional
            (let* ((var-spec (if (consp elem) (car elem) elem))
                   (default (if (consp elem) (cadr elem) nil))
                   (supplied-p (if (consp elem) (caddr elem) nil))
                   (new-rest (gensym "REST")))
              (if (consp var-spec)
                  ;; Nested destructuring in optional position: ((y z) default)
                  (let ((sub-var (gensym "OSUB")))
                    (push (list sub-var `(if ,rest-var (car ,rest-var) ,default)) bindings)
                    (dolist (b (%db-bindings var-spec sub-var))
                      (push b bindings))
                    (when supplied-p
                      (push (list supplied-p `(if ,rest-var t nil)) bindings)))
                  (progn
                    (push (list var-spec `(if ,rest-var (car ,rest-var) ,default)) bindings)
                    (when supplied-p
                      (push (list supplied-p `(if ,rest-var t nil)) bindings))))
              (push (list new-rest `(if ,rest-var (cdr ,rest-var) nil)) bindings)
              (setq rest-var new-rest)))
           (:rest
            (if (consp elem)
                ;; Nested destructuring on &rest/&body
                (let ((sub-var (gensym "RSUB")))
                  (push (list sub-var rest-var) bindings)
                  (dolist (b (%db-bindings elem sub-var))
                    (push b bindings)))
                (push (list elem rest-var) bindings)))
           (:key
            (let* ((key-spec (if (consp elem) elem (list elem)))
                   (var-spec (car key-spec))
                   ;; var-spec can be: symbol, (keyword var), or (keyword (destructure-pattern))
                   (inner-var (if (consp var-spec) (cadr var-spec) var-spec))
                   (keyword (if (consp var-spec)
                                (car var-spec)
                                (intern (symbol-name var-spec) "KEYWORD")))
                   (default (cadr key-spec))
                   (supplied-p (caddr key-spec))
                   (found-var (gensym "KV")))
              (push (list found-var `(member ,keyword ,rest-var)) bindings)
              (if (consp inner-var)
                  ;; Nested destructuring in key position: ((:A (B C)) default)
                  (let ((sub-var (gensym "KSUB")))
                    (push (list sub-var `(if ,found-var (cadr ,found-var) ,default)) bindings)
                    (dolist (b (%db-bindings inner-var sub-var))
                      (push b bindings))
                    (when supplied-p
                      (push (list supplied-p `(if ,found-var t nil)) bindings)))
                  (progn
                    (push (list inner-var `(if ,found-var (cadr ,found-var) ,default)) bindings)
                    (when supplied-p
                      (push (list supplied-p `(if ,found-var t nil)) bindings))))))
           (:aux
            ;; &aux (var init-form) — simple sequential binding
            (let* ((var (if (consp elem) (car elem) elem))
                   (init (if (consp elem) (cadr elem) nil)))
              (push (list var init) bindings)))))))
    (nreverse bindings)))

(defun %db-has-nil-var (pattern)
  "Check if a destructuring-bind pattern contains NIL as a variable name in required position."
  (when (consp pattern)
    (do ((rest pattern (cdr rest)))
        ((not (consp rest)) nil)
      (let ((elem (car rest)))
        (cond
          ((member elem '(&optional &rest &body &key &allow-other-keys &aux &whole))
           (return nil))  ; stop checking after lambda-list keywords
          ((null elem)
           (return t))
          ((consp elem)
           (when (%db-has-nil-var elem)
             (return t))))))))

(setf (gethash 'destructuring-bind *macros*)
      (lambda (form)
        ;; (destructuring-bind pattern expr body...)
        (let ((pattern (cadr form))
              (expr (caddr form))
              (body (cdddr form))
              (temp (gensym "FORM")))
          `(let* ((,temp ,expr)
                  ,@(%db-bindings pattern temp))
             ,@body))))

;;; --- loop ---
;;; LOOP implementation moved to compiler/loop.lisp (CMUCL-derived)
;;; Old hand-written implementation removed.

;;; --- setf expander table ---
(defvar *setf-expanders* (make-hash-table :test #'equal)
  "Table of custom setf expanders: accessor-name-string → (lambda (place value) expanded-form)")

;;; --- struct info table for :include support ---
(defvar *struct-info* (make-hash-table :test #'equal)
  "Table of struct metadata: struct-name-string → plist (:slots :parent :conc-prefix)")

(defvar *struct-accessors* (make-hash-table :test #'equal)
  "Maps accessor name (string) to slot index (integer) for compile-time inlining.
   Only populated for standard (non-typed) structs.")

(defvar *setf-expansion-fns* (make-hash-table :test #'equal)
  "DEFINE-SETF-EXPANDER entries: accessor-name-string → (lambda (place) → 5 values)")

;;; --- Setf-expander table key computation ---
;;; CL symbols (CAR, CDR, ELT, ...) use bare symbol-name to match the built-in
;;; registrations at the top of this file. Non-CL symbols use a qualified
;;; "PKG:NAME" key so that (defsetf L-MATH::ELT ...) does not clobber
;;; CL:ELT's setf-expander — otherwise l-math's expander body using
;;; (setf (cl:elt ...)) recurses infinitely into its own expander.
;;; Lookup must try the qualified key first, then fall back to the bare key.
(defun %setf-key (sym)
  (let ((pkg (and (symbolp sym) (symbol-package sym))))
    (if pkg
        (let ((pname (package-name pkg)))
          (if (or (string= pname "COMMON-LISP") (string= pname "CL"))
              (symbol-name sym)
              (concatenate 'string pname ":" (symbol-name sym))))
        (symbol-name sym))))

(defun %lookup-setf-expander (sym table)
  "Try qualified key first, fall back to bare symbol-name for CL inheritance."
  (or (gethash (%setf-key sym) table)
      (gethash (symbol-name sym) table)))

;; (setf (documentation x doc-type) new-value)
(setf (gethash "DOCUMENTATION" *setf-expanders*)
      (lambda (place value)
        (let ((tmp (gensym "V")))
          `(let ((,tmp ,value))
             (funcall #'(setf documentation) ,tmp ,(cadr place) ,(caddr place))
             ,tmp))))

;; (setf (char string index) char) → Runtime.SetChar
(setf (gethash "CHAR" *setf-expanders*)
      (lambda (place value)
        ;; place = (char str idx), value = new-char
        `(%set-char ,(second place) ,(third place) ,value)))

(setf (gethash "SCHAR" *setf-expanders*)
      (lambda (place value)
        `(%set-char ,(second place) ,(third place) ,value)))

(setf (gethash "ELT" *setf-expanders*)
      (lambda (place value)
        `(%set-elt ,(second place) ,(third place) ,value)))

(setf (gethash "AREF" *setf-expanders*)
      (lambda (place value)
        `(%aref-set ,(cadr place) ,@(cddr place) ,value)))

(setf (gethash "SVREF" *setf-expanders*)
      (lambda (place value)
        `(%set-elt ,(second place) ,(third place) ,value)))

(setf (gethash "FILL-POINTER" *setf-expanders*)
      (lambda (place value)
        `(%set-fill-pointer ,(second place) ,value)))

(setf (gethash "READTABLE-CASE" *setf-expanders*)
      (lambda (place value)
        `(%set-readtable-case ,(second place) ,value)))

(setf (gethash "LOGICAL-PATHNAME-TRANSLATIONS" *setf-expanders*)
      (lambda (place value)
        `(%set-logical-pathname-translations ,(second place) ,value)))

(setf (gethash "CLASS-NAME" *setf-expanders*)
      (lambda (place value)
        (let ((tmp (gensym "V")))
          `(let ((,tmp ,value))
             (funcall #'(setf class-name) ,tmp ,(second place))
             ,tmp))))

(setf (gethash "SBIT" *setf-expanders*)
      (lambda (place value)
        `(%aref-set ,(cadr place) ,@(cddr place) ,value)))

(setf (gethash "BIT" *setf-expanders*)
      (lambda (place value)
        `(%aref-set ,(cadr place) ,@(cddr place) ,value)))

(setf (gethash "ROW-MAJOR-AREF" *setf-expanders*)
      (lambda (place value)
        `(%set-row-major-aref ,(second place) ,(third place) ,value)))

(setf (gethash "SUBSEQ" *setf-expanders*)
      (lambda (place value)
        ;; (setf (subseq seq start end) new-seq)
        `(%set-subseq ,(second place) ,(third place) ,(fourth place) ,value)))

;; (setf (compiler-macro-function ...) ...) — no-op stub
(setf (gethash "GETF" *setf-expanders*)
      (lambda (place value)
        ;; (setf (getf plist-place indicator [default]) value)
        ;; CL spec: evaluate plist-place subforms, then indicator,
        ;; then default (for side effects only), then value, L→R, once each.
        (let* ((plist-place (second place))
               (indicator (third place))
               (default (fourth place))
               (ind-var (gensym "IND"))
               (val-var (gensym "VAL")))
          (multiple-value-bind (temps vals stores setter getter)
              (%get-setf-expansion plist-place)
            `(let* (,@(mapcar #'list temps vals)
                    (,ind-var ,indicator)
                    ,@(when default `((,(gensym "DEF") ,default)))
                    (,val-var ,value))
               (let ((,(car stores) (%putf ,getter ,ind-var ,val-var)))
                 ,setter)
               ,val-var)))))

;;; GETF proper 5-value setf expansion.
;;; The *setf-expanders* entry above is used by (setf ...) directly, but
;;; psetf/incf/rotatef use %get-setf-expansion → *setf-expansion-fns* lookup.
;;; Without this entry, the fallback wraps plist-place in a gensym temp and
;;; the setter writes to the temp instead of the original variable.
(setf (gethash "GETF" *setf-expansion-fns*)
      (lambda (place)
        (let* ((plist-place (second place))
               (indicator-form (third place))
               (default-form (fourth place))
               (ind-var (gensym "IND"))
               (s (gensym "STORE")))
          (multiple-value-bind (pl-temps pl-vals pl-stores pl-setter pl-getter)
              (%get-setf-expansion plist-place)
            (let* ((pl-store (car pl-stores))
                   ;; CLHS evaluation order: plist-subforms, then indicator,
                   ;; then default (for side effects), then new-value.
                   ;; So pl-temps/pl-vals come FIRST, ind-var/def after.
                   (def-var (when default-form (gensym "DEF")))
                   (my-temps (if def-var
                                 (append pl-temps (list ind-var def-var))
                                 (append pl-temps (list ind-var))))
                   (my-vals (if def-var
                                (append pl-vals (list indicator-form default-form))
                                (append pl-vals (list indicator-form)))))
              (values my-temps
                      my-vals
                      (list s)
                      `(let ((,pl-store (%putf ,pl-getter ,ind-var ,s)))
                         ,pl-setter)
                      (if def-var
                          `(getf ,pl-getter ,ind-var ,def-var)
                          `(getf ,pl-getter ,ind-var))))))))

(setf (gethash "SYMBOL-PLIST" *setf-expanders*)
      (lambda (place value)
        `((lambda (v s) (funcall #'(setf symbol-plist) v s) v) ,value ,(second place))))

(setf (gethash "GET" *setf-expanders*)
      (lambda (place value)
        ;; (setf (get sym indicator &optional default) value)
        ;; Default form must be evaluated for side effects (per CLHS)
        (let ((default-form (fourth place)))
          `((lambda (v s k)
              ,@(when default-form `(,default-form))
              (funcall #'(setf get) v s k) v)
            ,value ,(second place) ,(third place)))))

(setf (gethash "FIND-CLASS" *setf-expanders*)
      (lambda (place value)
        ;; (setf (find-class name &optional errorp env) class)
        ;; Must evaluate errorp/env for side effects even though they're unused
        (let ((extra-args (cddr place)))
          (if extra-args
              `(let ((%v ,value) (%n ,(second place)))
                 ,@extra-args  ; evaluate for side effects
                 (funcall #'(setf find-class) %v %n) %v)
              `((lambda (v n) (funcall #'(setf find-class) v n) v) ,value ,(second place))))))

(defun macro-key-for-symbol (sym)
  "Return key for *macros* — the symbol itself (identity-based lookup)."
  sym)

(setf (gethash "MACRO-FUNCTION" *setf-expanders*)
      (lambda (place value)
        ;; (setf (macro-function name) fn)
        ;; Update both C# _macroFunctions and *macros* Lisp table
        ;; D433: protect CL macros from foreign package overwrite
        ;; D594: if called via a non-CL shadow (e.g. SB-XC:MACRO-FUNCTION), skip dotcl *macros* update
        ;;       to prevent infinite recursion when the stored lambda calls cl:macro-function
        (let ((fn-sym (first place)))
          (if (and (symbolp fn-sym)
                   (let ((pkg (symbol-package fn-sym)))
                     (and pkg (not (string= (package-name pkg) "COMMON-LISP")))))
              ;; Non-CL variant (e.g. SB-XC:MACRO-FUNCTION): no-op for dotcl's macro table
              `(progn ,value)
              ;; Normal CL:MACRO-FUNCTION setf
              `(let ((v ,value) (n ,(second place)))
                 (if v
                     (let ((mkey (macro-key-for-symbol n)))
                       ;; D433: skip if CL macro already registered
                       (if (and (let ((pkg (symbol-package n)))
                                  (and pkg (string= (package-name pkg) "COMMON-LISP")))
                                (gethash mkey *macros*))
                           v  ;; CL macro already registered — don't overwrite
                           (progn
                             (%register-macro-function-rt n v)
                             (setf (gethash mkey *macros*)
                                   (let ((fn v)) (lambda (form) (funcall fn form nil))))
                             v)))
                     (progn
                       (%unregister-macro-function-rt n)
                       (remhash (macro-key-for-symbol n) *macros*)
                       v)))))))

(setf (gethash "FDEFINITION" *setf-expanders*)
      (lambda (place value)
        `(%set-fdefinition ,(second place) ,value)))

(setf (gethash "SYMBOL-FUNCTION" *setf-expanders*)
      (lambda (place value)
        `(%set-fdefinition ,(second place) ,value)))

;; (setf (compiler-macro-function name) fn) — delegate to %register-compiler-macro-rt.
(setf (gethash "COMPILER-MACRO-FUNCTION" *setf-expanders*)
      (lambda (place value)
        `(%register-compiler-macro-rt ,(second place) ,value)))

;;; setf for car/cdr and compound cXXXr forms
(setf (gethash "CAR" *setf-expanders*)
      (lambda (place value)
        `(progn (rplaca ,(second place) ,value) ,value)))
(setf (gethash "CDR" *setf-expanders*)
      (lambda (place value)
        `(progn (rplacd ,(second place) ,value) ,value)))
(setf (gethash "FIRST" *setf-expanders*)
      (lambda (place value)
        `(progn (rplaca ,(second place) ,value) ,value)))
(setf (gethash "REST" *setf-expanders*)
      (lambda (place value)
        `(progn (rplacd ,(second place) ,value) ,value)))

;; .NET interop: (setf (dotnet:invoke obj "Prop") v) → property/field set
;; (setf (dotnet:invoke obj "Item" idx) v) → indexed property set
;; Symbol intern is deferred to expansion time so SBCL host (cross-compile)
;; can read this file without the DOTNET package existing.
(setf (gethash "DOTNET:INVOKE" *setf-expanders*)
      (lambda (place value)
        `(,(intern "%SET-INVOKE" (find-package "DOTNET"))
          ,@(rest place) ,value)))
(setf (gethash "DOTNET:STATIC" *setf-expanders*)
      (lambda (place value)
        `(,(intern "%SET-STATIC" (find-package "DOTNET"))
          ,@(rest place) ,value)))
;; cXXr: (setf (caar x) v) → (progn (rplaca (car x) v) v)
(dolist (spec '(("CAAR" car car) ("CADR" car cdr) ("CDAR" cdr car) ("CDDR" cdr cdr)
               ("CAAAR" car car car) ("CAADR" car car cdr) ("CADAR" car cdr car)
               ("CADDR" car cdr cdr) ("CDAAR" cdr car car) ("CDADR" cdr car cdr)
               ("CDDAR" cdr cdr car) ("CDDDR" cdr cdr cdr)
               ("CAAAAR" car car car car) ("CAAADR" car car car cdr)
               ("CAADAR" car car cdr car) ("CAADDR" car car cdr cdr)
               ("CADAAR" car cdr car car) ("CADADR" car cdr car cdr)
               ("CADDAR" car cdr cdr car) ("CADDDR" car cdr cdr cdr)
               ("CDAAAR" cdr car car car) ("CDAADR" cdr car car cdr)
               ("CDADAR" cdr car cdr car) ("CDADDR" cdr car cdr cdr)
               ("CDDAAR" cdr cdr car car) ("CDDADR" cdr cdr car cdr)
               ("CDDDAR" cdr cdr cdr car) ("CDDDDR" cdr cdr cdr cdr)))
  (let* ((name (car spec))
         (ops (cdr spec))
         (setter (if (eq (car ops) 'car) 'rplaca 'rplacd))
         (inner-ops (cdr ops)))
    ;; Build the inner access chain: (car (cdr (car x))) etc.
    (setf (gethash name *setf-expanders*)
          (let ((s setter) (io inner-ops))
            (lambda (place value)
              (let ((inner (second place)))
                (dolist (op (reverse io))
                  (setf inner (list op inner)))
                `(progn (,s ,inner ,value) ,value)))))))

;; second through tenth
(dolist (pair '(("SECOND" car cdr) ("THIRD" car cdr cdr) ("FOURTH" car cdr cdr cdr)
               ("FIFTH" car cddddr) ("SIXTH" car cdr cddddr)
               ("SEVENTH" car cdr cdr cddddr) ("EIGHTH" car cdr cdr cdr cddddr)
               ("NINTH" car cddddr cddddr) ("TENTH" car cdr cddddr cddddr)))
  (let* ((name (car pair))
         (ops (cdr pair)))
    (setf (gethash name *setf-expanders*)
          (let ((captured-ops ops))
            (lambda (place value)
              ;; Build (setf (cadr x) v) etc. by wrapping inner ops then using (setf (caXr ...) v)
              (let ((inner (second place))
                    (val-var (gensym "VAL")))
                ;; Apply all but the first op from inside out
                (dolist (op (reverse (cdr captured-ops)))
                  (setf inner (list op inner)))
                ;; Final op determines setter — use temp var to evaluate value once
                (let ((final-op (car captured-ops)))
                  (if (eq final-op 'car)
                      `(let ((,val-var ,value)) (rplaca ,inner ,val-var) ,val-var)
                      `(let ((,val-var ,value)) (rplacd ,inner ,val-var) ,val-var)))))))))

;; (setf (nth n list) val)
(setf (gethash "NTH" *setf-expanders*)
      (lambda (place value)
        (let ((n-var (gensym "N"))
              (list-var (gensym "LIST"))
              (val-var (gensym "VAL")))
          `(let ((,n-var ,(second place))
                 (,list-var ,(third place))
                 (,val-var ,value))
             (rplaca (nthcdr ,n-var ,list-var) ,val-var)
             ,val-var))))

;;; LDB: (setf (ldb bytespec int-place) new-val)
;;; → (setf int-place (dpb new-val bytespec int-place))
;;; LDB setf: (setf (ldb bytespec int-place) new-val)
;;; Must evaluate bytespec subforms, then int-place subforms, then new-val,
;;; each exactly once and in left-to-right order.
(setf (gethash "LDB" *setf-expanders*)
      (lambda (place value)
        (let* ((bytespec-form (second place))
               (int-place (third place))
               (bs-var (gensym "BS"))
               (val-var (gensym "VAL")))
          (if (symbolp int-place)
              ;; Simple variable place
              `(let* ((,bs-var ,bytespec-form)
                      (,val-var ,value))
                 (setq ,int-place (dpb ,val-var ,bs-var ,int-place))
                 ,val-var)
              ;; Compound place: capture subforms in temps for correct eval order
              (let* ((head (car int-place))
                     (args (cdr int-place))
                     (temps (mapcar (lambda (a) (declare (ignore a)) (gensym "T")) args))
                     (bindings (mapcar #'list temps args))
                     (captured-place `(,head ,@temps)))
                `(let* ((,bs-var ,bytespec-form)
                        ,@bindings
                        (,val-var ,value))
                   (setf ,captured-place (dpb ,val-var ,bs-var ,captured-place))
                   ,val-var))))))

;;; MASK-FIELD setf: (setf (mask-field bytespec int-place) new-val)
;;; Same evaluation order requirements as LDB.
(setf (gethash "MASK-FIELD" *setf-expanders*)
      (lambda (place value)
        (let* ((bytespec-form (second place))
               (int-place (third place))
               (bs-var (gensym "BS"))
               (val-var (gensym "VAL")))
          (if (symbolp int-place)
              `(let* ((,bs-var ,bytespec-form)
                      (,val-var ,value))
                 (setq ,int-place (deposit-field ,val-var ,bs-var ,int-place))
                 ,val-var)
              (let* ((head (car int-place))
                     (args (cdr int-place))
                     (temps (mapcar (lambda (a) (declare (ignore a)) (gensym "T")) args))
                     (bindings (mapcar #'list temps args))
                     (captured-place `(,head ,@temps)))
                `(let* ((,bs-var ,bytespec-form)
                        ,@bindings
                        (,val-var ,value))
                   (setf ,captured-place (deposit-field ,val-var ,bs-var ,captured-place))
                   ,val-var))))))

;;; LDB/MASK-FIELD proper 5-value setf expansion (define-setf-expander protocol).
;;; The *setf-expanders* entries above handle the 2-arg (place value) form used
;;; by (setf ...) when *setf-expansion-fns* has no entry.  But psetf/rotatef/incf
;;; call %get-setf-expansion, which checks *setf-expansion-fns* first.  If LDB is
;;; only in *setf-expanders*, %get-setf-expansion wraps ALL place args in gensym
;;; temps before calling the expander, so int-place becomes a gensym temp and the
;;; generated (setq #:temp x) writes to the temp, not to the original variable.
;;; Registering in *setf-expansion-fns* lets %get-setf-expansion call the
;;; expander with the original place form and receive proper 5 values back.
(setf (gethash "LDB" *setf-expansion-fns*)
      (lambda (place)
        (let* ((bytespec-form (second place))
               (int-place (third place))
               (bs (gensym "BS"))
               (s (gensym "STORE")))
          (if (symbolp int-place)
              ;; Simple variable: no extra temps needed for int-place itself
              (values (list bs)
                      (list bytespec-form)
                      (list s)
                      `(setq ,int-place (dpb ,s ,bs ,int-place))
                      `(ldb ,bs ,int-place))
              ;; Compound place: recursively expand int-place so its subforms
              ;; are also evaluated exactly once
              (multiple-value-bind (int-temps int-vals int-stores int-setter int-getter)
                  (%get-setf-expansion int-place)
                (let ((int-store (car int-stores)))
                  (values (cons bs int-temps)
                          (cons bytespec-form int-vals)
                          (list s)
                          `(let ((,int-store (dpb ,s ,bs ,int-getter)))
                             ,int-setter)
                          `(ldb ,bs ,int-getter))))))))

(setf (gethash "MASK-FIELD" *setf-expansion-fns*)
      (lambda (place)
        (let* ((bytespec-form (second place))
               (int-place (third place))
               (bs (gensym "BS"))
               (s (gensym "STORE")))
          (if (symbolp int-place)
              (values (list bs)
                      (list bytespec-form)
                      (list s)
                      `(setq ,int-place (deposit-field ,s ,bs ,int-place))
                      `(mask-field ,bs ,int-place))
              (multiple-value-bind (int-temps int-vals int-stores int-setter int-getter)
                  (%get-setf-expansion int-place)
                (let ((int-store (car int-stores)))
                  (values (cons bs int-temps)
                          (cons bytespec-form int-vals)
                          (list s)
                          `(let ((,int-store (deposit-field ,s ,bs ,int-getter)))
                             ,int-setter)
                          `(mask-field ,bs ,int-getter))))))))

;;; --- setf ---
(setf (gethash 'setf *macros*)
      (lambda (form)
        ;; (setf place1 val1 place2 val2 ...)
        (let ((pairs (cdr form)))
          (if (= (length pairs) 2)
              ;; Single pair
              (let ((place (first pairs))
                    (value (second pairs)))
                (cond
                  ((symbolp place)
                   ;; Check for symbol-macro: (setf y ...) where y is a symbol-macro
                   (let ((sm-exp (lookup-symbol-macro place)))
                     (if sm-exp
                         ;; If symbol-macro expands to a non-place (e.g. constant 0 from XC stubs),
                         ;; treat as no-op: evaluate value for side effects and return it.
                         ;; Example: (define-symbol-macro *gc-epoch* 0) in SBCL's cross-misc.lisp
                         (if (or (symbolp sm-exp) (consp sm-exp))
                             `(setf ,sm-exp ,value)
                             (let ((tmp (gensym "SETF-NOP")))
                               `(let ((,tmp ,value)) ,tmp)))
                         `(setq ,place ,value))))
                  ((and (consp place) (eq (car place) 'car))
                   (let ((tmp (gensym "V")))
                     `(let ((,tmp ,value))
                        (rplaca ,(cadr place) ,tmp)
                        ,tmp)))
                  ((and (consp place) (eq (car place) 'cdr))
                   (let ((tmp (gensym "V")))
                     `(let ((,tmp ,value))
                        (rplacd ,(cadr place) ,tmp)
                        ,tmp)))
                  ((and (consp place) (eq (car place) 'nth))
                   (let ((n-var (gensym "N"))
                         (list-var (gensym "LIST"))
                         (val-var (gensym "VAL")))
                     `(let ((,n-var ,(cadr place))
                            (,list-var ,(caddr place))
                            (,val-var ,value))
                        (rplaca (nthcdr ,n-var ,list-var) ,val-var)
                        ,val-var)))
                  ((and (consp place) (eq (car place) 'gethash))
                   ;; (setf (gethash key table [default]) value)
                   ;; CL spec: evaluate key, table, default (for side effects), value in order
                   (let ((key-var (gensym "KEY"))
                         (table-var (gensym "TABLE"))
                         (val-var (gensym "VAL")))
                     `(let ((,key-var ,(cadr place))
                            (,table-var ,(caddr place)))
                        ,@(when (cadddr place)
                            `(,(cadddr place)))
                        (let ((,val-var ,value))
                          (puthash ,key-var ,table-var ,val-var)))))
                  ((and (consp place) (eq (car place) 'cadr))
                   (let ((tmp (gensym "V")))
                     `(let ((,tmp ,value))
                        (rplaca (cdr ,(cadr place)) ,tmp)
                        ,tmp)))
                  ((and (consp place) (eq (car place) 'cdar))
                   (let ((tmp (gensym "V")))
                     `(let ((,tmp ,value))
                        (rplacd (car ,(cadr place)) ,tmp)
                        ,tmp)))
                  ;; (setf (the type place) val) -> (setf place val)
                  ((and (consp place) (eq (car place) 'the))
                   `(setf ,(caddr place) ,value))
                  ;; (setf (values p1 p2 ...) expr) — assign each value to each place
                  ;; Per CL spec, each place receives one value from the RHS
                  ((and (consp place) (eq (car place) 'values))
                   (let* ((places (cdr place)))
                     (if (null places)
                         value
                         (let* ((stores (mapcar (lambda (p) (declare (ignore p)) (gensym "STORE")) places)))
                           `(multiple-value-bind ,stores ,value
                              ,@(mapcar (lambda (p s) `(setf ,p ,s)) places stores)
                              (values ,@stores))))))
                  ;; (setf (apply #'fn arg1 ... rest-list) val)
                  ;; → (apply #'(setf fn) val arg1 ... rest-list)
                  ((and (consp place) (eq (car place) 'apply))
                   (let* ((fn-form (cadr place))
                          (args (cddr place))
                          (fn-name (if (and (consp fn-form) (eq (car fn-form) 'function))
                                       (cadr fn-form)
                                       fn-form)))
                     `(apply (function (setf ,fn-name)) ,value ,@args)))
                  ;; (setf (slot-value obj 'name) val)
                  ((and (consp place) (eq (car place) 'slot-value))
                   `(%set-slot-value ,(cadr place) ,(caddr place) ,value))
                  ;; (setf (get symbol indicator &optional default) val)
                  ;; Per CLHS 5.1.1.1: evaluate place subforms left-to-right, then value
                  ((and (consp place) (eq (car place) 'get))
                   (let ((sym-var (gensym "SYM"))
                         (ind-var (gensym "IND"))
                         (val-var (gensym "VAL"))
                         (default-form (cadddr place)))
                     `(let* ((,sym-var ,(cadr place))
                             (,ind-var ,(caddr place))
                             ,@(when default-form `((,(gensym "DEF") ,default-form)))
                             (,val-var ,value))
                        (put-prop ,sym-var ,ind-var ,val-var))))
                  ;; (setf (aref array index...) val) — multi-index supported
                  ((and (consp place) (eq (car place) 'aref))
                   `(%aref-set ,(cadr place) ,@(cddr place) ,value))
                  ;; (setf (char string index) val)
                  ((and (consp place) (eq (car place) 'char))
                   `(%char-set ,(cadr place) ,(caddr place) ,value))
                  ;; (setf (symbol-value sym) val)
                  ((and (consp place) (eq (car place) 'symbol-value))
                   `(%set-symbol-value ,(cadr place) ,value))
                  ;; Local (setf sym) function in scope — direct call per CLHS 5.1.2.9
                  ;; Returns what the setter function returns (not necessarily value)
                  ((and (consp place) (symbolp (car place))
                        (assoc (mangle-name (list 'setf (car place))) *local-functions* :test #'string=))
                   `((setf ,(car place)) ,value ,@(cdr place)))
                  ;; DEFINE-SETF-EXPANDER entries — use 5-value protocol.
                  ;; Package-aware lookup: qualified key first (e.g.
                  ;; "L-MATH:ELT") then bare "ELT" for CL inheritance.
                  ((and (consp place)
                        (symbolp (car place))
                        (%lookup-setf-expander (car place) *setf-expansion-fns*))
                   (let ((dse-fn (%lookup-setf-expander (car place) *setf-expansion-fns*)))
                     (multiple-value-bind (temps vals stores setter getter)
                         (funcall dse-fn place)
                       (declare (ignore getter))
                       (if (= (length stores) 1)
                           `(let* (,@(mapcar #'list temps vals)
                                   (,(car stores) ,value))
                              ,setter
                              ,(car stores))
                           `(let* (,@(mapcar #'list temps vals))
                              (multiple-value-bind ,stores ,value
                                ,setter)
                              (values ,@stores))))))
                  ;; Custom setf expander lookup (package-aware)
                  ((and (consp place)
                        (symbolp (car place))
                        (%lookup-setf-expander (car place) *setf-expanders*))
                   (let ((expander (%lookup-setf-expander (car place) *setf-expanders*)))
                     (funcall expander place value)))
                  ;; Try macroexpanding the place (for defmacro/macrolet-defined accessors)
                  ;; This must come before the (SETF fn) fallback so macro-accessors work
                  ((and (consp place) (symbolp (car place))
                        (find-macro-expander (car place)))
                   (let ((expander (find-macro-expander (car place))))
                     `(setf ,(funcall expander place) ,value)))
                  ;; Fallback: try (setf accessor) function (CLOS generic setf)
                  ((and (consp place) (symbolp (car place)))
                   (let ((val-var (gensym "SETF")))
                     `(let ((,val-var ,value))
                        ((setf ,(car place)) ,val-var ,@(cdr place))
                        ,val-var)))
                  (t (error "SETF: unsupported place ~s" place))))
              ;; Multi-pair
              (let ((result '()))
                (loop while pairs
                      do (push `(setf ,(pop pairs) ,(pop pairs)) result))
                `(progn ,@(nreverse result)))))))

;;; --- defsetf ---
(setf (gethash 'defsetf *macros*)
      (lambda (form)
        ;; Short form: (defsetf accessor updater)
        ;; Long form:  (defsetf accessor (params...) (store-var) body...)
        (let ((accessor (cadr form))
              (rest (cddr form)))
          (if (and rest (symbolp (car rest)))
              ;; Short form: (defsetf accessor updater)
              ;; → (setf (accessor args...) val) expands to (updater args... val)
              (let ((updater (car rest)))
                `(progn
                   (eval-when (:compile-toplevel :load-toplevel :execute)
                     ;; Register under the package-qualified key so a defsetf on
                     ;; a shadowed accessor (e.g. L-MATH::ELT) does not clobber
                     ;; CL:ELT's built-in expander.
                     (setf (gethash ,(%setf-key accessor) *setf-expanders*)
                           (lambda (place value)
                             (let ((args (cdr place)))
                               (append (list ',updater) args (list value))))))
                   ',accessor))
              ;; Long form: (defsetf accessor (params...) (store-vars) body...)
              ;; CL spec: params are bound to gensyms; let* ensures left-to-right eval.
              ;; Multiple store-vars use multiple-value-bind.
              (let ((params (first rest))
                    (store-vars (second rest))
                    (body (cddr rest)))
                ;; Skip docstring if present
                (when (and (stringp (car body)) (cdr body))
                  (setf body (cdr body)))
                ;; Skip declarations
                (loop while (and (consp (car body))
                                 (eq (caar body) 'declare))
                      do (setf body (cdr body)))
                ;; Handle &rest in params: (defsetf acc (a &rest r) (sv) body)
                ;; body-fn cannot have sv after &rest r, so we split.
                ;; Also handle &optional in params: (defsetf acc (a &optional b) (sv) body)
                (let* ((rest-pos (position '&rest params))
                       (positional-params (if rest-pos
                                              (subseq params 0 rest-pos)
                                              params))
                       (rest-var (if rest-pos (nth (1+ rest-pos) params) nil))
                       ;; Split positional-params at &optional
                       (opt-pos (position '&optional positional-params))
                       (req-params (if opt-pos (subseq positional-params 0 opt-pos) positional-params))
                       (opt-params (if opt-pos (subseq positional-params (1+ opt-pos)) nil)))
                  `(progn
                     (eval-when (:compile-toplevel :load-toplevel :execute)
                       (setf (gethash ,(%setf-key accessor) *setf-expanders*)
                             ,(if rest-var
                                  ;; &rest case: body-fn takes (positionals... rest-list store-vars...)
                                  ;; rest-var gets a LIST of gensyms for remaining place args.
                                  `(let ((body-fn (lambda (,@positional-params ,rest-var ,@store-vars)
                                                    (block ,accessor ,@body)))
                                         (pos-names ',(mapcar #'symbol-name positional-params)))
                                     (lambda (place value)
                                       (let* ((place-args (cdr place))
                                              (pos-count ,(length positional-params))
                                              (pos-args (subseq place-args 0 pos-count))
                                              (rest-args (subseq place-args pos-count))
                                              (pos-gensyms (mapcar #'gensym pos-names))
                                              (rest-gensyms (loop repeat (length rest-args) collect (gensym)))
                                              (store-gensyms (mapcar (lambda (sv) (gensym (string sv))) ',store-vars))
                                              (body-form (apply body-fn
                                                                (append pos-gensyms
                                                                        (list rest-gensyms)
                                                                        store-gensyms)))
                                              (all-bindings (append (mapcar #'list pos-gensyms pos-args)
                                                                    (mapcar #'list rest-gensyms rest-args))))
                                         (if (= (length store-gensyms) 1)
                                             (list* 'let*
                                                    (append all-bindings
                                                            (list (list (car store-gensyms) value)))
                                                    (list body-form))
                                             (list* 'let* all-bindings
                                                    (list (list* 'multiple-value-bind
                                                                 store-gensyms value
                                                                 (list body-form))))))))
                                  ;; No &rest: handle &optional in params
                                  ;; body-fn takes (req-params... opt-params... store-vars...) all as required
                                  `(let ((body-fn (lambda (,@req-params ,@opt-params ,@store-vars)
                                                    (block ,accessor ,@body))))
                                     (lambda (place value)
                                       (let* ((req-gensyms
                                               (mapcar (lambda (p) (gensym (string p))) ',req-params))
                                              (opt-gensyms
                                               (mapcar (lambda (p) (gensym (string p))) ',opt-params))
                                              ;; How many optional args are actually provided in place
                                              (n-opt-provided
                                               (min (max 0 (- (length (cdr place)) ,(length req-params)))
                                                    ,(length opt-params)))
                                              (active-opt-gensyms (subseq opt-gensyms 0 n-opt-provided))
                                              (store-gensyms
                                               (mapcar (lambda (sv) (gensym (string sv))) ',store-vars))
                                              ;; Call body-fn with all params; pass nil for missing optionals
                                              (body-form
                                               (apply body-fn
                                                      (append req-gensyms
                                                              active-opt-gensyms
                                                              (make-list (- ,(length opt-params) n-opt-provided))
                                                              store-gensyms)))
                                              ;; Bindings only for args actually present in place
                                              (param-bindings
                                               (loop for g in (append req-gensyms active-opt-gensyms)
                                                     for arg-form in (cdr place)
                                                     collect (list g arg-form))))
                                         (if (= (length store-gensyms) 1)
                                             ;; Single store var
                                             (list* 'let*
                                                    (append param-bindings
                                                            (list (list (car store-gensyms) value)))
                                                    (list body-form))
                                             ;; Multiple store vars: use multiple-value-bind
                                             (list* 'let* param-bindings
                                                    (list (list* 'multiple-value-bind
                                                                 store-gensyms value
                                                                 (list body-form))))))))))
                     ',accessor))))))))

;;; --- define-setf-expander ---
;;; (define-setf-expander access-fn lambda-list body...)
;;; Defines a custom get-setf-expansion for access-fn.
;;; Body returns 5 values: (temps vals stores setter getter).
;;; Body is within implicit block named access-fn.
;;; Lambda-list may include &environment var (passed NIL at expansion time).
;;; Lambda-list may include &whole var (bound to the full place form).
(setf (gethash 'define-setf-expander *macros*)
      (lambda (form)
        (let* ((accessor (cadr form))
               (lambda-list (caddr form))
               (body (cdddr form))
               ;; Strip &whole from lambda-list: (&whole var ...) binds var to the place form
               (whole-pos (member '&whole lambda-list))
               (whole-var (if whole-pos (cadr whole-pos) nil))
               ;; Lambda-list without &whole and its variable name
               (no-whole-ll (if whole-pos
                                (append (ldiff lambda-list whole-pos) (cddr whole-pos))
                                lambda-list))
               ;; Strip &environment from lambda-list
               (env-pos (member '&environment no-whole-ll))
               (env-var (if env-pos (cadr env-pos) (gensym "ENV")))
               ;; Lambda-list without &environment varname
               (clean-ll (if env-pos
                             (append (ldiff no-whole-ll env-pos) (cddr env-pos))
                             no-whole-ll))
               ;; Detect and strip leading docstring
               (docstring (and (consp body) (stringp (car body)) (cdr body) (car body)))
               (real-body (if docstring (cdr body) body)))
          (if whole-var
              ;; With &whole: expander-fn takes (place-form env) and binds whole-var to place
              `(progn
                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   (setf (gethash ,(%setf-key accessor) *setf-expansion-fns*)
                         (let ((expander-fn
                                (lambda (,env-var ,whole-var ,@clean-ll)
                                  (declare (ignore ,env-var))
                                  (block ,accessor ,@real-body))))
                           (lambda (place)
                             (apply expander-fn nil place (cdr place))))))
                 ',accessor)
              ;; Without &whole: expander-fn takes (env arg1 arg2...) for args from (cdr place)
              `(progn
                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   (setf (gethash ,(%setf-key accessor) *setf-expansion-fns*)
                         (let ((expander-fn
                                (lambda (,env-var ,@clean-ll)
                                  (declare (ignore ,env-var))
                                  (block ,accessor ,@real-body))))
                           (lambda (place)
                             (apply expander-fn nil (cdr place))))))
                 ',accessor)))))

;;; --- %get-setf-expansion: helper for read-modify-write macros ---
;;; Returns (temps vals stores setter getter) for a place.
;;; Ensures each subform of the place is evaluated exactly once.
(defun %get-setf-expansion (place)
  (cond
    ((symbolp place)
     ;; Check for symbol-macro first
     (let ((sm-exp (lookup-symbol-macro place)))
       (if sm-exp
           (%get-setf-expansion sm-exp)
           (let ((s (gensym "STORE")))
             (values nil nil (list s) `(setq ,place ,s) place)))))
    ((consp place)
     (case (car place)
       (values
        ;; (values p1 p2 ...) — multiple-store expansion
        ;; Each sub-place gets its own store variable
        (let* ((sub-places (cdr place))
               (sub-expansions (mapcar (lambda (p) (multiple-value-list (%get-setf-expansion p)))
                                       sub-places))
               (all-temps (apply #'append (mapcar #'first sub-expansions)))
               (all-vals  (apply #'append (mapcar #'second sub-expansions)))
               (all-stores (apply #'append (mapcar #'third sub-expansions)))
               (setter `(progn ,@(mapcar #'fourth sub-expansions)))
               (getter `(values ,@(mapcar #'fifth sub-expansions))))
          (values all-temps all-vals all-stores setter getter)))
       (car
        (let ((obj (gensym "OBJ")) (s (gensym "STORE")))
          (values (list obj) (list (cadr place)) (list s)
                  ;; Storing-form must return store value per CLHS 5.1.2 —
                  ;; rplaca returns the cons, so wrap with progn.
                  `(progn (rplaca ,obj ,s) ,s)
                  `(car ,obj))))
       (cdr
        (let ((obj (gensym "OBJ")) (s (gensym "STORE")))
          (values (list obj) (list (cadr place)) (list s)
                  `(progn (rplacd ,obj ,s) ,s)
                  `(cdr ,obj))))
       ((aref sbit bit)
        ;; Multi-index support for aref/sbit/bit (rank-0 arrays have no indices)
        (let* ((arr (gensym "ARR"))
               (indices (cddr place))
               (idx-temps (mapcar (lambda (x) (declare (ignore x)) (gensym "IDX")) indices))
               (s (gensym "STORE")))
          (values (cons arr idx-temps)
                  (cons (cadr place) indices)
                  (list s)
                  `(%aref-set ,arr ,@idx-temps ,s)
                  `(,(car place) ,arr ,@idx-temps))))
       (svref
        (let ((arr (gensym "ARR")) (idx (gensym "IDX")) (s (gensym "STORE")))
          (values (list arr idx) (list (cadr place) (caddr place)) (list s)
                  `(%aref-set ,arr ,idx ,s)
                  `(svref ,arr ,idx))))
       (elt
        (let ((arr (gensym "ARR")) (idx (gensym "IDX")) (s (gensym "STORE")))
          (values (list arr idx) (list (cadr place) (caddr place)) (list s)
                  `(%set-elt ,arr ,idx ,s)
                  `(elt ,arr ,idx))))
       (nth
        (let ((n (gensym "N")) (lst (gensym "LST")) (s (gensym "STORE")))
          (values (list n lst) (list (cadr place) (caddr place)) (list s)
                  `(progn (rplaca (nthcdr ,n ,lst) ,s) ,s)
                  `(nth ,n ,lst))))
       (t
        ;; Check for define-setf-expander first (5-value protocol), package-aware
        (let ((dse-fn (and (symbolp (car place))
                           (%lookup-setf-expander (car place) *setf-expansion-fns*))))
          (if dse-fn
              (funcall dse-fn place)
              ;; Check for defsetf expander (2-arg: place value → form)
              (let ((exp-fn (and (symbolp (car place))
                                 (%lookup-setf-expander (car place) *setf-expanders*))))
                (if exp-fn
                    ;; Build proper 5-value expansion: evaluate each place arg exactly once
                    (let* ((args (cdr place))
                           (temps (mapcar (lambda (x) (declare (ignore x)) (gensym "TEMP")) args))
                           (s (gensym "STORE"))
                           (temp-place (cons (car place) temps)))
                      (values temps args (list s)
                              (funcall exp-fn temp-place s)
                              temp-place))
                    ;; Try macroexpanding the place before falling back
                    (let ((expander (and (symbolp (car place))
                                         (find-macro-expander (car place)))))
                      (if expander
                          (%get-setf-expansion (funcall expander place))
                          ;; Fallback: no temp binding, use setf/getter (may re-evaluate)
                          (let ((s (gensym "STORE")))
                            (values nil nil (list s) `(setf ,place ,s) place)))))))))))
    (t (error "get-setf-expansion: unknown place: ~s" place))))

;;; --- %dmm-helper: runtime helper for define-modify-macro generated macros ---
;;; Expands (name place extra-args) to a proper read-modify-write form.
;;; fn is the function symbol (e.g. '+), extra-args is a list of additional arg forms.
(defun %dmm-helper (place fn extra-args)
  (multiple-value-bind (temps vals stores setter getter)
      (%get-setf-expansion place)
    (let ((store (car stores))
          (computation (list* fn getter extra-args)))
      `(let* ,(mapcar #'list temps vals)
         (let ((,store ,computation))
           ,setter)))))

;;; --- push ---
(setf (gethash 'push *macros*)
      (lambda (form)
        (let ((item (cadr form))
              (place (caddr form)))
          (multiple-value-bind (temps vals stores setter getter)
              (%get-setf-expansion place)
            (let ((item-var (gensym "ITEM")))
              `(let* ((,item-var ,item)
                      ,@(mapcar #'list temps vals))
                 (let ((,(car stores) (cons ,item-var ,getter)))
                   ,setter
                   ,(car stores))))))))

;;; --- pushnew ---
(setf (gethash 'pushnew *macros*)
      (lambda (form)
        ;; (pushnew item place &key test test-not key)
        (let ((item (cadr form))
              (place (caddr form))
              (keys (cdddr form)))
          (multiple-value-bind (temps vals stores setter getter)
              (%get-setf-expansion place)
            (let ((item-var (gensym "ITEM")))
              `(let* ((,item-var ,item)
                      ,@(mapcar #'list temps vals))
                 (let ((,(car stores) (adjoin ,item-var ,getter ,@keys)))
                   ,setter
                   ,(car stores))))))))

;;; --- remf ---
(setf (gethash 'remf *macros*)
      (lambda (form)
        ;; (remf place indicator) → remove first pair with indicator from plist
        (let ((place (cadr form))
              (indicator (caddr form)))
          (multiple-value-bind (temps vals stores setter getter)
              (%get-setf-expansion place)
            (let ((ind-var (gensym "IND"))
                  (plist-var (gensym "PLIST")))
              `(let* (,@(mapcar #'list temps vals)
                      (,ind-var ,indicator)
                      (,plist-var ,getter))
                 (cond
                   ((null ,plist-var) nil)
                   ((eq (car ,plist-var) ,ind-var)
                    (let ((,(car stores) (cddr ,plist-var)))
                      ,setter)
                    t)
                   (t
                    (do ((tail ,plist-var (cddr tail)))
                        ((null (cddr tail)) nil)
                      (when (eq (caddr tail) ,ind-var)
                        (rplacd (cdr tail) (cddddr tail))
                        (return t)))))))))))

;;; --- pop ---
(setf (gethash 'pop *macros*)
      (lambda (form)
        (let ((place (cadr form)))
          (multiple-value-bind (temps vals stores setter getter)
              (%get-setf-expansion place)
            (let ((result-var (gensym "RESULT")))
              `(let* (,@(mapcar #'list temps vals))
                 (let* ((,result-var (car ,getter))
                        (,(car stores) (cdr ,getter)))
                   ,setter
                   ,result-var)))))))

;;; --- rotatef ---
;;; (rotatef p1 p2 ... pN) — cyclically rotates values through N places.
;;; p1 gets old p2, p2 gets old p3, ..., pN gets old p1. Returns nil.
(setf (gethash 'rotatef *macros*)
      (lambda (form)
        (let ((places (cdr form)))
          (if (null places)
              'nil
              (let* ((expansions
                      (mapcar (lambda (p)
                                (multiple-value-list (%get-setf-expansion p)))
                              places))
                     (save-vars
                      (mapcar (lambda (p) (declare (ignore p)) (gensym "OLD"))
                              places))
                     (all-bindings
                      (apply #'append
                             (mapcar (lambda (exp)
                                       (mapcar #'list (first exp) (second exp)))
                                     expansions)))
                     (save-bindings
                      (mapcar (lambda (sv exp) (list sv (fifth exp)))
                              save-vars expansions))
                     ;; rotate: place[i] <- old-val[i+1 mod N]
                     (rotated-saves (append (cdr save-vars) (list (car save-vars))))
                     (assignments
                      (mapcar (lambda (exp rotated)
                                `(let ((,(car (third exp)) ,rotated))
                                   ,(fourth exp)))
                              expansions rotated-saves)))
                `(let* (,@all-bindings ,@save-bindings)
                   ,@assignments
                   nil))))))

;;; --- shiftf ---
;;; (shiftf p1 p2 ... pN newval)
;;; Shifts values: p1←old(p2), p2←old(p3), ..., pN←newval.
;;; Returns old value of p1 (single value).
(setf (gethash 'shiftf *macros*)
      (lambda (form)
        (let* ((args (cdr form))
               (places (butlast args))
               (newval (car (last args)))
               (expansions
                (mapcar (lambda (p)
                          (multiple-value-list (%get-setf-expansion p)))
                        places))
               (all-bindings
                (apply #'append
                       (mapcar (lambda (exp)
                                 (mapcar #'list (first exp) (second exp)))
                               expansions)))
               (save-vars
                (mapcar (lambda (p) (declare (ignore p)) (gensym "OLD"))
                        places))
               (save-bindings
                (mapcar (lambda (sv exp) (list sv (fifth exp)))
                        save-vars expansions))
               ;; Each place gets old value of next; last gets newval
               (shifted-values (append (cdr save-vars) (list newval)))
               (assignments
                (mapcar (lambda (exp sv)
                          (let ((stores (third exp))
                                (setter (fourth exp)))
                            (if (cdr stores)
                                `(multiple-value-bind ,stores ,sv ,setter)
                                `(let ((,(car stores) ,sv)) ,setter))))
                        expansions shifted-values)))
          `(let* (,@all-bindings ,@save-bindings)
             ,@assignments
             ,(car save-vars)))))

;;; --- incf ---
(setf (gethash 'incf *macros*)
      (lambda (form)
        (let* ((place (cadr form))
               (delta (if (cddr form) (caddr form) 1))
               (exp (multiple-value-list (%get-setf-expansion place)))
               (temps (first exp))
               (vals  (second exp))
               (stores (third exp))
               (setter (fourth exp))
               (getter (fifth exp))
               (delta-tmp (gensym "INCF-DELTA")))
          `(let* (,@(mapcar #'list temps vals)
                  (,delta-tmp ,delta)
                  (,(car stores) (+ ,getter ,delta-tmp)))
             ,setter))))

;;; --- decf ---
(setf (gethash 'decf *macros*)
      (lambda (form)
        (let* ((place (cadr form))
               (delta (if (cddr form) (caddr form) 1))
               (exp (multiple-value-list (%get-setf-expansion place)))
               (temps (first exp))
               (vals  (second exp))
               (stores (third exp))
               (setter (fourth exp))
               (getter (fifth exp))
               (delta-tmp (gensym "DECF-DELTA")))
          `(let* (,@(mapcar #'list temps vals)
                  (,delta-tmp ,delta)
                  (,(car stores) (- ,getter ,delta-tmp)))
             ,setter))))

;;; --- prog1 ---
(setf (gethash 'prog1 *macros*)
      (lambda (form)
        (let ((tmp (gensym "P1")))
          `(let ((,tmp ,(cadr form)))
             ,@(cddr form)
             ,tmp))))

;;; --- prog2 ---
(setf (gethash 'prog2 *macros*)
      (lambda (form)
        (let ((tmp (gensym "P2")))
          `(progn
             ,(cadr form)
             (let ((,tmp ,(caddr form)))
               ,@(cdddr form)
               ,tmp)))))

;;; --- return ---
(setf (gethash 'return *macros*)
      (lambda (form)
        `(return-from nil ,(cadr form))))

;;; --- extract-declarations ---
;;; Returns two values: list of (declare ...) forms, and remaining body.
(defun extract-declarations (body)
  (let ((decls nil)
        (rest body))
    (loop
      (if (and rest (consp (car rest)) (eq (caar rest) 'declare))
          (progn (push (car rest) decls)
                 (setq rest (cdr rest)))
          (return (values (nreverse decls) rest))))))

;;; --- do ---
(setf (gethash 'do *macros*)
      (lambda (form)
        ;; (do ((var init step) ...) (end-test result...) body...)
        ;; var-clause can be: (var init step), (var init), (var), or bare var
        (let* ((var-clauses (mapcar (lambda (vc) (if (symbolp vc) (list vc nil) vc))
                                    (cadr form)))
               (end-clause (caddr form))
               (body (cdddr form))
               (end-test (car end-clause))
               (result-forms (cdr end-clause))
               (loop-tag (gensym "DO-LOOP"))
               (vars (mapcar #'car var-clauses))
               (inits (mapcar #'cadr var-clauses))
               (has-step (mapcar (lambda (vc) (not (null (cddr vc)))) var-clauses))
               (steps (mapcar (lambda (vc) (if (cddr vc) (caddr vc) nil)) var-clauses))
               ;; For parallel step: use temporaries
               (temps (mapcar (lambda (v) (if (find v var-clauses :key #'car :test #'eq)
                                              (gensym (symbol-name v))
                                              nil))
                              vars)))
          (multiple-value-bind (decls real-body) (extract-declarations body)
            `(block nil
               (let ,(loop for v in vars for i in inits collect (list v i))
                 ,@decls
                 (tagbody
                  ,loop-tag
                  (if ,end-test
                      (return ,(if result-forms `(progn ,@result-forms) nil)))
                  ,@real-body
                  ,@(let ((stepping (loop for v in vars
                                          for s in steps
                                          for hs in has-step
                                          when hs collect (list v s))))
                      (cond
                        ((null stepping) nil)
                        ;; Single stepping variable: no parallel needed
                        ((null (cdr stepping))
                         `((setq ,(caar stepping) ,(cadar stepping))))
                        ;; Multiple stepping variables: parallel via temps
                        (t `((let ,(loop for v in vars
                                         for s in steps
                                         for hs in has-step
                                         for tmp in temps
                                         when hs collect `(,tmp ,s))
                               ,@(loop for v in vars
                                        for s in steps
                                        for hs in has-step
                                        for tmp in temps
                                        when hs collect `(setq ,v ,tmp)))))))
                  (go ,loop-tag))))))))

;;; --- do* ---
(setf (gethash 'do* *macros*)
      (lambda (form)
        ;; (do* ((var init step) ...) (end-test result...) body...)
        ;; var-clause can be: (var init step), (var init), (var), or bare var
        (let* ((var-clauses (mapcar (lambda (vc) (if (symbolp vc) (list vc nil) vc))
                                    (cadr form)))
               (end-clause (caddr form))
               (body (cdddr form))
               (end-test (car end-clause))
               (result-forms (cdr end-clause)))
          (let ((loop-tag (gensym "DO*-LOOP")))
            (multiple-value-bind (decls real-body) (extract-declarations body)
              `(block nil
                 (let* ,(mapcar (lambda (vc) (list (car vc) (cadr vc))) var-clauses)
                   ,@decls
                   (tagbody
                    ,loop-tag
                    (if ,end-test
                        (return ,(if result-forms `(progn ,@result-forms) nil)))
                    ,@real-body
                    ,@(loop for vc in var-clauses
                            when (cddr vc)
                            collect `(setq ,(car vc) ,(caddr vc)))
                    (go ,loop-tag)))))))))

;;; --- dolist ---
(setf (gethash 'dolist *macros*)
      (lambda (form)
        ;; (dolist (var list-form &optional result) body...)
        (let* ((var-clause (cadr form))
               (var (car var-clause))
               (list-form (cadr var-clause))
               (result-form (if (cddr var-clause) (caddr var-clause) nil))
               (body (cddr form))
               (list-var (gensym "LIST"))
               (loop-tag (gensym "DOLIST-LOOP")))
          (multiple-value-bind (decls real-body) (extract-declarations body)
            `(block nil
               (let ((,list-var ,list-form)
                     (,var nil))
                 ,@decls
                 (tagbody
                  ,loop-tag
                  (if (null ,list-var)
                      (progn (setq ,var nil)
                             (return ,(or result-form nil))))
                  (setq ,var (car ,list-var))
                  (setq ,list-var (cdr ,list-var))
                  ,@real-body
                  (go ,loop-tag))))))))

;;; --- dotimes ---
(setf (gethash 'dotimes *macros*)
      (lambda (form)
        ;; (dotimes (var count-form &optional result) body...)
        (let* ((var-clause (cadr form))
               (var (car var-clause))
               (count-form (cadr var-clause))
               (result-form (if (cddr var-clause) (caddr var-clause) nil))
               (body (cddr form))
               (limit-var (gensym "LIMIT"))
               (loop-tag (gensym "DOTIMES-LOOP"))
               ;; D670: when count-form is statically fixnum, inject fixnum
               ;; declarations so the generated compare/increment hits the
               ;; native int64 path introduced in D667-D669. Conservative:
               ;; only when count-form is statically known fixnum; otherwise
               ;; bignum inputs would InvalidCastException on unbox.
               (fx-count (fixnum-typed-p count-form))
               (fx-decl (when fx-count
                          `((declare (fixnum ,limit-var ,var))))))
          (multiple-value-bind (decls real-body) (extract-declarations body)
            `(block nil
               (let ((,limit-var ,count-form)
                     (,var 0))
                 ,@fx-decl
                 ,@decls
                 (tagbody
                  ,loop-tag
                  (if (>= ,var ,limit-var)
                      (return ,(or result-form nil)))
                  ,@real-body
                  (setq ,var (1+ ,var))
                  (go ,loop-tag))))))))

;;; --- multiple-value-bind ---
(setf (gethash 'multiple-value-bind *macros*)
      (lambda (form)
        ;; (multiple-value-bind (v1 v2 ...) expr body...)
        (let* ((vars (cadr form))
               (value-form (caddr form))
               (body (cdddr form))
               (mvl-var (gensym "MVL")))
          `(let* ((,mvl-var (multiple-value-list ,value-form))
                  ,@(loop for v in vars
                          for i from 0
                          collect `(,v (nth ,i ,mvl-var))))
             ,@body))))

;;; --- typecase ---
(setf (gethash 'typecase *macros*)
      (lambda (form)
        ;; (typecase keyform (type1 body1...) (type2 body2...) ...)
        (let ((key-var (gensym "TC"))
              (clauses (cddr form)))
          `(let ((,key-var ,(cadr form)))
             (cond
               ,@(mapcar
                  (lambda (clause)
                    (let ((type (car clause))
                          (body (cdr clause)))
                      (let ((result (if (null body) '(nil) body)))
                        (cond
                          ((member type '(t otherwise) :test #'eq)
                           `(t ,@result))
                          (t
                           ;; type may be a class object (from #.) or a type symbol
                           `((typep ,key-var ',type) ,@result))))))
                  clauses))))))

;;; --- etypecase ---
(setf (gethash 'etypecase *macros*)
      (lambda (form)
        ;; (etypecase keyform (type1 body1...) ...)
        ;; Like typecase but signals type-error if no clause matches
        (let ((key-var (gensym "ETC"))
              (clauses (cddr form)))
          (let ((all-types (mapcar #'car clauses)))
            `(let ((,key-var ,(cadr form)))
               (cond
                 ,@(mapcar
                    (lambda (clause)
                      (let ((type (car clause))
                            (body (cdr clause)))
                        (let ((result (if (null body) '(nil) body)))
                          `((typep ,key-var ',type) ,@result))))
                    clauses)
                 (t (error 'type-error :datum ,key-var
                           :expected-type '(or ,@all-types)))))))))

;;; --- ctypecase ---
(setf (gethash 'ctypecase *macros*)
      (lambda (form)
        ;; Correctable typecase: signals type-error with STORE-VALUE restart
        (let ((key-var (gensym "CTC"))
              (keyform (cadr form))
              (clauses (cddr form)))
          (let ((all-types (mapcar #'car clauses)))
            `(let ((,key-var ,keyform))
               (loop
                 (cond
                   ,@(mapcar
                      (lambda (clause)
                        (let ((type (car clause))
                              (body (cdr clause)))
                          `((typep ,key-var ',type) (return (progn ,@body)))))
                      clauses)
                   (t (restart-case
                        (error 'type-error :datum ,key-var
                               :expected-type '(or ,@all-types))
                        (store-value (new-val)
                          :report "Supply a new value"
                          (setf ,key-var new-val)
                          (setf ,keyform ,key-var)))))))))))

;;; cerror is a function, not a macro (registered in Startup.cs)

;;; --- with-simple-restart ---
(setf (gethash 'with-simple-restart *macros*)
      (lambda (form)
        ;; (with-simple-restart (name description) body...)
        ;; → (restart-case (progn body...) (name () (values nil t)))
        (let* ((restart-spec (cadr form))
               (name (car restart-spec))
               (body (cddr form)))
          `(restart-case (progn ,@body)
             (,name () (values nil t))))))

;;; --- with-condition-restarts ---
(setf (gethash 'with-condition-restarts *macros*)
      (lambda (form)
        ;; (with-condition-restarts condition-form restarts-form body...)
        (let ((condition-form (cadr form))
              (restarts-form (caddr form))
              (body (cdddr form)))
          `(let ((%wcr-condition ,condition-form)
                 (%wcr-restarts ,restarts-form))
             (%associate-condition-restarts %wcr-condition %wcr-restarts)
             (unwind-protect
                 (progn ,@body)
               (%disassociate-condition-restarts %wcr-condition %wcr-restarts))))))

;;; --- ignore-errors ---
(setf (gethash 'ignore-errors *macros*)
      (lambda (form)
        ;; (ignore-errors body...) → (handler-case (progn body...) (error (c) (values nil c)))
        `(handler-case (progn ,@(cdr form))
           (error (c) (values nil c)))))

;;; --- check-type ---
(setf (gethash 'check-type *macros*)
      (lambda (form)
        ;; (check-type place type &optional string)
        (let ((place (cadr form))
              (type (caddr form))
              (type-string (cadddr form))
              (tag (gensym "CHECK-TYPE-")))
          `(block nil
             (tagbody ,tag
               (when (typep ,place ',type) (return nil))
               (restart-case
                 (error 'type-error :datum ,place :expected-type ',type)
                 (store-value (value)
                   :report ,(if type-string
                                `(lambda (s) (format s "Supply a new value of type ~A." ,type-string))
                                `(lambda (s) (format s "Supply a new value of type ~S." ',type)))
                   :interactive (lambda () (list (eval (read))))
                   (setf ,place value)))
               (go ,tag))))))

;;; --- defstruct ---
;; find-defstruct-option: look up an option keyword in defstruct options list.
;; Options can be bare keywords (:constructor) or lists ((:constructor name)).
;; Returns:
;;   nil      - option not present
;;   :bare    - bare keyword or list with no args: :constructor or (:constructor)
;;   the list - option with args: (:constructor name)
(defun find-defstruct-option (key options)
  (let ((result nil))
    (dolist (opt options)
      (cond
        ((and (symbolp opt) (eq opt key))
         (setf result :bare)
         (return))
        ((and (consp opt) (eq (car opt) key))
         (if (null (cdr opt))
             (setf result :bare)
             (setf result opt))
         (return))))
    result))

;; find-all-defstruct-options: find ALL occurrences of an option keyword.
;; Returns a list of results, each being :bare or the option list.
(defun find-all-defstruct-options (key options)
  (let ((results nil))
    (dolist (opt options)
      (cond
        ((and (symbolp opt) (eq opt key))
         (push :bare results))
        ((and (consp opt) (eq (car opt) key))
         (if (null (cdr opt))
             (push :bare results)
             (push opt results)))))
    (nreverse results)))

;; Parse a BOA lambda list into components.
;; Returns (required optional rest-var key allow-other-keys aux)
;; where optional is list of (name default), key is list of (name default),
;; aux is list of (name value).
(defun parse-boa-lambda-list (lambda-list)
  (let ((required nil) (optional nil) (rest-var nil) (key-params nil)
        (allow-other-keys nil) (aux nil) (state :required))
    (dolist (item lambda-list)
      (cond
        ((eq item '&optional) (setf state :optional))
        ((eq item '&rest) (setf state :rest))
        ((eq item '&key) (setf state :key))
        ((eq item '&allow-other-keys) (setf allow-other-keys t))
        ((eq item '&aux) (setf state :aux))
        (t (case state
             (:required (push item required))
             (:optional (if (consp item)
                            (push (list (car item) (cadr item)) optional)
                            (push (list item nil) optional)))
             (:rest (setf rest-var item))
             (:key (if (consp item)
                       (push (list (car item) (cadr item) (caddr item)) key-params)
                       (push (list item nil) key-params)))
             (:aux (if (consp item)
                       (push (list (car item) (cadr item)) aux)
                       (push (list item nil) aux)))))))
    (list (nreverse required) (nreverse optional) rest-var
          (nreverse key-params) allow-other-keys (nreverse aux))))

;; Generate a BOA constructor defun.
;; boa-lambda-list is the raw lambda list from (:constructor name (params...))
;; slots is list of (slot-name default-value)
;; struct-name is the struct type name
(defun generate-boa-constructor (ctor-name boa-lambda-list slots struct-name
                                  &optional type-option named-p initial-offset)
  (let* ((parsed (parse-boa-lambda-list boa-lambda-list))
         (required (car parsed))
         (optional (cadr parsed))
         (rest-var (caddr parsed))
         (key-params (cadddr parsed))
         ;; allow-other-keys is (car (cddddr parsed))
         (aux (cadr (cddddr parsed)))
         ;; Collect all param names mentioned in the lambda list
         (all-param-names (append required
                                  (mapcar #'car optional)
                                  (when rest-var (list rest-var))
                                  (mapcar (lambda (kp)
                                            (let ((spec (car kp)))
                                              (if (consp spec) (cadr spec) spec)))
                                          key-params)
                                  (remove nil (mapcar #'caddr key-params))
                                  (mapcar #'car aux)))
         ;; Build the actual defun lambda list.
         ;; For &optional and &key params that have no explicit default in the BOA list,
         ;; use the slot's defstruct default.
         (defun-lambda-list
           (let ((result nil) (state :required))
             (dolist (item boa-lambda-list)
               (cond
                 ((eq item '&optional) (setf state :optional) (push item result))
                 ((eq item '&rest) (setf state :rest) (push item result))
                 ((eq item '&key) (setf state :key) (push item result))
                 ((eq item '&allow-other-keys) (push item result))
                 ((eq item '&aux) (setf state :aux) (push item result))
                 (t (case state
                      (:required (push item result))
                      (:optional
                       (if (consp item)
                           (push item result)
                           ;; bare optional param - use slot default
                           (let ((slot (assoc item slots)))
                             (if (and slot (cadr slot))
                                 (push (list item (cadr slot)) result)
                                 (push item result)))))
                      (:rest (push item result))
                      (:key
                       (if (consp item)
                           (push item result)
                           ;; bare key param - use slot default
                           (let ((slot (assoc item slots)))
                             (if (and slot (cadr slot))
                                 (push (list item (cadr slot)) result)
                                 (push item result)))))
                      (:aux
                       (if (consp item)
                           (push item result)
                           ;; bare aux param - use slot default
                           (let ((slot (assoc item slots)))
                             (if (and slot (cadr slot))
                                 (push (list item (cadr slot)) result)
                                 (push item result)))))))))
             (nreverse result)))
         ;; Build constructor args in slot definition order
         (slot-values
           (mapcar (lambda (s)
                     (let ((slot-name (car s))
                           (slot-default (cadr s)))
                       (if (member slot-name all-param-names)
                           slot-name
                           slot-default)))
                   slots))
         (ioffset (or initial-offset 0))
         ;; Build the constructor body based on type
         (body
           (cond
             ((null type-option)
              ;; Standard struct
              `(%make-struct ',struct-name ,@slot-values))
             ((eq type-option 'list)
              ;; List-based typed struct
              `(list ,@(make-list ioffset :initial-element nil)
                     ,@(when named-p (list `',struct-name))
                     ,@slot-values))
             (t
              ;; Vector-based typed struct
              (let ((all-elems `(,@(make-list ioffset :initial-element nil)
                                ,@(when named-p (list `',struct-name))
                                ,@slot-values))
                    (elem-type (when (and (consp type-option) (consp (cdr type-option)))
                                 (cadr type-option))))
                (if elem-type
                    `(make-array ,(length all-elems)
                                :element-type ',elem-type
                                :initial-contents (list ,@all-elems))
                    `(vector ,@all-elems)))))))
    `(defun ,ctor-name ,defun-lambda-list
       ,body)))

(setf (gethash 'defstruct *macros*)
      (lambda (form)
        ;; (defstruct name slot1 (slot2 default2) ...)
        ;; name can be a symbol or (name options...) with various option forms
        (let* ((name-spec (cadr form))
               (name (if (consp name-spec) (car name-spec) name-spec))
               (name-str (symbol-name name))
               ;; Parse options from (name (:conc-name prefix) (:constructor name) ...)
               ;; Options can be bare keywords or lists
               (options (when (consp name-spec) (cdr name-spec)))
               (conc-raw (find-defstruct-option :conc-name options))
               (ctor-raw (find-defstruct-option :constructor options))
               (all-ctors (find-all-defstruct-options :constructor options))
               (copier-raw (find-defstruct-option :copier options))
               (pred-raw (find-defstruct-option :predicate options))
               (include-raw (find-defstruct-option :include options))
               (type-raw (find-defstruct-option :type options))
               (named-raw (find-defstruct-option :named options))
               (offset-raw (find-defstruct-option :initial-offset options))
               ;; Parse :type option
               ;; (:type list) -> LIST, (:type vector) -> VECTOR, (:type (vector bit)) -> (VECTOR BIT)
               (type-option (when type-raw
                              (if (eq type-raw :bare) nil
                                  (let ((tval (cadr type-raw)))
                                    (cond ((eq tval 'list) 'list)
                                          ((eq tval 'vector) 'vector)
                                          ((and (consp tval) (eq (car tval) 'vector)) tval)
                                          (t nil))))))
               (named-p (not (null named-raw)))
               (initial-offset (if (and offset-raw (not (eq offset-raw :bare)))
                                   (cadr offset-raw)
                                   0))
               ;; Base offset for slot indices: initial-offset + 1 if named
               (base-offset (+ (or initial-offset 0) (if named-p 1 0)))
               (raw-slots-with-doc (cddr form))
               ;; Filter out documentation string (first element if it's a string)
               (raw-slots (if (and raw-slots-with-doc
                                   (stringp (car raw-slots-with-doc)))
                              (cdr raw-slots-with-doc)
                              raw-slots-with-doc))
               ;; Parse slots: each is either symbol or (symbol default :key val ...)
               ;; Result: (name default :read-only ro-flag)
               (slots (mapcar (lambda (s)
                                (if (consp s)
                                    (list (car s) (cadr s)
                                          :read-only (getf (cddr s) :read-only))
                                    (list s nil :read-only nil)))
                              raw-slots))
               ;; Parse :include option for struct inheritance
               (include-name (when (and include-raw (not (eq include-raw :bare)))
                               (cadr include-raw)))
               (include-overrides (when include-name
                                    (cddr include-raw)))
               (include-info (when include-name
                               (gethash (symbol-name include-name) *struct-info*)))
               (include-parent-conc-prefix (when include-info
                                             (getf include-info :conc-prefix)))
               ;; Parent slots from :include, with overrides applied
               (include-slots
                 (when include-info
                   (let ((parent-slots (mapcar #'copy-list (getf include-info :slots))))
                     ;; Apply overrides from (:include parent (slot1 new-default) ...)
                     (dolist (override include-overrides)
                       (let* ((oname (if (consp override) (car override) override))
                              (odefault (if (consp override) (cadr override) nil))
                              (entry (assoc oname parent-slots)))
                         (when entry
                           (setf (cadr entry) odefault))))
                     parent-slots)))
               ;; For typed structs with :include, retrieve parent's layout info
               (parent-type-option (when include-info
                                     (getf include-info :type-option)))
               (parent-named-p (when include-info
                                 (getf include-info :named-p)))
               (parent-initial-offset (when include-info
                                        (getf include-info :initial-offset)))
               (parent-base-offset (when include-info
                                     (getf include-info :base-offset)))
               ;; All slots = inherited + own
               (all-slots (append (or include-slots nil) slots))
               (n-slots (length all-slots))
               ;; For typed structs with :include, recalculate effective base-offset
               ;; and compute per-slot accessor indices
               ;; Parent occupies: [parent-base-offset ... parent-base-offset + parent-slot-count - 1]
               ;; Child's additional padding: initial-offset positions
               ;; Child's named tag: 1 position if named-p
               ;; Child's slots start after all of the above
               (n-include-slots (length (or include-slots nil)))
               (typed-include-p (and type-option include-name include-info parent-base-offset))
               ;; For typed struct with :include, the child's own slots start at:
               ;; parent-base-offset + parent-slot-count + child-initial-offset + child-named-p
               (child-slot-start (if typed-include-p
                                     (+ parent-base-offset n-include-slots
                                        (or initial-offset 0)
                                        (if named-p 1 0))
                                     nil))
               ;; For typed structs with :include, retrieve the parent's ctor-layout
               ;; which is the full constructor body elements for the parent
               (parent-ctor-layout (when include-info
                                     (getf include-info :ctor-layout)))
               ;; Compute the full constructor body layout for typed structs
               ;; This is a list of elements that go into (list ...) or (vector ...)
               ;; Each element is either: nil (padding), (:name SYM) (named tag), (:slot SYM) (slot var)
               (ctor-layout
                 (when type-option
                   (if typed-include-p
                       ;; With :include: parent's full layout + child's padding + child's name + child's slots
                       (append (or parent-ctor-layout nil)
                               (make-list (or initial-offset 0) :initial-element nil)
                               (when named-p (list (list :name name)))
                               (mapcar (lambda (s) (list :slot (car s))) slots))
                       ;; Without :include: simple layout
                       (append (make-list (or initial-offset 0) :initial-element nil)
                               (when named-p (list (list :name name)))
                               (mapcar (lambda (s) (list :slot (car s))) slots)))))
               ;; Pre-compute index for each slot in all-slots
               (slot-indices (if typed-include-p
                                 ;; Use ctor-layout to find indices of all slots
                                 (let ((indices nil))
                                   (loop for elem in ctor-layout
                                         for pos from 0
                                         when (and (consp elem) (eq (car elem) :slot))
                                         do (push pos indices))
                                   (nreverse indices))
                                 ;; Non-include or non-typed: simple offset
                                 (loop for i from 0 below (length all-slots)
                                       collect (+ i base-offset))))
               ;; Generate accessor/constructor/predicate/copy names
               ;; conc-name: not present -> "NAME-", :bare or (:conc-name nil) -> "",
               ;;            (:conc-name prefix) -> use prefix
               (conc-prefix (cond
                              ((null conc-raw)
                               (concatenate 'string name-str "-"))
                              ((eq conc-raw :bare) "")
                              (t (let ((v (cadr conc-raw)))
                                   (cond ((null v) "")
                                         ((stringp v) v)
                                         ((characterp v) (string v))
                                         (t (symbol-name v)))))))
               ;; Constructor processing: generate list of constructor forms
               ;; Each entry is either:
               ;;   (:keyword name) - standard &key constructor
               ;;   (:boa name lambda-list) - BOA constructor
               ;;   nil - suppressed
               ;; Note: make-name kept for backward compat but constructors
               ;; are now generated from constructor-specs below
               (make-name nil)  ; will be set below, not used for generation
               (constructor-specs
                 (if (null all-ctors)
                     ;; No :constructor option -> default keyword constructor
                     (list (list :keyword (intern (concatenate 'string "MAKE-" name-str))))
                     ;; Process each :constructor option
                     (let ((specs nil))
                       (dolist (c all-ctors)
                         (cond
                           ;; :bare = (:constructor) -> default keyword constructor
                           ((eq c :bare)
                            (push (list :keyword (intern (concatenate 'string "MAKE-" name-str))) specs))
                           ;; (:constructor nil) -> suppress
                           ((null (cadr c))
                            (push nil specs))
                           ;; (:constructor name) with no lambda list -> keyword constructor
                           ((null (cddr c))
                            (push (list :keyword (cadr c)) specs))
                           ;; (:constructor name lambda-list) -> BOA
                           (t
                            (push (list :boa (cadr c) (caddr c)) specs))))
                       (nreverse specs))))
               ;; copier: not present or :bare -> default, (:copier nil) -> no copier,
               ;;         (:copier name) -> custom name
               (copy-name (cond
                            ((null copier-raw)
                             (intern (concatenate 'string "COPY-" name-str)))
                            ((eq copier-raw :bare)
                             (intern (concatenate 'string "COPY-" name-str)))
                            ((null (cadr copier-raw)) nil)
                            (t (cadr copier-raw))))
               ;; predicate: not present or :bare -> default, (:predicate nil) -> no pred,
               ;;            (:predicate name) -> custom name
               (pred-name (cond
                            ((null pred-raw)
                             (intern (concatenate 'string name-str "-P")))
                            ((eq pred-raw :bare)
                             (intern (concatenate 'string name-str "-P")))
                            ((null (cadr pred-raw)) nil)
                            (t (cadr pred-raw))))
               ;; Accessor names for inherited slots use CHILD's conc-prefix
               ;; (parent already defined its own accessors)
               (include-accessor-names
                 (when include-slots
                   (mapcar (lambda (s)
                             (intern (concatenate 'string conc-prefix
                                                  (symbol-name (car s)))))
                           include-slots)))
               ;; Accessor names for own slots use this struct's conc-prefix
               (own-accessor-names (mapcar (lambda (s)
                                             (intern (concatenate 'string conc-prefix
                                                                  (symbol-name (car s)))))
                                           slots))
               ;; All accessor names in order (inherited first, then own)
               (accessor-names (append (or include-accessor-names nil) own-accessor-names)))
          ;; Error: :predicate with :type (without :named) is invalid per CLHS
          (when (and type-option pred-raw pred-name (not named-p))
            (error "~A is an invalid DEFSTRUCT option for a typed structure." :predicate))
          ;; Error: :named with :type (vector element-type) where element-type
          ;; cannot hold a symbol (e.g., single-float, double-float, bit, (unsigned-byte n))
          (when (and named-p (consp type-option) (eq (car type-option) 'vector))
            (let ((et (cadr type-option)))
              (unless (or (null et) (eq et t) (eq et 'character))
                (error "DEFSTRUCT: :NAMED requires element-type to hold a symbol, but got ~S" et))))
          ;; Register setf expanders as side-effect of macro expansion
          (loop for acc in accessor-names
                for idx in slot-indices
                for i from 0
                do (let ((index idx)
                         (raw-index i))
                     ;; Register for compile-time struct accessor inlining (non-typed structs only)
                     (when (null type-option)
                       (setf (gethash (symbol-name acc) *struct-accessors*) raw-index))
                     (setf (gethash (symbol-name acc) *setf-expanders*)
                           (cond
                             ((null type-option)
                              ;; Standard struct: use %struct-set with raw index
                              (lambda (place value)
                                (let ((tmp (gensym "V")))
                                  `(let ((,tmp ,value))
                                     (%struct-set ,(cadr place) ,raw-index ,tmp)
                                     ,tmp))))
                             ((eq type-option 'list)
                              ;; List-typed struct: use (setf (nth index obj) val)
                              (lambda (place value)
                                (let ((tmp (gensym "V")))
                                  `(let ((,tmp ,value))
                                     (setf (nth ,index ,(cadr place)) ,tmp)
                                     ,tmp))))
                             (t
                              ;; Vector-typed struct: use (setf (aref obj index) val)
                              (lambda (place value)
                                (let ((tmp (gensym "V")))
                                  `(let ((,tmp ,value))
                                     (setf (aref ,(cadr place) ,index) ,tmp)
                                     ,tmp))))))))
          ;; Store struct info for :include lookups (at macro-expansion time)
          (setf (gethash (symbol-name name) *struct-info*)
                (list :slots (mapcar #'copy-list all-slots)
                      :parent include-name
                      :conc-prefix conc-prefix
                      :type-option type-option
                      :named-p named-p
                      :initial-offset initial-offset
                      :base-offset (if typed-include-p
                                       parent-base-offset
                                       base-offset)
                      :ctor-layout (when ctor-layout
                                     (mapcar #'identity ctor-layout))))
          `(progn
             ;; Register struct type in CLOS class registry (only for non-typed structs)
             ,@(unless type-option
                 `((%register-struct-class ',name ',(or include-name nil) ,@(mapcar (lambda (s) `',(car s)) all-slots))))
             ;; Constructors
             ,@(let ((ctor-forms nil))
                 (dolist (spec constructor-specs)
                   (when spec
                     (cond
                       ((eq (car spec) :keyword)
                        ;; Build &key params and local var names, handling constant slot names (T, NIL, etc.)
                        (let* ((slot-vars (mapcar (lambda (s)
                                                    (let ((sname (car s)))
                                                      (if (constantp sname)
                                                          (gensym (symbol-name sname))
                                                          sname)))
                                                  all-slots))
                               (key-params (mapcar (lambda (s var)
                                                     (if (eq var (car s))
                                                         (list (car s) (cadr s))
                                                         (list (list (intern (symbol-name (car s)) "KEYWORD") var) (cadr s))))
                                                   all-slots slot-vars)))
                          (cond
                            ((null type-option)
                             ;; Standard struct constructor
                             (push `(defun ,(cadr spec) (&key ,@key-params)
                                      (%make-struct ',name ,@slot-vars))
                                   ctor-forms))
                            ((eq type-option 'list)
                             ;; List-typed constructor: use ctor-layout for correct layout
                             (let ((body-elems (mapcar (lambda (elem)
                                                         (cond ((null elem) nil)
                                                               ((and (consp elem) (eq (car elem) :name))
                                                                `',(cadr elem))
                                                               ((and (consp elem) (eq (car elem) :slot))
                                                                ;; Find the var for this slot
                                                                (let ((pos (position (cadr elem) all-slots :key #'car)))
                                                                  (if pos (nth pos slot-vars) (cadr elem))))
                                                               (t nil)))
                                                       ctor-layout)))
                               (push `(defun ,(cadr spec) (&key ,@key-params)
                                        (list ,@body-elems))
                                     ctor-forms)))
                            (t
                             ;; Vector-typed constructor: use ctor-layout for correct layout
                             (let ((body-elems (mapcar (lambda (elem)
                                                         (cond ((null elem) nil)
                                                               ((and (consp elem) (eq (car elem) :name))
                                                                `',(cadr elem))
                                                               ((and (consp elem) (eq (car elem) :slot))
                                                                (let ((pos (position (cadr elem) all-slots :key #'car)))
                                                                  (if pos (nth pos slot-vars) (cadr elem))))
                                                               (t nil)))
                                                       ctor-layout)))
                               (push `(defun ,(cadr spec) (&key ,@key-params)
                                        ,(if (and (consp type-option) (consp (cdr type-option)))
                                             `(make-array ,(length body-elems)
                                                          :element-type ',(cadr type-option)
                                                          :initial-contents (list ,@body-elems))
                                             `(vector ,@body-elems)))
                                     ctor-forms))))))
                       ((eq (car spec) :boa)
                        (push (generate-boa-constructor (cadr spec) (caddr spec) all-slots name
                                                        type-option named-p initial-offset)
                              ctor-forms)))))
                 (nreverse ctor-forms))
             ;; Accessors (for all slots including inherited)
             ,@(loop for s in all-slots
                     for acc in accessor-names
                     for idx in slot-indices
                     for i from 0
                     collect (cond
                               ((null type-option)
                                `(defun ,acc (obj) (%struct-ref obj ,i)))
                               ((eq type-option 'list)
                                `(defun ,acc (obj) (nth ,idx obj)))
                               (t
                                `(defun ,acc (obj) (aref obj ,idx)))))
             ;; Setf accessor functions (skip :read-only slots)
             ,@(loop for s in all-slots
                     for acc in accessor-names
                     for idx in slot-indices
                     for i from 0
                     unless (getf (cddr s) :read-only)
                     collect (cond
                               ((null type-option)
                                `(defun (setf ,acc) (value obj) (%struct-set obj ,i value) value))
                               ((eq type-option 'list)
                                `(defun (setf ,acc) (value obj) (setf (nth ,idx obj) value)))
                               (t
                                `(defun (setf ,acc) (value obj) (setf (aref obj ,idx) value)))))
             ;; Predicate
             ,@(cond
                 ;; Standard struct: always generate predicate if pred-name
                 ((null type-option)
                  (when pred-name
                    `((defun ,pred-name (obj) (%struct-typep obj ',name)))))
                 ;; Typed struct with :named: generate type-specific predicate
                 ((and type-option named-p pred-name)
                  ;; Find the child's own name tag position in ctor-layout
                  (let ((name-pos (if ctor-layout
                                      ;; Search from the end for the child's name tag
                                      (let ((pos nil))
                                        (loop for elem in ctor-layout
                                              for i from 0
                                              when (and (consp elem) (eq (car elem) :name)
                                                        (eq (cadr elem) name))
                                              do (setf pos i))
                                        (or pos (or initial-offset 0)))
                                      (or initial-offset 0))))
                    (if (eq type-option 'list)
                        `((defun ,pred-name (obj)
                            (and (consp obj)
                                 (eq (nth ,name-pos obj) ',name))))
                        `((defun ,pred-name (obj)
                            (and (vectorp obj)
                                 (> (length obj) ,name-pos)
                                 (eq (aref obj ,name-pos) ',name)))))))
                 ;; Typed struct without :named: no predicate
                 (t nil))
             ;; Copier
             ,@(when copy-name
                 (cond
                   ((null type-option)
                    `((defun ,copy-name (obj) (%copy-struct obj))))
                   ((eq type-option 'list)
                    `((defun ,copy-name (obj) (copy-list obj))))
                   (t
                    `((defun ,copy-name (obj) (copy-seq obj))))))
             ;; Return type name
             ',name))))

;;; --- defclass ---
(setf (gethash 'defclass *macros*)
      (lambda (form)
        ;; (defclass name (supers...) ((slot-name :initarg :x :initform val :accessor acc) ...)
        ;;   (:default-initargs :key1 val1 :key2 val2 ...))
        (let* ((name (cadr form))
               (supers-spec (caddr form))
               (slot-specs (cadddr form))
               (class-options (cddddr form))
               ;; Validation 1: Check for duplicate slot names
               (_dup-slot-check
                 (let ((slot-names nil))
                   (dolist (spec (or slot-specs '()))
                     (let ((sname (if (symbolp spec) spec (car spec))))
                       (when (member sname slot-names)
                         (error 'program-error
                                :format-control "DEFCLASS ~S: duplicate slot name ~S"
                                :format-arguments (list name sname)))
                       (push sname slot-names)))))
               ;; Validation 2+3: Check for duplicate/unknown slot options
               (_dup-opt-check
                 (dolist (spec (or slot-specs '()))
                   (when (consp spec)
                     (let ((sname (car spec))
                           (props (cdr spec))
                           (seen-initform nil)
                           (seen-type nil)
                           (seen-doc nil)
                           (seen-alloc nil))
                       (loop while props
                             do (let ((key (pop props))
                                      (val (pop props)))
                                  (cond
                                    ((eq key :initform)
                                     (when seen-initform
                                       (error 'program-error
                                              :format-control "DEFCLASS ~S: duplicate :initform option in slot ~S"
                                              :format-arguments (list name sname)))
                                     (setf seen-initform t))
                                    ((eq key :type)
                                     (when seen-type
                                       (error 'program-error
                                              :format-control "DEFCLASS ~S: duplicate :type option in slot ~S"
                                              :format-arguments (list name sname)))
                                     (setf seen-type t))
                                    ((eq key :documentation)
                                     (when seen-doc
                                       (error 'program-error
                                              :format-control "DEFCLASS ~S: duplicate :documentation option in slot ~S"
                                              :format-arguments (list name sname)))
                                     (setf seen-doc t))
                                    ((eq key :allocation)
                                     (when seen-alloc
                                       (error 'program-error
                                              :format-control "DEFCLASS ~S: duplicate :allocation option in slot ~S"
                                              :format-arguments (list name sname)))
                                     (setf seen-alloc t))
                                    ((eq key :initarg) nil)
                                    ((eq key :reader) nil)
                                    ((eq key :writer) nil)
                                    ((eq key :accessor) nil)
                                    (t
                                     (error 'program-error
                                            :format-control "DEFCLASS ~S: unknown slot option ~S in slot ~S"
                                            :format-arguments (list name key sname))))))))))
               ;; Validation 4: Unknown class options signal program-error (CLHS 7.7 DEFCLASS)
               (_unknown-class-opt-check
                 (dolist (opt class-options)
                   (when (consp opt)
                     (let ((opt-name (car opt)))
                       (unless (member opt-name '(:default-initargs :documentation :metaclass))
                         (error 'program-error
                                :format-control "DEFCLASS ~S: unknown class option ~S"
                                :format-arguments (list name opt-name)))))))
               ;; Validation 5: Check for duplicate :default-initargs keys
               (_dup-initargs-check
                 (dolist (opt class-options)
                   (when (and (consp opt) (eq (car opt) :default-initargs))
                     (let ((pairs (cdr opt))
                           (seen-keys nil))
                       (loop while pairs
                             do (let ((key (pop pairs))
                                      (val (pop pairs)))
                                  (when (member key seen-keys)
                                    (error 'program-error
                                           :format-control "DEFCLASS ~S: duplicate :default-initargs key ~S"
                                           :format-arguments (list name key)))
                                  (push key seen-keys)))))))
               ;; Parse each slot spec
               (parsed-slots
                 (mapcar (lambda (spec)
                           (if (symbolp spec)
                               (list spec nil nil nil nil nil nil nil) ; (name initargs initform-present initform accessor reader writer allocation)
                               (let* ((sname (car spec))
                                      (props (cdr spec))
                                      (initargs nil)
                                      (initform nil)
                                      (initform-present nil)
                                      (accessors nil)
                                      (readers nil)
                                      (writers nil)
                                      (allocation nil))
                                 (loop while props
                                       do (let ((key (pop props))
                                                (val (pop props)))
                                            (cond
                                              ((eq key :initarg) (push val initargs))
                                              ((eq key :initform) (setf initform val) (setf initform-present t))
                                              ((eq key :accessor) (push val accessors))
                                              ((eq key :reader) (push val readers))
                                              ((eq key :writer) (push val writers))
                                              ((eq key :allocation) (setf allocation val)))))
                                 (list sname initargs initform-present initform accessors readers writers allocation))))
                         (or slot-specs '())))
               ;; Parse class options for :default-initargs
               (default-initargs-forms
                 (let ((result nil))
                   (dolist (opt class-options result)
                     (when (and (consp opt) (eq (car opt) :default-initargs))
                       (let ((pairs (cdr opt)))
                         (loop while pairs
                               do (let ((key (pop pairs))
                                        (val (pop pairs)))
                                    (push (list key val) result))))))))
               ;; Build super classes list expression
               (supers-expr (if (null supers-spec)
                                'nil
                                `(list ,@(mapcar (lambda (s) `(%find-or-forward-class ',s)) supers-spec))))
               ;; Build slot-def list expression
               (slotdefs-expr
                 `(list ,@(mapcar (lambda (ps)
                                    (let ((sname (first ps))
                                          (initargs (second ps))
                                          (initform-present (third ps))
                                          (initform (fourth ps))
                                          (allocation (eighth ps)))
                                      (if (eq allocation :class)
                                          `(%make-slot-def-with-allocation
                                            ',sname
                                            ,(if initargs
                                                 `(list ,@(mapcar (lambda (ia) `',ia) (reverse initargs)))
                                                 'nil)
                                            ,(if initform-present
                                                 `(lambda () ,initform)
                                                 'nil)
                                            'class)
                                          `(%make-slot-def
                                            ',sname
                                            ,(if initargs
                                                 `(list ,@(mapcar (lambda (ia) `',ia) (reverse initargs)))
                                                 'nil)
                                            ,(if initform-present
                                                 `(lambda () ,initform)
                                                 'nil)))))
                                  parsed-slots)))
               ;; Accessor definitions
               (accessor-defs
                 (let ((defs nil))
                   (dolist (ps parsed-slots (nreverse defs))
                     (let ((sname (first ps))
                           (accessors (fifth ps))
                           (readers (sixth ps))
                           (writers (seventh ps)))
                       ;; Accessor = reader + writer (use defmethod to work with defgeneric)
                       (dolist (accessor accessors)
                         (push `(defmethod ,accessor ((obj ,name)) (slot-value obj ',sname)) defs)
                         (push `(defmethod (setf ,accessor) ((val t) (obj ,name)) (%set-slot-value obj ',sname val) val) defs)
                         ;; Register setf expander — calls (setf accessor) GF so that
                         ;; :around and other qualifier methods are properly dispatched.
                         (let ((acc accessor))
                           (setf (gethash (%setf-key accessor) *setf-expanders*)
                                 (lambda (place value)
                                   (when (/= (length (cdr place)) 1)
                                     (error 'program-error
                                            :format-control
                                            "Wrong number of arguments for accessor SETF"))
                                   (let ((tmp (gensym "V")))
                                     `(let ((,tmp ,value))
                                        ((setf ,acc) ,tmp ,(cadr place))
                                        ,tmp))))))
                       ;; Readers (use defmethod)
                       (dolist (reader readers)
                         (push `(defmethod ,reader ((obj ,name)) (slot-value obj ',sname)) defs))
                       ;; Writers (use defmethod)
                       (dolist (writer writers)
                         (push `(defmethod ,writer ((val t) (obj ,name)) (%set-slot-value obj ',sname val)) defs))))))
               ;; Build default-initargs setup form
               (default-initargs-form
                 (when default-initargs-forms
                   (let ((args nil))
                     (dolist (pair (reverse default-initargs-forms))
                       (setf args (append args
                                          (list `',(first pair)
                                                `(lambda () ,(second pair))))))
                     `(%set-class-default-initargs
                       (find-class ',name)
                       (list ,@args))))))
          `(progn
             (%register-class (%make-class ',name ,supers-expr ,slotdefs-expr))
             ,@(when default-initargs-form (list default-initargs-form))
             ,@accessor-defs
             (find-class ',name)))))

;;; --- deftype ---
;; Transform deftype lambda-list: unsupplied &optional/&key params default to '* (CLHS 4.2.3)
(defun deftype-default-star (params)
  "Rewrite &optional/&key params without defaults to use '* as default."
  (let ((result '()) (state :required))
    (dolist (p params (nreverse result))
      (cond
        ((member p '(&optional &key &rest &body &whole &environment &aux &allow-other-keys))
         (setf state (if (member p '(&optional &key)) p :other))
         (push p result))
        ((and (member state '(&optional &key)) (symbolp p))
         ;; Bare symbol in &optional/&key: add default '*
         (push (list p ''*) result))
        ((and (member state '(&optional &key)) (consp p) (null (cdr p)))
         ;; (name) without default: add default '*
         (push (list (car p) ''*) result))
        (t (push p result))))))

(setf (gethash 'deftype *macros*)
      (lambda (form)
        ;; (deftype name lambda-list &body body)
        ;; Register a type expander: when (typep x '(name ...)) is evaluated,
        ;; the expander is called with the CDR of the type specifier as args.
        (let* ((name (cadr form))
               (params (deftype-default-star (or (caddr form) '())))
               (body (cdddr form)))
          `(progn
             (%register-type-expander ',name (lambda ,params (block ,name ,@body)))
             ',name))))

;;; --- define-condition ---
(setf (gethash 'define-condition *macros*)
      (lambda (form)
        ;; (define-condition name (parents...) (slots...) &rest options)
        ;; → expand to defclass + return name
        ;; Extract :report option to generate print-object method
        (let* ((name (cadr form))
               (parents (caddr form))
               (parents (or parents '(condition)))
               (slots (cadddr form))
               (options (cddddr form))
               (defclass-options nil)
               (report-option nil))
          (do ((rest options (cdr rest)))
              ((null rest))
            (let ((opt (car rest)))
              (cond ((and (consp opt) (eq (car opt) :report))
                     (setf report-option (cadr opt)))
                    ((consp opt)
                     (push opt defclass-options)))))
          (let ((forms (list `(defclass ,name ,parents ,slots
                                ,@(nreverse defclass-options)))))
            (when report-option
              (let ((report-body
                      (cond ((stringp report-option)
                             `(write-string ,report-option stream))
                            ((and (consp report-option)
                                  (eq (car report-option) 'lambda))
                             `(funcall ,report-option c stream))
                            (t
                             `(funcall (function ,report-option) c stream)))))
                (push `(defmethod print-object ((c ,name) stream)
                         (if *print-escape*
                             (call-next-method)
                             ,report-body))
                      forms)))
            `(progn ,@(nreverse forms) ',name)))))

;;; --- define-modify-macro ---
(setf (gethash 'define-modify-macro *macros*)
      (lambda (form)
        ;; (define-modify-macro name lambda-list function &optional doc)
        ;; Uses %dmm-helper at macroexpansion time for proper single-evaluation semantics.
        (let* ((name (cadr form))
               (lambda-list (caddr form))
               (fn (cadddr form))
               (place-var (gensym "PLACE"))
               ;; Check for &rest parameter
               (rest-pos (position '&rest lambda-list))
               (rest-param (if rest-pos (nth (1+ rest-pos) lambda-list) nil))
               ;; Extract non-rest parameter names (skip &optional/&rest keywords)
               (opt-params (remove-if (lambda (x) (member x '(&optional &rest)))
                                      (mapcar (lambda (x) (if (consp x) (car x) x))
                                              (if rest-pos
                                                  (subseq lambda-list 0 rest-pos)
                                                  lambda-list)))))
          (if rest-param
              ;; Has &rest: extra-args = (list* opt1 opt2 ... rest-arg)
              `(defmacro ,name (,place-var ,@lambda-list)
                 (%dmm-helper ,place-var ',fn (list* ,@opt-params ,rest-param)))
              ;; No &rest: extra-args = (list opt1 opt2 ...)
              `(defmacro ,name (,place-var ,@lambda-list)
                 (%dmm-helper ,place-var ',fn (list ,@opt-params)))))))

;;; --- assert ---
(setf (gethash 'assert *macros*)
      (lambda (form)
        ;; (assert test-form [places] [datum args...])
        (let* ((test (cadr form))
               (rest (cddr form))
               (has-places (and rest (listp (car rest))))
               (places (if has-places (car rest) nil))
               (datum-args (if has-places (cdr rest) rest))
               (tag (gensym "ASSERT-")))
          (declare (ignore places))
          (if datum-args
              `(block nil
                 (tagbody ,tag
                   (when ,test (return nil))
                   (restart-case
                     (error ,@datum-args)
                     (continue ()
                       :report (lambda (s) (format s "Retry the assertion."))))
                   (go ,tag)))
              `(block nil
                 (tagbody ,tag
                   (when ,test (return nil))
                   (restart-case
                     (error "Assertion failed: ~S" ',test)
                     (continue ()
                       :report (lambda (s) (format s "Retry the assertion."))))
                   (go ,tag)))))))

;;; --- make-instance (simple version for 5.5a) ---
(setf (gethash 'make-instance *macros*)
      (lambda (form)
        ;; (make-instance 'class-name :initarg1 val1 :initarg2 val2 ...)
        ;; For quoted class names, optimize to direct call.
        ;; For other expressions (instances, variables), go through GF dispatch.
        (when (null (cdr form))
          (error 'program-error :format-control "MAKE-INSTANCE: requires at least 1 argument"))
        (let ((class-expr (cadr form))
              (initargs (cddr form)))
          (if (and (consp class-expr) (eq (car class-expr) 'quote))
              ;; Quoted class name: use direct path
              `(%make-instance-with-initargs ,class-expr ,@initargs)
              ;; Non-quoted: go through GF dispatch for user methods
              `(funcall #'make-instance ,class-expr ,@initargs)))))

;;; --- with-accessors ---
(setf (gethash 'with-accessors *macros*)
      (lambda (form)
        ;; (with-accessors ((variable-name accessor-name) ...) instance-form body...)
        ;; Expands to symbol-macrolet so reads/writes go through accessor calls.
        (let* ((slots (cadr form))
               (obj-form (caddr form))
               (body (cdddr form))
               (obj-var (gensym "OBJ")))
          `(let ((,obj-var ,obj-form))
             (symbol-macrolet ,(mapcar (lambda (s)
                                         (if (consp s)
                                             `(,(car s) (,(cadr s) ,obj-var))
                                             `(,s (,s ,obj-var))))
                                       slots)
               ,@body)))))

;;; --- with-slots ---
(setf (gethash 'with-slots *macros*)
      (lambda (form)
        ;; (with-slots (slot-entry...) instance-form body...)
        ;; slot-entry ::= slot-name | (variable-name slot-name)
        ;; Expands to symbol-macrolet so reads/writes go through slot-value.
        (let* ((slots (cadr form))
               (obj-form (caddr form))
               (body (cdddr form))
               (obj-var (gensym "OBJ")))
          `(let ((,obj-var ,obj-form))
             (symbol-macrolet ,(mapcar (lambda (s)
                                         (if (consp s)
                                             `(,(car s) (slot-value ,obj-var ',(cadr s)))
                                             `(,s (slot-value ,obj-var ',s))))
                                       slots)
               ,@body)))))

;;; --- defgeneric ---
(setf (gethash 'defgeneric *macros*)
      (lambda (form)
        ;; (defgeneric name (params...) &optional options...)
        ;; options: (:documentation "..."), (:method-combination ...), (:method qual (params...) body...)
        (let* ((name (cadr form))
               (params (caddr form))
               (options (cdddr form))
               (plain-params (remove-if (lambda (p) (member p '(&rest &optional &key &allow-other-keys))) params))
               (arity (length plain-params))
               ;; Parse lambda list structure for congruence checking (CLHS 7.6.4)
               (gf-required-count 0)
               (gf-optional-count 0)
               (gf-has-rest nil)
               (gf-has-key nil)
               (gf-has-allow-other-keys nil)
               (gf-keyword-names nil)
               ;; Extract :method-combination option: (:method-combination name [arg1 arg2 ...])
               (mc-opt (find-if (lambda (opt)
                                  (and (consp opt)
                                       (symbolp (car opt))
                                       (string= (symbol-name (car opt)) "METHOD-COMBINATION")))
                                options))
               (mc-name (if mc-opt (cadr mc-opt) nil))
               (mc-order (if mc-opt (caddr mc-opt) nil))
               ;; For long-form: extra args after name
               (mc-args (if (and mc-opt (cddr mc-opt)) (cddr mc-opt) nil))
               ;; Extract :method options for inline method definitions
               (method-opts (remove-if-not (lambda (opt)
                                             (and (consp opt)
                                                  (symbolp (car opt))
                                                  (string= (symbol-name (car opt)) "METHOD")))
                                           options))
               ;; Known option keywords
               (known-opts '("DOCUMENTATION" "METHOD-COMBINATION" "METHOD"
                             "ARGUMENT-PRECEDENCE-ORDER" "DECLARE" "GENERIC-FUNCTION-CLASS"
                             "METHOD-CLASS")))
          ;; Parse lambda list structure for congruence checking
          (let ((ll-state :required))
            (dolist (p params)
              (cond
                ((eq p '&optional) (setf ll-state :optional))
                ((eq p '&rest) (setf ll-state :rest))
                ((eq p '&key) (setf gf-has-key t) (setf ll-state :key))
                ((eq p '&allow-other-keys) (setf gf-has-allow-other-keys t))
                (t (case ll-state
                     (:required (setf gf-required-count (1+ gf-required-count)))
                     (:optional (setf gf-optional-count (1+ gf-optional-count)))
                     (:rest (setf gf-has-rest t))
                     (:key (push (if (consp p)
                                     (if (consp (car p)) (caar p) (car p))
                                     p)
                                 gf-keyword-names)))))))  ; collect &key param names
          ;; Validate options at macro-expansion time
          ;; Check for unknown options
          (dolist (opt options)
            (when (and (consp opt) (symbolp (car opt)))
              (let ((oname (symbol-name (car opt))))
                (unless (member oname known-opts :test #'string=)
                  (error 'program-error :format-control "DEFGENERIC ~S: unknown option ~S"
                         :format-arguments (list name (car opt)))))))
          ;; Check for duplicate :documentation
          (let ((doc-count 0))
            (dolist (opt options)
              (when (and (consp opt) (symbolp (car opt))
                         (string= (symbol-name (car opt)) "DOCUMENTATION"))
                (setf doc-count (1+ doc-count))))
            (when (> doc-count 1)
              (error 'program-error :format-control "DEFGENERIC ~S: duplicate :documentation"
                     :format-arguments (list name))))
          ;; Check :argument-precedence-order for duplicates and completeness
          (let ((apo (find-if (lambda (opt)
                                (and (consp opt) (symbolp (car opt))
                                     (string= (symbol-name (car opt)) "ARGUMENT-PRECEDENCE-ORDER")))
                              options)))
            (when apo
              (let ((apo-params (cdr apo)))
                ;; Check for duplicates
                (let ((seen nil))
                  (dolist (p apo-params)
                    (when (member p seen)
                      (error 'program-error :format-control "DEFGENERIC ~S: duplicate in :argument-precedence-order: ~S"
                             :format-arguments (list name p)))
                    (push p seen)))
                ;; Check completeness: must mention all required params
                (unless (= (length apo-params) (length plain-params))
                  (error 'program-error :format-control "DEFGENERIC ~S: :argument-precedence-order must list all required parameters"
                         :format-arguments (list name))))))
          ;; Check lambda list congruency for inline :method forms (CLHS 7.6.4)
          (dolist (mopt method-opts)
            (let ((mparams (if (and (cdr mopt) (symbolp (cadr mopt)) (not (listp (cadr mopt))) (listp (caddr mopt)))
                               (caddr mopt)  ; has qualifier
                               (cadr mopt))) ; no qualifier
                  (m-req 0) (m-opt 0) (m-rest nil) (m-key nil)
                  (m-allow-other-keys nil) (m-keyword-names nil))
              (let ((state :required))
                (dolist (p mparams)
                  (cond
                    ((member p '(&optional)) (setf state :optional))
                    ((member p '(&rest &body)) (setf state :rest))
                    ((eq p '&key) (setf m-key t) (setf state :key))
                    ((eq p '&aux) (setf state :aux))
                    ((eq p '&allow-other-keys) (setf m-allow-other-keys t))
                    (t (case state
                         (:required (setf m-req (1+ m-req)))
                         (:optional (setf m-opt (1+ m-opt)))
                         (:rest (setf m-rest t))
                         (:key (push (if (consp p)
                                         (if (consp (car p)) (caar p) (car p))
                                         p)
                                     m-keyword-names))
                         (otherwise nil))))))
              ;; Rule 1: same number of required
              (when (/= gf-required-count m-req)
                (error 'program-error :format-control "DEFGENERIC ~S: inline method has ~D required parameter(s) but generic function has ~D"
                       :format-arguments (list name m-req gf-required-count)))
              ;; Rule 2: same number of optional
              (when (and (/= gf-optional-count m-opt) (not (or m-rest m-key)))
                (error 'program-error :format-control "DEFGENERIC ~S: inline method has ~D optional parameter(s) but generic function has ~D"
                       :format-arguments (list name m-opt gf-optional-count)))
              ;; Rule 3 (bidirectional): if any mentions &rest/&key, all must
              (when (and (or gf-has-rest gf-has-key) (not (or m-rest m-key)))
                (error 'program-error :format-control "DEFGENERIC ~S: inline method must accept &rest or &key because the generic function does"
                       :format-arguments (list name)))
              (when (and (or m-rest m-key) (not (or gf-has-rest gf-has-key)))
                (error 'program-error :format-control "DEFGENERIC ~S: generic function must accept &rest or &key because the inline method does"
                       :format-arguments (list name)))
              ;; Rule 4: if GF has &key with names, method must accept all of them
              (when (and gf-has-key gf-keyword-names (not m-allow-other-keys)
                         (not (and m-rest (not m-key))))
                (dolist (kw gf-keyword-names)
                  (unless (member (symbol-name kw) (mapcar #'symbol-name m-keyword-names) :test #'string=)
                    (error 'program-error :format-control "DEFGENERIC ~S: inline method does not accept keyword ~S required by the generic function"
                           :format-arguments (list name kw)))))))
          `(progn
             ;; CLHS: signal program-error if name is a special operator or macro
             ,@(when (symbolp name)
                 `((when (special-operator-p ',name)
                     (error 'program-error :format-control "DEFGENERIC: ~S names a special operator"
                            :format-arguments (list ',name)))
                   (when (macro-function ',name)
                     (error 'program-error :format-control "DEFGENERIC: ~S names a macro"
                            :format-arguments (list ',name)))))
             ;; CLHS: signal error if name is an ordinary function (not GF)
             (when (and (fboundp ',name)
                        (not (typep (fdefinition ',name) 'generic-function)))
               (error 'program-error :format-control "DEFGENERIC: ~S names an ordinary function"
                      :format-arguments (list ',name)))
             (let ((%gf (or (%find-gf ',name) (%make-gf ',name ,arity))))
               ;; CLHS: "methods defined by previous defgeneric forms are removed"
               (%clear-defgeneric-inline-methods %gf)
               (%register-gf ',name %gf)
               (%set-gf-lambda-list-info %gf ,gf-required-count ,gf-optional-count
                                         ,(if gf-has-rest t nil)
                                         ,(if gf-has-key t nil)
                                         ,(if gf-has-allow-other-keys t nil)
                                         (list ,@(mapcar (lambda (k) `',k) gf-keyword-names)))
               ,@(if (and mc-name (not (string= (symbol-name mc-name) "STANDARD")))
                     `((%set-method-combination %gf ',mc-name))
                     nil)
               ,@(if (and mc-order (symbolp mc-order) (string= (symbol-name mc-order) "MOST-SPECIFIC-LAST"))
                     `((%set-method-combination-order %gf ',mc-order))
                     nil)
               ,@(if mc-args
                     `((%set-method-combination-args %gf (list ,@mc-args)))
                     nil)
               ;; Set symbol-function to the GF object directly
               (setf (fdefinition ',name) %gf))
             ;; Process inline :method definitions and mark them as from defgeneric
             ,@(mapcar (lambda (mopt)
                         `(let ((%m (defmethod ,name ,@(cdr mopt))))
                            (%mark-defgeneric-inline-method (%find-gf ',name) %m)
                            %m))
                       method-opts)
             (fdefinition ',name)))))

;;; --- defmethod ---
(setf (gethash 'defmethod *macros*)
      (lambda (form)
        ;; (defmethod name [qualifier] ((param1 class1) param2 ...) body...)
        ;; qualifier is optional: :before, :after, :around
        (let* ((name (cadr form))
               (rest (cddr form))
               ;; Check for qualifier
               (qualifier nil)
               (specialized-params nil)
               (body nil))
          ;; Detect qualifier: a symbol (keyword or not) followed by a list (params)
          ;; e.g. :before, :after, :around, list, append, etc.
          (if (and (car rest) (symbolp (car rest)) (not (listp (car rest))) (listp (cadr rest)))
              (progn
                (setf qualifier (car rest))
                (setf specialized-params (cadr rest))
                (setf body (cddr rest)))
              (progn
                (setf specialized-params (car rest))
                (setf body (cdr rest))))
          ;; Parse specialized parameters: ((p1 class1) p2 ...)
          ;; Build: specializers list, plain parameter names
          ;; Only required params can be specialized; stop at &key/&optional/&rest/etc.
          (let* ((specializers nil)
                 (plain-params nil)
                 (in-required t))
            (dolist (sp specialized-params)
              (if (and (symbolp sp)
                       (member sp '(&key &optional &rest &body &allow-other-keys &aux)))
                  (progn
                    (setf in-required nil)
                    (push sp plain-params))
                  (if in-required
                      (if (consp sp)
                          (let ((param-name (car sp))
                                (spec (cadr sp)))
                            (push param-name plain-params)
                            ;; Handle EQL specializers: (param (eql value))
                            (if (and (consp spec) (eq (car spec) 'eql))
                                (push `(list 'eql ,(cadr spec)) specializers)
                                (push `(find-class ',spec) specializers)))
                          (progn
                            (push sp plain-params)
                            (push '(find-class 't) specializers)))
                      (push sp plain-params))))
            (setf specializers (nreverse specializers))
            (setf plain-params (nreverse plain-params))
            ;; CLHS 7.6.5: The effective method accepts the union of all applicable
            ;; methods' keyword parameters. Individual methods must not reject keywords
            ;; that other applicable methods accept. Add &allow-other-keys to method
            ;; lambdas so per-method CheckNoUnknownKeys doesn't fire incorrectly.
            (when (and (member '&key plain-params)
                       (not (member '&allow-other-keys plain-params)))
              (let ((pos (position '&aux plain-params)))
                (if pos
                    (setf plain-params (append (subseq plain-params 0 pos)
                                               '(&allow-other-keys)
                                               (subseq plain-params pos)))
                    (setf plain-params (append plain-params '(&allow-other-keys))))))
            ;; Parse method lambda list structure for congruence checking (CLHS 7.6.4)
            (let ((m-required-count (length specializers))
                  (m-optional-count 0)
                  (m-has-rest nil)
                  (m-has-key nil)
                  (m-has-allow-other-keys nil)
                  (m-keyword-names nil))
              (let ((ll-state :required))
                (dolist (sp specialized-params)
                  (cond
                    ((eq sp '&optional) (setf ll-state :optional))
                    ((eq sp '&rest) (setf ll-state :rest))
                    ((eq sp '&body) (setf ll-state :rest))
                    ((eq sp '&key) (setf m-has-key t) (setf ll-state :key))
                    ((eq sp '&allow-other-keys) (setf m-has-allow-other-keys t))
                    ((eq sp '&aux) (setf ll-state :aux))
                    (t (case ll-state
                         (:optional (setf m-optional-count (1+ m-optional-count)))
                         (:rest (setf m-has-rest t))
                         (:key (push (if (consp sp)
                                         (if (consp (car sp)) (caar sp) (car sp))
                                         sp)
                                     m-keyword-names))
                         (otherwise nil))))))
              ;; Ensure GF exists (auto-create if not)
              (let ((qual-list (if qualifier `(list ',qualifier) 'nil))
                    (n-params (length plain-params)))
                `(progn
                   ;; Auto-create GF if it doesn't exist. %register-gf installs
                   ;; both the GF registry entry and sym.Function (so this also
                   ;; handles extending CL generic functions when CL is locked).
                   (if (null (%find-gf ',name))
                       (let ((%gf (%make-gf ',name ,n-params)))
                         (%register-gf ',name %gf)
                         ;; Auto-created GF takes &key/&rest from the method but NOT
                         ;; specific keyword names — per CLHS, the GF accepts whatever
                         ;; each applicable method accepts (union), so pinning specific
                         ;; keywords from the first method would reject later methods
                         ;; that have different (valid) keyword sets.
                         (%set-gf-lambda-list-info %gf ,m-required-count ,m-optional-count
                                                   ,(if m-has-rest t nil)
                                                   ,(if m-has-key t nil)
                                                   ,(if m-has-allow-other-keys t nil)
                                                   nil)))
                   ;; defmethod returns the method object (CL spec)
                   (let ((%m (%make-method
                               (list ,@specializers)
                               ,qual-list
                               (lambda ,plain-params
                                 (block ,(if (and (consp name) (eq (car name) 'setf))
                                             (cadr name)
                                             name)
                                   ,@body)))))
                     (%set-method-lambda-list-info %m ,m-required-count ,m-optional-count
                                                   ,(if m-has-rest t nil)
                                                   ,(if m-has-key t nil)
                                                   ,(if m-has-allow-other-keys t nil)
                                                   (list ,@(mapcar (lambda (k) `',k) m-keyword-names)))
                     (%add-method (%find-gf ',name) %m)
                     %m))))))))

;;; --- defpackage ---

(setf (gethash 'defpackage *macros*)
      (lambda (form)
        ;; (defpackage name &rest options)
        (let ((pkg-name (string (cadr form)))
              (options (cddr form))
              (pkg-var (gensym "PKG")))
          ;; === Validation (CLHS 11.2.13) ===
          ;; 1. Check duplicate :size and :documentation options
          (let ((size-count 0) (doc-count 0))
            (dolist (option options)
              (when (consp option)
                (let ((key (car option)))
                  (when (eq key :size) (incf size-count))
                  (when (eq key :documentation) (incf doc-count)))))
            (when (> size-count 1)
              (error 'program-error
                     :format-control "DEFPACKAGE ~A: duplicate :SIZE option"
                     :format-arguments (list pkg-name)))
            (when (> doc-count 1)
              (error 'program-error
                     :format-control "DEFPACKAGE ~A: duplicate :DOCUMENTATION option"
                     :format-arguments (list pkg-name))))
          ;; 2. Collect name sets and check pairwise disjointness
          (let ((shadow-names nil)
                (shadowing-import-names nil)
                (import-names nil)
                (intern-names nil)
                (export-names nil)
                (nickname-strings nil)
                (local-nicknames nil))   ; list of (nick-str . actual-pkg-str) pairs
            (dolist (option options)
              (when (consp option)
                (let ((key (car option))
                      (args (cdr option)))
                  (cond
                    ((eq key :shadow)
                     (dolist (s args) (push (string s) shadow-names)))
                    ((eq key :shadowing-import-from)
                     (dolist (s (cdr args)) (push (string s) shadowing-import-names)))
                    ((eq key :import-from)
                     (dolist (s (cdr args)) (push (string s) import-names)))
                    ((eq key :intern)
                     (dolist (s args) (push (string s) intern-names)))
                    ((eq key :export)
                     (dolist (s args) (push (string s) export-names)))
                    ((eq key :nicknames)
                     (dolist (n args) (push (string n) nickname-strings)))
                    ((eq key :local-nicknames)
                     (dolist (pair args)
                       (push (cons (string (car pair)) (string (cadr pair)))
                             local-nicknames)))))))
            ;; Check pairwise disjointness of shadow/shadowing-import/import/intern
            (let ((pairs (list
                          (list shadow-names shadowing-import-names
                                ":SHADOW" ":SHADOWING-IMPORT-FROM")
                          (list shadow-names import-names
                                ":SHADOW" ":IMPORT-FROM")
                          (list shadow-names intern-names
                                ":SHADOW" ":INTERN")
                          (list shadowing-import-names import-names
                                ":SHADOWING-IMPORT-FROM" ":IMPORT-FROM")
                          (list shadowing-import-names intern-names
                                ":SHADOWING-IMPORT-FROM" ":INTERN")
                          (list import-names intern-names
                                ":IMPORT-FROM" ":INTERN"))))
              (dolist (pair pairs)
                (let ((set-a (car pair))
                      (set-b (cadr pair))
                      (name-a (caddr pair))
                      (name-b (cadddr pair)))
                  (dolist (n set-a)
                    (when (member n set-b :test #'string=)
                      (error 'program-error
                             :format-control "DEFPACKAGE ~A: name ~A appears in both ~A and ~A"
                             :format-arguments (list pkg-name n name-a name-b)))))))
            ;; Check export vs intern disjointness
            (dolist (n export-names)
              (when (member n intern-names :test #'string=)
                (error 'program-error
                       :format-control "DEFPACKAGE ~A: name ~A appears in both :EXPORT and :INTERN"
                       :format-arguments (list pkg-name n))))
            ;; === End validation ===
            ;; Generate code
            (let ((use-forms nil) (export-forms nil)
                  (import-forms nil) (shadow-forms nil)
                  (nickname-forms nil) (intern-forms nil)
                  (shadowing-import-forms nil)
                  (nickname-check-forms nil)
                  (local-nickname-forms nil))
              (dolist (option options)
                (when (consp option)
                  (let ((key (car option))
                        (args (cdr option)))
                    (cond
                      ((member key '(:use) :test #'eq)
                       ;; Pass the name string directly so that the runtime
                       ;; (Runtime.PackageUse → ResolvePackage) reports the
                       ;; missing package by its actual name instead of NIL
                       ;; when find-package would fail.
                       (dolist (u args)
                         (push `(%package-use ,pkg-var ,(string u))
                               use-forms)))
                      ((member key '(:export) :test #'eq)
                       (dolist (s args)
                         (push `(%package-export ,pkg-var
                                  (intern ,(string s) ,pkg-var))
                               export-forms)))
                      ((member key '(:import-from) :test #'eq)
                       ;; (car args) might be a list like (error ...) when all #+impl guards
                       ;; are stripped (metatilities-base pattern: #-(or sbcl ecl ...) (error ...)).
                       ;; In that case treat it as "no source package" and fall to DOTCL-MOP.
                       (let* ((pkg-name-raw (car args))
                              (pkg-name-valid (or (symbolp pkg-name-raw) (stringp pkg-name-raw)))
                              (from-pkg (when pkg-name-valid (string pkg-name-raw)))
                              ;; symbol names: (cdr args) when pkg-name is valid, else filter from args
                              (raw-syms (if pkg-name-valid (cdr args) args))
                              (sym-names (mapcar #'string
                                                 (remove-if-not (lambda (x) (or (symbolp x) (stringp x)))
                                                                raw-syms))))
                         (let ((pkg-obj-var (gensym "FROMPKG")))
                           ;; Generate a single group-level form so that when the source
                           ;; package is missing (e.g. #+sbcl/#:sb-mop stripped by reader),
                           ;; we can fall back to DOTCL-MOP and also import the "package
                           ;; name" string as a symbol (it was a MOP fn, not a real pkg).
                           (push
                             (if pkg-name-valid
                               `(let ((,pkg-obj-var (find-package ,from-pkg)))
                                  (cond
                                    (,pkg-obj-var
                                     ,@(mapcar
                                         (lambda (sym-name)
                                           `(multiple-value-bind (sym status)
                                                (find-symbol ,sym-name ,pkg-obj-var)
                                              (if status
                                                  (%package-import ,pkg-var sym)
                                                  (restart-case
                                                      (error 'package-error
                                                             :package ,from-pkg
                                                             :format-control "DEFPACKAGE: symbol ~A not found in package ~A"
                                                             :format-arguments (list ,sym-name ,from-pkg))
                                                    (continue ()
                                                      :report "Skip importing this symbol."
                                                      nil)))))
                                         sym-names))
                                    (t
                                     ;; Source package missing: fall back to DOTCL-MOP.
                                     ;; The "package name" was likely a MOP function name
                                     ;; whose #+feature guard stripped the real package name.
                                     (let ((mop (find-package "DOTCL-MOP")))
                                       (when mop
                                         (multiple-value-bind (sym ok) (find-symbol ,from-pkg mop)
                                           (when ok (%package-import ,pkg-var sym)))
                                         ,@(mapcar
                                             (lambda (sym-name)
                                               `(multiple-value-bind (sym ok) (find-symbol ,sym-name mop)
                                                  (when ok (%package-import ,pkg-var sym))))
                                             sym-names))))))
                               ;; Package name is not a string/symbol (e.g. (error ...) form):
                               ;; skip it and import sym-names from DOTCL-MOP directly.
                               `(let ((mop (find-package "DOTCL-MOP")))
                                  (when mop
                                    ,@(mapcar
                                        (lambda (sym-name)
                                          `(multiple-value-bind (sym ok) (find-symbol ,sym-name mop)
                                             (when ok (%package-import ,pkg-var sym))))
                                        sym-names))))
                             import-forms))))
                      ((member key '(:shadow) :test #'eq)
                       (dolist (s args)
                         (push `(%package-shadow ,pkg-var ,(string s))
                               shadow-forms)))
                      ((member key '(:intern) :test #'eq)
                       (dolist (s args)
                         (push `(intern ,(string s) ,pkg-var)
                               intern-forms)))
                      ((member key '(:shadowing-import-from) :test #'eq)
                       (let* ((pkg-name-raw (car args))
                              (pkg-name-valid (or (symbolp pkg-name-raw) (stringp pkg-name-raw)))
                              (from-pkg (when pkg-name-valid (string pkg-name-raw)))
                              (raw-syms (if pkg-name-valid (cdr args) args))
                              (sym-names (mapcar #'string
                                                 (remove-if-not (lambda (x) (or (symbolp x) (stringp x)))
                                                                raw-syms))))
                         (let ((pkg-obj-var (gensym "FROMPKG")))
                           (push
                             (if pkg-name-valid
                               `(let ((,pkg-obj-var (find-package ,from-pkg)))
                                  (cond
                                    (,pkg-obj-var
                                     ,@(mapcar
                                         (lambda (sym-name)
                                           `(multiple-value-bind (sym status)
                                                (find-symbol ,sym-name ,pkg-obj-var)
                                              (if status
                                                  (%shadowing-import sym ,pkg-var)
                                                  (restart-case
                                                      (error 'package-error
                                                             :package ,from-pkg
                                                             :format-control "DEFPACKAGE: symbol ~A not found in package ~A"
                                                             :format-arguments (list ,sym-name ,from-pkg))
                                                    (continue ()
                                                      :report "Skip importing this symbol."
                                                      nil)))))
                                         sym-names))
                                    (t
                                     (let ((mop (find-package "DOTCL-MOP")))
                                       (when mop
                                         (multiple-value-bind (sym ok) (find-symbol ,from-pkg mop)
                                           (when ok (%shadowing-import sym ,pkg-var)))
                                         ,@(mapcar
                                             (lambda (sym-name)
                                               `(multiple-value-bind (sym ok) (find-symbol ,sym-name mop)
                                                  (when ok (%shadowing-import sym ,pkg-var))))
                                             sym-names))))))
                               `(let ((mop (find-package "DOTCL-MOP")))
                                  (when mop
                                    ,@(mapcar
                                        (lambda (sym-name)
                                          `(multiple-value-bind (sym ok) (find-symbol ,sym-name mop)
                                             (when ok (%shadowing-import sym ,pkg-var))))
                                        sym-names))))
                             shadowing-import-forms))))
                      ((member key '(:nicknames) :test #'eq)
                       (dolist (n args)
                         (push `(%package-nickname ,pkg-var ,(string n))
                               nickname-forms)))
                      ((member key '(:local-nicknames) :test #'eq)
                       (dolist (pair args)
                         (push `(%add-local-nickname ,(string (car pair))
                                                     (find-package ,(string (cadr pair)))
                                                     ,pkg-var)
                               local-nickname-forms)))))))
              ;; 3. Emit runtime nickname conflict checks
              (dolist (n nickname-strings)
                (let ((nick-var (gensym "NICK")))
                  (push `(let ((,nick-var (find-package ,n)))
                           (when (and ,nick-var (not (eq ,nick-var ,pkg-var)))
                             (error 'package-error
                                    :package ,n
                                    :format-control "DEFPACKAGE ~A: nickname ~A conflicts with existing package"
                                    :format-arguments (list ,pkg-name ,n))))
                        nickname-check-forms)))
              (let ((inner `(let ((,pkg-var (%make-package ,pkg-name)))
                              ,@(nreverse nickname-check-forms)
                              ,@(nreverse nickname-forms)
                              ,@(nreverse shadow-forms)
                              ,@(nreverse shadowing-import-forms)
                              ,@(nreverse use-forms)
                              ,@(nreverse import-forms)
                              ,@(nreverse intern-forms)
                              ,@(nreverse export-forms)
                              ,@(nreverse local-nickname-forms)
                              ,pkg-var)))
                ;; In compile-file mode, wrap with eval-when so macrolet-expanded
                ;; defpackage forms are evaluated at compile time (CLHS 3.2.3.1).
                ;; Skip during cross-compilation: %make-package is dotcl-internal.
                (if *compile-file-mode*
                    `(eval-when (:compile-toplevel :load-toplevel :execute) ,inner)
                    inner)))))))

;;; --- do-symbols / do-external-symbols ---

(setf (gethash 'do-symbols *macros*)
      (lambda (form)
        ;; (do-symbols (var package-form &optional result) body...)
        (let* ((var-clause (cadr form))
               (var (car var-clause))
               (pkg-form (if (cdr var-clause) (cadr var-clause) '*package*))
               (result-form (caddr var-clause))
               (body (cddr form))
               (list-var (gensym "SYMS")))
          `(let ((,list-var (%package-all-symbols ,pkg-form)))
             (dolist (,var ,list-var ,result-form)
               ,@body)))))

(setf (gethash 'do-external-symbols *macros*)
      (lambda (form)
        ;; (do-external-symbols (var package-form &optional result) body...)
        (let* ((var-clause (cadr form))
               (var (car var-clause))
               (pkg-form (if (cdr var-clause) (cadr var-clause) '*package*))
               (result-form (caddr var-clause))
               (body (cddr form))
               (list-var (gensym "SYMS")))
          `(let ((,list-var (%package-external-symbols ,pkg-form)))
             (dolist (,var ,list-var ,result-form)
               ,@body)))))

(setf (gethash 'do-all-symbols *macros*)
      (lambda (form)
        ;; (do-all-symbols (var &optional result) body...)
        ;; Iterate over all symbols in all packages (present symbols only, no inherited duplicates)
        (let* ((var-clause (cadr form))
               (var (car var-clause))
               (result-form (cadr var-clause))
               (body (cddr form))
               ;; Separate declarations from body
               (decls nil)
               (real-body body)
               (pkg-var (gensym "PKG"))
               (pkgs-var (gensym "PKGS"))
               (syms-var (gensym "SYMS"))
               (loop-tag (gensym "LOOP"))
               (end-tag (gensym "END")))
          ;; Extract leading declarations
          (loop while (and real-body
                          (consp (car real-body))
                          (eq (caar real-body) 'declare))
                do (push (car real-body) decls)
                   (setq real-body (cdr real-body)))
          (setq decls (nreverse decls))
          `(block nil
             (let* ((,pkgs-var (list-all-packages))
                    (,pkg-var ,pkgs-var)
                    (,syms-var nil)
                    (,var nil))
               (declare (ignorable ,var))
               ,@decls
               (tagbody
                 ,loop-tag
                 (when (null ,syms-var)
                   (when (null ,pkg-var) (go ,end-tag))
                   (setq ,syms-var (%package-all-symbols (car ,pkg-var)))
                   (setq ,pkg-var (cdr ,pkg-var))
                   (go ,loop-tag))
                 (setq ,var (car ,syms-var))
                 (setq ,syms-var (cdr ,syms-var))
                 ,@real-body
                 (go ,loop-tag)
                 ,end-tag)
               ;; CLHS: result-form is evaluated with var bound to NIL.
               (setq ,var nil)
               ,result-form)))))

;;; --- with-package-iterator ---

(setf (gethash 'with-package-iterator *macros*)
      (lambda (form)
        ;; (with-package-iterator (name pkg-list-form &rest sym-types) body...)
        (let* ((iter-spec (cadr form))
               (name (car iter-spec))
               (pkg-list-form (cadr iter-spec))
               (sym-types (cddr iter-spec))
               (body (cddr form))
               (entries-var (gensym "ENTRIES"))
               (rest-var (gensym "REST")))
          (if (null sym-types)
              `(error 'program-error)
              `(let* ((,entries-var (%collect-package-iterator-entries
                                     ,pkg-list-form
                                     (list ,@sym-types)))
                      (,rest-var ,entries-var))
                 (flet ((,name ()
                          (if (null ,rest-var)
                              (values nil nil nil nil)
                              (let ((entry (car ,rest-var)))
                                (setq ,rest-var (cdr ,rest-var))
                                (values t (car entry) (cadr entry) (caddr entry))))))
                   ,@body))))))

;;; --- declaim ---

(setf (gethash 'declaim *macros*)
      (lambda (form)
        ;; CLHS: (declaim decl...) ≡ (eval-when (:compile-toplevel :load-toplevel :execute) (proclaim 'decl) ...)
        ;; compile-form also handles declaim directly for compile-time special tracking.
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           ,@(mapcar (lambda (decl) `(proclaim ',decl)) (cdr form)))))

(setf (gethash 'define-compiler-macro *macros*)
      (lambda (form)
        ;; (define-compiler-macro name lambda-list body...)
        ;; Generates a runtime registration call so COMPILER-MACRO-FUNCTION can retrieve it.
        (let* ((name (cadr form))
               (lambda-list (caddr form))
               (body (cdddr form))
               (whole-sym (gensym "WHOLE"))
               ;; Extract &whole var if present
               (whole-var (when (and (consp lambda-list) (eq (car lambda-list) '&whole))
                            (cadr lambda-list)))
               (rest-ll (if whole-var (cddr lambda-list) lambda-list))
               ;; Strip &environment (not valid in destructuring-bind)
               (env-var nil)
               (clean-ll (let ((result nil) (ll rest-ll))
                           (loop while ll do
                             (cond ((eq (car ll) '&environment)
                                    (setq env-var (cadr ll))
                                    (setq ll (cddr ll)))
                                   (t (push (car ll) result)
                                      (setq ll (cdr ll)))))
                           (nreverse result)))
               ;; &environment binding
               (wrapped-body (if env-var
                                 `((let ((,env-var nil)) ,@body))
                                 body))
               ;; Implicit block named after the function (CLHS: same as defmacro)
               ;; Use (symbolp name) to get the block name; (setf foo) names use car
               (block-name (if (consp name) (cadr name) name))
               (block-wrapped-body `((block ,block-name ,@wrapped-body)))
               (env-sym (gensym "ENV"))
               (expander-form
                 (if whole-var
                     `(lambda (,whole-sym ,env-sym)
                        (declare (ignore ,env-sym))
                        (let ((,whole-var ,whole-sym))
                          (destructuring-bind ,clean-ll (cdr ,whole-sym)
                            ,@block-wrapped-body)))
                     `(lambda (,whole-sym ,env-sym)
                        (declare (ignore ,env-sym))
                        (destructuring-bind ,clean-ll (cdr ,whole-sym)
                          ,@block-wrapped-body)))))
          `(progn
             (%register-compiler-macro-rt ',name ,expander-form)
             ',name))))

;;; --- define-method-combination ---
(setf (gethash 'define-method-combination *macros*)
      (lambda (form)
        ;; Detect long form: (define-method-combination name lambda-list (method-group-spec*) ...)
        ;; Short form: (define-method-combination name &key operator identity-with-one-argument documentation)
        ;; Long form is detected when the third element is a list (lambda-list)
        ;; and the fourth element is a list of lists (method-group-specs)
        (let ((name (cadr form))
              (rest (cddr form)))
          ;; Check for long form: rest starts with a list (lambda-list) followed by a list of lists
          (if (and rest (listp (car rest))
                   (cdr rest) (listp (cadr rest))
                   ;; Distinguish from short form keyword args: if first element is a keyword, it's short form
                   (not (keywordp (car rest))))
              ;; === Long form ===
              (let* ((lambda-list (car rest))
                     (group-specs-raw (cadr rest))
                     (body-and-options (cddr rest))
                     ;; Skip declarations and :arguments/:generic-function options
                     (body nil)
                     (arguments-lambda-list nil))
                ;; Parse body: skip (declare ...) and (:arguments ...) and (:generic-function ...)
                (dolist (item body-and-options)
                  (cond
                    ((and (consp item) (eq (car item) 'declare)) nil) ;; skip declarations
                    ((and (consp item) (eq (car item) :arguments))
                     (setf arguments-lambda-list (cdr item)))
                    ((and (consp item) (eq (car item) :generic-function)) nil) ;; skip
                    (t (push item body))))
                (setf body (nreverse body))
                ;; Build group specs as a list: ((name qualifier-pattern :order val :required val) ...)
                (let ((spec-forms nil))
                  (dolist (gs group-specs-raw)
                    (when (consp gs)
                      (let* ((gs-name (car gs))
                             (gs-rest (cdr gs))
                             (qual-pat (if (consp gs-rest) (car gs-rest) '*))
                             (gs-options (if (consp gs-rest) (cdr gs-rest) nil))
                             ;; Parse options
                             (order :most-specific-first)
                             (required nil))
                        (do ((opts gs-options (cddr opts)))
                            ((null opts))
                          (let ((k (car opts))
                                (v (cadr opts)))
                            (cond
                              ((eq k :order) (setf order v))
                              ((eq k :required) (setf required v))
                              ((eq k :description) nil)))) ;; ignore :description
                        (push `(list ',gs-name ',qual-pat :order ,order :required ,required) spec-forms))))
                  (setf spec-forms (nreverse spec-forms))
                  ;; Build the body function
                  ;; The body function receives three args: mc-args-list, method-groups-list, gf-args-list
                  ;; It destructures mc-args via destructuring-bind, binds method-group vars via nth,
                  ;; and optionally destructures gf-args via the :arguments lambda list
                  (let ((group-names (mapcar #'car group-specs-raw))
                        (mc-args-var (gensym "MC-ARGS"))
                        (groups-var (gensym "GROUPS"))
                        (gf-args-var (gensym "GF-ARGS"))
                        (spec-args-var (gensym "SPEC-ARGS")))
                    `(progn
                       (%register-long-method-combination
                        ,(string name)
                        ;; spec-function: (mc-args-list) -> spec-list, with lambda-list vars bound
                        (lambda (,spec-args-var)
                          (destructuring-bind ,lambda-list ,spec-args-var
                            (list ,@spec-forms)))
                        (lambda (,mc-args-var ,groups-var ,gf-args-var)
                          (declare (ignorable ,gf-args-var))
                          (destructuring-bind ,lambda-list ,mc-args-var
                            (let ,(let ((idx -1))
                                   (mapcar (lambda (gn)
                                             (setf idx (1+ idx))
                                             `(,gn (nth ,idx ,groups-var)))
                                           group-names))
                              ,@(if arguments-lambda-list
                                    `((destructuring-bind ,arguments-lambda-list ,gf-args-var
                                        ,@body))
                                    body)))))
                       ',name))))
              ;; === Short form ===
              (let ((operator nil)
                    (identity-with-one-arg nil))
                ;; Parse keyword arguments
                (do ((args rest (cddr args)))
                    ((null args))
                  (let ((key (car args))
                        (val (cadr args)))
                    (cond
                      ((eq key :operator) (setf operator val))
                      ((eq key :identity-with-one-argument) (setf identity-with-one-arg val))
                      ((eq key :documentation) nil)))) ;; ignore
                ;; Default operator is the name itself
                (unless operator (setf operator name))
                `(progn
                   (%register-method-combination ,(string name) ,(string operator) ,identity-with-one-arg)
                   ',name))))))

;;; --- in-package ---

(setf (gethash 'in-package *macros*)
      (lambda (form)
        ;; (in-package name)
        ;; At compile-time: switch *package*
        ;; At load-time: set *package* to named package
        (let ((name (string (cadr form))))
          ;; Side-effect at compile time: look up the package
          ;; (This is a simplified version - real CL does compile-time switch)
          `(let ((pkg (find-package ,name)))
             (unless pkg
               (error 'package-error :package ,name
                      :format-control "No package named ~S exists."
                      :format-arguments (list ,name)))
             (setq *package* pkg)
             pkg))))

;;; --- with-open-stream ---

(setf (gethash 'with-open-stream *macros*)
      (lambda (form)
        ;; (with-open-stream (var stream-expr) declaration* form*)
        (let* ((spec (cadr form))
               (var (car spec))
               (stream-expr (cadr spec))
               (body (cddr form)))
          (multiple-value-bind (decls real-body) (extract-declarations body)
            `(let ((,var ,stream-expr))
               ,@decls
               (unwind-protect
                 (progn ,@real-body)
                 (when ,var (close ,var))))))))

;;; --- with-open-file ---

(setf (gethash 'with-open-file *macros*)
      (lambda (form)
        ;; (with-open-file (var path &rest options) declaration* form*)
        (let* ((spec (cadr form))
               (var (car spec))
               (path (cadr spec))
               (options (cddr spec))
               (body (cddr form)))
          (multiple-value-bind (decls real-body) (extract-declarations body)
            `(let ((,var (open ,path ,@options)))
               ,@decls
               (unwind-protect
                 (progn ,@real-body)
                 (when ,var (close ,var))))))))

;;; --- with-output-to-string ---

(setf (gethash 'with-output-to-string *macros*)
      (lambda (form)
        ;; (with-output-to-string (var &optional string-form &key element-type) decl* form*)
        (let* ((spec (cadr form))
               (var (car spec))
               (rest-spec (cdr spec))
               (string-form nil)
               (element-type nil))
          ;; Parse optional string-form and keyword args from the spec
          (when rest-spec
            (let ((first (car rest-spec)))
              (cond
                ;; keyword arg directly (no string-form)
                ((and (symbolp first)
                      (let ((n (symbol-name first)))
                        (or (string= n "ELEMENT-TYPE")
                            (string= n ":ELEMENT-TYPE"))))
                 ;; first is :element-type keyword
                 (setq element-type (cadr rest-spec)))
                (t
                 ;; first is string-form
                 (setq string-form first)
                 ;; check for keyword args after string-form
                 (let ((kw-rest (cdr rest-spec)))
                   (loop
                     (when (null kw-rest) (return))
                     (let ((k (car kw-rest)))
                       (cond
                         ((and (symbolp k)
                               (let ((n (symbol-name k)))
                                 (or (string= n "ELEMENT-TYPE")
                                     (string= n ":ELEMENT-TYPE"))))
                          (setq element-type (cadr kw-rest))
                          (setq kw-rest (cddr kw-rest)))
                         (t (setq kw-rest (cddr kw-rest)))))))))))
          (multiple-value-bind (decls real-body) (extract-declarations (cddr form))
            (if string-form
                ;; String-form provided: write to existing string, return last body form value
                ;; element-type must still be evaluated for side effects (CLHS: left-to-right)
                ;; Evaluation order: string-form, then element-type
                (if element-type
                    (let ((str-var (gensym "STR"))
                          (et-var (gensym "ET")))
                      `(let* ((,str-var ,string-form)
                              (,et-var ,element-type)
                              (,var (make-string-output-stream-to-string ,str-var)))
                         (declare (ignore ,et-var))
                         ,@decls
                         (unwind-protect
                           (progn ,@real-body)
                           (close ,var))))
                    `(let ((,var (make-string-output-stream-to-string ,string-form)))
                       ,@decls
                       (unwind-protect
                         (progn ,@real-body)
                         (close ,var))))
                ;; No string-form: create new stream, return accumulated string
                `(let ((,var (make-string-output-stream
                               ,@(when element-type `(:element-type ,element-type)))))
                   ,@decls
                   (unwind-protect
                     (progn ,@real-body
                            (get-output-stream-string ,var))
                     (close ,var))))))))

;;; --- with-input-from-string ---

(setf (gethash 'with-input-from-string *macros*)
      (lambda (form)
        ;; (with-input-from-string (var string &key index start end) &body body)
        (let* ((spec (cadr form))
               (var (car spec))
               (str (cadr spec))
               (opts (cddr spec))
               (body (cddr form))
               (index-form nil)
               (start-form nil)
               (end-form nil))
          ;; Parse keyword args from spec
          (do ((o opts (cddr o)))
              ((null o))
            (let ((key (car o))
                  (val (cadr o)))
              (cond ((eq key :index) (setf index-form val))
                    ((eq key :start) (setf start-form val))
                    ((eq key :end)   (setf end-form val)))))
          ;; Build make-string-input-stream call
          (let ((stream-args (list str)))
            (when (or start-form end-form)
              (setf stream-args (append stream-args (list (or start-form 0))))
              (when end-form
                (setf stream-args (append stream-args (list end-form)))))
            (if index-form
                ;; With :index, set the index place to the stream position after body
                (let ((stream-var (gensym "STREAM")))
                  `(let ((,stream-var (make-string-input-stream ,@stream-args)))
                     (multiple-value-prog1
                       (let ((,var ,stream-var))
                         ,@body)
                       (setf ,index-form (file-position ,stream-var)))))
                `(let ((,var (make-string-input-stream ,@stream-args)))
                   ,@body))))))

;;; --- case / ecase / ccase ---

(setf (gethash 'case *macros*)
      (lambda (form)
        ;; (case keyform (keys body...)...)
        ;; Per CL spec: nil as key-list means empty (no match possible)
        ;; Empty body in a matching clause returns nil
        (let ((keyvar (gensym "KEY"))
              (keyform (cadr form))
              (clauses (cddr form)))
          `(let ((,keyvar ,keyform))
             (cond
               ,@(remove nil
                   (mapcar (lambda (clause)
                             (let ((keys (car clause))
                                   (body (cdr clause)))
                               (let ((result (if (null body) '(nil) body)))
                                 (cond
                                   ((member keys '(t otherwise) :test #'eq)
                                    `(t ,@result))
                                   ((null keys)
                                    nil) ; empty key list: never matches
                                   ((consp keys)
                                    `((member ,keyvar '(,@keys) :test #'eql) ,@result))
                                   (t
                                    `((eql ,keyvar ',keys) ,@result))))))
                           clauses)))))))

;;; Helper: collect all case clause keys (nil key = empty, consp key = list)
(defun %case-all-keys (clauses)
  (apply #'append
         (mapcar (lambda (c)
                   (let ((k (car c)))
                     (cond ((null k) '())
                           ((consp k) (coerce k 'list))
                           (t (list k)))))
                 clauses)))

;;; Helper: convert a case clause to a cond clause
(defun %case-to-cond (keyvar clause &optional wrap-return)
  (let* ((keys (car clause))
         (body (cdr clause))
         (result (if (null body) '(nil) body))
         (wrapped (if wrap-return `((return (progn ,@result))) result)))
    (cond
      ((null keys) nil)
      ((consp keys) `((member ,keyvar '(,@keys) :test #'eql) ,@wrapped))
      (t `((eql ,keyvar ',keys) ,@wrapped)))))

(setf (gethash 'ecase *macros*)
      (lambda (form)
        (let ((keyvar (gensym "KEY"))
              (keyform (cadr form))
              (clauses (cddr form)))
          (let ((all-keys (%case-all-keys clauses)))
            `(let ((,keyvar ,keyform))
               (cond
                 ,@(remove nil (mapcar (lambda (c) (%case-to-cond keyvar c)) clauses))
                 (t (error 'type-error :datum ,keyvar
                           :expected-type '(member ,@all-keys)))))))))

(setf (gethash 'ccase *macros*)
      (lambda (form)
        ;; ccase signals a correctable type-error with STORE-VALUE restart
        (let ((keyvar (gensym "KEY"))
              (keyform (cadr form))
              (clauses (cddr form)))
          (let ((all-keys (%case-all-keys clauses)))
            `(let ((,keyvar ,keyform))
               (loop
                 (cond
                   ,@(remove nil (mapcar (lambda (c) (%case-to-cond keyvar c t)) clauses))
                   (t (restart-case
                        (error 'type-error :datum ,keyvar
                               :expected-type '(member ,@all-keys))
                        (store-value (new-val)
                          :report "Supply a new value"
                          (setf ,keyvar new-val)))))))))))

;;; --- prog1 / prog2 (ensure they exist) ---

;;; --- fresh-line --- (now compiled directly in cil-compiler.lisp)

;;; --- with-compilation-unit (no-op) ---
(setf (gethash 'with-compilation-unit *macros*)
      (lambda (form)
        ;; (with-compilation-unit (&key override) body...) → progn body...
        `(progn ,@(cddr form))))

;;; --- with-standard-io-syntax ---

(setf (gethash 'with-standard-io-syntax *macros*)
      (lambda (form)
        (let ((body (cdr form)))
          `(let ((*package* (find-package "CL-USER"))
                 (*print-array* t)
                 (*print-base* 10)
                 (*print-case* :upcase)
                 (*print-circle* nil)
                 (*print-escape* t)
                 (*print-gensym* t)
                 (*print-length* nil)
                 (*print-level* nil)
                 (*print-lines* nil)
                 (*print-miser-width* nil)
                 (*print-pretty* nil)
                 (*print-radix* nil)
                 (*print-readably* nil)
                 (*print-right-margin* nil)
                 (*read-base* 10)
                 (*read-default-float-format* 'single-float)
                 (*read-eval* t)
                 (*read-suppress* nil)
                 (*readtable* (copy-readtable nil))
                 (*print-pprint-dispatch* (copy-pprint-dispatch nil)))
             ,@body))))

;;; --- multiple-value-setq (placeholder) ---

(setf (gethash 'multiple-value-setq *macros*)
      (lambda (form)
        ;; (multiple-value-setq (var1 var2 ...) values-form)
        ;; Correct expansion: evaluate ALL place subforms FIRST (before values-form),
        ;; then evaluate values-form, then store. This matches (setf (values ...) form).
        (let* ((vars (cadr form))
               (values-form (caddr form))
               (let-bindings '())   ; place temp bindings (evaluated before values-form)
               (store-vars '())     ; store variables for multiple-value-bind
               (setters '()))       ; setters (in order)
          ;; Process each variable left-to-right
          (dolist (v vars)
            (let ((sm-exp (lookup-symbol-macro v)))
              (if sm-exp
                  ;; Symbol macro: use get-setf-expansion to properly order side effects
                  (let* ((exp (multiple-value-list (%get-setf-expansion sm-exp)))
                         (temps (first exp))
                         (vals (second exp))
                         (stores (third exp))
                         (setter (fourth exp)))
                    ;; Collect place temp bindings (evaluated before values-form)
                    (dolist (pair (mapcar #'list temps vals))
                      (push pair let-bindings))
                    ;; Collect store vars and setter
                    (dolist (s stores) (push s store-vars))
                    (push setter setters))
                  ;; Regular variable: bind a store gensym and setq
                  (let ((sv (gensym "MVS")))
                    (push sv store-vars)
                    (push `(setq ,v ,sv) setters)))))
          ;; Build expansion with correct evaluation order:
          ;; 1. Place temps evaluated first
          ;; 2. Values-form evaluated second
          ;; 3. Setters run
          (let ((final-let-bindings (nreverse let-bindings))
                (final-store-vars (nreverse store-vars))
                (final-setters (nreverse setters)))
            (if (null vars)
                (let ((rv (gensym "MVSQ")))
                  `(multiple-value-bind (,rv) ,values-form ,rv))
                `(let* ,final-let-bindings
                   (multiple-value-bind ,final-store-vars ,values-form
                     ,@final-setters
                     ,(car final-store-vars))))))))

;;; --- nth-value ---

(setf (gethash 'nth-value *macros*)
      (lambda (form)
        `(nth ,(cadr form) (multiple-value-list ,(caddr form)))))

;;; --- psetq ---
;;; Parallel setq: evaluates all values first, then assigns.
;;; (psetq a e1 b e2) → (let ((#:ta e1) (#:tb e2)) (setq a #:ta b #:tb) nil)

(setf (gethash 'psetq *macros*)
      (lambda (form)
        (let* ((pairs (cdr form)))
          (if (null pairs)
              'nil
              (let* ((vars (loop for v in pairs by #'cddr collect v))
                     (vals (loop for v in (cdr pairs) by #'cddr collect v)))
                ;; Per CL spec: if any var is a symbol-macro, treat as PSETF
                ;; with each var replaced by its symbol-macro expansion.
                (let ((expanded-vars
                       (mapcar (lambda (v)
                                 (or (cdr (assoc v *symbol-macros* :test #'eq))
                                     (and (symbol-package v)
                                          (cdr (assoc (symbol-name v) *symbol-macros*
                                                      :key (lambda (k) (if (and (symbolp k) (symbol-package k))
                                                                           (symbol-name k) nil))
                                                      :test #'string=)))
                                     v))
                               vars)))
                  (if (some (lambda (v ev) (not (eq v ev))) vars expanded-vars)
                      ;; Has symbol-macros: delegate to psetf with expanded places
                      `(psetf ,@(loop for ev in expanded-vars for val in vals
                                      append (list ev val)))
                      ;; Normal psetq: capture values then assign
                      (let ((tmps (mapcar (lambda (v) (declare (ignore v)) (gensym "PSETQ")) vars)))
                        `(let (,@(mapcar #'list tmps vals))
                           (setq ,@(loop for var in vars for tmp in tmps
                                         append (list var tmp)))
                           nil)))))))))

;;; --- with-hash-table-iterator ---
;;; (with-hash-table-iterator (name hash-table) body...)
;;; Defines a local function NAME that iterates over the hash table.
;;; Each call returns: (values more? key value)
(setf (gethash 'with-hash-table-iterator *macros*)
      (lambda (form)
        (let ((name (caadr form))
              (ht-expr (cadadr form))
              (body (cddr form)))
          (let ((pairs-var (gensym "PAIRS"))
                (ptr-var (gensym "PTR"))
                (fn-var (gensym "HTITER")))
            `(let* ((,pairs-var (hash-table-pairs ,ht-expr))
                    (,ptr-var ,pairs-var))
               (flet ((,fn-var ()
                        (if ,ptr-var
                            (let ((pair (car ,ptr-var)))
                              (setf ,ptr-var (cdr ,ptr-var))
                              (values t (car pair) (cdr pair)))
                            nil)))
                 (macrolet ((,name (&rest args)
                              (cons ',fn-var args)))
                   ,@body)))))))

;;; --- psetf ---
;;; Parallel setf: evaluates all new values first, then does all assignments.
;;; (psetf p1 v1 p2 v2 ...) → (let ((#:t1 v1) (#:t2 v2) ...) (setf p1 #:t1) (setf p2 #:t2) ... nil)
(setf (gethash 'psetf *macros*)
      (lambda (form)
        ;; (psetf p1 v1 p2 v2 ...) — evaluates left-to-right (p1 subforms, v1, p2 subforms, v2, ...),
        ;; then does all assignments in parallel.
        (let* ((pairs (cdr form)))
          (if (null pairs)
              'nil
              (let* ((places (loop for p in pairs by #'cddr collect p))
                     (vals   (loop for p in (cdr pairs) by #'cddr collect p))
                     (expansions
                      (mapcar (lambda (p)
                                (multiple-value-list (%get-setf-expansion p)))
                              places))
                     ;; Interleave: (place1-bindings val1-binding place2-bindings val2-binding ...)
                     (val-tmps
                      (mapcar (lambda (v) (declare (ignore v)) (gensym "PSETF-V")) vals))
                     (interleaved-bindings
                      (apply #'append
                             (mapcar (lambda (exp val val-tmp)
                                       (append
                                        (mapcar #'list (first exp) (second exp))
                                        (list (list val-tmp val))))
                                     expansions vals val-tmps)))
                     (assignments
                      (mapcar (lambda (exp val-tmp)
                                (let ((stores (third exp))
                                      (setter (fourth exp)))
                                  (if (cdr stores)
                                      ;; Multi-store (values place): use multiple-value-bind
                                      `(multiple-value-bind ,stores ,val-tmp
                                         ,setter)
                                      ;; Single store
                                      `(let ((,(car stores) ,val-tmp))
                                         ,setter))))
                              expansions val-tmps)))
                `(let* (,@interleaved-bindings)
                   ,@assignments
                   nil))))))

;;; --- prog / prog* ---
;;; (prog (var-bindings) tag/form...) ≡ (block nil (let (var-bindings) (tagbody tag/form...)))
;;; (prog* (var-bindings) tag/form...) ≡ (block nil (let* (var-bindings) (tagbody tag/form...)))
(setf (gethash 'prog *macros*)
      (lambda (form)
        (let ((bindings (cadr form))
              (body     (cddr form)))
          ;; Lift leading declares from body into the let's body so extract-specials can see them
          (let ((decls nil) (remaining body))
            (loop while (and remaining (consp (car remaining)) (eq (caar remaining) 'declare))
                  do (push (pop remaining) decls))
            `(block nil
               (let ,bindings
                 ,@(nreverse decls)
                 (tagbody ,@remaining)))))))

(setf (gethash 'prog* *macros*)
      (lambda (form)
        (let ((bindings (cadr form))
              (body     (cddr form)))
          ;; Lift leading declares from body into the let*'s body
          (let ((decls nil) (remaining body))
            (loop while (and remaining (consp (car remaining)) (eq (caar remaining) 'declare))
                  do (push (pop remaining) decls))
            `(block nil
               (let* ,bindings
                 ,@(nreverse decls)
                 (tagbody ,@remaining)))))))

;;; --- progv ---
;;; Dynamically bind symbols to values for duration of body.
;;; Uses DynamicBindings.ProgvBind/ProgvUnbind for true dynamic binding.
(setf (gethash 'progv *macros*)
      (lambda (form)
        (let ((syms-expr (cadr form))
              (vals-expr (caddr form))
              (body (cdddr form)))
          (let ((syms-var (gensym "PSYMS")))
            `(let ((,syms-var ,syms-expr))
               (%progv-bind ,syms-var ,vals-expr)
               (unwind-protect
                 (progn ,@body)
                 (%progv-unbind ,syms-var)))))))

;;; MULTIPLE-VALUE-CALL: evaluate fn-form, collect all values from each arg-form,
;;; and apply fn to the combined list of values.
;;; Expands to: (apply fn (append (multiple-value-list a1) (multiple-value-list a2) ...))
;;; multiple-value-call is a special operator (CLHS 3.1.2.1.2.1) — handled in cil-compiler.lisp

;;; --- pprint-logical-block ---
;;; (pprint-logical-block (stream-sym list &key prefix per-line-prefix suffix) &body body)
;;; Implements pprint-pop and pprint-exit-if-list-exhausted as local macros.
(setf (gethash 'pprint-logical-block *macros*)
      (lambda (form)
        (let* ((header (cadr form))
               (body (cddr form))
               (stream-sym (car header))
               (list-form (cadr header))
               ;; Parse keyword args from header (3rd element onward)
               (rest-header (cddr header))
               (prefix nil)
               (per-line-prefix nil)
               (suffix nil))
          ;; Extract :prefix/:per-line-prefix/:suffix from rest-header
          (do ((r rest-header (cddr r)))
              ((null r))
            (cond ((eq (car r) :prefix) (setf prefix (cadr r)))
                  ((eq (car r) :per-line-prefix) (setf per-line-prefix (cadr r)))
                  ((eq (car r) :suffix) (setf suffix (cadr r)))))
          ;; If stream-sym is NIL, bind it to *standard-output*
          (let ((actual-stream (if (null stream-sym)
                                   (gensym "PPRINT-STREAM")
                                   stream-sym))
                (list-var (gensym "PPRINT-LIST"))
                (count-var (gensym "PPRINT-COUNT")))
            `(let ((,actual-stream ,(if (null stream-sym)
                                        '*standard-output*
                                        actual-stream)))
               (declare (ignorable ,actual-stream))
               (let ((,list-var ,list-form)
                     (,count-var 0))
                 (declare (ignorable ,count-var))
                 ,@(when (or prefix per-line-prefix)
                     `((let ((.pfx. ,(or prefix per-line-prefix)))
                         (unless (stringp .pfx.)
                           (error 'type-error :datum .pfx. :expected-type 'string)))))
                 ,@(when suffix
                     `((let ((.sfx. ,suffix))
                         (unless (stringp .sfx.)
                           (error 'type-error :datum .sfx. :expected-type 'string)))))
                 ;; *print-circle* support: scan list and check for circle labels
                 (let ((.circle-started. (%pprint-circle-scan ,list-var)))
                   (unwind-protect
                     (let ((.circle-label. (%pprint-circle-check ,list-var)))
                       (cond
                         ;; Back-reference (#n#): print label and skip entire block
                         ((and (stringp .circle-label.)
                               (> (length .circle-label.) 0)
                               (char= (char .circle-label. (1- (length .circle-label.))) #\#))
                          (write-string .circle-label. ,actual-stream))
                         ;; Normal or forward-reference: print label prefix if any, then block
                         (t
                          (when (stringp .circle-label.)
                            (write-string .circle-label. ,actual-stream))
                          (if (not (listp ,list-var))
                              (write ,list-var :stream ,actual-stream)
                              ;; Check *print-level*
                              (let ((.plevel. (if (boundp '*%pprint-level*) *%pprint-level* 0)))
                                (if (and *print-level* (>= .plevel. *print-level*))
                                    (write-string "#" ,actual-stream)
                                    (let ((*%pprint-level* (1+ .plevel.)))
                                      (declare (special *%pprint-level*))
                                      ,@(when (or prefix per-line-prefix)
                                          `((when *print-pretty*
                                              (write-string ,(or prefix per-line-prefix) ,actual-stream))))
                                      (when *print-pretty*
                                        (%pprint-start-block ,actual-stream
                                          ,(if (or prefix per-line-prefix) `(length ,(or prefix per-line-prefix)) 0)
                                          ,@(when per-line-prefix (list per-line-prefix))))
                                      (unwind-protect
                                        (block nil
                                          (macrolet ((pprint-exit-if-list-exhausted ()
                                                       (list 'when (list 'null ',list-var) '(return)))
                                                     (pprint-pop ()
                                                       (list 'progn
                                                             (list 'when (list 'and '*print-length*
                                                                           (list '>= ',count-var '*print-length*))
                                                                   (list 'write-string "..." ',actual-stream)
                                                                   '(return))
                                                             (list 'setf ',count-var (list '+ ',count-var 1))
                                                             ;; Circle check: if list-var is a cons back-reference, print ". #n#" and stop
                                                             (list 'let (list (list '.cdr-circle. (list '%pprint-circle-check ',list-var)))
                                                               (list 'when (list 'and (list 'stringp '.cdr-circle.)
                                                                                      (list '> (list 'length '.cdr-circle.) 0)
                                                                                      (list 'char= (list 'char '.cdr-circle. (list '1- (list 'length '.cdr-circle.))) '#\#))
                                                                     (list 'write-string ". " ',actual-stream)
                                                                     (list 'write-string '.cdr-circle. ',actual-stream)
                                                                     (list 'setf ',list-var nil)
                                                                     '(return)))
                                                             (list 'cond
                                                                   (list (list 'null ',list-var) nil)
                                                                   (list (list 'not (list 'consp ',list-var))
                                                                         (list 'write-string ". " ',actual-stream)
                                                                         (list 'write ',list-var :stream ',actual-stream)
                                                                         (list 'setf ',list-var nil)
                                                                         '(return))
                                                                   (list t (list 'prog1 (list 'car ',list-var)
                                                                                 (list 'setf ',list-var (list 'cdr ',list-var))))))))
                                            ,@body))
                                        (when *print-pretty*
                                          (%pprint-end-block)))
                                      ,@(when suffix
                                          `((when *print-pretty*
                                              (write-string ,suffix ,actual-stream)))))))))))
                     (when .circle-started. (%pprint-circle-end))))
                 nil))))))

;;; --- print-unreadable-object ---
;;; (print-unreadable-object (object stream &key type identity) &body body)
(setf (gethash 'print-unreadable-object *macros*)
      (lambda (form)
        (let* ((header (cadr form))
               (body (cddr form))
               (object-form (car header))
               (stream-form (cadr header))
               ;; Parse keyword args from header (3rd element onward)
               (rest-header (cddr header))
               (type-flag nil)
               (identity-flag nil)
               (obj-var (gensym "OBJ"))
               (stream-var (gensym "STREAM")))
          ;; Extract :type and :identity from rest-header
          (do ((r rest-header (cddr r)))
              ((null r))
            (cond ((eq (car r) :type) (setf type-flag (cadr r)))
                  ((eq (car r) :identity) (setf identity-flag (cadr r)))))
          `(let ((,obj-var ,object-form)
                 (,stream-var (let ((.s. ,stream-form))
                                (cond ((eq .s. t) *terminal-io*)
                                      ((null .s.) *standard-output*)
                                      (t .s.)))))
             (when *print-readably*
               (error 'print-not-readable :object ,obj-var))
             (write-string "#<" ,stream-var)
             ,@(when type-flag
                 `((format ,stream-var "~A" (type-of ,obj-var))
                   (write-char #\Space ,stream-var)))
             ,@(when body
                 `((progn ,@body)))
             ,@(when identity-flag
                 `((write-char #\Space ,stream-var)
                   (format ,stream-var "~D" (%object-id ,obj-var))))
             (write-char #\> ,stream-var)
             nil))))

;;; MULTIPLE-VALUE-PROG1: evaluate first-form, save all its values,
;;; evaluate remaining forms for side effects, then return saved values.
;;; multiple-value-prog1 is a special operator (CLHS 3.1.2.1.2.1) — handled in cil-compiler.lisp

;;; TIME: evaluate form, print elapsed time and GC statistics, return values
;;; Note: dotcl:gc-stats is referenced via FIND-SYMBOL so the source can be
;;; read by host SBCL during cross-compile (where package DOTCL does not exist).
(setf (gethash 'time *macros*)
      (lambda (form)
        (let ((start-var  (gensym "TIME-START"))
              (stats0-var (gensym "TIME-STATS0"))
              (result-var (gensym "TIME-RESULT"))
              (end-var    (gensym "TIME-END"))
              (stats1-var (gensym "TIME-STATS1"))
              (gc-stats   (or (find-symbol "GC-STATS" "DOTCL")
                              (intern "GC-STATS" "DOTCL")))
              (body (cadr form)))
          `(let ((,stats0-var (,gc-stats))
                 (,start-var  (get-internal-real-time)))
             (let ((,result-var (multiple-value-list ,body)))
               (let ((,end-var    (get-internal-real-time))
                     (,stats1-var (,gc-stats)))
                 (format *trace-output*
                         "~&Evaluation took:~%  ~,3F seconds of real time~%  GC: gen0 +~D, gen1 +~D, gen2 +~D~%  ~:D bytes allocated~%"
                         (/ (- ,end-var ,start-var) (float internal-time-units-per-second))
                         (- (nth 0 ,stats1-var) (nth 0 ,stats0-var))
                         (- (nth 1 ,stats1-var) (nth 1 ,stats0-var))
                         (- (nth 2 ,stats1-var) (nth 2 ,stats0-var))
                         (- (nth 4 ,stats1-var) (nth 4 ,stats0-var)))
                 (values-list ,result-var)))))))

;;; TRACE: (trace fn1 fn2 ...) → (%trace 'fn1 'fn2 ...)
(setf (gethash 'trace *macros*)
      (lambda (form)
        `(%trace ,@(mapcar (lambda (n) `',n) (cdr form)))))

;;; UNTRACE: (untrace fn1 fn2 ...) → (%untrace 'fn1 'fn2 ...)
(setf (gethash 'untrace *macros*)
      (lambda (form)
        `(%untrace ,@(mapcar (lambda (n) `',n) (cdr form)))))
