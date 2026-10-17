;;; cil-forms-let.lisp: let / let*, binding type inference, return type inference
;;; Part of the CIL compiler (A2 instruction list architecture)

(in-package :dotcl.cil-compiler)

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
   bounds fit; multiplication on such a var could produce a result outside
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

(defun extract-character-locals (body)
  "Scan (declare ...) forms at the head of BODY for CHARACTER type hints on
   locals, in both spellings: (declare (character c)) and
   (declare (type character c)). Returns a list of symbol-name strings.

   The CHAR= family is the only reader; see *CHARACTER-LOCALS*."
  (let ((result '()))
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare))
        (return))
      (dolist (decl (cdr form))
        (when (consp decl)
          (cond
            ((eq (car decl) 'character)
             (dolist (v (cdr decl))
               (when (symbolp v) (pushnew (var-name v) result :test #'string=))))
            ((and (eq (car decl) 'type) (eq (cadr decl) 'character))
             (dolist (v (cddr decl))
               (when (symbolp v) (pushnew (var-name v) result :test #'string=))))))))
    result))

(defun extract-u64-locals (body)
  "Scan the head (declare ...) forms of BODY for locals declared an integer type
   whose values need the 64th bit and fit in it ((unsigned-byte 64), (mod
   2^64), (integer 0 HI) with HI past int64). Returns a list of symbol-name
   strings. Such a local can hold its value's bits in an Int64 slot."
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
            (let ((range (declared-integer-range type)))
              (when (and range
                         (>= (car range) 0)
                         (<= (cdr range) #xFFFFFFFFFFFFFFFF)
                         (> (cdr range) +int64-max+))
                (dolist (v vars)
                  (when (symbolp v)
                    (pushnew (var-name v) result :test #'string=)))))))))
    result))

(defun extract-small-int-locals (body)
  "Scan head (declare ...) forms for bounded integer type hints,
   (signed-byte N) / (unsigned-byte N) / bit / (integer LO HI), on locals.
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
            (let ((range (declared-integer-range type)))
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

(defun %inline-int-param-use-p (form var)
  "True when FORM calls a recorded INLINE function, as (F ARGS...) or
   (FUNCALL (FUNCTION F) ARGS...), and passes VAR itself as an argument whose
   parameter the definition declares FIXNUM or a bounded integer type. Only for
   an all-required lambda list of the same length, the one shape
   MAYBE-EXPAND-INLINE substitutes; anything else answers NIL."
  (multiple-value-bind (name args)
      (cond ((and (consp form) (eq (car form) 'funcall)
                  (consp (cdr form)) (consp (cadr form))
                  (eq (car (cadr form)) 'function)
                  (consp (cdr (cadr form))))
             (values (cadr (cadr form)) (cddr form)))
            ((and (consp form) (symbolp (car form)) (car form))
             (values (car form) (cdr form)))
            (t (values nil nil)))
    (let ((def (and name
                    (or (symbolp name) (%setf-function-name-p name))
                    (consp args)
                    (null (cdr (last args)))
                    (member var args :test #'eq)
                    (gethash name *inline-defs*))))
      (when def
        (let ((params (car def)) (body (cdr def)))
          (when (and (listp params)
                     (every #'symbolp params)
                     (notany (lambda (p) (member p lambda-list-keywords)) params)
                     (= (length params) (length args)))
            (let ((int-names (append (extract-fixnum-locals body)
                                     (mapcar #'car (extract-small-int-locals body)))))
              (loop for p in params
                    for a in args
                    thereis (and (eq a var)
                                 (member (var-name p) int-names
                                         :test #'string=))))))))))

(defun %native-int-use-p (var body)
  "True when BODY syntactically reads VAR as an argument of a +NATIVE-INT-OPS+
   form. Gates the range-proven Int64 slot promotion in COMPILE-LET: a raw slot
   pays for itself only when some read skips an unbox, and a variable used purely
   in generic positions would just gain a Fixnum.Make on every read.

   Deliberately syntactic and under-approximating; BODY is unexpanded source
   here, so a use that only appears after macroexpansion is not seen and the
   binding simply stays boxed. Over-approximating in the other direction is
   harmless too: a wrong guess costs performance, never correctness, since both
   representations of the slot are fully compiled for.

   A call of a recorded INLINE function counts too, (F ... VAR ...) or
   (FUNCALL #'F ... VAR ...) with F a symbol or (SETF G), when the parameter
   VAR is passed as is declared an integer type: the substituted body binds that
   parameter to a raw slot, so VAR is read without an unbox. This is what the
   SETF expansion of an INLINE setf function looks like: its place arguments are
   bound to temps that are only passed on (see %INLINE-INT-PARAM-USE-P)."
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
                      ((%inline-int-param-use-p form var) t)
                      (t (do ((r form (cdr r)))
                             ((not (consp r)) nil)
                           (when (walk (car r)) (return t)))))))))
    (do ((r body (cdr r)))
        ((not (consp r)) nil)
      (when (walk (car r)) (return t)))))

(defparameter +native-float-ops+
  '(+ - * / 1+ 1- = /= < > <= >= min max abs zerop plusp minusp the
    floor truncate ceiling round ffloor ftruncate fceiling fround))

(defun %inline-single-param-use-p (form var)
  "True when FORM calls an INLINE local function passing VAR for a parameter
   declared SINGLE-FLOAT."
  (let* ((head (car form))
         (plan (and (symbolp head) head (nth 5 (local-function-entry head)))))
    (and plan
         (loop for p in (second plan)
               for a in (cdr form)
               thereis (and (eq a var) (%single-declared-p p (third plan)))))))

(defun %native-float-use-p (var forms)
  "True when FORMS read VAR as an argument of a +NATIVE-FLOAT-OPS+ form, or
   pass it to an INLINE local function's SINGLE-FLOAT parameter."
  (labels ((walk (form)
             (and (consp form)
                  (let ((head (car form)))
                    (cond
                      ((eq head 'quote) nil)
                      ((and (member head +native-float-ops+ :test #'eq)
                            (do ((a (cdr form) (cdr a)))
                                ((not (consp a)) nil)
                              (when (eq (car a) var) (return t))))
                       t)
                      ((%inline-single-param-use-p form var) t)
                      (t (do ((r form (cdr r)))
                             ((not (consp r)) nil)
                           (when (walk (car r)) (return t)))))))))
    (some #'walk forms)))

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

;;; ------------------------------------------------------------
;;; The type of an assigned LET variable: its init and every assignment
;;; ------------------------------------------------------------
;;;
;;; INFER-SMALL-INT-BINDINGS and the induction declarations type a binding from
;;; its init alone, so a variable the body assigns stays a LispObject: the
;;; registers of a hash (A = (MOD32+ A ...)) are boxed at every assignment.
;;; Here the body is walked once, with its macros expanded, collecting each
;;; value assigned to each variable bound in it as a small expression over the
;;; ranges of those variables (an IR). The ranges are then solved together,
;;; starting from nothing and growing each variable's range to cover its
;;; sources until nothing changes; a variable still growing after a few rounds
;;; is given up (a counter has no bound). A variable whose range comes out
;;; finite is declared that type, and the existing declared paths take over
;;; (a raw Int64 slot, or the (UNSIGNED-BYTE 64) bits slot).
;;;
;;; The walk gives up on the whole LET (no declaration) at anything it does not
;;; model: a special form it does not know, an FLET that hides a macro, a body
;;; too large. A form it does not understand otherwise only has an unknown
;;; value. Names are resolved the way the compiler does (VAR-NAME); a reference
;;; that matches a binding by name but not by identity is unknown.
;;;
;;; IR: (:K LO . HI) a known range, (:CELL . C) the range of variable C,
;;; (:OP OP ARG...) arithmetic, (:U A B) either, (:MEET A LO . HI) A within a
;;; known range, :TOP unknown.
;;; A cell is #(SYM DECL SOURCES TOP CUR NATIVE READS): DECL a declared range
;;; (then the sources do not matter), SOURCES the IR of the init and of every
;;; assigned value, TOP true when some value is unknown, CUR the solved range
;;; or :EMPTY, NATIVE the number of reads where an integer is used raw (an
;;; operand of arithmetic, a subscript, the init of a binding declared an
;;; integer type, which is how an INLINE function's parameter reads it), READS
;;; the number of all reads. Each other read of a raw slot boxes the value
;;; (Fixnum.Make, BoxU64), where a boxed slot boxes once per assignment.

(defvar *avt-scope* nil
  "Bindings in scope during the walk, innermost first: (:VAR SYM . CELL),
   (:FN NAME) or (:BLOCK NAME . CELL), the cell collecting the values of the
   RETURN-FROMs to that block.")

(defvar *avt-budget* 0
  "Forms the walk may still visit before it gives up.")

(defvar *avt-cells* nil
  "Every cell the walk made.")

(defparameter +avt-bound+ (expt 2 130)
  "A range reaching past this is unknown: it keeps the bounds small while a
   counter is still growing.")

(defun %avt-cell (sym sources &optional decl top)
  (let ((c (vector sym decl sources top :empty 0 0)))
    (push c *avt-cells*)
    c))

(defun %avt-k (r) (if (consp r) (list* :k (car r) (cdr r)) :top))

(defun %avt-give-up () (throw '%avt-abort nil))

(defun %avt-integer-type-range (type)
  "The range of the integer TYPE declared or asserted, or NIL."
  (if (eq type 'fixnum)
      (cons +int64-min+ +int64-max+)
      (declared-integer-range type)))

(defun %avt-outer-range (form)
  "The range of FORM, which mentions no variable bound inside the walked body,
   from what the compiler knows at the LET: a proven int64 range, or an
   (UNSIGNED-BYTE 64) array element or local. NIL when unknown."
  (or (let ((r (expr-int-range form))) (and r (range-fits-int64-p r) r))
      (let ((f (fold-typed-operand form)))
        (and (or (numeric-array-aref-u64-p f)
                 (and (symbolp f) f (u64-native-local-p f)))
             (cons 0 +u64-mask+)))))

(defun %avt-lookup-var (sym)
  "The innermost scope entry binding a variable named like SYM, or NIL."
  (let ((name (var-name sym)))
    (dolist (e *avt-scope* nil)
      (when (and (eq (car e) :var)
                 (or (eq (cadr e) sym)
                     (string= (var-name (cadr e)) name)))
        (return e)))))

(defun %avt-local-fn-p (name)
  (or (dolist (e *avt-scope* nil)
        (when (and (eq (car e) :fn) (equal (cadr e) name)) (return t)))
      (local-function-entry name)))

(defun %avt-decls (body)
  "(VALUES TYPES SPECIALS) of the leading declarations of BODY: TYPES an alist
   (SYM . RANGE) of the integer types declared, SPECIALS the names declared
   special."
  (let ((types '()) (specials '()))
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare)) (return))
      (dolist (d (cdr form))
        (when (consp d)
          (cond ((eq (car d) 'special)
                 (dolist (v (cdr d)) (push v specials)))
                ((member (car d) '(ignore ignorable dynamic-extent inline notinline
                                   optimize ftype)))
                (t
                 (multiple-value-bind (type vars)
                     (if (eq (car d) 'type)
                         (values (cadr d) (cddr d))
                         (values (car d) (cdr d)))
                   (let ((r (ignore-errors (%avt-integer-type-range type))))
                     (when r
                       (dolist (v vars)
                         (when (symbolp v) (push (cons v r) types)))))))))))
    (values types specials)))

(defun %avt-strip-decls (body)
  (loop while (and (consp body) (consp (car body)) (eq (caar body) 'declare))
        do (pop body))
  body)

(defmacro %avt-with-binding ((sym cell) &body body)
  "Run BODY with SYM bound to CELL, and no symbol macro of that name visible."
  `(let ((*avt-scope* (cons (list* :var ,sym ,cell) *avt-scope*))
         (*symbol-macros* (if (assoc ,sym *symbol-macros* :test #'eq)
                              (remove ,sym *symbol-macros* :key #'car :test #'eq)
                              *symbol-macros*)))
     ,@body))

(defun %avt-bind-cells (syms inits decl-types specials)
  "Cells for the variables SYMS with the init IRs INITS."
  (loop for s in syms
        for init in inits
        collect (let ((special (or (member s specials :test #'eq)
                                   (global-special-p s))))
                  (%avt-cell s (list init)
                             (and (not special) (cdr (assoc s decl-types :test #'eq)))
                             special))))

(defun %avt-shadow-specials (specials forms)
  "Walk FORMS (no declarations) with each of SPECIALS, names declared special
   there, bound to an unknown cell: a reference to one is the dynamic variable,
   not a binding of the walk."
  (if (null specials)
      (%avt-progn forms)
      (%avt-with-binding ((car specials) (%avt-cell (car specials) nil nil t))
        (%avt-shadow-specials (cdr specials) forms))))

(defun %avt-body (body)
  "Walk BODY, which may start with declarations."
  (%avt-shadow-specials (nth-value 1 (%avt-decls body)) (%avt-strip-decls body)))

(defun %avt-progn (forms)
  "Walk FORMS in order; the IR of the last one (:TOP for none)."
  (let ((ir :top))
    (dolist (f forms ir)
      (unless (and (consp f) (eq (car f) 'declare))
        (setf ir (%avt-form f))))))

(defun %avt-let (bindings body sequential-p)
  (let ((parsed (mapcar (lambda (b)
                          (cond ((symbolp b) (list b nil))
                                ((and (consp b) (symbolp (car b))) (list (car b) (cadr b)))
                                (t (%avt-give-up))))
                        bindings)))
    (multiple-value-bind (decl-types specials) (%avt-decls body)
      ;; An init read into a binding declared an integer type is read raw.
      (dolist (p parsed)
        (when (assoc (car p) decl-types :test #'eq)
          (%avt-mark-native (cadr p))))
      (labels ((bind-rest (ps)
                 (if (null ps)
                     (%avt-body body)
                     (let* ((p (car ps))
                            (cell (car (%avt-bind-cells
                                        (list (car p))
                                        (list (if (cadr p) (%avt-form (cadr p)) :top))
                                        decl-types specials))))
                       (%avt-with-binding ((car p) cell)
                         (bind-rest (cdr ps)))))))
        (if sequential-p
            (bind-rest parsed)
            (let* ((inits (mapcar (lambda (p) (if (cadr p) (%avt-form (cadr p)) :top)) parsed))
                   (cells (%avt-bind-cells (mapcar #'car parsed) inits decl-types specials)))
              (labels ((enter (ps cs)
                         (if (null ps)
                             (%avt-body body)
                             (%avt-with-binding ((car (car ps)) (car cs))
                               (enter (cdr ps) (cdr cs))))))
                (enter parsed cells))))))))

(defun %avt-lambda-vars (ll)
  "The variables a lambda list binds, and its init forms in order, as
   (VALUES VARS INITS)."
  (let ((vars '()) (inits '()))
    (do ((l ll (cdr l)))
        ((not (consp l)))
      (let ((p (car l)))
        (cond ((member p lambda-list-keywords))
              ((symbolp p) (push p vars))
              ((consp p)
               (let ((v (car p)))
                 (cond ((symbolp v) (push v vars))
                       ((and (consp v) (consp (cdr v)) (symbolp (cadr v)))
                        (push (cadr v) vars))
                       (t (%avt-give-up))))
               (when (consp (cdr p))
                 (push (cadr p) inits)
                 (when (and (consp (cddr p)) (symbolp (caddr p)))
                   (push (caddr p) vars))))
              (t (%avt-give-up)))))
    (when (and ll (atom (last ll)) (cdr (last ll)) (symbolp (cdr (last ll))))
      (push (cdr (last ll)) vars))
    (values (nreverse vars) (nreverse inits))))

(defun %avt-lambda (ll body)
  "Walk a function: its parameters are unknown inside it."
  (multiple-value-bind (vars inits) (%avt-lambda-vars ll)
    (labels ((enter (vs)
               (if (null vs)
                   (progn (dolist (i inits) (%avt-form i))
                          (%avt-body body))
                   (%avt-with-binding ((car vs) (%avt-cell (car vs) nil nil t))
                     (enter (cdr vs))))))
      (enter vars)
      :top)))

(defun %avt-mark-native (form)
  "Record that FORM, when it is a variable bound in the walk, is read as a raw
   integer."
  (when (and form (symbolp form))
    (let ((e (%avt-lookup-var form)))
      (when (and e (eq (cadr e) form) (cddr e))
        (incf (svref (cddr e) 5))))))

(defun %avt-mark-inline-int-args (head args)
  "For a call (HEAD ARGS...) to an INLINE function, mark the arguments passed to
   parameters its definition declares an integer type: substituted, the body
   binds them raw."
  (let ((def (and (not (member head *notinline-functions*))
                  (gethash head *inline-defs*))))
    (when (and def (listp (car def)) (every #'symbolp (car def))
               (notany (lambda (p) (member p lambda-list-keywords)) (car def))
               (= (length (car def)) (length args)))
      (let ((types (%avt-decls (cdr def))))
        (loop for p in (car def)
              for a in args
              when (assoc p types :test #'eq)
                do (%avt-mark-native a))))))

(defun %avt-assign (sym ir)
  (let ((e (%avt-lookup-var sym)))
    (when e
      (let ((cell (cddr e)))
        (if (eq (cadr e) sym)
            (push ir (svref cell 2))
            (setf (svref cell 3) t))))))

(defun %avt-symbol-macro (sym)
  "(VALUES EXPANSION FOUND) for SYM as a symbol macro at this point of the walk."
  (let ((sm (assoc sym *symbol-macros* :test #'eq)))
    (cond (sm (values (cdr sm) t))
          ((%avt-lookup-var sym) (values nil nil))
          (t (lookup-symbol-macro sym)))))

(defparameter +avt-ops+
  '(+ - * 1+ 1- logand logior logxor lognot ash mod min max)
  "The operators whose result range the walk computes from its arguments'.")

(defvar *avt-inline-stack* nil
  "The local functions whose bodies the walk is inside as substituted calls.")

(defun %avt-flet-entries (defs inline-names)
  "Scope entries for the functions of an FLET. One declared INLINE carries
   (PARAMS BODY SCOPE SNAPSHOT): its call is walked as its body, in the scope
   of the definition (see %AVT-INLINE-CALL)."
  (mapcar (lambda (d)
            (let ((name (car d)) (params (cadr d)))
              (if (and (member name inline-names :test #'eq)
                       (symbolp name)
                       (listp params)
                       (every #'symbolp params)
                       (notany (lambda (p) (member p lambda-list-keywords)) params))
                  (let ((snapshot (%local-inline-snapshot (cddr d) (cons name params))))
                    (if (eq snapshot :refuse)
                        (list :fn name)
                        (list :fn name params (cddr d) *avt-scope* snapshot)))
                  (list :fn name))))
          defs))

(defun %avt-local-fn-entry (name)
  (dolist (e *avt-scope* nil)
    (when (and (eq (car e) :fn) (equal (cadr e) name)) (return e))))

(defun %avt-inline-call (entry args)
  "The IR of a call to the INLINE local function of ENTRY: its body walked with
   the parameters bound to the arguments' IRs, in the scope where it was
   written. NIL when the call cannot be walked that way."
  (destructuring-bind (name params body scope snapshot) (cdr entry)
    (when (and (= (length params) (length args))
               (not (member name *avt-inline-stack* :test #'equal))
               (%local-inline-env-matches-p snapshot))
      (let ((irs (mapcar #'%avt-form args)))
        (multiple-value-bind (decl-types specials) (%avt-decls body)
          (loop for p in params
                for a in args
                when (assoc p decl-types :test #'eq) do (%avt-mark-native a))
          (let* ((*avt-inline-stack* (cons name *avt-inline-stack*))
                 (*avt-scope* scope)
                 (cells (%avt-bind-cells params irs decl-types specials))
                 (ir (labels ((enter (ps cs)
                                (if (null ps)
                                    (%avt-shadow-specials
                                     (remove-if (lambda (s) (member s params)) specials)
                                     (list `(block ,name ,@(%avt-strip-decls body))))
                                    (%avt-with-binding ((car ps) (car cs))
                                      (enter (cdr ps) (cdr cs))))))
                       (enter params cells))))
            (loop for c in cells
                  for a in args
                  when (plusp (svref c 5)) do (%avt-mark-native a))
            ir))))))

(defun %avt-call (head args)
  (cond
    ((%avt-local-fn-p head)
     (let ((e (%avt-local-fn-entry head)))
       (or (and e (cddr e) (%avt-inline-call e args))
           (progn (dolist (a args) (%avt-form a))
                  :top))))
    ((and (member head +avt-ops+) (not (macrolet-shadowed-p head)))
     (mapc #'%avt-mark-native args)
     (let ((irs (mapcar #'%avt-form args)))
       (if (and (member head '(+ - * logand logior logxor min max)) (cddr irs))
           (reduce (lambda (a b) (list :op head a b)) irs)
           (cons :op (cons head irs)))))
    ((and (eq head 'ldb) (= (length args) 2)
          (consp (car args)) (eq (car (car args)) 'byte)
          (integerp (cadr (car args))) (<= 0 (cadr (car args)) 64))
     (%avt-form (caddr (car args)))
     (%avt-mark-native (cadr args))
     (%avt-form (cadr args))
     (list* :k 0 (1- (ash 1 (cadr (car args))))))
    ((eq head 'aref)
     (let ((arr (car args)))
       (mapc #'%avt-mark-native (cdr args))
       (dolist (a args) (%avt-form a))
       (if (and arr (symbolp arr) (not (%avt-lookup-var arr))
                (not (nth-value 1 (%avt-symbol-macro arr))))
           (%avt-k (%avt-outer-range `(aref ,arr ,@(mapcar (lambda (i) (declare (ignore i)) 0)
                                                          (cdr args)))))
           :top)))
    (t
     (let ((ret (function-return-type head))
           (inl nil))
       (cond
         ((setq inl (maybe-expand-inline head (cons head args)))
          (let ((ir (%avt-form inl))
                (r (and ret (%avt-integer-type-range ret))))
            (if r (list* :meet ir r) ir)))
         ((and ret (%avt-integer-type-range ret))
          (%avt-mark-inline-int-args head args)
          (dolist (a args) (%avt-form a))
          (%avt-k (%avt-integer-type-range ret)))
         (t (when (or (member head +native-int-ops+)
                      (member head '(logandc1 logandc2 lognand lognor logeqv
                                     logorc1 logorc2 logcount integer-length)))
              (mapc #'%avt-mark-native args))
            (dolist (a args) (%avt-form a))
            :top))))))

(defun %avt-form (form)
  "Walk FORM, recording what it assigns; its IR."
  (when (<= (decf *avt-budget*) 0) (%avt-give-up))
  (cond
    ((integerp form) (list* :k form form))
    ((and form (symbolp form) (not (eq form t)) (not (keywordp form)))
     (multiple-value-bind (exp found) (%avt-symbol-macro form)
       (if found
           (%avt-form exp)
           (let ((e (%avt-lookup-var form)))
             (cond (e (if (eq (cadr e) form)
                          (let ((c (cddr e)))
                            (when c (incf (svref c 6)))
                            (cons :cell c))
                          :top))
                   ((foldable-constant-symbol-p form)
                    (let ((v (symbol-value form)))
                      (if (integerp v) (list* :k v v) :top)))
                   (t (%avt-k (%avt-outer-range form))))))))
    ((atom form) :top)
    ((not (%stack-space-available-p)) (%avt-give-up))
    (t
     (let ((head (car form)) (args (cdr form)))
       (unless (listp args) (%avt-give-up))
       (case head
         (quote (let ((v (car args))) (if (integerp v) (list* :k v v) :top)))
         ((progn eval-when)
          (%avt-progn (if (eq head 'eval-when) (cdr args) args)))
         (locally (%avt-body args))
         (if (%avt-form (car args))
             (let ((a (%avt-form (cadr args)))
                   (b (if (cddr args) (%avt-form (caddr args)) :top)))
               (list :u a b)))
         (setq
          (let ((ir :top))
            (loop for (var val) on args by #'cddr
                  do (unless (and var (symbolp var)) (%avt-give-up))
                     (multiple-value-bind (exp found) (%avt-symbol-macro var)
                       (if found
                           (setf ir (%avt-form `(setf ,exp ,val)))
                           (let ((v (%avt-form val)))
                             (%avt-assign var v)
                             (setf ir v)))))
            ir))
         ((let let*) (%avt-let (car args) (cdr args) (eq head 'let*)))
         (the
          (let ((ir (%avt-form (cadr args)))
                (r (ignore-errors (%avt-integer-type-range (car args)))))
            (when r (%avt-mark-native (cadr args)))
            (if r (%avt-k r) ir)))
         (block
          (let ((cell (%avt-cell nil nil)))
            (let ((last (let ((*avt-scope* (cons (list* :block (car args) cell) *avt-scope*))
                              (*inlining-stack* (inline-guard-stack (car args))))
                          (%avt-progn (cdr args)))))
              (list :u last (cons :cell cell)))))
         (return-from
          (let ((ir (if (cdr args) (%avt-form (cadr args)) :top))
                (e (dolist (e *avt-scope* nil)
                     (when (and (eq (car e) :block) (eq (cadr e) (car args))) (return e)))))
            (when e (push ir (svref (cddr e) 2)))
            :top))
         (tagbody (dolist (f args) (unless (atom f) (%avt-form f))) :top)
         (go :top)
         ((catch throw unwind-protect multiple-value-call progv)
          (let ((irs (mapcar #'%avt-form args)))
            (if (eq head 'unwind-protect) (car irs) :top)))
         (multiple-value-prog1
          (let ((irs (mapcar #'%avt-form args))) (car irs)))
         (load-time-value :top)
         (function
          (let ((f (car args)))
            (when (and (consp f) (eq (car f) 'lambda))
              (%avt-lambda (cadr f) (cddr f)))
            :top))
         ((flet labels)
          (let ((defs (car args)))
            (when (%flet-macro-shadows defs) (%avt-give-up))
            (let ((fn-scope (append (if (eq head 'flet)
                                        (%avt-flet-entries defs (extract-inline (cdr args)))
                                        (mapcar (lambda (d) (list :fn (car d))) defs))
                                    *avt-scope*)))
              (dolist (d defs)
                (let ((*avt-scope* (if (eq head 'labels) fn-scope *avt-scope*)))
                  (%avt-lambda (cadr d) (cddr d))))
              (let ((*avt-scope* fn-scope))
                (%avt-body (cdr args))))))
         (macrolet
          (let ((defs (car args)) (saved '()))
            (unwind-protect
                 (progn
                   (dolist (d defs)
                     (push (cons (car d) (gethash (car d) *macros*)) saved)
                     (setf (gethash (car d) *macros*)
                           (%eval-macrolet-expander (cadr d) (cddr d))))
                   (let ((*macroexpand-scope* (cons defs *macroexpand-scope*))
                         (*macrolet-shadowed* (append (mapcar #'car defs) *macrolet-shadowed*))
                         (*lexical-operators* (%macrolet-lexical-operators defs)))
                     (%avt-body (cdr args))))
              (dolist (s saved)
                (if (cdr s)
                    (setf (gethash (car s) *macros*) (cdr s))
                    (remhash (car s) *macros*))))))
         (symbol-macrolet
          (let ((*symbol-macros* (append (mapcar (lambda (b) (cons (car b) (cadr b))) (car args))
                                         *symbol-macros*)))
            (%avt-body (cdr args))))
         (t
          (cond
            ((and (consp head) (eq (car head) 'lambda))
             (dolist (a args) (%avt-form a))
             (%avt-lambda (cadr head) (cddr head)))
            ((not (symbolp head)) (%avt-give-up))
            ((and (not (%avt-local-fn-p head)) (%typed-vector-struct-read form))
             (%avt-form (%typed-vector-struct-read form)))
            ((and (not (%avt-local-fn-p head))
                  (let ((expander (find-macro-expander head)))
                    (and expander
                         (multiple-value-bind (exp hits)
                             (%walker-macroexpand form expander :none)
                           ;; A global symbol macro resolved as a place: the
                           ;; name may be a binding of the walk.
                           (when (or (null exp) hits) (%avt-give-up))
                           (return-from %avt-form (%avt-form exp)))))))
            ((special-operator-p head) (%avt-give-up))
            ((and (gethash head *compile-form-handlers*)
                  (not (fboundp head)))
             (if (eq head '%dotimes-1+)
                 (list :op '1+ (%avt-form (car args)))
                 (%avt-give-up)))
            (t (%avt-call head args)))))))))

;;; --- Solving ---

(defun %avt-bounded (r)
  (if (and (consp r)
           (or (> (abs (car r)) +avt-bound+) (> (abs (cdr r)) +avt-bound+)))
      nil
      r))

(defun %avt-range-op (op rs)
  "The range of (OP ARG...) for the known ranges RS of the arguments, or NIL."
  (let ((a (first rs)) (b (second rs)))
    (%avt-bounded
     (case op
       ((+ *) (if b (range-arith op a b) a))
       (- (if b (range-arith '- a b) (cons (- (cdr a)) (- (car a)))))
       (1+ (cons (1+ (car a)) (1+ (cdr a))))
       (1- (cons (1- (car a)) (1- (cdr a))))
       ((logior logxor logand)
        (cond ((null b) a)
              ((and (>= (car a) 0) (>= (car b) 0) (not (eq op 'logand)))
               (cons 0 (1- (ash 1 (max (integer-length (cdr a))
                                       (integer-length (cdr b)))))))
              ;; Two values of N bits and a sign give one of N bits and a sign.
              (t (let ((n (max (integer-length (car a)) (integer-length (cdr a))
                               (integer-length (car b)) (integer-length (cdr b)))))
                   (cons (- (ash 1 n)) (1- (ash 1 n)))))))
       (lognot (cons (- -1 (cdr a)) (- -1 (car a))))
       (ash (and b (<= -256 (car b)) (<= (cdr b) 256)
                 (let ((v (list (ash (car a) (car b)) (ash (car a) (cdr b))
                                (ash (cdr a) (car b)) (ash (cdr a) (cdr b)))))
                   (cons (reduce #'min v) (reduce #'max v)))))
       (mod (and b (= (car b) (cdr b)) (> (car b) 0) (cons 0 (1- (car b)))))
       (min (if b (cons (min (car a) (car b)) (min (cdr a) (cdr b))) a))
       (max (if b (cons (max (car a) (car b)) (max (cdr a) (cdr b))) a))
       (t nil)))))

(defun %avt-cell-value (c)
  (cond ((svref c 1))
        ((svref c 3) nil)
        (t (svref c 4))))

(defun %avt-eval (ir)
  "The range of IR under the current cell ranges: (LO . HI), :EMPTY when it
   has no value yet, NIL when unknown."
  (cond
    ((eq ir :top) nil)
    ((eq (car ir) :k) (cdr ir))
    ((eq (car ir) :cell) (%avt-cell-value (cdr ir)))
    ((eq (car ir) :meet)
     (let ((a (%avt-eval (second ir))) (k (cddr ir)))
       (cond ((null a) k)
             ((eq a :empty) a)
             ((or (> (car a) (cdr k)) (< (cdr a) (car k))) k)
             (t (cons (max (car a) (car k)) (min (cdr a) (cdr k)))))))
    ((eq (car ir) :u)
     (let ((a (%avt-eval (second ir))) (b (%avt-eval (third ir))))
       (cond ((or (null a) (null b)) nil)
             ((eq a :empty) b)
             ((eq b :empty) a)
             (t (cons (min (car a) (car b)) (max (cdr a) (cdr b)))))))
    ((eq (car ir) :op)
     (let ((rs (mapcar #'%avt-eval (cddr ir))))
       (if (eq (second ir) 'logand)
           ;; LOGAND with a non-negative value stays below it, whatever the
           ;; other operand is.
           (let ((nonneg (remove-if-not (lambda (r) (and (consp r) (>= (car r) 0))) rs)))
             (cond (nonneg (cons 0 (reduce #'min (mapcar #'cdr nonneg))))
                   ((member nil rs) nil)
                   ((member :empty rs) :empty)
                   (t (%avt-range-op 'logand rs))))
           (cond ((member nil rs) nil)
                 ((member :empty rs) :empty)
                 (t (%avt-range-op (second ir) rs))))))
    (t nil)))

(defun %avt-solve (cells)
  "Solve the ranges of CELLS: each grows to cover its sources until nothing
   changes. A cell still growing after the first rounds becomes unknown, so
   the solve ends. True when it settled."
  (loop for round from 0 below (+ 8 (length cells))
        do (let ((changed nil))
             (dolist (c cells)
               (unless (or (svref c 1) (svref c 3))
                 (let ((new :empty))
                   (dolist (s (svref c 2))
                     (let ((r (%avt-eval s)))
                       (cond ((null r) (setf new nil) (return))
                             ((eq r :empty))
                             ((eq new :empty) (setf new r))
                             (t (setf new (cons (min (car new) (car r))
                                                (max (cdr new) (cdr r))))))))
                   (setf new (%avt-bounded new))
                   (unless (equal new (svref c 4))
                     (setf changed t)
                     (if (or (null new) (>= round 4))
                         (setf (svref c 3) t)
                         (setf (svref c 4) new))))))
             (unless changed (return t)))
        finally (return nil)))

(defun %avt-var-declared-p (var body)
  "True when a leading declaration of BODY says anything about VAR's type."
  (dolist (form body nil)
    (unless (and (consp form) (eq (car form) 'declare)) (return nil))
    (dolist (d (cdr form))
      (when (and (consp d)
                 (not (member (car d) '(ignore ignorable dynamic-extent inline
                                        notinline optimize ftype))))
        (when (member var (if (eq (car d) 'type) (cddr d) (cdr d)) :test #'eq)
          (return-from %avt-var-declared-p t))))))

(defun %assigned-binding-declarations (binding-info body real-body mutated captured
                                       sequential-p)
  "Type declarations for the variables of a parallel LET that its body assigns:
   (TYPE (INTEGER LO HI) VAR) or (TYPE (UNSIGNED-BYTE 64) VAR) when the init and
   every value assigned to VAR (see above) stay in that range, and the body reads
   VAR where a raw integer is used (%NATIVE-INT-USE-P). NIL when there is none.
   Not for a special, a variable a closure captures, or one with a declared type."
  (unless sequential-p
    (let ((cands (loop for b in binding-info
                       for v = (first b)
                       when (and (not (third b)) (second b)
                                 (member (var-name v) mutated :test #'string=)
                                 (not (member (var-name v) captured :test #'string=))
                                 (not (%avt-var-declared-p v body)))
                         collect v)))
      (when cands
        (let ((*avt-cells* nil)
              (*avt-scope* nil)
              (*avt-budget* 200000))
          (catch '%avt-abort
            (let* ((inits (mapcar (lambda (b) (if (second b) (%avt-form (second b)) :top))
                                  binding-info))
                   (known (loop for b in binding-info
                                for i in inits
                                thereis (and (member (first b) cands)
                                             (consp (%avt-eval i))))))
              (when known
                (multiple-value-bind (decl-types specials) (%avt-decls body)
                  (let ((cells (%avt-bind-cells (mapcar #'first binding-info) inits
                                                decl-types specials)))
                    (loop for b in binding-info
                          for c in cells
                          when (third b) do (setf (svref c 3) t))
                    (labels ((enter (bs cs)
                               (if (null bs)
                                   (%avt-shadow-specials
                                    (remove-if (lambda (s) (member s binding-info :key #'first))
                                               specials)
                                    real-body)
                                   (%avt-with-binding ((first (car bs)) (car cs))
                                     (enter (cdr bs) (cdr cs))))))
                      (enter binding-info cells))
                    (when (%avt-solve *avt-cells*)
                      (loop for b in binding-info
                            for c in cells
                            for v = (first b)
                            for r = (and (member v cands) (%avt-cell-value c))
                            ;; Only where it pays: some read is raw, and the reads
                            ;; that box the value (see the cell) are no more than
                            ;; the assignments, which box it in a LispObject slot.
                            when (and (consp r)
                                      (plusp (svref c 5))
                                      (<= (- (svref c 6) (svref c 5))
                                          (1- (length (svref c 2)))))
                              append (cond ((range-fits-int64-p r)
                                            `((type (integer ,(car r) ,(cdr r)) ,v)))
                                           ((and (>= (car r) 0) (<= (cdr r) +u64-mask+)
                                                 (not *emit-frame-locals*))
                                            `((type (unsigned-byte 64) ,v))))))))))))))))

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
            ((and (eq (car decl) 'type) (%double-float-type-spec-p (cadr decl)))
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
   (type decimal ...) / (decimal ...) forms; decimal is non-standard, so there is
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
            ((and (eq (car decl) 'type) (%single-float-type-spec-p (cadr decl)))
             (dolist (v (cddr decl))
               (when (symbolp v) (pushnew (var-name v) result :test #'string=))))))))
    result))

(defun extract-array-backing-locals (body)
  "Scan the head declare forms of BODY for variables declared
   (SIMPLE-ARRAY <integer or float type> (DIM)) of rank 1 (or a SIMPLE-STRING)
   and return
   *array-backing-locals* entries (NAME KEY BACKING-SYMBOL KIND NARROW) for those
   whose element buffer may be fetched once at binding time.

   A variable is excluded when it is boxed (a captured, assigned variable does
   not live in a plain slot) or when BODY assigns to it: a SETQ can install a
   different array, and the buffer was fetched from the first one. The
   assignment question is asked with the same walk the boxing decision uses, and
   only when some declaration qualifies, so a body with no such declaration pays
   nothing.

   Must be called with *locals* bound -- KEY is the variable's slot via
   lookup-local, which ARRAY-BACKING-ENTRY re-checks."
  (let ((cands '()))
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare)) (return))
      (dolist (decl (cdr form))
        (when (consp decl)
          ;; Both spellings: (TYPE SPEC V) and the bare (SPEC V), because CL
          ;; lets a declaration use a type name directly and (SIMPLE-STRING S)
          ;; is how a string parameter is actually written.
          (let* ((typed (eq (car decl) (quote type)))
                 (spec (if typed (cadr decl) (car decl)))
                 (vars (if typed (cddr decl) (cdr decl)))
                 (kind (or (%simple-array-backing-kind spec)
                           (%simple-string-backing-kind spec))))
            (when kind
              (dolist (v vars)
                (when (and (symbolp v) (lookup-local v) (not (boxed-var-p v))
                           ;; The :CHAR fetch is gated on the variable being read
                           ;; from inside a loop; the integer kinds are not. The
                           ;; difference is that BackingChars never signals for a
                           ;; value that is not an array (and a non-simple one is
                           ;; caught by %CHECK-SIMPLE-DECL when this declines), so
                           ;; declining to hoist a string costs only speed --
                           ;; whereas declining to hoist an array would also drop
                           ;; the TYPE-ERROR a false (SIMPLE-ARRAY ...) declaration
                           ;; gets from the fetch, which two tests pin. The reason
                           ;; the array kinds are ungated is that diagnostic, and
                           ;; this kind has no diagnostic to lose.
                           ;;
                           ;; It matters because the prologue runs at every
                           ;; binding, loop or not: a parser that SUBSEQs a fresh
                           ;; string and reads it once was paying a fetch it could
                           ;; never amortise.
                           ;;
                           ;; The float kinds are gated the same way, for the
                           ;; same reason: their fetch answers null rather than
                           ;; signalling for a vector of another element type
                           ;; (the per-element helper reads such a vector), so
                           ;; it has no diagnostic to lose either.
                           (case kind
                             (:char (%var-iterated-access-p
                                     body (var-name v) nil
                                     (function %tree-finds-schar-p)))
                             ((:f64 :f32) (%var-iterated-access-p
                                           body (var-name v) nil
                                           (function %tree-finds-aref-p)))
                             (t t)))
                  (pushnew (list (var-name v) v kind
                                 (%simple-array-store-narrow spec)
                                 (and (not (eq kind :char))
                                      (%simple-array-backing-dims spec))
                                 (let ((ty (%expand-array-type spec)))
                                   (and (consp ty) (consp (cdr ty)) (cadr ty))))
                           cands
                           :test (function string=) :key (function car)))))))))
    (when cands
      (let ((mutated (find-mutated-and-captured-vars body (mapcar #'car cands))))
        ;; :CHECKED: above (safety 0) an integer buffer's fetch also matches
        ;; the vector's element type against the declared one
        ;; (ARRAY-BACKING-PROLOGUE), so its elements are known to be in the
        ;; declared element type (%ELEMENT-STORE-RANGE).
        (loop with checked = (not (compiling-at-safety-0-p))
              for (name sym kind narrow dims elt) in cands
              unless (member name mutated :test #'string=)
                collect (list name (lookup-local sym) (gen-local "ABACK") kind
                              narrow
                              (and checked (member kind '(1 2 3 4)) :checked)
                              (mapcar (lambda (d) (if (integerp d) d (gen-local "ADIM")))
                                      dims)
                              elt))))))

(defun %make-array-typed-element-name (spec)
  "The runtime's element type name (LispVector.ElementTypeName) for the constant
   element type SPEC, when it is one whose name does not depend on a DEFTYPE and
   that has storage of its own: (UNSIGNED-BYTE N) and (SIGNED-BYTE N) for N from
   1 to 64, FIXNUM, BIT, the float and the character types. Else NIL. Mirrors
   Runtime.ParseElementTypeName for these."
  (cond ((and (consp spec) (member (car spec) '(unsigned-byte signed-byte))
              (consp (cdr spec)) (null (cddr spec))
              (integerp (cadr spec)) (<= 1 (cadr spec) 64))
         (let* ((n (cadr spec))
                (unsigned (eq (car spec) 'unsigned-byte))
                (w (find-if (lambda (w) (<= n w))
                            (if unsigned '(1 2 4 7 8 15 16 31 32 62 63 64) '(8 16 32 63 64)))))
           (cond ((and unsigned (= w 1)) "BIT")
                 ((and (not unsigned) (= w 63)) "FIXNUM")
                 (t (format nil "~A-~D" (symbol-name (car spec)) w)))))
        ((eq spec 'fixnum) "FIXNUM")
        ((eq spec 'standard-char) "BASE-CHAR")
        ((member spec '(bit character base-char single-float double-float))
         (symbol-name spec))
        ((eq spec 'short-float) "SINGLE-FLOAT")
        ((eq spec 'long-float) "DOUBLE-FLOAT")
        (t nil)))

(defun compile-make-array (args)
  "(MAKE-ARRAY DIMS :ELEMENT-TYPE 'SPEC [:INITIAL-ELEMENT X]) with SPEC one
   %MAKE-ARRAY-TYPED-ELEMENT-NAME names, the keywords in either order:
   Runtime.MakeArrayOfType[Init] with the name resolved here, instead of the
   generic function, which looks up MAKE-ARRAY, builds an argument array and
   parses the element type on every call. Anything else is the generic call."
  (let* ((keys (cdr args))
         (plist (and (member (length keys) '(2 4))
                     (loop for (k) on keys by #'cddr
                           always (and (keywordp k)
                                       (member (symbol-name k) '("ELEMENT-TYPE" "INITIAL-ELEMENT")
                                               :test #'string=)))
                     (loop for (k v) on keys by #'cddr
                           collect (cons (symbol-name k) v))))
         (spec-form (cdr (assoc "ELEMENT-TYPE" plist :test #'string=)))
         (init (assoc "INITIAL-ELEMENT" plist :test #'string=))
         (name (and plist
                    (= (length plist)
                       (length (remove-duplicates plist :key #'car :test #'string=)))
                    (consp spec-form) (eq (car spec-form) 'quote) (consp (cdr spec-form))
                    (null (cddr spec-form))
                    (%make-array-typed-element-name (cadr spec-form)))))
    (cond ((and (not name) plist
                (= (length plist)
                   (length (remove-duplicates plist :key #'car :test #'string=)))
                (consp spec-form) (eq (car spec-form) 'array-element-type)
                (consp (cdr spec-form)) (null (cddr spec-form))
                (not (local-function-entry 'array-element-type))
                (or (null init)
                    (string= (symbol-name (first keys)) "ELEMENT-TYPE")))
           (if init
               `(,@(compile-value-args (list (first args) (cadr spec-form) (cdr init)))
                 (:call "Runtime.MakeArrayLikeInit"))
               `(,@(compile-value-args (list (first args) (cadr spec-form)))
                 (:call "Runtime.MakeArrayLike"))))
          ((and name init)
           `(,@(compile-value-args (list (first args) spec-form (cdr init)))
             (:ldstr ,name)
             (:call "Runtime.MakeArrayOfTypeInit")))
          (name
           `(,@(compile-value-args (list (first args) spec-form))
             (:ldstr ,name)
             (:call "Runtime.MakeArrayOfType")))
          (t (compile-named-call 'make-array args)))))

(defun compile-replace (args)
  "(REPLACE TARGET SOURCE &key START1 END1 START2 END2) with literal keywords, each
   at most once: Runtime.ReplaceKeys with the bounds in fixed positions (an absent
   start as 0, an absent end as NIL). The generic call looks up REPLACE and puts
   the arguments in an array, and reads the keywords out of it, on every call.
   The arguments are evaluated in the order written. Anything else (a keyword
   that is not a literal, :ALLOW-OTHER-KEYS, a repeated keyword, too few
   arguments) is the generic call."
  (let* ((keys (cddr args))
         (names (loop for (k) on keys by #'cddr collect k))
         (raw-copy (%raw-array-replace-form args)))
    (if raw-copy
        (compile-expr raw-copy)
    (if (and (>= (length args) 2)
             (evenp (length keys))
             (every (lambda (k)
                      (and (keywordp k)
                           (member (symbol-name k) '("START1" "END1" "START2" "END2")
                                   :test #'string=)))
                    names)
             (= (length names) (length (remove-duplicates names))))
        ;; The value forms in the order written: TARGET, SOURCE, then the
        ;; value of each keyword (the keywords themselves are constants).
        (let* ((forms (list* (first args) (second args)
                             (loop for (nil v) on keys by #'cddr collect v)))
               (temps (loop repeat (length forms) collect (gen-local "RPL")))
               (by-name (loop for k in names
                              for tmp in (cddr temps)
                              collect (cons (symbol-name k) tmp))))
          (flet ((arg (name default)
                   (let ((tmp (cdr (assoc name by-name :test #'string=))))
                     (if tmp
                         `((:ldloc ,tmp))
                         (let ((*in-tail-position* nil) (*in-mv-context* nil))
                           (compile-expr default))))))
            `(,@(loop for a in forms
                      for tmp in temps
                      append `((:declare-local ,tmp "LispObject")
                               ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                                   (compile-expr a))
                               (:stloc ,tmp)))
              (:ldloc ,(first temps))
              (:ldloc ,(second temps))
              ,@(arg "START1" 0)
              ,@(arg "END1" nil)
              ,@(arg "START2" 0)
              ,@(arg "END2" nil)
              (:call "Runtime.ReplaceKeys"))))
        (compile-named-call 'replace args)))))

(defun %raw-array-replace-form (args)
  "For (REPLACE A B) where A and B are different variables with element buffers
   of the same kind, one of them a *RAW-ARRAY-PLAN* array, the copy as an
   element loop, so the raw array is not passed to a function."
  (when (= (length args) 2)
    (destructuring-bind (dst src) args
      (let ((de (array-backing-entry (list 'aref dst 0)))
            (se (array-backing-entry (list 'aref src 0))))
        (when (and de se
                   (not (string= (var-name dst) (var-name src)))
                   (or (%raw-array-entry-p de) (%raw-array-entry-p se))
                   (eql (fourth de) (fourth se))
                   (not (eq (fourth de) :char)))
          (let ((i (gensym "I")))
            (flet ((len (v e)
                     (if (%raw-array-entry-p e)
                         (fourth (find (third e) *raw-array-plan* :key #'second))
                         `(length ,v))))
              (let ((dl (len dst de)) (sl (len src se)))
                `(progn
                   (dotimes (,i ,(if (and (integerp dl) (integerp sl))
                                     (min dl sl)
                                     `(min ,dl ,sl)))
                     (setf (aref ,dst ,i) (aref ,src ,i)))
                   ,dst)))))))))

(defun %make-array-simple-spec (init)
  "If INIT is (MAKE-ARRAY DIMS ...) whose result is a rank-1 simple array with a
   constant element type, the type it would be declared as -- (SIMPLE-ARRAY ELT
   (N)) for a literal size N, (SIMPLE-ARRAY ELT (*)) otherwise; else NIL.

   Only the keywords that cannot make the result non-simple are accepted
   (:ELEMENT-TYPE, :INITIAL-ELEMENT, :INITIAL-CONTENTS). :ADJUSTABLE,
   :FILL-POINTER and :DISPLACED-TO -- and any keyword this does not know -- give
   NIL, whatever their value, since a value form is not inspected."
  (and (consp init)
       (eq (car init) 'make-array)
       (not (local-function-entry 'make-array))
       (consp (cdr init))
       (eql (%make-array-static-rank (cadr init)) 1)
       (let ((spec nil) (seen nil))
         (do ((tail (cddr init) (cddr tail)))
             ((null tail)
              (and seen
                   (let ((dim (let ((d (cadr init)))
                                (cond ((integerp d) d)
                                      ((and (consp d) (eq (car d) 'quote)
                                            (integerp (cadr d)))
                                       (cadr d))
                                      (t '*)))))
                     (list 'simple-array spec (list dim)))))
           (let ((k (car tail)))
             (unless (and (symbolp k) (consp (cdr tail)))
               (return nil))
             (cond
               ((string= (symbol-name k) "ELEMENT-TYPE")
                (let ((f (cadr tail)))
                  (unless (and (not seen) (consp f) (eq (car f) 'quote)
                               (consp (cdr f)))
                    (return nil))
                  (setf spec (cadr f) seen t)))
               ((member (symbol-name k) '("INITIAL-ELEMENT" "INITIAL-CONTENTS")
                        :test #'string=))
               (t (return nil))))))))

(defun infer-array-backing-bindings (binding-info mutated body declared)
  "CSTATE-ARRAY-BACKING-LOCALS entries, in the shape EXTRACT-ARRAY-BACKING-LOCALS
   returns, for LET bindings with no type declaration whose init is a MAKE-ARRAY
   that %MAKE-ARRAY-SIMPLE-SPEC recognises: the binding is treated as declared
   with that SIMPLE-ARRAY type. SBCL derives the same type from the call.

   The conditions are the declared ones plus what makes the type a fact rather
   than a promise: the variable is lexical, not boxed, and not assigned in the
   body (MUTATED), so it holds the array the call made for as long as the
   binding lives. A name DECLARED already covers (a declared variable keeps its
   declaration) is skipped, as is one the body declares with any type at all.
   The float kinds keep the gate the declared float kinds have: the fetch is
   only paid for when the variable is read from inside a loop."
  (let ((result '()))
    (dolist (b binding-info)
      (let* ((var (first b))
             (name (var-name var))
             (spec (and (not (third b))
                        (second b)
                        (%make-array-simple-spec (second b))))
             (kind (and spec (%simple-array-backing-kind spec)))
             (raw (assoc (fourth b) *raw-array-plan*)))
        (when (and kind
                   (not (member name mutated :test #'string=))
                   (not (assoc name declared :test #'string=))
                   (not (assoc name result :test #'string=))
                   (eq (lookup-local var) (fourth b))
                   (not (boxed-var-p var))
                   (not (%var-type-declared-p body var))
                   (or raw
                       (case kind
                         ((:f64 :f32) (%var-iterated-access-p
                                       body name nil (function %tree-finds-aref-p)))
                         (t t))))
          ;; :MADE: the array is the one this binding's MAKE-ARRAY creates, so
          ;; its element type is known, not only declared (%ELEMENT-STORE-RANGE).
          (push (list name (fourth b) (if raw (second raw) (gen-local "ABACK")) kind
                      (%simple-array-store-narrow spec) :made nil (cadr spec))
                result))))
    (nreverse result)))

(defun %var-type-declared-p (body var)
  "T when a DECLARE at the head of BODY gives VAR a type, in either spelling.
   Anything that is not a known non-type declaration identifier counts."
  (dolist (form body nil)
    (unless (and (consp form) (eq (car form) 'declare)) (return nil))
    (dolist (decl (cdr form))
      (when (and (consp decl) (symbolp (car decl))
                 (not (member (symbol-name (car decl))
                              '("IGNORE" "IGNORABLE" "DYNAMIC-EXTENT" "SPECIAL"
                                "OPTIMIZE" "INLINE" "NOTINLINE" "FTYPE"
                                "DECLARATION")
                              :test #'string=))
                 (member var (if (string= (symbol-name (car decl)) "TYPE")
                                 (cddr decl)
                                 (cdr decl))
                         :test #'eq))
        (return-from %var-type-declared-p t)))))

(defparameter +iteration-operators+
  '("DO" "DO*" "DOTIMES" "DOLIST" "LOOP" "TAGBODY"
    "DO-SYMBOLS" "DO-EXTERNAL-SYMBOLS" "DO-ALL-SYMBOLS" "MAPHASH")
  "Operators whose body runs more than once. Matched by SYMBOL-NAME, the same
   way the rest of this file recognises operators across the cross-compile
   package split.")

(defun %raw-slot-access-of-p (form var layout)
  "T when FORM is (ACCESSOR VAR) for a DEFSTRUCT accessor naming one of the
   structure's RAW slots -- the shape of both a slot read and the place of a
   slot write, since SETF has not expanded yet where this runs.

   Raw specifically, because only a raw slot has anything to gain here. A
   structure whose loop touches only its boxed slots -- an open-addressed hash
   table probing the array in one of them -- would otherwise license a fetch
   that serves nothing inside the loop at all."
  (and (consp form)
       (symbolp (car form))
       (consp (cdr form))
       (null (cddr form))
       (symbolp (cadr form))
       (cadr form)
       (string= (var-name (cadr form)) var)
       (boundp '*struct-accessors*)
       (let ((packed (gethash (car form) *struct-accessors*)))
         (and packed
              (let ((cell (nth (logand packed (1- (ash 1 +slot-version-shift+)))
                               layout)))
                (and cell (>= (car cell) 0)))))
       t))

(defun %tree-finds-raw-access-p (form var layout)
  "T when FORM's tree contains a raw slot access on VAR. Walks CAR and CDR
   rather than mapping, so a dotted form in quoted data cannot break it."
  (cond ((%raw-slot-access-of-p form var layout) t)
        ((consp form) (or (%tree-finds-raw-access-p (car form) var layout)
                          (%tree-finds-raw-access-p (cdr form) var layout)))
        (t nil)))

(defun %tree-finds-schar-p (form var &optional layout)
  "T when FORM's tree contains (SCHAR VAR ...) or (CHAR VAR ...) -- the shape of
   both a character read and the place of a character write, since SETF has not
   expanded yet where this runs. LAYOUT is accepted and ignored so this can be
   passed where %TREE-FINDS-RAW-ACCESS-P is."
  (declare (ignore layout))
  (cond ((and (consp form)
              (symbolp (car form))
              (member (symbol-name (car form)) '("SCHAR" "CHAR") :test #'string=)
              (consp (cdr form))
              (symbolp (cadr form))
              (cadr form)
              (string= (var-name (cadr form)) var))
         t)
        ((consp form) (or (%tree-finds-schar-p (car form) var)
                          (%tree-finds-schar-p (cdr form) var)))
        (t nil)))

(defun %tree-finds-aref-p (form var &optional layout)
  "T when FORM's tree contains (AREF VAR ...) -- the shape of both an element
   read and the place of an element write, since SETF has not expanded yet
   where this runs -- or a call to an INLINE function with VAR among its
   arguments: its expansion binds a parameter to VAR, and that parameter's
   accesses use VAR's buffer (%ALIAS-ARRAY-BACKING-BINDINGS). That is how
   ironclad reads a word (UB64REF/LE VECTOR OFFSET). LAYOUT is accepted and
   ignored so this can be passed where %TREE-FINDS-RAW-ACCESS-P is."
  (declare (ignore layout))
  (cond ((and (consp form)
              (symbolp (car form))
              (or (string= (symbol-name (car form)) "AREF")
                  (eq (second (second (%simd-name-info (car form)))) :aref))
              (consp (cdr form))
              (symbolp (cadr form))
              (cadr form)
              (string= (var-name (cadr form)) var))
         t)
        ((and (consp form)
              (symbolp (car form))
              (car form)
              (gethash (car form) *inline-defs*)
              (not (local-function-entry (car form)))
              (loop for tail on (cdr form)
                    while (consp tail)
                    thereis (let ((a (car tail)))
                              (and (symbolp a) a (string= (var-name a) var)))))
         t)
        ((consp form) (or (%tree-finds-aref-p (car form) var)
                          (%tree-finds-aref-p (cdr form) var)))
        (t nil)))

(defun %var-iterated-access-p (form var layout &optional (finder (function %tree-finds-raw-access-p)))
  "T when FORM contains an iteration whose body reaches a structure slot of
   VAR -- the only shape the hoist pays for.

   A profitability test, not a correctness one, and deliberately crude in the
   safe direction: a false NO costs a hoist that was available, and it will
   give one for a loop that appears only after macroexpansion, and for a
   function that repeats itself by recursion rather than by looping.

   The question is per variable, and about its RAW slots, because those come
   apart from the body constantly. A tokenizer loops over a string while
   touching its structure's slots a fixed two or three times on either side of
   the loop. An open-addressed hash table probes inside the loop, but through
   the array in a BOXED slot, and reads its raw ones only on the way in. Both
   pay a fetch on every call to save nothing, and both are what asking only
   whether the body iterates got wrong."
  (cond ((not (consp form)) nil)
        ((and (symbolp (car form))
              (member (symbol-name (car form)) +iteration-operators+
                      :test #'string=)
              (funcall finder (cdr form) var layout))
         t)
        (t (or (%var-iterated-access-p (car form) var layout finder)
               (%var-iterated-access-p (cdr form) var layout finder)))))

(defun %tree-has-arith-on-p (form var)
  (cond ((not (consp form)) nil)
        ((and (symbolp (car form))
              (member (symbol-name (car form)) '("+" "-" "*" "1+" "1-") :test #'string=)
              (loop for tail on (cdr form)
                    while (consp tail)
                    thereis (let ((a (car tail)))
                              (and (symbolp a) a (string= (var-name a) var)))))
         t)
        (t (or (%tree-has-arith-on-p (car form) var)
               (%tree-has-arith-on-p (cdr form) var)))))

(defun %tree-finds-index-arith-p (form var &optional layout)
  "T when FORM's tree contains an array access whose subscript does arithmetic
   on VAR."
  (declare (ignore layout))
  (cond ((not (consp form)) nil)
        ((and (symbolp (car form))
              (member (symbol-name (car form))
                      '("AREF" "ROW-MAJOR-AREF" "SVREF" "SCHAR" "CHAR" "BIT" "SBIT")
                      :test #'string=)
              (consp (cdr form))
              (%tree-has-arith-on-p (cddr form) var))
         t)
        (t (or (%tree-finds-index-arith-p (car form) var)
               (%tree-finds-index-arith-p (cdr form) var)))))

(defvar *index-param-unswitch-limit* 300
  "Most conses of a body that %UNSWITCH-INDEX-PARAMS copies.")

(defun %cons-count-within (tree limit)
  "The number of conses in TREE, or NIL once it passes LIMIT."
  (let ((n 0))
    (labels ((walk (x)
               (loop while (consp x)
                     do (when (> (incf n) limit)
                          (return-from %cons-count-within nil))
                        (walk (car x))
                        (setq x (cdr x)))))
      (walk tree)
      n)))

(defun %unswitch-index-params (vars body)
  "BODY with a second copy, taken when each of VARS that a loop's subscript
   arithmetic uses is a (SIGNED-BYTE 32), in which those VARS are declared so."
  (multiple-value-bind (decls rest)
      (let ((head '()) (tail body))
        (loop while (and (cdr tail)
                         (or (stringp (car tail))
                             (and (consp (car tail)) (eq (caar tail) 'declare))))
              do (push (pop tail) head))
        (values (nreverse head) tail))
    (let ((cands
            (and rest
                 (%cons-count-within rest *index-param-unswitch-limit*)
                 (not (%tree-mentions-name-p rest "LOAD-TIME-VALUE"))
                 (loop for v in vars
                       when (and (symbolp v)
                                 (not (global-special-p v))
                                 (not (%tree-mentions-symbol-p decls v))
                                 (%var-iterated-access-p rest (var-name v) nil
                                                         (function %tree-finds-index-arith-p)))
                         collect v))))
      (when cands
        (let ((mutated (find-mutated-and-captured-vars rest (mapcar #'var-name cands))))
          (setq cands (remove-if (lambda (v) (member (var-name v) mutated :test #'string=))
                                 cands))))
      (if (null cands)
          body
          `(,@decls
            (if (and ,@(mapcar (lambda (v) `(typep ,v '(signed-byte 32))) cands))
                (let ,(mapcar (lambda (v) (list v v)) cands)
                  (declare (type (signed-byte 32) ,@cands))
                  ,@rest)
                (progn ,@rest)))))))

(defun %tree-mentions-symbol-p (tree sym)
  (cond ((eq tree sym) t)
        ((consp tree) (or (%tree-mentions-symbol-p (car tree) sym)
                          (%tree-mentions-symbol-p (cdr tree) sym)))
        (t nil)))

(defun %tree-mentions-name-p (tree name)
  (cond ((and (symbolp tree) tree (string= (symbol-name tree) name)) t)
        ((consp tree) (or (%tree-mentions-name-p (car tree) name)
                          (%tree-mentions-name-p (cdr tree) name)))
        (t nil)))

(defun %struct-raw-backing-type (spec)
  "(VERSION . LAYOUT) when SPEC names a structure that has raw slot storage, so
   a binding declared to hold one can have that storage fetched once; else NIL."
  (and (symbolp spec)
       spec
       (boundp '*struct-raw-layouts*)
       (gethash spec *struct-raw-layouts*)))

(defun extract-struct-backing-locals (body)
  "Scan the head declare forms of BODY for variables declared to hold a
   structure with raw slot storage, and return CSTATE-STRUCT-BACKING-LOCALS
   entries (NAME KEY . BACKING-SYMBOL) for those whose raw array may be
   fetched once at binding time.

   Both spellings are accepted, (TYPE K-POINT P) and the bare (K-POINT P),
   because DEFSTRUCT defines a type name and CL lets a declaration use it
   directly.

   Excluded on exactly the terms the array version uses: a boxed variable does
   not live in a plain slot, and a variable BODY assigns to may come to hold a
   different instance, whose raw array is a different array. The mutation walk
   only runs when some declaration qualified, so a body with no such
   declaration pays nothing for this.

   Excluded, additionally, unless the variable's slots are reached from inside
   an iteration. The fetch is not free -- a type test, a version test and a
   field load, on every entry to the function -- and it only pays for itself
   when the accesses it serves happen more than once. A body that touches each
   slot once and returns pays the fetch and gains nothing, and measurably
   loses: before this test was here the straight-line vector kernels in
   bench/il-parity ran about 20% slower and emitted five times the code,
   because each access also carries the arm for the fetch having failed.
   See %VAR-ITERATED-ACCESS-P for why the question is per variable.

   Must be called with *locals* bound -- KEY is the variable's slot via
   lookup-local, which STRUCT-BACKING-ENTRY re-checks."
  (let ((cands '()))
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare)) (return))
      (dolist (decl (cdr form))
        (when (consp decl)
          (let* ((typed (eq (car decl) 'type))
                 (spec (if typed (cadr decl) (car decl)))
                 (vars (if typed (cddr decl) (cdr decl)))
                 (info (%struct-raw-backing-type spec)))
            (when info
              (dolist (v vars)
                (when (and (symbolp v) (lookup-local v) (not (boxed-var-p v))
                           (%var-iterated-access-p body (var-name v) (cdr info)))
                  (pushnew (list* (var-name v) v spec info) cands
                           :test #'string= :key #'car))))))))
    (when cands
      (let ((mutated (find-mutated-and-captured-vars body (mapcar #'car cands))))
        (loop for (name . (sym spec . info)) in cands
              unless (member name mutated :test #'string=)
                collect (list* name (lookup-local sym) (gen-local "SBACK")
                               spec (car info) (cdr info)))))))

(defun struct-backing-prologue (entries)
  "Instructions that fetch each hoisted raw slot array once, right after the
   variables are bound.

   Unlike ARRAY-BACKING-PROLOGUE this does not check a declaration and does not
   signal. A false declaration, a stale layout version and an instance that
   dropped its raw storage at construction all answer null here, and every
   access site has a null arm that is the code it emitted before -- which is
   also the code that reports the errors. Signalling here would turn an
   instance that reads perfectly well today into an error."
  (loop for e in entries
        append `((:declare-local ,(struct-backing-slot e) "Int64[]")
                 (:ldloc ,(second e))
                 ,@(compile-quoted-cached (struct-backing-type e))
                 (:ldc-i4 ,(struct-backing-version e))
                 (:call "Runtime.StructRawBacking")
                 (:stloc ,(struct-backing-slot e)))))

(defun extract-numeric-array-locals (body)
  "Scan the head declare forms of BODY for variables declared with an
   simple-array/array/vector type of statically-known rank 1-3 whose element
   type gets an unboxed backing, returning *numeric-array-locals* entries:
   (NAME KEY RANK . :single/:double) for float backing, (NAME KEY RANK LO . HI)
   for integer backing. This lets aref on an array-typed PARAMETER or declared
   local ride the unboxed path -- the fft/mandelbrot kernels' (simple-array
   single-float (1025)) on Runtime.ArefNum*D, an (simple-array fixnum (*))
   walk on Runtime.ArefNum*L -- and not only make-array let-locals, which is
   the only shape that can be PROVEN rather than declared.

   Must be called with *locals* bound (KEY = the var's slot via lookup-local,
   which numeric-array-aref-entry re-checks so a shadow or a re-key
   self-invalidates). The runtime fast paths re-check _numKind, so a caller
   that passes an array with different backing falls back to the boxed path
   rather than corrupting data -- the declaration is a soundness-safe hint."
  (let ((result '()))
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare)) (return))
      (dolist (decl (cdr form))
        (when (and (consp decl) (eq (car decl) 'type))
          (let ((info (%array-type-numeric-info (cadr decl))))
            (when info
              (dolist (v (cddr decl))
                (when (and (symbolp v) (lookup-local v) (not (boxed-var-p v)))
                  (pushnew (list* (var-name v) (lookup-local v) info)
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
    ;; Operations that cannot leave the fixnum range. A bitwise op on two
    ;; fixnums, and the smaller or larger of two fixnums, is a fixnum by
    ;; construction, so no proof is needed.
    ((and (consp expr) (symbolp (car expr))
          (member (car expr) '(logand logior logxor min max))
          (cdr expr)
          (every (lambda (a)
                   (eq 'fixnum (infer-expr-return-type a var-types self-name)))
                 (cdr expr)))
     'fixnum)
    ;; Operations that CAN leave it. FIXNUM + FIXNUM spans one bit more than a
    ;; fixnum, and (ABS MOST-NEGATIVE-FIXNUM) is not a fixnum either, so
    ;; fixnum-typed operands do not make the result a fixnum -- they make it an
    ;; INTEGER. Answering FIXNUM here was not a missed optimization but a wrong
    ;; answer: it reached *FUNCTION-RETURN-TYPES*, and a caller that bound the
    ;; call in a LET took an Int64 slot for it and then could not unbox the
    ;; bignum the callee had correctly returned.
    ;;
    ;; FIXNUM is still available two ways, and both are decided above: a range
    ;; proof that the value fits, or the writer's own (THE FIXNUM ...), which
    ;; the THE clause answers before this one is reached.
    ((and (consp expr) (symbolp (car expr))
          (member (car expr) '(+ - * 1+ 1- abs))
          (cdr expr)
          (every (lambda (a)
                   (eq 'fixnum (infer-expr-return-type a var-types self-name)))
                 (cdr expr)))
     (if (fixnum-arith-unboxed-safe-p expr) 'fixnum 'integer))
    ;; ash: a non-negative shift can overflow int64, so only infer fixnum when
    ;; the value provably fits: negative shift (right shift), or a constant base
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
  Returns a concrete type symbol (e.g. 'fixnum) or NIL if unknown.
  The walk asks for a range proof (FIXNUM-ARITH-UNBOXED-SAFE-P) at every
  arithmetic level, each about the whole tree below it; one *INT-TYPE-MEMO*
  for the walk answers each subform once instead of once per level above it.
  The walk compiles nothing, so the answers stay valid throughout."
  (let* ((var-types (extract-declared-var-types body))
         (last-form (and body (car (last body)))))
    (when last-form
      (let ((ty (let ((*int-type-memo* (or *int-type-memo*
                                           (make-hash-table :test 'eq))))
                  (infer-expr-return-type last-form var-types self-name))))
        (when (and ty (not (eq ty :self-recursive)))
          ty)))))

(defun compile-let (bindings body sequential-p &optional boolean-tail)
  "Compile (let/let* ...), wrapping the result in a debug scope so its lexical
   locals are named only within this binding form. Nested lets nest their scopes,
   giving the debugger correct shadowing (a method-wide scope can't disambiguate
   two same-named vars). No-op wrapper when debug info is off."
  (let ((result (%compile-let bindings body sequential-p boolean-tail)))
    (if *emit-source-lines*
        `((:scope-begin) ,@result (:scope-end))
        result)))

(defun step-let*-type-locals (name fx chr dbl sgl dec)
  "Advance the five name-keyed type tables past one LET* binding of NAME.

   FX / CHR / DBL / SGL / DEC are the names this LET*'s body declares FIXNUM,
   CHARACTER, DOUBLE-FLOAT, SINGLE-FLOAT and DECIMAL. A SPECIAL binding passes
   NIL for all five: it introduces a dynamic binding, which carries no lexical
   type declaration, and the name must stop resolving to any outer lexical entry.

   The tables are special variables, so assigning them here is seen by the LET*
   loop that rebound them -- which is the point: each binding has to be visible
   to the inits that follow it, and nothing beyond this LET*."
  (setf *fixnum-locals*       (type-locals-after-binding name fx *fixnum-locals*)
        *character-locals*    (type-locals-after-binding name chr *character-locals*)
        *double-float-locals* (type-locals-after-binding name dbl *double-float-locals*)
        *single-float-locals* (type-locals-after-binding name sgl *single-float-locals*)
        *decimal-locals*      (type-locals-after-binding name dec *decimal-locals*))
  nil)

(defun compile-let-body-instrs (real-body body binding-info let-bound-names
                                needs-boxing mutated special-syms declared-specials
                                &optional boolean-tail)
  "Compile REAL-BODY in the scope a LET/LET* body sees, and return its
   instruction list. Establishes the body's own type declarations (name-keyed
   tables shadow-filtered by LET-BOUND-NAMES, slot-keyed tables rekeyed via
   BINDING-INFO), the inferred native/typed bindings, and the try-region flag
   when SPECIAL-SYMS forces a try/finally. DECLARED-SPECIALS non-NIL additionally
   drops those declared-special variables from the locals table (the sequential
   branch defers that removal to here; the parallel branch has already done it).
   Shared by both %COMPILE-LET branches; the scope rules of a LET body have
   one definition.

   A binding declared with a SIMPLE-* array type gets its check here, ahead of
   REAL-BODY (see INSERT-SIMPLE-DECL-CHECKS). Not for an empty body: the check
   is a statement, and there is no value form for it to precede."
  (when real-body
    (let ((checks (simple-decl-check-forms body let-bound-names)))
      (when checks (setq real-body (append checks real-body)))))
  (let ((*in-try-block* (or *in-try-block* special-syms))
        (*lexical-decl-frames* (%push-decl-frame :let binding-info body))
        (*fixnum-locals*
         (append (extract-fixnum-locals body)
                 (drop-shadowed-type-locals let-bound-names *fixnum-locals*)))
        (*character-locals*
         (append (extract-character-locals body)
                 (drop-shadowed-type-locals let-bound-names *character-locals*)))
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
                      (extract-numeric-array-locals body)
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
    ;; Hoisted element buffers are established here rather than by the caller:
    ;; this is the first point where the LET's own variables resolve, and the
    ;; prologue has to run after the bindings are in their slots, which is
    ;; exactly the head of the body. An inherited entry for a name this LET
    ;; rebinds is rejected by the key check in ARRAY-BACKING-ENTRY, the same way
    ;; the numeric-array table handles a shadow.
    (multiple-value-bind (backing aliases)
        (%alias-array-backing-bindings
         (let ((declared (extract-array-backing-locals body)))
           (append declared
                   (infer-array-backing-bindings
                    binding-info mutated body declared)))
         binding-info mutated body)
    (let* ((sbacking (extract-struct-backing-locals body))
           (emit-body (cond ((eq boolean-tail :long)
                             (lambda () (compile-progn-long-tail real-body)))
                            ;; A function: it compiles the body itself (the body
                            ;; of a typed entry, see COMPILE-TYPED-TAIL).
                            ((functionp boolean-tail)
                             (lambda () (funcall boolean-tail real-body)))
                            (boolean-tail
                             (lambda () (compile-progn-boolean-tail
                                         real-body (car boolean-tail) (cdr boolean-tail))))
                            (t (lambda () (compile-progn real-body))))))
      (if (and (null backing) (null aliases) (null sbacking))
          (funcall emit-body)
          (let* ((*cstate* (if (or backing aliases)
                               (cstate-with *cstate* +cs-array-backing-locals+
                                            (append backing aliases
                                                    (cstate-array-backing-locals)))
                               *cstate*))
                 (*cstate* (if sbacking
                               (cstate-with *cstate* +cs-struct-backing-locals+
                                            (append sbacking
                                                    (cstate-struct-backing-locals)))
                               *cstate*)))
            ;; An alias was fetched where its source was bound: only BACKING
            ;; gets a prologue, and the unswitch tests both (an alias of a
            ;; buffer an enclosing unswitch already tested costs one more
            ;; test of the same local, outside the loop).
            (append (array-backing-prologue backing)
                    (struct-backing-prologue sbacking)
                    (unswitch-array-backing (remove-if #'%raw-array-entry-p
                                                       (append backing aliases))
                                            real-body emit-body sbacking))))))))

(defvar *let-backing-sources* '()
  "Alist (BINDING-KEY . ENTRY) for the LET being compiled: the bindings whose
   init is a plain reference to a local with a hoisted element buffer, and that
   local's CSTATE-ARRAY-BACKING-LOCALS entry. Computed by %COMPILE-LET before
   the LET's own names are in scope. Keyed by the binding's fresh slot symbol,
   so an entry is never mistaken for another LET's.")

(defun %let-backing-sources (binding-info sequential-p)
  "*LET-BACKING-SOURCES* for BINDING-INFO, in the enclosing scope. In a LET* an
   init that names an earlier sibling refers to that sibling, whose buffer is
   not hoisted yet: such a binding gets no entry."
  (let ((result '()) (seen '()))
    (dolist (b binding-info)
      (let ((init (second b)))
        (unless (or (third b)
                    (not (symbolp init))
                    (and sequential-p
                         (member (var-name init) seen :test #'string=)))
          (let ((src (%array-backing-source init)))
            (when src (push (cons (fourth b) src) result)))))
      (push (var-name (first b)) seen))
    result))

(defun %array-backing-source (init)
  "When INIT is a plain reference to a local whose element buffer is hoisted in
   the enclosing scope, that local's CSTATE-ARRAY-BACKING-LOCALS entry; else
   NIL. Re-checked the way ARRAY-BACKING-ENTRY does it (binding key, boxed
   flag). Asked in the scope INIT is evaluated in: an INLINE expansion binds
   VECTOR to the caller's VECTOR, and inside the LET the name already means the
   new binding."
  (and (symbolp init)
       init
       (not (constantp init))
       (boundp '*cstate*)
       (let ((e (assoc (var-name init) (cstate-array-backing-locals)
                       :test #'string=)))
         (and e
              (eq (lookup-local init) (second e))
              (not (boxed-var-p init))
              e))))

(defun %alias-array-backing-bindings (entries binding-info mutated body)
  "Split ENTRIES, the LET's hoisted element buffers, into those that need a
   fetch of their own and aliases (entries whose BACKING-SYMBOL is the one an
   enclosing binding already fetched). Two values: the fetched entries and the
   aliases.

   A binding whose init is a plain reference to a local with a hoisted buffer
   (%ARRAY-BACKING-SOURCE) holds the same array as that local for as long as
   both live: neither is assigned (the source's entry exists only if its own
   body does not assign it, and this binding is in that body; this one is
   checked against MUTATED), and a simple array's storage is never replaced
   (see %SIMPLE-ARRAY-BACKING-KIND). So it can use the buffer already fetched.
   That is the shape an INLINE call takes: its expansion binds each parameter
   to the argument, and ironclad's UB64REF/LE expands UB32REF/LE twice and
   UB16REF/LE four times, each binding VECTOR to the caller's VECTOR and
   fetching the same buffer again.

   A declared binding is an alias only when its declaration gives the same
   buffer kind and store width as the source's; otherwise it keeps its own
   fetch (and the TYPE-ERROR that fetch signals above safety 0). A binding
   with no type declaration at all (a SETF expansion's temporary) becomes an
   alias of the source, with the source's kind and width: it is the source's
   value under another name."
  (let ((fetched '()) (aliases '()))
    (dolist (e entries)
      (let* ((b (find (first e) binding-info
                      :key (lambda (b) (var-name (first b))) :test #'string=))
             (src (and b (cdr (assoc (fourth b) *let-backing-sources*)))))
        (if (and src
                 (eql (fourth src) (fourth e))
                 (equal (fifth src) (fifth e))
                 (equal (seventh src) (seventh e))
                 (equal (eighth src) (eighth e)))
            (push (list (first e) (second e) (third src) (fourth e) (fifth e)
                        (sixth src) (seventh src) (eighth src))
                  aliases)
            (push e fetched))))
    (dolist (b binding-info)
      (let ((var (first b)))
        (unless (or (third b)
                    (find (var-name var) entries :key #'first :test #'string=)
                    (member (var-name var) mutated :test #'string=)
                    (not (eq (lookup-local var) (fourth b)))
                    (boxed-var-p var)
                    (%var-type-declared-p body var))
          (let ((src (cdr (assoc (fourth b) *let-backing-sources*))))
            (when src
              (push (list (var-name var) (fourth b) (third src) (fourth src)
                          (fifth src) (sixth src) (seventh src) (eighth src))
                    aliases))))))
    (values (nreverse fetched) (nreverse aliases))))

;;; ------------------------------------------------------------
;;; Counting variables of LOOP / DO / DOTIMES
;;; ------------------------------------------------------------
;;; A counting loop's variable starts at an integer, gains a constant DELTA per
;;; step and is tested against a limit before every step, so the values it can
;;; take are bounded by the start and the limit. The macros cannot use that
;;; themselves: the bound needs the ranges of the start and limit forms, which
;;; only the compiler knows, and it holds only if nothing else assigns the
;;; variable, which only the compiler can see through the body's macros. So
;;; they state the shape, as a declaration the compiler resolves here:
;;;
;;;   (%INDUCTION VAR DELTA LIMIT INCLUSIVE-P)
;;;
;;; VAR is bound by this LET; DELTA is a nonzero integer (VAR goes up when it is
;;; positive); LIMIT is an integer or a variable; the test ends the loop when VAR
;;; passes LIMIT (or reaches it, when INCLUSIVE-P is NIL) before every step; and
;;; every step is an assignment of (%INDUCTION-NEXT VAR DELTA) (DOTIMES writes
;;; (%DOTIMES-1+ VAR)). Before a step VAR is within LIMIT, so after it VAR is at
;;; most DELTA past the limit; the start bounds the other side.
;;;
;;; When the claim checks out the declaration becomes (TYPE (INTEGER LO HI) VAR),
;;; and from there VAR is a declared bounded integer like any other. Otherwise it
;;; is dropped and VAR stays as untyped as before.

(defun %induction-step-value-p (form var)
  "True when FORM is the value a counting macro assigns to VAR as its step:
   (%INDUCTION-NEXT VAR DELTA), (%DOTIMES-1+ VAR), either inside THE, or either
   as the first form of a PROG1 (how LOOP steps several variables in parallel). Matched by name:
   the symbols may be interned in either compiler package."
  (and (consp form)
       (symbolp (car form))
       (let ((n (symbol-name (car form))))
         (cond ((string= n "PROG1")
                (and (consp (cdr form)) (%induction-step-value-p (cadr form) var)))
               ((string= n "THE")
                (and (consp (cdr form)) (consp (cddr form))
                     (%induction-step-value-p (caddr form) var)))
               ((or (string= n "%INDUCTION-NEXT") (string= n "%DOTIMES-1+"))
                (and (consp (cdr form)) (eq (cadr form) var)))
               (t nil)))))

(defun %hide-induction-steps (form var dummy budget)
  "FORM with every step assignment of VAR (see %INDUCTION-STEP-VALUE-P) made to
   DUMMY instead, copying only the conses on the way to one. BUDGET is a cons
   whose car counts the conses still allowed; when it runs out the walk throws
   to %INDUCTION-GIVE-UP. QUOTE is not entered (a literal may be circular)."
  (cond ((atom form) form)
        ((minusp (decf (car budget))) (throw '%induction-give-up nil))
        ((eq (car form) 'quote) form)
        ((and (symbolp (car form))
              (member (symbol-name (car form)) '("SETQ" "PSETQ" "LOOP-REALLY-DESETQ")
                      :test #'string=)
              (listp (cdr form)))
         (let ((out '()) (changed nil))
           (do ((tail (cdr form) (cddr tail)))
               ((atom tail) (when tail (setq out (cons tail out))))
             (let* ((v (car tail))
                    (val (if (consp (cdr tail)) (cadr tail) nil))
                    (hide (and (eq v var) (%induction-step-value-p val var)))
                    (new-val (%hide-induction-steps val var dummy budget)))
               (when (or hide (not (eq new-val val))) (setq changed t))
               (push (if hide dummy v) out)
               (when (consp (cdr tail)) (push new-val out))))
           (if changed (cons (car form) (nreverse out)) form)))
        (t (let ((a (%hide-induction-steps (car form) var dummy budget))
                 (d (%hide-induction-steps (cdr form) var dummy budget)))
             (if (and (eq a (car form)) (eq d (cdr form))) form (cons a d))))))

(defun %induction-range (spec binding-info real-body mutated)
  "The (LO . HI) range an (%INDUCTION VAR DELTA LIMIT INCLUSIVE-P) declaration
   proves for VAR, or NIL. Called with the LET's enclosing scope in effect, so
   EXPR-INT-RANGE of an init form, or of a LIMIT variable this LET does not
   bind, sees what the form itself sees."
  (destructuring-bind (&optional var delta limit inclusive &rest more) (cdr spec)
    (declare (ignore more))
    (let ((b (and (symbolp var) var (assoc var binding-info :test #'eq))))
      (when (and b
                 (not (third b))
                 (integerp delta) (/= delta 0)
                 (or (integerp limit) (and limit (symbolp limit))))
        (let ((rs (expr-int-range (second b)))
              (rl (if (integerp limit)
                      (cons limit limit)
                      (let ((lb (assoc limit binding-info :test #'eq)))
                        (cond ((null lb)
                               (and (not (global-special-p limit))
                                    (expr-int-range limit)))
                              ((or (third lb)
                                   (member (var-name limit) mutated :test #'string=))
                               nil)
                              (t (expr-int-range (second lb))))))))
          (when (and rs rl
                     ;; Nothing assigns VAR but its own steps.
                     (catch '%induction-give-up
                       (not (member (var-name var)
                                    (find-mutated-and-captured-vars
                                     (%hide-induction-steps real-body var
                                                            (make-symbol "STEP")
                                                            (list 200000))
                                     (list (var-name var)))
                                    :test #'string=))))
            (let ((r (if (plusp delta)
                         (cons (car rs)
                               (max (cdr rs) (+ (cdr rl) delta (if inclusive 0 -1))))
                         (cons (min (car rs) (+ (car rl) delta (if inclusive 0 1)))
                               (cdr rs)))))
              (and (range-fits-int64-p r) r))))))))

(defun %resolve-induction-declarations (body binding-info real-body mutated sequential-p)
  "BODY with each leading (%INDUCTION ...) declaration replaced by the
   (TYPE (INTEGER LO HI) VAR) it proves, or dropped when it proves nothing (a
   LET* is not resolved: its inits see their earlier siblings). BODY itself when
   it has none."
  (flet ((induction-spec-p (d)
           (and (consp d) (symbolp (car d))
                (string= (symbol-name (car d)) "%INDUCTION"))))
    (if (not (loop for form in body
                   while (and (consp form) (eq (car form) 'declare))
                   thereis (some #'induction-spec-p (cdr form))))
        body
        (let ((rest body) (decls '()))
          (loop while (and (consp (car rest)) (eq (car (car rest)) 'declare))
                do (let ((specs '()))
                     (dolist (d (cdr (pop rest)))
                       (if (not (induction-spec-p d))
                           (push d specs)
                           (let ((r (and (not sequential-p)
                                         (%induction-range d binding-info
                                                           real-body mutated))))
                             (when r
                               (push `(type (integer ,(car r) ,(cdr r)) ,(cadr d))
                                     specs)))))
                     (push (cons 'declare (nreverse specs)) decls)))
          (append (nreverse decls) rest)))))

(defun %bounded-loop-candidate (body binding-info real-body sequential-p)
  "*BOUNDED-LOOP-CANDIDATE* for a LET whose body is one TAGBODY and whose
   %INDUCTION variable starts at 0 or above and counts up."
  (unless sequential-p
    (let ((spec (loop for form in body
                      while (and (consp form) (eq (car form) 'declare))
                      thereis (find-if (lambda (d)
                                         (and (consp d) (symbolp (car d))
                                              (string= (symbol-name (car d)) "%INDUCTION")))
                                       (cdr form)))))
      (when (and spec
                 (= (length real-body) 1)
                 (consp (car real-body))
                 (eq (caar real-body) 'tagbody))
        (destructuring-bind (&optional var delta limit inclusive &rest more) (cdr spec)
          (declare (ignore more))
          (let ((b (and var (symbolp var) (assoc var binding-info :test #'eq))))
            (when (and b (not (third b))
                       (integerp delta) (plusp delta)
                       (or (integerp limit) (and limit (symbolp limit)))
                       (let ((r (expr-int-range (second b))))
                         (and r (>= (car r) 0))))
              (list (car real-body) var limit inclusive delta
                    (let ((r (expr-int-range (second b))))
                      (and (eql (car r) (cdr r)) (car r)))))))))))

(defun %compile-let (bindings body sequential-p &optional boolean-tail)
  "Compile (let bindings body...) or (let* bindings body...)."
  (warn-unknown-declared-types body)
  (let ((no-raw '()))
    (loop
      (multiple-value-bind (instrs failed)
          (%compile-let-1 bindings body sequential-p boolean-tail no-raw)
        (if failed
            (setf no-raw (append failed no-raw))
            (return instrs))))))

(defun %raw-array-init-shape (init)
  "KIND and length N when INIT is (MAKE-ARRAY N :ELEMENT-TYPE 'SPEC) with a literal
   N and a SPEC whose rank-1 simple array has a numeric element buffer."
  (when (and (consp init)
             (eq (car init) 'make-array)
             (not (local-function-entry 'make-array))
             (not (macrolet-shadowed-p 'make-array))
             (= (length init) 4)
             (integerp (second init))
             (<= 0 (second init) (expt 2 24))
             (keywordp (third init))
             (string= (symbol-name (third init)) "ELEMENT-TYPE")
             (let ((f (fourth init)))
               (and (consp f) (eq (car f) 'quote) (consp (cdr f)) (null (cddr f)))))
    (let ((kind (%simple-array-backing-kind
                 (list 'simple-array (cadr (fourth init)) (list (second init))))))
      (when (and kind (not (eq kind :char)))
        (values kind (second init))))))

(defun %raw-array-plan (binding-info body real-body mutated captured no-raw)
  "*RAW-ARRAY-PLAN* entries for the bindings of this LET whose init
   %RAW-ARRAY-INIT-SHAPE accepts and whose variable is lexical, not assigned,
   not captured and not declared. Whether the variable's value is ever needed
   is decided after the body is compiled (%RAW-ARRAY-FAILURES); the LET is then
   compiled again without it, so the forms must be safe to compile twice."
  (unless (or *emit-source-lines* *emit-frame-locals*)
    (let ((plan '()))
      (dolist (b binding-info)
        (let ((var (first b)))
          (multiple-value-bind (kind n) (%raw-array-init-shape (second b))
            (when (and kind
                       (not (third b))
                       (not (member (var-name var) mutated :test #'string=))
                       (not (member (var-name var) captured :test #'string=))
                       (not (member (var-name var) no-raw :test #'string=))
                       (not (%var-type-declared-p body var))
                       (%raw-array-uses-p var real-body))
              (push (list (fourth b) (gen-local "ABACK") kind n (var-name var))
                    plan)))))
      (and plan
           (%backing-duplicable-p real-body)
           (%backing-duplicable-p (mapcar #'second binding-info))
           (nreverse plan)))))

(defun %raw-array-uses-p (var tree)
  "T when every occurrence of VAR in TREE is the array of an AREF or a LENGTH,
   an argument of a two-argument REPLACE, quoted (a macro may build the AREF), or
   in a DYNAMIC-EXTENT, IGNORE or IGNORABLE declaration. MACROLET definitions
   are not looked at. A cheap filter: %RAW-ARRAY-FAILURES decides."
  (labels ((ok-parent-p (form)
             (and (symbolp (car form))
                  (let ((op (symbol-name (car form))))
                    (cond ((member op '("AREF" "LENGTH") :test #'string=)
                           (eq (cadr form) var))
                          ((string= op "REPLACE")
                           (and (consp (cddr form)) (null (cdddr form))))
                          (t (member op '("DYNAMIC-EXTENT" "IGNORE" "IGNORABLE"
                                          "QUOTE")
                                     :test #'string=))))))
           (walk (form)
             (or (atom form)
                 (and (symbolp (car form))
                      (string= (symbol-name (car form)) "MACROLET")
                      (consp (cdr form))
                      (walk (cddr form)))
                 (and (or (not (loop for tail on form thereis (eq (car tail) var)))
                          (ok-parent-p form))
                      (loop for tail on form
                            always (and (walk (car tail))
                                        (not (eq (cdr tail) var))))))))
    (walk tree)))

(defun %let-init-code (key init-form)
  (let ((raw (assoc key *raw-array-plan*)))
    (cond (raw
           (destructuring-bind (aback kind n) (cdr (butlast raw))
             (let ((type (%backing-slot-type kind)))
               `((:declare-local ,aback ,type)
                 (:ldc-i4 ,n)
                 (:newarr ,(subseq type 0 (- (length type) 2)))
                 (:stloc ,aback)
                 (:ldnull)))))
          (init-form (compile-expr init-form))
          (t (emit-nil)))))

(defun %raw-array-failures (plan instrs)
  "The names in PLAN whose slot INSTRS reads other than to discard the value."
  (loop for (key nil nil nil name) in plan
        unless (%raw-array-slot-unread-p key instrs)
          collect name))

(defun %raw-array-slot-unread-p (key instrs)
  "T when INSTRS has the init's store into KEY and no read of it that is not
   discarded. A missing store means ELIMINATE-SINGLE-REF-LOCALS moved the init's
   value to its one read."
  (let ((total 0) (allowed 0) (stores 0))
    (labels ((walk (x)
               (cond ((eq x key) (incf total))
                     ((consp x) (walk (car x)) (walk (cdr x))))))
      (walk instrs))
    (loop for tail on instrs
          for ins = (car tail)
          do (when (and (consp ins) (eq (second ins) key))
               (case (car ins)
                 (:declare-local (incf allowed))
                 (:stloc (incf allowed) (incf stores))
                 (:ldloc
                  (let ((next (cdr tail)))
                    (loop while (equal (car next) '(:call "Runtime.UnwrapMv"))
                          do (setf next (cdr next)))
                    (when (equal (car next) '(:pop))
                      (incf allowed)))))))
    (and (= stores 1) (= total allowed))))

(defun %compile-let-1 (bindings body sequential-p boolean-tail no-raw)
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
           ;; Bindings that copy a local with a hoisted element buffer, looked
           ;; up here, in the scope the inits are evaluated in, before this
           ;; LET's own names shadow anything (see %ALIAS-ARRAY-BACKING-BINDINGS).
           (*let-backing-sources* (%let-backing-sources binding-info sequential-p))
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
           ;; A counting loop's variable becomes a declared bounded integer
           ;; when its %INDUCTION claim holds (see %INDUCTION-RANGE). Before
           ;; anything below reads the declarations.
           (bounded-candidate (%bounded-loop-candidate body binding-info real-body
                                                       sequential-p))
           (body (%resolve-induction-declarations body binding-info real-body
                                                  mutated sequential-p))
           ;; An assigned variable whose init and every assigned value stay in
           ;; one integer range is declared that type (see
           ;; %ASSIGNED-BINDING-DECLARATIONS), before anything below reads the
           ;; declarations.
           (body (let ((extra (%assigned-binding-declarations
                               binding-info body real-body mutated captured
                               sequential-p)))
                   (if extra (cons `(declare ,@extra) body) body)))
           (body (let ((u64 (%u64-slot-binding-names binding-info body scan-forms
                                                     mutated captured)))
                   (if u64 (cons `(declare (type (unsigned-byte 64) ,@u64)) body) body)))
           (body (let ((sgl (and (not sequential-p)
                                 (%single-binding-names binding-info body real-body
                                                        mutated captured))))
                   (if sgl (cons `(declare (single-float ,@sgl)) body) body)))
           (raw-plan (%raw-array-plan binding-info body real-body
                                      mutated captured no-raw))
           (*raw-array-plan* (append raw-plan *raw-array-plan*))
           (*raw-array-backings* (append (mapcar #'second raw-plan)
                                         *raw-array-backings*))
           ;; Separate
           (special-bindings (remove-if-not #'third binding-info))
           (lexical-bindings (remove-if #'third binding-info))
           ;; Long-rep candidates: fixnum-declared plain lexicals whose slot can
           ;; hold a raw Int64 instead of a boxed Fixnum. Requires: not special,
           ;; not captured by any closure (env capture loads the slot as an
           ;; object reference), and a fixnum-typed init so the initial store is
           ;; well-typed. Mutation is fine: compile-setq has an Int64-slot
           ;; path. References in boxed contexts re-box via Fixnum.Make; native
           ;; contexts (compare/arith/aref index) read the slot directly, which
           ;; is what removes the per-iteration box/unbox from loop counters.
           ;; A binding declared with a bounded integer type inside int64,
           ;; (UNSIGNED-BYTE 32) say, is promoted the same way: every value the
           ;; declaration admits fits the slot. Its TIGHT range stays recorded
           ;; (EXTRACT-SMALL-INT-LOCALS) and is what EXPR-INT-RANGE answers for
           ;; it, so a product of two such variables still has to be proven to
           ;; fit before it is computed raw.
           (fx-decl-names (append (extract-fixnum-locals body)
                                  (mapcar #'car (extract-small-int-locals body))))
           (fx-rep-names
             (when fx-decl-names
               (loop for b in binding-info
                     for nm = (var-name (first b))
                     when (and (not (third b))
                               (second b)
                               (member nm fx-decl-names :test #'string=)
                               (not (member nm captured :test #'string=))
                               ;; The declaration covers the stored value, so
                               ;; an init that is right only modulo 2^64 is
                               ;; enough here. COMPILE-EXPR-TO-LONG lowers it
                               ;; with a proof or an overflow check anyway.
                               (or (fixnum-wrap-typed-p (second b))
                                   ;; A word half computed from an (unsigned-byte
                                   ;; 64) in the 64-bit lane (COMPILE-INT-SLOT-INIT).
                                   ;; Parallel LET only: the init is planned in the
                                   ;; scope it is evaluated in.
                                   (and (not sequential-p)
                                        (%u64-low-bits-plan (second b)) t)))
                       collect nm)))
           ;; The same slot promotion WITHOUT a fixnum declaration: a binding
           ;; whose init has a statically proven int64 range (EXPR-INT-RANGE).
           ;; crc-division-step's (let ((new-rmdr (logior bit (* rmdr 2)))) ...)
           ;; is the motivating case: its init is already computed with native
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
               ;; outer variable of the same name: the same trap the native
               ;; decimal slots hit, where a LET*'s sibling init was declared
               ;; Decimal on the strength of an enclosing binding's type.
               (loop for b in binding-info
                     for nm = (var-name (first b))
                     when (and (not (third b))
                               ;; Compound init, or a variable already in a raw
                               ;; slot. Another atom costs nothing to store boxed,
                               ;; so promoting it only adds Fixnum.Make to every
                               ;; generic read.
                               (or (consp (second b))
                                   (native-slot-p (second b) (cstate-long-locals)))
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
           ;; Decimal-rep candidates: same rule as the float slots: declared
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
                       collect nm)))
           ;; The remaining name-keyed type declaration of this body, needed by
           ;; the LET* loop for the same reason the four above are.
           (chr-decl-names (extract-character-locals body))
           ;; Character-rep candidates: same rule again, with CHARACTER-TYPED-P
           ;; as the init test. That predicate is deliberately narrow -- it wants
           ;; an init whose CODE is reachable, not merely an init that is a
           ;; character -- because the whole point of the slot is to hold the
           ;; code, and an init that can only produce a LispChar would just move
           ;; the unwrapping to the binding.
           (char-rep-names
             (when chr-decl-names
               (loop for b in binding-info
                     for nm = (var-name (first b))
                     when (and (not (third b)) (second b)
                               (member nm chr-decl-names :test #'string=)
                               (not (member nm captured :test #'string=))
                               (character-typed-p (second b)))
                       collect nm)))
           ;; (unsigned-byte 64) slots: declared so, plain lexical, not
           ;; captured, with an init. The slot holds the value's bits; the
           ;; init and every assignment are checked to be an (unsigned-byte
           ;; 64) on the way in (COMPILE-AS-U64-BITS). Not under the in-process
           ;; debugger's frame recording, which has no entry for these bits.
           (u64-decl-names (and (not *emit-frame-locals*)
                                (extract-u64-locals body)))
           (u64-rep-names
             (when u64-decl-names
               (loop for b in binding-info
                     for nm = (var-name (first b))
                     when (and (not (third b)) (second b)
                               (member nm u64-decl-names :test #'string=)
                               (not (member nm captured :test #'string=)))
                       collect nm)))
           ;; DOTCL-SIMD pack slots, (NAME . KIND): declared a pack type, plain
           ;; lexical, not captured, with an init known to be a pack of KIND.
           (simd-decls (and (not *emit-frame-locals*)
                            (extract-simd-locals body)))
           (simd-rep
             (when simd-decls
               (loop for b in binding-info
                     for nm = (var-name (first b))
                     for d = (assoc nm simd-decls :test #'string=)
                     when (and d (not (third b)) (second b)
                               (not (member nm captured :test #'string=))
                               (simd-typed-init-p (second b) (cdr d)))
                       collect d)))
           (simd-rep-names (mapcar #'car simd-rep)))
      ;; Build new locals alist
      (let ((instrs
      (let* ((new-local-entries
               (mapcar (lambda (b)
                         (cons (first b) (fourth b)))
                       lexical-bindings))
             (init-instrs '())
             (bind-instrs '())
             ;; LET* only: (INSTRUCTIONS . SYMBOL) for each special binding, in
             ;; reverse source order. Everything up to and including that
             ;; symbol's DynamicBindings.Push is in INSTRUCTIONS; whatever the
             ;; loop emits afterwards belongs INSIDE the region that pops it.
             ;; See %WRAP-SPECIAL-POP.
             (bind-segments '())
             (special-syms '()))
        ;; For let (not let*): evaluate all inits in old scope first
        (if (not sequential-p)
            ;; Let: evaluate all inits, store to temp locals (only when the
            ;; binding routes through a special-var Push or a boxed array).
            ;; Plain lexical, non-boxed bindings store the init result directly
            ;; into the final binding key: no temp needed.
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
              ;; Evaluate inits in old scope: inits bind a single value:
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
                                             ,@(compile-int-slot-init init-form)
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
                           ((member nm char-rep-names :test #'string=)
                             ;; Native character rep: the slot holds the code.
                             (setf init-instrs
                                   (append init-instrs
                                           `((:declare-local ,key "Int64")
                                             ,@(compile-char-native-value init-form)
                                             (:stloc ,key)
                                             ,@(%local-var-marker key (first b) :char)))))
                           ((assoc nm simd-rep :test #'string=)
                            (let ((kind (cdr (assoc nm simd-rep :test #'string=))))
                              (setf init-instrs
                                    (append init-instrs
                                            `((:declare-local ,key ,(%simd-local-type kind))
                                              ,@(compile-simd-local-value init-form kind)
                                              (:stloc ,key)
                                              ,@(when *emit-source-lines*
                                                  `((:local-var ,key ,nm))))))))
                           ((member nm u64-rep-names :test #'string=)
                             ;; (unsigned-byte 64) rep: the slot holds the bits.
                             (setf init-instrs
                                   (append init-instrs
                                           `((:declare-local ,key "Int64")
                                             ,@(compile-as-u64-bits init-form)
                                             (:stloc ,key)
                                             ,@(when *emit-source-lines*
                                                 `((:local-var ,key ,(var-name (first b)))))))))
                           (t
                             (let ((init-code
                                     (let ((*in-tail-position* nil)
                                           (*in-mv-context* nil))
                                       (%let-init-code key init-form))))
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
              ;; %strings-minus, not SET-DIFFERENCE :test #'string=: both
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
                                 (cstate-native-decimal-locals))
                         +cs-native-char-locals+
                         (append (binding-keys-named binding-info char-rep-names)
                                 (cstate-native-char-locals))
                         +cs-u64-locals+
                         (append (binding-keys-named binding-info u64-rep-names)
                                 (cstate-u64-locals))
                         +cs-simd-locals+
                         (append (loop for b in binding-info
                                       for d = (assoc (var-name (first b)) simd-rep
                                                      :test #'string=)
                                       when d collect (cons (fourth b) (cdr d)))
                                 (cstate-simd-locals)))))
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
                ;; Compile body: inherits outer *in-tail-position*.
                ;; But when we have special bindings, the body is wrapped in a
                ;; try/finally (to Pop on exit), so self-TCO's raw `br` to the
                ;; loop label outside the try would produce invalid IL. Flag
                ;; *in-try-block* suppresses TCO branch generation in that case
                ;; while still allowing MV propagation through tail position.
                (let* ((let-bound-names (mapcar (lambda (b) (var-name (first b)))
                                                binding-info))
                       (body-instrs (let ((*bounded-loop-candidate* bounded-candidate))
                                      (compile-let-body-instrs
                                       real-body body binding-info let-bound-names
                                       needs-boxing mutated special-syms nil
                                       boolean-tail))))
                  (compile-let-with-specials
                   init-instrs bind-instrs body-instrs
                   (reverse special-syms)))))
            ;; Let*: sequential binding: incrementally extend *locals*
            (let ((*specials* all-specials)
                  (*symbol-macros* *symbol-macros*)
                  ;; The name-keyed type tables move with *LOCALS*. A binding
                  ;; type declaration in this LET*'s body starts applying at the
                  ;; binding it names (CLHS 3.3.4), so it covers every LATER init
                  ;; in the same LET*; and a name this LET* binds without such a
                  ;; declaration is a different variable from the outer one, so
                  ;; the outer entry has to stop applying. Before this, neither
                  ;; half held between siblings -- the tables were established
                  ;; only on the way into the BODY -- so (LET* ((X ..) (Y ..)
                  ;; (LEN (+ (* X X) (* Y Y)))) (DECLARE (DOUBLE-FLOAT X Y LEN))
                  ;; ..) compiled its multiplies on the generic path, and the
                  ;; same LET* rebinding a declared name as another type could
                  ;; read the new binding through the old declaration.
                  ;; Rebound here because the loop below SETFs them.
                  (*fixnum-locals* *fixnum-locals*)
                  (*character-locals* *character-locals*)
                  (*double-float-locals* *double-float-locals*)
                  (*single-float-locals* *single-float-locals*)
                  (*decimal-locals* *decimal-locals*)
                  (inferred-sgl '())
                  ;; Extended/shadowed incrementally as bindings are processed
                  ;; so each init sees exactly the earlier siblings' slots.
                  (*cstate* *cstate*))
              ;; Bind one at a time; compile init THEN extend scope
              (loop for b in binding-info
                    do (let ((var (first b))
                             (init-form (second b))
                             (is-special (third b))
                             (key (fourth b)))
                           (if is-special
                               (let ((tmp (gen-local "SPLTMP")))
                                 (push var special-syms)
                                 ;; A dynamic binding carries no lexical type
                                 ;; declaration, and from here on this name does
                                 ;; not name the outer lexical either.
                                 (step-let*-type-locals (var-name var)
                                                        nil nil nil nil nil)
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
                                                 (:call "DynamicBindings.Push"))))
                                 ;; Close the segment here: this binding is now
                                 ;; live, and every later init form runs inside
                                 ;; the region whose cleanup pops it.
                                 (push (cons bind-instrs var) bind-segments)
                                 (setf bind-instrs '()))
                               ;; Lexical: compile init in current scope (not tail pos), then extend
                               (let* ((long-rep-p (or (member (var-name var) long-rep-names
                                                              :test #'string=)
                                                      (%let*-range-rep-p
                                                       b binding-info real-body
                                                       captured mutated needs-boxing
                                                       (append fx-rep-names
                                                               double-rep-names
                                                               single-rep-names
                                                               decimal-rep-names
                                                               char-rep-names
                                                               u64-rep-names
                                                               simd-rep-names))))
                                      (nfk (cond ((member (var-name var) double-rep-names :test #'string=) :double)
                                                 ((member (var-name var) single-rep-names :test #'string=) :single)
                                                 (long-rep-p nil)
                                                 (t (%let*-float-rep b binding-info real-body
                                                                     captured mutated needs-boxing
                                                                     body dbl-decl-names sgl-decl-names))))
                                      (inferred-p (and (eq nfk :single)
                                                       (not (member (var-name var) sgl-decl-names
                                                                    :test #'string=))
                                                       (push var inferred-sgl)))
                                      (dec-rep-p (member (var-name var) decimal-rep-names
                                                         :test #'string=))
                                      (chr-rep-p (member (var-name var) char-rep-names
                                                         :test #'string=))
                                      (u64-rep-p (member (var-name var) u64-rep-names
                                                         :test #'string=))
                                      (simd-kind (cdr (assoc (var-name var) simd-rep
                                                             :test #'string=)))
                                      (init-code (cond
                                                   (long-rep-p (compile-expr-to-long init-form))
                                                   (nfk (compile-float-native-value init-form nfk))
                                                   (dec-rep-p (compile-decimal-native-value init-form))
                                                   (chr-rep-p (compile-char-native-value init-form))
                                                   (u64-rep-p (compile-as-u64-bits init-form))
                                                   (simd-kind (compile-simd-local-value init-form simd-kind))
                                                   (t (let ((*in-tail-position* nil)
                                                            (*in-mv-context* nil))
                                                        (%let-init-code key init-form)))))
                                      ;; The range INFER-SMALL-INT-BINDINGS gives
                                      ;; this binding for the body, computed in the
                                      ;; init's own scope so the later inits of this
                                      ;; LET* can use it too.
                                      (init-range
                                        (and init-form
                                             (not (or (member (var-name var) long-rep-names
                                                              :test #'string=)
                                                      nfk dec-rep-p chr-rep-p u64-rep-p simd-kind))
                                             (not (member (var-name var) needs-boxing
                                                          :test #'string=))
                                             (not (member (var-name var) mutated
                                                          :test #'string=))
                                             (let ((r (expr-int-range init-form)))
                                               (and r (range-fits-int64-p r) r)))))
                                 ;; Extend scope for subsequent bindings and body
                                 (setf *cstate*
                                       (cstate-with *cstate* +cs-locals+
                                                    (acons var key (cstate-locals))))
                                 (when init-range
                                   (setf *cstate*
                                         (cstate-with *cstate* +cs-small-int-locals+
                                                      (acons key init-range
                                                             (cstate-small-int-locals)))))
                                 ;; ...and the name-keyed type tables with it, so a
                                 ;; later init in this same LET* sees this
                                 ;; binding's declaration (CLHS 3.3.4) and stops
                                 ;; seeing an outer one this binding shadows.
                                 (step-let*-type-locals (var-name var)
                                                        fx-decl-names chr-decl-names
                                                        dbl-decl-names
                                                        (if inferred-p
                                                            (cons (var-name var) sgl-decl-names)
                                                            sgl-decl-names)
                                                        dec-decl-names)
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
                                 (when chr-rep-p
                                   (setf *cstate*
                                         (cstate-with *cstate* +cs-native-char-locals+
                                                      (cons key (cstate-native-char-locals)))))
                                 (when u64-rep-p
                                   (setf *cstate*
                                         (cstate-with *cstate* +cs-u64-locals+
                                                      (cons key (cstate-u64-locals)))))
                                 (when simd-kind
                                   (setf *cstate*
                                         (cstate-with *cstate* +cs-simd-locals+
                                                      (acons key simd-kind (cstate-simd-locals)))))
                                 ;; Shadow any symbol-macro with this name
                                 (setf *symbol-macros*
                                       (remove var *symbol-macros*
                                               :key #'car :test #'eq))
                                 (if (member (var-name var) needs-boxing :test #'string=)
                                     (progn
                                       (setf *cstate*
                                             (cstate-with *cstate* +cs-boxed-vars+
                                                          (cons var (cstate-boxed-vars))))
                                       (setf bind-instrs
                                             (append bind-instrs
                                                     (emit-box-create key init-code var))))
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
                                                               ;; A character code, at the
                                                               ;; same width the comparison
                                                               ;; path uses.
                                                               (chr-rep-p "Int64")
                                                               ;; (unsigned-byte 64) bits.
                                                               (u64-rep-p "Int64")
                                                               (simd-kind (%simd-local-type simd-kind))
                                                               (t "LispObject")))
                                                       ,@init-code
                                                       (:stloc ,key)
                                                       ,@(%local-var-marker
                                                          key var
                                                          (cond (long-rep-p :long)
                                                                (dec-rep-p :decimal)
                                                                (chr-rep-p :char)
                                                                (t nfk))))))))))))
              ;; Remove declared-specials from *locals* so body references use dynamic binding
              (let* ((let-bound-names (mapcar (lambda (b) (var-name (first b)))
                                             binding-info))
                     ;; Body inherits outer *in-tail-position*; TCO branches
                     ;; are suppressed via *in-try-block* when special-syms
                     ;; introduce a try/finally (see parallel-let branch).
                     (body-instrs (compile-let-body-instrs
                                    real-body
                                    (if inferred-sgl
                                        (cons `(declare (single-float ,@inferred-sgl)) body)
                                        body)
                                    binding-info let-bound-names
                                    needs-boxing mutated special-syms declared-specials
                                    boolean-tail)))
                (%compile-let-star-specials
                 bind-segments bind-instrs body-instrs)))))))
        (values instrs (and raw-plan (%raw-array-failures raw-plan instrs)))))))

(defun %let*-range-rep-p (b binding-info real-body captured mutated needs-boxing
                          other-rep-names)
  "Whether the LET* binding B gets a raw Int64 slot on the strength of its init's
   proven range: the RANGE-REP-NAMES promotion of a parallel LET, decided here
   one binding at a time because only now does the init see exactly the earlier
   siblings' bindings (EXPR-INT-RANGE answers for the current scope).

   Same conditions as the parallel LET, with one addition: an init that is a
   variable already held in a raw Int64 slot qualifies too, since copying it
   raw costs nothing while storing it boxed costs a Fixnum.Make. That is the
   SETF expansion's temp for a place argument that is itself a native variable.
   The native use is looked for in the later inits as well as the body."
  (let ((var (first b)) (init (second b)))
    (and (not (third b))
         init
         (let ((nm (var-name var)))
           (and (not (member nm other-rep-names :test #'string=))
                (not (member nm captured :test #'string=))
                (not (member nm needs-boxing :test #'string=))
                (not (member nm mutated :test #'string=))))
         (or (consp init)
             (native-slot-p init (cstate-long-locals)))
         (%native-int-use-p var (append (mapcar #'second
                                                (cdr (member b binding-info :test #'eq)))
                                        real-body))
         (let ((r (expr-int-range init)))
           (and r (range-fits-int64-p r))))))

(defun %u64-slot-binding-names (binding-info body forms mutated captured)
  "The variables to declare (UNSIGNED-BYTE 64): undeclared, unassigned, not
   captured, bound to a read of a structure slot declared so, and read in FORMS
   only as a direct operand of the operators the 64-bit lane computes, so that
   holding the bits never costs a box at a use."
  (loop for b in binding-info
        for v = (first b)
        for nm = (var-name v)
        when (and (not (third b))
                  (consp (second b))
                  (not *emit-frame-locals*)
                  (not (member nm mutated :test #'string=))
                  (not (member nm captured :test #'string=))
                  (not (%avt-var-declared-p v body))
                  (= 1 (count nm binding-info
                              :key (lambda (x) (var-name (first x))) :test #'string=))
                  (%u64-struct-slot-ref (second b))
                  (%u64-lane-uses-only-p v forms))
          collect v))

(defun %u64-lane-uses-only-p (var forms)
  "True when VAR occurs in FORMS, and only as a direct operand of LDB, LOGAND,
   LOGIOR, LOGXOR, ASH, +, -, * or MOD."
  (let ((total 0) (lane 0))
    (labels ((walk (x)
               (cond ((eq x var) (incf total))
                     ((consp x)
                      (when (member (car x) '(ldb logand logior logxor ash + - * mod))
                        (do ((y (cdr x) (cdr y))) ((atom y))
                          (when (eq (car y) var) (incf lane))))
                      (do ((y x (cdr y))) ((atom y) (when (eq y var) (incf total)))
                        (walk (car y)))))))
      (walk forms))
    (and (> total 0) (= total lane))))

(defun %single-binding-names (binding-info body real-body mutated captured)
  "The variables of a parallel LET to declare SINGLE-FLOAT: undeclared,
   unassigned, not captured, their init SINGLE-FLOAT-TYPED-P, and read in float
   arithmetic in the body."
  (loop for b in binding-info
        for v = (first b)
        for nm = (var-name v)
        when (and (not (third b))
                  (consp (second b))
                  (not (member nm mutated :test #'string=))
                  (not (member nm captured :test #'string=))
                  (not (%avt-var-declared-p v body))
                  (= 1 (count nm binding-info
                              :key (lambda (x) (var-name (first x))) :test #'string=))
                  (single-float-typed-p (second b))
                  (%native-float-use-p v real-body))
          collect v))

(defun %let*-float-rep (b binding-info real-body captured mutated needs-boxing
                        body dbl-decl-names sgl-decl-names)
  "The native float slot kind (:DOUBLE / :SINGLE / NIL) of the LET* binding B,
   decided with the init in its own scope, after the earlier siblings. A binding
   declared DOUBLE-FLOAT or SINGLE-FLOAT qualifies when its init is typed so; an
   undeclared, unassigned one whose init is SINGLE-FLOAT-TYPED-P qualifies when a
   later init or the body reads it in float arithmetic."
  (let* ((var (first b)) (init (second b)) (nm (var-name var)))
    (when (and (not (third b))
               init
               (not (member nm captured :test #'string=))
               (not (member nm needs-boxing :test #'string=)))
      (cond ((member nm dbl-decl-names :test #'string=)
             (and (double-float-typed-p init) :double))
            ((member nm sgl-decl-names :test #'string=)
             (and (single-float-typed-p init) :single))
            ((and (not (member nm mutated :test #'string=))
                  (not (%avt-var-declared-p var body))
                  (= 1 (count nm binding-info
                              :key (lambda (x) (var-name (first x))) :test #'string=))
                  (consp init)
                  (single-float-typed-p init)
                  (%native-float-use-p var (append (mapcar #'second
                                                           (cdr (member b binding-info :test #'eq)))
                                                   real-body)))
             :single)))))

(defun rekey-by-binding (name-alist binding-info)
  "Re-key a (NAME . INFO) alist to (SLOT-KEY . INFO) using BINDING-INFO. Entries
   naming nothing this form binds are dropped; the declaration scanners run over
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
          ;; tolerates any order, so this is behavior-neutral today: but it lets a
          ;; shallow-binding Pop take its O(1) top-of-stack fast path instead of the
          ;; out-of-order search fallback.
          ,@(loop for sym in (reverse special-syms)
                  append `(,@(compile-sym-lookup sym)
                           (:castclass "Symbol") (:call "DynamicBindings.Pop")))
          (:end-exception-block)
          (:ldloc ,result-key)))))

(defun %wrap-special-pop (sym inner-instrs)
  "INNER-INSTRS inside a protected region whose cleanup pops SYM's dynamic
   binding. The region begins AFTER the DynamicBindings.Push that established
   it, so the cleanup can only run on a path where the push happened -- which is
   what makes a per-binding region safe where one shared region is not. Pop
   finds its entry by symbol, so popping a binding that was never pushed would
   take an ENCLOSING binding of the same name instead."
  (let ((result-key (gen-local "SRES")))
    `((:declare-local ,result-key "LispObject")
      (:begin-exception-block)
      ,@inner-instrs
      (:stloc ,result-key)
      (:begin-finally-block)
      ,@(compile-sym-lookup sym)
      (:castclass "Symbol")
      (:call "DynamicBindings.Pop")
      (:end-exception-block)
      (:ldloc ,result-key))))

(defun %compile-let-star-specials (segments tail-instrs body-instrs)
  "Assemble a LET* whose bindings are evaluated one at a time. SEGMENTS is
   innermost-first; each is (INSTRUCTIONS . SYMBOL), where INSTRUCTIONS ends
   with SYMBOL's DynamicBindings.Push. TAIL-INSTRS is what the last special
   binding is followed by.

   The regions nest, one per special binding, rather than one region around all
   of them. A LET* evaluates each init form in the scope of the earlier
   bindings, so an init that exits non-locally -- a THROW, a RETURN-FROM, an
   error -- leaves the bindings made before it live. With a single region opened
   after the last push, that exit escaped before the region was entered and the
   earlier bindings were never popped: the dynamic stack kept an entry nobody
   would remove, and every later read of that variable, for the rest of the
   thread, saw the abandoned value.

   With no special bindings this is the plain instruction sequence -- the same
   one COMPILE-LET-WITH-SPECIALS produces for that case, peephole included -- so
   an ordinary LET* is unchanged."
  (if (null segments)
      (eliminate-single-ref-locals (append tail-instrs body-instrs))
      (let ((inner (append tail-instrs body-instrs)))
        (dolist (seg segments inner)
          (setq inner (append (car seg) (%wrap-special-pop (cdr seg) inner)))))))

(defun compile-let-star (bindings body)
  "Compile let* by delegating to compile-let with sequential flag."
  (compile-let bindings body t))
