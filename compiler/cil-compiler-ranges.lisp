;;; cil-compiler-ranges.lisp: Fixnum typing, numeric-backed arrays, integer ranges, constant folding
;;; Part of the CIL compiler (A2 instruction list architecture)

(in-package :dotcl.cil-compiler)

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
;;; Overflow safety: the raw int64 path is only taken when a static value-range
;;; analysis (expr-int-range) proves every intermediate +/-/*/1+/1- result fits
;;; in int64. dotcl's fixnum is full int64 (most-positive-fixnum = 2^63-1) with no
;;; tag headroom, so bare (declare (fixnum ...)) operands have the full int64 range
;;; and nested arithmetic over them is NOT provably safe: it falls back to the
;;; boxed promoting path (Runtime.Add/Subtract/Multiply) which yields a bignum on
;;; overflow. Tightly-declared operands ((the (integer lo hi) ...)) keep the fast
;;; unboxed path when their composed range stays in int64.
;;; ============================================================

;;; int64 bounds, as exact host integers.
(defconstant +int64-min+ -9223372036854775808)
(defconstant +int64-max+ +9223372036854775807)

(defun range-fits-int64-p (r)
  "True if range R = (lo . hi) lies entirely within int64."
  (and r (<= +int64-min+ (car r)) (<= (cdr r) +int64-max+)))

(defun small-int-local-range (expr)
  "If EXPR is a reference to a small-int local (see CSTATE-SMALL-INT-LOCALS) that is a
   plain non-captured slot, return its proven (LO . HI) range; else NIL. These
   slots hold a boxed Fixnum, so compile-as-long can unbox them inline."
  (and (symbolp expr)
       (boundp '*cstate*)
       (not (boxed-var-p expr))
       (let ((key (lookup-local expr)))
         (and key
              ;; Slot-keyed, like the native tables: the proven range belongs to
              ;; the binding, so an inner binding of the same name resolves to a
              ;; different slot and never inherits it. (It used to be name-keyed,
              ;; and an inner (let ((f 3))) handed its integer range to an outer
              ;; F holding a native double: native multiply on an r8 slot.)
              (cdr (assoc key (cstate-small-int-locals) :test #'equal))))))

;;; ------------------------------------------------------------
;;; Numeric-backed array locals: let bindings whose init is a make-array
;;; with a statically-known rank and a bounded-integer :element-type that
;;; the runtime stores unboxed (byte[]/ushort[]/int[]/long[]). aref on such
;;; a local can read/write the element as a raw int64 (Runtime.ArefNum*L),
;;; and the element's storage range feeds expr-int-range so surrounding
;;; arithmetic can prove int64-safety and stay native.
;;; ------------------------------------------------------------

(defun %numeric-storage-range (spec)
  "Storage range (LO . HI) of the unboxed backing the runtime picks for
   element-type SPEC, or NIL when SPEC gets no numeric backing. Must mirror
   the runtime upgrade (ParseElementTypeName + NumKindForElementType):
   [0,1] upgrades to bit-packed BIT (not numeric), [0,255]->u8,
   [0,65535]->u16, int32->i32, int64->i64, wider (e.g. (unsigned-byte 64))
   stays boxed."
  (if (eq spec 'fixnum)
      (cons +int64-min+ +int64-max+)
      (let ((r (integer-type-range spec)))
        (and r
             (let ((lo (car r)) (hi (cdr r)))
               (cond
                 ((and (<= 0 lo) (<= hi 1)) nil)
                 ((and (<= 0 lo) (<= hi 255)) (cons 0 255))
                 ((and (<= 0 lo) (<= hi 65535)) (cons 0 65535))
                 ((and (<= -2147483648 lo) (<= hi 2147483647))
                  (cons -2147483648 2147483647))
                 ((range-fits-int64-p r) (cons +int64-min+ +int64-max+))
                 (t nil)))))))

(defun %float-storage-kind (spec)
  "Float backing kind keyword for element-type SPEC, mirroring the runtime
   NumKindForElementType (single/short-float->float[]=:single,
   double/long-float->double[]=:double), or NIL when SPEC is not a float type."
  (cond ((member spec '(single-float short-float)) :single)
        ((member spec '(double-float long-float)) :double)
        (t nil)))

(defun %u64-value-type-p (type)
  "T when TYPE, a declared type, is a non-negative integer range that needs the
   64th bit and fits in it. DEFTYPEs are expanded."
  (let ((r (declared-integer-range type)))
    (and r
         (>= (car r) 0)
         (<= (cdr r) #xFFFFFFFFFFFFFFFF)
         (> (cdr r) +int64-max+))))

(defun %u64-storage-p (spec)
  "T when element-type SPEC gets the runtime's (unsigned-byte 64) storage, the
   values' bits in a ulong[]: a non-negative integer range that needs the 64th
   bit. Must mirror the runtime upgrade (ParseElementTypeName): narrower ranges
   get the storage %NUMERIC-STORAGE-RANGE describes."
  (let ((r (integer-type-range spec)))
    (and r
         (>= (car r) 0)
         (<= (cdr r) #xFFFFFFFFFFFFFFFF)
         (> (cdr r) +int64-max+))))

(defun %expand-array-type (type)
  "TYPE with a user DEFTYPE expanded when TYPE is not already written as a
   SIMPLE-ARRAY / ARRAY / VECTOR specifier. A DEFTYPE name is the same type as
   its expansion, so (deftype s-box () '(simple-array (unsigned-byte 32) (1024)))
   declares exactly what the expansion does; without this a declaration through
   a type name chose the generic per-element path. Anything that does not
   expand comes back unchanged."
  (if (or (and (symbolp type) type)
          (and (consp type) (symbolp (car type))
               (not (member (car type) '(simple-array array vector)))))
      (%typexpand-for-range type)
      type))

(defun %simple-array-backing-kind (type)
  "Element-buffer kind for a (SIMPLE-ARRAY <integer type> (DIM...)) specifier of
   rank 1 to 3 -- 1 = byte[], 2 = ushort[], 3 = int[], 4 = long[], mirroring the
   runtime's _numKind, :U64 for (unsigned-byte 64) -- or :F64 / :F32 for a
   DOUBLE-FLOAT / SINGLE-FLOAT element type, or NIL when TYPE is none of these.

   Only SIMPLE-ARRAY qualifies, and that is the whole safety argument for
   hoisting the buffer out of a loop: CLHS 1.4.4 says a simple array is neither
   displaced nor adjustable nor fill-pointered, so its element storage cannot be
   replaced while the binding lives (ADJUST-ARRAY on a simple array returns a
   fresh array rather than rewriting this one). ARRAY and VECTOR specifiers
   promise none of that and keep the per-element helper."
  (setf type (%expand-array-type type))
  (and (consp type)
       (eq (car type) 'simple-array)
       (consp (cdr type))
       (consp (cddr type))
       (let ((dims (caddr type)))
         (and (consp dims) (<= 1 (length dims) 3)
              (every (lambda (d) (or (eq d '*) (and (integerp d) (<= 0 d #x7fffffff))))
                     dims)))
       (let ((r (%numeric-storage-range (cadr type))))
         (cond
           ((member (cadr type) '(double-float long-float)) :f64)
           ((member (cadr type) '(single-float short-float)) :f32)
           ((%u64-storage-p (cadr type))
             :u64)
           (t
         (and r
              (cond ((and (eql (car r) 0) (eql (cdr r) 255)) 1)
                    ((and (eql (car r) 0) (eql (cdr r) 65535)) 2)
                    ((eql (car r) -2147483648) 3)
                    (t 4))))))))

(defun %simple-array-backing-dims (type)
  (setf type (%expand-array-type type))
  (let ((dims (caddr type)))
    (and (cdr dims) dims)))

(defun %backing-slot-type (kind)
  "CIL local type for a KIND element buffer. The (unsigned-byte 64) buffer is
   a ulong[] held as an Int64[] (Runtime.BackingU64): its elements are read and
   written as bits."
  (ecase kind (1 "Byte[]") (2 "UInt16[]") (3 "Int32[]") (4 "Int64[]")
         (:u64 "Int64[]") (:char "Char[]") (:f64 "Double[]") (:f32 "Single[]")))

(defun %backing-fetch-call (kind)
  "Runtime entry that checks the declaration once and returns the buffer.

   Above (safety 0) the *Checked entry: it signals a TYPE-ERROR where the plain
   one answers null for a non-simple array (adjustable, fill-pointered,
   displaced), which a SIMPLE-* declaration says the value is not. At (safety 0)
   the declaration is trusted, and the null sends each access to the
   per-element helper, which reads the right values. The branch is on the path
   the plain fetch takes when it declines, so a true declaration pays nothing."
  (if (compiling-at-safety-0-p)
      (ecase kind
        (1 "Runtime.BackingU8") (2 "Runtime.BackingU16")
        (3 "Runtime.BackingI32") (4 "Runtime.BackingI64")
        (:u64 "Runtime.BackingU64")
        (:char "Runtime.BackingChars")
        (:f64 "Runtime.BackingF64") (:f32 "Runtime.BackingF32"))
      (ecase kind
        (1 "Runtime.BackingU8Checked") (2 "Runtime.BackingU16Checked")
        (3 "Runtime.BackingI32Checked") (4 "Runtime.BackingI64Checked")
        (:u64 "Runtime.BackingU64Checked")
        (:char "Runtime.BackingCharsChecked")
        (:f64 "Runtime.BackingF64Checked") (:f32 "Runtime.BackingF32Checked"))))

(defun %backing-ldelem (kind)
  "Element load leaving a raw int64, or a raw r8 for the float kinds."
  (ecase kind (1 :ldelem-u1) (2 :ldelem-u2) (3 :ldelem-i4) (4 :ldelem-i8)
         (:u64 :ldelem-i8) (:char :ldelem-u2) (:f64 :ldelem-r8) (:f32 :ldelem-r4)))

(defun %backing-stelem (kind)
  "Element store taking a raw int64, or a raw r8 for the float kinds (the
   narrowing conv is in the opcode)."
  (ecase kind (1 :stelem-u1) (2 :stelem-u2) (3 :stelem-i4) (4 :stelem-i8)
         (:u64 :stelem-i8) (:char :stelem-u2) (:f64 :stelem-r8) (:f32 :stelem-r4)))

;; DOTCL:TYPEXPAND-1, looked up by name at load time: the SBCL cross-compile
;; host has no DOTCL package (see *TYPEXPAND-FN*).
(defvar *typexpand-1-fn*
  (ignore-errors
   (let ((sym (find-symbol "TYPEXPAND-1" "DOTCL")))
     (and sym (fboundp sym) (symbol-function sym)))))

(defun %range-element-width (spec)
  "+N when element type SPEC is an (INTEGER LO HI) / (MOD N) / (EQL N) range
   the runtime upgrades to (UNSIGNED-BYTE N), -N for (SIGNED-BYTE N), else NIL
   (BIT, a bound outside int64, or not such a range). Mirrors the runtime's
   ParseRequestedElementTypeName."
  (when (consp spec)
    (let ((lo nil) (hi nil))
      (case (car spec)
        (eql (when (and (consp (cdr spec)) (null (cddr spec)))
               (setf lo (cadr spec) hi (cadr spec))))
        (mod (when (and (consp (cdr spec)) (null (cddr spec))
                        (integerp (cadr spec)) (plusp (cadr spec)))
               (setf lo 0 hi (1- (cadr spec)))))
        (integer (when (and (consp (cdr spec)) (consp (cddr spec)) (null (cdddr spec)))
                   (setf lo (cadr spec) hi (caddr spec))
                   (when (and (consp lo) (integerp (car lo))) (setf lo (1+ (car lo))))
                   (when (and (consp hi) (integerp (car hi))) (setf hi (1- (car hi)))))))
      (and (typep lo '(signed-byte 64)) (typep hi '(signed-byte 64)) (<= lo hi)
           (not (and (>= lo 0) (<= hi 1)))
           (if (>= lo 0)
               (integer-length hi)
               (- (1+ (max (integer-length lo) (integer-length hi)))))))))

(defun %element-narrow-width (spec)
  "The runtime's narrow width for element type SPEC (LispVector._numNarrow): +N
   for (UNSIGNED-BYTE N) and -N for (SIGNED-BYTE N) whose storage is wider than
   the type ((unsigned-byte 32) in a long[], (signed-byte 8) in an int[]), else
   NIL. N is the width the runtime upgrades to (CanonicalElementTypeName), and
   an (INTEGER LO HI), (MOD N) or (EQL N) range is first named as the
   (UNSIGNED-BYTE N) / (SIGNED-BYTE N) of its INTEGER-LENGTH."
  ;; Expanded a step at a time, as the runtime does: a full expansion turns
  ;; (UNSIGNED-BYTE 32) itself into an INTEGER range.
  (loop repeat 16
        do (let ((w (%range-element-width spec)))
             (when w
               (setf spec (if (plusp w)
                              (list 'unsigned-byte w)
                              (list 'signed-byte (- w))))))
           (when (and (consp spec) (member (car spec) '(unsigned-byte signed-byte)))
             (return
               (and (consp (cdr spec)) (null (cddr spec)) (integerp (cadr spec))
                    (let ((n (cadr spec)))
                      (if (eq (car spec) 'unsigned-byte)
                          (let ((w (find-if (lambda (w) (<= n w)) '(2 4 7 8 15 16 31 32 62 63))))
                            (and w (>= n 2) (/= w 8) (/= w 16) w))
                          (let ((w (find-if (lambda (w) (<= n w)) '(8 16))))
                            (and w (>= n 1) (- w))))))))
           (unless (and *typexpand-1-fn*
                        (or (and (symbolp spec) spec)
                            (and (consp spec) (symbolp (car spec)))))
             (return nil))
           (multiple-value-bind (e expanded)
               (ignore-errors (funcall *typexpand-1-fn* spec))
             (unless expanded (return nil))
             (setf spec e))))

(defun %simple-array-store-narrow (type)
  "%ELEMENT-NARROW-WIDTH of the element type of the SIMPLE-ARRAY specifier TYPE
   (one %SIMPLE-ARRAY-BACKING-KIND accepted)."
  (setf type (%expand-array-type type))
  (and (consp type) (consp (cdr type)) (%element-narrow-width (cadr type))))

(defun %backing-store-check-instrs (kind narrow &optional val)
  "Instructions that take the raw int64 about to be stored into a KIND buffer
   and leave it, signalling first when it is not an element of the array's type.
   NARROW is the element type's width when it is narrower than the buffer (see
   %ELEMENT-NARROW-WIDTH): above (safety 0) the value is checked against that
   width, as the per-element path does, and not just against the buffer's.
   At (safety 0) only the buffer's own width is checked.

   VAL is the form whose value is stored. When its range is proven inside the
   range the check would enforce (the XOR of two octets stored as an octet),
   the check could never signal and nothing is emitted, at any safety."
  (let ((narrow (and narrow (not (compiling-at-safety-0-p)) narrow)))
    (if (%store-value-in-range-p val (%store-check-range kind narrow))
        nil
        (if narrow
            `((:ldc-i4 ,(abs narrow))
              (:call ,(if (plusp narrow) "Runtime.CheckStoreUnsigned" "Runtime.CheckStoreSigned")))
            (let ((chk (%backing-store-check kind)))
              (and chk `((:call ,chk))))))))

(defun %store-check-range (kind narrow)
  "(LO . HI), the values the store check for a KIND buffer accepts: the element
   type's when NARROW (see %ELEMENT-NARROW-WIDTH) is given, else the buffer's
   own width. NIL for the kinds that have no check."
  (cond (narrow (if (plusp narrow)
                    (cons 0 (1- (ash 1 narrow)))
                    (let ((h (ash 1 (1- (- narrow))))) (cons (- h) (1- h)))))
        ((eql kind 1) (cons 0 255))
        ((eql kind 2) (cons 0 65535))
        ((eql kind 3) (cons (- (ash 1 31)) (1- (ash 1 31))))
        (t nil)))

(defun %store-value-in-range-p (val range)
  "True when VAL's value is proven to lie in RANGE (LO . HI).
   At (safety 0) the proof is EXPR-INT-RANGE, which takes the declared types
   of variables and THE forms at their word. Above it the check is what
   reports a value outside the element type, and nothing checks a variable
   against its declaration on the way in, so only a proof from what is
   actually stored counts there: see %STORE-VALUE-RANGE."
  (and val range
       (let ((r (if (compiling-at-safety-0-p)
                    (expr-int-range val)
                    (%store-value-range val))))
         (and r (<= (car range) (car r)) (<= (cdr r) (cdr range))))))

(defvar *store-range-env* nil
  "Alist (VAR . (LO . HI)) of the variables bound by the LET forms
   %STORE-VALUE-RANGE is looking inside, innermost first.")

(defvar *store-range-declared* nil
  "True when %STORE-LET-RANGE may take a binding's declared integer type as its
   range, as the compiled LET does: for the value of a substituted local
   function, not for eliding a store's check.")

(defun %store-value-range (val)
  "(LO . HI) for VAL built only from what is actually stored and computed,
   else NIL:
   - integer literals
   - elements of integer arrays: the element type's range for an array made
     by the binding's own MAKE-ARRAY, or declared and matched by its fetch,
     with an element type narrower than its storage (an (UNSIGNED-BYTE 32)
     array in an Int64[]; every store into such an array is checked against
     the element type above (safety 0)), else the range of the storage
     (%ELEMENT-STORE-RANGE)
   - LOGAND / LOGIOR / LOGXOR of those. LOGAND with a non-negative operand is
     at most that operand; LOGIOR and LOGXOR of two non-negative operands have
     no bit above the higher one's top bit. (LDB (BYTE S P) X) is seen as the
     LOGAND it means (FOLD-TYPED-OPERAND), so it is in [0, 2^S - 1] whatever X is
   - (ASH X N) with X non-negative and N in [-64, 64]: the shifted bounds
   - (+ A B) and (- A B): the bounds' sums and differences
   - a call to a function proclaimed INLINE that is substituted here: the range
     of the substituted LET, so a ROL32 written as shifts and masks counts
   - (LET / LET* ((VAR INIT) ...) FORM) and (BLOCK NAME FORM), one body form,
     no SPECIAL binding: FORM's range with each VAR at its INIT's range. Every
     INIT has to have a range itself, so no code but these pure operators runs
     between a binding and its use, and a declared type of VAR is not used
     (nothing checks it when VAR is bound)."
  (let ((val (fold-nary-arith val)))
    (cond
      ((integerp val) (cons val val))
      ((and (symbolp val) val (assoc val *store-range-env* :test #'eq))
       (cdr (assoc val *store-range-env* :test #'eq)))
      ((and (not (and (consp val) (consp (cdr val)) (symbolp (cadr val))
                      (assoc (cadr val) *store-range-env* :test #'eq)))
            (%element-store-range val)))
      ((and (consp val) (= (length val) 3)
            (member (car val) '(logand logior logxor))
            (not (local-function-entry (car val))))
       (let ((ra (%store-value-range (cadr val)))
             (rb (%store-value-range (caddr val))))
         (if (and (eq (car val) 'logand)
                  (some (lambda (r) (and r (>= (car r) 0))) (list ra rb)))
             (let ((nonneg (remove-if-not (lambda (r) (and r (>= (car r) 0)))
                                          (list ra rb))))
               (cons 0 (reduce #'min (mapcar #'cdr nonneg))))
             (and ra rb
                  (if (and (>= (car ra) 0) (>= (car rb) 0))
                      (cons 0 (1- (ash 1 (integer-length (max (cdr ra) (cdr rb))))))
                      (let ((n (max (integer-length (car ra)) (integer-length (cdr ra))
                                    (integer-length (car rb)) (integer-length (cdr rb)))))
                        (cons (- (ash 1 n)) (1- (ash 1 n)))))))))
      ((and (consp val) (= (length val) 3) (eq (car val) 'ash)
            (not (local-function-entry 'ash)))
       (let ((rx (%store-value-range (cadr val)))
             (rn (%store-value-range (caddr val))))
         ;; ASH of a non-negative integer grows with both arguments.
         (and rx rn (>= (car rx) 0) (<= -64 (car rn)) (<= (cdr rn) 64)
              (cons (ash (car rx) (car rn)) (ash (cdr rx) (cdr rn))))))
      ((and (consp val) (= (length val) 3) (member (car val) '(+ -))
            (not (local-function-entry (car val))))
       (let ((ra (%store-value-range (cadr val)))
             (rb (%store-value-range (caddr val))))
         (and ra rb
              (if (eq (car val) '+)
                  (cons (+ (car ra) (car rb)) (+ (cdr ra) (cdr rb)))
                  (cons (- (car ra) (cdr rb)) (- (cdr ra) (car rb)))))))
      ;; The BLOCK of a substituted INLINE body carries the recursion guard
      ;; for its body, as where it is compiled: a call of the same function in
      ;; there is not substituted again.
      ((and (consp val) (eq (car val) 'block) (= (length val) 3))
       (let ((*inlining-stack* (inline-guard-stack (cadr val))))
         (%store-value-range (caddr val))))
      ((and (consp val) (member (car val) '(let let*)) (consp (cdr val)))
       (%store-let-range val))
      ((and (consp val) (symbolp (car val)))
       (let ((e (%substituted-inline-call val)))
         (and e (%store-value-range e))))
      (t nil))))

(defun %local-inline-call-range (expr)
  "The integer range of EXPR, a call to a local function that is substituted
   here: the range of the value form of its body with each parameter at the
   range of its argument. NIL when unknown. One body form, built only from the
   pure operators %STORE-VALUE-RANGE knows, so nothing assigns a parameter."
  (let ((plan (nth 5 (local-function-entry (car expr)))))
    (when (and plan
               (let ((*local-inline-report* nil)
                     (*local-inline-probe* t))
                 (maybe-expand-local-inline (car expr) expr)))
      (destructuring-bind (name params decls body blockp &rest more) plan
        (declare (ignore more))
        (when (and (not (member name *inlining-stack*))
                   (notany (lambda (d)
                             (some (lambda (spec) (and (consp spec) (eq (car spec) 'special)))
                                   (cdr d)))
                           decls)
                   (consp body) (null (cdr body))
                   (or (not blockp)
                       (notany (lambda (f) (form-has-return-from-p name f)) body)))
          (let* ((*store-range-declared* t)
                 (env (loop for p in params
                            for a in (cdr expr)
                            collect (cons p (%range-meet (expr-int-range a)
                                                         (%store-let-declared-range p decls))))))
            (let ((*store-range-env* env)
                  (*inlining-stack* (inline-guard-stack name)))
              (%store-value-range (car (last body))))))))))

(defun %range-meet (a b)
  "The range both A and B bound, either one when the other is NIL; B when they
   do not meet."
  (cond ((null a) b)
        ((null b) a)
        ((or (> (car a) (cdr b)) (< (cdr a) (car b))) b)
        (t (cons (max (car a) (car b)) (min (cdr a) (cdr b))))))

(defun %element-store-range (val)
  "For %STORE-VALUE-RANGE: the range of the element (AREF V I) reads, when V is
   an integer array local. The element type's range when that element type is
   narrower than the storage (its NARROW) and is known: V is bound to the array
   its own MAKE-ARRAY made (the backing entry is :MADE), or V's declared element
   type was matched against the vector's own when the buffer was fetched (the
   entry is :CHECKED, above (safety 0)). Else the storage's
   (NUMERIC-ARRAY-AREF-INFO): a declaration whose fetch matched only the buffer
   lets a FIXNUM vector through as an (UNSIGNED-BYTE 32) one."
  (let ((info (numeric-array-aref-info val)))
    (and info
         (let* ((e (array-backing-entry val))
                (narrow (and e (member (sixth e) '(:made :checked)) (fifth e))))
           (cond ((and (integerp narrow) (plusp narrow))
                  (cons 0 (1- (ash 1 narrow))))
                 ((and (integerp narrow) (minusp narrow))
                  (let ((h (ash 1 (1- (- narrow))))) (cons (- h) (1- h))))
                 (t (cdr info)))))))

(defun %store-let-declared-range (var decls)
  (when *store-range-declared*
    (dolist (d decls nil)
      (dolist (spec (cdr d))
        (when (consp spec)
          (multiple-value-bind (type vars)
              (if (eq (car spec) 'type)
                  (values (cadr spec) (cddr spec))
                  (values (car spec) (cdr spec)))
            (when (and (member var vars :test #'eq)
                       (not (member (car spec) '(ignore ignorable dynamic-extent inline
                                                 notinline optimize ftype special))))
              (let ((r (if (eq type 'fixnum)
                           (cons +int64-min+ +int64-max+)
                           (ignore-errors (declared-integer-range type)))))
                (when (and r (range-fits-int64-p r))
                  (return-from %store-let-declared-range r))))))))))

(defun %store-let-range (val)
  "%STORE-VALUE-RANGE of (LET / LET* BINDINGS DECL... FORM)."
  (let ((bindings (cadr val))
        (body (cddr val))
        (decls '()))
    (loop while (and (consp body) (consp (car body)) (eq (caar body) 'declare))
          do (push (pop body) decls))
    (and (listp bindings)
         (consp body) (null (cdr body))
         (notany (lambda (d)
                   (some (lambda (spec) (and (consp spec) (eq (car spec) 'special)))
                         (cdr d)))
                 decls)
         (let ((env *store-range-env*)
               (new '()))
           (dolist (b bindings
                      (let ((*store-range-env* (if (eq (car val) 'let)
                                                   (append new env)
                                                   env)))
                        (%store-value-range (car body))))
             (let ((var (if (consp b) (car b) b))
                   (init (and (consp b) (cdr b) (cadr b))))
               (unless (and var (symbolp var) (not (constantp var))
                            (not (global-special-p var)))
                 (return nil))
               (let* ((ri (let ((*store-range-env* (if (eq (car val) 'let) *store-range-env* env)))
                            (%store-value-range init)))
                      (r (%range-meet ri (%store-let-declared-range var decls))))
                 (unless r (return nil))
                 (if (eq (car val) 'let)
                     (push (cons var r) new)
                     (push (cons var r) env)))))))))

(defun %substituted-inline-call (expr)
  "The LET MAYBE-EXPAND-INLINE substitutes for EXPR, a call to a function
   proclaimed INLINE, when the call is compiled as that substitution (the
   conditions COMPILE-FORM goes through before it inlines); else NIL."
  (let ((op (car expr)))
    (and (symbolp op) op
         (gethash op *inline-defs*)
         (not (special-operator-name-p op))
         (not (gethash op *compile-form-handlers*))
         (not (local-function-entry op))
         (not (macrolet-shadowed-p op))
         (not (find-macro-expander op))
         (not (maybe-expand-compiler-macro op expr))
         (maybe-expand-inline op expr))))

(defun %backing-store-check (kind)
  "Runtime entry that signals the element-type violation an out-of-width store
   would otherwise commit silently, or NIL for the full-width kind. The boxed
   store path (LispVector.NumSet) raises the same error; narrowing in the
   opcode without this would wrap the value instead."
  (ecase kind
    (1 "Runtime.CheckStoreU8") (2 "Runtime.CheckStoreU16")
    (3 "Runtime.CheckStoreI32") (4 nil) (:u64 nil) (:char nil) (:f64 nil) (:f32 nil)))


(defun %simple-string-backing-kind (type)
  "The :CHAR backing kind for a SIMPLE-STRING type specifier of rank 1, else NIL.

   Accepts the names CL gives the same thing -- SIMPLE-STRING and
   SIMPLE-BASE-STRING -- and the spelled-out (SIMPLE-ARRAY CHARACTER (*)) with
   its BASE-CHAR and STANDARD-CHAR variants.

   Unlike %SIMPLE-ARRAY-BACKING-KIND this carries no safety argument of its own,
   because the declaration is not one here: a plain (MAKE-ARRAY n
   :element-type 'CHARACTER) is a simple string under CLHS while being a
   LispVector rather than a LispString, and programs pass adjustable and
   displaced character vectors under the declaration (SIMPLE-STRING-P used to
   accept them). The soundness lives entirely in Runtime.BackingChars, which
   takes the char[] of a LispString or of a simple rank-1 character LispVector
   and answers null for everything it cannot pin, and in the null arm every access
   carries. Above (safety 0) the fetch is BackingCharsChecked, which signals for
   the one case that is certainly a false declaration, a non-simple array, and
   still answers null for the rest."
  (labels ((char-elt-p (e)
             (and (symbolp e)
                  (member (symbol-name e) '("CHARACTER" "BASE-CHAR" "STANDARD-CHAR")
                          :test #'string=))))
    (and (or (and (symbolp type)
                  (member (symbol-name type) '("SIMPLE-STRING" "SIMPLE-BASE-STRING")
                          :test #'string=))
             (and (consp type)
                  (symbolp (car type))
                  (string= (symbol-name (car type)) "SIMPLE-ARRAY")
                  (consp (cdr type))
                  (char-elt-p (cadr type))
                  (let ((dims (and (cddr type) (caddr type))))
                    (and (consp dims) (null (cdr dims))
                         (or (eq (car dims) '*) (integerp (car dims)))))))
         :char)))

;;; Checking a SIMPLE-* array declaration where the variable is bound.
;;;
;;; Above (safety 0) a binding declared SIMPLE-ARRAY / SIMPLE-STRING /
;;; SIMPLE-BASE-STRING / SIMPLE-VECTOR / SIMPLE-BIT-VECTOR (the name or a
;;; compound spec headed by it) signals a TYPE-ERROR when the value is an array
;;; that is not simple: adjustable, fill-pointered or displaced. That is the one
;;; way such a declaration can be false that the declaration-driven fast paths
;;; would otherwise absorb silently, by falling back to the per-element helper.
;;;
;;; Where the element storage is hoisted, the prologue's fetch already tells a
;;; non-simple array apart (see %BACKING-FETCH-CALL), so the check costs
;;; nothing there and %CHECK-SIMPLE-DECL compiles to nothing. Everywhere else
;;; it is one inlined type test per binding.
;;;
;;; Only the non-simple case is checked. A value that is not an array at all is
;;; left to the operations that use it, as for every other declaration here.

(defparameter +simple-array-type-names+
  '("SIMPLE-ARRAY" "SIMPLE-STRING" "SIMPLE-BASE-STRING" "SIMPLE-VECTOR"
    "SIMPLE-BIT-VECTOR"))

(defun %simple-array-decl-spec-p (spec)
  "T when SPEC names a type every member of which is a simple array: one of
   +SIMPLE-ARRAY-TYPE-NAMES+ or a compound spec headed by one. Matched by
   SYMBOL-NAME, like the backing kinds, across the cross-compile package split."
  (let ((head (if (consp spec) (car spec) spec)))
    (and head
         (symbolp head)
         (member (symbol-name head) +simple-array-type-names+ :test #'string=)
         t)))

(defun simple-array-declared-vars (decl-forms names)
  "(VAR . SPEC) for each variable among NAMES (VAR-NAME strings) that the
   DECLARE forms at the head of DECL-FORMS declare with a SIMPLE-* array type,
   first declaration wins. A variable also declared IGNORE is left out, since
   checking it would read it."
  (let ((result '()) (ignored '()))
    (dolist (form decl-forms)
      (unless (and (consp form) (eq (car form) 'declare)) (return))
      (dolist (decl (cdr form))
        (when (consp decl)
          (cond
            ((and (symbolp (car decl))
                  (member (symbol-name (car decl)) '("IGNORE" "IGNORABLE")
                          :test #'string=))
             (when (string= (symbol-name (car decl)) "IGNORE")
               (dolist (v (cdr decl))
                 (when (symbolp v) (push (var-name v) ignored)))))
            (t
             (let* ((typed (and (symbolp (car decl))
                                (string= (symbol-name (car decl)) "TYPE")))
                    (spec (if typed (cadr decl) (car decl)))
                    (vars (if typed (cddr decl) (cdr decl))))
               (when (%simple-array-decl-spec-p spec)
                 (dolist (v vars)
                   (when (and (symbolp v) v
                              (member (var-name v) names :test #'string=)
                              (not (assoc (var-name v) result :test #'string=)))
                     (push (list (var-name v) v spec) result))))))))))
    (loop for (name v spec) in (nreverse result)
          unless (member name ignored :test #'string=)
            collect (cons v spec))))

(defun simple-decl-check-forms (decl-forms names)
  "The (%CHECK-SIMPLE-DECL VAR 'SPEC) statements for SIMPLE-ARRAY-DECLARED-VARS."
  (loop for (v . spec) in (simple-array-declared-vars decl-forms names)
        collect `(%check-simple-decl ,v (quote ,spec))))

(defun arg-type-declared-vars (decl-forms names)
  "(VAR . SPEC) for each type declaration the leading DECLARE forms of
   DECL-FORMS make on a variable among NAMES, IGNOREd variables left out."
  (let ((result '()) (ignored '()))
    (dolist (form decl-forms)
      (unless (or (stringp form) (and (consp form) (eq (car form) 'declare))) (return))
      (dolist (decl (and (consp form) (cdr form)))
        (when (consp decl)
          (let ((head (car decl)))
            (cond
              ((and (symbolp head)
                    (member (symbol-name head) '("IGNORE" "IGNORABLE") :test #'string=))
               (when (string= (symbol-name head) "IGNORE")
                 (dolist (v (cdr decl))
                   (when (symbolp v) (push (var-name v) ignored)))))
              (t
               (multiple-value-bind (spec vars)
                   (cond ((and (symbolp head) (string= (symbol-name head) "TYPE"))
                          (values (cadr decl) (cddr decl)))
                         ((and (symbolp head)
                               (not (member (symbol-name head)
                                            '("OPTIMIZE" "SPECIAL" "DYNAMIC-EXTENT" "INLINE"
                                              "NOTINLINE" "DECLARATION" "FTYPE" "VALUES")
                                            :test #'string=))
                               (%declared-type-known-p head))
                          (values head (cdr decl)))
                         ((and (consp head) (symbolp (car head)))
                          (values head (cdr decl)))
                         (t (values nil nil)))
                 (when (and spec (not (eq spec t)))
                   (dolist (v vars)
                     (when (and (symbolp v) v
                                (member (var-name v) names :test #'string=))
                       (push (cons v spec) result)))))))))))
    (remove-if (lambda (e) (member (var-name (car e)) ignored :test #'string=))
               (nreverse result))))

(defun %arg-check-spec (spec)
  "SPEC as checked on entry: a function type is checked as FUNCTION, as SBCL
   does; T when nothing is checked."
  (let ((e (%typexpand-for-range spec)))
    (flet ((head-p (name) (and (consp e) (symbolp (car e)) (string= (symbol-name (car e)) name))))
      (cond ((head-p "FUNCTION") 'function)
            ((head-p "VALUES") t)
            ((or (head-p "AND") (head-p "OR"))
             (let ((parts (mapcar #'%arg-check-spec (cdr e))))
               (if (every #'eq parts (cdr e)) spec (cons (car e) parts))))
            ((head-p "NOT")
             (if (eq (%arg-check-spec (second e)) (second e)) spec t))
            (t spec)))))

(defun arg-decl-checks (decl-forms names)
  "(VAR . SPEC) for the declared parameter types checked on entry."
  (unless *cross-compiling*
    (loop for (v . declared) in (arg-type-declared-vars decl-forms names)
          for spec = (%arg-check-spec declared)
          unless (eq spec t)
            collect (cons v spec))))

(defun arg-decl-check-forms (decl-forms names)
  (loop for (v . spec) in (arg-decl-checks decl-forms names)
        collect `(%check-arg-decl ,v (quote ,spec))))

(defun insert-simple-decl-checks (body names)
  "BODY with its SIMPLE-* declaration checks inserted right after its leading
   DECLARE forms, for the variables among NAMES (symbols or VAR-NAME strings)
   the form being compiled binds. BODY itself, EQ, when there is none."
  (let* ((names (mapcar (lambda (n) (if (stringp n) n (var-name n))) names))
         (first-form (find-if-not (lambda (f) (and (consp f) (eq (car f) 'declare))) body))
         (checks (and names
                      (not (and (consp first-form)
                                (member (car first-form) '(%check-simple-decl %check-arg-decl))))
                      (append (simple-decl-check-forms body names)
                                    (arg-decl-check-forms body names)))))
    (if (null checks)
        body
        (let ((decls '()) (rest body))
          (loop while (and rest (consp (car rest)) (eq (caar rest) 'declare))
                do (push (pop rest) decls))
          (append (nreverse decls) checks rest)))))

(defun %hoisted-backing-entry-p (var)
  "T when VAR's element storage is hoisted in the current scope (any kind), so
   the prologue's fetch has already checked its declaration."
  (and (symbolp var)
       var
       (boundp '*cstate*)
       (let ((e (assoc (var-name var) (cstate-array-backing-locals)
                       :test #'string=)))
         (and e
              (eq (lookup-local var) (second e))
              (not (boxed-var-p var))
              t))))

(defun string-backing-entry (var)
  "VAR's hoisted char[] entry, or NIL. Keyed and re-checked the way
   ARRAY-BACKING-ENTRY is, so a shadow or a closure re-key self-invalidates."
  (and (symbolp var)
       var
       (boundp '*cstate*)
       (let ((e (assoc (var-name var) (cstate-array-backing-locals)
                       :test #'string=)))
         (and e
              (eq (fourth e) :char)
              (eq (lookup-local var) (second e))
              (not (boxed-var-p var))
              e))))

(defun array-backing-entry (expr)
  "For (AREF V IDX...) on a local with a hoisted element buffer and as many
   subscripts as its rank, its CSTATE-ARRAY-BACKING-LOCALS entry; else NIL.
   Re-checks the binding key and the boxed flag exactly as
   NUMERIC-ARRAY-AREF-ENTRY does, so a shadow or a closure re-key
   self-invalidates. A constant subscript outside its declared dimension
   answers NIL: the helper is what signals for it."
  (and (consp expr)
       (eq (car expr) 'aref)
       (not (local-function-entry 'aref))
       (consp (cdr expr))
       (symbolp (cadr expr))
       (boundp '*cstate*)
       (let ((e (assoc (var-name (cadr expr)) (cstate-array-backing-locals)
                       :test #'string=)))
         (and e
              (not (eq (fourth e) :char))
              (= (length (cddr expr)) (max 1 (length (backing-entry-dims e))))
              (eq (lookup-local (cadr expr)) (second e))
              (not (boxed-var-p (cadr expr)))
              (loop for idx in (cddr expr)
                    for d in (backing-entry-dims e)
                    for c = (%const-index-value idx)
                    never (and c (or (minusp c) (and (integerp d) (>= c d)))))
              e))))

(defun backing-entry-dims (entry)
  "Rank 2 or 3: the dimensions, each an integer or the Int64 local a declared *
   is read into. NIL for rank 1."
  (seventh entry))

(defun %const-index-value (expr)
  "%LITERAL-INDEX-VALUE, extended through + - * 1+ 1-."
  (or (%literal-index-value expr)
      (and (consp expr)
           (member (car expr) '(+ - * 1+ 1-))
           (consp (cdr expr))
           (not (local-function-entry (car expr)))
           (not (macrolet-shadowed-p (car expr)))
           (let ((args (mapcar #'%const-index-value (cdr expr))))
             (and (every #'integerp args)
                  (apply (car expr) args))))))

(defun struct-backing-slot (entry)
  "The Int64[] slot holding ENTRY's hoisted raw slot array."
  (third entry))

(defun struct-backing-type (entry)
  "The structure name ENTRY's binding was declared to hold. The fetch compares
   it against the instance's own, because a position is read out of the
   DECLARED structure's layout and another structure maps the same slot index
   elsewhere."
  (fourth entry))

(defun struct-backing-version (entry)
  "The structure layout version ENTRY's binding was compiled against. The fetch
   compares it against the instance's, so a caller holding positions from an
   older definition gets null and falls back rather than reading the wrong
   slot -- the same thing the packed per-access constant does, asked once."
  (fifth entry))

(defun struct-backing-entry (obj packed)
  "For a raw slot read or write at the constant PACKED index on the local
   variable OBJ, (BACKING-ENTRY RAW-POSITION . KIND) when OBJ's raw slot array
   was hoisted and the slot PACKED names is a raw one; else NIL.

   Re-checks the binding key and the boxed flag exactly as ARRAY-BACKING-ENTRY
   does, so a shadowing binding or a closure re-key self-invalidates.

   PACKED carries the layout version the ACCESS was compiled against, and it
   must be the version the BINDING was compiled against, or the position taken
   from the binding's layout would describe a different definition. In one
   image they always agree; the test is what keeps that from being an
   assumption. When they disagree, or the slot is boxed, the access compiles
   the way it does today, which is also what reports a stale layout."
  (and (symbolp obj)
       obj
       (integerp packed)
       (boundp '*cstate*)
       (let ((e (assoc (var-name obj) (cstate-struct-backing-locals)
                       :test #'string=)))
         (and e
              (eq (lookup-local obj) (second e))
              (not (boxed-var-p obj))
              (= (ash packed (- +slot-version-shift+)) (struct-backing-version e))
              (let ((cell (nth (logand packed (1- (ash 1 +slot-version-shift+)))
                               (cdr (cddddr e)))))
                (and cell (>= (car cell) 0) (cons e cell)))))))

(defun %array-type-numeric-info (type)
  "For an array type specifier (simple-array ELT DIMS) / (array ELT DIMS) /
   (vector ELT ...) whose ELT gets an unboxed backing and whose rank is
   statically known to be 1-3, return the *numeric-array-locals* info tail:
   (RANK . :single/:double) for float backing, (RANK LO . HI) for integer
   backing; else NIL. DIMS must be a list of integers/* (its length is the
   rank); an unspecified rank (* or missing) is rejected.

   This is what lets an aref on an array-typed PARAMETER ride the unboxed path
   without a make-array in sight: (simple-array single-float (1025)) reads raw
   r8, (simple-array fixnum (*)) reads raw int64 (Runtime.ArefNum*L). The
   runtime fast paths re-check _numKind and fall back to the boxed path when
   the array is not actually backed that way, so a wrong declaration loses the
   speed rather than the data."
  (setf type (%expand-array-type type))
  (and (consp type)
       (let* ((head (car type))
              (elt (cadr type))
              (info (or (%float-storage-kind elt) (%numeric-storage-range elt)
                        (and (%u64-storage-p elt) :u64))))
         (and info
              (cond
                ((member head '(simple-array array))
                 (let ((dims (and (cddr type) (caddr type))))
                   (and (consp dims)
                        (every (lambda (d) (or (eq d '*) (integerp d))) dims)
                        (<= 1 (length dims) 3)
                        (cons (length dims) info))))
                ((eq head 'vector) (cons 1 info))
                (t nil))))))

(defun %make-array-static-rank (dims)
  "Statically-known rank of a make-array DIMS argument form, or NIL.
   A bare fixnum-typed variable is rank 1 (an integer dimension), a (list ...)
   or quoted literal list gives its length; anything else is unknown (the
   variable could hold a dimension LIST at runtime)."
  (cond ((integerp dims) 1)
        ((and (symbolp dims) (fixnum-typed-p dims)) 1)
        ((and (consp dims) (eq (car dims) 'list)) (length (cdr dims)))
        ((and (consp dims) (eq (car dims) 'quote))
         (let ((d (cadr dims)))
           (cond ((integerp d) 1)
                 ((and (consp d) (every #'integerp d)) (length d))
                 (t nil))))
        (t nil)))

(defun %make-array-numeric-info (init)
  "If INIT is a (make-array DIMS ...) call with static rank 1-3, a constant
   quoted :element-type that upgrades to a numeric backing, and no
   :displaced-to, return (RANK LO . HI); else NIL."
  (and (consp init)
       (eq (car init) 'make-array)
       (not (local-function-entry 'make-array))
       (consp (cdr init))
       (let ((rank (%make-array-static-rank (cadr init)))
             (spec nil)
             (ok t))
         (let ((tail (cddr init)))
           (loop while (and ok tail)
                 do (let ((k (car tail)))
                      (cond
                        ((not (and (symbolp k) (consp (cdr tail))))
                         (setf ok nil))
                        ((string= (symbol-name k) "DISPLACED-TO")
                         (setf ok nil))
                        ((string= (symbol-name k) "ELEMENT-TYPE")
                         (let ((f (cadr tail)))
                           (if (and (consp f) (eq (car f) 'quote))
                               (setf spec (cadr f))
                               (setf ok nil)))))
                      (setf tail (cddr tail)))))
         (and ok rank (<= 1 rank 3) spec
              (let ((fk (or (%float-storage-kind spec)
                            (and (%u64-storage-p spec) :u64))))
                (if fk
                    ;; Float backing: info tail is (RANK . :single/:double): an
                    ;; atom cdr distinguishes it from the integer (RANK LO . HI).
                    (cons rank fk)
                    (let ((r (%numeric-storage-range spec)))
                      (and r (cons rank r)))))))))

(defun infer-numeric-array-bindings (binding-info mutated outer)
  "Numeric-array environment for a let/let* body: start from OUTER, drop
   every name this let binds (shadowing), then add (NAME KEY RANK LO . HI)
   (or (NAME KEY RANK . :single/:double) for float backing) for each plain
   lexical, non-mutated binding whose init is a recognizable numeric make-array,
   OR whose init is a bare reference to an array local already proven numeric
   (copy propagation, e.g. the Gabriel fft's (prog ((ar areal)) ...) that
   aliases a (simple-array single-float (1025)) parameter). KEY pins the
   binding: a consumer only trusts the entry while (lookup-local NAME) still
   resolves to it (a closure body re-keys its captured vars, so stale entries
   self-invalidate). Mutated bindings are excluded; a setq could install an
   array with different backing; a mutated SOURCE alias is likewise not
   propagated."
  (let* ((bound-names (mapcar (lambda (b) (var-name (first b))) binding-info))
         (result (remove-if (lambda (e) (member (car e) bound-names :test #'string=))
                            outer)))
    (dolist (b binding-info)
      (let ((var (first b)) (init (second b)) (is-special (third b)) (key (fourth b)))
        (when (and (not is-special)
                   init
                   (not (member (var-name var) mutated :test #'string=)))
          (let ((info (or (%make-array-numeric-info init)
                          ;; copy propagation: init is a plain var already known
                          ;; numeric-backed and itself not mutated -> same backing.
                          (and (symbolp init)
                               (not (member (var-name init) mutated :test #'string=))
                               (let ((src (assoc (var-name init) result :test #'string=)))
                                 (and src (cddr src)))))))
            (when info
              (push (list* (var-name var) key info) result))))))
    result))

(defun struct-accessor-array-info (expr)
  "For (ACCESSOR OBJ) where ACCESSOR reads a structure slot declared to hold an
   array whose element type gets an unboxed backing, the *numeric-array-locals*
   info tail of that declaration -- (RANK LO . HI) or (RANK . :single/:double);
   else NIL.

   A slot read has no binding to hang a declaration on, so the slot's own :TYPE
   is the only evidence there is. It is the same evidence FIXNUM-TYPED-P already
   acts on for a slot declared to hold an integer, and it buys the same thing
   here: without it, an array reached through its accessor rather than through a
   declared local went down the generic per-element path even though the
   declaration said exactly which backing it has.

   Only the element read is decided here. The buffer is NOT hoisted (that is
   ARRAY-BACKING-ENTRY, which requires a plain local), so a slot whose contents
   contradict the declaration still falls back to the boxed path in the runtime
   rather than being rejected -- the declaration stays a hint, as it is for a
   parameter."
  (and (consp expr)
       (= (length expr) 2)
       (symbolp (car expr))
       (not (local-function-entry (car expr)))
       (boundp '*struct-accessor-types*)
       (let ((ty (gethash (car expr) *struct-accessor-types*)))
         (and ty (%array-type-numeric-info ty)))))

(defun numeric-array-aref-entry (expr)
  "If EXPR is (aref V IDX...) on a proven numeric-backed local with matching
   rank, return the info tail: (RANK LO . HI) for integer backing, or
   (RANK . :single/:double) for float backing; else NIL.
   numeric-array-aref-info / -float-kind split this by backing.

   V may also be a structure accessor call whose slot is declared to hold such
   an array (STRUCT-ACCESSOR-ARRAY-INFO), which is what puts
   (AREF (ACC X) I) on the same element path as (AREF A I) on a declared local.

   The subscripts are not required to be statically fixnum-typed: what this
   answers is how the ELEMENT is stored, which does not depend on them
   (COMPILE-INDEX-TO-LONG lowers a subscript of any shape). Requiring it cost
   the whole unboxed path whenever a loop counter carried no declaration;
   fft's inner loops are (DO ((I J (+ I LE))) ...), and every element they
   touched was boxed.

   (ROW-MAJOR-AREF V I) is answered too, but only when V is rank 1: there the
   row-major index IS the subscript (CLHS ROW-MAJOR-AREF), so the form reads
   exactly the element (AREF V I) does and every consumer of this answer, which
   takes the array and subscripts from (CADR EXPR) / (CDDR EXPR), compiles it
   unchanged. It is admitted with exactly one subscript, so the rank check
   below accepts only a rank-1 array. A higher rank would need a flat-index
   entry the runtime helpers do not have, so it stays on the generic call.

   A declaration the contents contradict fails as it does for AREF: the
   helper's fallback subscripts the actual array with one index, which signals
   for an array of another rank rather than reading a wrong element."
  (and (consp expr)
       (or (and (eq (car expr) 'aref)
                (not (local-function-entry 'aref)))
           (and (eq (car expr) 'row-major-aref)
                (not (local-function-entry 'row-major-aref))
                (consp (cdr expr))
                (consp (cddr expr))
                (null (cdddr expr))))
       (consp (cdr expr))
       (boundp '*cstate*)
       (let ((v (cadr expr))
             (idxs (cddr expr)))
         (if (symbolp v)
             (let ((entry (assoc (var-name v) (cstate-numeric-array-locals)
                                 :test #'string=)))
               (if entry
                   (and (eq (lookup-local v) (second entry))
                        (not (boxed-var-p v))
                        (= (length idxs) (third entry))
                        (cddr entry))
                   (let ((info (constant-array-numeric-info v)))
                     (and info (= (length idxs) (car info)) info))))
             (let ((info (struct-accessor-array-info v)))
               (and info
                    (= (length idxs) (car info))
                    info))))))

(defun constant-array-numeric-info (sym)
  "For SYM a constant variable whose value is a simple array with an unboxed
   backing, the *numeric-array-locals* info tail of its actual type."
  (and (%global-constant-symbol-p sym)
       (not (lookup-local sym))
       (boundp sym)
       (let ((val (symbol-value sym)))
         (and (typep val 'simple-array)
              (not (stringp val))
              (%array-type-numeric-info
               `(simple-array ,(array-element-type val) ,(array-dimensions val)))))))

(defun numeric-array-aref-info (expr)
  "Integer-backed aref only: (RANK LO . HI), else NIL. The (cdr info) is a
   (LO . HI) cons for integer backing but a keyword for float backing, so
   gating on a cons cdr excludes float arrays (which must not read raw longs)."
  (let ((info (numeric-array-aref-entry expr)))
    (and info (consp (cdr info)) info)))

(defun numeric-array-aref-float-kind (expr)
  "Float-backed aref only: :single / :double storage kind, else NIL."
  (let ((info (numeric-array-aref-entry expr)))
    (and info (member (cdr info) '(:single :double)) (cdr info))))

(defun numeric-array-aref-u64-p (expr)
  "T when EXPR is (AREF V IDX...) on an array known to hold (unsigned-byte
   64), whose elements are kept as their bits (COMPILE-U64-AREF-BITS)."
  (let ((info (numeric-array-aref-entry expr)))
    (and info (eq (cdr info) :u64))))

;; DOTCL:TYPEXPAND, looked up by name at load time: the SBCL cross-compile host
;; has no DOTCL package (same shape as *KNOWN-TYPE-NAME-P-FN*).
(defvar *typexpand-fn*
  (ignore-errors
   (let ((sym (find-symbol "TYPEXPAND" "DOTCL")))
     (and sym (fboundp sym) (symbol-function sym)))))

(defun %typexpand-for-range (type)
  "TYPE with user DEFTYPEs expanded, or TYPE itself when nothing can be
   expanded (or on the cross-compile host). Any error in an expander is
   treated as 'not expandable': this only feeds an optimization."
  (if (and *typexpand-fn* (or (symbolp type) (and (consp type) (symbolp (car type)))))
      (let ((e (ignore-errors (funcall *typexpand-fn* type))))
        (if e e type))
      type))

(defun %integer-type-bounds (type depth)
  "Bounds of the integers TYPE admits, as (LO . HI) where NIL means unbounded
   on that side; :NONE when TYPE is not known to be a subtype of INTEGER.
   AND intersects the bounds of the components known to be integer types and
   ignores the others: an AND is a subtype of each of its components, so any
   one integer component bounds it. FIXNUM is this runtime's fixnum, int64."
  (cond
    ((> depth 8) :none)
    ((eq type 'bit) (cons 0 1))
    ((eq type 'fixnum) (cons +int64-min+ +int64-max+))
    ((eq type 'unsigned-byte) (cons 0 nil))
    ((member type '(integer signed-byte)) (cons nil nil))
    ((and (consp type) (eq (car type) 'signed-byte) (integerp (cadr type)) (> (cadr type) 0))
     (let ((n (cadr type)))
       (cons (- (ash 1 (1- n))) (1- (ash 1 (1- n))))))
    ((and (consp type) (eq (car type) 'unsigned-byte) (integerp (cadr type)) (> (cadr type) 0))
     (cons 0 (1- (ash 1 (cadr type)))))
    ((and (consp type) (eq (car type) 'mod) (integerp (cadr type)) (> (cadr type) 0))
     (cons 0 (1- (cadr type))))
    ((and (consp type) (eq (car type) 'integer))
     (flet ((bound (b inc)
              (cond ((integerp b) b)
                    ((and (consp b) (integerp (car b)) (null (cdr b))) (+ (car b) inc))
                    (t nil))))
       (cons (bound (cadr type) 1)
             (and (cddr type) (bound (caddr type) -1)))))
    ((and (consp type) (eq (car type) 'and))
     (let ((lo nil) (hi nil) (known nil))
       (dolist (sub (cdr type))
         (let ((b (%integer-type-bounds sub (1+ depth))))
           (unless (eq b :none)
             (setf known t)
             (when (and (car b) (or (null lo) (> (car b) lo))) (setf lo (car b)))
             (when (and (cdr b) (or (null hi) (< (cdr b) hi))) (setf hi (cdr b))))))
       (if known (cons lo hi) :none)))
    ((or (symbolp type) (and (consp type) (symbolp (car type))
                             (not (member (car type) '(or not satisfies member eql)))))
     (let ((e (%typexpand-for-range type)))
       (if (equal e type) :none (%integer-type-bounds e (1+ depth)))))
    (t :none)))

(defun integer-type-range (type)
  "Inclusive (LO . HI) for a bounded integer TYPE specifier, or NIL if TYPE is
   not a bounded integer type. Recognizes bit, (signed-byte N), (unsigned-byte N),
   and (integer LO HI) with constant bounds. The caller gates on range-fits-int64-p,
   so e.g. (unsigned-byte 64) (whose HI exceeds int64) is rejected downstream.
   Storage decisions (raw structure slots, numeric array backing) use this one:
   they must agree with the runtime, which does not expand DEFTYPEs."
  (cond
    ((eq type 'bit) (cons 0 1))
    ((and (consp type) (eq (car type) 'signed-byte) (integerp (cadr type)))
     (let ((n (cadr type)))
       (cons (- (ash 1 (1- n))) (1- (ash 1 (1- n))))))
    ((and (consp type) (eq (car type) 'unsigned-byte) (integerp (cadr type)))
     (cons 0 (1- (ash 1 (cadr type)))))
    ((and (consp type) (eq (car type) 'integer)
          (integerp (cadr type)) (integerp (caddr type)))
     (cons (cadr type) (caddr type)))
    (t nil)))

(defun declared-integer-range (type)
  "Inclusive (LO . HI) for a TYPE declared on a variable or asserted by THE, or
   NIL. On top of INTEGER-TYPE-RANGE it understands (mod N), exclusive and
   half-open (integer ...) bounds, AND of an integer type with other types (e.g.
   (AND FIXNUM UNSIGNED-BYTE)), and DEFTYPEs expanding to these. A bare FIXNUM
   is left to the fixnum-locals path and answers NIL here. Only for values, never
   for storage: see INTEGER-TYPE-RANGE."
  (if (eq type 'fixnum)
      nil
      (let ((b (%integer-type-bounds type 0)))
        (and (consp b) (car b) (cdr b) (<= (car b) (cdr b)) b))))

(defun range-arith (op ra rb)
  "Interval arithmetic for OP over ranges RA=(lo . hi) and RB. Host bignums keep
   the bounds exact; the caller checks whether the result still fits int64."
  (let ((alo (car ra)) (ahi (cdr ra)) (blo (car rb)) (bhi (cdr rb)))
    (ecase op
      (+ (cons (+ alo blo) (+ ahi bhi)))
      (- (cons (- alo bhi) (- ahi blo)))
      (* (let ((c1 (* alo blo)) (c2 (* alo bhi)) (c3 (* ahi blo)) (c4 (* ahi bhi)))
           (cons (min c1 c2 c3 c4) (max c1 c2 c3 c4)))))))

(defun fixnum-leaf-range (expr)
  "Range (lo . hi) for a non-arithmetic fixnum-typed leaf, or NIL if EXPR is not
   such a leaf. Locals / calls / bitwise results take the full fixnum (int64) range;
   (the (integer lo hi) ...) contributes its declared bounds."
  (cond
    ;; Local with a proven bounded int64 range -> its TIGHT range (checked before
    ;; the full-range fixnum/long clauses so a tighter bound wins). This is what
    ;; lets e.g. (* rmdr 2) on a (signed-byte 56) prove int64-safety.
    ((small-int-local-range expr))
    ;; aref on a numeric-backed array local -> the element STORAGE range. Tight
    ;; enough to prove e.g. (+ (aref a i j) (aref b i j)) of two u16 arrays
    ;; stays in int64, keeping the whole store expression native.
    ((let ((info (numeric-array-aref-info expr)))
       (and info (cdr info))))
    ;; A structure slot declared to hold a bounded integer -> the DECLARED
    ;; range. FIXNUM-TYPED-P has taken this leaf since the slot storage became
    ;; raw, and COMPILE-AS-LONG has the matching STRUCT-SLOT-REF clause, but
    ;; the third of the three places that decide raw int64 did not, so
    ;; EXPR-INT-RANGE answered NIL and four downstream proofs failed together:
    ;; 1+ / 1- stayed on Runtime.Increment, FIXNUM-BINOP-NATIVE-P refused the
    ;; whole arithmetic expression, a subscript did not reach the raw element
    ;; path, and a LET bound from a slot got no Int64 slot.
    ;;
    ;; The declared type, not the full fixnum range: a slot declared
    ;; (SIGNED-BYTE 32) proves things a FIXNUM one cannot, and this is the
    ;; only place that knows which it was.
    ((and (consp expr) (= (length expr) 2) (symbolp (car expr))
          (not (local-function-entry (car expr)))
          (boundp '*struct-accessor-types*)
          (let ((ty (gethash (car expr) *struct-accessor-types*)))
            (and ty
                 (let ((r (if (eq ty 'fixnum)
                              (cons +int64-min+ +int64-max+)
                              (integer-type-range ty))))
                   (and r (range-fits-int64-p r) r))))))
    ;; (char-code (schar s i)) -> [0, 65535]. FIXNUM-TYPED-P has taken this
    ;; composition since COMPILE-AS-LONG grew the Runtime.CharCodeAtL lowering
    ;; for it, but FIXNUM-LEAF-RANGE did not, so the range was NIL and the
    ;; arithmetic around a string scan stayed generic even though the character
    ;; code itself was already arriving as a raw int64.
    ;;
    ;; The bound is entirely an argument from the type, with no step that holds
    ;; only on this machine. Every return of Runtime.CharCodeAtL is a C# char
    ;; widened to long: the two string backings, the char[] of a LispVector,
    ;; and Runtime.CharCode of a LispChar, whose Value is declared char. A
    ;; System.Char is 16 bits unsigned, so [0, 65535] holds for the same reason
    ;; the runtime type-checks, and CHAR-CODE-LIMIT is 65536 to match.
    ((string-char-code-ref expr)
     (cons 0 65535))
    ;; A word of 2 or 4 adjacent octets (OCTET-WORD-REF-INFO).
    ((let ((ow (octet-word-ref-info expr)))
       (and ow (< (fourth ow) 8) (cons 0 (1- (ash 1 (* 8 (fourth ow))))))))
    ;; (mod X N) / (rem X N) with a constant integer divisor -> the range the
    ;; divisor forces. Unlike the three leaves above, the lowering here was
    ;; ALREADY reachable: Runtime.ModFixnumL is emitted today for a mod whose
    ;; operands are fixnum-typed. What was not reachable is the arithmetic
    ;; AROUND it -- (* 3 (mod i 10)) lowered the mod natively and then called
    ;; the generic multiply, because EXPR-INT-RANGE had nothing to say about
    ;; the mod's result and so could not prove the product fits.
    ;;
    ;; Which is why this one has to be the TIGHT range and a conservative full
    ;; int64 range would be worth nothing: [int64min, int64max] times [3,3]
    ;; does not fit, so the enclosing proof fails exactly as it did before.
    ;;
    ;; The bounds are CLHS 12.1.3.1's sign rules and not a guess about
    ;; magnitude. MOD takes the sign of the DIVISOR, so a positive N gives
    ;; [0, N-1] and a negative one [N+1, 0]. REM takes the sign of the NUMBER,
    ;; so either way it is [-(|N|-1), |N|-1]. A zero divisor is excluded
    ;; because it has no result to bound, and the dividend must be
    ;; fixnum-typed: (mod 5.5 2) is 1.5, and a float result has no integer
    ;; range at all.
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(mod rem))
          (not (local-function-entry (car expr)))
          (integerp (caddr expr))
          (not (zerop (caddr expr)))
          (fixnum-typed-p (cadr expr))
          (let* ((d (caddr expr))
                 (m (1- (abs d)))
                 (r (if (eq (car expr) 'mod)
                        (if (plusp d) (cons 0 m) (cons (- m) 0))
                        (cons (- m) m))))
            (and (range-fits-int64-p r) r))))
    ;; (length x) -> [0, 2^31-1]. The other half of the same omission the slot
    ;; clause above fixes: FIXNUM-TYPED-P has long taken LENGTH as an integer
    ;; leaf, but FIXNUM-LEAF-RANGE never did, so EXPR-INT-RANGE answered NIL
    ;; for it. COMPILE-AS-LONG has no LENGTH clause at all -- the generic
    ;; call is correct and stays -- so everything this buys is downstream of
    ;; the range: 1+ / 1- go native, FIXNUM-BINOP-NATIVE-P admits the whole
    ;; expression, and a LET bound from a length earns an Int64 slot.
    ;;
    ;; Two arguments hold the bound up and they are not the same kind.
    ;;
    ;; From the type: all four returns of Runtime.Length are Fixnum.Make of a
    ;; C# int -- 0 for NIL, the counter for a list, s.Length, v.Length -- and
    ;; every one is non-negative. That is what gives [0, int32max], and it
    ;; holds for the same reason the code type-checks.
    ;;
    ;; From the machine: the list arm counts into an int, so a list of more
    ;; than 2^31 conses would overflow it. No such list exists -- 2^31 conses
    ;; is 32 GB and the counting loop would not finish -- but that is an
    ;; argument about this machine, not about the type. Anyone widening this
    ;; bound, or reusing it where the count does not come from memory the
    ;; process holds, is leaning on the second argument and should know it.
    ((and (consp expr) (= (length expr) 2)
          (eq (car expr) 'length)
          (not (local-function-entry 'length)))
     (cons 0 (1- (ash 1 31))))
    ;; Raw int64 local (native body) or declared-fixnum local -> full int64 range.
    ((and (symbolp expr)
          (boundp '*cstate*)
          (native-slot-p expr (cstate-long-locals)))
     (cons +int64-min+ +int64-max+))
    ((and (symbolp expr)
          (boundp '*fixnum-locals*)
          (member (var-name expr) *fixnum-locals* :test #'string=)
          (not (boxed-var-p expr))
          (lookup-local expr))
     (cons +int64-min+ +int64-max+))
    ;; (the (integer lo hi) E) / (the (signed-byte N) E) etc. with constant
    ;; bounds -> declared bounds (only when they fit int64); otherwise
    ;; (the fixnum E) / (the (integer lo *) E) -> full fixnum range.
    ((and (consp expr) (eq (car expr) 'the))
     (let ((ty (cadr expr)))
       (cond
         ((let ((r (declared-integer-range ty))) (and r (range-fits-int64-p r) r)))
         ((or (eq ty 'fixnum) (and (consp ty) (eq (car ty) 'integer)))
          (cons +int64-min+ +int64-max+))
         (t nil))))
    ;; %dotimes-1+ asserts (from loop structure: counter < limit <= int64-max
    ;; at the increment site) that the incremented result fits int64, so it is
    ;; a leaf with the full int64 range: no +1 widening that would defeat the
    ;; range proof for a plain (1+ counter) on a full-range fixnum counter.
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '%dotimes-1+)
          (fixnum-typed-p (cadr expr)))
     (cons +int64-min+ +int64-max+))
    ;; %dotimes-limit / %loop-limit-floor: a loop limit clamped into int64.
    ((and (consp expr) (= (length expr) 2)
          (member (car expr) '(%dotimes-limit %loop-limit-floor)))
     (cons +int64-min+ +int64-max+))
    ;; (logand X M) with M non-negative: [0, max M], and exact even when X is
    ;; only right modulo 2^64 (see FIXNUM-TYPED-P).
    ((and (consp expr) (= (length expr) 3) (eq (car expr) 'logand)
          (let ((m (%logand-nonneg-operand (cadr expr) (caddr expr))))
            (and m
                 (fixnum-wrap-typed-p (if (eq m (cadr expr)) (caddr expr) (cadr expr)))
                 (cons 0 (cdr (expr-int-range m)))))))
    ;; (logior A B) / (logxor A B) with both operands proved non-negative: the
    ;; result has no bit above the higher operand's top bit, so it is below the
    ;; next power of two. Two octets put together into a word stay a word.
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(logior logxor))
          (not (local-function-entry (car expr)))
          (let ((ra (expr-int-range (cadr expr))))
            (and ra (>= (car ra) 0)
                 (let ((rb (expr-int-range (caddr expr))))
                   (and rb (>= (car rb) 0)
                        (cons 0 (1- (ash 1 (integer-length (max (cdr ra) (cdr rb))))))))))))
    ;; Bitwise results stay within int64 (conservatively the full range).
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(logand logior logxor))
          (fixnum-typed-p (cadr expr)) (fixnum-typed-p (caddr expr)))
     (cons +int64-min+ +int64-max+))
    ((and (consp expr) (= (length expr) 2) (eq (car expr) 'lognot)
          (fixnum-typed-p (cadr expr)))
     (cons +int64-min+ +int64-max+))
    ;; ash with a constant shift is admitted by fixnum-typed-p only when it stays
    ;; in int64; mirror that here as the full range.
    ((and (consp expr) (= (length expr) 3) (eq (car expr) 'ash)
          (fixnum-typed-p expr))
     (cons +int64-min+ +int64-max+))
    ;; A call to a function whose return type is declared (or inferred) as an
    ;; integer type within int64 -> that type's range: FIXNUM gives the full
    ;; int64 range, (UNSIGNED-BYTE 8) gives [0, 255].
    ((call-int-return-range expr))
    (t nil)))

(defun fold-nary-arith (expr)
  "(+ a b c) as (+ (+ a b) c), and the same for -, * and the associative LOGAND,
   LOGIOR and LOGXOR; anything else unchanged.
   The three places that decide whether an integer expression can be computed in
   raw int64, FIXNUM-TYPED-P, EXPR-INT-RANGE and COMPILE-AS-LONG, each match a
   two-argument call, so a three-argument one fell off the native path entirely.
   The cost was not the arithmetic but what it disqualified: an index like
   (- n i 1) made the whole AREF take the generic path, so reading from a
   specialized array boxed its element (24 B a read, e.g. every element copy in
   the fft benchmark). Left-associating is what the n-ary call means (CLHS 12.2),
   and evaluation stays left to right."
  (if (and (consp expr) (member (car expr) '(+ - * logand logior logxor)) (> (length expr) 3))
      (let ((op (car expr)))
        (reduce (lambda (a b) (list op a b)) (cdr expr)))
      (fold-typed-operand expr)))

(defun foldable-constant-symbol-p (expr)
  "True when EXPR is a symbol naming a constant variable whose value is a number
   or a character, so a reference to it may be compiled as that literal.
   CLHS 3.2.2.3 lets a reference to a constant variable be replaced by its
   value, and DEFCONSTANT requires the value to be available at compile time
   and to stay the same (a non-EQL redefinition has undefined consequences).
   Only numbers and characters are folded: their identity is EQL, so a fresh
   literal cannot be told apart from the value the symbol holds. A list or
   string constant keeps its single object and stays a variable read.
   Nothing is folded while cross-compiling: there the values come from the
   host Lisp, whose constants are not the target's (MOST-POSITIVE-FIXNUM,
   CHAR-CODE-LIMIT and friends differ between SBCL and dotcl)."
  (and (symbolp expr)
       expr
       (not (eq expr t))
       (not (keywordp expr))
       (not *cross-compiling*)
       (constantp expr)
       (boundp expr)
       (not (lookup-local expr))
       (not (nth-value 1 (lookup-symbol-macro expr)))
       (typep (symbol-value expr) '(or number character))))

(defun %ltv-constant-symbol-p (expr)
  "True when EXPR, the form of a LOAD-TIME-VALUE, is a symbol naming a constant
   variable that is defined here, so its value is the one load time would see."
  (and (symbolp expr)
       expr
       (not (eq expr t))
       (not (keywordp expr))
       (not *cross-compiling*)
       (constantp expr)
       (boundp expr)
       (not (lookup-local expr))
       (not (nth-value 1 (lookup-symbol-macro expr)))))

(defun %constant-array-symbol-p (expr)
  "True when EXPR is a symbol naming a constant variable whose value, known at
   compile time, is an array. Same conditions as FOLDABLE-CONSTANT-SYMBOL-P but
   for the value's type."
  (and (symbolp expr)
       expr
       (not (eq expr t))
       (not (keywordp expr))
       (not *cross-compiling*)
       (constantp expr)
       (boundp expr)
       (not (lookup-local expr))
       (not (nth-value 1 (lookup-symbol-macro expr)))
       (arrayp (symbol-value expr))))

(defun %literal-index-value (expr)
  "The integer EXPR stands for when it is an integer literal, a foldable
   constant, or a symbol macro expanding to an integer (the variable of an
   unrolled loop, ironclad's DOTIMES-UNROLLED); else NIL."
  (let ((expr (if (foldable-constant-symbol-p expr) (symbol-value expr) expr)))
    (cond ((integerp expr) expr)
          ((and (symbolp expr) expr (not (lookup-local expr)))
           (multiple-value-bind (expansion found) (lookup-symbol-macro expr)
             (and found (integerp expansion) expansion)))
          (t nil))))

(defun %fold-constant-aref (expr)
  "The element (AREF C I...) reads, when C names a constant array
   (%CONSTANT-ARRAY-SYMBOL-P), every I is a literal index in bounds and the
   element is a number or a character; else NIL. CLHS 3.2.2.3 lets the reference
   to C be replaced by C's value, which is then a literal object that may not be
   modified (CLHS 3.7.1), so its element is the same every time. SBCL folds the
   same reads. ironclad indexes its constant tables this way inside unrolled
   rounds (BLAKE2's (AREF +BLAKE2-SIGMA+ I 0)), each read a dynamic lookup of
   the table and a generic AREF."
  (when (and (consp expr) (eq (car expr) 'aref) (consp (cdr expr))
             (not (local-function-entry 'aref))
             (not (macrolet-shadowed-p 'aref))
             (%constant-array-symbol-p (cadr expr)))
    (let ((array (symbol-value (cadr expr)))
          (indices (mapcar #'%literal-index-value (cddr expr))))
      (when (and (every #'integerp indices)
                 (= (length indices) (array-rank array))
                 (apply #'array-in-bounds-p array indices))
        (let ((v (apply #'aref array indices)))
          (and (typep v '(or number character)) v))))))

(defparameter *foldable-rational-functions*
  '(+ - * / 1+ 1- expt ash
    logand logior logxor lognot logeqv lognand lognor
    logandc1 logandc2 logorc1 logorc2 logcount integer-length
    mod rem floor ceiling truncate round
    abs min max gcd lcm isqrt signum numerator denominator))

(defparameter +fold-max-bits+ 4096)

(defun %fold-small-rational-p (x)
  (and (rationalp x)
       (<= (max (integer-length (numerator x)) (integer-length (denominator x)))
           +fold-max-bits+)))

(defun %fold-literal-arg (form)
  "The rational FORM stands for, and T; or NIL and NIL."
  (let ((v (cond ((rationalp form) form)
                 ((and (consp form) (eq (car form) 'quote) (consp (cdr form))
                       (null (cddr form)))
                  (cadr form))
                 ((foldable-constant-symbol-p form) (symbol-value form))
                 ((consp form)
                  (let ((r (%fold-rational-call form)))
                    (if (and (consp r) (eq (car r) 'values)) (cadr r) r)))
                 (t nil))))
    (if (%fold-small-rational-p v) (values v t) (values nil nil))))

(defun %fold-too-big-p (op args)
  (case op
    (expt (let ((base (first args)) (power (second args)))
            (and (integerp power)
                 (> (* (max (integer-length (numerator base))
                            (integer-length (denominator base)))
                       (abs power))
                    +fold-max-bits+))))
    (ash (> (second args) +fold-max-bits+))
    (t nil)))

(defvar *fold-call-memo* nil
  "A cons whose car is the table (call form -> ((env . result) ...)) of
   %FOLD-RATIONAL-CALL within one top-level compile, made on first use; NIL
   outside one.")

(defun %fold-arg-shape-p (a)
  (or (rationalp a)
      (and (consp a)
           (or (eq (car a) 'quote)
               (and (symbolp (car a))
                    (member (car a) *foldable-rational-functions*))))
      (foldable-constant-symbol-p a)))

(defun %fold-rational-call (expr)
  "EXPR as a literal when it calls a pure CL arithmetic function on rational
   literals (or forms that fold) and returns rationals without error; else NIL.
   A call returning more than one value folds to a VALUES form."
  (when (and (consp expr) (symbolp (car expr))
             (member (car expr) *foldable-rational-functions*)
             (listp (cdr expr))
             (every #'%fold-arg-shape-p (cdr expr)))
    (if *fold-call-memo*
        (let* ((env (list (cstate-local-functions) *macrolet-shadowed*
                          *notinline-functions*))
               (table (or (car *fold-call-memo*)
                          (setf (car *fold-call-memo*) (make-hash-table :test 'eq))))
               (hit (assoc env (gethash expr table) :test #'%fold-env=)))
          (if hit
              (cdr hit)
              (let ((v (%fold-rational-call-1 expr)))
                (push (cons env v) (gethash expr table))
                v)))
        (%fold-rational-call-1 expr))))

(defun %fold-env= (a b) (every #'eq a b))

(defun %fold-rational-call-1 (expr)
  (when (and (not (local-function-entry (car expr)))
             (not (macrolet-shadowed-p (car expr)))
             (not (member (car expr) *notinline-functions*))
             (not (%global-notinline-p (car expr))))
    (let ((args '()))
      (dolist (a (cdr expr))
        (multiple-value-bind (v ok) (%fold-literal-arg a)
          (unless ok (return-from %fold-rational-call-1 nil))
          (push v args)))
      (setf args (nreverse args))
      (when (and (member (car expr) '(expt ash)) (= (length args) 2)
                 (or (not (integerp (second args)))
                     (%fold-too-big-p (car expr) args)))
        (return-from %fold-rational-call-1 nil))
      (let ((vals (handler-case (multiple-value-list (apply (car expr) args))
                    (error () nil))))
        (when (and vals (every #'%fold-small-rational-p vals))
          (if (cdr vals) `(values ,@vals) (car vals)))))))

(defun fold-constant-symbol (expr)
  "EXPR with a reference to a foldable constant (FOLDABLE-CONSTANT-SYMBOL-P)
   replaced by its value, and a read of a constant array at literal indices
   (%FOLD-CONSTANT-AREF) by the element, and a pure arithmetic call on rational
   literals (%FOLD-RATIONAL-CALL) by its value; anything else unchanged. Applied where
   the typed paths look at a form before compiling it, so a constant is seen as
   the literal it stands for: (setf (aref a i) +k+) then takes the same raw store
   as (setf (aref a i) 1) instead of reading the symbol through the dynamic
   binding lookup and falling to the generic store."
  (cond ((foldable-constant-symbol-p expr) (symbol-value expr))
        ((%fold-constant-aref expr))
        ((%fold-rational-call expr))
        (t expr)))

(defun %literal-byte-spec (spec)
  "(SIZE . POS) when SPEC is (BYTE SIZE POS) whose SIZE and POS are integers in
   [0, 1024] once constants are folded ((byte 37 (- 64 37))); else NIL."
  (and (consp spec) (eq (car spec) 'byte)
       (consp (cdr spec)) (consp (cddr spec)) (null (cdddr spec))
       (let ((size (fold-constant-symbol (cadr spec)))
             (pos (fold-constant-symbol (caddr spec))))
         (and (typep size '(integer 0 1024))
              (typep pos '(integer 0 1024))
              (cons size pos)))))

(defun %literal-dpb-form-p (expr)
  "True when EXPR is (DPB V (BYTE SIZE POS) X) with literal SIZE and POS, the
   shape the DPB compiler entry open-codes as masks, a shift and a LOGIOR."
  (and (consp expr) (eq (car expr) 'dpb)
       (consp (cdr expr)) (consp (cddr expr)) (consp (cdddr expr))
       (null (cddddr expr))
       (%literal-byte-spec (caddr expr))
       (not (local-function-entry 'dpb))
       (not (local-function-entry 'byte))
       (not (macrolet-shadowed-p 'dpb))
       (not (macrolet-shadowed-p 'byte))))

(defun %literal-dpb-as-logior (expr)
  "(DPB V (BYTE SIZE POS) X) as the LOGIOR form it means, the same form the DPB
   compiler entry compiles. The typed paths see the masks through
   FOLD-TYPED-OPERAND: a byte deposited into a known integer is a known
   integer, which is how ironclad's UB16REF/LE builds a word out of two octets."
  (let* ((spec (%literal-byte-spec (caddr expr)))
         (size (car spec)) (pos (cdr spec))
         (mask (1- (ash 1 size)))
         (v (cadr expr)) (x (cadddr expr)))
    `(logior ,(if (zerop pos)
                  `(logand ,v ,mask)
                  `(ash (logand ,v ,mask) ,pos))
             (logand ,x ,(lognot (ash mask pos))))))

(defun fold-typed-operand (expr)
  "FOLD-CONSTANT-SYMBOL, and (LDB (BYTE SIZE POS) X) with literal SIZE and POS
   rewritten to the LOGAND / ASH form the LDB compiler entry open-codes (and
   (DPB V (BYTE SIZE POS) X) to the DPB entry's LOGIOR form). For the
   typed-path predicates and COMPILE-AS-LONG only, which look at a form before
   it is compiled. A call to a MACROLET-bound macro, or to a global macro, is
   looked at through its expansion (%EXPAND-LOCAL-MACRO-OPERAND)."
  (let ((expr (%expand-local-macro-operand expr)))
    (let ((a (%typed-vector-struct-read expr)))
      (when a (setf expr a)))
    (cond ((%literal-ldb-form-p expr) (%literal-ldb-as-logand expr))
          ((%literal-dpb-form-p expr) (%literal-dpb-as-logior expr))
          (t (fold-constant-symbol expr)))))

(defun %typed-vector-struct-read (expr)
  "For EXPR a call (ACC X) to the reader of a (:TYPE VECTOR) structure
   (*TYPED-VECTOR-STRUCT-ACCESSORS*), the (AREF X IDX) its DEFUN computes; else
   NIL. Not when ACC is a local function or is NOTINLINE."
  (and (consp expr)
       (symbolp (car expr)) (car expr)
       (consp (cdr expr)) (null (cddr expr))
       (let ((idx (gethash (car expr) *typed-vector-struct-accessors*)))
         (and idx
              (not (local-function-entry (car expr)))
              (not (macrolet-shadowed-p (car expr)))
              (not (member (car expr) *notinline-functions*))
              (not (%global-notinline-p (car expr)))
              (not (local-function-entry 'aref))
              `(aref ,(cadr expr) ,idx)))))

(defun %expand-local-macro-operand (expr)
  "EXPR, or when EXPR is a call to a macro bound by an enclosing MACROLET (or
   to a global macro, %GLOBAL-MACRO-OPERAND-EXPANDER), its expansion
   (repeated while the expansion is again such a call). A local macro
   is how a library names an array element or a word of state inside a hot
   function -- (macrolet ((ref (i) `(aref x ,i))) ...) -- and the typed-path
   predicates only see the form they are given, so (logxor (ref 0) ...) was not
   known to be an integer even when (aref x 0) is. The expansion goes through
   CACHED-MACROEXPAND, so where the form is then compiled it is the same
   expansion. An expander that signals leaves EXPR as it is: compiling the form
   reports the error where it always did."
  (loop repeat 16
        while (and (consp expr) (symbolp (car expr)))
        do (let ((expander (if (macrolet-shadowed-p (car expr))
                               (gethash (car expr) *macros*)
                               (%global-macro-operand-expander (car expr)))))
             (unless expander (return))
             (let ((e (handler-case (cached-macroexpand expr expander)
                        (error () (return)))))
               (when (eq e expr) (return))
               (setf expr e))))
  expr)

(defun %global-macro-operand-expander (op)
  "The expander COMPILE-FORM would use for a call to the global macro OP, or
   NIL when COMPILE-FORM would do anything else with the call: a form with a
   handler of its own, a local function of that name, or one of the internal
   heads it matches by name. Not for COMMON-LISP operators: the typed paths
   know the ones that compute a number, and expanding the others (LOOP,
   DOTIMES, ...) wherever an operand is looked at only costs compile time.
   Only inside a top-level compile, where CACHED-MACROEXPAND memoizes: the
   typed-path predicates look at the same operand many times, and the form
   compiled later must be the same expansion. A macro written as an arithmetic
   helper -- (defmacro p+ (a b) `(ldb (byte 62 0) (+ ,a ,b))) -- is then seen as
   the integer expression it is, like the same form written out."
  (and *macroexpand-cache*
       (not (gethash op *compile-form-handlers*))
       (not (local-function-entry op))
       (let ((expander (find-macro-expander op)))
         (and expander
              (let ((pkg (symbol-package op)))
                (not (and pkg (string= (package-name pkg) "COMMON-LISP"))))
              (not (member (symbol-name op)
                           '("TRY-EVAL" "%SET-CHAR" "%SET-ELT" "%SET-SUBSEQ"
                             "%PUTF" "INTERN" "DEFMACRO" "%DOTNET-CALL-DIRECT")
                           :test #'string=))
              expander))))

(defun %literal-ldb-form-p (expr)
  "True when EXPR is (LDB (BYTE SIZE POS) X) with literal SIZE and POS, the
   shape the LDB compiler entry open-codes as a shift and a mask."
  (and (consp expr) (eq (car expr) 'ldb)
       (consp (cdr expr)) (consp (cddr expr)) (null (cdddr expr))
       (%literal-byte-spec (cadr expr))
       (not (local-function-entry 'ldb))
       (not (local-function-entry 'byte))
       (not (macrolet-shadowed-p 'ldb))
       (not (macrolet-shadowed-p 'byte))))

(defun %literal-ldb-as-logand (expr)
  "(LDB (BYTE SIZE POS) X) as the LOGAND / ASH form it means. The typed paths
   look at a form through FOLD-TYPED-OPERAND, so they see the mask: without
   this, (logior (ldb (byte 32 0) (ash x 7)) ...) was not known to be an
   integer at all and every operator around it took the boxed generic path."
  (let* ((spec (%literal-byte-spec (cadr expr)))
         (size (car spec)) (pos (cdr spec))
         (mask (1- (ash 1 size)))
         (x (caddr expr)))
    (if (zerop pos)
        `(logand ,x ,mask)
        `(logand (ash ,x ,(- pos)) ,mask))))

(defvar *int-type-memo* nil
  "EQ table of answers of FIXNUM-TYPED-P and EXPR-INT-RANGE, bound for the
   duration of one outermost query. The two recurse into each other on the same
   operands (a LOGAND asks both whether its left operand is a fixnum and what
   its range is), so without the table a left-associated chain of N operands
   costs 2^N: a 30-operand LOGAND took minutes to compile. Within one query the
   compilation state they read does not change, so an answer can be reused.
   Each form maps to a list of (TAG *FIXNUM-WRAP-LICENSED* . ANSWER), the
   license being the one variable they rebind.
   Inside a native int64 chain an outermost query takes the table of
   *INT-TYPE-SPAN* instead of a fresh one, keeping the answers for the next
   query about an operand of the chain.
   Keyed by identity, not EQUAL: the recursion visits the operand conses
   themselves, so EQ finds them. An EQUAL hash looks only a few levels into a
   list, so in a deeply nested (+ 1 (+ 1 ...)) every operand had the same hash
   and each lookup compared whole trees: compiling N levels took N^3.")

(defun %int-type-memoized (tag expr fn)
  (if (atom expr)
      (funcall fn expr)
      (flet ((lookup ()
               (let* ((license *fixnum-wrap-licensed*)
                      (hit (find-if (lambda (e) (and (eq (car e) tag)
                                                     (eq (cadr e) license)))
                                    (gethash expr *int-type-memo*))))
                 (if hit
                     (cddr hit)
                     (let ((v (funcall fn expr)))
                       (push (list* tag license v) (gethash expr *int-type-memo*))
                       v)))))
        (if *int-type-memo*
            (lookup)
            (let ((*int-type-memo* (or (%int-type-span-table 1)
                                       (make-hash-table :test 'eq))))
              (lookup))))))

(defun expr-int-range (expr)
  "Provable inclusive integer range (lo . hi) for EXPR computed entirely within
   int64, or NIL if unknown or any intermediate +/-/*/1+/1- result could exceed
   int64. Used to gate the raw unboxed arithmetic path."
  (%int-type-memoized :range expr #'%expr-int-range))

(defun %expr-int-range (expr)
  "Provable inclusive integer range (lo . hi) for EXPR computed entirely within
   int64, or NIL if unknown or any intermediate +/-/*/1+/1- result could exceed
   int64. Used to gate the raw unboxed arithmetic path."
  (let ((expr (fold-nary-arith expr))
        (*fixnum-wrap-licensed* nil))
  (cond
    ((integerp expr) (cons expr expr))
    ((%rotate-byte-32-long-p expr) (cons 0 #xFFFFFFFF))
    ((and (consp expr) (= (length expr) 3) (member (car expr) '(+ - *)))
     (let ((ra (expr-int-range (cadr expr)))
           (rb (expr-int-range (caddr expr))))
       (and ra rb
            (let ((r (range-arith (car expr) ra rb)))
              (and (range-fits-int64-p r) r)))))
    ((and (consp expr) (= (length expr) 2) (member (car expr) '(1+ 1-)))
     (let ((ra (expr-int-range (cadr expr))))
       (and ra
            (let ((r (range-arith (if (eq (car expr) '1+) '+ '-) ra '(1 . 1))))
              (and (range-fits-int64-p r) r)))))
    ;; (min a b ...) / (max a b ...): the bounds of the bounds.
    ((and (consp expr) (member (car expr) '(min max)) (consp (cdr expr))
          (not (local-function-entry (car expr))))
     (let ((rs (mapcar #'expr-int-range (cdr expr))))
       (and (every #'identity rs)
            (if (eq (car expr) 'min)
                (cons (reduce #'min rs :key #'car) (reduce #'min rs :key #'cdr))
                (cons (reduce #'max rs :key #'car) (reduce #'max rs :key #'cdr))))))
    ;; (if c a b): the value is one arm or the other, so its range is the
    ;; union of theirs. Both arms are needed: a two-armed IF answers NIL when
    ;; the test fails. FIXNUM-TYPED-P and COMPILE-AS-LONG already take this
    ;; shape (raw int64 on each path); without the union here every consumer
    ;; that asks for a proof -- 1+ / 1-, the binop fast path, an Int64 slot's
    ;; init -- saw no range and compiled the IF boxed, arms included.
    ((and (consp expr) (eq (car expr) 'if) (= (length expr) 4))
     (let ((ra (expr-int-range (caddr expr))))
       (and ra
            (let ((rb (expr-int-range (cadddr expr))))
              (and rb
                   (cons (min (car ra) (car rb)) (max (cdr ra) (cdr rb))))))))
    ;; (ash X n), constant n: ASH is monotonic in X, so the range is the shifted
    ;; bounds. A left shift qualifies only when those fit int64, which is what
    ;; makes FIXNUM-TYPED-P accept it as exact.
    ((and (consp expr) (= (length expr) 3) (eq (car expr) 'ash)
          (integerp (caddr expr)) (<= -63 (caddr expr) 63)
          (not (local-function-entry 'ash)))
     (let ((ra (expr-int-range (cadr expr)))
           (n (caddr expr)))
       (and ra (fixnum-exact-p (cadr expr))
            (let ((r (cons (ash (car ra) n) (ash (cdr ra) n))))
              (and (range-fits-int64-p r) r)))))
    ;; (ash X n), N a variable within [-63, 63]: ASH is monotonic in each argument
    ;; with the other fixed, so the extremes are at the corners.
    ((and (consp expr) (= (length expr) 3) (eq (car expr) 'ash)
          (not (integerp (caddr expr)))
          (not (local-function-entry 'ash)))
     (or (let ((ra (expr-int-range (cadr expr)))
               (rn (expr-int-range (caddr expr))))
           (and ra rn (<= -63 (car rn)) (<= (cdr rn) 63)
                (fixnum-exact-p (cadr expr)) (fixnum-exact-p (caddr expr))
                (let* ((c (list (ash (car ra) (car rn)) (ash (car ra) (cdr rn))
                                (ash (cdr ra) (car rn)) (ash (cdr ra) (cdr rn))))
                       (r (cons (reduce #'min c) (reduce #'max c))))
                  (and (range-fits-int64-p r) r))))
         (and (or (and (fixnum-exact-p (cadr expr))
                       (%ash-count-range-within (caddr expr) -63 0))
                  (and *fixnum-wrap-licensed* (fixnum-typed-p (cadr expr))
                       (%ash-count-range-within (caddr expr) 0 63)))
              (cons +int64-min+ +int64-max+))))
    ;; (floor X d) / (truncate X d), constant nonzero d: the quotient is
    ;; monotonic in X (non-decreasing for a positive d, non-increasing for a
    ;; negative one), so its range is the quotients of X's bounds. Only the
    ;; first value is described; a caller that wants both values does not ask
    ;; for a range. (floor (length v) 8) as a loop count is the usual shape.
    ((%const-divisor-quotient-p expr)
     (let ((ra (expr-int-range (cadr expr)))
           (d (caddr expr)))
       (and ra
            ;; A divisor of -1 is negation: FLOOR of the most negative int64
            ;; by -1 is not computed here, so this does not depend on the host
            ;; getting that one quotient right.
            (let* ((fn (if (eq (car expr) 'floor) #'floor #'truncate))
                   (q1 (if (eql d -1) (- (car ra)) (funcall fn (car ra) d)))
                   (q2 (if (eql d -1) (- (cdr ra)) (funcall fn (cdr ra) d)))
                   (r (cons (min q1 q2) (max q1 q2))))
              (and (range-fits-int64-p r) r)))))
    (t (fixnum-leaf-range expr)))))

(defun %const-divisor-quotient-p (expr)
  "True when EXPR is (FLOOR X D) or (TRUNCATE X D) with D a nonzero integer
   constant and X an exact fixnum expression (FIXNUM-EXACT-P): the quotient of
   the true X, not of a wrapped one."
  (and (consp expr) (= (length expr) 3)
       (member (car expr) '(floor truncate))
       (not (local-function-entry (car expr)))
       (integerp (caddr expr))
       (not (zerop (caddr expr)))
       (<= +int64-min+ (caddr expr) +int64-max+)
       (fixnum-exact-p (cadr expr))))

(defun fixnum-binop-native-p (op args)
  "True when (OP A B) may be computed by COMPILE-FIXNUM-BINOP -- both operands
   pushed as raw int64 and the operation done by Runtime.{Add,Subtract,Multiply}Fixnum,
   which promotes to a bignum on int64 overflow and boxes the result.

   Two ways to qualify. The first is the value-range proof, which covers nested
   arithmetic: every intermediate is shown to fit int64, so COMPILE-AS-LONG may
   compute the operands raw.

   The second covers the shape the proof can never accept: operands that are
   already raw int64. A FIXNUM-declared parameter of a natively-entered function
   lives in an Int64 slot, so its range is the whole of int64 and (- N 1) is not
   provably in range -- yet N needs no computing at all, and the subtraction
   itself is what the promoting helper handles. Without this clause the operand
   was boxed back into a Fixnum only for the generic path to take it apart again,
   which is why declaring an FTYPE made a function slower than not declaring it.
   Only leaves qualify (a literal, an Int64 slot, a slot with a proven range):
   an arithmetic operand would reintroduce exactly the intermediate the range
   proof exists to check."
  (and (fixnum-typed-p (first args))
       (fixnum-typed-p (second args))
       (or (fixnum-arith-unboxed-safe-p (cons op args))
           (every (lambda (a)
                    (or (and (integerp a) (<= +int64-min+ a +int64-max+))
                        (and (symbolp a)
                             (boundp '*cstate*)
                             (or (native-slot-p a (cstate-long-locals))
                                 (small-int-local-range a)))))
                  args))))

(defun fixnum-arith-unboxed-safe-p (expr)
  "True when the BOXED fixnum fast path may compute EXPR with raw int64 ops and box
   the result. Safe only when a static value-range proof shows every intermediate
   +/-/*/1+/1- result fits int64; otherwise the caller must use the generic
   promoting path (Runtime.Add/Subtract/Multiply) so overflow yields a bignum.
   (The unsafe raw long path inside native bodies is gated separately by
   compile-as-long / fixnum-typed-p and is NOT routed through here.)"
  (and (expr-int-range expr) t))

(defun string-char-code-ref (expr)
  "(STRING . INDEX) when EXPR is (char-code (schar S I)) or (char-code (char S I)),
   NIL otherwise. That composition is how a string scan is written, and it has a
   typed entry (Runtime.CharCodeAtL) that produces the code directly -- reading
   the character first means a boxed index going in and a character object coming
   out, per iteration. A local flet/labels shadowing either name means the form
   is not this composition at all."
  (and (consp expr) (eq (car expr) 'char-code) (= (length expr) 2)
       (not (local-function-entry 'char-code))
       (let ((inner (cadr expr)))
         (and (consp inner) (member (car inner) '(schar char))
              (= (length inner) 3)
              (not (local-function-entry (car inner)))
              (cons (cadr inner) (caddr inner))))))
