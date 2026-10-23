;;; cil-compiler-fixnum.lisp: Octet words, fixnum-typed expressions, the 64-bit modular lane, int64 arithmetic
;;; Part of the CIL compiler (A2 instruction list architecture)

(in-package :dotcl.cil-compiler)

;;; ------------------------------------------------------------
;;; A word read from adjacent octets.
;;;
;;; (dpb (aref v (1+ i)) (byte 8 8) (aref v i)) reads a 16-bit little-endian
;;; word, and a library writing portable code builds its 32- and 64-bit words
;;; from that one through INLINE functions: ironclad's UB32REF/LE is two
;;; UB16REF/LE deposited into one word, UB64REF/LE two UB32REF/LE. Substituted,
;;; a 64-bit word is 8 octet loads, each with its own range check, and the
;;; shifts and ORs that put them together; the JIT does not merge adjacent
;;; loads. A call to an INLINE function whose body is such a word (of 2, 4 or 8
;;; octets, little- or big-endian, at a constant distance from its index
;;; parameter) is compiled instead as one range check and one wide load
;;; (Runtime.ReadU32LE and the like). The 64-bit word is an operand of the
;;; (unsigned-byte 64) lane as its bits; the narrower ones are fixnums.
;;; ------------------------------------------------------------

(defvar *octet-word-templates* (make-hash-table :test #'equal :synchronized t)
  "Inline function name -> (DEF . SUMMARY): what %OCTET-WORD-TEMPLATE found
   for the recorded definition DEF (the one in *INLINE-DEFS*). A redefinition
   records a new DEF, which is analysed again.")

(defun %ow-word (bytes)
  (list :word bytes (if bytes (+ 8 (reduce #'max (mapcar #'fourth bytes))) 0)))

(defun %ow-shift (word n)
  (%ow-word (mapcar (lambda (b) (list (first b) (second b) (third b) (+ (fourth b) n)))
                    (second word))))

(defun %ow-index (a)
  "A (:IDX PARAM OFFSET) for the abstract value A used as an index, or NIL."
  (case (car a)
    (:param (list :idx (second a) 0))
    (:idx a)
    (t nil)))

(defun %ow-eval (form env depth)
  "The abstract value of FORM, part of an inline function's body, with ENV
   giving its parameters' abstract values: (:CONST N), (:PARAM P) for the
   argument in position P itself, (:IDX P C) for that argument plus C, or
   (:WORD BYTES WIDTH), the sum of octets (VEC-PARAM IDX-PARAM C SHIFT) each
   read from element IDX + C of vector VEC and shifted left by SHIFT. NIL when
   FORM is anything else."
  (cond
    ((integerp form) (list :const form))
    ((and (symbolp form) form (not (eq form t)))
     (cdr (assoc form env)))
    ((or (not (consp form)) (not (symbolp (car form))) (not (listp (cdr form))))
     nil)
    (t
     (let ((op (car form)) (args (cdr form)))
       (flet ((ev (x) (%ow-eval x env depth)))
         (case op
           ((+)
            (case (length args)
              (1 (ev (first args)))
              (2 (let ((a (ev (first args))) (b (ev (second args))))
                   (cond ((and (eq (car a) :const) (eq (car b) :const))
                          (list :const (+ (second a) (second b))))
                         ((and (eq (car b) :const) (%ow-index a))
                          (let ((i (%ow-index a)))
                            (list :idx (second i) (+ (third i) (second b)))))
                         ((and (eq (car a) :const) (%ow-index b))
                          (let ((i (%ow-index b)))
                            (list :idx (second i) (+ (third i) (second a)))))
                         (t nil))))
              (t nil)))
           ((1+ 1-)
            (and (= (length args) 1)
                 (let ((i (%ow-index (ev (first args)))))
                   (and i (list :idx (second i)
                                (+ (third i) (if (eq op '1+) 1 -1)))))))
           ((-)
            (and (= (length args) 2)
                 (let ((i (%ow-index (ev (first args))))
                       (c (ev (second args))))
                   (and i (eq (car c) :const)
                        (list :idx (second i) (- (third i) (second c)))))))
           ((aref)
            (and (= (length args) 2)
                 (let ((v (ev (first args)))
                       (i (%ow-index (ev (second args)))))
                   (and (eq (car v) :param) i
                        (%ow-word (list (list (second v) (second i) (third i) 0)))))))
           ((dpb)
            ;; (dpb A (byte S P) B) is B with A in its bits P..P+S-1: B's own
            ;; bits there must be zero, and A must fit in S bits.
            (and (= (length args) 3)
                 (let ((bs (second args)))
                   (and (consp bs) (eq (car bs) 'byte) (= (length bs) 3)
                        (integerp (second bs)) (integerp (third bs))
                        (<= 0 (second bs)) (<= 0 (third bs))
                        (let ((a (ev (first args))) (b (ev (third args))))
                          (and (eq (car a) :word) (<= (third a) (second bs))
                               (or (and (eq (car b) :const) (eql (second b) 0))
                                   (and (eq (car b) :word) (<= (third b) (third bs))))
                               (%ow-word (append (and (eq (car b) :word) (second b))
                                                 (second (%ow-shift a (third bs)))))))))))
           ((logior)
            (and args
                 (let ((bytes '()))
                   (dolist (x args (%ow-word bytes))
                     (let ((a (ev x)))
                       (unless (eq (car a) :word) (return nil))
                       (setf bytes (append bytes (second a))))))))
           ((ash)
            (and (= (length args) 2) (integerp (second args)) (<= 0 (second args) 64)
                 (let ((a (ev (first args))))
                   (and (eq (car a) :word) (%ow-shift a (second args))))))
           ((block)
            (and (consp args) (symbolp (first args))
                 (consp (cdr args)) (null (cddr args))
                 (not (%block-returns-to-p (first args) (cdr args)))
                 (ev (second args))))
           ((progn)
            (and (= (length args) 1) (ev (first args))))
           (t
            ;; A call to another INLINE function: its body, with its parameters
            ;; standing for the abstract values of the arguments.
            (let ((def (and (< depth 8) (gethash op *inline-defs*))))
              (and def
                   (= (length (car def)) (length args))
                   (every (lambda (p) (and (symbolp p) p
                                           (not (member p lambda-list-keywords))))
                          (car def))
                   (let ((body (%ow-body-form (cdr def))))
                     (and body
                          (let ((inner (loop for p in (car def)
                                             for a in args
                                             for v = (ev a)
                                             unless v do (return :fail)
                                             collect (cons p v))))
                            (and (listp inner)
                                 (%ow-eval body inner (1+ depth)))))))))))))))

(defun %ow-body-form (body)
  "The single form of a recorded inline BODY after its docstring and
   declarations, or NIL."
  (let ((rest body))
    (when (and (stringp (car rest)) (cdr rest))
      (pop rest))
    (loop while (and (consp rest) (consp (car rest)) (eq (caar rest) 'declare))
          do (pop rest))
    (and (consp rest) (null (cdr rest)) (car rest))))

(defun %octet-word-analyze (def)
  "For DEF, (PARAMS . BODY) of an INLINE function of two parameters, whose
   value is a word of 2, 4 or 8 octets of its first argument starting at its
   second argument plus a constant: (START WIDTH BIG-ENDIAN-P). Else NIL."
  (let ((params (car def)))
    (and (= (length params) 2)
         (every (lambda (p) (and (symbolp p) p (not (member p lambda-list-keywords))))
                params)
         (not (eq (first params) (second params)))
         (let* ((body (%ow-body-form (cdr def)))
                (w (and body
                        (%ow-eval body (list (cons (first params) (list :param 0))
                                             (cons (second params) (list :param 1)))
                                  0))))
           (and (eq (car w) :word)
                (let* ((bytes (second w))
                       (k (length bytes)))
                  (and (member k '(2 4 8))
                       (every (lambda (b) (and (eql (first b) 0) (eql (second b) 1)))
                              bytes)
                       (let* ((start (reduce #'min (mapcar #'third bytes)))
                              (offs (sort (mapcar (lambda (b) (- (third b) start)) bytes) #'<)))
                         (and (equal offs (loop for j below k collect j))
                              (cond ((every (lambda (b) (= (fourth b) (* 8 (- (third b) start))))
                                            bytes)
                                     (list start k nil))
                                    ((every (lambda (b) (= (fourth b)
                                                           (* 8 (- (+ start k -1) (third b)))))
                                            bytes)
                                     (list start k t))
                                    (t nil)))))))))))

(defun %octet-word-template (name)
  (let ((def (gethash name *inline-defs*)))
    (and def
         (let ((hit (gethash name *octet-word-templates*)))
           (if (and hit (eq (car hit) def))
               (cdr hit)
               (let ((summary (%octet-word-analyze def)))
                 (setf (gethash name *octet-word-templates*) (cons def summary))
                 summary))))))

(defun octet-word-ref-info (expr)
  "(VECTOR INDEX START WIDTH BIG-ENDIAN-P) when EXPR is a call to an INLINE
   function that reads a word of WIDTH octets of VECTOR at INDEX + START (see
   %OCTET-WORD-ANALYZE) and would be substituted here; else NIL."
  (and (consp expr)
       (let ((op (car expr)))
         (and (symbolp op) op
              (not *cross-compiling*)
              (consp (cdr expr)) (consp (cddr expr)) (null (cdddr expr))
              (gethash op *inline-defs*)
              (let ((tmpl (%octet-word-template op)))
                (and tmpl
                     (not (special-operator-name-p op))
                     (not (gethash op *compile-form-handlers*))
                     (not (local-function-entry op))
                     (not (macrolet-shadowed-p op))
                     (not (find-macro-expander op))
                     (not (maybe-expand-compiler-macro op expr))
                     (maybe-expand-inline op expr)
                     (list* (cadr expr) (caddr expr) tmpl)))))))

(defun compile-octet-word-bits (info)
  "Instructions leaving the word INFO (from OCTET-WORD-REF-INFO) describes as an
   int64: the value for 2 and 4 octets, the bits of the (unsigned-byte 64) for
   8. The vector, then the index, are evaluated once, in that order.

   Through VECTOR's hoisted octet buffer when it has one: one wide read
   (Runtime.ReadU32LE ...), which checks the whole range once. Otherwise, and in
   the arm for a buffer the fetch declined, Runtime.OctetWordRef, which reads
   the vector it is given (above safety 0 the *Checked entry signals for a
   non-simple one, as the declaration of the octet reads does)."
  (destructuring-bind (vec idx start width be) info
    (let* ((*in-tail-position* nil) (*in-mv-context* nil)
           (entry (and (symbolp vec) vec
                       (let ((e (array-backing-entry (list 'aref vec 0))))
                         (and e (eql (fourth e) 1) e))))
           (ti (gen-local "OWI"))
           (idx-code `((:declare-local ,ti "Int64")
                       ,@(compile-index-to-long idx)
                       ,@(unless (zerop start) `((:ldc-i8 ,start) (:add)))
                       (:stloc ,ti)))
           (generic (lambda (vec-code)
                      `(,@vec-code
                        (:ldloc ,ti)
                        (:ldc-i4 ,width)
                        (:ldc-i4 ,(if be 1 0))
                        (:call ,(if (compiling-at-safety-0-p)
                                    "Runtime.OctetWordRef"
                                    "Runtime.OctetWordRefChecked"))))))
      (if entry
          (let* ((back (third entry))
                 (fast `((:ldloc ,back)
                         (:ldloc ,ti)
                         (:call ,(format nil "Runtime.ReadU~D~A" (* 8 width) (if be "BE" "LE")))))
                 (fall (funcall generic (compile-expr vec)))
                 (lfall (gen-label "OWFALL"))
                 (ldone (gen-label "OWDONE")))
            `(,@idx-code
              ,@(ecase (%backing-arm-for entry)
                  (:fast fast)
                  (:helper fall)
                  (:both `((:ldloc ,back)
                           (:brfalse ,lfall)
                           ,@fast
                           (:br ,ldone)
                           (:label ,lfall)
                           ,@fall
                           (:label ,ldone))))))
          (let ((tv (gen-local "OWV")))
            `((:declare-local ,tv "LispObject")
              ,@(compile-expr vec)
              (:stloc ,tv)
              ,@idx-code
              ,@(funcall generic `((:ldloc ,tv)))))))))

(defun compile-octet-word-boxed (info)
  "The word INFO describes as a Lisp integer."
  `(,@(compile-octet-word-bits info)
    (:call ,(if (= (fourth info) 8) "Runtime.BoxU64" "Fixnum.Make"))))

;;; The store of such a word, through the INLINE (SETF F) function of the same
;;; shape: (setf (aref v o) (logand value #xff) (aref v (1+ o)) (ldb (byte 8 8) value))
;;; and the wider ones as two (SETF F) calls of the narrower one with the halves
;;; of the value. Compiled as one range check and one wide store.

(defvar *octet-word-store-templates* (make-hash-table :test #'equal :synchronized t)
  "(SETF F) -> (DEF . SUMMARY), as *OCTET-WORD-TEMPLATES* for the readers.")

(defun %ow-value-eval (form env)
  "The abstract value of FORM as part of a value being stored: (:VBITS LO W),
   bits LO .. LO+W-1 of the value parameter (W NIL: all from LO up). NIL when
   FORM is anything else."
  (cond
    ((and (symbolp form) form (not (eq form t)))
     (let ((a (cdr (assoc form env))))
       (and (eq (car a) :vbits) a)))
    ((not (consp form)) nil)
    ((and (eq (car form) 'logand) (= (length form) 3))
     (let* ((m (if (integerp (second form)) (second form) (third form)))
            (x (if (integerp (second form)) (third form) (second form)))
            (a (and (integerp m) (plusp m) (= (logcount (1+ m)) 1)
                    (%ow-value-eval x env))))
       (and a
            (let ((n (integer-length m)))
              (list :vbits (second a) (if (third a) (min (third a) n) n))))))
    ((and (eq (car form) 'ldb) (= (length form) 3))
     (let ((bs (second form)))
       (and (consp bs) (eq (car bs) 'byte) (= (length bs) 3)
            (integerp (second bs)) (integerp (third bs))
            (<= 0 (second bs)) (<= 0 (third bs))
            (let ((a (%ow-value-eval (third form) env)))
              (and a
                   (let ((s (second bs)) (q (third bs)))
                     (list :vbits (+ (second a) q)
                           (if (third a) (max 0 (min s (- (third a) q))) s))))))))
    (t nil)))

(defun %ow-declared-type (body name)
  "The type BODY's leading declarations give NAME, or NIL."
  (let ((rest body))
    (when (and (stringp (car rest)) (cdr rest)) (pop rest))
    (loop while (and (consp rest) (consp (car rest)) (eq (caar rest) 'declare))
          do (dolist (d (cdr (pop rest)))
               (when (consp d)
                 (cond ((and (eq (car d) 'type) (consp (cdr d)) (member name (cddr d)))
                        (return-from %ow-declared-type (cadr d)))
                       ((and (consp (car d)) (member (caar d) '(unsigned-byte signed-byte integer))
                             (member name (cdr d)))
                        (return-from %ow-declared-type (car d)))))))
    nil))

(defun %ow-body-forms (body)
  (let ((rest body))
    (when (and (stringp (car rest)) (cdr rest)) (pop rest))
    (loop while (and (consp rest) (consp (car rest)) (eq (caar rest) 'declare))
          do (pop rest))
    rest))

(defun %ow-stores (def env depth)
  "The octets the (SETF F) definition DEF stores, as (VEC IDX C LO) entries:
   bits LO .. LO+7 of the value into element IDX + C of VEC, with ENV giving
   its parameters' abstract values. :FAIL when its body is anything other than
   such stores followed by its value parameter."
  (let* ((params (car def))
         (forms (%ow-body-forms (cdr def)))
         (stores '()))
    (unless (and (= (length params) 3)
                 (every (lambda (p) (and (symbolp p) p (not (member p lambda-list-keywords))))
                        params)
                 forms
                 (eq (car (last forms)) (first params)))
      (return-from %ow-stores :fail))
    (dolist (f (butlast forms) (nreverse stores))
      (unless (and (consp f) (eq (car f) 'setf) (evenp (length (cdr f))))
        (return-from %ow-stores :fail))
      (loop for (place val) on (cdr f) by #'cddr
            do (let ((v (%ow-value-eval val env)))
                 (unless (and v (third v) (consp place))
                   (return-from %ow-stores :fail))
                 (cond
                   ((and (eq (car place) 'aref) (= (length place) 3))
                    (let ((vec (%ow-eval (second place) env depth))
                          (i (%ow-index (%ow-eval (third place) env depth))))
                      (unless (and (eq (car vec) :param) i (= (third v) 8))
                        (return-from %ow-stores :fail))
                      (push (list (second vec) (second i) (third i) (second v)) stores)))
                   ((and (symbolp (car place)) (= (length place) 3) (< depth 8))
                    (let* ((name (list 'setf (car place)))
                           (idef (gethash name *inline-defs*)))
                      (unless idef (return-from %ow-stores :fail))
                      (let* ((ip (car idef))
                             (inner (and (= (length ip) 3)
                                         (%ow-stores
                                          idef
                                          (list (cons (first ip) (list :vbits (second v) (third v)))
                                                (cons (second ip) (%ow-eval (second place) env depth))
                                                (cons (third ip) (%ow-eval (third place) env depth)))
                                          (1+ depth)))))
                        ;; The inner value is exactly as wide as what it stores:
                        ;; its own declaration then holds.
                        (unless (and (listp inner) inner
                                     (= (third v) (* 8 (length inner))))
                          (return-from %ow-stores :fail))
                        (setf stores (append (reverse inner) stores)))))
                   (t (return-from %ow-stores :fail))))))))

(defun %octet-word-store-analyze (def)
  "For DEF, (PARAMS . BODY) of an INLINE (SETF F) of (VALUE VECTOR INDEX) that
   stores VALUE, declared (UNSIGNED-BYTE 8K), as a word of K = 2, 4 or 8 octets
   of VECTOR at INDEX plus a constant and returns VALUE: (START K BIG-ENDIAN-P).
   Else NIL."
  (let ((params (car def)))
    (and (= (length params) 3)
         (= (length (remove-duplicates params)) 3)
         (let ((stores (%ow-stores def
                                   (list (cons (first params) (list :vbits 0 nil))
                                         (cons (second params) (list :param 1))
                                         (cons (third params) (list :param 2)))
                                   0)))
           (and (listp stores)
                (let ((k (length stores)))
                  (and (member k '(2 4 8))
                       (equal (%ow-declared-type (cdr def) (first params))
                              (list 'unsigned-byte (* 8 k)))
                       (every (lambda (s) (and (eql (first s) 1) (eql (second s) 2))) stores)
                       (let* ((start (reduce #'min (mapcar #'third stores)))
                              (offs (sort (mapcar (lambda (s) (- (third s) start)) stores) #'<)))
                         (and (equal offs (loop for j below k collect j))
                              (cond ((every (lambda (s) (= (fourth s) (* 8 (- (third s) start))))
                                            stores)
                                     (list start k nil))
                                    ((every (lambda (s) (= (fourth s)
                                                           (* 8 (- (+ start k -1) (third s)))))
                                            stores)
                                     (list start k t))
                                    (t nil)))))))))))

(defun %octet-word-store-template (name)
  (let ((def (gethash name *inline-defs*)))
    (and def
         (let ((hit (gethash name *octet-word-store-templates*)))
           (if (and hit (eq (car hit) def))
               (cdr hit)
               (let ((summary (%octet-word-store-analyze def)))
                 (setf (gethash name *octet-word-store-templates*) (cons def summary))
                 summary))))))

(defun octet-word-store-info (form)
  "(VALUE VECTOR INDEX START WIDTH BIG-ENDIAN-P) when FORM is
   (FUNCALL #'(SETF F) VALUE VECTOR INDEX), what SETF of (F VECTOR INDEX)
   expands to, for an INLINE (SETF F) that stores a word of WIDTH octets (see
   %OCTET-WORD-STORE-ANALYZE) and would be substituted here; else NIL."
  (and (consp form) (eq (car form) 'funcall)
       (not *cross-compiling*)
       (not (local-function-entry 'funcall))
       (= (length form) 5)
       (let ((fe (second form)))
         (and (consp fe) (eq (car fe) 'function)
              (consp (cdr fe)) (null (cddr fe))
              (%setf-function-name-p (cadr fe))
              (let* ((name (cadr fe))
                     (tmpl (and (gethash name *inline-defs*)
                                (%octet-word-store-template name))))
                (and tmpl
                     (maybe-expand-inline name (cons name (cddr form)))
                     (list* (third form) (fourth form) (fifth form) tmpl)))))))

(defun compile-octet-word-store (info want-value)
  "Instructions storing the word INFO (from OCTET-WORD-STORE-INFO) describes,
   leaving the value of the SETF (the new value) when WANT-VALUE, else nothing.
   The value, the vector and the index are evaluated once, in that order.

   The value is taken as the octet stores take it. A 64-bit one goes through
   COMPILE-AS-U64-BITS, which checks it is an (UNSIGNED-BYTE 64), as the
   binding of the (SETF F) parameter so declared does. A narrower one is not
   checked against its declaration (no integer binding is): the octet stores
   take the low bits of any integer, LOGAND and LDB being two's complement,
   and the new value returned is the object given."
  (destructuring-bind (val vec idx start width be) info
    (let* ((*in-tail-position* nil) (*in-mv-context* nil)
           (tv (gen-local "OWS"))
           (ti (gen-local "OWI"))
           (to (and want-value (< width 8) (not (fixnum-exact-p val))
                    (gen-local "OWSO")))
           (val-code
             `((:declare-local ,tv "Int64")
               ,@(cond ((= width 8) (compile-as-u64-bits val))
                       ((fixnum-exact-p val) (compile-as-long val))
                       (to `((:declare-local ,to "LispObject")
                             ,@(compile-expr val)
                             (:dup)
                             (:stloc ,to)
                             (:call "Runtime.Low64")))
                       (t `(,@(compile-expr val)
                            (:call "Runtime.Low64"))))
               (:stloc ,tv)))
           (entry (and (symbolp vec) vec
                       (let ((e (array-backing-entry (list 'aref vec 0))))
                         (and e (eql (fourth e) 1) e))))
           (idx-code `((:declare-local ,ti "Int64")
                       ,@(compile-index-to-long idx)
                       ,@(unless (zerop start) `((:ldc-i8 ,start) (:add)))
                       (:stloc ,ti)))
           (generic (lambda (vec-code)
                      `(,@vec-code
                        (:ldloc ,ti)
                        (:ldloc ,tv)
                        (:ldc-i4 ,width)
                        (:ldc-i4 ,(if be 1 0))
                        (:call ,(if (compiling-at-safety-0-p)
                                    "Runtime.OctetWordSet"
                                    "Runtime.OctetWordSetChecked")))))
           (store
             (if entry
                 (let* ((back (third entry))
                        (fast `((:ldloc ,back)
                                (:ldloc ,ti)
                                (:ldloc ,tv)
                                (:call ,(format nil "Runtime.WriteU~D~A" (* 8 width)
                                                (if be "BE" "LE")))))
                        (fall (funcall generic (compile-expr vec)))
                        (lfall (gen-label "OWSFALL"))
                        (ldone (gen-label "OWSDONE")))
                   `(,@val-code
                     ,@idx-code
                     ,@(ecase (%backing-arm-for entry)
                         (:fast fast)
                         (:helper fall)
                         (:both `((:ldloc ,back)
                                  (:brfalse ,lfall)
                                  ,@fast
                                  (:br ,ldone)
                                  (:label ,lfall)
                                  ,@fall
                                  (:label ,ldone))))))
                 (let ((tvec (gen-local "OWSV")))
                   `(,@val-code
                     (:declare-local ,tvec "LispObject")
                     ,@(compile-expr vec)
                     (:stloc ,tvec)
                     ,@idx-code
                     ,@(funcall generic `((:ldloc ,tvec))))))))
      (cond ((not want-value) store)
            (to `(,@store (:ldloc ,to)))
            (t `(,@store
                 (:ldloc ,tv)
                 (:call ,(if (= width 8) "Runtime.BoxU64" "Fixnum.Make"))))))))

(defun fixnum-exact-p (expr)
  "True when COMPILE-AS-LONG of EXPR yields EXPR's exact value, not just its low
   64 bits: FIXNUM-TYPED-P with *FIXNUM-WRAP-LICENSED* NIL. Callers that may
   themselves run under the license (the fixnum-typed-p clauses for MOD, REM,
   ASH) use this for the operands whose high bits they read."
  (let ((*fixnum-wrap-licensed* nil))
    (fixnum-typed-p expr)))

(defun fixnum-wrap-typed-p (expr)
  "FIXNUM-TYPED-P under *FIXNUM-WRAP-LICENSED*: COMPILE-AS-LONG of EXPR is
   EXPR's value modulo 2^64. Only for a result that lands in a place declared
   FIXNUM; see *FIXNUM-WRAP-LICENSED*."
  (let ((*fixnum-wrap-licensed* t))
    (fixnum-typed-p expr)))

(defun %logand-nonneg-operand (a b)
  "For (LOGAND A B), the operand whose exact value is known non-negative, or
   NIL. LOGAND with a non-negative int64 keeps only bits below 63, and those
   are the same in the wrapped and the true value of the other operand, so the
   other one needs only FIXNUM-TYPED-P under the license."
  (flet ((nonneg (x) (let ((r (expr-int-range x)))
                       (and r (>= (car r) 0) (range-fits-int64-p r)
                            (fixnum-exact-p x)))))
    (cond ((nonneg b) b)
          ((nonneg a) a)
          (t nil))))

(defun %ash-count-range-within (count lo hi)
  "True when COUNT, an ASH shift count, is an exact integer expression whose
   proven range lies inside [LO, HI]."
  (let ((r (expr-int-range count)))
    (and r (<= lo (car r)) (<= (cdr r) hi)
         (fixnum-exact-p count))))

(defun %fixnum-min-max-p (expr)
  "True when EXPR is (MIN A B ...) or (MAX A B ...) of exact fixnums."
  (and (consp expr) (member (car expr) '(min max)) (consp (cdr expr)) (consp (cddr expr))
       (not (local-function-entry (car expr)))
       (every #'fixnum-exact-p (cdr expr))))

(defun compile-long-min-max (expr)
  "EXPR, a %FIXNUM-MIN-MAX-P form, as a raw int64. Each operand goes to an
   Int64 temp before the next is computed, as in COMPILE-LONG-BINOP."
  (let ((entry (if (eq (car expr) 'min) "Math.MinLong" "Math.MaxLong"))
        (code (compile-as-long (cadr expr))))
    (dolist (arg (cddr expr) code)
      (let ((ta (gen-local "LMA")) (tb (gen-local "LMB")))
        (setf code `((:declare-local ,ta "Int64") (:declare-local ,tb "Int64")
                     ,@code (:stloc ,ta)
                     ,@(compile-as-long arg) (:stloc ,tb)
                     (:ldloc ,ta) (:ldloc ,tb) (:call ,entry)))))))

(defun fixnum-typed-p (expr)
  "Return T if EXPR is statically known to produce a Fixnum value; see
   %FIXNUM-TYPED-P."
  (%int-type-memoized :fixnum expr #'%fixnum-typed-p))

(defun %fixnum-typed-p (expr)
  "Return T if EXPR is statically known to produce a Fixnum value.
   Recognizes: literal integers in fixnum range, (the fixnum E),
   references to lexical locals declared fixnum (via *fixnum-locals*),
   calls to functions declared (declaim (ftype (function (...) fixnum) NAME)),
   and fixnum-typed arithmetic (+, -, *, 1+, 1-) whose operands are
   themselves fixnum-typed and whose result a range proof keeps in int64.
   The int64 COMPILE-AS-LONG leaves for such an EXPR is its exact value; under
   *FIXNUM-WRAP-LICENSED* only its value modulo 2^64."
  (setf expr (fold-typed-operand expr))
  (cond
    ((integerp expr) (<= +int64-min+ expr +int64-max+))
    ((%rotate-byte-32-long-p expr) t)
    ((%u64-local-field expr) t)
    ;; Direct Int64 local in native function body: already long, no unbox needed
    ((and (symbolp expr)
          (boundp '*cstate*)
          (native-slot-p expr (cstate-long-locals)))
     t)
    ;; Local var declared fixnum: must be a non-captured simple local
    ((and (symbolp expr)
          (boundp '*fixnum-locals*)
          (member (var-name expr) *fixnum-locals* :test #'string=)
          ;; Boxed (captured) vars need indirection: stick with generic path
          (not (boxed-var-p expr))
          (lookup-local expr))
     t)
    ;; Local with a proven bounded int64 range (signed-byte/unsigned-byte/bit
    ;; declaration or let-init inference). Slot holds a boxed Fixnum.
    ((small-int-local-range expr) t)
    ;; aref on a proven numeric-backed array local: elements are integers
    ;; within the storage range (stores are range-checked by the runtime).
    ((numeric-array-aref-info expr) t)
    ;; (char-code (schar s i)): a character code is a non-negative integer below
    ;; CHAR-CODE-LIMIT, so it is a fixnum whatever the string holds.
    ((string-char-code-ref expr) t)
    ;; A word of 2 or 4 adjacent octets (OCTET-WORD-REF-INFO).
    ;; A 64-bit one only where its low 64 bits will do: its bits are those.
    ((let ((ow (octet-word-ref-info expr)))
       (and ow (or (< (fourth ow) 8) *fixnum-wrap-licensed*)))
     t)
    ;; (if c a b) with both arms fixnum-typed. Needs the else arm: a two-armed
    ;; IF yields NIL when the test fails, which is not a fixnum. Without this
    ;; the whole branch falls to the boxed path, and since a recursive function
    ;; is usually shaped (if base recur), that is where the boxing survives.
    ((and (consp expr) (eq (car expr) 'if) (= (length expr) 4)
          (fixnum-typed-p (caddr expr))
          (fixnum-typed-p (cadddr expr)))
     t)
    ((and (consp expr) (eq (car expr) 'the)
          (let ((ty (cadr expr)))
            (or (eq ty 'fixnum)
                (and (consp ty) (eq (car ty) 'integer))
                ;; (the (signed-byte N) E) etc.: only int64-fitting widths.
                ;; A wide type like (signed-byte 100) can hold a bignum at
                ;; runtime; bitwise/compare ops skip the overflow gate, so a
                ;; bare unbox-fixnum there would corrupt the value.
                (let ((r (declared-integer-range ty)))
                  (and r (range-fits-int64-p r))))))
     t)
    ;; +, -, *: fixnum-typed operands are not enough. COMPILE-AS-LONG computes
    ;; them with the raw int64 op, which wraps, so the result is exact only
    ;; where a range proof says it fits -- or where the license says the low
    ;; 64 bits are all the destination keeps.
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(+ - *))
          (fixnum-typed-p (cadr expr))
          (fixnum-typed-p (caddr expr))
          (or *fixnum-wrap-licensed* (expr-int-range expr)))
     t)
    ;; Three or more arguments: the same question about the left-associated form
    ;; the call stands for. Without this an index like (- n i 1) is not fixnum-
    ;; typed, which disqualifies the whole AREF from the native path.
    ((and (consp expr) (member (car expr) '(+ - * logand logior logxor)) (> (length expr) 3))
     (fixnum-typed-p (fold-nary-arith expr)))
    ((and (consp expr) (= (length expr) 2)
          (member (car expr) '(1+ 1-))
          (fixnum-typed-p (cadr expr))
          (or *fixnum-wrap-licensed* (expr-int-range expr)))
     t)
    ((and (consp expr) (= (length expr) 2)
          (eq (car expr) '-)
          (fixnum-typed-p (cadr expr))
          (or *fixnum-wrap-licensed* (expr-int-range expr)))
     t)
    ;; %dotimes-1+: dotimes-emitted increment whose result is asserted to fit
    ;; int64 (counter < limit at the increment site), fixnum-typed like 1+.
    ((and (consp expr) (= (length expr) 2)
          (eq (car expr) '%dotimes-1+)
          (fixnum-typed-p (cadr expr)))
     t)
    ;; %dotimes-limit / %loop-limit-floor: a loop limit clamped into int64.
    ((and (consp expr) (= (length expr) 2)
          (member (car expr) '(%dotimes-limit %loop-limit-floor)))
     t)
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '%loop-by)
          (fixnum-typed-p (cadr expr)))
     t)
    ;; logand/logior/logxor with fixnum operands -> fixnum result. Each bit of
    ;; the result depends on the same bit of the operands only, so exact
    ;; operands give an exact result and wrapped ones a wrapped one.
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(logand logior logxor))
          (fixnum-typed-p (cadr expr))
          (fixnum-typed-p (caddr expr)))
     t)
    ;; (logand X M) with M exact and non-negative is exact even when X is
    ;; wrapped: only bits below 63 survive, and wrapping keeps those.
    ;; (logand (+ a b) #xFFFFFFFF) is how 32-bit modular arithmetic is written.
    ((and (consp expr) (= (length expr) 3)
          (eq (car expr) 'logand)
          (not *fixnum-wrap-licensed*)
          (let ((m (%logand-nonneg-operand (cadr expr) (caddr expr))))
            (and m
                 (fixnum-wrap-typed-p (if (eq m (cadr expr)) (caddr expr) (cadr expr))))))
     t)
    ;; mod/rem of fixnums -> fixnum result (|r| < |divisor|, so no promotion).
    ;; The operands must be exact: the remainder of a wrapped value is not the
    ;; remainder of the true one.
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(mod rem))
          (fixnum-exact-p (cadr expr))
          (fixnum-exact-p (caddr expr)))
     t)
    ;; (floor X d) / (truncate X d) with a constant divisor whose quotient the
    ;; range proof keeps in int64: the first value, computed raw (COMPILE-AS-
    ;; LONG). The FLOOR handler still builds both values where both are wanted.
    ((and (%const-divisor-quotient-p expr)
          (expr-int-range expr))
     t)
    ;; MIN / MAX of exact fixnums is one of them.
    ((%fixnum-min-max-p expr) t)
    ((float-round-int-range expr) t)
    ;; lognot with fixnum operand -> fixnum result
    ((and (consp expr) (= (length expr) 2)
          (eq (car expr) 'lognot)
          (fixnum-typed-p (cadr expr)))
     t)
    ;; ash with fixnum value and constant shift -> fixnum result.
    ;; A non-negative shift can overflow int64 (and a raw CIL shl masks the count
    ;; mod 64), so it is only safe to treat as a raw long when both operands are
    ;; constants and the folded result fits in int64. Negative shifts (right
    ;; shift) always shrink and stay in range.
    ;; The shifted value must be EXACT (FIXNUM-EXACT-P), not merely
    ;; fixnum-typed: a right shift exposes the high bits, which a wrapped
    ;; product or sum has lost.
    ((and (consp expr) (= (length expr) 3)
          (eq (car expr) 'ash)
          (fixnum-exact-p (cadr expr))
          (integerp (caddr expr))
          (let ((n (caddr expr)))
            (or (< n 0)
                (and (integerp (cadr expr))
                     (typep (ash (cadr expr) n) '(signed-byte 64))))))
     t)
    ;; A left shift the range proof keeps in int64 is exact: SHL of a value
    ;; below 2^(63-n) in magnitude loses no bit. (ash sum 7) of a 32-bit sum.
    ((and (consp expr) (= (length expr) 3)
          (eq (car expr) 'ash)
          (integerp (caddr expr))
          (<= 0 (caddr expr) 63)
          (expr-int-range expr))
     t)
    ;; Under the license only the low 64 bits are wanted, and SHL by a count
    ;; below 64 computes exactly those from the low 64 bits of the operand:
    ;; (logand (ash x 7) #xFFFFFFFF) is how a 32-bit rotate is written. The count
    ;; may be a variable here, as long as its range is inside [0, 63] (CIL SHL
    ;; takes the count mod 64).
    ((and (consp expr) (= (length expr) 3)
          (eq (car expr) 'ash)
          *fixnum-wrap-licensed*
          (fixnum-typed-p (cadr expr))
          (%ash-count-range-within (caddr expr) 0 63))
     t)
    ;; A right shift by a variable count whose range is inside [-63, 0]: SHR by
    ;; the negated count, exact for an exact operand (see the constant case).
    ((and (consp expr) (= (length expr) 3)
          (eq (car expr) 'ash)
          (not (integerp (caddr expr)))
          (fixnum-exact-p (cadr expr))
          (%ash-count-range-within (caddr expr) -63 0))
     t)
    ;; A count of either sign: exact when the range proof keeps every result in int64.
    ((and (consp expr) (= (length expr) 3)
          (eq (car expr) 'ash)
          (not (integerp (caddr expr)))
          (fixnum-exact-p (cadr expr))
          (%ash-count-range-within (caddr expr) -63 63)
          (expr-int-range expr))
     t)
    ;; A structure slot declared to hold an integer that fits an int64. The
    ;; declaration is the only evidence -- a slot has no value to inspect at
    ;; compile time -- and it is now recorded and checked on every store, so it
    ;; is evidence worth acting on.
    ;;
    ;; What this decides is the READ. The slot's storage is raw, so a read that
    ;; is not known to be a fixnum goes through the generic path and boxes the
    ;; value on the way out -- on EVERY read, where the old boxed storage boxed
    ;; once and shared it. Raw storage without this clause is a pessimization,
    ;; not an optimization.
    ((and (consp expr) (= (length expr) 2) (symbolp (car expr))
          (not (local-function-entry (car expr)))
          (boundp '*struct-accessor-types*)
          (let ((ty (gethash (car expr) *struct-accessor-types*)))
            (and ty
                 (let ((r (if (eq ty 'fixnum)
                              (cons +int64-min+ +int64-max+)
                              (integer-type-range ty))))
                   (and r (range-fits-int64-p r))))))
     t)
    ;; (length x): a sequence length is a count of elements held in memory, so
    ;; it is a fixnum for any argument LENGTH accepts -- CLHS bounds a vector's
    ;; by ARRAY-TOTAL-SIZE-LIMIT, and a list's by what will fit. Here the bound
    ;; is tighter and exact: Runtime.Length counts into a C# int and every
    ;; return is Fixnum.Make of one, so the value cannot leave int32 range.
    ;;
    ;; What this buys is the loop bound. (let ((n (length arr))) (declare
    ;; (fixnum n)) ...) only earns an Int64 slot when its init is fixnum-typed,
    ;; so without this N sat in a boxed slot and every trip round the loop
    ;; unboxed it again to compare against the counter.
    ((and (consp expr) (= (length expr) 2)
          (eq (car expr) 'length)
          (not (local-function-entry 'length)))
     t)
    ;; A call to a function whose ftype promises an integer result that fits
    ;; int64: FIXNUM, or a narrower integer type such as (UNSIGNED-BYTE 32).
    ((call-int-return-range expr) t)
    (t nil)))

(defvar *long-simple-memo* nil
  "EQ set of the forms %LONG-OPERAND-SIMPLE-P has accepted on their shape alone,
   bound by the outermost COMPILE-LONG-BINOP / COMPILE-LONG-BINOP-SPILLED.
   Compiling (+ 1 (+ 1 ...)) asks the question at every level about the whole
   operand below it, so without the set N levels cost N^2 (most of the time
   to compile 2,000 levels). A form containing a two-operand LOGAND / LOGIOR /
   LOGXOR is not entered: its answer depends on the declarations in scope.")

(defvar *long-decl-memo* nil
  "EQ table of %LONG-OPERAND-SIMPLE-1 answers that depend on the declarations
   in scope, bound only during a walk inside an *INT-TYPE-SPAN*.")

(defun %long-operand-simple-p (expr)
  "True when EXPR compiles to a straight-line push with no branches, labels or
   exception regions; the only shape that is safe to leave half-evaluated on the
   CIL stack while the other operand is compiled.
   One *INT-TYPE-MEMO* serves the whole walk: each two-operand LOGAND / LOGIOR /
   LOGXOR in the tree asks FIXNUM-WRAP-TYPED-P about its operands, and as
   separate queries those re-walked the operand trees (N^3 for a nest of N).
   Nothing is compiled during the walk, so the answers stay valid throughout.
   Inside a native chain the walk's tables come from *INT-TYPE-SPAN*, so the
   walk at each level of a nest reuses what the level above found."
  (cond ((atom expr) (and (%long-operand-simple-1 expr) t))
        (*int-type-memo* (and (%long-operand-simple-1 expr) t))
        (t (let* ((*int-type-memo* (or (%int-type-span-table 1)
                                        (make-hash-table :test 'eq)))
                  (*long-decl-memo* (%int-type-span-table 2)))
             (and (%long-operand-simple-1 expr) t)))))

(defun %long-operand-simple-1 (expr)
  "%LONG-OPERAND-SIMPLE-P, answering :SHAPE when the answer does not depend on
   the declarations in scope (and so may be kept in *LONG-SIMPLE-MEMO*)."
  (cond
    ((or (integerp expr) (symbolp expr)) :shape)
    ((not (consp expr)) nil)
    ((and *long-simple-memo* (gethash expr *long-simple-memo*)) :shape)
    (*long-decl-memo*
     (multiple-value-bind (v found) (gethash expr *long-decl-memo*)
       (if found
           v
           (setf (gethash expr *long-decl-memo*)
                 (%long-operand-simple-2 expr)))))
    (t (%long-operand-simple-2 expr))))

(defun %long-operand-simple-2 (expr)
  "%LONG-OPERAND-SIMPLE-1 of a cons not found in the memo tables."
  (cond
    ((not (member (car expr) '(the + - * 1+ 1- logand logior logxor lognot ash
                               %dotimes-1+)))
     nil)
    (t
     (let ((answer :shape))
       ;; A bitwise op with an operand not known to be a fixnum branches
       ;; (COMPILE-GUARDED-LONG-BITOP).
       (when (and (member (car expr) '(logand logior logxor))
                  (= (length expr) 3))
         (unless (every #'fixnum-wrap-typed-p (cdr expr))
           (return-from %long-operand-simple-2 nil))
         (setf answer t))
       (dolist (arg (cdr expr))
         (let ((a (%long-operand-simple-1 arg)))
           (cond ((null a) (return-from %long-operand-simple-2 nil))
                 ((not (eq a :shape)) (setf answer t)))))
       (when (and (eq answer :shape) *long-simple-memo*)
         (setf (gethash expr *long-simple-memo*) t))
       answer))))

(defmacro with-long-simple-memo (&body body)
  `(let ((*int-type-span* (or *int-type-span*
                              (vector *type-env-epoch* nil nil))))
     (if *long-simple-memo*
         (progn ,@body)
         (let ((*long-simple-memo* (make-hash-table :test 'eq)))
           ,@body))))

(defun compile-long-binop-spilled (a b)
  "Push both operands as int64, evaluating a non-straight-line operand into its
   own Int64 temp first. Same reason as COMPILE-LONG-BINOP: an operand whose code
   contains branches or a join label cannot run with the other operand pending on
   the CIL stack."
  (with-long-simple-memo
  (if (and (%long-operand-simple-p a) (%long-operand-simple-p b))
      `(,@(compile-as-long a)
        ,@(compile-as-long b))
      (let ((ta (gen-local "LSA")) (tb (gen-local "LSB")))
        `((:declare-local ,ta "Int64")
          (:declare-local ,tb "Int64")
          ,@(compile-as-long a)
          (:stloc ,ta)
          ,@(compile-as-long b)
          (:stloc ,tb)
          (:ldloc ,ta)
          (:ldloc ,tb))))))

;; Operands of a bitwise op one of which is not known to be a fixnum, in a
;; place that wants an int64 result (the result is asserted, not the operands).
;; The unknown operands are computed boxed; when they are all fixnums the op runs
;; on raw int64 values as for typed operands, otherwise the generic boxed op runs
;; and its result is unboxed (a declaration that lied still fails there).
;; An operand known to be a fixnum stays an int64 on both paths; one only known
;; modulo 2^64 is computed boxed, since the slow path needs its exact value.
;;; DOTCL-ROTATE-BYTE:ROTATE-BYTE (the contrib, as sb-rotate-byte) on the
;;; whole 32- or 64-bit word: a hardware rotate.

(defun %rotate-byte-width (expr)
  "32 or 64 when EXPR is (DOTCL-ROTATE-BYTE:ROTATE-BYTE COUNT (BYTE W 0) X)
   with W 32 or 64, else NIL."
  (and (consp expr) (symbolp (car expr)) (car expr)
       (consp (cdr expr)) (consp (cddr expr)) (consp (cdddr expr)) (null (cddddr expr))
       (let ((pkg (symbol-package (car expr))))
         (and pkg (string= (package-name pkg) "DOTCL-ROTATE-BYTE")
              (string= (symbol-name (car expr)) "ROTATE-BYTE")))
       (%simd-callable-p (car expr))
       (let ((spec (third expr)))
         (cond ((and (consp spec) (eq (car spec) 'byte) (= (length spec) 3)
                     (not (local-function-entry 'byte)) (eql (third spec) 0)
                     (member (second spec) '(32 64)))
                (second spec))
               ((and (consp spec) (eq (car spec) 'quote) (consp (cadr spec))
                     (member (car (cadr spec)) '(32 64)) (eql (cdr (cadr spec)) 0))
                (car (cadr spec)))))))

(defun %rotate-byte-32-long-p (expr)
  "True when EXPR rotates a 32-bit field holding a value proven to lie in
   [0, 2^32) by an exact integer count: an int64 computation."
  (and (eql (%rotate-byte-width expr) 32)
       (fixnum-exact-p (second expr))
       (fixnum-exact-p (fourth expr))
       (let ((r (expr-int-range (fourth expr))))
         (and r (>= (car r) 0) (<= (cdr r) #xFFFFFFFF)))))

(defun %u64-local-field (expr)
  "(KEY SHIFT . MASK) when EXPR is (LDB (BYTE S P) X), or the LOGAND / ASH form
   it folds to, of a local X holding (UNSIGNED-BYTE 64) bits, with a mask below
   2^63: a logical shift and a mask of the raw slot, always a fixnum. Else NIL."
  (let ((e (fold-typed-operand expr)))
    (when (and (consp e) (eq (car e) 'logand) (= (length e) 3)
               (not (local-function-entry 'logand)))
      (let* ((m (if (integerp (caddr e)) (caddr e) (cadr e)))
             (x (if (eq m (caddr e)) (cadr e) (caddr e)))
             (shift 0))
        (when (and (integerp m) (<= 0 m +int64-max+))
          (when (and (consp x) (eq (car x) 'ash) (= (length x) 3)
                     (integerp (caddr x)) (<= -63 (caddr x) 0)
                     (not (local-function-entry 'ash)))
            (setf shift (- (caddr x)) x (cadr x)))
          (let ((key (and (symbolp x) x (u64-native-local-p x))))
            (and key (list* key shift m))))))))

(defun compile-rotate-byte-32-long (expr)
  (let ((tc (gen-local "ROTC")) (tx (gen-local "ROTX"))
        (*in-tail-position* nil) (*in-mv-context* nil))
    `((:declare-local ,tc "Int64") (:declare-local ,tx "Int64")
      ,@(compile-as-long (second expr)) (:stloc ,tc)
      ,@(compile-as-long (fourth expr)) (:stloc ,tx)
      (:ldloc ,tx) (:ldloc ,tc) (:call "Runtime.RotateLeft32"))))

;;; ------------------------------------------------------------
;;; 64-bit modular lane: integer expressions whose value is known to lie in
;;; [0, 2^64) and is made of operations that are exact modulo 2^64.
;;;
;;; An (unsigned-byte 64) at or above 2^63 is a bignum, so code written for
;;; 64-bit words -- (ldb (byte 64 0) (+ a b)), (logand x #xFFFFFFFFFFFFFFFF),
;;; a 64-bit rotate -- went through the generic bignum operations and
;;; allocated at every step. Here such an expression is computed in an int64
;;; holding the value's bits and boxed once at the end (Runtime.BoxU64).
;;;
;;; +, -, *, LOGAND, LOGIOR, LOGXOR, LOGNOT and a left shift are exact modulo
;;; 2^64 on any integers, so an operand under them only needs its low 64 bits
;;; (Runtime.Low64), whatever its declared type: a declaration that does not
;;; hold changes nothing. A right shift reads the high bits, so its operand has
;;; to be known to be in [0, 2^64): a mask, a proven range, or a value checked
;;; at run time (Runtime.IsU64), with the expression computed on the generic
;;; path from the same operand values when the check fails.
;;; ------------------------------------------------------------

(defparameter +u64-mask+ (1- (expt 2 64)))

(defvar *u64-leaves* nil
  "Operands of the expression being planned, in evaluation order: entries
   (LOCAL KIND EXPR GUARD [CODE]), KIND :long (computed as an exact int64), :bits
   (an element of an (unsigned-byte 64) array or local read as its bits, or an
   inline call whose CODE leaves its bits) or :obj
   (computed boxed). GUARD is true for an :obj operand that has to be checked
   to lie in [0, 2^64), and :CHECK for an argument declared (unsigned-byte 64),
   which signals a TYPE-ERROR when it does not.")

(defun %u64-op-p (expr op)
  (and (consp expr) (eq (car expr) op)
       (not (local-function-entry op))
       (not (macrolet-shadowed-p op))))

(defvar *u64-subst* nil
  "Parameters of the inline expansions being planned through, innermost first:
   entries (NAME PLAN EXACT-P). A reference to NAME in the expansion's body
   stands for PLAN, the plan of its argument; EXACT-P is true when that value
   is known to lie in [0, 2^64), not only its low 64 bits.")

(defvar *u64-in-inline-body* nil
  "True while planning the body of an inline expansion. No operand may be taken
   out of it: an operand is evaluated before the whole expression, outside the
   BLOCK and the parameter bindings the body is written against.")

(defvar *u64-mod-operand* nil
  "True while planning the operand of (MOD X 2^K). +, - and * of non-integers
   are valid there, so an operand taken by its low 64 bits is first checked to
   be an integer (GUARD :INT), the generic path computing the rest.")

(defvar *u64-mod-param-refs* nil
  "Parameters of the inline expansion being planned that its body reads under
   *U64-MOD-OPERAND*.")

(defun %u64-leaf (expr kind guard)
  (when (and (eq kind :obj) (null guard) *u64-mod-operand*)
    (setf guard :int))
  (unless *u64-in-inline-body*
    (let ((l (gen-local (if (eq kind :long) "U64L" "U64O"))))
      (push (list l kind expr guard) *u64-leaves*)
      (list :leaf l kind))))

(defvar *u64-structured-probing* nil
  "True while COMPILE-AS-U64-STRUCTURED is finding out whether an expansion's
   last form is exact in the lane. The code compiled for that is dropped.")

(defvar *u64-structured-nontail* nil
  "True while a probe compiles the parts of an expansion before its last form.
   Whether the probe succeeds depends only on the last form, so the operands
   inside those parts are not tried as structured ones there: each such try
   compiles its own expansion once more, which doubles the work at every
   level of nested inline calls.")

(defvar *u64-structured-skipped* nil
  "Set when a probe passed over an operand as described for
   *U64-STRUCTURED-NONTAIL*. A probe that passed over none compiled what the
   real compile would, and its code is kept.")

(defvar *u64-plan-root* nil
  "The expression a U64-PLAN-COMPOUND caller asked about. That caller rejects a
   plan that is a single operand, so the root is not compiled as a structured
   operand: whatever that gave, the plan would be one operand.")

(defun u64-plan-compound (expr need)
  "U64-PLAN for a caller that only uses a plan doing something to its operands
   (one that is not a single :LEAF)."
  (let ((*u64-plan-root* expr))
    (u64-plan expr need)))

(defun %u64-structured-leaf (expr)
  "A :bits operand for EXPR, an inline call compiled by
   COMPILE-AS-U64-STRUCTURED, or NIL."
  (when (and *u64-structured-nontail* (not *u64-in-inline-body*))
    (setf *u64-structured-skipped* t))
  (unless (or *u64-in-inline-body* *u64-structured-nontail*)
    (let ((code (let ((*u64-mod-operand* nil)) (compile-as-u64-structured expr))))
      (when code
        (let ((l (gen-local "U64O")))
          (push (list l :bits expr nil code) *u64-leaves*)
          (list :leaf l :bits))))))

(defun %u64-const-value (expr)
  "The integer EXPR stands for when it is a literal, a constant, a parameter
   bound to one, or a sum or difference of those; else NIL."
  (let ((expr (fold-constant-symbol expr)))
    (cond ((integerp expr) expr)
          ((and (symbolp expr) expr)
           (let ((e (assoc expr *u64-subst*)))
             (and e (eq (car (second e)) :const) (second (second e)))))
          ((and (consp expr) (member (car expr) '(+ -))
                (not (local-function-entry (car expr)))
                (not (macrolet-shadowed-p (car expr)))
                (<= 2 (length expr) 3))
           (let ((vals (mapcar #'%u64-const-value (cdr expr))))
             (and (every #'integerp vals) (apply (car expr) vals))))
          (t nil))))

(defun %u64-inline-expansion (expr)
  "The expansion (LET ((param arg) ...) decls... (BLOCK name body)) of EXPR, a
   call to a function proclaimed INLINE, as (BINDINGS NAME BODY DECLS), when it can be
   planned through: one body form, no RETURN-FROM to the BLOCK, no SPECIAL
   binding. Else NIL."
  (let ((op (and (consp expr) (car expr))))
    (when (and (symbolp op) op
               (gethash op *inline-defs*)
               (not (special-operator-name-p op))
               (not (gethash op *compile-form-handlers*))
               (not (local-function-entry op))
               (not (macrolet-shadowed-p op))
               (not (find-macro-expander op))
               (not (maybe-expand-compiler-macro op expr)))
      (let ((inl (maybe-expand-inline op expr)))
        (when (and (consp inl) (eq (car inl) 'let) (listp (cadr inl)))
          (let* ((bindings (cadr inl))
                 (forms (cddr inl))
                 (blk (car (last forms))))
            (when (and (%boolean-branchable-let-p bindings forms)
                       (every (lambda (b) (and (consp b) (consp (cdr b)) (null (cddr b))))
                              bindings)
                       (every (lambda (f) (and (consp f) (eq (car f) 'declare)))
                              (butlast forms))
                       (consp blk) (eq (car blk) 'block)
                       (consp (cdr blk)) (symbolp (cadr blk))
                       (consp (cddr blk)) (null (cdddr blk))
                       (not (%block-returns-to-p (cadr blk) (cddr blk))))
              (list bindings (cadr blk) (caddr blk) (butlast forms)))))))))

(defun %u64-checked-params (decls)
  "Names of the parameters DECLS declare (unsigned-byte 64) whose arguments are
   checked: all of them unless the safety the function declares, or else the
   one in force where it is expanded, is 0."
  (let ((safety nil))
    (dolist (f decls)
      (dolist (d (cdr f))
        (when (and (consp d) (eq (car d) 'optimize))
          (dolist (q (cdr d))
            (cond ((eq q 'safety) (setf safety 3))
                  ((and (consp q) (eq (car q) 'safety) (integerp (cadr q)))
                   (setf safety (cadr q))))))))
    (when (if safety (> safety 0) (not (compiling-at-safety-0-p)))
      (extract-u64-locals decls))))

(defun %u64-inline-low-bits-only-p (bindings name body need checked)
  "True when the BODY of an inline expansion plans with every parameter that
   is not a constant or checked (CHECKED) known only by its low 64 bits. The
   body takes no operand, so this compiles nothing."
  (let ((*u64-mod-param-refs* nil)
        (*u64-subst*
          (append (loop for b in bindings
                        for c = (%u64-const-value (second b))
                        collect (if c
                                    (list (car b) (list :const c) (<= 0 c +u64-mask+))
                                    (list (car b) (list :leaf (make-symbol "PROBE") :obj)
                                          (and (member (var-name (car b)) checked :test #'string=)
                                               t))))
                  *u64-subst*))
        (*u64-in-inline-body* t)
        (*inlining-stack* (inline-guard-stack name)))
    (and (u64-plan body need) t)))

(defun %u64-mark-int-leaves (new-leaves old-leaves)
  "Check the operands taken by their low 64 bits in NEW-LEAVES (those not in
   OLD-LEAVES) to be integers: the argument they were taken for is read under
   MOD."
  (loop for l on new-leaves
        until (eq l old-leaves)
        do (let ((leaf (car l)))
             (when (and (eq (second leaf) :obj) (null (fourth leaf)))
               (setf (fourth leaf) :int)))))

(defun u64-plan-inline (expansion need)
  "Plan an inline call through its EXPANSION (from %U64-INLINE-EXPANSION). Each
   argument is planned where it is, in order (so its operands are taken out
   like any other), and a reference to the parameter in the body stands for
   that plan. The body is pure arithmetic in the lane or the whole plan fails:
   it is then compiled as the call it is. A parameter used twice computes its
   argument's arithmetic twice, from the same operand values.

   When the body reads only the low 64 bits of its parameters ((MOD N 2^64),
   (LDB (BYTE 64 0) (+ A B))), each argument is planned for its low 64 bits,
   so an argument that is itself modular arithmetic stays in the lane.

   Otherwise an argument that is only an object computed boxed (a call, a
   variable that is not a word) is first taken by its low 64 bits, and only
   when the body needs the exact value of such a parameter is the body planned
   again with the argument checked to be an (unsigned-byte 64). The arguments
   are not planned again: planning one can compile its code
   (%U64-STRUCTURED-LEAF), and a body planned in the lane takes no operand."
  (destructuring-bind (bindings name body decls) expansion
    (let* ((saved *u64-leaves*)
           (entries nil)
           (low-bits '())
           (ranges '())
           (checked (%u64-checked-params decls))
           (low-only (and (not *u64-in-inline-body*)
                          (%u64-inline-low-bits-only-p bindings name body need checked))))
      (dolist (b bindings)
        (let* ((init (second b))
               (c (%u64-const-value init))
               (check (member (var-name (car b)) checked :test #'string=))
               (before *u64-leaves*))
          (push (cond
                  ((and c check (not (<= 0 c +u64-mask+)))
                   (setf *u64-leaves* saved)
                   (return-from u64-plan-inline nil))
                  (c (list (car b) (list :const c) (<= 0 c +u64-mask+)))
                  ((and low-only (not check))
                   (let ((p (u64-plan init :mod)))
                     (unless p
                       (setf *u64-leaves* saved)
                       (return-from u64-plan-inline nil))
                     (list (car b) p nil)))
                  (t (let ((p (let ((s *u64-leaves*))
                                (or (u64-plan init :exact)
                                    (progn (setf *u64-leaves* s) nil)))))
                       (cond
                         ((and p (eq (car p) :leaf) (eq (third p) :obj)
                               (eq (fourth (assoc (second p) *u64-leaves*)) t))
                          (let ((leaf (assoc (second p) *u64-leaves*))
                                (entry (list (car b) p nil)))
                            (cond (check
                                   (setf (fourth leaf) :check
                                         (third entry) t))
                                  (t
                                   (setf (fourth leaf) nil)
                                   (push (cons entry leaf) low-bits)))
                            entry))
                         (p (list (car b) p t))
                         (check
                          (setf *u64-leaves* saved)
                          (return-from u64-plan-inline nil))
                         (t
                          (let ((p (u64-plan init :mod)))
                            (unless p
                              (setf *u64-leaves* saved)
                              (return-from u64-plan-inline nil))
                            (list (car b) p nil)))))))
                entries)
          (push (list (car b) *u64-leaves* before) ranges)))
      (setf entries (nreverse entries))
      (let ((refs nil))
        (flet ((plan-body ()
                 (let ((*u64-subst* (append entries *u64-subst*))
                       (*u64-in-inline-body* t)
                       (*u64-mod-param-refs* nil)
                       (*inlining-stack* (inline-guard-stack name)))
                   (prog1 (u64-plan body need)
                     (setf refs *u64-mod-param-refs*)))))
          (let ((plan (or (plan-body)
                          (and low-bits
                               (progn
                                 ;; The body reads the exact value: check those arguments.
                                 (loop for (entry . leaf) in low-bits
                                       do (setf (third entry) t
                                                (fourth leaf) t))
                                 (plan-body))))))
            (cond (plan
                   (loop for (param new old) in ranges
                         when (member param refs)
                           do (%u64-mark-int-leaves new old))
                   plan)
                  (t (setf *u64-leaves* saved) nil))))))))

(defun u64-plan (expr need)
  "A plan for computing EXPR in the 64-bit lane, or NIL. NEED is :exact (the
   value itself must lie in [0, 2^64)) or :mod (its low 64 bits will do).
   Records the operands in *U64-LEAVES*."
  (let ((expr (fold-nary-arith (%spec-strip expr)))
        (inl nil))
    (cond
      ((integerp expr)
       (and (or (eq need :mod) (<= 0 expr +u64-mask+))
            (list :const expr)))
      ((and (symbolp expr) expr (assoc expr *u64-subst*))
       (let ((e (assoc expr *u64-subst*)))
         (when *u64-mod-operand*
           (push (car e) *u64-mod-param-refs*))
         (and (or (eq need :mod) (third e))
              (second e))))
      ;; (MOD X 2^K), K <= 64: the low K bits of X, whatever X's sign.
      ((and (%u64-op-p expr 'mod) (= (length expr) 3)
            (let ((m (%u64-const-value (caddr expr))))
              (and m (> m 1) (= (logcount m) 1) (<= (integer-length m) 65))))
       (let ((a (let ((*u64-mod-operand* t))
                  (u64-plan (cadr expr) :mod))))
         (and a (list :mod a (1- (integer-length (%u64-const-value (caddr expr))))))))
      ;; An element of an (unsigned-byte 64) array, or a local whose slot holds
      ;; one: its bits, already exact.
      ((and (consp expr) (not *u64-in-inline-body*) (numeric-array-aref-u64-p expr))
       (%u64-leaf expr :bits nil))
      ((and (symbolp expr) expr (not *u64-in-inline-body*) (u64-native-local-p expr))
       (%u64-leaf expr :bits nil))
      ;; A 64-bit word of adjacent octets: its bits, from one wide read.
      ((and (consp expr) (not *u64-in-inline-body*)
            (let ((ow (octet-word-ref-info expr))) (and ow (= (fourth ow) 8))))
       (%u64-leaf expr :bits nil))
      ((and (consp expr) (not *u64-in-inline-body*) (fixnum-exact-p expr))
       (let ((r (expr-int-range expr)))
         (and (or (eq need :mod) (and r (>= (car r) 0)))
              (%u64-leaf expr :long nil))))
      ((and (symbolp expr) expr (not (keywordp expr)) (fixnum-exact-p expr))
       (let ((r (expr-int-range expr)))
         (and (or (eq need :mod) (and r (>= (car r) 0)))
              (%u64-leaf expr :long nil))))
      ;; (LOGANDC1 A B) is (LOGAND (LOGNOT A) B), and LOGANDC2 the mirror image:
      ;; exact when the operand that is not complemented is. Written that way
      ;; only for the plan; when it fails, the call itself is the operand.
      ((and (or (%u64-op-p expr 'logandc1) (%u64-op-p expr 'logandc2))
            (= (length expr) 3)
            (not (local-function-entry 'logand)) (not (macrolet-shadowed-p 'logand))
            (not (local-function-entry 'lognot)) (not (macrolet-shadowed-p 'lognot)))
       (let* ((saved *u64-leaves*)
              (p (u64-plan (if (eq (car expr) 'logandc1)
                               `(logand (lognot ,(cadr expr)) ,(caddr expr))
                               `(logand ,(cadr expr) (lognot ,(caddr expr))))
                           need)))
         (or p
             (progn (setf *u64-leaves* saved)
                    (%u64-leaf expr :obj (eq need :exact))))))
      ((or (%u64-op-p expr 'logand) (%u64-op-p expr 'logior) (%u64-op-p expr 'logxor))
       (and (= (length expr) 3)
            (let* ((op (car expr)) (a (cadr expr)) (b (caddr expr))
                   (mask-b (and (integerp b) (<= 0 b +u64-mask+))))
              (u64-plan-binary op a b
                               (cond ((eq need :mod) '(:mod :mod))
                                     ((not (eq op 'logand)) '(:exact :exact))
                                     (mask-b '(:mod :exact))
                                     ;; LOGAND of anything with a value in
                                     ;; [0, 2^64) stays in it.
                                     (t '(:mod :exact :or :exact :mod)))))))
      ((and (eq need :mod)
            (or (%u64-op-p expr '+) (%u64-op-p expr '-) (%u64-op-p expr '*))
            (= (length expr) 3))
       (u64-plan-binary (car expr) (cadr expr) (caddr expr) '(:mod :mod)))
      ((and (eq need :mod) (or (%u64-op-p expr '1+) (%u64-op-p expr '1-))
            (= (length expr) 2))
       (let ((a (u64-plan (cadr expr) :mod)))
         (and a (list (if (eq (car expr) '1+) :add :sub) a (list :const 1)))))
      ((and (eq need :mod) (%u64-op-p expr 'lognot) (= (length expr) 2))
       (let ((a (u64-plan (cadr expr) :mod)))
         (and a (list :not a))))
      ((and (%u64-op-p expr 'ash) (= (length expr) 3))
       (let ((x (cadr expr)) (k (or (%u64-const-value (caddr expr)) (caddr expr))))
         (cond
           ((and (integerp k) (<= 0 k 63) (eq need :mod))
            (let ((a (u64-plan x :mod))) (and a (list :shl a k))))
           ;; A left shift is exact when its operand is known to stay below
           ;; 2^(64-K): (ash (logand hi #xffffffff) 32), the high half of a
           ;; word assembled from two 32-bit ones.
           ((and (integerp k) (<= 0 k 63))
            (let* ((saved *u64-leaves*)
                   (a (u64-plan x :exact))
                   (b (and a (%u64-plan-bound a))))
              (if (and b (<= (ash b k) +u64-mask+))
                  (list :shl a k)
                  (progn (setf *u64-leaves* saved) nil))))
           ((and (integerp k) (< k 0))
            ;; The node keeps the real count: only the 64-bit path may cap it
            ;; (anything from 64 up gives 0 there). The generic path, taken when
            ;; X is outside [0, 2^64), shifts the bignum by the count as written.
            (let ((a (u64-plan x :exact))) (and a (list :shr a (- k)))))
           ((integerp k) nil)
           (*u64-in-inline-body* nil)
           ((%ash-count-range-within k 0 64)
            (and (eq need :mod)
                 (let ((a (u64-plan x :mod)))
                   (and a (list :shlv a (%u64-leaf k :long nil))))))
           ((%ash-count-range-within k -64 0)
            (let ((a (u64-plan x :exact)))
              (and a (list :shrv a (%u64-leaf k :long nil)))))
           (t nil))))
      ((and (eql (%rotate-byte-width expr) 64) (not *u64-in-inline-body*)
            (fixnum-exact-p (second expr)))
       (let ((c (%u64-leaf (second expr) :long nil))
             (a (u64-plan (fourth expr) :exact)))
         (and c a (list :rotl a c))))
      ;; A structure slot declared (unsigned-byte 64): its bits.
      ((and (consp expr) (not *u64-in-inline-body*) (%u64-struct-slot-ref expr))
       (let ((ref (%u64-struct-slot-ref expr))
             (l (gen-local "U64O")))
         (push (list l :bits expr nil
                     `(,@(compile-struct-object (car ref) (struct-slot-ref-type expr))
                       (:ldc-i4 ,(cdr ref))
                       (:call "Runtime.StructRefU64")))
               *u64-leaves*)
         (list :leaf l :bits)))
      ;; A call to an INLINE function whose body is arithmetic in the lane:
      ;; planned through, so a chain of such calls is one expression boxed once
      ;; instead of one box per call.
      ;; One that cannot (its body reads an array through its own bindings)
      ;; is still an exact operand when its expansion computes the word in the
      ;; lane: its bits, compiled now in the body's own scope.
      ((and (consp expr) (setq inl (%u64-inline-expansion expr)))
       (or (u64-plan-inline inl need)
           (and (not (eq expr *u64-plan-root*))
                (%u64-structured-leaf expr))
           (%u64-leaf expr :obj (eq need :exact))))
      ;; A substituted local function call: its bits, when its body computes
      ;; the word in the lane.
      ((and (consp expr) (not *u64-in-inline-body*) (not (eq expr *u64-plan-root*))
            (%u64-local-inline-call-p expr)
            (%u64-structured-leaf expr)))
      ;; Any other operand is computed as it is and taken apart here.
      ((%literal-ldb-form-p expr) nil)
      ((and (symbolp expr) (or (null expr) (eq expr t) (keywordp expr))) nil)
      (t (%u64-leaf expr :obj (eq need :exact))))))

(defun %u64-struct-slot-ref (expr)
  "(OBJ . INDEX) when EXPR is a call to the accessor of a structure slot
   declared (unsigned-byte 64), else NIL."
  (and (symbolp (car expr))
       (boundp '*struct-accessor-types*)
       (let ((ty (gethash (car expr) *struct-accessor-types*)))
         (and ty (%u64-value-type-p ty)))
       (struct-slot-ref expr)))

(defun %u64-plan-bound (node)
  "An upper bound on the value of NODE, a plan asked for :exact, known to lie
   in [0, bound]; NIL when none is known beyond [0, 2^64)."
  (ecase (car node)
    (:const (and (<= 0 (second node) +u64-mask+) (second node)))
    (:leaf (let ((e (assoc (second node) *u64-leaves*)))
             (and e (eq (second e) :long)
                  (let ((r (expr-int-range (third e))))
                    (and r (>= (car r) 0) (cdr r))))))
    (:and (let ((ba (%u64-plan-bound (second node)))
                (bb (%u64-plan-bound (third node))))
            ;; LOGAND with a value in [0, B] stays in [0, B], whatever the
            ;; other operand is.
            (cond ((and ba bb) (min ba bb)) (t (or ba bb)))))
    ((:or :xor) (let ((ba (%u64-plan-bound (second node)))
                      (bb (%u64-plan-bound (third node))))
                  (and ba bb
                       (1- (ash 1 (max (integer-length ba) (integer-length bb)))))))
    (:shl (let ((ba (%u64-plan-bound (second node))))
            (and ba (ash ba (third node)))))
    (:shr (ash (or (%u64-plan-bound (second node)) +u64-mask+)
               (- (min 64 (third node)))))
    (:mod (1- (ash 1 (third node))))
    ((:add :sub :mul :not :shlv :shrv :rotl) nil)))

(defun u64-plan-binary (op a b needs)
  "Plan (OP A B) with A and B asked for (FIRST NEEDS) and (SECOND NEEDS); when
   NEEDS has a second pair after :OR, that is tried if the first fails."
  (let ((saved *u64-leaves*))
    (or (let ((pa (u64-plan a (first needs))))
          (and pa
               (let ((pb (u64-plan b (second needs))))
                 (and pb (list (ecase op
                                 (logand :and) (logior :or) (logxor :xor)
                                 (+ :add) (- :sub) (* :mul))
                               pa pb)))))
        (progn (setf *u64-leaves* saved)
               (and (eq (third needs) :or)
                    (u64-plan-binary op a b (cdddr needs)))))))

(defun %u64-plan-worth-p (plan)
  "True when PLAN reads an (unsigned-byte 64) array element or local, or does
   something the generic path would do
   in bignums: arithmetic, a shift, or a 64-bit wide constant. LOGAND / LOGIOR /
   LOGXOR of boxed fixnums with narrow constants are cheaper on the generic
   entries (inlined fixnum cases) than unpacking and checking each operand."
  (labels ((wide-p (node)
             (case (car node)
               (:const (> (abs (second node)) most-positive-fixnum))
               (:leaf nil)
               ((:add :sub :mul :shl :shlv :shr :shrv :rotl) t)
               (:mod (wide-p (second node)))
               (t (some #'wide-p (cdr node))))))
    (and (consp plan) (not (eq (car plan) :leaf)) (not (eq (car plan) :const))
         ;; An element of an (unsigned-byte 64) array or local is boxed (a
         ;; bignum at or above 2^63) on the generic path, whatever is done
         ;; with it. Otherwise the plan has to do something that may leave
         ;; the fixnum range (a word assembled from two 32-bit halves): the
         ;; generic path computes it in bignums.
         (or (some (lambda (l) (eq (second l) :bits)) *u64-leaves*)
             (wide-p plan)))))

(defun %u64-all-ones-p (node)
  "True when NODE is the constant with all 64 bits set, which LOGAND with an
   int64 leaves unchanged."
  (and (eq (car node) :const) (= (ldb (byte 64 0) (second node)) +u64-mask+)))

(defun %u64-fast (node)
  (ecase (car node)
    (:const (let ((v (ldb (byte 64 0) (second node))))
              `((:ldc-i8 ,(if (logbitp 63 v) (- v (expt 2 64)) v)))))
    ;; (logand x #xFFFFFFFFFFFFFFFF), the mask of every 64-bit modular sum, is
    ;; x itself in the 64-bit lane.
    (:and (cond ((%u64-all-ones-p (third node)) (%u64-fast (second node)))
                ((%u64-all-ones-p (second node)) (%u64-fast (third node)))
                (t `(,@(%u64-fast (second node)) ,@(%u64-fast (third node)) (:and)))))
    (:leaf (if (member (third node) '(:long :bits))
               `((:ldloc ,(second node)))
               `((:ldloc ,(second node)) (:call "Runtime.Low64"))))
    ((:or :xor :add :sub :mul)
     `(,@(%u64-fast (second node)) ,@(%u64-fast (third node)) (,(car node))))
    (:not `(,@(%u64-fast (second node)) (:not)))
    (:mod (if (= (third node) 64)
              (%u64-fast (second node))
              `(,@(%u64-fast (second node)) (:ldc-i8 ,(1- (ash 1 (third node)))) (:and))))
    (:shl `(,@(%u64-fast (second node)) (:ldc-i4 ,(third node)) (:shl)))
    (:shlv `(,@(%u64-fast (second node)) (:ldloc ,(second (third node))) (:call "Runtime.ShlU64")))
    (:shr `(,@(%u64-fast (second node)) (:ldc-i8 ,(min 64 (third node))) (:call "Runtime.ShrU64")))
    (:shrv `(,@(%u64-fast (second node)) (:ldloc ,(second (third node))) (:neg)
             (:call "Runtime.ShrU64")))
    (:rotl `(,@(%u64-fast (second node)) (:ldloc ,(second (third node)))
             (:call "Runtime.RotateLeft64")))))

(defun %u64-generic (node)
  "NODE computed on the generic path from the same operand values."
  (ecase (car node)
    (:const (let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (second node))))
    (:leaf (case (third node)
             (:long `((:ldloc ,(second node)) (:call "Fixnum.Make")))
             (:bits `((:ldloc ,(second node)) (:call "Runtime.BoxU64")))
             (t `((:ldloc ,(second node))))))
    ((:and :or :xor :add :sub :mul)
     `(,@(%u64-generic (second node)) ,@(%u64-generic (third node))
       (:call ,(ecase (car node)
                 (:and "Runtime.Logand2") (:or "Runtime.Logior2") (:xor "Runtime.Logxor2")
                 (:add "Runtime.Add") (:sub "Runtime.Subtract") (:mul "Runtime.Multiply")))))
    (:not `(,@(%u64-generic (second node)) (:call "Runtime.Lognot")))
    (:mod `(,@(%u64-generic (second node))
            ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                (compile-expr (ash 1 (third node))))
            (:call "Runtime.Mod")))
    (:shl `(,@(%u64-generic (second node)) (:ldc-i8 ,(third node)) (:call "Fixnum.Make")
            (:call "Runtime.Ash")))
    (:shlv `(,@(%u64-generic (second node)) (:ldloc ,(second (third node))) (:call "Fixnum.Make")
             (:call "Runtime.Ash")))
    (:shr `(,@(%u64-generic (second node))
            ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                (compile-expr (- (third node))))
            (:call "Runtime.Ash")))
    (:shrv `(,@(%u64-generic (second node)) (:ldloc ,(second (third node))) (:call "Fixnum.Make")
             (:call "Runtime.Ash")))
    (:rotl `((:ldloc ,(second (third node))) (:call "Fixnum.Make")
             ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                 (compile-expr ''(64 . 0)))
             ,@(%u64-generic (second node))
             (:call "Runtime.RotateByte")))))

(defun %u64-pure-leaf-p (leaf)
  "True when evaluating LEAF's operand cannot assign a variable: a variable that
   is not a symbol macro, an integer, or an AREF whose operands are those."
  (destructuring-bind (l kind x &optional guard code) leaf
    (declare (ignore l kind guard))
    (flet ((simple-p (y)
             (or (integerp y)
                 (and (symbolp y) y
                      (not (nth-value 1 (lookup-symbol-macro y)))))))
      (and (null code)
           (or (simple-p x)
               (and (consp x) (eq (car x) 'aref)
                    (not (local-function-entry 'aref))
                    (not (macrolet-shadowed-p 'aref))
                    (every #'simple-p (cdr x))))))))

(defun %u64-alias-leaf-copies (code leaves)
  "CODE, which begins with (%U64-LEAF-CODE LEAVES), without the copy of an
   (unsigned-byte 64) local into a :bits operand's own local: the operand's
   reads read the variable's slot instead. The copy keeps the value an operand
   had when it was evaluated, which only matters when a later operand assigns
   the variable; so it goes only when every operand is a plain read
   (%U64-PURE-LEAF-P). Each copy costs an IL local and two local references,
   and an unrolled 64-bit body (ironclad's THREEFISH1024) had 3,114 of them,
   past the JIT's 8,000 references."
  (if (notevery #'%u64-pure-leaf-p leaves)
      code
      (let ((alias '()))
        (dolist (leaf leaves)
          (destructuring-bind (l kind x &optional guard leaf-code) leaf
            (declare (ignore guard))
            (when (and (eq kind :bits) (null leaf-code) (symbolp x))
              (let ((v (u64-native-local-p x)))
                (when v (push (cons l v) alias))))))
        (if (null alias)
            code
            (labels ((copy-p (i next)
                       ;; (:declare-local L ..) / (:ldloc V) / (:stloc L) of an alias
                       (declare (ignore next))
                       (and (consp i) (eq (car i) :declare-local)
                            (assoc (cadr i) alias)))
                     (subst-ldloc (i)
                       (cond ((and (consp i) (eq (car i) :ldloc) (assoc (cadr i) alias))
                              (list :ldloc (cdr (assoc (cadr i) alias))))
                             (t i))))
              (let ((out '()))
                (loop while code
                      do (let ((i (pop code)))
                           (if (and (copy-p i nil)
                                    (equal (first code) (list :ldloc (cdr (assoc (cadr i) alias))))
                                    (equal (second code) (list :stloc (cadr i))))
                               (setf code (cddr code))
                               (push (subst-ldloc i) out))))
                (nreverse out)))))))

(defun %u64-leaf-code (leaves)
  "Evaluate the operands LEAVES (in order) into their locals: an int64 for a
   :long operand, the bits of the array element or local for a :bits one, the
   object otherwise."
  (loop for (l kind x guard code) in leaves
        append (ecase kind
                 (:long
                  `((:declare-local ,l "Int64")
                    ,@(compile-as-long x)
                    (:stloc ,l)))
                 (:bits
                  `((:declare-local ,l "Int64")
                    ,@(cond (code code)
                            ((symbolp x)
                             `((:ldloc ,(u64-native-local-p x))))
                            ((octet-word-ref-info x)
                             (compile-octet-word-bits (octet-word-ref-info x)))
                            (t (compile-u64-aref-bits (cadr x) (cddr x))))
                    (:stloc ,l)))
                 (:obj
                  `((:declare-local ,l "LispObject")
                    ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                        (compile-expr x))
                    ,@(when (eq guard :check) '((:call "Runtime.CheckU64Arg")))
                    (:stloc ,l))))))

(defun %u64-guarded-leaf-p (leaf)
  "True for an operand checked to lie in [0, 2^64) (or with GUARD :INT, to be
   an integer) with the generic path taken when it does not (a :CHECK operand
   signals a TYPE-ERROR instead)."
  (member (fourth leaf) '(t :int)))

(defun %u64-guard-code (leaf slow)
  `((:ldloc ,(first leaf))
    (:call ,(if (eq (fourth leaf) :int) "Runtime.IsTrueIntegerp" "Runtime.IsU64"))
    (:brfalse ,slow)))

(defun try-compile-u64 (expr)
  "EXPR compiled in the 64-bit lane, leaving the boxed value, or NIL."
  (let* ((*u64-leaves* nil)
         (plan (u64-plan-compound expr :exact)))
    (when (and plan (%u64-plan-worth-p plan))
      (let* ((leaves (reverse *u64-leaves*))
             (slow (gen-label "U64SLOW"))
             (done (gen-label "U64DONE"))
             (guards (remove-if-not #'%u64-guarded-leaf-p leaves)))
        (%u64-alias-leaf-copies
         `(,@(%u64-leaf-code leaves)
           ,@(loop for g in guards append (%u64-guard-code g slow))
           ,@(%u64-fast plan)
           (:call "Runtime.BoxU64")
           ,@(when guards
               `((:br ,done)
                 (:label ,slow)
                 ,@(%u64-generic plan)
                 (:label ,done))))
         leaves)))))

;;; Nested +, -, *: when every operand is a fixnum, the intermediates are int64
;;; with an overflow check and only the result is boxed; otherwise the generic calls.

(defvar *spec-leaves* nil)

(defun %spec-integer-type-p (type)
  (or (member type '(integer number rational real t))
      (and (consp type) (eq (car type) 'integer))))

(defun %spec-strip (expr)
  (loop
    (let ((e (strip-value-wrappers expr)))
      (cond ((and (consp e) (eq (car e) 'identity) (consp (cdr e)) (null (cddr e))
                  (not (local-function-entry 'identity)))
             (setf expr (cadr e)))
            ((and (consp e) (eq (car e) 'the) (consp (cdr e)) (consp (cddr e))
                  (null (cdddr e)) (%spec-integer-type-p (cadr e)))
             (setf expr (caddr e)))
            ((and (consp e) (member (car e) '(let let*)) (consp (cdr e))
                  (or (eq (car e) 'let) (and (consp (cadr e)) (null (cdr (cadr e))))))
             (let ((form (let-substitute-single-reads (cadr e) (cddr e))))
               (if form (setf expr form) (return e))))
            (t (return e))))))

(defun %spec-op (expr)
  "EXPR, a two-operand +, - or * (or 1+ / 1-), as (KIND A B); else NIL."
  (let ((e (%spec-strip expr)))
    (when (and (consp e) (symbolp (car e))
               (member (car e) '(+ - * 1+ 1-))
               (not (local-function-entry (car e)))
               (not (macrolet-shadowed-p (car e))))
      (let ((e (fold-nary-arith e)))
        (case (and (consp e) (car e))
          ((+ - *) (when (and (consp (cdr e)) (consp (cddr e)) (null (cdddr e)))
                     (list (ecase (car e) (+ :add) (- :sub) (* :mul)) (cadr e) (caddr e))))
          (1+ (when (and (consp (cdr e)) (null (cddr e))) (list :add (cadr e) 1)))
          (1- (when (and (consp (cdr e)) (null (cddr e))) (list :sub (cadr e) 1))))))))

(defun %spec-pure-p (expr)
  (or (integerp expr)
      (and (symbolp expr) expr (not (keywordp expr))
           (not (nth-value 1 (lookup-symbol-macro expr))))))

(defun %spec-plan (expr after-op root)
  "The tree of EXPR, or NIL. AFTER-OP: an operation precedes EXPR in evaluation
   order, so EXPR must not have side effects (the generic path would have
   signalled before evaluating it). ROOT: the caller checks FIXNUM-TYPED-P of
   EXPR itself, and only when the plan succeeds."
  (let ((op (let ((op (%spec-op expr)))
              (and op (or root (not (fixnum-typed-p expr))) op))))
    (if op
        (let* ((a (%spec-plan (second op) after-op nil))
               (b (and a (%spec-plan (third op) (or after-op (eq (car a) :node)) nil))))
          (and b (list :node (first op) a b)))
        (let ((x (%spec-strip expr)))
          (cond ((integerp x)
                 (and (<= +int64-min+ x +int64-max+) (list :const x)))
                ((or (numberp x) (characterp x) (stringp x)) nil)
                ((and after-op (not (%spec-pure-p x))) nil)
                ((> (length *spec-leaves*) 8) nil)
                ((or (double-float-typed-p x) (single-float-typed-p x) (decimal-typed-p x)) nil)
                (t (let ((l (gen-local "SPL"))
                         (kind (if (fixnum-typed-p x) :long :obj)))
                     (push (list l kind x) *spec-leaves*)
                     (list :leaf l kind))))))))

(defun %spec-load (node)
  (ecase (car node)
    (:const `((:ldc-i8 ,(second node))))
    (:leaf (if (eq (third node) :long)
               `((:ldloc ,(second node)))
               `((:ldloc ,(second node)) (:unbox-fixnum))))))

(defun %spec-fast (node slow root)
  "(values CODE LOAD): CODE computes NODE's intermediates into locals, LOAD
   pushes its int64 value; at ROOT, CODE leaves the boxed result."
  (if (not (eq (car node) :node))
      (values nil (%spec-load node))
      (multiple-value-bind (ca la) (%spec-fast (third node) slow nil)
        (multiple-value-bind (cb lb) (%spec-fast (fourth node) slow nil)
          (if root
              (values `(,@ca ,@cb ,@la ,@lb
                        (:call ,(ecase (second node)
                                  (:add "Runtime.AddFixnum") (:sub "Runtime.SubtractFixnum")
                                  (:mul "Runtime.MultiplyFixnum"))))
                      nil)
              (let ((tmp (gen-local "SPT")))
                (values `(,@ca ,@cb
                          (:declare-local ,tmp "Int64")
                          ,@la ,@lb
                          (:call ,(ecase (second node)
                                    (:add "Runtime.AddOrMin") (:sub "Runtime.SubtractOrMin")
                                    (:mul "Runtime.MultiplyOrMin")))
                          (:stloc ,tmp) (:ldloc ,tmp) (:ldc-i8 ,+int64-min+) (:beq ,slow))
                        `((:ldloc ,tmp)))))))))

(defun %spec-generic (node)
  (ecase (car node)
    (:const `((:ldc-i8 ,(second node)) (:call "Fixnum.Make")))
    (:leaf (if (eq (third node) :long)
               `((:ldloc ,(second node)) (:call "Fixnum.Make"))
               `((:ldloc ,(second node)))))
    (:node `(,@(%spec-generic (third node)) ,@(%spec-generic (fourth node))
             (:call ,(ecase (second node)
                       (:add "Runtime.Add") (:sub "Runtime.Subtract")
                       (:mul "Runtime.Multiply")))))))

(defun try-compile-spec-arith (expr)
  "EXPR, a +, - or * with another of them among its operands, computed on int64
   when its operands are fixnums (see above), leaving the boxed value; or NIL."
  (let* ((*spec-leaves* nil)
         (op (%spec-op expr))
         (plan (and op
                    (or (%spec-op (second op)) (%spec-op (third op)))
                    (let ((*int-type-memo* (or *int-type-memo* (make-hash-table :test 'eq))))
                      (%spec-plan expr nil t)))))
    (when (and plan (eq (car plan) :node)
               (or (eq (car (third plan)) :node) (eq (car (fourth plan)) :node))
               (<= (length *spec-leaves*) 8)
               (not (fixnum-typed-p expr)))
      (let ((leaves (reverse *spec-leaves*))
            (slow (gen-label "SPSLOW"))
            (done (gen-label "SPDONE")))
        `(,@(loop for (l kind x) in leaves
                  append `((:declare-local ,l ,(if (eq kind :long) "Int64" "LispObject"))
                           ,@(if (eq kind :long)
                                 (compile-as-long x)
                                 (let ((*in-tail-position* nil) (*in-mv-context* nil))
                                   (compile-expr x)))
                           (:stloc ,l)))
          ,@(loop for (l kind) in leaves
                  unless (eq kind :long)
                    append `((:ldloc ,l) (:call "Runtime.IsFixnumObject") (:brfalse ,slow)))
          ,@(%spec-fast plan slow t)
          (:br ,done)
          (:label ,slow)
          ,@(%spec-generic plan)
          (:label ,done))))))

(defun compile-guarded-long-bitop (fn a b)
  (let ((op (ecase fn (logand :and) (logior :or) (logxor :xor)))
        (method (ecase fn (logand "Runtime.Logand2") (logior "Runtime.Logior2")
                  (logxor "Runtime.Logxor2")))
        (slow (gen-label "BITSLOW"))
        (done (gen-label "BITDONE"))
        (parts nil))
    ;; PARTS: (LONG-P . LOCAL) per operand, in evaluation order.
    (let ((code
            (loop for x in (list a b)
                  append (if (fixnum-exact-p x)
                             (let ((l (gen-local "BITL")))
                               (push (cons t l) parts)
                               `((:declare-local ,l "Int64")
                                 ,@(compile-as-long x)
                                 (:stloc ,l)))
                             (let ((o (gen-local "BITO")))
                               (push (cons nil o) parts)
                               `((:declare-local ,o "LispObject")
                                 ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                                     (compile-expr x))
                                 (:stloc ,o)))))))
      (setf parts (nreverse parts))
      `(,@code
        ,@(loop for (long-p . l) in parts
                unless long-p
                  append `((:ldloc ,l) (:call "Runtime.IsFixnumObject") (:brfalse ,slow)))
        ,@(loop for (long-p . l) in parts
                append (if long-p `((:ldloc ,l)) `((:ldloc ,l) (:unbox-fixnum))))
        (,op)
        (:br ,done)
        (:label ,slow)
        ,@(loop for (long-p . l) in parts
                append (if long-p `((:ldloc ,l) (:call "Fixnum.Make")) `((:ldloc ,l))))
        (:call ,method)
        (:unbox-fixnum)
        (:label ,done)))))

(defun compile-long-binop (op a b)
  "Emit a native int64 binop. Each operand that is not straight-line code is
   evaluated into its own Int64 temp FIRST, so nothing of this expression is
   pending on the CIL stack while that operand's code runs.

   Leaving operand 1 on the stack across operand 2 is only valid if operand 2 is
   branch-free. It is not, for instance, when operand 2 is a call to a function
   proclaimed INLINE whose body is a COND: MAYBE-EXPAND-INLINE rewrites the call
   to (LET (...) (BLOCK f body)), the block's arms jump to a join label, and the
   pending operand makes the stack depth at that label disagree between paths.
   The result was IL the JIT rejects outright; real libraries hit it (fset's
   CHAMP nodes, where every accessor is an inlined COND used inside fixnum
   arithmetic), and the failure surfaces only when the method is first CALLED,
   because JIT is lazy."
  (with-long-simple-memo
  (if (and (%long-operand-simple-p a) (%long-operand-simple-p b))
      (let* ((ca (%long-code-own (compile-as-long a)))
             (cb (%long-code-own (compile-as-long b))))
        (%long-code-join (list ca cb (%long-code-fresh (list (list op))))))
      (let* ((ta (gen-local "LBA")) (tb (gen-local "LBB"))
             (head (%long-code-fresh (list (list :declare-local ta "Int64")
                                           (list :declare-local tb "Int64"))))
             (ca (%long-code-own (compile-as-long a)))
             (cb (%long-code-own (compile-as-long b))))
        (%long-code-join
         (list head
               ca
               (%long-code-fresh (list (list :stloc ta)))
               cb
               (%long-code-fresh (list (list :stloc tb) (list :ldloc ta)
                                       (list :ldloc tb) (list op)))))))))

(defvar *long-code-owned* nil
  "(CODE . LAST-CONS) of the list the most recent COMPILE-LONG-BINOP returned.
   That list is freshly made and nothing else holds it, so the binop the
   result is an operand of links it into its own code instead of copying it.
   Copying made a nest of N levels cost N^2: each level copied the code of
   the whole operand below it.")

(defun %long-code-fresh (list)
  "LIST, freshly made by the caller, as (LIST . LAST-CONS) for %LONG-CODE-JOIN."
  (and list (cons list (last list))))

(defun %long-code-own (code)
  "CODE, just returned by COMPILE-AS-LONG, as (LIST . LAST-CONS) for
   %LONG-CODE-JOIN: CODE itself when it is the list the most recent
   COMPILE-LONG-BINOP returned (once only), a copy otherwise."
  (let ((o *long-code-owned*))
    (setf *long-code-owned* nil)
    (cond ((null code) nil)
          ((and o (eq code (car o))) o)
          (t (%long-code-fresh (copy-list code))))))

(defun %long-code-join (parts)
  "Concatenate PARTS, each NIL or a (LIST . LAST-CONS) the caller owns, by
   linking them, and record the result in *LONG-CODE-OWNED*."
  (let ((head nil) (tail nil))
    (dolist (p parts)
      (when p
        (if tail
            (setf (cdr tail) (car p))
            (setf head (car p)))
        (setf tail (cdr p))))
    (setf *long-code-owned* (and head (cons head tail)))
    head))

(defun compile-as-long (expr)
  "Compile EXPR leaving an int64 on the stack. Caller must have verified
   fixnum-typed-p; this routine assumes the invariant."
  (setf expr (fold-typed-operand expr))
  (cond
    ((integerp expr)
     `((:ldc-i8 ,expr)))
    ((%rotate-byte-32-long-p expr) (compile-rotate-byte-32-long expr))
    ((%u64-local-field expr)
     (destructuring-bind (key shift . mask) (%u64-local-field expr)
       `((:ldloc ,key)
         ,@(when (plusp shift) `((:ldc-i8 ,shift) (:call "Runtime.ShrU64")))
         (:ldc-i8 ,mask) (:and))))
    ;; Direct Int64 local in native body: already long, no unbox
    ((and (symbolp expr)
          (boundp '*cstate*)
          (native-slot-p expr (cstate-long-locals)))
     `((:ldloc ,(lookup-local expr))))
    ;; An (unsigned-byte 64) local holds the value's bits as a raw Int64, not a
    ;; boxed Fixnum, even when it is also known to be a fixnum (a FIXNUM
    ;; declaration, or a range inferred from its init). The caller has proven
    ;; the value lies in the fixnum range, where the bits are the value. Checked
    ;; before the boxed-local clauses below, which would unbox the raw slot.
    ((and (symbolp expr) (u64-native-local-p expr))
     `((:ldloc ,(u64-native-local-p expr))))
    ;; A call from a typed body to itself whose result is an integer: the raw
    ;; result of the body method, nothing boxed either way.
    ((and (typed-self-call-p expr)
          (eq (car (third (cstate-typed-self))) :long))
     (compile-typed-self-call expr))
    ;; Native self-call: long args avoid boxing; InvokeNativeN returns LispObject,
    ;; so unbox-fixnum extracts the long back for the caller.
    ((and (consp expr) (symbolp (car expr))
          (boundp '*cstate*) (cstate-native-self-name)
          (cstate-self-fn-local)
          (string= (mangle-name (car expr)) (cstate-native-self-name))
          (self-call-shortcut-p (car expr))
          (not (local-function-entry (car expr)))
          (let ((n (length (cdr expr)))) (and (>= n 1) (<= n 4)))
          ;; The parameters of a native self are all declared FIXNUM.
          (every #'fixnum-wrap-typed-p (cdr expr)))
     (let ((n-args (length (cdr expr))))
       `(,(if (eq (cstate-self-fn-local) :arg0) '(:ldarg 0)
              `(:ldloc ,(cstate-self-fn-local)))
         ;; APPEND, not MAPCAN. A backquoted instruction list whose tail is all
         ;; constant folds to one literal for that tail, so COMPILE-AS-LONG can
         ;; hand back a list whose last cons is a literal shared by every call
         ;; that took the same branch. MAPCAN would NCONC the next argument's
         ;; instructions onto it, and they would still be there the next time
         ;; that branch is taken -- carrying a local from a function compiled
         ;; earlier into a body that never declared it.
         ,@(loop for arg in (cdr expr)
                 append (let ((*in-tail-position* nil) (*in-mv-context* nil))
                          (compile-as-long arg)))
         (:callvirt ,(invoke-native-name n-args))
         (:unbox-fixnum))))
    ;; Declared-fixnum local: load slot (LispObject) then unbox.
    ;; A captured+mutated (boxed) local's slot holds the LispObject[1] cell,
    ;; not the Fixnum: unboxing it directly would castclass the cell array.
    ;; Boxed vars fall through to the fallback, whose compile-expr dereferences
    ;; the cell before the unbox.
    ((and (symbolp expr)
          (boundp '*fixnum-locals*)
          (member (var-name expr) *fixnum-locals* :test #'string=)
          (not (boxed-var-p expr))
          (lookup-local expr))
     `((:ldloc ,(lookup-local expr))
       (:unbox-fixnum)))
    ;; Small-int local (bounded range): slot holds a boxed Fixnum: load + unbox.
    ((small-int-local-range expr)
     `((:ldloc ,(lookup-local expr))
       (:unbox-fixnum)))
    ;; aref on a numeric-backed array local: raw long element read, no box.
    ((numeric-array-aref-info expr)
     (compile-numeric-aref-as-long
      (cadr expr) (cddr expr) (car (numeric-array-aref-info expr))))
    ;; (char-code (schar s i)): one typed call, no boxed index, no character.
    ((string-char-code-ref expr)
     (let ((ref (string-char-code-ref expr)))
       (compile-string-char-code-as-long (car ref) (cdr ref))))
    ;; A word of adjacent octets: one wide read. A 64-bit one is here only
    ;; under *FIXNUM-WRAP-LICENSED*, which asks for its low 64 bits.
    ((octet-word-ref-info expr)
     (compile-octet-word-bits (octet-word-ref-info expr)))
    ;; A structure slot read in a fixnum context: raw long out of the slot,
    ;; instead of the slot's LispObject through UnwrapMv and an unbox.
    ((struct-slot-ref expr)
     (let ((ref (struct-slot-ref expr)))
       (compile-struct-slot-as-long (car ref) (cdr ref) (struct-slot-ref-type expr))))
    ;; (if c a b), both arms fixnum-typed: branch with a raw int64 on each path
    ;; instead of boxing at the merge. The condition goes through the same
    ;; compile-boolean-branch the ordinary IF uses, so fused comparisons and
    ;; and/or/not chains behave identically here.
    ((and (consp expr) (eq (car expr) 'if) (= (length expr) 4)
          (fixnum-typed-p (caddr expr))
          (fixnum-typed-p (cadddr expr)))
     (let ((else-label (gen-label "LONGELSE"))
           (end-label (gen-label "LONGEND")))
       `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
             (compile-boolean-branch (cadr expr) else-label nil))
         ,@(compile-as-long (caddr expr))
         (:br ,end-label)
         (:label ,else-label)
         ,@(compile-as-long (cadddr expr))
         (:label ,end-label))))
    ((%fixnum-min-max-p expr)
     (compile-long-min-max expr))
    ((and (consp expr) (eq (car expr) 'the))
     ;; (the fixnum E): the declaration asserts E is a fixnum, so lower E
     ;; natively via compile-as-long: it handles +/-/*/1+/1-/locals as raw
     ;; int64 and falls back to compile-expr+unbox for forms it doesn't know.
     ;; Previously this always took compile-expr+unbox, forcing e.g.
     ;; (the fixnum (1- x)) through a box->Runtime.Decrement->unbox round-trip
     ;; (a method call) in hot fixnum loops like tak/stak.
     (compile-as-long (caddr expr)))
    ((and (consp expr) (= (length expr) 3) (member (car expr) '(+ - *)))
     (let ((op (ecase (car expr) (+ :add) (- :sub) (* :mul))))
       (compile-long-binop op (cadr expr) (caddr expr))))
    ;; Three or more arguments: emit the left-associated form the call stands for
    ;; (FIXNUM-TYPED-P accepts it on the same reading).
    ((and (consp expr) (member (car expr) '(+ - * logand logior logxor)) (> (length expr) 3))
     (compile-as-long (fold-nary-arith expr)))
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '1+))
     `(,@(compile-as-long (cadr expr))
       (:ldc-i8 1)
       (:add)))
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '1-))
     `(,@(compile-as-long (cadr expr))
       (:ldc-i8 1)
       (:sub)))
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '-))
     `(,@(compile-as-long (cadr expr))
       (:neg)))
    ;; %dotimes-1+: raw add: the emitting macro asserts the result fits int64
    ;; (the increment site is only reached while counter < limit <= int64-max).
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '%dotimes-1+))
     `(,@(compile-as-long (cadr expr))
       (:ldc-i8 1)
       (:add)))
    ;; %dotimes-limit / %loop-limit-floor: a loop limit clamped into int64 by
    ;; the runtime (ceiling / floor of a non-integer). A limit already known
    ;; to be a fixnum is itself.
    ((and (consp expr) (= (length expr) 2)
          (member (car expr) '(%dotimes-limit %loop-limit-floor)))
     (if (fixnum-typed-p (cadr expr))
         (compile-as-long (cadr expr))
         `(,@(compile-value-arg (cadr expr))
           (:call ,(if (eq (car expr) '%dotimes-limit)
                       "Runtime.DotimesLimit"
                       "Runtime.LoopLimitFloor")))))
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '%loop-by))
     `(,@(compile-as-long (cadr expr))
       (:call "Runtime.LoopByFixnum")))
    ;; mod/rem: leaves int64 on stack. The helpers carry CL's sign rules
    ;; (MOD follows the divisor, REM truncates); a raw CIL :rem would give REM's
    ;; answer for both.
    ;; The operands are re-checked for exactness because this can be reached
    ;; under THE or under the license, where nobody asked about them; a wrapped
    ;; operand takes the generic path through the fallback below.
    ;; (floor X d) / (truncate X d), constant d, first value. A positive power
    ;; of two is an arithmetic shift for FLOOR (it rounds toward -inf, as SHR
    ;; does) and for TRUNCATE of an X proven non-negative; otherwise the int64
    ;; helper. FIXNUM-TYPED-P admitted the form only with the quotient in
    ;; int64, so the helper's one overflow (most-negative / -1) cannot occur.
    ((%const-divisor-quotient-p expr)
     (let* ((x (cadr expr))
            (d (caddr expr))
            (k (and (> d 0) (= (logcount d) 1) (1- (integer-length d)))))
       (cond
         ((eql d 1) (compile-as-long x))
         ((and k (or (eq (car expr) 'floor)
                     (let ((r (expr-int-range x))) (and r (>= (car r) 0)))))
          `(,@(compile-as-long x)
            (:ldc-i4 ,k)
            (:shr)))
         (t
          `(,@(compile-long-binop-spilled x d)
            (:call ,(if (eq (car expr) 'floor)
                        "Runtime.FloorFixnumL" "Runtime.TruncateFixnumL")))))))
    ((float-round-int-range expr)
     (let ((kind (%float-round-arg-kind (car expr) (cdr expr))))
       `(,@(if (eq kind :single)
               `(,@(compile-as-single (cadr expr)) (:conv-r8))
               (compile-as-double (cadr expr)))
         (:call ,(%float-round-entry (car expr)))
         (:conv-i8))))
    ((and (consp expr) (= (length expr) 3) (member (car expr) '(mod rem))
          (fixnum-exact-p (cadr expr)) (fixnum-exact-p (caddr expr)))
     `(,@(compile-long-binop-spilled (cadr expr) (caddr expr))
       (:call ,(if (eq (car expr) 'mod) "Runtime.ModFixnumL" "Runtime.RemFixnumL"))))
    ;; Bitwise ops: leaves int64 on stack (callers box if needed).
    ;; Each operand has to be one COMPILE-AS-LONG can produce (its value, or
    ;; that modulo 2^64). Under (THE FIXNUM (LOGAND X #x3F)) only the result is
    ;; asserted: X may well be a bignum, and unboxing it unconditionally failed
    ;; with a .NET cast error. Such a form takes COMPILE-GUARDED-LONG-BITOP.
    ((and (consp expr) (= (length expr) 3) (member (car expr) '(logand logior logxor))
          (fixnum-wrap-typed-p (cadr expr)) (fixnum-wrap-typed-p (caddr expr)))
     (let ((op (ecase (car expr) (logand :and) (logior :or) (logxor :xor))))
       (compile-long-binop op (cadr expr) (caddr expr))))
    ((and (consp expr) (= (length expr) 3) (member (car expr) '(logand logior logxor)))
     (compile-guarded-long-bitop (car expr) (cadr expr) (caddr expr)))
    ((and (consp expr) (= (length expr) 2) (eq (car expr) 'lognot))
     `(,@(compile-as-long (cadr expr))
       (:not)))
    ;; ash with a variable count, admitted by FIXNUM-TYPED-P only with the
    ;; count's range inside [0, 63] (left) or [-63, 0] (right).
    ((and (consp expr) (= (length expr) 3) (eq (car expr) 'ash)
          (not (integerp (caddr expr)))
          (%ash-count-range-within (caddr expr) -63 0))
     `(,@(compile-long-binop-spilled (cadr expr) (caddr expr))
       (:neg)
       (:conv-i4)
       (:shr)))
    ((and (consp expr) (= (length expr) 3) (eq (car expr) 'ash)
          (not (integerp (caddr expr)))
          (%ash-count-range-within (caddr expr) 0 63))
     `(,@(compile-long-binop-spilled (cadr expr) (caddr expr))
       (:conv-i4)
       (:shl)))
    ((and (consp expr) (= (length expr) 3) (eq (car expr) 'ash)
          (not (integerp (caddr expr)))
          (%ash-count-range-within (caddr expr) -63 63)
          (fixnum-typed-p expr))
     `(,@(compile-long-binop-spilled (cadr expr) (caddr expr))
       (:call "Runtime.AshLong")))
    ;; ash with constant shift. fixnum-typed-p guarantees we only get here for a
    ;; negative shift (right shift, always safe) or a constant base whose folded
    ;; result fits int64: fold that to a literal so no overflowing SHL is emitted.
    ((and (consp expr) (= (length expr) 3) (eq (car expr) 'ash) (integerp (caddr expr))
          (or (>= (caddr expr) 0) (fixnum-exact-p (cadr expr))))
     (let ((n (caddr expr)) (x (cadr expr)))
       (cond
         ((< n 0)
          ;; Count capped at 63: CIL SHR takes it mod 64 (see COMPILE-ASH-FAST).
          `(,@(compile-as-long x)
            (:ldc-i4 ,(min 63 (- n)))
            (:shr)))
         ((integerp x)
          ;; Folded modulo 2^64: under the wrap license a constant may be
          ;; shifted past int64, and only the low 64 bits are asked for.
          (let ((v (ldb (byte 64 0) (ash x n))))
            `((:ldc-i8 ,(if (logbitp 63 v) (- v (ash 1 64)) v)))))
         (t
          ;; A left shift FIXNUM-TYPED-P admitted: exact by the range proof, or
          ;; wanted modulo 2^64 under the license. The count is below 64.
          `(,@(compile-as-long x)
            (:ldc-i4 ,n)
            (:shl))))))
    ;; A call to an inline function (or a binding form) whose value is an
    ;; integer the caller wants raw: its last form is compiled as an int64
    ;; instead of being boxed and unboxed again.
    ((compile-as-long-structured expr))
    ((let ((*in-tail-position* nil) (*in-mv-context* nil))
       (%funcall-aref-long-plan expr))
     (let ((*in-tail-position* nil) (*in-mv-context* nil))
       (compile-funcall-aref (cadr expr) (%funcall-aref-long-plan expr)
                             (%call-site-mv-mode (cdr expr)) t)))
    ((let ((*in-tail-position* nil) (*in-mv-context* nil))
       (%funcall-long-arg-expr-p expr))
     (let ((*in-tail-position* nil) (*in-mv-context* nil))
       (compile-funcall-long-arg (cadr expr) (caddr expr)
                                 (%call-site-mv-mode (cdr expr)) t)))
    (t
     (multiple-value-bind (code raw) (%compile-raw-typed-call expr :long)
       (cond (raw code)
             (code `(,@code (:unbox-fixnum)))
             (t `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                      (compile-expr expr))
                  (:unbox-fixnum))))))))

(defun checked-long-arith-p (expr)
  "True for +/-/* (two operands after folding) or 1+/1- whose operands can all be
   produced as raw int64. Such an expression can be computed natively with an
   overflow CHECK even when no range proof exists; the destination is an Int64
   slot, so a result that overflows has nowhere to go and must signal either way."
  (let ((expr (fold-nary-arith expr)))
    (and (consp expr)
         (or (and (member (car expr) '(+ - *)) (= (length expr) 3)
                  (fixnum-typed-p (cadr expr)) (fixnum-typed-p (caddr expr)))
             (and (member (car expr) '(floor truncate)) (= (length expr) 3)
                  (not (local-function-entry (car expr)))
                  (fixnum-exact-p (cadr expr)) (fixnum-exact-p (caddr expr)))
             (and (member (car expr) '(1+ 1-)) (= (length expr) 2)
                  (fixnum-typed-p (cadr expr)))))))

(defun fixnum-long-lowerable-p (expr)
  "True when COMPILE-EXPR-TO-LONG lowers EXPR natively and exactly: either
   FIXNUM-TYPED-P (exact), or top-level +, -, *, 1+, 1- of such operands, which
   it computes with an overflow check. For destinations that need an int64 and
   have no use for a bignum anyway -- an array subscript above all -- where an
   overflow signalling is as good as a generic call signalling later."
  (or (fixnum-typed-p expr) (checked-long-arith-p expr)))

(defun compile-checked-long-arith (expr)
  "EXPR as a raw int64, checking for overflow rather than proving it away."
  (let* ((expr (fold-nary-arith expr))
         (op (car expr))
         (helper (ecase op
                   ((+ 1+) "Runtime.AddFixnumChecked")
                   ((- 1-) "Runtime.SubtractFixnumChecked")
                   (* "Runtime.MultiplyFixnumChecked")
                   (floor "Runtime.FloorFixnumL")
                   (truncate "Runtime.TruncateFixnumL"))))
    (if (member op '(1+ 1-))
        `(,@(compile-as-long (cadr expr)) (:ldc-i8 1) (:call ,helper))
        `(,@(compile-long-binop-spilled (cadr expr) (caddr expr)) (:call ,helper)))))

(defun compile-expr-to-long (expr)
  "Compile EXPR leaving a raw int64 on the stack, without risking silent wrap:
   the raw long path (compile-as-long) is taken only when a value-range proof
   shows every intermediate fits int64. Failing that, arithmetic whose operands
   are themselves raw goes native with an overflow check; the range proof can
   never succeed for FIXNUM + FIXNUM (that spans one bit more than int64), so
   without this every (SETQ ACC (+ ACC X)) in a declared-fixnum loop boxed both
   operands, called the generic promoting ADD, and unboxed the result. Anything
   else is evaluated boxed on the generic promoting path and unboxed; a
   declaration-violating bignum then signals (InvalidCast) instead of wrapping.
   Used to initialize and assign Int64-slot locals."
  (cond
    ((fixnum-arith-unboxed-safe-p expr) (compile-as-long expr))
    ((checked-long-arith-p expr) (compile-checked-long-arith expr))
    (t `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
             (compile-expr expr))
         (:unbox-fixnum)))))

(defun %u64-masked-index-p (expr)
  "True when EXPR is (LOGAND X M), or an LDB the same as one, with M a literal
   in [0, 2^63): its value fits an int64 whatever integer X is."
  (let ((e (fold-typed-operand expr)))
    (and (consp e) (eq (car e) 'logand) (= (length e) 3)
         (not (local-function-entry 'logand))
         (flet ((mask-p (m) (and (integerp m) (<= 0 m +int64-max+))))
           (or (mask-p (cadr e)) (mask-p (caddr e)))))))

(defun compile-index-to-long (expr)
  "Compile an array subscript leaving a raw int64 on the stack. A fixnum-typed
   subscript lowers natively (COMPILE-EXPR-TO-LONG); anything else is evaluated
   boxed and converted by Runtime.IndexL, which reports a non-integer subscript
   the way AREF does; where a bare unbox would surface it as a .NET cast.
   The unboxed element paths used to demand a statically fixnum-typed subscript,
   but an element's storage does not depend on the subscript. Requiring it meant
   an undeclared loop variable, the usual shape, (DO ((I 0 (1+ I))) ...), boxed
   every element the loop read."
  (cond ((fixnum-long-lowerable-p expr)
         (compile-expr-to-long expr))
        ;; A subscript computed from (UNSIGNED-BYTE 64) words, such as a byte
        ;; of one, (LOGAND (ASH W -8) #xFF): its low 64 bits are its value when
        ;; its range fits an int64.
        ((and (%u64-masked-index-p expr) (%u64-low-bits-plan expr))
         (compile-int-slot-init expr))
        (t
         `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
               (compile-expr expr))
           (:call "Runtime.IndexL")))))

(defun compile-fixnum-binop (args op)
  "Emit native int64 binop, boxing result back to LispObject. All of +/-/*
   promote to Bignum on int64 overflow via Runtime.{Add,Subtract,Multiply}Fixnum.
   A raw native :add/:sub would silently wrap, so even the
   fixnum fast path must go through the promoting (AggressiveInlining) helper."
  (let ((helper (ecase op
                  (:add "Runtime.AddFixnum")
                  (:sub "Runtime.SubtractFixnum")
                  (:mul "Runtime.MultiplyFixnum"))))
    `(,@(compile-long-binop-spilled (first args) (second args))
      (:call ,helper))))

(defun compile-fixbit-binop (args op)
  "Emit native int64 bitwise binop (AND/OR/XOR), boxing result back to LispObject."
  `(,@(compile-long-binop-spilled (first args) (second args))
    (,op)
    (:call "Fixnum.Make")))

(defun compile-fixmod (args op)
  "Emit native int64 MOD/REM, boxing the result back to LispObject. The result
   is bounded by the divisor, so it is always a Fixnum -- no promotion check."
  `(,@(compile-long-binop-spilled (first args) (second args))
    (:call ,(if (eq op 'mod) "Runtime.ModFixnumL" "Runtime.RemFixnumL"))
    (:call "Fixnum.Make")))

(defun compile-fixbit-not (args)
  "Emit native int64 bitwise NOT, boxing result back to LispObject."
  `(,@(compile-as-long (first args))
    (:not)
    (:call "Fixnum.Make")))

(defun compile-ash-fast (args)
  "Emit native int64 ash when shift amount is a compile-time constant integer.
   Returns NIL if not applicable (non-constant shift)."
  (let ((x (first args))
        (n (second args)))
    (when (and (fixnum-exact-p x) (integerp n))
      (if (>= n 0)
          ;; Left shift can overflow int64 (and raw CIL shl masks count mod 64),
          ;; so route through Runtime.AshLeftLong which promotes to Bignum.
          `(,@(compile-as-long x)
            (:ldc-i4 ,n)
            (:call "Runtime.AshLeftLong"))
          ;; Right shift of a fixnum-typed long always fits: native SHR is safe
          ;; once the count is capped. CIL SHR takes the count mod 64, so
          ;; (ash x -64) came back as x; any count >= 63 leaves only the sign
          ;; (0 or -1), which is what SHR by 63 gives.
          `(,@(compile-as-long x)
            (:ldc-i4 ,(min 63 (- n)))
            (:shr)
            (:call "Fixnum.Make"))))))

(defun char-code-operand (expr)
  "Instructions leaving EXPR's character code on the stack as a raw int64, or
   NIL when EXPR is not something whose code is reachable without building a
   character object.

   Three shapes qualify, and only three:
   - a character literal, whose code is known here and becomes an immediate
   - a lexical local declared CHARACTER (see *CHARACTER-LOCALS*), read from its
     slot through CHAR-CODE-OF-L
   - (schar s i) / (char s i), which already has a typed entry that reads the
     code directly, so the character is never built at all

   Anything else is left to the generic comparison: this is an optimization,
   and declining it can only cost speed."
  (setf expr (fold-constant-symbol expr))
  (cond
    ((characterp expr) `((:ldc-i8 ,(char-code expr))))
    ;; A raw-code slot IS the code: the read is the whole operand. This is the
    ;; clause the native character representation exists for -- the one below
    ;; pays a call per comparison, and a scanner's whitespace test makes three
    ;; of them per input character.
    ((and (symbolp expr) expr (char-native-local-p expr) (lookup-local expr))
     `((:ldloc ,(lookup-local expr))))
    ((and (symbolp expr)
          expr
          (boundp '*character-locals*)
          (member (var-name expr) *character-locals* :test #'string=)
          (not (boxed-var-p expr))
          (lookup-local expr))
     `((:ldloc ,(lookup-local expr)) (:call "Runtime.CharCodeOfL")))
    ((and (consp expr) (member (car expr) '(schar char)) (= (length expr) 3)
          (not (local-function-entry (car expr))))
     (compile-string-char-code-as-long (cadr expr) (caddr expr)))
    (t nil)))

(defun compile-char-cmp (args op)
  "Emit a character comparison as an integer comparison of the codes. OP is the
   same keyword set COMPILE-FIXNUM-CMP takes, and the result is the same: an i4
   on the stack for :brfalse / :brtrue.

   CHAR= on two characters is EQL on their codes, so this is the same answer the
   generic entry gives -- without building the boolean object that entry returns
   and then asking IsTruthy about it."
  (let ((body (ecase op
                (:lt '((:clt)))
                (:gt '((:cgt)))
                (:eq '((:ceq)))
                (:le '((:cgt) (:ldc-i4 0) (:ceq)))
                (:ge '((:clt) (:ldc-i4 0) (:ceq)))
                (:ne '((:ceq) (:ldc-i4 0) (:ceq))))))
    ;; Both operands are pushed before the comparison, so each must be a
    ;; straight-line push; the two shapes that are not (a slot read and an
    ;; immediate are) go through COMPILE-STRING-CHAR-CODE-AS-LONG, which spills
    ;; to temps itself when its subexpressions are compound.
    `(,@(char-code-operand (first args))
      ,@(char-code-operand (second args))
      ,@body)))

(defun compile-fixnum-cmp (args op)
  "Emit native int64 comparison. OP is :lt :le :gt :ge :eq :ne.
   Leaves an i4 (0 or 1) on stack; suitable for :brfalse / :brtrue.
   Callers: compile-if-fused-comparison dispatch."
  (let ((body (ecase op
                (:lt '((:clt)))
                (:gt '((:cgt)))
                (:eq '((:ceq)))
                (:le '((:cgt) (:ldc-i4 0) (:ceq)))   ; not greater
                (:ge '((:clt) (:ldc-i4 0) (:ceq)))   ; not less
                (:ne '((:ceq) (:ldc-i4 0) (:ceq)))))) ; not equal
    `(,@(compile-long-binop-spilled (first args) (second args))
      ,@body)))

(defun literal-symbol-operand (expr)
  "If EXPR is a quoted symbol literal other than T and NIL, return that symbol.
   T and NIL are excluded because EQ and EQL bridge the T and NIL objects to
   the T and NIL symbols, which a plain reference comparison does not do."
  (and (consp expr)
       (eq (car expr) 'quote)
       (let ((s (cadr expr)))
         (and (symbolp s) (not (null s)) (not (eq s t)) s))))

(defun compile-sym-eq (args)
  "Emit an inline reference comparison for (EQ x 'SYM) / (EQL x 'SYM).
   Leaves an i4 (0 or 1) on stack; the same contract as COMPILE-FIXNUM-CMP.

   Runtime.IsTrueEq is Primary on both sides, ReferenceEquals, then a bridge
   that can only fire when one side is the T or NIL symbol; IsTrueEql adds the
   number and character cases after that. So against any other symbol literal
   the whole call reduces to Primary + a reference compare, and the literal
   side needs neither. What is saved is the call itself, which CASE pays once
   for every clause it walks past.

   CEQ is symmetric, but the operands are still emitted in source order.
   Loading a symbol literal is not a pure operation here: a keyword literal
   interns on every execution, and a literal in another package resolves (and
   can signal) at execution time. So compiling the other operand first would be
   observable if the package state changed between compilation and the call.
   When the literal comes first it goes through a temp rather than staying on
   the stack, because the other operand may compile to code containing a try
   region, and CIL only allows entering one with an empty stack."
  (let* ((lit-first (literal-symbol-operand (first args)))
         (lit (if lit-first (first args) (second args)))
         (other (if lit-first (second args) (first args)))
         (lit-instrs (let ((*in-tail-position* nil) (*in-mv-context* nil))
                       (compile-expr lit)))
         (other-instrs (let ((*in-tail-position* nil) (*in-mv-context* nil))
                         (compile-expr other))))
    (if lit-first
        (let ((tmp (gen-local "SE")))
          `((:declare-local ,tmp "LispObject")
            ,@lit-instrs
            (:stloc ,tmp)
            ,@other-instrs
            ,@(unless (%plain-local-load-p other other-instrs)
                '((:call "Runtime.Primary")))
            (:ldloc ,tmp)
            (:ceq)))
        `(,@other-instrs
          ,@(unless (%plain-local-load-p other other-instrs)
              '((:call "Runtime.Primary")))
          ,@lit-instrs
          (:ceq)))))

(defun %plain-local-load-p (expr code)
  "True when EXPR is a variable whose read compiled to CODE, a single load of
   an unboxed local slot."
  (and (symbolp expr) code (null (cdr code)) (eq (car (first code)) :ldloc)))
