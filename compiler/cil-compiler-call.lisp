;;; cil-compiler-call.lisp: Call helpers
;;; Part of the CIL compiler (A2 instruction list architecture)

(in-package :dotcl.cil-compiler)

;;; ============================================================
;;; Call helpers
;;; ============================================================

(defun handler-op-name ()
  (if *handler-op* (symbol-name *handler-op*) ""))

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
      ((and (= n 2)
            (or (and (fixnum-typed-p (first args)) (fixnum-typed-p (second args)))
                (and (float-typed-p (first args)) (float-typed-p (second args)))))
       ;; The test position compares raw values; the value position used to
       ;; box them again for the generic entry.
       (compile-expr `(if (,op ,@args) t nil)))
      ((= n 2) (compile-binary-call args method))
      (t
       ;; Expand (op a b c ...) -> (let* ((t0 a) (t1 b) (t2 c) ...) (and (op t0 t1) (op t1 t2) ...))
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
        ;; A keyword is a constant. It must not reach LOOKUP-LOCAL below: the
        ;; name fallback there can match a local of the same name (:CELL against
        ;; a variable CELL), and whether it does depends on the package the
        ;; compiler's own symbols ended up in, so the answer changed with the
        ;; host that built the compiler.
        ((keywordp expr) t)
        ((symbolp expr)
         ;; Simple if it's a lexical variable (not boxed, not symbol-macro)
         (and (not (lookup-symbol-macro expr))
              (lookup-local expr)
              (not (boxed-var-p expr))))
        ((and (consp expr) (eq (car expr) 'the))
         (simple-expr-p (caddr expr)))
        ((and (consp expr) (eq (car expr) 'quote)) t)
        (t nil)))

(defun compile-binary-call (args method &optional (fn-name (handler-op-name)))
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

(defun compile-ternary-call (args method &optional (fn-name (handler-op-name)))
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

(defun compile-quaternary-call (args method &optional (fn-name (handler-op-name)))
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

(defun compile-quinary-call (args method &optional (fn-name (handler-op-name)))
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

(defun compile-unary-call (args method &optional (fn-name (handler-op-name)))
  (unless (= (length args) 1)
    (let ((errmsg (format nil "~a: wrong number of arguments: ~a (expected 1)" fn-name (length args))))
      (return-from compile-unary-call
        (compile-static-program-error errmsg))))
  ;; Arg is single-valued: never in tail position, never in MV context
  (let ((code (let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))))
    `(,@code (:call ,(%unwrapped-predicate method code)))))

(defun %unwrapped-predicate (method code)
  "METHOD, or its variant without the multiple-values unwrap when CODE only
   loads a local or an argument: a variable never holds a values bundle."
  (let ((plain (and (= (length code) 1)
                    (consp (car code))
                    (member (caar code) '(:ldloc :ldarg))
                    (assoc method '(("Runtime.IsTrueConsp" . "Runtime.IsConsObject")
                                    ("Runtime.IsTrueAtom" . "Runtime.IsAtomObject")
                                    ("Runtime.IsTrueListp" . "Runtime.IsListObject"))
                           :test #'equal))))
    (if plain (cdr plain) method)))

(defun compile-append (args)
  "Compile (append ...) with 0+ arguments.
   Reduces multi-arg append to nested binary Runtime.Append calls."
  (cond
    ((null args) (emit-nil))
    ((null (cdr args)) (compile-for-single-value (car args)))
    ((null (cddr args)) (compile-binary-call args "Runtime.Append"))
    (t ;; 3+ args: fold left into nested binary appends
       (let ((acc (car args)))
         (dolist (a (cdr args))
           (setq acc (list 'append acc a)))
         (compile-form acc)))))

(defun compile-aref-native-index-call (arr idx-exprs method &optional (val nil val-p))
  "Compile an array read/write call whose index arguments are lowered to raw
   int64 on the stack (no Fixnum boxing); METHOD is one of the Runtime.*L
   variants taking long indices. ARR (and VAL for setters) stay boxed
   LispObject. Callers gate on every index being fixnum-typed-p. Mirrors the
   compile-N-ary-call discipline: all-simple args push directly; otherwise
   each arg is evaluated to a temp (Int64 temps for indices) so the stack is
   empty whenever a non-simple subexpression is compiled."
  (let ((all (append (list arr) idx-exprs (when val-p (list val)))))
    (if (every #'simple-expr-p all)
        `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
              (compile-expr arr))
          ,@(loop for idx in idx-exprs append (compile-expr-to-long idx))
          ,@(when val-p
              (let ((*in-tail-position* nil) (*in-mv-context* nil))
                (compile-expr val)))
          (:call ,method))
        (let ((arr-tmp (gen-local "NAA"))
              (idx-tmps (mapcar (lambda (i) (declare (ignore i)) (gen-local "NAI"))
                                idx-exprs))
              (val-tmp (when val-p (gen-local "NAV"))))
          `((:declare-local ,arr-tmp "LispObject")
            ,@(mapcar (lambda (tk) `(:declare-local ,tk "Int64")) idx-tmps)
            ,@(when val-p `((:declare-local ,val-tmp "LispObject")))
            ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                (compile-expr arr))
            (:stloc ,arr-tmp)
            ,@(loop for idx in idx-exprs
                    for tk in idx-tmps
                    append `(,@(compile-expr-to-long idx) (:stloc ,tk)))
            ,@(when val-p
                `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                      (compile-expr val))
                  (:stloc ,val-tmp)))
            (:ldloc ,arr-tmp)
            ,@(mapcar (lambda (tk) `(:ldloc ,tk)) idx-tmps)
            ,@(when val-p `((:ldloc ,val-tmp)))
            (:call ,method))))))

(defun %string-backing-read (entry idx fall-instrs boxp)
  "A character read against STR's hoisted char[] buffer, with FALL-INSTRS as the
   arm that runs when there is no buffer. BOXP leaves a character object
   (LispChar.Make); otherwise the raw code stays as a native int.

   The buffer slot is NIL whenever Runtime.BackingChars declined -- a LispString
   still holding a System.String, or a character vector that is not simple
   (adjustable, fill-pointered, displaced) or not rank 1. A simple character
   LispVector, which is what (MAKE-ARRAY n :element-type 'CHARACTER) builds, is
   fetched like a LispString's char[]. FALL-INSTRS is the typed call that ran before this
   path existed, so declining costs the speed and nothing else. Only one arm
   runs, so a subscript with a side effect is evaluated exactly once."
  (let* ((back (third entry))
         (fall (gen-label "SBFALL"))
         (done (gen-label "SBDONE"))
         (fast (if (simple-expr-p idx)
                   `((:ldloc ,back)
                     ,@(compile-index-to-long idx)
                     (:conv-i)
                     (:ldelem-u2)
                     ,@(when boxp '((:call "LispChar.Make"))))
                   (let ((tk (gen-local "SBI")))
                     `((:declare-local ,tk "Int64")
                       ,@(compile-index-to-long idx)
                       (:stloc ,tk)
                       (:ldloc ,back)
                       (:ldloc ,tk)
                       (:conv-i)
                       (:ldelem-u2)
                       ,@(when boxp '((:call "LispChar.Make"))))))))
    (ecase (%backing-arm-for entry)
      (:fast fast)
      (:helper fall-instrs)
      (:both `((:ldloc ,back)
               (:brfalse ,fall)
               ,@fast
               (:br ,done)
               (:label ,fall)
               ,@fall-instrs
               (:label ,done))))))

(defun compile-string-char-code-as-long (str idx)
  "Emit (char-code (schar STR IDX)) as a raw int64. When STR has a hoisted
   char[] buffer the read is a bare ldelem against it with the typed call as the
   other arm; otherwise it is the typed call alone."
  (let ((back (string-backing-entry str)))
    (if back
        (%string-backing-read back idx (%string-char-code-call str idx) nil)
        (%string-char-code-call str idx))))

(defun %string-char-code-call (str idx)
  "The typed call form: (char-code (schar STR IDX)) as a raw int64 (Runtime.CharCodeAtL).
   STR stays a LispObject -- the runtime entry decides whether it is the simple
   string it can read directly -- and IDX is lowered the way an array subscript
   is. A non-simple subexpression is evaluated into a temp first, so the stack is
   empty whenever one compiles."
  (if (and (simple-expr-p str) (simple-expr-p idx))
      `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
            (compile-expr str))
        ,@(compile-index-to-long idx)
        (:call "Runtime.CharCodeAtL"))
      (let ((str-tmp (gen-local "SCS"))
            (idx-tmp (gen-local "SCI")))
        `((:declare-local ,str-tmp "LispObject")
          (:declare-local ,idx-tmp "Int64")
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
              (compile-expr str))
          (:stloc ,str-tmp)
          ,@(compile-index-to-long idx)
          (:stloc ,idx-tmp)
          (:ldloc ,str-tmp)
          (:ldloc ,idx-tmp)
          (:call "Runtime.CharCodeAtL")))))

(defun compile-string-char-as-object (str idx)
  "Emit (schar STR IDX), leaving the character on the stack. Hoisted buffer with
   the typed call as the other arm, exactly as the code read is."
  (let ((back (string-backing-entry str)))
    (if back
        (%string-backing-read back idx (%string-char-call str idx) t)
        (%string-char-call str idx))))

(defun %string-char-call (str idx)
  "The typed call form: (schar STR IDX) as one call (Runtime.CharAtL), leaving the
   character on the stack.

   The value-position counterpart of COMPILE-STRING-CHAR-CODE-AS-LONG, and it
   spills the same way. What it removes is what a generic call to a two-argument
   builtin pays per element: the subscript boxed into a Fixnum, the symbol
   lookup, and the virtual Invoke. STR stays a LispObject -- the runtime entry
   is the one that decides whether it is the simple string it can read
   directly -- and IDX is lowered the way an array subscript is, so an
   undeclared index still goes in raw."
  (if (and (simple-expr-p str) (simple-expr-p idx))
      `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
            (compile-expr str))
        ,@(compile-index-to-long idx)
        (:call "Runtime.CharAtL"))
      (let ((str-tmp (gen-local "SAS"))
            (idx-tmp (gen-local "SAI")))
        `((:declare-local ,str-tmp "LispObject")
          (:declare-local ,idx-tmp "Int64")
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
              (compile-expr str))
          (:stloc ,str-tmp)
          ,@(compile-index-to-long idx)
          (:stloc ,idx-tmp)
          (:ldloc ,str-tmp)
          (:ldloc ,idx-tmp)
          (:call "Runtime.CharAtL")))))

(defun %backing-index-code (entry idxs)
  "Two values: instructions run before the buffer is loaded (non-simple
   subscripts, in order), and instructions that then leave the row-major index
   as an int64. Above (safety 0) each subscript of a rank 2 or 3 array is
   checked against its dimension; at rank 1 the CLR's bounds check does it."
  (let ((dims (backing-entry-dims entry)))
    (if (null dims)
        (let ((idx (first idxs)))
          (if (simple-expr-p idx)
              (values '() (compile-index-to-long idx))
              (let ((tk (gen-local "NAI")))
                (values `((:declare-local ,tk "Int64")
                          ,@(compile-index-to-long idx)
                          (:stloc ,tk))
                        `((:ldloc ,tk))))))
        (%backing-row-major-code dims idxs))))

(defun %backing-row-major-code (dims idxs)
  (let* ((check (not (compiling-at-safety-0-p)))
         (consts (mapcar #'%const-index-value idxs))
         (spill (loop for idx in idxs for c in consts
                      thereis (and (null c) (not (simple-expr-p idx)))))
         (pre '()))
    (flet ((dim-code (d) (if (integerp d) `((:ldc-i8 ,d)) `((:ldloc ,d)))))
      (let ((axes
              (loop for idx in idxs for c in consts for d in dims for k from 0
                    collect
                    (cond
                      ((and c (integerp d)) c)
                      (t (let ((code
                                 (cond (c `((:ldc-i8 ,c)))
                                       ((not spill) (compile-index-to-long idx))
                                       (t (let ((tk (gen-local "NAI")))
                                            (setf pre (append pre
                                                              `((:declare-local ,tk "Int64")
                                                                ,@(compile-index-to-long idx)
                                                                (:stloc ,tk))))
                                            `((:ldloc ,tk)))))))
                           (if check
                               `(,@code ,@(dim-code d) (:ldc-i4 ,k) (:call "Runtime.AxisIndexL"))
                               code)))))))
        (values
         pre
         (if (every #'integerp dims)
             (let ((const 0) (code '()))
               (loop for a in axes
                     for k from 0
                     for stride = (reduce #'* (nthcdr (1+ k) dims))
                     do (if (integerp a)
                            (incf const (* a stride))
                            (let ((term (if (= stride 1)
                                            a
                                            `(,@a (:ldc-i8 ,stride) (:mul)))))
                              (setf code (if code `(,@code ,@term (:add)) term)))))
               (cond ((null code) `((:ldc-i8 ,const)))
                     ((zerop const) code)
                     (t `(,@code (:ldc-i8 ,const) (:add)))))
             (let ((code (let ((a (first axes))) (if (integerp a) `((:ldc-i8 ,a)) a))))
               (loop for a in (rest axes)
                     for d in (rest dims)
                     do (setf code `(,@code ,@(dim-code d) (:mul)
                                     ,@(if (integerp a) `((:ldc-i8 ,a)) a) (:add))))
               code)))))))

(defun compile-backing-aref-as-long (entry idxs fall-instrs)
  "Emit (AREF V IDX...) as a bare element load against V's hoisted buffer, leaving
   a raw int64, with FALL-INSTRS as the arm that runs when there is no buffer.
   The subscript is narrowed to a native int rather than an i4, so the CLR's own
   bounds check still rejects a subscript outside the array (conv.i4 would wrap
   one into range first).

   The buffer slot is NIL whenever the fetch declined -- a fill-pointered or
   adjustable vector, whose storage VECTOR-PUSH-EXTEND and ADJUST-ARRAY replace,
   and anything that is not the declared array at all. FALL-INSTRS is the
   per-element helper that ran before this path existed, so declining costs the
   speed and nothing else. Only one arm runs, so a subscript with a side effect
   is still evaluated exactly once."
  (let ((back (third entry))
        (kind (fourth entry))
        (fall (gen-label "ABFALL"))
        (done (gen-label "ABDONE")))
    (let ((ref (%bounded-backing-ref entry idxs)))
      (when ref
        (return-from compile-backing-aref-as-long
          `((:ldloc ,ref)
            ,@(compile-index-to-long (first idxs))
            (:call ,(concatenate 'string "Runtime.At" (%backing-ref-suffix kind)))))))
    (let ((fast (multiple-value-bind (pre code) (%backing-index-code entry idxs)
                  `(,@pre
                    (:ldloc ,back)
                    ,@code
                    (:conv-i)
                    (,(%backing-ldelem kind))))))
      (ecase (%backing-arm-for entry)
        (:fast fast)
        (:helper fall-instrs)
        (:both `((:ldloc ,back)
                 (:brfalse ,fall)
                 ,@fast
                 (:br ,done)
                 (:label ,fall)
                 ,@fall-instrs
                 (:label ,done)))))))

(defun %backing-index-into (entry idxs tk)
  (multiple-value-bind (pre code) (%backing-index-code entry idxs)
    `((:declare-local ,tk "Int64") ,@pre ,@code (:stloc ,tk))))

(defun compile-backing-aref-set (entry idxs val fall-instrs)
  "Emit (SETF (AREF V IDX...) VAL) against V's hoisted buffer, with FALL-INSTRS as
   the arm that runs when there is no buffer. The value is range-checked for the
   narrow kinds first -- the opcode's narrowing would wrap an out-of-width value
   where the boxed store path signals -- and left on the stack as a RAW int64,
   as FALL-INSTRS must leave it too. The caller boxes it once, after the arms
   join: boxed in each arm, the box sat before the join label, where the
   peephole that drops a box nobody reads (statement position) cannot see it.

   Only one arm runs, so VAL is evaluated exactly once even though both arms
   contain code to evaluate it."
  (let* ((back (third entry))
         (kind (fourth entry))
         (chk (%backing-store-check-instrs kind (fifth entry) val))
         (tk (gen-local "NAI"))
         (vk (gen-local "NAV"))
         (fall (gen-label "ABFALL"))
         (done (gen-label "ABDONE"))
         (bounded (%bounded-backing-ref entry idxs))
         (fast `(,@(%backing-index-into entry idxs tk)
                 (:declare-local ,vk "Int64")
                 ,@(compile-expr-to-long val)
                 ,@chk
                 (:stloc ,vk)
                 (:ldloc ,(or bounded back))
                 (:ldloc ,tk)
                 ,@(if bounded
                       `((:ldloc ,vk)
                         (:call ,(concatenate 'string "Runtime.SetAt" (%backing-ref-suffix kind))))
                       `((:conv-i) (:ldloc ,vk) (,(%backing-stelem kind))))
                 (:ldloc ,vk))))
    (ecase (if bounded :fast (%backing-arm-for entry))
      (:fast fast)
      (:helper fall-instrs)
      (:both `((:ldloc ,back)
               (:brfalse ,fall)
               ,@fast
               (:br ,done)
               (:label ,fall)
               ,@fall-instrs
               (:label ,done))))))


(defun %backing-duplicable-p (form)
  "T when FORM may be compiled twice. Compiling is not a pure function of the
   source: LOAD-TIME-VALUE takes a slot, DEFMETHOD and DEFSTRUCT run at assembly
   time whether or not their branch is taken, and EVAL-WHEN can do anything. A
   body containing any of those would get two of whatever it does, so it keeps
   the per-access test instead. Crude in the safe direction: an unfamiliar
   operator is assumed unsafe only if it is on this list, and the list is of
   things whose compile-time effect is known, so a false YES is possible and is
   caught by the test suites rather than by this."
  (cond ((not (consp form)) t)
        ((and (symbolp (car form))
              (member (symbol-name (car form))
                      '("LOAD-TIME-VALUE" "DEFMETHOD" "DEFGENERIC" "DEFSTRUCT"
                        "DEFCLASS" "EVAL-WHEN" "DEFCONSTANT" "DEFPACKAGE"
                        "DEFINE-COMPILER-MACRO" "DEFSETF" "DEFINE-SETF-EXPANDER"
                        "DEFMACRO" "IN-PACKAGE")
                      :test #'string=))
         nil)
        (t (and (%backing-duplicable-p (car form))
                (%backing-duplicable-p (cdr form))))))

(defun %struct-backing-length (entry)
  "How many elements ENTRY's raw slot array must have for every position its
   layout names to be in range: one more than the largest position."
  (1+ (or (loop for cell in (cdr (cddddr entry))
                when (and cell (>= (car cell) 0)) maximize (car cell))
           -1)))

(defun unswitch-array-backing (entries body thunk &optional struct-entries)
  "Emit BODY's instructions twice, once with every hoisted buffer known present
   and once with none of them used, behind a single test per buffer. A body of
   more than *UNSWITCH-BODY-LIMIT* instructions is emitted once instead, with
   the test at each access, when that is the smaller of the two.

   The test a hoisted access would otherwise carry is loop-invariant, and
   leaving it at the access is what costs. Measured on the array-walk kernel:
   the per-access form turned the inner loop from a 7-instruction single block
   with a fall-through back-edge into 11 instructions across 4 blocks, because
   the test also stopped the JIT hoisting the data base and the element count
   out of the loop. Both are consequences of where the test is, not of its
   existence, so it moves here and the fast copy goes back to the original
   block.

   All buffers are tested together rather than one unswitch per buffer: n
   buffers would otherwise want 2^n copies, and a body that hoists two arrays
   and gets one of them is rare enough not to pay for that.

   STRUCT-ENTRIES are hoisted structure raw slot arrays (EXTRACT-STRUCT-BACKING-
   LOCALS, which already keeps only those whose slots are reached from inside
   a loop). Each is tested for being present AND for being as long as its
   layout says. The length cannot be short for an array the fetch handed out
   -- the fetch matched the layout version -- so that half is not for
   correctness: it is what lets the JIT drop the bounds check on every
   constant-position access in the fast copy, which it cannot do from the
   null test alone. In the slow copy a structure access keeps its per-access
   test, because the copy is taken when ANY buffer was missing, not
   necessarily that one."
  (let* ((entries (remove-if (lambda (e) (or (backing-entry-dims e)
                                             (%backing-never-declines-p e)
                                             (eq (tenth e) :never-null)))
                             entries))
         (arrays (and entries
                      (some (lambda (e)
                              (%var-iterated-access-p
                               body (first e) nil
                               (if (eq (fourth e) :char)
                                   (function %tree-finds-schar-p)
                                   (function %tree-finds-aref-p))))
                            entries)))
         (structs (and struct-entries (%backing-duplicable-p body))))
    (if (or (not (or arrays structs))
            (not (%backing-duplicable-p body)))
        (funcall thunk)
        (let* ((slow (gen-label "ABUSLOW"))
               (done (gen-label "ABUDONE"))
               (tests `(,@(when arrays
                            (loop for e in entries
                                  append `((:ldloc ,(third e)) (:brfalse ,slow))))
                        ,@(loop for e in struct-entries
                                append `((:ldloc ,(struct-backing-slot e))
                                         (:brfalse ,slow)
                                         (:ldloc ,(struct-backing-slot e))
                                         (:ldlen)
                                         (:conv-i4)
                                         (:ldc-i4 ,(%struct-backing-length e))
                                         (:blt ,slow)))))
               (fast (let ((*backing-arm* (if arrays :fast *backing-arm*))
                           (*array-backing-tested*
                             (if arrays
                                 (append (mapcar #'third entries)
                                         (if (eq *backing-arm* :fast)
                                             *array-backing-tested*
                                             '()))
                                 *array-backing-tested*))
                           (*struct-backing-known* (append struct-entries
                                                           *struct-backing-known*)))
                       (funcall thunk)))
               (slow-copy (let ((*backing-arm* (if arrays :helper *backing-arm*))
                                (*array-backing-tested* (if arrays '() *array-backing-tested*)))
                            (funcall thunk)))
               (plain (and (> (length fast) *unswitch-body-limit*)
                           (funcall thunk))))
          ;; A body too large to be a tight loop takes whichever of the two
          ;; shapes is smaller: two copies, or one with the test at each
          ;; access (which carries both arms of every access, so it is not
          ;; always the smaller).
          (if (and plain
                   (< (length plain) (+ (length tests) (length fast) (length slow-copy))))
              plain
              `(,@tests
                ,@fast
                (:br ,done)
                (:label ,slow)
                ,@slow-copy
                (:label ,done)))))))

(defun array-backing-prologue (entries)
  "Instructions that fetch each hoisted element buffer once, right after the
   variables are bound. At (safety 0) the fetch answers NIL when it declines --
   a fill-pointered or adjustable vector, whose storage VECTOR-PUSH-EXTEND and
   ADJUST-ARRAY replace, a displaced one -- and every access carries an arm that
   runs the per-element helper for exactly that case, so a declaration this
   cannot honour costs the speed and never the answer. Above (safety 0) such a
   value is a TYPE-ERROR from the fetch itself (see %BACKING-FETCH-CALL)."
  (loop for e in entries
        for kind = (fourth e)
        for dims = (backing-entry-dims e)
        unless (%raw-array-entry-p e)
        append `((:declare-local ,(third e) ,(%backing-slot-type kind))
                 ,@(cond
                     ((second e) `((:ldloc ,(second e))))
                     ((nth 10 e)
                      `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                            (compile-expr (ninth e)))
                        (:declare-local ,(nth 10 e) "LispObject")
                        (:dup)
                        (:stloc ,(nth 10 e))))
                     (t (let ((*in-tail-position* nil) (*in-mv-context* nil))
                          (compile-expr (ninth e)))))
                 ,@(cond
                     (dims
                      `((:ldc-i4 ,(length dims))
                        ,@(loop for k below 3
                                for d = (nth k dims)
                                collect `(:ldc-i4 ,(if (integerp d) d -1)))
                        (:ldc-i4 ,(if (or (eq (sixth e) :checked) (ninth e)) (or (fifth e) 0) 0))
                        (:ldc-i4 ,(if (compiling-at-safety-0-p) 0 1))
                        (:call ,(ecase kind
                                  (1 "Runtime.BackingNDU8") (2 "Runtime.BackingNDU16")
                                  (3 "Runtime.BackingNDI32") (4 "Runtime.BackingNDI64")
                                  (:u64 "Runtime.BackingNDU64")
                                  (:f64 "Runtime.BackingNDF64") (:f32 "Runtime.BackingNDF32")))))
                     ((and (eq (sixth e) :checked) (not (member kind '(:f64 :f32))))
                       `((:ldc-i4 ,(or (fifth e) 0))
                         (:call ,(%backing-narrow-fetch-call kind))))
                     ((eq (tenth e) :never-null)
                      `((:call ,(%backing-fetch-call kind t))))
                     (t `((:call ,(%backing-fetch-call kind)))))
                 (:stloc ,(third e))
                 ,@(when (and (ninth e) (eq (tenth e) :never-null) (null dims))
                     (let ((ok (gen-label "ABHAVE")))
                       `((:ldloc ,(third e))
                         (:brtrue ,ok)
                         (:load-const ,(ninth e))
                         (:call "Runtime.ConstantArrayChanged")
                         (:throw)
                         (:label ,ok))))
                 ,@(loop for d in dims
                         for k from 0
                         unless (integerp d)
                           append `((:declare-local ,d "Int64")
                                    (:ldloc ,(second e))
                                    (:ldc-i4 ,k)
                                    (:call "Runtime.ArrayAxisOrZero")
                                    (:stloc ,d))))))

(defun %backing-narrow-fetch-call (kind)
  "The fetch of a :CHECKED entry (EXTRACT-ARRAY-BACKING-LOCALS): the *Checked
   entry's tests, and the vector's element type has to be the declared one too,
   by its narrow width (%ELEMENT-NARROW-WIDTH, 0 when the element type is the
   buffer's own). The *Checked entry matches only the buffer, so a FIXNUM vector
   passes for an (UNSIGNED-BYTE 32) one. A false declaration is a TYPE-ERROR,
   as in SBCL above (safety 0)."
  (ecase kind
    (1 "Runtime.BackingU8Narrow") (2 "Runtime.BackingU16Narrow")
    (3 "Runtime.BackingI32Narrow") (4 "Runtime.BackingI64Narrow")))

(defun %aref-arr-temp (arr)
  "Split the array operand of a numeric AREF into three instruction lists:
   what to DECLARE, what to evaluate UP FRONT, and how to LOAD it where the
   call wants it.

   A plain lexical local is pure, so it declares nothing, costs nothing up
   front and is simply loaded in place. A structure accessor call is not pure
   and must be evaluated FIRST, into a temp: the emitters below lower
   non-simple subscripts into temps before they load the array, and moving a
   call past a subscript would evaluate the two in the wrong order."
  (if (simple-expr-p arr)
      (values '() '()
              (let ((*in-tail-position* nil) (*in-mv-context* nil))
                (compile-expr arr)))
      (let ((tmp (gen-local "NAA")))
        (values `((:declare-local ,tmp "LispObject"))
                `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                      (compile-expr arr))
                  (:stloc ,tmp))
                `((:ldloc ,tmp))))))

(defun compile-numeric-aref-as-long (arr idxs rank)
  "Emit (aref ARR IDX...) on a numeric-backed array as a raw int64.

   When the variable has a hoisted element buffer, the read is a bare ldelem
   against it with the per-element helper as the other arm: the buffer slot is
   NIL whenever the fetch declined, and declining has to cost speed rather than
   correctness. Everything else compiles to the helper alone."
  (let ((helper (%numeric-aref-helper-as-long arr idxs rank))
        (back (array-backing-entry-for-read (list* 'aref arr idxs))))
    (if back
        (compile-backing-aref-as-long back idxs helper)
        helper)))

(defun %numeric-aref-helper-as-long (arr idxs rank)
  "Emit (aref ARR IDX...) on a proven numeric-backed array as a raw int64 on
   the stack (Runtime.ArefNum*L). ARR is a plain lexical local or a structure
   accessor call (numeric-array-aref-info); non-simple indices are evaluated to
   Int64 temps first so the stack is empty whenever a non-simple subexpression
   compiles, and ARR goes into a temp ahead of them when it is not a local."
  (let ((method (ecase rank
                  (1 "Runtime.ArefNumL")
                  (2 "Runtime.ArefNum2DL")
                  (3 "Runtime.ArefNum3DL"))))
    (if (every #'simple-expr-p idxs)
        `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
              (compile-expr arr))
          ,@(loop for idx in idxs append (compile-index-to-long idx))
          (:call ,method))
        (multiple-value-bind (arr-decl arr-pre arr-load) (%aref-arr-temp arr)
          (let ((idx-tmps (mapcar (lambda (i) (declare (ignore i)) (gen-local "NAI"))
                                  idxs)))
            `(,@arr-decl
              ,@(mapcar (lambda (tk) `(:declare-local ,tk "Int64")) idx-tmps)
              ,@arr-pre
              ,@(loop for idx in idxs
                      for tk in idx-tmps
                      append `(,@(compile-index-to-long idx) (:stloc ,tk)))
              ,@arr-load
              ,@(mapcar (lambda (tk) `(:ldloc ,tk)) idx-tmps)
              (:call ,method)))))))

(defun compile-numeric-aref-set (arr idxs val rank)
  "Emit (setf (aref ARR IDX...) VAL) on a numeric-backed array, boxed value on
   the stack. Hoisted buffer with a helper arm, exactly as the read is."
  (let ((helper (%numeric-aref-helper-set arr idxs val rank))
        (back (array-backing-entry (list* 'aref arr idxs))))
    `(,@(if back
            (compile-backing-aref-set back idxs val helper)
            helper)
      (:call "Fixnum.Make"))))

(defun %numeric-aref-helper-set (arr idxs val rank)
  "Emit (setf (aref ARR IDX...) VAL) on a numeric-backed array with the value
   lowered to a raw int64 (Runtime.ArefSetNum*L; range-checked store, returns
   the stored long). Leaves that RAW long on the stack; COMPILE-NUMERIC-AREF-SET
   boxes it for the general setf contract, and in statement position the
   peephole (P5 family) deletes the box."
  (let ((method (ecase rank
                  (1 "Runtime.ArefSetNumL")
                  (2 "Runtime.ArefSetNum2DL")
                  (3 "Runtime.ArefSetNum3DL"))))
    (if (and (every #'simple-expr-p idxs) (simple-expr-p val))
        `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
              (compile-expr arr))
          ,@(loop for idx in idxs append (compile-index-to-long idx))
          ,@(compile-expr-to-long val)
          (:call ,method))
        (multiple-value-bind (arr-decl arr-pre arr-load) (%aref-arr-temp arr)
          (let ((idx-tmps (mapcar (lambda (i) (declare (ignore i)) (gen-local "NAI"))
                                  idxs))
                (val-tmp (gen-local "NAV")))
            `(,@arr-decl
              ,@(mapcar (lambda (tk) `(:declare-local ,tk "Int64")) idx-tmps)
              (:declare-local ,val-tmp "Int64")
              ,@arr-pre
              ,@(loop for idx in idxs
                      for tk in idx-tmps
                      append `(,@(compile-index-to-long idx) (:stloc ,tk)))
              ,@(compile-expr-to-long val)
              (:stloc ,val-tmp)
              ,@arr-load
              ,@(mapcar (lambda (tk) `(:ldloc ,tk)) idx-tmps)
              (:ldloc ,val-tmp)
              (:call ,method)))))))

(defvar *float-aref-set-no-backing* nil
  "Bound to T while COMPILE-NUMERIC-AREF-SET-FLOAT builds its own helper arm, so
   that arm is the per-element store and not the hoisted one again.")

(defun %float-backing-entry (arr idxs rank)
  "ARR's hoisted double[] / float[] entry, or NIL."
  (declare (ignore rank))
  (and (not *float-aref-set-no-backing*)
       (let ((e (array-backing-entry (list* 'aref arr idxs))))
         (and e (member (fourth e) '(:f64 :f32)) e))))

(defun compile-numeric-aref-float (arr idxs rank)
  "Emit (aref ARR IDX...) on a proven float-backed numeric array as a native
   r8 (double) on the stack. Mirrors compile-numeric-aref-as-long but leaves a
   raw double and never boxes. single-float backing widens to double here;
   callers narrow with conv.r4. A local whose double[] / float[] was hoisted at
   its binding is read with a bare ldelem, with the per-element helper as the
   arm for an array the fetch declined."
  (let ((helper (%numeric-aref-float-helper arr idxs rank))
        (back (%float-backing-entry arr idxs rank)))
    (if back
        (compile-backing-aref-as-long back idxs helper)
        helper)))

(defun %numeric-aref-float-helper (arr idxs rank)
  "COMPILE-NUMERIC-AREF-FLOAT through the per-element helper Runtime.ArefNum*D."
  (let ((method (ecase rank
                  (1 "Runtime.ArefNumD")
                  (2 "Runtime.ArefNum2DD")
                  (3 "Runtime.ArefNum3DD"))))
    (if (every #'simple-expr-p idxs)
        `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
              (compile-expr arr))
          ,@(loop for idx in idxs append (compile-index-to-long idx))
          (:call ,method))
        (multiple-value-bind (arr-decl arr-pre arr-load) (%aref-arr-temp arr)
          (let ((idx-tmps (mapcar (lambda (i) (declare (ignore i)) (gen-local "NAI"))
                                  idxs)))
            `(,@arr-decl
              ,@(mapcar (lambda (tk) `(:declare-local ,tk "Int64")) idx-tmps)
              ,@arr-pre
              ,@(loop for idx in idxs
                      for tk in idx-tmps
                      append `(,@(compile-index-to-long idx) (:stloc ,tk)))
              ,@arr-load
              ,@(mapcar (lambda (tk) `(:ldloc ,tk)) idx-tmps)
              (:call ,method)))))))

(defun compile-numeric-aref-set-float (arr idxs val rank kind)
  "Emit (setf (aref ARR IDX...) VAL) on a float-backed numeric array. VAL is
   lowered to a native r8 (single-typed values widen with conv.r8) and stored
   via Runtime.ArefSetNum*D (narrows to float for single backing). Leaves the
   value BOXED for the setf contract; peephole removes it in statement position."
  (let ((method (ecase rank
                  (1 "Runtime.ArefSetNumD")
                  (2 "Runtime.ArefSetNum2DD")
                  (3 "Runtime.ArefSetNum3DD")))
        (box (ecase kind
               (:single '((:conv-r4) (:newobj "SingleFloat")))
               (:double '((:newobj "DoubleFloat")))))
        (back (%float-backing-entry arr idxs rank)))
    (flet ((val-as-double ()
             (if (eq kind :single)
                 `(,@(compile-as-single val) (:conv-r8))
                 (compile-as-double val))))
      (if back
          ;; Hoisted buffer: a bare stelem, with the helper store (which this
          ;; same function emits without BACK) as the arm for a declined fetch.
          ;; Both arms leave the raw r8; the box is made once after they join.
          (let* ((fall-instrs
                   (let ((full (let ((*float-aref-set-no-backing* t))
                                 (compile-numeric-aref-set-float arr idxs val rank kind))))
                     (butlast full (length box))))
                 (tk (gen-local "NAI"))
                 (vk (gen-local "NAV"))
                 (fall (gen-label "ABFALL"))
                 (done (gen-label "ABDONE"))
                 (bounded (%bounded-backing-ref back idxs))
                 (fast `(,@(%backing-index-into back idxs tk)
                         (:declare-local ,vk "Double")
                         ,@(val-as-double)
                         (:stloc ,vk)
                         (:ldloc ,(or bounded (third back)))
                         (:ldloc ,tk)
                         ,@(if bounded
                               `((:ldloc ,vk)
                                 (:call ,(concatenate 'string "Runtime.SetAt"
                                                      (%backing-ref-suffix (fourth back)))))
                               `((:conv-i) (:ldloc ,vk) (,(%backing-stelem (fourth back)))))
                         (:ldloc ,vk))))
            `(,@(ecase (if bounded :fast (%backing-arm-for back))
                  (:fast fast)
                  (:helper fall-instrs)
                  (:both `((:ldloc ,(third back))
                           (:brfalse ,fall)
                           ,@fast
                           (:br ,done)
                           (:label ,fall)
                           ,@fall-instrs
                           (:label ,done))))
              ,@box))
      (if (and (every #'simple-expr-p idxs) (simple-expr-p val))
          `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                (compile-expr arr))
            ,@(loop for idx in idxs append (compile-index-to-long idx))
            ,@(val-as-double)
            (:call ,method)
            ,@box)
          (multiple-value-bind (arr-decl arr-pre arr-load) (%aref-arr-temp arr)
            (let ((idx-tmps (mapcar (lambda (i) (declare (ignore i)) (gen-local "NAI"))
                                    idxs))
                  (val-tmp (gen-local "NAV")))
              `(,@arr-decl
                ,@(mapcar (lambda (tk) `(:declare-local ,tk "Int64")) idx-tmps)
                (:declare-local ,val-tmp "Double")
                ,@arr-pre
                ,@(loop for idx in idxs
                        for tk in idx-tmps
                        append `(,@(compile-index-to-long idx) (:stloc ,tk)))
                ,@(val-as-double)
                (:stloc ,val-tmp)
                ,@arr-load
                ,@(mapcar (lambda (tk) `(:ldloc ,tk)) idx-tmps)
                (:ldloc ,val-tmp)
                (:call ,method)
                ,@box))))))))

;;; LIST / LIST* build their conses inline for a small, fixed argument count.
;;;
;;; Both used to hand their arguments to a runtime function through an array, so
;;; the call cost 24 + 8n bytes on top of the conses it returned. Writing the same
;;; conses by hand already cost exactly the conses and nothing else. Measured
;;; against SBCL -- whose cons is 16 bytes to a .NET object's 32, so 2x is the
;;; floor here -- (cons a (cons b nil)) sat exactly on that floor while
;;; (list a b) was 40 bytes above it. LIST is in every library there is.
;;;
;;; Left-to-right argument order is preserved: nested CONS evaluates its car
;;; before its cdr. The bound keeps the emitted nesting from growing without
;;; limit; past it the array is the cheaper shape.

(defparameter +inline-list-max-args+ 8
  "Longest LIST / LIST* call whose conses are emitted inline instead of through
   the args-array runtime entry.")

(defun %nested-cons-form (args tail)
  "(CONS a1 (CONS a2 ... TAIL)) for ARGS = (a1 a2 ...)."
  (let ((form tail))
    (dolist (a (reverse args) form)
      (setq form `(cons ,a ,form)))))

(defun compile-list-call (args)
  (cond
    ((null args) (emit-nil))
    ((<= (length args) +inline-list-max-args+)
     (compile-expr (%nested-cons-form args nil)))
    (t `(,@(compile-args-array args)
         (:call "Runtime.List")
         (:call "MultipleValues.Primary")))))

(defun compile-list-star-call (args)
  (cond
    ((null args) (error "LIST* requires at least one argument"))
    ;; (LIST* x) is x. Compiling it as such removes a call, and there is no cons
    ;; to build.
    ((null (cdr args)) (compile-for-single-value (car args)))
    ((<= (length args) +inline-list-max-args+)
     (compile-expr (%nested-cons-form (butlast args) (car (last args)))))
    (t `(,@(compile-args-array args)
         (:call "Runtime.ListStar")))))

(defun compile-gethash (args)
  (let ((nargs (length args)))
    (cond
      ((< nargs 2) (compile-static-program-error "GETHASH: too few arguments (expected 2-3)"))
      ((> nargs 3) (compile-static-program-error "GETHASH: too many arguments (expected 2-3)"))
      (t
       ;; The arguments are single-valued wherever the GETHASH form is: a
       ;; table from a function that returns more than one value is its
       ;; primary value, also when the GETHASH is in tail position or inside
       ;; MULTIPLE-VALUE-BIND.
       `(,@(compile-value-args (list (first args) (second args)
                                     (if (= nargs 3) (third args) '(:code (:ldnull)))))
         (:call "Runtime.Gethash"))))))

(defun compile-value-args (items)
  "Push the single value of each of ITEMS, left to right, for a known-function
   intrinsic call. An item is a form, or (:CODE . instructions) pushed as is.
   Code that leaves a protected region or opens one (CATCH, HANDLER-CASE,
   UNWIND-PROTECT, a loop exited with LEAVE) needs an empty evaluation stack,
   and the values of the items before it would be on it, so when any item after
   the first has such code every value is taken into a temp first."
  (let ((codes (let ((*in-tail-position* nil) (*in-mv-context* nil))
                 (mapcar (lambda (it)
                           (if (and (consp it) (eq (car it) :code))
                               (cdr it)
                               (compile-expr it)))
                         items))))
    (if (notany #'%instrs-need-empty-stack-p (cdr codes))
        (apply #'append codes)
        (let ((temps (loop repeat (length codes) collect (gen-local "VA"))))
          (append (loop for code in codes
                        for tmp in temps
                        append `((:declare-local ,tmp "LispObject") ,@code (:stloc ,tmp)))
                  (loop for tmp in temps collect `(:ldloc ,tmp)))))))

(defun compile-puthash (args)
  "Compile (puthash key table value)."
  `(,@(compile-value-args args)
    (:call "Runtime.Puthash")))

(defun compile-values-call (args)
  (cond
    ;; (values): a shared marker, no array and no wrapper to build.
    ((null args) '((:call "MultipleValues.Values0")))
    ;; (values x) where only the primary value is taken, nothing reads what is published.
    ((and (null (cdr args)) (null *in-mv-context*) (null *in-tail-position*))
     (compile-for-single-value (car args)))
    ;; (values x) is "the primary value of x, published as the only value", which
    ;; is exactly what MULTIPLEVALUES.PRIMARY does -- and it publishes through a
    ;; per-thread cached array, so nothing is allocated. Going through the general
    ;; entry built a LispObject[1] the callee then threw away, 32 bytes a call.
    ((null (cdr args))
     `(,@(let ((*in-mv-context* t)) (compile-expr (car args)))
       (:call "MultipleValues.Primary")))
    ;; In a tail position with a value mode (the tail of a body whose caller
    ;; passed one, or the form of a capture in this body): the values in the
    ;; form the mode names, without an MvReturn (MultipleValues.ValuesNMode).
    ((and *in-tail-position* (<= 2 (length args) 8) (%mv-mode-local))
     (let* ((da (compile-direct-call-args args))
            (temps (car da)))
       `(,@(cdr da)
         ,@(loop for tmp in temps append `((:ldloc ,tmp)))
         ,@(%mv-mode-push)
         (:call ,(format nil "MultipleValues.Values~DMode" (length args))))))
    ((and *in-tail-position* (%mv-mode-local))
     `(,@(compile-args-array args)
       ,@(%mv-mode-push)
       (:call "MultipleValues.ValuesArrayMode")))
    ;; Two values: the shape TRUNCATE, FLOOR, ROUND, GETHASH and INTERN return, and
    ;; the one the argument array cost the most on (40 of the 64 bytes a
    ;; multiple-value return allocated). RUNTIME.VALUES2 takes them as arguments.
    ((= (length args) 2)
     `(,@(compile-binary-call args "Runtime.Values2")))
    (t `(,@(compile-args-array args)
         (:call "Runtime.Values")))))

(defun compile-subseq (args)
  "Compile (subseq seq start &optional end)."
  (let ((nargs (length args)))
    (cond
      ((or (< nargs 2) (> nargs 3))
       ;; Wrong number of args -> PROGRAM-ERROR
       (compile-expr '(error 'program-error)))
      (t
       ;; Single-valued arguments wherever the SUBSEQ form is (see GETHASH).
       `(,@(compile-value-args (list (first args) (second args) (third args)))
         (:call "Runtime.Subseq"))))))

(defun compile-concatenate (args)
  "Compile (concatenate result-type seq1 seq2 ...)."
  (if (null args)
      (compile-expr '(error 'program-error))
      (let ((rt-tmp (gen-local "RTTYP"))
            (arr-tmp (gen-local "CATARR")))
        `((:declare-local ,rt-tmp "LispObject")
          (:declare-local ,arr-tmp "LispObject[]")
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))
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

;;; ------------------------------------------------------------
;;; (unsigned-byte 64) arrays: the runtime keeps the elements as their bits
;;; (a ulong[]). On an array known to hold them, AREF reads the bits as an
;;; int64 and (SETF AREF) stores bits, so a word at or above 2^63 is a bignum
;;; only where a value has to be boxed. The 64-bit lane takes such a read as an
;;; operand that is already exact (U64-PLAN).
;;; ------------------------------------------------------------

(defun %u64-aref-helper-bits (arr idxs)
  "(AREF ARR IDX...) as the element's bits through Runtime.ArefU64Bits*, which
   also reads any other array (a declaration that does not hold, a non-simple
   vector) and then checks the value is an (unsigned-byte 64)."
  (let ((method (ecase (length idxs)
                  (1 "Runtime.ArefU64Bits")
                  (2 "Runtime.ArefU64Bits2D")
                  (3 "Runtime.ArefU64Bits3D"))))
    (if (every #'simple-expr-p idxs)
        `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
              (compile-expr arr))
          ,@(loop for idx in idxs append (compile-index-to-long idx))
          (:call ,method))
        (multiple-value-bind (arr-decl arr-pre arr-load) (%aref-arr-temp arr)
          (let ((tks (mapcar (lambda (i) (declare (ignore i)) (gen-local "NAI")) idxs)))
            `(,@arr-decl
              ,@(loop for tk in tks collect `(:declare-local ,tk "Int64"))
              ,@arr-pre
              ,@(loop for idx in idxs for tk in tks
                      append `(,@(compile-index-to-long idx) (:stloc ,tk)))
              ,@arr-load
              ,@(loop for tk in tks collect `(:ldloc ,tk))
              (:call ,method)))))))

(defun compile-u64-aref-bits (arr idxs)
  "Emit (AREF ARR IDX...) on an (unsigned-byte 64) array as the element's
   bits, an int64 on the stack. A hoisted buffer is read with ldelem.i8, with
   the helper as the arm for an array the fetch declined."
  (let ((helper (%u64-aref-helper-bits arr idxs))
        (back (array-backing-entry-for-read (list* 'aref arr idxs))))
    (if (and back (eq (fourth back) :u64))
        (compile-backing-aref-as-long back idxs helper)
        helper)))

(defun compile-as-u64-bits (val)
  "Emit VAL as the bits of an (unsigned-byte 64), an int64 on the stack. A
   value that is not an integer in [0, 2^64) signals a TYPE-ERROR, as the
   store into such an array does on the generic path.

   An element read of another (unsigned-byte 64) array is its bits as they
   are; an expression the 64-bit lane can compute is computed there, with its
   generic path (taken when an operand is not what the lane needs) checked
   once at the end; an int64 is checked to be non-negative; anything else is
   computed boxed and checked."
  (let ((*in-tail-position* nil) (*in-mv-context* nil))
    (cond
      ((numeric-array-aref-u64-p val)
       (compile-u64-aref-bits (cadr val) (cddr val)))
      ((u64-native-local-p val)
       `((:ldloc ,(u64-native-local-p val))))
      ((let* ((*u64-leaves* nil)
              (plan (u64-plan val :exact)))
         (when (and plan
                    (not (and (eq (car plan) :leaf) (eq (third plan) :obj))))
           (let* ((leaves (reverse *u64-leaves*))
                  (slow (gen-label "U64SSLOW"))
                  (done (gen-label "U64SDONE"))
                  (guards (remove-if-not #'%u64-guarded-leaf-p leaves)))
             (%u64-alias-leaf-copies
              `(,@(%u64-leaf-code leaves)
                ,@(loop for g in guards append (%u64-guard-code g slow))
                ,@(%u64-fast plan)
                ,@(when guards
                    `((:br ,done)
                      (:label ,slow)
                      ,@(%u64-generic plan)
                      (:call "Runtime.U64Bits")
                      (:label ,done))))
              leaves)))))
      ((fixnum-exact-p val)
       `(,@(compile-as-long val)
         (:call "Runtime.CheckStoreU64")))
      (t
       `(,@(compile-expr val)
         (:call "Runtime.U64Bits"))))))

(defun %u64-low-bits-plan (expr)
  "A plan computing the low 64 bits of EXPR in the 64-bit lane, as
   (PLAN . LEAVES), when EXPR is arithmetic on at least one (unsigned-byte 64)
   operand held as its bits (a local, an array element, an inline call) and on
   no operand that would have to be computed boxed. Else NIL.

   For a binding whose every value fits an int64 (a declared (UNSIGNED-BYTE 32),
   or a proven range), those bits read as an int64 are the value itself: a
   half of a 64-bit word, (LOGAND W #xFFFFFFFF) or (LDB (BYTE 32 32) W), is
   then never boxed on its way into the binding."
  (let* ((expr (fold-typed-operand expr))
         (*u64-leaves* nil)
         (plan (and (consp expr) (u64-plan-compound expr :mod))))
    (when (and plan
               (not (eq (car plan) :leaf))
               (some (lambda (l) (eq (second l) :bits)) *u64-leaves*)
               (notany (lambda (l) (eq (second l) :obj)) *u64-leaves*))
      (cons plan (reverse *u64-leaves*)))))

(defun compile-int-slot-init (expr)
  "COMPILE-EXPR-TO-LONG for the init of a LET binding held in a raw Int64 slot,
   whose every value is known to fit an int64 (by its declaration or by a range
   proof). Arithmetic on (unsigned-byte 64) words is computed in the 64-bit lane
   (%U64-LOW-BITS-PLAN) instead of being boxed and unboxed."
  (let ((p (and (not (fixnum-arith-unboxed-safe-p expr))
                (not (checked-long-arith-p expr))
                (%u64-low-bits-plan expr))))
    (if p
        (let ((*in-tail-position* nil) (*in-mv-context* nil))
          (%u64-alias-leaf-copies
           `(,@(%u64-leaf-code (cdr p))
             ,@(%u64-fast (car p)))
           (cdr p)))
        (compile-expr-to-long expr))))

(defun compile-u64-aref-set (arr idxs val)
  "Emit (SETF (AREF ARR IDX...) VAL) on an (unsigned-byte 64) array, the
   boxed value on the stack. The bits are stored as they are; the box is made
   once after the arms join, where the peephole drops it in statement
   position. ARR, the subscripts and VAL are evaluated once each, in that
   order."
  (let* ((back (let ((e (array-backing-entry (list* 'aref arr idxs))))
                 (and e (eq (fourth e) :u64) e)))
         (tk (gen-local "NAI"))
         (vk (gen-local "NAV"))
         (helper
           (let ((tks (mapcar (lambda (i) (declare (ignore i)) (gen-local "NAI")) idxs))
                 (vk (gen-local "NAV")))
             (multiple-value-bind (arr-decl arr-pre arr-load) (%aref-arr-temp arr)
               `(,@arr-decl
                 ,@(loop for tk in tks collect `(:declare-local ,tk "Int64"))
                 (:declare-local ,vk "Int64")
                 ,@arr-pre
                 ,@(loop for idx in idxs for tk in tks
                         append `(,@(compile-index-to-long idx) (:stloc ,tk)))
                 ,@(compile-as-u64-bits val)
                 (:stloc ,vk)
                 ,@arr-load
                 ,@(loop for tk in tks collect `(:ldloc ,tk))
                 (:ldloc ,vk)
                 (:call ,(ecase (length idxs)
                           (1 "Runtime.ArefSetU64Bits")
                           (2 "Runtime.ArefSetU64Bits2D")
                           (3 "Runtime.ArefSetU64Bits3D"))))))))
    `(,@(if back
            (let ((fall (gen-label "ABFALL"))
                  (done (gen-label "ABDONE"))
                  (fast `(,@(%backing-index-into back idxs tk)
                          (:declare-local ,vk "Int64")
                          ,@(compile-as-u64-bits val)
                          (:stloc ,vk)
                          (:ldloc ,(third back))
                          (:ldloc ,tk)
                          (:conv-i)
                          (:ldloc ,vk)
                          (:stelem-i8)
                          (:ldloc ,vk))))
              (ecase (%backing-arm-for back)
                (:fast fast)
                (:helper helper)
                (:both `((:ldloc ,(third back))
                         (:brfalse ,fall)
                         ,@fast
                         (:br ,done)
                         (:label ,fall)
                         ,@helper
                         (:label ,done)))))
            helper)
      (:call "Runtime.BoxU64"))))
