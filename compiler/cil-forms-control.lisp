;;; cil-forms-control.lisp: block, catch, tagbody, unwind-protect, handlers, restarts, and / or / cond
;;; Part of the CIL compiler (A2 instruction list architecture)

(in-package :dotcl.cil-compiler)

;;; ============================================================
;;; block / return-from
;;; ============================================================

;; A filter takes the exception into a local before pushing anything: Mono's
;; interpreter (untiered code) misreads it as a call argument straight off the stack.
(defun %filter-exception-to-local ()
  (let ((k (gen-local "FEX")))
    `((:declare-local ,k "Object") (:stloc ,k) (:ldloc ,k))))

(defun compile-block (name body)
  "Compile (block name body...).
   If no non-local return-from is used, skip the try/catch overhead entirely."
  (let* ((tag-key (gen-local "BTAG"))
         (tag-var-sym (compiler-name-sym (block-tag-var-name name)))
         (result-key (gen-local "BRES"))
         (end-label (gen-label "BEND"))
         (ex-key (gen-local "BEX"))
         (needs-catch (list nil))
         ;; A block in the tail of a body that took a value mode: what it
         ;; returns, by falling off its end or by RETURN-FROM, is that body's
         ;; return value, so its tail VALUES can answer the mode too.
         (value-tail-p (and *in-tail-position* (%mv-mode-local) t))
         ;; The block standing for the body of a local function that takes the
         ;; exit protocol (see %PFN-FN-FACTS) is that body: what is in its tail
         ;; is in the function's.
         (pfn-body-p (and name (symbolp name) (get name 'pfn-exit) t))
         ;; Entry format: (tag-key result-key end-label local-result-key local-end-label
         ;;                needs-catch value-tail-p)
         (*cstate* (cstate-with *cstate*
                     +cs-block-tags+
                     (acons name (list tag-key result-key end-label result-key end-label needs-catch
                                       value-tail-p)
                            (cstate-block-tags))
                     +cs-locals+ (acons tag-var-sym tag-key (cstate-locals))))
         (body-instrs (let ((*in-tail-position* (cond (pfn-body-p *in-tail-position*)
                                                      (value-tail-p :value)
                                                      (t nil)))
                            (*in-mv-context* t)
                            (*inlining-stack* (inline-guard-stack name)))
                        (compile-progn body))))
    (if (or (car needs-catch)
            ;; The tag can be read without a RETURN-FROM having been compiled:
            ;; a closure captures it from a local function's body that was
            ;; never compiled (an unused one deferred for inlining).
            (sil-references-symbol-p body-instrs tag-key))
        ;; Non-local return detected: need try/catch
        `((:declare-local ,tag-key "LispObject")
          (:ldsfld "Nil.Instance") (:ldsfld "Nil.Instance") (:call "Runtime.MakeCons")
          (:stloc ,tag-key)
          (:declare-local ,result-key "LispObject")
          ;; Outer try/finally: the tag records that the block has been left, so a
          ;; closure that outlives it signals CONTROL-ERROR instead of throwing a
          ;; transfer nothing catches.
          (:begin-exception-block)
          (:begin-exception-block)
          ,@body-instrs
          (:stloc ,result-key)
          (:leave ,end-label)
          ;; Filter: exception on the stack, answer 1 (mine) / 0 (keep unwinding).
          ;; A catch that rethrew what it did not own left a live handler funclet at
          ;; every block a non-local return crossed, so crossing N of them cost N
          ;; stacked dispatches (see compile-catch).
          (:begin-filter-block)
          ,@(%filter-exception-to-local)
          (:call "ControlFlowFilters.Transfer")
          (:ldloc ,tag-key)
          (:call "ControlFlowFilters.BlockTagMatches")
          (:begin-filter-handler)
          (:call "ControlFlowFilters.TakeTransfer")
          (:castclass "BlockReturnException")
          (:declare-local ,ex-key "BlockReturnException")
          (:stloc ,ex-key)
          (:ldloc ,ex-key) (:callvirt "BlockReturnException.get_Value")
          (:stloc ,result-key)
          (:leave ,end-label)
          (:end-exception-block)
          (:label ,end-label)
          (:begin-finally-block)
          (:ldloc ,tag-key)
          (:call "ControlFlowFilters.MarkExited")
          (:end-exception-block)
          (:ldloc ,result-key))
        ;; All returns are local: no try/catch needed
        `((:declare-local ,result-key "LispObject")
          ,@body-instrs
          (:stloc ,result-key)
          (:label ,end-label)
          (:ldloc ,result-key)))))

(defun %reset-before-neutral (instrs)
  "INSTRS, the value of a form whose values are read, preceded by
   MultipleValues.Reset when they leave the channel untouched: a call made
   before them may have left values whose first one is this value."
  (if (%mv-neutral-instrs-p instrs)
      (cons '(:call "MultipleValues.ResetOnExit") instrs)
      instrs))

(defun %innermost-pfn-entry ()
  "The block-table entry of the body of the local function being compiled when
   it takes the exit protocol and this frame is its own, else NIL."
  (let ((e (find-if (lambda (e) (and (car e) (symbolp (car e)) (get (car e) 'pfn-exit)))
                    (cstate-block-tags))))
    (and e (fourth (cdr e)) e)))

(defun %with-exit-token-check (local-fn instrs)
  "INSTRS, a call of the local function entry LOCAL-FN, followed by what looks at
   its result when the function takes the exit protocol (element 7 of its entry,
   (GS . BLOCKS)). A BLOCK-EXIT-TOKEN result is handed on when this frame is the
   body of a function taking the protocol too; else it becomes the return from
   the block it names when that is a block of this frame; else an exception."
  (let ((info (eighth local-fn)))
    (if (null info)
        instrs
        (let* ((ok (gen-label "XOK"))
               (tok (gen-local "XTOK"))
               (pf (and (not *in-finally-block*) (%innermost-pfn-entry))))
          `(,@instrs
            (:dup) (:call "ControlFlowFilters.IsExitToken") (:brfalse ,ok)
            (:declare-local ,tok "LispObject")
            (:stloc ,tok)
            ,@(cond
                (pf `((:ldloc ,tok) (:stloc ,(fourth (cdr pf))) (:leave ,(fifth (cdr pf)))))
                (t (append
                    (unless *in-finally-block*
                      (loop for b in (cdr info)
                            for e = (assoc b (cstate-block-tags))
                            when (and e (fourth (cdr e)))
                              append (let ((next (gen-label "XNEXT")))
                                       `((:ldloc ,tok) (:ldloc ,(first (cdr e)))
                                         (:call "ControlFlowFilters.TokenTagIs")
                                         (:brfalse ,next)
                                         (:ldloc ,tok) (:call "ControlFlowFilters.TokenValue")
                                         (:stloc ,(fourth (cdr e)))
                                         (:leave ,(fifth (cdr e)))
                                         (:label ,next)))))
                    `((:ldloc ,tok) (:call "ControlFlowFilters.TokenException") (:throw)))))
            (:label ,ok))))))

(defun compile-return-from (name value-expr)
  "Compile (return-from name value).
   Local return (same compilation unit, not in finally block) uses leave.
   Non-local return (from closure or finally block) throws BlockReturnException."
  (when *transparent-blocks* (%transparent-block-check name))
  (let ((entry (assoc name (cstate-block-tags))))
    (unless entry (error 'program-error :format-control "return-from: no block named ~s" :format-arguments (list name)))
    (let ((tag-key (first (cdr entry)))
          (local-result-key (fourth (cdr entry)))
          (local-end-label (fifth (cdr entry))))
      (let ((needs-catch (sixth (cdr entry))))
        ;; value-expr's multiple values become the block's return values;
        ;; preserve MvReturn wrapping so the block's caller sees MV.
        (if (and local-result-key (not *in-finally-block*))
            ;; Local return: store result and leave to block end
            `(,@(%reset-before-neutral
                 (if value-expr
                    (let ((*in-mv-context* t)
                          ;; The block's value is the body's return value (see
                          ;; compile-block): the returned form is in its tail,
                          ;; unless the way out runs cleanup code that could
                          ;; publish values of its own (UNWIND-PROTECT).
                          (*in-tail-position* (if (and (seventh (cdr entry)) (%mv-mode-local)
                                                       (not *mv-exit-cleanup*))
                                                  :value
                                                  *in-tail-position*))
                          ;; Any other block's value is not this body's return
                          ;; value: no value mode applies to the returned form.
                          (*mv-mode-local* (and (seventh (cdr entry)) (not *mv-exit-cleanup*)
                                                *mv-mode-local*)))
                      (compile-expr value-expr))
                    (emit-nil)))
              (:stloc ,local-result-key)
              (:leave ,local-end-label))
            ;; Non-local return from the body of a local function that takes the
            ;; exit protocol: the function returns a token the caller turns
            ;; into the return (see %EXIT-TOKEN-CHECK-INSTRS).
            (let ((pf (and (not *in-finally-block*) tag-key (%innermost-pfn-entry))))
              (if pf
                  (progn
                    (when needs-catch (setf (car needs-catch) t))
                    (let ((val-key (gen-local "RVAL")))
                      `((:declare-local ,val-key "LispObject")
                        ,@(if value-expr
                              (let ((*in-mv-context* t) (*mv-mode-local* nil))
                                (compile-expr value-expr))
                              (emit-nil))
                        (:stloc ,val-key)
                        (:ldloc ,tag-key)
                        (:call "ControlFlowFilters.CheckBlockLive")
                        (:ldloc ,tag-key)
                        (:ldloc ,val-key)
                        (:call "ControlFlowFilters.MakeExitToken")
                        (:stloc ,(fourth (cdr pf)))
                        (:leave ,(fifth (cdr pf))))))
            ;; Non-local return: throw BlockReturnException
            (progn
              (when needs-catch (setf (car needs-catch) t))
              (let ((val-key (gen-local "RVAL")))
                `((:declare-local ,val-key "LispObject")
                  ,@(if value-expr
                        ;; The values travel in the exception to another
                        ;; body's block: no value mode of this body applies.
                        (let ((*in-mv-context* t) (*mv-mode-local* nil))
                          (compile-expr value-expr))
                        (emit-nil))
                  (:stloc ,val-key)
                  (:ldloc ,tag-key)
                  (:call "ControlFlowFilters.CheckBlockLive")
                  (:ldloc ,tag-key)
                  (:ldloc ,val-key)
                  (:newobj "BlockReturnException")
                  (:throw)))))))))))

;;; ============================================================
;;; catch / throw
;;; ============================================================

(defun %throw-return-call (instrs)
  "The ThrowReturn.CallN that replaces INSTRS' last instruction when that is
   the call whose value is the CATCH body's value, or NIL."
  (let ((last (car (last instrs))))
    (and (consp last) (eq (car last) :callvirt)
         (cdr (assoc (second last)
                     '(("LispFunction.Invoke0" . "ThrowReturn.Call0S")
                       ("LispFunction.Invoke1" . "ThrowReturn.Call1S")
                       ("LispFunction.Invoke2" . "ThrowReturn.Call2S")
                       ("LispFunction.Invoke3" . "ThrowReturn.Call3S")
                       ("LispFunction.Invoke4" . "ThrowReturn.Call4S"))
                     :test #'equal)))))

(defun compile-catch (tag-expr body)
  "Compile (catch tag body...).
   Uses try/filter/finally for CatchThrowException with EQ tag matching.
   Pushes tag to CatchTagStack so (throw ...) inside (eval ...) can propagate
   correctly to an outer (catch ...) even across the eval boundary.

   The tag test is a CIL exception FILTER, not a catch that rethrows what it does
   not own. A rethrow restarts dispatch from inside the handler funclet, and that
   funclet stays live for the rest of the throw's journey, so N nested catches
   cost N stacked dispatches' worth of stack. The tree-walk evaluator wraps every
   interpreted call in a BLOCK, which is one CATCH per call, so a THROW out of
   deep interpreted recursion needed stack proportional to the depth it crossed
   and blew the .NET stack fatally. A filter answers \"not mine\" without touching
   the frame."
  (let* ((tag-key (gen-local "CTAG"))
         (result-key (gen-local "CRES"))
         (end-label (gen-label "CEND"))
         (ex-key (gen-local "CEX"))
         ;; Body in MV-propagating position: catch returns body's values (CL spec).
         (body-instrs (let ((*in-tail-position* nil) (*in-mv-context* t)
                            (*throw-return-forward* nil))
                        (compile-progn body)))
         (call (%throw-return-call body-instrs))
         (state-key (gen-local "CST")))
    `((:declare-local ,tag-key "LispObject")
      ;; The tag is one value: the first of several, NIL for none (CLHS 3.1.7).
      ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
          (compile-expr tag-expr))
      (:stloc ,tag-key)
      ;; Register tag so (throw ...) inside (eval ...) can propagate to this catch
      (:ldloc ,tag-key)
      (:declare-local ,state-key "Object")
      (:call "CatchTagStack.Enter")
      (:stloc ,state-key)
      (:declare-local ,result-key "LispObject")
      (:begin-exception-block)
      ,@(if call
            `(,@(butlast body-instrs) (:ldloc ,state-key) (:call ,call))
            body-instrs)
      (:stloc ,result-key)
      (:leave ,end-label)
      ;; Filter: exception on the stack, answer 1 (mine) / 0 (keep unwinding).
      (:begin-filter-block)
      ,@(%filter-exception-to-local)
      (:call "ControlFlowFilters.Transfer")
      (:ldloc ,tag-key)
      (:call "ControlFlowFilters.CatchTagMatches")
      (:begin-filter-handler)
      (:call "ControlFlowFilters.TakeTransfer")
      (:castclass "CatchThrowException")
      (:declare-local ,ex-key "CatchThrowException")
      (:stloc ,ex-key)
      (:ldloc ,ex-key) (:callvirt "CatchThrowException.get_Value")
      (:stloc ,result-key)
      (:leave ,end-label)
      (:begin-finally-block)
      (:ldloc ,state-key)
      (:call "CatchTagStack.Leave")
      (:end-exception-block)
      (:label ,end-label)
      (:ldloc ,result-key))))

(defun compile-throw (tag-expr result-expr)
  "Compile (throw tag result).
   Stores tag and result in locals before constructing the exception,
   so that non-local exits within result-expr don't leave tag on stack.
   The exception is built by Runtime.MakeCatchThrow, which signals the CLHS 5.2
   CONTROL-ERROR here, at the throw, when no CATCH for the tag is outstanding.
   Converting it later -- at the EVAL or LOAD boundary -- is too late for a
   HANDLER-CASE around the THROW, and where no boundary did it the .NET exception
   left the process."
  (let ((tag-key (gen-local "TTAG"))
        (res-key (gen-local "TRES")))
    `((:declare-local ,tag-key "LispObject")
      (:declare-local ,res-key "LispObject")
      ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
          (compile-expr tag-expr))
      (:stloc ,tag-key)
      ;; result-expr's multiple values become the catch's return values
      ;; regardless of where the throw form sits (CLHS 5.2); preserve
      ;; MvReturn wrapping like return-from does. This must not depend on
      ;; the ambient MV context of the position the throw appears in.
      ,@(let ((*in-tail-position* nil) (*in-mv-context* t))
          (compile-expr result-expr))
      (:stloc ,res-key)
      ,@(let ((tr (and (not *in-finally-block*) (cstate-throw-return))))
          (when tr
            (destructuring-bind (claim-key exit-label value-key used state-key) tr
              (setf (car used) t)
              (let ((throw-label (gen-label "TTHROW")))
                `((:ldloc ,tag-key) (:ldloc ,claim-key) (:ldloc ,state-key)
                  (:call "ThrowReturn.CanReturnS")
                  (:brfalse ,throw-label)
                  (:ldloc ,res-key) (:stloc ,value-key)
                  (:leave ,exit-label)
                  (:label ,throw-label))))))
      (:ldloc ,tag-key) (:ldloc ,res-key)
      (:call "Runtime.MakeCatchThrow")
      (:throw))))

;;; ============================================================
;;; tagbody / go
;;; ============================================================

(defun parse-tagbody-forms (forms)
  "Parse tagbody forms into segments: ((label . forms-list) ...).
   The first segment has label nil."
  (let ((segments '())
        (current-label nil)
        (current-forms '()))
    (dolist (form forms)
      (if (or (and (symbolp form) (not (null form))) (integerp form))
          (progn
            (push (cons current-label (reverse current-forms)) segments)
            (setf current-label form)
            (setf current-forms '()))
          (push form current-forms)))
    (push (cons current-label (reverse current-forms)) segments)
    (reverse segments)))

(defun sil-references-symbol-p (instrs sym)
  "Deep scan: does SYM appear anywhere in the SIL tree INSTRS (including nested
   :body lists for hoisted closures)? Used by compile-tagbody to decide whether
   the tagbody id local is referenced, by a non-local go's GoException throw
   OR by a closure capturing the id into its env, in which case the GoException
   try/catch must be kept. A structural check, immune to compile ordering."
  (labels ((walk (x)
             (cond ((eq x sym) t)
                   ;; (:load-const OBJ) holds a data literal, which may be
                   ;; circular; it is never SIL (see %SIL-REFERENCES-LOCAL-P).
                   ((and (consp x) (eq (car x) :load-const)) nil)
                   ((consp x) (or (walk (car x)) (walk (cdr x))))
                   (t nil))))
    (walk instrs)))

(defun %lower-go-locals (instrs index-key label-for-index)
  "Replace this tagbody's (:go-local IDX INDEX-KEY LOOP-LABEL) placeholders.
LABEL-FOR-INDEX NIL gives the dispatch form (store the index, leave to the
dispatch); otherwise it is a function of the segment index returning a label to
branch to straight. Placeholders belonging to an enclosing tagbody carry a
different INDEX-KEY and are passed through for that tagbody to lower."
  (loop for ins in instrs
        append (if (and (consp ins) (eq (car ins) :go-local)
                        (eq (third ins) index-key))
                   (if label-for-index
                       `((:br ,(funcall label-for-index (second ins))))
                       `((:ldc-i4 ,(second ins))
                         (:stloc ,(third ins))
                         (:leave ,(fourth ins))))
                   (list ins))))

(defun %simple-loop-go-position (instrs index-key)
  "The segment index a tagbody's ONE own GO targets, when this instruction list
is a simple loop: exactly one GO belonging to this tagbody, standing as the last
instruction (a dead POP of the GO's value may follow), with every exception
region it opened closed again. Otherwise NIL.

That shape is what DOTIMES, DO and LOOP expand to, and it needs none of the
dispatch machinery: control reaches the tag by falling into it and returns there
by branching, so the index variable, the done flag, the switch and the per-
iteration LEAVE are all bookkeeping for a re-entry that cannot happen. The
conditions are read off the COMPILED instructions rather than the source,
because a macro can expand into a GO and only the compiled form says how many
there really are.

The balanced-region test is what makes the plain branch legal: a BR may not
leave a protected region, and balance means the GO is not inside one."
  (let ((mine (loop for ins in instrs
                    when (and (consp ins) (eq (car ins) :go-local)
                              (eq (third ins) index-key))
                      collect ins)))
    (when (and (= (length mine) 1)
               (= (count-if (lambda (i) (and (consp i) (eq (car i) :begin-exception-block)))
                            instrs)
                  (count-if (lambda (i) (and (consp i) (eq (car i) :end-exception-block)))
                            instrs)))
      (let* ((tail (last instrs 2))
             (last-ins (car (last instrs))))
        (when (or (eq (car tail) (car mine))
                  (and (eq last-ins (car mine))))
          ;; last, or second-to-last followed by the POP of its value
          (when (or (eq last-ins (car mine))
                    (equal last-ins '(:pop)))
            (second (car mine))))))))

(defun compile-tagbody (forms)
  (let ((cand *bounded-loop-candidate*)
        (*bounded-loop-candidate* nil))
    (or (and cand
             (eq forms (cdr (first cand)))
             (%compile-bounded-loop forms (second cand) (third cand) (fourth cand)
                                    (fifth cand) (sixth cand)))
        (%compile-tagbody forms nil))))

(defun %bounded-loop-body (forms var limit inclusive)
  "The forms of the counting loop FORMS between its end test and its step, when
   FORMS is TAG (IF (>= VAR LIMIT) EXIT) BODY... (SETQ VAR step) (GO TAG) [END]
   and BODY has no tags. VAR is then below LIMIT throughout BODY."
  (destructuring-bind (&optional tag test &rest rest) forms
    (let* ((end (car (last rest)))
           (rest (if (and end (atom end)) (butlast rest) rest))
           (n (length rest))
           (step (and (>= n 2) (nth (- n 2) rest)))
           (go (and (>= n 2) (nth (- n 1) rest)))
           (body (and (>= n 2) (subseq rest 0 (- n 2)))))
      (and tag (atom tag)
           (consp test)
           (or (and (eq (car test) 'if) (= (length test) 3))
               (eq (car test) 'when))
           (equal (second test) (list (if inclusive '> '>=) var limit))
           (consp step) (eq (car step) 'setq) (= (length step) 3)
           (eq (second step) var)
           (%induction-step-value-p (third step) var)
           (equal go (list 'go tag))
           (every #'consp body)
           body))))

(defmacro %note-bounded-width (entry width place)
  `(let ((cell (assoc ,entry ,place :test #'eq)))
     (if cell
         (setf (cdr cell) (max (cdr cell) ,width))
         (push (cons ,entry ,width) ,place))))

(defun %bounded-loop-entries (body var &optional pack-ok)
  "Alist (ENTRY . M) of the hoisted rank 1 buffers BODY reads or writes as
   (AREF V VAR), M 1, or as a DOTCL-SIMD read (X-AREF V VAR) or (X-AREF V (+ VAR
   K)), M the value of PACK-OK (a function of the span K + lane count, or NIL
   when the read does not count)."
  (let ((found '()))
    (labels ((walk (x)
               (when (and (consp x) (not (eq (car x) 'quote)))
                 (when (and (eq (car x) 'aref)
                            (consp (cdr x)) (consp (cddr x)) (null (cdddr x))
                            (eq (third x) var))
                   (let ((e (array-backing-entry x)))
                     (when (and e
                                (null (backing-entry-dims e))
                                (not (eq (%backing-arm-for e) :helper)))
                       (%note-bounded-width e 1 found))))
                 (when (and pack-ok (symbolp (car x))
                            (consp (cdr x)) (consp (cddr x)) (null (cdddr x)))
                   (let* ((info (%simd-name-info (car x)))
                          (k (%simd-index-offset (third x) var))
                          (m (and info k (eq (second (second info)) :aref)
                                  (funcall pack-ok (+ k (/ 128 (%simd-lane-bits (first info)))))))
                          (e (and m (%simd-aref-backing (first info) (second x) (third x)))))
                     (when e (%note-bounded-width e m found))))
                 (loop for tail on x
                       do (walk (car tail))
                       while (consp (cdr tail))))))
      (walk body))
    (nreverse found)))


(defun %compile-bounded-loop (forms var limit inclusive &optional delta start)
  "A counting loop whose variable subscripts hoisted buffers, emitted twice:
   once with each access unchecked, taken when the limit is within every
   buffer, and once as before. NIL when the loop is not of that shape. A pack
   read of the elements VAR + K to VAR + K + W - 1 counts with M the power of 2
   at or above K + W, when M divides START and DELTA: VAR is then a multiple of M
   below the limit rounded up to M, so the limit at most the buffer's length
   rounded down to M keeps the read in."
  (let* ((body (%bounded-loop-body forms var limit inclusive))
         (key (and body (symbolp var) (not (boxed-var-p var)) (lookup-local var)))
         (names (list (var-name var)))
         (entries
           (and key
                (not (member (var-name var)
                             (find-mutated-and-captured-vars body names)
                             :test #'string=))
                (or (integerp limit)
                    (and (lookup-local limit)
                         (not (boxed-var-p limit))
                         (not (global-special-p limit))
                         (fixnum-long-lowerable-p limit)
                         (not (member (var-name limit)
                                      (find-mutated-and-captured-vars
                                       forms (list (var-name limit)))
                                      :test #'string=))))
                (%backing-duplicable-p forms)
                (%bounded-loop-entries
                 body var
                 (and (%simd-lowering-p) (integerp delta) (integerp start)
                      (lambda (span)
                        (let ((m (ash 1 (integer-length (1- span)))))
                          (and (zerop (mod delta m)) (zerop (mod start m)) m))))))))
    ;; The body is compiled twice here (and again by the caller when the result is too long), and
    ;; the next loop of a nest sits in it: from the third level on the loop is emitted once.
    (when (and entries (< *bounded-loop-depth* +unswitch-depth-limit+))
      (let* ((*bounded-loop-depth* (1+ *bounded-loop-depth*))
             (slow (gen-label "BLSLOW"))
             (done (gen-label "BLDONE"))
             (limit-code (if (integerp limit)
                             `((:ldc-i8 ,limit))
                             (let ((*in-tail-position* nil) (*in-mv-context* nil))
                               (compile-expr-to-long limit))))
             (refs (loop for (e . m) in entries collect (list (third e) (gen-local "BLREF") m)))
             (tests (loop for (e . m) in entries
                          for back = (third e)
                          append `(,@(unless (and (eq *backing-arm* :fast)
                                                  (member back *array-backing-tested*))
                                       `((:ldloc ,back) (:brfalse ,slow)))
                                   ,@limit-code
                                   (:ldloc ,back) (:ldlen) (:conv-i8)
                                   ,@(when (> m 1) `((:ldc-i8 ,(- m)) (:and)))
                                   (,(if inclusive :bge :bgt) ,slow))))
             (ref-init (loop for (e) in entries
                             for r = (second (assoc (third e) refs))
                             append `((:declare-local ,r ,(%backing-ref-type (fourth e)))
                                      (:ldloc ,(third e))
                                      (:call ,(concatenate 'string "Runtime.Data"
                                                           (%backing-ref-suffix (fourth e))))
                                      (:stloc ,r))))
             (fast (%compile-tagbody forms (cons (cons key refs) body))))
        (when (<= (length fast) *unswitch-body-limit*)
          `(,@tests
            ,@ref-init
            ,@fast
            (:br ,done)
            (:label ,slow)
            ,@(%compile-tagbody forms nil)
            (:label ,done)))))))

(defun %compile-tagbody (forms region)
  "Compile (tagbody forms...).
   Local go (same tagbody, not in closure) uses index + leave instead of exceptions.
   Non-local go (from closures) still uses GoException throw/catch.
   REGION is (BOUND . BODY-FORMS): BOUND is pushed on *BOUNDED-INDEX* while
   those forms compile."
  (let* ((segments (parse-tagbody-forms forms))
         (tb-id-key (gen-local "TBID"))
         (index-key (gen-local "TBIDX"))
         (done-key (gen-local "TBDONE"))
         (end-label (gen-label "TBEND"))
         (loop-label (gen-label "TBLOOP"))
         (leave-label (gen-label "TBLEAVE"))
         (ex-key (gen-local "TBEX"))
         ;; Build tag->index map (tags start from index 0)
         (tag-counter 0)
         (tag-indices
           (let ((result '()))
             (dolist (seg segments)
               (when (car seg)
                 (push (cons (car seg) tag-counter) result))
               (incf tag-counter))
             (reverse result)))
         ;; Segment labels
         (seg-labels (loop for i from 0 below (length segments)
                           collect (gen-label (format nil "SEG~d" i))))
         ;; Synthetic variable for the tagbody ID (for non-local go from closures)
         (tb-var-name (concatenate 'string "%TBID-" (symbol-name tb-id-key) "%"))
         (tb-var-sym (compiler-name-sym tb-var-name))
         (*cstate* (cstate-with *cstate* +cs-locals+
                                (acons tb-var-sym tb-id-key (cstate-locals))))
         ;; needs-catch: shared cell flagged by compile-go when it emits a
         ;; NON-LOCAL (GoException throw) go targeting this tagbody. If none is
         ;; emitted while compiling the segments, the catch is dead and omitted
         ;; (mirrors compile-block). Eliding the per-iteration try is a large
         ;; win for hot loops (dotimes/do/loop -> tagbody).
         (needs-catch (list nil))
         ;; has-go: shared cell flagged by compile-go when ANY go targets this
         ;; tagbody. A flagged tagbody is a (potential) loop and gets an
         ;; interrupt safepoint on every re-entry (the dispatch label, or each
         ;; GO when they branch straight to their tags); a straight-line
         ;; tagbody (no go at all) does not.
         (has-go (list nil))
         ;; Extended format: (tag-name tb-var-name tb-id-key label-idx index-key
         ;;                    leave-label needs-catch has-go). 5th/6th enable
         ;; local go; 7th lets a captured non-local go flag this tagbody's
         ;; needs-catch; 8th records that a go exists at all.
         (*cstate* (cstate-with *cstate* +cs-go-tags+
                     (append
                      (mapcar (lambda (ti) (list (car ti) tb-var-name tb-id-key (cdr ti)
                                                 index-key loop-label needs-catch has-go))
                              tag-indices)
                      (cstate-go-tags))))
         ;; Compile segments NOW so compile-go runs (and may flag needs-catch)
         ;; before we decide whether the GoException try/catch is needed.
         (seg-instrs (let ((*in-tail-position* nil) (*in-mv-context* nil))
                       (loop for seg in segments
                             for label in seg-labels
                             append `((:label ,label)
                                      ,@(loop for form in (cdr seg)
                                              append (let ((*bounded-index*
                                                             (if (and region
                                                                      (member form (cdr region) :test #'eq))
                                                                 (cons (car region) *bounded-index*)
                                                                 *bounded-index*)))
                                                       (compile-and-pop form)))))))
         ;; Interrupt safepoint on the dispatch label = the shared back-edge of
         ;; every loop this tagbody expresses. Only when a go exists (otherwise
         ;; the tagbody runs straight through once) and the body is not
         ;; declared (optimize (safety 0)).
         (poll-instrs (when (and (car has-go) (not (cstate-no-safepoint)))
                        '((:call "ConditionSystem.PollInterrupt"))))
         ;; The one shape that needs no dispatch at all -- see
         ;; %SIMPLE-LOOP-GO-POSITION. Asked only where the GoException catch is
         ;; already ruled out, so every other tagbody reaches the code below
         ;; with exactly the instructions it had before.
         (simple-target
           (unless (or (car needs-catch)
                       (sil-references-symbol-p seg-instrs tb-id-key))
             (%simple-loop-go-position seg-instrs index-key))))
    (when simple-target
      (let ((target-label (nth simple-target seg-labels)))
        (return-from %compile-tagbody
          `(,@(loop for ins in (%lower-go-locals
                                seg-instrs index-key
                                (lambda (i) (nth i seg-labels)))
                    append (if (equal ins `(:label ,target-label))
                               (cons ins poll-instrs)
                               (list ins)))
            (:label ,end-label)
            (:call "MultipleValues.Reset")
            ,@(emit-nil)))))
    ;; No GoException can land here: every GO is a LEAVE straight to its
    ;; segment, with the interrupt safepoint in front of it. LEAVE rather than
    ;; BR because a GO may sit inside an expression with values on the stack,
    ;; which LEAVE empties; the target is in the same region as the dispatch
    ;; label the GO used to leave to, so the transfer is legal wherever that
    ;; one was. Before, each GO stored the segment index and left to a switch
    ;; that branched to the segment, which the JIT does not see as a loop:
    ;; a LOOP (two GOs, so not a simple loop) cost several times its body.
    (unless (or (car needs-catch)
                ;; tb-id-key referenced in the body => a non-local go's throw
                ;; or a closure capturing the tagbody id => the catch is
                ;; required. This structural check backs up the needs-catch
                ;; side-effect flag.
                (sil-references-symbol-p seg-instrs tb-id-key))
      (return-from %compile-tagbody
        `(,@(loop for ins in seg-instrs
                  append (if (and (consp ins) (eq (car ins) :go-local)
                                  (eq (third ins) index-key))
                             `(,@poll-instrs
                               (:leave ,(nth (second ins) seg-labels)))
                             (list ins)))
          (:label ,end-label)
          (:call "MultipleValues.Reset")
          ,@(emit-nil))))
    (setf seg-instrs (%lower-go-locals seg-instrs index-key nil))
    ;; A non-local go can land here: keep the GoException try/catch, with the
    ;; local GOs going through the same dispatch as the caught ones.
    `((:declare-local ,tb-id-key "LispObject")
      (:ldsfld "Nil.Instance") (:ldsfld "Nil.Instance") (:call "Runtime.MakeCons")
      (:stloc ,tb-id-key)
      (:declare-local ,index-key "Int32")
      (:ldc-i4 0) (:stloc ,index-key)
      (:declare-local ,done-key "Boolean")
      (:ldc-i4 0) (:stloc ,done-key)
      ;; Outer try/finally: records that the tagbody has been left (see compile-block).
      (:begin-exception-block)
      (:label ,loop-label)
      ,@poll-instrs
      (:ldloc ,done-key) (:brtrue ,end-label)
      (:begin-exception-block)
      (:ldloc ,index-key)
      (:switch ,seg-labels)
      (:br ,leave-label)
      ,@seg-instrs
      (:label ,leave-label)
      (:ldc-i4 1) (:stloc ,done-key)
      (:leave ,loop-label)
      ;; Filter: exception on the stack, answer 1 (mine) / 0 (keep unwinding).
      ;; A catch that rethrew another tagbody's GO left a live handler funclet at
      ;; every level it crossed: a GO out of 20000 nested tagbodies took 4.1s and
      ;; 50000 killed the process (see compile-catch).
      (:begin-filter-block)
      ,@(%filter-exception-to-local)
      (:call "ControlFlowFilters.Transfer")
      (:ldloc ,tb-id-key)
      (:call "ControlFlowFilters.GoTagbodyMatches")
      (:begin-filter-handler)
      (:call "ControlFlowFilters.TakeTransfer")
      (:castclass "GoException")
      (:declare-local ,ex-key "GoException")
      (:stloc ,ex-key)
      (:ldloc ,ex-key) (:callvirt "GoException.get_TargetLabel")
      (:stloc ,index-key)
      (:leave ,loop-label)
      (:end-exception-block)
      (:br ,loop-label)
      (:label ,end-label)
      (:begin-finally-block)
      (:ldloc ,tb-id-key)
      (:call "ControlFlowFilters.MarkExited")
      (:end-exception-block)
      (:call "MultipleValues.Reset")  ; tagbody return is always single NIL
      ,@(emit-nil))))

(defun compile-go (tag)
  "Compile (go tag).
   Local go (5th element in *go-tags* entry present) sets index + leave.
   Non-local go (from closures, no 5th element) uses GoException throw."
  (let ((entry (assoc tag (cstate-go-tags))))
    (unless entry (error 'program-error :format-control "go: no tagbody tag named ~s" :format-arguments (list tag)))
    ;; Format: (tag-name tb-var-name tb-id-key label-idx [index-key loop-label])
    (let ((tb-id-key (third entry))
          (label-idx (fourth entry))
          (local-index-key (fifth entry))
          (local-loop-label (sixth entry))
          (needs-catch (seventh entry))
          (has-go (eighth entry)))
      ;; Any go makes the target tagbody a potential loop -> it places an
      ;; interrupt safepoint on its dispatch label (see compile-tagbody).
      (when has-go (setf (car has-go) t))
      (if (and local-index-key (not *in-finally-block*))
          ;; Local go, left as a placeholder for the target COMPILE-TAGBODY to
          ;; lower. It has three lowerings -- a plain branch when the tagbody
          ;; turns out to be a simple loop, a LEAVE to the tag when no
          ;; GoException can reach the tagbody, the dispatch-index store
          ;; otherwise -- and which one applies is not known until every
          ;; segment has been compiled. The index key identifies the target, so a GO to an OUTER
          ;; tagbody passes through the inner one untouched.
          `((:go-local ,label-idx ,local-index-key ,local-loop-label))
          ;; Non-local go (or go from finally block): throw GoException. Flag the
          ;; target tagbody so it keeps its GoException catch.
          (progn
            (when needs-catch (setf (car needs-catch) t))
            `((:ldloc ,tb-id-key)
              (:call "ControlFlowFilters.CheckTagbodyLive")
              (:ldloc ,tb-id-key)
              (:ldc-i4 ,label-idx)
              (:newobj "GoException")
              (:throw)))))))

;;; ============================================================
;;; unwind-protect
;;; ============================================================

(defun %compile-held-form (form mode-p)
  "FORM, whose values MultipleValues.HoldValues takes the moment it returns (see
   %HELD-VALUES-RESTORE). With MODE-P its tail answers the capture mode: the values
   come back in the thread state without an MvReturn, as for %COMPILE-FOR-CAPTURE.
   A RETURN-FROM out of FORM skips the hold, so its values travel in an MvReturn
   (*MV-EXIT-CLEANUP*)."
  (if mode-p
      (let ((*in-tail-position* :value) (*in-mv-context* t) (*mv-exit-cleanup* t)
            (*mv-mode-local* +mv-mode-capture+)
            (*mv-expect-args* (if (consp form) (cons (cdr form) :capture) nil)))
        (compile-expr form))
      (let ((*in-tail-position* nil) (*in-mv-context* t) (*mv-exit-cleanup* t))
        (compile-expr form))))

(defun %held-values-mode-p ()
  "True when the values of the form being compiled go to a consumer that named a
   value mode: they can then be held and handed on without an MvReturn."
  (and *mv-mode-elision* *in-tail-position* (%mv-mode-local) t))

(defun %held-values-restore (held-key result-key mode-p)
  `((:ldloc ,held-key) (:ldloc ,result-key)
    ,@(if mode-p (%mv-mode-push) '((:ldc-i4 -1)))
    (:call "MultipleValues.RestoreValues")))

(defun compile-unwind-protect (protected-form cleanup-forms)
  "Compile (unwind-protect protected cleanup...).
   Holds the protected form's values across the cleanup (MultipleValues.HoldValues)
   and publishes them again AFTER the exception block, so only on a normal return."
  (let ((result-key (gen-local "UPRES"))
        (held-key (gen-local "UPHELD"))
        (normal-label (gen-label "UPNORM"))
        (mode-p (%held-values-mode-p)))
    `((:declare-local ,result-key "LispObject")
      (:declare-local ,held-key "MvHeld")
      ,@(emit-nil) (:stloc ,result-key)
      (:begin-exception-block)
      ,@(%compile-held-form protected-form mode-p)
      ;; If protected-form did a non-local exit (leave/throw),
      ;; the stloc below is unreachable. Use a label so dead-code
      ;; elimination in the assembler can handle it cleanly.
      (:label ,normal-label)
      (:stloc ,result-key)
      (:ldloc ,result-key) (:call "MultipleValues.HoldValues") (:stloc ,held-key)
      (:begin-finally-block)
      ,@(let ((*in-tail-position* nil)
              (*in-finally-block* t))
           (loop for form in cleanup-forms
                 append (compile-and-pop form)))
      (:end-exception-block)
      ,@(%held-values-restore held-key result-key mode-p))))

(defun compile-multiple-value-prog1 (first-form rest-forms)
  (cond
    ((null rest-forms) (compile-expr first-form))
    ((and (not *in-mv-context*) (not *in-tail-position*))
     (let ((tmp (gensym "MVP1")))
       (compile-expr `(let ((,tmp ,first-form)) ,@rest-forms ,tmp))))
    (t
     (let ((result-key (gen-local "MP1RES"))
           (held-key (gen-local "MP1HELD"))
           (mode-p (%held-values-mode-p)))
       `(,@(%compile-held-form first-form mode-p)
         (:declare-local ,result-key "LispObject")
         (:stloc ,result-key)
         (:declare-local ,held-key "MvHeld")
         (:ldloc ,result-key) (:call "MultipleValues.HoldValues") (:stloc ,held-key)
         ,@(let ((*in-tail-position* nil))
             (loop for form in rest-forms append (compile-and-pop form)))
         ,@(%held-values-restore held-key result-key mode-p))))))

;;; ============================================================
;;; handler-case
;;; ============================================================

(defun %handler-type-spec-load (type-spec)
  "Instructions that push the handler clause's type specifier as a LispObject for
   Runtime.Typep. A bare symbol / class metaobject loads its name symbol itself,
   package and all (loading it by bare name gave a DOTCL-INTERNAL placeholder that
   only matched by name); a compound specifier; (or ...), (and ...),
   (member ...), a deftype, etc.; is loaded as its literal list so Typep evaluates
   it (was previously stringified into one bogus symbol, so (or ...) never matched)."
  (cond
    ((symbolp type-spec) (compile-quoted type-spec))
    ((and (typep type-spec 'standard-object) (class-name type-spec))
     (compile-quoted (class-name type-spec)))
    (t (compile-quoted type-spec))))

(defun compile-handler-case (body-form clauses)
  "Compile (handler-case body (type1 (var) handler1...) (type2 () handler2...) ...).
   Uses HandlerClusterStack so handler-case takes priority over enclosing handler-bind
   handlers (innermost wins, per CL spec).

   Key CIL constraint: brfalse/brtrue inside a catch block can only branch WITHIN that
   catch block; use 'leave' to exit the catch block to a label in an enclosing try."
  (let* ((result-key (gen-local "HCRES"))
         (outer-end-label (gen-label "HCOUTEREND"))
         (inner-end-label (gen-label "HCINNEREND"))
         (cond-key (gen-local "HCCOND"))
         (exobj-key (gen-local "HCEXOBJ"))
         (specs-key (gen-local "HCSPECS"))
         (ci-key (gen-local "HCCIKEY"))
         ;; Parse clauses: each is (type-name var handler-body)
         (parsed (mapcar (lambda (clause)
                           (let* ((type-spec (car clause))
                                  (lambda-list (cadr clause))
                                  ;; A clause lambda list is (var) or (); when it
                                  ;; is (nil) the name is the constant NIL, which
                                  ;; cannot be bound -- that used to read as "no
                                  ;; variable" and pass silently.
                                  (var (if lambda-list
                                           (check-binding-name (car lambda-list)
                                                               "HANDLER-CASE")
                                           nil))
                                  (handler-body (cddr clause)))
                             (list type-spec var handler-body)))
                         clauses))
         (n (length parsed))
         ;; Generate unified clause body labels (shared across all catch dispatches)
         (clause-labels (loop for i from 0 below n
                              collect (gen-label (format nil "HCCLAUSE~d" i))))
         ;; Skip labels for the ceq+brfalse dispatch, which must stay WITHIN the
         ;; filter's handler block (a brfalse cannot leave a funclet).
         (ci-skip-labels (loop for i from 0 below n
                               collect (gen-label (format nil "HCCISKIP~d" i)))))
    `(;; No tag object: the cluster array built below is allocated once per entry
      ;; and is already unique, so it identifies this handler-case invocation.
      ;; Shared condition local (set before dispatching to clause body)
      (:declare-local ,cond-key "LispObject")
      ,@(emit-nil) (:stloc ,cond-key)
      ;; Matched clause index, written by the filter and read by its handler.
      (:declare-local ,ci-key "Int32")
      ;; The in-flight exception, kept by the filter for the handler.
      (:declare-local ,exobj-key "Object")
      ;; The cluster: one HandlerBinding per clause, carrying the type specifier the
      ;; filter matches on and the clause index. Kept in a local because the filter
      ;; reads the specifiers from it -- a second array of the same specifiers, and a
      ;; tag object to identify the invocation, were 56 bytes on every entry saying
      ;; what this array already says.
      (:declare-local ,specs-key "HandlerBinding[]")
      (:ldc-i4 ,n)
      (:newarr "HandlerBinding")
      ,@(loop for (type-spec var handler-body) in parsed
              for i from 0
              append `((:dup) (:ldc-i4 ,i)
                        ;; Type specifier (symbol/class name, or compound list literal)
                        ,@(%handler-type-spec-load type-spec)
                        ;; The clause is identified by (cluster, index); the binding
                        ;; carries the index, so entering a handler-case builds no
                        ;; handler function per clause.
                        (:ldc-i4 ,i)
                        (:newobj "HandlerBindingHcCluster")
                        (:stelem-ref)))
      (:dup) (:stloc ,specs-key)
      (:call "HandlerClusterStack.PushCluster")
      ;; Result local
      (:declare-local ,result-key "LispObject")
      ,@(emit-nil) (:stloc ,result-key)
      ;; try-catch-FINALLY: body + exception dispatch. PopCluster lives in the finally
      ;; so it runs on EVERY exit from the try (normal, matched-clause :leave, rethrow,
      ;; and non-local :leave out of the body). Handler bodies still run with the
      ;; cluster removed (per CL spec) because the :leave to a clause label runs the
      ;; finally before the clause body executes.
      (:begin-exception-block)
      ;; Body in MV-propagating position: handler-case returns body's values (CL spec).
      ;; Self-TCO inside handler-case: use `leave` to exit the try block and prepend
      ;; PopCluster so each iteration has a clean handler stack.
      ,@(let ((*in-try-block* t)         ; protect: :ret invalid in try/catch region
              (*in-mv-context* t)
              ;; When TCO is active, mark the try/catch context so the self-call uses
              ;; `leave TCOLOOP` instead of `ret`. The cluster PopCluster on that leave
              ;; is now handled by the finally block (see below), so we no longer
              ;; prepend it to the leave-instrs (doing so would double-pop).
              (*cstate* (cstate-with *cstate*
                          +cs-tco-in-try-catch+
                          (if (cstate-tco-self-name) t (cstate-tco-in-try-catch)))))
          (compile-expr body-form))
      (:stloc ,result-key)
      (:leave ,inner-end-label)
      ;; One filter for every exception shape this handler-case can take: its own
      ;; HandlerCaseInvocationException (the main path, thrown by the handler
      ;; function HandlerClusterStack.Signal calls), a LispErrorException whose
      ;; condition matches a clause type, and a raw .NET exception wrapped into a
      ;; condition that does. The filter answers "which clause" (or -1) without
      ;; entering the frame, so a signal that crosses N nested handler-cases costs
      ;; one dispatch instead of N stacked ones. Catching everything and rethrowing
      ;; what did not match, the shape this replaces, left a live handler funclet
      ;; at every level, and deep recursion with a handler-case per level died as an
      ;; uncatchable .NET StackOverflowException.
      (:begin-filter-block)
      (:stloc ,exobj-key) (:ldloc ,exobj-key)
      (:call "ControlFlowFilters.Transfer")
      (:ldloc ,specs-key)
      (:call "ControlFlowFilters.HandlerCaseClauseCluster")
      (:stloc ,ci-key)
      (:ldloc ,ci-key) (:ldc-i4 -1) (:cgt)
      (:begin-filter-handler)
      (:pop)
      ;; The clause variable binds the condition the filter matched on.
      (:ldloc ,exobj-key)
      (:call "ControlFlowFilters.TakeTransfer")
      (:call "ControlFlowFilters.HandlerCaseCondition")
      (:stloc ,cond-key)
      ,@(loop for label in clause-labels
              for ci-skip in ci-skip-labels
              for i from 0
              append `((:ldloc ,ci-key) (:ldc-i4 ,i) (:ceq) (:brfalse ,ci-skip) ;; within handler
                       (:leave ,label) ;; exit handler to clause body after the block
                       (:label ,ci-skip)))
      ;; Unreachable: the filter only answered 1 for an index in range.
      (:leave ,inner-end-label)
      ;; Finally: pop the cluster on EVERY exit from the try: normal completion, a
      ;; matched-clause :leave, a no-match rethrow, AND a non-local :leave
      ;; (return-from / go) out of the body. The old code popped only on the catch
      ;; and normal-exit paths, so a :leave *through the body* leaked the handler
      ;; cluster; a later signal then fired the stale handler and threw
      ;; HandlerCaseInvocationException past this frame (the make-host-2 irrat crash).
      ;; Clauses still run outside the cluster: the :leave to a clause label runs the
      ;; finally (pop) before the clause body executes.
      (:begin-finally-block)
      (:call "HandlerClusterStack.PopCluster")
      (:end-exception-block) ;; end try-catch-finally
      ;; Normal exit: finally already popped; jump to end
      (:label ,inner-end-label)
      (:br ,outer-end-label)
      ;; Clause bodies (AFTER try-catch, outside exception block). The cluster was
      ;; already popped by the finally when the catch did :leave to the clause label.
      ,@(loop for (type-spec var handler-body) in parsed
              for label in clause-labels
              append (multiple-value-bind (declared-specials real-body)
                         (extract-specials handler-body)
                       (let* ((var-is-special (and var
                                                   (or (member var declared-specials)
                                                       (global-special-p var))))
                              (*specials* (append declared-specials *specials*))
                              ;; The clause var shadows a symbol macro of the
                              ;; same name, as a LET binding does (CLHS 3.4.2).
                              (*symbol-macros* (if var
                                                   (%symbol-macros-without (list var))
                                                   *symbol-macros*))
                              ;; The clause var binds the condition into a plain
                              ;; LispObject slot; it must SHADOW an enclosing boxed
                              ;; var of the same name (e.g. a boxed LOOP variable
                              ;; captured by this same-named handler), else its
                              ;; reference compiles to a boxed `slot[0]` ldelem-ref
                              ;; on the condition object -> ArrayTypeMismatch.
                              (*cstate*
                               (if (and var (not var-is-special))
                                   (cstate-with *cstate*
                                     +cs-locals+
                                     (let ((var-key (gen-local "HCV")))
                                       (acons var var-key (cstate-locals)))
                                     +cs-boxed-vars+
                                     (remove-if (lambda (x)
                                                  (string= (if (symbolp x) (var-name x) x)
                                                           (var-name var)))
                                                (cstate-boxed-vars)))
                                   *cstate*)))
                         (let ((var-key (if (and var (not var-is-special))
                                            (lookup-local var) nil)))
                           `((:label ,label)
                             ;; cluster already popped by the finally on the catch's
                             ;; :leave to this clause label (was a manual pop here)
                             ,@(cond
                                 (var-is-special
                                  ;; Bind as special (dynamic) variable with try/finally.
                                  ;; Set *in-try-block* before compiling body so TCO hooks
                                  ;; emit `leave` instead of `br` (mirrors compile-let).
                                  (let ((tmp-key (gen-local "HCTMP")))
                                    (xref-record-use :binds var)
                                    `((:declare-local ,tmp-key "LispObject")
                                      (:ldloc ,cond-key) (:stloc ,tmp-key)
                                      ,@(compile-let-with-specials
                                         '()
                                         `(,@(compile-sym-lookup var)
                                           (:castclass "Symbol")
                                           (:ldloc ,tmp-key)
                                           (:call "DynamicBindings.Push"))
                                         (let ((*in-try-block* (or *in-try-block* (list var))))
                                           (compile-progn real-body))
                                         (list var)))))
                                 (var
                                  `((:declare-local ,var-key "LispObject")
                                    (:ldloc ,cond-key) (:stloc ,var-key)
                                    ,@(compile-progn real-body)))
                                 (t (compile-progn real-body)))
                             (:stloc ,result-key)
                             (:br ,outer-end-label))))))
      (:label ,outer-end-label)
      (:ldloc ,result-key))))

;;; ============================================================
;;; restart-case
;;; ============================================================

(defun %compile-restart-clause (name params handler-body args-key)
  "Bind the clause's ordinary lambda list to ARGS-KEY and compile its body."
  (multiple-value-bind (required optional key rest-param aux allow-other-keys-p has-key-p)
      (parse-lambda-list params)
    ;; Declarations are handled by the surrounding compiler state, as before.
    (let ((effective-body handler-body))
      (loop while (and effective-body
                       (consp (car effective-body))
                       (eq (caar effective-body) 'declare))
            do (pop effective-body))
      (let* ((all-params (append required
                                 (mapcar #'car optional)
                                 (remove nil (mapcar #'third optional))
                                 (mapcar #'second key)
                                 (remove nil (mapcar #'fourth key))
                                 (when rest-param (list rest-param))))
             (param-names (mapcar #'var-name all-params))
             (local-keys (gen-param-local-keys all-params))
             (needs-boxing (params-needs-boxing effective-body all-params))
             (*cstate* (cstate-with *cstate*
                         +cs-locals+ (append local-keys (cstate-locals))
                         +cs-boxed-vars+
                         (append (params-boxed-vars all-params needs-boxing)
                                 (remove-if (lambda (v)
                                              (member (var-name v) param-names
                                                      :test #'string=))
                                            (cstate-boxed-vars)))))
             (*symbol-macros* (%symbol-macros-without all-params))
             (n-required (length required))
             (key-start (+ n-required (length optional)))
             (args-array `((:ldloc ,args-key)))
             (param-instrs
               (compile-args-param-instrs
                required optional key rest-param local-keys n-required key-start
                (lambda (i) `((:ldloc ,args-key) (:ldc-i4 ,i) (:ldelem-ref)))
                args-array
                (lambda (masked)
                  (remove-if (lambda (entry)
                               (member (var-name (car entry)) masked :test #'string=))
                             (cstate-locals)))))
             (arity-instrs
               (compile-args-arity-instrs name optional key rest-param has-key-p
                                          n-required args-array))
             (key-check-instrs
               (compile-args-key-check-instrs name key has-key-p allow-other-keys-p
                                              key-start args-array)))
        `(,@arity-instrs ,@key-check-instrs ,@param-instrs
          ,@(compile-progn (wrap-aux-body aux effective-body)))))))

(defun compile-restart-case (body-form clauses)
  "Compile (restart-case body (name (params...) body...) ...).
   Uses RestartClusterStack + RestartInvocationException matching on tag.
   Handler bodies execute AFTER try-catch with PopCluster first,
   so they run outside the restart-case's dynamic scope."
  (let* ((is-signaling-body (labels ((check-signaling (form)
                                        (cond ((not (consp form)) nil)
                                              ((member (car form) '(error signal cerror warn)) t)
                                              ;; Macro form: expand and check (CLHS 9.2.2.5)
                                              ((and (symbolp (car form))
                                                    (find-macro-expander (car form))
                                                    (not (local-function-entry (car form))))
                                               (check-signaling (cached-macroexpand form (find-macro-expander (car form)))))
                                              (t nil))))
                                (or (check-signaling body-form)
                                    ;; Also check symbol-macrolet expansion
                                    (and (symbolp body-form)
                                         (lookup-symbol-macro body-form)
                                         (check-signaling (lookup-symbol-macro body-form))))))
         (result-key (gen-local "RCRES"))
         (try-end-label (gen-label "RCTRYEND"))
         (done-label (gen-label "RCDONE"))
         ;; The in-flight exception, kept by the filter for its handler.
         (exobj-key (gen-local "RCEXOBJ"))
         ;; Matched clause index, written by the filter and read by its handler.
         (ri-key (gen-local "RCIDX"))
         (args-key (gen-local "RCARGS"))
         ;; One unique tag object per clause, in clause order. The LispRestart
         ;; entries and the filter both read them from here.
         (tags-key (gen-local "RCTAGS"))
         ;; Parse clauses: ((name params body...) ...)
         ;; Extract :report, :interactive, :test keyword options before body
         (parsed (mapcar (lambda (clause)
                           (let ((name (symbol-name (car clause)))
                                 (params (cadr clause))
                                 (rest (cddr clause))
                                 (report nil)
                                 (interactive nil)
                                 (test-fn nil))
                             ;; Parse keyword options from the beginning of rest
                             (loop while rest
                                   do (cond ((eq (car rest) :report)
                                             (setq report (cadr rest))
                                             (setq rest (cddr rest)))
                                            ((eq (car rest) :interactive)
                                             (setq interactive (cadr rest))
                                             (setq rest (cddr rest)))
                                            ((eq (car rest) :test)
                                             (setq test-fn (cadr rest))
                                             (setq rest (cddr rest)))
                                            (t (return))))
                             (list name params rest report interactive (car clause) test-fn)))
                         clauses))
         ;; Generate labels for each clause
         (clause-labels (loop for i from 0 below (length parsed)
                              collect (gen-label (format nil "RC~d" i))))
         ;; Skip labels for the ceq+brfalse dispatch, which must stay WITHIN the
         ;; filter's handler block (a brfalse cannot leave a funclet).
         (skip-labels (loop for i from 0 below (length parsed)
                            collect (gen-label (format nil "RCSKIP~d" i)))))
    `((:declare-local ,result-key "LispObject")
      ,@(emit-nil) (:stloc ,result-key)
      (:declare-local ,args-key "LispObject[]")
      (:declare-local ,exobj-key "Object")
      (:declare-local ,ri-key "Int32")
      ;; Create a tag object for each restart
      (:declare-local ,tags-key "Object[]")
      (:ldc-i4 ,(length parsed))
      (:newarr "Object")
      ,@(loop for i from 0 below (length parsed)
              append `((:dup) (:ldc-i4 ,i) (:newobj "Object") (:stelem-ref)))
      (:stloc ,tags-key)
      ;; Build LispRestart[] array and push cluster
      (:ldc-i4 ,(length parsed))
      (:newarr "LispRestart")
      ,@(loop for (name params handler-body report interactive name-sym test-fn) in parsed
              for i from 0
              append `((:dup) (:ldc-i4 ,i)
                       (:ldstr ,name)   ;; restart name
                       (:ldnull)        ;; handler (unused, dispatch is via tag)
                       ,@(if (stringp report)
                             `((:ldstr ,report))
                             `((:ldnull)))  ;; description
                       (:ldloc ,tags-key) (:ldc-i4 ,i) (:ldelem-ref)  ;; tag
                       (:ldc-i4 0)      ;; isBindRestart = false
                       (:newobj "LispRestart")
                       ;; Set NameSymbol to original symbol
                       ,@(when name-sym
                           `((:dup)
                             ,@(compile-expr (list 'quote name-sym))
                             (:castclass "Symbol")
                             (:callvirt "LispRestart.set_NameSymbol")))
                       ;; Set ReportFunction if report is a lambda/function form.
                       ;; CLHS: a symbol names a function (symbol -> #'name), the
                       ;; same as :interactive and :test below.
                       ,@(if (and report (not (stringp report)))
                             `((:dup)
                               ,@(compile-expr (if (symbolp report)
                                                   `(function ,report)
                                                   report))
                               (:callvirt "LispRestart.set_ReportFunction"))
                             nil)
                       ;; Set InteractiveFunction if provided
                       ;; CLHS: :interactive takes a function designator (symbol -> #'name)
                       ,@(if interactive
                             `((:dup)
                               ,@(compile-expr (if (symbolp interactive)
                                                   `(function ,interactive)
                                                   interactive))
                               (:callvirt "LispRestart.set_InteractiveFunction"))
                             nil)
                       ;; Set TestFunction if provided (symbol -> #'name)
                       ,@(if test-fn
                             `((:dup)
                               ,@(compile-expr (if (symbolp test-fn)
                                                   `(function ,test-fn)
                                                   test-fn))
                               (:callvirt "LispRestart.set_TestFunction"))
                             nil)
                       (:stelem-ref)))
      (:call "RestartClusterStack.PushCluster")
      ;; try-catch for body + RestartInvocationException
      (:begin-exception-block)
      ;; Body in MV-propagating position: restart-case returns body's values.
      ,@(let ((*in-tail-position* nil) (*in-mv-context* t))
           (compile-expr (if is-signaling-body
                             ;; The associations the handler makes go when this
                             ;; RESTART-CASE's restarts do, on every way out.
                             `(let ((%rc-restarts (%top-cluster-restarts))
                                    (%rc-mark (%condition-restarts-mark)))
                                (unwind-protect
                                     (handler-bind ((condition
                                                     (lambda (%rc-cond)
                                                       (%associate-condition-restarts
                                                        %rc-cond
                                                        %rc-restarts))))
                                       ,body-form)
                                  (%condition-restarts-restore %rc-mark)))
                             body-form)))
      (:stloc ,result-key)
      (:leave ,try-end-label)
      ;; A CIL exception FILTER answers "which of MY clauses does this invocation
      ;; target" (or -1) without entering the frame. The shape this replaces caught
      ;; every RestartInvocationException and rethrew the ones it did not own, and a
      ;; rethrow restarts dispatch from inside the handler funclet, which then stays
      ;; live for the rest of the exception's journey. Invoking a restart established
      ;; N levels out therefore stacked N dispatches: quadratic time, and past ~20000
      ;; levels an uncatchable .NET StackOverflowException, at a depth the same
      ;; recursion descends ten times over.
      (:begin-filter-block)
      (:stloc ,exobj-key) (:ldloc ,exobj-key)
      (:call "ControlFlowFilters.Transfer")
      (:ldloc ,tags-key)
      (:call "ControlFlowFilters.RestartCaseTag")
      (:stloc ,ri-key)
      (:ldloc ,ri-key) (:ldc-i4 -1) (:cgt)
      (:begin-filter-handler)
      (:pop)
      ;; Save arguments array
      (:ldloc ,exobj-key)
      (:call "ControlFlowFilters.TakeTransfer")
      (:castclass "RestartInvocationException")
      (:callvirt "RestartInvocationException.get_Arguments")
      (:stloc ,args-key)
      ;; Dispatch on the clause index the filter picked
      ,@(loop for label in clause-labels
              for skip in skip-labels
              for i from 0
              append `((:ldloc ,ri-key) (:ldc-i4 ,i) (:ceq)
                       (:brfalse ,skip)  ;; skip if not this clause (within handler)
                       (:leave ,label)   ;; exit handler to clause body
                       (:label ,skip)))  ;; within handler
      ;; Unreachable: the filter only answered 1 for an index in range.
      (:leave ,try-end-label)
      ;; Finally: pop the cluster on EVERY exit from the try: normal completion,
      ;; a matched-restart :leave to a clause, an invocation the filter declined,
      ;; AND a non-local transfer (return-from / throw / go) out of the body. The
      ;; old code popped only on the normal-exit and rethrow paths, so a non-local exit *through the
      ;; body* leaked the restart cluster. SBCL's compile-file body does exactly
      ;; that, leaking one RECOMPILE cluster per stem in the make-host-2 XC build.
      (:begin-finally-block)
      (:call "RestartClusterStack.PopCluster")
      (:end-exception-block)
      ;; Normal exit: finally already popped; jump to done (skip clause bodies).
      (:label ,try-end-label)
      (:br ,done-label)
      ;; Clause bodies (AFTER try-catch, with PopCluster before handler body)
      ,@(loop for (name params handler-body report interactive name-sym test-fn) in parsed
              for label in clause-labels
              append `((:label ,label)
                       ;; cluster already popped by the finally on the
                       ;; catch's :leave to this clause label
                       ,@(%compile-restart-clause name params handler-body args-key)
                       (:stloc ,result-key)
                       (:br ,done-label)))
      (:label ,done-label)
      (:ldloc ,result-key))))

;;; ============================================================
;;; restart-bind
;;; ============================================================

(defun compile-restart-bind (bindings body)
  "Compile (restart-bind ((name function &key ...) ...) body...).
   Creates LispRestart[] with isBindRestart=true, pushes cluster,
   body in try/finally with PopCluster. Unlike restart-case, there
   is no catch block - invoke-restart calls the handler directly."
  (let* ((n (length bindings))
         (result-key (gen-local "RBRES")))
    `(;; Build LispRestart[] array
      (:ldc-i4 ,n)
      (:newarr "LispRestart")
      ,@(loop for binding in bindings
              for i from 0
              append (let* ((name (car binding))
                            (fn-form (cadr binding))
                            (name-str (if name (symbol-name name) "NIL"))
                            (rest-args (cddr binding))
                            (test-fn nil)
                            (report-fn nil)
                            (interactive-fn nil))
                       ;; Parse keyword args
                       (loop while rest-args
                             do (cond ((eq (car rest-args) :test-function)
                                       (setq test-fn (cadr rest-args))
                                       (setq rest-args (cddr rest-args)))
                                      ((eq (car rest-args) :report-function)
                                       (setq report-fn (cadr rest-args))
                                       (setq rest-args (cddr rest-args)))
                                      ((eq (car rest-args) :interactive-function)
                                       (setq interactive-fn (cadr rest-args))
                                       (setq rest-args (cddr rest-args)))
                                      (t (setq rest-args (cddr rest-args)))))
                       `((:dup) (:ldc-i4 ,i)
                         (:ldstr ,name-str)         ;; restart name
                         ;; handler function: compile the function form
                         ,@(compile-expr fn-form)
                         (:castclass "LispFunction")
                         (:callvirt "LispFunction.get_RawFunction")
                         (:ldnull)                  ;; description
                         (:ldnull)                  ;; tag (unused for bind restarts)
                         (:ldc-i4 1)                ;; isBindRestart = true
                         (:newobj "LispRestart")
                         ;; Set NameSymbol
                         ,@(when name
                             `((:dup)
                               ,@(compile-expr (list 'quote name))
                               (:castclass "Symbol")
                               (:callvirt "LispRestart.set_NameSymbol")))
                         ;; Set TestFunction if provided
                         ,@(when test-fn
                             `((:dup)
                               ,@(compile-expr test-fn)
                               (:callvirt "LispRestart.set_TestFunction")))
                         ;; Set ReportFunction if provided
                         ,@(when report-fn
                             `((:dup)
                               ,@(compile-expr report-fn)
                               (:callvirt "LispRestart.set_ReportFunction")))
                         ;; Set InteractiveFunction if provided
                         ,@(when interactive-fn
                             `((:dup)
                               ,@(compile-expr interactive-fn)
                               (:callvirt "LispRestart.set_InteractiveFunction")))
                         (:stelem-ref))))
      ;; PushCluster
      (:call "RestartClusterStack.PushCluster")
      ;; try/finally to ensure PopCluster
      (:declare-local ,result-key "LispObject")
      ,@(emit-nil) (:stloc ,result-key)
      (:begin-exception-block)
      ;; Body in MV-propagating position: restart-bind returns body's values.
      ,@(let ((*in-tail-position* nil) (*in-mv-context* t)) (compile-progn body))
      (:stloc ,result-key)
      (:begin-finally-block)
      (:call "RestartClusterStack.PopCluster")
      (:end-exception-block)
      (:ldloc ,result-key))))

;;; ============================================================
;;; handler-bind
;;; ============================================================

(defun compile-handler-bind (bindings body)
  "Compile (handler-bind ((type handler-fn) ...) body...).
   Creates HandlerBinding[] array, pushes cluster, body in try/finally with PopCluster."
  (let* ((n (length bindings))
         (result-key (gen-local "HBRES")))
    `(;; Build HandlerBinding[] array
      (:ldc-i4 ,n)
      (:newarr "HandlerBinding")
      ,@(loop for binding in bindings
              for i from 0
              append (let ((type-spec (car binding))
                           (handler-form (cadr binding)))
                       `((:dup) (:ldc-i4 ,i)
                         ;; type specifier as symbol
                         ,@(compile-quoted type-spec)
                         ;; handler function
                         ,@(compile-expr handler-form)
                         (:call "Runtime.CoerceToFunction")
                         (:newobj "HandlerBinding")
                         (:stelem-ref))))
      ;; PushCluster
      (:call "HandlerClusterStack.PushCluster")
      ;; try { try { body } catch(System.Exception) { wrap+signal+rethrow } } finally { PopCluster }
      (:declare-local ,result-key "LispObject")
      ,@(emit-nil) (:stloc ,result-key)
      ,@(let ((inner-end-label (gen-label "HBINNER")))
          `((:begin-exception-block)  ;; outer try (for finally)
            (:begin-exception-block)  ;; inner try (for catch)
            ;; Body in MV-propagating position: handler-bind returns body's values.
            ,@(let ((*in-tail-position* nil) (*in-mv-context* t)) (compile-progn body))
            (:stloc ,result-key)
            (:leave ,inner-end-label)
            ;; Raw .NET exceptions: the filter wraps one as a condition and
            ;; signals it while the frames that raised it are still on the stack
            ;; (so a handler sees them in a backtrace), and the handler block
            ;; continues the handler's non-local exit or unwinds with the
            ;; condition as a LispErrorException. Lisp conditions and non-local
            ;; exits are not taken, so they pass through without a rethrow.
            (:begin-filter-block)
            (:call "Runtime.HandlerBindFilter")
            (:begin-filter-handler)
            (:call "Runtime.HandlerBindRethrow")
            (:end-exception-block)  ;; end inner try-filter
            (:label ,inner-end-label)
            (:begin-finally-block)
            (:call "HandlerClusterStack.PopCluster")
            (:end-exception-block)  ;; end outer try-finally
            (:ldloc ,result-key))))))

;;; ============================================================
;;; and / or / cond
;;; ============================================================

;; What MULTIPLEVALUES.PRIMARY did for the value of FORM, on the stack: make
;; it the form's only value. A count of -1 already reads as "the primary
;; value alone", so the published copy PRIMARY made is not needed.
(defun %only-primary-value (form)
  `(,@(unless (single-value-form-p form) '((:call "Runtime.UnwrapMv")))
    (:call "MultipleValues.Reset")))

;; The truth test of FORM's value after %ONLY-PRIMARY-VALUE: a reference
;; comparison when that went through UnwrapMv.
(defun %primary-truth-test (form)
  (if (single-value-form-p form)
      '(:call "Runtime.IsTruthy")
      '(:call "Runtime.IsNotNil")))

;; A test COMPILE-BOOLEAN-BRANCH lowers without building its value, and whose
;; value is T when true: a fused comparison or predicate, or NOT / NULL.
(defun %t-or-nil-test-form-p (form)
  (and (consp form)
       (symbolp (car form))
       (or (and (compile-if-fused-comparison-p form)
                (not (eq (car form) '%key-absent-p)))
           (and (member (car form) '(not null))
                (consp (cdr form)) (null (cddr form))
                (not (local-function-entry (car form)))))))

(defun compile-and (args)
  "Compile (and a b c) with short-circuit evaluation."
  (cond
    ((null args) (emit-t))
    ((null (cdr args)) (compile-expr (car args)))
    (t (let ((end-label (gen-label "ANDEND"))
             (false-label nil)
             (result-key (gen-local "AND")))
         `((:declare-local ,result-key "LispObject")
           ,@(loop for (arg . rest) on args
                   if rest
                     ;; Only tested: when it fails the AND is NIL whatever it
                     ;; returned, so branch on it instead of keeping its value.
                     ;; On fixnum operands this is what keeps a raw slot read
                     ;; from being boxed.
                     append (progn
                              (unless false-label (setq false-label (gen-label "ANDFALSE")))
                              (let ((*in-tail-position* nil) (*in-mv-context* nil))
                                (compile-boolean-branch arg false-label nil)))
                   else
                     ;; Last arg: pass through all values
                     append `(,@(compile-expr arg)
                              (:stloc ,result-key)))
           ,@(when false-label
               `((:br ,end-label)
                 (:label ,false-label)
                 ,@(emit-nil)
                 (:call "MultipleValues.Reset")
                 (:stloc ,result-key)))
           (:label ,end-label)
           (:ldloc ,result-key))))))

(defun compile-or (args)
  "Compile (or a b c) with short-circuit evaluation."
  (cond
    ((null args) (emit-nil))
    ((null (cdr args)) (compile-expr (car args)))
    (t (let ((end-label (gen-label "OREND"))
             (true-label nil)
             (result-key (gen-local "OR")))
         `((:declare-local ,result-key "LispObject")
           ,@(loop for (arg . rest) on args
                   if (and rest (%t-or-nil-test-form-p arg))
                     ;; A predicate whose true value is T: branch on it.
                     append (progn
                              (unless true-label (setq true-label (gen-label "ORTRUE")))
                              (let ((*in-tail-position* nil) (*in-mv-context* nil))
                                (compile-boolean-branch arg true-label t)))
                   else if rest
                     ;; Intermediate arg: NOT in tail position: its value is
                     ;; consumed by the IsTruthy test, not returned. Binding
                     ;; *in-tail-position* nil prevents a self-call here from
                     ;; being TCO-rewritten into a value-discarding loop-back,
                     ;; which made e.g. (or (f (car x)) (f (cdr x))) return the
                     ;; wrong value on deep inputs. The last arg
                     ;; below keeps tail position so genuine tail self-calls
                     ;; still TCO.
                     append `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr arg))
                              ,@(%only-primary-value arg)
                              (:stloc ,result-key)
                              (:ldloc ,result-key)
                              ,(%primary-truth-test arg)
                              (:brtrue ,end-label))
                   else
                     ;; Last arg: pass through all values (tail position kept)
                     append `(,@(compile-expr arg)
                              (:stloc ,result-key)))
           ,@(when true-label
               `((:br ,end-label)
                 (:label ,true-label)
                 ,@(emit-t)
                 (:call "MultipleValues.Reset")
                 (:stloc ,result-key)))
           (:label ,end-label)
           (:ldloc ,result-key))))))

(defun compile-cond (clauses &optional shared-tmp)
  "Compile (cond (test1 body1...) ...) to nested conditional.
   shared-tmp: shared LispObject local for no-body arms."
  (if (null clauses)
      (emit-nil)
      (let* ((clause (progn
                       ;; Every clause must be a non-empty list, also after a T clause that ends the cond.
                       (unless shared-tmp
                         (dolist (c clauses)
                           (unless (consp c) (error 'type-error :datum c :expected-type 'cons))))
                       (if (consp (car clauses))
                           (car clauses)
                           (error 'type-error :datum (car clauses) :expected-type 'cons))))
             (test (car clause))
             (body (cdr clause)))
        (if (eq test t)
            ;; Default clause
            (if body (compile-progn body) (emit-t))
            ;; Normal clause
            (let ((else-label (gen-label "CELSE"))
                  (end-label (gen-label "CEND")))
              (if body
                    ;; The same test lowering as IF: native comparisons, NOT,
                    ;; AND / OR chains, inlined predicates.
                    (compile-if (list test (cons 'progn body) (cons 'cond (cdr clauses))))
                    ;; No body: all no-body arms share one CTMP slot
                    (let ((tmp (or shared-tmp (gen-local "CTMP"))))
                      ;; Test-only arm: the test value becomes the result, but the
                      ;; test itself is not in tail position: a self-recursive call
                      ;; here must compute a value, not TCO-jump.
                      `(,@(unless shared-tmp `((:declare-local ,tmp "LispObject")))
                        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr test))
                        ,@(%only-primary-value test)
                        (:stloc ,tmp)
                        (:ldloc ,tmp)
                        ,(%primary-truth-test test)
                        (:brfalse ,else-label)
                        (:ldloc ,tmp)
                        (:br ,end-label)
                        (:label ,else-label)
                        ,@(compile-cond (cdr clauses) tmp)
                        (:label ,end-label)))))))))
