;;; DOTCL:BACKTRACE must show the same frames whichever evaluator ran the code.
;;;
;;; The debugger call stack is dotcl's own, not the .NET one: LispFunction.Invoke*
;;; pushes a frame when the callee has a name, and anonymous callees push none.
;;; That gave the tree-walk evaluator a backtrace made entirely of the WRONG
;;; frames:
;;;
;;;   compiled     ("%P-LEAF" "%P-MID" "PROBE")
;;;   interpreted  ("%MINI-EVAL" "%MINI-EVAL-PROGN" "%MINI-BIND-WALK"
;;;                 "%MINI-BIND-PARAMS-CALL" ... x14)
;;;
;;; for two reasons, both fixed here:
;;;
;;;   1. The evaluator is itself written in Lisp, so ITS helpers are ordinary
;;;      named functions and every interpreted call pushed about a dozen of them.
;;;      BACKTRACE-WITH-ARGS then printed the evaluator's internal environment
;;;      alists as if they were the user's arguments. Such a helper now carries no
;;;      frame name at all.
;;;   2. An interpreted DEFUN builds an anonymous closure, so the user's own
;;;      function pushed nothing. It is now named, exactly as the compiler names
;;;      what it emits.
;;;
;;; A third gap turned up on the way: LispFunction.Invoke(params) — the entry used
;;; by APPLY and by the evaluator's own call site — pushed no frame at all, so a
;;; named callee reached through APPLY was missing from a COMPILED backtrace too.
;;;
;;; The parity target is what the compiler produces: global named definitions
;;; (DEFUN, DEFMETHOD) appear; FLET, LABELS and plain LAMBDA do not. The two
;;; evaluators are asserted against that same shape below, each under function
;;; names only its own mode ever defines — sharing names would let a stale
;;; compiled definition answer for the interpreted case.

;;; The -COMPILE halves are DEFTEST-COMPILED-ONLY: an emit-free build has no
;;; compiler, so :COMPILE there is the tree-walk evaluator under another name and
;;; the pair would assert the same run twice. Their point is to pin what the
;;; interpreted halves have to match, which only means something where both
;;; evaluators exist.
;;;
;;; A MACRO, not a DEFUN: a helper function would push a frame of its own into
;;; every backtrace asserted below. Expanded in place, the EVAL runs at top level,
;;; where neither evaluator has a named frame to record.
(defmacro %ibt (mode form)
  `(let ((dotcl:*evaluator-mode* ,mode))
     (eval ,form)))

;;; --- a chain of DEFUNs: both evaluators list it, innermost first

(deftest-compiled-only interp-backtrace.defun-chain-compile
  (%ibt :compile '(progn (defun %ibt-c-leaf () (dotcl:backtrace))
                         (defun %ibt-c-mid () (%ibt-c-leaf))
                         (%ibt-c-mid)))
  ("%IBT-C-LEAF" "%IBT-C-MID"))

(deftest interp-backtrace.defun-chain-interpret
  (%ibt :interpret '(progn (defun %ibt-i-leaf () (dotcl:backtrace))
                           (defun %ibt-i-mid () (%ibt-i-leaf))
                           (%ibt-i-mid)))
  ("%IBT-I-LEAF" "%IBT-I-MID"))

;;; --- the evaluator's own machinery is never visible

(deftest interp-backtrace.no-evaluator-internals
  (%ibt :interpret '(progn
                     (defun %ibt-i-deep (n)
                       (if (= n 0)
                           (remove-if-not (lambda (f) (search "%MINI-" f)) (dotcl:backtrace))
                           (car (mapcar (lambda (x) (%ibt-i-deep x)) (list (1- n))))))
                     (%ibt-i-deep 3)))
  nil)

;;; --- BACKTRACE-WITH-ARGS carries the user's arguments, not the evaluator's
;;; environment. This is the one that printed alists like ((S . "x") (N . 7))
;;; alongside %MINI-BLOCKS markers.

(defparameter %ibt-args-c
  '(progn (defun %ibt-c-al (a b) (declare (ignore a b)) (dotcl:backtrace-with-args))
          (defun %ibt-c-am (q) (%ibt-c-al q 9))
          (%ibt-c-am 5)))

(defparameter %ibt-args-i
  '(progn (defun %ibt-i-al (a b) (declare (ignore a b)) (dotcl:backtrace-with-args))
          (defun %ibt-i-am (q) (%ibt-i-al q 9))
          (%ibt-i-am 5)))

(deftest-compiled-only interp-backtrace.with-args-compile
  (%ibt :compile %ibt-args-c)
  (("%IBT-C-AL" 5 9) ("%IBT-C-AM" 5)))

(deftest interp-backtrace.with-args-interpret
  (%ibt :interpret %ibt-args-i)
  (("%IBT-I-AL" 5 9) ("%IBT-I-AM" 5)))

;;; a captured argument is the real object, usable as one
(deftest interp-backtrace.with-args-real-objects-interpret
  (%ibt :interpret '(progn (defun %ibt-i-obj (n s) (declare (ignore s))
                             (1+ (cadr (car (dotcl:backtrace-with-args)))))
                           (%ibt-i-obj 30 "x")))
  31)

;;; --- a generic function frame appears under both evaluators

(deftest-compiled-only interp-backtrace.defmethod-compile
  (%ibt :compile '(progn (defclass %ibt-c-k () ())
                         (defmethod %ibt-c-gf ((x %ibt-c-k)) (dotcl:backtrace))
                         (defun %ibt-c-callgf () (%ibt-c-gf (make-instance '%ibt-c-k)))
                         (%ibt-c-callgf)))
  ("%IBT-C-GF" "%IBT-C-CALLGF"))

(deftest interp-backtrace.defmethod-interpret
  (%ibt :interpret '(progn (defclass %ibt-i-k () ())
                           (defmethod %ibt-i-gf ((x %ibt-i-k)) (dotcl:backtrace))
                           (defun %ibt-i-callgf () (%ibt-i-gf (make-instance '%ibt-i-k)))
                           (%ibt-i-callgf)))
  ("%IBT-I-GF" "%IBT-I-CALLGF"))

;;; --- APPLY reaches a named callee, and now records it (both evaluators)

;;; The compiled side goes straight to the callee — the compiler turns this APPLY
;;; into an ordinary call, so no APPLY frame is recorded whether the argument list
;;; is constant or built at run time. (The APPLY that does reach Runtime.Apply,
;;; and so depends on Invoke recording a frame, is covered in frame-locals.lisp.)
(deftest-compiled-only interp-backtrace.apply-compile
  (%ibt :compile '(progn (defun %ibt-c-ap (x) (declare (ignore x)) (dotcl:backtrace))
                         (defun %ibt-c-apc (n) (apply #'%ibt-c-ap (list n)))
                         (%ibt-c-apc 1)))
  ("%IBT-C-AP" "%IBT-C-APC"))

;;; APPLY itself is a Lisp DEFUN in the standard library. The compiler open-codes
;;; the call, so no APPLY frame exists there; the interpreter really calls the
;;; function, so one does. That difference is the compiler's inlining showing
;;; through, not the frame machinery diverging — asserted as it is rather than
;;; papered over, since suppressing it would mean hiding a call that happened.
(deftest interp-backtrace.apply-interpret
  (%ibt :interpret '(progn (defun %ibt-i-ap (x) (declare (ignore x)) (dotcl:backtrace))
                           (defun %ibt-i-apc (n) (apply #'%ibt-i-ap (list n)))
                           (%ibt-i-apc 1)))
  ("%IBT-I-AP" "APPLY" "%IBT-I-APC"))

;;; --- over-fix guards: naming must stop where the compiler stops -----------
;;;
;;; A compiled backtrace lists neither FLET, LABELS nor plain LAMBDA frames, so
;;; naming every interpreted closure — the obvious way to write the fix — would
;;; make the interpreted path list MORE than the compiled one. Each case below is
;;; asserted for both evaluators so the pair has to move together.

(deftest-compiled-only interp-backtrace.flet-not-named-compile
  (%ibt :compile '(progn (defun %ibt-c-fl () (flet ((inner () (dotcl:backtrace))) (inner)))
                         (%ibt-c-fl)))
  ("%IBT-C-FL"))

(deftest interp-backtrace.flet-not-named-interpret
  (%ibt :interpret '(progn (defun %ibt-i-fl () (flet ((inner () (dotcl:backtrace))) (inner)))
                           (%ibt-i-fl)))
  ("%IBT-I-FL"))

(deftest-compiled-only interp-backtrace.labels-not-named-compile
  (%ibt :compile '(progn (defun %ibt-c-lb () (labels ((inner () (dotcl:backtrace))) (inner)))
                         (%ibt-c-lb)))
  ("%IBT-C-LB"))

(deftest interp-backtrace.labels-not-named-interpret
  (%ibt :interpret '(progn (defun %ibt-i-lb () (labels ((inner () (dotcl:backtrace))) (inner)))
                           (%ibt-i-lb)))
  ("%IBT-I-LB"))

(deftest-compiled-only interp-backtrace.lambda-not-named-compile
  (%ibt :compile '(progn (defun %ibt-c-lm () (funcall (lambda () (dotcl:backtrace))))
                         (%ibt-c-lm)))
  ("%IBT-C-LM"))

(deftest interp-backtrace.lambda-not-named-interpret
  (%ibt :interpret '(progn (defun %ibt-i-lm () (funcall (lambda () (dotcl:backtrace))))
                           (%ibt-i-lm)))
  ("%IBT-I-LM"))

;;; --- the primitives that stand in for a special form stay invisible too
;;;
;;; %MINI-EVAL implements HANDLER-BIND by calling %CALL-WITH-HANDLER-CLUSTER,
;;; which runs the body as a thunk — so that primitive sat on the call stack for
;;; the whole body and appeared between the user's own frames, where compiled
;;; code (which emits the cluster inline) shows nothing.

(deftest-compiled-only interp-backtrace.handler-case-adds-no-frame-compile
  (%ibt :compile '(progn (defun %ibt-c-hcl () (dotcl:backtrace))
                         (defun %ibt-c-hcm () (handler-case (%ibt-c-hcl) (error () :e)))
                         (%ibt-c-hcm)))
  ("%IBT-C-HCL" "%IBT-C-HCM"))

(deftest interp-backtrace.handler-case-adds-no-frame-interpret
  (%ibt :interpret '(progn (defun %ibt-i-hcl () (dotcl:backtrace))
                           (defun %ibt-i-hcm () (handler-case (%ibt-i-hcl) (error () :e)))
                           (%ibt-i-hcm)))
  ("%IBT-I-HCL" "%IBT-I-HCM"))

(deftest interp-backtrace.handler-bind-adds-no-frame-interpret
  (%ibt :interpret '(progn (defun %ibt-i-hbl () (dotcl:backtrace))
                           (defun %ibt-i-hbm ()
                             (handler-bind ((error #'identity)) (%ibt-i-hbl)))
                           (%ibt-i-hbm)))
  ("%IBT-I-HBL" "%IBT-I-HBM"))

;;; the handler still runs — suppressing the FRAME must not suppress the handler
(deftest interp-backtrace.suppressed-frame-still-handles-interpret
  (%ibt :interpret '(handler-case (error "boom") (error () :caught)))
  :caught)

;;; BACKTRACE itself is registered unnamed and stays out of its own result
(deftest interp-backtrace.self-excluded-interpret
  (%ibt :interpret '(progn (defun %ibt-i-self () (member "BACKTRACE" (dotcl:backtrace) :test #'string=))
                           (%ibt-i-self)))
  nil)

;;; The stack unwinds: a frame is popped when its call returns, so a second call
;;; at the same place sees the same depth rather than a growing one.
(deftest interp-backtrace.frames-pop-interpret
  (%ibt :interpret '(progn (defun %ibt-i-pop () (length (dotcl:backtrace)))
                           (defun %ibt-i-popc () (list (%ibt-i-pop) (%ibt-i-pop) (%ibt-i-pop)))
                           (%ibt-i-popc)))
  (2 2 2))
