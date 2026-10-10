;;; A call in tail position does not grow the stack, whatever function it calls.
;;;
;;; Compiled code made a call to another function in tail position with the IL
;;; tail. prefix, but the call went into LispFunction.InvokeN, which runs the
;;; callee inside the try/finally that unlinks its debugger frame. InvokeN's own
;;; native frame therefore stayed on the stack for the whole call, and mutual
;;; recursion grew the stack by one such frame per call: a million calls overflowed
;;; the 256 MB main thread. A call in the branch of an IF / COND / CASE was not
;;; even prefixed. Now every call whose value the function returns as it is goes
;;; straight into the callee's body as a tail call, and the debugger frame of the
;;; caller is handed to the callee instead of a new one being pushed.
;;;
;;; As in SBCL, a call is not in tail position under a special binding,
;;; UNWIND-PROTECT, HANDLER-CASE or CATCH, or as the argument of THROW, and the
;;; backtrace shows the function a tail call ran in place of its caller. A
;;; function compiled with (DEBUG 3) makes no tail calls, so it stays in the
;;; backtrace.
;;;
;;; The depth tests are DEFTEST-EMITTING-ONLY: the definitions here are compiled
;;; whenever the image has a compiler (LOAD compiles even under :INTERPRET), and
;;; on an emit-free build they are interpreted, where only a call back into the
;;; same function is a tail call (interp-tail-calls.lisp).

(defconstant +tc-depth+ 1000000)

;;; --- shapes of mutual recursion -------------------------------------------

;; A body that is only the call.
(defun %tc-wrap-a (n) (%tc-wrap-b n))
(defun %tc-wrap-b (n) (if (= n 0) :done (%tc-wrap-a (1- n))))

;; Calls in the branches of IF, COND and CASE.
(defun %tc-ev (n) (if (= n 0) :even (%tc-od (1- n))))
(defun %tc-od (n) (if (= n 0) :odd (%tc-ev (1- n))))
(defun %tc-cond (n)
  (cond ((= n 0) :done)
        ((evenp n) (%tc-case (1- n)))
        (t (%tc-case (1- n)))))
(defun %tc-case (n)
  (case (mod n 3)
    (0 (%tc-cond n))
    (t (if (= n 0) :done (%tc-cond (1- n))))))

;; Under a LET (lexical only) and through FUNCALL of a function object.
(defun %tc-let (n) (let ((m (1- n))) (if (< m 0) :done (%tc-let-b m))))
(defun %tc-let-b (n) (%tc-let n))
(defun %tc-funcall (f n) (if (= n 0) :done (funcall f f (1- n))))

;; A closure in the loop, and five arguments (past the four a frame holds inline).
(defun %tc-closure (n)
  (let ((g (lambda (k) (if (= k 0) :done (%tc-closure (1- k))))))
    (funcall g n)))
(defun %tc-five-a (a b c d n) (if (= n 0) (list a b c d) (%tc-five-b a b c d (1- n))))
(defun %tc-five-b (a b c d n) (%tc-five-a b c d a n))

;; The value reaching the RET through a local: a BLOCK left by RETURN, a
;; RETURN-FROM the function, and an inlined function's body (Coalton's method
;; call shape: an inline accessor FUNCALLing the method under THE).
(defun %tc-block (n) (if (= n 0) :done (block nil (return (%tc-block-b (1- n))))))
(defun %tc-block-b (n) (%tc-block n))
(defun %tc-return-from (n)
  (when (> n 0) (return-from %tc-return-from (%tc-return-from-b (1- n))))
  :done)
(defun %tc-return-from-b (n) (%tc-return-from n))
(defvar *tc-methods*)
(declaim (inline %tc-method))
(defun %tc-method (dict x)
  (declare (optimize (speed 3) (safety 0)))
  (funcall (car dict) x))
(defun %tc-generic (dict x) (the (values t &optional) (%tc-method dict (1+ x))))
(defun %tc-instance (x)
  (the (values fixnum &optional)
       (if (< x +tc-depth+) (%tc-generic *tc-methods* x) x)))

(deftest-emitting-only tail-calls.wrapper-mutual-recursion
  (%tc-wrap-a +tc-depth+)
  :done)

(deftest-emitting-only tail-calls.if-branches
  (%tc-ev +tc-depth+)
  :even)

(deftest-emitting-only tail-calls.cond-and-case-branches
  (%tc-cond +tc-depth+)
  :done)

(deftest-emitting-only tail-calls.under-a-lexical-let
  (%tc-let +tc-depth+)
  :done)

(deftest-emitting-only tail-calls.funcall
  (%tc-funcall #'%tc-funcall +tc-depth+)
  :done)

(deftest-emitting-only tail-calls.through-a-closure
  (%tc-closure +tc-depth+)
  :done)

(deftest-emitting-only tail-calls.five-arguments
  (%tc-five-a 1 2 3 4 +tc-depth+)
  (1 2 3 4))

(deftest-emitting-only tail-calls.block-and-return
  (%tc-block +tc-depth+)
  :done)

(deftest-emitting-only tail-calls.return-from-the-function
  (%tc-return-from +tc-depth+)
  :done)

(deftest-emitting-only tail-calls.inlined-method-call-under-the
  (let ((*tc-methods* (list #'%tc-instance)))
    (%tc-generic *tc-methods* 0))
  1000000)

;;; --- what is not a tail call still behaves -------------------------------

(defvar *tc-special* :global)
(defun %tc-read-special () *tc-special*)
(defun %tc-bind-special () (let ((*tc-special* :bound)) (%tc-read-special)))

(deftest tail-calls.special-binding-outlives-the-call
  (list (%tc-bind-special) *tc-special*)
  (:bound :global))

(defun %tc-signal () (error "tail-calls"))
(defun %tc-handled () (handler-case (%tc-signal) (error () :caught)))

(deftest tail-calls.handler-case-still-handles
  (%tc-handled)
  :caught)

(defvar *tc-log* '())
(defun %tc-body () (push :body *tc-log*) :value)
(defun %tc-protected ()
  (unwind-protect (%tc-body) (push :cleanup *tc-log*)))

(deftest tail-calls.unwind-protect-cleanup-runs-after-the-call
  (let ((*tc-log* '()))
    (list (%tc-protected) (reverse *tc-log*)))
  (:value (:body :cleanup)))

;; THROW's argument ends in a call in both branches: neither may return from
;; the function in place of throwing.
(defun %tc-value (x) (list :value x))
(defun %tc-thrower (x) (throw '%tc-tag (if x (%tc-value x) (%tc-value :none))))
(defun %tc-catcher (x) (list :caught (catch '%tc-tag (%tc-thrower x) :fell-through)))

(deftest tail-calls.throw-argument-is-not-a-tail-call
  (list (%tc-catcher 1) (%tc-catcher nil))
  ((:caught (:value 1)) (:caught (:value :none))))

;; Every value comes back through a tail call, RETURN included; a LET binding
;; takes only the first, so a call whose value is bound and returned is not a
;; tail call.
(defun %tc-values () (values 1 2 3))
(defun %tc-values-tail (x) (if x (%tc-values) (values)))
(defun %tc-values-return () (block nil (return (%tc-values))))
(defun %tc-values-bound () (let ((v (%tc-values))) v))

(deftest tail-calls.multiple-values-survive
  (list (multiple-value-list (%tc-values-tail t))
        (multiple-value-list (%tc-values-tail nil))
        (multiple-value-list (%tc-values-return))
        (multiple-value-list (%tc-values-bound)))
  ((1 2 3) () (1 2 3) (1)))

;; A callee with &OPTIONAL, called with the wrong number of arguments, and not
;; defined at all: the ordinary entry still decides.
(defun %tc-optional (a &optional (b :default)) (list a b))
(defun %tc-calls-optional (x) (%tc-optional x))
(defun %tc-one-arg (a) a)
(defun %tc-calls-wrongly (x) (funcall '%tc-one-arg x x))
(defun %tc-calls-undefined (x) (funcall '%tc-no-such-function x))

(deftest tail-calls.optional-parameters
  (%tc-calls-optional 1)
  (1 :default))

(deftest tail-calls.wrong-argument-count-is-still-an-error
  (handler-case (%tc-calls-wrongly 1) (program-error () :program-error))
  :program-error)

(deftest tail-calls.undefined-function-is-still-an-error
  (handler-case (%tc-calls-undefined 1) (undefined-function () :undefined))
  :undefined)

;;; --- the backtrace ---------------------------------------------------------

;; The leaf's own call to BACKTRACE is a tail call too, into a function that
;; records no frame: the leaf keeps its name.
(defun %tc-bt-leaf (a) (declare (ignore a)) (dotcl:backtrace-with-args))
(defun %tc-bt-mid (x) (%tc-bt-leaf (list x)))
(defun %tc-bt-mid-debug-3 (x)
  (declare (optimize (debug 3)))
  (%tc-bt-leaf (list x)))

(defun %tc-frame-names (bt)
  (loop for f in bt
        when (member (car f) '("%TC-BT-LEAF" "%TC-BT-MID" "%TC-BT-MID-DEBUG-3")
                     :test #'string=)
          collect f))

(deftest-compiled-only tail-calls.backtrace-shows-the-callee-in-place-of-the-caller
  (%tc-frame-names (%tc-bt-mid 5))
  (("%TC-BT-LEAF" (5))))

(deftest-compiled-only tail-calls.debug-3-keeps-the-caller
  (%tc-frame-names (%tc-bt-mid-debug-3 5))
  (("%TC-BT-LEAF" (5)) ("%TC-BT-MID-DEBUG-3" 5)))

;; A lambda the named caller tail-calls records no frame: the frame keeps the
;; caller's name, and the lambda's own tail call hands it on.
(defun %tc-bt-via-lambda (x)
  (funcall (lambda (y) (%tc-bt-leaf y)) (list x)))

(deftest-compiled-only tail-calls.lambda-passes-the-frame-on
  (%tc-frame-names (%tc-bt-via-lambda 6))
  (("%TC-BT-LEAF" (6))))

;; An error deep in a tail-called chain still reports from the innermost frame.
(defun %tc-err-leaf (x) (car x))
(defun %tc-err-mid (x) (%tc-err-leaf x))

(deftest-compiled-only tail-calls.error-backtrace-starts-at-the-callee
  (block nil
    (handler-bind ((type-error
                     (lambda (c) (declare (ignore c))
                       (return (car (member "%TC-ERR-LEAF" (dotcl:backtrace) :test #'string=))))))
      (%tc-err-mid 1)))
  "%TC-ERR-LEAF")
