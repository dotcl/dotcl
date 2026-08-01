;;; DOTCL:FRAME-LOCALS — in-process lexical variables of a running frame, for the
;;; CL-native debugger (sldb frame locals, the debugger's :bt). Compiling with
;;; DOTCL:*EMIT-FRAME-LOCALS* true makes each body record its user lexicals in a
;;; runtime debug frame; with it false (the default) nothing is recorded and the
;;; emitted code is unchanged.

(defvar *fl-captured* nil)

;;; Records the locals of its CALLER (frame 0 is this probe itself).
(defun fl-probe ()
  (setf *fl-captured* (dotcl:frame-locals 1))
  nil)

(defun fl-var (name)
  (cdr (assoc name *fl-captured* :test #'string=)))

(setf dotcl:*emit-frame-locals* t)

(defun fl-plain (a b)
  (let ((s (+ a b)))
    (fl-probe)
    s))

(defun fl-opt (a &optional (b 7) &key (c 9))
  (fl-probe)
  (list a b c))

(defun fl-rec (n)
  (if (= n 0)
      (progn (fl-probe) nil)
      (cons n (fl-rec (- n 1)))))

(defun fl-lambda (n)
  (let ((z (* n 2)))
    (funcall (lambda (i) (fl-probe) i) 1)
    z))

(defun fl-boxed (n)
  (let ((acc 0))
    (funcall (lambda () (incf acc n)))
    (funcall (lambda () (incf acc 100)))
    (fl-probe)
    acc))

;;; Same boxed variable, but the probe runs inside the closure that mutates it.
(defun fl-boxed-inner (n)
  (let ((acc 0))
    (funcall (lambda (k) (incf acc k) (fl-probe) acc) n)))

;;; labels mutual recursion: the function cells are boxed too, but they are
;;; compiler machinery, not user variables, and must stay out of the frame.
(defun fl-labels-parity (n)
  (labels ((ev (k) (if (= k 0) 'even (od (- k 1))))
           (od (k) (if (= k 0) 'odd (ev (- k 1)))))
    (list (ev n) (od n))))

(defun fl-setq (a)
  (let ((s 0))
    (setq s (+ a 1))
    (setq s (* s 2))
    (fl-probe)
    s))

(defun fl-tco (n acc)
  (if (= n 0)
      (progn (fl-probe) acc)
      (fl-tco (- n 1) (+ acc n))))

;;; Native-rep slots: a declared fixnum / float local (and DOTIMES's counter) lives
;;; in a raw Int64 / Double slot, which the frame store has to box.
(defun fl-native-fixnum (n)
  (declare (fixnum n))
  (let ((acc 0))
    (dotimes (i n) (setq acc (+ acc i)))
    (fl-probe)
    acc))

(defun fl-native-float (x)
  (declare (double-float x))
  (let ((d (* x 1.5d0)))
    (declare (double-float d))
    (fl-probe)
    d))

(defun fl-native-tco (n acc)
  (declare (fixnum n acc))
  (if (= n 0)
      (progn (fl-probe) acc)
      (fl-native-tco (- n 1) (+ acc n))))

;;; Named functions reached through a call path that pushes no call-stack frame:
;;; APPLY and the runtime's call to *DEBUGGER-HOOK* both go through
;;; LispFunction.Invoke(params). Such a body runs at its caller's depth, so it
;;; must not take over the caller's backtrace position — the frame an sldb user
;;; is looking at when the hook runs.
(defun fl-borrowed (a)
  (let ((q (* a 2)))
    (fl-probe)
    q))

(defun fl-borrow-caller (m)
  (let ((keep (+ m 100)))
    (apply #'fl-borrowed (list m))
    keep))

(defun fl-hook-fn (c hook)
  (declare (ignore c hook))
  (let ((ignored 'hook-local))
    (setf *fl-captured* (dotcl:frame-locals 0))
    (throw 'fl-done ignored)))

(defun fl-hook-caller (a)
  (let ((s (* a 3)))
    (catch 'fl-done
      (let ((*debugger-hook* #'fl-hook-fn))
        (error "fl boom")))
    s))

(setf dotcl:*emit-frame-locals* nil)

(defun fl-off (a)
  (let ((b (1+ a)))
    (fl-probe)
    b))

;;; Parameters first, then LET bindings, in binding order.
(deftest frame-locals-params-and-let
  (progn (fl-plain 2 3) *fl-captured*)
  (("A" . 2) ("B" . 3) ("S" . 5)))

;;; The instrumentation must not change what the code computes.
(deftest frame-locals-value-unchanged
  (fl-plain 2 3)
  5)

(deftest frame-locals-optional-and-key
  (progn (fl-opt 1) *fl-captured*)
  (("A" . 1) ("B" . 7) ("C" . 9)))

;;; Each recursion depth has its own frame: the innermost one is asked about.
(deftest frame-locals-innermost-recursion-frame
  (progn (fl-rec 3) *fl-captured*)
  (("N" . 0)))

;;; A lambda runs without a call-stack frame of its own, so it shares its caller's
;;; backtrace position; asking for that position must still answer with the
;;; caller's own variables, not the lambda's.
(deftest frame-locals-lambda-keeps-caller-locals
  (progn (fl-lambda 4) (list (fl-var "N") (fl-var "Z")))
  (4 8))

;;; A boxed variable (mutated AND captured) lives in a heap cell shared with the
;;; closures that see it. The frame records the cell, so mutations made through
;;; those closures show up without a store of their own.
(deftest frame-locals-boxed-var
  (progn (fl-boxed 3) *fl-captured*)
  (("N" . 3) ("ACC" . 103)))

(deftest frame-locals-boxed-var-from-inside-closure
  (progn (fl-boxed-inner 7) (list (fl-var "N") (fl-var "ACC")))
  (7 7))

(deftest frame-locals-labels-mutual-recursion
  (fl-labels-parity 4)
  (EVEN ODD))

;;; The frame holds values, so an assignment has to update it: a mutated variable
;;; must read as what it is now, not what it was bound to.
(deftest frame-locals-follows-setq
  (progn (fl-setq 3) *fl-captured*)
  (("A" . 3) ("S" . 8)))

;;; A tail self-call rebinds the parameters and loops inside one frame — the frame
;;; must show the current iteration's arguments.
(deftest frame-locals-follows-tail-call-rebind
  (progn (fl-tco 3 0) *fl-captured*)
  (("N" . 0) ("ACC" . 6)))

;;; Off (the default): nothing recorded.
(deftest frame-locals-off-records-nothing
  (progn (fl-off 1) *fl-captured*)
  nil)

(deftest frame-locals-off-value-unchanged
  (fl-off 1)
  2)

(deftest frame-locals-native-fixnum-slots
  (progn (fl-native-fixnum 4)
         (list (fl-var "N") (fl-var "ACC")
               (notnot (assoc "I" *fl-captured* :test #'string=))))
  (4 6 t))

(deftest frame-locals-native-float-slots
  (progn (fl-native-float 2.0d0) (list (fl-var "X") (fl-var "D")))
  (2.0d0 3.0d0))

;;; A native (all-fixnum) tail self-call rebinds Int64 slots; the frame follows.
(deftest frame-locals-native-tail-call-rebind
  (progn (fl-native-tco 3 0) *fl-captured*)
  (("N" . 0) ("ACC" . 6)))

;;; DOTCL:PRINT-FRAME-LOCALS renders the same "NAME = value" lines the debugger's
;;; :locals command shows.
(setf dotcl:*emit-frame-locals* t)
(defun fl-print-probe ()
  (let ((s (make-string-output-stream)))
    (dotcl:print-frame-locals 1 s)
    (get-output-stream-string s)))
(defun fl-printed (a)
  (let ((w (list a)))
    (fl-print-probe)))
(setf dotcl:*emit-frame-locals* nil)

(deftest frame-locals-print-frame-locals
  (let ((out (fl-printed 5)))
    (and (search "A = 5" out) (search "W = (5)" out) t))
  t)

(deftest frame-locals-print-frame-locals-empty
  (let ((s (make-string-output-stream)))
    (dotcl:print-frame-locals 999 s)
    (notnot (search "no locals recorded" (get-output-stream-string s))))
  t)

;;; A function entered through APPLY pushes no call-stack frame, so the position
;;; it shares is its caller's and must still read as the caller's variables.
(deftest frame-locals-borrowed-frame-keeps-caller-locals
  (progn (fl-borrow-caller 3) *fl-captured*)
  (("M" . 3) ("KEEP" . 103)))

;;; The sldb case: *DEBUGGER-HOOK* runs at the depth of the frame that signalled,
;;; and the debugger must see that frame's variables, not the hook's own.
(deftest frame-locals-debugger-hook-sees-signalling-frame
  (progn (fl-hook-caller 2) *fl-captured*)
  (("A" . 2) ("S" . 6)))

(deftest frame-locals-index-past-end
  (dotcl:frame-locals 999)
  nil)

(deftest frame-locals-negative-index
  (dotcl:frame-locals -1)
  nil)
