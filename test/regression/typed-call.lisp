;;; Calls through a typed entry: a caller that knows a global function's
;;; declaimed FTYPE (numeric arguments and result, see typed-entry.lisp) computes
;;; the arguments raw, fetches the function from the name on every call and
;;; enters its typed body directly when the function there has a typed entry of
;;; the signature the caller was compiled for. Any other function under the name
;;; gets the boxed arguments the ordinary way.
;;;
;;; What must hold:
;;; - the answers are those of the ordinary call, also after the callee is
;;;   redefined (into another typed body, into one without a typed entry, into
;;;   one with other types), with the caller and the callee compiled in memory or
;;;   loaded from fasls;
;;; - the callee's debugger frame is there, named and with its arguments;
;;; - the declaration is checked as for an ordinary call: the kind of each
;;;   argument by the caller from (safety 1) up, the range of an integer argument
;;;   by the callee's body from its (safety 1) up, the result of a function
;;;   without the typed entry by the caller;
;;; - a call in tail position is still the ordinary tail call.

(setf dotcl:*save-sil* t)

(defun %tc-outcome (thunk)
  (handler-case (funcall thunk)
    (type-error (e) (list :type-error (type-error-datum e) (type-error-expected-type e)))))

(defun %tc-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(declaim (notinline %tc-add %tc-addd %tc-mix %tc-inc8))
(declaim (ftype (function (fixnum fixnum) fixnum) %tc-add))
(defun %tc-add (a b) (+ a b))
(declaim (ftype (function (double-float double-float) double-float) %tc-addd))
(defun %tc-addd (a b) (* (+ a b) 2))
(declaim (ftype (function (fixnum double-float fixnum double-float) double-float) %tc-mix))
(defun %tc-mix (a x b y) (+ (* a x) (* b y)))
(declaim (ftype (function ((unsigned-byte 8)) (unsigned-byte 8)) %tc-inc8))
(defun %tc-inc8 (x) (1+ x))

(defun %tc-sum (n)
  (let ((acc 0))
    (declare (fixnum acc))
    (dotimes (i n acc) (setq acc (%tc-add acc i)))))
(defun %tc-dsum (n)
  (let ((x 0d0))
    (declare (double-float x))
    (dotimes (i n x) (setq x (%tc-addd x 0.25d0)))))
(defun %tc-generic (a b) (list (%tc-add a b) (%tc-addd 1d0 2d0) (%tc-mix 2 0.5d0 3 0.25d0)
                               (%tc-inc8 a)))

(deftest-emitting-only typed-call.uses-typed-entry
  (list (%tc-count "CALL-TYPED-SYM" #'%tc-sum)
        (%tc-count "CALL-TYPED-SYM" #'%tc-dsum)
        (%tc-count "CALL-TYPED-SYM" #'%tc-generic)
        (%tc-count "Invoke2" #'%tc-sum))
  (1 1 4 0))

(deftest typed-call.values
  (list (%tc-sum 1000) (%tc-dsum 4) (%tc-generic 3 1000000000000))
  (499500 7.5d0 (1000000000003 6d0 1.75d0 4)))

;;; The callee's frame: its name and its arguments, given raw.
(defun %tc-probe () (dotcl:backtrace-with-args))
(declaim (notinline %tc-framed))
(declaim (ftype (function (fixnum double-float) fixnum) %tc-framed))
(defvar *tc-frames* nil)
(defun %tc-framed (a x)
  (setq *tc-frames* (%tc-probe))
  (if (> x 0d0) a 0))
(defun %tc-framed-caller () (list (%tc-framed 7 2.5d0)))

(deftest typed-call.frame
  (progn (%tc-framed-caller)
         (find "%TC-FRAMED" *tc-frames* :key #'car :test #'string=))
  ("%TC-FRAMED" 7 2.5d0))

;;; Redefinition, in memory: the caller reaches whatever is under the name now.
(declaim (ftype (function (fixnum fixnum) fixnum) %tc-redef))
(defun %tc-redef (a b) (+ a b))
(defun %tc-redef-caller (x) (list (%tc-redef x 10)))

(deftest typed-call.redefine
  (let ((r '()))
    (push (%tc-redef-caller 2) r)
    (eval '(defun %tc-redef (a b) (* a b)))
    (push (%tc-redef-caller 2) r)
    ;; No typed entry: the ordinary entry answers.
    (eval '(defun %tc-redef (a b &optional (c 100)) (+ a b c)))
    (push (%tc-redef-caller 2) r)
    ;; Not compiled at all.
    (setf (fdefinition '%tc-redef) (lambda (a b) (- a b)))
    (push (%tc-redef-caller 2) r)
    (eval '(defun %tc-redef (a b) (- a b)))
    (push (%tc-redef-caller 2) r)
    (nreverse r))
  ((12) (20) (112) (-8) (-8)))

;;; Into a typed entry of another signature: the caller's raw arguments go in
;;; boxed, and the new definition checks them against its own declaration.
(declaim (ftype (function (fixnum fixnum) fixnum) %tc-resig))
(defun %tc-resig (a b) (+ a b))
(defun %tc-resig-caller (x) (list (%tc-resig x 10)))

(deftest-compiled-only typed-call.redefine-other-signature
  (let ((r (list (%tc-resig-caller 2))))
    (proclaim '(ftype (function (double-float fixnum) fixnum) %tc-resig))
    (eval '(defun %tc-resig (a b) (+ (floor a) b)))
    (push (%tc-outcome (lambda () (%tc-resig-caller 2))) r)
    (proclaim '(ftype (function (fixnum fixnum) fixnum) %tc-resig))
    (eval '(defun %tc-resig (a b) (- a b)))
    (push (%tc-resig-caller 2) r)
    (nreverse r))
  ((12) (:type-error 2 double-float) (-8)))

;;; The result of a function without the typed entry is checked against the
;;; declaration the caller relied on.
(declaim (ftype (function (fixnum) fixnum) %tc-liar))
(defun %tc-liar (x) (1+ x))
(defun %tc-liar-caller (x) (list (%tc-liar x)))

(deftest-compiled-only typed-call.untyped-callee-result-checked
  (progn
    (setf (fdefinition '%tc-liar) (lambda (x) (if (> x 0) "big" x)))
    (list (%tc-liar-caller -1) (%tc-outcome (lambda () (%tc-liar-caller 1)))))
  ((-1) (:type-error "big" fixnum)))

;;; Checks: the caller converts each argument to its kind (a TYPE-ERROR from
;;; (safety 1) up), the callee checks the range it declares from its own
;;; (safety 1) up, and trusts it at (safety 0) whoever calls.
(declaim (ftype (function ((unsigned-byte 8)) (unsigned-byte 8)) %tc-inc8-s0))
(defun %tc-inc8-s0 (x) (declare (optimize (safety 0))) (1+ x))
(defun %tc-checks (v s)
  (list (%tc-outcome (lambda () (%tc-add 1 s)))
        (%tc-outcome (lambda () (%tc-addd 1d0 v)))
        (%tc-outcome (lambda () (%tc-inc8 (+ v 296))))
        (%tc-outcome (lambda () (%tc-inc8 (+ v 251))))
        (%tc-inc8-s0 (+ v 296))))

(deftest-compiled-only typed-call.checked
  (%tc-checks 4 "x")
  ((:type-error "x" fixnum)
   (:type-error 4 double-float)
   (:type-error 300 (integer 0 255))
   (:type-error 256 (integer 0 255))
   301))

;;; A call in tail position stays the ordinary call, which is a tail call
;;; (the frame and the arguments go through TailPrep).
(defun %tc-tail (n) (%tc-add n 1))

(deftest-emitting-only typed-call.tail-position-keeps-ordinary-call
  (list (%tc-tail 41) (%tc-count "CALL-TYPED-SYM" #'%tc-tail))
  (42 0))

;;; Across fasls: the callee's FTYPE comes to the caller's file through the
;;; callee's fasl, and the caller's fasl follows the callee's redefinition.
(defvar *tc-dir*
  (let ((dir (concatenate 'string (regression-temp-dir) "/dotcl-tc-test/")))
    (ensure-directories-exist dir)
    dir))

(defun %tc-compile-and-load (source name)
  (let ((lisp (concatenate 'string *tc-dir* name ".lisp")))
    (with-open-file (s lisp :direction :output :if-exists :supersede)
      (write-string source s))
    (load (compile-file lisp))
    t))

(deftest-compiled-only typed-call.fasl
  (progn
    (%tc-compile-and-load
     "(in-package :cl-user)
      (declaim (notinline %tc-f-mul))
      (declaim (ftype (function (fixnum double-float) double-float) %tc-f-mul))
      (defun %tc-f-mul (n x) (* n x))"
     "tc-callee")
    (%tc-compile-and-load
     "(in-package :cl-user)
      (defun %tc-f-caller (n) (list (%tc-f-mul n 1.5d0)))"
     "tc-caller")
    (list (funcall (intern "%TC-F-CALLER") 4)
          (dotcl::%typed-entry-signature (fdefinition (intern "%TC-F-MUL")))
          (progn (eval `(defun ,(intern "%TC-F-MUL") (n x) (+ n x)))
                 (funcall (intern "%TC-F-CALLER") 4))
          (progn (eval `(defun ,(intern "%TC-F-MUL") (n x &optional (k 1d0)) (* k (- n x))))
                 (funcall (intern "%TC-F-CALLER") 4))))
  ((6d0) "LD>D" (5.5d0) (2.5d0)))

;;; In a typed body, a call in tail position to another function's typed entry
;;; of the same result kind is a tail call: mutual recursion runs in constant
;;; stack, through a callee with a frame and one without ((debug 0)), and the
;;; frame the chain runs under names the function the tail call reached.
(declaim (ftype (function (fixnum fixnum) fixnum) %tc-ev %tc-od))
(defun %tc-ev (n acc) (if (= n 0) acc (%tc-od (1- n) (+ acc 1))))
(defun %tc-od (n acc) (if (= n 0) acc (%tc-ev (1- n) (+ acc 2))))
(declaim (ftype (function (fixnum double-float) double-float) %tc-dev %tc-dod))
(defun %tc-dev (n x) (if (= n 0) x (%tc-dod (1- n) (+ x 1d0))))
(defun %tc-dod (n x) (declare (optimize (debug 0))) (if (= n 0) x (%tc-dev (1- n) (+ x 0.5d0))))

(deftest-emitting-only typed-call.typed-tail-calls-run-in-constant-stack
  (list (%tc-ev 10000000 0) (%tc-dev 10000000 0d0)
        ;; Entered through the boxed entry (a frame with boxed arguments).
        (funcall '%tc-ev 10000000 0))
  (15000000 7500000d0 15000000))

(declaim (ftype (function (fixnum) fixnum) %tc-ta %tc-tb))
(defun %tc-ta (n) (if (= n 0) 0 (%tc-tb n)))
(defun %tc-tb (n) (setq *tc-frames* (%tc-probe)) (* 2 n))
(defun %tc-tail-caller () (list (%tc-ta 5)))

(deftest-emitting-only typed-call.typed-tail-call-frame
  (progn (%tc-tail-caller)
         (list (find "%TC-TB" *tc-frames* :key #'car :test #'string=)
               (find "%TC-TA" *tc-frames* :key #'car :test #'string=)))
  (("%TC-TB" 5) nil))

;;; Not a tail call: a callee of another result kind, or one redefined without
;;; the typed entry (the call then goes the ordinary way, still in tail position).
(declaim (ftype (function (fixnum) double-float) %tc-to-d))
(defun %tc-to-d (n) (float n 1d0))
(declaim (ftype (function (fixnum) fixnum) %tc-kind %tc-tail-redef %tc-tail-redef-callee))
(defun %tc-kind (n) (floor (%tc-to-d n)))
(defun %tc-tail-redef-callee (n) (1+ n))
(defun %tc-tail-redef (n) (%tc-tail-redef-callee n))

(deftest typed-call.typed-tail-call-fallbacks
  (list (%tc-kind 7)
        (%tc-tail-redef 1)
        (progn (setf (fdefinition '%tc-tail-redef-callee) (lambda (n) (* 10 n)))
               (%tc-tail-redef 2)))
  (7 2 20))

;;; A typed body returns the raw value, so a call in its tail position to a
;;; function without a typed entry (of the same result kind) would have to return
;;; to the body to have its value taken out of the box: not a tail call, and a
;;; recursion through it grew the stack by two frames a round (the body, and the
;;; shell that boxes its result for the ordinary entry). A function with such a
;;; call goes without the typed entry and keeps the tail call; the combinations
;;; of a function with and without an FTYPE calling each other in tail position
;;; all run in constant stack.
(defconstant +tx-depth+ 1000000)

;; The shape from the report: the declared function calls one declared to
;; return T, which calls it back through FUNCALL.
(defvar *tx-barf*)
(declaim (ftype (function (t) (values t &optional)) %tx-foo))
(defun %tx-foo (x) (funcall *tx-barf* (1+ x)))
(declaim (ftype (function ((unsigned-byte 62)) (values (unsigned-byte 62) &optional)) %tx-bar))
(defun %tx-bar (x) (if (< x 10000000) (%tx-foo x) x))
(setf *tx-barf* #'%tx-bar)

;; With an FTYPE -> without, and the one without calling back by name.
(declaim (ftype (function (fixnum) fixnum) %tx-typed-a))
(defun %tx-typed-a (n) (if (= n 0) 0 (%tx-plain-a (1- n))))
(defun %tx-plain-a (n) (if (= n 0) 1 (%tx-typed-a (1- n))))

;; Without -> with, entered from the side without: a function whose FTYPE
;; gives no raw result (T) has no typed entry anyway, one returning FIXNUM has.
(defun %tx-plain-b (n) (if (= n 0) :plain (%tx-typed-b (1- n))))
(declaim (ftype (function (fixnum) t) %tx-typed-b))
(defun %tx-typed-b (n) (if (= n 0) :typed (%tx-plain-b (1- n))))
(declaim (ftype (function (fixnum) fixnum) %tx-typed-c))
(defun %tx-plain-c (n) (if (= n 0) -1 (%tx-typed-c (1- n))))
(defun %tx-typed-c (n) (if (= n 0) -2 (%tx-plain-c (1- n))))

;; With -> with: the same type, another integer range of the same kind, other
;; argument kinds, and a double-float pair.
(declaim (ftype (function ((unsigned-byte 62)) (unsigned-byte 62)) %tx-same-a %tx-same-b))
(defun %tx-same-a (n) (if (= n 0) 10 (%tx-same-b (1- n))))
(defun %tx-same-b (n) (if (= n 0) 20 (%tx-same-a (1- n))))
(declaim (ftype (function (fixnum) fixnum) %tx-range-a))
(declaim (ftype (function ((unsigned-byte 62)) (unsigned-byte 62)) %tx-range-b))
(defun %tx-range-a (n) (if (= n 0) 30 (%tx-range-b (1- n))))
(defun %tx-range-b (n) (if (= n 0) 40 (%tx-range-a (1- n))))
(declaim (ftype (function (fixnum double-float) fixnum) %tx-args-a))
(declaim (ftype (function (fixnum) fixnum) %tx-args-b))
(defun %tx-args-a (n x) (if (= n 0) (floor x) (%tx-args-b (1- n))))
(defun %tx-args-b (n) (if (= n 0) 50 (%tx-args-a (1- n) 60d0)))
(declaim (ftype (function (fixnum double-float) double-float) %tx-dbl-a %tx-dbl-b))
(defun %tx-dbl-a (n x) (if (= n 0) x (%tx-dbl-b (1- n) (+ x 1d0))))
(defun %tx-dbl-b (n x) (if (= n 0) x (%tx-dbl-a (1- n) x)))

;; Coalton's shape: an instance method with an FTYPE calls a polymorphic
;; function, which calls the method back through the dictionary, an inline
;; accessor FUNCALLing it under THE.
(defvar *tx-dict*)
(declaim (inline %tx-method))
(defun %tx-method (dict x)
  (declare (optimize (speed 3) (safety 0)))
  (funcall (car dict) x))
(defun %tx-poly (dict x) (the (values t &optional) (%tx-method dict (1+ x))))
(declaim (ftype (function ((unsigned-byte 62)) (values (unsigned-byte 62) &optional)) %tx-instance))
(defun %tx-instance (x)
  (if (>= x 2000000) x (the (values (unsigned-byte 62) &optional) (%tx-poly *tx-dict* x))))
(setf *tx-dict* (list #'%tx-instance))

(deftest-emitting-only typed-call.mixed-tail-calls-run-in-constant-stack
  (list (%tx-bar 0)
        (%tx-typed-a +tx-depth+) (%tx-plain-a +tx-depth+)
        (%tx-plain-b +tx-depth+) (%tx-typed-b (1+ +tx-depth+))
        (%tx-plain-c +tx-depth+) (%tx-typed-c +tx-depth+)
        (%tx-same-a +tx-depth+) (%tx-range-a +tx-depth+) (%tx-range-b +tx-depth+)
        (%tx-args-a +tx-depth+ 1d0) (%tx-dbl-a +tx-depth+ 0d0)
        (%tx-instance 0))
  (10000000 0 1 :plain :plain -1 -2 10 30 40 60 500000d0 2000000))

;;; Which of them keep the typed entry: the ones whose tail calls are to typed
;;; entries of their own result kind.
(deftest-emitting-only typed-call.typed-entry-only-without-boxed-tail-calls
  (mapcar (lambda (f) (dotcl::%typed-entry-signature (fdefinition f)))
          '(%tx-bar %tx-typed-a %tx-typed-c %tx-same-a %tx-range-a %tx-args-a %tx-dbl-a
            %tx-instance))
  (nil nil nil "L>L" "L>L" "LD>L" "LD>D" nil))
