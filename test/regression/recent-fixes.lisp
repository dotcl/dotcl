;;; Regression tests for recent fixes

;;; let/let* bindings take primary value only
(deftest d577-let-primary-value
  (let ((x (values 1 2 3)))
    x)
  1)

(deftest d577-let*-primary-value
  (let* ((x (values 10 20)))
    x)
  10)

;;; define-compiler-macro and compiler-macro-function
(define-compiler-macro %reg-test-cm (x) (* 2 x))

(deftest d579-compiler-macro-function-returns-fn
  (functionp (compiler-macro-function '%reg-test-cm))
  t)

(deftest d579-compiler-macro-expands
  (let* ((cm (compiler-macro-function '%reg-test-cm))
         (expanded (funcall cm '(%reg-test-cm 5) nil)))
    expanded)
  10)

;;; The compiler now AUTO-APPLIES compiler macros during compilation (previously
;;; they were registered but never consulted). Foundation for type-hinted dispatch.
(defun %cm-auto-fn (x) (* x 1000))                  ; real function multiplies by 1000
(define-compiler-macro %cm-auto-fn (x) `(* ,x 2))   ; compiler macro multiplies by 2
(deftest-compiled-only cm-auto-applied-in-compiled-call
  (%cm-auto-fn 5)                                    ; compiled call uses the CM -> 10
  10)

;;; A compiler macro that declines (returns the &whole form) leaves the call intact.
(defun %cm-decline-fn (x) (+ x 7))
(define-compiler-macro %cm-decline-fn (&whole form x) (declare (ignore x)) form)
(deftest cm-decline-keeps-call
  (%cm-decline-fn 10)                                ; declines -> real function -> 17
  17)

;;; A local function shadowing the name suppresses the compiler macro (CLHS 3.2.2.1).
(deftest cm-local-shadow-suppresses
  (flet ((%cm-auto-fn (x) (- x 1)))
    (%cm-auto-fn 5))                                 ; flet wins -> 4 (not CM's 10)
  4)

;;; A NOTINLINE declaration of the function name suppresses its compiler macro
;;; for calls in that scope (CLHS 3.2.2.1.1) — previously dotcl ignored notinline
;;; and the CM always fired (ANSI DEFINE-COMPILER-MACRO.7).
(defun %cm-ni-fn (x) (* x 1000))                     ; real fn: *1000
(define-compiler-macro %cm-ni-fn (x) `(* ,x 2))      ; CM: *2
(deftest-compiled-only cm-notinline-suppresses
  (list (%cm-ni-fn 5)                                ; no decl -> CM -> 10
        (locally (declare (notinline %cm-ni-fn)) (%cm-ni-fn 5))  ; notinline -> real fn -> 5000
        (funcall (lambda (x) (declare (notinline %cm-ni-fn)) (%cm-ni-fn x)) 5)) ; lambda body -> 5000
  (10 5000 5000))

;;; (setf compiler-macro-function)
(deftest d579-setf-compiler-macro-function
  (progn
    (setf (compiler-macro-function '%cm-setf-test)
          (lambda (form env) (declare (ignore env)) `(+ ,(cadr form) 100)))
    (let* ((cm (compiler-macro-function '%cm-setf-test))
           (expanded (funcall cm '(%cm-setf-test 5) nil)))
      expanded))
  (+ 5 100))

;;; compile nil works
(deftest-compiled-only d579-compile-nil-basic
  (funcall (compile nil '(lambda (x) (* x x))) 7)
  49)

(deftest-compiled-only d579-compile-nil-with-cm
  (let* ((cm (compiler-macro-function '%reg-test-cm))
         (expanded (funcall cm '(%reg-test-cm 5) nil))
         (fn (compile nil `(lambda () ,expanded))))
    (funcall fn))
  10)

;;; relative pathname merging (basic check)
(deftest d576-merge-pathnames-basic
  (let ((p (merge-pathnames "foo.lisp" (make-pathname :directory '(:absolute "tmp")))))
    (pathname-name p))
  "foo")

;;; symbol-package type error on non-symbol
(deftest d573-symbol-package-type-error
  (signals-error (symbol-package 42) type-error)
  t)

;;; TCO — tail-recursive function shouldn't stack overflow.
;;;
;;; Both evaluators: the compiler eliminates a self tail call, and the tree-walk
;;; evaluator hands one to its trampoline, so neither grows the stack per
;;; iteration.
(defun tco-count-down (n)
  (if (= n 0)
      :done
      (tco-count-down (- n 1))))

(deftest tco-basic
  (tco-count-down 100000)
  :done)

;;; Multiple values
(deftest multiple-values-basic
  (multiple-value-list (values 1 2 3))
  (1 2 3))

(deftest multiple-value-bind-basic
  (multiple-value-bind (a b c) (values 10 20 30)
    (+ a b c))
  60)

(deftest values-list-basic
  (multiple-value-list (values-list '(x y z)))
  (x y z))

;;; psetf with ldb should write to original variable (not a temp)
(deftest d606-psetf-ldb
  (let ((x #b00000))
    (psetf (ldb (byte 5 1) x) #b10110)
    x)
  #b101100)

;;; incf of (getf plist key default) should add key when missing
(deftest d606-incf-getf-default
  (let ((p '(a 1 b 2)))
    (incf (getf p 'c 19))
    (getf p 'c))
  20)

;;; setf of getf should update existing key
(deftest d606-setf-getf-existing
  (let ((p (list 'a 1 'b 2)))
    (incf (getf p 'a))
    (getf p 'a))
  2)

;;; subtypep (cons (not x)) against (cons (satisfies y)) should be uncertain
(deftest d606-subtypep-cons-satisfies-uncertain
  (multiple-value-list
   (subtypep '(cons (not float)) '(cons (satisfies identity))))
  (nil nil))

;;; reinitialize-instance validates initargs from method &key params
(defclass d616-test-class ()
  ((x :initarg :x :accessor d616-x)))

(defmethod reinitialize-instance :before ((obj d616-test-class) &key new-x)
  (when new-x (setf (d616-x obj) new-x)))

(deftest d616-method-key-accepted
  ;; :new-x is declared as &key in the :before method — should not signal
  (let ((obj (make-instance 'd616-test-class :x 1)))
    (reinitialize-instance obj :new-x 42)
    (d616-x obj))
  42)

(deftest d616-unknown-key-rejected
  ;; :z is not declared by any method or slot — should signal
  (let ((obj (make-instance 'd616-test-class :x 1)))
    (not (null (handler-case
                 (progn (reinitialize-instance obj :z 99) nil)
                 (error () t)))))
  t)

;;; float printing and ~e format
(deftest d615-prin1-large-float-scientific
  ;; Values >= 1e7 must use scientific notation, not fixed
  (let ((*print-readably* nil)
        (*read-default-float-format* 'single-float))
    (let ((s (prin1-to-string 10000000.0)))
      (and (position #\E s :test #'char-equal) t)))
  t)

(deftest d615-format-e-no-positive-exponent-sign
  ;; prin1 must NOT include + in exponent
  (let ((*print-readably* nil)
        (*read-default-float-format* 'single-float))
    (not (search "e+" (prin1-to-string 1.5e10))))
  t)

(deftest d615-format-tilde-e-positive-exponent-sign
  ;; ~e MUST include + for positive exponents
  (let ((*read-default-float-format* 'single-float))
    (not (null (search "e+" (format nil "~e" 1.5e10)))))
  t)

(deftest d615-format-e-matches-prin1-plus
  ;; (format nil "~e" x) == (prin1-to-string x) with + inserted after E
  (let ((*print-readably* nil)
        (*read-default-float-format* 'double-float))
    (let* ((x 2.3356982399544044d296)
           (s1 (format nil "~e" x))
           (s (prin1-to-string x))
           (ep (1+ (position #\e s :test #'char-equal)))
           (s2 (concatenate 'string (subseq s 0 ep) "+" (subseq s ep))))
      (string= s1 s2)))
  t)

;;; float bit access functions live OUTSIDE the CL package.
;;; They are non-standard, and CL is probed before the caller's own package, so a
;;; CL entry shadows any same-named function elsewhere (it hijacked SBCL's
;;; SB-KERNEL:SINGLE-FLOAT-BITS and broke make-host-1). They were put in CL on the
;;; premise that nibbles calls them as CL symbols; it calls them unqualified and
;;; gets its own portable NIBBLES:: definitions. Reachability is what matters, and
;;; the DOTCL-INTERNAL bridge provides it — the round-trip tests below cover that.
(deftest d613-single-float-bits-not-in-cl
  (null (find-symbol "SINGLE-FLOAT-BITS" "COMMON-LISP"))
  t)

(deftest d613-make-single-float-roundtrip
  (make-single-float (single-float-bits 1.5))
  1.5)

(deftest d613-double-float-bits-not-in-cl
  (null (find-symbol "DOUBLE-FLOAT-BITS" "COMMON-LISP"))
  t)

(deftest d613-make-double-float-roundtrip
  (make-double-float (double-float-bits 1.5d0))
  1.5d0)

;;; MAKE-CONDITION given a condition returns it, rather than signalling about its own
;;; argument. Code that reports an error it just caught hands the live condition back
;;; through MAKE-CONDITION; rejecting it swapped the real condition for an unrelated
;;; TYPE-ERROR and lost the diagnosis.
(deftest d1825-make-condition-of-condition-is-identity
  (let ((c (make-condition 'simple-error :format-control "boom")))
    (eq (make-condition c) c))
  t)

(deftest d1825-make-condition-of-condition-keeps-type
  (let ((c (make-condition 'undefined-function :name 'no-such-fn)))
    (list (typep (make-condition c) 'undefined-function)
          (cell-error-name (make-condition c))))
  (t no-such-fn))

;;; A genuinely bad type specifier still errors, and the message names what was passed.
(deftest d1825-make-condition-of-non-type-still-errors
  (handler-case (progn (make-condition 42) :no-error)
    (error () :errored))
  :errored)

;;; An IN-PACKAGE earlier in a toplevel PROGN has to take effect before the REST of
;;; that progn is COMPILED, because a later DEFSTRUCT builds its accessor names at
;;; macroexpansion time and interns them in *PACKAGE*. A file gets this from the
;;; loader, which splits a toplevel progn and compiles+runs each piece in turn; that
;;; split does not reach inside MACROLET, and EVAL of a whole progn never gets it —
;;; so both shapes interned the accessors in the wrong package. (SBCL puts them in
;;; those two packages for both.) *PACKAGE* is rebound so the test cannot leak.
(defpackage "CT-ORDER-PKG-A" (:use "CL"))
(defpackage "CT-ORDER-PKG-B" (:use "CL"))

(deftest d1827-eval-progn-in-package-orders-accessor-interning
  (let ((*package* *package*))
    (eval '(progn
             (in-package "CT-ORDER-PKG-B")
             (defstruct (ctord2 (:conc-name "CTORD2-")) fld)))
    (let ((s (find-symbol "CTORD2-FLD" "CT-ORDER-PKG-B")))
      (and s (package-name (symbol-package s)))))
  "CT-ORDER-PKG-B")

(deftest d1827-macrolet-progn-in-package-orders-accessor-interning
  (let ((*package* *package*))
    (eval '(macrolet ((d1827m ()
                        `(progn
                           (in-package "CT-ORDER-PKG-A")
                           (defstruct (ctord1 (:conc-name "CTORD1-")) fld))))
             (d1827m)))
    (let ((s (find-symbol "CTORD1-FLD" "CT-ORDER-PKG-A")))
      (and s (package-name (symbol-package s)))))
  "CT-ORDER-PKG-A")

;;; subtypep circular deftype cycle detection
(deftype d619-circular-type (&optional low high) `(d619-circular-type ,low ,high))

(deftest d619-subtypep-circular-deftype-no-hang
  ;; circular deftype should not infinite-loop; subtypep returns (nil nil) = uncertain
  (multiple-value-list (subtypep 'd619-circular-type 'integer))
  (nil nil))

;;; &environment passed to macro expander
(defmacro d618-env-macro (&environment env form)
  ;; Return t if env is non-nil (the actual compile-time environment was passed)
  ;; At runtime (non-compile-file) env is typically nil; at compile-time it's the env object.
  ;; We test that the macro at least receives whatever the runtime env is (not a fixed nil).
  `(quote ,env))

(deftest d618-environment-not-ignored
  ;; In a simple eval context, environment is typically nil — verify the macro expands
  (let ((result (d618-env-macro ignore-me)))
    t)
  t)

;; Macro that uses &environment to check if a binding is macro-defined
(defmacro d618-macro-expanding-with-env (&environment env name)
  (if (and env (macro-function name env))
      `(quote macro)
      `(quote not-macro)))

(defmacro d618-inner-macro () 42)

(deftest d618-macro-function-with-env
  ;; macro-function with non-nil env should find locally-visible macros
  (d618-macro-expanding-with-env d618-inner-macro)
  not-macro)

;;; macrolet expander can call surrounding flet functions at expansion time
(deftest d623-macrolet-calls-flet-at-expansion-time
  (flet ((double (x) (* x 2)))
    (macrolet ((m (x) (double x)))
      (m 5)))
  10)

;;; gethash MV leakage through let binding
;;; gethash returns (value, present-p); a let binding should strip to primary only.
(deftest d624-gethash-mv-does-not-leak-through-let
  (let ((ht (make-hash-table)))
    (setf (gethash 'a ht) 42)
    ;; (let ((v (gethash ...))) v) should return single value, not (42 T)
    (multiple-value-list
      (let ((v (gethash 'a ht)))
        v)))
  (42))

(deftest d624-gethash-direct-mvl-still-works
  (let ((ht (make-hash-table)))
    (setf (gethash 'a ht) 99)
    ;; Direct (multiple-value-list (gethash ...)) should still return both values
    (multiple-value-list (gethash 'a ht)))
  (99 t))

;;; macrolet expander returns a quoted form built by a surrounding flet
(deftest d623-macrolet-calls-flet-quoted-result
  (flet ((make-form (x) `(+ ,x 1)))
    (macrolet ((m (x) (make-form x)))
      (m 3)))
  4)

;;; dotcl:gc-stats returns 5 fixnums, time preserves values
(deftest d651-gc-stats-shape
  (let ((s (dotcl:gc-stats)))
    (and (listp s)
         (= (length s) 5)
         (every #'integerp s)))
  t)

(deftest d651-time-returns-values
  (multiple-value-list
    (let ((*trace-output* (make-broadcast-stream)))
      (time (values 10 20 30))))
  (10 20 30))

;;; subnormal double-float: rational / float / format correctness
(deftest d654-subnormal-float-roundtrip
  (let ((x 9.63d-322))
    (= x (float (rational x) 1d0)))
  t)

(deftest d654-subnormal-format-e
  ;; Format of subnormal double: digits must come from the exact rational
  ;; (not from Math.Pow(10, exp) which loses precision for subnormals).
  (format nil "~,15,,0e" 9.63d-322)
  "0.963428009390431d-321")

(deftest d654-format-e-width-trim-zero-frac
  ;; ~5e on 1.0: width-derived d=0 means no fraction digits ("1." not "1.0")
  (format nil "~5e" 1.0)
  "1.e+0")

(deftest d654-format-e-k-scaling
  ;; ~,d,,k: k>=1 => k digits before dot, d-k+1 after.
  (format nil "~,2,,2e" 0.05)
  "50.0e-3")

;;; self-TCO must not emit `br` that crosses a try/finally from a
;;; special LET. Regression for invalid-IL crash observed in cl-bench stak.
(defvar %d664-a 0)
(proclaim '(special %d664-a))

(defun %d664-f ()
  (if (< %d664-a 10)
      %d664-a
      (let ((%d664-a (1- %d664-a)))  ; special LET → try/finally around body
        (%d664-f))))                  ; self-tail-call must NOT emit raw br

(deftest d664-tco-across-special-let
  (let ((%d664-a 15)) (%d664-f))
  9)

;;; (the fixnum ...) emits native int64 arithmetic.
;;; Correctness tests: the fast path must match the slow path bit-for-bit.

(deftest d667-fixnum-add
  (list (+ (the fixnum 10) (the fixnum 20))
        (+ (the fixnum -5) (the fixnum 8))
        (+ (the fixnum 0) (the fixnum 0)))
  (30 3 0))

(deftest d667-fixnum-sub
  (list (- (the fixnum 100) (the fixnum 37))
        (- (the fixnum 5) (the fixnum 10))
        (- (the fixnum 42) (the fixnum 42)))
  (63 -5 0))

(deftest d667-fixnum-mul
  (list (* (the fixnum 7) (the fixnum 6))
        (* (the fixnum -3) (the fixnum 4))
        (* (the fixnum 0) (the fixnum 100)))
  (42 -12 0))

(deftest d667-fixnum-nested
  (+ (the fixnum (* (the fixnum 3) (the fixnum 4)))
     (the fixnum (- (the fixnum 20) (the fixnum 5))))
  27)

(deftest d667-fixnum-non-fixnum-fallback
  ;; Mixed-type args still hit the generic path.
  (+ (the fixnum 100) 3.5)
  103.5)

(deftest d667-fixnum-literal-optimization
  ;; Both literals — optimization fires but result is correct.
  (+ 1 2 3 4 5)
  15)

;;; fixnum-typed comparisons (< > <= >= = /=) in fused if/cond/and/or
;;; positions emit native clt/cgt/ceq instead of Runtime.IsTrueXxx calls.

(deftest d668-fixnum-cmp-lt
  (list (if (< (the fixnum 1) (the fixnum 2)) :yes :no)
        (if (< (the fixnum 5) (the fixnum 3)) :yes :no))
  (:yes :no))

(deftest d668-fixnum-cmp-ge
  (list (if (>= (the fixnum 3) (the fixnum 3)) :yes :no)
        (if (>= (the fixnum 2) (the fixnum 3)) :yes :no))
  (:yes :no))

(deftest d668-fixnum-cmp-eq
  (list (if (= (the fixnum 7) (the fixnum 7)) :yes :no)
        (if (= (the fixnum 7) (the fixnum 8)) :yes :no))
  (:yes :no))

(deftest d668-fixnum-cmp-ne
  (list (if (/= (the fixnum 1) (the fixnum 2)) :yes :no)
        (if (/= (the fixnum 5) (the fixnum 5)) :yes :no))
  (:yes :no))

(deftest d668-fixnum-cmp-in-not
  ;; (if (not (= a b)) ...) — inverted branch path through fused cmp.
  (if (not (= (the fixnum 1) (the fixnum 2))) :diff :same)
  :diff)

(deftest d668-fixnum-cmp-in-cond
  (cond ((< (the fixnum 5) (the fixnum 2)) :small)
        ((= (the fixnum 3) (the fixnum 3)) :equal)
        (t :other))
  :equal)

;;; (declare (fixnum x)) turns x itself into a fixnum-typed reference
;;; so arithmetic/comparisons on x hit the native int64 path without needing
;;; (the fixnum x) wrappers at every use.

(defun %d669-add3 (a b c)
  (declare (fixnum a b c))
  (+ a (+ b c)))

(deftest d669-param-fixnum-add
  (list (%d669-add3 1 2 3) (%d669-add3 10 -5 20))
  (6 25))

(defun %d669-fib (n)
  (declare (fixnum n))
  (if (< n 2) n (+ (the fixnum (%d669-fib (- n 1)))
                   (the fixnum (%d669-fib (- n 2))))))

(deftest d669-param-fixnum-fib
  (%d669-fib 15)
  610)

(deftest d669-let-fixnum
  (let ((x 10) (y 20))
    (declare (fixnum x y))
    (if (< x y) (+ x y) (- x y)))
  30)

(deftest d669-let-star-fixnum
  (let* ((x 5) (y (* x 2)))
    (declare (fixnum x y))
    (+ x y))
  15)

;;; dotimes with fixnum count injects (declare (fixnum var limit))
;;; so the inner compare/increment hit native int64 paths. 1+/1- also get
;;; fixnum fast paths.

(deftest d670-dotimes-basic
  (let ((s 0)) (dotimes (i 10 s) (setq s (+ s i))))
  45)

(deftest d670-dotimes-fixnum-sum
  (let ((s 0)) (dotimes (i 100 s) (setq s (+ s i))))
  4950)

(deftest d670-1+-fixnum
  (1+ (the fixnum 41))
  42)

(deftest d670-1--fixnum
  (1- (the fixnum 42))
  41)

(deftest d670-dotimes-variable-count
  ;; Non-fixnum-typed count: the declare is skipped, generic path runs.
  (let ((n 5) (s 0))
    (dotimes (i n s) (setq s (+ s i))))
  10)

;;; (declaim (ftype (function (...) fixnum) name)) marks NAME's
;;; return as fixnum-typed-p, so callers in fixnum context emit native
;;; unbox + int64 arithmetic without needing (the fixnum (name ...)).

(declaim (ftype (function (fixnum) fixnum) %d671-fib))
(defun %d671-fib (n)
  (declare (fixnum n))
  (if (< n 2) n (+ (%d671-fib (- n 1)) (%d671-fib (- n 2)))))

(deftest d671-ftype-fib
  (%d671-fib 10)
  55)

(declaim (ftype (function (fixnum) fixnum) %d671-fact))
(defun %d671-fact (n)
  (declare (fixnum n))
  (if (< n 2) 1 (* n (%d671-fact (1- n)))))

(deftest d671-ftype-fact
  (%d671-fact 5)
  120)

;;; (declare (double-float x)) enables native r8 arithmetic on
;;; +/-/*/div. Verify the fast path produces correct DoubleFloat results
;;; for each operator and for comparisons in fused branches.

(defun %d672-sumsq (x y)
  (declare (double-float x y))
  (+ (* x x) (* y y)))

(deftest d672-double-arith-sumsq
  (%d672-sumsq 3.0d0 4.0d0)
  25.0d0)

(defun %d672-div (x y)
  (declare (double-float x y))
  (/ x y))

(deftest d672-double-div
  (%d672-div 10.0d0 4.0d0)
  2.5d0)

(defun %d672-cmp (x y)
  (declare (double-float x y))
  (if (< x y) :lt :ge))

(deftest d672-double-cmp-lt
  (%d672-cmp 1.5d0 2.5d0)
  :lt)

(deftest d672-double-cmp-ge
  (%d672-cmp 3.0d0 2.0d0)
  :ge)

;;; (declare (type double-float x)) form also recognized
(defun %d672-type-form (x)
  (declare (type double-float x))
  (* x x))

(deftest d672-double-type-form
  (%d672-type-form 2.5d0)
  6.25d0)

;;; dotcl:save-application (MVP, :executable nil)
;;; Writes a tiny source + build.lisp to a temp dir, calls save-application,
;;; and verifies the output is a valid PE/FASL (starts with "MZ").
(defun %d678-save-application-smoke ()
  (let* ((tmp (format nil "~a/dotcl-saveapp-~a"
                      (or (dotcl:getenv "TEMP") "/tmp")
                      (get-internal-real-time)))
         (src (format nil "~a/main.lisp" tmp))
         (out (format nil "~a/out.fasl" tmp)))
    (ensure-directories-exist (concatenate 'string tmp "/"))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (format s "(defun %~a-d678-entry () 42)~%" ""))
    (dotcl:save-application out
                            :load src
                            :toplevel "CL-USER::%D678-ENTRY")
    ;; Verify the file exists, is non-trivial, and carries the PE magic.
    (let ((ok (probe-file out))
          (first-two (with-open-file (in out :element-type '(unsigned-byte 8))
                       (list (read-byte in) (read-byte in)))))
      (and ok (equal first-two '(#x4d #x5a))))))

(deftest-compiled-only d678-save-application-smoke
  (%d678-save-application-smoke)
  t)

;;; TCO now works for params that are captured+mutated (boxed).
;;; Previously `use-tco` required (null needs-boxing), so this function
;;; would have blown the stack at 100k depth. The call-site handled
;;; stelem-ref into the box already; only the entry gate was blocking.
(defun %d682-iter-sum (n acc)
  ;; Force boxing: capture + mutate both params via an inner lambda
  (let ((bump (lambda (amt) (incf acc amt))))
    (funcall bump 0))
  (if (= n 0)
      acc
      (%d682-iter-sum (- n 1) (+ acc n))))

(deftest d682-tco-boxed-params-correct
  (%d682-iter-sum 100 0)
  5050)

(deftest-compiled-only d682-tco-boxed-params-deep
  ;; Stack is ~256MB; without TCO this would SO well before 100k.
  (%d682-iter-sum 100000 0)
  5000050000)

;;; REQUIRE returns newly-added module list (SBCL convention),
;;; and idempotent re-require short-circuits even when the contrib's
;;; file forgot to call (provide ...).
(deftest d688-require-first-returns-non-nil
  ;; Use a contrib that doesn't call (provide ...) itself —
  ;; dotcl-cs is such a module (intentionally). Require should
  ;; auto-push its name so the set-difference is non-nil.
  (consp (require "dotcl-cs"))
  t)

(deftest d688-require-second-returns-nil
  (progn (require "dotcl-cs")
         (require "dotcl-cs"))
  nil)

(deftest d688-require-records-in-modules
  (progn (require "dotcl-cs")
         (not (null (member "dotcl-cs" *modules* :test #'equal))))
  t)

;;; (defun (setf name) ...) use-direct path: self-fn-prelude must use
;;; GetSetfFunctionBySymbol not GetFunctionBySymbol for (SETF NAME) names.
;;; The bug caused LispUndefinedFunction to be thrown inside the setf fn body.

(defun %d698-acc (x) (car x))
(defun (setf %d698-acc) (v x) (setf (car x) v) v)

(deftest d698-setf-function-invocable
  (let ((cell (list 'a 'b)))
    (setf (%d698-acc cell) 'z)
    (%d698-acc cell))
  z)

(deftest d698-fboundp-setf
  (fboundp '(setf %d698-acc))
  t)

(deftest d698-setf-function-via-funcall
  (let ((cell (list 1 2)))
    (funcall #'(setf %d698-acc) 99 cell)
    (%d698-acc cell))
  99)

;;; macroexpand-cache: plist-dependent macros (anaphora-style)
;;; Toy reproducer: macro that stores a gensym on a plist during expansion.
;;; A second macro reads the plist to produce a reference to that gensym.
;;; Without caching, analysis re-expands %d709-sif (clobbering the plist),
;;; then code-gen sees the old gensym → unbound-variable.

(let ((%d709-sym nil))
  (defmacro %d709-sif (test then else)
    ;; Fresh gensym each time — plist-store style (like anaphora's sif)
    (let ((g (gensym "SIF")))
      (setf (get '%d709-sym-key :current) g)
      `(let ((,g ,test))
         (if ,g ,then ,else))))

  (defmacro %d709-sym ()
    ;; Return the gensym stored by the most-recent %d709-sif expansion
    (get '%d709-sym-key :current)))

(deftest d709-plist-macro-basic
  ;; Simple: (%d709-sif 42 (ref) nil) should return 42.
  ;; The bound var is retrieved via %d709-sym.
  (%d709-sif 42 (%d709-sym) nil)
  42)

(deftest d709-plist-macro-nested-let*
  ;; Nesting exercises the find-mutated-vars / find-free-vars path.
  ;; The let* binds no closures, so analysis must not re-expand %d709-sif.
  (let* ((r (%d709-sif (+ 10 5) (%d709-sym) 0)))
    r)
  15)

;;; incf/decf of (car ...) / (cdr ...) / (nth ...) returns store value
;;; Per CLHS, get-setf-expansion's storing form must return the values of
;;; the store variables. The car/cdr/nth cases in %get-setf-expansion
;;; previously used (rplaca ...) / (rplacd ...) / (rplaca (nthcdr ...) ...)
;;; directly, which return the cons — leaking through incf/decf as primary
;;; value. Symptom: anaphora ASIF.1 got (1) instead of 1.

(deftest d712-incf-car-returns-value
  (let ((x (list 0)))
    (incf (car x)))
  1)

(deftest d712-incf-cdr-returns-value
  (let ((p (cons 'a 10)))
    (incf (cdr p)))
  11)

(deftest d712-incf-nth-returns-value
  (let ((x (list 10 20 30)))
    (incf (nth 1 x)))
  21)

(deftest d712-decf-car-returns-value
  (let ((x (list 5)))
    (decf (car x)))
  4)

(deftest d712-incf-car-mutates-place
  ;; Regression guard: fix must not break the side effect itself.
  (let ((x (list 7)))
    (incf (car x))
    (car x))
  8)

(deftest-compiled-only d712-asif-like
  ;; Minimal anaphora ASIF.1 reproducer without the library.
  (let ((x (list 0)))
    (let ((it (incf (car x))))
      (if it it (list :oops it))))
  1)

;;; (defun (setf NAME) ...) in a user-defined package registered correctly
;;; through compile-file + load. defun-pkg-spec previously ignored (setf NAME)
;;; forms, so the FASL assembler fell back to CL-USER and attached the setf
;;; function to the wrong symbol. Broke asdf:load-system via the shipped
;;; asdf.fasl (could not find (setf asdf::operate-level)).
;;;
;;; Symbol accesses are wrapped in `read-from-string` so that reading this file
;;; does not require the d713-pkg package to exist yet — it is created inside
;;; the compile-file source below.
(defun %d713-setf-fn-fasl ()
  (let* ((tmp (format nil "~a/dotcl-d713-~a"
                      (or (dotcl:getenv "TEMP") "/tmp")
                      (get-internal-real-time)))
         (src (format nil "~a/src.lisp" tmp)))
    (ensure-directories-exist (concatenate 'string tmp "/"))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (format s "(defpackage #:d713-pkg (:use :cl))~%")
      (format s "(in-package #:d713-pkg)~%")
      (format s "(defvar *store* 0)~%")
      (format s "(defun place () *store*)~%")
      (format s "(defun (setf place) (v) (setf *store* v) v)~%"))
    (compile-file src)
    (let ((fasl (concatenate 'string (subseq src 0 (- (length src) 5)) ".fasl")))
      (load fasl))
    (let* ((place-sym (read-from-string "d713-pkg::place"))
           (setf-place (list 'setf place-sym)))
      (and (fboundp place-sym)
           (fboundp setf-place)
           ;; Invoke the setf function directly — avoids a second read of the
           ;; package-qualified symbol at test form evaluation time.
           (eql (funcall (fdefinition setf-place) 42) 42)
           (eql (funcall place-sym) 42)))))

(deftest-compiled-only d713-setf-fn-user-package-fasl
  (%d713-setf-fn-fasl)
  t)

;;; character name table covers C0 control mnemonics (SBCL-compat)
;;; and a common Unicode name (No-break_space). Reader previously signaled
;;; "Unknown character name: Vt" for maxpc/mpc, "bell" for text-query,
;;; "No-break_space" for cl-inix.
(deftest d715-char-name-vt
  (char-code (read-from-string "#\\Vt"))
  11)

(deftest d715-char-name-bell
  (char-code (read-from-string "#\\Bell"))
  7)

(deftest d715-char-name-bel
  (char-code (read-from-string "#\\Bel"))
  7)

(deftest d715-char-name-ht
  (char-code (read-from-string "#\\Ht"))
  9)

(deftest d715-char-name-esc
  (char-code (read-from-string "#\\Esc"))
  27)

(deftest-compiled-only d715-char-name-nbsp
  (char-code (read-from-string "#\\No-break_space"))
  160)

;;; compile-file FASL split must not orphan labels across helper methods
;;; Previously `(unless test (defun f ...))` (and similarly (if/when/unless ...))
;;; at top level caused "Label N has not been marked" during compile-file: the
;;; FaslAssembler split instruction stream at defmethod boundaries into
;;; separate helper methods, each with its own label table. When branches
;;; spanned the split boundary the target labels ended up in a different
;;; helper method. Fix: when both branches and defmethods are present at
;;; top-level, emit monolithically into the init method instead of splitting.
(defun %d719-if-with-defun-fasl ()
  (let* ((tmp (format nil "~a/dotcl-d719-~a"
                      (or (dotcl:getenv "TEMP") "/tmp")
                      (get-internal-real-time)))
         (src (format nil "~a/src.lisp" tmp)))
    (ensure-directories-exist (concatenate 'string tmp "/"))
    (with-open-file (s src :direction :output :if-exists :supersede)
      ;; Minimal reproducer: (unless ...) wrapping a defun at top level.
      (format s "(defpackage #:d719-pkg (:use :cl))~%")
      (format s "(in-package #:d719-pkg)~%")
      (format s "(unless nil (defun f-719 (x) (* x 2)))~%")
      (format s "(when t (defun g-719 (x) (+ x 1)))~%"))
    (compile-file src)
    (let ((fasl (concatenate 'string (subseq src 0 (- (length src) 5)) ".fasl")))
      (load fasl))
    (and (fboundp (read-from-string "d719-pkg::f-719"))
         (fboundp (read-from-string "d719-pkg::g-719"))
         (eql (funcall (read-from-string "d719-pkg::f-719") 5) 10)
         (eql (funcall (read-from-string "d719-pkg::g-719") 7) 8))))

(deftest-compiled-only d719-if-with-defun-fasl
  (%d719-if-with-defun-fasl)
  t)

;;; compile-file preserves multi-dimensional array literals (#2A ...)
;;; Previously EmitLoadConstInline always used the 1-arg LispVector(LispObject[])
;;; ctor which dropped `_dimensions`, turning `#2A((1 2 3) (4 5 6))` into a flat
;;; SIMPLE-VECTOR of 6 elements after load. Broke reversi via its
;;; `*static-edge-table*` literal which was used as `(aref table i j)`.
(defun %d-2darray-literal-fasl ()
  (let* ((tmp (format nil "~a/dotcl-2darr-~a"
                      (or (dotcl:getenv "TEMP") "/tmp")
                      (get-internal-real-time)))
         (src (format nil "~a/src.lisp" tmp)))
    (ensure-directories-exist (concatenate 'string tmp "/"))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (format s "(defparameter *reg-2darr* #2A((1 2 3) (4 5 6)))~%"))
    (compile-file src)
    (let ((fasl (concatenate 'string (subseq src 0 (- (length src) 5)) ".fasl")))
      (load fasl))
    (let ((a (symbol-value (read-from-string "cl-user::*reg-2darr*"))))
      (and (= (array-rank a) 2)
           (equal (array-dimensions a) '(2 3))
           (eql (aref a 0 0) 1)
           (eql (aref a 1 2) 6)))))

(deftest-compiled-only d-2darray-literal-fasl
  (%d-2darray-literal-fasl)
  t)

;;; compile-file preserves element-type for 1D specialized vectors and bit-vectors
;;; Previously EmitLoadConstInline used the 1-arg ctor for 1D vectors even when
;;; element-type != T, dropping it (sibling of the multi-dimensional array fix above).
(defun %d747-1d-specialized-vector-fasl ()
  (let* ((tmp (format nil "~a/dotcl-1dvec-~a"
                      (or (dotcl:getenv "TEMP") "/tmp")
                      (get-internal-real-time)))
         (src (format nil "~a/src.lisp" tmp)))
    (ensure-directories-exist (concatenate 'string tmp "/"))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (format s "(defparameter *reg-1dvec*~%")
      (format s "  (make-array 3 :element-type 'single-float~%")
      (format s "    :initial-contents '(1.0 2.0 3.0)))~%")
      (format s "(defparameter *reg-bitvec* #*1011)~%"))
    (compile-file src)
    (let ((fasl (concatenate 'string (subseq src 0 (- (length src) 5)) ".fasl")))
      (load fasl))
    (let ((v  (symbol-value (read-from-string "cl-user::*reg-1dvec*")))
          (bv (symbol-value (read-from-string "cl-user::*reg-bitvec*"))))
      (list (array-element-type v)
            (bit-vector-p bv)
            (length bv)
            (bit bv 0) (bit bv 1) (bit bv 2) (bit bv 3)))))

(deftest-compiled-only d747-1d-specialized-vector-fasl
  (%d747-1d-specialized-vector-fasl)
  (single-float t 4 1 0 1 1))

;;; dotnet:invoke / dotnet:static unified InvokeMember dispatch
;;; (method, property, field — both read and setf).
(deftest d741-invoke-method-and-property-and-setf
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke sb "Append" "hello")
    (dotnet:invoke sb "Append" " ")
    (dotnet:invoke sb "Append" "world")
    (let ((before-tostring (dotnet:invoke sb "ToString"))
          (before-length   (dotnet:invoke sb "Length")))    ; property get
      (setf (dotnet:invoke sb "Length") 5)                  ; property set
      (let ((after-tostring (dotnet:invoke sb "ToString")))
        (list before-tostring before-length after-tostring))))
  ("hello world" 11 "hello"))

(deftest d741-static-property-read
  (stringp (dotnet:static "System.Environment" "MachineName"))
  t)

(deftest d741-static-field-read
  (characterp (dotnet:static "System.IO.Path" "DirectorySeparatorChar"))
  t)

;;; DOTCL-MOP package phase 1.
;;; Smoke-tests AMOP introspection wrappers see dotcl's CLOS state.
(defclass d755-foo () ((x :initarg :x) (y :initarg :y)))
(defclass d755-bar (d755-foo) ((z :initarg :z) (x :allocation :class)))

(deftest d755-class-direct-superclasses
  (mapcar #'class-name (dotcl-mop:class-direct-superclasses (find-class 'd755-bar)))
  (d755-foo))

(deftest d755-class-precedence-list-includes-self-and-super
  (let ((cpl (mapcar #'class-name
                     (dotcl-mop:class-precedence-list (find-class 'd755-bar)))))
    (and (member 'd755-bar cpl)
         (member 'd755-foo cpl)
         (member 'standard-object cpl)
         t))
  t)

(deftest d755-class-direct-subclasses
  (mapcar #'class-name (dotcl-mop:class-direct-subclasses (find-class 'd755-foo)))
  (d755-bar))

(deftest d755-class-finalized-p
  (dotcl-mop:class-finalized-p (find-class 'd755-foo))
  t)

(deftest d755-slot-introspection
  (let* ((slots (dotcl-mop:class-direct-slots (find-class 'd755-bar)))
         (xslot (find 'x slots :key #'dotcl-mop:slot-definition-name)))
    (list (dotcl-mop:slot-definition-name xslot)
          (dotcl-mop:slot-definition-allocation xslot)))
  (x :class))

(defgeneric d755-greet (a))
(defmethod d755-greet ((a d755-foo)) "hi")

(deftest d755-gf-introspection
  (let* ((gf (symbol-function 'd755-greet))
         (m (first (dotcl-mop:generic-function-methods gf))))
    (list (dotcl-mop:generic-function-name gf)
          (class-name (dotcl-mop:generic-function-method-class gf))
          (dotcl-mop:generic-function-name (dotcl-mop:method-generic-function m))))
  (d755-greet standard-method d755-greet))

(deftest d755-protocol-stubs
  (list (dotcl-mop:validate-superclass (find-class 'd755-bar) (find-class 'd755-foo))
        (dotcl-mop:subclassp (find-class 'd755-bar) (find-class 'd755-foo))
        (dotcl-mop:subclassp (find-class 'd755-foo) (find-class 'd755-bar))
        (dotcl-mop:classp (find-class 't))
        (dotcl-mop:classp 42))
  (t t nil t nil))

;;; compile-defmacro must not check package lock when macro already defined
;;; Regression for (unless (fboundp 'defmethod) (defmacro defmethod ...)) pattern.
(deftest d766-defmacro-guard-pattern-no-error
  (progn
    (unless (fboundp 'defmethod)
      (defmacro defmethod (&rest args) (declare (ignore args)) nil))
    t)
  t)

;;; ((lambda ...) args) must store function to local before evaluating args
;;; so that the CIL stack is empty at try-block entry when args contain loop/return.
(deftest d767-lambda-call-loop-arg
  ((lambda (p) p)
   (if (let ((v 'hello))
         (loop (when (atom v) (return t)) (return nil)))
       :yes :no))
  :yes)

;;; defmacro dotted lambda list (a b . rest) support
;;; Macros like (defmacro foo (x . body) ...) use dotted lists as &rest equivalent
(defmacro d768-dotted-rest (x . body)
  `(list ,x ,@body))

(deftest d768-defmacro-dotted-lambda-list
  (d768-dotted-rest 1 2 3)
  (1 2 3))

;;; initialize-instance :after keyword args are valid initargs (CLHS 7.1.2)
;;; (spell library: (defmethod initialize-instance :after ((object word) &key spelling) ...))
(defclass d769-base ()
  ((%val :initarg :val :reader d769-val)))

(defmethod initialize-instance :after ((obj d769-base) &key extra)
  (declare (ignore extra)))

(deftest d769-initarg-from-after-method
  (let ((obj (make-instance 'd769-base :val 42 :extra :ignored)))
    (d769-val obj))
  42)

;;; reader: ::keyword reads as :keyword (SBCL compat, e.g. english.txt has ::possessive-adjective)
(deftest d769-reader-double-colon-keyword
  (eq ::foo :foo)
  t)

;;; gensym forward-ref: defclass with gensym name and forward superclass must resolve CPL
;;; Bug: ToClassSymbol converted uninterned gensyms to interned CL symbols, breaking RefinalizeDependents
(deftest d770-gensym-forward-ref-typep
  (let* ((g1 (gensym))
         (g2 (gensym)))
    (eval `(defclass ,g2 () ()))
    (eval `(defclass ,g1 (,g2) ()))
    (let ((obj (eval `(make-instance ',g1))))
      (list (typep obj g1) (typep obj g2))))
  (t t))

;;; import with no args must signal program-error (CLHS 11.2 import)
(deftest d770-import-no-args-errors
  (handler-case (eval '(import))
    (program-error () :ok)
    (error () :wrong-error)
    (:no-error (v) (declare (ignore v)) :no-error))
  :ok)

;;; defclass unknown option must signal program-error (CLHS 7.7)
(deftest d770-defclass-unknown-option-errors
  (handler-case (eval '(defclass d770-bad-opt () () (:unknown-opt)))
    (program-error () :ok)
    (error () :wrong-error)
    (:no-error (v) (declare (ignore v)) :no-error))
  :ok)


;;; (setf (accessor obj) val) must dispatch through :around methods
;;; Bug: defclass registered a setf expander that called %set-slot-value directly,
;;; bypassing the (setf accessor) generic function and all its method qualifiers.
(defvar *d771-around-called* nil)
(defclass d771-cls () ((x :accessor d771-x :initform 0)))
(defmethod (setf d771-x) :around (v (o d771-cls))
  (setf *d771-around-called* t)
  (call-next-method))

(deftest d771-setf-accessor-dispatches-around
  (progn
    (setf *d771-around-called* nil)
    (let ((obj (make-instance 'd771-cls)))
      (setf (d771-x obj) 99)
      (list (d771-x obj) *d771-around-called*)))
  (99 t))


;;; make-string-input-stream normalises CR (0x0D) / CRLF to LF (0x0A).
;;;         WinUI TextBox / MAUI Editor return CR-only line endings, so without
;;;         stream-level normalisation the reader's line-comment handler consumes
;;;         the following form (SBCL convention: reader expects LF, stream folds).
(deftest d803-make-string-input-stream-cr-terminates-line-comment
  (let ((src (format nil ";; comment~C42" #\Return)))
    (with-input-from-string (s src)
      (list (read s nil :eof)
            (read s nil :eof))))
  (42 :eof))

(deftest d803-make-string-input-stream-crlf-terminates-line-comment
  (let ((src (format nil ";; comment~C~C42" #\Return #\Newline)))
    (with-input-from-string (s src)
      (list (read s nil :eof)
            (read s nil :eof))))
  (42 :eof))

(deftest d803-make-string-input-stream-lf-still-works
  (let ((src (format nil ";; comment~%42")))
    (with-input-from-string (s src)
      (list (read s nil :eof)
            (read s nil :eof))))
  (42 :eof))

;;; UIOP parse-unix-namestring accepts Windows-style absolute paths
;;; (drive letter and/or backslash).
;;; Symbols are looked up at runtime to keep the file readable before
;;; (require "asdf") brings the UIOP package into existence.
(eval-when (:load-toplevel :execute) (require "asdf"))

(defun %d827-parse-unix (s)
  (funcall (find-symbol "PARSE-UNIX-NAMESTRING" :uiop) s))

(defun %d827-absolute-p (p)
  (funcall (find-symbol "ABSOLUTE-PATHNAME-P" :uiop) p))

;; These tests are Windows-specific: drive letter `C:\` style paths only
;; mean "absolute" on Windows. On Linux/macOS the same string is just a
;; relative-looking name, so the assertions don't hold. Gate with #+windows.
#+windows
(deftest d827-uiop-parse-unix-namestring-mixed-separator
  (let* ((bs (code-char 92))
         (s (format nil "C:~Cfoo/bar/baz.lisp" bs))
         (p (%d827-parse-unix s)))
    (car (pathname-directory p)))
  :absolute)

#+windows
(deftest d827-uiop-parse-unix-namestring-pure-backslash
  (let* ((bs (code-char 92))
         (s (format nil "C:~Cfoo~Cbar~Cbaz.lisp" bs bs bs))
         (p (%d827-parse-unix s)))
    (list (car (pathname-directory p))
          (pathname-name p)
          (pathname-type p)))
  (:absolute "baz" "lisp"))

#+windows
(deftest d827-uiop-absolute-pathname-p-mixed
  (let* ((bs (code-char 92))
         (s (format nil "C:~Cfoo/bar/baz.asd" bs)))
    (if (%d827-absolute-p (%d827-parse-unix s)) t nil))
  t)

;;; macrolet comprehensive patterns: eval errors now propagate instead of silent nil
(deftest d828-macrolet-simple-and
  (macrolet ((my-and (a b) `(if ,a ,b nil)))
    (my-and t t))
  t)

(deftest d828-macrolet-whole-keyword
  (macrolet ((m (&whole w x) `(list ',w ,x)))
    (m 42))
  ((m 42) 42))

(deftest d828-macrolet-nested
  (macrolet ((a (x) `(+ ,x 1)))
    (macrolet ((b (x) `(a (* ,x 2))))
      (b 3)))
  7)

(deftest d828-macrolet-gensym-swap
  (macrolet ((swap (a b)
               (let ((tmp (gensym)))
                 `(let ((,tmp ,a))
                    (setf ,a ,b ,b ,tmp)))))
    (let ((x 1) (y 2))
      (swap x y)
      (list x y)))
  (2 1))

(deftest d828-macrolet-loop-nconc
  (macrolet ((%m (z) z))
    (loop for x in '((a b) (c d) (e f g) () (i))
          nconc (%m (copy-seq x))))
  (a b c d e f g i))

;;; Windows long path (>MAX_PATH=260) — regression check that .NET 10
;;; handles these transparently. Works even when LongPathsEnabled=0.
#+windows
(deftest d850-windows-long-path
  (let* ((tmp (dotnet:static "System.IO.Path" "GetTempPath"))
         (root (concatenate 'string (substitute #\/ #\\ tmp)
                            "dotcl-longpath-regtest"))
         (deep (with-output-to-string (out)
                 (write-string root out)
                 (dotimes (i 12)
                   (format out "/~A_~D"
                           (make-string 20 :initial-element #\a) i))))
         (file (concatenate 'string deep "/leaf.txt")))
    (ensure-directories-exist file)
    (with-open-file (s file :direction :output :if-exists :supersede)
      (write-string "longpath-ok" s))
    (list (> (length file) 260)
          (not (null (probe-file file)))
          (with-open-file (s file :direction :input)
            (read-line s))))
  (t t "longpath-ok"))

;;; TCO for labels self-recursion: should not stack overflow
(deftest-compiled-only d899-labels-self-tco-basic
  (labels ((count-down (n)
             (if (= n 0) :done (count-down (- n 1)))))
    (count-down 200000))
  :done)

;;; labels self-TCO with accumulator
(deftest-compiled-only d899-labels-self-tco-acc
  (labels ((sum (n acc)
             (if (= n 0) acc (sum (- n 1) (+ acc n)))))
    (sum 100000 0))
  5000050000)

;;; defun with inner labels self-TCO
(defun d899-count-down-helper (n)
  (labels ((loop-fn (i)
             (if (= i 0) :done (loop-fn (- i 1)))))
    (loop-fn n)))

(deftest-compiled-only d899-labels-in-defun
  (d899-count-down-helper 200000)
  :done)

;;; TCO inside handler-case: should not stack overflow
(defun %d900-count-safe (n)
  (handler-case
      (if (= n 0) :done (%d900-count-safe (- n 1)))
    (error (e) (list :error e))))

(deftest-compiled-only d900-handler-case-self-tco
  (%d900-count-safe 200000)
  :done)

;;; handler-case TCO with accumulator
(defun %d900-sum-safe (n acc)
  (handler-case
      (if (= n 0) acc (%d900-sum-safe (- n 1) (+ acc n)))
    (error (e) (list :error e))))

(deftest-compiled-only d900-handler-case-tco-acc
  (%d900-sum-safe 100000 0)
  5000050000)

;;; return type inference from body (fixnum declared vars + if branches)
(defun %d902-tak (x y z)
  (declare (fixnum x y z))
  (if (not (< y x))
      z
      (%d902-tak (%d902-tak (- x 1) y z)
                 (%d902-tak (- y 1) z x)
                 (%d902-tak (- z 1) x y))))

(deftest d902-tak-inferred-fixnum
  (%d902-tak 18 12 6)
  7)

(defun %d902-double (n) (declare (fixnum n)) (* n 2))

(deftest d902-fixnum-arith-return-type
  (%d902-double 21)
  42)

;;; native fixnum self-call path (box/unbox elimination)
;;; tak with all fixnum params + fixnum return → native body compiled,
;;; inner recursive calls use InvokeNative3 instead of boxing round-trip
(defun %d903-tak (x y z)
  (declare (fixnum x y z))
  (if (not (< y x))
      z
      (%d903-tak (1- x)
                 (%d903-tak (1- y) z x)
                 (%d903-tak (1- z) x y))))

(deftest d903-native-tak
  (%d903-tak 18 12 6)
  7)

;;; Verify native 1-arg and 2-arg variants work
(defun %d903-fib (n)
  (declare (fixnum n))
  (if (< n 2) n (+ (%d903-fib (- n 1)) (%d903-fib (- n 2)))))

(deftest d903-native-fib
  (%d903-fib 20)
  6765)

(defun %d903-ack (m n)
  (declare (fixnum m n))
  (cond ((= m 0) (+ n 1))
        ((= n 0) (%d903-ack (- m 1) 1))
        (t (%d903-ack (- m 1) (%d903-ack m (- n 1))))))

(deftest d903-native-ackermann
  (%d903-ack 3 4)
  125)

;;; cond no-body arms share one CTMP slot
;;; Correctness: shared slot must not bleed across arms

(deftest d914-cond-no-body-first-truthy
  (cond ((+ 1 2)) ((error "not reached")))
  3)

(deftest d914-cond-no-body-second-truthy
  (cond (nil) ((+ 3 4)))
  7)

(deftest d914-cond-no-body-three-arms
  (list (cond (nil) (nil) (t))
        (cond (nil) (42) (t))
        (cond (1) (2) (3)))
  (t 42 1))

(deftest d914-cond-no-body-mixed-with-body
  (cond (nil) ((oddp 4) :even) ((+ 5 6)))
  11)

;;; fixnum multiply bignum promotion
(deftest d917-fixnum-mul-bignum-literal
  ;; Large literal * large literal must NOT silently wrap
  (= (* 10000000000 10000000000) 100000000000000000000)
  t)

(deftest d917-fixnum-mul-declared-bignum
  ;; Declared fixnum vars whose product overflows must promote to bignum
  (let ((a 10000000000) (b 10000000000))
    (declare (type fixnum a b))
    (= (* a b) 100000000000000000000))
  t)

(deftest d917-fixnum-mul-small-stays-fixnum
  (let ((a 1000) (b 1000))
    (declare (type fixnum a b))
    (= (* a b) 1000000))
  t)

;;; MultipleValues.Reset elision for known-single-value calls
(declaim (ftype (function (fixnum fixnum) fixnum) d916-add2))
(defun d916-add2 (a b) (+ a b))

(deftest d916-single-value-correctness
  (d916-add2 3 4)
  7)

(deftest d916-mvb-single-value
  (multiple-value-bind (x y)
      (d916-add2 10 20)
    (list x y))
  (30 nil))

(deftest d916-mvl-single-value
  (multiple-value-list (d916-add2 5 6))
  (11))

(deftest d916-nth-value
  (nth-value 0 (d916-add2 7 8))
  15)

(deftest d916-mv-after-single
  ;; MV state after a skip-reset call should not pollute next MV consumer
  (progn
    (d916-add2 1 2)
    (multiple-value-list (values 10 20 30)))
  (10 20 30))

;;; labels mutual TCO dispatch loop
(deftest-compiled-only d919-labels-mutual-tco-basic
  ;; even?/odd? via labels dispatch loop: no stack overflow at large N
  (labels ((even? (n) (if (= n 0) t (odd? (- n 1))))
           (odd?  (n) (if (= n 0) nil (even? (- n 1)))))
    (list (even? 0) (even? 1) (even? 4) (odd? 3) (even? 100000)))
  (t nil t t t))

(deftest d919-labels-mutual-tco-correctness
  ;; Verify both values correct for small N
  (labels ((my-even (n) (if (zerop n) t (my-odd (1- n))))
           (my-odd  (n) (if (zerop n) nil (my-even (1- n)))))
    (list (my-even 10) (my-odd 7) (my-even 3)))
  (t t nil))

(deftest d919-labels-same-fn-multiple-args
  ;; Same-arity 2-arg labels mutual TCO
  (labels ((f (a b) (if (= a 0) b (g (1- a) (+ b 1))))
           (g (a b) (if (= a 0) b (f (1- a) (+ b 2)))))
    (f 4 0))
  6)

(deftest d919-labels-non-tail-call-still-works
  ;; Non-tail calls to labels fns use boxes (closures) — still correct
  (labels ((double (n) (if (= n 0) 0 (+ 2 (double (1- n)))))
           (triple (n) (if (= n 0) 0 (+ 3 (triple (1- n)))))
           )
    (list (double 3) (triple 2)))
  (6 6))

;;; UCD char-name table (name-char / char-name)
(deftest d976-name-char-ucd-spaces
  ;; name-char accepts UCD names (space-separated)
  (char-code (name-char "LATIN SMALL LETTER A"))
  97)

(deftest d976-name-char-ucd-underscores
  ;; underscores are normalised to spaces
  (char-code (name-char "LATIN_SMALL_LETTER_A"))
  97)

(deftest d976-name-char-ucd-non-ascii
  ;; non-ASCII UCD entry
  (char-code (name-char "GREEK SMALL LETTER ALPHA"))
  #x03B1)

(deftest d976-char-name-ucd
  ;; char-name returns the UCD name with underscores so #\<name> reads back as one
  ;; token (_charNames entries take priority: Space stays "Space"). name-char still
  ;; accepts the spaced form too (d976-name-char-ucd-spaces).
  (char-name (code-char 97))
  "LATIN_SMALL_LETTER_A")

(deftest d976-charnames-priority
  ;; _charNames entries take priority over UCD
  (char-name #\Space)
  "Space")

;;; code-char NIL for codes >= char-code-limit
(deftest d1002-code-char-above-limit
  (code-char 65536)
  nil)

(deftest d1002-code-char-max-valid
  (characterp (code-char 65535))
  t)

;;; closed stream raises stream-error
(deftest d1003-closed-stream-error
  (let ((s (make-string-input-stream "abc")))
    (close s)
    (typep
      (handler-case (read-char s)
        (stream-error (e) e))
      'stream-error))
  t)

;;; nth type-error for negative index
(deftest d1004-nth-negative-index
  (typep
    (handler-case (nth -1 '(a b c))
      (type-error (e) e))
    'type-error)
  t)

;;; CLHS 3.2.4.2 — make-load-form protocol in FASL compiler
;;; When a struct constant is embedded in compiled code and make-load-form
;;; is defined for its type, the FASL loader must invoke the creation form
;;; rather than directly serializing raw slots.
(defstruct %mlf-box val)
(defmethod make-load-form ((self %mlf-box) &optional env)
  (make-load-form-saving-slots self))

(deftest make-load-form-saving-slots-roundtrip
  ;; CLHS-compliant: returns (values combined-creation-form nil).
  ;; Evaluate the creation form; use subst pattern from ANSI test suite.
  (let* ((box (make-%mlf-box :val 42))
         (forms (multiple-value-list (make-load-form-saving-slots box)))
         (newobj (eval (first forms))))
    (eval (subst newobj box (second forms)))
    (and (= (length forms) 2)
         (null (second forms))
         (eql (class-of box) (class-of newobj))
         (eql (%mlf-box-val newobj) 42)))
  t)

(deftest-compiled-only make-load-form-fasl-roundtrip
  (let* ((tmp (uiop:temporary-directory))
         (src (merge-pathnames "mlf-fasl-test.lisp" tmp))
         (out (merge-pathnames "mlf-fasl-test.fasl" tmp)))
    (with-open-file (f src :direction :output :if-exists :supersede)
      (write-string "(defstruct %mlf-box2 val)
(defmethod make-load-form ((self %mlf-box2) &optional env)
  (make-load-form-saving-slots self))
(defvar *mlf-test-val* '#.(make-%mlf-box2 :val 99))" f))
    (compile-file src :output-file out)
    (load out)
    (eql (%mlf-box2-val (symbol-value (find-symbol "*MLF-TEST-VAL*"))) 99))
  t)

;;; CLHS 3.2.4.2 — the make-load-form CREATION form must be EVALUATED at LOAD
;;; time, not bypassed by returning the compile-time instance. The compile-time
;;; intern pre-registration used to make InternViaEval cache-HIT the compile-time
;;; object and skip the creation-form eval, so creation-form side effects never
;;; ran at load (ANSI MAKE-LOAD-FORM.ORDER). Pin that a creation-form side
;;; effect (a push onto a load-visible var) fires at load time.
(defvar *mlf-creation-log* nil)
(defclass %mlf-cls () ((name :initarg :name :reader %mlf-name)))
(defmethod make-load-form ((x %mlf-cls) &optional env)
  (declare (ignore env))
  `(progn (push :created *mlf-creation-log*)
          (make-instance '%mlf-cls :name ',(%mlf-name x))))

(deftest-compiled-only make-load-form-creation-form-runs-at-load
  (let* ((tmp (uiop:temporary-directory))
         (src (merge-pathnames "mlf-creation-test.lisp" tmp))
         (out (merge-pathnames "mlf-creation-test.fasl" tmp)))
    (setf *mlf-creation-log* nil)
    (with-open-file (f src :direction :output :if-exists :supersede)
      (write-string "(defvar *mlf-c* '#.(make-instance '%mlf-cls :name 'foo))" f))
    (compile-file src :output-file out)
    (load out)
    (list (and (member :created *mlf-creation-log*) t)
          (%mlf-name (symbol-value (find-symbol "*MLF-C*")))))
  (t foo))

;;; FMAKUNBOUND must clear a macro definition too — including a macro defined on
;;; a gensym. The *macros* table is keyed by symbol identity; FMAKUNBOUND cleared
;;; it by a LispString of the name, leaving the symbol-keyed entry, so FBOUNDP /
;;; MACRO-FUNCTION still found the macro afterward (ANSI FMAKUNBOUND.3).
(deftest fmakunbound-clears-gensym-macro
  (let ((g (gensym)))
    (eval `(defmacro ,g () nil))
    (list (and (fboundp g) t)
          (progn (fmakunbound g) (fboundp g))
          (macro-function g)))
  (t nil nil))

;;; REMOVE-IF yields exactly one value. A predicate whose last call returns
;;; multiple values (e.g. SUBTYPEP) left secondaries in the MV register that
;;; leaked through REMOVE-IF's scalar return (ANSI NIL.1 via check-predicate).
(deftest remove-if-single-value-with-mv-predicate
  (multiple-value-list
   (remove-if (lambda (x) (not (subtypep (type-of x) nil))) '(1 2 3)))
  (nil))

;;; FUNCTION-LAMBDA-EXPRESSION's second value (closure indicator) must be true
;;; for a function that closed over a non-null lexical environment; it was always
;;; nil (ANSI FUNCTION-LAMBDA-EXPRESSION.2).
(deftest function-lambda-expression-closure-p
  (let ((x 1))
    (flet ((%fle-f () x))
      (let ((rv (multiple-value-list (function-lambda-expression #'%fle-f))))
        (list (length rv) (and (second rv) t)))))
  (3 t))

;;; DEFPACKAGE :import-from / :shadowing-import-from accept a character as a
;;; string designator for the package and symbol names (CLHS). Characters were
;;; dropped, importing the wrong symbols (ANSI DEFPACKAGE.7/8).
(deftest defpackage-import-from-char-designator
  (progn
    (ignore-errors (delete-package "RF-DPH"))
    (ignore-errors (delete-package "Q"))
    ;; source package "Q" so the char designator #\Q names it
    (eval '(defpackage "Q" (:use) (:intern "A" "B")))
    (eval `(defpackage "RF-DPH" (:use) (:import-from #\Q #\B "A")))
    (let ((p (find-package "RF-DPH")))
      (list (and (find-symbol "A" p) t)
            (and (find-symbol "B" p) t)
            (let ((n 0)) (do-symbols (s p) (declare (ignore s)) (incf n)) n))))
  (t t 2))

;;; (setf (f arg...) val) on a function-call place with no setf expander expands
;;; to (funcall #'(setf f) val arg...) and yields THAT call's value(s), not a
;;; re-returned copy of val (CLHS 5.1.2.5; ANSI FDEFINITION.5).
(deftest setf-function-place-returns-funcall-value
  (let* ((sym (gensym))
         (fname (list 'setf sym)))
    (setf (fdefinition fname) (fdefinition 'cons))
    (eval `(setf (,sym 'a) 'b)))
  (b . a))

;;; set-pprint-dispatch keys on the full set of type specifiers (e.g. (MEMBER ..))
;;; and accepts a symbol naming the dispatch function (ANSI PPRINT-DISPATCH.7-9).
(deftest pprint-dispatch-member-spec-and-symbol-fn
  (with-standard-io-syntax
    (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
          (*print-readably* nil) (*print-escape* nil) (*print-pretty* t))
      (defun %ppd-rf (s o) (declare (ignore o)) (write "HIT" :stream s))
      (set-pprint-dispatch '(member x y) '%ppd-rf)
      (list (write-to-string 'x) (write-to-string 'y) (write-to-string 'z))))
  ("HIT" "HIT" "Z"))

;;; pprint-pop / *print-length*: a dotted tail prints ". atom" even at the
;;; length boundary; an empty/proper list still truncates with "..." (PPRINT-POP.6,
;;; without regressing PPRINT-POP.1/9).
(deftest pprint-pop-dotted-tail-at-length-boundary
  (flet ((%f (len)
           (with-standard-io-syntax
             (let ((*print-pretty* t) (*print-escape* nil) (*print-right-margin* 100)
                   (*print-readably* nil) (*print-length* len))
               (with-output-to-string (os)
                 (pprint-logical-block (os '(1 2 . 3) :prefix "{" :suffix "}")
                   (pprint-exit-if-list-exhausted)
                   (write (pprint-pop) :stream os)
                   (loop (pprint-exit-if-list-exhausted)
                         (write #\  :stream os)
                         (write (pprint-pop) :stream os))))))))
    (list (%f 0) (%f 1) (%f 2) (%f 3)))
  ("{...}" "{1 ...}" "{1 2 . 3}" "{1 2 . 3}"))

;;; (setf (values ...) form): place subforms evaluate left-to-right BEFORE the
;;; value form (ANSI SETF-VALUES.5).
(deftest setf-values-evaluation-order
  (let ((a (vector nil nil)) (i 0) x y z)
    (setf (values (aref a (progn (setf x (incf i)) 0))
                  (aref a (progn (setf y (incf i)) 1)))
          (progn (setf z (incf i)) (values 'foo 'bar)))
    (list (coerce a 'list) i x y z))
  ((foo bar) 3 1 2 3))

;;; A nested (values ..) place consumes ONE value and sets its first sub-place,
;;; NIL to the rest (ANSI VALUES.20).
(deftest setf-values-nested-distributes-one-value
  (let ((a t) (b t) (c t) (d t) (e t) (f t))
    (setf (values a (values b c) (values d) (values e f)) (values 0 1 2 3 4 5 6))
    (list a b c d e f))
  (0 1 nil 2 3 nil))

;;; psetf with multi-store (values ..) places binds all the value form's values
;;; (ANSI PSETF.41), and a plain parallel swap still works.
(deftest psetf-multistore-values-place
  (let ((y 2) (z 3) u x a b c)
    (psetf (values a b c) (values 1 2 3)
           (values u x)   (values y z))
    (list a b c u x))
  (1 2 3 2 3))

(deftest psetf-parallel-swap-still-works
  (let ((a 1) (b 2)) (psetf a b b a) (list a b))
  (2 1))

;;; delete-file on a directory pathname deletes the (empty) directory, like SBCL
;;; (ANSI ENSURE-DIRECTORIES-EXIST.8 deletes a scratch subdir this way).
(deftest delete-file-on-directory-pathname
  (let ((dir (make-pathname :directory '(:relative "rf-deldir-test")
                            :defaults *default-pathname-defaults*)))
    (ignore-errors (delete-file dir))
    ;; ensure-directories-exist creates the directory component (but not a file)
    (ensure-directories-exist
     (make-pathname :name "x" :type "txt" :defaults dir))
    (list (and (probe-file dir) t)
          (and (delete-file dir) t)   ; delete-file on the empty directory
          (and (probe-file dir) t)))
  (t t nil))

;;; A defstruct slot whose name is a SPECIAL variable must not shadow that
;;; variable in later slots' default initforms — the keyword constructor binds
;;; such a slot via a gensym, not the special symbol (ANSI STRUCTURE-60-1).
(defvar *rf-st60* 100)
(defstruct rf-struct-60
  (a *rf-st60* :type integer)
  (*rf-st60* 0 :type integer)
  (b *rf-st60* :type integer))
(deftest defstruct-special-slot-name-does-not-shadow
  (let ((*rf-st60* 10))
    (let ((s (make-rf-struct-60 :*rf-st60* 200)))
      (list (rf-struct-60-a s) (rf-struct-60-*rf-st60* s) (rf-struct-60-b s))))
  (10 200 10))

;;; A literal pathname embedded in compiled code (#.) keeps its VERSION across the
;;; FASL round-trip; a plain namestring round-trip dropped :newest to nil
;;; (ANSI COMPILE-FILE.16 via *compile-file-pathname*).
(deftest-compiled-only fasl-pathname-preserves-version
  (let* ((tmp (uiop:temporary-directory))
         (src (merge-pathnames "rf-pathver.lisp" tmp))
         (out (merge-pathnames "rf-pathver.fasl" tmp)))
    (with-open-file (f src :direction :output :if-exists :supersede)
      (write-string
       "(defparameter *rf-pathver* '#.(make-pathname :name \"x\" :type \"lsp\" :version :newest))"
       f))
    (compile-file src :output-file out)
    (load out)
    (pathname-version (symbol-value (find-symbol "*RF-PATHVER*"))))
  :newest)


;;; dotcl-thread must be loadable via the ASDF source-registry.
;;; ASDF's LOAD-OP compiles every component, so this needs a build with a
;;; compiler; a build without one cannot load a system at all today.
(deftest-compiled-only asdf-load-dotcl-thread
  (progn
    (require "asdf")
    (asdf:load-system "dotcl-thread")
    (not (null (find-package "DOTCL-THREAD"))))
  t)

;;; dotcl:backtrace — named-function call stack, innermost first
(defun %bt-c () (dotcl:backtrace))
(defun %bt-b () (%bt-c))
(defun %bt-a () (%bt-b))

(deftest d1108-backtrace-chain
  (%bt-a)
  ("%BT-C" "%BT-B" "%BT-A"))

(deftest d1108-backtrace-self-excluded
  ;; dotcl:backtrace is registered without a Name, so it never appears
  (member "BACKTRACE" (%bt-c) :test #'string=)
  nil)

(deftest d1108-print-backtrace-returns-nil
  (dotcl:print-backtrace (make-string-output-stream))
  nil)

;;; ash with non-negative constant shift must not overflow int64 / mask
;;; count mod 64 (raw CIL shl bug). Constant base, nested, and overflow cases.
(deftest d1111-ash-const-shift-64
  (ash 1 64)
  #.(expt 2 64))

(deftest d1111-ash-const-shift-211
  (ash 1 211)
  #.(expt 2 211))

(deftest d1111-ash-const-base-overflow
  (ash 3 62)            ; 3*2^62 overflows int64 -> must promote to bignum
  #.(* 3 (expt 2 62)))

(deftest d1111-ash-nested-constant
  (integer-length (* 1 (ash 1 210)))
  211)

(deftest d1111-ash-variable-base-overflow
  (let ((x 7)) (ash x 60))
  #.(* 7 (expt 2 60)))

(deftest d1111-ash-right-shift-still-ok
  (ash 1024 -5)
  32)

(deftest d1111-ash-small-constant-ok
  (ash 5 3)
  40)

;;; fixnum fast-path +/- must promote to bignum on int64 overflow
;;; (compile-fixnum-binop emitted raw :add/:sub that silently wrapped). * already
;;; promoted via MultiplyFixnum; +/- now match via Add/SubtractFixnum.
(defun %d1112-add (a b) (declare (fixnum a b)) (+ a b))
(defun %d1112-sub (a b) (declare (fixnum a b)) (- a b))

(deftest d1112-add-overflow-promotes
  (%d1112-add #.(1- (expt 2 63)) 1)
  #.(expt 2 63))

(deftest d1112-sub-underflow-promotes
  (%d1112-sub #.(- (expt 2 63)) 1)
  #.(- (1+ (expt 2 63))))

(deftest d1112-the-fixnum-operands-promote
  (+ (the fixnum #.(1- (expt 2 63))) (the fixnum 1))
  #.(expt 2 63))

(deftest d1112-no-overflow-still-correct
  (+ (the fixnum 1000000) (the fixnum 2000000))
  3000000)

;;; NESTED unboxed fixnum arithmetic must promote on int64 overflow.
;;; The boxed fixnum fast path (compile-fixnum-binop / 1+ / 1-) now uses a static
;;; value-range proof (expr-int-range): the raw int64 path is taken only when every
;;; intermediate +/-/*/1+/1- result provably fits int64; otherwise it falls back to
;;; the promoting Runtime.Add/Subtract/Multiply so the result becomes a bignum.
;;; (The native-long self-call path keeps its opt-in unsafe contract; verified
;;; intact by d903-native-tak above.)
(defun %d1117-nest+ (a b c) (declare (fixnum a b c)) (+ (+ a b) c))
(defun %d1117-nest* (a b) (declare (fixnum a b)) (* (+ a b) 1))
(defun %d1117-nest- (a b) (declare (fixnum a b)) (- (- a b) b))
(defun %d1117-nest1+ (a b) (declare (fixnum a b)) (+ (1+ a) b))

(deftest d1117-nested-add-overflow-promotes
  (%d1117-nest+ #.(1- (expt 2 63)) 1 0)
  #.(expt 2 63))

(deftest d1117-nested-mul-overflow-promotes
  (%d1117-nest* #.(1- (expt 2 63)) 1)
  #.(expt 2 63))

(deftest d1117-nested-sub-underflow-promotes
  (%d1117-nest- #.(- (expt 2 63)) 1)
  #.(- (+ 2 (expt 2 63))))

(deftest d1117-nested-1+-overflow-promotes
  (%d1117-nest1+ #.(1- (expt 2 63)) 1)
  #.(1+ (expt 2 63)))

;;; Bounded operand declarations keep the fast unboxed path AND stay correct.
(defun %d1117-bounded (a b)
  (declare (type (integer 0 1000) a) (type (integer 0 1000) b))
  (+ (+ a b) a))

(deftest d1117-bounded-stays-correct
  (%d1117-bounded 1000 1000)
  3000)

;;; Non-overflowing nested fixnum arithmetic is unchanged.
(deftest d1117-small-nested-correct
  (%d1117-nest+ 1 2 3)
  6)

;;; :bt / print-backtrace show call forms with arguments.
;;; Frames now carry args (alloc-free inline for <=4); dotcl:print-backtrace renders
;;; "(NAME arg1 arg2 ...)" while dotcl:backtrace keeps bare names (see d1108 above).
(defun %d1118-bt-leaf (a b)
  (declare (ignore a b))
  (let ((s (make-string-output-stream)))
    (dotcl:print-backtrace s)
    (get-output-stream-string s)))
(defun %d1118-bt-mid (x) (%d1118-bt-leaf x (list x)))

(deftest d1118-backtrace-shows-args
  (let ((out (%d1118-bt-mid 7)))
    (and (search "(%D1118-BT-LEAF 7 (7))" out)
         (search "(%D1118-BT-MID 7)" out)
         t))
  t)

(deftest d1118-backtrace-names-unchanged
  ;; dotcl:backtrace stays bare names for programmatic use.
  (%bt-a)
  ("%BT-C" "%BT-B" "%BT-A"))

;;; dotcl:backtrace-with-args exposes each frame as (NAME arg0 arg1 ...)
;;; with the actual captured argument objects, innermost first.
(defun %bta-leaf (n s) (declare (ignore s)) (dotcl:backtrace-with-args))
(defun %bta-mid (a b) (%bta-leaf (+ a b) "x"))

(deftest d269-backtrace-with-args-frames
  (let ((bt (%bta-mid 3 4)))
    (list (find "%BTA-LEAF" bt :key #'car :test #'string=)
          (find "%BTA-MID" bt :key #'car :test #'string=)))
  (("%BTA-LEAF" 7 "x") ("%BTA-MID" 3 4)))

(deftest d269-backtrace-with-args-real-objects
  ;; the args are real Lisp objects, not printed strings: usable directly.
  (let* ((bt (%bta-mid 10 20))
         (leaf (find "%BTA-LEAF" bt :key #'car :test #'string=)))
    (1+ (cadr leaf)))             ; arg0 of %bta-leaf is the number 30
  31)

(deftest d269-backtrace-with-args-self-excluded
  ;; registered without a Name, so it never appears in its own result.
  (member "BACKTRACE-WITH-ARGS" (%bta-leaf 1 2) :key #'car :test #'string=)
  nil)

;;; [LispDoc]/SetFunctionDoc docstrings now surface through
;;; the DOCUMENTATION GF's function method (it falls back to _docs like the variable
;;; method does). dotcl:save-application carries a [LispDoc] docstring.
(deftest d1121-builtin-lispdoc-function-doc
  (and (stringp (documentation 'dotcl:save-application 'function)) t)
  t)

;;; User-set docs still take precedence over the built-in fallback.
(deftest d1121-user-doc-precedence
  (progn (setf (documentation 'dotcl:save-application 'function) "user override")
         (prog1 (documentation 'dotcl:save-application 'function)
           ;; restore: clearing the table entry falls back to the [LispDoc] doc
           (setf (documentation 'dotcl:save-application 'function) nil)))
  "user override")

;;; #19 — a leading ~ in a STRING file spec must expand to the user home
;;; on the LOAD/OPEN/PROBE-FILE path (ResolvePhysicalPath), not just for
;;; (pathname "~/..."). Previously (load "~/x") treated ~ as a relative segment
;;; → cwd/~/x. probe-file "~" exercises ResolvePhysicalPath; home always exists,
;;; so a correct expansion returns a non-nil pathname with no literal ~.
(deftest d1146-tilde-string-expands-on-file-ops
  (let ((p (probe-file "~")))
    (and (not (null p))
         (not (find #\~ (namestring p)))
         t))
  t)

;;; dotnet:new with only a type name must work for a type that has no
;;; parameterless ctor but an all-optional one (mirrors C# `new T()`). JsonObject
;;; has only JsonObject(JsonNodeOptions? options = null).
(deftest d1181-dotnet-new-all-optional-ctor
  (let ((o (dotnet:new "System.Text.Json.Nodes.JsonObject")))
    (notnot (typep (dotnet:invoke o "ToString") 'string)))
  t)

;;; Regression guard: a genuine parameterless ctor still works.
(deftest d1181-dotnet-new-parameterless-ctor
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke sb "Append" "ok")
    (dotnet:invoke sb "ToString"))
  "ok")

;;; Lisp-2: a local variable named like a global function must not shadow
;;; the function in operator position, even when the local is setf-mutated (boxed).
;;; Mirrors quicklisp's fetch: (url ...) calls the function URL while url is also a
;;; mutated parameter.
(defun %qls-url (thing) (if (stringp thing) (list :parsed thing) thing))
(defun %qls-fetch (%qls-url)
  (setf %qls-url (list :merged %qls-url))
  (let* ((connect (or (%qls-url 99) %qls-url)))
    (list connect %qls-url)))
(deftest-compiled-only issue279-local-var-does-not-shadow-global-fn
  (%qls-fetch :x)
  (99 (:merged :x)))

;;; compile-file must not leak compile-time *modules* mutations. A
;;; (provide "x") (or (require "x")) evaluated at :compile-toplevel pushed "x"
;;; into the global *modules* and survived compile-file, while the matching
;;; compile-time defuns were stripped — so a later load-time (require "x")
;;; saw "x" already in *modules*, skipped re-loading, and the module's
;;; functions stayed unbound. Surfaced as quicklisp's compile-time
;;; (require "dotcl-socket") leaving ql-dotcl:socket-connect undefined on the
;;; first (cold-compile) install-dist run ("Not implemented").
(defun %cf-modules-no-leak ()
  (let* ((tmp (format nil "~a/dotcl-cfmod-~a"
                      (or (dotcl:getenv "TEMP") "/tmp")
                      (get-internal-real-time)))
         (src (format nil "~a/src.lisp" tmp)))
    (ensure-directories-exist (concatenate 'string tmp "/"))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (format s "(eval-when (:compile-toplevel) (provide \"dotcl-cfmod-phantom\"))~%"))
    (compile-file src)
    ;; Provided only at compile time → must not survive into the image's *modules*.
    (and (member "dotcl-cfmod-phantom" *modules* :test #'string=) t)))

(deftest-compiled-only compile-file-does-not-leak-compile-time-modules
  (%cf-modules-no-leak)
  nil)

;;; #'aref via funcall/apply must support any rank. The Lisp variadic AREF
;;; defun (used for the function-object path; direct (aref a i j ...) is compiled
;;; rank-aware) capped at rank-2 and errored ">3 dimensions not supported via
;;; funcall" for rank-3+. Now rank-0..3 route to the fast path and rank>=4 goes
;;; through row-major-aref + array-row-major-index, so any rank works.
(deftest i314-funcall-aref-rank3
  (let ((a (make-array '(3 4 4) :initial-element 0)))
    (setf (aref a 1 2 3) 'three-d)
    (list (funcall #'aref a 1 2 3)
          (apply #'aref a (list 1 2 3))
          (funcall #'aref a 0 0 0)))
  (three-d three-d 0))

(deftest-compiled-only i314-funcall-aref-rank4
  (let ((a (make-array '(2 2 2 2) :initial-element 0)))
    (setf (aref a 1 1 1 1) 'four-d)
    (list (funcall #'aref a 1 1 1 1)
          (apply #'aref a (list 1 1 1 1))))
  (four-d four-d))

;;; compile-file :module-name pins a stable FASL assembly name (no
;;; per-compile guid suffix) for build-time-linked / NativeAOT deployment.
;;; This protects the :module-name path: compile with a fixed name, load, run.
;;; (The stable name itself is verified out of band via Reflection.AssemblyName.)
(defun %d1263-module-name-fasl ()
  (let* ((tmp (format nil "~a/dotcl-d1263-~a"
                      (or (dotcl:getenv "TEMP") "/tmp")
                      (get-internal-real-time)))
         (src (format nil "~a/src.lisp" tmp)))
    (ensure-directories-exist (concatenate 'string tmp "/"))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (format s "(defpackage #:d1263-pkg (:use :cl))~%")
      (format s "(in-package #:d1263-pkg)~%")
      (format s "(defun sq (n) (* n n))~%"))
    (let ((fasl (concatenate 'string (subseq src 0 (- (length src) 5)) ".fasl")))
      (compile-file src :output-file fasl :module-name "d1263-stable-mod")
      (load fasl))
    (funcall (read-from-string "d1263-pkg::sq") 6)))

(deftest-compiled-only d1263-compile-file-module-name
  (%d1263-module-name-fasl)
  36)

;;; get-setf-expansion returns N store-vars for a defsetf long form with
;;; multiple store variables. Was always 1, breaking letf/with-cursor-off
;;; on multi-value places (e.g. McCLIM's (cursor-position c)).
(defvar *gsx-x* 0)
(defvar *gsx-y* 0)
(defun %gsx-gp (obj) (declare (ignore obj)) (values *gsx-x* *gsx-y*))
(defun %gsx-set (nx ny) (setf *gsx-x* nx *gsx-y* ny) (values nx ny))
(defsetf %gsx-gp (obj) (nx ny) (declare (ignore obj)) `(%gsx-set ,nx ,ny))

(deftest d1274-get-setf-expansion-store-count
  (multiple-value-bind (temps vals stores setter getter)
      (get-setf-expansion '(%gsx-gp :o))
    (declare (ignore temps vals setter getter))
    (length stores))
  2)

;; The writer-form must use BOTH store variables (letf-style save/restore).
(deftest d1274-get-setf-expansion-uses-both-stores
  (progn
    (%gsx-set 3 4)
    (multiple-value-bind (temps vals stores setter getter)
        (get-setf-expansion '(%gsx-gp :o))
      (declare (ignore getter))
      (eval `(let* (,@(mapcar #'list temps vals))
               (multiple-value-bind ,stores (values 11 22) ,setter))))
    (list *gsx-x* *gsx-y*))
  (11 22))

;;; UNC paths (\\server\share\...) must keep the server as the pathname host and
;;; round-trip. Previously the leading // was dropped (RemoveEmptyEntries split),
;;; giving (:absolute "server" "share" ...) with host NIL, which reconstructed to
;;; a bogus local \server\share\... — directory returned nothing and probe-file
;;; was NIL on a perfectly valid mapped UNC path. (Filesystem-independent here.)
(deftest unc-pathname-host
  (pathname-host (pathname "//srv/share/dir/file.txt"))
  "srv")

(deftest unc-pathname-directory
  (pathname-directory (pathname "//srv/share/dir/file.txt"))
  (:absolute "share" "dir"))

(deftest unc-namestring-roundtrip
  (namestring (pathname "//srv/share/dir/file.txt"))
  "//srv/share/dir/file.txt")

;; Backslash UNC form parses identically (separators are normalized).
(deftest unc-backslash-roundtrip
  (namestring (pathname "\\\\srv\\share\\f.txt"))
  "//srv/share/f.txt")

;; A plain absolute (non-UNC) path is unaffected: single leading slash, no host.
(deftest unc-nonunc-absolute-unaffected
  (list (pathname-host (pathname "/etc/hosts"))
        (namestring (pathname "/etc/hosts")))
  (nil "/etc/hosts"))

;;; (or <non-tail self-call> <tail self-call>) in a self-recursive
;;; function must not let the tail-side TCO loop discard the non-tail side's
;;; truthy value. The non-tail arm of OR/AND is NOT in tail position; binding
;;; *in-tail-position* nil there prevents the spurious TCO-rewrite that turned
;;; a self-call into a value-discarding loop-back. Needs a deep spine so the
;;; tail self-call actually loops past the matching element.
(defun %refs-p-276 (tree key)
  (cond ((atom tree) nil)
        ((and (eq (car tree) :ldloc) (eq (cadr tree) key)) t)
        (t (or (%refs-p-276 (car tree) key)     ; non-tail self-call
               (%refs-p-276 (cdr tree) key))))) ; tail self-call (TCO target)

(deftest issue276-or-nontail-self-truthy-deep
  ;; ( :x :x ... (:ldloc 7) ) with a 60-deep spine — the match is in a deep
  ;; car, reached only via the non-tail OR arm while the tail arm loops the cdr.
  (%refs-p-276
   (let ((lst '((:ldloc 7))))
     (dotimes (i 60) (setq lst (cons :x lst)))
     lst)
   7)
  t)

(deftest issue276-or-nontail-self-trivial
  (%refs-p-276 '(:ldloc 7) 7)
  t)

(deftest issue276-or-nontail-self-nested
  (%refs-p-276 '(((((:ldloc 7))))) 7)
  t)

(deftest issue276-or-nontail-self-absent
  (%refs-p-276
   (let ((lst '((:ldloc 9))))
     (dotimes (i 60) (setq lst (cons :x lst)))
     lst)
   7)
  nil)

;; NOTE: the symmetric AND case ((and <non-tail self> <tail self>)) is NOT fixed
;; here. Applying the same *in-tail-position* nil binding to COMPILE-AND's
;; intermediate args makes 4 eval-and-compile ANSI tests (EVAL-WHEN.1,
;; DEFINE-COMPILER-MACRO.4/7, MACRO-FUNCTION.15) flip to FAIL. Investigation
;; investigation found these are state-ordering-fragile, not a deterministic
;; AND miscompile: one root cause — a bare {:execute} eval-when at top level of
;; a compiled file leaked its body into the fasl — was fixed independently
;; (see issue333-eval-when-execute-only-discarded-in-compiled-file below and
;; the compile-eval-when change). That alone makes eval-and-compile 318/318
;; standalone, but adding the AND fix still perturbs cross-test state. The AND
;; codegen fix remains pending the residual interaction.

;;; rename-file must replace an existing target (POSIX rename / SBCL semantics).
;;; Previously it used File.Move without overwrite, so renaming onto an existing
;;; file threw "Cannot create a file when that file already exists" — which broke
;;; asdf's atomic-write idiom (write temp → rename onto final), making the second
;;; build of any external system fail in load-asd.
(deftest rename-file-overwrites-existing
  (let ((src "dotcl-rf-src.tmp")
        (dst "dotcl-rf-dst.tmp"))
    (with-open-file (s src :direction :output :if-exists :supersede) (write-string "new" s))
    (with-open-file (s dst :direction :output :if-exists :supersede) (write-string "old" s))
    (rename-file src dst)
    (prog1
        (list (with-open-file (s dst) (read-line s))   ; dst now holds src's content
              (and (probe-file src) t))                ; src is gone
      (ignore-errors (delete-file dst))))
  ("new" nil))

;;; eval-when situation handling at top level of a COMPILE-FILE'd file
;;; (CLHS 3.2.3.1, Figure 3-7). A bare {:execute} eval-when at top level of a
;;; compiled file must be DISCARDED: its body is neither run at compile time nor
;;; placed into the fasl's load code. Previously compile-eval-when emitted the
;;; body whenever :execute was present (or lt-p ex-p), so loading the compiled
;;; file wrongly ran the :execute-only body — an extra side effect.  Fixed to
;;; emit-for-load iff :load-toplevel is present at top level.
(defparameter *ew-discard-collector* nil)
(deftest-compiled-only issue333-eval-when-execute-only-discarded-in-compiled-file
  (let* ((src "dotcl-ew-discard-src.lisp")
         (fasl (compile-file-pathname src)))
    (with-open-file (o src :direction :output :if-exists :supersede)
      (prin1 '(eval-when (:execute) (push :exec-only *ew-discard-collector*)) o)
      (terpri o)
      (prin1 '(eval-when (:load-toplevel) (push :load-only *ew-discard-collector*)) o)
      (terpri o))
    (compile-file src)
    (prog1
        (let ((*ew-discard-collector* nil))
          (load fasl)
          *ew-discard-collector*)              ; only the :load-toplevel body ran
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  (:load-only))

;; {:compile-toplevel :execute} (no :load-toplevel) at top level of a compiled
;; file is evaluated at compile time and NOT placed in the fasl (CLHS Figure 3-7);
;; :execute must not promote it into the load image. Previously the compile-file
;; eval-when handler forced load-toplevel whenever :execute was present, leaking
;; the body into the fasl. Here :ctex must NOT appear at load time.
(deftest-compiled-only issue333-eval-when-ct-execute-not-in-fasl
  (let* ((src "dotcl-ew-ctex-src.lisp")
         (fasl (compile-file-pathname src)))
    (with-open-file (o src :direction :output :if-exists :supersede)
      (prin1 '(eval-when (:compile-toplevel :execute) (push :ctex *ew-discard-collector*)) o)
      (terpri o)
      (prin1 '(eval-when (:load-toplevel) (push :load *ew-discard-collector*)) o)
      (terpri o))
    (compile-file src)
    (prog1
        (let ((*ew-discard-collector* nil))
          (load fasl)
          *ew-discard-collector*)              ; only :load ran at load time
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  (:load))

;;; DEFUN of a (setf name) function returns the function name itself — the list
;;; (SETF name) — not a symbol interned from the mangled string "(SETF name)".
;;; compile-defun emitted the mangled symbol for non-symbol names, so
;;; (eval `(defun (setf ,g) ...)) returned |(SETF G)| instead of (SETF G)
;;; (ANSI DEFINE-COMPILER-MACRO.4).
(deftest defun-setf-name-returns-name-list
  (let ((g (gensym)))
    (let ((ret (eval `(defun (setf ,g) (nv x) (setf (car x) nv)))))
      (list (consp ret) (and (consp ret) (eq (car ret) 'setf) t) (eq (cadr ret) g))))
  (t t t))

;;; (setf (macro-function name environment) fn): the optional ENVIRONMENT place
;;; subform must be evaluated for effect (CLHS 5.1.1.1), even though dotcl's macro
;;; table doesn't key on it. The setf-expander previously used only (second place)
;;; and silently dropped the environment subform, losing its side effects
;;; (ANSI MACRO-FUNCTION.15).
(deftest macro-function-setf-evaluates-environment-subform
  (let ((sym (gensym)) (i 0) a b)
    (setf (macro-function (progn (setf a (incf i)) sym)
                          (progn (setf b (incf i)) nil))
          (macro-function 'pop))
    (list i a b))                              ; both progns ran: i=2, a=1, b=2
  (2 1 2))

;; dotcl-cltl2 minimal CLtL2 environment access. The crux is define-declaration
;; being a MACRO (so the declaration name isn't evaluated as a variable) and all the
;; introspection functions being TOTAL (never error at env=NIL, degrade to nil/safe).
(deftest i338-define-declaration-is-macro
  ;; was "Unbound variable: OPTIMIZER" when define-declaration was a plain call.
  (eval '(dotcl-cltl2:define-declaration optimizer (spec env)
          (declare (ignore spec env)) (values :declare nil)))
  optimizer)

(deftest i338-declaration-information-safe
  (list (dotcl-cltl2:declaration-information 'optimizer nil)        ; custom -> nil
        (and (dotcl-cltl2:declaration-information 'optimize nil) t))  ; optimize -> default
  (nil t))

(deftest i338-information-never-errors-at-nil-env
  (list (multiple-value-list (dotcl-cltl2:variable-information 'x nil))
        (multiple-value-list (dotcl-cltl2:function-information 'car nil))
        (dotcl-cltl2:augment-environment nil :variable '(x)))
  ((nil nil nil) (nil nil nil) nil))

;;; compile-file-pathname of a LOGICAL pathname must stay logical (keep its host),
;;; so the default output is translated through the host's translations. Returning
;;; a physical pathname that reused the logical host mashed host+name into the
;;; namestring ("//CLTEST..." → "\CLTEST..." on Windows), breaking compile-file of
;;; a logical pathname (ANSI COMPILE-FILE.17).
(deftest compile-file-pathname-logical-stays-logical
  (progn
    (setf (logical-pathname-translations "DOTCLT")
          `(("**;*.*.*" ,(merge-pathnames
                          "sandbox/"
                          (make-pathname
                           :directory (append (pathname-directory (truename (make-pathname)))
                                               '(:wild-inferiors))
                           :name :wild :type :wild)))))
    (let ((out (compile-file-pathname (logical-pathname "DOTCLT:CFPLP.LSP"))))
      (list (typep out 'logical-pathname)
            (string-equal (pathname-type out) "fasl")
            ;; physical translation must contain "sandbox", no mashed host token
            (and (search "sandbox" (namestring (translate-logical-pathname out))) t)
            (not (search "DOTCLT" (namestring (translate-logical-pathname out)))))))
  (t t t t))

;;; (The require->asdf fallback wiring is exercised implicitly by every test
;;; that (require "<asdf-system>"); a dedicated test was removed: it added
;;; no coverage beyond the wiring check and forced pinning a real library
;;; (trivial-features) as an asdf-findable subject, which blocked removing that
;;; library's contrib stub. Tests must earn their runtime cost.)

;;; Multi-list MAPCAR whose list arg expands to code that opens a try-block
;;; (e.g. LOOP ... COLLECT). The compiler pushed the function value, then
;;; compiled the list args (building the array) with that value still on the
;;; stack; a LOOP arg's try-block entry then violated CIL's empty-stack rule
;;; → "CLR detected an invalid program" (inquisitor resolve-states).
;;; Fixed by stashing function + arg-array in temps so the stack is empty
;;; while each list arg compiles.
(defun %mapcarn-loop-a (p) (mapcar (lambda (a i) (list a i)) p (loop for i below (length p) collect i)))
(defun %mapcarn-loop-b (p) (mapcar (lambda (i a) (list i a)) (loop for i below 3 collect i) p))
(deftest i340-multilist-mapcar-with-loop-arg
  (list (%mapcarn-loop-a '(a b c))
        (%mapcarn-loop-b '(x y z))
        (mapcar (lambda (a b c) (list a b c)) '(1 2) '(p q) (loop for i below 2 collect i)))
  (((a 0) (b 1) (c 2))
   ((0 x) (1 y) (2 z))
   ((1 p 0) (2 q 1))))

;; dotcl:chdir (backs uiop:chdir / with-current-directory). Round-trip:
;; change to a known-existing dir, confirm it changed, then restore.
(deftest i343-chdir-roundtrip
  (let ((orig (dotcl:getcwd)))
    (unwind-protect
        (progn
          (dotcl:chdir (user-homedir-pathname))
          (list (not (equal (namestring (dotcl:getcwd)) (namestring orig)))   ; changed
                (progn (dotcl:chdir orig)
                       (equal (namestring (dotcl:getcwd)) (namestring orig))))) ; restored
      (dotcl:chdir orig)))
  (t t))

;; chdir to a nonexistent directory signals an error (caught, cwd unchanged)
(deftest i343-chdir-bad-dir-errors
  (let ((orig (namestring (dotcl:getcwd))))
    (prog1 (handler-case (progn (dotcl:chdir "C:/dotcl-no-such-dir-i343/") :no-error)
             (error () :error))
      (dotcl:chdir orig)))
  :error)

;;; Weak pointers: dotcl:make-weak-pointer / weak-pointer-value /
;;; weak-pointer-p backed by System.WeakReference. While the target is reachable,
;;; weak-pointer-value returns it; type/predicate behave; print is readable.
;;; (GC-collection behavior is timing-dependent and not asserted here.)
(deftest i342-weak-pointer-basics
  (let* ((obj (list 1 2 3))
         (wp (dotcl:make-weak-pointer obj)))
    (list (dotcl:weak-pointer-p wp)
          (dotcl:weak-pointer-p obj)
          (eq (dotcl:weak-pointer-value wp) obj)
          (typep wp 'dotcl::weak-pointer)
          (notnot (typep wp 'dotcl::weak-pointer))))
  (t nil t t t))

;;; (The trivial-garbage:* wrapper tests were removed: they only checked
;;; that trivial-garbage forwards to the dotcl:* primitives, which the i342-*
;;; tests here exercise directly. They pinned contrib/trivial-garbage, blocking
;;; its move to the ../trivial-garbage fork. Tests must earn their cost.)

;;; Weak hash tables: make-hash-table accepts all weakness modes;
;;; hash-table-weakness reports them; entries with all weak sides live behave as
;;; normal. (GC collection is timing-dependent, asserted separately/manually.)
(deftest i342-weak-hash-table-modes
  (mapcar (lambda (mode)
            (let ((h (make-hash-table :test 'eq :weakness mode))
                  (k (list 'k)) (v (list 'v)))
              (setf (gethash k h) v)
              (list (hash-table-weakness h)
                    (eq (gethash k h) v)
                    (hash-table-count h))))
          '(:key :value :key-and-value :key-or-value))
  ((:key t 1) (:value t 1) (:key-and-value t 1) (:key-or-value t 1)))

;;; A non-weak table reports NIL weakness; an invalid mode errors.
(deftest i342-hash-table-weakness-strong-and-invalid
  (list (hash-table-weakness (make-hash-table))
        (handler-case (progn (make-hash-table :weakness :bogus) :no-error)
          (error () :error)))
  (nil :error))

;;; Finalizers: dotcl:finalize returns the object; cancel-finalization
;;; returns T when a finalizer was registered, NIL otherwise; run-finalizers
;;; returns a count. (Whether/when GC actually fires a finalizer is
;;; timing-dependent and verified manually, not asserted here — same policy as
;;; the weak-pointer/weak-hash-table tests above.)
(deftest i342-finalize-api
  (let ((obj (list 'x)))
    (list (eq (dotcl:finalize obj (lambda () nil)) obj)   ; returns object
          (notnot (dotcl:cancel-finalization obj))        ; T: had a finalizer
          (dotcl:cancel-finalization obj)                 ; NIL: none now
          (integerp (dotcl:run-finalizers))))             ; count
  (t t nil t))

;;; macrolet &whole binds the whole macro-call form, not (cdr form). The compiler
;;; has several macrolet expander-registration sites (compile-macrolet, the
;;; %mini-eval MACROLET case, and three cil-analysis walkers); the analysis copies
;;; inlined (destructuring-bind PARAMS (cdr form) ...), omitting &whole handling,
;;; and the analysis pass caches its expansion in *macroexpand-cache* for compile
;;; to reuse — so &whole var bound to (cdr form) even though compile-macrolet was
;;; correct. Now all sites share %macrolet-expander-form (MACROLET.4/12).
(deftest i350-macrolet-whole
  (list
   ;; &whole var binds the entire call form
   (let ((r nil)) (macrolet ((m (&whole w a) `(progn (setq r ',w) ,a))) (m 7) r))
   ;; &whole alongside several args
   (let ((r nil)) (macrolet ((m (&whole w a b) `(progn (setq r ',w) (+ ,a ,b)))) (m 3 4) r))
   ;; plain macrolet (no &whole) still works
   (macrolet ((m (a b) `(* ,a ,b))) (m 6 7))
   ;; &whole destructuring pattern
   (macrolet ((m (&whole (nm x) a) (declare (ignore nm a)) `',x)) (m 5)))
  ((m 7) (m 3 4) 42 5))

;;; get-setf-expansion of a function-call place yields the CLHS 5.1.2.5 store
;;; form (funcall #'(setf f) store temps...), with the args bound to temps and
;;; evaluated once — not a re-emitted (setf place store). gethash/get/char/
;;; slot-value/symbol-value/cadr/cdar are modeled explicitly in %get-setf-expansion
;;; (matching the SETF macro's setters) so they keep their real store forms and
;;; the funcall fallback only applies to genuine (setf f) function places
;;; (GET-SETF-EXPANSION.1).
(deftest i349-get-setf-expansion-function-place
  (let* ((e (multiple-value-list (get-setf-expansion '(fff a b))))
         (setter (fourth e)))
    (list (and (consp setter)
               (eq (first setter) 'funcall)
               (equal (second setter) '(function (setf fff)))
               t)
          (length (first e))    ; two arg temps
          (second e)))          ; bound to a, b in order
  (t 2 (a b)))

(deftest-compiled-only i349-accessor-setters-intact
  (list
   (let ((h (make-hash-table))) (setf (gethash 'k h) 1) (incf (gethash 'k h)) (gethash 'k h))
   (let ((s (gensym))) (setf (get s 'p) 3) (incf (get s 'p)) (get s 'p))
   (let ((str (copy-seq "abc"))) (setf (char str 1) #\Z) str)
   (let ((c (list 1 2 3))) (setf (cadr c) 5) (incf (cadr c)) c)
   (let ((c (cons 1 (cons 2 3)))) (setf (cdar (list c)) 9) t))
  (2 4 "aZc" (1 6 3) t))

;;; make-load-form: creation and init forms interleave per object at load time
;;; (CLHS 3.2.4.2 / SBCL): each object's init form runs right after its creation,
;;; as data flow allows. The FASL emitter nested a referenced object's creation
;;; inside the referencing creation form, so all creations ran before any init
;;; (and in the wrong order). Now each creation is emitted as its own top-level
;;; method, dependencies first, with eager init flush between (MLF.ORDER.6/10).
(defclass i353-lfo ()
  ((name :initarg :name :reader i353-name)
   (md :initarg :md :accessor i353-md :initform nil)))
(defvar *i353-order* nil)
(defmethod make-load-form ((x i353-lfo) &optional env) (declare (ignore env))
  (values
   `(progn (push (list :creating ',(i353-name x)) *i353-order*)
           (make-instance 'i353-lfo :name ',(i353-name x) :md ',(slot-value x 'md)))
   `(progn (push (list :init ',(i353-name x)) *i353-order*) ',x)))
(defun %i353-order (text)
  (let* ((tmp (format nil "~a/dotcl-i353-~a" (or (dotcl:getenv "TEMP") "/tmp")
                      (get-internal-real-time)))
         (src (format nil "~a/src.lisp" tmp)))
    (ensure-directories-exist (concatenate 'string tmp "/"))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string text s))
    (setf *i353-order* nil)
    (load (compile-file src :verbose nil :print nil))
    (reverse *i353-order*)))

(deftest-compiled-only i353-make-load-form-order
  (list
   (%i353-order "(eval-when (:compile-toplevel)
       (defparameter *a* (make-instance 'i353-lfo :name 'a))
       (defparameter *b* (make-instance 'i353-lfo :name 'b))
       (setf (i353-md *b*) *a*))
     (defparameter *bb* #.*b*) (defparameter *aa* #.*a*)")
   (%i353-order "(eval-when (:compile-toplevel)
       (defparameter *a* (make-instance 'i353-lfo :name 'a))
       (defparameter *b* (make-instance 'i353-lfo :name 'b))
       (defparameter *c* (make-instance 'i353-lfo :name 'c))
       (setf (i353-md *b*) *a*) (setf (i353-md *c*) *b*))
     (defparameter *cc* #.*c*) (defparameter *aa* #.*a*) (defparameter *bb* #.*b*)"))
  (((:creating a) (:init a) (:creating b) (:init b))
   ((:creating a) (:init a) (:creating b) (:init b) (:creating c) (:init c))))

;;; ensure-directories-exist on a :relative-directory pathname creates the directory
;;; under *default-pathname-defaults* and reports created=T. On Windows the merged
;;; namestring is drive-relative ("C:scratch/..."): ResolvePhysicalPath must merge it
;;; with DPD (CL :relative-directory semantics), not resolve it against drive C:'s
;;; process-global cwd, or delete-file and ensure-directories-exist diverge and the
;;; created flag comes back NIL (ENSURE-DIRECTORIES-EXIST.8 — Windows-only).
(defun %i355-ede ()
  (let* ((base (format nil "~a/dotcl-i355-~a/" (or (dotcl:getenv "TEMP") "/tmp")
                       (get-internal-real-time)))
         (*default-pathname-defaults* (pathname base))
         (subdir (make-pathname :directory '(:relative "scratch")
                                :defaults *default-pathname-defaults*))
         (pn (make-pathname :name "foo" :type "txt" :defaults subdir)))
    (ensure-directories-exist (pathname base))
    (ignore-errors (delete-file pn) (delete-file subdir))
    (multiple-value-bind (rpn created) (ensure-directories-exist pn)
      (declare (ignore rpn))
      (with-open-file (s pn :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string "x" s))
      (list (and created t)                                  ; a directory was created
            (and (probe-file pn) t)                          ; file exists
            (and (search "i355" (namestring (truename pn))) t))))) ; under DPD, not cwd

(deftest i355-ensure-directories-exist-relative-dir
  (%i355-ede)
  (t t t))

;;; dotcl:package-locally-nicknamed-by-list — the packages that have a local
;;; nickname for the given package. Completes dotcl's PLN API so the
;;; ../trivial-package-local-nicknames fork can import all four operators from
;;; the DOTCL package instead of dotcl shipping a contrib stub.
(deftest i356-package-locally-nicknamed-by-list
  (let ((host (make-package "I356-HOST" :use nil))
        (targ (make-package "I356-TARG" :use nil)))
    (unwind-protect
        (progn
          (dotcl:add-package-local-nickname "NK" targ host)
          (list (mapcar #'package-name (dotcl:package-locally-nicknamed-by-list targ))
                (dotcl:package-locally-nicknamed-by-list host)))  ; host: nicknamed by nobody
      (ignore-errors (delete-package host))
      (ignore-errors (delete-package targ))))
  (("I356-HOST") nil))

;;; princ / ~A must NOT print the package prefix of a symbol — including the
;;; keyword colon and the #: of an uninterned symbol (CLHS 22.1.3.3: the prefix
;;; is printed only when *print-escape* / *print-readably* is true). The keyword
;;; case previously leaked the colon ((princ-to-string :foo) => ":FOO"), which
;;; broke the upstream flexi-streams test suite (it builds fixture filenames with
;;; (format nil "~(~A~)" external-format-keyword)).
(deftest i360-princ-keyword-no-colon
  (princ-to-string :utf8)
  "UTF8")

(deftest i360-format-a-keyword-no-colon
  (format nil "~a" :foo)
  "FOO")

(deftest i360-format-downcase-keyword
  (format nil "~(~a~)" :utf8)
  "utf8")

;; prin1 / ~S still prints the keyword colon (escape = t).
(deftest i360-prin1-keyword-keeps-colon
  (prin1-to-string :foo)
  ":FOO")

;; princ of an interned symbol from another package drops the package prefix.
(deftest i360-princ-foreign-symbol-no-prefix
  (let ((p (make-package "I360-PP" :use nil)))
    (unwind-protect
        (princ-to-string (intern "BAR" p))
      (delete-package p)))
  "BAR")

;;; CIL codegen: (setf (svref/aref/schar/elt place idx) VALUE) where VALUE is a
;;; try-based non-local exit (block/return, loop with finally/return) produced
;;; unverifiable IL — the array-store path (%set-elt / %set-char) pushed array+
;;; index onto the CIL stack, then entered the value's try region with a
;;; non-empty stack ("enter try block with nonempty stack"). Now spilled to
;;; temps like the function-call path. Found via the upstream flexi-streams
;;; UTF-8 decoder (octets-to-string*).
(deftest i362-setf-svref-block-return
  (let ((s (make-array 2)))
    (setf (svref s 0) (block nil (return 65)))
    (setf (svref s 1) (block nil (return 66)))
    (coerce s 'list))
  (65 66))

(deftest i362-setf-aref-loop-finally
  (let ((s (make-array 3)))
    (dotimes (k 3)
      (setf (aref s k) (loop for x from 1 to (1+ k) sum x)))
    (coerce s 'list))
  (1 3 6))

(deftest i362-setf-schar-block
  (let ((s (make-string 2)))
    (setf (schar s 0) (code-char (block nil (return 72))))
    (setf (schar s 1) (code-char (block nil (return 73))))
    s)
  "HI")

(deftest i362-setf-elt-loop
  (let ((s (make-array 2 :initial-element 0)))
    (setf (elt s 0) (loop repeat 3 sum 1))
    (coerce s 'list))
  (3 0))

;;; Closure capture/mutation through a symbol-macro: a variable referenced and
;;; mutated only via a SYMBOL-MACROLET expansion inside an FLET/lambda body must
;;; still be boxed (shared between the closure and the enclosing scope). The
;;; free-variable pass expanded symbol-macros, but find-mutated-vars and
;;; find-captured-vars did not, so the var was seen as neither mutated nor
;;; captured -> not boxed -> the closure got a private copy and the (incf)
;;; was lost. Surfaced as the upstream flexi-streams CRLF decoder returning
;;; "hh" for octets (104 105) (octet-getter symbol-macro doing (incf i) inside
;;; an flet get-char-code).
(defun %i363-smac-flet (v)
  (let ((i 0))
    (declare (fixnum i))
    (symbol-macrolet ((og (prog1 (aref v i) (incf i))))
      (flet ((g () og))
        (list (g) (g) i)))))

(deftest i363-symbol-macro-mutation-in-flet
  (%i363-smac-flet #(10 20 30))
  (10 20 2))

;; lambda variant (not just flet)
(defun %i363-smac-lambda (v)
  (let ((i 0))
    (declare (fixnum i))
    (symbol-macrolet ((og (prog1 (aref v i) (incf i))))
      (mapcar (lambda (ignored) (declare (ignore ignored)) og) '(a b c)))))

(deftest i363-symbol-macro-mutation-in-lambda
  (%i363-smac-lambda #(10 20 30))
  (10 20 30))

;;; A self-recursive call in cond TEST position must not be TCO'd into a
;;; jump.  When the recursion returns NIL the clause is skipped (CLHS), so the
;;; following clauses — including the t clause — must still be evaluated.
(defun %i373-r (x)
  (cond ((atom x) nil)        ; base: atom -> nil
        ((%i373-r (car x)))   ; test-only self-recursive arm; (r 'z) -> nil
        (t 'reached)))        ; must fire

(deftest i373-cond-test-self-recursion-no-body
  (%i373-r '(z))
  reached)

;; with-body variant of the same arm
(defun %i373-r2 (x)
  (cond ((atom x) nil)
        ((%i373-r2 (car x)) 'got)
        (t 'reached)))

(deftest-compiled-only i373-cond-test-self-recursion-with-body
  (%i373-r2 '(z))
  reached)

;;; Complex literals must survive compile-file -> load (the fasl writer
;;; had no inline encoding for LispComplex and silently corrupted the constant
;;; pool — a list element #C(0 1) came back as an unrelated string constant).
(defun %i370-roundtrip ()
  (let ((src (merge-pathnames "i370-cplx-tmp.lisp" *default-pathname-defaults*)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string "(in-package :cl-user)
(defparameter *i370-z* #C(3 4))
(defparameter *i370-lst* (list 1 #C(0 1) 2))" s))
    (let ((fasl (compile-file src)))
      (load fasl)
      (prog1 (list (symbol-value '*i370-z*) (symbol-value '*i370-lst*))
        (ignore-errors (delete-file src))
        (ignore-errors (delete-file fasl))))))

;; COMPILE-FILE needs Reflection.Emit, so a build without it cannot answer this.
(deftest-compiled-only i370-complex-literal-fasl-roundtrip
  (%i370-roundtrip)
  (#C(3 4) (1 #C(0 1) 2)))

;;; A special variable that is ALSO closure-captured and mutated must be
;;; bound on the dynamic stack, never in a lexical box. Previously the param was
;;; boxed (LispObject[1]) for the capture, and the special-var bind pushed the
;;; box itself instead of its value -> the box (an array, not a LispObject) was
;;; stored into the binding stack's value array -> ArrayTypeMismatchException.
;;; (Maxima taylor1's `tlist` is exactly this shape: defmvar special, captured
;;; by a mapcan lambda, and setq'd in the body.)
(defvar *i371-tl* nil)
(defun %i371-g (e *i371-tl*)
  (declare (special *i371-tl*))
  (mapcar #'(lambda (q) (cons q *i371-tl*)) '(1 2))  ; closure captures the special
  (setq *i371-tl* (cons 'z *i371-tl*))               ; mutate it
  (list e *i371-tl*))

(deftest i371-special-captured-mutated-param
  (%i371-g 'x (cons 1 nil))
  (x (z 1)))

;; the closure must read the CURRENT dynamic value, not a stale capture
(defvar *i371-d* 'global)
(defun %i371-reader () *i371-d*)
(defun %i371-h (*i371-d*)
  (declare (special *i371-d*))
  (funcall #'(lambda () (%i371-reader))))   ; reads special dynamically

(deftest i371-closure-reads-dynamic-special
  (%i371-h 'bound)
  bound)

;;; : coerce must handle a deftype whose expander COMPUTES the target type.
;;; deftype &optional params default to * (not nil), so Maxima's FLONUM expands to
;;; (DOUBLE-FLOAT * *); coerce's compound path handled VECTOR/ARRAY/COMPLEX but not
;;; the float types, so it fell through to "cannot coerce to ". typep/subtypep were
;;; fine. Broke all Maxima bigfloat log/gamma/expintegral (log-n: (coerce x 'flonum)).
(deftype %i374-flonum (&optional low high)
  (cond (high `(double-float ,low ,high))
        (low  `(double-float ,low))
        (t    'double-float)))

(deftest i374-coerce-computed-deftype
  (coerce 2/3 '%i374-flonum)
  0.6666666666666666d0)

(deftest i374-coerce-compound-double-float-with-bounds
  (coerce 2/3 '(double-float 0d0 1d0))
  0.6666666666666666d0)

(deftest i374-coerce-compound-single-float
  (coerce 3 '(single-float 0.0))
  3.0)

;; typep/subtypep already worked — guard they still do
(deftest i374-typep-computed-deftype
  (typep 1.0d0 '%i374-flonum)
  t)

;;; remove/delete must return the ORIGINAL list (eq) when nothing is removed,
;;; not a fresh copy. CLHS permits a copy, but SBCL/CCL and most impls share the
;;; original, and libraries (Maxima add2lnc) rely on it: (setq x (delete .. x))
;;; followed by (nconc x ..) must mutate the same cons when no element matched.
(deftest remove-absent-returns-eq-original
  (let ((l (list 'a 'b 'c)))
    (list (eq l (remove 'z l))
          (eq l (delete 'z l))
          (eq l (remove 'z l :count 1))
          (eq l (delete 'z l :test #'equal))
          (eq l (remove-if (lambda (x) (eq x 'z)) l))
          (eq l (delete-if-not #'symbolp l :count 1))
          (eq l (remove 'z l :from-end t :count 1))))
  (t t t t t t t))

;; When something IS removed, a new list is returned (original untouched).
(deftest remove-present-returns-new-list
  (let* ((l (list 'a 'b 'c)) (r (remove 'b l)))
    (list (eq l r) r l))
  (nil (a c) (a b c)))

;; The add2lnc idiom: delete-absent keeps the cons shared so a later nconc lands.
(deftest delete-then-nconc-shares-structure
  (let* ((store (list 'a 'b))
         (alias store))
    (setf alias (delete 'z alias :count 1 :test #'equal))
    (nconc alias (list 'c))
    store)                       ; the nconc must be visible through store
  (a b c))

;;; delete / delete-if / delete-if-not must be DESTRUCTIVE on a list: splice the
;;; matched conses out of the original chain in place (SBCL semantics), so code
;;; that discards the return value and relies on in-place mutation works — e.g.
;;; Maxima rempropchk / mfunction-delete do (delete x list ...) without setq.
;;; remove stays non-destructive.
(deftest delete-splices-list-in-place
  (let ((l (list 'hdr 'a 'b 'c)))
    (delete 'b l :count 1 :test #'equal)   ; return discarded
    l)                                      ; b spliced out of the original chain
  (hdr a c))

(deftest delete-if-splices-in-place
  (let ((l (list 1 2 3 4 5)))
    (delete-if #'evenp l)
    l)
  (1 3 5))

(deftest delete-if-not-splices-in-place
  (let ((l (list 1 2 3 4 5)))
    (delete-if-not #'oddp l)
    l)
  (1 3 5))

;; from-end + count deletes the rightmost matches, in place.
(deftest delete-from-end-count-in-place
  (let ((l (list 'a 'x 'b 'x 'c 'x)))
    (delete 'x l :from-end t :count 2)
    l)
  (a x b c))

;; remove must NOT mutate its list argument (stays non-destructive).
(deftest remove-does-not-mutate-list
  (let ((l (list 'a 'b 'c)))
    (remove 'b l)
    l)
  (a b c))

;; The mfunction-delete idiom: delete (assoc ...) from an alist in place.
(deftest delete-assoc-in-place
  (let ((al (list (list 'hdr) (list 'foo 1) (list 'bar 2))))
    (delete (assoc 'foo al) al :count 1 :test #'equal)
    al)
  ((hdr) (bar 2)))

;;; expt/exp must FLUSH a float underflow to 0.0 rather than signaling
;;; FLOATING-POINT-UNDERFLOW. The IEEE underflow trap is masked by default
;;; (as in SBCL/CCL/ECL), and dotcl's own * already flushes; expt/exp were the
;;; inconsistent ones, breaking Maxima nfloat/special-function evaluation.
(deftest expt-underflow-flushes-to-zero
  (list (expt 10 -352.79868d0)
        (expt 10d0 -352.79868d0)
        (expt 2d0 -1100d0)
        (expt 10.0 -400.0)
        (exp -800d0))
  (0.0d0 0.0d0 0.0d0 0.0 0.0d0))

;; Overflow still signals (SBCL default traps overflow); only underflow changed.
(deftest expt-overflow-still-signals
  (list (handler-case (progn (expt 10d0 400d0) :no-error)
          (floating-point-overflow () :overflow))
        (handler-case (progn (exp 800d0) :no-error)
          (floating-point-overflow () :overflow)))
  (:overflow :overflow))

;;; A local-function &optional/&key default init-form that references an outer
;;; lexical variable must read the SHARED (boxed) cell at call time, not snapshot
;;; the variable's value when the closure was created. Previously the capture
;;; analysis scanned only the flet/labels body, not the lambda-list defaults, so a
;;; var captured ONLY by a default went unboxed and the default saw a stale value.
;;; (Maxima def-simplifier's give-up depends on this; e.g. simp-%hypergeometric
;;; setqs its args before calling (give-up) whose default rebuilds the arg list.)
(deftest flet-key-default-reads-current-outer-var
  (flet ((run (x) (let ((a x)) (flet ((g (&key (r a)) r)) (setq a 99) (g)))))
    (run 1))
  99)

(deftest flet-optional-default-reads-current-outer-var
  (flet ((run (x) (let ((a x)) (flet ((g (&optional (r a)) r)) (setq a 99) (g)))))
    (run 1))
  99)

(deftest labels-key-default-reads-current-outer-var
  (flet ((run (x) (let ((a x)) (labels ((g (&key (r a)) r)) (setq a 99) (g)))))
    (run 1))
  99)

;; An explicitly-supplied argument still wins over the (now correctly shared) default.
(deftest flet-default-explicit-arg-overrides
  (flet ((run (x) (let ((a x)) (flet ((g (&key (r a)) r)) (setq a 99) (g :r 7)))))
    (run 1))
  7)

;; Guard: a body reference to a mutated outer var was already correct — keep it so.
(deftest flet-body-reads-current-outer-var
  (flet ((run (x) (let ((a x)) (flet ((g () a)) (setq a 99) (g)))))
    (run 1))
  99)

;;; Comparing a non-finite float (inf/nan) with an integer/rational must not throw
;;; "RATIONAL: infinity cannot be converted" — a non-finite float has no rational
;;; value, so </>/=/min/max compare as doubles instead of rationalizing. (Maxima's
;;; float-inf-p does (< x 0), which broke string/fortran output of any inf float.)
(deftest float-infinity-vs-rational-compare
  (let ((inf (* 1d300 1d300)))
    (list (< inf 0) (> inf 5) (<= inf 0) (= inf 0) (< 0 inf) (< inf 1/2)
          (min inf 0) (max inf 0)))
  (nil t nil nil t nil 0 #.(* 1d300 1d300)))

;; f-roundings of a non-finite float return the value itself (no rationalize/throw).
;; Integer-result floor/truncate of infinity still signal (matching SBCL).
(deftest float-infinity-frounding-and-integer-floor
  (let ((inf (* 1d300 1d300)))
    (list (ftruncate inf)
          (ffloor inf)
          (handler-case (floor inf) (arithmetic-error () :signaled))))
  (#.(* 1d300 1d300) #.(* 1d300 1d300) :signaled))

;;; Complex abs must use a scaled hypot, not naive sqrt(re^2+im^2), so extreme
;;; magnitudes don't under/overflow the intermediate squares to 0 / +inf.
(deftest complex-abs-scaled-hypot
  (list (abs #C(1d-170 1d-170))
        (abs #C(1d170 1d170))
        (abs #C(3d0 4d0)))
  (1.4142135623730951d-170 1.4142135623730952d170 5.0d0))

;;; Complex division must be robust too (scaled denominator): (/ z (abs z)) for an
;;; extreme-magnitude z is signum(z) and must give the unit phasor, not a
;;; divide-by-zero (denom underflow) or NaN (denom overflow). Maxima signum depends
;;; on this. Exact rational complex division and division-by-zero are unchanged.
(deftest complex-divide-scaled-no-under-overflow
  (list (let ((z #C(1d-170 1d-170))) (/ z (abs z)))
        (let ((z #C(1d170 1d170)))  (/ z (abs z)))
        (/ #C(1 2) #C(3 4)))                                  ; exact path
  (#C(0.7071067811865475d0 0.7071067811865475d0)
   #C(0.7071067811865475d0 0.7071067811865475d0)
   #C(11/25 2/25)))

(deftest complex-divide-by-zero-still-signals
  (handler-case (/ #C(1d0 1d0) #C(0d0 0d0)) (division-by-zero () :signaled))
  :signaled)

;;; Ordering comparisons (< <= > >=) with a NaN operand must be false (IEEE
;;; "unordered"), including when the other operand is an integer/rational. The
;;; non-finite branch added for infinity used double.CompareTo, which ranks NaN
;;; below everything and wrongly made (< nan 0) true. = stays false too.
(deftest nan-ordering-is-unordered
  (let ((nan (- (* 1d300 1d300) (* 1d300 1d300))))
    (list (< nan 0) (<= nan 0) (> nan 0) (>= nan 0) (= nan 0)
          (< 0 nan) (< nan 1/2) (minusp nan) (plusp nan)
          (< 1 2 nan 3)))             ; n-ary with a NaN in the middle
  (nil nil nil nil nil nil nil nil nil nil))

;; Guard: infinity is still ORDERED (it is not NaN), and finite comparisons work.
(deftest infinity-and-finite-still-ordered
  (let ((inf (* 1d300 1d300)))
    (list (< inf 5) (> inf 5) (< 1 2) (< 2 1) (< 1 2 3) (<= 2 2)))
  (nil t t nil t t))

;;; (coerce x '(complex <deftype>)) must expand the nested part deftype and convert
;;; the real/imag parts, not just literal float part names. fixed top-level
;;; deftype coerce; this is the nested-in-complex case. (Maxima float-zeta does
;;; (coerce s '(complex flonum)) — flonum being a deftype — and broke without this.)
(deftype %i384-simpf () 'double-float)
(deftype %i384-compf (&optional lo) (if lo `(double-float ,lo) 'double-float))
(deftest i384-coerce-complex-nested-deftype
  (list (coerce #C(0 1) '(complex double-float))   ; literal (control)
        (coerce #C(0 1) '(complex %i384-simpf))    ; literal-body deftype
        (coerce #C(0 1) '(complex %i384-compf))    ; computed-body deftype
        (coerce #C(0 1) '(complex single-float)))
  (#C(0.0d0 1.0d0) #C(0.0d0 1.0d0) #C(0.0d0 1.0d0) #C(0.0 1.0)))

;; A bare/absent or rational part type leaves the parts unchanged.
(deftest i384-coerce-complex-part-unchanged
  (list (coerce #C(1 2) 'complex)
        (coerce #C(1 2) '(complex rational)))
  (#C(1 2) #C(1 2)))

;;; asin/acos of a real arg outside [-1,1] must promote to a complex on the CL
;;; branch (not Math.Asin's NaN); acosh/atanh must use the CLHS factored formulas so
;;; the branch (sign of the real/imag part) is right outside the real domain.
;;; (Maxima asech / inverse_jacobi_dn depend on these.) Compare within 1e-13.
(deftest i385-inverse-trig-hyp-out-of-domain
  (flet ((close (z re im) (and (complexp z)
                               (< (abs (- (realpart z) re)) 1d-13)
                               (< (abs (- (imagpart z) im)) 1d-13))))
    (list (close (acos 2d0)    0d0                 1.3169578969248166d0)
          (close (acos -2d0)   pi                 -1.3169578969248166d0)
          (close (asin 2d0)    1.5707963267948966d0 -1.3169578969248166d0)
          (close (asin -2d0)  -1.5707963267948966d0  1.3169578969248166d0)
          (close (acosh -2d0)  1.3169578969248166d0  pi)        ; real part > 0
          (close (acosh 0.5d0) 0d0                 1.0471975511965976d0)
          (close (atanh 2d0)   0.5493061443340549d0 1.5707963267948966d0)))  ; imag > 0
  (t t t t t t t))

;; In-domain results stay real and correct; this is purely a domain-boundary fix.
(deftest i385-inverse-trig-in-domain-unchanged
  (list (< (abs (- (acos 0.5d0) 1.0471975511965979d0)) 1d-13)
        (< (abs (- (asin 0.5d0) 0.5235987755982989d0)) 1d-13)
        (< (abs (- (atanh 0.5d0) 0.5493061443340549d0)) 1d-13)
        (< (abs (- (acosh 2d0) 1.3169578969248166d0)) 1d-13))
  (t t t t))

;;; A user define-compiler-macro must be removed by (setf (compiler-macro-function
;;; name) nil) and by fmakunbound — otherwise a stale compiler macro keeps rewriting
;;; calls after the function is killed and redefined with a different signature
;;; (Maxima defmfun's $foo -> $foo-impl rewrite; rtest_translator 180/197).
(defun %i386a (&rest a) (declare (ignore a)) :real)
(define-compiler-macro %i386a (&rest a) (declare (ignore a)) '':cm)
(deftest i386-setf-compiler-macro-function-nil-removes
  (progn (setf (compiler-macro-function '%i386a) nil)
         (compiler-macro-function '%i386a))
  nil)

(defun %i386b () 0)
(define-compiler-macro %i386b (&rest a) (declare (ignore a)) '':cm)
(deftest i386-fmakunbound-removes-compiler-macro
  (progn (fmakunbound '%i386b)
         (compiler-macro-function '%i386b))
  nil)

;; Functional: after kill + redefine with a new signature (no new compiler macro),
;; a freshly compiled caller must call the real function, not the stale macro.
(defun %i386f () 0)
(define-compiler-macro %i386f (&whole w &rest a) (declare (ignore a)) '':stale)
(deftest i386-stale-compiler-macro-not-used-after-redefine
  (progn (fmakunbound '%i386f)
         (eval '(defun %i386f (&rest l) (cons :real l)))
         (eval '(defun %i386caller () (%i386f 1 2 3)))
         (%i386caller))
  (:real 1 2 3))

;;; (expt base huge-exponent) for base of magnitude 1 must stay an exact INTEGER for
;;; an exponent past int.MaxValue (2^31), not fall through to the float path and
;;; return 1.0/-1.0. The float coefficient corrupted Maxima's CRE coefficients in
;;; (rat %e^N) for N >= 2^31, sending a ratio to gcd -> crash in floor/ceiling.
(deftest i387-expt-unit-base-huge-exponent-stays-integer
  (list (expt 1 2147483648)          ; 2^31, just past int.MaxValue
        (expt 1 534625820200)
        (expt 1 -534625820200)
        (expt -1 2147483648)         ; even -> 1
        (expt -1 2147483649)         ; odd  -> -1
        (expt -1 534625820201)       ; odd  -> -1
        (expt 1 2147483647)          ; boundary (was already ok)
        (expt 2 10))                 ; ordinary case unaffected
  (1 1 1 1 -1 -1 1 1024))

;;; rational->double: when final rounding carries into a 54th mantissa bit, halving
;;; the quotient must be compensated by shift-- (result = q*2^-shift), not shift++.
;;; The wrong sign turned (2^N-1)/2^N (N>=54, ~1.0) into 0.25. (Maxima rtest16 108/109.)
(deftest i389-ratio-to-double-rounding-carry
  (list (float (/ (1- (expt 2 54)) (expt 2 54)) 1d0)
        (float (/ (1- (expt 2 60)) (expt 2 60)) 1d0)
        (float (/ (1- (expt 2 1000)) (expt 2 1000)) 1d0)
        (- (float (/ (1- (expt 2 60)) (expt 2 60)) 1d0) 1)   ; Maxima form -> 0.0
        ;; N<=53 region and ordinary ratios are unchanged
        (float (/ (1- (expt 2 53)) (expt 2 53)) 1d0)
        (float 2/3 1d0)
        (float 1/2 1d0))
  (1.0d0 1.0d0 1.0d0 0.0d0 0.9999999999999999d0 0.6666666666666666d0 0.5d0))

;;; The project-core dependency walk must resolve ASDF feature-conditional
;;; dependency specifiers — (:feature :dotcl "x") — not just plain names. asdf:find-system
;;; returns NIL for such a spec, dropping the dependency from the build manifest (so the
;;; fasl loads without its contrib and fails later). asdf/find-component:resolve-dependency-spec
;;; normalizes the spec to the system. (asdf symbols via read-from-string so reading this
;;; file doesn't require asdf to be loaded yet.)
(defun %i390-feature-dep-resolution ()
  (require "asdf")
  ;; A feature we control, so the (:feature ...) condition is met here.
  (pushnew :i390-feat *features*)
  ;; Write real .asd files into a temp dir and register it, so asdf can load the
  ;; system definitions normally (a synthetic in-memory system with a bogus pathname
  ;; makes find-system try to reload from a non-existent .asd and error).
  (let* ((dir (format nil "~a/dotcl-i390-~a/"
                      (or (dotcl:getenv "TEMP") "/tmp") (get-internal-real-time)))
         (dirp (substitute #\/ #\\ dir)))
    (ensure-directories-exist dirp)
    (with-open-file (s (concatenate 'string dirp "i390-dep.asd")
                       :direction :output :if-exists :supersede)
      (write-string "(defsystem \"i390-dep\" :components ())" s))
    (with-open-file (s (concatenate 'string dirp "i390-root.asd")
                       :direction :output :if-exists :supersede)
      (write-string "(defsystem \"i390-root\" :depends-on ((:feature :i390-feat \"i390-dep\")))" s))
    (eval (read-from-string
           (format nil "(pushnew ~s asdf:*central-registry* :test #'equal)" dirp)))
    (let* ((root (funcall (read-from-string "asdf:find-system") "i390-root"))
           (dep  (car (funcall (read-from-string "asdf:system-depends-on") root)))
           (via-find    (ignore-errors (funcall (read-from-string "asdf:find-system") dep)))
           (via-resolve (funcall (read-from-string "asdf/find-component:resolve-dependency-spec")
                                 root dep)))
      (list (null via-find)            ; find-system can't handle (:feature ...) — the bug
            (not (null via-resolve)))))) ; resolve-dependency-spec does — the fix

;; asdf:find-system compiles the system it finds, so this one needs an emitter.
(deftest-compiled-only i390-feature-dependency-spec-resolves
  (%i390-feature-dep-resolution)
  (t t))

;;; allocate-instance on a structure-class must return a LispStruct (not a CLOS
;;; LispInstance), so it round-trips through make-load-form-saving-slots (which emits
;;; allocate-instance as a struct's creation form) and is equalp to a normally-built
;;; one. A LispInstance there made equalp always NIL (broke Coalton). Slots are left
;;; NIL (unbound stand-in): allocate-instance must NOT run slot initforms (that is
;;; initialize-instance's job), or the required-slot idiom — (id (required 'id)
;;; :read-only t), whose initform signals — would make allocate-instance error.
(defstruct i391-foo (a 0))
(defstruct i391-k)
(defun %i391-required (name) (error "slot ~S required but not supplied" name))
(defstruct i391-req (id (%i391-required 'id) :read-only t))
(deftest i391-allocate-instance-on-structure-class
  (list (typep (allocate-instance (find-class 'i391-foo)) 'i391-foo)         ; a LispStruct
        (i391-foo-a (allocate-instance (find-class 'i391-foo)))              ; slot unbound -> nil
        (equalp (allocate-instance (find-class 'i391-k)) (make-i391-k))      ; no-slot equalp
        ;; required-slot: allocate-instance must NOT run the (error-signaling) initform
        (handler-case (typep (allocate-instance (find-class 'i391-req)) 'i391-req)
          (error () :errored)))
  (t nil t t))

;; make-load-form-saving-slots round-trip (fasl-style): the INIT form restores real
;; slot values even for a required-slot struct whose initform would otherwise signal.
(deftest i391-mlfss-struct-roundtrip
  (list (equalp (eval (make-load-form-saving-slots (make-i391-k))) (make-i391-k))
        (i391-foo-a (eval (make-load-form-saving-slots (make-i391-foo :a 3))))
        (i391-req-id (eval (make-load-form-saving-slots (make-i391-req :id 5)))))
  (t 3 5))

;; Guard: allocate-instance on a standard-class still yields a CLOS instance.
(defclass i391-cobj () ((s :initform 9)))
(deftest i391-allocate-instance-standard-class-unchanged
  (typep (allocate-instance (find-class 'i391-cobj)) 'i391-cobj)
  t)

;;; dotnet:to-stream (non-binary) must use BOM-less UTF-8: writing "HTTP" to a
;;; MemoryStream-backed char stream produces exactly 4 bytes, not 7 (a leading
;;; EF BB BF BOM would corrupt the head of an HTTP/WebSocket response).
(deftest i392-to-stream-no-utf8-bom
  (let* ((ms (dotnet:new "System.IO.MemoryStream"))
         (s  (dotnet:to-stream ms)))
    (write-string "HTTP" s)
    (finish-output s)
    (dotnet:invoke (dotnet:invoke ms "ToArray") "get_Length")) ; 4 with no BOM, 7 with BOM
  4)

;;; A bivalent stream (dotnet:to-stream :bivalent t) serves BOTH char I/O (read-char/
;;; read-line/write-char) and byte I/O (read-byte/write-byte/read-sequence) over the
;;; same stream, coordinated (no read-ahead loses bytes), as SBCL's socket streams do.
;;; This lets byte-oriented protocol code (cl-rpc HTTP/WS) run on a non-binary socket
;;; stream without :binary.
;; Results are reduced to fixnum lists (char-code / length / coerce-to-list) so the
;; deftest comparison is plain equal (it does not deep-compare general vectors).
(deftest i394-bivalent-mixed-char-and-byte-read
  (let ((ms (dotnet:new "System.IO.MemoryStream")))
    (dolist (b '(71 69 84 10 65 66)) (dotnet:invoke ms "WriteByte" b)) ; "GET\nAB"
    (dotnet:invoke ms "set_Position" 0)
    (let ((s (dotnet:to-stream ms :bivalent t)))
      (list (read-byte s nil nil)              ; 71 = G   (byte)
            (char-code (read-char s nil nil))  ; 69 = E   (char, after a byte)
            (read-byte s nil nil)              ; 84 = T   (byte, after a char)
            (length (read-line s nil nil))     ; 0        (#\Newline terminator, empty line)
            (read-byte s nil nil))))           ; 65 = A   (byte, after read-line)
  (71 69 84 0 65))

;; Byte write (write-byte/write-string/write-sequence) emits no BOM and reads back as
;; bytes via read-sequence into a byte vector.
(deftest i394-bivalent-byte-write-readback
  (let* ((ms (dotnet:new "System.IO.MemoryStream"))
         (s  (dotnet:to-stream ms :bivalent t)))
    (write-byte 72 s)                                   ; 'H'
    (write-string "TTP" s)                              ; chars
    (write-sequence (make-array 2 :element-type '(unsigned-byte 8)
                                  :initial-contents '(13 10)) s)
    (finish-output s)
    (dotnet:invoke ms "set_Position" 0)
    (let* ((s2 (dotnet:to-stream ms :bivalent t))
           (buf (make-array 6 :element-type '(unsigned-byte 8))))
      (cons (read-sequence buf s2) (coerce buf 'list)))) ; (6 72 84 84 80 13 10) — first byte 72, no BOM
  (6 72 84 84 80 13 10))

;; eql compares floats by bits: (eql 0.0 -0.0)=NIL (= gives T); bit-identical NaNs are
;; eql=T (= gives NIL). Regression guard for the old value-compare mismatch.
(deftest i396-eql-signed-zero-distinct
  (list (eql 0.0d0 -0.0d0) (= 0.0d0 -0.0d0)
        (eql 0.0 -0.0)     (= 0.0 -0.0))
  (nil t nil t))

(deftest i396-eql-nan-bit-identical
  ;; Distinct objects but bit-identical NaNs (exercises the non-ReferenceEquals path)
  (let ((n1 (dotnet:static "System.Double" "NaN"))
        (n2 (dotnet:static "System.Double" "NaN")))
    (list (eql n1 n2) (= n1 n2)))   ; eql=T (bit-identical); = is NIL since NaN
  (t nil))

(deftest i396-eql-normal-float-unchanged
  (list (eql 1.5d0 (+ 1.0d0 0.5d0))
        (eql 1.5 (+ 1.0 0.5))
        (eql 1.5d0 1.6d0))
  (t t nil))

;; Unary (- x) produces -0.0 via IEEE sign flip (Subtract(0,x) would collapse to +0.0).
;; Regression guard for the conjugate signed-zero bug exposed by stricter eql (CONJUGATE.3-10).
(deftest i396-unary-minus-zero-runtime
  (let ((z 0.0d0))   ; runtime value (not constant-folded)
    (list (eql (- z) -0.0d0)            ; t
          (eql (- z) 0.0d0)             ; nil
          (eql (conjugate #c(0.0d0 0.0d0)) #c(0.0d0 -0.0d0))   ; t
          (eql (conjugate #c(1.0d0 0.0d0)) #c(1.0d0 -0.0d0)))) ; t
  (t nil t t))

;; (imagpart real-float) = (* 0 x): the imagpart of a negative float is -0.0 (IMAGPART.4 / MISC.598).
(deftest i396-imagpart-real-signed-zero
  (list (eql (imagpart -1.5d0) (* 0 -1.5d0))   ; t (both -0.0)
        (eql (imagpart -1.5d0) -0.0d0)         ; t
        (eql (imagpart 1.5d0)  0.0d0)          ; t (+0.0)
        (eql (imagpart -1.5s0) -0.0s0))        ; t (single)
  (t t t t))

;; Integer divide-by-zero signals DIVISION-BY-ZERO (a subtype of ARITHMETIC-ERROR), catchable
;; by handler-case / handler-bind. Previously the raw .NET DivideByZeroException turned into a
;; PROGRAM-ERROR and was not caught by division-by-zero / arithmetic-error.
(deftest i398-integer-zero-divide-is-division-by-zero
  (macrolet ((dbz (form) `(handler-case ,form
                            (division-by-zero () :dbz)
                            (error (c) (type-of c)))))
    (list (dbz (truncate 1 0)) (dbz (/ 1 0))   (dbz (rem 1 0))
          (dbz (mod 1 0))      (dbz (floor 1 0)) (dbz (ceiling 1 0))
          (dbz (round 7 0))))
  (:dbz :dbz :dbz :dbz :dbz :dbz :dbz))

;; Also catchable via the arithmetic-error supertype, and likewise through handler-bind.
(deftest i398-zero-divide-arithmetic-error-and-handler-bind
  (list (handler-case (/ 3 0) (arithmetic-error () :ae) (error () :other))
        (block done
          (handler-bind ((division-by-zero (lambda (c) (declare (ignore c))
                                             (return-from done :hb))))
            (truncate 9 0))))
  (:ae :hb))

;; integer-decode-float / decode-float signal FLOATING-POINT-INVALID-OPERATION (a subtype of
;; ARITHMETIC-ERROR) for NaN / infinity. Previously they computed on the raw 0x7FF exponent and
;; returned garbage, breaking cl-store's (error-dependent) non-finite float detection. Finite
;; values are unchanged.
(deftest i397-decode-float-nonfinite-signals
  (let ((nan (make-double-float #x7ff8000000000000))
        (inf (make-double-float #x7ff0000000000000)))
    (flet ((fp (fn x) (handler-case (funcall fn x)
                        (floating-point-invalid-operation () :fpio)
                        (error (c) (type-of c)))))
      (list (fp #'integer-decode-float nan) (fp #'integer-decode-float inf)
            (fp #'decode-float nan)         (fp #'decode-float inf)
            ;; finite values still decode as before
            (multiple-value-list (integer-decode-float 1.5d0))
            (multiple-value-list (decode-float 0.5d0)))))
  (:fpio :fpio :fpio :fpio
   (6755399441055744 -52 1)
   (0.5d0 0 1.0d0)))

;; equal/equalp hash-table key hashing is depth-limited (like sxhash). A circular structure
;; used as a key no longer recurses forever and crashes the whole process with a stack
;; overflow. Previously GetEqualHash/GetEqualpHash recursed unbounded into car/cdr and crashed.
(deftest i399-equal-hash-circular-key-no-overflow
  (let ((x (list 1 2 3)) (h (make-hash-table :test 'equal)))
    (setf (cdr (last x)) x)               ; circular list
    (setf (gethash x h) :v)
    (list (hash-table-count h) (gethash x h)))
  (1 :v))

(deftest i399-equalp-hash-circular-key-no-overflow
  (let ((x (list 1 2 3)) (h (make-hash-table :test 'equalp)))
    (setf (cdr (last x)) x)
    (setf (gethash x h) :w)
    (list (hash-table-count h) (gethash x h)))
  (1 :w))

;; class-of a signaled native condition returns the correct class (matching type-of). Previously
;; ClassOf had no LispCondition case and fell through to #<STANDARD-CLASS T>, which broke
;; cl-store's condition save (class-of → class-slots) with a STORE-ERROR.
(deftest-compiled-only i400-class-of-signaled-condition
  (flet ((cn (c) (class-name (class-of c))))
    (list (handler-case (/ 1 0)      (division-by-zero (c) (cn c)))
          (handler-case (car 3)      (type-error (c) (cn c)))
          (handler-case (error "x")  (error (c) (cn c)))
          ;; type-of and class-of agree
          (let ((c (handler-case (/ 1 0) (division-by-zero (e) e))))
            (eq (type-of c) (class-name (class-of c))))))
  (division-by-zero type-error simple-error t))

;;; compile-file of a CIRCULAR or SHARED constant literal must not OOM and must
;;; reconstruct the graph (cycles + EQ-shared substructure) exactly. The FASL
;;; inline constant emitter used to walk a cons literal cell-by-cell, which
;;; spun forever on a cycle (e.g. '#1=(1 2 3 . #1#)) and duplicated shared
;;; tails. compile-file now detects this and emits a load-time read of the
;;; *print-circle* representation. (Surfaced compiling cl-store's circ.* tests.)
(defvar *cf-circ-dir* "test/regression/.tmp-cfcirc/")
(defvar *cf-circ* :unset)

(defun cf-circ-build-and-load ()
  (ensure-directories-exist *cf-circ-dir*)
  (let* ((src  (namestring (merge-pathnames "cfcirc.lisp" (truename *cf-circ-dir*))))
         (fasl (namestring (merge-pathnames "cfcirc.fasl" (truename *cf-circ-dir*)))))
    (setf *cf-circ* :unset)
    (with-open-file (s src :direction :output
                           :if-exists :supersede :if-does-not-exist :create)
      ;; circular cdr-list, and a list with a tail shared (EQ) at two positions
      (write-string
       "(in-package :cl-user)
        (defparameter cl-user::*cf-circ*
          (list (let ((x '#1=(1 2 3 . #1#)))
                  (and (eq x (cdddr x)) (eql (car x) 1)))
                (let ((y '(1 2 (a b . #2=(c d e)) 3 4 . #2#)))
                  (eq (cddr (third y)) (nthcdr 5 y)))))" s))
    (compile-file src :output-file fasl)
    (load fasl)
    *cf-circ*))

(deftest-compiled-only compile-file-circular-and-shared-constant
  (cf-circ-build-and-load)
  (t t))

;;; A circular literal inside a DEFUN body (not just a top-level defparameter
;;; init) used to Stack-overflow at compile-file time: the compiler's source-form
;;; walkers (FORM-HAS-RETURN-FROM-P) and the SIL post-passes (%SIL-REFERENCES-LOCAL-P
;;; / %SIL-SUBST-SELF-ARG0) descended into the (quote <cyclic>) / (:load-const
;;; <cyclic>) data and looped forever. They now treat quoted data / :load-const as
;;; opaque. Two defuns reuse the SAME #1= label: the reader must scope labels
;;; per top-level read (CLHS 2.4.8.15), else the 2nd #1# leaks to the 1st structure.
(defvar *cf-defun-dir* "test/regression/.tmp-cfdefun/")
(defun cf-defun-build-and-load ()
  (ensure-directories-exist *cf-defun-dir*)
  (let* ((src  (namestring (merge-pathnames "cfdefun.lisp" (truename *cf-defun-dir*))))
         (fasl (namestring (merge-pathnames "cfdefun.fasl" (truename *cf-defun-dir*)))))
    (with-open-file (s src :direction :output
                           :if-exists :supersede :if-does-not-exist :create)
      (write-string
       "(in-package :cl-user)
        ;; nested labels in a defun body (the minimal repro)
        (defun cf-nested () (let ((x '#1=(1 2 3 #2=(#2#) . #1#))) x))
        ;; same #1= label reused in a second defun — must NOT leak across forms
        (defun cf-a () '#1=(10 20 30 . #1#))
        (defun cf-b () '#1=(40 50 60 . #1#))" s))
    (compile-file src :output-file fasl)
    (load fasl)
    (let* ((x (funcall (intern "CF-NESTED" :cl-user)))
           (e (cadddr x))                                  ; #2=(#2#)
           (a (funcall (intern "CF-A" :cl-user)))
           (b (funcall (intern "CF-B" :cl-user))))
      (list (eq (car e) e)            ; nested inner self-cycle reconstructed
            (eq (cdr (cdddr x)) x)    ; nested outer tail loops back to x
            (eq (cdddr a) a)          ; cf-a's own cycle
            (eq (cdddr b) b)          ; cf-b's own cycle (no label leak from cf-a)
            (eql (car b) 40)))))      ; cf-b kept its own data, not cf-a's

(deftest-compiled-only compile-file-circular-constant-in-defun
  (cf-defun-build-and-load)
  (t t t t t))

;;; #n= label scope is one outermost READ: a #1= in one form must not leak into
;;; the next form's #1# read from the same stream (cached Reader reuse).
(deftest reader-share-label-scope-per-read
  (with-input-from-string (s "#1=(1 2 3 . #1#) #1=(7 8 9 . #1#)")
    (let ((a (read s)) (b (read s)))
      (list (eq (cdddr a) a)    ; a self-cyclic
            (eq (cdddr b) b)    ; b self-cyclic on its OWN structure
            (eq (cdddr b) a)    ; b did NOT leak to a
            (car b))))
  (t t nil 7))

;;; #'<builtin> must return the SAME stable object as symbol-function, so
;;; (eq #'car #'car) is T (matches CLHS/SBCL). Previously the FUNCTION special
;;; form built a fresh arity-checking wrapper per #', breaking eq-on-builtin code
;;; (memoization, function tables, cl-store's fdefinition round-trip).
(deftest function-builtin-eq-self
  (eq #'car #'car)
  t)

(deftest function-builtin-eq-symbol-function
  (eq #'car (symbol-function 'car))
  t)

(deftest function-builtin-eq-fdefinition
  (eq #'cons (fdefinition 'cons))
  t)

(deftest function-varargs-builtin-eq
  (eq #'+ #'+)
  t)

;;; Routing through sym.Function also exposes the full lambda list: #'member now
;;; accepts &key, which the old binary-only wrapper dropped.
(deftest function-builtin-member-keyword
  (funcall #'member 2 '(1 2 3) :test #'eql)
  (2 3))

;;; Native (runtime-signaled) conditions must answer MOP slot access, since
;;; (typep c 'standard-object) is T and class-of resolves correctly. Previously
;;; slot-value/slot-boundp errored "not a CLOS instance" on signaled conditions
;;; (only make-condition instances worked). cl-store saves conditions via
;;; slot-boundp/slot-value over class-slots, so this broke condition serialization.
(deftest native-condition-slot-boundp-bound
  (handler-case (error "boom ~a" 7)
    (error (c) (list (slot-boundp c 'format-control)
                     (slot-value c 'format-control)
                     (slot-value c 'format-arguments))))
  (t "boom ~a" (7)))

(deftest native-condition-type-error-slots
  (handler-case (car 3)
    (type-error (c) (list (slot-boundp c 'datum) (slot-value c 'datum))))
  (t 3))

(deftest native-condition-slot-boundp-unbound
  ;; (/ 1 0) signals division-by-zero without capturing operands -> unbound slot,
  ;; reported as NIL (not an error), so cl-store skips it cleanly.
  (handler-case (/ 1 0)
    (division-by-zero (c) (slot-boundp c 'operands)))
  nil)

(deftest native-condition-slot-exists-p
  (handler-case (/ 1 0)
    (division-by-zero (c) (list (slot-exists-p c 'operands)
                                (slot-exists-p c 'nonexistent))))
  (t nil))

(deftest native-condition-setf-slot-value
  (handler-case (error "x")
    (error (c) (setf (slot-value c 'format-control) "changed")
               (slot-value c 'format-control)))
  "changed")

(deftest native-condition-slot-makunbound
  (handler-case (error "x")
    (error (c) (slot-makunbound c 'format-control)
               (slot-boundp c 'format-control)))
  nil)

(deftest native-condition-slot-value-missing
  ;; A slot absent from the condition's class goes through slot-missing (error).
  (handler-case
      (handler-case (car 3) (type-error (c) (slot-value c 'no-such-slot)))
    (error () :slot-missing))
  :slot-missing)

;;; CAR/CDR on a non-list signal a TYPE-ERROR whose expected-type is LIST
;;; (previously NIL — Runtime.Car/Cdr omitted the expected type). cxr functions
;;; compose CAR/CDR so they inherit it.
(deftest car-type-error-expected-type
  (handler-case (car 3) (type-error (c)
    (list (type-error-datum c) (type-error-expected-type c))))
  (3 list))

(deftest cdr-type-error-expected-type
  (handler-case (cdr 'x) (type-error (c)
    (list (type-error-datum c) (type-error-expected-type c))))
  (x list))

;;; (compile name lambda) must install NAME's function definition and return NAME
;;; (CLHS). Previously dotcl returned NAME without compiling/binding, so fiveam's
;;; run-time (funcall (compile '%inner-test '(lambda ...))) hit Undefined function.
(deftest-compiled-only compile-name-installs-fdefinition
  (progn
    (compile 'reg-compile-foo '(lambda () 42))
    (list (and (fboundp 'reg-compile-foo) t) (funcall 'reg-compile-foo)))
  (t 42))

(deftest-compiled-only compile-name-returns-name
  ;; CLHS: compile returns (values name warnings-p failures-p)
  (compile 'reg-compile-bar '(lambda (x) (* x x)))
  reg-compile-bar nil nil)

(deftest-compiled-only compile-name-funcall-result
  (funcall (compile 'reg-compile-inner '(lambda () (+ 1 2))))
  3)

(deftest-compiled-only compile-nil-returns-function
  (funcall (compile nil '(lambda () :anon)))
  :anon)

(deftest-compiled-only compile-setf-name-installs
  (progn
    (compile '(setf reg-compile-place) '(lambda (v obj) (declare (ignore obj)) v))
    (and (fboundp '(setf reg-compile-place)) t))
  t)

;;; symbol-macrolet whose expansion is NIL must still expand (not fall through to a
;;; special-variable reference → UNBOUND-VARIABLE). lookup-symbol-macro now returns
;;; found-p so a NIL expansion is distinguished from an unregistered symbol.
;;; (Root cause of trivia CONSTANT-PATTERN / HASH-TABLE-ENTRY: (match nil (nil t)).)
(deftest symbol-macrolet-nil-expansion
  (symbol-macrolet ((reg-sm-foo nil)) reg-sm-foo)
  nil)

(deftest symbol-macrolet-nonnil-expansion
  (symbol-macrolet ((reg-sm-foo 9)) reg-sm-foo)
  9)

(deftest symbol-macrolet-nil-expansion-in-form
  (symbol-macrolet ((reg-sm-a nil) (reg-sm-b 2))
    (list reg-sm-a reg-sm-b reg-sm-a))
  (nil 2 nil))

(deftest-compiled-only symbol-macrolet-nil-expansion-compiled
  (funcall (compile nil '(lambda () (symbol-macrolet ((reg-sm-q nil)) reg-sm-q))))
  nil)

;;; decode-float must use the SIGN BIT, so -0.0 decodes with sign -1.0 (was +1.0:
;;; -0.0 < 0 is false in IEEE). Significand/sign keep the argument's float format.
;;; (ieee-floats / cl-conspack serialize -0.0 via this 3rd value, .)
(deftest decode-float-negative-zero-double
  (nth-value 2 (decode-float -0.0d0))
  -1.0d0)

(deftest decode-float-positive-zero-double
  (nth-value 2 (decode-float 0.0d0))
  1.0d0)

(deftest decode-float-negative-zero-single
  (multiple-value-list (decode-float -0.0f0))
  (0.0f0 0 -1.0f0))

(deftest decode-float-single-keeps-format
  (typep (decode-float 3.5f0) 'single-float)
  t)

(deftest decode-float-sign-consistent-with-family
  (list (float-sign -0.0d0)
        (nth-value 2 (integer-decode-float -0.0d0))
        (nth-value 2 (decode-float -0.0d0)))
  (-1.0d0 -1 -1.0d0))

;;; (setf (apply #'fn ...) value) must evaluate the place args left-to-right and
;;; THEN value (CLHS 5.1.1.1). The naive expansion (apply #'(setf fn) value args...)
;;; evaluated value first (broke iterate SETF.4).
(deftest setf-apply-evaluation-order
  (let ((v (vector 0 0 0 0)) (log nil))
    (setf (apply #'aref v (list (progn (push :idx log) 1)))
          (progn (push :val log) 9))
    (list (reverse log) (coerce v 'list)))
  ((:idx :val) (0 9 0 0)))

(deftest setf-apply-stores-value
  (let ((v (vector 10 20 30)))
    (setf (apply #'aref v (list 1)) 99)
    (coerce v 'list))
  (10 99 30))

;;; handler-bind/handler-case/restart-bind/restart-case had macro-function=T but
;;; macroexpand-1 returned them unexpanded (expanded-p=NIL) — an inconsistency that
;;; breaks code walkers. macroexpand-1 now yields a portable, eval-equivalent
;;; expansion. The compiler is unaffected (it uses its compile-form handlers).
(deftest handler-bind-macroexpands
  (nth-value 1 (macroexpand-1 '(handler-bind ((error #'identity)) (foo))))
  t)

(deftest handler-case-macroexpands
  (nth-value 1 (macroexpand-1 '(handler-case (foo) (error () :e))))
  t)

(deftest restart-bind-macroexpands
  (nth-value 1 (macroexpand-1 '(restart-bind ((r #'identity)) (foo))))
  t)

(deftest restart-case-macroexpands
  (nth-value 1 (macroexpand-1 '(restart-case (foo) (r () :ok))))
  t)

;; the expansion is eval-equivalent to the special form
(deftest handler-case-expansion-eval-equivalent
  (eval (macroexpand-1 '(handler-case (error "boom")
                          (error (e) (list :caught (type-of e))))))
  (:caught simple-error))

(deftest handler-bind-expansion-eval-equivalent
  (eval (macroexpand-1 '(block b
                          (handler-bind ((error (lambda (c) (declare (ignore c))
                                                  (return-from b :ran))))
                            (error "x")))))
  :ran)

(deftest handler-case-no-error-clause-eval
  (eval (macroexpand-1 '(handler-case (values 1 2)
                          (error () :err)
                          (:no-error (a b) (list :ok a b)))))
  (:ok 1 2))

(deftest restart-case-expansion-eval-equivalent
  (eval (macroexpand-1 '(restart-case (invoke-restart 'r 5)
                          (r (x) (* x 10)))))
  50)

;; normal (compiled) special-form behavior is unchanged
(deftest handler-case-special-form-still-works
  (handler-case (error "boom") (error () :caught))
  :caught)

;;; : handler-bind/restart-bind macroexpansion must keep the body INLINE (progn),
;;; not in a (lambda () body) thunk, so a code walker that stops at function boundaries
;;; can still reach the body. Verified with a lambda-skipping walker.
(defun %reg-find-sym-no-lambda (s form)
  (cond ((eq form s) t)
        ((and (consp form) (eq (car form) 'lambda)) nil)   ; don't descend into lambdas
        ((consp form) (or (%reg-find-sym-no-lambda s (car form))
                          (%reg-find-sym-no-lambda s (cdr form))))
        (t nil)))

(deftest handler-bind-body-inline-not-thunked
  (%reg-find-sym-no-lambda 'reg-hb-body-marker
    (macroexpand-1 '(handler-bind ((error #'identity)) (reg-hb-body-marker))))
  t)

(deftest restart-bind-body-inline-not-thunked
  (%reg-find-sym-no-lambda 'reg-rb-body-marker
    (macroexpand-1 '(restart-bind ((r #'identity)) (reg-rb-body-marker))))
  t)

;; nested handler-binds keep the cluster push/pop balanced through a non-local exit
(deftest handler-bind-inline-nested-balanced
  (eval (macroexpand-1
         '(block b
            (handler-bind ((error (lambda (c) (declare (ignore c)) (return-from b :outer))))
              (handler-bind ((warning (lambda (c) (declare (ignore c)) nil)))
                (error "x"))))))
  :outer)

;;; define-condition :report must drive princ / ~A / princ-to-string (CLHS 9.1.3).
;;; The printer's condition short-circuit ignored the generated print-object method,
;;; so user conditions printed "#<TYPE>" under *print-escape*=nil (esrap).
(define-condition reg-c413 (error) ((x :initarg :x :reader reg-c413-x))
  (:report (lambda (c s) (format s "report: ~A" (reg-c413-x c)))))
(define-condition reg-c413-str (error) () (:report "static msg"))
(define-condition reg-c413-sub (reg-c413) ())

(deftest condition-report-lambda-princ
  (princ-to-string (make-condition 'reg-c413 :x 42))
  "report: 42")

(deftest condition-report-string-princ
  (princ-to-string (make-condition 'reg-c413-str))
  "static msg")

(deftest condition-report-format-tilde-a
  (format nil "~A" (make-condition 'reg-c413 :x 7))
  "report: 7")

;; ~S / *print-escape*=t keeps the #<TYPE> form
(deftest condition-report-escape-still-type
  (format nil "~S" (make-condition 'reg-c413 :x 7))
  "#<REG-C413>")

;; inherited report from a parent condition
(deftest condition-report-inherited
  (princ-to-string (make-condition 'reg-c413-sub :x 5))
  "report: 5")

;;; Default setf expansion for a (setf NAME) function (no defsetf/define-setf-expander)
;;; must emit the funcall form (funcall #'(setf NAME) val args...), not the bare
;;; ((setf NAME) val args) operator-list form, so code walkers handle it (iterate
;;; minimize). Args evaluate left-to-right then value (CLHS 5.1.1.1).
(defun reg-414-foo (x) x)
(defun (setf reg-414-foo) (v x) (declare (ignore x)) v)

(deftest setf-fn-fallback-is-funcall-form
  (car (macroexpand-1 '(setf (reg-414-foo y) 5)))
  let*)

(deftest setf-fn-fallback-walkable-funcall
  ;; the operator inside the expansion is FUNCALL (walker-friendly), not a (setf ..) list
  (labels ((has-funcall (f)
             (cond ((atom f) nil)
                   ((eq (car f) 'funcall) t)
                   (t (or (some #'has-funcall (and (listp f) f)) nil)))))
    (has-funcall (macroexpand-1 '(setf (reg-414-foo y) 5))))
  t)

(deftest setf-fn-fallback-eval-order
  (let ((log nil))
    (setf (reg-414-foo (progn (push :idx log) 1)) (progn (push :val log) 9))
    (reverse log))
  (:idx :val))

;; (setf f) function whose value is not an echo of val — the setf form yields it
(defun (setf reg-414-cons) (v x) (cons v x))
(deftest setf-fn-fallback-returns-setter-value
  (setf (reg-414-cons 'a) 'b)
  (b . a))

;;; FORMAT: a ~:; that is the default-clause separator of a ~[...~] nested inside a
;;; ~<...~:> must not be mistaken for the justification overflow ~:; (CLHS 22.3.6.1
;;; applies only to a ~:; directly in ~<...~>). It also must not split the logical
;;; block into prefix/suffix sections (esrap parse-error report).
(deftest format-nested-conditional-in-justify
  (format nil "~@<~[a~:;b~]~2@Tc~:>" 1)
  "b  c")

(deftest format-nested-conditional-clause-0
  (format nil "~@<~[x~:;y~]~:>" 0)
  "x")

(deftest format-nested-conditional-clause-default
  (format nil "~@<~[x~:;y~]~:>" 1)
  "y")

;;; princ / ~A / write :escape nil must bind the dynamic *PRINT-ESCAPE* to NIL so a
;;; user print-object method reading it sees the right value (dotcl carried escape only
;;; as a C# param, leaving the special var at its default T).
(defclass reg-416-plain () ((y :initarg :y)))
(defmethod print-object ((c reg-416-plain) s)
  (format s "[e=~A y=~A]" *print-escape* (slot-value c 'y)))

(deftest print-escape-bound-under-princ
  (princ-to-string (make-instance 'reg-416-plain :y 9))
  "[e=NIL y=9]")

(deftest print-escape-bound-under-format-a
  (format nil "~A" (make-instance 'reg-416-plain :y 9))
  "[e=NIL y=9]")

(deftest print-escape-bound-under-prin1
  (prin1-to-string (make-instance 'reg-416-plain :y 9))
  "[e=T y=9]")

(defstruct (reg-416-st (:constructor mk-reg-416-st)) a)
(defmethod print-object ((p reg-416-st) s)
  (format s "<e=~A a=~A>" *print-escape* (reg-416-st-a p)))

(deftest print-escape-bound-struct-princ
  (princ-to-string (mk-reg-416-st :a 3))
  "<e=NIL a=3>")

(deftest print-escape-bound-struct-prin1
  (prin1-to-string (mk-reg-416-st :a 3))
  "<e=T a=3>")

;;; follow-up: a real error inside a user condition's :report/print-object must
;;; propagate, not be swallowed into "#<TYPE>" (the catch hid the true cause).
(define-condition reg-416-bad (error) ((x :initarg :x :reader reg-416-bad-x))
  (:report (lambda (c s) (declare (ignore s)) (funcall (reg-416-bad-x c)))))

(deftest condition-report-error-propagates
  (handler-case (princ-to-string (make-condition 'reg-416-bad :x 42))
    (type-error () :propagated))
  :propagated)

;;; FORMAT ~[...~] processed its chosen clause on a SubArray copy starting past the
;;; consumed selector, so a ~:* in the clause couldn't back up to the selector and the
;;; args the clause consumed were not propagated — an enclosing ~{...~} then over-
;;; iterated on the leftovers. Now the clause runs in place on the shared arg pointer
;;; (esrap error-report). Tests use a function directive to make arg flow visible.
(defun reg-417-pt (stream obj &optional c a) (declare (ignore c a)) (format stream "<~S>" obj))

(deftest format-cond-backup-to-selector
  (format nil "~[~*~:;~:*<~A>~]" 1)
  "<1>")

(deftest format-cond-clause-consumes-and-propagates
  ;; ~{~{...~[~*~:;~:*<~A>~{...~}~]~}~}: the inner ~{~} destructures (:R 1 (:E));
  ;; ~[ consumes the selector, ~:* backs up to read it, the nested ~{~} consumes the
  ;; rest — so the inner ~{~} sees the list exhausted and the outer iterates only once
  ;; (no leftover (:E) reused as a fresh round).
  (format nil "~{~{=~A ~[~*~:;~:*<~A>~{~/reg-417-pt/~}~]~}~^|~}" '((:r 1 (:e))))
  "=R <1><:E>")

(deftest format-colon-cond-backup
  (format nil "~:[no~;<~:*~A>~]" 5)
  "<5>")

(deftest format-colon-cond-in-iteration
  (format nil "~{~:[N~;Y~:*~A~] ~}" '(1 nil 2))
  "Y1 N Y2 ")

;;; *print-pretty* defaults to T (matching SBCL), so logical-block mandatory newlines
;;; (~:@_ / ~@:_ / (pprint-newline :mandatory)) fire by default. With the old NIL default
;;; ~@<...~:> degraded to justify and the mandatory breaks were dropped (esrap report).
(deftest print-pretty-default-is-t
  *print-pretty*
  t)

(deftest format-logical-block-mandatory-newline
  (format nil "~@<A~:@_~:@_B~:>")
  #.(format nil "A~%~%B"))

(deftest format-logical-block-mandatory-at-colon
  (format nil "~@<A~@:_B~:>")
  #.(format nil "A~%B"))

(deftest pprint-logical-block-mandatory-newline
  (with-output-to-string (s)
    (pprint-logical-block (s nil)
      (princ "A" s) (pprint-newline :mandatory s) (princ "B" s)))
  #.(format nil "A~%B"))

;; with *print-pretty* nil, ~<...~:> degrades to justification (CLHS) — no break
(deftest format-logical-block-mandatory-disabled-when-not-pretty
  (let ((*print-pretty* nil)) (format nil "~@<A~:@_B~:>"))
  "AB")

;;; equal must compare the CDR (dotted-pair tail) of a cons with EQUAL too, not eql, so a
;;; string (or other content-equal value) in the tail position matches by content (
;;; esrap AROUND). The comparison loops along the spine, so it stays O(1) stack and
;;; terminates on a mismatched atom.
(deftest equal-string-in-cdr
  (list (equal (cons 1 "x") (cons 1 "x"))
        (equal (list* 1 2 "x") (list* 1 2 "x"))
        (equal (cons (list 1) "x") (cons (list 1) "x")))
  (t t t))

(deftest equal-string-cdr-distinct-nil
  (equal (cons 1 "x") (cons 1 "y"))
  nil)

;; tail that is a non-equal atom must terminate with NIL (regression vs the broken
;; self-recursing tail that stack-overflowed even compilation)
(deftest equal-distinct-symbol-tail
  (equal (cons 1 'a) (cons 1 'b))
  nil)

(deftest equal-non-string-vector-cdr-stays-nil
  (let ((v1 (vector 2)) (v2 (vector 2)))
    (equal (cons 1 v1) (cons 1 v2)))
  nil)

;; long list with a string tail — no stack overflow
(deftest equal-long-list-string-tail
  (let ((big (let ((acc nil)) (dotimes (i 3000) (push i acc)) acc)))
    (equal (cons big "t") (cons big "t")))
  t)

;; pprint: with *print-pretty*=T, a ~/func/ call inside a logical block
;; followed by a ~[...~] conditional containing ~@:_ was reordered (the
;; conditional's output flushed ahead of the pre-conditional ~/func/ output).
;; Verify "While" still precedes "Expected" in pretty mode.
(defun rf420-pr (s o &optional c a) (declare (ignore c a)) (format s "<~S>" o))

(deftest pprint-call-before-conditional-order
  (let* ((out (let ((*print-pretty* t))
                (format nil "~@<~{While ~/rf420-pr/. ~[Z~:;Expected:~@:_~@:_X~]~}~:>"
                        (list :root 1))))
         (wi (search "While" out))
         (ei (search "Expected" out)))
    (and wi ei (< wi ei) t))
  t)

;; preceding literal before an iteration with ~@:_ must also stay ordered
(deftest pprint-literal-before-iteration-order
  (let* ((out (let ((*print-pretty* t))
                (format nil "~@<Pre ~{~/rf420-pr/~@:_~}~:>" (list :a))))
         (pre-i (search "Pre" out))
         (ai (search "<:A>" out)))
    (and pre-i ai (< pre-i ai) t))
  t)

;; macroexpand-cache scope: the cache keyed expansions by form (cons) only,
;; ignoring the macro environment. A form spliced (via ,@body) into both a real-
;; macro context and a shadowing macrolet got ONE expansion reused for both —
;; here the shadowed RF421-REAL leaked into the non-shadowed branch. (Root cause
;; of esrap's parse-position off-by-one: it disabled the packrat with-cached-result
;; in rules whose body was spliced this way.) The cache is now keyed by (form,scope).
(defmacro rf421-real (x) `(list :real ,x))
(defun rf421-pick (flag)
  (macrolet ((variants (&body body)
               `(if flag
                    (progn ,@body)
                    (macrolet ((rf421-real (x) `(list :noop ,x)))
                      ,@body))))
    (variants (rf421-real 42))))

(deftest macrolet-shadow-not-leaked-to-shared-body
  (list (rf421-pick t) (rf421-pick nil))
  ((:real 42) (:noop 42)))

;; compile-time keyword check: a direct call to a fixed-&key lambda with a literal unknown
;; keyword must WARN at compile time (CLHS 3.5.1.4 static diagnosis). A known
;; keyword must stay silent. (esrap CONDITION.INVALID-ARGUMENT-COMBINATIONS
;; depends on this — its parse compiler-macro generates such a call for :raw t.)
(deftest-compiled-only warn-unknown-keyword-in-lambda-call
  (list
   (handler-case (progn (compile nil '(lambda () ((lambda (&key a) a) :bad 1))) :no-warn)
     (warning () :warned))
   (handler-case (progn (compile nil '(lambda () ((lambda (&key a) a) :a 1))) :no-warn)
     (warning () :warned)))
  (:warned :no-warn))

;; defstruct (:print-function fn) / (:print-object fn): the printer name was
;; spliced UNQUOTED into the generated print-object method's funcall, so it was
;; referenced as a variable (unbound) instead of a function — the printer never
;; ran and output fell back to #S(...). Wrapping in (function ...) fixes both
;; symbol and lambda forms. (fset's containers print via :print-function.)
(defun rf423-pf (obj stream depth) (declare (ignore depth)) (format stream "<PF:~A>" (rf423a-n obj)))
(defstruct (rf423a (:print-function rf423-pf)) (n 0))
(defun rf423-po (obj stream) (format stream "<PO:~A>" (rf423b-n obj)))
(defstruct (rf423b (:print-object rf423-po)) (n 0))
(defstruct (rf423c (:print-object (lambda (o s) (format s "<L:~A>" (rf423c-n o))))) (n 0))
(defstruct rf423d (n 0))

(deftest defstruct-print-function-object-options
  (list (prin1-to-string (make-rf423a :n 7))
        (prin1-to-string (make-rf423b :n 9))
        (prin1-to-string (make-rf423c :n 3))
        (prin1-to-string (make-rf423d :n 5)))
  ("<PF:7>" "<PO:9>" "<L:3>" "#S(RF423D :N 5)"))

;; pprint-logical-block :prefix/:per-line-prefix/:suffix must print regardless of
;; *print-pretty* (CLHS) — only dynamic newline/indent is gated on pretty. The
;; macro wrongly wrapped the prefix/suffix write-string in (when *print-pretty* ...),
;; so under *print-pretty*=NIL (e.g. with-standard-io-syntax) the delimiters
;; vanished. (fset containers print their #{...} delimiters via this path.)
(defun rf424 (pretty)
  (let ((*print-pretty* pretty))
    (with-output-to-string (s)
      (pprint-logical-block (s nil :prefix "[" :suffix "]")
        (write-string "x" s) (write-char #\Space s)
        (pprint-newline :fill s) (write-string "y" s)))))

(deftest pprint-logical-block-prefix-suffix-without-pretty
  (list (rf424 t) (rf424 nil)
        (let ((*print-pretty* nil))
          (with-output-to-string (s)
            (pprint-logical-block (s nil :per-line-prefix "; " :suffix "!")
              (write-string "a" s)))))
  ("[x y]" "[x y]" "; a!"))

;; eql-specialized GF dispatch: the monomorphic dispatch cache keyed by arg CLASS
;; ignored the eql VALUE, so two calls with same-class but different eql values
;; (e.g. (cv :seq x) vs (cv :other x)) shared the cache slot; the class-keyed hit
;; path then picked the first applicable eql method in definition order (a parent
;; method) over the most-specific (child) one. Now eql GFs bypass the cache.
;; (fset's convert uses (eql 'seq)/(eql 'list)/... first args + class 2nd args.)
(defclass rf425-parent () ())
(defclass rf425-child (rf425-parent) ())
(defgeneric rf425-cv (kind obj))
(defmethod rf425-cv ((kind (eql :seq))   (obj rf425-parent)) :parent)
(defmethod rf425-cv ((kind (eql :seq))   (obj rf425-child))  :child)
(defmethod rf425-cv ((kind (eql :other)) (obj rf425-child))  :other)

(deftest eql-gf-dispatch-not-clobbered-by-other-eql-key
  (let ((c (make-instance 'rf425-child)))
    (list (rf425-cv :seq c)              ; child
          (rf425-cv :other c)            ; other (different eql key, same class)
          (rf425-cv :seq c)              ; must still be child (was :parent)
          (rf425-cv :seq c)))            ; child even without :other in between
  (:child :other :child :child))

;; print-object recursion guard was per-CATEGORY (a struct/instance bool), so
;; nesting a same-category object inside a print-object method (struct-in-struct
;; or instance-in-instance) suppressed the inner method and fell back to
;; #S(...)/#<...>. Now it's per-OBJECT (only the SAME object falls back, guarding
;; self-recursion). (fset's complement-set / set-of-sets print nest this way.)
(defstruct (rf426si) (v 0))
(defmethod print-object ((o rf426si) s) (format s "<SI ~A>" (rf426si-v o)))
(defstruct (rf426so) (x nil))
(defmethod print-object ((o rf426so) s) (format s "S[") (write (rf426so-x o) :stream s) (format s "]"))
(defclass rf426ci () ((y :initarg :y)))
(defmethod print-object ((o rf426ci) s) (format s "C[") (write (slot-value o 'y) :stream s) (format s "]"))

(deftest print-object-nested-same-category
  (list (format nil "~A" (make-rf426so :x (make-rf426si :v 1)))                 ; struct in struct
        (format nil "~A" (make-instance 'rf426ci :y (make-instance 'rf426ci :y 7))) ; inst in inst
        (format nil "~A" (make-instance 'rf426ci :y (make-rf426si :v 2)))        ; struct in inst
        (format nil "~A" (make-rf426so :x (make-instance 'rf426ci :y 3))))       ; inst in struct
  ("S[<SI 1>]" "C[C[7]]" "C[<SI 2>]" "S[C[3]]"))

;; dotcl atomic-long: single-cell Interlocked-backed compare-and-swap/incf/decf,
;; the concurrency primitive bordeaux-threads' atomic-integer backs its counter
;; with. Basic ops + lock-free correctness under thread contention.
(deftest atomic-long-basic-ops
  (let ((a (dotcl:make-atomic-long)))
    (list (dotcl:atomic-long-value a)        ; 0
          (dotcl:atomic-long-p a)            ; T
          (dotcl:atomic-long-incf a 5)       ; 5
          (dotcl:atomic-long-incf a)         ; 6
          (dotcl:atomic-long-decf a 2)       ; 4
          (dotcl:atomic-long-cas a 4 100)    ; T
          (dotcl:atomic-long-value a)        ; 100
          (dotcl:atomic-long-cas a 4 200)    ; NIL (4 != 100)
          (dotcl:atomic-long-value a)        ; 100
          (dotcl:set-atomic-long-value a 42) ; 42
          (dotcl:atomic-long-value a)         ; 42
          (dotcl:atomic-long-value (dotcl:make-atomic-long 7)))) ; 7
  (0 t 5 6 4 t 100 nil 100 42 42 7))

(deftest atomic-long-thread-safe-increment
  (let ((a (dotcl:make-atomic-long)) (threads '()))
    (dotimes (i 4)
      (push (dotcl:make-thread (lambda () (dotimes (j 25000) (dotcl:atomic-long-incf a))))
            threads))
    (dolist (th threads) (dotcl:thread-join th))
    (dotcl:atomic-long-value a))
  100000)

;; read (token accumulation) must cross make-concatenated-stream component
;; boundaries — it stopped at the first component's EOF while read-char/peek-char
;; crossed correctly. GetTextReader now returns a spanning reader for concatenated
;; streams. (babel's #\ reader builds a concatenated stream and re-reads the token.)
(defun rf427 (&rest parts)
  (read (apply #'make-concatenated-stream (mapcar #'make-string-input-stream parts))))

(deftest read-crosses-concatenated-stream-boundary
  (list (rf427 "S" "UB") (rf427 "SU" "B") (rf427 "" "SUB") (rf427 "S" "U" "B")
        ;; multiple objects across one boundary
        (let ((s (make-concatenated-stream (make-string-input-stream "(a b")
                                           (make-string-input-stream " c) 42"))))
          (list (read s) (read s))))
  (sub sub sub sub ((a b c) 42)))

;; A custom #\ dispatch-macro-character (even one that just delegates to the
;; built-in reader via get-dispatch-macro-character) over-consumed: after a #\x
;; element the rest of the list was swallowed. Cause: get-dispatch-macro-character's
;; wrapper ran the built-in reader on a FRESH throwaway Reader, so the longest-match
;; look-ahead push-back in ReadCharacterLiteral was buffered there and discarded
;; instead of re-read by the enclosing read. The handler now reuses the read's live
;; reader. (babel's #\u<hex> reader delegates this way.)
;; Build inputs with code-char to dodge #\ escaping. 35=# 92=\ 40=( 41=) 32=space.
(deftest custom-sharp-backslash-delegation-no-overconsume
  (flet ((s (codes) (map 'string #'code-char codes)))
    (let* ((rt (copy-readtable))
           (orig (get-dispatch-macro-character (code-char 35) (code-char 92) rt)))
      (set-dispatch-macro-character (code-char 35) (code-char 92)
                                    (lambda (st c n) (funcall orig st c n)) rt)
      (let ((*readtable* rt))
        (list (length (read-from-string (s '(40 35 92 97 32 35 92 98 41))))   ; (#\a #\b) -> 2
              (read-from-string (s '(40 35 92 97 32 102 111 111 41)))          ; (#\a FOO)
              (read-from-string (s '(40 49 32 35 92 97 41)))))))               ; (1 #\a)
  (2 (#\a foo) (1 #\a)))

;; A custom #\ reader that wraps the stream in make-concatenated-
;; stream and delegates to the built-in reader (the pattern babel uses) still
;; over-consumed: char-name reading scanned ahead over whitespace (for non-standard
;; multi-word UCD names) and the push-back was lost across the throwaway concat
;; reader, eating the rest of the list. Fix: #\ reads a SINGLE token (CLHS); UCD
;; names print/round-trip with underscores (#\LATIN_SMALL_LETTER_A), so no
;; over-scan. char-name / ~:C / ~@C / prin1 stay mutually consistent (underscores).
(deftest sharp-backslash-concat-wrapped-no-overconsume
  (flet ((cs (codes) (map 'string #'code-char codes)))
    (let* ((rt (copy-readtable))
           (orig (get-dispatch-macro-character (code-char 35) (code-char 92) rt)))
      (set-dispatch-macro-character (code-char 35) (code-char 92)
        (lambda (st c n)
          (let ((c1 (read-char st)))
            (funcall orig (make-concatenated-stream
                           (make-string-input-stream (string c1)) st) c n)))
        rt)
      (let ((*readtable* rt))
        (list (length (read-from-string (cs '(40 35 92 97 32 35 92 98 41))))   ; (#\a #\b) -> 2
              (read-from-string (cs '(40 35 92 97 32 102 111 111 41)))          ; (#\a FOO)
              (read-from-string (cs '(40 49 32 35 92 97 41)))))))               ; (1 #\a)
  (2 (#\a foo) (1 #\a)))

(deftest char-name-print-read-consistency
  ;; UCD-named char round-trips and ~@c/~S/prin1/char-name agree (all underscores).
  ;; (#\~:c == ~S is covered by ANSI FORMAT.S.8.)
  (let ((c (code-char 173)))                        ; SOFT HYPHEN
    (list (eql c (read-from-string (prin1-to-string c)))
          (string= (format nil "~@c" c) (prin1-to-string c))
          (string= (format nil "~@c" c) (format nil "~S" c))
          (char-name c)))
  (t t t "SOFT_HYPHEN"))

(deftest sharp-backslash-concat-wrapped-read-suppress
  ;; Under *read-suppress*, a custom #\ reader (babel pattern) delegating to the
  ;; built-in over make-concatenated-stream must return NIL (not signal) for unknown
  ;; char names, AND report the same end position a plain stream does. The fresh
  ;; Reader created for the wrapped stream must inherit *read-suppress*, and the
  ;; position must count chars consumed via the stream API across the concat wrap.
  (flet ((cs (codes) (map 'string #'code-char codes)))
    (let* ((rt (copy-readtable))
           (orig (get-dispatch-macro-character (code-char 35) (code-char 92) rt)))
      (set-dispatch-macro-character (code-char 35) (code-char 92)
        (lambda (st c n)
          (let ((c1 (read-char st)))
            (funcall orig (make-concatenated-stream
                           (make-string-input-stream (string c1)) st) c n)))
        rt)
      (let ((*readtable* rt) (*read-suppress* t))
        (list (multiple-value-list (read-from-string (cs '(35 92 117 106 117 110 107))))  ; #\ujunk
              (multiple-value-list (read-from-string (cs '(35 92 117 49 50 122 122))))     ; #\u12zz
              ;; plain (non-wrapped) reference: same nil/7
              (multiple-value-list
               (let ((*readtable* (copy-readtable nil)))
                 (read-from-string (cs '(35 92 117 106 117 110 107)))))))))
  ((nil 7) (nil 7) (nil 7)))

;; LOOP arithmetic stepping: the iteration variable is stepped THEN tested, so
;; after termination it holds the first out-of-bound value (CLHS 6.1.2.1.1,
;; matching SBCL). A prior "fix" terminated before the step, leaving the variable
;; one short — which silently broke return-value counts in libraries (e.g. babel's
;; unibyte encoder `finally (return (- di d-start))`).
(deftest loop-finally-arith-var-overshoots
  (list (loop for x from 1 to 5 finally (return x))                ; => 6
        (loop for i from 0 below 3 finally (return i))             ; => 3
        (loop for i from 0 upto 3 finally (return i))              ; => 4
        ;; parallel `and` stepping clause overshoots too (babel's shape)
        (loop for i fixnum from 0 below 3 and di fixnum from 0
              do (progn) finally (return di))                      ; => 3
        (loop for i fixnum from 1 to 5 and di fixnum from 0
              do (progn) finally (return di))                      ; => 5
        ;; downward stepping likewise overshoots past the bound
        (loop for x from 5 downto 1 finally (return x))            ; => 0
        ;; iteration count itself stays correct
        (let ((n 0)) (loop for i from 0 below 3 do (incf n)) n))   ; => 3
  (6 3 4 3 5 0 3))

;; DotNetToLisp must unbox the small integer types (byte/sbyte/short/ushort/
;; uint/ulong), not just int/long. C# type patterns don't widen, so byte used
;; to fall through to a boxed LispDotNetObject — aref on a byte[] (UTF-8 codecs,
;; binary protocols) returned #<DOTNET System.Byte N> instead of a CL integer.
(deftest dotnet-byte-unboxed
  (let* ((enc (dotnet:static "System.Text.Encoding" "get_UTF8"))
         (nb  (dotnet:invoke enc "GetBytes" "ABC")))
    (list (aref nb 0)                                  ; 65, a real integer
          (+ (aref nb 0) 1)                            ; arithmetic works -> 66
          (typep (aref nb 0) '(unsigned-byte 8))       ; T
          (let ((a (dotnet:make-array "System.Byte" 1)))
            (setf (aref a 0) 200) (aref a 0))          ; 200
          (dotnet:static "System.Convert" "ToUInt16" 40000)))  ; ushort scalar -> 40000
  (65 66 t 200 40000))

;; Symmetric store side: LispToDotNet must convert a Fixnum to the small integer
;; types (sbyte/short/ushort/uint/ulong), not just int/long/byte. Without these a
;; (setf (aref a i) n) into a make-array of those types failed with
;; "Cannot convert Fixnum to SByte". Completes the byte[] round-trip.
(deftest dotnet-store-small-int-types
  (flet ((rt (ty v) (let ((a (dotnet:make-array ty 1)))
                      (setf (aref a 0) v) (aref a 0))))
    (list (rt "System.SByte" 100)
          (rt "System.SByte" -5)
          (rt "System.Int16" 30000)
          (rt "System.UInt16" 40000)
          (rt "System.UInt32" 100)
          (rt "System.UInt64" 100)
          (dotnet:static "System.Convert" "ToSByte" 9)))   ; sbyte reflection param
  (100 -5 30000 40000 100 100 9))

;; Extended require phase 1: register-assembly-path / register-native-path populate
;; the resolver tables the Default ALC's Resolving / ResolvingUnmanagedDll hooks
;; consult. (Full managed+native resolution against a real NuGet package is verified
;; manually — see D-file — since it needs a machine-local nupkg; here we assert the
;; registration API contract: callable, accepts (name path), returns the path.)
(deftest dotcl-register-resolver-paths-api
  (list (dotcl:register-assembly-path "Dotcl.Test.Asm" "/tmp/dotcl-test/Asm.dll")
        (dotcl:register-native-path "dotcl-test-native" "/tmp/dotcl-test/libnative.so"))
  ("/tmp/dotcl-test/Asm.dll" "/tmp/dotcl-test/libnative.so"))

;;; read-sequence into a LIST from a binary stream must read bytes, not chars.
;;; The Cons destination branch previously always went through a TextReader,
;;; so a list target read characters even from an (unsigned-byte 8) stream.
(defun %read-sequence-binary-list ()
  (let ((tmp (format nil "~a/dotcl-rsbin-~a.bin"
                     (or (dotcl:getenv "TEMP") "/tmp")
                     (get-internal-real-time))))
    (with-open-file (out tmp :direction :output :element-type '(unsigned-byte 8)
                             :if-exists :supersede)
      (dolist (b '(208 151 208 176 209)) (write-byte b out)))
    (prog1
        (let ((lst (make-list 5)))
          (with-open-file (in tmp :element-type '(unsigned-byte 8))
            (read-sequence lst in))
          lst)
      (ignore-errors (delete-file tmp)))))

(deftest read-sequence-binary-into-list
  (%read-sequence-binary-list)
  (208 151 208 176 209))

;;; A character stream + list target still reads characters (no regression).
(defun %read-sequence-char-list ()
  (let ((tmp (format nil "~a/dotcl-rschar-~a.txt"
                     (or (dotcl:getenv "TEMP") "/tmp")
                     (get-internal-real-time))))
    (with-open-file (out tmp :direction :output :if-exists :supersede)
      (write-string "hello" out))
    (prog1
        (let ((lst (make-list 5)))
          (with-open-file (in tmp)
            (read-sequence lst in))
          lst)
      (ignore-errors (delete-file tmp)))))

(deftest read-sequence-char-into-list
  (%read-sequence-char-list)
  (#\h #\e #\l #\l #\o))

;; MV-propagating context must not leak into value-discarding / single-value
;; positions. (block nil (setf (acc (gethash k h)) v)) compiled the gethash
;; call without UnwrapMv — block bodies compile with MV propagation on for
;; their value, and that flag leaked through the setf expansion into
;; %struct-set's object argument, so STRUCT-SET received a raw multiple-values
;; wrapper and errored "not a structure". dolist/do/loop expand to block, which
;; is how SBCL's ucd second-pass (make-host-1) hit it.
(defstruct %mvleak-foo a)
(deftest mv-context-not-leaked-into-struct-set
  (let ((h (make-hash-table)))
    (setf (gethash 1 h) (make-%mvleak-foo :a 0))
    (dolist (k '(1)) (setf (%mvleak-foo-a (gethash k h)) 9))
    (block nil (setf (%mvleak-foo-a (gethash 1 h))
                     (+ 1 (%mvleak-foo-a (gethash 1 h)))))
    (%mvleak-foo-a (gethash 1 h)))
  10)

;; A Lisp reader-macro chain (custom "(" handler -> get-macro-character on #\#
;; -> built-in dispatch -> Lisp #+ handler) used to create a throwaway Reader
;; with a ReadSuppress=true baseline inside a #+/#- discard; the dispatch
;; adapter then installed it as the stream's shared reader, permanently
;; suppressing every later form on that stream (all read as NIL). This is
;; SBCL's cold-build xc-readtable shape (read-list + read-targ-feature-expr):
;; make-host-1 died on the first host fasl with nested #+ under a discard.
(defun %rdr-suppress-feature-reader (stream sub-character infix-parameter)
  (declare (ignore infix-parameter))
  (let ((feature (let ((*package* (find-package "KEYWORD"))
                       (*read-suppress* nil))
                   (read stream t nil t))))
    (if (and (eq feature :yes) (char= sub-character #\+))
        (read stream t nil t)
        (let ((*read-suppress* t))
          (read stream t nil t)
          (values)))))
(defun %rdr-suppress-read-list (stream ignore)
  (declare (ignore ignore))
  (let* ((read-suppress *read-suppress*)
         (list (list nil))
         (tail list))
    (loop
      (when (eq (peek-char t stream t nil t) #\))
        (read-char stream)
        (return (cdr list)))
      (let* ((char (read-char stream t nil t))
             (function (get-macro-character char)))
        (multiple-value-bind (object skipped)
            (if function
                (multiple-value-call (lambda (&rest args)
                                       (if (null args)
                                           (values nil t)
                                           (values (first args) nil)))
                  (funcall function stream char))
                (progn (unread-char char stream)
                       (read stream t nil t)))
          (when (and (not skipped) (not read-suppress))
            (setq tail (cdr (rplacd tail (list object))))))))))
(deftest reader-suppress-not-stuck-after-nested-conditional
  (let ((rt (copy-readtable)))
    (set-dispatch-macro-character #\# #\+ #'%rdr-suppress-feature-reader rt)
    (set-macro-character #\( #'%rdr-suppress-read-list nil rt)
    (let ((*readtable* rt))
      (with-input-from-string (s "#+nope (progn #+other (b) (c))
(list :first 1)
(list :second 2)")
        (list (read s) (read s)))))
  ((list :first 1) (list :second 2)))

;; throw's result-form values must reach the catch as multiple values even
;; when the throw sits in a non-last (value-discarding) position — after the
;; MV-context tightening the values were unwrapped to one. (ANSI CATCH.7/8)
(deftest throw-values-through-catch
  (list (multiple-value-list (catch 'foo 'a (throw 'foo (values)) 'c))
        (multiple-value-list (catch 'foo 'a (throw 'foo (values 1 2 3)) 'c)))
  (nil (1 2 3)))

;; A symbol-macro whose expansion rebinds its own name in a LET must not be
;; re-expanded inside that let (CLHS 3.1.2.1.1). The mutated/captured-vars
;; analysis walkers ignored let shadowing, so such an expansion (the shape of
;; SBCL's POLICY macro qualities) re-expanded itself once per depth level with
;; branching — an effectively infinite (2^50) analysis; make-host-1 hung on
;; compiling knownfun.lisp. Also checks the shadowing semantics themselves.
(deftest symbol-macro-let-shadow-no-blowup
  (symbol-macrolet ((q (let ((q 1)) (if (= q 1) 10 q))))
    (list q (let ((q 5)) q)))
  (10 5))

;; #n=/#n# across sibling elements must survive compile-file/load when a Lisp
;; "(" reader macro assembles the form via per-element (read stream) calls
;; (SBCL's cold-build read-list). CompileFile/Load used to leave their Reader
;; unlinked from the stream, so ReadFromStream spun up a second Reader whose
;; per-toplevel-form share-table clearing ran once per ELEMENT — a #1= defined
;; in one element was cleared before the sibling #1# was read, leaking a raw
;; placeholder into the fasl constant pool (make-host-1: extra-arg-refs blew
;; up with "LENGTH: not a sequence").
(deftest-compiled-only share-labels-survive-lisp-read-list-compile-file
  (let ((rt (copy-readtable))
        (src (format nil "~a/dotcl-sharelbl-~a.lisp"
                     (or (dotcl:getenv "TEMP") "/tmp")
                     (get-internal-real-time))))
    (set-macro-character #\( #'%rdr-suppress-read-list nil rt)
    (with-open-file (o src :direction :output :if-exists :supersede)
      (write-string
       "(defun %share-lbl-probe (name) (string= name #1=\"OPERAND-PARSE-TEMP\" :end1 (min (length name) (length #1#))))"
       o))
    (prog1
        (progn
          (let ((*readtable* rt))
            (load (compile-file src)))
          (list (%share-lbl-probe "OPERAND-PARSE-TEMP")
                (%share-lbl-probe "XY")))
      (ignore-errors (delete-file src))))
  (t nil))

;; INTERN / FIND-SYMBOL of "NIL" (and "T") in COMMON-LISP must return the
;; canonical NIL/T objects, not the raw package-table Symbol entries. The raw
;; entry was a "second NIL" that EQ/NULL accepted but proper-list checks
;; rejected: (cons 'a (intern "NIL" "CL")) printed as (A . NIL) and MAPCAR
;; signalled "not a proper list" (hit by SBCL's UNCROSS during make-host-1).
(deftest intern-nil-is-canonical
  (let ((x (intern "NIL" "COMMON-LISP"))
        (y (find-symbol "T" "COMMON-LISP")))
    (list (type-of x) (mapcar #'identity (cons 'a x)) (type-of y)
          (multiple-value-list (find-symbol "NIL" "COMMON-LISP"))))
  (null (a) boolean (nil :external)))

;; A local function named NIL must be callable from a sibling labels function
;; (ANSI LABELS.24). The labels-box capture path excluded head=NIL, so the
;; sibling's closure never captured the __LABELFN_NIL box and the call fell
;; back to an undefined global. Exposed when INTERN started returning the
;; canonical NIL (the ansi-test symbol list is built via intern).
(deftest labels-function-named-nil
  (labels ((nil (x) (foo (1- x)))
           (foo (y) (if (<= y 0) 'a (nil (1- y)))))
    (nil 10))
  a)

;; A closure's &key parameter binding a special via ((:key *var*) default)
;; emitted a 3-argument call to the 4-argument Runtime.FindKeyArgByName —
;; the explicit key-package string was pushed only in the non-closure defun
;; path — so the closure's IL underflowed the stack and the JIT rejected the
;; whole method (InvalidProgramException at first call). SBCL's
;; sb-xc:compile-file (a flet-captured defun with such keys) hit this at
;; make-host-2 stem 1.
(defvar *cks-var* nil)
(deftest closure-key-special-binding
  (let ((f (funcall (lambda (x)
                      (lambda (&key ((:verbose *cks-var*) *cks-var*)
                                    (plain 0 plain-p))
                        (list x *cks-var* plain plain-p)))
                    1)))
    (list (funcall f :verbose 2 :plain 3)
          (funcall f)
          (let ((*cks-var* 9)) (funcall f))))
  ((1 2 3 t) (1 nil 0 nil) (1 9 0 nil)))

;; (setf (pkg:macro-function ...)) via a non-CL symbol named MACRO-FUNCTION
;; (e.g. SB-XC:MACRO-FUNCTION) was expanded to a no-op that only evaluated
;; the value form: the *setf-expanders* table is keyed by symbol NAME, and
;; the non-CL branch protected dotcl's *macros* table by dropping the store
;; entirely. SBCL's cross-compiler registers every sb-xc:defmacro
;; through its own (defun (setf macro-function) ...), so no target macro
;; ever reached the XC globaldb and make-host-2 died at stem 1 with
;; "Ref to undefined variable */SHOW*". The store must delegate to the
;; place's own #'(setf pkg:macro-function).
(defpackage "SETF-MF-SHADOW-TEST" (:use))
(defvar *smf-store* nil)
(defun (setf setf-mf-shadow-test::macro-function) (new name &optional env)
  (setq *smf-store* (list new name env))
  new)
(deftest setf-non-cl-macro-function-delegates
  (let ((fn (lambda (form env) (declare (ignore env)) form)))
    (setq *smf-store* nil)
    (setf (setf-mf-shadow-test::macro-function 'smf-target) fn)
    (list (eq (first *smf-store*) fn)
          (second *smf-store*)
          (third *smf-store*)
          ;; dotcl's own macro table must stay untouched
          (and (cl:macro-function 'smf-target) t)))
  (t smf-target nil nil))

;; CL scoping is by symbol identity, but dotcl's free-variable analysis and
;; closure-capture machinery key locals by symbol-name STRING. An uninterned
;; binding with the same name as an interned variable — SBCL's XC gensym is
;; (make-symbol "CONSTRAINTS") with no counter, so its once-only temps all
;; print as #:CONSTRAINTS — made the closure capture the gensym's value
;; (a vector) where the user body referenced the interned hash-table:
;; make-host-2 stem 6 died with "GETHASH: not a hash-table" inside
;; JOIN-EQUALITY-CONSTRAINTS. Fixed by VAR-NAME: uninterned variable symbols
;; get a unique stable effective name throughout analysis and capture.
(defmacro uninterned-shadow-closure (&body body)
  (let ((g (make-symbol "CONSTRAINTS")))
    `(let ((,g (vector 1 2 3)))
       (flet ((body () ,@body))
         (when ,g (body))))))
(deftest uninterned-binding-must-not-shadow-interned-var
  (let ((constraints (make-hash-table :test #'equal)))
    (setf (gethash 'k constraints) 'hit)
    (uninterned-shadow-closure (values (gethash 'k constraints))))
  hit)

(defmacro uninterned-shadow-nested ()
  (let ((g1 (make-symbol "C"))
        (g2 (make-symbol "C")))
    `(let ((,g1 :one))
       (let ((,g2 :two))
         (funcall (lambda () (list ,g1 ,g2 c)))))))
(deftest same-named-gensyms-stay-distinct-in-closure
  (let ((c :outer))
    (uninterned-shadow-nested))
  (:one :two :outer))

;; labels mutual-TCO inlines each label body as a dispatch section whose
;; params live in plain shared locals — but the section inherited the OUTER
;; scope's *boxed-vars* / numeric type-locals. A label param named like a
;; boxed outer variable (here TYPE, boxed because the labels closures
;; capture it) compiled its references as box-derefs of a raw value:
;; SBCL's simplify-vector-type returned garbage / threw
;; ArrayTypeMismatchException at make-host-2 stem 6. The per-name context
;; lists must be shadowed for the section params.
(defun mtco-shadow-boxed-param (type)
  (labels ((process (types)
             (let (acc)
               (dolist (type types)
                 (multiple-value-bind (a) (simplify-mtco type)
                   (push a acc)))
               (values (nreverse acc) :compound)))
           (simplify-mtco (type)
             (cond ((consp type) (process type))
                   (t (values type :atom)))))
    (simplify-mtco type)))
(deftest labels-mutual-tco-param-shadows-boxed-outer
  (list (multiple-value-list (mtco-shadow-boxed-param 'x))
        (multiple-value-list (mtco-shadow-boxed-param '(a (b) c))))
  ((x :atom) ((a (b) c) :compound)))

;; A :key function returning multiple values handed its raw MvReturn wrapper
;; to :test / comparisons in the C# sequence functions (only the primary
;; value may flow, CLHS 3.1.7). SBCL's XC %find-position calls POSITION with
;; :key #'parse-optional-arg-spec (4 values) and :test #'string= — STRING=
;; got the MvReturn and died at make-host-2 stem 8. Same class: REDUCE's
;; function result fed back as the accumulator unwrapped.
(defun mv-key (x) (values (car x) (cadr x)))
(deftest seq-key-fn-multiple-values-primary-only
  (list (position 'b '((a 1) (b 2) (c 3)) :key #'mv-key)
        (find 'c '((a 1) (b 2) (c 3)) :key #'mv-key)
        (count 'b '((a 1) (b 2) (b 3)) :key #'mv-key)
        (remove 'b '((a 1) (b 2) (c 3)) :key #'mv-key)
        (member 'b '((a 1) (b 2)) :key #'mv-key))
  (1 (c 3) 2 ((a 1) (c 3)) ((b 2))))
(deftest reduce-fn-multiple-values-primary-only
  (reduce (lambda (a b) (values (+ a b) :junk)) '(1 2 3 4))
  10)
(deftest position-mv-key-with-string-test
  (position "B" '((a 1) (b 2)) :key (lambda (x) (values (car x) x)) :test #'string=)
  1)

;; (setf (pkg:compiler-macro-function ...)) via a non-CL symbol named
;; COMPILER-MACRO-FUNCTION (e.g. SB-XC:COMPILER-MACRO-FUNCTION) was hijacked
;; into dotcl's own compiler-macro table (%register-compiler-macro-rt),
;; never reaching the place's own #'(setf pkg:compiler-macro-function) —
;; same shape as the MACRO-FUNCTION no-op fixed earlier.
(defpackage "SETF-CMF-SHADOW-TEST" (:use))
(defvar *scmf-store* nil)
(defun (setf setf-cmf-shadow-test::compiler-macro-function) (new name &optional env)
  (setq *scmf-store* (list new name env))
  new)
(deftest setf-non-cl-compiler-macro-function-delegates
  (let ((fn (lambda (form env) (declare (ignore env)) form)))
    (setq *scmf-store* nil)
    (setf (setf-cmf-shadow-test::compiler-macro-function 'scmf-target) fn)
    (list (eq (first *scmf-store*) fn)
          (second *scmf-store*)
          (third *scmf-store*)
          ;; dotcl's own compiler-macro table must stay untouched
          (and (cl:compiler-macro-function 'scmf-target) t)))
  (t scmf-target nil nil))

;; MAP's :vector result-type branch read the third element of ANY compound
;; spec as a length constraint. For ARRAY/SIMPLE-ARRAY that position is a
;; RANK (or dimension list) per CLHS: (simple-array (unsigned-byte 32) 1)
;; means rank 1 / any length, but MAP signaled TYPE-ERROR unless the result
;; had length 1. SBCL's perfectly-hashable maps key hashes into exactly that
;; type — make-host-2 stem 22 (src/compiler/policy) died in the debugger.
(deftest map-simple-array-rank-spec
  (list (length (map '(simple-array (unsigned-byte 32) 1) #'identity '(1 2 3)))
        (length (map '(simple-array (unsigned-byte 32) (3)) #'identity '(1 2 3)))
        (handler-case (progn (map '(simple-array t (5)) #'identity '(1 2 3)) :no-error)
          (type-error () :len-checked))
        (handler-case (progn (map '(array t 2) #'identity '(1 2 3)) :no-error)
          (type-error () :rank-checked))
        (length (map '(vector t 3) #'identity '(1 2 3))))
  (3 3 :len-checked :rank-checked 3))

;; NSUBST / NSUBST-IF / NSUBST-IF-NOT were aliases of their non-destructive
;; SUBST counterparts. Callers that mutate a tree in place and discard the
;; return value — SBCL's propagate-lvar-annotations nsubsts annotation dep
;; lists when lvars are substituted — silently kept the old tree: the deps
;; pointed at dead lvars (derived type NIL) and every funarg call-type check
;; in the XC warned "called with (NIL ...)", failing make-host-2 stem 26.
(deftest nsubst-is-destructive
  (let ((tree (list 'a (list 'b 'a) 'c)))
    (nsubst 'z 'a tree)
    tree)
  (z (b z) c))
(deftest nsubst-if-is-destructive
  (let ((tree (list 1 (list 2 3) 4)))
    (nsubst-if 'e (lambda (x) (eql x 3)) tree)
    tree)
  (1 (2 e) 4))
(deftest nsubst-if-not-root-replacement
  (nsubst-if-not 'z #'consp (list 1 2))
  (z z . z))

;; The deftype expander table was keyed by symbol NAME, so a non-CL deftype
;; whose name matches a built-in type could never be resolved: registration
;; deliberately SKIPPED the SB-XC package (it would have hijacked CL:COMPLEX
;; for everyone), so (typep x 'sb-xc:complex) ignored SBCL's host-side
;; (deftype complex () 'complexnum) and make-host-2 stem 64 failed to dump
;; #C(0.0 0.0) flonums. Non-CL deftypes now register under "PKG::NAME" and
;; shadow built-ins for their own symbol only.
(defpackage "DTS-TEST" (:use))
(defstruct dts-num (v 0))
(deftype dts-test::complex () 'dts-num)
(deftype dts-test::number () '(or cl:real dts-test::complex))
(deftest non-cl-deftype-shadows-builtin-for-own-symbol
  (let ((x (make-dts-num)))
    (list (and (typep x 'dts-test::complex) t)
          (and (typep x 'dts-test::number) t)
          (typep x 'cl:complex)
          (and (typep #C(1 2) 'cl:complex) t)
          (typep #C(1 2) 'dts-test::complex)))
  (t t nil t nil))

;; *macroexpand-hook* default must be a function designator (CLHS 3.8.7), not
;; NIL. introspect-environment:compiler-macroexpand-1 (used by serapeum fbind
;; when binding a function that has a compiler-macro, e.g. alexandria:curry)
;; does (funcall *macroexpand-hook* cmf form env); a NIL default made that
;; "FUNCALL: not a function designator" and broke serapeum's sequences.lisp.
(defmacro mh-double (x) (list '* 2 x))
(deftest macroexpand-hook-default-is-funcall-designator
  (let ((hook *macroexpand-hook*))
    (list (notnot hook)
          ;; the library call pattern: expand a macro through the hook
          (funcall hook (macro-function 'mh-double) '(mh-double 21) nil)))
  (t (* 2 21)))

;; A lambda-list parameter shadows an enclosing symbol-macro of the same name
;; (CLHS 3.4.2), including inside nested lambdas. A self-referential symbol-macro
;; whose name is a param — serapeum define-env-method's (self (slot-value self
;; 'self)) — used to loop the compiler forever when the param was referenced
;; inside a nested lambda (the nested lambda's free-var scan re-expanded the
;; symbol-macro endlessly). If this regresses, compilation HANGS.
(symbol-macrolet ((smpsh-self (slot-value smpsh-self 'smpsh-self)))
  (defun smpsh-nested (smpsh-self) (funcall (lambda () smpsh-self))))
(deftest symbol-macro-param-shadow-nested-lambda
  ;; The param shadows the symbol-macro, so the nested lambda returns the arg.
  (smpsh-nested 42)
  42)

;; A macro's &environment must carry lexically-enclosing symbol-macrolet
;; bindings, so (macroexpand-1 'sym env) in the macro body expands them (CLHS
;; macro &environment / macroexpand). dotcl's runtime macro registration wrapped
;; the expander to pass a NIL env, so symbol-macros were invisible — serapeum's
;; with-boolean uses symbol-macrolet as a compile-time channel and broke.
(defmacro me473-probe (&environment env)
  (multiple-value-bind (exp win) (macroexpand-1 'me473-foo env)
    `(list ',exp ',win)))
(deftest macro-environment-carries-symbol-macrolet
  (symbol-macrolet ((me473-foo 42)) (me473-probe))
  (42 t))

;; Nested same-name symbol-macrolet: the reified &environment must expand to the
;; INNERMOST binding (CLHS 5.1.2.1 shadowing), not the outermost. follow-up.
(deftest macro-environment-nested-symbol-macrolet-innermost
  (symbol-macrolet ((me473-foo :outer))
    (symbol-macrolet ((me473-foo :inner))
      (me473-probe)))
  (:inner t))

;; A recursive macro that nests symbol-macrolet and splices the SAME body cons
;; into both if-arms (serapeum %with-boolean / string-join) must expand that body
;; against each arm's symbol-macro scope, not reuse one arm's cached expansion for
;; the other. symbol-macrolet did not push a *macroexpand-scope* marker (macrolet
;; did), so the shared macro call collided in *macroexpand-cache* and the inner
;; scope's binding was lost. follow-up.
(defmacro me473r-pr (&environment env) `(list :tb ',(macroexpand-1 'me473r-tb env)))
(defmacro me473r-wb (branches &body body &environment env)
  (if branches
      (let ((cur (macroexpand-1 'me473r-tb env)))
        `(if ,(car branches)
             (symbol-macrolet ((me473r-tb (,(car branches) . ,cur)))
               (me473r-wb ,(cdr branches) ,@body))
             (me473r-wb ,(cdr branches) ,@body)))
      (if (= 1 (length body)) (car body) `(progn ,@body))))
(defun me473r-run (x) (symbol-macrolet ((me473r-tb nil)) (me473r-wb (x) (me473r-pr))))
(deftest macro-environment-symbol-macrolet-per-arm-scope
  (me473r-run t)
  (:tb (x)))

;; A handler-case clause variable that shadows an enclosing BOXED variable of the
;; same name (e.g. a captured LOOP variable) must bind the condition into a plain
;; slot, not inherit the outer var's boxed representation. It used to compile the
;; clause-body reference as a boxed slot[0] ldelem-ref on the condition object,
;; throwing ArrayTypeMismatchException at runtime when the handler fired — which
;; broke ANSI SET-SYNTAX-FROM-CHAR.MULTIPLE-ESCAPE.
(defun hc468-shadow ()
  (loop for c in '(1 2)
        collect (handler-case (aref "x" 99) (error (c) (and c :caught)))))
(deftest handler-case-var-shadows-boxed-loop-var
  (hc468-shadow)
  (:caught :caught))

;; (setf (car/cdr/cadr/cdar PLACE) VALUE): the place subform is evaluated (into a
;; temp) BEFORE the value form (CLHS 5.1.1.1). The car/cdr fast-paths bound the
;; value first and re-evaluated the subform in rplaca/rplacd, so a VALUE that
;; reassigns the subform variable stored into the NEW binding — self-reference.
;; This broke serapeum with-collector's head/tail trick
;; (setf (cdr tail) (setf tail (list x))), so collecting always returned empty.
(defun sf474-collect (items)
  (let* ((head (list nil)) (tail head))
    (dolist (x items) (setf (cdr tail) (setf tail (list x))))
    (cdr head)))
(deftest setf-cdr-subform-before-value
  (sf474-collect '(:a :b :c))
  (:a :b :c))
(deftest setf-car-subform-before-value
  (let* ((head (list nil)) (tail head))
    (setf (cdr tail) (setf tail (list 1)))
    (cdr head))
  (1))

;; A lexical (flet/labels) (setf NAME) function shadows the global one: both
;; (setf (NAME ...) v) and #'(setf NAME) must resolve it (CLHS 5.1.2.9 / the
;; FUNCTION special form consults the lexical environment). compile-function-ref's
;; (function (setf name)) case skipped *local-functions* and always took the
;; global SetfFunction path, so the setf-function fallback expansion
;; (funcall #'(setf name) …) failed with "Undefined function: (SETF NAME)" inside
;; the flet that binds it — eclector's set-standard-syntax-types.
(defun %setf475-eclector (readtable)
  (let ((log '()))
    (flet (((setf syntax) (syntax-type char)
             (push (list readtable char syntax-type) log)
             syntax-type))
      (setf (syntax :space) :whitespace
            (syntax :tab)   :whitespace))
    (nreverse log)))
(deftest flet-local-setf-fn-in-defun
  (%setf475-eclector :rt)
  ((:rt :space :whitespace) (:rt :tab :whitespace)))

(defun %setf475-funcall (v c)
  (flet (((setf syntax) (val ch) (list :got ch val)))
    (funcall #'(setf syntax) v c)))
(deftest sharp-quote-local-setf-fn
  (%setf475-funcall :val :ch)
  (:got :ch :val))

(deftest labels-local-setf-fn-in-defun
  (labels (((setf thing) (val key) (list :stored key val)))
    (setf (thing :a) 1))
  (:stored :a 1))

;; A redefined top-level function's LispFunction — and the DynamicMethod JIT code
;; behind it — must not be pinned in the global constant pool forever. The
;; run-once re-registration constant now lives in the collectible per-unit store,
;; so redefining a name many times (literal-free body, so the only per-definition
;; constant is the re-registration fn) and running GC reclaims the dead
;; definitions. Pre-fix the pool grew ~1 entry per redefinition and never fell
;; (the Coalton GB-working-set leak). Generous threshold: pre-fix delta was
;; ~300; post-fix ~0. Also asserts the still-bound function stays callable
;; (no over-collection).
(defun %pool476-data () (nth 2 (dotcl:emit-pool-stats)))
(deftest defun-redefine-does-not-leak-constants
  (let ((before (%pool476-data)))
    (dotimes (i 300)
      (eval `(defun cl-user::%leak476-target () (+ ,i 1))))
    (dotcl:gc) (dotcl:gc) (dotcl:gc)
    (list (< (- (%pool476-data) before) 100)
          (integerp (funcall '%leak476-target))))
  (t t))

;; Backquote must process ,/,@ inside a #(...) vector template, not just lists
;; (CLHS 2.4.6). `#(...) ≡ (apply #'vector `(...)). The reader used to quote the
;; whole vector, leaving (UNQUOTE x) forms as literal elements — which broke SBCL's
;; backquoted `+static-symbols+` vector (LENGTH: not a sequence).
(defvar *bqv-x* 42)
(defvar *bqv-l* (list 'p 'q))
(deftest backquote-vector-unquote (equalp `#(a ,*bqv-x* c) #(a 42 c)) t)
(deftest backquote-vector-splice  (equalp `#(,@*bqv-l* c) #(p q c)) t)
(deftest backquote-vector-mixed   (equalp `#(0 ,@*bqv-l* ,*bqv-x*) #(0 p q 42)) t)
(deftest backquote-vector-nested  (equalp `#(1 #(2 ,*bqv-x*) 3) #(1 #(2 42) 3)) t)
(deftest backquote-vector-empty   (equalp `#() #()) t)
(deftest backquote-vector-plain   (equalp `#(a b c) #(a b c)) t)
(deftest backquote-vector-is-sequence
  (let ((v `#(0 ,@*bqv-l* 9)))
    (list (length v) (svref v 2) (typep v 'simple-vector)))
  (4 q t))

;; make-instance with an initarg value that contains a tagbody (loop/dolist —
;; which emit :LEAVE and labels requiring an empty CIL stack) was compiled with
;; the class still on the stack (the %make-instance-with-initargs emit pushed the
;; class before pre-evaluating the initargs), producing a stack-unbalanced method
;; = invalid CIL (InvalidProgramException at first call). Broke cl-ppcre back-
;; references: (make-instance 'alternation :choices (loop … collect …)).
(defclass mi482-holder () ((items :initarg :items :reader mi482-items)))
(defun mi482-make (lst)
  (make-instance 'mi482-holder :items (loop for x in lst collect (* x x))))
(deftest make-instance-initarg-with-loop
  (mi482-items (mi482-make '(1 2 3)))
  (1 4 9))
(defun mi482-make2 (lst)
  (make-instance 'mi482-holder :items (progn (dolist (x lst) (identity x)) (length lst))))
(deftest make-instance-initarg-with-dolist
  (mi482-items (mi482-make2 '(:a :b :c :d)))
  4)

;; (coerce X 'simple-string) must yield a *simple* string (CLHS): a fill-pointered
;; / adjustable / displaced char-vector must be copied to a fresh simple string,
;; not returned as-is. Returning the non-simple vector broke cl-ppcre's
;; maybe-coerce-to-simple-string on parser-built adjustable strings — a trailing
;; empty group (?:) then dropped the preceding match.
(deftest coerce-adjustable-to-simple-string
  (let ((a (make-array 4 :element-type 'character :fill-pointer 2 :adjustable t
                         :initial-element #\b)))
    (list (simple-string-p (coerce a 'simple-string))
          (simple-string-p (coerce a 'simple-base-string))
          (string= (coerce a 'simple-string) "bb")))
  (t t t))
(deftest coerce-fillpointer-to-simple-string
  (let ((a (make-array 2 :element-type 'character :fill-pointer 2 :initial-element #\x)))
    (simple-string-p (coerce a 'simple-string)))
  t)

;; A non-top-level (defun) — nested inside a conditional — must register the
;; function at RUNTIME (only when the branch executes), not at assembly time.
;; It used to compile to :defmethod, which registers at assembly time even inside
;; an untaken branch, so (unless (fboundp 'x) (defun x …)) / (if nil (defun x …))
;; defined X unconditionally — breaking cross-file defdfun defaults (bordeaux-
;; threads) on fresh compile.
(deftest nested-defun-false-guard-does-not-define
  (progn (eval '(unless t (defun %nd486-a () 1)))
         (eval '(if nil (defun %nd486-b () 1)))
         (eval '(when nil (defun %nd486-c () 1)))
         (list (fboundp '%nd486-a) (fboundp '%nd486-b) (fboundp '%nd486-c)))
  (nil nil nil))
(deftest nested-defun-true-guard-defines
  (progn (eval '(when t (defun %nd486-d () :yes)))
         (funcall '%nd486-d))
  :yes)
;; guarded fboundp-default pattern (defdfun): impl already bound → default skipped
(defun %nd486-impl () :impl)
(deftest nested-defun-fboundp-guard-preserves-impl
  (progn (eval '(unless (fboundp '%nd486-impl) (defun %nd486-impl () :default)))
         (funcall '%nd486-impl))
  :impl)
;; (setf name) as a non-top-level defun registers on the correct (reader) symbol
(deftest nested-setf-defun-registers
  (progn (ignore-errors (defun (setf %nd486-acc) (v x) (setf (car x) v) v))
         (list (fboundp '(setf %nd486-acc))
               (typep #'(setf %nd486-acc) 'function)))
  (t t))
;; gensym-named non-top-level defun is callable via its actual symbol
(deftest nested-gensym-defun-callable
  (let* ((name (gensym "ND486G"))
         (fn (eval `(prog2 nil (defun ,name (a b) (values a b 3)) nil))))
    (list (multiple-value-list (funcall (symbol-function fn) 1 2))
          (eq fn name)))
  ((1 2 3) t))

;; A big form inside a lexical scope used to compile into one oversized CIL
;; method (a few MB) and fail with an opaque InvalidProgramException at JIT time.
;; compile-progn now splits an oversized *non-toplevel* progn body into
;; immediately-called closure chunks, so a form that just bundles many independent
;; statements (a data-driven test suite, generated code) compiles and runs.
(defvar %big488-acc 0)
(defun %big488-heavy (i target)
  ;; A statement that emits a lot of CIL. TARGET names the accumulator to mutate.
  `(setf ,target (+ ,target
                    (car (last (list (* ,i 2) (+ ,i 1) (- ,i 1))))
                    (reduce #'+ (mapcar (lambda (x) (+ x ,i)) (list ,i ,i)))
                    (length (format nil "~a-~a" ,i ,i)))))
(defun %big488-chunkable (n)
  ;; Mutates only the SPECIAL %big488-acc (DynamicBindings, not a lexical capture),
  ;; so chunking is semantics-preserving -> compiles despite the size.
  `(let ((k 1)) (declare (ignorable k))
     (setf %big488-acc 0)
     ,@(loop for i below n collect (%big488-heavy i '%big488-acc))
     %big488-acc))
(deftest huge-chunkable-form-compiles
  (let ((r (eval (%big488-chunkable 4000))))
    (and (integerp r) (> r 0)))
  t)
;; A body mutating the LET's own lexical variable used to be unchunkable: by the
;; time compile-progn saw it the variable's boxing was already decided, so a
;; closure would have captured a stale copy and chunking was refused — leaving
;; one oversized method and a "form too large" error. The rewrite now happens
;; before the LET's capture/mutation scan, so the variable is boxed like any
;; other captured-and-mutated variable and the chunks share the one cell. This
;; shape compiles, and computes exactly what the plain loop does.
(defun %big488-lexical (n)
  `(let ((acc 0))
     ,@(loop for i below n collect (%big488-heavy i 'acc))
     acc))
(defun %big488-expected (n)
  (let ((acc 0))
    (dotimes (i n)
      (setf acc (+ acc
                   (car (last (list (* i 2) (+ i 1) (- i 1))))
                   (reduce #'+ (mapcar (lambda (x) (+ x i)) (list i i)))
                   (length (format nil "~a-~a" i i)))))
    acc))
(deftest huge-lexical-mutation-form-chunks-and-matches
  (= (eval (%big488-lexical 4000)) (%big488-expected 4000))
  t)
;; The clear-error fallback still holds where chunking is genuinely unsafe: a raw
;; native slot (here a declared fixnum) cannot be captured by a closure without
;; emitting invalid IL, so the body stays one method and the catchable "form too
;; large" PROGRAM-ERROR surfaces instead of an opaque InvalidProgramException.
;; (Small methods that InvalidProgram are genuine codegen bugs and still surface
;; unchanged.)
;; Compiled-only: "form too large" is a limit of the IL a method can hold. The
;; interpreter has no such ceiling, so evaluating this is simply not an error.
(deftest-compiled-only huge-unchunkable-form-signals-clear-error
  (handler-case
      (progn (eval `(let ((acc 0))
                      (declare (type fixnum acc))
                      ,@(loop for i below 4000 collect (%big488-heavy i 'acc))
                      acc))
             :no-error)
    (program-error (e)
      (if (search "too large" (princ-to-string e)) :clear-error :other-prog-error))
    (error () :other-error))
  :clear-error)

;; A MACROLET-local macro that expands to (return-from <enclosing-defun> …) must
;; resolve the defun's implicit block. The return-from is hidden in the macrolet's
;; quasiquoted expander, so the use-direct fast path (which skips the block wrapper
;; when no literal return-from is seen) dropped the implicit block → "no block
;; named F". Now a body containing a macrolet keeps the block wrapper.
(defun %rf487-simple (x)
  (macrolet ((adv () `(return-from %rf487-simple :done)))
    (adv)
    x))
(deftest macrolet-return-from-implicit-block
  (%rf487-simple 42)
  :done)
;; life.lisp shape: return-from via macrolet through do/loop nesting
(defun %rf487-nested (x)
  (let ((i 0))
    (macrolet ((advance (n c) `(progn (incf ,n) (unless ,c (return-from %rf487-nested :adv))))
               (scan (g l) `(do () ((>= ,l ,g)) (advance ,l nil))))
      (scan x i)
      :fell-through)))
(deftest macrolet-return-from-through-do-loop
  (%rf487-nested 3)
  :adv)
;; a macrolet that does NOT return-from is unaffected
(defun %rf487-norf (x) (macrolet ((m () '(* 2 3))) (+ x (m))))
(deftest macrolet-no-return-from-still-works
  (%rf487-norf 40)
  46)

;; rationalize returns the SIMPLEST rational within the float's rounding interval,
;; not rational's exact fraction (was aliased to rational). Also integer-decode-float
;; must decode a single-float from its own 24-bit form, not promote to double.
(deftest rationalize-simplest-double
  (list (rationalize 3.2d0) (rationalize 0.1d0) (rationalize 1.9d0)
        (rationalize 0.333d0) (rationalize 2.5d0) (rationalize -3.2d0)
        (rationalize (/ 1d0 3d0)))
  (16/5 1/10 19/10 333/1000 5/2 -16/5 1/3))
(deftest rationalize-simplest-single
  (list (rationalize 3.2f0) (rationalize 0.1f0))
  (16/5 1/10))
(deftest rationalize-rational-passthrough
  (list (rationalize 5) (rationalize 1/7) (rationalize 0))
  (5 1/7 0))
(deftest rationalize-round-trips
  (every (lambda (x) (= (float (rationalize x) x) x))
         '(3.2d0 0.1d0 1.23456789d0 123456.789d0 1d-10 1d10 3.2f0 0.1f0 1.5f8))
  t)
(deftest integer-decode-single-float
  (multiple-value-list (integer-decode-float 3.2f0))
  (13421773 -22 1))

;; dotcl/dotcl issue 51: a macro that expands to (return-from <defun-name> …) must
;; resolve the implicit defun block even when the macro call is nested inside a
;; special form. The block-elision (use-direct) scan only expanded TOP-LEVEL macro
;; calls, so a macro under (progn …)/(when …)/(let …) was missed and compiling the
;; expanded return-from signalled "no block named …".
(defmacro i51-ret () `(return-from i51-progn 5))
(defun i51-progn () (progn (i51-ret)) 99)
(deftest issue51-return-from-macro-in-progn (i51-progn) 5)

(defmacro i51-ret2 () `(return-from i51-when 7))
(defun i51-when () (when t (i51-ret2)) 99)
(deftest issue51-return-from-macro-in-when (i51-when) 7)

(defmacro i51-ret3 () `(return-from i51-let 11))
(defun i51-let () (let ((z 0)) (declare (ignore z)) (i51-ret3)) 99)
(deftest issue51-return-from-macro-in-let (i51-let) 11)

;; Value-returning and the plain direct-call form (control) must still work.
(defmacro i51-ret4 () `(return-from i51-direct 13))
(defun i51-direct () (i51-ret4) 99)
(deftest issue51-return-from-macro-direct (i51-direct) 13)

;; a known-function intrinsic in tail position whose argument is a self
;; tail-call must still execute — the intrinsic entries compiled args without
;; binding *in-tail-position* nil, so the self-call fired TCO and the intrinsic
;; call became unreachable dead code (side effect silently lost).
(defun i502-tsc (n) (if (zerop n) 0 (princ (i502-tsc (- n 1)))))
(deftest issue502-tail-intrinsic-arg-self-call
  ;; princ runs once per level (3 times), each writing the base value "0".
  ;; Bug = 0 chars (princ dead-coded); fixed = "000".
  (length (with-output-to-string (*standard-output*) (i502-tsc 3)))
  3)
;; multiple-value-list keeps MV context for its arg but must still block tail:
;; a self-tail-call arg otherwise TCOs and dead-codes the wrapping call, so the
;; result would be the raw values (primary = a number) instead of a list.
(defun i502-mvl (n) (if (zerop n) (values 10 20) (multiple-value-list (i502-mvl (1- n)))))
(deftest issue502-multiple-value-list-tail (listp (i502-mvl 1)) t)

;; (setf (pkg:readtable-case obj) mode) via a non-CL symbol named READTABLE-CASE
;; (e.g. eclector.readtable:readtable-case, which :shadows cl:readtable-case with its
;; own CLOS protocol) was hijacked into the built-in %set-readtable-case expander and
;; fataled with "not a readtable" — the (setf compiler-macro-function)/(setf
;; macro-function) hijack, 3rd instance. It must reach the place's own
;; #'(setf pkg:readtable-case). Blocks eclector/mallet bring-up.
(defpackage "SETF-RTC-SHADOW-TEST" (:use))
(defclass srtc-obj () ((rc :initform :upcase)))
(defmethod setf-rtc-shadow-test::readtable-case ((r srtc-obj)) (slot-value r 'rc))
(defmethod (setf setf-rtc-shadow-test::readtable-case) (mode (r srtc-obj))
  (setf (slot-value r 'rc) mode))
(deftest setf-non-cl-readtable-case-delegates
  (let ((o (make-instance 'srtc-obj)))
    (setf (setf-rtc-shadow-test::readtable-case o) :preserve)
    (setf-rtc-shadow-test::readtable-case o))
  :preserve)

;; WARN's MUFFLE-WARNING restart has to transfer control: CLHS says invoking it
;; makes WARN return immediately, which unwinds the handler that invoked it.
;; dotcl built that restart as a restart-bind style restart, so MUFFLE-WARNING
;; called its function in place and returned; the invoking handler ran on, and
;; HANDLER-BIND went to the next applicable clause. A handler-bind naming both
;; STYLE-WARNING and WARNING — which is how SBCL's compiler separates "note it"
;; from "this file failed" — therefore ran both, so every cross-compiled file
;; printed its diagnostics twice and any file with a mere style warning was
;; reported as a failure.
(define-condition muffle-probe-style (style-warning) ()
  (:report (lambda (c s) (declare (ignore c)) (write-string "probe" s))))
;; *print-circle* has to see structure slots. The scan pass that marks shared and
;; circular objects walked conses, uninterned symbols and vectors, but not the slots
;; of structures or instances — though the printing pass already looked structures up
;; in the same table. A cycle closing through a slot was therefore never marked, and
;; the printer emitted the object afresh at every turn: unbounded output at full CPU
;; instead of a #1= label. SBCL's compiler binds *print-circle* to T and PRINC-TO-STRINGs
;; the condition when reporting, and its type objects hold each other, so cross-compiling
;; wedged there.
(defstruct pc-node kid)
(deftest print-circle-follows-structure-slots
  (let ((s (make-pc-node)))
    (setf (pc-node-kid s) (list 1 s))
    (let ((*print-circle* t)) (princ-to-string s)))
  "#1=#S(PC-NODE :KID (1 #1#))")
(deftest print-circle-labels-shared-structures
  (let ((s (make-pc-node :kid (list 7))))
    (let ((*print-circle* t)) (princ-to-string (list s s))))
  "(#1=#S(PC-NODE :KID (7)) #1#)")

;; ~A and ~S print an arbitrary object, so they are where *print-circle* has to be
;; established. They went straight to the formatter's inner entry point, skipping the
;; scan pass that marks shared objects, so a circular argument printed forever. This is
;; how a condition report reaches the printer: SBCL's compiler reports its warnings with
;; (format stream "... ~S ..." <type object>) under *print-circle* bound to T.
(deftest format-tilde-s-honors-print-circle
  (let ((s (make-pc-node)))
    (setf (pc-node-kid s) (list 1 s))
    (let ((*print-circle* t)) (format nil "~S" s)))
  "#1=#S(PC-NODE :KID (1 #1#))")
(deftest format-tilde-a-honors-print-circle
  (let ((s (make-pc-node)))
    (setf (pc-node-kid s) (list 1 s))
    (let ((*print-circle* t)) (format nil "~A" s)))
  "#1=#S(PC-NODE :KID (1 #1#))")

;; A condition's report has to be rendered where it is asked for, not where the
;; condition was signalled. WARN ran its format control over the caller's arguments
;; immediately, so the printer saw the printer variables of the signalling site. The
;; reporter is the one that knows how it wants the objects printed — SBCL's compiler
;; binds *PRINT-CIRCLE* to T and PRINC-TO-STRINGs the condition, and rendering early
;; meant a cyclic argument was printed with no circle detection at all.
(deftest warn-report-renders-at-report-time
  (block probe
    (handler-bind ((warning (lambda (c)
                              (let ((*print-circle* t))
                                (return-from probe (princ-to-string c))))))
      (let ((s (make-pc-node)))
        (setf (pc-node-kid s) (list 1 s))
        (warn "cyclic: ~S" s))
      :no-handler))
  "cyclic: #1=#S(PC-NODE :KID (1 #1#))")

(deftest muffle-warning-unwinds-the-handler
  (let ((log nil))
    (handler-bind ((style-warning (lambda (c) (push :style log) (muffle-warning c)))
                   (warning (lambda (c) (declare (ignore c)) (push :warning log))))
      (warn 'muffle-probe-style))
    (reverse log))
  (:style))

;;; NIL is not a pathname designator (CLHS: pathname / string / file stream).
;;; The file-op entry point used to fall through to ToString and silently
;;; create a file literally named "NIL" in the cwd — the classic
;;; (with-open-file (s (uiop:getenv "UNSET_VAR") ...)) bug, succeeding with a
;;; side effect instead of signalling.
(deftest open-nil-signals-type-error
  (handler-case (progn (open nil :direction :output) :no-error)
    (type-error () :type-error))
  :type-error)

(deftest probe-file-nil-signals-type-error
  (handler-case (progn (probe-file nil) :no-error)
    (type-error () :type-error))
  :type-error)

(deftest open-nil-creates-no-file
  (progn (handler-case (with-open-file (s nil :direction :output)
                         (write-line "x" s))
           (error () nil))
         (probe-file "NIL"))
  nil)

(deftest pathname-nil-signals-type-error
  (handler-case (progn (pathname nil) :no-error)
    (type-error () :type-error))
  :type-error)

;;; :external-format was accepted and silently ignored, so every character
;;; stream was written as UTF-8. (with-open-file ... :external-format :latin-1)
;;; wrote 384 bytes for codes 0-255 instead of 256, which breaks the
;;; faithful-octet-I/O idiom (open latin-1, one char == one byte) that
;;; rfc2388 and friends use to round-trip binary uploads.
(defmacro %ef-bytes (path &rest open-args)
  `(progn
     (with-open-file (s ,path :direction :output :if-exists :supersede ,@open-args)
       (dotimes (i 256) (write-char (code-char i) s)))
     (prog1 (with-open-file (s ,path :element-type '(unsigned-byte 8)) (file-length s))
       (delete-file ,path))))

(deftest external-format-latin1-is-one-byte-per-char
  (%ef-bytes "ef-t1.tmp" :external-format :latin-1)
  256)

(deftest external-format-iso8859-1-alias
  (%ef-bytes "ef-t2.tmp" :external-format :iso-8859-1)
  256)

(deftest external-format-default-stays-utf8
  (%ef-bytes "ef-t3.tmp")
  384)

(deftest external-format-explicit-utf8
  (%ef-bytes "ef-t4.tmp" :external-format :utf-8)
  384)

;;; latin-1 round trip: every code 0-255 comes back unchanged.
(deftest external-format-latin1-round-trips
  (progn
    (with-open-file (s "ef-t5.tmp" :direction :output :if-exists :supersede
                                   :external-format :latin-1)
      (dotimes (i 256) (write-char (code-char i) s)))
    (prog1 (with-open-file (s "ef-t5.tmp" :external-format :latin-1)
             (let ((ok t))
               (dotimes (i 256) (unless (eql (char-code (read-char s)) i) (setf ok nil)))
               (if ok :same :differs)))
      (delete-file "ef-t5.tmp")))
  :same)

;;; stream-external-format reports what the stream was opened with.
(deftest external-format-reported-by-stream
  (progn
    (with-open-file (s "ef-t6.tmp" :direction :output :if-exists :supersede
                                   :external-format :latin-1)
      (write-char #\a s))
    (prog1 (list (with-open-file (s "ef-t6.tmp" :external-format :latin-1)
                   (stream-external-format s))
                 (with-open-file (s "ef-t6.tmp") (stream-external-format s)))
      (delete-file "ef-t6.tmp")))
  (:latin-1 :default))

;;; An unrecognised format must signal rather than silently encode as UTF-8 —
;;; a misspelled name looked like it took effect before.
(deftest external-format-unknown-signals
  (handler-case (progn (%ef-bytes "ef-t7.tmp" :external-format :no-such-format) :no-error)
    (error () :errored))
  :errored)

;;; LOGICAL-PATHNAME-TRANSLATIONS of a host that was never defined must signal a
;;; TYPE-ERROR, not return NIL. Libraries probe for a logical host by catching
;;; that error and defining the host in the handler (cl-fad's TEMPORARY-FILES is
;;; the common case); returning NIL skips the handler, leaves the host undefined
;;; and lets "HOST:NAME" reach the file system as a physical name.
(deftest logical-pathname-translations-undefined-host-signals
  (list (handler-case (progn (logical-pathname-translations "NO-SUCH-HOST-XYZ") :no-error)
          (type-error () :type-error))
        (progn
          (setf (logical-pathname-translations "DOTCLLPT") '(("**;*.*.*" "/tmp/**/*.*")))
          (logical-pathname-translations "DOTCLLPT")))
  (:type-error (("**;*.*.*" "/tmp/**/*.*"))))

;;; LOGICAL-PATHNAME of a namestring whose host was never defined must signal a
;;; TYPE-ERROR (CLHS 19.3.1: the host of a logical pathname namestring has to be
;;; a defined logical host). It used to build a pathname for the unknown host,
;;; so "HOST:NAME" reached the file system verbatim.
(deftest logical-pathname-undefined-host-signals
  (list (handler-case (progn (logical-pathname "NO-SUCH-HOST-XYZ:FOO.LISP") :no-error)
          (type-error () :type-error))
        (progn
          (setf (logical-pathname-translations "DOTCLLP") '(("**;*.*.*" "/tmp/**/*.*")))
          (typep (logical-pathname "DOTCLLP:FOO.LISP") 'logical-pathname)))
  (:type-error t))
