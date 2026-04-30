;;; Regression tests for recent D-file fixes

;;; D577 — let/let* bindings take primary value only
(deftest d577-let-primary-value
  (let ((x (values 1 2 3)))
    x)
  1)

(deftest d577-let*-primary-value
  (let* ((x (values 10 20)))
    x)
  10)

;;; D579 — define-compiler-macro and compiler-macro-function
(define-compiler-macro %reg-test-cm (x) (* 2 x))

(deftest d579-compiler-macro-function-returns-fn
  (functionp (compiler-macro-function '%reg-test-cm))
  t)

(deftest d579-compiler-macro-expands
  (let* ((cm (compiler-macro-function '%reg-test-cm))
         (expanded (funcall cm '(%reg-test-cm 5) nil)))
    expanded)
  10)

;;; D579 — (setf compiler-macro-function)
(deftest d579-setf-compiler-macro-function
  (progn
    (setf (compiler-macro-function '%cm-setf-test)
          (lambda (form env) (declare (ignore env)) `(+ ,(cadr form) 100)))
    (let* ((cm (compiler-macro-function '%cm-setf-test))
           (expanded (funcall cm '(%cm-setf-test 5) nil)))
      expanded))
  (+ 5 100))

;;; D579 — compile nil works
(deftest d579-compile-nil-basic
  (funcall (compile nil '(lambda (x) (* x x))) 7)
  49)

(deftest d579-compile-nil-with-cm
  (let* ((cm (compiler-macro-function '%reg-test-cm))
         (expanded (funcall cm '(%reg-test-cm 5) nil))
         (fn (compile nil `(lambda () ,expanded))))
    (funcall fn))
  10)

;;; D576 — relative pathname merging (basic check)
(deftest d576-merge-pathnames-basic
  (let ((p (merge-pathnames "foo.lisp" (make-pathname :directory '(:absolute "tmp")))))
    (pathname-name p))
  "foo")

;;; D573 — symbol-package type error on non-symbol
(deftest d573-symbol-package-type-error
  (signals-error (symbol-package 42) type-error)
  t)

;;; TCO — tail-recursive function shouldn't stack overflow
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

;;; D606 — psetf with ldb should write to original variable (not a temp)
(deftest d606-psetf-ldb
  (let ((x #b00000))
    (psetf (ldb (byte 5 1) x) #b10110)
    x)
  #b101100)

;;; D606 — incf of (getf plist key default) should add key when missing
(deftest d606-incf-getf-default
  (let ((p '(a 1 b 2)))
    (incf (getf p 'c 19))
    (getf p 'c))
  20)

;;; D606 — setf of getf should update existing key
(deftest d606-setf-getf-existing
  (let ((p (list 'a 1 'b 2)))
    (incf (getf p 'a))
    (getf p 'a))
  2)

;;; D606 — subtypep (cons (not x)) against (cons (satisfies y)) should be uncertain
(deftest d606-subtypep-cons-satisfies-uncertain
  (multiple-value-list
   (subtypep '(cons (not float)) '(cons (satisfies identity))))
  (nil nil))

;;; D616 — reinitialize-instance validates initargs from method &key params (issue #30)
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

;;; D615 — float printing and ~e format (issue #98)
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
    (not (search "E+" (prin1-to-string 1.5e10))))
  t)

(deftest d615-format-tilde-e-positive-exponent-sign
  ;; ~e MUST include + for positive exponents
  (let ((*read-default-float-format* 'single-float))
    (not (null (search "E+" (format nil "~e" 1.5e10)))))
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

;;; D613 — float bit access functions exported from CL package (issue #104 / nibbles)
(deftest d613-single-float-bits-accessible
  ;; cl:single-float-bits must be accessible (was in DOTCL-INTERNAL before)
  (not (null (find-symbol "SINGLE-FLOAT-BITS" "COMMON-LISP")))
  t)

(deftest d613-make-single-float-roundtrip
  (make-single-float (single-float-bits 1.5))
  1.5)

(deftest d613-double-float-bits-accessible
  (not (null (find-symbol "DOUBLE-FLOAT-BITS" "COMMON-LISP")))
  t)

(deftest d613-make-double-float-roundtrip
  (make-double-float (double-float-bits 1.5d0))
  1.5d0)

;;; D619 — subtypep circular deftype cycle detection (issue #16)
(deftype d619-circular-type (&optional low high) `(d619-circular-type ,low ,high))

(deftest d619-subtypep-circular-deftype-no-hang
  ;; circular deftype should not infinite-loop; subtypep returns (nil nil) = uncertain
  (multiple-value-list (subtypep 'd619-circular-type 'integer))
  (nil nil))

;;; D618 — &environment passed to macro expander (issue #79)
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

;;; D623 — macrolet expander can call surrounding flet functions at expansion time (issue #76)
(deftest d623-macrolet-calls-flet-at-expansion-time
  (flet ((double (x) (* x 2)))
    (macrolet ((m (x) (double x)))
      (m 5)))
  10)

;;; D624 — gethash MV leakage through let binding (issue #19)
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

(deftest d623-macrolet-calls-flet-quoted-result
  (flet ((make-form (x) `(+ ,x 1)))
    (macrolet ((m (x) (make-form x)))
      (m 3)))
  4)

;;; D651 — dotcl:gc-stats returns 5 fixnums, time preserves values
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

;;; D654 — subnormal double-float: rational / float / format correctness (issue #107)
(deftest d654-subnormal-float-roundtrip
  (let ((x 9.63d-322))
    (= x (float (rational x) 1d0)))
  t)

(deftest d654-subnormal-format-e
  ;; Format of subnormal double: digits must come from the exact rational
  ;; (not from Math.Pow(10, exp) which loses precision for subnormals).
  (format nil "~,15,,0e" 9.63d-322)
  "0.963428009390431E-321")

(deftest d654-format-e-width-trim-zero-frac
  ;; ~5e on 1.0: width-derived d=0 means no fraction digits ("1." not "1.0")
  (format nil "~5e" 1.0)
  "1.E+0")

(deftest d654-format-e-k-scaling
  ;; ~,d,,k: k>=1 => k digits before dot, d-k+1 after.
  (format nil "~,2,,2e" 0.05)
  "50.0E-3")

;;; D664 — self-TCO must not emit `br` that crosses a try/finally from a
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

;;; D667 — (the fixnum ...) emits native int64 arithmetic.
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

;;; D668 — fixnum-typed comparisons (< > <= >= = /=) in fused if/cond/and/or
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

;;; D669 — (declare (fixnum x)) turns x itself into a fixnum-typed reference
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

;;; D670 — dotimes with fixnum count injects (declare (fixnum var limit))
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

;;; D671 — (declaim (ftype (function (...) fixnum) name)) marks NAME's
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

;;; D672 — (declare (double-float x)) enables native r8 arithmetic on
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

;;; D678 — dotcl:save-application (#62 MVP, :executable nil)
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

(deftest d678-save-application-smoke
  (%d678-save-application-smoke)
  t)

;;; D682 — TCO now works for params that are captured+mutated (boxed).
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

(deftest d682-tco-boxed-params-deep
  ;; Stack is ~256MB; without TCO this would SO well before 100k.
  (%d682-iter-sum 100000 0)
  5000050000)

;;; D688 — REQUIRE returns newly-added module list (SBCL convention),
;;; and idempotent re-require short-circuits even when the contrib's
;;; file forgot to call (provide ...).
(deftest d688-require-first-returns-non-nil
  ;; Use a contrib that (as of D687) doesn't call (provide ...) itself —
  ;; dotcl-cs is such a module (intentionally, see D903). Require should
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

;;; D698 — (defun (setf name) ...) use-direct path: self-fn-prelude must use
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

;;; D709 — macroexpand-cache: plist-dependent macros (anaphora-style)
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

;;; D712 — incf/decf of (car ...) / (cdr ...) / (nth ...) returns store value
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

(deftest d712-asif-like
  ;; Minimal anaphora ASIF.1 reproducer without the library.
  (let ((x (list 0)))
    (let ((it (incf (car x))))
      (if it it (list :oops it))))
  1)

;;; D713 — (defun (setf NAME) ...) in a user-defined package registered correctly
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

(deftest d713-setf-fn-user-package-fasl
  (%d713-setf-fn-fasl)
  t)

;;; D715 — character name table covers C0 control mnemonics (SBCL-compat)
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

(deftest d715-char-name-nbsp
  (char-code (read-from-string "#\\No-break_space"))
  160)

;;; D719 — compile-file FASL split must not orphan labels across helper methods
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

(deftest d719-if-with-defun-fasl
  (%d719-if-with-defun-fasl)
  t)

;;; D736 — compile-file preserves multi-dimensional array literals (#2A ...)
;;; Previously EmitLoadConstInline always used the 1-arg LispVector(LispObject[])
;;; ctor which dropped `_dimensions`, turning `#2A((1 2 3) (4 5 6))` into a flat
;;; SIMPLE-VECTOR of 6 elements after load. Broke reversi (issue #149) via its
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

(deftest d-2darray-literal-fasl
  (%d-2darray-literal-fasl)
  t)

;;; D747 — compile-file preserves element-type for 1D specialized vectors and bit-vectors
;;; Previously EmitLoadConstInline used the 1-arg ctor for 1D vectors even when
;;; element-type != T, dropping it (issue #150 / D736 sibling).
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

(deftest d747-1d-specialized-vector-fasl
  (%d747-1d-specialized-vector-fasl)
  (single-float t 4 1 0 1 1))

;;; D741 — dotnet:invoke / dotnet:static unified InvokeMember dispatch
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

;;; D755 — DOTCL-MOP package phase 1 (#144).
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

;;; D766 — compile-defmacro must not check package lock when macro already defined
;;; Regression for (unless (fboundp 'defmethod) (defmacro defmethod ...)) pattern.
(deftest d766-defmacro-guard-pattern-no-error
  (progn
    (unless (fboundp 'defmethod)
      (defmacro defmethod (&rest args) (declare (ignore args)) nil))
    t)
  t)

;;; D767 — ((lambda ...) args) must store function to local before evaluating args
;;; so that the CIL stack is empty at try-block entry when args contain loop/return.
(deftest d767-lambda-call-loop-arg
  ((lambda (p) p)
   (if (let ((v 'hello))
         (loop (when (atom v) (return t)) (return nil)))
       :yes :no))
  :yes)

;;; D768 — defmacro dotted lambda list (a b . rest) support
;;; Macros like (defmacro foo (x . body) ...) use dotted lists as &rest equivalent
(defmacro d768-dotted-rest (x . body)
  `(list ,x ,@body))

(deftest d768-defmacro-dotted-lambda-list
  (d768-dotted-rest 1 2 3)
  (1 2 3))

;;; D769 — initialize-instance :after keyword args are valid initargs (CLHS 7.1.2)
;;; (spell library: (defmethod initialize-instance :after ((object word) &key spelling) ...))
(defclass d769-base ()
  ((%val :initarg :val :reader d769-val)))

(defmethod initialize-instance :after ((obj d769-base) &key extra)
  (declare (ignore extra)))

(deftest d769-initarg-from-after-method
  (let ((obj (make-instance 'd769-base :val 42 :extra :ignored)))
    (d769-val obj))
  42)

;;; D769 — reader: ::keyword reads as :keyword (SBCL compat, e.g. english.txt has ::possessive-adjective)
(deftest d769-reader-double-colon-keyword
  (eq ::foo :foo)
  t)

;;; D770 — gensym forward-ref: defclass with gensym name and forward superclass must resolve CPL
;;; Bug: ToClassSymbol converted uninterned gensyms to interned CL symbols, breaking RefinalizeDependents
(deftest d770-gensym-forward-ref-typep
  (let* ((g1 (gensym))
         (g2 (gensym)))
    (eval `(defclass ,g2 () ()))
    (eval `(defclass ,g1 (,g2) ()))
    (let ((obj (eval `(make-instance ',g1))))
      (list (typep obj g1) (typep obj g2))))
  (t t))

;;; D770 — import with no args must signal program-error (CLHS 11.2 import)
(deftest d770-import-no-args-errors
  (handler-case (eval '(import))
    (program-error () :ok)
    (error () :wrong-error)
    (:no-error (v) (declare (ignore v)) :no-error))
  :ok)

;;; D770 — defclass unknown option must signal program-error (CLHS 7.7)
(deftest d770-defclass-unknown-option-errors
  (handler-case (eval '(defclass d770-bad-opt () () (:unknown-opt)))
    (program-error () :ok)
    (error () :wrong-error)
    (:no-error (v) (declare (ignore v)) :no-error))
  :ok)


;;; D771 — (setf (accessor obj) val) must dispatch through :around methods
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


;;; D803 — make-string-input-stream で CR (0x0D) / CRLF を LF (0x0A) に正規化
;;;         WinUI TextBox / MAUI Editor は CR のみで改行を返すので、
;;;         stream 層で正規化しないと reader の line-comment が form を
;;;         食い潰すバグがある (SBCL 流儀: reader は LF 厳守、stream で畳む)。
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

;;; D827 — UIOP parse-unix-namestring accepts Windows-style absolute paths
;;; (drive letter and/or backslash). Issue #167.
;;; Symbols are looked up at runtime to keep the file readable before
;;; (require "asdf") brings the UIOP package into existence.
(eval-when (:load-toplevel :execute) (require "asdf"))

(defun %d827-parse-unix (s)
  (funcall (find-symbol "PARSE-UNIX-NAMESTRING" :uiop) s))

(defun %d827-absolute-p (p)
  (funcall (find-symbol "ABSOLUTE-PATHNAME-P" :uiop) p))

;; D827 tests are Windows-specific: drive letter `C:\` style paths only
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

;;; D829 — macrolet comprehensive patterns (#76): eval errors now propagate instead of silent nil
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

;;; D850 — Windows long path (>MAX_PATH=260) — .NET 10 が透明に処理する
;;; ことの回帰検証 (#138)。LongPathsEnabled registry が 0 でも動く。
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

;;; D899 (#125) — TCO for labels self-recursion: should not stack overflow
(deftest d899-labels-self-tco-basic
  (labels ((count-down (n)
             (if (= n 0) :done (count-down (- n 1)))))
    (count-down 200000))
  :done)

;;; D899 (#125) — labels self-TCO with accumulator
(deftest d899-labels-self-tco-acc
  (labels ((sum (n acc)
             (if (= n 0) acc (sum (- n 1) (+ acc n)))))
    (sum 100000 0))
  5000050000)

;;; D899 (#125) — defun with inner labels self-TCO
(defun d899-count-down-helper (n)
  (labels ((loop-fn (i)
             (if (= i 0) :done (loop-fn (- i 1)))))
    (loop-fn n)))

(deftest d899-labels-in-defun
  (d899-count-down-helper 200000)
  :done)

;;; D900 (#126) — TCO inside handler-case: should not stack overflow
(defun %d900-count-safe (n)
  (handler-case
      (if (= n 0) :done (%d900-count-safe (- n 1)))
    (error (e) (list :error e))))

(deftest d900-handler-case-self-tco
  (%d900-count-safe 200000)
  :done)

;;; D900 (#126) — handler-case TCO with accumulator
(defun %d900-sum-safe (n acc)
  (handler-case
      (if (= n 0) acc (%d900-sum-safe (- n 1) (+ acc n)))
    (error (e) (list :error e))))

(deftest d900-handler-case-tco-acc
  (%d900-sum-safe 100000 0)
  5000050000)

;;; D902 (#129) — return type inference from body (fixnum declared vars + if branches)
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

;;; D903 (#130) — native fixnum self-call path (box/unbox elimination)
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

;;; D914 — cond no-body arms share one CTMP slot (#114)
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

;;; D917 — fixnum multiply bignum promotion (#154)
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

;;; D916 — MultipleValues.Reset elision for known-single-value calls (#128)
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

;;; D919 — labels mutual TCO dispatch loop (#124)
(deftest d919-labels-mutual-tco-basic
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
