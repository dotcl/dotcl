;;; dotnet-cs:inline-cs splices the IL Roslyn writes for a C# body into the
;;; enclosing function. Roslyn writes the compact encodings -- small constants,
;;; argument and local indices in the opcode itself, one-byte operands and
;;; one-byte branch offsets -- and the splice used to hand them to the assembler
;;; as they were, which knows only the general forms: any constant from -1 to 8
;;; failed with "unknown opcode LDC-I4-3". Each body below is written to make
;;; Roslyn emit one family of them, checked against the IL it actually emits.
;;;
;;; Needs the contrib's lib/ (Roslyn and its helper), which a source tree has
;;; only after `make contrib-dotnet-cs'. Without it the tests are skipped and
;;; counted, and INLINE-CS-SUMMARY prints the count at the end of the run, so a
;;; run without lib/ says what it did not run. In the full CI run (DOTCL_CI=1,
;;; DOTCL_REGRESSION_TIER full or unset) a missing lib/ is a failure instead:
;;; that run builds it first, and a skip there would pass looking like a pass.
;;; Splicing IL needs an emitter, so not on the emit-free build (counted too).
;;;
;;; Loaded after the REQUIRE tests in recent-fixes.lisp, which need dotnet-cs not
;;; to be loaded yet.

(require "dotnet-cs")

(defun %inline-cs-lib-built-p ()
  (let ((dir (symbol-value (find-symbol "*CONTRIB-DIR*" "DOTNET-CS"))))
    (and dir
         (probe-file (merge-pathnames
                      (make-pathname :directory '(:relative "lib")
                                     :name "DotCL.Contrib.DotnetCs" :type "dll")
                      dir))
         t)))

(defvar *inline-cs-tests-run* 0)
(defvar *inline-cs-skipped-no-lib* 0)
(defvar *inline-cs-skipped-emit-free* 0)

(defmacro deftest-inline-cs (name form &rest expected)
  `(cond ((not (emitting-mode-p)) (incf *inline-cs-skipped-emit-free*))
         ((not (%inline-cs-lib-built-p)) (incf *inline-cs-skipped-no-lib*))
         (t (incf *inline-cs-tests-run*)
            (deftest ,name ,form ,@expected))))

(defun %inline-cs-lib-required-p ()
  (and (equal (dotcl:getenv "DOTCL_CI") "1")
       (eq (regression-tier) :full)
       (emitting-mode-p)))

;;; The full CI run builds lib/ before the suite; there it must be present.
(when (%inline-cs-lib-required-p)
  (deftest inline-cs-lib-built-in-full-ci-run
    (if (%inline-cs-lib-built-p)
        t
        "contrib/dotnet-cs/lib is missing; run `make contrib-dotnet-cs' before the suite")
    t))

(unless (%inline-cs-lib-built-p)
  (format t "~&;; inline-cs-short-opcodes: contrib/dotnet-cs/lib not built, tests skipped~:[~;~
             ; this run requires it (make contrib-dotnet-cs)~]~%"
          (%inline-cs-lib-required-p)))

(defun inline-cs-summary ()
  (print (list 'INLINE-CS 'TESTS-RUN *inline-cs-tests-run*
               'SKIPPED-NO-LIB *inline-cs-skipped-no-lib*
               'SKIPPED-EMIT-FREE *inline-cs-skipped-emit-free*
               'LIB-REQUIRED (%inline-cs-lib-required-p)))
  t)

;;; Compile a function of ARGS whose body is the C# BODY (every argument a
;;; long) and call it on VALS.
(defmacro %ics (args body &rest vals)
  `(funcall (compile nil '(lambda ,args
                            (dotnet-cs:inline-cs
                             ,(mapcar (lambda (a) (list a 'long a)) args)
                             :returns long :body ,body)))
            ,@vals))

;;; The opcodes Roslyn emits for BODY, to check that a test exercises what it
;;; says it does.
(defun %ics-ops (body)
  (mapcar #'car (dotnet-cs:disassemble-cs body)))

;;; ldc.i4.m1, ldc.i4.0 .. ldc.i4.8: the constant in the opcode.
(deftest-inline-cs inline-cs-ldc-i4-n
  (list (%ics (x y) "return x + y + 3;" 1 2)
        (%ics (x) "return x * -1;" 7)
        (%ics (x) "return x + 0;" 7)
        (%ics (x) "return x - 8;" 7))
  (6 -7 7 -1))

(deftest-inline-cs inline-cs-ldc-i4-n-is-what-roslyn-emits
  (and (member :ldc-i4-3 (%ics-ops "public static long f(long x) { return x + 3; }"))
       (member :ldc-i4-8 (%ics-ops "public static long f(long x) { return x - 8; }"))
       t)
  t)

(deftest-inline-cs inline-cs-every-ldc-i4-n
  (loop for k from -1 to 8
        collect (funcall (compile nil `(lambda (a)
                                         (dotnet-cs:inline-cs ((x long a)) :returns long
                                           :body ,(format nil "return x * ~D;" k))))
                         10))
  (-10 0 10 20 30 40 50 60 70 80))

;;; ldc.i4.s: a one-byte operand, signed.
(deftest-inline-cs inline-cs-ldc-i4-s
  (list (%ics (x) "return x * 100;" 7)
        (%ics (x) "return x * -100;" 7))
  (700 -700))

;;; ldarg.s / starg.s: the fifth argument and up is read with ldarg.s, and an
;;; assignment to an argument is starg.s.
(deftest-inline-cs inline-cs-ldarg-s-starg-s
  (list (%ics (a b c d e g) "return g * 10 + e;" 1 2 3 4 5 6)
        (%ics (x) "if (x < 0) x = 0; return x;" -5))
  (65 0))

(deftest-inline-cs inline-cs-ldarg-s-starg-s-is-what-roslyn-emits
  (and (member :ldarg-s (%ics-ops "public static long f(long a, long b, long c, long d, long e) { return e; }"))
       (member :starg-s (%ics-ops "public static long f(long x) { if (x < 0) x = 0; return x; }"))
       t)
  t)

;;; ldloc.N / stloc.N and br.s / blt.s: a loop with C# locals of two types.
(deftest-inline-cs inline-cs-loop-locals-short-branches
  (list (%ics (x) "long s = 0; for (long i = 0; i < x; i++) s += i * 100; return s;" 10)
        (%ics (x) "long s = 0; for (int i = 0; i < x; i++) s += i; return s;" 10))
  (4500 45))

;;; ldloc.s / stloc.s: a body with more than four locals.
(deftest-inline-cs inline-cs-ldloc-s-stloc-s
  (%ics (x) "long a = 0, b = 0, c = 0, d = 0, e = 0;
             for (long i = 0; i < x; i++) { a += i; b += a; c += b; d += c; e += d; }
             return a + b + c + d + e;"
        4)
  80)

(deftest-inline-cs inline-cs-ldloc-s-is-what-roslyn-emits
  (and (member :stloc-s (%ics-ops "public static long f(long x) {
             long a = 0, b = 0, c = 0, d = 0, e = 0;
             for (long i = 0; i < x; i++) { a += i; b += a; c += b; d += c; e += d; }
             return a + b + c + d + e; }"))
       t)
  t)

;;; Short conditional branches: beq.s bne.un.s bge.s bgt.s ble.s blt.s, and the
;;; unsigned ones Roslyn emits for ulong comparisons.
(deftest-inline-cs inline-cs-short-conditional-branches
  (list (%ics (a b) "if (a == b) return 1; return 2;" 3 3)
        (%ics (a b) "if (a != b) return 1; return 2;" 3 3)
        (%ics (a b) "return a < b ? 5 : -1;" 1 2)
        (%ics (a b) "return a <= b ? 5 : -1;" 3 2)
        (%ics (a b) "return a > b ? 5 : -1;" 3 2)
        (%ics (a b) "return a >= b ? 5 : -1;" 1 2))
  (1 2 5 -1 5 -1))

(deftest-inline-cs inline-cs-unsigned-branches
  ;; -1 is the largest ulong.
  (list (%ics (x) "if ((ulong)x > 10UL) x = 2; return x;" -1)
        (%ics (x) "if ((ulong)x < 10UL) x = 2; return x;" -1)
        (%ics (x) "if ((ulong)x >= 10UL) x = 2; return x;" -1)
        (%ics (x) "if ((ulong)x <= 10UL) x = 2; return x;" -1)
        (%ics (x) "if ((ulong)x > 10UL) x = 2; return x;" 5))
  (2 -1 2 -1 5))

;;; Unsigned arithmetic and narrowing conversions.
(deftest-inline-cs inline-cs-unsigned-arithmetic
  (list (%ics (x) "return (long)((ulong)x >> 60);" -1)
        (%ics (x) "return (long)((ulong)x / 2UL);" -2)
        (%ics (x) "return (long)((ulong)x % 10UL);" -1)
        (%ics (x) "return (long)(byte)x + (long)(sbyte)x;" 255)
        (%ics (x) "return (long)(ushort)x + (long)(short)x;" 65535)
        (%ics (x) "return (long)(uint)x;" -1))
  (15 9223372036854775807 5 254 65534 4294967295))

;;; switch, and a RET before the end of the body.
(deftest-inline-cs inline-cs-switch-and-early-return
  (list (%ics (x) "switch (x) { case 0: return 10; case 1: return 20; case 2: return 30; default: return -1; }" 2)
        (%ics (x) "switch (x) { case 0: return 10; case 1: return 20; case 2: return 30; default: return -1; }" 9)
        (%ics (x) "if (x > 0) return 1; return 2;" 5)
        (%ics (x) "if (x > 0) return 1; return 2;" -5))
  (30 -1 1 2))

;;; Spliced into an expression with a value already on the stack, and twice in
;;; one function: the labels and locals of each splice are its own.
(deftest-inline-cs inline-cs-two-splices-in-one-function
  (funcall (compile nil '(lambda (n)
                           (+ 1
                              (dotnet-cs:inline-cs ((x long n)) :returns long
                                :body "long s = 0; for (long i = 0; i < x; i++) s += i; return s;")
                              (dotnet-cs:inline-cs ((x long n)) :returns long
                                :body "long s = 1; for (long i = 1; i <= x; i++) s *= 2; return s;"))))
           10)
  1070)
