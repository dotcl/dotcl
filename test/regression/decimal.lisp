;;; Item A: first-class CLR decimal (LispDecimal) — reader/printer, predicates,
;;; exact rational round-trip, tower participation by value, and .NET marshalling.

;; --- reader / printer (#m, scale preserved) ---
(deftest dec-read-print
  (princ-to-string #m1.50)
  "#m1.50")

(deftest dec-read-string-form
  (princ-to-string #m"3.14159")
  "#m3.14159")

(deftest dec-read-preserves-scale        ; 1.50m keeps its trailing zero
  (princ-to-string #m1.500)
  "#m1.500")

;; --- predicates / type ---
(deftest dec-decimalp
  (list (dotcl:decimalp #m1.5) (dotcl:decimalp 1.5) (dotcl:decimalp 3/2))
  (t nil nil))

(deftest dec-number-classification        ; third exactness category
  (list (numberp #m1.5) (realp #m1.5) (rationalp #m1.5) (floatp #m1.5)
        (typep #m1.5 'decimal) (typep #m1.5 'real) (typep #m1.5 'rational))
  (t t nil nil t t nil))

;; --- exact conversions out ---
(deftest dec-rational-exact
  (list (rational #m1.50) (rational #m100) (rational #m0.125))
  (3/2 100 1/8))

(deftest dec-float
  (list (float #m1.5) (float #m1.5 1d0))
  (1.5 1.5d0))

;; --- tower participates by exact value; standard ops yield standard types (invariant) ---
(deftest dec-equal-by-value               ; = compares value, ignoring scale/type
  (list (= #m1.0 1) (= #m1.50 3/2) (= #m1.0 #m1.00) (= #m0.5 0.5))
  (t t t t))

(deftest dec-order
  (list (< #m1.5 #m1.6) (> #m2.5 #m1.5) (<= #m1.0 1) (>= #m2.0 2))
  (t t t t))

(deftest dec-arith-degrades-to-rational   ; standard + never produces a decimal
  (list (+ #m1.5 1) (* #m0.5 4) (- #m1.5) (abs #m-2.5)
        (dotcl:decimalp (+ #m1.5 #m1.5)))
  (5/2 2 -3/2 5/2 nil))

;; --- eql is representation-sensitive (scale); = is value-based ---
(deftest dec-eql
  (list (eql #m1.0 #m1.0) (eql #m1.0 #m1.00) (eql #m1.5 #m1.5))
  (t nil t))

;; --- .NET interop: decimal-returning/-taking APIs marshal with scale intact ---
(deftest dec-interop-outbound             ; System.Decimal.Parse -> LispDecimal, scale kept
  (let ((d (dotnet:static "System.Decimal" "Parse" "12.750")))
    (list (dotcl:decimalp d) (princ-to-string d) (= d 51/4)))
  (t "#m12.750" t))

(deftest dec-interop-inbound-exact        ; a CL ratio marshals into a decimal param exactly
  (princ-to-string (dotnet:static "System.Decimal" "Multiply" 1/2 4))
  "#m2.0")

(deftest dec-interop-decimal-in-out
  (princ-to-string (dotnet:static "System.Decimal" "Add" #m1.25 #m2.25))
  "#m3.50")

(deftest dec-interop-precision-error      ; 1/3 is not exactly representable as decimal
  (handler-case (progn (dotnet:static "System.Decimal" "Multiply" 1/3 1) :no-error)
    (error () :precision-error))
  :precision-error)

;; --- Item C: declared-scope native System.Decimal arithmetic (scale preserved) ---
;; In a (declare (type dotcl:decimal ...)) scope, +,-,*,/ compile to System.Decimal ops and
;; keep scale, where the undeclared tower path degrades a decimal to its rational value.

(defun %dec-sum (x y) (declare (type dotcl:decimal x y)) (+ x y))
(defun %dec-diff (x y) (declare (type dotcl:decimal x y)) (- x y))
(defun %dec-mul (x y) (declare (type dotcl:decimal x y)) (* x y))
(defun %dec-neg (x) (declare (type dotcl:decimal x)) (- x))
(defun %dec-chain (x y z) (declare (type dotcl:decimal x y z)) (+ (* x y) z))

(deftest dec-decl-add-preserves-scale
  (let ((r (%dec-sum #m1.50 #m2.25)))
    (list (dotcl:decimalp r) (princ-to-string r)))
  (t "#m3.75"))

(deftest dec-decl-sub
  (princ-to-string (%dec-diff #m5.00 #m1.25))
  "#m3.75")

(deftest dec-decl-mul
  (princ-to-string (%dec-mul #m1.5 #m2))
  "#m3.0")

(deftest dec-decl-neg
  (princ-to-string (%dec-neg #m3.14))
  "#m-3.14")

(deftest dec-decl-chain               ; (+ (* x y) z), all native decimal
  (princ-to-string (%dec-chain #m1.5 #m2.0 #m0.25))
  "#m3.25")

;; let-bound decimal locals get the same native path
(deftest dec-decl-let
  (let ((r (let ((a #m1.50) (b #m2.25))
             (declare (type dotcl:decimal a b))
             (+ a b))))
    (list (dotcl:decimalp r) (princ-to-string r)))
  (t "#m3.75"))

;; (the dotcl:decimal E) is a "strong" trigger: native decimal arithmetic even without a
;; declared local. Bare-literal arithmetic (dec-arith-degrades-to-rational) stays on
;; the standard tower — only a declared decimal / (the decimal) opts into native ops.
(deftest dec-the-triggers-native
  (princ-to-string (+ (the dotcl:decimal #m1.50) (the dotcl:decimal #m2.25)))
  "#m3.75")

(deftest dec-bare-literal-degrades      ; no declaration → standard tower (invariant #1)
  (list (dotcl:decimalp (+ #m1.50 #m2.25)) (- #m1.5))
  (nil -3/2))

;; --- declared-scope mixing guard: decimal + float requires explicit coerce ---
;; A binary op mixing a declared/`the` decimal with a statically float-typed value
;; has no lossless meaning (.NET forbids implicit decimal<->double); the compiler
;; emits a program-error so the mix cannot silently widen to double. An explicit
;; coerce ((rational d) / (float d)) dissolves it.

(defun %dec-mix (x y) (declare (type dotcl:decimal x) (type double-float y)) (+ x y))
(deftest dec-mix-float-errors
  (handler-case (progn (%dec-mix #m1.5 2.0d0) :no-error)
    (program-error () :mix-error))
  :mix-error)

(defun %dec-mix-single (x y) (declare (type dotcl:decimal x) (type single-float y)) (* x y))
(deftest dec-mix-single-errors
  (handler-case (progn (%dec-mix-single #m2.0 3.0) :no-error)
    (program-error () :mix-error))
  :mix-error)

;; explicit rational coerce -> standard tower (double contagion), no error
(defun %dec-mix-coerce (x y) (declare (type dotcl:decimal x) (type double-float y)) (+ (rational x) y))
(deftest dec-mix-coerce-dissolves
  (%dec-mix-coerce #m1.5 2.0d0)
  3.5d0)

;; decimal + integer literal is NOT a float mix -> stays on the standard tower (rational),
;; no error (only decimal<->float is guarded)
(defun %dec-plus-int (x) (declare (type dotcl:decimal x)) (+ x 1))
(deftest dec-plus-int-no-error
  (%dec-plus-int #m1.5)
  5/2)

;; --- coerce INTO decimal ---
(deftest dec-coerce-integer               ; integer -> exact decimal
  (let ((d (coerce 1 'dotcl:decimal)))
    (list (dotcl:decimalp d) (princ-to-string d)))
  (t "#m1"))

(deftest dec-coerce-bignum-in-range
  (princ-to-string (coerce 1000000000000000000000000000 'dotcl:decimal))
  "#m1000000000000000000000000000")

(deftest dec-coerce-ratio-exact           ; 2/5-smooth denominators -> exact, minimal scale
  (mapcar (lambda (r) (princ-to-string (coerce r 'dotcl:decimal)))
          '(1/2 3/4 1/8 1/5))
  ("#m0.5" "#m0.75" "#m0.125" "#m0.2"))

(deftest dec-coerce-float-escape          ; float is the explicit escape out of the mix ban
  (list (princ-to-string (coerce 1.5d0 'dotcl:decimal))
        (princ-to-string (coerce 1.5f0 'dotcl:decimal)))
  ("#m1.5" "#m1.5"))

(deftest dec-coerce-decimal-idempotent    ; already a decimal -> returned as-is, scale kept
  (princ-to-string (coerce #m2.50 'dotcl:decimal))
  "#m2.50")

(deftest dec-coerce-nonrepresentable-signals  ; 1/3 denominator has factor 3 -> error, no rounding
  (handler-case (progn (coerce 1/3 'dotcl:decimal) :no-error)
    (error () :signalled))
  :signalled)

(deftest dec-coerce-result-typep          ; unqualified 'decimal resolves too
  (typep (coerce 7 'decimal) 'dotcl:decimal)
  t)
