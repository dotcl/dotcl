# Numbers across the .NET boundary

Every form below was run against dotcl and shows its real result.

.NET has numeric types Common Lisp does not name, and Lisp has numeric types
.NET does not. The rule dotcl follows is: **a .NET number arrives as an ordinary
Lisp number whenever one can hold it exactly.** Nothing new to learn, nothing to
unwrap.

```lisp
(dotnet:static "System.Numerics.BigInteger" "Pow" 2 100)
;; => 1267650600228229401496703205376        an integer, not a wrapper

(dotnet:static "System.Int128" "MaxValue")
;; => 170141183460469231731687303715884105727

(dotnet:static "System.UInt64" "MaxValue")
;; => 18446744073709551615
```

There is one exception, `System.Decimal`, described at the end: it carries
information — the number of decimal places — that no Lisp number can hold.

## What comes back

| .NET | Lisp | |
| --- | --- | --- |
| `Byte` `SByte` `Int16` `UInt16` `Int32` `UInt32` `Int64` `IntPtr` `UIntPtr` | `integer` | |
| `UInt64` | `integer` | above `most-positive-fixnum` it is a bignum |
| `Int128` `UInt128` | `integer` | |
| `BigInteger` | `integer` | exact in both directions; CL integers are unbounded |
| `Half` | `single-float` | exact: every `Half` value fits binary32 |
| `Single` | `single-float` | both are IEEE binary32 |
| `Double` | `double-float` | |
| `Decimal` | `decimal` | a dotcl type; see below |

The integer widths are not distinct Lisp types. An `Int128` result is an
integer, and if it happens to be small it is a fixnum — there is no
`int128` to typecase on. What is 128 bits wide is the .NET side of the
boundary, not the value.

## What you can pass in

An integer goes into any .NET integer parameter **if it fits that parameter's
width**. If it does not, you get a `type-error` rather than a silently truncated
value:

```lisp
(dotnet:new-array "System.Byte" 300)     ; signals: 300 does not fit 8 bits
(dotnet:new-array "System.UInt64" -1)    ; signals: unsigned, and -1 is negative
```

"Fits the width" is deliberately not "is within the parameter's declared range".
Common Lisp has no unsigned integer types, so `(ldb (byte 32 0) x)` — the only
way to name a 32-bit pattern — always produces a non-negative integer. A
**signed** target therefore also accepts the unsigned form of the same pattern
and reinterprets it:

```lisp
(dotnet:invoke (dotnet:new-array "System.Int32" #xFFFFFFFF) "get_Item" 0)
;; => -1

(dotnet:static "System.Numerics.BitOperations" "PopCount" 18446744073709551615)
;; => 64                                  a bignum into a ulong parameter
```

Unsigned targets stay strict: `(ldb ...)` already yields non-negative values
there, and a negative argument is far likelier to be a mistake than a request
for all-ones.

## Decimal

`System.Decimal` is the one .NET number no Lisp type can hold, because it
records how many decimal places it has: `1.5m` and `1.50m` are the same
quantity but not the same value. dotcl gives it a type of its own rather than
quietly dropping that.

Write one with `#m`, and it prints the same way:

```lisp
#m1.50                     ; => #m1.50           the trailing zero survives
(dotcl:decimalp #m1.50)    ; => T
```

It is a real number but neither rational nor float — a third exactness
category:

```lisp
(list (numberp #m1.5) (realp #m1.5) (rationalp #m1.5) (floatp #m1.5))
;; => (T T NIL NIL)
```

`=` compares quantity, `eql` compares representation:

```lisp
(list (= #m1.0 #m1.00) (eql #m1.0 #m1.00))    ; => (T NIL)
```

Ordinary arithmetic never produces a decimal. Mixing one into a standard
operation gives you the exact rational the decimal denotes, so the number of
decimal places — meaningless once you have left the decimal domain — does not
silently follow the value around:

```lisp
(+ #m1.5 1)          ; => 5/2
(rational #m1.50)    ; => 3/2
(float #m1.5)        ; => 1.5
(coerce 1/2 'dotcl:decimal)   ; => #m0.5
```

To keep decimal semantics through a calculation, say so. In the scope of a
`dotcl:decimal` declaration, `+ - * /` compile to `System.Decimal` arithmetic
and the decimal places are preserved:

```lisp
(defun dsum (x y) (declare (type dotcl:decimal x y)) (+ x y))

(dsum #m1.50 #m2.25)    ; => #m3.75      decimal arithmetic
(+ #m1.50 #m2.25)       ; => 15/4        undeclared: ordinary arithmetic
```

Mixing a decimal with a float inside such a scope is an error, not a silent
conversion — the same rule .NET itself applies:

```lisp
(defun dmix (x y) (declare (type dotcl:decimal x) (type double-float y)) (+ x y))
(dmix #m1.5 2.0d0)      ; signals: coerce explicitly, e.g. (rational x) or (float x)
```

Marshalling is exact in both directions. A Lisp rational goes into a `decimal`
parameter when it is exactly representable, and signals when it is not, rather
than rounding behind your back:

```lisp
(dotnet:static "System.Decimal" "Multiply" 1/2 4)    ; => #m2.0
(dotnet:static "System.Decimal" "Multiply" 1/3 1)    ; signals
```

## Float widths are preserved

Each .NET float type keeps its width on the way across, so the Lisp type tells
you what the value actually is and later arithmetic runs at the right
precision:

```lisp
(dotnet:static "System.MathF" "Abs" -1.5)    ; => 1.5        a single-float
(dotnet:static "System.Math"  "Abs" -1.5d0)  ; => 1.5d0      a double-float
(dotnet:static "System.Single" "MaxValue")   ; => 3.4028235e38
```

This matters beyond `typep`. Widening a binary32 result to `double-float` would
not add precision it never had — it would only print the binary32 rounding error
at binary64 width, and pull every later operation into double precision.
