# dotcl-simd

128-bit SIMD values, named as in sb-simd. `(require "dotcl-simd")`; everything
is exported from the package `dotcl-simd`.

    (require "dotcl-simd")
    (use-package :dotcl-simd)

    (let ((a (make-f64.2 1d0 2d0))
          (b (f64.2 10d0)))          ; a number is repeated in every lane
      (f64.2-values (f64.2+ a b)))   ; => 11.0d0, 12.0d0

A pack is an immutable object. Every operation is an ordinary function that
takes packs and returns a pack, so the module works the same in the compiler,
in the interpreter and on netstandard2.0. On net8 and net10 the operations run
on `Vector128<T>`; on netstandard2.0, which has no `System.Runtime.Intrinsics`,
they run lane by lane. Both give the same results.

## Types

| Type | Lanes | Lane type |
| --- | --- | --- |
| `f64.2` | 2 | `double-float` |
| `f32.4` | 4 | `single-float` |
| `s64.2` `u64.2` | 2 | `(signed-byte 64)` `(unsigned-byte 64)` |
| `s32.4` `u32.4` | 4 | `(signed-byte 32)` `(unsigned-byte 32)` |
| `s16.8` `u16.8` | 8 | `(signed-byte 16)` `(unsigned-byte 16)` |
| `s8.16` `u8.16` | 16 | `(signed-byte 8)` `(unsigned-byte 8)` |

Each name is also a type (`(typep x 'f64.2)`, `(declare (type u8.16 x))`), a
predicate (`f64.2p`) and the cast function. `(f64.2 x)` returns `x` when it is
an `f64.2` and repeats `x` in every lane when it is a number of the lane type;
anything else, or a number outside the lane type's range, is a `type-error`.
The operations below cast their arguments the same way, so a plain number can
stand for a pack.

## Construction and access

| Function | Meaning |
| --- | --- |
| `(make-X lane0 lane1 ...)` | A pack from one value per lane |
| `(X-values pack)` | The lanes as multiple values |
| `(X! pack-or-number)` | The same 128 bits read as another lane layout |

## Operations

`X` stands for any type name.

| Group | Functions |
| --- | --- |
| Arithmetic | `X+ X- X*`, and for floats `X/` and `X-sqrt`. Integers wrap |
| Min, max | `X-min X-max` |
| Bits | `X-and X-or X-xor X-andc1 X-not` (`andc1` is `(and (not a) b)`). They also work on floats, on the bits |
| Shifts | `X-shiftl X-shiftr` on integer types |
| Compare | `X= X/= X< X<= X> X>=`, giving a mask |
| Select | `(X-if mask a b)`: lanes of `a` where the mask lane is all ones, else of `b` |
| Horizontal | `X-horizontal+ X-horizontal-min X-horizontal-max`, giving a number |
| Rounding | `X-round X-floor X-ceiling X-truncate` on float types |
| Arrays | `X-aref X-row-major-aref` and their `setf` |
| Conversion | `f32.4-from-s32.4` `f32.4-from-u32.4` `s32.4-from-f32.4` `u32.4-from-f32.4` `f64.2-from-s64.2` `f64.2-from-u64.2` `s64.2-from-f64.2` `u64.2-from-f64.2` `f64.2-from-f32.4` `f32.4-from-f64.2` |

Meaning, where implementations differ:

- Integer `+ - *` wrap modulo 2^lane-bits.
- `X-min` and `X-max` on floats propagate a NaN, and treat -0.0 as smaller than
  0.0. (sb-simd on x86 returns the second operand for a NaN.)
- The shift count of `X-shiftl` and `X-shiftr` is one integer for all lanes, from
  0 to lane-bits minus 1; any other count is a `type-error`. A right shift of a
  signed type is arithmetic, of an unsigned type logical.
- Comparisons give a mask of the unsigned type with the same lane width:
  `f64.2<` returns a `u64.2` whose lanes are all ones or all zeros. A comparison
  with a NaN is false, except `/=`, which is true.
- `X-horizontal+` adds the lanes as a balanced tree, `(l0+l1)+(l2+l3)`, so a
  float sum is the same on every implementation. An integer sum wraps.
- `X-round` rounds halves to even.
- A float to integer conversion truncates toward zero, saturates at the range
  of the integer type, and turns NaN into 0. `f64.2-from-f32.4` widens the
  first two lanes; `f32.4-from-f64.2` fills the first two lanes and zeroes the
  others.

## Arrays

`(X-aref array index ...)` reads as many consecutive lanes as the type has,
starting at the element `index` (one index per dimension). `(setf (X-aref array
index) pack)` writes them back and returns the pack. `X-row-major-aref` takes a
single row-major index. The array's element type must be the lane type
exactly: a `(simple-array double-float (*))` for `f64.2`, an
`(unsigned-byte 8)` array for `u8.16`, and so on. An index that leaves fewer
than a full pack of elements is an error.

## Not provided

Non-temporal loads and stores, `sap-ref` and `string-ref` access, instructions
specific to one instruction set (`pshufb`, `hadd`, ...), and the 256-bit types.
There is no per-lane variable shift.

## Compiled code

On net8 and net10 the compiler turns a nest of these calls into vector
instructions: `+ - * /`, `-min -max`, `-and -or -xor -andc1 -not`, `-sqrt`,
the comparisons and `-if`, `-shiftl` and `-shiftr` with a constant count, the
conversions between integer and float packs of the same lane width, the cast
`X`, `-horizontal+ -horizontal-min -horizontal-max` (whose result is a number),
and `X-aref` and its `setf` on a rank 1 array declared as a
`simple-array` of the lane type (for `f64.2`, `f32.4`, `s64.2`, `u64.2`,
`s32.4`, `u16.8` and `u8.16`). Inside the nest the values are not packs, so
nothing is allocated; a pack is made only where a value leaves it (it is
returned, stored in a variable not declared as below, or passed to another
function). A double-float
or single-float constant in such a nest is repeated in every lane at no cost.

    (defun add (x y z n)
      (declare (type (simple-array double-float (*)) x y z)
               (type (integer 0 1000000) n))
      (do ((i 0 (+ i 2))) ((>= i n))
        (declare (type (integer 0 1000002) i))
        (setf (f64.2-aref z i) (f64.2+ (f64.2-aref x i) (f64.2-aref y i)))))

A variable bound by `let`, `let*` or `do` and declared a pack type, whose
initial value is one of these calls (or another such variable), holds the
value the same way, so an accumulator in a loop allocates nothing:

    (defun dot (x y n)
      (declare (type (simple-array double-float (*)) x y)
               (type (integer 0 1000000) n))
      (let ((acc (f64.2 0d0)))
        (declare (type f64.2 acc))
        (do ((i 0 (+ i 2))) ((>= i n))
          (declare (type (integer 0 1000002) i))
          (setf acc (f64.2+ acc (f64.2* (f64.2-aref x i) (f64.2-aref y i)))))
        (f64.2-horizontal+ acc)))

A parameter declared a pack type, of a `defun` that has only required
parameters, is held the same way after the check of its type.

Assigning such a variable a value that is not a pack of its type signals a
`type-error`. A variable a closure refers to is an ordinary variable.

The results and the errors are those of the calls. At `(safety 0)` an index
is not checked. A `notinline` declaration of an operation keeps it an
ordinary call. `compile-file` with `:corlib-name "netstandard"` keeps every
call ordinary, since such a fasl cannot refer to `Vector128`.
