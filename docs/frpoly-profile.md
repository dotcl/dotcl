# Where cl-bench frpoly spends its time

`frpoly` computes powers of a multivariate polynomial represented as a list of
alternating exponents and coefficients. The cl-bench variants differ only in the
coefficients: `frpoly/fixnum` uses small integers, `frpoly/bignum` multiplies the
polynomial by 100000 first so the coefficients grow, and `frpoly/float` uses
single floats. `docs/benchmarks.md` records `frpoly/fixnum` at 9.52x and
`frpoly/bignum` at 10.56x the SBCL time, among the worst ratios in the suite.

This report profiles the fixnum and bignum variants on the machine described
below, reads the SIL that dotcl emits for the hot functions, and separates what
is measured from what is inferred. No compiler or runtime change was made for
it.

## How the numbers were taken

- Machine: a Linux x86-64 KVM virtual machine with 6 vCPUs and 9 GiB of memory.
  The host CPU model is not exposed to the guest. .NET SDK 10.0.402.
- dotcl: the development tree of 2026-10-24, built with `make bench-build`
  (Release runtime, `runtime/bin/Release/net10.0/runtime`).
- Benchmarks: `make bench SUITE=gabriel BENCH=frpoly/fixnum` and the same with
  `frpoly/bignum` and `frpoly/float`. The runner loads the benchmark source with
  `load`, not from a fasl, and times one block of the benchmark's own repeat
  count.
- Profile: `dotnet-trace` with the in-process sampler, attached to the running
  process after 80 (fixnum) or 20 (bignum) warm-up iterations and left on for
  20 seconds. The trace was converted to speedscope and interpreted per thread;
  the table below is exclusive time on the Lisp worker thread, with the
  profiler's synthetic `CPU_TIME` and `UNMANAGED_CODE_TIME` leaves excluded.
- SIL: `DOTCL_DUMP_SIL_FOR=P` prints the instruction list of each compiled
  `defun` to standard error. The excerpts below are from that output, from the
  same `load` path the cl-bench runner uses.
- Allocation: `DOTCL_ALLOC_PROF=1` with `dotcl:alloc-reset` and
  `dotcl:alloc-report` around the timed runs.

Benchmark times on this VM:

| benchmark | runs per block | observed time (ms) | consed |
| --- | ---: | ---: | ---: |
| frpoly/fixnum | 30 | 220, 239, 277 | 52.3 MB |
| frpoly/bignum | 10 | 201, 245, 386 | 46.1 MB |
| frpoly/float | 30 | 380 | 64.0 MB |

The runs are listed because the bignum figure moved by almost 2x between
processes; the allocation figure was stable. The ratios in `docs/benchmarks.md`
come from a different machine, so they are not recomputed here.

A fresh process does not start at the speed of the warm loop. In a process that
loaded the source and then ran timed blocks, the first 30-run fixnum block took
0.194 s against 0.136 s for the fastest later block; the first 10-run bignum
block took 0.179 s against 0.093 s. The cl-bench "cold" number therefore
contains a large warm-up component (JIT of the benchmark functions, first-call
paths, GC growth) in addition to the arithmetic. The profile below was taken
after warm-up, so it describes the part that remains once that is paid.

## Top functions by time

### frpoly/fixnum

20.06 s trace of the warm loop on the Lisp worker thread.

| function | exclusive |
| --- | ---: |
| PTIMES1 | 35.11% |
| PTIMES | 17.44% |
| PTIMES3 | 15.38% |
| PTIMES2 | 8.19% |
| PPLUS1 | 5.19% |
| PCTIMES | 4.38% |
| PPLUS | 3.78% |
| PCTIMES1 | 3.67% |
| CastHelpers.IsInstanceOfClass | 2.46% |
| PCPLUS | 1.32% |
| PCOEFADD | 1.10% |
| Runtime.Multiply | 0.62% |
| SymFnSite.Resolve | 0.41% |
| PSIMP | 0.28% |
| Runtime.Add | 0.27% |
| DynamicBindings.UnbindToOn | 0.25% |
| PEXPTSQ | 0.05% |

The nine polynomial functions account for 94.5% of the sampled time.

### frpoly/bignum

20.08 s trace of the warm loop.

| function | exclusive |
| --- | ---: |
| PTIMES1 | 21.35% |
| PTIMES | 12.38% |
| PTIMES3 | 10.17% |
| Arithmetic.MultiplyWords | 9.78% |
| BigInteger.TryGetBytes | 6.93% |
| Arithmetic.MultiplyIntegers | 6.50% |
| PTIMES2 | 4.92% |
| BigInteger..ctor(ReadOnlySpan<byte>, ...) | 3.47% |
| PPLUS | 3.04% |
| PPLUS1 | 2.96% |
| PCTIMES1 | 2.42% |
| CastHelpers.IsInstanceOfClass | 1.71% |
| BigInteger.Add(ReadOnlySpan<uint>, int, ...) | 1.65% |
| PCTIMES | 1.63% |
| Runtime.Add | 1.61% |
| Runtime.Multiply | 1.33% |
| SpanHelpers.Memmove | 1.23% |
| Arithmetic.AddIntegers | 1.16% |
| BigIntegerCalculator.Add | 1.13% |
| PCOEFADD | 0.68% |

Below these are `BigInteger..ctor(ReadOnlySpan<uint>, bool)` 0.65%,
`SpanHelpers.LastIndexOfValueType` 0.57%, `BigInteger.Multiply(...)` 0.55%,
`BigIntegerCalculator.Multiply` 0.47%, PCPLUS 0.45%, `SymFnSite.Resolve` 0.25%,
`Multiply128` 0.19%, and `DynamicBindings.UnbindToOn` 0.17%. Explicit
`BigInteger`/`Arithmetic` work, including `Memmove` and `Multiply128`, is about
33% of the worker's time before counting the arithmetic inlined into the
polynomial functions.

A caveat applies to both tables: the JIT inlines the small runtime helpers into
the compiled Lisp method. The time spent in `DynamicBindings.GetLazy`,
`MultipleValues.Reset`, `Fixnum.Make`, `Runtime.Car`, `Runtime.Cdr` and the
generic arithmetic predicates is therefore charged to the enclosing function,
not shown as separate rows. The table is a lower bound on the cost of the
mechanisms described next.

In a trace started at process launch, the reader and compiler frames also
appear, together with the tier-0 call path. On the Lisp worker the largest
were `CilAssembler.AssembleAndRunSingle` (3.8% fixnum, 5.4% bignum),
`LispFunction.InvokeFast1M` (2.9%, 5.4%), `LispFunction.Call2` (0.9%, 1.3%) and
`Monitor.Enter_Slowpath` (1.3%, 2.3%). Those costs belong to loading and to the
first, unoptimized tier of the benchmark rather than to the steady-state loop.

## What the hot functions do that costs

### Calls through symbols

Every call to another Lisp function is emitted as a symbol lookup and a virtual
invoke, not as a .NET call to the callee's method. This excerpt is the body of
PTIMES1, the hottest fixnum function, from the call to PTIMES2 through the
second `setq`:

```
(:LOAD-SYM-FN "PTIMES2" "CL-BENCH.GABRIEL")
(:CASTCLASS "Symbol")
(:CALL "CilAssembler.GetFunctionBySymbol")
(:CALL "MultipleValues.Reset")
(:LDLOC #:Y_2)
(:CALLVIRT "LispFunction.Invoke1")
(:CALL "Runtime.UnwrapMv")
(:STLOC #:SETQSPL_15)
...
(:LOAD-SYM-FN "PTIMES3" "CL-BENCH.GABRIEL")
(:CASTCLASS "Symbol")
(:CALL "CilAssembler.GetFunctionBySymbol")
(:CALL "MultipleValues.Reset")
(:LDLOC #:Y_2)
(:CALLVIRT "LispFunction.Invoke1")
(:CALL "Runtime.UnwrapMv")
```

The same pattern occurs at every call site in the polynomial code. The
instruction lists of the hot functions contain:

| function | indirect calls | symbol lookups | multiple-values ops | Car/Cdr family | type checks |
| --- | ---: | ---: | ---: | ---: | ---: |
| PTIMES | 8 | 8 | 11 | 11 | 10 |
| PTIMES1 | 2 | 2 | 4 | 1 | 1 |
| PTIMES2 | 3 | 2 | 5 | 5 | 1 |
| PTIMES3 | 7 | 6 | 30 | 32 | 20 |
| PPLUS | 8 | 8 | 9 | 11 | 4 |
| PPLUS1 | 5 | 2 | 8 | 15 | 4 |
| PCTIMES | 2 | 2 | 3 | 2 | 1 |
| PCTIMES1 | 3 | 2 | 5 | 3 | 1 |
| PCOEFADD | 0 | 0 | 1 | 0 | 3 |
| PSIMP | 0 | 0 | 1 | 2 | 3 |
| PEXPTSQ | 2 | 2 | 8 | 0 | 7 |

The counts are operations in the dumped instruction list, including the entry
and exit protocol, so they are not normalized IL instruction counts. They show
the shape: the more polynomial arithmetic a function does, the more of its body
is call and multiple-value plumbing. PPLUS1 has five calls but only two symbol
lookups and two `GetFunctionBySymbol` because its three self tail calls use the
function object already in argument 0.

The symbol call also prevents the compiler from inlining the callee and from
specializing on its argument types. `CilAssembler.GetFunctionBySymbol` is a
cache lookup; `SymFnSite.Resolve` is the slow path and is only 0.4% of the
profile. The cost is not one expensive lookup; it is that the call remains a
virtual `InvokeNM` and the MV protocol around it remains.

### The multiple-values protocol

Every function that can return more than one value takes a mode at entry and
every caller resets and unwraps around the call:

```
(:CALL "MultipleValues.TakeMode") (:STLOC #:MVMODE_4)
...
(:CALL "MultipleValues.Reset")
(:CALLVIRT "LispFunction.Invoke2M")
(:CALL "Runtime.UnwrapMv")
```

In the fixnum dump, PTIMES3 alone has 30 multiple-value operations. `pzerop`,
the macro used for every coefficient test, also produces `Runtime.Zerop` +
`UnwrapMv` + `IsTruthy` + `Reset`, because `Zerop` returns a multiple value.

### Special variables

`*x*`, `*frpoly-v*` and `u*` are declared with `defvar` in the benchmark, so
PTIMES1, PTIMES2 and PTIMES3 communicate through dynamic bindings. PTIMES1
saves the binding stack, binds the variables, and restores them in exception
blocks:

```
(:CALL "DynamicBindings.Here") (:STLOC #:DSTACK_23)
(:CALL "DynamicBindings.BindDepthOn") (:STLOC #:BDEPTH_22)
(:LOAD-SYM-PKG "*X*" "CL-BENCH.GABRIEL") (:CASTCLASS "Symbol")
(:LDLOC #:*X*_1) (:LDLOC #:DSTACK_23) (:CALL "DynamicBindings.PushOn")
(:BEGIN-EXCEPTION-BLOCK)
...
(:BEGIN-FINALLY-BLOCK)
(:LDLOC #:BDEPTH_22) (:LDLOC #:DSTACK_23)
(:CALL "DynamicBindings.UnbindToOn")
```

Each read inside the loop pays the symbol load again. This is PTIMES3 computing
`(+ (car *x*) (car y))`:

```
(:LOAD-SYM-PKG "*X*" "CL-BENCH.GABRIEL") (:CASTCLASS "Symbol")
(:LDLOCA #:DSTACK_72) (:CALL "DynamicBindings.GetLazy")
(:CALL "Runtime.Car")
(:LDLOC #:Y_1) (:CALL "Runtime.Car")
(:CALL "Runtime.Add")
```

The symbol lookup and the binding-stack check are inside the inner loops of the
polynomial multiply. PTIMES1 has 14 `DynamicBindings` operations and PTIMES3
has 22 in the dump.

### Generic arithmetic and boxing

There is no fixnum fast path in the emitted code. `(+ ...)` becomes
`Runtime.Add` and `(* ...)` becomes `Runtime.Multiply`, both taking and
returning `LispObject`:

```
(:DECLARE-LOCAL #:DA_9 "LispObject")
(:LDLOC #:DA_12) (:LDLOC #:DA_13)
(:CALL "Runtime.Add")
...
(:LDLOC #:C_1) (:LDLOC #:P_2)
(:CALL "Runtime.Multiply")
```

Every arithmetic result in the fixnum variant is a new boxed `Fixnum`. The
allocation counters show it.

### List operations and type checks

`car`, `cdr`, `cadr`, `cddr`, `caddr`, `cons`, `rplaca` and `rplacd` are runtime
calls on `LispObject`. The dump contains `Runtime.Car`, `Runtime.Cdr`,
`Runtime.Cadr`, `Runtime.Cddr`, `Runtime.Caddr`, `Runtime.MakeCons`,
`Runtime.Rplaca` and `Runtime.Rplacd`. The predicates are equally general:
`Runtime.IsAtomObject`, `Runtime.IsNotNil`, `Runtime.IsTrueNumberp`,
`Runtime.Zerop`, `Runtime.IsTrueEq`, `Runtime.IsTrueNumEq`, `Runtime.IsTrueGt`,
`Runtime.IsTrueLt`, `Runtime.IsTruthy`, `Runtime.Not`, and `Runtime.GetProp`
for `(get x 'order)`. The code has to re-establish at run time facts that the
algorithm never violates, because the source has no declarations.

### Bignum arithmetic

In the bignum variant the same polynomial code calls the same generic
`Runtime.Multiply`; the difference is the implementation below it.
`Arithmetic.MultiplyIntegers` converts both operands to `BigInteger` and calls
`MultiplyBig`. For operands whose bit lengths put them in the 64-bit limb range,
`MultiplyWords` writes each `BigInteger` to a byte buffer, multiplies the limbs,
and constructs a new `BigInteger` from the result bytes:

```
Span<ulong> a = stackalloc ulong[MulWordsMax];
Span<ulong> b = stackalloc ulong[MulWordsMax];
Span<ulong> r = stackalloc ulong[2 * MulWordsMax];
...
x.TryWriteBytes(MemoryMarshal.AsBytes(a), out _, isUnsigned: true);
y.TryWriteBytes(MemoryMarshal.AsBytes(b), out _, isUnsigned: true);
for (int i = 0; i < na; i++) { ... Math.BigMul(ai, b[j], out ulong lo); ... }
return new BigInteger(MemoryMarshal.AsBytes(r.Slice(0, na + nb)), isUnsigned: true);
```

The profile rows for this path are the measured cost: `MultiplyWords` 9.78%,
`TryGetBytes` 6.93%, the `BigInteger` byte-span constructor 3.47%, and the
`BigInteger.Add`/`BigIntegerCalculator.Add` family another 2.8%. Converting to
bytes and back is a large share of a multiplication that already has the
operands in registers.

## Allocation

Counts after warm-up, reset immediately before the timed runs.

| type | fixnum, 30 runs | per run | bignum, 10 runs | per run |
| --- | ---: | ---: | ---: | ---: |
| Cons | 1,267,877 | 42,263 | 425,037 | 42,504 |
| Fixnum | 599,662 | 19,989 | 1,962 | 196 |
| Bignum | 1 | 0 | 369,001 | 36,900 |
| total | 1,868,018 | 62,267 | 796,478 | 79,648 |

The fixnum kernel allocates about 20,000 boxed fixnums and 42,000 conses per
run. The bignum kernel allocates about 37,000 bignums and 42,000 conses per run,
which is the source of its 4.6 MB per run and its higher GC load.

## What is measured and what is guessed

Measured:

- the benchmark times and allocation figures above;
- the exclusive-time percentages, from the sampled traces;
- the SIL instruction sequences and the operation counts in the table;
- the warm-loop source-vs-fasl comparison below (about 10% for both variants).

Guessed:

- how much of the time charged to a Lisp function is the inlined helper code;
- the saving each fix would produce, except where the fix was actually run;
- the split between the arithmetic and the calling convention for the bignum
  functions, although the explicit `Arithmetic` rows put a floor under the
  arithmetic part.

## Fixes that look possible

The estimates are not additive: several of them remove the same call-site
overhead.

| fix | what it removes | estimated saving | basis |
| --- | --- | --- | --- |
| Make `load` know functions defined earlier in the same file, or run the benchmark from a fasl | symbol lookup, `GetFunctionBySymbol`, virtual `Invoke`, and enables inlining | about 10% (measured: warm fixnum 0.140 s -> 0.125 s, bignum 0.096 s -> 0.086 s, medians of minima) | measured A/B: source vs `compile-file` fasl, 3 rounds each, `DOTCL_NO_JIT_PROFILE=1`; `compile-file` emits `CALL-SAME-FILE` where `load` emits the symbol sequence |
| Cheaper special-variable access and binding | `DynamicBindings.GetLazy`/`SetCheckedLazy`, symbol load, binding-stack push/pop and the exception blocks | 10-20% fixnum, less for bignum | SIL shows 14-22 dynamic-binding operations in PTIMES1/PTIMES3; not isolated in a timing run |
| Skip the multiple-values protocol when the caller wants one value | `TakeMode`, `Reset`, `UnwrapMv` around every call and at every entry | 5-15% | SIL; the dump has up to 30 MV operations in one hot function |
| Inline or open-code `car`/`cdr`/`cons` and fixnum arithmetic | `Runtime.Car`/`Cdr`/`MakeCons`, `Runtime.Add`/`Multiply`, `Fixnum.Make` | 10-25% fixnum, 5-10% bignum | profile and SIL; guesses, because most helper calls are inlined and charged to the caller |
| Avoid the byte round-trip in bignum multiplication | `TryGetBytes`, `BigInteger` byte-span construction, `Memmove` | 10-20% bignum | measured explicit arithmetic is about 33% of the bignum worker's time; the byte/constructor rows are about 10% |
| Reduce boxing of fixnum intermediates | `Fixnum` allocation on every arithmetic result | 5-10% fixnum | 20,000 Fixnums per run, measured; saving is a guess |
| Compile the cl-bench sources before running them | the `load`-time compilation and the same symbol calls as the first row | 5-15% overall | static SIL and the same 10% warm A/B; the cold run also benefits from not compiling inside the timed process |

The first and last rows are the same mechanism seen from two sides: the compiler
can already emit a direct same-file call when it has file context
(`CALL-SAME-FILE` appears in the `compile-file` SIL), but the cl-bench runner
loads each top-level form separately, so it cannot. The measured 10% is the part
of the warm loop that this removes; it is not 10x.

The larger gap is the calling convention and the generic operations, not one
missing optimization. A fix that only makes symbol lookup faster will leave the
virtual invoke, the MV protocol and the boxed arithmetic in place, and the
profile rows are dominated by the polynomial functions that contain all three.
Declared code is closer to C#: the loop kernels in `bench/csharp-parity`, which
carry full type declarations, run at about 1x C# on the same runtime, though the
call-heavy kernels there still pay a large part of the calling convention.
Adding declarations to frpoly would measure a different benchmark, so the useful
targets are the undeclared path: direct calls, cheap special bindings, a
single-value call convention, and primitive list/fixnum operations.

## Reproducing

```
make bench-build
make bench SUITE=gabriel BENCH=frpoly/fixnum
make bench SUITE=gabriel BENCH=frpoly/bignum
```

For the profiles, run the benchmark in a process that stays alive, attach
`dotnet-trace` to it, and convert the result to speedscope:

```
dotnet-trace collect -p <pid> --duration 0:0:0:20 -o frpoly.nettrace
dotnet-trace convert frpoly.nettrace --format Speedscope -o frpoly
dotnet-trace report frpoly.nettrace topN -n 40
```

The per-thread percentages in this report were read from the speedscope file;
`dotnet-trace report topN` alone mixes in the launcher and finalizer threads.
SIL is dumped with `DOTCL_DUMP_SIL_FOR=<substring>`, for example
`DOTCL_DUMP_SIL_FOR=P` for all the polynomial functions.
