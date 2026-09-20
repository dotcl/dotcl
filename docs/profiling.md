# Profiling dotcl code

dotcl compiles Lisp functions to real .NET methods, and it names those methods
after the Lisp functions they came from. That means the standard .NET profilers
work on dotcl programs as-is; a CPU profile shows your Lisp function names, not
a wall of interpreter internals.

No dotcl-side setup is needed. There is nothing to enable and no special build.

## Quick start

Install the .NET diagnostic tool once:

```
dotnet tool install -g dotnet-trace
```

Collect a CPU profile by launching your program under it:

```
dotnet-trace collect --format speedscope -o app.nettrace -- \
    dotcl run app.lisp
```

The trace stops when the program exits (or press Enter). To attach to an
already-running process instead, use `dotnet-trace collect -p <pid>`.

Read the result either way:

```
# text: top functions by self time
dotnet-trace report app.nettrace topN -n 25

# interactive flame graph: open app.speedscope.json at https://speedscope.app
```

`speedscope.app` runs entirely in the browser; the file is not uploaded.

## What your functions look like

The frame name is `<assembly>!<type>.<LISP-NAME>`, with the Lisp name in the
case the symbol has (usually upper). Three shapes, depending on where the code
came from:

| Source of the code | Frame |
| --- | --- |
| Compiled in memory (`load` of a `.lisp`, `eval`, the REPL) | `DotCL.Runtime!dynamicClass.COMPUTE-ADJUSTMENT_direct(...)` |
| A `.fasl` from `compile-file` | `gabriel_d9bdc006!CompiledModule.MATCH_body_48(...)` |
| The runtime's own core | `core!CompiledModule.ModuleInit()` |

The suffixes say which entry point of a function is on the stack, and are worth
recognizing:

- `_direct` / `_direct_N`: the fast path, called with the arguments as real
  .NET arguments. This is where a normal call lands.
- `_body_N`: the body of a `.fasl` function, called through its wrapper.
- `_native_N`: the unboxed-integer entry point (declared-fixnum arithmetic).
- `_opt2`, `_opt3`, ...: the entry point for a call that supplied that many
  optional arguments.
- `lambda_direct`, `lambda_closure_direct`: anonymous functions. They have no
  name to show; look at the caller to place them.

`toplevel()` is one top-level form of a file being loaded.

Interleaved between your functions you will see `LispFunction.Invoke1`,
`Invoke2`, ...: that is the call itself. Their *inclusive* time is near 100% by
construction; their *exclusive* time is the real per-call overhead.

## Build the code you are measuring in Release

A Debug build of the runtime is not just slower; it is differently shaped, and
it will send you after the wrong thing. Diagnostic counters that a Release build
folds away entirely still show up in a Debug profile, and the dynamic-method
teardown that a Debug run does can dominate the whole trace.

```
dotnet build runtime/runtime.csproj -c Release
```

then profile `runtime/bin/Release/net10.0/runtime.exe`.

## Reading the result

A few frames appear in almost every dotcl profile. They are not noise, but they
are also not your code:

- `Thread.Join(int32)`: the launcher thread waiting for the Lisp thread.
  Ignore it; it is not CPU time.
- `CastHelpers.IsInstanceOfClass`: the type checks that compiled Lisp code
  performs. Attributed to the runtime, caused by your code's dynamic typing.
- `Runtime.UnwrapMv`, `MultipleValues.Reset`: the multiple-values protocol.
- `Fixnum.Make`, `Fixnum..ctor`: integer boxing. If these are high, the
  arithmetic in the hot loop is not on the unboxed path; a type declaration on
  the loop variables usually moves it there.
- `GC.RunFinalizers`, `PollGCWorker`: allocation pressure.

Sampling is statistical, so short runs report noise. Give the workload at least
a few seconds of the behaviour you care about, ideally in a loop, and compare
runs rather than trusting one.

## Allocation, rather than time

For "what is my hot loop allocating", the runtime has a per-type counter that is
cheaper to read than a full allocation profile:

```
DOTCL_ALLOC_PROF=1 dotcl run app.lisp
```

and in the program, around the region of interest:

```lisp
(dotcl:alloc-reset)
(run-the-thing)
(dotcl:alloc-report)   ; prints a per-type count table
```

The counters are compiled out unless `DOTCL_ALLOC_PROF=1` is set at startup.

A count says what is being allocated, not who allocates it. To get the callers,
name one type to sample stacks for:

```
DOTCL_ALLOC_PROF=1 DOTCL_ALLOC_STACK=LispFunction dotcl run app.lisp
```

and print them with `(dotcl:alloc-stacks)` (optionally with how many stacks to
show; default 20):

```
;; LispFunction: 277 samples (1 per 64 allocations), 6 distinct stacks
     236  85.2%  LispFunction..ctor < LispFunction.MakeDirectClosure < ... < MY-FUNCTION
```

Since compiled Lisp functions are .NET methods named after the Lisp function,
the frames read as Lisp names. Capturing a stack is expensive, so only one type
is sampled and only every 64th allocation of it; `DOTCL_ALLOC_STACK_EVERY` and
`DOTCL_ALLOC_STACK_DEPTH` change the interval and how many frames make up a
stack.

## How far off C# is

`make bench-parity` is a fixed point for one question: does Lisp code with
every declaration written out compete with C# on the same machine? It runs
seven small kernels -- tak, fib, a fixnum loop, a double-float loop, a
`simple-array fixnum` walk, structure slot read/write, and a string walk -- as
a matched pair, `bench/csharp-parity/kernels.lisp` against a plain Release
build of `bench/csharp-parity/Parity.csproj`, over the same input sizes. Each
kernel's dotcl/C# time ratio is written into `bench-state.json` as
`parity/<name>`, and the same target then checks every ratio against the one
recorded for this platform in `bench/ratio-baseline.json`: a ratio above
**1.2x** its recorded value fails the build, and so does the median of the
cl-bench dotcl/SBCL ratios once a baseline for it exists. Ratios are keyed by
platform because they are only reproducible on the machine they were measured
on; a platform with no recorded baseline is reported as unbaselined and passes,
printing its measured ratios in the format the file wants so they can be pasted
in. To refresh the recorded numbers after a change that is genuinely meant to
move them, run `make bench-parity` a few times on a quiet machine, take the
median per kernel, and edit `bench/ratio-baseline.json`, updating the matching
`recorded` entry with the date and the machine. Do not refresh it to turn a red
build green without first establishing which of the two the number describes:
the code got slower, or the baseline was measured somewhere else.

## Which instructions are the difference

`make bench-parity` says whether dotcl is in the contest. `make il-parity` says
what the gap is made of. Five small data structures -- a stack, an open-addressed
hash table, a binary heap, a ring buffer and a string tokenizer -- are written
twice, once in C# (`bench/il-parity/<case>/Ref.cs`) and once in fully declared
Lisp (`impl.lisp`), and both halves are compiled to IL: the C# to a Release
assembly, the Lisp to a fasl, which is the same kind of file. One reader
(`bench/il-parity/tool`, on the in-box `System.Reflection.Metadata`, with its
opcode table reflected out of `System.Reflection.Emit.OpCodes` rather than
transcribed) decodes both into normalized instruction sequences -- local numbers,
branch targets and literals dropped, the operand kept only where it is the point:
a call, a cast, a field. The target prints a per-method count table
(box / unbox / castclass / isinst / newobj / call / callvirt / field / element)
and the **surplus**: the multiset of instructions the Lisp body pays for and the
C# body does not, which is the sentence you actually want -- "this method calls
`Runtime.StructRefI` six times where C# has `ldfld`". Entry conventions are
excluded from the count (the leading argument copies and the multiple-values
protocol) because boxed arguments are their own question; what is compared is the
body. Every case carries a self-check whose value both halves must return, run
before any of this, because comparing the IL of two programs that compute
different things is meaningless. Unlike a time ratio the counts are
deterministic -- same compiler, same source, same numbers on any machine -- so
the gate has no threshold: `bench/il-parity/counts-baseline.tsv` records what
dotcl emits today and any category that goes **up** fails. When a count goes
down, which is the object of the exercise, run `make il-parity-accept` to record
the better number.
