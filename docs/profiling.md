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
on. Two machines of one platform can differ by more than the threshold, so a
machine can also have entries of its own, keyed `<platform>@<cpu>` with the CPU
model as the Makefile's `BENCH_CPU` spells it (`make -pn bench-parity | grep
^BENCH_CPU`); `make bench-parity` uses them when they exist and the plain
`<platform>` entries otherwise. A measurement with no recorded baseline is not judged, and its measured
ratio is printed in the format the file wants so it can be pasted in; the last
line of output says how many measurements were judged and how many were not. A
platform with no recorded baseline at all is reported as `UNJUDGED` and exits
with status 3 instead of passing, because a run that compared nothing cannot
say that nothing got slower. On such a machine,
`CHECK_RATIOS_ALLOW_UNJUDGED=1 make bench-parity` accepts that and exits 0,
still printing `UNJUDGED`. To refresh the recorded numbers after a change that is genuinely meant to
move them, run `make bench-parity` a few times on a quiet machine, take the
median per kernel, and edit `bench/ratio-baseline.json`, updating the matching
`recorded` entry with the date and the machine. Do not refresh it to turn a red
build green without first establishing which of the two the number describes:
the code got slower, or the baseline was measured somewhere else.

## One binary, two speeds

A kernel can run at two distinct speeds depending on the process it lands in,
with nothing changed but where the JIT placed the code. On a Linux x64 machine
(Intel i5-8350U) `array-walk` ran at 39-40 ms in some processes and 57-58 ms
in the others, 10 of 14 processes slow, from one binary. The instructions of
the inner loop were identical; what differed was whether the loop crossed a
32-byte boundary, which depends on where the method's code starts, and that
moves from process to process. `string-walk` split the same way, 103 against
176 ms.

Two things go wrong with such a kernel if it is read as one noisy value. Its
median counts how many processes drew each group rather than measuring the
code, and its spread (13-20% CV) is so wide that a rule like "a difference
bigger than twice the CV is real" cannot see anything.

So `make bench-parity` runs each half in `PARITY_PASSES` processes (5 by
default) and checks every kernel's per-process values for a split, with the
rule in `bench/modes.awk`: sort them, take the widest gap between neighbours
that leaves at least two values above it, and call the values split when that
gap is at least 10% of the fast group's median and at least 3 times the wider
group's own range. One slow process alone is not a group: interference only
adds time. One fast process alone is. The target prints each kernel's
per-process distribution, records any split in `bench-state.json` next to the
ratio (`"dotcl_split": {"fast_n": ..., "fast_ms": ..., "slow_n": ...,
"slow_ms": ...}`, or `null`), and the gate prints it under the kernel with the
ratio the slow group alone would give. The gated ratio stays the minimum over
processes, which is the fast group whenever one process drew it and does not
move with how many did; a median would flip between the groups on that share.
What the gate cannot see is a run where every process drew the slow group: it
reads as one slow group. A FAIL on a kernel known to split is worth one rerun,
or a larger `PARITY_PASSES`, before it is believed. When recording a baseline
for such a kernel, take it from a run that shows the split, so the recorded
number is the fast group's.

`bench/test-modes.sh` holds the rule to the values it was written against
(three split cases and three noisy one-group cases from that machine) and runs
first in `make bench-parity`.

## A kernel measured alone is not the same kernel

The obvious way to work on one slow kernel is to copy it and its harness into a
file of its own and iterate there. Do not: the number that comes back is far
slower than the one the gate reports, and the difference is not the kernel.

Five runs are enough in `kernels.lisp` only because the kernels ahead of a
given one have already pushed the process into its fully-tiered state. From a
cold start they are not. Measured on `struct-slots`: **188 ms** in the full
file, **496 ms** in a file holding that kernel and the same harness and
nothing else. The same code, 2.6x apart, and the slow figure is the one you
would be staring at while trying to make it faster.

Two things follow. A kernel's recorded ratio depends on its **position** in
`kernels.lisp`, so reordering that file changes the numbers and is not a
tidy-up. And to study one kernel on its own, either raise `*parity-runs*`
(12 was enough for `struct-slots`, giving 206 ms) or set
`DOTNET_TieredCompilation=0`, which gives a slower absolute figure (242 ms)
that is at least self-consistent, so variants can be compared against each
other. Why tiering on is worse here than tiering off is not understood; with
tiering on, one variant of a decomposition came out slower than a variant
doing strictly more work.

## Comparing two builds: turn the JIT profile off

Set `DOTCL_NO_JIT_PROFILE=1` on **every** run of an A/B that launches separate
processes. The runtime records a JIT profile and the next run reads it back, so
each launch changes the one after it and the two builds are not being compared
on equal terms.

The size of it, measured on one binary copied into two directories and run
alternately: 39.6 ms against 49.5, then 46.6 against 53.2, drifting from 39.6
to 71.9 over four rounds -- for identical code. With the variable set, the same
control came back 38.9 against 39.0 and 46.5 against 46.7.

That control is the thing to run first. A/B the unmodified binary against a
copy of itself, and only believe a measurement if that comes back flat: a 40%
difference manufactured by the harness looks exactly like a 40% difference in
the code, and it is the more likely of the two.

Then look at each side's per-process values before comparing anything.
Concatenate each side's outputs into one file (one `name<TAB>ms` line per
kernel per process, as `kernels.lisp` and the C# half print them) and run

    bench/process-modes.sh base=base.txt variant=variant.txt

It prints each side's count, minimum, median and whether it is split. For a
kernel that is one group on both sides it sets the medians and minimums side
by side. For a kernel that is split on either side it prints the share of
processes in the fast group and the fast-group medians instead, because the
medians there only compare shares. An A/B of that kind once read, by medians,
as a 33% speed-up; the fast groups were 2% apart, and the change had moved the
share of fast processes from 3 in 7 to 7 in 7. Both halves are true, and they
are different claims.

## Which part of it is the cost

`make il-parity` names the surplus instructions, but not which of them you
would have to remove to close the gap. `bench/ablation` is for that: the same
loop run several times over with one suspected cause removed each time, all the
variants alternated inside one process, and a variant with everything removed
as the reachable ceiling. Its README has the rules and a worked example, in
which the bottom of the table -- two variants landing exactly on the ceiling --
was what showed that a representation suspected of being the problem was in
fact free, and that the whole of a 5.2x gap was guard code around it.

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
