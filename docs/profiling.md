# Profiling dotcl code

dotcl compiles Lisp functions to real .NET methods, and it names those methods
after the Lisp functions they came from. That means the standard .NET profilers
work on dotcl programs as-is — a CPU profile shows your Lisp function names, not
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

`speedscope.app` runs entirely in the browser — the file is not uploaded.

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

- `_direct` / `_direct_N` — the fast path, called with the arguments as real
  .NET arguments. This is where a normal call lands.
- `_body_N` — the body of a `.fasl` function, called through its wrapper.
- `_native_N` — the unboxed-integer entry point (declared-fixnum arithmetic).
- `_opt2`, `_opt3`, … — the entry point for a call that supplied that many
  optional arguments.
- `lambda_direct`, `lambda_closure_direct` — anonymous functions. They have no
  name to show; look at the caller to place them.

`toplevel()` is one top-level form of a file being loaded.

Interleaved between your functions you will see `LispFunction.Invoke1`,
`Invoke2`, … — that is the call itself. Their *inclusive* time is near 100% by
construction; their *exclusive* time is the real per-call overhead.

## Build the code you are measuring in Release

A Debug build of the runtime is not just slower — it is differently shaped, and
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

- `Thread.Join(int32)` — the launcher thread waiting for the Lisp thread.
  Ignore it; it is not CPU time.
- `CastHelpers.IsInstanceOfClass` — the type checks that compiled Lisp code
  performs. Attributed to the runtime, caused by your code's dynamic typing.
- `Runtime.UnwrapMv`, `MultipleValues.Reset` — the multiple-values protocol.
- `Fixnum.Make`, `Fixnum..ctor` — integer boxing. If these are high, the
  arithmetic in the hot loop is not on the unboxed path; a type declaration on
  the loop variables usually moves it there.
- `GC.RunFinalizers`, `PollGCWorker` — allocation pressure.

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
