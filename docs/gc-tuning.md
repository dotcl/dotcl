# GC settings for compute-heavy programs

If a long-running computation that allocates a lot (batch jobs, benchmarks,
Coalton-style code) is slower than you expect, the .NET garbage collector
settings can help. dotcl keeps the .NET defaults, because the defaults give
short pauses, which is what a REPL or a server wants. The settings below trade
pause time for throughput, and apply to the whole process.

Measured with the ReadyToRun distribution on Linux x86-64 (sv7), Windows x64
(lv9) and macOS arm64, 2026-10-26 and 2026-10-27. The tables show sv7 unless
stated; the section "On other machines" lists what differs elsewhere. Treat the
ratios as indicative; they come from three to five alternating rounds per row,
and individual cl-bench rows vary by about 10%.

## When to keep the defaults

Keep them for a REPL, an editor session, or a server. With the defaults, a
10 ms timer task that allocates about 0.8 MB per tick (live heap about 200 MB)
is delayed by at most 98 ms. The settings below make that worse.

## First choice: a larger gen0 budget

    DOTNET_GCgen0size=0x4000000

This sets the generation 0 budget to 64 MB. It must be an environment variable;
`System.GC.Gen0Size` in `runtimeconfig.json` has no effect.

| Measure | Default | `GCgen0size` 64 MB |
|---|---:|---:|
| Coalton list-ops | 715 ms | 582 ms |
| Coalton ord-sort | 377 ms | 320 ms |
| cl-bench, all suites, total cold | 34926 ms | 33684 ms (0.96x) |
| Maximum delay of the 10 ms task | 98 ms | 148 ms |
| Full GC with a 200 MB live heap | 482 ms | 792 ms (+64%) |
| gen0 collections in 15 s | 387 | 38 |

The cost is a longer worst-case delay, slower full collections, and more memory.
No cl-bench row got markedly worse (bitvectors cold 1.12x).

## Second choice: turn off background GC

    DOTNET_gcConcurrent=0

or, in `runtimeconfig.json`, under `configProperties`:

    "System.GC.Concurrent": false

The gain on the allocation-heavy kernels is about the same (Coalton list-ops
701 -> 569 ms), but the cost is larger: cl-bench bitvectors is 2.3x (cold) to
2.9x (steady) slower, and the maximum delay of the 10 ms task rises from 98 ms to
518 ms. Prefer the gen0 setting unless you have measured otherwise.

## Per executable

- A published directory: edit `runtime.runtimeconfig.json` and add the property
  under `configProperties`. This works for `System.GC.Concurrent`.
- `save-application :executable t` produces a single-file executable with the
  runtimeconfig embedded, so it cannot be changed afterwards. Use the
  environment variables instead.
- `DOTNET_GCgen0size` is always an environment variable, for every kind of
  executable.

## What does not help

Setting `System.Runtime.GCSettings.LatencyMode` to `Batch` at run time does not
speed up Coalton list-ops (708-730 ms, same as the default) and does not change
the number of collections. The gain from `gcConcurrent=0` comes from the larger
gen0 budget, which `LatencyMode` does not change. Batch only stops background GC,
and the worst-case delay goes up to 440 ms. `SustainedLowLatency` behaves like
the default.

Not measured: gen0 sizes other than 32 and 64 MB.

## On other machines

The same three settings were measured on Windows x64 (lv9, Core i5-10310U) and
macOS arm64 (a Mac mini that was also running other work, so only the ratios
are usable). Ratio to the default, median over the rounds, with the range over
the rounds in brackets; below 1 is faster.

| Measure | sv7 gen0 | Windows gen0 | arm64 gen0 | sv7 conc0 | Windows conc0 | arm64 conc0 |
|---|---:|---:|---:|---:|---:|---:|
| Coalton list-ops | 0.82 | 0.77 | 0.76 | 0.84 | 0.74 | 0.78 |
| Coalton ord-sort | 0.86 | 0.82 | 0.72 | 0.85 | 0.82 | 0.71 |
| Coalton tree-insert-fold | 0.97 | 0.96 | 0.83 | 0.91 | 0.89 | 0.90 (0.73-1.13) |
| cl-bench bitvectors, cold | 1.12 | 1.74 (1.37-1.75) | 1.09 | 2.61 | 3.41 | 3.16 |
| cl-bench bitvectors, steady | 0.96 | 0.98 | 0.94 | 3.00 | 3.69 | 3.08 |
| cl-bench hash-strings, steady | 0.57 | 0.49 | 0.61 | 0.67 | 0.68 | 0.92 (0.85-1.04) |
| cl-bench browse, steady | 1.09 | 1.11 | 1.09 | 0.97 | 0.96 | 1.00 |
| cl-bench, all suites, steady | 0.95 | 0.97 | 0.96 | 1.04 | 1.07 | 1.02 |
| Full GC, 8M cons live | 1.22 | 1.29 | 0.60 | 1.03 | 1.13 | 0.58 |
| Largest allocation step, same heap | 0.85 | 0.34 | 0.67 | 1.35 | 0.61 | 2.20 |

- The recommendation is the same on all three: `DOTNET_GCgen0size` helps the
  allocation-heavy Coalton kernels by 5 to 25% and costs at most a few percent
  on the whole cl-bench suite, and `gcConcurrent=0` gains about the same on
  those kernels but makes bitvectors three times slower everywhere.
- `GCgen0size` is honoured on Windows and on arm64 as it is on Linux.
- On Windows the first (cold) bitvectors run is 1.4 to 1.7 times slower with
  `GCgen0size` (about 75 ms); steady state is unchanged.
- The full GC of a large live heap is slower with either setting on x64
  (up to 1.3 times) but faster on arm64 (0.6 times). The largest single
  allocation step gets longer with `gcConcurrent=0` on Linux and arm64 and
  shorter on Windows. These two rows come from one synthetic program
  (`bench/jit-tiering/gcpause.lisp`), so read them as direction only.
- arm64 noise: on the shared Mac a few rows have a range that crosses 1
  (tree-insert-fold with `gcConcurrent=0`, hash-strings steady, the suite
  total cold); do not read those ratios.

See also [the list of GC settings a running process can change](gc-latency-report.md).
