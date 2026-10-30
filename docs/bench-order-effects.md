# Why cl-bench `deriv` changes speed when the gabriel suite runs

`make bench SUITE=gabriel` runs all 21 gabriel benchmarks in one dotcl process.
`dderiv` is the eighth benchmark and `deriv` the ninth. Measured with the script
in [`bench/order-effects/run.lisp`](../bench/order-effects/run.lisp), `deriv` is
about 11% slower in the full suite than when it runs alone in a fresh process
with the same benchmark file loaded. A full `dotcl:gc` before each benchmark
removes the difference. The benchmark itself allocates exactly the same amount
in every case; the extra time is garbage-collector pause.

The raw output of the run described below is in
[`bench/order-effects/results`](../bench/order-effects/results).

## How the suite is run

The `bench` target in the `Makefile` builds the Release runtime and starts one
dotcl process:

```sh
DOTNET_gcServer=0 runtime/bin/Release/net10.0/runtime \
  --asm compiler/cil-out.sil bench/cl-bench-runner/run.lisp
```

`bench/cl-bench-runner/run.lisp` loads `cl-bench/files/gabriel.lisp` once and
runs the benchmarks in a fixed order with fixed iteration counts. There is no
warm-up and no collection between benchmarks. In the scalar mode used by
`make bench`, each benchmark is one timed run of its iteration count. The number
therefore depends on the heap and collector state left by everything before it.

## Method

The measuring script has three modes. Each mode runs in a fresh dotcl process
and loads `cl-bench/files/gabriel.lisp` the same way the runner does:

- `alone`: run only `dderiv` or `deriv`, with that benchmark's suite iteration
  count. This is the same code path as `make bench BENCH=deriv SUITE=gabriel`.
- `suite`: run all 21 gabriel benchmarks in runner order in one process.
- `suite-gc`: the same, but call `dotcl:gc` before every benchmark. The
  collection time is not charged to the benchmark.

The script starts each mode as a child process, rotates the four scenarios each
round, and repeats for five rounds. It records, immediately before and after
each timed region, `dotcl:gc-stats`: generation 0, 1 and 2 collection counts,
`GC.GetTotalMemory(false)` as heap size, bytes allocated, and total GC pause
time. Times are `get-internal-real-time`. All children inherit
`DOTNET_gcServer=0` (workstation GC) and run the Release runtime.

The machine was a Linux x64 VM with 6 logical CPUs and 5.8 GiB RAM, .NET
10.0.402, dotcl from the development tree of 2026-10-24, built with `make cross-compile bench-build`. Command,
from the project root:

```sh
DOTNET_gcServer=0 runtime/bin/Release/net10.0/runtime \
  --asm compiler/cil-out.sil bench/order-effects/run.lisp < /dev/null
```

## Results

Times in seconds; five child processes per row.

| Benchmark | Mode | Min | Median | Median vs alone |
| --- | --- | ---: | ---: | ---: |
| `dderiv` | alone | 0.199 | 0.205 | 1.000 |
| `dderiv` | full suite | 0.224 | 0.228 | 1.112 |
| `dderiv` | full suite, full GC before each | 0.204 | 0.206 | 1.005 |
| `deriv` | alone | 0.174 | 0.177 | 1.000 |
| `deriv` | full suite | 0.191 | 0.197 | 1.113 |
| `deriv` | full suite, full GC before each | 0.174 | 0.178 | 1.006 |

A second run of the same script on the same machine gave the same ordering and a
smaller gap for `deriv`: 0.181 alone, 0.194 in the suite, 0.176 with a full GC
before each benchmark. The direction is stable; the size of the gap depends on
the state of the machine and the heap.

The collector state around the timed region explains the difference. Below is
`deriv`; `dderiv` behaves the same way. Each line is one child process.

| Mode | Run | g0 before | g1 before | g2 before | heap before (MiB) | g0 after | g1 after | g2 after | heap after (MiB) | allocated (MiB) | GC pause (s) |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| alone | 1 | 105 | 44 | 6 | 245.0 | 152 | 45 | 6 | 243.9 | 373.8 | 0.015 |
| alone | 2 | 105 | 44 | 6 | 245.2 | 152 | 45 | 6 | 244.0 | 373.8 | 0.014 |
| alone | 3 | 105 | 44 | 6 | 244.5 | 152 | 45 | 6 | 243.5 | 373.8 | 0.014 |
| alone | 4 | 105 | 44 | 6 | 244.8 | 152 | 45 | 6 | 243.8 | 373.8 | 0.013 |
| alone | 5 | 105 | 44 | 6 | 244.8 | 152 | 45 | 6 | 243.7 | 373.8 | 0.014 |
| suite | 1 | 175 | 52 | 7 | 37.7 | 222 | 52 | 7 | 36.7 | 373.8 | 0.028 |
| suite | 2 | 174 | 53 | 6 | 246.0 | 221 | 53 | 6 | 244.9 | 373.8 | 0.027 |
| suite | 3 | 174 | 51 | 6 | 245.9 | 221 | 51 | 6 | 244.8 | 373.8 | 0.028 |
| suite | 4 | 175 | 52 | 7 | 37.6 | 222 | 52 | 7 | 36.5 | 373.8 | 0.028 |
| suite | 5 | 175 | 54 | 7 | 37.7 | 222 | 54 | 7 | 36.6 | 373.8 | 0.028 |
| suite-gc | 1 | 190 | 66 | 24 | 34.4 | 236 | 66 | 24 | 41.4 | 373.8 | 0.010 |
| suite-gc | 2 | 190 | 66 | 24 | 34.3 | 236 | 66 | 24 | 41.3 | 373.8 | 0.010 |
| suite-gc | 3 | 190 | 66 | 24 | 34.4 | 236 | 66 | 24 | 41.4 | 373.8 | 0.011 |
| suite-gc | 4 | 190 | 66 | 24 | 34.4 | 236 | 66 | 24 | 41.4 | 373.8 | 0.010 |
| suite-gc | 5 | 190 | 66 | 24 | 34.4 | 236 | 66 | 24 | 41.3 | 373.8 | 0.010 |

For `dderiv`, the same pattern holds: 396.7 MiB allocated in every mode, GC
pause about 0.014 s alone, 0.029 s in the suite, 0.011 s with a full GC before
each benchmark, and generation 0 rising from 105 to 125 before the timed region
in the suite. The complete table is in `results`.

Suite cost:

| Mode | Min suite time (s) | Median suite time (s) | Min wall (s) | Median wall (s) |
| --- | ---: | ---: | ---: | ---: |
| suite | 2.382 | 2.429 | 10.192 | 10.319 |
| suite-gc | 3.367 | 3.392 | 11.117 | 11.198 |

Suite time is the sum of the timed benchmark regions; wall time is process
start to exit. The difference is the one-time core load and the load/compile of
`gabriel.lisp`.

## What explains it

The benchmark body did the same work in all three modes. `deriv` allocated
373.8 MiB and `dderiv` allocated 396.7 MiB in every run, and the generation 0
count rose by 46 or 47 during every timed region. No generation 1 or generation
2 collection happened inside any timed region. So the extra 0.020 s (11%) in
the full suite is not extra Lisp work and not a different number of collections
inside the benchmark.

It is the cost of the generation 0 collections that do happen. When `deriv`
starts alone, the pause attributed to its collections is about 0.014 s. When it
starts after the other benchmarks, the pause is about 0.028 s for the same 47
generation 0 collections. The extra 0.014 s accounts for most of the 0.020 s
timing difference. `dderiv` shows the same shift: 0.014-0.015 s alone,
0.029-0.030 s in the suite, for the same allocation.

The suite leaves different generation state. Before `deriv`, the suite has
already counted 175 generation 0 collections and 52 generation 1 collections,
against 105 and 44 alone. Those earlier benchmarks changed the collector's
generation budgets and the contents of the older generations, so each later
generation 0 collection is more expensive even though it collects a similar
amount of fresh garbage.

Heap size from `GC.GetTotalMemory(false)` is not the predictor. In the suite,
some runs show 37.6 MiB and some 245.9 MiB before `deriv`, depending on whether
a background collection had just finished, but the GC pause is 0.027-0.028 s in
both groups. A full `dotcl:gc` before each benchmark leaves the heap at about
34 MiB and lowers the `deriv` pause to 0.010 s, below even the alone case. The
collection counts before the benchmark are higher in this mode (190, 66, 24),
but the heap is compact and has little old garbage, so collection is cheap.

This is why "the generated code of `deriv` is identical" and "`deriv` alone
shows no difference" can both be true while the full-suite number moves: the
suite number measures the heap history of the twenty earlier benchmarks as much
as it measures `deriv`. A 7-11% shift in these runs is enough to hide or fake a
15-20% code-generation regression, and the shift is large enough to move an
acceptance threshold.

## Recommendation

Do not use a number from the full suite as if it were a property of the
benchmark. Either make the starting heap state fixed, or isolate each benchmark.

**Preferred: a full GC before each benchmark.** This costs about 0.05 s per
benchmark on this machine, 1.086 s of explicit collection over the 21 gabriel
benchmarks. It adds 0.963 s to the 2.429 s median suite time and 0.879 s to the
10.319 s median process wall time (about 8.5%). In exchange, the suite numbers
match the alone numbers to within the run-to-run noise (`dderiv` 0.206 vs
0.205 s, `deriv` 0.178 vs 0.177 s median). The runner should expose this as an
option and use it for any run whose numbers are compared, recorded or used as a
regression gate.

**Alternative: one process per benchmark.** This removes the order effect
completely and also isolates crashes and hangs. It is much more expensive: an
alone child process takes about 8.1 s wall on this machine, of which only about
0.18 s is `deriv`, because every child loads the core and compiles/JITs
`gabriel.lisp`. Twenty-one such processes cost about 170 s, against 10.3 s for
the full suite, roughly sixteen times slower. Use it for diagnosing a suspect
benchmark, or as an optional "isolated" mode, not as the default for the whole
suite.

Whichever is chosen, compare like with like: an alone run and a suite run must
use the same pre-benchmark GC rule. Changing the rule between an old and a new
build is the same mistake as measuring after a different set of benchmarks.
