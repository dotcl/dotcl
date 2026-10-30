# Benchmark numbers

This page collects the numbers from three measurements the project runs on
itself. Each measurement answers a different question:

| measurement | question | kind of number |
| --- | --- | --- |
| bench-parity (`bench/csharp-parity`) | How does fully declared Lisp compare with the same loop written in C#, on the same .NET runtime? | time ratio, dotcl / C# |
| il-parity (`bench/il-parity`) | Which IL instructions does dotcl emit that the C# compiler does not? | static instruction count |
| cl-bench (`bench/cl-bench-runner`) | How does dotcl compare with a native Common Lisp compiler on a general benchmark suite? | time ratio, dotcl / SBCL |

These are snapshots, each from one commit and one machine; the machine and the
date are given at the head of each section, and they differ between sections.
They are not a
general statement about dotcl's speed. Small kernels say little about a real
program, and time ratios change with the machine, the .NET version and the code
placement the JIT happens to choose. Read the "how it was measured" notes before
quoting a number.

## bench-parity: dotcl vs C#

Eight small kernels (`bench/csharp-parity/kernels.lisp`), each written once in
Common Lisp with every declaration a careful writer would give
(`(optimize (speed 3) (safety 0) (debug 0))`, fixnum / double-float / array /
struct slot types) and once in C# (`bench/csharp-parity/Program.cs`), over the
same inputs.

### How it was measured

- Machine: a Linux x86-64 KVM virtual machine with 1 vCPU and 5.8 GiB of
  memory. The host CPU model is not exposed to the guest.
- .NET SDK 10.0.401, runtime 10.0.12. Both sides run as Release builds.
- dotcl: the 0.1.31 development sources of 2026-09-30.
- Procedure (`make bench-parity`): 5 processes per side, alternating C# and
  dotcl. Inside each process every kernel runs one discarded warmup block and
  then 5 timed blocks, and the process reports the fastest block. The ratio
  uses the minimum over the 5 processes on each side.
- Per-process values are checked for a split into two groups (the JIT
  sometimes settles on a different code placement for a whole process;
  `bench/gate/modes.awk`). In this run every kernel on both sides formed one group.

### Result

Times are milliseconds for the whole repeated block. The ratio is dotcl / C#,
so below 1.00 means the dotcl figure was lower.

| kernel | C# min | C# median | dotcl min | dotcl median | ratio (min / min) |
| --- | ---: | ---: | ---: | ---: | ---: |
| tak | 190.0 | 193.7 | 546.2 | 610.4 | 2.87 |
| fib | 57.6 | 58.0 | 207.0 | 236.0 | 3.59 |
| fixnum-loop | 59.0 | 59.1 | 58.9 | 59.1 | 1.00 |
| double-loop | 118.9 | 119.1 | 118.8 | 118.9 | 1.00 |
| array-walk | 65.2 | 65.3 | 61.6 | 61.9 | 0.94 |
| struct-slots | 31.0 | 31.3 | 89.1 | 89.1 | 2.87 |
| string-walk | 64.8 | 65.0 | 66.3 | 66.4 | 1.02 |
| string-walk-vector | 65.8 | 65.9 | 66.3 | 66.4 | 1.01 |

The same ratios from three earlier runs on 2026-09-29, each from the development
sources of that time and on the same kind of machine, to show how far they move
between runs. Code changes
and run-to-run noise are mixed in these; they are not separated here.

| kernel | 2026-09-29, run 1 | 2026-09-29, run 2 | 2026-09-29, run 3 | 2026-09-30 |
| --- | ---: | ---: | ---: | ---: |
| tak | 2.87 | 2.92 | 3.21 | 2.87 |
| fib | 3.67 | 3.66 | 3.92 | 3.59 |
| fixnum-loop | 0.99 | 1.00 | 1.00 | 1.00 |
| double-loop | 0.99 | 1.00 | 1.00 | 1.00 |
| array-walk | 0.94 | 0.94 | 0.94 | 0.94 |
| struct-slots | 3.01 | 2.88 | 2.87 | 2.87 |
| string-walk | 0.98 | 1.02 | 1.02 | 1.02 |
| string-walk-vector | 0.97 | 1.01 | 1.00 | 1.01 |

### Reading it

- The loop kernels over fixnums, doubles, arrays and strings land within a few
  percent of C# in these runs.
- The two call-heavy kernels (tak, fib) and the struct slot kernel are about 3x
  to 4x the C# time. tak also spreads the most between processes of the same
  run (546 to 728 ms).
- A kernel's figure depends on its position in the file, because the kernels
  ahead of it warm the process up. The file header in `kernels.lisp` explains
  this; do not cut one kernel out and compare it with these numbers.

## il-parity: emitted IL vs C#

Six small data structures (`bench/il-parity/<case>/`) written in C# and in fully
declared Common Lisp. Both sides are compiled to IL assemblies (the C# side by
the C# compiler in Release, the Lisp side by dotcl's `compile-file`), and a
reader counts the IL instructions in each method body. Before any count is
reported, a self-check confirms that both halves compute the same number.

### How it was measured

- Machine: Apple M1, macOS 27.0, .NET SDK 10.0.401. The counts do not depend on
  the machine: the same compiler and source give the same IL.
- dotcl: the 0.1.31 development sources of 2026-09-30, `make il-parity`.
- All six self-checks agreed with the C# values.

### Result

Total IL instructions per method body.

| case | C# method | Lisp function | C# | dotcl |
| --- | --- | --- | ---: | ---: |
| stack | Stack.Push | ILP-STACK-PUSH | 14 | 45 |
| stack | Stack.Pop | ILP-STACK-POP | 12 | 29 |
| stack | Stack.Peek | ILP-STACK-PEEK | 7 | 30 |
| hash | HashTable.Hash | ILP-HASH-INDEX | 12 | 22 |
| hash | HashTable.Put | ILP-HASH-PUT | 63 | 274 |
| hash | HashTable.Get | ILP-HASH-GET | 39 | 176 |
| heap | Heap.Push | ILP-HEAP-PUSH | 49 | 214 |
| heap | Heap.Pop | ILP-HEAP-POP | 75 | 247 |
| ring | Ring.Enqueue | ILP-RING-ENQUEUE | 23 | 66 |
| ring | Ring.Dequeue | ILP-RING-DEQUEUE | 21 | 51 |
| ring | Ring.Peek | ILP-RING-PEEK | 5 | 26 |
| tokenizer | Tokenizer.NextToken | ILP-TOK-NEXT | 53 | 267 |
| tokenizer | Tokenizer.Digest | ILP-TOK-DIGEST | 64 | 329 |
| vec2 | Vec2.Length | ILP-VEC2-LENGTH | 12 | 17 |
| vec2 | Vec2.Normalize | ILP-VEC2-NORMALIZE | 31 | 55 |
| vec2 | Vec2.Dot | ILP-VEC2-DOT | 11 | 16 |

`make il-parity` also prints, per method, the counts by category (box, unbox,
castclass, call, field, element) and the list of dotcl instructions with no
counterpart in the C# body.

### Reading it

An instruction count is evidence about emitted size, not about time.
`bench/il-parity/README.md` records cases where the count went up while the
program got faster (a loop body emitted twice so that a check moves out of the
loop) and cases where it stayed flat while the program got faster. Do not
convert these counts into a speed estimate.

## cl-bench: dotcl vs SBCL

The cl-bench suite (github.com/benkard/cl-bench), run unchanged on dotcl and
on SBCL. This section comes from a different run, on a different machine, than
the two sections above. The numbers between the `ledger:begin` and
`ledger:end` markers in this file's source are generated by
`bench/ledger/doc.sh` from one run of the ledger (`bench/ledger/`), and are
rewritten together when a release is prepared.

### How it was measured

<!-- ledger:begin setup -->
- Machine and date: Intel Core i5-8350U CPU @ 1.70GHz (8 logical CPUs, Linux x86_64, .NET 10.0.14), measured on 2026-10-30.
- dotcl: the development sources at commit e91e19f9 (after v0.1.35). Release build, with `DOTCL_NO_JIT_PROFILE=1` and `DOTNET_gcServer=0`.
  SBCL 2.6.8 (installed through Roswell), started with `--dynamic-space-size 4096`.
- cl-bench revision 6e1b6e62.
<!-- ledger:end setup -->
- Procedure (`bench/ledger/run.sh`): every benchmark is its own process on each
  side. dotcl and SBCL alternate three times and the fastest cold value of each
  side is kept. Processes run one at a time under a lock, so nothing else on the
  machine runs during a measurement.
- "Cold" is one run of the benchmark in a fresh process: the benchmark repeats
  its own body the number of times cl-bench sets (the "runs" column) and the
  total is timed once, so JIT compilation on dotcl is included. The "suite"
  column comes from a separate pair of processes that run the whole suite in
  one process each, so earlier benchmarks have already warmed the runtime when
  a later one starts. The ledger also measures a warm form (one warm-up and
  three timed runs in the same process, minimum), for a subset of the
  benchmarks, and a steady-state column: its own
  process repeats the benchmark until three consecutive runs are within 5% of
  each other after at least ten runs (with caps on runs and time), and records
  the minimum of those runs. The minimum keeps the JIT's early tiers from
  passing for the steady state. The steady column is not shown here; see
  [bench/ledger/README.md](../bench/ledger/README.md).
- The ratio is dotcl milliseconds / SBCL milliseconds. A benchmark without a
  ratio (SBCL reported 0 ms, or one side failed) is left out of the statistics.

### Result

<!-- ledger:begin cl-bench -->
| statistic | cold | suite |
| --- | ---: | ---: |
| benchmarks with a ratio | 56 | 56 |
| geometric mean of the ratios | 2.81 | 2.61 |
| median of the ratios | 3.45 | 2.72 |
| total dotcl time / total SBCL time | 1.58 | 1.52 |

Without a cold ratio: `puzzle` (SBCL reported 0 ms).

The 4 longest benchmarks on dotcl (`string-concat`, `strings/adjustable`, `3d-arrays` and `clos/methodcalls`) are 47% of the dotcl total in the cold column, at 0.89x to 5.19x of the SBCL time.

| group | benchmark | runs | dotcl ms | SBCL ms | ratio (cold) | ratio (suite) |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| gabriel | tak | 100 | 22 | 20 | 1.10 | 1.05 |
| gabriel | takl | 10 | 43 | 18 | 2.39 | 2.50 |
| gabriel | stak | 50 | 261 | 32 | 8.16 | 8.41 |
| gabriel | ctak | 50 | 45 | 5 | 9.00 | 9.20 |
| gabriel | trtak | 100 | 26 | 21 | 1.24 | 1.29 |
| gabriel | boyer | 10 | 266 | 45 | 5.91 | 5.78 |
| gabriel | browse | 5 | 213 | 35 | 6.09 | 5.92 |
| gabriel | dderiv | 50 | 333 | 74 | 4.50 | 5.88 |
| gabriel | deriv | 50 | 267 | 67 | 3.99 | 2.49 |
| gabriel | destructive | 50 | 130 | 36 | 3.61 | 2.91 |
| gabriel | div2-test-1 | 50 | 93 | 36 | 2.58 | 1.75 |
| gabriel | div2-test-2 | 50 | 144 | 53 | 2.72 | 4.24 |
| gabriel | fft | 10 | 20 | 3 | 6.67 | 8.00 |
| gabriel | frpoly/fixnum | 30 | 209 | 30 | 6.97 | 7.56 |
| gabriel | frpoly/bignum | 10 | 254 | 27 | 9.41 | 8.23 |
| gabriel | frpoly/float | 30 | 291 | 43 | 6.77 | 5.72 |
| gabriel | puzzle | 5 | 64 | 0 | - | - |
| gabriel | triangle | 1 | 294 | 67 | 4.39 | 4.51 |
| gabriel | traverse | 5 | 395 | 155 | 2.55 | 2.28 |
| gabriel | fprint/ugly | 50 | 186 | 69 | 2.70 | 2.03 |
| gabriel | fprint/pretty | 20 | 183 | 80 | 2.29 | 1.69 |
| math | factorial | 1000 | 212 | 53 | 4.00 | 2.43 |
| math | fib | 50 | 68 | 58 | 1.17 | 1.13 |
| math | fib-ratio | 500 | 29 | 7 | 4.14 | 3.62 |
| math | ackermann | 1 | 1139 | 949 | 1.20 | 1.33 |
| math | mandelbrot/complex | 100 | 157 | 101 | 1.55 | 1.85 |
| math | mandelbrot/dfloat | 100 | 9 | 2 | 4.50 | 4.50 |
| math | mrg32k3a | 20 | 1363 | 227 | 6.00 | 3.83 |
| math | crc40 | 2 | 1112 | 235 | 4.73 | 3.78 |
| bignum | bignum/elem-100-1000 | 1 | 59 | 21 | 2.81 | 2.90 |
| bignum | bignum/elem-1000-100 | 1 | 99 | 44 | 2.25 | 1.91 |
| bignum | bignum/elem-10000-1 | 1 | 112 | 22 | 5.09 | 1.91 |
| bignum | bignum/pari-100-10 | 1 | 16 | 6 | 2.67 | 2.83 |
| bignum | bignum/pari-200-5 | 1 | 40 | 18 | 2.22 | 1.94 |
| bignum | pi-decimal/small | 100 | 438 | 191 | 2.29 | 2.23 |
| bignum | pi-decimal/big | 2 | 187 | 87 | 2.15 | 2.39 |
| bignum | pi-atan | 200 | 669 | 317 | 2.11 | 2.31 |
| bignum | pi-ratios | 2 | 451 | 440 | 1.02 | 0.94 |
| hash | hash-strings | 2 | 285 | 55 | 5.18 | 2.71 |
| hash | hash-integers | 10 | 304 | 71 | 4.28 | 4.28 |
| arrays | 1d-arrays | 1 | 102 | 15 | 6.80 | 6.40 |
| arrays | 2d-arrays | 1 | 1454 | 204 | 7.13 | 6.56 |
| arrays | 3d-arrays | 1 | 2839 | 547 | 5.19 | 4.25 |
| arrays | bitvectors | 3 | 242 | 100 | 2.42 | 1.29 |
| arrays | strings | 1 | 127 | 678 | 0.19 | 0.16 |
| arrays | strings/adjustable | 1 | 3546 | 834 | 4.25 | 2.21 |
| arrays | string-concat | 1 | 7721 | 8312 | 0.93 | 0.73 |
| arrays | search-sequence | 1 | 820 | 157 | 5.22 | 3.23 |
| clos | clos/defclass | 1 | 438 | 112 | 3.91 | 2.74 |
| clos | clos/defmethod | 1 | 120 | 476 | 0.25 | 0.28 |
| clos | clos/instantiate | 2 | 650 | 198 | 3.28 | 3.22 |
| clos | clos/simple-instantiate | 200 | 135 | 28 | 4.82 | 2.93 |
| clos | clos/methodcalls | 5 | 1602 | 1807 | 0.89 | 1.00 |
| clos | clos/method+after | 2 | 1394 | 1879 | 0.74 | 3.68 |
| clos | clos/complex-methods | 5 | 441 | 1775 | 0.25 | 1.08 |
| clos | clos/eql-fib | 2 | 116 | 41 | 2.83 | 2.36 |
| richards | richards | 5 | 1330 | 281 | 4.73 | 4.45 |

- Cold, 6 of the 56 benchmarks came out faster on dotcl (`strings` 0.19, `clos/defmethod` 0.25, `clos/complex-methods` 0.25, `clos/method+after` 0.74, `clos/methodcalls` 0.89 and `string-concat` 0.93), 6 are between 1x and 2x, 44 between 2x and 10x, and 0 above 10x.
- The largest cold ratios: `frpoly/bignum` 9.41, `ctak` 9.00, `stak` 8.16, `2d-arrays` 7.13, `frpoly/fixnum` 6.97 and `1d-arrays` 6.80.
- 5 benchmarks have an SBCL time under 10 ms: `ctak` (5 ms), `fft` (3 ms), `fib-ratio` (7 ms), `mandelbrot/dfloat` (2 ms) and `bignum/pari-100-10` (6 ms).
- The suite ratio is lower than the cold ratio for 35 of the 56 benchmarks that have both.
<!-- ledger:end cl-bench -->

### Reading it

- The total-time ratio is dominated by the few long benchmarks. The geometric
  mean and the median weigh every benchmark equally, short ones included.
- `ctak` uses catch/throw, which dotcl implements with .NET exceptions; this is
  why it stays among the largest ratios.
- When the SBCL time is under 10 ms, one timer tick is a large part of the
  figure, and the ratio can move by 30% from run to run. Some other rows land
  in one of two clusters of values from run to run on the same machine (for
  example `div2-test-1`), which moves their ratio by about 20%; the minimum of
  three rounds does not remove that.
- The suite ratio is often lower than the cold ratio. Earlier benchmarks in the
  same process have already warmed the runtime, which is a likely reason,
  though it was not isolated. A script that starts, runs once and exits looks
  like the cold column.
- The rows where dotcl is faster have mostly been method definition, string
  handling and allocation. They say nothing about compiled arithmetic or
  calls.
- These are specific benchmarks on one machine and one day, against one
  SBCL. They are not a prediction for another program or another machine.

## Reproducing

From a checkout with the .NET 10 SDK:

```
make bench-parity     # bench-parity; also runs the ratio check
make il-parity        # il-parity; deterministic counts
make bench-state      # cl-bench on dotcl and SBCL (SBCL via SBCL_RUN, default: ros -L sbcl-bin run)
bench/ledger/run.sh   # the cl-bench table above (cold and suite), plus the other dotcl-vs-SBCL rulers
bench/ledger/doc.sh out/ledger-<machine>   # rewrite the cl-bench numbers above from one ledger run
```

`bench/gate/check-ratios.sh` compares the ratios with the per-machine baselines in
`bench/gate/ratio-baseline.json`. Those baselines belong to one fixed machine, so a
run elsewhere should be compared with itself (two builds measured alternately
on the same machine), not with the numbers on this page.

## Regression suite time

`make regression-time` runs the compiled regression suite the way
`make test-regression` does, one file at a time, and records its wall time in
`bench-state.json` as `regression/compiled`. The suite compiles thousands of
small forms and runs them, so the number is mostly compile time, which none of
the measurements above looks at. `bench/gate/regression-time.sh` judges the time
of each file against the table `bench/gate/regression-file-times.tsv`: a file
that became more than 2 s and 1.5 times slower fails, and so does the whole
suite when its time per test is over 1.15 times what the table predicts (new
files are counted at their own measured time). A new file over 10 s gets a
warning. The times are in seconds, not ratios, so only a table from the same
machine says anything; a machine without rows gets UNJUDGED, as in the ratio
check. The suite has to pass for its time to be judged.
`make regression-time-baseline` runs the suite twice on the machine and writes
the smaller time of each file into the table.
