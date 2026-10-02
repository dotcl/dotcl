# Benchmark numbers

This page collects the numbers from three measurements the project runs on
itself. Each measurement answers a different question:

| measurement | question | kind of number |
| --- | --- | --- |
| bench-parity (`bench/csharp-parity`) | How does fully declared Lisp compare with the same loop written in C#, on the same .NET runtime? | time ratio, dotcl / C# |
| il-parity (`bench/il-parity`) | Which IL instructions does dotcl emit that the C# compiler does not? | static instruction count |
| cl-bench | How does dotcl compare with a native Common Lisp compiler on a general benchmark suite? | time ratio, dotcl / SBCL |

These are snapshots from one commit on one or two machines. They are not a
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
  `bench/modes.awk`). In this run every kernel on both sides formed one group.

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
on SBCL.

### How it was measured

- Machine and date: the same machine and the same run as the bench-parity
  result above (Linux x86-64 KVM, 1 vCPU; the 0.1.31 development sources of
  2026-09-30).
- dotcl as a Release build on .NET 10.0.12. SBCL 2.6.7 (installed through
  Roswell).
- Procedure (`make bench-state`): one process per implementation, each running
  the whole suite once. Each benchmark repeats its own body the number of
  times cl-bench sets (the "runs" column) and reports the total. There is no
  min-of-N over processes, so a single benchmark's ratio is noisier than the
  bench-parity ratios.
- The ratio is dotcl seconds / SBCL seconds. Two benchmarks have no ratio:
  `puzzle` (SBCL reported 0.000 s) and `hash-strings` (the benchmark itself
  signalled an unbound-variable error under SBCL). That leaves 55.

### Result

Median of the 55 ratios: **4.90** in this run. In the three earlier runs
listed under bench-parity the median was 5.10, 5.80 and 5.20.

| group | benchmark | runs | dotcl s | SBCL s | ratio |
| --- | --- | ---: | ---: | ---: | ---: |
| gabriel | tak | 100 | 0.038 | 0.029 | 1.3 |
| gabriel | takl | 10 | 0.399 | 0.019 | 21.0 |
| gabriel | stak | 50 | 1.803 | 0.043 | 41.9 |
| gabriel | ctak | 50 | 3.143 | 0.006 | 523.8 |
| gabriel | trtak | 100 | 0.042 | 0.028 | 1.5 |
| gabriel | boyer | 10 | 0.643 | 0.078 | 8.2 |
| gabriel | browse | 5 | 0.309 | 0.034 | 9.1 |
| gabriel | dderiv | 50 | 0.550 | 0.061 | 9.0 |
| gabriel | deriv | 50 | 0.448 | 0.048 | 9.3 |
| gabriel | destructive | 50 | 0.272 | 0.028 | 9.7 |
| gabriel | div2-test-1 | 50 | 0.099 | 0.024 | 4.1 |
| gabriel | div2-test-2 | 50 | 0.425 | 0.041 | 10.4 |
| gabriel | fft | 10 | 0.073 | 0.003 | 24.3 |
| gabriel | frpoly/fixnum | 30 | 0.441 | 0.027 | 16.3 |
| gabriel | frpoly/bignum | 10 | 0.348 | 0.027 | 12.9 |
| gabriel | frpoly/float | 30 | 0.959 | 0.037 | 25.9 |
| gabriel | puzzle | 5 | 0.034 | 0.000 | - |
| gabriel | triangle | 1 | 0.471 | 0.060 | 7.8 |
| gabriel | traverse | 5 | 1.174 | 0.148 | 7.9 |
| gabriel | fprint/ugly | 50 | 0.299 | 0.073 | 4.1 |
| gabriel | fprint/pretty | 20 | 0.123 | 0.073 | 1.7 |
| math | factorial | 1000 | 0.213 | 0.049 | 4.3 |
| math | fib | 50 | 0.757 | 0.070 | 10.8 |
| math | fib-ratio | 500 | 0.068 | 0.006 | 11.3 |
| math | ackermann | 1 | 9.039 | 1.702 | 5.3 |
| math | mandelbrot/complex | 100 | 0.245 | 0.095 | 2.6 |
| math | mandelbrot/dfloat | 100 | 0.024 | 0.002 | 12.0 |
| math | mrg32k3a | 20 | 0.989 | 0.203 | 4.9 |
| math | crc40 | 2 | 1.616 | 0.269 | 6.0 |
| bignum | bignum/elem-100-1000 | 1 | 0.030 | 0.019 | 1.6 |
| bignum | bignum/elem-1000-100 | 1 | 0.060 | 0.041 | 1.5 |
| bignum | bignum/elem-10000-1 | 1 | 0.093 | 0.024 | 3.9 |
| bignum | bignum/pari-100-10 | 1 | 0.008 | 0.004 | 2.0 |
| bignum | bignum/pari-200-5 | 1 | 0.028 | 0.013 | 2.2 |
| bignum | pi-decimal/small | 100 | 0.974 | 0.152 | 6.4 |
| bignum | pi-decimal/big | 2 | 0.671 | 0.073 | 9.2 |
| bignum | pi-atan | 200 | 0.627 | 0.186 | 3.4 |
| bignum | pi-ratios | 2 | 0.359 | 0.406 | 0.9 |
| hash | hash-strings | 2 | 0.975 | error | - |
| hash | hash-integers | 10 | 0.630 | 0.142 | 4.4 |
| arrays | 1d-arrays | 1 | 0.094 | 0.017 | 5.5 |
| arrays | 2d-arrays | 1 | 0.673 | 0.192 | 3.5 |
| arrays | 3d-arrays | 1 | 2.058 | 0.514 | 4.0 |
| arrays | bitvectors | 3 | 0.322 | 0.135 | 2.4 |
| arrays | strings | 1 | 0.101 | 0.940 | 0.1 |
| arrays | strings/adjustable | 1 | 3.266 | 0.928 | 3.5 |
| arrays | string-concat | 1 | 8.818 | 9.511 | 0.9 |
| arrays | search-sequence | 1 | 0.596 | 0.155 | 3.8 |
| clos | clos/defclass | 1 | 0.270 | 0.168 | 1.6 |
| clos | clos/defmethod | 1 | 0.087 | 0.510 | 0.2 |
| clos | clos/instantiate | 2 | 2.721 | 0.221 | 12.3 |
| clos | clos/simple-instantiate | 200 | 0.279 | 0.077 | 3.6 |
| clos | clos/methodcalls | 5 | 1.018 | 1.711 | 0.6 |
| clos | clos/method+after | 2 | 0.729 | 0.328 | 2.2 |
| clos | clos/complex-methods | 5 | 2.641 | 0.178 | 14.8 |
| clos | clos/eql-fib | 2 | 0.982 | 0.058 | 16.9 |
| richards | richards | 5 | 2.708 | 0.377 | 7.2 |

### Reading it

- Most ratios sit between about 2x and 25x. `ctak` (catch/throw) and `stak`
  (special variables) are far outside that range.
- A few benchmarks came out faster on dotcl in this run (`strings`,
  `clos/defmethod`, `clos/methodcalls`, `pi-ratios`, `string-concat`). These
  are single measurements; they have not been repeated to rule out noise, and
  some of them spend most of their time in allocation or in method definition
  rather than in compiled code.
- Many of the SBCL times are a few milliseconds. At that size one timer tick is
  a large part of the figure, so the ratio has few significant digits.

## Reproducing

From a checkout with the .NET 10 SDK:

```
make bench-parity     # bench-parity; also runs the ratio check
make il-parity        # il-parity; deterministic counts
make bench-state      # cl-bench on dotcl and SBCL (SBCL via SBCL_RUN, default: ros -L sbcl-bin run)
```

`bench/check-ratios.sh` compares the ratios with the per-machine baselines in
`bench/ratio-baseline.json`. Those baselines belong to one fixed machine, so a
run elsewhere should be compared with itself (two builds measured alternately
on the same machine), not with the numbers on this page.
