# cl-bench runner

`run.lisp` runs the cl-bench suites on dotcl or SBCL. The `Makefile` targets
`bench`, `bench-state` and `bench-survey` start it for you:

```sh
make bench SUITE=gabriel
make bench SUITE=gabriel BENCH=deriv
make bench-survey SUITE=gabriel BENCH=deriv RUNS=5 WARMUP=1
```

## Full GC before each benchmark

A benchmark's time depends on the heap state left by the benchmarks before it.
`BENCH_GC_EACH=1` runs a full collection before every timed iteration, so each
iteration starts from a compact heap:

```sh
make bench SUITE=gabriel BENCH=deriv BENCH_GC_EACH=1
make bench-survey SUITE=gabriel BENCH=deriv RUNS=5 BENCH_GC_EACH=1
```

The collection runs before the timer and the allocation counter are read, so
its time is not charged to the benchmark. It applies to scalar mode and to
survey mode (including its warmup iterations). The default is off, which keeps
existing numbers comparable; use the option for any run whose numbers are
compared, recorded or used as a gate.

The option is the special variable `*bench-gc-each*`, so a direct run can set
it the same way as `*bench-suite*` and `*bench-name*`:

```sh
dotnet run --project runtime -- --asm compiler/cil-out.sil \
  --eval '(setq *bench-gc-each* t)' bench/cl-bench-runner/run.lisp
```

On SBCL the runner uses `sb-ext:gc :full t`; on dotcl it uses `dotcl:gc`.

## Compiled files instead of source

The runner loads each suite file as source, which compiles it a form at a
time. `BENCH_FASL_DIR=<dir>` compiles each file with `compile-file` into that
directory and loads the fasl instead, so the numbers include what file
compilation does (calls between functions of the same file, for one):

```sh
BENCH_FASL_DIR=out/bench-fasl make bench SUITE=richards
```
