# bench

The measurements dotcl runs on itself, so the numbers in
[docs/benchmarks.md](../docs/benchmarks.md) can be reproduced. One directory
per job:

- `cl-bench-runner/`: runs the cl-bench suites on dotcl and SBCL and merges the
  times into `bench-state.json` (`make bench`, `make bench-state`,
  `make bench-survey`).
- `csharp-parity/`: small kernels written in fully declared Lisp and in C#,
  timed on the same runtime (`make bench-parity`).
- `il-parity/`: the same data structures in C# and in Lisp, compared by the IL
  instructions each compiler emits (`make il-parity`).
- `gate/`: the per-machine baselines and the scripts that judge a run against
  them (`check-ratios.sh`, `regression-time.sh`), and the rule that detects a
  timing split into two groups (`modes.awk`, `process-modes.sh` for A/B runs).
- `ablation/`: C# variants of one kernel, each removing one thing dotcl does, to
  see what a gap is made of.
- `sil-shapes/`: a corpus of small forms for diffing the compiler's output
  before and after a change.
- `jit-minopts/`: counts the methods of compiled libraries that the JIT
  compiles without optimization because they are too large.
- `library-status/`: loads and tests Quicklisp libraries and renders
  [docs/library-status.md](../docs/library-status.md).
- `docker/`: a base image with dotcl and SBCL side by side, for measurements
  that need a fixed Linux environment.
- `probes/`: single-question scripts (allocation per call, fasl load cost,
  generic function dispatch) and a profile helper. Each file's header says how
  to run it and what its rows mean.
