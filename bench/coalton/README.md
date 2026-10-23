# Coalton runtime benchmark

Times code that Coalton generates, on dotcl and on SBCL, with the same Coalton
checkout. Coalton has no benchmark suite of its own; `kernels.lisp` holds small
programs in common Coalton styles, and `run.lisp` adds the ray tracer that ships
in Coalton's `examples/raytrace` (at 160x90, 4 samples, depth 6).

| Kernel | Shape |
| --- | --- |
| `poly-sum-integer`, `poly-sum-f64` | one polymorphic `(Num :a) (Ord :a)` loop, at Integer and F64 |
| `class-dispatch` | a user class with two instances, folded over lists |
| `ord-sort` | `list:sort` of 200k Integers |
| `tree-insert-fold` | an ADT search tree: insert 200k keys, then sum and depth by `match` |
| `optional-chain`, `result-chain` | `>>=` over `Optional` and `Result` |
| `list-ops` | `map`, `filter`, `fold` over a 100k-element list |
| `fib-integer` | naive recursion on Integer |
| `mandel-f64`, `f32-series` | float loops on F64 and F32 |
| `string-build` | `string:concat`, `<>`, `string:reverse`, `into` |
| `raytrace` | Coalton's own ray tracer: records, `Optional`, an ADT tree, F64 |

## Running

```
bench/coalton/compare.sh /path/to/coalton [out-dir]
```

Both Lisps need Quicklisp with Coalton's dependencies. The script sets
`COALTON_ENV=release` and `DOTCL_NO_JIT_PROFILE=1`, gives each Lisp its own ASDF
cache under the output directory (default `out/coalton-bench`), and prints:

```
kernel               dotcl ms    SBCL ms    ratio  checksum
```

Times are the minimum of 5 runs after one warm-up, with a full GC before each run.
`checksum` says whether both Lisps computed the same result. `make bench-coalton
COALTON_DIR=...` runs the same script after building the Release runtime.

To time one implementation, or profile it, load `run.lisp` directly;
`COALTON_BENCH_ONLY` selects kernels and `COALTON_BENCH_REPEAT` runs them again
after the timing, between `PROFILE-START` and `PROFILE-END` lines (see the
header of `run.lisp`).
