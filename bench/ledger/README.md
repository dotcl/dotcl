# Ledger: dotcl against SBCL, every ruler in one table

`run.sh` re-measures all the rulers listed in `items.tsv` on dotcl and on SBCL
and writes one table of current values: cl-bench (cold, and warm for a subset),
the Coalton kernels, the C# parity kernels, the ironclad test suite (cold and
warm), and the imago bicubic kernel.

```
bench/ledger/run.sh                     # setup, measure, render
bench/ledger/run.sh render              # redo the table from out/raw.tsv
bench/ledger/remote.sh MACHINE          # the same on a machine from machines.tsv
LEDGER_ONLY='^coalton/' LEDGER_ROUNDS=1 bench/ledger/run.sh
```

Needs a Release runtime and `compiler/dotcl.core` in the tree
(`make cross-compile compile-asdf-fasl compile-quicklisp-fasl bench-build compile-core-fasl`), `ros` with `sbcl-bin`
2.6.8, and a Quicklisp home for dotcl. `setup` clones cl-bench and Coalton into
the work directory and builds the two ironclad source trees.

## Method

- Each row of `items.tsv` is its own process per side. dotcl and SBCL alternate
  `LEDGER_ROUNDS` times (3); the fastest value of each side is kept.
- `DOTCL_NO_JIT_PROFILE=1`, `DOTNET_gcServer=0`, Release.
- Every process runs under `flock $LEDGER_LOCK`, one at a time.
- A process that dies or prints no value is written as `failed`; the run goes on.
- The first process of Coalton, ironclad and imago per side fills the fasl cache
  and is not measured.

## Output

`out/results.md` (the table with the run header), `results.tsv`, `raw.tsv`
(every value), `results-detail.tsv` (per-test ironclad lines), `regressions.tsv`.
Rows whose ratio is 10% or more above the previous run (`LEDGER_PREV`, saved
with `LEDGER_SAVE=1`) are listed above the table.

`LEDGER_PUBLISH=1` replaces the body of the ledger issue with the table after
the run, keeping the old body as a comment. Adding or removing a ruler is a line
in `items.tsv`; `notes.tsv` holds per-row remarks shown in the table.
