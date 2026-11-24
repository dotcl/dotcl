# Ledger: dotcl against SBCL, every ruler in one table

`run.sh` re-measures all the rulers listed in `items.tsv` on dotcl and on SBCL
and writes one table of current values: cl-bench (cold, warm for a subset, and the whole
suite), the Coalton kernels, the C# parity kernels, the ironclad test suite
(cold and warm), and the imago bicubic kernel. Each row has a cold value and a steady-state value per side.

```
bench/ledger/run.sh                     # setup, measure, render
bench/ledger/run.sh render              # redo the table from out/raw.tsv
bench/ledger/remote.sh MACHINE          # the same on a machine from machines.tsv
LEDGER_ONLY='^coalton/' LEDGER_ROUNDS=1 bench/ledger/run.sh
```

Needs a Release runtime and `compiler/dotcl.core` in the tree, built by `bench/ledger/build.sh`
(it builds the runtime, then rebuilds `dotcl.core` and the asdf, quicklisp and contrib fasls that
depend on the runtime source; `bench/ledger/build.sh check` only checks, and `run.sh` checks before
it measures, so a core left from an earlier commit is refused instead of measured), `ros` with
`sbcl-bin` 2.6.8, and a Quicklisp home for dotcl whose dotcl dist is at least the version in
`dotcl-dist-min.txt` (`LEDGER_DOTCL_DIST_MIN` overrides it) and loads `float-features`; `run.sh check`
reports the version it found. `setup` clones cl-bench and Coalton into
the work directory and builds the two ironclad source trees.

## Cold and steady

Cold is the measurement a row already had before steady state existed: one
fresh process per side, with the ruler's own run count and warm-up. For cl-bench
that is one scalar run with the JIT included; the Coalton, parity and imago
rulers keep the one warm-up and minimum of five runs they always used. dotcl and
SBCL alternate `LEDGER_ROUNDS` times (3) and the fastest cold value of each side
is kept.

Steady is a separate measurement in one fresh process per side. The steady
value is the warm-up-free plateau: the fastest run of the first window of
`LEDGER_STEADY_WINDOW` (3) consecutive runs that are all within
`LEDGER_STEADY_TOLERANCE` (0.05, so 5%) of the smallest of them, taken after
`LEDGER_STEADY_MIN_RUNS` (10) runs and `LEDGER_STEADY_MIN_SECONDS` (8) seconds,
and not still dropping.

The benchmark is repeated in that process until such a window is reached.
Three rules keep the tiered JIT's slow early plateaus out of the value. The
minimum run count and the minimum time (measured from the end of loading the
benchmark's files, once per process) matter because .NET's tiered JIT can
produce several early runs that agree while it has not reached its final code
yet: a benchmark can sit on a plateau 20 to 200 percent slower than its final
speed for the first one to four seconds, and a shorter minimum declared that
plateau stable. The window is also rejected while it is still dropping: when
the last window's fastest run is more than the tolerance below the fastest run
of the window before it, the benchmark is still climbing to a faster tier.
Every run's time is kept (the `TIMES` line of the steady output, in order), so
a plateau can be read off afterwards. The minimum of the stable window and the
total number of runs taken are recorded. Repeating stops early when the
minimum time has passed and `LEDGER_STEADY_MAX_RUNS` (20) further runs have
been taken, or when `LEDGER_STEADY_MAX_SECONDS` (120) is reached; a row that
never stabilises is shown with a `steady: not stable` note. The same detection
is used on dotcl and SBCL, so the ratio compares like with like.

The two thresholds are set at the top of `run.sh` and can be overridden through
the environment:

- cold: `LEDGER_COLD_MAX` (10), the script-like limit;
- steady: `LEDGER_STEADY_MAX` (2), the target, and `LEDGER_STEADY_STRONG` (3),
  the point where a row needs a look.

In the table `!` marks a row whose cold ratio is over 10x or whose steady ratio
is over 2x; `!!` marks a row whose steady ratio is over 3x.

## Startup

`startup/empty` and `startup/hello` measure how long a fresh process takes to
start and run a script: an empty file, or a file that prints one line. One
measurement is the median wall time of `LEDGER_STARTUP_COUNT` (10) fresh
processes after one unmeasured process, taken by `startup-time.lisp` (a dotcl script); dotcl runs
with `--no-init`, SBCL with `--non-interactive --no-userinit`. The rounds
alternate the two sides as for every row and the fastest median is kept.

Startup has no steady state, so the steady columns show `-` and the row is never
marked. The cold ratio is kept and takes part in the comparison with the previous
run. SBCL is started directly, not through `ros` (which would add its own launch time):
`run.sh` looks for the newest `sbcl-bin/<version>/bin/sbcl` under `$LEDGER_ROSWELL_HOME`
(default `~/.roswell`) for the version `SBCL_CMD` reports, with its `sbcl.core`, then for `sbcl` on PATH.
If neither is found, the SBCL side of the startup rows is left empty (`-`) and only dotcl is recorded.

The ledger runs a non-ReadyToRun build, so startup here is larger than in the
shipped tool (about 200 ms shipped; 655 ms for dotcl on the Apple-silicon mac the
row was added on, against 14.5 ms for SBCL, fastest median of 3 rounds). The ratio
to SBCL is therefore large and is never marked. Only the trend between runs on the same
machine matters.

## Method

- Each row of `items.tsv` is its own process per side. dotcl and SBCL alternate
  `LEDGER_ROUNDS` times for cold and run once each for steady.
- `DOTCL_NO_JIT_PROFILE=1`, `DOTNET_gcServer=0`, Release.
- Every process runs under `flock $LEDGER_LOCK`, one at a time.
- A process that dies or prints no value is written as `failed`; the run goes on.
- The first process of Coalton, ironclad and imago per side fills the fasl cache
  and is not measured.

## Output

`out/results.md` (the table with the run header), `results.tsv`, `raw.tsv`
(every value), `results-detail.tsv` (per-test ironclad lines), `regressions.tsv`.
The table columns are: item, cold dotcl, cold SBCL, cold ratio, steady dotcl,
steady SBCL, steady ratio, runs to stability, mark, unit, how and note. `runs`
is written as `dotcl/SBCL` when the two sides needed a different number of runs.
`results.tsv` keeps the cold ratio in column 4, where `results-detail.tsv` keeps
per-test cold and steady values. The previous run is read from `LEDGER_PREV`
(default `baseline/MACHINE.tsv`); its cold ratio column is still column 4, so a
baseline written before the steady columns existed remains readable. Rows whose
cold ratio is 10% or more above the previous run (`LEDGER_PREV`, saved with
`LEDGER_SAVE=1`) are listed above the table. `remote.sh` sends
`baseline/MACHINE.tsv` as the previous run and saves the new one there;
`LEDGER_BASELINE=FILE` uses another local file for both, and `LEDGER_DEST=DIR`
copies the output somewhere other than `out/ledger-MACHINE/`.

`LEDGER_PUBLISH=1` replaces the body of the ledger issue with the table after
the run, keeping the old body as a comment. Adding or removing a ruler is a line
in `items.tsv`; `notes.tsv` holds per-row remarks shown in the table.

## The cl-bench numbers in docs/benchmarks.md

`doc.sh` rewrites the cl-bench section of `docs/benchmarks.md` from one ledger
run:

```
bench/ledger/doc.sh out/ledger-MACHINE  # results.md and results.tsv of that run
```

Only the text between `<!-- ledger:begin NAME -->` and `<!-- ledger:end NAME -->`
is replaced (`setup`: machine, date, dotcl, .NET and SBCL versions; `cl-bench`:
the summary, the per-benchmark table and the counts); the prose around the
markers is kept. Every block comes from the one run, so nothing from an earlier
run or another machine is left behind. Running it twice on the same input gives
the same file.

The numbers are not edited by hand. They are regenerated when a release is
prepared: the ledger is measured on the release's closing commit, and the
version commit, which changes only the version number and documents, carries
the regenerated page and the new baseline file. During a release, do not edit
`docs/benchmarks.md` by hand between that measurement and the version commit.
