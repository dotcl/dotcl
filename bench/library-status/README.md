# library-status

The pipeline behind `docs/library-status.md`: which Common Lisp libraries run on
dotcl, ordered by how much of Quicklisp depends on them. The table is generated,
never hand-edited, so that keeping it current is a command rather than a
judgement call.

Three stages. Stage 1 and 3 are pure data handling and run on any Lisp; stage 2
(loading, and 2b, the test suites) is the measurement and runs on dotcl.

```
  dist metadata            targets.txt              results.json + annotations.json
 (systems/releases)  -->   targets.tsv     -->     (one entry per   -->  docs/library-status.md
      rank.lisp                                     system tried)         render.lisp
                                                     stage 2
                                                   tests.json
                                                     stage 2b
```

## Stage 1 -- pick the targets (`rank.lisp`)

```
sbcl --non-interactive --load bench/library-status/rank.lisp
```

Reads `distinfo.txt`, `systems.txt` and `releases.txt` from a Quicklisp dist
already on disk (`DOTCL_QL_DIST` overrides the search) and writes:

- `targets.txt` -- one system name per line, most depended upon first. This is
  the row order of the published table and the work list for stage 2.
- `targets.tsv` -- the same rows with rank, release and referrer count, so the
  order can be checked without rerunning anything.

What is counted is how many **other projects** name a project in a
`depends-on`, directly rather than transitively. One row per release: a library
and its test system are one thing to install, so they are one row. Nothing is
downloaded; with no dist on disk the script prints the three URLs to fetch.

Re-run it when a new dist comes out. The dist version is written into both files
and travels through to the published table.

The first rows include systems dotcl already bundles (`uiop`, for instance);
they are left in rather than special-cased, so the list stays a plain function of
the dist.

## Stage 2 -- try them (`run-quickload.sh`)

```
make library-status                       # stage 2 then stage 3
sh bench/library-status/run-quickload.sh  # stage 2 alone
```

Loads each system in `targets.txt` with dotcl and records what happened. It
rewrites `results.json` after every system, so the file is valid at every moment
and an interrupted run is not lost: `LIBRARY_STATUS_RESUME=1` continues, skipping
the systems already recorded. `LIMIT=N` stops after N systems -- `LIMIT=1` is the
way to check the pipeline without paying for the whole list, and it wants its
outputs redirected so a smoke test does not overwrite the real ones:

```
make library-status LIMIT=1 \
  LIBRARY_STATUS_JSON=/tmp/smoke.json LIBRARY_STATUS_OUT=/tmp/smoke.md
```

The run takes roughly half an hour for 150 systems on a warm cache. What it does
and why, since each rule below came from something that went wrong:

- **Use a checkout dedicated to the run.** dotcl keeps a separate worktree for
  library bring-up precisely so that a build in the main tree cannot fight it
  over the same runtime DLLs. Build it first (`make cross-compile build`) so
  that the FASLs and the runtime come from the same commit -- a stale
  `cil-out.sil` against a fresh `runtime.dll` produces failures that belong to
  neither.
- **One at a time.** Never run two dotcl processes in parallel for this: the
  results stop being comparable and the machine stops being usable.
- **Bound each system, not the whole run.** 120 seconds per system
  (`LIBRARY_STATUS_TIMEOUT`) covers everything that works; a system that exceeds
  it is recorded as `fail` with a note saying it timed out. Only the process the
  script started is bounded (`timeout`), never a kill by process name -- other
  work on the same machine uses the same executable name.
- Clear the FASL cache and any cached ASDF output between versions of dotcl, or
  a rebuilt compiler will be measured through yesterday's FASLs. Three places
  hold them: the ASDF cache (keyed by dotcl version, so it takes care of
  itself), FASLs sitting next to dist sources, and `contrib/*/*.fasl` -- the
  last is the one that bites, because dotcl only *warns* that a contrib FASL
  came from another core and then uses it anyway. `make compile-contrib-fasls`
  before a run.

Write one JSON object per system tried:

```json
[
  {"system": "alexandria",
   "release": "alexandria-20241012-git",
   "status": "load-only",
   "issue": null,
   "checked": "2026-09-14",
   "note": ""}
]
```

| field | meaning |
| --- | --- |
| `system` | the system named in `targets.txt` |
| `release` | the dist release it came from (`targets.tsv` has it) |
| `status` | `load-only` (loads), `patched` (loads from the patched release in the dotcl dist overlay), `fail`. `ok` is not written here: the table shows a loading row as `ok` when stage 2b recorded a `pass` for it |
| `issue` | the issue tracking the failure, or `null` |
| `checked` | the date of the measurement, `YYYY-MM-DD` |
| `note` | one short line: what failed, which test framework ran, why it is patched |

The table is published, so **`issue` must name an issue in the public
`dotcl/dotcl` repository**. Anything tracked where a reader of the table cannot
open it leaves the field `null` and explains itself in `note` instead.

`note` is a **machine field**: every run rewrites it from what that run saw. Do
not put a worked-out reason there -- the next measurement erases it. Reasons go
in `annotations.json` (below), which no run opens.

Error text carries whatever path the failure happened under, which is the
measuring machine's home directory and checkout. Both scripts replace absolute
paths in a note with `<path>` before writing it (`scrub_paths`), because the note
is published.

A system with no entry is rendered as `not checked`, so a partial run is
publishable: it says what has been measured rather than implying the rest is
fine. `results.sample.json` is a three-row example of the shape, not data.

## Stage 2b -- run the test suites (`run-tests.sh`)

```
make library-status-tests                  # stage 2b then stage 3
sh bench/library-status/run-tests.sh       # stage 2b alone
```

Stage 2 says whether a system loads. Whether it *works* is what its own test
suite says, and there is no one command for that across libraries: ASDF's
`test-system` runs whatever the system's `test-op` does, and which framework
that is, whether a failure signals, and whether anything runs at all is up to
each library. So this stage is two parts:

1. For each system that loads (`load-only` or `patched` in `results.json`, in
   `targets.txt` order), a fresh dotcl quickloads it and the systems its
   `test-op` depends on (Quicklisp fetches only through `quickload`), then calls
   `(asdf:test-system SYS)`.
2. The log is judged afterwards, outside the process, by one small recogniser
   per framework reading the summary that framework prints: fiveam (`Did N
   checks` / `Pass:` / `Fail:`), rt (`No tests failed.` / `K out of M total
   tests failed`), rove and prove (`N tests completed` / `K of M tests
   failed`), parachute (`;; Summary:` / `Passed:` / `Failed:`), stefil and
   hu.dwim.stefil (`#<test-run: N tests, A assertions, F failures`; when a
   test op runs the suite without printing that line, the driver prints the
   library's `*LAST-TEST-RESULT*` after `LIBTEST-STEFIL-RESULT`, read only
   when the output had no summary of its own), fiasco
   (the per-test `[ OK ]` / `[FAIL]` lines and `Test run had N failures:`),
   clunit and clunit2 (`Tested N assertions.` / `Passed: P/N` / `Failed:` /
   `Errors:`), Try (the `#<TRY:TRIAL (NAME) OUTCOME 1.2s COUNTS>` line the test
   op prints, whose per-category counts are read). Only output
   after the driver's `LIBTEST-BEGIN` marker is read, so a summary printed
   while loading is not a result.

Each system gets one verdict in `tests.json`:

| verdict | meaning |
| --- | --- |
| `pass` | a recognised summary counted at least one passing check and no failing one, and the run finished normally |
| `fail` | a recognised summary counted at least one failure |
| `error` | `test-system` signalled, the debugger was entered, or the process exited abnormally, with no recognised failure count |
| `no-result` | the run finished but printed nothing a recogniser knows |
| `timeout` | the bound (`LIBRARY_STATUS_TEST_TIMEOUT`, default 900 s) was hit |
| `load-fail` | the system or one of its test systems did not load |

**Only `pass` turns a row into `ok`.** `no-result` is the verdict this stage
exists to keep separate: a `test-op` that runs nothing, or a framework with no
recogniser yet, finishes cleanly and prints nothing that can be counted, and
that is "nothing was looked at", not "nothing went wrong". A library whose
framework is not recognised stays `load-only` until a recogniser is added --
add one to `judge` in the script rather than special-casing the library.

`tests.json` is kept apart from `results.json` so that load checks and test runs
can be redone independently; `render.lisp` merges them and shows the verdict in
the `tests` column. The same rules as stage 2 apply: one system per process,
serial, each bounded, `LIBRARY_STATUS_RESUME=1` and `LIMIT=N` work the same way,
and `LIBRARY_STATUS_TEST_TARGETS` names a file of systems to run instead of
every loading row. Logs go to `out/library-status-tests/` in the checkout.

A failing suite is not proof of a dotcl bug. Before reporting one, run the same
`(asdf:test-system SYS)` on SBCL: a test that fails there too, or depends on the
network or a native library the host lacks, belongs to the library or the host.

## The reasons (`annotations.json`)

```json
{"swank": "The Quicklisp dist pins SLIME v2.32, whose swank-loader ...",
 "stefil": "Fails only because it loads swank, and for swank's reason."}
```

One sentence per system, written by hand, rendered as the `why` column next to
the machine-generated `note`. `run-quickload.sh` never reads or writes this
file, which is the point: a re-measurement replaces `note` and leaves `why`
alone.

- Keys beginning with `//` are comments -- JSON has no comment syntax and the
  file carries its own instructions.
- Each sentence is **published verbatim**, so: English, no internal issue
  numbers, and a `dotcl/dotcl#N` reference only when N is public.
- Say why the row looks the way it does. What the error was is already in
  `note`; a row that needs no explanation gets no entry.
- The cell is Markdown prose (only `|` is escaped for you), so put identifiers
  and error text in backticks -- `` `*SYSDEP-FILES*` `` rather than a bare
  `*SYSDEP-FILES*`, which Markdown reads as emphasis and swallows.

## Stage 3 -- render the table (`render.lisp`)

```
LIBRARY_STATUS_JSON=path/to/results.json DOTCL_VERSION=0.1.28 \
  sbcl --non-interactive --load bench/library-status/render.lisp
```

Writes `docs/library-status.md`: the rows of `targets.txt` in order, each with
the status from the JSON, and any measured system that is not in the target list
appended after them. The dist version comes from `targets.txt` and the dotcl
version from `DOTCL_VERSION`; both are printed at the top of the table, because
a status table without the two versions it was produced against is not a claim
about anything.

The `note` column is rendered as a code span, because it holds raw error text
and Markdown reads raw error text: `Unbound variable: *SYSDEP-FILES*` came out
as "Unbound variable: SYSDEP-FILES" in italics until it did. The `why` column
beside it is the hand-written sentence from `annotations.json`.

Inputs and outputs all have defaults relative to this directory and can be
overridden: `LIBRARY_STATUS_JSON`, `LIBRARY_STATUS_TARGETS`,
`LIBRARY_STATUS_ANNOTATIONS`, `LIBRARY_STATUS_OUT`, `DOTCL_VERSION`.
`library-status/render:main` takes the same five as keyword arguments.

The JSON reader is part of the script. Rendering the table has to work in a
checkout where nothing has been installed yet, which a dependency on a JSON
library would break.

## Not automated yet

- Test frameworks without a recogniser (lisp-unit, lisp-unit2, ptester, and
  suites that print their own format) leave their rows at
  `no-result`, which renders as `load-only`.
- Stage 1 is still its own run: choosing targets needs a dist on disk and a new
  dist is rare, so `make library-status` deliberately does stages 2 and 3 only.
