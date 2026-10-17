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
sh bench/library-status/fetch-ql-projects.sh out/quicklisp-projects
DOTCL_QL_PROJECTS=out/quicklisp-projects \
  sbcl --non-interactive --load bench/library-status/rank.lisp
```

Reads `distinfo.txt`, `systems.txt` and `releases.txt` from a Quicklisp dist
already on disk (`DOTCL_QL_DIST` overrides the search), and the
`projects/<name>/source.txt` files of a
[quicklisp-projects](https://github.com/quicklisp/quicklisp-projects) checkout
(`DOTCL_QL_PROJECTS`, required), and writes:

- `targets.txt` -- one system name per line, most depended upon first. This is
  the row order of the published table and the work list for stage 2.
- `targets.tsv` -- the same rows with rank, release, the dependent count the
  order comes from and the direct referrer count beside it, so the order can be
  checked without rerunning anything. Its header records the dist and the
  quicklisp-projects commit.

The unit is the project (one release): a library and its test system are one
thing to install, so they are one row. What is counted for a project P is its
**dependents**: every other project that is built on P, directly or through
other projects, and is owned by someone other than P's owner. Every project
with at least 4 dependents is a target (`THRESHOLD=N` changes the cut-off).
Three rules make that precise:

- **Transitive.** A library most of Quicklisp reaches through one popular
  intermediary is as load-bearing as one they name directly; counting direct
  referrers alone ranked it near the bottom.
- **Test systems are not followed.** A dependency written by a system whose
  name contains `test` or `bench` is left out: a library that only test suites
  pull in (a test framework's own helpers, say) is not something the ecosystem
  is built on. The rule is a name match, so it errs on the side of leaving an
  edge out; `rank.lisp` (`%TEST-SYSTEM-P`) is the definition.
- **Not the same owner.** An author who splits one program into twenty projects
  would otherwise put each of them at the top. The owner is read from where
  Quicklisp fetches the project (the first data line of its `source.txt`): the
  user or organisation on a shared host (github.com, gitlab.com, codeberg.org,
  ...), otherwise the host, with `kmr-git` and `ediware-http` mapped to the
  hosts they stand for. Different accounts are different owners even when one
  person runs both. A project with no `source.txt` shares an owner with nothing,
  and the run prints how many there were.

`fetch-ql-projects.sh DIR [COMMIT]` makes a detached checkout at the commit
matching the dist (the default is the last commit before quicklisp
2026-01-01); `rank.lisp` reads the commit back from `.git/HEAD` and writes it
into `targets.tsv`. Nothing else is downloaded; with no dist on disk the script
prints the three URLs to fetch.

Re-run it when a new dist comes out, with the quicklisp-projects commit for
that dist. The dist version is written into both files and travels through to
the published table.

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

The run takes roughly half an hour for 150 systems on a warm cache, so a few
hours for the whole list. What it does
and why, since each rule below came from something that went wrong:

- **Use a checkout dedicated to the run.** dotcl keeps a separate worktree for
  library bring-up precisely so that a build in the main tree cannot fight it
  over the same runtime DLLs. Build it first (`make cross-compile build
  bench-build compile-core-fasl compile-contrib-fasls`) so that the FASLs and the runtime come
  from the same commit -- a stale `cil-out.sil` against a fresh `runtime.dll`
  produces failures that belong to neither.
- **Measure the Release runtime.** That is the build a user runs, and a Debug
  build of the runtime is several times slower on the same work (the JIT
  treats a Debug assembly as debuggable), enough to turn a library that loads
  into one that times out. All three measuring scripts take
  `runtime/bin/Release/net10.0` (`runtime.sh`; `make bench-build` builds it,
  `LIBRARY_STATUS_CONFIG=Debug` measures the Debug build instead) and stop when
  it is older than any C# source under `runtime/`. The core is
  `compiler/dotcl.core`, the one the packs ship (`make compile-core-fasl`; the
  scripts stop when it is older than `compiler/cil-out.sil`), which starts
  about 1.5 seconds sooner per system than assembling `cil-out.sil`. The core
  and the fasls are IL written by dotcl's compiler and are the same for either
  build. Each entry
  records the build as `"runtime"`, and `render.lisp` prints it at the top of
  the table counted per row, the same way as the dists below. The load and
  test entries also record the wall-clock time of their run as `"seconds"`, to a tenth
  (a load that timed out and was retried counts both runs), so two
  measurements can be compared row by row.
- **Know which dists were measured.** All three measuring scripts read the
  versions of the `quicklisp` and `dotcl` dists in the Quicklisp home
  (`dists.sh`) and write them into every entry as `"dists"`; `render.lisp`
  prints them at the top of the table, counted per row: a row's load entry,
  its test entry and its with-forks entry together give the row's versions
  (an entry from before the field existed counts as not recorded, and entries
  that disagree make the row "differs between stages"). One version for every
  row is printed as that version; anything else is printed as `mixed:` with
  the number of rows behind each version, so a table that is partly an older
  measurement cannot read as a new one. Before measuring, the installed dotcl dist is
  compared with the published one (`https://dotcl.github.io/dist/dotcl.txt`,
  `LIBRARY_STATUS_DOTCL_DIST_URL` overrides it); any other version stops the
  run with the command that updates it, and so do a home with no dotcl dist and
  a published version that cannot be read (no network).
  `LIBRARY_STATUS_ALLOW_STALE_DIST=1` goes on anyway, and the entries then
  record the version that was measured. `make test-library-status-dists`
  checks this.
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
measuring machine's home directory and checkout, and sometimes the build that
measured it: a library that refuses an implementation it does not know says
`not yet implemented for dotcl 0.1.29+273.ge515cc0 on Arm64`. Both scripts pass
a note through `scrub_note` (`scrub.sh`) before writing it, because the note is
published: absolute paths become `<path>`, the version `<version>` and the
machine type after it `<arch>`. `check-scrub.sh` tests those filters and fails
if any of them has leaked into `results.json`, `tests.json` or the rendered
table; the make targets run it after rendering.

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
   `targets.txt` order), a fresh dotcl quickloads it and what the systems its
   `test-op` depends on depend on (Quicklisp fetches only through `quickload`).
   A test system missing from the Quicklisp index (it did not build when the
   dist was made) is looked for as `NAME.asd` under the library's directory.
   The Lisp runs in an empty directory, and from `LIBTEST-BEGIN` on
   `*default-pathname-defaults*` is the library's directory, since suites open
   their own files by relative names (LIFT's dribble and summary files then
   land in the release directory of the Quicklisp home measured).
   The test systems themselves are loaded after the `LIBTEST-BEGIN` marker,
   because many run their suite while loading (a `run!` at top level); then the
   driver prints `LIBTEST-TEST-OP` and calls `(asdf:test-system SYS)`. What was
   recognised while the test systems loaded counts only when the test op itself
   printed nothing a recogniser knows, so a suite that runs at load time and
   again from the test op is counted once.
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
   op prints, whose per-category counts are read), 1am (`Success: N tests, M
   checks.`; a failing check signals, so a failure shows up as an error), and
   cl-ppcre's own harness (`Test: NAME` per suite, `NNN:` per failing test,
   then `All tests passed.` or `Some tests failed.`; it prints no counts, so
   each suite counts as one), and lisp-unit2 (`Test Summary ... (N tests ...)`
   then `| P passed`, `| F failed`, `| E execution errors`; an execution
   error counts as a failure), lift (`Test Report for ...: N tests run, all
   passed!` or `..., F Failures.`, possibly on the line of lift's `Start:`
   progress; lift can print the same report twice, so the last one is counted), ptester, the harness cl-base64 and puri use
   (`Errors detected in this test: E` / `Successes this test:S`), lisp-unit
   (`Unit Test Summary` then `| P passed`, `| F failed`, `| E execution
   errors`), unit-test, used by cl-fad and yason (`N tests total.` then
   `P passed.`, `F failed.`, `C crashed.`; a crash counts as a failure), the
   `with-test-suite` harness of flexi-streams (cl-ppcre's shape with `Test
   suite: NAME` lines and `Test FORM failed.` per failing check), and
   should-test (one `Test NAME: ... OK` or `FAILED` per test, no summary),
   and the harness of trivial-package-local-nicknames (`;; N tests run, F
   failures.`).
   fiveam's `run` (rather than `run!`) prints only progress, `Running test
   NAME` followed by one character per check; those are counted when no `Did N
   checks.` summary was printed. LIFT, clunit and clunit2 keep their last
   result, and a test op that drops it prints only `Start:` or `PROGRESS:`
   lines, so the driver prints its counts as `LIBTEST-LAST-RESULT FRAMEWORK
   PASSED FAILED`, read only when the output had no summary of that
   framework. Parachute's `quiet` report prints nothing, so the driver prints
   its counts the same way. Only output after the driver's `LIBTEST-BEGIN` marker is read,
   so a summary printed while the library or its dependencies load is not a
   result.

   `run-tests.sh --judge LOG...` judges logs already written, without running
   anything. The driver is plain Common Lisp apart from loading Quicklisp,
   quitting, the debugger hook and the MOP package, so the same driver run on
   SBCL gives a log to compare a recogniser against. On SBCL the hook
   `--non-interactive` installs (`sb-ext:*invoke-debugger-hook*`) is unset
   while the test op runs: it runs before `*debugger-hook*`, so neither the
   driver's hook nor one a test binds would be called.

Each system gets one verdict in `tests.json`:

| verdict | meaning |
| --- | --- |
| `pass` | a recognised summary counted at least one passing check and no failing one, and the run finished normally |
| `fail` | a recognised summary counted at least one failure |
| `error` | `test-system` signalled, the debugger was entered, or the process exited abnormally, with no recognised failure count |
| `no-result` | the run finished but printed nothing a recogniser knows |
| `no-tests` | the run finished and there was nothing to run: neither `test-op` nor the test systems it depends on have a primary `perform` method of the library's own (only ASDF's empty default; `:around` methods such as the one LIFT adds to every system do not count), a test system it names is in no dist and has no `.asd` in the release, or hu.dwim.asdf reported that no tests were run |
| `timeout` | the bound (`LIBRARY_STATUS_TEST_TIMEOUT`, default 900 s) was hit |
| `load-fail` | the system or one of its test systems did not load |

**Only `pass` turns a row into `ok`.** `no-result` is the verdict this stage
exists to keep separate: a `test-op` that runs nothing, or a framework with no
recogniser yet, finishes cleanly and prints nothing that can be counted, and
that is "nothing was looked at", not "nothing went wrong". `no-tests` splits
off the first of those when the driver can see it for itself (it asks ASDF what
`test-op` would do before running it); it does not make a row `ok` either. A library whose
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

## Stage 2c -- rows waiting on a fork (`run-forks.sh`)

```
LIBRARY_STATUS_FORKS=path/to/forks sh bench/library-status/run-forks.sh
LIBRARY_STATUS_FORKS=path/to/forks LIBRARY_STATUS_CANDIDATES=1 \
  sh bench/library-status/run-forks.sh          # list the candidates only
```

The status column is what a user gets from the dists: Quicklisp plus the
dotcl overlay. Some libraries already have a dotcl fork with the fix, waiting
to be added to the overlay, and a row that fails only for want of it looks the
same as one that fails for a dotcl bug. This stage tells the two apart without
touching the status column:

1. Every subdirectory of `LIBRARY_STATUS_FORKS` is a git checkout of a fork. A
   fork whose HEAD is the commit the dotcl dist already ships (its release
   names end in the commit) is left out; the rest are pending. Their systems
   are the `defsystem` names in their `.asd` files.
2. The candidates are the rows that are not ok with the dists alone (they do
   not load, or their suite fails, errs, times out, or its test system does
   not load) and whose dependency tree reaches a pending fork. The tree is
   the depends-on lists the dists record in `systems.txt`, from the row's
   system and its test systems; a Quicklisp project the dotcl dist carries is
   read from the dotcl dist. `LIBRARY_STATUS_CANDIDATES=1` stops here.
3. Only the candidates are measured again, by the same two scripts as stages
   2 and 2b, with the pending forks put ahead of the dists through
   `CL_SOURCE_REGISTRY` (ASDF searches its source registry before Quicklisp).
4. `forks.json` gets one entry per candidate: the pending forks its tree
   reaches, what the second run saw, and a judgement from comparing the two
   runs, by load first ("does not load" < "loads"), then by test verdict
   (`timeout`, `load-fail` < `error` < `no-result`, `no-tests` < `fail` <
   `pass`), then, when both fail, by the number of failures:

| judgement | meaning |
| --- | --- |
| `pending-fork` | loads only with the forks, or passes only with them: the row waits on a fork reaching the dotcl dist |
| `improved` | better with the forks, but still not ok |
| `same` | no better: the forks are not what the row waits on |
| `worse` | worse with the forks |

The forks named are the pending ones the tree reaches, not a proof of which
one made the difference; when a row reaches several, removing them one at a
time is the way to find out.

The dists are read from the Quicklisp home the bundled client uses
(`LIBRARY_STATUS_QL_HOME` overrides it), and should be the same home stages 2
and 2b ran against, or the comparison is between two different dists.
`render.lisp` shows the entries in the `with forks` column.

## Stage 2d -- the same rows on SBCL

```
LIBRARY_STATUS_LISP=sbcl LIBRARY_STATUS_QL_HOME=path/to/quicklisp \
  sh bench/library-status/run-quickload.sh
LIBRARY_STATUS_LISP=sbcl LIBRARY_STATUS_QL_HOME=path/to/quicklisp \
  sh bench/library-status/run-tests.sh
```

The time a row takes on dotcl means little without the time the same work
takes elsewhere, so stages 2 and 2b also run on SBCL. The drivers are the
same; only loading Quicklisp (the `setup.lisp` of `LIBRARY_STATUS_QL_HOME`,
a home with the Quicklisp client and the same dists as the dotcl run) and
quitting differ. `LIBRARY_STATUS_SBCL` names the executable. The outputs
default to `results-sbcl.json` and `tests-sbcl.json`, and the test list to the
rows that load on SBCL. Give each implementation an empty compile cache
(`XDG_CACHE_HOME`) and the same row order, so that a row's time includes the
same dependency compilation on both.

`render.lisp` shows the `"seconds"` of both as the `seconds, dotcl / SBCL`
column, and the sums over the rows measured on both at the top of the table.
The verdicts on SBCL are kept in the files but not shown.

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
`LIBRARY_STATUS_ANNOTATIONS`, `LIBRARY_STATUS_TESTS_JSON`,
`LIBRARY_STATUS_FORKS_JSON`, `LIBRARY_STATUS_SBCL_JSON`,
`LIBRARY_STATUS_SBCL_TESTS_JSON`, `LIBRARY_STATUS_OUT`, `DOTCL_VERSION`.
`library-status/render:main` takes the same as keyword arguments.

The JSON reader is part of the script. Rendering the table has to work in a
checkout where nothing has been installed yet, which a dependency on a JSON
library would break.

## Not automated yet

- Test frameworks without a recogniser (lisp-unit, and
  suites that print their own format) leave their rows at
  `no-result`, which renders as `load-only`.
- Stage 1 is still its own run: choosing targets needs a dist on disk and a new
  dist is rare, so `make library-status` deliberately does stages 2 and 3 only.
