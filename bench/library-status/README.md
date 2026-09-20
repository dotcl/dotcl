# library-status

The pipeline behind `docs/library-status.md`: which Common Lisp libraries run on
dotcl, ordered by how much of Quicklisp depends on them. The table is generated,
never hand-edited, so that keeping it current is a command rather than a
judgement call.

Three stages. Stage 1 and 3 are pure data handling and run on any Lisp; stage 2
is the measurement and runs on dotcl.

```
  dist metadata            targets.txt              results.json + annotations.json
 (systems/releases)  -->   targets.tsv     -->     (one entry per   -->  docs/library-status.md
      rank.lisp                                     system tried)         render.lisp
                                                     stage 2
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
   "status": "ok",
   "issue": null,
   "checked": "2026-09-14",
   "note": "test suite run with fiveam"}
]
```

| field | meaning |
| --- | --- |
| `system` | the system named in `targets.txt` |
| `release` | the dist release it came from (`targets.tsv` has it) |
| `status` | `ok` (loads and its test suite passes), `load-only` (loads; tests not run), `patched` (loads from the patched release in the dotcl dist overlay), `fail` |
| `issue` | the issue tracking the failure, or `null` |
| `checked` | the date of the measurement, `YYYY-MM-DD` |
| `note` | one short line: what failed, which test framework ran, why it is patched |

The table is published, so **`issue` must name an issue in the public
`dotcl/dotcl` repository**. Anything tracked where a reader of the table cannot
open it leaves the field `null` and explains itself in `note` instead.

`note` is a **machine field**: every run rewrites it from what that run saw. Do
not put a worked-out reason there -- the next measurement erases it. Reasons go
in `annotations.json` (below), which no run opens.

A system with no entry is rendered as `not checked`, so a partial run is
publishable: it says what has been measured rather than implying the rest is
fine. `results.sample.json` is a three-row example of the shape, not data.

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

- The test-suite column: stage 2 reports whether a system loads, so no row can
  say `ok` yet. Telling `ok` from `load-only` needs a per-library way to run its
  tests -- there is no one command that does it across libraries.
- Stage 1 is still its own run: choosing targets needs a dist on disk and a new
  dist is rare, so `make library-status` deliberately does stages 2 and 3 only.
