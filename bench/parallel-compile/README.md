# Parallel compilation probes

Two scripts for asking how fast dotcl compiles libraries today, and what would
break or speed up if it compiled on several threads.

- `ctime.lisp`: a cold ASDF build of one system, timed per system and per file,
  on dotcl or SBCL. It also builds the file-level dependency graph that ASDF's
  plan implies and schedules it on 2, 4, 8 and unboundedly many workers with the
  measured file times, which bounds what building files in parallel could gain.
  See the header for the output lines.
- `threads.lisp`: runs `compile`, `compile-file` and `eval` from several threads
  at once (dotcl only) and checks the results: fasls that load and define what
  their source defines, `load-time-value`, gensym, packages, CLOS, and a real
  system's files compiled from N threads. Run each test with `THR_N=1` first as
  the control.
- `pbuild.lisp`: a cold build of one system with `contrib/asdf-parallel` and
  N compiles at once (`PB_JOBS`), on dotcl or SBCL. `PB_JOBS=1` is ASDF's own
  serial build.
- `fasl-digest.cs`: compares the fasls of two builds (`--compare dir-a
  dir-b`, the two `XDG_CACHE_HOME`s). Two compiles of one file never give the
  same bytes, so it compares what the fasls contain instead; `--gensyms` also
  ignores the numbers in the names of uninterned symbols, which depend on what
  else the process compiled meanwhile. See its header.

To compare the two implementations on the same sources, point both at one
`CL_SOURCE_REGISTRY` that lists each project once (for example, dotcl's
Quicklisp home with its overlay dist first, then the stock dist), rather than
letting each use its own Quicklisp, and give every run an empty
`XDG_CACHE_HOME`.
