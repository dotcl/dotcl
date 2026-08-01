# Line coverage for .lisp

`compile-file` writes a PE assembly plus a portable PDB, and that PDB's document
table names the `.lisp` source with a sequence point per form. An off-the-shelf
.NET coverage collector reads IL and PDBs and knows nothing about the language,
so it reports line coverage against the `.lisp` itself. There is no
coverage-specific code in dotcl.

```
dotnet tool install -g dotnet-coverage
make test-coverage
```

## Measuring your own code

Coverage is collected per process: the collector has to launch the program,
because it installs a profiler before the runtime starts. It cannot be switched
on inside a running REPL.

Three things are required.

1. **Compile the code you want measured.** Only compiled code has a PDB. Code
   evaluated straight from source, or typed at the REPL, is emitted dynamically
   and is invisible to the collector.
2. **`DOTCL_EMIT_PDB=1`** — without it `compile-file` writes no PDB, and the
   collector has nothing tying IL back to your source.
3. **`DOTCL_FASL_LOADFROM=1`** — `load` normally reads a fasl into memory, and an
   assembly loaded from bytes has no file location, which is how the collector
   picks what to instrument. This switch loads the fasl by path instead.

Given a `driver.lisp` that compiles your code, loads it, and exercises it:

```sh
DOTCL_EMIT_PDB=1 DOTCL_FASL_LOADFROM=1 \
  dotnet-coverage collect --output coverage.cobertura.xml --output-format cobertura -- \
  dotcl driver.lisp
```

The report is ordinary cobertura, so anything that reads cobertura works —
including the VS Code coverage extensions, which will shade the `.lisp` directly.

`DOTCL_FASL_LOADFROM` is not the default because loading by path holds the file
open and caches the assembly against that path: recompiling a fasl and loading it
again in the same session would fail to write on Windows, and elsewhere would
hand back the assembly already loaded. A coverage run is one process that
compiles, loads, runs and exits, so it never reaches that.

## What the numbers mean

Coverage is per line, and a line counts as covered if it was reached at all —
the collector does not report execution counts, so a loop body reads the same as
a statement that ran once.

Resolution is bounded by where the compiler emits sequence points, which is
coarser than one per line: the arms of an `if` do not currently carry their own
points, so a line inside a branch that never ran can still be reported as
covered. Treat the output as reliable at the level of "was this function
exercised", and as approximate within a function.
