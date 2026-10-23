# asdf-extra-deps

Adds dependencies an ASDF system uses but does not declare, from a file kept
outside the system. The `.asd` is not edited and ASDF itself is not changed.

```lisp
(require "asdf-extra-deps")                 ; on other Lisps: load asdf-extra-deps.lisp after ASDF
(asdf-extra-deps:load-supplements "supplements.sexp")
(asdf:load-system "serapeum")
```

A supplement file holds one form per system:

```lisp
(:system "split-sequence"
 :asd-sha256 ("...")                          ; optional, the SHA-256 of the .asd
 :depends-on (("vector" "package")
              ("api" "package" "vector" "list")))
```

Each `:depends-on` entry is a component and the components it should depend
on. A component is named by its path from the system: a string for a child of
the system, a list of strings below that (`("level1" "vectors")`). A
dependency can also be another system, `(:system "alexandria")`. The
component's `prepare-op` then depends on the `load-op` of each, which is what
`:depends-on` in the `.asd` would have given.

`:asd-sha256` (and `:version`, a list of versions) restrict a form to the
releases it was written for. When the system's `.asd` changes, for example in
a release that declares the dependency itself, the form stops applying.

## Why

A file that uses a macro, an inline function, a special variable or a
package from another file of its system, without depending on it, builds
only because ASDF's order happens to load that file first. Any other order
the declared dependencies allow can compile it differently or fail to
compile it: `asdf-parallel`, a change to an unrelated dependency, another
implementation's `:if-feature` components.

## Finding them: audit.lisp

`audit.lisp` builds a system twice, in fresh processes, from an empty cache:
once in ASDF's order, once in the reverse order its dependencies allow (or
with `asdf-parallel`). The first build records what each file defines; each
file is then read again for the symbols, packages and features it names. A
file that names something defined, before it, by a file it does not depend on
is reported, and the fasls of the two builds are compared to show which
differences it explains.

```lisp
(load "contrib/asdf-extra-deps/audit.lisp")
(asdf-extra-deps-audit:audit "serapeum"
  :lisp '("sbcl" "--non-interactive" "--load")   ; or '("dotcl")
  :work "/tmp/audit/" :baseline t :output "serapeum.sexp")
```

```
;; serapeum: 130 files; plan 16796 ms, reverse 14759 ms; 9 fasls differ (and 3 between two plan builds), 7 files failed in the reverse order
UNDECLARED serapeum/level1/vectors -> serapeum/level1/sequences [fasl differs]
    function SERAPEUM:SINGLE
    inline SERAPEUM:SINGLE
    type SERAPEUM:SINGLE
UNDECLARED split-sequence/api -> split-sequence/package [failed]
    package SPLIT-SEQUENCE
WEAK       alexandria/alexandria-1/macros -> alexandria/alexandria-1/conditions
    function ALEXANDRIA:SIMPLE-PROGRAM-ERROR
    class ALEXANDRIA:SIMPLE-PROGRAM-ERROR
UNEXPLAINED serapeum/level0/clos: fasl differs
  uses what a later file defines: serapeum/level1/lists
    function SERAPEUM:IN
    compiler-macro SERAPEUM:IN
;; differ between two plan builds: lisp-namespace/src/namespace trivia.level2/impl type-i/package
```

`UNDECLARED` is a use that changes how the file compiles (a macro, compiler
macro, symbol macro, inline function, structure accessor, setf expander,
special variable, constant, type, package or feature); these go into the
supplement forms `audit` returns and writes to `:output`. `WEAK` uses only
functions, classes or methods, which a file needs when it runs, not when it
compiles; they go in only for a file that failed to compile in the other
order and has no other candidate. `UNEXPLAINED` lists the remaining fasls
that differ, with what the file uses from files after it in ASDF's order
(which the other order may have loaded first); `:baseline t` builds once more
in ASDF's order and sets apart the files whose fasls differ between two
builds in the same order.

The children are started with `:lisp` followed by the script to load. Their
`:setup` (Lisp text) loads Quicklisp by default; the system and its
dependencies must be installed. In the `:plan` and `:reverse` orders each
compile starts with `*gensym-counter*` at 0 (against `:parallel`, dotcl's
comparison ignores the digits in gensym names instead). On SBCL the fasls are compared byte for byte. dotcl's fasls differ in
every build, so they are compared with `bench/parallel-compile/fasl-digest.cs`
(`asdf-extra-deps-audit:*fasl-digest*`, needs the .NET SDK); `:compare nil`
skips the comparison.

What a file defines is read from the image after each file loads: portable
queries (`macro-function`, `compiler-macro-function`, `find-class`, symbol
macros, `*features*`, packages) and, on SBCL and dotcl, the implementation's
own record of inline functions, special variables, types, setf expanders and
structure accessors. On other implementations those kinds are not seen.
Files that change the readtable in ways other than `named-readtables` may
not be read to the end; the report says which.
