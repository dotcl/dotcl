# Writing libraries whose fasls run on every platform

A dotcl fasl is a .NET assembly of IL, and IL does not depend on the OS or CPU.
What goes into the IL does. Two things are decided before any IL is written,
on the machine that compiles the file:

- **Reader conditionals.** `#+windows` and `#-arm64` are decided when the
  source is read. The fasl holds only the branch that was true on the
  machine that compiled it.
- **Code that runs at compile time.** `eval-when (:compile-toplevel)`, macro
  expanders, `#.`, `defconstant` and `deftype` run while the file is compiled.
  If they ask which OS or CPU this is, the answer becomes a constant in the
  fasl.

A library packaged with `dotcl pack --library` (see
[Packaging a library](dotcl-pack.md#packaging-a-library---library)) ships
fasls compiled on one machine. This page is about writing a library so that
those fasls are correct on every platform, and about a check that finds the
places where they might not be.

## What dotcl does with a package from another platform

`dotcl pack --library` writes a note into the package: the dotcl that compiled
the fasls and the OS and CPU features it had (`windows win32 linux macos
darwin bsd unix x86 x86-64 arm arm64 32-bit 64-bit little-endian
big-endian`, whichever were present).

When a consumer loads the package on another OS or CPU, or the package has no
such note (made by dotcl 0.1.32 or earlier), dotcl does not use the fasls. It
compiles the sources, which the package always contains, and prints one line:

```
; nuget: <package directory> was compiled on macos darwin bsd unix arm64 64-bit little-endian; this is windows win32 x86-64 64-bit little-endian, so its sources are compiled instead
```

dotcl cannot tell from a fasl whether its sources looked at the platform, so
it assumes they did. A wrong fasl is never loaded this way; the cost is the
compile time on the consumer's machine.

ReadyToRun images (which `dotcl pack --library` adds by default) are per platform by construction: a
`foo.fasl.r2r-<rid>` sibling is used only on that runtime identifier, and
anywhere else the fasl's IL is compiled by the JIT as usual (see
[Ahead-of-time compiled FASLs](readytorun.md)).

The platforms dotcl ships for, and the features each has:

| RID | OS features | CPU feature | on all of them |
| --- | --- | --- | --- |
| `win-x64`, `win-arm64` | `:windows :win32` | `:x86-64` / `:arm64` | `:dotcl :64-bit :little-endian` |
| `linux-x64`, `linux-arm64` | `:linux :unix` | `:x86-64` / `:arm64` | |
| `osx-x64`, `osx-arm64` | `:macos :darwin :unix :bsd` | `:x86-64` / `:arm64` | |

UIOP adds `:os-windows`, `:os-unix` and `:os-macosx` to match.

## Guidelines

### Decide the platform at run time

Ask at run time, where the answer is the consumer's machine:

```lisp
;; Decided when the file is compiled: the fasl keeps one branch.
(defun null-device ()
  #+windows "NUL"
  #-windows "/dev/null")

;; Decided when the function runs.
(defun null-device ()
  (if (uiop:os-windows-p) "NUL" "/dev/null"))
```

`uiop:os-windows-p`, `uiop:os-unix-p`, `uiop:os-macosx-p` and
`(uiop:featurep :arm64)` are ordinary functions. `uiop:os-cond` is a macro,
and on dotcl it expands into a `cond` that runs its tests at run time, so it
is fine too (on most other implementations it picks the branch at
macroexpansion time).

Implementation features are fine as reader conditionals: `#+dotcl`,
`#-sbcl` and `#+(or ccl dotcl)` read the same on every machine dotcl runs on.
So do `#+64-bit` and `#+little-endian` on the platforms above, though a .NET on
a 32-bit or big-endian CPU would read them differently.

When a branch cannot be written for every platform (it names a package that
only exists on one, say), keep the reader conditional and treat the library
as one to compile per platform.

### Do not make the machine a constant at compile time

Everything that runs at compile time runs on the build machine. These keep
its answer:

```lisp
(defconstant +separator+ (if (uiop:os-windows-p) #\\ #\/))   ; a constant

(defmacro with-native-path ((var path) &body body)            ; the expander decides
  (if (uiop:os-windows-p)
      `(let ((,var (substitute #\\ #\/ ,path))) ,@body)
      `(let ((,var ,path)) ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)        ; a feature pushed at
  (when (member :x86-64 *features*)                           ; compile time, then
    (pushnew :my-lib-fast-path *features*)))                  ; read by #+my-lib-fast-path
```

Write the test into the code the macro produces, and use `defvar` or
`defparameter` (evaluated at load time) instead of `defconstant`:

```lisp
(defparameter *separator* (if (uiop:os-windows-p) #\\ #\/))

(defmacro with-native-path ((var path) &body body)
  `(let ((,var (if (uiop:os-windows-p) (substitute #\\ #\/ ,path) ,path)))
     ,@body))
```

The same holds for `machine-type`, `software-type`, `uiop:architecture`,
`uiop:operating-system` and, with CFFI, `foreign-type-size` of a type whose
size differs between platforms.

### Keep the build machine's files out of the fasl

A package is unpacked in a different directory on the consumer's machine,
even on the same platform. `#.*compile-file-truename*`, or
`*load-truename*`, `user-homedir-pathname` or an environment variable read
inside a macro expander, puts a path of the build machine into the fasl.
Find files at load or run time instead:

```lisp
(defvar *data-directory*
  (asdf:system-relative-pathname "my-lib" "data/"))
```

### C: compile per platform

CFFI's groveller (`:cffi-grovel-file`) and wrapper files compile and run C on
the build machine; their output is that machine's. A library that uses them
is one to compile on each platform it is used on.

In `defcstruct`, `defcunion` and `defctype`, prefer types whose size is the
same everywhere (`:int32`, `:int64`, `:pointer`, `:size`) to `:long` and
`:unsigned-long`, which are 32 bits on Windows and 64 bits elsewhere. Slot
offsets are computed when the definition is compiled.

### Choosing files in the .asd is fine

`(:file "win32" :if-feature :windows)` and `#+windows` inside a system
definition are decided by the consumer, which reads the `.asd` itself. They do
not make the other files' fasls depend on the platform.

## Checking a library

The `asdf-nuget` module has an ASDF operation that reads a system's sources and
lists the places that can make its fasls depend on the platform. It loads,
compiles and runs nothing:

```lisp
(require "asdf-nuget")
(asdf:operate 'asdf-nuget:check-portability-op "my-lib")
```

The files are the ones ASDF lists as the system's components, together with
those of the systems of the same name it depends on (`my-lib/core`, or the
parts of a package-inferred system). Other libraries it depends on are not
checked; name them to check them. A component with `:if-feature` is checked when
some platform in the table above has the feature, and reported when not all of
them do.

`dotcl pack --library` runs the same check on the systems it packs and prints
what it finds as a warning, `asdf-nuget:fasl-portability-warning`, before it
writes the package. The package is written either way: a consumer on another
platform compiles the sources (see above), and the warning is there for the
cases that are wrong anyway, a build machine path above all.

A reader conditional is checked by evaluating it for each platform in the
table above; it is reported when the results differ. Output looks like this:

```
.../src/util.lisp:36: read-platform: #-windows is true on linux-x64 linux-arm64 osx-x64 osx-arm64 only
.../src/util.lisp:91: compile-probe: SOFTWARE-TYPE in EVAL-WHEN (:COMPILE-TOPLEVEL)
.../acceptor.lisp:33: compile-environment: *COMPILE-FILE-TRUENAME* in #.

Counts:
      4  read-platform  -- reader conditional that reads differently by OS or CPU
      ...
```

| Kind | What it found | Blocks a portable fasl |
| --- | --- | --- |
| `read-platform` | a reader conditional that reads differently on two platforms | yes |
| `compile-probe` | an OS or CPU question in code that runs at compile time | yes |
| `c-probe` | a groveller or wrapper component in the `.asd` | yes |
| `c-layout` | `:long` / `:unsigned-long` in a C struct, union or type definition | yes |
| `compile-environment` | the build machine's paths or environment read at compile time | not by platform, but by location |
| `asd-platform-component` | a component with an OS or CPU `:if-feature` | no |
| `platform-constant` | a platform feature that reads the same everywhere (`#+64-bit`) | no |
| `runtime-check` | `uiop:os-windows-p` and the like at run time | no |

The report prints the blocking kinds and `compile-environment` one per line,
then the counts and what the check cannot see. The pack warns about the same
lines. From Lisp, `asdf-nuget:check-portability` returns the findings (and the
files read) without printing, and `asdf-nuget:report-portability` prints them
(all kinds with `:verbose t`) and returns true when nothing blocks. A finding
is a structure with `finding-file`, `finding-line`, `finding-kind` and
`finding-detail`.

### What the check cannot see

It reads text, so it reports candidates, not proofs, in both directions:

- A function called by a macro expander, a reader macro, or a feature a
  library pushes at compile time and tests later is not followed. The push is
  reported when it names a platform feature; the later `#+` is not.
- A reported conditional may sit in code that is never called on dotcl.
- A probe inside `eval-when (:compile-toplevel :load-toplevel)` is reported
  even when the load-time value is what is used.
- Custom read syntax can throw the reader off for the rest of a form.
- The `.asd` was read on the machine running the check, so a component it
  chooses with `#+windows` inside `:components` is seen as it reads there.

`asdf-nuget:*portability-limitations*` holds this list, and the report prints
it.
