# Releases

User-facing release notes for dotcl. Each section corresponds to a tagged
release on the public mirror (dotcl/dotcl).

## v0.1.12 — 2026-06-18

### Precompile your Lisp from a plain PackageReference

A project that references `DotCL.Runtime` can now precompile an ASDF system to a
fasl at build time with a single property — no repo checkout and no global tool
install. Point `DotclProjectAsd` at your `.asd`:

```xml
<PropertyGroup>
  <DotclProjectAsd>$(MSBuildProjectDirectory)/MyApp.asd</DotclProjectAsd>
</PropertyGroup>
<ItemGroup>
  <PackageReference Include="DotCL.Runtime" Version="0.1.12" />
</ItemGroup>
```

The build walks the system's `:depends-on` graph, compiles each component, and
writes a deployment manifest next to your output. At run time, load it in one
call:

```csharp
DotclHost.Initialize();
DotclHost.LoadFromManifest(
    Path.Combine(AppContext.BaseDirectory, "dotcl-fasl", "dotcl-deps.txt"));
```

Compilation runs in-process inside MSBuild (no subprocess), reusing the runtime
the package already ships, so the feature adds only a few KB to the package. It
requires building with the .NET 10 SDK. A Lisp error during the build surfaces as
an ordinary MSBuild error. The `.asd` file name must match the system name
(`MyApp.asd` ↔ `(defsystem "MyApp" ...)`).

GUI and heavy-dependency apps work too. The build does **not** need the .NET
types your Lisp references to be loaded — a `dotnet:define-class` that inherits,
say, `Avalonia.Application` compiles fine without Avalonia in the build process;
the base type is resolved when the fasl is loaded. For that to succeed, force the
relevant assemblies to load before `LoadFromManifest`:

```csharp
DotclHost.Initialize();
_ = typeof(Avalonia.Application).Assembly;   // referencing one type pulls it in
DotclHost.LoadFromManifest(manifest);
```

### Script arguments

`dotcl file.lisp arg1 arg2` runs `file.lisp` as a script and passes the trailing
arguments through to `uiop:command-line-arguments`. Arguments after the script
file are treated as the program's argv (the Unix convention), so passing a data
path — `dotcl viewer.lisp image.png` — no longer tries to load `image.png` as
Lisp source.

### `dotcl build` and a tidier CLI

ASDF system compilation is now a subcommand:

```
dotcl build MyApp.asd --output obj/MyApp.fasl
```

`dotcl --help` lists only user-facing options; build-tooling flags are internal.
New `--no-init` skips the user init file, and `--readline` / `--no-readline`
force the line-editing REPL on or off (it is on by default for an interactive
console, off when input is piped).

### Gray Streams predicates

`open-stream-p`, `force-output`, `finish-output`, and `clear-output` now accept
Gray Streams instances (any object for which `streamp` returns true), matching
the behavior of `input-stream-p` / `output-stream-p`. Output flush operations
trampoline to the corresponding `stream-force-output` / `stream-finish-output` /
`stream-clear-output` generic functions.

## v0.1.11 — 2026-06-17

### Type-declared .NET calls compile to a direct call

When the receiver and arguments of `dotnet:invoke` carry a static .NET type via
`(the (dotnet "Type.FullName") x)`, the call now compiles to a direct `callvirt`
to the resolved overload instead of a runtime member lookup — about 3.5x faster
than the (already cached) dynamic path. Untyped calls are unchanged, so this is
opt-in:

```lisp
(dotnet:invoke (the (dotnet "System.Text.StringBuilder") sb) "get_Length")
```

### uiop:run-program redirection and :directory

`dotcl:launch-process` now takes per-stream redirection specs (`:stream`, a file
pathname, `nil`, or inherit) and a working directory, so `uiop:run-program` /
`uiop:launch-program` correctly handle file and null output/input redirection and
the `:directory` argument.

### Custom metaclasses

`ensure-class` and `defclass`'s `(:metaclass ...)` option now honor a custom
metaclass: the resulting class is an instance of that metaclass (so `class-of`
and `typep` agree on the class metaobject), slots the metaclass adds beyond
`standard-class` are stored on the class metaobject (readable/writable with
`slot-value`), and the metaclass's inherited `initialize-instance` /
`shared-initialize` methods run when the class is created — so `:after`-computed
metaclass slots are initialized too.

### dotnet:new with optional-only constructors

`(dotnet:new "Type")` now works for a type whose only constructor has
all-optional parameters (e.g. `FluentTheme(Uri? baseUri = null)`), supplying the
defaults like C#'s `new T()`.

### Windows desktop assemblies

WinForms and WPF assemblies load by simple name —
`(dotnet:load-assembly "PresentationFramework")` — with transitive
shared-framework references resolved automatically. WinForms runs from the plain
runtime; full WPF should be hosted in a WindowsDesktop (`UseWPF`) app. See
`docs/windows.md` for the recipe, including the STA thread + message-pump setup.

### Embedding (DotCL.Runtime)

The `DotCL.Runtime` package now copies `dotcl.core` and the `contrib` tree to a
referencing project's build output, so `DotclHost.FindCore()` and
`(require :dotnet-class)` (i.e. `dotnet:define-class`) work from a plain
`PackageReference` with no manual asset wiring.

## v0.1.10 — 2026-06-16

### Faster .NET interop calls

`dotnet:invoke` and `dotnet:static` now cache the resolved method per call shape
(receiver type, member name, argument types), so a hot interop loop pays member
lookup and overload resolution only once — roughly 4.6x faster on repeated calls.
COM/IDispatch targets, `params` / by-ref methods, and calls with `nil` arguments
keep the previous dynamic dispatch, so behavior is unchanged.

### External processes via uiop:run-program

A new `dotcl:launch-process` primitive exposes a streaming child-process handle
(stdin/stdout/stderr, wait, exit code), and `uiop:run-program` /
`uiop:launch-program` are implemented on top of it.

### Compiler macros are applied during compilation

Compiler macros defined with `define-compiler-macro` (or via
`(setf (compiler-macro-function ...) ...)`) are now expanded while compiling call
forms, per CLHS 3.2.2.1 — previously they were registered but never consulted. A
macro that declines (returns the original form) or a locally shadowed operator
leaves the call untouched.

### Correct class identity for same-named .NET types

Two .NET types that share a simple name in different namespaces (e.g.
`System.Collections.ArrayList` vs. another `ArrayList`) now map to distinct Lisp
classes, so `class-of`, `typep`, and method dispatch no longer conflate them.

### Embedding (DotCL.Runtime)

The embeddable runtime library gains `DotclHost.ToClr` (convert a Lisp value to a
.NET object), `DotclHost.Register` (expose a host function to Lisp), and a
precompiled-only mode that forbids runtime code generation — for hosts where
dynamic code is unavailable (AOT / IL2CPP-style targets). `DotCL.Runtime` now
multi-targets `net8.0` in addition to `net10.0`.

## v0.1.9 — 2026-06-13

### Performance: native int64 fixnum arithmetic

Fixnum-declared numeric code now compiles to native `int64` operations instead
of boxing every intermediate value. This covers `(the fixnum ...)` expressions,
`(signed-byte N)` / `(unsigned-byte N)` / `bit` declared locals, and locals whose
value range is inferred from their initializer. A new SIL peephole pass removes
redundant load/store and dead stack traffic. `+` / `-` / `*` / `ash` still promote
to bignum on int64 overflow (a value-range proof gates the unboxed path), so the
optimization is transparent to correctness.

Self-recursive calls no longer re-resolve the function from its symbol on every
entry (the function is threaded through as a hidden argument). Combined with the
above, tight recursive/numeric benchmarks improve substantially (e.g. `tak` and
`trtak` are several times faster; `ackermann` ~25% faster). `double-float` and
`single-float` literals and unary negation now use native r8/r4 arithmetic.

### Thread-safe generic-function dispatch

Calling a generic function from multiple threads while another thread defines
methods on it no longer corrupts dispatch. Previously a concurrent `defmethod`
during dispatch could surface as a spurious `CALL-NEXT-METHOD: no next method`
error or a hang/crash (notably when driving a GUI from a separate thread). The
method list is now copy-on-write.

### CLOS / MOP

- Method dispatch honors `:argument-precedence-order` from `defgeneric`.
- Standard generic functions preserve their lambda list, and the MOP readers
  (`generic-function-lambda-list`, etc.) return the real names.
- `slot-definition` metaobjects are dispatchable; `slot-definition-class`,
  `standard-instance-access`, integer `slot-definition-location`, and
  `slot-boundp`/`slot-makunbound-using-class` are wired up — enough MOP for
  custom-metaclass dynamic slots.
- `defmethod` specializers work on `dotnet:define-class` instances and on raw
  CLR types (dispatch on a .NET object's class).

### .NET interop

- `dotnet:resolve-type` is now public — turn a type-name string into a
  `System.Type` (#17).
- `dotnet:invoke-generic` — call generic instance methods (#23).
- `dotnet:invoke` / `dotnet:static` may omit trailing parameters that have C#
  default values (#24).
- `dotnet:define-class` supports method/constructor overloading.
- `type-of` / `class-of` on a wrapped CLR object now report the CLR type instead
  of `T`, and `typep` against it works (#31).
- Lisp strings backed by a character vector now marshal to a .NET `String` when
  passed to `dotnet:` calls (previously they could reach .NET as the raw vector).
- `dotcl:thread-object` returns the underlying `System.Thread` of a dotcl thread
  (#26).
- Built-in `dotnet:` functions carry docstrings, surfaced through `documentation`
  (and `(setf documentation)` is now a callable function) (#25).

### Debugging

- `dotcl:backtrace` and `dotcl:print-backtrace` — Lisp-callable backtraces; the
  printed form now includes call arguments.
- `dotcl:jit-disassemble` (contrib) — view the JIT-generated native code of a
  compiled function.
- REPL: readline is interruptible, and UP/DOWN history direction is fixed.

### Fixes

- A leading `~` in a file name string passed to `load` / `open` / `probe-file`
  now expands to the user's home directory, not just `(pathname "~/...")` (#19).
- `make install` on Linux/macOS no longer fails to locate `crossgen2` (a host-RID
  detection bug added stray whitespace to the lookup path) (#21).
- `+` / `-` / `*` fast paths and `ash` with a constant shift promote to bignum
  correctly on int64 overflow instead of wrapping.
- `(setf accessor)` calls resolve by symbol identity, fixing cross-package
  mis-dispatch of same-named accessors.

## v0.1.8 — 2026-05-22

### ANSI test suite: 21928/21929 pass (99.995%)

One additional conformance fix since 0.1.7: `FORMAT ~E` subnormal double
rounding now passes consistently (was flaky). Known failure: `DEFGENERIC.ERROR.1`
(SBCL-compatibility warn instead of error, intentional).

### New: `dotnet:call-base` — invoke base class methods

`(dotnet:call-base self "MethodName" arg1 arg2 ...)` calls the base class
implementation of a virtual method non-virtually, equivalent to C# `base.Method(args)`.
Useful in `dotnet:define-class` method overrides for MonoGame, MAUI, etc.:

```lisp
(:methods
  ("Draw" ((gt "Microsoft.Xna.Framework.GameTime")) :returns Void :override t
    (dotnet:call-base self "Draw" gt)
    ;; custom drawing logic
    ))
```

### New: `dotnet:define-class` — base constructor arguments

`:ctor` bodies may now start with `(:base arg1 arg2 ...)` to pass arguments
to the base class constructor (C# `: base(...)`):

```lisp
(dotnet:define-class "MyRenderer" ("SomeBaseRenderer")
  (:ctor ((device "GraphicsDevice"))
    (:base device)
    (initialize-renderer self device)))
```

### New: `dotnet:define-class` — constructors with arguments supported

`:ctor` can have arguments as per the previous example.
The parameter list consists of `(<name> <type>)` pairs.

However, only one constructor is currently supported; any additional
constructors after the first will be silently ignored.

### New: `dotnet:ref` works on C# arrays

`(dotnet:ref arr index)` and `(setf (dotnet:ref arr index) val)` now work
on plain C# arrays (`T[]`), in addition to indexed collections like `List<T>`
and `Dictionary<K,V>`.

### New: `compile-macrolet` is NativeAOT-compatible

`compile-macrolet` no longer uses `Reflection.Emit` internally. Macro
expanders are now interpreted via a lightweight evaluator, making
`macrolet` available in NativeAOT and IL2CPP builds.

### New: MOP `slot-value-using-class`

`slot-value-using-class` generic function dispatch is now supported for
custom metaclasses, enabling MOP-level slot access interception.

### New: `dotcl:dotcl-homedir-pathname` — ASDF source-registry integration

`dotcl:dotcl-homedir-pathname` returns the parent directory of dotcl's
`contrib/` folder as a pathname, analogous to SBCL's `sbcl-homedir-pathname`.
This enables ASDF's `wrapping-source-registry` to discover dotcl's contrib
systems automatically, so libraries that depend on `dotcl-thread` (e.g.
bordeaux-threads) can be loaded without manual registry configuration.

### Bug fixes

- **Build**: `make install` now correctly finds `crossgen2` on Linux with
  .NET 8+ (Ubuntu 22.04/24.04 reported `RID: ubuntu.xx.xx-x64`; the Makefile
  now normalizes to the portable `linux-x64` NuGet package name).
- **Build**: `(setf documentation)` error when running `make compile-asdf-fasl`
  with an installed `dotcl` binary against a fresh source checkout is fixed.
- **CLOS**: `next-method-p` now returns `nil` correctly in all call paths,
  including `(funcall #'next-method-p)` inside primary methods when no
  next method exists.
- **Compiler**: `defgeneric` lambda-list congruence check now correctly
  identifies required parameters when `&optional` is present.
- **Compiler**: `(setf documentation)` on `cl:` symbols no longer mutates
  the wrong slot due to package-qualified setf key lookup.

## v0.1.7 — 2026-05-16

### ANSI test suite: 21927/21929 pass (99.99%)

Updated to the latest ansi-test submodule (21791 → 21929 tests). Four
conformance fixes were applied:

- **`make-load-form-saving-slots`**: now returns CLHS 3.2.4.2-compliant
  creation + init forms. Unknown keyword arguments now signal `program-error`
  (previously ignored via `&allow-other-keys`).
- **`PROBE-FILE` with logical pathnames**: fixed translation during test
  sandbox execution (test infrastructure fix).
- **`DEFGENERIC :argument-precedence-order`**: the check now verifies that
  *all* required parameters appear in the list, not just that listed
  parameters are required parameters.
- **`DEFMETHOD` optional parameter count**: now requires an exact match with
  the generic function (was only rejecting the method-has-more case).

Known failures: `FORMAT.E.26` (subnormal double rounding, flaky) and
`DEFGENERIC.ERROR.1` (SBCL-compatibility warn instead of error, intentional).

### New: `dotcl-repl` — terminal readline contrib

`contrib/dotcl-repl` provides a line-editing REPL with history and basic
readline-style key bindings on all platforms. Load with `(require "dotcl-repl")`.

### New: `cerror` starts the interactive debugger

`cerror` now invokes the debugger with an active `CONTINUE` restart rather
than immediately continuing. Matches the CLHS specification.

### New: `DOTCL:GC`

`(dotcl:gc)` forces a .NET garbage collection. Useful for memory-sensitive
applications and testing.

### Bug fixes

- **CLOS**: cross-package `compute-class-precedence-list` bug fixed (classes
  defined in a different package than their superclasses could fail to inherit
  correctly).
- **CLOS**: `default-initarg` thunk results are now correctly unwrapped from
  `MvReturn` wrappers.
- **Compiler**: `let` bindings now correctly shadow symbol-macro bindings in
  the same scope.
- **Compiler**: builtin function lookup is now restricted to the `CL` package,
  preventing accidental shadowing by user-defined functions with CL-like names.
- **Compiler**: free-variable analysis now correctly walks `cond` test
  expressions (closures capturing variables used only in `cond` conditions
  were broken).
- **Compiler**: `load-time-value` slot IDs are now namespaced per compiled
  module, preventing collisions when multiple FASLs are loaded.
- **Compiler**: mutation detection for `setf` with compound place expressions
  now correctly marks the variable as mutated (fixes missing boxing).
- **Runtime**: `FORMAT` now accepts character vectors (`char-vector`) as
  format control strings.
- **Runtime**: `(and integer ...)` compound type intersection is now
  normalized to a numeric range for `subtypep`.
- **Runtime**: `*READ-SUPPRESS*` is now also bound as a Lisp dynamic variable
  during `#-`/`#+` feature conditional exclusion.
- **Runtime**: `defstruct` now re-registers `*struct-info*` at FASL load
  time, fixing cross-module struct accessor calls after `load`.
- **Runtime**: `make-load-form` protocol is now applied when serializing
  `LispInstance` objects to FASL.

## v0.1.6 — 2026-05-07

### New: `DotCL.Runtime` embeddable library NuGet package

`DotCL.Runtime` is now published as a separate NuGet package
(`PackageId=DotCL.Runtime`, `OutputType=Library`). Projects that embed
dotcl can now reference it without the `NU1212` error:

```xml
<PackageReference Include="DotCL.Runtime" Version="0.1.6" />
```

The package bundles `dotcl.core` as a content file, so it is automatically
copied to the consuming project's output directory. The `DotclAsLibrary=true`
workaround used by the sample projects is eliminated.

### New: `save-application` improvements

- `:r2r t` enables ReadyToRun AOT compilation (`--self-contained` only).
- `:no-self-contained t` now correctly passes `--no-self-contained` to
  `dotnet publish` (was silently ignored before).
- Single-file compression is applied automatically for self-contained builds.

### New: `save-application :executable` — ASDF/UIOP standalone exe

`save-application` with `:executable t` produces a standalone executable
that invokes the Lisp image's top-level entry point. `--help` and
`--version` flags are forwarded to the Lisp side rather than intercepted
by dotcl.

### New: `dotcl:getcwd`

`(dotcl:getcwd)` returns the current working directory as a pathname,
matching the behaviour of `uiop:getcwd`.

### Changed: ANSI conformance 21791/21791 (100%)

- `defmethod` docstrings are retrievable via `(documentation name 'method)`.
- `defgeneric` / `defmethod` CLHS error conditions tightened.
- `read-char` / `peek-char`: `recursive-p` no longer overrides `eof-error-p`.
- `reader`: `recursive-p` argument implemented.
- `defstruct`: `:print-function` / `:print-object` options implemented.
- `defgeneric`: `RegisterFunctionOnSymbol` skips package lock for GFs.
- `defmethod`: optional arity may be less than the GF's.
- `reader`: `SET-MACRO-CHARACTER` / `FlattenTopLevel` unwrap `MvReturn` leak.
- `asdf`: `:package-local-nicknames` added to target features; `defgeneric`
  redefinition demoted to a warning for ASDF compatibility.
- Symbol reference in non-FASL mode changed to inline lookup.

## v0.1.5 — 2026-05-06

CLHS conformance pass: completed a chapter-by-chapter audit of CLHS
chapters 2–25 and fixed the spec violations found. ANSI test pass count
is 21789/21791 (99.99%; the 2 remaining failures are intentional
SBCL-compatible deviations).

### Spec compliance

Reader / Printer / Format:
- `with-standard-io-syntax` resets `*print-readably*` to T.
- `peek-char` no longer double-echoes on echo streams.
- `get-dispatch-macro-character` returns the registered function.
- `print-unreadable-object` evaluates `:type` / `:identity` at runtime.
- `princ` dynamically binds `*print-escape*` to nil.
- `#\Space` prints as `#\Space` (CLHS 22.1.3.2).
- `fresh-line` is correct after `write-char` / `write-string` / `terpri`.
- FORMAT `~G` (general floating-point) and `~$` (monetary) implemented.
- `#+` / `#-` match feature names with package equality, fixing both
  `#-common-lisp` suppression and the non-keyword feature case.

Pathnames / Files / Streams:
- `translate-logical-pathname` handles string patterns; supports `**`
  inferiors and lowercases components when translating to physical.
- `pathname-match-p` matches `**` against nil-directory pathnames.
- `translate-pathname` recognizes logical-pathname strings.
- `ensure-directories-exist` returns a pathname.
- `make-broadcast-stream` writes to every component.
- Operations on closed streams signal `stream-error`.

CLOS:
- `print` / `write` / `princ` dispatch through the `print-object` GF
  for CLOS instances. User-defined `print-object` methods now take
  effect from `print` (previously only direct calls worked).
- `shared-initialize` overrides existing slot values from initargs, so
  `reinitialize-instance` actually applies its initargs.
- `macro-function` returns NIL for `IF` (special operator).

Documentation:
- `defclass`, `defstruct`, `defun`, and `defmacro` doc strings are
  retrievable via `(documentation name <type>)`. (`defmethod` is a
  follow-up.)

Type / error conformance:
- `typep` recognizes `(MOD n)` type specifiers.
- `defstruct` rejects `setf` on `:read-only` slots.
- `write-byte` signals `type-error` on non-binary-output streams.
- `name-char` accepts string designators.
- `code-char` returns NIL for codes ≥ `char-code-limit`.
- `nth` signals `type-error` on negative indices.
- `apply` signals `type-error` when the last argument is not a proper
  list.
- `invoke-restart` signals `control-error` when the restart is not
  active.
- `intern` returns the correct `:internal` / `:external` / `:inherited`
  status keyword.
- `copy-symbol` copies `SetfFunction` when `copy-props` is true.

### Added

- `compare-and-swap` / `atomic-incf` / `atomic-decf` macros in the
  `DOTCL` package (lock-based).

## v0.1.4 — 2026-05-04

CLOS / MOP and ecosystem compatibility release.

### Changed

- ANSI test count reached 21791/21791 briefly during this cycle
  (LOGICAL-PATHNAME.ERROR.9, PROBE-FILE.4, DEFCLASS.ERROR.23,
  DEFGENERIC.30 fixes).
- Method lambda-list congruence relaxed to allow methods with fewer
  optionals than the GF (SBCL behavior; required for Gray-stream
  libraries like `babel` / `fast-io` / `quri` / `chunga`).
- `defgeneric` warns instead of erroring when an existing ordinary
  function is replaced (SBCL behavior; required for `cl-ppcre` etc.).
- `slot-value` works on `defstruct` instances.

### Added

- MOP: `make-instance 'standard-generic-function` / `'standard-method`,
  `reinitialize-instance` on class objects, `validate-superclass` as a
  GF, and custom slot options — unblocking `closer-mop` and libraries
  built on it.
- Reader: full Unicode character-name support (UCD-derived tables
  generated at build time), including non-BMP `#\Uxxxxxxxx`.
- `with-compilation-unit` honors `:override`.
- Threading primitives exposed as the `DOTCL` public API.
- `bordeaux-threads-2` lock timeout / `with-timeout`.
- `trivial-gray-streams` lambda-list and `stream-element-type` GF fixes.

## v0.1.3 — 2026-05-03

Ecosystem and demo support.

### Added

- `dotcl:run-process` public API.
- `dotcl:command-line-arguments` public API.
- `MonoGameLispDemo` csproj toggles TFM/RID per host OS.
- `MauiLispDemo` workload restore step documented.

### Fixed

- `LispErrorFormat` / `LispWarnFormat` now use the real `format`
  implementation (better diagnostic output).
- Windows path normalization applied as an `asdf` load advice.
- `crossgen2` path auto-detected from the host RID.
- CI build no longer fails when `git describe` is unavailable.

## v0.1.2 — 2026-05-03

Build, packaging, and toolchain release.

### Added

- `compile-file :target-features` for cross-compiling FASLs against a
  different feature set; per-OS asdf fasl loading.
- `--version` reports a `git describe`–derived semver string.
- `:package-local-nicknames` added to `*features*`.

### Fixed

- Public mirror build / install issues (dotcl/dotcl #1, #2).
- `ros-pack`: per-RID tarballs now have the `runtime` exec bit set, and
  cross-RID R2R FASL duplication is removed.
- `handler-case`: `*in-try-block*` propagation in `var-is-special`
  branches.
- Copy-propagation peephole removes single-reference `let` / `let*`
  locals.

## v0.1.1 — 2026-04-30

RID expansion release.

### Changed

- **R2R AOT FASLs now ship for win-x64, win-arm64, linux-x64, linux-arm64,
  osx-x64, osx-arm64.** v0.1.0 only had R2R for win-arm64; other
  platforms used the framework-dependent FASL (slower cold start).
- Added per-RID nupkgs: `dotcl.win-x64`, `dotcl.linux-x64`,
  `dotcl.linux-arm64`, `dotcl.osx-x64`, `dotcl.osx-arm64`.

### Notes

- 32-bit builds (win-x86) intentionally not shipped.
- crossgen2 cross-compile is used; FASLs for non-host RIDs are produced
  on the win-arm64 dev machine.

## v0.1.0 — 2026-04-29

Initial public release.

### Highlights

- Common Lisp implementation on .NET 10. Lisp source is compiled to CIL
  and runs on the .NET JIT — same Lisp image runs on Windows, macOS, and
  Linux across x86-64 and ARM64.
- Broadly conforms to the ANSI Common Lisp standard (verified against
  the [ansi-test suite](https://gitlab.common-lisp.net/ansi-test/ansi-test)).
- `dotnet:` package for .NET interop: instantiate types, invoke methods,
  subclass via `dotnet:define-class` (real .NET classes emitted from
  Lisp; frameworks like MAUI / ASP.NET Core / MonoGame see them as
  ordinary subclasses).
- ASDF support (forked + adapted, ships as IL-only `.fasl` for
  cross-platform load).
- Working sample integrations in `samples/`: ASP.NET Core, MAUI,
  MonoGame, MCP server.

### Known limitations

- Single-image model (load-time codegen). No separate compilation per
  file in the traditional Lisp sense.
- Multi-platform R2R AOT currently only available for `win-arm64`.

### License

MIT. See [`LICENSE`](LICENSE).
