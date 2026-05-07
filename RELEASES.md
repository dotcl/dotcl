# Releases

User-facing release notes for dotcl. Each section corresponds to a tagged
release on the public mirror (dotcl/dotcl).

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
- `defgeneric` / `defmethod` CLHS error conditions tightened (#217, #218).
- `read-char` / `peek-char`: `recursive-p` no longer overrides `eof-error-p`.
- `reader`: `recursive-p` argument implemented (#211).
- `defstruct`: `:print-function` / `:print-object` options implemented (#230).
- `defgeneric`: `RegisterFunctionOnSymbol` skips package lock for GFs (#234).
- `defmethod`: optional arity may be less than the GF's (#235).
- `reader`: `SET-MACRO-CHARACTER` / `FlattenTopLevel` unwrap `MvReturn` leak.
- `asdf`: `:package-local-nicknames` added to target features; `defgeneric`
  redefinition demoted to a warning for ASDF compatibility.
- Symbol reference in non-FASL mode changed to inline lookup (#106).

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
