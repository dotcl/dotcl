# Releases

User-facing release notes for dotcl. Each section corresponds to a tagged
release on the public mirror (dotcl/dotcl).

## v0.1.24 -- 2026-08-11

Where dotcl cannot generate code at run time -- in the browser, under NativeAOT,
under Unity's IL2CPP -- `eval` goes through a tree-walk interpreter instead of the
compiler. This release closes a long list of differences between the two, and
the regression suite now runs through the interpreter on every push, so they
cannot drift apart unnoticed again.

### Upgrading

- A .NET `Single` now arrives as a `single-float` rather than a `double-float`.
  The value is unchanged -- binary32 widened to binary64 exactly -- but the Lisp
  type is now the right one, so **later arithmetic runs in single precision where
  it used to run in double**. Results can differ. Where you want the wider type,
  say so: `(* x 2.0d0)`.
- The `was compiled by a different dotcl core` warning that 0.1.23 printed on
  every start was wrong. Nothing was stale; it is gone.

### Running without run-time code generation

These builds evaluate through the interpreter. What was broken and now works:

- `load` on a `.lisp` file — previously it could not read source at all.
- Backtraces show your frames instead of the interpreter's internals.
- `handler-case` and `handler-bind` catch exceptions thrown by .NET, not only
  conditions signalled from Lisp.
- An uncaught `throw` signals a condition instead of terminating the process.
- `eval` from more than one thread no longer deadlocks.
- Lambda lists are checked: wrong argument counts signal a `program-error`
  instead of being accepted or crashing.

Beyond that, a long tail of semantic differences from the compiler is corrected —
`block` scope was dynamic, `macrolet` leaked into the global macro table, the
function and variable namespaces were shared, `symbol-macrolet` did not reach
`&environment`, `restart-case` did not associate its restarts with the condition,
`handler-case`'s `:no-error` clause ran inside its own handler.

New `:dotcl-emit` feature so code can ask which kind of build it is on.

### Bundled applications answer for themselves

Libraries commonly read their own version at load time with
`(asdf:component-version (asdf:find-system :self))`. A bundle has no `.asd`
files, so that form used to signal and the whole image refused to load —
dexador and cl-str both do this. `save-application` now records the systems it
bundled, with the versions read at build time, so those forms answer correctly
in the deployed image.

### Numbers across the .NET boundary

The rules — which .NET numeric types arrive as which Lisp types, when an integer
argument is rejected, and how `System.Decimal` works — are now written down in
`docs/numbers.md`, with every example run against dotcl.

## v0.1.23 -- 2026-08-06

SBCL's `make-host-2` cross-build stage now runs on dotcl without the workaround
harness it used to need.
Threads can be interrupted while they are computing, not only while they wait.
`save-application` follows a real dependency graph. And .NET's wider numeric
types -- `BigInteger`, `Int128`, `UInt128`, `Half` -- arrive as ordinary Lisp
numbers.

### SBCL's cross-build, unmodified

- dotcl runs SBCL's stock `make-host-2.sh` (which chains `make-genesis-2.sh`)
  with no patches to SBCL's sources and no Lisp-level workarounds on our side.
  What remains on ours is two environment lines in the cross-compilation host
  wrapper; the harness that used to stand in for missing pieces is gone. SBCL's
  own two-pass consistency check passes: "header files match between first and
  second GENESIS".
- The earlier `make-host-1` stage still runs with scaffolding. That is the next
  piece of work, not something this release finishes.
- Getting this far drove most of the correctness fixes below. Compiling a
  large, demanding body of portable Common Lisp remains the most productive way
  we have found to surface real bugs.

### Interrupting threads that are working

- `interrupt-thread` and `destroy-thread` now reach a thread that is busy
  computing, not just one blocked in a lock, sleep, or join: compiled loops
  carry an interrupt safepoint at the back edge.
- On Unix, Ctrl+C at the REPL interrupts the running form instead of killing
  the process.
- Running out of stack or heap signals a `storage-condition` you can handle,
  rather than taking the process down. Deep `apply` chains no longer die
  during the signalling itself.

### save-application

- The image is built by compiling as it loads, so definitions that a later
  file's macro needs are present when it expands.
- Component collection walks the dependency graph itself. ASDF's
  `required-components` is not transitively closed, which could silently drop
  whole systems from a saved image.
- New `:prelude` accepts source that must run before the closure is computed --
  the escape hatch for systems whose load-time forms need a build environment.

### .NET numeric types

- `System.Numerics.BigInteger`, `Int128`, `UInt128`, and `System.Half` now
  marshal in both directions as ordinary Lisp numbers. A `BigInteger` or
  `Int128` result is a CL integer; a `Half` is a `single-float`, which holds
  every `Half` value exactly. No new Lisp types were introduced.
- Passing a CL integer to a fixed-width .NET parameter is now checked: a value
  that does not fit the target's width signals a `type-error` instead of
  silently wrapping. **This can turn code that appeared to work into an error**
  -- an unchecked `(byte)300` used to store 44. Bit patterns still work:
  a signed target accepts the unsigned N-bit form, so `#xFF800000` reaches an
  `int` parameter and reinterprets as expected.
- These types can also appear in signatures dotcl emits, via
  `dotnet:define-class`.
- New `dotcl-float` contrib: IEEE float bit/value primitives, which is what
  Shinmera's float-features needs from an implementation.

### Interop and I/O

- Constructor exceptions from `dotnet:new` are unwrapped with the inner
  exception preserved, and `dotnet:exception-object` reaches the exception
  instance itself.
- `dotnet:call-out` treats `ref` parameters as in-out.
- `launch-process` accepts `:environment`; `open` accepts `:external-format`.
- Stream I/O failures signal `stream-error`, `file-position` on a closed stream
  signals rather than returning nonsense, and a `nil` pathname designator is a
  `type-error` instead of a confusing downstream failure.
- Gray streams: `force-output`/`clear-input` work on binary streams,
  `unread-char` dispatches to `stream-unread-char`, and `format ~T` asks the
  stream for its column.

### Tooling

- `dotcl:who-calls` reports callers of a function.
- New `dotcl-cltl2:macroexpand-all`, a real code walker that handles local
  `macrolet`. Tools that expand a whole form -- and the portability shims that
  look for an implementation's spelling of it -- now have one to find.
- Compiled `.fasl` files carry the generation of the compiler that produced
  them and warn on load when it does not match the running core -- the usual
  cause of "I fixed it but nothing changed".
- The `clrmd` contrib can aggregate a heap: type histogram, symbols per
  package, list structure.
- `samples/HotReloadDemo` shows a running C# host swapping in a changed
  `.lisp` file.

### Browser (WASM) groundwork

Not a supported target yet, but the pieces are in place: the build produces a
`browser-wasm` runtime, the core can be loaded from a byte array rather than a
file, and it survives IL trimming. Measurements of what actually works in a
browser -- filesystem, speed, JS interop -- are in the docs.

### Thanks

- Bohong Huang -- for the `dotcl-float` contrib and for finding that a
  compile-time `(require ...)` left the module registered but its definitions
  stripped, so the load-time `require` did nothing. The contrib also turned up
  an over-strict range check in our integer marshalling.

## v0.1.22 -- 2026-08-01

Quicklisp works out of the box, .NET interop reads the way you would write it
by hand, the debugger shows locals in every frame, and the function-call path
got roughly twice as fast. Also: CPU profiles and line coverage of Lisp code
now come from the standard .NET tools, unmodified.

### Quicklisp out of the box

- `(require "quicklisp")` loads a bundled client; `(ql:setup)` fetches over
  HTTPS and installs the dotcl overlay dist automatically, so
  `(ql:quickload :alexandria)` works on a fresh install with no manual
  bootstrap.

### Calling .NET (and .NET calling back)

- New call-chain syntax: `(dotcl:-> obj (Method a) Property ...)` reads in
  call order, and `doto` threads an object through several member calls.
  Lisp scalars work directly as receivers. The full surface is documented in
  `docs/dotnet-package.md`.
- CLOS now dispatches over .NET types: `defmethod` accepts .NET type
  specializers, interfaces participate in dispatch with a consistent
  precedence order, and generic variance is honored. Generic .NET types can
  be composed from Lisp.
- Callbacks passed to .NET propagate Lisp non-local exits correctly, and
  callback errors can be re-raised on the calling side.
- Class libraries: `dotnet new dotcl-classlib` scaffolds a Lisp library that
  a C# project can reference; a host API accepts .NET collections where Lisp
  sequences are expected.

### Debugging: locals in every frame

- The built-in debugger walks frames and shows their locals — including
  variables in boxed cells, natively-stored (unboxed) locals, and dynamic
  (special) variables. The same view is wired into SLIME's debugger, with
  eval-in-frame.

### Faster calls, smaller loads

- Function calls resolve their callee through a per-call-site cache
  (compiled and loaded code alike), and `(declaim (inline f))` now actually
  inline-expands small functions at call sites. The fixed cost of a simple
  call dropped from ~171ns to ~57ns; call-heavy benchmarks run up to 2x
  faster.
- Declared `decimal` locals and integer locals with statically proven ranges
  use raw native slots, extending the unboxed-arithmetic paths.
- The precompiled core loads in file-sized segments: peak memory while
  loading dropped by about 40%, and warm startup improved measurably.

### Profiling and coverage with stock .NET tools

- dotcl compiles Lisp functions to real .NET methods under their Lisp names,
  so `dotnet-trace` CPU profiles show your functions directly — no dotcl-side
  setup. See `docs/profiling.md`.
- Line coverage of `.lisp` sources works with the standard .NET coverage
  tools via the emitted PDBs. See `docs/coverage.md`.

### Correctness

- Sequence and string functions validate `:start`/`:end` bounding indexes.
- `stable-sort` is now actually stable; `format ~E` no longer double-rounds.
- Compiled files no longer resolve a call to an undefined global against a
  same-named function in another package.
- Native-representation locals shadowed by a same-named inner binding no
  longer corrupt the outer slot.
- `bordeaux-threads:interrupt-thread` gained a first tier: threads blocked in
  waits (locks, sleeps, joins) can be interrupted; `destroy-thread` ends the
  thread quietly.
- `file-position` now works through Gray stream bridges — thanks to
  Bohong Huang for the fix.

## v0.1.21 -- 2026-07-25

Compile Common Lisp definitions into a .NET assembly that C# can reference at
compile time (experimental), and run more of UIOP/ASDF unmodified: environment
writes, working-directory changes, hostname, and merged process output. Also
cuts the memory needed to load very large compiled files.

### Compiling Lisp into a C#-referenceable library (experimental)

- A new emit path turns dotcl definitions into a .NET DLL that a C# project can
  reference at compile time. Enums, structs, constants, delegates, interfaces,
  and exception types are exported as their C# equivalents; `defun`s become
  public static methods; several types can be collected into a single assembly;
  and docstrings are carried through as XML doc comments. This is experimental --
  the surface and conventions may still change.

### UIOP / ASDF portability

- Environment variables can now be written: `(setf (uiop:getenv "X") "...")`,
  and removed by setting the value to `nil`.
- `uiop:chdir` / `uiop:with-current-directory`, `uiop:hostname`, and
  `uiop:delete-empty-directory` are implemented.
- `run-program` accepts `:error-output :output`, sending a child's stderr to the
  same destination as its stdout.

### Loading large compiled files

- Long list and vector literals, and deeply nested literals, are split across
  several methods, so very large compiled files load with far less memory --
  files that previously needed multiple gigabytes now load in a fraction of that.

### Packaging

- `dotcl pack` copies `.asd` metadata (author, license, description) into the
  generated NuGet nuspec.
- Distributed packages no longer ship `.pdb` files, and stop packing the same
  content more than once.

### Correctness

- `loop` runs `:initially` clauses before `for x = form` variable
  initialization, matching SBCL's order.
- Fixed a compiler internal-table corruption that could occur when compiling in
  parallel.

## v0.1.20 -- 2026-07-24

Makes Common Lisp a first-class Visual Studio project: scaffold with
`dotnet new dotcl-app`, build through an MSBuild SDK, and F5-debug the `.lisp`
source. Also hardens cross-package symbol resolution and guards `dotcl pack`
against a silent misconfiguration.

### Visual Studio debugging

- `dotnet new dotcl-app` scaffolds a Common Lisp console app as an ordinary
  MSBuild project that references the `DotCL.Runtime` package and compiles the
  Lisp into the build output. You can run it and, in Visual Studio, debug it with
  F5. A `DotCL.Sdk` MSBuild SDK is also published as a more concise way to write
  the same project (`<Project Sdk="DotCL.Sdk">`).
- A Debug build emits Portable PDBs beside the compiled Lisp, so you can set
  breakpoints in `.lisp`, step through it expression by expression, and inspect
  variables in the Locals window -- parameters, `let` / `let*` bindings, and
  variables a lambda closes over, each shown by name with its printed value.
  Several `.lisp` files in one project each debug against their own source.

### Cross-package symbol resolution

- An unqualified call to a function whose printed name is also interned in another
  package (for example COMMON-LISP's backquote markers) no longer resolves to the
  wrong symbol: a package's own `fbound` function wins over a same-named symbol
  that merely exists elsewhere. This unblocks loading libraries such as ironclad.
  Thanks to Bohong Huang for the reports and fixes.

### `dotcl pack`

- `dotcl pack --from <dir>` now fails loudly when the payload runtime is too old
  to load a loose user FASL, instead of silently building a tool that drops into a
  REPL rather than running your program.

### Build

- Release builds pick the crossgen2 and runtime-reference packages by highest
  installed version rather than lexicographic order, so a machine with several
  .NET bands installed always uses the newest.

## v0.1.19 -- 2026-07-20

Adds `dotcl pack` for shipping a Lisp system as a .NET tool, broadens Gray
stream interop, lays the groundwork for source-aware debugging, and fixes a
compiler variable-scoping bug across packages.

### `dotcl pack` -- ship a Lisp system as a .NET tool

- `dotcl pack` turns an ASDF system (`.asd`) into a `dotnet tool` NuGet package:
  the system is compiled to a FASL, bundled with the runtime, and stamped into a
  self-contained CLI tool that `dotnet tool install -g` can pull. An executable
  carrying a user FASL runs that program directly instead of dropping into
  dotcl's own REPL. See `docs/dotcl-pack.md` for usage.

### Gray stream interop

- `make-two-way-stream`, `make-echo-stream`, and `make-concatenated-stream` now
  accept Gray streams, and `clear-input`, `stream-element-type`, and
  `stream-external-format` dispatch to Gray stream methods. Portable code layered
  on trivial-gray-streams composes with the built-in stream constructors.

### Threads

- `make-thread` no longer inherits the parent thread's dynamic bindings, matching
  bordeaux-threads' contract (a fresh thread starts from the global values).

### Source-aware debugging (foundation)

- Loading or compiling a file now records where each top-level definition was
  written, and `dotcl:function-source-location` reports it. This backs a debugger
  backend's "jump to definition" / "jump to the erroring frame" so a SLIME-style
  debugger can open the source at the right place.

### Compiler correctness

- Fixed a multi-package lexical variable collision: when two packages interned a
  symbol with the same printed name and both were used as lexical variables, a
  reference to one could resolve to the other's binding, reading the wrong
  variable at runtime. Variable lookup is now package-aware. Thanks to Bohong
  Huang for the report and fix.
- `dotnet:define-class` now binds the method receiver `self` as a symbol in the
  caller's package, so a bare `self` in a method body resolves regardless of
  which package the class is defined in (previously it could fail with an unbound
  `self`, including when loaded from a prebuilt FASL). Thanks to Douglas P.
  Fields, Jr. for the detailed report.

### Performance

- Many sequence and list operations -- `find`, `find-if`, `remove`, `remove-if`,
  `remove-if-not`, `delete`, `member`, `assoc`, `macro-function` -- take a faster
  direct call path that avoids per-call argument-array allocation.

### CLOS and interop

- Generic-function dispatch and class metadata are hardened against concurrent
  evaluation, removing intermittent failures under parallel compilation.
- Method specializers on .NET types no longer depend on class load order -- a
  specializer registered before its argument type is first seen still dispatches.
- In-process project compilation resolves the consuming project's referenced
  assemblies at compile time.

## v0.1.18 -- 2026-07-13

Adds a first-class .NET `decimal` type, tightens a couple of interop and
concurrency contracts, and fixes `dotnet tool install` so it pulls the fast
native build.

### System.Decimal as a first-class number

- `System.Decimal` values are now a first-class CL number instead of an opaque
  object. A decimal is a distinct exactness category -- `numberp` and `realp` are
  true, `rationalp` and `floatp` are false -- so its base-10 scale (the trailing
  zeros in `1.50`) survives, which a CL ratio would normalize away.
- Read and print decimals with `#m`: `#m1.50` and `#m"1.50"` read a decimal
  preserving scale, and it prints back the same way.
- `dotcl:decimalp` tests for one and `(typep x 'dotcl:decimal)` works. `(rational
  d)` gives the exact ratio and `(float d)` a float; `=` / `<` / `>` compare by
  value (`(= #m1.0 1)` is true) while `eql` is representation-sensitive.
- .NET APIs that take or return `decimal` marshal naturally: a returned decimal
  keeps its scale, and a CL integer or exactly-representable ratio passed to a
  decimal parameter converts exactly (a non-representable ratio like 1/3 signals
  an error rather than rounding silently).
- Standard arithmetic stays conservative -- `(+ #m1.5 1)` yields a rational,
  never a decimal, so code that never mentions decimals never meets one. Inside a
  `(declare (type dotcl:decimal x y))` scope, `+ - * /` instead compile to native
  `System.Decimal` operations and preserve scale (`1.50m + 2.25m` stays `3.75m`).
  Mixing a declared decimal with a float in one operation is rejected -- coerce
  explicitly -- since .NET itself forbids implicit decimal/double conversion.

### Generic-function dispatch

- Polymorphic call sites are much faster. The dispatch cache is now N-way, so a
  generic function called on several classes in rotation stays warm instead of
  recomputing the applicable methods every call. Standard slot accessors read and
  write the slot directly, and a reader accessor called at a monomorphic site is
  inlined to a direct slot read.

### Concurrency

- `dotcl:compare-and-swap` returns the prior value of the place (matching SBCL,
  CCL, and `Interlocked.CompareExchange`) instead of a boolean. Success is `(eq
  old result)`; a failed swap hands back the current value to reuse as the next
  `old` in a retry loop, with no separate racy re-read.

### Fixes

- `(setf (readtable-case rt) mode)` on a non-CL `readtable-case` -- a package that
  shadows the CL symbol with its own CLOS protocol, such as Eclector's -- reaches
  the user's own writer instead of being hijacked by the built-in.
- `dotnet tool install -g dotcl` installs the host's Ready-to-Run build again.
  The tool's package pointer had listed only the framework-dependent `any` build,
  so installs fell back to the slower non-R2R runtime; it now enumerates every
  per-RID package.

### Thanks

- Bohong Huang -- for the FILE-POSITION string-stream / Gray-stream fix (a pull
  request) and for reporting the `return-from`-in-an-external-macro block-resolution
  bug, both fixed this release.

## v0.1.17 -- 2026-07-10

A performance, interop, and concurrency release, with the usual long tail of
conformance fixes surfaced by getting real libraries (an APL interpreter,
cl-ppcre, serapeum, lparallel) to run on dotcl.

### Performance

- Function-call overhead is cut sharply. Named functions and common-arity
  built-ins now dispatch through per-arity direct delegates instead of packing
  and unpacking an argument array on every call -- the dominant cost in the
  previous call convention.
- Numeric float work stays unboxed. Type inference keeps `(array single-float)`
  / `(array double-float)` in raw `float[]` / `double[]` storage and float
  locals in unboxed slots, so tight numeric kernels no longer box each element or
  each assignment.

### .NET interop

- `dotnet:resolve-type` now finds `PackageReference` types on its own. On a miss
  it loads the managed assemblies sitting in the application base directory and
  retries -- lazily, memoized, and auto-invalidated when a new assembly loads.
  Deployed apps no longer need a manual `load-assembly` or a `typeof(...)`
  force-load to make a referenced type resolvable.
- New `dotnet:class-for-type` returns the CLOS class dotcl uses for a .NET type
  (given a `System.Type` or a type-name string, registering it lazily on first
  call), so you can specialize a method on a .NET type -- including a closed
  generic -- without hand-spelling its long, assembly-qualified class symbol.
- Closed generic types now get distinct, readable class names -- `List<Int32>`,
  `Dictionary<String,Int32>` -- instead of every instantiation colliding on the
  bare ``List`1``, where the friendly name went to whichever instantiation
  happened to be reflected over first. Dispatching on a specific instantiation is now
  predictable, and a one-time warning is emitted if two types still collide on a
  name.
- `defmethod` accepts a class object as a specializer via read-time `#.`, the
  way SBCL and CCL do; libraries such as serapeum rely on it.

### Concurrency

- Opt-in parallel `eval` (`dotcl:set-parallel-eval`), with the internal macro
  table made thread-safe.
- Generic atomic compare-and-swap / increment / decrement over any Common Lisp
  place, plus an `atomic-long` primitive built on `Interlocked`.
- Worker threads now establish a top-level `abort` restart, so an error raised
  in a spawned thread is recoverable rather than fatal to the process (lparallel
  and friends depend on this).

### New tooling

- `decompiler` contrib -- decompile a loaded .NET assembly back to C#.
- `clrmd` contrib -- walk a running process's heap and enumerate live instances.
- The live-method-advice contrib is renamed from `harmony` to `advice`, to avoid
  clashing with the unrelated Quicklisp `harmony` (a sound system). The API is
  unchanged apart from the package name: `advice:watch` / `advice:patch` /
  `advice:trace`, loaded with `(require "advice")`.

### Reader, compiler, and conformance

- Backquote expands `unquote` / `unquote-splicing` inside `#(...)` vector
  literals.
- `rationalize` returns the simplest fraction in the rounding interval;
  `integer-decode-float` decodes single-floats at their true 24-bit precision.
- A long tail of compiler corrections surfaced by that library work:
  `make-instance` initarg values containing a `tagbody` (`loop` / `dolist`), a
  `macrolet`-expanded `return-from` resolving a `defun`'s implicit block,
  `symbol-macrolet` scope shared across both `if` arms, a `handler-case` clause
  variable shadowing an enclosing boxed variable, `setf` of `car` / `cdr`
  evaluating the place after the value, and `#'(setf name)` resolving a local
  `flet` / `labels` `(setf name)`.
- An oversized single form that exceeds the CIL per-method limit now raises a
  clear, catchable error instead of crashing, and a non-top-level `defun`
  registers at run time.

### Distribution

- Per-RID Ready-to-Run tool packages (`dotcl.<rid>`) are packed again in CI, and
  an R2R cold-start regression was fixed.

### Thanks

- Douglas P. Fields, Jr. for the detailed write-up of generic-type dispatch on
  .NET, which drove much of this cycle's interop work.

## v0.1.16 -- 2026-07-03

This cycle adds two pieces of interop tooling -- one-line NuGet resolution and
live method advice -- alongside continued performance work and the usual long
tail of conformance fixes from real-library bring-up.

### NuGet from Lisp

- New `nuget` contrib. `(require "nuget")` then `(nuget:require "SkiaSharp")`
  resolves a NuGet package and its full transitive dependency graph -- version-
  unified managed assemblies plus the RID-specific native libraries -- and
  registers them with the runtime's assembly resolver, from one call. `:version`,
  `:source`, `:prerelease`, `:rid` and `:tfm` are keyword arguments. This folds
  the old manual "download + AssemblyResolve + path index" work into a single
  form, so any NuGet library (Avalonia, SkiaSharp, ...) is reachable from a bare
  `dotcl`.

### Live method advice

- New `harmony` contrib. Attach CLOS-style advice to *any* .NET method at run
  time, with no restart: `(harmony:watch "Type" "Method" fn)` observes a call's
  arguments and result, `harmony:patch` rewrites its return value in place, and
  `harmony:trace` times each call. Built on HarmonyX. Handy for inspecting or
  hot-fixing a running application, or scripting over managed code.

### A GUI in five minutes

- The README now walks through building a cross-platform Avalonia window from a
  single Lisp file (`examples/hello-gui.lisp`), using the `nuget` contrib to
  pull Avalonia at run time.

### .NET interop

- `dotnet:` marshalling round-trips the full set of small integer types
  (byte / sbyte / short / ushort / uint / ulong) in both directions.
- `dotnet:make-ffi-callback` exposes a Lisp function as a native function
  pointer callable from C, for callback-based native APIs.
- New atomic primitives (an `atomic-long` with compare-and-swap / increment /
  decrement) built on .NET's `Interlocked`.

### Performance

- CLOS dispatch is substantially faster in common cases: a dispatch cache for
  EQL-specialized generic functions, lazily-built `call-next-method` closures
  (no per-dispatch allocation), and memoized built-in specializer classes.
- Numeric arrays declared with a bounded integer or float element type now use
  unboxed backing storage, and `aref` / `(setf aref)` take an unboxed integer
  index -- removing per-access boxing in tight loops.

### Gray streams and I/O

- `read-char`, `peek-char`, `read-sequence` and `write-sequence` dispatch to
  user-defined Gray stream methods, and bivalent (binary/character) streams
  route correctly. This is enough to run the flexi-streams library unmodified.

### Reader, printer, and conformance

- Character (`#\`) reading, concatenated streams, and `#n=` / `#n#` shared
  structure across `compile-file` were corrected in several edge cases.
- The pretty printer, `print-object` nesting, and `defstruct` custom printers
  moved closer to the standard.
- A long tail of compiler fixes around multiple-value propagation, symbol-macro
  shadowing, `setf` of `macro-function` in non-CL packages, and `labels` tail-
  call scoping -- mostly invisible until you hit the case they fix.

## v0.1.15 -- 2026-06-29

A conformance-and-robustness release. The bulk of this cycle was driven by
bringing up real libraries (notably the Esrap parser generator) on dotcl, which
surfaced a long tail of edge cases in numerics, the condition system, the
printer, and the compiler. Most changes are invisible until you hit the case
they fix -- but together they move dotcl meaningfully closer to behaving like an
established Common Lisp.

### Numerics

- `decode-float` / `integer-decode-float` now signal on NaN and infinities and
  preserve the sign of negative zero.
- `eql` compares floats bit-for-bit, so `±0.0` and distinct NaNs behave per the
  standard.
- Integer division by zero signals `division-by-zero` (catchable by handlers)
  instead of escaping as a host error.
- Complex `abs` and division use scaled `hypot` / Smith's method, removing
  spurious overflow and underflow. Comparisons and ordering involving floating
  infinities, NaNs, and rationals were corrected.
- `expt` / `exp` flush floating underflow to `0.0` rather than erroring;
  `(expt ±1 huge-exponent)` stays an integer; `asin` / `acos` / `acosh` /
  `atanh` branch into the complex plane exactly where CLHS requires.

### CLOS and the metaobject protocol

- A batch of AMOP introspection and protocol functions landed:
  `compute-applicable-methods-using-classes`,
  `compute-discriminating-function`, `compute-effective-method` (standard method
  combination), `ensure-class-using-class`,
  `ensure-generic-function-using-class`, accessor-method slot definitions, and
  the surrounding introspection accessors.
- `allocate-instance` works on `structure-class`, and `class-of` / MOP slot
  access behave correctly on signalled host conditions.

### Condition system

- `handler-bind`, `handler-case`, `restart-bind`, and `restart-case` are fully
  macroexpandable, so code walkers and `macroexpand-1` see through them.
- Compound handler/condition types such as `(or ...)` and `(and ...)` match,
  and a `define-condition` `:report` is honoured by `princ` and `~A`.

### Printer and FORMAT

- `*print-pretty*` now defaults to true, enabling mandatory newlines inside
  logical blocks.
- Pretty-printing places `~/function/` output in the correct order inside
  conditionals and iteration, and resolves nested `~[...~]` inside `~<...~>`.
- Assorted `~[...~]` / `~<...~>` directive fixes.

### Reader and compiler

- Circular and shared literal constants (`#n=` / `#n#`) work inside `defun` and
  survive `compile-file`, fixing an out-of-memory failure on large
  self-referential literals.
- A literal unknown keyword argument passed to a fixed-`&key` lambda is now
  reported at compile time.
- `(compile name lambda)` installs the function definition, and symbol-macros
  that expand to `NIL` expand correctly.

### Interop and I/O

- Bivalent socket streams carry both character and byte I/O on one stream.
- `dotnet:to-stream` emits BOM-less UTF-8 for text streams.

### Build tooling

- A project build now keeps its compiled dependency FASLs under the project's
  intermediate output directory, so `dotnet clean` removes them along with the
  rest of the build (dotcl/dotcl#47).
- Compile errors during a project build are reported with the original source
  file and line in MSBuild's `file(line): error` form, so IDEs surface them in
  the Error List with click-to-navigate (dotcl/dotcl#48).

### Memory

- Compiled closures and their dynamic methods are now collectible, reducing the
  resident set of long-running compile sessions.

## v0.1.14 -- 2026-06-26

### Async / await

dotcl now speaks .NET's `Task` world. `dotnet:await` blocks for a `Task` /
`ValueTask` result, while `dotcl:async` / `dotcl:await` give non-blocking,
continuation-passing async that yields the thread instead of holding it. The
control-flow operators you expect keep working across an `await` boundary:
special-variable bindings, `handler-bind`, `handler-case` (including its
`:no-error` clause), `unwind-protect`, and `restart-case` all survive a
suspension and resume correctly. The `samples/AspNetLispDemo` sample wires a
non-blocking Lisp endpoint into ASP.NET, producing its `Task` on the ASP.NET
side.

### .NET interop expansion

Interop gained a lot of reach:

- `dotnet:invoke` resolves **extension methods**, so LINQ (`Where`, `Select`,
  `OrderBy`, ...) is callable directly on a receiver.
- `dotnet:make-array` builds sized and multi-dimensional typed .NET arrays, and
  `aref` / `(setf aref)` index a wrapped .NET array transparently -- no unwrap
  step.
- `dotnet:make-generic-type` constructs a closed generic type, and `dotnet:new`
  accepts a `System.Type` (so you can instantiate the type you just built).
- `dotnet:is-instance-of` and `dotnet:cast` for run-time type tests and
  reference conversions; `dotnet:enum-or` combines `[Flags]` enum members;
  `dotnet:call-out-generic` calls generic methods that have `out` / `ref`
  parameters.
- `dotnet:new` admits constructors with an optional tail, fixing a
  struct->primitive overload selection case.
- `handler-bind` / `handler-case` can catch a **specific** .NET exception type:
  the wrapped condition answers `typep` against the CLR exception type, so you
  can handle, say, only `System.IO.FileNotFoundException`.

### CLOS

The class-precedence list now follows the CLHS class linearization, including
non-monotonic hierarchies. A batch of conformance fixes landed: `change-class`
no longer overwrites a target's `:allocation :class` slot; `slot-boundp` and
`slot-value` return a single value; generic-function keyword validation handles
odd / non-symbol keyword plists and `:allow-other-keys` on both the cache-miss
and cache-hit dispatch paths; an invalid method qualifier is signaled by the
operator method-combination; class-redefinition identity no longer reuses a
class whose proper name was cleared; `ensure-generic-function` applies a new
lambda-list / argument-precedence-order in place; and a `make-load-form` creation
form is evaluated at load time.

### Places and `setf`

`setf` of multiple values now distributes correctly --
`(setf (values q r) (floor 17 5))` sets `q` to 3 and `r` to 2 -- and `psetf`
evaluates its value forms before assigning. Assigning to a function-call place,
`(setf (foo x) v)`, returns whatever the underlying `(setf foo)` call returns, as
the standard requires.

### Weak references, finalizers, and weak hash tables

Real GC-backed weak pointers (over `System.WeakReference`),
`dotcl:finalize` / `cancel-finalization` / `run-finalizers`, and hash tables that
support every weakness mode -- including true key-weakness -- are now available.

### Binary Gray streams

`read-byte` / `write-byte` and `read-sequence` / `write-sequence` dispatch to
Gray binary streams, so a custom byte stream behaves like a built-in one.
`princ` / `~A` no longer prints a keyword's package prefix.

### `require`, ASDF, and builds

`cl:require` is wired to ASDF through the module-provider protocol, so
`(require "system")` loads an ASDF system. `dotcl:chdir` backs
`uiop:chdir` / `uiop:with-current-directory`. Project builds gained a
`<DotclAsdSearchPath>` item to register external ASDF system directories
declaratively, and a project-core build loads each `:depends-on` fasl before
compiling the root system.

### Pathnames and I/O

A `:relative-directory` pathname merges against `*default-pathname-defaults*`;
`compile-file-pathname` of a logical pathname stays logical; `delete-file` on a
directory pathname removes the empty directory; `rename-file` overwrites an
existing target (which unblocks ASDF's atomic write-then-rename idiom); and a
literal pathname embedded in compiled code keeps its version component.

### Library compatibility

`remove` / `delete` (and the `-if` / `-if-not` forms) return the **original**
list when nothing is removed, matching the de-facto convention that real
libraries rely on -- for example Maxima's info-list bookkeeping, which mutates the
list in place after a no-op `delete`. Special variables are handled through a
dynamic-binding stack rather than boxing, `coerce` handles compound float type
specifiers, and several previously bundled `trivial-*` shims (gray-streams,
garbage, features, sockets, package-local-nicknames) now defer to
upstream-tracked forks. `dotcl:package-locally-nicknamed-by-list` is provided for
package-local-nickname introspection.

### Platform

dotcl runs on `net10.0-android` as a `PackageReference`-only embedding, and the
emit-free `netstandard2.0` runtime build is fixed so the package packs cleanly
for every target framework. A new `dotcl-cltl2` package provides minimal CLtL2
environment access (`variable-information`, `function-information`,
`declaration-information`, `augment-environment`).

### Other fixes

`.NET` stack traces attached to wrapped conditions are gated behind
`dotcl:*debug-stacktrace*` (off by default); `maphash` tolerates mid-iteration
modification; `function-lambda-expression` reports closure-p as its second value;
`remove-if` returns a single value; and several compiler-correctness fixes for
`eval-when` in compiled files, symbol-macro capture, multi-list `mapcar`, and
tail-call handling in `and` / `or` / `cond`.

## v0.1.13 -- 2026-06-21

### Run precompiled Lisp where runtime code generation is forbidden (NativeAOT / IL2CPP)

dotcl now runs on platforms that ban `Reflection.Emit` entirely. The
`netstandard2.0` runtime build contains no emitter; a tree-walk interpreter
evaluates `eval` / `defun` / `defmacro` / CLOS at run time with no code
generation, while precompiled `.fasl` images run as ordinary linked assemblies.
The new `samples/PrecompiledLispDemoAot` ships precompiled Lisp inside a single
**NativeAOT** native binary -- `IsDynamicCodeSupported` is `False`, no JIT, no
`Assembly.LoadFrom` -- and still evaluates new `defun` / `defmacro` / CLOS at run
time. The same path runs under Unity's **IL2CPP** backend, including WebGL in the
browser -- `samples/PrecompiledLispDemoWebGL` draws a curve whose every point is
computed by precompiled Lisp each frame, and reshapes it live from Lisp typed
into the page.

### Build-task package fixed

The `DotCL.Runtime` 0.1.12 package shipped without its in-process MSBuild task, so
referencing it as a build task failed with `MSB4036`. 0.1.13 includes the task,
and the package build now fails loudly if the task assembly is ever missing rather
than shipping a broken package.

### Declaring build-time dependencies

A project-core build no longer scans your `~/quicklisp` (or Roswell) directories --
a build, and a shipped binary, shouldn't silently depend on whatever happens to be
installed on the build machine. To make external ASDF systems discoverable for a
build, add a `<DotclBuildInit>` item pointing at a Lisp script the build loads
before resolving dependencies:

```xml
<ItemGroup>
  <DotclBuildInit Include="build-setup.lisp" />
</ItemGroup>
```

The script can `(pushnew ... asdf:*central-registry*)`, boot quicklisp, or do
whatever your project needs -- build-time only, so the shipped binary never reaches
into your home directory. (Because `.asd` files are Lisp, you can equivalently put
that setup at the top of your `.asd`.)

### Quicklisp

Fixed a compiler bug -- incorrect shadowing across the function/value namespaces --
that blocked loading quicklisp. The quicklisp-client portability patch is
submitted upstream.

### Gray streams

`format`, `princ` / `prin1` / `print` / `write`, `open-stream-p`, and
`interactive-stream-p` now funnel through the Gray stream protocol instead of
leaking to the console or signaling "not a stream", and Gray stream detection is
robust to a same-named class loaded from another package (for example after
loading `trivial-gray-streams`).

### .NET interop

Typed `dotnet:invoke` infers the receiver type from `let`-bound variables and
propagates it through method chains for direct, boxing-free calls; new
`dotnet:hint-type` / `dotnet:object-type` accessors; `dotnet:box` marshals a
primitive to an implemented interface type; and an unhandled condition crossing a
C# -> Lisp callback boundary is caught instead of crashing the host.

### Other fixes

`get-setf-expansion` returns multiple store variables for the `defsetf` long form;
`#'aref` works through `funcall` / `apply` at any rank; a `call-next-method`
concurrency race is fixed and explicit arguments are honored from `:around`
methods; and `dotcl:backtrace-with-args` adds argument values to backtraces.
Windows UNC paths (`\\server\share\...`) now work with `directory`, `probe-file`,
and related operations -- the server is kept as the pathname host instead of being
mis-parsed as a local directory.

## v0.1.12 -- 2026-06-18

### Precompile your Lisp from a plain PackageReference

A project that references `DotCL.Runtime` can now precompile an ASDF system to a
fasl at build time with a single property -- no repo checkout and no global tool
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
(`MyApp.asd` <-> `(defsystem "MyApp" ...)`).

GUI and heavy-dependency apps work too. The build does **not** need the .NET
types your Lisp references to be loaded -- a `dotnet:define-class` that inherits,
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
path -- `dotcl viewer.lisp image.png` -- no longer tries to load `image.png` as
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

## v0.1.11 -- 2026-06-17

### Type-declared .NET calls compile to a direct call

When the receiver and arguments of `dotnet:invoke` carry a static .NET type via
`(the (dotnet "Type.FullName") x)`, the call now compiles to a direct `callvirt`
to the resolved overload instead of a runtime member lookup -- about 3.5x faster
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
`shared-initialize` methods run when the class is created -- so `:after`-computed
metaclass slots are initialized too.

### dotnet:new with optional-only constructors

`(dotnet:new "Type")` now works for a type whose only constructor has
all-optional parameters (e.g. `FluentTheme(Uri? baseUri = null)`), supplying the
defaults like C#'s `new T()`.

### Windows desktop assemblies

WinForms and WPF assemblies load by simple name --
`(dotnet:load-assembly "PresentationFramework")` -- with transitive
shared-framework references resolved automatically. WinForms runs from the plain
runtime; full WPF should be hosted in a WindowsDesktop (`UseWPF`) app. See
`docs/windows.md` for the recipe, including the STA thread + message-pump setup.

### Embedding (DotCL.Runtime)

The `DotCL.Runtime` package now copies `dotcl.core` and the `contrib` tree to a
referencing project's build output, so `DotclHost.FindCore()` and
`(require :dotnet-class)` (i.e. `dotnet:define-class`) work from a plain
`PackageReference` with no manual asset wiring.

## v0.1.10 -- 2026-06-16

### Faster .NET interop calls

`dotnet:invoke` and `dotnet:static` now cache the resolved method per call shape
(receiver type, member name, argument types), so a hot interop loop pays member
lookup and overload resolution only once -- roughly 4.6x faster on repeated calls.
COM/IDispatch targets, `params` / by-ref methods, and calls with `nil` arguments
keep the previous dynamic dispatch, so behavior is unchanged.

### External processes via uiop:run-program

A new `dotcl:launch-process` primitive exposes a streaming child-process handle
(stdin/stdout/stderr, wait, exit code), and `uiop:run-program` /
`uiop:launch-program` are implemented on top of it.

### Compiler macros are applied during compilation

Compiler macros defined with `define-compiler-macro` (or via
`(setf (compiler-macro-function ...) ...)`) are now expanded while compiling call
forms, per CLHS 3.2.2.1 -- previously they were registered but never consulted. A
macro that declines (returns the original form) or a locally shadowed operator
leaves the call untouched.

### Correct class identity for same-named .NET types

Two .NET types that share a simple name in different namespaces (e.g.
`System.Collections.ArrayList` vs. another `ArrayList`) now map to distinct Lisp
classes, so `class-of`, `typep`, and method dispatch no longer conflate them.

### Embedding (DotCL.Runtime)

The embeddable runtime library gains `DotclHost.ToClr` (convert a Lisp value to a
.NET object), `DotclHost.Register` (expose a host function to Lisp), and a
precompiled-only mode that forbids runtime code generation -- for hosts where
dynamic code is unavailable (AOT / IL2CPP-style targets). `DotCL.Runtime` now
multi-targets `net8.0` in addition to `net10.0`.

## v0.1.9 -- 2026-06-13

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
  `slot-boundp`/`slot-makunbound-using-class` are wired up -- enough MOP for
  custom-metaclass dynamic slots.
- `defmethod` specializers work on `dotnet:define-class` instances and on raw
  CLR types (dispatch on a .NET object's class).

### .NET interop

- `dotnet:resolve-type` is now public -- turn a type-name string into a
  `System.Type` (#17).
- `dotnet:invoke-generic` -- call generic instance methods (#23).
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

- `dotcl:backtrace` and `dotcl:print-backtrace` -- Lisp-callable backtraces; the
  printed form now includes call arguments.
- `dotcl:jit-disassemble` (contrib) -- view the JIT-generated native code of a
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

## v0.1.8 -- 2026-05-22

### ANSI test suite: 21928/21929 pass (99.995%)

One additional conformance fix since 0.1.7: `FORMAT ~E` subnormal double
rounding now passes consistently (was flaky). Known failure: `DEFGENERIC.ERROR.1`
(SBCL-compatibility warn instead of error, intentional).

### New: `dotnet:call-base` -- invoke base class methods

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

### New: `dotnet:define-class` -- base constructor arguments

`:ctor` bodies may now start with `(:base arg1 arg2 ...)` to pass arguments
to the base class constructor (C# `: base(...)`):

```lisp
(dotnet:define-class "MyRenderer" ("SomeBaseRenderer")
  (:ctor ((device "GraphicsDevice"))
    (:base device)
    (initialize-renderer self device)))
```

### New: `dotnet:define-class` -- constructors with arguments supported

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

### New: `dotcl:dotcl-homedir-pathname` -- ASDF source-registry integration

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

## v0.1.7 -- 2026-05-16

### ANSI test suite: 21927/21929 pass (99.99%)

Updated to the latest ansi-test submodule (21791 -> 21929 tests). Four
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

### New: `dotcl-repl` -- terminal readline contrib

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

## v0.1.6 -- 2026-05-07

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

### New: `save-application :executable` -- ASDF/UIOP standalone exe

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

## v0.1.5 -- 2026-05-06

CLHS conformance pass: completed a chapter-by-chapter audit of CLHS
chapters 2-25 and fixed the spec violations found. ANSI test pass count
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
- `code-char` returns NIL for codes >= `char-code-limit`.
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

## v0.1.4 -- 2026-05-04

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
  GF, and custom slot options -- unblocking `closer-mop` and libraries
  built on it.
- Reader: full Unicode character-name support (UCD-derived tables
  generated at build time), including non-BMP `#\Uxxxxxxxx`.
- `with-compilation-unit` honors `:override`.
- Threading primitives exposed as the `DOTCL` public API.
- `bordeaux-threads-2` lock timeout / `with-timeout`.
- `trivial-gray-streams` lambda-list and `stream-element-type` GF fixes.

## v0.1.3 -- 2026-05-03

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

## v0.1.2 -- 2026-05-03

Build, packaging, and toolchain release.

### Added

- `compile-file :target-features` for cross-compiling FASLs against a
  different feature set; per-OS asdf fasl loading.
- `--version` reports a `git describe`-derived semver string.
- `:package-local-nicknames` added to `*features*`.

### Fixed

- Public mirror build / install issues (dotcl/dotcl #1, #2).
- `ros-pack`: per-RID tarballs now have the `runtime` exec bit set, and
  cross-RID R2R FASL duplication is removed.
- `handler-case`: `*in-try-block*` propagation in `var-is-special`
  branches.
- Copy-propagation peephole removes single-reference `let` / `let*`
  locals.

## v0.1.1 -- 2026-04-30

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

## v0.1.0 -- 2026-04-29

Initial public release.

### Highlights

- Common Lisp implementation on .NET 10. Lisp source is compiled to CIL
  and runs on the .NET JIT -- same Lisp image runs on Windows, macOS, and
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
