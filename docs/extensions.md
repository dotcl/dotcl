# Extensions

Everything dotcl offers beyond ANSI Common Lisp, in one list: the packages that
are there from the start, the modules `require` loads, and the one piece of
reader syntax. Each line names a thing and says what it is for; the linked page
has the details. Check here before building something new: the part you need
may already exist.

Several of these are what portable libraries call underneath. When a portable
interface exists, code that should also run elsewhere is better off with it:
UIOP for the environment, files and processes, bordeaux-threads for threads,
trivial-garbage for weak pointers and finalizers, closer-mop for the
metaobject protocol, static-vectors for pinned vectors.

## Packages

| Package | How you get it | What it is |
| --- | --- | --- |
| `dotnet` | built in | Calling .NET: objects, types, delegates, tasks, assemblies, native libraries. [Calling .NET from Lisp](dotnet-package.md) |
| `dotcl` | built in | The rest of the runtime's extensions: the process, threads, memory, packages, introspection, delivery |
| `dotcl-mop` | built in | The metaobject protocol (AMOP): class, slot, generic function and method metaobjects |
| `dotcl-cltl2` | built in | The CLtL2 environment functions: `variable-information`, `function-information` and `declaration-information` report the proclamations and, given the `&environment` of a macro expanded by the compiler, the lexical bindings and declarations around the call; `augment-environment`, `parse-macro`, `enclose`, `macroexpand-all`. The interpreter's environments carry no declarations |

## Modules

`require` loads a module that ships with dotcl; nothing is downloaded. Each
module dotcl itself provides has a directory under `contrib/` with a README.
The first four groups below are libraries for your programs. The last one,
development tools, is for looking inside a running process from the REPL
rather than for code you ship.

### Systems and libraries

- `asdf`: ASDF, for loading systems defined in `.asd` files.
  `(require "asdf")`. [Using libraries](libraries.md#common-lisp-libraries)
- `quicklisp`: the Quicklisp client. `(require "quicklisp")` loads it without
  touching the network; the first `ql:quickload` installs a dist.
  [Using libraries](libraries.md#common-lisp-libraries)
- `dotcl-lock` (experimental, the lock format may change): loads two versions
  of one system in the same image. A lock file names the `.asd`
  of each system and puts the conflicting version in a package unit:
  `(dotcl-lock:use-lock "dotcl-systems.lock")`, then `asdf:load-system`.
  `resolve.lisp` beside it writes the lock from directories holding several
  versions of a system, choosing versions as Cargo does (also runs on SBCL).
- `asdf-version-range` (experimental): a `:version` dependency may name a
  range, `(:version "foo" (:^ "1.2"))` or `(:version "foo" (:>= "1.2" :< "2"))`,
  and ASDF checks it. Other ASDFs accept the form and ignore the range. Also
  runs on SBCL.
- `asdf-parallel` (experimental): builds a system with several files compiling
  at once, on threads, following ASDF's dependencies:
  `(asdf-parallel:load-system "ironclad" :jobs 4)`. Loads stay in the calling
  thread. The same file runs on SBCL, with forked processes.
- `asdf-extra-deps` (experimental): adds dependencies a system uses but does
  not declare, from a supplement file guarded by the hash of its `.asd`,
  without editing it: `(asdf-extra-deps:load-supplements "file.sexp")`.
  `audit.lisp` beside it finds them by building the system in two orders.
  Also runs on SBCL.

### Common Lisp compatibility

What other implementations provide and portable libraries build on.

- `dotcl-thread`: threads, locks, condition variables and semaphores, with a
  bordeaux-threads style surface. `(require "dotcl-thread")`, then
  `(dotcl-thread:make-thread (lambda () ...))`.
- `dotcl-socket`: TCP sockets as bidirectional Lisp streams.
  `dotcl-socket:socket-connect`, or `make-server-socket` and `socket-accept`;
  then read and write the `socket-stream`. For UDP, socket options and
  timeouts, use a portable library such as usocket.
- `dotcl-gray`: Gray streams. Subclass `dotcl-gray:fundamental-character-input-stream`
  and its siblings, and define the `stream-*` generic functions.
- `dotcl-float`: the IEEE bit patterns of floats (`dotcl-float:double-float-bits`,
  `bits-double-float`), the infinities and NaNs as constants, and
  `dotcl-float:float-nan-p` and `float-infinity-p` (as in `sb-ext`).
- `dotcl-rotate-byte`: `dotcl-rotate-byte:rotate-byte`, as in sb-rotate-byte.
  A rotate of a whole `(unsigned-byte 32)` or `(unsigned-byte 64)` word
  (`(byte 32 0)`, `(byte 64 0)`) compiles to one rotate instruction.
- `dotcl-simd`: 128-bit SIMD values and operations with the names of sb-simd
  (`dotcl-simd:f64.2`, `f64.2+`, `u8.16-aref` ...). [dotcl-simd](dotcl-simd.md)

### The REPL and editors

- `dotcl-repl`: the REPL's line editor, history, completion and comma commands.
  The REPL loads it; elsewhere, `(dotcl-repl:readline "> ")` reads a line with
  it. `dotcl-repl:*terminal*` and `make-terminal` put a host's own key source
  and output in place of System.Console, and `run-repl` / `interrupt-repl` run
  the whole REPL there. [The REPL](repl.md)
- `dotcl-code-info`: what an editor asks a live image about the code at a point:
  `dotcl-code-info:completions`, `documentation-url` and `describe-at`, each given
  the text and an offset into it.
- `dotcl-lsp`: a Language Server Protocol server in the running process, on
  standard input and output (`dotcl-lsp:serve-stdio`) or TCP
  (`dotcl-lsp:start-server`). An early stage: documents, diagnostics and hover.
- `dotcl-lsp-coalton`: Coalton type errors and types for `dotcl-lsp`, checked
  from the editor's text without loading it. Needs Coalton loaded.

### .NET

- `dotnet-class`: `dotnet:define-class` emits a named .NET type from Lisp, for
  .NET code that wants a subclass or an interface implementation.
  [Defining .NET classes](define-class.md)
- `dotnet-ffi`: `dotnet:define-ffi` declares a native function and makes a Lisp
  function that calls it (P/Invoke).
- `dotnet-nuget`: resolves NuGet packages and their dependencies, with versions
  and a lock file: `(nuget:require "Newtonsoft.Json" :version "13.0.3")`.
  [Using libraries](libraries.md#net-packages)
- `asdf-nuget`: a `(:nuget "Id" :nuget-version "...")` component for `.asd`
  files, named in `:defsystem-depends-on`. Also `check-portability-op`, which
  lists what would tie a system's fasls to one OS or CPU; `dotcl pack
  --library` runs the same check. [Using libraries](libraries.md#in-a-system-definition),
  [Portable fasls](portable-fasls.md)
- `dotnet-cs`: compiles C# with Roslyn and splices its IL into a Lisp function:
  `(dotnet-cs:inline-cs ((x long a)) :returns long :body "return x * 2;")`.
  `long` arguments and result only, for now.

### Development tools

For looking inside the running process while you develop. The first three
resolve their NuGet package on first use.

- `dotcl-advice`: watch, patch or time any .NET method in the running process
  (HarmonyX): `advice:watch`, `advice:patch`, `advice:trace`.
- `dotcl-clrmd`: walks the managed heap of the running process (ClrMD):
  `clrmd:instances-of`, `clrmd:heap-report`.
- `dotcl-decompiler`: decompiles a .NET type or method back to C# (ILSpy's
  engine): `decompiler:type-source`, `decompiler:method-source`.
- `dotcl-jitdisasm`: makes `dotcl:jit-disassemble` print the native code the JIT
  produced for a function. Not part of a packaged dotcl: build it from a source
  tree with `make contrib-dotcl-jitdisasm`.
- `dotcl-dap`: a Debug Adapter Protocol server in the running process
  (`dotcl-dap:start-server`). A function breakpoint, or a condition that would
  enter the debugger, stops only the thread it happens on; the REPL and other
  threads keep running. Function breakpoints, stack, variables, evaluate,
  continue, and resuming through a restart. Line breakpoints and stepping are
  not there yet.

### Lower layers

Not meant to be used directly.

- `dotcl-bsd-sockets`: TCP and UDP over System.Net.Sockets in the shape of
  SBCL's sb-bsd-sockets. The layer under the dotcl backends of socket
  libraries such as usocket; use those.
- `dotcl-jsonrpc`: JSON and the `Content-Length` framing of DAP and LSP
  messages, under `dotcl-dap` and `dotcl-lsp`.

## Reader syntax

- `#m1.50`: a `System.Decimal` literal, scale kept. [Numbers](numbers.md#decimal)

## The `dotnet` package

[Calling .NET from Lisp](dotnet-package.md) covers this package with examples.
The names below are grouped as that page is.

Objects and members:

- `dotnet:new`: construct an instance, choosing the constructor by its arguments.
- `dotnet:invoke`: an instance method, property or field; a place.
- `dotnet:static`: a static method, property or field; a place.
- `dotnet:->`: a chain of member accesses in call order; a place.
- `dotnet:doto`: apply several members to one object and return the object.
- `dotnet:invoke-generic`, `dotnet:static-generic`: call a generic method with explicit type arguments.
- `dotnet:call-out`, `dotnet:call-out-generic`: call a method with `out` / `ref` parameters; they come back as extra values.
- `dotnet:call-base`: call the base class implementation of a virtual method.
- `dotnet:box`: pass a value as a particular .NET type, to pick an overload. `dotnet:hint-type` reads that type back.
- `dotnet:null`: an explicit .NET `null` argument (`nil` is not one).
- `dotnet:enum-or`: combine `[Flags]` enum members.
- `dotnet:ref` (from `dotnet-class`): an indexer; a place.
- `dotnet:using` (from `dotnet-class`): C#'s `using`; disposes on the way out.

Types:

- `dotnet:resolve-type`: a `System.Type` from its name.
- `dotnet:make-generic-type`: a closed generic type from an open one and type arguments.
- `dotnet:object-type`: the runtime type of a .NET object.
- `dotnet:is-instance-of`, `dotnet:cast`: type test, and a checked reference cast.
- `dotnet:class-for-type`: the CLOS class dotcl uses for a .NET type. [Dispatching on .NET types](dotnet-package.md#dispatching-on-net-types)
- `dotnet:clear-type-cache`: forget memoized type lookups.
- `dotnet:define-class` (from `dotnet-class`): emit a real .NET class from Lisp. [Defining .NET classes](define-class.md)
- `dotnet:library`, `dotnet:register-class-handlers` (from `dotnet-class`): write several classes into a `.dll` for C# to reference, and attach their Lisp bodies when the `.dll` is used. [Producing a .dll](define-class.md#producing-a-dll-for-c-to-reference)
- `dotnet:*type-aliases*` (from `dotnet-class`): short names for types in `define-class` signatures. [Defining .NET classes](define-class.md#the-shape-of-a-definition)

Arrays, streams, delegates, events, tasks:

- `dotnet:new-array`, `dotnet:make-array`: typed .NET arrays, from elements or from dimensions.
- `dotnet:to-stream`: wrap a `System.IO.Stream` as a Lisp character, binary or bivalent stream.
- `dotnet:make-delegate`: a Lisp function as a .NET delegate. [Delegates and callbacks](dotnet-package.md#delegates-and-callbacks)
- `dotnet:add-event`, `dotnet:remove-event`: subscribe a Lisp function to a .NET event, and unsubscribe it.
- `dotnet:await`: block until a .NET task completes and return its result. [Tasks](dotnet-package.md#tasks)
- `dotnet:ui-invoke`, `dotnet:ui-post` (Windows): run a function on the WinForms / WPF UI thread. [UI threads](dotnet-package.md#ui-threads)

Exceptions:

- `dotnet:handler-bind`: `handler-bind` with .NET exception type names as the types.
- `dotnet:exception-typep`, `dotnet:exception-type`, `dotnet:exception-object`: the .NET exception behind a condition.

Finding what to call:

- `dotnet:members`: the members of a type or object, with signatures. [What can I call?](dotnet-package.md#what-can-i-call)
- `dotnet:documentation`: the XML documentation .NET ships for a type or member.
- `dotnet:type-names`: type and namespace names starting with a prefix.

Assemblies and packages:

- `dotnet:load-assembly`: load an assembly by name or path. [Loading assemblies](dotnet-package.md#loading-assemblies)
- `dotnet:require`: download and load a NuGet package by name. For anything beyond a quick script, `nuget:require` from `dotnet-nuget` is the one with versions, lock files and packaging. [Using libraries](libraries.md#net-packages)

Native code:

- `dotnet:ffi`: call a function in a native library, giving argument and return types.
- `dotnet:define-ffi` (from `dotnet-ffi`): define a Lisp function that calls a native function.
- `dotnet:load-library`, `dotnet:free-library`, `dotnet:library-path`: load and unload a native library.
- `dotnet:find-symbol`, `dotnet:find-symbol-any`: the address of an exported native symbol.
- `dotnet:alloc-mem`, `dotnet:free-mem`, `dotnet:mem-read`, `dotnet:mem-write`: unmanaged memory.
- `dotnet:type-size`, `dotnet:type-align`: the marshalled size and alignment of a type.
- `dotnet:make-ffi-callback`: a Lisp function as a native function pointer.

## The `dotcl` package

Scripts and the process:

- `dotcl:script-arguments`: the arguments a script was given. [Writing scripts](scripting.md#arguments)
- `dotcl:command-line-arguments`: the command line, in the shape UIOP expects. [Writing scripts](scripting.md#arguments)
- `dotcl:quit`: exit the process with a code. [Writing scripts](scripting.md#exit-code)
- `dotcl:*exit-hooks*`: functions of no arguments called, in order, when the process exits through `dotcl:quit` or at the end of the program, as `sb-ext:*exit-hooks*`. An error in one is printed and the rest still run.
- `dotcl:getenv`, `dotcl:setenv`, `dotcl:unsetenv`: environment variables.
- `dotcl:posix-environ`: the whole environment as a list of `"NAME=VALUE"` strings, as `sb-ext:posix-environ`; the form `dotcl:launch-process` takes for `:environment`.
- `dotcl:getpid`: the process id, as `sb-posix:getpid`.
- `dotcl:getcwd`, `dotcl:chdir`: the process's current directory.
- `dotcl:native-namestring`, `dotcl:parse-native-namestring`: convert between a pathname and the operating system's spelling of it.
- `dotcl:delete-directory`: delete a directory; `:recursive t` deletes its contents too.
- `dotcl:user-init-file`: the init file dotcl loads at startup. [The REPL](repl.md#the-init-file)
- `dotcl:dotcl-homedir-pathname`: the directory dotcl's bundled modules are found under.
- `dotcl:*host-features*`: the features of the platform dotcl is running on, even while `*features*` is rebound for another target.

External processes:

- `dotcl:run-process`: run a program to completion; returns the exit code, standard output and standard error.
- `dotcl:launch-process`: start a program with its standard streams as Lisp streams, or redirected to files. As with `sb-ext:run-program`, `:input`, `:output` and `:error` also take any Lisp stream (a string stream, `*standard-output*`, a Gray stream), copied in a thread until the pipe ends, and `:error :output` sends the error output where the output goes. `dotcl:process-wait` returns once the copies are done, and signals an error that stopped one. On Unix the child leads a process group of its own unless `:input` is `t` (as with SBCL), so killing its group does not reach the caller.
- `dotcl:process-input`, `dotcl:process-output`, `dotcl:process-error`: those streams.
- `dotcl:process-wait`, `dotcl:process-alive-p`, `dotcl:process-exit-code`, `dotcl:process-pid`, `dotcl:process-kill`: wait for, query and stop a launched process.

Threads:

- `dotcl:make-thread`, `dotcl:current-thread`, `dotcl:all-threads`, `dotcl:thread-join`, `dotcl:thread-yield`: create and join threads (they are .NET threads).
- `dotcl:threadp`, `dotcl:thread-name`, `dotcl:thread-alive-p`, `dotcl:thread-object`: about a thread.
- `dotcl:interrupt-thread`, `dotcl:destroy-thread`: run a function in another thread, or stop it.
- `dotcl:without-interrupts`, `dotcl:with-interrupts`, `dotcl:allow-with-interrupts`, `dotcl:with-local-interrupts`, `dotcl:*interrupts-enabled*`, `dotcl:*allow-with-interrupts*`: hold back `interrupt-thread`, `destroy-thread` and Ctrl-C until the body is left (normally or by a non-local exit), as in SBCL. A sleep or wait inside the body is not cut short, except that `destroy-thread` still ends a thread blocked in one. `with-local-interrupts` lets them in again inside the body; `with-interrupts` does only under `allow-with-interrupts`.
- `dotcl:make-lock`, `dotcl:acquire-lock`, `dotcl:release-lock`, `dotcl:lockp`, `dotcl:make-recursive-lock`, `dotcl:recursive-lock-p`: locks.
- `dotcl:make-condition-variable`, `dotcl:condition-wait`, `dotcl:condition-notify`, `dotcl:condition-broadcast`, `dotcl:condition-variable-p`: condition variables.
- `dotcl:make-semaphore`, `dotcl:signal-semaphore`, `dotcl:wait-on-semaphore`, `dotcl:semaphore-p`: semaphores.
- `dotcl:compare-and-swap`, `dotcl:atomic-incf`, `dotcl:atomic-decf`: atomic operations on a place.
- `dotcl:make-atomic-long`, `dotcl:atomic-long-value`, `dotcl:set-atomic-long-value`, `dotcl:atomic-long-cas`, `dotcl:atomic-long-incf`, `dotcl:atomic-long-decf`, `dotcl:atomic-long-p`: a 64-bit cell with atomic operations.
- `dotcl:call-on-main-thread`, `dotcl:main-thread-p`: run a function on the thread the process started on, which macOS UI work needs. [UI threads](dotnet-package.md#ui-threads)
- `dotcl:set-parallel-eval`, `dotcl:parallel-eval-p`: let `eval` and `compile` run on several threads at once instead of one at a time.

Asynchronous code:

- `dotcl:async`, `dotcl:await`: write a body that waits for .NET tasks without blocking a thread; `async` evaluates to a task. Unlike `dotnet:await`, which blocks.

Memory:

- `dotcl:gc`: run a full collection, then the pending finalizers.
- `dotcl:gc-stats`: collection counts per generation, heap size, bytes allocated, and the total time the collector has paused the process in internal time units (as `sb-ext:*gc-run-time*`).
- `dotcl:make-weak-pointer`, `dotcl:weak-pointer-value`, `dotcl:weak-pointer-p`: weak pointers.
- `make-hash-table :weakness` and `dotcl:hash-table-weakness`: weak hash tables (`:key`, `:value`, `:key-and-value`, `:key-or-value`).
- `dotcl:finalize`, `dotcl:cancel-finalization`, `dotcl:run-finalizers`: run a function after an object is collected.
- `dotcl:make-pinned-vector`, `dotcl:pinned-vector-address`, `dotcl:unpin-vector`: a vector the collector does not move, so native code can use its address.
- `dotcl:pinned-vector-p`: true for a pinned vector whose storage has not been replaced (by `adjust-array`); never signals.

Packages:

- `dotcl:add-package-local-nickname`, `dotcl:remove-package-local-nickname`, `dotcl:package-local-nicknames`, `dotcl:package-locally-nicknamed-by-list`: package-local nicknames; `defpackage` takes `:local-nicknames` too.
- `dotcl:lock-package`, `dotcl:unlock-package`, `dotcl:package-locked-p`: package locks. Redefining an external symbol of a locked package signals `package-error`.
- `dotcl:without-package-locks`, `dotcl:*package-locks-disabled*`: suspend the locks for a body.

Introspection and debugging:

- `dotcl:function-lambda-list`: the lambda list of a function or macro. [Using libraries](libraries.md#what-ships-with-dotcl)
- `dotcl:function-source-location`: the file and line where a name was last defined.
- `dotcl:watch-fdefinition`: call a function after each change to a symbol's global function definition (a debugger re-wrapping a redefined function).
- `dotcl:definition-sources`: every recorded definition of a name (function, macro, compiler macro, generic function and each method, variable, class, type, method combination, ...; a package's `defpackage` given its name), each with its kind, file and line.
- `dotcl:definition-source`: the file and line where a function, macro function, generic function, method, class or package object was defined.
- `dotcl:record-definition-sources`: attribute the definitions in a form to a file and line, for a tool that compiles one form at a time.
- `dotcl:who-calls`, `dotcl:who-is-called-by`: cross references between functions.
- `dotcl:typexpand-1`, `dotcl:typexpand`: expand a `deftype`.
- `dotcl:symbol-global-value`: the global value of a symbol, ignoring dynamic bindings; a place.
- `dotcl:charpos`: the column an output stream is at.
- `dotcl:pretty-stream-p`: true for the stream of a logical block being pretty printed, including the stream a `print-object` method or a condition report is handed inside one; as `sb-pretty:pretty-stream-p`.
- `dotcl:backtrace`, `dotcl:backtrace-with-args`, `dotcl:print-backtrace`: the Lisp call stack.
- `dotcl:frame-locals`, `dotcl:print-frame-locals`, `dotcl:frame-specials`: a frame's lexical variables (when compiled with `dotcl:*emit-frame-locals*` true) and the dynamic bindings in effect. [The REPL](repl.md#when-a-form-signals)
- `dotcl:*debug-stacktrace*`: print the .NET stack trace with an unhandled error.
- `dotcl:*foreign-callback-handler*`, `dotcl:*foreign-callback-propagate*`: what happens to a Lisp error inside a callback from .NET. [Delegates and callbacks](dotnet-package.md#delegates-and-callbacks)
- `dotcl:jit-disassemble`: the native code of a function; needs the `dotcl-jitdisasm` module.

Compiling and shipping:

- `dotcl:save-application`: bundle sources into a FASL or an executable. [Packaging an app](dotcl-pack.md)
- `dotcl:combine-fasls`: one FASL that loads several; what `uiop:combine-fasls` calls.
- `dotcl:fasl-version-mismatch`, `dotcl:fasl-version-mismatch-pathname`, `dotcl:fasl-version-mismatch-built-by`, `dotcl:load-as-is`: the error LOAD signals for a FASL another dotcl compiled; the restart `dotcl:load-as-is` (or `continue`) loads it anyway, except when it needs a newer `DotCL.Runtime`. ASDF's `load-op` compiles such a file again instead.
- `dotcl:*compile-r2r*`, `dotcl:*load-r2r*`, `dotcl:write-r2r-sibling`, `dotcl:r2r-stats`: ahead-of-time compiled siblings of FASLs. [ReadyToRun](readytorun.md)
- `dotcl:package-rid`: the portable RID (`linux-x64`, `osx-arm64`, `linux-musl-x64`, ...) that compiled packages are made and looked up for. [Libraries](libraries.md)
- `dotcl:precompiled-only`: refuse to generate code at run time, so only precompiled code runs. [Embedding dotcl](embedding.md#without-run-time-code-generation)
- `dotcl:register-assembly-path`, `dotcl:register-native-path`: tell the assembly resolver where a managed or native library is.

Text and octets:

- `dotcl:string-to-octets`, `dotcl:octets-to-string`: encode a string to a `(simple-array (unsigned-byte 8) (*))` and back, as in `sb-ext`. `:external-format` takes what `open` takes and defaults to UTF-8; `:start`, `:end`, and `:null-terminate` for encoding. A character that cannot be encoded, or bytes that are not valid in the format, become the format's replacement character.

Numbers:

- `dotcl:decimal`, `dotcl:decimalp`: the `System.Decimal` type. [Numbers](numbers.md#decimal)
- `dotcl:double-float-positive-infinity`, `dotcl:double-float-negative-infinity`, `dotcl:double-float-nan`, and the same three for `single-float`: constants that cannot be written as literals.

Profiling:

- `dotcl:alloc-report`, `dotcl:alloc-stacks`, `dotcl:alloc-reset`: per-type allocation counts and sampled stacks, with `DOTCL_ALLOC_PROF=1`. [Profiling](profiling.md#allocation-rather-than-time)
- `dotcl:phase-report`, `dotcl:phase-reset`: where `compile-file` spent its time, with `DOTCL_PHASE_PROF=1`.

## Elsewhere

- The command line, `--load`, `--eval` and shebang scripts: [Writing scripts](scripting.md).
- The REPL's commands and init file: [The REPL](repl.md).
- `dotcl pack`, which turns an ASDF system into a .NET tool: [Packaging an app](dotcl-pack.md).
- Running Lisp inside a .NET program (`DotclHost`): [Embedding dotcl](embedding.md).
- Compiling `.lisp` files in a .NET build (`DotCL.Sdk`): [SDK and MSBuild properties](sdk.md).
- Where dotcl differs from the standard or from SBCL on purpose: [Deliberate deviations](deviations.md).
