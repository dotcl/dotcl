# dotcl documentation

Guides and reference for dotcl: Common Lisp for .NET applications.

**Embedding dotcl in a .NET program?** Start with the
[Quick start](../README.md#quick-start), then [Embedding dotcl](embedding.md).
**Using dotcl as a Lisp?** Start with [dotcl for Lisp programmers](for-lispers.md).

## Embedding in a .NET application

- [Embedding dotcl](embedding.md): start the runtime, evaluate and call Lisp,
  pass values both ways, errors, output
- [Hosting dotcl safely](hosting-safely.md): what Lisp code can reach, one
  image per process, threads
- [SDK and MSBuild properties](sdk.md): compiling `.lisp` files as part of a
  .NET build, and which knob adds a dependency
- [Upgrading a .NET host](upgrading.md): changes to the C# surface of
  `DotCL.Runtime` between releases, and what to write instead

## Interop with .NET

- [Calling .NET from Lisp](dotnet-package.md): the `dotnet:` package
- [Defining .NET classes](define-class.md): emit a real .NET type from Lisp
- [Numbers across the boundary](numbers.md): which .NET numeric types arrive
  as which Lisp types

## Performance

- [Benchmarks](benchmarks.md): measured numbers, and how to reproduce them
- [Profiling](profiling.md): CPU and allocation profiles; your Lisp function
  names appear in the stacks as-is
- [Ahead-of-time compiled FASLs](readytorun.md): ReadyToRun siblings, what
  ships with one, how to tell they are being used

## Using dotcl as a Lisp

- [dotcl for Lisp programmers](for-lispers.md): what is the same, what is
  different, and what .NET adds
- [Getting started](getting-started.md): install, `dotnet new dotcl-app`, add
  a library, publish an `.exe`
- [Using libraries](libraries.md): ASDF, Quicklisp, and pulling in .NET /
  NuGet dependencies
- [Library status](library-status.md): which Quicklisp libraries load and pass
  their tests
- [The REPL](repl.md): starting one, the line editor, the init file, and what
  happens when a form signals
- [Writing scripts](scripting.md): arguments, exit codes, shebang, and how
  `--load` differs from a positional file
- [Packaging an app](dotcl-pack.md): turn an ASDF system into a dotnet tool
- [Deliberate deviations](deviations.md): where dotcl knowingly differs from
  the standard or from SBCL, and why

## Examples

Scripts you can run as they stand, in `examples/`:

- [`http-json.lisp`](../examples/http-json.lisp): resolve a NuGet package, await
  an async .NET method, read the JSON that comes back
- [`crypto.lisp`](../examples/crypto.lisp): the class library with nothing to
  install: hashing, key derivation, authenticated encryption
- [`hello-gui.lisp`](../examples/hello-gui.lisp): a cross-platform window
  (Avalonia)
- [`windows/`](../examples/windows/): COM, WMI, P/Invoke and WinForms

`samples/` holds the other direction: complete projects where a .NET host,
ASP.NET Core, MAUI, MonoGame, drives Lisp code.

## Platform and tooling

- [Editors](editors.md): connecting Emacs (SLIME or SLY) to dotcl
- [Windows](windows.md): Windows-specific notes (COM / WMI / P/Invoke, UI)

## Contributing / development

- [Line coverage for `.lisp`](coverage.md): an off-the-shelf .NET coverage
  collector reports line coverage against Lisp sources, via the PDB that
  `compile-file` writes
