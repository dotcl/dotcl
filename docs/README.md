# dotcl documentation

Guides and reference for dotcl: Common Lisp on .NET.

**New here?** [Getting started](getting-started.md) walks from installing
dotcl to a published executable, one command at a time. The
[five-minute tutorial](../README.md) in the top-level README is the shorter
look; the documents below go deeper on one topic each.

## Using dotcl

- [Getting started](getting-started.md): install, `dotnet new dotcl-app`, add
  a library, publish an `.exe`
- [Using libraries](libraries.md): ASDF, Quicklisp, and pulling in .NET /
  NuGet dependencies
- [SDK and MSBuild properties](sdk.md): every property a Lisp build reads,
  and which knob adds a dependency
- [Packaging an app](dotcl-pack.md): turn an ASDF system into a dotnet tool
- [The REPL](repl.md): starting one, the line editor, the init file, and what
  happens when a form signals
- [Writing scripts](scripting.md): arguments, exit codes, shebang, and how
  `--load` differs from a positional file
- [Deliberate deviations](deviations.md): where dotcl knowingly differs from
  the standard or from SBCL, and why

## Interop with .NET

- [Calling .NET from Lisp](dotnet-package.md): the `dotnet:` package
- [Defining .NET classes](define-class.md): emit a real .NET type from Lisp
- [Numbers across the boundary](numbers.md): which .NET numeric types arrive
  as which Lisp types

## Examples

Scripts you can run as they stand, in `examples/`:

- [`http-json.lisp`](../examples/http-json.lisp): resolve a NuGet package, await
  an async .NET method, read the JSON that comes back
- [`crypto.lisp`](../examples/crypto.lisp): the class library with nothing to
  install: hashing, key derivation, authenticated encryption
- [`hello-gui.lisp`](../examples/hello-gui.lisp): a cross-platform window
  (Avalonia), the file from the README
- [`windows/`](../examples/windows/): COM, WMI, P/Invoke and WinForms

`samples/` holds the other direction: complete projects where a .NET host,
ASP.NET Core, MAUI, MonoGame, drives Lisp code.

## Platform and tooling

- [Windows](windows.md): Windows-specific notes (COM / WMI / P/Invoke, UI)
- [Profiling](profiling.md): CPU and allocation profiles; your Lisp function
  names appear in the stacks as-is
- [Ahead-of-time compiled FASLs](readytorun.md): ReadyToRun siblings, what
  ships with one, how to write your own, how to tell they are being used

## Contributing / development

- [Line coverage for `.lisp`](coverage.md): an off-the-shelf .NET coverage
  collector reports line coverage against Lisp sources, via the PDB that
  `compile-file` writes
