# dotcl documentation

Guides and reference for dotcl — Common Lisp on .NET. Start with the
[five-minute tutorial](../README.md) in the top-level README; the documents
below go deeper on one topic each.

## Using dotcl

- [Using libraries](libraries.md) — ASDF, Quicklisp, and pulling in .NET /
  NuGet dependencies
- [Packaging an app](dotcl-pack.md) — turn an ASDF system into a dotnet tool
- [Writing scripts](scripting.md) — arguments, exit codes, shebang, and how
  `--load` differs from a positional file

## Interop with .NET

- [Calling .NET from Lisp](dotnet-package.md) — the `dotnet:` package
- [Defining .NET classes](define-class.md) — emit a real .NET type from Lisp
- [Numbers across the boundary](numbers.md) — which .NET numeric types arrive
  as which Lisp types

## Examples

Scripts you can run as they stand, in `examples/`:

- [`http-json.lisp`](../examples/http-json.lisp) — resolve a NuGet package, await
  an async .NET method, read the JSON that comes back
- [`crypto.lisp`](../examples/crypto.lisp) — the class library with nothing to
  install: hashing, key derivation, authenticated encryption
- [`hello-gui.lisp`](../examples/hello-gui.lisp) — a cross-platform window
  (Avalonia), the file from the README
- [`windows/`](../examples/windows/) — COM, WMI, P/Invoke and WinForms

`samples/` holds the other direction: complete projects where a .NET host —
ASP.NET Core, MAUI, MonoGame — drives Lisp code.

## Platform and tooling

- [Windows](windows.md) — Windows-specific notes (COM / WMI / P/Invoke, UI)
- [Profiling](profiling.md) — CPU and allocation profiles; your Lisp function
  names appear in the stacks as-is
