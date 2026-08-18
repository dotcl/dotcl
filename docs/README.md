# dotcl documentation

Guides and reference for dotcl — Common Lisp on .NET. Start with the
[five-minute tutorial](../README.md) in the top-level README; the documents
below go deeper on one topic each.

## Using dotcl

- [Using libraries](libraries.md) — ASDF, Quicklisp, and pulling in .NET /
  NuGet dependencies
- [Packaging an app](dotcl-pack.md) — turn an ASDF system into a dotnet tool

## Interop with .NET

- [Calling .NET from Lisp](dotnet-package.md) — the `dotnet:` package
- [Defining .NET classes](define-class.md) — emit a real .NET type from Lisp
- [Numbers across the boundary](numbers.md) — which .NET numeric types arrive
  as which Lisp types

## Platform and tooling

- [Windows](windows.md) — Windows-specific notes (COM / WMI / P/Invoke, UI)
- [Profiling](profiling.md) — CPU and allocation profiles; your Lisp function
  names appear in the stacks as-is
