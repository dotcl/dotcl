# dotcl

Common Lisp implementation on .NET. Lisp source is compiled to CIL
(Common Intermediate Language) and runs on the .NET JIT — so the same
Lisp image runs on Windows, macOS, and Linux across x86-64 and ARM64
without per-platform porting work.

**Broadly conforms to the ANSI Common Lisp standard** — verified
against the
[ansi-test suite](https://gitlab.common-lisp.net/ansi-test/ansi-test).

## What dotcl is good for

- **Embedding Common Lisp in .NET applications.** `dotcl.runtime` is a
  regular .NET library; you load it from any C# / F# / VB.NET project,
  evaluate Lisp code, and call back and forth.
- **Writing .NET code in Lisp.** The `dotnet:` package gives direct
  access to .NET types: `(dotnet:new "System.Text.StringBuilder")`,
  `(dotnet:invoke sb "Append" "x")`, `(dotnet:static "System.Math" "Sin"
  1.0)`. You can subclass .NET types from Lisp via `dotnet:define-class`
  — the compiler emits real .NET classes, so frameworks like MAUI,
  ASP.NET Core, and MonoGame just see them as ordinary subclasses.
- **Cross-platform CL with NuGet ecosystem access.** Any NuGet package
  is reachable from Lisp; any Quicklisp library that doesn't rely on
  SBCL-only internals tends to work too (asdf, alexandria, etc. are
  routinely loaded).

## Quick start

```bash
# Install dotcl as a global .NET tool (works on any host with .NET SDK 10+).
dotnet tool install --global dotcl

# REPL
dotcl repl

# Evaluate a form
dotcl --eval "(format t \"hello, ~a~%\" (lisp-implementation-type))"

# Run a file
dotcl --load my-program.lisp
```

The framework-dependent `dotcl` package is portable across OS / arch but
JIT-compiles the core on first launch (~3 s cold start). For faster
startup, install the RID-specific package — it bundles ahead-of-time
(R2R) FASLs:

```bash
# Pick the one matching your host:
dotnet tool install --global dotcl.win-x64
dotnet tool install --global dotcl.win-arm64
dotnet tool install --global dotcl.linux-x64
dotnet tool install --global dotcl.linux-arm64
dotnet tool install --global dotcl.osx-x64
dotnet tool install --global dotcl.osx-arm64
```

The two variants share the `dotcl` command name, so install only one.

For Roswell users, per-RID tarballs are also published on each
[release page](https://github.com/dotcl/dotcl/releases).

### Prerequisites

- **.NET SDK 10+** — see install table below

#### Installing .NET SDK 10

| OS | Command |
|----|---------|
| macOS (Homebrew) | `brew install --cask dotnet-sdk` |
| Ubuntu 24.04+ | `sudo apt install dotnet-sdk-10.0` |
| Debian | add the Microsoft package repository, then `apt install dotnet-sdk-10.0` — see [official guide](https://learn.microsoft.com/dotnet/core/install/linux-debian) |
| Windows (winget) | `winget install Microsoft.DotNet.SDK.10` |
| Windows (Scoop) | `scoop install dotnet-sdk` |
| Cross-platform script | [`dotnet-install.sh` / `dotnet-install.ps1`](https://learn.microsoft.com/dotnet/core/tools/dotnet-install-script) |
| Other | https://dotnet.microsoft.com/download |

### Building from source

If you want to hack on dotcl itself rather than just use it, clone the
repo and bootstrap with [Roswell](https://github.com/roswell/roswell):

```bash
make cross-compile   # uses Roswell/SBCL to bootstrap the compiler
make install         # builds and installs the local nupkg as `dotcl`
```

After the first cross-compile, dotcl can self-host: `DOTCL_LISP=dotcl
make cross-compile` rebuilds the compiler using dotcl itself.

## Samples

Working integrations in `samples/`:

- **MauiLispDemo** — a .NET MAUI app (Windows + Android) where
  `Application` / `ContentPage` / view model are all defined in Lisp
  via `dotnet:define-class`.
- **AspNetLispDemo** — ASP.NET Core controller written in Lisp, with
  attribute routing.
- **MonoGameLispDemo** — `Game` subclass in Lisp; the `Draw` override
  runs on the MonoGame frame loop and animates the background colour.
- **McpServerDemo** — Model Context Protocol server exposing a Lisp
  REPL to MCP clients (Claude Desktop, etc.).

Each sample's `README.md` walks through the boot pattern.

## Architecture

- **Compiler** (`compiler/`, written in Lisp): transforms S-expressions
  into a flat list of CIL instructions (SIL).
- **Runtime** (`runtime/`, written in C#): object representation,
  reader, CIL assembler (`PersistedAssemblyBuilder`-based for `.fasl`
  output and `Reflection.Emit` for in-memory codegen), and the standard
  library functions that aren't expressible in pure Lisp.
- **Bootstrap** is by cross-compile: a Roswell SBCL runs
  `compiler/cil-compile.lisp` to emit `compiler/cil-out.sil`, which the
  .NET runtime loads to bring up the Lisp environment. From that point
  dotcl can rebuild itself.

Architectural detail and design history are in
[`DESIGN.md`](DESIGN.md).

## Platform notes

- **Windows**: see [`docs/windows.md`](docs/windows.md) for
  installation, encoding (UTF-8 stdin/stdout always), pathname
  conventions, and Windows-side .NET interop (Registry / WMI / WinForms
  / MAUI / COM).

## License

MIT. See [`LICENSE`](LICENSE).
