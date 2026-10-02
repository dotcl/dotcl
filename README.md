# dotcl

Common Lisp for .NET applications. Embed it in a C# program to evaluate Lisp,
call Lisp functions, and let Lisp call back into your code. Lisp values are
.NET objects and Lisp code calls any .NET API directly, so there is no
marshalling layer between the two.

```sh
dotnet add package DotCL.Runtime
```

## Quick start

```csharp
using DotCL;

DotclHost.EnsureCore();

// Evaluate Lisp and take the result back as a .NET value.
var answer = DotclHost.EvalString("(+ 40 2)");
Console.WriteLine(DotclHost.ToClr<int>(answer));                      // 42

// Let Lisp call into C#.
DotclHost.Register("greet", args => $"Hello, {args[0]}!");
DotclHost.EvalString("(print (greet \"Lisp\"))");                     // "Hello, Lisp!"

// Define a function in Lisp and call it from C#.
DotclHost.EvalString("(defun fact (n) (if (< n 2) 1 (* n (fact (1- n)))))");
Console.WriteLine(DotclHost.ToClr<long>(DotclHost.Call("fact", 20))); // 2432902008176640000
```

[Embedding dotcl](docs/embedding.md) covers the host API: loading code,
passing values both ways, errors, output, and running where no code may be
generated at run time.

## Features

- **The whole .NET library from Lisp.** `(dotnet:new "System.Text.StringBuilder")`,
  `(dotnet:invoke sb "Append" "x")`, `(dotnet:static "System.Math" "Sin" 1.0)`;
  NuGet packages load at run time. See [Calling .NET from Lisp](docs/dotnet-package.md).
- **Real .NET classes from Lisp.** `dotnet:define-class` emits an ordinary .NET
  type, so frameworks such as ASP.NET Core, MAUI, Avalonia and MonoGame see a
  plain subclass. See [Defining .NET classes](docs/define-class.md).
- **Runs where code generation is not allowed.** Lisp compiled ahead of time
  runs under NativeAOT and Unity IL2CPP, including a WebGL build. See the
  `Precompiled*` [samples](samples/).
- **Common Lisp, not a subset.** The
  [ansi-test](https://gitlab.common-lisp.net/ansi-test/ansi-test) suite passes
  except for one deliberate [deviation](docs/deviations.md), with the few
  tests that ansi-test itself marks as specification problems left out. Most
  Quicklisp libraries load, some of them with patches from the dotcl dist;
  [library status](docs/library-status.md) is the measured list.
- **Edit while it runs.** Redefine a function and the next call uses the new
  definition; a host can reload a file without restarting.
  See [HotReloadDemo](samples/HotReloadDemo/).
- One package for Windows, macOS and Linux on x64 and ARM64, with builds for
  .NET 10, .NET 8 and `netstandard2.0` (Unity).

How fast, measured against C# and SBCL: [Benchmarks](docs/benchmarks.md).

## Before you embed it

dotcl runs in your process with the same rights as your code, and Lisp code can
reach any .NET API. It is not a sandbox: do not run code you do not trust. The
runtime is one image per process, shared by every thread that calls into it.
[Hosting dotcl safely](docs/hosting-safely.md) says what that means for threads
and for untrusted input.

## Using dotcl as a Lisp

dotcl is also a Common Lisp you can use on its own, with a REPL, scripts and
Quicklisp:

```sh
dotnet tool install --global dotcl
dotcl repl
dotcl --load my-program.lisp
```

[dotcl for Lisp programmers](docs/for-lispers.md) starts there: libraries,
editors, the REPL, and shipping what you write as a .NET tool or an executable.
To try it without installing anything, the
[playground](https://dotcl.github.io/playground/) runs dotcl in your browser.

## Showcase

- **[paalam](https://github.com/dotcl/paalam)**: an image / comic / PDF viewer
  (Avalonia). UI and application logic are Common Lisp end to end.
- **[playa](https://github.com/dotcl/playa)**: an Emacs-style split-tiling
  video / music player (Avalonia + LibVLCSharp), Lisp compiled ahead of time by
  the project's own `dotnet build`.

## Samples

Complete projects in [`samples/`](samples/), each with its own README:

- **[AspNetLispDemo](samples/AspNetLispDemo/)**: an ASP.NET Core controller written in Lisp.
- **[MauiLispDemo](samples/MauiLispDemo/)**: a .NET MAUI app (Windows + Android) defined in Lisp.
- **[MonoGameLispDemo](samples/MonoGameLispDemo/)**: a MonoGame `Game` subclass in Lisp.
- **[HotReloadDemo](samples/HotReloadDemo/)**: a host that reloads a Lisp file on every save.
- **[McpServerDemo](samples/McpServerDemo/)**: a Model Context Protocol server exposing a Lisp REPL.
- **[PrecompiledLispDemo](samples/PrecompiledLispDemo/)**,
  **[...Aot](samples/PrecompiledLispDemoAot/)**,
  **[...WebGL](samples/PrecompiledLispDemoWebGL/)**: precompiled Lisp with no
  run-time code generation, on CoreCLR, NativeAOT and Unity IL2CPP WebGL.

## Documentation

[The documentation index](docs/README.md). Building dotcl from source is in
[Getting started](docs/getting-started.md#2c-from-source); the architecture and
design history are in [`DESIGN.md`](DESIGN.md).

## License

MIT. See [`LICENSE`](LICENSE).
