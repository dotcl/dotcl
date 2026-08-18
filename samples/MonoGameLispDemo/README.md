# MonoGameLispDemo — a recipe for embedding dotcl in a C# project (MonoGame edition)

A sample that demonstrates the boilerplate for **embedding dotcl as an
in-process runtime in an existing .NET project**, using a MonoGame DesktopGL
app. It is meant to let a reader who knows Common Lisp apply the same pattern to
their own C# game project.

Key points:

- The C# side is **just boot and `Game.Run()`**. The `Game` subclass
  (`Demo.LispGame`) is emitted on the Lisp side with `dotnet:define-class`.
- In its ctor it creates a `GraphicsDeviceManager(this)`, overrides `Draw`, and
  writes the background color every frame with `GraphicsDevice.Clear(...)`.
- The background color comes from `(pulse-color seconds)`, gradating over time.

## When you launch the demo

A window opens and the background gradates red → green → blue → red on a 6-second
cycle. The `Game.Update` / `Game.Draw` loop runs on the MonoGame side, and only
the Lisp code inside the `Draw` override (calling `pulse-color` to build a
`Color`) is evaluated each frame.

## Layout

```
MonoGameLispDemo/
├── MonoGameLispDemo.csproj   # net10.0-windows / win-x64 / DesktopGL
├── MonoGameLispDemo.asd      # ASDF definition: depends-on dotnet-class
├── main.lisp                 # emits Demo.LispGame via define-class
├── Program.cs                # boot + Run() only
└── CsharpSanityGame.cs       # environment check (launch with --csharp-sanity)
```

The `<Import Project=".../Dotcl.targets" />` in `MonoGameLispDemo.csproj`
compile-files `main.lisp` at build time and places it under
`bin/.../dotcl-fasl/` (project-core flow). At run time
`DotclHost.LoadFromManifest` reads the manifest and loads everything together.

## Why DesktopGL / win-x64

- **DesktopGL (SDL2 + OpenGL)** is more portable, including ARM64. WindowsDX
  (SharpDX) was confirmed to render nothing on Snapdragon Windows ARM64.
- **`<RuntimeIdentifier>win-x64</RuntimeIdentifier>`** is pinned, which runs the
  sample under Prism (x64 emulation) on an ARM64 machine. As a dev sample the
  performance penalty is acceptable. The pin dates from when
  `MonoGame.Library.SDL` had no win-arm64 native; it ships one now (resolving the
  package for `win-arm64` lays out an SDL2 binary distinct from the x64 one), so
  the pin is probably removable — nobody has re-tested the sample natively on
  ARM64 since.

## Environment check

If rendering is all black, use this to tell whether the Lisp integration or the
MonoGame environment is at fault:

```
MonoGameLispDemo.exe --csharp-sanity
```

It launches the pure-C# `CsharpSanityGame` (a solid red Clear). If that is also
black, suspect MonoGame / the GPU driver; if only that works, suspect the dotcl
integration.

## Run

```bash
dotnet build MonoGameLispDemo.csproj -c Debug
./bin/Debug/net10.0-windows/win-x64/MonoGameLispDemo.exe
```

With the `net10.0-windows` target + `win-x64` RID, the x64 **.NET Desktop
Runtime** must be present under `C:\Program Files\dotnet\x64\` (not the ASP.NET
Core Runtime).
