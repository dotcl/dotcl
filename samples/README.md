# dotcl samples

Complete projects, each one a .NET application that runs Common Lisp: either a
C# host that embeds dotcl in-process, or a Lisp program compiled ahead of time
and shipped where no code can be generated at run time. Single-file scripts you
can run as they stand live in [`examples/`](../examples/) instead.

Every sample has its own `README.md` with the walk-through. The list below says
what each one shows and what has to be installed to build it.

## Embedding dotcl in a .NET host

- **[AspNetLispDemo](AspNetLispDemo/)**: an ASP.NET Core `ControllerBase`
  subtype defined in Lisp serves HTTP endpoints, with attribute routing and one
  async handler built on `dotcl:async` / `dotcl:await`.
  Needs .NET SDK 10+; runs on Windows, Linux and macOS.

- **[MauiLispDemo](MauiLispDemo/)**: a .NET MAUI app whose `Application`,
  `ContentPage` and view model are all emitted from Lisp with
  `dotnet:define-class`; the app also evaluates Lisp typed into its own editor.
  Needs .NET SDK 10+ plus the MAUI workload (`dotnet workload restore`), and
  Windows 10.0.19041 or later for the Windows target; the Android target is
  described in the sample's `ANDROID-SETUP.md`.

- **[MonoGameLispDemo](MonoGameLispDemo/)**: a MonoGame `Game` subclass in
  Lisp, whose `Draw` override runs on the frame loop and animates the
  background colour.
  Needs .NET SDK 10+ and Windows: the project targets `net10.0-windows` with
  the `win-x64` RID pinned, so the x64 .NET Desktop Runtime has to be
  installed (on an ARM64 machine it runs under x64 emulation).

- **[McpServerDemo](McpServerDemo/)**: dotcl exposed as a Model Context
  Protocol server: an MCP client calls the `lisp_eval` tool and the form is
  evaluated in the dotcl image inside the server process.
  Needs .NET SDK 10+, and an MCP client to drive it.

- **[HotReloadDemo](HotReloadDemo/)**: a console host reloads a `.lisp` file
  on every save while it keeps serving requests, so an edit takes effect on the
  next call with no restart and no forbidden-edit list.
  Needs .NET SDK 10+; runs on Windows, Linux and macOS.

## Writing platform components in Lisp

- **[AndroidServiceLispSpike](AndroidServiceLispSpike/)**: an Android
  `[Service]` and a launcher `[Activity]` written in Lisp and saved with
  `dotnet:library` to a .dll, which the Android build scans like any other
  assembly: the Java wrappers and dex entries come out the same way they do for
  C#. A spike that stops at the build output; running on a device is not
  covered yet.
  Needs .NET SDK 10+ with the `android` workload (the emitting side loads the
  runtime pack's `Mono.Android.dll`) and a JDK to build the consumer app.

## Shipping with no run-time code generation

All three precompile Lisp at build time, so they need `compiler/cil-out.sil`
built once from the repository root with `make cross-compile`.

- **[PrecompiledLispDemo](PrecompiledLispDemo/)**: a console host loads a
  precompiled Lisp image and calls between C# and Lisp in both directions with
  `DotclHost.PrecompiledOnly = true`, which forbids every form of run-time
  codegen. This is how to check that your Lisp will run on an AOT target,
  while still on ordinary CoreCLR.
  Needs .NET SDK 10+; runs on Windows, Linux and macOS.

- **[PrecompiledLispDemoAot](PrecompiledLispDemoAot/)**: the same idea inside
  a NativeAOT native executable, where `Reflection.Emit` is not merely unused
  but absent. It runs precompiled Lisp and still evaluates new definitions at
  run time through the tree-walk interpreter.
  Needs .NET SDK 10+ and the native toolchain `PublishAot` requires for your
  platform (MSVC build tools on Windows, clang and the usual build essentials
  on Linux and macOS).

- **[PrecompiledLispDemoWebGL](PrecompiledLispDemoWebGL/)**: the same runtime
  in a browser, as a Unity IL2CPP WebGL build: precompiled Lisp draws an
  animated curve frame by frame, and an input box on the page evaluates Lisp
  live to reshape it.
  Needs .NET SDK 10+ plus the Unity editor version recorded in
  `ProjectSettings/ProjectVersion.txt` with its WebGL module, and any static
  HTTP server to serve the result.
