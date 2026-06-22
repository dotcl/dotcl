# PrecompiledLispDemoAot

*Precompiled dotcl + a dynamic emit-free evaluator inside a **NativeAOT** native binary.*

This is the strongest proof of the **shippable runtime** goal (see
[`DESIGN.md`](../../DESIGN.md)). Linking the emit-free `netstandard2.0` runtime on
a plain CoreCLR host only proves the runtime doesn't *use* `Reflection.Emit` — it
*could* still exist there. This sample is **fully ahead-of-time compiled**
(`PublishAot`): in the produced native executable `IsDynamicCodeSupported` is
`False` and
`Assembly.LoadFrom` throws `PlatformNotSupportedException`. It proves dotcl
survives a platform that *forbids* codegen outright — the property an
AOT / IL2CPP / iOS / WebGL target actually imposes — while still running
precompiled Lisp **and** evaluating new code (`defun` / `defmacro` / CLOS) at run
time through the tree-walk interpreter, with no emitter present.

## The build-time-link trick

You cannot `Assembly.LoadFrom` a `.fasl` under NativeAOT. Instead the two Lisp
images are **referenced as fixed-name assemblies** and baked into the native
image by the AOT compiler, then booted by name:

- `dotclcore.dll` — the runtime core (compiler + stdlib), from the dev SIL core.
- `appfasl.dll` — this app's Lisp, from `app.lisp`.

Two things make this work:

1. **Stable assembly names.** The default `compile-file` / `sil-to-fasl` name
   carries a per-build guid (`app_1e23687b…`) that changes every build and can't
   be referenced. `precompile.lisp` pins stable names with
   `compile-file … :module-name "appfasl"` and the 3rd `sil-to-fasl` argument.
2. **File name == internal assembly name.** ILC resolves assemblies by *simple
   name* via the file name, so the outputs must be `appfasl.dll` / `dotclcore.dll`.

The host boots each by stable name through one runtime helper that uses
`Assembly.Load` on the already-linked assembly (never `Assembly.LoadFrom`):

```csharp
DotclHost.RunLinkedModuleByName("dotclcore");   // the FASL core
DotclHost.RunLinkedModuleByName("appfasl");     // the app image
```

`<TrimmerRootAssembly>` keeps both images (and `DotCL.Runtime`) whole so their
`CompiledModule.ModuleInit` survives trimming.

## How the project is wired

The csproj runs two targets before reference resolution, so a single
`dotnet publish` is self-contained:

1. **`BuildNs2Runtime`** builds the emit-free `netstandard2.0` `DotCL.Runtime.dll`
   (referenced by `HintPath`; a `ProjectReference` with `SetTargetFramework`
   would propagate the AOT setting → `NETSDK1207`).
2. **`PrecompileLisp`** runs the dev dotcl compiler (`runtime.csproj`, `net10`,
   which *does* have `Reflection.Emit` — the dev/build side) to emit the two
   stable-named Lisp IL assemblies.

The shipped native binary then never emits.

## Run

NativeAOT needs a C toolchain for the target platform: clang + zlib headers on
Linux, Xcode command line tools on macOS, or Visual Studio with the "Desktop
development with C++" workload on Windows (ILC finds MSVC via `vswhere` — no
manual `vcvarsall` needed when the workload is installed). `publish.sh` picks the
RID from the host and is the same on every OS:

```sh
./publish.sh            # Linux / macOS / Windows (git-bash)
RID=linux-arm64 ./publish.sh   # cross-RID override
# -> bin/<arch>/Release/net10.0/<rid>/publish/PrecompiledLispDemoAot(.exe)
```

It runs the plain .NET command underneath — `dotnet publish -r <rid> -c Release
-p:PublishAot=true` — so you can invoke that directly if you prefer.

Or, for a quick dev check on CoreCLR (no native toolchain, but still emit-free —
`PublishAot` puts `IsDynamicCodeSupported=False` in `runtimeconfig.json`):

```sh
dotnet run -c Release
```

(Both require `compiler/cil-out.sil` — build it once from the repo root with
`make cross-compile`.)

## Expected output

```
IsDynamicCodeSupported = False
Core booted (build-time linked, no Assembly.LoadFrom).
App fasl linked.

fib(20)          = 6765
sum-squares(100) = 328350
    [C# host-log] hello dotcl, from precompiled Lisp
greet len        = 5

Emit-free eval (IsDynamicCodeSupported=False):
  (+ 1 2)                       = 3
  (loop for i to 4 collect i)   = (0 1 2 3 4)
  (fib 10)  [calls compiled fn] = 55
  defun cube; (cube 5)          = 125
  defmacro twice; (twice ..)    = 54
  defclass/defmethod; (sq ..)   = 49

Done — precompiled Lisp + emit-free eval (defun/defmacro/CLOS) in a NativeAOT binary, no Reflection.Emit.
```
