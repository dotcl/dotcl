# PrecompiledLispDemo

*Precompiled, emit-less dotcl embedding.* A plain .NET console app that embeds
dotcl, loads a **precompiled** Lisp image and runs it **without generating any
code at run time**, calling between C# and Lisp in both directions.

Here **"precompiled" / "emit-less" means: no runtime code generation** (no
`Reflection.Emit`, no JIT of Lisp). It does *not* refer to static linking. This
is the same constraint an AOT / IL2CPP target imposes, so this sample is how you
validate that dotcl logic will run on those targets — on plain CoreCLR, today.

## How it works

1. **Build time** (`precompile.lisp`): the `PrecompileLisp` MSBuild target runs
   the dotcl compiler to produce two precompiled .NET IL assemblies —
   `app.fasl` (this app's Lisp, from `app.lisp`) and `dotcl.core` (the runtime
   core, converted from the dev SIL core via `dotcl:sil-to-fasl`). The *compiler*
   uses `Reflection.Emit` — that is the dev/build side, not the shipped side.
2. **Run time** (`Program.cs`): the host
   - `DotclHost.Initialize()` + `LoadCore("dotcl.core")` — boot the runtime; a
     FASL core loads via ModuleInit, so even **boot generates no code**,
   - `DotclHost.PrecompiledOnly = true` — **forbid all runtime codegen** from here,
   - `DotclHost.Register("host-log", ...)` — expose a C# function to Lisp,
   - `DotclHost.LoadLispFile("app.fasl")` — load the precompiled image (no codegen),
   - `DotclHost.Call("FIB", 20)` etc. — call Lisp from C#,
   - `greet` calls back into `host-log` — call C# from Lisp,
   - and a final `EvalString("(+ 1 2)")` shows that runtime `eval` is now blocked.

   Everything after boot — registration, loading the app, and running it — happens
   under `PrecompiledOnly`, so the whole app lifecycle is provably emit-less.

`app.lisp`'s source is never read or `eval`'d by the running app; only the
precompiled `app.fasl` is loaded.

## Run

```sh
dotnet run
```

(Requires `compiler/cil-out.sil` to exist — build it once from the repo root with
`make cross-compile`.)

## Expected output

```
PrecompiledOnly = true (runtime code generation is now forbidden)

fib(20)          = 6765
sum-squares(100) = 328350
    [C# host-log] hello dotcl, from precompiled Lisp
greet returned name length = 5

eval blocked as expected: precompiled-only mode (dotcl:precompiled-only): ...

Done — ran precompiled Lisp with no runtime code generation.
```

## Notes

- The dev SIL core (`cil-out.sil`) is used only at *build* time, to run the
  compiler; the *app* boots from the FASL core (`dotcl.core`), generated during
  the build, which loads without emitting.
- This proves the contract on plain CoreCLR. Actual AOT validation (NativeAOT,
  eventually IL2CPP) additionally needs the core/emitter split, so the runtime
  can build with no `Reflection.Emit` reference at all; this sample is the
  behavioural proof that gets you there.
