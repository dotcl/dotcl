# SDK and MSBuild properties

What an MSBuild build reads when it compiles Lisp: every property and item
group a project can set, what each one defaults to, and which of the three
ways of wiring a project you are in.

New to this? Start with [Getting started](getting-started.md); this page is
the reference behind it.

## The three wirings

All three end in the same place: an ASDF system compiled to a FASL, its
dependencies bundled beside it, and a manifest the host loads at startup.

| Wiring | The project says | Compiles via | Use it for |
|--------|------------------|--------------|------------|
| Package reference | `<PackageReference Include="DotCL.Runtime">` plus `<DotclProjectAsd>` | in-process MSBuild tasks from the package | the default; what `dotnet new dotcl-app` writes |
| MSBuild SDK | `<Project Sdk="DotCL.Sdk/VERSION">` | same tasks (the SDK adds the package reference for you) | the shortest csproj |
| Repo import | `<Import Project=".../runtime/build/Dotcl.targets" />` | the `dotcl` CLI, or `dotnet run` against the runtime csproj | working inside a dotcl checkout |

The SDK form is the whole project file. The version after the slash is the
dotcl release to build against, the same number as the `DotCL.Runtime`
package; 0.1.29 below is an example, not a requirement:

```xml
<Project Sdk="DotCL.Sdk/0.1.29">
  <PropertyGroup>
    <DotclProjectAsd>$(MSBuildProjectDirectory)/app.asd</DotclProjectAsd>
  </PropertyGroup>
</Project>
```

It sets `OutputType=Exe`, `TargetFramework=net10.0`, `ImplicitUsings=enable`
and `Nullable=enable` as defaults you can override, then injects the
`DotCL.Runtime` package reference. Do not add that reference yourself unless
you also set `DisableImplicitDotCLRuntimeReference`: the SDK owns it.

The repo import is a different implementation, not a different spelling: it
shells out to the dotcl CLI. Importing the *package's* targets by path out of
a checkout does not work, because the task assembly is resolved relative to
the package directory; the build fails with `MSB4036: The "DotclResolveDeps"
task was not found`. Inside a checkout, import `Dotcl.targets` (below); from
outside, use a package reference.

## Properties

### The switch

| Property | Default | Meaning |
|----------|---------|---------|
| `DotclProjectAsd` | *(empty)* | Path to the `.asd` to build. **Everything below is inert until this is set** -- a project that only embeds the runtime is untouched by these targets. |

### Inputs you set

| Property / item | Default | Meaning |
|-----------------|---------|---------|
| `DotclAsdSearchPath` (item) | *(empty)* | Directories to search for dependency systems. Each is pushed onto `asdf:*central-registry*` before dependency resolution. One item per directory; a trailing slash is fine. |
| `DotclBuildInit` (item) | *(empty)* | Lisp files evaluated at build time before resolution. The escape hatch for anything a directory list cannot express (booting Quicklisp, computed paths). Build time only: nothing here reaches the shipped app. |
| `DotclBaseCore` | the `dotcl.core` shipped in the package | The base image the build compiles against and copies into the output. Point it elsewhere to build against a core of your own. |
| `DotclDebugInfo` | `true` when `Configuration=Debug`, else `false` | Emit a Portable PDB next to each FASL so a debugger binds breakpoints in the `.lisp` source. |
| `DotclTrimmerRoots` | `true` | Hand the trimmer a descriptor for the .NET types your Lisp sources name, so a trimmed publish keeps them. See [Trimmed publish](#trimmed-publish). `false` turns it off. |
| `DotclReadyToRunFasls` | *(unset)* | On a `PublishReadyToRun` publish, compile the published FASLs to native code as well. See [ReadyToRun publish](#readytorun-publish). Unset: a FASL crossgen2 cannot compile is published as IL with a warning. `true`: that fails the publish. `false`: FASLs stay IL. Unlike the rest of this table it does not need `DotclProjectAsd`: it also covers the `dotcl.core` and `contrib/` a plain embedder publishes. |
| `DotclPublishAllContribs` | *(unset)* | `true` publishes every contrib module, as before. Unset, a publish carries only the contrib modules your systems depend on. See [Contrib modules in a publish](#contrib-modules-in-a-publish). |
| `DotCLRuntimeVersion` | the version the SDK shipped with | *(SDK wiring only)* Which `DotCL.Runtime` package version the SDK injects. |
| `DisableImplicitDotCLRuntimeReference` | *(unset)* | *(SDK wiring only)* `true` stops the SDK injecting the package reference, so you can pin the runtime yourself. |
| `DotclTool` | `dotcl` | *(repo import only)* The CLI to invoke. |
| `DotclRuntimeProject` | *(empty)* | *(repo import only)* Path to `runtime/runtime.csproj`, invoked with `dotnet run`. Set this **or** `DotclTool`, not both. |

### Outputs the build computes

Set by the targets. Read them if you need to find an artifact; overriding them
is not supported.

| Property | Value |
|----------|-------|
| `DotclBundleDir` | `$(IntermediateOutputPath)dotcl-fasl/` -- where FASLs are staged before they are copied to the output |
| `DotclRootFasl` | `$(DotclBundleDir)$(MSBuildProjectName).fasl` -- your system, compiled |
| `DotclDeployedManifest` | `$(DotclBundleDir)dotcl-deps.txt` -- the load order: base core, dependency FASLs, then your FASL |
| `DotclProjectManifest` | `$(DotclBundleDir)$(MSBuildProjectName).deps.txt` -- the same list under a per-project name, so a referenced Lisp library has a manifest it can name for itself (only one file can be called `dotcl-deps.txt` in a shared output directory) |
| `DotclTrimDescriptor` | `$(DotclBundleDir)$(MSBuildProjectName).trim.xml` -- the trimmer descriptor written by the compile of your system. Build input only; it is not copied to the output |

Everything in the bundle directory is added to the build as `None` items
linked under `dotcl-fasl/` (on Android/MAUI, as `MauiAsset` and
`AndroidAsset`), so it lands next to your assembly and travels with
`dotnet publish`.

### Hooking the build

The pipeline's own targets are named with a leading underscore and carry no
stability promise. `DotclBuild` is the public anchor:

```xml
<Target Name="MyPostStep" AfterTargets="DotclBuild"> ... </Target>
```

It runs once per build, after the FASL and the manifest exist, and only when
`DotclProjectAsd` is set. It is intentionally empty -- it exists to be
scheduled around.

## Trimmed publish

A FASL is loaded at run time and reaches .NET only through reflection, so the
trimmer (`PublishTrimmed`, `PublishAot`) never sees what it uses and removes it.
The runtime assembly protects itself. For the types your code names, the
compile of your system writes `DotclTrimDescriptor`, and the build hands it to
the trimmer as a `TrimmerRootDescriptor`.

A type is picked up when its name is a literal string in a type position:

```lisp
(dotnet:new "System.Text.StringBuilder")
(dotnet:static "System.Globalization.ISOWeek" "GetYear" date)
(dotnet:make-generic-type "System.Collections.Generic.List" (list "MyApp.Item"))
```

The same goes for `dotnet:new-array`, `dotnet:make-array`, `dotnet:make-delegate`,
`dotnet:resolve-type`, `dotnet:members`, `dotnet:class-for-type`, `dotnet:enum-or`,
`dotnet:cast`, `dotnet:box`, `dotnet:is-instance-of`, `dotnet:exception-typep`
and the type arguments of `dotnet:static-generic`, `dotnet:invoke-generic` and
`dotnet:call-out-generic`. Each named type is kept whole (`preserve="all"`).

The name is resolved at build time, against the framework and the project's
references. A name that does not resolve there is taken to be a type of the
project being built, which is not compiled yet, and is rooted in
`$(AssemblyName)`; if it is not there either, the trimmer warns about it.

Not covered, and rooted by hand with your own `TrimmerRootDescriptor` or
`TrimmerRootAssembly`:

- a type whose name is built at run time, or held in a variable;
- a type reached only as the value of another call. In
  `(dotnet:invoke (dotnet:static "System.IO.File" "OpenText" p) "ReadLine")`,
  `File` is kept but `StreamReader.ReadLine` is not, unless something else
  keeps it;
- the dependency FASLs. Only your own system's sources are looked at.

## ReadyToRun publish

`PublishReadyToRun` compiles the application's DLLs to native code, but a FASL
is only a file to copy as far as the SDK is concerned. The targets fill that
gap: on a publish with `PublishReadyToRun=true` and a runtime identifier, every
FASL being published -- `dotcl.core`, the FASLs under `dotcl-fasl/`, and the
`contrib/` tree -- goes through the same crossgen2 the SDK restored for the
DLLs, and the native image is published under the FASL's own name.

```
dotnet publish -c Release -r win-x64 -p:PublishReadyToRun=true
```

The image replaces the FASL rather than sitting next to it as a sibling (see
[Ahead-of-time compiled FASLs](readytorun.md)): the publish output belongs to
one runtime identifier already, and a FASL replaced in place is the only form
whose first load from a fresh copy reads no IL at all. On Windows most of the
gain is in that first run.

Measured on a small application that loads the core, two FASLs and one
contrib, the whole process from start to exit:

| | FASLs as IL | FASLs ReadyToRun |
|---|---|---|
| macOS arm64, second run onwards | 335-339 ms | 174-180 ms |
| Windows x64, second run onwards | 557-603 ms | 323-369 ms |
| Windows x64, first run of a fresh copy | 1234-1944 ms | 439-481 ms |

The cost is size: a native FASL is about three times its IL form (that
application's publish grew from 17 to 30-35 MB).

`dotnet build` and a publish without `PublishReadyToRun` leave the FASLs IL. A
`PublishReadyToRun` publish with no `-r` gets the SDK's own runtime identifier,
as the DLLs do, and is compiled too. A FASL named in `PublishReadyToRunExclude`
is left IL, as a DLL named there is. Each FASL compiled is kept under
`$(IntermediateOutputPath)dotcl-r2r/`, so a publish with nothing changed does
not run crossgen2 again.

## Contrib modules in a publish

The package copies its whole `contrib/` tree into the build output, so
`(require "x")` works for any bundled module while you develop. A publish of a
project with `DotclProjectAsd` keeps only the modules the build's manifest
requires: the contrib systems your systems (and their dependencies) name in
`:depends-on`, and the modules those require in turn. The rest are left out,
the way a static link leaves out what nothing declared.

**If the application REQUIREs a contrib module at run time, say so in an
`.asd`** (`:depends-on ("dotcl-socket")` in your system), **or publish with
`DotclPublishAllContribs=true`.** A `(require "x")` evaluated at run time is not
seen by the build, and in the published application it fails with the module
not found.

The selection is made before the [ReadyToRun](#readytorun-publish) step, so
only the modules that are published are compiled. For an application that
depends on `dotnet-class` and `dotcl-thread`, the macOS arm64 ReadyToRun
publish went from 48.8 to 32.2 MB (its `contrib/` from 17 MB to 0.4 MB). A project without
`DotclProjectAsd` (an application that only embeds the runtime) has no manifest
and publishes the whole tree, as before. `dotnet build` output is not
narrowed.

## Adding a dependency: which knob

Three of these look interchangeable and are not.

- **`DotclProjectAsd`** names *your* system. It is the entry point, not a
  search path.
- **`DotclAsdSearchPath`** is how a build finds systems that are not yours and
  not bundled with dotcl. This is the declarative, supported route.
- **`DotclBuildInit`** runs Lisp instead. Reach for it only when a directory
  list cannot say what you mean.

Systems bundled with dotcl (contrib) need none of these: they are found
because the build hands the packaged contrib directory to the compiler.

### `CL_SOURCE_REGISTRY` is not read by the build

Measured, both directions, with a dependency reachable only through one of the
two mechanisms:

| Route | `CL_SOURCE_REGISTRY` | `DotclAsdSearchPath` | Result |
|-------|----------------------|----------------------|--------|
| CLI (`dotcl ...`) | set | -- | loads |
| CLI | unset | -- | `MISSING-COMPONENT` |
| MSBuild build | set | unset | **fails** |
| MSBuild build | unset | set | loads |

The spelling of the variable was identical in all four runs, and the CLI rows
prove the value was well-formed. So "it loads from the command line but not
from `dotnet build`" is expected, not a misconfiguration: the build needs a
`DotclAsdSearchPath` item.

### When a dependency is missing, the error names the wrong layer

A system the build cannot find does not fail in dependency resolution. That
step passes silently, and the failure surfaces in the compile step as a
reader error:

```
error : dotcl compile-project failed for .../app.asd: Package "CSREXT" not found
```

The cause is a system that was never found; the symptom is an undefined
package. If you see this, check the search path before you go looking for a
missing `defpackage`.
