# PluginHostDemo

*A plain C# plugin host, with a plugin written in Common Lisp.* The host defines
an interface, loads every plugin DLL it finds in a folder through
`AssemblyLoadContext`, and calls them. It is ordinary C# and knows nothing about
dotcl. One plugin is C#; two are Common Lisp, built by `dotnet build` into DLLs
that implement the host's interface: one from a single Lisp file, one from an
ASDF system that depends on another system.

A Lisp plugin is loaded like any other assembly: no `.lisp` source and no
dotcl core are read when it runs, and the host does nothing to start Lisp. The
DLL boots the runtime itself the first time one of its types is touched.

## Layout

```
PluginHost.Abstractions/   IPlugin { string Name; string Run(string input, Action<string> progress); event Action<string> Completed; }  (netstandard2.0)
PluginHost/                the host: scans plugins/<Name>/<Name>.dll, calls each IPlugin;
                           its build copies the plugins into bin/.../plugins/
CSharpPlugin/              a plugin in C# (upper-cases its input)
LispPlugin/                a plugin in Common Lisp (reverses each word, counts words)
  LispPlugin.csproj
  plugin.lisp
WordStatsPlugin/           a plugin made from the ASDF system word-stats
  WordStatsPlugin.csproj
  word-stats.asd           package.lisp, stats.lisp, and plugin.lisp as (:dotnet-library ...)
text-util/                 a plain Lisp library (system text-util) that word-stats depends on
```

## How the Lisp plugin is made

`LispPlugin.csproj` is the whole project:

```xml
<Project Sdk="DotCL.Sdk">
  <ItemGroup>
    <LispLibrary Include="plugin.lisp" />
    <ProjectReference Include="../PluginHost.Abstractions/PluginHost.Abstractions.csproj" />
  </ItemGroup>
</Project>
```

The build loads the project's references, so `plugin.lisp` can name
`PluginHost.IPlugin`, then loads `plugin.lisp`, whose `dotnet:library` form
becomes the project's assembly:

```lisp
(dotnet:library ("LispPlugin" :version "1.0.0.0")
  (:defun split-words (s) ...)
  (:class "LispPlugin.ReversePlugin" ()
    (:implements "PluginHost.IPlugin")
    (:properties ("Name" String))
    (:events ("Completed" "System.Action`1[System.String]"))
    (:ctor () (dotnet:%set-invoke self "Name" "reverse words (Common Lisp)"))
    (:methods
      (run ((input String) (progress "System.Action`1[System.String]")) :returns String ...))))
```

Method bodies named by a symbol are compiled into the DLL. The host's `Action<string>`
arrives in the Lisp body as a function (`(funcall progress w)`), and the `:events` clause
gives the class a `Completed` event that the body raises with `(dotnet:invoke self
"OnCompleted" result)`; the host subscribes with `+=` like it would to any C# event.
The DLL calls into `DotCL.Runtime` directly; the SDK makes it a package dependency,
so `DotCL.Runtime.dll` is copied beside the DLL in the plugin's own folder and the
host never references it. Details are in
[Producing a .dll for C# to reference](../../docs/define-class.md) and
[SDK and MSBuild properties](../../docs/sdk.md#lisp-libraries).

## A plugin made from an ASDF system

`WordStatsPlugin` is the same kind of plugin with its code in an ASDF system,
`word-stats`, which depends on another system, `text-util` (plain Common Lisp
with no .NET in it). The project names the system and where the dependency is:

```xml
<Project Sdk="DotCL.Sdk">
  <ItemGroup>
    <LispSystem Include="word-stats.asd" />
    <DotclAsdSearchPath Include="../text-util/" />
    <ProjectReference Include="../PluginHost.Abstractions/PluginHost.Abstractions.csproj" />
  </ItemGroup>
</Project>
```

`word-stats.asd` names `plugin.lisp`, the file with the `dotnet:library` form, as
a `(:dotnet-library ...)` component and `dotnet-library-op` as the system's build
operation:

```lisp
(defsystem "word-stats"
  :defsystem-depends-on ("dotnet-library")
  :build-operation "dotnet-library-op"
  :depends-on ("text-util")
  :serial t
  :components ((:file "package")
               (:file "stats")
               (:dotnet-library "plugin")))
```

The method body calls `split-words` from `text-util` and `summary` from
`stats.lisp`. The build compiles those into the DLL from their source, with what
they call in turn (`title`, `longest`, `capitalize`); nothing else of the two
systems goes in. Without MSBuild the same DLL comes from `(asdf:make
"word-stats")` in a dotcl where both systems are visible to ASDF (it then goes to
the form's `:path`, or the system's `:build-pathname`), or from `dotcl
build-library --system word-stats.asd --search-path ../text-util ...`.

## Run

Needs .NET SDK 10+.

```sh
cd samples/PluginHostDemo
dotnet build PluginHost
dotnet run --project PluginHost
```

`DotCL.Sdk` comes from NuGet. Outside a dotcl checkout, name its version in a
`global.json` next to the projects (a release after 0.1.35):

```json
{ "msbuild-sdks": { "DotCL.Sdk": "VERSION" } }
```

and delete `Directory.Build.targets`.

### Inside a dotcl checkout

`Directory.Build.targets` builds the Lisp plugin against the tree instead of the
`DotCL.Runtime` package: a `ProjectReference` to `runtime/DotCL.Runtime.csproj`,
and the core and build tool from the tree. Once, and again after changing
`runtime/sdk/`, run from the repository root:

```sh
make compile-core-fasl
sh runtime/sdk/dev-feed.sh . samples/PluginHostDemo
```

`dev-feed.sh` packs the tree's `DotCL.Sdk` onto `out/dev-feed` and writes a
`global.json` and `nuget.config` here that point at it.

## Expected output

```
input: "hello plugin world"
  progress: upper-casing
  completed: HELLO PLUGIN WORLD
[CSharpPlugin.dll] upper (C#): HELLO PLUGIN WORLD
  progress: hello
  progress: plugin
  progress: world
  completed: olleh nigulp dlrow [3 words]
[LispPlugin.dll] reverse words (Common Lisp): olleh nigulp dlrow [3 words]
  progress: 3 words
  completed: 3 words, longest "plugin", Hello Plugin World
[WordStatsPlugin.dll] word stats (Common Lisp, ASDF system): 3 words, longest "plugin", Hello Plugin World
DotCL.Runtime is loaded in a plugin context; DotclHost.CoreLoaded = False
DotCL.Runtime is loaded in a plugin context; DotclHost.CoreLoaded = False
```

The last lines are a diagnostic the host prints by reflection, one per Lisp
plugin: each runs with `DotclHost.CoreLoaded` false, that is, with no core loaded.

`make test-plugin-host-demo` from the repository root builds the sample and
checks this output.
