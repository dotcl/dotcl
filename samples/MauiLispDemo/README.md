# MauiLispDemo — a recipe for embedding dotcl in a C# project (MAUI edition)

A sample that demonstrates the boilerplate for **embedding dotcl as an
in-process runtime in an existing .NET project**, using a MAUI Windows / Android
app. It is meant to let a reader who knows Common Lisp apply the same pattern to
their own C# project.

Key points:

- The C# side is **just boot and framework wiring**. The UI / VM / logic are all
  written in Lisp.
- The Lisp side emits **MAUI's `Application` / `ContentPage` / VM** with
  `dotnet:define-class`. The Lisp-emitted class stands in for the `x:Class`
  partial class that XamlC expects.
- To avoid the MAUI XAML compiler (XamlC), `MainPage.xaml` is a plain embedded
  resource that is `LoadFromXaml`'d at runtime.

## When you launch the demo

Below the title, a list of snippets (a CollectionView) is shown; selecting one
puts that snippet's body in the editor (`Editor`) below. The buttons at the
bottom are:

- **🌐** — switch the snippet display language.
- **▶ Run my-click** — call the function `(defun my-click ...)` defined on the
  Lisp side. Redefine `my-click` in the editor → press ▶ and the new behavior
  runs immediately (live coding).
- **Evaluate** — `read` → `eval` the editor's content and print the result on
  the bottom line. Side effects such as `defun` / `defparameter` persist within
  the session.

If a snippet performs a VM operation like `(setf (slot-value vm 'count) ...)`,
the UI updates immediately via INotifyPropertyChanged. Overwriting a Lisp
function and then pressing `▶` swaps the behavior — that is the feel of "a MAUI
app built in Lisp".

## Embedding recipe

### 1. csproj wiring

```xml
<ProjectReference Include="..\..\runtime\runtime.csproj">
  <!-- reference as a Library (suppress the dotnet-tool Exe) -->
  <AdditionalProperties>DotclAsLibrary=true;RuntimeIdentifier=</AdditionalProperties>
</ProjectReference>

<None Include="..\..\compiler\dotcl.core" Link="dotcl.core">
  <CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>
</None>
<None Include="..\..\contrib\**\*.lisp" LinkBase="contrib">
  <CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>
</None>
<None Include="main.lisp">
  <CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>
</None>
```

On Android, use `<MauiAsset>` instead of `<None>` to bundle into the APK and
extract to `FileSystem.AppDataDirectory` at launch (see `ANDROID-SETUP.md`).

### 2. C# boot

```csharp
DotclHost.Initialize();
DotclHost.LoadCore(DotclHost.FindCore() ?? throw ...);
DotclHost.LoadLispFile(Path.Combine(AppContext.BaseDirectory, "main.lisp"));
```

`DotclHost` (`runtime/DotclHost.cs`) is the embedding façade. The order is
`Initialize → LoadCore → LoadLispFile`. In MAUI, run it at the top of
`CreateMauiApp` in `MauiProgram.cs`.

### 3. C# ↔ Lisp wiring

At a framework fixed point on the C# side (for MAUI, `App.CreateWindow`), call a
Lisp function, unwrap the result, and hand it to the framework:

```csharp
// App.xaml.cs
protected override Window CreateWindow(IActivationState? state)
{
    var result = DotclHost.Call("BUILD-MAIN-PAGE");
    if (result is LispDotNetObject dno && dno.Value is Page page)
        return new Window(page);
    throw new InvalidOperationException(...);
}
```

`LispDotNetObject` is the .NET-object wrapper dotcl returns from `dotnet:new`
etc. Its `Value` property gives the raw instance.

## Lisp-side prefabs

### `MauiLispDemo.MainVM` — VM (INotifyPropertyChanged + ICommand)

```lisp
(dotnet:define-class "MauiLispDemo.MainVM" (Object)
  (:implements INotifyPropertyChanged)
  (:events ("PropertyChanged" PropertyChangedEventHandler))
  (:properties
    ("Title" String :notify t)        ; setter auto-fires PropertyChanged
    ("Count" Int32 :notify t)
    ("IncrementCommand" ICommand))
  (:ctor () ...)
  (:methods
    ("Increment" () :returns Void ...)))
```

- `:notify t` inserts a PropertyChanged firing at the end of the auto-property
  setter.
- The `ICommand` property holds a Lisp lambda wrapped in `LispCommand` (the XAML
  side uses `Command="{Binding IncrementCommand}"`).

### `MauiLispDemo.MainPage` — a ContentPage subclass

```lisp
(dotnet:define-class "MauiLispDemo.MainPage" (ContentPage)
  (:ctor ()
    (let ((xaml (dotnet:static "MauiLispDemo.XamlHelper" "ReadEmbeddedXaml"
                               "MauiLispDemo.MainPage.xaml")))
      (dotnet:static "MauiLispDemo.XamlHelper" "LoadFromXaml" self xaml))
    (dotnet:%set-invoke self "BindingContext" (dotnet:new "MauiLispDemo.MainVM"))))
```

In the ctor:

1. Pull the XAML from the assembly's manifest resources.
2. Expand it into self via
   `Microsoft.Maui.Controls.Xaml.Extensions.LoadFromXaml(self, xaml)`.
3. Set a `MainVM` as the BindingContext.

### XAML conventions

`MainPage.xaml` is **taken out of MAUI's MauiXaml pipeline and embedded as an
embedded resource**. XamlC expects the C# partial named by `x:Class` to exist at
compile time, but here it is emitted at runtime on the Lisp side, so running it
through XamlC yields "type not found". csproj:

```xml
<ItemGroup>
  <MauiXaml Remove="MainPage.xaml" />
  <Content Remove="MainPage.xaml" />
  <None Remove="MainPage.xaml" />
  <EmbeddedResource Remove="MainPage.xaml" />
  <EmbeddedResource Include="MainPage.xaml">
    <LogicalName>MauiLispDemo.MainPage.xaml</LogicalName>
  </EmbeddedResource>
</ItemGroup>
```

`App.xaml`, conversely, keeps **the standard MAUI partial-class style** (an empty
`<Application x:Class="MauiLispDemo.App" />` + `App.xaml.cs`). This is a
compromise: `MauiProgram.UseMauiApp<App>()` requires the `App` type to exist at
compile time, and running XamlC keeps the C# boot wiring shorter. Moving it to a
Lisp emit is still possible (TODO).

## Build & run

```sh
cd samples/MauiLispDemo
dotnet workload restore       # load necessary libraries
dotnet build                  # Debug
dotnet run -c Release         # Release (warm boot ~1s; debug is 5-10s)
```

The GUI process has no console, so the boot trace and exceptions are written to
`bin/.../dotcl-maui.log`. Seeing `[App] BUILD-MAIN-PAGE returned
MauiLispDemo.MainPage` means the wiring succeeded.

For the Android target, see **`ANDROID-SETUP.md`** (workload, build flags,
adb / scrcpy, troubleshooting).

## File guide

| File | Role |
|---|---|
| `MauiProgram.cs` | dotcl boot + `MauiAppBuilder` wiring |
| `App.xaml` / `App.xaml.cs` | `Application` subclass (calls BUILD-MAIN-PAGE in CreateWindow) |
| `MainPage.xaml` | UI markup; embedded as an embedded resource |
| `XamlHelper.cs` | non-generic wrapper around `LoadFromXaml<T>` + manifest resource reader |
| `main.lisp` | defines MainVM / MainPage / build-main-page in Lisp |
| `Makefile` | `make build-windows` / `build-android` / `run-android` etc. |
| `ANDROID-SETUP.md` | Android device setup steps |

## See also

- `runtime/DotclHost.cs` — the full embedding API.
- `contrib/dotnet-class/` — macros such as `dotnet:define-class`.
