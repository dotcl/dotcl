# MauiLispDemo — dotcl を C# プロジェクトに組み込むレシピ (MAUI 版)

dotcl を **既存の .NET プロジェクトのインプロセスランタイムとして埋め込む**
ための定型を、MAUI Windows / Android アプリで実演するサンプル。
Common Lisp が分かる読者が、自分の C# プロジェクトに同じパターンを
適用できることを目的にしている。

要点:

- C# 側は **boot とフレームワーク連結だけ**。UI / VM / ロジックは
  すべて Lisp で書く
- Lisp 側は `dotnet:define-class` で **MAUI の `Application` / `ContentPage`
  / VM を emit** する。XamlC が要求する `x:Class` の partial class を
  Lisp emit クラスが代行する
- MAUI XAML compiler (XamlC) を回避するため、`MainPage.xaml` を素の
  embedded resource にして runtime で `LoadFromXaml` する

## デモを起動すると

タイトル下にスニペット一覧 (CollectionView) が並び、選ぶと下のエディタ
(`Editor`) にそのスニペット本文が出る。下端のボタンは:

- **🌐** — スニペット表示言語の切替
- **▶ Run my-click** — Lisp 側で `(defun my-click ...)` されている関数を
  呼ぶ。エディタ内で `my-click` を再定義 → ▶ で即座に新挙動が走る
  (live coding)
- **Evaluate** — エディタの内容を `read` → `eval` し、結果を最下行に出す。
  `defun` / `defparameter` 等の副作用は session 内で持続

スニペット側に `(setf (slot-value vm 'count) ...)` のような VM 操作を
書けば、INotifyPropertyChanged 経由で UI が即時更新される。`▶` で
Lisp 関数を上書きしてから押せば挙動が差し替わる、というのが
"Lisp で組まれた MAUI アプリ" の体感。

## 組み込みレシピ

### 1. csproj 配線

```xml
<ProjectReference Include="..\..\runtime\runtime.csproj">
  <!-- Library として参照 (dotnet tool 兼用 Exe を抑止)。D820 -->
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

Android では `<None>` の代わりに `<MauiAsset>` で APK に bundle、起動
時に `FileSystem.AppDataDirectory` に extract する (詳細は `ANDROID-SETUP.md`)。

### 2. C# 側 boot

```csharp
DotclHost.Initialize();
DotclHost.LoadCore(DotclHost.FindCore() ?? throw ...);
DotclHost.LoadLispFile(Path.Combine(AppContext.BaseDirectory, "main.lisp"));
```

`DotclHost` (`runtime/DotclHost.cs`) が組み込み用 façade。
`Initialize → LoadCore → LoadLispFile` の順。MAUI では `MauiProgram.cs`
の `CreateMauiApp` 冒頭で実行する。

### 3. C# ↔ Lisp 結線

C# 側のフレームワーク fixed-point (MAUI なら `App.CreateWindow`) で
Lisp 関数を呼び、戻り値を unwrap してフレームワークに渡す:

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

`LispDotNetObject` は dotcl が `dotnet:new` 等で返す .NET オブジェクト
ラッパー。`Value` プロパティで生インスタンスを取り出す。

## Lisp 側の prefab

### `MauiLispDemo.MainVM` — VM (INotifyPropertyChanged + ICommand)

```lisp
(dotnet:define-class "MauiLispDemo.MainVM" (Object)
  (:implements INotifyPropertyChanged)
  (:events ("PropertyChanged" PropertyChangedEventHandler))
  (:properties
    ("Title" String :notify t)        ; setter で PropertyChanged 自動発火
    ("Count" Int32 :notify t)
    ("IncrementCommand" ICommand))
  (:ctor () ...)
  (:methods
    ("Increment" () :returns Void ...)))
```

- `:notify t` が auto-property の setter 末尾に PropertyChanged 発火を
  挿入 (D790)
- `ICommand` プロパティは `LispCommand` で wrap した
  Lisp lambda を入れる (XAML 側は `Command="{Binding IncrementCommand}"`)

### `MauiLispDemo.MainPage` — ContentPage 派生

```lisp
(dotnet:define-class "MauiLispDemo.MainPage" (ContentPage)
  (:ctor ()
    (let ((xaml (dotnet:static "MauiLispDemo.XamlHelper" "ReadEmbeddedXaml"
                               "MauiLispDemo.MainPage.xaml")))
      (dotnet:static "MauiLispDemo.XamlHelper" "LoadFromXaml" self xaml))
    (dotnet:%set-invoke self "BindingContext" (dotnet:new "MauiLispDemo.MainVM"))))
```

ctor で:

1. XAML をアセンブリの manifest resource から取り出し
2. `Microsoft.Maui.Controls.Xaml.Extensions.LoadFromXaml(self, xaml)` で
   自分自身に展開
3. `MainVM` を BindingContext に挿す

### XAML 側のお作法

`MainPage.xaml` は **MAUI の MauiXaml パイプラインから外して
embedded resource として埋める**。XamlC は `x:Class` で指定された C#
partial が compile time に存在することを期待するが、ここでは Lisp 側で
runtime emit するので XamlC を通すと "type not found" になる。csproj:

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

`App.xaml` は逆に **MAUI 標準の partial class 流儀** を維持している
(空の `<Application x:Class="MauiLispDemo.App" />` + `App.xaml.cs`)。
これは `MauiProgram.UseMauiApp<App>()` が compile-time に `App` 型の
存在を要求し、かつ XamlC を回す方が C# 側の boot 配線が短く書けるため
妥協した部分。Lisp emit に寄せる余地は残っている (TODO)。

## ビルド・実行

```sh
cd samples/MauiLispDemo
dotnet workload restore       # Load necessary libraries
dotnet build                  # Debug
dotnet run -c Release         # Release (warm boot ~1s, debug は 5-10s)
```

GUI プロセスはコンソールを持たないので、boot trace と例外は
`bin/.../dotcl-maui.log` に書く。`[App] BUILD-MAIN-PAGE returned
MauiLispDemo.MainPage` が出れば結線成功。

Android target は **`ANDROID-SETUP.md`** 参照 (workload、build flag、
adb / scrcpy、トラブルシューティング)。

## ファイル案内

| ファイル | 役割 |
|---|---|
| `MauiProgram.cs` | dotcl boot + `MauiAppBuilder` 配線 |
| `App.xaml` / `App.xaml.cs` | `Application` 派生 (CreateWindow で BUILD-MAIN-PAGE 呼出) |
| `MainPage.xaml` | UI markup。embedded resource として埋め込み |
| `XamlHelper.cs` | `LoadFromXaml<T>` の非ジェネリックラッパー + manifest resource reader |
| `main.lisp` | MainVM / MainPage / build-main-page を Lisp で定義 |
| `Makefile` | `make build-windows` / `build-android` / `run-android` 等 |
| `ANDROID-SETUP.md` | Android 実機セットアップ手順 |

## 関連

- `runtime/DotclHost.cs` — embedding API 一式
- `contrib/dotnet-class/` — `dotnet:define-class` 等のマクロ
- `docs/decisions/D820-D822` — Android 対応で踏んだ罠 (Library mode、
  Console.In fallback、EmbedAssembliesIntoApk、etc.)
- `docs/decisions/D785-D795` — `dotnet:define-class` の各 Step
  (auto-property、INotifyPropertyChanged 自動配線、XAML embed)
