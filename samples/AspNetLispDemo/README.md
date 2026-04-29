# AspNetLispDemo — ASP.NET Core Controller を Lisp で書く

dotcl を ASP.NET Core プロセスのインプロセスランタイムとして埋め込み、
**`Microsoft.AspNetCore.Mvc.ControllerBase` 派生型を Lisp 側で定義** して
HTTP endpoint を serve する最小サンプル (#164)。MAUI demo (#161 phase 2)
と同じ「.NET framework type を Lisp で継承」パターンを web 側に展開した
形。

## 動作

```
$ dotnet build
$ bin/Debug/net10.0/AspNetLispDemo.exe
[aspnet] DotclDynamic_1: controllers=[Demo.HelloController]
[aspnet] running on http://localhost:5180

$ curl http://localhost:5180/api/hello
"hello from lisp"
```

## Lisp 側

`main.lisp` 全体は ~10 行:

```lisp
(require :dotnet-class)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (gethash "CONTROLLERBASE" dotnet::*type-aliases*)
        "Microsoft.AspNetCore.Mvc.ControllerBase")
  (setf (gethash "IACTIONRESULT" dotnet::*type-aliases*)
        "Microsoft.AspNetCore.Mvc.IActionResult"))

(dotnet:define-class "Demo.HelloController" (ControllerBase)
  (:attributes
    ("Microsoft.AspNetCore.Mvc.RouteAttribute" "api/hello"))
  (:methods
    ("Get" () :returns IActionResult
       :attributes (("Microsoft.AspNetCore.Mvc.HttpGetAttribute"))
      (dotnet:new "Microsoft.AspNetCore.Mvc.OkObjectResult"
                  "hello from lisp"))))
```

- `dotnet:define-class` の `:attributes` (D845) でクラスに `[Route]`、
  メソッドに `[HttpGet]` を載せる
- メソッドの戻り値は `IActionResult`、body で `OkObjectResult` を
  返すと MVC がそのまま JSON / text として serialize

## C# 側 (`Program.cs`)

dotcl boot → ASP.NET 起動 → 動的アセンブリを ApplicationPart として
adopt:

```csharp
DotclHost.Initialize();
DotclHost.LoadFromManifest(...);   // dotcl.core + dotnet-class.fasl
                                    //   + AspNetLispDemo.fasl

builder.Services.AddControllers()
    .ConfigureApplicationPartManager(apm =>
    {
        // AppDomain.CurrentDomain.GetAssemblies() を walk、ControllerBase
        // 派生型を持つアセンブリを ApplicationPart として登録。
        // dotcl が emit した DotclDynamic_N アセンブリもここに含まれる。
    });

app.MapControllers();
app.Run("http://localhost:5180");
```

## 組込みかた (#166 project-core)

`AspNetLispDemo.csproj` は MauiLispDemo と同じ `<DotclProjectAsd>` +
`<Import Project="...Dotcl.targets" />` パターン。`AspNetLispDemo.asd`
で `:depends-on ("dotnet-class")` + `:components ((:file "main"))` を
宣言、build target が manifest 経由で必要なものを bundle する。

## 制約 / 後続課題

- **静的 Controller 定義のみ**: ランタイムで `(dotnet:define-class ...)` で
  新しい Controller を生やしても MVC の routing table は再構築されない。
  Hot-add / hot-redefine は別の仕組みが要る (e.g.,
  `IActionDescriptorChangeProvider`)
- **Minimal API ではなく MVC**: MVC のほうが「Lisp で Controller」の絵に
  なるが、Minimal API + Lambda trampoline ならランタイム endpoint 追加が
  軽い。両方サポートはまだ未着手
- **DI container**: 現在 Controller は parameterless ctor 想定。コンストラクタ
  注入は dotcl 側の ctor シグネチャ拡張が要る (`dotnet:define-class` の
  `:ctor` は現状 zero-arg のみ)

## 関連

- #164 (本 demo の主旨)
- D845 (`define-class` の method-level :attributes 拡張、本 demo の前提)
- MauiLispDemo (#161 / D771-D795): 同じ "Lisp で .NET type を継承" の
  パターンを desktop / mobile に適用
- D820-D843 (#166 project-core): build pipeline はそのまま流用
