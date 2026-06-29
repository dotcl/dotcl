# AspNetLispDemo — write an ASP.NET Core Controller in Lisp

A minimal sample that embeds dotcl as an in-process runtime inside an ASP.NET
Core process and **defines a `Microsoft.AspNetCore.Mvc.ControllerBase` subtype
in Lisp** to serve HTTP endpoints. It applies the same "subclass a .NET
framework type from Lisp" pattern as the MAUI demo, on the web side.

## Behavior

```
$ dotnet build
$ bin/Debug/net10.0/AspNetLispDemo.exe
[aspnet] DotclDynamic_1: controllers=[Demo.HelloController]
[aspnet] running on http://localhost:5180

$ curl http://localhost:5180/api/hello
"hello from lisp"

$ curl http://localhost:5180/api/async-hello
hello from async lisp
```

`/api/hello` is a synchronous MVC controller; `/api/async-hello` wires a Lisp
`(dotcl:async ...)` handler (the Task producer side) to a Minimal API route. The
latter does not block the request thread — the `dotcl:await` inside the handler
waits on a real .NET Task (`Task.Delay`) on the thread pool.

## The Lisp side

The whole of `main.lisp` is ~10 lines:

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

- `dotnet:define-class`'s `:attributes` put `[Route]` on the class and
  `[HttpGet]` on the method.
- The method returns `IActionResult`; returning an `OkObjectResult` from the
  body lets MVC serialize it as JSON / text.

## The C# side (`Program.cs`)

dotcl boot → start ASP.NET → adopt the dynamic assembly as an ApplicationPart:

```csharp
DotclHost.Initialize();
DotclHost.LoadFromManifest(...);   // dotcl.core + dotnet-class.fasl
                                    //   + AspNetLispDemo.fasl

builder.Services.AddControllers()
    .ConfigureApplicationPartManager(apm =>
    {
        // Walk AppDomain.CurrentDomain.GetAssemblies() and register any
        // assembly that holds a ControllerBase subtype as an ApplicationPart.
        // The DotclDynamic_N assembly dotcl emitted is included here too.
    });

app.MapControllers();
app.Run("http://localhost:5180");
```

## How it's wired

`AspNetLispDemo.csproj` uses the same `<DotclProjectAsd>` +
`<Import Project="...Dotcl.targets" />` pattern as MauiLispDemo.
`AspNetLispDemo.asd` declares `:depends-on ("dotnet-class")` +
`:components ((:file "main"))`, and the build target bundles what's needed via
the manifest.

## Limitations / follow-ups

- **Static controller definition only**: defining a new controller at runtime
  with `(dotnet:define-class ...)` does not rebuild MVC's routing table.
  Hot-add / hot-redefine needs a separate mechanism (e.g.
  `IActionDescriptorChangeProvider`).
- **MVC + Minimal API together**: the MVC controller (`/api/hello`) shows
  "a controller in Lisp"; the Minimal API trampoline (`/api/async-hello`) is a
  lightweight path that wires the Lisp `dotcl:async` handler (Task producer
  side). The latter calls the Lisp function via `DotclHost.Call`, gets a
  `Task<LispObject>`, and `await`s it on the C# side. Directly binding an MVC
  async action (returning `Task<IActionResult>`) needs type conversion, so for
  now going through Minimal API is the simplest.
- **DI container**: controllers currently assume a parameterless ctor.
  Constructor injection requires extending the ctor signature on the dotcl side
  (`dotnet:define-class`'s `:ctor` is currently zero-arg only).

## See also

- MauiLispDemo: the same "subclass a .NET type from Lisp" pattern on desktop / mobile.
- The build pipeline reuses the same `<DotclProjectAsd>` pattern as MauiLispDemo.
