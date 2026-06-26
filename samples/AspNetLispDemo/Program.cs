using DotCL;
using Microsoft.AspNetCore.Mvc.ApplicationParts;

// Boot dotcl BEFORE ASP.NET starts, so the Lisp side has a chance to
// (dotnet:define-class "Demo.HelloController" ...) and the dynamically
// emitted assembly is loaded into the AppDomain. ASP.NET MVC's controller
// discovery walks AppDomain assemblies for ControllerBase subclasses, so
// once the FASL is loaded the type is visible.

DotclHost.Initialize();

// Force MVC core assemblies loaded so dotcl's ResolveDotNetType can see
// ControllerBase / ActionResult / OkObjectResult / RouteAttribute when
// the Lisp side names them by short name.
_ = typeof(Microsoft.AspNetCore.Mvc.ControllerBase).FullName;
_ = typeof(Microsoft.AspNetCore.Mvc.OkObjectResult).FullName;
_ = typeof(Microsoft.AspNetCore.Mvc.IActionResult).FullName;
_ = typeof(Microsoft.AspNetCore.Mvc.RouteAttribute).FullName;
_ = typeof(Microsoft.AspNetCore.Mvc.HttpGetAttribute).FullName;

var manifestPath = Path.Combine(
    AppContext.BaseDirectory, "dotcl-fasl", "dotcl-deps.txt");
Console.WriteLine($"[dotcl] manifest: {manifestPath}");
var loaded = DotclHost.LoadFromManifest(manifestPath);
Console.WriteLine($"[dotcl] LoadFromManifest loaded {loaded} fasls");

// Build ASP.NET. After Lisp has emitted controller types, register the
// emitted assembly as an ApplicationPart so MVC discovers them.
var builder = WebApplication.CreateBuilder(args);

builder.Services.AddControllers()
    .ConfigureApplicationPartManager(apm =>
    {
        // Walk every loaded assembly and add as ApplicationPart if it
        // contains anything looking like a Controller. The FASL we just
        // loaded contains an emitted type derived from ControllerBase.
        // Note: FASLs from compile-file are NOT marked IsDynamic (they're
        // loaded from disk via Assembly.LoadFrom); the AssemblyBuilder
        // ones from dotnet:define-class ARE IsDynamic but still need to
        // be adopted as ApplicationPart for MVC discovery.
        foreach (var asm in AppDomain.CurrentDomain.GetAssemblies())
        {
            try
            {
                Type[] types;
                try { types = asm.GetTypes(); }
                catch (System.Reflection.ReflectionTypeLoadException rtle)
                {
                    types = rtle.Types.Where(t => t != null).ToArray()!;
                }
                var controllers = types.Where(t =>
                    typeof(Microsoft.AspNetCore.Mvc.ControllerBase).IsAssignableFrom(t)
                    && !t.IsAbstract).ToArray();
                if (controllers.Length > 0)
                {
                    Console.WriteLine($"[aspnet] {asm.GetName().Name}: " +
                        $"controllers=[{string.Join(", ", controllers.Select(t => t.FullName))}]");
                    if (!apm.ApplicationParts.Any(p => p.Name == asm.GetName().Name))
                        apm.ApplicationParts.Add(new AssemblyPart(asm));
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[aspnet] skip {asm.GetName().Name}: {ex.GetType().Name}");
            }
        }
    });

var app = builder.Build();
app.MapControllers();

// Async endpoint (Task-producing side). The Lisp `async-hello` returns a
// .NET Task<LispObject> built by (dotcl:async ...) — it awaits a real Task.Delay
// off the request thread, then yields a value. We await that Task here and write
// its (string) result, so the request completes without a thread blocked on the
// delay. This is the Minimal-API trampoline the README flagged as the lighter
// path for runtime endpoints (vs. an MVC async action's Task<IActionResult>).
app.MapGet("/api/async-hello", async () =>
{
    // DotclHost.Call returns the Lisp value wrapping the Task<LispObject>.
    var wrapped = (LispDotNetObject)DotclHost.Call("ASYNC-HELLO");
    var task = (Task<LispObject>)wrapped.Value;   // the (dotcl:async ...) Task
    var result = await task;                       // off-thread await, no blocking
    return Results.Text(DotclHost.ToClr<string>(result));
});

Console.WriteLine("[aspnet] running on http://localhost:5180");
app.Run("http://localhost:5180");
