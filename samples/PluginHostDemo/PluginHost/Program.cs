using System.Reflection;
using System.Runtime.Loader;
using PluginHost;

var dir = Path.GetFullPath(args.Length > 0 ? args[0] : Path.Combine(AppContext.BaseDirectory, "plugins"));
var input = args.Length > 1 ? args[1] : "hello plugin world";
var abstractions = typeof(IPlugin).Assembly;

Console.WriteLine($"input: \"{input}\"");
// Convention: plugins/<Name>/<Name>.dll, with the plugin's own dependencies beside it.
foreach (var path in Directory.GetDirectories(dir).OrderBy(d => d)
             .Select(d => Path.Combine(d, Path.GetFileName(d) + ".dll")).Where(File.Exists))
{
    var asm = Load(path);
    if (asm is null) continue;
    foreach (var type in SafeTypes(asm).Where(t => !t.IsAbstract && typeof(IPlugin).IsAssignableFrom(t)))
    {
        var plugin = (IPlugin)Activator.CreateInstance(type)!;
        plugin.Completed += result => Console.WriteLine($"  completed: {result}");
        var output = plugin.Run(input, step => Console.WriteLine($"  progress: {step}"));
        Console.WriteLine($"[{Path.GetFileName(path)}] {plugin.Name}: {output}");
    }
}

// Diagnostic only: the host does not use dotcl. If a plugin brought DotCL.Runtime along,
// report whether a core was loaded into it.
foreach (var ctx in AssemblyLoadContext.All)
    foreach (var a in ctx.Assemblies.Where(a => a.GetName().Name == "DotCL.Runtime"))
    {
        var loaded = a.GetType("DotCL.DotclHost")?.GetProperty("CoreLoaded")?.GetValue(null);
        Console.WriteLine($"DotCL.Runtime is loaded in a plugin context; DotclHost.CoreLoaded = {loaded}");
    }

Assembly? Load(string path)
{
    var ctx = new PluginLoadContext(path);
    try { return ctx.LoadFromAssemblyPath(path); }
    catch (BadImageFormatException) { return null; }
}

static IEnumerable<Type> SafeTypes(Assembly asm)
{
    try { return asm.GetTypes(); }
    catch (ReflectionTypeLoadException e) { foreach (var x in e.LoaderExceptions) Console.Error.WriteLine("plugin load: " + x?.Message); return e.Types.Where(t => t != null)!; }
}

sealed class PluginLoadContext : AssemblyLoadContext
{
    readonly string _dir;
    public PluginLoadContext(string path) : base(isCollectible: false)
        => _dir = Path.GetDirectoryName(path)!;

    protected override Assembly? Load(AssemblyName name)
    {
        // Shared contract assembly comes from the host; everything else from the plugin folder.
        if (name.Name == typeof(IPlugin).Assembly.GetName().Name) return null;
        var p = Path.Combine(_dir, name.Name + ".dll");
        return File.Exists(p) ? LoadFromAssemblyPath(p) : null;
    }
}
