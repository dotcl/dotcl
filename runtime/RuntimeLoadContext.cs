using System.Reflection;
#if !NETSTANDARD2_0
using System.Reflection.Emit;
#endif

namespace DotCL;

/// <summary>
/// Loads compiled Lisp (the core, fasls) and other assemblies into the load
/// context this runtime itself lives in. A host that puts DotCL.Runtime in a
/// context of its own (a plugin host, a COM in-process server) would otherwise
/// see those assemblies bind to a second, uninitialized copy of the runtime in
/// the default context, or to none at all. In the default context every method
/// here is the plain Assembly call it replaces.
/// </summary>
public static class RuntimeLoadContext
{
#if NETSTANDARD2_0
    public static bool IsDefault => true;

    public static Assembly LoadFrom(string path) => Assembly.LoadFrom(path);

    public static Assembly LoadBytes(byte[] image, byte[]? symbols = null)
        => symbols == null ? Assembly.Load(image) : Assembly.Load(image, symbols);
#else
    private static readonly System.Runtime.Loader.AssemblyLoadContext s_context =
        System.Runtime.Loader.AssemblyLoadContext.GetLoadContext(typeof(RuntimeLoadContext).Assembly)
        ?? System.Runtime.Loader.AssemblyLoadContext.Default;

    /// <summary>The context DotCL.Runtime was loaded into.</summary>
    public static System.Runtime.Loader.AssemblyLoadContext Context => s_context;

    public static bool IsDefault => s_context == System.Runtime.Loader.AssemblyLoadContext.Default;

    // Directories LoadFrom took an assembly from, probed for its dependencies the
    // way Assembly.LoadFrom does in the default context.
    private static readonly List<string> s_loadFromDirs = new();

    /// <summary>Assembly.LoadFrom, into the runtime's context.</summary>
    public static Assembly LoadFrom(string path)
    {
        if (IsDefault) return Assembly.LoadFrom(path);
        var full = Path.GetFullPath(path);
        lock (s_loadFromDirs)
        {
            foreach (var a in s_context.Assemblies)
                if (!a.IsDynamic && string.Equals(a.Location, full, StringComparison.OrdinalIgnoreCase))
                    return a;
            var dir = Path.GetDirectoryName(full);
            if (dir != null && !s_loadFromDirs.Contains(dir, StringComparer.OrdinalIgnoreCase))
                s_loadFromDirs.Add(dir);
        }
        return s_context.LoadFromAssemblyPath(full);
    }

    /// <summary>Assembly.Load(byte[]): a context of its own for each image, whose
    /// references resolve through the runtime's context.</summary>
    public static Assembly LoadBytes(byte[] image, byte[]? symbols = null)
    {
        if (IsDefault)
            return symbols == null ? Assembly.Load(image) : Assembly.Load(image, symbols);
        var ctx = NewContext("dotcl image");
        using var ms = new MemoryStream(image, false);
        if (symbols == null) return ctx.LoadFromStream(ms);
        using var ps = new MemoryStream(symbols, false);
        return ctx.LoadFromStream(ms, ps);
    }

    /// <summary>A fresh context for one assembly, so the same name can load again.</summary>
    internal static System.Runtime.Loader.AssemblyLoadContext NewContext(string name)
        => new ChildContext(name, s_context);

    /// <summary>A dynamic assembly referencing a collectible runtime must be
    /// collectible too.</summary>
    internal static AssemblyBuilderAccess RunAccess
        => s_context.IsCollectible ? AssemblyBuilderAccess.RunAndCollect : AssemblyBuilderAccess.Run;

    internal static Assembly? ProbeLoadFromDirs(System.Runtime.Loader.AssemblyLoadContext ctx, AssemblyName name)
    {
        string[] dirs;
        lock (s_loadFromDirs) dirs = s_loadFromDirs.ToArray();
        foreach (var dir in dirs)
        {
            var dll = Path.Combine(dir, name.Name + ".dll");
            if (File.Exists(dll)) return ctx.LoadFromAssemblyPath(dll);
        }
        return null;
    }

    private sealed class ChildContext : System.Runtime.Loader.AssemblyLoadContext
    {
        private readonly System.Runtime.Loader.AssemblyLoadContext _parent;

        public ChildContext(string name, System.Runtime.Loader.AssemblyLoadContext parent)
            : base(name, parent.IsCollectible)
            => _parent = parent;

        protected override Assembly? Load(AssemblyName name)
        {
            if (_parent == Default) return null;
            try { return _parent.LoadFromAssemblyName(name); }
            catch (FileNotFoundException) { return null; }
        }
    }
#endif
}
