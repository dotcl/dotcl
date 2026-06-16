using System.Reflection;

namespace DotCL;

/// <summary>
/// Minimal embedding API for host applications (MAUI, ASP.NET, etc.) that
/// want to run dotcl as a library rather than as the main entry point.
///
/// Typical sequence:
///   DotclHost.Initialize();
///   var core = DotclHost.FindCore();            // bundled dotcl.core
///   if (core != null) DotclHost.LoadCore(core); // boot compiler + stdlib
///   DotclHost.LoadLispFile("main.lisp");        // run user Lisp code
///
/// Before <see cref="LoadCore"/>, only the C# Startup primitives are
/// available. User Lisp code (including the DOTNET:* / DOTCL:* packages)
/// needs the core to be loaded.
/// </summary>
public static class DotclHost
{
    private static bool _initialized;

    /// <summary>
    /// Bootstraps the Lisp runtime (packages, readtable, core functions).
    /// Safe to call multiple times; only the first call does work.
    /// </summary>
    public static void Initialize()
    {
        if (_initialized) return;
        Startup.Initialize();
        _initialized = true;
    }

    /// <summary>
    /// Locate a bundled dotcl core (.fasl PE or .sil text). Looks next to
    /// the entry assembly, under share/dotcl/, and under a dev-tree
    /// fallback at compiler/cil-out.sil. Returns null if nothing matches.
    /// </summary>
    public static string? FindCore()
    {
        var baseDir = AppContext.BaseDirectory;
        var candidates = new[]
        {
            System.IO.Path.Combine(baseDir, "dotcl.core"),
            System.IO.Path.Combine(baseDir, "..", "share", "dotcl", "dotcl.core"),
            System.IO.Path.Combine(baseDir, "..", "..", "..", "..", "compiler", "cil-out.sil"),
        };
        return candidates.Select(System.IO.Path.GetFullPath)
            .FirstOrDefault(System.IO.File.Exists);
    }

    /// <summary>
    /// Load and execute a compiled core. Accepts a FASL PE assembly
    /// (recognized by the "MZ" PE header at byte 0) or a SIL text file.
    /// Must be called after <see cref="Initialize"/>.
    /// </summary>
    public static void LoadCore(string filePath)
    {
        byte[] header = new byte[2];
        using (var fs = System.IO.File.OpenRead(filePath))
        {
            int n = fs.Read(header, 0, 2);
            if (n >= 2 && header[0] == 0x4D && header[1] == 0x5A)
            {
                LoadCoreFasl(filePath);
                return;
            }
        }

        var source = System.IO.File.ReadAllText(filePath);
        var reader = new Reader(new System.IO.StringReader(source));

        if (!reader.TryRead(out var instrList))
            throw new InvalidOperationException($"Empty core file: {filePath}");

        var packageSym = Startup.Sym("*PACKAGE*");
        var oldPackage = DynamicBindings.Get(packageSym);
        try { Emitter.CilAssembler.AssembleAndRun(instrList); }
        finally { DynamicBindings.Set(packageSym, oldPackage); }
    }

    private static void LoadCoreFasl(string filePath)
    {
        var asm = System.Reflection.Assembly.LoadFrom(filePath);
        var t = asm.GetType("CompiledModule")
            ?? throw new InvalidOperationException(
                $"FASL core {filePath}: CompiledModule type not found");
        var mi = t.GetMethod("ModuleInit",
                BindingFlags.Public | BindingFlags.Static)
            ?? throw new InvalidOperationException(
                $"FASL core {filePath}: ModuleInit method not found");

        var packageSym = Startup.Sym("*PACKAGE*");
        var oldPackage = DynamicBindings.Get(packageSym);
        try { mi.Invoke(null, null); }
        finally { DynamicBindings.Set(packageSym, oldPackage); }
    }

    /// <summary>
    /// Load and evaluate a Lisp source file. Same semantics as CL LOAD.
    /// </summary>
    public static void LoadLispFile(string path)
    {
        Runtime.Load(new LispObject[] { new LispString(path) });
    }

    /// <summary>
    /// Load every FASL listed in <paramref name="manifestPath"/>, in order.
    /// Each non-blank line is "<name>\t<filename>" (matching the format
    /// emitted by <c>--resolve-deps --manifest-out</c>); &lt;filename&gt; is
    /// resolved against the manifest's own directory if relative, used as-is
    /// if absolute.
    ///
    /// Intended for project-core deployments (#166): the build target ships
    /// a manifest plus the listed FASLs into the app's asset directory; the
    /// host extracts them and calls this once after <see cref="LoadCore"/>
    /// to bring in all required contribs in dependency order.
    ///
    /// Returns the number of FASLs loaded.
    /// </summary>
    public static int LoadFromManifest(string manifestPath)
    {
        var fullManifest = System.IO.Path.GetFullPath(manifestPath);
        var dir = System.IO.Path.GetDirectoryName(fullManifest)
                  ?? throw new InvalidOperationException(
                      $"LoadFromManifest: cannot determine directory of {manifestPath}");

        var modulesSym = Startup.Sym("*MODULES*");

        int count = 0;
        foreach (var rawLine in System.IO.File.ReadAllLines(fullManifest))
        {
            var line = rawLine.Trim();
            if (line.Length == 0) continue;
            // Split on first tab; bare "<filename>" lines are also accepted.
            var tab = line.IndexOf('\t');
            var fileName = tab >= 0 ? line[(tab + 1)..] : line;
            var resolved = System.IO.Path.IsPathRooted(fileName)
                ? fileName
                : System.IO.Path.Combine(dir, fileName);
            Runtime.Load(new LispObject[] { new LispString(resolved) });

            // Treat each loaded fasl as a "provided" module so a later
            // (require :foo) from user code doesn't trigger module-provide-
            // contrib's filesystem search (which would fail in deployment
            // where the contrib/ tree isn't shipped). Module name is the
            // filename without extension, lowercased — matching the keyword/
            // string normalization REQUIRE applies. dotcl.core is excluded
            // since it's a base image, not a library.
            var moduleName = System.IO.Path.GetFileNameWithoutExtension(fileName).ToLowerInvariant();
            if (moduleName.Length > 0 && moduleName != "dotcl")
            {
                bool present = false;
                for (LispObject c = DynamicBindings.Get(modulesSym); c is Cons cc; c = cc.Cdr)
                    if (cc.Car is LispString s && s.Value == moduleName) { present = true; break; }
                if (!present)
                    DynamicBindings.Set(modulesSym,
                        new Cons(new LispString(moduleName), DynamicBindings.Get(modulesSym)));
            }
            count++;
        }
        return count;
    }

    /// <summary>
    /// Read and evaluate a Lisp source expression given as a string.
    /// </summary>
    public static LispObject EvalString(string source)
    {
        var reader = new Reader(new System.IO.StringReader(source));
        LispObject last = Nil.Instance;
        while (reader.TryRead(out var form))
            last = Runtime.Eval(form);
        return last;
    }

    /// <summary>
    /// Call a Lisp function by name (interned in CL-USER) with .NET object
    /// arguments. Each arg is converted via <see cref="Runtime.DotNetToLisp"/>;
    /// the return is a <see cref="LispObject"/>. Use
    /// <see cref="LispString.Value"/> etc. to extract typed results.
    /// </summary>
    public static LispObject Call(string functionName, params object?[] args)
    {
        var sym = Startup.Sym(functionName);
        if (sym.Function is not LispFunction fn)
            throw new InvalidOperationException(
                $"DotclHost.Call: symbol {functionName} has no function binding");
        var lispArgs = new LispObject[args.Length];
        for (int i = 0; i < args.Length; i++)
            lispArgs[i] = Runtime.DotNetToLisp(args[i]);
        return fn.Invoke(lispArgs);
    }

    /// <summary>
    /// Convert a Lisp result to its natural .NET representation: NIL → null,
    /// T → true, integers → int (or long when out of int range), floats →
    /// double/float, strings → string, a wrapped .NET object → the object
    /// itself. Values without a natural scalar counterpart (lists, symbols,
    /// hash-tables, …) are returned as the underlying <see cref="LispObject"/>,
    /// which the caller can inspect or walk directly. Inverse of the
    /// <see cref="Runtime.DotNetToLisp"/> conversion used on the way in.
    /// </summary>
    public static object? ToClr(LispObject value) => Runtime.LispToDotNetGeneric(value);

    /// <summary>
    /// Convert a Lisp result to the requested .NET type <typeparamref name="T"/>,
    /// using the same marshalling applied to .NET method arguments (so e.g. a
    /// small integer can be requested as <c>long</c>, a keyword as an enum, etc.).
    /// Returns <c>default</c> for NIL; throws <see cref="InvalidCastException"/>
    /// when the value cannot be represented as T.
    /// </summary>
    public static T ToClr<T>(LispObject value)
    {
        var converted = Runtime.LispToDotNet(value, typeof(T));
        if (converted is null) return default!;
        if (converted is T t) return t;
        throw new InvalidCastException(
            $"DotclHost.ToClr<{typeof(T).Name}>: cannot represent {converted.GetType().Name} as {typeof(T).Name}");
    }

    /// <summary>
    /// Precompiled-only mode. When enabled, any attempt to generate code at
    /// runtime — eval/compile of compound forms, dotnet:define-class, native FFI
    /// thunks — throws instead of emitting. A host that loads a precompiled image
    /// can set this after loading to assert it never JITs, mirroring an AOT/IL2CPP
    /// target. Running already-compiled code is unaffected.
    /// </summary>
    public static bool PrecompiledOnly
    {
        get => Emitter.CilAssembler.PrecompiledOnly;
        set => Emitter.CilAssembler.PrecompiledOnly = value;
    }

    /// <summary>
    /// Expose a host .NET function to Lisp under NAME, callable like any Lisp
    /// function (the counterpart of <see cref="Call"/>'s Lisp→C# direction).
    /// The symbol is interned in CL-USER, so Lisp code reads <c>(name ...)</c>
    /// without a package prefix. Arguments arrive as natural .NET values (same
    /// conversion as <see cref="ToClr"/>) and the return is converted back via
    /// <see cref="Runtime.DotNetToLisp"/>; return null for a Lisp NIL. Registering
    /// a function does not generate code, so it is allowed under PrecompiledOnly.
    /// </summary>
    public static void Register(string name, Func<object?[], object?> fn)
    {
        var pkg = Package.FindPackage("CL-USER") ?? Startup.CLUser;
        var (sym, _) = pkg.Intern(name.ToUpperInvariant());
        sym.Function = new LispFunction(args =>
        {
            var clrArgs = new object?[args.Length];
            for (int i = 0; i < args.Length; i++)
                clrArgs[i] = Runtime.LispToDotNetGeneric(args[i]);
            return Runtime.DotNetToLisp(fn(clrArgs));
        });
    }
}
