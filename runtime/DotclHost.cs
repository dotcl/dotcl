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
    private static bool _coreLoaded;

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
#if NET5_0_OR_GREATER
        // Android: the core ships as an APK AndroidAsset (assets/dotcl/dotcl.core,
        // see build/DotCL.Runtime.targets). APK assets are not real filesystem
        // paths, so AppContext.BaseDirectory/dotcl.core does not exist.
        // Extract the asset tree to a writable dir and return the extracted path.
        // (Guarded to NET5+: OperatingSystem.IsAndroid is absent on netstandard2.0,
        // which is the emit-free desktop runtime and never runs on Android.)
        if (OperatingSystem.IsAndroid())
        {
            var extracted = TryExtractAndroidCore();
            if (extracted != null) return extracted;
            // fall through to the file probes below (dev/unusual layouts)
        }
#endif

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

#if NET5_0_OR_GREATER
    /// <summary>
    /// Extract the bundled dotcl asset tree (dotcl.core + contrib/**) from the
    /// Android APK's asset manager into a writable cache dir, returning the path
    /// to the extracted dotcl.core (or null if not on Android / asset missing).
    ///
    /// Uses reflection on Android.App.Application so this file compiles on the
    /// plain net10.0 TFM (DotCL.Runtime does not target net10.0-android): the
    /// Mono.Android assembly is present at runtime on Android, absent elsewhere.
    /// The contrib tree is extracted alongside so (require :dotnet-class) etc.
    /// resolve from the extracted dir (which the host should add as a contrib
    /// search path, or which sits next to dotcl.core).
    /// </summary>
    private static string? TryExtractAndroidCore()
    {
        try
        {
            // Android.App.Application.Context  (static)
            var appType = Type.GetType("Android.App.Application, Mono.Android");
            var context = appType?.GetProperty("Context",
                BindingFlags.Public | BindingFlags.Static)?.GetValue(null);
            if (context == null) return null;

            // context.Assets  -> AssetManager
            var assets = context.GetType().GetProperty("Assets")?.GetValue(context);
            if (assets == null) return null;

            // context.CacheDir.AbsolutePath  -> writable dir
            var cacheDir = context.GetType().GetProperty("CacheDir")?.GetValue(context);
            var destRoot = cacheDir?.GetType().GetProperty("AbsolutePath")?.GetValue(cacheDir) as string;
            if (string.IsNullOrEmpty(destRoot)) return null;
            destRoot = System.IO.Path.Combine(destRoot, "dotcl");

            var open = assets.GetType().GetMethod("Open", new[] { typeof(string) });
            var list = assets.GetType().GetMethod("List", new[] { typeof(string) });
            if (open == null || list == null) return null;

            // Recursively extract the "dotcl" asset subtree.
            ExtractAssetTree(assets, open, list, "dotcl", destRoot);

            var corePath = System.IO.Path.Combine(destRoot, "dotcl.core");
            return System.IO.File.Exists(corePath) ? corePath : null;
        }
        catch { return null; }   // any reflection/IO failure → fall back to file probes
    }

    private static void ExtractAssetTree(object assets,
        System.Reflection.MethodInfo open, System.Reflection.MethodInfo list,
        string assetPath, string destDir)
    {
        var children = (string[]?)list.Invoke(assets, new object[] { assetPath });
        if (children == null || children.Length == 0)
        {
            // Leaf (a file): copy it. (AssetManager.List returns empty for files.)
            System.IO.Directory.CreateDirectory(System.IO.Path.GetDirectoryName(destDir)!);
            using var src = (System.IO.Stream)open.Invoke(assets, new object[] { assetPath })!;
            using var dst = System.IO.File.Create(destDir);
            src.CopyTo(dst);
            return;
        }
        // Directory: recurse into each child.
        System.IO.Directory.CreateDirectory(destDir);
        foreach (var child in children)
            ExtractAssetTree(assets, open, list,
                assetPath + "/" + child, System.IO.Path.Combine(destDir, child));
    }
#endif

    /// <summary>
    /// Load and execute a compiled core. Accepts a FASL PE assembly
    /// (recognized by the "MZ" PE header at byte 0) or a SIL text file.
    /// Must be called after <see cref="Initialize"/>.
    /// </summary>
    public static void LoadCore(string filePath)
    {
        _coreLoaded = true;
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

        RunCoreSil(System.IO.File.ReadAllText(filePath), filePath);
    }

    /// <summary>
    /// Load and execute a compiled core already in memory. Same two formats as
    /// <see cref="LoadCore(string)"/> — a FASL PE assembly (the "MZ" header) or SIL
    /// text — for a host with no filesystem to read from. A browser fetches the core
    /// over HTTP and hands the bytes straight here; there is no path to open.
    ///
    /// The PE form goes through Assembly.Load(byte[]), so the module has no file
    /// Location. That is the only option without a filesystem, and it is why this is
    /// an overload rather than a replacement: the path version keeps LoadFrom, whose
    /// file-backed module is what tools selecting per loaded module can see.
    ///
    /// The SIL text form still needs Reflection.Emit to assemble, so on an emit-free
    /// build the core has to be the FASL form.
    /// </summary>
    public static void LoadCore(byte[] coreImage)
    {
        if (coreImage == null || coreImage.Length == 0)
            throw new ArgumentException("LoadCore: the core image is empty", nameof(coreImage));
        _coreLoaded = true;

        if (coreImage.Length >= 2 && coreImage[0] == 0x4D && coreImage[1] == 0x5A)
        {
            RunCoreModuleInit(System.Reflection.Assembly.Load(coreImage), "FASL core (in memory)");
            return;
        }

        var source = System.Text.Encoding.UTF8.GetString(coreImage);
        RunCoreSil(source, "core (in memory)");
    }

    private static void LoadCoreFasl(string filePath)
        => RunCoreModuleInit(System.Reflection.Assembly.LoadFrom(filePath), $"FASL core {filePath}");

    /// <summary>Call a loaded core assembly's CompiledModule.ModuleInit, restoring
    /// *PACKAGE* afterwards. WHAT names the core in errors.</summary>
    private static void RunCoreModuleInit(System.Reflection.Assembly asm, string what)
    {
        // Same reason as Program.RunCoreFasl: the generation stamp is read off the
        // core assembly, so an embedding host must record it too — including the
        // in-memory path, which has no file to fall back to.
        Startup.CoreAssembly = asm;
        var t = asm.GetType("CompiledModule")
            ?? throw new InvalidOperationException(
                $"{what}: CompiledModule type not found");
        var mi = t.GetMethod("ModuleInit",
                BindingFlags.Public | BindingFlags.Static)
            ?? throw new InvalidOperationException(
                $"{what}: ModuleInit method not found");

        var packageSym = Startup.Sym("*PACKAGE*");
        var oldPackage = DynamicBindings.Get(packageSym);
        try { mi.Invoke(null, null); }
        finally { DynamicBindings.Set(packageSym, oldPackage); }
    }

    /// <summary>Assemble and run SIL core text, restoring *PACKAGE* afterwards.</summary>
    private static void RunCoreSil(string source, string what)
    {
        var reader = new Reader(new System.IO.StringReader(source));
        if (!reader.TryRead(out var instrList))
            throw new InvalidOperationException($"Empty core file: {what}");

        var packageSym = Startup.Sym("*PACKAGE*");
        var oldPackage = DynamicBindings.Get(packageSym);
        try { Emitter.CilAssembler.AssembleAndRun(instrList); }
        finally { DynamicBindings.Set(packageSym, oldPackage); }
    }

    /// <summary>
    /// Run a build-time-linked compiled module's <c>CompiledModule.ModuleInit</c>
    /// without loading any assembly at run time. This is the AOT/IL2CPP path:
    /// the .fasl (core or app) is referenced as a normal assembly at build time
    /// (so the AOT compiler bakes it in), and the host hands its ModuleInit
    /// method group here, e.g.
    /// <code>
    ///   extern alias dotclcore;            // &lt;Reference ...&gt;&lt;Aliases&gt;dotclcore&lt;/Aliases&gt;
    ///   DotclHost.RunLinkedModule(dotclcore::CompiledModule.ModuleInit);
    /// </code>
    /// Unlike <see cref="LoadCore"/>/<see cref="LoadLispFile"/>, this never calls
    /// <c>Assembly.LoadFrom</c> (which throws PlatformNotSupportedException under
    /// NativeAOT). The <c>*PACKAGE*</c> binding is saved/restored exactly as the
    /// reflection-based loader does. Each compiled module exposes a public static
    /// <c>CompiledModule.ModuleInit()</c>; collisions between the core's and the
    /// app's same-named type are resolved by extern alias at the call site.
    /// </summary>
    public static LispObject? RunLinkedModule(Func<LispObject?> moduleInit)
    {
        if (moduleInit is null) throw new ArgumentNullException(nameof(moduleInit));
        var packageSym = Startup.Sym("*PACKAGE*");
        var oldPackage = DynamicBindings.Get(packageSym);
        try { return moduleInit(); }
        finally { DynamicBindings.Set(packageSym, oldPackage); }
    }

    /// <summary>
    /// Build-time-link convenience over <see cref="RunLinkedModule"/>: resolve an
    /// already-baked-in compiled module by its stable assembly NAME — the
    /// <c>:module-name</c> passed to <c>compile-file</c> / <c>dotcl:sil-to-fasl</c>,
    /// which must equal the referenced file's base name — and run its
    /// <c>CompiledModule.ModuleInit</c>. Uses <see cref="Assembly.Load(AssemblyName)"/>
    /// on an assembly that is already linked into the image; it never calls
    /// <c>Assembly.LoadFrom</c> (PlatformNotSupported under NativeAOT), so it is the
    /// AOT/IL2CPP boot path. The module's assembly must be kept whole via
    /// <c>&lt;TrimmerRootAssembly&gt;</c> so the reflected type and method survive
    /// trimming. This centralizes the reflection a host would otherwise hand-write,
    /// letting the host boot a stable-named core/app fasl with a single call:
    /// <code>
    ///   DotclHost.RunLinkedModuleByName("dotclcore");   // the FASL core
    ///   DotclHost.RunLinkedModuleByName("appfasl");     // the app image
    /// </code>
    /// </summary>
    public static LispObject? RunLinkedModuleByName(string assemblyName)
    {
        if (assemblyName is null) throw new ArgumentNullException(nameof(assemblyName));
        var asm = Assembly.Load(new AssemblyName(assemblyName));
        var t = asm.GetType("CompiledModule")
            ?? throw new InvalidOperationException(
                $"RunLinkedModuleByName: {assemblyName}: CompiledModule type not found");
        var mi = t.GetMethod("ModuleInit", BindingFlags.Public | BindingFlags.Static)
            ?? throw new InvalidOperationException(
                $"RunLinkedModuleByName: {assemblyName}: ModuleInit method not found");
        return RunLinkedModule(() => (LispObject?)mi.Invoke(null, null));
    }

    /// <summary>
    /// True once a core has been loaded through <see cref="LoadCore"/> or
    /// <see cref="LoadFromManifest"/> in this process.
    /// </summary>
    public static bool CoreLoaded => _coreLoaded;

    /// <summary>
    /// Load the bundled core unless one is already loaded. Idempotent, so a
    /// component that must run on a booted image — a library facade, a plugin —
    /// can call it without knowing whether the host booted dotcl first. Loading
    /// a core twice is not benign: the second pass redefines CL functions and
    /// signals "package COMMON-LISP is locked".
    /// </summary>
    public static void EnsureCore()
    {
        if (_coreLoaded) return;
        var core = FindCore()
            ?? throw new InvalidOperationException(
                "DotclHost.EnsureCore: no dotcl.core found next to the application. "
                + "A project referencing DotCL.Runtime gets one copied to its output; "
                + "otherwise pass an explicit path to LoadCore.");
        LoadCore(core);
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
    /// Intended for project-core deployments: the build target ships
    /// a manifest plus the listed FASLs into the app's asset directory; the
    /// host extracts them and calls this once after <see cref="LoadCore"/>
    /// to bring in all required contribs in dependency order.
    ///
    /// Loading is idempotent per entry: the core is loaded at most once per
    /// process, and a FASL whose module is already in <c>*MODULES*</c> is
    /// skipped. Several manifests can therefore be loaded in one process — an
    /// app's own plus one per referenced Lisp library — with the overlap (the
    /// core, shared contribs) paid for once. Re-loading the core is not benign:
    /// it redefines CL functions and signals "package COMMON-LISP is locked".
    ///
    /// Returns the number of FASLs loaded, not counting entries skipped as
    /// already loaded.
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

            // Module name is the filename without extension, lowercased —
            // matching the keyword/string normalization REQUIRE applies. The
            // base image is "dotcl" and is tracked by _coreLoaded rather than
            // *MODULES*: it is a core, not a library.
            var moduleName = System.IO.Path.GetFileNameWithoutExtension(fileName).ToLowerInvariant();
            var isCore = moduleName == "dotcl";

            if (isCore ? _coreLoaded : ModuleProvided(modulesSym, moduleName))
                continue;

            Runtime.Load(new LispObject[] { new LispString(resolved) });

            // Treat each loaded fasl as a "provided" module so a later
            // (require :foo) from user code doesn't trigger module-provide-
            // contrib's filesystem search (which would fail in deployment
            // where the contrib/ tree isn't shipped).
            if (isCore)
                _coreLoaded = true;
            else if (moduleName.Length > 0)
                DynamicBindings.Set(modulesSym,
                    new Cons(new LispString(moduleName), DynamicBindings.Get(modulesSym)));
            count++;
        }
        return count;
    }

    /// <summary>
    /// True if MODULENAME is already on <c>*MODULES*</c> — i.e. a manifest load
    /// or a REQUIRE has brought it in.
    /// </summary>
    private static bool ModuleProvided(Symbol modulesSym, string moduleName)
    {
        if (moduleName.Length == 0) return false;
        for (LispObject c = DynamicBindings.Get(modulesSym); c is Cons cc; c = cc.Cdr)
            if (cc.Car is LispString s && s.Value == moduleName) return true;
        return false;
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
    /// Resolve a function name a host passed in, for <see cref="Call"/>.
    ///
    /// "PKG:NAME" / "PKG::NAME" names a package explicitly and always wins.
    /// An unqualified name goes through the normal resolver first (CL and
    /// dotcl's own packages), and then — only if that found nothing callable —
    /// through every package that has an fbound symbol of that name. The last
    /// step is what makes a host call into a Lisp library work: the library's
    /// entry points live in the library's own package, which the internal
    /// name-based bridge deliberately does not search. Ambiguity is an error
    /// rather than a coin flip: the caller is told to qualify the name.
    /// </summary>
    private static Symbol ResolveCallable(string functionName)
    {
        var colon = functionName.IndexOf(':');
        if (colon > 0)
        {
            var pkgName = functionName[..colon];
            var symName = functionName[colon..].TrimStart(':');
            var pkg = Package.FindPackage(pkgName)
                ?? throw new InvalidOperationException(
                    $"DotclHost.Call: no package named {pkgName} (in \"{functionName}\")");
            var (qualified, qualifiedStatus) = pkg.FindSymbol(symName);
            if (qualifiedStatus == SymbolStatus.None)
                throw new InvalidOperationException(
                    $"DotclHost.Call: package {pkgName} has no symbol {symName}");
            return qualified;
        }

        var sym = Startup.SymFn(functionName);
        if (sym.Function != null) return sym;

        Symbol? found = null;
        List<string>? ambiguous = null;
        foreach (var pkg in Package.AllPackages)
        {
            var (candidate, status) = pkg.FindSymbol(functionName);
            // Inherited hits are the same symbol seen through a use-list; only
            // the home-ish statuses are considered so a symbol counts once.
            if (status != SymbolStatus.External && status != SymbolStatus.Internal) continue;
            if (candidate.Function == null) continue;
            if (found == null || ReferenceEquals(found, candidate)) { found = candidate; continue; }
            ambiguous ??= new List<string> { $"{found.HomePackage?.Name}::{functionName}" };
            ambiguous.Add($"{candidate.HomePackage?.Name}::{functionName}");
        }
        if (ambiguous != null)
            throw new InvalidOperationException(
                $"DotclHost.Call: {functionName} is ambiguous ({string.Join(", ", ambiguous)}); "
                + "name the package explicitly, e.g. \"PKG:NAME\"");
        return found ?? sym;
    }

    /// <summary>
    /// Call a Lisp function by name with .NET object arguments. The name may be
    /// package-qualified ("MYLIB:ENTRY"); an unqualified name resolves as
    /// described on <see cref="ResolveCallable"/>. Each arg is converted via
    /// <see cref="Runtime.DotNetToLisp"/>; the return is a
    /// <see cref="LispObject"/>. Use <see cref="LispString.Value"/> etc. to
    /// extract typed results.
    /// </summary>
    public static LispObject Call(string functionName, params object?[] args)
    {
        var sym = ResolveCallable(functionName);
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
    /// Build a Lisp LIST from a .NET sequence, converting each element with the
    /// same marshalling <see cref="Call"/> applies to arguments.
    ///
    /// Passing a .NET array or collection straight to <see cref="Call"/> hands
    /// the Lisp side a foreign object, not a sequence — deliberately, so a
    /// byte[] stays the same buffer. This is the explicit way to say "as a Lisp
    /// list", for calling a function that takes one sequence argument.
    /// </summary>
    public static LispObject ToLispList(System.Collections.IEnumerable items)
    {
        if (items is null) throw new ArgumentNullException(nameof(items));
        var elements = new List<LispObject>();
        foreach (var item in items) elements.Add(Runtime.DotNetToLisp(item));
        LispObject result = Nil.Instance;
        for (int i = elements.Count - 1; i >= 0; i--) result = new Cons(elements[i], result);
        return result;
    }

    /// <summary>
    /// Build a Lisp simple VECTOR from a .NET sequence. The vector counterpart
    /// of <see cref="ToLispList"/>.
    /// </summary>
    public static LispObject ToLispVector(System.Collections.IEnumerable items)
    {
        if (items is null) throw new ArgumentNullException(nameof(items));
        var elements = new List<LispObject>();
        foreach (var item in items) elements.Add(Runtime.DotNetToLisp(item));
        return new LispVector(elements.ToArray());
    }

    /// <summary>
    /// Convert a Lisp sequence — a list or a vector — to a .NET array, each
    /// element converted to <typeparamref name="T"/> as <see cref="ToClr{T}"/>
    /// does. NIL is the empty sequence, so it yields an empty array.
    /// </summary>
    public static T[] ToClrArray<T>(LispObject sequence) => ToClrList<T>(sequence).ToArray();

    /// <summary>
    /// List form of <see cref="ToClrArray{T}"/>.
    /// </summary>
    public static List<T> ToClrList<T>(LispObject sequence)
    {
        var result = new List<T>();
        switch (sequence)
        {
            case null:
            case Nil:
                return result;
            case LispVector v:
                for (int i = 0; i < v.Length; i++) result.Add(ToClr<T>(v.ElementAt(i)));
                return result;
            case Cons:
                for (LispObject c = sequence; c is Cons cc; c = cc.Cdr) result.Add(ToClr<T>(cc.Car));
                return result;
            default:
                throw new InvalidCastException(
                    $"DotclHost.ToClrList<{typeof(T).Name}>: not a Lisp list or vector: "
                    + sequence.GetType().Name);
        }
    }

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

    /// <summary>
    /// Bind <c>*debugger-hook*</c> so an unhandled condition throws back to the
    /// .NET caller instead of entering the interactive debugger. For
    /// non-interactive hosts (MSBuild tasks, servers) with no console to drive
    /// the debugger — otherwise a Lisp error stalls on "stdin closed". The thrown
    /// <see cref="InvalidOperationException"/> carries the condition type + message.
    /// </summary>
    public static void SetThrowingDebuggerHook()
    {
        var hookSym = Startup.Sym("*DEBUGGER-HOOK*");
        DynamicBindings.Set(hookSym, new LispFunction(a =>
        {
            var cond = a.Length > 0 ? a[0] : Nil.Instance;
            var msg = cond is LispCondition lc ? lc.Message : cond.ToString();
            var typeName = cond is LispCondition lc2 ? lc2.ConditionTypeName : "ERROR";
            throw new InvalidOperationException($"{typeName}: {msg}");
        }, "*NON-INTERACTIVE-DEBUGGER-HOOK*", 2));
    }

    // ── Project-core build (ASDF → fasl) ────────────────────────────────────
    // Shared by the `dotcl build` CLI subcommand (runtime/Program.cs) and the
    // MSBuild integration. Assumes Initialize() + LoadCore() have already run.
    // These throw on error (FileNotFoundException for a missing .asd); callers
    // map that to their own diagnostic (CLI: stderr+exit; MSBuild task: Log).

    /// <summary>
    /// Walk an ASDF system's <c>:depends-on</c> graph (dependency-first) and
    /// emit one fasl path per line in load order, excluding the root system.
    /// Output goes to <paramref name="manifestOut"/> (or stdout when null).
    /// Dep systems without a pre-built <c>&lt;name&gt;.fasl</c> are compiled on
    /// the fly via concatenate-source-op. When <paramref name="rootSourcesOut"/>
    /// is non-null, also writes the root system's component source paths in
    /// declared order (used by MSBuild as Inputs). <paramref name="targetRid"/>,
    /// when given, prefers <c>&lt;name&gt;-r2r-&lt;rid&gt;.fasl</c> if present.
    /// </summary>
    /// <summary>
    /// Load each user-supplied build-init script (the &lt;DotclBuildInit&gt; items)
    /// before dependency resolution. dotcl does NOT auto-scan ~/quicklisp etc.; a
    /// build that needs external systems makes them discoverable here — e.g. the
    /// script does (pushnew #p"…/foo/" asdf:*central-registry*) or boots quicklisp.
    /// Build-time only: the shipped runtime never runs these, so it can't end up
    /// depending on the dev machine's paths. Called after (require "asdf").
    /// </summary>
    private static void LoadBuildInitScripts(string[]? scripts)
    {
        if (scripts == null) return;
        foreach (var s in scripts)
        {
            if (string.IsNullOrWhiteSpace(s)) continue;
            var abs = System.IO.Path.GetFullPath(s.Trim());
            if (!System.IO.File.Exists(abs))
                throw new System.IO.FileNotFoundException(
                    $"DotclBuildInit script not found: {abs}", abs);
            var lisp = abs.Replace("\\", "/");
            Runtime.Eval(MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString($"(load \"{lisp}\")") })));
        }
    }

    /// <summary>
    /// Register each user-declared external system directory (the
    /// &lt;DotclAsdSearchPath&gt; items) onto <c>asdf:*central-registry*</c> so the
    /// project's <c>:depends-on</c> resolves systems that live outside the shipped
    /// contrib — without dotcl auto-scanning the dev machine. This is the
    /// declarative common case; &lt;DotclBuildInit&gt; remains the escape hatch for
    /// anything a plain dir list can't express (booting quicklisp, etc.). Like
    /// build-init, this runs at build time only and never in the shipped runtime.
    /// Called after (require "asdf"), before the build-init scripts.
    /// </summary>
    private static void RegisterAsdSearchPaths(string[]? dirs)
    {
        if (dirs == null) return;
        foreach (var d in dirs)
        {
            if (string.IsNullOrWhiteSpace(d)) continue;
            // A directory arg whose value ends in "\" gets a trailing quote
            // glued on by Windows command-line escaping (\" → literal "), since
            // the MSBuild Exec passes %(FullPath) of a dir (…\extlib\) quoted.
            // Strip the surrounding-quote artifact before resolving.
            var t = d.Trim().Trim('"');
            if (t.Length == 0) continue;
            var abs = System.IO.Path.GetFullPath(t).Replace("\\", "/");
            if (!abs.EndsWith("/")) abs += "/";
            Runtime.Eval(MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString(
                    $"(pushnew #p\"{abs}\" asdf:*central-registry* :test #'equal)") })));
        }
    }

    /// <summary>
    /// Route ASDF's compile output under <paramref name="cacheDir"/> (a dir
    /// inside the project's obj/) instead of the default user cache
    /// (~/.cache/common-lisp/…). ASDF caches each system's component fasls keyed
    /// by source path; that cache lives outside the project and survives
    /// `dotnet clean`, so a regenerated source can be shadowed by a stale cached
    /// fasl (dotcl/dotcl#53). Sending it under obj/ makes `dotnet clean` (which
    /// wipes obj/) clear it too — one project-local cache, no external trap. The
    /// source tree is mirrored under the dir so distinct sources never collide.
    /// Called after (require "asdf"), before any load/compile. MSBuild path only
    /// (the CLI keeps ASDF's default shared cache).
    /// </summary>
    private static void RedirectAsdfOutput(string? cacheDir)
    {
        if (string.IsNullOrEmpty(cacheDir)) return;
        var dir = System.IO.Path.GetFullPath(cacheDir).Replace("\\", "/").TrimEnd('/') + "/";
        var form = $"(asdf:initialize-output-translations "
                 + $"(list :output-translations "
                 + $"(list t (list #p\"{dir}\" :**/ :*.*.*)) "
                 + $":ignore-inherited-configuration))";
        Runtime.Eval(MultipleValues.Primary(
            Runtime.ReadFromString(new LispObject[] { new LispString(form) })));
    }

    public static void ResolveDeps(string asdPath, string? manifestOut, string? rootSourcesOut, string? targetRid = null, string[]? buildInit = null, string[]? searchPaths = null)
    {
        var absAsd = System.IO.Path.GetFullPath(asdPath);
        if (!System.IO.File.Exists(absAsd))
            throw new System.IO.FileNotFoundException($"resolve-deps: file not found: {absAsd}", absAsd);

        // Bring asdf in. (require "asdf") goes through module-provide-contrib
        // and side-effects *central-registry* with shipped contrib subdirs.
        Runtime.Eval(MultipleValues.Primary(
            Runtime.ReadFromString(new LispObject[] { new LispString("(require \"asdf\")") })));

        // MSBuild path (manifest to a file): route ASDF's compile cache under
        // obj/ so `dotnet clean` clears it (dotcl/dotcl#53). CLI resolve-deps to
        // stdout keeps ASDF's default shared cache.
        if (manifestOut != null)
        {
            var mDir = System.IO.Path.GetDirectoryName(System.IO.Path.GetFullPath(manifestOut));
            RedirectAsdfOutput(System.IO.Path.Combine(mDir ?? ".", "asdf-cache"));
        }

        // Declarative external system dirs (<DotclAsdSearchPath>), then the
        // build-init scripts (escape hatch, can override / do more).
        RegisterAsdSearchPaths(searchPaths);
        LoadBuildInitScripts(buildInit);

        var asdLisp = absAsd.Replace("\\", "/");
        var manifestForm = manifestOut == null
            ? "*standard-output*"
            : $"(open \"{manifestOut.Replace("\\", "/")}\" :direction :output :if-exists :supersede)";
        // Progress lines ("[resolve-deps] compiling X...") go to stdout in the
        // MSBuild path (manifest written to a file, so stdout is free), where
        // <Exec> shows them as ordinary build messages. PowerShell 5.1 wraps any
        // native-process *stderr* as a red NativeCommandError, so emitting
        // progress on stderr made a successful build look broken. When the
        // manifest itself goes to stdout (manifestOut == null), keep progress on
        // stderr to avoid corrupting the manifest stream.
        var progressStream = manifestOut == null ? "*error-output*" : "*standard-output*";
        var rootSourcesForm = rootSourcesOut == null
            ? "nil"
            : $"(open \"{rootSourcesOut.Replace("\\", "/")}\" :direction :output :if-exists :supersede)";
        // Project-based dep fasl cache (dotcl/dotcl#47): when a manifest path is given
        // (the MSBuild build), put on-the-fly-compiled dep fasls in a "deps/" subdir
        // next to the manifest — i.e. under obj/.../dotcl-fasl/ — instead of polluting
        // each dep's source dir. That makes them cleanable by `dotnet clean` (which wipes
        // obj/), at the cost of recompiling deps per project (the .NET obj/ model). The
        // CompileProject load step uses the same convention. A prebuilt -r2r-<rid> AOT
        // fasl shipped next to the dep source is still preferred read-only. Direct CLI
        // resolve-deps to stdout (manifestOut == null) keeps the old next-to-source cache.
        string? depCacheDir = null;
        if (manifestOut != null)
        {
            var manDir = System.IO.Path.GetDirectoryName(System.IO.Path.GetFullPath(manifestOut));
            depCacheDir = System.IO.Path.Combine(manDir ?? ".", "deps");
            System.IO.Directory.CreateDirectory(depCacheDir);
        }
        var depCacheLisp = depCacheDir == null ? null : depCacheDir.Replace("\\", "/").TrimEnd('/') + "/";
        // FASL path for a dep's on-the-fly build: cache dir (if set) else next to source.
        string DepFaslForm(string nameExpr) => depCacheLisp == null
            ? $"(concatenate 'string dir {nameExpr} \".fasl\")"
            : $"(concatenate 'string \"{depCacheLisp}\" {nameExpr} \".fasl\")";
        // For each dep system, if its fasl exists, use it. Otherwise
        // concatenate-source-op + compile-file the dep's :components into the dep
        // fasl on the fly. Empty :components (marker systems) are skipped silently.
        var form = $@"
(let* ((seen '()) (order '()))
  (labels ((walk (sys)
             (unless (member sys seen :test #'eq)
               (push sys seen)
               (dolist (d (asdf:system-depends-on sys))
                 ;; resolve-dependency-spec normalizes ASDF dependency specifiers
                 ;; ((:feature :dotcl ""x""), (:version ...), plain names) to a
                 ;; system, returning nil when a :feature condition is unmet. Using
                 ;; asdf:find-system directly returned nil for (:feature ...) forms,
                 ;; dropping those deps from the manifest (e.g. micros' dotcl-thread).
                 (let ((ds (ignore-errors (asdf/find-component:resolve-dependency-spec sys d))))
                   (when ds (walk ds))))
               (push sys order)))
           (ensure-fasl (sys)
             (let* ((src  (asdf:component-pathname sys))
                    (dir  (directory-namestring src))
                    (name (asdf:component-name sys))
                    (r2r-fasl {(targetRid == null
                        ? "nil"
                        : $"(concatenate 'string dir name \"-r2r-\" \"{targetRid}\" \".fasl\")")})
                    (fasl {DepFaslForm("name")}))
               (when (and r2r-fasl (probe-file r2r-fasl))
                 (return-from ensure-fasl r2r-fasl))
               (unless (probe-file fasl)
                 (when (asdf:component-children sys)
                   (format {progressStream}
                           ""[resolve-deps] compiling ~A...~%"" name)
                   (asdf:operate 'asdf::concatenate-source-op sys)
                   (let ((concat (first
                                  (asdf:output-files
                                   (asdf:make-operation 'asdf::concatenate-source-op)
                                   sys))))
                     ;; same concat compile-time-eval as CompileProject,
                     ;; for dependency systems built on the fly.
                     (dotcl.cil-compiler:compile-file-concatenated concat fasl))))
               fasl)))
    (asdf:load-asd ""{asdLisp}"")
    (let* ((root (asdf:find-system
                   (pathname-name (pathname ""{asdLisp}""))))
           (deps (remove root (nreverse (progn (walk root) order)))))
      (let ((stream {manifestForm}))
        (unwind-protect
          (dolist (sys deps)
            (when (asdf:component-children sys)
              (let ((fasl (ensure-fasl sys)))
                (format stream ""~A~%"" fasl))))
          (when {(manifestOut == null ? "nil" : "t")} (close stream))))
      (let ((rstream {rootSourcesForm}))
        (when rstream
          (unwind-protect
            (dolist (c (asdf:component-children root))
              (let ((p (asdf:component-pathname c)))
                (when p (format rstream ""~A~%"" (namestring p)))))
            (close rstream)))))))";
        Runtime.Eval(MultipleValues.Primary(
            Runtime.ReadFromString(new LispObject[] { new LispString(form) })));
    }

    /// <summary>
    /// Concatenate the root system's <c>:components</c> (declared order) via
    /// <c>asdf::concatenate-files</c> and <c>compile-file</c> the result into
    /// <paramref name="outputPath"/>. Only the root system is compiled;
    /// dependencies stay as pre-built fasls resolved by <see cref="ResolveDeps"/>.
    /// </summary>
    public static void CompileProject(string asdPath, string outputPath, string[]? buildInit = null, string[]? searchPaths = null, bool debugInfo = false)
    {
        var absAsd = System.IO.Path.GetFullPath(asdPath);
        if (!System.IO.File.Exists(absAsd))
            throw new System.IO.FileNotFoundException($"compile-project: file not found: {absAsd}", absAsd);
        var absOut = System.IO.Path.GetFullPath(outputPath);
        var outDir = System.IO.Path.GetDirectoryName(absOut);
        if (!string.IsNullOrEmpty(outDir) && !System.IO.Directory.Exists(outDir))
            System.IO.Directory.CreateDirectory(outDir);

        // Load dep fasls from the same project-based cache dir resolve-deps wrote them
        // to (dotcl/dotcl#47): "deps/" next to the output fasl, i.e. under obj/. Must
        // match ResolveDeps's DepFaslForm convention.
        var depCacheDir = System.IO.Path.Combine(outDir ?? ".", "deps");
        var depCacheLisp = depCacheDir.Replace("\\", "/").TrimEnd('/') + "/";

        // Non-interactive build: a compile-time error must NOT drop into the
        // interactive debugger (it loops on closed stdin and buries the message).
        // Bind *debugger-hook* to re-raise the condition so it unwinds to the
        // source-location wrap + MSBuild-canonical formatter (dotcl/dotcl#48).
        var hookSym = Startup.Sym("*DEBUGGER-HOOK*");
        var oldHook = DynamicBindings.Get(hookSym);
        DynamicBindings.Set(hookSym, new LispFunction(hookArgs =>
        {
            var cond = hookArgs[0];
            throw new LispErrorException(
                cond is LispCondition lc ? lc : new LispError(cond.ToString()));
        }, "*BUILD-DEBUGGER-HOOK*", 2));

        Runtime.Eval(MultipleValues.Primary(
            Runtime.ReadFromString(new LispObject[] { new LispString("(require \"asdf\")") })));

        // Route ASDF's compile cache under obj/ so `dotnet clean` clears it and a
        // stale user-cache fasl can't shadow a regenerated source (dotcl/dotcl#53).
        RedirectAsdfOutput(System.IO.Path.Combine(outDir ?? ".", "asdf-cache"));

        // Declarative external system dirs (<DotclAsdSearchPath>), then the
        // build-init scripts (escape hatch, can override / do more).
        RegisterAsdSearchPaths(searchPaths);
        LoadBuildInitScripts(buildInit);

        var asdLisp = absAsd.Replace("\\", "/");
        var outLisp = absOut.Replace("\\", "/");
        var concatLisp = (outDir == null ? "" : outDir.Replace("\\", "/") + "/")
                       + System.IO.Path.GetFileNameWithoutExtension(outputPath)
                       + ".concat.lisp";
        // Phase 1: load the asd, load the resolved :depends-on fasls, and
        // concatenate the root's sources into the concat file. Return the ordered
        // source namestrings so we can build a concat-line -> (file, line) map for
        // diagnostics (dotcl/dotcl#48).
        var setupForm = $@"
(progn
  (asdf:load-asd ""{asdLisp}"")
  (let* ((root (asdf:find-system
                 (pathname-name (pathname ""{asdLisp}""))))
         (sources (mapcar #'asdf:component-pathname
                          (asdf:component-children root))))
    ;; Load the resolved :depends-on fasls into the image BEFORE compiling the
    ;; root, so the deps' defpackage/macros are available at the root's compile
    ;; time — same as a standard ASDF load-op-then-compile. Without this the
    ;; root must itself (require :dep), because the concatenated unit holds only
    ;; the root's own sources. The dep fasls are the
    ;; ones resolve-deps built at the project deps/ cache dir, in topo order.
    (let ((seen '()) (order '()))
      (labels ((walk (sys)
                 (unless (member sys seen :test #'eq)
                   (push sys seen)
                   (dolist (d (asdf:system-depends-on sys))
                     ;; resolve-dependency-spec normalizes ASDF dependency specifiers
                 ;; ((:feature :dotcl ""x""), (:version ...), plain names) to a
                 ;; system, returning nil when a :feature condition is unmet. Using
                 ;; asdf:find-system directly returned nil for (:feature ...) forms,
                 ;; dropping those deps from the manifest (e.g. micros' dotcl-thread).
                 (let ((ds (ignore-errors (asdf/find-component:resolve-dependency-spec sys d))))
                       (when ds (walk ds))))
                   (push sys order))))
        (walk root))
      (dolist (sys (remove root (nreverse order)))
        (when (asdf:component-children sys)
          (let* ((name (asdf:component-name sys))
                 (fasl (concatenate 'string ""{depCacheLisp}"" name "".fasl"")))
            (when (probe-file fasl) (load fasl))))))
    (asdf::concatenate-files sources ""{concatLisp}"")
    (mapcar #'namestring sources)))";
        var sourcesResult = Runtime.Eval(MultipleValues.Primary(
            Runtime.ReadFromString(new LispObject[] { new LispString(setupForm) })));
        var sourcePaths = ListToStringArray(sourcesResult);
        var lineMap = BuildConcatLineMap(sourcePaths);

        // Progress trace (dotcl/dotcl#48 point 2): which files this build compiles,
        // in order — so a failing build shows what was processed before the error.
        System.Console.Error.WriteLine(
            $"[build] {System.IO.Path.GetFileNameWithoutExtension(absAsd)}: compiling {sourcePaths.Length} source(s)");
        foreach (var sp in sourcePaths)
            System.Console.Error.WriteLine($"[build]   {sp}");

        // Phase 2: compile the concatenated unit. compile-file-concatenated binds
        // *concatenate-build* (cross-compiled, so the binding shares symbol identity
        // with the compiler's read) so the compiler evaluates toplevel
        // require/use-package/load at compile time within the single concatenated
        // unit — restoring the compile+load interleaving a normal multi-file load-op
        // would have given the original :components.
        //
        // EmitBuildSourceLocations makes COMPILE-FILE attach the concat file + form
        // line to a compile error; we then remap that concat line back to the
        // original source file:line via lineMap (dotcl/dotcl#48).
        var compileForm =
            $@"(dotcl.cil-compiler:compile-file-concatenated ""{concatLisp}"" ""{outLisp}"")";
        var prevEmit = Runtime.EmitBuildSourceLocations;
        Runtime.EmitBuildSourceLocations = true;
        // Debug build: emit a Portable PDB from the project compile. For a
        // single-source project point the PDB document at the real .lisp (so F5
        // breaks in the user's source, not the generated concat unit); a
        // multi-source project keeps the concat until per-document mapping lands.
        var prevEmitPdb = Runtime.BuildEmitPdb;
        var prevDebugSrc = Runtime.BuildDebugSourceOverride;
        var prevLineMap = Runtime.BuildDebugLineMap;
        Runtime.BuildEmitPdb = debugInfo;
        // Single source: point the one document at the real .lisp. Multiple
        // sources: hand COMPILE-FILE the concat line map so it emits one document
        // per file and each .lisp gets its own breakpoints (lineMap already built
        // above for error remapping).
        Runtime.BuildDebugSourceOverride =
            debugInfo && sourcePaths.Length == 1 ? sourcePaths[0] : null;
        Runtime.BuildDebugLineMap =
            debugInfo && sourcePaths.Length > 1 ? lineMap : null;
        try
        {
            Runtime.Eval(MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString(compileForm) })));
        }
        catch (LispSourceException lse)
        {
            throw RemapConcatException(lse, concatLisp, lineMap);
        }
        finally
        {
            Runtime.EmitBuildSourceLocations = prevEmit;
            Runtime.BuildEmitPdb = prevEmitPdb;
            Runtime.BuildDebugSourceOverride = prevDebugSrc;
            Runtime.BuildDebugLineMap = prevLineMap;
            DynamicBindings.Set(hookSym, oldHook);
        }
    }

    /// <summary>
    /// Build a single self-contained FASL for <c>dotcl pack</c>: monolithic-
    /// concatenate the named ASDF system — its root sources AND all dependency
    /// sources — via <c>asdf:monolithic-concatenate-source-op</c>, then
    /// <c>compile-file-concatenated</c> the result into
    /// <paramref name="outputFasl"/>. Unlike <see cref="CompileProject"/> (root
    /// only, deps stay as separate fasls) the produced FASL loads standalone, so
    /// the pack restamp can drop it into the tool package as a single
    /// dotcl.user.fasl with no dep fasls to bundle.
    ///
    /// When <paramref name="toplevel"/> is non-null a call <c>(toplevel)</c> is
    /// appended so the tool runs that entry point on launch. A system that
    /// already invokes its entry at load time (e.g. a roswell <c>&lt;name&gt;/exe</c>
    /// launcher) needs none.
    /// </summary>
    public static void PackFasl(string system, string outputFasl, string? toplevel = null,
                                string[]? buildInit = null, string[]? searchPaths = null)
    {
        var absOut = System.IO.Path.GetFullPath(outputFasl);
        var outDir = System.IO.Path.GetDirectoryName(absOut);
        if (!string.IsNullOrEmpty(outDir) && !System.IO.Directory.Exists(outDir))
            System.IO.Directory.CreateDirectory(outDir);

        // Non-interactive: a compile-time error must unwind, not drop into the
        // debugger on closed stdin (same rationale as CompileProject).
        var hookSym = Startup.Sym("*DEBUGGER-HOOK*");
        var oldHook = DynamicBindings.Get(hookSym);
        DynamicBindings.Set(hookSym, new LispFunction(hookArgs =>
        {
            var cond = hookArgs[0];
            throw new LispErrorException(
                cond is LispCondition lc ? lc : new LispError(cond.ToString()));
        }, "*PACK-DEBUGGER-HOOK*", 2));

        Runtime.Eval(MultipleValues.Primary(
            Runtime.ReadFromString(new LispObject[] { new LispString("(require \"asdf\")") })));
        RegisterAsdSearchPaths(searchPaths);
        LoadBuildInitScripts(buildInit);

        var outLisp = absOut.Replace("\\", "/");
        var sysEsc = system.Replace("\\", "\\\\").Replace("\"", "\\\"");
        var workConcat = (outDir == null ? "" : outDir.Replace("\\", "/") + "/")
                       + System.IO.Path.GetFileNameWithoutExtension(outputFasl) + ".pack.concat.lisp";
        // Launcher appended only when --toplevel was given.
        var appendForm = toplevel == null ? "" :
            $@"(with-open-file (o work :direction :output :if-exists :append :if-does-not-exist :error)
                 (terpri o) (write-line ""({toplevel})"" o))";

        // monolithic-concatenate-source-op writes the concat into asdf's shared
        // cache; copy it to a writable work file next to the output (keep the
        // cached one pristine), append the optional launcher, then compile.
        var form = $@"
(let* ((sys (asdf:find-system ""{sysEsc}""))
       (op 'asdf:monolithic-concatenate-source-op))
  (asdf:operate op sys)
  (let ((concat (namestring (first (asdf:output-files op sys))))
        (work ""{workConcat}""))
    (with-open-file (o work :direction :output :if-exists :supersede :if-does-not-exist :create)
      (with-open-file (i concat)
        (loop for line = (read-line i nil :eof) until (eq line :eof)
              do (write-line line o))))
    {appendForm}
    (dotcl.cil-compiler:compile-file-concatenated work ""{outLisp}"")))";

        var prevEmit = Runtime.EmitBuildSourceLocations;
        Runtime.EmitBuildSourceLocations = true;
        try
        {
            Runtime.Eval(MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString(form) })));
        }
        finally
        {
            Runtime.EmitBuildSourceLocations = prevEmit;
            DynamicBindings.Set(hookSym, oldHook);
        }
    }

    /// <summary>
    /// Metadata read off an ASDF system definition, used to fill in nuspec
    /// fields for `dotcl pack`. Every field is null when the .asd omits it.
    /// </summary>
    public sealed class SystemMeta
    {
        public string? Description;
        public string? Homepage;
        public string? SourceControlUrl;
        public string? Author;
        public string? License;
        public string? AsdDirectory;   // where to look for a sibling README
    }

    /// <summary>
    /// Read the standard metadata slots off an ASDF system. `dotcl pack` uses
    /// these as nuspec defaults so a packed tool describes itself rather than
    /// inheriting the description and URLs of the dotcl packages it was
    /// restamped from. Returns a SystemMeta whose fields are null where the .asd
    /// is silent; returns null if the system cannot be found at all (packing
    /// proceeds — the fasl build reports a missing system with a better error).
    /// </summary>
    public static SystemMeta? ReadSystemMeta(string system, string[]? searchPaths = null)
    {
        try
        {
            Runtime.Eval(MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString("(require \"asdf\")") })));
            RegisterAsdSearchPaths(searchPaths);

            var sysEsc = system.Replace("\\", "\\\\").Replace("\"", "\\\"");
            // :source-control is (:git "url") / (:github "url") / a bare string.
            // Normalize to the url alone here so the C# side stays shapeless.
            var form = $@"
(let* ((sys (asdf:find-system ""{sysEsc}""))
       (sc (asdf:system-source-control sys))
       (asd (asdf:system-source-file sys)))
  (list (asdf:system-description sys)
        (asdf:system-homepage sys)
        (cond ((stringp sc) sc)
              ((and (consp sc) (stringp (second sc))) (second sc))
              ((and (consp sc) (stringp (cdr sc))) (cdr sc)))
        (asdf:system-author sys)
        (asdf:system-license sys)
        (and asd (namestring (make-pathname :name nil :type nil :defaults asd)))))";
            var result = MultipleValues.Primary(Runtime.Eval(MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString(form) }))));

            var items = new List<string?>();
            var cur = result;
            while (cur is Cons c)
            {
                items.Add(c.Car is LispString s && s.Value.Length > 0 ? s.Value : null);
                cur = c.Cdr;
            }
            while (items.Count < 6) items.Add(null);
            return new SystemMeta
            {
                Description = items[0],
                Homepage = items[1],
                SourceControlUrl = items[2],
                Author = items[3],
                License = items[4],
                AsdDirectory = items[5],
            };
        }
        catch
        {
            // Metadata is best-effort: never fail a pack because a .asd omits
            // slots or uses a shape we do not recognize.
            return null;
        }
    }

    /// Walk a proper Lisp list of LispStrings into a C# string[].
    private static string[] ListToStringArray(LispObject list)
    {
        var result = new System.Collections.Generic.List<string>();
        var cur = list;
        while (cur is Cons c)
        {
            if (c.Car is LispString s) result.Add(s.Value);
            cur = c.Cdr;
        }
        return result.ToArray();
    }

    /// Build a concat-line -> source map. asdf::concatenate-files joins the raw
    /// bytes of each source with no separators, so source file k begins at concat
    /// line (1 + total newlines in files 0..k-1). Returns entries sorted by start
    /// line so a concat line L maps to the last entry with startLine &lt;= L.
    private static (int startLine, string path)[] BuildConcatLineMap(string[] sourcePaths)
    {
        var map = new (int, string)[sourcePaths.Length];
        int start = 1;
        for (int i = 0; i < sourcePaths.Length; i++)
        {
            map[i] = (start, sourcePaths[i]);
            int newlines = 0;
            try
            {
                foreach (var b in System.IO.File.ReadAllBytes(sourcePaths[i]))
                    if (b == (byte)'\n') newlines++;
            }
            catch { /* unreadable source — leave start where it is */ }
            start += newlines;
        }
        return map;
    }

    /// Remap a LispSourceException pointing into the concatenated unit back to the
    /// original source file:line. Other (already-original) frames pass through.
    private static LispSourceException RemapConcatException(
        LispSourceException lse, string concatPath,
        (int startLine, string path)[] lineMap)
    {
        string concatFull;
        try { concatFull = System.IO.Path.GetFullPath(concatPath); }
        catch { concatFull = concatPath; }

        bool SameAsConcat(string f)
        {
            try { return string.Equals(System.IO.Path.GetFullPath(f), concatFull,
                System.StringComparison.OrdinalIgnoreCase); }
            catch { return false; }
        }
        if (!SameAsConcat(lse.FilePath) || lineMap.Length == 0)
            return lse;

        // Find the source file whose span contains the concat line.
        int concatLine = lse.Line;
        int idx = 0;
        for (int i = 0; i < lineMap.Length; i++)
            if (lineMap[i].startLine <= concatLine) idx = i; else break;
        var origPath = lineMap[idx].path;
        var origLine = concatLine - lineMap[idx].startLine + 1;
        return new LispSourceException(origPath, origLine, lse.InnerException!);
    }
}
