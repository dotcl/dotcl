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
    // Volatile: EnsureCoreLoaded reads it outside the lock on its fast path.
    private static volatile bool _coreLoaded;
    // Held while EnsureCore finds and loads the core, so that threads calling it
    // at the same time load it once. Separate from _initLock: a core load is long
    // and has nothing to do with the bootstrap that lock protects.
    private static readonly object _coreLock = new object();
    private static readonly object _initLock = new object();
    private static int _initializeCount;

    /// <summary>
    /// Bootstraps the Lisp runtime (packages, readtable, core functions).
    /// Safe to call multiple times and from several threads at once; only the
    /// first call does work.
    ///
    /// The lock is what makes the second sentence true. A host with more than
    /// one entry point into Lisp (a web request, a game callback, a plugin)
    /// reaches this from whichever thread arrives first, and an unguarded
    /// check-then-set let two of them into the bootstrap together. Monitor is
    /// re-entrant, so a nested Initialize on the same thread still passes.
    /// </summary>
    public static void Initialize()
    {
        lock (_initLock)
        {
            if (_initialized) return;
            Startup.Initialize();
            // Only after it returns: a bootstrap that threw has not happened,
            // and the next caller must be allowed to try again.
            _initializeCount++;
            _initialized = true;
        }
    }

    /// <summary>
    /// How many times <see cref="Initialize"/> has actually run the bootstrap in
    /// this process. 0 before the first call, 1 afterwards however many threads
    /// called it. Hosts do not need this; it is what lets a test say that
    /// concurrent callers bootstrapped once rather than merely not crashing.
    /// </summary>
    public static int InitializeCount => _initializeCount;

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
        catch { return null; }   // any reflection/IO failure -> fall back to file probes
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
        bool isPeImage;
        byte[] header = new byte[2];
        using (var fs = System.IO.File.OpenRead(filePath))
        {
            int n = fs.Read(header, 0, 2);
            isPeImage = n >= 2 && header[0] == 0x4D && header[1] == 0x5A;
        }

        if (isPeImage)
            LoadCoreFasl(filePath);
        else
            RunCoreSil(System.IO.File.ReadAllText(filePath), filePath);

        // Last, not first: a load that threw has not loaded a core, and
        // CoreLoaded saying otherwise turns EnsureCore into a no-op that leaves
        // the host on an image that was never booted.
        _coreLoaded = true;
    }

    /// <summary>
    /// Load and execute a compiled core already in memory. Same two formats as
    /// <see cref="LoadCore(string)"/>, a FASL PE assembly (the "MZ" header) or SIL
    /// text, for a host with no filesystem to read from. A browser fetches the core
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

        if (coreImage.Length >= 2 && coreImage[0] == 0x4D && coreImage[1] == 0x5A)
            RunCoreModuleInit(System.Reflection.Assembly.Load(coreImage), "FASL core (in memory)");
        else
            RunCoreSil(System.Text.Encoding.UTF8.GetString(coreImage), "core (in memory)");

        // Set after the load succeeds, for the reason LoadCore(string) does.
        _coreLoaded = true;
    }

    private static void LoadCoreFasl(string filePath)
        => RunCoreModuleInit(System.Reflection.Assembly.LoadFrom(filePath), $"FASL core {filePath}");

    /// <summary>Call a loaded core assembly's CompiledModule.ModuleInit, restoring
    /// *PACKAGE* afterwards. WHAT names the core in errors.</summary>
    private static void RunCoreModuleInit(System.Reflection.Assembly asm, string what)
    {
        // Same reason as Program.RunCoreFasl: the generation stamp is read off the
        // core assembly, so an embedding host must record it too: including the
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
        if (!Reader.TryReadSilCore(source, out var instrList))
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
    /// already-baked-in compiled module by its stable assembly NAME: the
    /// <c>:module-name</c> passed to <c>compile-file</c> / <c>dotcl:sil-to-fasl</c>,
    /// which must equal the referenced file's base name: and run its
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
    /// component that must run on a booted image, a library facade, a plugin,
    /// can call it without knowing whether the host booted dotcl first. Loading
    /// a core twice is not benign: the second pass redefines CL functions and
    /// signals "package COMMON-LISP is locked".
    /// </summary>
    public static void EnsureCore()
    {
        // Initialize first. It is idempotent and everything below needs the
        // symbol table; without it the core load reached Startup.Sym on an
        // uninitialised runtime and died with a NullReferenceException that
        // named nothing.
        Initialize();
        EnsureCoreLoaded();
    }

    private static void EnsureCoreLoaded()
    {
        if (_coreLoaded) return;
        // Several components may make sure of the core from their own threads at
        // once. Without the lock each saw no core yet and loaded one: the loads
        // corrupted each other's collections, and a second load alone fails on the
        // locked COMMON-LISP package.
        lock (_coreLock)
        {
            if (_coreLoaded) return;
            var core = FindCore()
                ?? throw new InvalidOperationException(
                    "DotclHost.EnsureCore: no dotcl.core found next to the application. "
                    + "A project referencing DotCL.Runtime gets one copied to its output; "
                    + "otherwise pass an explicit path to LoadCore.");
            LoadCore(core);
        }
    }

    /// <summary>
    /// Load and evaluate a Lisp source file. Same semantics as CL LOAD.
    /// </summary>
    public static void LoadLispFile(string path)
    {
        HostEntry(() => Runtime.Load(new LispObject[] { new LispString(path) }));
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
    /// skipped. Several manifests can therefore be loaded in one process, an
    /// app's own plus one per referenced Lisp library, with the overlap (the
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

            // Module name is the filename without extension, lowercased;
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
    /// True if MODULENAME is already on <c>*MODULES*</c>: i.e. a manifest load
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
    /// The result is the primary value of the last form, as a single-value
    /// position in Lisp sees it (NIL when the form returns no values); use
    /// <see cref="EvalStringMv"/> for every value.
    /// </summary>
    public static LispObject EvalString(string source)
    {
        return HostEntry(() =>
        {
            var reader = new Reader(new System.IO.StringReader(source));
            LispObject last = Nil.Instance;
            while (reader.TryRead(out var form))
                last = Runtime.Eval(form);
            return PrimaryOf(last);
        });
    }

    /// <summary>
    /// The package an unqualified name is resolved in, by name. Reads and writes
    /// the same <c>*PACKAGE*</c> the Lisp side sees, so a host can steer it
    /// without going through <c>(in-package ...)</c> as a string. Setting it to
    /// a package that does not exist is an error rather than a silent no-op.
    /// </summary>
    public static string CurrentPackage
    {
        get
        {
            Initialize();
            return DynamicBindings.Get(Startup.Sym("*PACKAGE*")) is Package p
                ? p.Name : "COMMON-LISP-USER";
        }
        set
        {
            Initialize();
            var name = ReadHostName(value, "CurrentPackage");
            if (name.Package != null)
                throw new InvalidOperationException(
                    $"DotclHost.CurrentPackage: \"{value}\" is a qualified symbol, not a package name");
            var pkg = Package.FindPackage(name.Name)
                ?? throw new InvalidOperationException(
                    $"DotclHost.CurrentPackage: no package named {name.Name}"
                    + SpellingHint(value, name.Name, true));
            DynamicBindings.Set(Startup.Sym("*PACKAGE*"), pkg);
        }
    }

    /// <summary>
    /// A name as a host wrote it, read the way the Lisp reader reads a symbol
    /// token: PACKAGE is the package name (null when unqualified, "KEYWORD" for a
    /// leading colon), NAME the symbol name, INTERNAL whether "::" was written.
    /// </summary>
    private readonly struct HostName
    {
        public readonly string? Package;
        public readonly string Name;
        public readonly bool Internal;
        public HostName(string? package, string name, bool isInternal)
        { Package = package; Name = name; Internal = isInternal; }
    }

    private static LispReadtable? CurrentReadtable()
        => DynamicBindings.TryGet(Startup.Sym("*READTABLE*"), out var rt) ? rt as LispReadtable : null;

    /// <summary>
    /// Read TEXT as the reader reads a symbol token: unescaped characters follow
    /// the current readtable's case (upcased under the standard readtable),
    /// "|...|" and "\" escape, and "PKG:NAME" / "PKG::NAME" qualify, the
    /// package name read by the same rules. The text is only parsed: nothing is
    /// interned, and a token the reader would take as a number is still a name.
    /// </summary>
    private static HostName ReadHostName(string text, string api)
    {
        if (text is null) throw new ArgumentNullException(nameof(text));
        var rt = CurrentReadtable();
        var readCase = rt?.Case ?? ReadtableCase.Upcase;
        var chars = new List<(char ch, bool escaped)>();
        int split = -1;           // index into chars where the symbol name starts
        int markers = 0;
        bool lastWasMarker = false;
        bool inBar = false;
        Exception Bad(string why) => new InvalidOperationException(
            $"DotclHost.{api}: \"{text}\" is not a symbol name: {why}");
        for (int i = 0; i < text.Length; i++)
        {
            char c = text[i];
            if (inBar)
            {
                if (c == '|') inBar = false;
                else if (c == '\\')
                {
                    if (++i >= text.Length) throw Bad("it ends in an escape");
                    chars.Add((text[i], true));
                }
                else chars.Add((c, true));
                lastWasMarker = false;
                continue;
            }
            if (c == '|') { inBar = true; lastWasMarker = false; continue; }
            if (c == '\\')
            {
                if (++i >= text.Length) throw Bad("it ends in an escape");
                chars.Add((text[i], true));
                lastWasMarker = false;
                continue;
            }
            if (c == ':')
            {
                if (markers == 0) { split = chars.Count; markers = 1; }
                else if (markers == 1 && lastWasMarker) markers = 2;
                else throw Bad("too many package markers");
                lastWasMarker = true;
                continue;
            }
            chars.Add((readCase == ReadtableCase.Invert || rt == null ? c : rt.ApplyCase(c), false));
            if (rt == null && readCase == ReadtableCase.Upcase)
                chars[^1] = (char.ToUpperInvariant(c), false);
            lastWasMarker = false;
        }
        if (inBar) throw Bad("a \"|\" is not closed");
        if (readCase == ReadtableCase.Invert)
        {
            bool upper = false, lower = false;
            foreach (var (ch, esc) in chars)
                if (!esc && char.IsLetter(ch)) { if (char.IsUpper(ch)) upper = true; else lower = true; }
            if (upper != lower)
                for (int i = 0; i < chars.Count; i++)
                    if (!chars[i].escaped && char.IsLetter(chars[i].ch))
                        chars[i] = (upper ? char.ToLowerInvariant(chars[i].ch)
                                          : char.ToUpperInvariant(chars[i].ch), false);
        }
        string Text(int from, int to)
        {
            var sb = new System.Text.StringBuilder(to - from);
            for (int i = from; i < to; i++) sb.Append(chars[i].ch);
            return sb.ToString();
        }
        if (markers == 0)
        {
            if (chars.Count == 0) throw Bad("it is empty");
            return new HostName(null, Text(0, chars.Count), false);
        }
        if (split == chars.Count) throw Bad("there is no name after the package marker");
        var package = split == 0 ? "KEYWORD" : Text(0, split);
        return new HostName(package, Text(split, chars.Count), markers == 2 || split == 0);
    }

    /// <summary>
    /// How a host writes NAME so that <see cref="ReadHostName"/> reads it back
    /// as exactly NAME: as is when the readtable leaves it alone, else in bars.
    /// </summary>
    private static string HostSpelling(string name)
    {
        bool plain = name.Length > 0;
        foreach (var c in name)
            if (c == '|' || c == '\\' || c == ':' || char.IsWhiteSpace(c)) { plain = false; break; }
        if (plain)
        {
            try { if (ReadHostName(name, "").Name == name && ReadHostName(name, "").Package == null) return name; }
            catch (InvalidOperationException) { }
        }
        return "|" + name.Replace("\\", "\\\\").Replace("|", "\\|") + "|";
    }

    /// <summary>
    /// Appended to a "not found" message: what the host's text was read as, when
    /// that differs from what was written, and a symbol or package whose name
    /// differs from it only in case, with the spelling that reaches it.
    /// </summary>
    private static string SpellingHint(string written, string readName, bool isPackage)
    {
        var sb = new System.Text.StringBuilder();
        if (written != readName) sb.Append($" (\"{written}\" reads as {readName})");
        var alternatives = new List<string>();
        if (isPackage)
        {
            foreach (var pkg in Package.AllPackages)
                if (pkg.Name != readName && string.Equals(pkg.Name, readName, StringComparison.OrdinalIgnoreCase)
                    && !alternatives.Contains(pkg.Name))
                    alternatives.Add(pkg.Name);
        }
        else
        {
            foreach (var pkg in Package.AllPackages)
                foreach (var sym in pkg.ExternalSymbols.Concat(pkg.InternalSymbols))
                    if (sym.Name != readName
                        && string.Equals(sym.Name, readName, StringComparison.OrdinalIgnoreCase)
                        && !alternatives.Contains(sym.Name))
                        alternatives.Add(sym.Name);
        }
        if (alternatives.Count > 0)
            sb.Append(" -- names are read as the Lisp reader reads them; ")
              .Append(string.Join(", ", alternatives.Select(n => $"{n} is written \"{HostSpelling(n)}\"")));
        return sb.ToString();
    }

    /// <summary>
    /// The package a host-written name is qualified with, or the error saying
    /// which package it was read as.
    /// </summary>
    private static Package HostPackage(HostName name, string text, string api)
        => Package.FindPackage(name.Package!)
           ?? throw new InvalidOperationException(
               $"DotclHost.{api}: no package named {name.Package} (in \"{text}\")"
               + SpellingHint(text[..Math.Max(0, text.IndexOf(':'))], name.Package!, true));

    /// <summary>
    /// The symbol a qualified host name names: one colon reaches the exported
    /// surface, two reach everything, as in the reader.
    /// </summary>
    private static Symbol QualifiedSymbol(HostName name, string text, string api)
    {
        var pkg = HostPackage(name, text, api);
        var (sym, status) = pkg.FindSymbol(name.Name);
        if (status == SymbolStatus.None)
            throw new InvalidOperationException(
                $"DotclHost.{api}: package {pkg.Name} has no symbol {name.Name} (in \"{text}\")"
                + SpellingHint(text[(text.LastIndexOf(':') + 1)..], name.Name, false));
        if (!name.Internal && status != SymbolStatus.External)
            throw new InvalidOperationException(
                $"DotclHost.{api}: {pkg.Name} does not export {name.Name}; "
                + $"write \"{HostSpelling(pkg.Name)}::{HostSpelling(name.Name)}\" to reach it anyway");
        return sym;
    }

    /// <summary>
    /// Resolve a function name a host passed in, for <see cref="Call"/>.
    ///
    /// The string is read the way the Lisp reader reads a symbol (see
    /// <see cref="ReadHostName"/>), so "fact" names what (defun fact ...)
    /// defined, "|fact|" a lowercase symbol, and "mylib:entry" an external
    /// symbol of MYLIB. One rule, the reader's, so a string has one meaning.
    ///
    /// An unqualified name is resolved in <see cref="CurrentPackage"/>, exactly
    /// as the reader would resolve it there -- inherited symbols included. It is
    /// NOT searched for across every package: that made a working call start
    /// failing as ambiguous the day an unrelated library defined the same name,
    /// and hid which package had answered. When resolution fails, the packages
    /// that do have such a function are named in the error, so the convenience
    /// survives as a diagnostic instead of as a rule.
    /// </summary>
    private static Symbol ResolveCallable(string functionName, string api = "Call")
    {
        var name = ReadHostName(functionName, api);
        if (name.Package != null) return QualifiedSymbol(name, functionName, api);

        var current = DynamicBindings.Get(Startup.Sym("*PACKAGE*")) as Package;
        if (current != null)
        {
            var (sym, status) = current.FindSymbol(name.Name);
            if (status != SymbolStatus.None && sym.Function != null) return sym;
        }
        throw new InvalidOperationException(
            $"DotclHost.{api}: no function named {name.Name} in "
            + $"{current?.Name ?? "COMMON-LISP-USER"}{SpellingHint(functionName, name.Name, false)}"
            + ElsewhereHint(name.Name));
    }

    /// <summary>Packages that do own a function of this exact name, for the
    /// "not found here" message. Resolution does not consult them.</summary>
    private static string ElsewhereHint(string symbolName)
    {
        var owners = new List<string>();
        foreach (var pkg in Package.AllPackages)
        {
            var (candidate, status) = pkg.FindSymbol(symbolName);
            if (status != SymbolStatus.External && status != SymbolStatus.Internal) continue;
            if (candidate.Function == null) continue;
            var home = candidate.HomePackage?.Name ?? pkg.Name;
            if (!owners.Contains(home)) owners.Add(home);
        }
        if (owners.Count == 0) return "";
        return $"; defined in {string.Join(", ", owners)} -- write "
             + $"\"{HostSpelling(owners[0])}:{HostSpelling(symbolName)}\" or set DotclHost.CurrentPackage";
    }

    /// <summary>
    /// Call a Lisp function by name with .NET object arguments. The name is read
    /// as the Lisp reader reads a symbol ("fact", "|lower|", "mylib:entry");
    /// an unqualified name resolves as described on <see cref="ResolveCallable"/>.
    /// Each arg is converted via <see cref="Runtime.DotNetToLisp"/>; the return is
    /// the function's primary value as a <see cref="LispObject"/>, the way a
    /// single-value position in Lisp receives it: (floor 7 2) gives the Fixnum 3,
    /// and a function returning no values gives NIL. Use <see cref="CallMv"/> for
    /// every value, and <see cref="LispString.Value"/> etc. to extract typed results.
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
        return HostEntry(() => PrimaryOf(fn.Invoke(lispArgs)));
    }

    /// <summary>
    /// <see cref="Call"/> keeping every value the function returned, not just the
    /// primary one. A Lisp function returns as many values as it likes -- FLOOR
    /// returns two, GETHASH returns the value and whether it was present, and a
    /// host that only ever sees the first cannot tell "absent" from "present and
    /// NIL". The array is the values in order; a function returning no values at
    /// all gives an empty array, and the ordinary single-value case gives one
    /// element (never null).
    /// </summary>
    public static LispObject[] CallMv(string functionName, params object?[] args)
    {
        var sym = ResolveCallable(functionName, "CallMv");
        if (sym.Function is not LispFunction fn)
            throw new InvalidOperationException(
                $"DotclHost.CallMv: symbol {functionName} has no function binding");
        var lispArgs = new LispObject[args.Length];
        for (int i = 0; i < args.Length; i++)
            lispArgs[i] = Runtime.DotNetToLisp(args[i]);
        // Clear the channel first, so "did the callee publish?" is a question
        // about THIS call. Without it a previous (values) is still current and a
        // function that returns one value the ordinary way looks like it returned
        // none -- the same trap the compiled call sequence avoids the same way.
        MultipleValues.Reset();
        return HostEntry(() => ValuesOf(fn.Invoke(lispArgs)));
    }

    /// <summary>
    /// <see cref="EvalString"/> keeping every value of the LAST form, for the
    /// same reason as <see cref="CallMv"/>. Earlier forms are evaluated for
    /// effect, exactly as EvalString does.
    /// </summary>
    public static LispObject[] EvalStringMv(string source)
    {
        return HostEntry(() =>
        {
            var reader = new Reader(new System.IO.StringReader(source));
            LispObject last = Nil.Instance;
            MultipleValues.Reset();
            while (reader.TryRead(out var form))
            {
                MultipleValues.Reset();
                last = Runtime.Eval(form);
            }
            return ValuesOf(last);
        });
    }

    /// <summary>
    /// The values a call produced, given its primary result. The rule lives in
    /// <see cref="MultipleValues.Of"/>, which the REPL's history variables read
    /// as well; the callers here supply the RESET it requires.
    /// </summary>
    private static LispObject[] ValuesOf(LispObject primary)
        => MultipleValues.Of(primary);

    /// <summary>
    /// The value a single-value receiver takes from a call's result. A call that
    /// returned several values (or none) hands back a wrapper for them, and that
    /// wrapper is opened: its first value, or NIL when there are none. Anything
    /// else IS the value. The thread's values channel is not read: after a
    /// non-local exit (a HANDLER-CASE clause, under the interpreter) it can still
    /// hold what an inner form published, and the clause's own value is the result.
    /// </summary>
    private static LispObject PrimaryOf(LispObject result)
    {
        if (result is not MvReturn mv) return result;
        var values = mv.ToArray();
        return values.Length > 0 ? values[0] : Nil.Instance;
    }

    /// <summary>
    /// The value of a special variable, by name. Resolution follows the same
    /// rule as <see cref="Call"/>: "*FOO*" in <see cref="CurrentPackage"/>,
    /// "PKG:*FOO*" for an exported one, "PKG::*FOO*" to reach an internal one.
    /// The name is read as the Lisp reader reads a symbol, so "*foo*" and
    /// "*FOO*" are the same variable under the standard readtable.
    ///
    /// This is the general form of <see cref="CurrentPackage"/>, which stays as
    /// the convenience for the one variable every host touches.
    /// </summary>
    public static LispObject GetSpecial(string variableName)
    {
        Initialize();
        var sym = ResolveVariable(variableName, "GetSpecial");
        if (!DynamicBindings.TryGet(sym, out var value))
            throw new InvalidOperationException(
                $"DotclHost.GetSpecial: {variableName} is unbound");
        return value;
    }

    /// <summary>
    /// Set a special variable by name, converting VALUE with
    /// <see cref="Runtime.DotNetToLisp"/> the way <see cref="Call"/> converts
    /// arguments. Pass a <see cref="LispObject"/> to set it exactly.
    ///
    /// This assigns the CURRENT binding, which is what a host wants: it is the
    /// global one unless the host is called back from inside a LET of that
    /// variable, and then assigning the innermost binding is the CL meaning of
    /// SETQ anyway.
    /// </summary>
    public static void SetSpecial(string variableName, object? value)
    {
        Initialize();
        var sym = ResolveVariable(variableName, "SetSpecial");
        DynamicBindings.Set(sym, value is LispObject lo ? lo : Runtime.DotNetToLisp(value));
    }

    /// <summary>
    /// Resolve a variable name the way <see cref="ResolveCallable"/> resolves a
    /// function name, minus the requirement that it name a function. An
    /// unqualified name that names nothing yet is interned in the current
    /// package, so SetSpecial can create a variable the Lisp side then reads --
    /// which is the point of having a setter at all.
    /// </summary>
    private static Symbol ResolveVariable(string variableName, string api)
    {
        var name = ReadHostName(variableName, api);
        if (name.Package != null) return QualifiedSymbol(name, variableName, api);
        var current = DynamicBindings.Get(Startup.Sym("*PACKAGE*")) as Package;
        if (current != null) return current.Intern(name.Name).symbol;
        return Startup.Sym(name.Name);
    }

    /// <summary>
    /// Send what Lisp writes to <c>*STANDARD-OUTPUT*</c> to WRITER. A host that
    /// embeds Lisp usually has somewhere of its own for output -- a log, a text
    /// box, a test buffer -- and without this the only way there was to write
    /// Lisp code that bound the stream itself.
    ///
    /// Passing null restores the process's own standard output. The writer is
    /// used as given: the caller owns it, including flushing and disposal.
    /// </summary>
    public static void SetStandardOutput(System.IO.TextWriter? writer)
        => SetOutputStream("*STANDARD-OUTPUT*", writer, System.Console.Out);

    /// <summary>
    /// The <c>*ERROR-OUTPUT*</c> counterpart of <see cref="SetStandardOutput"/>.
    /// Separate because a host usually wants diagnostics somewhere else than
    /// program output, which is the distinction the two variables exist for.
    /// </summary>
    public static void SetErrorOutput(System.IO.TextWriter? writer)
        => SetOutputStream("*ERROR-OUTPUT*", writer, System.Console.Error);

    private static void SetOutputStream(string variableName, System.IO.TextWriter? writer,
                                        System.IO.TextWriter fallback)
    {
        Initialize();
        DynamicBindings.Set(Startup.Sym(variableName),
                            new LispOutputStream(writer ?? fallback));
    }

    /// <summary>
    /// Convert a Lisp result to its natural .NET representation: NIL -> null,
    /// T -> true, integers -> int (or long when out of int range), floats ->
    /// double/float, strings -> string, a wrapped .NET object -> the object
    /// itself. Values without a natural scalar counterpart (lists, symbols,
    /// hash-tables, ...) are returned as the underlying <see cref="LispObject"/>,
    /// which the caller can inspect or walk directly. Inverse of the
    /// <see cref="Runtime.DotNetToLisp"/> conversion used on the way in.
    /// </summary>
    public static object? ToClr(LispObject value) => Runtime.LispToDotNetGeneric(value);
    /// Passing a .NET array or collection straight to <see cref="Call"/> hands
    /// the Lisp side a foreign object, not a sequence: deliberately, so a
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
    /// Convert a Lisp sequence, a list or a vector, to a .NET array, each
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
    /// runtime, eval/compile of compound forms, dotnet:define-class, native FFI
    /// thunks, throws instead of emitting. A host that loads a precompiled image
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
    /// function (the counterpart of <see cref="Call"/>'s Lisp->C# direction).
    /// NAME is read as the Lisp reader reads a symbol; an unqualified name is
    /// interned in CL-USER, so Lisp code there reads <c>(name ...)</c> without a
    /// package prefix. Arguments arrive as natural .NET values (same
    /// conversion as <see cref="ToClr"/>) and the return is converted back via
    /// <see cref="Runtime.DotNetToLisp"/>; return null for a Lisp NIL. Registering
    /// a function does not generate code, so it is allowed under PrecompiledOnly.
    /// </summary>
    public static void Register(string name, Func<object?[], object?> fn)
    {
        // Read like any other host name. Unqualified, it is a symbol in CL-USER
        // whatever CurrentPackage is; "PKG::NAME" makes it in PKG, and
        // "PKG:NAME" names an existing external symbol of PKG.
        var read = ReadHostName(name, "Register");
        Symbol sym;
        if (read.Package == null)
            sym = (Package.FindPackage("CL-USER") ?? Startup.CLUser).Intern(read.Name).symbol;
        else if (read.Internal)
            sym = HostPackage(read, name, "Register").Intern(read.Name).symbol;
        else
            sym = QualifiedSymbol(read, name, "Register");
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
    /// the debugger: otherwise a Lisp error stalls on "stdin closed".
    ///
    /// Throws <see cref="DotclConditionException"/>, which carries the condition
    /// object, its type name and any wrapped .NET exception. A host that wants
    /// to act on the failure, read the condition's slots, choose a restart,
    /// needs the condition, and flattening it into a message string threw that
    /// away. <see cref="SetThrowingDebuggerHook(bool)"/> selects the older
    /// <see cref="InvalidOperationException"/> form for a host that matches on it.
    /// </summary>
    public static void SetThrowingDebuggerHook() => SetThrowingDebuggerHook(true);

    /// <summary>
    /// <see cref="SetThrowingDebuggerHook()"/>, choosing what it throws.
    /// TYPED true (what the no-argument overload does) throws
    /// <see cref="DotclConditionException"/>. TYPED false restores the original
    /// behaviour, an <see cref="InvalidOperationException"/> whose message is
    /// <c>"TYPE: report"</c> and which carries nothing else, for a host written
    /// against it. The no-argument overload is kept as its own signature rather
    /// than made a defaulted parameter: code already compiled against it (a
    /// shipped fasl, a host assembly) calls that exact signature.
    /// </summary>
    public static void SetThrowingDebuggerHook(bool typed)
    {
        var hookSym = Startup.Sym("*DEBUGGER-HOOK*");
        var hook = new LispFunction(a =>
        {
            var cond = a.Length > 0 ? a[0] : Nil.Instance;
            if (typed) throw new DotclConditionException(cond);
            throw new InvalidOperationException(ConditionText.Line(cond));
        }, "*NON-INTERACTIVE-DEBUGGER-HOOK*", 2);
        _typedThrowingHook = typed ? hook : null;
        DynamicBindings.Set(hookSym, hook);
    }

    /// <summary>The hook the typed <see cref="SetThrowingDebuggerHook(bool)"/>
    /// installed last, or null when the last one installed was the untyped form.</summary>
    private static LispFunction? _typedThrowingHook;

    /// <summary>
    /// True when an error the runtime raised itself should reach the host as a
    /// <see cref="DotclConditionException"/>: the typed throwing hook is what
    /// <c>*debugger-hook*</c> holds right now. Such an error -- a .NET method that
    /// threw under <c>dotnet:invoke</c>, a type error from CAR -- is thrown as a
    /// <see cref="LispErrorException"/> without running the hook, so without this
    /// a host catching DotclConditionException would miss it. A host that has
    /// since bound <c>*debugger-hook*</c> to something else gets the exception
    /// unchanged.
    /// </summary>
    private static bool ConvertsRuntimeErrors()
    {
        var hook = _typedThrowingHook;
        return hook != null
            && ReferenceEquals(DynamicBindings.Get(Startup.Sym("*DEBUGGER-HOOK*")), hook);
    }

    [ThreadStatic] private static int t_hostEntryDepth;

    /// <summary>
    /// Run BODY as a call from the host into Lisp. At the outermost such call on
    /// the thread, a runtime-raised error on its way out becomes a
    /// <see cref="DotclConditionException"/> (see <see cref="ConvertsRuntimeErrors"/>).
    /// A nested call -- Lisp code that called back into the host, which called
    /// into Lisp again -- leaves it alone, so the Lisp frames in between still
    /// see the original condition and their handlers still apply.
    /// </summary>
    private static T HostEntry<T>(Func<T> body)
    {
        // The depth is captured rather than read in the filter: filters run in
        // the first pass of exception dispatch, before the finally blocks of any
        // nested entry have restored the counter.
        int depth = ++t_hostEntryDepth;
        try { return body(); }
        catch (LispErrorException e) when (depth == 1 && ConvertsRuntimeErrors())
        {
            throw new DotclConditionException(e.Condition, e);
        }
        finally { t_hostEntryDepth--; }
    }

    private static void HostEntry(Action body) => HostEntry<object?>(() => { body(); return null; });

    // The build-tool entry points moved to DotclBuild, which is where a build
    // tool should look for them; these forward so code compiled against the old
    // names keeps working for one release. Removing a member is what breaks a
    // shipped fasl, so the names go out with a warning first rather than
    // disappearing. The implementations are in DotclBuild.cs.
    [System.Obsolete("Moved to DotclBuild.ResolveDeps.")]
    public static void ResolveDeps(string asdPath, string? manifestOut, string? rootSourcesOut,
                                   string? targetRid = null, string[]? buildInit = null,
                                   string[]? searchPaths = null)
        => DotclBuild.ResolveDeps(asdPath, manifestOut, rootSourcesOut, targetRid, buildInit, searchPaths);

    [System.Obsolete("Moved to DotclBuild.CompileProject.")]
    public static void CompileProject(string asdPath, string outputPath, string[]? buildInit = null,
                                      string[]? searchPaths = null, bool debugInfo = false)
        => DotclBuild.CompileProject(asdPath, outputPath, buildInit, searchPaths, debugInfo);

    [System.Obsolete("Moved to DotclBuild.PackFasl.")]
    public static void PackFasl(string system, string outputFasl, string? toplevel = null,
                                string[]? buildInit = null, string[]? searchPaths = null)
        => DotclBuild.PackFasl(system, outputFasl, toplevel, buildInit, searchPaths);

    [System.Obsolete("Moved to DotclBuild.ReadSystemMeta.")]
    public static DotclBuild.SystemMeta? ReadSystemMeta(string system, string[]? searchPaths = null)
        => DotclBuild.ReadSystemMeta(system, searchPaths);

}
