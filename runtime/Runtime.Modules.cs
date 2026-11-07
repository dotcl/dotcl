using System.Collections.Concurrent;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    // --- Modules (provide/require) ---

    // Serialize provide/require so concurrent (require "x") calls don't
    // double-load the same module. _modulesLock is process-wide; a thread
    // doing (require "x") may invoke user code (the provider) while
    // holding the lock, so all other concurrent require/provide calls
    // wait. This is heavy but correct; finer-grained per-module locking
    // is reserved for a follow-up if contention shows up.
    private static readonly object _modulesLock = new();

    public static LispObject Provide(LispObject moduleName)
    {
        string name = ToStringDesignator(moduleName, "PROVIDE");
        // Add to *modules* if not already present
        var modulesSym = Startup.Sym("*MODULES*");
        lock (_modulesLock)
        {
            LispObject modules = DynamicBindings.Get(modulesSym);
            // Check if already present
            LispObject cur = modules;
            while (cur is Cons c)
            {
                if (c.Car is LispString s && s.Value == name) return T.Instance;
                cur = c.Cdr;
            }
            DynamicBindings.Set(modulesSym, new Cons(new LispString(name), modules));
            ProvideShared(modulesSym, name);
            return T.Instance;
        }
    }

    private static bool ModuleListed(LispObject? modules, string name)
    {
        for (var c = modules; c is Cons cc; c = cc.Cdr)
            if (cc.Car is LispString s && s.Value == name) return true;
        return false;
    }

    /// <summary>A module provided in a package unit that keeps its own *MODULES* but
    /// does not own the module is provided for every unit: loaded once, shared.</summary>
    private static void ProvideShared(Symbol modulesSym, string name)
    {
        if (!Package.SharesRegistryEntry(modulesSym, name)) return;
        if (!ModuleListed(modulesSym.Value, name))
            modulesSym.Value = new Cons(new LispString(name), modulesSym.Value ?? Nil.Instance);
    }

    /// <summary>
    /// The four contribs that used to ship under an unprefixed name, and what
    /// they are called now. Four general words (advice, clrmd, decompiler,
    /// nuget) sitting in asdf:*central-registry* quietly won over a system of
    /// the same name belonging to the user, so they were given the dotcl-
    /// prefix the other bundled systems already had.
    ///
    /// Consulted only after every module provider has declined, so a user who
    /// does have their own "nuget" system still gets it: this only turns the
    /// failure that would otherwise say nothing into one that names the new
    /// module.
    /// </summary>
    private static string? RenamedContribHint(string name) => name.ToLowerInvariant() switch
    {
        "advice" => "this contrib was renamed to \"dotcl-advice\"",
        "clrmd" => "this contrib was renamed to \"dotcl-clrmd\"",
        "decompiler" => "this contrib was renamed to \"dotcl-decompiler\"",
        "nuget" => "this contrib was renamed to \"dotnet-nuget\"",
        _ => null
    };

    public static LispObject Require(LispObject[] args)
    {
        if (args.Length == 0)
            throw new LispErrorException(new LispProgramError("REQUIRE: missing module name"));
        string name = ToStringDesignator(args[0], "REQUIRE");
        var modulesSym = Startup.Sym("*MODULES*");

        // Hold _modulesLock for the entire snapshot/load/republish sequence
        // so concurrent (require "x") calls cannot both fall past the
        // already-loaded check and invoke the provider twice.
        lock (_modulesLock)
        {

        // Snapshot *modules* so we can return the set-difference (SBCL
        // convention: REQUIRE returns the list of modules newly provided).
        var before = new HashSet<string>();
        for (LispObject c = DynamicBindings.Get(modulesSym); c is Cons cc; c = cc.Cdr)
            if (cc.Car is LispString s) before.Add(s.Value);

        // Already in *modules*: nothing to do, empty diff = NIL.
        if (before.Contains(name)) return Nil.Instance;
        if (Package.SharesRegistryEntry(modulesSym, name) && ModuleListed(modulesSym.Value, name))
        {
            DynamicBindings.Set(modulesSym, new Cons(new LispString(name), DynamicBindings.Get(modulesSym)));
            return Nil.Instance;
        }

        // If pathname-list provided, load them
        if (args.Length > 1 && args[1] is not Nil)
        {
            var pathnames = args[1];
            if (StringValueOrNull(pathnames) != null)
                Load(new LispObject[] { pathnames });
            else if (pathnames is LispPathname pp)
                Load(new LispObject[] { pp });
            else
            {
                var paths = pathnames;
                while (paths is Cons pc)
                {
                    Load(new LispObject[] { pc.Car });
                    paths = pc.Cdr;
                }
            }
        }
        else
        {
            // No pathnames: call *module-provider-functions* (SBCL-compatible)
            var mpfSym = Startup.Sym("*MODULE-PROVIDER-FUNCTIONS*");
            LispObject providers = DynamicBindings.Get(mpfSym);
            bool found = false;
            while (providers is Cons pc)
            {
                var provider = pc.Car;
                LispObject result;
                if (provider is Symbol sym && sym.Function is LispFunction sfn)
                    result = sfn.Invoke(new LispString(name));
                else if (provider is LispFunction fn)
                    result = fn.Invoke(new LispString(name));
                else
                {
                    providers = pc.Cdr;
                    continue;
                }
                if (result is not Nil)
                {
                    found = true;
                    break;
                }
                providers = pc.Cdr;
            }
            if (!found)
                throw new LispErrorException(new LispError(
                    RenamedContribHint(name) is string hint
                        ? $"REQUIRE: module \"{name}\" not found -- {hint}"
                        : $"REQUIRE: module \"{name}\" not found"));
        }

        // Some contribs' files forget to call (provide ...), which would
        // let a second (require "same") re-load the file. Defensively push
        // NAME onto *modules* if the load/providers didn't. This deviates
        // slightly from strict SBCL (which trusts the module) but makes
        // REQUIRE idempotent regardless of contrib hygiene.
        bool nameInAfter = false;
        for (LispObject c = DynamicBindings.Get(modulesSym); c is Cons cc; c = cc.Cdr)
            if (cc.Car is LispString s && s.Value == name) { nameInAfter = true; break; }
        if (!nameInAfter)
            DynamicBindings.Set(modulesSym,
                new Cons(new LispString(name), DynamicBindings.Get(modulesSym)));
        ProvideShared(modulesSym, name);

        // Collect names added to *modules* during this call. *modules* is
        // prepended newest-first; iterating head->tail and prepending to the
        // result reverses to chronological order of the PROVIDE calls.
        LispObject added = Nil.Instance;
        var compiling = s_compileFileRequired.Count > 0 ? s_compileFileRequired.Peek() : null;
        for (LispObject c = DynamicBindings.Get(modulesSym); c is Cons cc; c = cc.Cdr)
            if (cc.Car is LispString s && !before.Contains(s.Value))
            {
                added = new Cons(new LispString(s.Value), added);
                compiling?.Add(s.Value);
            }
        return added;

        }
    }

    /// <summary>
    /// Extra contrib search paths added by host applications (MAUI, etc.)
    /// whose contrib bundle lives somewhere other than
    /// <c>AppContext.BaseDirectory/contrib</c>. Each path is the parent of a
    /// contrib tree: i.e. candidate files are <c>&lt;path&gt;/name/name.ext</c>.
    /// Entries are prepended to the search order so hosts can override.
    /// </summary>
    public static readonly List<string> ContribExtraSearchPaths = new();

    public static LispObject ContribParent(LispObject[] args)
    {
        var baseDir = AppContext.BaseDirectory;
        var searchDirs = new List<string>();
        searchDirs.AddRange(ContribExtraSearchPaths);
        searchDirs.Add(Path.Combine(baseDir, "contrib"));
        searchDirs.Add(Path.Combine(baseDir, "..", "..", "..", "contrib"));
        searchDirs.Add(Path.Combine(baseDir, "..", "..", "..", "..", "contrib"));
        foreach (var dir in searchDirs)
        {
            var full = Path.GetFullPath(dir);
            if (Directory.Exists(full))
            {
                var parent = Directory.GetParent(full)?.FullName;
                if (parent != null)
                    return LispPathname.FromString(parent + Path.DirectorySeparatorChar);
            }
        }
        return Nil.Instance;
    }

    /// <summary>
    /// Default module provider: searches contrib/ directory relative to the binary.
    /// Tries .fasl, .sil, then .lisp extensions (like SBCL's module-provide-contrib).
    /// Hosts can register additional roots via
    /// <see cref="ContribExtraSearchPaths"/>; these are searched before the
    /// default BaseDirectory-relative paths.
    /// </summary>
    public static LispObject ModuleProvideContrib(LispObject[] args)
    {
        if (args.Length == 0) return Nil.Instance;
        string name = ToStringDesignator(args[0], "MODULE-PROVIDE-CONTRIB").ToLowerInvariant();

        var baseDir = AppContext.BaseDirectory;
        var searchDirs = new List<string>();
        searchDirs.AddRange(ContribExtraSearchPaths);
        searchDirs.Add(Path.Combine(baseDir, "contrib"));
        // Dev fallbacks: runtime/bin/Debug/net10.0/ -> runtime/contrib/ or
        // project-root contrib/
        searchDirs.Add(Path.Combine(baseDir, "..", "..", "..", "contrib"));
        searchDirs.Add(Path.Combine(baseDir, "..", "..", "..", "..", "contrib"));

        string[] extensions = { ".fasl", ".sil", ".lisp" };
        // Fasls built against other runtime source, passed over for a later
        // candidate; the first is loaded when there is none.
        var otherRuntime = new List<(string Path, string Built)>();

        // OS key for per-OS fasl lookup (runtimes/{os}/name.fasl).
        // dotcl ships a single OS-agnostic asdf.fasl: the .NET IL is portable and all
        // OS-divergent behavior is resolved at run time (os-cond is runtime for dotcl,
        // and dotcl-reachable read-time #+os-* are impl-gated/invariant), so there is no
        // per-OS runtimes/{os}/ variant to prefer.
        foreach (var dir in searchDirs)
        {
            foreach (var ext in extensions)
            {
                // contrib/name/name.ext
                var path = Path.GetFullPath(Path.Combine(dir, name, name + ext));
                if (!File.Exists(path)) continue;
                // Another implementation's fasl sitting where ours would be:
                // warn and fall through to the .sil / .lisp candidate instead of
                // failing the whole (require ...) on a file we cannot read.
                if (IsForeignFasl(path))
                {
                    Console.Error.WriteLine(
                        $";; warning: ignoring {path}: not a dotcl fasl (foreign Lisp fasl?)");
                    continue;
                }
                if (ext != ".lisp" && IsOlderThanContribSource(path, dir, name))
                    continue;
                if (ext == ".fasl" && OtherRuntimeFasl(path) is string built)
                {
                    otherRuntime.Add((path, built));
                    continue;
                }
                foreach (var (p, b) in otherRuntime)
                    Console.Error.WriteLine(
                        $";; warning: ignoring {p}: built against other runtime source " +
                        $"({b}, this runtime {Startup.RuntimeSource()})");
                LoadContrib(name, path, searchDirs);
                return T.Instance;
            }
        }
        if (otherRuntime.Count == 0) return Nil.Instance;
        LoadContrib(name, otherRuntime[0].Path, searchDirs);
        return T.Instance;
    }

    private static void LoadContrib(string name, string path, List<string> searchDirs)
    {
        // The quicklisp client reads asdf: symbols (client.lisp,
        // dist.lisp, misc.lisp, setup.lisp). Loading its fasl with
        // asdf absent does not fail here: the references are
        // resolved lazily and the ASDF package is never even
        // created: so the breakage would surface much later as a
        // missing-package error inside quickload. Pull asdf in
        // first, the way upstream's bootstrap loads asdf.lisp ahead
        // of the client. The nested Require re-enters _modulesLock
        // on the same thread, which is fine (lock is reentrant).
        if (name == "quicklisp")
            Require(new LispObject[] { new LispString("asdf") });

        if (path.EndsWith(".fasl", StringComparison.OrdinalIgnoreCase))
            s_nextFaslMismatchWarnsOnly = true;
        // A shipped fasl records its source as /dotcl-src/NAME/...; the source ships next to it.
        if (path.EndsWith(".fasl", StringComparison.OrdinalIgnoreCase)
            && Path.GetDirectoryName(path) is string contribDir)
            s_sourceRoots.TryAdd(name, contribDir);
        try { Load(new LispObject[] { new LispString(path) }); }
        finally { s_nextFaslMismatchWarnsOnly = false; }
        if (name == "quicklisp")
            RegisterQuicklispWithAsdf();
        if (name == "asdf")
        {
            RegisterUserAsdSearchPaths();
            PatchUiopWindowsPath();
            RegisterContribWithAsdf(searchDirs.ToArray());
            InstallAsdfModuleProvider();
            UseAsdfSourceLoadingWithoutCompiler();
            KeepRunningAsdfAndUiop();
            InstallAsdfR2rHook();
            InstallAsdfRecompileOnFaslMismatch();
            UseErrorOnCompileFileFailure();
        }
    }

    /// <summary>The runtime digest the fasl at PATH was compiled against, when it
    /// has one and it is not this runtime's; otherwise null. Read from the
    /// metadata without loading the assembly.</summary>
    private static string? OtherRuntimeFasl(string path)
    {
#if NETSTANDARD2_0
        return null;
#else
        if (Startup.RuntimeSource() is not string mine) return null;
        try
        {
            using var fs = File.OpenRead(path);
            using var pe = new System.Reflection.PortableExecutable.PEReader(fs);
            if (!pe.HasMetadata) return null;
            var md = System.Reflection.Metadata.PEReaderExtensions.GetMetadataReader(pe);
            foreach (var h in md.GetAssemblyDefinition().GetCustomAttributes())
            {
                var blob = md.GetBlobReader(md.GetCustomAttribute(h).Value);
                if (blob.Length < 2 || blob.ReadUInt16() != 1) continue;
                string? key, value;
                try { key = blob.ReadSerializedString(); value = blob.ReadSerializedString(); }
                catch { continue; }
                if (key != Startup.CoreGenerationKey || value == null) continue;
                int nl = value.IndexOf('\n');
                var gen = nl < 0 ? value : value.Substring(0, nl);
                if (gen.Length == 0) return null;
                var theirs = Startup.RuntimePart(gen) ?? "none";
                return theirs == mine ? null : theirs;
            }
        }
        catch { }
        return null;
#endif
    }

    /// <summary>Whether BUILTPATH is older than the module's .lisp beside it
    /// (a source tree edited after the build); warns when it is.</summary>
    private static bool IsOlderThanContribSource(string builtPath, string dir, string name)
    {
        var src = Path.GetFullPath(Path.Combine(dir, name, name + ".lisp"));
        try
        {
            if (!File.Exists(src)
                || File.GetLastWriteTimeUtc(builtPath) >= File.GetLastWriteTimeUtc(src))
                return false;
        }
        catch { return false; }
        Console.Error.WriteLine(
            $";; warning: ignoring {builtPath}: older than its source {src}");
        return true;
    }

    /// <summary>
    /// User-supplied .asd search paths from --asd-search-path. Set by Program.cs
    /// before module load; consumed by RegisterUserAsdSearchPaths after asdf loads.
    /// </summary>
    public static readonly List<string> UserAsdSearchPaths = new();

    /// <summary>
    /// Script user-arguments from a positional `dotcl file.lisp arg1 arg2`
    /// invocation. Null when not running a positional script (REPL, --eval,
    /// --load-only, save-application exe). Set by Program.cs before the script
    /// runs; surfaced via dotcl:command-line-arguments so that
    /// uiop:command-line-arguments returns these args. See COMMAND-LINE-ARGUMENTS
    /// registration in Startup.cs for the `--`-delimited shape.
    /// </summary>
    public static List<string>? ScriptArgs = null;

    /// <summary>True in an executable that carries its program (save-application :executable,
    /// a restamped pack tool): every command-line argument belongs to that program.</summary>
    public static bool HostIsApplication = false;

    /// <summary>
    /// On Windows hosts, wrap uiop:parse-unix-namestring so that strings
    /// containing `\` separators or a drive-letter prefix are normalized
    /// to UNIX form before reaching ASDF's parser. .NET's Path APIs return
    /// native separators, so user code feeding `Path.Combine`-style results
    /// straight into asdf:load-asd / asdf:load-system would otherwise hit
    /// "Expected an absolute pathname" because the UNIX parser sees `\` as
    /// a filename character. Runtime patch (no asdf source change), so
    /// the behavior stays scoped to dotcl-on-Windows.
    /// </summary>
    private static void PatchUiopWindowsPath()
    {
        if (!Compat.IsWindows()) return;
        // ASDF coerces pathname designators through uiop:parse-unix-namestring, which
        // is UNIX-only and cannot represent a drive letter, so a native Windows path
        // (backslashes or a C: drive) fails ASDF's absolute-pathname check.
        // dotcl:parse-native-namestring parses native Windows paths correctly
        // (drive -> device, UNC -> host), so on Windows we route native paths through
        // it (lossless: the drive is preserved) and let genuine unix-style namestrings
        // fall through to the original. This replaces an earlier lossy version that
        // stripped the drive (wrong on non-C: drives).
        const string patch = @"
(let ((orig (fdefinition 'uiop:parse-unix-namestring)))
  (setf (fdefinition 'uiop:parse-unix-namestring)
        (lambda (name &rest keys)
          (if (and (stringp name)
                   (>= (length name) 2)
                   (or (find #\\ name)
                       (and (alpha-char-p (char name 0))
                            (eql (char name 1) #\:))))
              (let ((pn (dotcl:parse-native-namestring name)))
                (if (getf keys :ensure-directory)
                    (uiop:ensure-directory-pathname pn)
                    pn))
              (apply orig name keys)))))";
        try
        {
            var read = MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString(patch) }));
            Runtime.EvalSetupForm(read);
        }
        catch { /* ignore if uiop symbols missing */ }
    }

    /// <summary>
    /// Push the user-supplied --asd-search-path values to asdf:*central-registry*.
    /// These take effect after asdf is loaded.
    /// </summary>
    private static void RegisterUserAsdSearchPaths()
    {
        if (UserAsdSearchPaths.Count == 0) return;
        var dirs = new List<string>();
        foreach (var p in UserAsdSearchPaths)
        {
            var full = Path.GetFullPath(p);
            if (Directory.Exists(full)) dirs.Add(full);
        }
        PushDirsToCentralRegistry(dirs);
    }

    /// <summary>PUSHNEW (PATHNAME dir) onto asdf:*central-registry* :test #'equal for each of DIRS in turn,
    /// done on the symbol's value directly: building and compiling a PUSHNEW form for each of the
    /// 39 bundled contrib directories took longer than loading asdf itself did (the first compile of
    /// a process pays for the JIT of the compiler). Does nothing when ASDF is not loaded.</summary>
    private static void PushPathnamesToCentralRegistry(IList<string> dirs)
    {
        if (dirs.Count == 0) return;
        try
        {
            var asdfPkg = Package.FindPackage("ASDF");
            if (asdfPkg == null) return;
            var (sym, status) = asdfPkg.FindSymbol("*CENTRAL-REGISTRY*");
            if (status == SymbolStatus.None || !DynamicBindings.TryGet(sym, out var current)) return;
            LispObject list = current;
            foreach (var d in dirs)
            {
                var pn = Runtime.Pathname(new LispString(d));
                bool present = false;
                for (var c = list; c is Cons cc; c = cc.Cdr)
                    if (Runtime.IsTrueEqual(pn, cc.Car)) { present = true; break; }
                if (!present) list = new Cons(pn, list);
            }
            Runtime.SetSymbolValue(sym, list);
        }
        catch { /* ignore if ASDF symbols not yet available */ }
    }

    private static void PushDirsToCentralRegistry(IList<string> dirs)
        => PushPathnamesToCentralRegistry(dirs.Select(d => d.Replace('\\', '/').TrimEnd('/') + "/").ToList());

    /// <summary>
    /// The bundled quicklisp client is one file with no quicklisp.asd, so a
    /// system that lists "quicklisp" in :depends-on (Quicklisp tooling such as
    /// quickdist) could not be found, where a stock Quicklisp install has the
    /// .asd beside its sources. Tell ASDF the system is already in the image.
    /// </summary>
    private static void RegisterQuicklispWithAsdf()
    {
        try
        {
            var read = MultipleValues.Primary(Runtime.ReadFromString(new LispObject[] {
                new LispString("(asdf:register-preloaded-system \"quicklisp\")") }));
            Runtime.EvalSetupForm(read);
        }
        catch { /* ASDF absent: nothing to tell */ }
    }

    /// <summary>
    /// After ASDF is loaded, push every contrib subdirectory onto
    /// asdf:*central-registry*, so the bundled systems (dotcl-gray, dotcl-socket and
    /// the rest) are found by name without the user registering a source directory.
    /// The central registry is searched before the source registry, so a bundled
    /// name wins over a same-named system found elsewhere.
    /// </summary>
    private static void RegisterContribWithAsdf(IEnumerable<string> searchDirs)
    {
        // Walk every search dir (not just the first one with a hit). asdf's
        // pushnew on *central-registry* deduplicates by equal pathname, so
        // contributions from runtime/bin/...staged copy/ + source contrib/
        // merge cleanly. Without this, a stale staged copy hides newer .asd
        // files added under source contrib/.
        var dirs = new List<string>();
        foreach (var baseContrib in searchDirs)
        {
            var contribFull = Path.GetFullPath(baseContrib);
            if (!Directory.Exists(contribFull)) continue;
            foreach (var sub in Directory.GetDirectories(contribFull))
                dirs.Add(Path.GetFullPath(sub).Replace('\\', '/') + "/");
        }
        PushPathnamesToCentralRegistry(dirs);
        ResolveAsdlessContribsAsRequireSystems(searchDirs);
    }

    /// <summary>
    /// A contrib with no .asd of its own (quicklisp, dotnet-ffi) resolves, ahead
    /// of any same-named .asd in ASDF's configuration, to the system already in
    /// the image or to a REQUIRE-SYSTEM, as its own .asd would. "asdf" is left to ASDF.
    /// </summary>
    private static void ResolveAsdlessContribsAsRequireSystems(IEnumerable<string> searchDirs)
    {
        var withAsd = new HashSet<string>();
        var modules = new HashSet<string>();
        foreach (var baseContrib in searchDirs)
        {
            var contribFull = Path.GetFullPath(baseContrib);
            if (!Directory.Exists(contribFull)) continue;
            foreach (var sub in Directory.GetDirectories(contribFull))
            {
                var name = Path.GetFileName(sub).ToLowerInvariant();
                if (File.Exists(Path.Combine(sub, name + ".asd"))) withAsd.Add(name);
                else if (File.Exists(Path.Combine(sub, name + ".fasl"))
                         || File.Exists(Path.Combine(sub, name + ".sil"))
                         || File.Exists(Path.Combine(sub, name + ".lisp")))
                    modules.Add(name);
            }
        }
        modules.ExceptWith(withAsd);
        modules.Remove("asdf");
        if (modules.Count == 0) return;
        var names = new System.Text.StringBuilder();
        foreach (var n in modules) names.Append(" \"").Append(n).Append('"');
        var form = @"
(let ((sf (find-symbol ""*SYSTEM-DEFINITION-SEARCH-FUNCTIONS*"" ""ASDF""))
      (reg (find-symbol ""REGISTERED-SYSTEM"" ""ASDF""))
      (rs (find-symbol ""REQUIRE-SYSTEM"" ""ASDF""))
      (names '(" + names + @"))
      (made (make-hash-table :test 'equal)))
  (when (and sf reg rs (boundp sf) (fboundp reg) (find-class rs nil))
    (let ((reg-fn (fdefinition reg)))
      (setf (symbol-value sf)
            (cons (lambda (name)
                    (when (member name names :test #'equal)
                      (or (funcall reg-fn name)
                          (gethash name made)
                          (setf (gethash name made) (make-instance rs :name name)))))
                  (symbol-value sf))))))";
        try
        {
            var read = MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString(form) }));
            Runtime.EvalSetupForm(read);
        }
        catch { /* ignore if ASDF symbols not yet available */ }
    }

    /// <summary>
    /// After ASDF loads, push its module provider onto CL:*MODULE-PROVIDER-FUNCTIONS*
    /// so that (require "some-asdf-system") falls back to ASDF: the way SBCL wires
    /// asdf:module-provide-asdf into sb-ext:*module-provider-functions* (asdf's
    /// footer.lisp does this only for known implementations; dotcl is not in that
    /// feature list, and dotcl's hook lives in CL, not sb-ext, so the integration
    /// never happened). asdf:module-provide-asdf returns T after loading the
    /// matching system and NIL when no such system exists, exactly the
    /// *module-provider-functions* contract REQUIRE expects.
    /// </summary>
    private static void InstallAsdfModuleProvider()
    {
        const string form = @"
(let ((p (find-symbol ""MODULE-PROVIDE-ASDF"" ""ASDF""))
      (v (find-symbol ""*MODULE-PROVIDER-FUNCTIONS*"" ""CL"")))
  (when (and p (fboundp p) v (boundp v))
    (set v (adjoin p (symbol-value v)))))";
        try
        {
            var read = MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString(form) }));
            Runtime.EvalSetupForm(read);
        }
        catch { /* ignore if ASDF symbols not yet available */ }
    }

    /// <summary>
    /// On a build with no compiler (netstandard2.0 / wasm / -p:DotclNoEmit=true),
    /// make ASDF's LOAD-SYSTEM run LOAD-SOURCE-OP instead of LOAD-OP. LOAD-OP
    /// COMPILE-FILEs every component, so (require "some-system") and every
    /// asdf:load-system died on "COMPILE-FILE requires System.Reflection.Emit":
    /// the whole ASDF world was unreachable on those builds, not merely slower.
    /// LOAD-SOURCE-OP is ASDF's own operation for loading a system as source and
    /// needs nothing but LOAD. ASDF hardcodes 'LOAD-OP inside LOAD-SYSTEM (this
    /// generation has no *LOAD-SYSTEM-OPERATION* to set), so the redirection has
    /// to replace the function. Builds that have a compiler are untouched.
    /// </summary>
    /// <summary>Have ASDF write an ahead-of-time sibling for each fasl it
    /// compiles, when dotcl:*compile-r2r* says so. The method is always installed
    /// and reads the variable when it runs, so the decision can be made (or bound
    /// around one operation) at any point, not only before ASDF was loaded. It has to be ASDF that asks: ASDF
    /// compiles to a temporary output and renames it into place, so a hook inside
    /// COMPILE-FILE sees a name that no longer exists a moment later (the siblings
    /// came out as &lt;name&gt;-tmpXXXXXXXX.fasl.r2r-&lt;rid&gt; and were never found again).
    ///
    /// Nothing above this needs to know: the system is still compiled and loaded as
    /// the same .fasl, and the sibling is picked up at load or ignored if stale.
    /// </summary>
    private static void InstallAsdfR2rHook()
    {
        const string form = @"
(let ((perform (find-symbol ""PERFORM"" ""ASDF""))
      (cop     (find-symbol ""COMPILE-OP"" ""ASDF""))
      (src     (find-symbol ""CL-SOURCE-FILE"" ""ASDF""))
      (outs    (find-symbol ""OUTPUT-FILES"" ""ASDF"")))
  (when (and perform cop src outs)
    (dotcl.cil-compiler::%mini-eval
     `(defmethod ,perform :after ((op ,cop) (c ,src))
        (when dotcl:*compile-r2r*
          (dolist (f (,outs op c))
            (when (equal (pathname-type f) ""fasl"")
              (ignore-errors (dotcl:write-r2r-sibling (namestring f)))))))
     nil)))";
        try
        {
            var read = MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString(form) }));
            Runtime.EvalSetupForm(read);
        }
        catch { }
    }

    /// <summary>ASDF's LOAD-OP compiles a file again, by its TRY-RECOMPILING restart,
    /// when its fasl was compiled by another dotcl, once per action.</summary>
    private static void InstallAsdfRecompileOnFaslMismatch()
    {
        const string form = @"
(let ((pwr (find-symbol ""PERFORM-WITH-RESTARTS"" ""ASDF""))
      (lop (find-symbol ""LOAD-OP"" ""ASDF""))
      (src (find-symbol ""CL-SOURCE-FILE"" ""ASDF"")))
  (when (and pwr lop src)
    (dotcl.cil-compiler::%mini-eval
     `(defmethod ,pwr :around ((o ,lop) (c ,src))
        (let ((retried nil))
          (handler-bind ((dotcl:fasl-version-mismatch
                           (lambda (e)
                             (let ((r (and (not retried)
                                           (find-restart 'try-recompiling e))))
                               (when r
                                 (setf retried t)
                                 (invoke-restart r))))))
            (call-next-method))))
     nil)))";
        try
        {
            var read = MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString(form) }));
            Runtime.EvalSetupForm(read);
        }
        catch { }
    }

    /// <summary>A COMPILE-FILE that reports failure-p stops the ASDF build, as on
    /// SBCL (UIOP's default is :error only for SBCL and MKCL, :warn elsewhere).
    /// With :warn the build went on and the first error seen was a later one
    /// (a package the failed file should have made), not the cause.</summary>
    private static void UseErrorOnCompileFileFailure()
    {
        const string form = @"
(let ((s (find-symbol ""*COMPILE-FILE-FAILURE-BEHAVIOUR*"" ""UIOP"")))
  (when s (setf (symbol-value s) :error)))";
        try
        {
            var read = MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString(form) }));
            Runtime.EvalSetupForm(read);
        }
        catch { /* ignore if UIOP is not there */ }
    }

    /// <summary>
    /// A system that depends on "asdf" or "uiop" gets the running ones rather than an
    /// asdf.asd / uiop.asd seen in the registry: reloading them from source undoes what
    /// dotcl replaced after loading ASDF. A registry copy whose version file is strictly
    /// newer is still left to ASDF, which upgrades to it; without a compiler never.
    /// </summary>
    private static void KeepRunningAsdfAndUiop()
    {
        const string form = @"
(let ((emit-free (not (find :dotcl-emit *features*)))
      (self nil))
  (labels ((found-elsewhere (name)
             (dolist (f asdf:*system-definition-search-functions*)
               (unless (eq f self)
                 (let ((x (funcall f name)))
                   (when x
                     (return (if (typep x 'asdf:system) (asdf:system-source-file x) x)))))))
           (newer-in-registry-p (name)
             (let ((asd (found-elsewhere name)))
               (when asd
                 (let* ((asdfp (equal name ""asdf""))
                        (file (uiop:subpathname (pathname asd) ""version""
                                                :type (if asdfp ""lisp-expr"" ""lisp"")))
                        (v (ignore-errors
                            (and (uiop:probe-file* file)
                                 (uiop:read-file-form file :at (if asdfp '(0) '(2 2 2)))))))
                   (and (stringp v) (uiop:version< (asdf:asdf-version) v)))))))
    (setf self
          (lambda (name)
            (when (and (member name '(""asdf"" ""asdf-package-system"" ""uiop"") :test #'equal)
                       (or emit-free
                           (equal name ""asdf-package-system"")
                           (not (newer-in-registry-p name))))
              (or (asdf::registered-system name)
                  (asdf::sysdef-preloaded-system-search name)))))
    (push self asdf:*system-definition-search-functions*)))";
        try
        {
            var read = MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString(form) }));
            Runtime.EvalSetupForm(read);
        }
        catch { /* ignore if ASDF symbols not yet available */ }
    }

    private static void UseAsdfSourceLoadingWithoutCompiler()
    {
        // A REQUIRE-SYSTEM is exempt: it has no source to load -- its LOAD-OP just
        // calls REQUIRE, which is how dotcl's own contribs are reached, and REQUIRE
        // finds a prebuilt fasl. Sending those through LOAD-SOURCE-OP silently did
        // nothing (that operation has no method for the class), so the contrib never
        // loaded and the failure surfaced later as a missing package.
        const string form = @"
(unless (find :dotcl-emit *features*)
  (let ((ls (find-symbol ""LOAD-SYSTEM"" ""ASDF""))
        (op (find-symbol ""OPERATE"" ""ASDF""))
        (src (find-symbol ""LOAD-SOURCE-OP"" ""ASDF""))
        (load-op (find-symbol ""LOAD-OP"" ""ASDF""))
        (finder (find-symbol ""FIND-SYSTEM"" ""ASDF""))
        (reqsys (find-symbol ""REQUIRE-SYSTEM"" ""ASDF"")))
    (when (and ls op src (fboundp op))
      (setf (fdefinition ls)
            (lambda (system &rest keys)
              (let ((found (and finder reqsys (fboundp finder)
                                (ignore-errors (funcall (fdefinition finder) system)))))
                (apply (fdefinition op)
                       (if (and found (typep found reqsys)) load-op src)
                       system keys))
              t))))
  ;; OPERATE first calls UPGRADE-ASDF, which runs LOAD-OP on the system
  ;; ""asdf"". When an asdf.asd is visible in the source registry (an ASDF
  ;; checkout under a :tree), that system is the one from the .asd and its
  ;; LOAD-OP compiles ASDF from source, so every OPERATE died on COMPILE-FILE.
  ;; Without a compiler the running ASDF cannot be replaced: keep it.
  (let ((up (find-symbol ""UPGRADE-ASDF"" ""ASDF"")))
    (when (and up (fboundp up))
      (setf (fdefinition up) (lambda () nil)))))";
        try
        {
            var read = MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString(form) }));
            Runtime.EvalSetupForm(read);
        }
        catch { /* ignore if ASDF symbols not yet available */ }
    }
}
