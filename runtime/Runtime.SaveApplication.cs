using System.Collections.Concurrent;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    // --- save-application (MVP) ---

    /// <lispdoc>(dotcl:save-application output-path &amp;key load prelude system toplevel executable r2r no-self-contained target runtime-csproj) -- Bundle Lisp sources into a .fasl or self-contained exe. :system collects ASDF transitive deps; :prelude compiles sources ahead of them, for a shim the bundled libraries need in place before they load; :r2r t enables ReadyToRun ahead-of-time compilation; :no-self-contained t omits the .NET runtime (smaller binary, requires .NET on target); :target :linux-arm64 etc. for cross-platform publish; :runtime-csproj or DOTCL_RUNTIME_CSPROJ env var for installed-tool use.</lispdoc>
    /// <summary>
    /// <c>(dotcl:save-application output-path &amp;key load system toplevel executable target runtime-csproj)</c>
    ///
    /// Bundle Lisp sources into a .fasl or a self-contained single-file exe.
    ///
    /// <list type="bullet">
    /// <item><term>:load</term><description>
    ///   Pathname or list of pathnames. Sources are compiled into the output in order,
    ///   after the :system closure.
    /// </description></item>
    /// <item><term>:prelude</term><description>
    ///   Pathname or list of pathnames, compiled <em>before</em> the :system closure,
    ///   and loaded into this image before the closure is collected. For what a
    ///   bundled library needs to find already in place while it loads; typically
    ///   ASDF answers about itself, which a bundle has no .asd files to give.
    ///   Its contents are the caller's to write: standing in for a system that is not
    ///   there is only safe where nothing will act on the answer.
    /// </description></item>
    /// <item><term>:system</term><description>
    ///   ASDF system name (symbol or string). Collects the transitive set of
    ///   cl-source-file components via <c>asdf:required-components</c> and prepends
    ///   them before :load files. ASDF must already be loaded in the current session.
    /// </description></item>
    /// <item><term>:toplevel</term><description>
    ///   Symbol or "PKG:NAME" string. Appended as <c>(funcall (symbol-function 'X))</c>
    ///   at the end of the fasl's init code.
    /// </description></item>
    /// <item><term>:executable</term><description>
    ///   When true, invokes <c>dotnet publish --self-contained -p:PublishSingleFile=true</c>
    ///   to produce a standalone exe with the .NET runtime bundled.
    /// </description></item>
    /// <item><term>:target</term><description>
    ///   Cross-platform RID keyword: :win-x64, :win-arm64, :linux-x64, :linux-arm64,
    ///   :osx-x64, :osx-arm64. Defaults to the current host RID.
    ///   Only meaningful with :executable t.
    /// </description></item>
    /// <item><term>:runtime-csproj</term><description>
    ///   Override path to runtime.csproj for :executable t builds.
    ///   Also honoured via the DOTCL_RUNTIME_CSPROJ environment variable.
    /// </description></item>
    /// </list>
    ///
    /// State reconstruction note: defvar/defpackage/reader-macros defined in
    /// the compiled sources are re-evaluated as part of the fasl's ModuleInit, so
    /// they are reconstructed naturally. Runtime-only state mutations are not captured.
    /// </summary>
    [LispDoc("DOTCL:SAVE-APPLICATION")]
    public static LispObject SaveApplication(LispObject[] args)
    {
#if DOTCL_EMIT
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError(
                "SAVE-APPLICATION: requires output-path"));

        string outputPath = ResolvePhysicalPath(args[0]);

        LispObject? loadArg = null;
        LispObject? preludeArg = null;
        LispObject? toplevelArg = null;
        bool isExecutable = false;
        bool isR2r = false;
        bool noSelfContained = false;
        string? targetRid = null;
        string? runtimeCsprojOverride = null;
        LispObject? systemArg = null;

        if ((args.Length - 1) % 2 != 0)
            throw new LispErrorException(new LispProgramError(
                "SAVE-APPLICATION: odd number of keyword arguments"));
        for (int i = 1; i + 1 < args.Length; i += 2)
        {
            string key = args[i] is Symbol ks ? ks.Name : args[i].ToString();
            switch (key)
            {
                case "LOAD": loadArg = args[i + 1]; break;
                case "PRELUDE": preludeArg = args[i + 1]; break;
                case "TOPLEVEL": toplevelArg = args[i + 1]; break;
                case "EXECUTABLE": isExecutable = args[i + 1] is not Nil; break;
                case "R2R": isR2r = args[i + 1] is not Nil; break;
                case "NO-SELF-CONTAINED": noSelfContained = args[i + 1] is not Nil; break;
                case "SYSTEM":
                    if (args[i + 1] is not Nil)
                        systemArg = args[i + 1];
                    break;
                case "TARGET":
                    if (args[i + 1] is not Nil)
                        targetRid = MapTargetToRid(args[i + 1]);
                    break;
                case "RUNTIME-CSPROJ":
                    if (args[i + 1] is not Nil)
                        runtimeCsprojOverride = ResolvePhysicalPath(args[i + 1]);
                    break;
                default:
                    throw new LispErrorException(new LispProgramError(
                        $"SAVE-APPLICATION: unknown keyword :{key}"));
            }
        }

        // The prelude says what has to be in place before anything else, so the
        // builder gets it first, not just the image being built. Collecting the
        // closure runs the build steps of systems that generate their own
        // sources (cl-unicode writes its character tables that way), and those
        // steps need whatever the prelude provides as much as the closure does.
        // Compiling it at the head of the source list is too late for them:
        // collection happens before any source is compiled.
        var preludeSources = NormalizeSourceList(preludeArg);
        LoadLispFiles(preludeSources, "SAVE-APPLICATION :prelude source");

        // Collect ASDF system sources (transitive, topologically ordered) if :system given.
        // This requires ASDF to be loaded in the current session.
        var asdfsources = new List<string>();
        var asdfSystems = new List<(string Name, string Version)>();
        if (systemArg != null)
            asdfsources = CollectAsdfSystemSources(systemArg, out asdfSystems);

        var userSources = NormalizeSourceList(loadArg);

        // :prelude, then the ASDF closure (the deps), then :load.
        //
        // The slot before the closure exists because a deployed image is not the
        // image the sources were written for. Libraries reasonably ask ASDF about
        // themselves while loading: cl-str binds its +version+ from
        // (asdf:component-version (asdf:find-system "str")), Lem does the same for
        // its own version and git revision: and in a bundle there are no .asd
        // files for ASDF to find, so those forms signal and the load stops.
        //
        // Whatever answers them has to be in place before their own file runs, and
        // :load is too late by construction. What goes there is the caller's to
        // write and to justify: a hand-written shim for one deployment target is a
        // different thing from this function inventing system definitions on its
        // own. Registering a system that does not exist is a lie ASDF will repeat
        // to anything that asks it later, which is tolerable only where nothing
        // asks, a browser page that never loads another system, and that is a
        // judgement about the target, which only the caller can make.
        var sources = new List<string>(
            preludeSources.Count + asdfsources.Count + userSources.Count);
        sources.AddRange(preludeSources);
        sources.AddRange(asdfsources);
        sources.AddRange(userSources);

        // Extract the toplevel entry. Accept Symbol directly, or a string
        // "PKG:NAME" / "NAME" which is resolved at save time (after :load files
        // have been compiled, so any package defined there is available).
        // String form is necessary when the calling build.lisp cannot reference
        // the entry package at read time (it doesn't exist yet).
        LispObject? toplevelDesignator = toplevelArg is Nil ? null : toplevelArg;

        // A Windows executable needs the .exe extension to be started by name.
        if (isExecutable && (targetRid ?? Compat.RuntimeIdentifier) is { } exeRid
            && exeRid.StartsWith("win", StringComparison.OrdinalIgnoreCase)
            && Path.GetExtension(outputPath).Length == 0)
            outputPath += ".exe";

        // Core mode: write the .fasl directly to outputPath.
        // Exec mode: write the .fasl to a temp path, then invoke `dotnet publish`
        // to produce a self-contained single-file exe with the .fasl embedded.
        string faslPath = isExecutable
            ? Path.Combine(Path.GetTempPath(), $"dotcl-saveapp-{Guid.NewGuid():N}.fasl")
            : outputPath;

        var outDir = Path.GetDirectoryName(outputPath);
        if (!string.IsNullOrEmpty(outDir) && !Directory.Exists(outDir))
            Directory.CreateDirectory(outDir);

        try
        {
            BuildFaslFromSources(faslPath, sources, toplevelDesignator, asdfSystems);
            if (isExecutable)
                BuildExecutableFromFasl(faslPath, outputPath, targetRid, runtimeCsprojOverride, isR2r, noSelfContained);
        }
        finally
        {
            if (isExecutable)
                try { File.Delete(faslPath); } catch { /* best-effort cleanup */ }
        }

        return LispPathname.FromString(Path.GetFullPath(outputPath));
#else
        throw new LispErrorException(new LispProgramError(
            "SAVE-APPLICATION requires System.Reflection.Emit, which is unavailable in this runtime build"));
#endif
    }

#if DOTCL_EMIT
    /// <summary>
    /// Compile :load sources into the FASL at `faslPath`, append an entry call form
    /// if a toplevel designator was given, save. Shared between save-application's
    /// :executable nil and :executable t paths.
    /// </summary>
    private static void BuildFaslFromSources(
        string faslPath, List<string> sources, LispObject? toplevelDesignator,
        List<(string Name, string Version)>? asdfSystems = null)
    {
        var moduleName = Path.GetFileNameWithoutExtension(faslPath)
            + "_" + Guid.NewGuid().ToString("N").Substring(0, 8);
        var faslAsm = new DotCL.Emitter.FaslAssembler(moduleName);

        var compileFileModeSym = Startup.Sym("*COMPILE-FILE-MODE*");
        var oldCompileFileMode = DynamicBindings.Get(compileFileModeSym);
        DynamicBindings.Set(compileFileModeSym, T.Instance);

        var packageSym = Startup.Sym("*PACKAGE*");
        var oldPackage = DynamicBindings.Get(packageSym);
        var readtableSym = Startup.Sym("*READTABLE*");
        var oldReadtable = DynamicBindings.Get(readtableSym);

        try
        {
            // Prepend (require "asdf") so UIOP is available when sources
            // compiled with ASDF deps load in a standalone exe context. Skipped when
            // nothing could refer to it: no ASDF closure, and no ASDF / UIOP package
            // in this session for the sources to have been read against. Loading
            // asdf.fasl costs about a second of every start of the application.
            bool needsAsdf = (asdfSystems != null && asdfSystems.Count > 0)
                || Package.FindPackage("ASDF") != null || Package.FindPackage("UIOP") != null;
            if (sources.Count > 0 && needsAsdf)
            {
                var requireForm = new Cons(Startup.Sym("REQUIRE"),
                    new Cons(new LispString("asdf"), Nil.Instance));
                faslAsm.AddTopLevelForm(CompileTopLevel(requireForm));
            }

            // Tell ASDF which systems this image already contains, before any of
            // their own files run. Libraries routinely bind their version from
            // (asdf:component-version (asdf:find-system :self)) at load time,
            // dexador and cl-str both do, and a bundle has no .asd files, so
            // those forms would signal and stop the load.
            //
            // register-preloaded-system is ASDF's own answer for "in the image,
            // no filesystem backing"; it registers the system directly rather
            // than through the search functions, so a real .asd on the target
            // still wins if there is one. The stronger register-immutable-system
            // (which makes the in-image copy unconditionally win) is a
            // deployment policy, so it stays the caller's to add via :prelude.
            //
            // Versions are literals read at collection time: :version t means
            // "keep the version of the already-loaded system", which is true in
            // the building image and false in the deployed one.
            // Count, not null: with no :system there is no closure and ASDF may
            // not even be loaded, so resolving the symbol would signal.
            if (asdfSystems != null && asdfSystems.Count > 0)
            {
                var registerSym = ResolveSymbolName("ASDF:REGISTER-PRELOADED-SYSTEM");
                foreach (var (name, version) in asdfSystems)
                {
                    if (version == RequireModuleMarker)
                    {
                        // A module of the image, not a system with files: the deployed
                        // image loads it with REQUIRE before the sources that use it.
                        faslAsm.AddTopLevelForm(CompileTopLevel(new Cons(Startup.Sym("REQUIRE"),
                            new Cons(new LispString(name), Nil.Instance))));
                        continue;
                    }
                    LispObject args = new Cons(new LispString(name), Nil.Instance);
                    if (!string.IsNullOrEmpty(version))
                        ((Cons)args).Cdr = new Cons(Startup.SymInPkg("VERSION", "KEYWORD"),
                            new Cons(new LispString(version), Nil.Instance));
                    faslAsm.AddTopLevelForm(CompileTopLevel(new Cons(registerSym, args)));
                }
            }

            // Say where we are. A closure of several hundred sources takes long
            // enough that silence is indistinguishable from a hang, and when it
            // does stop, the last line printed is the first thing worth knowing.
            var progress = GetStandardOutputWriter();
            for (int i = 0; i < sources.Count; i++)
            {
                progress.WriteLine($"; [{i + 1}/{sources.Count}] {sources[i]}");
                progress.Flush();
                // Each source starts with the package and readtable the build began
                // with, as COMPILE-FILE binds them: an IN-PACKAGE does not carry over
                // into the next file (the :load file after an ASDF closure).
                DynamicBindings.Set(packageSym, oldPackage);
                DynamicBindings.Set(readtableSym, oldReadtable);
                CompileSourceInto(sources[i], faslAsm);
            }

            if (toplevelDesignator != null)
            {
                // Resolve the toplevel designator *now* (after sources have been
                // compiled, so any package defined in them is available).
                Symbol toplevelSym = toplevelDesignator switch
                {
                    Symbol s => s,
                    LispString lstr => ResolveSymbolName(lstr.Value),
                    _ => throw new LispErrorException(new LispProgramError(
                        "SAVE-APPLICATION: :toplevel must be a symbol or string"))
                };

                // Set *image-dumped-p* to :executable so uiop:command-line-arguments
                // returns user args correctly (without looking for -- separator).
                // Uses find-package/find-symbol to avoid compile-time symbol interning
                // issues across fresh runtimes (dotcl serializes symbols by name+pkg).
                // uiop:getcwd is now implemented via dotcl:getcwd in UIOP's os.lisp patch.
                {
                    const string patchUiop =
                        "(let ((uiop-pkg (find-package \"UIOP\")))" +
                        "  (when uiop-pkg" +
                        "    (let ((dumped-sym (find-symbol \"*IMAGE-DUMPED-P*\" uiop-pkg)))" +
                        "      (when dumped-sym (setf (symbol-value dumped-sym) :executable)))))";
                    var patchReader = new Reader(new System.IO.StringReader(patchUiop));
                    if (patchReader.TryRead(out var patchForm) && patchForm != null)
                        faslAsm.AddTopLevelForm(CompileTopLevel(patchForm));
                }

                // Append `(funcall (symbol-function 'TOPLEVEL))`: late-bound
                // so redefinitions (e.g. by asdf) resolve to the current definition
                // at runtime.
                var quoteSym = Startup.Sym("QUOTE");
                var funcallSym = Startup.Sym("FUNCALL");
                var symFunSym = Startup.Sym("SYMBOL-FUNCTION");
                var entryForm = new Cons(funcallSym,
                    new Cons(new Cons(symFunSym,
                        new Cons(new Cons(quoteSym,
                            new Cons(toplevelSym, Nil.Instance)), Nil.Instance)),
                        Nil.Instance));
                var instrList = CompileTopLevel(entryForm);
                faslAsm.AddTopLevelForm(instrList);
            }

            faslAsm.Save(faslPath);
        }
        finally
        {
            DynamicBindings.Set(compileFileModeSym, oldCompileFileMode);
            DynamicBindings.Set(packageSym, oldPackage);
            DynamicBindings.Set(readtableSym, oldReadtable);
        }
    }

    /// <summary>
    /// Invoke `dotnet publish` on runtime.csproj with the user fasl embedded as a
    /// manifest resource, producing a self-contained single-file exe at `outputExe`.
    ///
    /// MVP scope: requires the dotcl source tree to be accessible (runtime.csproj).
    /// The installed-tool-only case is a follow-up (no source tree).
    /// </summary>
    private static void BuildExecutableFromFasl(
        string userFaslPath, string outputExe,
        string? targetRid = null, string? runtimeCsprojOverride = null,
        bool readyToRun = false, bool noSelfContained = false)
    {
        var runtimeCsproj = runtimeCsprojOverride
            ?? FindRuntimeCsproj()
            ?? throw new LispErrorException(new LispProgramError(
                "SAVE-APPLICATION :EXECUTABLE T: runtime.csproj not found. "
                + "Provide the path via :runtime-csproj or the DOTCL_RUNTIME_CSPROJ "
                + "environment variable, or run from the dotcl source tree."));

        // Make sure dotcl.core is present next to runtime.csproj so the
        // published exe bundles it. If it's missing, copy from compiler/dotcl.core
        // (produced by `make compile-core-fasl`) as a best-effort.
        var runtimeDir = Path.GetDirectoryName(runtimeCsproj)!;
        var coreInRuntimeDir = Path.Combine(runtimeDir, "dotcl.core");
        bool coreCopiedByUs = false;
        if (!File.Exists(coreInRuntimeDir))
        {
            var coreSrc = FindBuiltCore()
                ?? throw new LispErrorException(new LispProgramError(
                    "SAVE-APPLICATION :EXECUTABLE T: dotcl.core not found. "
                    + "Run `make compile-core-fasl` first."));
            File.Copy(coreSrc, coreInRuntimeDir);
            coreCopiedByUs = true;
        }

        var publishOut = Path.Combine(Path.GetTempPath(),
            $"dotcl-publish-{Guid.NewGuid():N}");
        // COMPAT.RUNTIMEIDENTIFIER answers null only on netstandard2.0, which has no
        // run-time RID. That build cannot publish anyway, but the type is nullable on
        // every target, so say what is missing rather than pass null down.
        var rid = targetRid ?? Compat.RuntimeIdentifier
            ?? throw new LispErrorException(new LispError(
                "SAVE-APPLICATION: this build has no runtime identifier; pass :TARGET-RID"));

        try
        {
            var psi = new System.Diagnostics.ProcessStartInfo("dotnet")
            {
                UseShellExecute = false,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
            };
            psi.ArgumentList.Add("publish");
            psi.ArgumentList.Add(runtimeCsproj);
            psi.ArgumentList.Add("-c"); psi.ArgumentList.Add("Release");
            psi.ArgumentList.Add("-r"); psi.ArgumentList.Add(rid);
            psi.ArgumentList.Add(noSelfContained ? "--no-self-contained" : "--self-contained");
            psi.ArgumentList.Add("-p:PublishSingleFile=true");
            if (!noSelfContained)
                psi.ArgumentList.Add("-p:EnableCompressionInSingleFile=true");
            psi.ArgumentList.Add("-p:PackAsTool=false");
            psi.ArgumentList.Add($"-p:DotclUserFasl={userFaslPath}");
            if (readyToRun)
                psi.ArgumentList.Add("-p:PublishReadyToRun=true");
            psi.ArgumentList.Add("-o"); psi.ArgumentList.Add(publishOut);

            using var proc = System.Diagnostics.Process.Start(psi)
                ?? throw new InvalidOperationException("failed to start dotnet");
            string stdout = proc.StandardOutput.ReadToEnd();
            string stderr = proc.StandardError.ReadToEnd();
            proc.WaitForExit();
            if (proc.ExitCode != 0)
                throw new LispErrorException(new LispError(
                    $"SAVE-APPLICATION: dotnet publish failed (exit {proc.ExitCode})."
                    + $" stderr: {stderr.TrimEnd()} stdout tail: {TailLines(stdout, 10)}"));

            // Default AssemblyName for runtime.csproj is "runtime": find it and
            // copy to the user's target path.
            var exeName = Compat.IsWindows() ? "runtime.exe" : "runtime";
            var producedExe = Path.Combine(publishOut, exeName);
            if (!File.Exists(producedExe))
                throw new LispErrorException(new LispError(
                    $"SAVE-APPLICATION: expected publish output at {producedExe}"));

            File.Copy(producedExe, outputExe, overwrite: true);

            // Copy dotcl.core and contrib/ alongside the output exe.
            // These are needed at runtime: dotcl.core for the stdlib,
            // contrib/ for (require "asdf") and other modules.
            var outputDir = Path.GetDirectoryName(outputExe)!;

            var publishedCore = Path.Combine(publishOut, "dotcl.core");
            var coreDestPath = Path.Combine(outputDir, "dotcl.core");
            if (File.Exists(publishedCore))
                File.Copy(publishedCore, coreDestPath, overwrite: true);
            else
            {
                var coreSrcFallback = FindBuiltCore();
                if (coreSrcFallback != null)
                    File.Copy(coreSrcFallback, coreDestPath, overwrite: true);
            }

            // Copy contrib/ (module fasls including asdf) so require works at runtime.
            // Prefer the publish output's contrib; fall back to the runtime source tree.
            var publishedContrib = Path.Combine(publishOut, "contrib");
            var contribDest = Path.Combine(outputDir, "contrib");
            if (Directory.Exists(publishedContrib))
                CopyDirectory(publishedContrib, contribDest);

            // The publish output may omit runtime/contrib/asdf (gitignored, not in
            // Content items). Copy it explicitly from the runtime source tree.
            var runtimeContribAsdf = Path.Combine(runtimeDir, "contrib", "asdf");
            var destContribAsdf = Path.Combine(contribDest, "asdf");
            if (Directory.Exists(runtimeContribAsdf) && !Directory.Exists(destContribAsdf))
                CopyDirectory(runtimeContribAsdf, destContribAsdf);
        }
        finally
        {
            try { if (Directory.Exists(publishOut)) Directory.Delete(publishOut, true); } catch { }
            if (coreCopiedByUs)
                try { File.Delete(coreInRuntimeDir); } catch { }
        }
    }
#endif

    /// <summary>
    /// LOAD each of PATHS into this image, in order, resolved against the working
    /// directory. Shared by the two build-time options that need it: the
    /// build-init scripts, and a prelude, which has to be in place before the
    /// closure is collected as well as being compiled into the image. WHAT names
    /// the option in the not-found error, which is the caller's own spelling.
    /// </summary>
    internal static void LoadLispFiles(IEnumerable<string>? paths, string what)
    {
        if (paths == null) return;
        foreach (var s in paths)
        {
            if (string.IsNullOrWhiteSpace(s)) continue;
            var abs = Path.GetFullPath(s.Trim());
            if (!File.Exists(abs))
                throw new FileNotFoundException($"{what} not found: {abs}", abs);
            var lisp = abs.Replace("\\", "/");
            Eval(MultipleValues.Primary(
                ReadFromString(new LispObject[] { new LispString($"(load \"{lisp}\")") })));
        }
    }

    /// <summary>
    /// Build one self-contained fasl from an ASDF system's whole closure, each
    /// source compiled in turn in dependency order. This is what SAVE-APPLICATION
    /// :SYSTEM does, and `dotcl pack` calls the same path so the two cannot drift.
    /// PRESOURCES are compiled ahead of the closure: a caller-supplied prelude,
    /// and any source generated for declarations that are not files and so are
    /// not in the walk.
    /// </summary>
    internal static void BuildSystemFasl(
        string systemName, string faslPath, string? toplevel, List<string> preSources)
    {
#if DOTCL_EMIT
        var closure = CollectAsdfSystemSources(new LispString(systemName), out var systems);
        // A fasl with none of the system's code in it still loads, and does
        // nothing: refuse it here rather than ship it.
        if (closure.Count == 0)
            throw new LispErrorException(new LispError(
                $"cannot build system {systemName}: its closure has no Lisp source files, "
                + "so the fasl would contain none of its code"));
        var sources = new List<string>(preSources.Count + closure.Count);
        sources.AddRange(preSources);
        sources.AddRange(closure);
        LispObject? entry = string.IsNullOrEmpty(toplevel) ? null : new LispString(toplevel!);
        BuildFaslFromSources(faslPath, sources, entry, systems);
#else
        throw new LispErrorException(new LispProgramError(
            "Building a system fasl requires System.Reflection.Emit, "
            + "which is unavailable in this runtime build"));
#endif
    }

    /// <summary>
    /// Build one dependency system's fasl from its own SOURCES, in the order
    /// ASDF would compile them, the way ASDF builds it: each file is compiled
    /// and loaded before the next one is read. A later file may read, at read
    /// time or in a macro, what an earlier one defined or registered when it was
    /// loaded (micros' #.(gray-package-name) needs its backend loaded). The
    /// system is loaded in this image afterwards, as LOAD-SYSTEM would leave it.
    ///
    /// Each file starts with the *PACKAGE* and *READTABLE* the build started
    /// with, as COMPILE-FILE binds them: an IN-PACKAGE does not carry over into
    /// the next file. The fasl is written only when every file compiled.
    /// </summary>
    internal static void BuildDependencyFasl(string faslPath, IReadOnlyList<string> sources)
    {
#if DOTCL_EMIT
        var moduleName = Path.GetFileNameWithoutExtension(faslPath)
            + "_" + Guid.NewGuid().ToString("N").Substring(0, 8);
        var faslAsm = new DotCL.Emitter.FaslAssembler(moduleName);

        var compileFileModeSym = Startup.Sym("*COMPILE-FILE-MODE*");
        var packageSym = Startup.Sym("*PACKAGE*");
        var readtableSym = Startup.Sym("*READTABLE*");
        var oldCompileFileMode = DynamicBindings.Get(compileFileModeSym);
        var oldPackage = DynamicBindings.Get(packageSym);
        var oldReadtable = DynamicBindings.Get(readtableSym);
        DynamicBindings.Set(compileFileModeSym, T.Instance);
        try
        {
            foreach (var source in sources)
            {
                DynamicBindings.Set(packageSym, oldPackage);
                DynamicBindings.Set(readtableSym, oldReadtable);
                CompileSourceInto(source, faslAsm, "resolve-deps");
            }
            faslAsm.Save(faslPath);
        }
        finally
        {
            DynamicBindings.Set(compileFileModeSym, oldCompileFileMode);
            DynamicBindings.Set(packageSym, oldPackage);
            DynamicBindings.Set(readtableSym, oldReadtable);
        }
#else
        throw new LispErrorException(new LispProgramError(
            "Building a dependency fasl requires System.Reflection.Emit, "
            + "which is unavailable in this runtime build"));
#endif
    }

    private static string? FindRuntimeCsproj()
    {
        var envPath = System.Environment.GetEnvironmentVariable("DOTCL_RUNTIME_CSPROJ");
        if (!string.IsNullOrEmpty(envPath) && File.Exists(envPath))
            return envPath;

        var baseDir = AppContext.BaseDirectory;
        var candidates = new[]
        {
            // dev tree: runtime/bin/{Debug,Release}/netXX -> 4 levels up to repo root
            Path.Combine(baseDir, "..", "..", "..", "..", "runtime", "runtime.csproj"),
            // less common: exe living next to the csproj
            Path.Combine(baseDir, "runtime.csproj"),
        };
        return candidates.Select(Path.GetFullPath).FirstOrDefault(File.Exists);
    }

    private static string MapTargetToRid(LispObject target)
    {
        string name = target is Symbol s ? s.Name
                    : target is LispString ls ? ls.Value
                    : target.ToString();
        return name.ToUpperInvariant() switch
        {
            "WIN-X64"    or "WIN-AMD64"   => "win-x64",
            "WIN-X86"                     => "win-x86",
            "WIN-ARM64"                   => "win-arm64",
            "LINUX-X64"  or "LINUX-AMD64" => "linux-x64",
            "LINUX-ARM64"                 => "linux-arm64",
            "LINUX-ARM"  or "LINUX-ARM32" => "linux-arm",
            "OSX-X64"    or "OSX-AMD64"   => "osx-x64",
            "OSX-ARM64"  or "MAC-ARM64"   => "osx-arm64",
            _ => throw new LispErrorException(new LispProgramError(
                $"SAVE-APPLICATION: unknown :target {name}. "
                + "Use :win-x64, :win-arm64, :linux-x64, :linux-arm64, :osx-x64, :osx-arm64"))
        };
    }

    /// <summary>
    /// A source-list argument (:load, :prelude) as physical paths. Accepts one
    /// designator or a list of them.
    /// </summary>
    private static List<string> NormalizeSourceList(LispObject? arg)
    {
        var paths = new List<string>();
        if (arg == null || arg is Nil) return paths;
        if (arg is Cons)
        {
            var cur = arg;
            while (cur is Cons c)
            {
                paths.Add(ResolvePhysicalPath(c.Car));
                cur = c.Cdr;
            }
        }
        else
        {
            paths.Add(ResolvePhysicalPath(arg));
        }
        return paths;
    }

    /// <summary>
    /// Use ASDF (must already be loaded) to collect the transitive set of
    /// CL source files for SYSTEM-DESIGNATOR, in topological load order.
    /// Closes over system-depends-on here and asks asdf only for one system's
    /// own files at a time; see the comment on the query for why.
    /// </summary>
    /// <summary>Source files of a system's transitive closure, dependencies first.
    /// SYSTEMS receives the (name, version) of every system walked: read here while
    /// the building image still has a real ASDF registry, so the deployed image can
    /// answer for itself. See the register-preloaded-system emission in
    /// BuildFaslFromSources.</summary>
    private const string RequireModuleMarker = "\u0001require";

    private static List<string> CollectAsdfSystemSources(
        LispObject systemDesignator, out List<(string Name, string Version)> systems)
    {
        // Convert designator to a lowercase string for ASDF
        string sysName = systemDesignator switch
        {
            Symbol sym    => sym.Name.ToLowerInvariant(),
            LispString ls => ls.Value.ToLowerInvariant(),
            _             => systemDesignator.ToString().ToLowerInvariant()
        };

        // Walk the system graph here rather than asking asdf for the whole
        // closure at once. required-components with :other-systems t is not
        // transitively closed: asked for lem-server-min it returns 492 files
        // including dexador's, while asked for dexador alone, same image, same
        // run, it returns 212 that include all of alexandria, which the larger
        // answer omits entirely. A core built from the larger answer compiles and
        // then refuses to load with "No package named ALEXANDRIA".
        //
        // It is reliable for one system at a time, so that is all it is asked:
        // close over system-depends-on to get the systems, order them
        // dependencies-first, and take each system's own files from asdf.
        //
        // Dependency designators are not all plain names: dexador declares
        // (FEATURE WINDOWS winhttp) and (VERSION uiop "3.1.1") among others. A
        // :feature clause whose expression is false is not a dependency at all,
        // and :require names a module rather than a system.
        //
        // A dependency that cannot be found, or whose .asd fails to load, stops
        // the build with the system named. Skipping it left a closure without
        // that system's files -- and when the root's own dependencies were the
        // ones missing, without the root's files too -- which compiled into an
        // image that looked built and died at its first call.
        var exprStr = $@"(let ((seen (make-hash-table :test 'equal))
                               (order '())
                               (problems '()))
                           (labels
                             ((dep-name (d)
                                (cond ((stringp d) d)
                                      ((symbolp d) (string-downcase (symbol-name d)))
                                      ((consp d)
                                       (let ((head (string-upcase (string (car d)))))
                                         (cond ((string= head ""VERSION"") (dep-name (second d)))
                                               ((string= head ""FEATURE"")
                                                (when (uiop:featurep (second d))
                                                  (dep-name (third d))))
                                               (t nil))))
                                      (t nil)))
                              (note-problem (key parent what)
                                (push (if parent
                                          (format nil ""~a: ~a (required by ~a)"" key what parent)
                                          (format nil ""~a: ~a"" key what))
                                      problems))
                              (visit (name &optional parent)
                                (let ((key (string-downcase name)))
                                  (unless (gethash key seen)
                                    (setf (gethash key seen) t)
                                    (let* ((failed nil)
                                           (sys (handler-case (asdf:find-system key nil)
                                                  (error (c)
                                                    (setf failed t)
                                                    (note-problem
                                                     key parent
                                                     (format nil ""error loading its definition: ~a"" c))
                                                    nil))))
                                      (cond
                                        (sys
                                         (dolist (d (asdf:system-depends-on sys))
                                           (let ((n (dep-name d))) (when n (visit n key))))
                                         (push sys order))
                                        ((not failed)
                                         (note-problem key parent ""not found"")))))))
                              ;; What a system asks ASDF to prepare beyond the
                              ;; dependencies its .asd lists. cl-unicode ships
                              ;; lists.lisp, hash-tables.lisp and methods.lisp as
                              ;; generated files and adds (load-op cl-unicode/build)
                              ;; here until they exist; a walk that never asks
                              ;; reaches a source file that is not there.
                              (generated-deps (sys)
                                (let ((declared (remove nil (mapcar #'dep-name
                                                                    (asdf:system-depends-on sys))))
                                      (self (asdf:component-name sys))
                                      (extra '()))
                                  (dolist (entry
                                            (or (ignore-errors
                                                  (asdf:component-depends-on
                                                    (asdf:make-operation 'asdf:prepare-op) sys))
                                                '()))
                                    (when (consp entry)
                                      (dolist (d (rest entry))
                                        (let ((n (dep-name d)))
                                          (when (and n
                                                     (not (string-equal n self))
                                                     (not (member n declared :test #'string-equal)))
                                            (push (cons (first entry) n) extra))))))
                                  (nreverse extra)))
                              ;; Only the extra entries are operated, never
                              ;; prepare-op itself: prepare-op on a system also
                              ;; load-ops everything it depends on, and this image
                              ;; is about to compile those same sources. Running
                              ;; their compile-time effects twice is how
                              ;; cl-interpol's defreadtable ends in
                              ;; reader-macro-conflict.
                              ;;
                              ;; A generator is upstream code run for its files,
                              ;; and it can stop part way with the files that
                              ;; matter already written, so a failure here is
                              ;; reported rather than fatal. Whether the build has
                              ;; what it needs is answered when each source is
                              ;; compiled.
                              (ensure-generated (sys)
                                (dolist (e (generated-deps sys))
                                  (handler-case
                                      (handler-bind ((warning #'muffle-warning))
                                        (asdf:operate (car e) (cdr e)))
                                    (error (c)
                                      (format *error-output*
                                              ""~&; note: generating ~a for ~a stopped: ~a~%""
                                              (cdr e) (asdf:component-name sys) c)
                                      (let* ((outputs
                                               (ignore-errors
                                                 (asdf:output-files (car e) (cdr e))))
                                             (missing (remove-if #'probe-file outputs)))
                                        (cond
                                          (missing
                                           (format *error-output*
                                                   "";        it had not written yet:~%"")
                                           (dolist (m missing)
                                             (format *error-output* "";          ~a~%"" m)))
                                          ;; Stopped after its files were written
                                          ;; (cl-unicode's generator can stop in its
                                          ;; test-data step): say that the build has
                                          ;; them, so the note does not read as the
                                          ;; failure of this build.
                                          (outputs
                                           (format *error-output*
                                                   "";        the files it generates are all present, so the build goes on~%"")))))))))
                             (visit ""{sysName}"")
                           (when problems
                             (error ""~a""
                                    (with-output-to-string (o)
                                      (format o ""cannot build system ~a: ~
                                                 systems in its closure could not be resolved:""
                                              ""{sysName}"")
                                      (dolist (p (reverse problems))
                                        (format o ""~%  ~a"" p))
                                      (format o ""~%Make them visible to ASDF ~
                                                 (--asd-search-path, CL_SOURCE_REGISTRY).""))))
                           (let ((files '())
                                 (emitted (make-hash-table :test 'equal))
                                 (ordered (nreverse order)))
                             (dolist (sys ordered)
                               (ensure-generated sys)
                               (dolist (c (handler-case
                                              (asdf:required-components sys :other-systems nil)
                                            (error (e)
                                              (error ""cannot build system ~a: ~
                                                       listing the components of ~a failed: ~a""
                                                     ""{sysName}"" (asdf:component-name sys) e))))
                                 (when (typep c 'asdf:cl-source-file)
                                   (let ((p (namestring (asdf:component-pathname c))))
                                     (unless (gethash p emitted)
                                       (setf (gethash p emitted) t)
                                       (push p files))))))
                             ;; Second value: (name version) for each system in the
                             ;; closure, read here while the build image still has a
                             ;; real registry. The deployed image has no .asd files,
                             ;; so this is the only chance to learn the versions.
                             (list (nreverse files)
                                   (mapcar (lambda (sys)
                                             (list (asdf:component-name sys)
                                                   ;; A system that is a module of the image
                                                   ;; (dotcl-bsd-sockets, found through REQUIRE)
                                                   ;; has no files in the walk: it is marked,
                                                   ;; and the deployed image requires it.
                                                   (if (typep sys 'asdf::require-system)
                                                       (concatenate 'string (string (code-char 1)) ""require"")
                                                       (let ((v (ignore-errors
                                                                  (asdf:component-version sys))))
                                                         (if (stringp v) v """")))))
                                           ordered)))))";

        LispObject result;
        try
        {
            var reader = new Reader(new System.IO.StringReader(exprStr));
            if (!reader.TryRead(out var form))
                throw new InvalidOperationException("failed to read ASDF query form");
            result = Runtime.Eval(form!);
        }
        // Only a genuine .NET failure is rewrapped. A Lisp error, and the
        // exceptions that carry a non-local exit (a HANDLER-CASE, restart, THROW
        // or RETURN-FROM around the build), pass through as they are: rewrapping
        // one turned a caller's handler-case into this message.
        catch (Exception ex) when (ex is not (LispErrorException
                                               or HandlerCaseInvocationException
                                               or RestartInvocationException
                                               or CatchThrowException
                                               or BlockReturnException
                                               or GoException))
        {
            throw new LispErrorException(new LispProgramError(
                $"SAVE-APPLICATION :system {sysName}: error querying ASDF: {ex.Message}. "
                + "Ensure ASDF is loaded before calling save-application with :system."));
        }

        var files = new List<string>();
        systems = new List<(string Name, string Version)>();
        var cur = result is Cons top ? top.Car : result;
        while (cur is Cons c)
        {
            if (c.Car is LispString ls)
                files.Add(ls.Value);
            else if (c.Car is LispPathname pn)
                files.Add(pn.ToNamestring());
            cur = c.Cdr;
        }
        var sysList = result is Cons top2 && top2.Cdr is Cons second ? second.Car : Nil.Instance;
        while (sysList is Cons sc)
        {
            if (sc.Car is Cons pair
                && pair.Car is LispString nameStr
                && pair.Cdr is Cons vcell && vcell.Car is LispString verStr)
                systems.Add((nameStr.Value, verStr.Value));
            sysList = sc.Cdr;
        }
        return files;
    }

    private static string? FindBuiltCore()
    {
        var baseDir = AppContext.BaseDirectory;
        var candidates = new[]
        {
            // dev tree: produced by `make compile-core-fasl`
            Path.Combine(baseDir, "..", "..", "..", "..", "compiler", "dotcl.core"),
            // bundled next to running exe
            Path.Combine(baseDir, "dotcl.core"),
        };
        return candidates.Select(Path.GetFullPath).FirstOrDefault(File.Exists);
    }

    private static string TailLines(string s, int n)
    {
        if (string.IsNullOrEmpty(s)) return "";
        var lines = s.TrimEnd().Split('\n');
        int start = Math.Max(0, lines.Length - n);
        return string.Join("\n", lines.Skip(start));
    }

    private static void CopyDirectory(string src, string dst)
    {
        Directory.CreateDirectory(dst);
        foreach (var file in Directory.GetFiles(src))
            File.Copy(file, Path.Combine(dst, Path.GetFileName(file)), overwrite: true);
        foreach (var dir in Directory.GetDirectories(src))
            CopyDirectory(dir, Path.Combine(dst, Path.GetFileName(dir)));
    }

    /// <summary>
    /// Resolve a "PKG:NAME" / "PKG::NAME" / "NAME" string into a Symbol.
    /// Called at save-application time when :toplevel was given as a string.
    /// </summary>
    private static Symbol ResolveSymbolName(string designator)
    {
        int idx = designator.IndexOf(':');
        if (idx < 0)
            return Startup.Sym(designator.ToUpperInvariant());

        string pkgName = designator.Substring(0, idx).ToUpperInvariant();
        int nameStart = idx + 1;
        if (nameStart < designator.Length && designator[nameStart] == ':')
            nameStart++;
        string symName = designator.Substring(nameStart).ToUpperInvariant();

        var pkg = Package.FindPackage(pkgName);
        if (pkg == null)
            throw new LispErrorException(new LispProgramError(
                $"SAVE-APPLICATION: package {pkgName} not found for :toplevel {designator}"));
        var (sym, _) = pkg.Intern(symName);
        return sym;
    }

#if DOTCL_EMIT
    /// <summary>
    /// Read a .lisp source and emit each top-level form into the given FASL assembler.
    /// Mirrors the core loop of CompileFile but without warnings tracking or SIL output
    /// (MVP scope). Respects eval-when per CLHS 3.2.3.1.
    /// </summary>
    // Deliberately NOT the compile-file rule above, which runs only the forms
    // CLHS gives compile-time semantics (defmacro, defpackage, defclass, ...) and
    // merely emits the rest. That rule is right for one file compiled against an
    // image that already holds its dependencies. Here a whole dependency closure
    // is compiled in order, and a macro in a later file routinely expands by
    // calling a function an earlier file defined: emit-without-running leaves
    // that function absent for the remainder of the build. So every top-level
    // form is run exactly once as it is emitted: the load-as-you-go that ASDF's
    // bundle operations perform, and that the intended clean-image build needs
    // anyway.
    //
    // log4cl is where this stops being theoretical. Its defs.lisp
    // fmakunbounds a list of names under (eval-when (:compile-toplevel :execute)
    // ...), :load-toplevel deliberately absent, so that it survives being
    // loaded over an older version. Compiling it therefore deletes those
    // definitions from the building image, while the defgeneric in naming.lisp
    // that would restore them is emitted rather than run. Every later
    // macroexpansion reaching naming-option then fails, which is what log:info
    // does, which is what Lem uses.
    private static void CompileSourceInto(string inputPath, DotCL.Emitter.FaslAssembler faslAsm,
                                          string what = "SAVE-APPLICATION")
    {
        if (!File.Exists(inputPath))
            throw new LispErrorException(new LispProgramError(
                $"SAVE-APPLICATION: source file not found: {inputPath}"));

        var source = File.ReadAllText(inputPath);

        // Say which file is being processed, the way compile-file and load do.
        // Locating a failure in a closure of several hundred sources is otherwise
        // guesswork: the build is silent for minutes and names no file when it
        // stops.
        var full = Path.GetFullPath(inputPath);

        // A source file that finds a data file beside itself is an ordinary
        // pattern: cl-mustache reads its version.lisp-expr through
        // (merge-pathnames "version.lisp-expr"
        //                  (or *compile-file-pathname* *load-truename*))
        // in a #. at read time. Leaving these unbound makes merge-pathnames fall
        // back to *default-pathname-defaults*, i.e. whatever directory the build
        // was started from, and the file is simply not there. Bind them per
        // source, as compile-file does for the one file it is given.
        var cfpSym = Startup.Sym("*COMPILE-FILE-PATHNAME*");
        var cftSym = Startup.Sym("*COMPILE-FILE-TRUENAME*");
        var loadPathSym = Startup.Sym("*LOAD-PATHNAME*");
        var loadTrueSym = Startup.Sym("*LOAD-TRUENAME*");
        var oldCfp = DynamicBindings.Get(cfpSym);
        var oldCft = DynamicBindings.Get(cftSym);
        var oldLoadPath = DynamicBindings.Get(loadPathSym);
        var oldLoadTrue = DynamicBindings.Get(loadTrueSym);
        var here = LispPathname.FromString(full);
        DynamicBindings.Set(cfpSym, here);
        DynamicBindings.Set(cftSym, here);
        // Set the load variables too: this pass is a load as much as a
        // compilation, and code that reaches for one or the other should find
        // the same answer.
        DynamicBindings.Set(loadPathSym, here);
        DynamicBindings.Set(loadTrueSym, here);

        SortedDictionary<string, bool>? hostFeaturePrev = null;
        try
        {
            var reader = new Reader(new StringReader(source));
            hostFeaturePrev = HostFeatures.Begin(faslAsm.HostFeatureRecord);

            while (reader.TryRead(out var form))
            {
                foreach (var subForm in FlattenTopLevel(form))
                {
                    if (IsEvalWhenForCompileFile(subForm, out var ewBody,
                        out bool hasCT, out bool hasLT))
                    {
                        var prognForm = MakeProgn(ewBody);
                        var bodyInstrList = CompileTopLevel(prognForm);
                        // Once, not once per situation: a form marked both
                        // :compile-toplevel and :load-toplevel must not run twice.
                        if (hasCT || hasLT)
                            DotCL.Emitter.CilAssembler.AssembleAndRun(bodyInstrList);
                        if (hasLT)
                            faslAsm.AddTopLevelForm(bodyInstrList);
                    }
                    else
                    {
                        var instrList = CompileTopLevel(subForm);
                        DotCL.Emitter.CilAssembler.AssembleAndRun(instrList);
                        faslAsm.AddTopLevelForm(instrList);
                    }
                }
            }
        }
        catch (Exception e) when (e is LispErrorException)
        {
            // Name the file. Without this the report is a bare condition and the
            // only way to find the source is to bisect the closure by hand.
            throw new LispErrorException(new LispProgramError(
                $"{what}: while processing {full}: "
                + ((LispErrorException)e).Condition));
        }
        finally
        {
            DynamicBindings.Set(cfpSym, oldCfp);
            DynamicBindings.Set(cftSym, oldCft);
            DynamicBindings.Set(loadPathSym, oldLoadPath);
            DynamicBindings.Set(loadTrueSym, oldLoadTrue);
            HostFeatures.End(hostFeaturePrev);
        }
    }
#endif
}
