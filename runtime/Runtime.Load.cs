using System.Collections.Concurrent;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    // --- Load ---

    /// <summary>
    /// 3-arg overload used by the compiler's direct (:call "Runtime.Load") CIL instruction.
    /// </summary>
    public static LispObject Load(LispObject filespec, LispObject verbose, LispObject print)
        => Load(new LispObject[] { filespec,
            Startup.Sym("VERBOSE"), verbose,
            Startup.Sym("PRINT"), print });

    /// <summary>
    /// CL LOAD with keyword arguments: (load filespec &amp;key verbose print if-does-not-exist external-format)
    /// </summary>
    public static LispObject Load(LispObject[] args)
    {
        if (args.Length == 0)
            throw new LispErrorException(new LispProgramError("LOAD: requires at least 1 argument"));

        var filespec = args[0];

        // Parse keyword arguments from args[1..]
        LispObject? verboseArg = null;
        LispObject? printArg = null;
        LispObject? ifDoesNotExistArg = null;
        bool allowOtherKeys = false;

        // First pass: check for :allow-other-keys
        for (int i = 1; i + 1 < args.Length; i += 2)
        {
            if (args[i] is Symbol ks && ks.Name == "ALLOW-OTHER-KEYS" && args[i + 1] is not Nil)
            {
                allowOtherKeys = true;
                break;
            }
        }

        // Check for odd number of keyword arguments
        if ((args.Length - 1) % 2 != 0)
            throw new LispErrorException(new LispProgramError("LOAD: odd number of keyword arguments"));

        // Second pass: parse known keys
        for (int i = 1; i + 1 < args.Length; i += 2)
        {
            string key = args[i] switch
            {
                Symbol s => s.Name,
                _ => args[i].ToString()
            };
            switch (key)
            {
                case "VERBOSE": verboseArg ??= args[i + 1]; break;
                case "PRINT": printArg ??= args[i + 1]; break;
                case "IF-DOES-NOT-EXIST": ifDoesNotExistArg ??= args[i + 1]; break;
                case "EXTERNAL-FORMAT": break; // accepted but ignored
                case "ALLOW-OTHER-KEYS": break; // already handled
                default:
                    if (!allowOtherKeys)
                        throw new LispErrorException(new LispProgramError($"LOAD: unrecognized keyword argument :{key}"));
                    break;
            }
        }

        // Defaults from *load-verbose* and *load-print*
        var verboseSym = Startup.Sym("*LOAD-VERBOSE*");
        var printSym = Startup.Sym("*LOAD-PRINT*");
        LispObject verbose = verboseArg ?? DynamicBindings.Get(verboseSym);
        LispObject print = printArg ?? DynamicBindings.Get(printSym);

        bool isVerbose = verbose is not Nil;
        bool isPrint = print is not Nil;

        // CLHS: filespec can be a stream: read and eval forms from it directly
        if (filespec is LispStream)
        {
            if (isVerbose)
            {
                var w = GetStandardOutputWriter();
                w.WriteLine("; Loading from stream");
                w.Flush();
            }

            TextReader streamReader = GetTextReader(filespec);
            var reader = new Reader(streamReader);

            // Save and bind *package* and *readtable* per CLHS (but NOT *load-pathname*/*load-truename* for streams)
            var packageSym = Startup.Sym("*PACKAGE*");
            var readtableSym2 = Startup.Sym("*READTABLE*");
            var oldPackage = DynamicBindings.Get(packageSym);
            var oldReadtable2 = DynamicBindings.Get(readtableSym2);
            int streamBindDepth = DynamicBindings.Depth;
            DynamicBindings.Push(packageSym, oldPackage);
            DynamicBindings.Push(readtableSym2, oldReadtable2);
            PushPolicyBindings();

            try
            {
                LispObject result = Nil.Instance;
                while (reader.TryRead(out var form))
                {
                    foreach (var subForm in FlattenTopLevel(form))
                    {
                        var instrList = CompileTopLevel(subForm);
                        result = DotCL.Emitter.CilAssembler.AssembleAndRun(instrList);
                    }

                    if (isPrint)
                    {
                        var w = GetStandardOutputWriter();
                        w.WriteLine(FormatTop(result, true));
                        w.Flush();
                    }
                }
                return T.Instance;
            }
            finally
            {
                DynamicBindings.TruncateTo(streamBindDepth);
            }
        }

        string filePath = ResolvePhysicalPath(filespec);

        // Merge with *default-pathname-defaults* if file not found at given path
        if (!File.Exists(filePath))
        {
            var dpd = DynamicBindings.Get(Startup.Sym("*DEFAULT-PATHNAME-DEFAULTS*"));
            if (dpd is LispPathname dpdPath)
            {
                var merged = LispPathname.FromString(filePath).MergeWith(dpdPath).ToNamestring();
                if (File.Exists(merged))
                    filePath = merged;
            }
        }

        if (!File.Exists(filePath))
        {
            // :if-does-not-exist nil => return NIL instead of signaling
            if (ifDoesNotExistArg != null && ifDoesNotExistArg is Nil)
                return Nil.Instance;
            var err = new LispError($"LOAD: file not found: {filePath}");
            err.ConditionTypeName = "FILE-ERROR";
            err.FileErrorPathnameRef = filespec;
            throw new LispErrorException(err);
        }

        if (isVerbose)
        {
            var w = GetStandardOutputWriter();
            w.WriteLine($"; Loading {filePath}");
            w.Flush();
        }

        // Handle persisted .NET assembly: detect by PE header ("MZ"), not extension.
        // One open answers everything LOAD asks of the file before loading it: on
        // Windows the first open of a freshly written PE is a full antivirus scan.
#if NET9_0_OR_GREATER
        if (TakeCompiledFasl(filePath) is { } compiled)
        {
            try { return LoadFasl(filePath, null, filespec, isVerbose, isPrint, compiled); }
            finally { WhitelistLoadedDefinitionsForCompileFile(); }
        }
#endif
        var peFile = ProbePeFile(filePath);
        if (peFile != null)
        {
            try { return LoadFasl(filePath, peFile, filespec, isVerbose, isPrint); }
            finally { WhitelistLoadedDefinitionsForCompileFile(); }
        }

        if (HasFaslExtension(filePath))
            throw FaslFileError(
                $"LOAD: not a dotcl fasl (foreign Lisp fasl?): {filePath}", filespec);

        var source = File.ReadAllText(filePath);
        var loadStringReader = new StringReader(source);
        // Position-tracking, like CompileFile below. Without it FILE-POSITION on
        // the stream LOAD hands to a reader macro answers NIL, and a reader macro
        // that records source locations (eclector, and so coalton, whose reader
        // macro runs its whole front end) does arithmetic on that NIL. Reading the
        // very same file with COMPILE-FILE or WITH-OPEN-FILE reports a position,
        // so the failure looked like it belonged to the library.
        var loadTrackingReader = new PositionTrackingReader(loadStringReader);
        var reader2 = new Reader(loadTrackingReader) { LineSource = source };
        // Same live-reader/share-table link as CompileFile: a Lisp reader macro
        // reading elements via (read stream) must reuse THIS reader so per-form
        // share-table clearing doesn't run per element (see CompileFile).
        var loadStream = new LispStringInputStream(loadTrackingReader, 0, source, source.Length);
        if (LispStringInputStream.Utf8Preamble(filePath) is int loadPre && loadPre >= 0)
            loadStream.ReportFileBytes(loadPre);
        reader2.LispStreamRef = loadStream;
        loadStream.CachedReader = reader2;
        reader2.AdoptStreamShareTables(loadStream);

        // Save and bind *load-pathname*, *load-truename*, *package*, and *readtable* per CLHS
        // CLHS: *load-pathname* = (merge-pathnames filespec *default-pathname-defaults*)
        // CLHS: *load-truename* = (truename *load-pathname*)
        // CLHS: LOAD binds *readtable* and *package* to their pre-load values
        var loadPathSym = Startup.Sym("*LOAD-PATHNAME*");
        var loadTrueSym = Startup.Sym("*LOAD-TRUENAME*");
        {
            var packageSym = Startup.Sym("*PACKAGE*");
            var readtableSym = Startup.Sym("*READTABLE*");
            var oldPackage = DynamicBindings.Get(packageSym);
            var oldReadtable = DynamicBindings.Get(readtableSym);
            var loadInputPath = filespec switch
            {
                LispPathname p => p,
                LispString s => LispPathname.FromString(s.Value),
                _ => LispPathname.FromString(filespec.ToString())
            };
            var dpdForLoad = DynamicBindings.Get(Startup.Sym("*DEFAULT-PATHNAME-DEFAULTS*"));
            var mergedLoadPath = MergePathnames(loadInputPath, dpdForLoad is LispPathname ? dpdForLoad : LispPathname.FromString(""));
            int loadBindDepth = DynamicBindings.Depth;
            DynamicBindings.Push(loadPathSym, mergedLoadPath);
            DynamicBindings.Push(loadTrueSym, LispPathname.FromString(Path.GetFullPath(filePath)));
            DynamicBindings.Push(packageSym, oldPackage);
            DynamicBindings.Push(readtableSym, oldReadtable);
            PushPolicyBindings();

            var savedSourceLines = SourceLineTable;
            var savedSourcePositions = SourcePositionTable;
            BeginCompilerUnit(filePath);
            try
            {
                LispObject result = Nil.Instance;
                bool isCompiled = filePath.EndsWith(".sil", StringComparison.OrdinalIgnoreCase)
                    || filePath.EndsWith(".ufsl", StringComparison.OrdinalIgnoreCase)
                    || source.StartsWith(";; -*- dotcl-compiled -*-");
                if (!isCompiled)
                {
                    SourceLineTable = new Dictionary<object, (int, int, int, int)>(ReferenceEqualityComparer.Instance);
                    SourcePositionTable = new Dictionary<object, (int, int)>(ReferenceEqualityComparer.Instance);
                }

                while (reader2.TryRead(out var form))
                {
                    int formLine = reader2.LastFormLine;
                    SetCompilerTopLevel(form, formLine);
                    if (!isCompiled) RecordDefinitionSources(form, filePath, formLine, useSourceLines: true);
                    try
                    {
                        if (isCompiled)
                        {
                            // .sil file: form is already a compiled instruction list
                            result = DotCL.Emitter.CilAssembler.AssembleAndRun(form);
                        }
                        else
                        {
                            // .lisp file: compile each top-level form
                            var defNames = new List<(DefinitionEntry Entry, int Line)>();
                            var savedDefNames = s_flattenedDefinitionNames;
                            var savedRunning = s_runningTopLevelForm;
                            s_flattenedDefinitionNames = defNames;
                            s_runningTopLevelForm = false;
                            try
                            {
                            foreach (var subForm in FlattenTopLevel(form, null, formLine))
                            {
#if DOTCL_EMIT
                                var instrList = CompileTopLevel(subForm, TopLevelFormXrefName(filePath, formLine, form));
                                s_runningTopLevelForm = true;
                                try { result = DotCL.Emitter.CilAssembler.AssembleAndRun(instrList); }
                                finally { s_runningTopLevelForm = false; }
#else
                                // No Reflection.Emit in this build: the tree-walk
                                // interpreter is the only evaluator, so a source file
                                // is loaded by evaluating its top-level forms. Without
                                // this branch an emit-free runtime could boot its FASL
                                // core and answer --eval, but LOAD of any .lisp died on
                                // the CilAssembler guard: which is why the suite had
                                // never been run on such a build.
                                result = Eval(subForm);
#endif
                            }
                            }
                            finally
                            {
                                s_flattenedDefinitionNames = savedDefNames;
                                s_runningTopLevelForm = savedRunning;
                            }
                            RecordDefinitionNames(ExpandedDefinitionNames(form, defNames), filePath, formLine);
                        }
                    }
                    catch (Exception ex)
                    {
                        // A THROW whose catcher is gone is not control flow any more,
                        // it is the CONTROL-ERROR of CLHS 5.2. Checked before the
                        // control-flow test below, which would otherwise let it past
                        // LOAD and out of the process: a library with a load-time
                        // THROW that misses its catcher killed the host instead of
                        // signalling something a handler could take. EVAL has had
                        // this check; LOAD did not.
                        var unmatched = Runtime.UnmatchedThrowError(ex);
                        if (unmatched != null) throw unmatched;
                        // Don't wrap control flow exceptions: they must reach their
                        // intended catch blocks (handler-case, block, catch, go, restart).
                        if (Runtime.IsLispControlFlowException(ex))
                            throw;
                        throw new LispSourceException(filePath, formLine, ex);
                    }

                    if (isPrint)
                    {
                        var w = GetStandardOutputWriter();
                        w.WriteLine(FormatTop(result, true));
                        w.Flush();
                    }
                    if (!isCompiled)
                    {
                        SourceLineTable?.Clear();
                        SourcePositionTable?.Clear();
                    }
                }
                return T.Instance;
            }
            finally
            {
                EndCompilerUnit();
                SourceLineTable = savedSourceLines;
                SourcePositionTable = savedSourcePositions;
                WhitelistLoadedDefinitionsForCompileFile();
                DynamicBindings.TruncateTo(loadBindDepth);
            }
        }
    }

    /// <summary>What LOAD learns about a PE file before loading it.</summary>
    private sealed class PeFile
    {
        public bool ReadyToRun;
#if NETSTANDARD2_0
        /// <summary>The whole image: this build loads fasls from bytes.</summary>
        public byte[]? Image;
#endif
    }

    /// <summary>Null when the file is not a PE (.NET) image, judged by its first two
    /// bytes ("MZ"), or cannot be read. Otherwise what loading it needs to know,
    /// from this one open.</summary>
    private static PeFile? ProbePeFile(string filePath)
    {
        try
        {
            using var fs = File.OpenRead(filePath);
            if (fs.ReadByte() != 'M' || fs.ReadByte() != 'Z') return null;
            fs.Position = 0;
#if NETSTANDARD2_0
            // No AssemblyLoadContext here, so the fasl is loaded from bytes.
            var image = new byte[fs.Length];
            int n = 0, r;
            while (n < image.Length && (r = fs.Read(image, n, image.Length - n)) > 0) n += r;
            if (n != image.Length) return null;
            return new PeFile { Image = image, ReadyToRun = IsReadyToRunImage(new MemoryStream(image, false)) };
#else
            return new PeFile { ReadyToRun = IsReadyToRunImage(fs) };
#endif
        }
        catch { return null; }
    }

    /// <summary>A .fasl that is not a PE image belongs to some other Lisp: the
    /// extension is shared across implementations and their files turn up in
    /// quicklisp dists and bundle directories. dotcl fasls are always .NET
    /// assemblies, so treat this as "foreign fasl" rather than reading the
    /// binary as source text (which reports a reader error about whatever byte
    /// came first).</summary>
    private static bool IsForeignFasl(string filePath) =>
        HasFaslExtension(filePath) && !HasPeSignature(filePath);

    private static bool HasFaslExtension(string filePath) =>
        string.Equals(Path.GetExtension(filePath), ".fasl", StringComparison.OrdinalIgnoreCase);

    /// <summary>Whether the file starts with the PE signature "MZ".</summary>
    private static bool HasPeSignature(string filePath)
    {
        try
        {
            using var fs = File.OpenRead(filePath);
            return fs.ReadByte() == 'M' && fs.ReadByte() == 'Z';
        }
        catch { return false; }
    }

    private static LispErrorException FaslFileError(string message, LispObject filespec)
    {
        var err = new LispError(message);
        err.ConditionTypeName = "FILE-ERROR";
        err.FileErrorPathnameRef = filespec;
        return new LispErrorException(err);
    }

    /// <summary>Load a .fasl (persisted .NET assembly) file.</summary>

    /// <summary>
    /// Run a fasl assembly's CompiledModule.ModuleInit, which is what actually
    /// installs its definitions. A .NET assembly that is not a dotcl fasl is
    /// reported as a FILE-ERROR naming the file rather than as a bare
    /// reflection failure.
    /// </summary>
    private static void RunFaslModuleInit(System.Reflection.Assembly asm,
        string filePath, LispObject filespec)
    {
        Type? moduleType;
        try { moduleType = asm.GetType("CompiledModule"); }
        catch (IOException ioe) when (IsNewerRuntimeBindFailure(ioe))
        {
            SignalNewerRuntimeFasl(filePath, NeededRuntimeVersion(ioe), RunningRuntimeVersion());
            throw;
        }
        moduleType = moduleType
            ?? throw FaslFileError(
                $"LOAD: not a dotcl fasl (.NET assembly without a CompiledModule type): {filePath}",
                filespec);
        var initMethod = moduleType.GetMethod("ModuleInit")
            ?? throw FaslFileError(
                $"LOAD: not a dotcl fasl (CompiledModule has no ModuleInit method): {filePath}",
                filespec);
        try
        {
            var result = (LispObject?)initMethod.Invoke(null, null) ?? Nil.Instance;
        }
        catch (System.Reflection.TargetInvocationException tie) when (tie.InnerException != null)
        {
            if (IsNewerRuntimeBindFailure(tie.InnerException))
                SignalNewerRuntimeFasl(filePath, NeededRuntimeVersion(tie.InnerException), RunningRuntimeVersion());
            throw tie.InnerException;
        }
        catch (IOException ioe) when (IsNewerRuntimeBindFailure(ioe))
        {
            SignalNewerRuntimeFasl(filePath, NeededRuntimeVersion(ioe), RunningRuntimeVersion());
            throw;
        }
    }

    /// <summary>
    /// If ASM is a bundle written by dotcl:combine-fasls, load every part it
    /// carries, in bundled order, and return true. Returns false for an ordinary
    /// fasl so the caller falls through to the single-module path.
    ///
    /// The parts are resources rather than merged IL: a fasl is a .NET assembly,
    /// and merging two of those means reconciling their types and symbol tables,
    /// while a container preserves each part exactly as compile-file produced it.
    /// The cost is that the parts load from bytes, so they have no file Location.
    /// </summary>
    private static bool LoadFaslBundleParts(System.Reflection.Assembly asm,
        string filePath, LispObject filespec, bool warnOnly)
    {
        var names = new List<string>();
        foreach (var n in asm.GetManifestResourceNames())
            if (n.StartsWith(FaslBundlePartPrefix, StringComparison.Ordinal))
                names.Add(n);
        if (names.Count == 0) return false;

        // The index is zero-padded in the name, so ordinal order is bundle order.
        names.Sort(StringComparer.Ordinal);

        foreach (var n in names)
        {
            byte[] bytes;
            using (var s = asm.GetManifestResourceStream(n)
                   ?? throw FaslFileError(
                       $"LOAD: bundle part {n} is unreadable: {filePath}", filespec))
            {
                bytes = new byte[s.Length];
                int read = 0;
                while (read < bytes.Length)
                {
                    int got = s.Read(bytes, read, bytes.Length - read);
                    if (got <= 0) break;
                    read += got;
                }
            }

            System.Reflection.Assembly part;
            try
            {
                part = RuntimeLoadContext.LoadBytes(bytes);
            }
            catch (BadImageFormatException)
            {
                throw FaslFileError(
                    $"LOAD: bundle part {n} is not a .NET assembly: {filePath}", filespec);
            }
            CheckFaslStamp(part, filePath, warnOnly);
            RunFaslModuleInit(part, $"{filePath} [{n}]", filespec);
        }
        return true;
    }

    /// <summary>Write <c>&lt;fasl&gt;.r2r-&lt;rid&gt;</c> next to a freshly compiled fasl,
    /// when DOTCL_R2R_AFTER_COMPILE is set. Loading picks it up automatically
    /// (see TryFindR2rSibling), so ASDF and everything above it need to know
    /// nothing: they still compile and load the same .fasl.
    ///
    /// Off by default because the trade is real and depends on the file. It costs
    /// a crossgen2 run per fasl and roughly 4x the disk, and buys back the JIT at
    /// every load. Worth it for something like Coalton, which is compiled rarely
    /// and loaded often; not obviously worth it for a file being edited.
    ///
    /// Silent on every failure: no crossgen2 on this machine, an unsupported
    /// target, a crossgen2 that refuses the image. The fasl is complete either way
    /// and the loader falls back to it.</summary>
    /// <summary>Write the sibling unconditionally. ASDF compiles to a temporary
    /// output and renames it into place, so a hook inside COMPILE-FILE only ever
    /// sees the temporary name; the caller that knows the final path has to ask.
    /// </summary>
    internal static void WriteR2rSibling(string faslPath)
    {
        try
        {
            var rid = PortableRid();
            var cg = FindCrossgen2(rid);
            if (rid == null || !File.Exists(faslPath)) return;
            if (cg == null)
            {
                // Asked for and unavailable is worth saying once. Every other way
                // this route has failed was silent, and this is the one a user is
                // most likely to hit: crossgen2 is not in the SDK, it is a NuGet
                // pack the SDK restores when something asks for ReadyToRun.
                if (!_warnedNoCrossgen2)
                {
                    _warnedNoCrossgen2 = true;
                    Console.Error.WriteLine(
                        ";; note: dotcl:*compile-r2r* is true but crossgen2 was not found,");
                    Console.Error.WriteLine(
                        ";;       so no ahead-of-time siblings are being written.");
                    Console.Error.WriteLine(
                        $";;       Restore it once with:  dotnet publish -p:PublishReadyToRun=true -r {rid}");
                    Console.Error.WriteLine(
                        ";;       Set dotcl:*compile-r2r* to nil to stop asking.");
                }
                return;
            }

            // crossgen2 wants an assembly-looking input, and writing the output
            // straight onto the final name would leave a truncated sibling behind
            // if it fails midway -- one that is NEWER than the fasl, so the loader
            // would prefer it.
            var tmpIn = Path.Combine(Path.GetTempPath(),
                $"dotcl-r2r-{Guid.NewGuid():N}.dll");
            var tmpOut = tmpIn + ".out";
            File.Copy(faslPath, tmpIn, true);
            try
            {
                var psi = new System.Diagnostics.ProcessStartInfo(cg)
                {
                    UseShellExecute = false,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                };
                Compat.AddArg(psi, tmpIn);
                Compat.AddArg(psi, "-r");
                Compat.AddArg(psi, Path.Combine(
                    System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory(), "*.dll"));
                Compat.AddArg(psi, "-r");
                Compat.AddArg(psi, Path.Combine(AppContext.BaseDirectory, "*.dll"));
                Compat.AddArg(psi, "--targetos");
                Compat.AddArg(psi, rid.StartsWith("win") ? "windows"
                                 : rid.StartsWith("osx") ? "osx" : "linux");
                Compat.AddArg(psi, "--targetarch");
                Compat.AddArg(psi, rid.Substring(rid.LastIndexOf('-') + 1));
                Compat.AddArg(psi, "-O");
                Compat.AddArg(psi, "-o");
                Compat.AddArg(psi, tmpOut);
                using var proc = System.Diagnostics.Process.Start(psi);
                if (proc == null) return;
                proc.StandardOutput.ReadToEnd();
                var err = proc.StandardError.ReadToEnd();
                proc.WaitForExit();
                if (proc.ExitCode != 0 || !File.Exists(tmpOut))
                {
                    if (!_warnedCrossgen2Failed)
                    {
                        _warnedCrossgen2Failed = true;
                        var why = err.Trim();
                        if (why.Length > 300) why = why.Substring(0, 300) + " ...";
                        Console.Error.WriteLine(
                            $";; note: crossgen2 failed on {Path.GetFileName(faslPath)} (exit {proc.ExitCode});");
                        Console.Error.WriteLine(
                            ";;       loading falls back to the IL fasl.");
                        if (why.Length > 0) Console.Error.WriteLine(";;       " + why);
                    }
                    return;
                }
                var dest = faslPath + ".r2r-" + rid;
                Compat.MoveFile(tmpOut, dest, true);
                // The loader ignores a sibling older than its fasl; make sure a
                // filesystem with coarse timestamps cannot make this one look old.
                File.SetLastWriteTimeUtc(dest, DateTime.UtcNow);
            }
            finally
            {
                try { File.Delete(tmpIn); } catch { }
                try { if (File.Exists(tmpOut)) File.Delete(tmpOut); } catch { }
            }
        }
        catch { }
    }

    private static bool _warnedNoCrossgen2;
    private static bool _warnedCrossgen2Failed;

    /// <summary>The crossgen2 host tool from the restored NuGet packages, highest
    /// version. Returns null when it is not on this machine -- it ships with the
    /// SDK's R2R support, not with a dotcl install.</summary>
    private static string? FindCrossgen2(string? rid)
    {
        if (rid == null) return null;
        var home = Environment.GetEnvironmentVariable("NUGET_PACKAGES")
                   ?? Path.Combine(Environment.GetFolderPath(
                          Environment.SpecialFolder.UserProfile), ".nuget", "packages");
        var pkg = Path.Combine(home, $"microsoft.netcore.app.crossgen2.{rid}");
        if (!Directory.Exists(pkg)) return null;
        var exe = rid.StartsWith("win") ? "crossgen2.exe" : "crossgen2";
        string? best = null;
        Version? bestV = null;
        foreach (var dir in Directory.EnumerateDirectories(pkg))
        {
            var cand = Path.Combine(dir, "tools", exe);
            if (!File.Exists(cand)) continue;
            Version.TryParse(Path.GetFileName(dir).Split('-')[0], out var v);
            if (best == null || (v != null && bestV != null && v > bestV)) { best = cand; bestV = v; }
        }
        return best;
    }

    /// <summary>The portable os-arch RID (linux-x64, win-arm64, osx-x64 ...),
    /// which is how the build names its per-RID artifacts.</summary>
    /// <summary>How many fasls were loaded from an ahead-of-time sibling, and how
    /// many were loaded at all. Every way this route has failed so far has been
    /// silent -- a sibling written under a temporary name, one left older than its
    /// fasl, one named for the wrong spelling of the RID -- and each looked exactly
    /// like success from the outside. Two counters make the difference visible.
    /// </summary>
    public static long R2rFaslsLoaded;
    public static long FaslsLoaded;

    /// <summary>os-arch (linux-x64, osx-arm64, win-x64, ...): the name of the
    /// R2R siblings. Never the distro-specific RuntimeIdentifier.</summary>
    public static string? PortableRid()
    {
        string? os = System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
                        System.Runtime.InteropServices.OSPlatform.Windows) ? "win"
                  : System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
                        System.Runtime.InteropServices.OSPlatform.OSX) ? "osx"
                  : System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
                        System.Runtime.InteropServices.OSPlatform.Linux) ? "linux"
                  : null;
        if (os == null) return null;
        string? arch = System.Runtime.InteropServices.RuntimeInformation.ProcessArchitecture switch
        {
            System.Runtime.InteropServices.Architecture.X64 => "x64",
            System.Runtime.InteropServices.Architecture.Arm64 => "arm64",
            System.Runtime.InteropServices.Architecture.X86 => "x86",
            _ => null,
        };
        return arch == null ? null : $"{os}-{arch}";
    }

    static string? _packageRid;

    /// <summary>The RID compiled packages are made and looked up for: PortableRid,
    /// with linux-musl-ARCH on a musl libc.</summary>
    internal static string? PackageRid()
    {
        if (_packageRid != null) return _packageRid;
        string? maps = null;
        var rid = PortableRid();
        if (rid != null && rid.StartsWith("linux-", StringComparison.Ordinal))
            try { maps = File.ReadAllText("/proc/self/maps"); } catch { }
        return _packageRid = PackageRidFor(rid, Compat.RuntimeIdentifier, maps);
    }

    /// <summary>PORTABLE with linux- turned into linux-musl- when the runtime's own
    /// RID or the libraries mapped into the process (MAPS) say musl.</summary>
    internal static string? PackageRidFor(string? portable, string? runtimeRid, string? maps)
    {
        if (portable == null || !portable.StartsWith("linux-", StringComparison.Ordinal))
            return portable;
        bool musl = runtimeRid != null
                    && (runtimeRid.Contains("-musl-") || runtimeRid.StartsWith("alpine", StringComparison.Ordinal))
                 || maps != null && maps.Contains("/ld-musl-");
        return musl ? "linux-musl-" + portable.Substring("linux-".Length) : portable;
    }

    /// <summary>The ahead-of-time sibling of a fasl, or null when there is none.
    /// The launcher needs this for a packed application's own fasl, which lives
    /// outside LOAD. Safe before the core is up: anything needing Lisp state
    /// answers null rather than throwing.</summary>
    public static string? FindR2rSibling(string faslFull)
    {
        try { return TryFindR2rSibling(faslFull); }
        catch { return null; }
    }

    /// <summary>The ahead-of-time compiled sibling of a fasl, if there is one for
    /// this platform and it is not older than the fasl itself. Returns null when
    /// there is none, when it is stale, or when dotcl:*load-r2r* is NIL.</summary>
    private static string? TryFindR2rSibling(string faslFull)
    {
        if (DynamicBindings.Get(Startup.SymInPkg("*LOAD-R2R*", "DOTCL")) is Nil) return null;
        try
        {
            var dir = Path.GetDirectoryName(faslFull);
            if (dir == null) return null;
            var baseName = Path.GetFileName(faslFull);
            // Two spellings. RuntimeIdentifier is the specific one the process was
            // built for (ubuntu.24.04-x64 here), while everything that PRODUCES
            // these files -- the Makefile's HOST_RID, crossgen2's package names,
            // the per-RID contrib fasls -- uses the portable os-arch form. Probe the
            // portable spelling first because that is what the tooling writes;
            // a distro-specific build that pinned its own is still found.
            string? cand = null;
            foreach (var rid in new[] { PortableRid(), Compat.RuntimeIdentifier })
            {
                if (rid == null) continue;
                var c = Path.Combine(dir, $"{baseName}.r2r-{rid}");
                if (File.Exists(c)) { cand = c; break; }
            }
            if (cand == null) return null;
            // Older than the fasl means the fasl was rebuilt and this is left over.
            // Equal is fine: a sibling produced from the fasl can share its second.
            return File.GetLastWriteTimeUtc(cand) >= File.GetLastWriteTimeUtc(faslFull)
                ? cand : null;
        }
        catch { return null; }
    }

    /// <summary>Evaluate FORM without the compiler when it is a call of a
    /// global function on constants, (F 'X 3 :K ...): the creation form most
    /// MAKE-LOAD-FORM methods return, which a fasl evaluates once per object at
    /// load. EVAL compiled each one. Only when F is a function now (not a macro,
    /// not a special operator, and fbound itself) and every argument is a
    /// constant (QUOTE, a keyword, T, NIL or a non-symbol atom), so the call is
    /// what EVAL would run; a compiler macro, which EVAL is free to ignore, is
    /// ignored. False when FORM is anything else, for EVAL to handle.</summary>
    internal static bool TryCallConstantForm(LispObject form, out LispObject result)
    {
        result = Nil.Instance;
        if (form is not Cons c || c.Car is not Symbol op || op.Function is not LispFunction fn)
            return false;
        if (SpecialOperatorP(op) is not Nil || MacroFunction(op) is not Nil
            || Startup.LookupCompilerMacroAsMacro(op) != null)
            return false;
        int n = 0;
        for (var a = c.Cdr; a is Cons ac; a = ac.Cdr)
        {
            var x = ac.Car;
            if (x is Symbol s && s.HomePackage?.Name != "KEYWORD") return false;
            if (x is Cons q && !(q.Car is Symbol qs && qs.Name == "QUOTE"
                                  && qs.HomePackage?.Name == "COMMON-LISP"
                                  && q.Cdr is Cons qa && qa.Cdr is Nil))
                return false;
            n++;
        }
        var args = new LispObject[n];
        int i = 0;
        for (var a = c.Cdr; a is Cons ac; a = ac.Cdr)
            args[i++] = ac.Car is Cons q ? ((Cons)q.Cdr).Car : ac.Car;
        MultipleValues.Reset();
        result = fn.Invoke(args);
        return true;
    }

    /// <summary>Whether the file is a ReadyToRun image: a PE file whose CLI
    /// header names a managed native header. Reads only the headers. False for
    /// anything it cannot parse, which then loads as before.</summary>
    public static bool IsReadyToRunImage(string path)
    {
        try
        {
            using var fs = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite | FileShare.Delete);
            return IsReadyToRunImage(fs);
        }
        catch { return false; }
    }

    /// <summary>The same test on an open stream, read from its start.</summary>
    private static bool IsReadyToRunImage(Stream fs)
    {
        try
        {
            fs.Seek(0, SeekOrigin.Begin);
            var head = new byte[4096];
            int n = 0, r;
            while (n < head.Length && (r = fs.Read(head, n, head.Length - n)) > 0) n += r;
            int U16(int o) => o + 2 <= n ? BitConverter.ToUInt16(head, o) : -1;
            long U32(int o) => o + 4 <= n ? BitConverter.ToUInt32(head, o) : -1;
            if (n < 0x40 || head[0] != (byte)'M' || head[1] != (byte)'Z') return false;
            int pe = (int)U32(0x3C);
            if (pe < 0 || U32(pe) != 0x00004550) return false;          // "PE\0\0"
            int coff = pe + 4;
            int sections = U16(coff + 2);
            int optSize = U16(coff + 16);
            int opt = coff + 20;
            int magic = U16(opt);
            int dirs = magic == 0x20b ? opt + 112 : magic == 0x10b ? opt + 96 : -1;
            if (dirs < 0 || sections <= 0) return false;
            long cliRva = U32(dirs + 14 * 8);
            if (cliRva <= 0) return false;
            // RVA to file offset through the section table.
            int sec = opt + optSize;
            long cliOff = -1;
            for (int i = 0; i < sections; i++, sec += 40)
            {
                long va = U32(sec + 12), size = U32(sec + 8), raw = U32(sec + 20);
                if (va < 0 || size < 0 || raw < 0) return false;
                if (cliRva >= va && cliRva < va + size) { cliOff = cliRva - va + raw; break; }
            }
            if (cliOff < 0) return false;
            // IMAGE_COR20_HEADER: ManagedNativeHeader is the directory at offset 64.
            var cor = new byte[72];
            fs.Seek(cliOff, SeekOrigin.Begin);
            int got = 0;
            while (got < cor.Length && (r = fs.Read(cor, got, cor.Length - got)) > 0) got += r;
            if (got < cor.Length) return false;
            return BitConverter.ToUInt32(cor, 64) != 0 && BitConverter.ToUInt32(cor, 68) != 0;
        }
        catch { return false; }
    }

#if !NETSTANDARD2_0
    /// <summary>Fasl paths this process has loaded by path.</summary>
    private static readonly HashSet<string> s_faslPathsLoaded = new(StringComparer.OrdinalIgnoreCase);

    /// <summary>Load a fasl by path, the first time this process loads that path.
    ///
    /// By path because loading from a byte array costs a second antivirus scan on
    /// Windows (AMSI inspects every image handed over in memory, on top of the
    /// scan of the file when it is opened), and because a file-backed module is
    /// what debuggers and coverage collectors see: they find the .pdb next to it.
    ///
    /// Only the first time: the runtime keeps the image it opened for a path and
    /// hands it back to the next load of the same path, in any load context, even
    /// after the file was replaced. So LOADing a fasl again after recompiling it
    /// would run the old module. A path loaded before is read into memory and
    /// loaded from the bytes, as every fasl used to be.
    ///
    /// A context of its own for each, as Assembly.Load(byte[]) gives each array:
    /// the default context refuses a second assembly of the same name.</summary>
    private static System.Reflection.Assembly LoadFaslFromPath(string faslFull)
    {
        bool first;
        lock (s_faslPathsLoaded) first = s_faslPathsLoaded.Add(faslFull);
        if (!first)
        {
            var bytes = File.ReadAllBytes(faslFull);
            var pdbPath = Path.ChangeExtension(faslFull, ".pdb");
            if (File.Exists(pdbPath))
            {
                try { return RuntimeLoadContext.LoadBytes(bytes, File.ReadAllBytes(pdbPath)); }
                catch (BadImageFormatException) { throw; }
                catch { } // mismatched or unreadable pdb: load without it
            }
            return RuntimeLoadContext.LoadBytes(bytes);
        }
        var context = RuntimeLoadContext.NewContext("fasl " + Path.GetFileName(faslFull));
        return context.LoadFromAssemblyPath(faslFull);
    }
#endif

    /// <summary>How many fasls LOAD took from the module COMPILE-FILE emitted in
    /// memory instead of reading the file.</summary>
    public static long FaslsLoadedInMemory;

#if NET9_0_OR_GREATER
    /// <summary>The fasl COMPILE-FILE wrote last, and the same module emitted in
    /// memory. On Windows, opening a freshly written PE waits for the
    /// antivirus scan of the file, and loading one from bytes waits for AMSI;
    /// a module emitted in memory goes through neither.</summary>
    private sealed class CompiledFasl
    {
        public string Path = "";
        public long Length;
        public DateTime WriteTimeUtc;
        public System.Reflection.Assembly Assembly = null!;
    }

    private static CompiledFasl? s_compiledFasl;
    private static readonly object s_compiledFaslLock = new();

    /// <summary>Off by default; DOTCL_FASL_LOAD_IN_MEMORY=1 turns it on.</summary>
    internal static bool FaslInMemoryLoadEnabled() =>
        Environment.GetEnvironmentVariable("DOTCL_FASL_LOAD_IN_MEMORY") == "1";

    private static bool SamePath(string a, string b) =>
        string.Equals(a, b, OperatingSystem.IsWindows()
            ? StringComparison.OrdinalIgnoreCase : StringComparison.Ordinal);

    internal static void RememberCompiledFasl(string path, System.Reflection.Assembly asm)
    {
        var full = Path.GetFullPath(path);
        var fi = new FileInfo(full);
        var entry = new CompiledFasl
        {
            Path = full, Length = fi.Length, WriteTimeUtc = fi.LastWriteTimeUtc, Assembly = asm,
        };
        lock (s_compiledFaslLock) s_compiledFasl = entry;
    }

    /// <summary>RENAME-FILE moved FROM to TO: ASDF compiles to a temporary name
    /// and renames the fasl into place before loading it.</summary>
    internal static void NoteFileRenamed(string from, string to)
    {
        lock (s_compiledFaslLock)
        {
            var e = s_compiledFasl;
            if (e == null) return;
            if (SamePath(e.Path, Path.GetFullPath(from))) e.Path = Path.GetFullPath(to);
            else if (SamePath(e.Path, Path.GetFullPath(to))) s_compiledFasl = null;
        }
    }

    /// <summary>The in-memory module for FILEPATH when it is the fasl COMPILE-FILE
    /// wrote last and the file has not changed since; null otherwise. Used once.</summary>
    private static System.Reflection.Assembly? TakeCompiledFasl(string filePath)
    {
        CompiledFasl? e;
        var full = Path.GetFullPath(filePath);
        lock (s_compiledFaslLock)
        {
            e = s_compiledFasl;
            if (e == null || !SamePath(e.Path, full)) return null;
            s_compiledFasl = null;
        }
        var fi = new FileInfo(full);
        if (!fi.Exists || fi.Length != e.Length || fi.LastWriteTimeUtc != e.WriteTimeUtc
            || TryFindR2rSibling(full) != null)
            return null;
        return e.Assembly;
    }
#endif

    private static LispObject LoadFasl(string filePath, PeFile? peFile, LispObject filespec,
        bool isVerbose, bool isPrint, System.Reflection.Assembly? compiled = null)
    {
        bool warnOnly = s_nextFaslMismatchWarnsOnly;
        s_nextFaslMismatchWarnsOnly = false;
        var loadPathSym = Startup.Sym("*LOAD-PATHNAME*");
        var loadTrueSym = Startup.Sym("*LOAD-TRUENAME*");
        var packageSym = Startup.Sym("*PACKAGE*");
        var readtableSym = Startup.Sym("*READTABLE*");
        var oldPackage = DynamicBindings.Get(packageSym);
        var oldReadtable = DynamicBindings.Get(readtableSym);
        var loadInputPath = filespec switch
        {
            LispPathname p => p,
            LispString s => LispPathname.FromString(s.Value),
            _ => LispPathname.FromString(filespec.ToString())
        };
        var dpdForLoad = DynamicBindings.Get(Startup.Sym("*DEFAULT-PATHNAME-DEFAULTS*"));
        var mergedLoadPath = MergePathnames(loadInputPath,
            dpdForLoad is LispPathname ? dpdForLoad : LispPathname.FromString(""));
        int faslBindDepth = DynamicBindings.Depth;
        DynamicBindings.Push(loadPathSym, mergedLoadPath);
        DynamicBindings.Push(loadTrueSym, LispPathname.FromString(Path.GetFullPath(filePath)));
        DynamicBindings.Push(packageSym, oldPackage);
        DynamicBindings.Push(readtableSym, oldReadtable);
        PushPolicyBindings();

        try
        {
            var faslFull = Path.GetFullPath(filePath);
            System.Reflection.Assembly asm;
            try
            {
                // An ahead-of-time sibling, <name>.fasl.r2r-<rid>, holds the same
                // module already compiled to native code. Loading it BY PATH is
                // what makes the runtime use that code: an assembly loaded from a
                // byte array is not file-backed and its R2R code is ignored, so
                // the two halves only pay off together.
                //
                // A sibling rather than a replacement: the .fasl stays the artifact
                // ASDF compares against its source, so nothing has to be told that
                // the file it planned around was swapped, and a stale sibling is
                // ignored rather than silently preferred. The marker goes after the
                // whole filename, not into the stem: a sibling must not match a
                // *.fasl glob, or every caller that walks fasls has to remember to
                // exclude it (the Makefile and two csprojs each used to).
                var r2rPath = compiled == null ? TryFindR2rSibling(faslFull) : null;
                if (compiled != null)
                {
                    asm = compiled;
                    FaslsLoadedInMemory++;
                }
                else if (r2rPath != null)
                {
                    asm = RuntimeLoadContext.LoadFrom(r2rPath);
                    R2rFaslsLoaded++;
                }
                else if (peFile!.ReadyToRun)
                {
                    // The fasl itself is the ahead-of-time image (a per-RID
                    // package ships asdf.fasl that way, under its own name).
                    // Read into a byte array, its native code was ignored and
                    // every top level form of ASDF was JIT-compiled on each
                    // (require "asdf"); by path the code is used.
                    asm = RuntimeLoadContext.LoadFrom(faslFull);
                    R2rFaslsLoaded++;
                }
                else
                {
#if NETSTANDARD2_0
                    // No AssemblyLoadContext on this target: load the bytes. When a
                    // matching .pdb sits next to the fasl, hand it to the
                    // symbol-store overload, since an assembly loaded from bytes has
                    // no file Location for a debugger to find the pdb by.
                    var faslBytes = peFile!.Image!;
                    var pdbPath = Path.ChangeExtension(faslFull, ".pdb");
                    if (File.Exists(pdbPath))
                    {
                        try { asm = RuntimeLoadContext.LoadBytes(faslBytes, File.ReadAllBytes(pdbPath)); }
                        catch { asm = RuntimeLoadContext.LoadBytes(faslBytes); } // mismatched/locked pdb
                    }
                    else
                    {
                        asm = RuntimeLoadContext.LoadBytes(faslBytes);
                    }
#else
                    asm = LoadFaslFromPath(faslFull);
#endif
                }
            }
            catch (BadImageFormatException)
            {
                // PE header but not managed IL (a native DLL, or a corrupt file).
                // Left alone this reaches the user as a bare "Bad IL format."
                throw FaslFileError(
                    $"LOAD: not a dotcl fasl (not a .NET assembly): {filePath}", filespec);
            }
            FaslsLoaded++;
            if (compiled != null)
            {
                CheckFaslStamp(asm, faslFull, warnOnly, checkGeneration: false);
                RunFaslModuleInit(asm, filePath, filespec);
                return T.Instance;
            }
            CheckFaslStamp(asm, faslFull, warnOnly);
            // A bundle (uiop bundle-op / dotcl:combine-fasls) is an assembly with
            // no code whose parts ride along as resources. Run each part in the
            // order they were bundled; a later part may depend on an earlier one.
            if (LoadFaslBundleParts(asm, filePath, filespec, warnOnly)) return T.Instance;

            // A PE file that is not a dotcl fasl: a plain .NET library, or another
            // implementation's fasl that happens to be an assembly. Report it as a
            // FILE-ERROR carrying the pathname, not as the raw reflection failure.
            RunFaslModuleInit(asm, filePath, filespec);
            return T.Instance;
        }
        finally
        {
            DynamicBindings.TruncateTo(faslBindDepth);
        }
    }

    /// <summary>
    /// Flatten the bodies of the top level forms CLHS 3.2.3.1 says carry top level
    /// forms inside them (PROGN, EVAL-WHEN, LOCALLY, MACROLET, SYMBOL-MACROLET),
    /// plus macro forms, into individual top-level forms.
    /// This ensures that defvar/defparameter values are set before subsequent
    /// forms in the same block are compiled (important for macro expansion), and
    /// it keeps one body from becoming one huge method (one method per form).
    /// </summary>
    private static IEnumerable<LispObject> FlattenTopLevel(LispObject form)
        => FlattenTopLevel(form, null, 0);

    /// <summary>An enclosing top level MACROLET / SYMBOL-MACROLET: its operator,
    /// its bindings and its declarations.</summary>
    private sealed record LocalMacroScope(Symbol Op, LispObject Bindings, List<LispObject> Decls);

    /// <param name="scopes">The top level MACROLET / SYMBOL-MACROLET forms this form
    /// sits in, outermost first, or null. Their bindings are lexical and the global
    /// macro table does not know them, so a body form is not expanded with it:
    /// expanding a call to a name a MACROLET shadows would take the global
    /// definition. A call to one of the local macros themselves is expanded in
    /// their scope (LocalMacroexpand1), because its expansion is made of top level
    /// forms too (CLHS 3.2.3.1): SBCL's build expands one local macro into a PROGN
    /// of every delayed DEFSTRUCT, and those have to be processed one at a time,
    /// each before the literals of the next are made.</param>
    private static IEnumerable<LispObject> FlattenTopLevel(LispObject form,
        List<LocalMacroScope>? scopes, int line)
    {
        bool inLocalMacroScope = scopes != null;
        // Defensive: if a reader macro or macro expander leaked MvReturn, take primary value.
        if (form is MvReturn mvr)
            form = mvr.PrimaryValue;
        // A form the reader saw keeps its own line; one a macro produced inherits
        // the line of the macro form it came from.
        if (form is Cons fc) line = SourceLineOf(fc, line);
        s_flattenedDefinitionLine = line;
        if (s_flattenedDefinitionNames is { } defNames && form is Cons definer)
            DefinerEntries(definer, defNames, line);
        if (form is Cons c && c.Car is Symbol sym)
        {
            if (sym.Name == "EVAL-WHEN" && c.Cdr is Cons rest)
            {
                // (eval-when (situations...) body...)
                // Only flatten if there are multiple body forms
                var situations = rest.Car;
                var body = rest.Cdr;
                // Count body forms
                int count = 0;
                var tmp = body;
                while (tmp is Cons) { count++; tmp = ((Cons)tmp).Cdr; }
                if (count > 1)
                {
                    while (body is Cons bodyCell)
                    {
                        // (eval-when (situations) single-form)
                        var wrappedForm = new Cons(sym,
                            new Cons(situations, new Cons(bodyCell.Car, Nil.Instance)));
                        // Don't recurse into the wrapped form (it's already single)
                        // but do flatten the inner form if it's progn
                        yield return wrappedForm;
                        body = bodyCell.Cdr;
                    }
                    yield break;
                }
            }
            if (sym.Name == "PROGN")
            {
                var body = c.Cdr;
                while (body is Cons bodyCell)
                {
                    foreach (var sub in FlattenTopLevel(bodyCell.Car, scopes, line))
                        yield return sub;
                    body = bodyCell.Cdr;
                }
                yield break;
            }
            // Per CLHS 3.2.3.1, the body of a top level LOCALLY is likewise
            // processed as top level forms, with the declarations still in
            // effect: so each body form is re-wrapped in its own LOCALLY.
            // The Lisp side already agrees (compile-locally keeps
            // *at-toplevel*); only this flattener did not, so a file whose
            // definitions sit inside one top level LOCALLY compiled its whole
            // body into a single _toplevel_N method. A library that emits a few
            // hundred definitions that way reached multi-MB of IL in one method,
            // and JITting it at load cost gigabytes of RSS.
            if (sym.Name == "LOCALLY" && c.Cdr is Cons lrest)
            {
                var decls = new List<LispObject>();
                LispObject lbody = lrest;
                while (lbody is Cons rc && rc.Car is Cons dc
                       && dc.Car is Symbol dsym && dsym.Name == "DECLARE")
                {
                    decls.Add(rc.Car);
                    lbody = rc.Cdr;
                }
                int bodyCount = 0;
                for (var t = lbody; t is Cons tc; t = tc.Cdr) bodyCount++;
                // One body form is already one method; splitting it would only
                // add a wrapper. Zero means the LOCALLY is declarations only.
                // But one body form that is itself a PROGN, a LOCALLY or a
                // (SYMBOL-)MACROLET holding several forms is split like any
                // other: those are top level forms as well.
                if (bodyCount > 1
                    || (bodyCount == 1 && IsSplittableTopLevelScope(((Cons)lbody).Car)))
                {
                    for (var t = lbody; t is Cons tc; t = tc.Cdr)
                    {
                        foreach (var sub in FlattenTopLevel(tc.Car, scopes, line))
                        {
                            LispObject wrapped = new Cons(sub, Nil.Instance);
                            for (int i = decls.Count - 1; i >= 0; i--)
                                wrapped = new Cons(decls[i], wrapped);
                            yield return new Cons(sym, wrapped);
                        }
                    }
                    yield break;
                }
            }
            // CLHS 3.2.3.1 names MACROLET and SYMBOL-MACROLET alongside LOCALLY:
            // their bodies are top level forms too. Same shape as LOCALLY, one
            // element further in: the bindings list rides along with the
            // declarations onto every body form, so each keeps the local macros
            // and symbol macros it was written under. Without this a library that
            // wraps its definitions in one top level MACROLET compiles the whole
            // body into a single _toplevel_N method: the same failure the LOCALLY
            // case above fixes, with the same cost at load time.
            if ((sym.Name == "MACROLET" || sym.Name == "SYMBOL-MACROLET")
                && c.Cdr is Cons mrest)
            {
                var bindings = mrest.Car;
                var mdecls = new List<LispObject>();
                LispObject mbody = mrest.Cdr;
                while (mbody is Cons mrc && mrc.Car is Cons mdc
                       && mdc.Car is Symbol mdsym && mdsym.Name == "DECLARE")
                {
                    mdecls.Add(mrc.Car);
                    mbody = mrc.Cdr;
                }
                int mBodyCount = 0;
                for (var t = mbody; t is Cons tc; t = tc.Cdr) mBodyCount++;
                var inner = new List<LocalMacroScope>(scopes ?? new List<LocalMacroScope>())
                    { new LocalMacroScope(sym, bindings, mdecls) };
                // One body form is split too when it calls one of the local macros:
                // its expansion is what holds the top level forms.
                if (mBodyCount > 1
                    || (mBodyCount == 1 && IsLocalMacroCall(((Cons)mbody).Car, inner)))
                {
                    var localNames = LocalBindingNames(bindings);
                    for (var t = mbody; t is Cons tc; t = tc.Cdr)
                    {
                        foreach (var sub in FlattenTopLevel(tc.Car, inner, line))
                        {
                            // A form that names none of the local macros (and no
                            // declarations to carry) needs no wrapper; left bare it
                            // is seen as what it is, so an IN-PACKAGE or EVAL-WHEN
                            // in the body acts on the rest of the file as it does
                            // outside a MACROLET.
                            if (mdecls.Count == 0 && !MentionsAny(sub, localNames))
                            {
                                yield return sub;
                                continue;
                            }
                            LispObject wrapped = new Cons(sub, Nil.Instance);
                            for (int i = mdecls.Count - 1; i >= 0; i--)
                                wrapped = new Cons(mdecls[i], wrapped);
                            yield return new Cons(sym, new Cons(bindings, wrapped));
                        }
                    }
                    yield break;
                }
            }
            // Per CLHS 3.2.3.1: "If a top level form is a macro form,
            // the macro form is expanded and the result is processed as a top level form."
            // Don't expand forms already handled by ShouldExecuteAtCompileTime or
            // IsEvalWhenForCompileFile: they need their original identity preserved.
            // Only CL's own symbols; another package can shadow DEFUN with a macro.
            if (!inLocalMacroScope && !IsCompileTimeSideEffectForm(sym.Name)
                && !(ReferenceEquals(sym.HomePackage, Startup.CL)
                     && (sym.Name is "DEFUN" or "DEFVAR" or "DEFPARAMETER")))
            {
                var expanded = TryMacroexpand1(form);
                if (expanded != null && !ReferenceEquals(expanded, form))
                {
                    foreach (var sub in FlattenTopLevel(expanded, scopes, line))
                        yield return sub;
                    yield break;
                }
            }
            if (inLocalMacroScope && IsLocalMacroCall(form, scopes!))
            {
                var expanded = LocalMacroexpand1(form, scopes!);
                if (expanded != null)
                {
                    foreach (var sub in FlattenTopLevel(expanded, scopes, line))
                        yield return sub;
                    yield break;
                }
            }
        }
        yield return form;
    }

    /// <summary>True when FORM is a PROGN, LOCALLY, MACROLET or SYMBOL-MACROLET
    /// with more than one body form (after declarations), which FlattenTopLevel
    /// splits into its body forms.</summary>
    private static bool IsSplittableTopLevelScope(LispObject form)
    {
        if (form is not Cons c || c.Car is not Symbol sym) return false;
        LispObject body;
        if (sym.Name == "PROGN" || sym.Name == "LOCALLY") body = c.Cdr;
        else if ((sym.Name == "MACROLET" || sym.Name == "SYMBOL-MACROLET")
                 && c.Cdr is Cons mrest) body = mrest.Cdr;
        else return false;
        if (sym.Name != "PROGN")
            while (body is Cons bc && bc.Car is Cons dc
                   && dc.Car is Symbol dsym && dsym.Name == "DECLARE")
                body = bc.Cdr;
        return body is Cons first && first.Cdr is Cons;
    }

    /// <summary>The names a MACROLET / SYMBOL-MACROLET binding list defines.</summary>
    private static HashSet<Symbol> LocalBindingNames(LispObject bindings)
    {
        var names = new HashSet<Symbol>(ReferenceEqualityComparer.Instance);
        for (var b = bindings; b is Cons bc; b = bc.Cdr)
            if (bc.Car is Cons def && def.Car is Symbol n) names.Add(n);
        return names;
    }

    /// <summary>True when NAMES appears anywhere in FORM's conses, or when FORM is too
    /// large to look through (then it is kept in its scope, as before).</summary>
    private static bool MentionsAny(LispObject form, HashSet<Symbol> names)
    {
        if (names.Count == 0) return false;
        var stack = new Stack<LispObject>();
        stack.Push(form);
        int budget = 100000;
        while (stack.Count > 0)
        {
            if (--budget < 0) return true;
            var x = stack.Pop();
            if (x is Symbol sx) { if (names.Contains(sx)) return true; }
            else if (x is Cons cx) { stack.Push(cx.Cdr); stack.Push(cx.Car); }
        }
        return false;
    }

    /// <summary>True when FORM calls a macro one of SCOPES' MACROLETs defines.</summary>
    private static bool IsLocalMacroCall(LispObject form, List<LocalMacroScope> scopes)
    {
        if (form is not Cons c || c.Car is not Symbol op) return false;
        foreach (var scope in scopes)
        {
            if (scope.Op.Name != "MACROLET") continue;
            for (var b = scope.Bindings; b is Cons bc; b = bc.Cdr)
                if (bc.Car is Cons def && ReferenceEquals(def.Car, op)) return true;
        }
        return false;
    }

    /// <summary>MACROEXPAND-1 of FORM in the lexical scope of SCOPES: evaluates
    /// (macrolet* SCOPES (macrolet ((#:g (&amp;environment #:e) ...)) (#:g))), whose
    /// inner macro hands back the expansion. Null when it does not expand or the
    /// expansion fails (the form is then left whole, as before). A failure is
    /// left in *FILE-EXPANSION-FAILURES* for the compiler to signal again.</summary>
    private static LispObject? LocalMacroexpand1(LispObject form, List<LocalMacroScope> scopes)
    {
        try
        {
            var g = new Symbol("LOCAL-EXPAND", null);
            var e = new Symbol("ENV", null);
            var c = new Symbol("C", null);
            var quote = Startup.Sym("QUOTE");
            // (handler-case (list 'quote (multiple-value-list (macroexpand-1 'FORM e)))
            //   (error (c) (list 'quote (list nil c))))
            var expandCall = Runtime.List(Startup.Sym("HANDLER-CASE"),
                Runtime.List(Startup.Sym("LIST"), Runtime.List(quote, quote),
                    Runtime.List(Startup.Sym("MULTIPLE-VALUE-LIST"),
                        Runtime.List(Startup.Sym("MACROEXPAND-1"), Runtime.List(quote, form), e))),
                Runtime.List(Startup.Sym("ERROR"), Runtime.List(c),
                    Runtime.List(Startup.Sym("LIST"), Runtime.List(quote, quote),
                        Runtime.List(Startup.Sym("LIST"), Nil.Instance, c))));
            var def = Runtime.List(g, Runtime.List(Startup.Sym("&ENVIRONMENT"), e), expandCall);
            LispObject probe = Runtime.List(Startup.Sym("MACROLET"), Runtime.List(def), Runtime.List(g));
            for (int i = scopes.Count - 1; i >= 0; i--)
            {
                var s = scopes[i];
                LispObject body = new Cons(probe, Nil.Instance);
                for (int d = s.Decls.Count - 1; d >= 0; d--) body = new Cons(s.Decls[d], body);
                probe = new Cons(s.Op, new Cons(s.Bindings, body));
            }
            var result = Runtime.Eval(probe);
            if (result is Cons rc && rc.Cdr is Cons flag && flag.Car is not Nil)
            {
                if (flag.Car is T) return rc.Car;
                var failure = flag.Car;
                // Keyed by the scope the compiler will see: the binding lists,
                // innermost first.
                if (DynamicBindings.TryGet(Startup.Sym("*FILE-EXPANSION-FAILURES*"), out var failures)
                    && failures is LispHashTable failuresTable)
                {
                    LispObject scope = Nil.Instance;
                    foreach (var s in scopes) scope = new Cons(s.Bindings, scope);
                    failuresTable.Set(form, new Cons(scope, failure));
                }
            }
            return null;
        }
        catch (Exception) { return null; }
    }
}
