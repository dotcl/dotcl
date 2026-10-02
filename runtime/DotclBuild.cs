namespace DotCL;

/// <summary>
/// The build-tool half of the .NET surface: resolving an ASDF system's
/// dependencies, compiling a project to a fasl, packing one for distribution,
/// and reading a system's metadata for a nuspec. These are what the
/// <c>dotcl</c> CLI and the MSBuild tasks call.
///
/// Separate from <see cref="DotclHost"/> because the two have different
/// audiences and different lifetimes. DotclHost is what an application embeds:
/// it is about running Lisp inside a process that is doing something else, and
/// its contract is about lifecycle, conditions, values and streams. What is
/// here runs at BUILD time, in a tool, and its contract is about files on disk.
/// A host that embeds Lisp in a game never calls any of it, and having both on
/// one type made the embedding surface look larger and less decided than it is.
///
/// </summary>
public static class DotclBuild
{
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
        public string? Version;        // null unless the .asd states a string version
        public string? EntryPoint;     // :entry-point, as a "pkg:name" string; null if none
    }

    /// <summary>
    /// Resolve an ASDF system's dependencies and write the manifest and root
    /// source list the build then consumes.
    /// </summary>
    public static void ResolveDeps(string asdPath, string? manifestOut, string? rootSourcesOut,
                                   string? targetRid = null, string[]? buildInit = null,
                                   string[]? searchPaths = null)
        => ResolveDepsCore(asdPath, manifestOut, rootSourcesOut, targetRid, buildInit, searchPaths);

    /// <summary>
    /// Compile an ASDF system's own sources to <paramref name="outputPath"/>,
    /// with its dependencies staying as pre-built fasls resolved by
    /// <see cref="ResolveDeps"/>.
    /// </summary>
    public static void CompileProject(string asdPath, string outputPath, string[]? buildInit = null,
                                      string[]? searchPaths = null, bool debugInfo = false)
        => CompileProjectCore(asdPath, outputPath, buildInit, searchPaths, debugInfo);

    /// <summary>
    /// <see cref="CompileProject(string, string, string[], string[], bool)"/>, naming
    /// the assembly the project itself builds. The compile also writes
    /// <c>&lt;output&gt;.trim.xml</c>, an ILLink descriptor rooting the .NET types the
    /// sources name; a type the build cannot find yet is taken to be in
    /// <paramref name="appAssemblyName"/>.
    /// </summary>
    public static void CompileProject(string asdPath, string outputPath, string[]? buildInit,
                                      string[]? searchPaths, bool debugInfo, string? appAssemblyName)
        => CompileProjectCore(asdPath, outputPath, buildInit, searchPaths, debugInfo, appAssemblyName);

    /// <summary>
    /// Build a system and everything it depends on into one fasl at
    /// <paramref name="outputFasl"/>, for shipping.
    /// </summary>
    public static void PackFasl(string system, string outputFasl, string? toplevel = null,
                                string[]? buildInit = null, string[]? searchPaths = null)
        => PackFaslCore(system, outputFasl, toplevel, buildInit, searchPaths);

    /// <summary>
    /// As <see cref="PackFasl(string, string, string?, string[]?, string[]?)"/>,
    /// with PRELUDE sources compiled ahead of the system's closure.
    /// </summary>
    public static void PackFasl(string system, string outputFasl, string? toplevel,
                                string[]? buildInit, string[]? searchPaths,
                                string[]? prelude)
        => PackFaslCore(system, outputFasl, toplevel, buildInit, searchPaths, prelude);

    /// <summary>
    /// Read the standard metadata slots off an ASDF system, as nuspec defaults.
    /// Fields are null where the .asd is silent; the result is null when the
    /// system cannot be found at all.
    /// </summary>
    public static SystemMeta? ReadSystemMeta(string system, string[]? searchPaths = null)
        => ReadSystemMetaCore(system, searchPaths);

    /// <summary>
    /// As <see cref="ReadSystemMeta(string, string[]?)"/>, and when the result is
    /// null <paramref name="error"/> says why: the system is not visible to ASDF,
    /// or loading its .asd signalled. Null error means the system was read.
    /// </summary>
    public static SystemMeta? ReadSystemMeta(string system, string[]? searchPaths, out string? error)
        => ReadSystemMetaCore(system, searchPaths, out error);

    /// <summary>
    /// Why NuGet cannot serve <paramref name="version"/>, or null when it can.
    ///
    /// Only meant for a version taken from a .asd's <c>:version</c>, which ASDF
    /// has already reduced to dot-separated non-negative integers. Of those,
    /// NuGet accepts one to four components, each within Int32. Anything else
    /// is written into the nupkg without complaint, and the later
    /// <c>dotnet tool install</c> answers only that the package is not found.
    /// A version typed with <c>--version</c> is not checked: NuGet's full
    /// SemVer2 grammar (prerelease and metadata suffixes) is wider than this
    /// rule, and rejecting a version NuGet accepts would be worse than saying
    /// nothing.
    /// </summary>
    public static string? AsdVersionNuGetProblem(string version)
    {
        var parts = version.Split('.');
        if (parts.Length > 4)
            return $"it has {parts.Length} components; NuGet accepts at most 4";
        foreach (var p in parts)
            if (!int.TryParse(p, System.Globalization.NumberStyles.None,
                    System.Globalization.CultureInfo.InvariantCulture, out _))
                return $"component \"{p}\" is not an integer NuGet accepts (0 to {int.MaxValue})";
        return null;
    }

    // -- Project-core build (ASDF -> fasl) ------------------------------------
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
    /// when given, prefers <c>&lt;name&gt;.fasl.r2r-&lt;rid&gt;</c> if present.
    /// </summary>
    /// <summary>
    /// Load each user-supplied build-init script (the &lt;DotclBuildInit&gt; items)
    /// before dependency resolution. dotcl does NOT auto-scan ~/quicklisp etc.; a
    /// build that needs external systems makes them discoverable here; e.g. the
    /// script does (pushnew #p".../foo/" asdf:*central-registry*) or boots quicklisp.
    /// Build-time only: the shipped runtime never runs these, so it can't end up
    /// depending on the dev machine's paths. Called after (require "asdf").
    /// </summary>
    private static void LoadBuildInitScripts(string[]? scripts)
        => Runtime.LoadLispFiles(scripts, "DotclBuildInit script");

    /// <summary>Lisp preamble shared by the build forms: resolve the root system
    /// of the .asd the build was pointed at, and refuse to build a different one
    /// that merely shares its name.
    ///
    /// ASDF looks systems up by NAME. The build says "compile this file", loads
    /// it with LOAD-ASD, and then asks FIND-SYSTEM for the name: at which point
    /// any other .asd of the same name that ASDF can see (its source registry
    /// scans whole trees) can answer instead, and the build compiles someone
    /// else's sources without a word. That is not hypothetical: the in-tree
    /// project-compose fixture is named DotclApp, so is templates/dotcl-app, and
    /// the build silently produced the template's code.</summary>
    /// <summary>Evaluate a Lisp source string for its side effects.</summary>
    private static void EvalLisp(string source) =>
        Runtime.Eval(MultipleValues.Primary(
            Runtime.ReadFromString(new LispObject[] { new LispString(source) })));

    private const string RootSystemHelper = @"
(progn
(defun %root-system-of (asd)
  (flet ((norm (p) (and p (substitute #\/ #\\ (namestring p)))))
    (let* ((want (norm (truename (pathname asd))))
           (sys  (asdf:find-system (pathname-name (pathname asd))))
           (got  (norm (ignore-errors (truename (asdf:system-source-file sys))))))
      (unless (and got (string-equal got want))
        (error ""~a defines system ~s, but that name resolves to ~a.~%~
                Two .asd files in reach of this build define the same system; ~
                rename one, or keep the other out of the search path.""
               want (asdf:component-name sys) (or got ""an unknown file"")))
      sys)))

;; The Lisp source files of SYS itself, in the order ASDF would compile them.
;; Components inside a :module are included: listing only the system's direct
;; children dropped every file under a module and compiled the rest as if that
;; were the whole system. Static files and file-less components (a :nuget
;; declaration) have nothing to compile and are left out.
(defun %system-source-files (sys)
  (loop for c in (asdf:required-components sys :other-systems nil)
        when (typep c 'asdf:cl-source-file)
          collect c)))
";

    /// <summary>
    /// Register each user-declared external system directory (the
    /// &lt;DotclAsdSearchPath&gt; items) onto <c>asdf:*central-registry*</c> so the
    /// project's <c>:depends-on</c> resolves systems that live outside the shipped
    /// contrib: without dotcl auto-scanning the dev machine. This is the
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
            // glued on by Windows command-line escaping (\" -> literal "), since
            // the MSBuild Exec passes %(FullPath) of a dir (...\extlib\) quoted.
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
    /// (~/.cache/common-lisp/...). ASDF caches each system's component fasls keyed
    /// by source path; that cache lives outside the project and survives
    /// `dotnet clean`, so a regenerated source can be shadowed by a stale cached
    /// fasl (dotcl/dotcl#53). Sending it under obj/ makes `dotnet clean` (which
    /// wipes obj/) clear it too: one project-local cache, no external trap. The
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

    private static void ResolveDepsCore(string asdPath, string? manifestOut, string? rootSourcesOut, string? targetRid = null, string[]? buildInit = null, string[]? searchPaths = null)
    {
        var absAsd = System.IO.Path.GetFullPath(asdPath);
        if (!System.IO.File.Exists(absAsd))
            throw new System.IO.FileNotFoundException($"resolve-deps: file not found: {absAsd}", absAsd);

        // Bring asdf in. (require "asdf") goes through module-provide-contrib
        // and side-effects *central-registry* with shipped contrib subdirs.
        Runtime.Eval(MultipleValues.Primary(
            Runtime.ReadFromString(new LispObject[] { new LispString("(require \"asdf\")") })));
        // Before any .asd is read: a stock asdf.asd visible to the build would
        // otherwise replace the running ASDF (see PinBundledAsdf).
        PinBundledAsdf();
        EvalLisp(RootSystemHelper);

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
        // next to the manifest, i.e. under obj/.../dotcl-fasl/, instead of polluting
        // each dep's source dir. That makes them cleanable by `dotnet clean` (which wipes
        // obj/), at the cost of recompiling deps per project (the .NET obj/ model). The
        // CompileProject load step uses the same convention. A prebuilt .fasl.r2r-<rid> AOT
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
                 ;;
                 ;; NIL and an error mean different things and used to be handled
                 ;; the same (ignore-errors, skip): NIL is ""this dependency does
                 ;; not apply here"", an error is ""this dependency was declared and
                 ;; cannot be found"". Swallowing the second wrote a manifest that
                 ;; silently lacked the system, so the build succeeded and the
                 ;; application failed later, where nothing points back here.
                 (let ((ds (handler-case
                               (asdf/find-component:resolve-dependency-spec sys d)
                             (error (e)
                               (error ""resolve-deps: ~a depends on ~s, which cannot be found: ~a""
                                      (asdf:component-name sys) d e)))))
                   (when ds (walk ds))))
               (push sys order)))
           (ensure-fasl (sys)
             (let* ((src  (asdf:component-pathname sys))
                    (dir  (directory-namestring src))
                    (name (asdf:component-name sys))
                    (r2r-fasl {(targetRid == null
                        ? "nil"
                        : $"(concatenate 'string dir name \".fasl.r2r-\" \"{targetRid}\")")})
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
    (let* ((root (%root-system-of ""{asdLisp}""))
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
            (dolist (c (%system-source-files root))
              (format rstream ""~A~%"" (namestring (asdf:component-pathname c))))
            (close rstream)))))))";
        Runtime.Eval(MultipleValues.Primary(
            Runtime.ReadFromString(new LispObject[] { new LispString(form) })));
    }

    /// <summary>
    /// Concatenate the root system's <c>:components</c> (declared order) via
    /// <c>asdf::concatenate-files</c> and <c>compile-file</c> the result into
    /// <paramref name="outputPath"/>. Only the root system is compiled into it;
    /// dependencies are loaded first, from the fasls <see cref="ResolveDeps"/>
    /// built when all of them are there, otherwise through ASDF.
    /// </summary>
    private static void CompileProjectCore(string asdPath, string outputPath, string[]? buildInit = null, string[]? searchPaths = null, bool debugInfo = false,
                                            string? appAssemblyName = null)
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
        PinBundledAsdf();

        EvalLisp(RootSystemHelper);
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
        // Beside the concat, under obj/: generated, and cleaned with everything else.
        var preambleLisp = (outDir == null ? "" : outDir.Replace("\\", "/") + "/")
                         + System.IO.Path.GetFileNameWithoutExtension(outputPath)
                         + ".nuget-preamble.lisp";
        // Phase 1: load the asd, load the resolved :depends-on fasls, and
        // concatenate the root's sources into the concat file. Return the ordered
        // source namestrings so we can build a concat-line -> (file, line) map for
        // diagnostics (dotcl/dotcl#48).
        var setupForm = $@"
(progn
  (asdf:load-asd ""{asdLisp}"")
  (let* ((root (%root-system-of ""{asdLisp}""))
         ;; Only the children that have a source to contribute. A component can
         ;; legitimately have none -- (:nuget ...) declares a NuGet package, not a
         ;; file -- and its COMPONENT-PATHNAME is then the system's own directory,
         ;; which CONCATENATE-FILES tried to read and failed on: the whole build
         ;; died with ""File not found: <system dir>/"".
         (sources (mapcar #'asdf:component-pathname (%system-source-files root)))
         ;; What those file-less components asked for, turned back into source:
         ;; the concatenated unit is not loaded through ASDF, so nothing else
         ;; would ever perform them (see DOTCL-NUGET-ASDF). Written as a file of
         ;; its own and put first, rather than prepended to the concatenation, so
         ;; that the concat-line -> source-line map stays exact.
         (nuget-asdf (find-package ""DOTCL-NUGET-ASDF""))
         (preamble (when nuget-asdf
                     (funcall (find-symbol ""SYSTEM-NUGET-PREAMBLE"" nuget-asdf) root))))
    (when preamble
      (let ((path ""{preambleLisp}""))
        (with-open-file (o path :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
          (write-string preamble o))
        (setf sources (cons (pathname path) sources))))
    ;; Load the root's :depends-on closure into the image BEFORE compiling the
    ;; root, so the deps' defpackage/macros are available at the root's compile
    ;; time: same as a standard ASDF load-op-then-compile. The concatenated unit
    ;; holds only the root's own sources.
    ;;
    ;; The MSBuild build runs resolve-deps first, which leaves one fasl per
    ;; dependency in the project deps/ cache dir, in topo order; those are
    ;; loaded as they are. When any is missing (the CLI `dotcl build --output`
    ;; without a resolve-deps step, or a --target-rid build whose r2r fasls sit
    ;; next to the dep sources instead) the closure is loaded through ASDF
    ;; instead. Loading only the fasls that happen to exist skipped the rest
    ;; silently, and the root then failed to read with ""Package X not found"".
    ;; It is all or nothing so a system is never loaded twice, once from its
    ;; fasl and again as a dependency of one that ASDF loads.
    (let ((seen '()) (order '()))
      (labels ((walk (sys)
                 (unless (member sys seen :test #'eq)
                   (push sys seen)
                   (dolist (d (asdf:system-depends-on sys))
                     ;; resolve-dependency-spec normalizes ASDF dependency specifiers
                     ;; ((:feature :dotcl ""x""), (:version ...), plain names) to a
                     ;; system, returning nil when a :feature condition is unmet.
                     ;; An error means the dependency was declared and cannot be
                     ;; found; that stops the build here rather than as a missing
                     ;; package in the root's code.
                     (let ((ds (handler-case
                                   (asdf/find-component:resolve-dependency-spec sys d)
                                 (error (e)
                                   (error ""build: ~a depends on ~s, which cannot be found: ~a""
                                          (asdf:component-name sys) d e)))))
                       (when ds (walk ds))))
                   (push sys order))))
        (walk root))
      (let* ((deps (remove-if-not #'asdf:component-children
                                  (remove root (nreverse order))))
             (fasls (mapcar (lambda (sys)
                              (probe-file
                               (concatenate 'string ""{depCacheLisp}""
                                            (asdf:component-name sys) "".fasl"")))
                            deps)))
        (if (every #'identity fasls)
            (mapc #'load fasls)
            (progn
              (format *error-output*
                      ""[build] loading ~d dependenc~:@p through ASDF~%"" (length deps))
              (dolist (sys deps) (asdf:load-system sys))))))
    (asdf::concatenate-files sources ""{concatLisp}"")
    (mapcar #'namestring sources)))";
        var sourcesResult = Runtime.Eval(MultipleValues.Primary(
            Runtime.ReadFromString(new LispObject[] { new LispString(setupForm) })));
        var sourcePaths = ListToStringArray(sourcesResult);
        var lineMap = BuildConcatLineMap(sourcePaths);

        // Progress trace (dotcl/dotcl#48 point 2): which files this build compiles,
        // in order: so a failing build shows what was processed before the error.
        System.Console.Error.WriteLine(
            $"[build] {System.IO.Path.GetFileNameWithoutExtension(absAsd)}: compiling {sourcePaths.Length} source(s)");
        foreach (var sp in sourcePaths)
            System.Console.Error.WriteLine($"[build]   {sp}");

        // Phase 2: compile the concatenated unit. compile-file-concatenated binds
        // *concatenate-build* (cross-compiled, so the binding shares symbol identity
        // with the compiler's read) so the compiler evaluates toplevel
        // require/use-package/load at compile time within the single concatenated
        // unit: restoring the compile+load interleaving a normal multi-file load-op
        // would have given the original :components.
        //
        // EmitBuildSourceLocations makes COMPILE-FILE attach the concat file + form
        // line to a compile error; we then remap that concat line back to the
        // original source file:line via lineMap (dotcl/dotcl#48).
        var compileForm =
            $@"(dotcl.cil-compiler::compile-file-concatenated-collecting ""{concatLisp}"" ""{outLisp}"")";
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
            var typeNames = ListToStringArray(Runtime.Eval(MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString(compileForm) }))));
            WriteTrimmerDescriptor(
                System.IO.Path.ChangeExtension(absOut, ".trim.xml"), typeNames, appAssemblyName);
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
            // The concatenated unit and the preamble are intermediates: the fasl
            // (and its PDB, which names the real sources through lineMap or the
            // single-source override) is the output. A compile error has already
            // been remapped to the original file:line above, so nothing points at
            // the concat any more. Left in place they piled up next to the output.
            TryDeleteFile(concatLisp);
            TryDeleteFile(preambleLisp);
        }
    }

    /// <summary>
    /// Write an ILLink descriptor that roots every .NET type the compiled unit
    /// names literally (dotnet:new, dotnet:static, ...). The fasl is loaded at
    /// run time and reaches those types only through reflection, so a trimmed
    /// publish would otherwise remove them. The build's MSBuild targets hand the
    /// file to the trimmer as a TrimmerRootDescriptor.
    ///
    /// A name is resolved here, in the build process, which sees the framework
    /// and the project's referenced assemblies. A name that does not resolve is
    /// most likely a type of the project being built (it is not compiled yet),
    /// so it is rooted in <paramref name="appAssemblyName"/>; if it is not there
    /// either, the trimmer reports it. Types defined from Lisp at run time
    /// (dotnet:define-class) resolve to a dynamic assembly and are skipped.
    /// </summary>
    internal static void WriteTrimmerDescriptor(string path, string[] typeNames, string? appAssemblyName)
    {
        var byAssembly = new SortedDictionary<string, SortedSet<string>>(StringComparer.Ordinal);
        void Add(string assembly, string typeFullName)
        {
            if (!byAssembly.TryGetValue(assembly, out var set))
                byAssembly[assembly] = set = new SortedSet<string>(StringComparer.Ordinal);
            // ILLink spells a nested type Outer/Inner; reflection spells it Outer+Inner.
            set.Add(typeFullName.Replace('+', '/'));
        }
        void AddType(Type t)
        {
            while (t.HasElementType) t = t.GetElementType()!;   // T[], T&, T*
            if (t.IsGenericParameter || t.Assembly.IsDynamic) return;
            if (t.IsGenericType && !t.IsGenericTypeDefinition)
            {
                foreach (var a in t.GetGenericArguments()) AddType(a);
                t = t.GetGenericTypeDefinition();
            }
            if (t.FullName == null || t.FullName == "System.__ComObject") return;
            Add(t.Assembly.GetName().Name!, t.FullName);
        }
        foreach (var name in typeNames)
        {
            Type? t = null;
            try { t = Runtime.TryResolveDotNetType(name); }
            catch (Exception) { }
            if (t != null) { AddType(t); continue; }
            // "T, Assembly" names its own assembly; anything else is taken to be
            // the project's own type.
            int comma = name.IndexOf(',');
            if (comma > 0)
                Add(name[(comma + 1)..].Trim(), name[..comma].Trim());
            else if (!string.IsNullOrEmpty(appAssemblyName))
                Add(appAssemblyName!, name.Trim());
        }
        var sb = new System.Text.StringBuilder();
        sb.Append("<linker>\n");
        foreach (var kv in byAssembly)
        {
            sb.Append("  <assembly fullname=\"").Append(System.Security.SecurityElement.Escape(kv.Key)).Append("\">\n");
            foreach (var t in kv.Value)
                sb.Append("    <type fullname=\"").Append(System.Security.SecurityElement.Escape(t))
                  .Append("\" preserve=\"all\" />\n");
            sb.Append("  </assembly>\n");
        }
        sb.Append("</linker>\n");
        System.IO.File.WriteAllText(path, sb.ToString());
    }

    private static void TryDeleteFile(string path)
    {
        try { if (System.IO.File.Exists(path)) System.IO.File.Delete(path); }
        catch (System.IO.IOException) { }
        catch (System.UnauthorizedAccessException) { }
    }

    /// <summary>
    /// Build a single self-contained FASL for <c>dotcl pack</c>: the named ASDF
    /// system and its whole dependency closure, compiled one source at a time in
    /// dependency order, into <paramref name="outputFasl"/>. Unlike
    /// <see cref="CompileProject"/> (root only, deps stay as separate fasls) the
    /// produced FASL loads standalone, so the pack restamp can drop it into the
    /// tool package as a single dotcl.user.fasl with no dep fasls to bundle.
    ///
    /// This is the same collect-and-compile path SAVE-APPLICATION :SYSTEM uses.
    /// pack used to concatenate the closure into one unit through ASDF's
    /// MONOLITHIC-CONCATENATE-SOURCE-OP and compile that, which cannot work for a
    /// system whose sources use #. at read time: read-time eval assumes the
    /// earlier forms have been evaluated, and in one concatenated unit they have
    /// only been compiled, so the first (declare #.*standard-optimize-settings*)
    /// dies reading. cl-ppcre, flexi-streams, cl-unicode and cl-interpol all do
    /// this, which between them covers a large part of Quicklisp.
    ///
    /// When <paramref name="toplevel"/> is non-null a call to it is appended so
    /// the tool runs that entry point on launch. A system that already invokes
    /// its entry at load time (e.g. a roswell <c>&lt;name&gt;/exe</c> launcher)
    /// needs none.
    ///
    /// <paramref name="prelude"/> sources are compiled ahead of the closure, for
    /// whatever a deployed image needs in place before any library code runs.
    /// </summary>
    private static void PackFaslCore(string system, string outputFasl, string? toplevel = null,
                                string[]? buildInit = null, string[]? searchPaths = null,
                                string[]? prelude = null)
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
        PinBundledAsdf();
        RegisterAsdSearchPaths(searchPaths);
        LoadBuildInitScripts(buildInit);

        var sysEsc = system.Replace("\\", "\\\\").Replace("\"", "\\\"");
        var preambleFile = (outDir == null ? "" : outDir.Replace("\\", "/") + "/")
                         + System.IO.Path.GetFileNameWithoutExtension(outputFasl)
                         + ".pack.nuget.lisp";

        // The running ASDF and UIOP are pinned above (PinBundledAsdf). Beyond
        // that, before the walk:
        //
        // FIND-SYSTEM, never LOAD-SYSTEM. The walk needs the dependency graph,
        // not the code, and compiling a system's sources in an image that has
        // already loaded them re-runs everything inside an EVAL-WHEN
        // :COMPILE-TOPLEVEL a second time: cl-interpol's
        // (defreadtable :interpol-syntax ...) answers that with
        // READER-MACRO-CONFLICT.
        //
        // A (:nuget ...) component is not a file, and the walk gathers files, so
        // the declaration would be dropped here -- in the one direction that
        // matters, since the artifact this builds is what runs where there is no
        // .NET SDK. DOTCL-NUGET-ASDF turns the declarations back into source,
        // which goes in front of the system's own code so the packages are
        // registered before anything names a type from them. The package exists
        // only when the .asd asked for it (:defsystem-depends-on), which
        // FIND-SYSTEM here has by then loaded.
        var setupForm = $@"
(progn
  (let* ((sys (asdf:find-system ""{sysEsc}""))
         (nuget-asdf (find-package ""DOTCL-NUGET-ASDF""))
         (preamble (when nuget-asdf
                     (funcall (find-symbol ""SYSTEM-NUGET-PREAMBLE"" nuget-asdf) sys))))
    (when (and (stringp preamble) (plusp (length preamble)))
      (with-open-file (o ""{preambleFile}"" :direction :output
                         :if-exists :supersede :if-does-not-exist :create)
        (write-string preamble o))
      t)))";

        var preSources = new System.Collections.Generic.List<string>();
        var prevEmit = Runtime.EmitBuildSourceLocations;
        Runtime.EmitBuildSourceLocations = true;
        try
        {
            // The prelude runs here as well as being compiled into the image. It
            // says what has to be in place before anything else, and the builder
            // is the first thing that needs it: systems that generate their own
            // sources are built during the walk below, and those builds need
            // whatever the prelude provides as much as the closure does.
            //
            // trivial-gray-streams is the case to keep in mind. It picks the Gray
            // stream package with (:import-from #+dotcl :dotcl-gray ...), and
            // DOTCL-GRAY has to exist when that DEFPACKAGE is EVALUATED, which
            // happens at compile time inside cl-unicode's table generator, long
            // before any source of the closure itself is compiled. Putting the
            // prelude at the head of the compile list cannot help there, because
            // the walk runs before anything is compiled at all.
            Runtime.LoadLispFiles(prelude, "--prelude source");

            var wrotePreamble = Runtime.Eval(MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString(setupForm) })));

            if (prelude != null)
                foreach (var p in prelude)
                    preSources.Add(System.IO.Path.GetFullPath(p));
            if (wrotePreamble is not Nil)
                preSources.Add(preambleFile);

            Runtime.BuildSystemFasl(system, absOut, toplevel, preSources);
        }
        finally
        {
            Runtime.EmitBuildSourceLocations = prevEmit;
            DynamicBindings.Set(hookSym, oldHook);
        }
    }


    /// <summary>
    /// Read the standard metadata slots off an ASDF system, its version among
    /// them. `dotcl pack` uses these as nuspec defaults so a packed tool
    /// describes itself rather than inheriting the description and URLs of the
    /// dotcl packages it was restamped from, and so a project states its
    /// version once, in the .asd, rather than again on every pack command
    /// line. Returns a SystemMeta whose fields are null where the .asd
    /// is silent; returns null if the system cannot be found at all (packing
    /// proceeds: the fasl build reports a missing system with a better error).
    /// </summary>
    private static SystemMeta? ReadSystemMetaCore(string system, string[]? searchPaths = null)
        => ReadSystemMetaCore(system, searchPaths, out _);

    /// <summary>
    /// As above, and says why when there is no metadata: ERROR is null when the
    /// system was read, and otherwise names what went wrong -- the system is not
    /// visible to ASDF, or loading its .asd signalled (the condition's text).
    /// `dotcl pack` reports that and stops. Answering "no metadata" alone made
    /// the missing :version the only thing pack could say, so a .asd that failed
    /// to load surfaced as "missing required option(s): --version".
    /// </summary>
    private static SystemMeta? ReadSystemMetaCore(string system, string[]? searchPaths,
                                                              out string? error)
    {
        error = null;
        try
        {
            Runtime.Eval(MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString("(require \"asdf\")") })));
            PinBundledAsdf();
            RegisterAsdSearchPaths(searchPaths);

            var sysEsc = system.Replace("\\", "\\\\").Replace("\"", "\\\"");
            // :source-control is (:git "url") / (:github "url") / a bare string.
            // Normalize to the url alone here so the C# side stays shapeless.
            //
            // A failure comes back as (:error "text") rather than being signalled:
            // nothing outside this form handles it, and unhandled it would enter
            // the debugger on a closed stdin before anything could report it.
            var form = $@"
(handler-case
    (let ((sys (asdf:find-system ""{sysEsc}"" nil)))
      (if (null sys)
          (list :error (format nil ""system ~a not found; make it visible to ASDF ~
                                     (--asd-search-path, CL_SOURCE_REGISTRY)""
                               ""{sysEsc}""))
          (let ((sc (asdf:system-source-control sys))
                (asd (asdf:system-source-file sys)))
            (list (asdf:system-description sys)
                  (asdf:system-homepage sys)
                  (cond ((stringp sc) sc)
                        ((and (consp sc) (stringp (second sc))) (second sc))
                        ((and (consp sc) (stringp (cdr sc))) (cdr sc)))
                  (asdf:system-author sys)
                  (asdf:system-license sys)
                  (and asd (namestring (make-pathname :name nil :type nil :defaults asd)))
                  ;; SYSTEM-VERSION, not COMPONENT-VERSION: a secondary
                  ;; system (app/exe) that states no :version answers with its
                  ;; primary system's, the way ASDF already answers author,
                  ;; license and description.
                  (asdf:system-version sys)
                  ;; :entry-point is a string naming a function, or a symbol.
                  (let ((e (asdf::component-entry-point sys)))
                    (cond ((stringp e) e)
                          ((and e (symbolp e) (symbol-package e))
                           (format nil ""~a::~a""
                                   (package-name (symbol-package e)) (symbol-name e)))))))))
  (error (c)
    (list :error (format nil ""loading the definition of system ~a failed: ~a""
                         ""{sysEsc}"" c))))";
            var result = MultipleValues.Primary(Runtime.Eval(MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString(form) }))));

            if (result is Cons ec && ec.Car is Symbol k && k.Name == "ERROR"
                && ec.Cdr is Cons msgCell && msgCell.Car is LispString msg)
            {
                error = msg.Value;
                return null;
            }

            var items = new List<string?>();
            var cur = result;
            while (cur is Cons c)
            {
                items.Add(c.Car is LispString s && s.Value.Length > 0 ? s.Value : null);
                cur = c.Cdr;
            }
            while (items.Count < 8) items.Add(null);
            return new DotclBuild.SystemMeta
            {
                Description = items[0],
                Homepage = items[1],
                SourceControlUrl = items[2],
                Author = items[3],
                License = items[4],
                AsdDirectory = items[5],
                // A version ASDF hands back as anything other than a string
                // (:version can be read from a file) lands here as null, which
                // reads the same as a .asd that states no version at all: the
                // command line has to supply one. Better than packing under a
                // version nobody wrote.
                Version = items[6],
                EntryPoint = items[7],
            };
        }
        catch (Exception ex)
        {
            // Metadata is best-effort for callers that only want the fields;
            // ERROR carries the reason for the ones that have to explain it.
            error = ex.Message;
            return null;
        }
    }

    /// <summary>
    /// Pin the running ASDF and UIOP so a build never replaces them. dotcl
    /// ships its own patched ASDF, and a dependency bundle (qlot, a Quicklisp
    /// bundle) often carries stock asdf.asd / uiop.asd as well. ASDF loads a
    /// registered asdf.asd of the same version "to allow loading from modified
    /// source", so merely having the stock one visible made the first .asd load
    /// rebuild ASDF from sources that do not know dotcl, which fails compiling
    /// UIOP's RAW-COMMAND-LINE-ARGUMENTS. Immutable systems are answered from
    /// the running image and never looked up on disk.
    /// </summary>
    internal static void PinBundledAsdf()
    {
        EvalLisp("(progn (asdf:register-immutable-system \"asdf\") "
                 + "(asdf:register-immutable-system \"uiop\"))");
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
            catch { /* unreadable source; leave start where it is */ }
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
