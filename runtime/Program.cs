using System.Reflection;
using DotCL.Emitter;

namespace DotCL;

class Program
{
    static void Main(string[] args)
    {
        // Parallel background JIT on 2nd+ run using method-use profile from previous run.
        // First run: StartProfile is a no-op (profile doesn't exist yet). From 2nd run:
        // cold startup 20-50% faster. Profile is invalidated by .NET when the assembly changes.
        var profileDir = AppContext.BaseDirectory;
        System.Runtime.ProfileOptimization.SetProfileRoot(profileDir);
        System.Runtime.ProfileOptimization.StartProfile("dotcl.profile");

        // Run on a thread with a larger stack to handle deeply nested code
        // (e.g., SBCL cross-compiler macro expansions)
        const int stackSize = 256 * 1024 * 1024; // 256MB
        Exception? threadException = null;
        var thread = new Thread(() => {
            try { MainInner(args); }
            catch (Exception ex) { threadException = ex; }
        }, stackSize);
        thread.Start();
        thread.Join();
        if (threadException != null)
            throw threadException;
    }

    // True when the REPL is active — CancelKeyPress delivers interrupt instead of killing process.
    static bool _replMode = false;

    // Startup profiling: enabled with DOTCL_STARTUP_PROFILE=1. Prints
    // wall-clock elapsed at key phase boundaries to stderr. Cost when
    // disabled: one env-var read + a Stopwatch.StartNew().
    static readonly bool _profile =
        Environment.GetEnvironmentVariable("DOTCL_STARTUP_PROFILE") == "1";
    static readonly System.Diagnostics.Stopwatch _profileSw =
        System.Diagnostics.Stopwatch.StartNew();
    static long _profileLast;
    static void ProfileMark(string label)
    {
        if (!_profile) return;
        var now = _profileSw.ElapsedMilliseconds;
        Console.Error.WriteLine($"[startup-profile] {label,-28} +{now - _profileLast,5} ms  (total {now} ms)");
        _profileLast = now;
    }

    static void MainInner(string[] args)
    {
        // Ensure stdin/stdout/stderr are UTF-8 on Windows (default InputEncoding
        // is the OEM code page, e.g. CP437, which garbles non-ASCII read-line input).
        // OutputEncoding is already UTF-8 on modern .NET, but set explicitly for safety.
        // We also replace Console.In with an explicit UTF-8 StreamReader so that
        // piped input (e.g. from MSYS2 bash) is decoded correctly regardless of
        // whether Console.InputEncoding setter triggers Console.In re-creation.
        var utf8NoBom = new System.Text.UTF8Encoding(encoderShouldEmitUTF8Identifier: false);
        Console.InputEncoding  = utf8NoBom;
        Console.OutputEncoding = utf8NoBom;
        Console.SetIn(new System.IO.StreamReader(Console.OpenStandardInput(), utf8NoBom, detectEncodingFromByteOrderMarks: false, bufferSize: 4096, leaveOpen: true));

        // Enable ANSI VT100 escape sequences on Windows legacy conhost (cmd.exe).
        // Modern .NET enables this implicitly via Console.Out, but redirected
        // stdout / certain hosts skip it. We call SetConsoleMode explicitly so
        // (format t "~C[31mRED~C[0m" #\Esc #\Esc) renders colored on cmd.exe.
        // Opt-out via DOTCL_NO_VT=1.
        if (OperatingSystem.IsWindows() &&
            Environment.GetEnvironmentVariable("DOTCL_NO_VT") != "1")
        {
            EnableWindowsVtMode();
        }

        ProfileMark("main-entry");
        Startup.Initialize();
        ProfileMark("Startup.Initialize");

        // Register Ctrl-C handler: in REPL mode, deliver INTERACTIVE-INTERRUPT condition
        // instead of terminating the process.
        Console.CancelKeyPress += (_, args2) => {
            if (_replMode)
            {
                args2.Cancel = true;          // don't kill the process
                ConditionSystem.RequestInterrupt();
            }
            // else: default behavior — process exits with SIGINT
        };

        // --help / --version: handled before core loading for fast response.
        // Skip for save-application :executable t outputs — those embed a
        // "dotcl.user.fasl" manifest resource and handle their own --help.
        bool hasEmbeddedFasl =
            typeof(Program).Assembly
                .GetManifestResourceStream("dotcl.user.fasl") != null;

        if (!hasEmbeddedFasl && args.Any(a => a == "--help"))
        {
            Console.WriteLine(@"dotcl [options] [script-file [arguments...]]

Options:
  --help                       Display this message
  --version                    Display version information
  --core <file>                Use specified core file
  --load <file>                Load a file
  --eval <expr>                Evaluate an expression
  --script <file>              Run as script (skip init, no REPL)
  --resolve-deps <asd>         Walk an ASDF system's :depends-on graph and
                               emit one fasl path per line in load order
  --manifest-out <path>        With --resolve-deps: write manifest to file
                               instead of stdout
  --root-sources-out <path>    With --resolve-deps: also write the root
                               system's component source paths to <path>
                               (used by MSBuild as Inputs)
  --compile-project <asd>      Concatenate the asd's root system's components
                               and compile-file to --output <path>
  --output <path>              Output path for --compile-project
  --completion <shell>         Emit a shell completion script for
                               pwsh / bash / zsh / fish
  --asd-search-path <dir>      Append <dir> to asdf:*central-registry*
                               after asdf loads (repeatable)
  --target-rid <rid>           With --resolve-deps: prefer
                               <dir>/<name>-r2r-<rid>.fasl over plain
                               <name>.fasl when it exists

Subcommands:
  repl            Start REPL (even with --load/--eval)

Example:
  dotcl --script hello.lisp
  dotcl --resolve-deps MyApp.asd --manifest-out obj/dotcl-deps.txt");
            return;
        }
        if (!hasEmbeddedFasl && args.Any(a => a == "--version"))
        {
            var version = typeof(Program).Assembly
                .GetCustomAttribute<System.Reflection.AssemblyInformationalVersionAttribute>()
                ?.InformationalVersion ?? "unknown";
            Console.WriteLine($"dotcl {version}");
            return;
        }

        // --completion <shell>: emit shell completion script and exit. Handled
        // before core loading so it stays fast (no Lisp init).
        for (int ci = 0; ci < args.Length; ci++)
        {
            if (args[ci] == "--completion")
            {
                var shell = ci + 1 < args.Length ? args[ci + 1] : "pwsh";
                Environment.Exit(CliCompletion.Emit(shell));
                return;
            }
        }

        // --asm: legacy behavior (run .sil directly, load additional scripts, exit)
        // Used by test-a2 and Makefile targets. No REPL, no core auto-discovery.
        if (args.Length >= 2 && args[0] == "--asm")
        {
            try
            {
                RunCore(args[1]);
                bool replMode = false;
                for (int i = 2; i < args.Length; i++)
                {
                    if (args[i] == "--repl")
                        replMode = true;
                    else if (args[i] == "--eval" && i + 1 < args.Length)
                    {
                        i++;
                        var reader = new Reader(new StringReader(args[i]));
                        while (reader.TryRead(out var form))
                            Runtime.Eval(form);
                    }
                    else if (args[i] == "--load" && i + 1 < args.Length)
                    {
                        i++;
                        Runtime.Load(new LispObject[] { new LispString(args[i]) });
                    }
                    else
                        Runtime.Load(new LispObject[] { new LispString(args[i]) });
                }
                if (replMode)
                {
                    RunRepl();
                    return;
                }
            }
            catch (LispSourceException lse)
            {
                Console.Error.WriteLine(lse.FormatTrace());
                Environment.Exit(1);
            }
            return;
        }


        // New-style invocation: auto-discover (or --core override) + optional scripts + REPL
        // Supports --load <file> (SBCL-compatible) and --eval <expr> interleaved with scripts.
        var rest = new List<string>(args);
        string? coreOverride = null;
        for (int i = 0; i < rest.Count - 1; i++)
        {
            if (rest[i] == "--core")
            {
                coreOverride = rest[i + 1];
                rest.RemoveRange(i, 2);
                break;
            }
        }
        // Extract --script <file>
        string? scriptFile = null;
        for (int i = 0; i < rest.Count - 1; i++)
        {
            if (rest[i] == "--script")
            {
                scriptFile = rest[i + 1];
                rest.RemoveRange(i, 2);
                break;
            }
        }
        bool scriptMode = scriptFile != null;

        // Extract --resolve-deps <asd> [--manifest-out <path>] [--root-sources-out <path>]
        // Used by build tooling (#166): walk the .asd's :depends-on graph,
        // emit one fasl path per line in load order. Optionally also writes
        // the root system's component source files (one per line) to a
        // separate file, used by MSBuild as Inputs for the root compile.
        string? resolveDepsAsd = null;
        string? resolveDepsManifestOut = null;
        string? resolveDepsRootSourcesOut = null;
        for (int i = 0; i < rest.Count - 1; i++)
        {
            if (rest[i] == "--resolve-deps")
            {
                resolveDepsAsd = rest[i + 1];
                rest.RemoveRange(i, 2);
                break;
            }
        }
        for (int i = 0; i < rest.Count - 1; i++)
        {
            if (rest[i] == "--manifest-out")
            {
                resolveDepsManifestOut = rest[i + 1];
                rest.RemoveRange(i, 2);
                break;
            }
        }
        for (int i = 0; i < rest.Count - 1; i++)
        {
            if (rest[i] == "--root-sources-out")
            {
                resolveDepsRootSourcesOut = rest[i + 1];
                rest.RemoveRange(i, 2);
                break;
            }
        }

        // --target-rid <rid>: when set, --resolve-deps prefers
        // <dir>/<name>-r2r-<rid>.fasl over plain <dir>/<name>.fasl. Used by
        // release pack pipelines that pre-compile per-RID R2R fasls (#170).
        // Falls back to the IL-only fasl silently if the R2R variant is
        // missing, so dev builds without R2R artifacts keep working.
        string? targetRid = null;
        for (int i = 0; i < rest.Count - 1; i++)
        {
            if (rest[i] == "--target-rid")
            {
                targetRid = rest[i + 1];
                rest.RemoveRange(i, 2);
                break;
            }
        }

        // Extract --asd-search-path <dir> (repeatable). These are appended to
        // asdf:*central-registry* after asdf loads, in addition to the
        // standard QL/CL source registry locations auto-detected at boot.
        for (int i = 0; i < rest.Count - 1; )
        {
            if (rest[i] == "--asd-search-path")
            {
                Runtime.UserAsdSearchPaths.Add(rest[i + 1]);
                rest.RemoveRange(i, 2);
                continue;
            }
            i++;
        }

        // Extract --compile-project <asd> --output <fasl-path>
        // Concatenates the asd's root system's :components in order, then
        // compile-files the result to <fasl-path>. MSBuild calls this with
        // proper Inputs/Outputs to skip rebuilds when sources haven't moved.
        string? compileProjectAsd = null;
        string? compileProjectOutput = null;
        for (int i = 0; i < rest.Count - 1; i++)
        {
            if (rest[i] == "--compile-project")
            {
                compileProjectAsd = rest[i + 1];
                rest.RemoveRange(i, 2);
                break;
            }
        }
        for (int i = 0; i < rest.Count - 1; i++)
        {
            if (rest[i] == "--output")
            {
                compileProjectOutput = rest[i + 1];
                rest.RemoveRange(i, 2);
                break;
            }
        }

        // Extract "repl" subcommand
        bool explicitRepl = false;
        for (int i = 0; i < rest.Count; i++)
        {
            if (rest[i] == "repl")
            {
                explicitRepl = true;
                rest.RemoveAt(i);
                break;
            }
        }

        // Collect ordered list of (kind, value): kind = "script" | "load" | "eval"
        var actions = new List<(string kind, string value)>();
        for (int i = 0; i < rest.Count; i++)
        {
            if ((rest[i] == "--load" || rest[i] == "--eval") && i + 1 < rest.Count)
            {
                actions.Add((rest[i][2..], rest[i + 1]));
                i++;
            }
            else if (!rest[i].StartsWith('-'))
            {
                actions.Add(("script", rest[i]));
            }
        }
        var scripts = actions; // renamed for clarity below

        ProfileMark("arg-parse");

        // Find and boot core (compiler + stdlib)
        var corePath = coreOverride ?? FindCore();
        ProfileMark("FindCore");
        if (corePath != null)
        {
            try { RunCore(corePath); }
            catch (LispSourceException lse)
            {
                Console.Error.WriteLine(lse.FormatTrace());
                Environment.Exit(1);
            }
            ProfileMark("RunCore");
        }

        // save-application :executable t output: run the embedded user.fasl
        // then exit. Produced via `dotnet publish /p:DotclUserFasl=...` which
        // bundles the user's compiled .fasl as a manifest resource named
        // "dotcl.user.fasl" (see runtime.csproj, D679). Skipped for normal
        // runs — the resource is only present in save-application-built exes.
        if (TryRunEmbeddedUserFasl())
            return;

        // --resolve-deps <asd> [--manifest-out <p>] [--root-sources-out <p>]:
        // walk the .asd's :depends-on graph and emit one fasl path per line.
        // With --root-sources-out, also write the root system's component
        // source files. Project-core build tools (#166) invoke this.
        if (resolveDepsAsd != null)
        {
            try { RunResolveDeps(resolveDepsAsd, resolveDepsManifestOut, resolveDepsRootSourcesOut, targetRid); }
            catch (LispSourceException lse)
            {
                Console.Error.WriteLine(lse.FormatTrace());
                Environment.Exit(1);
            }
            return;
        }

        // --compile-project <asd> --output <fasl-path>: concatenate the .asd's
        // root system's :components in declared order, compile-file the
        // result. Used by the MSBuild target with Inputs/Outputs.
        if (compileProjectAsd != null)
        {
            if (compileProjectOutput == null)
            {
                Console.Error.WriteLine("--compile-project requires --output <path>");
                Environment.Exit(2);
            }
            try { RunCompileProject(compileProjectAsd, compileProjectOutput); }
            catch (LispSourceException lse)
            {
                Console.Error.WriteLine(lse.FormatTrace());
                Environment.Exit(1);
            }
            return;
        }

        // --script mode: register #! reader macro, execute script, exit
        if (scriptMode)
        {
            // Set *debugger-hook* to print error and exit (no interactive debugger)
            var hookSym = Startup.Sym("*DEBUGGER-HOOK*");
            DynamicBindings.Set(hookSym, new LispFunction(hookArgs => {
                var cond = hookArgs[0];
                var msg = cond is LispCondition lc3 ? lc3.Message : cond.ToString();
                var typeName = cond is LispCondition lc4 ? lc4.ConditionTypeName : "ERROR";
                Console.Error.WriteLine($"{typeName}: {msg}");
                Environment.Exit(1);
                return Nil.Instance;
            }, "*SCRIPT-DEBUGGER-HOOK*", 2));

            try
            {
                // Register #! as line comment for shebang support
                var shebangReader = new Reader(new StringReader(
                    "(set-dispatch-macro-character #\\# #\\! (lambda (s c n) (read-line s nil nil) (values)))"));
                if (shebangReader.TryRead(out var shebangForm))
                    Runtime.Eval(shebangForm);

                Runtime.Load(new LispObject[] { new LispString(scriptFile!) });
            }
            catch (LispSourceException lse)
            {
                Console.Error.WriteLine(lse.FormatTrace());
                Environment.Exit(1);
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine($"Error: {ex.Message}");
                if (Startup.DebugStacktrace) Console.Error.WriteLine(ex.StackTrace);
                Environment.Exit(1);
            }
            return;
        }

        // Load user init file (unless --script mode)
        {
            var configDir = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData);
            var initFile = Path.Combine(configDir, "dotcl", "init.lisp");
            if (File.Exists(initFile))
            {
                try
                {
                    Runtime.Load(new LispObject[] { new LispString(initFile) });
                }
                catch (LispSourceException lse)
                {
                    Console.Error.WriteLine($"Error loading init file {initFile}:");
                    Console.Error.WriteLine(lse.FormatTrace());
                }
            }
        }
        ProfileMark("init-file");

        // Execute actions in order
        foreach (var (kind, value) in scripts)
        {
            try
            {
                if (kind == "eval")
                {
                    var reader = new Reader(new StringReader(value));
                    while (reader.TryRead(out var form))
                        Runtime.Eval(form);
                }
                else // "script" or "load"
                    Runtime.Load(new LispObject[] { new LispString(value) });
            }
            catch (LispSourceException lse)
            {
                Console.Error.WriteLine(lse.FormatTrace());
                Environment.Exit(1);
            }
        }

        ProfileMark("actions");

        // REPL if: explicit "repl" subcommand, or no actions.
        if (explicitRepl || scripts.Count == 0)
            RunRepl();
    }

    /// <summary>
    /// Search for dotcl.core in standard locations.
    /// Supports dotnet-tool layout (./dotcl.core) and
    /// Unix FHS layout (../share/dotcl/dotcl.core relative to bin/).
    /// </summary>
    static string? FindCore()
    {
        var baseDir = AppContext.BaseDirectory;
        var candidates = new[]
        {
            // dotnet tool: files co-located with the assembly
            Path.Combine(baseDir, "dotcl.core"),
            // Unix FHS: /usr/share/dotcl/dotcl.core  (bin is one level up from share)
            Path.Combine(baseDir, "..", "share", "dotcl", "dotcl.core"),
            // dev fallback: running from runtime/bin/Debug/net*/
            Path.Combine(baseDir, "..", "..", "..", "..", "compiler", "cil-out.sil"),
        };
        return candidates.Select(Path.GetFullPath).FirstOrDefault(File.Exists);
    }

    /// <summary>Load and execute a compiled core (.sil text or .fasl PE assembly).</summary>
    static void RunCore(string filePath)
    {
        // Detect PE signature ("MZ") at byte 0 — PersistedAssemblyBuilder output.
        // Any other bytes → treat as SIL text and fall through to Reader.
        byte[] header = new byte[2];
        using (var fs = File.OpenRead(filePath))
        {
            int n = fs.Read(header, 0, 2);
            if (n >= 2 && header[0] == 0x4D && header[1] == 0x5A)
            {
                RunCoreFasl(filePath);
                return;
            }
        }

        var source = File.ReadAllText(filePath);
        var reader = new Reader(new StringReader(source));

        if (!reader.TryRead(out var instrList))
        {
            Console.Error.WriteLine($"Error: empty core file: {filePath}");
            return;
        }

        var packageSym = Startup.Sym("*PACKAGE*");
        var oldPackage = DynamicBindings.Get(packageSym);
        try
        {
            CilAssembler.AssembleAndRun(instrList);
        }
        finally
        {
            DynamicBindings.Set(packageSym, oldPackage);
        }
    }

    /// <summary>
    /// Implementation of <c>--resolve-deps &lt;asd&gt;</c>. Loads ASDF, loads
    /// the user's .asd, walks its <c>:depends-on</c> graph in dependency-first
    /// load order, and emits one absolute fasl path per line of the deps
    /// (excluding the root system itself). Output goes to stdout or
    /// <paramref name="manifestOut"/>.
    ///
    /// When <paramref name="rootSourcesOut"/> is non-null, also writes the
    /// root system's <c>:components</c> source paths (one per declared order)
    /// to that file. The MSBuild target uses this list as Inputs to its root
    /// compile target so source-file mtimes drive incremental rebuilds.
    /// </summary>
    static void RunResolveDeps(string asdPath, string? manifestOut, string? rootSourcesOut, string? targetRid = null)
    {
        var absAsd = System.IO.Path.GetFullPath(asdPath);
        if (!System.IO.File.Exists(absAsd))
        {
            Console.Error.WriteLine($"--resolve-deps: file not found: {absAsd}");
            Environment.Exit(2);
        }

        // Bring asdf in. (require "asdf") goes through module-provide-contrib
        // and side-effects *central-registry* with shipped contrib subdirs.
        Runtime.Eval(MultipleValues.Primary(
            Runtime.ReadFromString(new LispObject[] { new LispString("(require \"asdf\")") })));

        // Run the dep walker as a small Lisp form. The form:
        //   1. (asdf:load-asd <abs-asd>) so the user's system is registered.
        //   2. Recursively walk asdf:system-depends-on, returning systems
        //      in dependency-first order (deps before dependents).
        //   3. For each system, format a "<name>\t<fasl-path>" line where
        //      fasl-path is "<dir-of-asd>/<name>.fasl".
        //   4. Write all lines to <manifestOut> (or stdout via *standard-output*).
        var asdLisp = absAsd.Replace("\\", "/");
        var manifestForm = manifestOut == null
            ? "*standard-output*"
            : $"(open \"{manifestOut.Replace("\\", "/")}\" :direction :output :if-exists :supersede)";
        var rootSourcesForm = rootSourcesOut == null
            ? "nil"
            : $"(open \"{rootSourcesOut.Replace("\\", "/")}\" :direction :output :if-exists :supersede)";
        // Manifest format: one absolute fasl path per line. Simple to parse
        // (MSBuild ReadLinesFromFile + Copy directly), and the system name
        // can be derived from the filename when needed for diagnostics.
        //
        // For each dep system, if <dir>/<name>.fasl exists, use it. Otherwise
        // concatenate-source-op + compile-file the dep's :components into
        // <dir>/<name>.fasl on the fly. This unlocks any QL library
        // (alexandria, etc.) without dotcl needing to ship pre-built fasls
        // for every package out there. Empty :components (marker systems)
        // are skipped silently.
        var form = $@"
(let* ((seen '()) (order '()))
  (labels ((walk (sys)
             (unless (member sys seen :test #'eq)
               (push sys seen)
               (dolist (d (asdf:system-depends-on sys))
                 (let ((ds (ignore-errors (asdf:find-system d))))
                   (when ds (walk ds))))
               (push sys order)))
           (ensure-fasl (sys)
             (let* ((src  (asdf:component-pathname sys))
                    (dir  (directory-namestring src))
                    (name (asdf:component-name sys))
                    ;; If --target-rid is given, prefer the R2R variant
                    ;; <dir>/<name>-r2r-<rid>.fasl when it exists. Falls
                    ;; back to plain <dir>/<name>.fasl otherwise (#170).
                    (r2r-fasl {(targetRid == null
                        ? "nil"
                        : $"(concatenate 'string dir name \"-r2r-\" \"{targetRid}\" \".fasl\")")})
                    (fasl (concatenate 'string dir name "".fasl"")))
               (when (and r2r-fasl (probe-file r2r-fasl))
                 (return-from ensure-fasl r2r-fasl))
               (unless (probe-file fasl)
                 (when (asdf:component-children sys)
                   (format *error-output*
                           ""[resolve-deps] compiling ~A...~%"" name)
                   ;; Delegate ordering / nested-module traversal to ASDF
                   ;; (alexandria has :module components with their own
                   ;; :depends-on, so a flat (asdf:component-children sys)
                   ;; walk would miss the right order).
                   (asdf:operate 'asdf::concatenate-source-op sys)
                   (let ((concat (first
                                  (asdf:output-files
                                   (asdf:make-operation 'asdf::concatenate-source-op)
                                   sys))))
                     (compile-file concat :output-file fasl))))
               fasl)))
    (asdf:load-asd ""{asdLisp}"")
    (let* ((root (asdf:find-system
                   (pathname-name (pathname ""{asdLisp}""))))
           (deps (remove root (nreverse (progn (walk root) order)))))
      ;; Emit deps' fasl paths to manifestOut/stdout, compiling on-the-fly
      ;; if absent. Skip systems whose :components is empty (marker .asd).
      (let ((stream {manifestForm}))
        (unwind-protect
          (dolist (sys deps)
            (when (asdf:component-children sys)
              (let ((fasl (ensure-fasl sys)))
                (format stream ""~A~%"" fasl))))
          (when {(manifestOut == null ? "nil" : "t")} (close stream))))
      ;; If --root-sources-out is given, emit the root system's component
      ;; source paths in declared order. (asdf:component-children root)
      ;; preserves :components order.
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
    /// Implementation of <c>--compile-project &lt;asd&gt; --output &lt;fasl&gt;</c>.
    /// Concatenates the .asd's root system's <c>:components</c> in declared
    /// order using <c>asdf::concatenate-files</c>, then <c>compile-file</c>s
    /// the result into <paramref name="outputPath"/>.
    ///
    /// Only the root system is compiled — :depends-on'd contribs stay as
    /// pre-built fasls (resolved via --resolve-deps and bundled separately).
    /// MSBuild owns the incremental decision via Inputs/Outputs on the
    /// component source files.
    /// </summary>
    static void RunCompileProject(string asdPath, string outputPath)
    {
        var absAsd = System.IO.Path.GetFullPath(asdPath);
        if (!System.IO.File.Exists(absAsd))
        {
            Console.Error.WriteLine($"--compile-project: file not found: {absAsd}");
            Environment.Exit(2);
        }
        var absOut = System.IO.Path.GetFullPath(outputPath);
        var outDir = System.IO.Path.GetDirectoryName(absOut);
        if (!string.IsNullOrEmpty(outDir) && !System.IO.Directory.Exists(outDir))
            System.IO.Directory.CreateDirectory(outDir);

        Runtime.Eval(MultipleValues.Primary(
            Runtime.ReadFromString(new LispObject[] { new LispString("(require \"asdf\")") })));

        var asdLisp = absAsd.Replace("\\", "/");
        var outLisp = absOut.Replace("\\", "/");
        // Concatenate .lisp output sits next to the asd in build cache. We
        // pin it to outDir so it's predictable / debuggable.
        var concatLisp = (outDir == null ? "" : outDir.Replace("\\", "/") + "/")
                       + System.IO.Path.GetFileNameWithoutExtension(outputPath)
                       + ".concat.lisp";
        // Wrap in progn so ReadFromString picks up the whole multi-form body
        // as one expression (it reads a single top-level form per call).
        var form = $@"
(progn
  (asdf:load-asd ""{asdLisp}"")
  (let* ((root (asdf:find-system
                 (pathname-name (pathname ""{asdLisp}""))))
         (sources (mapcar #'asdf:component-pathname
                          (asdf:component-children root))))
    (asdf::concatenate-files sources ""{concatLisp}"")
    (compile-file ""{concatLisp}"" :output-file ""{outLisp}"")))";
        Runtime.Eval(MultipleValues.Primary(
            Runtime.ReadFromString(new LispObject[] { new LispString(form) })));
    }

    /// <summary>
    /// Check for an embedded "dotcl.user.fasl" manifest resource — present only
    /// in exes produced by dotcl:save-application with :executable t. When found,
    /// loads the embedded PE/FASL via Assembly.Load and invokes its ModuleInit.
    /// Returns true if a resource was present and run; false otherwise.
    /// </summary>
    static bool TryRunEmbeddedUserFasl()
    {
        var selfAsm = typeof(Program).Assembly;
        using var stream = selfAsm.GetManifestResourceStream("dotcl.user.fasl");
        if (stream == null) return false;

        using var ms = new MemoryStream();
        stream.CopyTo(ms);
        var userAsm = System.Reflection.Assembly.Load(ms.ToArray());
        var t = userAsm.GetType("CompiledModule")
            ?? throw new InvalidOperationException(
                "embedded dotcl.user.fasl: CompiledModule type not found");
        var mi = t.GetMethod("ModuleInit",
            System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Static)
            ?? throw new InvalidOperationException(
                "embedded dotcl.user.fasl: ModuleInit method not found");

        var packageSym = Startup.Sym("*PACKAGE*");
        var oldPackage = DynamicBindings.Get(packageSym);
        try { mi.Invoke(null, null); }
        finally { DynamicBindings.Set(packageSym, oldPackage); }
        return true;
    }

    /// <summary>Load a pre-compiled FASL core (PE assembly) and invoke its ModuleInit.</summary>
    static void RunCoreFasl(string filePath)
    {
        var asm = System.Reflection.Assembly.LoadFrom(filePath);
        var t = asm.GetType("CompiledModule")
            ?? throw new InvalidOperationException($"FASL core {filePath}: CompiledModule type not found");
        var mi = t.GetMethod("ModuleInit",
            System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Static)
            ?? throw new InvalidOperationException($"FASL core {filePath}: ModuleInit method not found");

        var packageSym = Startup.Sym("*PACKAGE*");
        var oldPackage = DynamicBindings.Get(packageSym);
        try
        {
            mi.Invoke(null, null);
        }
        finally
        {
            DynamicBindings.Set(packageSym, oldPackage);
        }
    }


    static void RunRepl()
    {
        _replMode = true;
        Console.WriteLine("dotcl REPL. Ctrl+D to exit.");

        var buffer = new System.Text.StringBuilder();

        while (true)
        {
            var pkg = DynamicBindings.Get(Startup.Sym("*PACKAGE*")) as Package;
            var pkgName = pkg != null
                ? new[] { pkg.Name }.Concat(pkg.Nicknames).OrderBy(n => n.Length).First()
                : "CL-USER";

            var primary = $"{pkgName}> ";
            var prompt = buffer.Length == 0 ? primary : new string(' ', primary.Length);

            Console.Write(prompt);
            string? line = Console.ReadLine();

            if (line == null)
            {
                // EOF. Drop any pending partial form and exit.
                break;
            }
            if (buffer.Length == 0 && string.IsNullOrWhiteSpace(line)) continue;

            if (buffer.Length > 0) buffer.Append('\n');
            buffer.Append(line);

            // Try to read all forms from the accumulated buffer. Reader
            // signals "more input needed" by throwing EndOfStreamException
            // (from ReadStep1 on raw EOF) or a LispError of condition type
            // END-OF-FILE (from mid-list / mid-string etc. — see
            // Reader.MakeEndOfFileError). Both mean "keep the buffer and
            // re-prompt with the continuation indent". Anything else is a
            // real syntax error: print and drop the buffer.
            var forms = new List<LispObject>();
            bool incomplete = false;
            bool readError = false;
            try
            {
                var reader = new Reader(new StringReader(buffer.ToString()));
                while (reader.TryRead(out var expr))
                    forms.Add(expr);
            }
            catch (EndOfStreamException)
            {
                incomplete = true;
            }
            catch (LispErrorException ex) when (
                ex.Condition is LispCondition lc && lc.ConditionTypeName == "END-OF-FILE")
            {
                incomplete = true;
            }
            catch (LispErrorException ex)
            {
                Console.Error.WriteLine($"; read error: {ex.Condition}");
                readError = true;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine($"; read error: {ex.Message}");
                readError = true;
            }

            if (incomplete) continue;
            if (readError) { buffer.Clear(); continue; }
            buffer.Clear();

            // Establish ABORT restart that returns to REPL prompt
            var abortTag = new object();
            var abortRestart = new LispRestart("ABORT",
                _ => Nil.Instance,
                description: "Return to top level.",
                tag: abortTag);
            RestartClusterStack.PushCluster(new[] { abortRestart });
            try
            {
                foreach (var form in forms)
                {
                    var result = Runtime.Eval(form);
                    Console.WriteLine(Runtime.FormatTop(result, true));
                }
            }
            catch (RestartInvocationException rie) when (ReferenceEquals(rie.Tag, abortTag))
            {
                // ABORT restart invoked → return to prompt
            }
            catch (LispErrorException ex) when (ex.Condition is LispInteractiveInterrupt)
            {
                Console.Error.WriteLine("; Interrupted.");
            }
            catch (LispErrorException ex)
            {
                Console.Error.WriteLine($"; {ex.Condition}");
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine($"Error: {ex.Message}");
                if (Startup.DebugStacktrace) Console.Error.WriteLine(ex.StackTrace);
            }
            finally
            {
                RestartClusterStack.PopCluster();
            }
        }
    }

    private const int STD_OUTPUT_HANDLE = -11;
    private const int STD_ERROR_HANDLE  = -12;
    private const uint ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x0004;

    [System.Runtime.InteropServices.DllImport("kernel32.dll", SetLastError = true)]
    private static extern IntPtr GetStdHandle(int nStdHandle);

    [System.Runtime.InteropServices.DllImport("kernel32.dll", SetLastError = true)]
    private static extern bool GetConsoleMode(IntPtr hConsoleHandle, out uint lpMode);

    [System.Runtime.InteropServices.DllImport("kernel32.dll", SetLastError = true)]
    private static extern bool SetConsoleMode(IntPtr hConsoleHandle, uint dwMode);

    private static void EnableWindowsVtMode()
    {
        foreach (var which in new[] { STD_OUTPUT_HANDLE, STD_ERROR_HANDLE })
        {
            try
            {
                var h = GetStdHandle(which);
                if (h == IntPtr.Zero || h == new IntPtr(-1)) continue;
                if (!GetConsoleMode(h, out var mode)) continue;
                SetConsoleMode(h, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
            }
            catch
            {
                // Best-effort: fail silently if console isn't attached or P/Invoke fails.
            }
        }
    }
}
