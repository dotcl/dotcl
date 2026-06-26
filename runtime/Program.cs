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

        // Restore the terminal on exit (Unix). .NET's Console driver switches
        // the terminal into "application" keypad / cursor-key mode (terminfo
        // smkx: ESC[?1h ESC=) the first time it reads interactively, but does
        // not reliably emit the matching reset (rmkx: ESC[?1l ESC>) when the
        // process exits via Environment.Exit / EOF / signal. The terminal is
        // then left in application mode, so arrow keys send ESC O A instead of
        // ESC [ A — this conflicts with rlwrap's readline (garbled / "16R"
        // cursor-report fragments) and requires `stty sane` after a crash.
        // We emit the reset ourselves on ProcessExit. Best-effort, TTY only,
        // opt-out via DOTCL_NO_TTY_RESTORE=1.
        if (!OperatingSystem.IsWindows() &&
            Environment.GetEnvironmentVariable("DOTCL_NO_TTY_RESTORE") != "1")
        {
            AppDomain.CurrentDomain.ProcessExit += (_, _) => RestoreTerminal();

            // ProcessExit does NOT fire when the process is killed by a signal
            // (SIGTERM from `kill`, SIGHUP on terminal close, SIGQUIT) — the
            // terminal is then left in application mode and needs `stty sane`
            // (public dotcl/dotcl#37 symptom 2, signal path). Trap the catchable
            // termination signals, restore the terminal, then let the default
            // action run (Cancel stays false) so exit semantics are unchanged.
            // SIGKILL is uncatchable by design and cannot be handled.
            foreach (var sig in new[] { System.Runtime.InteropServices.PosixSignal.SIGTERM,
                                        System.Runtime.InteropServices.PosixSignal.SIGHUP,
                                        System.Runtime.InteropServices.PosixSignal.SIGQUIT })
            {
                try
                {
                    // Keep the registration alive: PosixSignalRegistration is
                    // IDisposable and unregisters when GC'd, so the handle must
                    // be rooted for the process lifetime.
                    _signalRegistrations.Add(
                        System.Runtime.InteropServices.PosixSignalRegistration.Create(
                            sig, _ => RestoreTerminal()));
                }
                catch { /* signal not supported on this platform — best-effort */ }
            }
        }

        // --help / --version: handled before core loading for fast response.
        // Skip for save-application :executable t outputs — those embed a
        // "dotcl.user.fasl" manifest resource and handle their own --help.
        bool hasEmbeddedFasl =
            typeof(Program).Assembly
                .GetManifestResourceStream("dotcl.user.fasl") != null;

        if (!hasEmbeddedFasl && args.Any(a => a == "--help"))
        {
            Console.WriteLine(@"dotcl [options] [script-file [arguments...]]

Usage:
  dotcl                        Start a REPL
  dotcl file.lisp [args...]    Run file.lisp as a script, then exit
  dotcl --load file.lisp       Load file.lisp and continue in the REPL
  dotcl --eval ""(+ 1 2)""       Evaluate an expression, then exit

Options:
  --help                       Display this message
  --version                    Display version information
  --core <file>                Use specified core file
  --load <file>                Load a file (REPL continues unless an action exits)
  --eval <expr>                Evaluate an expression
  --no-init                    Skip loading the user init file (REPL/--eval/--load)
  --readline / --no-readline   Force the line-editing REPL on / off
                               (default: on for an interactive console)
  --completion <shell>         Emit a shell completion script for
                               pwsh / bash / zsh / fish
  --asd-search-path <dir>      Append <dir> to asdf:*central-registry*
                               after asdf loads (repeatable)

Subcommands:
  repl                         Start REPL (even with --load/--eval)
  build <asd> --output <fasl>  Compile an ASDF system to a fasl

Example:
  dotcl hello.lisp arg1 arg2
  dotcl --eval ""(format t \""hi~%\"")""
  dotcl build MyApp.asd --output obj/MyApp.fasl

Build-tooling flags (--resolve-deps / --compile-project / etc.) are internal
and invoked by the MSBuild integration; they are intentionally omitted here.");
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
            catch (LispErrorException lee)
            {
                Console.Error.WriteLine($"Error: {lee.Message}");
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

        // `build` subcommand — the user-facing entry point for ASDF project
        // builds. Invoked by the MSBuild integration (runtime/build/Dotcl.targets).
        // Two modes off the same positional <asd>:
        //   dotcl build <asd> --output <fasl>
        //       Concatenate the root system's :components and compile-file to
        //       <fasl>. (compile-project)
        //   dotcl build <asd> --resolve-deps --manifest-out <p>
        //                     [--root-sources-out <p>] [--target-rid <rid>]
        //       Walk the :depends-on graph, emit one fasl path per line in load
        //       order to <p> (or stdout). With --root-sources-out, also emit the
        //       root system's component source paths (MSBuild Inputs).
        //       --target-rid prefers <dir>/<name>-r2r-<rid>.fasl when present.
        // The flags below are build-internal and intentionally absent from
        // --help / completion.
        bool buildMode = rest.Count > 0 && rest[0] == "build";
        string? buildAsd = null;
        string? buildOutput = null;
        bool buildResolveDeps = false;
        string? buildManifestOut = null;
        string? buildRootSourcesOut = null;
        string? buildTargetRid = null;
        var buildInit = new List<string>();
        var buildSearchPaths = new List<string>();
        if (buildMode)
        {
            rest.RemoveAt(0);
            for (int i = 0; i < rest.Count; i++)
            {
                var a = rest[i];
                if (a == "--output" && i + 1 < rest.Count) buildOutput = rest[++i];
                else if (a == "--resolve-deps") buildResolveDeps = true;
                else if (a == "--manifest-out" && i + 1 < rest.Count) buildManifestOut = rest[++i];
                else if (a == "--root-sources-out" && i + 1 < rest.Count) buildRootSourcesOut = rest[++i];
                else if (a == "--target-rid" && i + 1 < rest.Count) buildTargetRid = rest[++i];
                else if (a == "--build-init" && i + 1 < rest.Count) buildInit.Add(rest[++i]);
                else if (a == "--asd-search-path" && i + 1 < rest.Count) buildSearchPaths.Add(rest[++i]);
                else if (!a.StartsWith('-') && buildAsd == null) buildAsd = a;
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

        // Extract --no-init: skip loading %APPDATA%/dotcl/init.lisp in REPL /
        // --eval / --load sessions. Script mode (dotcl file.lisp) already skips
        // the init file regardless, so this only affects interactive/eval runs.
        bool noInit = false;
        for (int i = 0; i < rest.Count; i++)
        {
            if (rest[i] == "--no-init")
            {
                noInit = true;
                rest.RemoveAt(i);
                break;
            }
        }

        // Extract --readline / --no-readline: control the line-editing REPL
        // (dotcl-repl contrib: history, cursor movement). null = auto: enable
        // when stdin is an interactive console, disable when redirected/piped.
        // Only meaningful for the REPL path; ignored in script/eval runs.
        bool? readlinePref = null;
        for (int i = 0; i < rest.Count; i++)
        {
            if (rest[i] == "--readline") { readlinePref = true; rest.RemoveAt(i); break; }
            if (rest[i] == "--no-readline") { readlinePref = false; rest.RemoveAt(i); break; }
        }

        // Collect ordered --load/--eval actions. The FIRST bare (non-flag) token
        // is the positional script file; once seen, parsing stops and every
        // remaining token (flag or not) becomes the script's argv — the Unix
        // convention that args after the script belong to the program, not to
        // dotcl. This also stops data args (e.g. an image path) from being
        // mis-loaded as Lisp source.
        var actions = new List<(string kind, string value)>();
        string? positionalScript = null;
        List<string> positionalArgv = new();
        for (int i = 0; i < rest.Count; i++)
        {
            if ((rest[i] == "--load" || rest[i] == "--eval") && i + 1 < rest.Count)
            {
                actions.Add((rest[i][2..], rest[i + 1]));
                i++;
            }
            else if (!rest[i].StartsWith('-'))
            {
                positionalScript = rest[i];
                positionalArgv = rest.GetRange(i + 1, rest.Count - (i + 1));
                break;
            }
        }
        bool scriptMode = positionalScript != null;
        var scripts = actions; // pre-script --load/--eval actions

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
        // "dotcl.user.fasl" (see runtime.csproj). Skipped for normal
        // runs — the resource is only present in save-application-built exes.
        if (TryRunEmbeddedUserFasl())
            return;

        // `build` subcommand dispatch. --resolve-deps walks the :depends-on
        // graph; otherwise --output compile-files the root system. Both need the
        // core booted (they drive asdf). Used by runtime/build/Dotcl.targets.
        if (buildMode)
        {
            if (buildAsd == null)
            {
                Console.Error.WriteLine("build: missing <asd> path");
                Environment.Exit(2);
            }
            try
            {
                var buildInitArr = buildInit.Count > 0 ? buildInit.ToArray() : null;
                var searchPathArr = buildSearchPaths.Count > 0 ? buildSearchPaths.ToArray() : null;
                if (buildResolveDeps)
                    RunResolveDeps(buildAsd, buildManifestOut, buildRootSourcesOut, buildTargetRid, buildInitArr, searchPathArr);
                else if (buildOutput != null)
                    RunCompileProject(buildAsd, buildOutput, buildInitArr, searchPathArr);
                else
                {
                    Console.Error.WriteLine("build: requires --output <fasl> or --resolve-deps");
                    Environment.Exit(2);
                }
            }
            catch (LispSourceException lse)
            {
                Console.Error.WriteLine(lse.FormatTrace());
                Environment.Exit(1);
            }
            return;
        }

        // Positional script mode: `dotcl file.lisp [args...]`. Any preceding
        // --load/--eval run first, then the script, then exit (no REPL). The
        // trailing args are exposed to uiop:command-line-arguments via
        // Runtime.ScriptArgs. Non-interactive: errors print and exit non-zero.
        if (scriptMode)
        {
            // Expose trailing args to (uiop:command-line-arguments).
            Runtime.ScriptArgs = positionalArgv;

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

                // Run any --load/--eval that preceded the script file.
                foreach (var (kind, value) in scripts)
                {
                    if (kind == "eval")
                    {
                        var reader = new Reader(new StringReader(value));
                        while (reader.TryRead(out var form))
                            Runtime.Eval(form);
                    }
                    else // "load"
                        Runtime.Load(new LispObject[] { new LispString(value) });
                }

                Runtime.Load(new LispObject[] { new LispString(positionalScript!) });
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

        // Load user init file (unless script mode or --no-init)
        if (!noInit)
        {
            var initFile = Startup.UserInitFilePath();
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
        {
            // Auto-enable the line-editing REPL (dotcl-repl) unless overridden.
            // Default: on for an interactive console, off when stdin is piped /
            // redirected (the custom reader uses Console.ReadKey, which has no
            // meaning for non-interactive input). A user init file may already
            // have enabled it — don't clobber that.
            bool enableReadline = readlinePref ?? !Console.IsInputRedirected;
            if (enableReadline && Startup.ReadlineHook == null)
                TryEnableReadline();
            RunRepl();
        }
    }

    /// <summary>
    /// Load the dotcl-repl contrib and wire its line editor into the REPL read
    /// loop. Best-effort: if the contrib is missing or fails to load, fall back
    /// to the basic Console.ReadLine path with a one-line note on stderr.
    /// </summary>
    static void TryEnableReadline()
    {
        try
        {
            Runtime.Eval(MultipleValues.Primary(Runtime.ReadFromString(new LispObject[] {
                new LispString(
                    "(progn (require \"dotcl-repl\") (funcall (find-symbol \"ENABLE\" \"DOTCL-REPL\")))")
            })));
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine(
                $"; readline unavailable ({ex.Message}); using basic line input");
        }
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
    static void RunResolveDeps(string asdPath, string? manifestOut, string? rootSourcesOut, string? targetRid = null, string[]? buildInit = null, string[]? searchPaths = null)
    {
        try { DotclHost.ResolveDeps(asdPath, manifestOut, rootSourcesOut, targetRid, buildInit, searchPaths); }
        catch (System.IO.FileNotFoundException ex)
        {
            Console.Error.WriteLine(ex.Message);
            Environment.Exit(2);
        }
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
    static void RunCompileProject(string asdPath, string outputPath, string[]? buildInit = null, string[]? searchPaths = null)
    {
        try { DotclHost.CompileProject(asdPath, outputPath, buildInit, searchPaths); }
        catch (System.IO.FileNotFoundException ex)
        {
            Console.Error.WriteLine(ex.Message);
            Environment.Exit(2);
        }
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


    /// <summary>
    /// REPL input reader. On a Unix TTY this reads fd 0 directly via a plain
    /// FileStream instead of Console.In, because .NET's UnixConsoleStream puts
    /// the terminal into raw / non-canonical mode on every read. Raw mode makes
    /// rlwrap think dotcl "asks for single keypresses" (forcing --always-readline)
    /// and leaks raw arrow-key escapes (ESC[A) into the Lisp reader. Reading the
    /// raw fd keeps the kernel's canonical line discipline. Falls back to
    /// Console.In on Windows, for redirected input, or if opening fd 0 fails.
    /// </summary>
    private static System.IO.TextReader OpenReplStdin()
    {
        try
        {
            if (!OperatingSystem.IsWindows() && isatty(0) == 1)
            {
                var fs = new System.IO.FileStream(
                    new Microsoft.Win32.SafeHandles.SafeFileHandle((IntPtr)0, ownsHandle: false),
                    System.IO.FileAccess.Read);
                return new System.IO.StreamReader(
                    fs, new System.Text.UTF8Encoding(false),
                    detectEncodingFromByteOrderMarks: false, bufferSize: 4096);
            }
        }
        catch { /* fall back to Console.In below */ }
        return Console.In;
    }

    static void RunRepl()
    {
        _replMode = true;
        // Read input through OpenReplStdin (raw fd 0 on a Unix TTY) rather than
        // Console.In / Console.ReadLine, which would route through .NET's Unix
        // console driver and switch the tty into raw mode — breaking rlwrap and
        // leaking arrow-key escapes. Only do this for the default read path; a
        // raw readline hook (dotcl-repl) does its own ReadKey-based editing.
        var stdin = Startup.ReadlineHook == null ? OpenReplStdin() : Console.In;
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

            string? line;
            if (Startup.ReadlineHook != null)
            {
                try
                {
                    var result = Startup.ReadlineHook.Invoke(new LispObject[] { new LispString(prompt) });
                    line = result is Nil ? null : (result as LispString)?.Value ?? result.ToString();
                }
                catch (Exception ex)
                {
                    // The line editor failed (e.g. --readline forced on a
                    // non-console where Console.ReadKey/CursorLeft are invalid).
                    // Disable it and fall back to plain line input for the rest
                    // of the session instead of crashing the REPL.
                    Console.Error.WriteLine(
                        $"; readline failed ({ex.Message}); falling back to basic line input");
                    Startup.ReadlineHook = null;
                    stdin = OpenReplStdin();
                    continue;
                }
            }
            else
            {
                Console.Write(prompt);
                line = stdin.ReadLine();
            }

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

    [System.Runtime.InteropServices.DllImport("libc", SetLastError = true)]
    private static extern int isatty(int fd);

    // Track whether we already restored, so multiple exit paths don't double-write.
    private static int _terminalRestored;

    // Roots the PosixSignalRegistration handles for the process lifetime; without
    // a live reference they would be GC'd and the signal handlers unregistered.
    private static readonly System.Collections.Generic.List<System.Runtime.InteropServices.PosixSignalRegistration>
        _signalRegistrations = new();

    /// <summary>
    /// Emit the terminfo rmkx reset (DECCKM reset ESC[?1l + DECKPNM ESC>) so the
    /// terminal leaves the "application" keypad / cursor-key mode that .NET's
    /// Console driver enters on interactive read. Only acts when stdout is a TTY.
    /// </summary>
    private static void RestoreTerminal()
    {
        if (System.Threading.Interlocked.Exchange(ref _terminalRestored, 1) != 0) return;
        try
        {
            // fd 1 = stdout — the same fd .NET wrote the smkx (ESC[?1h ESC=) to.
            // Only act when it is a real terminal, so redirected output stays clean.
            if (isatty(1) != 1) return;
            // ESC[?1l = normal cursor keys, ESC> = numeric keypad.
            var reset = new byte[] { 0x1b, (byte)'[', (byte)'?', (byte)'1', (byte)'l', 0x1b, (byte)'>' };
            using var stdout = Console.OpenStandardOutput();
            stdout.Write(reset, 0, reset.Length);
            stdout.Flush();
        }
        catch
        {
            // Best-effort: ignore if the write fails (closed handle, redirected, etc.).
        }
    }

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
