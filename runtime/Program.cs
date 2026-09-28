using System.Reflection;
using DotCL.Emitter;

namespace DotCL;

class Program
{
    static void Main(string[] args)
    {
        StartJitProfile();

        // Run on a thread with a larger stack to handle deeply nested code
        // (e.g., SBCL cross-compiler macro expansions)
        const int stackSize = 256 * 1024 * 1024; // 256MB
        Exception? threadException = null;
        var thread = new Thread(() => {
            try { MainInner(args); }
            catch (Exception ex) { threadException = ex; }
            finally { MainThread.Shutdown(); }
        }, stackSize);
        // Keep the main thread available as a work queue instead of only joining
        // it: some UI toolkits (macOS AppKit) accept nothing but thread 0, and a
        // main thread cannot be created after the fact. Lisp submits work with
        // DOTCL:CALL-ON-MAIN-THREAD. With nothing submitted this blocks exactly
        // like the Join it replaces. See MainThread.cs.
        MainThread.Install();
        thread.Start();
        MainThread.Pump();
        thread.Join();
        if (threadException != null)
        {
            RestoreConsoleCodePages();
            throw threadException;
        }
    }

    /// <summary>
    /// Parallel background JIT on the 2nd and later runs, from the method-use
    /// profile the previous run left. The first run is a no-op, since there is
    /// no profile yet. After that a cold start is roughly 15-35% faster when
    /// dotcl.core is plain IL and has to be JIT compiled, as in a development
    /// tree, but only about 2-5% faster in the shipped ReadyToRun layout, where
    /// most of the startup code is already precompiled. The profile goes in the
    /// user's cache, keyed by this executable: see JitProfile for why neither
    /// half of that is free to change.
    ///
    /// Best effort from end to end. Everything it touches is optional, the
    /// worst outcome of failing is the start this was meant to shorten, and it
    /// runs before there is any way to report an error, so nothing here may
    /// throw or print. DOTCL_NO_JIT_PROFILE=1 switches it off.
    /// </summary>
    static void StartJitProfile()
    {
        try
        {
            if (!JitProfile.Recording()) return;
            var dir = JitProfile.Root();
            Directory.CreateDirectory(dir);
            System.Runtime.ProfileOptimization.SetProfileRoot(dir);
            System.Runtime.ProfileOptimization.StartProfile(JitProfile.Name());
        }
        catch { /* a slower start is the entire cost of getting this wrong */ }
    }

    // Global options that take a value, so a scan for the subcommand has to step
    // over two tokens rather than one. Kept next to the flag list below because
    // the two are read together and drift apart silently otherwise.
    static readonly string[] _globalsWithValue =
        { "--core", "--asm", "--load", "--eval", "--asd-search-path", "--completion" };
    static readonly string[] _globalFlags =
        { "--no-init", "--readline", "--no-readline", "--help", "--version" };

    /// <summary>Index of the first argument that is not a global option (nor the
    /// value of one), or -1 if there is none. Stops at anything unrecognised, so
    /// an unknown flag is left to the ordinary path exactly as before.</summary>
    static int FirstNonGlobalArg(string[] args)
    {
        for (int i = 0; i < args.Length; i++)
        {
            var a = args[i];
            if (_globalsWithValue.Contains(a)) { i++; continue; }   // skip its value too
            if (_globalFlags.Contains(a)) continue;
            if (a.StartsWith("--color=")) continue;                 // one token, value included
            if (a.StartsWith("--")) return -1;                      // unknown flag: leave it alone
            return i;
        }
        return -1;
    }

    // The positional script meaning standard input. The bare-token rule can
    // never produce it (a bare token does not start with '-'), so a file of
    // that name has to be spelled `./-` to be run instead.
    const string StdinScript = "-";

    // True when the REPL is active: CancelKeyPress delivers interrupt instead of killing process.
    // volatile: read from the SIGINT signal handler, which runs on another thread.
    static volatile bool _replMode = false;

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
        //
        // On Windows the two setters change the code pages of the console itself
        // (SetConsoleCP / SetConsoleOutputCP), which outlives this process and is
        // shared with the shell that started it. Remember what was there and put
        // it back on every way out, so a later program in the same window that
        // expects, say, 932 or 437 does not find 65001.
        if (OperatingSystem.IsWindows()) SaveConsoleCodePages();
        var utf8NoBom = new System.Text.UTF8Encoding(encoderShouldEmitUTF8Identifier: false);
        Console.InputEncoding  = utf8NoBom;
        Console.OutputEncoding = utf8NoBom;
        Console.SetIn(new StdinReader(
            new System.IO.StreamReader(Console.OpenStandardInput(), utf8NoBom, detectEncodingFromByteOrderMarks: false, bufferSize: 4096, leaveOpen: true),
            consoleEndMark: OperatingSystem.IsWindows() && !Console.IsInputRedirected));

        // Enable ANSI VT100 escape sequences on Windows legacy conhost (cmd.exe).
        // Modern .NET enables this implicitly via Console.Out, but redirected
        // stdout / certain hosts skip it. We call SetConsoleMode explicitly so
        // (format t "~C[31mRED~C[0m" #\Esc #\Esc) renders colored on cmd.exe.
        // Opt-out via DOTCL_NO_VT=1.
        if (OperatingSystem.IsWindows() &&
            Environment.GetEnvironmentVariable("DOTCL_NO_VT") != "1")
        {
            (_vtOut, _vtErr) = EnableWindowsVtMode();
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
            // else: default behavior: process exits with SIGINT. That exit
            // may skip ProcessExit, so the console code pages are restored here.
            if (!args2.Cancel) RestoreConsoleCodePages();
        };

        // On Unix, Console.CancelKeyPress never fires: the REPL reads raw fd 0 and
        // deliberately avoids .NET's Unix console driver (which is where CancelKeyPress
        // is wired), so Ctrl-C would otherwise terminate the process outright. Deliver
        // the interrupt through a PosixSignalRegistration for SIGINT instead; the same
        // driver-independent mechanism used for SIGTERM/SIGHUP/SIGQUIT below. Windows
        // keeps using CancelKeyPress above; this path is Unix-only to avoid double
        // handling. In non-REPL (script) mode, leave Cancel=false so the default action
        // still terminates the process: unchanged from before.
        if (!OperatingSystem.IsWindows())
        {
            try
            {
                _signalRegistrations.Add(
                    System.Runtime.InteropServices.PosixSignalRegistration.Create(
                        System.Runtime.InteropServices.PosixSignal.SIGINT,
                        ctx =>
                        {
                            if (_replMode)
                            {
                                ctx.Cancel = true;   // don't terminate; interrupt instead
                                ConditionSystem.RequestInterrupt();
                            }
                        }));
            }
            catch { /* SIGINT not registerable here; best-effort */ }
        }

        // Restore the terminal on exit (Unix). .NET's Console driver switches
        // the terminal into "application" keypad / cursor-key mode (terminfo
        // smkx: ESC[?1h ESC=) the first time it reads interactively, but does
        // not reliably emit the matching reset (rmkx: ESC[?1l ESC>) when the
        // process exits via Environment.Exit / EOF / signal. The terminal is
        // then left in application mode, so arrow keys send ESC O A instead of
        // ESC [ A: this conflicts with rlwrap's readline (garbled / "16R"
        // cursor-report fragments) and requires `stty sane` after a crash.
        // We emit the reset ourselves on ProcessExit. Best-effort, TTY only,
        // opt-out via DOTCL_NO_TTY_RESTORE=1.
        if (!OperatingSystem.IsWindows() &&
            Environment.GetEnvironmentVariable("DOTCL_NO_TTY_RESTORE") != "1")
        {
            AppDomain.CurrentDomain.ProcessExit += (_, _) => RestoreTerminal();

            // ProcessExit does NOT fire when the process is killed by a signal
            // (SIGTERM from `kill`, SIGHUP on terminal close, SIGQUIT): the
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
                catch { /* signal not supported on this platform; best-effort */ }
            }
        }

        // --help / --version: handled before core loading for fast response.
        // Skipped when a user FASL is present: then this executable IS an
        // application and every argument belongs to it (see HasUserFasl).
        bool hasUserFasl = HasUserFasl();

        if (!hasUserFasl && args.Any(a => a == "--help") && FirstNonGlobalArg(args) is var _packHelpAt
            && _packHelpAt >= 0 && args[_packHelpAt] == "pack")
        {
            Console.WriteLine(@"dotcl pack --system <name> --id <pkgid> --command <cmd>
           -o <dir> --from <dir> [options...]

Package an ASDF system as a .NET tool nupkg, built by restamping the
published dotcl runtime packages in --from with your system's fasl.

Required:
  --system <name>              ASDF system to compile
  --id <pkgid>                 NuGet id of the produced tool
  --command <cmd>               Command your users type after install
  -o, --output <dir>           Output directory for the nupkg(s)
  --from <dir>                 Directory holding the dotcl runtime packages
                               to build on (dotcl.<version>.nupkg plus one
                               dotcl.<rid>.<version>.nupkg per target
                               platform)

Optional:
  --version <ver>               Tool version. Default: the .asd's :version
  --dotcl-version <ver>         Which dotcl version in --from to build on.
                               Default: inferred, when --from holds exactly
                               one dotcl.<version>.nupkg. Required when it
                               holds more than one
  --toplevel <fn>                Exported function to call at startup.
                               Default: the .asd's :entry-point, if any;
                               with neither, pack warns and the tool only
                               loads the system
  --asd-search-path <dir>      Append <dir> to asdf:*central-registry* so
                               the system (and, repeated, its dependencies)
                               resolve from there. One directory, not
                               searched recursively (repeatable). For a
                               whole dependency tree, use CL_SOURCE_REGISTRY
                               or a source-registry.conf.d file instead --
                               see docs/dotcl-pack.md
  --rids <csv>                  Target platforms. Default:
                               win-x64,win-arm64,linux-x64,linux-arm64,
                               osx-x64,osx-arm64,any
  --bundle <dir>                 Extra files shipped next to the executable
  --prelude <file>               Source file loaded before the closure is
                               collected and compiled ahead of it
                               (repeatable)
  --r2r                          Also crossgen2-compile the fasl per RID
  --dry-run                      Print the planned fasl and packages
                               without producing them (does not compile)
  --description <text>          Override nuspec fields the .asd would
  --project-url <url>            otherwise supply
  --repository <url[#commit]>
  --readme <file>
  --tags <csv>
  --authors <text>
  --copyright <text>

Example:
  dotcl pack --system hello --id hello-tool --command hello \
             --version 0.1.0 -o out/ --from ./dotcl-pkgs/ \
             --rids win-arm64 --toplevel hello:main --asd-search-path .

See docs/dotcl-pack.md for the full walkthrough.");
            return;
        }
        if (!hasUserFasl && args.Any(a => a == "--help"))
        {
            Console.WriteLine(@"dotcl [options] [script-file [arguments...]]

Usage:
  dotcl repl                   Start a REPL
  dotcl file.lisp [args...]    Run file.lisp as a script, then exit
  dotcl - [args...]            Run standard input as a script, then exit
                               (also plain `dotcl` when stdin is not a
                               terminal and no program is given)
  dotcl --load file.lisp       Load file.lisp, then exit
  dotcl --eval ""(+ 1 2)""       Evaluate an expression, then exit

Options:
  --help                       Display this message
  --version                    Display version information
  --core <file>                Use specified core file
  --load <file>                Load a file, then exit (add `repl` to stay)
  --eval <expr>                Evaluate an expression
  --no-init                    Skip loading the user init file (REPL/--eval/--load)
  --readline / --no-readline   Force the line-editing REPL on / off
                               (default: on when input and output are a
                               terminal). TERM=dumb turns it off in every
                               case
  --color=<when>               Colour the REPL's prompt, values, warnings
                               and errors: auto (default: only on a
                               terminal), always or never. NO_COLOR and
                               TERM=dumb turn it off in every case
  --completion <shell>         Emit a shell completion script for
                               pwsh / bash / zsh / fish
  --asd-search-path <dir>      Append <dir> to asdf:*central-registry*
                               after asdf loads (repeatable)

Subcommands:
  repl                         Start a REPL, or stay in one when --load /
                               --eval are done. Must come before a script file:
                               after one it is that script's argument
  clean                        Remove dotcl's caches: the shared ASDF compile
                               cache (one directory per dotcl build under
                               <cache-home>/common-lisp/) and the JIT startup
                               profiles (<cache-home>/dotcl/jit/). A project's
                               own obj/ cache goes with `dotnet clean`
                               [--dry-run] [--verbose]
                               [--keep-current: compile cache only]
  build <asd> --output <fasl>  Compile an ASDF system to a fasl
  pack --system <name> ...     Package an ASDF system as a dotnet tool nupkg,
                               built by restamping the published dotcl tool
                               packages in --from
                               (--id <pkgid> --command <cmd>
                               -o <dir> --from <dir>
                               [--version <ver>; default: the .asd's]
                               [--dotcl-version <ver>]
                               [--toplevel <fn>] [--bundle <dir>]
                               [--prelude <file>] [--rids <csv>] [--r2r]
                               [--dry-run]
                               [--description <text>] [--project-url <url>]
                               [--repository <url[#commit]>] [--readme <file>]
                               [--tags <csv>] [--authors <text>]
                               [--copyright <text>])

Example:
  dotcl hello.lisp arg1 arg2
  dotcl --eval ""(format t \""hi~%\"")""
  dotcl build MyApp.asd --output obj/MyApp.fasl
  dotcl pack --system myapp/exe --id myapp --command myapp \
             --version 0.1.0 -o out/ --from ~/dotcl-nupkgs/

Build-tooling flags (--resolve-deps / --compile-project / etc.) are internal
and invoked by the MSBuild integration; they are intentionally omitted here.");
            return;
        }
        // `pack` takes --version as the produced package's version, so don't let
        // the global "print dotcl's version" handler swallow it in that subcommand.
        // (Guard on presence, not args[0]: leading globals like --core can precede
        // the subcommand token.)
        if (!hasUserFasl && args.Any(a => a == "--version")
            && !args.Contains("pack"))
        {
            var version = typeof(Program).Assembly
                .GetCustomAttribute<System.Reflection.AssemblyInformationalVersionAttribute>()
                ?.InformationalVersion ?? "unknown";
            Console.WriteLine($"dotcl {version}");
            return;
        }

        // `clean` subcommand: empty the caches dotcl writes, the shared ASDF
        // compile cache and the JIT startup profiles. Handled here,
        // before the core loads, because a broken cache is exactly what stops the
        // core from loading -- cleaning must not depend on what it is fixing.
        //
        // Found by skipping the global options rather than by looking at args[0]:
        // with a global in front, `dotcl --core x.core clean` used to fall through
        // to the ordinary path, where `clean` is not a flag, so it was taken as a
        // script name and reported as "LOAD: file not found: .../clean". --help,
        // --version and pack are all position-independent already; this is the
        // odd one out. Scanning stops at the first token that is neither a known
        // global nor its value, so an unknown flag leaves the behaviour exactly
        // as it was (see the separate question of rejecting unknown flags).
        int cleanIdx = FirstNonGlobalArg(args);
        if (!hasUserFasl && cleanIdx >= 0 && args[cleanIdx] == "clean")
        {
            args = args.Skip(cleanIdx).ToArray();
            bool cleanDryRun = args.Contains("--dry-run");
            bool cleanVerbose = args.Contains("--verbose");
            string? keepPrefix = null;
            if (args.Contains("--keep-current"))
            {
                var v = typeof(Program).Assembly
                    .GetCustomAttribute<System.Reflection.AssemblyInformationalVersionAttribute>()
                    ?.InformationalVersion;
                if (v != null) keepPrefix = $"dotcl-{v}-";
            }
            var unknown = args.Skip(1).FirstOrDefault(
                a => a != "--dry-run" && a != "--verbose" && a != "--keep-current");
            if (unknown != null)
            {
                Console.Error.WriteLine($"dotcl clean: unknown option '{unknown}'");
                Console.Error.WriteLine("  usage: dotcl clean [--dry-run] [--keep-current] [--verbose]");
                Environment.Exit(2);
            }
            Environment.Exit(FaslCache.Run(cleanDryRun, keepPrefix, cleanVerbose, Console.Out));
            return;
        }

        // --completion <shell>: emit shell completion script and exit. Handled
        // before core loading so it stays fast (no Lisp init). Completions
        // describe dotcl's own CLI, so an app built on it must not answer them.
        if (!hasUserFasl)
        {
            for (int ci = 0; ci < args.Length; ci++)
            {
                if (args[ci] == "--completion")
                {
                    var shell = ci + 1 < args.Length ? args[ci + 1] : "pwsh";
                    Environment.Exit(CliCompletion.Emit(shell));
                    return;
                }
            }
        }

        // --asm: legacy behavior (run .sil directly, load additional scripts, exit)
        // Used by test-a2 and Makefile targets. No REPL, no core auto-discovery.
        //
        // Accepted anywhere in the command line, not only first. It used to be
        // recognized at args[0] alone, so putting any other flag ahead of it fell
        // through to the ordinary path: where --asm is not a known flag, so the
        // .sil that followed it was LOADed as source and died with "package
        // COMMON-LISP is locked; cannot redefine EQ", a message that says nothing
        // about argument order.
        //
        // A subcommand (`repl`, `build`, `pack`) is not this path's business. The
        // loop below knows --eval, --load and --asd-search-path and LOADs every
        // other token as a file, so `--asm x.sil repl` used to die with
        // "LOAD: file not found: .../repl" while `--core x.sil repl` worked.
        // With a subcommand present, --asm is read as --core and the invocation
        // goes to the ordinary path, which parses subcommands; RunCore takes a
        // .sil as readily as a .fasl, so the core is the same either way. The
        // subcommand is looked for where one can be (FirstNonGlobalArg, the
        // same scan the ordinary path uses), so a script argument that happens
        // to be spelled `repl` is still that script's argument.
        var asmArgs = HoistAsmFlag(args);
        if (!hasUserFasl && asmArgs != null && AsmSubcommand(asmArgs) != null)
        {
            asmArgs[0] = "--core";
            args = asmArgs;
            asmArgs = null;
        }
        if (!hasUserFasl && asmArgs != null)
        {
            args = asmArgs;
            if (!File.Exists(args[1]))
            {
                Console.Error.WriteLine($"Error: core file not found: {args[1]}");
                Environment.Exit(2);
            }
            try
            {
                RunCore(args[1]);
                // A bare file among the arguments makes this a script run, the
                // run `--core <core> file.lisp` is, and it starts with the same
                // *debugger-hook*: an unhandled error prints and exits 1. With
                // only --eval / --load / --asd-search-path it stays NIL, as those
                // flags alone leave it on the ordinary path. Installed before any
                // argument runs, as the ordinary path installs it before the
                // --load/--eval that precede the script.
                if (AsmHasScriptFile(args))
                    InstallScriptDebuggerHook();
                for (int i = 2; i < args.Length; i++)
                {
                    if (args[i] == "--eval" && i + 1 < args.Length)
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
                    else if (args[i] == "--asd-search-path" && i + 1 < args.Length)
                    {
                        // Same flag the ordinary path takes: the directories are
                        // pushed onto asdf:*central-registry* once asdf loads.
                        // Without this case it was LOADed as if it were a file.
                        Runtime.UserAsdSearchPaths.Add(args[++i]);
                    }
                    else
                        Runtime.Load(new LispObject[] { new LispString(args[i]) });
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


        // New-style invocation: auto-discover (or --core override) + optional scripts,
        // and the REPL when the `repl` subcommand asks for it
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

        // `build` subcommand: the user-facing entry point for ASDF project
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
        //       --target-rid prefers <dir>/<name>.fasl.r2r-<rid> when present.
        // The flags below are build-internal and intentionally absent from
        // --help / completion.
        bool buildMode = !hasUserFasl && rest.Count > 0 && rest[0] == "build";
        string? buildAsd = null;
        string? buildOutput = null;
        bool buildResolveDeps = false;
        bool buildDebugInfo = false;
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
                else if (a == "--debug-info") buildDebugInfo = true;
                else if (a == "--manifest-out" && i + 1 < rest.Count) buildManifestOut = rest[++i];
                else if (a == "--root-sources-out" && i + 1 < rest.Count) buildRootSourcesOut = rest[++i];
                else if (a == "--target-rid" && i + 1 < rest.Count) buildTargetRid = rest[++i];
                else if (a == "--build-init" && i + 1 < rest.Count) buildInit.Add(rest[++i]);
                else if (a == "--asd-search-path" && i + 1 < rest.Count) buildSearchPaths.Add(rest[++i]);
                else if (!a.StartsWith('-') && buildAsd == null) buildAsd = a;
            }
        }

        // `pack` subcommand: package an ASDF system as a dotnet-tool nupkg
        //   dotcl pack --system <name> --id <pkgid> --command <cmd>
        //              -o <dir> --from <dotcl-nupkg-dir>
        //              [--version <ver>, else the .asd's :version]
        //              [--dotcl-version <ver>] [--toplevel <fn>]
        //              [--bundle <dir>] [--prelude <file>] [--rids <csv>]
        //              [--r2r] [--no-android] [--dry-run]
        //              [--description <text>] [--project-url <url>]
        //              [--repository <url[#commit]>] [--readme <file>]
        //              [--tags <csv>] [--authors <text>] [--copyright <text>]
        // Compile the system and its whole closure into a single fasl, then
        // restamp the published dotcl tool packages in --from into the app's
        // own base pointer + per-RID packages with that fasl injected.
        bool packMode = !hasUserFasl && rest.Count > 0 && rest[0] == "pack";
        var pack = new PackOptions();
        if (packMode)
        {
            rest.RemoveAt(0);
            for (int i = 0; i < rest.Count; i++)
            {
                var a = rest[i];
                if (a == "--system" && i + 1 < rest.Count) pack.System = rest[++i];
                else if (a == "--id" && i + 1 < rest.Count) pack.Id = rest[++i];
                else if (a == "--command" && i + 1 < rest.Count) pack.Command = rest[++i];
                else if (a == "--version" && i + 1 < rest.Count) pack.Version = rest[++i];
                else if ((a == "-o" || a == "--output") && i + 1 < rest.Count) pack.Output = rest[++i];
                else if (a == "--toplevel" && i + 1 < rest.Count) pack.Toplevel = rest[++i];
                else if (a == "--bundle" && i + 1 < rest.Count) pack.Bundle = rest[++i];
                else if (a == "--rids" && i + 1 < rest.Count) pack.Rids = rest[++i];
                else if (a == "--from" && i + 1 < rest.Count) pack.From = rest[++i];
                else if (a == "--dotcl-version" && i + 1 < rest.Count) pack.DotclVersion = rest[++i];
                else if (a == "--no-android") pack.NoAndroid = true;
                else if (a == "--r2r") pack.ReadyToRun = true;
                else if (a == "--dry-run") pack.DryRun = true;
                else if (a == "--asd-search-path" && i + 1 < rest.Count) pack.SearchPaths.Add(rest[++i]);
                else if (a == "--prelude" && i + 1 < rest.Count) pack.Prelude.Add(rest[++i]);
                else if (a == "--description" && i + 1 < rest.Count) pack.Description = rest[++i];
                else if (a == "--project-url" && i + 1 < rest.Count) pack.ProjectUrl = rest[++i];
                else if (a == "--repository" && i + 1 < rest.Count) pack.Repository = rest[++i];
                else if (a == "--readme" && i + 1 < rest.Count) pack.Readme = rest[++i];
                else if (a == "--tags" && i + 1 < rest.Count) pack.Tags = rest[++i];
                else if (a == "--authors" && i + 1 < rest.Count) pack.Authors = rest[++i];
                else if (a == "--copyright" && i + 1 < rest.Count) pack.Copyright = rest[++i];
                else if (!a.StartsWith('-') && pack.Asd == null) pack.Asd = a;
            }
        }

        // Extract the `repl` subcommand -- but only where a subcommand can be, which
        // is the first argument that is not a global option or a global's value. Once
        // a file has been named, everything after it belongs to that script, `repl`
        // included: a script has to be able to receive any argument, and a word that
        // the launcher quietly takes for itself is a trap for whatever invokes it.
        // "Run this and then leave me in the REPL" is spelled `--load file repl`,
        // where the file is dotcl's instruction rather than the program.
        bool explicitRepl = false;
        if (!hasUserFasl)
        {
            int at = FirstNonGlobalArg(rest.ToArray());
            if (at >= 0 && rest[at] == "repl")
            {
                explicitRepl = true;
                rest.RemoveAt(at);
            }
        }

        // Extract --no-init: skip loading the user init file in REPL /
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
        // (dotcl-repl contrib: history, cursor movement). null = auto, which
        // ConfigureLineEditing decides. Only meaningful for the REPL path;
        // ignored in script/eval runs.
        bool? readlinePref = null;
        for (int i = 0; i < rest.Count; i++)
        {
            if (rest[i] == "--readline") { readlinePref = true; rest.RemoveAt(i); break; }
            if (rest[i] == "--no-readline") { readlinePref = false; rest.RemoveAt(i); break; }
        }

        // Extract --color=auto|always|never: colour in the REPL. Only the REPL
        // reads it; a script run is never painted. Only the spelling with `=`:
        // a separate value would be read as a script name by the bare-token rule
        // below, which is a worse surprise than being told how to spell it.
        for (int i = 0; !hasUserFasl && !buildMode && !packMode && i < rest.Count; i++)
        {
            if (rest[i] == "--color" || rest[i].StartsWith("--color="))
            {
                var value = rest[i] == "--color" ? "" : rest[i].Substring("--color=".Length);
                var mode = ReplColor.ParseMode(value);
                if (mode == null)
                {
                    Console.Error.WriteLine(
                        $"dotcl: --color takes auto, always or never, as --color=always (got '{rest[i]}')");
                    Environment.Exit(2);
                }
                _colorMode = mode.Value;
                rest.RemoveAt(i);
                i--;
            }
        }

        // Collect ordered --load/--eval actions. The FIRST bare (non-flag) token
        // is the positional script file; once seen, parsing stops and every
        // remaining token (flag or not) becomes the script's argv: the Unix
        // convention that args after the script belong to the program, not to
        // dotcl. This also stops data args (e.g. an image path) from being
        // mis-loaded as Lisp source.
        //
        // Skipped entirely under `build` and `pack`: those subcommands have
        // already parsed the whole of REST in their own loops above, and their
        // options are not dotcl's. `build` used to survive this loop by
        // accident -- it leaves its own "build" token in REST, so the bare-token
        // rule below stopped the scan at once -- while `pack` removes its token
        // and so handed its first option straight to the unknown-option exit.
        // Neither subcommand ever reaches the script path (both dispatch and
        // return before SCRIPTMODE is read), so nothing this loop computes is
        // wanted on those two paths.
        //
        // Skipped for a packed application as well. Its argv belongs to the
        // program, not to dotcl: TryRunEmbeddedUserFasl below returns before
        // anything this loop computes is read, so the only effect the loop could
        // have there is to reject the application's own options -- a tool packed
        // with `dotcl pack` or save-application could not be given a `--format`
        // or a `--help` of its own.
        var actions = new List<(string kind, string value)>();
        string? positionalScript = null;
        List<string> positionalArgv = new();
        for (int i = 0; !hasUserFasl && !buildMode && !packMode && i < rest.Count; i++)
        {
            if (rest[i] == "--load" || rest[i] == "--eval")
            {
                // A value-taking option with nothing after it used to fall
                // through and be dropped, so `dotcl --eval` did nothing and then
                // looked like an ordinary start.
                if (i + 1 >= rest.Count)
                {
                    Console.Error.WriteLine($"dotcl: {rest[i]} requires an argument");
                    Environment.Exit(2);
                }
                actions.Add((rest[i][2..], rest[i + 1]));
                i++;
            }
            else if (rest[i] == "-")
            {
                // `-` names standard input as the script, as for sbcl --script
                // and python. What follows is the script's argv, as for a file.
                // A file actually called `-` is still reachable as `./-`.
                positionalScript = StdinScript;
                positionalArgv = rest.GetRange(i + 1, rest.Count - (i + 1));
                break;
            }
            else if (!rest[i].StartsWith('-'))
            {
                positionalScript = rest[i];
                positionalArgv = rest.GetRange(i + 1, rest.Count - (i + 1));
                break;
            }
            else
            {
                // Anything else starting with '-' reached here without matching a
                // known option: it was silently discarded, which turned a typo
                // (`--evla '(princ 1)'`) into a run that did nothing. Arguments
                // meant for a script are not affected -- the loop stops at the
                // script name above, and everything after it is the script's.
                // Subcommand options are not affected either: the loop header
                // declines to run at all under build/pack, so the MSBuild
                // integration is out of scope here.
                Console.Error.WriteLine($"dotcl: unknown option '{rest[i]}'");
                Console.Error.WriteLine("  dotcl --help for usage");
                Environment.Exit(2);
            }
        }
        // No program named anywhere (no script, no --load / --eval, no `repl`)
        // and standard input is not a terminal: the program is on standard
        // input, as with `python` and `sbcl --script`. Standard input is taken
        // as the program only when nothing else is, so `... | dotcl --eval`
        // still reads it as data and `... | dotcl repl` is still a REPL. From a
        // terminal nothing changes: the usage message below and exit 2.
        if (!hasUserFasl && !buildMode && !packMode && !explicitRepl
            && positionalScript == null && actions.Count == 0
            && Console.IsInputRedirected)
            positionalScript = StdinScript;
        bool scriptMode = positionalScript != null;
        var scripts = actions; // pre-script --load/--eval actions

        ProfileMark("arg-parse");

        // Find and boot core (compiler + stdlib)
        var corePath = coreOverride ?? FindCore();
        ProfileMark("FindCore");
        if (corePath != null)
        {
            // A core path the user typed can simply not exist. Say that, rather
            // than letting File.OpenRead's exception out of Main as an unhandled
            // FileNotFoundException with a stack trace through Program.Main.
            if (!File.Exists(corePath))
            {
                Console.Error.WriteLine($"Error: core file not found: {corePath}");
                Environment.Exit(2);
            }
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
        // runs: the resource is only present in save-application-built exes.
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
                    RunCompileProject(buildAsd, buildOutput, buildInitArr, searchPathArr, buildDebugInfo);
                else
                {
                    Console.Error.WriteLine("build: requires --output <fasl> or --resolve-deps");
                    Environment.Exit(2);
                }
            }
            // Build diagnostics in MSBuild canonical format so IDEs surface them in
            // the Error List with click-to-navigate (dotcl/dotcl#48).
            catch (LispSourceException lse)
            {
                Console.Error.WriteLine(lse.FormatMsBuildDiagnostic());
                Environment.Exit(1);
            }
            catch (LispErrorException lee)
            {
                // Error without a located form (e.g. during setup, dep load, or the
                // module link/save phase). Use the asd as origin so it is still
                // canonical (clickable to the project) rather than an unhandled stack.
                Console.Error.WriteLine($"{buildAsd} : error DOTCL: {lee.Message}");
                Environment.Exit(1);
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine($"{buildAsd} : error DOTCL: {ex.Message}");
                if (Startup.DebugStacktrace) Console.Error.WriteLine(ex.StackTrace);
                Environment.Exit(1);
            }
            return;
        }

        // `pack` subcommand dispatch: build the user fasl, then restamp.
        if (packMode)
        {
            Environment.Exit(RunPack(pack));
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
            InstallScriptDebuggerHook();

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

                // Standard input goes through LOAD as a stream: the same reader
                // and evaluation as a script file, reading the process's stdin
                // (Console.In, which *standard-input* wraps). *load-pathname*
                // stays NIL, as LOAD specifies for a stream.
                Runtime.Load(new LispObject[] {
                    positionalScript == StdinScript
                        ? Startup.StandardInput
                        : new LispString(positionalScript!) });
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
            // Reachable only as `dotcl repl file.lisp`, where the subcommand comes
            // before the file: after the file, `repl` is one of the file's arguments.
            // Returning without honouring it would drop the word on the floor.
            if (explicitRepl)
                RunRepl(readlinePref);
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
            // Same treatment as the script-file path below: report and exit 1.
            // Without this an error with no source location, a --load naming a
            // file that is not there is the everyday one, left Main as an
            // unhandled exception, printing a .NET stack trace and exiting 127
            // where the identical mistake in a positional script exits 1 with a
            // one-line message.
            catch (Exception ex)
            {
                Console.Error.WriteLine($"Error: {ex.Message}");
                if (Startup.DebugStacktrace) Console.Error.WriteLine(ex.StackTrace);
                Environment.Exit(1);
            }
        }

        ProfileMark("actions");

        // REPL only when asked for it. The `repl` subcommand exists precisely so
        // that entering the REPL is a thing you say, not a thing that happens
        // when nothing else matched -- an invocation whose arguments were not
        // understood should not look like a successful start. Dropping into the
        // REPL on an empty action list is what made a mistyped flag
        // (`dotcl --evla '(...)'`) read as a normal REPL start.
        if (!explicitRepl && scripts.Count == 0 && positionalScript == null)
        {
            Console.Error.WriteLine("dotcl: nothing to do.");
            Console.Error.WriteLine("  dotcl repl                 Start a REPL");
            Console.Error.WriteLine("  dotcl <file> [args...]     Run a script");
            Console.Error.WriteLine("  dotcl - [args...]          Run standard input as a script");
            Console.Error.WriteLine("  dotcl --eval \"<form>\"      Evaluate a form");
            Console.Error.WriteLine("  dotcl --help               Full usage");
            Environment.Exit(2);
        }
        if (explicitRepl)
            RunRepl(readlinePref);
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

    /// <summary>The *debugger-hook* a script run starts with: print the condition on
    /// one line and exit 1, since a script has nobody to ask. Shared by the ordinary
    /// script path and the --asm path so that the two cannot drift apart.</summary>
    static void InstallScriptDebuggerHook()
    {
        var hookSym = Startup.Sym("*DEBUGGER-HOOK*");
        DynamicBindings.Set(hookSym, new LispFunction(hookArgs => {
            var cond = hookArgs[0];
            Console.Error.WriteLine(ConditionText.Line(cond));
            Environment.Exit(1);
            return Nil.Instance;
        }, "*SCRIPT-DEBUGGER-HOOK*", 2));
    }

    /// <summary>Whether a hoisted --asm command line (args[0..1] are the --asm pair)
    /// names a file to LOAD as a bare argument, i.e. is a script run rather than
    /// only --eval / --load / --asd-search-path.</summary>
    static bool AsmHasScriptFile(string[] args)
    {
        for (int i = 2; i < args.Length; i++)
        {
            if ((args[i] == "--eval" || args[i] == "--load" || args[i] == "--asd-search-path")
                && i + 1 < args.Length)
            {
                i++;
                continue;
            }
            return true;
        }
        return false;
    }

    /// <summary>If the command line asks for <c>--asm &lt;file&gt;</c> anywhere, return it
    /// rearranged so the pair comes first, with every other argument in its original
    /// order; otherwise null. The legacy --asm path is written against that shape,
    /// and rearranging is what keeps the flag's meaning independent of where the
    /// caller put it.</summary>
    static string[]? HoistAsmFlag(string[] args)
    {
        int at = Array.IndexOf(args, "--asm");
        if (at < 0 || at + 1 >= args.Length) return null;
        if (at == 0) return args;
        var rearranged = new List<string> { "--asm", args[at + 1] };
        for (int i = 0; i < args.Length; i++)
        {
            if (i == at) { i++; continue; }   // skip the pair itself
            rearranged.Add(args[i]);
        }
        return rearranged.ToArray();
    }

    /// <summary>The subcommand the ordinary path would dispatch on, if the command
    /// line has one where a subcommand can be; otherwise null. Only the ones the
    /// ordinary path handles after the --asm path would have returned: `clean`
    /// is dispatched before either.</summary>
    static string? AsmSubcommand(string[] args)
    {
        int at = FirstNonGlobalArg(args);
        if (at < 0) return null;
        return args[at] is "repl" or "build" or "pack" ? args[at] : null;
    }

    /// <summary>Load and execute a compiled core (.sil text or .fasl PE assembly).</summary>
    static void RunCore(string filePath)
    {
        // Remember which core this image was built from, so a .fasl written now
        // can be stamped with it and a .fasl written by a different compiler can
        // be recognised on load. Only the path is kept here; hashing it is
        // deferred to the first fasl save/check so startup pays nothing.
        Startup.CorePath = filePath;
        // Detect PE signature ("MZ") at byte 0: PersistedAssemblyBuilder output.
        // Any other bytes -> treat as SIL text and fall through to Reader.
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

        if (!Reader.TryReadSilCore(source, out var instrList))
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
        try { DotclBuild.ResolveDeps(asdPath, manifestOut, rootSourcesOut, targetRid, buildInit, searchPaths); }
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
    /// Only the root system is compiled: :depends-on'd contribs stay as
    /// pre-built fasls (resolved via --resolve-deps and bundled separately).
    /// MSBuild owns the incremental decision via Inputs/Outputs on the
    /// component source files.
    /// </summary>
    /// <summary>Stage the NuGet layouts each packaged RID needs, one bundle directory
    /// per RID, and answer a lookup from RID to that directory (null when the system
    /// declares no packages, so a pack without any passes the user's --bundle through
    /// untouched).
    ///
    /// Every RID is laid out, not only the one this machine runs: `dotcl pack` builds
    /// a package per platform and a package that carries another platform's assets
    /// carries nothing it can use. Laying one out means a `dotnet build` per RID, so
    /// this is the slow part of packing an app that declares packages -- and it is
    /// the work the shipped program would otherwise have to do on first start, on a
    /// machine that may have neither the SDK nor a network.
    ///
    /// A RID that will not lay out is reported and skipped: a package can have
    /// nothing for a platform, and the answer to that is to ship what does exist and
    /// let the rest resolve on the target, not to refuse to build for it.
    ///
    /// The user's own --bundle is copied into each RID's directory rather than
    /// written into: it is their directory, and a pack should not leave anything
    /// behind in it.</summary>
    static Func<string, string?>? StageNugetBundles(string? userBundle, string faslPath,
                                                    string system, IReadOnlyList<string> rids)
    {
        var pkg = Package.FindPackage("NUGET");
        if (pkg == null) return null;                       // nothing ever required
        var stage = pkg.FindSymbol("STAGE-BUNDLE");
        if (stage.status == SymbolStatus.None || stage.symbol.Function == null) return null;

        var asdf = Package.FindPackage("DOTCL-NUGET-ASDF");
        var resolveForRid = asdf?.FindSymbol("RESOLVE-SYSTEM-FOR-RID");

        var root = Path.Combine(Path.GetDirectoryName(Path.GetFullPath(faslPath))!, "pack-bundle");
        if (Directory.Exists(root)) Directory.Delete(root, recursive: true);

        var map = new Dictionary<string, string>();
        foreach (var rid in rids)
        {
            // "any" is the RID-agnostic package: there is no platform to lay out
            // for, so it ships without a bundle and resolves on the target.
            if (rid == "any") continue;

            if (resolveForRid != null && resolveForRid.Value.status != SymbolStatus.None
                && resolveForRid.Value.symbol.Function is LispFunction rf)
            {
                var failed = rf.Invoke2(new LispString(system), new LispString(rid));
                for (var c = failed; c is Cons cell; c = cell.Cdr)
                    if (cell.Car is Cons f)
                        Console.Error.WriteLine(
                            $"pack: {rid}: {Runtime.PrincToString(f.Car)} not laid out, it "
                            + $"will resolve on the target ({Runtime.PrincToString(f.Cdr)})");
            }

            var dir = Path.Combine(root, rid);
            Directory.CreateDirectory(dir);
            if (userBundle != null) CopyTree(Path.GetFullPath(userBundle), dir);
            var n = ((LispFunction)stage.symbol.Function)
                .Invoke2(new LispString(dir.Replace("\\", "/")), new LispString(rid));
            var count = n is Fixnum fx ? (int)fx.Value : 0;
            if (count == 0 && userBundle == null) { Directory.Delete(dir, recursive: true); continue; }
            Console.WriteLine($"pack: {rid}: bundled {count} NuGet layout(s)");
            map[rid] = dir;
        }
        return map.Count == 0 ? null : (rid => map.TryGetValue(rid, out var d) ? d : null);
    }

    static void CopyTree(string from, string to)
    {
        foreach (var f in Directory.GetFiles(from, "*", SearchOption.AllDirectories))
        {
            var dst = Path.Combine(to, Path.GetRelativePath(from, f));
            Directory.CreateDirectory(Path.GetDirectoryName(dst)!);
            File.Copy(f, dst, overwrite: true);
        }
    }

    static void RunCompileProject(string asdPath, string outputPath, string[]? buildInit = null, string[]? searchPaths = null, bool debugInfo = false)
    {
        try { DotclBuild.CompileProject(asdPath, outputPath, buildInit, searchPaths, debugInfo); }
        catch (System.IO.FileNotFoundException ex)
        {
            Console.Error.WriteLine(ex.Message);
            Environment.Exit(2);
        }
    }

    /// <summary>Options for the `pack` subcommand (dotcl pack ...).</summary>
    sealed class PackOptions
    {
        public string? System;    // ASDF system name (e.g. "myapp/exe")
        public string? Id;        // nupkg / tool package id
        public string? Command;   // tool command name (installed executable)
        public string? Version;   // produced package version
        public string? Output;    // output directory (-o / --output)
        public string? Toplevel;  // optional entry fn to synthesize a launcher
        public string? Bundle;    // optional extra-files bundle dir (DotclAppBundle)
        public string? Rids;      // optional RID list override (comma/semicolon)
        public string? Asd;       // optional explicit .asd path (else from System)
        public string? From;      // dir holding the published dotcl.* nupkgs to restamp
        public string? DotclVersion; // which dotcl version in --from (else inferred)
        public bool NoAndroid = true;   // desktop RIDs only (release default)
        public bool ReadyToRun;         // also compile the fasl's R2R sibling per RID
        public bool DryRun;
        public readonly List<string> SearchPaths = new();
        public readonly List<string> Prelude = new();  // sources compiled ahead of the closure
        // nuspec metadata overrides (else inherited from the dotcl packages).
        public string? Description;
        public string? ProjectUrl;
        public string? Repository; // url[#commit]
        public string? Readme;     // path to a README file to embed
        public string? Tags;       // comma/semicolon/space separated
        public string? Authors;
        public string? Copyright;
    }

    /// <summary>
    /// `pack` subcommand: turn an ASDF system into a set of dotnet-tool nupkgs.
    /// Step 1 compiles the system and its dependencies into a single fasl, one
    /// source at a time in dependency order;
    /// step 2 restamps the published dotcl tool packages found in --from into
    /// the app's own id / version / command with that fasl injected (see
    /// PackRestamp). Returns a process exit code (0 ok, 1 failure, 2 usage).
    /// </summary>
    static int RunPack(PackOptions o)
    {
        // The .asd already describes the system; read it so a pack does not have
        // to restate on the command line what the system definition says. CLI
        // flags win, .asd fills the gaps, and anything still unset is dropped
        // rather than inherited from dotcl (see PackRestamp.Meta). This happens
        // before the required-argument check because the version is one of the
        // gaps a .asd can fill, so whether --version is missing depends on it.
        var asdSearch = Runtime.UserAsdSearchPaths.Count > 0
            ? Runtime.UserAsdSearchPaths.ToArray() : null;
        string? asdError = null;
        var asd = string.IsNullOrEmpty(o.System)
            ? null : DotclBuild.ReadSystemMeta(o.System!, asdSearch, out asdError);
        // A system that cannot be read stops the pack here, with the reason.
        // Going on would report the first gap the .asd was meant to fill --
        // "missing required option(s): --version" -- instead of the cause.
        if (asdError != null)
        {
            Console.Error.WriteLine($"pack: {asdError}");
            return 1;
        }
        // An explicit --version always wins: release paths pass a version of
        // their own choosing and must not start getting the .asd's instead.
        var version = !string.IsNullOrEmpty(o.Version) ? o.Version : asd?.Version;

        var missing = new List<string>();
        if (string.IsNullOrEmpty(o.System)) missing.Add("--system");
        if (string.IsNullOrEmpty(o.Id)) missing.Add("--id");
        if (string.IsNullOrEmpty(o.Command)) missing.Add("--command");
        if (string.IsNullOrEmpty(version)) missing.Add("--version");
        if (string.IsNullOrEmpty(o.Output)) missing.Add("-o/--output");
        if (string.IsNullOrEmpty(o.From)) missing.Add("--from");
        if (missing.Count > 0)
        {
            Console.Error.WriteLine($"pack: missing required option(s): {string.Join(", ", missing)}");
            Console.Error.WriteLine("usage: dotcl pack --system <name> --id <pkgid> --command <cmd> --version <ver> -o <dir> --from <dotcl-nupkg-dir>");
            if (missing.Contains("--version"))
                Console.Error.WriteLine("pack: --version defaults to :version in the .asd; supply one or the other");
            return 2;
        }

        // Fail fast, before building the fasl, if the --from payload predates the
        // loose-fasl loader: a tool restamped from it would silently run a REPL.
        try { PackRestamp.EnsureLoaderCapablePayload(o.DotclVersion ?? PackRestamp.InferDotclVersion(o.From!)); }
        catch (Exception ex) { Console.Error.WriteLine($"pack: {ex.Message}"); return 1; }

        // Default RID set: 6 desktop RIDs + `any` fallback (android excluded).
        var rids = (!string.IsNullOrEmpty(o.Rids)
                ? o.Rids!.Replace(';', ',')
                : "win-x64,win-arm64,linux-x64,linux-arm64,osx-x64,osx-arm64,any")
            .Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
            .ToList();

        string faslPath = System.IO.Path.Combine(o.Output!, "obj",
            o.System!.Replace('/', '_').Replace('\\', '_') + ".fasl");

        if (!string.IsNullOrEmpty(o.Readme) && !File.Exists(o.Readme))
        {
            Console.Error.WriteLine($"pack: --readme file not found: {o.Readme}");
            return 1;
        }

        // Split --repository <url[#commit]> into its parts.
        string? repoUrl = null, repoCommit = null;
        if (!string.IsNullOrEmpty(o.Repository))
        {
            var hash = o.Repository!.IndexOf('#');
            if (hash >= 0)
            {
                repoUrl = o.Repository[..hash];
                repoCommit = o.Repository[(hash + 1)..];
            }
            else repoUrl = o.Repository;
        }

        // A README sitting next to the .asd is the package README by default;
        // the same convention every other packaging tool uses.
        string? readmePath = o.Readme;
        if (readmePath == null && asd?.AsdDirectory != null)
        {
            foreach (var candidate in new[] { "README.md", "readme.md", "README.MD" })
            {
                var p = System.IO.Path.Combine(asd.AsdDirectory, candidate);
                if (File.Exists(p)) { readmePath = p; break; }
            }
        }

        var meta = new PackRestamp.Meta
        {
            Description = o.Description ?? asd?.Description,
            ProjectUrl = o.ProjectUrl ?? asd?.Homepage,
            RepositoryUrl = repoUrl ?? asd?.SourceControlUrl,
            RepositoryCommit = repoCommit,
            ReadmePath = readmePath,
            Tags = o.Tags,
            Authors = o.Authors ?? PackRestamp.AuthorsFromAsd(asd?.Author),
            Copyright = o.Copyright,
            License = asd?.License,
        };

        // Say so when the nuspec's authors are not the .asd's :author verbatim.
        if (o.Authors == null && asd?.Author != null && meta.Authors != asd.Author)
            Console.WriteLine($"pack: authors \"{meta.Authors}\" (the system's :author without mail addresses)");

        // Fail before building the fasl, not after producing a package NuGet
        // would reject or that would misdescribe itself.
        try { PackRestamp.EnsureRequiredMetadata(o.Id!, meta); }
        catch (Exception ex) { Console.Error.WriteLine($"pack: {ex.Message}"); return 1; }

        // What the tool calls at startup: --toplevel, else the system's own
        // :entry-point. With neither, the tool loads the system and exits having
        // called nothing -- right for a system that runs itself at load time,
        // and otherwise a tool that silently does nothing, so say so.
        var toplevel = o.Toplevel;
        if (string.IsNullOrEmpty(toplevel) && !string.IsNullOrEmpty(asd?.EntryPoint))
        {
            toplevel = asd!.EntryPoint;
            Console.WriteLine($"pack: toplevel {toplevel} (the system's :entry-point)");
        }
        else if (string.IsNullOrEmpty(toplevel))
        {
            Console.Error.WriteLine(
                $"pack: warning: no --toplevel, and system {o.System} declares no :entry-point; "
                + "the tool will load the system and exit without calling anything");
        }

        if (o.DryRun)
        {
            try
            {
                var planned = PackRestamp.Run(o.From!, o.DotclVersion, o.Id!, o.Command!,
                                              version!, faslPath, o.Bundle, rids,
                                              o.Output!, meta, dryRun: true);
                Console.WriteLine($"pack: would build user fasl  {faslPath}");
                foreach (var p in planned) Console.WriteLine($"pack: would write  {p}");
                return 0;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine($"pack: {ex.Message}");
                return 1;
            }
        }

        // Step 1: build the self-contained user FASL from the ASDF system. The
        // asd is located via the source registry + any --asd-search-path dirs
        // (extracted globally before subcommand parsing, so read them here).
        var searchPaths = Runtime.UserAsdSearchPaths.Count > 0
            ? Runtime.UserAsdSearchPaths.ToArray() : null;
        try
        {
            Console.Error.WriteLine($"[pack] system '{o.System}' -> {faslPath}");
            DotclBuild.PackFasl(o.System!, faslPath, toplevel, null, searchPaths,
                                o.Prelude.Count > 0 ? o.Prelude.ToArray() : null);
        }
        catch (LispSourceException lse)
        {
            Console.Error.WriteLine(lse.FormatMsBuildDiagnostic());
            return 1;
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"pack: fasl build failed: {ex.Message}");
            if (Startup.DebugStacktrace) Console.Error.WriteLine(ex.StackTrace);
            return 1;
        }
        Console.WriteLine($"pack: built user fasl  {faslPath}");

        // Step 1b: carry the NuGet packages each packaged platform needs. Building
        // the fasl ran the (:nuget ...) declarations -- they are compiled into the
        // unit as NUGET:REQUIRE calls -- so this process already holds a laid-out
        // copy for the RID it packs on; the other RIDs are laid out here. Copying
        // them beside the executable is what lets the packaged app start where
        // there is no .NET SDK and no network: NUGET:BUNDLED-ROOT is consulted
        // before the cache and before `dotnet build`.
        Func<string, string?>? bundleForRid = null;
        try
        {
            bundleForRid = StageNugetBundles(o.Bundle, faslPath, o.System!, rids);
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"pack: staging NuGet layouts failed: {ex.Message}");
            return 1;
        }

        // Step 1c: the ReadyToRun sibling of the fasl, one per RID. The launcher
        // takes it in place of the fasl, which is where the packed application's
        // own code stops being JITted at every start. Opt-in: it costs a crossgen2
        // run per RID and roughly doubles the package, and a package without it is
        // correct, only slower.
        Dictionary<string, string>? r2rForRid = null;
        if (o.ReadyToRun)
        {
            r2rForRid = new Dictionary<string, string>();
            var dotclVersion = o.DotclVersion ?? PackRestamp.InferDotclVersion(o.From!);
            var work = Path.Combine(Path.GetDirectoryName(Path.GetFullPath(faslPath))!, "r2r");
            foreach (var rid in rids)
            {
                var ridPkg = Path.Combine(o.From!, $"dotcl.{rid}.{dotclVersion}.nupkg");
                var sibling = PackR2r.Compile(faslPath, rid, ridPkg, work, out var why);
                if (sibling == null)
                {
                    Console.Error.WriteLine($"pack: no ReadyToRun image for {rid}: {why}");
                    Console.Error.WriteLine(
                        "pack: the package is complete without it; the app starts slower.");
                    continue;
                }
                r2rForRid[rid] = sibling;
                Console.WriteLine($"pack: built ReadyToRun image for {rid}  {sibling}");
            }
        }

        // Step 2: restamp the published dotcl tool packages into this app's.
        List<string> produced;
        try
        {
            produced = PackRestamp.Run(o.From!, o.DotclVersion, o.Id!, o.Command!, version!,
                                       faslPath, o.Bundle, rids, o.Output!, meta, dryRun: false,
                                       bundleForRid, r2rForRid);
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"pack: restamp failed: {ex.Message}");
            if (Startup.DebugStacktrace) Console.Error.WriteLine(ex.StackTrace);
            return 1;
        }

        foreach (var p in produced) Console.WriteLine($"pack: wrote  {p}");
        Console.WriteLine($"pack: id={o.Id} command={o.Command} version={version} "
                          + $"rids={string.Join(",", rids)}");
        return 0;
    }

    /// <summary>
    /// Run a user FASL if one is present, then signal the caller to exit. Two
    /// sources, in order:
    ///   1. An embedded "dotcl.user.fasl" manifest resource: exes produced by
    ///      dotcl:save-application with :executable t.
    ///   2. A loose "dotcl.user.fasl" file next to the executable: the
    ///      `dotcl pack` restamp path drops the fasl into the tool package's
    ///      tools/net10.0/&lt;rid&gt;/ dir (alongside runtime.exe) rather than
    ///      embedding it in the assembly, since a published nupkg is restamped,
    ///      not rebuilt. Either way the FASL is a PE assembly whose
    ///      CompiledModule.ModuleInit is invoked. Returns true if one was found
    ///      and run; false otherwise.
    /// </summary>
    /// <summary>
    /// True when this executable carries a user FASL, in either shape that
    /// TryRunEmbeddedUserFasl accepts: an embedded "dotcl.user.fasl" manifest
    /// resource (save-application :executable t) or a loose file next to the
    /// executable (`dotcl pack`). Such an executable IS the application, not
    /// the dotcl CLI, so every argument belongs to the app: dotcl's own flags
    /// (--help / --version / --completion / --asm) and subcommands (build /
    /// pack / repl) must all stand down. Otherwise an app that legitimately
    /// defines those names: roswell has its own `ros build` and `ros --version`;
    /// would have them silently answered by dotcl instead.
    ///
    /// Must agree with TryRunEmbeddedUserFasl about what counts as present: if
    /// this says no and that says yes, dotcl eats the app's arguments.
    /// </summary>
    static bool HasUserFasl()
    {
        using (var stream = typeof(Program).Assembly
                   .GetManifestResourceStream("dotcl.user.fasl"))
        {
            if (stream != null) return true;
        }
        return System.IO.File.Exists(
            System.IO.Path.Combine(AppContext.BaseDirectory, "dotcl.user.fasl"));
    }

    static bool TryRunEmbeddedUserFasl()
    {
        var selfAsm = typeof(Program).Assembly;
        using (var stream = selfAsm.GetManifestResourceStream("dotcl.user.fasl"))
        {
            if (stream != null)
            {
                using var ms = new MemoryStream();
                stream.CopyTo(ms);
                RunUserFaslBytes(ms.ToArray(), "embedded dotcl.user.fasl");
                return true;
            }
        }

        var loosePath = System.IO.Path.Combine(AppContext.BaseDirectory, "dotcl.user.fasl");
        if (System.IO.File.Exists(loosePath))
        {
            // Prefer the ahead-of-time sibling, exactly as LOAD does. An assembly
            // read into a byte array is not file-backed, so .NET ignores whatever
            // ReadyToRun code it holds -- which meant a packed application's own
            // code was JITted at every start no matter how the image was built,
            // while the runtime and core underneath it were native. That is the
            // case where it costs most: the application is the whole program and
            // it starts once per invocation.
            var r2r = Runtime.FindR2rSibling(System.IO.Path.GetFullPath(loosePath));
            // Counted like any other fasl, so (dotcl:r2r-stats) answers for the
            // application too -- it is the one fasl a packed tool cares about.
            Runtime.FaslsLoaded++;
            if (r2r != null)
            {
                Runtime.R2rFaslsLoaded++;
                RunUserFaslAssembly(System.Reflection.Assembly.LoadFrom(r2r), r2r);
            }
            else
            {
                RunUserFaslBytes(System.IO.File.ReadAllBytes(loosePath), loosePath);
            }
            return true;
        }
        return false;
    }

    /// <summary>Load a user FASL from PE assembly bytes and run it. The embedded
    /// case has no file to load from, so it cannot use a ReadyToRun sibling.</summary>
    static void RunUserFaslBytes(byte[] bytes, string what)
        => RunUserFaslAssembly(System.Reflection.Assembly.Load(bytes), what);

    /// <summary>Invoke a user FASL's CompiledModule.ModuleInit, preserving
    /// *package* across the run. <paramref name="what"/> names the source for
    /// diagnostics.</summary>
    static void RunUserFaslAssembly(System.Reflection.Assembly userAsm, string what)
    {
        var t = userAsm.GetType("CompiledModule")
            ?? throw new InvalidOperationException($"{what}: CompiledModule type not found");
        var mi = t.GetMethod("ModuleInit",
            System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Static)
            ?? throw new InvalidOperationException($"{what}: ModuleInit method not found");

        var packageSym = Startup.Sym("*PACKAGE*");
        var oldPackage = DynamicBindings.Get(packageSym);
        try { mi.Invoke(null, null); }
        finally { DynamicBindings.Set(packageSym, oldPackage); }
    }

    /// <summary>Load a pre-compiled FASL core (PE assembly) and invoke its ModuleInit.</summary>
    static void RunCoreFasl(string filePath)
    {
        var asm = System.Reflection.Assembly.LoadFrom(filePath);
        // The core's generation stamp is read off this assembly, not off the file:
        // see Startup.CoreGeneration.
        Startup.CoreAssembly = asm;
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

    /// <summary>
    /// The key that ends input at the prompt. The line editor reads Ctrl+D
    /// itself on every platform. Without it a Windows console reads Ctrl+D as
    /// an ordinary character and ends input on Ctrl+Z followed by Enter.
    /// </summary>
    private static string ExitKeyHint()
        => Startup.ReadlineHook == null && OperatingSystem.IsWindows() && !Console.IsInputRedirected
            ? "Ctrl+Z then Enter"
            : "Ctrl+D";

    static void RunRepl(bool? readlinePref)
    {
        _replMode = true;
        // The debugger may ask only when a human is at the prompt, which is
        // when standard input is a terminal (on Unix .NET decides this with
        // isatty). `echo ... | dotcl repl` has nobody to answer: there the
        // debugger reports, and the session ends with a failure status below
        // rather than resuming at the next form as if the error were handled.
        Debugger.InteractiveRepl = !Console.IsInputRedirected;
        ConfigureReplColor();
        ConfigureLineEditing(readlinePref);
        // Read input through OpenReplStdin (raw fd 0 on a Unix TTY) rather than
        // Console.In / Console.ReadLine, which would route through .NET's Unix
        // console driver and switch the tty into raw mode: breaking rlwrap and
        // leaking arrow-key escapes. Only do this for the default read path; a
        // raw readline hook (dotcl-repl) does its own ReadKey-based editing.
        var stdin = Startup.ReadlineHook == null ? OpenReplStdin() : Console.In;
        Debugger.Input = stdin;
        Console.WriteLine($"dotcl REPL. {ExitKeyHint()} to exit.");

        var buffer = new System.Text.StringBuilder();

        while (true)
        {
            // The ABORT restart covers the WHOLE iteration -- prompt, read and
            // eval -- not just the evaluation. Ctrl-C arriving while the REPL
            // waits for input used to reach the debugger with an empty restart
            // cluster: "Available restarts:" followed by nothing, and no way
            // back to the prompt except Ctrl-D (dotcl/dotcl issue 61). The
            // interrupt is delivered at a safepoint on this thread, so the
            // cluster established here is the one it sees.
            var abortTag = new object();
            var abortRestart = new LispRestart("ABORT",
                _ => Nil.Instance,
                description: "Return to top level.",
                tag: abortTag);
            RestartClusterStack.PushCluster(new[] { abortRestart });
            try
            {
            var pkg = DynamicBindings.Get(Startup.Sym("*PACKAGE*")) as Package;
            var pkgName = pkg != null
                ? new[] { pkg.Name }.Concat(pkg.Nicknames).OrderBy(n => n.Length).First()
                : "CL-USER";

            // The continuation prompt is as wide as the primary one looks,
            // which is the plain text: the colour takes no columns.
            var plainPrimary = $"{pkgName}> ";
            var primary = ReplColor.ForOut("PROMPT", $"{pkgName}>") + " ";
            var prompt = buffer.Length == 0 ? primary : new string(' ', plainPrimary.Length);

            string? line;
            if (Startup.ReadlineHook != null)
            {
                // An interrupt that arrives while the editor waits for a key is
                // not an interrupt of any computation: it means "drop what I
                // typed". On a Windows console the editor reads Ctrl+C as a key
                // while it waits (TreatControlCAsInput) and drops the form
                // itself. On Unix the key arrives as SIGINT instead, so the
                // condition is caught here rather than entering the debugger,
                // with the same fresh prompt as the result. The editor also
                // signals it for Ctrl+C on a continuation line, whose earlier
                // lines are in this loop's buffer.
                var interruptTag = new object();
                HandlerClusterStack.PushCluster(new[] {
                    new HandlerBinding(Startup.Sym("INTERACTIVE-INTERRUPT"),
                        new LispFunction(hargs => throw new HandlerCaseInvocationException(
                            interruptTag, 0, hargs.Length > 0 ? hargs[0] : Nil.Instance),
                            "%REPL-INPUT-INTERRUPT", -1))
                });
                try
                {
                    var result = Startup.ReadlineHook.Invoke(new LispObject[] { new LispString(prompt) });
                    // A reader that ends in READ-LINE returns two values; an
                    // interpreted one hands them back as a single MvReturn.
                    if (result is MvReturn mv) result = mv.PrimaryValue;
                    line = result is Nil ? null : (result as LispString)?.Value ?? result.ToString();
                }
                catch (HandlerCaseInvocationException hce) when (ReferenceEquals(hce.Tag, interruptTag))
                {
                    Console.WriteLine("^C");
                    buffer.Clear();
                    continue;
                }
                // Transfers of control are not the editor failing. ABORT from a
                // debugger entered inside the editor, a THROW, an interrupt: each
                // goes to whoever established it (the handlers below), and the
                // editor stays on for the next prompt.
                catch (RestartInvocationException) { throw; }
                catch (HandlerCaseInvocationException) { throw; }
                catch (CatchThrowException) { throw; }
                catch (BlockReturnException) { throw; }
                catch (GoException) { throw; }
                catch (LispErrorException ex) when (ex.Condition is LispInteractiveInterrupt) { throw; }
                catch (Exception ex)
                {
                    // The line editor failed (e.g. --readline forced on a
                    // non-console where Console.ReadKey/CursorLeft are invalid).
                    // Disable it and fall back to plain line input for the rest
                    // of the session instead of crashing the REPL.
                    Startup.ReadlineHook = null;
                    Console.Error.WriteLine(
                        $"; readline failed ({ex.Message}); falling back to basic line input"
                        + $" ({ExitKeyHint()} to exit)");
                    stdin = OpenReplStdin();
                    continue;
                }
                finally
                {
                    HandlerClusterStack.PopCluster();
                }
            }
            else
            {
                Console.Write(prompt);
                line = stdin.ReadLine();
                // On a Windows console, Ctrl+C makes the pending ReadLine return
                // null as if the input had ended. Tell the two apart by the
                // interrupt the same key requested, and treat it like the
                // editor's Ctrl+C rather than leaving the REPL.
                if (line == null && ConditionSystem.TakeConsoleInterrupt())
                {
                    Console.WriteLine("^C");
                    buffer.Clear();
                    continue;
                }
            }

            if (line == null)
            {
                // EOF. Drop any pending partial form and exit.
                break;
            }
            if (buffer.Length == 0 && string.IsNullOrWhiteSpace(line)) continue;

            if (buffer.Length > 0) buffer.Append('\n');
            buffer.Append(line);

            // Try to read all forms from the accumulated buffer. Reader signals
            // "more input needed" as a LispError of condition type END-OF-FILE
            // (mid-list, mid-string, after a quote: see
            // Reader.MakeEndOfFileError), which means "keep the buffer and
            // re-prompt with the continuation indent". Anything else is a real
            // syntax error: print and drop the buffer.
            var forms = new List<LispObject>();
            bool incomplete = false;
            bool readError = false;
            try
            {
                var reader = new Reader(new StringReader(buffer.ToString()));
                while (reader.TryRead(out var expr))
                    forms.Add(expr);
            }
            catch (LispErrorException ex) when (
                ex.Condition is LispCondition lc && lc.ConditionTypeName == "END-OF-FILE")
            {
                incomplete = true;
            }
            catch (LispErrorException ex)
            {
                Console.Error.WriteLine(ReplColor.ForErr("ERROR", $"; read error: {ConditionText.Line(ex.Condition)}"));
                readError = true;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ReplColor.ForErr("ERROR", $"; read error: {ex.Message}"));
                readError = true;
            }

            if (incomplete) continue;
            if (readError) { buffer.Clear(); continue; }
            buffer.Clear();

                // An error the runtime signals while evaluating or printing
                // (CAR of a non-list, an undefined function, ...) that nothing
                // handles enters the debugger here, as ERROR does, with the
                // signalling frames still live for :bt and :locals. Only here:
                // the reader above signals END-OF-FILE for every unfinished
                // form, and that must stay a request for more input.
                ConditionSystem.UnhandledErrorsEnterDebugger = true;
                try
                {
                foreach (var form in forms)
                {
                    // The history variables, CLHS 25.1.1. AFTEREVAL runs before
                    // the result is printed, because printing can run Lisp and
                    // so can overwrite the values it reads.
                    ReplHistory.BeforeEval(form);
                    var result = Runtime.Eval(form);
                    ReplHistory.AfterEval(result);
                    Console.WriteLine(ReplColor.ForOut("RESULT", Runtime.FormatTop(result, true)));
                }
                }
                finally
                {
                    ConditionSystem.UnhandledErrorsEnterDebugger = false;
                }
            }
            catch (RestartInvocationException rie) when (ReferenceEquals(rie.Tag, abortTag))
            {
                // ABORT invoked -- drop any partial input and re-prompt.
                buffer.Clear();
            }
            catch (LispErrorException ex) when (ex.Condition is LispInteractiveInterrupt)
            {
                buffer.Clear();
                Console.Error.WriteLine(ReplColor.ForErr("ERROR", "; Interrupted."));
            }
            catch (DebuggerDeclinedException ex)
            {
                // A REPL on a pipe reached the debugger: stop as a script does.
                Console.Error.WriteLine(ReplColor.ForErr("ERROR", $"; {ConditionText.Line(ex.Condition)}"));
                Console.Out.Flush();
                Environment.Exit(1);
            }
            catch (LispErrorException ex)
            {
                Console.Error.WriteLine(ReplColor.ForErr("ERROR", $"; {ConditionText.Line(ex.Condition)}"));
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ReplColor.ForErr("ERROR", $"Error: {ex.Message}"));
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

    [System.Runtime.InteropServices.DllImport("kernel32.dll")]
    private static extern uint GetConsoleCP();

    [System.Runtime.InteropServices.DllImport("kernel32.dll")]
    private static extern uint GetConsoleOutputCP();

    [System.Runtime.InteropServices.DllImport("kernel32.dll", SetLastError = true)]
    private static extern bool SetConsoleCP(uint wCodePageID);

    [System.Runtime.InteropServices.DllImport("kernel32.dll", SetLastError = true)]
    private static extern bool SetConsoleOutputCP(uint wCodePageID);

    // The console's code pages as found at startup; 0 when there is no console.
    private static uint _savedConsoleCP, _savedConsoleOutputCP;
    private static int _consoleCodePagesRestored;

    /// <summary>
    /// Record the console's input and output code pages before they are switched
    /// to UTF-8, and arrange for <see cref="RestoreConsoleCodePages"/> to run on
    /// normal exit, Environment.Exit and an unhandled exception. Ctrl+C and
    /// Ctrl+Break are covered by the CancelKeyPress handler. A hard kill
    /// (TerminateProcess) runs no code and cannot be covered.
    /// </summary>
    private static void SaveConsoleCodePages()
    {
        try
        {
            _savedConsoleCP = GetConsoleCP();
            _savedConsoleOutputCP = GetConsoleOutputCP();
        }
        catch
        {
            return;
        }
        if (_savedConsoleCP == 0 && _savedConsoleOutputCP == 0) return;
        AppDomain.CurrentDomain.ProcessExit += (_, _) => RestoreConsoleCodePages();
        AppDomain.CurrentDomain.UnhandledException += (_, _) => RestoreConsoleCodePages();
    }

    /// <summary>
    /// Put back the console code pages saved at startup. Pending console output
    /// is flushed first, so it is shown in the code page it was written for.
    /// Runs at most once.
    /// </summary>
    private static void RestoreConsoleCodePages()
    {
        if (_savedConsoleCP == 0 && _savedConsoleOutputCP == 0) return;
        if (System.Threading.Interlocked.Exchange(ref _consoleCodePagesRestored, 1) != 0) return;
        try { Console.Out.Flush(); } catch { }
        try { Console.Error.Flush(); } catch { }
        try
        {
            if (_savedConsoleCP != 0 && GetConsoleCP() != _savedConsoleCP)
                SetConsoleCP(_savedConsoleCP);
            if (_savedConsoleOutputCP != 0 && GetConsoleOutputCP() != _savedConsoleOutputCP)
                SetConsoleOutputCP(_savedConsoleOutputCP);
        }
        catch
        {
            // Best-effort: the console may already be gone.
        }
    }

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
            // fd 1 = stdout: the same fd .NET wrote the smkx (ESC[?1h ESC=) to.
            // Only act when it is a real terminal, so redirected output stays clean.
            if (isatty(1) != 1) return;
            // A terminal that takes no escape sequences (an editor's shell
            // buffer) was never put in application mode, and would show the
            // reset as text on the last line.
            if (ReplTerminal.Dumb(Environment.GetEnvironmentVariable("TERM"))) return;
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

    /// <summary>
    /// Turn on virtual terminal processing for standard output and standard
    /// error. Returns, for each, whether it is now a console that interprets
    /// escape sequences: false for a handle that is not a console (a pipe, a
    /// file, mintty's pipes) and for a console too old to take the mode.
    /// </summary>
    private static (bool stdout, bool stderr) EnableWindowsVtMode()
    {
        var ok = new bool[2];
        var handles = new[] { STD_OUTPUT_HANDLE, STD_ERROR_HANDLE };
        for (int k = 0; k < handles.Length; k++)
        {
            try
            {
                var h = GetStdHandle(handles[k]);
                if (h == IntPtr.Zero || h == new IntPtr(-1)) continue;
                if (!GetConsoleMode(h, out var mode)) continue;
                if ((mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING) != 0) { ok[k] = true; continue; }
                ok[k] = SetConsoleMode(h, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
            }
            catch
            {
                // Best-effort: fail silently if console isn't attached or P/Invoke fails.
            }
        }
        return (ok[0], ok[1]);
    }

    /// <summary>
    /// Turn the line editor (dotcl-repl) on or off for this REPL, as
    /// <see cref="ReplTerminal.LineEditing"/> decides from --readline /
    /// --no-readline, TERM and whether standard input and output are a
    /// terminal. A user init file may already have turned the editor on: that
    /// is kept, except in a terminal that says it takes no escape sequences
    /// (TERM=dumb), where the editor would fill the screen with them. The init
    /// file serves every terminal the REPL is started in, an editor's shell
    /// buffer included, so it cannot have meant that one.
    /// </summary>
    static void ConfigureLineEditing(bool? readlinePref)
    {
        var term = Environment.GetEnvironmentVariable("TERM");
        if (ReplTerminal.Dumb(term))
        {
            Startup.ReadlineHook = null;
            Startup.DebuggerReadHook = null;
            return;
        }
        bool outTerminal = !Console.IsOutputRedirected && (!OperatingSystem.IsWindows() || _vtOut);
        if (ReplTerminal.LineEditing(readlinePref, term, !Console.IsInputRedirected, outTerminal)
            && Startup.ReadlineHook == null)
            TryEnableReadline();
    }

    // What --color said; auto unless given.
    static ReplColorMode _colorMode = ReplColorMode.Auto;
    // Whether EnableWindowsVtMode turned the mode on (Windows only).
    static bool _vtOut, _vtErr;

    /// <summary>
    /// Decide the REPL's colour for both streams. A stream counts as a terminal
    /// when it is not redirected and, on Windows, when its console took virtual
    /// terminal processing: an old console would print the escape sequences as
    /// text. --color=always paints regardless, as asked.
    /// </summary>
    static void ConfigureReplColor()
    {
        bool win = OperatingSystem.IsWindows();
        bool outTerminal = !Console.IsOutputRedirected && (!win || _vtOut);
        bool errTerminal = !Console.IsErrorRedirected && (!win || _vtErr);
        ReplColor.Configure(_colorMode, outTerminal, errTerminal);
    }
}
/// <summary>
/// Standard input as dotcl reads it. Two things a Windows shell can put on a
/// pipe are taken out here: a byte order mark at the very start, which would
/// otherwise become the first character of the first token (a symbol whose
/// name prints as nothing), and the CR of a CR LF line end, which READ-LINE
/// would otherwise return at the end of every line. Python and Node do the
/// same with their standard input. Only the first character is checked for
/// the mark; a U+FEFF anywhere else is data.
///
/// On a Windows console one more thing is put back: a line that starts with
/// Ctrl+Z (U+001A) is the end of input, which is how the console's own reader
/// treats it. Standard input is opened here as a plain stream, below that
/// reader, so without this the key arrives as a character and there is no way
/// to end input from the keyboard. The rest of that line is dropped, as the
/// console does, and reading may go on afterwards. A pipe or a file is left
/// alone: there the byte is data, as it is to .NET and Python.
/// </summary>
sealed class StdinReader : System.IO.TextReader
{
    private const int None = -2;
    private const char EndMark = '\u001A';
    private readonly System.IO.TextReader _in;
    private readonly bool _endMark;
    private bool _started;
    // Only tracked when _endMark is on: the next character begins a line.
    private bool _atLineStart = true;
    // One character already taken by Peek, or None.
    private int _pending = None;

    public StdinReader(System.IO.TextReader inner, bool consoleEndMark = false)
    {
        _in = inner;
        _endMark = consoleEndMark;
    }

    private void Start()
    {
        if (_started) return;
        _started = true;
        if (_in.Peek() == 0xFEFF) _in.Read();
    }

    // The next character with CR LF folded to LF.
    private int Next()
    {
        int c = _in.Read();
        if (c == '\r' && _in.Peek() == '\n') c = _in.Read();
        return c;
    }

    // Next, with a Ctrl+Z that begins a line read as the end of input.
    private int NextChecked()
    {
        int c = Next();
        if (!_endMark) return c;
        if (c == EndMark && _atLineStart)
        {
            while (c != '\n' && c != -1) c = Next();
            return -1;
        }
        _atLineStart = c == '\n';
        return c;
    }

    private int Take()
    {
        if (_pending == None) return NextChecked();
        int p = _pending;
        _pending = None;
        return p;
    }

    public override int Peek()
    {
        Start();
        if (_pending == None) _pending = NextChecked();
        return _pending;
    }

    public override int Read()
    {
        Start();
        return Take();
    }

    public override int Read(char[] buffer, int index, int count)
    {
        Start();
        if (count == 0) return 0;
        if (_endMark)
        {
            // Someone is typing: a character at a time up to the end of the
            // line, which is what the console hands over in one read anyway.
            int k = 0;
            while (k < count)
            {
                int c = Take();
                if (c == -1) break;
                buffer[index + k++] = (char)c;
                if (c == '\n') break;
            }
            return k;
        }
        if (_pending != None)
        {
            int p = _pending;
            _pending = None;
            if (p == -1) return 0;
            buffer[index] = (char)p;
            return 1;
        }
        int n = _in.Read(buffer, index, count);
        int end = index + n, w = index;
        for (int r = index; r < end; r++)
        {
            char c = buffer[r];
            if (c == '\r')
            {
                // A CR whose LF is in this block, or the next one, is dropped.
                if (r + 1 < end ? buffer[r + 1] == '\n' : _in.Peek() == '\n')
                    continue;
            }
            buffer[w++] = c;
        }
        // Everything read was a dropped CR: read on rather than report end of input.
        if (n > 0 && w == index) return Read(buffer, index, count);
        return w - index;
    }

    public override string? ReadLine()
    {
        Start();
        if (_endMark)
        {
            int c = Take();
            if (c == -1) return null;
            var sb = new System.Text.StringBuilder();
            while (c != -1 && c != '\n')
            {
                sb.Append((char)c);
                c = Take();
            }
            return sb.ToString();
        }
        if (_pending == None) return _in.ReadLine();
        int p = _pending;
        _pending = None;
        if (p == -1) return null;
        if (p == '\n' || p == '\r') return "";
        return (char)p + (_in.ReadLine() ?? "");
    }

    protected override void Dispose(bool disposing)
    {
        if (disposing) _in.Dispose();
        base.Dispose(disposing);
    }
}
