using System.Collections.Concurrent;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    // ===== Environment functions =====

    public static LispObject LispImplementationType(LispObject[] args)
    {
        if (args.Length != 0) throw new LispErrorException(new LispProgramError("LISP-IMPLEMENTATION-TYPE: wrong number of arguments: " + args.Length + " (expected 0)"));
        return new LispString("dotcl");
    }

    public static LispObject LispImplementationVersion(LispObject[] args)
    {
        if (args.Length != 0) throw new LispErrorException(new LispProgramError("LISP-IMPLEMENTATION-VERSION: wrong number of arguments: " + args.Length + " (expected 0)"));
        var version = typeof(Runtime).Assembly
            .GetCustomAttribute<System.Reflection.AssemblyInformationalVersionAttribute>()
            ?.InformationalVersion ?? "unknown";
        return new LispString(version);
    }

    public static LispObject SoftwareType(LispObject[] args)
    {
        if (args.Length != 0) throw new LispErrorException(new LispProgramError("SOFTWARE-TYPE: wrong number of arguments: " + args.Length + " (expected 0)"));
        return new LispString(System.Runtime.InteropServices.RuntimeInformation.OSDescription);
    }

    public static LispObject SoftwareVersion(LispObject[] args)
    {
        if (args.Length != 0) throw new LispErrorException(new LispProgramError("SOFTWARE-VERSION: wrong number of arguments: " + args.Length + " (expected 0)"));
        return new LispString(Environment.OSVersion.VersionString);
    }

    public static LispObject MachineType(LispObject[] args)
    {
        if (args.Length != 0) throw new LispErrorException(new LispProgramError("MACHINE-TYPE: wrong number of arguments: " + args.Length + " (expected 0)"));
        return new LispString(System.Runtime.InteropServices.RuntimeInformation.OSArchitecture.ToString());
    }

    public static LispObject MachineVersion(LispObject[] args)
    {
        if (args.Length != 0) throw new LispErrorException(new LispProgramError("MACHINE-VERSION: wrong number of arguments: " + args.Length + " (expected 0)"));
        return new LispString(System.Runtime.InteropServices.RuntimeInformation.ProcessArchitecture.ToString());
    }

    public static LispObject MachineInstance(LispObject[] args)
    {
        if (args.Length != 0) throw new LispErrorException(new LispProgramError("MACHINE-INSTANCE: wrong number of arguments: " + args.Length + " (expected 0)"));
        return new LispString(Environment.MachineName);
    }

    public static LispObject ShortSiteName(LispObject[] args)
    {
        if (args.Length != 0) throw new LispErrorException(new LispProgramError("SHORT-SITE-NAME: wrong number of arguments: " + args.Length + " (expected 0)"));
        return Nil.Instance;
    }

    public static LispObject LongSiteName(LispObject[] args)
    {
        if (args.Length != 0) throw new LispErrorException(new LispProgramError("LONG-SITE-NAME: wrong number of arguments: " + args.Length + " (expected 0)"));
        return Nil.Instance;
    }

    // The time zone reported alongside a decoded time excludes daylight saving.
    private static double StandardUtcOffsetHours(System.TimeZoneInfo tz, System.DateTime dt)
    {
        double offset = tz.GetUtcOffset(dt).TotalHours;
        if (!tz.IsDaylightSavingTime(dt)) return offset;
        System.TimeZoneInfo.AdjustmentRule? match = null;
        foreach (var rule in tz.GetAdjustmentRules())
            if (dt >= rule.DateStart && dt <= rule.DateEnd
                && (match == null || rule.DateStart > match.DateStart))
                match = rule;
        return match == null ? offset : offset - match.DaylightDelta.TotalHours;
    }

    public static LispObject AproposList(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError("APROPOS-LIST: requires at least 1 argument"));
        if (args.Length > 2)
            throw new LispErrorException(new LispProgramError("APROPOS-LIST: wrong number of arguments: " + args.Length + " (expected 1-2)"));
        string searchStr;
        if (args[0] is LispString ls) searchStr = ls.Value;
        else if (args[0] is Symbol sym) searchStr = sym.Name;
        else if (args[0] is LispChar lc) searchStr = lc.Value.ToString();
        else if (args[0] is LispVector lv && lv.IsCharVector) searchStr = lv.ToCharString();
        else throw new LispErrorException(new LispTypeError("APROPOS-LIST: argument must be a string designator", args[0]));

        searchStr = searchStr.ToUpperInvariant();

        IEnumerable<Package> packages;
        bool singlePackage = args.Length >= 2 && !(args[1] is Nil);
        if (singlePackage)
        {
            var pkg = ResolvePackage(args[1], "APROPOS-LIST");
            packages = new[] { pkg };
        }
        else
        {
            packages = Package.AllPackages;
        }

        LispObject result = Nil.Instance;
        var seen = new HashSet<Symbol>();
        void AddMatch(Symbol sym)
        {
            if (sym.Name.ToUpperInvariant().Contains(searchStr) && seen.Add(sym))
                result = new Cons(CanonicalizeSymbol(sym), result);
        }
        foreach (var pkg in packages)
        {
            foreach (var sym in pkg.ExternalSymbols) AddMatch(sym);
            foreach (var sym in pkg.InternalSymbols) AddMatch(sym);
            if (singlePackage)
                foreach (var used in pkg.UseList)
                    foreach (var sym in used.ExternalSymbols)
                    {
                        var (found, status) = pkg.FindSymbol(sym.Name);
                        if (status == SymbolStatus.Inherited && ReferenceEquals(found, sym))
                            AddMatch(sym);
                    }
        }
        return result;
    }

    public static LispObject Apropos(LispObject[] args)
    {
        if (args.Length > 2)
            throw new LispErrorException(new LispProgramError("APROPOS: wrong number of arguments: " + args.Length + " (expected 1-2)"));
        var list = AproposList(args);
        var writer = GetStandardOutputWriter();
        var cur = list;
        while (cur is Cons c)
        {
            {
                var sym = GetSymbol(c.Car, "APROPOS");
                writer.Write(sym.Name);
                if (sym.Function != null) writer.Write(" (function)");
                if (sym.Value != null && !sym.IsConstant) writer.Write(" (variable)");
                writer.WriteLine();
            }
            cur = c.Cdr;
        }
        writer.Flush();
        // APROPOS returns no values
        return MultipleValues.Values();
    }

    public static LispObject Describe(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError("DESCRIBE: requires at least 1 argument"));
        if (args.Length > 2)
            throw new LispErrorException(new LispProgramError("DESCRIBE: wrong number of arguments: " + args.Length + " (expected 1-2)"));
        var obj = args[0];
        var streamArg = args.Length >= 2 ? args[1] : Nil.Instance;

        // Resolve stream designator per CLHS:
        // NIL -> *standard-output*, T -> *terminal-io*, otherwise the stream itself
        LispObject stream;
        if (streamArg is Nil)
        {
            if (!DynamicBindings.TryGet(Startup.Sym("*STANDARD-OUTPUT*"), out stream))
                stream = Startup.Sym("*STANDARD-OUTPUT*").Value ?? Nil.Instance;
        }
        else if (streamArg is T)
        {
            if (!DynamicBindings.TryGet(Startup.Sym("*TERMINAL-IO*"), out stream))
                stream = Startup.Sym("*TERMINAL-IO*").Value ?? Nil.Instance;
        }
        else
        {
            stream = streamArg;
        }

        // Call describe-object GF
        var describeObjectFn = Startup.Sym("DESCRIBE-OBJECT").Function as LispFunction;
        if (describeObjectFn != null)
        {
            describeObjectFn.Invoke(new LispObject[] { obj, stream });
        }
        else
        {
            // Fallback if GF not yet registered
            var writer = GetOutputWriter(streamArg);
            var typeObj = TypeOf(obj);
            writer.Write(FormatObject(obj, true));
            writer.WriteLine();
            writer.Write("  [");
            writer.Write(FormatObject(typeObj, true));
            writer.WriteLine("]");
            writer.Flush();
        }

        // DESCRIBE returns no values
        return MultipleValues.Values();
    }

    public static LispObject Room(LispObject[] args)
    {
        if (args.Length > 1)
            throw new LispErrorException(new LispProgramError("ROOM: wrong number of arguments: " + args.Length + " (expected 0-1)"));
        var writer = GetStandardOutputWriter();
        long totalMem = GC.GetTotalMemory(false);
        long allocated = Compat.GetTotalAllocatedBytes(false);
        writer.WriteLine($"Total memory: {totalMem:N0} bytes");
        writer.WriteLine($"Total allocated: {allocated:N0} bytes");
        writer.WriteLine($"GC generation 0 collections: {GC.CollectionCount(0)}");
        writer.WriteLine($"GC generation 1 collections: {GC.CollectionCount(1)}");
        writer.WriteLine($"GC generation 2 collections: {GC.CollectionCount(2)}");
        writer.Flush();
        return Nil.Instance;
    }

    // Returns (gen0-count gen1-count gen2-count total-memory total-allocated-bytes)
    // Used by the TIME macro to compute GC deltas around a form.
    private static int _exitHooksInstalled;
    private static int _exitHooksRan;

    /// <summary>Run the functions in the global value of HOOKS (dotcl:*exit-hooks*)
    /// when the process exits. ProcessExit is raised both by Environment.Exit
    /// (dotcl:quit) and when Main returns, on the exiting thread. The hooks run
    /// once, in list order; an error in one is printed and the rest still run.
    /// Standard output and error output are finished afterwards, so what a hook
    /// prints is not lost.</summary>
    internal static void InstallExitHooks(Symbol hooks)
    {
        if (System.Threading.Interlocked.Exchange(ref _exitHooksInstalled, 1) != 0) return;
        AppDomain.CurrentDomain.ProcessExit += (_, _) => RunExitHooks(hooks);
    }

    private static void RunExitHooks(Symbol hooks)
    {
        if (System.Threading.Interlocked.Exchange(ref _exitHooksRan, 1) != 0) return;
        var list = hooks.Value;
        if (list is not Cons) return;
        for (var cur = list; cur is Cons c; cur = c.Cdr)
        {
            try
            {
                var fn = c.Car is Symbol fs ? fs.Function : c.Car;
                if (fn is LispFunction lf) lf.Invoke();
                else throw new LispErrorException(new LispTypeError(
                    "*EXIT-HOOKS*: not a function", c.Car, Startup.Sym("FUNCTION")));
            }
            catch (Exception ex)
            {
                try
                {
                    Console.Error.WriteLine($"dotcl: error in exit hook {c.Car}: {ex.Message}");
                    Console.Error.Flush();
                }
                catch { }
            }
        }
        foreach (var name in new[] { "*STANDARD-OUTPUT*", "*ERROR-OUTPUT*" })
        {
            try
            {
                var stream = Startup.Sym(name).Value;
                if (stream != null && Startup.Sym("FINISH-OUTPUT").Function is LispFunction fo)
                    fo.Invoke(stream);
            }
            catch { }
        }
        try { Console.Out.Flush(); } catch { }
    }

    public static LispObject GcStats(LispObject[] args)
    {
        if (args.Length != 0)
            throw new LispErrorException(new LispProgramError("GC-STATS: takes no arguments"));
        long gen0 = GC.CollectionCount(0);
        long gen1 = GC.CollectionCount(1);
        long gen2 = GC.CollectionCount(2);
        long totalMem = GC.GetTotalMemory(false);
        long allocated = Compat.GetTotalAllocatedBytes(false);
        // Total GC pause time in internal time units (microseconds), as
        // sb-ext:*gc-run-time* is; NIL where the runtime cannot tell.
        var pause = Compat.GetTotalGcPauseDuration();
        LispObject pauseTime = pause is TimeSpan p ? Fixnum.Make(p.Ticks / 10) : Nil.Instance;
        return new Cons(Fixnum.Make(gen0),
               new Cons(Fixnum.Make(gen1),
               new Cons(Fixnum.Make(gen2),
               new Cons(Fixnum.Make(totalMem),
               new Cons(Fixnum.Make(allocated),
               new Cons(pauseTime, Nil.Instance))))));
    }

    // COMPILER-MACRO-FUNCTION body (shared by the args-array wrapper and the
    // 1-arg direct delegate).
    private static LispObject CompilerMacroFunctionImpl(LispObject nameArg)
    {
        // (setf foo) form: a cons (setf . (foo . nil))
        if (nameArg is Cons setfCons && setfCons.Car is Symbol setfSym && setfSym.Name == "SETF") {
            var innerSym = (setfCons.Cdr is Cons c2) ? c2.Car as Symbol : null;
            if (innerSym != null && _setfCompilerMacros.TryGetValue(innerSym, out var sfn))
                return sfn;
            return Nil.Instance;
        }
        var sym = nameArg as Symbol;
        if (sym == null) return Nil.Instance;
        return _compilerMacroFunctions.TryGetValue(sym, out var fn)
            ? (LispObject)fn : Nil.Instance;
    }

    // FBOUNDP body (shared by the args-array wrapper and the 1-arg direct delegate).
    private static LispObject FboundpImpl(LispObject obj)
    {
        if (obj is Cons setfCons && setfCons.Car is Symbol setfKw && setfKw.Name == "SETF")
        {
            if (setfCons.Cdr is not Cons setfRest || setfRest.Cdr is not Nil)
                throw new LispErrorException(new LispTypeError("FBOUNDP: invalid (setf ...) form", obj));
            var setfName = Runtime.SetfNameTarget(obj);
            if (setfName == null)
                throw new LispErrorException(new LispTypeError("FBOUNDP: second element of (setf ...) must be a symbol", setfRest.Car));
            // sym.SetfFunction is authoritative.
            return setfName.SetfFunction != null ? T.Instance : Nil.Instance;
        }
        Symbol s;
        if (obj is Symbol sym2) s = sym2;
        else if (obj is Nil) s = Startup.NIL_SYM;
        else if (obj is T) s = Startup.T_SYM;
        else throw new LispErrorException(new LispTypeError("FBOUNDP: not a symbol", obj));
        if (s.Function != null) return T.Instance;
        if (Runtime.MacroFunction(s) != Nil.Instance) return T.Instance;
        if (Startup.LookupCompilerMacro(s) != null) return T.Instance;
        if (Runtime.IsSpecialOperator(s)) return T.Instance;
        return Nil.Instance;
    }

    // dotcl:emit-pool-stats: (total-entries dynamic-methods data-literals committed-bytes)
    // Diagnostic for the CilAssembler constant-pool retention leak: the
    // global pool roots every compiled DynamicMethod forever, so DYNAMIC-METHODS
    // tracks the JIT-backed code that never frees. COMMITTED-BYTES is the GC's
    // committed heap (the off-heap JIT code is the gap between this and RSS).
    public static LispObject EmitPoolStats(LispObject[] args)
    {
        if (args.Length != 0)
            throw new LispErrorException(new LispProgramError("EMIT-POOL-STATS: takes no arguments"));
        var (total, dm, data) = Emitter.CilAssembler.ConstantsPoolStats();
#if DOTCL_EMIT
        long committed = GC.GetGCMemoryInfo().TotalCommittedBytes;
#else
        long committed = 0; // GetGCMemoryInfo is unavailable on netstandard2.0
#endif
        return new Cons(Fixnum.Make(total),
               new Cons(Fixnum.Make(dm),
               new Cons(Fixnum.Make(data),
               new Cons(Fixnum.Make(committed), Nil.Instance))));
    }

    public static LispObject Disassemble(LispObject[] args)
    {
        if (args.Length != 1)
            throw new LispErrorException(new LispProgramError(
                $"DISASSEMBLE: wrong number of arguments: {args.Length} (expected 1)"));
        var arg = args[0];

        // Resolve function designator to LispFunction
        LispFunction? fn = null;
        string? fnName = null;
        if (arg is LispFunction lf)
        {
            fn = lf;
            fnName = lf.Name;
        }
        else if (arg is Symbol sym)
        {
            fn = sym.Function as LispFunction;
            fnName = sym.Name;
            if (fn == null)
                throw new LispErrorException(new LispUndefinedFunction(sym));
        }
        else if (arg is Cons c && c.Car is Symbol cs)
        {
            if (cs.Name == "LAMBDA")
            {
                // Compile and disassemble
                var compiled = CompileTopLevel(arg);
                var result = Emitter.CilAssembler.AssembleAndRun(compiled);
                if (result is LispFunction compiledFn)
                {
                    fn = compiledFn;
                    fnName = "(LAMBDA)";
                }
            }
            else if (cs.Name == "SETF" && c.Cdr is Cons c2 && c2.Car is Symbol setfSym && c2.Cdr is Nil)
            {
                // sym.SetfFunction is authoritative.
                fn = setfSym.SetfFunction as LispFunction;
                fnName = $"(SETF {setfSym.Name})";
            }
        }
        if (fn == null)
            throw new LispErrorException(new LispTypeError(
                "DISASSEMBLE: argument is not a valid function designator", arg,
                Startup.Sym("FUNCTION-DESIGNATOR")));

        var writer = GetStandardOutputWriter();
        if (fn.Sil != null)
        {
            writer.WriteLine($"; disassembly for {fnName ?? "#<FUNCTION>"}");
            DisassembleSil(writer, fn.Sil);
        }
        else
        {
            writer.WriteLine($"; No disassembly for {fnName ?? "#<FUNCTION>"}.");
            writer.WriteLine("; Set (SETF DOTCL:*SAVE-SIL* T) then redefine the function to enable.");
        }
        writer.Flush();
        return Nil.Instance;
    }

    // Hook set by dotcl-jitdisasm contrib at (require "dotcl-jitdisasm") time.
    public static Action<LispFunction, TextWriter>? JitDisassembleHook;

    public static LispObject JitDisassemble(LispObject[] args)
    {
        if (args.Length != 1)
            throw new LispErrorException(new LispProgramError(
                $"DOTCL:JIT-DISASSEMBLE: wrong number of arguments: {args.Length} (expected 1)"));
        var arg = args[0];

        LispFunction? fn = null;
        if (arg is LispFunction lf)
            fn = lf;
        else if (arg is Symbol sym)
        {
            fn = sym.Function as LispFunction;
            if (fn == null)
                throw new LispErrorException(new LispUndefinedFunction(sym));
        }
        else if (arg is Cons c && c.Car is Symbol cs && cs.Name == "SETF"
                 && c.Cdr is Cons c2 && c2.Car is Symbol setfSym && c2.Cdr is Nil)
            fn = setfSym.SetfFunction as LispFunction;

        if (fn == null)
            throw new LispErrorException(new LispTypeError(
                "DOTCL:JIT-DISASSEMBLE: not a valid function designator", arg,
                Startup.Sym("FUNCTION-DESIGNATOR")));

        if (JitDisassembleHook == null)
            throw new LispErrorException(new LispProgramError(
                "DOTCL:JIT-DISASSEMBLE: not available; load the contrib first: (require \"dotcl-jitdisasm\")"));

        var writer = GetStandardOutputWriter();
        JitDisassembleHook(fn, writer);
        writer.Flush();
        return Nil.Instance;
    }

    /// <summary>
    /// DOTCL:BACKTRACE: return the current Lisp call stack as a list of
    /// function-name strings, innermost (most recent) frame first. Only named
    /// functions are tracked (see LispFunction call-stack push/pop); anonymous
    /// lambdas and native-fixnum self-calls do not appear. This function itself
    /// is registered without a Name, so it never shows up in its own result.
    /// </summary>
    public static LispObject Backtrace(LispObject[] args)
    {
        var frames = LispFunction.GetCallStack();
        LispObject result = Nil.Instance;
        for (int i = frames.Length - 1; i >= 0; i--)
            result = new Cons(new LispString(frames[i]), result);
        return result;
    }

    /// <summary>
    /// DOTCL:BACKTRACE-WITH-ARGS: like DOTCL:BACKTRACE, but each frame is a list
    /// (NAME arg0 arg1 ...) whose args are the actual captured argument objects,
    /// innermost (most recent) frame first. Lets a debugger / error handler inspect
    /// the arguments a frame was called with, not just function names (cf.
    /// sb-debug:list-backtrace). Same tracking caveats as DOTCL:BACKTRACE (only
    /// named functions; anonymous lambdas and native-fixnum self-calls are absent).
    /// </summary>
    public static LispObject BacktraceWithArgs(LispObject[] args)
    {
        var frames = LispFunction.GetCallStackWithArgs();
        LispObject result = Nil.Instance;
        for (int i = frames.Length - 1; i >= 0; i--)
            result = new Cons(frames[i], result);
        return result;
    }

    /// <summary>
    /// DOTCL:FRAME-LOCALS (&amp;optional (n 0)): the lexical variables of backtrace
    /// frame N (0 = innermost, same numbering as DOTCL:BACKTRACE) as an alist
    /// (("NAME" . value) ...) in binding order. NIL when the frame recorded none:
    /// its function was compiled with frame-locals mode off (the default), is
    /// implemented in C#, or binds no user variables. To record locals, compile the
    /// code with DOTCL:*EMIT-FRAME-LOCALS* true.
    /// </summary>
    public static LispObject FrameLocals(LispObject[] args)
    {
        int idx = 0;
        if (args.Length >= 1 && args[0] is not Nil)
        {
            if (args[0] is not Fixnum f)
                throw new LispErrorException(new LispTypeError(
                    "FRAME-LOCALS: frame index must be a fixnum",
                    args[0], Startup.Sym("FIXNUM")));
            idx = (int)f.Value;
        }
        return DebugFrames.Locals(idx);
    }

    /// <summary>
    /// DOTCL:FRAME-SPECIALS (&amp;optional (n 0)): the dynamic (special-variable)
    /// bindings in effect, innermost first, as ((SYMBOL value . own-p) ...).
    /// OWN-P is true for the ones backtrace frame N or its callees established.
    ///
    /// Every binding is listed, including ones shadowed by an inner rebinding of
    /// the same symbol: that is what a reader of nested LETs wants to see.
    /// Unlike FRAME-LOCALS this needs no frame-locals mode: the binding stack is
    /// always there, and N only decides where the OWN-P line falls.
    /// </summary>
    public static LispObject FrameSpecials(LispObject[] args)
    {
        int idx = 0;
        if (args.Length >= 1 && args[0] is not Nil)
        {
            if (args[0] is not Fixnum f)
                throw new LispErrorException(new LispTypeError(
                    "FRAME-SPECIALS: frame index must be a fixnum",
                    args[0], Startup.Sym("FIXNUM")));
            idx = (int)f.Value;
        }
        return DebugFrames.Specials(idx);
    }

    /// <summary>
    /// DOTCL:PRINT-FRAME-LOCALS (&amp;optional (n 0) stream): print backtrace frame
    /// N's lexical variables, one "NAME = value" line each, indented. Defaults to
    /// *ERROR-OUTPUT*. Same rendering the debugger's :locals command uses.
    /// </summary>
    public static LispObject PrintFrameLocals(LispObject[] args)
    {
        int idx = 0;
        if (args.Length >= 1 && args[0] is not Nil)
        {
            if (args[0] is not Fixnum f)
                throw new LispErrorException(new LispTypeError(
                    "PRINT-FRAME-LOCALS: frame index must be a fixnum",
                    args[0], Startup.Sym("FIXNUM")));
            idx = (int)f.Value;
        }
        LispObject streamArg = args.Length >= 2 ? args[1] : Nil.Instance;
        if (streamArg is Nil)
            streamArg = DynamicBindings.Get(Startup.Sym("*ERROR-OUTPUT*"));
        var writer = GetTextWriter(streamArg);
        var lines = DebugFrames.FormatLocals(idx);
        if (lines.Length == 0)
            writer.Write("  (no locals recorded for this frame)\n");
        else
            foreach (var line in lines)
                writer.Write($"  {line}\n");
        writer.Flush();
        return Nil.Instance;
    }

    /// <summary>
    /// DOTCL:PRINT-BACKTRACE (&amp;optional stream): print the current call stack
    /// as numbered frames. Defaults to *ERROR-OUTPUT* (cf. sb-debug:print-backtrace).
    /// </summary>
    public static LispObject PrintBacktrace(LispObject[] args)
    {
        LispObject streamArg = args.Length >= 1 ? args[0] : Nil.Instance;
        if (streamArg is Nil)
            streamArg = DynamicBindings.Get(Startup.Sym("*ERROR-OUTPUT*"));
        var writer = GetTextWriter(streamArg);
        var frames = LispFunction.GetCallStackForms();
        if (frames.Length == 0)
            writer.Write("; (no Lisp frames)\n");
        else
            for (int i = 0; i < frames.Length; i++)
                writer.Write($"{i,3}: {frames[i]}\n");
        writer.Flush();
        return Nil.Instance;
    }

    private static void DisassembleSil(TextWriter writer, LispObject sil)
    {
        // SIL is a flat list of instructions: ((:INSTR args...) (:INSTR args...) ...)
        var cur = sil;
        int indent = 2;
        while (cur is Cons c)
        {
            var instr = c.Car;
            if (instr is Cons ic)
            {
                var opSym = ic.Car;
                var opName = opSym is Symbol s ? s.Name : opSym?.ToString() ?? "?";

                // Labels get no indent
                if (opName == "LABEL")
                {
                    var labelName = ic.Cdr is Cons lc ? FormatSilArg(lc.Car) : "?";
                    writer.WriteLine($"{labelName}:");
                }
                else
                {
                    // Format: "  (:OP arg1 arg2 ...)"
                    var sb = new System.Text.StringBuilder();
                    sb.Append(' ', indent);
                    sb.Append(opName.ToLower());
                    var argCur = ic.Cdr;
                    while (argCur is Cons ac)
                    {
                        sb.Append(' ');
                        sb.Append(FormatSilArg(ac.Car));
                        argCur = ac.Cdr;
                    }
                    writer.WriteLine(sb.ToString());
                }
            }
            cur = c.Cdr;
        }
    }

    private static string FormatSilArg(LispObject arg)
    {
        if (arg is LispString ls) return $"\"{ls.Value}\"";
        if (arg is Symbol sym)
        {
            // Strip compiler package prefix for readability
            var name = sym.Name;
            if (sym.HomePackage?.Name == "DOTCL.CIL-COMPILER")
            {
                // Strip gensym suffixes like _1234 for readability? No, keep them.
                return name;
            }
            return sym.ToString();
        }
        if (arg is Cons c)
        {
            // Sub-list (e.g., switch targets)
            var sb = new System.Text.StringBuilder("(");
            var cur = (LispObject)c;
            bool first = true;
            while (cur is Cons cc)
            {
                if (!first) sb.Append(' ');
                sb.Append(FormatSilArg(cc.Car));
                cur = cc.Cdr;
                first = false;
            }
            sb.Append(')');
            return sb.ToString();
        }
        return arg?.ToString() ?? "NIL";
    }

    // Trace infrastructure: maps function name key -> original (unwrapped) function
    private static readonly ConcurrentDictionary<string, LispFunction> _tracedOriginals = new();
    private static readonly ConcurrentDictionary<string, LispObject> _tracedNames = new();
    // The wrapper TRACE installed; UNTRACE restores only while it still holds the name.
    private static readonly ConcurrentDictionary<string, LispFunction> _tracedWrappers = new();
    // Traced generic functions keep their identity: the trace wrapper is installed
    // as the generic function's discriminating function, so #'NAME stays a generic
    // function (what SBCL does). Key -> (wrapper, discriminating function it replaced).
    private static readonly ConcurrentDictionary<string, (LispFunction Wrapper, LispFunction? Previous)> _tracedGfDispatch = new();
    // Trace wrapper -> the function it wraps, so introspection on a traced name
    // (DOCUMENTATION, the lambda list) answers about the traced function, not the
    // wrapper.
    private static readonly System.Runtime.CompilerServices.ConditionalWeakTable<LispFunction, LispFunction> _traceWrapperOriginals = new();

    /// <summary>The function a trace wrapper stands for, or FN itself.</summary>
    internal static LispObject UntracedFunction(LispObject fn)
    {
        while (fn is LispFunction lf && _traceWrapperOriginals.TryGetValue(lf, out var orig))
            fn = orig;
        return fn;
    }

    private static LispObject TraceCall(LispObject name, LispObject[] wrapArgs,
        Func<LispObject[], LispObject> call)
    {
        var traceWriter = GetTraceOutputWriter();
        traceWriter.Write("  Calling ");
        traceWriter.Write(FormatObject(name, true));
        traceWriter.Write(" with (");
        for (int i = 0; i < wrapArgs.Length; i++)
        {
            if (i > 0) traceWriter.Write(" ");
            traceWriter.Write(FormatObject(wrapArgs[i], true));
        }
        traceWriter.Write(")\n");
        traceWriter.Flush();
        var result = call(wrapArgs);
        traceWriter.Write("  => ");
        traceWriter.Write(FormatObject(result, true));
        traceWriter.Write('\n');
        traceWriter.Flush();
        return result;
    }

    private static string TraceNameKey(LispObject name)
    {
        if (name is Symbol sym) return TypeExpanderKey(sym);
        if (name is Cons c && c.Car is Symbol s && s.Name == "SETF" && c.Cdr is Cons c2 && c2.Car is Symbol s2)
            return "(SETF " + TypeExpanderKey(s2) + ")";
        return name.ToString()!;
    }

    public static LispObject Trace(LispObject[] args)
    {
        if (args.Length == 0)
        {
            // Return list of currently traced function names
            LispObject result = Nil.Instance;
            foreach (var kvp in _tracedNames)
                result = new Cons(kvp.Value, result);
            return result;
        }
        foreach (var name in args)
        {
            var key = TraceNameKey(name);
            if (_tracedOriginals.TryRemove(key, out var earlierOriginal))
            {
                _tracedNames.TryRemove(key, out var earlierName);
                RestoreTracedFunction(key, earlierOriginal, earlierName);
            }
            // Find the current function
            LispFunction? original = null;
            if (name is Symbol sym && sym.Function is LispFunction sf)
                original = sf;
            else
            {
                try { original = (LispFunction)Fdefinition(name); }
                catch { }
            }
            if (original == null) continue;
            _tracedOriginals[key] = original;
            _tracedNames[key] = name;
            var capturedName = name;
            if (original is GenericFunction tracedGf)
            {
                var previous = tracedGf.DispatchFunction;
                var gfWrapper = new LispFunction(wrapArgs => TraceCall(capturedName, wrapArgs,
                    a => previous != null ? previous.Invoke(a) : DispatchGFCore(tracedGf, a)),
                    original.Name, -1);
                _tracedGfDispatch[key] = (gfWrapper, previous);
                tracedGf.DispatchFunction = gfWrapper;
                tracedGf.InvalidateCache();
                continue;
            }
            // Create wrapper that prints to *trace-output*
            var capturedOriginal = original;
            var wrapper = new LispFunction(wrapArgs =>
                TraceCall(capturedName, wrapArgs, capturedOriginal.Invoke),
                original.Name, original.Arity);
            _traceWrapperOriginals.Add(wrapper, original);
            _tracedWrappers[key] = wrapper;
            // Replace the function
            if (name is Symbol symN)
            {
                symN.Function = wrapper;
                Emitter.CilAssembler.RegisterFunction(key, wrapper);
            }
            else if (name is Cons setfC && setfC.Car is Symbol setfS && setfS.Name == "SETF"
                     && setfC.Cdr is Cons setfC2 && setfC2.Car is Symbol setfTargetSym)
            {
                // (setf SYM): install on the ACTUAL target symbol's SetfFunction, not
                // Startup.Sym(name-string)'s. Compiled call sites resolve (setf sym)
                // via GetSetfFunctionBySymbol on the source-code symbol (which preserves
                // package); installing on a same-named default-package symbol
                // would leave those calls hitting the untraced original (TRACE.8).
                setfTargetSym.SetfFunction = wrapper;
            }
            else
            {
                Emitter.CilAssembler.RegisterFunction(key, wrapper);
            }
        }
        // Return list of traced names
        return Trace(Array.Empty<LispObject>());
    }

    public static LispObject Untrace(LispObject[] args)
    {
        if (args.Length == 0)
        {
            // Untrace all
            foreach (var kvp in _tracedOriginals.ToArray())
            {
                _tracedNames.TryGetValue(kvp.Key, out var name);
                RestoreTracedFunction(kvp.Key, kvp.Value, name);
            }
            _tracedOriginals.Clear();
            _tracedNames.Clear();
            _tracedWrappers.Clear();
            return Nil.Instance;
        }
        foreach (var name in args)
        {
            var key = TraceNameKey(name);
            if (_tracedOriginals.TryGetValue(key, out var original))
            {
                _tracedNames.TryGetValue(key, out var tracedName);
                RestoreTracedFunction(key, original, tracedName);
                _tracedOriginals.TryRemove(key, out _);
                _tracedNames.TryRemove(key, out _);
            }
        }
        return Trace(Array.Empty<LispObject>());
    }

    private static void RestoreFunctionIfWrapper(Symbol sym, LispFunction wrapper, LispFunction original)
    {
        if (ReferenceEquals(sym.Function, wrapper)) sym.Function = original;
    }

    private static void RestoreTracedFunction(string key, LispFunction original, LispObject? tracedName)
    {
        if (original is GenericFunction tracedGf && _tracedGfDispatch.TryRemove(key, out var installed))
        {
            // Put back what the trace replaced, unless something installed a
            // discriminating function of its own since.
            if (ReferenceEquals(tracedGf.DispatchFunction, installed.Wrapper))
            {
                tracedGf.DispatchFunction = installed.Previous;
                tracedGf.InvalidateCache();
            }
            return;
        }
        var wrapper = _tracedWrappers.TryRemove(key, out var installedWrapper) ? installedWrapper : null;
        // (setf SYM): restore on the ACTUAL target symbol's SetfFunction (mirror of
        // the package-correct install in Trace), not Startup.Sym(name-string)'s.
        if (tracedName is Cons setfC && setfC.Car is Symbol setfS && setfS.Name == "SETF"
            && setfC.Cdr is Cons setfC2 && setfC2.Car is Symbol setfTargetSym)
        {
            if (wrapper != null && ReferenceEquals(setfTargetSym.SetfFunction, wrapper))
                setfTargetSym.SetfFunction = original;
            return;
        }
        if (wrapper == null) return;
        RestoreFunctionIfWrapper(Startup.SymForRegistration(key), wrapper, original);
        // Restore on the original symbol that was traced
        if (tracedName is Symbol origSym)
            RestoreFunctionIfWrapper(origSym, wrapper, original);
        // Also restore on CL/CL-USER symbols
        if (!key.StartsWith("("))
        {
            var cl = Package.FindPackage("CL");
            if (cl != null)
            {
                var (sym, status) = cl.FindSymbol(key);
                if (status != SymbolStatus.None) RestoreFunctionIfWrapper(sym, wrapper, original);
            }
            var clUser = Package.FindPackage("CL-USER");
            if (clUser != null)
            {
                var (sym, status) = clUser.FindSymbol(key);
                if (status != SymbolStatus.None) RestoreFunctionIfWrapper(sym, wrapper, original);
            }
        }
    }

    private static LispObject TracedKeyToName(string key)
    {
        if (key.StartsWith("(SETF "))
        {
            var inner = key.Substring(6, key.Length - 7);
            return new Cons(Startup.Sym("SETF"), new Cons(Startup.Sym(inner), Nil.Instance));
        }
        return Startup.Sym(key);
    }

    private static TextWriter GetTraceOutputWriter()
    {
        var traceOutSym = Startup.Sym("*TRACE-OUTPUT*");
        if (DynamicBindings.TryGet(traceOutSym, out var val) && val is LispObject streamObj)
            return GetTextWriter(streamObj);
        if (traceOutSym.Value is LispObject globalStream)
            return GetTextWriter(globalStream);
        return Console.Error;
    }

    public static LispObject Ed(LispObject[] args)
    {
        if (args.Length > 1)
            throw new LispErrorException(new LispProgramError("ED: wrong number of arguments: " + args.Length + " (expected 0-1)"));
        return Nil.Instance;
    }

    public static LispObject Dribble(LispObject[] args)
    {
        if (args.Length > 1)
            throw new LispErrorException(new LispProgramError("DRIBBLE: wrong number of arguments: " + args.Length + " (expected 0-1)"));
        return Nil.Instance;
    }

    public static LispObject Inspect(LispObject[] args)
    {
        if (args.Length != 1)
            throw new LispErrorException(new LispProgramError("INSPECT: wrong number of arguments: " + args.Length + " (expected 1)"));
        // Minimal: print the object then return it (implementation-defined)
        var writer = GetStandardOutputWriter();
        writer.WriteLine(FormatObject(args[0], true));
        writer.Flush();
        return args[0];
    }

    // Documentation storage: keyed by (object identity hash + doc-type name)
    private static readonly ConcurrentDictionary<(int, string), LispObject> _documentationStore = new();

    public static LispObject GetDocumentation(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("DOCUMENTATION: requires 2 arguments"));
        string docType = args[1] is Symbol s ? s.Name : FormatObject(args[1], true);
        var key = (System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(args[0]), docType);
        return _documentationStore.TryGetValue(key, out var doc) ? doc : Nil.Instance;
    }

    public static LispObject SetDocumentation(LispObject[] args)
    {
        // args: new-value, object, doc-type
        if (args.Length < 3)
            throw new LispErrorException(new LispProgramError("(SETF DOCUMENTATION): requires 3 arguments"));
        var newValue = args[0];
        string docType = args[2] is Symbol s ? s.Name : FormatObject(args[2], true);
        var key = (System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(args[1]), docType);
        if (newValue is Nil)
            _documentationStore.TryRemove(key, out _);
        else
            _documentationStore[key] = newValue;
        return newValue;
    }
}
