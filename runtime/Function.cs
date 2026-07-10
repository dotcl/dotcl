using System.Runtime.CompilerServices;

namespace DotCL;

public class LispFunction : LispObject
{
    private readonly Func<LispObject[], LispObject> _func;
    public string? Name { get; }
    public int Arity { get; }
    public Func<LispObject[], LispObject> RawFunction => _func;
    public object[]? Environment { get; internal set; }
    // Debug: SIL body stored when dotcl:*save-sil* is true at defun time
    public LispObject? Sil { get; internal set; }

    // Closure delegate: receives explicit env array
    private readonly Func<object[], LispObject[], LispObject>? _closureFunc;

    // S4: strong reference to this function's compilation-unit closure-DM
    // store (CilAssembler unit holder). Set on functions whose body builds
    // closures (the enclosing defun/lambda) and on the closures themselves, so a
    // unit's closure DynamicMethods stay alive exactly while some function that
    // can still call MakeClosure(unit) is reachable. When the last such function
    // dies, the holder dies and the off-GC-heap JIT code behind those DMs frees.
    // The global CilAssembler unit map only holds the holder weakly.
    internal object? RetainUnit;

    // Direct-param delegates for 0-8 arg fast path (set by assembler for simple functions)
    internal Func<LispObject>? _func0;
    internal Func<LispObject, LispObject>? _func1;
    internal Func<LispObject, LispObject, LispObject>? _func2;
    internal Func<LispObject, LispObject, LispObject, LispObject>? _func3;
    internal Func<LispObject, LispObject, LispObject, LispObject, LispObject>? _func4;
    internal Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>? _func5;
    internal Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>? _func6;
    internal Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>? _func7;
    internal Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>? _func8;

    // Native delegates: (self, long args) → LispObject return.
    // Avoids boxing of ARGUMENTS (the main allocation bottleneck in fixnum recursion).
    // Return is still LispObject so the body compiles unchanged.
    // The leading LispFunction is the function itself, threaded through so a native
    // self-call can reach the receiver from arg0 instead of re-resolving #'NAME from
    // its symbol on every recursive entry (the old per-call self-fn prelude).
    internal Func<LispFunction, long, LispObject>? _nativeFunc1;
    internal Func<LispFunction, long, long, LispObject>? _nativeFunc2;
    internal Func<LispFunction, long, long, long, LispObject>? _nativeFunc3;
    internal Func<LispFunction, long, long, long, long, LispObject>? _nativeFunc4;

    public LispFunction(Func<LispObject[], LispObject> func, string? name = null, int arity = -1)
    {
        _func = func;
        Name = name;
        Arity = arity;
        DotCL.Diagnostics.AllocCounter.Inc("LispFunction");
    }

    // Closure constructor: env is stored and passed explicitly on each call
    public LispFunction(Func<object[], LispObject[], LispObject> closureFunc,
                        object[] env, string? name = null, int arity = -1)
    {
        _closureFunc = closureFunc;
        Environment = env;
        _func = args => closureFunc(env, args);
        Name = name;
        Arity = arity;
        DotCL.Diagnostics.AllocCounter.Inc("LispFunction+Closure");
    }

    // Factory for direct-params closures (per-arity body delegates; built by
    // CilAssembler.MakeClosureDirect). The closure body DynamicMethod takes
    // (object[] env, LispObject a0..aN-1) instead of (object[] env,
    // LispObject[] args), so an exactly-N-arg InvokeN call runs the body without
    // the args-array InvokeSlow detour. The args-array _func wrapper keeps
    // apply / spread-arg calls working: it performs the same
    // Runtime.CheckArityExact the compiled args-array body used to perform
    // (identical error type and message), then spreads the array. The direct
    // _funcN path needs no check — the delegate signature structurally
    // guarantees the argc (an InvokeM call with M != N finds _funcM null and
    // falls back to the wrapper). fnName is captured only by the wrapper
    // lambda; Name stays null like every closure, so PushFrame behavior and
    // per-call cost are unchanged.
    public static LispFunction MakeDirectClosure(Delegate del, object[] env, string fnName)
    {
        // The _funcN wrappers include PeriodicStackCheck: unlike assembler-built
        // simple functions, a closure can recurse through itself via its own box
        // (funcall of a captured self-reference) with no named-call site in
        // between, and the InvokeN fast path skips InvokeSlow's check — without
        // this, runaway closure recursion dies as an uncatchable .NET
        // StackOverflowException instead of the catchable Lisp "Stack overflow"
        // PROGRAM-ERROR that the args-array path has always produced.
        LispFunction fn;
        switch (del)
        {
            case Func<object[], LispObject> d0:
            {
                var f = fn = new LispFunction(args => { Runtime.CheckArityExact(fnName, args, 0); return d0(env); }, null, 0);
                fn._func0 = () => { f.PeriodicStackCheck(); return d0(env); };
                break;
            }
            case Func<object[], LispObject, LispObject> d1:
            {
                var f = fn = new LispFunction(args => { Runtime.CheckArityExact(fnName, args, 1); return d1(env, args[0]); }, null, 1);
                fn._func1 = a => { f.PeriodicStackCheck(); return d1(env, a); };
                break;
            }
            case Func<object[], LispObject, LispObject, LispObject> d2:
            {
                var f = fn = new LispFunction(args => { Runtime.CheckArityExact(fnName, args, 2); return d2(env, args[0], args[1]); }, null, 2);
                fn._func2 = (a, b) => { f.PeriodicStackCheck(); return d2(env, a, b); };
                break;
            }
            case Func<object[], LispObject, LispObject, LispObject, LispObject> d3:
            {
                var f = fn = new LispFunction(args => { Runtime.CheckArityExact(fnName, args, 3); return d3(env, args[0], args[1], args[2]); }, null, 3);
                fn._func3 = (a, b, c) => { f.PeriodicStackCheck(); return d3(env, a, b, c); };
                break;
            }
            case Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject> d4:
            {
                var f = fn = new LispFunction(args => { Runtime.CheckArityExact(fnName, args, 4); return d4(env, args[0], args[1], args[2], args[3]); }, null, 4);
                fn._func4 = (a, b, c, d) => { f.PeriodicStackCheck(); return d4(env, a, b, c, d); };
                break;
            }
            case Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> d5:
            {
                var f = fn = new LispFunction(args => { Runtime.CheckArityExact(fnName, args, 5); return d5(env, args[0], args[1], args[2], args[3], args[4]); }, null, 5);
                fn._func5 = (a, b, c, d, e) => { f.PeriodicStackCheck(); return d5(env, a, b, c, d, e); };
                break;
            }
            case Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> d6:
            {
                var f = fn = new LispFunction(args => { Runtime.CheckArityExact(fnName, args, 6); return d6(env, args[0], args[1], args[2], args[3], args[4], args[5]); }, null, 6);
                fn._func6 = (a, b, c, d, e, f2) => { f.PeriodicStackCheck(); return d6(env, a, b, c, d, e, f2); };
                break;
            }
            default:
                throw new ArgumentException($"MakeDirectClosure: unsupported delegate type {del.GetType().Name}");
        }
        fn.Environment = env;
        return fn;
    }

    // Lisp-level call stack for debugger backtrace. Each frame keeps the callee
    // name plus its arguments. To preserve the alloc-free push on the hot path,
    // Frame is a struct stored inline in Stack<Frame>'s backing array, with up to
    // four arguments inline; only 5+ argument calls (rare) reference an array.
    internal readonly struct Frame
    {
        public readonly string Name;
        public readonly int Argc;
        private readonly LispObject? _a0, _a1, _a2, _a3;
        private readonly LispObject[]? _rest; // non-null when args came as an array

        public Frame(string name)
        { Name = name; Argc = 0; _a0 = _a1 = _a2 = _a3 = null; _rest = null; }
        public Frame(string name, LispObject a0)
        { Name = name; Argc = 1; _a0 = a0; _a1 = _a2 = _a3 = null; _rest = null; }
        public Frame(string name, LispObject a0, LispObject a1)
        { Name = name; Argc = 2; _a0 = a0; _a1 = a1; _a2 = _a3 = null; _rest = null; }
        public Frame(string name, LispObject a0, LispObject a1, LispObject a2)
        { Name = name; Argc = 3; _a0 = a0; _a1 = a1; _a2 = a2; _a3 = null; _rest = null; }
        public Frame(string name, LispObject a0, LispObject a1, LispObject a2, LispObject a3)
        { Name = name; Argc = 4; _a0 = a0; _a1 = a1; _a2 = a2; _a3 = a3; _rest = null; }
        public Frame(string name, LispObject[] args)
        { Name = name; Argc = args.Length; _a0 = _a1 = _a2 = _a3 = null; _rest = args; }

        public LispObject? Arg(int i)
        {
            if (_rest != null) return (uint)i < (uint)_rest.Length ? _rest[i] : null;
            return i switch { 0 => _a0, 1 => _a1, 2 => _a2, 3 => _a3, _ => null };
        }
    }

    [ThreadStatic] private static Stack<Frame>? s_callStack;

    /// <summary>Backtrace as callee-name strings, innermost first. Used by the
    /// programmatic DOTCL:BACKTRACE.</summary>
    internal static string[] GetCallStack()
    {
        if (s_callStack is not { Count: > 0 } s) return Array.Empty<string>();
        var frames = s.ToArray();
        var result = new string[frames.Length];
        for (int i = 0; i < frames.Length; i++) result[i] = frames[i].Name;
        return result;
    }

    /// <summary>Backtrace as printed call forms "(NAME arg1 arg2 ...)", innermost
    /// first. Used by the :bt debugger command and DOTCL:PRINT-BACKTRACE. Argument
    /// rendering happens here (off the call hot path) and is bounded/cycle-safe.</summary>
    internal static string[] GetCallStackForms()
    {
        if (s_callStack is not { Count: > 0 } s) return Array.Empty<string>();
        var frames = s.ToArray();
        var result = new string[frames.Length];
        for (int i = 0; i < frames.Length; i++) result[i] = FormatFrame(frames[i]);
        return result;
    }

    /// <summary>Backtrace frames as Lisp lists (NAME arg0 arg1 ...), innermost
    /// first, where the args are the ACTUAL captured LispObjects (not printed
    /// strings). Backs DOTCL:BACKTRACE-WITH-ARGS so callers can inspect frame
    /// arguments programmatically (cf. sb-debug:list-backtrace).</summary>
    internal static LispObject[] GetCallStackWithArgs()
    {
        if (s_callStack is not { Count: > 0 } s) return Array.Empty<LispObject>();
        var frames = s.ToArray();
        var result = new LispObject[frames.Length];
        for (int i = 0; i < frames.Length; i++)
        {
            var f = frames[i];
            LispObject args = Nil.Instance;
            for (int j = f.Argc - 1; j >= 0; j--)
                args = new Cons(f.Arg(j) ?? Nil.Instance, args);
            result[i] = new Cons(new LispString(f.Name), args);
        }
        return result;
    }

    private static string FormatFrame(Frame f)
    {
        if (f.Argc == 0) return "(" + f.Name + ")";
        var sb = new System.Text.StringBuilder(32);
        sb.Append('(').Append(f.Name);
        for (int i = 0; i < f.Argc; i++)
        {
            sb.Append(' ');
            var a = f.Arg(i);
            sb.Append(a == null ? "?" : Runtime.BacktraceArgString(a));
        }
        return sb.Append(')').ToString();
    }

    // Backward compat: existing Generated.cs uses Invoke(params)
    // Includes stack overflow guard for C#-implemented functions that can recurse via Lisp dispatch
    [ThreadStatic] private static int _stackCheckCounter;
    public LispObject Invoke(params LispObject[] args)
    {
        if (++_stackCheckCounter % 256 == 0)
        {
            if (!Compat.TryEnsureSufficientExecutionStack())
                throw new LispErrorException(new LispProgramError(
                    $"Stack overflow in function {Name ?? "anonymous"}"));
            ConditionSystem.CheckInterrupt();
        }
        return _func(args);
    }

    // Direct-param invoke: avoids array allocation when _funcN is set.
    // Fallback paths through _func include periodic stack overflow check
    // to prevent uncatchable .NET StackOverflowException from recursive macros.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void PeriodicStackCheck()
    {
        if (++_stackCheckCounter % 256 == 0)
        {
            if (!Compat.TryEnsureSufficientExecutionStack())
                throw new LispErrorException(new LispProgramError(
                    $"Stack overflow in function {Name ?? "anonymous"}"));
            ConditionSystem.CheckInterrupt();
        }
    }

    // Push the current function name onto the debugger call stack and return a
    // scope whose Dispose pops it. Struct + `using` keeps this alloc-free (no
    // closure, no boxing); anonymous functions (Name == null) skip the stack.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private FrameScope PushFrame()
    {
        if (Name == null) return default;
        (s_callStack ??= new Stack<Frame>()).Push(new Frame(Name));
        return new FrameScope(true);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private FrameScope PushFrame(LispObject a)
    {
        if (Name == null) return default;
        (s_callStack ??= new Stack<Frame>()).Push(new Frame(Name, a));
        return new FrameScope(true);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private FrameScope PushFrame(LispObject a, LispObject b)
    {
        if (Name == null) return default;
        (s_callStack ??= new Stack<Frame>()).Push(new Frame(Name, a, b));
        return new FrameScope(true);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private FrameScope PushFrame(LispObject a, LispObject b, LispObject c)
    {
        if (Name == null) return default;
        (s_callStack ??= new Stack<Frame>()).Push(new Frame(Name, a, b, c));
        return new FrameScope(true);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private FrameScope PushFrame(LispObject a, LispObject b, LispObject c, LispObject d)
    {
        if (Name == null) return default;
        (s_callStack ??= new Stack<Frame>()).Push(new Frame(Name, a, b, c, d));
        return new FrameScope(true);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private FrameScope PushFrame(LispObject[] args)
    {
        if (Name == null) return default;
        (s_callStack ??= new Stack<Frame>()).Push(new Frame(Name, args));
        return new FrameScope(true);
    }

    private readonly struct FrameScope : IDisposable
    {
        private readonly bool _pushed;
        public FrameScope(bool pushed) { _pushed = pushed; }
        public void Dispose() { if (_pushed) s_callStack!.TryPop(out _); }
    }

    public LispObject Invoke0()
    {
        if (_func0 != null) { using (PushFrame()) return _func0(); }
        return InvokeSlow(Array.Empty<LispObject>());
    }

    public LispObject Invoke1(LispObject a)
    {
        if (_func1 != null) { using (PushFrame(a)) return _func1(a); }
        return InvokeSlow(new[] { a });
    }

    public LispObject Invoke2(LispObject a, LispObject b)
    {
        if (_func2 != null) { using (PushFrame(a, b)) return _func2(a, b); }
        return InvokeSlow(new[] { a, b });
    }

    public LispObject Invoke3(LispObject a, LispObject b, LispObject c)
    {
        if (_func3 != null) { using (PushFrame(a, b, c)) return _func3(a, b, c); }
        return InvokeSlow(new[] { a, b, c });
    }

    public LispObject Invoke4(LispObject a, LispObject b, LispObject c, LispObject d)
    {
        if (_func4 != null) { using (PushFrame(a, b, c, d)) return _func4(a, b, c, d); }
        return InvokeSlow(new[] { a, b, c, d });
    }

    public LispObject Invoke5(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e)
    {
        if (_func5 != null) { var args = new[] { a, b, c, d, e }; using (PushFrame(args)) return _func5(a, b, c, d, e); }
        return InvokeSlow(new[] { a, b, c, d, e });
    }

    public LispObject Invoke6(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f)
    {
        if (_func6 != null) { var args = new[] { a, b, c, d, e, f }; using (PushFrame(args)) return _func6(a, b, c, d, e, f); }
        return InvokeSlow(new[] { a, b, c, d, e, f });
    }

    public LispObject Invoke7(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f, LispObject g)
    {
        if (_func7 != null) { var args = new[] { a, b, c, d, e, f, g }; using (PushFrame(args)) return _func7(a, b, c, d, e, f, g); }
        return InvokeSlow(new[] { a, b, c, d, e, f, g });
    }

    public LispObject Invoke8(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f, LispObject g, LispObject h)
    {
        if (_func8 != null) { var args = new[] { a, b, c, d, e, f, g, h }; using (PushFrame(args)) return _func8(a, b, c, d, e, f, g, h); }
        return InvokeSlow(new[] { a, b, c, d, e, f, g, h });
    }

    // Native fixnum invoke: long args avoid boxing, LispObject return is body result
    public LispObject InvokeNative1(long a) => _nativeFunc1!(this, a);
    public LispObject InvokeNative2(long a, long b) => _nativeFunc2!(this, a, b);
    public LispObject InvokeNative3(long a, long b, long c) => _nativeFunc3!(this, a, b, c);
    public LispObject InvokeNative4(long a, long b, long c, long d) => _nativeFunc4!(this, a, b, c, d);

    // Install a native long→LispObject delegate for the appropriate arity.
    public void SetNativeDelegate(Delegate del)
    {
        switch (del)
        {
            case Func<LispFunction, long, LispObject> f1: _nativeFunc1 = f1; break;
            case Func<LispFunction, long, long, LispObject> f2: _nativeFunc2 = f2; break;
            case Func<LispFunction, long, long, long, LispObject> f3: _nativeFunc3 = f3; break;
            case Func<LispFunction, long, long, long, long, LispObject> f4: _nativeFunc4 = f4; break;
            default: throw new ArgumentException($"SetNativeDelegate: unsupported type {del.GetType().Name}");
        }
    }

    // Install a typed direct-call delegate for the appropriate arity.
    // Public so FASL-emitted code (in a separate assembly) can bypass the
    // internal field visibility without extra reflection hops.
    public void SetDirectDelegate(Delegate del)
    {
        switch (del)
        {
            case Func<LispObject> f0: _func0 = f0; break;
            case Func<LispObject, LispObject> f1: _func1 = f1; break;
            case Func<LispObject, LispObject, LispObject> f2: _func2 = f2; break;
            case Func<LispObject, LispObject, LispObject, LispObject> f3: _func3 = f3; break;
            case Func<LispObject, LispObject, LispObject, LispObject, LispObject> f4: _func4 = f4; break;
            case Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> f5: _func5 = f5; break;
            case Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> f6: _func6 = f6; break;
            case Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> f7: _func7 = f7; break;
            case Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> f8: _func8 = f8; break;
            default:
                throw new ArgumentException($"SetDirectDelegate: unsupported delegate type {del.GetType().Name}");
        }
    }

    // --- InvokeSlow call statistics (opt-in diagnostic) ---
    // Counts InvokeSlow entries per (callee name, argc) so the fast-path gap
    // (functions still going through the args-array _func) can be measured.
    // Off by default: the only cost on the hot path is a single branch.
    // Lisp API: dotcl:collect-invoke-stats / dotcl:invoke-slow-stats /
    // dotcl:reset-invoke-slow-stats (registered in Startup.cs).
    internal static bool CollectInvokeStats;
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<(string Name, int Argc), long>
        s_invokeSlowStats = new();

    internal static void ResetInvokeSlowStats() => s_invokeSlowStats.Clear();

    /// <summary>Snapshot of the InvokeSlow counters, sorted by count descending.</summary>
    internal static List<KeyValuePair<(string Name, int Argc), long>> InvokeSlowStatsSnapshot()
    {
        var list = new List<KeyValuePair<(string Name, int Argc), long>>(s_invokeSlowStats.Count);
        foreach (var kv in s_invokeSlowStats) list.Add(kv);
        list.Sort((a, b) => b.Value.CompareTo(a.Value));
        return list;
    }

    private LispObject InvokeSlow(LispObject[] args)
    {
        if (CollectInvokeStats)
            s_invokeSlowStats.AddOrUpdate((Name ?? AnonOriginTag(), args.Length), 1,
                                          static (_, c) => c + 1);
        PeriodicStackCheck();
        if (Name == null) return _func(args);
        (s_callStack ??= new Stack<Frame>()).Push(new Frame(Name, args));
        try { return _func(args); }
        finally { s_callStack.TryPop(out _); }
    }

    // Origin tag for anonymous functions in the InvokeSlow statistics. The
    // backing method's name identifies the generation site: DynamicMethod
    // names are chosen per emitter site ("lambda", "lambda_direct",
    // "lambda_closure", "lambda_closure_direct", FASL "closure_N", ...), and
    // C# lambdas carry a compiler-generated name embedding the enclosing
    // method (e.g. "<MakeHandlerCaseFunction>b__1_0"). Digits are stripped so
    // per-instance names (closure_42) collapse into one statistics key.
    // Only called with CollectInvokeStats enabled — no cost otherwise.
    private string AnonOriginTag()
    {
        var m = _closureFunc != null ? _closureFunc.Method : _func.Method;
        var sb = new System.Text.StringBuilder("<anon:");
        foreach (var ch in m.Name)
            if (!char.IsDigit(ch)) sb.Append(ch);
        return sb.Append('>').ToString();
    }

    public (Delegate Delegate, string Label) GetJitDelegate()
    {
        if (_nativeFunc1 != null) return (_nativeFunc1, "native-1");
        if (_nativeFunc2 != null) return (_nativeFunc2, "native-2");
        if (_nativeFunc3 != null) return (_nativeFunc3, "native-3");
        if (_nativeFunc4 != null) return (_nativeFunc4, "native-4");
        if (_func1 != null) return (_func1, "func-1");
        if (_func2 != null) return (_func2, "func-2");
        if (_func3 != null) return (_func3, "func-3");
        if (_func4 != null) return (_func4, "func-4");
        if (_func5 != null) return (_func5, "func-5");
        if (_func6 != null) return (_func6, "func-6");
        if (_func7 != null) return (_func7, "func-7");
        if (_func8 != null) return (_func8, "func-8");
        return (_func, "func");
    }

    public override string ToString() =>
        Name != null ? $"#<FUNCTION {Name}>" : "#<FUNCTION>";
}
