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

    private LispObject InvokeSlow(LispObject[] args)
    {
        PeriodicStackCheck();
        if (Name == null) return _func(args);
        (s_callStack ??= new Stack<Frame>()).Push(new Frame(Name, args));
        try { return _func(args); }
        finally { s_callStack.TryPop(out _); }
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
