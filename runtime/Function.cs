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

    // Native delegates: long args → LispObject return.
    // Avoids boxing of ARGUMENTS (the main allocation bottleneck in fixnum recursion).
    // Return is still LispObject so the body compiles unchanged (#130).
    internal Func<long, LispObject>? _nativeFunc1;
    internal Func<long, long, LispObject>? _nativeFunc2;
    internal Func<long, long, long, LispObject>? _nativeFunc3;
    internal Func<long, long, long, long, LispObject>? _nativeFunc4;

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

    // Lisp-level call stack for debugger backtrace
    [ThreadStatic] private static Stack<string>? s_callStack;
    internal static string[] GetCallStack() =>
        s_callStack is { Count: > 0 } s ? s.ToArray() : Array.Empty<string>();

    // Backward compat: existing Generated.cs uses Invoke(params)
    // Includes stack overflow guard for C#-implemented functions that can recurse via Lisp dispatch
    [ThreadStatic] private static int _stackCheckCounter;
    public LispObject Invoke(params LispObject[] args)
    {
        if (++_stackCheckCounter % 256 == 0)
        {
            if (!RuntimeHelpers.TryEnsureSufficientExecutionStack())
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
            if (!RuntimeHelpers.TryEnsureSufficientExecutionStack())
                throw new LispErrorException(new LispProgramError(
                    $"Stack overflow in function {Name ?? "anonymous"}"));
            ConditionSystem.CheckInterrupt();
        }
    }

    public LispObject Invoke0()
    {
        if (_func0 != null) return Track(_func0);
        return InvokeSlow(Array.Empty<LispObject>());
    }

    public LispObject Invoke1(LispObject a)
    {
        if (_func1 != null) return Track(() => _func1(a));
        return InvokeSlow(new[] { a });
    }

    public LispObject Invoke2(LispObject a, LispObject b)
    {
        if (_func2 != null) return Track(() => _func2(a, b));
        return InvokeSlow(new[] { a, b });
    }

    public LispObject Invoke3(LispObject a, LispObject b, LispObject c)
    {
        if (_func3 != null) return Track(() => _func3(a, b, c));
        return InvokeSlow(new[] { a, b, c });
    }

    public LispObject Invoke4(LispObject a, LispObject b, LispObject c, LispObject d)
    {
        if (_func4 != null) return Track(() => _func4(a, b, c, d));
        return InvokeSlow(new[] { a, b, c, d });
    }

    public LispObject Invoke5(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e)
    {
        if (_func5 != null) return Track(() => _func5(a, b, c, d, e));
        return InvokeSlow(new[] { a, b, c, d, e });
    }

    public LispObject Invoke6(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f)
    {
        if (_func6 != null) return Track(() => _func6(a, b, c, d, e, f));
        return InvokeSlow(new[] { a, b, c, d, e, f });
    }

    public LispObject Invoke7(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f, LispObject g)
    {
        if (_func7 != null) return Track(() => _func7(a, b, c, d, e, f, g));
        return InvokeSlow(new[] { a, b, c, d, e, f, g });
    }

    public LispObject Invoke8(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f, LispObject g, LispObject h)
    {
        if (_func8 != null) return Track(() => _func8(a, b, c, d, e, f, g, h));
        return InvokeSlow(new[] { a, b, c, d, e, f, g, h });
    }

    private LispObject Track(Func<LispObject> call)
    {
        if (Name == null) return call();
        (s_callStack ??= new Stack<string>()).Push(Name);
        try { return call(); }
        finally { s_callStack.TryPop(out _); }
    }

    // Native fixnum invoke: long args avoid boxing, LispObject return is body result (#130)
    public LispObject InvokeNative1(long a) => _nativeFunc1!(a);
    public LispObject InvokeNative2(long a, long b) => _nativeFunc2!(a, b);
    public LispObject InvokeNative3(long a, long b, long c) => _nativeFunc3!(a, b, c);
    public LispObject InvokeNative4(long a, long b, long c, long d) => _nativeFunc4!(a, b, c, d);

    // Install a native long→LispObject delegate for the appropriate arity.
    public void SetNativeDelegate(Delegate del)
    {
        switch (del)
        {
            case Func<long, LispObject> f1: _nativeFunc1 = f1; break;
            case Func<long, long, LispObject> f2: _nativeFunc2 = f2; break;
            case Func<long, long, long, LispObject> f3: _nativeFunc3 = f3; break;
            case Func<long, long, long, long, LispObject> f4: _nativeFunc4 = f4; break;
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
        (s_callStack ??= new Stack<string>()).Push(Name);
        try { return _func(args); }
        finally { s_callStack.TryPop(out _); }
    }

    public override string ToString() =>
        Name != null ? $"#<FUNCTION {Name}>" : "#<FUNCTION>";
}
