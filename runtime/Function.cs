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
        => _func0 != null ? _func0() : InvokeSlow(Array.Empty<LispObject>());

    public LispObject Invoke1(LispObject a)
        => _func1 != null ? _func1(a) : InvokeSlow(new[] { a });

    public LispObject Invoke2(LispObject a, LispObject b)
        => _func2 != null ? _func2(a, b) : InvokeSlow(new[] { a, b });

    public LispObject Invoke3(LispObject a, LispObject b, LispObject c)
        => _func3 != null ? _func3(a, b, c) : InvokeSlow(new[] { a, b, c });

    public LispObject Invoke4(LispObject a, LispObject b, LispObject c, LispObject d)
        => _func4 != null ? _func4(a, b, c, d) : InvokeSlow(new[] { a, b, c, d });

    public LispObject Invoke5(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e)
        => _func5 != null ? _func5(a, b, c, d, e) : InvokeSlow(new[] { a, b, c, d, e });

    public LispObject Invoke6(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f)
        => _func6 != null ? _func6(a, b, c, d, e, f) : InvokeSlow(new[] { a, b, c, d, e, f });

    public LispObject Invoke7(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f, LispObject g)
        => _func7 != null ? _func7(a, b, c, d, e, f, g) : InvokeSlow(new[] { a, b, c, d, e, f, g });

    public LispObject Invoke8(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f, LispObject g, LispObject h)
        => _func8 != null ? _func8(a, b, c, d, e, f, g, h) : InvokeSlow(new[] { a, b, c, d, e, f, g, h });

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
        return _func(args);
    }

    public override string ToString() =>
        Name != null ? $"#<FUNCTION {Name}>" : "#<FUNCTION>";
}
