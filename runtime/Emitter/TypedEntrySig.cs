namespace DotCL.Emitter;

/// <summary>
/// The signature of a typed entry, as the compiler writes it: one letter per
/// argument and one for the result, "L" for a raw long (an integer type that
/// fits int64) and "D" for a raw double (DOUBLE-FLOAT), e.g. "LD>L". The entry
/// is a Func&lt;LispFunction, T1..Tn, R&gt; whose leading argument is the function
/// itself.
///
/// The code is what LispFunction keeps as the tag of the entry: bit 0x100 set,
/// the argument count in bits 5-7, the result kind in bit 4 and the argument
/// kinds in bits 0-3 (1 = double), so equal codes mean equal delegate types.
/// </summary>
public static class TypedEntrySig
{
    public const int MaxArgs = 4;

    public static bool IsValid(string sig)
    {
        int gt = sig.IndexOf('>');
        if (gt < 1 || gt > MaxArgs || gt != sig.Length - 2) return false;
        foreach (var ch in sig)
            if (ch != 'L' && ch != 'D' && ch != '>') return false;
        return true;
    }

    public static int Code(string sig)
    {
        if (!IsValid(sig)) throw new System.ArgumentException($"bad typed-entry signature {sig}");
        int n = sig.Length - 2;
        int code = 0x100 | (n << 5) | (sig[n + 1] == 'D' ? 0x10 : 0);
        for (int i = 0; i < n; i++)
            if (sig[i] == 'D') code |= 1 << i;
        return code;
    }

    public static string FromCode(int code)
    {
        int n = (code >> 5) & 7;
        var chars = new char[n + 2];
        for (int i = 0; i < n; i++) chars[i] = (code & (1 << i)) != 0 ? 'D' : 'L';
        chars[n] = '>';
        chars[n + 1] = (code & 0x10) != 0 ? 'D' : 'L';
        return new string(chars);
    }

    public static System.Type KindType(char k) => k == 'D' ? typeof(double) : typeof(long);

    /// <summary>Func&lt;LispFunction, T1..Tn, R&gt; for SIG.</summary>
    public static System.Type DelegateType(string sig)
    {
        int n = sig.Length - 2;
        var targs = new System.Type[n + 2];
        targs[0] = typeof(LispFunction);
        for (int i = 0; i < n; i++) targs[i + 1] = KindType(sig[i]);
        targs[n + 1] = KindType(sig[n + 1]);
        System.Type open = n switch
        {
            1 => typeof(System.Func<,,>),
            2 => typeof(System.Func<,,,>),
            3 => typeof(System.Func<,,,,>),
            4 => typeof(System.Func<,,,,,>),
            _ => throw new System.ArgumentException($"bad typed-entry signature {sig}")
        };
        return open.MakeGenericType(targs);
    }

    /// <summary>The parameter types of the body method: the function, then the raw arguments.</summary>
    public static System.Type[] ParamTypes(string sig)
    {
        int n = sig.Length - 2;
        var t = new System.Type[n + 1];
        t[0] = typeof(LispFunction);
        for (int i = 0; i < n; i++) t[i + 1] = KindType(sig[i]);
        return t;
    }

    public static System.Type ReturnType(string sig) => KindType(sig[sig.Length - 1]);

    /// <summary>Call the typed entry DEL of signature SIG with boxed ARGS, checking
    /// them as the shell does, and box the result. For diagnostics and tests: the
    /// compiled code never goes through here.</summary>
    public static LispObject InvokeBoxed(LispFunction fn, System.Delegate del, string sig, LispObject[] args)
    {
        int n = sig.Length - 2;
        var call = new object[n + 1];
        call[0] = fn;
        for (int i = 0; i < n; i++)
            call[i + 1] = sig[i] == 'D' ? (object)Runtime.TypedArgD(args[i]) : (object)Runtime.TypedArgL(args[i]);
        object? r;
        try { r = del.DynamicInvoke(call); }
        catch (System.Reflection.TargetInvocationException e) when (e.InnerException != null)
        {
            System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(e.InnerException).Throw();
            throw;
        }
        return sig[n + 1] == 'D' ? new DoubleFloat((double)r!) : Fixnum.Make((long)r!);
    }
}
