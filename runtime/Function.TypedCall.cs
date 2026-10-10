using System.Runtime.CompilerServices;

#pragma warning disable CS8500

namespace DotCL;

/// <summary>The raw arguments of a call made through a typed entry, kept beside
/// the debugger frame that call pushes (Frame.TypedArgsBit): the frame shows them
/// boxed only when a backtrace asks. CODE is the signature code, which says which
/// of them are doubles.</summary>
public struct TypedFrameArgs
{
    internal long R0, R1, R2, R3;
    internal int Code;

    internal LispObject Box(int i)
    {
        long bits = i switch { 0 => R0, 1 => R1, 2 => R2, _ => R3 };
        return (Code & (1 << i)) != 0
            ? new DoubleFloat(System.BitConverter.Int64BitsToDouble(bits))
            : Fixnum.Make(bits);
    }
}

// Calls from compiled code to a global function through its typed entry. The
// caller knows the callee's FTYPE, computes the arguments as raw long / double
// and calls InvokeTypedN with the signature code it was compiled against. When
// the function it finds under the name has a typed entry of that code, the body
// method runs on the raw values, under the debugger frame InvokeN would push
// (named the same, its arguments boxed only if a backtrace reads them). Any
// other function (redefined without the FTYPE, redefined with other types,
// interpreted, traced) gets the boxed arguments through InvokeN, and its result
// is taken back to the raw kind the caller was compiled for (a value of another
// type is a TYPE-ERROR: the caller relied on the declaration).
//
// The arguments are not checked here: the caller checked them against the
// declaration it was compiled with (from (safety 1) up), as the shell checks
// them for a boxed caller.
public partial class LispFunction
{
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static long RawBits<T>(T v) where T : unmanaged => Unsafe.As<T, long>(ref v);

    static LispObject BoxRaw<T>(T v) where T : unmanaged =>
        typeof(T) == typeof(double)
            ? new DoubleFloat(Unsafe.As<T, double>(ref v))
            : Fixnum.Make(Unsafe.As<T, long>(ref v));

    static R UnboxRaw<R>(LispObject v) where R : unmanaged
    {
        if (typeof(R) == typeof(double)) { double d = Runtime.TypedRetD(v); return Unsafe.As<double, R>(ref d); }
        long l = Runtime.TypedRetL(v);
        return Unsafe.As<long, R>(ref l);
    }

    public static unsafe R InvokeTyped1<A0, R>(LispFunction fn, A0 a0, int code)
        where A0 : unmanaged where R : unmanaged
    {
        if (fn._nativeArity != code) return UnboxRaw<R>(fn.Invoke1(BoxRaw(a0)));
        fn.PeriodicStackCheck();
        var d = Unsafe.As<System.Func<LispFunction, A0, R>>(fn._nativeDel!);
        var n = fn._frameName;
        if (n == null)
        {
            if (fn.RetainUnit == null) return d(fn, a0);
            if (s_top == null) return UnderKeeperTyped1<A0, R>(fn, a0, code);
            var r = d(fn, a0); System.GC.KeepAlive(fn); return r;
        }
        TypedFrameArgs ta; ta.R0 = RawBits(a0); ta.R1 = ta.R2 = ta.R3 = 0; ta.Code = code;
        var fr = new Frame(n, 1, &ta); Link(&fr); fr._unit = fn.RetainUnit;
        try { return d(fn, a0); } finally { s_top = fr.Prev; }
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static unsafe R UnderKeeperTyped1<A0, R>(LispFunction fn, A0 a0, int code)
        where A0 : unmanaged where R : unmanaged
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return InvokeTyped1<A0, R>(fn, a0, code); } finally { s_top = kf.Prev; }
    }

    public static unsafe R InvokeTyped2<A0, A1, R>(LispFunction fn, A0 a0, A1 a1, int code)
        where A0 : unmanaged where A1 : unmanaged where R : unmanaged
    {
        if (fn._nativeArity != code) return UnboxRaw<R>(fn.Invoke2(BoxRaw(a0), BoxRaw(a1)));
        fn.PeriodicStackCheck();
        var d = Unsafe.As<System.Func<LispFunction, A0, A1, R>>(fn._nativeDel!);
        var n = fn._frameName;
        if (n == null)
        {
            if (fn.RetainUnit == null) return d(fn, a0, a1);
            if (s_top == null) return UnderKeeperTyped2<A0, A1, R>(fn, a0, a1, code);
            var r = d(fn, a0, a1); System.GC.KeepAlive(fn); return r;
        }
        TypedFrameArgs ta; ta.R0 = RawBits(a0); ta.R1 = RawBits(a1); ta.R2 = ta.R3 = 0; ta.Code = code;
        var fr = new Frame(n, 2, &ta); Link(&fr); fr._unit = fn.RetainUnit;
        try { return d(fn, a0, a1); } finally { s_top = fr.Prev; }
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static unsafe R UnderKeeperTyped2<A0, A1, R>(LispFunction fn, A0 a0, A1 a1, int code)
        where A0 : unmanaged where A1 : unmanaged where R : unmanaged
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return InvokeTyped2<A0, A1, R>(fn, a0, a1, code); } finally { s_top = kf.Prev; }
    }

    public static unsafe R InvokeTyped3<A0, A1, A2, R>(LispFunction fn, A0 a0, A1 a1, A2 a2, int code)
        where A0 : unmanaged where A1 : unmanaged where A2 : unmanaged where R : unmanaged
    {
        if (fn._nativeArity != code) return UnboxRaw<R>(fn.Invoke3(BoxRaw(a0), BoxRaw(a1), BoxRaw(a2)));
        fn.PeriodicStackCheck();
        var d = Unsafe.As<System.Func<LispFunction, A0, A1, A2, R>>(fn._nativeDel!);
        var n = fn._frameName;
        if (n == null)
        {
            if (fn.RetainUnit == null) return d(fn, a0, a1, a2);
            if (s_top == null) return UnderKeeperTyped3<A0, A1, A2, R>(fn, a0, a1, a2, code);
            var r = d(fn, a0, a1, a2); System.GC.KeepAlive(fn); return r;
        }
        TypedFrameArgs ta; ta.R0 = RawBits(a0); ta.R1 = RawBits(a1); ta.R2 = RawBits(a2); ta.R3 = 0; ta.Code = code;
        var fr = new Frame(n, 3, &ta); Link(&fr); fr._unit = fn.RetainUnit;
        try { return d(fn, a0, a1, a2); } finally { s_top = fr.Prev; }
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static unsafe R UnderKeeperTyped3<A0, A1, A2, R>(LispFunction fn, A0 a0, A1 a1, A2 a2, int code)
        where A0 : unmanaged where A1 : unmanaged where A2 : unmanaged where R : unmanaged
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return InvokeTyped3<A0, A1, A2, R>(fn, a0, a1, a2, code); } finally { s_top = kf.Prev; }
    }

    public static unsafe R InvokeTyped4<A0, A1, A2, A3, R>(LispFunction fn, A0 a0, A1 a1, A2 a2, A3 a3, int code)
        where A0 : unmanaged where A1 : unmanaged where A2 : unmanaged where A3 : unmanaged where R : unmanaged
    {
        if (fn._nativeArity != code)
            return UnboxRaw<R>(fn.Invoke4(BoxRaw(a0), BoxRaw(a1), BoxRaw(a2), BoxRaw(a3)));
        fn.PeriodicStackCheck();
        var d = Unsafe.As<System.Func<LispFunction, A0, A1, A2, A3, R>>(fn._nativeDel!);
        var n = fn._frameName;
        if (n == null)
        {
            if (fn.RetainUnit == null) return d(fn, a0, a1, a2, a3);
            if (s_top == null) return UnderKeeperTyped4<A0, A1, A2, A3, R>(fn, a0, a1, a2, a3, code);
            var r = d(fn, a0, a1, a2, a3); System.GC.KeepAlive(fn); return r;
        }
        TypedFrameArgs ta; ta.R0 = RawBits(a0); ta.R1 = RawBits(a1); ta.R2 = RawBits(a2); ta.R3 = RawBits(a3); ta.Code = code;
        var fr = new Frame(n, 4, &ta); Link(&fr); fr._unit = fn.RetainUnit;
        try { return d(fn, a0, a1, a2, a3); } finally { s_top = fr.Prev; }
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static unsafe R UnderKeeperTyped4<A0, A1, A2, A3, R>(LispFunction fn, A0 a0, A1 a1, A2 a2, A3 a3, int code)
        where A0 : unmanaged where A1 : unmanaged where A2 : unmanaged where A3 : unmanaged where R : unmanaged
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return InvokeTyped4<A0, A1, A2, A3, R>(fn, a0, a1, a2, a3, code); } finally { s_top = kf.Prev; }
    }

    // ---- Tail calls through a typed entry ----
    //
    // A typed body whose result is the result of a call to another function's
    // typed entry with the same result kind makes that call a tail call, as the
    // ordinary tail calls do (TailPrepN): TailPrepTypedN decides, and the body
    // then calls the callee's typed body with the IL tail. prefix
    // (TailKindDirect), or calls InvokeTypedN with it (TailKindInvoke: the
    // callee has no typed entry of that code, or needs a frame the caller
    // cannot give it). The innermost frame, when it is the calling body's own,
    // is made to stand for the callee when a typed call pushed it: its raw
    // argument storage takes the new arguments.

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private unsafe int TailPrepTypedCore(int code, string? self, int argc,
                                         long r0, long r1, long r2, long r3)
    {
        if (_nativeArity != code) return TailKindInvoke;
        ConditionSystem.PollInterrupt();
        var top = s_top;
        var callee = _frameName;
        bool owned = self != null && top != null
            && (self.Length == 0 ? top->Lent : ReferenceEquals(top->Name, self) || string.Equals(top->Name, self));
        var unit = RetainUnit;
        if (unit != null && top == null) return TailKindInvoke;
        if (callee != null)
        {
            if (!owned) return TailKindInvoke;
            // The frame keeps raw arguments only when a typed call pushed it.
            // Any other frame would need the arguments boxed on every call of
            // the chain: the callee gets a typed frame of its own instead
            // (InvokeTypedN), once, and the calls after it reuse that one.
            if ((top->_argc & Frame.TypedArgsBit) == 0) return TailKindInvoke;
            var ta = (TypedFrameArgs*)top->_ext;
            ta->R0 = r0; ta->R1 = r1; ta->R2 = r2; ta->R3 = r3; ta->Code = code;
            top->Name = callee;
            top->_argc = argc | Frame.TypedArgsBit;
        }
        else if (owned)
            top->Lend();
        if (unit != null) top->KeepUnit(unit);
        return TailKindDirect;
    }

    public static int TailPrepTyped1<A0>(LispFunction callee, int code, string? self, A0 a0)
        where A0 : unmanaged =>
        callee.TailPrepTypedCore(code, self, 1, RawBits(a0), 0, 0, 0);
    public static int TailPrepTyped2<A0, A1>(LispFunction callee, int code, string? self, A0 a0, A1 a1)
        where A0 : unmanaged where A1 : unmanaged =>
        callee.TailPrepTypedCore(code, self, 2, RawBits(a0), RawBits(a1), 0, 0);
    public static int TailPrepTyped3<A0, A1, A2>(LispFunction callee, int code, string? self, A0 a0, A1 a1, A2 a2)
        where A0 : unmanaged where A1 : unmanaged where A2 : unmanaged =>
        callee.TailPrepTypedCore(code, self, 3, RawBits(a0), RawBits(a1), RawBits(a2), 0);
    public static int TailPrepTyped4<A0, A1, A2, A3>(LispFunction callee, int code, string? self, A0 a0, A1 a1, A2 a2, A3 a3)
        where A0 : unmanaged where A1 : unmanaged where A2 : unmanaged where A3 : unmanaged =>
        callee.TailPrepTypedCore(code, self, 4, RawBits(a0), RawBits(a1), RawBits(a2), RawBits(a3));

    /// <summary>The typed body delegate, after TailPrepTypedN answered TailKindDirect.</summary>
    public static D TailTypedDelegate<D>(LispFunction fn) where D : class => Unsafe.As<D>(fn._nativeDel!);

    /// <summary>The InvokeTypedN instantiation for the typed-entry signature SIG
    /// ("LD>L"): R InvokeTypedN&lt;T1..Tn, R&gt;(LispFunction, T1..Tn, int code).</summary>
    public static System.Reflection.MethodInfo TypedInvokeMethod(string sig)
    {
        int n = sig.Length - 2;
        var targs = new System.Type[n + 1];
        for (int i = 0; i < n; i++) targs[i] = DotCL.Emitter.TypedEntrySig.KindType(sig[i]);
        targs[n] = DotCL.Emitter.TypedEntrySig.KindType(sig[n + 1]);
        return typeof(LispFunction).GetMethod("InvokeTyped" + n)!.MakeGenericMethod(targs);
    }
}
