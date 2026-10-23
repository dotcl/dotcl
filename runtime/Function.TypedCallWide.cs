using System.Runtime.CompilerServices;

#pragma warning disable CS8500

namespace DotCL;

/// <summary>The arguments of a call through a wide typed entry (a signature with
/// an object argument or result, or more than four arguments), kept beside the
/// debugger frame that call pushes (Frame.TypedWideBit): raw arguments as bits,
/// object arguments as they are. CODE says which is which.</summary>
public struct TypedFrameArgsW
{
    internal long R0, R1, R2, R3, R4, R5;
    internal LispObject? X0, X1, X2, X3, X4, X5;
    internal int Code;

    internal LispObject Box(int i)
    {
        if (Emitter.TypedEntrySig.IsObjectArg(Code, i))
            return (i switch { 0 => X0, 1 => X1, 2 => X2, 3 => X3, 4 => X4, _ => X5 }) ?? Nil.Instance;
        long bits = i switch { 0 => R0, 1 => R1, 2 => R2, 3 => R3, 4 => R4, _ => R5 };
        return Emitter.TypedEntrySig.IsDoubleArg(Code, i)
            ? new DoubleFloat(System.BitConverter.Int64BitsToDouble(bits))
            : Fixnum.Make(bits);
    }
}

// The calls through a wide typed entry: as InvokeTypedN / CallTypedN /
// CallTypedSymN / EnterTypedN / TailPrepTypedN (Function.TypedCall.cs), with
// each argument and the result a long, a double or a LispObject, up to six
// arguments.
public partial class LispFunction
{
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static void PutW<T>(ref long r, ref LispObject? x, T v)
    {
        if (typeof(T) == typeof(LispObject)) x = Unsafe.As<T, LispObject>(ref v);
        else r = Unsafe.As<T, long>(ref v);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static LispObject BoxRawW<T>(T v)
    {
        if (typeof(T) == typeof(LispObject)) return Unsafe.As<T, LispObject>(ref v);
        if (typeof(T) == typeof(double)) return new DoubleFloat(Unsafe.As<T, double>(ref v));
        return Fixnum.Make(Unsafe.As<T, long>(ref v));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static R UnboxRawW<R>(LispObject v)
    {
        if (typeof(R) == typeof(LispObject)) return Unsafe.As<LispObject, R>(ref v);
        if (typeof(R) == typeof(double)) { double d = Runtime.TypedRetD(v); return Unsafe.As<double, R>(ref d); }
        long l = Runtime.TypedRetL(v);
        return Unsafe.As<long, R>(ref l);
    }

    public static unsafe R InvokeTypedW1<A0, R>(LispFunction fn, A0 a0, int code)
    {
        if (fn._nativeArity != code) return UnboxRawW<R>(fn.Invoke1(BoxRawW(a0)));
        fn.PeriodicStackCheck();
        var d = Unsafe.As<System.Func<LispFunction, A0, R>>(fn._nativeDel!);
        var n = fn._frameName;
        if (n == null)
        {
            if (fn.RetainUnit == null) return d(fn, a0);
            if (s_top == null) return UnderKeeperTypedW1<A0, R>(fn, a0, code);
            var r = d(fn, a0); System.GC.KeepAlive(fn); return r;
        }
        TypedFrameArgsW ta = default; ta.Code = code;
        PutW(ref ta.R0, ref ta.X0, a0);
        var fr = new Frame(n, 1, &ta); Link(&fr); fr._unit = fn.RetainUnit;
        try { return d(fn, a0); } finally { s_top = fr.Prev; }
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static unsafe R UnderKeeperTypedW1<A0, R>(LispFunction fn, A0 a0, int code)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return InvokeTypedW1<A0, R>(fn, a0, code); } finally { s_top = kf.Prev; }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTypedW1<A0, R>(LispFunction fn, A0 a0, int code)
        => fn._nativeArity == code && fn._typedEntry != 0
            ? ((delegate*<LispFunction, A0, R>)fn._typedEntry)(fn, a0)
            : InvokeTypedW1<A0, R>(fn, a0, code);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTypedSymW1<A0, R>(Symbol s, A0 a0, int code)
    {
        if (s._typedCode == code)
        {
            var e = System.Threading.Volatile.Read(ref s._typedEntry);
            var f = s.FunctionRaw;
            if (s._typedCode == code)
                return ((delegate*<LispFunction, A0, R>)e)(Unsafe.As<LispFunction>(f!), a0);
        }
        return CallTypedW1<A0, R>(Emitter.CilAssembler.GetFunctionBySymbol(s), a0, code);
    }

    public static unsafe int TailPrepTypedW1<A0>(LispFunction callee, int code, string? self, A0 a0)
    {
        int k = callee.TailPrepTypedCoreW(code, self, 1, out var ta);
        if (ta != null) { PutW(ref ta->R0, ref ta->X0, a0); }
        return k;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void EnterTypedW1<A0>(ref Frame fr, ref TypedFrameArgsW ta, string name, int code, A0 a0)
    {
        PutW(ref ta.R0, ref ta.X0, a0);
        EnterTypedCoreW(ref fr, ref ta, name, 1, code);
    }

    public static unsafe R InvokeTypedW2<A0, A1, R>(LispFunction fn, A0 a0, A1 a1, int code)
    {
        if (fn._nativeArity != code) return UnboxRawW<R>(fn.Invoke2(BoxRawW(a0), BoxRawW(a1)));
        fn.PeriodicStackCheck();
        var d = Unsafe.As<System.Func<LispFunction, A0, A1, R>>(fn._nativeDel!);
        var n = fn._frameName;
        if (n == null)
        {
            if (fn.RetainUnit == null) return d(fn, a0, a1);
            if (s_top == null) return UnderKeeperTypedW2<A0, A1, R>(fn, a0, a1, code);
            var r = d(fn, a0, a1); System.GC.KeepAlive(fn); return r;
        }
        TypedFrameArgsW ta = default; ta.Code = code;
        PutW(ref ta.R0, ref ta.X0, a0); PutW(ref ta.R1, ref ta.X1, a1);
        var fr = new Frame(n, 2, &ta); Link(&fr); fr._unit = fn.RetainUnit;
        try { return d(fn, a0, a1); } finally { s_top = fr.Prev; }
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static unsafe R UnderKeeperTypedW2<A0, A1, R>(LispFunction fn, A0 a0, A1 a1, int code)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return InvokeTypedW2<A0, A1, R>(fn, a0, a1, code); } finally { s_top = kf.Prev; }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTypedW2<A0, A1, R>(LispFunction fn, A0 a0, A1 a1, int code)
        => fn._nativeArity == code && fn._typedEntry != 0
            ? ((delegate*<LispFunction, A0, A1, R>)fn._typedEntry)(fn, a0, a1)
            : InvokeTypedW2<A0, A1, R>(fn, a0, a1, code);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTypedSymW2<A0, A1, R>(Symbol s, A0 a0, A1 a1, int code)
    {
        if (s._typedCode == code)
        {
            var e = System.Threading.Volatile.Read(ref s._typedEntry);
            var f = s.FunctionRaw;
            if (s._typedCode == code)
                return ((delegate*<LispFunction, A0, A1, R>)e)(Unsafe.As<LispFunction>(f!), a0, a1);
        }
        return CallTypedW2<A0, A1, R>(Emitter.CilAssembler.GetFunctionBySymbol(s), a0, a1, code);
    }

    public static unsafe int TailPrepTypedW2<A0, A1>(LispFunction callee, int code, string? self, A0 a0, A1 a1)
    {
        int k = callee.TailPrepTypedCoreW(code, self, 2, out var ta);
        if (ta != null) { PutW(ref ta->R0, ref ta->X0, a0); PutW(ref ta->R1, ref ta->X1, a1); }
        return k;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void EnterTypedW2<A0, A1>(ref Frame fr, ref TypedFrameArgsW ta, string name, int code, A0 a0, A1 a1)
    {
        PutW(ref ta.R0, ref ta.X0, a0); PutW(ref ta.R1, ref ta.X1, a1);
        EnterTypedCoreW(ref fr, ref ta, name, 2, code);
    }

    public static unsafe R InvokeTypedW3<A0, A1, A2, R>(LispFunction fn, A0 a0, A1 a1, A2 a2, int code)
    {
        if (fn._nativeArity != code) return UnboxRawW<R>(fn.Invoke3(BoxRawW(a0), BoxRawW(a1), BoxRawW(a2)));
        fn.PeriodicStackCheck();
        var d = Unsafe.As<System.Func<LispFunction, A0, A1, A2, R>>(fn._nativeDel!);
        var n = fn._frameName;
        if (n == null)
        {
            if (fn.RetainUnit == null) return d(fn, a0, a1, a2);
            if (s_top == null) return UnderKeeperTypedW3<A0, A1, A2, R>(fn, a0, a1, a2, code);
            var r = d(fn, a0, a1, a2); System.GC.KeepAlive(fn); return r;
        }
        TypedFrameArgsW ta = default; ta.Code = code;
        PutW(ref ta.R0, ref ta.X0, a0); PutW(ref ta.R1, ref ta.X1, a1); PutW(ref ta.R2, ref ta.X2, a2);
        var fr = new Frame(n, 3, &ta); Link(&fr); fr._unit = fn.RetainUnit;
        try { return d(fn, a0, a1, a2); } finally { s_top = fr.Prev; }
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static unsafe R UnderKeeperTypedW3<A0, A1, A2, R>(LispFunction fn, A0 a0, A1 a1, A2 a2, int code)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return InvokeTypedW3<A0, A1, A2, R>(fn, a0, a1, a2, code); } finally { s_top = kf.Prev; }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTypedW3<A0, A1, A2, R>(LispFunction fn, A0 a0, A1 a1, A2 a2, int code)
        => fn._nativeArity == code && fn._typedEntry != 0
            ? ((delegate*<LispFunction, A0, A1, A2, R>)fn._typedEntry)(fn, a0, a1, a2)
            : InvokeTypedW3<A0, A1, A2, R>(fn, a0, a1, a2, code);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTypedSymW3<A0, A1, A2, R>(Symbol s, A0 a0, A1 a1, A2 a2, int code)
    {
        if (s._typedCode == code)
        {
            var e = System.Threading.Volatile.Read(ref s._typedEntry);
            var f = s.FunctionRaw;
            if (s._typedCode == code)
                return ((delegate*<LispFunction, A0, A1, A2, R>)e)(Unsafe.As<LispFunction>(f!), a0, a1, a2);
        }
        return CallTypedW3<A0, A1, A2, R>(Emitter.CilAssembler.GetFunctionBySymbol(s), a0, a1, a2, code);
    }

    public static unsafe int TailPrepTypedW3<A0, A1, A2>(LispFunction callee, int code, string? self, A0 a0, A1 a1, A2 a2)
    {
        int k = callee.TailPrepTypedCoreW(code, self, 3, out var ta);
        if (ta != null) { PutW(ref ta->R0, ref ta->X0, a0); PutW(ref ta->R1, ref ta->X1, a1); PutW(ref ta->R2, ref ta->X2, a2); }
        return k;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void EnterTypedW3<A0, A1, A2>(ref Frame fr, ref TypedFrameArgsW ta, string name, int code, A0 a0, A1 a1, A2 a2)
    {
        PutW(ref ta.R0, ref ta.X0, a0); PutW(ref ta.R1, ref ta.X1, a1); PutW(ref ta.R2, ref ta.X2, a2);
        EnterTypedCoreW(ref fr, ref ta, name, 3, code);
    }

    public static unsafe R InvokeTypedW4<A0, A1, A2, A3, R>(LispFunction fn, A0 a0, A1 a1, A2 a2, A3 a3, int code)
    {
        if (fn._nativeArity != code) return UnboxRawW<R>(fn.Invoke4(BoxRawW(a0), BoxRawW(a1), BoxRawW(a2), BoxRawW(a3)));
        fn.PeriodicStackCheck();
        var d = Unsafe.As<System.Func<LispFunction, A0, A1, A2, A3, R>>(fn._nativeDel!);
        var n = fn._frameName;
        if (n == null)
        {
            if (fn.RetainUnit == null) return d(fn, a0, a1, a2, a3);
            if (s_top == null) return UnderKeeperTypedW4<A0, A1, A2, A3, R>(fn, a0, a1, a2, a3, code);
            var r = d(fn, a0, a1, a2, a3); System.GC.KeepAlive(fn); return r;
        }
        TypedFrameArgsW ta = default; ta.Code = code;
        PutW(ref ta.R0, ref ta.X0, a0); PutW(ref ta.R1, ref ta.X1, a1); PutW(ref ta.R2, ref ta.X2, a2); PutW(ref ta.R3, ref ta.X3, a3);
        var fr = new Frame(n, 4, &ta); Link(&fr); fr._unit = fn.RetainUnit;
        try { return d(fn, a0, a1, a2, a3); } finally { s_top = fr.Prev; }
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static unsafe R UnderKeeperTypedW4<A0, A1, A2, A3, R>(LispFunction fn, A0 a0, A1 a1, A2 a2, A3 a3, int code)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return InvokeTypedW4<A0, A1, A2, A3, R>(fn, a0, a1, a2, a3, code); } finally { s_top = kf.Prev; }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTypedW4<A0, A1, A2, A3, R>(LispFunction fn, A0 a0, A1 a1, A2 a2, A3 a3, int code)
        => fn._nativeArity == code && fn._typedEntry != 0
            ? ((delegate*<LispFunction, A0, A1, A2, A3, R>)fn._typedEntry)(fn, a0, a1, a2, a3)
            : InvokeTypedW4<A0, A1, A2, A3, R>(fn, a0, a1, a2, a3, code);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTypedSymW4<A0, A1, A2, A3, R>(Symbol s, A0 a0, A1 a1, A2 a2, A3 a3, int code)
    {
        if (s._typedCode == code)
        {
            var e = System.Threading.Volatile.Read(ref s._typedEntry);
            var f = s.FunctionRaw;
            if (s._typedCode == code)
                return ((delegate*<LispFunction, A0, A1, A2, A3, R>)e)(Unsafe.As<LispFunction>(f!), a0, a1, a2, a3);
        }
        return CallTypedW4<A0, A1, A2, A3, R>(Emitter.CilAssembler.GetFunctionBySymbol(s), a0, a1, a2, a3, code);
    }

    public static unsafe int TailPrepTypedW4<A0, A1, A2, A3>(LispFunction callee, int code, string? self, A0 a0, A1 a1, A2 a2, A3 a3)
    {
        int k = callee.TailPrepTypedCoreW(code, self, 4, out var ta);
        if (ta != null) { PutW(ref ta->R0, ref ta->X0, a0); PutW(ref ta->R1, ref ta->X1, a1); PutW(ref ta->R2, ref ta->X2, a2); PutW(ref ta->R3, ref ta->X3, a3); }
        return k;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void EnterTypedW4<A0, A1, A2, A3>(ref Frame fr, ref TypedFrameArgsW ta, string name, int code, A0 a0, A1 a1, A2 a2, A3 a3)
    {
        PutW(ref ta.R0, ref ta.X0, a0); PutW(ref ta.R1, ref ta.X1, a1); PutW(ref ta.R2, ref ta.X2, a2); PutW(ref ta.R3, ref ta.X3, a3);
        EnterTypedCoreW(ref fr, ref ta, name, 4, code);
    }

    public static unsafe R InvokeTypedW5<A0, A1, A2, A3, A4, R>(LispFunction fn, A0 a0, A1 a1, A2 a2, A3 a3, A4 a4, int code)
    {
        if (fn._nativeArity != code) return UnboxRawW<R>(fn.Invoke5(BoxRawW(a0), BoxRawW(a1), BoxRawW(a2), BoxRawW(a3), BoxRawW(a4)));
        fn.PeriodicStackCheck();
        var d = Unsafe.As<System.Func<LispFunction, A0, A1, A2, A3, A4, R>>(fn._nativeDel!);
        var n = fn._frameName;
        if (n == null)
        {
            if (fn.RetainUnit == null) return d(fn, a0, a1, a2, a3, a4);
            if (s_top == null) return UnderKeeperTypedW5<A0, A1, A2, A3, A4, R>(fn, a0, a1, a2, a3, a4, code);
            var r = d(fn, a0, a1, a2, a3, a4); System.GC.KeepAlive(fn); return r;
        }
        TypedFrameArgsW ta = default; ta.Code = code;
        PutW(ref ta.R0, ref ta.X0, a0); PutW(ref ta.R1, ref ta.X1, a1); PutW(ref ta.R2, ref ta.X2, a2); PutW(ref ta.R3, ref ta.X3, a3); PutW(ref ta.R4, ref ta.X4, a4);
        var fr = new Frame(n, 5, &ta); Link(&fr); fr._unit = fn.RetainUnit;
        try { return d(fn, a0, a1, a2, a3, a4); } finally { s_top = fr.Prev; }
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static unsafe R UnderKeeperTypedW5<A0, A1, A2, A3, A4, R>(LispFunction fn, A0 a0, A1 a1, A2 a2, A3 a3, A4 a4, int code)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return InvokeTypedW5<A0, A1, A2, A3, A4, R>(fn, a0, a1, a2, a3, a4, code); } finally { s_top = kf.Prev; }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTypedW5<A0, A1, A2, A3, A4, R>(LispFunction fn, A0 a0, A1 a1, A2 a2, A3 a3, A4 a4, int code)
        => fn._nativeArity == code && fn._typedEntry != 0
            ? ((delegate*<LispFunction, A0, A1, A2, A3, A4, R>)fn._typedEntry)(fn, a0, a1, a2, a3, a4)
            : InvokeTypedW5<A0, A1, A2, A3, A4, R>(fn, a0, a1, a2, a3, a4, code);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTypedSymW5<A0, A1, A2, A3, A4, R>(Symbol s, A0 a0, A1 a1, A2 a2, A3 a3, A4 a4, int code)
    {
        if (s._typedCode == code)
        {
            var e = System.Threading.Volatile.Read(ref s._typedEntry);
            var f = s.FunctionRaw;
            if (s._typedCode == code)
                return ((delegate*<LispFunction, A0, A1, A2, A3, A4, R>)e)(Unsafe.As<LispFunction>(f!), a0, a1, a2, a3, a4);
        }
        return CallTypedW5<A0, A1, A2, A3, A4, R>(Emitter.CilAssembler.GetFunctionBySymbol(s), a0, a1, a2, a3, a4, code);
    }

    public static unsafe int TailPrepTypedW5<A0, A1, A2, A3, A4>(LispFunction callee, int code, string? self, A0 a0, A1 a1, A2 a2, A3 a3, A4 a4)
    {
        int k = callee.TailPrepTypedCoreW(code, self, 5, out var ta);
        if (ta != null) { PutW(ref ta->R0, ref ta->X0, a0); PutW(ref ta->R1, ref ta->X1, a1); PutW(ref ta->R2, ref ta->X2, a2); PutW(ref ta->R3, ref ta->X3, a3); PutW(ref ta->R4, ref ta->X4, a4); }
        return k;
    }

    public static unsafe R InvokeTypedW6<A0, A1, A2, A3, A4, A5, R>(LispFunction fn, A0 a0, A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, int code)
    {
        if (fn._nativeArity != code) return UnboxRawW<R>(fn.Invoke6(BoxRawW(a0), BoxRawW(a1), BoxRawW(a2), BoxRawW(a3), BoxRawW(a4), BoxRawW(a5)));
        fn.PeriodicStackCheck();
        var d = Unsafe.As<System.Func<LispFunction, A0, A1, A2, A3, A4, A5, R>>(fn._nativeDel!);
        var n = fn._frameName;
        if (n == null)
        {
            if (fn.RetainUnit == null) return d(fn, a0, a1, a2, a3, a4, a5);
            if (s_top == null) return UnderKeeperTypedW6<A0, A1, A2, A3, A4, A5, R>(fn, a0, a1, a2, a3, a4, a5, code);
            var r = d(fn, a0, a1, a2, a3, a4, a5); System.GC.KeepAlive(fn); return r;
        }
        TypedFrameArgsW ta = default; ta.Code = code;
        PutW(ref ta.R0, ref ta.X0, a0); PutW(ref ta.R1, ref ta.X1, a1); PutW(ref ta.R2, ref ta.X2, a2); PutW(ref ta.R3, ref ta.X3, a3); PutW(ref ta.R4, ref ta.X4, a4); PutW(ref ta.R5, ref ta.X5, a5);
        var fr = new Frame(n, 6, &ta); Link(&fr); fr._unit = fn.RetainUnit;
        try { return d(fn, a0, a1, a2, a3, a4, a5); } finally { s_top = fr.Prev; }
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static unsafe R UnderKeeperTypedW6<A0, A1, A2, A3, A4, A5, R>(LispFunction fn, A0 a0, A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, int code)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return InvokeTypedW6<A0, A1, A2, A3, A4, A5, R>(fn, a0, a1, a2, a3, a4, a5, code); } finally { s_top = kf.Prev; }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTypedW6<A0, A1, A2, A3, A4, A5, R>(LispFunction fn, A0 a0, A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, int code)
        => fn._nativeArity == code && fn._typedEntry != 0
            ? ((delegate*<LispFunction, A0, A1, A2, A3, A4, A5, R>)fn._typedEntry)(fn, a0, a1, a2, a3, a4, a5)
            : InvokeTypedW6<A0, A1, A2, A3, A4, A5, R>(fn, a0, a1, a2, a3, a4, a5, code);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTypedSymW6<A0, A1, A2, A3, A4, A5, R>(Symbol s, A0 a0, A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, int code)
    {
        if (s._typedCode == code)
        {
            var e = System.Threading.Volatile.Read(ref s._typedEntry);
            var f = s.FunctionRaw;
            if (s._typedCode == code)
                return ((delegate*<LispFunction, A0, A1, A2, A3, A4, A5, R>)e)(Unsafe.As<LispFunction>(f!), a0, a1, a2, a3, a4, a5);
        }
        return CallTypedW6<A0, A1, A2, A3, A4, A5, R>(Emitter.CilAssembler.GetFunctionBySymbol(s), a0, a1, a2, a3, a4, a5, code);
    }

    public static unsafe int TailPrepTypedW6<A0, A1, A2, A3, A4, A5>(LispFunction callee, int code, string? self, A0 a0, A1 a1, A2 a2, A3 a3, A4 a4, A5 a5)
    {
        int k = callee.TailPrepTypedCoreW(code, self, 6, out var ta);
        if (ta != null) { PutW(ref ta->R0, ref ta->X0, a0); PutW(ref ta->R1, ref ta->X1, a1); PutW(ref ta->R2, ref ta->X2, a2); PutW(ref ta->R3, ref ta->X3, a3); PutW(ref ta->R4, ref ta->X4, a4); PutW(ref ta->R5, ref ta->X5, a5); }
        return k;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static unsafe void EnterTypedCoreW(ref Frame fr, ref TypedFrameArgsW ta, string name, int argc, int code)
    {
        ta.Code = code;
        fr.Name = name;
        fr._argc = argc | Frame.TypedArgsBit | Frame.TypedWideBit;
        fr._ext = (FrameExt*)Unsafe.AsPointer(ref ta);
        LinkTop(ref fr);
        if ((fr.Depth & 255) == 0 || ConditionSystem.PollGate != 0) EnterSlow(ref fr);
    }

    // As TailPrepTypedCore, for a wide callee: the frame the calling body runs
    // under is reused only when a wide typed call pushed it. TA is that frame's
    // argument storage, for the caller to fill, or null.
    private unsafe int TailPrepTypedCoreW(int code, string? self, int argc, out TypedFrameArgsW* ta)
    {
        ta = null;
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
            const int wide = Frame.TypedArgsBit | Frame.TypedWideBit;
            if ((top->_argc & wide) != wide) return TailKindInvoke;
            ta = (TypedFrameArgsW*)top->_ext;
            ta->Code = code;
            top->Name = callee;
            top->_argc = argc | wide;
        }
        else if (owned)
            top->Lend();
        if (unit != null) top->KeepUnit(unit);
        return TailKindDirect;
    }
}
