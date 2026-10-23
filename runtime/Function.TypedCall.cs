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

    // ---- Framed typed entries ----
    //
    // A typed function that records a frame also has an entry that links that
    // frame itself (no try/finally, as a DEFUN body, Function.SelfFramed.cs)
    // around a call to the typed body: R Entry(LispFunction self, T1..Tn).
    // CallTypedN enters it by calli when the function's typed code is the
    // caller's, and otherwise takes InvokeTypedN.

    internal nint _typedEntry;   // read only while _nativeArity is the typed code

    public void SetTypedFramedEntry(System.IntPtr entry, int sigCode)
    {
        if (_nativeArity == sigCode) _typedEntry = entry;
    }

    // What the boxed shell checks the first two arguments against, a
    // TypedArgSpec byte each; 0 when not recorded (an older fasl). A ushort
    // fits the object's padding; a wider field grows every function.
    internal ushort _typedArgSpec;

    public void SetTypedArgRanges(long lo0, long hi0, long lo1, long hi1,
                                  long lo2, long hi2, long lo3, long hi3)
        => _typedArgSpec = (ushort)(TypedArgSpec(lo0, hi0) | TypedArgSpec(lo1, hi1) << 8);

    // 1: any value; 2 + W: (UNSIGNED-BYTE W); 66 + W: (SIGNED-BYTE W);
    // 0 for any other range, which the fast paths do not take.
    static int TypedArgSpec(long lo, long hi)
    {
        if (lo == long.MinValue && hi == long.MaxValue) return 1;
        for (int w = 1; w < 64; w++)
        {
            if (lo == 0 && hi == (1L << w) - 1) return 2 + w;
            if (lo == -(1L << (w - 1)) && hi == (1L << (w - 1)) - 1) return 66 + w;
        }
        return 0;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static bool TypedArgFits(int spec, long v)
    {
        if (spec == 1) return true;
        if (spec <= 65) return spec >= 3 && (ulong)v >> (spec - 2) == 0;
        long t = v >> (spec - 67);
        return t == 0 || t == -1;
    }

    /// <summary>For tests: "framed", "typed" (InvokeTypedN only) or "none".</summary>
    public static string TypedEntryState(LispObject fn) =>
        fn is LispFunction f ? (f.TypedEntryCode != 0 && f._typedEntry != 0 ? "framed" : f.TypedEntryCode != 0 ? "typed" : "none") : "none";

    const int TypedL2L = 0x120, TypedL2D = 0x130, TypedD2L = 0x121, TypedD2D = 0x131;

    // The common case of TryTypedOnElement on its own: an integer -> integer
    // entry and a simple 1-D vector stored as long[].
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static unsafe bool TryTypedLongOnLongElement(LispFunction fn, LispObject a, long i, out long r)
    {
        if (fn._nativeArity == TypedL2L
            && a is LispVector v && v._numData is long[] l
            && v._dimensions == null && v._displacedTo == null
            && (ulong)i < (ulong)l.Length)
        {
            long e = l[(int)i];
            var entry = fn._typedEntry;
            if (entry != 0 && TypedArgFits(fn._typedArgSpec & 0xFF, e))
            {
                r = ((delegate*<LispFunction, long, long>)entry)(fn, e);
                return true;
            }
        }
        r = 0;
        return false;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static unsafe bool TryTypedLongOnLong(LispFunction fn, long e, out long r)
    {
        var entry = fn._typedEntry;
        if (fn._nativeArity == TypedL2L && entry != 0 && TypedArgFits(fn._typedArgSpec & 0xFF, e))
        {
            r = ((delegate*<LispFunction, long, long>)entry)(fn, e);
            return true;
        }
        r = 0;
        return false;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static bool TryTypedOnLongElement(LispFunction fn, LispObject a, long i, out LispObject r)
    {
        if (TryTypedLongOnLongElement(fn, a, i, out long x))
        {
            r = Fixnum.Make(x);
            return true;
        }
        r = null!;
        return false;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static bool TryTypedOnElement(LispFunction fn, LispVector v, int x, out LispObject r)
    {
        int spec = fn._typedArgSpec & 0xFF;
        int c = fn._nativeArity;
        if (spec != 0 && fn._typedEntry != 0)
        {
            var nd = v._numData;
            if (c == TypedL2L || c == TypedL2D)
            {
                long e;
                if (nd != null)
                {
                    if (!v.IsRawNumKind || v.IsFloatNumKind) goto no;
                    e = v.NumGet(x);
                }
                else if (v._elements[x] is Fixnum f) e = f.Value;
                else goto no;
                if (!TypedArgFits(spec, e)) goto no;
                r = c == TypedL2L
                    ? Fixnum.Make(CallTyped1<long, long>(fn, e, c))
                    : new DoubleFloat(CallTyped1<long, double>(fn, e, c));
                return true;
            }
            if (c == TypedD2L || c == TypedD2D)
            {
                double e;
                if (nd != null)
                {
                    if (v._numKind != 6) goto no;
                    e = ((double[])nd)[x];
                }
                else if (v._elements[x] is DoubleFloat d) e = d.Value;
                else goto no;
                r = c == TypedD2L
                    ? Fixnum.Make(CallTyped1<double, long>(fn, e, c))
                    : new DoubleFloat(CallTyped1<double, double>(fn, e, c));
                return true;
            }
        }
    no:
        r = null!;
        return false;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTyped1<A0, R>(LispFunction fn, A0 a0, int code)
        where A0 : unmanaged where R : unmanaged
        => fn._nativeArity == code && fn._typedEntry != 0
            ? ((delegate*<LispFunction, A0, R>)fn._typedEntry)(fn, a0)
            : InvokeTyped1<A0, R>(fn, a0, code);
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTyped2<A0, A1, R>(LispFunction fn, A0 a0, A1 a1, int code)
        where A0 : unmanaged where A1 : unmanaged where R : unmanaged
        => fn._nativeArity == code && fn._typedEntry != 0
            ? ((delegate*<LispFunction, A0, A1, R>)fn._typedEntry)(fn, a0, a1)
            : InvokeTyped2<A0, A1, R>(fn, a0, a1, code);
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTyped3<A0, A1, A2, R>(LispFunction fn, A0 a0, A1 a1, A2 a2, int code)
        where A0 : unmanaged where A1 : unmanaged where A2 : unmanaged where R : unmanaged
        => fn._nativeArity == code && fn._typedEntry != 0
            ? ((delegate*<LispFunction, A0, A1, A2, R>)fn._typedEntry)(fn, a0, a1, a2)
            : InvokeTyped3<A0, A1, A2, R>(fn, a0, a1, a2, code);
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTyped4<A0, A1, A2, A3, R>(LispFunction fn, A0 a0, A1 a1, A2 a2, A3 a3, int code)
        where A0 : unmanaged where A1 : unmanaged where A2 : unmanaged where A3 : unmanaged where R : unmanaged
        => fn._nativeArity == code && fn._typedEntry != 0
            ? ((delegate*<LispFunction, A0, A1, A2, A3, R>)fn._typedEntry)(fn, a0, a1, a2, a3)
            : InvokeTyped4<A0, A1, A2, A3, R>(fn, a0, a1, a2, a3, code);

    // Calls through the typed entry the symbol keeps for its function. The code
    // is read again after the entry and the function: a redefinition clears it
    // first, so a second match means the entry read is one of signature CODE.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTypedSym1<A0, R>(Symbol s, A0 a0, int code)
        where A0 : unmanaged where R : unmanaged
    {
        if (s._typedCode == code)
        {
            var e = System.Threading.Volatile.Read(ref s._typedEntry);
            var f = s.FunctionRaw;
            if (s._typedCode == code)
                return ((delegate*<LispFunction, A0, R>)e)(Unsafe.As<LispFunction>(f!), a0);
        }
        return CallTyped1<A0, R>(Emitter.CilAssembler.GetFunctionBySymbol(s), a0, code);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTypedSym2<A0, A1, R>(Symbol s, A0 a0, A1 a1, int code)
        where A0 : unmanaged where A1 : unmanaged where R : unmanaged
    {
        if (s._typedCode == code)
        {
            var e = System.Threading.Volatile.Read(ref s._typedEntry);
            var f = s.FunctionRaw;
            if (s._typedCode == code)
                return ((delegate*<LispFunction, A0, A1, R>)e)(Unsafe.As<LispFunction>(f!), a0, a1);
        }
        return CallTyped2<A0, A1, R>(Emitter.CilAssembler.GetFunctionBySymbol(s), a0, a1, code);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTypedSym3<A0, A1, A2, R>(Symbol s, A0 a0, A1 a1, A2 a2, int code)
        where A0 : unmanaged where A1 : unmanaged where A2 : unmanaged where R : unmanaged
    {
        if (s._typedCode == code)
        {
            var e = System.Threading.Volatile.Read(ref s._typedEntry);
            var f = s.FunctionRaw;
            if (s._typedCode == code)
                return ((delegate*<LispFunction, A0, A1, A2, R>)e)(Unsafe.As<LispFunction>(f!), a0, a1, a2);
        }
        return CallTyped3<A0, A1, A2, R>(Emitter.CilAssembler.GetFunctionBySymbol(s), a0, a1, a2, code);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe R CallTypedSym4<A0, A1, A2, A3, R>(Symbol s, A0 a0, A1 a1, A2 a2, A3 a3, int code)
        where A0 : unmanaged where A1 : unmanaged where A2 : unmanaged where A3 : unmanaged where R : unmanaged
    {
        if (s._typedCode == code)
        {
            var e = System.Threading.Volatile.Read(ref s._typedEntry);
            var f = s.FunctionRaw;
            if (s._typedCode == code)
                return ((delegate*<LispFunction, A0, A1, A2, A3, R>)e)(Unsafe.As<LispFunction>(f!), a0, a1, a2, a3);
        }
        return CallTyped4<A0, A1, A2, A3, R>(Emitter.CilAssembler.GetFunctionBySymbol(s), a0, a1, a2, a3, code);
    }

    /// <summary>CallTypedSymN (CallTypedSymWN for a wide one) for the typed-entry signature SIG.</summary>
    public static System.Reflection.MethodInfo TypedSymCallMethod(string sig) => TypedGeneric("CallTypedSym", sig, true);

    // Like InvokeTypedN, the frame is linked whatever CallTls.Borrow says.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static unsafe void EnterTypedCore(ref Frame fr, ref TypedFrameArgs ta, string name, int argc, int code)
    {
        ta.Code = code;
        fr.Name = name;
        fr._argc = argc | Frame.TypedArgsBit;
        fr._ext = (FrameExt*)Unsafe.AsPointer(ref ta);
        LinkTop(ref fr);
        if ((fr.Depth & 255) == 0 || ConditionSystem.PollGate != 0) EnterSlow(ref fr);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void EnterTyped1<A0>(ref Frame fr, ref TypedFrameArgs ta, string name, int code, A0 a0)
        where A0 : unmanaged
    {
        ta.R0 = RawBits(a0);
        EnterTypedCore(ref fr, ref ta, name, 1, code);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void EnterTyped2<A0, A1>(ref Frame fr, ref TypedFrameArgs ta, string name, int code, A0 a0, A1 a1)
        where A0 : unmanaged where A1 : unmanaged
    {
        ta.R0 = RawBits(a0); ta.R1 = RawBits(a1);
        EnterTypedCore(ref fr, ref ta, name, 2, code);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void EnterTyped3<A0, A1, A2>(ref Frame fr, ref TypedFrameArgs ta, string name, int code, A0 a0, A1 a1, A2 a2)
        where A0 : unmanaged where A1 : unmanaged where A2 : unmanaged
    {
        ta.R0 = RawBits(a0); ta.R1 = RawBits(a1); ta.R2 = RawBits(a2);
        EnterTypedCore(ref fr, ref ta, name, 3, code);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void EnterTyped4<A0, A1, A2, A3>(ref Frame fr, ref TypedFrameArgs ta, string name, int code, A0 a0, A1 a1, A2 a2, A3 a3)
        where A0 : unmanaged where A1 : unmanaged where A2 : unmanaged where A3 : unmanaged
    {
        ta.R0 = RawBits(a0); ta.R1 = RawBits(a1); ta.R2 = RawBits(a2); ta.R3 = RawBits(a3);
        EnterTypedCore(ref fr, ref ta, name, 4, code);
    }

    /// <summary>CallTypedN for the typed-entry signature SIG.</summary>
    public static System.Reflection.MethodInfo TypedCallMethod(string sig) => TypedGeneric("CallTyped", sig, true);

    /// <summary>EnterTypedN for the argument kinds of SIG.</summary>
    public static System.Reflection.MethodInfo TypedEnterMethod(string sig) => TypedGeneric("EnterTyped", sig, false);

    static System.Reflection.MethodInfo TypedGeneric(string stem, string sig, bool withResult)
    {
        int n = sig.Length - 2;
        if (DotCL.Emitter.TypedEntrySig.IsWide(sig)) stem += "W";
        var targs = new System.Type[withResult ? n + 1 : n];
        for (int i = 0; i < n; i++) targs[i] = DotCL.Emitter.TypedEntrySig.KindType(sig[i]);
        if (withResult) targs[n] = DotCL.Emitter.TypedEntrySig.KindType(sig[n + 1]);
        return typeof(LispFunction).GetMethod(stem + n)!.MakeGenericMethod(targs);
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
            if ((top->_argc & (Frame.TypedArgsBit | Frame.TypedWideBit)) != Frame.TypedArgsBit) return TailKindInvoke;
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
        var stem = DotCL.Emitter.TypedEntrySig.IsWide(sig) ? "InvokeTypedW" : "InvokeTyped";
        return typeof(LispFunction).GetMethod(stem + n)!.MakeGenericMethod(targs);
    }
}

public partial class LispFunction
{
    /// <summary>TailPrepTypedN (TailPrepTypedWN for a wide one) for the argument kinds of SIG.</summary>
    public static System.Reflection.MethodInfo TypedTailPrepMethod(string sig) => TypedGeneric("TailPrepTyped", sig, false);
}
