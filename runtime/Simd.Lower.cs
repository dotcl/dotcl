#if !NETSTANDARD2_0
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Runtime.Intrinsics;

namespace DotCL;

/// <summary>Entry points for DOTCL-SIMD calls the compiler lowers to raw
/// <c>Vector128&lt;T&gt;</c> values. Each is closed over the lane type in the
/// assembler. Anything off the fast path (a value of the wrong type, an index
/// out of range, an array without a direct buffer) goes to the boxed function,
/// so the diagnostics are the same as for an ordinary call.</summary>
public static class SimdLower
{
    static class KindOf<T>
    {
        public static readonly SimdKind K =
            typeof(T) == typeof(double) ? SimdKind.F64x2 :
            typeof(T) == typeof(float) ? SimdKind.F32x4 :
            typeof(T) == typeof(long) ? SimdKind.S64x2 :
            typeof(T) == typeof(ulong) ? SimdKind.U64x2 :
            typeof(T) == typeof(int) ? SimdKind.S32x4 :
            typeof(T) == typeof(uint) ? SimdKind.U32x4 :
            typeof(T) == typeof(short) ? SimdKind.S16x8 :
            typeof(T) == typeof(ushort) ? SimdKind.U16x8 :
            typeof(T) == typeof(sbyte) ? SimdKind.S8x16 : SimdKind.U8x16;
    }

    public static readonly Type[] LaneTypes =
    {
        typeof(double), typeof(float), typeof(long), typeof(ulong), typeof(int),
        typeof(uint), typeof(short), typeof(ushort), typeof(sbyte), typeof(byte),
    };

    static readonly bool LanesAdjacent = HiFollowsLo();

    static bool HiFollowsLo()
    {
        var p = new LispSimdPack(SimdKind.U64x2, 0, 0);
        return Unsafe.ByteOffset(ref Unsafe.AsRef(in p.Lo), ref Unsafe.AsRef(in p.Hi)) == 8;
    }

    // One vector load: building the vector from the two ulongs inserts into the
    // previous value of the register, which chains the iterations of a loop.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static Vector128<T> V<T>(LispSimdPack p) where T : struct =>
        LanesAdjacent
            ? Unsafe.ReadUnaligned<Vector128<T>>(ref Unsafe.As<ulong, byte>(ref Unsafe.AsRef(in p.Lo)))
            : Vector128.Create(p.Lo, p.Hi).As<ulong, T>();

    public static LispObject Box<T>(Vector128<T> v) where T : struct
    {
        var u = v.As<T, ulong>();
        return new LispSimdPack(KindOf<T>.K, u.GetElement(0), u.GetElement(1));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> Unbox<T>(LispObject o, string fn) where T : struct
    {
        if (o is LispSimdPack p && p.Kind == KindOf<T>.K) return V<T>(p);
        return V<T>(SimdInfo.Cast(KindOf<T>.K, o, fn));
    }

    /// <summary>The value of a local declared a pack type: a pack of exactly that type.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> Check<T>(LispObject o, string fn) where T : struct
    {
        if (o is LispSimdPack p && p.Kind == KindOf<T>.K) return V<T>(p);
        return CheckSlow<T>(o, fn);
    }

    static Vector128<T> CheckSlow<T>(LispObject o, string fn) where T : struct
    {
        o = Runtime.Primary(o);
        if (o is LispSimdPack p && p.Kind == KindOf<T>.K) return V<T>(p);
        throw new LispErrorException(SimdInfo.TypeError(fn, o, KindOf<T>.K));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> Broadcast<T>(T x) where T : struct => Vector128.Create(x);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> Add<T>(Vector128<T> a, Vector128<T> b) where T : struct => Vector128.Add(a, b);
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> Sub<T>(Vector128<T> a, Vector128<T> b) where T : struct => Vector128.Subtract(a, b);
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> Mul<T>(Vector128<T> a, Vector128<T> b) where T : struct => Vector128.Multiply(a, b);
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> Div<T>(Vector128<T> a, Vector128<T> b) where T : struct => Vector128.Divide(a, b);
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> And<T>(Vector128<T> a, Vector128<T> b) where T : struct => Vector128.BitwiseAnd(a, b);
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> Or<T>(Vector128<T> a, Vector128<T> b) where T : struct => Vector128.BitwiseOr(a, b);
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> Xor<T>(Vector128<T> a, Vector128<T> b) where T : struct => Vector128.Xor(a, b);
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> AndC1<T>(Vector128<T> a, Vector128<T> b) where T : struct => Vector128.AndNot(b, a);
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> Not<T>(Vector128<T> a) where T : struct => Vector128.OnesComplement(a);
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> Sqrt<T>(Vector128<T> a) where T : struct => Vector128.Sqrt(a);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> Min<T>(Vector128<T> a, Vector128<T> b) where T : struct
    {
#if !NET9_0_OR_GREATER
        if (typeof(T) == typeof(double) || typeof(T) == typeof(float))
            return MinMaxScalar(SimdOp.Min, a, b);
#endif
        return Vector128.Min(a, b);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> Max<T>(Vector128<T> a, Vector128<T> b) where T : struct
    {
#if !NET9_0_OR_GREATER
        if (typeof(T) == typeof(double) || typeof(T) == typeof(float))
            return MinMaxScalar(SimdOp.Max, a, b);
#endif
        return Vector128.Max(a, b);
    }

#if !NET9_0_OR_GREATER
    static Vector128<T> MinMaxScalar<T>(SimdOp op, Vector128<T> a, Vector128<T> b) where T : struct
    {
        var x = (LispSimdPack)Box(a);
        var y = (LispSimdPack)Box(b);
        return V<T>(SimdScalar.Binary(op, x, y));
    }
#endif

    static Type MaskOf(Type t) =>
        t == typeof(double) || t == typeof(long) || t == typeof(ulong) ? typeof(ulong) :
        t == typeof(float) || t == typeof(int) || t == typeof(uint) ? typeof(uint) :
        t == typeof(short) || t == typeof(ushort) ? typeof(ushort) : typeof(byte);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<M> Eq<T, M>(Vector128<T> a, Vector128<T> b) where T : struct where M : struct =>
        Vector128.Equals(a, b).As<T, M>();
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<M> Ne<T, M>(Vector128<T> a, Vector128<T> b) where T : struct where M : struct =>
        Vector128.OnesComplement(Vector128.Equals(a, b)).As<T, M>();
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<M> Lt<T, M>(Vector128<T> a, Vector128<T> b) where T : struct where M : struct =>
        Vector128.LessThan(a, b).As<T, M>();
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<M> Le<T, M>(Vector128<T> a, Vector128<T> b) where T : struct where M : struct =>
        Vector128.LessThanOrEqual(a, b).As<T, M>();
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<M> Gt<T, M>(Vector128<T> a, Vector128<T> b) where T : struct where M : struct =>
        Vector128.GreaterThan(a, b).As<T, M>();
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<M> Ge<T, M>(Vector128<T> a, Vector128<T> b) where T : struct where M : struct =>
        Vector128.GreaterThanOrEqual(a, b).As<T, M>();
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> If<T, M>(Vector128<M> m, Vector128<T> a, Vector128<T> b) where T : struct where M : struct =>
        Vector128.ConditionalSelect(m.As<M, T>(), a, b);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> Shl<T>(Vector128<T> a, int n) where T : struct
    {
        if (typeof(T) == typeof(long)) return Vector128.ShiftLeft(a.As<T, long>(), n).As<long, T>();
        if (typeof(T) == typeof(ulong)) return Vector128.ShiftLeft(a.As<T, ulong>(), n).As<ulong, T>();
        if (typeof(T) == typeof(int)) return Vector128.ShiftLeft(a.As<T, int>(), n).As<int, T>();
        if (typeof(T) == typeof(uint)) return Vector128.ShiftLeft(a.As<T, uint>(), n).As<uint, T>();
        if (typeof(T) == typeof(short)) return Vector128.ShiftLeft(a.As<T, short>(), n).As<short, T>();
        if (typeof(T) == typeof(ushort)) return Vector128.ShiftLeft(a.As<T, ushort>(), n).As<ushort, T>();
        if (typeof(T) == typeof(sbyte)) return Vector128.ShiftLeft(a.As<T, sbyte>(), n).As<sbyte, T>();
        return Vector128.ShiftLeft(a.As<T, byte>(), n).As<byte, T>();
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> Shr<T>(Vector128<T> a, int n) where T : struct
    {
        if (typeof(T) == typeof(long)) return Vector128.ShiftRightArithmetic(a.As<T, long>(), n).As<long, T>();
        if (typeof(T) == typeof(ulong)) return Vector128.ShiftRightLogical(a.As<T, ulong>(), n).As<ulong, T>();
        if (typeof(T) == typeof(int)) return Vector128.ShiftRightArithmetic(a.As<T, int>(), n).As<int, T>();
        if (typeof(T) == typeof(uint)) return Vector128.ShiftRightLogical(a.As<T, uint>(), n).As<uint, T>();
        if (typeof(T) == typeof(short)) return Vector128.ShiftRightArithmetic(a.As<T, short>(), n).As<short, T>();
        if (typeof(T) == typeof(ushort)) return Vector128.ShiftRightLogical(a.As<T, ushort>(), n).As<ushort, T>();
        if (typeof(T) == typeof(sbyte)) return Vector128.ShiftRightArithmetic(a.As<T, sbyte>(), n).As<sbyte, T>();
        return Vector128.ShiftRightLogical(a.As<T, byte>(), n).As<byte, T>();
    }

    static LispObject Lane<T>(T x) where T : struct
    {
        ulong bits = 0;
        Unsafe.As<ulong, T>(ref bits) = x;
        return SimdInfo.LaneToLisp(KindOf<T>.K, bits & SimdInfo.Mask(KindOf<T>.K));
    }

    static bool IsFloat<T>() => typeof(T) == typeof(double) || typeof(T) == typeof(float);

    static T Reduce<T>(SimdOp op, Vector128<T> v, int start, int len) where T : struct
    {
        if (len == 1) return v.GetElement(start);
        int h = len / 2;
        var a = Vector128.CreateScalar(Reduce(op, v, start, h));
        var b = Vector128.CreateScalar(Reduce(op, v, start + h, h));
        return (op == SimdOp.Add ? Vector128.Add(a, b) : op == SimdOp.Min ? Min(a, b) : Max(a, b)).ToScalar();
    }

    /// <summary>The horizontal sum, in the order of the boxed function: a float sum
    /// as the balanced tree, an integer sum wrapping.</summary>
    public static LispObject HAdd<T>(Vector128<T> v) where T : struct =>
        Lane(IsFloat<T>() ? Reduce(SimdOp.Add, v, 0, Vector128<T>.Count) : Vector128.Sum(v));

    public static LispObject HMin<T>(Vector128<T> v) where T : struct => Lane(Reduce(SimdOp.Min, v, 0, Vector128<T>.Count));

    public static LispObject HMax<T>(Vector128<T> v) where T : struct => Lane(Reduce(SimdOp.Max, v, 0, Vector128<T>.Count));

    static readonly (Type From, Type To)[] ConversionPairs =
    {
        (typeof(int), typeof(float)), (typeof(uint), typeof(float)),
        (typeof(float), typeof(int)), (typeof(float), typeof(uint)),
        (typeof(long), typeof(double)), (typeof(ulong), typeof(double)),
        (typeof(double), typeof(long)), (typeof(double), typeof(ulong)),
    };

    /// <summary>Lane conversion. Where the vector form has the same results on every
    /// runtime (net9 and later, or an exact conversion) it is used; otherwise the
    /// scalar reference, as the boxed function does.</summary>
    public static Vector128<TTo> Cvt<TFrom, TTo>(Vector128<TFrom> a) where TFrom : struct where TTo : struct
    {
        if (typeof(TFrom) == typeof(int) && typeof(TTo) == typeof(float))
            return Vector128.ConvertToSingle(a.As<TFrom, int>()).As<float, TTo>();
        if (typeof(TFrom) == typeof(long) && typeof(TTo) == typeof(double))
            return Vector128.ConvertToDouble(a.As<TFrom, long>()).As<double, TTo>();
#if NET9_0_OR_GREATER
        if (typeof(TFrom) == typeof(uint) && typeof(TTo) == typeof(float))
            return Vector128.ConvertToSingle(a.As<TFrom, uint>()).As<float, TTo>();
        if (typeof(TFrom) == typeof(ulong) && typeof(TTo) == typeof(double))
            return Vector128.ConvertToDouble(a.As<TFrom, ulong>()).As<double, TTo>();
        if (typeof(TFrom) == typeof(float) && typeof(TTo) == typeof(int))
            return Vector128.ConvertToInt32(a.As<TFrom, float>()).As<int, TTo>();
        if (typeof(TFrom) == typeof(float) && typeof(TTo) == typeof(uint))
            return Vector128.ConvertToUInt32(a.As<TFrom, float>()).As<uint, TTo>();
        if (typeof(TFrom) == typeof(double) && typeof(TTo) == typeof(long))
            return Vector128.ConvertToInt64(a.As<TFrom, double>()).As<long, TTo>();
        if (typeof(TFrom) == typeof(double) && typeof(TTo) == typeof(ulong))
            return Vector128.ConvertToUInt64(a.As<TFrom, double>()).As<ulong, TTo>();
#endif
        return CvtScalar<TFrom, TTo>(a);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static Vector128<TTo> CvtScalar<TFrom, TTo>(Vector128<TFrom> a) where TFrom : struct where TTo : struct =>
        V<TTo>(SimdScalar.Convert(KindOf<TTo>.K, (LispSimdPack)Box(a)));

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static bool InRange<T>(T[]? a, long i) where T : struct =>
        a != null && a.Length >= Vector128<T>.Count && (ulong)i <= (ulong)(a.Length - Vector128<T>.Count);

    /// <summary>The pack at element I of A, the buffer of the array ARR. FN names the reader.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> Load<T>(T[]? a, long i, LispObject arr, string fn) where T : struct
    {
        if (InRange(a, i))
            return Vector128.LoadUnsafe(ref MemoryMarshal.GetArrayDataReference(a!), (nuint)i);
        return LoadSlow<T>(arr, i, fn);
    }

    /// <summary>LOAD without the index check, for (safety 0).</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> LoadNoCheck<T>(T[]? a, long i, LispObject arr, string fn) where T : struct
    {
        if (a != null)
            return Vector128.LoadUnsafe(ref MemoryMarshal.GetArrayDataReference(a), (nuint)i);
        return LoadSlow<T>(arr, i, fn);
    }

    /// <summary>LOAD with neither the index check nor a missing buffer: (safety 0) where the buffer is known.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> LoadRaw<T>(T[] a, long i) where T : struct =>
        Vector128.LoadUnsafe(ref MemoryMarshal.GetArrayDataReference(a), (nuint)i);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void StoreRaw<T>(T[] a, long i, Vector128<T> v) where T : struct =>
        Vector128.StoreUnsafe(v, ref MemoryMarshal.GetArrayDataReference(a), (nuint)i);

    /// <summary>The pack at element I from R, the first element of a buffer a counting
    /// loop has proven long enough.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<T> LoadAt<T>(ref T r, long i) where T : struct =>
        Vector128.LoadUnsafe(ref r, (nuint)i);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void StoreAt<T>(ref T r, long i, Vector128<T> v) where T : struct =>
        Vector128.StoreUnsafe(v, ref r, (nuint)i);

    [MethodImpl(MethodImplOptions.NoInlining)]
    static Vector128<T> LoadSlow<T>(LispObject arr, long i, string fn) where T : struct =>
        Unbox<T>(SimdTable.Table[fn].Invoke(arr, Fixnum.Make(i)), fn);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Store<T>(T[]? a, long i, Vector128<T> v, LispObject arr, string fn) where T : struct
    {
        if (InRange(a, i))
            Vector128.StoreUnsafe(v, ref MemoryMarshal.GetArrayDataReference(a!), (nuint)i);
        else
            StoreSlow(arr, i, v, fn);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void StoreNoCheck<T>(T[]? a, long i, Vector128<T> v, LispObject arr, string fn) where T : struct
    {
        if (a != null)
            Vector128.StoreUnsafe(v, ref MemoryMarshal.GetArrayDataReference(a), (nuint)i);
        else
            StoreSlow(arr, i, v, fn);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static void StoreSlow<T>(LispObject arr, long i, Vector128<T> v, string fn) where T : struct =>
        SimdTable.Table["(SETF " + fn + ")"].Invoke(Box(v), arr, Fixnum.Make(i));

    // The (unsigned-byte 64) buffer is held as a long[] (Runtime.BackingU64).
    public static Vector128<ulong> LoadU64(long[]? a, long i, LispObject arr, string fn) =>
        Load(Unsafe.As<ulong[]>(a), i, arr, fn);
    public static Vector128<ulong> LoadNoCheckU64(long[]? a, long i, LispObject arr, string fn) =>
        LoadNoCheck(Unsafe.As<ulong[]>(a), i, arr, fn);
    public static Vector128<ulong> LoadRawU64(long[] a, long i) => LoadRaw(Unsafe.As<ulong[]>(a), i);
    public static void StoreU64(long[]? a, long i, Vector128<ulong> v, LispObject arr, string fn) =>
        Store(Unsafe.As<ulong[]>(a), i, v, arr, fn);
    public static void StoreNoCheckU64(long[]? a, long i, Vector128<ulong> v, LispObject arr, string fn) =>
        StoreNoCheck(Unsafe.As<ulong[]>(a), i, v, arr, fn);
    public static void StoreRawU64(long[] a, long i, Vector128<ulong> v) => StoreRaw(Unsafe.As<ulong[]>(a), i, v);
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128<ulong> LoadAtU64(ref long r, long i) => LoadAt(ref Unsafe.As<long, ulong>(ref r), i);
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void StoreAtU64(ref long r, long i, Vector128<ulong> v) => StoreAt(ref Unsafe.As<long, ulong>(ref r), i, v);

    /// <summary>Methods the assembler calls by name: "Simd.Op.LaneType" for each of the ten lane types.</summary>
    public static IEnumerable<(string Name, System.Reflection.MethodInfo Method)> Methods()
    {
        string[] ops =
        {
            "Box", "Unbox", "Check", "Broadcast", "Add", "Sub", "Mul", "Div", "And", "Or", "Xor", "AndC1", "Not",
            "Sqrt", "Min", "Max", "HAdd", "HMin", "HMax", "Load", "LoadNoCheck", "LoadRaw", "LoadAt", "Store", "StoreNoCheck", "StoreRaw", "StoreAt",
        };
        foreach (var t in LaneTypes)
        {
            var m = MaskOf(t);
            foreach (var op in new[] { "Eq", "Ne", "Lt", "Le", "Gt", "Ge", "If" })
                yield return ("Simd." + op + "." + t.Name, typeof(SimdLower).GetMethod(op)!.MakeGenericMethod(t, m));
        }
        foreach (var t in LaneTypes)
        {
            if (t == typeof(double) || t == typeof(float)) continue;
            yield return ("Simd.Shl." + t.Name, typeof(SimdLower).GetMethod("Shl")!.MakeGenericMethod(t));
            yield return ("Simd.Shr." + t.Name, typeof(SimdLower).GetMethod("Shr")!.MakeGenericMethod(t));
        }
        foreach (var (from, to) in ConversionPairs)
            yield return ("Simd.Cvt." + from.Name + "." + to.Name,
                          typeof(SimdLower).GetMethod("Cvt")!.MakeGenericMethod(from, to));
        foreach (var op in ops)
        {
            var def = typeof(SimdLower).GetMethod(op)!;
            foreach (var t in LaneTypes)
            {
                if (t == typeof(ulong) && (op.StartsWith("Load") || op.StartsWith("Store")))
                    yield return ("Simd." + op + "." + t.Name, typeof(SimdLower).GetMethod(op + "U64")!);
                else
                    yield return ("Simd." + op + "." + t.Name, def.MakeGenericMethod(t));
            }
        }
    }
}
#endif
