#if !NETSTANDARD2_0
using System.Runtime.Intrinsics;
#endif

namespace DotCL;

public enum SimdOp
{
    Add, Sub, Mul, Div, Min, Max, And, Or, Xor, AndC1,
    Sqrt, Not, Round, Floor, Ceiling, Truncate,
    Eq, Ne, Lt, Le, Gt, Ge,
}

/// <summary>Lane-by-lane reference implementation. It is the only one on netstandard2.0
/// and the oracle the Vector128 implementation is tested against.</summary>
public static class SimdScalar
{
    static double MinD(double a, double b)
    {
        if (double.IsNaN(a)) return a;
        if (double.IsNaN(b)) return b;
        if (a == b) return BitConverter.DoubleToInt64Bits(a) < 0 ? a : b;
        return a < b ? a : b;
    }

    static double MaxD(double a, double b)
    {
        if (double.IsNaN(a)) return a;
        if (double.IsNaN(b)) return b;
        if (a == b) return BitConverter.DoubleToInt64Bits(a) < 0 ? b : a;
        return a > b ? a : b;
    }

    static float MinS(float a, float b)
    {
        if (float.IsNaN(a)) return a;
        if (float.IsNaN(b)) return b;
        if (a == b) return Compat.SingleToInt32Bits(a) < 0 ? a : b;
        return a < b ? a : b;
    }

    static float MaxS(float a, float b)
    {
        if (float.IsNaN(a)) return a;
        if (float.IsNaN(b)) return b;
        if (a == b) return Compat.SingleToInt32Bits(a) < 0 ? b : a;
        return a > b ? a : b;
    }

    static double RoundD(SimdOp op, double d) => op switch
    {
        SimdOp.Round => Math.Round(d, MidpointRounding.ToEven),
        SimdOp.Floor => Math.Floor(d),
        SimdOp.Ceiling => Math.Ceiling(d),
        _ => Math.Truncate(d),
    };

    static ulong LaneBinary(SimdKind k, SimdOp op, ulong x, ulong y)
    {
        if (k == SimdKind.F64x2)
        {
            double a = SimdInfo.LaneDouble(x), b = SimdInfo.LaneDouble(y);
            return SimdInfo.DoubleLane(op switch
            {
                SimdOp.Add => a + b,
                SimdOp.Sub => a - b,
                SimdOp.Mul => a * b,
                SimdOp.Div => a / b,
                SimdOp.Min => MinD(a, b),
                _ => MaxD(a, b),
            });
        }
        if (k == SimdKind.F32x4)
        {
            float a = SimdInfo.LaneSingle(x), b = SimdInfo.LaneSingle(y);
            return SimdInfo.SingleLane(op switch
            {
                SimdOp.Add => (float)(a + b),
                SimdOp.Sub => (float)(a - b),
                SimdOp.Mul => (float)(a * b),
                SimdOp.Div => (float)(a / b),
                SimdOp.Min => MinS(a, b),
                _ => MaxS(a, b),
            });
        }
        ulong m = SimdInfo.Mask(k);
        switch (op)
        {
            case SimdOp.Add: return unchecked(x + y) & m;
            case SimdOp.Sub: return unchecked(x - y) & m;
            case SimdOp.Mul: return unchecked(x * y) & m;
            default:
                bool less;
                if (SimdInfo.IsSigned(k))
                {
                    int bits = SimdInfo.Bits(k);
                    less = SimdInfo.SignExtend(x, bits) < SimdInfo.SignExtend(y, bits);
                }
                else less = x < y;
                return (op == SimdOp.Min) == less ? x : y;
        }
    }

    public static LispSimdPack Binary(SimdOp op, LispSimdPack a, LispSimdPack b)
    {
        var k = a.Kind;
        switch (op)
        {
            case SimdOp.And: return new LispSimdPack(k, a.Lo & b.Lo, a.Hi & b.Hi);
            case SimdOp.Or: return new LispSimdPack(k, a.Lo | b.Lo, a.Hi | b.Hi);
            case SimdOp.Xor: return new LispSimdPack(k, a.Lo ^ b.Lo, a.Hi ^ b.Hi);
            case SimdOp.AndC1: return new LispSimdPack(k, ~a.Lo & b.Lo, ~a.Hi & b.Hi);
        }
        var x = SimdInfo.LaneArray(a);
        var y = SimdInfo.LaneArray(b);
        for (int i = 0; i < x.Length; i++) x[i] = LaneBinary(k, op, x[i], y[i]);
        return SimdInfo.FromLanes(k, x);
    }

    public static LispSimdPack Unary(SimdOp op, LispSimdPack a)
    {
        var k = a.Kind;
        if (op == SimdOp.Not) return new LispSimdPack(k, ~a.Lo, ~a.Hi);
        var x = SimdInfo.LaneArray(a);
        for (int i = 0; i < x.Length; i++)
        {
            if (k == SimdKind.F64x2)
            {
                double d = SimdInfo.LaneDouble(x[i]);
                x[i] = SimdInfo.DoubleLane(op == SimdOp.Sqrt ? Math.Sqrt(d) : RoundD(op, d));
            }
            else
            {
                double d = SimdInfo.LaneSingle(x[i]);
                x[i] = SimdInfo.SingleLane((float)(op == SimdOp.Sqrt ? Math.Sqrt(d) : RoundD(op, d)));
            }
        }
        return SimdInfo.FromLanes(k, x);
    }

    public static LispSimdPack Compare(SimdOp op, LispSimdPack a, LispSimdPack b)
    {
        var k = a.Kind;
        var x = SimdInfo.LaneArray(a);
        var y = SimdInfo.LaneArray(b);
        int bits = SimdInfo.Bits(k);
        for (int i = 0; i < x.Length; i++)
        {
            int c; // -1, 0, 1, or 2 for unordered
            if (k == SimdKind.F64x2)
            {
                double p = SimdInfo.LaneDouble(x[i]), q = SimdInfo.LaneDouble(y[i]);
                c = double.IsNaN(p) || double.IsNaN(q) ? 2 : p < q ? -1 : p > q ? 1 : 0;
            }
            else if (k == SimdKind.F32x4)
            {
                float p = SimdInfo.LaneSingle(x[i]), q = SimdInfo.LaneSingle(y[i]);
                c = float.IsNaN(p) || float.IsNaN(q) ? 2 : p < q ? -1 : p > q ? 1 : 0;
            }
            else if (SimdInfo.IsSigned(k))
                c = Math.Sign(SimdInfo.SignExtend(x[i], bits).CompareTo(SimdInfo.SignExtend(y[i], bits)));
            else
                c = Math.Sign(x[i].CompareTo(y[i]));
            bool r = op switch
            {
                SimdOp.Eq => c == 0,
                SimdOp.Ne => c != 0,
                SimdOp.Lt => c == -1,
                SimdOp.Le => c == -1 || c == 0,
                SimdOp.Gt => c == 1,
                _ => c == 1 || c == 0,
            };
            x[i] = r ? ulong.MaxValue : 0;
        }
        return SimdInfo.FromLanes(SimdInfo.MaskKind(k), x);
    }

    public static LispSimdPack Select(LispSimdPack mask, LispSimdPack a, LispSimdPack b) =>
        new(a.Kind, (mask.Lo & a.Lo) | (~mask.Lo & b.Lo), (mask.Hi & a.Hi) | (~mask.Hi & b.Hi));

    /// <summary>Left shift, or right shift: arithmetic for signed lanes, logical otherwise.
    /// N is in 0..lane bits - 1.</summary>
    public static LispSimdPack Shift(bool left, LispSimdPack a, int n)
    {
        var k = a.Kind;
        int bits = SimdInfo.Bits(k);
        ulong m = SimdInfo.Mask(k);
        bool signed = SimdInfo.IsSigned(k);
        var x = SimdInfo.LaneArray(a);
        for (int i = 0; i < x.Length; i++)
            x[i] = left ? (x[i] << n) & m
                 : signed ? (ulong)(SimdInfo.SignExtend(x[i], bits) >> n) & m
                 : x[i] >> n;
        return SimdInfo.FromLanes(k, x);
    }

    /// <summary>Reduces the lanes as a balanced tree: (l0 op l1) op (l2 op l3).</summary>
    public static LispObject Horizontal(SimdOp op, LispSimdPack a)
    {
        var x = SimdInfo.LaneArray(a);
        return SimdInfo.LaneToLisp(a.Kind, Reduce(a.Kind, op, x, 0, x.Length));
    }

    static ulong Reduce(SimdKind k, SimdOp op, ulong[] x, int start, int len)
    {
        if (len == 1) return x[start];
        int h = len / 2;
        return LaneBinary(k, op, Reduce(k, op, x, start, h), Reduce(k, op, x, start + h, h));
    }

    static ulong SatI64(double d) =>
        double.IsNaN(d) ? 0 : d >= 9223372036854775808.0 ? (ulong)long.MaxValue
        : d <= -9223372036854775808.0 ? unchecked((ulong)long.MinValue) : unchecked((ulong)(long)d);

    static ulong SatU64(double d) =>
        double.IsNaN(d) || d <= 0 ? 0 : d >= 18446744073709551616.0 ? ulong.MaxValue : (ulong)d;

    static ulong SatI32(double d) =>
        double.IsNaN(d) ? 0 : d >= 2147483648.0 ? 0x7FFFFFFFUL
        : d <= -2147483648.0 ? 0x80000000UL : (uint)(int)d;

    static ulong SatU32(double d) =>
        double.IsNaN(d) || d <= 0 ? 0 : d >= 4294967296.0 ? uint.MaxValue : (uint)d;

    /// <summary>Value conversion from the lanes of A to kind TO. Float to integer
    /// truncates toward zero and saturates, with NaN as 0. The 64-bit float to 32-bit
    /// float conversions use the low two lanes (the narrowed result has zero upper lanes).</summary>
    public static LispSimdPack Convert(SimdKind to, LispSimdPack a)
    {
        var from = a.Kind;
        var x = SimdInfo.LaneArray(a);
        var r = new ulong[SimdInfo.Lanes(to)];
        for (int i = 0; i < r.Length; i++)
        {
            ulong v = i < x.Length ? x[i] : 0;
            switch (to)
            {
                case SimdKind.F32x4 when from == SimdKind.S32x4: r[i] = SimdInfo.SingleLane((int)(uint)v); break;
                case SimdKind.F32x4 when from == SimdKind.U32x4: r[i] = SimdInfo.SingleLane((uint)v); break;
                case SimdKind.F32x4: r[i] = i < 2 ? SimdInfo.SingleLane((float)SimdInfo.LaneDouble(v)) : 0; break;
                case SimdKind.F64x2 when from == SimdKind.S64x2: r[i] = SimdInfo.DoubleLane((long)v); break;
                case SimdKind.F64x2 when from == SimdKind.U64x2: r[i] = SimdInfo.DoubleLane((double)v); break;
                case SimdKind.F64x2: r[i] = SimdInfo.DoubleLane(SimdInfo.LaneSingle(v)); break;
                case SimdKind.S32x4: r[i] = SatI32(SimdInfo.LaneSingle(v)); break;
                case SimdKind.U32x4: r[i] = SatU32(SimdInfo.LaneSingle(v)); break;
                case SimdKind.S64x2: r[i] = SatI64(SimdInfo.LaneDouble(v)); break;
                default: r[i] = SatU64(SimdInfo.LaneDouble(v)); break;
            }
        }
        return SimdInfo.FromLanes(to, r);
    }
}

#if !NETSTANDARD2_0
/// <summary>Vector128 implementation. Where an API differs between runtimes (float
/// Min/Max, rounding and float to integer conversion changed meaning in .NET 9) the
/// older targets use the scalar code.</summary>
public static class SimdVector
{
    static Vector128<T> V<T>(LispSimdPack p) where T : struct => Vector128.Create(p.Lo, p.Hi).As<ulong, T>();

    static LispSimdPack P<T>(SimdKind k, Vector128<T> v) where T : struct
    {
        var u = v.As<T, ulong>();
        return new LispSimdPack(k, u.GetElement(0), u.GetElement(1));
    }

    static LispSimdPack Bin<T>(SimdOp op, LispSimdPack a, LispSimdPack b) where T : struct
    {
        var x = V<T>(a);
        var y = V<T>(b);
        Vector128<T> r;
        switch (op)
        {
            case SimdOp.Add: r = Vector128.Add(x, y); break;
            case SimdOp.Sub: r = Vector128.Subtract(x, y); break;
            case SimdOp.Mul: r = Vector128.Multiply(x, y); break;
            case SimdOp.Div: r = Vector128.Divide(x, y); break;
            case SimdOp.Min: r = Vector128.Min(x, y); break;
            case SimdOp.Max: r = Vector128.Max(x, y); break;
            case SimdOp.And: r = Vector128.BitwiseAnd(x, y); break;
            case SimdOp.Or: r = Vector128.BitwiseOr(x, y); break;
            case SimdOp.Xor: r = Vector128.Xor(x, y); break;
            default: r = Vector128.AndNot(y, x); break;
        }
        return P(a.Kind, r);
    }

    public static LispSimdPack Binary(SimdOp op, LispSimdPack a, LispSimdPack b)
    {
#if !NET9_0_OR_GREATER
        if ((op == SimdOp.Min || op == SimdOp.Max) && SimdInfo.IsFloat(a.Kind))
            return SimdScalar.Binary(op, a, b);
#endif
        switch (a.Kind)
        {
            case SimdKind.F64x2: return Bin<double>(op, a, b);
            case SimdKind.F32x4: return Bin<float>(op, a, b);
            case SimdKind.S64x2: return Bin<long>(op, a, b);
            case SimdKind.U64x2: return Bin<ulong>(op, a, b);
            case SimdKind.S32x4: return Bin<int>(op, a, b);
            case SimdKind.U32x4: return Bin<uint>(op, a, b);
            case SimdKind.S16x8: return Bin<short>(op, a, b);
            case SimdKind.U16x8: return Bin<ushort>(op, a, b);
            case SimdKind.S8x16: return Bin<sbyte>(op, a, b);
            default: return Bin<byte>(op, a, b);
        }
    }

    public static LispSimdPack Unary(SimdOp op, LispSimdPack a)
    {
        if (op == SimdOp.Not) return new LispSimdPack(a.Kind, ~a.Lo, ~a.Hi);
        if (op == SimdOp.Sqrt)
            return a.Kind == SimdKind.F64x2
                ? P(a.Kind, Vector128.Sqrt(V<double>(a)))
                : P(a.Kind, Vector128.Sqrt(V<float>(a)));
#if NET9_0_OR_GREATER
        if (a.Kind == SimdKind.F64x2)
        {
            var d = V<double>(a);
            return P(a.Kind, op switch
            {
                SimdOp.Round => Vector128.Round(d),
                SimdOp.Floor => Vector128.Floor(d),
                SimdOp.Ceiling => Vector128.Ceiling(d),
                _ => Vector128.Truncate(d),
            });
        }
        var f = V<float>(a);
        return P(a.Kind, op switch
        {
            SimdOp.Round => Vector128.Round(f),
            SimdOp.Floor => Vector128.Floor(f),
            SimdOp.Ceiling => Vector128.Ceiling(f),
            _ => Vector128.Truncate(f),
        });
#else
        return SimdScalar.Unary(op, a);
#endif
    }

    static LispSimdPack Cmp<T>(SimdOp op, LispSimdPack a, LispSimdPack b) where T : struct
    {
        var x = V<T>(a);
        var y = V<T>(b);
        Vector128<T> r = op switch
        {
            SimdOp.Eq => Vector128.Equals(x, y),
            SimdOp.Ne => Vector128.OnesComplement(Vector128.Equals(x, y)),
            SimdOp.Lt => Vector128.LessThan(x, y),
            SimdOp.Le => Vector128.LessThanOrEqual(x, y),
            SimdOp.Gt => Vector128.GreaterThan(x, y),
            _ => Vector128.GreaterThanOrEqual(x, y),
        };
        return P(SimdInfo.MaskKind(a.Kind), r);
    }

    public static LispSimdPack Compare(SimdOp op, LispSimdPack a, LispSimdPack b)
    {
        switch (a.Kind)
        {
            case SimdKind.F64x2: return Cmp<double>(op, a, b);
            case SimdKind.F32x4: return Cmp<float>(op, a, b);
            case SimdKind.S64x2: return Cmp<long>(op, a, b);
            case SimdKind.U64x2: return Cmp<ulong>(op, a, b);
            case SimdKind.S32x4: return Cmp<int>(op, a, b);
            case SimdKind.U32x4: return Cmp<uint>(op, a, b);
            case SimdKind.S16x8: return Cmp<short>(op, a, b);
            case SimdKind.U16x8: return Cmp<ushort>(op, a, b);
            case SimdKind.S8x16: return Cmp<sbyte>(op, a, b);
            default: return Cmp<byte>(op, a, b);
        }
    }

    public static LispSimdPack Select(LispSimdPack mask, LispSimdPack a, LispSimdPack b)
    {
        var r = Vector128.ConditionalSelect(V<ulong>(mask), V<ulong>(a), V<ulong>(b));
        return P(a.Kind, r);
    }

    public static LispSimdPack Shift(bool left, LispSimdPack a, int n)
    {
        var k = a.Kind;
        switch (k)
        {
            case SimdKind.S64x2: { var v = V<long>(a); return P(k, left ? Vector128.ShiftLeft(v, n) : Vector128.ShiftRightArithmetic(v, n)); }
            case SimdKind.U64x2: { var v = V<ulong>(a); return P(k, left ? Vector128.ShiftLeft(v, n) : Vector128.ShiftRightLogical(v, n)); }
            case SimdKind.S32x4: { var v = V<int>(a); return P(k, left ? Vector128.ShiftLeft(v, n) : Vector128.ShiftRightArithmetic(v, n)); }
            case SimdKind.U32x4: { var v = V<uint>(a); return P(k, left ? Vector128.ShiftLeft(v, n) : Vector128.ShiftRightLogical(v, n)); }
            case SimdKind.S16x8: { var v = V<short>(a); return P(k, left ? Vector128.ShiftLeft(v, n) : Vector128.ShiftRightArithmetic(v, n)); }
            case SimdKind.U16x8: { var v = V<ushort>(a); return P(k, left ? Vector128.ShiftLeft(v, n) : Vector128.ShiftRightLogical(v, n)); }
            case SimdKind.S8x16: { var v = V<sbyte>(a); return P(k, left ? Vector128.ShiftLeft(v, n) : Vector128.ShiftRightArithmetic(v, n)); }
            case SimdKind.U8x16: { var v = V<byte>(a); return P(k, left ? Vector128.ShiftLeft(v, n) : Vector128.ShiftRightLogical(v, n)); }
            default: throw new InvalidOperationException("shift of a float pack");
        }
    }

    /// <summary>Integer sums use the vector sum (the order does not matter when wrapping);
    /// float sums and every min and max follow the scalar tree.</summary>
    public static LispObject Horizontal(SimdOp op, LispSimdPack a)
    {
        if (op != SimdOp.Add || SimdInfo.IsFloat(a.Kind)) return SimdScalar.Horizontal(op, a);
        ulong s;
        switch (a.Kind)
        {
            case SimdKind.S64x2: s = (ulong)Vector128.Sum(V<long>(a)); break;
            case SimdKind.U64x2: s = Vector128.Sum(V<ulong>(a)); break;
            case SimdKind.S32x4: s = (ulong)Vector128.Sum(V<int>(a)); break;
            case SimdKind.U32x4: s = Vector128.Sum(V<uint>(a)); break;
            case SimdKind.S16x8: s = (ulong)Vector128.Sum(V<short>(a)); break;
            case SimdKind.U16x8: s = Vector128.Sum(V<ushort>(a)); break;
            case SimdKind.S8x16: s = (ulong)Vector128.Sum(V<sbyte>(a)); break;
            default: s = Vector128.Sum(V<byte>(a)); break;
        }
        return SimdInfo.LaneToLisp(a.Kind, s & SimdInfo.Mask(a.Kind));
    }

    public static LispSimdPack Convert(SimdKind to, LispSimdPack a)
    {
        var from = a.Kind;
        if (to == SimdKind.F32x4 && from == SimdKind.S32x4) return P(to, Vector128.ConvertToSingle(V<int>(a)));
        if (to == SimdKind.F64x2 && from == SimdKind.S64x2) return P(to, Vector128.ConvertToDouble(V<long>(a)));
#if NET9_0_OR_GREATER
        if (to == SimdKind.F32x4 && from == SimdKind.U32x4) return P(to, Vector128.ConvertToSingle(V<uint>(a)));
        if (to == SimdKind.F64x2 && from == SimdKind.U64x2) return P(to, Vector128.ConvertToDouble(V<ulong>(a)));
        if (to == SimdKind.S32x4) return P(to, Vector128.ConvertToInt32(V<float>(a)));
        if (to == SimdKind.U32x4) return P(to, Vector128.ConvertToUInt32(V<float>(a)));
        if (to == SimdKind.S64x2) return P(to, Vector128.ConvertToInt64(V<double>(a)));
        if (to == SimdKind.U64x2) return P(to, Vector128.ConvertToUInt64(V<double>(a)));
#endif
        return SimdScalar.Convert(to, a);
    }
}
#endif

/// <summary>Entry points used by the named functions. ForceScalar selects the
/// reference implementation, for testing it against the Vector128 one.</summary>
public static class SimdOps
{
    public static bool ForceScalar;

    public static LispSimdPack Binary(SimdOp op, LispSimdPack a, LispSimdPack b)
    {
#if !NETSTANDARD2_0
        if (!ForceScalar) return SimdVector.Binary(op, a, b);
#endif
        return SimdScalar.Binary(op, a, b);
    }

    public static LispSimdPack Unary(SimdOp op, LispSimdPack a)
    {
#if !NETSTANDARD2_0
        if (!ForceScalar) return SimdVector.Unary(op, a);
#endif
        return SimdScalar.Unary(op, a);
    }

    public static LispSimdPack Compare(SimdOp op, LispSimdPack a, LispSimdPack b)
    {
#if !NETSTANDARD2_0
        if (!ForceScalar) return SimdVector.Compare(op, a, b);
#endif
        return SimdScalar.Compare(op, a, b);
    }

    public static LispSimdPack Select(LispSimdPack mask, LispSimdPack a, LispSimdPack b)
    {
#if !NETSTANDARD2_0
        if (!ForceScalar) return SimdVector.Select(mask, a, b);
#endif
        return SimdScalar.Select(mask, a, b);
    }

    public static LispSimdPack Shift(bool left, LispSimdPack a, int n)
    {
#if !NETSTANDARD2_0
        if (!ForceScalar) return SimdVector.Shift(left, a, n);
#endif
        return SimdScalar.Shift(left, a, n);
    }

    public static LispObject Horizontal(SimdOp op, LispSimdPack a)
    {
#if !NETSTANDARD2_0
        if (!ForceScalar) return SimdVector.Horizontal(op, a);
#endif
        return SimdScalar.Horizontal(op, a);
    }

    public static LispSimdPack Convert(SimdKind to, LispSimdPack a)
    {
#if !NETSTANDARD2_0
        if (!ForceScalar) return SimdVector.Convert(to, a);
#endif
        return SimdScalar.Convert(to, a);
    }
}
