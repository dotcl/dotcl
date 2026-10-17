namespace DotCL;

public enum SimdKind : byte { F64x2, F32x4, S64x2, U64x2, S32x4, U32x4, S16x8, U16x8, S8x16, U8x16 }

/// <summary>An immutable 128-bit value: ten lane layouts over the same two words.</summary>
public sealed class LispSimdPack : LispObject
{
    public readonly SimdKind Kind;
    public readonly ulong Lo, Hi;

    public LispSimdPack(SimdKind kind, ulong lo, ulong hi)
    {
        Kind = kind;
        Lo = lo;
        Hi = hi;
    }

    public bool SameBits(LispSimdPack o) => Kind == o.Kind && Lo == o.Lo && Hi == o.Hi;

    public override string ToString() => SimdInfo.Print(this);
}

public static class SimdInfo
{
    public const int KindCount = 10;

    public static readonly string[] Names =
        { "F64.2", "F32.4", "S64.2", "U64.2", "S32.4", "U32.4", "S16.8", "U16.8", "S8.16", "U8.16" };
    static readonly int[] LaneCounts = { 2, 4, 2, 2, 4, 4, 8, 8, 16, 16 };
    static readonly int[] LaneBits = { 64, 32, 64, 64, 32, 32, 16, 16, 8, 8 };
    static readonly string[] ElementTypes =
    {
        "DOUBLE-FLOAT", "SINGLE-FLOAT", "SIGNED-BYTE-64", "UNSIGNED-BYTE-64", "SIGNED-BYTE-32",
        "UNSIGNED-BYTE-32", "SIGNED-BYTE-16", "UNSIGNED-BYTE-16", "SIGNED-BYTE-8", "UNSIGNED-BYTE-8",
    };

    public static int Lanes(SimdKind k) => LaneCounts[(int)k];
    public static int Bits(SimdKind k) => LaneBits[(int)k];
    public static string Name(SimdKind k) => Names[(int)k];
    public static string ElementType(SimdKind k) => ElementTypes[(int)k];
    public static LispObject ElementTypeSpec(SimdKind k)
    {
        if (IsFloat(k)) return Startup.Sym(ElementType(k));
        return Runtime.List(Startup.Sym(IsSigned(k) ? "SIGNED-BYTE" : "UNSIGNED-BYTE"), Fixnum.Make(Bits(k)));
    }

    public static bool IsFloat(SimdKind k) => k <= SimdKind.F32x4;
    public static bool IsSigned(SimdKind k) => k is SimdKind.S64x2 or SimdKind.S32x4 or SimdKind.S16x8 or SimdKind.S8x16;
    public static ulong Mask(SimdKind k) => Bits(k) == 64 ? ulong.MaxValue : (1UL << Bits(k)) - 1;

    public static SimdKind MaskKind(SimdKind k) => Bits(k) switch
    {
        64 => SimdKind.U64x2,
        32 => SimdKind.U32x4,
        16 => SimdKind.U16x8,
        _ => SimdKind.U8x16,
    };

    public static long SignExtend(ulong v, int bits) => bits == 64 ? (long)v : (long)(v << (64 - bits)) >> (64 - bits);

    public static ulong Lane(LispSimdPack p, int i)
    {
        int bits = Bits(p.Kind);
        int per = 64 / bits;
        ulong w = i < per ? p.Lo : p.Hi;
        return (w >> ((i % per) * bits)) & Mask(p.Kind);
    }

    public static ulong[] LaneArray(LispSimdPack p)
    {
        var r = new ulong[Lanes(p.Kind)];
        for (int i = 0; i < r.Length; i++) r[i] = Lane(p, i);
        return r;
    }

    public static LispSimdPack FromLanes(SimdKind k, ulong[] lanes)
    {
        int bits = Bits(k);
        int per = 64 / bits;
        ulong m = Mask(k);
        ulong lo = 0, hi = 0;
        for (int i = 0; i < lanes.Length; i++)
        {
            ulong v = (lanes[i] & m) << ((i % per) * bits);
            if (i < per) lo |= v; else hi |= v;
        }
        return new LispSimdPack(k, lo, hi);
    }

    public static double LaneDouble(ulong bits) => BitConverter.Int64BitsToDouble((long)bits);
    public static ulong DoubleLane(double d) => (ulong)BitConverter.DoubleToInt64Bits(d);
    public static float LaneSingle(ulong bits) => Compat.Int32BitsToSingle((int)(uint)bits);
    public static ulong SingleLane(float f) => (uint)Compat.SingleToInt32Bits(f);

    public static LispObject LaneToLisp(SimdKind k, ulong bits)
    {
        switch (k)
        {
            case SimdKind.F64x2: return new DoubleFloat(LaneDouble(bits));
            case SimdKind.F32x4: return new SingleFloat(LaneSingle(bits));
            case SimdKind.U64x2: return Runtime.BoxU64((long)bits);
            default:
                return IsSigned(k) ? Fixnum.Make(SignExtend(bits, Bits(k))) : Fixnum.Make((long)bits);
        }
    }

    public static LispTypeError TypeError(string fn, LispObject datum, SimdKind k) =>
        new LispTypeError($"{fn}: {Runtime.FormatObject(datum, true)} is not of type {Name(k)}",
            datum, Startup.SymInPkg(Name(k), "DOTCL-SIMD"));

    static LispTypeError LaneError(string fn, LispObject datum, SimdKind k)
    {
        LispObject et = IsFloat(k)
            ? Startup.Sym(k == SimdKind.F64x2 ? "DOUBLE-FLOAT" : "SINGLE-FLOAT")
            : Runtime.List(Startup.Sym(IsSigned(k) ? "SIGNED-BYTE" : "UNSIGNED-BYTE"), Fixnum.Make(Bits(k)));
        return new LispTypeError($"{fn}: {Runtime.FormatObject(datum, true)} is not of type {Runtime.FormatObject(et, true)}",
            datum, et);
    }

    public static ulong LaneFromLisp(SimdKind k, LispObject o, string fn)
    {
        switch (k)
        {
            case SimdKind.F64x2:
                if (o is DoubleFloat d) return DoubleLane(d.Value);
                break;
            case SimdKind.F32x4:
                if (o is SingleFloat s) return SingleLane(s.Value);
                break;
            default:
                System.Numerics.BigInteger v;
                if (o is Fixnum f) v = f.Value;
                else if (o is Bignum b) v = b.Value;
                else break;
                int bits = Bits(k);
                bool ok = IsSigned(k)
                    ? v >= -(System.Numerics.BigInteger.One << (bits - 1)) && v < (System.Numerics.BigInteger.One << (bits - 1))
                    : v.Sign >= 0 && v < (System.Numerics.BigInteger.One << bits);
                if (!ok) break;
                return IsSigned(k) ? (ulong)(long)v & Mask(k) : (ulong)v;
        }
        throw new LispErrorException(LaneError(fn, o, k));
    }

    public static LispSimdPack Broadcast(SimdKind k, ulong lane)
    {
        var a = new ulong[Lanes(k)];
        for (int i = 0; i < a.Length; i++) a[i] = lane;
        return FromLanes(k, a);
    }

    /// <summary>A pack of kind K, or a number broadcast to every lane.</summary>
    public static LispSimdPack Cast(SimdKind k, LispObject o, string fn)
    {
        o = Runtime.Primary(o);
        if (o is LispSimdPack p)
        {
            if (p.Kind == k) return p;
            throw new LispErrorException(TypeError(fn, o, k));
        }
        if (o is Number) return Broadcast(k, LaneFromLisp(k, o, fn));
        throw new LispErrorException(TypeError(fn, o, k));
    }

    public static LispSimdPack Pack(LispObject o, string fn)
    {
        o = Runtime.Primary(o);
        if (o is LispSimdPack p) return p;
        throw new LispErrorException(new LispTypeError($"{fn}: {Runtime.FormatObject(o, true)} is not a SIMD pack", o,
            Startup.SymInPkg("SIMD-PACK", "DOTCL-SIMD")));
    }

    public static string Print(LispSimdPack p)
    {
        var sb = new System.Text.StringBuilder("#<").Append(Name(p.Kind));
        for (int i = 0; i < Lanes(p.Kind); i++)
            sb.Append(' ').Append(Runtime.FormatObject(LaneToLisp(p.Kind, Lane(p, i)), true));
        return sb.Append('>').ToString();
    }
}
