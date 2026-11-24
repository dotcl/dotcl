using System.Numerics;

namespace DotCL;

internal static class FlagsEnumOps
{
    internal enum Op { Or, And, Xor }

    internal static bool IsEnum(LispObject obj)
        => obj is LispDotNetObject { Value: Enum };

    internal static bool HasEnum(LispObject[] args)
    {
        foreach (var arg in args)
            if (IsEnum(arg)) return true;
        return false;
    }

    internal static LispObject Or2(LispObject a, LispObject b) => Binary(a, b, Op.Or);
    internal static LispObject And2(LispObject a, LispObject b) => Binary(a, b, Op.And);
    internal static LispObject Xor2(LispObject a, LispObject b) => Binary(a, b, Op.Xor);
    internal static LispObject Or(LispObject[] args) => Aggregate(args, Op.Or);
    internal static LispObject And(LispObject[] args) => Aggregate(args, Op.And);
    internal static LispObject Xor(LispObject[] args) => Aggregate(args, Op.Xor);

    internal static LispObject Not(LispObject a)
    {
        var type = RequireFlagsEnum(a, "LOGNOT");
        return Make(type, ~Bits(a));
    }

    internal static LispObject Test(LispObject a, LispObject b)
    {
        ResolveType(a, b, "LOGTEST");
        return (Bits(a) & Bits(b)) != BigInteger.Zero ? T.Instance : Nil.Instance;
    }

    internal static LispObject Unary(LispObject a)
    {
        RequireFlagsEnum(a, "LOGIOR");
        return a;
    }

    private static LispObject Binary(LispObject a, LispObject b, Op op)
    {
        var type = ResolveType(a, b, Name(op));
        return Make(type, Apply(op, Bits(a), Bits(b)));
    }

    private static LispObject Aggregate(LispObject[] args, Op op)
    {
        var type = ResolveType(args, Name(op));
        var result = Identity(op);
        foreach (var arg in args)
            result = Apply(op, result, Bits(arg));
        return Make(type, result);
    }

    private static string Name(Op op) => op switch
    {
        Op.Or => "LOGIOR",
        Op.And => "LOGAND",
        _ => "LOGXOR",
    };

    private static BigInteger Identity(Op op)
        => op == Op.And ? BigInteger.MinusOne : BigInteger.Zero;

    private static BigInteger Apply(Op op, BigInteger a, BigInteger b) => op switch
    {
        Op.Or => a | b,
        Op.And => a & b,
        _ => a ^ b,
    };

    private static Type ResolveType(LispObject a, LispObject b, string op)
    {
        var first = IsEnum(a) ? RequireFlagsEnum(a, op) : null;
        var second = IsEnum(b) ? RequireFlagsEnum(b, op) : null;
        if (first != null && second != null && first != second)
            throw new LispErrorException(new LispTypeError(
                $"{op}: cannot combine {first.FullName} and {second.FullName}: different enum types",
                b));
        return first ?? second!;
    }

    private static Type ResolveType(LispObject[] args, string op)
    {
        Type? type = null;
        foreach (var arg in args)
        {
            if (!IsEnum(arg)) continue;
            var current = RequireFlagsEnum(arg, op);
            if (type == null) type = current;
            else if (type != current)
                throw new LispErrorException(new LispTypeError(
                    $"{op}: cannot combine {type.FullName} and {current.FullName}: different enum types",
                    arg));
        }
        return type!;
    }

    private static Type RequireFlagsEnum(LispObject arg, string op)
    {
        var type = ((Enum)((LispDotNetObject)arg).Value).GetType();
        if (!type.IsDefined(typeof(System.FlagsAttribute), false))
            throw new LispErrorException(new LispTypeError(
                $"{op}: {type.FullName} is not a [Flags] enum", arg));
        return type;
    }

    private static BigInteger Bits(LispObject arg) => arg switch
    {
        LispDotNetObject { Value: Enum e } => EnumBits(e),
        Fixnum f => f.Value,
        Bignum b => b.Value,
        _ => throw new LispErrorException(new LispTypeError("not an integer", arg)),
    };

    private static BigInteger EnumBits(Enum value) => System.Type.GetTypeCode(
        System.Enum.GetUnderlyingType(value.GetType())) switch
    {
        TypeCode.Byte => System.Convert.ToByte(value),
        TypeCode.SByte => System.Convert.ToSByte(value),
        TypeCode.Int16 => System.Convert.ToInt16(value),
        TypeCode.UInt16 => System.Convert.ToUInt16(value),
        TypeCode.Int32 => System.Convert.ToInt32(value),
        TypeCode.UInt32 => System.Convert.ToUInt32(value),
        TypeCode.Int64 => System.Convert.ToInt64(value),
        _ => System.Convert.ToUInt64(value),
    };

    private static readonly BigInteger ByteMask = new(byte.MaxValue);
    private static readonly BigInteger SByteMask = new(byte.MaxValue);
    private static readonly BigInteger Int16Mask = new(ushort.MaxValue);
    private static readonly BigInteger UInt16Mask = new(ushort.MaxValue);
    private static readonly BigInteger Int32Mask = new(uint.MaxValue);
    private static readonly BigInteger UInt32Mask = new(uint.MaxValue);
    private static readonly BigInteger Int64Mask = new(ulong.MaxValue);
    private static readonly BigInteger UInt64Mask = new(ulong.MaxValue);

    private static LispObject Make(Type type, BigInteger bits)
    {
        var underlying = System.Enum.GetUnderlyingType(type);
        object value =
            underlying == typeof(byte) ? (byte)(ulong)(bits & ByteMask) :
            underlying == typeof(sbyte) ? unchecked((sbyte)(byte)(ulong)(bits & SByteMask)) :
            underlying == typeof(short) ? unchecked((short)(ushort)(ulong)(bits & Int16Mask)) :
            underlying == typeof(ushort) ? (ushort)(ulong)(bits & UInt16Mask) :
            underlying == typeof(int) ? unchecked((int)(uint)(ulong)(bits & Int32Mask)) :
            underlying == typeof(uint) ? (uint)(ulong)(bits & UInt32Mask) :
            underlying == typeof(long) ? unchecked((long)(ulong)(bits & Int64Mask)) :
            (ulong)(bits & UInt64Mask);
        return Runtime.DotNetToLisp(System.Enum.ToObject(type, value));
    }
}
