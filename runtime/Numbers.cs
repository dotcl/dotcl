using System.Numerics;
using System.Runtime.CompilerServices;

namespace DotCL;

public abstract class Number : LispObject { }

public sealed class Fixnum : Number
{
    public long Value { get; }

    private const int CacheMin = -128;
    private const int CacheMax = 65535;
    private const int CacheSize = CacheMax - CacheMin + 1;
    private static readonly Fixnum[] Cache = new Fixnum[CacheSize];

    static Fixnum()
    {
        for (int i = 0; i < CacheSize; i++)
            Cache[i] = new Fixnum(i + CacheMin);
    }

    internal Fixnum(long value)
    {
        Value = value;
        DotCL.Diagnostics.AllocCounter.Inc("Fixnum");
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Fixnum Make(long value) =>
        (value >= CacheMin && value <= CacheMax) ? Cache[value - CacheMin] : new Fixnum(value);

    public override string ToString() => Value.ToString();

    public override bool Equals(object? obj) =>
        obj is Fixnum other && Value == other.Value;

    public override int GetHashCode() => Value.GetHashCode();
}

public sealed class Bignum : Number
{
    public BigInteger Value { get; }

    public Bignum(BigInteger value)
    {
        Value = value;
        DotCL.Diagnostics.AllocCounter.Inc("Bignum");
    }

    public static Number MakeInteger(BigInteger value)
    {
        if (value >= long.MinValue && value <= long.MaxValue)
            return Fixnum.Make((long)value);
        return new Bignum(value);
    }

    public override string ToString() => Value.ToString();

    public override bool Equals(object? obj) =>
        obj is Bignum other && Value == other.Value;

    public override int GetHashCode() => Value.GetHashCode();
}

public sealed class Ratio : Number
{
    public BigInteger Numerator { get; }
    public BigInteger Denominator { get; }

    private Ratio(BigInteger num, BigInteger den)
    {
        Numerator = num;
        Denominator = den;
        DotCL.Diagnostics.AllocCounter.Inc("Ratio");
    }

    public static Number Make(BigInteger num, BigInteger den)
    {
        if (den == 0) throw new DivideByZeroException("Division by zero");
        if (den < 0) { num = -num; den = -den; }
        // An integer result needs no reduction. Every exact operation that is not
        // fixnum-by-fixnum lands here with den = 1 (Arithmetic.Add/Subtract/Multiply
        // fall through to the rational form), and reducing by GCD 1 still walks the
        // whole numerator and then divides it twice: BigInteger.op_Division was 43%
        // of a benchmark whose only operation is multiplication.
        if (den.IsOne) return Bignum.MakeInteger(num);
        var gcd = BigInteger.GreatestCommonDivisor(BigInteger.Abs(num), den);
        num /= gcd;
        den /= gcd;
        if (den == 1)
            return Bignum.MakeInteger(num);
        return new Ratio(num, den);
    }

    /// <summary>
    /// A ratio whose numerator and denominator the caller knows to be coprime.
    /// Skips the GCD, which is the whole cost where that promise can be made:
    /// reducing two hundred-digit numbers by a divisor known in advance to be 1.
    /// Sign normalisation and the collapse to an integer still happen here.
    ///
    /// Only for callers that can prove it. MAKE is the one to use otherwise.
    /// </summary>
    public static Number MakeReduced(BigInteger num, BigInteger den)
    {
        if (den == 0) throw new DivideByZeroException("Division by zero");
        if (den < 0) { num = -num; den = -den; }
        // A zero numerator is the integer zero whatever denominator it arrives
        // with, and a cancelling sum can arrive with one larger than 1.
        if (den.IsOne || num.IsZero) return Bignum.MakeInteger(num);
        return new Ratio(num, den);
    }

    public override string ToString() => $"{Numerator}/{Denominator}";

    public override bool Equals(object? obj) =>
        obj is Ratio other && Numerator == other.Numerator && Denominator == other.Denominator;

    public override int GetHashCode() => HashCode.Combine(Numerator, Denominator);
}

/// <summary>A first-class CLR <see cref="decimal"/> (System.Decimal) value: a base-10,
/// scale-preserving number (96-bit mantissa × 10^-scale). Distinct third exactness
/// category — <c>numberp</c>/<c>realp</c>=T but <c>rationalp</c>=<c>floatp</c>=NIL — so
/// that trailing-zero / scale information (1.00m ≠ representation of 1) survives, which a
/// CL ratio would normalize away. It arises only from explicit construction (#m literal),
/// coercion, or .NET interop; standard arithmetic treats it by its exact rational value and
/// yields standard tower types (conservative extension — existing code never meets it).</summary>
public class LispDecimal : Number
{
    public decimal Value { get; }

    public LispDecimal(decimal value)
    {
        Value = value;
        DotCL.Diagnostics.AllocCounter.Inc("LispDecimal");
    }

    /// <summary>Exact rational value (num/den, un-normalized denominator = 10^scale).
    /// Always exact: a decimal is mantissa/10^scale by construction.</summary>
    public (BigInteger Num, BigInteger Den) AsRatio()
    {
        int[] bits = decimal.GetBits(Value);
        BigInteger mantissa = ((BigInteger)(uint)bits[2] << 64)
            | ((BigInteger)(uint)bits[1] << 32) | (uint)bits[0];
        int flags = bits[3];
        int scale = (flags >> 16) & 0xFF;              // 0..28
        bool negative = (flags & unchecked((int)0x80000000)) != 0;
        var num = negative ? -mantissa : mantissa;
        var den = BigInteger.Pow(10, scale);
        return (num, den);
    }

    /// <summary>The decimal's value as a normalized CL rational (Fixnum/Bignum/Ratio).</summary>
    public Number ToRational()
    {
        var (num, den) = AsRatio();
        return (Number)Ratio.Make(num, den);
    }

    // "#m1.50" — round-trippable, scale preserved (InvariantCulture "." decimal point).
    public override string ToString() =>
        "#m" + Value.ToString(System.Globalization.CultureInfo.InvariantCulture);

    // EQL is representation-sensitive (scale included): 1.0m and 1.00m are NOT eql,
    // though = compares them equal (via the exact rational value in the tower). GetBits
    // captures mantissa+scale+sign, so its sequence equality distinguishes scales.
    public override bool Equals(object? obj)
    {
        if (obj is not LispDecimal other) return false;
        var a = decimal.GetBits(Value);
        var b = decimal.GetBits(other.Value);
        return a[0] == b[0] && a[1] == b[1] && a[2] == b[2] && a[3] == b[3];
    }

    public override int GetHashCode() => Value.GetHashCode();
}

public sealed class SingleFloat : Number
{
    public float Value { get; }

    public SingleFloat(float value)
    {
        Value = value;
        DotCL.Diagnostics.AllocCounter.Inc("SingleFloat");
    }

    public override string ToString()
    {
        if (float.IsPositiveInfinity(Value)) return Runtime.NonFiniteFloatFormSafe("SINGLE-FLOAT-POSITIVE-INFINITY");
        if (float.IsNegativeInfinity(Value)) return Runtime.NonFiniteFloatFormSafe("SINGLE-FLOAT-NEGATIVE-INFINITY");
        if (float.IsNaN(Value)) return Runtime.NonFiniteFloatFormSafe("SINGLE-FLOAT-NAN");
        var s = Value.ToString("R");
        if (s.Contains('E') || s.Contains('e'))
            return s;
        if (!s.Contains('.'))
            return s + ".0";
        return s;
    }

    public override bool Equals(object? obj) =>
        obj is SingleFloat other && Value == other.Value;

    public override int GetHashCode() => Value.GetHashCode();
}

public sealed class DoubleFloat : Number
{
    public double Value { get; }

    public DoubleFloat(double value)
    {
        Value = value;
        DotCL.Diagnostics.AllocCounter.Inc("DoubleFloat");
    }

    public override string ToString()
    {
        if (double.IsPositiveInfinity(Value)) return Runtime.NonFiniteFloatFormSafe("DOUBLE-FLOAT-POSITIVE-INFINITY");
        if (double.IsNegativeInfinity(Value)) return Runtime.NonFiniteFloatFormSafe("DOUBLE-FLOAT-NEGATIVE-INFINITY");
        if (double.IsNaN(Value)) return Runtime.NonFiniteFloatFormSafe("DOUBLE-FLOAT-NAN");
        var s = Value.ToString("R");
        if (s.Contains('E') || s.Contains('e'))
            return s.Replace("E", "d").Replace("e", "d");
        if (!s.Contains('.'))
            return s + ".0d0";
        return s + "d0";
    }

    public override bool Equals(object? obj) =>
        obj is DoubleFloat other && Value == other.Value;

    public override int GetHashCode() => Value.GetHashCode();
}

/// <summary>
/// A complex number. Two representations, one meaning: DoubleComplex keeps the
/// parts as raw doubles, BoxedComplex keeps them as Number objects.
///
/// Complex arithmetic over doubles is the common case and it used to cost three
/// objects per operation -- the complex plus a DoubleFloat for each part -- where
/// one is enough. Which representation a value gets is decided once, in OF: every
/// double/double pair becomes a DoubleComplex, so the two never describe the same
/// value and nothing has to compare across them.
/// </summary>
public abstract class LispComplex : Number
{
    public abstract Number Real { get; }
    public abstract Number Imaginary { get; }

    /// <summary>The complex with these parts, in whichever representation fits.
    /// Every construction goes through here (or through OfDoubles).</summary>
    public static LispComplex Of(Number real, Number imaginary) =>
        real is DoubleFloat dr && imaginary is DoubleFloat di
            ? new DoubleComplex(dr.Value, di.Value)
            : new BoxedComplex(real, imaginary);

    public static LispComplex OfDoubles(double real, double imaginary) =>
        new DoubleComplex(real, imaginary);

    public override string ToString() => $"#C({Real} {Imaginary})";

    public override bool Equals(object? obj) =>
        obj is LispComplex other && Real.Equals(other.Real) && Imaginary.Equals(other.Imaginary);

    public override int GetHashCode() => HashCode.Combine(Real, Imaginary);
}

/// <summary>Both parts double: the values live in the object, unboxed.</summary>
public sealed class DoubleComplex : LispComplex
{
    public readonly double RealValue;
    public readonly double ImagValue;

    public DoubleComplex(double real, double imaginary)
    {
        RealValue = real;
        ImagValue = imaginary;
    }

    // Each read boxes. Arithmetic on complex doubles reads the fields instead,
    // which is the point of this class.
    public override Number Real => new DoubleFloat(RealValue);
    public override Number Imaginary => new DoubleFloat(ImagValue);
}

/// <summary>Anything else: rational parts, single floats, mixed exact values.</summary>
public sealed class BoxedComplex : LispComplex
{
    private readonly Number _real;
    private readonly Number _imaginary;

    public BoxedComplex(Number real, Number imaginary)
    {
        _real = real;
        _imaginary = imaginary;
    }

    public override Number Real => _real;
    public override Number Imaginary => _imaginary;
}
