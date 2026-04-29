using System.Numerics;
using System.Runtime.CompilerServices;

namespace DotCL;

public abstract class Number : LispObject { }

public class Fixnum : Number
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

public class Bignum : Number
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

public class Ratio : Number
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
        var gcd = BigInteger.GreatestCommonDivisor(BigInteger.Abs(num), den);
        num /= gcd;
        den /= gcd;
        if (den == 1)
            return Bignum.MakeInteger(num);
        return new Ratio(num, den);
    }

    public override string ToString() => $"{Numerator}/{Denominator}";

    public override bool Equals(object? obj) =>
        obj is Ratio other && Numerator == other.Numerator && Denominator == other.Denominator;

    public override int GetHashCode() => HashCode.Combine(Numerator, Denominator);
}

public class SingleFloat : Number
{
    public float Value { get; }

    public SingleFloat(float value)
    {
        Value = value;
        DotCL.Diagnostics.AllocCounter.Inc("SingleFloat");
    }

    public override string ToString()
    {
        if (float.IsPositiveInfinity(Value)) return "#.SINGLE-FLOAT-POSITIVE-INFINITY";
        if (float.IsNegativeInfinity(Value)) return "#.SINGLE-FLOAT-NEGATIVE-INFINITY";
        if (float.IsNaN(Value)) return "#.SINGLE-FLOAT-NAN";
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

public class DoubleFloat : Number
{
    public double Value { get; }

    public DoubleFloat(double value)
    {
        Value = value;
        DotCL.Diagnostics.AllocCounter.Inc("DoubleFloat");
    }

    public override string ToString()
    {
        if (double.IsPositiveInfinity(Value)) return "#.DOUBLE-FLOAT-POSITIVE-INFINITY";
        if (double.IsNegativeInfinity(Value)) return "#.DOUBLE-FLOAT-NEGATIVE-INFINITY";
        if (double.IsNaN(Value)) return "#.DOUBLE-FLOAT-NAN";
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

public class LispComplex : Number
{
    public Number Real { get; }
    public Number Imaginary { get; }

    public LispComplex(Number real, Number imaginary)
    {
        Real = real;
        Imaginary = imaginary;
    }

    public override string ToString() => $"#C({Real} {Imaginary})";

    public override bool Equals(object? obj) =>
        obj is LispComplex other && Real.Equals(other.Real) && Imaginary.Equals(other.Imaginary);

    public override int GetHashCode() => HashCode.Combine(Real, Imaginary);
}
