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
        (value >= CacheMin && value <= CacheMax) ? Cache[value - CacheMin]
        : (ulong)(value - (CacheMax + 1)) < (ulong)MidSize ? MakeMid(value)
        : new Fixnum(value);

    // Above the eager cache, up to MidMax: boxes are made on first use and kept, so a list or an
    // array of small counters does not allocate one box per element, and a process that never
    // sees such numbers pays nothing but the (untouched) array.
    private const int MidSize = (1 << 18) - (CacheMax + 1);
    private static readonly Fixnum?[] Mid = new Fixnum?[MidSize];

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static Fixnum MakeMid(long value)
    {
        int i = (int)(value - (CacheMax + 1));
        return Mid[i] ??= new Fixnum(value);
    }

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
/// scale-preserving number (96-bit mantissa x 10^-scale). Distinct third exactness
/// category, <c>numberp</c>/<c>realp</c>=T but <c>rationalp</c>=<c>floatp</c>=NIL, so
/// that trailing-zero / scale information (1.00m != representation of 1) survives, which a
/// CL ratio would normalize away. It arises only from explicit construction (#m literal),
/// coercion, or .NET interop; standard arithmetic treats it by its exact rational value and
/// yields standard tower types (conservative extension: existing code never meets it).</summary>
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

    // "#m1.50": round-trippable, scale preserved (InvariantCulture "." decimal point).
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

    /// <summary>
    /// Boxes a raw float for compiled code that reads a native float local as a
    /// Lisp object. Kept out of line on purpose. With an inline newobj the raw
    /// value stays live across the allocation helper call, and the System V
    /// x64 ABI has no callee-saved XMM registers, so the JIT keeps the local in
    /// a stack slot for its whole lifetime: a float accumulator in a loop then
    /// pays a store and a reload on every iteration, on its loop-carried
    /// dependency chain. Passing the value as an argument ends its lifetime at
    /// the call, and the loop keeps it in a register.
    /// </summary>
    [MethodImpl(MethodImplOptions.NoInlining)]
    public static SingleFloat Box(float value) => new SingleFloat(value);

    public override string ToString()
    {
        if (float.IsPositiveInfinity(Value)) return Runtime.NonFiniteFloatFormSafe("SINGLE-FLOAT-POSITIVE-INFINITY");
        if (float.IsNegativeInfinity(Value)) return Runtime.NonFiniteFloatFormSafe("SINGLE-FLOAT-NEGATIVE-INFINITY");
        if (float.IsNaN(Value)) return Runtime.NonFiniteFloatFormSafe("SINGLE-FLOAT-NAN");
        var s = FloatText.R(Value);
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

    /// <summary>
    /// Boxes a raw double for compiled code that reads a native double local as a
    /// Lisp object. Kept out of line on purpose. With an inline newobj the raw
    /// value stays live across the allocation helper call, and the System V
    /// x64 ABI has no callee-saved XMM registers, so the JIT keeps the local in
    /// a stack slot for its whole lifetime: a float accumulator in a loop then
    /// pays a store and a reload on every iteration, on its loop-carried
    /// dependency chain. Passing the value as an argument ends its lifetime at
    /// the call, and the loop keeps it in a register.
    /// </summary>
    [MethodImpl(MethodImplOptions.NoInlining)]
    public static DoubleFloat Box(double value) => new DoubleFloat(value);

    public override string ToString()
    {
        if (double.IsPositiveInfinity(Value)) return Runtime.NonFiniteFloatFormSafe("DOUBLE-FLOAT-POSITIVE-INFINITY");
        if (double.IsNegativeInfinity(Value)) return Runtime.NonFiniteFloatFormSafe("DOUBLE-FLOAT-NEGATIVE-INFINITY");
        if (double.IsNaN(Value)) return Runtime.NonFiniteFloatFormSafe("DOUBLE-FLOAT-NAN");
        var s = FloatText.R(Value);
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

/// <summary>The shortest decimal text that reads back as the same float, which is what PRIN1 owes
/// (CLHS 22.1.3.1.3). .NET's "R" is right nearly always; for a few values at a power of two
/// (2^-25 as a double) it drops a digit the reader needs, because the interval below such a value
/// is half as wide as the one above it. Then the text is checked by reading it back, and the
/// 17- (9- for single) digit form replaces it.</summary>
internal static class FloatText
{
    public static string R(double v)
    {
        var inv = System.Globalization.CultureInfo.InvariantCulture;
        var s = v.ToString("R", inv);
        if (double.IsNaN(v) || double.IsInfinity(v)) return s;
        if (double.Parse(s, inv) != v)
        {
            for (int digits = 16; digits <= 17; digits++)
            {
                var t = v.ToString("E" + (digits - 1), inv);
                if (double.Parse(t, inv) == v) return Shorten(t, s);
            }
            return v.ToString("G17", inv);
        }
        return TieUp(s, v);
    }

    public static string R(float v)
    {
        var inv = System.Globalization.CultureInfo.InvariantCulture;
        var s = v.ToString("R", inv);
        if (float.IsNaN(v) || float.IsInfinity(v)) return s;
        if (float.Parse(s, inv) != v)
        {
            for (int digits = 8; digits <= 9; digits++)
            {
                var t = v.ToString("E" + (digits - 1), inv);
                if (float.Parse(t, inv) == v) return Shorten(t, s);
            }
            return v.ToString("G9", inv);
        }
        return TieUp(s, v);
    }

    // When the value is exactly halfway between the shortest digit string S and the next one up
    // (922126456.03515625 between ...562 and ...563), .NET keeps the even last digit and Steele-White
    // free-format printing (SBCL's) takes the larger one. Both read back as V; use the larger.
    private static string TieUp(string s, double v)
    {
        long bits = BitConverter.DoubleToInt64Bits(v);
        int be = (int)((bits >> 52) & 0x7FF);
        if (be == 0x7FF) return s;
        long frac = bits & 0xFFFFFFFFFFFFFL;
        long m64 = be == 0 ? frac : (frac | (1L << 52));
        int exp2 = (be == 0 ? 1 : be) - 1075;
        if (!CanBeTie(m64, exp2, 59900)) return s;
        return TieUpDouble(s, v, m64, exp2);
    }

    // apart from TieUp so the closure it needs is not allocated for the values CanBeTie turns away
    private static string TieUpDouble(string s, double v, long m64, int exp2)
        => TieUpDigits(s, m64, exp2, t => double.Parse(t, System.Globalization.CultureInfo.InvariantCulture) == v);

    private static string TieUp(string s, float v)
    {
        int bits = Compat.SingleToInt32Bits(v);
        int be = (bits >> 23) & 0xFF;
        if (be == 0xFF) return s;
        int frac = bits & 0x7FFFFF;
        long m32 = be == 0 ? frac : (frac | (1 << 23));
        int exp2 = (be == 0 ? 1 : be) - 150;
        if (!CanBeTie(m32, exp2, 33300)) return s;
        return TieUpFloat(s, v, m32, exp2);
    }

    private static string TieUpFloat(string s, float v, long m32, int exp2)
        => TieUpDigits(s, m32, exp2, t => float.Parse(t, System.Globalization.CultureInfo.InvariantCulture) == v);

    // A value can sit exactly halfway between two shortest-digit strings only when its exact decimal
    // expansion is a short finite one: V = M x 2^E with M odd needs -E digits after the point (E < 0)
    // and has about log10(M x 5^-E) significant digits, at most one more than the shortest digits
    // (17 for a double, 9 for a single). LIMIT is that count as a bit length times 1000
    // (10^18 is 2^59.8; 10^10 is 2^33.2). Almost every value fails this with a few integer operations,
    // so the BigInteger test below is for the few that pass.
    private static bool CanBeTie(long mant, int exp2, int limit)
    {
        if (mant == 0) return false;
        // the exponent of a power of two (and of an integer below 2^53) as a double is its bit position
        int tz = (int)((BitConverter.DoubleToInt64Bits((double)(mant & -mant)) >> 52) - 1023);
        int k = -(exp2 + tz);
        if (k < 1 || k > 26) return false;
        int bits = (int)((BitConverter.DoubleToInt64Bits((double)(mant >> tz)) >> 52) - 1023) + 1;
        return bits * 1000 + 2322 * k <= limit;
    }

    private static string TieUpDigits(string s, System.Numerics.BigInteger mant, int exp2, Func<string, bool> readsBack)
    {
        if (mant.IsZero) return s;
        var inv = System.Globalization.CultureInfo.InvariantCulture;
        string body = s.TrimStart('-');
        string sign = s.Length > body.Length ? "-" : "";
        int exp10 = 0;
        int ei = body.IndexOf('E');
        string mpart = body;
        if (ei >= 0)
        {
            exp10 = int.Parse(body.Substring(ei + 1), inv);
            mpart = body.Substring(0, ei);
        }
        int point = mpart.IndexOf('.');
        string digits = mpart.Replace(".", "");
        int intLen = point < 0 ? mpart.Length : point;
        // value = 0.DIGITS x 10^e10, after the leading and trailing zeros are dropped
        int e10 = exp10 + intLen;
        int lead = 0;
        while (lead < digits.Length - 1 && digits[lead] == '0') { lead++; e10--; }
        digits = digits.Substring(lead).TrimEnd('0');
        if (digits.Length == 0 || ((digits[digits.Length - 1] - '0') & 1) != 0) return s;
        // the midpoint (N * 10 + 5) x 10^(e10 - n - 1) must be the value exactly
        var n = System.Numerics.BigInteger.Parse(digits, inv);
        var mid = n * 10 + 5;
        int q = e10 - digits.Length - 1;
        var left = mant;
        var right = mid;
        if (exp2 >= 0) left *= System.Numerics.BigInteger.Pow(2, exp2); else right *= System.Numerics.BigInteger.Pow(2, -exp2);
        if (q >= 0) right *= System.Numerics.BigInteger.Pow(10, q); else left *= System.Numerics.BigInteger.Pow(10, -q);
        if (left != right) return s;
        // raise the last significant digit of S in place (it is even, so there is no carry); the text
        // keeps the shape ("R" chose fixed or scientific) its callers expect
        int endMant = s.IndexOf('E');
        if (endMant < 0) endMant = s.Length;
        int last = endMant - 1;
        while (last >= 0 && s[last] == '0' && s.IndexOf('.') >= 0 && last > s.IndexOf('.')) last--;
        if (last < 0 || !char.IsDigit(s[last])) return s;
        string t = s.Substring(0, last) + (char)(s[last] + 1) + s.Substring(last + 1);
        return readsBack(t) ? t : s;
    }

    // "E16" gives d.dddddddddddddddE-008; return it in the same shape "R" would have used
    // (digits, optional fraction, E and a signed exponent) so the callers' parsing of RAW still works.
    private static string Shorten(string e, string like)
    {
        // The caller of "R" gets either fixed ("0.001") or scientific ("1E-05") text; scientific is
        // always acceptable to them, so hand back the E form without leading zeros in the exponent.
        int ei = e.IndexOf('E');
        var mant = e.Substring(0, ei).TrimEnd('0');
        if (mant.EndsWith(".", System.StringComparison.Ordinal)) mant = mant.Substring(0, mant.Length - 1);
        int exp = int.Parse(e.Substring(ei + 1), System.Globalization.CultureInfo.InvariantCulture);
        return mant + "E" + (exp < 0 ? "-" : "+") + Math.Abs(exp).ToString(System.Globalization.CultureInfo.InvariantCulture);
    }
}
