using System.Numerics;
using System.Runtime.CompilerServices;

namespace DotCL;

public static class Arithmetic
{
    // --- Addition ---
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Number Add(Number a, Number b)
    {
        // Fixnum fast path — avoid BigInteger when result fits in long
        if (a is Fixnum fa && b is Fixnum fb)
        {
            long av = fa.Value, bv = fb.Value, r = av + bv;
            // Overflow check: sign of result must be consistent
            if (((av ^ bv) < 0) || ((av ^ r) >= 0))
                return Fixnum.Make(r);
            return new Bignum((BigInteger)av + bv);
        }

        // SingleFloat fast path — common in numeric code
        if (a is SingleFloat sfa && b is SingleFloat sfb)
            return new SingleFloat(sfa.Value + sfb.Value);

        // DoubleFloat fast path
        if (a is DoubleFloat da && b is DoubleFloat db)
            return new DoubleFloat(da.Value + db.Value);

        // Complex contagion
        if (a is LispComplex || b is LispComplex)
        {
            var ac = AsComplex(a);
            var bc = AsComplex(b);
            return MakeComplex(Add(ac.Real, bc.Real), Add(ac.Imaginary, bc.Imaginary));
        }

        // Float contagion
        if (a is DoubleFloat || b is DoubleFloat)
            return new DoubleFloat(ToDouble(a) + ToDouble(b));
        if (a is SingleFloat || b is SingleFloat)
            return new SingleFloat(ToSingle(a) + ToSingle(b));

        // Exact arithmetic
        var (an, ad) = AsRational(a);
        var (bn, bd) = AsRational(b);
        return (Number)Ratio.Make(an * bd + bn * ad, ad * bd);
    }

    // --- Subtraction ---
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Number Subtract(Number a, Number b)
    {
        // Fixnum fast path
        if (a is Fixnum fa && b is Fixnum fb)
        {
            long av = fa.Value, bv = fb.Value, r = av - bv;
            if (((av ^ bv) >= 0) || ((av ^ r) >= 0))
                return Fixnum.Make(r);
            return new Bignum((BigInteger)av - bv);
        }

        // SingleFloat fast path
        if (a is SingleFloat sfa && b is SingleFloat sfb)
            return new SingleFloat(sfa.Value - sfb.Value);

        // DoubleFloat fast path
        if (a is DoubleFloat da && b is DoubleFloat db)
            return new DoubleFloat(da.Value - db.Value);

        if (a is LispComplex || b is LispComplex)
        {
            var ac = AsComplex(a);
            var bc = AsComplex(b);
            return MakeComplex(Subtract(ac.Real, bc.Real), Subtract(ac.Imaginary, bc.Imaginary));
        }

        if (a is DoubleFloat || b is DoubleFloat)
            return new DoubleFloat(ToDouble(a) - ToDouble(b));
        if (a is SingleFloat || b is SingleFloat)
            return new SingleFloat(ToSingle(a) - ToSingle(b));

        var (an, ad) = AsRational(a);
        var (bn, bd) = AsRational(b);
        return (Number)Ratio.Make(an * bd - bn * ad, ad * bd);
    }

    // --- Multiplication ---
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Number Multiply(Number a, Number b)
    {
        // Fixnum fast path — use checked to detect overflow
        if (a is Fixnum fa && b is Fixnum fb)
        {
            try { return Fixnum.Make(checked(fa.Value * fb.Value)); }
            catch (OverflowException) { return new Bignum((BigInteger)fa.Value * fb.Value); }
        }

        // SingleFloat fast path
        if (a is SingleFloat sfa && b is SingleFloat sfb)
            return new SingleFloat(sfa.Value * sfb.Value);

        // DoubleFloat fast path
        if (a is DoubleFloat da && b is DoubleFloat db)
            return new DoubleFloat(da.Value * db.Value);

        if (a is LispComplex || b is LispComplex)
        {
            var ac = AsComplex(a);
            var bc = AsComplex(b);
            // (a+bi)(c+di) = (ac-bd) + (ad+bc)i
            return MakeComplex(
                Subtract(Multiply(ac.Real, bc.Real), Multiply(ac.Imaginary, bc.Imaginary)),
                Add(Multiply(ac.Real, bc.Imaginary), Multiply(ac.Imaginary, bc.Real)));
        }

        if (a is DoubleFloat || b is DoubleFloat)
            return new DoubleFloat(ToDouble(a) * ToDouble(b));
        if (a is SingleFloat || b is SingleFloat)
            return new SingleFloat(ToSingle(a) * ToSingle(b));

        var (an, ad) = AsRational(a);
        var (bn, bd) = AsRational(b);
        return (Number)Ratio.Make(an * bn, ad * bd);
    }

    // --- Division ---
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Number Divide(Number a, Number b)
    {
        if (a is LispComplex || b is LispComplex)
        {
            var ac = AsComplex(a);
            var bc = AsComplex(b);
            // (a+bi)/(c+di) = ((ac+bd) + (bc-ad)i) / (c²+d²)
            var denom = Add(Multiply(bc.Real, bc.Real), Multiply(bc.Imaginary, bc.Imaginary));
            return MakeComplex(
                Divide(Add(Multiply(ac.Real, bc.Real), Multiply(ac.Imaginary, bc.Imaginary)), denom),
                Divide(Subtract(Multiply(ac.Imaginary, bc.Real), Multiply(ac.Real, bc.Imaginary)), denom));
        }

        if (a is DoubleFloat || b is DoubleFloat)
        {
            double bval = ToDouble(b);
            if (bval == 0.0) throw new DivideByZeroException("Division by zero");
            return new DoubleFloat(ToDouble(a) / bval);
        }
        if (a is SingleFloat || b is SingleFloat)
        {
            float bval2 = ToSingle(b);
            if (bval2 == 0.0f) throw new DivideByZeroException("Division by zero");
            return new SingleFloat(ToSingle(a) / bval2);
        }

        var (an, ad) = AsRational(a);
        var (bn, bd) = AsRational(b);
        return (Number)Ratio.Make(an * bd, ad * bn);
    }

    // --- Negate ---
    public static Number Negate(Number a)
    {
        return a switch
        {
            Fixnum f => f.Value != long.MinValue ? (Number)Fixnum.Make(-f.Value)
                : (Number)Bignum.MakeInteger(-(System.Numerics.BigInteger)f.Value),
            Bignum b => (Number)Bignum.MakeInteger(-b.Value),
            Ratio => Subtract(Fixnum.Make(0), a),
            SingleFloat sf => new SingleFloat(-sf.Value),
            DoubleFloat df => new DoubleFloat(-df.Value),
            LispComplex c => MakeComplex(Negate(c.Real), Negate(c.Imaginary)),
            _ => throw new NotImplementedException()
        };
    }

    // --- Abs ---
    public static Number Abs(Number a)
    {
        return a switch
        {
            Fixnum f => f.Value >= 0 ? f : (f.Value == long.MinValue ? (Number)new Bignum(-((BigInteger)f.Value)) : Fixnum.Make(System.Math.Abs(f.Value))),
            Bignum b => b.Value >= 0 ? b : new Bignum(BigInteger.Abs(b.Value)),
            Ratio r => r.Numerator >= 0 ? a : (Number)Ratio.Make(-r.Numerator, r.Denominator),
            SingleFloat sf => new SingleFloat(System.Math.Abs(sf.Value)),
            DoubleFloat df => new DoubleFloat(System.Math.Abs(df.Value)),
            LispComplex c => (c.Real is SingleFloat && c.Imaginary is SingleFloat)
                ? (Number)new SingleFloat((float)System.Math.Sqrt(
                    System.Math.Pow(ToDouble(c.Real), 2) + System.Math.Pow(ToDouble(c.Imaginary), 2)))
                : new DoubleFloat(System.Math.Sqrt(
                    System.Math.Pow(ToDouble(c.Real), 2) + System.Math.Pow(ToDouble(c.Imaginary), 2))),
            _ => throw new NotImplementedException()
        };
    }

    // --- Modular arithmetic ---
    public static Number Mod(Number a, Number b)
    {
        // Fast path: both Fixnum
        if (a is Fixnum fa && b is Fixnum fb)
        {
            long av = fa.Value, bv = fb.Value;
            long r = av % bv;
            // CL mod: result has same sign as divisor
            if (r != 0 && ((r ^ bv) < 0)) r += bv;
            return Fixnum.Make(r);
        }
        // Float contagion: if either arg is float, use float arithmetic
        if (a is SingleFloat || b is SingleFloat || a is DoubleFloat || b is DoubleFloat)
        {
            double ad = AsDouble(a), bd = AsDouble(b);
            double q = Math.Floor(ad / bd);
            double result = ad - q * bd;
            if (a is DoubleFloat || b is DoubleFloat)
                return new DoubleFloat(result);
            return new SingleFloat((float)result);
        }
        var (an, _) = AsRational(a);
        var (bn, _) = AsRational(b);
        var iresult = an % bn;
        // CL mod: result has same sign as divisor
        if (iresult != 0 && (iresult < 0) != (bn < 0))
            iresult += bn;
        return (Number)Bignum.MakeInteger(iresult);
    }

    public static Number Rem(Number a, Number b)
    {
        // Float contagion: if either arg is float, use float arithmetic
        if (a is SingleFloat || b is SingleFloat || a is DoubleFloat || b is DoubleFloat)
        {
            double ad = AsDouble(a), bd = AsDouble(b);
            double q = Math.Truncate(ad / bd);
            double result = ad - q * bd;
            if (a is DoubleFloat || b is DoubleFloat)
                return new DoubleFloat(result);
            return new SingleFloat((float)result);
        }
        var (an, _) = AsRational(a);
        var (bn, _) = AsRational(b);
        return (Number)Bignum.MakeInteger(an % bn);
    }

    // IEEE 754 double has an 11-bit exponent field with bias 1023, giving a
    // maximum exponent of 1023. We subtract 3 as a safety margin so that
    // the shifted numerator/denominator still fit comfortably within the
    // representable exponent range before casting to double.
    // 1020 = 1023 (max double exponent) - 3 (safety margin)
    private const int DoubleExponentSafeShift = 1020;

    // Convert BigInteger to double with IEEE 754 round-to-nearest-even.
    // C#'s (double)BigInteger is not guaranteed to be correctly rounded for large values.
    private static double BigIntToDouble(BigInteger value)
    {
        if (value.IsZero) return 0.0;
        bool negative = value.Sign < 0;
        BigInteger abs = BigInteger.Abs(value);
        int bitLength = (int)abs.GetBitLength();
        double result;
        if (bitLength <= 53)
        {
            result = (double)(ulong)abs;
        }
        else
        {
            int shift = bitLength - 53;
            BigInteger mantissa = abs >> shift;  // top 53 bits
            // Round bit: bit just below the cut
            bool roundBit = !((abs >> (shift - 1)).IsEven);
            // Sticky bit: any bits below the round bit
            bool stickyBit = shift > 1 && (abs & ((BigInteger.One << (shift - 1)) - BigInteger.One)) != BigInteger.Zero;
            // Round-to-nearest-even
            if (roundBit && (stickyBit || !mantissa.IsEven))
            {
                mantissa++;
                if ((int)mantissa.GetBitLength() > 53) { mantissa >>= 1; shift++; }
            }
            result = Math.ScaleB((double)(ulong)mantissa, shift);
        }
        return negative ? -result : result;
    }

    // Convert BigInteger ratio to double with IEEE 754 round-to-nearest-even.
    // Uses shift-and-divide so subnormals (exponent < -1022) are produced
    // correctly. Naive (double)num / (double)den underflows to 0 whenever
    // num or den overflow double range even if the ratio itself is representable.
    internal static double RatioToDouble(Ratio r)
    {
        var num = r.Numerator;
        var den = r.Denominator;
        if (num.IsZero) return 0.0;
        bool negative = (num.Sign < 0) ^ (den.Sign < 0);
        num = BigInteger.Abs(num);
        den = BigInteger.Abs(den);

        int numBits = (int)num.GetBitLength();
        int denBits = (int)den.GetBitLength();

        // Choose shift so that (num << shift) / den yields a quotient of ~54
        // bits (53 mantissa + 1 guard). For shift < 0 we shift num down and
        // record the dropped low bits for stickiness.
        int shift = 54 - numBits + denBits;
        BigInteger scaledNum;
        BigInteger droppedBits = BigInteger.Zero;
        if (shift >= 0)
        {
            scaledNum = num << shift;
        }
        else
        {
            int ds = -shift;
            droppedBits = num & ((BigInteger.One << ds) - BigInteger.One);
            scaledNum = num >> ds;
        }

        var q = BigInteger.DivRem(scaledNum, den, out var rem);

        // Normalize so q has exactly 54 bits (handles ±1 bit from estimate)
        int qBits = (int)q.GetBitLength();
        while (qBits < 54)
        {
            q <<= 1;
            rem <<= 1;
            if (rem >= den) { rem -= den; q |= BigInteger.One; }
            shift++;
            qBits++;
        }
        while (qBits > 54)
        {
            if (!q.IsEven) droppedBits = BigInteger.One; // record stickiness
            q >>= 1;
            shift--;
            qBits--;
        }

        // Extract guard bit; sticky is any dropped low bits or nonzero remainder.
        bool guard = !q.IsEven;
        q >>= 1;
        shift--;
        bool sticky = !rem.IsZero || !droppedBits.IsZero;

        // Round to nearest, ties to even.
        if (guard && (sticky || !q.IsEven))
        {
            q++;
            if ((int)q.GetBitLength() > 53)
            {
                q >>= 1;
                shift++;
            }
        }

        if (q.IsZero) return negative ? -0.0 : 0.0;

        // Value is q * 2^-shift. Math.ScaleB handles subnormal range.
        double result = Math.ScaleB((double)(ulong)q, -shift);
        if (double.IsInfinity(result))
            return negative ? double.NegativeInfinity : double.PositiveInfinity;
        return negative ? -result : result;
    }

    private static double AsDouble(Number n) => n switch
    {
        Fixnum f => (double)f.Value,
        Bignum b => BigIntToDouble(b.Value),
        SingleFloat sf => sf.Value,
        DoubleFloat df => df.Value,
        Ratio r => RatioToDouble(r),
        _ => throw new ArgumentException($"Not a real number: {n}")
    };

    // --- Comparison ---
    public static bool IsNumericEqual(Number a, Number b)
    {
        if (a is Fixnum fa && b is Fixnum fb) return fa.Value == fb.Value;
        // DoubleFloat fast path
        if (a is DoubleFloat da && b is DoubleFloat db) return da.Value == db.Value;
        return IsNumericEqualSlow(a, b);
    }

    private static bool IsNumericEqualSlow(Number a, Number b)
    {
        if (a is LispComplex ca && b is LispComplex cb)
            return IsNumericEqual(ca.Real, cb.Real) && IsNumericEqual(ca.Imaginary, cb.Imaginary);
        if (a is LispComplex || b is LispComplex)
        {
            var ac = AsComplex(a);
            var bc = AsComplex(b);
            return IsNumericEqual(ac.Real, bc.Real) && IsNumericEqual(ac.Imaginary, bc.Imaginary);
        }

        // CL 12.1.4.1: when comparing float with rational, convert float to rational
        bool aFloat = a is SingleFloat || a is DoubleFloat;
        bool bFloat = b is SingleFloat || b is DoubleFloat;
        if (aFloat && !bFloat) {
            // convert a (float) to rational, then compare as rationals
            var ar = FloatToRational(a);
            var (an2, ad2) = AsRational(ar);
            var (bn2, bd2) = AsRational(b);
            return an2 * bd2 == bn2 * ad2;
        }
        if (bFloat && !aFloat) {
            var br = FloatToRational(b);
            var (an2, ad2) = AsRational(a);
            var (bn2, bd2) = AsRational(br);
            return an2 * bd2 == bn2 * ad2;
        }
        if (aFloat && bFloat)
            return ToDouble(a) == ToDouble(b);

        var (an, ad) = AsRational(a);
        var (bn, bd) = AsRational(b);
        return an * bd == bn * ad;
    }

    public static int Compare(Number a, Number b)
    {
        if (a is Fixnum fa && b is Fixnum fb) return fa.Value.CompareTo(fb.Value);

        // DoubleFloat fast path
        if (a is DoubleFloat da && b is DoubleFloat db)
            return da.Value.CompareTo(db.Value);

        // CL 12.1.4.1: when comparing float with rational, convert float to rational
        bool aFloat = a is SingleFloat || a is DoubleFloat;
        bool bFloat = b is SingleFloat || b is DoubleFloat;
        if (aFloat && !bFloat) {
            var ar = FloatToRational(a);
            var (an2, ad2) = AsRational(ar);
            var (bn2, bd2) = AsRational(b);
            return (an2 * bd2).CompareTo(bn2 * ad2);
        }
        if (bFloat && !aFloat) {
            var br = FloatToRational(b);
            var (an2, ad2) = AsRational(a);
            var (bn2, bd2) = AsRational(br);
            return (an2 * bd2).CompareTo(bn2 * ad2);
        }
        if (aFloat && bFloat)
            return ToDouble(a).CompareTo(ToDouble(b));

        var (an, ad) = AsRational(a);
        var (bn, bd) = AsRational(b);
        return (an * bd).CompareTo(bn * ad);
    }

    /// <summary>Convert a float Number to a rational Number via IEEE 754 decomposition.</summary>
    private static Number FloatToRational(Number n)
    {
        double d = ToDouble(n);
        return (Number)Runtime.DoubleToRational(d);
    }

    // --- Helper: convert Number to rational BigInteger pair, handling floats ---
    private static (BigInteger num, BigInteger den) AsRationalAny(Number n)
    {
        if (n is SingleFloat || n is DoubleFloat)
            return AsRational(FloatToRational(n));
        return AsRational(n);
    }

    // --- Floor / Ceiling / Truncate / Round ---
    public static (Number quotient, Number remainder) Floor(Number a, Number b)
    {
        // Fast path: both Fixnum — avoid BigInteger conversion
        if (a is Fixnum fa && b is Fixnum fb)
        {
            long av = fa.Value, bv = fb.Value;
            long q = Math.DivRem(av, bv, out long r);
            // Floor: round toward negative infinity — adjust if remainder has different sign from divisor
            if (r != 0 && ((r ^ bv) < 0)) { q--; r += bv; }
            return (Fixnum.Make(q), Fixnum.Make(r));
        }
        var (an, ad) = AsRationalAny(a);
        var (bn, bd) = AsRationalAny(b);
        // a/b as rational = (an * bd) / (ad * bn)
        var num = an * bd;
        var den = ad * bn;
        // Ensure denominator is positive for consistent sign handling
        if (den < 0) { num = -num; den = -den; }
        var q2 = BigInteger.DivRem(num, den, out var rem);
        // Floor: if remainder < 0, subtract 1 (round toward negative infinity)
        if (rem < 0) q2 -= 1;
        var qn = (Number)Bignum.MakeInteger(q2);
        var remainder = Subtract(a, Multiply(qn, b));
        return (qn, remainder);
    }

    public static (Number quotient, Number remainder) Truncate(Number a, Number b)
    {
        // Fast path: both Fixnum
        if (a is Fixnum fa && b is Fixnum fb)
        {
            long av = fa.Value, bv = fb.Value;
            long q = Math.DivRem(av, bv, out long r);
            return (Fixnum.Make(q), Fixnum.Make(r));
        }
        var (an, ad) = AsRationalAny(a);
        var (bn, bd) = AsRationalAny(b);
        var num = an * bd;
        var den = ad * bn;
        if (den < 0) { num = -num; den = -den; }
        // Truncate: DivRem truncates toward zero by default
        var q2 = BigInteger.DivRem(num, den, out _);
        var qn = (Number)Bignum.MakeInteger(q2);
        var remainder = Subtract(a, Multiply(qn, b));
        return (qn, remainder);
    }

    public static (Number quotient, Number remainder) Ceiling(Number a, Number b)
    {
        // Fast path: both Fixnum
        if (a is Fixnum fa && b is Fixnum fb)
        {
            long av = fa.Value, bv = fb.Value;
            long q = Math.DivRem(av, bv, out long r);
            // Ceiling: round toward positive infinity — adjust if remainder has same sign as divisor
            if (r != 0 && ((r ^ bv) >= 0)) { q++; r -= bv; }
            return (Fixnum.Make(q), Fixnum.Make(r));
        }
        var (an, ad) = AsRationalAny(a);
        var (bn, bd) = AsRationalAny(b);
        var num = an * bd;
        var den = ad * bn;
        if (den < 0) { num = -num; den = -den; }
        var q2 = BigInteger.DivRem(num, den, out var rem);
        // Ceiling: if remainder > 0, add 1 (round toward positive infinity)
        if (rem > 0) q2 += 1;
        var qn = (Number)Bignum.MakeInteger(q2);
        var remainder = Subtract(a, Multiply(qn, b));
        return (qn, remainder);
    }

    public static (Number quotient, Number remainder) Round(Number a, Number b)
    {
        var (an, ad) = AsRationalAny(a);
        var (bn, bd) = AsRationalAny(b);
        var num = an * bd;
        var den = ad * bn;
        if (den < 0) { num = -num; den = -den; }
        var q = BigInteger.DivRem(num, den, out var rem);
        // Round to nearest, ties to even
        var absRem2 = BigInteger.Abs(rem) * 2;
        var absDen = BigInteger.Abs(den);
        if (absRem2 > absDen)
        {
            // Round away from zero
            q += rem < 0 ? -1 : 1;
        }
        else if (absRem2 == absDen)
        {
            // Tie: round to even
            if (!q.IsEven)
                q += rem < 0 ? -1 : 1;
        }
        var qn = (Number)Bignum.MakeInteger(q);
        var remainder = Subtract(a, Multiply(qn, b));
        return (qn, remainder);
    }

    // --- Helper: convert integer quotient to float matching argument types ---
    private static Number QuotientToFloat(Number quotient, Number a, Number b)
    {
        double qd = ToDouble(quotient);
        // If either argument is double-float, result is double-float
        if (a is DoubleFloat || b is DoubleFloat)
            return new DoubleFloat(qd);
        // If either argument is single-float, result is single-float
        if (a is SingleFloat || b is SingleFloat)
            return new SingleFloat((float)qd);
        // Both rational → single-float
        return new SingleFloat((float)qd);
    }

    // --- FFloor / FCeiling / FTruncate / FRound ---
    public static (Number quotient, Number remainder) FFloor(Number a, Number b)
    {
        var (q, r) = Floor(a, b);
        return (QuotientToFloat(q, a, b), r);
    }

    public static (Number quotient, Number remainder) FTruncate(Number a, Number b)
    {
        var (q, r) = Truncate(a, b);
        return (QuotientToFloat(q, a, b), r);
    }

    public static (Number quotient, Number remainder) FCeiling(Number a, Number b)
    {
        var (q, r) = Ceiling(a, b);
        return (QuotientToFloat(q, a, b), r);
    }

    public static (Number quotient, Number remainder) FRound(Number a, Number b)
    {
        var (q, r) = Round(a, b);
        return (QuotientToFloat(q, a, b), r);
    }

    // --- GCD / LCM ---
    public static Number Gcd(Number a, Number b)
    {
        var (an, _) = AsRational(a);
        var (bn, _) = AsRational(b);
        return (Number)Bignum.MakeInteger(BigInteger.GreatestCommonDivisor(an, bn));
    }

    public static Number Lcm(Number a, Number b)
    {
        var (an, _) = AsRational(a);
        var (bn, _) = AsRational(b);
        if (an == BigInteger.Zero || bn == BigInteger.Zero) return Fixnum.Make(0);
        var g = BigInteger.GreatestCommonDivisor(BigInteger.Abs(an), BigInteger.Abs(bn));
        return (Number)Bignum.MakeInteger(BigInteger.Abs(an / g * bn));
    }

    // --- Type conversion helpers ---
    public static float ToSingle(Number n) => n switch
    {
        SingleFloat sf => sf.Value,
        Fixnum f => (float)f.Value,
        Bignum b => (float)b.Value,
        Ratio r => (float)r.Numerator / (float)r.Denominator,
        DoubleFloat df => (float)df.Value,
        _ => throw new NotImplementedException($"ToSingle not implemented for {n.GetType().Name}")
    };

    public static double ToDouble(Number n) => n switch
    {
        Fixnum f => f.Value,
        Bignum b => BigIntToDouble(b.Value),
        Ratio r => RatioToDouble(r),
        SingleFloat sf => sf.Value,
        DoubleFloat df => df.Value,
        _ => throw new NotImplementedException($"ToDouble not implemented for {n.GetType().Name}")
    };

    private static (BigInteger num, BigInteger den) AsRational(Number n) => n switch
    {
        Fixnum f => (f.Value, BigInteger.One),
        Bignum b => (b.Value, BigInteger.One),
        Ratio r => (r.Numerator, r.Denominator),
        _ => throw new ArgumentException($"Not a rational number: {n}")
    };

    private static LispComplex AsComplex(Number n) => n switch
    {
        LispComplex c => c,
        _ => new LispComplex(n, Fixnum.Make(0))
    };

    private static Number MakeComplex(Number real, Number imag)
    {
        if (imag is Fixnum fi && fi.Value == 0) return real;
        return new LispComplex(real, imag);
    }

    public static LispObject MakeComplexPublic(Number real, Number imag)
    {
        // Per CL spec: if imagpart is 0 and realpart is rational, return rational
        if (imag is Fixnum fi && fi.Value == 0 && real is not SingleFloat && real is not DoubleFloat)
            return real;
        // Float contagion: if either part is DoubleFloat, widen the other to DoubleFloat
        if (real is DoubleFloat && imag is not DoubleFloat)
            imag = new DoubleFloat(ToDouble(imag));
        else if (imag is DoubleFloat && real is not DoubleFloat)
            real = new DoubleFloat(ToDouble(real));
        // If either part is SingleFloat, coerce the other to SingleFloat (if not already float)
        else if (real is SingleFloat && imag is not SingleFloat)
            imag = new SingleFloat((float)ToDouble(imag));
        else if (imag is SingleFloat && real is not SingleFloat)
            real = new SingleFloat((float)ToDouble(real));
        // Per CL spec: if imagpart is 0.0 for floats, still return complex (already float)
        return new LispComplex(real, imag);
    }

    /// <summary>Convert a LispComplex to System.Numerics.Complex.</summary>
    public static System.Numerics.Complex ToSystemComplex(LispComplex c)
        => new System.Numerics.Complex(ToDouble(c.Real), ToDouble(c.Imaginary));

    /// <summary>Convert a System.Numerics.Complex back to a Lisp number.
    /// If imaginary part is 0, returns just the real part as a DoubleFloat.</summary>
    public static LispObject FromSystemComplex(System.Numerics.Complex c)
    {
        if (c.Imaginary == 0.0)
            return new DoubleFloat(c.Real);
        return new LispComplex(new DoubleFloat(c.Real), new DoubleFloat(c.Imaginary));
    }

    /// <summary>Convert a System.Numerics.Complex back, preserving the float type of the original input.</summary>
    public static LispObject FromSystemComplex(System.Numerics.Complex c, Number original)
    {
        bool originalIsComplex = original is LispComplex;
        bool useSingle = original is SingleFloat ||
            (original is LispComplex lco && lco.Real is SingleFloat && lco.Imaginary is SingleFloat);
        if (useSingle)
        {
            // If original was complex, keep result as complex even with zero imaginary
            if (c.Imaginary == 0.0 && !originalIsComplex)
                return new SingleFloat((float)c.Real);
            return new LispComplex(new SingleFloat((float)c.Real), new SingleFloat((float)c.Imaginary));
        }
        // DoubleFloat path
        if (c.Imaginary == 0.0 && !originalIsComplex)
            return new DoubleFloat(c.Real);
        return new LispComplex(new DoubleFloat(c.Real), new DoubleFloat(c.Imaginary));
    }
}
