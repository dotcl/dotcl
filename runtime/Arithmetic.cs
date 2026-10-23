using System.Numerics;
using System.Runtime.CompilerServices;

namespace DotCL;

public static class Arithmetic
{
    // --- Addition ---
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Number Add(Number a, Number b)
    {
        // Fixnum fast path: avoid BigInteger when result fits in long
        if (a is Fixnum fa && b is Fixnum fb)
        {
            long av = fa.Value, bv = fb.Value, r = av + bv;
            // Overflow check: sign of result must be consistent
            if (((av ^ bv) < 0) || ((av ^ r) >= 0))
                return Fixnum.Make(r);
            return AddIntegers(a, b);
        }

        // SingleFloat fast path: common in numeric code
        if (a is SingleFloat sfa && b is SingleFloat sfb)
            return new SingleFloat(sfa.Value + sfb.Value);

        // DoubleFloat fast path
        if (a is DoubleFloat da && b is DoubleFloat db)
            return new DoubleFloat(da.Value + db.Value);

        // Complex contagion
        if (a is LispComplex || b is LispComplex)
            return AddComplex(a, b);

        // Float contagion
        if (a is DoubleFloat || b is DoubleFloat)
            return new DoubleFloat(ToDouble(a) + ToDouble(b));
        if (a is SingleFloat || b is SingleFloat)
            return new SingleFloat(ToSingle(a) + ToSingle(b));

        // Exact arithmetic
        return AddExact(a, b);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static Number AddComplex(Number a, Number b)
    {
        if (TryDoubleParts(a, out double aar, out double aai)
            && TryDoubleParts(b, out double abr, out double abi))
            return new DoubleComplex(aar + abr, aai + abi);
        var ac = AsComplex(a);
        var bc = AsComplex(b);
        return MakeComplex(Add(ac.Real, bc.Real), Add(ac.Imaginary, bc.Imaginary));
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static Number AddExact(Number a, Number b)
    {
        if (a is Bignum or Fixnum && b is Bignum or Fixnum)
            return Bignum.MakeInteger(IntValue(a) + IntValue(b));

        var (an, ad) = AsRational(a);
        var (bn, bd) = AsRational(b);
        if (BothExact(a, b)) return RationalAddSub(an, ad, bn, bd, false);
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
            return SubtractIntegers(a, b);
        }

        // SingleFloat fast path
        if (a is SingleFloat sfa && b is SingleFloat sfb)
            return new SingleFloat(sfa.Value - sfb.Value);

        // DoubleFloat fast path
        if (a is DoubleFloat da && b is DoubleFloat db)
            return new DoubleFloat(da.Value - db.Value);

        if (a is LispComplex || b is LispComplex)
            return SubtractComplex(a, b);

        if (a is DoubleFloat || b is DoubleFloat)
            return new DoubleFloat(ToDouble(a) - ToDouble(b));
        if (a is SingleFloat || b is SingleFloat)
            return new SingleFloat(ToSingle(a) - ToSingle(b));

        return SubtractExact(a, b);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static Number SubtractComplex(Number a, Number b)
    {
        if (TryDoubleParts(a, out double sar, out double sai)
            && TryDoubleParts(b, out double sbr, out double sbi))
            return new DoubleComplex(sar - sbr, sai - sbi);
        var ac = AsComplex(a);
        var bc = AsComplex(b);
        return MakeComplex(Subtract(ac.Real, bc.Real), Subtract(ac.Imaginary, bc.Imaginary));
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static Number SubtractExact(Number a, Number b)
    {
        if (a is Bignum or Fixnum && b is Bignum or Fixnum)
            return Bignum.MakeInteger(IntValue(a) - IntValue(b));

        var (an, ad) = AsRational(a);
        var (bn, bd) = AsRational(b);
        if (BothExact(a, b)) return RationalAddSub(an, ad, bn, bd, true);
        return (Number)Ratio.Make(an * bd - bn * ad, ad * bd);
    }

    // --- Multiplication ---
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Number Multiply(Number a, Number b)
    {
        if (a is Fixnum fa && b is Fixnum fb)
        {
            long hi = Compat.BigMul(fa.Value, fb.Value, out long lo);
            if (hi == (lo >> 63)) return Fixnum.Make(lo);
            return MultiplyIntegers(a, b);
        }
        if (a is Bignum or Fixnum && b is Bignum or Fixnum)
            return MultiplyIntegers(a, b);

        // SingleFloat fast path
        if (a is SingleFloat sfa && b is SingleFloat sfb)
            return new SingleFloat(sfa.Value * sfb.Value);

        // DoubleFloat fast path
        if (a is DoubleFloat da && b is DoubleFloat db)
            return new DoubleFloat(da.Value * db.Value);

        if (a is LispComplex || b is LispComplex)
            return MultiplyComplex(a, b);

        if (a is DoubleFloat || b is DoubleFloat)
            return new DoubleFloat(ToDouble(a) * ToDouble(b));
        if (a is SingleFloat || b is SingleFloat)
            return new SingleFloat(ToSingle(a) * ToSingle(b));

        return MultiplyRational(a, b);
    }

    // Out of line for the reason given at Divide.
    [MethodImpl(MethodImplOptions.NoInlining)]
    private static Number MultiplyComplex(Number a, Number b)
    {
        // A real factor scales each part. Going through the (a+bi)(c+di) form
        // with an imaginary part of zero instead multiplies that zero by the
        // other part, and 0 * infinity is NaN: (* 1 #C(inf 0.0)) came back as
        // #C(inf NaN). It also loses a signed zero, since 0*x + 0*y is +0.0
        // whatever the signs were. Scaling keeps both.
        if (a is not LispComplex && b is LispComplex bsc)
            return MakeComplex(Multiply(a, bsc.Real), Multiply(a, bsc.Imaginary));
        if (b is not LispComplex && a is LispComplex asc)
            return MakeComplex(Multiply(asc.Real, b), Multiply(asc.Imaginary, b));
        // All-double: the general form below boxes each of the four products
        // and both of the sums, seven allocations to produce one result. It
        // is the same formula over the same doubles rounded at the same
        // points, so this answers bit-identically without the intermediates.
        if (TryDoubleParts(a, out double mar, out double mai)
            && TryDoubleParts(b, out double mbr, out double mbi))
            return new DoubleComplex(mar * mbr - mai * mbi,
                                     mar * mbi + mai * mbr);
        var ac = AsComplex(a);
        var bc = AsComplex(b);
        // (a+bi)(c+di) = (ac-bd) + (ad+bc)i
        return MakeComplex(
            Subtract(Multiply(ac.Real, bc.Real), Multiply(ac.Imaginary, bc.Imaginary)),
            Add(Multiply(ac.Real, bc.Imaginary), Multiply(ac.Imaginary, bc.Real)));
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static Number MultiplyRational(Number a, Number b)
    {
        var (an, ad) = AsRational(a);
        var (bn, bd) = AsRational(b);
        if (BothExact(a, b)) return RationalMultiply(an, ad, bn, bd);
        return (Number)Ratio.Make(an * bn, ad * bd);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    internal static Number AddIntegers(LispObject a, LispObject b) =>
        Bignum.MakeInteger(IntValue(a) + IntValue(b));

    [MethodImpl(MethodImplOptions.NoInlining)]
    internal static Number SubtractIntegers(LispObject a, LispObject b) =>
        Bignum.MakeInteger(IntValue(a) - IntValue(b));

    [MethodImpl(MethodImplOptions.NoInlining)]
    internal static Number MultiplyIntegers(LispObject a, LispObject b) =>
        Bignum.MakeInteger(MultiplyBig(IntValue(a), IntValue(b)));

    internal static BigInteger MultiplyBig(BigInteger x, BigInteger y)
    {
#if !NETSTANDARD2_0
        var mx = BigInteger.Abs(x);
        var my = BigInteger.Abs(y);
        long lx = mx.GetBitLength(), ly = my.GetBitLength();
        long words = ((lx + 31) >> 5) * ((ly + 31) >> 5);
        if (lx <= MulWordsMax * 64 && ly <= MulWordsMax * 64 && words >= 9)
        {
            BigInteger r;
            if (lx <= 128 && ly <= 128) r = Multiply128(mx, my);
            else if (words >= 20 && lx > 32 && ly > 32) r = MultiplyWords(mx, my, lx, ly);
            else return x * y;
            return (x.Sign ^ y.Sign) < 0 ? -r : r;
        }
#endif
        return x * y;
    }

#if !NETSTANDARD2_0
    // BigInteger multiplies in 32-bit limbs. With 64-bit limbs a product of up
    // to 4 x 4 limbs is faster, once the 32-bit limb products number 9 or more
    // (two-limb operands) or 20 or more (wider ones).
    [SkipLocalsInit]
    private static BigInteger Multiply128(BigInteger x, BigInteger y)
    {
        UInt128 a = (UInt128)x, b = (UInt128)y;
        ulong a0 = (ulong)a, a1 = (ulong)(a >> 64), b0 = (ulong)b, b1 = (ulong)(b >> 64);
        UInt128 p00 = (UInt128)a0 * b0, p01 = (UInt128)a0 * b1, p10 = (UInt128)a1 * b0;
        UInt128 mid = (p00 >> 64) + (ulong)p01 + (ulong)p10;
        UInt128 hi = (mid >> 64) + (p01 >> 64) + (p10 >> 64) + (UInt128)a1 * b1;
        Span<ulong> r = stackalloc ulong[4];
        r[0] = (ulong)p00;
        r[1] = (ulong)mid;
        r[2] = (ulong)hi;
        r[3] = (ulong)(hi >> 64);
        return new BigInteger(System.Runtime.InteropServices.MemoryMarshal.AsBytes(r), isUnsigned: true);
    }

    private const int MulWordsMax = 32;

    [SkipLocalsInit]
    private static BigInteger MultiplyWords(BigInteger x, BigInteger y, long lx, long ly)
    {
        Span<ulong> a = stackalloc ulong[MulWordsMax];
        Span<ulong> b = stackalloc ulong[MulWordsMax];
        Span<ulong> r = stackalloc ulong[2 * MulWordsMax];
        int na = (int)((lx + 63) >> 6), nb = (int)((ly + 63) >> 6);
        a.Clear(); b.Clear(); r.Slice(0, na + nb).Clear();
        x.TryWriteBytes(System.Runtime.InteropServices.MemoryMarshal.AsBytes(a), out _, isUnsigned: true);
        y.TryWriteBytes(System.Runtime.InteropServices.MemoryMarshal.AsBytes(b), out _, isUnsigned: true);
        for (int i = 0; i < na; i++)
        {
            ulong carry = 0, ai = a[i];
            for (int j = 0; j < nb; j++)
            {
                ulong hi = Math.BigMul(ai, b[j], out ulong lo);
                lo += carry;
                hi += lo < carry ? 1UL : 0UL;
                ulong t = r[i + j] + lo;
                hi += t < lo ? 1UL : 0UL;
                r[i + j] = t;
                carry = hi;
            }
            r[i + nb] = carry;
        }
        return new BigInteger(System.Runtime.InteropServices.MemoryMarshal.AsBytes(r.Slice(0, na + nb)), isUnsigned: true);
    }
#endif

#if !NETSTANDARD2_0
    private const int RemWordsMax = 32;

    /// <summary>x % y (the sign of x), with 64-bit limbs when |y| has 2 to 32
    /// limbs and |x| at least 2 more and at most 64: Knuth's algorithm D, each
    /// quotient limb estimated with a precomputed reciprocal of the divisor's top
    /// limb. A quotient of one or two limbs is left to BigInteger, which needs no
    /// reciprocal.</summary>
    internal static BigInteger RemainderBig(BigInteger x, BigInteger y)
    {
        if (y.IsZero) return x % y;
        var mx = BigInteger.Abs(x);
        var my = BigInteger.Abs(y);
        long lx = mx.GetBitLength(), ly = my.GetBitLength();
        int m = (int)((lx + 63) >> 6), n = (int)((ly + 63) >> 6);
        if (n < 2 || n > RemWordsMax || m > 2 * RemWordsMax || m - n < 2)
            return x % y;
        var r = RemainderWords(mx, my, m, n);
        return x.Sign < 0 ? -r : r;
    }

    [SkipLocalsInit]
    private static BigInteger RemainderWords(BigInteger x, BigInteger y, int m, int n)
    {
        Span<ulong> u = stackalloc ulong[2 * RemWordsMax + 1];
        Span<ulong> v = stackalloc ulong[RemWordsMax];
        u.Slice(0, m + 1).Clear(); v.Slice(0, n).Clear();
        x.TryWriteBytes(System.Runtime.InteropServices.MemoryMarshal.AsBytes(u.Slice(0, m)), out _, isUnsigned: true);
        y.TryWriteBytes(System.Runtime.InteropServices.MemoryMarshal.AsBytes(v.Slice(0, n)), out _, isUnsigned: true);
        int s = System.Numerics.BitOperations.LeadingZeroCount(v[n - 1]);
        if (s != 0)
        {
            for (int i = n - 1; i > 0; i--) v[i] = (v[i] << s) | (v[i - 1] >> (64 - s));
            v[0] <<= s;
            u[m] = u[m - 1] >> (64 - s);
            for (int i = m - 1; i > 0; i--) u[i] = (u[i] << s) | (u[i - 1] >> (64 - s));
            u[0] <<= s;
        }
        ulong d = v[n - 1], d2 = v[n - 2];
        ulong inv = Reciprocal(d);
        for (int j = m - n; j >= 0; j--)
        {
            ulong uh = u[j + n], ul = u[j + n - 1];
            ulong qhat, rhat;
            bool rbig = false;
            if (uh >= d)
            {
                qhat = ulong.MaxValue;
                rhat = ul + d;
                rbig = rhat < ul;
            }
            else
            {
                qhat = Div2By1(uh, ul, d, inv, out rhat);
            }
            while (!rbig)
            {
                UInt128 lhs = (UInt128)qhat * d2;
                UInt128 rhs = ((UInt128)rhat << 64) | u[j + n - 2];
                if (lhs <= rhs) break;
                qhat--;
                ulong r2 = rhat + d;
                rbig = r2 < rhat;
                rhat = r2;
            }
            ulong carry = 0;
            for (int i = 0; i < n; i++)
            {
                UInt128 p = (UInt128)qhat * v[i] + carry;
                ulong plo = (ulong)p;
                carry = (ulong)(p >> 64);
                ulong t = u[i + j] - plo;
                carry += t > u[i + j] ? 1UL : 0UL;
                u[i + j] = t;
            }
            ulong top = u[j + n];
            u[j + n] = top - carry;
            if (carry > top)
            {
                ulong c = 0;
                for (int i = 0; i < n; i++)
                {
                    ulong a = u[i + j], b = v[i];
                    ulong t = a + b;
                    ulong c1 = t < a ? 1UL : 0UL;
                    ulong t2 = t + c;
                    ulong c2 = t2 < t ? 1UL : 0UL;
                    u[i + j] = t2;
                    c = c1 + c2;
                }
                u[j + n] += c;
            }
        }
        if (s != 0)
        {
            for (int i = 0; i < n - 1; i++) u[i] = (u[i] >> s) | (u[i + 1] << (64 - s));
            u[n - 1] >>= s;
        }
        return new BigInteger(System.Runtime.InteropServices.MemoryMarshal.AsBytes(u.Slice(0, n)), isUnsigned: true);
    }

    /// <summary>floor((2^128 - 1) / d) mod 2^64 for a normalized d, as
    /// (~d : 2^64 - 1) / d in two 64 by 32 bit steps (Hacker's Delight, divlu).</summary>
    private static ulong Reciprocal(ulong d)
    {
        const ulong b = 1UL << 32;
        ulong u1 = ~d;
        ulong vn1 = d >> 32, vn0 = d & 0xFFFFFFFF;
        ulong q1 = u1 / vn1, rhat = u1 - q1 * vn1;
        while (q1 >= b || q1 * vn0 > ((rhat << 32) | 0xFFFFFFFF))
        {
            q1--; rhat += vn1;
            if (rhat >= b) break;
        }
        ulong un21 = (u1 << 32) + 0xFFFFFFFF - q1 * d;
        ulong q0 = un21 / vn1;
        rhat = un21 - q0 * vn1;
        while (q0 >= b || q0 * vn0 > ((rhat << 32) | 0xFFFFFFFF))
        {
            q0--; rhat += vn1;
            if (rhat >= b) break;
        }
        return (q1 << 32) + q0;
    }

    /// <summary>(uh:ul) / d for a normalized d and uh &lt; d, with inv = floor((2^128 - 1) / d) mod 2^64
    /// (Moller and Granlund, "Improved division by invariant integers", algorithm 4).</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static ulong Div2By1(ulong uh, ulong ul, ulong d, ulong inv, out ulong rem)
    {
        UInt128 q = (UInt128)inv * uh + (((UInt128)uh << 64) | ul);
        ulong q1 = (ulong)(q >> 64) + 1, q0 = (ulong)q;
        ulong r = ul - q1 * d;
        if (r > q0) { q1--; r += d; }
        if (r >= d) { q1++; r -= d; }
        rem = r;
        return q1;
    }
#else
    internal static BigInteger RemainderBig(BigInteger x, BigInteger y) => x % y;
#endif

    // --- Division ---
    // The complex and rational cases are kept out of line: inlined, their
    // BigInteger temporaries gave every caller a frame of several KB that the
    // prolog zeroes on each call, float division included.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Number Divide(Number a, Number b)
    {
        if (a is LispComplex || b is LispComplex)
            return DivideComplex(a, b);
        // Mixed float/rational division: the result type is a float, so IEEE
        // applies here as it does in Runtime.Divide -- (/ 1 0.0d0) must not
        // answer differently from (/ 1.0d0 0.0d0).
        if (a is DoubleFloat || b is DoubleFloat)
            return new DoubleFloat(ToDouble(a) / ToDouble(b));
        if (a is SingleFloat || b is SingleFloat)
            return new SingleFloat(ToSingle(a) / ToSingle(b));
        return DivideRational(a, b);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static Number DivideComplex(Number a, Number b)
    {
        var ac = AsComplex(a);
        var bc = AsComplex(b);
        static bool IsFlt(Number n) => n is SingleFloat || n is DoubleFloat;
        bool anyFloat = IsFlt(ac.Real) || IsFlt(ac.Imaginary) || IsFlt(bc.Real) || IsFlt(bc.Imaginary);
        if (anyFloat)
        {
            // Float complex division via System.Numerics (Smith's algorithm): the
            // denominator is scaled, so c^2+d^2 no longer over/underflows for extreme
            // magnitudes. The naive ((c^2+d^2)) form made (/ z (abs z)) for |z|~1e+/-170
            // hit divide-by-zero (denom->0) or NaN (denom->inf). Maxima signum(complex)
            // depends on this.
            var bcx = ToSystemComplex(bc);
            if (bcx.Real == 0.0 && bcx.Imaginary == 0.0)
                throw new DivideByZeroException("Division by zero");
            var q = ToSystemComplex(ac) / bcx;
            bool allSingle = !(ac.Real is DoubleFloat) && !(ac.Imaginary is DoubleFloat)
                          && !(bc.Real is DoubleFloat) && !(bc.Imaginary is DoubleFloat)
                          && (ac.Real is SingleFloat || ac.Imaginary is SingleFloat
                              || bc.Real is SingleFloat || bc.Imaginary is SingleFloat);
            return allSingle
                ? MakeComplex(new SingleFloat((float)q.Real), new SingleFloat((float)q.Imaginary))
                : MakeComplex(new DoubleFloat(q.Real), new DoubleFloat(q.Imaginary));
        }
        // Exact (integer/rational) complex division: (a+bi)/(c+di) =
        // ((ac+bd) + (bc-ad)i) / (c^2+d^2). BigInteger arithmetic: no overflow.
        var denom = Add(Multiply(bc.Real, bc.Real), Multiply(bc.Imaginary, bc.Imaginary));
        return MakeComplex(
            Divide(Add(Multiply(ac.Real, bc.Real), Multiply(ac.Imaginary, bc.Imaginary)), denom),
            Divide(Subtract(Multiply(ac.Imaginary, bc.Real), Multiply(ac.Real, bc.Imaginary)), denom));
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static Number DivideRational(Number a, Number b)
    {
        var (an, ad) = AsRational(a);
        var (bn, bd) = AsRational(b);
        // Division is multiplication by the reciprocal, which is the divisor
        // turned upside down -- still a reduced fraction, once the sign is moved
        // to the numerator.
        if (BothExact(a, b))
        {
            if (bn.IsZero) throw new DivideByZeroException("Division by zero");
            var rn = bd;
            var rd = bn;
            if (rd.Sign < 0) { rn = -rn; rd = -rd; }
            return RationalMultiply(an, ad, rn, rd);
        }
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
            LispDecimal d => Negate(d.ToRational()),   // standard op -> standard (rational) result
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
            LispDecimal d => Abs(d.ToRational()),   // standard op -> standard (rational) result
            // Magnitude via System.Numerics.Complex.Abs, which uses a scaled hypot
            // (max*sqrt(1+(min/max)^2)) so it doesn't over/underflow when re^2/im^2
            // would: e.g. (abs #C(1d170 1d170)) and (abs #C(1d-170 1d-170)) are exact
            // rather than +inf / 0. The naive sqrt(re^2+im^2) squared the magnitude.
            // Only a double part makes the magnitude a double. Rational parts give
            // a single float, because the magnitude is (sqrt (+ (* r r) (* i i)))
            // and SQRT of a rational answers in the default float format -- ABS of
            // a complex rational used to be the one place that said double while
            // SQRT of the same number said single.
            DoubleComplex dc => new DoubleFloat(
                System.Numerics.Complex.Abs(new System.Numerics.Complex(dc.RealValue, dc.ImagValue))),
            LispComplex c => (c.Real is DoubleFloat || c.Imaginary is DoubleFloat)
                ? (Number)new DoubleFloat(System.Numerics.Complex.Abs(ToSystemComplex(c)))
                : new SingleFloat((float)System.Numerics.Complex.Abs(ToSystemComplex(c))),
            _ => throw new NotImplementedException()
        };
    }

    // --- Modular arithmetic ---
    // |v| > 2^63, so |v| > |a| for every fixnum a (most-negative-fixnum is -2^63).
    private static readonly System.Numerics.BigInteger TwoTo63 = System.Numerics.BigInteger.One << 63;
    private static bool OutsideLong(System.Numerics.BigInteger v)
        => v > TwoTo63 || v < -TwoTo63;

    public static Number Mod(Number a, Number b)
    {
        // Fast path: both Fixnum
        if (a is Fixnum fa && b is Fixnum fb)
        {
            long av = fa.Value, bv = fb.Value;
            if (bv == 0) throw Runtime.QuotientByZero(av);
            if (bv == -1) return Fixnum.Make(0);
            long r = av % bv;
            // CL mod: result has same sign as divisor
            if (r != 0 && ((r ^ bv) < 0)) r += bv;
            return Fixnum.Make(r);
        }
        // A fixnum by a bignum: the bignum's magnitude is larger than any fixnum's,
        // so the quotient's floor is 0 (same signs, or a = 0) or -1. No BigInteger
        // division: (mod x (expt 2 64)) is how code keeps a value in 64 bits.
        if (b is Fixnum { Value: 0 }) throw Runtime.QuotientByZero(a, b);
        if (a is Fixnum sa && b is Bignum sb && OutsideLong(sb.Value))
        {
            long av = sa.Value;
            if (av == 0 || (av > 0) == (sb.Value.Sign > 0)) return sa;
            return (Number)Bignum.MakeInteger(sb.Value + av);
        }
        // A float operand: MOD is the second value of FLOOR, computed the way
        // FLOOR computes it (see FloatRound).
        if (IsFloatNum(a) || IsFloatNum(b))
            return Floor(a, b).remainder;
        // A ratio operand: the same, exactly. The integer path below reads the
        // numerators only, so (mod 7/2 1) came back as 0.
        if (a is Ratio || b is Ratio)
            return Floor(a, b).remainder;
        var (an, _) = AsRational(a);
        var (bn, _) = AsRational(b);
        var iresult = RemainderBig(an, bn);
        // CL mod: result has same sign as divisor
        if (iresult != 0 && (iresult < 0) != (bn < 0))
            iresult += bn;
        return (Number)Bignum.MakeInteger(iresult);
    }

    public static Number Rem(Number a, Number b)
    {
        // A float operand: REM is the second value of TRUNCATE.
        // A fixnum by a bignum: |a| < |b|, so the truncated quotient is 0.
        if (b is Fixnum { Value: 0 }) throw Runtime.QuotientByZero(a, b);
        if (a is Fixnum && b is Bignum rb && OutsideLong(rb.Value)) return a;
        if (IsFloatNum(a) || IsFloatNum(b) || a is Ratio || b is Ratio)
            return Truncate(a, b).remainder;
        var (an, _) = AsRational(a);
        var (bn, _) = AsRational(b);
        return (Number)Bignum.MakeInteger(RemainderBig(an, bn));
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
            result = Compat.ScaleB((double)(ulong)mantissa, shift);
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
        // Both exact as doubles: one IEEE division rounds the quotient correctly.
        const long Exact = 1L << 53;
        if (num >= -Exact && num <= Exact && den >= -Exact && den <= Exact)
            return (double)(long)num / (double)(long)den;
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

        // Normalize so q has exactly 54 bits (handles +/-1 bit from estimate)
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
                // Rounding carried into a 54th bit. Halving q (q >>= 1) must be
                // compensated by shift-- (the result is q * 2^-shift, so a smaller q
                // needs a smaller shift to hold the value): same convention as the
                // guard-bit extraction above. shift++ here turned (2^N-1)/2^N into 0.25.
                q >>= 1;
                shift--;
            }
        }

        if (q.IsZero) return negative ? -0.0 : 0.0;

        // Value is q * 2^-shift. Math.ScaleB handles subnormal range.
        double result = Compat.ScaleB((double)(ulong)q, -shift);
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
        if (b is Fixnum bf)
        {
            if (a is SingleFloat asf) return FloatEqualsLong(asf.Value, bf.Value);
            if (a is DoubleFloat adf) return FloatEqualsLong(adf.Value, bf.Value);
        }
        if (a is SingleFloat sa && b is SingleFloat sb) return sa.Value == sb.Value;
        return IsNumericEqualSlow(a, b);
    }

    private static bool IsNumericEqualSlow(Number a, Number b)
    {
        if ((a is Bignum && b is Fixnum) || (a is Fixnum && b is Bignum)) return false;
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
        // A non-finite float (inf/nan) has no rational value; compare as doubles
        // instead of rationalizing (which would throw "cannot be converted").
        if (IsNonFiniteFloat(a, aFloat) || IsNonFiniteFloat(b, bFloat))
            return ToDouble(a) == ToDouble(b);
        // A float against a fixnum: the float equals it only when it is a whole
        // number, and a whole double inside the long range converts exactly, so
        // the comparison needs no rational at all. ZEROP on a float coefficient
        // used to build one (and box it) on every call.
        if (aFloat && b is Fixnum bfx) return FloatEqualsLong(ToDouble(a), bfx.Value);
        if (bFloat && a is Fixnum afx) return FloatEqualsLong(ToDouble(b), afx.Value);
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

        // Two integers compare as themselves. The rational form below multiplies
        // each by the other.s denominator, and an integer.s denominator is 1: a
        // full BigInteger multiply that copies the whole magnitude to produce the
        // number it started with. ZEROP on a bignum coefficient does this.
        if (a is Bignum or Fixnum && b is Bignum or Fixnum)
            return IntValue(a) == IntValue(b);
        var (an, ad) = AsRational(a);
        var (bn, bd) = AsRational(b);
        return an * bd == bn * ad;
    }

    /// <summary>Exact = between a finite double and a long. -2^63 &lt;= d &lt; 2^63 is
    /// the range in which (long)d is exact for a whole d.</summary>
    private static bool FloatEqualsLong(double d, long v)
    {
        if (System.Math.Floor(d) != d) return false;
        if (d < -9.2233720368547758E18 || d >= 9.2233720368547758E18) return false;
        return (long)d == v;
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
        // A non-finite float (inf/nan) has no rational value; compare as doubles
        // (matching the float/float path) instead of rationalizing (which throws).
        // Two floats compare as doubles whether or not either is finite: the
        // non-finite branch answers with this same expression. Settle it here
        // rather than probing both operands for finiteness first.
        if (aFloat && bFloat)
            return ToDouble(a).CompareTo(ToDouble(b));
        if (IsNonFiniteFloat(a, aFloat) || IsNonFiniteFloat(b, bFloat))
            return ToDouble(a).CompareTo(ToDouble(b));
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

        // See IsNumericEqualSlow: two integers need no cross-multiplication.
        if (a is Bignum or Fixnum && b is Bignum or Fixnum)
            return IntValue(a).CompareTo(IntValue(b));
        var (an, ad) = AsRational(a);
        var (bn, bd) = AsRational(b);
        return (an * bd).CompareTo(bn * ad);
    }

    /// <summary>True if either operand is a NaN float. The ordering predicates
    /// (&lt; &lt;= &gt; &gt;=) must return false when an operand is NaN; IEEE comparisons
    /// with NaN are "unordered", but Compare() returns a total order (CompareTo
    /// ranks NaN below everything), so callers must check this first.</summary>
    public static bool EitherNaN(Number a, Number b)
        => (a is DoubleFloat da && double.IsNaN(da.Value))
        || (a is SingleFloat sa && float.IsNaN(sa.Value))
        || (b is DoubleFloat db && double.IsNaN(db.Value))
        || (b is SingleFloat sb && float.IsNaN(sb.Value));

    /// <summary>True when N is a float operand whose value is infinite or NaN
    /// (so it has no rational representation). ns2.0-safe (no double.IsFinite).</summary>
    private static bool IsNonFiniteFloat(Number n, bool isFloat)
    {
        if (!isFloat) return false;
        double d = ToDouble(n);
        return double.IsInfinity(d) || double.IsNaN(d);
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

    /// <summary>
    /// The remainder belonging to a quotient, built from the remainder the
    /// division already produced.
    ///
    /// The four operations below all computed it as a - q*b instead, which is a
    /// multiplication by a quotient that can be thousands of digits wide -- more
    /// work than the division that preceded it. DivRem hands back the remainder
    /// for free, and a rounding adjustment of one moves it by exactly one
    /// divisor, so an addition finishes the job.
    ///
    /// REM and DEN are in the frame the caller normalised to (DEN made positive,
    /// FLIPPED recording whether both were negated). ADJUST is how the caller
    /// moved the truncated quotient: -1, 0 or +1. AD and BD are the denominators
    /// of the two operands, so the exact remainder is the integer one scaled by
    /// 1/(AD*BD).
    ///
    /// Exact operands only. A float operand has to give a float remainder, and
    /// this answers in the rationals.
    /// </summary>
    private static Number ExactRemainder(BigInteger rem, BigInteger den, int adjust,
                                         bool flipped, BigInteger ad, BigInteger bd)
    {
        var r = adjust == 0 ? rem : adjust > 0 ? rem - den : rem + den;
        if (flipped) r = -r;
        if (bd.IsOne)
        {
            // r = an - q*ad*bn (+- ad*bn), so gcd(r, ad) = gcd(an, ad) = 1.
            return ad.IsOne ? (Number)Bignum.MakeInteger(r) : (Number)Ratio.MakeReduced(r, ad);
        }
        var scale = ad * bd;
        return scale.IsOne ? (Number)Bignum.MakeInteger(r) : (Number)Ratio.Make(r, scale);
    }

    /// <summary>True when both operands are exact, so ExactRemainder applies.</summary>
    private static bool BothExact(Number a, Number b) =>
        a is Fixnum or Bignum or Ratio && b is Fixnum or Bignum or Ratio;

    private static bool IsFloatNum(Number n) => n is SingleFloat || n is DoubleFloat;

    private enum RoundMode { Floor, Ceiling, Truncate, Round }

    /// <summary>
    /// FLOOR / CEILING / TRUNCATE / ROUND and their F variants when an operand
    /// is a float: divide in floating point, then round the float quotient.
    ///
    /// Both operands are first taken to the result format (double if either is
    /// a double, else single). The quotient is x / f rounded in that format, and
    /// the remainder is x - f * q, also in that format. So (floor 1d0 0.1d0) is
    /// 10 and 0d0: 1d0 / 0.1d0 rounds to exactly 10d0 even though the true ratio
    /// of the two binary values is a hair under 10. The alternative, rounding the
    /// exact rational ratio, gives 9 and 0.09999999999999998d0, and its remainder
    /// then disagrees with what MOD and REM must return (CLHS: MOD is the second
    /// value of FLOOR, REM of TRUNCATE). This is also what SBCL computes.
    ///
    /// FLOATQUOTIENT selects the F variants: the quotient stays a float (so a
    /// negative quotient that rounds to zero is -0.0). Otherwise the quotient is
    /// the exact integer of the rounded float, and -0.0 does not leak into the
    /// remainder. Returns false when the quotient is not finite (a zero divisor,
    /// or overflow): the caller keeps its exact path for those, which reports a
    /// zero divisor or a non-finite operand as it always has.
    /// </summary>
    private static bool FloatRound(Number a, Number b, RoundMode mode, bool floatQuotient,
                                   out (Number quotient, Number remainder) result)
    {
        result = default;
        if (a is DoubleFloat || b is DoubleFloat)
        {
            double x = ToDouble(a), f = ToDouble(b);
            double div = x / f;
            if (double.IsInfinity(div) || double.IsNaN(div)) return false;
            double q = RoundDouble(div, mode);
            if (floatQuotient)
            {
                result = (new DoubleFloat(q), new DoubleFloat(x - q * f));
                return true;
            }
            result = (FloatToInteger(q), new DoubleFloat(x - f * (q + 0.0)));
            return true;
        }
        else
        {
            float x = ToSingleRounded(a), f = ToSingleRounded(b);
            float div = x / f;
            if (float.IsInfinity(div) || float.IsNaN(div)) return false;
            float q = (float)RoundDouble(div, mode);
            if (floatQuotient)
            {
                float prod = q * f;
                result = (new SingleFloat(q), new SingleFloat(x - prod));
                return true;
            }
            float qz = q + 0.0f;
            float p = f * qz;
            result = (FloatToInteger(q), new SingleFloat(x - p));
            return true;
        }
    }

    // The value is a float already, so rounding it in double is exact, and a
    // single-precision value comes back unchanged when narrowed again.
    private static double RoundDouble(double d, RoundMode mode) => mode switch
    {
        RoundMode.Floor => Math.Floor(d),
        RoundMode.Ceiling => Math.Ceiling(d),
        RoundMode.Truncate => Math.Truncate(d),
        _ => Math.Round(d, MidpointRounding.ToEven),
    };

    // A rational taken to single-float the way FLOAT takes it.
    private static float ToSingleRounded(Number n) => n switch
    {
        SingleFloat sf => sf.Value,
        Fixnum fx => (float)fx.Value,
        _ => (float)ToDouble(n),
    };

    // The exact integer an integral float stands for.
    internal static Number FloatToInteger(double q)
    {
        if (q > -9.2e18 && q < 9.2e18) return Fixnum.Make((long)q);
        return (Number)Bignum.MakeInteger(new BigInteger(q));
    }

    public static (Number quotient, Number remainder) Floor(Number a, Number b)
    {
        // Fast path: both Fixnum: avoid BigInteger conversion
        if (a is Fixnum fa && b is Fixnum fb)
        {
            long av = fa.Value, bv = fb.Value;
            if (bv == 0) throw Runtime.QuotientByZero(av);
            // A divisor of -1 divides exactly; negate instead, since
            // most-negative-fixnum / -1 overflows the int64 division.
            if (bv == -1) return (Negate(fa), Fixnum.Make(0));
            long q = Math.DivRem(av, bv, out long r);
            // Floor: round toward negative infinity: adjust if remainder has different sign from divisor
            if (r != 0 && ((r ^ bv) < 0)) { q--; r += bv; }
            return (Fixnum.Make(q), Fixnum.Make(r));
        }
        if (b is Fixnum { Value: 0 }) throw Runtime.QuotientByZero(a, b);
        if ((IsFloatNum(a) || IsFloatNum(b)) && FloatRound(a, b, RoundMode.Floor, false, out var fr))
            return fr;
        var (an, ad) = AsRationalAny(a);
        var (bn, bd) = AsRationalAny(b);
        // a/b as rational = (an * bd) / (ad * bn)
        var num = an * bd;
        var den = ad * bn;
        // Ensure denominator is positive for consistent sign handling
        bool flipped = den < 0;
        if (flipped) { num = -num; den = -den; }
        var q2 = BigInteger.DivRem(num, den, out var rem);
        // Floor: if remainder < 0, subtract 1 (round toward negative infinity)
        int adjust = rem < 0 ? -1 : 0;
        q2 += adjust;
        var qn = (Number)Bignum.MakeInteger(q2);
        if (BothExact(a, b)) return (qn, ExactRemainder(rem, den, adjust, flipped, ad, bd));
        var remainder = Subtract(a, Multiply(qn, b));
        return (qn, remainder);
    }

    public static (Number quotient, Number remainder) Truncate(Number a, Number b)
    {
        // Fast path: both Fixnum
        if (a is Fixnum fa && b is Fixnum fb)
        {
            long av = fa.Value, bv = fb.Value;
            if (bv == 0) throw Runtime.QuotientByZero(av);
            // A divisor of -1 divides exactly; negate instead, since
            // most-negative-fixnum / -1 overflows the int64 division.
            if (bv == -1) return (Negate(fa), Fixnum.Make(0));
            long q = Math.DivRem(av, bv, out long r);
            return (Fixnum.Make(q), Fixnum.Make(r));
        }
        if (b is Fixnum { Value: 0 }) throw Runtime.QuotientByZero(a, b);
        if ((IsFloatNum(a) || IsFloatNum(b)) && FloatRound(a, b, RoundMode.Truncate, false, out var fr))
            return fr;
        var (an, ad) = AsRationalAny(a);
        var (bn, bd) = AsRationalAny(b);
        var num = an * bd;
        var den = ad * bn;
        bool flipped = den < 0;
        if (flipped) { num = -num; den = -den; }
        // Truncate: DivRem truncates toward zero by default, so its remainder is
        // already the answer -- no adjustment.
        var q2 = BigInteger.DivRem(num, den, out var rem);
        var qn = (Number)Bignum.MakeInteger(q2);
        if (BothExact(a, b)) return (qn, ExactRemainder(rem, den, 0, flipped, ad, bd));
        var remainder = Subtract(a, Multiply(qn, b));
        return (qn, remainder);
    }

    public static (Number quotient, Number remainder) Ceiling(Number a, Number b)
    {
        // Fast path: both Fixnum
        if (a is Fixnum fa && b is Fixnum fb)
        {
            long av = fa.Value, bv = fb.Value;
            if (bv == 0) throw Runtime.QuotientByZero(av);
            // A divisor of -1 divides exactly; negate instead, since
            // most-negative-fixnum / -1 overflows the int64 division.
            if (bv == -1) return (Negate(fa), Fixnum.Make(0));
            long q = Math.DivRem(av, bv, out long r);
            // Ceiling: round toward positive infinity: adjust if remainder has same sign as divisor
            if (r != 0 && ((r ^ bv) >= 0)) { q++; r -= bv; }
            return (Fixnum.Make(q), Fixnum.Make(r));
        }
        if (b is Fixnum { Value: 0 }) throw Runtime.QuotientByZero(a, b);
        if ((IsFloatNum(a) || IsFloatNum(b)) && FloatRound(a, b, RoundMode.Ceiling, false, out var fr))
            return fr;
        var (an, ad) = AsRationalAny(a);
        var (bn, bd) = AsRationalAny(b);
        var num = an * bd;
        var den = ad * bn;
        bool flipped = den < 0;
        if (flipped) { num = -num; den = -den; }
        var q2 = BigInteger.DivRem(num, den, out var rem);
        // Ceiling: if remainder > 0, add 1 (round toward positive infinity)
        int adjust = rem > 0 ? 1 : 0;
        q2 += adjust;
        var qn = (Number)Bignum.MakeInteger(q2);
        if (BothExact(a, b)) return (qn, ExactRemainder(rem, den, adjust, flipped, ad, bd));
        var remainder = Subtract(a, Multiply(qn, b));
        return (qn, remainder);
    }

    public static (Number quotient, Number remainder) Round(Number a, Number b)
    {
        if (b is Fixnum { Value: 0 }) throw Runtime.QuotientByZero(a, b);
        if ((IsFloatNum(a) || IsFloatNum(b)) && FloatRound(a, b, RoundMode.Round, false, out var fr))
            return fr;
        var (an, ad) = AsRationalAny(a);
        var (bn, bd) = AsRationalAny(b);
        var num = an * bd;
        var den = ad * bn;
        bool flipped = den < 0;
        if (flipped) { num = -num; den = -den; }
        var q = BigInteger.DivRem(num, den, out var rem);
        // Round to nearest, ties to even
        var absRem2 = BigInteger.Abs(rem) * 2;
        var absDen = BigInteger.Abs(den);
        int adjust = 0;
        if (absRem2 > absDen)
        {
            // Round away from zero
            adjust = rem < 0 ? -1 : 1;
        }
        else if (absRem2 == absDen)
        {
            // Tie: round to even
            if (!q.IsEven)
                adjust = rem < 0 ? -1 : 1;
        }
        q += adjust;
        var qn = (Number)Bignum.MakeInteger(q);
        if (BothExact(a, b)) return (qn, ExactRemainder(rem, den, adjust, flipped, ad, bd));
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
        // Both rational -> single-float
        return new SingleFloat((float)qd);
    }

    // --- FFloor / FCeiling / FTruncate / FRound ---
    // A non-finite float (inf/nan) can't be rounded to a rational. All four
    // float-result roundings just return the (float) value itself, SBCL returns
    // the infinity/NaN as the quotient, instead of going through Floor/Truncate
    // which would rationalize and throw. (Integer-result floor/truncate still
    // error on infinity, matching SBCL; only the f* float-result forms differ.)
    private static bool TryFNonFinite(Number a, Number b, out (Number quotient, Number remainder) result)
    {
        if (IsNonFiniteFloat(a, a is SingleFloat || a is DoubleFloat) ||
            IsNonFiniteFloat(b, b is SingleFloat || b is DoubleFloat))
        {
            double q = ToDouble(a) / ToDouble(b);                 // inf/1 = inf, inf/inf = NaN
            double rem = ToDouble(a) - q * ToDouble(b);           // inf - inf = NaN
            bool dbl = a is DoubleFloat || b is DoubleFloat;
            Number quot = dbl ? new DoubleFloat(q) : (Number)new SingleFloat((float)q);
            Number remN = dbl ? new DoubleFloat(rem) : (Number)new SingleFloat((float)rem);
            result = (quot, remN);
            return true;
        }
        result = default;
        return false;
    }

    public static (Number quotient, Number remainder) FFloor(Number a, Number b)
    {
        if (TryFNonFinite(a, b, out var nf)) return nf;
        if ((IsFloatNum(a) || IsFloatNum(b)) && FloatRound(a, b, RoundMode.Floor, true, out var fr))
            return fr;
        var (q, r) = Floor(a, b);
        return (QuotientToFloat(q, a, b), r);
    }

    public static (Number quotient, Number remainder) FTruncate(Number a, Number b)
    {
        if (TryFNonFinite(a, b, out var nf)) return nf;
        if ((IsFloatNum(a) || IsFloatNum(b)) && FloatRound(a, b, RoundMode.Truncate, true, out var fr))
            return fr;
        var (q, r) = Truncate(a, b);
        return (QuotientToFloat(q, a, b), r);
    }

    public static (Number quotient, Number remainder) FCeiling(Number a, Number b)
    {
        if (TryFNonFinite(a, b, out var nf)) return nf;
        if ((IsFloatNum(a) || IsFloatNum(b)) && FloatRound(a, b, RoundMode.Ceiling, true, out var fr))
            return fr;
        var (q, r) = Ceiling(a, b);
        return (QuotientToFloat(q, a, b), r);
    }

    public static (Number quotient, Number remainder) FRound(Number a, Number b)
    {
        if (TryFNonFinite(a, b, out var nf)) return nf;
        if ((IsFloatNum(a) || IsFloatNum(b)) && FloatRound(a, b, RoundMode.Round, true, out var fr))
            return fr;
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
        // Through the double quotient, as FLOAT does (ToSingleRounded): taking
        // numerator and denominator to single-float separately overflows both to
        // infinity once they pass 3.4e38, and infinity / infinity is NaN.
        Ratio r => (float)RatioToDouble(r),
        DoubleFloat df => (float)df.Value,
        LispDecimal d => (float)d.Value,
        _ => throw new NotImplementedException($"ToSingle not implemented for {n.GetType().Name}")
    };

    public static double ToDouble(Number n) => n switch
    {
        Fixnum f => f.Value,
        Bignum b => BigIntToDouble(b.Value),
        Ratio r => RatioToDouble(r),
        SingleFloat sf => sf.Value,
        DoubleFloat df => df.Value,
        LispDecimal d => (double)d.Value,
        _ => throw new NotImplementedException($"ToDouble not implemented for {n.GetType().Name}")
    };

    /// <summary>The exact integer value of a FIXNUM or BIGNUM. Used by the
    /// integer fast paths in Add/Subtract/Multiply, which exist so an operation on
    /// integers does not build a rational and reduce it.</summary>
    private static BigInteger IntValue(LispObject n) =>
        n is Fixnum f ? f.Value : ((Bignum)n).Value;

    // --- Exact rational arithmetic (Knuth 4.5.1) ---
    //
    // The naive way to add or multiply two fractions is to combine them and then
    // reduce the result by a GCD. That GCD runs on the products, which are about
    // twice as long as the operands, and it is the dominant cost. Taking the
    // common factors out FIRST -- from the denominators for a sum, crosswise for
    // a product -- means every GCD runs on operand-sized numbers, and in the
    // common case where they share no factor it means no reduction at all.
    //
    // Both operands must be reduced fractions with positive denominators, which
    // is what AsRational gives for a Fixnum, a Bignum or a Ratio (a Ratio is
    // reduced by construction). A LispDecimal is not covered.

    /// <summary>GCD with the cases that need no division taken first. GCD of a
    /// thousand-digit number with 1 still divides it, for an answer that was
    /// known by inspection.</summary>
    private static BigInteger GcdFast(BigInteger x, BigInteger y)
    {
        if (x.IsOne || y.IsOne) return BigInteger.One;
        if (x.IsZero) return y;
        if (y.IsZero) return x;
        // Common powers of two come out by shifts: a denominator built from
        // (expt 16 i) is mostly a power of two, and the odd parts left are small.
#if !NETSTANDARD2_0
        if (x.GetBitLength() > 128 && y.GetBitLength() > 128)
        {
            int tx = (int)BigInteger.TrailingZeroCount(x), ty = (int)BigInteger.TrailingZeroCount(y);
            if ((tx | ty) != 0)
                return BigInteger.GreatestCommonDivisor(x >> tx, y >> ty) << Math.Min(tx, ty);
        }
#endif
        return BigInteger.GreatestCommonDivisor(x, y);
    }

    /// <summary>AN/AD + BN/BD (or minus, when SUBTRACT), already in lowest terms.</summary>
    private static Number RationalAddSub(BigInteger an, BigInteger ad,
                                         BigInteger bn, BigInteger bd, bool subtract)
    {
        if (subtract) bn = -bn;
        // An integer operand: its multiple of the other denominator keeps that
        // denominator coprime to the numerator.
        if (bd.IsOne) return Ratio.MakeReduced(an + bn * ad, ad);
        if (ad.IsOne) return Ratio.MakeReduced(an * bd + bn, bd);
        var g1 = GcdFast(ad, bd);
        // Denominators with no common factor: the sum over their product is
        // already in lowest terms. This is every integer-plus-fraction, and most
        // fraction pairs.
        if (g1.IsOne) return Ratio.MakeReduced(an * bd + bn * ad, ad * bd);
        var ad1 = ad / g1;
        var t = an * (bd / g1) + bn * ad1;
        var g2 = GcdFast(BigInteger.Abs(t), g1);
        return Ratio.MakeReduced(t / g2, ad1 * (bd / g2));
    }

    /// <summary>(AN/AD) * (BN/BD), already in lowest terms.</summary>
    private static Number RationalMultiply(BigInteger an, BigInteger ad,
                                           BigInteger bn, BigInteger bd)
    {
        // Cancel each numerator against the OTHER denominator: that is where a
        // common factor can be, and the numbers are still operand-sized here.
        // Dividing by a factor of 1 is skipped -- BigInteger divides even by one,
        // and for coprime operands that would be four such divisions.
        var g1 = GcdFast(BigInteger.Abs(an), bd);
        var g2 = GcdFast(BigInteger.Abs(bn), ad);
        var num = MulSkipOne(g1.IsOne ? an : an / g1, g2.IsOne ? bn : bn / g2);
        var den = MulSkipOne(g2.IsOne ? ad : ad / g2, g1.IsOne ? bd : bd / g1);
        return Ratio.MakeReduced(num, den);
    }

    private static BigInteger MulSkipOne(BigInteger x, BigInteger y) =>
        x.IsOne ? y : y.IsOne ? x : x * y;

    /// <summary>1/R: the reduced fraction turned over, the sign moved to the numerator.</summary>
    internal static Number RatioReciprocal(Ratio r) =>
        r.Numerator.Sign < 0
            ? Ratio.MakeReduced(-r.Denominator, -r.Numerator)
            : Ratio.MakeReduced(r.Denominator, r.Numerator);

    private static (BigInteger num, BigInteger den) AsRational(Number n) => n switch
    {
        Fixnum f => (f.Value, BigInteger.One),
        Bignum b => (b.Value, BigInteger.One),
        Ratio r => (r.Numerator, r.Denominator),
        // A decimal contributes its exact rational value; standard exact arithmetic then
        // yields a standard rational/integer (never a decimal: conservative extension).
        LispDecimal d => d.AsRatio(),
        // A complex reaching here means a real-only operation (FLOOR / CEILING /
        // TRUNCATE / ROUND ...) was handed one: a wrong argument, not a broken
        // program. A raw ArgumentException here surfaced as PROGRAM-ERROR with no
        // datum to inspect.
        _ => throw new LispErrorException(new LispTypeError(
                 $"not a rational number: {n}", n,
                 Startup.Sym(n is LispComplex ? "REAL" : "RATIONAL")))
    };

    // A complex whose parts are both DoubleFloat, or a real DoubleFloat (whose
    // imaginary part is an exact zero that every path below would turn into 0.0
    // anyway). Reading the parts as raw doubles lets the complex operations run
    // the same formula on the same values without boxing each intermediate.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool TryDoubleParts(Number n, out double re, out double im)
    {
        // Reading the fields, not the Real/Imaginary properties: those box.
        if (n is DoubleComplex dc) { re = dc.RealValue; im = dc.ImagValue; return true; }
        if (n is DoubleFloat d) { re = d.Value; im = 0.0; return true; }
        re = im = 0.0; return false;
    }

    private static LispComplex AsComplex(Number n) => n switch
    {
        LispComplex c => c,
        _ => LispComplex.Of(n, Fixnum.Make(0))
    };

    private static Number MakeComplex(Number real, Number imag)
    {
        // Float contagion (CLHS 12.1.4.4) governs the pair, not just the part an
        // operation happened to touch. Adding a double to #C(1 2) runs the real
        // parts through ADD and carries the imaginary part across untouched, so
        // without this the answer was #C(4.5d0 2) -- a complex with one float
        // part and one rational part, which no CL value may be.
        if (real is DoubleFloat && imag is not DoubleFloat)
            imag = new DoubleFloat(ToDouble(imag));
        else if (imag is DoubleFloat && real is not DoubleFloat)
            real = new DoubleFloat(ToDouble(real));
        else if (real is SingleFloat && imag is not SingleFloat)
            imag = new SingleFloat((float)ToDouble(imag));
        else if (imag is SingleFloat && real is not SingleFloat)
            real = new SingleFloat((float)ToDouble(real));
        // After the widening, not before: a zero imaginary part collapses to the
        // real number only while it is still exact. Once contagion has made it
        // 0.0 the value stays complex, which is what CLHS 12.1.5.3 requires.
        if (imag is Fixnum fi && fi.Value == 0) return real;
        return LispComplex.Of(real, imag);
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
        return LispComplex.Of(real, imag);
    }

    /// <summary>CLHS asin(z) = -i log(iz + sqrt(1-z)*sqrt(1+z)). Used to promote a
    /// real |x|>1 to the CL-correct complex branch (.NET Complex.Asin picks the other
    /// side of the real cut).</summary>
    public static System.Numerics.Complex AsinComplex(System.Numerics.Complex z)
    {
        var i = System.Numerics.Complex.ImaginaryOne;
        var s = System.Numerics.Complex.Sqrt(1 - z) * System.Numerics.Complex.Sqrt(1 + z);
        return -i * System.Numerics.Complex.Log(i * z + s);
    }

    /// <summary>CLHS acos(z) = -i log(z + i*sqrt(1-z)*sqrt(1+z)).</summary>
    public static System.Numerics.Complex AcosComplex(System.Numerics.Complex z)
    {
        var i = System.Numerics.Complex.ImaginaryOne;
        var s = System.Numerics.Complex.Sqrt(1 - z) * System.Numerics.Complex.Sqrt(1 + z);
        return -i * System.Numerics.Complex.Log(z + i * s);
    }

    /// <summary>CLHS acosh(z) = log(z + sqrt(z-1)*sqrt(z+1)). The factored sqrt
    /// (not sqrt(z*z-1)) chooses the branch CL specifies on the real cut.</summary>
    public static System.Numerics.Complex AcoshComplex(System.Numerics.Complex z)
        => System.Numerics.Complex.Log(z + System.Numerics.Complex.Sqrt(z - 1) * System.Numerics.Complex.Sqrt(z + 1));

    /// <summary>CLHS atanh(z) = (log(1+z) - log(1-z))/2. Separated logs (not
    /// log((1+z)/(1-z))) keep the branch right on the real cut |x|>1.</summary>
    public static System.Numerics.Complex AtanhComplex(System.Numerics.Complex z)
        => 0.5 * (System.Numerics.Complex.Log(1 + z) - System.Numerics.Complex.Log(1 - z));

    /// <summary>Convert a LispComplex to System.Numerics.Complex.</summary>
    public static System.Numerics.Complex ToSystemComplex(LispComplex c)
        // The DoubleComplex case first: reading its Real/Imaginary properties
        // would box both parts on the way to unboxing them again.
        => c is DoubleComplex dc
            ? new System.Numerics.Complex(dc.RealValue, dc.ImagValue)
            : new System.Numerics.Complex(ToDouble(c.Real), ToDouble(c.Imaginary));

    /// <summary>Convert a System.Numerics.Complex back to a Lisp number.
    /// If imaginary part is 0, returns just the real part as a DoubleFloat.</summary>
    public static LispObject FromSystemComplex(System.Numerics.Complex c)
    {
        if (c.Imaginary == 0.0)
            return new DoubleFloat(c.Real);
        return new DoubleComplex(c.Real, c.Imaginary);
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
            return new BoxedComplex(new SingleFloat((float)c.Real), new SingleFloat((float)c.Imaginary));
        }
        // DoubleFloat path
        if (c.Imaginary == 0.0 && !originalIsComplex)
            return new DoubleFloat(c.Real);
        return new DoubleComplex(c.Real, c.Imaginary);
    }
}
