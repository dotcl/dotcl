using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    // --- Arithmetic (backward compat + number tower) ---

    public static LispObject Add(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb)
        {
            long va = fa.Value, vb = fb.Value;
            long result = va + vb;
            // Overflow check: sign of result must be consistent
            if (((va ^ vb) < 0) || ((va ^ result) >= 0))
                return Fixnum.Make(result);
            return Bignum.MakeInteger(new System.Numerics.BigInteger(va) + vb);
        }
        if (a is DoubleFloat da && b is DoubleFloat db)
            return new DoubleFloat(da.Value + db.Value);
        if (a is SingleFloat sfa && b is SingleFloat sfb)
            return new SingleFloat(sfa.Value + sfb.Value);
        return Arithmetic.Add(AsNumber(a), AsNumber(b));
    }

    public static LispObject Subtract(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb)
        {
            long va = fa.Value, vb = fb.Value;
            long result = va - vb;
            // Overflow check: sign of result must be consistent
            if (((va ^ vb) >= 0) || ((va ^ result) >= 0))
                return Fixnum.Make(result);
            return Bignum.MakeInteger(new System.Numerics.BigInteger(va) - vb);
        }
        if (a is DoubleFloat da && b is DoubleFloat db)
            return new DoubleFloat(da.Value - db.Value);
        if (a is SingleFloat sfa && b is SingleFloat sfb)
            return new SingleFloat(sfa.Value - sfb.Value);
        return Arithmetic.Subtract(AsNumber(a), AsNumber(b));
    }

    /// <summary>Fast path for (1+ x): avoids creating Fixnum(1) and second type check.</summary>
    public static LispObject Increment(LispObject a)
    {
        if (a is Fixnum fa)
        {
            long v = fa.Value;
            if (v < long.MaxValue) return Fixnum.Make(v + 1);
            return Bignum.MakeInteger(new System.Numerics.BigInteger(v) + 1);
        }
        return Arithmetic.Add(AsNumber(a), Fixnum.Make(1));
    }

    /// <summary>Fast path for (1- x): avoids creating Fixnum(1) and second type check.</summary>
    public static LispObject Decrement(LispObject a)
    {
        if (a is Fixnum fa)
        {
            long v = fa.Value;
            if (v > long.MinValue) return Fixnum.Make(v - 1);
            return Bignum.MakeInteger(new System.Numerics.BigInteger(v) - 1);
        }
        return Arithmetic.Subtract(AsNumber(a), Fixnum.Make(1));
    }

    /// <summary>Checked fixnum multiply: returns Fixnum or Bignum. (#154/D917)</summary>
    public static LispObject MultiplyFixnum(long a, long b)
    {
        long hi = Math.BigMul(a, b, out long lo);
        if (hi == (lo >> 63))
            return Fixnum.Make(lo);
        return Bignum.MakeInteger(new System.Numerics.BigInteger(a) * b);
    }

    public static LispObject Multiply(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb)
        {
            long va = fa.Value, vb = fb.Value;
            long hi = Math.BigMul(va, vb, out long result);
            if (hi == (result >> 63))
                return Fixnum.Make(result);
            return Bignum.MakeInteger(new System.Numerics.BigInteger(va) * vb);
        }
        if (a is DoubleFloat da && b is DoubleFloat db)
            return new DoubleFloat(da.Value * db.Value);
        if (a is SingleFloat sfa && b is SingleFloat sfb)
            return new SingleFloat(sfa.Value * sfb.Value);
        return Arithmetic.Multiply(AsNumber(a), AsNumber(b));
    }

    public static LispObject Divide(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb)
        {
            long bv = fb.Value;
            if (bv == 0) throw new LispErrorException(new LispError("/: division by zero") { ConditionTypeName = "DIVISION-BY-ZERO" });
            long av = fa.Value;
            if (av % bv == 0) return Fixnum.Make(av / bv);
            // Fall through to ratio creation via Arithmetic.Divide
        }
        if (a is DoubleFloat da && b is DoubleFloat db)
        {
            if (db.Value == 0.0) throw new LispErrorException(new LispError("/: division by zero") { ConditionTypeName = "DIVISION-BY-ZERO" });
            return new DoubleFloat(da.Value / db.Value);
        }
        if (a is SingleFloat sfa && b is SingleFloat sfb)
        {
            if (sfb.Value == 0.0f) throw new LispErrorException(new LispError("/: division by zero") { ConditionTypeName = "DIVISION-BY-ZERO" });
            return new SingleFloat(sfa.Value / sfb.Value);
        }
        try { return Arithmetic.Divide(AsNumber(a), AsNumber(b)); }
        catch (DivideByZeroException) { throw new LispErrorException(new LispError("/: division by zero") { ConditionTypeName = "DIVISION-BY-ZERO" }); }
    }

    // --- Comparison (backward compat) ---

    public static bool IsTruthy(LispObject obj) => Primary(obj) is not Nil;

    public static LispObject GreaterThan(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb)
            return fa.Value > fb.Value ? T.Instance : Nil.Instance;
        if (a is DoubleFloat da)
        {
            if (b is DoubleFloat db) return da.Value > db.Value ? T.Instance : Nil.Instance;
            if (b is SingleFloat sb) return da.Value > (double)sb.Value ? T.Instance : Nil.Instance;
        }
        if (a is SingleFloat sa)
        {
            if (b is SingleFloat sb2) return sa.Value > sb2.Value ? T.Instance : Nil.Instance;
            if (b is DoubleFloat db2) return (double)sa.Value > db2.Value ? T.Instance : Nil.Instance;
        }
        return Arithmetic.Compare(AsNumber(a), AsNumber(b)) > 0 ? T.Instance : Nil.Instance;
    }

    public static LispObject LessThan(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb)
            return fa.Value < fb.Value ? T.Instance : Nil.Instance;
        if (a is DoubleFloat da)
        {
            if (b is DoubleFloat db) return da.Value < db.Value ? T.Instance : Nil.Instance;
            if (b is SingleFloat sb) return da.Value < (double)sb.Value ? T.Instance : Nil.Instance;
        }
        if (a is SingleFloat sa)
        {
            if (b is SingleFloat sb2) return sa.Value < sb2.Value ? T.Instance : Nil.Instance;
            if (b is DoubleFloat db2) return (double)sa.Value < db2.Value ? T.Instance : Nil.Instance;
        }
        return Arithmetic.Compare(AsNumber(a), AsNumber(b)) < 0 ? T.Instance : Nil.Instance;
    }

    public static LispObject GreaterEqual(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb)
            return fa.Value >= fb.Value ? T.Instance : Nil.Instance;
        if (a is DoubleFloat da && b is DoubleFloat db)
            return da.Value >= db.Value ? T.Instance : Nil.Instance;
        return Arithmetic.Compare(AsNumber(a), AsNumber(b)) >= 0 ? T.Instance : Nil.Instance;
    }

    public static LispObject LessEqual(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb)
            return fa.Value <= fb.Value ? T.Instance : Nil.Instance;
        if (a is DoubleFloat da && b is DoubleFloat db)
            return da.Value <= db.Value ? T.Instance : Nil.Instance;
        return Arithmetic.Compare(AsNumber(a), AsNumber(b)) <= 0 ? T.Instance : Nil.Instance;
    }

    public static LispObject NumEqual(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb)
            return fa.Value == fb.Value ? T.Instance : Nil.Instance;
        if (a is DoubleFloat da && b is DoubleFloat db)
            return da.Value == db.Value ? T.Instance : Nil.Instance;
        return Arithmetic.IsNumericEqual(AsNumber(a), AsNumber(b)) ? T.Instance : Nil.Instance;
    }

    public static LispObject NumNotEqual(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb)
            return fa.Value != fb.Value ? T.Instance : Nil.Instance;
        return !Arithmetic.IsNumericEqual(AsNumber(a), AsNumber(b)) ? T.Instance : Nil.Instance;
    }

    // --- Bool-returning comparisons for fused comparison+branch in compile-if ---

    public static bool IsTrueGt(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb) return fa.Value > fb.Value;
        if (a is DoubleFloat da && b is DoubleFloat db) return da.Value > db.Value;
        return Arithmetic.Compare(AsNumber(a), AsNumber(b)) > 0;
    }

    public static bool IsTrueLt(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb) return fa.Value < fb.Value;
        if (a is DoubleFloat da && b is DoubleFloat db) return da.Value < db.Value;
        return Arithmetic.Compare(AsNumber(a), AsNumber(b)) < 0;
    }

    public static bool IsTrueGe(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb) return fa.Value >= fb.Value;
        if (a is DoubleFloat da && b is DoubleFloat db) return da.Value >= db.Value;
        return Arithmetic.Compare(AsNumber(a), AsNumber(b)) >= 0;
    }

    public static bool IsTrueLe(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb) return fa.Value <= fb.Value;
        if (a is DoubleFloat da && b is DoubleFloat db) return da.Value <= db.Value;
        return Arithmetic.Compare(AsNumber(a), AsNumber(b)) <= 0;
    }

    public static bool IsTrueNumEq(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb) return fa.Value == fb.Value;
        if (a is DoubleFloat da && b is DoubleFloat db) return da.Value == db.Value;
        return Arithmetic.IsNumericEqual(AsNumber(a), AsNumber(b));
    }

    // --- Bool-returning unary predicates (fused in compile-if) ---

    public static bool IsTrueZerop(LispObject a)
    {
        a = Primary(a);
        if (a is Fixnum fa) return fa.Value == 0;
        if (a is DoubleFloat da) return da.Value == 0.0;
        return Arithmetic.IsNumericEqual(AsNumber(a), Fixnum.Make(0));
    }

    public static bool IsTrueMinusp(LispObject a)
    {
        a = Primary(a);
        if (a is Fixnum fa) return fa.Value < 0;
        if (a is DoubleFloat da) return da.Value < 0.0;
        return Arithmetic.Compare(AsNumber(a), Fixnum.Make(0)) < 0;
    }

    public static bool IsTruePlusp(LispObject a)
    {
        a = Primary(a);
        if (a is Fixnum fa) return fa.Value > 0;
        if (a is DoubleFloat da) return da.Value > 0.0;
        return Arithmetic.Compare(AsNumber(a), Fixnum.Make(0)) > 0;
    }

    // --- Bool-returning equality (fused in compile-if) ---

    public static bool IsTrueEq(LispObject a, LispObject b)
    {
        a = Primary(a); b = Primary(b);
        if (ReferenceEquals(a, b)) return true;
        if ((a is T && ReferenceEquals(b, Startup.T_SYM)) ||
            (b is T && ReferenceEquals(a, Startup.T_SYM)))
            return true;
        if ((a is Nil && ReferenceEquals(b, Startup.NIL_SYM)) ||
            (b is Nil && ReferenceEquals(a, Startup.NIL_SYM)))
            return true;
        return false;
    }

    public static bool IsTrueEql(LispObject a, LispObject b)
    {
        a = Primary(a); b = Primary(b);
        if (ReferenceEquals(a, b)) return true;
        if ((a is T && ReferenceEquals(b, Startup.T_SYM)) ||
            (b is T && ReferenceEquals(a, Startup.T_SYM)))
            return true;
        if ((a is Nil && ReferenceEquals(b, Startup.NIL_SYM)) ||
            (b is Nil && ReferenceEquals(a, Startup.NIL_SYM)))
            return true;
        if (a is Fixnum fa && b is Fixnum fb) return fa.Value == fb.Value;
        if (a is DoubleFloat da && b is DoubleFloat db) return da.Value == db.Value;
        if (a is SingleFloat sa && b is SingleFloat sb) return sa.Value == sb.Value;
        if (a is LispChar ca && b is LispChar cb) return ca.Value == cb.Value;
        if (a is Bignum ba && b is Bignum bb) return ba.Value == bb.Value;
        if (a is Ratio ra && b is Ratio rb) return ra.Numerator == rb.Numerator && ra.Denominator == rb.Denominator;
        if (a is LispComplex xa && b is LispComplex xb)
            return IsTrueEql(xa.Real, xb.Real) && IsTrueEql(xa.Imaginary, xb.Imaginary);
        return false;
    }

    // --- Variadic comparison (for #'= etc. via funcall with any arg count) ---

    private static void RequireAtLeastOne(string name, LispObject[] args)
    {
        if (args.Length == 0) throw new LispErrorException(new LispProgramError($"{name}: too few arguments"));
    }

    public static LispObject NumEqualN(params LispObject[] args)
    {
        RequireAtLeastOne("=", args);
        if (args.Length == 1) { AsNumber(args[0]); return T.Instance; }
        for (int i = 0; i < args.Length - 1; i++)
            if (!Arithmetic.IsNumericEqual(AsNumber(args[i]), AsNumber(args[i + 1]))) return Nil.Instance;
        return T.Instance;
    }

    public static LispObject NumNotEqualN(params LispObject[] args)
    {
        RequireAtLeastOne("/=", args);
        if (args.Length == 1) { AsNumber(args[0]); return T.Instance; }
        var nums = new Number[args.Length];
        for (int i = 0; i < args.Length; i++) nums[i] = AsNumber(args[i]);
        for (int i = 0; i < nums.Length - 1; i++)
            for (int j = i + 1; j < nums.Length; j++)
                if (Arithmetic.IsNumericEqual(nums[i], nums[j])) return Nil.Instance;
        return T.Instance;
    }

    public static LispObject LessThanN(params LispObject[] args)
    {
        RequireAtLeastOne("<", args);
        if (args.Length == 1) { AsNumber(args[0]); return T.Instance; }
        for (int i = 0; i < args.Length - 1; i++)
            if (Arithmetic.Compare(AsNumber(args[i]), AsNumber(args[i + 1])) >= 0) return Nil.Instance;
        return T.Instance;
    }

    public static LispObject GreaterThanN(params LispObject[] args)
    {
        RequireAtLeastOne(">", args);
        if (args.Length == 1) { AsNumber(args[0]); return T.Instance; }
        for (int i = 0; i < args.Length - 1; i++)
            if (Arithmetic.Compare(AsNumber(args[i]), AsNumber(args[i + 1])) <= 0) return Nil.Instance;
        return T.Instance;
    }

    public static LispObject LessEqualN(params LispObject[] args)
    {
        RequireAtLeastOne("<=", args);
        if (args.Length == 1) { AsNumber(args[0]); return T.Instance; }
        for (int i = 0; i < args.Length - 1; i++)
            if (Arithmetic.Compare(AsNumber(args[i]), AsNumber(args[i + 1])) > 0) return Nil.Instance;
        return T.Instance;
    }

    public static LispObject GreaterEqualN(params LispObject[] args)
    {
        RequireAtLeastOne(">=", args);
        if (args.Length == 1) { AsNumber(args[0]); return T.Instance; }
        for (int i = 0; i < args.Length - 1; i++)
            if (Arithmetic.Compare(AsNumber(args[i]), AsNumber(args[i + 1])) < 0) return Nil.Instance;
        return T.Instance;
    }

    // --- Variadic arithmetic ---

    public static LispObject AddN(params LispObject[] args)
    {
        if (args.Length == 0) return Fixnum.Make(0);
        LispObject result = args[0];
        for (int i = 1; i < args.Length; i++)
            result = Add(result, args[i]);
        return result;
    }

    public static LispObject MultiplyN(params LispObject[] args)
    {
        if (args.Length == 0) return Fixnum.Make(1);
        LispObject result = args[0];
        for (int i = 1; i < args.Length; i++)
            result = Multiply(result, args[i]);
        return result;
    }

    public static LispObject SubtractN(params LispObject[] args)
    {
        if (args.Length == 0) throw new ArgumentException("- requires at least one argument");
        if (args.Length == 1) return Arithmetic.Negate(AsNumber(args[0]));
        LispObject result = args[0];
        for (int i = 1; i < args.Length; i++)
            result = Subtract(result, args[i]);
        return result;
    }

    public static LispObject DivideN(params LispObject[] args)
    {
        if (args.Length == 0) throw new ArgumentException("/ requires at least one argument");
        if (args.Length == 1)
        {
            try { return Arithmetic.Divide(Fixnum.Make(1), AsNumber(args[0])); }
            catch (DivideByZeroException) { throw new LispErrorException(new LispError("/: division by zero") { ConditionTypeName = "DIVISION-BY-ZERO" }); }
        }
        LispObject result = args[0];
        for (int i = 1; i < args.Length; i++)
            result = Divide(result, args[i]);
        return result;
    }

    // --- Math functions ---

    public static LispObject Abs(LispObject a)
    {
        if (a is Fixnum f)
            return f.Value >= 0 ? (LispObject)f : (f.Value == long.MinValue ? (LispObject)new Bignum(-((System.Numerics.BigInteger)f.Value)) : Fixnum.Make(System.Math.Abs(f.Value)));
        if (a is DoubleFloat df)
            return new DoubleFloat(System.Math.Abs(df.Value));
        if (a is SingleFloat sf)
            return new SingleFloat(System.Math.Abs(sf.Value));
        return Arithmetic.Abs(AsNumber(a));
    }

    private static LispRandomState GetCurrentRandomState()
    {
        var val = DynamicBindings.Get(Startup.Sym("*RANDOM-STATE*"));
        return val is LispRandomState rs ? rs : new LispRandomState();
    }
    private static LispObject RandomImpl(LispObject limit, LispRandomState rs)
    {
        if (limit is Fixnum f && f.Value > 0)
            return Fixnum.Make((long)rs.NextBelow(f.Value));
        if (limit is SingleFloat sf && sf.Value > 0)
            return new SingleFloat(rs.NextSingle() * sf.Value);
        if (limit is DoubleFloat d && d.Value > 0)
            return new DoubleFloat(rs.NextDouble() * d.Value);
        if (limit is Bignum bg && bg.Value > 0)
            return Bignum.MakeInteger(rs.NextBelow(bg.Value));
        throw new LispErrorException(new LispTypeError("RANDOM: limit must be a positive number", limit));
    }
    public static LispObject Random(LispObject limit)
        => RandomImpl(limit, GetCurrentRandomState());
    public static LispObject Random2(LispObject limit, LispObject state)
    {
        var rs = state is LispRandomState r ? r : GetCurrentRandomState();
        return RandomImpl(limit, rs);
    }
    public static LispObject Expt(LispObject baseObj, LispObject power)
    {
        // Determine if each arg is a float (and which kind)
        bool baseIsFloat = baseObj is SingleFloat || baseObj is DoubleFloat;
        bool powerIsFloat = power is SingleFloat || power is DoubleFloat;
        bool baseIsDouble = baseObj is DoubleFloat;
        bool powerIsDouble = power is DoubleFloat;
        bool baseIsSingle = baseObj is SingleFloat;
        bool powerIsSingle = power is SingleFloat;
        bool baseIsExact = baseObj is Fixnum || baseObj is Bignum || baseObj is Ratio;
        bool powerIsExact = power is Fixnum || power is Bignum || power is Ratio;
        bool baseIsComplex = baseObj is LispComplex;
        bool powerIsComplex = power is LispComplex;

        // Validate inputs are numbers
        if (!baseIsFloat && !baseIsExact && !baseIsComplex)
            throw new LispErrorException(new LispTypeError("EXPT: not a number", baseObj));
        if (!powerIsFloat && !powerIsExact && !powerIsComplex)
            throw new LispErrorException(new LispTypeError("EXPT: not a number", power));

        // 0. base == 0 with positive realpart(power): (expt 0 y) = (* 0 y) for type contagion
        {
            bool baseZero = (baseObj is Fixnum fzb && fzb.Value == 0)
                || (baseObj is Bignum bzb && bzb.Value.IsZero)
                || (baseObj is SingleFloat sfzb && sfzb.Value == 0.0f)
                || (baseObj is DoubleFloat dfzb && dfzb.Value == 0.0)
                || (baseObj is LispComplex lczb
                    && Arithmetic.ToDouble(lczb.Real) == 0.0
                    && Arithmetic.ToDouble(lczb.Imaginary) == 0.0);
            if (baseZero)
            {
                double powerReal = power is LispComplex lcpw
                    ? Arithmetic.ToDouble(lcpw.Real)
                    : Arithmetic.ToDouble(AsNumber(power));
                if (powerReal > 0)
                    return Arithmetic.Multiply(AsNumber(baseObj), AsNumber(power));
            }
        }

        // 1. power == 0: return appropriate 1 (before complex handling)
        bool powerIsZero = (power is Fixnum fz && fz.Value == 0)
            || (power is Bignum bz && bz.Value.IsZero)
            || (power is SingleFloat sfz && sfz.Value == 0.0f)
            || (power is DoubleFloat dfz && dfz.Value == 0.0)
            || (power is Ratio rz && rz.Numerator.IsZero);
        if (powerIsZero)
        {
            if (baseIsComplex)
            {
                var lc = (LispComplex)baseObj;
                // (complex single-float) => #c(1.0s0 0.0s0)
                if (lc.Real is SingleFloat)
                    return new LispComplex(new SingleFloat(1.0f), new SingleFloat(0.0f));
                // (complex double-float) => #c(1.0d0 0.0d0)
                if (lc.Real is DoubleFloat)
                    return new LispComplex(new DoubleFloat(1.0), new DoubleFloat(0.0));
                // (complex integer) or (complex rational) => 1
                return Fixnum.Make(1);
            }
            // CL spec: (expt float 0) => (float 1 float)
            if (baseIsDouble || powerIsDouble)
                return new DoubleFloat(1.0);
            if (baseIsSingle || powerIsSingle)
                return new SingleFloat(1.0f);
            return Fixnum.Make(1);
        }

        // 2. Handle complex base with integer power: exact arithmetic
        if (baseIsComplex && (power is Fixnum || power is Bignum))
        {
            var bigPow = power is Fixnum fpC ? (System.Numerics.BigInteger)fpC.Value : ((Bignum)power).Value;
            var lc = (LispComplex)baseObj;
            if (bigPow > 0 && bigPow <= int.MaxValue)
            {
                int p = (int)bigPow;
                // Multiply base by itself p times using exact Arithmetic
                Number result = lc;
                for (int i = 1; i < p; i++)
                    result = Arithmetic.Multiply(result, lc);
                return result;
            }
            else if (bigPow < 0 && -bigPow <= int.MaxValue)
            {
                int p = (int)(-bigPow);
                // Compute base^|p| exactly, then take reciprocal: 1/(a+bi)
                Number pos = lc;
                for (int i = 1; i < p; i++)
                    pos = Arithmetic.Multiply(pos, lc);
                // Divide 1 by the result
                return Arithmetic.Divide(Fixnum.Make(1), pos);
            }
        }

        // 3. Handle remaining complex inputs via System.Numerics.Complex
        if (baseIsComplex || powerIsComplex)
        {
            var bc = baseObj is LispComplex lc1
                ? Arithmetic.ToSystemComplex(lc1)
                : new System.Numerics.Complex(Arithmetic.ToDouble(AsNumber(baseObj)), 0);
            var pc = power is LispComplex lc2
                ? Arithmetic.ToSystemComplex(lc2)
                : new System.Numerics.Complex(Arithmetic.ToDouble(AsNumber(power)), 0);
            return Arithmetic.FromSystemComplex(System.Numerics.Complex.Pow(bc, pc));
        }

        // 3. Both exact integers and power > 0: use BigInteger.Pow
        if ((baseObj is Fixnum || baseObj is Bignum) && (power is Fixnum || power is Bignum))
        {
            var bigBase = baseObj is Fixnum fb ? (System.Numerics.BigInteger)fb.Value : ((Bignum)baseObj).Value;
            var bigPow = power is Fixnum fp ? (System.Numerics.BigInteger)fp.Value : ((Bignum)power).Value;

            if (bigPow > 0)
            {
                // Power must fit in int for BigInteger.Pow
                if (bigPow <= int.MaxValue)
                {
                    var result = System.Numerics.BigInteger.Pow(bigBase, (int)bigPow);
                    return MakeInteger(result);
                }
                // Huge exponent: fall through to double
            }
            else if (bigPow < 0)
            {
                // (expt integer negative-integer) => ratio (1 / base^|power|)
                if (bigBase.IsZero)
                    throw new LispErrorException(new LispError("EXPT: division by zero (0 raised to negative power)") { ConditionTypeName = "DIVISION-BY-ZERO" });
                if (-bigPow <= int.MaxValue)
                {
                    var denom = System.Numerics.BigInteger.Pow(bigBase, (int)(-bigPow));
                    return Ratio.Make(System.Numerics.BigInteger.One, denom);
                }
                // Huge negative exponent: fall through to double
            }
        }

        // 4. Ratio base with integer power: exact computation
        if (baseObj is Ratio rBase && (power is Fixnum || power is Bignum))
        {
            var bigPow = power is Fixnum fpR ? (System.Numerics.BigInteger)fpR.Value : ((Bignum)power).Value;
            if (bigPow >= 0 && bigPow <= int.MaxValue)
            {
                int p = (int)bigPow;
                var num = System.Numerics.BigInteger.Pow(rBase.Numerator, p);
                var den = System.Numerics.BigInteger.Pow(rBase.Denominator, p);
                return Ratio.Make(num, den);
            }
            else if (bigPow < 0 && -bigPow <= int.MaxValue)
            {
                int p = (int)(-bigPow);
                // Invert and raise to positive power
                var num = System.Numerics.BigInteger.Pow(rBase.Denominator, p);
                var den = System.Numerics.BigInteger.Pow(rBase.Numerator, p);
                return Ratio.Make(num, den);
            }
            // Huge exponent: fall through to double
        }

        // 5. Float path: convert to double, compute, then apply contagion
        double bd = Arithmetic.ToDouble(AsNumber(baseObj));
        double pd = Arithmetic.ToDouble(AsNumber(power));
        // Negative base with non-integer power produces complex result
        if (bd < 0 && pd != Math.Floor(pd))
        {
            var bc = new System.Numerics.Complex(bd, 0);
            var pc = new System.Numerics.Complex(pd, 0);
            return Arithmetic.FromSystemComplex(System.Numerics.Complex.Pow(bc, pc), AsNumber(baseObj));
        }
        double result_d = Math.Pow(bd, pd);

        // Check for single-float overflow/underflow: result_d is finite in double but
        // overflows or underflows when cast to single-float
        bool resultIsSingle = (baseIsSingle || powerIsSingle) && !baseIsDouble && !powerIsDouble;
        if (resultIsSingle && !double.IsInfinity(result_d) && !double.IsNaN(result_d))
        {
            float resultF = (float)result_d;
            if (float.IsInfinity(resultF))
            {
                var cond = new LispError($"EXPT: floating-point overflow computing ({baseObj}) ^ ({power})")
                    { ConditionTypeName = "FLOATING-POINT-OVERFLOW" };
                throw new LispErrorException(cond);
            }
            if (resultF == 0.0f && result_d != 0.0)
            {
                var cond = new LispError($"EXPT: floating-point underflow computing ({baseObj}) ^ ({power})")
                    { ConditionTypeName = "FLOATING-POINT-UNDERFLOW" };
                throw new LispErrorException(cond);
            }
        }

        // Check for overflow (infinity)
        if (double.IsInfinity(result_d) && !double.IsInfinity(bd) && !double.IsInfinity(pd))
        {
            var cond = new LispError($"EXPT: floating-point overflow computing ({baseObj}) ^ ({power})")
                { ConditionTypeName = "FLOATING-POINT-OVERFLOW" };
            throw new LispErrorException(cond);
        }

        // Check for underflow (result is 0 but inputs are nonzero)
        if (result_d == 0.0 && bd != 0.0 && pd != 0.0 && !double.IsNaN(result_d))
        {
            // Only signal underflow if inputs were floats (exact->float conversion loss is normal)
            if (baseIsFloat || powerIsFloat)
            {
                var cond = new LispError($"EXPT: floating-point underflow computing ({baseObj}) ^ ({power})")
                    { ConditionTypeName = "FLOATING-POINT-UNDERFLOW" };
                throw new LispErrorException(cond);
            }
        }

        // Float contagion: double wins over single, single wins over exact
        if (baseIsDouble || powerIsDouble)
            return new DoubleFloat(result_d);
        if (baseIsSingle || powerIsSingle)
            return new SingleFloat((float)result_d);
        // Both exact but we fell through to float (huge exponent, etc.)
        return new DoubleFloat(result_d);
    }

    public static LispObject Mod(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb)
        {
            long av = fa.Value, bv = fb.Value;
            long r = av % bv;
            if (r != 0 && ((r ^ bv) < 0)) r += bv;
            return Fixnum.Make(r);
        }
        return Arithmetic.Mod(AsNumber(a), AsNumber(b));
    }

    public static LispObject Rem(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb)
            return Fixnum.Make(fa.Value % fb.Value);
        return Arithmetic.Rem(AsNumber(a), AsNumber(b));
    }
    public static LispObject Gcd(LispObject a, LispObject b) => Arithmetic.Gcd(AsNumber(a), AsNumber(b));

    public static LispObject FloorOp(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb)
        {
            long av = fa.Value, bv = fb.Value;
            long q = Math.DivRem(av, bv, out long r);
            if (r != 0 && ((r ^ bv) < 0)) { q--; r += bv; }
            return MultipleValues.Values(Fixnum.Make(q), Fixnum.Make(r));
        }
        var (qq, rr) = Arithmetic.Floor(AsNumber(a), AsNumber(b));
        return MultipleValues.Values(qq, rr);
    }
    public static LispObject TruncateOp(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb)
        {
            long av = fa.Value, bv = fb.Value;
            long q = av / bv;
            long r = av - q * bv;
            return MultipleValues.Values(Fixnum.Make(q), Fixnum.Make(r));
        }
        var (qq, rr) = Arithmetic.Truncate(AsNumber(a), AsNumber(b));
        return MultipleValues.Values(qq, rr);
    }
    public static LispObject CeilingOp(LispObject a, LispObject b)
    {
        var (q, r) = Arithmetic.Ceiling(AsNumber(a), AsNumber(b));
        return MultipleValues.Values(q, r);
    }
    public static LispObject RoundOp(LispObject a, LispObject b)
    {
        var (q, r) = Arithmetic.Round(AsNumber(a), AsNumber(b));
        return MultipleValues.Values(q, r);
    }
    public static LispObject Lcm(LispObject a, LispObject b) => Arithmetic.Lcm(AsNumber(a), AsNumber(b));

    // Helper: extract integer value as BigInteger (handles Fixnum and Bignum)
    internal static System.Numerics.BigInteger GetBigInt(LispObject a) =>
        a is Fixnum f ? (System.Numerics.BigInteger)f.Value :
        a is Bignum b ? b.Value :
        throw new LispErrorException(new LispTypeError("not an integer", a));

    // Helper: return Fixnum if result fits in long, else Bignum
    private static LispObject MakeInteger(System.Numerics.BigInteger n) =>
        n >= long.MinValue && n <= long.MaxValue ? Fixnum.Make((long)n) : Bignum.MakeInteger(n);

    // Binary bitwise operations — avoid LispObject[] allocation for common 2-arg case
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject Logior2(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb)
            return Fixnum.Make(fa.Value | fb.Value);
        return MakeInteger(GetBigInt(a) | GetBigInt(b));
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject Logand2(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb)
            return Fixnum.Make(fa.Value & fb.Value);
        return MakeInteger(GetBigInt(a) & GetBigInt(b));
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject Logxor2(LispObject a, LispObject b)
    {
        if (a is Fixnum fa && b is Fixnum fb)
            return Fixnum.Make(fa.Value ^ fb.Value);
        return MakeInteger(GetBigInt(a) ^ GetBigInt(b));
    }

    // Bitwise logical operations — fixnum fast paths avoid BigInteger allocation
    public static LispObject Logior(LispObject[] args)
    {
        if (args.Length == 2 && args[0] is Fixnum f0 && args[1] is Fixnum f1)
            return Fixnum.Make(f0.Value | f1.Value);
        // All-fixnum fast path
        bool allFixnum = true;
        long result = 0;
        foreach (var a in args)
        {
            if (a is Fixnum f) result |= f.Value;
            else { allFixnum = false; break; }
        }
        if (allFixnum) return Fixnum.Make(result);
        var big = System.Numerics.BigInteger.Zero;
        foreach (var a in args) big |= GetBigInt(a);
        return MakeInteger(big);
    }
    public static LispObject Logand(LispObject[] args)
    {
        if (args.Length == 2 && args[0] is Fixnum f0 && args[1] is Fixnum f1)
            return Fixnum.Make(f0.Value & f1.Value);
        bool allFixnum = true;
        long result = -1; // identity for AND
        foreach (var a in args)
        {
            if (a is Fixnum f) result &= f.Value;
            else { allFixnum = false; break; }
        }
        if (allFixnum) return Fixnum.Make(result);
        var big = new System.Numerics.BigInteger(-1);
        foreach (var a in args) big &= GetBigInt(a);
        return MakeInteger(big);
    }
    public static LispObject Logxor(LispObject[] args)
    {
        if (args.Length == 2 && args[0] is Fixnum f0 && args[1] is Fixnum f1)
            return Fixnum.Make(f0.Value ^ f1.Value);
        bool allFixnum = true;
        long result = 0;
        foreach (var a in args)
        {
            if (a is Fixnum f) result ^= f.Value;
            else { allFixnum = false; break; }
        }
        if (allFixnum) return Fixnum.Make(result);
        var big = System.Numerics.BigInteger.Zero;
        foreach (var a in args) big ^= GetBigInt(a);
        return MakeInteger(big);
    }
    public static LispObject Lognot(LispObject a) =>
        a is Fixnum f ? Fixnum.Make(~f.Value) : MakeInteger(~GetBigInt(a));
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject Ash(LispObject integer, LispObject count)
    {
        integer = Primary(integer); count = Primary(count);
        // Fixnum fast path: avoid BigInteger allocation for common case
        if (integer is Fixnum fi && count is Fixnum fc)
        {
            long n = fi.Value;
            long c = fc.Value;
            if (n == 0) return Fixnum.Make(0);
            if (c >= 0 && c < 63)
            {
                // Check for overflow: if shifting would exceed long range, fall through to BigInteger
                if (c == 0) return integer;
                int shift = (int)c;
                // Safe if high bits won't be lost
                if (n > 0 && n <= (long.MaxValue >> shift))
                    return Fixnum.Make(n << shift);
                if (n < 0 && n >= (long.MinValue >> shift))
                    return Fixnum.Make(n << shift);
                // Fall through to BigInteger path
            }
            else if (c < 0 && c > -64)
            {
                return Fixnum.Make(n >> (int)(-c));
            }
            else if (c <= -64)
            {
                return n < 0 ? Fixnum.Make(-1) : Fixnum.Make(0);
            }
        }
        System.Numerics.BigInteger bn = integer is Fixnum fi2 ? fi2.Value :
            integer is Bignum bi ? bi.Value :
            throw new LispErrorException(new LispTypeError("ASH: not an integer", integer));
        System.Numerics.BigInteger bc = count is Fixnum fc2 ? fc2.Value :
            count is Bignum bci ? bci.Value :
            throw new LispErrorException(new LispTypeError("ASH: count not an integer", count));
        if (bn.IsZero) return Fixnum.Make(0);
        System.Numerics.BigInteger result;
        if (bc >= 0)
        {
            if (bc > int.MaxValue)
                throw new LispErrorException(new LispError("ASH: shift count too large"));
            result = bn << (int)bc;
        }
        else
        {
            if (-bc > int.MaxValue)
                return bn < 0 ? Fixnum.Make(-1) : Fixnum.Make(0);
            result = bn >> (int)(-bc);
        }
        if (result >= long.MinValue && result <= long.MaxValue)
            return Fixnum.Make((long)result);
        return Bignum.MakeInteger(result);
    }
    public static LispObject IntegerLength(LispObject obj)
    {
        System.Numerics.BigInteger n = obj switch
        {
            Fixnum f => f.Value,
            Bignum b => b.Value,
            _ => throw new LispErrorException(new LispTypeError("INTEGER-LENGTH: not an integer", obj, Startup.Sym("INTEGER")))
        };
        if (n.IsZero) return Fixnum.Make(0);
        if (n < 0) n = ~n;
        return Fixnum.Make((int)n.GetBitLength());
    }

    public static LispObject Logbitp(LispObject index, LispObject integer)
    {
        System.Numerics.BigInteger bitIdx = index is Fixnum fi ? fi.Value :
            index is Bignum bi ? bi.Value :
            throw new LispErrorException(new LispTypeError("LOGBITP: not a non-negative integer", index));
        var n = GetBigInt(integer);
        if (bitIdx < 0)
            throw new LispErrorException(new LispTypeError("LOGBITP: index must be non-negative", index));
        if (bitIdx > int.MaxValue)
            // For very large bit index, the sign bit determines the result
            return n < 0 ? T.Instance : Nil.Instance;
        return (n & (System.Numerics.BigInteger.One << (int)bitIdx)) != 0 ? T.Instance : Nil.Instance;
    }

    // CL documentation storage: (symbol, doc-type-string) → LispObject
    private static readonly Dictionary<(string sym, string docType), LispObject> _docs = new();

    // Called by GeneratedDocs.Register() (source-generated from [LispDoc] attributes).
    internal static void SetFunctionDoc(string lispName, string docstring) =>
        _docs[(lispName, "FUNCTION")] = new LispString(docstring);

    // Logical pathname translations: host name (uppercase) -> list of (from to) translation rules
    internal static readonly Dictionary<string, LispObject> _logicalPathnameTranslations = new(StringComparer.OrdinalIgnoreCase);

    public static LispObject LogicalPathnameTranslations(LispObject host)
    {
        var hostName = host is LispString s ? s.Value : host is LispVector v && v.IsCharVector ? v.ToCharString() : host.ToString();
        hostName = hostName.ToUpperInvariant();
        return _logicalPathnameTranslations.TryGetValue(hostName, out var val) ? val : Nil.Instance;
    }

    public static LispObject SetLogicalPathnameTranslations(LispObject host, LispObject translations)
    {
        var hostName = host is LispString s ? s.Value : host is LispVector v && v.IsCharVector ? v.ToCharString() : host.ToString();
        hostName = hostName.ToUpperInvariant();
        _logicalPathnameTranslations[hostName] = translations;
        return translations;
    }

    /// <summary>
    /// Check if a string looks like a logical pathname (contains ":" with all-uppercase host before it).
    /// </summary>
    public static bool IsLogicalPathnameString(string s)
    {
        int colonPos = s.IndexOf(':');
        if (colonPos <= 0) return false;
        // Drive letters like "C:" are not logical pathnames (single char)
        if (colonPos == 1) return false;
        string host = s[..colonPos];
        // Check if host name has translations registered
        return _logicalPathnameTranslations.ContainsKey(host.ToUpperInvariant());
    }

    public static LispObject LogicalPathname(LispObject thing)
    {
        if (thing is LispLogicalPathname) return thing;
        if (thing is LispString s)
        {
            // CLHS: string must be a valid logical pathname namestring (must have a host)
            int colonPos = s.Value.IndexOf(':');
            if (colonPos <= 0)
                throw new LispErrorException(new LispTypeError(
                    "LOGICAL-PATHNAME: not a valid logical pathname namestring", thing,
                    Startup.Sym("LOGICAL-PATHNAME")));
            return LispLogicalPathname.FromLogicalString(s.Value);
        }
        if (thing is LispVector v && v.IsCharVector)
        {
            string str = v.ToCharString();
            int colonPos = str.IndexOf(':');
            if (colonPos <= 0)
                throw new LispErrorException(new LispTypeError(
                    "LOGICAL-PATHNAME: not a valid logical pathname namestring", thing,
                    Startup.Sym("LOGICAL-PATHNAME")));
            return LispLogicalPathname.FromLogicalString(str);
        }
        if (thing is LispFileStream fs)
        {
            // Check if stream was opened with a logical pathname
            if (fs.OriginalPathname is LispLogicalPathname lp) return lp;
            // CLHS: stream — the associated pathname must be a logical pathname
            var p = LispPathname.FromString(fs.FilePath);
            if (p is LispLogicalPathname lp2) return lp2;
            // Check if the file path looks like a logical pathname
            if (IsLogicalPathnameString(fs.FilePath))
                return LispLogicalPathname.FromLogicalString(fs.FilePath);
            throw new LispErrorException(new LispTypeError(
                "LOGICAL-PATHNAME: stream's pathname is not a logical pathname", thing,
                Startup.Sym("LOGICAL-PATHNAME")));
        }
        // Non-file streams (string streams, etc.) are not valid
        if (thing is LispStream)
            throw new LispErrorException(new LispTypeError(
                "LOGICAL-PATHNAME: not a valid argument", thing,
                Startup.Sym("LOGICAL-PATHNAME")));
        // Physical pathnames and other types are not valid
        throw new LispErrorException(new LispTypeError(
            "LOGICAL-PATHNAME: cannot convert to logical pathname", thing,
            Startup.Sym("LOGICAL-PATHNAME")));
    }

    public static LispObject TranslateLogicalPathname(LispObject pathname)
    {
        if (pathname is LispString s)
        {
            if (IsLogicalPathnameString(s.Value))
                pathname = LispLogicalPathname.FromLogicalString(s.Value);
            else
                return LispPathname.FromString(s.Value);
        }
        if (pathname is LispVector v && v.IsCharVector)
        {
            var str = v.ToCharString();
            if (IsLogicalPathnameString(str))
                pathname = LispLogicalPathname.FromLogicalString(str);
            else
                return LispPathname.FromString(str);
        }

        if (pathname is not LispLogicalPathname lp)
            return pathname is LispPathname ? pathname : LispPathname.FromString(pathname.ToString());

        // Get host translations
        var hostStr = lp.Host is LispString hs ? hs.Value : "";
        if (!_logicalPathnameTranslations.TryGetValue(hostStr.ToUpperInvariant(), out var translations) || translations is Nil)
        {
            throw new LispErrorException(new LispError($"No logical pathname translations for host \"{hostStr}\"") { ConditionTypeName = "FILE-ERROR" });
        }

        // Find matching translation rule
        var cur = translations;
        while (cur is Cons c)
        {
            if (c.Car is Cons rule)
            {
                // rule = (from-pattern to-pattern)
                var toPattern = (rule.Cdr is Cons cdr2) ? cdr2.Car : null;
                if (toPattern is LispPathname toPn)
                {
                    // Simple translation: replace wild components with actual components
                    return TranslateWithPattern(lp, toPn);
                }
            }
            cur = c.Cdr;
        }

        throw new LispErrorException(new LispError($"No matching translation for logical pathname \"{lp}\"") { ConditionTypeName = "FILE-ERROR" });
    }

    private static LispPathname TranslateWithPattern(LispLogicalPathname logical, LispPathname toPattern)
    {
        // Replace wild components in toPattern with actual components from logical pathname
        // For the common case: **;*.*.* -> /abs/path/sandbox/**/*.*
        // CLTEST:foo.txt (dir=nil, name=FOO, type=TXT)
        // -> /abs/path/sandbox/foo.txt

        LispObject? dir = toPattern.DirectoryComponent;
        LispObject? name = toPattern.NameComponent;
        LispObject? type = toPattern.TypeComponent;

        // Replace :wild-inferiors in to-directory with logical's directory components
        if (dir is Cons dirCons)
        {
            var resultDirs = new List<LispObject>();
            var cur = (LispObject)dirCons;
            while (cur is Cons dc)
            {
                if (dc.Car is Symbol ws && ws.Name == "WILD-INFERIORS")
                {
                    // Insert logical pathname's directory components here (without :absolute/:relative)
                    if (logical.DirectoryComponent is Cons logDir)
                    {
                        var logCur = (LispObject)logDir.Cdr; // skip :absolute/:relative
                        while (logCur is Cons lc)
                        {
                            resultDirs.Add(lc.Car);
                            logCur = lc.Cdr;
                        }
                    }
                }
                else
                {
                    resultDirs.Add(dc.Car);
                }
                cur = dc.Cdr;
            }
            dir = Runtime.List(resultDirs.ToArray());
        }

        // Replace :wild name/type with logical's components (lowercased for physical)
        if (name is Symbol ns && ns.Name == "WILD")
        {
            if (logical.NameComponent is LispString logName)
                name = new LispString(logName.Value.ToLowerInvariant());
            else
                name = logical.NameComponent;
        }
        if (type is Symbol ts && ts.Name == "WILD")
        {
            if (logical.TypeComponent is LispString logType)
                type = new LispString(logType.Value.ToLowerInvariant());
            else
                type = logical.TypeComponent;
        }

        return new LispPathname(toPattern.Host, toPattern.Device, dir, name, type, toPattern.Version);
    }

    public static LispObject SetVariableDocumentation(LispObject sym, LispObject doc)
    {
        if (sym is Symbol s)
            _docs[(s.Name, "VARIABLE")] = doc;
        return doc;
    }

    public static LispObject GetVariableDocumentation(LispObject sym)
    {
        if (sym is Symbol s && _docs.TryGetValue((s.Name, "VARIABLE"), out var doc))
            return doc;
        return Nil.Instance;
    }

    public static LispObject Documentation(LispObject obj, LispObject docType)
    {
        string? symName = obj is Symbol s ? s.Name : null;
        if (symName != null)
        {
            var key = (symName, docType.ToString().ToUpperInvariant());
            if (_docs.TryGetValue(key, out var doc)) return doc;
        }
        return Nil.Instance;
    }

    public static LispObject Min(LispObject a, LispObject b) =>
        Arithmetic.Compare(AsNumber(a), AsNumber(b)) <= 0 ? a : b;

    public static LispObject Max(LispObject a, LispObject b) =>
        Arithmetic.Compare(AsNumber(a), AsNumber(b)) >= 0 ? a : b;

    internal static void RegisterArithmeticBuiltins()
    {
        // FLOAT function
        Emitter.CilAssembler.RegisterFunction("FLOAT", new LispFunction(args => {
            Runtime.CheckArityMin("FLOAT", args, 1);
            Runtime.CheckArityMax("FLOAT", args, 2);
            var num = args[0];
            // CL spec: (float x) with no prototype returns x unchanged if already a float
            if (args.Length == 1) {
                if (num is SingleFloat || num is DoubleFloat) return num;
                // Not a float: convert to single-float by default
                return num switch {
                    Fixnum f => (LispObject)new SingleFloat((float)f.Value),
                    Bignum b => (LispObject)new SingleFloat((float)(double)b.Value),
                    Ratio r => (LispObject)new SingleFloat((float)Arithmetic.RatioToDouble(r)),
                    _ => throw new LispErrorException(new LispTypeError("FLOAT: not a number", num, Startup.Sym("REAL")))
                };
            }
            // With prototype: convert to match prototype type
            bool wantDouble = args[1] is DoubleFloat;
            return num switch {
                Fixnum f => wantDouble ? (LispObject)new DoubleFloat((double)f.Value)
                                       : new SingleFloat((float)f.Value),
                Bignum b => wantDouble ? (LispObject)new DoubleFloat((double)b.Value)
                                       : new SingleFloat((float)(double)b.Value),
                Ratio r => wantDouble ? (LispObject)new DoubleFloat(Arithmetic.RatioToDouble(r))
                                      : new SingleFloat((float)Arithmetic.RatioToDouble(r)),
                SingleFloat sf => wantDouble ? (LispObject)new DoubleFloat((double)sf.Value) : sf,
                DoubleFloat df => wantDouble ? df : (LispObject)new SingleFloat((float)df.Value),
                _ => throw new LispErrorException(new LispTypeError("FLOAT: not a number", num, Startup.Sym("REAL")))
            };
        }, "FLOAT"));

        // PARSE-INTEGER
        Emitter.CilAssembler.RegisterFunction("PARSE-INTEGER",
            new LispFunction(args => Runtime.ParseInteger(args)));

        // UPGRADED-COMPLEX-PART-TYPE
        Emitter.CilAssembler.RegisterFunction("UPGRADED-COMPLEX-PART-TYPE",
            new LispFunction(args => Runtime.UpgradedComplexPartType(args)));

        // COMPLEXP, COMPLEX, REALPART, IMAGPART
        Startup.RegisterUnary("COMPLEXP", Runtime.Complexp);
        Emitter.CilAssembler.RegisterFunction("COMPLEX", new LispFunction(Runtime.Complex, "COMPLEX", -1));
        Startup.RegisterUnary("REALPART", Runtime.Realpart);
        Startup.RegisterUnary("IMAGPART", Runtime.Imagpart);

        // NUMERATOR, DENOMINATOR, RATIONAL
        Startup.RegisterUnary("NUMERATOR", obj => obj switch {
            Ratio r => (LispObject)Bignum.MakeInteger(r.Numerator),
            Fixnum f => f,
            Bignum b => b,
            _ => throw new LispErrorException(new LispTypeError("NUMERATOR: not rational", obj))
        });
        Startup.RegisterUnary("DENOMINATOR", obj => obj switch {
            Ratio r => (LispObject)Bignum.MakeInteger(r.Denominator),
            Fixnum => Fixnum.Make(1),
            Bignum => Fixnum.Make(1),
            _ => throw new LispErrorException(new LispTypeError("DENOMINATOR: not rational", obj))
        });
        Startup.RegisterUnary("RATIONAL", obj => obj switch {
            Fixnum f => f,
            Bignum b => b,
            Ratio r => r,
            SingleFloat sf => Runtime.DoubleToRational((double)sf.Value),
            DoubleFloat df => Runtime.DoubleToRational(df.Value),
            _ => throw new LispErrorException(new LispTypeError("RATIONAL: not real", obj))
        });

        // SQRT
        Startup.RegisterUnary("SQRT", obj => {
            var n = Runtime.AsNumber(obj);
            if (n is LispComplex lc) return Arithmetic.FromSystemComplex(System.Numerics.Complex.Sqrt(Arithmetic.ToSystemComplex(lc)), n);
            double d = Arithmetic.ToDouble(n);
            if (d < 0) {
                var im = System.Math.Sqrt(-d);
                if (obj is DoubleFloat) return Arithmetic.MakeComplexPublic(new DoubleFloat(0.0), new DoubleFloat(im));
                return Arithmetic.MakeComplexPublic(new SingleFloat(0.0f), new SingleFloat((float)im));
            }
            return Startup.MakeFloat(System.Math.Sqrt(d), obj);
        });

        // LOG
        Emitter.CilAssembler.RegisterFunction("LOG", new LispFunction(args => {
            if (args.Length == 0 || args.Length > 2)
                throw new LispErrorException(new LispProgramError($"LOG: wrong number of arguments: {args.Length}"));
            var n = Runtime.AsNumber(args[0]);
            if (n is LispComplex lc) {
                var z = System.Numerics.Complex.Log(Arithmetic.ToSystemComplex(lc));
                if (args.Length > 1) {
                    var n2 = Runtime.AsNumber(args[1]);
                    System.Numerics.Complex b2;
                    if (n2 is LispComplex lc2) b2 = Arithmetic.ToSystemComplex(lc2);
                    else b2 = new System.Numerics.Complex(Arithmetic.ToDouble(n2), 0);
                    z = z / System.Numerics.Complex.Log(b2);
                }
                return Arithmetic.FromSystemComplex(z, n);
            }
            double d = Arithmetic.ToDouble(n);
            if (d == 0.0)
                throw new LispErrorException(new LispError("LOG: division by zero") { ConditionTypeName = "DIVISION-BY-ZERO" });
            if (args.Length > 1) {
                var n2 = Runtime.AsNumber(args[1]);
                if (d < 0 && !double.IsNegativeInfinity(d)) {
                    var z = System.Numerics.Complex.Log(new System.Numerics.Complex(d, 0));
                    System.Numerics.Complex b2;
                    if (n2 is LispComplex lc2b) b2 = Arithmetic.ToSystemComplex(lc2b);
                    else b2 = new System.Numerics.Complex(Arithmetic.ToDouble(n2), 0);
                    return Arithmetic.FromSystemComplex(z / System.Numerics.Complex.Log(b2), n);
                }
                double b = Arithmetic.ToDouble(n2);
                return Startup.MakeFloat(System.Math.Log(d, b), args[0]);
            }
            if (d < 0 && !double.IsNegativeInfinity(d)) {
                var z = new System.Numerics.Complex(d, 0);
                return Arithmetic.FromSystemComplex(System.Numerics.Complex.Log(z), n);
            }
            return Startup.MakeFloat(System.Math.Log(d), args[0]);
        }, "LOG", -1));

        // EXP
        Startup.RegisterUnary("EXP", obj => {
            var n = Runtime.AsNumber(obj);
            if (n is LispComplex lc) return Arithmetic.FromSystemComplex(System.Numerics.Complex.Exp(Arithmetic.ToSystemComplex(lc)), n);
            double d = Arithmetic.ToDouble(n);
            double result = System.Math.Exp(d);
            if (double.IsInfinity(result) && !double.IsInfinity(d))
                throw new LispErrorException(new LispError("EXP: floating-point overflow") { ConditionTypeName = "FLOATING-POINT-OVERFLOW" });
            if (result == 0.0 && d != 0.0)
                throw new LispErrorException(new LispError("EXP: floating-point underflow") { ConditionTypeName = "FLOATING-POINT-UNDERFLOW" });
            // Single-float overflow/underflow: double result is fine but float cast overflows
            if (n is SingleFloat && !double.IsInfinity(result) && !double.IsNaN(result)) {
                float sf = (float)result;
                if (float.IsInfinity(sf))
                    throw new LispErrorException(new LispError("EXP: floating-point overflow") { ConditionTypeName = "FLOATING-POINT-OVERFLOW" });
                if (sf == 0.0f && result != 0.0)
                    throw new LispErrorException(new LispError("EXP: floating-point underflow") { ConditionTypeName = "FLOATING-POINT-UNDERFLOW" });
            }
            return Startup.MakeFloat(result, obj);
        });

        // Trig functions: SIN, COS, TAN, ASIN, ACOS
        Startup.RegisterUnary("SIN", obj => {
            var n = Runtime.AsNumber(obj);
            if (n is LispComplex lc) return Arithmetic.FromSystemComplex(System.Numerics.Complex.Sin(Arithmetic.ToSystemComplex(lc)), n);
            return Startup.MakeFloat(System.Math.Sin(Arithmetic.ToDouble(n)), obj);
        });
        Startup.RegisterUnary("COS", obj => {
            var n = Runtime.AsNumber(obj);
            if (n is LispComplex lc) return Arithmetic.FromSystemComplex(System.Numerics.Complex.Cos(Arithmetic.ToSystemComplex(lc)), n);
            return Startup.MakeFloat(System.Math.Cos(Arithmetic.ToDouble(n)), obj);
        });
        Startup.RegisterUnary("TAN", obj => {
            var n = Runtime.AsNumber(obj);
            if (n is LispComplex lc) return Arithmetic.FromSystemComplex(System.Numerics.Complex.Tan(Arithmetic.ToSystemComplex(lc)), n);
            return Startup.MakeFloat(System.Math.Tan(Arithmetic.ToDouble(n)), obj);
        });
        Startup.RegisterUnary("ASIN", obj => {
            var n = Runtime.AsNumber(obj);
            if (n is LispComplex lc) return Arithmetic.FromSystemComplex(System.Numerics.Complex.Asin(Arithmetic.ToSystemComplex(lc)), n);
            return Startup.MakeFloat(System.Math.Asin(Arithmetic.ToDouble(n)), obj);
        });
        Startup.RegisterUnary("ACOS", obj => {
            var n = Runtime.AsNumber(obj);
            if (n is LispComplex lc) return Arithmetic.FromSystemComplex(System.Numerics.Complex.Acos(Arithmetic.ToSystemComplex(lc)), n);
            return Startup.MakeFloat(System.Math.Acos(Arithmetic.ToDouble(n)), obj);
        });

        // ATAN (1 or 2 args)
        Emitter.CilAssembler.RegisterFunction("ATAN", new LispFunction(args => {
            if (args.Length < 1 || args.Length > 2)
                throw new LispErrorException(new LispProgramError($"ATAN: expected 1 or 2 arguments, got {args.Length}"));
            var n = Runtime.AsNumber(args[0]);
            if (n is LispComplex lc) {
                if (args.Length > 1) throw new LispErrorException(new LispProgramError("ATAN: complex argument not allowed with two arguments"));
                return Arithmetic.FromSystemComplex(System.Numerics.Complex.Atan(Arithmetic.ToSystemComplex(lc)), n);
            }
            double y = Arithmetic.ToDouble(n);
            if (args.Length > 1) {
                double x = Arithmetic.ToDouble(Runtime.AsNumber(args[1]));
                // 2-arg ATAN: result type follows the widest float type of the two args
                bool isDouble = args[0] is DoubleFloat || args[1] is DoubleFloat;
                return isDouble ? new DoubleFloat(System.Math.Atan2(y, x)) : (LispObject)new SingleFloat((float)System.Math.Atan2(y, x));
            }
            return Startup.MakeFloat(System.Math.Atan(y), args[0]);
        }, "ATAN", -1));

        // Hyperbolic: SINH, COSH, TANH
        Startup.RegisterUnary("SINH", obj => {
            var n = Runtime.AsNumber(obj);
            if (n is LispComplex lc) return Arithmetic.FromSystemComplex(System.Numerics.Complex.Sinh(Arithmetic.ToSystemComplex(lc)), n);
            return Startup.MakeFloat(System.Math.Sinh(Arithmetic.ToDouble(n)), obj);
        });
        Startup.RegisterUnary("COSH", obj => {
            var n = Runtime.AsNumber(obj);
            if (n is LispComplex lc) return Arithmetic.FromSystemComplex(System.Numerics.Complex.Cosh(Arithmetic.ToSystemComplex(lc)), n);
            return Startup.MakeFloat(System.Math.Cosh(Arithmetic.ToDouble(n)), obj);
        });
        Startup.RegisterUnary("TANH", obj => {
            var n = Runtime.AsNumber(obj);
            if (n is LispComplex lc) return Arithmetic.FromSystemComplex(System.Numerics.Complex.Tanh(Arithmetic.ToSystemComplex(lc)), n);
            return Startup.MakeFloat(System.Math.Tanh(Arithmetic.ToDouble(n)), obj);
        });

        // ASINH, ACOSH, ATANH
        Startup.RegisterUnary("ASINH", obj => {
            var n = Runtime.AsNumber(obj);
            if (n is LispComplex lc) { var z = Arithmetic.ToSystemComplex(lc); return Arithmetic.FromSystemComplex(System.Numerics.Complex.Log(z + System.Numerics.Complex.Sqrt(z * z + 1)), n); }
            return Startup.MakeFloat(System.Math.Asinh(Arithmetic.ToDouble(n)), obj);
        });
        Startup.RegisterUnary("ACOSH", obj => {
            var n = Runtime.AsNumber(obj);
            if (n is LispComplex lc) { var z = Arithmetic.ToSystemComplex(lc); return Arithmetic.FromSystemComplex(System.Numerics.Complex.Log(z + System.Numerics.Complex.Sqrt(z * z - 1)), n); }
            double d = Arithmetic.ToDouble(n);
            if (d < 1.0) {
                // acosh of x < 1 produces complex result
                var z = new System.Numerics.Complex(d, 0);
                return Arithmetic.FromSystemComplex(System.Numerics.Complex.Log(z + System.Numerics.Complex.Sqrt(z * z - 1)), n);
            }
            return Startup.MakeFloat(System.Math.Acosh(d), obj);
        });
        Startup.RegisterUnary("ATANH", obj => {
            var n = Runtime.AsNumber(obj);
            if (n is LispComplex lc) { var z = Arithmetic.ToSystemComplex(lc); return Arithmetic.FromSystemComplex(0.5 * System.Numerics.Complex.Log((1 + z) / (1 - z)), n); }
            double d = Arithmetic.ToDouble(n);
            if (System.Math.Abs(d) > 1.0) {
                // atanh of |x| > 1 produces complex result
                var z = new System.Numerics.Complex(d, 0);
                return Arithmetic.FromSystemComplex(0.5 * System.Numerics.Complex.Log((1 + z) / (1 - z)), n);
            }
            return Startup.MakeFloat(System.Math.Atanh(d), obj);
        });

        // CIS: (cis x) => (complex (cos x) (sin x)), only accepts real
        Startup.RegisterUnary("CIS", obj => {
            double d = Arithmetic.ToDouble(Runtime.AsNumber(obj));
            if (obj is DoubleFloat)
                return Arithmetic.MakeComplexPublic(new DoubleFloat(System.Math.Cos(d)), new DoubleFloat(System.Math.Sin(d)));
            return Arithmetic.MakeComplexPublic(new SingleFloat((float)System.Math.Cos(d)), new SingleFloat((float)System.Math.Sin(d)));
        });

        // ISQRT
        Startup.RegisterUnary("ISQRT", obj => {
            var num = Runtime.AsNumber(obj);
            System.Numerics.BigInteger n;
            if (num is Fixnum f) n = f.Value;
            else if (num is Bignum b) n = b.Value;
            else throw new LispErrorException(new LispTypeError("ISQRT: argument is not a non-negative integer", obj, Startup.Sym("INTEGER")));
            if (n < 0) throw new LispErrorException(new LispTypeError("ISQRT: argument must be a non-negative integer", obj));
            if (n == 0) return Fixnum.Make(0);
            // Newton's method for integer square root
            // Initial guess: 2^((bitLength+1)/2) which is >= sqrt(n)
            int bitLen = (int)n.GetBitLength();
            System.Numerics.BigInteger x = System.Numerics.BigInteger.One << ((bitLen + 1) / 2);
            while (true)
            {
                System.Numerics.BigInteger x1 = (x + n / x) / 2;
                if (x1 >= x) break;
                x = x1;
            }
            return (LispObject)Bignum.MakeInteger(x);
        });

        // PHASE
        Startup.RegisterUnary("PHASE", obj => {
            var n = Runtime.AsNumber(obj);
            if (n is LispComplex c) {
                double angle = System.Math.Atan2(Arithmetic.ToDouble(c.Imaginary), Arithmetic.ToDouble(c.Real));
                // Complex phase type follows the parts' type
                if (c.Real is DoubleFloat || c.Imaginary is DoubleFloat) return new DoubleFloat(angle);
                return new SingleFloat((float)angle);
            }
            double val = Arithmetic.ToDouble(n) >= 0 ? 0.0 : System.Math.PI;
            return Startup.MakeFloat(val, obj);
        });

        // ABS, SIGNUM
        Startup.RegisterUnary("ABS", Runtime.Abs);
        Startup.RegisterUnary("SIGNUM", obj => {
            var n = Runtime.AsNumber(obj);
            if (n is LispComplex lc) {
                double r = Arithmetic.ToDouble(lc.Real);
                double i = Arithmetic.ToDouble(lc.Imaginary);
                if (r == 0.0 && i == 0.0) return obj;
                double mag = System.Math.Sqrt(r*r + i*i);
                if (lc.Real is DoubleFloat || lc.Imaginary is DoubleFloat)
                    return Arithmetic.MakeComplexPublic(new DoubleFloat(r/mag), new DoubleFloat(i/mag));
                return Arithmetic.MakeComplexPublic(new SingleFloat((float)(r/mag)), new SingleFloat((float)(i/mag)));
            }
            if (n is Fixnum fx) {
                long v = fx.Value;
                if (v == 0) return obj;
                if (v == long.MinValue) return Fixnum.Make(-1);
                return Fixnum.Make(v > 0 ? 1 : -1);
            }
            if (n is SingleFloat sf) {
                float fv = sf.Value;
                if (fv == 0.0f) return obj;
                return new SingleFloat(fv > 0 ? 1.0f : -1.0f);
            }
            if (n is DoubleFloat df) {
                double dv = df.Value;
                if (dv == 0.0) return obj;
                return new DoubleFloat(dv > 0 ? 1.0 : -1.0);
            }
            if (Arithmetic.IsNumericEqual(n, Fixnum.Make(0))) return obj;
            return Arithmetic.Divide(n, (Number)Runtime.Abs(obj));
        });

        // Arithmetic: /, /=, MOD, REM
        Emitter.CilAssembler.RegisterFunction("/",
            new LispFunction(args => Runtime.DivideN(args), "/", -1));
        Emitter.CilAssembler.RegisterFunction("/=",
            new LispFunction(args => Runtime.NumNotEqualN(args), "/=", -1));
        Startup.RegisterBinary("MOD", Runtime.Mod);
        Startup.RegisterBinary("REM", Runtime.Rem);

        // INTEGER-LENGTH: number of bits needed to represent integer
        Startup.RegisterUnary("INTEGER-LENGTH", obj => {
            System.Numerics.BigInteger n = obj switch {
                Fixnum f => f.Value,
                Bignum b => b.Value,
                _ => throw new LispErrorException(new LispTypeError("INTEGER-LENGTH: not an integer", obj, Startup.Sym("INTEGER")))
            };
            if (n.IsZero) return Fixnum.Make(0);
            if (n < 0) n = ~n;
            return Fixnum.Make((int)n.GetBitLength());
        });

        // Floor/Ceiling/Truncate/Round: (func number &optional divisor) → (values quotient remainder)
        foreach (var (fname, fop) in new (string, Func<Number, Number, (Number, Number)>)[] {
            ("FLOOR",    Arithmetic.Floor),
            ("TRUNCATE", Arithmetic.Truncate),
        })
        {
            var capturedOp = fop;
            var capturedName = fname;
            Emitter.CilAssembler.RegisterFunction(capturedName, new LispFunction(args => {
                Runtime.CheckArityMin(capturedName, args, 1);
                Runtime.CheckArityMax(capturedName, args, 2);
                var a = Runtime.AsNumber(args[0]);
                var b = args.Length > 1 ? Runtime.AsNumber(args[1]) : Fixnum.Make(1);
                var (q, r) = capturedOp(a, b);
                return MultipleValues.Values(q, r);
            }, capturedName, -1));
        }
        Emitter.CilAssembler.RegisterFunction("CEILING", new LispFunction(args => {
            Runtime.CheckArityMin("CEILING", args, 1);
            Runtime.CheckArityMax("CEILING", args, 2);
            var a = Runtime.AsNumber(args[0]);
            var b = args.Length > 1 ? Runtime.AsNumber(args[1]) : Fixnum.Make(1);
            var (q, r) = Arithmetic.Ceiling(a, b);
            return MultipleValues.Values(q, r);
        }, "CEILING", -1));
        Emitter.CilAssembler.RegisterFunction("ROUND", new LispFunction(args => {
            Runtime.CheckArityMin("ROUND", args, 1);
            Runtime.CheckArityMax("ROUND", args, 2);
            var a = Runtime.AsNumber(args[0]);
            var b = args.Length > 1 ? Runtime.AsNumber(args[1]) : Fixnum.Make(1);
            var (q, r) = Arithmetic.Round(a, b);
            return MultipleValues.Values(q, r);
        }, "ROUND", -1));
        // FFloor/FCeiling/FTruncate/FRound: float versions
        foreach (var (fname, fop) in new (string, Func<Number, Number, (Number, Number)>)[] {
            ("FFLOOR",    Arithmetic.FFloor),
            ("FTRUNCATE", Arithmetic.FTruncate),
            ("FCEILING",  Arithmetic.FCeiling),
            ("FROUND",    Arithmetic.FRound),
        })
        {
            var capturedOp = fop;
            var capturedName = fname;
            Emitter.CilAssembler.RegisterFunction(capturedName, new LispFunction(args => {
                Runtime.CheckArityMin(capturedName, args, 1);
                Runtime.CheckArityMax(capturedName, args, 2);
                var a = Runtime.AsNumber(args[0]);
                var b = args.Length > 1 ? Runtime.AsNumber(args[1]) : Fixnum.Make(1);
                var (q, r) = capturedOp(a, b);
                return MultipleValues.Values(q, r);
            }, capturedName, -1));
        }

        // RANDOM as callable function (for funcall/apply)
        Emitter.CilAssembler.RegisterFunction("RANDOM", new LispFunction(args => {
            if (args.Length == 0) throw new LispErrorException(new LispError("RANDOM: too few args"));
            return args.Length >= 2 ? Runtime.Random2(args[0], args[1]) : Runtime.Random(args[0]);
        }));

        // LOGNOT, ASH, LOGBITP
        Startup.RegisterUnary("LOGNOT", Runtime.Lognot);
        Startup.RegisterBinary("ASH", Runtime.Ash);
        Startup.RegisterBinary("LOGBITP", Runtime.Logbitp);
        // BOOLE function
        static long booleToLong(LispObject x) => x switch {
            Fixnum f => f.Value, Bignum bg => (long)bg.Value, _ => throw new LispErrorException(new LispTypeError("BOOLE", x)) };
        Emitter.CilAssembler.RegisterFunction("BOOLE", new LispFunction(args => {
            Runtime.CheckArityExact("BOOLE", args, 3);
            long op = booleToLong(args[0]);
            long a = booleToLong(args[1]), b = booleToLong(args[2]);
            long result = op switch {
                0 => 0L, 1 => -1L, 2 => a, 3 => b, 4 => ~a, 5 => ~b,
                6 => a & b, 7 => a | b, 8 => a ^ b, 9 => ~(a ^ b),
                10 => ~(a & b), 11 => ~(a | b), 12 => ~a & b, 13 => a & ~b,
                14 => ~a | b, 15 => a | ~b,
                _ => throw new LispErrorException(new LispProgramError($"BOOLE: invalid operation {op}"))
            };
            return Fixnum.Make(result);
        }));
        Emitter.CilAssembler.RegisterFunction("LOGIOR", new LispFunction(Runtime.Logior));
        Emitter.CilAssembler.RegisterFunction("LOGAND", new LispFunction(Runtime.Logand));
        Emitter.CilAssembler.RegisterFunction("LOGXOR", new LispFunction(Runtime.Logxor));
        Startup.RegisterBinary("LOGTEST", (a, b) => {
            var ai = Runtime.GetBigInt(a);
            var bi = Runtime.GetBigInt(b);
            return (ai & bi) != System.Numerics.BigInteger.Zero ? (LispObject)T.Instance : Nil.Instance;
        });

        // *random-state* and make-random-state
        var defaultRandomState = new LispRandomState();
        var randomStateSym = Startup.InternExport("*RANDOM-STATE*");
        randomStateSym.IsSpecial = true;
        randomStateSym.Value = defaultRandomState;
        Emitter.CilAssembler.RegisterFunction("MAKE-RANDOM-STATE",
            new LispFunction(args => {
                if (args.Length > 1)
                    throw new LispErrorException(new LispProgramError("MAKE-RANDOM-STATE: too many arguments"));
                var currentRS = (LispRandomState)DynamicBindings.Get(randomStateSym);
                if (args.Length == 0 || args[0] is Nil)
                    return new LispRandomState(currentRS); // copy current *random-state*
                if (args[0] is T)
                    return new LispRandomState(); // fresh random seed
                if (args[0] is LispRandomState rs)
                    return new LispRandomState(rs); // copy given state
                throw new LispErrorException(new LispTypeError("MAKE-RANDOM-STATE: invalid argument", args[0]));
            }, "MAKE-RANDOM-STATE", -1));
        // Internal function for readable printing of random-state via #.(make-random-state-from-seeds s0 s1)
        Emitter.CilAssembler.RegisterFunction("MAKE-RANDOM-STATE-FROM-SEEDS",
            new LispFunction(args => {
                if (args.Length != 2)
                    throw new LispErrorException(new LispProgramError("MAKE-RANDOM-STATE-FROM-SEEDS: requires exactly 2 arguments"));
                ulong s0 = Runtime.ToUlong(args[0], "MAKE-RANDOM-STATE-FROM-SEEDS");
                ulong s1 = Runtime.ToUlong(args[1], "MAKE-RANDOM-STATE-FROM-SEEDS");
                return LispRandomState.FromSeeds(s0, s1);
            }, "MAKE-RANDOM-STATE-FROM-SEEDS", -1));
        Startup.Sym("MAKE-RANDOM-STATE-FROM-SEEDS").Function =
            (LispFunction)Emitter.CilAssembler.GetFunction("MAKE-RANDOM-STATE-FROM-SEEDS");
        Startup.RegisterUnary("RANDOM-STATE-P", obj => obj is LispRandomState ? (LispObject)T.Instance : Nil.Instance);

        // EXPT: exponentiation
        Startup.RegisterBinary("EXPT", Runtime.Expt);

        // LCM: n-ary least common multiple
        Emitter.CilAssembler.RegisterFunction("LCM", new LispFunction(args => {
            if (args.Length == 0) return Fixnum.Make(1);
            // Type-check all arguments are integers
            foreach (var arg in args)
                if (arg is not Fixnum && arg is not Bignum)
                    throw new LispErrorException(new LispTypeError("LCM: argument is not an integer", arg, Startup.Sym("INTEGER")));
            if (args.Length == 1) return Runtime.Abs(args[0]);
            LispObject result = args[0];
            for (int i = 1; i < args.Length; i++)
                result = Runtime.Lcm(result, args[i]);
            return result;
        }, "LCM"));

        // GCD: n-ary greatest common divisor
        Emitter.CilAssembler.RegisterFunction("GCD", new LispFunction(args => {
            if (args.Length == 0) return Fixnum.Make(0);
            // Type-check all arguments are integers
            foreach (var arg in args)
                if (arg is not Fixnum && arg is not Bignum)
                    throw new LispErrorException(new LispTypeError("GCD: argument is not an integer", arg, Startup.Sym("INTEGER")));
            if (args.Length == 1) return Runtime.Abs(args[0]);
            LispObject result = args[0];
            for (int i = 1; i < args.Length; i++)
                result = Runtime.Gcd(result, args[i]);
            return result;
        }, "GCD"));

        // MIN: n-ary minimum (1+ args required)
        Emitter.CilAssembler.RegisterFunction("MIN", new LispFunction(args => {
            if (args.Length == 0)
                throw new LispErrorException(new LispProgramError("MIN: too few arguments: 0 (expected at least 1)"));
            LispObject result = args[0];
            Runtime.AsNumber(result); // type check
            for (int i = 1; i < args.Length; i++)
                result = Runtime.Min(result, args[i]);
            return result;
        }, "MIN", -1));

        // MAX: n-ary maximum (1+ args required)
        Emitter.CilAssembler.RegisterFunction("MAX", new LispFunction(args => {
            if (args.Length == 0)
                throw new LispErrorException(new LispProgramError("MAX: too few arguments: 0 (expected at least 1)"));
            LispObject result = args[0];
            Runtime.AsNumber(result); // type check
            for (int i = 1; i < args.Length; i++)
                result = Runtime.Max(result, args[i]);
            return result;
        }, "MAX", -1));

        // INTEGER-DECODE-FLOAT: (integer-decode-float f) => (values significand exponent sign)
        Emitter.CilAssembler.RegisterFunction("INTEGER-DECODE-FLOAT",
            new LispFunction(args => {
                double d = Runtime.ObjToDouble(args[0]);
                long bits = BitConverter.DoubleToInt64Bits(d);
                int sign = (bits < 0) ? -1 : 1;
                int exponent = (int)((bits >> 52) & 0x7FFL);
                long mantissa = bits & 0xFFFFFFFFFFFFFL;
                if (exponent == 0) { // denormalized
                    exponent = -1074;
                } else {
                    mantissa |= (1L << 52);
                    exponent = exponent - 1023 - 52;
                }
                return MultipleValues.Values(new Fixnum(mantissa), new Fixnum(exponent), new Fixnum(sign));
            }, "INTEGER-DECODE-FLOAT", 1));

        // DECODE-FLOAT: (decode-float f) => (values significand exponent sign)
        Emitter.CilAssembler.RegisterFunction("DECODE-FLOAT",
            new LispFunction(args => {
                double d = Runtime.ObjToDouble(args[0]);
                double sign = d < 0 ? -1.0 : 1.0;
                d = Math.Abs(d);
                if (d == 0.0) {
                    return MultipleValues.Values(new DoubleFloat(0.0), new Fixnum(0), new DoubleFloat(sign));
                }
                int exponent = (int)Math.Floor(Math.Log2(d)) + 1;
                double significand = d / Math.Pow(2.0, exponent);
                return MultipleValues.Values(new DoubleFloat(significand), new Fixnum(exponent), new DoubleFloat(sign));
            }, "DECODE-FLOAT", 1));

        // FLOAT-RADIX: always 2 for IEEE floats
        Emitter.CilAssembler.RegisterFunction("FLOAT-RADIX",
            new LispFunction(args => new Fixnum(2), "FLOAT-RADIX", 1));

        // FLOAT-DIGITS: 53 for double-float
        Emitter.CilAssembler.RegisterFunction("FLOAT-DIGITS",
            new LispFunction(args => new Fixnum(53), "FLOAT-DIGITS", 1));

        // FLOAT-PRECISION
        Emitter.CilAssembler.RegisterFunction("FLOAT-PRECISION",
            new LispFunction(args => {
                double d = Runtime.ObjToDouble(args[0]);
                return new Fixnum(d == 0.0 ? 0 : 53);
            }, "FLOAT-PRECISION", 1));

        // FLOAT-SIGN: (float-sign f1 &optional f2) => sign of f1 * magnitude of f2 (default 1.0)
        Emitter.CilAssembler.RegisterFunction("FLOAT-SIGN",
            new LispFunction(args => {
                double f1 = Runtime.ObjToDouble(args[0]);
                double f2 = args.Length > 1 ? Runtime.ObjToDouble(args[1]) : 1.0;
                double sign = (f1 < 0 || (f1 == 0.0 && double.IsNegativeInfinity(1.0/f1))) ? -1.0 : 1.0;
                return new DoubleFloat(sign * Math.Abs(f2));
            }, "FLOAT-SIGN", -1));
    }


}
