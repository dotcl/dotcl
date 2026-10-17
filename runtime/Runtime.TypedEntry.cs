using System.Runtime.CompilerServices;

namespace DotCL;

// The conversions at the boundary of a typed entry: a global function whose
// DECLAIM FTYPE gives every argument and the result a numeric type gets a body
// method taking and returning raw long / double, and its ordinary (boxed) entry
// becomes a shell that converts the arguments, calls it and boxes the result.
//
// From (safety 1) up the declaration is an assertion: an argument or a result
// that is not of the declared type signals a TYPE-ERROR naming the value and the
// type. At (safety 0) it is trusted, and the shell only unboxes.
public static partial class Runtime
{
    static LispErrorException TypedEntryTypeError(LispObject datum, LispObject expected) =>
        new LispErrorException(new LispTypeError(
            $"The value {FormatObject(datum, true)} is not of type {FormatObject(expected, true)}",
            datum, expected));

    static LispObject IntegerRangeType(long lo, long hi) =>
        new Cons(Startup.Sym("INTEGER"),
                 new Cons(Fixnum.Make(lo), new Cons(Fixnum.Make(hi), Nil.Instance)));

    /// <summary>A FIXNUM argument of a typed entry, checked.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long TypedArgL(LispObject v) =>
        v is Fixnum f ? f.Value : throw TypedEntryTypeError(v, Startup.Sym("FIXNUM"));

    /// <summary>An argument declared an integer type within [LO, HI], checked.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long TypedArgLR(LispObject v, long lo, long hi)
    {
        if (v is Fixnum f && f.Value >= lo && f.Value <= hi) return f.Value;
        throw TypedEntryTypeError(v, IntegerRangeType(lo, hi));
    }

    /// <summary>A DOUBLE-FLOAT argument of a typed entry, checked.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double TypedArgD(LispObject v) =>
        v is DoubleFloat d ? d.Value : throw TypedEntryTypeError(v, Startup.Sym("DOUBLE-FLOAT"));

    /// <summary>The value a typed entry returns, when its body produced it as an
    /// object: the primary value, checked to be a FIXNUM.</summary>
    public static long TypedRetL(LispObject v)
    {
        if (v is Fixnum f) return f.Value;
        if (v is MvReturn mv && mv.PrimaryValue is Fixnum pf) return pf.Value;
        throw TypedEntryTypeError(v is MvReturn m ? m.PrimaryValue : v, Startup.Sym("FIXNUM"));
    }

    /// <summary>TypedRetL for a DOUBLE-FLOAT result.</summary>
    public static double TypedRetD(LispObject v)
    {
        if (v is DoubleFloat d) return d.Value;
        if (v is MvReturn mv && mv.PrimaryValue is DoubleFloat pd) return pd.Value;
        throw TypedEntryTypeError(v is MvReturn m ? m.PrimaryValue : v, Startup.Sym("DOUBLE-FLOAT"));
    }

    /// <summary>A raw integer checked to lie in [LO, HI], the declared type of a
    /// typed entry's argument or result.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long CheckLongRange(long v, long lo, long hi) =>
        v >= lo && v <= hi ? v : throw TypedEntryTypeError(Fixnum.Make(v), IntegerRangeType(lo, hi));

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void CheckArgFixnum(LispObject v)
    {
        if (v is not Fixnum) throw TypedEntryTypeError(v, Startup.Sym("FIXNUM"));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void CheckArgRange(LispObject v, long lo, long hi)
    {
        if (!(v is Fixnum f && f.Value >= lo && f.Value <= hi))
            throw TypedEntryTypeError(v, IntegerRangeType(lo, hi));
    }

    public static void CheckArgU64(LispObject v)
    {
        if (!IsU64(v)) throw TypedEntryTypeError(v, SizedByte("UNSIGNED-BYTE", 64));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void CheckArgDouble(LispObject v)
    {
        if (v is not DoubleFloat) throw TypedEntryTypeError(v, Startup.Sym("DOUBLE-FLOAT"));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void CheckArgSingle(LispObject v)
    {
        if (v is not SingleFloat) throw TypedEntryTypeError(v, Startup.Sym("SINGLE-FLOAT"));
    }

    /// <summary>(SINGLE-FLOAT LO HI) with inclusive bounds; anything else
    /// goes to CheckArgType with SPEC, which decides and reports.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void CheckArgSingleRange(LispObject v, float lo, float hi, LispObject spec)
    {
        if (v is SingleFloat sf && sf.Value >= lo && sf.Value <= hi) return;
        CheckArgType(v, spec);
    }

    /// <summary>The DOUBLE-FLOAT counterpart of CheckArgSingleRange.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void CheckArgDoubleRange(LispObject v, double lo, double hi, LispObject spec)
    {
        if (v is DoubleFloat df && df.Value >= lo && df.Value <= hi) return;
        CheckArgType(v, spec);
    }

    static readonly ConditionalWeakTable<LispObject, TypeTest> ArgTypeTests = new();

    public static void CheckArgType(LispObject v, LispObject spec)
    {
        TypeTest? test = spec is Symbol sym ? SymbolTypeTest(sym)
            : ArgTypeTests.GetValue(spec, static s => BuildTypeTest(s));
        bool ok;
        if (test != null)
        {
            try { ok = test.Test(v); }
            catch (LispErrorException) { return; }
        }
        else
        {
            try { ok = Typep(v, spec) is not Nil; }
            catch (LispErrorException) { return; }
        }
        if (!ok) throw TypedEntryTypeError(v, spec);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void CheckArgList(LispObject v)
    {
        if (v is not (Cons or Nil)) throw TypedEntryTypeError(v, Startup.Sym("LIST"));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void CheckArgCons(LispObject v)
    {
        if (v is not Cons) throw TypedEntryTypeError(v, Startup.Sym("CONS"));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void CheckArgSymbol(LispObject v)
    {
        if (v is not (Symbol or Nil or T)) throw TypedEntryTypeError(v, Startup.Sym("SYMBOL"));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void CheckArgFunction(LispObject v)
    {
        if (v is not LispFunction) throw TypedEntryTypeError(v, Startup.Sym("FUNCTION"));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void CheckArgCharacter(LispObject v)
    {
        if (v is not LispChar) throw TypedEntryTypeError(v, Startup.Sym("CHARACTER"));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void CheckArgInteger(LispObject v)
    {
        if (v is not (Fixnum or Bignum)) throw TypedEntryTypeError(v, Startup.Sym("INTEGER"));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void CheckArgString(LispObject v, LispObject spec)
    {
        if (v is not LispString) CheckArgType(v, spec);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void CheckArgStructSym(LispObject v, LispObject sym)
    {
        if (!(v is LispStruct s && (ReferenceEquals(s.TypeName, sym) || IsStructSubclass(s.TypeName, sym))))
            CheckArgType(v, sym);
    }

    static bool IsStructSubclass(Symbol name, LispObject sym) =>
        FindClassOrNil(name) is LispClass own && own.IsStructureClass
        && FindClassOrNil(sym) is LispClass cls && cls.IsStructureClass
        && ReferenceEquals(cls.Name, sym) && ClassIncludes(own, cls);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void CheckArgSimpleVector(LispObject v, LispObject spec, string et)
    {
        if (!(v is LispVector lv && lv._dimensions == null && lv.IsSimple
              && lv.ElementTypeName == et))
            CheckArgType(v, spec);
    }
}
