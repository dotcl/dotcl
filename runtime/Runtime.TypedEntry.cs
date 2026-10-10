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
}
