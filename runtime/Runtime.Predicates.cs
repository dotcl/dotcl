namespace DotCL;

public static partial class Runtime
{
    /// <summary>
    /// Return the primary value of an object: unwrap MvReturn to its first value,
    /// or pass through any non-MV object. Single-value consumers call this before
    /// type-checking so MvReturn can't leak into functions that want a specific type.
    /// </summary>
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static LispObject Primary(LispObject obj)
    {
        if (obj is MvReturn mv)
            return mv.Values.Length > 0 ? mv.Values[0] : Nil.Instance;
        return obj;
    }

    // --- Equality ---

    /// <summary>
    /// Reference-level eq test that handles T.Instance/T_SYM and Nil.Instance/NIL_SYM equivalence.
    /// Use this instead of ReferenceEquals when implementing eq-equivalent comparisons.
    /// </summary>
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static bool IsEqRef(LispObject a, LispObject b)
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

    public static LispObject Eq(LispObject a, LispObject b)
    {
        return IsEqRef(a, b) ? T.Instance : Nil.Instance;
    }

    public static LispObject Eql(LispObject a, LispObject b)
    {
        a = Primary(a); b = Primary(b);
        if (ReferenceEquals(a, b)) return T.Instance;
        // T.Instance and T_SYM are the same symbol in CL
        if ((a is T && ReferenceEquals(b, Startup.T_SYM)) ||
            (b is T && ReferenceEquals(a, Startup.T_SYM)))
            return T.Instance;
        // Nil.Instance and NIL_SYM are the same symbol in CL
        if ((a is Nil && ReferenceEquals(b, Startup.NIL_SYM)) ||
            (b is Nil && ReferenceEquals(a, Startup.NIL_SYM)))
            return T.Instance;
        if (a is Fixnum fa && b is Fixnum fb && fa.Value == fb.Value) return T.Instance;
        if (a is LispChar ca && b is LispChar cb && ca.Value == cb.Value) return T.Instance;
        if (a is SingleFloat sa && b is SingleFloat sb && sa.Value == sb.Value) return T.Instance;
        if (a is DoubleFloat da && b is DoubleFloat db && da.Value == db.Value) return T.Instance;
        if (a is Bignum ba && b is Bignum bb && ba.Value == bb.Value) return T.Instance;
        if (a is Ratio ra && b is Ratio rb && ra.Numerator == rb.Numerator && ra.Denominator == rb.Denominator) return T.Instance;
        if (a is LispComplex xa && b is LispComplex xb)
            return IsTrueEql(xa.Real, xb.Real) && IsTrueEql(xa.Imaginary, xb.Imaginary)
                ? T.Instance : Nil.Instance;
        return Nil.Instance;
    }

    public static LispObject Equal(LispObject a, LispObject b)
    {
        a = Primary(a); b = Primary(b);
        if (IsTrueEql(a, b)) return T.Instance;
        // String comparison: LispString or char-vector — compare by content
        bool aIsStr = a is LispString || (a is LispVector av && av.IsCharVector);
        bool bIsStr = b is LispString || (b is LispVector bv && bv.IsCharVector);
        if (aIsStr && bIsStr)
        {
            string sa = a is LispString las ? las.Value : ((LispVector)a).ToCharString();
            string sb = b is LispString lbs ? lbs.Value : ((LispVector)b).ToCharString();
            return sa == sb ? T.Instance : Nil.Instance;
        }
        // Pathname: compare component-by-component per CLHS
        if (a is LispPathname pa && b is LispPathname pb)
        {
            return IsTruthy(Equal(pa.Host ?? Nil.Instance, pb.Host ?? Nil.Instance))
                && IsTruthy(Equal(pa.Device ?? Nil.Instance, pb.Device ?? Nil.Instance))
                && IsTruthy(Equal(pa.DirectoryComponent ?? Nil.Instance, pb.DirectoryComponent ?? Nil.Instance))
                && IsTruthy(Equal(pa.NameComponent ?? Nil.Instance, pb.NameComponent ?? Nil.Instance))
                && IsTruthy(Equal(pa.TypeComponent ?? Nil.Instance, pb.TypeComponent ?? Nil.Instance))
                && IsTruthy(Equal(pa.Version ?? Nil.Instance, pb.Version ?? Nil.Instance))
                ? T.Instance : Nil.Instance;
        }
        // Bit-vector: compare element-by-element
        if (a is LispVector bva && bva.IsBitVector && b is LispVector bvb && bvb.IsBitVector)
        {
            if (bva.Length != bvb.Length) return Nil.Instance;
            for (int i = 0; i < bva.Length; i++)
                if (!IsTrueEql(bva.GetElement(i), bvb.GetElement(i))) return Nil.Instance;
            return T.Instance;
        }
        // Iteratively compare cons cells to avoid stack overflow for long lists
        while (a is Cons ca && b is Cons cb)
        {
            if (!IsTruthy(Equal(ca.Car, cb.Car))) return Nil.Instance;
            a = ca.Cdr;
            b = cb.Cdr;
        }
        if (!(a is Cons) && !(b is Cons))
            return IsTrueEql(a, b) ? T.Instance : Nil.Instance;
        return Nil.Instance;
    }

    public static bool IsTrueEqual(LispObject a, LispObject b) => Equal(a, b) is not Nil;

    // --- Type predicates ---

    public static bool IsNilObj(LispObject obj)
    {
        obj = Primary(obj);
        return obj is Nil || ReferenceEquals(obj, Startup.NIL_SYM);
    }
    public static LispObject Null(LispObject obj) => IsNilObj(obj) ? T.Instance : Nil.Instance;
    public static LispObject Not(LispObject obj) => IsNilObj(obj) ? T.Instance : Nil.Instance;
    public static LispObject Atom(LispObject obj) => Primary(obj) is not Cons ? T.Instance : Nil.Instance;
    public static LispObject Consp(LispObject obj) => Primary(obj) is Cons ? T.Instance : Nil.Instance;

    public static LispObject Listp(LispObject obj)
    { obj = Primary(obj); return (obj is Cons || obj is Nil) ? T.Instance : Nil.Instance; }

    public static LispObject Numberp(LispObject obj) => Primary(obj) is Number ? T.Instance : Nil.Instance;
    public static LispObject Integerp(LispObject obj)
    { obj = Primary(obj); return (obj is Fixnum || obj is Bignum) ? T.Instance : Nil.Instance; }

    public static LispObject Rationalp(LispObject obj)
    { obj = Primary(obj); return (obj is Fixnum || obj is Bignum || obj is Ratio) ? T.Instance : Nil.Instance; }

    public static LispObject Floatp(LispObject obj)
    { obj = Primary(obj); return (obj is SingleFloat || obj is DoubleFloat) ? T.Instance : Nil.Instance; }

    public static LispObject Complexp(LispObject obj) => Primary(obj) is LispComplex ? T.Instance : Nil.Instance;

    public static LispObject Complex(LispObject[] args)
    {
        if (args.Length < 1 || args.Length > 2)
            throw new LispErrorException(new LispProgramError("COMPLEX: requires 1 or 2 arguments"));
        var real = AsNumber(args[0]);
        var imag = args.Length == 2 ? AsNumber(args[1]) : Fixnum.Make(0);
        return Arithmetic.MakeComplexPublic(real, imag);
    }

    public static LispObject Realpart(LispObject obj) =>
        obj is LispComplex c ? c.Real : AsNumber(obj);

    public static LispObject Imagpart(LispObject obj)
    {
        if (obj is not Number)
            throw new LispErrorException(new LispTypeError("IMAGPART: argument is not a number", obj, Startup.Sym("NUMBER")));
        if (obj is LispComplex c) return c.Imaginary;
        // For real numbers: imaginary part is 0 of same float type
        return obj switch
        {
            SingleFloat _ => new SingleFloat(0f),
            DoubleFloat _ => new DoubleFloat(0.0),
            _ => Fixnum.Make(0)
        };
    }
    public static LispObject Symbolp(LispObject obj)
    { obj = Primary(obj); return (obj is Symbol || obj is Nil || obj is T) ? T.Instance : Nil.Instance; }

    public static LispObject Stringp(LispObject obj)
    { obj = Primary(obj); return obj is LispString || (obj is LispVector v && v.IsCharVector && v.Rank == 1) ? T.Instance : Nil.Instance; }
    public static LispObject SimpleStringp(LispObject obj)
    { obj = Primary(obj); return obj is LispString || (obj is LispVector sv && sv.IsCharVector && !sv.HasFillPointer && sv.Rank == 1) ? T.Instance : Nil.Instance; }
    public static LispObject Characterp(LispObject obj) => Primary(obj) is LispChar ? T.Instance : Nil.Instance;
    public static LispObject Functionp(LispObject obj) => Primary(obj) is LispFunction ? T.Instance : Nil.Instance;
    public static LispObject Packagep(LispObject obj) => Primary(obj) is Package ? T.Instance : Nil.Instance;

    // --- Bool-returning predicates (fused in compile-if) ---
    public static bool IsTrueConsp(LispObject obj) => Primary(obj) is Cons;
    public static bool IsTrueAtom(LispObject obj) => Primary(obj) is not Cons;
    public static bool IsTrueListp(LispObject obj) { obj = Primary(obj); return obj is Cons || obj is Nil; }
    public static bool IsTrueNumberp(LispObject obj) => Primary(obj) is Number;
    public static bool IsTrueIntegerp(LispObject obj) { obj = Primary(obj); return obj is Fixnum || obj is Bignum; }
    public static bool IsTrueSymbolp(LispObject obj) { obj = Primary(obj); return obj is Symbol || obj is T || obj is Nil; }
    public static bool IsTrueStringp(LispObject obj) { obj = Primary(obj); return obj is LispString || (obj is LispVector v && v.IsCharVector && v.Rank == 1); }
    public static bool IsTrueCharacterp(LispObject obj) => Primary(obj) is LispChar;
    public static bool IsTrueFunctionp(LispObject obj) => Primary(obj) is LispFunction;
    public static LispObject Vectorp(LispObject obj)
    { obj = Primary(obj); return (obj is LispVector vp && vp.Rank == 1) || obj is LispString ? T.Instance : Nil.Instance; }
    public static LispObject BitVectorp(LispObject obj) => Primary(obj) is LispVector v && v.IsBitVector && v.Rank == 1 ? T.Instance : Nil.Instance;
    public static LispObject SimpleVectorp(LispObject obj) =>
        Primary(obj) is LispVector sv && sv.Rank == 1 && !sv.IsCharVector && !sv.IsBitVector && !sv.HasFillPointer && sv.ElementTypeName == "T" ? T.Instance : Nil.Instance;
    public static LispObject SimpleBitVectorp(LispObject obj) =>
        Primary(obj) is LispVector sbv && sbv.IsBitVector && sbv.Rank == 1 && !sbv.HasFillPointer ? T.Instance : Nil.Instance;
    public static LispObject Arrayp(LispObject obj)
    { obj = Primary(obj); return obj is LispVector || obj is LispString ? T.Instance : Nil.Instance; }
    public static LispObject Hash_table_p(LispObject obj) => Primary(obj) is LispHashTable ? T.Instance : Nil.Instance;
    public static LispObject Keywordp(LispObject obj) =>
        Primary(obj) is Symbol sym && sym.HomePackage != null && sym.HomePackage.Name == "KEYWORD"
            ? T.Instance : Nil.Instance;

    public static LispObject ArrayElementType(LispObject array)
    {
        if (array is LispString) return Startup.Sym("CHARACTER");
        if (array is LispVector v)
        {
            var et = v.ElementTypeName;
            if (et == "NIL") return Nil.Instance;
            // Compound types stored as "UNSIGNED-BYTE-8", "SIGNED-BYTE-16", etc.
            // — reconstruct the list form (UNSIGNED-BYTE 8).
            if (et.StartsWith("UNSIGNED-BYTE-") || et.StartsWith("SIGNED-BYTE-"))
            {
                int dash = et.LastIndexOf('-');
                if (int.TryParse(et.AsSpan(dash + 1), out int n))
                    return new Cons(Startup.Sym(et[..dash]), new Cons(Fixnum.Make(n), Nil.Instance));
            }
            return Startup.Sym(et);
        }
        throw new LispErrorException(new LispTypeError("ARRAY-ELEMENT-TYPE", array, Startup.Sym("ARRAY")));
    }

    public static LispObject ArrayHasFillPointerP(LispObject array)
    {
        if (array is LispVector v) return v.HasFillPointer ? T.Instance : Nil.Instance;
        if (array is LispString) return Nil.Instance;
        throw new LispErrorException(new LispTypeError("ARRAY-HAS-FILL-POINTER-P: not an array", array));
    }

    public static LispObject AdjustableArrayP(LispObject array)
    {
        if (array is LispVector v) return v.IsAdjustable ? T.Instance : Nil.Instance;
        if (array is LispString) return Nil.Instance; // simple strings are not adjustable
        throw new LispErrorException(new LispTypeError("ADJUSTABLE-ARRAY-P: not an array", array));
    }

    public static LispObject ArrayDisplacement(LispObject array)
    {
        if (array is LispVector v)
        {
            if (v.IsDisplaced) return Values(v.DisplacedTo!, Fixnum.Make(v.DisplacedOffset));
            return Values(Nil.Instance, Fixnum.Make(0));
        }
        if (array is LispString) return Values(Nil.Instance, Fixnum.Make(0));
        throw new LispErrorException(new LispTypeError("ARRAY-DISPLACEMENT: not an array", array));
    }

    internal static void RegisterPredicateBuiltins()
    {
        // Type predicates (inline-compiled but also needed as callable function objects)
        Startup.RegisterUnary("VECTORP", Runtime.Vectorp);
        Startup.RegisterUnary("BIT-VECTOR-P", Runtime.BitVectorp);
        Startup.RegisterUnary("SIMPLE-VECTOR-P", Runtime.SimpleVectorp);
        Startup.RegisterUnary("SIMPLE-BIT-VECTOR-P", Runtime.SimpleBitVectorp);
        Startup.RegisterUnary("ARRAYP", Runtime.Arrayp);
        Startup.RegisterUnary("ATOM", Runtime.Atom);
        Startup.RegisterUnary("CONSP", Runtime.Consp);
        Startup.RegisterUnary("LISTP", Runtime.Listp);
        Startup.RegisterUnary("NUMBERP", Runtime.Numberp);
        Startup.RegisterUnary("SYMBOLP", Runtime.Symbolp);
        Startup.RegisterUnary("STRINGP", Runtime.Stringp);
        Startup.RegisterUnary("SIMPLE-STRING-P", Runtime.SimpleStringp);
        Startup.RegisterUnary("CHARACTERP", Runtime.Characterp);
        Startup.RegisterUnary("CHARACTER", obj => obj switch {
            LispChar lc => lc,
            LispString s when s.Value.Length == 1 => LispChar.Make(s.Value[0]),
            Symbol sym when sym.Name.Length == 1 => LispChar.Make(sym.Name[0]),
            _ => throw new LispErrorException(new LispTypeError("CHARACTER: not a character designator", obj))
        });
        Startup.RegisterUnary("FUNCTIONP", Runtime.Functionp);
        Startup.RegisterUnary("INTEGERP", Runtime.Integerp);
        Startup.RegisterUnary("FLOATP", Runtime.Floatp);
        Startup.RegisterUnary("COMPILED-FUNCTION-P", obj => obj is LispFunction && obj is not GenericFunction ? T.Instance : Nil.Instance);
        Startup.RegisterUnary("NULL", Runtime.Null);
        Startup.RegisterUnary("NOT", Runtime.Not);
        Startup.RegisterUnary("PACKAGEP", Runtime.Packagep);

        // Character: CHAR-CODE, CODE-CHAR
        Startup.RegisterUnary("CHAR-CODE", Runtime.CharCode);
        Startup.RegisterUnary("CODE-CHAR", Runtime.CodeChar);

        // Type/coercion: COERCE, TYPE-OF, RATIONALP
        Startup.RegisterBinary("COERCE", Runtime.Coerce);
        Startup.RegisterUnary("TYPE-OF", Runtime.TypeOf);
        Startup.RegisterUnary("RATIONALP", Runtime.Rationalp);

        // Equality: EQ, EQL, EQUAL, EQUALP
        Startup.RegisterBinary("EQ", Runtime.Eq);
        Startup.RegisterBinary("EQL", Runtime.Eql);
        Startup.RegisterBinary("EQUAL", Runtime.Equal);
        Startup.RegisterBinary("EQUALP", (a, b) => LispHashTable.Equalp(a, b) ? T.Instance : Nil.Instance);

        // TYPEP accepts optional 3rd env arg (ignored)
        Emitter.CilAssembler.RegisterFunction("TYPEP",
            new LispFunction(args => {
                if (args.Length < 2) throw new LispErrorException(new LispProgramError("TYPEP: too few arguments"));
                return Runtime.Typep(args[0], args[1]);
            }));

        // TYPEXPAND-1: one step of deftype expansion, returns (expanded-type . expanded?)
        Emitter.CilAssembler.RegisterFunction("TYPEXPAND-1", new LispFunction(Runtime.TypeExpand1));

        // DIGIT-CHAR: (digit-char weight &optional radix) -> char or nil
        Emitter.CilAssembler.RegisterFunction("DIGIT-CHAR", new LispFunction(args => {
            Runtime.CheckArityMin("DIGIT-CHAR", args, 1);
            Runtime.CheckArityMax("DIGIT-CHAR", args, 2);
            if (args[0] is not Fixnum wf) return Nil.Instance;
            int weight = (int)wf.Value;
            int radix = args.Length >= 2 && args[1] is Fixnum rf ? (int)rf.Value : 10;
            if (radix < 2 || radix > 36) throw new LispErrorException(new LispTypeError("DIGIT-CHAR: radix out of range", args.Length >= 2 ? args[1] : Nil.Instance));
            if (weight < 0 || weight >= radix) return Nil.Instance;
            char c = weight < 10 ? (char)('0' + weight) : (char)('A' + weight - 10);
            return LispChar.Make(c);
        }));

        // CHAR-INT: like char-code (implementation-defined conversion)
        Startup.RegisterUnary("CHAR-INT", obj => obj is LispChar lc ? (LispObject)Fixnum.Make(lc.Value) :
            throw new LispErrorException(new LispTypeError("CHAR-INT: not a character", obj)));

        // PATHNAMEP as callable function
        Startup.RegisterUnary("PATHNAMEP", obj => obj is LispPathname ? T.Instance : Nil.Instance);

        // Character predicates
        Startup.RegisterUnary("UPPER-CASE-P", Runtime.UpperCaseP);
        Startup.RegisterUnary("LOWER-CASE-P", Runtime.LowerCaseP);
        Startup.RegisterUnary("ALPHA-CHAR-P", Runtime.AlphaCharP);
        Startup.RegisterUnary("ALPHANUMERICP", Runtime.Alphanumericp);
        Startup.RegisterUnary("GRAPHIC-CHAR-P", Runtime.GraphicCharP);
        Startup.RegisterUnary("BOTH-CASE-P", Runtime.BothCaseP);
        Emitter.CilAssembler.RegisterFunction("DIGIT-CHAR-P",
            new LispFunction(args => {
                var radix = args.Length > 1 ? args[1] : Fixnum.Make(10);
                return Runtime.DigitCharP(args[0], radix);
            }));
        Startup.RegisterUnary("CHAR-UPCASE", Runtime.CharUpcase);
        Startup.RegisterUnary("CHAR-DOWNCASE", Runtime.CharDowncase);
        Startup.RegisterUnary("STANDARD-CHAR-P", obj => {
            if (obj is not LispChar lc) throw new LispErrorException(new LispTypeError("STANDARD-CHAR-P: not a character", obj, Startup.Sym("CHARACTER")));
            char c2 = lc.Value;
            // Standard chars: space, newline, and printable ASCII 33-126
            return (c2 == ' ' || c2 == '\n' || (c2 >= '!' && c2 <= '~')) ? T.Instance : Nil.Instance;
        });
        // CHAR-NAME: returns the name of a character (e.g. #\Space -> "Space")
        Startup.RegisterUnary("CHAR-NAME", obj => {
            if (obj is not LispChar lc) throw new LispErrorException(new LispTypeError("CHAR-NAME: not a character", obj));
            return Runtime.CharName(lc.Value) is string n ? (LispObject)new LispString(n) : Nil.Instance;
        });
        // NAME-CHAR: returns the character with the given name (case-insensitive)
        Startup.RegisterUnary("NAME-CHAR", obj => {
            string s;
            if (obj is LispString ls) s = ls.Value;
            else if (obj is Nil) s = "NIL";
            else if (obj is T) s = "T";
            else if (obj is Symbol sym) s = sym.Name;
            else if (obj is LispChar lc) s = new string(lc.Value, 1);
            else if (obj is LispVector vec && vec.IsCharVector)
                s = vec.ToCharString();
            else throw new LispErrorException(new LispTypeError("NAME-CHAR: not a string designator", obj));
            return Runtime.NameChar(s) is char c2 ? (LispObject)LispChar.Make(c2) : Nil.Instance;
        });
    }

}
