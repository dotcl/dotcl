namespace DotCL;

public static partial class Runtime
{
    // --- String comparison ---

    /// <summary>Coerce a string designator (string, symbol, character) to its string value.</summary>
    public static string AsStringDesignator(LispObject obj, string caller)
    {
        return obj switch
        {
            LispString s => s.Value,
            LispVector v when v.IsCharVector => v.ToCharString(),
            Nil => "NIL",
            T => "T",
            Symbol sym => sym.Name,
            LispChar c => c.Value.ToString(),
            _ => throw new LispErrorException(new LispTypeError($"{caller}: not a string designator", obj))
        };
    }

    /// <summary>The characters of any Lisp string (base, fill-pointer, displaced), or null.</summary>
    public static string? StringValueOrNull(LispObject obj) => obj switch
    {
        LispString s => s.Value,
        LispVector v when v.IsCharVector && v.Rank == 1 => v.ToCharString(),
        _ => null
    };

    // Parse keyword args for string comparison functions.
    // Keywords: :start1, :end1, :start2, :end2, :allow-other-keys
    private static void ParseStringCmpArgs(LispObject[] args, string fname,
        out string s1, out string s2, out int start1, out int end1, out int start2, out int end2)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError($"{fname}: wrong number of arguments: {args.Length} (expected at least 2)"));
        s1 = ToStringDesignator(args[0], fname);
        s2 = ToStringDesignator(args[1], fname);
        start1 = 0; end1 = s1.Length; start2 = 0; end2 = s2.Length;
        if ((args.Length - 2) % 2 != 0)
            throw new LispErrorException(new LispProgramError($"{fname}: odd number of keyword arguments"));
        bool? allowOtherKeys = null;
        bool hasUnknown = false;
        bool start1Seen = false, end1Seen = false, start2Seen = false, end2Seen = false;
        for (int i = 2; i < args.Length; i += 2)
        {
            if (args[i] is not Symbol kwSym)
                throw new LispErrorException(new LispProgramError($"{fname}: not a keyword: {args[i]}"));
            var kwName = kwSym.Name;
            var val = args[i + 1];
            switch (kwName)
            {
                case "START1":
                    if (!start1Seen) { start1 = val is Fixnum fs1 ? Runtime.ClampIndex(fs1.Value) : val is Nil ? 0 : throw new LispErrorException(new LispTypeError($"{fname}: :start1 must be integer", val, Startup.Sym("INTEGER"))); start1Seen = true; }
                    break;
                case "END1":
                    if (!end1Seen) { end1 = val is Fixnum fe1 ? Runtime.ClampIndex(fe1.Value) : val is Nil ? s1.Length : throw new LispErrorException(new LispTypeError($"{fname}: :end1 must be integer or nil", val, Startup.Sym("INTEGER"))); end1Seen = true; }
                    break;
                case "START2":
                    if (!start2Seen) { start2 = val is Fixnum fs2 ? Runtime.ClampIndex(fs2.Value) : val is Nil ? 0 : throw new LispErrorException(new LispTypeError($"{fname}: :start2 must be integer", val, Startup.Sym("INTEGER"))); start2Seen = true; }
                    break;
                case "END2":
                    if (!end2Seen) { end2 = val is Fixnum fe2 ? Runtime.ClampIndex(fe2.Value) : val is Nil ? s2.Length : throw new LispErrorException(new LispTypeError($"{fname}: :end2 must be integer or nil", val, Startup.Sym("INTEGER"))); end2Seen = true; }
                    break;
                case "ALLOW-OTHER-KEYS": if (allowOtherKeys == null) allowOtherKeys = IsTruthy(val); break;
                default: hasUnknown = true; break;
            }
        }
        if (hasUnknown && allowOtherKeys != true)
            throw new LispErrorException(new LispProgramError($"{fname}: unknown keyword argument"));
        CheckBoundingIndices(start1, end1, s1.Length, fname);
        CheckBoundingIndices(start2, end2, s2.Length, fname);
    }

    /// <summary>Compare two strings under ONE bounds keyword pair without building an
    /// args array: the (string= name prefix :end1 n) shape, which prefix checks make the
    /// most common keyworded string comparison by a wide margin. Returns false when the
    /// pair is anything else (:allow-other-keys, a non-integer bound, an unknown keyword),
    /// leaving the caller to fall back to the shared variadic parser so behaviour and
    /// error messages stay in one place.</summary>
    private static bool TryStringCmp4(LispObject a, LispObject b, LispObject k, LispObject v,
                                      string fname, bool caseInsensitive, out (int pos, int cmp) result)
    {
        result = default;
        if (k is not Symbol kw || v is not Fixnum fv) return false;
        var s1 = ToStringSpan(a, fname);
        var s2 = ToStringSpan(b, fname);
        int start1 = 0, end1 = s1.Length, start2 = 0, end2 = s2.Length;
        switch (kw.Name)
        {
            case "START1": start1 = Runtime.ClampIndex(fv.Value); break;
            case "END1": end1 = Runtime.ClampIndex(fv.Value); break;
            case "START2": start2 = Runtime.ClampIndex(fv.Value); break;
            case "END2": end2 = Runtime.ClampIndex(fv.Value); break;
            default: return false;
        }
        if (start1 < 0 || end1 > s1.Length || start1 > end1 ||
            start2 < 0 || end2 > s2.Length || start2 > end2) return false;   // let the general path report
        result = CompareSubstrings(s1, start1, end1, s2, start2, end2, caseInsensitive);
        return true;
    }

    private static bool IsCmpBoundName(string name) =>
        name == "START1" || name == "END1" || name == "START2" || name == "END2";

    private static void ApplyCmpBound(string name, long value,
                                      ref bool seenStart1, ref bool seenEnd1,
                                      ref bool seenStart2, ref bool seenEnd2,
                                      ref int start1, ref int end1,
                                      ref int start2, ref int end2)
    {
        int v = Runtime.ClampIndex(value);
        switch (name)
        {
            case "START1": if (!seenStart1) { start1 = v; seenStart1 = true; } break;
            case "END1": if (!seenEnd1) { end1 = v; seenEnd1 = true; } break;
            case "START2": if (!seenStart2) { start2 = v; seenStart2 = true; } break;
            case "END2": if (!seenEnd2) { end2 = v; seenEnd2 = true; } break;
        }
    }

    /// <summary>Two bounds keyword pairs (:start1 i :end1 j and the other orders),
    /// handled without an args array. Anything else takes the shared parser.</summary>
    private static bool TryStringCmp6(LispObject a, LispObject b,
                                      LispObject k1, LispObject v1,
                                      LispObject k2, LispObject v2,
                                      string fname, bool caseInsensitive,
                                      out (int pos, int cmp) result)
    {
        result = default;
        if (k1 is not Symbol kw1 || k2 is not Symbol kw2) return false;
        if (v1 is not Fixnum fv1 || v2 is not Fixnum fv2) return false;
        string n1 = kw1.Name, n2 = kw2.Name;
        if (!IsCmpBoundName(n1) || !IsCmpBoundName(n2)) return false;
        var s1 = ToStringSpan(a, fname);
        var s2 = ToStringSpan(b, fname);
        int start1 = 0, end1 = s1.Length, start2 = 0, end2 = s2.Length;
        bool seenStart1 = false, seenEnd1 = false, seenStart2 = false, seenEnd2 = false;
        ApplyCmpBound(n1, fv1.Value, ref seenStart1, ref seenEnd1, ref seenStart2, ref seenEnd2,
                      ref start1, ref end1, ref start2, ref end2);
        ApplyCmpBound(n2, fv2.Value, ref seenStart1, ref seenEnd1, ref seenStart2, ref seenEnd2,
                      ref start1, ref end1, ref start2, ref end2);
        if (start1 < 0 || end1 > s1.Length || start1 > end1 ||
            start2 < 0 || end2 > s2.Length || start2 > end2) return false;   // let the general path report
        result = CompareSubstrings(s1, start1, end1, s2, start2, end2, caseInsensitive);
        return true;
    }

    // Compare substrings; returns (mismatchPos, cmpSign) where:
    //   mismatchPos = index in s1 of first difference (or start1 + min(len1,len2))
    //   cmpSign = negative(s1<s2), 0(equal), positive(s1>s2)
    private static (int pos, int cmp) CompareSubstrings(ReadOnlySpan<char> s1, int start1, int end1,
        ReadOnlySpan<char> s2, int start2, int end2, bool ignoreCase)
    {
        int len1 = end1 - start1, len2 = end2 - start2;
        int minLen = Math.Min(len1, len2);
        for (int i = 0; i < minLen; i++)
        {
            char c1 = s1[start1 + i], c2 = s2[start2 + i];
            if (ignoreCase) { c1 = CharCase.Fold(c1); c2 = CharCase.Fold(c2); }
            if (c1 != c2) return (start1 + i, c1 - c2);
        }
        return (start1 + minLen, len1 - len2);
    }

    // Variadic string comparison functions with :start1/:end1/:start2/:end2/:allow-other-keys
    public static LispObject StringEq(LispObject[] args)
    {
        ParseStringCmpArgs(args, "STRING=", out var s1, out var s2, out var st1, out var en1, out var st2, out var en2);
        var (_, cmp) = CompareSubstrings(s1, st1, en1, s2, st2, en2, false);
        return cmp == 0 ? T.Instance : Nil.Instance;
    }

    // Shared core for the 2-arg direct-delegate entries of the string comparison
    // family (installed as _func2 at registration so Invoke2 skips InvokeSlow's
    // args-array path). Semantically identical to the variadic entry with
    // args.Length == 2: ParseStringCmpArgs then reduces to two ToStringDesignator
    // coercions and a full-range compare: no keyword loop runs, no arity/
    // odd-keyword error can fire. Each 2-arg entry keeps its own function's
    // (pos, cmp) -> result mapping unchanged.
    private static (int pos, int cmp) CompareFull2(LispObject a, LispObject b,
                                                   string fname, bool ignoreCase)
    {
        var s1 = ToStringSpan(a, fname);
        var s2 = ToStringSpan(b, fname);
        return CompareSubstrings(s1, 0, s1.Length, s2, 0, s2.Length, ignoreCase);
    }

    // 4-arg entries: one bounds keyword pair, handled without an args array; anything
    // else falls back to the variadic entry so there is a single parser and a single
    // set of error messages.
    public static LispObject StringEq4(LispObject a, LispObject b, LispObject k, LispObject v)
        => TryStringCmp4(a, b, k, v, "STRING=", false, out var r)
            ? (r.cmp == 0 ? T.Instance : (LispObject)Nil.Instance)
            : StringEq(new[] { a, b, k, v });
    public static LispObject StringNotEq4(LispObject a, LispObject b, LispObject k, LispObject v)
        => TryStringCmp4(a, b, k, v, "STRING/=", false, out var r)
            ? (r.cmp != 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringNotEq(new[] { a, b, k, v });
    public static LispObject StringLt4(LispObject a, LispObject b, LispObject k, LispObject v)
        => TryStringCmp4(a, b, k, v, "STRING<", false, out var r)
            ? (r.cmp < 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringLt(new[] { a, b, k, v });
    public static LispObject StringGt4(LispObject a, LispObject b, LispObject k, LispObject v)
        => TryStringCmp4(a, b, k, v, "STRING>", false, out var r)
            ? (r.cmp > 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringGt(new[] { a, b, k, v });
    public static LispObject StringLe4(LispObject a, LispObject b, LispObject k, LispObject v)
        => TryStringCmp4(a, b, k, v, "STRING<=", false, out var r)
            ? (r.cmp <= 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringLe(new[] { a, b, k, v });
    public static LispObject StringGe4(LispObject a, LispObject b, LispObject k, LispObject v)
        => TryStringCmp4(a, b, k, v, "STRING>=", false, out var r)
            ? (r.cmp >= 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringGe(new[] { a, b, k, v });
    public static LispObject StringEqual4(LispObject a, LispObject b, LispObject k, LispObject v)
        => TryStringCmp4(a, b, k, v, "STRING-EQUAL", true, out var r)
            ? (r.cmp == 0 ? T.Instance : (LispObject)Nil.Instance)
            : StringEqualFn(new[] { a, b, k, v });
    public static LispObject StringNotEqual4(LispObject a, LispObject b, LispObject k, LispObject v)
        => TryStringCmp4(a, b, k, v, "STRING-NOT-EQUAL", true, out var r)
            ? (r.cmp != 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringNotEqualFn(new[] { a, b, k, v });
    public static LispObject StringLessp4(LispObject a, LispObject b, LispObject k, LispObject v)
        => TryStringCmp4(a, b, k, v, "STRING-LESSP", true, out var r)
            ? (r.cmp < 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringLessp(new[] { a, b, k, v });
    public static LispObject StringGreaterp4(LispObject a, LispObject b, LispObject k, LispObject v)
        => TryStringCmp4(a, b, k, v, "STRING-GREATERP", true, out var r)
            ? (r.cmp > 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringGreaterp(new[] { a, b, k, v });
    public static LispObject StringNotGreaterp4(LispObject a, LispObject b, LispObject k, LispObject v)
        => TryStringCmp4(a, b, k, v, "STRING-NOT-GREATERP", true, out var r)
            ? (r.cmp <= 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringNotGreaterp(new[] { a, b, k, v });
    public static LispObject StringNotLessp4(LispObject a, LispObject b, LispObject k, LispObject v)
        => TryStringCmp4(a, b, k, v, "STRING-NOT-LESSP", true, out var r)
            ? (r.cmp >= 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringNotLessp(new[] { a, b, k, v });

    // 6-arg entries: two bounds keyword pairs, the (string= a b :start1 i :end1 j)
    // shape; anything else falls back to the variadic entry.
    public static LispObject StringEq6(LispObject a, LispObject b, LispObject k1, LispObject v1, LispObject k2, LispObject v2)
        => TryStringCmp6(a, b, k1, v1, k2, v2, "STRING=", false, out var r)
            ? (r.cmp == 0 ? T.Instance : (LispObject)Nil.Instance)
            : StringEq(new[] { a, b, k1, v1, k2, v2 });
    public static LispObject StringNotEq6(LispObject a, LispObject b, LispObject k1, LispObject v1, LispObject k2, LispObject v2)
        => TryStringCmp6(a, b, k1, v1, k2, v2, "STRING/=", false, out var r)
            ? (r.cmp != 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringNotEq(new[] { a, b, k1, v1, k2, v2 });
    public static LispObject StringLt6(LispObject a, LispObject b, LispObject k1, LispObject v1, LispObject k2, LispObject v2)
        => TryStringCmp6(a, b, k1, v1, k2, v2, "STRING<", false, out var r)
            ? (r.cmp < 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringLt(new[] { a, b, k1, v1, k2, v2 });
    public static LispObject StringGt6(LispObject a, LispObject b, LispObject k1, LispObject v1, LispObject k2, LispObject v2)
        => TryStringCmp6(a, b, k1, v1, k2, v2, "STRING>", false, out var r)
            ? (r.cmp > 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringGt(new[] { a, b, k1, v1, k2, v2 });
    public static LispObject StringLe6(LispObject a, LispObject b, LispObject k1, LispObject v1, LispObject k2, LispObject v2)
        => TryStringCmp6(a, b, k1, v1, k2, v2, "STRING<=", false, out var r)
            ? (r.cmp <= 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringLe(new[] { a, b, k1, v1, k2, v2 });
    public static LispObject StringGe6(LispObject a, LispObject b, LispObject k1, LispObject v1, LispObject k2, LispObject v2)
        => TryStringCmp6(a, b, k1, v1, k2, v2, "STRING>=", false, out var r)
            ? (r.cmp >= 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringGe(new[] { a, b, k1, v1, k2, v2 });
    public static LispObject StringEqual6(LispObject a, LispObject b, LispObject k1, LispObject v1, LispObject k2, LispObject v2)
        => TryStringCmp6(a, b, k1, v1, k2, v2, "STRING-EQUAL", true, out var r)
            ? (r.cmp == 0 ? T.Instance : (LispObject)Nil.Instance)
            : StringEqualFn(new[] { a, b, k1, v1, k2, v2 });
    public static LispObject StringNotEqual6(LispObject a, LispObject b, LispObject k1, LispObject v1, LispObject k2, LispObject v2)
        => TryStringCmp6(a, b, k1, v1, k2, v2, "STRING-NOT-EQUAL", true, out var r)
            ? (r.cmp != 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringNotEqualFn(new[] { a, b, k1, v1, k2, v2 });
    public static LispObject StringLessp6(LispObject a, LispObject b, LispObject k1, LispObject v1, LispObject k2, LispObject v2)
        => TryStringCmp6(a, b, k1, v1, k2, v2, "STRING-LESSP", true, out var r)
            ? (r.cmp < 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringLessp(new[] { a, b, k1, v1, k2, v2 });
    public static LispObject StringGreaterp6(LispObject a, LispObject b, LispObject k1, LispObject v1, LispObject k2, LispObject v2)
        => TryStringCmp6(a, b, k1, v1, k2, v2, "STRING-GREATERP", true, out var r)
            ? (r.cmp > 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringGreaterp(new[] { a, b, k1, v1, k2, v2 });
    public static LispObject StringNotGreaterp6(LispObject a, LispObject b, LispObject k1, LispObject v1, LispObject k2, LispObject v2)
        => TryStringCmp6(a, b, k1, v1, k2, v2, "STRING-NOT-GREATERP", true, out var r)
            ? (r.cmp <= 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringNotGreaterp(new[] { a, b, k1, v1, k2, v2 });
    public static LispObject StringNotLessp6(LispObject a, LispObject b, LispObject k1, LispObject v1, LispObject k2, LispObject v2)
        => TryStringCmp6(a, b, k1, v1, k2, v2, "STRING-NOT-LESSP", true, out var r)
            ? (r.cmp >= 0 ? Fixnum.Make(r.pos) : (LispObject)Nil.Instance)
            : StringNotLessp(new[] { a, b, k1, v1, k2, v2 });

    // Case-sensitive 2-arg entries
    public static LispObject StringEq2(LispObject a, LispObject b)
    { var (_, cmp) = CompareFull2(a, b, "STRING=", false); return cmp == 0 ? T.Instance : Nil.Instance; }
    public static LispObject StringNotEq2(LispObject a, LispObject b)
    { var (pos, cmp) = CompareFull2(a, b, "STRING/=", false); return cmp != 0 ? Fixnum.Make(pos) : Nil.Instance; }
    public static LispObject StringLt2(LispObject a, LispObject b)
    { var (pos, cmp) = CompareFull2(a, b, "STRING<", false); return cmp < 0 ? Fixnum.Make(pos) : Nil.Instance; }
    public static LispObject StringGt2(LispObject a, LispObject b)
    { var (pos, cmp) = CompareFull2(a, b, "STRING>", false); return cmp > 0 ? Fixnum.Make(pos) : Nil.Instance; }
    public static LispObject StringLe2(LispObject a, LispObject b)
    { var (pos, cmp) = CompareFull2(a, b, "STRING<=", false); return cmp <= 0 ? Fixnum.Make(pos) : Nil.Instance; }
    public static LispObject StringGe2(LispObject a, LispObject b)
    { var (pos, cmp) = CompareFull2(a, b, "STRING>=", false); return cmp >= 0 ? Fixnum.Make(pos) : Nil.Instance; }

    // Case-insensitive 2-arg entries
    public static LispObject StringEqual2(LispObject a, LispObject b)
    { var (_, cmp) = CompareFull2(a, b, "STRING-EQUAL", true); return cmp == 0 ? T.Instance : Nil.Instance; }
    public static LispObject StringNotEqual2(LispObject a, LispObject b)
    { var (pos, cmp) = CompareFull2(a, b, "STRING-NOT-EQUAL", true); return cmp != 0 ? Fixnum.Make(pos) : Nil.Instance; }
    public static LispObject StringLessp2(LispObject a, LispObject b)
    { var (pos, cmp) = CompareFull2(a, b, "STRING-LESSP", true); return cmp < 0 ? Fixnum.Make(pos) : Nil.Instance; }
    public static LispObject StringGreaterp2(LispObject a, LispObject b)
    { var (pos, cmp) = CompareFull2(a, b, "STRING-GREATERP", true); return cmp > 0 ? Fixnum.Make(pos) : Nil.Instance; }
    public static LispObject StringNotGreaterp2(LispObject a, LispObject b)
    { var (pos, cmp) = CompareFull2(a, b, "STRING-NOT-GREATERP", true); return cmp <= 0 ? Fixnum.Make(pos) : Nil.Instance; }
    public static LispObject StringNotLessp2(LispObject a, LispObject b)
    { var (pos, cmp) = CompareFull2(a, b, "STRING-NOT-LESSP", true); return cmp >= 0 ? Fixnum.Make(pos) : Nil.Instance; }

    public static LispObject StringNotEq(LispObject[] args)
    {
        ParseStringCmpArgs(args, "STRING/=", out var s1, out var s2, out var st1, out var en1, out var st2, out var en2);
        var (pos, cmp) = CompareSubstrings(s1, st1, en1, s2, st2, en2, false);
        return cmp != 0 ? Fixnum.Make(pos) : Nil.Instance;
    }

    public static LispObject StringLt(LispObject[] args)
    {
        ParseStringCmpArgs(args, "STRING<", out var s1, out var s2, out var st1, out var en1, out var st2, out var en2);
        var (pos, cmp) = CompareSubstrings(s1, st1, en1, s2, st2, en2, false);
        return cmp < 0 ? Fixnum.Make(pos) : Nil.Instance;
    }

    public static LispObject StringGt(LispObject[] args)
    {
        ParseStringCmpArgs(args, "STRING>", out var s1, out var s2, out var st1, out var en1, out var st2, out var en2);
        var (pos, cmp) = CompareSubstrings(s1, st1, en1, s2, st2, en2, false);
        return cmp > 0 ? Fixnum.Make(pos) : Nil.Instance;
    }

    public static LispObject StringLe(LispObject[] args)
    {
        ParseStringCmpArgs(args, "STRING<=", out var s1, out var s2, out var st1, out var en1, out var st2, out var en2);
        var (pos, cmp) = CompareSubstrings(s1, st1, en1, s2, st2, en2, false);
        return cmp <= 0 ? Fixnum.Make(pos) : Nil.Instance;
    }

    public static LispObject StringGe(LispObject[] args)
    {
        ParseStringCmpArgs(args, "STRING>=", out var s1, out var s2, out var st1, out var en1, out var st2, out var en2);
        var (pos, cmp) = CompareSubstrings(s1, st1, en1, s2, st2, en2, false);
        return cmp >= 0 ? Fixnum.Make(pos) : Nil.Instance;
    }

    // Case-insensitive variants (STRING-EQUAL etc.)
    public static LispObject StringEqualFn(LispObject[] args)
    {
        ParseStringCmpArgs(args, "STRING-EQUAL", out var s1, out var s2, out var st1, out var en1, out var st2, out var en2);
        var (_, cmp) = CompareSubstrings(s1, st1, en1, s2, st2, en2, true);
        return cmp == 0 ? T.Instance : Nil.Instance;
    }

    public static LispObject StringNotEqualFn(LispObject[] args)
    {
        ParseStringCmpArgs(args, "STRING-NOT-EQUAL", out var s1, out var s2, out var st1, out var en1, out var st2, out var en2);
        var (pos, cmp) = CompareSubstrings(s1, st1, en1, s2, st2, en2, true);
        return cmp != 0 ? Fixnum.Make(pos) : Nil.Instance;
    }

    public static LispObject StringLessp(LispObject[] args)
    {
        ParseStringCmpArgs(args, "STRING-LESSP", out var s1, out var s2, out var st1, out var en1, out var st2, out var en2);
        var (pos, cmp) = CompareSubstrings(s1, st1, en1, s2, st2, en2, true);
        return cmp < 0 ? Fixnum.Make(pos) : Nil.Instance;
    }

    public static LispObject StringGreaterp(LispObject[] args)
    {
        ParseStringCmpArgs(args, "STRING-GREATERP", out var s1, out var s2, out var st1, out var en1, out var st2, out var en2);
        var (pos, cmp) = CompareSubstrings(s1, st1, en1, s2, st2, en2, true);
        return cmp > 0 ? Fixnum.Make(pos) : Nil.Instance;
    }

    public static LispObject StringNotGreaterp(LispObject[] args)
    {
        ParseStringCmpArgs(args, "STRING-NOT-GREATERP", out var s1, out var s2, out var st1, out var en1, out var st2, out var en2);
        var (pos, cmp) = CompareSubstrings(s1, st1, en1, s2, st2, en2, true);
        return cmp <= 0 ? Fixnum.Make(pos) : Nil.Instance;
    }

    public static LispObject StringNotLessp(LispObject[] args)
    {
        ParseStringCmpArgs(args, "STRING-NOT-LESSP", out var s1, out var s2, out var st1, out var en1, out var st2, out var en2);
        var (pos, cmp) = CompareSubstrings(s1, st1, en1, s2, st2, en2, true);
        return cmp >= 0 ? Fixnum.Make(pos) : Nil.Instance;
    }

    // --- Sequence operations ---

    /// <summary>A bounding index as an int. An index beyond int range is past the end of any
    /// sequence here, so it clamps to the nearest int and fails the usual range check, instead
    /// of wrapping to a small valid-looking index.</summary>
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static int ClampIndex(long v) =>
        unchecked((ulong)(v - int.MinValue)) <= uint.MaxValue ? (int)v : (v < 0 ? int.MinValue : int.MaxValue);

    /// <summary>A .NET one-dimensional array or collection (not a string) that Lisp reads as a
    /// sequence: a LispVector holding a snapshot of its elements. The wrapped object itself is
    /// left alone (a byte[] stays the same buffer); this is for reading, as foreach does.</summary>
    internal static bool TryDotNetSequence(LispObject obj, out LispVector snapshot)
    {
        snapshot = null!;
        if (obj is not LispDotNetObject { Value: { } v } || v is string) return false;
        if (v is System.Array { Rank: 1 } || (v is System.Collections.IEnumerable && v is not System.Collections.IDictionary))
        {
            var items = new List<LispObject>();
            foreach (var item in (System.Collections.IEnumerable)v) items.Add(DotNetToLisp(item));
            snapshot = new LispVector(items.ToArray());
            return true;
        }
        return false;
    }

    /// <summary>A .NET array or collection argument of a sequence function as the vector snapshot
    /// TryDotNetSequence makes; any other argument unchanged. The test is one type check, so the
    /// vector, string and list paths neither allocate nor call anything more.</summary>
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    internal static LispObject AsSequenceArg(LispObject seq)
    {
        // An array of rank other than 1 is not a sequence.
        if (seq is LispVector mv && mv.Rank != 1)
            throw new LispErrorException(new LispTypeError("not a sequence", seq, Startup.Sym("SEQUENCE")));
        return seq is LispDotNetObject && TryDotNetSequence(seq, out var snap) ? snap : seq;
    }

    // The range ELT's index had to fall in, as a type: (INTEGER 0 (0)) for an empty
    // sequence, (INTEGER 0 N-1) otherwise.
    internal static int ListPrefixLength(LispObject list)
    {
        int n = 0;
        for (var c = list; c is Cons cc; c = cc.Cdr) n++;
        return n;
    }

    internal static LispObject EltIndexType(int length) => length <= 0
        ? List(Startup.Sym("INTEGER"), Fixnum.Make(0), List(Fixnum.Make(0)))
        : List(Startup.Sym("INTEGER"), Fixnum.Make(0), Fixnum.Make(length - 1));

    public static LispObject Elt(LispObject seq, LispObject index)
    {
        if (index is not Fixnum f)
            throw new LispErrorException(new LispTypeError("ELT: index must be integer", index));
        long idx = f.Value;
        int i;
        if ((ulong)idx > int.MaxValue)
        {
            if (idx < 0)
                throw new LispErrorException(new LispTypeError($"ELT: index {idx} is negative", index, Startup.Sym("UNSIGNED-BYTE")));
            // An index beyond int is past the end of anything this implementation can hold.
            i = int.MaxValue;
        }
        else i = (int)idx;
        if (seq is LispVector v)
        {
            if (i >= v.Length) throw new LispErrorException(new LispTypeError($"ELT: index {idx} out of bounds for vector of length {v.Length}", index, EltIndexType(v.Length)));
            return v[i];
        }
        if (seq is LispString s)
        {
            if (i >= s.Length) throw new LispErrorException(new LispTypeError($"ELT: index {idx} out of range for string of length {s.Length}", index, EltIndexType(s.Length)));
            return LispChar.Make(s[i]);
        }
        if (seq is Nil)
            throw new LispErrorException(new LispTypeError($"ELT: index {idx} out of bounds for empty sequence", index, EltIndexType(0)));
        if (seq is Cons)
        {
            var cur = seq;
            for (int j = 0; j < i; j++)
            {
                if (cur is not Cons c) throw new LispErrorException(new LispTypeError($"ELT: index {idx} out of bounds for list", index, EltIndexType(ListPrefixLength(seq))));
                cur = c.Cdr;
            }
            if (cur is Cons cn) return cn.Car;
            throw new LispErrorException(new LispTypeError($"ELT: index {idx} out of bounds for list", index, EltIndexType(ListPrefixLength(seq))));
        }
        if (TryDotNetSequence(seq, out var dsnap)) return Elt(dsnap, index);
        throw new LispErrorException(new LispTypeError("ELT: not a sequence", seq));
    }

    /// <summary>Check that START and END really are bounding indices of a sequence of
    /// LENGTH elements (CLHS: 0 &lt;= start &lt;= end &lt;= length), signalling a type error if
    /// not. Without this a too-large END silently produced a wrong answer rather than an
    /// error: (subseq '(1 2 3) 0 99) returned the list padded with 96 NILs.</summary>
    // The type of an :END value: a bounding index or NIL.
    private static LispObject EndIndexType()
        => List(Startup.Sym("OR"), Startup.Sym("NULL"), Startup.Sym("UNSIGNED-BYTE"));

    private static void CheckBoundingIndices(int start, int end, int length, string fname)
    {
        if (start < 0 || start > length)
            throw new LispErrorException(new LispTypeError(
                $"{fname}: start index {start} is not in [0, {length}]", Fixnum.Make(start)));
        if (end < 0)
            throw new LispErrorException(new LispTypeError(
                $"{fname}: end index {end} is negative", Fixnum.Make(end), EndIndexType()));
        if (end < start || end > length)
        {
            // As SBCL signals an END that is out of range: the datum is the pair (START . END)
            // and the expected type is the pair of ranges the indices should have been in.
            LispObject expected = List(Startup.Sym("CONS"),
                List(Startup.Sym("INTEGER"), Fixnum.Make(0), Fixnum.Make(length)),
                List(Startup.Sym("INTEGER"), Fixnum.Make(start), Fixnum.Make(length)));
            throw new LispErrorException(new LispTypeError(
                end < start && end >= 0 && end <= length
                    ? $"{fname}: end index {end} is before the start index {start}"
                    : $"{fname}: end index {end} is not in [{start}, {length}]",
                new Cons(Fixnum.Make(start), Fixnum.Make(end)), expected));
        }
    }

    private static int CheckedListLength(LispObject seq, string fname)
    {
        int len = 0;
        var cur = seq;
        while (cur is Cons c) { len++; cur = c.Cdr; }
        if (cur is not Nil)
            throw new LispErrorException(new LispTypeError($"{fname}: not a proper list", cur, Startup.Sym("LIST")));
        return len;
    }

    public static LispObject Subseq(LispObject seq, LispObject start, LispObject end)
    {
        int s = (start is Fixnum fs) ? Runtime.ClampIndex(fs.Value) : throw new LispErrorException(new LispTypeError("SUBSEQ: start must be integer", start));
        int? e = end is Nil ? null : (end is Fixnum fe ? (int?)Runtime.ClampIndex(fe.Value) : throw new LispErrorException(new LispTypeError("SUBSEQ: end must be integer or nil", end, EndIndexType())));

        if (seq is LispString str)
        {
            int endIdx = e ?? str.Length;
            CheckBoundingIndices(s, endIdx, str.Length, "SUBSEQ");
            // Read the live backing in place: Value would build the whole string again for a
            // string a program has written to, whatever the size of the piece asked for.
            return new LispString(str.CharsOrNull is { } live
                ? new string(live, s, endIdx - s)
                : str.StrOrNull!.Substring(s, endIdx - s));
        }
        if (seq is LispVector vec)
        {
            int endIdx = e ?? vec.Length;
            CheckBoundingIndices(s, endIdx, vec.Length, "SUBSEQ");
            var items = new LispObject[endIdx - s];
            if (vec.IsPlainObjectVector)
                Array.Copy(vec._elements, s, items, 0, endIdx - s);   // the piece in one block copy
            else
                for (int i = s; i < endIdx; i++) items[i - s] = vec.GetElement(i);
            return new LispVector(items, vec.ElementTypeName);
        }
        if (seq is Cons || seq is Nil)
        {
            // List subseq. With an END the walk stops there, so a circular list can be cut
            // ((subseq circular 0 5) is the usual way to take a few elements of one); the length is
            // only known, for the error message, when the list ends before END.
            if (e is int endGiven)
            {
                var walk = seq;
                // A negative START or an END below START is an error whatever the list holds.
                if (s < 0 || endGiven < s)
                {
                    // the length only names the expected range: count at most a million cells,
                    // so a circular list does not hang the error
                    int known = 0;
                    for (var probe = seq; known < 1000000 && probe is Cons pk; probe = pk.Cdr) known++;
                    CheckBoundingIndices(s, endGiven, known < 1000000 ? known : int.MaxValue, "SUBSEQ");
                }
                for (int i = 0; i < s; i++)
                {
                    if (walk is Cons wc) walk = wc.Cdr;
                    else CheckBoundingIndices(s, endGiven, i, "SUBSEQ");
                }
                // The cells actually present are counted first (no allocation), so an END far past
                // the list's length is an error at the list's end, not an allocation of END - START.
                int wanted = endGiven - s;
                int present = 0;
                for (var probe = walk; present < wanted && probe is Cons pc; probe = pc.Cdr) present++;
                if (present < wanted) CheckBoundingIndices(s, endGiven, s + present, "SUBSEQ");
                var taken = new LispObject[wanted];
                for (int i = 0; i < taken.Length; i++)
                {
                    var wc = (Cons)walk;
                    taken[i] = wc.Car; walk = wc.Cdr;
                }
                return List(taken);
            }
            int listLen = ListLength(seq);
            CheckBoundingIndices(s, e ?? listLen, listLen, "SUBSEQ");
            LispObject cur = seq;
            for (int i = 0; i < s; i++)
            {
                if (cur is Cons c) cur = c.Cdr;
                else break;
            }
            int count = (e ?? listLen) - s;
            var items = new LispObject[count];
            for (int i = 0; i < count; i++)
            {
                if (cur is Cons c) { items[i] = c.Car; cur = c.Cdr; }
                else items[i] = Nil.Instance;
            }
            return List(items);
        }
        if (TryDotNetSequence(seq, out var dsnap)) return Subseq(dsnap, start, end);
        throw new LispErrorException(new LispTypeError("SUBSEQ: not a sequence", seq));
    }

    public static LispObject CopySeq(LispObject seq)
    {
        if (seq is LispString str)
            return new LispString(str.Value);
        if (seq is LispVector v)
        {
            if (v._displacedTo == null && v._dimensions == null
                && CopyVectorStorage(v) is LispVector fast)
                return fast;
            // Copy elements, preserve ElementTypeName (important for char/bit vectors)
            var items = new LispObject[v.Length];
            for (int i = 0; i < v.Length; i++) items[i] = v[i];
            return new LispVector(items, v.ElementTypeName);
        }
        if (seq is Nil) return Nil.Instance;
        if (seq is Cons)
            return CopyList(seq);
        if (TryDotNetSequence(seq, out var dsnap)) return CopySeq(dsnap);
        throw new LispErrorException(new LispTypeError("COPY-SEQ: not a sequence", seq));
    }

    /// <summary>COPY-SEQ of a one-dimensional vector that owns its storage, as a
    /// copy of that storage. The element-by-element path unpacked a bit or
    /// numeric vector into a fresh object array (8 bytes an element, a boxed
    /// value each) only for the new vector to pack it again. Null when the
    /// storage is of a shape this does not cover.</summary>
    private static LispVector? CopyVectorStorage(LispVector v)
    {
        int n = v.Length;
        if (v._bitData != null)
        {
            var r = new LispVector(n, Nil.Instance, "BIT");
            int words = (n + 63) >> 6;
            Array.Copy(v._bitData, r._bitData!, words);
            // Bits past the fill pointer are not part of the copy.
            int tail = n & 63;
            if (tail != 0) r._bitData![words - 1] &= (1UL << tail) - 1;
            return r;
        }
        if (v._numData != null)
        {
            var r = new LispVector(n, Nil.Instance, v.ElementTypeName);
            if (r._numData == null || r._numKind != v._numKind) return null;
            Array.Copy(v._numData, r._numData, n);
            return r;
        }
        if (v.IsBitVector || v.ElementTypeName != "T") return null;
        var items = new LispObject[n];
        Array.Copy(v._elements, items, n);
        return new LispVector(items, "T");
    }

    private static void CollectSequenceElements(LispObject seq, List<LispObject> items)
    {
        if (seq is Nil) return;
        if (seq is Cons)
        {
            var cur = seq;
            while (cur is Cons c) { items.Add(c.Car); cur = c.Cdr; }
            if (cur is not Nil)
                throw new LispErrorException(new LispTypeError("CONCATENATE: not a proper list", seq, Startup.Sym("LIST")));
        }
        else if (seq is LispString s)
            foreach (char c in s.Value) items.Add(LispChar.Make(c));
        else if (seq is LispVector v)
            for (int i = 0; i < v.Length; i++) items.Add(v.ElementAt(i));
        else
            throw new LispErrorException(new LispTypeError("CONCATENATE: not a sequence", seq));
    }

    /// <summary>Total element count of the sequences. Knowing it up front lets
    /// CONCATENATE fill one exact-size result instead of growing a List and copying
    /// it out. Walking a list twice is free next to that.</summary>
    private static int TotalSequenceLength(LispObject[] sequences)
    {
        int n = 0;
        foreach (var seq in sequences)
        {
            if (seq is Nil) continue;
            else if (seq is Cons) n += CheckedListLength(seq, "CONCATENATE");
            else if (seq is LispString s) n += s.Length;
            else if (seq is LispVector v) n += v.Length;
            else throw new LispErrorException(new LispTypeError("CONCATENATE: not a sequence", seq));
        }
        return n;
    }

    /// <summary>Fill items with every element of the sequences in order. The array must
    /// be exactly TotalSequenceLength long.</summary>
    private static void FillSequenceElements(LispObject[] sequences, LispObject[] items)
    {
        int k = 0;
        foreach (var seq in sequences)
        {
            if (seq is Cons) { for (var cur = seq; cur is Cons c; cur = c.Cdr) items[k++] = c.Car; }
            else if (seq is LispString s) { foreach (char ch in s.Value) items[k++] = LispChar.Make(ch); }
            else if (seq is LispVector v) { for (int i = 0; i < v.Length; i++) items[k++] = v.ElementAt(i); }
        }
    }

    /// <summary>The length a sequence result type with an array head requires, or
    /// -1 when it leaves the length open. (VECTOR et N) and (SIMPLE-VECTOR N) give
    /// a length; for ARRAY and SIMPLE-ARRAY the third element is a dimension spec,
    /// where an integer is a RANK (CLHS 15.1.2.1 / the ARRAY type specifier), so
    /// (simple-array (unsigned-byte 4) 1) is any rank-1 array, and (N) is a
    /// length. An array type of any other rank is not a sequence type.</summary>
    internal static int RequiredSequenceLength(Cons type, string who)
    {
        string head = type.Car is Symbol hs ? hs.Name : "";
        var afterHead = type.Cdr as Cons;
        LispObject? spec = head == "SIMPLE-VECTOR" ? afterHead?.Car : (afterHead?.Cdr as Cons)?.Car;
        if (spec == null || spec is Symbol { Name: "*" } && spec is not Nil) return -1;
        if (head is "ARRAY" or "SIMPLE-ARRAY")
        {
            bool rankOne = spec switch
            {
                Fixnum rank => rank.Value == 1,
                Cons dims => dims.Cdr is Nil,
                _ => false,   // NIL: rank 0
            };
            if (!rankOne)
                throw new LispErrorException(new LispTypeError(
                    $"{who}: {type} is not a sequence type (its rank is not 1)", type, Startup.Sym("SEQUENCE")));
            return spec is Cons d1 ? (d1.Car is Fixnum n ? SizeSpecInt(n.Value) : d1.Car is Bignum ? int.MaxValue : -1) : -1;
        }
        return spec is Fixnum len ? SizeSpecInt(len.Value) : spec is Bignum ? int.MaxValue : -1;
    }

    /// <summary>The length of SEQ when it is NIL or a simple general (element type T) vector whose
    /// elements are all in its own array, else -1.</summary>
    private static int SimpleGeneralLength(LispObject seq)
        => seq is LispVector v ? (v.Rank == 1 && v.IsSimple && v.IsGeneralElement ? v.Length : -1)
           : seq is Nil ? 0 : -1;

    private static void CopySimpleGeneral(LispObject seq, LispObject[] dst, ref int pos)
    {
        if (seq is LispVector v)
        {
            int n = v.Length;
            Array.Copy(v._elements, 0, dst, pos, n);
            pos += n;
        }
    }

    /// <summary>(CONCATENATE 'SIMPLE-VECTOR / 'VECTOR A B [C]) of simple general vectors and NILs:
    /// one array allocated and copied. Null when the call is of another shape (the general path
    /// answers it).</summary>
    private static LispObject? TryConcatGeneralVectors(LispObject resultType, LispObject a, LispObject b, LispObject? c)
    {
        if (resultType is not Symbol rs || (rs.Name != "SIMPLE-VECTOR" && rs.Name != "VECTOR")) return null;
        int la = SimpleGeneralLength(a), lb = SimpleGeneralLength(b);
        int lc = c is null ? 0 : SimpleGeneralLength(c);
        if (la < 0 || lb < 0 || lc < 0) return null;
        var items = new LispObject[la + lb + lc];
        int pos = 0;
        CopySimpleGeneral(a, items, ref pos);
        CopySimpleGeneral(b, items, ref pos);
        if (c is not null) CopySimpleGeneral(c, items, ref pos);
        return new LispVector(items);
    }

    public static LispObject Concatenate2(LispObject resultType, LispObject a, LispObject b)
    {
        if (TryConcatGeneralVectors(resultType, a, b, null) is { } fast) return fast;
        if (resultType is Symbol sym && (sym.Name == "STRING" || sym.Name == "SIMPLE-STRING")
            && a is LispString sa && b is LispString sb)
            return new LispString(string.Concat(sa.Value, sb.Value));
        return Concatenate(resultType, new[] { a, b });
    }

    public static LispObject Concatenate3(LispObject resultType, LispObject a, LispObject b, LispObject c)
    {
        if (TryConcatGeneralVectors(resultType, a, b, c) is { } fast) return fast;
        if (resultType is Symbol sym && (sym.Name == "STRING" || sym.Name == "SIMPLE-STRING")
            && a is LispString sa && b is LispString sb && c is LispString sc)
            return new LispString(string.Concat(sa.Value, sb.Value, sc.Value));
        return Concatenate(resultType, new[] { a, b, c });
    }

    public static LispObject Concatenate(LispObject resultType, params LispObject[] sequences)
    {
        var result = ConcatenateCore(resultType, sequences);
        // (concatenate '(string 3) "ab"): a string result type that names its length has to get it.
        if (resultType is Cons spec && spec.Car is Symbol head)
        {
            LispObject? lenSpec = null;
            switch (head.Name)
            {
                case "STRING": case "SIMPLE-STRING": case "BASE-STRING": case "SIMPLE-BASE-STRING":
                    lenSpec = spec.Cdr is Cons l1 ? l1.Car : null;
                    break;
                case "VECTOR": case "SIMPLE-ARRAY": case "ARRAY":
                    if (spec.Cdr is Cons e && e.Car is Symbol et && et.Name is "CHARACTER" or "BASE-CHAR" or "STANDARD-CHAR"
                        && e.Cdr is Cons l2)
                        lenSpec = head.Name == "VECTOR" ? l2.Car
                                : l2.Car is Cons dims && dims.Cdr is Nil ? dims.Car : null;
                    break;
            }
            if (lenSpec is Fixnum n && result is LispString rs && rs.Value.Length != n.Value)
                throw new LispErrorException(new LispTypeError(
                    $"CONCATENATE: the result has length {rs.Value.Length}, but the type {FormatObject(resultType, true)} asks for {n.Value}",
                    result, resultType));
        }
        return result;
    }

    private static LispObject ConcatenateCore(LispObject resultType, params LispObject[] sequences)
    {
        for (int ci = 0; ci < sequences.Length; ci++) sequences[ci] = AsSequenceArg(sequences[ci]);
        // Determine the effective type name, handling compound type specifiers like (vector * *)
        string typeName;
        if (resultType is Symbol sym)
            typeName = sym.Name;
        else if (resultType is T)
            typeName = "T";
        else if (resultType is Cons headCons)
            typeName = headCons.Car is Symbol headSym ? headSym.Name : "";
        else
            typeName = "";

        if (typeName is "BASE-STRING" or "SIMPLE-BASE-STRING" ||
            (typeName == "VECTOR" && resultType is Cons brtc && brtc.Cdr is Cons brtc2 &&
             brtc2.Car is Symbol betSym && betSym.Name is "BASE-CHAR" or "STANDARD-CHAR"))
        {
            var cs = Concatenate(Startup.Sym("STRING"), sequences);
            return MakeBaseString(cs is LispString cls ? cls.Value : ((LispVector)cs).ToCharString());
        }
        if (typeName == "STRING" || typeName == "SIMPLE-STRING" ||
            (typeName == "VECTOR" && resultType is Cons rtc && rtc.Cdr is Cons rtc2 &&
             rtc2.Car is Symbol etSym && etSym.Name is "CHARACTER"))
        {
            // Strings only -- which is what (concatenate 'string a b) almost always
            // is: the total length is known before copying, so the result can be
            // built once instead of growing a StringBuilder through its chunks.
            bool allStrings = true;
            int total = 0;
            foreach (var seq in sequences)
            {
                if (seq is LispString ss) total += ss.Value.Length;
                else { allStrings = false; break; }
            }
            if (allStrings)
            {
                if (sequences.Length == 0) return new LispString("");
                if (sequences.Length == 1) return new LispString(((LispString)sequences[0]).Value);
                if (sequences.Length == 2)
                    return new LispString(string.Concat(((LispString)sequences[0]).Value,
                                                        ((LispString)sequences[1]).Value));
                var buf = new char[total];
                int pos = 0;
                foreach (var seq in sequences)
                {
                    var v = ((LispString)seq).Value;
                    v.CopyTo(0, buf, pos, v.Length);
                    pos += v.Length;
                }
                return new LispString(new string(buf, 0, total));
            }

            var sb = new System.Text.StringBuilder();
            foreach (var seq in sequences)
            {
                if (seq is LispString s) sb.Append(s.Value);
                else if (seq is Nil) { }
                else if (seq is Cons)
                {
                    var cur = seq;
                    while (cur is Cons c)
                    {
                        if (c.Car is LispChar ch) sb.Append(ch.Value);
                        else throw new LispErrorException(new LispTypeError("CONCATENATE: not a character", c.Car));
                        cur = c.Cdr;
                    }
                    if (cur is not Nil)
                        throw new LispErrorException(new LispTypeError("CONCATENATE: not a proper list", seq, Startup.Sym("LIST")));
                }
                else if (seq is LispVector sv)
                    for (int i = 0; i < sv.Length; i++)
                    {
                        if (sv.ElementAt(i) is LispChar ch2) sb.Append(ch2.Value);
                        else throw new LispErrorException(new LispTypeError("CONCATENATE: not a character", sv.ElementAt(i)));
                    }
                else throw new LispErrorException(new LispTypeError("CONCATENATE: not a sequence", seq));
            }
            return new LispString(sb.ToString());
        }
        if (typeName == "SEQUENCE")
            throw new LispErrorException(new LispError("CONCATENATE: SEQUENCE is abstract and cannot be used as a result type"));
        if (typeName == "LIST" || typeName == "CONS")
        {
            // Cons the answer as the sequences are walked. Collecting into a List and
            // handing its array to List conses exactly these cells anyway, after
            // paying for the List, its backing store and the copy.
            Cons? head = null, tail = null;
            void Emit(LispObject x)
            {
                var cell = new Cons(x, Nil.Instance);
                if (tail == null) head = cell; else tail.Cdr = cell;
                tail = cell;
            }
            foreach (var seq in sequences)
            {
                if (seq is Nil) continue;
                else if (seq is Cons)
                {
                    var cur = seq;
                    for (; cur is Cons c; cur = c.Cdr) Emit(c.Car);
                    if (cur is not Nil)
                        throw new LispErrorException(new LispTypeError("CONCATENATE: not a proper list", seq, Startup.Sym("LIST")));
                }
                else if (seq is LispString cs) { foreach (char ch in cs.Value) Emit(LispChar.Make(ch)); }
                else if (seq is LispVector cv) { for (int i = 0; i < cv.Length; i++) Emit(cv.ElementAt(i)); }
                else throw new LispErrorException(new LispTypeError("CONCATENATE: not a sequence", seq));
            }
            return head ?? (LispObject)Nil.Instance;
        }
        if (typeName == "NULL")
        {
            // NULL concatenation: all sequences must be empty, result is nil
            foreach (var seq in sequences)
                if (!(seq is Nil) && !(seq is LispVector ev && ev.Length == 0) && !(seq is LispString es && es.Length == 0))
                    throw new LispErrorException(new LispTypeError("CONCATENATE: cannot coerce non-empty sequence to NULL", seq));
            return Nil.Instance;
        }
        if (typeName == "VECTOR" || typeName == "SIMPLE-VECTOR" || typeName == "ARRAY" || typeName == "SIMPLE-ARRAY")
        {
            int count = TotalSequenceLength(sequences);
            var items = new LispObject[count];
            FillSequenceElements(sequences, items);
            // For (simple-array element-type dims) or (vector element-type n): parse element type
            string elemTypeName = "T";
            if (resultType is Cons rtc0)
            {
                var etSpec = (rtc0.Cdr as Cons)?.Car;  // element-type arg
                if (etSpec != null && !(etSpec is T) && !(etSpec is Symbol wtSym && wtSym.Name == "*"))
                    elemTypeName = ParseElementTypeName(etSpec);
                int required = RequiredSequenceLength(rtc0, "CONCATENATE");
                if (required >= 0 && count != required)
                    throw new LispErrorException(new LispTypeError($"CONCATENATE: result has {count} elements, type requires {required}", resultType));
            }
            return new LispVector(items, elemTypeName);
        }
        // Try deftype expansion for compound or named sequence types
        if (resultType is Symbol typeAlias && TryGetTypeExpander(typeAlias, out var concatExpander))
        {
            var expanded = Funcall(concatExpander);
            if (expanded is not Symbol es || es.Name != typeAlias.Name)
                return Concatenate(expanded, sequences);
        }
        if (resultType is Cons compAlias && compAlias.Car is Symbol compHead
            && TryGetTypeExpander(compHead, out var compConcatExpander))
        {
            var compArgs = ToList(compAlias.Cdr).ToArray();
            var compExpanded = Funcall(compConcatExpander, compArgs);
            if (!ReferenceEquals(compExpanded, resultType))
                return Concatenate(compExpanded, sequences);
        }
        if (typeName == "BIT-VECTOR" || typeName == "SIMPLE-BIT-VECTOR")
        {
            var items = new List<LispObject>();
            foreach (var seq in sequences) CollectSequenceElements(seq, items);
            // Check compound size constraint: (bit-vector N)
            if (resultType is Cons bvc && bvc.Cdr is Cons bvc2 && bvc2.Car is Fixnum bsizeF)
                if (items.Count != (int)bsizeF.Value)
                    throw new LispErrorException(new LispTypeError($"CONCATENATE: result has {items.Count} elements, type requires {bsizeF.Value}", resultType));
            var arr = new LispObject[items.Count];
            for (int i = 0; i < items.Count; i++)
            {
                var elem = items[i];
                if (elem is Fixnum fi && (fi.Value == 0 || fi.Value == 1))
                    arr[i] = elem;
                else
                    throw new LispErrorException(new LispTypeError("CONCATENATE: not a bit", elem));
            }
            return new LispVector(arr, "BIT");
        }
        throw new LispErrorException(new LispTypeError($"CONCATENATE: not a sequence type: {resultType}", resultType, Startup.Sym("SEQUENCE")));
    }

    public static LispObject Sort(LispObject seq, LispObject predicate)
    {
        return SortImpl(seq, predicate, null);
    }

    public static LispObject StableSort(LispObject seq, LispObject predicate)
    {
        return SortImpl(seq, predicate, null, stable: true);
    }

    // 4-arg direct entry: (sort seq pred k v): the only keyword SORT takes is
    // :key, so the single pair is decoded here instead of running the general
    // scan over an args array. Anything else (an unknown keyword, or
    // :allow-other-keys paired with one) falls back to SortFull so there is a
    // single error path. Companion 2-arg entry is Sort.
    public static LispObject Sort4(LispObject seq, LispObject predicate, LispObject k, LispObject v) =>
        Sort4Core(seq, predicate, k, v, stable: false);

    public static LispObject StableSort4(LispObject seq, LispObject predicate, LispObject k, LispObject v) =>
        Sort4Core(seq, predicate, k, v, stable: true);

    private static LispObject Sort4Core(LispObject seq, LispObject predicate, LispObject k, LispObject v, bool stable)
    {
        if (k is Symbol ks)
        {
            switch (ks.Name)
            {
                case "KEY":
                    var karg = v;
                    if (karg is Symbol ksym && ksym.Function is LispFunction ksf) karg = ksf;
                    if (karg is LispFunction || karg is Nil)
                        return SortImpl(seq, predicate, karg as LispFunction, stable);
                    break;      // a symbol with no function or a non-designator: the full path reports it
                case "ALLOW-OTHER-KEYS":
                    return SortImpl(seq, predicate, null, stable);
            }
        }
        return SortFullCore(new[] { seq, predicate, k, v }, stable);
    }

    public static LispObject SortFull(LispObject[] args) => SortFullCore(args, stable: false);

    public static LispObject StableSortFull(LispObject[] args) => SortFullCore(args, stable: true);

    private static LispObject SortFullCore(LispObject[] args, bool stable)
    {
        // (sort seq predicate &key key)
        var name = stable ? "STABLE-SORT" : "SORT";
        var seq = args[0];
        var predicate = args[1];
        int keyArgCount = args.Length - 2;
        if (keyArgCount % 2 != 0)
            throw new LispErrorException(new LispProgramError($"{name}: odd number of keyword arguments"));
        bool allowOtherKeys = false, allowOtherKeysSeen = false;
        for (int i = 2; i < args.Length - 1; i += 2)
        {
            if (args[i] is not Symbol sk)
                throw new LispErrorException(new LispProgramError($"{name}: keyword must be a symbol, got {args[i]}"));
            if (!allowOtherKeysSeen && sk.Name == "ALLOW-OTHER-KEYS")
            {
                allowOtherKeys = args[i + 1] != Nil.Instance;
                allowOtherKeysSeen = true;
            }
        }
        LispFunction? keyFn = null;
        bool keySeen = false;
        for (int i = 2; i < args.Length - 1; i += 2)
        {
            if (args[i] is not Symbol ks)
                throw new LispErrorException(new LispProgramError($"{name}: keyword must be a symbol, got {args[i]}"));
            switch (ks.Name)
            {
                case "KEY":
                    if (!keySeen)
                    {
                        var karg = args[i + 1];
                        if (karg is Symbol ksym && ksym.Function is LispFunction ksf) karg = ksf;
                        if (karg is LispFunction kf) keyFn = kf;
                        else if (karg is not Nil && karg is not Symbol)
                            throw new LispErrorException(new LispTypeError(
                                $"{name}: :key is not a function designator: {FormatObject(karg, true)}",
                                karg, List(Startup.Sym("OR"), Startup.Sym("FUNCTION"), Startup.Sym("SYMBOL"))));
                        else if (karg is Symbol) keyFn = CoerceToFunctionFor(karg, name);
                        keySeen = true;
                    }
                    break;
                case "ALLOW-OTHER-KEYS": break;
                default:
                    if (!allowOtherKeys)
                        throw new LispErrorException(new LispProgramError($"{name}: unknown keyword :{ks.Name}"));
                    break;
            }
        }
        return SortImpl(seq, predicate, keyFn, stable);
    }

    // Sort ITEMS in place with a merge sort that asks one question of the
    // predicate: "does the element on the right go before the one on the left?".
    // Ties (the predicate false both ways) keep their original order, so the same
    // sort serves SORT and STABLE-SORT.
    //
    // Array.Sort needs a consistent three-way comparator, which a Lisp predicate
    // does not have to give: with a non-strict one such as #'>= or #'<= both
    // (p a b) and (p b a) are true for equal keys, the comparator derived from it
    // contradicted itself, and introsort gave up with an exception that was
    // swallowed, leaving the sequence partly or entirely unsorted. A merge sort
    // driven by the single question still produces a sorted result for such a
    // predicate, as SBCL's does.
    //
    // Keys are computed once per element rather than once per comparison.
    private static void SortObjects(LispObject[] items, LispFunction fn, LispFunction? keyFn, bool stable)
    {
        int n = items.Length;
        if (n < 2) return;
        LispObject[] keys = items;
        LispObject[] tmpItems = new LispObject[(n + 1) / 2];
        LispObject[] tmpKeys = tmpItems;
        if (keyFn != null)
        {
            keys = new LispObject[n];
            for (int i = 0; i < n; i++) keys[i] = ApplyKeyFn(keyFn, items[i]);
            tmpKeys = new LispObject[tmpItems.Length];
        }
        var top = LispFunction.CurrentTop;
        try { MergeSortRange(items, keys, tmpItems, tmpKeys, 0, n, fn); }
        finally { LispFunction.RestoreTop(top); }
    }

    private const int SortInsertionThreshold = 12;

    /// <summary>Sort [lo, hi) of ITEMS, moving KEYS in step (KEYS may be ITEMS itself).
    /// TMP arrays hold at least half the range.</summary>
    private static void MergeSortRange(LispObject[] items, LispObject[] keys,
        LispObject[] tmpItems, LispObject[] tmpKeys, int lo, int hi, LispFunction fn)
    {
        bool sep = !ReferenceEquals(keys, items);
        if (hi - lo <= SortInsertionThreshold)
        {
            for (int i = lo + 1; i < hi; i++)
            {
                var x = items[i];
                var kx = keys[i];
                int j = i;
                while (j > lo && IsTruthy(LispFunction.Call2(fn, kx, keys[j - 1])))
                {
                    SortStore(items, j, items[j - 1]);
                    if (sep) SortStore(keys, j, keys[j - 1]);
                    j--;
                }
                SortStore(items, j, x);
                if (sep) SortStore(keys, j, kx);
            }
            return;
        }
        int mid = lo + (hi - lo) / 2;
        MergeSortRange(items, keys, tmpItems, tmpKeys, lo, mid, fn);
        MergeSortRange(items, keys, tmpItems, tmpKeys, mid, hi, fn);
        // Already in order across the boundary: nothing to merge.
        if (!IsTruthy(LispFunction.Call2(fn, keys[mid], keys[mid - 1]))) return;
        int nl = mid - lo;
        Array.Copy(items, lo, tmpItems, 0, nl);
        if (sep) Array.Copy(keys, lo, tmpKeys, 0, nl);
        int l = 0, r = mid, d = lo;
        while (l < nl && r < hi)
        {
            if (IsTruthy(LispFunction.Call2(fn, keys[r], tmpKeys[l])))
            {
                SortStore(items, d, items[r]);
                if (sep) SortStore(keys, d, keys[r]);
                r++;
            }
            else
            {
                SortStore(items, d, tmpItems[l]);
                if (sep) SortStore(keys, d, tmpKeys[l]);
                l++;
            }
            d++;
        }
        while (l < nl)
        {
            SortStore(items, d, tmpItems[l]);
            if (sep) SortStore(keys, d, tmpKeys[l]);
            l++;
            d++;
        }
    }

    // A store into a LispObject[] of an element read from a LispObject[] needs
    // no covariance check; the checked store took the slow path for every
    // element whose class is not exactly LispObject.
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    private static void SortStore(LispObject[] a, int i, LispObject v)
    {
#if NET5_0_OR_GREATER
        if ((uint)i >= (uint)a.Length) throw new IndexOutOfRangeException();
        System.Runtime.CompilerServices.Unsafe.Add(
            ref System.Runtime.InteropServices.MemoryMarshal.GetArrayDataReference(a), i) = v;
#else
        a[i] = v;
#endif
    }

    private static LispObject SortImpl(LispObject seq, LispObject predicate, LispFunction? keyFn, bool stable = false)
        => MultipleValues.Alone(SortScan(seq, predicate, keyFn, stable));

    private static LispObject SortScan(LispObject seq, LispObject predicate, LispFunction? keyFn, bool stable)
    {
        seq = AsSequenceArg(seq);
        // Accept symbol as function designator (ANSI CL: function designator can be symbol or function)
        if (predicate is Symbol psym && psym.Function is LispFunction pf)
            predicate = pf;
        if (predicate is not LispFunction fn)
            throw new LispErrorException(new LispTypeError("SORT: predicate must be a function", predicate));
        if (seq is Nil) return Nil.Instance;
        if (seq is Cons)
        {
            // Count first, then fill: going through a List and ToArray copied the
            // elements twice before the comparator ran once.
            int n = 0;
            for (var p = seq; p is Cons pc; p = pc.Cdr) n++;
            var arr = new LispObject[n];
            {
                int i = 0;
                for (var p = seq; p is Cons pc; p = pc.Cdr) arr[i++] = pc.Car;
            }
            SortObjects(arr, fn, keyFn, stable);
            // ANSI lets SORT destroy the list it was given, and the vector and string
            // paths right below already write their result back in place. Reusing the
            // cells we just walked saves consing the whole list a second time, which
            // was the largest part of sorting a short list.
            {
                int i = 0;
                for (var p = seq; p is Cons pc; p = pc.Cdr) pc.Car = arr[i++];
            }
            return seq;
        }
        if (seq is LispString str)
        {
            var chars = str.Value.ToCharArray();
            var charObjs = new LispObject[chars.Length];
            for (int i = 0; i < chars.Length; i++) charObjs[i] = LispChar.Make(chars[i]);
            SortObjects(charObjs, fn, keyFn, stable);
            var sb = new System.Text.StringBuilder(charObjs.Length);
            foreach (var o in charObjs) if (o is LispChar lc) sb.Append(lc.Value);
            for (int i = 0; i < sb.Length; i++) str[i] = sb[i];
            return str;
        }
        if (seq is LispVector vec)
        {
            var items = new LispObject[vec.Length];
            for (int i = 0; i < vec.Length; i++) items[i] = vec.ElementAt(i);
            SortObjects(items, fn, keyFn, stable);
            for (int i = 0; i < items.Length; i++) vec.SetElement(i, items[i]);
            return vec;
        }
        throw new LispErrorException(new LispTypeError("SORT: not a sequence", seq));
    }

    public static LispObject Reverse(LispObject seq)
    {
        if (seq is Nil) return Nil.Instance;
        if (seq is Cons)
        {
            LispObject result = Nil.Instance;
            var cur = seq;
            while (cur is Cons c) { result = new Cons(c.Car, result); cur = c.Cdr; }
            if (cur is not Nil)
                throw new LispErrorException(new LispTypeError("REVERSE: not a proper list", cur, Startup.Sym("LIST")));
            return result;
        }
        if (seq is LispString s)
        {
#if NETSTANDARD2_0
            var chars = s.Value.ToCharArray();
            Array.Reverse(chars);
            return new LispString(new string(chars));
#else
            // One copy: the reversed characters are written straight into the new string.
            return new LispString(string.Create(s.Length, s, static (span, src) =>
            {
                src.Chars.CopyTo(span);
                span.Reverse();
            }));
#endif
        }
        if (seq is LispVector v)
        {
            // A vector that owns its storage: copy the storage as a block, then reverse the copy.
            if (v._displacedTo == null && v._dimensions == null
                && CopyVectorStorage(v) is LispVector copy && ReverseVectorStorage(copy))
                return copy;
            var items = new LispObject[v.Length];
            for (int i = 0; i < v.Length; i++) items[i] = v.ElementAt(v.Length - 1 - i);
            return new LispVector(items, v.ElementTypeName);
        }
        if (TryDotNetSequence(seq, out var dsnap)) return Reverse(dsnap);
        throw new LispErrorException(new LispTypeError("REVERSE: not a sequence", seq));
    }

    /// <summary>Reverses the active elements of V in place when V keeps them in a plain object array or a
    /// numeric backing (not displaced, not a bit vector, not multi-dimensional). False when it does not.</summary>
    internal static bool ReverseVectorStorage(LispVector v)
    {
        if (v._displacedTo != null || v._dimensions != null || v._bitData != null) return false;
        int n = v.Length;
        if (v._numData != null)
        {
            if (v._numData is long[] la) Array.Reverse(la, 0, n);
            else Array.Reverse(v._numData, 0, n);
            return true;
        }
        if (v._elements == null) return false;
        Array.Reverse(v._elements, 0, n);
        return true;
    }

    /// <summary>A list that is converted to another kind of sequence must be a proper list.</summary>
    private static void RequireProperListToCoerce(LispObject obj, LispObject resultType)
    {
        if (obj is not Cons || resultType is T) return;
        LispObject slow = obj, fast = obj;
        while (true)
        {
            if (fast is not Cons f1) break;
            fast = f1.Cdr;
            if (fast is not Cons f2) break;
            fast = f2.Cdr;
            slow = ((Cons)slow).Cdr;
            if (ReferenceEquals(fast, slow)) break;
        }
        if (fast is Nil) return;
        if (fast is Cons) return;  // circular: left to the conversion, as before
        throw new LispErrorException(new LispTypeError("COERCE: not a proper list", obj, Startup.Sym("LIST")));
    }

    private static Symbol? s_coerceDoubleSym, s_coerceSimpleStringSym;
    public static LispObject Coerce(LispObject obj, LispObject resultType)
    {
        // (coerce <fixnum> 'double-float): the integer-to-float step Coalton emits for every
        // integral->num, answered before the type-test and sequence machinery below.
        if (obj is Fixnum fxd && ReferenceEquals(resultType, s_coerceDoubleSym ??= Startup.Sym("DOUBLE-FLOAT")))
            return new DoubleFloat((double)fxd.Value);
        // (coerce <adjustable or fill-pointered string> 'simple-string): the copy the string
        // builders end with, answered before the type-test and sequence machinery below.
        if (obj is LispVector { Rank: 1, IsCharVector: true, IsSimple: false } cv
            && ReferenceEquals(resultType, s_coerceSimpleStringSym ??= Startup.Sym("SIMPLE-STRING")))
        {
            int n = cv.Length;
            var buf = new char[n];
            for (int i = 0; i < n; i++)
            {
                if (cv.ElementAt(i) is not LispChar ch) goto general;
                buf[i] = ch.Value;
            }
            return new LispString(buf);
        }
        general:
        if (resultType is Symbol same && ReferenceEquals(same.HomePackage, Startup.CL)
            && (obj is Fixnum || obj is Bignum ? same.Name == "INTEGER"
                : obj is SingleFloat ? same.Name == "SINGLE-FLOAT" : obj is DoubleFloat && same.Name == "DOUBLE-FLOAT"))
            return obj;
        if (resultType is Symbol rts && SymbolTypeTest(rts) is { } rtt && rtt.Test(obj))
            return obj;
        if (TryDotNetSequence(obj, out var dseq)) obj = dseq;
        RequireProperListToCoerce(obj, resultType);

        // Handle class objects as type specifiers
        if (resultType is LispClass lc)
            return Coerce(obj, lc.Name);

        // Handle compound type specifiers like (VECTOR *), (VECTOR * 2), (SIMPLE-ARRAY ...), etc.
        if (resultType is Cons compType && compType.Car is Symbol headSym)
        {
            // Per CLHS: if object already satisfies the type, return it as-is
            if (IsTruthy(Typep(obj, resultType))) return obj;
            RequireProperListToCoerce(obj, resultType);

            string head = headSym.Name;
            if (head is "VECTOR" or "SIMPLE-VECTOR" or "ARRAY" or "SIMPLE-ARRAY")
            {
                // OBJ is not of the type, so copy, even a non-simple vector of the right element type.
                LispObject? elemTypeSpec = head == "SIMPLE-VECTOR" ? T.Instance : (compType.Cdr as Cons)?.Car;
                string elemTypeName = elemTypeSpec == null || elemTypeSpec is T || elemTypeSpec is Symbol { Name: "*" }
                    ? "T"
                    : ParseElementTypeName(elemTypeSpec);
                int expectedLen = RequiredSequenceLength(compType, "COERCE");

                LispObject result;
                if (elemTypeName is "BASE-CHAR")
                    result = Coerce(obj, Startup.Sym("SIMPLE-BASE-STRING"));
                else if (elemTypeName is "CHARACTER")
                    // A base string is a SIMPLE-STRING too, but not of element type CHARACTER.
                    result = obj is LispVector { IsBaseCharVector: true } bcv
                        ? new LispString(bcv.ToCharString())
                        : Coerce(obj, Startup.Sym("SIMPLE-STRING"));
                else
                {
                    LispObject[] items = obj switch
                    {
                        Nil => Array.Empty<LispObject>(),
                        Cons => ListToArray(obj),
                        LispString str => str.Value.Select(c => (LispObject)LispChar.Make(c)).ToArray(),
                        LispVector { Rank: 1 } vec => Enumerable.Range(0, vec.Length).Select(vec.ElementAt).ToArray(),
                        _ => throw new LispErrorException(new LispTypeError($"COERCE: cannot coerce to {resultType}", obj, resultType)),
                    };
                    // Packed storage reads NIL as an unset slot (0), so it would be stored silently.
                    if (LispVector.NumKindForElementType(elemTypeName) != 0)
                        foreach (var item in items)
                            if (item is Nil)
                                throw new LispErrorException(new LispTypeError(
                                    $"COERCE: NIL is not of element type {elemTypeName}", item, elemTypeSpec));
                    result = new LispVector(items, elemTypeName);
                }
                if (expectedLen >= 0)
                {
                    int actualLen = result is LispString rs ? rs.Length : ((LispVector)result).Length;
                    if (actualLen != expectedLen)
                        throw new LispErrorException(new LispTypeError($"COERCE: result length {actualLen} does not match required length {expectedLen}", Fixnum.Make(actualLen), new Cons(Startup.Sym("EQL"), new Cons(Fixnum.Make(expectedLen), Nil.Instance))));
                }
                return result;
            }
            if (head is "STRING" or "SIMPLE-STRING" or "BASE-STRING" or "SIMPLE-BASE-STRING"
                     or "BIT-VECTOR" or "SIMPLE-BIT-VECTOR")
            {
                var result = Coerce(obj, headSym);
                if ((compType.Cdr as Cons)?.Car is Fixnum want)
                {
                    int actualLen = result is LispString rs ? rs.Length : ((LispVector)result).Length;
                    if (actualLen != want.Value)
                        throw new LispErrorException(new LispTypeError($"COERCE: result length {actualLen} does not match required length {want.Value}", Fixnum.Make(actualLen), new Cons(Startup.Sym("EQL"), new Cons(Fixnum.Make(want.Value), Nil.Instance))));
                }
                return result;
            }
            // Try compound deftype expansion: (my-type args...) -> expanded type
            if (TryGetTypeExpander(headSym, out var compExpander))
            {
                var compArgs = ToList(compType.Cdr).ToArray();
                var compExpanded = Funcall(compExpander, compArgs);
                if (!ReferenceEquals(compExpanded, resultType))
                    return Coerce(obj, compExpanded);
            }
            if (head is "LIST") return Coerce(obj, Startup.Sym("LIST"));
            if (head is "COMPLEX")
            {
                var rest1 = compType.Cdr as Cons;
                var partType = rest1?.Car;
                var num = AsNumber(obj);
                Number real, imag;
                if (num is LispComplex cx) { real = cx.Real; imag = cx.Imaginary; }
                else { real = num; imag = Fixnum.Make(0); }
                // Coerce each part through Coerce(part, partType) rather than matching
                // a literal float name, so a deftype part: e.g. (complex flonum), where
                // flonum is a deftype expanding to double-float: is expanded and the
                // parts converted. A wildcard/absent part type leaves the parts as-is.
                if (partType is not null && partType is not Nil
                    && !(partType is Symbol wsym && wsym.Name == "*"))
                {
                    real = AsNumber(Coerce(real, partType));
                    imag = AsNumber(Coerce(imag, partType));
                }
                // The parts of a float complex share one float format: a float real part with
                // a rational imaginary part, or a single-float one with a double-float one,
                // is brought to a common format.
                if (real is SingleFloat or DoubleFloat || imag is SingleFloat or DoubleFloat)
                {
                    bool dbl = real is DoubleFloat || imag is DoubleFloat;
                    if (dbl)
                    {
                        if (real is not DoubleFloat) real = new DoubleFloat(Arithmetic.ToDoubleChecked(real, "COERCE"));
                        if (imag is not DoubleFloat) imag = new DoubleFloat(Arithmetic.ToDoubleChecked(imag, "COERCE"));
                    }
                    else
                    {
                        if (real is not SingleFloat) real = new SingleFloat(Arithmetic.ToSingleChecked(real, "COERCE"));
                        if (imag is not SingleFloat) imag = new SingleFloat(Arithmetic.ToSingleChecked(imag, "COERCE"));
                    }
                }
                // A complex with a rational zero imaginary part is the real itself.
                if (imag is Fixnum { Value: 0 } && real is not (SingleFloat or DoubleFloat)) return real;
                return LispComplex.Of(real, imag);
            }
            // Compound float types: e.g. (DOUBLE-FLOAT low high), which a deftype
            // like Maxima's FLONUM expands to ((&optional low high) -> (double-float
            // * *), since deftype optionals default to * not nil). Range bounds are
            // irrelevant to the coercion target; coerce to the base float type
            // (matches SBCL: (coerce 2/3 '(double-float 0d0 1d0)) => 0.666d0).
            if (head is "DOUBLE-FLOAT" or "LONG-FLOAT" or "SINGLE-FLOAT"
                     or "SHORT-FLOAT" or "FLOAT")
                return Coerce(obj, headSym);
        }

        string typeName = resultType switch
        {
            Symbol sym => sym.Name,
            T => "T",
            _ => ""
        };

        switch (typeName)
        {
            case "LIST":
                if (obj is Nil || obj is Cons) return obj;
                // Cons the result straight from the source. Filling an array and
                // handing it to List conses exactly the same cells and then drops
                // the array, which for a short sequence is most of the cost.
                if (obj is LispString s)
                {
                    LispObject sacc = Nil.Instance;
                    for (int i = s.Length - 1; i >= 0; i--) sacc = new Cons(LispChar.Make(s[i]), sacc);
                    return sacc;
                }
                if (obj is LispVector { Rank: 1 } lv)
                {
                    LispObject vacc = Nil.Instance;
                    for (int i3 = lv.Length - 1; i3 >= 0; i3--) vacc = new Cons(lv.ElementAt(i3), vacc);
                    return vacc;
                }
                throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to list", obj));

            case "BASE-STRING": case "SIMPLE-BASE-STRING":
                if (obj is LispVector bsv0 && bsv0.IsBaseCharVector && bsv0.Rank == 1
                    && (typeName == "BASE-STRING" || bsv0.IsSimple))
                    return obj;
                {
                    if (BaseStringFromSequence(obj) is LispVector bs) return bs;
                    var cs0 = Coerce(obj, Startup.Sym("SIMPLE-STRING"));
                    return MakeBaseString(cs0 is LispString cls ? cls.Value : ((LispVector)cs0).ToCharString());
                }
            case "STRING": case "SIMPLE-STRING":
                if (obj is LispString) return obj;
                if (obj is Nil) return new LispString("");  // empty list -> empty string
                if (obj is Cons)
                {
                    int count = 0;
                    for (var cur = obj; cur is Cons c2; cur = c2.Cdr) count++;
                    var chars = new char[count];
                    int i = 0;
                    for (var cur = obj; cur is Cons c2; cur = c2.Cdr)
                    {
                        if (c2.Car is LispChar lch) chars[i++] = lch.Value;
                        else throw new LispErrorException(new LispTypeError("COERCE: list element not a character", c2.Car, Startup.Sym("CHARACTER")));
                    }
                    return new LispString(chars);
                }
                if (obj is LispVector { Rank: 1 } vec)
                {
                    // For a SIMPLE-STRING target the result must be a *simple* string
                    // (CLHS): a fill-pointered / adjustable / displaced char-vector must
                    // be copied to a fresh simple string, not returned as-is. Returning
                    // the non-simple vector broke code that (coerce ... 'simple-string)s
                    // and then relies on simplicity: e.g. cl-ppcre's
                    // maybe-coerce-to-simple-string on parser-built adjustable strings.
                    bool wantSimple = typeName is "SIMPLE-STRING";
                    // A char-vector already satisfies STRING. For a SIMPLE
                    // target return it as-is only if it is already simple (the
                    // LispVector.IsSimple that CheckSimpleType's SIMPLE-STRING reads,
                    // plus rank 1): CLHS: coerce returns the object itself when it is
                    // already of the type. Otherwise fall through and copy.
                    if (vec.IsCharVector
                        && (!wantSimple || vec.IsSimple))
                        return obj;
                    var sb = new System.Text.StringBuilder(vec.Length);
                    for (int i = 0; i < vec.Length; i++)
                    {
                        if (vec.ElementAt(i) is LispChar lch) sb.Append(lch.Value);
                        else throw new LispErrorException(new LispTypeError("COERCE: vector element not a character", vec.ElementAt(i), Startup.Sym("CHARACTER")));
                    }
                    return new LispString(sb.ToString());
                }
                throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to string", obj));

            // CONS and NULL name lists: the sequence becomes a list, which must be non-empty / empty.
            case "CONS":
            {
                var asList = Coerce(obj, Startup.Sym("LIST"));
                if (asList is Cons) return asList;
                throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to cons", obj, resultType));
            }
            case "NULL":
            {
                var asList = Coerce(obj, Startup.Sym("LIST"));
                if (asList is Nil) return asList;
                throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to null", obj, resultType));
            }

            case "CHARACTER":
                if (obj is T) return LispChar.Make('T');
                if (obj is LispChar) return obj;
                if (obj is LispString cs && cs.Length == 1)
                    return LispChar.Make(cs.Value[0]);
                if (obj is Symbol charSym && charSym.Name.Length == 1)
                    return LispChar.Make(charSym.Name[0]);
                throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to character", obj));

            case "COMPLEX":
                if (obj is LispComplex) return obj;
                if (obj is Number num)
                    return Arithmetic.MakeComplexPublic(num, Fixnum.Make(0));
                throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to complex", obj));

            case "FLOAT": case "SINGLE-FLOAT": case "SHORT-FLOAT":
                if (obj is SingleFloat) return obj;
                if (obj is LispComplex)
                    throw new LispErrorException(new LispTypeError("COERCE: cannot coerce a complex to a float", obj, resultType));
                if (obj is Number nf) return new SingleFloat(Arithmetic.ToSingleChecked(nf, "COERCE"));
                throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to float", obj, Startup.Sym("REAL")));

            case "DOUBLE-FLOAT": case "LONG-FLOAT":
                if (obj is DoubleFloat) return obj;
                if (obj is LispComplex)
                    throw new LispErrorException(new LispTypeError("COERCE: cannot coerce a complex to a double-float", obj, resultType));
                if (obj is Number nd) return new DoubleFloat(Arithmetic.ToDoubleChecked(nd, "COERCE"));
                throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to double-float", obj));

            case "DECIMAL":
                // dotcl:decimal: a distinct exactness category. Exact reals (integer,
                // ratio) convert exactly or signal (a denominator with a prime factor
                // other than 2/5, e.g. 1/3, is not representable: no silent rounding).
                // A float is the explicit escape hatch out of the decimal/float mixing
                // ban, so coercing one imports its value via the .NET decimal cast (lossy
                // by nature of the source, ~15 significant digits).
                if (obj is LispDecimal) return obj;
                if (obj is Fixnum dfx) return new LispDecimal((decimal)dfx.Value);
                if (obj is Bignum dbn)
                {
                    if (dbn.Value < DecimalMinInt || dbn.Value > DecimalMaxInt)
                        throw new LispErrorException(new LispTypeError("COERCE: value out of System.Decimal range", obj));
                    return new LispDecimal((decimal)dbn.Value);
                }
                if (obj is Ratio drt)
                    return new LispDecimal(RationalToDecimalExact(drt.Numerator, drt.Denominator, obj));
                if (obj is SingleFloat dsf)
                {
                    try { return new LispDecimal((decimal)dsf.Value); }
                    catch (OverflowException) { throw new LispErrorException(new LispTypeError("COERCE: value not representable as System.Decimal", obj)); }
                }
                if (obj is DoubleFloat ddf)
                {
                    try { return new LispDecimal((decimal)ddf.Value); }
                    catch (OverflowException) { throw new LispErrorException(new LispTypeError("COERCE: value not representable as System.Decimal", obj)); }
                }
                throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to decimal", obj));

            case "VECTOR":
                // Only a rank-1 array is a sequence, so only it can become a vector.
                if ((obj is LispVector v && v.Rank == 1) || obj is LispString) return obj;
                if (obj is Nil) return new LispVector(Array.Empty<LispObject>());
                if (obj is Cons) return new LispVector(ListToArray(obj));
                throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to vector", obj));

            case "SIMPLE-VECTOR":
                // Already a simple T-element-type vector? return as-is. A
                // fill-pointered / adjustable / displaced one is not a SIMPLE-VECTOR
                // and is copied below, as the SIMPLE-STRING case above does.
                if (obj is LispVector sv2 && sv2.Rank == 1 && sv2.IsSimple
                    && (sv2.ElementTypeName == null || sv2.ElementTypeName == "T"))
                    return obj;
                if (obj is Nil) return new LispVector(Array.Empty<LispObject>());
                if (obj is Cons) return new LispVector(ListToArray(obj));
                if (obj is LispString vs)
                {
                    var items = new LispObject[vs.Length];
                    for (int i = 0; i < vs.Length; i++)
                        items[i] = LispChar.Make(vs.Value[i]);
                    return new LispVector(items);
                }
                // Convert non-T vector (bit-vector etc.) to T-element-type vector
                if (obj is LispVector { Rank: 1 } sv3)
                {
                    var items2 = new LispObject[sv3.Length];
                    for (int i = 0; i < sv3.Length; i++) items2[i] = sv3.ElementAt(i);
                    return new LispVector(items2);
                }
                throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to vector", obj));

            case "BIT-VECTOR": case "SIMPLE-BIT-VECTOR":
                if (obj is LispVector bv && bv.Rank == 1 && bv.IsBitVector
                    && (typeName == "BIT-VECTOR" || bv.IsSimple))
                    return obj;
                {
                    if (obj is Nil || obj is LispString { Length: 0 }) return new LispVector(Array.Empty<LispObject>(), "BIT");
                    if (obj is Cons) return new LispVector(ListToArray(obj), "BIT");
                    if (obj is LispVector { Rank: 1 } sv)
                    {
                        var bitItems = new LispObject[sv.Length];
                        for (int i2 = 0; i2 < sv.Length; i2++) bitItems[i2] = sv.ElementAt(i2);
                        return new LispVector(bitItems, "BIT");
                    }
                    if (obj is LispString bstr)
                        throw new LispErrorException(new LispTypeError("COERCE: element not a bit", LispChar.Make(bstr[0]), Startup.Sym("BIT")));
                    throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to bit-vector", obj));
                }

            case "FUNCTION":
                if (obj is LispFunction) return obj;
                if (obj is Symbol funcSym)
                {
                    if (funcSym.Function is LispFunction sfn) return sfn;
                    try { return DotCL.Emitter.CilAssembler.GetFunction(funcSym.Name); }
                    catch (LispErrorException) { }
                    throw new LispErrorException(new LispError($"COERCE: no function bound to {funcSym.Name}"));
                }
                // Coerce a lambda-form (lambda ...) to function
                if (obj is Cons lambdaCons && lambdaCons.Car is Symbol lambdaSym && lambdaSym.Name == "LAMBDA"
                    && lambdaCons.Cdr is Cons lambdaRest && (lambdaRest.Car is Nil || lambdaRest.Car is Cons))
                    return Eval(obj);
                throw new LispErrorException(new LispTypeError($"COERCE: cannot coerce to function", obj));

            case "T":
                return obj;

            default:
                // If already of the target type, return as-is
                if (IsTruthy(Typep(obj, resultType))) return obj;
                // Try deftype expansion for named type aliases
                if (resultType is Symbol aliasSym && TryGetTypeExpander(aliasSym, out var symExpander))
                {
                    var symExpanded = Funcall(symExpander);
                    if (symExpanded is not Symbol se || se.Name != typeName)
                        return Coerce(obj, symExpanded);
                }
                throw new LispErrorException(new LispTypeError($"COERCE: cannot coerce to {typeName}", obj, resultType));
        }
    }

    private enum SeqTestKind { Eql, Eq, Equal, NumEq, CharEq, General }

    // Helper: parse common sequence keyword args (test, test-not, key, start, end, from-end, count)
    private struct SeqKwArgs
    {
        public LispFunction? Test, TestNot, Key;
        public int Start;
        public int? End;
        public bool FromEnd;
        public int? Count; // null = no limit
        public SeqTestKind TestKind;
    }

    private struct SeqKwSeen { public bool Test, TestNot, Key, Start, End, FromEnd, Count, Unknown, IfForm; }

    private static void ApplySeqKw(ref SeqKwArgs kw, ref SeqKwSeen seen,
                                   LispObject key, LispObject val, string fnName)
    {
        if (key is not Symbol s)
            throw new LispErrorException(new LispProgramError($"{fnName}: keyword must be a symbol, got {key}"));
        switch (s.Name)
        {
            case "TEST":
                if (seen.IfForm) { seen.Unknown = true; break; }
                if (!seen.Test) { if (val is not Nil) kw.Test = CoerceToFunctionFor(val, fnName); seen.Test = true; }
                break;
            case "TEST-NOT":
                if (seen.IfForm) { seen.Unknown = true; break; }
                if (!seen.TestNot) { if (val is not Nil) kw.TestNot = CoerceToFunctionFor(val, fnName); seen.TestNot = true; }
                break;
            case "KEY": if (!seen.Key) { if (val is not Nil) kw.Key = SeqKeyFunction(val, fnName); seen.Key = true; } break;
            case "START":
                if (!seen.Start) { kw.Start = SeqBoundIndex(val, fnName) ?? 0; seen.Start = true; }
                break;
            case "END":
                if (!seen.End) { kw.End = SeqBoundIndex(val, fnName, true); seen.End = true; }
                break;
            case "FROM-END":
                if (!seen.FromEnd) { kw.FromEnd = IsTruthy(val); seen.FromEnd = true; }
                break;
            case "COUNT" when SeqFunctionHasNoCountKey(fnName):
                seen.Unknown = true;
                break;
            case "COUNT":
                if (!seen.Count)
                {
                    if (val is Nil) { }
                    else if (val is Fixnum cf) { kw.Count = (int)Math.Max(Math.Min(cf.Value, int.MaxValue), int.MinValue); }
                    else if (val is Bignum bg) { kw.Count = bg.Value.Sign < 0 ? int.MinValue : int.MaxValue; }
                    else throw new LispErrorException(new LispTypeError($"{fnName}: :count must be integer or nil", val,
                        List(Startup.Sym("OR"), Startup.Sym("NULL"), Startup.Sym("INTEGER"))));
                    seen.Count = true;
                }
                break;
            case "ALLOW-OTHER-KEYS": break;
            default: seen.Unknown = true; break;
        }
    }

    /// <summary>The sequence functions that take no :COUNT: it is a key of REMOVE, DELETE and SUBSTITUTE only.</summary>
    private static bool SeqFunctionHasNoCountKey(string fnName) => fnName is
        "FIND" or "FIND-IF" or "FIND-IF-NOT" or "POSITION" or "POSITION-IF" or "POSITION-IF-NOT"
        or "COUNT" or "COUNT-IF" or "COUNT-IF-NOT" or "REMOVE-DUPLICATES" or "DELETE-DUPLICATES";

    private static bool? AllowOtherKeysValue(LispObject key, LispObject val) =>
        key is Symbol s && s.Name == "ALLOW-OTHER-KEYS" ? IsTruthy(val) : null;

    /// <summary>A :START / :END value as an int, or null for NIL. An integer outside the int range is
    /// clamped to the nearest end of it (int.MaxValue / -1), never wrapped, so the bounding-index
    /// check that follows reports it ((find 2 list :end (expt 2 32)) was read as 0).</summary>
    private static int? SeqBoundIndex(LispObject val, string fnName, bool isEnd = false)
    {
        switch (val)
        {
            case Nil: return null;
            case Fixnum { Value: < 0 } when isEnd:
            case Bignum { Value.Sign: < 0 } when isEnd:
                throw new LispErrorException(new LispTypeError($"{fnName}: end index {FormatObject(val, true)} is negative", val, EndIndexType()));
            case Fixnum f: return f.Value > int.MaxValue ? int.MaxValue : f.Value < 0 ? -1 : (int)f.Value;
            case Bignum b: return b.Value.Sign > 0 ? int.MaxValue : -1;
            default:
                throw new LispErrorException(new LispTypeError($"{fnName}: invalid index", val, isEnd ? EndIndexType() : Startup.Sym("UNSIGNED-BYTE")));
        }
    }

    private static SeqKwArgs ParseSeqKwArgs(LispObject[] args, int kwStart, string fnName, bool ifForm = false)
    {
        var kw = new SeqKwArgs();
        if ((args.Length - kwStart) % 2 != 0)
            throw new LispErrorException(new LispProgramError($"{fnName}: odd number of keyword arguments"));
        bool? allowOtherKeys = null;
        for (int i = kwStart; i < args.Length - 1 && allowOtherKeys == null; i += 2)
            allowOtherKeys = AllowOtherKeysValue(args[i], args[i + 1]);
        var seen = new SeqKwSeen { IfForm = ifForm };
        for (int i = kwStart; i < args.Length - 1; i += 2)
            ApplySeqKw(ref kw, ref seen, args[i], args[i + 1], fnName);
        if (seen.Unknown && allowOtherKeys != true)
            throw new LispErrorException(new LispProgramError($"{fnName}: unknown keyword argument"));
        FinishSeqTestKind(ref kw, fnName);
        return kw;
    }

    private static SeqKwArgs ParseSeqKwPairs(LispObject k1, LispObject v1, string fnName, bool ifForm = false)
    {
        var kw = new SeqKwArgs();
        var seen = new SeqKwSeen { IfForm = ifForm };
        ApplySeqKw(ref kw, ref seen, k1, v1, fnName);
        if (seen.Unknown && AllowOtherKeysValue(k1, v1) != true)
            throw new LispErrorException(new LispProgramError($"{fnName}: unknown keyword argument"));
        FinishSeqTestKind(ref kw, fnName);
        return kw;
    }

    private static SeqKwArgs ParseSeqKwPairs(LispObject k1, LispObject v1,
                                             LispObject k2, LispObject v2, string fnName,
                                             bool ifForm = false)
    {
        var kw = new SeqKwArgs();
        var seen = new SeqKwSeen { IfForm = ifForm };
        ApplySeqKw(ref kw, ref seen, k1, v1, fnName);
        ApplySeqKw(ref kw, ref seen, k2, v2, fnName);
        if (seen.Unknown && (AllowOtherKeysValue(k1, v1) ?? AllowOtherKeysValue(k2, v2)) != true)
            throw new LispErrorException(new LispProgramError($"{fnName}: unknown keyword argument"));
        FinishSeqTestKind(ref kw, fnName);
        return kw;
    }

    private static SeqKwArgs ParseSeqKwPairs(LispObject k1, LispObject v1,
                                             LispObject k2, LispObject v2,
                                             LispObject k3, LispObject v3, string fnName,
                                             bool ifForm = false)
    {
        var kw = new SeqKwArgs();
        var seen = new SeqKwSeen { IfForm = ifForm };
        ApplySeqKw(ref kw, ref seen, k1, v1, fnName);
        ApplySeqKw(ref kw, ref seen, k2, v2, fnName);
        ApplySeqKw(ref kw, ref seen, k3, v3, fnName);
        if (seen.Unknown && (AllowOtherKeysValue(k1, v1) ?? AllowOtherKeysValue(k2, v2)
                             ?? AllowOtherKeysValue(k3, v3)) != true)
            throw new LispErrorException(new LispProgramError($"{fnName}: unknown keyword argument"));
        FinishSeqTestKind(ref kw, fnName);
        return kw;
    }

    static LispObject? s_eqlFn, s_eqFn, s_equalFn, s_numEqFn, s_charEqFn, s_equalpFn;

    /// <summary>The hash-table test name that stands for KW's test (EQL when none, or EQL,
    /// EQUAL, EQUALP), or null when the test is anything else (including :TEST-NOT) and
    /// elements have to be compared pairwise.</summary>
    private static string? HashableSeqTest(in SeqKwArgs kw)
    {
        if (kw.TestNot != null) return null;
        var t = kw.Test;
        if (t == null) return "EQL";
        if (ReferenceEquals(t, SeqStandardFunction(ref s_eqlFn, "EQL"))) return "EQL";
        if (ReferenceEquals(t, SeqStandardFunction(ref s_equalFn, "EQUAL"))) return "EQUAL";
        if (ReferenceEquals(t, SeqStandardFunction(ref s_equalpFn, "EQUALP"))) return "EQUALP";
        return null;
    }
    static LispObject? s_minuspFn, s_zeropFn, s_pluspFn, s_evenpFn, s_oddpFn;

    private static LispObject? SeqStandardFunction(ref LispObject? cache, string name)
    {
        cache ??= Startup.CL.FindSymbol(name).symbol?.Function;
        return cache;
    }

    /// <summary>CLHS 17.2.1: giving both :TEST and :TEST-NOT is an error (SIMPLE-ERROR, as in SBCL).
    /// A NIL value counts as not given, as it always did here.</summary>
    internal static LispErrorException TestAndTestNotError(string fnName)
    {
        var err = new LispError($"{fnName}: both :TEST and :TEST-NOT were supplied");
        err.ConditionTypeName = "SIMPLE-ERROR";
        return new LispErrorException(err);
    }

    private static void FinishSeqTestKind(ref SeqKwArgs kw, string fnName)
    {
        if (kw.Test != null && kw.TestNot != null) throw TestAndTestNotError(fnName);
        if (kw.TestNot != null || kw.Key != null) { kw.TestKind = SeqTestKind.General; return; }
        var t = kw.Test;
        if (t == null) { kw.TestKind = SeqTestKind.Eql; return; }
        if (ReferenceEquals(t, SeqStandardFunction(ref s_eqlFn, "EQL"))) kw.TestKind = SeqTestKind.Eql;
        else if (ReferenceEquals(t, SeqStandardFunction(ref s_eqFn, "EQ"))) kw.TestKind = SeqTestKind.Eq;
        else if (ReferenceEquals(t, SeqStandardFunction(ref s_equalFn, "EQUAL"))) kw.TestKind = SeqTestKind.Equal;
        else if (ReferenceEquals(t, SeqStandardFunction(ref s_numEqFn, "="))) kw.TestKind = SeqTestKind.NumEq;
        else if (ReferenceEquals(t, SeqStandardFunction(ref s_charEqFn, "CHAR="))) kw.TestKind = SeqTestKind.CharEq;
        else kw.TestKind = SeqTestKind.General;
    }

    private enum SeqPredKind { General, MinusP, ZeroP, PlusP, EvenP, OddP }

    private static SeqPredKind SeqPredKindOf(in SeqKwArgs kw, LispFunction predFn)
    {
        if (kw.Key != null) return SeqPredKind.General;
        if (ReferenceEquals(predFn, SeqStandardFunction(ref s_minuspFn, "MINUSP"))) return SeqPredKind.MinusP;
        if (ReferenceEquals(predFn, SeqStandardFunction(ref s_zeropFn, "ZEROP"))) return SeqPredKind.ZeroP;
        if (ReferenceEquals(predFn, SeqStandardFunction(ref s_pluspFn, "PLUSP"))) return SeqPredKind.PlusP;
        if (ReferenceEquals(predFn, SeqStandardFunction(ref s_evenpFn, "EVENP"))) return SeqPredKind.EvenP;
        if (ReferenceEquals(predFn, SeqStandardFunction(ref s_oddpFn, "ODDP"))) return SeqPredKind.OddP;
        return SeqPredKind.General;
    }

    /// <summary>ITEM is a fixnum and the keywords ask for the plain EQL or = test (no :key, no :test-not):
    /// an integer array can then be searched without boxing its elements.</summary>
    private static bool IsPlainFixnumQuery(LispObject item, in SeqKwArgs kw, out long value)
    {
        value = 0;
        if (kw.Key != null || kw.TestNot != null) return false;
        if (kw.TestKind != SeqTestKind.Eql && kw.TestKind != SeqTestKind.NumEq) return false;
        if (UnwrapMv(item) is not Fixnum f) return false;
        value = f.Value;
        return true;
    }

    private static int SignOpOf(SeqPredKind kind) => kind switch
    {
        SeqPredKind.MinusP => 1, SeqPredKind.ZeroP => 0, SeqPredKind.PlusP => 2, _ => -1,
    };

    /// <summary>MAP with one sequence (a list, a vector or a string), a function object, and a result type
    /// of NIL, LIST, VECTOR or SIMPLE-VECTOR: the walk and the result are built here, with no
    /// intermediate list. Answers the keyword :%NO when the call has another shape, and the caller
    /// takes the general route; nothing has been called by then.</summary>
    private static LispObject MapElem(LispObject seq, int i)
        => seq is LispVector v ? v[i] : LispChar.Make(((LispString)seq)[i]);

    private const int MapKindNil = 0, MapKindList = 1, MapKindVector = 2, MapKindString = 3;

    /// <summary>The result types the fast MAP walks build: NIL, LIST, VECTOR,
    /// STRING and a one-dimensional array type whose dimension is *.</summary>
    private static bool MapResultShape(LispObject resultType, out int kind, out string elemType)
    {
        kind = MapKindNil;
        elemType = "T";
        if (resultType is Nil) return true;
        if (resultType is Symbol rs && rs.HomePackage == Startup.CL)
        {
            switch (rs.Name)
            {
                case "LIST": kind = MapKindList; return true;
                case "VECTOR": case "SIMPLE-VECTOR": kind = MapKindVector; return true;
                case "STRING": case "SIMPLE-STRING": kind = MapKindString; return true;
                default: return false;
            }
        }
        if (resultType is Cons c && c.Car is Symbol head && head.HomePackage == Startup.CL
            && head.Name is "VECTOR" or "SIMPLE-VECTOR" or "ARRAY" or "SIMPLE-ARRAY"
            && c.Cdr is Cons spec)
        {
            if (spec.Cdr is Cons dims)
            {
                // (simple-array etype (N)) / (vector etype N) constrain the length;
                // only the unconstrained * spelling takes the direct walk.
                if (dims.Car is not Cons star || star.Car is not Symbol { Name: "*" } || star.Cdr is not Nil)
                    return false;
            }
            var et = spec.Car;
            elemType = et is T || (et is Symbol es && es.Name == "*") ? "T" : ParseElementTypeName(et);
            // (vector character) is a string to MAP, not a character vector.
            if (elemType is "CHARACTER" or "BASE-CHAR" or "STANDARD-CHAR") return false;
            kind = MapKindVector;
            return true;
        }
        return false;
    }

    private static int CountListCells(LispObject list)
    {
        int n = 0;
        for (var cur = list; cur is Cons c; cur = c.Cdr) n++;
        return n;
    }

    private static LispObject MapCharResult(ReadOnlySpan<char> chars) => new LispString(chars.ToString());

        // Kept out of Map1Fast: a method with a stackalloc has a heavier prolog on every call.
    private static LispObject MapListToString(LispFunction fn, LispObject seq, int n)
    {
        Span<char> chars = n <= 512 ? stackalloc char[n] : new char[n];
        var cur = seq;
        int i = 0;
        for (; cur is Cons c; cur = c.Cdr)
        {
            var v = UnwrapMv(fn.Invoke1(c.Car));
            if (v is not LispChar lc)
                throw new LispErrorException(new LispTypeError("COERCE: list element not a character", v));
            chars[i++] = lc.Value;
        }
        if (cur is not Nil) throw new LispErrorException(new LispTypeError("MAP: not a proper list", cur, Startup.Sym("LIST")));
        return MapCharResult(chars);
    }

    private static LispObject MapSeqToString(LispFunction fn, LispObject seq, int len)
    {
        Span<char> chars = len <= 512 ? stackalloc char[len] : new char[len];
        for (int i = 0; i < len; i++)
        {
            var v = UnwrapMv(fn.Invoke1(MapElem(seq, i)));
            if (v is not LispChar lc)
                throw new LispErrorException(new LispTypeError("COERCE: list element not a character", v));
            chars[i] = lc.Value;
        }
        return MapCharResult(chars);
    }

public static LispObject Map1Fast(LispObject resultType, LispObject function, LispObject seq)
    {
        if (function is not LispFunction fn) return Startup.Keyword("%NO");
        if (!MapResultShape(resultType, out int kind, out string elemType)) return Startup.Keyword("%NO");

        if (seq is Cons || seq is Nil)
        {
            if (kind == MapKindNil)
            {
                var c0 = seq;
                for (; c0 is Cons cc; c0 = cc.Cdr) fn.Invoke1(cc.Car);
                if (c0 is not Nil) throw new LispErrorException(new LispTypeError("MAP: not a proper list", c0, Startup.Sym("LIST")));
                MultipleValues.Reset();
                return Nil.Instance;
            }
            if (kind == MapKindList)
            {
                Cons? head = null, tail = null;
                var cur = seq;
                for (; cur is Cons c; cur = c.Cdr)
                {
                    var cell = new Cons(UnwrapMv(fn.Invoke1(c.Car)), Nil.Instance);
                    if (tail == null) head = cell; else tail.Cdr = cell;
                    tail = cell;
                }
                if (cur is not Nil) throw new LispErrorException(new LispTypeError("MAP: not a proper list", cur, Startup.Sym("LIST")));
                return head ?? (LispObject)Nil.Instance;
            }
            // A list whose length is known before the result is allocated: the
            // walk is repeated, but only the elements are read twice.
            int n = CountListCells(seq);
            if (kind == MapKindString) return MapListToString(fn, seq, n);
            if (elemType == "T")
            {
                var arrT = new LispObject[n];
                var curT = seq;
                int iT = 0;
                for (; curT is Cons c; curT = c.Cdr) arrT[iT++] = UnwrapMv(fn.Invoke1(c.Car));
                if (curT is not Nil) throw new LispErrorException(new LispTypeError("MAP: not a proper list", curT, Startup.Sym("LIST")));
                return new LispVector(arrT);
            }
            var outVec = new LispVector(n, Nil.Instance, elemType);
            {
                var cur = seq;
                int i = 0;
                for (; cur is Cons c; cur = c.Cdr) outVec[i++] = UnwrapMv(fn.Invoke1(c.Car));
                if (cur is not Nil) throw new LispErrorException(new LispTypeError("MAP: not a proper list", cur, Startup.Sym("LIST")));
            }
            return outVec;
        }
        int len;
        if (seq is LispVector vec0) len = vec0.Length;
        else if (seq is LispString str0) len = str0.Length;
        else return Startup.Keyword("%NO");
        if (kind == MapKindNil)
        {
            for (int i = 0; i < len; i++) fn.Invoke1(MapElem(seq, i));
            MultipleValues.Reset();
            return Nil.Instance;
        }
        if (kind == MapKindList)
        {
            Cons? head = null, tail = null;
            for (int i = 0; i < len; i++)
            {
                var cell = new Cons(UnwrapMv(fn.Invoke1(MapElem(seq, i))), Nil.Instance);
                if (tail == null) head = cell; else tail.Cdr = cell;
                tail = cell;
            }
            return head ?? (LispObject)Nil.Instance;
        }
        if (kind == MapKindString) return MapSeqToString(fn, seq, len);
        if (elemType == "T")
        {
            var arrT = new LispObject[len];
            for (int i = 0; i < len; i++) arrT[i] = UnwrapMv(fn.Invoke1(MapElem(seq, i)));
            return new LispVector(arrT);
        }
        var vec = new LispVector(len, Nil.Instance, elemType);
        for (int i = 0; i < len; i++) vec[i] = UnwrapMv(fn.Invoke1(MapElem(seq, i)));
        return vec;
    }

    /// <summary>MAP over exactly two sequences: the walk and the result are built
    /// here, with no intermediate list and no argument array per element.</summary>
    public static LispObject Map2Fast(LispObject resultType, LispObject function,
                                      LispObject seq1, LispObject seq2)
    {
        if (function is not LispFunction fn) return Startup.Keyword("%NO");
        if (!MapResultShape(resultType, out int kind, out string elemType)) return Startup.Keyword("%NO");
        if (!(seq1 is Cons || seq1 is Nil || seq1 is LispVector || seq1 is LispString)) return Startup.Keyword("%NO");
        if (!(seq2 is Cons || seq2 is Nil || seq2 is LispVector || seq2 is LispString)) return Startup.Keyword("%NO");

        if (kind == MapKindList || kind == MapKindNil)
        {
            var ca = new ForwardSeqCursor(seq1, 0);
            var cb = new ForwardSeqCursor(seq2, 0);
            Cons? head = null, tail = null;
            while (!ca.AtEnd && !cb.AtEnd)
            {
                if (ca.BadTail) throw new LispErrorException(new LispTypeError("MAP: not a proper list", ca.BadTailValue, Startup.Sym("LIST")));
                if (cb.BadTail) throw new LispErrorException(new LispTypeError("MAP: not a proper list", cb.BadTailValue, Startup.Sym("LIST")));
                var value = UnwrapMv(fn.Invoke2(ca.Next(), cb.Next()));
                if (kind == MapKindList)
                {
                    var cell = new Cons(value, Nil.Instance);
                    if (tail == null) head = cell; else tail.Cdr = cell;
                    tail = cell;
                }
            }
            CheckMap2BadTails(ca, cb);
            if (kind == MapKindNil)
            {
                MultipleValues.Reset();
                return Nil.Instance;
            }
            return head ?? (LispObject)Nil.Instance;
        }

        int len = Math.Min(MapSeqLength(seq1), MapSeqLength(seq2));
        var a = new ForwardSeqCursor(seq1, 0);
        var b = new ForwardSeqCursor(seq2, 0);
        if (kind == MapKindString)
        {
            Span<char> chars = len <= 512 ? stackalloc char[len] : new char[len];
            for (int i = 0; i < len; i++)
            {
                if (a.BadTail) throw new LispErrorException(new LispTypeError("MAP: not a proper list", a.BadTailValue, Startup.Sym("LIST")));
                if (b.BadTail) throw new LispErrorException(new LispTypeError("MAP: not a proper list", b.BadTailValue, Startup.Sym("LIST")));
                var v = UnwrapMv(fn.Invoke2(a.Next(), b.Next()));
                if (v is not LispChar lc)
                    throw new LispErrorException(new LispTypeError("COERCE: list element not a character", v));
                chars[i] = lc.Value;
            }
            CheckMap2BadTails(a, b);
            return MapCharResult(chars);
        }
        var vec = new LispVector(len, Nil.Instance, elemType);
        for (int i = 0; i < len; i++)
        {
            if (a.BadTail) throw new LispErrorException(new LispTypeError("MAP: not a proper list", a.BadTailValue, Startup.Sym("LIST")));
            if (b.BadTail) throw new LispErrorException(new LispTypeError("MAP: not a proper list", b.BadTailValue, Startup.Sym("LIST")));
            vec[i] = UnwrapMv(fn.Invoke2(a.Next(), b.Next()));
        }
        CheckMap2BadTails(a, b);
        return vec;
    }

    // The sequences are looked at in order: a bad tail in one that is reached is an
    // error, and one that ended first ends the walk without a look at the later ones.
    private static void CheckMap2BadTails(in ForwardSeqCursor a, in ForwardSeqCursor b)
    {
        if (a.BadTail) throw new LispErrorException(new LispTypeError("MAP: not a proper list", a.BadTailValue, Startup.Sym("LIST")));
        if (a.AtEnd) return;
        if (b.BadTail) throw new LispErrorException(new LispTypeError("MAP: not a proper list", b.BadTailValue, Startup.Sym("LIST")));
    }

    private static int MapSeqLength(LispObject seq) =>
        seq is LispVector v ? v.Length : seq is LispString s ? s.Length : CountListCells(seq);

    /// <summary>MAP-INTO over one source sequence, as a direct entry: the result
    /// vector is written where it stands and no per-element argument array is built.</summary>
    public static LispObject MapInto3(LispObject result, LispObject function, LispObject source)
    {
        if (result is Nil) return Nil.Instance;
        if (result is not Cons && result is not LispVector) return Startup.Keyword("%NO");
        if (!(source is Nil || source is Cons || source is LispVector || source is LispString))
            return Startup.Keyword("%NO");
        var fn = CoerceToFunctionFor(function, "MAP-INTO");
        bool hasFp = result is LispVector fpv && fpv.HasFillPointer;
        int resultCap = hasFp ? ((LispVector)result).Capacity
                              : (int)((Fixnum)Length(result)).Value;
        bool listResult = result is Cons;
        LispObject rtail = listResult ? result : Nil.Instance;
        int n = 0;

        void Store(LispObject value)
        {
            // FUNCTION may return several values (ROUND does): the element is the first.
            value = UnwrapMv(value);
            if (listResult)
            {
                var cell = (Cons)rtail;
                cell.Car = value;
                rtail = cell.Cdr;
            }
            else ((LispVector)result).SetElement(n, value);
        }

        if (source is Cons || source is Nil)
        {
            var tail = source;
            while (n < resultCap && tail is Cons c)
            {
                Store(fn.Invoke1(c.Car));
                tail = c.Cdr;
                n++;
            }
            if (n < resultCap && tail is not Nil)
                throw new LispErrorException(new LispTypeError("MAP-INTO: not a proper list", tail, Startup.Sym("LIST")));
        }
        else
        {
            int len = source is LispVector sv ? sv.Length : ((LispString)source).Length;
            int limit = Math.Min(resultCap, len);
            // A general vector into a general vector: store straight into the element array.
            if (!hasFp && result is LispVector rv0 && rv0.IsPlainObjectVector && rv0.ElementTypeName == "T"
                && source is LispVector sv0 && sv0.IsPlainObjectVector)
            {
                for (; n < limit; n++)
                    rv0._elements[n] = UnwrapMv(fn.Invoke1(sv0._elements[n]));
                return MultipleValues.Alone(result);
            }
            while (n < limit)
            {
                Store(fn.Invoke1(MapElem(source, n)));
                n++;
            }
        }
        if (hasFp) ((LispVector)result).SetFillPointer(n);
        return MultipleValues.Alone(result);
    }

    /// <summary>Installs direct entries so MAP, MAP-INTO and MAPC reach the fast
    /// walks without their &amp;rest list; a declined shape falls back.</summary>
    public static LispObject AttachMapFastPaths(LispObject[] args)
    {
        if (args.Length != 3)
            throw new LispErrorException(new LispProgramError("%ATTACH-MAP-FAST-PATHS: expected 3 arguments"));
        var noFast = Startup.Keyword("%NO");   // looked up once, not per call
        if (args[0] is LispFunction map)
        {
            var fallback = map.RawFunction;
            map.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject>)
                ((rt, f, seq) => {
                    var fast = Map1Fast(rt, f, seq);
                    if (!ReferenceEquals(fast, noFast)) return MultipleValues.Alone(fast);
                    return fallback(new[] { rt, f, seq });
                }));
            map.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)
                ((rt, f, a, b) => {
                    var fast = Map2Fast(rt, f, a, b);
                    if (!ReferenceEquals(fast, noFast)) return MultipleValues.Alone(fast);
                    return fallback(new[] { rt, f, a, b });
                }));
        }
        if (args[1] is LispFunction mapInto)
        {
            var fallback = mapInto.RawFunction;
            mapInto.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject>)
                ((result, f, seq) => {
                    var fast = MapInto3(result, f, seq);
                    if (!ReferenceEquals(fast, noFast)) return fast;
                    return fallback(new[] { result, f, seq });
                }));
        }
        if (args[2] is LispFunction mapc)
        {
            var fallback = mapc.RawFunction;
            mapc.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject>)
                ((f, a, b) => {
                    if (!(a is Cons || a is Nil) && !(b is Cons || b is Nil)) return fallback(new[] { f, a, b });
                    var fast = Mapc3(f, a, b);
                    if (!ReferenceEquals(fast, noFast)) return fast;
                    return fallback(new[] { f, a, b });
                }));
        }
        return Nil.Instance;
    }

    /// <summary>MAPC over exactly two lists: the walk allocates nothing the Lisp
    /// two-list branch did not, and the &amp;rest list is not built.</summary>
    public static LispObject Mapc3(LispObject function, LispObject list1, LispObject list2)
    {
        if (!((list1 is Cons || list1 is Nil) && (list2 is Cons || list2 is Nil)))
            return Startup.Keyword("%NO");
        var fn = CoerceToFunctionFor(function, "MAPC");
        var a = list1;
        var b = list2;
        while (a is Cons ca && b is Cons cb)
        {
            UnwrapMv(fn.Invoke2(ca.Car, cb.Car));
            a = ca.Cdr;
            b = cb.Cdr;
        }
        if (a is not Nil && a is not Cons)
            throw new LispErrorException(new LispTypeError("MAPC: not a proper list", a, Startup.Sym("LIST")));
        if (a is Cons && b is not Nil && b is not Cons)
            throw new LispErrorException(new LispTypeError("MAPC: not a proper list", b, Startup.Sym("LIST")));
        return MultipleValues.Alone(list1);
    }

    private static bool SeqPredicateMatch(LispFunction predFn, LispObject elem,
                                          in SeqKwArgs kw, SeqPredKind kind)
    {
        var val = ApplySeqKey(kw, elem);
        return kind switch
        {
            SeqPredKind.MinusP => IsTrueMinusp(val),
            SeqPredKind.ZeroP => IsTrueZerop(val),
            SeqPredKind.PlusP => IsTruePlusp(val),
            // A fixnum is decided here; anything else (a bignum, or an argument EVENP rejects) goes to the function.
            SeqPredKind.EvenP when val is Fixnum fe => (fe.Value & 1L) == 0,
            SeqPredKind.OddP when val is Fixnum fo => (fo.Value & 1L) != 0,
            _ => IsTruthy(predFn.Invoke1(val)),
        };
    }

    // :key / user-function results must flow as their PRIMARY value: a key fn
    // returning (values a b ...) hands an MvReturn to :test otherwise (SBCL's
    // %find-position with :key #'parse-optional-arg-spec hit STRING= with an
    // MvReturn at make-host-2 stem 8).
    private static LispObject ApplySeqKey(in SeqKwArgs kw, LispObject elem)
        => kw.Key != null ? UnwrapMv(kw.Key.Invoke1(elem)) : elem;

    private static LispObject ApplyKeyFn(LispFunction? keyFn, LispObject elem)
        => keyFn != null ? UnwrapMv(keyFn.Invoke1(elem)) : elem;

    private static LispObject ApplySeqKey(ListKwArgs kw, LispObject elem)
        => kw.Key != null ? UnwrapMv(kw.Key.Invoke1(elem)) : elem;

    /// <summary>The function CL:IDENTITY had when it was first defined. A :KEY is no key only when it is
    /// this one: after a program redefines IDENTITY, #'IDENTITY is a different function and is called.</summary>
    internal static LispFunction? OriginalIdentity;

    /// <summary>The :KEY argument VAL as a function, or null when it is #'IDENTITY, which is no key
    /// at all: the default paths (a character searched for in a string, EQL on elements) then apply
    /// to a call that spells the default out.</summary>
    private static LispFunction? SeqKeyFunction(LispObject val, string fnName)
    {
        var f = CoerceToFunctionFor(val, fnName);
        return OriginalIdentity != null && ReferenceEquals(f, OriginalIdentity) ? null : f;
    }

    static Symbol? s_charEqSym, s_charEqualSym, s_equalSym, s_equalpSym;

    /// <summary>1 for EQL/EQUAL/CHAR=/absent, 2 for CHAR-EQUAL/EQUALP, 0 otherwise.</summary>
    private static int StringCharTestKind(LispObject item, in SeqKwArgs kw)
    {
        if (item is not LispChar || kw.Key != null || kw.TestNot != null) return 0;
        var t = kw.Test;
        if (t == null) return 1;
        if (ReferenceEquals(t, (s_eqlSym ??= Startup.CL.FindSymbol("EQL").symbol)?.Function)
            || ReferenceEquals(t, (s_equalSym ??= Startup.CL.FindSymbol("EQUAL").symbol)?.Function)
            || ReferenceEquals(t, (s_charEqSym ??= Startup.CL.FindSymbol("CHAR=").symbol)?.Function)) return 1;
        if (ReferenceEquals(t, (s_charEqualSym ??= Startup.CL.FindSymbol("CHAR-EQUAL").symbol)?.Function)
            || ReferenceEquals(t, (s_equalpSym ??= Startup.CL.FindSymbol("EQUALP").symbol)?.Function)) return 2;
        return 0;
    }

    // The same mapping as char-upcase for ASCII, without the table lookup.
    private static char FoldChar(char c) => CharCase.Fold(c);

    private static int CharVectorIndex(LispVector vec, char c, bool foldCase, int start, int end, bool fromEnd)
    {
        if (foldCase) c = FoldChar(c);
        if (vec._displacedTo == null && vec._numData is char[] chars)
            return CharSpanIndex(chars, c, foldCase, start, end, fromEnd);
        int step = fromEnd ? -1 : 1;
        for (int i = fromEnd ? end - 1 : start; fromEnd ? i >= start : i < end; i += step)
        {
            if (vec[i] is not LispChar e) continue;
            if ((foldCase ? FoldChar(e.Value) : e.Value) == c) return i;
        }
        return -1;
    }

    private static int StringCharIndex(LispString str, char c, bool foldCase, int start, int end, bool fromEnd)
    {
        if (foldCase)
        {
            c = FoldChar(c);
            if (fromEnd)
            {
                for (int i = end - 1; i >= start; i--) if (FoldChar(str[i]) == c) return i;
                return -1;
            }
            for (int i = start; i < end; i++) if (FoldChar(str[i]) == c) return i;
            return -1;
        }
        if (fromEnd)
        {
            for (int i = end - 1; i >= start; i--) if (str[i] == c) return i;
            return -1;
        }
        for (int i = start; i < end; i++) if (str[i] == c) return i;
        return -1;
    }

    /// <summary>C already case-folded when FOLDCASE.</summary>
    private static int CharSpanIndex(char[] chars, char c, bool foldCase, int start, int end, bool fromEnd)
    {
        if (fromEnd)
        {
            for (int i = end - 1; i >= start; i--)
                if ((foldCase ? FoldChar(chars[i]) : chars[i]) == c) return i;
            return -1;
        }
        for (int i = start; i < end; i++)
            if ((foldCase ? FoldChar(chars[i]) : chars[i]) == c) return i;
        return -1;
    }

    private static bool SeqTestMatch(LispObject item, LispObject elem, in SeqKwArgs kw)
    {
        var val = ApplySeqKey(kw, elem);
        if (kw.TestNot != null)
            return !IsTruthy(kw.TestNot.Invoke2(item, val));
        switch (kw.TestKind)
        {
            case SeqTestKind.Eql: return IsTrueEql(item, val);
            case SeqTestKind.Eq: return IsTrueEq(item, val);
            case SeqTestKind.Equal: return IsTrueEqual(item, val);
            case SeqTestKind.NumEq: return IsTrueNumEq(item, val);
            case SeqTestKind.CharEq: return IsTruthy(CharEqual(item, val));
            default:
                return kw.Test != null ? IsTruthy(kw.Test.Invoke2(item, val)) : IsTrueEql(item, val);
        }
    }

    public static LispObject Find(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("FIND: too few arguments"));
        return FindCore(args[0], args[1], ParseSeqKwArgs(args, 2, "FIND"));
    }

    public static LispObject Find4(LispObject item, LispObject seq, LispObject k1, LispObject v1) =>
        FindCore(item, seq, ParseSeqKwPairs(k1, v1, "FIND"));

    public static LispObject Find6(LispObject item, LispObject seq,
                                   LispObject k1, LispObject v1, LispObject k2, LispObject v2) =>
        FindCore(item, seq, ParseSeqKwPairs(k1, v1, k2, v2, "FIND"));

    public static LispObject Find8(LispObject item, LispObject seq,
                                   LispObject k1, LispObject v1, LispObject k2, LispObject v2,
                                   LispObject k3, LispObject v3) =>
        FindCore(item, seq, ParseSeqKwPairs(k1, v1, k2, v2, k3, v3, "FIND"));

    // 2-arg direct entry: (find item seq) with no keywords -- the shape most
    // call sites have. Skips the args array the variadic path builds.
    public static LispObject Find2(LispObject item, LispObject seq) =>
        FindCore(item, seq, new SeqKwArgs());

    private static LispFunction SeqTestFunction(ref Symbol? cache, string name) =>
        Emitter.CilAssembler.GetFunctionBySymbol(cache ??= Startup.CL.FindSymbol(name).symbol!);

    public static LispObject FindEql(LispObject item, LispObject seq) =>
        FindCore(item, seq, new SeqKwArgs { Test = SeqTestFunction(ref s_eqlSym, "EQL"), TestKind = SeqTestKind.Eql });

    public static LispObject FindEqual(LispObject item, LispObject seq) =>
        FindCore(item, seq, new SeqKwArgs { Test = SeqTestFunction(ref s_equalSym, "EQUAL"), TestKind = SeqTestKind.Equal });

    public static LispObject FindEqualp(LispObject item, LispObject seq) =>
        FindCore(item, seq, new SeqKwArgs { Test = SeqTestFunction(ref s_equalpSym, "EQUALP"), TestKind = SeqTestKind.General });

    public static LispObject FindCharEq(LispObject item, LispObject seq) =>
        FindCore(item, seq, new SeqKwArgs { Test = SeqTestFunction(ref s_charEqSym, "CHAR="), TestKind = SeqTestKind.CharEq });

    public static LispObject FindCharEqual(LispObject item, LispObject seq) =>
        FindCore(item, seq, new SeqKwArgs { Test = SeqTestFunction(ref s_charEqualSym, "CHAR-EQUAL"), TestKind = SeqTestKind.General });

    private static LispObject FindCore(LispObject item, LispObject seq, in SeqKwArgs kw)
    {
        var r = FindScan(item, seq, kw);
        return kw.Test != null || kw.TestNot != null || kw.Key != null ? MultipleValues.Alone(r) : r;
    }

    private static LispObject FindScan(LispObject item, LispObject seq, in SeqKwArgs kw)
    {
        seq = AsSequenceArg(seq);
        int len = seq is LispVector v ? v.Length : seq is LispString ls ? ls.Length : SeqLengthOf(seq, "FIND");
        int start = kw.Start;
        int end = kw.End ?? len;
        CheckBoundingIndices(start, end, len, "FIND");

        // :FROM-END over something indexable walks backwards and stops at the first
        // match -- the answer is the rightmost one either way, but scanning forward and
        // keeping the last applied the test to every element. POSITION already did it
        // this way; FIND did not.
        if (seq is LispVector vec)
        {
            int ck = vec.IsCharVector ? StringCharTestKind(item, kw) : 0;
            if (ck != 0)
            {
                int at = CharVectorIndex(vec, ((LispChar)item).Value, ck == 2, start, end, kw.FromEnd);
                return at < 0 ? Nil.Instance : vec[at];
            }
            if (vec.IsPlainIntegerNum && IsPlainFixnumQuery(item, kw, out long numFind))
            {
                int at = vec.NumIndexOf(numFind, start, end, kw.FromEnd);
                if (at >= -1) return at < 0 ? Nil.Instance : vec[at];
            }
            var m = new SeqMatcher(item, kw);
            if (kw.FromEnd)
            {
                for (int i = end - 1; i >= start; i--)
                    if (m.Match(vec[i], kw)) return vec[i];
                return Nil.Instance;
            }
            for (int i = start; i < end; i++)
                if (m.Match(vec[i], kw)) return vec[i];
            return Nil.Instance;
        }
        if (seq is LispString str)
        {
            int ck = StringCharTestKind(item, kw);
            if (ck != 0)
            {
                int at = StringCharIndex(str, ((LispChar)item).Value, ck == 2, start, end, kw.FromEnd);
                return at < 0 ? Nil.Instance : LispChar.Make(str[at]);
            }
            var m = new SeqMatcher(item, kw);
            if (kw.FromEnd)
            {
                for (int i = end - 1; i >= start; i--)
                {
                    var ch = LispChar.Make(str[i]);
                    if (m.Match(ch, kw)) return ch;
                }
                return Nil.Instance;
            }
            for (int i = start; i < end; i++)
            {
                var ch = LispChar.Make(str[i]);
                if (m.Match(ch, kw)) return ch;
            }
            return Nil.Instance;
        }
        // List
        var ml = new SeqMatcher(item, kw);
        var cur = seq;
        for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
        if (kw.FromEnd)
        {
            LispObject result = Nil.Instance;
            for (int i = start; i < end && cur is Cons c; i++) { if (ml.Match(c.Car, kw)) result = c.Car; cur = c.Cdr; }
            return result;
        }
        for (int i = start; i < end && cur is Cons c2; i++) { if (ml.Match(c2.Car, kw)) return c2.Car; cur = c2.Cdr; }
        return Nil.Instance;
    }

    public static LispObject FindIf(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("FIND-IF: too few arguments"));
        var predFn = CoerceToFunctionFor(args[0], "FIND-IF");
        return FindIfCore(predFn, args[1], ParseSeqKwArgs(args, 2, "FIND-IF", ifForm: true));
    }

    // 2-arg direct entry: (find-if pred seq): no keywords. Skips the args array
    // and the ParseSeqKwArgs scan (a default SeqKwArgs is exactly its zero-keyword
    // result). See RemoveIf2.
    public static LispObject FindIf2(LispObject pred, LispObject seq) =>
        FindIfCore(CoerceToFunctionFor(pred, "FIND-IF"), seq, new SeqKwArgs());

    public static LispObject FindIf4(LispObject pred, LispObject seq, LispObject k1, LispObject v1)
    {
        var predFn = CoerceToFunctionFor(pred, "FIND-IF");
        return FindIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, "FIND-IF", ifForm: true));
    }

    public static LispObject FindIf6(LispObject pred, LispObject seq,
                                     LispObject k1, LispObject v1, LispObject k2, LispObject v2)
    {
        var predFn = CoerceToFunctionFor(pred, "FIND-IF");
        return FindIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, k2, v2, "FIND-IF", ifForm: true));
    }

    public static LispObject FindIf8(LispObject pred, LispObject seq,
                                     LispObject k1, LispObject v1, LispObject k2, LispObject v2,
                                     LispObject k3, LispObject v3)
    {
        var predFn = CoerceToFunctionFor(pred, "FIND-IF");
        return FindIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, k2, v2, k3, v3, "FIND-IF", ifForm: true));
    }

    // FIND-IF-NOT without the per-call predicate wrapper.
    public static LispObject FindIfNotFull(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("FIND-IF-NOT: too few arguments"));
        var predFn = CoerceToFunctionFor(args[0], "FIND-IF-NOT");
        return FindIfCore(predFn, args[1], ParseSeqKwArgs(args, 2, "FIND-IF", ifForm: true), true);
    }

    public static LispObject FindIfNot2(LispObject pred, LispObject seq) =>
        FindIfCore(CoerceToFunctionFor(pred, "FIND-IF-NOT"), seq, new SeqKwArgs(), true);

    public static LispObject FindIfNot4(LispObject pred, LispObject seq, LispObject k1, LispObject v1)
    {
        var predFn = CoerceToFunctionFor(pred, "FIND-IF-NOT");
        return FindIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, "FIND-IF", ifForm: true), true);
    }

    public static LispObject FindIfNot6(LispObject pred, LispObject seq,
                                        LispObject k1, LispObject v1, LispObject k2, LispObject v2)
    {
        var predFn = CoerceToFunctionFor(pred, "FIND-IF-NOT");
        return FindIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, k2, v2, "FIND-IF", ifForm: true), true);
    }

    public static LispObject FindIfNot8(LispObject pred, LispObject seq,
                                        LispObject k1, LispObject v1, LispObject k2, LispObject v2,
                                        LispObject k3, LispObject v3)
    {
        var predFn = CoerceToFunctionFor(pred, "FIND-IF-NOT");
        return FindIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, k2, v2, k3, v3, "FIND-IF", ifForm: true), true);
    }

    private static LispObject FindIfCore(LispFunction predFn, LispObject seq, in SeqKwArgs kw,
                                         bool invert = false)
        => MultipleValues.Alone(FindIfScan(predFn, seq, kw, invert));

    private static LispObject FindIfScan(LispFunction predFn, LispObject seq, in SeqKwArgs kw,
                                         bool invert)
    {
        seq = AsSequenceArg(seq);
        int len = seq is LispVector v ? v.Length : seq is LispString ls ? ls.Length : SeqLengthOf(seq, "FIND-IF");
        int start = kw.Start;
        int end = kw.End ?? len;
        CheckBoundingIndices(start, end, len, "FIND-IF");
        var kind = SeqPredKindOf(kw, predFn);

        if (seq is LispVector vec)
        {
            if (!invert && SignOpOf(kind) >= 0 && vec.IsPlainIntegerNum)
            {
                int at = vec.NumIndexWhereSign(SignOpOf(kind), start, end, kw.FromEnd);
                if (at >= -1) return at < 0 ? Nil.Instance : vec[at];
            }
            if (kw.FromEnd)
            {
                // Backwards, stopping at the first match: see FindCore.
                for (int i = end - 1; i >= start; i--)
                    if (SeqPredicateMatch(predFn, vec[i], kw, kind) != invert) return vec[i];
                return Nil.Instance;
            }
            for (int i = start; i < end; i++)
                if (SeqPredicateMatch(predFn, vec[i], kw, kind) != invert) return vec[i];
            return Nil.Instance;
        }
        if (seq is LispString str)
        {
            if (kw.FromEnd)
            {
                for (int i = end - 1; i >= start; i--)
                {
                    var ch = LispChar.Make(str[i]);
                    if (SeqPredicateMatch(predFn, ch, kw, kind) != invert) return ch;
                }
                return Nil.Instance;
            }
            for (int i = start; i < end; i++)
            {
                var ch = LispChar.Make(str[i]);
                if (SeqPredicateMatch(predFn, ch, kw, kind) != invert) return ch;
            }
            return Nil.Instance;
        }
        // List
        var cur = seq;
        for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
        if (kw.FromEnd)
        {
            LispObject result = Nil.Instance;
            for (int i = start; i < end && cur is Cons c; i++)
            {
                if (SeqPredicateMatch(predFn, c.Car, kw, kind) != invert) result = c.Car;
                cur = c.Cdr;
            }
            return result;
        }
        for (int i = start; i < end && cur is Cons c2; i++)
        {
            if (SeqPredicateMatch(predFn, c2.Car, kw, kind) != invert) return c2.Car;
            cur = c2.Cdr;
        }
        return Nil.Instance;
    }

    public static LispObject Position(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("POSITION: too few arguments"));
        return PositionCore(args[0], args[1], ParseSeqKwArgs(args, 2, "POSITION"));
    }

    // 2-arg direct entry: (position item seq), the shape most call sites have.
    public static LispObject Position2(LispObject item, LispObject seq) =>
        PositionCore(item, seq, new SeqKwArgs());

    public static LispObject Position4(LispObject item, LispObject seq, LispObject k1, LispObject v1) =>
        PositionCore(item, seq, ParseSeqKwPairs(k1, v1, "POSITION"));

    public static LispObject Position8(LispObject item, LispObject seq,
                                       LispObject k1, LispObject v1, LispObject k2, LispObject v2,
                                       LispObject k3, LispObject v3) =>
        PositionCore(item, seq, ParseSeqKwPairs(k1, v1, k2, v2, k3, v3, "POSITION"));

    public static LispObject Position6(LispObject item, LispObject seq,
                                       LispObject k1, LispObject v1, LispObject k2, LispObject v2) =>
        PositionCore(item, seq, ParseSeqKwPairs(k1, v1, k2, v2, "POSITION"));

    // (position item seq :test #'NAME) for the standard test functions, the
    // function resolved as #'NAME resolves it.
    public static LispObject PositionEql(LispObject item, LispObject seq) =>
        PositionCore(item, seq, new SeqKwArgs { Test = SeqTestFunction(ref s_eqlSym, "EQL"), TestKind = SeqTestKind.Eql });

    public static LispObject PositionEqual(LispObject item, LispObject seq) =>
        PositionCore(item, seq, new SeqKwArgs { Test = SeqTestFunction(ref s_equalSym, "EQUAL"), TestKind = SeqTestKind.Equal });

    public static LispObject PositionEqualp(LispObject item, LispObject seq) =>
        PositionCore(item, seq, new SeqKwArgs { Test = SeqTestFunction(ref s_equalpSym, "EQUALP"), TestKind = SeqTestKind.General });

    public static LispObject PositionCharEqual(LispObject item, LispObject seq) =>
        PositionCore(item, seq, new SeqKwArgs { Test = SeqTestFunction(ref s_charEqualSym, "CHAR-EQUAL"), TestKind = SeqTestKind.General });

    public static LispObject PositionCharEq(LispObject item, LispObject seq) =>
        PositionCore(item, seq, new SeqKwArgs { Test = SeqTestFunction(ref s_charEqSym, "CHAR="), TestKind = SeqTestKind.CharEq });

    // POSITION / FIND on a literal string, from the compiler's table.
    public static LispObject PositionLiteralString(LispObject item, LispObject seq, LispString table,
                                                   int testKind, int start, int end, int fromEnd) =>
        LiteralStringLookup(item, seq, table, testKind, start, end, fromEnd != 0, true);

    public static LispObject FindLiteralString(LispObject item, LispObject seq, LispString table,
                                               int testKind, int start, int end, int fromEnd) =>
        LiteralStringLookup(item, seq, table, testKind, start, end, fromEnd != 0, false);

    private static LispObject LiteralStringLookup(LispObject item, LispObject seq, LispString table,
                                                  int testKind, int start, int end, bool fromEnd,
                                                  bool position)
    {
        if (item is LispChar c)
        {
            if (c.Value < table.Length)
            {
                int idx = table[c.Value] - 1;
                LispObject r = idx < 0 ? Nil.Instance
                    : position ? Fixnum.Make(idx) : LispChar.Make(((LispString)seq)[idx]);
                return testKind == 0 ? r : MultipleValues.Alone(r);
            }
            // Case folding can bring a code outside the table back into it.
            if (testKind == 3)
                return LiteralStringFallback(item, seq, testKind, start, end, fromEnd, position);
            return testKind == 0 ? Nil.Instance : MultipleValues.Alone(Nil.Instance);
        }
        if (testKind == 0) return Nil.Instance;
        if (testKind == 1) return MultipleValues.Alone(Nil.Instance);
        return LiteralStringFallback(item, seq, testKind, start, end, fromEnd, position);
    }

    private static LispObject LiteralStringFallback(LispObject item, LispObject seq, int testKind,
                                                    int start, int end, bool fromEnd, bool position)
    {
        var kw = new SeqKwArgs { Start = start, End = end, FromEnd = fromEnd };
        switch (testKind)
        {
            case 2:
                kw.Test = SeqTestFunction(ref s_charEqSym, "CHAR=");
                kw.TestKind = SeqTestKind.CharEq;
                break;
            case 3:
                kw.Test = SeqTestFunction(ref s_charEqualSym, "CHAR-EQUAL");
                kw.TestKind = SeqTestKind.General;
                break;
            default:
                kw.Test = SeqTestFunction(ref s_eqlSym, "EQL");
                kw.TestKind = SeqTestKind.Eql;
                break;
        }
        return position ? PositionCore(item, seq, kw) : FindCore(item, seq, kw);
    }

    /// <summary>Length of a list (or other non-vector sequence) for the search functions;
    /// a non-sequence is reported under the calling function's name.</summary>
    private static int SeqLengthOf(LispObject seq, string who)
    {
        if (seq is not Cons && seq is not Nil)
            throw new LispErrorException(new LispTypeError($"{who}: not a sequence", seq, Startup.Sym("SEQUENCE")));
        return (int)((Fixnum)Length(seq)).Value;
    }

    private static LispObject PositionCore(LispObject item, LispObject seq, in SeqKwArgs kw)
    {
        var r = PositionScan(item, seq, kw);
        return kw.Test != null || kw.TestNot != null || kw.Key != null ? MultipleValues.Alone(r) : r;
    }

    private static LispObject PositionScan(LispObject item, LispObject seq, in SeqKwArgs kw)
    {
        seq = AsSequenceArg(seq);
        int len = seq is LispVector v ? v.Length : seq is LispString ls ? ls.Length : SeqLengthOf(seq, "POSITION");
        int start = kw.Start;
        int end = kw.End ?? len;
        CheckBoundingIndices(start, end, len, "POSITION");

        if (seq is LispVector vec)
        {
            int ck = vec.IsCharVector ? StringCharTestKind(item, kw) : 0;
            if (ck != 0)
            {
                int at = CharVectorIndex(vec, ((LispChar)item).Value, ck == 2, start, end, kw.FromEnd);
                return at < 0 ? Nil.Instance : Fixnum.Make(at);
            }
            if (vec.IsPlainIntegerNum && IsPlainFixnumQuery(item, kw, out long numPos))
            {
                int at = vec.NumIndexOf(numPos, start, end, kw.FromEnd);
                if (at >= -1) return at < 0 ? Nil.Instance : Fixnum.Make(at);
            }
            var m = new SeqMatcher(item, kw);
            if (kw.FromEnd)
            {
                for (int i = end - 1; i >= start; i--)
                    if (m.Match(vec[i], kw)) return Fixnum.Make(i);
                return Nil.Instance;
            }
            for (int i = start; i < end; i++)
                if (m.Match(vec[i], kw)) return Fixnum.Make(i);
            return Nil.Instance;
        }
        if (seq is LispString str)
        {
            int ck = StringCharTestKind(item, kw);
            if (ck != 0)
            {
                int at = StringCharIndex(str, ((LispChar)item).Value, ck == 2, start, end, kw.FromEnd);
                return at < 0 ? Nil.Instance : Fixnum.Make(at);
            }
            var m = new SeqMatcher(item, kw);
            if (kw.FromEnd)
            {
                for (int i = end - 1; i >= start; i--)
                    if (m.Match(LispChar.Make(str[i]), kw)) return Fixnum.Make(i);
                return Nil.Instance;
            }
            for (int i = start; i < end; i++)
                if (m.Match(LispChar.Make(str[i]), kw)) return Fixnum.Make(i);
            return Nil.Instance;
        }
        // List
        var ml = new SeqMatcher(item, kw);
        var cur = seq;
        for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
        if (kw.FromEnd)
        {
            LispObject result = Nil.Instance;
            for (int i = start; i < end && cur is Cons c; i++) { if (ml.Match(c.Car, kw)) result = Fixnum.Make(i); cur = c.Cdr; }
            return result;
        }
        for (int i = start; i < end && cur is Cons c2; i++) { if (ml.Match(c2.Car, kw)) return Fixnum.Make(i); cur = c2.Cdr; }
        return Nil.Instance;
    }

    public static LispObject PositionIf(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("POSITION-IF: too few arguments"));
        return PositionIfCore(CoerceToFunctionFor(args[0], "POSITION-IF"), args[1], ParseSeqKwArgs(args, 2, "POSITION-IF", ifForm: true));
    }

    /// <summary>(POSITION-IF predicate sequence) as a direct entry: the two arguments
    /// arrive in registers instead of an array built for the variadic entry to walk.</summary>
    public static LispObject PositionIf2(LispObject pred, LispObject seq) =>
        PositionIfCore(CoerceToFunctionFor(pred, "POSITION-IF"), seq, new SeqKwArgs());

    public static LispObject PositionIf4(LispObject pred, LispObject seq, LispObject k1, LispObject v1)
    {
        var predFn = CoerceToFunctionFor(pred, "POSITION-IF");
        return PositionIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, "POSITION-IF", ifForm: true));
    }

    public static LispObject PositionIf6(LispObject pred, LispObject seq,
                                         LispObject k1, LispObject v1, LispObject k2, LispObject v2)
    {
        var predFn = CoerceToFunctionFor(pred, "POSITION-IF");
        return PositionIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, k2, v2, "POSITION-IF", ifForm: true));
    }

    public static LispObject PositionIf8(LispObject pred, LispObject seq,
                                         LispObject k1, LispObject v1, LispObject k2, LispObject v2,
                                         LispObject k3, LispObject v3)
    {
        var predFn = CoerceToFunctionFor(pred, "POSITION-IF");
        return PositionIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, k2, v2, k3, v3, "POSITION-IF", ifForm: true));
    }

    // POSITION-IF-NOT with the predicate answer inverted.
    public static LispObject PositionIfNotFull(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("POSITION-IF-NOT: too few arguments"));
        var predFn = CoerceToFunctionFor(args[0], "POSITION-IF-NOT");
        return PositionIfCore(predFn, args[1], ParseSeqKwArgs(args, 2, "POSITION-IF", ifForm: true), true);
    }

    public static LispObject PositionIfNot2(LispObject pred, LispObject seq) =>
        PositionIfCore(CoerceToFunctionFor(pred, "POSITION-IF-NOT"), seq, new SeqKwArgs(), true);

    public static LispObject PositionIfNot4(LispObject pred, LispObject seq, LispObject k1, LispObject v1)
    {
        var predFn = CoerceToFunctionFor(pred, "POSITION-IF-NOT");
        return PositionIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, "POSITION-IF", ifForm: true), true);
    }

    public static LispObject PositionIfNot6(LispObject pred, LispObject seq,
                                            LispObject k1, LispObject v1, LispObject k2, LispObject v2)
    {
        var predFn = CoerceToFunctionFor(pred, "POSITION-IF-NOT");
        return PositionIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, k2, v2, "POSITION-IF", ifForm: true), true);
    }

    public static LispObject PositionIfNot8(LispObject pred, LispObject seq,
                                            LispObject k1, LispObject v1, LispObject k2, LispObject v2,
                                            LispObject k3, LispObject v3)
    {
        var predFn = CoerceToFunctionFor(pred, "POSITION-IF-NOT");
        return PositionIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, k2, v2, k3, v3, "POSITION-IF", ifForm: true), true);
    }

    private static LispObject PositionIfCore(LispFunction predFn, LispObject seq, in SeqKwArgs kw,
                                             bool invert = false)
        => MultipleValues.Alone(PositionIfScan(predFn, seq, kw, invert));

    private static LispObject PositionIfScan(LispFunction predFn, LispObject seq, in SeqKwArgs kw,
                                             bool invert)
    {
        seq = AsSequenceArg(seq);
        int len = seq is LispVector v ? v.Length : seq is LispString ls ? ls.Length : SeqLengthOf(seq, "POSITION-IF");
        int start = kw.Start;
        int end = kw.End ?? len;
        CheckBoundingIndices(start, end, len, "POSITION-IF");
        var kind = SeqPredKindOf(kw, predFn);

        if (seq is LispVector vec)
        {
            if (kw.FromEnd)
            {
                for (int i = end - 1; i >= start; i--)
                    if (SeqPredicateMatch(predFn, vec[i], kw, kind) != invert) return Fixnum.Make(i);
                return Nil.Instance;
            }
            for (int i = start; i < end; i++)
                if (SeqPredicateMatch(predFn, vec[i], kw, kind) != invert) return Fixnum.Make(i);
            return Nil.Instance;
        }
        if (seq is LispString str)
        {
            if (kw.FromEnd)
            {
                for (int i = end - 1; i >= start; i--)
                {
                    var ch = LispChar.Make(str[i]);
                    if (SeqPredicateMatch(predFn, ch, kw, kind) != invert) return Fixnum.Make(i);
                }
                return Nil.Instance;
            }
            for (int i = start; i < end; i++)
            {
                var ch = LispChar.Make(str[i]);
                if (SeqPredicateMatch(predFn, ch, kw, kind) != invert) return Fixnum.Make(i);
            }
            return Nil.Instance;
        }
        // List
        var cur = seq;
        for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
        if (kw.FromEnd)
        {
            for (int i = start; i < end && cur is Cons c; i++)
            {
                if (SeqPredicateMatch(predFn, c.Car, kw, kind) != invert)
                {
                    // For lists with from-end, continue scanning
                    LispObject result = Fixnum.Make(i);
                    cur = c.Cdr;
                    for (int j = i + 1; j < end && cur is Cons c3; j++)
                    {
                        if (SeqPredicateMatch(predFn, c3.Car, kw, kind) != invert) result = Fixnum.Make(j);
                        cur = c3.Cdr;
                    }
                    return result;
                }
                cur = c.Cdr;
            }
            return Nil.Instance;
        }
        for (int i = start; i < end && cur is Cons c2; i++)
        {
            if (SeqPredicateMatch(predFn, c2.Car, kw, kind) != invert) return Fixnum.Make(i);
            cur = c2.Cdr;
        }
        return Nil.Instance;
    }

    // A short list with FROM-END is walked backwards by recursion.
    private const int ReverseListScanLimit = 256;

    private static int CountListFromEnd(SeqMatcher m, LispObject cur, int left, in SeqKwArgs kw)
    {
        if (left <= 0 || cur is not Cons c) return 0;
        int rest = CountListFromEnd(m, c.Cdr, left - 1, kw);
        return rest + (m.Match(c.Car, kw) ? 1 : 0);
    }

    private static int CountIfListFromEnd(LispFunction predFn, LispObject cur, int left,
                                          in SeqKwArgs kw, SeqPredKind kind, bool invert)
    {
        if (left <= 0 || cur is not Cons c) return 0;
        int rest = CountIfListFromEnd(predFn, c.Cdr, left - 1, kw, kind, invert);
        return rest + ((SeqPredicateMatch(predFn, c.Car, kw, kind) != invert) ? 1 : 0);
    }

    public static LispObject Count(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("COUNT: too few arguments"));
        return CountCore(args[0], args[1], ParseSeqKwArgs(args, 2, "COUNT"));
    }

    /// <summary>(COUNT item sequence) as a direct entry: the two arguments arrive in
    /// registers instead of an array built for the variadic entry to walk.</summary>
    public static LispObject Count2(LispObject item, LispObject seq) =>
        CountCore(item, seq, new SeqKwArgs());

    public static LispObject Count4(LispObject item, LispObject seq, LispObject k1, LispObject v1) =>
        CountCore(item, seq, ParseSeqKwPairs(k1, v1, "COUNT"));

    public static LispObject Count6(LispObject item, LispObject seq,
                                    LispObject k1, LispObject v1, LispObject k2, LispObject v2) =>
        CountCore(item, seq, ParseSeqKwPairs(k1, v1, k2, v2, "COUNT"));

    private static LispObject CountCore(LispObject item, LispObject seq, in SeqKwArgs kw)
    {
        var r = CountScan(item, seq, kw);
        return kw.Test != null || kw.TestNot != null || kw.Key != null ? MultipleValues.Alone(r) : r;
    }

    private static LispObject CountScan(LispObject item, LispObject seq, in SeqKwArgs kw)
    {
        seq = AsSequenceArg(seq);
        int len = seq is LispVector v ? v.Length : seq is LispString ls ? ls.Length : SeqLengthOf(seq, "COUNT");
        int start = kw.Start;
        int end = kw.End ?? len;
        CheckBoundingIndices(start, end, len, "COUNT");
        int count = 0;

        if (seq is LispVector vec)
        {
            if (vec.IsPlainIntegerNum && IsPlainFixnumQuery(item, kw, out long numCount))
            {
                int c = vec.NumCountOf(numCount, start, end);
                if (c >= 0) return Fixnum.Make(c);
            }
            var m = new SeqMatcher(item, kw);
            if (kw.FromEnd)
                for (int i = end - 1; i >= start; i--) { if (m.Match(vec[i], kw)) count++; }
            else
                for (int i = start; i < end; i++) { if (m.Match(vec[i], kw)) count++; }
            return Fixnum.Make(count);
        }
        if (seq is LispString str)
        {
            // A character with EQL / CHAR= (1) or CHAR-EQUAL / EQUALP (2): count in the string.
            int ck = StringCharTestKind(item, kw);
            if (ck != 0)
            {
                char c = ((LispChar)item).Value;
                if (ck == 2)
                {
                    c = FoldChar(c);
                    for (int i = start; i < end; i++) if (FoldChar(str[i]) == c) count++;
                }
                else
#if NETSTANDARD2_0
                    for (int i = start; i < end; i++) if (str[i] == c) count++;
#else
                    count = System.MemoryExtensions.Count(str.Chars.Slice(start, end - start), c);
#endif
                return Fixnum.Make(count);
            }
            var m = new SeqMatcher(item, kw);
            if (kw.FromEnd)
                for (int i = end - 1; i >= start; i--) { if (m.Match(LispChar.Make(str[i]), kw)) count++; }
            else
                for (int i = start; i < end; i++) { if (m.Match(LispChar.Make(str[i]), kw)) count++; }
            return Fixnum.Make(count);
        }
        var ml = new SeqMatcher(item, kw);
        // The default EQL test cannot observe order, so :FROM-END needs no
        // reversed copy; a caller's :TEST or :KEY still walks from the right.
        if (kw.FromEnd && (kw.Key != null || kw.Test != null || kw.TestNot != null))
        {
            if (end - start <= ReverseListScanLimit)
            {
                var cur = seq;
                for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
                count = CountListFromEnd(ml, cur, end - start, kw);
            }
            else
            {
                var elems = new System.Collections.Generic.List<LispObject>();
                var cur = seq;
                for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
                for (int i = start; i < end && cur is Cons c2; i++) { elems.Add(c2.Car); cur = c2.Cdr; }
                for (int i = elems.Count - 1; i >= 0; i--) { if (ml.Match(elems[i], kw)) count++; }
            }
        }
        else
        {
            var cur = seq;
            for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
            for (int i = start; i < end && cur is Cons c2; i++) { if (ml.Match(c2.Car, kw)) count++; cur = c2.Cdr; }
        }
        return Fixnum.Make(count);
    }

    public static LispObject CountIf(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("COUNT-IF: too few arguments"));
        return CountIfCore(CoerceToFunctionFor(args[0], "COUNT-IF"), args[1], ParseSeqKwArgs(args, 2, "COUNT-IF", ifForm: true));
    }

    /// <summary>(COUNT-IF predicate sequence) as a direct entry: the two arguments
    /// arrive in registers instead of an array built for the variadic entry to walk.</summary>
    public static LispObject CountIf2(LispObject pred, LispObject seq) =>
        CountIfCore(CoerceToFunctionFor(pred, "COUNT-IF"), seq, new SeqKwArgs());

    public static LispObject CountIf4(LispObject pred, LispObject seq, LispObject k1, LispObject v1)
    {
        var predFn = CoerceToFunctionFor(pred, "COUNT-IF");
        return CountIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, "COUNT-IF", ifForm: true));
    }

    public static LispObject CountIf6(LispObject pred, LispObject seq,
                                      LispObject k1, LispObject v1, LispObject k2, LispObject v2)
    {
        var predFn = CoerceToFunctionFor(pred, "COUNT-IF");
        return CountIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, k2, v2, "COUNT-IF", ifForm: true));
    }

    // COUNT-IF-NOT with the predicate answer inverted.
    public static LispObject CountIfNotFull(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("COUNT-IF-NOT: too few arguments"));
        var predFn = CoerceToFunctionFor(args[0], "COUNT-IF-NOT");
        return CountIfCore(predFn, args[1], ParseSeqKwArgs(args, 2, "COUNT-IF", ifForm: true), true);
    }

    public static LispObject CountIfNot2(LispObject pred, LispObject seq) =>
        CountIfCore(CoerceToFunctionFor(pred, "COUNT-IF-NOT"), seq, new SeqKwArgs(), true);

    public static LispObject CountIfNot4(LispObject pred, LispObject seq, LispObject k1, LispObject v1)
    {
        var predFn = CoerceToFunctionFor(pred, "COUNT-IF-NOT");
        return CountIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, "COUNT-IF", ifForm: true), true);
    }

    public static LispObject CountIfNot6(LispObject pred, LispObject seq,
                                         LispObject k1, LispObject v1, LispObject k2, LispObject v2)
    {
        var predFn = CoerceToFunctionFor(pred, "COUNT-IF-NOT");
        return CountIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, k2, v2, "COUNT-IF", ifForm: true), true);
    }

    private static LispObject CountIfCore(LispFunction predFn, LispObject seq, in SeqKwArgs kw,
                                          bool invert = false)
        => MultipleValues.Alone(CountIfScan(predFn, seq, kw, invert));

    private static LispObject CountIfScan(LispFunction predFn, LispObject seq, in SeqKwArgs kw,
                                          bool invert)
    {
        seq = AsSequenceArg(seq);
        int len = seq is LispVector v ? v.Length : seq is LispString ls ? ls.Length : SeqLengthOf(seq, "COUNT-IF");
        int start = kw.Start;
        int end = kw.End ?? len;
        CheckBoundingIndices(start, end, len, "COUNT-IF");
        var kind = SeqPredKindOf(kw, predFn);
        int count = 0;

        if (seq is LispVector vec)
        {
            if (kw.FromEnd)
                for (int i = end - 1; i >= start; i--) { if (SeqPredicateMatch(predFn, vec[i], kw, kind) != invert) count++; }
            else
                for (int i = start; i < end; i++) { if (SeqPredicateMatch(predFn, vec[i], kw, kind) != invert) count++; }
            return Fixnum.Make(count);
        }
        if (seq is LispString str)
        {
            if (kw.FromEnd)
                for (int i = end - 1; i >= start; i--) { var ch = LispChar.Make(str[i]); if (SeqPredicateMatch(predFn, ch, kw, kind) != invert) count++; }
            else
                for (int i = start; i < end; i++) { var ch = LispChar.Make(str[i]); if (SeqPredicateMatch(predFn, ch, kw, kind) != invert) count++; }
            return Fixnum.Make(count);
        }
        // List - for from-end, visit in reverse; a long list is copied.
        if (kw.FromEnd)
        {
            if (end - start <= ReverseListScanLimit)
            {
                var cur = seq;
                for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
                count = CountIfListFromEnd(predFn, cur, end - start, kw, kind, invert);
            }
            else
            {
                var elems = new System.Collections.Generic.List<LispObject>();
                var cur = seq;
                for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
                for (int i = start; i < end && cur is Cons c2; i++) { elems.Add(c2.Car); cur = c2.Cdr; }
                for (int i = elems.Count - 1; i >= 0; i--) { if (SeqPredicateMatch(predFn, elems[i], kw, kind) != invert) count++; }
            }
        }
        else
        {
            var cur = seq;
            for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
            for (int i = start; i < end && cur is Cons c2; i++) { if (SeqPredicateMatch(predFn, c2.Car, kw, kind) != invert) count++; cur = c2.Cdr; }
        }
        return Fixnum.Make(count);
    }

    // Helper: parse test/test-not/key keyword args for list functions (member, assoc, etc.)
    private struct ListKwArgs
    {
        public LispFunction? Test, TestNot, Key;
        public bool IsEqTest, IsEqlTest, IsEqualTest; // fast path flags
    }

    private struct ListKwSeen { public bool Test, TestNot, Key, Unknown, IfForm; }

    /// <summary>Apply one keyword pair. The first setting of a keyword wins, which is
    /// what the standard says about a repeated keyword argument.</summary>
    private static void ApplyListKw(ref ListKwArgs kw, ref ListKwSeen seen,
                                    LispObject key, LispObject val, string fnName)
    {
        if (key is not Symbol s)
            throw new LispErrorException(new LispProgramError($"{fnName}: keyword must be a symbol, got {key}"));
        switch (s.Name)
        {
            case "TEST":
                if (seen.IfForm) { seen.Unknown = true; break; }
                if (!seen.Test) { if (val is not Nil) kw.Test = CoerceToFunctionFor(val, fnName); seen.Test = true; }
                break;
            case "TEST-NOT":
                if (seen.IfForm) { seen.Unknown = true; break; }
                if (!seen.TestNot) { if (val is not Nil) kw.TestNot = CoerceToFunctionFor(val, fnName); seen.TestNot = true; }
                break;
            case "KEY": if (!seen.Key) { if (val is not Nil) kw.Key = SeqKeyFunction(val, fnName); seen.Key = true; } break;
            case "ALLOW-OTHER-KEYS": break;
            default: seen.Unknown = true; break;
        }
    }

    // EQ and EQL are looked up once. FindSymbol is a hash lookup, and it ran on
    // every MEMBER/ASSOC call that passed a :test.
    private static Symbol? s_eqlSym, s_eqSym, s_equalLSym;

    /// <summary>Work out which comparison fast path the parsed keywords allow.</summary>
    private static void FinishListKw(ref ListKwArgs kw, string fnName)
    {
        if (kw.Test != null && kw.TestNot != null) throw TestAndTestNotError(fnName);
        if (kw.TestNot != null || kw.Key != null) return;
        if (kw.Test == null) { kw.IsEqlTest = true; return; }
        s_eqlSym ??= Startup.CL.FindSymbol("EQL").symbol;
        s_eqSym ??= Startup.CL.FindSymbol("EQ").symbol;
        if (kw.Test == s_eqlSym?.Function) kw.IsEqlTest = true;
        else if (kw.Test == s_eqSym?.Function) kw.IsEqTest = true;
        else if (kw.Test == (s_equalLSym ??= Startup.CL.FindSymbol("EQUAL").symbol)?.Function) kw.IsEqualTest = true;
    }

    private static ListKwArgs ParseListKwArgs(LispObject[] args, int kwStart, string fnName,
                                              bool ifForm = false)
    {
        var kw = new ListKwArgs();
        if ((args.Length - kwStart) % 2 != 0)
            throw new LispErrorException(new LispProgramError($"{fnName}: odd number of keyword arguments"));
        bool? allowOtherKeys = null;
        for (int i = kwStart; i < args.Length - 1; i += 2)
            if (args[i] is Symbol kw0 && kw0.Name == "ALLOW-OTHER-KEYS" && allowOtherKeys == null)
                allowOtherKeys = IsTruthy(args[i + 1]);
        var seen = new ListKwSeen { IfForm = ifForm };
        for (int i = kwStart; i < args.Length - 1; i += 2)
            ApplyListKw(ref kw, ref seen, args[i], args[i + 1], fnName);
        if (seen.Unknown && allowOtherKeys != true)
            throw new LispErrorException(new LispProgramError($"{fnName}: unknown keyword argument"));
        FinishListKw(ref kw, fnName);
        return kw;
    }

    /// <summary>One or two keyword pairs, straight from a direct entry's parameters.
    /// The pairs arrive in registers, and putting them into an array so the general
    /// parser could walk it was the whole cost of the call: 40 bytes for
    /// (member x l :test #'string=), which is how most of the standard library
    /// dispatches on a name.</summary>
    private static ListKwArgs ParseListKwPairs(LispObject k1, LispObject v1,
                                               LispObject k2, LispObject v2,
                                               bool hasSecond, string fnName,
                                               bool ifForm = false)
    {
        var kw = new ListKwArgs();
        bool? allowOtherKeys = null;
        if (k1 is Symbol a1 && a1.Name == "ALLOW-OTHER-KEYS") allowOtherKeys = IsTruthy(v1);
        if (allowOtherKeys == null && hasSecond && k2 is Symbol a2 && a2.Name == "ALLOW-OTHER-KEYS")
            allowOtherKeys = IsTruthy(v2);
        var seen = new ListKwSeen { IfForm = ifForm };
        ApplyListKw(ref kw, ref seen, k1, v1, fnName);
        if (hasSecond) ApplyListKw(ref kw, ref seen, k2, v2, fnName);
        if (seen.Unknown && allowOtherKeys != true)
            throw new LispErrorException(new LispProgramError($"{fnName}: unknown keyword argument"));
        FinishListKw(ref kw, fnName);
        return kw;
    }

    private static bool ListTestMatch(LispObject item, LispObject element, in ListKwArgs kw)
    {
        var k = ApplySeqKey(kw, element);
        if (kw.TestNot != null)
            return !IsTruthy(kw.TestNot.Invoke2(item, k));
        if (kw.IsEqualTest)
            return IsTrueEqual(item, k);
        if (kw.Test != null)
            return IsTruthy(kw.Test.Invoke2(item, k));
        return IsTrueEql(item, k);
    }

    // MEMBER: (member item list &key test test-not key)
    public static LispObject MemberFull(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("MEMBER: too few arguments"));
        var item = args[0];
        var list = args[1];
        var kw = ParseListKwArgs(args, 2, "MEMBER");
        return MemberCore(item, list, kw);
    }

    // No-keyword ListKwArgs: provably what ParseListKwArgs returns for zero
    // keyword pairs (loops don't run; TestNot/Key null, Test null -> IsEqlTest).
    // Shared by the 2-arg direct entries of MEMBER/ASSOC.
    private static readonly ListKwArgs s_eqlListKw = new ListKwArgs { IsEqlTest = true };

    // 2-arg direct entry: (member item list): no keyword pairs.
    public static LispObject Member2(LispObject item, LispObject list) =>
        MemberCore(item, list, s_eqlListKw);

    // 4-arg direct entry: (member item list kw val). The keyword pair goes
    // through the exact shared parser (over a 2-element array: half the
    // allocation of the InvokeSlow path's 4-element args array).
    public static LispObject Member4(LispObject item, LispObject list, LispObject k, LispObject v) =>
        MemberCore(item, list, ParseListKwPairs(k, v, Nil.Instance, Nil.Instance, false, "MEMBER"));

    // 6-arg direct entry: (member item list k1 v1 k2 v2): two keyword pairs,
    // e.g. (member x l :key #'symbol-name :test #'string=), which the compiler's
    // special-variable lookup performs per variable reference. Same shared parser
    // over a 4-element array; the InvokeSlow path's 6-element args array is skipped.
    public static LispObject Member6(LispObject item, LispObject list,
                                     LispObject k1, LispObject v1, LispObject k2, LispObject v2) =>
        MemberCore(item, list, ParseListKwPairs(k1, v1, k2, v2, true, "MEMBER"));

    private static LispObject MemberCore(LispObject item, LispObject list, in ListKwArgs kw)
    {
        var r = MemberScan(item, list, kw);
        return kw.Test != null || kw.TestNot != null || kw.Key != null ? MultipleValues.Alone(r) : r;
    }

    private static LispObject MemberScan(LispObject item, LispObject list, in ListKwArgs kw)
    {
        if (list is Nil) return Nil.Instance;
        if (list is not Cons)
            throw new LispErrorException(new LispTypeError("MEMBER: not a proper list", list, Startup.Sym("LIST")));

        // Fast path: eq test, no key
        if (kw.IsEqTest)
        {
            var cur = list;
            // Only a fixnum item can be EQ to a different object; skip that test otherwise.
            if (item is Fixnum)
            {
                for (; cur is Cons c; cur = c.Cdr)
                    if (IsEqRef(item, c.Car)) return c;
            }
            else
            {
                for (; cur is Cons c; cur = c.Cdr)
                    if (ReferenceEquals(item, c.Car)) return c;
            }
            if (cur is not Nil) throw new LispErrorException(new LispTypeError("MEMBER: not a proper list", cur, Startup.Sym("LIST")));
            return Nil.Instance;
        }
        // Fast path: eql test, no key
        if (kw.IsEqlTest)
        {
            var cur = list;
            if (EqlIsIdentity(item = Primary(item)))
            {
                for (; cur is Cons c; cur = c.Cdr)
                    if (ReferenceEquals(item, c.Car) || c.Car is MvReturn && ReferenceEquals(item, Primary(c.Car)))
                        return c;
            }
            else
            {
                for (; cur is Cons c; cur = c.Cdr)
                    if (IsTrueEql(item, c.Car)) return c;
            }
            if (cur is not Nil) throw new LispErrorException(new LispTypeError("MEMBER: not a proper list", cur, Startup.Sym("LIST")));
            return Nil.Instance;
        }
        // General case
        {
            var cur = list;
            for (; cur is Cons c; cur = c.Cdr)
                if (ListTestMatch(item, c.Car, kw)) return c;
            if (cur is not Nil) throw new LispErrorException(new LispTypeError("MEMBER: not a proper list", cur, Startup.Sym("LIST")));
            return Nil.Instance;
        }
    }

    // MEMBER-IF: (member-if predicate list &key key)
    public static LispObject MemberIf(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("MEMBER-IF: too few arguments"));
        return MemberIfCore(args[0], args[1], ParseListKwArgs(args, 2, "MEMBER-IF", ifForm: true));
    }

    public static LispObject MemberIf2(LispObject predicate, LispObject list) =>
        MemberIfCore(predicate, list, default);

    public static LispObject MemberIf4(LispObject predicate, LispObject list, LispObject k, LispObject v) =>
        MemberIfCore(predicate, list,
                     ParseListKwPairs(k, v, Nil.Instance, Nil.Instance, false, "MEMBER-IF", true));

    // MEMBER-IF-NOT with the predicate answer inverted.
    public static LispObject MemberIfNotFull(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("MEMBER-IF-NOT: too few arguments"));
        var predFn = CoerceToFunctionFor(args[0], "MEMBER-IF-NOT");
        return MemberIfCore(predFn, args[1], ParseListKwArgs(args, 2, "MEMBER-IF", ifForm: true), true);
    }

    public static LispObject MemberIfNot2(LispObject predicate, LispObject list) =>
        MemberIfCore(CoerceToFunctionFor(predicate, "MEMBER-IF-NOT"), list, default, true);

    public static LispObject MemberIfNot4(LispObject predicate, LispObject list, LispObject k, LispObject v) =>
        MemberIfCore(CoerceToFunctionFor(predicate, "MEMBER-IF-NOT"), list,
                     ParseListKwPairs(k, v, Nil.Instance, Nil.Instance, false, "MEMBER-IF", true), true);

    private static LispObject MemberIfCore(LispObject predicate, LispObject list, in ListKwArgs kw,
                                           bool invert = false)
    {
        var predFn = CoerceToFunctionFor(predicate, "MEMBER-IF");
        if (list is not Nil && list is not Cons)
            throw new LispErrorException(new LispTypeError("MEMBER-IF: not a proper list", list));
        var cur = list;
        for (; cur is Cons c; cur = c.Cdr)
        {
            var elem = ApplySeqKey(kw, c.Car);
            if (IsTruthy(predFn.Invoke1(elem)) != invert) return MultipleValues.Alone(c);
        }
        if (cur is not Nil) throw new LispErrorException(new LispTypeError("MEMBER-IF: not a proper list", cur));
        return MultipleValues.Alone(Nil.Instance);
    }

    // ADJOIN: (adjoin item list &key key test test-not);
    // (if (member (funcall key item) list :key key :test test) list (cons item list)).
    // The key is applied to ITEM for the comparison, but the element consed on is
    // the original ITEM. PUSHNEW expands into this, so the keyworded shapes are
    // common in ordinary code; the direct entries below mirror MEMBER's.
    public static LispObject AdjoinFull(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("ADJOIN: too few arguments"));
        return AdjoinCore(args[0], args[1], ParseListKwArgs(args, 2, "ADJOIN"));
    }

    public static LispObject Adjoin2(LispObject item, LispObject list) =>
        AdjoinCore(item, list, s_eqlListKw);

    public static LispObject Adjoin4(LispObject item, LispObject list, LispObject k, LispObject v) =>
        AdjoinCore(item, list, ParseListKwPairs(k, v, Nil.Instance, Nil.Instance, false, "ADJOIN"));

    public static LispObject Adjoin6(LispObject item, LispObject list,
                                     LispObject k1, LispObject v1, LispObject k2, LispObject v2) =>
        AdjoinCore(item, list, ParseListKwPairs(k1, v1, k2, v2, true, "ADJOIN"));

    private static LispObject AdjoinCore(LispObject item, LispObject list, in ListKwArgs kw)
    {
        var r = AdjoinScan(item, list, kw);
        return kw.Test != null || kw.TestNot != null || kw.Key != null ? MultipleValues.Alone(r) : r;
    }

    private static LispObject AdjoinScan(LispObject item, LispObject list, in ListKwArgs kw)
    {
        if (list is not Nil && list is not Cons)
            throw new LispErrorException(new LispTypeError("ADJOIN: not a proper list", list));
        var itemKey = ApplySeqKey(kw, item);
        var cur = list;
        for (; cur is Cons c; cur = c.Cdr)
            if (ListTestMatch(itemKey, c.Car, kw)) return list;
        if (cur is not Nil) throw new LispErrorException(new LispTypeError("ADJOIN: not a proper list", cur));
        return new Cons(item, list);
    }

    // ASSOC: (assoc item alist &key test test-not key)
    public static LispObject AssocFull(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("ASSOC: too few arguments"));
        var item = args[0];
        var alist = args[1];
        var kw = ParseListKwArgs(args, 2, "ASSOC");
        return AssocCore(item, alist, kw);
    }

    // 2-/4-arg direct entries: see Member2/Member4.
    public static LispObject Assoc2(LispObject item, LispObject alist) =>
        AssocCore(item, alist, s_eqlListKw);

    public static LispObject Assoc4(LispObject item, LispObject alist, LispObject k, LispObject v) =>
        AssocCore(item, alist, ParseListKwPairs(k, v, Nil.Instance, Nil.Instance, false, "ASSOC"));

    // 6-arg direct entry: (assoc item alist k1 v1 k2 v2): two keyword pairs.
    // See Member6.
    public static LispObject Assoc6(LispObject item, LispObject alist,
                                    LispObject k1, LispObject v1, LispObject k2, LispObject v2) =>
        AssocCore(item, alist, ParseListKwPairs(k1, v1, k2, v2, true, "ASSOC"));

    /// NIL is a legal (skipped) alist element; any other non-cons is a type error.
    private static void CheckAssocEntry(LispObject entry)
    {
        if (entry is not Nil)
            throw new LispErrorException(new LispTypeError(
                "ASSOC: alist entry is not a cons", entry, Startup.Sym("LIST")));
    }

    private static LispObject AssocCore(LispObject item, LispObject alist, in ListKwArgs kw)
    {
        var r = AssocScan(item, alist, kw);
        return kw.Test != null || kw.TestNot != null || kw.Key != null ? MultipleValues.Alone(r) : r;
    }

    private static LispObject AssocScan(LispObject item, LispObject alist, in ListKwArgs kw)
    {
        if (alist is Nil) return Nil.Instance;

        // A NIL element is allowed and skipped (CLHS assoc); anything else that is
        // not a cons is a type error. Runtime.Assoc, what the compiler emits inline
        // for the 2-argument call, already signalled it, so (assoc 'z '((a . b) :bad))
        // errored when written literally but returned NIL through #'ASSOC, on BOTH
        // evaluator paths (ansi-test ASSOC.ERROR.11): the same divergence unary #'-
        // had, where the inlined form is right and the function object is not.
        // The extra test only runs for entries that are not conses, so the eq/eql
        // fast paths keep their cost.
        // Fast path: eq test, no key
        if (kw.IsEqTest)
        {
            var cur = alist;
            bool byValue = item is Fixnum;
            for (; cur is Cons c; cur = c.Cdr)
            {
                if (c.Car is Cons pair) { if (byValue ? IsEqRef(item, pair.Car) : ReferenceEquals(item, pair.Car)) return pair; }
                else CheckAssocEntry(c.Car);
            }
            if (cur is not Nil) throw new LispErrorException(new LispTypeError("ASSOC: not a proper list", cur));
            return Nil.Instance;
        }
        // Fast path: eql test, no key
        if (kw.IsEqlTest)
        {
            var cur = alist;
            for (; cur is Cons c; cur = c.Cdr)
            {
                if (c.Car is Cons pair) { if (IsTrueEql(item, pair.Car)) return pair; }
                else CheckAssocEntry(c.Car);
            }
            if (cur is not Nil) throw new LispErrorException(new LispTypeError("ASSOC: not a proper list", cur));
            return Nil.Instance;
        }
        // Fast path: equal test, no key
        if (kw.IsEqualTest)
        {
            var cur = alist;
            for (; cur is Cons c; cur = c.Cdr)
            {
                if (c.Car is Cons pair) { if (IsTrueEqual(item, pair.Car)) return pair; }
                else CheckAssocEntry(c.Car);
            }
            if (cur is not Nil) throw new LispErrorException(new LispTypeError("ASSOC: not a proper list", cur));
            return Nil.Instance;
        }
        // General case
        {
            var cur = alist;
            for (; cur is Cons c; cur = c.Cdr)
            {
                if (c.Car is not Cons pair) { CheckAssocEntry(c.Car); continue; }
                var k = ApplySeqKey(kw, pair.Car);
                if (kw.TestNot != null)
                { if (!IsTruthy(kw.TestNot.Invoke2(item, k))) return pair; }
                else if (kw.Test != null)
                { if (IsTruthy(kw.Test.Invoke2(item, k))) return pair; }
                else
                { if (IsTrueEql(item, k)) return pair; }
            }
            if (cur is not Nil) throw new LispErrorException(new LispTypeError("ASSOC: not a proper list", cur));
            return Nil.Instance;
        }
    }

    // ASSOC-IF: (assoc-if predicate alist &key key)
    public static LispObject AssocIf(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("ASSOC-IF: too few arguments"));
        return AssocIfCore(args[0], args[1], ParseListKwArgs(args, 2, "ASSOC-IF", ifForm: true));
    }

    public static LispObject AssocIf2(LispObject predicate, LispObject alist) =>
        AssocIfCore(predicate, alist, default);

    public static LispObject AssocIf4(LispObject predicate, LispObject alist, LispObject k, LispObject v) =>
        AssocIfCore(predicate, alist,
                    ParseListKwPairs(k, v, Nil.Instance, Nil.Instance, false, "ASSOC-IF", true));

    // ASSOC-IF-NOT with the predicate answer inverted.
    public static LispObject AssocIfNotFull(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("ASSOC-IF-NOT: too few arguments"));
        var predFn = CoerceToFunctionFor(args[0], "ASSOC-IF-NOT");
        return AssocIfCore(predFn, args[1], ParseListKwArgs(args, 2, "ASSOC-IF", ifForm: true), true);
    }

    public static LispObject AssocIfNot2(LispObject predicate, LispObject alist) =>
        AssocIfCore(CoerceToFunctionFor(predicate, "ASSOC-IF-NOT"), alist, default, true);

    public static LispObject AssocIfNot4(LispObject predicate, LispObject alist, LispObject k, LispObject v) =>
        AssocIfCore(CoerceToFunctionFor(predicate, "ASSOC-IF-NOT"), alist,
                    ParseListKwPairs(k, v, Nil.Instance, Nil.Instance, false, "ASSOC-IF", true), true);

    private static LispObject AssocIfCore(LispObject predicate, LispObject alist, in ListKwArgs kw,
                                          bool invert = false)
    {
        var predFn = CoerceToFunctionFor(predicate, "ASSOC-IF");
        if (alist is not Nil && alist is not Cons)
            throw new LispErrorException(new LispTypeError("ASSOC-IF: not a proper list", alist));
        var cur = alist;
        for (; cur is Cons c; cur = c.Cdr)
        {
            if (c.Car is Nil) continue; // nil entries are allowed
            if (c.Car is not Cons pair)
                throw new LispErrorException(new LispTypeError("ASSOC-IF: alist entry is not a cons or nil", c.Car));
            var elem = ApplySeqKey(kw, pair.Car);
            if (IsTruthy(predFn.Invoke1(elem)) != invert) return MultipleValues.Alone(pair);
        }
        if (cur is not Nil) throw new LispErrorException(new LispTypeError("ASSOC-IF: not a proper list", cur));
        return MultipleValues.Alone(Nil.Instance);
    }

    // RASSOC: (rassoc item alist &key test test-not key)
    public static LispObject RassocFull(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("RASSOC: too few arguments"));
        return RassocCore(args[0], args[1], ParseListKwArgs(args, 2, "RASSOC"));
    }

    /// <summary>(RASSOC item alist) as a direct entry: the two arguments arrive in
    /// registers instead of an array built for the variadic entry to walk.</summary>
    public static LispObject Rassoc2(LispObject item, LispObject alist) =>
        RassocCore(item, alist, s_eqlListKw);

    /// <summary>(RASSOC item alist kw val), the other shape that shows up.</summary>
    public static LispObject Rassoc4(LispObject item, LispObject alist, LispObject k, LispObject v) =>
        RassocCore(item, alist, ParseListKwPairs(k, v, Nil.Instance, Nil.Instance, false, "RASSOC"));

    public static LispObject Rassoc6(LispObject item, LispObject alist,
                                     LispObject k1, LispObject v1, LispObject k2, LispObject v2) =>
        RassocCore(item, alist, ParseListKwPairs(k1, v1, k2, v2, true, "RASSOC"));

    private static LispObject RassocCore(LispObject item, LispObject alist, in ListKwArgs kw)
    {
        var r = RassocScan(item, alist, kw);
        return kw.Test != null || kw.TestNot != null || kw.Key != null ? MultipleValues.Alone(r) : r;
    }

    private static LispObject RassocScan(LispObject item, LispObject alist, in ListKwArgs kw)
    {
        if (alist is Nil) return Nil.Instance;

        // Fast path: eql test, no key
        if (kw.IsEqlTest)
        {
            var cur = alist;
            for (; cur is Cons c; cur = c.Cdr)
                if (c.Car is Cons pair && IsTrueEql(item, pair.Cdr)) return pair;
            if (cur is not Nil) throw new LispErrorException(new LispTypeError("RASSOC: not a proper list", cur));
            return Nil.Instance;
        }
        // General case
        {
            var cur = alist;
            for (; cur is Cons c; cur = c.Cdr)
            {
                if (c.Car is not Cons pair) continue;
                var k = ApplySeqKey(kw, pair.Cdr);
                if (kw.TestNot != null)
                { if (!IsTruthy(kw.TestNot.Invoke2(item, k))) return pair; }
                else if (kw.Test != null)
                { if (IsTruthy(kw.Test.Invoke2(item, k))) return pair; }
                else
                { if (IsTrueEql(item, k)) return pair; }
            }
            if (cur is not Nil) throw new LispErrorException(new LispTypeError("RASSOC: not a proper list", cur));
            return Nil.Instance;
        }
    }

    // RASSOC-IF: (rassoc-if predicate alist &key key)
    public static LispObject RassocIf(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("RASSOC-IF: too few arguments"));
        return RassocIfCore(args[0], args[1], ParseListKwArgs(args, 2, "RASSOC-IF", ifForm: true));
    }

    public static LispObject RassocIf2(LispObject predicate, LispObject alist) =>
        RassocIfCore(predicate, alist, default);

    public static LispObject RassocIf4(LispObject predicate, LispObject alist, LispObject k, LispObject v) =>
        RassocIfCore(predicate, alist,
                     ParseListKwPairs(k, v, Nil.Instance, Nil.Instance, false, "RASSOC-IF", true));

    // RASSOC-IF-NOT with the predicate answer inverted.
    public static LispObject RassocIfNotFull(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("RASSOC-IF-NOT: too few arguments"));
        var predFn = CoerceToFunctionFor(args[0], "RASSOC-IF-NOT");
        return RassocIfCore(predFn, args[1], ParseListKwArgs(args, 2, "RASSOC-IF", ifForm: true), true);
    }

    public static LispObject RassocIfNot2(LispObject predicate, LispObject alist) =>
        RassocIfCore(CoerceToFunctionFor(predicate, "RASSOC-IF-NOT"), alist, default, true);

    public static LispObject RassocIfNot4(LispObject predicate, LispObject alist, LispObject k, LispObject v) =>
        RassocIfCore(CoerceToFunctionFor(predicate, "RASSOC-IF-NOT"), alist,
                     ParseListKwPairs(k, v, Nil.Instance, Nil.Instance, false, "RASSOC-IF", true), true);

    private static LispObject RassocIfCore(LispObject predicate, LispObject alist, in ListKwArgs kw,
                                           bool invert = false)
    {
        var predFn = CoerceToFunctionFor(predicate, "RASSOC-IF");
        if (alist is not Nil && alist is not Cons)
            throw new LispErrorException(new LispTypeError("RASSOC-IF: not a proper list", alist));
        var cur = alist;
        for (; cur is Cons c; cur = c.Cdr)
        {
            if (c.Car is Nil) continue;
            if (c.Car is not Cons pair)
                throw new LispErrorException(new LispTypeError("RASSOC-IF: alist entry is not a cons or nil", c.Car));
            var elem = ApplySeqKey(kw, pair.Cdr);
            if (IsTruthy(predFn.Invoke1(elem)) != invert) return MultipleValues.Alone(pair);
        }
        if (cur is not Nil) throw new LispErrorException(new LispTypeError("RASSOC-IF: not a proper list", cur));
        return MultipleValues.Alone(Nil.Instance);
    }

    // REDUCE: (reduce function sequence &key key from-end start end initial-value)
    private struct ReduceKwArgs
    {
        public LispFunction? KeyFn;
        public bool FromEnd, HasIV;
        public LispObject IV;
        public int? Start, End;
        public bool KeySet, FromEndSet, StartSet, EndSet, IVSet, Unknown;
    }

    private static void ApplyReduceKw(ref ReduceKwArgs kw, LispObject key, LispObject val)
    {
        if (key is not Symbol s)
            throw new LispErrorException(new LispProgramError($"REDUCE: keyword must be a symbol, got {key}"));
        switch (s.Name)
        {
            case "KEY": if (!kw.KeySet) { if (val is not Nil) kw.KeyFn = CoerceToFunctionFor(val, "REDUCE"); kw.KeySet = true; } break;
            case "FROM-END": if (!kw.FromEndSet) { kw.FromEnd = IsTruthy(val); kw.FromEndSet = true; } break;
            case "START": if (!kw.StartSet) { kw.Start = SeqBoundIndex(val, "REDUCE") ?? 0; kw.StartSet = true; } break;
            case "END": if (!kw.EndSet) { kw.End = SeqBoundIndex(val, "REDUCE", true); kw.EndSet = true; } break;
            case "INITIAL-VALUE": if (!kw.IVSet) { kw.IV = val; kw.HasIV = true; kw.IVSet = true; } break;
            case "ALLOW-OTHER-KEYS": break;
            default: kw.Unknown = true; break;
        }
    }

    private static LispObject ReduceWith(LispFunction fn, LispObject seq, in ReduceKwArgs kw, bool? allowOtherKeys)
    {
        if (kw.Unknown && allowOtherKeys != true)
            throw new LispErrorException(new LispProgramError("REDUCE: unknown keyword argument"));
        return ReduceCore(fn, seq, kw.KeyFn, kw.FromEnd, kw.HasIV, kw.IV, kw.Start, kw.End);
    }

    public static LispObject Reduce(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("REDUCE: too few arguments"));
        if ((args.Length - 2) % 2 != 0)
            throw new LispErrorException(new LispProgramError("REDUCE: odd number of keyword arguments"));
        bool? allowOtherKeys = null;
        for (int i = 2; i < args.Length - 1 && allowOtherKeys == null; i += 2)
            allowOtherKeys = AllowOtherKeysValue(args[i], args[i + 1]);
        var kw = default(ReduceKwArgs);
        for (int i = 2; i < args.Length - 1; i += 2)
            ApplyReduceKw(ref kw, args[i], args[i + 1]);
        return ReduceWith(CoerceToFunctionFor(args[0], "REDUCE"), args[1], kw, allowOtherKeys);
    }

    /// <summary>(REDUCE function sequence) as a direct entry: the two arguments arrive
    /// in registers instead of an array built for the variadic entry to walk.</summary>
    public static LispObject Reduce2(LispObject fn, LispObject seq) =>
        ReduceCore(CoerceToFunctionFor(fn, "REDUCE"), seq, null, false, false, Nil.Instance, null, null);

    public static LispObject Reduce4(LispObject fn, LispObject seq, LispObject k1, LispObject v1)
    {
        var kw = default(ReduceKwArgs);
        ApplyReduceKw(ref kw, k1, v1);
        return ReduceWith(CoerceToFunctionFor(fn, "REDUCE"), seq, kw, AllowOtherKeysValue(k1, v1));
    }

    public static LispObject Reduce6(LispObject fn, LispObject seq,
                                     LispObject k1, LispObject v1, LispObject k2, LispObject v2)
    {
        var kw = default(ReduceKwArgs);
        ApplyReduceKw(ref kw, k1, v1);
        ApplyReduceKw(ref kw, k2, v2);
        return ReduceWith(CoerceToFunctionFor(fn, "REDUCE"), seq, kw,
            AllowOtherKeysValue(k1, v1) ?? AllowOtherKeysValue(k2, v2));
    }

    public static LispObject Reduce8(LispObject fn, LispObject seq,
                                     LispObject k1, LispObject v1, LispObject k2, LispObject v2,
                                     LispObject k3, LispObject v3)
    {
        var kw = default(ReduceKwArgs);
        ApplyReduceKw(ref kw, k1, v1);
        ApplyReduceKw(ref kw, k2, v2);
        ApplyReduceKw(ref kw, k3, v3);
        return ReduceWith(CoerceToFunctionFor(fn, "REDUCE"), seq, kw,
            AllowOtherKeysValue(k1, v1) ?? AllowOtherKeysValue(k2, v2) ?? AllowOtherKeysValue(k3, v3));
    }

    private static LispObject ReduceCore(LispFunction fn, LispObject seq, LispFunction? keyFn,
                                         bool fromEnd, bool hasIV, LispObject iv,
                                         int? startOpt, int? endOpt)
        => MultipleValues.Alone(ReduceScan(fn, seq, keyFn, fromEnd, hasIV, iv, startOpt, endOpt));

    private static LispObject ReduceScan(LispFunction fn, LispObject seq, LispFunction? keyFn,
                                         bool fromEnd, bool hasIV, LispObject iv,
                                         int? startOpt, int? endOpt)
    {
        seq = AsSequenceArg(seq);
        int len = SeqLengthChecked(seq, "REDUCE");
        int start = startOpt ?? 0;
        int end = endOpt ?? len;
        CheckBoundingIndices(start, end, len, "REDUCE");
        int count = end - start;
        var empty = false;

        if (seq is LispVector vec)
        {
            if (count == 0) empty = true;
            else if (fromEnd)
            {
                var result = hasIV ? iv : ApplyKeyFn(keyFn, vec[end - 1]);
                int i = hasIV ? end - 1 : end - 2;
                for (; i >= start; i--)
                    result = UnwrapMv(fn.Invoke2(ApplyKeyFn(keyFn, vec[i]), result));
                return result;
            }
            else
            {
                var result = hasIV ? iv : ApplyKeyFn(keyFn, vec[start]);
                int i = hasIV ? start : start + 1;
                for (; i < end; i++)
                    result = UnwrapMv(fn.Invoke2(result, ApplyKeyFn(keyFn, vec[i])));
                return result;
            }
        }
        else if (seq is LispString str)
        {
            if (count == 0) empty = true;
            else if (fromEnd)
            {
                var result = hasIV ? iv : ApplyKeyFn(keyFn, LispChar.Make(str[end - 1]));
                int i = hasIV ? end - 1 : end - 2;
                for (; i >= start; i--)
                    result = UnwrapMv(fn.Invoke2(ApplyKeyFn(keyFn, LispChar.Make(str[i])), result));
                return result;
            }
            else
            {
                var result = hasIV ? iv : ApplyKeyFn(keyFn, LispChar.Make(str[start]));
                int i = hasIV ? start : start + 1;
                for (; i < end; i++)
                    result = UnwrapMv(fn.Invoke2(result, ApplyKeyFn(keyFn, LispChar.Make(str[i]))));
                return result;
            }
        }

        if (empty || count == 0)
            return hasIV ? iv : UnwrapMv(fn.Invoke(Array.Empty<LispObject>()));

        if (fromEnd)
        {
            // A list has no reverse index: collect the range once, then fold it
            // from the right.
            var elems = new LispObject[count];
            var cur = seq;
            for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
            int n = 0;
            for (int i = start; i < end && cur is Cons c2; i++) { elems[n++] = c2.Car; cur = c2.Cdr; }
            var result = hasIV ? iv : ApplyKeyFn(keyFn, elems[n - 1]);
            int idx = hasIV ? n - 1 : n - 2;
            for (; idx >= 0; idx--)
                result = UnwrapMv(fn.Invoke2(ApplyKeyFn(keyFn, elems[idx]), result));
            return result;
        }

        // A forward fold over a list walks the conses in place.
        var cell = seq;
        for (int i = 0; i < start && cell is Cons sc; i++) cell = sc.Cdr;
        LispObject acc;
        if (hasIV) acc = iv;
        else
        {
            var first = (Cons)cell;
            acc = ApplyKeyFn(keyFn, first.Car);
            cell = first.Cdr;
        }
        int remaining = hasIV ? count : count - 1;
        for (int i = 0; i < remaining && cell is Cons c; i++)
        {
            acc = UnwrapMv(fn.Invoke2(acc, ApplyKeyFn(keyFn, c.Car)));
            cell = c.Cdr;
        }
        return acc;
    }

    // Helper: coerce list of elements back to same sequence type as original
    private static LispObject CoerceResult(System.Collections.Generic.List<LispObject> elems, LispObject origSeq)
    {
        if (origSeq is Cons || origSeq is Nil)
            return List(elems.ToArray());
        if (origSeq is LispString)
        {
            var chars = new char[elems.Count];
            for (int i = 0; i < elems.Count; i++)
                chars[i] = ((LispChar)elems[i]).Value;
            return new LispString(new string(chars));
        }
        if (origSeq is LispVector ov)
        {
            var items = elems.ToArray();
            return new LispVector(items, ov.ElementTypeName);
        }
        throw new LispErrorException(new LispTypeError("not a sequence", origSeq));
    }

    // REMOVE: (remove item sequence &key test test-not key count from-end start end)
    // --- element predicates as structs, not delegates ---
    //
    // RemoveCore and friends used to take a Func<LispObject, bool>. Every call
    // therefore allocated a display class (the lambda captures the item or the
    // predicate plus the SeqKwArgs struct, which is copied into it) and a delegate
    // on top -- about 128 bytes before the walk even started, on a call whose
    // whole job might be two conses. Measured: (remove 2 '(1 2 3)) cost 247.9
    // bytes where SBCL costs 15.7.
    //
    // A struct type parameter is the same shape the bit-vector word ops already
    // use here: the JIT specialises the method per struct, so Match is a direct
    // call and nothing is allocated. IElemMatch exists only to constrain it.
    private interface IElemMatch { bool Match(LispObject elem); }

    /// <summary>The element test of FIND / POSITION / COUNT / REMOVE with an item: the plain EQL
    /// test (no :key, no :test-not) on a fixnum or an object EQL compares by identity is a
    /// pointer or value compare of its own; anything else goes through SeqTestMatch.</summary>
    private readonly struct SeqMatcher
    {
        private readonly LispObject _item;
        internal LispObject Item => _item;
        private readonly int _mode;   // 0 general, 1 identity, 2 fixnum, 3 fixnum under =, 4 character
        private readonly long _fx;
        public SeqMatcher(LispObject item, in SeqKwArgs kw)
        {
            _item = item; _mode = 0; _fx = 0;
            if (kw.Key == null && kw.TestNot == null && kw.TestKind == SeqTestKind.NumEq
                && UnwrapMv(item) is Fixnum nf)
            {
                // (= fixnum x): a fixnum element compares by value; anything else by =.
                _item = nf; _mode = 3; _fx = nf.Value;
            }
            else if (kw.Key == null && kw.TestNot == null && kw.TestKind == SeqTestKind.Eql)
            {
                var it = UnwrapMv(item);
                if (it is Fixnum f) { _item = it; _mode = 2; _fx = f.Value; }
                else if (it is LispChar lc) { _item = it; _mode = 4; _fx = lc.Value; }
                else if (it is Symbol or Nil or T or Cons or LispString) { _item = it; _mode = 1; }
            }
        }
        [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
        public bool Match(LispObject elem, in SeqKwArgs kw)
        {
            if (_mode == 2) return ReferenceEquals(elem, _item) || (elem is Fixnum g && g.Value == _fx);
            if (_mode == 1) return ReferenceEquals(elem, _item);
            if (_mode == 4) return ReferenceEquals(elem, _item) || (elem is LispChar gc && gc.Value == _fx);
            if (_mode == 3) return elem is Fixnum g3 ? g3.Value == _fx : IsTrueNumEq(_item, elem);
            return SeqTestMatch(_item, elem, kw);
        }
    }

    /// <summary>(REMOVE item seq ...) / (DELETE item seq ...): the element matches
    /// when the :TEST (or :TEST-NOT) says it equals ITEM, after :KEY.</summary>
    private readonly struct ItemMatch : IElemMatch
    {
        private readonly SeqMatcher _m;
        private readonly SeqKwArgs _kw;
        public ItemMatch(LispObject item, SeqKwArgs kw) { _kw = kw; _m = new SeqMatcher(item, kw); }
        public bool Match(LispObject elem) => _m.Match(elem, _kw);

        /// <summary>The item is a character compared with EQL or CHAR= (no :key, no :test-not):
        /// a string can then be searched by its characters.</summary>
        public bool TryCharItem(out char c)
        {
            c = '\0';
            if (_kw.Key != null || _kw.TestNot != null) return false;
            if (_kw.TestKind != SeqTestKind.Eql && _kw.TestKind != SeqTestKind.CharEq) return false;
            if (UnwrapMv(_m.Item) is not LispChar lc) return false;
            c = lc.Value;
            return true;
        }
    }

    /// <summary>(REMOVE-IF pred seq ...) and its NOT / DELETE variants: the element
    /// matches when the predicate says so, after :KEY.</summary>
    private readonly struct PredMatch : IElemMatch
    {
        private readonly LispFunction _pred;
        private readonly SeqKwArgs _kw;
        private readonly SeqPredKind _kind;
        private readonly bool _negate;
        public PredMatch(LispFunction pred, SeqKwArgs kw, bool negate)
        { _pred = pred; _kw = kw; _kind = SeqPredKindOf(kw, pred); _negate = negate; }
        public bool Match(LispObject elem)
        {
            var t = SeqPredicateMatch(_pred, elem, _kw, _kind);
            return _negate ? !t : t;
        }
    }

    public static LispObject RemoveFull(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("REMOVE: too few arguments"));
        var item = args[0];
        var seq = args[1];
        seq = AsSequenceArg(seq);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("REMOVE: not a sequence", seq));
        var kw = ParseSeqKwArgs(args, 2, "REMOVE");
        return RemoveCore(seq, kw, new ItemMatch(item, kw));
    }

    // 2-arg direct entry: (remove item seq): no keywords, default EQL test.
    // Skips the args array and the ParseSeqKwArgs scan (a default SeqKwArgs is
    // exactly its zero-keyword result). See RemoveIf2.
    public static LispObject Remove2(LispObject item, LispObject seq)
    {
        seq = AsSequenceArg(seq);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("REMOVE: not a sequence", seq));
        var kw = new SeqKwArgs();
        return RemoveCore(seq, kw, new ItemMatch(item, kw));
    }

    private static void CheckSequenceArg(LispObject seq, string fnName)
    {
        seq = AsSequenceArg(seq);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError($"{fnName}: not a sequence", seq));
    }

    public static LispObject Remove4(LispObject item, LispObject seq, LispObject k, LispObject v)
    {
        CheckSequenceArg(seq, "REMOVE");
        var kw = ParseSeqKwPairs(k, v, "REMOVE");
        return RemoveCore(seq, kw, new ItemMatch(item, kw));
    }

    public static LispObject Remove6(LispObject item, LispObject seq,
                                     LispObject k1, LispObject v1, LispObject k2, LispObject v2)
    {
        CheckSequenceArg(seq, "REMOVE");
        var kw = ParseSeqKwPairs(k1, v1, k2, v2, "REMOVE");
        return RemoveCore(seq, kw, new ItemMatch(item, kw));
    }

    // REMOVE-IF: (remove-if predicate sequence &key key count from-end start end)
    public static LispObject RemoveIf(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("REMOVE-IF: too few arguments"));
        var predFn = CoerceToFunctionFor(args[0], "REMOVE-IF");
        var seq = args[1];
        seq = AsSequenceArg(seq);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("REMOVE-IF: not a sequence", seq));
        return RemoveIfCore(predFn, seq, ParseSeqKwArgs(args, 2, "REMOVE-IF", ifForm: true));
    }

    // 2-arg direct entry: (remove-if pred seq): no keywords. The compiler calls
    // this shape frequently (bound-name filtering). Skips the args array and the
    // ParseSeqKwArgs scan; a default SeqKwArgs is exactly ParseSeqKwArgs's zero-
    // keyword result (all fields default).
    public static LispObject RemoveIf2(LispObject pred, LispObject seq)
    {
        var predFn = CoerceToFunctionFor(pred, "REMOVE-IF");
        seq = AsSequenceArg(seq);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("REMOVE-IF: not a sequence", seq));
        return RemoveIfCore(predFn, seq, new SeqKwArgs());
    }

    // 2-arg direct entry: (remove-if-not pred seq): no keywords. Inverts the
    // predicate inline (negate: true) rather than allocating a negating wrapper
    // LispFunction the way the args-array registration does, so both the args
    // array and the per-element wrapper InvokeSlow are avoided. See RemoveIf2.
    public static LispObject RemoveIfNot2(LispObject pred, LispObject seq)
    {
        var predFn = CoerceToFunctionFor(pred, "REMOVE-IF-NOT");
        seq = AsSequenceArg(seq);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("REMOVE-IF-NOT: not a sequence", seq));
        return RemoveIfCore(predFn, seq, new SeqKwArgs(), negate: true);
    }

    public static LispObject RemoveIf4(LispObject pred, LispObject seq, LispObject k1, LispObject v1)
    {
        var predFn = CoerceToFunctionFor(pred, "REMOVE-IF");
        CheckSequenceArg(seq, "REMOVE-IF");
        return RemoveIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, "REMOVE-IF", ifForm: true));
    }

    public static LispObject RemoveIf6(LispObject pred, LispObject seq,
                                       LispObject k1, LispObject v1, LispObject k2, LispObject v2)
    {
        var predFn = CoerceToFunctionFor(pred, "REMOVE-IF");
        CheckSequenceArg(seq, "REMOVE-IF");
        return RemoveIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, k2, v2, "REMOVE-IF", ifForm: true));
    }

    public static LispObject RemoveIfNot4(LispObject pred, LispObject seq, LispObject k1, LispObject v1)
    {
        var predFn = CoerceToFunctionFor(pred, "REMOVE-IF-NOT");
        CheckSequenceArg(seq, "REMOVE-IF");
        return RemoveIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, "REMOVE-IF", ifForm: true), negate: true);
    }

    public static LispObject RemoveIfNot6(LispObject pred, LispObject seq,
                                          LispObject k1, LispObject v1, LispObject k2, LispObject v2)
    {
        var predFn = CoerceToFunctionFor(pred, "REMOVE-IF-NOT");
        CheckSequenceArg(seq, "REMOVE-IF");
        return RemoveIfCore(predFn, seq, ParseSeqKwPairs(k1, v1, k2, v2, "REMOVE-IF", ifForm: true), negate: true);
    }

    private static LispObject RemoveIfCore(LispFunction predFn, LispObject seq, SeqKwArgs kw, bool negate = false)
    {
        return RemoveCore(seq, kw, new PredMatch(predFn, kw, negate));
    }

    // DELETE: like REMOVE but destructive on list arguments (in-place splice).
    public static LispObject DeleteFull(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("DELETE: too few arguments"));
        var item = args[0];
        var seq = args[1];
        seq = AsSequenceArg(seq);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("DELETE: not a sequence", seq));
        var kw = ParseSeqKwArgs(args, 2, "DELETE");
        return RemoveCore(seq, kw, new ItemMatch(item, kw), destructive: true);
    }

    /// <summary>(DELETE item seq) with no keywords, as a direct entry. The list
    /// path allocates nothing at all now, so the argument array the call used to
    /// build was the whole cost of a DELETE that splices in place.</summary>
    public static LispObject Delete2(LispObject item, LispObject seq)
    {
        seq = AsSequenceArg(seq);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("DELETE: not a sequence", seq));
        var kw = new SeqKwArgs();
        return RemoveCore(seq, kw, new ItemMatch(item, kw), destructive: true);
    }

    public static LispObject Delete4(LispObject item, LispObject seq, LispObject k, LispObject v)
    {
        CheckSequenceArg(seq, "DELETE");
        var kw = ParseSeqKwPairs(k, v, "DELETE");
        return RemoveCore(seq, kw, new ItemMatch(item, kw), destructive: true);
    }

    public static LispObject Delete6(LispObject item, LispObject seq,
                                     LispObject k1, LispObject v1, LispObject k2, LispObject v2)
    {
        CheckSequenceArg(seq, "DELETE");
        var kw = ParseSeqKwPairs(k1, v1, k2, v2, "DELETE");
        return RemoveCore(seq, kw, new ItemMatch(item, kw), destructive: true);
    }

    // DELETE-IF: like REMOVE-IF but destructive on list arguments.
    public static LispObject DeleteIf(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("DELETE-IF: too few arguments"));
        var predFn = CoerceToFunctionFor(args[0], "DELETE-IF");
        var seq = args[1];
        seq = AsSequenceArg(seq);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("DELETE-IF: not a sequence", seq));
        var kw = ParseSeqKwArgs(args, 2, "DELETE-IF", ifForm: true);
        return RemoveCore(seq, kw, new PredMatch(predFn, kw, false), destructive: true);
    }

    public static LispObject DeleteIf2(LispObject pred, LispObject seq)
    {
        var predFn = CoerceToFunctionFor(pred, "DELETE-IF");
        seq = AsSequenceArg(seq);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("DELETE-IF: not a sequence", seq));
        var kw = new SeqKwArgs();
        return RemoveCore(seq, kw, new PredMatch(predFn, kw, false), destructive: true);
    }

    public static LispObject DeleteIf4(LispObject pred, LispObject seq, LispObject k1, LispObject v1)
    {
        var predFn = CoerceToFunctionFor(pred, "DELETE-IF");
        CheckSequenceArg(seq, "DELETE-IF");
        var kw = ParseSeqKwPairs(k1, v1, "DELETE-IF", ifForm: true);
        return RemoveCore(seq, kw, new PredMatch(predFn, kw, false), destructive: true);
    }

    public static LispObject DeleteIf6(LispObject pred, LispObject seq,
                                       LispObject k1, LispObject v1, LispObject k2, LispObject v2)
    {
        var predFn = CoerceToFunctionFor(pred, "DELETE-IF");
        CheckSequenceArg(seq, "DELETE-IF");
        var kw = ParseSeqKwPairs(k1, v1, k2, v2, "DELETE-IF", ifForm: true);
        return RemoveCore(seq, kw, new PredMatch(predFn, kw, false), destructive: true);
    }

    // Core remove logic shared by REMOVE/REMOVE-IF (destructive=false) and
    // DELETE/DELETE-IF (destructive=true). When destructive and the sequence is a
    // list, matched conses are spliced out of the original chain in place (SBCL
    // semantics) so code that discards the return value and relies on in-place
    // mutation, e.g. Maxima rempropchk / mfunction-delete, works.
    /// <summary>Element I of a vector or string, for the indexed REMOVE walk. A
    /// method rather than the `i =&gt; vec[i]` lambda it replaces: that lambda captured
    /// a local of RemoveCore, which forced the whole method.s closure -- including
    /// the ones the LIST path uses -- onto the heap on every call, list or not.
    /// Measured at 96 bytes a call before the walk began.</summary>
    private static LispObject SeqElemAt(LispObject seq, int i)
        => seq is LispVector v ? v[i]
         : seq is LispString s ? LispChar.Make(s[i])
         : throw new LispErrorException(new LispTypeError("not an indexed sequence", seq));

    private static LispObject RemoveCore<TMatch>(LispObject seq, SeqKwArgs kw, TMatch matches,
                                                bool destructive = false)
        where TMatch : struct, IElemMatch
        => MultipleValues.Alone(RemoveScan(seq, kw, matches, destructive));

    private static LispObject RemoveScan<TMatch>(LispObject seq, SeqKwArgs kw, TMatch matches,
                                                bool destructive)
        where TMatch : struct, IElemMatch
    {
        if (seq is Nil)
        {
            CheckBoundingIndices(kw.Start, kw.End ?? 0, 0, "REMOVE");
            return Nil.Instance;
        }

        // Normalize count: null=no limit, <=0=remove nothing
        int? maxRemove = kw.Count;
        if (maxRemove.HasValue && maxRemove.Value <= 0)
        {
            // count <= 0: nothing removed. Return the original sequence (eq), as
            // SBCL/CCL and most impls do: many libraries (e.g. Maxima add2lnc)
            // rely on (setq x (delete .. x)) preserving x's cons when no element
            // matches, so a following (nconc x ..) still mutates the shared list.
            int boundLen = SeqLengthChecked(seq, "REMOVE");
            CheckBoundingIndices(kw.Start, kw.End ?? boundLen, boundLen, "REMOVE");
            return seq;
        }

        if (seq is LispVector vec)
        {
            int len = vec.Length;
            int start = kw.Start;
            int end = kw.End ?? len;
            CheckBoundingIndices(start, end, len, "REMOVE");
            return RemoveCoreIndexed(len, start, end, kw.FromEnd, maxRemove, seq);
        }
        if (seq is LispString str)
        {
            int len = str.Length;
            int start = kw.Start;
            int end = kw.End ?? len;
            CheckBoundingIndices(start, end, len, "REMOVE");
            return RemoveCoreIndexed(len, start, end, kw.FromEnd, maxRemove, seq);
        }
        // List. Its length is only needed to validate the range, the walk itself
        // does not need it, so it is computed here rather than inside the walk.
        {
            int listLen = (int)((Fixnum)Length(seq)).Value;
            CheckBoundingIndices(kw.Start, kw.End ?? listLen, listLen, "REMOVE");
        }
        return RemoveCoreList(seq, kw.Start, kw.End, kw.FromEnd, maxRemove);

        // Local function for indexed sequences (vector/string)
        LispObject RemoveCoreIndexed(int len, int start, int end, bool fromEnd, int? maxRem,
            LispObject origSeq)
        {
            // Mark the positions to drop in a bitmap, then fill one exact-size result.
            // Both branches used to grow a List of every surviving element and copy it
            // out; the FROM-END one added a List of match positions and a HashSet on
            // top. A REMOVE that matched nothing paid for all of that before finding
            // out the answer was the sequence it was handed.
            int words = (len + 63) >> 6;
            Span<ulong> drop = words <= 8 ? stackalloc ulong[8] : new ulong[words];
            drop = drop.Slice(0, words);
            drop.Clear();
            int removed = 0;

            if (fromEnd && maxRem.HasValue)
            {
                // Walk backwards and stop once COUNT matches have been found: that is
                // the whole point of :FROM-END, and the test is what it costs. Marking
                // every match forward and then unmarking the leftmost excess gave the
                // same answer while calling the test on the entire range -- observable
                // when the test has side effects, and needless work when COUNT is small
                // and the sequence is long.
                for (int i = end - 1; i >= start && removed < maxRem.Value; i--)
                    if (matches.Match(SeqElemAt(origSeq, i)))
                    { drop[i >> 6] |= 1UL << (i & 63); removed++; }
            }
            else
            {
                for (int i = start; i < end; i++)
                {
                    if (maxRem.HasValue && removed >= maxRem.Value) break;
                    if (matches.Match(SeqElemAt(origSeq, i)))
                    { drop[i >> 6] |= 1UL << (i & 63); removed++; }
                }
            }

            if (removed == 0) return origSeq;

            // DELETE on a vector with a fill pointer works in place, as on SBCL: the
            // survivors move down, the fill pointer drops by the number removed,
            // and the vector itself is the result. event-emitter keeps its
            // listeners in such a vector and goes on calling VECTOR-PUSH-EXTEND on
            // what DELETE returned.
            if (destructive && origSeq is LispVector fpv && fpv.HasFillPointer)
            {
                int w = 0;
                for (int i = 0; i < len; i++)
                    if ((drop[i >> 6] & (1UL << (i & 63))) == 0)
                    {
                        if (w != i) fpv[w] = fpv[i];
                        w++;
                    }
                fpv.SetFillPointer(len - removed);
                return fpv;
            }

            if (origSeq is LispString ostr)
            {
                Span<char> chars = len - removed <= 512 ? stackalloc char[len - removed] : new char[len - removed];
                int ci = 0;
                for (int i = 0; i < len; i++)
                    if ((drop[i >> 6] & (1UL << (i & 63))) == 0) chars[ci++] = ostr[i];
                return new LispString(chars.ToString());
            }
            {
                var items = new LispObject[len - removed];
                int k = 0;
                for (int i = 0; i < len; i++)
                    if ((drop[i >> 6] & (1UL << (i & 63))) == 0) items[k++] = SeqElemAt(origSeq, i);
                return new LispVector(items, ((LispVector)origSeq).ElementTypeName);
            }
        }

        // Local function for list sequences
        LispObject RemoveCoreList(LispObject listSeq, int start, int? endOpt, bool fromEnd, int? maxRem)
        {
            // The ordinary shape -- whole list, no :count, no :from-end, not
            // destructive -- walks once and conses only the survivors. The
            // general path below first materialises every element into a List
            // and every dropped index into a HashSet, which is most of what
            // (remove-if p list) cost.
            // DELETE on a list, ordinary shape: splice the matches out of the
            // chain in place. Nothing is allocated at all -- the general path
            // below built a List of every element and a HashSet of every dropped
            // index to reach the same answer.
            if (destructive && !fromEnd && !maxRem.HasValue && start == 0 && endOpt == null)
            {
                var keptHead = listSeq;
                while (keptHead is Cons hc && matches.Match(hc.Car)) keptHead = hc.Cdr;
                if (keptHead is not Cons prev) return keptHead;
                for (var cur1 = prev.Cdr; cur1 is Cons c1; cur1 = c1.Cdr)
                {
                    if (matches.Match(c1.Car)) prev.Cdr = c1.Cdr;
                    else prev = c1;
                }
                return keptHead;
            }

            if (!destructive && !fromEnd && start == 0 && endOpt == null)
            {
                // Find the first element to drop before consing anything. When
                // nothing matches -- which is most calls -- the answer is the list
                // itself and this allocates nothing at all; the previous version
                // copied the whole list and then threw the copy away. A :COUNT
                // only limits how many matches are dropped, so the same walk works.
                int remaining = maxRem ?? int.MaxValue;
                var first = listSeq;
                while (first is Cons f && !(remaining > 0 && matches.Match(f.Car))) first = f.Cdr;
                if (first is not Cons) return listSeq;
                remaining--;

                // Copy the untouched prefix, then walk the rest consing survivors.
                Cons? head = null, tail = null;
                for (var cur0 = listSeq; !ReferenceEquals(cur0, first); cur0 = ((Cons)cur0).Cdr)
                {
                    var cell = new Cons(((Cons)cur0).Car, Nil.Instance);
                    if (tail == null) head = cell; else tail.Cdr = cell;
                    tail = cell;
                }
                for (var cur0 = ((Cons)first).Cdr; cur0 is Cons c0; cur0 = c0.Cdr)
                {
                    if (remaining > 0 && matches.Match(c0.Car)) { remaining--; continue; }
                    var cell = new Cons(c0.Car, Nil.Instance);
                    if (tail == null) head = cell; else tail.Cdr = cell;
                    tail = cell;
                }
                return head ?? (LispObject)Nil.Instance;
            }

            // Collect all elements with indices
            var allElems = new System.Collections.Generic.List<LispObject>();
            for (var cur = listSeq; cur is Cons c; cur = c.Cdr)
                allElems.Add(c.Car);
            int len = allElems.Count;
            int end = endOpt ?? len;

            // Determine which indices to drop. matches() (which may run user :key /
            // :test code) is called in the same order/count as the legacy build path.
            var removeSet = new System.Collections.Generic.HashSet<int>();
            if (fromEnd && maxRem.HasValue)
            {
                // FROM-END with COUNT: walk backwards and stop at COUNT matches, so the
                // test is applied to the right end of the sequence and no further. The
                // elements are already in hand here, so this costs nothing over the
                // forward scan it replaces.
                for (int i = end - 1; i >= start && removeSet.Count < maxRem.Value; i--)
                    if (matches.Match(allElems[i])) removeSet.Add(i);
            }
            else
            {
                // Forward scan: drop first maxRem matches in [start,end)
                int removed = 0;
                for (int i = 0; i < len; i++)
                    if (i >= start && i < end && (!maxRem.HasValue || removed < maxRem.Value) && matches.Match(allElems[i]))
                        { removeSet.Add(i); removed++; }
            }

            // Nothing removed: return the original list (eq), so a no-op delete still
            // shares structure for a following nconc (Maxima add2lnc idiom).
            if (removeSet.Count == 0) return listSeq;

            if (destructive)
            {
                // Splice matched conses out of the original chain in place, relinking
                // each survivor's cdr past the removed run. Returns the (possibly new) head.
                LispObject newHead = listSeq;
                Cons? prev = null;
                int idx = 0;
                for (var cur = listSeq; cur is Cons c; idx++)
                {
                    var next = c.Cdr;
                    if (removeSet.Contains(idx))
                    {
                        if (prev == null) newHead = next; else prev.Cdr = next;
                    }
                    else prev = c;
                    cur = next;
                }
                return newHead;
            }

            // Non-destructive (remove): build a fresh list of the survivors.
            var result = new System.Collections.Generic.List<LispObject>(len - removeSet.Count);
            for (int i = 0; i < len; i++)
                if (!removeSet.Contains(i)) result.Add(allElems[i]);
            return List(result.ToArray());
        }
    }

    // SUBSTITUTE: (substitute newitem olditem sequence &key test test-not key count from-end start end)
    /// <summary>(SUBSTITUTE new old seq) with no keywords, as a direct entry: the
    /// call reaches it without building an argument array, which was the last
    /// allocation left on this shape once the core stopped materialising the whole
    /// sequence twice.</summary>
    public static LispObject Substitute3(LispObject newitem, LispObject olditem, LispObject seq)
    {
        seq = AsSequenceArg(seq);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("SUBSTITUTE: not a sequence", seq));
        var kw = new SeqKwArgs();
        return SubstituteCore(newitem, seq, kw, new ItemMatch(olditem, kw));
    }

    public static LispObject Substitute5(LispObject newitem, LispObject olditem, LispObject seq,
                                         LispObject k1, LispObject v1)
    {
        CheckSequenceArg(seq, "SUBSTITUTE");
        var kw = ParseSeqKwPairs(k1, v1, "SUBSTITUTE");
        return SubstituteCore(newitem, seq, kw, new ItemMatch(olditem, kw));
    }

    public static LispObject Substitute7(LispObject newitem, LispObject olditem, LispObject seq,
                                         LispObject k1, LispObject v1, LispObject k2, LispObject v2)
    {
        CheckSequenceArg(seq, "SUBSTITUTE");
        var kw = ParseSeqKwPairs(k1, v1, k2, v2, "SUBSTITUTE");
        return SubstituteCore(newitem, seq, kw, new ItemMatch(olditem, kw));
    }

    public static LispObject SubstituteFull(LispObject[] args)
    {
        if (args.Length < 3)
            throw new LispErrorException(new LispProgramError("SUBSTITUTE: too few arguments"));
        var newitem = args[0];
        var olditem = args[1];
        var seq = args[2];
        seq = AsSequenceArg(seq);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("SUBSTITUTE: not a sequence", seq));
        var kw = ParseSeqKwArgs(args, 3, "SUBSTITUTE");
        return SubstituteCore(newitem, seq, kw, new ItemMatch(olditem, kw));
    }

    // SUBSTITUTE-IF: (substitute-if newitem predicate sequence &key key count from-end start end)
    public static LispObject SubstituteIf(LispObject[] args)
    {
        if (args.Length < 3)
            throw new LispErrorException(new LispProgramError("SUBSTITUTE-IF: too few arguments"));
        var newitem = args[0];
        var predFn = CoerceToFunctionFor(args[1], "SUBSTITUTE-IF");
        var seq = args[2];
        seq = AsSequenceArg(seq);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("SUBSTITUTE-IF: not a sequence", seq));
        var kw = ParseSeqKwArgs(args, 3, "SUBSTITUTE-IF", ifForm: true);
        return SubstituteCore(newitem, seq, kw, new PredMatch(predFn, kw, false));
    }

    public static LispObject SubstituteIf3(LispObject newitem, LispObject pred, LispObject seq)
    {
        var predFn = CoerceToFunctionFor(pred, "SUBSTITUTE-IF");
        seq = AsSequenceArg(seq);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("SUBSTITUTE-IF: not a sequence", seq));
        var kw = new SeqKwArgs();
        return SubstituteCore(newitem, seq, kw, new PredMatch(predFn, kw, false));
    }

    public static LispObject SubstituteIf5(LispObject newitem, LispObject pred, LispObject seq,
                                           LispObject k1, LispObject v1)
    {
        var predFn = CoerceToFunctionFor(pred, "SUBSTITUTE-IF");
        CheckSequenceArg(seq, "SUBSTITUTE-IF");
        var kw = ParseSeqKwPairs(k1, v1, "SUBSTITUTE-IF", ifForm: true);
        return SubstituteCore(newitem, seq, kw, new PredMatch(predFn, kw, false));
    }

    public static LispObject SubstituteIf7(LispObject newitem, LispObject pred, LispObject seq,
                                           LispObject k1, LispObject v1, LispObject k2, LispObject v2)
    {
        var predFn = CoerceToFunctionFor(pred, "SUBSTITUTE-IF");
        CheckSequenceArg(seq, "SUBSTITUTE-IF");
        var kw = ParseSeqKwPairs(k1, v1, k2, v2, "SUBSTITUTE-IF", ifForm: true);
        return SubstituteCore(newitem, seq, kw, new PredMatch(predFn, kw, false));
    }

    // NSUBSTITUTE: (nsubstitute newitem olditem sequence &key test test-not key count from-end start end)
    public static LispObject NsubstituteFull(LispObject[] args)
    {
        if (args.Length < 3)
            throw new LispErrorException(new LispProgramError("NSUBSTITUTE: too few arguments"));
        var newitem = args[0];
        var olditem = args[1];
        var seq = args[2];
        seq = AsSequenceArg(seq);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("NSUBSTITUTE: not a sequence", seq));
        var kw = ParseSeqKwArgs(args, 3, "NSUBSTITUTE");
        return NsubstituteCore(newitem, seq, kw, new ItemMatch(olditem, kw));
    }

    public static LispObject Nsubstitute3(LispObject newitem, LispObject olditem, LispObject seq)
    {
        seq = AsSequenceArg(seq);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("NSUBSTITUTE: not a sequence", seq));
        var kw = new SeqKwArgs();
        return NsubstituteCore(newitem, seq, kw, new ItemMatch(olditem, kw));
    }

    public static LispObject Nsubstitute5(LispObject newitem, LispObject olditem, LispObject seq,
                                          LispObject k1, LispObject v1)
    {
        CheckSequenceArg(seq, "NSUBSTITUTE");
        var kw = ParseSeqKwPairs(k1, v1, "NSUBSTITUTE");
        return NsubstituteCore(newitem, seq, kw, new ItemMatch(olditem, kw));
    }

    public static LispObject Nsubstitute7(LispObject newitem, LispObject olditem, LispObject seq,
                                          LispObject k1, LispObject v1, LispObject k2, LispObject v2)
    {
        CheckSequenceArg(seq, "NSUBSTITUTE");
        var kw = ParseSeqKwPairs(k1, v1, k2, v2, "NSUBSTITUTE");
        return NsubstituteCore(newitem, seq, kw, new ItemMatch(olditem, kw));
    }

    // NSUBSTITUTE-IF: (nsubstitute-if newitem predicate sequence &key key count from-end start end)
    public static LispObject NsubstituteIf(LispObject[] args)
    {
        if (args.Length < 3)
            throw new LispErrorException(new LispProgramError("NSUBSTITUTE-IF: too few arguments"));
        var newitem = args[0];
        var predFn = CoerceToFunctionFor(args[1], "NSUBSTITUTE-IF");
        var seq = args[2];
        seq = AsSequenceArg(seq);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("NSUBSTITUTE-IF: not a sequence", seq));
        var kw = ParseSeqKwArgs(args, 3, "NSUBSTITUTE-IF", ifForm: true);
        return NsubstituteCore(newitem, seq, kw, new PredMatch(predFn, kw, false));
    }

    public static LispObject NsubstituteIf3(LispObject newitem, LispObject pred, LispObject seq)
    {
        var predFn = CoerceToFunctionFor(pred, "NSUBSTITUTE-IF");
        seq = AsSequenceArg(seq);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("NSUBSTITUTE-IF: not a sequence", seq));
        var kw = new SeqKwArgs();
        return NsubstituteCore(newitem, seq, kw, new PredMatch(predFn, kw, false));
    }

    public static LispObject NsubstituteIf5(LispObject newitem, LispObject pred, LispObject seq,
                                            LispObject k1, LispObject v1)
    {
        var predFn = CoerceToFunctionFor(pred, "NSUBSTITUTE-IF");
        CheckSequenceArg(seq, "NSUBSTITUTE-IF");
        var kw = ParseSeqKwPairs(k1, v1, "NSUBSTITUTE-IF", ifForm: true);
        return NsubstituteCore(newitem, seq, kw, new PredMatch(predFn, kw, false));
    }

    public static LispObject NsubstituteIf7(LispObject newitem, LispObject pred, LispObject seq,
                                            LispObject k1, LispObject v1, LispObject k2, LispObject v2)
    {
        var predFn = CoerceToFunctionFor(pred, "NSUBSTITUTE-IF");
        CheckSequenceArg(seq, "NSUBSTITUTE-IF");
        var kw = ParseSeqKwPairs(k1, v1, k2, v2, "NSUBSTITUTE-IF", ifForm: true);
        return NsubstituteCore(newitem, seq, kw, new PredMatch(predFn, kw, false));
    }

    // Core substitute logic (non-destructive)
    private static LispObject SubstituteCore<TMatch>(LispObject newitem, LispObject seq,
                                                     SeqKwArgs kw, TMatch matches)
        where TMatch : struct, IElemMatch
        => MultipleValues.Alone(SubstituteScan(newitem, seq, kw, matches));

    private static LispObject SubstituteScan<TMatch>(LispObject newitem, LispObject seq,
                                                     SeqKwArgs kw, TMatch matches)
        where TMatch : struct, IElemMatch
    {
        if (seq is Nil)
        {
            CheckBoundingIndices(kw.Start, kw.End ?? 0, 0, "SUBSTITUTE");
            return Nil.Instance;
        }

        int? maxSub = kw.Count;
        if (maxSub.HasValue && maxSub.Value <= 0)
        {
            int boundLen = SeqLengthChecked(seq, "SUBSTITUTE");
            CheckBoundingIndices(kw.Start, kw.End ?? boundLen, boundLen, "SUBSTITUTE");
            return CopySeq(seq);
        }

        // The ordinary shape -- a list, whole range, no :COUNT, no :FROM-END --
        // walks once and conses the answer. The general path below materialises
        // every element into a List, builds a second List of the result, and
        // hands that to the coercion, which is three intermediates for a list it
        // could write directly.
        if (seq is Cons && !kw.FromEnd && !maxSub.HasValue && kw.Start == 0 && kw.End == null)
        {
            Cons? subHead = null, subTail = null;
            var cur = seq;
            for (; cur is Cons c; cur = c.Cdr)
            {
                var cell = new Cons(matches.Match(c.Car) ? newitem : c.Car, Nil.Instance);
                if (subTail == null) subHead = cell; else subTail.Cdr = cell;
                subTail = cell;
            }
            if (cur is not Nil)
                throw new LispErrorException(new LispTypeError("SUBSTITUTE: not a proper list", cur, Startup.Sym("LIST")));
            return subHead ?? (LispObject)Nil.Instance;
        }


        // A string or vector answer has exactly the input's length -- SUBSTITUTE
        // replaces elements, it never adds or drops one -- so it can be written
        // straight into a destination of that size. The general path below
        // materialises every element into a List, builds a second List of the
        // result, and hands that to the coercion: three intermediates for a
        // sequence whose size was known before the walk started. That is the
        // shape REMOVE was taken out of; SUBSTITUTE still had it.
        //
        // :FROM-END is left to the general path. It changes which elements are
        // replaced only together with :COUNT, and getting that right needs the
        // right-to-left marking pass the code below already has.
        if (!kw.FromEnd && (seq is LispString || seq is LispVector))
        {
            int flen = seq is LispString fs ? fs.Length : ((LispVector)seq).Length;
            int fstart = kw.Start;
            int fend = kw.End ?? flen;
            CheckBoundingIndices(fstart, fend, flen, "SUBSTITUTE");
            int subbed = 0;
            if (seq is LispString sstr)
            {
                Span<char> chars = flen <= 512 ? stackalloc char[flen] : new char[flen];
                if (matches is ItemMatch charMatch && charMatch.TryCharItem(out char itemChar))
                {
                    // A character item: compare characters, and make the replacement
                    // character only when something matches (a non-character NEWITEM is
                    // still an error then, and only then).
                    char newChar = '\0';
                    bool haveNew = false;
                    for (int i = 0; i < flen; i++)
                    {
                        char ch = sstr[i];
                        if (ch == itemChar && i >= fstart && i < fend && (!maxSub.HasValue || subbed < maxSub.Value))
                        {
                            if (!haveNew) { newChar = ((LispChar)newitem).Value; haveNew = true; }
                            subbed++;
                            chars[i] = newChar;
                        }
                        else chars[i] = ch;
                    }
                    return new LispString(chars.ToString());
                }
                for (int i = 0; i < flen; i++)
                {
                    var elem = LispChar.Make(sstr[i]);
                    bool take = i >= fstart && i < fend
                                && (!maxSub.HasValue || subbed < maxSub.Value)
                                && matches.Match(elem);
                    if (take) subbed++;
                    // The cast is what makes a non-character NEWITEM an error, as
                    // it was when the coercion did it -- and, as there, only when
                    // something actually matched.
                    chars[i] = ((LispChar)(take ? newitem : elem)).Value;
                }
                return new LispString(chars.ToString());
            }
            var svec = (LispVector)seq;
            var items = new LispObject[flen];
            for (int i = 0; i < flen; i++)
            {
                var elem = svec[i];
                bool take = i >= fstart && i < fend
                            && (!maxSub.HasValue || subbed < maxSub.Value)
                            && matches.Match(elem);
                if (take) subbed++;
                items[i] = take ? newitem : elem;
            }
            return new LispVector(items, svec.ElementTypeName);
        }

        // Collect elements
        var allElems = new System.Collections.Generic.List<LispObject>();
        int len;
        if (seq is LispVector vec)
        {
            len = vec.Length;
            for (int i = 0; i < len; i++) allElems.Add(vec[i]);
        }
        else if (seq is LispString str)
        {
            len = str.Length;
            for (int i = 0; i < len; i++) allElems.Add(LispChar.Make(str[i]));
        }
        else
        {
            var cur = seq;
            for (; cur is Cons c; cur = c.Cdr) allElems.Add(c.Car);
            if (cur is not Nil)
                throw new LispErrorException(new LispTypeError("SUBSTITUTE: not a proper list", cur, Startup.Sym("LIST")));
            len = allElems.Count;
        }

        int start = kw.Start;
        int end = kw.End ?? len;
        CheckBoundingIndices(start, end, len, "SUBSTITUTE");

        if (kw.FromEnd)
        {
            // FROM-END: scan right-to-left, mark positions to substitute
            var subSet = new System.Collections.Generic.HashSet<int>();
            int subbed = 0;
            for (int i = end - 1; i >= start; i--)
            {
                if (maxSub.HasValue && subbed >= maxSub.Value) break;
                if (matches.Match(allElems[i])) { subSet.Add(i); subbed++; }
            }
            var result = new System.Collections.Generic.List<LispObject>();
            for (int i = 0; i < len; i++)
                result.Add(subSet.Contains(i) ? newitem : allElems[i]);
            return CoerceResult(result, seq);
        }
        else
        {
            // Forward scan
            var result = new System.Collections.Generic.List<LispObject>();
            int subbed = 0;
            for (int i = 0; i < len; i++)
            {
                if (i >= start && i < end && (!maxSub.HasValue || subbed < maxSub.Value) && matches.Match(allElems[i]))
                {
                    result.Add(newitem);
                    subbed++;
                }
                else
                    result.Add(allElems[i]);
            }
            return CoerceResult(result, seq);
        }
    }

    // Core nsubstitute logic (destructive)
    private static LispObject NsubstituteCore<TMatch>(LispObject newitem, LispObject seq,
                                                      SeqKwArgs kw, TMatch matches)
        where TMatch : struct, IElemMatch
        => MultipleValues.Alone(NsubstituteScan(newitem, seq, kw, matches));

    private static LispObject NsubstituteScan<TMatch>(LispObject newitem, LispObject seq,
                                                      SeqKwArgs kw, TMatch matches)
        where TMatch : struct, IElemMatch
    {
        if (seq is Nil)
        {
            CheckBoundingIndices(kw.Start, kw.End ?? 0, 0, "NSUBSTITUTE");
            return Nil.Instance;
        }

        int? maxSub = kw.Count;
        if (maxSub.HasValue && maxSub.Value <= 0)
        {
            int boundLen = SeqLengthChecked(seq, "NSUBSTITUTE");
            CheckBoundingIndices(kw.Start, kw.End ?? boundLen, boundLen, "NSUBSTITUTE");
            return seq;
        }

        if (seq is LispVector vec)
        {
            int len = vec.Length;
            int start = kw.Start;
            int end = kw.End ?? len;
            CheckBoundingIndices(start, end, len, "NSUBSTITUTE");

            if (kw.FromEnd)
            {
                int subbed = 0;
                for (int i = end - 1; i >= start; i--)
                {
                    if (maxSub.HasValue && subbed >= maxSub.Value) break;
                    if (matches.Match(vec[i])) { vec.SetElement(i, newitem); subbed++; }
                }
            }
            else
            {
                int subbed = 0;
                for (int i = start; i < end; i++)
                {
                    if (maxSub.HasValue && subbed >= maxSub.Value) break;
                    if (matches.Match(vec[i])) { vec.SetElement(i, newitem); subbed++; }
                }
            }
            return seq;
        }
        if (seq is LispString str)
        {
            // A string is changed in place, like any other sequence (CLHS 17.3).
            int len = str.Length;
            int start = kw.Start;
            int end = kw.End ?? len;
            CheckBoundingIndices(start, end, len, "NSUBSTITUTE");
            var replacement = newitem as LispChar;
            int subbed = 0;
            if (kw.FromEnd)
            {
                for (int i = end - 1; i >= start; i--)
                {
                    if (maxSub.HasValue && subbed >= maxSub.Value) break;
                    if (matches.Match(LispChar.Make(str[i])))
                    {
                        if (replacement == null)
                            throw new LispErrorException(new LispTypeError("NSUBSTITUTE: a string takes only characters", newitem, Startup.Sym("CHARACTER")));
                        str[i] = replacement.Value; subbed++;
                    }
                }
            }
            else
            {
                for (int i = start; i < end; i++)
                {
                    if (maxSub.HasValue && subbed >= maxSub.Value) break;
                    if (matches.Match(LispChar.Make(str[i])))
                    {
                        if (replacement == null)
                            throw new LispErrorException(new LispTypeError("NSUBSTITUTE: a string takes only characters", newitem, Startup.Sym("CHARACTER")));
                        str[i] = replacement.Value; subbed++;
                    }
                }
            }
            return seq;
        }
        // List: modify in place via rplaca
        {
            if (!kw.FromEnd && !maxSub.HasValue && kw.Start == 0 && kw.End == null)
            {
                var cur0 = seq;
                for (; cur0 is Cons c0; cur0 = c0.Cdr)
                    if (matches.Match(c0.Car)) c0.Car = newitem;
                if (cur0 is not Nil)
                    throw new LispErrorException(new LispTypeError("NSUBSTITUTE: not a proper list", cur0, Startup.Sym("LIST")));
                return seq;
            }
            int listLen = 0;
            var cells = new System.Collections.Generic.List<Cons>();
            var cur = seq;
            for (; cur is Cons c; cur = c.Cdr, listLen++) cells.Add(c);
            if (cur is not Nil)
                throw new LispErrorException(new LispTypeError("NSUBSTITUTE: not a proper list", cur, Startup.Sym("LIST")));
            int start = kw.Start;
            int end = kw.End ?? listLen;
            CheckBoundingIndices(start, end, listLen, "NSUBSTITUTE");

            if (kw.FromEnd)
            {
                int subbed = 0;
                for (int i = end - 1; i >= start; i--)
                {
                    if (maxSub.HasValue && subbed >= maxSub.Value) break;
                    if (matches.Match(cells[i].Car)) { cells[i].Car = newitem; subbed++; }
                }
            }
            else
            {
                int subbed = 0;
                for (int i = start; i < end; i++)
                {
                    if (maxSub.HasValue && subbed >= maxSub.Value) break;
                    if (matches.Match(cells[i].Car)) { cells[i].Car = newitem; subbed++; }
                }
            }
            return seq;
        }
    }

    // REMOVE-DUPLICATES: (remove-duplicates sequence &key test test-not key from-end start end)
    public static LispObject RemoveDuplicatesFull(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError("REMOVE-DUPLICATES: too few arguments"));
        return RemoveDuplicatesCore(args[0], ParseSeqKwArgs(args, 1, "REMOVE-DUPLICATES"));
    }

    /// <summary>(REMOVE-DUPLICATES sequence) as a direct entry: the argument arrives in
    /// a register instead of an array built for the variadic entry to walk.</summary>
    public static LispObject RemoveDuplicates1(LispObject seq) =>
        RemoveDuplicatesCore(seq, new SeqKwArgs());

    public static LispObject RemoveDuplicates3(LispObject seq, LispObject k1, LispObject v1) =>
        RemoveDuplicatesCore(seq, ParseSeqKwPairs(k1, v1, "REMOVE-DUPLICATES"));

    public static LispObject RemoveDuplicates5(LispObject seq,
                                               LispObject k1, LispObject v1, LispObject k2, LispObject v2) =>
        RemoveDuplicatesCore(seq, ParseSeqKwPairs(k1, v1, k2, v2, "REMOVE-DUPLICATES"));

    // DELETE-DUPLICATES on a vector with a fill pointer works in place, as DELETE does.
    private static LispObject DeleteDuplicatesInPlace(LispObject orig, LispObject r)
    {
        if (orig is LispVector fv && fv.HasFillPointer && r is LispVector rv && !ReferenceEquals(fv, rv))
        {
            int n = rv.Length;
            for (int i = 0; i < n; i++) fv[i] = rv[i];
            fv.SetFillPointer(n);
            return fv;
        }
        return r;
    }

    public static LispObject DeleteDuplicatesFull(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError("DELETE-DUPLICATES: too few arguments"));
        return DeleteDuplicatesInPlace(args[0], RemoveDuplicatesCore(args[0], ParseSeqKwArgs(args, 1, "DELETE-DUPLICATES")));
    }

    public static LispObject DeleteDuplicates1(LispObject seq) =>
        DeleteDuplicatesInPlace(seq, RemoveDuplicatesCore(seq, new SeqKwArgs()));

    public static LispObject DeleteDuplicates3(LispObject seq, LispObject k1, LispObject v1) =>
        DeleteDuplicatesInPlace(seq, RemoveDuplicatesCore(seq, ParseSeqKwPairs(k1, v1, "DELETE-DUPLICATES")));

    public static LispObject DeleteDuplicates5(LispObject seq,
        LispObject k1, LispObject v1, LispObject k2, LispObject v2) =>
        DeleteDuplicatesInPlace(seq, RemoveDuplicatesCore(seq, ParseSeqKwPairs(k1, v1, k2, v2, "DELETE-DUPLICATES")));

    private static LispObject RemoveDuplicatesCore(LispObject seq, in SeqKwArgs kw)
    {
        var r = RemoveDuplicatesScan(seq, kw);
        return kw.Test != null || kw.TestNot != null || kw.Key != null ? MultipleValues.Alone(r) : r;
    }

    private const int ListScanLimit = 16;

    // A short list is scanned in place, without the indexing array.
    private static LispObject RemoveDuplicatesListSmall(LispObject seq, int start, int end,
                                                        int len, in SeqKwArgs kw)
    {
        int words = (len + 63) >> 6;
        Span<ulong> dup = words <= 8 ? stackalloc ulong[8] : new ulong[words];
        dup = dup.Slice(0, words);
        dup.Clear();
        bool anyDup = false;

        var ci = seq;
        for (int i = 0; i < start && ci is Cons cs; i++) ci = cs.Cdr;
        if (kw.FromEnd)
        {
            for (int i = start; i < end && ci is Cons c; i++, ci = c.Cdr)
            {
                if ((dup[i >> 6] & (1UL << (i & 63))) != 0) continue;
                var ki = ApplySeqKey(kw, c.Car);
                var cj = c.Cdr;
                for (int j = i + 1; j < end && cj is Cons c2; j++, cj = c2.Cdr)
                {
                    if ((dup[j >> 6] & (1UL << (j & 63))) != 0) continue;
                    if (SeqTestMatch2(ApplySeqKey(kw, c2.Car), ki, kw))
                    {
                        dup[j >> 6] |= 1UL << (j & 63);
                        anyDup = true;
                    }
                }
            }
        }
        else
        {
            for (int i = start; i < end && ci is Cons c; i++, ci = c.Cdr)
            {
                if ((dup[i >> 6] & (1UL << (i & 63))) != 0) continue;
                var ki = ApplySeqKey(kw, c.Car);
                var cj = c.Cdr;
                for (int j = i + 1; j < end && cj is Cons c2; j++, cj = c2.Cdr)
                {
                    if ((dup[j >> 6] & (1UL << (j & 63))) != 0) continue;
                    if (SeqTestMatch2(ki, ApplySeqKey(kw, c2.Car), kw))
                    {
                        dup[i >> 6] |= 1UL << (i & 63);
                        anyDup = true;
                        break;
                    }
                }
            }
        }

        // No duplicates: return the input, as the array path does.
        if (!anyDup) return seq;

        Cons? head = null, tail = null;
        var cur = seq;
        for (int i = 0; i < len && cur is Cons c; i++, cur = c.Cdr)
        {
            if ((dup[i >> 6] & (1UL << (i & 63))) != 0) continue;
            var cell = new Cons(c.Car, Nil.Instance);
            if (tail == null) head = cell; else tail.Cdr = cell;
            tail = cell;
        }
        return head ?? (LispObject)Nil.Instance;
    }

    private static LispObject RemoveDuplicatesScan(LispObject seq, in SeqKwArgs kw)
    {
        seq = AsSequenceArg(seq);
        seq = AsSequenceArg(seq);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("REMOVE-DUPLICATES: not a sequence", seq));
        if (seq is Nil)
        {
            CheckBoundingIndices(kw.Start, kw.End ?? 0, 0, "REMOVE-DUPLICATES");
            return Nil.Instance;
        }

        // A short list is scanned in place; a long one still uses an array.
        if (seq is Cons)
        {
            int listLen = SeqLengthOf(seq, "REMOVE-DUPLICATES");
            int listStart = kw.Start;
            int listEnd = kw.End ?? listLen;
            CheckBoundingIndices(listStart, listEnd, listLen, "REMOVE-DUPLICATES");
            if (listEnd - listStart <= ListScanLimit)
                return RemoveDuplicatesListSmall(seq, listStart, listEnd, listLen, kw);
        }
        LispObject[]? elems = null;
        int len;
        if (seq is LispVector v0) len = v0.Length;
        else if (seq is LispString s0) len = s0.Length;
        else { elems = ListToArray(seq); len = elems.Length; }

        int start = kw.Start;
        int end = kw.End ?? len;
        CheckBoundingIndices(start, end, len, "REMOVE-DUPLICATES");

        LispObject At(int i) => elems != null ? elems[i] : SeqElemAt(seq, i);

        // Duplicate positions go in a bitmap -- on the stack for sequences up to
        // 512 elements.
        int words = (len + 63) >> 6;
        Span<ulong> dup = words <= 8 ? stackalloc ulong[8] : new ulong[words];
        dup = dup.Slice(0, words);
        dup.Clear();
        int removed = 0;

        // With EQL, EQUAL or EQUALP as the test the elements are looked up in a hash table, as
        // SBCL does, instead of compared pairwise: linear instead of quadratic. The result is
        // the same: from-end keeps the first of equal elements, otherwise the last.
        var hashTest = end - start > 16 ? HashableSeqTest(kw) : null;
        if (hashTest != null)
        {
            var seen = new LispHashTable(hashTest);
            if (kw.FromEnd)
            {
                for (int i = start; i < end; i++)
                {
                    var ki = ApplySeqKey(kw, At(i));
                    if (seen.Get(ki, Nil.Instance) is not Nil) { dup[i >> 6] |= 1UL << (i & 63); removed++; }
                    else seen.Set(ki, T.Instance);
                }
            }
            else
            {
                for (int i = end - 1; i >= start; i--)
                {
                    var ki = ApplySeqKey(kw, At(i));
                    if (seen.Get(ki, Nil.Instance) is not Nil) { dup[i >> 6] |= 1UL << (i & 63); removed++; }
                    else seen.Set(ki, T.Instance);
                }
            }
        }
        else
        for (int i = start; i < end; i++)
        {
            if ((dup[i >> 6] & (1UL << (i & 63))) != 0) continue;
            var ki = ApplySeqKey(kw, At(i));
            if (kw.FromEnd)
            {
                // from-end=t: keep the first occurrence, mark the later ones
                for (int j = i + 1; j < end; j++)
                {
                    if ((dup[j >> 6] & (1UL << (j & 63))) != 0) continue;
                    // The test is called with the later element first, as SBCL does.
                    if (SeqTestMatch2(ApplySeqKey(kw, At(j)), ki, kw))
                    { dup[j >> 6] |= 1UL << (j & 63); removed++; }
                }
            }
            else
            {
                // default: keep the last occurrence, mark the earlier ones
                for (int j = i + 1; j < end; j++)
                {
                    if ((dup[j >> 6] & (1UL << (j & 63))) != 0) continue;
                    if (SeqTestMatch2(ki, ApplySeqKey(kw, At(j)), kw))
                    { dup[i >> 6] |= 1UL << (i & 63); removed++; break; }
                }
            }
        }

        if (seq is LispString ostr)
        {
            Span<char> chars = len - removed <= 512 ? stackalloc char[len - removed] : new char[len - removed];
            int k = 0;
            for (int i = 0; i < len; i++)
                if ((dup[i >> 6] & (1UL << (i & 63))) == 0) chars[k++] = ostr[i];
            return new LispString(chars.ToString());
        }
        if (seq is LispVector ovec)
        {
            var items = new LispObject[len - removed];
            int k = 0;
            for (int i = 0; i < len; i++)
                if ((dup[i >> 6] & (1UL << (i & 63))) == 0) items[k++] = ovec[i];
            return new LispVector(items, ovec.ElementTypeName);
        }
        {
            // Cons the survivors straight out of the array.
            Cons? head = null, tail = null;
            for (int i = 0; i < len; i++)
            {
                if ((dup[i >> 6] & (1UL << (i & 63))) != 0) continue;
                var cell = new Cons(elems![i], Nil.Instance);
                if (tail == null) head = cell; else tail.Cdr = cell;
                tail = cell;
            }
            return head ?? (LispObject)Nil.Instance;
        }
    }

    // Test match for remove-duplicates (two elements, not item+elem)
    private static bool SeqTestMatch2(LispObject a, LispObject b, SeqKwArgs kw)
    {
        if (kw.TestNot != null)
            return !IsTruthy(kw.TestNot.Invoke2(a, b));
        if (kw.Test != null)
            return IsTruthy(kw.Test.Invoke2(a, b));
        return IsTrueEql(a, b);
    }

    // EVERY: (every predicate &rest sequences)
    public static LispObject Every(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("EVERY: too few arguments"));
        if (args.Length == 2) return Every2(args[0], args[1]);
        // Multiple sequences: parallel iteration
        return MultipleValues.Primary(EveryMulti(CoerceToFunctionFor(args[0], "EVERY"), args));
    }

    // 2-arg direct entry: single-sequence fast path. The answer is one value: the
    // walk returns right after a predicate call, whose values are still the
    // current ones (SOME, EVERY, NOTANY and NOTEVERY all passed them on).
    public static LispObject Every2(LispObject pred, LispObject seq) =>
        MultipleValues.Primary(Every2Walk(pred, seq));

    private static LispObject Every2Walk(LispObject pred, LispObject seq)
    {
        seq = AsSequenceArg(seq);
        var predFn = CoerceToFunctionFor(pred, "EVERY");
        if (seq is Cons || seq is Nil)
        {
            var cur = seq;
            while (cur is Cons c) { if (!IsTruthy(predFn.Invoke1(c.Car))) return Nil.Instance; cur = c.Cdr; }
            if (cur is not Nil) throw new LispErrorException(new LispTypeError("EVERY: not a proper list", cur));
            return T.Instance;
        }
        if (seq is LispVector vec)
        {
            for (int i = 0; i < vec.Length; i++)
                if (!IsTruthy(predFn.Invoke1(vec[i]))) return Nil.Instance;
            return T.Instance;
        }
        if (seq is LispString str)
        {
            for (int i = 0; i < str.Length; i++)
                if (!IsTruthy(predFn.Invoke1(LispChar.Make(str[i])))) return Nil.Instance;
            return T.Instance;
        }
        throw new LispErrorException(new LispTypeError("EVERY: not a sequence", seq));
    }

    private static LispObject EveryMulti(LispFunction predFn, LispObject[] args)
    {
        int nseqs = args.Length - 1;
        if (nseqs == 2 && IsPlainSequence(args[1]) && IsPlainSequence(args[2]))
            return Every2Seq(predFn, args[1], args[2]);
        // Validate and collect sequence info
        var seqs = new LispObject[nseqs];
        var cursors = new LispObject?[nseqs]; // for lists
        var indices = new int[nseqs];
        var lengths = new int[nseqs];
        var isList = new bool[nseqs];
        for (int s = 0; s < nseqs; s++)
        {
            seqs[s] = AsSequenceArg(args[s + 1]);
            var seq = seqs[s];
            if (seq is Cons || seq is Nil) { isList[s] = true; cursors[s] = seq; lengths[s] = int.MaxValue; }
            else if (seq is LispVector v) { lengths[s] = v.Length; }
            else if (seq is LispString str) { lengths[s] = str.Length; }
            else throw new LispErrorException(new LispTypeError("EVERY: not a sequence", seq));
        }

        while (true)
        {
            var callArgs = new LispObject[nseqs];
            for (int s = 0; s < nseqs; s++)
            {
                if (isList[s])
                {
                    if (cursors[s] is Cons c) { callArgs[s] = c.Car; cursors[s] = c.Cdr; }
                    else if (cursors[s] is Nil) return T.Instance;
                    else throw new LispErrorException(new LispTypeError("EVERY: not a proper list", cursors[s]!));
                }
                else
                {
                    if (indices[s] >= lengths[s]) return T.Instance;
                    callArgs[s] = seqs[s] is LispVector v ? v[indices[s]] : LispChar.Make(((LispString)seqs[s])[indices[s]]);
                    indices[s]++;
                }
            }
            if (!IsTruthy(predFn.Invoke(callArgs))) return Nil.Instance;
        }
    }

    private static bool IsPlainSequence(LispObject seq) =>
        seq is Nil || seq is Cons || seq is LispVector || seq is LispString;

    // Two plain sequences: the general parallel walk, but through Invoke2
    // instead of a rebuilt argument array per element.
    private static LispObject Every2Seq(LispFunction predFn, LispObject a, LispObject b)
    {
        a = AsSequenceArg(a);
        b = AsSequenceArg(b);
        var ca = new ForwardSeqCursor(a, 0);
        var cb = new ForwardSeqCursor(b, 0);
        while (!ca.AtEnd && !cb.AtEnd)
        {
            if (ca.BadTail) throw new LispErrorException(new LispTypeError("EVERY: not a proper list", ca.BadTailValue));
            if (cb.BadTail) throw new LispErrorException(new LispTypeError("EVERY: not a proper list", cb.BadTailValue));
            var va = ca.Next();
            var vb = cb.Next();
            if (!IsTruthy(predFn.Invoke2(va, vb))) return Nil.Instance;
        }
        if (ca.BadTail) throw new LispErrorException(new LispTypeError("EVERY: not a proper list", ca.BadTailValue));
        return T.Instance;
    }

    private static LispObject Some2Seq(LispFunction predFn, LispObject a, LispObject b)
    {
        a = AsSequenceArg(a);
        b = AsSequenceArg(b);
        var ca = new ForwardSeqCursor(a, 0);
        var cb = new ForwardSeqCursor(b, 0);
        while (!ca.AtEnd && !cb.AtEnd)
        {
            if (ca.BadTail) throw new LispErrorException(new LispTypeError("SOME: not a proper list", ca.BadTailValue));
            if (cb.BadTail) throw new LispErrorException(new LispTypeError("SOME: not a proper list", cb.BadTailValue));
            var va = ca.Next();
            var vb = cb.Next();
            var result = predFn.Invoke2(va, vb);
            if (IsTruthy(result)) return result;
        }
        if (ca.BadTail) throw new LispErrorException(new LispTypeError("SOME: not a proper list", ca.BadTailValue));
        return Nil.Instance;
    }

    public static LispObject Every3(LispObject pred, LispObject a, LispObject b)
    {
        var fn = CoerceToFunctionFor(pred, "EVERY");
        // A non-sequence argument is a TYPE-ERROR even when the other sequence is empty.
        if (!IsPlainSequence(a) || !IsPlainSequence(b))
            return MultipleValues.Primary(EveryMulti(fn, new[] { pred, a, b }));
        return MultipleValues.Primary(Every2Seq(fn, a, b));
    }

    public static LispObject Some3(LispObject pred, LispObject a, LispObject b)
    {
        var fn = CoerceToFunctionFor(pred, "SOME");
        if (!IsPlainSequence(a) || !IsPlainSequence(b))
            return MultipleValues.Primary(SomeMulti(fn, new[] { pred, a, b }));
        return MultipleValues.Primary(Some2Seq(fn, a, b));
    }

    // SOME: (some predicate &rest sequences)
    public static LispObject Some(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("SOME: too few arguments"));
        if (args.Length == 2) return Some2(args[0], args[1]);
        // Multiple sequences
        return MultipleValues.Primary(SomeMulti(CoerceToFunctionFor(args[0], "SOME"), args));
    }

    // 2-arg direct entry: single-sequence fast path. One value, as Every2.
    public static LispObject Some2(LispObject pred, LispObject seq) =>
        MultipleValues.Primary(Some2Walk(pred, seq));

    // NOTANY / NOTEVERY as direct entries: negate the single-sequence walk
    // without building the argument array the variadic entries need.
    public static LispObject NotAny2(LispObject pred, LispObject seq) =>
        MultipleValues.Primary(IsTruthy(Some2Walk(pred, seq)) ? Nil.Instance : T.Instance);

    public static LispObject NotEvery2(LispObject pred, LispObject seq) =>
        MultipleValues.Primary(IsTruthy(Every2Walk(pred, seq)) ? Nil.Instance : T.Instance);

    private static LispObject Some2Walk(LispObject pred, LispObject seq)
    {
        seq = AsSequenceArg(seq);
        var predFn = CoerceToFunctionFor(pred, "SOME");
        if (seq is Cons || seq is Nil)
        {
            var cur = seq;
            while (cur is Cons c) { var result = predFn.Invoke1(c.Car); if (IsTruthy(result)) return result; cur = c.Cdr; }
            if (cur is not Nil) throw new LispErrorException(new LispTypeError("SOME: not a proper list", cur));
            return Nil.Instance;
        }
        if (seq is LispVector vec)
        {
            for (int i = 0; i < vec.Length; i++)
            {
                var result = predFn.Invoke1(vec[i]);
                if (IsTruthy(result)) return result;
            }
            return Nil.Instance;
        }
        if (seq is LispString str)
        {
            for (int i = 0; i < str.Length; i++)
            {
                var result = predFn.Invoke1(LispChar.Make(str[i]));
                if (IsTruthy(result)) return result;
            }
            return Nil.Instance;
        }
        throw new LispErrorException(new LispTypeError("SOME: not a sequence", seq));
    }

    private static LispObject SomeMulti(LispFunction predFn, LispObject[] args)
    {
        int nseqs = args.Length - 1;
        if (nseqs == 2 && IsPlainSequence(args[1]) && IsPlainSequence(args[2]))
            return Some2Seq(predFn, args[1], args[2]);
        var seqs = new LispObject[nseqs];
        var cursors = new LispObject?[nseqs];
        var indices = new int[nseqs];
        var lengths = new int[nseqs];
        var isList = new bool[nseqs];
        for (int s = 0; s < nseqs; s++)
        {
            seqs[s] = AsSequenceArg(args[s + 1]);
            var seq = seqs[s];
            if (seq is Cons || seq is Nil) { isList[s] = true; cursors[s] = seq; lengths[s] = int.MaxValue; }
            else if (seq is LispVector v) { lengths[s] = v.Length; }
            else if (seq is LispString str) { lengths[s] = str.Length; }
            else throw new LispErrorException(new LispTypeError("SOME: not a sequence", seq));
        }

        while (true)
        {
            var callArgs = new LispObject[nseqs];
            for (int s = 0; s < nseqs; s++)
            {
                if (isList[s])
                {
                    if (cursors[s] is Cons c) { callArgs[s] = c.Car; cursors[s] = c.Cdr; }
                    else if (cursors[s] is Nil) return Nil.Instance;
                    else throw new LispErrorException(new LispTypeError("SOME: not a proper list", cursors[s]!));
                }
                else
                {
                    if (indices[s] >= lengths[s]) return Nil.Instance;
                    callArgs[s] = seqs[s] is LispVector v ? v[indices[s]] : LispChar.Make(((LispString)seqs[s])[indices[s]]);
                    indices[s]++;
                }
            }
            var result = predFn.Invoke(callArgs);
            if (IsTruthy(result)) return result;
        }
    }

    // MISMATCH: (mismatch seq1 seq2 &key test test-not key start1 end1 start2 end2 from-end)
    private struct MismatchKw
    {
        public LispFunction? TestFn, TestNotFn, KeyFn;
        public int S1, S2;
        public int? E1, E2;
        public bool FromEnd;
        public bool TestSet, TestNotSet, KeySet, S1Set, E1Set, S2Set, E2Set, FeSet, Unknown;
    }

    private static void ApplyMismatchKw(ref MismatchKw m, LispObject k, LispObject v)
    {
        if (k is not Symbol s)
            throw new LispErrorException(new LispProgramError($"MISMATCH: keyword must be a symbol, got {k}"));
        switch (s.Name)
        {
            case "TEST": if (!m.TestSet) { m.TestFn = CoerceToFunctionFor(v, "MISMATCH"); m.TestSet = true; } if (m.TestFn != null && m.TestNotFn != null) throw TestAndTestNotError("MISMATCH"); break;
            case "TEST-NOT": if (!m.TestNotSet) { m.TestNotFn = CoerceToFunctionFor(v, "MISMATCH"); m.TestNotSet = true; } if (m.TestFn != null && m.TestNotFn != null) throw TestAndTestNotError("MISMATCH"); break;
            case "KEY": if (!m.KeySet) { if (v is not Nil) m.KeyFn = CoerceToFunctionFor(v, "MISMATCH"); m.KeySet = true; } break;
            case "START1": if (!m.S1Set) { m.S1 = Runtime.ClampIndex(StartIndexArg(v)); m.S1Set = true; } break;
            case "END1": if (!m.E1Set) { m.E1 = SeqEndArg(v, "MISMATCH"); m.E1Set = true; } break;
            case "START2": if (!m.S2Set) { m.S2 = Runtime.ClampIndex(StartIndexArg(v)); m.S2Set = true; } break;
            case "END2": if (!m.E2Set) { m.E2 = SeqEndArg(v, "MISMATCH"); m.E2Set = true; } break;
            case "FROM-END": if (!m.FeSet) { m.FromEnd = IsTruthy(v); m.FeSet = true; } break;
            case "ALLOW-OTHER-KEYS": break;
            default: m.Unknown = true; break;
        }
    }

    private static LispObject MismatchWith(LispObject seq1, LispObject seq2, in MismatchKw m, bool? allowOtherKeys)
    {
        if (m.Unknown && allowOtherKeys != true)
            throw new LispErrorException(new LispProgramError("MISMATCH: unknown keyword argument"));
        return MismatchCore(seq1, seq2, m.TestFn, m.TestNotFn, m.KeyFn, m.S1, m.E1, m.S2, m.E2, m.FromEnd);
    }

    public static LispObject MismatchFull(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("MISMATCH: too few arguments"));
        if ((args.Length - 2) % 2 != 0)
            throw new LispErrorException(new LispProgramError("MISMATCH: odd number of keyword arguments"));
        bool? allowOtherKeys = null;
        for (int i = 2; i < args.Length - 1 && allowOtherKeys == null; i += 2)
            allowOtherKeys = AllowOtherKeysValue(args[i], args[i + 1]);
        var m = default(MismatchKw);
        for (int i = 2; i < args.Length - 1; i += 2)
            ApplyMismatchKw(ref m, args[i], args[i + 1]);
        return MismatchWith(args[0], args[1], m, allowOtherKeys);
    }

    public static LispObject Mismatch4(LispObject seq1, LispObject seq2, LispObject k1, LispObject v1)
    {
        var m = default(MismatchKw);
        ApplyMismatchKw(ref m, k1, v1);
        return MismatchWith(seq1, seq2, m, AllowOtherKeysValue(k1, v1));
    }

    public static LispObject Mismatch6(LispObject seq1, LispObject seq2,
                                       LispObject k1, LispObject v1, LispObject k2, LispObject v2)
    {
        var m = default(MismatchKw);
        ApplyMismatchKw(ref m, k1, v1);
        ApplyMismatchKw(ref m, k2, v2);
        return MismatchWith(seq1, seq2, m, AllowOtherKeysValue(k1, v1) ?? AllowOtherKeysValue(k2, v2));
    }

    /// <summary>(MISMATCH a b) with no keywords: straight to the core. Wrapping the
    /// two arguments in an array so the keyword parser could look at them was an
    /// allocation per call, the same one SEARCH's two-argument entry removed.</summary>
    public static LispObject Mismatch2(LispObject seq1, LispObject seq2)
        => MismatchCore(seq1, seq2, null, null, null, 0, null, 0, null, false);

    private static LispObject MismatchCore(LispObject seq1, LispObject seq2,
                                           LispFunction? testFn, LispFunction? testNotFn,
                                           LispFunction? keyFn,
                                           int s1, int? e1opt, int s2, int? e2opt, bool fromEnd)
    {
        var r = MismatchScan(seq1, seq2, testFn, testNotFn, keyFn, s1, e1opt, s2, e2opt, fromEnd);
        return testFn != null || testNotFn != null || keyFn != null ? MultipleValues.Alone(r) : r;
    }

    private static LispObject MismatchScan(LispObject seq1, LispObject seq2,
                                           LispFunction? testFn, LispFunction? testNotFn,
                                           LispFunction? keyFn,
                                           int s1, int? e1opt, int s2, int? e2opt, bool fromEnd)
    {
        seq1 = AsSequenceArg(seq1);
        seq2 = AsSequenceArg(seq2);
        int len1 = SeqLengthChecked(seq1, "MISMATCH");
        int len2 = SeqLengthChecked(seq2, "MISMATCH");
        int e1 = e1opt ?? len1;
        int e2 = e2opt ?? len2;
        // MISMATCH was the one bounding-index taker that did not check: SEARCH,
        // SUBSTITUTE, REPLACE and FILL all call this. Without it a reversed or
        // out-of-range range produced an answer instead of an error, and it could
        // be a negative index -- (mismatch "hi" "hello" :start2 4 :end2 2) answered
        // -2. Both sequences are checked, SEQ1 first, so the message names the
        // range the caller wrote first.
        CheckBoundingIndices(s1, e1, len1, "MISMATCH");
        CheckBoundingIndices(s2, e2, len2, "MISMATCH");
        int count1 = e1 - s1;
        int count2 = e2 - s2;

        if (fromEnd)
        {
            // A vector or a string reads at an index from the right; only a list
            // needs a copy to be walked backwards.
            LispObject[]? elems1 = seq1 is LispVector or LispString ? null : CollectSeqElements(seq1, "MISMATCH");
            LispObject[]? elems2 = seq2 is LispVector or LispString ? null : CollectSeqElements(seq2, "MISMATCH");
            for (int i = 1; i <= count1 && i <= count2; i++)
            {
                var x1 = elems1 != null ? elems1[e1 - i]
                    : seq1 is LispVector v1 ? v1[e1 - i]
                    : LispChar.Make(((LispString)seq1)[e1 - i]);
                var x2 = elems2 != null ? elems2[e2 - i]
                    : seq2 is LispVector v2 ? v2[e2 - i]
                    : LispChar.Make(((LispString)seq2)[e2 - i]);
                var k1 = ApplyKeyFn(keyFn, x1);
                var k2 = ApplyKeyFn(keyFn, x2);
                bool match = testNotFn != null ? !IsTruthy(testNotFn.Invoke2(k1, k2))
                           : testFn != null ? IsTruthy(testFn.Invoke2(k1, k2))
                           : IsTrueEql(k1, k2);
                if (!match) return Fixnum.Make(1 + (e1 - i));
            }
            int mc = Math.Min(count1, count2);
            return count1 == count2 ? (LispObject)Nil.Instance : Fixnum.Make(e1 - mc);
        }
        else
        {
            // Forward: each element is read once and in order, so a cursor is
            // enough. Copying both sequences into LispObject[] first -- for a
            // walk that usually stops at the first difference -- was most of what
            // MISMATCH cost.
            // Two strings, or two vectors, with the plain EQL test: compare in place.
            if (testFn == null && testNotFn == null && keyFn == null)
            {
                int n = Math.Min(count1, count2);
                if (seq1 is LispString a1 && seq2 is LispString a2)
                {
                    for (int i = 0; i < n; i++)
                        if (a1[s1 + i] != a2[s2 + i]) return Fixnum.Make(s1 + i);
                    return count1 == count2 ? (LispObject)Nil.Instance : Fixnum.Make(s1 + n);
                }
                if (seq1 is LispVector v1 && seq2 is LispVector v2)
                {
                    for (int i = 0; i < n; i++)
                    {
                        var x1 = v1[s1 + i];
                        var x2 = v2[s2 + i];
                        if (ReferenceEquals(x1, x2)) continue;
                        if (x1 is Fixnum f1 && x2 is Fixnum f2) { if (f1.Value == f2.Value) continue; return Fixnum.Make(s1 + i); }
                        if (!IsTrueEql(x1, x2)) return Fixnum.Make(s1 + i);
                    }
                    return count1 == count2 ? (LispObject)Nil.Instance : Fixnum.Make(s1 + n);
                }
            }
            var cur1 = new ForwardSeqCursor(seq1, s1);
            var cur2 = new ForwardSeqCursor(seq2, s2);
            for (int i = 0; i < count1 && i < count2; i++)
            {
                var x1 = cur1.Next();
                var x2 = cur2.Next();
                var k1 = ApplyKeyFn(keyFn, x1);
                var k2 = ApplyKeyFn(keyFn, x2);
                bool match = testNotFn != null ? !IsTruthy(testNotFn.Invoke2(k1, k2))
                           : testFn != null ? IsTruthy(testFn.Invoke2(k1, k2))
                           : IsTrueEql(k1, k2);
                if (!match) return Fixnum.Make(s1 + i);
            }
            return count1 == count2 ? (LispObject)Nil.Instance : Fixnum.Make(s1 + Math.Min(count1, count2));
        }
    }

    /// <summary>The sequence's length, with the same type check COLLECTSEQELEMENTS
    /// makes, but without copying it.</summary>
    private static int SeqLengthChecked(LispObject seq, string fnName)
    {
        if (seq is LispVector v) return v.Length;
        if (seq is LispString s) return s.Length;
        if (seq is Nil) return 0;
        if (seq is Cons) return CheckedListLength(seq, fnName);
        throw new LispErrorException(new LispTypeError($"{fnName}: not a sequence", seq));
    }

    /// <summary>Reads one sequence left to right without copying it: an index for a
    /// string or vector, a cons pointer for a list. NEXT is only called as many
    /// times as the caller's bounds allow, so it does not re-check them.</summary>
    private struct ForwardSeqCursor
    {
        private readonly LispVector? _vec;
        private readonly LispString? _str;
        private LispObject? _cell;
        private int _idx;

        public ForwardSeqCursor(LispObject seq, int start)
        {
            _vec = seq as LispVector;
            _str = seq as LispString;
            _idx = start;
            _cell = null;
            if (_vec == null && _str == null)
            {
                var c = seq;
                for (int i = 0; i < start && c is Cons cc; i++) c = cc.Cdr;
                _cell = c;
            }
        }

        public LispObject Next()
        {
            if (_vec != null) return _vec[_idx++];
            if (_str != null) return LispChar.Make(_str[_idx++]);
            var cell = (Cons)_cell!;
            _cell = cell.Cdr;
            return cell.Car;
        }

        public bool AtEnd => _vec != null ? _idx >= _vec.Length
                           : _str != null ? _idx >= _str.Length
                           : _cell is Nil;

        public bool BadTail => _vec == null && _str == null && _cell is not Nil and not Cons;

        public LispObject BadTailValue => _cell ?? Nil.Instance;
    }

    // Helper: collect all elements from a sequence into an array
    private static LispObject[] CollectSeqElements(LispObject seq, string fnName)
    {
        if (seq is LispVector vec)
        {
            var elems = new LispObject[vec.Length];
            for (int i = 0; i < vec.Length; i++) elems[i] = vec[i];
            return elems;
        }
        if (seq is LispString str)
        {
            var elems = new LispObject[str.Length];
            for (int i = 0; i < str.Length; i++) elems[i] = LispChar.Make(str[i]);
            return elems;
        }
        if (seq is Nil) return Array.Empty<LispObject>();
        if (seq is Cons) return ListToArray(seq);
        throw new LispErrorException(new LispTypeError($"{fnName}: not a sequence", seq));
    }

    // A :START1 / :START2 value: a fixnum, or a type-error naming the value and UNSIGNED-BYTE.
    private static long StartIndexArg(LispObject v)
        => v is Fixnum f ? f.Value
           : throw new LispErrorException(new LispTypeError("the index is not an integer: " + FormatObject(v, true), v, Startup.Sym("UNSIGNED-BYTE")));

    public static LispObject Search(LispObject seq1, LispObject seq2)
        // No keywords: straight to the core. Wrapping the two arguments in an
        // array so the keyword parser could look at them was an allocation per
        // (search a b), which is the shape most call sites have.
        => SearchCore(seq1, seq2, false, null, null, null, 0, null, 0, null);

    // Keyword state for SEARCH, filled one (key value) pair at a time so the
    // direct entries below need no argument array. First-wins on duplicates,
    // which is what the args-array path does.
    private struct SearchKwState
    {
        public bool FromEnd;
        public LispFunction? TestFn, TestNotFn, KeyFn;
        public int Start1, Start2;
        public int? End1, End2;
        public bool AllowOtherKeys;
        public bool FeSet, TestSet, TestNotSet, KeySet, S1Set, S2Set, E1Set, E2Set;
    }

    /// <summary>An :END1 / :END2 value: NIL is the default (null), an integer is the bound, and
    /// anything else (1.5, "1", #\a) is a type-error instead of being ignored.</summary>
    internal static int? SeqEndArg(LispObject v, string fname)
    {
        if (v is Nil) return null;
        if (v is Fixnum f) return Runtime.ClampIndex(f.Value);
        if (v is Bignum b) return b.Value.Sign < 0 ? int.MinValue : int.MaxValue;
        throw new LispErrorException(new LispTypeError($"{fname}: :end must be an integer or NIL", v, Startup.Sym("INTEGER")));
    }

    private static void ApplySearchKw(LispObject k, LispObject v, ref SearchKwState st)
    {
        if (k is not Symbol kw)
            throw new LispErrorException(new LispProgramError(
                $"SEARCH: keyword must be a symbol, got {k}"));
        switch (kw.Name)
        {
            case "FROM-END": if (!st.FeSet) { st.FromEnd = IsTruthy(v); st.FeSet = true; } break;
            case "TEST": if (!st.TestSet) { st.TestFn = CoerceToFunctionFor(v, "SEARCH"); st.TestSet = true; } if (st.TestFn != null && st.TestNotFn != null) throw TestAndTestNotError("SEARCH"); break;
            case "TEST-NOT": if (!st.TestNotSet) { st.TestNotFn = CoerceToFunctionFor(v, "SEARCH"); st.TestNotSet = true; } if (st.TestFn != null && st.TestNotFn != null) throw TestAndTestNotError("SEARCH"); break;
            case "KEY": if (!st.KeySet) { st.KeyFn = v is Nil ? null : CoerceToFunctionFor(v, "SEARCH"); st.KeySet = true; } break;
            case "START1": if (!st.S1Set) { st.Start1 = Runtime.ClampIndex(StartIndexArg(v)); st.S1Set = true; } break;
            case "END1": if (!st.E1Set) { st.End1 = SeqEndArg(v, "SEARCH"); st.E1Set = true; } break;
            case "START2": if (!st.S2Set) { st.Start2 = Runtime.ClampIndex(StartIndexArg(v)); st.S2Set = true; } break;
            case "END2": if (!st.E2Set) { st.End2 = SeqEndArg(v, "SEARCH"); st.E2Set = true; } break;
            case "ALLOW-OTHER-KEYS": break;
            default:
                if (!st.AllowOtherKeys)
                    throw new LispErrorException(new LispProgramError(
                        $"SEARCH: unknown keyword :{kw.Name}"));
                break;
        }
    }

    // True when (K V) is :allow-other-keys, in which case OUT carries its value.
    private static bool IsAllowOtherKeysPair(LispObject? k, LispObject v, out bool value)
    {
        value = false;
        if (k is Symbol s && s.Name == "ALLOW-OTHER-KEYS") { value = IsTruthy(v); return true; }
        return false;
    }

    /// <summary>SEARCH with up to three keyword pairs, taken as separate arguments.
    /// SEARCH was the one sequence builtin with no direct entries at all -- not even
    /// the two-argument one, which already existed as a method -- so every call
    /// built an argument array. cl-ppcre's scanner makes one (search pattern string
    /// :start2 s :end2 e) per SCAN, and paid 88 bytes for the array each time.</summary>
    private static LispObject SearchKeys(LispObject seq1, LispObject seq2,
                                         LispObject? k1, LispObject? v1,
                                         LispObject? k2, LispObject? v2,
                                         LispObject? k3, LispObject? v3)
    {
        var st = default(SearchKwState);
        // First pass, first-wins, exactly as the args-array path does it.
        if (k1 != null && IsAllowOtherKeysPair(k1, v1!, out var a1)) st.AllowOtherKeys = a1;
        else if (k2 != null && IsAllowOtherKeysPair(k2, v2!, out var a2)) st.AllowOtherKeys = a2;
        else if (k3 != null && IsAllowOtherKeysPair(k3, v3!, out var a3)) st.AllowOtherKeys = a3;
        if (k1 != null) ApplySearchKw(k1, v1!, ref st);
        if (k2 != null) ApplySearchKw(k2, v2!, ref st);
        if (k3 != null) ApplySearchKw(k3, v3!, ref st);
        return SearchCore(seq1, seq2, st.FromEnd, st.TestFn, st.TestNotFn, st.KeyFn,
                          st.Start1, st.End1, st.Start2, st.End2);
    }

    public static LispObject Search4(LispObject seq1, LispObject seq2,
                                     LispObject k1, LispObject v1)
        => SearchKeys(seq1, seq2, k1, v1, null, null, null, null);

    public static LispObject Search6(LispObject seq1, LispObject seq2,
                                     LispObject k1, LispObject v1, LispObject k2, LispObject v2)
        => SearchKeys(seq1, seq2, k1, v1, k2, v2, null, null);

    public static LispObject Search8(LispObject seq1, LispObject seq2,
                                     LispObject k1, LispObject v1, LispObject k2, LispObject v2,
                                     LispObject k3, LispObject v3)
        => SearchKeys(seq1, seq2, k1, v1, k2, v2, k3, v3);

    public static LispObject SearchFull(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("SEARCH: wrong number of arguments"));
        var seq1 = args[0];
        var seq2 = args[1];
        bool fromEnd = false;
        LispFunction? testFn = null, testNotFn = null, keyFn = null;
        int start1 = 0, start2 = 0;
        int? end1 = null, end2 = null;

        int kwCount = args.Length - 2;
        if (kwCount % 2 != 0)
            throw new LispErrorException(new LispProgramError("SEARCH: odd number of keyword arguments"));
        // First pass: check :allow-other-keys (first-wins)
        bool allowOtherKeys = false;
        for (int i = 2; i + 1 < args.Length; i += 2)
        {
            if (args[i] is not Symbol kw2)
                throw new LispErrorException(new LispProgramError($"SEARCH: keyword must be a symbol, got {args[i]}"));
            if (kw2.Name == "ALLOW-OTHER-KEYS") { allowOtherKeys = IsTruthy(args[i + 1]); break; }
        }
        // First-wins: use bool flags to implement first-wins for duplicate keys
        bool feSet = false, testSet = false, testNotSet = false, keySet = false;
        bool s1Set = false, s2Set = false, e1Set = false, e2Set = false;
        for (int i = 2; i + 1 < args.Length; i += 2)
        {
            if (args[i] is not Symbol kw)
                throw new LispErrorException(new LispProgramError($"SEARCH: keyword must be a symbol, got {args[i]}"));
            switch (kw.Name)
            {
                case "FROM-END": if (!feSet) { fromEnd = IsTruthy(args[i + 1]); feSet = true; } break;
                case "TEST": if (!testSet) { testFn = CoerceToFunctionFor(args[i + 1], "SEARCH"); testSet = true; } if (testFn != null && testNotFn != null) throw TestAndTestNotError("SEARCH"); break;
                case "TEST-NOT": if (!testNotSet) { testNotFn = CoerceToFunctionFor(args[i + 1], "SEARCH"); testNotSet = true; } if (testFn != null && testNotFn != null) throw TestAndTestNotError("SEARCH"); break;
                case "KEY": if (!keySet) { keyFn = args[i + 1] is Nil ? null : CoerceToFunctionFor(args[i + 1], "SEARCH"); keySet = true; } break;
                case "START1": if (!s1Set) { start1 = Runtime.ClampIndex(StartIndexArg(args[i + 1])); s1Set = true; } break;
                case "END1": if (!e1Set) { end1 = SeqEndArg(args[i + 1], "SEARCH"); e1Set = true; } break;
                case "START2": if (!s2Set) { start2 = Runtime.ClampIndex(StartIndexArg(args[i + 1])); s2Set = true; } break;
                case "END2": if (!e2Set) { end2 = SeqEndArg(args[i + 1], "SEARCH"); e2Set = true; } break;
                case "ALLOW-OTHER-KEYS": break;
                default:
                    if (!allowOtherKeys)
                        throw new LispErrorException(new LispProgramError($"SEARCH: unknown keyword :{kw.Name}"));
                    break;
            }
        }

        return SearchCore(seq1, seq2, fromEnd, testFn, testNotFn, keyFn, start1, end1, start2, end2);
    }

    // The element test as a plain call rather than a delegate: building the
    // :test / :test-not closures forced a display class on every SEARCH, keyword
    // or not.
    private static bool SearchElemTest(LispFunction? testFn, LispFunction? testNotFn,
                                       LispObject a, LispObject b)
        => testFn != null ? IsTruthy(testFn.Invoke2(a, b))
         : testNotFn != null ? !IsTruthy(testNotFn.Invoke2(a, b))
         : IsTrueEql(a, b);

    private static LispObject SearchCore(LispObject seq1, LispObject seq2, bool fromEnd,
                                         LispFunction? testFn, LispFunction? testNotFn, LispFunction? keyFn,
                                         int start1, int? end1, int start2, int? end2)
    {
        var r = SearchScan(seq1, seq2, fromEnd, testFn, testNotFn, keyFn, start1, end1, start2, end2);
        return testFn != null || testNotFn != null || keyFn != null ? MultipleValues.Alone(r) : r;
    }

    private static LispObject SearchScan(LispObject seq1, LispObject seq2, bool fromEnd,
                                         LispFunction? testFn, LispFunction? testNotFn, LispFunction? keyFn,
                                         int start1, int? end1, int start2, int? end2)
    {
        seq1 = AsSequenceArg(seq1);
        seq2 = AsSequenceArg(seq2);
        int len1 = ReplaceSeqLength(seq1, "SEARCH"), len2 = ReplaceSeqLength(seq2, "SEARCH");
        int e1 = end1 ?? len1, e2 = end2 ?? len2;
        CheckBoundingIndices(start1, e1, len1, "SEARCH");
        CheckBoundingIndices(start2, e2, len2, "SEARCH");
        int patLen = e1 - start1;
        int searchLen = e2 - start2;

        if (patLen == 0)
            return fromEnd ? Fixnum.Make(e2) : Fixnum.Make(start2);
        if (patLen > searchLen) return Nil.Instance;

        int limit = start2 + searchLen - patLen;

        // Fast path: string-to-string search with default EQL test
        if (seq1 is LispString searchStr1 && seq2 is LispString searchStr2 && keyFn == null && testFn == null && testNotFn == null)
        {
            var chars1 = searchStr1.Chars;
            var chars2 = searchStr2.Chars;
            if (fromEnd)
            {
                for (int i = limit; i >= start2; i--)
                {
                    bool match = true;
                    for (int j = 0; j < patLen; j++)
                    {
                        if (chars1[start1 + j] != chars2[i + j]) { match = false; break; }
                    }
                    if (match) return Fixnum.Make(i);
                }
            }
            else
            {
                for (int i = start2; i <= limit; i++)
                {
                    bool match = true;
                    for (int j = 0; j < patLen; j++)
                    {
                        if (chars1[start1 + j] != chars2[i + j]) { match = false; break; }
                    }
                    if (match) return Fixnum.Make(i);
                }
            }
            return Nil.Instance;
        }

        // A list on either side: scan forwards with cursors, not by index from the head of the
        // list for every element. FROM-END keeps the last match of the one forward scan.
        if (seq1 is Cons || seq2 is Cons)
        {
            LispObject? t1 = seq1 is Cons ? ListTail(seq1, start1) : null;
            LispObject? c2 = seq2 is Cons ? ListTail(seq2, start2) : null;
            int last = -1;
            for (int i = start2; i <= limit; i++)
            {
                LispObject? p1 = t1, p2 = c2;
                bool match = true;
                for (int j = 0; j < patLen && match; j++)
                {
                    LispObject x1, x2;
                    if (p1 is Cons q1) { x1 = q1.Car; p1 = q1.Cdr; } else x1 = ReplaceSeqGet(seq1, start1 + j);
                    if (p2 is Cons q2) { x2 = q2.Car; p2 = q2.Cdr; } else x2 = ReplaceSeqGet(seq2, i + j);
                    var a = ApplyKeyFn(keyFn, x1);
                    var b = ApplyKeyFn(keyFn, x2);
                    if (!SearchElemTest(testFn, testNotFn, a, b)) match = false;
                }
                if (match) { if (!fromEnd) return Fixnum.Make(i); last = i; }
                if (c2 is Cons cc2) c2 = cc2.Cdr;
            }
            return last >= 0 ? Fixnum.Make(last) : Nil.Instance;
        }

        if (fromEnd)
        {
            for (int i = limit; i >= start2; i--)
            {
                bool match = true;
                for (int j = 0; j < patLen && match; j++)
                {
                    var a = ApplyKeyFn(keyFn, ReplaceSeqGet(seq1, start1 + j));
                    var b = ApplyKeyFn(keyFn, ReplaceSeqGet(seq2, i + j));
                    if (!SearchElemTest(testFn, testNotFn, a, b)) match = false;
                }
                if (match) return Fixnum.Make(i);
            }
        }
        else
        {
            for (int i = start2; i <= limit; i++)
            {
                bool match = true;
                for (int j = 0; j < patLen && match; j++)
                {
                    var a = ApplyKeyFn(keyFn, ReplaceSeqGet(seq1, start1 + j));
                    var b = ApplyKeyFn(keyFn, ReplaceSeqGet(seq2, i + j));
                    if (!SearchElemTest(testFn, testNotFn, a, b)) match = false;
                }
                if (match) return Fixnum.Make(i);
            }
        }
        return Nil.Instance;
    }

    public static LispObject String(LispObject obj)
    {
        if (obj is LispString) return obj;
        if (obj is LispVector v && v.IsCharVector && v.Rank == 1) return obj; // rank-1 char-vector is a string
        // CLHS: STRING of a symbol is its name: the same string SYMBOL-NAME
        // answers with, so share it (SBCL does too).
        if (obj is Symbol sym) return sym.NameString;
        if (obj is Nil || obj is T) return SymbolName(obj);
        if (obj is LispChar ch) return new LispString(ch.Value.ToString());
        throw new LispErrorException(new LispTypeError("STRING: not a string designator", obj,
            List(Startup.Sym("OR"), Startup.Sym("STRING"), Startup.Sym("SYMBOL"), Startup.Sym("CHARACTER"))));
    }

    internal static void RegisterSequenceBuiltins()
    {
        // COUNT, COUNT-IF, COUNT-IF-NOT
        // COUNT and COUNT-IF: a two-argument direct entry, so (count x l) reaches
        // the body without building an args array for the variadic entry to walk.
        var countFn = new LispFunction(args => Runtime.Count(args));
        countFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Count2);
        countFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Count4);
        countFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Count6);
        Emitter.CilAssembler.RegisterFunction("COUNT", countFn);
        var countIfFn = new LispFunction(args => Runtime.CountIf(args));
        countIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.CountIf2);
        countIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.CountIf4);
        countIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.CountIf6);
        Emitter.CilAssembler.RegisterFunction("COUNT-IF", countIfFn);
        var countIfNotFn = new LispFunction(args => Runtime.CountIfNotFull(args));
        countIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.CountIfNot2);
        countIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.CountIfNot4);
        countIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.CountIfNot6);
        Emitter.CilAssembler.RegisterFunction("COUNT-IF-NOT", countIfNotFn);
        // FILL
        var fillFn = new LispFunction(args => Runtime.Fill(args));
        fillFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Fill2);
        fillFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Fill4);
        fillFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Fill6);
        Emitter.CilAssembler.RegisterFunction("FILL", fillFn);
        // FIND, FIND-IF, FIND-IF-NOT
        var findFn = new LispFunction(args => Runtime.Find(args));
        findFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Find2);
        findFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Find4);
        findFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Find6);
        findFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Find8);
        Emitter.CilAssembler.RegisterFunction("FIND", findFn);
        var findIfFn = new LispFunction(args => Runtime.FindIf(args));
        findIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.FindIf2);
        findIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.FindIf4);
        findIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.FindIf6);
        findIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.FindIf8);
        Emitter.CilAssembler.RegisterFunction("FIND-IF", findIfFn);
        var findIfNotFn = new LispFunction(args => Runtime.FindIfNotFull(args));
        findIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.FindIfNot2);
        findIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.FindIfNot4);
        findIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.FindIfNot6);
        findIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.FindIfNot8);
        Emitter.CilAssembler.RegisterFunction("FIND-IF-NOT", findIfNotFn);
        // POSITION, POSITION-IF, POSITION-IF-NOT
        var positionFn = new LispFunction(args => Runtime.Position(args));
        positionFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Position2);
        positionFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Position4);
        positionFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Position6);
        positionFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Position8);
        Emitter.CilAssembler.RegisterFunction("POSITION", positionFn);
        var positionIfFn = new LispFunction(args => Runtime.PositionIf(args));
        positionIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.PositionIf2);
        positionIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.PositionIf4);
        positionIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.PositionIf6);
        positionIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.PositionIf8);
        Emitter.CilAssembler.RegisterFunction("POSITION-IF", positionIfFn);
        var positionIfNotFn = new LispFunction(args => Runtime.PositionIfNotFull(args));
        positionIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.PositionIfNot2);
        positionIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.PositionIfNot4);
        positionIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.PositionIfNot6);
        positionIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.PositionIfNot8);
        Emitter.CilAssembler.RegisterFunction("POSITION-IF-NOT", positionIfNotFn);
        // REDUCE
        var reduceFn = new LispFunction(args => Runtime.Reduce(args));
        reduceFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Reduce2);
        reduceFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Reduce4);
        reduceFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Reduce6);
        reduceFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Reduce8);
        Emitter.CilAssembler.RegisterFunction("REDUCE", reduceFn);
        // MEMBER, MEMBER-IF, MEMBER-IF-NOT
        // MEMBER/ASSOC: attach direct entries for the dominant call shapes
        // ((item list) and (item list kw val): e.g. :test #'string=). The
        // 2-arg entry skips the args array entirely; the 4-arg entry runs the
        // exact shared keyword parser over a 2-element array.
        var memberFn = new LispFunction(args => Runtime.MemberFull(args));
        memberFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Member2);
        memberFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Member4);
        memberFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Member6);
        Emitter.CilAssembler.RegisterFunction("MEMBER", memberFn);
        // ADJOIN: same shapes as MEMBER (PUSHNEW expands into (adjoin item place kw val)).
        var adjoinFn = new LispFunction(args => Runtime.AdjoinFull(args));
        adjoinFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Adjoin2);
        adjoinFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Adjoin4);
        adjoinFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Adjoin6);
        Emitter.CilAssembler.RegisterFunction("ADJOIN", adjoinFn);
        var memberIfFn = new LispFunction(args => Runtime.MemberIf(args));
        memberIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.MemberIf2);
        memberIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.MemberIf4);
        Emitter.CilAssembler.RegisterFunction("MEMBER-IF", memberIfFn);
        var memberIfNotFn = new LispFunction(args => Runtime.MemberIfNotFull(args));
        memberIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.MemberIfNot2);
        memberIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.MemberIfNot4);
        Emitter.CilAssembler.RegisterFunction("MEMBER-IF-NOT", memberIfNotFn);
        // ASSOC, ASSOC-IF, ASSOC-IF-NOT
        var assocFn = new LispFunction(args => Runtime.AssocFull(args));
        assocFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Assoc2);
        assocFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Assoc4);
        assocFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Assoc6);
        Emitter.CilAssembler.RegisterFunction("ASSOC", assocFn);
        var assocIfFn = new LispFunction(args => Runtime.AssocIf(args));
        assocIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.AssocIf2);
        assocIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.AssocIf4);
        Emitter.CilAssembler.RegisterFunction("ASSOC-IF", assocIfFn);
        var assocIfNotFn = new LispFunction(args => Runtime.AssocIfNotFull(args));
        assocIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.AssocIfNot2);
        assocIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.AssocIfNot4);
        Emitter.CilAssembler.RegisterFunction("ASSOC-IF-NOT", assocIfNotFn);
        // RASSOC, RASSOC-IF, RASSOC-IF-NOT
        var rassocFn = new LispFunction(args => Runtime.RassocFull(args));
        rassocFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Rassoc2);
        rassocFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Rassoc4);
        rassocFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Rassoc6);
        Emitter.CilAssembler.RegisterFunction("RASSOC", rassocFn);
        var rassocIfFn = new LispFunction(args => Runtime.RassocIf(args));
        rassocIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.RassocIf2);
        rassocIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.RassocIf4);
        Emitter.CilAssembler.RegisterFunction("RASSOC-IF", rassocIfFn);
        var rassocIfNotFn = new LispFunction(args => Runtime.RassocIfNotFull(args));
        rassocIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.RassocIfNot2);
        rassocIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.RassocIfNot4);
        Emitter.CilAssembler.RegisterFunction("RASSOC-IF-NOT", rassocIfNotFn);
        // REMOVE, REMOVE-IF, REMOVE-IF-NOT
        var removeFn = new LispFunction(args => Runtime.RemoveFull(args));
        removeFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Remove2);
        removeFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Remove4);
        removeFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Remove6);
        Emitter.CilAssembler.RegisterFunction("REMOVE", removeFn);
        var removeIfFn = new LispFunction(args => Runtime.RemoveIf(args));
        removeIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.RemoveIf2);
        removeIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.RemoveIf4);
        removeIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.RemoveIf6);
        Emitter.CilAssembler.RegisterFunction("REMOVE-IF", removeIfFn);
        var removeIfNotFn = new LispFunction(args =>
            {
                var predFn = Runtime.CoerceToFunctionFor(args[0], "REMOVE-IF-NOT");
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[0] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.RemoveIf(newArgs);
            }).WithArgCount(2, -1);
        removeIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.RemoveIfNot2);
        removeIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.RemoveIfNot4);
        removeIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.RemoveIfNot6);
        Emitter.CilAssembler.RegisterFunction("REMOVE-IF-NOT", removeIfNotFn);
        // DELETE, DELETE-IF, DELETE-IF-NOT: destructive on list args.
        var deleteFn = new LispFunction(args => Runtime.DeleteFull(args));
        deleteFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Delete2);
        deleteFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Delete4);
        deleteFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Delete6);
        Emitter.CilAssembler.RegisterFunction("DELETE", deleteFn);
        var deleteIfFn = new LispFunction(args => Runtime.DeleteIf(args));
        deleteIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.DeleteIf2);
        deleteIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.DeleteIf4);
        deleteIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.DeleteIf6);
        Emitter.CilAssembler.RegisterFunction("DELETE-IF", deleteIfFn);
        Emitter.CilAssembler.RegisterFunction("DELETE-IF-NOT",
            new LispFunction(args =>
            {
                var predFn = Runtime.CoerceToFunctionFor(args[0], "DELETE-IF-NOT");
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[0] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.DeleteIf(newArgs);
            }).WithArgCount(2, -1));
        // SUBSTITUTE, SUBSTITUTE-IF, SUBSTITUTE-IF-NOT
        var substituteFn = new LispFunction(args => Runtime.SubstituteFull(args));
        substituteFn.SetDirectDelegate(
            (Func<LispObject, LispObject, LispObject, LispObject>)Runtime.Substitute3);
        substituteFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Substitute5);
        substituteFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Substitute7);
        Emitter.CilAssembler.RegisterFunction("SUBSTITUTE", substituteFn);
        var substituteIfFn = new LispFunction(args => Runtime.SubstituteIf(args));
        substituteIfFn.SetDirectDelegate(
            (Func<LispObject, LispObject, LispObject, LispObject>)Runtime.SubstituteIf3);
        substituteIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.SubstituteIf5);
        substituteIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.SubstituteIf7);
        Emitter.CilAssembler.RegisterFunction("SUBSTITUTE-IF", substituteIfFn);
        Emitter.CilAssembler.RegisterFunction("SUBSTITUTE-IF-NOT",
            new LispFunction(args =>
            {
                Runtime.CheckArityMin("SUBSTITUTE-IF-NOT", args, 3);
                var predFn = Runtime.CoerceToFunctionFor(args[1], "SUBSTITUTE-IF-NOT");
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[1] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.SubstituteIf(newArgs);
            }));
        // NSUBSTITUTE, NSUBSTITUTE-IF, NSUBSTITUTE-IF-NOT
        var nsubstituteFn = new LispFunction(args => Runtime.NsubstituteFull(args));
        nsubstituteFn.SetDirectDelegate(
            (Func<LispObject, LispObject, LispObject, LispObject>)Runtime.Nsubstitute3);
        nsubstituteFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Nsubstitute5);
        nsubstituteFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Nsubstitute7);
        Emitter.CilAssembler.RegisterFunction("NSUBSTITUTE", nsubstituteFn);
        var nsubstituteIfFn = new LispFunction(args => Runtime.NsubstituteIf(args));
        nsubstituteIfFn.SetDirectDelegate(
            (Func<LispObject, LispObject, LispObject, LispObject>)Runtime.NsubstituteIf3);
        nsubstituteIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.NsubstituteIf5);
        nsubstituteIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.NsubstituteIf7);
        Emitter.CilAssembler.RegisterFunction("NSUBSTITUTE-IF", nsubstituteIfFn);
        Emitter.CilAssembler.RegisterFunction("NSUBSTITUTE-IF-NOT",
            new LispFunction(args =>
            {
                Runtime.CheckArityMin("NSUBSTITUTE-IF-NOT", args, 3);
                var predFn = Runtime.CoerceToFunctionFor(args[1], "NSUBSTITUTE-IF-NOT");
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[1] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.NsubstituteIf(newArgs);
            }));
        // EVERY, SOME, NOTEVERY, NOTANY
        var everyFn = new LispFunction(args => Runtime.Every(args));
        everyFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Every2);
        everyFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject>)Runtime.Every3);
        Emitter.CilAssembler.RegisterFunction("EVERY", everyFn);
        var someFn = new LispFunction(args => Runtime.Some(args));
        someFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Some2);
        someFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject>)Runtime.Some3);
        Emitter.CilAssembler.RegisterFunction("SOME", someFn);
        var notEveryFn = new LispFunction(args =>
            MultipleValues.Primary(Runtime.IsTruthy(Runtime.Every(args)) ? Nil.Instance : T.Instance));
        notEveryFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.NotEvery2);
        Emitter.CilAssembler.RegisterFunction("NOTEVERY", notEveryFn);
        var notAnyFn = new LispFunction(args =>
            MultipleValues.Primary(Runtime.IsTruthy(Runtime.Some(args)) ? Nil.Instance : T.Instance));
        notAnyFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.NotAny2);
        Emitter.CilAssembler.RegisterFunction("NOTANY", notAnyFn);
        // MISMATCH, REMOVE-DUPLICATES, DELETE-DUPLICATES, REPLACE
        var mismatchFn = new LispFunction(args => Runtime.MismatchFull(args));
        mismatchFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Mismatch2);
        mismatchFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Mismatch4);
        mismatchFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Mismatch6);
        Emitter.CilAssembler.RegisterFunction("MISMATCH", mismatchFn);
        var removeDupFn = new LispFunction(args => Runtime.RemoveDuplicatesFull(args));
        removeDupFn.SetDirectDelegate((Func<LispObject, LispObject>)Runtime.RemoveDuplicates1);
        removeDupFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject>)Runtime.RemoveDuplicates3);
        removeDupFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.RemoveDuplicates5);
        Emitter.CilAssembler.RegisterFunction("REMOVE-DUPLICATES", removeDupFn);
        var deleteDupFn = new LispFunction(args => Runtime.DeleteDuplicatesFull(args));
        deleteDupFn.SetDirectDelegate((Func<LispObject, LispObject>)Runtime.DeleteDuplicates1);
        deleteDupFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject>)Runtime.DeleteDuplicates3);
        deleteDupFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.DeleteDuplicates5);
        Emitter.CilAssembler.RegisterFunction("DELETE-DUPLICATES", deleteDupFn);
        Emitter.CilAssembler.RegisterFunction("REPLACE",
            new LispFunction(args => { Runtime.CheckArityMin("REPLACE", args, 2); return Runtime.Replace(args); }));
        // MAKE-STRING: direct entries for (make-string n) and for
        // (make-string n :initial-element c), so neither builds an args array.
        static LispObject MakeStringVariadic(LispObject[] args)
        {
            // (make-string size &key initial-element element-type)
            if (args.Length == 0)
                throw new LispErrorException(new LispProgramError("MAKE-STRING: wrong number of arguments: 0"));
            var size = args[0];
            // Check for extra positional args (non-keyword after size)
            if (args.Length > 1 && args[1] is not Symbol)
                throw new LispErrorException(new LispProgramError($"MAKE-STRING: too many positional arguments"));
            // Validate keyword args
            int kwCount = args.Length - 1;
            if (kwCount % 2 != 0)
                throw new LispErrorException(new LispProgramError("MAKE-STRING: odd number of keyword arguments"));
            LispObject initChar = Nil.Instance;
            bool initCharSet = false;
            LispObject? elementType = null;
            bool? allowOtherKeys = null;
            bool hasUnknown = false;
            for (int i = 1; i < args.Length; i += 2)
            {
                if (args[i] is not Symbol kw)
                    throw new LispErrorException(new LispProgramError($"MAKE-STRING: not a keyword: {args[i]}"));
                var val = args[i + 1];
                switch (kw.Name)
                {
                    case "INITIAL-ELEMENT": if (!initCharSet) { initChar = val; initCharSet = true; } break;
                    case "ELEMENT-TYPE": elementType ??= val; break;
                    case "ALLOW-OTHER-KEYS": if (allowOtherKeys == null) allowOtherKeys = Runtime.IsTruthy(val); break;
                    default: hasUnknown = true; break;
                }
            }
            if (hasUnknown && allowOtherKeys != true)
                throw new LispErrorException(new LispProgramError("MAKE-STRING: unknown keyword argument"));
            // The element type has to be a subtype of CHARACTER.
            if (elementType != null && Runtime.SubtypepClean(elementType, Startup.Sym("CHARACTER")) is Nil)
                throw new LispErrorException(new LispTypeError("MAKE-STRING: :ELEMENT-TYPE is not a subtype of CHARACTER",
                    elementType, Startup.Sym("CHARACTER")));
            if (elementType != null && Runtime.ParseElementTypeName(elementType) == "BASE-CHAR")
            {
                var s = (LispString)Runtime.MakeString(size, initChar);
                return Runtime.MakeBaseString(s.Value);
            }
            return Runtime.MakeString(size, initChar);
        }
        static LispObject MakeString3(LispObject size, LispObject key, LispObject value)
            => key is Symbol kw && kw.Name == "INITIAL-ELEMENT"
                ? Runtime.MakeString(size, value)
                : MakeStringVariadic(new[] { size, key, value });
        var makeStringFn = new LispFunction(MakeStringVariadic, "MAKE-STRING", -1);
        makeStringFn.SetDirectDelegate((Func<LispObject, LispObject>)(size => Runtime.MakeString(size, Nil.Instance)));
        makeStringFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject>)MakeString3);
        Emitter.CilAssembler.RegisterFunction("MAKE-STRING", makeStringFn);

        // String comparison functions. Each gets a 2-arg direct delegate
        // (the dominant no-keyword call shape; bypasses the args-array
        // InvokeSlow) alongside the full variadic keyword-parsing entry.
        static void RegisterStringCmp(string name,
            Func<LispObject[], LispObject> variadic,
            Func<LispObject, LispObject, LispObject> twoArg,
            Func<LispObject, LispObject, LispObject, LispObject, LispObject> fourArg,
            Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> sixArg)
        {
            var fn = new LispFunction(variadic, name, -1);
            fn.SetDirectDelegate(twoArg);
            // 4 args = one keyword pair, e.g. the (string= name prefix :end1 n) that
            // prefix checks are written with: the dominant keyworded shape.
            fn.SetDirectDelegate(fourArg);
            // 6 args = two keyword pairs, e.g. (string= a b :start1 i :end1 j).
            fn.SetDirectDelegate(sixArg);
            Emitter.CilAssembler.RegisterFunction(name, fn);
        }
        RegisterStringCmp("STRING=",  Runtime.StringEq,    Runtime.StringEq2,    Runtime.StringEq4,
                          Runtime.StringEq6);
        RegisterStringCmp("STRING<",  Runtime.StringLt,    Runtime.StringLt2,    Runtime.StringLt4,
                          Runtime.StringLt6);
        RegisterStringCmp("STRING>",  Runtime.StringGt,    Runtime.StringGt2,    Runtime.StringGt4,
                          Runtime.StringGt6);
        RegisterStringCmp("STRING<=", Runtime.StringLe,    Runtime.StringLe2,    Runtime.StringLe4,
                          Runtime.StringLe6);
        RegisterStringCmp("STRING>=", Runtime.StringGe,    Runtime.StringGe2,    Runtime.StringGe4,
                          Runtime.StringGe6);
        RegisterStringCmp("STRING/=", Runtime.StringNotEq, Runtime.StringNotEq2, Runtime.StringNotEq4,
                          Runtime.StringNotEq6);
        RegisterStringCmp("STRING-EQUAL",        Runtime.StringEqualFn,     Runtime.StringEqual2,
                          Runtime.StringEqual4, Runtime.StringEqual6);
        RegisterStringCmp("STRING-NOT-EQUAL",    Runtime.StringNotEqualFn,  Runtime.StringNotEqual2,
                          Runtime.StringNotEqual4, Runtime.StringNotEqual6);
        RegisterStringCmp("STRING-LESSP",        Runtime.StringLessp,       Runtime.StringLessp2,
                          Runtime.StringLessp4, Runtime.StringLessp6);
        RegisterStringCmp("STRING-GREATERP",     Runtime.StringGreaterp,    Runtime.StringGreaterp2,
                          Runtime.StringGreaterp4, Runtime.StringGreaterp6);
        RegisterStringCmp("STRING-NOT-GREATERP", Runtime.StringNotGreaterp, Runtime.StringNotGreaterp2,
                          Runtime.StringNotGreaterp4, Runtime.StringNotGreaterp6);
        RegisterStringCmp("STRING-NOT-LESSP",    Runtime.StringNotLessp,    Runtime.StringNotLessp2,
                          Runtime.StringNotLessp4, Runtime.StringNotLessp6);
        // Case functions: direct entries for no, one and two keyword pairs.
        static void RegisterCase(string name,
            Func<LispObject[], LispObject> variadic,
            Func<LispObject, LispObject> one,
            Func<LispObject, LispObject, LispObject, LispObject> three,
            Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> five)
        {
            var fn = new LispFunction(variadic, name, -1);
            fn.SetDirectDelegate(one);
            fn.SetDirectDelegate(three);
            fn.SetDirectDelegate(five);
            Emitter.CilAssembler.RegisterFunction(name, fn);
        }
        RegisterCase("STRING-UPCASE", Runtime.StringUpcase,
                     Runtime.StringUpcase1, Runtime.StringUpcase3, Runtime.StringUpcase5);
        RegisterCase("STRING-DOWNCASE", Runtime.StringDowncase,
                     Runtime.StringDowncase1, Runtime.StringDowncase3, Runtime.StringDowncase5);
        RegisterCase("STRING-CAPITALIZE", Runtime.StringCapitalize,
                     Runtime.StringCapitalize1, Runtime.StringCapitalize3, Runtime.StringCapitalize5);
        RegisterCase("NSTRING-UPCASE", Runtime.NStringUpcase,
                     Runtime.NStringUpcase1, Runtime.NStringUpcase3, Runtime.NStringUpcase5);
        RegisterCase("NSTRING-DOWNCASE", Runtime.NStringDowncase,
                     Runtime.NStringDowncase1, Runtime.NStringDowncase3, Runtime.NStringDowncase5);
        RegisterCase("NSTRING-CAPITALIZE", Runtime.NStringCapitalize,
                     Runtime.NStringCapitalize1, Runtime.NStringCapitalize3, Runtime.NStringCapitalize5);

        // ELT
        Startup.RegisterBinary("ELT", Runtime.Elt);
        // REVERSE, NREVERSE
        Startup.RegisterUnary("REVERSE", Runtime.Reverse);
        Startup.RegisterUnary("NREVERSE", Runtime.Nreverse);
        // SUBSEQ
        Emitter.CilAssembler.RegisterFunction("SUBSEQ",
            new LispFunction(args => {
                if (args.Length < 2 || args.Length > 3)
                    throw new LispErrorException(new LispProgramError($"SUBSEQ: wrong number of arguments: {args.Length} (expected 2-3)"));
                var end = args.Length > 2 ? args[2] : Nil.Instance;
                return Runtime.Subseq(args[0], args[1], end);
            }, "SUBSEQ", -1));

        // SORT, STABLE-SORT
        var sortFn = new LispFunction(args => { Runtime.CheckArityMin("SORT", args, 2); return Runtime.SortFull(args); });
        sortFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Sort);
        sortFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Sort4);
        Emitter.CilAssembler.RegisterFunction("SORT", sortFn);
        var stableSortFn = new LispFunction(args => { Runtime.CheckArityMin("STABLE-SORT", args, 2); return Runtime.StableSortFull(args); });
        stableSortFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.StableSort);
        stableSortFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.StableSort4);
        Emitter.CilAssembler.RegisterFunction("STABLE-SORT", stableSortFn);
        // SEARCH. Direct entries for the shapes real code writes: no keywords, and
        // one to three keyword pairs. Without them every call built an argument
        // array, including the two-argument one whose implementation already
        // existed but was never installed.
        var searchFn = new LispFunction(args => Runtime.SearchFull(args));
        searchFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Search);
        searchFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Search4);
        searchFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Search6);
        searchFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Search8);
        Emitter.CilAssembler.RegisterFunction("SEARCH", searchFn);
        // COPY-SEQ
        Startup.RegisterUnary("COPY-SEQ", Runtime.CopySeq);

        // The Lisp MAP / MAP-INTO definitions call this once, after both are defined,
        // to install direct entries over the fast walks above.
        Emitter.CilAssembler.RegisterFunction("%ATTACH-MAP-FAST-PATHS",
            new LispFunction(Runtime.AttachMapFastPaths, "%ATTACH-MAP-FAST-PATHS", 3));

        // CONCATENATE
        Emitter.CilAssembler.RegisterFunction("CONCATENATE",
            new LispFunction(args => {
                Runtime.CheckArityMin("CONCATENATE", args, 1);
                var seqs = new LispObject[args.Length - 1];
                Array.Copy(args, 1, seqs, 0, seqs.Length);
                return Runtime.Concatenate(args[0], seqs);
            }, "CONCATENATE", -1));
    }


}
