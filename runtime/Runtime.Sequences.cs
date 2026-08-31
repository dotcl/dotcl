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
        for (int i = 2; i < args.Length; i += 2)
        {
            if (args[i] is not Symbol kwSym)
                throw new LispErrorException(new LispProgramError($"{fname}: not a keyword: {args[i]}"));
            var kwName = kwSym.Name;
            var val = args[i + 1];
            switch (kwName)
            {
                case "START1": start1 = val is Fixnum fs1 ? (int)fs1.Value : val is Nil ? 0 : throw new LispErrorException(new LispProgramError($"{fname}: :start1 must be integer")); break;
                case "END1": end1 = val is Fixnum fe1 ? (int)fe1.Value : val is Nil ? s1.Length : throw new LispErrorException(new LispProgramError($"{fname}: :end1 must be integer or nil")); break;
                case "START2": start2 = val is Fixnum fs2 ? (int)fs2.Value : val is Nil ? 0 : throw new LispErrorException(new LispProgramError($"{fname}: :start2 must be integer")); break;
                case "END2": end2 = val is Fixnum fe2 ? (int)fe2.Value : val is Nil ? s2.Length : throw new LispErrorException(new LispProgramError($"{fname}: :end2 must be integer or nil")); break;
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
    /// args array — the (string= name prefix :end1 n) shape, which prefix checks make the
    /// most common keyworded string comparison by a wide margin. Returns false when the
    /// pair is anything else (:allow-other-keys, a non-integer bound, an unknown keyword),
    /// leaving the caller to fall back to the shared variadic parser so behaviour and
    /// error messages stay in one place.</summary>
    private static bool TryStringCmp4(LispObject a, LispObject b, LispObject k, LispObject v,
                                      string fname, bool caseInsensitive, out (int pos, int cmp) result)
    {
        result = default;
        if (k is not Symbol kw || v is not Fixnum fv) return false;
        var s1 = ToStringDesignator(a, fname);
        var s2 = ToStringDesignator(b, fname);
        int start1 = 0, end1 = s1.Length, start2 = 0, end2 = s2.Length;
        switch (kw.Name)
        {
            case "START1": start1 = (int)fv.Value; break;
            case "END1": end1 = (int)fv.Value; break;
            case "START2": start2 = (int)fv.Value; break;
            case "END2": end2 = (int)fv.Value; break;
            default: return false;
        }
        if (start1 < 0 || end1 > s1.Length || start1 > end1 ||
            start2 < 0 || end2 > s2.Length || start2 > end2) return false;   // let the general path report
        result = CompareSubstrings(s1, start1, end1, s2, start2, end2, caseInsensitive);
        return true;
    }

    // Compare substrings; returns (mismatchPos, cmpSign) where:
    //   mismatchPos = index in s1 of first difference (or start1 + min(len1,len2))
    //   cmpSign = negative(s1<s2), 0(equal), positive(s1>s2)
    private static (int pos, int cmp) CompareSubstrings(string s1, int start1, int end1,
        string s2, int start2, int end2, bool ignoreCase)
    {
        int len1 = end1 - start1, len2 = end2 - start2;
        int minLen = Math.Min(len1, len2);
        for (int i = 0; i < minLen; i++)
        {
            char c1 = s1[start1 + i], c2 = s2[start2 + i];
            if (ignoreCase) { c1 = char.ToUpperInvariant(c1); c2 = char.ToUpperInvariant(c2); }
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
    // coercions and a full-range compare — no keyword loop runs, no arity/
    // odd-keyword error can fire. Each 2-arg entry keeps its own function's
    // (pos, cmp) → result mapping unchanged.
    private static (int pos, int cmp) CompareFull2(LispObject a, LispObject b,
                                                   string fname, bool ignoreCase)
    {
        var s1 = ToStringDesignator(a, fname);
        var s2 = ToStringDesignator(b, fname);
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

    public static LispObject Elt(LispObject seq, LispObject index)
    {
        if (index is not Fixnum f)
            throw new LispErrorException(new LispTypeError("ELT: index must be integer", index));
        long idx = f.Value;
        if (idx < 0)
            throw new LispErrorException(new LispTypeError($"ELT: index {idx} is negative", index));
        int i = (int)idx;
        if (seq is LispVector v)
        {
            if (i >= v.Length) throw new LispErrorException(new LispTypeError($"ELT: index {i} out of bounds for vector of length {v.Length}", index));
            return v[i];
        }
        if (seq is LispString s)
        {
            if (i >= s.Length) throw new LispErrorException(new LispTypeError($"ELT: index {i} out of range for string of length {s.Length}", index));
            return LispChar.Make(s[i]);
        }
        if (seq is Nil)
            throw new LispErrorException(new LispTypeError($"ELT: index {i} out of bounds for empty sequence", index));
        if (seq is Cons)
        {
            var cur = seq;
            for (int j = 0; j < i; j++)
            {
                if (cur is not Cons c) throw new LispErrorException(new LispTypeError($"ELT: index {i} out of bounds for list", index));
                cur = c.Cdr;
            }
            if (cur is Cons cn) return cn.Car;
            throw new LispErrorException(new LispTypeError($"ELT: index {i} out of bounds for list", index));
        }
        throw new LispErrorException(new LispTypeError("ELT: not a sequence", seq));
    }

    /// <summary>Check that START and END really are bounding indices of a sequence of
    /// LENGTH elements (CLHS: 0 &lt;= start &lt;= end &lt;= length), signalling a type error if
    /// not. Without this a too-large END silently produced a wrong answer rather than an
    /// error — (subseq '(1 2 3) 0 99) returned the list padded with 96 NILs.</summary>
    private static void CheckBoundingIndices(int start, int end, int length, string fname)
    {
        if (start < 0 || start > length)
            throw new LispErrorException(new LispTypeError(
                $"{fname}: start index {start} is not in [0, {length}]", Fixnum.Make(start)));
        if (end < start || end > length)
            throw new LispErrorException(new LispTypeError(
                $"{fname}: end index {end} is not in [{start}, {length}]", Fixnum.Make(end)));
    }

    public static LispObject Subseq(LispObject seq, LispObject start, LispObject end)
    {
        int s = (start is Fixnum fs) ? (int)fs.Value : throw new LispErrorException(new LispTypeError("SUBSEQ: start must be integer", start));
        int? e = end is Nil ? null : (end is Fixnum fe ? (int?)fe.Value : throw new LispErrorException(new LispTypeError("SUBSEQ: end must be integer or nil", end)));

        if (seq is LispString str)
        {
            int endIdx = e ?? str.Length;
            CheckBoundingIndices(s, endIdx, str.Length, "SUBSEQ");
            return new LispString(str.Value.Substring(s, endIdx - s));
        }
        if (seq is LispVector vec)
        {
            int endIdx = e ?? vec.Length;
            CheckBoundingIndices(s, endIdx, vec.Length, "SUBSEQ");
            var items = new LispObject[endIdx - s];
            for (int i = s; i < endIdx; i++) items[i - s] = vec.GetElement(i);
            return new LispVector(items, vec.ElementTypeName);
        }
        if (seq is Cons || seq is Nil)
        {
            // List subseq
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
        throw new LispErrorException(new LispTypeError("SUBSEQ: not a sequence", seq));
    }

    public static LispObject CopySeq(LispObject seq)
    {
        if (seq is LispString str)
            return new LispString(new string(str.Value.ToCharArray()));
        if (seq is LispVector v)
        {
            // Copy elements, preserve ElementTypeName (important for char/bit vectors)
            var items = new LispObject[v.Length];
            for (int i = 0; i < v.Length; i++) items[i] = v[i];
            return new LispVector(items, v.ElementTypeName);
        }
        if (seq is Nil) return Nil.Instance;
        if (seq is Cons)
            return CopyList(seq);
        throw new LispErrorException(new LispTypeError("COPY-SEQ: not a sequence", seq));
    }

    private static void CollectSequenceElements(LispObject seq, List<LispObject> items)
    {
        if (seq is Nil) return;
        if (seq is Cons)
        {
            var cur = seq;
            while (cur is Cons c) { items.Add(c.Car); cur = c.Cdr; }
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
            else if (seq is Cons) { for (var cur = seq; cur is Cons c; cur = c.Cdr) n++; }
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

    public static LispObject Concatenate(LispObject resultType, params LispObject[] sequences)
    {
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

        if (typeName == "STRING" || typeName == "SIMPLE-STRING" || typeName == "BASE-STRING" ||
            typeName == "SIMPLE-BASE-STRING" ||
            (typeName == "VECTOR" && resultType is Cons rtc && rtc.Cdr is Cons rtc2 &&
             rtc2.Car is Symbol etSym && etSym.Name is "CHARACTER" or "BASE-CHAR" or "STANDARD-CHAR"))
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
                else if (seq is Cons) { for (var cur = seq; cur is Cons c; cur = c.Cdr) Emit(c.Car); }
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
                // Check compound size constraint: (vector * N) where N is the required length
                var dimsSpec = (rtc0.Cdr as Cons)?.Cdr as Cons;
                var sizeArg = dimsSpec?.Car;
                if (sizeArg is Fixnum sizeF)
                    if (count != (int)sizeF.Value)
                        throw new LispErrorException(new LispTypeError($"CONCATENATE: result has {count} elements, type requires {sizeF.Value}", resultType));
                else if (sizeArg is Cons sizeList && sizeList.Car is Fixnum dimFix)
                    if (count != (int)dimFix.Value)
                        throw new LispErrorException(new LispTypeError($"CONCATENATE: result has {count} elements, type requires {dimFix.Value}", resultType));
            }
            return new LispVector(items, elemTypeName);
        }
        // Try deftype expansion for compound or named sequence types
        if (resultType is Symbol typeAlias && TypeExpanders.TryGetValue(typeAlias.Name, out var concatExpander))
        {
            var expanded = Funcall(concatExpander);
            if (expanded is not Symbol es || es.Name != typeAlias.Name)
                return Concatenate(expanded, sequences);
        }
        if (resultType is Cons compAlias && compAlias.Car is Symbol compHead
            && TypeExpanders.TryGetValue(compHead.Name, out var compConcatExpander))
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
        throw new LispErrorException(new LispTypeError($"CONCATENATE: not a sequence type: {resultType}", resultType));
    }

    public static LispObject Sort(LispObject seq, LispObject predicate)
    {
        return SortImpl(seq, predicate, null);
    }

    public static LispObject StableSort(LispObject seq, LispObject predicate)
    {
        return SortImpl(seq, predicate, null, stable: true);
    }

    // 4-arg direct entry: (sort seq pred k v) — the only keyword SORT takes is
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
                    return SortImpl(seq, predicate, karg as LispFunction, stable);
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
        bool allowOtherKeys = false;
        for (int i = 2; i < args.Length - 1; i += 2)
        {
            if (args[i] is not Symbol sk)
                throw new LispErrorException(new LispProgramError($"{name}: keyword must be a symbol, got {args[i]}"));
            if (sk.Name == "ALLOW-OTHER-KEYS" && args[i + 1] != Nil.Instance)
                allowOtherKeys = true;
        }
        LispFunction? keyFn = null;
        for (int i = 2; i < args.Length - 1; i += 2)
        {
            if (args[i] is not Symbol ks)
                throw new LispErrorException(new LispProgramError($"{name}: keyword must be a symbol, got {args[i]}"));
            switch (ks.Name)
            {
                case "KEY":
                    var karg = args[i + 1];
                    if (karg is Symbol ksym && ksym.Function is LispFunction ksf) karg = ksf;
                    if (karg is LispFunction kf) keyFn = kf;
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

    private static int SortCompare(LispFunction fn, LispFunction? keyFn, LispObject a, LispObject b)
    {
        var ka = ApplyKeyFn(keyFn, a);
        var kb = ApplyKeyFn(keyFn, b);
        if (IsTruthy(fn.Invoke2(ka, kb))) return -1;
        if (IsTruthy(fn.Invoke2(kb, ka))) return 1;
        return 0;
    }

    // .NET wraps comparator exceptions in InvalidOperationException or ArgumentException.
    // Unwrap and rethrow Lisp control/error exceptions so they propagate correctly.
    private static void UnwrapSortException(Exception ex)
    {
        var inner = ex.InnerException;
        if (inner is LispErrorException or HandlerCaseInvocationException
            or BlockReturnException or CatchThrowException or GoException or RestartInvocationException)
            System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(inner).Throw();
        // Otherwise it's a genuine sort inconsistency (e.g. inconsistent comparator) - ignore
    }

    // Sort ITEMS in place. Array.Sort is introsort — fine for SORT, which ANSI
    // leaves unstable, but STABLE-SORT must keep the original order of elements
    // the predicate considers equal. Stability is obtained by sorting a
    // permutation of indices and breaking ties on the index, so both modes share
    // one comparator and one exception-unwrapping path.
    /// <summary>Comparator for SORT. Passing a lambda here allocated a display class
    /// for the captured predicate and key, a delegate over it, and the wrapper
    /// Array.Sort puts around a Comparison -- three objects before the first
    /// comparison ran. One object holding the two functions does the same work.</summary>
    private sealed class LispComparer : System.Collections.Generic.IComparer<LispObject>
    {
        private readonly LispFunction _fn;
        private readonly LispFunction? _keyFn;
        public LispComparer(LispFunction fn, LispFunction? keyFn) { _fn = fn; _keyFn = keyFn; }
        public int Compare(LispObject? a, LispObject? b) => SortCompare(_fn, _keyFn, a!, b!);
    }

    /// <summary>Comparator for STABLE-SORT: orders a permutation of indices and breaks
    /// ties on the index, so elements the predicate calls equal keep their order.</summary>
    private sealed class LispStableComparer : System.Collections.Generic.IComparer<int>
    {
        private readonly LispObject[] _src;
        private readonly LispFunction _fn;
        private readonly LispFunction? _keyFn;
        public LispStableComparer(LispObject[] src, LispFunction fn, LispFunction? keyFn)
        { _src = src; _fn = fn; _keyFn = keyFn; }
        public int Compare(int i, int j)
        {
            int c = SortCompare(_fn, _keyFn, _src[i], _src[j]);
            return c != 0 ? c : i.CompareTo(j);
        }
    }

    private static void SortObjects(LispObject[] items, LispFunction fn, LispFunction? keyFn, bool stable)
    {
        // .NET wraps comparator exceptions in InvalidOperationException or ArgumentException.
        // Unwrap and rethrow Lisp errors/control exceptions.
        try
        {
            if (!stable)
            {
                Array.Sort(items, new LispComparer(fn, keyFn));
                return;
            }
            var src = (LispObject[])items.Clone();
            var order = new int[items.Length];
            for (int i = 0; i < order.Length; i++) order[i] = i;
            Array.Sort(order, new LispStableComparer(src, fn, keyFn));
            for (int i = 0; i < items.Length; i++) items[i] = src[order[i]];
        }
        catch (InvalidOperationException ioe) { UnwrapSortException(ioe); }
        catch (ArgumentException ae) { UnwrapSortException(ae); }
    }

    private static LispObject SortImpl(LispObject seq, LispObject predicate, LispFunction? keyFn, bool stable = false)
    {
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
            return result;
        }
        if (seq is LispString s)
        {
            var chars = s.Value.ToCharArray();
            Array.Reverse(chars);
            return new LispString(new string(chars));
        }
        if (seq is LispVector v)
        {
            var items = new LispObject[v.Length];
            for (int i = 0; i < v.Length; i++) items[i] = v.ElementAt(v.Length - 1 - i);
            return new LispVector(items, v.ElementTypeName);
        }
        throw new LispErrorException(new LispTypeError("REVERSE: not a sequence", seq));
    }

    public static LispObject Coerce(LispObject obj, LispObject resultType)
    {

        // Handle class objects as type specifiers
        if (resultType is LispClass lc)
            return Coerce(obj, lc.Name);

        // Handle compound type specifiers like (VECTOR *), (VECTOR * 2), (SIMPLE-ARRAY ...), etc.
        if (resultType is Cons compType && compType.Car is Symbol headSym)
        {
            // Per CLHS: if object already satisfies the type, return it as-is
            if (IsTruthy(Typep(obj, resultType))) return obj;

            string head = headSym.Name;
            if (head is "VECTOR" or "SIMPLE-VECTOR" or "ARRAY" or "SIMPLE-ARRAY")
            {
                // (simple-array element-type dims) or (vector element-type size)
                var rest1 = compType.Cdr as Cons;       // (element-type . rest)
                var elemTypeSpec = rest1?.Car;           // element-type or * or T
                var rest2 = rest1?.Cdr as Cons;          // (dims) or (size)
                var sizeSpec = rest2?.Car;               // dims list (*) or fixnum size or *

                // Parse element type — * and T both mean "any element"
                string elemTypeName = "T";
                if (elemTypeSpec != null && !(elemTypeSpec is T) && !(elemTypeSpec is Symbol wc && wc.Name == "*"))
                    elemTypeName = ParseElementTypeName(elemTypeSpec);

                // Get the base vector (using generic coerce path)
                var result = Coerce(obj, Startup.Sym("VECTOR"));

                // If a specific element type was requested, re-wrap with that type
                if (elemTypeName != "T" && result is LispVector tv)
                {
                    if (tv.ElementTypeName != elemTypeName)
                    {
                        var items = new LispObject[tv.Length];
                        for (int i = 0; i < tv.Length; i++) items[i] = tv.ElementAt(i);
                        result = new LispVector(items, elemTypeName);
                    }
                }

                // Check size constraint if specified
                if (sizeSpec is Fixnum sizeFix)
                {
                    int expectedLen = (int)sizeFix.Value;
                    int actualLen = result is LispVector rv ? rv.Length : (result is LispString rs ? rs.Length : 0);
                    if (actualLen != expectedLen)
                        throw new LispErrorException(new LispTypeError($"COERCE: result length {actualLen} does not match required length {expectedLen}", obj));
                }
                else if (sizeSpec is Cons sizeList && sizeList.Car is Fixnum dimFix)
                {
                    int expectedLen = (int)dimFix.Value;
                    int actualLen = result is LispVector rv2 ? rv2.Length : (result is LispString rs2 ? rs2.Length : 0);
                    if (actualLen != expectedLen)
                        throw new LispErrorException(new LispTypeError($"COERCE: result length {actualLen} does not match required length {expectedLen}", obj));
                }
                return result;
            }
            // Try compound deftype expansion: (my-type args...) → expanded type
            if (TypeExpanders.TryGetValue(headSym.Name, out var compExpander))
            {
                var compArgs = ToList(compType.Cdr).ToArray();
                var compExpanded = Funcall(compExpander, compArgs);
                if (!ReferenceEquals(compExpanded, resultType))
                    return Coerce(obj, compExpanded);
            }
            if (head is "LIST") return Coerce(obj, Startup.Sym("LIST"));
            if (head is "STRING" or "SIMPLE-STRING" or "BASE-STRING") return Coerce(obj, Startup.Sym(head));
            if (head is "COMPLEX")
            {
                var rest1 = compType.Cdr as Cons;
                var partType = rest1?.Car;
                var num = AsNumber(obj);
                Number real, imag;
                if (num is LispComplex cx) { real = cx.Real; imag = cx.Imaginary; }
                else { real = num; imag = Fixnum.Make(0); }
                // Coerce each part through Coerce(part, partType) rather than matching
                // a literal float name, so a deftype part — e.g. (complex flonum), where
                // flonum is a deftype expanding to double-float — is expanded and the
                // parts converted. A wildcard/absent part type leaves the parts as-is.
                if (partType is not null && partType is not Nil
                    && !(partType is Symbol wsym && wsym.Name == "*"))
                {
                    real = AsNumber(Coerce(real, partType));
                    imag = AsNumber(Coerce(imag, partType));
                }
                return LispComplex.Of(real, imag);
            }
            // Compound float types — e.g. (DOUBLE-FLOAT low high), which a deftype
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
                if (obj is LispVector lv)
                {
                    LispObject vacc = Nil.Instance;
                    for (int i3 = lv.Length - 1; i3 >= 0; i3--) vacc = new Cons(lv.ElementAt(i3), vacc);
                    return vacc;
                }
                throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to list", obj));

            case "STRING": case "SIMPLE-STRING": case "BASE-STRING": case "SIMPLE-BASE-STRING":
                if (obj is LispString) return obj;
                if (obj is Nil) return new LispString("");  // empty list → empty string
                if (obj is Symbol sym) return new LispString(sym.Name);
                if (obj is T) return new LispString("T");
                if (obj is LispChar ch) return new LispString(ch.Value.ToString());
                if (obj is Cons)
                {
                    var sb = new System.Text.StringBuilder();
                    var cur = obj;
                    while (cur is Cons c2)
                    {
                        if (c2.Car is LispChar lch) sb.Append(lch.Value);
                        else throw new LispErrorException(new LispTypeError("COERCE: list element not a character", c2.Car));
                        cur = c2.Cdr;
                    }
                    return new LispString(sb.ToString());
                }
                if (obj is LispVector vec)
                {
                    // For a SIMPLE-STRING target the result must be a *simple* string
                    // (CLHS): a fill-pointered / adjustable / displaced char-vector must
                    // be copied to a fresh simple string, not returned as-is. Returning
                    // the non-simple vector broke code that (coerce … 'simple-string)s
                    // and then relies on simplicity — e.g. cl-ppcre's
                    // maybe-coerce-to-simple-string on parser-built adjustable strings.
                    bool wantSimple = typeName is "SIMPLE-STRING" or "SIMPLE-BASE-STRING";
                    // A char-vector already satisfies (BASE-)STRING. For a SIMPLE
                    // target return it as-is only if it is already simple (matches
                    // CheckSimpleType's SIMPLE-STRING criterion: no fill pointer,
                    // rank 1) — CLHS: coerce returns the object itself when it is
                    // already of the type. Otherwise fall through and copy.
                    if (vec.IsCharVector
                        && (!wantSimple || (!vec.HasFillPointer && vec.Rank == 1)))
                        return obj;
                    var sb = new System.Text.StringBuilder(vec.Length);
                    for (int i = 0; i < vec.Length; i++)
                    {
                        if (vec.ElementAt(i) is LispChar lch) sb.Append(lch.Value);
                        else throw new LispErrorException(new LispTypeError("COERCE: vector element not a character", vec.ElementAt(i)));
                    }
                    return new LispString(sb.ToString());
                }
                throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to string", obj));

            case "CHARACTER":
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
                if (obj is Number nf) return new SingleFloat((float)Arithmetic.ToDouble(nf));
                throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to float", obj));

            case "DOUBLE-FLOAT": case "LONG-FLOAT":
                if (obj is DoubleFloat) return obj;
                if (obj is Number nd) return new DoubleFloat(Arithmetic.ToDouble(nd));
                throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to double-float", obj));

            case "DECIMAL":
                // dotcl:decimal — a distinct exactness category. Exact reals (integer,
                // ratio) convert exactly or signal (a denominator with a prime factor
                // other than 2/5, e.g. 1/3, is not representable — no silent rounding).
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
                // Any LispVector or LispString satisfies VECTOR — return as-is
                if (obj is LispVector || obj is LispString) return obj;
                if (obj is Nil) return new LispVector(Array.Empty<LispObject>());
                if (obj is Cons) return new LispVector(ListToArray(obj));
                throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to vector", obj));

            case "SIMPLE-VECTOR":
                // Already a T-element-type vector (simple-vector)? return as-is
                if (obj is LispVector sv2 && (sv2.ElementTypeName == null || sv2.ElementTypeName == "T"))
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
                if (obj is LispVector sv3)
                {
                    var items2 = new LispObject[sv3.Length];
                    for (int i = 0; i < sv3.Length; i++) items2[i] = sv3.ElementAt(i);
                    return new LispVector(items2);
                }
                throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to vector", obj));

            case "BIT-VECTOR": case "SIMPLE-BIT-VECTOR":
                if (obj is LispVector bv && bv.IsBitVector) return obj;
                {
                    if (obj is Nil) return new LispVector(Array.Empty<LispObject>(), "BIT");
                    if (obj is Cons) return new LispVector(ListToArray(obj), "BIT");
                    if (obj is LispVector sv)
                    {
                        var bitItems = new LispObject[sv.Length];
                        for (int i2 = 0; i2 < sv.Length; i2++) bitItems[i2] = sv.ElementAt(i2);
                        return new LispVector(bitItems, "BIT");
                    }
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
                if (obj is Cons lambdaCons && lambdaCons.Car is Symbol lambdaSym && lambdaSym.Name == "LAMBDA")
                    return Eval(obj);
                throw new LispErrorException(new LispTypeError($"COERCE: cannot coerce to function", obj));

            case "T":
                return obj;

            default:
                // If already of the target type, return as-is
                if (IsTruthy(Typep(obj, resultType))) return obj;
                // Try deftype expansion for named type aliases
                if (TypeExpanders.TryGetValue(typeName, out var symExpander))
                {
                    var symExpanded = Funcall(symExpander);
                    if (symExpanded is not Symbol se || se.Name != typeName)
                        return Coerce(obj, symExpanded);
                }
                throw new LispErrorException(new LispTypeError($"COERCE: cannot coerce to {typeName}", obj));
        }
    }

    // Helper: parse common sequence keyword args (test, test-not, key, start, end, from-end, count)
    private struct SeqKwArgs
    {
        public LispFunction? Test, TestNot, Key;
        public int Start;
        public int? End;
        public bool FromEnd;
        public int? Count; // null = no limit
    }

    private static SeqKwArgs ParseSeqKwArgs(LispObject[] args, int kwStart, string fnName)
    {
        var kw = new SeqKwArgs();
        int kwCount = args.Length - kwStart;
        if (kwCount % 2 != 0)
            throw new LispErrorException(new LispProgramError($"{fnName}: odd number of keyword arguments"));
        bool? allowOtherKeys = null;
        bool hasUnknown = false;
        bool testSet = false, testNotSet = false, keySet = false, startSet = false, endSet = false, fromEndSet = false, countSet = false;
        // First pass: check :allow-other-keys
        for (int i = kwStart; i < args.Length - 1; i += 2)
            if (args[i] is Symbol kw0 && kw0.Name == "ALLOW-OTHER-KEYS" && allowOtherKeys == null)
                allowOtherKeys = IsTruthy(args[i + 1]);
        for (int i = kwStart; i < args.Length - 1; i += 2)
        {
            if (args[i] is not Symbol s)
                throw new LispErrorException(new LispProgramError($"{fnName}: keyword must be a symbol, got {args[i]}"));
            switch (s.Name)
            {
                case "TEST": if (!testSet) { kw.Test = CoerceToFunction(args[i + 1]); testSet = true; } break;
                case "TEST-NOT": if (!testNotSet) { kw.TestNot = CoerceToFunction(args[i + 1]); testNotSet = true; } break;
                case "KEY": if (!keySet) { if (args[i + 1] is not Nil) kw.Key = CoerceToFunction(args[i + 1]); keySet = true; } break;
                case "START":
                    if (!startSet) { kw.Start = (int)((Fixnum)args[i + 1]).Value; startSet = true; }
                    break;
                case "END":
                    if (!endSet) { kw.End = args[i + 1] is Fixnum ef ? (int?)ef.Value : null; endSet = true; }
                    break;
                case "FROM-END":
                    if (!fromEndSet) { kw.FromEnd = IsTruthy(args[i + 1]); fromEndSet = true; }
                    break;
                case "COUNT":
                    if (!countSet)
                    {
                        var cval = args[i + 1];
                        if (cval is Nil) { /* null = no limit */ }
                        else if (cval is Fixnum cf) { kw.Count = (int)Math.Max(Math.Min(cf.Value, int.MaxValue), int.MinValue); }
                        else if (cval is Bignum bg) { kw.Count = bg.Value.Sign < 0 ? int.MinValue : int.MaxValue; }
                        else throw new LispErrorException(new LispProgramError($"{fnName}: :count must be integer or nil"));
                        countSet = true;
                    }
                    break;
                case "ALLOW-OTHER-KEYS": break;
                default: hasUnknown = true; break;
            }
        }
        if (hasUnknown && allowOtherKeys != true)
            throw new LispErrorException(new LispProgramError($"{fnName}: unknown keyword argument"));
        return kw;
    }

    // :key / user-function results must flow as their PRIMARY value: a key fn
    // returning (values a b ...) hands an MvReturn to :test otherwise (SBCL's
    // %find-position with :key #'parse-optional-arg-spec hit STRING= with an
    // MvReturn at make-host-2 stem 8).
    private static LispObject ApplySeqKey(SeqKwArgs kw, LispObject elem)
        => kw.Key != null ? UnwrapMv(kw.Key.Invoke1(elem)) : elem;

    private static LispObject ApplyKeyFn(LispFunction? keyFn, LispObject elem)
        => keyFn != null ? UnwrapMv(keyFn.Invoke1(elem)) : elem;

    private static LispObject ApplySeqKey(ListKwArgs kw, LispObject elem)
        => kw.Key != null ? UnwrapMv(kw.Key.Invoke1(elem)) : elem;

    private static bool SeqTestMatch(LispObject item, LispObject elem, SeqKwArgs kw)
    {
        var val = ApplySeqKey(kw, elem);
        if (kw.TestNot != null)
            return !IsTruthy(kw.TestNot.Invoke2(item, val));
        if (kw.Test != null)
            return IsTruthy(kw.Test.Invoke2(item, val));
        return IsTrueEql(item, val); // default test is eql
    }

    public static LispObject Find(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("FIND: too few arguments"));
        return FindCore(args[0], args[1], ParseSeqKwArgs(args, 2, "FIND"));
    }

    // 6-arg direct entry: (find item seq k1 v1 k2 v2) — two keyword pairs, e.g.
    // (find name l :key #'symbol-name :test #'string=), which the compiler runs
    // per special/local-function lookup. Shared parser over a 4-element array;
    // the InvokeSlow path's 6-element args array is skipped. See Member6.
    public static LispObject Find6(LispObject item, LispObject seq,
                                   LispObject k1, LispObject v1, LispObject k2, LispObject v2) =>
        FindCore(item, seq, ParseSeqKwArgs(new[] { k1, v1, k2, v2 }, 0, "FIND"));

    // 2-arg direct entry: (find item seq) with no keywords -- the shape most
    // call sites have. Skips the args array the variadic path builds.
    public static LispObject Find2(LispObject item, LispObject seq) =>
        FindCore(item, seq, new SeqKwArgs());

    private static LispObject FindCore(LispObject item, LispObject seq, in SeqKwArgs kw)
    {
        int len = seq is LispVector v ? v.Length : seq is LispString ls ? ls.Length : (int)((Fixnum)Length(seq)).Value;
        int start = kw.Start;
        int end = kw.End ?? len;
        CheckBoundingIndices(start, end, len, "FIND");

        // :FROM-END over something indexable walks backwards and stops at the first
        // match -- the answer is the rightmost one either way, but scanning forward and
        // keeping the last applied the test to every element. POSITION already did it
        // this way; FIND did not.
        if (seq is LispVector vec)
        {
            if (kw.FromEnd)
            {
                for (int i = end - 1; i >= start; i--)
                    if (SeqTestMatch(item, vec[i], kw)) return vec[i];
                return Nil.Instance;
            }
            for (int i = start; i < end; i++)
                if (SeqTestMatch(item, vec[i], kw)) return vec[i];
            return Nil.Instance;
        }
        if (seq is LispString str)
        {
            if (kw.FromEnd)
            {
                for (int i = end - 1; i >= start; i--)
                {
                    var ch = LispChar.Make(str[i]);
                    if (SeqTestMatch(item, ch, kw)) return ch;
                }
                return Nil.Instance;
            }
            for (int i = start; i < end; i++)
            {
                var ch = LispChar.Make(str[i]);
                if (SeqTestMatch(item, ch, kw)) return ch;
            }
            return Nil.Instance;
        }
        // List
        var cur = seq;
        for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
        if (kw.FromEnd)
        {
            LispObject result = Nil.Instance;
            for (int i = start; i < end && cur is Cons c; i++) { if (SeqTestMatch(item, c.Car, kw)) result = c.Car; cur = c.Cdr; }
            return result;
        }
        for (int i = start; i < end && cur is Cons c2; i++) { if (SeqTestMatch(item, c2.Car, kw)) return c2.Car; cur = c2.Cdr; }
        return Nil.Instance;
    }

    public static LispObject FindIf(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("FIND-IF: too few arguments"));
        var predFn = CoerceToFunction(args[0]);
        return FindIfCore(predFn, args[1], ParseSeqKwArgs(args, 2, "FIND-IF"));
    }

    // 2-arg direct entry: (find-if pred seq) — no keywords. Skips the args array
    // and the ParseSeqKwArgs scan (a default SeqKwArgs is exactly its zero-keyword
    // result). See RemoveIf2.
    public static LispObject FindIf2(LispObject pred, LispObject seq) =>
        FindIfCore(CoerceToFunction(pred), seq, new SeqKwArgs());

    private static LispObject FindIfCore(LispFunction predFn, LispObject seq, in SeqKwArgs kw)
    {
        int len = seq is LispVector v ? v.Length : seq is LispString ls ? ls.Length : (int)((Fixnum)Length(seq)).Value;
        int start = kw.Start;
        int end = kw.End ?? len;
        CheckBoundingIndices(start, end, len, "FIND-IF");

        if (seq is LispVector vec)
        {
            if (kw.FromEnd)
            {
                // Backwards, stopping at the first match: see FindCore.
                for (int i = end - 1; i >= start; i--)
                {
                    var elem = ApplySeqKey(kw, vec[i]);
                    if (IsTruthy(predFn.Invoke1(elem))) return vec[i];
                }
                return Nil.Instance;
            }
            for (int i = start; i < end; i++)
            {
                var elem = ApplySeqKey(kw, vec[i]);
                if (IsTruthy(predFn.Invoke1(elem))) return vec[i];
            }
            return Nil.Instance;
        }
        if (seq is LispString str)
        {
            if (kw.FromEnd)
            {
                for (int i = end - 1; i >= start; i--)
                {
                    var ch = LispChar.Make(str[i]);
                    var elem = ApplySeqKey(kw, ch);
                    if (IsTruthy(predFn.Invoke1(elem))) return ch;
                }
                return Nil.Instance;
            }
            for (int i = start; i < end; i++)
            {
                var ch = LispChar.Make(str[i]);
                var elem = ApplySeqKey(kw, ch);
                if (IsTruthy(predFn.Invoke1(elem))) return ch;
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
                var elem = ApplySeqKey(kw, c.Car);
                if (IsTruthy(predFn.Invoke1(elem))) result = c.Car;
                cur = c.Cdr;
            }
            return result;
        }
        for (int i = start; i < end && cur is Cons c2; i++)
        {
            var elem = ApplySeqKey(kw, c2.Car);
            if (IsTruthy(predFn.Invoke1(elem))) return c2.Car;
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

    private static LispObject PositionCore(LispObject item, LispObject seq, in SeqKwArgs kw)
    {
        int len = seq is LispVector v ? v.Length : seq is LispString ls ? ls.Length : (int)((Fixnum)Length(seq)).Value;
        int start = kw.Start;
        int end = kw.End ?? len;
        CheckBoundingIndices(start, end, len, "POSITION");

        if (seq is LispVector vec)
        {
            if (kw.FromEnd)
            {
                for (int i = end - 1; i >= start; i--)
                    if (SeqTestMatch(item, vec[i], kw)) return Fixnum.Make(i);
                return Nil.Instance;
            }
            for (int i = start; i < end; i++)
                if (SeqTestMatch(item, vec[i], kw)) return Fixnum.Make(i);
            return Nil.Instance;
        }
        if (seq is LispString str)
        {
            if (kw.FromEnd)
            {
                for (int i = end - 1; i >= start; i--)
                    if (SeqTestMatch(item, LispChar.Make(str[i]), kw)) return Fixnum.Make(i);
                return Nil.Instance;
            }
            for (int i = start; i < end; i++)
                if (SeqTestMatch(item, LispChar.Make(str[i]), kw)) return Fixnum.Make(i);
            return Nil.Instance;
        }
        // List
        var cur = seq;
        for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
        if (kw.FromEnd)
        {
            LispObject result = Nil.Instance;
            for (int i = start; i < end && cur is Cons c; i++) { if (SeqTestMatch(item, c.Car, kw)) result = Fixnum.Make(i); cur = c.Cdr; }
            return result;
        }
        for (int i = start; i < end && cur is Cons c2; i++) { if (SeqTestMatch(item, c2.Car, kw)) return Fixnum.Make(i); cur = c2.Cdr; }
        return Nil.Instance;
    }

    public static LispObject PositionIf(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("POSITION-IF: too few arguments"));
        return PositionIfCore(CoerceToFunction(args[0]), args[1], ParseSeqKwArgs(args, 2, "POSITION-IF"));
    }

    /// <summary>(POSITION-IF predicate sequence) as a direct entry: the two arguments
    /// arrive in registers instead of an array built for the variadic entry to walk.</summary>
    public static LispObject PositionIf2(LispObject pred, LispObject seq) =>
        PositionIfCore(CoerceToFunction(pred), seq, new SeqKwArgs());

    private static LispObject PositionIfCore(LispFunction predFn, LispObject seq, in SeqKwArgs kw)
    {
        int len = seq is LispVector v ? v.Length : seq is LispString ls ? ls.Length : (int)((Fixnum)Length(seq)).Value;
        int start = kw.Start;
        int end = kw.End ?? len;
        CheckBoundingIndices(start, end, len, "POSITION-IF");

        if (seq is LispVector vec)
        {
            if (kw.FromEnd)
            {
                for (int i = end - 1; i >= start; i--)
                {
                    var elem = ApplySeqKey(kw, vec[i]);
                    if (IsTruthy(predFn.Invoke1(elem))) return Fixnum.Make(i);
                }
                return Nil.Instance;
            }
            for (int i = start; i < end; i++)
            {
                var elem = ApplySeqKey(kw, vec[i]);
                if (IsTruthy(predFn.Invoke1(elem))) return Fixnum.Make(i);
            }
            return Nil.Instance;
        }
        if (seq is LispString str)
        {
            if (kw.FromEnd)
            {
                for (int i = end - 1; i >= start; i--)
                {
                    var ch = LispChar.Make(str[i]);
                    var elem = ApplySeqKey(kw, ch);
                    if (IsTruthy(predFn.Invoke1(elem))) return Fixnum.Make(i);
                }
                return Nil.Instance;
            }
            for (int i = start; i < end; i++)
            {
                var ch = LispChar.Make(str[i]);
                var elem = ApplySeqKey(kw, ch);
                if (IsTruthy(predFn.Invoke1(elem))) return Fixnum.Make(i);
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
                var elem = ApplySeqKey(kw, c.Car);
                if (IsTruthy(predFn.Invoke1(elem)))
                {
                    // For lists with from-end, continue scanning
                    LispObject result = Fixnum.Make(i);
                    cur = c.Cdr;
                    for (int j = i + 1; j < end && cur is Cons c3; j++)
                    {
                        var elem2 = ApplySeqKey(kw, c3.Car);
                        if (IsTruthy(predFn.Invoke1(elem2))) result = Fixnum.Make(j);
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
            var elem = ApplySeqKey(kw, c2.Car);
            if (IsTruthy(predFn.Invoke1(elem))) return Fixnum.Make(i);
            cur = c2.Cdr;
        }
        return Nil.Instance;
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

    private static LispObject CountCore(LispObject item, LispObject seq, in SeqKwArgs kw)
    {
        int len = seq is LispVector v ? v.Length : seq is LispString ls ? ls.Length : (int)((Fixnum)Length(seq)).Value;
        int start = kw.Start;
        int end = kw.End ?? len;
        CheckBoundingIndices(start, end, len, "COUNT");
        int count = 0;

        if (seq is LispVector vec)
        {
            if (kw.FromEnd)
                for (int i = end - 1; i >= start; i--) { if (SeqTestMatch(item, vec[i], kw)) count++; }
            else
                for (int i = start; i < end; i++) { if (SeqTestMatch(item, vec[i], kw)) count++; }
            return Fixnum.Make(count);
        }
        if (seq is LispString str)
        {
            if (kw.FromEnd)
                for (int i = end - 1; i >= start; i--) { if (SeqTestMatch(item, LispChar.Make(str[i]), kw)) count++; }
            else
                for (int i = start; i < end; i++) { if (SeqTestMatch(item, LispChar.Make(str[i]), kw)) count++; }
            return Fixnum.Make(count);
        }
        // List - for from-end, collect elements then iterate in reverse
        if (kw.FromEnd)
        {
            var elems = new System.Collections.Generic.List<LispObject>();
            var cur = seq;
            for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
            for (int i = start; i < end && cur is Cons c2; i++) { elems.Add(c2.Car); cur = c2.Cdr; }
            for (int i = elems.Count - 1; i >= 0; i--) { if (SeqTestMatch(item, elems[i], kw)) count++; }
        }
        else
        {
            var cur = seq;
            for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
            for (int i = start; i < end && cur is Cons c2; i++) { if (SeqTestMatch(item, c2.Car, kw)) count++; cur = c2.Cdr; }
        }
        return Fixnum.Make(count);
    }

    public static LispObject CountIf(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("COUNT-IF: too few arguments"));
        return CountIfCore(CoerceToFunction(args[0]), args[1], ParseSeqKwArgs(args, 2, "COUNT-IF"));
    }

    /// <summary>(COUNT-IF predicate sequence) as a direct entry: the two arguments
    /// arrive in registers instead of an array built for the variadic entry to walk.</summary>
    public static LispObject CountIf2(LispObject pred, LispObject seq) =>
        CountIfCore(CoerceToFunction(pred), seq, new SeqKwArgs());

    private static LispObject CountIfCore(LispFunction predFn, LispObject seq, in SeqKwArgs kw)
    {
        int len = seq is LispVector v ? v.Length : seq is LispString ls ? ls.Length : (int)((Fixnum)Length(seq)).Value;
        int start = kw.Start;
        int end = kw.End ?? len;
        CheckBoundingIndices(start, end, len, "COUNT-IF");
        int count = 0;

        if (seq is LispVector vec)
        {
            if (kw.FromEnd)
                for (int i = end - 1; i >= start; i--) { var elem = ApplySeqKey(kw, vec[i]); if (IsTruthy(predFn.Invoke1(elem))) count++; }
            else
                for (int i = start; i < end; i++) { var elem = ApplySeqKey(kw, vec[i]); if (IsTruthy(predFn.Invoke1(elem))) count++; }
            return Fixnum.Make(count);
        }
        if (seq is LispString str)
        {
            if (kw.FromEnd)
                for (int i = end - 1; i >= start; i--) { var ch = LispChar.Make(str[i]); var elem = ApplySeqKey(kw, ch); if (IsTruthy(predFn.Invoke1(elem))) count++; }
            else
                for (int i = start; i < end; i++) { var ch = LispChar.Make(str[i]); var elem = ApplySeqKey(kw, ch); if (IsTruthy(predFn.Invoke1(elem))) count++; }
            return Fixnum.Make(count);
        }
        // List - for from-end, collect elements then iterate in reverse
        if (kw.FromEnd)
        {
            var elems = new System.Collections.Generic.List<LispObject>();
            var cur = seq;
            for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
            for (int i = start; i < end && cur is Cons c2; i++) { elems.Add(c2.Car); cur = c2.Cdr; }
            for (int i = elems.Count - 1; i >= 0; i--) { var elem = ApplySeqKey(kw, elems[i]); if (IsTruthy(predFn.Invoke1(elem))) count++; }
        }
        else
        {
            var cur = seq;
            for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
            for (int i = start; i < end && cur is Cons c2; i++) { var elem = ApplySeqKey(kw, c2.Car); if (IsTruthy(predFn.Invoke1(elem))) count++; cur = c2.Cdr; }
        }
        return Fixnum.Make(count);
    }

    // Helper: parse test/test-not/key keyword args for list functions (member, assoc, etc.)
    private struct ListKwArgs
    {
        public LispFunction? Test, TestNot, Key;
        public bool IsEqTest, IsEqlTest; // fast path flags
    }

    private struct ListKwSeen { public bool Test, TestNot, Key, Unknown; }

    /// <summary>Apply one keyword pair. The first setting of a keyword wins, which is
    /// what the standard says about a repeated keyword argument.</summary>
    private static void ApplyListKw(ref ListKwArgs kw, ref ListKwSeen seen,
                                    LispObject key, LispObject val, string fnName)
    {
        if (key is not Symbol s)
            throw new LispErrorException(new LispProgramError($"{fnName}: keyword must be a symbol, got {key}"));
        switch (s.Name)
        {
            case "TEST": if (!seen.Test) { kw.Test = CoerceToFunction(val); seen.Test = true; } break;
            case "TEST-NOT": if (!seen.TestNot) { kw.TestNot = CoerceToFunction(val); seen.TestNot = true; } break;
            case "KEY": if (!seen.Key) { if (val is not Nil) kw.Key = CoerceToFunction(val); seen.Key = true; } break;
            case "ALLOW-OTHER-KEYS": break;
            default: seen.Unknown = true; break;
        }
    }

    // EQ and EQL are looked up once. FindSymbol is a hash lookup, and it ran on
    // every MEMBER/ASSOC call that passed a :test.
    private static Symbol? s_eqlSym, s_eqSym;

    /// <summary>Work out which comparison fast path the parsed keywords allow.</summary>
    private static void FinishListKw(ref ListKwArgs kw)
    {
        if (kw.TestNot != null || kw.Key != null) return;
        if (kw.Test == null) { kw.IsEqlTest = true; return; }
        s_eqlSym ??= Startup.CL.FindSymbol("EQL").symbol;
        s_eqSym ??= Startup.CL.FindSymbol("EQ").symbol;
        if (kw.Test == s_eqlSym?.Function) kw.IsEqlTest = true;
        else if (kw.Test == s_eqSym?.Function) kw.IsEqTest = true;
    }

    private static ListKwArgs ParseListKwArgs(LispObject[] args, int kwStart, string fnName)
    {
        var kw = new ListKwArgs();
        if ((args.Length - kwStart) % 2 != 0)
            throw new LispErrorException(new LispProgramError($"{fnName}: odd number of keyword arguments"));
        bool? allowOtherKeys = null;
        for (int i = kwStart; i < args.Length - 1; i += 2)
            if (args[i] is Symbol kw0 && kw0.Name == "ALLOW-OTHER-KEYS" && allowOtherKeys == null)
                allowOtherKeys = IsTruthy(args[i + 1]);
        var seen = default(ListKwSeen);
        for (int i = kwStart; i < args.Length - 1; i += 2)
            ApplyListKw(ref kw, ref seen, args[i], args[i + 1], fnName);
        if (seen.Unknown && allowOtherKeys != true)
            throw new LispErrorException(new LispProgramError($"{fnName}: unknown keyword argument"));
        FinishListKw(ref kw);
        return kw;
    }

    /// <summary>One or two keyword pairs, straight from a direct entry's parameters.
    /// The pairs arrive in registers, and putting them into an array so the general
    /// parser could walk it was the whole cost of the call: 40 bytes for
    /// (member x l :test #'string=), which is how most of the standard library
    /// dispatches on a name.</summary>
    private static ListKwArgs ParseListKwPairs(LispObject k1, LispObject v1,
                                               LispObject k2, LispObject v2,
                                               bool hasSecond, string fnName)
    {
        var kw = new ListKwArgs();
        bool? allowOtherKeys = null;
        if (k1 is Symbol a1 && a1.Name == "ALLOW-OTHER-KEYS") allowOtherKeys = IsTruthy(v1);
        if (allowOtherKeys == null && hasSecond && k2 is Symbol a2 && a2.Name == "ALLOW-OTHER-KEYS")
            allowOtherKeys = IsTruthy(v2);
        var seen = default(ListKwSeen);
        ApplyListKw(ref kw, ref seen, k1, v1, fnName);
        if (hasSecond) ApplyListKw(ref kw, ref seen, k2, v2, fnName);
        if (seen.Unknown && allowOtherKeys != true)
            throw new LispErrorException(new LispProgramError($"{fnName}: unknown keyword argument"));
        FinishListKw(ref kw);
        return kw;
    }

    private static bool ListTestMatch(LispObject item, LispObject element, in ListKwArgs kw)
    {
        var k = ApplySeqKey(kw, element);
        if (kw.TestNot != null)
            return !IsTruthy(kw.TestNot.Invoke2(item, k));
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

    // No-keyword ListKwArgs — provably what ParseListKwArgs returns for zero
    // keyword pairs (loops don't run; TestNot/Key null, Test null → IsEqlTest).
    // Shared by the 2-arg direct entries of MEMBER/ASSOC.
    private static readonly ListKwArgs s_eqlListKw = new ListKwArgs { IsEqlTest = true };

    // 2-arg direct entry: (member item list) — no keyword pairs.
    public static LispObject Member2(LispObject item, LispObject list) =>
        MemberCore(item, list, s_eqlListKw);

    // 4-arg direct entry: (member item list kw val). The keyword pair goes
    // through the exact shared parser (over a 2-element array — half the
    // allocation of the InvokeSlow path's 4-element args array).
    public static LispObject Member4(LispObject item, LispObject list, LispObject k, LispObject v) =>
        MemberCore(item, list, ParseListKwPairs(k, v, Nil.Instance, Nil.Instance, false, "MEMBER"));

    // 6-arg direct entry: (member item list k1 v1 k2 v2) — two keyword pairs,
    // e.g. (member x l :key #'symbol-name :test #'string=), which the compiler's
    // special-variable lookup performs per variable reference. Same shared parser
    // over a 4-element array; the InvokeSlow path's 6-element args array is skipped.
    public static LispObject Member6(LispObject item, LispObject list,
                                     LispObject k1, LispObject v1, LispObject k2, LispObject v2) =>
        MemberCore(item, list, ParseListKwPairs(k1, v1, k2, v2, true, "MEMBER"));

    private static LispObject MemberCore(LispObject item, LispObject list, in ListKwArgs kw)
    {
        if (list is Nil) return Nil.Instance;
        if (list is not Cons)
            throw new LispErrorException(new LispTypeError("MEMBER: not a proper list", list, Startup.Sym("LIST")));

        // Fast path: eq test, no key
        if (kw.IsEqTest)
        {
            var cur = list;
            for (; cur is Cons c; cur = c.Cdr)
                if (IsEqRef(item, c.Car)) return c;
            if (cur is not Nil) throw new LispErrorException(new LispTypeError("MEMBER: not a proper list", cur, Startup.Sym("LIST")));
            return Nil.Instance;
        }
        // Fast path: eql test, no key
        if (kw.IsEqlTest)
        {
            var cur = list;
            for (; cur is Cons c; cur = c.Cdr)
                if (IsTrueEql(item, c.Car)) return c;
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
        var predFn = CoerceToFunction(args[0]);
        var list = args[1];
        if (list is not Nil && list is not Cons)
            throw new LispErrorException(new LispTypeError("MEMBER-IF: not a proper list", list));
        LispFunction? key = null;
        bool keySet = false;
        // Parse keyword args with validation
        int kwCount = args.Length - 2;
        if (kwCount % 2 != 0)
            throw new LispErrorException(new LispProgramError("MEMBER-IF: odd number of keyword arguments"));
        bool? allowOtherKeys = null;
        bool hasUnknown = false;
        for (int i = 2; i < args.Length - 1; i += 2)
            if (args[i] is Symbol kw0 && kw0.Name == "ALLOW-OTHER-KEYS" && allowOtherKeys == null)
                allowOtherKeys = IsTruthy(args[i + 1]);
        for (int i = 2; i < args.Length - 1; i += 2)
        {
            if (args[i] is not Symbol s)
                throw new LispErrorException(new LispProgramError($"MEMBER-IF: keyword must be a symbol, got {args[i]}"));
            switch (s.Name)
            {
                case "KEY": if (!keySet) { if (args[i + 1] is not Nil) key = CoerceToFunction(args[i + 1]); keySet = true; } break;
                case "ALLOW-OTHER-KEYS": break;
                default: hasUnknown = true; break;
            }
        }
        if (hasUnknown && allowOtherKeys != true)
            throw new LispErrorException(new LispProgramError("MEMBER-IF: unknown keyword argument"));
        var cur2 = list;
        for (; cur2 is Cons c; cur2 = c.Cdr)
        {
            var elem = key != null ? key.Invoke1(c.Car) : c.Car;
            if (IsTruthy(predFn.Invoke1(elem))) return c;
        }
        if (cur2 is not Nil) throw new LispErrorException(new LispTypeError("MEMBER-IF: not a proper list", cur2));
        return Nil.Instance;
    }

    // ADJOIN: (adjoin item list &key key test test-not) —
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

    // 2-/4-arg direct entries — see Member2/Member4.
    public static LispObject Assoc2(LispObject item, LispObject alist) =>
        AssocCore(item, alist, s_eqlListKw);

    public static LispObject Assoc4(LispObject item, LispObject alist, LispObject k, LispObject v) =>
        AssocCore(item, alist, ParseListKwPairs(k, v, Nil.Instance, Nil.Instance, false, "ASSOC"));

    // 6-arg direct entry: (assoc item alist k1 v1 k2 v2) — two keyword pairs.
    // See Member6.
    public static LispObject Assoc6(LispObject item, LispObject alist,
                                    LispObject k1, LispObject v1, LispObject k2, LispObject v2) =>
        AssocCore(item, alist, ParseListKwPairs(k1, v1, k2, v2, true, "ASSOC"));

    /// NIL is a legal (skipped) alist element; any other non-cons is a type error.
    private static void CheckAssocEntry(LispObject entry)
    {
        if (entry is not Nil)
            throw new LispErrorException(new LispTypeError(
                "ASSOC: alist entry is not a cons", entry, Startup.Sym("CONS")));
    }

    private static LispObject AssocCore(LispObject item, LispObject alist, in ListKwArgs kw)
    {
        if (alist is Nil) return Nil.Instance;

        // A NIL element is allowed and skipped (CLHS assoc); anything else that is
        // not a cons is a type error. Runtime.Assoc — what the compiler emits inline
        // for the 2-argument call — already signalled it, so (assoc 'z '((a . b) :bad))
        // errored when written literally but returned NIL through #'ASSOC, on BOTH
        // evaluator paths (ansi-test ASSOC.ERROR.11) — the same divergence unary #'-
        // had, where the inlined form is right and the function object is not.
        // The extra test only runs for entries that are not conses, so the eq/eql
        // fast paths keep their cost.
        // Fast path: eq test, no key
        if (kw.IsEqTest)
        {
            var cur = alist;
            for (; cur is Cons c; cur = c.Cdr)
            {
                if (c.Car is Cons pair) { if (IsEqRef(item, pair.Car)) return pair; }
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
        var predFn = CoerceToFunction(args[0]);
        var alist = args[1];
        if (alist is not Nil && alist is not Cons)
            throw new LispErrorException(new LispTypeError("ASSOC-IF: not a proper list", alist));
        var kw = ParseListKwArgs(args, 2, "ASSOC-IF");
        var cur2 = alist;
        for (; cur2 is Cons c; cur2 = c.Cdr)
        {
            if (c.Car is Nil) continue; // nil entries are allowed
            if (c.Car is not Cons pair)
                throw new LispErrorException(new LispTypeError("ASSOC-IF: alist entry is not a cons or nil", c.Car));
            var elem = ApplySeqKey(kw, pair.Car);
            if (IsTruthy(predFn.Invoke1(elem))) return pair;
        }
        if (cur2 is not Nil) throw new LispErrorException(new LispTypeError("ASSOC-IF: not a proper list", cur2));
        return Nil.Instance;
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

    private static LispObject RassocCore(LispObject item, LispObject alist, in ListKwArgs kw)
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
        var predFn = CoerceToFunction(args[0]);
        var alist = args[1];
        if (alist is not Nil && alist is not Cons)
            throw new LispErrorException(new LispTypeError("RASSOC-IF: not a proper list", alist));
        var kw = ParseListKwArgs(args, 2, "RASSOC-IF");
        var cur2 = alist;
        for (; cur2 is Cons c; cur2 = c.Cdr)
        {
            if (c.Car is Nil) continue;
            if (c.Car is not Cons pair)
                throw new LispErrorException(new LispTypeError("RASSOC-IF: alist entry is not a cons or nil", c.Car));
            var elem = ApplySeqKey(kw, pair.Cdr);
            if (IsTruthy(predFn.Invoke1(elem))) return pair;
        }
        if (cur2 is not Nil) throw new LispErrorException(new LispTypeError("RASSOC-IF: not a proper list", cur2));
        return Nil.Instance;
    }

    // REDUCE: (reduce function sequence &key key from-end start end initial-value)
    public static LispObject Reduce(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("REDUCE: too few arguments"));
        var fn = CoerceToFunction(args[0]);
        var seq = args[1];
        // Validate sequence type
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("REDUCE: not a sequence", seq));
        // Parse keywords
        LispFunction? keyFn = null;
        bool fromEnd = false, hasIV = false;
        LispObject iv = Nil.Instance;
        int? startOpt = null, endOpt = null;
        int kwCount = args.Length - 2;
        if (kwCount % 2 != 0)
            throw new LispErrorException(new LispProgramError("REDUCE: odd number of keyword arguments"));
        bool? allowOtherKeys = null;
        bool hasUnknown = false;
        bool keySet = false, fromEndSet = false, startSet = false, endSet = false, ivSet = false;
        for (int i = 2; i < args.Length - 1; i += 2)
            if (args[i] is Symbol kw0 && kw0.Name == "ALLOW-OTHER-KEYS" && allowOtherKeys == null)
                allowOtherKeys = IsTruthy(args[i + 1]);
        for (int i = 2; i < args.Length - 1; i += 2)
        {
            if (args[i] is not Symbol s)
                throw new LispErrorException(new LispProgramError($"REDUCE: keyword must be a symbol, got {args[i]}"));
            switch (s.Name)
            {
                case "KEY": if (!keySet) { if (args[i + 1] is not Nil) keyFn = CoerceToFunction(args[i + 1]); keySet = true; } break;
                case "FROM-END": if (!fromEndSet) { fromEnd = IsTruthy(args[i + 1]); fromEndSet = true; } break;
                case "START": if (!startSet) { startOpt = (int)((Fixnum)args[i + 1]).Value; startSet = true; } break;
                case "END": if (!endSet) { endOpt = args[i + 1] is Fixnum ef ? (int?)ef.Value : null; endSet = true; } break;
                case "INITIAL-VALUE": if (!ivSet) { iv = args[i + 1]; hasIV = true; ivSet = true; } break;
                case "ALLOW-OTHER-KEYS": break;
                default: hasUnknown = true; break;
            }
        }
        if (hasUnknown && allowOtherKeys != true)
            throw new LispErrorException(new LispProgramError("REDUCE: unknown keyword argument"));
        return ReduceCore(fn, seq, keyFn, fromEnd, hasIV, iv, startOpt, endOpt);
    }

    /// <summary>(REDUCE function sequence) as a direct entry: the two arguments arrive
    /// in registers instead of an array built for the variadic entry to walk.</summary>
    public static LispObject Reduce2(LispObject fn, LispObject seq)
    {
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("REDUCE: not a sequence", seq));
        return ReduceCore(CoerceToFunction(fn), seq, null, false, false, Nil.Instance, null, null);
    }

    private static LispObject ReduceCore(LispFunction fn, LispObject seq, LispFunction? keyFn,
                                         bool fromEnd, bool hasIV, LispObject iv,
                                         int? startOpt, int? endOpt)
    {
        // A forward fold over a list needs no materialisation: the elements are
        // already reachable in order. Collecting them into a List and then an
        // array copied the sequence twice before the first call to FN.
        if (!fromEnd && startOpt == null && endOpt == null && (seq is Cons || seq is Nil))
        {
            LispObject acc;
            var cur = seq;
            if (hasIV) acc = iv;
            else
            {
                if (cur is not Cons first)
                    return UnwrapMv(fn.Invoke(Array.Empty<LispObject>()));
                acc = ApplyKeyFn(keyFn, first.Car);
                cur = first.Cdr;
            }
            for (; cur is Cons c; cur = c.Cdr)
                acc = UnwrapMv(fn.Invoke2(acc, ApplyKeyFn(keyFn, c.Car)));
            return acc;
        }

        // Get elements as array for direct access
        int len;
        LispObject[] elems;
        if (seq is LispVector vec)
        {
            len = vec.Length;
            int start = startOpt ?? 0;
            int end = endOpt ?? len;
            CheckBoundingIndices(start, end, len, "REDUCE");
            int count = end - start;
            elems = new LispObject[count];
            for (int i = 0; i < count; i++)
                elems[i] = ApplyKeyFn(keyFn, vec[start + i]);
        }
        else if (seq is LispString str)
        {
            len = str.Length;
            int start = startOpt ?? 0;
            int end = endOpt ?? len;
            CheckBoundingIndices(start, end, len, "REDUCE");
            int count = end - start;
            elems = new LispObject[count];
            for (int i = 0; i < count; i++)
            {
                var ch = LispChar.Make(str[start + i]);
                elems[i] = ApplyKeyFn(keyFn, ch);
            }
        }
        else
        {
            // List: collect elements
            var list = new System.Collections.Generic.List<LispObject>();
            int idx = 0;
            int start = startOpt ?? 0;
            for (var cur = seq; cur is Cons c; cur = c.Cdr, idx++)
            {
                if (idx >= start) list.Add(ApplyKeyFn(keyFn, c.Car));
            }
            len = idx;
            int end = endOpt ?? len;
            // The walk above already skipped to START, so the range is validated here,
            // once the list's length is known.
            CheckBoundingIndices(start, end, len, "REDUCE");
            int count = end - start;
            if (count < list.Count) elems = list.GetRange(0, count).ToArray();
            else elems = list.ToArray();
        }

        if (elems.Length == 0)
        {
            if (hasIV) return iv;
            return UnwrapMv(fn.Invoke(Array.Empty<LispObject>()));
        }

        if (fromEnd)
        {
            var result = hasIV ? iv : elems[elems.Length - 1];
            int startIdx = hasIV ? elems.Length - 1 : elems.Length - 2;
            for (int i = startIdx; i >= 0; i--)
                result = UnwrapMv(fn.Invoke2(elems[i], result));
            return result;
        }
        else
        {
            var result = hasIV ? iv : elems[0];
            int startIdx = hasIV ? 0 : 1;
            for (int i = startIdx; i < elems.Length; i++)
                result = UnwrapMv(fn.Invoke2(result, elems[i]));
            return result;
        }
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

    /// <summary>(REMOVE item seq ...) / (DELETE item seq ...): the element matches
    /// when the :TEST (or :TEST-NOT) says it equals ITEM, after :KEY.</summary>
    private readonly struct ItemMatch : IElemMatch
    {
        private readonly LispObject _item;
        private readonly SeqKwArgs _kw;
        public ItemMatch(LispObject item, SeqKwArgs kw) { _item = item; _kw = kw; }
        public bool Match(LispObject elem) => SeqTestMatch(_item, elem, _kw);
    }

    /// <summary>(REMOVE-IF pred seq ...) and its NOT / DELETE variants: the element
    /// matches when the predicate says so, after :KEY.</summary>
    private readonly struct PredMatch : IElemMatch
    {
        private readonly LispFunction _pred;
        private readonly SeqKwArgs _kw;
        private readonly bool _negate;
        public PredMatch(LispFunction pred, SeqKwArgs kw, bool negate)
        { _pred = pred; _kw = kw; _negate = negate; }
        public bool Match(LispObject elem)
        {
            var t = IsTruthy(_pred.Invoke1(ApplySeqKey(_kw, elem)));
            return _negate ? !t : t;
        }
    }

    public static LispObject RemoveFull(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("REMOVE: too few arguments"));
        var item = args[0];
        var seq = args[1];
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("REMOVE: not a sequence", seq));
        var kw = ParseSeqKwArgs(args, 2, "REMOVE");
        return RemoveCore(seq, kw, new ItemMatch(item, kw));
    }

    // 2-arg direct entry: (remove item seq) — no keywords, default EQL test.
    // Skips the args array and the ParseSeqKwArgs scan (a default SeqKwArgs is
    // exactly its zero-keyword result). See RemoveIf2.
    public static LispObject Remove2(LispObject item, LispObject seq)
    {
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("REMOVE: not a sequence", seq));
        var kw = new SeqKwArgs();
        return RemoveCore(seq, kw, new ItemMatch(item, kw));
    }

    // 4-arg direct entry: (remove item seq k v) — one keyword pair, e.g.
    // (remove x list :test #'string=) or (remove x list :key #'car). This is by
    // far the most common REMOVE shape in the compiler itself. The shared keyword
    // parser runs over a 2-element array, so only the args array of the InvokeSlow
    // path is saved — same trade-off as Member4/Assoc4.
    public static LispObject Remove4(LispObject item, LispObject seq, LispObject k, LispObject v)
    {
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("REMOVE: not a sequence", seq));
        var kw = ParseSeqKwArgs(new[] { k, v }, 0, "REMOVE");
        return RemoveCore(seq, kw, new ItemMatch(item, kw));
    }

    // 6-arg direct entry: (remove item seq k1 v1 k2 v2) — two keyword pairs,
    // e.g. (remove x l :key #'car :test #'string=).
    public static LispObject Remove6(LispObject item, LispObject seq,
                                     LispObject k1, LispObject v1, LispObject k2, LispObject v2)
    {
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("REMOVE: not a sequence", seq));
        var kw = ParseSeqKwArgs(new[] { k1, v1, k2, v2 }, 0, "REMOVE");
        return RemoveCore(seq, kw, new ItemMatch(item, kw));
    }

    // REMOVE-IF: (remove-if predicate sequence &key key count from-end start end)
    public static LispObject RemoveIf(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("REMOVE-IF: too few arguments"));
        var predFn = CoerceToFunction(args[0]);
        var seq = args[1];
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("REMOVE-IF: not a sequence", seq));
        return RemoveIfCore(predFn, seq, ParseSeqKwArgs(args, 2, "REMOVE-IF"));
    }

    // 2-arg direct entry: (remove-if pred seq) — no keywords. The compiler calls
    // this shape frequently (bound-name filtering). Skips the args array and the
    // ParseSeqKwArgs scan; a default SeqKwArgs is exactly ParseSeqKwArgs's zero-
    // keyword result (all fields default).
    public static LispObject RemoveIf2(LispObject pred, LispObject seq)
    {
        var predFn = CoerceToFunction(pred);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("REMOVE-IF: not a sequence", seq));
        return RemoveIfCore(predFn, seq, new SeqKwArgs());
    }

    // 2-arg direct entry: (remove-if-not pred seq) — no keywords. Inverts the
    // predicate inline (negate: true) rather than allocating a negating wrapper
    // LispFunction the way the args-array registration does, so both the args
    // array and the per-element wrapper InvokeSlow are avoided. See RemoveIf2.
    public static LispObject RemoveIfNot2(LispObject pred, LispObject seq)
    {
        var predFn = CoerceToFunction(pred);
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("REMOVE-IF-NOT: not a sequence", seq));
        return RemoveIfCore(predFn, seq, new SeqKwArgs(), negate: true);
    }

    private static LispObject RemoveIfCore(LispFunction predFn, LispObject seq, SeqKwArgs kw, bool negate = false)
    {
        var result = RemoveCore(seq, kw, new PredMatch(predFn, kw, negate));
        // The last predicate call may have left secondary values in the MV register
        // (e.g. a predicate calling SUBTYPEP, which returns two values). REMOVE-IF
        // yields exactly one value, so install the result as the primary, clearing
        // the stale secondaries that would otherwise leak to our caller (ANSI NIL.1
        // via check-predicate's remove-if).
        return MultipleValues.Primary(result);
    }

    // DELETE: like REMOVE but destructive on list arguments (in-place splice).
    public static LispObject DeleteFull(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("DELETE: too few arguments"));
        var item = args[0];
        var seq = args[1];
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
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("DELETE: not a sequence", seq));
        var kw = new SeqKwArgs();
        return RemoveCore(seq, kw, new ItemMatch(item, kw), destructive: true);
    }

    // 4-arg direct entry: (delete item seq kw val) — one keyword pair (e.g.
    // :test #'eq). Pure addition (DeleteFull unchanged); shared parser over a
    // 2-element array. See Remove2 / Member4.
    public static LispObject Delete4(LispObject item, LispObject seq, LispObject k, LispObject v)
    {
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("DELETE: not a sequence", seq));
        var kw = ParseSeqKwArgs(new[] { k, v }, 0, "DELETE");
        return RemoveCore(seq, kw, new ItemMatch(item, kw), destructive: true);
    }

    // DELETE-IF: like REMOVE-IF but destructive on list arguments.
    public static LispObject DeleteIf(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("DELETE-IF: too few arguments"));
        var predFn = CoerceToFunction(args[0]);
        var seq = args[1];
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("DELETE-IF: not a sequence", seq));
        var kw = ParseSeqKwArgs(args, 2, "DELETE-IF");
        var result = RemoveCore(seq, kw, new PredMatch(predFn, kw, false), destructive: true);
        return MultipleValues.Primary(result);
    }

    // Core remove logic shared by REMOVE/REMOVE-IF (destructive=false) and
    // DELETE/DELETE-IF (destructive=true). When destructive and the sequence is a
    // list, matched conses are spliced out of the original chain in place (SBCL
    // semantics) so code that discards the return value and relies on in-place
    // mutation — e.g. Maxima rempropchk / mfunction-delete — works.
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
    {
        if (seq is Nil) return Nil.Instance;

        // Normalize count: null=no limit, <=0=remove nothing
        int? maxRemove = kw.Count;
        if (maxRemove.HasValue && maxRemove.Value <= 0)
        {
            // count <= 0: nothing removed. Return the original sequence (eq), as
            // SBCL/CCL and most impls do — many libraries (e.g. Maxima add2lnc)
            // rely on (setq x (delete .. x)) preserving x's cons when no element
            // matches, so a following (nconc x ..) still mutates the shared list.
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
        // List. Its length is only needed to validate the range — the walk itself
        // does not need it — so it is computed here rather than inside the walk.
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

            if (origSeq is LispString ostr)
            {
                var chars = new char[len - removed];
                int ci = 0;
                for (int i = 0; i < len; i++)
                    if ((drop[i >> 6] & (1UL << (i & 63))) == 0) chars[ci++] = ostr[i];
                return new LispString(new string(chars));
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

            if (!destructive && !fromEnd && !maxRem.HasValue && start == 0 && endOpt == null)
            {
                // Find the first element to drop before consing anything. When
                // nothing matches -- which is most calls -- the answer is the list
                // itself and this allocates nothing at all; the previous version
                // copied the whole list and then threw the copy away.
                var first = listSeq;
                while (first is Cons f && !matches.Match(f.Car)) first = f.Cdr;
                if (first is not Cons) return listSeq;

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
                    if (matches.Match(c0.Car)) continue;
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
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("SUBSTITUTE: not a sequence", seq));
        var kw = new SeqKwArgs();
        return SubstituteCore(newitem, seq, kw, new ItemMatch(olditem, kw));
    }

    public static LispObject SubstituteFull(LispObject[] args)
    {
        if (args.Length < 3)
            throw new LispErrorException(new LispProgramError("SUBSTITUTE: too few arguments"));
        var newitem = args[0];
        var olditem = args[1];
        var seq = args[2];
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
        var predFn = CoerceToFunction(args[1]);
        var seq = args[2];
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("SUBSTITUTE-IF: not a sequence", seq));
        var kw = ParseSeqKwArgs(args, 3, "SUBSTITUTE-IF");
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
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("NSUBSTITUTE: not a sequence", seq));
        var kw = ParseSeqKwArgs(args, 3, "NSUBSTITUTE");
        return NsubstituteCore(newitem, seq, kw, new ItemMatch(olditem, kw));
    }

    // NSUBSTITUTE-IF: (nsubstitute-if newitem predicate sequence &key key count from-end start end)
    public static LispObject NsubstituteIf(LispObject[] args)
    {
        if (args.Length < 3)
            throw new LispErrorException(new LispProgramError("NSUBSTITUTE-IF: too few arguments"));
        var newitem = args[0];
        var predFn = CoerceToFunction(args[1]);
        var seq = args[2];
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("NSUBSTITUTE-IF: not a sequence", seq));
        var kw = ParseSeqKwArgs(args, 3, "NSUBSTITUTE-IF");
        return NsubstituteCore(newitem, seq, kw, new PredMatch(predFn, kw, false));
    }

    // Core substitute logic (non-destructive)
    private static LispObject SubstituteCore<TMatch>(LispObject newitem, LispObject seq,
                                                     SeqKwArgs kw, TMatch matches)
        where TMatch : struct, IElemMatch
    {
        if (seq is Nil) return Nil.Instance;

        int? maxSub = kw.Count;
        if (maxSub.HasValue && maxSub.Value <= 0)
            return CopySeq(seq);

        // The ordinary shape -- a list, whole range, no :COUNT, no :FROM-END --
        // walks once and conses the answer. The general path below materialises
        // every element into a List, builds a second List of the result, and
        // hands that to the coercion, which is three intermediates for a list it
        // could write directly.
        if (seq is Cons && !kw.FromEnd && !maxSub.HasValue && kw.Start == 0 && kw.End == null)
        {
            Cons? subHead = null, subTail = null;
            for (var cur = seq; cur is Cons c; cur = c.Cdr)
            {
                var cell = new Cons(matches.Match(c.Car) ? newitem : c.Car, Nil.Instance);
                if (subTail == null) subHead = cell; else subTail.Cdr = cell;
                subTail = cell;
            }
            return subHead ?? (LispObject)Nil.Instance;
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
            for (var cur = seq; cur is Cons c; cur = c.Cdr) allElems.Add(c.Car);
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
    {
        if (seq is Nil) return Nil.Instance;

        int? maxSub = kw.Count;
        if (maxSub.HasValue && maxSub.Value <= 0)
            return seq;

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
            // Strings are immutable in our implementation, fall back to substitute
            return SubstituteCore(newitem, seq, kw, matches);
        }
        // List: modify in place via rplaca
        {
            int start = kw.Start;
            // Collect cells in range
            var cells = new System.Collections.Generic.List<Cons>();
            var cur = seq;
            for (int i = 0; cur is Cons c; cur = c.Cdr, i++)
            {
                int end = kw.End ?? int.MaxValue;
                if (i >= end) break;
                if (i >= start) cells.Add(c);
            }

            if (kw.FromEnd)
            {
                int subbed = 0;
                for (int i = cells.Count - 1; i >= 0; i--)
                {
                    if (maxSub.HasValue && subbed >= maxSub.Value) break;
                    if (matches.Match(cells[i].Car)) { cells[i].Car = newitem; subbed++; }
                }
            }
            else
            {
                int subbed = 0;
                for (int i = 0; i < cells.Count; i++)
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

    private static LispObject RemoveDuplicatesCore(LispObject seq, in SeqKwArgs kw)
    {
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("REMOVE-DUPLICATES: not a sequence", seq));
        if (seq is Nil) return Nil.Instance;

        // A list is walked into one exact-size array, because the scan below indexes
        // arbitrarily and re-walking the list per index would make an already
        // quadratic algorithm cubic. A vector or string is indexed where it stands.
        // The previous version grew a List of every element, a bool[] of every
        // index, and a second List of the survivors, then copied that out again.
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
                    if (SeqTestMatch2(ki, ApplySeqKey(kw, At(j)), kw))
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
            var chars = new char[len - removed];
            int k = 0;
            for (int i = 0; i < len; i++)
                if ((dup[i >> 6] & (1UL << (i & 63))) == 0) chars[k++] = ostr[i];
            return new LispString(new string(chars));
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
        return EveryMulti(CoerceToFunction(args[0]), args);
    }

    // 2-arg direct entry: single-sequence fast path, extracted verbatim.
    public static LispObject Every2(LispObject pred, LispObject seq)
    {
        var predFn = CoerceToFunction(pred);
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
        // Validate and collect sequence info
        var seqs = new LispObject[nseqs];
        var cursors = new LispObject?[nseqs]; // for lists
        var indices = new int[nseqs];
        var lengths = new int[nseqs];
        var isList = new bool[nseqs];
        for (int s = 0; s < nseqs; s++)
        {
            seqs[s] = args[s + 1];
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

    // SOME: (some predicate &rest sequences)
    public static LispObject Some(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("SOME: too few arguments"));
        if (args.Length == 2) return Some2(args[0], args[1]);
        // Multiple sequences
        return SomeMulti(CoerceToFunction(args[0]), args);
    }

    // 2-arg direct entry: single-sequence fast path, extracted verbatim.
    public static LispObject Some2(LispObject pred, LispObject seq)
    {
        var predFn = CoerceToFunction(pred);
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
        var seqs = new LispObject[nseqs];
        var cursors = new LispObject?[nseqs];
        var indices = new int[nseqs];
        var lengths = new int[nseqs];
        var isList = new bool[nseqs];
        for (int s = 0; s < nseqs; s++)
        {
            seqs[s] = args[s + 1];
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
    public static LispObject MismatchFull(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("MISMATCH: too few arguments"));
        var seq1 = args[0];
        var seq2 = args[1];

        // Parse keywords manually (needs start1/end1/start2/end2 instead of start/end)
        LispFunction? testFn = null, testNotFn = null, keyFn = null;
        int s1 = 0, s2 = 0;
        int? e1opt = null, e2opt = null;
        bool fromEnd = false;
        int kwCount = args.Length - 2;
        if (kwCount % 2 != 0)
            throw new LispErrorException(new LispProgramError("MISMATCH: odd number of keyword arguments"));
        bool? allowOtherKeys = null;
        bool hasUnknown = false;
        bool testSet = false, testNotSet = false, keySet = false;
        bool s1Set = false, e1Set = false, s2Set = false, e2Set = false, feSet = false;
        for (int i = 2; i < args.Length - 1; i += 2)
            if (args[i] is Symbol kw0 && kw0.Name == "ALLOW-OTHER-KEYS" && allowOtherKeys == null)
                allowOtherKeys = IsTruthy(args[i + 1]);
        for (int i = 2; i < args.Length - 1; i += 2)
        {
            if (args[i] is not Symbol s)
                throw new LispErrorException(new LispProgramError($"MISMATCH: keyword must be a symbol, got {args[i]}"));
            switch (s.Name)
            {
                case "TEST": if (!testSet) { testFn = CoerceToFunction(args[i + 1]); testSet = true; } break;
                case "TEST-NOT": if (!testNotSet) { testNotFn = CoerceToFunction(args[i + 1]); testNotSet = true; } break;
                case "KEY": if (!keySet) { if (args[i + 1] is not Nil) keyFn = CoerceToFunction(args[i + 1]); keySet = true; } break;
                case "START1": if (!s1Set) { s1 = (int)((Fixnum)args[i + 1]).Value; s1Set = true; } break;
                case "END1": if (!e1Set) { e1opt = args[i + 1] is Fixnum ef ? (int?)ef.Value : null; e1Set = true; } break;
                case "START2": if (!s2Set) { s2 = (int)((Fixnum)args[i + 1]).Value; s2Set = true; } break;
                case "END2": if (!e2Set) { e2opt = args[i + 1] is Fixnum ef2 ? (int?)ef2.Value : null; e2Set = true; } break;
                case "FROM-END": if (!feSet) { fromEnd = IsTruthy(args[i + 1]); feSet = true; } break;
                case "ALLOW-OTHER-KEYS": break;
                default: hasUnknown = true; break;
            }
        }
        if (hasUnknown && allowOtherKeys != true)
            throw new LispErrorException(new LispProgramError("MISMATCH: unknown keyword argument"));

        // Collect elements from both sequences
        var elems1 = CollectSeqElements(seq1, "MISMATCH");
        var elems2 = CollectSeqElements(seq2, "MISMATCH");
        int len1 = elems1.Length;
        int len2 = elems2.Length;
        int e1 = e1opt ?? len1;
        int e2 = e2opt ?? len2;
        int count1 = e1 - s1;
        int count2 = e2 - s2;

        if (fromEnd)
        {
            for (int i = 1; i <= count1 && i <= count2; i++)
            {
                var x1 = elems1[e1 - i];
                var x2 = elems2[e2 - i];
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
            for (int i = 0; i < count1 && i < count2; i++)
            {
                var x1 = elems1[s1 + i];
                var x2 = elems2[s2 + i];
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

    private static void ApplySearchKw(LispObject k, LispObject v, ref SearchKwState st)
    {
        if (k is not Symbol kw)
            throw new LispErrorException(new LispProgramError(
                $"SEARCH: keyword must be a symbol, got {k}"));
        switch (kw.Name)
        {
            case "FROM-END": if (!st.FeSet) { st.FromEnd = IsTruthy(v); st.FeSet = true; } break;
            case "TEST": if (!st.TestSet) { st.TestFn = CoerceToFunction(v); st.TestSet = true; } break;
            case "TEST-NOT": if (!st.TestNotSet) { st.TestNotFn = CoerceToFunction(v); st.TestNotSet = true; } break;
            case "KEY": if (!st.KeySet) { st.KeyFn = v is Nil ? null : CoerceToFunction(v); st.KeySet = true; } break;
            case "START1": if (!st.S1Set) { st.Start1 = (int)((Fixnum)v).Value; st.S1Set = true; } break;
            case "END1": if (!st.E1Set && v is Fixnum f1) { st.End1 = (int)f1.Value; st.E1Set = true; } break;
            case "START2": if (!st.S2Set) { st.Start2 = (int)((Fixnum)v).Value; st.S2Set = true; } break;
            case "END2": if (!st.E2Set && v is Fixnum f2) { st.End2 = (int)f2.Value; st.E2Set = true; } break;
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
                case "TEST": if (!testSet) { testFn = CoerceToFunction(args[i + 1]); testSet = true; } break;
                case "TEST-NOT": if (!testNotSet) { testNotFn = CoerceToFunction(args[i + 1]); testNotSet = true; } break;
                case "KEY": if (!keySet) { keyFn = args[i + 1] is Nil ? null : CoerceToFunction(args[i + 1]); keySet = true; } break;
                case "START1": if (!s1Set) { start1 = (int)((Fixnum)args[i + 1]).Value; s1Set = true; } break;
                case "END1": if (!e1Set && args[i + 1] is Fixnum f1) { end1 = (int)f1.Value; e1Set = true; } break;
                case "START2": if (!s2Set) { start2 = (int)((Fixnum)args[i + 1]).Value; s2Set = true; } break;
                case "END2": if (!e2Set && args[i + 1] is Fixnum f2) { end2 = (int)f2.Value; e2Set = true; } break;
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
        int len1 = ReplaceSeqLength(seq1), len2 = ReplaceSeqLength(seq2);
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
            var chars1 = searchStr1.RawChars;
            var chars2 = searchStr2.RawChars;
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
        // CLHS: STRING of a symbol is its name — the same string SYMBOL-NAME
        // answers with, so share it (SBCL does too).
        if (obj is Symbol sym) return sym.NameString;
        if (obj is Nil || obj is T) return SymbolName(obj);
        if (obj is LispChar ch) return new LispString(ch.Value.ToString());
        throw new LispErrorException(new LispTypeError("STRING: cannot convert to string", obj));
    }

    internal static void RegisterSequenceBuiltins()
    {
        // COUNT, COUNT-IF, COUNT-IF-NOT
        // COUNT and COUNT-IF: a two-argument direct entry, so (count x l) reaches
        // the body without building an args array for the variadic entry to walk.
        var countFn = new LispFunction(args => Runtime.Count(args));
        countFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Count2);
        Emitter.CilAssembler.RegisterFunction("COUNT", countFn);
        var countIfFn = new LispFunction(args => Runtime.CountIf(args));
        countIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.CountIf2);
        Emitter.CilAssembler.RegisterFunction("COUNT-IF", countIfFn);
        Emitter.CilAssembler.RegisterFunction("COUNT-IF-NOT",
            new LispFunction(args =>
            {
                Runtime.CheckArityMin("COUNT-IF-NOT", args, 2);
                var predFn = Runtime.CoerceToFunction(args[0]);
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[0] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.CountIf(newArgs);
            }));
        // FILL
        Emitter.CilAssembler.RegisterFunction("FILL",
            new LispFunction(args => Runtime.Fill(args)));
        // FIND, FIND-IF, FIND-IF-NOT
        var findFn = new LispFunction(args => Runtime.Find(args));
        findFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Find2);
        findFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Find6);
        Emitter.CilAssembler.RegisterFunction("FIND", findFn);
        var findIfFn = new LispFunction(args => Runtime.FindIf(args));
        findIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.FindIf2);
        Emitter.CilAssembler.RegisterFunction("FIND-IF", findIfFn);
        Emitter.CilAssembler.RegisterFunction("FIND-IF-NOT",
            new LispFunction(args =>
            {
                Runtime.CheckArityMin("FIND-IF-NOT", args, 2);
                var predFn = Runtime.CoerceToFunction(args[0]);
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[0] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.FindIf(newArgs);
            }));
        // POSITION, POSITION-IF, POSITION-IF-NOT
        var positionFn = new LispFunction(args => Runtime.Position(args));
        positionFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Position2);
        Emitter.CilAssembler.RegisterFunction("POSITION", positionFn);
        var positionIfFn = new LispFunction(args => Runtime.PositionIf(args));
        positionIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.PositionIf2);
        Emitter.CilAssembler.RegisterFunction("POSITION-IF", positionIfFn);
        Emitter.CilAssembler.RegisterFunction("POSITION-IF-NOT",
            new LispFunction(args =>
            {
                Runtime.CheckArityMin("POSITION-IF-NOT", args, 2);
                var predFn = Runtime.CoerceToFunction(args[0]);
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[0] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.PositionIf(newArgs);
            }));
        // REDUCE
        var reduceFn = new LispFunction(args => Runtime.Reduce(args));
        reduceFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Reduce2);
        Emitter.CilAssembler.RegisterFunction("REDUCE", reduceFn);
        // MEMBER, MEMBER-IF, MEMBER-IF-NOT
        // MEMBER/ASSOC: attach direct entries for the dominant call shapes
        // ((item list) and (item list kw val) — e.g. :test #'string=). The
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
        Emitter.CilAssembler.RegisterFunction("MEMBER-IF",
            new LispFunction(args => Runtime.MemberIf(args)));
        Emitter.CilAssembler.RegisterFunction("MEMBER-IF-NOT",
            new LispFunction(args =>
            {
                Runtime.CheckArityMin("MEMBER-IF-NOT", args, 2);
                var predFn = Runtime.CoerceToFunction(args[0]);
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[0] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.MemberIf(newArgs);
            }));
        // ASSOC, ASSOC-IF, ASSOC-IF-NOT
        var assocFn = new LispFunction(args => Runtime.AssocFull(args));
        assocFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Assoc2);
        assocFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Assoc4);
        assocFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Assoc6);
        Emitter.CilAssembler.RegisterFunction("ASSOC", assocFn);
        Emitter.CilAssembler.RegisterFunction("ASSOC-IF",
            new LispFunction(args => Runtime.AssocIf(args)));
        Emitter.CilAssembler.RegisterFunction("ASSOC-IF-NOT",
            new LispFunction(args =>
            {
                Runtime.CheckArityMin("ASSOC-IF-NOT", args, 2);
                var predFn = Runtime.CoerceToFunction(args[0]);
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[0] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.AssocIf(newArgs);
            }));
        // RASSOC, RASSOC-IF, RASSOC-IF-NOT
        var rassocFn = new LispFunction(args => Runtime.RassocFull(args));
        rassocFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Rassoc2);
        rassocFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Rassoc4);
        Emitter.CilAssembler.RegisterFunction("RASSOC", rassocFn);
        Emitter.CilAssembler.RegisterFunction("RASSOC-IF",
            new LispFunction(args => Runtime.RassocIf(args)));
        Emitter.CilAssembler.RegisterFunction("RASSOC-IF-NOT",
            new LispFunction(args =>
            {
                Runtime.CheckArityMin("RASSOC-IF-NOT", args, 2);
                var predFn = Runtime.CoerceToFunction(args[0]);
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[0] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.RassocIf(newArgs);
            }));
        // REMOVE, REMOVE-IF, REMOVE-IF-NOT
        var removeFn = new LispFunction(args => Runtime.RemoveFull(args));
        removeFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Remove2);
        removeFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Remove4);
        removeFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Remove6);
        Emitter.CilAssembler.RegisterFunction("REMOVE", removeFn);
        var removeIfFn = new LispFunction(args => Runtime.RemoveIf(args));
        removeIfFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.RemoveIf2);
        Emitter.CilAssembler.RegisterFunction("REMOVE-IF", removeIfFn);
        var removeIfNotFn = new LispFunction(args =>
            {
                var predFn = Runtime.CoerceToFunction(args[0]);
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[0] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.RemoveIf(newArgs);
            });
        removeIfNotFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.RemoveIfNot2);
        Emitter.CilAssembler.RegisterFunction("REMOVE-IF-NOT", removeIfNotFn);
        // DELETE, DELETE-IF, DELETE-IF-NOT — destructive on list args.
        var deleteFn = new LispFunction(args => Runtime.DeleteFull(args));
        deleteFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Delete2);
        deleteFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)Runtime.Delete4);
        Emitter.CilAssembler.RegisterFunction("DELETE", deleteFn);
        Emitter.CilAssembler.RegisterFunction("DELETE-IF",
            new LispFunction(args => Runtime.DeleteIf(args)));
        Emitter.CilAssembler.RegisterFunction("DELETE-IF-NOT",
            new LispFunction(args =>
            {
                var predFn = Runtime.CoerceToFunction(args[0]);
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[0] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.DeleteIf(newArgs);
            }));
        // SUBSTITUTE, SUBSTITUTE-IF, SUBSTITUTE-IF-NOT
        var substituteFn = new LispFunction(args => Runtime.SubstituteFull(args));
        substituteFn.SetDirectDelegate(
            (Func<LispObject, LispObject, LispObject, LispObject>)Runtime.Substitute3);
        Emitter.CilAssembler.RegisterFunction("SUBSTITUTE", substituteFn);
        Emitter.CilAssembler.RegisterFunction("SUBSTITUTE-IF",
            new LispFunction(args => Runtime.SubstituteIf(args)));
        Emitter.CilAssembler.RegisterFunction("SUBSTITUTE-IF-NOT",
            new LispFunction(args =>
            {
                Runtime.CheckArityMin("SUBSTITUTE-IF-NOT", args, 3);
                var predFn = Runtime.CoerceToFunction(args[1]);
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[1] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.SubstituteIf(newArgs);
            }));
        // NSUBSTITUTE, NSUBSTITUTE-IF, NSUBSTITUTE-IF-NOT
        Emitter.CilAssembler.RegisterFunction("NSUBSTITUTE",
            new LispFunction(args => Runtime.NsubstituteFull(args)));
        Emitter.CilAssembler.RegisterFunction("NSUBSTITUTE-IF",
            new LispFunction(args => Runtime.NsubstituteIf(args)));
        Emitter.CilAssembler.RegisterFunction("NSUBSTITUTE-IF-NOT",
            new LispFunction(args =>
            {
                Runtime.CheckArityMin("NSUBSTITUTE-IF-NOT", args, 3);
                var predFn = Runtime.CoerceToFunction(args[1]);
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[1] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.NsubstituteIf(newArgs);
            }));
        // EVERY, SOME, NOTEVERY, NOTANY
        var everyFn = new LispFunction(args => Runtime.Every(args));
        everyFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Every2);
        Emitter.CilAssembler.RegisterFunction("EVERY", everyFn);
        var someFn = new LispFunction(args => Runtime.Some(args));
        someFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)Runtime.Some2);
        Emitter.CilAssembler.RegisterFunction("SOME", someFn);
        Emitter.CilAssembler.RegisterFunction("NOTEVERY",
            new LispFunction(args => Runtime.IsTruthy(Runtime.Every(args)) ? Nil.Instance : T.Instance));
        Emitter.CilAssembler.RegisterFunction("NOTANY",
            new LispFunction(args => Runtime.IsTruthy(Runtime.Some(args)) ? Nil.Instance : T.Instance));
        // MISMATCH, REMOVE-DUPLICATES, DELETE-DUPLICATES, REPLACE
        Emitter.CilAssembler.RegisterFunction("MISMATCH",
            new LispFunction(args => Runtime.MismatchFull(args)));
        var removeDupFn = new LispFunction(args => Runtime.RemoveDuplicatesFull(args));
        removeDupFn.SetDirectDelegate((Func<LispObject, LispObject>)Runtime.RemoveDuplicates1);
        Emitter.CilAssembler.RegisterFunction("REMOVE-DUPLICATES", removeDupFn);
        var deleteDupFn = new LispFunction(args => Runtime.RemoveDuplicatesFull(args));
        deleteDupFn.SetDirectDelegate((Func<LispObject, LispObject>)Runtime.RemoveDuplicates1);
        Emitter.CilAssembler.RegisterFunction("DELETE-DUPLICATES", deleteDupFn);
        Emitter.CilAssembler.RegisterFunction("REPLACE",
            new LispFunction(args => { Runtime.CheckArityMin("REPLACE", args, 2); return Runtime.Replace(args); }));
        // MAKE-STRING
        Emitter.CilAssembler.RegisterFunction("MAKE-STRING",
            new LispFunction(args =>
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
                        case "ELEMENT-TYPE": break; // ignored
                        case "ALLOW-OTHER-KEYS": if (allowOtherKeys == null) allowOtherKeys = Runtime.IsTruthy(val); break;
                        default: hasUnknown = true; break;
                    }
                }
                if (hasUnknown && allowOtherKeys != true)
                    throw new LispErrorException(new LispProgramError("MAKE-STRING: unknown keyword argument"));
                return Runtime.MakeString(size, initChar);
            }));

        // String comparison functions. Each gets a 2-arg direct delegate
        // (the dominant no-keyword call shape; bypasses the args-array
        // InvokeSlow) alongside the full variadic keyword-parsing entry.
        static void RegisterStringCmp(string name,
            Func<LispObject[], LispObject> variadic,
            Func<LispObject, LispObject, LispObject> twoArg,
            Func<LispObject, LispObject, LispObject, LispObject, LispObject> fourArg)
        {
            var fn = new LispFunction(variadic, name, -1);
            fn.SetDirectDelegate(twoArg);
            // 4 args = one keyword pair, e.g. the (string= name prefix :end1 n) that
            // prefix checks are written with — the dominant keyworded shape.
            fn.SetDirectDelegate(fourArg);
            Emitter.CilAssembler.RegisterFunction(name, fn);
        }
        RegisterStringCmp("STRING=",  Runtime.StringEq,    Runtime.StringEq2,    Runtime.StringEq4);
        RegisterStringCmp("STRING<",  Runtime.StringLt,    Runtime.StringLt2,    Runtime.StringLt4);
        RegisterStringCmp("STRING>",  Runtime.StringGt,    Runtime.StringGt2,    Runtime.StringGt4);
        RegisterStringCmp("STRING<=", Runtime.StringLe,    Runtime.StringLe2,    Runtime.StringLe4);
        RegisterStringCmp("STRING>=", Runtime.StringGe,    Runtime.StringGe2,    Runtime.StringGe4);
        RegisterStringCmp("STRING/=", Runtime.StringNotEq, Runtime.StringNotEq2, Runtime.StringNotEq4);
        RegisterStringCmp("STRING-EQUAL",        Runtime.StringEqualFn,     Runtime.StringEqual2,
                          Runtime.StringEqual4);
        RegisterStringCmp("STRING-NOT-EQUAL",    Runtime.StringNotEqualFn,  Runtime.StringNotEqual2,
                          Runtime.StringNotEqual4);
        RegisterStringCmp("STRING-LESSP",        Runtime.StringLessp,       Runtime.StringLessp2,
                          Runtime.StringLessp4);
        RegisterStringCmp("STRING-GREATERP",     Runtime.StringGreaterp,    Runtime.StringGreaterp2,
                          Runtime.StringGreaterp4);
        RegisterStringCmp("STRING-NOT-GREATERP", Runtime.StringNotGreaterp, Runtime.StringNotGreaterp2,
                          Runtime.StringNotGreaterp4);
        RegisterStringCmp("STRING-NOT-LESSP",    Runtime.StringNotLessp,    Runtime.StringNotLessp2,
                          Runtime.StringNotLessp4);
        // STRING-UPCASE/DOWNCASE/CAPITALIZE
        Emitter.CilAssembler.RegisterFunction("STRING-UPCASE",
            new LispFunction(Runtime.StringUpcase, "STRING-UPCASE", -1));
        Emitter.CilAssembler.RegisterFunction("STRING-DOWNCASE",
            new LispFunction(Runtime.StringDowncase, "STRING-DOWNCASE", -1));
        Emitter.CilAssembler.RegisterFunction("STRING-CAPITALIZE",
            new LispFunction(Runtime.StringCapitalize, "STRING-CAPITALIZE", -1));
        // NSTRING-* destructive in-place operations
        Emitter.CilAssembler.RegisterFunction("NSTRING-UPCASE",
            new LispFunction(Runtime.NStringUpcase, "NSTRING-UPCASE", -1));
        Emitter.CilAssembler.RegisterFunction("NSTRING-DOWNCASE",
            new LispFunction(Runtime.NStringDowncase, "NSTRING-DOWNCASE", -1));
        Emitter.CilAssembler.RegisterFunction("NSTRING-CAPITALIZE",
            new LispFunction(Runtime.NStringCapitalize, "NSTRING-CAPITALIZE", -1));

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
