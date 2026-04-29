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

    public static LispObject Subseq(LispObject seq, LispObject start, LispObject end)
    {
        int s = (start is Fixnum fs) ? (int)fs.Value : throw new LispErrorException(new LispTypeError("SUBSEQ: start must be integer", start));
        int? e = end is Nil ? null : (end is Fixnum fe ? (int?)fe.Value : throw new LispErrorException(new LispTypeError("SUBSEQ: end must be integer or nil", end)));

        if (seq is LispString str)
        {
            int endIdx = e ?? str.Length;
            return new LispString(str.Value.Substring(s, endIdx - s));
        }
        if (seq is LispVector vec)
        {
            int endIdx = e ?? vec.Length;
            var items = new LispObject[endIdx - s];
            for (int i = s; i < endIdx; i++) items[i - s] = vec.GetElement(i);
            return new LispVector(items, vec.ElementTypeName);
        }
        if (seq is Cons || seq is Nil)
        {
            // List subseq
            LispObject cur = seq;
            for (int i = 0; i < s; i++)
            {
                if (cur is Cons c) cur = c.Cdr;
                else break;
            }
            int count = (e ?? ListLength(seq)) - s;
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
            var items = new List<LispObject>();
            foreach (var seq in sequences) CollectSequenceElements(seq, items);
            return List(items.ToArray());
        }
        if (typeName == "NULL")
        {
            // NULL concatenation: all sequences must be empty, result is nil
            foreach (var seq in sequences)
                if (!(seq is Nil) && !(seq is LispVector ev && ev.Length == 0) && !(seq is LispString es && es.Length == 0))
                    throw new LispErrorException(new LispTypeError("CONCATENATE: cannot coerce non-empty sequence to NULL", seq));
            return Nil.Instance;
        }
        if (typeName == "VECTOR" || typeName == "SIMPLE-VECTOR" || typeName == "ARRAY")
        {
            var items = new List<LispObject>();
            foreach (var seq in sequences) CollectSequenceElements(seq, items);
            // Check compound size constraint: (vector * N) where N is the required length
            if (resultType is Cons vc && vc.Cdr is Cons vc2 && vc2.Cdr is Cons vc3 && vc3.Car is Fixnum sizeF)
                if (items.Count != (int)sizeF.Value)
                    throw new LispErrorException(new LispTypeError($"CONCATENATE: result has {items.Count} elements, type requires {sizeF.Value}", resultType));
            return new LispVector(items.ToArray(), "T");
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

    public static LispObject SortFull(LispObject[] args)
    {
        // (sort seq predicate &key key)
        var seq = args[0];
        var predicate = args[1];
        int keyArgCount = args.Length - 2;
        if (keyArgCount % 2 != 0)
            throw new LispErrorException(new LispProgramError("SORT: odd number of keyword arguments"));
        bool allowOtherKeys = false;
        for (int i = 2; i < args.Length - 1; i += 2)
        {
            if (args[i] is not Symbol sk)
                throw new LispErrorException(new LispProgramError($"SORT: keyword must be a symbol, got {args[i]}"));
            if (sk.Name == "ALLOW-OTHER-KEYS" && args[i + 1] != Nil.Instance)
                allowOtherKeys = true;
        }
        LispFunction? keyFn = null;
        for (int i = 2; i < args.Length - 1; i += 2)
        {
            if (args[i] is not Symbol ks)
                throw new LispErrorException(new LispProgramError($"SORT: keyword must be a symbol, got {args[i]}"));
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
                        throw new LispErrorException(new LispProgramError($"SORT: unknown keyword :{ks.Name}"));
                    break;
            }
        }
        return SortImpl(seq, predicate, keyFn);
    }

    private static int SortCompare(LispFunction fn, LispFunction? keyFn, LispObject a, LispObject b)
    {
        var ka = keyFn != null ? keyFn.Invoke1(a) : a;
        var kb = keyFn != null ? keyFn.Invoke1(b) : b;
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

    private static LispObject SortImpl(LispObject seq, LispObject predicate, LispFunction? keyFn)
    {
        // Accept symbol as function designator (ANSI CL: function designator can be symbol or function)
        if (predicate is Symbol psym && psym.Function is LispFunction pf)
            predicate = pf;
        if (predicate is not LispFunction fn)
            throw new LispErrorException(new LispTypeError("SORT: predicate must be a function", predicate));
        if (seq is Nil) return Nil.Instance;
        if (seq is Cons)
        {
            var items = new List<LispObject>();
            var cur = seq;
            while (cur is Cons c) { items.Add(c.Car); cur = c.Cdr; }
            // .NET wraps comparator exceptions in InvalidOperationException or ArgumentException.
            // Unwrap and rethrow Lisp errors/control exceptions.
            try { items.Sort((a, b) => SortCompare(fn, keyFn, a, b)); }
            catch (InvalidOperationException ioe) { UnwrapSortException(ioe); }
            catch (ArgumentException ae) { UnwrapSortException(ae); }
            return List(items.ToArray());
        }
        if (seq is LispString str)
        {
            var chars = str.Value.ToCharArray();
            var charObjs = new LispObject[chars.Length];
            for (int i = 0; i < chars.Length; i++) charObjs[i] = LispChar.Make(chars[i]);
            try { Array.Sort(charObjs, (a, b) => SortCompare(fn, keyFn, a, b)); }
            catch (InvalidOperationException ioe) { UnwrapSortException(ioe); }
            catch (ArgumentException ae) { UnwrapSortException(ae); }
            var sb = new System.Text.StringBuilder(charObjs.Length);
            foreach (var o in charObjs) if (o is LispChar lc) sb.Append(lc.Value);
            for (int i = 0; i < sb.Length; i++) str[i] = sb[i];
            return str;
        }
        if (seq is LispVector vec)
        {
            var items = new LispObject[vec.Length];
            for (int i = 0; i < vec.Length; i++) items[i] = vec.ElementAt(i);
            try { Array.Sort(items, (a, b) => SortCompare(fn, keyFn, a, b)); }
            catch (InvalidOperationException ioe) { UnwrapSortException(ioe); }
            catch (ArgumentException ae) { UnwrapSortException(ae); }
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
                // Extract size from (vector * n) or (array * (n)) — 3rd arg for vectors
                var rest1 = compType.Cdr as Cons;
                var rest2 = rest1?.Cdr as Cons;
                var sizeSpec = rest2?.Car;  // could be a Fixnum for vectors or a list for arrays
                // Coerce to vector type
                var result = Coerce(obj, Startup.Sym(head is "SIMPLE-ARRAY" or "ARRAY" ? "VECTOR" : "VECTOR"));
                // Check size constraint if specified (for 1D vectors)
                if (sizeSpec is Fixnum sizeFix)
                {
                    int expectedLen = (int)sizeFix.Value;
                    int actualLen = result is LispVector rv ? rv.Length : (result is LispString rs ? rs.Length : 0);
                    if (actualLen != expectedLen)
                        throw new LispErrorException(new LispTypeError($"COERCE: result length {actualLen} does not match required length {expectedLen}", obj));
                }
                return result;
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
                if (partType is Symbol pts)
                {
                    string ptn = pts.Name;
                    if (ptn is "SINGLE-FLOAT" or "SHORT-FLOAT")
                    {
                        real = new SingleFloat((float)Arithmetic.ToDouble(real));
                        imag = new SingleFloat((float)Arithmetic.ToDouble(imag));
                    }
                    else if (ptn is "DOUBLE-FLOAT" or "LONG-FLOAT")
                    {
                        real = new DoubleFloat(Arithmetic.ToDouble(real));
                        imag = new DoubleFloat(Arithmetic.ToDouble(imag));
                    }
                }
                return new LispComplex(real, imag);
            }
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
                if (obj is LispString s)
                {
                    var items = new LispObject[s.Length];
                    for (int i = 0; i < s.Length; i++)
                        items[i] = LispChar.Make(s[i]);
                    return List(items);
                }
                if (obj is LispVector lv)
                {
                    var items2 = new LispObject[lv.Length];
                    for (int i3 = 0; i3 < lv.Length; i3++) items2[i3] = lv.ElementAt(i3);
                    return List(items2);
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
                    // char-vector already satisfies 'string — return as-is
                    if (vec.IsCharVector) return obj;
                    var sb = new System.Text.StringBuilder();
                    for (int i = 0; i < vec.Length; i++)
                    {
                        if (vec[i] is LispChar lch) sb.Append(lch.Value);
                        else throw new LispErrorException(new LispTypeError("COERCE: vector element not a character", vec[i]));
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

            case "VECTOR":
                // Any LispVector or LispString satisfies VECTOR — return as-is
                if (obj is LispVector || obj is LispString) return obj;
                if (obj is Nil) return new LispVector(Array.Empty<LispObject>());
                if (obj is Cons)
                {
                    var vitems = new System.Collections.Generic.List<LispObject>();
                    LispObject vcur = obj;
                    while (vcur is Cons vc) { vitems.Add(vc.Car); vcur = vc.Cdr; }
                    return new LispVector(vitems.ToArray());
                }
                throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to vector", obj));

            case "SIMPLE-VECTOR":
                // Already a T-element-type vector (simple-vector)? return as-is
                if (obj is LispVector sv2 && (sv2.ElementTypeName == null || sv2.ElementTypeName == "T"))
                    return obj;
                if (obj is Nil) return new LispVector(Array.Empty<LispObject>());
                if (obj is Cons)
                {
                    var items = new System.Collections.Generic.List<LispObject>();
                    LispObject cur = obj;
                    while (cur is Cons cc) { items.Add(cc.Car); cur = cc.Cdr; }
                    return new LispVector(items.ToArray());
                }
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
                    var bitItems = new System.Collections.Generic.List<LispObject>();
                    if (obj is Nil) { /* empty */ }
                    else if (obj is Cons)
                    {
                        var cur = obj;
                        while (cur is Cons c2) { bitItems.Add(c2.Car); cur = c2.Cdr; }
                    }
                    else if (obj is LispVector sv)
                        for (int i2 = 0; i2 < sv.Length; i2++) bitItems.Add(sv.ElementAt(i2));
                    else throw new LispErrorException(new LispTypeError("COERCE: cannot coerce to bit-vector", obj));
                    return new LispVector(bitItems.ToArray(), "BIT");
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

    private static bool SeqTestMatch(LispObject item, LispObject elem, SeqKwArgs kw)
    {
        var val = kw.Key != null ? kw.Key.Invoke1(elem) : elem;
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
        var item = args[0];
        var seq = args[1];
        var kw = ParseSeqKwArgs(args, 2, "FIND");
        int len = seq is LispVector v ? v.Length : seq is LispString ls ? ls.Length : (int)((Fixnum)Length(seq)).Value;
        int start = kw.Start;
        int end = kw.End ?? len;

        if (seq is LispVector vec)
        {
            if (kw.FromEnd)
            {
                LispObject result = Nil.Instance;
                for (int i = start; i < end; i++)
                    if (SeqTestMatch(item, vec[i], kw)) result = vec[i];
                return result;
            }
            for (int i = start; i < end; i++)
                if (SeqTestMatch(item, vec[i], kw)) return vec[i];
            return Nil.Instance;
        }
        if (seq is LispString str)
        {
            if (kw.FromEnd)
            {
                LispObject result = Nil.Instance;
                for (int i = start; i < end; i++)
                {
                    var ch = LispChar.Make(str[i]);
                    if (SeqTestMatch(item, ch, kw)) result = ch;
                }
                return result;
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
        var seq = args[1];
        var kw = ParseSeqKwArgs(args, 2, "FIND-IF");
        int len = seq is LispVector v ? v.Length : seq is LispString ls ? ls.Length : (int)((Fixnum)Length(seq)).Value;
        int start = kw.Start;
        int end = kw.End ?? len;

        if (seq is LispVector vec)
        {
            if (kw.FromEnd)
            {
                LispObject result = Nil.Instance;
                for (int i = start; i < end; i++)
                {
                    var elem = kw.Key != null ? kw.Key.Invoke1(vec[i]) : vec[i];
                    if (IsTruthy(predFn.Invoke1(elem))) result = vec[i];
                }
                return result;
            }
            for (int i = start; i < end; i++)
            {
                var elem = kw.Key != null ? kw.Key.Invoke1(vec[i]) : vec[i];
                if (IsTruthy(predFn.Invoke1(elem))) return vec[i];
            }
            return Nil.Instance;
        }
        if (seq is LispString str)
        {
            if (kw.FromEnd)
            {
                LispObject result = Nil.Instance;
                for (int i = start; i < end; i++)
                {
                    var ch = LispChar.Make(str[i]);
                    var elem = kw.Key != null ? kw.Key.Invoke1(ch) : ch;
                    if (IsTruthy(predFn.Invoke1(elem))) result = ch;
                }
                return result;
            }
            for (int i = start; i < end; i++)
            {
                var ch = LispChar.Make(str[i]);
                var elem = kw.Key != null ? kw.Key.Invoke1(ch) : ch;
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
                var elem = kw.Key != null ? kw.Key.Invoke1(c.Car) : c.Car;
                if (IsTruthy(predFn.Invoke1(elem))) result = c.Car;
                cur = c.Cdr;
            }
            return result;
        }
        for (int i = start; i < end && cur is Cons c2; i++)
        {
            var elem = kw.Key != null ? kw.Key.Invoke1(c2.Car) : c2.Car;
            if (IsTruthy(predFn.Invoke1(elem))) return c2.Car;
            cur = c2.Cdr;
        }
        return Nil.Instance;
    }

    public static LispObject Position(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("POSITION: too few arguments"));
        var item = args[0];
        var seq = args[1];
        var kw = ParseSeqKwArgs(args, 2, "POSITION");
        int len = seq is LispVector v ? v.Length : seq is LispString ls ? ls.Length : (int)((Fixnum)Length(seq)).Value;
        int start = kw.Start;
        int end = kw.End ?? len;

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
        var predFn = CoerceToFunction(args[0]);
        var seq = args[1];
        var kw = ParseSeqKwArgs(args, 2, "POSITION-IF");
        int len = seq is LispVector v ? v.Length : seq is LispString ls ? ls.Length : (int)((Fixnum)Length(seq)).Value;
        int start = kw.Start;
        int end = kw.End ?? len;

        if (seq is LispVector vec)
        {
            if (kw.FromEnd)
            {
                for (int i = end - 1; i >= start; i--)
                {
                    var elem = kw.Key != null ? kw.Key.Invoke1(vec[i]) : vec[i];
                    if (IsTruthy(predFn.Invoke1(elem))) return Fixnum.Make(i);
                }
                return Nil.Instance;
            }
            for (int i = start; i < end; i++)
            {
                var elem = kw.Key != null ? kw.Key.Invoke1(vec[i]) : vec[i];
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
                    var elem = kw.Key != null ? kw.Key.Invoke1(ch) : ch;
                    if (IsTruthy(predFn.Invoke1(elem))) return Fixnum.Make(i);
                }
                return Nil.Instance;
            }
            for (int i = start; i < end; i++)
            {
                var ch = LispChar.Make(str[i]);
                var elem = kw.Key != null ? kw.Key.Invoke1(ch) : ch;
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
                var elem = kw.Key != null ? kw.Key.Invoke1(c.Car) : c.Car;
                if (IsTruthy(predFn.Invoke1(elem)))
                {
                    // For lists with from-end, continue scanning
                    LispObject result = Fixnum.Make(i);
                    cur = c.Cdr;
                    for (int j = i + 1; j < end && cur is Cons c3; j++)
                    {
                        var elem2 = kw.Key != null ? kw.Key.Invoke1(c3.Car) : c3.Car;
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
            var elem = kw.Key != null ? kw.Key.Invoke1(c2.Car) : c2.Car;
            if (IsTruthy(predFn.Invoke1(elem))) return Fixnum.Make(i);
            cur = c2.Cdr;
        }
        return Nil.Instance;
    }

    public static LispObject Count(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("COUNT: too few arguments"));
        var item = args[0];
        var seq = args[1];
        var kw = ParseSeqKwArgs(args, 2, "COUNT");
        int len = seq is LispVector v ? v.Length : seq is LispString ls ? ls.Length : (int)((Fixnum)Length(seq)).Value;
        int start = kw.Start;
        int end = kw.End ?? len;
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
        var predFn = CoerceToFunction(args[0]);
        var seq = args[1];
        var kw = ParseSeqKwArgs(args, 2, "COUNT-IF");
        int len = seq is LispVector v ? v.Length : seq is LispString ls ? ls.Length : (int)((Fixnum)Length(seq)).Value;
        int start = kw.Start;
        int end = kw.End ?? len;
        int count = 0;

        if (seq is LispVector vec)
        {
            if (kw.FromEnd)
                for (int i = end - 1; i >= start; i--) { var elem = kw.Key != null ? kw.Key.Invoke1(vec[i]) : vec[i]; if (IsTruthy(predFn.Invoke1(elem))) count++; }
            else
                for (int i = start; i < end; i++) { var elem = kw.Key != null ? kw.Key.Invoke1(vec[i]) : vec[i]; if (IsTruthy(predFn.Invoke1(elem))) count++; }
            return Fixnum.Make(count);
        }
        if (seq is LispString str)
        {
            if (kw.FromEnd)
                for (int i = end - 1; i >= start; i--) { var ch = LispChar.Make(str[i]); var elem = kw.Key != null ? kw.Key.Invoke1(ch) : ch; if (IsTruthy(predFn.Invoke1(elem))) count++; }
            else
                for (int i = start; i < end; i++) { var ch = LispChar.Make(str[i]); var elem = kw.Key != null ? kw.Key.Invoke1(ch) : ch; if (IsTruthy(predFn.Invoke1(elem))) count++; }
            return Fixnum.Make(count);
        }
        // List - for from-end, collect elements then iterate in reverse
        if (kw.FromEnd)
        {
            var elems = new System.Collections.Generic.List<LispObject>();
            var cur = seq;
            for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
            for (int i = start; i < end && cur is Cons c2; i++) { elems.Add(c2.Car); cur = c2.Cdr; }
            for (int i = elems.Count - 1; i >= 0; i--) { var elem = kw.Key != null ? kw.Key.Invoke1(elems[i]) : elems[i]; if (IsTruthy(predFn.Invoke1(elem))) count++; }
        }
        else
        {
            var cur = seq;
            for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
            for (int i = start; i < end && cur is Cons c2; i++) { var elem = kw.Key != null ? kw.Key.Invoke1(c2.Car) : c2.Car; if (IsTruthy(predFn.Invoke1(elem))) count++; cur = c2.Cdr; }
        }
        return Fixnum.Make(count);
    }

    // Helper: parse test/test-not/key keyword args for list functions (member, assoc, etc.)
    private struct ListKwArgs
    {
        public LispFunction? Test, TestNot, Key;
        public bool IsEqTest, IsEqlTest; // fast path flags
    }

    private static ListKwArgs ParseListKwArgs(LispObject[] args, int kwStart, string fnName)
    {
        var kw = new ListKwArgs();
        int kwCount = args.Length - kwStart;
        if (kwCount % 2 != 0)
            throw new LispErrorException(new LispProgramError($"{fnName}: odd number of keyword arguments"));
        bool? allowOtherKeys = null;
        bool hasUnknown = false;
        bool testSet = false, testNotSet = false, keySet = false;
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
                case "ALLOW-OTHER-KEYS": break;
                default: hasUnknown = true; break;
            }
        }
        if (hasUnknown && allowOtherKeys != true)
            throw new LispErrorException(new LispProgramError($"{fnName}: unknown keyword argument"));
        // Detect fast paths
        if (kw.TestNot == null && kw.Key == null)
        {
            if (kw.Test == null) kw.IsEqlTest = true;
            else
            {
                // Check if test function is the EQL or EQ symbol-function
                var eqlSym = Startup.CL.FindSymbol("EQL").symbol;
                var eqSym = Startup.CL.FindSymbol("EQ").symbol;
                if (kw.Test == eqlSym?.Function) kw.IsEqlTest = true;
                else if (kw.Test == eqSym?.Function) kw.IsEqTest = true;
            }
        }
        return kw;
    }

    private static bool ListTestMatch(LispObject item, LispObject element, in ListKwArgs kw)
    {
        var k = kw.Key != null ? kw.Key.Invoke1(element) : element;
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
        if (list is Nil) return Nil.Instance;
        if (list is not Cons)
            throw new LispErrorException(new LispTypeError("MEMBER: not a proper list", list));

        // Fast path: eq test, no key
        if (kw.IsEqTest)
        {
            var cur = list;
            for (; cur is Cons c; cur = c.Cdr)
                if (IsEqRef(item, c.Car)) return c;
            if (cur is not Nil) throw new LispErrorException(new LispTypeError("MEMBER: not a proper list", cur));
            return Nil.Instance;
        }
        // Fast path: eql test, no key
        if (kw.IsEqlTest)
        {
            var cur = list;
            for (; cur is Cons c; cur = c.Cdr)
                if (IsTrueEql(item, c.Car)) return c;
            if (cur is not Nil) throw new LispErrorException(new LispTypeError("MEMBER: not a proper list", cur));
            return Nil.Instance;
        }
        // General case
        {
            var cur = list;
            for (; cur is Cons c; cur = c.Cdr)
                if (ListTestMatch(item, c.Car, kw)) return c;
            if (cur is not Nil) throw new LispErrorException(new LispTypeError("MEMBER: not a proper list", cur));
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

    // ASSOC: (assoc item alist &key test test-not key)
    public static LispObject AssocFull(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("ASSOC: too few arguments"));
        var item = args[0];
        var alist = args[1];
        var kw = ParseListKwArgs(args, 2, "ASSOC");
        if (alist is Nil) return Nil.Instance;

        // Fast path: eq test, no key
        if (kw.IsEqTest)
        {
            var cur = alist;
            for (; cur is Cons c; cur = c.Cdr)
                if (c.Car is Cons pair && IsEqRef(item, pair.Car)) return pair;
            if (cur is not Nil) throw new LispErrorException(new LispTypeError("ASSOC: not a proper list", cur));
            return Nil.Instance;
        }
        // Fast path: eql test, no key
        if (kw.IsEqlTest)
        {
            var cur = alist;
            for (; cur is Cons c; cur = c.Cdr)
                if (c.Car is Cons pair && IsTrueEql(item, pair.Car)) return pair;
            if (cur is not Nil) throw new LispErrorException(new LispTypeError("ASSOC: not a proper list", cur));
            return Nil.Instance;
        }
        // General case
        {
            var cur = alist;
            for (; cur is Cons c; cur = c.Cdr)
            {
                if (c.Car is not Cons pair) continue; // skip nil entries
                var k = kw.Key != null ? kw.Key.Invoke1(pair.Car) : pair.Car;
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
            var elem = kw.Key != null ? kw.Key.Invoke1(pair.Car) : pair.Car;
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
        var item = args[0];
        var alist = args[1];
        var kw = ParseListKwArgs(args, 2, "RASSOC");
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
                var k = kw.Key != null ? kw.Key.Invoke1(pair.Cdr) : pair.Cdr;
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
            var elem = kw.Key != null ? kw.Key.Invoke1(pair.Cdr) : pair.Cdr;
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

        // Get elements as array for direct access
        int len;
        LispObject[] elems;
        if (seq is LispVector vec)
        {
            len = vec.Length;
            int start = startOpt ?? 0;
            int end = endOpt ?? len;
            int count = end - start;
            elems = new LispObject[count];
            for (int i = 0; i < count; i++)
                elems[i] = keyFn != null ? keyFn.Invoke1(vec[start + i]) : vec[start + i];
        }
        else if (seq is LispString str)
        {
            len = str.Length;
            int start = startOpt ?? 0;
            int end = endOpt ?? len;
            int count = end - start;
            elems = new LispObject[count];
            for (int i = 0; i < count; i++)
            {
                var ch = LispChar.Make(str[start + i]);
                elems[i] = keyFn != null ? keyFn.Invoke1(ch) : ch;
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
                if (idx >= start) list.Add(keyFn != null ? keyFn.Invoke1(c.Car) : c.Car);
            }
            len = idx;
            int end = endOpt ?? len;
            int count = end - start;
            if (count < list.Count) elems = list.GetRange(0, count).ToArray();
            else elems = list.ToArray();
        }

        if (elems.Length == 0)
        {
            if (hasIV) return iv;
            return fn.Invoke(Array.Empty<LispObject>());
        }

        if (fromEnd)
        {
            var result = hasIV ? iv : elems[elems.Length - 1];
            int startIdx = hasIV ? elems.Length - 1 : elems.Length - 2;
            for (int i = startIdx; i >= 0; i--)
                result = fn.Invoke2(elems[i], result);
            return result;
        }
        else
        {
            var result = hasIV ? iv : elems[0];
            int startIdx = hasIV ? 0 : 1;
            for (int i = startIdx; i < elems.Length; i++)
                result = fn.Invoke2(result, elems[i]);
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
    public static LispObject RemoveFull(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("REMOVE: too few arguments"));
        var item = args[0];
        var seq = args[1];
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("REMOVE: not a sequence", seq));
        var kw = ParseSeqKwArgs(args, 2, "REMOVE");
        return RemoveCore(seq, kw, (elem) => SeqTestMatch(item, elem, kw));
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
        var kw = ParseSeqKwArgs(args, 2, "REMOVE-IF");
        return RemoveCore(seq, kw, (elem) =>
        {
            var val = kw.Key != null ? kw.Key.Invoke1(elem) : elem;
            return IsTruthy(predFn.Invoke1(val));
        });
    }

    // Core remove logic shared by REMOVE and REMOVE-IF
    private static LispObject RemoveCore(LispObject seq, SeqKwArgs kw, Func<LispObject, bool> matches)
    {
        if (seq is Nil) return Nil.Instance;

        // Normalize count: null=no limit, <=0=remove nothing
        int? maxRemove = kw.Count;
        if (maxRemove.HasValue && maxRemove.Value <= 0)
        {
            // count <= 0: remove nothing, return copy
            return CopySeq(seq);
        }

        if (seq is LispVector vec)
        {
            int len = vec.Length;
            int start = kw.Start;
            int end = kw.End ?? len;
            return RemoveCoreIndexed(len, start, end, kw.FromEnd, maxRemove,
                i => vec[i], seq);
        }
        if (seq is LispString str)
        {
            int len = str.Length;
            int start = kw.Start;
            int end = kw.End ?? len;
            return RemoveCoreIndexed(len, start, end, kw.FromEnd, maxRemove,
                i => LispChar.Make(str[i]), seq);
        }
        // List
        return RemoveCoreList(seq, kw.Start, kw.End, kw.FromEnd, maxRemove);

        // Local function for indexed sequences (vector/string)
        LispObject RemoveCoreIndexed(int len, int start, int end, bool fromEnd, int? maxRem,
            Func<int, LispObject> getElem, LispObject origSeq)
        {
            if (fromEnd && maxRem.HasValue)
            {
                // FROM-END with COUNT: find all match positions, remove last maxRem
                var matchPositions = new System.Collections.Generic.List<int>();
                for (int i = start; i < end; i++)
                    if (matches(getElem(i))) matchPositions.Add(i);
                // Take the last maxRem positions (rightmost matches)
                var removeSet = new System.Collections.Generic.HashSet<int>();
                for (int i = matchPositions.Count - 1; i >= 0 && removeSet.Count < maxRem.Value; i--)
                    removeSet.Add(matchPositions[i]);
                var result = new System.Collections.Generic.List<LispObject>();
                for (int i = 0; i < len; i++)
                    if (!removeSet.Contains(i)) result.Add(getElem(i));
                return CoerceResult(result, origSeq);
            }
            else
            {
                // Forward scan: remove first maxRem matches in [start,end)
                var result = new System.Collections.Generic.List<LispObject>();
                int removed = 0;
                for (int i = 0; i < len; i++)
                {
                    var elem = getElem(i);
                    if (i >= start && i < end && (!maxRem.HasValue || removed < maxRem.Value) && matches(elem))
                        removed++;
                    else
                        result.Add(elem);
                }
                return CoerceResult(result, origSeq);
            }
        }

        // Local function for list sequences
        LispObject RemoveCoreList(LispObject listSeq, int start, int? endOpt, bool fromEnd, int? maxRem)
        {
            // Collect all elements with indices
            var allElems = new System.Collections.Generic.List<LispObject>();
            for (var cur = listSeq; cur is Cons c; cur = c.Cdr)
                allElems.Add(c.Car);
            int len = allElems.Count;
            int end = endOpt ?? len;

            if (fromEnd && maxRem.HasValue)
            {
                // FROM-END with COUNT: find match positions in [start,end), remove rightmost maxRem
                var matchPositions = new System.Collections.Generic.List<int>();
                for (int i = start; i < end; i++)
                    if (matches(allElems[i])) matchPositions.Add(i);
                var removeSet = new System.Collections.Generic.HashSet<int>();
                for (int i = matchPositions.Count - 1; i >= 0 && removeSet.Count < maxRem.Value; i--)
                    removeSet.Add(matchPositions[i]);
                var result = new System.Collections.Generic.List<LispObject>();
                for (int i = 0; i < len; i++)
                    if (!removeSet.Contains(i)) result.Add(allElems[i]);
                return List(result.ToArray());
            }
            else
            {
                // Forward scan
                var result = new System.Collections.Generic.List<LispObject>();
                int removed = 0;
                for (int i = 0; i < len; i++)
                {
                    if (i >= start && i < end && (!maxRem.HasValue || removed < maxRem.Value) && matches(allElems[i]))
                        removed++;
                    else
                        result.Add(allElems[i]);
                }
                return List(result.ToArray());
            }
        }
    }

    // SUBSTITUTE: (substitute newitem olditem sequence &key test test-not key count from-end start end)
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
        return SubstituteCore(newitem, seq, kw, (elem) => SeqTestMatch(olditem, elem, kw));
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
        return SubstituteCore(newitem, seq, kw, (elem) =>
        {
            var val = kw.Key != null ? kw.Key.Invoke1(elem) : elem;
            return IsTruthy(predFn.Invoke1(val));
        });
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
        return NsubstituteCore(newitem, seq, kw, (elem) => SeqTestMatch(olditem, elem, kw));
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
        return NsubstituteCore(newitem, seq, kw, (elem) =>
        {
            var val = kw.Key != null ? kw.Key.Invoke1(elem) : elem;
            return IsTruthy(predFn.Invoke1(val));
        });
    }

    // Core substitute logic (non-destructive)
    private static LispObject SubstituteCore(LispObject newitem, LispObject seq, SeqKwArgs kw, Func<LispObject, bool> matches)
    {
        if (seq is Nil) return Nil.Instance;

        int? maxSub = kw.Count;
        if (maxSub.HasValue && maxSub.Value <= 0)
            return CopySeq(seq);

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

        if (kw.FromEnd)
        {
            // FROM-END: scan right-to-left, mark positions to substitute
            var subSet = new System.Collections.Generic.HashSet<int>();
            int subbed = 0;
            for (int i = end - 1; i >= start; i--)
            {
                if (maxSub.HasValue && subbed >= maxSub.Value) break;
                if (matches(allElems[i])) { subSet.Add(i); subbed++; }
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
                if (i >= start && i < end && (!maxSub.HasValue || subbed < maxSub.Value) && matches(allElems[i]))
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
    private static LispObject NsubstituteCore(LispObject newitem, LispObject seq, SeqKwArgs kw, Func<LispObject, bool> matches)
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

            if (kw.FromEnd)
            {
                int subbed = 0;
                for (int i = end - 1; i >= start; i--)
                {
                    if (maxSub.HasValue && subbed >= maxSub.Value) break;
                    if (matches(vec[i])) { vec.SetElement(i, newitem); subbed++; }
                }
            }
            else
            {
                int subbed = 0;
                for (int i = start; i < end; i++)
                {
                    if (maxSub.HasValue && subbed >= maxSub.Value) break;
                    if (matches(vec[i])) { vec.SetElement(i, newitem); subbed++; }
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
                    if (matches(cells[i].Car)) { cells[i].Car = newitem; subbed++; }
                }
            }
            else
            {
                int subbed = 0;
                for (int i = 0; i < cells.Count; i++)
                {
                    if (maxSub.HasValue && subbed >= maxSub.Value) break;
                    if (matches(cells[i].Car)) { cells[i].Car = newitem; subbed++; }
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
        var seq = args[0];
        if (seq is not Nil && seq is not Cons && seq is not LispVector && seq is not LispString)
            throw new LispErrorException(new LispTypeError("REMOVE-DUPLICATES: not a sequence", seq));
        var kw = ParseSeqKwArgs(args, 1, "REMOVE-DUPLICATES");
        if (seq is Nil) return Nil.Instance;

        // Collect all elements
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

        // Determine which elements are duplicates
        var isDup = new bool[len];
        for (int i = start; i < end; i++)
        {
            if (isDup[i]) continue;
            var ki = kw.Key != null ? kw.Key.Invoke1(allElems[i]) : allElems[i];
            if (kw.FromEnd)
            {
                // from-end=t: keep first occurrence, mark later duplicates
                for (int j = i + 1; j < end; j++)
                {
                    if (isDup[j]) continue;
                    var kj = kw.Key != null ? kw.Key.Invoke1(allElems[j]) : allElems[j];
                    if (SeqTestMatch2(ki, kj, kw))
                        isDup[j] = true;
                }
            }
            else
            {
                // default: keep last occurrence, mark earlier duplicates
                for (int j = i + 1; j < end; j++)
                {
                    if (isDup[j]) continue;
                    var kj = kw.Key != null ? kw.Key.Invoke1(allElems[j]) : allElems[j];
                    if (SeqTestMatch2(ki, kj, kw))
                    {
                        isDup[i] = true;
                        break;
                    }
                }
            }
        }

        var result = new System.Collections.Generic.List<LispObject>();
        for (int i = 0; i < len; i++)
            if (!isDup[i]) result.Add(allElems[i]);
        return CoerceResult(result, seq);
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
        var predFn = CoerceToFunction(args[0]);

        if (args.Length == 2)
        {
            // Single sequence fast path
            var seq = args[1];
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

        // Multiple sequences: parallel iteration
        return EveryMulti(predFn, args);
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
        var predFn = CoerceToFunction(args[0]);

        if (args.Length == 2)
        {
            // Single sequence fast path
            var seq = args[1];
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

        // Multiple sequences
        return SomeMulti(predFn, args);
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
                var k1 = keyFn != null ? keyFn.Invoke1(x1) : x1;
                var k2 = keyFn != null ? keyFn.Invoke1(x2) : x2;
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
                var k1 = keyFn != null ? keyFn.Invoke1(x1) : x1;
                var k2 = keyFn != null ? keyFn.Invoke1(x2) : x2;
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
        if (seq is Cons)
        {
            var list = new System.Collections.Generic.List<LispObject>();
            for (var cur = seq; cur is Cons c; cur = c.Cdr) list.Add(c.Car);
            return list.ToArray();
        }
        throw new LispErrorException(new LispTypeError($"{fnName}: not a sequence", seq));
    }

    public static LispObject Search(LispObject seq1, LispObject seq2)
    {
        // Delegate to full implementation with no keyword args
        return SearchFull(new LispObject[] { seq1, seq2 });
    }

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

        Func<LispObject, LispObject, bool> elemTest;
        if (testFn != null) elemTest = (a, b) => IsTruthy(testFn.Invoke2(a, b));
        else if (testNotFn != null) elemTest = (a, b) => !IsTruthy(testNotFn.Invoke2(a, b));
        else elemTest = (a, b) => IsTrueEql(a, b);

        int len1 = ReplaceSeqLength(seq1), len2 = ReplaceSeqLength(seq2);
        int e1 = end1 ?? len1, e2 = end2 ?? len2;
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
                    var a = keyFn != null ? keyFn.Invoke1(ReplaceSeqGet(seq1, start1 + j)) : ReplaceSeqGet(seq1, start1 + j);
                    var b = keyFn != null ? keyFn.Invoke1(ReplaceSeqGet(seq2, i + j)) : ReplaceSeqGet(seq2, i + j);
                    if (!elemTest(a, b)) match = false;
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
                    var a = keyFn != null ? keyFn.Invoke1(ReplaceSeqGet(seq1, start1 + j)) : ReplaceSeqGet(seq1, start1 + j);
                    var b = keyFn != null ? keyFn.Invoke1(ReplaceSeqGet(seq2, i + j)) : ReplaceSeqGet(seq2, i + j);
                    if (!elemTest(a, b)) match = false;
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
        if (obj is Symbol sym) return new LispString(sym.Name);
        if (obj is Nil) return new LispString("NIL");
        if (obj is T) return new LispString("T");
        if (obj is LispChar ch) return new LispString(ch.Value.ToString());
        throw new LispErrorException(new LispTypeError("STRING: cannot convert to string", obj));
    }

    internal static void RegisterSequenceBuiltins()
    {
        // COUNT, COUNT-IF, COUNT-IF-NOT
        Emitter.CilAssembler.RegisterFunction("COUNT",
            new LispFunction(args => Runtime.Count(args)));
        Emitter.CilAssembler.RegisterFunction("COUNT-IF",
            new LispFunction(args => Runtime.CountIf(args)));
        Emitter.CilAssembler.RegisterFunction("COUNT-IF-NOT",
            new LispFunction(args =>
            {
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
        Emitter.CilAssembler.RegisterFunction("FIND",
            new LispFunction(args => Runtime.Find(args)));
        Emitter.CilAssembler.RegisterFunction("FIND-IF",
            new LispFunction(args => Runtime.FindIf(args)));
        Emitter.CilAssembler.RegisterFunction("FIND-IF-NOT",
            new LispFunction(args =>
            {
                var predFn = Runtime.CoerceToFunction(args[0]);
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[0] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.FindIf(newArgs);
            }));
        // POSITION, POSITION-IF, POSITION-IF-NOT
        Emitter.CilAssembler.RegisterFunction("POSITION",
            new LispFunction(args => Runtime.Position(args)));
        Emitter.CilAssembler.RegisterFunction("POSITION-IF",
            new LispFunction(args => Runtime.PositionIf(args)));
        Emitter.CilAssembler.RegisterFunction("POSITION-IF-NOT",
            new LispFunction(args =>
            {
                var predFn = Runtime.CoerceToFunction(args[0]);
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[0] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.PositionIf(newArgs);
            }));
        // REDUCE
        Emitter.CilAssembler.RegisterFunction("REDUCE",
            new LispFunction(args => Runtime.Reduce(args)));
        // MEMBER, MEMBER-IF, MEMBER-IF-NOT
        Emitter.CilAssembler.RegisterFunction("MEMBER",
            new LispFunction(args => Runtime.MemberFull(args)));
        Emitter.CilAssembler.RegisterFunction("MEMBER-IF",
            new LispFunction(args => Runtime.MemberIf(args)));
        Emitter.CilAssembler.RegisterFunction("MEMBER-IF-NOT",
            new LispFunction(args =>
            {
                var predFn = Runtime.CoerceToFunction(args[0]);
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[0] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.MemberIf(newArgs);
            }));
        // ASSOC, ASSOC-IF, ASSOC-IF-NOT
        Emitter.CilAssembler.RegisterFunction("ASSOC",
            new LispFunction(args => Runtime.AssocFull(args)));
        Emitter.CilAssembler.RegisterFunction("ASSOC-IF",
            new LispFunction(args => Runtime.AssocIf(args)));
        Emitter.CilAssembler.RegisterFunction("ASSOC-IF-NOT",
            new LispFunction(args =>
            {
                var predFn = Runtime.CoerceToFunction(args[0]);
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[0] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.AssocIf(newArgs);
            }));
        // RASSOC, RASSOC-IF, RASSOC-IF-NOT
        Emitter.CilAssembler.RegisterFunction("RASSOC",
            new LispFunction(args => Runtime.RassocFull(args)));
        Emitter.CilAssembler.RegisterFunction("RASSOC-IF",
            new LispFunction(args => Runtime.RassocIf(args)));
        Emitter.CilAssembler.RegisterFunction("RASSOC-IF-NOT",
            new LispFunction(args =>
            {
                var predFn = Runtime.CoerceToFunction(args[0]);
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[0] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.RassocIf(newArgs);
            }));
        // REMOVE, REMOVE-IF, REMOVE-IF-NOT
        Emitter.CilAssembler.RegisterFunction("REMOVE",
            new LispFunction(args => Runtime.RemoveFull(args)));
        Emitter.CilAssembler.RegisterFunction("REMOVE-IF",
            new LispFunction(args => Runtime.RemoveIf(args)));
        Emitter.CilAssembler.RegisterFunction("REMOVE-IF-NOT",
            new LispFunction(args =>
            {
                var predFn = Runtime.CoerceToFunction(args[0]);
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[0] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.RemoveIf(newArgs);
            }));
        // DELETE, DELETE-IF, DELETE-IF-NOT
        Emitter.CilAssembler.RegisterFunction("DELETE",
            new LispFunction(args => Runtime.RemoveFull(args)));
        Emitter.CilAssembler.RegisterFunction("DELETE-IF",
            new LispFunction(args => Runtime.RemoveIf(args)));
        Emitter.CilAssembler.RegisterFunction("DELETE-IF-NOT",
            new LispFunction(args =>
            {
                var predFn = Runtime.CoerceToFunction(args[0]);
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[0] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.RemoveIf(newArgs);
            }));
        // SUBSTITUTE, SUBSTITUTE-IF, SUBSTITUTE-IF-NOT
        Emitter.CilAssembler.RegisterFunction("SUBSTITUTE",
            new LispFunction(args => Runtime.SubstituteFull(args)));
        Emitter.CilAssembler.RegisterFunction("SUBSTITUTE-IF",
            new LispFunction(args => Runtime.SubstituteIf(args)));
        Emitter.CilAssembler.RegisterFunction("SUBSTITUTE-IF-NOT",
            new LispFunction(args =>
            {
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
                var predFn = Runtime.CoerceToFunction(args[1]);
                var newArgs = new LispObject[args.Length];
                Array.Copy(args, newArgs, args.Length);
                newArgs[1] = new LispFunction(a => Runtime.IsTruthy(predFn.Invoke(a)) ? Nil.Instance : T.Instance);
                return Runtime.NsubstituteIf(newArgs);
            }));
        // EVERY, SOME, NOTEVERY, NOTANY
        Emitter.CilAssembler.RegisterFunction("EVERY",
            new LispFunction(args => Runtime.Every(args)));
        Emitter.CilAssembler.RegisterFunction("SOME",
            new LispFunction(args => Runtime.Some(args)));
        Emitter.CilAssembler.RegisterFunction("NOTEVERY",
            new LispFunction(args => Runtime.IsTruthy(Runtime.Every(args)) ? Nil.Instance : T.Instance));
        Emitter.CilAssembler.RegisterFunction("NOTANY",
            new LispFunction(args => Runtime.IsTruthy(Runtime.Some(args)) ? Nil.Instance : T.Instance));
        // MISMATCH, REMOVE-DUPLICATES, DELETE-DUPLICATES, REPLACE
        Emitter.CilAssembler.RegisterFunction("MISMATCH",
            new LispFunction(args => Runtime.MismatchFull(args)));
        Emitter.CilAssembler.RegisterFunction("REMOVE-DUPLICATES",
            new LispFunction(args => Runtime.RemoveDuplicatesFull(args)));
        Emitter.CilAssembler.RegisterFunction("DELETE-DUPLICATES",
            new LispFunction(args => Runtime.RemoveDuplicatesFull(args)));
        Emitter.CilAssembler.RegisterFunction("REPLACE",
            new LispFunction(args => Runtime.Replace(args)));
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

        // String comparison functions
        Emitter.CilAssembler.RegisterFunction("STRING=",   new LispFunction(Runtime.StringEq,          "STRING=",          -1));
        Emitter.CilAssembler.RegisterFunction("STRING<",   new LispFunction(Runtime.StringLt,          "STRING<",          -1));
        Emitter.CilAssembler.RegisterFunction("STRING>",   new LispFunction(Runtime.StringGt,          "STRING>",          -1));
        Emitter.CilAssembler.RegisterFunction("STRING<=",  new LispFunction(Runtime.StringLe,          "STRING<=",         -1));
        Emitter.CilAssembler.RegisterFunction("STRING>=",  new LispFunction(Runtime.StringGe,          "STRING>=",         -1));
        Emitter.CilAssembler.RegisterFunction("STRING/=",  new LispFunction(Runtime.StringNotEq,       "STRING/=",         -1));
        Emitter.CilAssembler.RegisterFunction("STRING-EQUAL",        new LispFunction(Runtime.StringEqualFn,       "STRING-EQUAL",        -1));
        Emitter.CilAssembler.RegisterFunction("STRING-NOT-EQUAL",    new LispFunction(Runtime.StringNotEqualFn,    "STRING-NOT-EQUAL",    -1));
        Emitter.CilAssembler.RegisterFunction("STRING-LESSP",        new LispFunction(Runtime.StringLessp,         "STRING-LESSP",        -1));
        Emitter.CilAssembler.RegisterFunction("STRING-GREATERP",     new LispFunction(Runtime.StringGreaterp,      "STRING-GREATERP",     -1));
        Emitter.CilAssembler.RegisterFunction("STRING-NOT-GREATERP", new LispFunction(Runtime.StringNotGreaterp,   "STRING-NOT-GREATERP", -1));
        Emitter.CilAssembler.RegisterFunction("STRING-NOT-LESSP",    new LispFunction(Runtime.StringNotLessp,      "STRING-NOT-LESSP",    -1));
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
        Emitter.CilAssembler.RegisterFunction("SORT",
            new LispFunction(args => Runtime.SortFull(args)));
        Emitter.CilAssembler.RegisterFunction("STABLE-SORT",
            new LispFunction(args => Runtime.SortFull(args)));
        // SEARCH
        Emitter.CilAssembler.RegisterFunction("SEARCH",
            new LispFunction(args => Runtime.SearchFull(args)));
        // COPY-SEQ
        Startup.RegisterUnary("COPY-SEQ", Runtime.CopySeq);

        // CONCATENATE
        Emitter.CilAssembler.RegisterFunction("CONCATENATE",
            new LispFunction(args => {
                var seqs = new LispObject[args.Length - 1];
                Array.Copy(args, 1, seqs, 0, seqs.Length);
                return Runtime.Concatenate(args[0], seqs);
            }, "CONCATENATE", -1));
    }


}
