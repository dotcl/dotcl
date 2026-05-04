namespace DotCL;

public static partial class Runtime
{
    // --- Bit array operations ---

    private static LispVector GetBitArray(LispObject obj, string fname)
    {
        if (obj is LispVector v && v.IsBitVector) return v;
        throw new LispErrorException(new LispTypeError($"{fname}: not a bit-array", obj));
    }

    public static LispVector BitOp(LispObject[] args, string fname, Func<int, int, int> op)
    {
        if (args.Length < 2) throw new LispErrorException(new LispProgramError($"{fname}: too few arguments"));
        if (args.Length > 3) throw new LispErrorException(new LispProgramError($"{fname}: too many arguments"));
        var a1 = GetBitArray(args[0], fname);
        var a2 = GetBitArray(args[1], fname);
        int size = a1.Capacity;
        if (a2.Capacity != size) throw new LispErrorException(new LispTypeError($"{fname}: arrays must be same size", args[1]));
        LispVector result;
        if (args.Length >= 3 && args[2] is LispVector rv && rv.IsBitVector)
            result = rv;
        else if (args.Length >= 3 && args[2] is T)
            result = a1;
        else
        {
            if (a1.Rank != 1)
            {
                var items = new LispObject[size];
                Array.Fill(items, Fixnum.Make(0));
                result = new LispVector(items, a1.Dimensions, "BIT");
            }
            else
                result = new LispVector(size, Fixnum.Make(0), "BIT");
        }

        // Fast path: all three have packed bit data → operate on ulong[] directly
        if (a1._bitData != null && a2._bitData != null && result._bitData != null)
        {
            int words = a1._bitData.Length;
            // Encode truth table as 4-bit code to select direct operation
            int code = ((op(1, 1) & 1) << 3) | ((op(1, 0) & 1) << 2) | ((op(0, 1) & 1) << 1) | (op(0, 0) & 1);
            switch (code)
            {
                case 0b1000: // AND
                    for (int w = 0; w < words; w++) result._bitData[w] = a1._bitData[w] & a2._bitData[w];
                    break;
                case 0b1110: // IOR
                    for (int w = 0; w < words; w++) result._bitData[w] = a1._bitData[w] | a2._bitData[w];
                    break;
                case 0b0110: // XOR
                    for (int w = 0; w < words; w++) result._bitData[w] = a1._bitData[w] ^ a2._bitData[w];
                    break;
                case 0b0111: // NAND
                    for (int w = 0; w < words; w++) result._bitData[w] = ~(a1._bitData[w] & a2._bitData[w]);
                    break;
                case 0b0001: // NOR
                    for (int w = 0; w < words; w++) result._bitData[w] = ~(a1._bitData[w] | a2._bitData[w]);
                    break;
                case 0b1001: // EQV
                    for (int w = 0; w < words; w++) result._bitData[w] = ~(a1._bitData[w] ^ a2._bitData[w]);
                    break;
                case 0b0010: // ANDC1: ~a1 & a2
                    for (int w = 0; w < words; w++) result._bitData[w] = ~a1._bitData[w] & a2._bitData[w];
                    break;
                case 0b0100: // ANDC2: a1 & ~a2
                    for (int w = 0; w < words; w++) result._bitData[w] = a1._bitData[w] & ~a2._bitData[w];
                    break;
                case 0b1011: // ORC1: ~a1 | a2
                    for (int w = 0; w < words; w++) result._bitData[w] = ~a1._bitData[w] | a2._bitData[w];
                    break;
                case 0b1101: // ORC2: a1 | ~a2
                    for (int w = 0; w < words; w++) result._bitData[w] = a1._bitData[w] | ~a2._bitData[w];
                    break;
                default: // Generic fallback
                    for (int w = 0; w < words; w++)
                    {
                        ulong v1 = a1._bitData[w], v2 = a2._bitData[w];
                        ulong r = 0;
                        if ((code & 1) != 0) r |= ~v1 & ~v2;
                        if ((code & 2) != 0) r |= ~v1 & v2;
                        if ((code & 4) != 0) r |= v1 & ~v2;
                        if ((code & 8) != 0) r |= v1 & v2;
                        result._bitData[w] = r;
                    }
                    break;
            }
            return result;
        }

        // Slow path: element-by-element (displaced arrays, etc.)
        for (int i = 0; i < size; i++)
        {
            int b1 = a1.GetElement(i) is Fixnum f1 ? (int)f1.Value : 0;
            int b2 = a2.GetElement(i) is Fixnum f2 ? (int)f2.Value : 0;
            result.SetElement(i, Fixnum.Make(op(b1, b2) & 1));
        }
        return result;
    }

    public static LispVector BitOpUnary(LispObject[] args, string fname, Func<int, int> op)
    {
        if (args.Length < 1) throw new LispErrorException(new LispProgramError($"{fname}: too few arguments"));
        if (args.Length > 2) throw new LispErrorException(new LispProgramError($"{fname}: too many arguments"));
        var a1 = GetBitArray(args[0], fname);
        int size = a1.Capacity;
        LispVector result;
        if (args.Length >= 2 && args[1] is LispVector rv && rv.IsBitVector)
            result = rv;
        else if (args.Length >= 2 && args[1] is T)
            result = a1;
        else
        {
            if (a1.Rank != 1)
            {
                var items = new LispObject[size];
                Array.Fill(items, Fixnum.Make(0));
                result = new LispVector(items, a1.Dimensions, "BIT");
            }
            else
                result = new LispVector(size, Fixnum.Make(0), "BIT");
        }

        // Fast path: packed bit data
        if (a1._bitData != null && result._bitData != null)
        {
            int words = a1._bitData.Length;
            int test1 = op(1);
            int test0 = op(0);
            for (int w = 0; w < words; w++)
            {
                ulong v1 = a1._bitData[w];
                ulong r = 0;
                if ((test0 & 1) != 0) r |= ~v1;
                if ((test1 & 1) != 0) r |= v1;
                result._bitData[w] = r;
            }
            return result;
        }

        // Slow path
        for (int i = 0; i < size; i++)
        {
            int b1 = a1.GetElement(i) is Fixnum f1 ? (int)f1.Value : 0;
            result.SetElement(i, Fixnum.Make(op(b1) & 1));
        }
        return result;
    }

    // Character access and comparison
    // CHAR is like AREF - it ignores the fill pointer and accesses raw elements
    public static LispObject CharAccess(LispObject str, LispObject index)
    {
        int idx = index is Fixnum fi ? (int)fi.Value : -1;
        if (str is LispString s) return LispChar.Make(s[idx]);
        if (str is LispVector v && v.IsCharVector)
            return v.GetElement(idx) is LispChar lc ? (LispObject)lc : LispChar.Make('\0');
        throw new LispErrorException(new LispTypeError("CHAR: invalid arguments", str, Startup.Sym("STRING")));
    }

    public static LispObject CharSet(LispObject str, LispObject index, LispObject value)
    {
        if (index is Fixnum f && value is LispChar c)
        {
            if (str is LispString s) { s[(int)f.Value] = c.Value; return value; }
            if (str is LispVector v && v.IsCharVector) { v.SetElement((int)f.Value, c); return value; }
        }
        throw new LispErrorException(new LispTypeError("(SETF CHAR): invalid arguments", str));
    }

    public static LispObject FillPointer(LispObject vec)
    {
        if (vec is LispVector v && v.HasFillPointer) return Fixnum.Make(v.Length);
        throw new LispErrorException(new LispTypeError("FILL-POINTER: not a vector with fill-pointer", vec));
    }

    public static LispObject SetFillPointer(LispObject vec, LispObject fp)
    {
        if (vec is LispVector v && v.HasFillPointer && fp is Fixnum f)
        {
            v.SetFillPointer((int)f.Value);
            return fp;
        }
        throw new LispErrorException(new LispTypeError("(SETF FILL-POINTER): invalid arguments", vec));
    }

    public static LispObject CharEqual(LispObject a, LispObject b)
    {
        if (a is LispChar ca && b is LispChar cb)
            return ca.Value == cb.Value ? T.Instance : Nil.Instance;
        throw new LispErrorException(new LispTypeError("CHAR=: not characters", a, Startup.Sym("CHARACTER")));
    }

    public static LispObject CharLt(LispObject a, LispObject b) =>
        a is LispChar ca && b is LispChar cb
            ? (ca.Value < cb.Value ? T.Instance : Nil.Instance)
            : throw new LispErrorException(new LispTypeError("CHAR<: not characters", a));
    public static LispObject CharGt(LispObject a, LispObject b) =>
        a is LispChar ca && b is LispChar cb
            ? (ca.Value > cb.Value ? T.Instance : Nil.Instance)
            : throw new LispErrorException(new LispTypeError("CHAR>: not characters", a));
    public static LispObject CharLe(LispObject a, LispObject b) =>
        a is LispChar ca && b is LispChar cb
            ? (ca.Value <= cb.Value ? T.Instance : Nil.Instance)
            : throw new LispErrorException(new LispTypeError("CHAR<=: not characters", a));
    public static LispObject CharGe(LispObject a, LispObject b) =>
        a is LispChar ca && b is LispChar cb
            ? (ca.Value >= cb.Value ? T.Instance : Nil.Instance)
            : throw new LispErrorException(new LispTypeError("CHAR>=: not characters", a));
    public static LispObject CharNotEqual(LispObject a, LispObject b) =>
        a is LispChar ca && b is LispChar cb
            ? (ca.Value != cb.Value ? T.Instance : Nil.Instance)
            : throw new LispErrorException(new LispTypeError("CHAR/=: not characters", a));

    // Case-insensitive character comparisons
    public static char CharFoldCase(LispChar c) => char.ToUpperInvariant(c.Value);
    public static LispObject CharEqualCI(LispObject a, LispObject b) =>
        a is LispChar ca && b is LispChar cb
            ? (CharFoldCase(ca) == CharFoldCase(cb) ? T.Instance : Nil.Instance)
            : throw new LispErrorException(new LispTypeError("CHAR-EQUAL: not characters", a));
    public static LispObject CharNotEqualCI(LispObject a, LispObject b) =>
        a is LispChar ca && b is LispChar cb
            ? (CharFoldCase(ca) != CharFoldCase(cb) ? T.Instance : Nil.Instance)
            : throw new LispErrorException(new LispTypeError("CHAR-NOT-EQUAL: not characters", a));
    public static LispObject CharLesspCI(LispObject a, LispObject b) =>
        a is LispChar ca && b is LispChar cb
            ? (CharFoldCase(ca) < CharFoldCase(cb) ? T.Instance : Nil.Instance)
            : throw new LispErrorException(new LispTypeError("CHAR-LESSP: not characters", a));
    public static LispObject CharGreaterpCI(LispObject a, LispObject b) =>
        a is LispChar ca && b is LispChar cb
            ? (CharFoldCase(ca) > CharFoldCase(cb) ? T.Instance : Nil.Instance)
            : throw new LispErrorException(new LispTypeError("CHAR-GREATERP: not characters", a));
    public static LispObject CharNotLesspCI(LispObject a, LispObject b) =>
        a is LispChar ca && b is LispChar cb
            ? (CharFoldCase(ca) >= CharFoldCase(cb) ? T.Instance : Nil.Instance)
            : throw new LispErrorException(new LispTypeError("CHAR-NOT-LESSP: not characters", a));
    public static LispObject CharNotGreaterpCI(LispObject a, LispObject b) =>
        a is LispChar ca && b is LispChar cb
            ? (CharFoldCase(ca) <= CharFoldCase(cb) ? T.Instance : Nil.Instance)
            : throw new LispErrorException(new LispTypeError("CHAR-NOT-GREATERP: not characters", a));

    // Character predicates
    public static LispObject UpperCaseP(LispObject obj) =>
        obj is LispChar c ? (char.IsUpper(c.Value) ? T.Instance : Nil.Instance)
            : throw new LispErrorException(new LispTypeError("UPPER-CASE-P: not a character", obj));
    public static LispObject LowerCaseP(LispObject obj) =>
        obj is LispChar c ? (char.IsLower(c.Value) ? T.Instance : Nil.Instance)
            : throw new LispErrorException(new LispTypeError("LOWER-CASE-P: not a character", obj));
    public static LispObject AlphaCharP(LispObject obj) =>
        obj is LispChar c ? (char.IsLetter(c.Value) ? T.Instance : Nil.Instance)
            : throw new LispErrorException(new LispTypeError("ALPHA-CHAR-P: not a character", obj));
    public static LispObject Alphanumericp(LispObject obj) =>
        obj is LispChar c ? (char.IsLetterOrDigit(c.Value) ? T.Instance : Nil.Instance)
            : throw new LispErrorException(new LispTypeError("ALPHANUMERICP: not a character", obj));
    public static LispObject GraphicCharP(LispObject obj) =>
        obj is LispChar c ? (IsGraphicChar(c.Value) ? T.Instance : Nil.Instance)
            : throw new LispErrorException(new LispTypeError("GRAPHIC-CHAR-P: not a character", obj));
    internal static bool IsGraphicChar(char ch) =>
        !char.IsControl(ch) && ch != '\x7F'
        && char.GetUnicodeCategory(ch) != System.Globalization.UnicodeCategory.Format
        && char.GetUnicodeCategory(ch) != System.Globalization.UnicodeCategory.Surrogate
        && char.GetUnicodeCategory(ch) != System.Globalization.UnicodeCategory.PrivateUse
        && char.GetUnicodeCategory(ch) != System.Globalization.UnicodeCategory.OtherNotAssigned;
    public static LispObject BothCaseP(LispObject obj) =>
        obj is LispChar c ? ((char.IsUpper(c.Value) || char.IsLower(c.Value)) ? T.Instance : Nil.Instance)
            : throw new LispErrorException(new LispTypeError("BOTH-CASE-P: not a character", obj));
    public static LispObject CharUpcase(LispObject obj) =>
        obj is LispChar c ? LispChar.Make(char.IsLower(c.Value) ? char.ToUpperInvariant(c.Value) : c.Value)
            : throw new LispErrorException(new LispTypeError("CHAR-UPCASE: not a character", obj));
    public static LispObject CharDowncase(LispObject obj) =>
        obj is LispChar c ? LispChar.Make(char.IsUpper(c.Value) ? char.ToLowerInvariant(c.Value) : c.Value)
            : throw new LispErrorException(new LispTypeError("CHAR-DOWNCASE: not a character", obj));

    // Hash table iteration
    public static LispObject Maphash(LispObject fn, LispObject table)
    {
        if (fn is not LispFunction func)
            throw new LispErrorException(new LispTypeError("MAPHASH: not a function", fn, Startup.Sym("FUNCTION")));
        if (table is not LispHashTable ht)
            throw new LispErrorException(new LispTypeError("MAPHASH: not a hash table", table, Startup.Sym("HASH-TABLE")));
        ht.ForEach((k, v) => func.Invoke2(k, v));
        return Nil.Instance;
    }

    // CL spec: type-of returns the most specific type.
    // For integers: return (INTEGER n n) so it's a subtype of all appropriate numeric types.
    // For chars: return STANDARD-CHAR/BASE-CHAR/CHARACTER based on code point.
    private static LispObject IntegerSingletonType(LispObject nObj)
        => new Cons(Startup.Sym("INTEGER"), new Cons(nObj, new Cons(nObj, Nil.Instance)));

    private static bool IsStandardChar(char c)
        => c == '\n' || (c >= ' ' && c <= '~');

    public static LispObject TypeOf(LispObject obj) => obj switch
    {
        Nil => Startup.Sym("NULL"),
        T => Startup.Sym("BOOLEAN"),
        Fixnum f => IntegerSingletonType(f),
        Bignum b => IntegerSingletonType(b),
        Ratio => Startup.Sym("RATIO"),
        SingleFloat => Startup.Sym("SINGLE-FLOAT"),
        DoubleFloat => Startup.Sym("DOUBLE-FLOAT"),
        LispComplex => Startup.Sym("COMPLEX"),
        Cons => Startup.Sym("CONS"),
        Symbol sym2 when sym2.HomePackage?.Name == "KEYWORD" => Startup.Sym("KEYWORD"),
        Symbol => Startup.Sym("SYMBOL"),
        LispString => Startup.Sym("SIMPLE-BASE-STRING"),
        LispChar c when IsStandardChar(c.Value) => Startup.Sym("STANDARD-CHAR"),
        LispChar c when c.Value <= '\x7F' => Startup.Sym("BASE-CHAR"),
        LispChar => Startup.Sym("CHARACTER"),
        GenericFunction gf when gf.StoredClass != null => gf.StoredClass.Name,
        GenericFunction => Startup.Sym("STANDARD-GENERIC-FUNCTION"),
        LispFunction => Startup.Sym("COMPILED-FUNCTION"),
        LispVector v when v.IsBitVector && !v.HasFillPointer && v.Rank == 1 => Startup.Sym("SIMPLE-BIT-VECTOR"),
        LispVector v when v.IsBitVector && v.Rank == 1 => Startup.Sym("BIT-VECTOR"),
        LispVector v when v.IsCharVector && v.ElementTypeName != "NIL" && !v.HasFillPointer && v.Rank == 1 => Startup.Sym("SIMPLE-BASE-STRING"),
        LispVector v when v.IsCharVector && v.ElementTypeName != "NIL" && v.HasFillPointer && v.Rank == 1 => Startup.Sym("BASE-STRING"),
        LispVector v when v.IsCharVector && v.ElementTypeName == "NIL" => VectorTypeOf(v),
        LispVector v when !v.IsCharVector && !v.IsBitVector && !v.HasFillPointer && v.ElementTypeName == "T" && v.Rank == 1 => Startup.Sym("SIMPLE-VECTOR"),
        LispVector v when v.Rank == 1 && !v.HasFillPointer => VectorTypeOf(v), // (SIMPLE-ARRAY et (n))
        LispVector v when v.Rank == 1 => Startup.Sym("VECTOR"),
        LispVector v => VectorTypeOf(v),
        LispReadtable => Startup.Sym("READTABLE"),
        LispRandomState => Startup.Sym("RANDOM-STATE"),
        LispMethod => Startup.Sym("STANDARD-METHOD"),
        LispHashTable => Startup.Sym("HASH-TABLE"),
        Package => Startup.Sym("PACKAGE"),
        LispStream s2 when s2.StreamTypeName != null => Startup.Sym(s2.StreamTypeName),
        LispStream => Startup.Sym("STREAM"),
        LispInstanceCondition lic => TypeOf(lic.Instance),
        LispCondition cond => Startup.Sym(cond.ConditionTypeName),
        LispLogicalPathname => Startup.Sym("LOGICAL-PATHNAME"),
        LispPathname => Startup.Sym("PATHNAME"),
        LispStruct s => s.TypeName,
        LispInstance inst => !inst.Class.NameCleared && inst.Class.Name is Symbol name && Runtime.FindClassOrNil(name) is LispClass foundClass && ReferenceEquals(foundClass, inst.Class) ? name : (LispObject)inst.Class,
        _ => Startup.Sym("T")
    };

    // Convert a stored ElementTypeName string to a proper Lisp element-type specifier
    private static LispObject ElemTypeSpecifier(string elemTypeName)
    {
        if (elemTypeName.StartsWith("UNSIGNED-BYTE-", StringComparison.Ordinal) &&
            long.TryParse(elemTypeName.AsSpan(14), out long ubits))
            return new Cons(Startup.Sym("UNSIGNED-BYTE"), new Cons(Fixnum.Make(ubits), Nil.Instance));
        if (elemTypeName.StartsWith("SIGNED-BYTE-", StringComparison.Ordinal) &&
            long.TryParse(elemTypeName.AsSpan(12), out long sbits))
            return new Cons(Startup.Sym("SIGNED-BYTE"), new Cons(Fixnum.Make(sbits), Nil.Instance));
        return Startup.Sym(elemTypeName);
    }

    // Returns compound type specifier (SIMPLE-ARRAY elem dims) for multi-dim/0-dim arrays
    private static LispObject VectorTypeOf(LispVector v)
    {
        bool isSimple = !v.HasFillPointer;
        string elemTypeName = (v.IsCharVector && v.ElementTypeName != "NIL") ? "CHARACTER" : v.IsBitVector ? "BIT" : (v.ElementTypeName ?? "T");
        var head = isSimple ? Startup.Sym("SIMPLE-ARRAY") : Startup.Sym("ARRAY");
        var elem = ElemTypeSpecifier(elemTypeName);
        LispObject dimList;
        if (v.Rank == 0)
        {
            dimList = Nil.Instance; // NIL = () = rank-0
        }
        else
        {
            var dims = v.Dimensions;
            LispObject cur = Nil.Instance;
            for (int i = dims.Length - 1; i >= 0; i--)
                cur = new Cons(new Fixnum(dims[i]), cur);
            dimList = cur;
        }
        return new Cons(head, new Cons(elem, new Cons(dimList, Nil.Instance)));
    }

    // --- List operations ---

    public static LispObject Car(LispObject obj)
    {
        if (obj is Cons c) return c.Car;
        if (obj is Nil) return Nil.Instance;
        var objStr = obj?.ToString() ?? "null";
        if (objStr.Length > 80) objStr = objStr[..80] + "...";
        var frames = new System.Diagnostics.StackTrace(1, false).GetFrames();
        var topFrames = frames.Take(8).Select(f => {
            var m = f.GetMethod();
            return m != null ? $"{m.DeclaringType?.Name}.{m.Name}" : "?";
        });
        var stackHint = string.Join(" → ", topFrames);
        throw new LispErrorException(new LispTypeError($"CAR: not a list (got {obj?.GetType().Name ?? "null"}: {objStr})\n  at: {stackHint}", obj));
    }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static LispObject Cdr(LispObject obj)
    {
        if (obj is Cons c) return c.Cdr;
        if (obj is Nil) return Nil.Instance;
        throw new LispErrorException(new LispTypeError("CDR: not a list", obj));
    }

    public static LispObject MakeCons(LispObject car, LispObject cdr) => new Cons(car, cdr);

    public static void CheckUnaryArity(string name, LispObject[] args)
    {
        if (args.Length != 1)
        {
            throw new LispErrorException(new LispProgramError($"{name}: wrong number of arguments: {args.Length} (expected 1)"));
        }
    }

    public static void CheckBinaryArity(string name, LispObject[] args)
    {
        if (args.Length != 2)
            throw new LispErrorException(new LispProgramError($"{name}: wrong number of arguments: {args.Length} (expected 2)"));
    }

    /// <summary>Signal a PROGRAM-ERROR with the given message (callable from CIL).</summary>
    public static LispObject ProgramError(string message)
    {
        throw new LispErrorException(new LispProgramError(message));
    }

    public static void CheckArityExact(string name, LispObject[] args, int n)
    {
        if (args.Length != n)
        {
            // Debug: log details for the "3 expected 5" pattern
            throw new LispErrorException(new LispProgramError($"{name}: wrong number of arguments: {args.Length} (expected {n})"));
        }
    }

    public static void CheckArityMin(string name, LispObject[] args, int min)
    {
        if (args.Length < min)
            throw new LispErrorException(new LispProgramError($"{name}: too few arguments: {args.Length} (expected at least {min})"));
    }

    public static void CheckArityMax(string name, LispObject[] args, int max)
    {
        if (args.Length > max)
            throw new LispErrorException(new LispProgramError($"{name}: too many arguments: {args.Length} (expected at most {max})"));
    }

    public static void CheckNoUnknownKeys(string name, LispObject[] args, int keyStart, string[] validKeys)
    {
        CheckNoUnknownKeys2(name, args, keyStart, validKeys, null);
    }

    public static void CheckNoUnknownKeys2(string name, LispObject[] args, int keyStart, string[] validKeys, string[]? validKeyPackages)
    {
        // If fewer args than keyStart, no keyword args were passed — nothing to check
        if (args.Length <= keyStart) return;
        // Check for odd number of keyword arguments
        if ((args.Length - keyStart) % 2 != 0)
            throw new LispErrorException(new LispProgramError($"{name}: odd number of keyword arguments"));
        // First-wins: find the first :allow-other-keys occurrence to determine if unknown keys are suppressed
        bool allowOtherKeys = false;
        for (int i = keyStart; i + 1 < args.Length; i += 2)
        {
            if (args[i] is Symbol sym2 &&
                string.Equals(sym2.Name, "ALLOW-OTHER-KEYS", StringComparison.OrdinalIgnoreCase))
            {
                allowOtherKeys = args[i + 1] != Nil.Instance;
                break; // first wins
            }
        }
        if (allowOtherKeys) return;
        // Check each keyword argument
        for (int i = keyStart; i < args.Length; i += 2)
        {
            string? kname = null;
            string? kpkg = null;
            if (args[i] is Symbol sym)
            {
                kname = sym.Name;
                kpkg = sym.HomePackage?.Name;
            }
            else if (args[i] is LispString ls)
                kname = ls.Value;
            if (kname == null)
                throw new LispErrorException(new LispProgramError($"{name}: keyword argument must be a symbol, got {args[i]}"));
            // :allow-other-keys is always valid
            if (string.Equals(kname, "ALLOW-OTHER-KEYS", StringComparison.OrdinalIgnoreCase))
                continue;
            bool found = false;
            for (int j = 0; j < validKeys.Length; j++)
            {
                if (string.Equals(kname, validKeys[j], StringComparison.Ordinal))
                {
                    // If package info available, check package too
                    if (validKeyPackages != null && validKeyPackages[j] != null)
                    {
                        string expectedPkg = validKeyPackages[j];
                        if (expectedPkg == "" ? kpkg == null : string.Equals(kpkg, expectedPkg, StringComparison.Ordinal))
                        { found = true; break; }
                    }
                    else
                    {
                        // No package constraint — match by name only (legacy behavior for normal &key)
                        if (kpkg == "KEYWORD") { found = true; break; }
                    }
                }
            }
            if (!found)
                throw new LispErrorException(new LispProgramError($"{name}: unrecognized keyword argument :{kname}"));
        }
    }

    public static LispObject List(params LispObject[] args)
    {
        LispObject result = Nil.Instance;
        for (int i = args.Length - 1; i >= 0; i--)
            result = new Cons(args[i], result);
        return result;
    }

    public static LispObject ListStar(params LispObject[] args)
    {
        if (args.Length == 0) throw new ArgumentException("LIST* requires at least one argument");
        if (args.Length == 1) return args[0];
        LispObject result = args[^1];
        for (int i = args.Length - 2; i >= 0; i--)
            result = new Cons(args[i], result);
        return result;
    }

    public static LispObject Append(LispObject a, LispObject b)
    {
        if (a is Nil) return b;
        if (a is not Cons ca) throw new LispErrorException(new LispTypeError("APPEND: not a list", a));
        return new Cons(ca.Car, Append(ca.Cdr, b));
    }

    public static int ListLength(LispObject obj)
    {
        int len = 0;
        while (obj is Cons c) { len++; obj = c.Cdr; }
        return len;
    }

    public static LispObject Length(LispObject obj)
    {
        if (obj is Nil) return Fixnum.Make(0);
        if (obj is Cons)
        {
            int len = 0;
            var cur = obj;
            while (cur is Cons c) { len++; cur = c.Cdr; }
            if (cur is not Nil)
                throw new LispErrorException(new LispTypeError("LENGTH: not a proper sequence", obj));
            return Fixnum.Make(len);
        }
        if (obj is LispString s) return Fixnum.Make(s.Length);
        if (obj is LispVector v) return Fixnum.Make(v.Length);
        throw new LispErrorException(new LispTypeError("LENGTH: not a sequence", obj));
    }

    // --- Symbol operations ---

    public static LispObject SymbolName(LispObject obj)
    {
        obj = Primary(obj);
        if (obj is Symbol sym) return new LispString(sym.Name);
        if (obj is Nil) return new LispString("NIL");
        if (obj is T) return new LispString("T");
        throw new LispErrorException(new LispTypeError(
            $"SYMBOL-NAME: {obj} is not of type SYMBOL", obj, Startup.Sym("SYMBOL")));
    }

    public static LispObject SymbolPackage(LispObject obj)
    {
        obj = Primary(obj);
        if (obj is Symbol sym) return sym.HomePackage ?? (LispObject)Nil.Instance;
        if (obj is Nil) return Startup.CL;
        if (obj is T) return Startup.CL;
        throw new LispErrorException(new LispTypeError(
            $"SYMBOL-PACKAGE: {obj} is not of type SYMBOL", obj, Startup.Sym("SYMBOL")));
    }

    public static LispObject SetSymbolValue(LispObject sym, LispObject value)
    {
        var s = GetSymbol(sym, "SET");
        DynamicBindings.Set(s, value);
        return value;
    }

    public static LispObject Boundp(LispObject obj)
    {
        var sym = GetSymbol(obj, "BOUNDP");
        return DynamicBindings.TryGet(sym, out _) ? (LispObject)T.Instance : Nil.Instance;
    }

    public static LispObject SymbolValue(LispObject obj)
    {
        var sym = GetSymbol(obj, "SYMBOL-VALUE");
        if (DynamicBindings.TryGet(sym, out var val))
            return val;
        throw new LispErrorException(new LispUnboundVariable(sym));
    }

    public static LispObject Getenv(LispObject name)
    {
        var s = AsStringDesignator(name, "GETENV");
        var val = Environment.GetEnvironmentVariable(s);
        return val != null ? new LispString(val) : Nil.Instance;
    }

    // --- Property list operations ---

    internal static Symbol GetSymbol(LispObject obj, string fn)
    {
        if (obj is Symbol sym) return sym;
        if (obj is Nil) return Startup.NIL_SYM;
        if (obj is T) return Startup.T_SYM;
        throw new LispErrorException(new LispTypeError($"{fn}: not a symbol", obj));
    }

    public static LispObject GetProp(LispObject symbol, LispObject indicator, LispObject defaultValue)
    {
        var sym = GetSymbol(symbol, "GET");
        var plist = sym.Plist;
        while (plist is Cons c)
        {
            if (IsTrueEq(c.Car, indicator))
            {
                if (c.Cdr is Cons valCons) return MultipleValues.Primary(valCons.Car);
                return MultipleValues.Primary(defaultValue);
            }
            plist = c.Cdr is Cons rest ? rest.Cdr : Nil.Instance;
        }
        return MultipleValues.Primary(defaultValue);
    }

    public static LispObject PutProp(LispObject symbol, LispObject indicator, LispObject value)
    {
        var sym = GetSymbol(symbol, "SETF GET");
        var plist = sym.Plist;
        var current = plist;
        while (current is Cons c)
        {
            if (IsTrueEq(c.Car, indicator))
            {
                if (c.Cdr is Cons valCons) { valCons.Car = value; return value; }
            }
            current = c.Cdr is Cons rest ? rest.Cdr : Nil.Instance;
        }
        // Not found — prepend indicator + value
        sym.Plist = new Cons(indicator, new Cons(value, sym.Plist));
        return value;
    }

    public static LispObject Remprop(LispObject symbol, LispObject indicator)
    {
        var sym = GetSymbol(symbol, "REMPROP");
        var plist = sym.Plist;
        if (plist is Nil) return Nil.Instance;
        // Check first pair
        if (plist is Cons first && IsTrueEq(first.Car, indicator))
        {
            sym.Plist = first.Cdr is Cons rest ? rest.Cdr : Nil.Instance;
            return DotCL.T.Instance;
        }
        // Check rest
        var prev = plist;
        while (prev is Cons pc && pc.Cdr is Cons valCons && valCons.Cdr is Cons next)
        {
            if (IsTrueEq(next.Car, indicator))
            {
                valCons.Cdr = next.Cdr is Cons nv ? nv.Cdr : Nil.Instance;
                return DotCL.T.Instance;
            }
            prev = next;
        }
        return Nil.Instance;
    }

    public static LispObject CopySymbol(LispObject obj) => CopySymbolFull(obj, Nil.Instance);
    public static LispObject CopySymbolFull(LispObject obj, LispObject copyProperties)
    {
        var sym = GetSymbol(obj, "COPY-SYMBOL");
        var newSym = new Symbol(sym.Name);
        if (IsTruthy(copyProperties))
        {
            if (sym.IsBound) newSym.Value = sym.Value;
            if (sym.Function != null) newSym.Function = sym.Function;
            newSym.Plist = CopyList(sym.Plist);
        }
        return newSym;
    }

    // --- String operations ---

    // Parse :start/:end/:allow-other-keys for string case functions; validate keyword args.
    // Returns (str, start, end). Throws program-error on bad args.
    private static (string str, int start, int end) ParseStringCaseArgs(LispObject[] args, string fname)
    {
        if (args.Length == 0)
            throw new LispErrorException(new LispProgramError($"{fname}: wrong number of arguments: 0"));
        var str = ToStringDesignator(args[0], fname);
        int strLen = str.Length;
        int start = 0, end = strLen;
        if ((args.Length - 1) % 2 != 0)
            throw new LispErrorException(new LispProgramError($"{fname}: odd number of keyword arguments"));
        bool? allowOtherKeys = null;
        bool hasUnknown = false;
        for (int i = 1; i < args.Length; i += 2)
        {
            if (args[i] is not Symbol kwSym)
                throw new LispErrorException(new LispProgramError($"{fname}: not a keyword: {args[i]}"));
            var kwName = kwSym.Name;
            var val = args[i + 1];
            switch (kwName)
            {
                case "START":
                    start = val is Fixnum fs ? (int)fs.Value : val is Nil ? 0 : throw new LispErrorException(new LispProgramError($"{fname}: :start must be an integer"));
                    break;
                case "END":
                    end = val is Fixnum fe ? (int)fe.Value : val is Nil ? strLen : throw new LispErrorException(new LispProgramError($"{fname}: :end must be an integer or nil"));
                    break;
                case "ALLOW-OTHER-KEYS":
                    if (allowOtherKeys == null) allowOtherKeys = IsTruthy(val);
                    break;
                default:
                    hasUnknown = true;
                    break;
            }
        }
        if (hasUnknown && allowOtherKeys != true)
            throw new LispErrorException(new LispProgramError($"{fname}: unknown keyword argument"));
        return (str, start, end);
    }

    public static LispObject StringUpcase(LispObject[] args)
    {
        var (str, start, end) = ParseStringCaseArgs(args, "STRING-UPCASE");
        var chars = str.ToCharArray();
        for (int i = start; i < end; i++) chars[i] = char.ToUpperInvariant(chars[i]);
        return new LispString(chars);
    }

    public static LispObject StringDowncase(LispObject[] args)
    {
        var (str, start, end) = ParseStringCaseArgs(args, "STRING-DOWNCASE");
        var chars = str.ToCharArray();
        for (int i = start; i < end; i++) chars[i] = char.ToLowerInvariant(chars[i]);
        return new LispString(chars);
    }

    public static LispObject StringCapitalize(LispObject[] args)
    {
        var (str, start, end) = ParseStringCaseArgs(args, "STRING-CAPITALIZE");
        var chars = str.ToCharArray();
        bool wordBoundary = true;
        for (int i = start; i < end; i++)
        {
            char c = chars[i];
            if (char.IsLetter(c))
            {
                chars[i] = wordBoundary ? char.ToUpperInvariant(c) : char.ToLowerInvariant(c);
                wordBoundary = false;
            }
            else if (char.IsDigit(c)) wordBoundary = false;
            else wordBoundary = true;
        }
        return new LispString(chars);
    }

    // Destructive in-place string case functions (NSTRING-*)
    private static (int start, int end) ParseStartEnd(LispObject[] args, int strLen)
    {
        int start = 0, end = strLen;
        for (int i = 1; i < args.Length - 1; i += 2)
        {
            if (args[i] is Symbol kw)
            {
                switch (kw.Name)
                {
                    case "START": if (args[i+1] is Fixnum fs) start = (int)fs.Value; break;
                    case "END": if (args[i+1] is Fixnum fe) end = (int)fe.Value; break;
                }
            }
        }
        return (start, end);
    }

    public static LispObject NStringUpcase(LispObject[] args)
    {
        var (_, start, end) = ParseStringCaseArgs(args, "NSTRING-UPCASE");
        if (args[0] is LispString str) { str.ToUpperInPlace(start, end); return str; }
        if (args[0] is LispVector vec && vec.IsCharVector) { vec.ToUpperInPlace(start, end); return vec; }
        throw new LispErrorException(new LispTypeError("NSTRING-UPCASE: not a string", args[0]));
    }

    public static LispObject NStringDowncase(LispObject[] args)
    {
        var (_, start, end) = ParseStringCaseArgs(args, "NSTRING-DOWNCASE");
        if (args[0] is LispString str) { str.ToLowerInPlace(start, end); return str; }
        if (args[0] is LispVector vec && vec.IsCharVector) { vec.ToLowerInPlace(start, end); return vec; }
        throw new LispErrorException(new LispTypeError("NSTRING-DOWNCASE: not a string", args[0]));
    }

    public static LispObject NStringCapitalize(LispObject[] args)
    {
        var (_, start, end) = ParseStringCaseArgs(args, "NSTRING-CAPITALIZE");
        if (args[0] is LispString str) { str.ToCapitalizeInPlace(start, end); return str; }
        if (args[0] is LispVector vec && vec.IsCharVector) { vec.ToCapitalizeInPlace(start, end); return vec; }
        throw new LispErrorException(new LispTypeError("NSTRING-CAPITALIZE: not a string", args[0]));
    }

    private static string ToStringDesignator(LispObject obj, string fname)
    {
        if (obj is LispString s) return s.Value;
        if (obj is LispVector v && v.IsCharVector) return v.ToCharString();
        if (obj is Symbol sym) return sym.Name;
        if (obj is Nil) return "NIL";   // NIL is a symbol with name "NIL"
        if (obj is T) return "T";       // T is a symbol with name "T"
        if (obj is LispChar c) return c.Value.ToString();
        throw new LispErrorException(new LispTypeError($"{fname}: not a string designator", obj));
    }

    public static LispObject StringTrim(LispObject charBag, LispObject obj)
    {
        var chars = GetTrimChars(charBag);
        return new LispString(ToStringDesignator(obj, "STRING-TRIM").Trim(chars));
    }

    public static LispObject StringLeftTrim(LispObject charBag, LispObject obj)
    {
        var chars = GetTrimChars(charBag);
        return new LispString(ToStringDesignator(obj, "STRING-LEFT-TRIM").TrimStart(chars));
    }

    public static LispObject StringRightTrim(LispObject charBag, LispObject obj)
    {
        var chars = GetTrimChars(charBag);
        return new LispString(ToStringDesignator(obj, "STRING-RIGHT-TRIM").TrimEnd(chars));
    }

    private static char[] GetTrimChars(LispObject charBag)
    {
        if (charBag is LispString s)
            return s.Value.ToCharArray();
        if (charBag is LispVector vec)
        {
            var chars = new char[vec.Length];
            for (int i = 0; i < vec.Length; i++)
                chars[i] = vec[i] is LispChar c ? c.Value : '\0';
            return chars;
        }
        // List of characters
        var list = new System.Collections.Generic.List<char>();
        var cur = charBag;
        while (cur is Cons cc)
        {
            if (cc.Car is LispChar c) list.Add(c.Value);
            cur = cc.Cdr;
        }
        return list.ToArray();
    }

    public static LispObject MakeString(LispObject size, LispObject initChar)
    {
        int len = size is Fixnum f ? (int)f.Value : 0;
        char ch = initChar is LispChar c ? c.Value : '\0';
        return new LispString(new string(ch, len));
    }

    /// <summary>Convert a Lisp list to a C# List&lt;LispObject&gt;.</summary>
    public static System.Collections.Generic.List<LispObject> ToList(LispObject obj)
    {
        var result = new System.Collections.Generic.List<LispObject>();
        while (obj is Cons c) { result.Add(c.Car); obj = c.Cdr; }
        return result;
    }

    public static LispObject Fill(LispObject[] args)
    {
        // (fill sequence item &key start end)
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("FILL: too few arguments"));
        var seq = args[0];
        var item = args[1];
        int start = 0;
        int end = -1;
        int kwCount = args.Length - 2;
        if (kwCount % 2 != 0)
            throw new LispErrorException(new LispProgramError("FILL: odd number of keyword arguments"));
        bool startSet = false, endSet = false;
        bool? allowOtherKeys = null;
        bool hasUnknown = false;
        // First pass: find :allow-other-keys
        for (int i = 2; i < args.Length - 1; i += 2)
        {
            if (args[i] is Symbol kw0 && kw0.Name == "ALLOW-OTHER-KEYS" && allowOtherKeys == null)
                allowOtherKeys = IsTruthy(args[i + 1]);
        }
        for (int i = 2; i < args.Length - 1; i += 2)
        {
            if (args[i] is not Symbol kw)
                throw new LispErrorException(new LispProgramError($"FILL: keyword argument must be a symbol, got {args[i]}"));
            switch (kw.Name)
            {
                case "START":
                    if (!startSet)
                    {
                        if (args[i + 1] is not Fixnum sf)
                            throw new LispErrorException(new LispTypeError("FILL: :START must be a non-negative integer", args[i + 1]));
                        start = (int)sf.Value;
                        if (start < 0)
                            throw new LispErrorException(new LispTypeError("FILL: :START must be a non-negative integer", args[i + 1]));
                        startSet = true;
                    }
                    break;
                case "END":
                    if (!endSet)
                    {
                        if (args[i + 1] is Nil) { /* nil means use length */ }
                        else if (args[i + 1] is Fixnum ef)
                        {
                            end = (int)ef.Value;
                            if (end < 0)
                                throw new LispErrorException(new LispTypeError("FILL: :END must be a non-negative integer or NIL", args[i + 1]));
                        }
                        else
                            throw new LispErrorException(new LispTypeError("FILL: :END must be a non-negative integer or NIL", args[i + 1]));
                        endSet = true;
                    }
                    break;
                case "ALLOW-OTHER-KEYS": break;
                default: hasUnknown = true; break;
            }
        }
        if (hasUnknown && allowOtherKeys != true)
            throw new LispErrorException(new LispProgramError("FILL: unknown keyword argument"));
        if (seq is LispString ls)
        {
            int e = end >= 0 ? end : ls.Length;
            char ch = item is LispChar lc ? lc.Value : throw new LispErrorException(new LispTypeError("FILL: not a character", item));
            Array.Fill(ls.RawChars, ch, start, e - start);
            return seq;
        }
        if (seq is LispVector v)
        {
            int e = end >= 0 ? end : v.Length;
            for (int i = start; i < e; i++) v[i] = item;
            return seq;
        }
        if (seq is Cons || seq is Nil)
        {
            int e = end >= 0 ? end : Length(seq) is Fixnum fl ? (int)fl.Value : 0;
            LispObject cur = seq;
            for (int i = 0; i < start && cur is Cons c1; i++) cur = c1.Cdr;
            for (int i = start; i < e && cur is Cons c2; i++) { c2.Car = item; cur = c2.Cdr; }
            return seq;
        }
        throw new LispErrorException(new LispTypeError("FILL: not a sequence", seq));
    }

    public static LispObject Replace(LispObject[] args)
    {
        // (replace target source &key start1 end1 start2 end2)
        var target = args[0];
        var source = args[1];
        int keyCount = args.Length - 2;
        // Validate keyword args: odd count, non-symbol key, unknown key, :allow-other-keys nil
        if (keyCount % 2 != 0)
            throw new LispErrorException(new LispProgramError("REPLACE: odd number of keyword arguments"));
        bool allowOtherKeys = false;
        // First pass: check :allow-other-keys
        for (int i = 2; i < args.Length - 1; i += 2)
        {
            if (args[i] is not Symbol kw2)
                throw new LispErrorException(new LispProgramError($"REPLACE: keyword argument must be a symbol, got {args[i]}"));
            if (kw2.Name == "ALLOW-OTHER-KEYS" && args[i + 1] != Nil.Instance)
                allowOtherKeys = true;
        }
        int start1 = 0, start2 = 0;
        int end1 = -1, end2 = -1;
        bool s1Set = false, s2Set = false, e1Set = false, e2Set = false;
        // Use first-wins for duplicate keyword args
        for (int i = 2; i < args.Length - 1; i += 2)
        {
            if (args[i] is not Symbol kw)
                throw new LispErrorException(new LispProgramError($"REPLACE: keyword argument must be a symbol, got {args[i]}"));
            switch (kw.Name)
            {
                case "START1": if (!s1Set) { start1 = (int)((Fixnum)args[i + 1]).Value; s1Set = true; } break;
                case "END1": if (!e1Set && args[i + 1] is Fixnum f1) { end1 = (int)f1.Value; e1Set = true; } break;
                case "START2": if (!s2Set) { start2 = (int)((Fixnum)args[i + 1]).Value; s2Set = true; } break;
                case "END2": if (!e2Set && args[i + 1] is Fixnum f2) { end2 = (int)f2.Value; e2Set = true; } break;
                case "ALLOW-OTHER-KEYS": break; // handled
                default:
                    if (!allowOtherKeys)
                        throw new LispErrorException(new LispProgramError($"REPLACE: unknown keyword argument :{kw.Name}"));
                    break;
            }
        }
        int len1 = ReplaceSeqLength(target);
        int len2 = ReplaceSeqLength(source);
        int e1 = end1 >= 0 ? end1 : len1;
        int e2 = end2 >= 0 ? end2 : len2;
        int copyLen = Math.Min(e1 - start1, e2 - start2);
        if (copyLen <= 0) return target;
        // Fast path: string-to-string copy using direct char access
        if (target is LispString ts && source is LispString ss)
        {
            Array.Copy(ss.RawChars, start2, ts.RawChars, start1, copyLen);
            return target;
        }
        // When target == source and ranges overlap, buffer source elements first to avoid clobbering
        bool selfOverlap = ReferenceEquals(target, source) &&
                           !(start1 + copyLen <= start2 || start2 + copyLen <= start1);
        if (selfOverlap)
        {
            var buf = new LispObject[copyLen];
            for (int i = 0; i < copyLen; i++) buf[i] = ReplaceSeqGet(source, start2 + i);
            for (int i = 0; i < copyLen; i++) ReplaceSeqSet(target, start1 + i, buf[i]);
        }
        else
        {
            for (int i = 0; i < copyLen; i++)
            {
                var elem = ReplaceSeqGet(source, start2 + i);
                ReplaceSeqSet(target, start1 + i, elem);
            }
        }
        return target;
    }

    private static int ReplaceSeqLength(LispObject seq) => seq switch
    {
        Nil => 0,
        Cons => ListLength(seq),
        LispString s => s.Length,
        LispVector v => v.Length,
        _ => throw new LispErrorException(new LispTypeError("REPLACE: not a sequence", seq))
    };

    private static LispObject ReplaceSeqGet(LispObject seq, int i)
    {
        if (seq is LispString s) return LispChar.Make(s[i]);
        if (seq is LispVector v) return v.ElementAt(i);
        // list
        LispObject cur = seq;
        for (int j = 0; j < i; j++)
            cur = cur is Cons c ? c.Cdr : Nil.Instance;
        return cur is Cons cc ? cc.Car : Nil.Instance;
    }

    private static void ReplaceSeqSet(LispObject seq, int i, LispObject value)
    {
        if (seq is LispString s && value is LispChar ch) { s[i] = ch.Value; return; }
        if (seq is LispVector v) { v.SetElement(i, value); return; }
        // list
        LispObject cur = seq;
        for (int j = 0; j < i; j++)
            cur = cur is Cons c ? c.Cdr : Nil.Instance;
        if (cur is Cons cc) cc.Car = value;
    }

    public static LispObject SetChar(LispObject str, LispObject index, LispObject ch)
    {
        if (index is Fixnum idx && ch is LispChar c)
        {
            if (str is LispString s) { s[(int)idx.Value] = c.Value; return ch; }
            if (str is LispVector v && v.IsCharVector) { v.SetElement((int)idx.Value, c); return ch; }
        }
        throw new LispErrorException(new LispTypeError("(SETF CHAR): invalid arguments", str));
    }

    public static LispObject SetElt(LispObject seq, LispObject index, LispObject value)
    {
        if (index is not Fixnum fi)
            throw new LispErrorException(new LispTypeError("(SETF ELT): index must be integer", index));
        long idxL = fi.Value;
        if (idxL < 0)
            throw new LispErrorException(new LispTypeError($"(SETF ELT): index {idxL} is negative", index));
        int idx = (int)idxL;
        if (seq is LispVector v)
        {
            if (idx >= v.Length) throw new LispErrorException(new LispTypeError($"(SETF ELT): index {idx} out of bounds for vector of length {v.Length}", index));
            v[idx] = value;
            return value;
        }
        if (seq is LispString s && value is LispChar c)
        {
            if (idx >= s.Length) throw new LispErrorException(new LispTypeError($"(SETF ELT): index {idx} out of bounds for string of length {s.Length}", index));
            s[idx] = c.Value;
            return value;
        }
        // List: walk to nth element
        if (seq is Cons || seq is Nil)
        {
            var cur = seq;
            for (int i = 0; i < idx; i++)
            {
                if (cur is not Cons cc) throw new LispErrorException(new LispTypeError($"(SETF ELT): index {idx} out of bounds for list", index));
                cur = cc.Cdr;
            }
            if (cur is Cons cn) { cn.Car = value; return value; }
            throw new LispErrorException(new LispTypeError($"(SETF ELT): index {idx} out of bounds for list", index));
        }
        throw new LispErrorException(new LispTypeError("(SETF ELT): not a sequence", seq));
    }

    // --- Character operations ---

    public static LispObject CharCode(LispObject obj)
    {
        if (obj is LispChar c) return Fixnum.Make(c.Value);
        throw new LispErrorException(new LispTypeError("CHAR-CODE: not a character", obj));
    }

    public static LispObject CodeChar(LispObject obj)
    {
        if (obj is Fixnum f && f.Value >= 0 && f.Value <= 0x10FFFF)
            return LispChar.Make((char)f.Value);
        throw new LispErrorException(new LispTypeError("CODE-CHAR: not a valid character code", obj));
    }

    public static LispObject DigitCharP(LispObject ch, LispObject radix)
    {
        if (ch is not LispChar c)
            throw new LispErrorException(new LispTypeError("DIGIT-CHAR-P: not a character", ch));
        int r = radix is Fixnum f ? (int)f.Value : 10;
        char chr = c.Value;
        int weight;
        if (chr >= '0' && chr <= '9')
            weight = chr - '0';
        else if (chr >= 'A' && chr <= 'Z')
            weight = chr - 'A' + 10;
        else if (chr >= 'a' && chr <= 'z')
            weight = chr - 'a' + 10;
        else
            return Nil.Instance;
        return weight < r ? Fixnum.Make(weight) : Nil.Instance;
    }

    public static LispObject ParseInteger(LispObject[] args)
    {
        // (parse-integer string &key :start :end :radix :junk-allowed)
        if (args.Length == 0)
            throw new LispErrorException(new LispProgramError("PARSE-INTEGER: wrong number of arguments"));
        string str;
        if (args[0] is LispString ls)
            str = ls.Value;
        else if (args[0] is LispVector vec && vec.IsCharVector)
        {
            int vlen = vec.Length;
            var sb = new System.Text.StringBuilder(vlen);
            for (int vi = 0; vi < vlen; vi++)
                sb.Append(((LispChar)vec[vi]).Value);
            str = sb.ToString();
        }
        else
            throw new LispErrorException(new LispTypeError("PARSE-INTEGER: not a string", args[0]));
        int start = 0, end = str.Length, radix = 10;
        int origLen = str.Length;
        bool junkAllowed = false;
        if ((args.Length - 1) % 2 != 0)
            throw new LispErrorException(new LispProgramError("PARSE-INTEGER: odd number of keyword arguments"));
        // CL spec: leftmost keyword occurrence wins
        bool seenStart = false, seenEnd = false, seenRadix = false, seenJunk = false;
        bool allowOtherKeys = false; bool seenAllowOtherKeys = false;
        string? unknownKey = null;
        for (int j = 1; j < args.Length; j += 2)
        {
            string key = args[j] is Symbol s ? s.Name : "";
            switch (key)
            {
                case "START":
                    if (!seenStart) { start = args[j + 1] is Fixnum sf ? (int)sf.Value : 0; seenStart = true; }
                    break;
                case "END":
                    if (!seenEnd) {
                        if (args[j + 1] is Nil) end = origLen;
                        else if (args[j + 1] is Fixnum ef) end = (int)ef.Value;
                        else end = origLen;
                        seenEnd = true;
                    }
                    break;
                case "RADIX":
                    if (!seenRadix) { radix = args[j + 1] is Fixnum rf ? (int)rf.Value : 10; seenRadix = true; }
                    break;
                case "JUNK-ALLOWED":
                    if (!seenJunk) { junkAllowed = args[j + 1] is not Nil; seenJunk = true; }
                    break;
                case "ALLOW-OTHER-KEYS":
                    if (!seenAllowOtherKeys) { allowOtherKeys = args[j + 1] is not Nil; seenAllowOtherKeys = true; }
                    break;
                default:
                    if (unknownKey == null) unknownKey = key;
                    break;
            }
        }
        if (unknownKey != null && !allowOtherKeys)
            throw new LispErrorException(new LispProgramError($"PARSE-INTEGER: unrecognized keyword :{unknownKey}"));
        // Skip leading whitespace
        int i = start;
        while (i < end && char.IsWhiteSpace(str[i])) i++;
        if (i >= end) {
            if (junkAllowed) return MultipleValues.Values(Nil.Instance, Fixnum.Make(i));
            throw new LispErrorException(new LispError($"PARSE-INTEGER: no integer in substring") { ConditionTypeName = "PARSE-ERROR" });
        }
        bool negative = false;
        if (str[i] == '-') { negative = true; i++; }
        else if (str[i] == '+') { i++; }
        System.Numerics.BigInteger result = 0;
        bool hasDigits = false;
        while (i < end)
        {
            char c = str[i];
            int digit;
            if (c >= '0' && c <= '9') digit = c - '0';
            else if (c >= 'A' && c <= 'Z') digit = c - 'A' + 10;
            else if (c >= 'a' && c <= 'z') digit = c - 'a' + 10;
            else break;
            if (digit >= radix) break;
            result = result * radix + digit;
            hasDigits = true;
            i++;
        }
        if (!hasDigits)
        {
            if (junkAllowed) return MultipleValues.Values(Nil.Instance, Fixnum.Make(i));
            throw new LispErrorException(new LispError($"PARSE-INTEGER: no integer in substring") { ConditionTypeName = "PARSE-ERROR" });
        }
        // Skip trailing whitespace
        while (i < end && char.IsWhiteSpace(str[i])) i++;
        if (i < end && !junkAllowed)
            throw new LispErrorException(new LispError($"PARSE-INTEGER: junk in string at position {i}") { ConditionTypeName = "PARSE-ERROR" });
        if (negative) result = -result;
        return MultipleValues.Values(MakeInteger(result), Fixnum.Make(i));
    }

    // --- Hash table operations ---

    public static LispObject MakeHashTable(LispObject test)
    {
        string testName = test switch
        {
            Symbol sym => sym.Name,
            LispFunction fn => fn.Name ?? "EQL",
            _ => "EQL"
        };
        return new LispHashTable(testName);
    }

    public static LispObject MakeHashTableSync(LispObject test, bool synchronized)
    {
        string testName = test switch
        {
            Symbol sym => sym.Name,
            LispFunction fn => fn.Name ?? "EQL",
            _ => "EQL"
        };
        return new LispHashTable(testName, synchronized);
    }

    public static LispObject MakeHashTable0()
    {
        return new LispHashTable("EQL");
    }

    public static LispObject Gethash(LispObject key, LispObject table, LispObject? defaultValue = null)
    {
        if (table is not LispHashTable ht)
            throw new LispErrorException(new LispTypeError("GETHASH: not a hash-table", table));
        if (ht.TryGet(key, out var value))
            return MultipleValues.Values(value, T.Instance);
        return MultipleValues.Values(defaultValue ?? Nil.Instance, Nil.Instance);
    }

    public static LispObject HashTablePairs(LispObject table)
    {
        if (table is not LispHashTable ht)
            throw new LispErrorException(new LispTypeError("HASH-TABLE-PAIRS: not a hash-table", table));
        LispObject result = Nil.Instance;
        foreach (var kvp in ht.Entries)
            result = new Cons(new Cons(kvp.Key, kvp.Value), result);
        return result;
    }

    public static LispObject Puthash(LispObject key, LispObject table, LispObject value)
    {
        if (table is not LispHashTable ht)
            throw new LispErrorException(new LispTypeError("PUTHASH: not a hash-table", table));
        ht.Set(key, value);
        return value;
    }

    public static LispObject Remhash(LispObject key, LispObject table)
    {
        if (table is not LispHashTable ht)
            throw new LispErrorException(new LispTypeError("REMHASH: not a hash-table", table));
        return ht.Remove(key) ? (LispObject)T.Instance : Nil.Instance;
    }

    public static LispObject Clrhash(LispObject table)
    {
        if (table is not LispHashTable ht)
            throw new LispErrorException(new LispTypeError("CLRHASH: not a hash-table", table));
        ht.Clear();
        return table;
    }

    // SXHASH: stable content-based hash, depth-limited for circular structures
    public static LispObject Sxhash(LispObject obj) =>
        Fixnum.Make(SxhashCompute(obj, 5) & 0x3FFFFFFF);

    private static int SxhashCompute(LispObject obj, int depth)
    {
        if (depth == 0) return 0;
        return obj switch
        {
            Nil => 0,
            T => 1,
            Fixnum f => f.Value.GetHashCode(),
            LispChar ch => ch.Value.GetHashCode(),
            Symbol s => s.Name.GetHashCode(StringComparison.Ordinal),
            LispString ls => ls.Value.GetHashCode(StringComparison.Ordinal),
            LispVector lv when lv.IsCharVector => lv.ToCharString().GetHashCode(StringComparison.Ordinal),
            LispVector lv when lv.IsBitVector => SxhashBitVector(lv),
            Cons c => unchecked(SxhashCompute(c.Car, depth - 1) * 31 + SxhashCompute(c.Cdr, depth - 1)),
            LispPathname p => p.ToString().GetHashCode(StringComparison.OrdinalIgnoreCase),
            _ => 0,
        };
    }

    private static int SxhashBitVector(LispVector lv)
    {
        int h = 0;
        int len = lv.Length;
        for (int i = 0; i < len; i++)
            h = unchecked(h * 31 + (lv.GetElement(i) is Fixnum f ? (int)f.Value : 0));
        return h;
    }

    internal static void RegisterCollectionBuiltins()
    {
        // LIST, LIST*
        Emitter.CilAssembler.RegisterFunction("LIST",
            new LispFunction(args => Runtime.List(args)));
        Emitter.CilAssembler.RegisterFunction("LIST*",
            new LispFunction(args => Runtime.ListStar(args)));

        // COPY-LIST
        Startup.RegisterUnary("COPY-LIST", Runtime.CopyList);

        // COPY-SYMBOL
        Emitter.CilAssembler.RegisterFunction("COPY-SYMBOL", new LispFunction(args => {
            if (args.Length < 1 || args.Length > 2)
                throw new LispErrorException(new LispProgramError($"COPY-SYMBOL: wrong number of arguments: {args.Length}"));
            return Runtime.CopySymbolFull(args[0], args.Length == 2 ? args[1] : Nil.Instance);
        }));

        // BUTLAST
        Startup.RegisterUnary("BUTLAST", Runtime.Butlast);

        // VECTOR constructor: (vector &rest args) → simple-vector
        Emitter.CilAssembler.RegisterFunction("VECTOR", new LispFunction(
            args => new LispVector(args), "VECTOR", -1));

        // CONS, RPLACA, RPLACD, NTH, NTHCDR
        Startup.RegisterBinary("CONS", Runtime.MakeCons);
        Startup.RegisterBinary("RPLACA", Runtime.Rplaca);
        Startup.RegisterBinary("RPLACD", Runtime.Rplacd);
        Startup.RegisterBinary("NTH", Runtime.Nth);
        Startup.RegisterBinary("NTHCDR", Runtime.Nthcdr);

        // REMHASH, MAKE-HASH-TABLE
        Startup.RegisterBinary("REMHASH", Runtime.Remhash);
        Emitter.CilAssembler.RegisterFunction("MAKE-HASH-TABLE",
            new LispFunction(args => {
                if (args.Length == 0) return Runtime.MakeHashTable0();
                LispObject? testArg = null;
                bool synchronized = false;
                string? weakness = null;
                for (int i = 0; i < args.Length - 1; i += 2)
                {
                    if (args[i] is Symbol kw)
                    {
                        if (kw.Name == "TEST") testArg = args[i + 1];
                        else if (kw.Name == "SYNCHRONIZED") synchronized = args[i + 1] is not Nil;
                        else if (kw.Name == "WEAKNESS")
                        {
                            // Accept :value (SBCL extension); other modes
                            // throw at LispHashTable construction (#147).
                            if (args[i + 1] is Symbol ws) weakness = ws.Name;
                            else if (args[i + 1] is Nil) weakness = null;
                        }
                    }
                }
                string testName = testArg switch
                {
                    Symbol s => s.Name.ToUpperInvariant(),
                    LispString ls => ls.Value.ToUpperInvariant(),
                    LispFunction fn => (fn.Name ?? "EQL").ToUpperInvariant(),
                    null => "EQL",
                    _ => "EQL"
                };
                return new LispHashTable(testName, synchronized, weakness);
            }, "MAKE-HASH-TABLE", -1));

        // APPEND as variadic symbol-function
        Emitter.CilAssembler.RegisterFunction("APPEND",
            new LispFunction(args => {
                if (args.Length == 0) return Nil.Instance;
                if (args.Length == 1) return args[0];
                LispObject result = args[args.Length - 1];
                for (int i = args.Length - 2; i >= 0; i--)
                    result = Runtime.Append(args[i], result);
                return result;
            }));

        // SVREF
        Emitter.CilAssembler.RegisterFunction("SVREF",
            new LispFunction(args => {
                if (args.Length != 2)
                    throw new LispErrorException(new LispProgramError("SVREF: expected 2 arguments"));
                return Runtime.Aref(args[0], args[1]);
            }));
        Emitter.CilAssembler.RegisterFunction("(SETF SVREF)", new LispFunction(args => {
            if (args.Length != 3)
                throw new LispErrorException(new LispProgramError("(SETF SVREF): expected 3 arguments"));
            return Runtime.ArefSet(args[1], args[2], args[0]);
        }));

        // SYMBOL-PLIST, (SETF SYMBOL-PLIST)
        Emitter.CilAssembler.RegisterFunction("SYMBOL-PLIST",
            new LispFunction(args => {
                if (args.Length != 1)
                    throw new LispErrorException(new LispProgramError("SYMBOL-PLIST: expected 1 argument"));
                var sym = Runtime.GetSymbol(args[0], "SYMBOL-PLIST");
                return sym.Plist;
            }));
        Emitter.CilAssembler.RegisterFunction("(SETF SYMBOL-PLIST)", new LispFunction(args => {
            if (args.Length != 2)
                throw new LispErrorException(new LispProgramError("(SETF SYMBOL-PLIST): expected 2 arguments"));
            var sym = Runtime.GetSymbol(args[1], "(SETF SYMBOL-PLIST)");
            sym.Plist = args[0]; return args[0];
        }));

        // SBIT, (SETF SBIT)
        Emitter.CilAssembler.RegisterFunction("SBIT",
            new LispFunction(args => {
                if (args.Length < 1)
                    throw new LispErrorException(new LispProgramError("SBIT: too few arguments"));
                return Runtime.ArefMulti(args);
            }));
        Emitter.CilAssembler.RegisterFunction("(SETF SBIT)", new LispFunction(args => Runtime.ArefSetMulti(args)));

        // BIT, (SETF BIT)
        Emitter.CilAssembler.RegisterFunction("BIT",
            new LispFunction(args => {
                if (args.Length < 1)
                    throw new LispErrorException(new LispProgramError("BIT: too few arguments"));
                return Runtime.ArefMulti(args);
            }));
        Emitter.CilAssembler.RegisterFunction("(SETF BIT)", new LispFunction(args => Runtime.ArefSetMulti(args)));

        // MAKE-LIST
        Emitter.CilAssembler.RegisterFunction("MAKE-LIST",
            new LispFunction(args => {
                if (args.Length < 1)
                    throw new LispErrorException(new LispProgramError("MAKE-LIST: too few arguments"));
                if (args[0] is not Fixnum szf || szf.Value < 0)
                    throw new LispErrorException(new LispTypeError("MAKE-LIST: size must be a non-negative integer", args[0], Startup.Sym("UNSIGNED-BYTE")));
                int size = (int)szf.Value;
                int kwStart = 1;
                if ((args.Length - kwStart) % 2 != 0)
                    throw new LispErrorException(new LispProgramError("MAKE-LIST: odd number of keyword arguments"));
                // Check if :allow-other-keys t is present (first occurrence wins)
                bool allowOtherKeys = false;
                for (int i = kwStart; i < args.Length - 1; i += 2)
                    if (args[i] is Symbol aks && aks.Name == "ALLOW-OTHER-KEYS"
                        && !(args[i + 1] is Nil))
                    { allowOtherKeys = true; break; }
                // Validate keyword positions
                for (int i = kwStart; i < args.Length; i += 2)
                {
                    if (args[i] is not Symbol kws)
                        throw new LispErrorException(new LispProgramError($"MAKE-LIST: not a keyword: {args[i]}"));
                    if (!allowOtherKeys && kws.Name != "INITIAL-ELEMENT" && kws.Name != "ALLOW-OTHER-KEYS")
                        throw new LispErrorException(new LispProgramError($"MAKE-LIST: unknown keyword: :{kws.Name}"));
                }
                // Find :initial-element (first occurrence wins per CLHS)
                LispObject init = Nil.Instance;
                bool found = false;
                for (int i = kwStart; i < args.Length - 1; i += 2)
                    if (args[i] is Symbol kw2 && kw2.Name == "INITIAL-ELEMENT" && !found)
                    { init = args[i + 1]; found = true; }
                LispObject result = Nil.Instance;
                for (int i = 0; i < size; i++)
                    result = new Cons(init, result);
                return result;
            }, "MAKE-LIST", -1));

        // ARRAY-ELEMENT-TYPE, ARRAY-HAS-FILL-POINTER-P, ADJUSTABLE-ARRAY-P
        Startup.RegisterUnary("ARRAY-ELEMENT-TYPE", Runtime.ArrayElementType);
        Startup.RegisterUnary("ARRAY-HAS-FILL-POINTER-P", Runtime.ArrayHasFillPointerP);
        Startup.RegisterUnary("ADJUSTABLE-ARRAY-P", Runtime.AdjustableArrayP);

        // ARRAY-DISPLACEMENT
        Emitter.CilAssembler.RegisterFunction("ARRAY-DISPLACEMENT", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError($"ARRAY-DISPLACEMENT: wrong number of arguments: {args.Length} (expected 1)"));
            return Runtime.ArrayDisplacement(args[0]);
        }));

        // FILL-POINTER, %SET-FILL-POINTER
        Startup.RegisterUnary("FILL-POINTER", Runtime.FillPointer);
        Startup.RegisterBinary("%SET-FILL-POINTER", Runtime.SetFillPointer);

        // ARRAY-RANK
        Startup.RegisterUnary("ARRAY-RANK", obj => obj switch
        {
            LispVector v => Fixnum.Make(v.Rank),
            LispString => Fixnum.Make(1),
            _ => throw new LispErrorException(new LispTypeError("ARRAY-RANK: not an array", obj))
        });

        // ARRAY-TOTAL-SIZE
        Startup.RegisterUnary("ARRAY-TOTAL-SIZE", obj => obj switch
        {
            LispVector v => Fixnum.Make(v.Capacity),
            LispString s => Fixnum.Make(s.Length),
            _ => throw new LispErrorException(new LispTypeError("ARRAY-TOTAL-SIZE: not an array", obj))
        });

        // UPGRADED-ARRAY-ELEMENT-TYPE
        Emitter.CilAssembler.RegisterFunction("UPGRADED-ARRAY-ELEMENT-TYPE", new LispFunction(args => {
            if (args.Length < 1 || args.Length > 2) throw new LispErrorException(new LispProgramError($"UPGRADED-ARRAY-ELEMENT-TYPE: wrong number of arguments: {args.Length}"));
            var typeSpec = args[0];
            string name = typeSpec is Symbol s ? s.Name : typeSpec is T ? "T" : typeSpec is Nil ? "NIL" : "T";
            return Startup.Sym(name switch {
                "BIT" => "BIT",
                "CHARACTER" or "STANDARD-CHAR" => "CHARACTER",
                "BASE-CHAR" => "BASE-CHAR",  // keep BASE-CHAR distinct
                "NIL" => "NIL",
                _ => "T"
            });
        }));

        // ARRAY-DIMENSION
        Startup.RegisterBinary("ARRAY-DIMENSION", (arr, axisObj) => {
            int axis = axisObj is Fixnum fa ? (int)fa.Value : 0;
            if (arr is LispVector v) {
                var dims = v.Dimensions;
                if (axis < 0 || axis >= dims.Length)
                    throw new LispErrorException(new LispProgramError($"ARRAY-DIMENSION: axis {axis} out of range for rank-{dims.Length} array"));
                return Fixnum.Make(dims[axis]);
            }
            if (arr is LispString s) {
                if (axis != 0) throw new LispErrorException(new LispProgramError("ARRAY-DIMENSION: axis out of range"));
                return Fixnum.Make(s.Length);
            }
            throw new LispErrorException(new LispTypeError("ARRAY-DIMENSION: not an array", arr));
        });

        // ARRAY-DIMENSIONS
        Startup.RegisterUnary("ARRAY-DIMENSIONS", obj => {
            if (obj is LispVector v) {
                LispObject result = Nil.Instance;
                var dims = v.Dimensions;
                for (int i = dims.Length - 1; i >= 0; i--)
                    result = new Cons(Fixnum.Make(dims[i]), result);
                return result;
            }
            if (obj is LispString s) return new Cons(Fixnum.Make(s.Length), Nil.Instance);
            throw new LispErrorException(new LispTypeError("ARRAY-DIMENSIONS: not an array", obj));
        });

        // ARRAY-ROW-MAJOR-INDEX
        Emitter.CilAssembler.RegisterFunction("ARRAY-ROW-MAJOR-INDEX", new LispFunction(args => {
            if (args.Length < 1) throw new LispErrorException(new LispProgramError("ARRAY-ROW-MAJOR-INDEX: too few args"));
            var arr = args[0];
            int[] dims;
            if (arr is LispVector vArr) dims = vArr.Dimensions;
            else if (arr is LispString) dims = new[] { ((LispString)arr).Length };
            else throw new LispErrorException(new LispTypeError("ARRAY-ROW-MAJOR-INDEX: not an array", arr));
            if (args.Length - 1 != dims.Length)
                throw new LispErrorException(new LispProgramError($"ARRAY-ROW-MAJOR-INDEX: wrong number of subscripts"));
            int rowMajorIdx = 0;
            for (int i = 0; i < dims.Length; i++)
            {
                int sub = (int)((Fixnum)args[i + 1]).Value;
                rowMajorIdx = rowMajorIdx * dims[i] + sub;
            }
            return Fixnum.Make(rowMajorIdx);
        }));

        // ROW-MAJOR-AREF, %SET-ROW-MAJOR-AREF
        Emitter.CilAssembler.RegisterFunction("ROW-MAJOR-AREF", new LispFunction(args => {
            Runtime.CheckArityExact("ROW-MAJOR-AREF", args, 2);
            var arr = args[0]; var idx = (int)((Fixnum)args[1]).Value;
            if (arr is LispVector v2) return v2.GetElement(idx);
            if (arr is LispString s2) return LispChar.Make(s2[idx]);
            throw new LispErrorException(new LispTypeError("ROW-MAJOR-AREF: not an array", arr));
        }, "ROW-MAJOR-AREF", 2));
        Emitter.CilAssembler.RegisterFunction("%SET-ROW-MAJOR-AREF", new LispFunction(args => {
            Runtime.CheckArityExact("%SET-ROW-MAJOR-AREF", args, 3);
            var arr = args[0]; var idx = (int)((Fixnum)args[1]).Value; var val = args[2];
            if (arr is LispVector v2) { v2.SetElement(idx, val); return val; }
            throw new LispErrorException(new LispTypeError("(SETF ROW-MAJOR-AREF): not a vector", arr));
        }, "%SET-ROW-MAJOR-AREF", 3));

        // VECTOR-PUSH / VECTOR-PUSH-EXTEND / VECTOR-POP
        Emitter.CilAssembler.RegisterFunction("VECTOR-PUSH", new LispFunction(args => {
            if (args.Length < 2) throw new LispErrorException(new LispProgramError($"VECTOR-PUSH: wrong number of arguments: {args.Length} (expected 2)"));
            if (args.Length > 2) throw new LispErrorException(new LispProgramError($"VECTOR-PUSH: wrong number of arguments: {args.Length} (expected 2)"));
            if (args[1] is not LispVector vec)
                throw new LispErrorException(new LispTypeError("VECTOR-PUSH: not a vector", args[1]));
            return vec.VectorPushCL(args[0]);
        }));
        Emitter.CilAssembler.RegisterFunction("VECTOR-PUSH-EXTEND", new LispFunction(args => {
            if (args.Length < 2) throw new LispErrorException(new LispProgramError($"VECTOR-PUSH-EXTEND: wrong number of arguments: {args.Length} (expected 2-3)"));
            if (args.Length > 3) throw new LispErrorException(new LispProgramError($"VECTOR-PUSH-EXTEND: wrong number of arguments: {args.Length} (expected 2-3)"));
            if (args[1] is not LispVector vec)
                throw new LispErrorException(new LispTypeError("VECTOR-PUSH-EXTEND: not a vector", args[1]));
            int ext = args.Length >= 3 && args[2] is Fixnum fe ? (int)fe.Value : 0;
            return Fixnum.Make(vec.VectorPushExtend(args[0], ext));
        }));
        Emitter.CilAssembler.RegisterFunction("VECTOR-POP", new LispFunction(args => {
            if (args.Length < 1) throw new LispErrorException(new LispProgramError("VECTOR-POP: too few arguments"));
            if (args.Length > 1) throw new LispErrorException(new LispProgramError($"VECTOR-POP: too many arguments: {args.Length} (expected 1)"));
            if (args[0] is not LispVector vec)
                throw new LispErrorException(new LispTypeError("VECTOR-POP: not a vector with fill pointer", args[0]));
            if (!vec.HasFillPointer)
                throw new LispErrorException(new LispTypeError("VECTOR-POP: vector has no fill pointer", args[0]));
            if (vec.Length == 0)
                throw new LispErrorException(new LispError("VECTOR-POP: vector is empty"));
            int newLen = vec.Length - 1;
            vec.SetFillPointer(newLen);
            return vec.GetElement(newLen);
        }));

        // ARRAY-IN-BOUNDS-P
        Emitter.CilAssembler.RegisterFunction("ARRAY-IN-BOUNDS-P", new LispFunction(args => {
            if (args.Length < 1) throw new LispErrorException(new LispProgramError("ARRAY-IN-BOUNDS-P: too few args"));
            var arr = args[0];
            int[] dims;
            if (arr is LispVector vArr) dims = vArr.Dimensions;
            else if (arr is LispString sArr) dims = new[] { sArr.Length };
            else throw new LispErrorException(new LispTypeError("ARRAY-IN-BOUNDS-P: not an array", arr));
            int nSubs = args.Length - 1;
            if (nSubs != dims.Length) return Nil.Instance; // wrong number of subscripts
            for (int i = 0; i < dims.Length; i++)
            {
                // Bignum index is always out of bounds
                if (args[i + 1] is Bignum) return Nil.Instance;
                int sub = args[i + 1] is Fixnum fi ? (int)fi.Value : -1;
                if (sub < 0 || sub >= dims[i]) return Nil.Instance;
            }
            return T.Instance;
        }));

        // Bit array operations
        Emitter.CilAssembler.RegisterFunction("BIT-AND",   new LispFunction(args => Runtime.BitOp(args, "BIT-AND",   (a,b) => a & b)));
        Emitter.CilAssembler.RegisterFunction("BIT-IOR",   new LispFunction(args => Runtime.BitOp(args, "BIT-IOR",   (a,b) => a | b)));
        Emitter.CilAssembler.RegisterFunction("BIT-XOR",   new LispFunction(args => Runtime.BitOp(args, "BIT-XOR",   (a,b) => a ^ b)));
        Emitter.CilAssembler.RegisterFunction("BIT-EQV",   new LispFunction(args => Runtime.BitOp(args, "BIT-EQV",   (a,b) => ~(a ^ b))));
        Emitter.CilAssembler.RegisterFunction("BIT-NAND",  new LispFunction(args => Runtime.BitOp(args, "BIT-NAND",  (a,b) => ~(a & b))));
        Emitter.CilAssembler.RegisterFunction("BIT-NOR",   new LispFunction(args => Runtime.BitOp(args, "BIT-NOR",   (a,b) => ~(a | b))));
        Emitter.CilAssembler.RegisterFunction("BIT-ANDC1", new LispFunction(args => Runtime.BitOp(args, "BIT-ANDC1", (a,b) => (~a) & b)));
        Emitter.CilAssembler.RegisterFunction("BIT-ANDC2", new LispFunction(args => Runtime.BitOp(args, "BIT-ANDC2", (a,b) => a & (~b))));
        Emitter.CilAssembler.RegisterFunction("BIT-ORC1",  new LispFunction(args => Runtime.BitOp(args, "BIT-ORC1",  (a,b) => (~a) | b)));
        Emitter.CilAssembler.RegisterFunction("BIT-ORC2",  new LispFunction(args => Runtime.BitOp(args, "BIT-ORC2",  (a,b) => a | (~b))));
        Emitter.CilAssembler.RegisterFunction("BIT-NOT",   new LispFunction(args => Runtime.BitOpUnary(args, "BIT-NOT", a => ~a)));

        // (SETF AREF) / (SETF BIT) / (SETF SBIT) as function objects for apply
        Emitter.CilAssembler.RegisterFunction("(SETF AREF)", new LispFunction(args => {
            // Convention: (new-value array subscript1 subscript2 ...)
            if (args.Length < 2)
                throw new LispErrorException(new LispProgramError("(SETF AREF): requires value and array"));
            var value = args[0]; var array = args[1];
            // Reorder to ArefSetMulti convention: (array idx1 ... value)
            var reordered = new LispObject[args.Length];
            reordered[0] = array;
            for (int i = 0; i < args.Length - 2; i++) reordered[i + 1] = args[i + 2];
            reordered[args.Length - 1] = value;
            return Runtime.ArefSetMulti(reordered);
        }));
        Emitter.CilAssembler.RegisterFunction("(SETF BIT)", new LispFunction(args => {
            // Convention: (new-value bit-vector index)
            if (args.Length != 3)
                throw new LispErrorException(new LispProgramError("(SETF BIT): requires 3 args"));
            return Runtime.ArefSet(args[1], args[2], args[0]);
        }));
        Emitter.CilAssembler.RegisterFunction("(SETF SBIT)", new LispFunction(args => {
            if (args.Length != 3)
                throw new LispErrorException(new LispProgramError("(SETF SBIT): requires 3 args"));
            return Runtime.ArefSet(args[1], args[2], args[0]);
        }));

        // Hash-table accessors
        Startup.RegisterUnary("HASH-TABLE-P", Runtime.Hash_table_p);
        Startup.RegisterUnary("HASH-TABLE-COUNT", obj =>
            obj is LispHashTable ht ? Fixnum.Make(ht.Count)
            : throw new LispErrorException(new LispTypeError("HASH-TABLE-COUNT: not a hash-table", obj)));
        Startup.RegisterUnary("HASH-TABLE-SIZE", obj =>
            obj is LispHashTable ht2 ? Fixnum.Make(Math.Max(ht2.Count, 16))
            : throw new LispErrorException(new LispTypeError("HASH-TABLE-SIZE: not a hash-table", obj)));
        Startup.RegisterUnary("HASH-TABLE-TEST", obj =>
            obj is LispHashTable ht3 ? Startup.Sym(ht3.TestName)
            : throw new LispErrorException(new LispTypeError("HASH-TABLE-TEST: not a hash-table", obj)));
        // SBCL extension: hash-table-weakness returns :value (if value-weak),
        // or NIL for strong tables. Other weakness modes are not yet supported.
        Startup.RegisterUnary("HASH-TABLE-WEAKNESS", obj =>
            obj is LispHashTable htw
                ? (htw.Weakness == ":VALUE" ? (LispObject)Startup.Keyword("VALUE") : Nil.Instance)
                : throw new LispErrorException(new LispTypeError("HASH-TABLE-WEAKNESS: not a hash-table", obj)));
        Startup.RegisterUnary("HASH-TABLE-REHASH-SIZE", obj =>
            obj is LispHashTable ? new SingleFloat(1.5f)
            : throw new LispErrorException(new LispTypeError("HASH-TABLE-REHASH-SIZE: not a hash-table", obj)));
        Startup.RegisterUnary("HASH-TABLE-REHASH-THRESHOLD", obj =>
            obj is LispHashTable ? new SingleFloat(1.0f)
            : throw new LispErrorException(new LispTypeError("HASH-TABLE-REHASH-THRESHOLD: not a hash-table", obj)));

        // SXHASH, CLRHASH
        Startup.RegisterUnary("SXHASH", Runtime.Sxhash);
        Startup.RegisterUnary("CLRHASH", Runtime.Clrhash);

        // GETF, %PUTF
        Emitter.CilAssembler.RegisterFunction("GETF",
            new LispFunction(args => {
                Runtime.CheckArityMin("GETF", args, 2);
                Runtime.CheckArityMax("GETF", args, 3);
                return Runtime.Getf(args[0], args[1], args.Length > 2 ? args[2] : Nil.Instance);
            }));
        Emitter.CilAssembler.RegisterFunction("%PUTF",
            new LispFunction(args => Runtime.Putf(args[0], args[1], args[2])));

        // GET, (SETF GET)
        Emitter.CilAssembler.RegisterFunction("GET",
            new LispFunction(args => {
                Runtime.CheckArityMin("GET", args, 2);
                Runtime.CheckArityMax("GET", args, 3);
                var sym = Runtime.GetSymbol(args[0], "GET");
                return Runtime.Getf(sym.Plist, args[1], args.Length > 2 ? args[2] : Nil.Instance);
            }));
        Emitter.CilAssembler.RegisterFunction("(SETF GET)", new LispFunction(args => {
            if (args.Length != 3)
                throw new LispErrorException(new LispProgramError("(SETF GET): expected 3 arguments (value sym indicator)"));
            var sym = Runtime.GetSymbol(args[1], "(SETF GET)");
            sym.Plist = Runtime.Putf(sym.Plist, args[2], args[0]);
            return args[0];
        }));

        // REMPROP
        Emitter.CilAssembler.RegisterFunction("REMPROP",
            new LispFunction(args => {
                Runtime.CheckArityExact("REMPROP", args, 2);
                var sym = Runtime.GetSymbol(args[0], "REMPROP");
                var indicator = args[1];
                bool found = false;
                LispObject result = Nil.Instance;
                LispObject? tail = sym.Plist;
                Cons? prev = null;
                while (tail is Cons pair && pair.Cdr is Cons vpair)
                {
                    if (Runtime.IsTrueEq(pair.Car, indicator))
                    {
                        found = true;
                        if (prev == null) sym.Plist = vpair.Cdr;
                        else prev.Cdr = vpair.Cdr;
                        break;
                    }
                    prev = vpair;
                    tail = vpair.Cdr;
                }
                return found ? T.Instance : Nil.Instance;
            }));

        // CHAR, SCHAR
        Startup.RegisterBinary("CHAR", Runtime.CharAccess);
        Startup.RegisterBinary("SCHAR", Runtime.CharAccess);

        // %SET-SUBSEQ (setf subseq)
        Emitter.CilAssembler.RegisterFunction("%SET-SUBSEQ",
            new LispFunction(args => {
                // args: target start end source
                var target = args[0];
                int start = (int)((Fixnum)args[1]).Value;
                int end = args[2] is Fixnum fe ? (int)fe.Value : -1;
                var source = args[3];
                if (target is LispString ts)
                {
                    int srcLen = source is LispString srcStr ? srcStr.Length
                        : source is LispVector srcVec ? srcVec.Length : 0;
                    int len = end >= 0 ? end - start : ts.Length - start;
                    for (int i = 0; i < len && i < srcLen; i++)
                    {
                        char c = source is LispString ss2 ? ss2[i]
                            : source is LispVector sv2 && sv2.GetElement(i) is LispChar lc2 ? lc2.Value : '\0';
                        ts[start + i] = c;
                    }
                }
                else if (target is LispVector tv)
                {
                    int srcLen = source is LispString srcStr2 ? srcStr2.Length
                        : source is LispVector srcVec2 ? srcVec2.Length : 0;
                    int len = end >= 0 ? end - start : tv.Length - start;
                    for (int i = 0; i < len && i < srcLen; i++)
                    {
                        LispObject elem = source is LispString ss3 ? LispChar.Make(ss3[i])
                            : source is LispVector sv3 ? sv3.GetElement(i) : Nil.Instance;
                        tv.SetElement(start + i, elem);
                    }
                }
                else if (target is Cons || target is Nil)
                {
                    // List target: walk and set elements
                    int targetLen = ListLength(target);
                    int e = end >= 0 ? end : targetLen;
                    int srcLen = source is Cons || source is Nil ? ListLength(source)
                        : source is LispVector sv4 ? sv4.Length
                        : source is LispString ss4 ? ss4.Length : 0;
                    for (int i = start, j = 0; i < e && j < srcLen; i++, j++)
                        Runtime.SetElt(target, Fixnum.Make(i), Runtime.Elt(source, Fixnum.Make(j)));
                }
                return source;
            }));

        // MAKE-ARRAY, ADJUST-ARRAY
        Emitter.CilAssembler.RegisterFunction("MAKE-ARRAY",
            new LispFunction(args => Runtime.MakeArray(args)));
        Emitter.CilAssembler.RegisterFunction("ADJUST-ARRAY",
            new LispFunction(args => Runtime.AdjustArray(args)));
    }


}
