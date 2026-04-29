// CType: Internal type representation for subtypep/typep.
// Type specifiers (Lisp forms) are parsed into CType objects once,
// then all comparisons are done on CType objects (no re-parsing).

namespace DotCL;

// ============================================================
// CType class hierarchy
// ============================================================

/// <summary>Base class for all internal type representations.</summary>
public abstract class CType : LispObject
{
    public override string ToString() => $"#<CTYPE {ToSpecifier()}>";
    public abstract string ToSpecifier();
}

/// <summary>Named built-in type: T, NIL, SYMBOL, INTEGER, NUMBER, etc.</summary>
public class NamedType : CType
{
    public string Name { get; }
    private NamedType(string name) { Name = name; }
    public override string ToSpecifier() => Name;

    // Singleton registry for built-in type names (ConcurrentDictionary for #83).
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<string, NamedType> _instances = new();
    public static NamedType Get(string name)
    {
        return _instances.GetOrAdd(name, n => new NamedType(n));
    }

    // Well-known singletons
    public static readonly NamedType T_TYPE = Get("T");
    public static readonly NamedType NIL_TYPE = Get("NIL");
}

/// <summary>Numeric type with optional interval bounds: (INTEGER 0 10), (FLOAT * 1.0), etc.</summary>
public class NumericType : CType
{
    public string NumClass { get; }     // "INTEGER", "RATIONAL", "REAL", "FLOAT", "SINGLE-FLOAT", etc.
    public LispObject? Low { get; }     // null = unbounded
    public LispObject? High { get; }    // null = unbounded
    public bool LowExclusiveP { get; }
    public bool HighExclusiveP { get; }

    public NumericType(string numClass, LispObject? low = null, bool lowExcl = false,
                       LispObject? high = null, bool highExcl = false)
    {
        NumClass = numClass;
        Low = low; High = high;
        LowExclusiveP = lowExcl; HighExclusiveP = highExcl;
    }

    public override string ToSpecifier()
    {
        if (Low == null && High == null) return NumClass;
        var l = Low == null ? "*" : (LowExclusiveP ? $"({Low})" : Low.ToString());
        var h = High == null ? "*" : (HighExclusiveP ? $"({High})" : High!.ToString());
        return $"({NumClass} {l} {h})";
    }
}

/// <summary>Array type: (ARRAY element-type dimensions), (SIMPLE-ARRAY ...), etc.</summary>
public class ArrayType : CType
{
    public CType ElementType { get; }   // element type (NamedType("*") for wildcard)
    public LispObject? Dimensions { get; }  // null=*, Fixnum=rank, Cons=dimension list
    public bool? SimpleP { get; }       // true=simple, false=not-simple, null=either

    public ArrayType(CType elementType, LispObject? dimensions = null, bool? simpleP = null)
    {
        ElementType = elementType; Dimensions = dimensions; SimpleP = simpleP;
    }

    public override string ToSpecifier()
    {
        var head = SimpleP == true ? "SIMPLE-ARRAY" : "ARRAY";
        var et = ElementType is NamedType nt && nt.Name == "*" ? "*" : ElementType.ToSpecifier();
        var dims = Dimensions?.ToString() ?? "*";
        return $"({head} {et} {dims})";
    }
}

/// <summary>Cons type: (CONS car-type cdr-type)</summary>
public class ConsType : CType
{
    public CType CarType { get; }
    public CType CdrType { get; }

    public ConsType(CType carType, CType cdrType) { CarType = carType; CdrType = cdrType; }

    public override string ToSpecifier() => $"(CONS {CarType.ToSpecifier()} {CdrType.ToSpecifier()})";
}

/// <summary>Union type: (OR type1 type2 ...)</summary>
public class UnionType : CType
{
    public CType[] Types { get; }
    public UnionType(CType[] types) { Types = types; }

    public override string ToSpecifier() =>
        $"(OR {string.Join(" ", Types.Select(t => t.ToSpecifier()))})";
}

/// <summary>Intersection type: (AND type1 type2 ...)</summary>
public class IntersectionType : CType
{
    public CType[] Types { get; }
    public IntersectionType(CType[] types) { Types = types; }

    public override string ToSpecifier() =>
        $"(AND {string.Join(" ", Types.Select(t => t.ToSpecifier()))})";
}

/// <summary>Negation type: (NOT type)</summary>
public class NegationType : CType
{
    public CType Inner { get; }
    public NegationType(CType inner) { Inner = inner; }

    public override string ToSpecifier() => $"(NOT {Inner.ToSpecifier()})";
}

/// <summary>Member type: (MEMBER obj1 obj2 ...) or (EQL obj)</summary>
public class MemberType : CType
{
    public LispObject[] Members { get; }
    public MemberType(LispObject[] members) { Members = members; }

    public override string ToSpecifier() =>
        Members.Length == 1
            ? $"(EQL {Members[0]})"
            : $"(MEMBER {string.Join(" ", Members.Select(m => m.ToString()))})";
}

/// <summary>Satisfies type: (SATISFIES predicate)</summary>
public class SatisfiesType : CType
{
    public Symbol Predicate { get; }
    public SatisfiesType(Symbol predicate) { Predicate = predicate; }

    public override string ToSpecifier() => $"(SATISFIES {Predicate.Name})";
}

/// <summary>CLOS class type: wraps a LispClass for structure-class or standard-class types.</summary>
public class ClassCType : CType
{
    public LispClass Class { get; }
    public ClassCType(LispClass cls) { Class = cls; }

    public override string ToSpecifier() => Class.Name.Name;
}

// ============================================================
// TypeParser: type specifier → CType
// ============================================================

public static class TypeParser
{
    // Cache: symbol → parsed CType (invalidated on deftype). ConcurrentDictionary for #83.
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<string, CType> _symbolCache = new();

    /// <summary>Clear cache for a specific type name (called when deftype is registered).</summary>
    public static void InvalidateCache(string name)
    {
        _symbolCache.TryRemove(name, out _);
    }

    /// <summary>Parse a Lisp type specifier into a CType.</summary>
    public static CType Parse(LispObject specifier) => Parse(specifier, null);

    private static CType Parse(LispObject specifier, HashSet<string>? expanding)
    {
        switch (specifier)
        {
            case T:
                return NamedType.T_TYPE;
            case Nil:
                return NamedType.NIL_TYPE;

            case Symbol sym:
                return ParseSymbol(sym, expanding);

            case Cons cons:
                return ParseCompound(cons, expanding);

            case LispClass cls:
                // Built-in type classes → NamedType for consistent comparison
                if (Runtime.IsBuiltinTypeName(cls.Name.Name))
                    return NamedType.Get(cls.Name.Name);
                return new ClassCType(cls);

            default:
                // Unknown specifier — treat as named type
                return NamedType.Get(specifier.ToString() ?? "?");
        }
    }

    private static CType ParseSymbol(Symbol sym, HashSet<string>? expanding)
    {
        var name = sym.Name;

        // Check cache
        if (_symbolCache.TryGetValue(name, out var cached))
            return cached;

        // Built-in type names — return singleton NamedType
        if (Runtime.IsBuiltinTypeName(name) || name == "T" || name == "NIL" || name == "*")
        {
            var nt = NamedType.Get(name);
            _symbolCache[name] = nt;
            return nt;
        }

        // Check for deftype expander
        if (Runtime.TypeExpanders.TryGetValue(name, out var expander))
        {
            // Circular expansion detection
            expanding ??= new HashSet<string>();
            if (!expanding.Add(name))
            {
                // Circular — return as named type (best effort)
                var nt = NamedType.Get(name);
                _symbolCache[name] = nt;
                return nt;
            }

            try
            {
                var expanded = Runtime.Funcall(expander);
                // Self-referential guard
                if (expanded is Symbol rs && rs.Name == name)
                {
                    var nt = NamedType.Get(name);
                    _symbolCache[name] = nt;
                    return nt;
                }
                var result = Parse(expanded, expanding);
                _symbolCache[name] = result;
                return result;
            }
            catch
            {
                // Expansion failed — treat as named type
                var nt = NamedType.Get(name);
                _symbolCache[name] = nt;
                return nt;
            }
            finally
            {
                expanding.Remove(name);
            }
        }

        // Check CLOS class registry
        var cls = Runtime.FindClassByName(name);
        if (cls != null)
        {
            var ct = new ClassCType(cls);
            _symbolCache[name] = ct;
            return ct;
        }

        // Unknown type — keep as NamedType
        var named = NamedType.Get(name);
        _symbolCache[name] = named;
        return named;
    }

    private static CType ParseCompound(Cons cons, HashSet<string>? expanding)
    {
        if (cons.Car is not Symbol head)
            return NamedType.Get(cons.ToString() ?? "?");

        var headName = head.Name;

        switch (headName)
        {
            case "OR":
                return ParseUnion(cons.Cdr, expanding);
            case "AND":
                return ParseIntersection(cons.Cdr, expanding);
            case "NOT":
                return ParseNegation(cons.Cdr, expanding);
            case "MEMBER":
                return ParseMember(cons.Cdr);
            case "EQL":
                return ParseEql(cons.Cdr);
            case "SATISFIES":
                return ParseSatisfies(cons.Cdr);
            case "CONS":
                return ParseCons(cons.Cdr, expanding);

            // Numeric interval types
            case "INTEGER" or "RATIONAL" or "REAL" or "NUMBER"
                or "FLOAT" or "SINGLE-FLOAT" or "DOUBLE-FLOAT"
                or "SHORT-FLOAT" or "LONG-FLOAT":
                return ParseNumeric(headName, cons.Cdr);

            // Unsigned/signed byte
            case "UNSIGNED-BYTE" or "SIGNED-BYTE" or "MOD":
                return ParseByteMod(headName, cons.Cdr);

            // Array types
            case "ARRAY" or "SIMPLE-ARRAY":
                return ParseArray(headName == "SIMPLE-ARRAY", cons.Cdr, expanding);
            case "VECTOR" or "SIMPLE-VECTOR":
                return ParseVector(headName == "SIMPLE-VECTOR", cons.Cdr, expanding);
            case "STRING" or "SIMPLE-STRING" or "BASE-STRING" or "SIMPLE-BASE-STRING"
                or "BIT-VECTOR" or "SIMPLE-BIT-VECTOR":
                return ParseSpecializedVector(headName, cons.Cdr);

            // Complex compound types: throw to bypass CType and fall through to
            // manual subtypep which handles upgraded-complex-part-type correctly
            case "COMPLEX":
                throw new NotSupportedException("complex compound type");

            // Function / Values — stub for now
            case "FUNCTION" or "VALUES":
                return NamedType.Get(headName);

            default:
                // Might be a compound deftype: (my-type arg1 arg2)
                return ParseCompoundDeftype(head, cons.Cdr, expanding);
        }
    }

    // --- Compound type parsers ---

    private static CType ParseUnion(LispObject? args, HashSet<string>? expanding)
    {
        var types = CollectArgs(args).Select(a => Parse(a, expanding)).ToArray();
        if (types.Length == 0) return NamedType.NIL_TYPE;
        if (types.Length == 1) return types[0];
        return new UnionType(types);
    }

    private static CType ParseIntersection(LispObject? args, HashSet<string>? expanding)
    {
        var types = CollectArgs(args).Select(a => Parse(a, expanding)).ToArray();
        if (types.Length == 0) return NamedType.T_TYPE;
        if (types.Length == 1) return types[0];
        return new IntersectionType(types);
    }

    private static CType ParseNegation(LispObject? args, HashSet<string>? expanding)
    {
        if (args is not Cons c) return NamedType.T_TYPE;
        return new NegationType(Parse(c.Car, expanding));
    }

    private static CType ParseMember(LispObject? args)
    {
        var members = CollectArgs(args).ToArray();
        if (members.Length == 0) return NamedType.NIL_TYPE;
        return new MemberType(members);
    }

    private static CType ParseEql(LispObject? args)
    {
        if (args is not Cons c) return NamedType.NIL_TYPE;
        return new MemberType(new[] { c.Car });
    }

    private static CType ParseSatisfies(LispObject? args)
    {
        if (args is Cons c && c.Car is Symbol pred)
            return new SatisfiesType(pred);
        return NamedType.T_TYPE;
    }

    private static CType ParseCons(LispObject? args, HashSet<string>? expanding)
    {
        CType carType = NamedType.T_TYPE;
        CType cdrType = NamedType.T_TYPE;
        if (args is Cons c1)
        {
            if (c1.Car is not Symbol s1 || s1.Name != "*")
                carType = Parse(c1.Car, expanding);
            if (c1.Cdr is Cons c2 && !(c2.Car is Symbol s2 && s2.Name == "*"))
                cdrType = Parse(c2.Car, expanding);
        }
        return new ConsType(carType, cdrType);
    }

    // --- Numeric types ---

    private static CType ParseNumeric(string numClass, LispObject? args)
    {
        LispObject? low = null, high = null;
        bool lowExcl = false, highExcl = false;

        if (args is Cons c1)
        {
            ParseBound(c1.Car, out low, out lowExcl);
            if (c1.Cdr is Cons c2)
                ParseBound(c2.Car, out high, out highExcl);
        }

        // If no bounds, just return NamedType (e.g. bare INTEGER)
        if (low == null && high == null) return NamedType.Get(numClass);

        return new NumericType(numClass, low, lowExcl, high, highExcl);
    }

    private static void ParseBound(LispObject bound, out LispObject? value, out bool exclusive)
    {
        if (bound is Symbol s && s.Name == "*")
        {
            value = null; exclusive = false;
        }
        else if (bound is Cons bc && bc.Car is LispObject inner && bc.Cdr is Nil)
        {
            // Exclusive bound: (n)
            value = inner; exclusive = true;
        }
        else
        {
            value = bound; exclusive = false;
        }
    }

    private static CType ParseByteMod(string headName, LispObject? args)
    {
        // (UNSIGNED-BYTE n) → (INTEGER 0 (2^n - 1))
        // (SIGNED-BYTE n) → (INTEGER -(2^(n-1)) (2^(n-1) - 1))
        // (MOD n) → (INTEGER 0 (n - 1))
        if (args is not Cons c || c.Car is Symbol ws && ws.Name == "*")
            return NamedType.Get(headName == "MOD" ? "UNSIGNED-BYTE" : headName);

        long? n = c.Car switch
        {
            Fixnum f => f.Value,
            Bignum b when b.Value >= 0 && b.Value <= 128 => (long)b.Value,
            _ => null
        };
        if (n == null) return NamedType.Get(headName);

        if (headName == "MOD")
        {
            // (MOD n) = (INTEGER 0 (n-1))
            var hi = n.Value <= 1 ? (LispObject)new Fixnum(0)
                : n.Value - 1 <= long.MaxValue ? new Fixnum(n.Value - 1)
                : (LispObject)new Bignum(new System.Numerics.BigInteger(n.Value - 1));
            return new NumericType("INTEGER", new Fixnum(0), false, hi, false);
        }
        if (headName == "UNSIGNED-BYTE")
        {
            // (UNSIGNED-BYTE n) = (INTEGER 0 (2^n - 1))
            var hiVal = System.Numerics.BigInteger.Pow(2, (int)n.Value) - 1;
            LispObject hi = hiVal <= long.MaxValue ? new Fixnum((long)hiVal) : (LispObject)new Bignum(hiVal);
            return new NumericType("INTEGER", new Fixnum(0), false, hi, false);
        }
        // SIGNED-BYTE
        {
            // (SIGNED-BYTE n) = (INTEGER -(2^(n-1)) (2^(n-1) - 1))
            var halfPow = System.Numerics.BigInteger.Pow(2, (int)(n.Value - 1));
            var loVal = -halfPow;
            var hiVal = halfPow - 1;
            LispObject lo = loVal >= long.MinValue ? new Fixnum((long)loVal) : (LispObject)new Bignum(loVal);
            LispObject hi = hiVal <= long.MaxValue ? new Fixnum((long)hiVal) : (LispObject)new Bignum(hiVal);
            return new NumericType("INTEGER", lo, false, hi, false);
        }
    }

    // --- Array types ---

    private static CType ParseArray(bool simple, LispObject? args, HashSet<string>? expanding)
    {
        var et = NamedType.Get("*") as CType;
        LispObject? dims = null;

        if (args is Cons c1)
        {
            if (c1.Car is not Symbol s1 || s1.Name != "*")
                et = Parse(c1.Car, expanding);
            if (c1.Cdr is Cons c2)
                dims = c2.Car is Symbol s2 && s2.Name == "*" ? null : c2.Car;
        }

        return new ArrayType(et, dims, simple ? true : null);
    }

    private static CType ParseVector(bool simple, LispObject? args, HashSet<string>? expanding)
    {
        CType et;
        LispObject? size = null;

        if (simple)
        {
            // SIMPLE-VECTOR: (SIMPLE-VECTOR [size]) — element type is always T
            et = NamedType.Get("T");
            if (args is Cons c1 && c1.Car is not (Symbol { Name: "*" }))
                size = c1.Car;
        }
        else
        {
            // VECTOR: (VECTOR [element-type [size]])
            et = NamedType.Get("*");
            if (args is Cons c1)
            {
                if (c1.Car is not Symbol s1 || s1.Name != "*")
                    et = Parse(c1.Car, expanding);
                if (c1.Cdr is Cons c2 && c2.Car is not (Symbol { Name: "*" }))
                    size = c2.Car;
            }
        }

        // Vector = (ARRAY et (*)) with optional size constraint
        var dimSpec = size != null
            ? (LispObject)new Cons(size, Nil.Instance)
            : new Cons(Startup.Sym("*"), Nil.Instance);

        return new ArrayType(et, dimSpec, simple ? true : null);
    }

    private static CType ParseSpecializedVector(string headName, LispObject? args)
    {
        // STRING → (ARRAY CHARACTER (*))
        // SIMPLE-STRING → (SIMPLE-ARRAY CHARACTER (*))
        // BIT-VECTOR → (ARRAY BIT (*))
        // etc.
        // Keep as NamedType for now — handled by the hierarchy table
        // Full array-type normalization is Phase 2 work
        return NamedType.Get(headName);
    }

    // --- Compound deftype ---

    private static CType ParseCompoundDeftype(Symbol head, LispObject? args, HashSet<string>? expanding)
    {
        if (!Runtime.TypeExpanders.TryGetValue(head.Name, out var expander))
            return NamedType.Get(head.Name);

        expanding ??= new HashSet<string>();
        if (!expanding.Add(head.Name))
            return NamedType.Get(head.Name);

        try
        {
            var expandArgs = CollectArgs(args).ToArray();
            var expanded = Runtime.Funcall(expander, expandArgs);
            if (expanded is Cons rc && rc.Car is Symbol rh && rh.Name == head.Name)
                return NamedType.Get(head.Name);
            return Parse(expanded, expanding);
        }
        catch
        {
            return NamedType.Get(head.Name);
        }
        finally
        {
            expanding.Remove(head.Name);
        }
    }

    // --- Helpers ---

    private static List<LispObject> CollectArgs(LispObject? list)
    {
        var result = new List<LispObject>();
        while (list is Cons c)
        {
            result.Add(c.Car);
            list = c.Cdr;
        }
        return result;
    }
}

// ============================================================
// CTypeSubtypep: type algebra on CType objects
// ============================================================

public static class CTypeOps
{
    /// <summary>
    /// Determine if ct1 is a subtype of ct2.
    /// Returns (result, certain) where certain=true means the answer is definitive.
    /// </summary>
    public static (bool result, bool certain) Subtypep(CType ct1, CType ct2)
    {
        // === Normalize ClassCType to NamedType for built-in classes ===
        if (ct1 is ClassCType cls1n && Runtime.IsBuiltinTypeName(cls1n.Class.Name.Name))
            ct1 = NamedType.Get(cls1n.Class.Name.Name);
        if (ct2 is ClassCType cls2n && Runtime.IsBuiltinTypeName(cls2n.Class.Name.Name))
            ct2 = NamedType.Get(cls2n.Class.Name.Name);

        // === Identity ===
        if (ReferenceEquals(ct1, ct2)) return (true, true);

        // === Universal / Empty ===
        if (ct2 is NamedType nt2)
        {
            if (nt2.Name == "T") return (true, true);
            if (nt2.Name == "NIL")
            {
                if (ct1 is NamedType nt1nil && nt1nil.Name == "NIL") return (true, true);
                // Complex ct1 might be empty (e.g., (AND X (NOT X))) — can't be sure
                return (false, false);
            }
        }
        if (ct1 is NamedType nt1)
        {
            if (nt1.Name == "NIL") return (true, true);    // NIL is bottom type
            if (nt1.Name == "T")
            {
                if (ct2 is NamedType nt2t && nt2t.Name == "T") return (true, true);
                // T <: non-T — false, but might not be certain for complex ct2
                return (false, false);
            }
        }

        // === Dispatch on type2 (target type) ===

        if (ct2 is NegationType neg2)
            return SubtypepNot(ct1, neg2);
        if (ct2 is UnionType union2)
            return SubtypepUnion2(ct1, union2);
        if (ct2 is IntersectionType inter2)
            return SubtypepIntersection2(ct1, inter2);

        // === Dispatch on type1 (source type) ===

        if (ct1 is UnionType union1)
            return SubtypepUnion1(union1, ct2);
        if (ct1 is IntersectionType inter1)
            return SubtypepIntersection1(inter1, ct2);
        if (ct1 is NegationType)
        {
            // (NOT A) <: B — hard in general. Only handle B=T (already done above).
            return (false, false);
        }

        // === Same-class comparisons ===

        if (ct1 is NamedType n1 && ct2 is NamedType n2)
            return SubtypepNamed(n1, n2);

        if (ct1 is NumericType num1 && ct2 is NumericType num2)
            return SubtypepNumeric(num1, num2);
        if (ct1 is NumericType num1b && ct2 is NamedType n2b)
            return SubtypepNumericNamed(num1b, n2b);
        if (ct1 is NamedType n1b && ct2 is NumericType num2b)
            return SubtypepNamedNumeric(n1b, num2b);

        if (ct1 is ArrayType arr1 && ct2 is ArrayType arr2)
            return SubtypepArray(arr1, arr2);
        if (ct1 is ArrayType arr1b && ct2 is NamedType n2arr)
        {
            var arr2conv = NamedToArrayType(n2arr.Name);
            if (arr2conv != null) return SubtypepArray(arr1b, arr2conv);
        }
        if (ct1 is NamedType n1arr && ct2 is ArrayType arr2b)
        {
            var arr1conv = NamedToArrayType(n1arr.Name);
            if (arr1conv != null) return SubtypepArray(arr1conv, arr2b);
        }

        if (ct1 is ConsType cons1 && ct2 is ConsType cons2)
            return SubtypepCons(cons1, cons2);

        if (ct1 is ClassCType cls1 && ct2 is ClassCType cls2)
            return SubtypepClass(cls1, cls2);
        if (ct1 is ClassCType cls1b && ct2 is NamedType n2c)
            return SubtypepClassNamed(cls1b, n2c);
        if (ct1 is NamedType n1c && ct2 is ClassCType cls2b)
            return SubtypepNamedClass(n1c, cls2b);

        if (ct1 is MemberType mem1)
            return SubtypepMember(mem1, ct2);

        // SATISFIES — cannot determine in general
        if (ct1 is SatisfiesType || ct2 is SatisfiesType)
            return (false, false);

        return (false, false);
    }

    // --- NOT ---

    private static (bool, bool) SubtypepNot(CType ct1, NegationType neg2)
    {
        // Special case: ct1 = (NOT X), ct2 = (NOT Y) → ct1 ⊆ ct2 iff Y ⊆ X
        if (ct1 is NegationType neg1)
        {
            return Subtypep(neg2.Inner, neg1.Inner);
        }
        // ct1 <: (NOT inner) iff ct1 and inner are disjoint
        var (isSub, cert) = Subtypep(ct1, neg2.Inner);
        if (isSub) return (false, true);  // ct1 ⊆ inner → ct1 ⊄ (NOT inner)
        if (cert && AreDisjoint(ct1, neg2.Inner))
            return (true, true);  // definitely disjoint → ct1 ⊆ (NOT inner)
        return (false, false);
    }

    // --- OR ---

    private static (bool, bool) SubtypepUnion2(CType ct1, UnionType union2)
    {
        // ct1 <: (OR a b c) iff ct1 <: any member
        foreach (var member in union2.Types)
        {
            var (sub, _) = Subtypep(ct1, member);
            if (sub) return (true, true);
        }
        // Check if union is universal: (OR X (NOT X)) = T, everything is a subtype
        if (IsUniversalUnion(union2))
            return (true, true);
        return (false, false);
    }

    /// <summary>
    /// Check if a union type is provably universal (= T).
    /// Detects (OR X (NOT X)) and (OR X (NOT Y)) where Y ⊆ X patterns.
    /// </summary>
    private static bool IsUniversalUnion(UnionType union)
    {
        var types = union.Types;
        for (int i = 0; i < types.Length; i++)
        {
            if (types[i] is NegationType neg)
            {
                for (int j = 0; j < types.Length; j++)
                {
                    if (j == i) continue;
                    // If neg.Inner ⊆ types[j], then (OR types[j] (NOT neg.Inner)) = T
                    var (sub, cert) = Subtypep(neg.Inner, types[j]);
                    if (sub && cert) return true;
                }
            }
        }
        return false;
    }

    private static (bool, bool) SubtypepUnion1(UnionType union1, CType ct2)
    {
        // (OR a b c) <: ct2 iff ALL members <: ct2
        bool allSub = true, allCert = true;
        foreach (var member in union1.Types)
        {
            var (sub, cert) = Subtypep(member, ct2);
            if (!sub) { allSub = false; if (!cert) allCert = false; }
        }
        if (allSub) return (true, true);
        if (allCert) return (false, true);
        return (false, false);
    }

    // --- AND ---

    private static (bool, bool) SubtypepIntersection2(CType ct1, IntersectionType inter2)
    {
        // ct1 <: (AND a b c) iff ct1 <: ALL members
        bool allSub = true, allCert = true;
        foreach (var member in inter2.Types)
        {
            var (sub, cert) = Subtypep(ct1, member);
            if (!sub) { allSub = false; if (!cert) allCert = false; }
        }
        if (allSub) return (true, true);
        if (allCert) return (false, true);
        return (false, false);
    }

    private static (bool, bool) SubtypepIntersection1(IntersectionType inter1, CType ct2)
    {
        // (AND a b c) <: ct2 iff ANY member <: ct2 (conservative)
        foreach (var member in inter1.Types)
        {
            var (sub, _) = Subtypep(member, ct2);
            if (sub) return (true, true);
        }
        // Check if intersection is empty: (AND X (NOT X)) = NIL, which is subtype of everything
        if (IsEmptyIntersection(inter1))
            return (true, true);
        return (false, false);
    }

    /// <summary>
    /// Check if an intersection type is provably empty.
    /// Detects (AND X (NOT X)) and (AND X (NOT Y)) where X ⊆ Y patterns.
    /// </summary>
    private static bool IsEmptyIntersection(IntersectionType inter)
    {
        var types = inter.Types;
        // Check each pair: if any positive member is disjoint with another, the intersection is empty
        for (int i = 0; i < types.Length; i++)
        {
            for (int j = i + 1; j < types.Length; j++)
            {
                if (AreDisjoint(types[i], types[j]))
                    return true;
            }
            // Check if a member and (NOT member) both appear
            if (types[i] is NegationType neg)
            {
                for (int j = 0; j < types.Length; j++)
                {
                    if (j == i) continue;
                    var (sub, cert) = Subtypep(types[j], neg.Inner);
                    if (sub && cert) return true;  // types[j] ⊆ neg.Inner → types[j] AND (NOT neg.Inner) = empty
                }
            }
        }
        return false;
    }

    // --- Named types ---

    private static (bool, bool) SubtypepNamed(NamedType n1, NamedType n2)
    {
        // Use the built-in hierarchy table
        if (Runtime.CheckSubtypeByName(n1.Name, n2.Name))
            return (true, true);
        // Also check CLOS class registry for user-defined types
        if (Runtime.FindClassByName(n1.Name) is LispClass cls1)
        {
            foreach (var c in cls1.ClassPrecedenceList)
                if (c.Name.Name == n2.Name) return (true, true);
        }
        // Only definitive if BOTH types are known (built-in or CLOS class)
        bool n1Known = Runtime.IsBuiltinTypeName(n1.Name) || Runtime.FindClassByName(n1.Name) != null;
        bool n2Known = Runtime.IsBuiltinTypeName(n2.Name) || Runtime.FindClassByName(n2.Name) != null;
        if (n1Known && n2Known) return (false, true);
        return (false, false);  // Unknown type — can't be sure
    }

    // --- Numeric types ---

    private static (bool, bool) SubtypepNumeric(NumericType num1, NumericType num2)
    {
        // Check base class compatibility
        if (!Runtime.CheckSubtypeByName(num1.NumClass, num2.NumClass))
            return (false, true);

        // For integer types, normalize exclusive bounds to inclusive
        // (INTEGER (9)) = (INTEGER 10), (INTEGER * (10)) = (INTEGER * 9)
        NormalizeBounds(num1, out var low1, out var lowExcl1, out var high1, out var highExcl1);
        NormalizeBounds(num2, out var low2, out var lowExcl2, out var high2, out var highExcl2);

        // Check interval containment: [low1, high1] ⊆ [low2, high2]
        if (!BoundContained(low1, lowExcl1, low2, lowExcl2, isLow: true))
            return (false, true);
        if (!BoundContained(high1, highExcl1, high2, highExcl2, isLow: false))
            return (false, true);

        return (true, true);
    }

    private static void NormalizeBounds(NumericType num,
        out LispObject? low, out bool lowExcl, out LispObject? high, out bool highExcl)
    {
        low = num.Low; lowExcl = num.LowExclusiveP;
        high = num.High; highExcl = num.HighExclusiveP;
        // Only normalize for integer types: exclusive → inclusive by ±1
        if (num.NumClass != "INTEGER") return;
        if (low != null && lowExcl)
        {
            try { low = Runtime.Add(low, new Fixnum(1)); lowExcl = false; }
            catch { /* leave as-is */ }
        }
        if (high != null && highExcl)
        {
            try { high = Runtime.Subtract(high, new Fixnum(1)); highExcl = false; }
            catch { /* leave as-is */ }
        }
    }

    private static (bool, bool) SubtypepNumericNamed(NumericType num, NamedType named)
    {
        // (INTEGER 0 10) <: NUMBER → true if INTEGER <: NUMBER
        if (Runtime.CheckSubtypeByName(num.NumClass, named.Name)) return (true, true);
        // Try converting named type to numeric range: FIXNUM, BIT, UNSIGNED-BYTE
        var namedNum = NamedToNumericType(named.Name);
        if (namedNum != null) return SubtypepNumeric(num, namedNum);
        // BIGNUM = integers outside fixnum range: (OR (INTEGER * (min-fixnum)) (INTEGER (max-fixnum) *))
        if (named.Name == "BIGNUM" && num.NumClass == "INTEGER")
        {
            // Check if the entire range is outside fixnum range
            bool belowFixnum = num.High != null && BoundBelow(num.High, num.HighExclusiveP, long.MinValue);
            bool aboveFixnum = num.Low != null && BoundAbove(num.Low, num.LowExclusiveP, long.MaxValue);
            if (belowFixnum || aboveFixnum) return (true, true);
            return (false, false);  // might overlap with fixnum range
        }
        if (Runtime.IsBuiltinTypeName(named.Name)) return (false, true);
        return (false, false);
    }

    private static bool BoundBelow(LispObject bound, bool exclusive, long limit)
    {
        // Check if all values satisfying the bound are below limit
        try
        {
            int cmp = CompareBounds(bound, new Fixnum(limit));
            if (cmp == int.MinValue) return false;
            if (cmp < 0) return true;  // bound < limit
            if (cmp == 0) return exclusive;  // bound == limit, exclusive means strictly below
            return false;
        }
        catch { return false; }
    }

    private static bool BoundAbove(LispObject bound, bool exclusive, long limit)
    {
        // Check if all values satisfying the bound are above limit
        try
        {
            int cmp = CompareBounds(bound, new Fixnum(limit));
            if (cmp == int.MinValue) return false;
            if (cmp > 0) return true;  // bound > limit
            if (cmp == 0) return exclusive;  // bound == limit, exclusive means strictly above
            return false;
        }
        catch { return false; }
    }

    private static (bool, bool) SubtypepNamedNumeric(NamedType named, NumericType num)
    {
        // Try converting named type to numeric range first
        var namedNum = NamedToNumericType(named.Name);
        if (namedNum != null) return SubtypepNumeric(namedNum, num);
        // INTEGER <: (INTEGER 0 10) → only if named is a bounded subrange
        if (!Runtime.CheckSubtypeByName(named.Name, num.NumClass))
            return (false, Runtime.IsBuiltinTypeName(named.Name));
        if (num.Low != null || num.High != null)
            return (false, false);  // unbounded named vs bounded numeric — uncertain
        return (true, true);
    }

    /// <summary>Convert named array types to their equivalent ArrayType.</summary>
    private static ArrayType? NamedToArrayType(string name)
    {
        // Only convert structural array types (not STRING/BIT-VECTOR which have
        // implementation-dependent element types via upgraded-array-element-type)
        return name switch
        {
            "ARRAY" => new ArrayType(NamedType.Get("*")),
            "SIMPLE-ARRAY" => new ArrayType(NamedType.Get("*"), null, true),
            "VECTOR" => new ArrayType(NamedType.Get("*"),
                new Cons(Startup.Sym("*"), Nil.Instance)),
            "SIMPLE-VECTOR" => new ArrayType(NamedType.Get("T"),
                new Cons(Startup.Sym("*"), Nil.Instance), true),
            _ => null,
        };
    }

    /// <summary>Convert named numeric types to their equivalent NumericType range.</summary>
    private static NumericType? NamedToNumericType(string name)
    {
        return name switch
        {
            "FIXNUM" => new NumericType("INTEGER",
                new Fixnum(long.MinValue), false, new Fixnum(long.MaxValue), false),
            "BIT" => new NumericType("INTEGER",
                new Fixnum(0), false, new Fixnum(1), false),
            "UNSIGNED-BYTE" => new NumericType("INTEGER",
                new Fixnum(0), false, null, false),
            "SIGNED-BYTE" => new NumericType("INTEGER",
                new Fixnum(-128), false, new Fixnum(127), false),
            _ => null,
        };
    }

    // --- Bound containment ---

    private static bool BoundContained(LispObject? inner, bool innerExcl,
                                        LispObject? outer, bool outerExcl, bool isLow)
    {
        // outer is unbounded → always contained
        if (outer == null) return true;
        // inner is unbounded but outer is bounded → not contained
        if (inner == null) return false;

        // Compare bound values
        int cmp = CompareBounds(inner, outer);
        if (cmp == int.MinValue) return false;  // incomparable

        if (isLow)
        {
            // For low bounds: inner_low >= outer_low
            if (cmp > 0) return true;
            if (cmp < 0) return false;
            // Equal: inner_excl=T, outer_excl=F → inner is tighter → contained
            // inner_excl=F, outer_excl=T → inner includes boundary, outer doesn't → not contained
            return innerExcl || !outerExcl;
        }
        else
        {
            // For high bounds: inner_high <= outer_high
            if (cmp < 0) return true;
            if (cmp > 0) return false;
            return innerExcl || !outerExcl;
        }
    }

    private static int CompareBounds(LispObject a, LispObject b)
    {
        // Compare two bound values using Lisp numeric comparison
        try
        {
            if (Runtime.IsTrueLt(a, b)) return -1;
            if (Runtime.IsTrueLt(b, a)) return 1;
            return 0;  // equal
        }
        catch
        {
            return int.MinValue;  // incomparable
        }
    }

    // --- Array types ---

    private static (bool, bool) SubtypepArray(ArrayType a1, ArrayType a2)
    {
        // Simple constraint
        if (a2.SimpleP == true && a1.SimpleP != true) return (false, true);
        if (a2.SimpleP == false && a1.SimpleP == true) return (false, true);

        // Element type: must be equivalent (per upgraded-array-element-type)
        if (a2.ElementType is not NamedType { Name: "*" })
        {
            if (a1.ElementType is NamedType { Name: "*" }) return (false, false);
            // Element types must match (after upgrading)
            var (sub1, _) = Subtypep(a1.ElementType, a2.ElementType);
            var (sub2, _) = Subtypep(a2.ElementType, a1.ElementType);
            if (!sub1 || !sub2) return (false, true);  // element types differ
        }

        // Dimensions
        if (a2.Dimensions != null)
        {
            if (a1.Dimensions == null) return (false, false);
            // Both have dimensions — compare
            if (!DimensionsMatch(a1.Dimensions, a2.Dimensions))
                return (false, true);
        }

        return (true, true);
    }

    private static bool DimensionsMatch(LispObject d1, LispObject d2)
    {
        // Both are rank (Fixnum): must be equal
        if (d1 is Fixnum f1 && d2 is Fixnum f2)
            return f1.Value == f2.Value;
        // Both are dimension lists (Cons): must match element-by-element
        if (d1 is Cons && d2 is Cons)
        {
            var c1 = d1; var c2 = d2;
            while (c1 is Cons cc1 && c2 is Cons cc2)
            {
                // * matches any dimension
                bool w1 = cc1.Car is Symbol s1 && s1.Name == "*";
                bool w2 = cc2.Car is Symbol s2 && s2.Name == "*";
                if (!w1 && !w2)
                {
                    // Both are specific dimensions — must match for subtype
                    if (cc1.Car is Fixnum df1 && cc2.Car is Fixnum df2)
                    {
                        if (df1.Value != df2.Value) return false;
                    }
                    else return false;
                }
                else if (!w2)
                {
                    // d1 is *, d2 is specific: d1 NOT subtype of d2
                    return false;
                }
                c1 = cc1.Cdr; c2 = cc2.Cdr;
            }
            // Must have same length
            return (c1 is Nil || c1 == null) && (c2 is Nil || c2 == null);
        }
        // Rank vs dimension list: rank N matches dimension list of length N
        // Special: rank 0 matches NIL (empty dimension list)
        if (d1 is Fixnum rank1)
        {
            int len = 0;
            var cur = d2;
            while (cur is Cons cc) { len++; cur = cc.Cdr; }
            return rank1.Value == len;
        }
        if (d2 is Fixnum rank2)
        {
            int len = 0;
            var cur = d1;
            while (cur is Cons cc) { len++; cur = cc.Cdr; }
            return len == rank2.Value;
        }
        return false;
    }

    // --- Cons types ---

    private static (bool, bool) SubtypepCons(ConsType c1, ConsType c2)
    {
        var (carSub, carCert) = Subtypep(c1.CarType, c2.CarType);
        if (!carSub) return (false, carCert);
        var (cdrSub, cdrCert) = Subtypep(c1.CdrType, c2.CdrType);
        if (!cdrSub) return (false, cdrCert);
        return (true, true);
    }

    // --- Class types ---

    private static (bool, bool) SubtypepClass(ClassCType c1, ClassCType c2)
    {
        foreach (var c in c1.Class.ClassPrecedenceList)
            if (ReferenceEquals(c, c2.Class) || c.Name.Name == c2.Class.Name.Name)
                return (true, true);
        return (false, true);
    }

    private static (bool, bool) SubtypepClassNamed(ClassCType c1, NamedType n2)
    {
        // Check CPL for the named type
        foreach (var c in c1.Class.ClassPrecedenceList)
            if (c.Name.Name == n2.Name) return (true, true);
        // Also check built-in hierarchy for structure-object etc.
        if (c1.Class.IsStructureClass && Runtime.CheckSubtypeByName("STRUCTURE-OBJECT", n2.Name))
            return (true, true);
        return (false, false);  // conservative — named type might be a deftype alias
    }

    private static (bool, bool) SubtypepNamedClass(NamedType n1, ClassCType c2)
    {
        // Named type <: Class type — check if the named type is a known subclass
        if (Runtime.FindClassByName(n1.Name) is LispClass cls)
        {
            foreach (var c in cls.ClassPrecedenceList)
                if (ReferenceEquals(c, c2.Class) || c.Name.Name == c2.Class.Name.Name)
                    return (true, true);
        }
        return (false, false);  // conservative
    }

    // --- Member types ---

    private static (bool, bool) SubtypepMember(MemberType mem, CType ct2)
    {
        // Fast path: (MEMBER a b c) <: (MEMBER ...) — check set containment
        if (ct2 is MemberType mem2)
        {
            foreach (var obj in mem.Members)
            {
                bool found = false;
                foreach (var obj2 in mem2.Members)
                {
                    if (Runtime.IsTrueEql(obj, obj2)) { found = true; break; }
                }
                if (!found) return (false, true);
            }
            return (true, true);
        }
        // (MEMBER a b c) <: T2 iff all members satisfy T2.
        // If T2 involves SATISFIES, IsTypep may throw (predicate undefined, etc.)
        // and the exception is swallowed as false.  Returning (false, true) in that
        // case incorrectly claims certainty.  Use uncertain (false, false) instead
        // when the type involves SATISFIES, per CLHS permission to return NIL NIL
        // for satisfies-involving types (D694).
        bool hasSatisfies = ContainsSatisfies(ct2);
        foreach (var obj in mem.Members)
        {
            if (!CTypeOps.IsTypep(obj, ct2))
                return (false, hasSatisfies ? false : true);
        }
        return (true, true);
    }

    /// <summary>Returns true if ct (or any nested type) is a SatisfiesType.</summary>
    private static bool ContainsSatisfies(CType ct) =>
        ct is SatisfiesType ||
        (ct is NegationType neg && ContainsSatisfies(neg.Inner)) ||
        (ct is UnionType u && System.Array.Exists(u.Types, ContainsSatisfies)) ||
        (ct is IntersectionType i && System.Array.Exists(i.Types, ContainsSatisfies));

    // --- IsTypep: check if a value is of a CType (for MemberType) ---

    /// <summary>Check if obj is of the given CType. Used by MemberType subtypep.</summary>
    public static bool IsTypep(LispObject obj, CType ct)
    {
        // Convert CType back to specifier and use existing Typep
        // This is a temporary bridge — Phase 3 will replace Typep entirely
        try
        {
            var spec = CTypeToSpecifier(ct);
            return Runtime.Typep(obj, spec) != Nil.Instance;
        }
        catch { return false; }
    }

    private static LispObject CTypeToSpecifier(CType ct)
    {
        switch (ct)
        {
            case NamedType nt:
                return nt.Name == "T" ? (LispObject)T.Instance
                     : nt.Name == "NIL" ? Nil.Instance
                     : Startup.Sym(nt.Name);
            case ClassCType cls:
                return cls.Class.Name;
            case MemberType mem:
            {
                // Build (MEMBER obj1 obj2 ...) or (EQL obj) as a proper Lisp list
                LispObject result = Nil.Instance;
                for (int i = mem.Members.Length - 1; i >= 0; i--)
                    result = new Cons(mem.Members[i], result);
                return new Cons(Startup.Sym(mem.Members.Length == 1 ? "EQL" : "MEMBER"), result);
            }
            case NumericType num:
            {
                // Build (INTEGER low high) etc.
                var head = Startup.Sym(num.NumClass);
                LispObject low = num.Low != null ? (num.LowExclusiveP ? (LispObject)new Cons(num.Low, Nil.Instance) : num.Low) : Startup.Sym("*");
                LispObject high = num.High != null ? (num.HighExclusiveP ? (LispObject)new Cons(num.High, Nil.Instance) : num.High) : Startup.Sym("*");
                return new Cons(head, new Cons(low, new Cons(high, Nil.Instance)));
            }
            case NegationType neg:
                return new Cons(Startup.Sym("NOT"), new Cons(CTypeToSpecifier(neg.Inner), Nil.Instance));
            case UnionType union:
            {
                LispObject result = Nil.Instance;
                for (int i = union.Types.Length - 1; i >= 0; i--)
                    result = new Cons(CTypeToSpecifier(union.Types[i]), result);
                return new Cons(Startup.Sym("OR"), result);
            }
            case IntersectionType inter:
            {
                LispObject result = Nil.Instance;
                for (int i = inter.Types.Length - 1; i >= 0; i--)
                    result = new Cons(CTypeToSpecifier(inter.Types[i]), result);
                return new Cons(Startup.Sym("AND"), result);
            }
            case ConsType cons:
                return new Cons(Startup.Sym("CONS"),
                    new Cons(CTypeToSpecifier(cons.CarType),
                        new Cons(CTypeToSpecifier(cons.CdrType), Nil.Instance)));
            default:
                // Fallback: parse the string representation
                try { return Runtime.ReadFromString(new LispObject[] { new LispString(ct.ToSpecifier()) }); }
                catch { return Startup.Sym(ct.ToSpecifier()); }
        }
    }

    // ============================================================
    // Disjoint type checking
    // ============================================================

    /// <summary>
    /// Determine if two types are known to be disjoint (no common elements).
    /// Conservative: returns false if unsure.
    /// </summary>
    public static bool AreDisjoint(CType a, CType b)
    {
        // T is never disjoint with anything
        if (a is NamedType { Name: "T" } || b is NamedType { Name: "T" }) return false;
        // NIL is disjoint with everything (it's the empty type)
        if (a is NamedType { Name: "NIL" } || b is NamedType { Name: "NIL" }) return true;

        // If either is a subtype of the other, not disjoint
        var (sub1, cert1) = Subtypep(a, b);
        if (sub1) return false;
        var (sub2, cert2) = Subtypep(b, a);
        if (sub2) return false;

        // Get disjoint groups for named/class types
        var groupA = GetDisjointGroup(a);
        var groupB = GetDisjointGroup(b);
        if (groupA != null && groupB != null && groupA != groupB)
            return true;

        return false;
    }

    // Disjoint group names (top-level partitions of CL type system)
    private static readonly Dictionary<string, string> _disjointGroups = new()
    {
        // NUMBER group
        ["NUMBER"] = "NUMBER", ["REAL"] = "NUMBER", ["RATIONAL"] = "NUMBER",
        ["INTEGER"] = "NUMBER", ["FIXNUM"] = "NUMBER", ["BIGNUM"] = "NUMBER",
        ["BIT"] = "NUMBER", ["UNSIGNED-BYTE"] = "NUMBER", ["SIGNED-BYTE"] = "NUMBER",
        ["RATIO"] = "NUMBER", ["FLOAT"] = "NUMBER",
        ["SINGLE-FLOAT"] = "NUMBER", ["DOUBLE-FLOAT"] = "NUMBER",
        ["SHORT-FLOAT"] = "NUMBER", ["LONG-FLOAT"] = "NUMBER",
        ["COMPLEX"] = "NUMBER",
        // CHARACTER group
        ["CHARACTER"] = "CHARACTER", ["BASE-CHAR"] = "CHARACTER",
        ["STANDARD-CHAR"] = "CHARACTER", ["EXTENDED-CHAR"] = "CHARACTER",
        // SYMBOL group
        ["SYMBOL"] = "SYMBOL", ["KEYWORD"] = "SYMBOL", ["BOOLEAN"] = "SYMBOL", ["NULL"] = "SYMBOL",
        // CONS group (only CONS itself — LIST includes NIL which is SYMBOL)
        ["CONS"] = "CONS",
        // ARRAY group
        ["ARRAY"] = "ARRAY", ["VECTOR"] = "ARRAY", ["SIMPLE-ARRAY"] = "ARRAY",
        ["SIMPLE-VECTOR"] = "ARRAY", ["STRING"] = "ARRAY", ["SIMPLE-STRING"] = "ARRAY",
        ["BASE-STRING"] = "ARRAY", ["SIMPLE-BASE-STRING"] = "ARRAY",
        ["BIT-VECTOR"] = "ARRAY", ["SIMPLE-BIT-VECTOR"] = "ARRAY",
        // FUNCTION group
        ["FUNCTION"] = "FUNCTION", ["COMPILED-FUNCTION"] = "FUNCTION",
        ["GENERIC-FUNCTION"] = "FUNCTION", ["STANDARD-GENERIC-FUNCTION"] = "FUNCTION",
        // STREAM group
        ["STREAM"] = "STREAM", ["FILE-STREAM"] = "STREAM", ["STRING-STREAM"] = "STREAM",
        ["BROADCAST-STREAM"] = "STREAM", ["CONCATENATED-STREAM"] = "STREAM",
        ["ECHO-STREAM"] = "STREAM", ["SYNONYM-STREAM"] = "STREAM", ["TWO-WAY-STREAM"] = "STREAM",
        // Singletons
        ["HASH-TABLE"] = "HASH-TABLE",
        ["PACKAGE"] = "PACKAGE",
        ["PATHNAME"] = "PATHNAME", ["LOGICAL-PATHNAME"] = "PATHNAME",
        ["RANDOM-STATE"] = "RANDOM-STATE",
        ["RESTART"] = "RESTART",
    };

    private static string? GetDisjointGroup(CType ct)
    {
        if (ct is NamedType nt && _disjointGroups.TryGetValue(nt.Name, out var group))
            return group;
        if (ct is NumericType) return "NUMBER";
        if (ct is ArrayType) return "ARRAY";
        if (ct is ConsType) return "CONS";
        if (ct is ClassCType cls)
        {
            // Structure classes: check if any CPL member has a known group
            foreach (var c in cls.Class.ClassPrecedenceList)
            {
                if (_disjointGroups.TryGetValue(c.Name.Name, out var g))
                    return g;
            }
            // Default for structure-object: its own name as group
            // (distinct structures are disjoint unless they share inheritance)
            if (cls.Class.IsStructureClass)
                return "STRUCT:" + cls.Class.Name.Name;
        }
        return null;
    }
}
