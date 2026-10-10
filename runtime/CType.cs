// CType: Internal type representation for subtypep/typep.
// Type specifiers (Lisp forms) are parsed into CType objects once,
// then all comparisons are done on CType objects (no re-parsing).

using System.Numerics;

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

    // Singleton registry for built-in type names (ConcurrentDictionary for thread safety).
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

/// <summary>
/// (COMPLEX part-type) narrower than COMPLEX: the complexes of the representations
/// in Mask (Runtime.ComplexRationalParts / ComplexSingleParts / ComplexDoubleParts).
/// Every (COMPLEX X) is one of these, COMPLEX itself, or NIL.
/// </summary>
public class ComplexType : CType
{
    public int Mask { get; }
    /// <summary>The part type when it is its own upgraded part type (see
    /// Runtime.ComplexPartMask): the complexes whose parts are both of it. Null
    /// when the type is every complex of the representations in Mask.</summary>
    public LispObject? Exact { get; }
    public ComplexType(int mask, LispObject? exact = null) { Mask = mask; Exact = exact; }

    public LispObject PartSpec => Exact ?? Runtime.ComplexPartTypeSpec(Mask);

    public override string ToSpecifier() => $"(COMPLEX {PartSpec})";
}

/// <summary>Array type: (ARRAY element-type dimensions), (SIMPLE-ARRAY ...), etc.</summary>
public class ArrayType : CType
{
    public CType ElementType { get; }   // element type (NamedType("*") for wildcard)
    public LispObject? Dimensions { get; }  // null=*, Fixnum=rank, Cons=dimension list
    public bool? SimpleP { get; }       // true=simple, false=not-simple, null=either
    /// <summary>The element type as written, when parsed from a specifier
    /// (null otherwise): SUBTYPEP compares its upgraded element type.</summary>
    public LispObject? ElementSpec { get; }

    public ArrayType(CType elementType, LispObject? dimensions = null, bool? simpleP = null,
                     LispObject? elementSpec = null)
    {
        ElementType = elementType; Dimensions = dimensions; SimpleP = simpleP;
        ElementSpec = elementSpec;
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
// TypeParser: type specifier -> CType
// ============================================================

public static class TypeParser
{
    // Cache: symbol identity -> parsed CType. Uses symbol reference equality so that
    // pkg1::FOO and pkg2::FOO (same name, different packages) cache independently.
    // ConcurrentDictionary for thread safety. Symbol does not override Equals, so default
    // reference equality applies without a custom comparer.
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<Symbol, CType> _symbolCache = new();

    /// <summary>Clear cache for a specific type name (called when deftype is registered).</summary>
    public static void InvalidateCache(string name)
    {
        // Remove all cached entries whose symbol has the given name (any package).
        foreach (var key in _symbolCache.Keys.Where(k => k.Name == name).ToList())
            _symbolCache.TryRemove(key, out _);
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
                // Built-in type classes -> NamedType for consistent comparison
                if (Runtime.IsBuiltinTypeName(cls.Name.Name))
                    return NamedType.Get(cls.Name.Name);
                return new ClassCType(cls);

            default:
                // Unknown specifier: treat as named type
                return NamedType.Get(specifier.ToString() ?? "?");
        }
    }

    private static CType ParseSymbol(Symbol sym, HashSet<string>? expanding)
    {
        var name = sym.Name;

        // Check cache (keyed by symbol identity: package-aware)
        if (_symbolCache.TryGetValue(sym, out var cached))
            return cached;

        // A non-CL deftype shadows built-ins for its own symbol
        if (Runtime.TryGetQualifiedTypeExpander(sym, out var qExpander))
        {
            var qResult = Parse(Runtime.Funcall(qExpander), expanding);
            _symbolCache[sym] = qResult;
            return qResult;
        }

        // Built-in type names: return singleton NamedType
        if (Runtime.IsBuiltinTypeName(name) || name == "T" || name == "NIL" || name == "*")
        {
            var nt = NamedType.Get(name);
            _symbolCache[sym] = nt;
            return nt;
        }

        // Check for a deftype expander registered for this very symbol
        if (Runtime.TryGetTypeExpander(sym, out var expander))
        {
            // Circular expansion detection
            var expKey = Runtime.TypeExpanderKey(sym);
            expanding ??= new HashSet<string>();
            if (!expanding.Add(expKey))
            {
                // Circular: return as named type (best effort)
                var nt = NamedType.Get(name);
                _symbolCache[sym] = nt;
                return nt;
            }

            try
            {
                var expanded = Runtime.Funcall(expander);
                // Self-referential guard
                if (expanded is Symbol rs && rs.Name == name)
                {
                    var nt = NamedType.Get(name);
                    _symbolCache[sym] = nt;
                    return nt;
                }
                var result = Parse(expanded, expanding);
                _symbolCache[sym] = result;
                return result;
            }
            catch
            {
                // Expansion failed: treat as named type
                var nt = NamedType.Get(name);
                _symbolCache[sym] = nt;
                return nt;
            }
            finally
            {
                expanding.Remove(expKey);
            }
        }

        // Check CLOS class registry by symbol. A symbol that names no class is not
        // resolved to a same-named class from another package.
        var cls = Runtime.FindClassOrNil(sym) as LispClass;
        if (cls != null)
        {
            var ct = new ClassCType(cls);
            _symbolCache[sym] = ct;
            return ct;
        }

        // Unknown type: keep as NamedType
        var named = NamedType.Get(name);
        _symbolCache[sym] = named;
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

            case "COMPLEX":
            {
                var part = cons.Cdr is Cons pc ? pc.Car : null;
                int mask = Runtime.ComplexPartMask(part, out bool exact);
                return exact ? new ComplexType(mask, part)
                     : mask == 0 ? NamedType.NIL_TYPE
                     : mask == Runtime.ComplexAllParts ? NamedType.Get("COMPLEX")
                     : new ComplexType(mask);
            }

            // Function / Values: stub for now
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
        // (UNSIGNED-BYTE n) -> (INTEGER 0 (2^n - 1))
        // (SIGNED-BYTE n) -> (INTEGER -(2^(n-1)) (2^(n-1) - 1))
        // (MOD n) -> (INTEGER 0 (n - 1))
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
                : (LispObject)new Bignum(new BigInteger(n.Value - 1));
            return new NumericType("INTEGER", new Fixnum(0), false, hi, false);
        }
        if (headName == "UNSIGNED-BYTE")
        {
            // (UNSIGNED-BYTE n) = (INTEGER 0 (2^n - 1))
            var hiVal = BigInteger.Pow(2, (int)n.Value) - 1;
            LispObject hi = hiVal <= long.MaxValue ? new Fixnum((long)hiVal) : (LispObject)new Bignum(hiVal);
            return new NumericType("INTEGER", new Fixnum(0), false, hi, false);
        }
        // SIGNED-BYTE
        {
            // (SIGNED-BYTE n) = (INTEGER -(2^(n-1)) (2^(n-1) - 1))
            var halfPow = BigInteger.Pow(2, (int)(n.Value - 1));
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
        LispObject? etSpec = null;

        if (args is Cons c1)
        {
            if (c1.Car is not Symbol s1 || s1.Name != "*")
            {
                et = Parse(c1.Car, expanding);
                etSpec = c1.Car;
            }
            if (c1.Cdr is Cons c2)
                dims = c2.Car is Symbol s2 && s2.Name == "*" ? null : c2.Car;
        }

        return new ArrayType(et, dims, simple ? true : null, etSpec);
    }

    private static CType ParseVector(bool simple, LispObject? args, HashSet<string>? expanding)
    {
        CType et;
        LispObject? size = null;
        LispObject? etSpec = null;

        if (simple)
        {
            // SIMPLE-VECTOR: (SIMPLE-VECTOR [size]): element type is always T
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
                {
                    et = Parse(c1.Car, expanding);
                    etSpec = c1.Car;
                }
                if (c1.Cdr is Cons c2 && c2.Car is not (Symbol { Name: "*" }))
                    size = c2.Car;
            }
        }

        // Vector = (ARRAY et (*)) with optional size constraint
        var dimSpec = size != null
            ? (LispObject)new Cons(size, Nil.Instance)
            : new Cons(Startup.Sym("*"), Nil.Instance);

        return new ArrayType(et, dimSpec, simple ? true : null, etSpec);
    }

    private static CType ParseSpecializedVector(string headName, LispObject? args)
    {
        // STRING -> (ARRAY CHARACTER (*))
        // SIMPLE-STRING -> (SIMPLE-ARRAY CHARACTER (*))
        // BIT-VECTOR -> (ARRAY BIT (*))
        // etc.
        // Keep as NamedType for now: handled by the hierarchy table
        // Full array-type normalization is Phase 2 work
        return NamedType.Get(headName);
    }

    // --- Compound deftype ---

    private static CType ParseCompoundDeftype(Symbol head, LispObject? args, HashSet<string>? expanding)
    {
        if (!Runtime.TryGetTypeExpander(head, out var expander))
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
        if (ReferenceEquals(ct1, ct2) || StructurallyEqual(ct1, ct2)) return (true, true);

        // === Universal / Empty ===
        if (ct2 is NamedType nt2)
        {
            if (nt2.Name == "T") return (true, true);
            if (nt2.Name == "NIL")
            {
                if (ct1 is NamedType nt1nil && nt1nil.Name == "NIL") return (true, true);
                // An intersection of disjoint types is empty: (AND INTEGER RATIO),
                // (AND (COMPLEX RATIONAL) (COMPLEX FLOAT)).
                if (ct1 is IntersectionType interNil && IsEmptyIntersection(interNil)) return (true, true);
                // Complex ct1 might be empty (e.g., (AND X (NOT X))): can't be sure
                return (false, false);
            }
        }
        if (ct1 is NamedType nt1)
        {
            if (nt1.Name == "NIL") return (true, true);    // NIL is bottom type
            if (nt1.Name == "T")
            {
                if (ct2 is NamedType nt2t && nt2t.Name == "T") return (true, true);
                // T <: non-T: false, but might not be certain for complex ct2
                return (false, false);
            }
        }

        // === Exact decompositions first ===
        // (OR a b) <: X iff a <: X and b <: X; X <: (AND a b) iff X <: a and X <: b.
        // Both are exact, so they go before the rules below that only find
        // sufficient conditions (X <: (OR ...) tried member by member would miss
        // (OR a b) <: (OR a b), and (NOT ...) on the right would hide the ct1 split).
        if (ct1 is UnionType union1x)
            return SubtypepUnion1(union1x, ct2);
        if (ct2 is IntersectionType inter2x)
            return SubtypepIntersection2(ct1, inter2x);

        // (AND a b) <: X holds as soon as one conjunct is a subtype of X, whatever
        // the shape of X: (AND SYMBOL (NOT (MEMBER T NIL))) <: (NOT (MEMBER T NIL)).
        if (ct1 is IntersectionType inter1x)
        {
            foreach (var member in inter1x.Types)
            {
                var (sub, _) = Subtypep(member, ct2);
                if (sub) return (true, true);
            }
        }

        // (MEMBER a b) <: X is decided object by object, whatever the shape of X,
        // so it goes before the rules on X that only find sufficient conditions:
        // (MEMBER :X :Y) <: (OR (EQL :X) (EQL :Y)) holds though neither member of
        // the union alone covers it.
        if (ct1 is MemberType mem1x)
            return SubtypepMember(mem1x, ct2);

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
            // (NOT A) <: B: hard in general. Only handle B=T (already done above).
            return (false, false);
        }

        // === Same-class comparisons ===

        if (ComplexMask(ct1) is int cm1 && ComplexMask(ct2) is int cm2)
            return ((cm1 & ~cm2) == 0, true);
        if (ct1 is ComplexType { Exact: not null } || ct2 is ComplexType { Exact: not null })
        {
            // Some part type is exact: the rational complexes go by the mask, the
            // float complexes by the part types.
            if ((ct1 is ComplexType || ComplexMask(ct1) != null)
                && (ct2 is ComplexType || ComplexMask(ct2) != null))
            {
                int x1 = ct1 is ComplexType c1x ? c1x.Mask : Runtime.ComplexAllParts;
                int x2 = ct2 is ComplexType c2x ? c2x.Mask : Runtime.ComplexAllParts;
                if ((x1 & Runtime.ComplexRationalParts) != 0 && (x2 & Runtime.ComplexRationalParts) == 0)
                    return (false, true);
                const int R = Runtime.ComplexRationalParts;
                // The floats of ct1's parts, which must be floats of ct2's parts.
                // An exact part type holds some floats, so none on the right is NO.
                LispObject floats1;
                if (ct1 is ComplexType { Exact: not null } e1)
                {
                    if (ct2 is not ComplexType { Exact: not null } && (x2 & ~R) == 0) return (false, true);
                    floats1 = (x1 & R) == 0 ? e1.Exact!
                        : Runtime.List(Startup.Sym("AND"), e1.Exact!, Startup.Sym("FLOAT"));
                }
                else
                {
                    if ((x1 & ~R) == 0) return (true, true);
                    floats1 = Runtime.ComplexPartTypeSpec(x1 & ~R);
                }
                var p2 = ct2 is ComplexType { Exact: not null } y2 ? y2.Exact!
                    : Runtime.ComplexPartTypeSpec(x2 & ~R);
                // The whole SUBTYPEP: the algebra alone leaves FLOAT <: (FLOAT 0.0 3.0) open.
                bool ps = Runtime.Subtypep(floats1, p2) != Nil.Instance;
                var mv = MultipleValues.Get();
                bool pc = mv.Length > 1 && mv[1] != Nil.Instance;
                if (ps) return (true, true);
                return (false, pc);
            }
        }
        if (ct1 is ComplexType)
        {
            // Some but not all complexes: a subtype of what holds every complex,
            // not of what holds none.
            var complexAll = NamedType.Get("COMPLEX");
            if (Subtypep(complexAll, ct2).result) return (true, true);
            if (AreDisjoint(complexAll, ct2)) return (false, true);
            return (false, false);
        }
        if (ct2 is ComplexType)
        {
            // Only COMPLEX (handled above) or NIL among the named types is not
            // wider than or disjoint from some complexes; both mean NO when non-empty.
            if (ct1 is NamedType ne && ne.Name is not ("NIL" or "EXTENDED-CHAR" or "*")
                && Runtime.IsBuiltinTypeName(ne.Name))
                return (false, true);
            if (ct1 is NumericType nr && (nr.Low == null || nr.High == null))
                return (false, true);
            if (ct1 is ConsType or ArrayType) return (false, true);
        }

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

        // SATISFIES: cannot determine in general
        if (ct1 is SatisfiesType || ct2 is SatisfiesType)
            return (false, false);

        return (false, false);
    }

    // --- Structural equality ---

    /// <summary>
    /// True when the two CTypes were parsed from equivalent specifiers. Only a
    /// sufficient test for type equality; false means "not shown equal".
    /// </summary>
    private static bool StructurallyEqual(CType a, CType b)
    {
        if (ReferenceEquals(a, b)) return true;
        switch (a)
        {
            case NamedType na:
                return b is NamedType nb && na.Name == nb.Name;
            case ClassCType ca:
                return b is ClassCType cb && ReferenceEquals(ca.Class, cb.Class);
            case SatisfiesType sa:
                return b is SatisfiesType sb && ReferenceEquals(sa.Predicate, sb.Predicate);
            case NegationType ga:
                return b is NegationType gb && StructurallyEqual(ga.Inner, gb.Inner);
            case ConsType ka:
                return b is ConsType kb && StructurallyEqual(ka.CarType, kb.CarType)
                    && StructurallyEqual(ka.CdrType, kb.CdrType);
            case UnionType ua:
                return b is UnionType ub && SameElements(ua.Types, ub.Types);
            case IntersectionType ia:
                return b is IntersectionType ib && SameElements(ia.Types, ib.Types);
            case NumericType ma:
                return b is NumericType mb && ma.NumClass == mb.NumClass
                    && SameBound(ma.Low, mb.Low) && ma.LowExclusiveP == mb.LowExclusiveP
                    && SameBound(ma.High, mb.High) && ma.HighExclusiveP == mb.HighExclusiveP;
            case MemberType ea:
                if (b is not MemberType eb) return false;
                foreach (var o in ea.Members)
                    if (!System.Array.Exists(eb.Members, o2 => Runtime.IsTrueEql(o, o2))) return false;
                foreach (var o in eb.Members)
                    if (!System.Array.Exists(ea.Members, o2 => Runtime.IsTrueEql(o, o2))) return false;
                return true;
            default:
                return false;
        }
    }

    private static bool SameElements(CType[] xs, CType[] ys)
    {
        if (xs.Length != ys.Length) return false;
        for (int i = 0; i < xs.Length; i++)
            if (!StructurallyEqual(xs[i], ys[i])) return false;
        return true;
    }

    private static bool SameBound(LispObject? x, LispObject? y) =>
        x == null ? y == null : y != null && Runtime.IsTrueEql(x, y);

    // --- NOT ---

    private static (bool, bool) SubtypepNot(CType ct1, NegationType neg2)
    {
        // Special case: ct1 = (NOT X), ct2 = (NOT Y) -> ct1 subset-of ct2 iff Y subset-of X
        if (ct1 is NegationType neg1)
        {
            return Subtypep(neg2.Inner, neg1.Inner);
        }
        // ct1 <: (NOT inner) iff ct1 and inner are disjoint
        var (isSub, cert) = Subtypep(ct1, neg2.Inner);
        if (isSub) return (false, true);  // ct1 subset-of inner -> ct1 not-subset-of (NOT inner)
        if (cert && AreDisjoint(ct1, neg2.Inner))
            return (true, true);  // definitely disjoint -> ct1 subset-of (NOT inner)
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
        // Complexes: the representations the members hold together must cover
        // ct1's. COMPLEX <: (OR (COMPLEX RATIONAL) (COMPLEX FLOAT)), but not
        // <: (OR REAL (COMPLEX RATIONAL)).
        if (ComplexMask(ct1) is int needParts)
        {
            int haveParts = 0;
            bool exactParts = true;
            var complexAll = NamedType.Get("COMPLEX");
            foreach (var m in union2.Types)
            {
                if (ComplexMask(m) is int mm) haveParts |= mm;
                else if (m is ComplexType { Exact: not null } me)
                {
                    haveParts |= me.Mask & Runtime.ComplexRationalParts;
                    exactParts = false;
                }
                else if (Subtypep(complexAll, m).result) haveParts = Runtime.ComplexAllParts;
                else if (!AreDisjoint(complexAll, m)) exactParts = false;
            }
            if ((needParts & ~haveParts) == 0) return (true, true);
            if (exactParts) return (false, true);
        }
        // An integer type is a subtype of a union iff the integers in the union
        // cover it. INTEGER <: (OR (INTEGER 0 *) (INTEGER * -1)); (INTEGER 0 10) is
        // not a subtype of (OR (INTEGER 0 4) (INTEGER 6 10)) nor of (OR RATIO FLOAT).
        var need = new List<(BigInteger? lo, BigInteger? hi)>();
        if ((IsIntegerOnly(ct1) || ct1 is NamedType { Name: "BIGNUM" }) && IntegerPieces(ct1, need))
        {
            var have = new List<(BigInteger? lo, BigInteger? hi)>();
            bool exact = true;
            foreach (var m in union2.Types)
                if (!IntegerPieces(m, have) && !AreDisjoint(NamedType.Get("INTEGER"), m)) exact = false;
            if (need.TrueForAll(r => RangesCover(r.lo, r.hi, have))) return (true, true);
            if (exact) return (false, true);
        }
        // X <: (OR A rest) iff (AND X (NOT A)) <: rest. Only the YES is taken. Tried
        // for a MEMBER, and for a member whose complement another member names:
        // SYMBOL <: (OR (MEMBER T NIL) (AND SYMBOL (NOT (MEMBER T NIL)))),
        // STRING <: (OR SIMPLE-STRING (AND STRING (NOT SIMPLE-STRING))).
        for (int i = 0; i < union2.Types.Length; i++)
        {
            var mem = union2.Types[i];
            if (mem is not MemberType && !OtherMemberNegates(union2.Types, i)) continue;
            var rest = union2.Types.Where((_, j) => j != i).ToArray();
            CType restType = rest.Length == 1 ? rest[0] : new UnionType(rest);
            var conj = (ct1 is IntersectionType ci ? ci.Types : new[] { ct1 })
                .Append(new NegationType(mem)).ToArray();
            var (sub, cert) = Subtypep(new IntersectionType(conj), restType);
            if (sub && cert) return (true, true);
        }
        // A type that is the disjoint union of its parts is a subtype iff every part
        // is: REAL <: (OR INTEGER RATIO FLOAT), LIST <: (OR CONS NULL). Exact, so a
        // certain NO for one part is a certain NO for the whole.
        if (ct1 is NamedType pn && TypePartition(pn.Name) is string[] parts)
        {
            bool allCert = true;
            foreach (var part in parts)
            {
                var (sub, cert) = Subtypep(NamedType.Get(part), union2);
                if (!sub)
                {
                    if (cert) return (false, true);
                    allCert = false;
                }
            }
            if (allCert) return (true, true);
        }
        // A non-empty built-in type disjoint from every member is not a subtype:
        // INTEGER is not a subtype of (OR RATIO FLOAT).
        if (ct1 is NamedType ne && ne.Name is not ("NIL" or "*") && Runtime.IsBuiltinTypeName(ne.Name)
            && System.Array.TrueForAll(union2.Types, m => AreDisjoint(ct1, m)))
            return (false, true);
        return (false, false);
    }

    /// <summary>A NumericType INTEGER or an integer type name, so every element is an integer.</summary>
    private static bool IsIntegerOnly(CType ct) =>
        ct is NumericType { NumClass: "INTEGER" }
        || ct is NamedType { Name: "INTEGER" or "SIGNED-BYTE" or "FIXNUM" or "BIT" or "UNSIGNED-BYTE" };

    /// <summary>
    /// Add to INTO the integers of ct as inclusive ranges (null = unbounded).
    /// False when they cannot be told exactly; then INTO may hold some of them.
    /// </summary>
    private static bool IntegerPieces(CType ct, List<(BigInteger? lo, BigInteger? hi)> into)
    {
        switch (ct)
        {
            case NamedType n:
                switch (n.Name)
                {
                    case "INTEGER" or "SIGNED-BYTE" or "RATIONAL" or "REAL" or "NUMBER" or "T":
                        into.Add((null, null)); return true;
                    case "FIXNUM": into.Add((long.MinValue, long.MaxValue)); return true;
                    case "BIT": into.Add((0, 1)); return true;
                    case "UNSIGNED-BYTE": into.Add((0, null)); return true;
                    case "BIGNUM":
                        into.Add((null, (BigInteger)long.MinValue - 1));
                        into.Add(((BigInteger)long.MaxValue + 1, null));
                        return true;
                }
                break;
            case NumericType { NumClass: "INTEGER" or "RATIONAL" or "REAL" } num:
            {
                BigInteger? lo = null, hi = null;
                if (num.Low != null)
                {
                    if (CeilingOf(num.Low, num.LowExclusiveP) is not BigInteger l) return false;
                    lo = l;
                }
                if (num.High != null)
                {
                    if (FloorOf(num.High, num.HighExclusiveP) is not BigInteger h) return false;
                    hi = h;
                }
                into.Add((lo, hi));
                return true;
            }
            case MemberType mem:
                foreach (var o in mem.Members)
                    if (AsBig(o) is BigInteger v) into.Add((v, v));
                return true;
        }
        // Classes of numbers that hold no integer (RATIO, FLOAT, COMPLEX, ...).
        if (NumericNode(ct) is string node
            && !NumericAncestorOrSelf(node, "INTEGER") && !NumericAncestorOrSelf("INTEGER", node))
            return true;
        return false;
    }

    private static BigInteger? AsBig(LispObject x) => x switch
    {
        Fixnum f => f.Value,
        Bignum b => b.Value,
        _ => null,
    };

    /// <summary>The least integer above (or at, when inclusive) a rational bound; null for a float.</summary>
    private static BigInteger? CeilingOf(LispObject x, bool exclusive)
    {
        if (AsBig(x) is BigInteger v) return exclusive ? v + 1 : v;
        if (x is Ratio r)
        {
            var q = BigInteger.Divide(r.Numerator, r.Denominator);  // truncates toward zero
            return r.Numerator > 0 ? q + 1 : q;
        }
        return null;
    }

    /// <summary>The greatest integer below (or at, when inclusive) a rational bound; null for a float.</summary>
    private static BigInteger? FloorOf(LispObject x, bool exclusive)
    {
        if (AsBig(x) is BigInteger v) return exclusive ? v - 1 : v;
        if (x is Ratio r)
        {
            var q = BigInteger.Divide(r.Numerator, r.Denominator);
            return r.Numerator < 0 ? q - 1 : q;
        }
        return null;
    }

    /// <summary>Do the inclusive ranges PIECES cover every integer in [lo, hi]?</summary>
    private static bool RangesCover(BigInteger? lo, BigInteger? hi,
        List<(BigInteger? lo, BigInteger? hi)> pieces)
    {
        var cur = lo;  // first integer not yet covered; null = minus infinity
        for (int step = 0; step <= pieces.Count; step++)
        {
            if (cur != null && hi != null && cur > hi) return true;
            bool found = false;
            BigInteger? reach = null;
            foreach (var (plo, phi) in pieces)
            {
                bool starts = plo == null || (cur != null && plo <= cur);
                bool spans = phi == null || cur == null || phi >= cur;
                if (!starts || !spans) continue;
                if (phi == null) return true;
                if (!found || phi > reach) { reach = phi; found = true; }
            }
            if (!found) return false;
            cur = reach + 1;
        }
        return cur != null && hi != null && cur > hi;
    }

    /// <summary>
    /// Does a member other than the I-th one have (NOT the I-th) as itself or as
    /// a conjunct?
    /// </summary>
    private static bool OtherMemberNegates(CType[] types, int i)
    {
        for (int j = 0; j < types.Length; j++)
        {
            if (j == i) continue;
            var parts = types[j] is IntersectionType it ? it.Types : new[] { types[j] };
            foreach (var p in parts)
                if (p is NegationType neg && StructurallyEqual(neg.Inner, types[i])) return true;
        }
        return false;
    }

    /// <summary>
    /// Types that partition a built-in type, or null. SHORT-FLOAT is SINGLE-FLOAT
    /// and LONG-FLOAT is DOUBLE-FLOAT here. A SEQUENCE is a list or a vector: no
    /// other object is one (TYPEP), and a class cannot have SEQUENCE as a superclass.
    /// </summary>
    private static string[]? TypePartition(string name) => name switch
    {
        "LIST" => new[] { "CONS", "NULL" },
        "SEQUENCE" => new[] { "LIST", "VECTOR" },
        "NUMBER" => new[] { "REAL", "COMPLEX" },
        "REAL" => new[] { "RATIONAL", "FLOAT" },
        "RATIONAL" => new[] { "INTEGER", "RATIO" },
        "INTEGER" or "SIGNED-BYTE" => new[] { "FIXNUM", "BIGNUM" },
        "FLOAT" => new[] { "SINGLE-FLOAT", "DOUBLE-FLOAT" },
        _ => null,
    };

    /// <summary>The complex representations a type holds, if it holds only complexes.</summary>
    private static int? ComplexMask(CType ct) => ct switch
    {
        ComplexType { Exact: null } c => c.Mask,
        NamedType { Name: "COMPLEX" } => Runtime.ComplexAllParts,
        _ => null,
    };

    /// <summary>
    /// The node of a numeric type in the class tree NUMBER > REAL, COMPLEX;
    /// REAL > RATIONAL, FLOAT; RATIONAL > INTEGER, RATIO; INTEGER > FIXNUM, BIGNUM;
    /// FLOAT > SINGLE-FLOAT, DOUBLE-FLOAT. Null for anything else. Ranges of
    /// integers sit at INTEGER, so they are never called disjoint from FIXNUM.
    /// </summary>
    private static string? NumericNode(CType ct)
    {
        string? name = ct switch
        {
            NamedType n => n.Name,
            NumericType num => num.NumClass,
            ComplexType => "COMPLEX",
            _ => null,
        };
        return name switch
        {
            "NUMBER" or "REAL" or "COMPLEX" or "RATIONAL" or "FLOAT" or "INTEGER"
                or "RATIO" or "FIXNUM" or "BIGNUM" => name,
            "SIGNED-BYTE" or "UNSIGNED-BYTE" or "BIT" => "INTEGER",
            "SINGLE-FLOAT" or "SHORT-FLOAT" => "SINGLE-FLOAT",
            "DOUBLE-FLOAT" or "LONG-FLOAT" => "DOUBLE-FLOAT",
            _ => null,
        };
    }

    private static string? NumericParent(string node) => node switch
    {
        "REAL" or "COMPLEX" => "NUMBER",
        "RATIONAL" or "FLOAT" => "REAL",
        "INTEGER" or "RATIO" => "RATIONAL",
        "FIXNUM" or "BIGNUM" => "INTEGER",
        "SINGLE-FLOAT" or "DOUBLE-FLOAT" => "FLOAT",
        _ => null,
    };

    private static bool NumericAncestorOrSelf(string anc, string node)
    {
        for (string? n = node; n != null; n = NumericParent(n))
            if (n == anc) return true;
        return false;
    }

    /// <summary>
    /// Check if a union type is provably universal (= T).
    /// Detects (OR X (NOT X)) and (OR X (NOT Y)) where Y subset-of X patterns.
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
                    // If neg.Inner subset-of types[j], then (OR types[j] (NOT neg.Inner)) = T
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
        // (AND a b c) <: ct2: sufficient if ANY member <: ct2
        foreach (var member in inter1.Types)
        {
            var (sub, _) = Subtypep(member, ct2);
            if (sub) return (true, true);
        }
        // Check if intersection is empty
        if (IsEmptyIntersection(inter1))
            return (true, true);
        // Try reducing the intersection to a single numeric range.
        // e.g., (AND FIXNUM UNSIGNED-BYTE) -> (INTEGER 0 most-positive-fixnum)
        var reduced = ReduceIntersectionToNumeric(inter1);
        if (reduced != null)
        {
            var (sub2, cert2) = Subtypep(reduced, ct2);
            if (cert2) return (sub2, true);
        }
        return (false, false);
    }

    /// <summary>
    /// Try to reduce all members of an IntersectionType to a single INTEGER range.
    /// Returns null if any member cannot be expressed as an integer range.
    /// </summary>
    private static NumericType? ReduceIntersectionToNumeric(IntersectionType inter)
    {
        LispObject? low = null; bool lowExcl = false;
        LispObject? high = null; bool highExcl = false;

        foreach (var member in inter.Types)
        {
            NumericType? num = member is NumericType n ? n
                             : member is NamedType named ? NamedToNumericType(named.Name)
                             : null;
            if (num == null || num.NumClass != "INTEGER") return null;

            // Intersect lower bounds: take the maximum
            if (num.Low != null)
            {
                if (low == null)
                {
                    low = num.Low; lowExcl = num.LowExclusiveP;
                }
                else
                {
                    int cmp = CompareBounds(num.Low, low);
                    if (cmp == int.MinValue) return null;
                    if (cmp > 0) { low = num.Low; lowExcl = num.LowExclusiveP; }
                    else if (cmp == 0 && num.LowExclusiveP) lowExcl = true;
                }
            }

            // Intersect upper bounds: take the minimum
            if (num.High != null)
            {
                if (high == null)
                {
                    high = num.High; highExcl = num.HighExclusiveP;
                }
                else
                {
                    int cmp = CompareBounds(num.High, high);
                    if (cmp == int.MinValue) return null;
                    if (cmp < 0) { high = num.High; highExcl = num.HighExclusiveP; }
                    else if (cmp == 0 && num.HighExclusiveP) highExcl = true;
                }
            }
        }
        return new NumericType("INTEGER", low, lowExcl, high, highExcl);
    }

    /// <summary>
    /// Check if an intersection type is provably empty.
    /// Detects (AND X (NOT X)) and (AND X (NOT Y)) where X subset-of Y patterns.
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
                    if (sub && cert) return true;  // types[j] subset-of neg.Inner -> types[j] AND (NOT neg.Inner) = empty
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
        // A NamedType is a built-in or unknown name (a symbol naming a class parses to
        // ClassCType), so only dotcl's own classes are looked up by name here.
        if (Runtime.FindSystemClassByName(n1.Name) is LispClass cls1)
        {
            foreach (var c in cls1.ClassPrecedenceList)
                if (c.Name.Name == n2.Name) return (true, true);
        }
        // Only definitive if BOTH types are known (built-in or CLOS class)
        bool n1Known = Runtime.IsBuiltinTypeName(n1.Name) || Runtime.FindSystemClassByName(n1.Name) != null;
        bool n2Known = Runtime.IsBuiltinTypeName(n2.Name) || Runtime.FindSystemClassByName(n2.Name) != null;
        if (n1Known && n2Known) return (false, true);
        return (false, false);  // Unknown type; can't be sure
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

        // Check interval containment: [low1, high1] subset-of [low2, high2]
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
        // Only normalize for integer types: exclusive -> inclusive by +/-1
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
        // (INTEGER 0 10) <: NUMBER -> true if INTEGER <: NUMBER
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
        // INTEGER <: (INTEGER 0 10) -> only if named is a bounded subrange
        if (!Runtime.CheckSubtypeByName(named.Name, num.NumClass))
            return (false, Runtime.IsBuiltinTypeName(named.Name));
        if (num.Low != null || num.High != null)
            return (false, false);  // unbounded named vs bounded numeric; uncertain
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
        // outer is unbounded -> always contained
        if (outer == null) return true;
        // inner is unbounded but outer is bounded -> not contained
        if (inner == null) return false;

        // Compare bound values
        int cmp = CompareBounds(inner, outer);
        if (cmp == int.MinValue) return false;  // incomparable

        if (isLow)
        {
            // For low bounds: inner_low >= outer_low
            if (cmp > 0) return true;
            if (cmp < 0) return false;
            // Equal: inner_excl=T, outer_excl=F -> inner is tighter -> contained
            // inner_excl=F, outer_excl=T -> inner includes boundary, outer doesn't -> not contained
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

    /// <summary>True when element types E1 and E2 upgrade to the same
    /// specialized (not T) array element type.</summary>
    private static bool SameSpecializedUpgrade(LispObject e1, LispObject e2)
    {
        try
        {
            var u1 = Runtime.ParseElementTypeName(e1);
            return u1 != "T" && u1 == Runtime.ParseElementTypeName(e2);
        }
        catch (LispErrorException) { return false; }
    }

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
            // Two element types that are not the same type can still upgrade
            // to the same specialized one: (integer 0 100) and (unsigned-byte 7)
            // both make (unsigned-byte 7) arrays, so the array types are the
            // same (CLHS 15.1.2.1). Only specialized element types are compared
            // this way: T is also what an unknown type name upgrades to.
            if ((!sub1 || !sub2) && a1.ElementSpec != null && a2.ElementSpec != null
                && SameSpecializedUpgrade(a1.ElementSpec, a2.ElementSpec))
                sub1 = sub2 = true;
            if (!sub1 || !sub2) return (false, true);  // element types differ
        }

        // Dimensions
        if (a2.Dimensions != null)
        {
            if (a1.Dimensions == null) return (false, false);
            // Both have dimensions: compare
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
                    // Both are specific dimensions: must match for subtype
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
        // A NamedType that is neither built-in nor one of dotcl's own classes came
        // from a symbol naming nothing (a symbol naming a class parses to ClassCType),
        // so a same-named class in the CPL is some other package's class.
        if (!Runtime.IsBuiltinTypeName(n2.Name) && n2.Name is not ("T" or "NIL" or "*")
            && Runtime.FindSystemClassByName(n2.Name) == null)
            return (false, false);
        // Check CPL for the named type
        foreach (var c in c1.Class.ClassPrecedenceList)
            if (c.Name.Name == n2.Name) return (true, true);
        // Also check built-in hierarchy for structure-object etc.
        if (c1.Class.IsStructureClass && Runtime.CheckSubtypeByName("STRUCTURE-OBJECT", n2.Name))
            return (true, true);
        return (false, false);  // conservative; named type might be a deftype alias
    }

    private static (bool, bool) SubtypepNamedClass(NamedType n1, ClassCType c2)
    {
        // Named type <: Class type: check if the named type is a known subclass
        if (Runtime.FindSystemClassByName(n1.Name) is LispClass cls)
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
        // Fast path: (MEMBER a b c) <: (MEMBER ...): check set containment
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
        // for satisfies-involving types.
        // A type name that names no type gives no answer either: TYPEP on it is
        // not a fact about the objects.
        bool uncertainNo = ContainsSatisfies(ct2) || ContainsUnknownName(ct2);
        foreach (var obj in mem.Members)
        {
            if (!CTypeOps.IsTypep(obj, ct2))
                return (false, !uncertainNo);
        }
        return (true, true);
    }

    /// <summary>
    /// True if ct (or any nested type) is a name that is neither a built-in type
    /// nor one of dotcl's own classes: a symbol that names no type at all, or a
    /// DEFTYPE whose expansion failed or was circular.
    /// </summary>
    internal static bool ContainsUnknownName(CType ct) => ct switch
    {
        NamedType n => n.Name is not ("T" or "NIL" or "*")
            && !Runtime.IsBuiltinTypeName(n.Name)
            && Runtime.FindSystemClassByName(n.Name) == null,
        NegationType neg => ContainsUnknownName(neg.Inner),
        UnionType u => System.Array.Exists(u.Types, ContainsUnknownName),
        IntersectionType i => System.Array.Exists(i.Types, ContainsUnknownName),
        ConsType c => ContainsUnknownName(c.CarType) || ContainsUnknownName(c.CdrType),
        _ => false,
    };

    /// <summary>
    /// An object that is of TYPE1 but not of TYPE2 shows TYPE1 is not a subtype of
    /// TYPE2. Tried: the objects named by a MEMBER or EQL in either type, then
    /// objects of many built-in kinds (s_witnesses): VECTOR is not a subtype of
    /// (OR STRING BIT-VECTOR) because #(1 2) is neither. Types with SATISFIES are
    /// not tried (the predicate could do anything), nor are objects whose TYPEP fails.
    /// </summary>
    public static bool HasMemberWitness(CType ct1, CType ct2, LispObject type1, LispObject type2)
    {
        if (ContainsSatisfies(ct1) || ContainsSatisfies(ct2)) return false;
        if (ContainsUnknownName(ct1) || ContainsUnknownName(ct2)) return false;
        var objs = new List<LispObject>();
        CollectMembers(ct1, objs);
        CollectMembers(ct2, objs);
        foreach (var o in objs)
        {
            try
            {
                if (Runtime.Typep(o, type1) != Nil.Instance && Runtime.Typep(o, type2) == Nil.Instance)
                    return true;
            }
            catch { }
        }
        // The built-in objects only where TYPEP cannot signal (a handler of the
        // caller would see it) and agrees with this algebra: no array type with an
        // element type (TYPEP looks at the elements' types where the algebra looks
        // at the upgraded element type), no VALUES. Asked of the parsed types, so
        // (FUNCTION (T) T) is tested as FUNCTION, as everywhere here.
        if (!WitnessSafe(ct1) || !WitnessSafe(ct2)) return false;
        foreach (var o in s_witnesses.Value)
        {
            if (IsTypep(o, ct1) && !IsTypep(o, ct2))
                return true;
        }
        return false;
    }

    private static bool WitnessSafe(CType ct) => ct switch
    {
        NamedType n => n.Name is not ("VALUES" or "*"),
        NumericType or MemberType or ClassCType or ComplexType => true,
        ConsType c => WitnessSafe(c.CarType) && WitnessSafe(c.CdrType),
        NegationType neg => WitnessSafe(neg.Inner),
        UnionType u => System.Array.TrueForAll(u.Types, WitnessSafe),
        IntersectionType i => System.Array.TrueForAll(i.Types, WitnessSafe),
        _ => false,
    };

    /// <summary>
    /// One object of each of many built-in kinds: numbers of every class and both
    /// signs, symbols, conses, strings and other vectors simple or not, arrays of
    /// rank 0 and 2, characters, a hash table, a package, a function. Read once
    /// under the standard syntax.
    /// </summary>
    private static readonly Lazy<LispObject[]> s_witnesses = new(() =>
    {
        const string src =
            "(0 1 -1 2 100000000000000000000000 -100000000000000000000000 1/2 -1/2" +
            " 1.5f0 -1.5f0 1.5d0 #c(1 2) #c(1.5f0 2.5f0) #c(1.5d0 2.5d0)" +
            " cl:nil cl:t #:w :w #\\a (:w) (1 . 2) \"abc\" #(1 2) #() #*101 #2a((1 2)) #0a1" +
            " #.(cl:make-array 3 :element-type 'cl:character :adjustable cl:t :fill-pointer 1)" +
            " #.(cl:make-array 3 :adjustable cl:t :fill-pointer 1)" +
            " #.(cl:make-array 3 :element-type 'cl:bit :adjustable cl:t)" +
            " #.(cl:make-array 2 :element-type 'cl:fixnum :initial-element 0)" +
            " #.(cl:make-array 2 :element-type 'cl:double-float :initial-element 0d0)" +
            " #.(cl:make-hash-table) #.(cl:find-package \"COMMON-LISP\") #.(cl:function cl:car))";
        var syms = new[] { "*READTABLE*", "*READ-BASE*", "*READ-SUPPRESS*", "*READ-EVAL*",
                           "*READ-DEFAULT-FLOAT-FORMAT*" };
        var vals = new LispObject[] { Startup.FaslReadtable, Fixnum.Make(10), Nil.Instance, T.Instance,
                                      Startup.Sym("SINGLE-FLOAT") };
        for (int i = 0; i < syms.Length; i++) DynamicBindings.Push(Startup.Sym(syms[i]), vals[i]);
        try
        {
            var list = Runtime.ReadFromString(new LispObject[] { new LispString(src) });
            if (list is MvReturn mv) list = mv.PrimaryValue;
            var objs = new List<LispObject>();
            for (var p = list; p is Cons c; p = c.Cdr) objs.Add(c.Car);
            return objs.ToArray();
        }
        catch { return System.Array.Empty<LispObject>(); }
        finally
        {
            for (int i = syms.Length - 1; i >= 0; i--) DynamicBindings.Pop(Startup.Sym(syms[i]));
        }
    });

    private static void CollectMembers(CType ct, List<LispObject> into)
    {
        switch (ct)
        {
            case MemberType m: into.AddRange(m.Members); break;
            case NegationType n: CollectMembers(n.Inner, into); break;
            case UnionType u: foreach (var t in u.Types) CollectMembers(t, into); break;
            case IntersectionType i: foreach (var t in i.Types) CollectMembers(t, into); break;
            case ConsType: break;
        }
    }

    /// <summary>Returns true if ct (or any nested type) is a SatisfiesType.</summary>
    private static bool ContainsSatisfies(CType ct) =>
        ct is SatisfiesType ||
        (ct is NegationType neg && ContainsSatisfies(neg.Inner)) ||
        (ct is UnionType u && System.Array.Exists(u.Types, ContainsSatisfies)) ||
        (ct is IntersectionType i && System.Array.Exists(i.Types, ContainsSatisfies)) ||
        (ct is ConsType c && (ContainsSatisfies(c.CarType) || ContainsSatisfies(c.CdrType))) ||
        (ct is ArrayType a && ContainsSatisfies(a.ElementType));

    // --- IsTypep: check if a value is of a CType (for MemberType) ---

    /// <summary>Check if obj is of the given CType. Used by MemberType subtypep.</summary>
    public static bool IsTypep(LispObject obj, CType ct)
    {
        // Convert CType back to specifier and use existing Typep
        // This is a temporary bridge: Phase 3 will replace Typep entirely
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
            case ComplexType cx:
                return Runtime.List(Startup.Sym("COMPLEX"), cx.PartSpec);
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
            case SatisfiesType sat:
                return new Cons(Startup.Sym("SATISFIES"), new Cons(sat.Predicate, Nil.Instance));
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
        if (ComplexMask(a) is int ma && ComplexMask(b) is int mb) return (ma & mb) == 0;
        if (a is ComplexType ca && b is ComplexType cb && (ca.Mask & cb.Mask) == 0) return true;
        if (a is ComplexType ca2 && ComplexMask(b) is int mb2 && (ca2.Mask & mb2) == 0) return true;
        if (b is ComplexType cb2 && ComplexMask(a) is int ma2 && (cb2.Mask & ma2) == 0) return true;
        if ((a is ComplexType || ComplexMask(a) != null) && (b is ComplexType || ComplexMask(b) != null))
            return false;
        if (a is ComplexType) return AreDisjoint(NamedType.Get("COMPLEX"), b);
        if (b is ComplexType) return AreDisjoint(a, NamedType.Get("COMPLEX"));

        // If either is a subtype of the other, not disjoint
        var (sub1, cert1) = Subtypep(a, b);
        if (sub1) return false;
        var (sub2, cert2) = Subtypep(b, a);
        if (sub2) return false;

        // Two numeric classes neither of which contains the other: INTEGER and RATIO,
        // (INTEGER 0 *) and FLOAT.
        if (NumericNode(a) is string na && NumericNode(b) is string nb
            && !NumericAncestorOrSelf(na, nb) && !NumericAncestorOrSelf(nb, na))
            return true;

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
        // CONS group (only CONS itself: LIST includes NIL which is SYMBOL)
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
        if (ct is NumericType or ComplexType) return "NUMBER";
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
