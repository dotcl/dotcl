namespace DotCL;

// Type tests built once from a type specifier and reused.
//
// TYPEP interprets its specifier on every call: a symbol goes through a chain
// of class-table, deftype-table and built-in-name checks before the one that
// decides, and (OR NULL FOO) does that for each branch. Structure slot type
// checks and code that tests against a type known only at run time ask the
// same few specifiers over and over, so the decision is worked out once per
// specifier here and kept.
//
// The built tests only ever answer where every path of the general TYPEP
// provably arrives at the same answer; anything else, including every object
// kind with a class-based rule (instances, conditions, class metaobjects,
// .NET objects), is handed back to the general TYPEP. A test built for a
// symbol depends on the class and deftype tables and on the symbol's home
// package, and is rebuilt when any of those changes.
public static partial class Runtime
{
    internal abstract class TypeTest
    {
        internal abstract bool Test(LispObject obj);
    }

    /// <summary>The general TYPEP, for what the built tests do not cover.</summary>
    private sealed class GeneralTypeTest : TypeTest
    {
        private readonly LispObject _spec;
        internal GeneralTypeTest(LispObject spec) { _spec = spec; }
        internal override bool Test(LispObject obj) => TypepGeneral(obj, _spec) is not Nil;
    }

    private sealed class ConstTypeTest : TypeTest
    {
        internal static readonly ConstTypeTest True = new(true), False = new(false);
        private readonly bool _value;
        private ConstTypeTest(bool value) { _value = value; }
        internal override bool Test(LispObject obj) => _value;
    }

    /// <summary>A symbol leaf inside a compound specifier: asks TYPEP of the
    /// symbol at test time, so the compound test itself never goes stale.</summary>
    private sealed class SymbolLeafTypeTest : TypeTest
    {
        private readonly Symbol _sym;
        internal SymbolLeafTypeTest(Symbol sym) { _sym = sym; }
        internal override bool Test(LispObject obj)
        {
            var t = SymbolTypeTest(_sym);
            return t != null ? t.Test(obj) : TypepGeneral(obj, _sym) is not Nil;
        }
    }

    private sealed class OrTypeTest : TypeTest
    {
        private readonly TypeTest[] _parts;
        internal OrTypeTest(TypeTest[] parts) { _parts = parts; }
        internal override bool Test(LispObject obj)
        {
            foreach (var p in _parts) if (p.Test(obj)) return true;
            return false;
        }
    }

    private sealed class AndTypeTest : TypeTest
    {
        private readonly TypeTest[] _parts;
        internal AndTypeTest(TypeTest[] parts) { _parts = parts; }
        internal override bool Test(LispObject obj)
        {
            foreach (var p in _parts) if (!p.Test(obj)) return false;
            return true;
        }
    }

    private sealed class NotTypeTest : TypeTest
    {
        private readonly TypeTest _part;
        internal NotTypeTest(TypeTest part) { _part = part; }
        internal override bool Test(LispObject obj) => !_part.Test(obj);
    }

    private sealed class MemberTypeTest : TypeTest
    {
        private readonly LispObject[] _items;
        internal MemberTypeTest(LispObject[] items) { _items = items; }
        internal override bool Test(LispObject obj)
        {
            foreach (var x in _items) if (IsTrueEql(obj, x)) return true;
            return false;
        }
    }

    /// <summary>(INTEGER low high) with fixnum bounds; a non-fixnum object goes
    /// to the general TYPEP.</summary>
    private sealed class FixnumRangeTypeTest : TypeTest
    {
        private readonly long _lo, _hi;
        private readonly LispObject _spec;
        internal FixnumRangeTypeTest(long lo, long hi, LispObject spec) { _lo = lo; _hi = hi; _spec = spec; }
        internal override bool Test(LispObject obj)
            => obj is Fixnum f ? f.Value >= _lo && f.Value <= _hi : TypepGeneral(obj, _spec) is not Nil;
    }

    /// <summary>Objects whose TYPEP against a symbol is decided by their .NET
    /// representation alone: no class precedence list, name match, or
    /// metaobject rule applies to them in the general TYPEP.</summary>
    private static bool IsPlainTypepObject(LispObject obj)
        => obj is Nil || obj is T || obj is Fixnum || obj is Cons || obj is Symbol
           || obj is LispString || obj is LispChar || obj is LispVector
           || obj is Bignum || obj is DoubleFloat || obj is SingleFloat || obj is Ratio;

    /// <summary>A COMMON-LISP type name whose general TYPEP answer is this
    /// predicate for plain objects and structures (a structure matches none of
    /// them but ATOM, unless it is itself named by the symbol).</summary>
    private sealed class ClNameTypeTest : TypeTest
    {
        private readonly Symbol _sym;
        private readonly System.Func<LispObject, bool> _pred;
        internal ClNameTypeTest(Symbol sym, System.Func<LispObject, bool> pred) { _sym = sym; _pred = pred; }
        internal override bool Test(LispObject obj)
        {
            if (obj is LispStruct st) return ReferenceEquals(st.TypeName, _sym) || _pred(st);
            if (IsPlainTypepObject(obj)) return _pred(obj);
            return TypepGeneral(obj, _sym) is not Nil;
        }
    }

    private static System.Func<LispObject, bool>? ClNamePredicate(string name) => name switch
    {
        "NULL" => static o => o is Nil,
        "CONS" => static o => o is Cons,
        "LIST" => static o => o is Cons || o is Nil,
        "SYMBOL" => static o => o is Symbol || o is Nil || o is T,
        "ATOM" => static o => o is not Cons,
        "BOOLEAN" => static o => o is Nil || o is T,
        "KEYWORD" => static o => o is Symbol s && s.HomePackage?.Name == "KEYWORD",
        "FIXNUM" => static o => o is Fixnum,
        "INTEGER" => static o => o is Fixnum || o is Bignum,
        "CHARACTER" => static o => o is LispChar,
        "STRING" => static o => o is LispString || (o is LispVector v && v.IsCharVector && v.Rank == 1),
        "SIMPLE-STRING" => static o => o is LispString || (o is LispVector v && v.IsCharVector && v.IsSimple && v.Rank == 1),
        "VECTOR" => static o => (o is LispVector v && v.Rank == 1) || o is LispString,
        "SIMPLE-VECTOR" => static o => o is LispVector v && v.Rank == 1 && !v.IsCharVector && !v.IsBitVector
                                       && v.IsSimple && v.ElementTypeName == "T",
        _ => null
    };

    /// <summary>A symbol naming a structure class (and nothing else: no DEFTYPE,
    /// not a built-in type name). A plain object is never of that type; a
    /// structure is decided by its class precedence list, as the general path
    /// does, except where that path matches by name.</summary>
    private sealed class StructClassTypeTest : TypeTest
    {
        private readonly Symbol _sym;
        private readonly LispClass _cls;
        internal StructClassTypeTest(Symbol sym, LispClass cls) { _sym = sym; _cls = cls; }
        internal override bool Test(LispObject obj)
        {
            if (obj is LispStruct st)
            {
                if (ReferenceEquals(st.TypeName, _sym)) return true;
                if (st.TypeName.Name != _sym.Name && FindClassOrNil(st.TypeName) is LispClass own)
                {
                    foreach (var c in own.ClassPrecedenceList)
                        if (ReferenceEquals(c, _cls)) return true;
                    return false;
                }
                return TypepGeneral(obj, _sym) is not Nil;
            }
            if (IsPlainTypepObject(obj)) return false;
            return TypepGeneral(obj, _sym) is not Nil;
        }
    }

    /// <summary>A DEFTYPE'd symbol outside COMMON-LISP that is not also a
    /// built-in type name: a plain object is tested against the expansion.</summary>
    private sealed class DeftypeTypeTest : TypeTest
    {
        private readonly Symbol _sym;
        private readonly TypeTest _expansion;
        internal DeftypeTypeTest(Symbol sym, TypeTest expansion) { _sym = sym; _expansion = expansion; }
        internal override bool Test(LispObject obj)
            => IsPlainTypepObject(obj) ? _expansion.Test(obj) : TypepGeneral(obj, _sym) is not Nil;
    }

    internal sealed class SymbolTypeTestMemo
    {
        internal readonly int ClassEpoch, ExpanderEpoch;
        internal readonly Package? Package;
        internal readonly TypeTest? Test;
        internal SymbolTypeTestMemo(int classEpoch, int expanderEpoch, Package? pkg, TypeTest? test)
        { ClassEpoch = classEpoch; ExpanderEpoch = expanderEpoch; Package = pkg; Test = test; }
    }

    /// <summary>The built test for SYM as a type specifier, or null when only
    /// the general TYPEP can answer.</summary>
    internal static TypeTest? SymbolTypeTest(Symbol sym)
    {
        int ce = System.Threading.Volatile.Read(ref ClassRegistry.Epoch);
        int ee = System.Threading.Volatile.Read(ref TypeExpanderEpoch);
        var pkg = sym.HomePackage;
        var memo = sym.TypeTestMemo;
        if (memo != null && memo.ClassEpoch == ce && memo.ExpanderEpoch == ee
            && ReferenceEquals(memo.Package, pkg))
            return memo.Test;
        TypeTest? test;
        try { test = BuildSymbolTypeTest(sym, 0); }
        catch (LispErrorException) { test = null; }
        sym.TypeTestMemo = new SymbolTypeTestMemo(ce, ee, pkg, test);
        return test;
    }

    private static TypeTest? BuildSymbolTypeTest(Symbol sym, int depth)
    {
        var pkg = sym.HomePackage;
        if (pkg == Startup.CL)
        {
            var pred = ClNamePredicate(sym.Name);
            return pred != null ? new ClNameTypeTest(sym, pred) : null;
        }
        if (pkg == null || IsBuiltinTypeSymbol(sym) || sym.Name is "T" or "NIL" or "VALUES")
            return null;
        if (TryGetTypeExpander(sym, out var expander))
        {
            if (FindClassOrNil(sym) is LispClass || depth > 16) return null;
            var expansion = ExpandTypeSymbol(sym, expander);
            return new DeftypeTypeTest(sym, BuildTypeTest(expansion, depth + 1));
        }
        if (FindClassOrNil(sym) is LispClass cls && cls.IsStructureClass && ReferenceEquals(cls.Name, sym))
            return new StructClassTypeTest(sym, cls);
        return null;
    }

    /// <summary>A test for SPEC. Compound specifiers are built structurally
    /// from their COMMON-LISP operators; a symbol inside one is looked up at
    /// test time.</summary>
    internal static TypeTest BuildTypeTest(LispObject spec, int depth = 0)
    {
        if (spec is T) return ConstTypeTest.True;
        if (spec is Nil) return ConstTypeTest.False;
        if (depth > 32) return new GeneralTypeTest(spec);
        if (spec is Symbol sym) return new SymbolLeafTypeTest(sym);
        if (spec is not Cons c || c.Car is not Symbol head || head.HomePackage != Startup.CL)
            return new GeneralTypeTest(spec);
        switch (head.Name)
        {
            case "OR":
            case "AND":
            {
                var parts = new System.Collections.Generic.List<TypeTest>();
                for (var cur = c.Cdr; cur is Cons pc; cur = pc.Cdr)
                    parts.Add(BuildTypeTest(pc.Car, depth + 1));
                return head.Name == "OR" ? new OrTypeTest(parts.ToArray()) : new AndTypeTest(parts.ToArray());
            }
            case "NOT":
                if (c.Cdr is Cons nc && nc.Cdr is Nil)
                    return new NotTypeTest(BuildTypeTest(nc.Car, depth + 1));
                break;
            case "MEMBER":
            {
                var items = new System.Collections.Generic.List<LispObject>();
                for (var cur = c.Cdr; cur is Cons mc; cur = mc.Cdr) items.Add(mc.Car);
                return new MemberTypeTest(items.ToArray());
            }
            case "EQL":
                if (c.Cdr is Cons ec && ec.Cdr is Nil)
                    return new MemberTypeTest(new[] { ec.Car });
                break;
            case "INTEGER":
                if (IntegerRangeBound(c.Cdr is Cons lc ? lc.Car : null, true, out long lo)
                    && IntegerRangeBound(c.Cdr is Cons lc2 && lc2.Cdr is Cons hc ? hc.Car : null, false, out long hi))
                    return new FixnumRangeTypeTest(lo, hi, spec);
                break;
        }
        return new GeneralTypeTest(spec);
    }

    /// <summary>An (INTEGER low high) bound as an inclusive long, when it is
    /// absent, *, a fixnum, or a list of one fixnum (exclusive).</summary>
    private static bool IntegerRangeBound(LispObject? b, bool low, out long v)
    {
        v = low ? long.MinValue : long.MaxValue;
        if (b == null || b is Nil || (b is Symbol s && s.Name == "*" && s.HomePackage == Startup.CL)) return true;
        if (b is Fixnum f) { v = f.Value; return true; }
        if (b is Cons bc && bc.Cdr is Nil && bc.Car is Fixnum ef
            && ef.Value != long.MinValue && ef.Value != long.MaxValue)
        {
            v = low ? ef.Value + 1 : ef.Value - 1;
            return true;
        }
        return false;
    }
}
