namespace DotCL;

// (dotcl:valid-type-specifier-p object): T when OBJECT is a well-formed type
// specifier, NIL otherwise. It never signals.
//
// This is a validator of its own, separate from TYPEP: it looks inside the
// arguments of a compound specifier (the bounds of INTEGER, the dimensions of
// ARRAY, the shape of a FUNCTION lambda list) instead of waiting for a type
// test to trip over them. Nothing here is consulted by TYPEP or the compiler.

public static partial class Runtime
{
    private const int VtsMaxDepth = 100;
    private const int VtsMaxListLength = 1 << 20;

    public static LispObject ValidTypeSpecifierP(LispObject[] args)
    {
        if (args.Length != 1)
            throw new LispErrorException(new LispProgramError(
                $"VALID-TYPE-SPECIFIER-P: wrong number of arguments: {args.Length} (expected 1)"));
        bool ok;
        try { ok = VtsValid(args[0], 0, true); }
        catch (Exception) { ok = false; }
        return ok ? T.Instance : Nil.Instance;
    }

    private static bool VtsIsStar(LispObject x) =>
        x is Symbol s && s.Name == "*" && ReferenceEquals(s.HomePackage, Startup.CL);

    private static bool VtsProperList(LispObject x, out List<LispObject> items)
    {
        items = new List<LispObject>();
        while (x is Cons c)
        {
            if (items.Count >= VtsMaxListLength) return false;
            items.Add(c.Car);
            x = c.Cdr;
        }
        return x is Nil;
    }

    private static bool VtsIsInteger(LispObject x) => x is Fixnum || x is Bignum;

    private static bool VtsNonNegInteger(LispObject x) =>
        x is Fixnum f ? f.Value >= 0 : x is Bignum b && b.Value.Sign >= 0;

    private static bool VtsPositiveInteger(LispObject x) =>
        x is Fixnum f ? f.Value > 0 : x is Bignum b && b.Value.Sign > 0;

    private static bool VtsIsRational(LispObject x) => VtsIsInteger(x) || x is Ratio;

    private static bool VtsIsReal(LispObject x) =>
        VtsIsRational(x) || x is SingleFloat || x is DoubleFloat || x.GetType() == typeof(LispDecimal);

    private static bool VtsValid(LispObject spec, int depth, bool allowValues)
    {
        if (depth > VtsMaxDepth) return false;
        switch (spec)
        {
            case T:
            case Nil:
            case LispClass:
                return true;
            case Symbol sym:
                return VtsAtomic(sym, depth, allowValues);
            case Cons c:
                return VtsCompound(c, depth, allowValues);
            default:
                return false;
        }
    }

    // Expand a DEFTYPE and validate the expansion. A failing expander is NIL.
    private static bool VtsExpand(LispObject expander, LispObject[] args, int depth, bool allowValues)
    {
        LispObject expansion;
        try { expansion = Primary(Funcall(expander, args)); }
        catch (Exception) { return false; }
        return VtsValid(expansion, depth + 1, allowValues);
    }

    private static bool VtsAtomic(Symbol sym, int depth, bool allowValues)
    {
        if (TryGetQualifiedTypeExpander(sym, out var qExp))
            return VtsExpand(qExp, Array.Empty<LispObject>(), depth, allowValues);
        var home = sym.HomePackage;
        if ((ReferenceEquals(home, Startup.CL) || ReferenceEquals(home, Startup.Internal)
                || ReferenceEquals(home, Startup.DotclPkg))
            && sym.Name != "VALUES"
            && (IsBuiltinTypeName(sym.Name) || sym.Name == "*"))
            return true;
        if (TryGetTypeExpander(sym, out var exp))
            return VtsExpand(exp, Array.Empty<LispObject>(), depth, allowValues);
        return FindClassOrNil(sym) is LispClass || FindCompileTimeClass(sym) is LispClass;
    }

    private static bool VtsCompound(Cons form, int depth, bool allowValues)
    {
        if (form.Car is not Symbol head) return false;
        if (!VtsProperList(form.Cdr, out var args)) return false;
        bool isCl = ReferenceEquals(head.HomePackage, Startup.CL);
        if (!isCl && TryGetTypeExpander(head, out var userExp))
            return VtsExpand(userExp, args.ToArray(), depth, allowValues);
        if (!isCl) return false;
        if (!CompoundTypeArgLimits.TryGetValue(head.Name, out var lim))
        {
            if (TryGetTypeExpander(head, out var clExp))
                return VtsExpand(clExp, args.ToArray(), depth, allowValues);
            return false;
        }
        int n = args.Count;
        if (n < lim.Min || n > lim.Max) return false;
        if (lim.Max == 0) return true;
        int d = depth + 1;
        switch (head.Name)
        {
            case "AND":
            case "OR":
                foreach (var a in args) if (!VtsValid(a, d, false)) return false;
                return true;
            case "NOT":
                return VtsValid(args[0], d, false);
            case "MEMBER":
            case "EQL":
                return true;
            case "SATISFIES":
                return args[0] is Symbol || args[0] is T || args[0] is Nil;
            case "MOD":
                return VtsPositiveInteger(args[0]);
            case "UNSIGNED-BYTE":
            case "SIGNED-BYTE":
                return n == 0 || VtsIsStar(args[0]) || VtsPositiveInteger(args[0]);
            case "INTEGER":
                return VtsBounds(args, x => VtsIsInteger(x));
            case "RATIONAL":
                return VtsBounds(args, x => VtsIsRational(x));
            case "REAL":
            case "FLOAT":
                return VtsBounds(args, x => VtsIsReal(x));
            case "SHORT-FLOAT":
            case "SINGLE-FLOAT":
                return VtsBounds(args, x => x is SingleFloat);
            case "DOUBLE-FLOAT":
            case "LONG-FLOAT":
                return VtsBounds(args, x => x is DoubleFloat);
            case "COMPLEX":
                if (n == 0 || VtsIsStar(args[0])) return true;
                if (!VtsValid(args[0], d, false)) return false;
                try { return IsTruthy(Subtypep(args[0], Startup.Sym("REAL"))); }
                catch (Exception) { return false; }
            case "CONS":
                foreach (var a in args) if (!VtsValid(a, d, false)) return false;
                return true;
            case "ARRAY":
            case "SIMPLE-ARRAY":
                if (n >= 1 && !VtsValid(args[0], d, false)) return false;
                return n < 2 || VtsDimensions(args[1]);
            case "VECTOR":
                if (n >= 1 && !VtsValid(args[0], d, false)) return false;
                return n < 2 || VtsIsStar(args[1]) || VtsNonNegInteger(args[1]);
            case "SIMPLE-VECTOR":
            case "STRING":
            case "SIMPLE-STRING":
            case "BASE-STRING":
            case "SIMPLE-BASE-STRING":
            case "BIT-VECTOR":
            case "SIMPLE-BIT-VECTOR":
                return n == 0 || VtsIsStar(args[0]) || VtsNonNegInteger(args[0]);
            case "FUNCTION":
                if (n >= 1 && !(VtsIsStar(args[0]) || VtsFunctionArgs(args[0], d))) return false;
                return n < 2 || VtsValid(args[1], d, true);
            case "VALUES":
                return allowValues && VtsValuesShape(args, d);
            default:
                return false;
        }
    }

    // A bound is *, a number of the right kind, or a one-element list of one.
    private static bool VtsBounds(List<LispObject> args, Func<LispObject, bool> kind)
    {
        foreach (var b in args)
        {
            if (VtsIsStar(b) || kind(b)) continue;
            if (b is Cons c && c.Cdr is Nil && kind(c.Car)) continue;
            return false;
        }
        return true;
    }

    // ARRAY dimensions: *, a rank, or a list of * / dimension sizes.
    private static bool VtsDimensions(LispObject dims)
    {
        const int rankLimit = 32;
        if (VtsIsStar(dims)) return true;
        if (VtsNonNegInteger(dims)) return dims is Fixnum f && f.Value < rankLimit;
        if (!VtsProperList(dims, out var items) || items.Count >= rankLimit) return false;
        foreach (var x in items)
            if (!(VtsIsStar(x) || VtsNonNegInteger(x))) return false;
        return true;
    }

    private static string? VtsLambdaKeyword(LispObject x) =>
        x is Symbol s && s.Name.Length > 0 && s.Name[0] == '&' ? s.Name : null;

    // FUNCTION lambda list: required* [&optional type*] [&rest type]
    // [&key (name type)* [&allow-other-keys]]
    private static bool VtsFunctionArgs(LispObject lambda, int depth)
    {
        if (!VtsProperList(lambda, out var items)) return false;
        int state = 0; // 0 required, 1 optional, 2 after &rest, 3 key
        bool needRestType = false, allowOther = false;
        HashSet<LispObject>? keys = null;
        foreach (var x in items)
        {
            if (needRestType)
            {
                if (VtsLambdaKeyword(x) != null || !VtsValid(x, depth, false)) return false;
                needRestType = false;
                continue;
            }
            if (allowOther) return false;
            var kw = VtsLambdaKeyword(x);
            if (kw != null)
            {
                switch (kw)
                {
                    case "&OPTIONAL":
                        if (state != 0) return false;
                        state = 1;
                        break;
                    case "&REST":
                        if (state > 1) return false;
                        state = 2;
                        needRestType = true;
                        break;
                    case "&KEY":
                        if (state == 3) return false;
                        state = 3;
                        keys = new HashSet<LispObject>();
                        break;
                    case "&ALLOW-OTHER-KEYS":
                        if (state != 3) return false;
                        allowOther = true;
                        break;
                    default:
                        return false;
                }
                continue;
            }
            if (state == 3)
            {
                if (!VtsProperList(x, out var pair) || pair.Count != 2) return false;
                var k = pair[0];
                if (!(k is Symbol || k is T || k is Nil) || !keys!.Add(k)) return false;
                if (!VtsValid(pair[1], depth, false)) return false;
            }
            else if (state == 2)
                return false;
            else if (!VtsValid(x, depth, false))
                return false;
        }
        return !needRestType;
    }

    // VALUES: required* [&optional type*] [&rest type]
    private static bool VtsValuesShape(List<LispObject> items, int depth)
    {
        int state = 0; // 0 required, 1 optional, 2 after &rest
        bool needRestType = false;
        foreach (var x in items)
        {
            if (needRestType)
            {
                if (VtsLambdaKeyword(x) != null || !VtsValid(x, depth, false)) return false;
                needRestType = false;
                continue;
            }
            var kw = VtsLambdaKeyword(x);
            if (kw != null)
            {
                if (kw == "&OPTIONAL" && state == 0) state = 1;
                else if (kw == "&REST" && state <= 1) { state = 2; needRestType = true; }
                else return false;
                continue;
            }
            if (state == 2 || !VtsValid(x, depth, false)) return false;
        }
        return !needRestType;
    }
}
