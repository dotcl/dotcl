namespace DotCL;

/// <summary>The named functions of the DOTCL-SIMD contrib. Built on first use, so a
/// program that never loads the contrib pays nothing at startup.</summary>
public static partial class SimdTable
{
    static Dictionary<string, LispFunction>? _table;
    static readonly object _lock = new();

    public static Dictionary<string, LispFunction> Table
    {
        get
        {
            if (_table != null) return _table;
            lock (_lock) return _table ??= Build();
        }
    }

    static LispErrorException ArgCount(string name, int got, string expected) =>
        new(new LispProgramError($"{name}: wrong number of arguments: {got} (expected {expected})"));

    internal static void Def(Dictionary<string, LispFunction> t, string name, int arity, Func<LispObject[], LispObject> f)
    {
        t[name] = new LispFunction(args =>
        {
            if (arity >= 0 && args.Length != arity) throw ArgCount(name, args.Length, arity.ToString());
            return f(args);
        }, "DOTCL-SIMD:" + name, arity);
    }

    static Dictionary<string, LispFunction> Build()
    {
        var t = new Dictionary<string, LispFunction>();
        for (int ki = 0; ki < SimdInfo.KindCount; ki++)
        {
            var k = (SimdKind)ki;
            string n = SimdInfo.Name(k);
            int lanes = SimdInfo.Lanes(k);
            Def(t, n + "P", 1, a => Runtime.Primary(a[0]) is LispSimdPack p && p.Kind == k ? T.Instance : Nil.Instance);
            Def(t, n, 1, a => SimdInfo.Cast(k, a[0], n));
            Def(t, "MAKE-" + n, lanes, a =>
            {
                var l = new ulong[lanes];
                for (int i = 0; i < lanes; i++) l[i] = SimdInfo.LaneFromLisp(k, a[i], "MAKE-" + n);
                return SimdInfo.FromLanes(k, l);
            });
            Def(t, n + "-VALUES", 1, a =>
            {
                var p = SimdInfo.Cast(k, a[0], n + "-VALUES");
                var v = new LispObject[lanes];
                for (int i = 0; i < lanes; i++) v[i] = SimdInfo.LaneToLisp(k, SimdInfo.Lane(p, i));
                return Runtime.Values(v);
            });
            Def(t, n + "!", 1, a =>
            {
                var o = Runtime.Primary(a[0]);
                if (o is LispSimdPack p) return new LispSimdPack(k, p.Lo, p.Hi);
                if (o is Number) return SimdInfo.Cast(k, o, n + "!");
                throw new LispErrorException(new LispTypeError(
                    $"{n}!: {Runtime.FormatObject(o, true)} is not a SIMD pack or a number", o,
                    Startup.SymInPkg("SIMD-PACK", "DOTCL-SIMD")));
            });
            AddOps(t, k);
            AddArrayOps(t, k);
        }
        AddConversions(t);
        return t;
    }

    public static LispObject Install(LispObject pkgObj)
    {
        if (pkgObj is not Package pkg)
            throw new LispErrorException(new LispTypeError("%SIMD-INSTALL: not a package", pkgObj, Startup.Sym("PACKAGE")));
        foreach (var (name, fn) in Table)
        {
            bool setf = name.StartsWith("(SETF ", StringComparison.Ordinal);
            var (sym, _) = pkg.Intern(setf ? name.Substring(6, name.Length - 7) : name);
            pkg.Export(sym);
            if (setf) sym.SetfFunction = fn; else sym.Function = fn;
        }
        return T.Instance;
    }

    public static LispObject TypeNames()
    {
        LispObject r = Nil.Instance;
        for (int i = SimdInfo.KindCount - 1; i >= 0; i--) r = new Cons(new LispString(SimdInfo.Names[i]), r);
        return r;
    }
}
