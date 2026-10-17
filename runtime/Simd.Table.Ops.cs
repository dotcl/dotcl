namespace DotCL;

public static partial class SimdTable
{
    static readonly (SimdKind To, SimdKind From)[] Conversions =
    {
        (SimdKind.F32x4, SimdKind.S32x4), (SimdKind.F32x4, SimdKind.U32x4),
        (SimdKind.S32x4, SimdKind.F32x4), (SimdKind.U32x4, SimdKind.F32x4),
        (SimdKind.F64x2, SimdKind.S64x2), (SimdKind.F64x2, SimdKind.U64x2),
        (SimdKind.S64x2, SimdKind.F64x2), (SimdKind.U64x2, SimdKind.F64x2),
        (SimdKind.F64x2, SimdKind.F32x4), (SimdKind.F32x4, SimdKind.F64x2),
    };

    static int ShiftCount(SimdKind k, LispObject o, string fn)
    {
        int max = SimdInfo.Bits(k) - 1;
        o = Runtime.Primary(o);
        if (o is Fixnum f && f.Value >= 0 && f.Value <= max) return (int)f.Value;
        var type = Runtime.List(Startup.Sym("INTEGER"), Fixnum.Make(0), Fixnum.Make(max));
        throw new LispErrorException(new LispTypeError(
            $"{fn}: shift count {Runtime.FormatObject(o, true)} is not of type {Runtime.FormatObject(type, true)}", o, type));
    }

    static void AddOps(Dictionary<string, LispFunction> t, SimdKind k)
    {
        string n = SimdInfo.Name(k);
        bool fl = SimdInfo.IsFloat(k);
        var mk = SimdInfo.MaskKind(k);

        void Bin(string suffix, SimdOp op)
        {
            string f = n + suffix;
            Def(t, f, 2, a => SimdOps.Binary(op, SimdInfo.Cast(k, a[0], f), SimdInfo.Cast(k, a[1], f)));
        }
        void Un(string suffix, SimdOp op)
        {
            string f = n + suffix;
            Def(t, f, 1, a => SimdOps.Unary(op, SimdInfo.Cast(k, a[0], f)));
        }
        void Cmp(string suffix, SimdOp op)
        {
            string f = n + suffix;
            Def(t, f, 2, a => SimdOps.Compare(op, SimdInfo.Cast(k, a[0], f), SimdInfo.Cast(k, a[1], f)));
        }
        void Hor(string suffix, SimdOp op)
        {
            string f = n + suffix;
            Def(t, f, 1, a => SimdOps.Horizontal(op, SimdInfo.Cast(k, a[0], f)));
        }

        Bin("+", SimdOp.Add); Bin("-", SimdOp.Sub); Bin("*", SimdOp.Mul);
        Bin("-MIN", SimdOp.Min); Bin("-MAX", SimdOp.Max);
        Bin("-AND", SimdOp.And); Bin("-OR", SimdOp.Or); Bin("-XOR", SimdOp.Xor); Bin("-ANDC1", SimdOp.AndC1);
        Un("-NOT", SimdOp.Not);
        Cmp("=", SimdOp.Eq); Cmp("/=", SimdOp.Ne); Cmp("<", SimdOp.Lt);
        Cmp("<=", SimdOp.Le); Cmp(">", SimdOp.Gt); Cmp(">=", SimdOp.Ge);
        Hor("-HORIZONTAL+", SimdOp.Add); Hor("-HORIZONTAL-MIN", SimdOp.Min); Hor("-HORIZONTAL-MAX", SimdOp.Max);

        string ifName = n + "-IF";
        Def(t, ifName, 3, a => SimdOps.Select(SimdInfo.Cast(mk, a[0], ifName),
            SimdInfo.Cast(k, a[1], ifName), SimdInfo.Cast(k, a[2], ifName)));

        if (fl)
        {
            Bin("/", SimdOp.Div);
            Un("-SQRT", SimdOp.Sqrt);
            Un("-ROUND", SimdOp.Round); Un("-FLOOR", SimdOp.Floor);
            Un("-CEILING", SimdOp.Ceiling); Un("-TRUNCATE", SimdOp.Truncate);
        }
        else
        {
            string l = n + "-SHIFTL", r = n + "-SHIFTR";
            Def(t, l, 2, a => SimdOps.Shift(true, SimdInfo.Cast(k, a[0], l), ShiftCount(k, a[1], l)));
            Def(t, r, 2, a => SimdOps.Shift(false, SimdInfo.Cast(k, a[0], r), ShiftCount(k, a[1], r)));
        }
    }

    static void AddConversions(Dictionary<string, LispFunction> t)
    {
        foreach (var (to, from) in Conversions)
        {
            string f = SimdInfo.Name(to) + "-FROM-" + SimdInfo.Name(from);
            Def(t, f, 1, a => SimdOps.Convert(to, SimdInfo.Cast(from, a[0], f)));
        }
    }
}
