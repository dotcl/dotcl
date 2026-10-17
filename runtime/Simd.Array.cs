namespace DotCL;

public static partial class SimdTable
{
    [ThreadStatic] static byte[]? _buf;

    static Type? StorageType(SimdKind k) => k switch
    {
        SimdKind.F64x2 => typeof(double[]),
        SimdKind.F32x4 => typeof(float[]),
        SimdKind.S64x2 => typeof(long[]),
        SimdKind.U64x2 => typeof(ulong[]),
        SimdKind.S32x4 => typeof(int[]),
        SimdKind.U16x8 => typeof(ushort[]),
        SimdKind.U8x16 => typeof(byte[]),
        _ => null,
    };

    static LispVector ArrayArg(SimdKind k, LispObject o, string fn)
    {
        o = Runtime.Primary(o);
        if (o is LispVector v && v.ElementTypeName == SimdInfo.ElementType(k)) return v;
        var type = Runtime.List(Startup.Sym("ARRAY"), SimdInfo.ElementTypeSpec(k));
        throw new LispErrorException(new LispTypeError(
            $"{fn}: {Runtime.FormatObject(o, true)} is not of type {Runtime.FormatObject(type, true)}", o, type));
    }

    static LispErrorException IndexError(string fn, LispObject index, long limit)
    {
        var type = Runtime.List(Startup.Sym("INTEGER"), Fixnum.Make(0), Fixnum.Make(Math.Max(limit - 1, 0)));
        return new LispErrorException(new LispTypeError(
            $"{fn}: index {Runtime.FormatObject(index, true)} is out of bounds for {limit} lanes", index, type));
    }

    static long TotalSize(LispVector v)
    {
        long n = 1;
        foreach (var d in v.Dimensions) n *= d;
        return n;
    }

    /// <summary>Row-major index of the first lane, checked so that all lanes are inside the array.</summary>
    static int FirstLane(SimdKind k, string fn, LispVector v, LispObject[] args, int first, int count)
    {
        var dims = v.Dimensions;
        if (count != dims.Length)
            throw new LispErrorException(new LispProgramError(
                $"{fn}: array has rank {dims.Length} but {count} subscripts were given"));
        long idx = 0;
        for (int i = 0; i < count; i++)
        {
            var s = Runtime.Primary(args[first + i]);
            if (s is not Fixnum f || f.Value < 0 || f.Value >= dims[i])
                throw IndexError(fn, s, dims[i]);
            idx = idx * dims[i] + f.Value;
        }
        long total = TotalSize(v);
        if (idx + SimdInfo.Lanes(k) > total)
            throw IndexError(fn, args[first + count - 1], total - SimdInfo.Lanes(k) + 1);
        return (int)idx;
    }

    static bool Direct(SimdKind k, LispVector v) =>
        BitConverter.IsLittleEndian && v._displacedTo == null && v._numData != null
        && v._numData.GetType() == StorageType(k);

    public static LispSimdPack LoadLanes(SimdKind k, LispVector v, int idx, string fn)
    {
        if (Direct(k, v))
        {
            int lane = SimdInfo.Bits(k) / 8;
            var buf = _buf ??= new byte[16];
            Buffer.BlockCopy(v._numData!, idx * lane, buf, 0, 16);
            return new LispSimdPack(k, BitConverter.ToUInt64(buf, 0), BitConverter.ToUInt64(buf, 8));
        }
        var lanes = new ulong[SimdInfo.Lanes(k)];
        for (int i = 0; i < lanes.Length; i++) lanes[i] = SimdInfo.LaneFromLisp(k, v.GetElement(idx + i), fn);
        return SimdInfo.FromLanes(k, lanes);
    }

    public static void StoreLanes(SimdKind k, LispVector v, int idx, LispSimdPack p)
    {
        if (Direct(k, v))
        {
            int lane = SimdInfo.Bits(k) / 8;
            var buf = _buf ??= new byte[16];
            BitConverter.GetBytes(p.Lo).CopyTo(buf, 0);
            BitConverter.GetBytes(p.Hi).CopyTo(buf, 8);
            Buffer.BlockCopy(buf, 0, v._numData!, idx * lane, 16);
            return;
        }
        for (int i = 0; i < SimdInfo.Lanes(k); i++)
            v.SetElement(idx + i, SimdInfo.LaneToLisp(k, SimdInfo.Lane(p, i)));
    }

    static void AddArrayOps(Dictionary<string, LispFunction> t, SimdKind k)
    {
        string n = SimdInfo.Name(k);
        string aref = n + "-AREF", rm = n + "-ROW-MAJOR-AREF";

        Def(t, aref, -1, a =>
        {
            if (a.Length < 2) throw ArgCount(aref, a.Length, "at least 2");
            var v = ArrayArg(k, a[0], aref);
            return LoadLanes(k, v, FirstLane(k, aref, v, a, 1, a.Length - 1), aref);
        });
        Def(t, rm, 2, a =>
        {
            var v = ArrayArg(k, a[0], rm);
            long total = TotalSize(v);
            var s = Runtime.Primary(a[1]);
            if (s is not Fixnum f || f.Value < 0 || f.Value + SimdInfo.Lanes(k) > total)
                throw IndexError(rm, s, total - SimdInfo.Lanes(k) + 1);
            return LoadLanes(k, v, (int)f.Value, rm);
        });

        string setAref = "(SETF " + aref + ")", setRm = "(SETF " + rm + ")";
        Def(t, setAref, -1, a =>
        {
            if (a.Length < 3) throw ArgCount(setAref, a.Length, "at least 3");
            var p = SimdInfo.Cast(k, a[0], setAref);
            var v = ArrayArg(k, a[1], setAref);
            StoreLanes(k, v, FirstLane(k, setAref, v, a, 2, a.Length - 2), p);
            return p;
        });
        Def(t, setRm, 3, a =>
        {
            var p = SimdInfo.Cast(k, a[0], setRm);
            var v = ArrayArg(k, a[1], setRm);
            long total = TotalSize(v);
            var s = Runtime.Primary(a[2]);
            if (s is not Fixnum f || f.Value < 0 || f.Value + SimdInfo.Lanes(k) > total)
                throw IndexError(setRm, s, total - SimdInfo.Lanes(k) + 1);
            StoreLanes(k, v, (int)f.Value, p);
            return p;
        });
    }
}
