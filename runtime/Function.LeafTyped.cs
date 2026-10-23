using System.Runtime.CompilerServices;

#pragma warning disable CS8500

namespace DotCL;

// The typed entry of a leaf (CilAssembler.IsTypedLeaf) links no frame. These
// helpers are its only way to signal: they get the frame it would have linked
// and link it around the slow path of the original helper.
public partial class LispFunction
{
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe long LeafAddFixnumChecked(long a, long b, string name, int code, int argc, long r0, long r1, long r2, long r3)
    {
        long r = unchecked(a + b);
        if (((a ^ r) & (b ^ r)) < 0)
            return InLeafFrame(&Runtime.AddFixnumChecked, a, b, name, code, argc, r0, r1, r2, r3);
        return r;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe long LeafSubtractFixnumChecked(long a, long b, string name, int code, int argc, long r0, long r1, long r2, long r3)
    {
        long r = unchecked(a - b);
        if (((a ^ b) & (a ^ r)) < 0)
            return InLeafFrame(&Runtime.SubtractFixnumChecked, a, b, name, code, argc, r0, r1, r2, r3);
        return r;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe long LeafMultiplyFixnumChecked(long a, long b, string name, int code, int argc, long r0, long r1, long r2, long r3)
    {
        long hi = Compat.BigMul(a, b, out long r);
        if (hi != (r >> 63))
            return InLeafFrame(&Runtime.MultiplyFixnumChecked, a, b, name, code, argc, r0, r1, r2, r3);
        return r;
    }

    // Division: only a zero divisor signals, and -1 is left to the helper as well.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe long LeafFloorFixnumL(long a, long b, string name, int code, int argc, long r0, long r1, long r2, long r3)
        => (ulong)(b + 1) <= 1
            ? InLeafFrame(&Runtime.FloorFixnumL, a, b, name, code, argc, r0, r1, r2, r3)
            : Runtime.FloorFixnumL(a, b);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe long LeafTruncateFixnumL(long a, long b, string name, int code, int argc, long r0, long r1, long r2, long r3)
        => (ulong)(b + 1) <= 1
            ? InLeafFrame(&Runtime.TruncateFixnumL, a, b, name, code, argc, r0, r1, r2, r3)
            : Runtime.TruncateFixnumL(a, b);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe long LeafModFixnumL(long a, long b, string name, int code, int argc, long r0, long r1, long r2, long r3)
        => b == 0
            ? InLeafFrame(&Runtime.ModFixnumL, a, b, name, code, argc, r0, r1, r2, r3)
            : Runtime.ModFixnumL(a, b);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe long LeafRemFixnumL(long a, long b, string name, int code, int argc, long r0, long r1, long r2, long r3)
        => b == 0
            ? InLeafFrame(&Runtime.RemFixnumL, a, b, name, code, argc, r0, r1, r2, r3)
            : Runtime.RemFixnumL(a, b);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long LeafCheckLongRange(long v, long lo, long hi, string name, int code, int argc, long r0, long r1, long r2, long r3)
        => v >= lo && v <= hi ? v : InLeafFrameRange(v, lo, hi, name, code, argc, r0, r1, r2, r3);

    [MethodImpl(MethodImplOptions.NoInlining)]
    static long InLeafFrameRange(long v, long lo, long hi,
        string name, int code, int argc, long r0, long r1, long r2, long r3)
    {
        Frame fr = default;
        TypedFrameArgs ta = default;
        ta.R0 = r0; ta.R1 = r1; ta.R2 = r2; ta.R3 = r3;
        EnterTypedCore(ref fr, ref ta, name, argc, code);
        long r = Runtime.CheckLongRange(v, lo, hi);
        Leave(ref fr);
        return r;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static unsafe long InLeafFrame(delegate*<long, long, long> op, long a, long b,
        string name, int code, int argc, long r0, long r1, long r2, long r3)
    {
        Frame fr = default;
        TypedFrameArgs ta = default;
        ta.R0 = r0; ta.R1 = r1; ta.R2 = r2; ta.R3 = r3;
        EnterTypedCore(ref fr, ref ta, name, argc, code);
        long r = op(a, b);
        Leave(ref fr);
        return r;
    }
}
