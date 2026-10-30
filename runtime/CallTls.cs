#pragma warning disable CS8500
using System;

namespace DotCL;

/// <summary>
/// The thread-static state touched on every call: the multiple-values count and mode
/// (MultipleValues), the innermost debugger frame and the periodic stack-check
/// counter (LispFunction).
///
/// The runtime keeps a small area (48 bytes in .NET 10) of thread-static data
/// directly in each thread's TLS block, given first come first served to
/// non-GC thread statics of classes that have no static constructor (or have
/// already run it). A field there is one TLS lookup and one store; a field
/// anywhere else goes through two more dependent loads (the thread's table of
/// thread-static blocks, then the class's block). These three were in classes
/// with static constructors, so they always took the long way; here they sit
/// in a class without one, claimed first thing in Startup.Initialize before
/// other classes take the space.
/// </summary>
internal static unsafe class CallTls
{
    [ThreadStatic] internal static int MvCount;
    [ThreadStatic] internal static int StackCheck;
    // Address of this thread's innermost-frame word (a LispFunction.Frame*).
    // Frames keep it (Frame._slot), so the word must not move; a thread static
    // itself moves with the GC once the TLS area above is used up by the host.
    [ThreadStatic] internal static nint TopCell;
#if NET5_0_OR_GREATER
    [ThreadStatic] private static nint[]? s_topCellArray;
#else
    [ThreadStatic] private static TopCellHolder? s_topCellHolder;
    private sealed class TopCellHolder
    {
        internal readonly nint Cell = System.Runtime.InteropServices.Marshal.AllocHGlobal(IntPtr.Size);
        ~TopCellHolder() { System.Runtime.InteropServices.Marshal.FreeHGlobal(Cell); }
    }
#endif

    internal static LispFunction.Frame** TopSlot
    {
        [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
        get
        {
            var c = TopCell;
            return c != 0 ? (LispFunction.Frame**)c : NewTopCell();
        }
    }

    internal static ref LispFunction.Frame* Top
    {
        [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
        get => ref *TopSlot;
    }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    private static LispFunction.Frame** NewTopCell()
    {
#if NET5_0_OR_GREATER
        var a = GC.AllocateArray<nint>(1, pinned: true);
        s_topCellArray = a;
        var c = (nint)System.Runtime.CompilerServices.Unsafe.AsPointer(
            ref System.Runtime.InteropServices.MemoryMarshal.GetArrayDataReference(a));
#else
        var h = new TopCellHolder();
        s_topCellHolder = h;
        var c = h.Cell;
        *(nint*)c = 0;
#endif
        TopCell = c;
        return (LispFunction.Frame**)c;
    }
    // See LispFunction.LinkOwn.
    [ThreadStatic] internal static int Borrow;
    // See MultipleValues.TakeMode.
    [ThreadStatic] internal static int Mode;
    // See MultipleValues.CaptureForBind.
    [ThreadStatic] internal static int BindCount;
    // Address of element 0 of MultipleValues' bind snapshot (pinned), 0 before the first.
    [ThreadStatic] internal static nint Snap;

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    internal static void Claim()
    {
        MvCount = 0;
        StackCheck = 0;
        TopCell = 0;
        NewTopCell();
        Borrow = 0;
        Mode = 0;
        BindCount = 0;
        Snap = 0;
    }
}
