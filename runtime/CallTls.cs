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
    // A LispFunction.Frame*, typed nint so that its address can be taken without
    // a pin (a struct-typed thread static would be boxed on the GC heap).
    [ThreadStatic] internal static nint TopWord;
    internal static ref LispFunction.Frame* Top
    {
        [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
        get => ref *(LispFunction.Frame**)System.Runtime.CompilerServices.Unsafe.AsPointer(ref TopWord);
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
        TopWord = 0;
        Borrow = 0;
        Mode = 0;
        BindCount = 0;
        Snap = 0;
    }
}
