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
    [ThreadStatic] internal static LispFunction.Frame* Top;
    // See LispFunction.LinkOwn.
    [ThreadStatic] internal static int Borrow;
    // See MultipleValues.TakeMode.
    [ThreadStatic] internal static int Mode;

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    internal static void Claim()
    {
        MvCount = 0;
        StackCheck = 0;
        Top = null;
        Borrow = 0;
        Mode = 0;
    }
}
