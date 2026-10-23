using System.Runtime.CompilerServices;

#pragma warning disable CS8500

namespace DotCL;

// A DEFUN body links its own debugger frame (a local of the body method) and
// unlinks it before every return and tail call, with no try/finally. An
// exception leaving the body leaves CallTls.Top pointing into the unwound native
// frame: a compiled handler puts the list back (Resume / ResumeBelow), and
// InvokeN, for callers written in C#, restores the list it was entered with in a
// finally. Not in a filter: the finally of a framed call further in runs after
// the filters and would link a dying frame back in.
public partial class LispFunction
{
    internal nint _entry;
    internal int _entryKey;   // arity + 1 while _entry is set
    internal bool _selfFramed;   // the array entry too
    internal int _sfMask;        // bit N: _funcN links its own frame
    internal bool _closureEntry; // _entry is a closure body's: no frame, no _funcN

    public void SetSelfFramedEntry(System.IntPtr entry, int arity)
    {
        _selfFramed = true;
        SetArityEntry(entry, arity);
    }

    /// <summary>_funcN of an &amp;optional / &amp;key function links its own frame. The
    /// first arity given an address is the one CallN enters by calli.</summary>
    public void SetArityEntry(System.IntPtr entry, int arity)
    {
        _sfMask |= 1 << arity;
        if (entry != System.IntPtr.Zero && _entryKey == 0)
        {
            _entry = entry;
            _entryKey = arity + 1;
        }
    }

    /// <summary>A closure body that records no frame, entered through
    /// _entry by CallN. ENTRY takes (LispFunction self, a0..aN-1).</summary>
    public static LispObject WithClosureEntry(LispObject fn, System.IntPtr entry, int arity)
    {
        var f = (LispFunction)fn;
        if (f._entryKey == 0 && f._frameName == null)
        {
            f._entry = entry;
            f._entryKey = arity + 1;
            f._closureEntry = true;
        }
        return fn;
    }

    /// <summary>The innermost frame, for C# that calls Lisp through CallN in a
    /// loop and puts the frame list back once in its own finally, as InvokeN
    /// does on every call.</summary>
    internal static unsafe nint CurrentTop => (nint)s_top;
    internal static unsafe void RestoreTop(nint top) => s_top = (Frame*)top;

    /// <summary>The check InvokeN makes before a closure body, made by the body
    /// itself when it has a closure entry.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void ClosureEntryCheck()
    {
        if (++CallTls.StackCheck % 256 == 0) ClosureEntryCheckSlow();
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static void ClosureEntryCheckSlow()
    {
        int mode = MultipleValues.TakeMode();
        try
        {
            if (!Compat.TryEnsureSufficientExecutionStackWithMargin())
                throw new LispErrorException(new LispStorageCondition(
                    "Stack overflow in function anonymous"));
            ConditionSystem.CheckInterrupt();
        }
        finally { if (mode != 0) MultipleValues.SetMode(mode); }
    }

    /// <summary>For tests: "entry", "closure-entry", "self-framed" (no entry) or "none".</summary>
    public static string EntryState(LispObject fn) =>
        fn is LispFunction f ? (f._entryKey != 0 ? (f._closureEntry ? "closure-entry" : "entry") : f._sfMask != 0 ? "self-framed" : "none") : "none";

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe LispObject Call0(LispFunction fn)
        => fn._entryKey == 1 ? ((delegate*<LispFunction, LispObject>)fn._entry)(fn) : fn.InvokeFast0();
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe LispObject Call1(LispFunction fn, LispObject a)
        => fn._entryKey == 2 ? ((delegate*<LispFunction, LispObject, LispObject>)fn._entry)(fn, a) : fn.InvokeFast1(a);
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe LispObject Call2(LispFunction fn, LispObject a, LispObject b)
        => fn._entryKey == 3 ? ((delegate*<LispFunction, LispObject, LispObject, LispObject>)fn._entry)(fn, a, b) : fn.InvokeFast2(a, b);
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe LispObject Call3(LispFunction fn, LispObject a, LispObject b, LispObject c)
        => fn._entryKey == 4 ? ((delegate*<LispFunction, LispObject, LispObject, LispObject, LispObject>)fn._entry)(fn, a, b, c) : fn.InvokeFast3(a, b, c);
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe LispObject Call4(LispFunction fn, LispObject a, LispObject b, LispObject c, LispObject d)
        => fn._entryKey == 5 ? ((delegate*<LispFunction, LispObject, LispObject, LispObject, LispObject, LispObject>)fn._entry)(fn, a, b, c, d) : fn.InvokeFast4(a, b, c, d);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe LispObject Call0M(LispFunction fn, int mode)
    {
        if (fn._entryKey != 1) return fn.InvokeFast0M(mode);
        if ((fn._mvModeMask & 1) != 0) MultipleValues.SetMode(mode);
        return ((delegate*<LispFunction, LispObject>)fn._entry)(fn);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe LispObject Call1M(LispFunction fn, LispObject a, int mode)
    {
        if (fn._entryKey != 2) return fn.InvokeFast1M(a, mode);
        if ((fn._mvModeMask & 2) != 0) MultipleValues.SetMode(mode);
        return ((delegate*<LispFunction, LispObject, LispObject>)fn._entry)(fn, a);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe LispObject Call2M(LispFunction fn, LispObject a, LispObject b, int mode)
    {
        if (fn._entryKey != 3) return fn.InvokeFast2M(a, b, mode);
        if ((fn._mvModeMask & 4) != 0) MultipleValues.SetMode(mode);
        return ((delegate*<LispFunction, LispObject, LispObject, LispObject>)fn._entry)(fn, a, b);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe LispObject Call3M(LispFunction fn, LispObject a, LispObject b, LispObject c, int mode)
    {
        if (fn._entryKey != 4) return fn.InvokeFast3M(a, b, c, mode);
        if ((fn._mvModeMask & 8) != 0) MultipleValues.SetMode(mode);
        return ((delegate*<LispFunction, LispObject, LispObject, LispObject, LispObject>)fn._entry)(fn, a, b, c);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe LispObject Call4M(LispFunction fn, LispObject a, LispObject b, LispObject c, LispObject d, int mode)
    {
        if (fn._entryKey != 5) return fn.InvokeFast4M(a, b, c, d, mode);
        if ((fn._mvModeMask & 16) != 0) MultipleValues.SetMode(mode);
        return ((delegate*<LispFunction, LispObject, LispObject, LispObject, LispObject, LispObject>)fn._entry)(fn, a, b, c, d);
    }

    /// <summary>Tail call from a self-framed body to a callee that links its own
    /// frame, through TailDirectN: false when it has no entry of this arity. A
    /// borrowed frame is handed on.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe bool SelfTailEntry(ref Frame fr, LispFunction callee, int arity, int mode)
    {
        if (callee._entryKey != arity + 1 || callee._closureEntry) return false;
        ConditionSystem.PollInterrupt();
        if (mode >= 0 && (callee._mvModeMask & (1 << arity)) != 0) MultipleValues.SetMode(mode);
        if (fr.Depth == 0) CallTls.Borrow = 1;
        else if (fr.Depth > 0) CallTls.Top = fr.Prev;
        return true;
    }

    /// <summary>Tail call from a self-framed body straight to a body of the same file.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe void SameFileTail(ref Frame fr)
    {
        ConditionSystem.PollInterrupt();
        if (fr.Depth == 0) CallTls.Borrow = 1;
        else if (fr.Depth > 0) CallTls.Top = fr.Prev;
    }

    /// <summary>The same from a body without a frame of its own, SELF as for TailPrepN.</summary>
    public static unsafe void SameFileTailFrom(string? self)
    {
        ConditionSystem.PollInterrupt();
        var top = s_top;
        if (self != null && top != null
            && (self.Length == 0 ? top->Lent : string.Equals(top->Name, self)))
            CallTls.Borrow = 1;
    }

    public static readonly string TailLendMarker = new string('\0', 1);

    /// <summary>Tail call from a self-framed body to a callee with no entry.
    /// TailLendMarker: the callee records no frame, so the body lends it its own
    /// and calls it without a tail call (the frame would go with the native
    /// one). Otherwise the frame name to pass to TailPrepN.</summary>
    public static unsafe string? SelfTailPrepare(ref Frame fr, LispFunction callee, string name)
    {
        if (fr.Depth == 0) return name;
        if (fr.Depth < 0) return null;
        if (callee._frameName == null) { fr._argc |= Frame.LentBit; return TailLendMarker; }
        CallTls.Top = fr.Prev;
        return null;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Enter0(ref Frame fr, string name)
    {
        fr.Name = name;
        LinkOwn(ref fr);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Enter1(ref Frame fr, string name, LispObject a0)
    {
        fr.Name = name; fr._argc = 1; fr._a0 = a0;
        LinkOwn(ref fr);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Enter2(ref Frame fr, string name, LispObject a0, LispObject a1)
    {
        fr.Name = name; fr._argc = 2; fr._a0 = a0; fr._a1 = a1;
        LinkOwn(ref fr);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Enter3(ref Frame fr, string name, LispObject a0, LispObject a1, LispObject a2)
    {
        fr.Name = name; fr._argc = 3; fr._a0 = a0; fr._a1 = a1; fr._a2 = a2;
        LinkOwn(ref fr);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Enter4(ref Frame fr, string name, LispObject a0, LispObject a1, LispObject a2, LispObject a3)
    {
        fr.Name = name; fr._argc = 4; fr._a0 = a0; fr._a1 = a1; fr._a2 = a2; fr._a3 = a3;
        LinkOwn(ref fr);
    }

    internal string? FrameNameField => _frameName;

    // The body, when calling it needs nothing but a frame already on the thread
    // (for RetainUnit) and the caller's stack check.
    internal Func<LispObject, LispObject>? PlainFunc1 => _frameName == null && (_sfMask & 2) == 0 ? _func1 : null;
    internal Func<LispObject, LispObject, LispObject>? PlainFunc2 => _frameName == null && (_sfMask & 4) == 0 ? _func2 : null;

    // A self-framed callee that records no frame: the caller's frame is not handed on.
    internal static void DropBorrow()
    {
        if (CallTls.Borrow != 0) CallTls.Borrow = 0;
    }

    internal static void EnterArgs(ref Frame fr, string name, LispObject[] args)
    {
        fr.Name = name; fr._argc = args.Length; fr._rest = args;
        DropBorrow();
        LinkOwn(ref fr);
    }

    // After a tail call from the owner of the innermost frame (CallTls.Borrow)
    // the body runs on that frame instead: FR.Depth 0, FR.Prev the frame.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static unsafe void LinkOwn(ref Frame fr)
    {
        if (CallTls.Borrow != 0) { EnterBorrowed(ref fr); return; }
        LinkTop(ref fr);
        if ((fr.Depth & 255) == 0 || ConditionSystem.PollGate != 0) EnterSlow(ref fr);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static unsafe void LinkTop(ref Frame fr)
    {
        var slot = (Frame**)Unsafe.AsPointer(ref CallTls.TopWord);
        fr._slot = slot;
        var p = *slot;
        fr.Prev = p;
        fr.Depth = p == null ? 1 : p->Depth + 1;
        *slot = (Frame*)Unsafe.AsPointer(ref fr);
    }

    // EnterN followed by EnterUnit, for a body whose function may hold a RetainUnit.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void LinkOwnUnit(ref Frame fr, LispFunction self)
    {
        fr._unit = self.RetainUnit;
        if (CallTls.Borrow != 0) { EnterBorrowedUnit(ref fr); return; }
        LinkTop(ref fr);
        if ((fr.Depth & 255) == 0 || ConditionSystem.PollGate != 0) EnterSlow(ref fr);
    }

    // A borrowed frame keeps the unit in the frame it borrows.
    [MethodImpl(MethodImplOptions.NoInlining)]
    private static unsafe void EnterBorrowedUnit(ref Frame fr)
    {
        EnterBorrowed(ref fr);
        if (fr.Depth == 0 && fr._unit is { } u) fr.Prev->KeepUnit(u);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Enter0U(ref Frame fr, string name, LispFunction self)
    {
        fr.Name = name;
        LinkOwnUnit(ref fr, self);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Enter1U(ref Frame fr, string name, LispFunction self, LispObject a0)
    {
        fr.Name = name; fr._argc = 1; fr._a0 = a0;
        LinkOwnUnit(ref fr, self);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Enter2U(ref Frame fr, string name, LispFunction self, LispObject a0, LispObject a1)
    {
        fr.Name = name; fr._argc = 2; fr._a0 = a0; fr._a1 = a1;
        LinkOwnUnit(ref fr, self);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Enter3U(ref Frame fr, string name, LispFunction self, LispObject a0, LispObject a1, LispObject a2)
    {
        fr.Name = name; fr._argc = 3; fr._a0 = a0; fr._a1 = a1; fr._a2 = a2;
        LinkOwnUnit(ref fr, self);
    }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Enter4U(ref Frame fr, string name, LispFunction self, LispObject a0, LispObject a1, LispObject a2, LispObject a3)
    {
        fr.Name = name; fr._argc = 4; fr._a0 = a0; fr._a1 = a1; fr._a2 = a2; fr._a3 = a3;
        LinkOwnUnit(ref fr, self);
    }

    // The value mode the caller handed this body is kept across the poll, and
    // dropped when the poll or the stack check throws: the body does not run.
    [MethodImpl(MethodImplOptions.NoInlining)]
    private static void EnterSlow(ref Frame fr)
    {
        int mode = MultipleValues.TakeMode();
        if ((fr.Depth & 255) == 0 && !Compat.TryEnsureSufficientExecutionStackWithMargin())
            throw new LispErrorException(new LispStorageCondition(
                $"Stack overflow in function {fr.Name}"));
        if (ConditionSystem.PollGate != 0) ConditionSystem.PollCallSlow();
        if (mode != 0) MultipleValues.SetMode(mode);
    }

    // Borrow 2 (InvokeNoFrame): run on no frame at all (Depth -1).
    [MethodImpl(MethodImplOptions.NoInlining)]
    private static unsafe void EnterBorrowed(ref Frame fr)
    {
        var how = CallTls.Borrow;
        CallTls.Borrow = 0;
        fixed (Frame** pt = &CallTls.Top) fr._slot = pt;
        var top = CallTls.Top;
        fr.Prev = top;
        if (how == 1 && top != null)
        {
            top->Retarget(fr.Name, fr._argc, fr._a0, fr._a1, fr._a2, fr._a3, null, null, null, null);
            fr.Depth = 0;
        }
        else fr.Depth = -1;
        if (ConditionSystem.PollGate != 0) EnterSlow(ref fr);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe void EnterUnit(ref Frame fr, LispFunction self)
    {
        var u = self.RetainUnit;
        if (u == null) return;
        if (fr.Depth > 0) fr._unit = u;
        else if (fr.Depth == 0) fr.Prev->KeepUnit(u);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe void Leave(ref Frame fr)
    {
#if NET5_0_OR_GREATER
        *fr._slot = fr.Prev;
#else
        CallTls.Top = fr.Prev;
#endif
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe void Resume(ref Frame fr) =>
        CallTls.Top = fr.Depth > 0 ? (Frame*)Unsafe.AsPointer(ref fr) : fr.Prev;

    /// <summary>After ResumeBelow: FR, a frame local of the handler's method that
    /// may lie above its marker, is unlinked when it is still the innermost.</summary>
    public static unsafe void UnlinkIfTop(ref Frame fr)
    {
        if (s_top == (Frame*)Unsafe.AsPointer(ref fr)) s_top = fr.Prev;
    }

    /// <summary>MARKER is a local of the handler's method.</summary>
    // A frame below MARKER (the stack grows down) belongs to a method deeper than
    // the handler's.
    public static unsafe void ResumeBelow(ref byte marker)
    {
        var live = Unsafe.AsPointer(ref marker);
        var p = s_top;
        while (p != null && (void*)p < live) p = p->Prev;
        s_top = p;
    }
}
