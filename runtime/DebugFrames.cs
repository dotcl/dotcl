namespace DotCL;

/// <summary>
/// The lexical variables of one running compiled body, as seen by the in-process
/// debugger. Created by <see cref="DebugFrames.Enter"/> from IL the compiler emits
/// at body entry when frame-locals mode is on; each user variable binding then
/// stores itself here with <see cref="Set"/>. Nothing of this exists in the normal
/// (frame-locals off) code path — no frame is opened and no store IL is emitted.
/// </summary>
public sealed class DebugFrame
{
    /// <summary>Lisp name of this body's function, or — for a lambda, which has no
    /// name of its own — of the caller whose call-stack frame it runs under (null
    /// at top level). Used together with <see cref="CallDepth"/> to match a debugger
    /// backtrace frame to its locals.</summary>
    internal readonly string? FunctionName;

    /// <summary>LispFunction call-stack depth at body entry.</summary>
    internal readonly int CallDepth;

    /// <summary>True for a body that runs without a call-stack frame of its own —
    /// a lambda / closure, or a named function reached through a call path that
    /// pushes no frame. Such a frame borrows its caller's depth, so it neither
    /// evicts the caller's frame nor outranks it when the debugger asks for that
    /// backtrace position's locals.</summary>
    internal readonly bool Borrowed;

    /// <summary>Dynamic-binding stack depth when this body started. Everything
    /// pushed at or above it was established by this body or something it called,
    /// which is what lets the debugger say which specials belong to the frame
    /// being inspected rather than to its callers.</summary>
    internal readonly int DynDepth;

    // Insertion-ordered name → holder: either the value itself, or, for a boxed
    // variable, the heap cell holding it (resolved by Read at print time, so
    // later mutations through the box are seen). Rebinding the same name
    // overwrites in place, so a variable appears once and shows its newest value
    // (a shadowing inner LET wins over the outer binding while it is in scope).
    private readonly List<KeyValuePair<string, object>> _vars = new(4);

    internal DebugFrame(string? functionName, int callDepth, bool borrowed, int dynDepth)
    {
        FunctionName = functionName;
        CallDepth = callDepth;
        Borrowed = borrowed;
        DynDepth = dynDepth;
    }

    /// <summary>Record a lexical binding. Called from emitted IL right after the
    /// variable's slot is stored, and again after every assignment to it — the
    /// frame holds the value, so it has to follow.</summary>
    public void Set(string name, LispObject value) => Store(name, value);

    /// <summary>Record a boxed variable (mutated AND captured, so it lives in a
    /// heap cell shared with the closures that see it) by storing the cell itself:
    /// mutations through any of those closures are then visible without a store of
    /// their own. BOX is a LispBox or a LispObject[1], whichever representation the
    /// body was compiled with.</summary>
    public void SetBox(string name, object box) => Store(name, box);

    private void Store(string name, object holder)
    {
        for (int i = 0; i < _vars.Count; i++)
        {
            if (_vars[i].Key == name)
            {
                _vars[i] = new KeyValuePair<string, object>(name, holder);
                return;
            }
        }
        _vars.Add(new KeyValuePair<string, object>(name, holder));
    }

    private static LispObject Read(object holder) => holder switch
    {
        LispBox b => b.Value,
        LispObject[] a => a.Length > 0 ? a[0] : Nil.Instance,
        LispObject v => v,
        _ => Nil.Instance,
    };

    /// <summary>The frame's variables as an alist (("NAME" . value) ...) in
    /// binding order.</summary>
    internal LispObject ToAlist()
    {
        LispObject result = Nil.Instance;
        for (int i = _vars.Count - 1; i >= 0; i--)
            result = new Cons(new Cons(new LispString(_vars[i].Key), Read(_vars[i].Value)), result);
        return result;
    }
}

/// <summary>
/// Per-thread stack of <see cref="DebugFrame"/>s: the in-process view a CL-native
/// debugger (sldb, the :bt/:locals commands) uses to read a running frame's
/// lexical variables by name.
///
/// A frame is opened by IL at body entry, but never explicitly closed: wrapping
/// every compiled body in a try/finally just to pop would fight tail calls and
/// change codegen far beyond the debug annotation. Instead <see cref="Enter"/>
/// drops the frames that provably cannot be live any more — anything deeper than
/// the frame being entered (its callee has returned), and, at the same depth,
/// anything a body with a call-stack frame of its own enters over (that frame was
/// pushed after the ones already sitting at this depth, so they have returned). A
/// body that pushed no call-stack frame and therefore shares its caller's depth —
/// a lambda, or a named function reached through a path that pushes no frame —
/// evicts only earlier such frames, leaving its caller's own live locals alone.
/// </summary>
public static class DebugFrames
{
    [ThreadStatic] private static List<DebugFrame>? s_frames;

    /// <summary>Open a frame for the body now starting on this thread. Called
    /// from emitted IL; the body keeps the returned frame in a local slot, so
    /// its variable stores never have to search for it. OWNNAME is the body's own
    /// function name, or null for a lambda / closure body.
    ///
    /// A named body normally owns the innermost call-stack frame, but not always:
    /// LispFunction.Invoke(params) pushes none, so a function reached that way
    /// (APPLY, and the way the runtime calls *debugger-hook*) runs at its caller's
    /// depth. Comparing OWNNAME with the innermost frame's name tells the two
    /// apart; without that check such a body would inherit its caller's identity —
    /// evicting the caller's locals and answering for its backtrace position,
    /// which is exactly the frame an sldb user is looking at.</summary>
    public static DebugFrame Enter(string? ownName)
    {
        int depth = LispFunction.CallStackDepth;
        string? current = LispFunction.CurrentFrameName;
        bool borrowed = ownName == null || current != ownName;
        var frames = s_frames ??= new List<DebugFrame>();
        for (int i = frames.Count - 1; i >= 0; i--)
        {
            var f = frames[i];
            if (f.CallDepth < depth) break; // shallower ⇒ still live, and so is everything before it
            if (f.CallDepth > depth || !borrowed || f.Borrowed) frames.RemoveAt(i);
        }
        // A lambda has no name to be found under, so it is recorded under the frame
        // it runs in and answers for it when the named body itself recorded nothing
        // (see Locals). A borrowed *named* body keeps its own name instead: it is
        // absent from the backtrace, so nothing should reach its locals by index.
        var frame = new DebugFrame(ownName ?? current, depth, borrowed, DynamicBindings.Depth);
        frames.Add(frame);
        return frame;
    }

    /// <summary>Locals of backtrace frame FRAMEINDEX (0 = innermost, same
    /// numbering as DOTCL:BACKTRACE) as an alist (("NAME" . value) ...), or NIL
    /// when that frame recorded none — it was compiled without frame-locals mode,
    /// is a C#-implemented function, or simply binds nothing.</summary>
    /// <summary>A frame's locals rendered one "NAME = value" line per variable, for
    /// the debugger's :locals and DOTCL:PRINT-FRAME-LOCALS (which share the
    /// formatting). Empty when the frame recorded none. Values print through the
    /// bounded, cycle-safe backtrace printer.</summary>
    internal static string[] FormatLocals(int frameIndex)
    {
        var lines = new List<string>();
        var cur = Locals(frameIndex);
        while (cur is Cons c)
        {
            if (c.Car is Cons entry && entry.Car is LispString name)
                lines.Add($"{name.Value} = {Runtime.BacktraceArgString(entry.Cdr)}");
            cur = c.Cdr;
        }
        return lines.ToArray();
    }

    /// <summary>
    /// The dynamic (special-variable) bindings on this thread's stack, innermost
    /// first, as ((SYMBOL value . own-p) ...). OWN-P is true for a binding this
    /// frame or something it called established — the rest belong to its callers.
    ///
    /// Unlike locals, this does not depend on frame-locals mode: the binding stack
    /// is always there. What FRAMEINDEX buys is only the OWN-P split, so an
    /// unrecorded frame (no DebugFrame) still gets the full list, just with
    /// everything marked as not its own rather than guessing.
    /// </summary>
    public static LispObject Specials(int frameIndex)
    {
        int boundary = FrameDynDepth(frameIndex);
        LispObject result = Nil.Instance;
        var bindings = DynamicBindings.CurrentBindings();   // innermost first
        for (int i = bindings.Count - 1; i >= 0; i--)       // build back to front
        {
            var (sym, val, depth) = bindings[i];
            var ownP = boundary >= 0 && depth >= boundary ? (LispObject)T.Instance : Nil.Instance;
            result = new Cons(new Cons(sym, new Cons(val, ownP)), result);
        }
        return result;
    }

    /// <summary>Binding-stack depth recorded when backtrace frame FRAMEINDEX
    /// started, or -1 when that frame recorded nothing.</summary>
    private static int FrameDynDepth(int frameIndex)
    {
        var callStack = LispFunction.GetCallStack();
        if (frameIndex < 0 || frameIndex >= callStack.Length) return -1;
        int depth = callStack.Length - frameIndex;
        string name = callStack[frameIndex];
        var frames = s_frames;
        if (frames == null) return -1;
        for (int i = frames.Count - 1; i >= 0; i--)
            if (frames[i].CallDepth == depth && frames[i].FunctionName == name)
                return frames[i].DynDepth;
        return -1;
    }

    /// <summary>A frame's specials rendered one line per binding, marking the ones
    /// the frame itself established. Shares the value printer with :locals.</summary>
    internal static string[] FormatSpecials(int frameIndex)
    {
        var lines = new List<string>();
        var cur = Specials(frameIndex);
        while (cur is Cons c)
        {
            if (c.Car is Cons entry && entry.Car is Symbol sym && entry.Cdr is Cons vc)
            {
                var mark = vc.Cdr is Nil ? "  " : "* ";
                lines.Add($"{mark}{sym.Name} = {Runtime.BacktraceArgString(vc.Car)}");
            }
            cur = c.Cdr;
        }
        return lines.ToArray();
    }

    public static LispObject Locals(int frameIndex)
    {
        var callStack = LispFunction.GetCallStack();
        if (frameIndex < 0 || frameIndex >= callStack.Length) return Nil.Instance;
        int depth = callStack.Length - frameIndex;
        string name = callStack[frameIndex];
        var frames = s_frames;
        if (frames == null) return Nil.Instance;
        // Newest match wins: the current invocation pushed last. The frame the
        // backtrace names is the one that owns it; a lambda running inside it has
        // its own frame at the same depth, but no backtrace position of its own, so
        // it only answers when the named body recorded nothing (e.g. a lambda
        // invoked from a C# builtin).
        for (int i = frames.Count - 1; i >= 0; i--)
            if (frames[i].CallDepth == depth && frames[i].FunctionName == name
                && !frames[i].Borrowed)
                return frames[i].ToAlist();
        for (int i = frames.Count - 1; i >= 0; i--)
            if (frames[i].CallDepth == depth && frames[i].FunctionName == name)
                return frames[i].ToAlist();
        return Nil.Instance;
    }
}
