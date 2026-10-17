using System.Runtime.CompilerServices;

// Frame (a struct holding managed references) is addressed through pointers to
// locals of the Invoke methods; see the comment on Frame.
#pragma warning disable CS8500

namespace DotCL;

/// <summary>The lambda list of an anonymous function compiled into a FASL, on
/// the method its delegate calls. A FASL function made by DEFUN records its
/// lambda list when it is defined; a LAMBDA can be made any number of times (a
/// closure once per evaluation), so storing the list on every function object
/// would cost each creation something that only a development tool reads.
/// Metadata costs nothing until FUNCTION-LAMBDA-LIST asks. PACKAGE is the
/// package current when the FASL was written, where the variable names (printed
/// uninterned) are looked up again.</summary>
[System.AttributeUsage(System.AttributeTargets.Method, Inherited = false)]
public sealed class LispLambdaListAttribute : System.Attribute
{
    public string Text { get; }
    public string? Package { get; }
    public LispLambdaListAttribute(string text, string? package)
    {
        Text = text;
        Package = package;
    }
}

public partial class LispFunction : LispObject
{
    // Null only for a direct-params closure, whose body delegate is called
    // straight from _directDel + Environment (see MakeDirectClosure): building
    // an args-array wrapper for it meant a closure object and a delegate on
    // every closure creation, for a path most closures never take.
    private readonly Func<LispObject[], LispObject>? _func;

    // Direct-params closure body: Func<object[] env, LispObject a0..aN-1, LispObject>.
    internal Delegate? _directDel;
    private string? _name;
    public string? Name
    {
        get => _name;
        // Settable so the tree-walk evaluator can name the closure it builds for
        // an interpreted DEFUN. Compiled code names its functions at emit time;
        // the interpreter has no emit step, so it names them right after
        // construction through %SET-FUNCTION-NAME. Nothing else mutates this.
        internal set { _name = value; _frameName = FrameNameOf(value); }
    }

    // Name to record on the debugger call stack, or null when this function must
    // not appear in a backtrace. It differs from Name only for the machinery the
    // tree-walk evaluator runs a form through, which is ordinary named Lisp and
    // would otherwise show up as if the user had called it:
    //
    //   %MINI-*       the evaluator's own helpers. Every interpreted call pushed a
    //                 dozen of these, burying the user's frames and leaking the
    //                 evaluator's environment through BACKTRACE-WITH-ARGS.
    //   %CALL-WITH-*  the primitives that stand in for a special form the
    //                 evaluator cannot emit code for (%CALL-WITH-HANDLER-CLUSTER
    //                 for HANDLER-BIND). Compiled code emits the equivalent inline
    //                 and shows no frame, so a frame here is a divergence.
    //
    // Derived once at naming time so PushFrame stays a single null test on the
    // call hot path.
    private string? _frameName;

    /// <summary>Stop recording a debugger frame for this function, keeping its
    /// name for everything else (error messages, BACKTRACE of its callers,
    /// DESCRIBE, statistics). What a function compiled under
    /// (optimize (debug 0)) asks for: the frame push is the one thing on the
    /// call path that a caller cannot avoid paying, measured at 18% of richards.
    /// Set after Name, which derives the frame name.</summary>
    public void SuppressDebugFrame() => _frameName = null;

    // Interned, so a tail call can tell whether it owns the innermost frame by
    // comparing the frame's name with a string literal by reference (TailPrepN).
    private static string? FrameNameOf(string? name) =>
        name == null || name.StartsWith("%MINI-", StringComparison.Ordinal)
                     || name.StartsWith("%CALL-WITH-", StringComparison.Ordinal)
            ? null : string.Intern(name);

    // Number of REQUIRED parameters, or -1 when unknown. Settable for the same
    // reason Name is: compiled code fixes it at emit time, but the tree-walk
    // evaluator builds every closure as one variadic (&rest args) lambda, so
    // without a way to record the user's own lambda list every interpreted
    // function claimed zero required parameters. Overload selection for a .NET
    // delegate parameter reads this, and a Lisp lambda passed to Enumerable.Select
    // matched neither overload. Set through %SET-FUNCTION-ARITY right after
    // construction; nothing else mutates it.
    public int Arity { get; internal set; }
    // What the tree-walk evaluator needs to run this function's BODY without
    // calling the function: its lambda list, captured environment, special
    // parameters and body continuation. Only interpreted closures carry it, and
    // only their trampoline reads it: a tail call whose callee has this can be
    // continued in the caller's own loop instead of on a new .NET frame, which is
    // what makes interpreted tail recursion run in constant stack. Null for
    // everything else, so the trampoline falls back to an ordinary call.
    public LispObject? InterpInfo { get; internal set; }

    // A direct-params closure has no stored args-array delegate; hand out one
    // over the spreading entry instead (restart-bind takes this, and a closure
    // handler is rare enough that binding a delegate there costs nothing that
    // matters).
    public Func<LispObject[], LispObject> RawFunction => _func ?? CallDirectWithArray;
    // The delegate over the compiled body of this function: the closure body for
    // a closure, otherwise the args-array entry. FUNCTION-LAMBDA-LIST finds an
    // anonymous function's lambda list through it.
    internal Delegate? BodyDelegate => (Delegate?)_closureFunc ?? _directDel ?? _func;
    public object[]? Environment { get; internal set; }
    // The lambda list the user wrote, when something recorded it. A function that
    // came out of a FASL holds it as the LispString it was written as until
    // something asks, because a FASL has no constant pool to hang a list on and
    // building one per function at load time would charge every start-up for what
    // almost nothing reads. FUNCTION-LAMBDA-LIST does that reading; no other
    // reader of this slot should assume it is a list. Nothing in the
    // call path reads this: it exists so a development tool can answer "what are
    // this function's arguments?" -- SLIME/SLY autodoc, DESCRIBE, completion.
    // Arity is not enough (it counts required parameters only) and InterpInfo
    // exists only for interpreted closures, so a compiled DEFUN had nothing.
    public LispObject? StoredLambdaList { get; internal set; }

    // Debug: SIL body stored when dotcl:*save-sil* is true at defun time
    public LispObject? Sil { get; internal set; }

    // Name this function was registered under, for the InvokeSlow statistics only.
    // C# builtins are constructed as anonymous lambdas and keep Name == null on
    // purpose (a non-null Name makes InvokeSlow push a call-stack frame), so the
    // statistics used to collapse every lambda of one registration method into a
    // single <anon:<RegisterSequenceBuiltins>b___> bucket. RegisterFunction fills
    // this in so the counters name the symbol instead. Read only while
    // CollectInvokeStats is on: no hot-path cost.
    internal string? StatsName;

    // Closure delegate: receives explicit env array
    private readonly Func<object[], LispObject[], LispObject>? _closureFunc;

    // S4: strong reference to this function's compilation-unit closure-DM
    // store (CilAssembler unit holder). Set on functions whose body builds
    // closures (the enclosing defun/lambda) and on the closures themselves, so a
    // unit's closure DynamicMethods stay alive exactly while some function that
    // can still call MakeClosure(unit) is reachable. When the last such function
    // dies, the holder dies and the off-GC-heap JIT code behind those DMs frees.
    // The global CilAssembler unit map only holds the holder weakly.
    internal object? RetainUnit;

    // Direct-param delegates for 0-8 arg fast path (set by assembler for simple functions)
    internal Func<LispObject>? _func0;
    internal Func<LispObject, LispObject>? _func1;
    internal Func<LispObject, LispObject, LispObject>? _func2;
    internal Func<LispObject, LispObject, LispObject, LispObject>? _func3;
    internal Func<LispObject, LispObject, LispObject, LispObject, LispObject>? _func4;
    internal Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>? _func5;
    internal Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>? _func6;
    internal Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>? _func7;
    internal Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>? _func8;

    // Native delegate: (self, long args) -> LispObject return.
    // Avoids boxing of ARGUMENTS (the main allocation bottleneck in fixnum recursion).
    // Return is still LispObject so the body compiles unchanged.
    // The leading LispFunction is the function itself, threaded through so a native
    // self-call can reach the receiver from arg0 instead of re-resolving #'NAME from
    // its symbol on every recursive entry (the old per-call self-fn prelude).
    //
    // A function has one arity, so it has at most one native entry: one field for
    // the delegate and its arity as the tag, instead of one typed field per arity.
    // Every LispFunction carried all four, which is 24 bytes of every closure. The
    // tag is what makes the reinterpretation below safe: _nativeDel is a
    // Func<LispFunction, long x N, LispObject> exactly when _nativeArity is N (set
    // together in SetNativeDelegate, the only writer), and a type test on a generic
    // delegate type costs more than the call it guards.
    private Delegate? _nativeDel;
    private int _nativeArity;

    public LispFunction(Func<LispObject[], LispObject> func, string? name = null, int arity = -1)
    {
        _func = func;
        Name = name;
        Arity = arity;
        DotCL.Diagnostics.AllocCounter.Inc("LispFunction");
    }

    // Direct-params closure: the body delegate and the environment are held as
    // they are; every entry point binds them at call time.
    private LispFunction(Delegate directDel, object[] env, string? fnName, int arity)
    {
        _directDel = directDel;
        Environment = env;
        StatsName = fnName;   // used for the arity-error message and for stats
        Arity = arity;
        DotCL.Diagnostics.AllocCounter.Inc("LispFunction+Closure");
    }

    // Closure constructor: env is stored and passed explicitly on each call
    public LispFunction(Func<object[], LispObject[], LispObject> closureFunc,
                        object[] env, string? name = null, int arity = -1)
    {
        _closureFunc = closureFunc;
        Environment = env;
        _func = args => closureFunc(env, args);
        Name = name;
        Arity = arity;
        DotCL.Diagnostics.AllocCounter.Inc("LispFunction+Closure");
    }

    // Factory for direct-params closures (per-arity body delegates; built by
    // CilAssembler.MakeClosureDirect). The closure body DynamicMethod takes
    // (object[] env, LispObject a0..aN-1) instead of (object[] env,
    // LispObject[] args), so an exactly-N-arg InvokeN call runs the body without
    // the args-array InvokeSlow detour. The args-array _func wrapper keeps
    // apply / spread-arg calls working: it performs the same
    // Runtime.CheckArityExact the compiled args-array body used to perform
    // (identical error type and message), then spreads the array. The direct
    // _funcN path needs no check: the delegate signature structurally
    // guarantees the argc (an InvokeM call with M != N finds _funcM null and
    // falls back to the wrapper). fnName is captured only by the wrapper
    // lambda; Name stays null like every closure, so PushFrame behavior and
    // per-call cost are unchanged.
    public static LispFunction MakeDirectClosure(Delegate del, object[] env, string fnName)
    {
        // The body delegate and the environment are stored as they are; InvokeN
        // binds them at call time and CallDirectWithArray covers apply / spread.
        // Building a per-closure lambda for each of those two paths (plus the
        // display class they shared) was ~170 B on EVERY closure creation --
        // paid whether or not the closure was ever called.
        int arity = del switch
        {
            Func<object[], LispObject> => 0,
            Func<object[], LispObject, LispObject> => 1,
            Func<object[], LispObject, LispObject, LispObject> => 2,
            Func<object[], LispObject, LispObject, LispObject, LispObject> => 3,
            Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject> => 4,
            Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> => 5,
            Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> => 6,
            _ => throw new ArgumentException($"MakeDirectClosure: unsupported delegate type {del.GetType().Name}")
        };
        return new LispFunction(del, env, fnName, arity);
    }

    // Lisp-level call stack for debugger backtrace. Each frame keeps the callee
    // name plus its arguments, up to four inline; 5+ argument calls reference
    // an array.
    //
    // A frame lives in the native stack frame of the Invoke method that pushed
    // it, and frames are linked through a thread-static pointer to the
    // innermost one. Storing into a local costs no GC write barrier, where
    // storing the same references into a heap-allocated stack (the previous
    // Stack<Frame>) paid one per reference plus card marking whenever an
    // argument was younger than the array, on every named call. The pointer is
    // only ever followed while the frame it names is live: each push is paired
    // with a finally that restores the previous head before the Invoke method
    // returns or unwinds. The GC keeps reporting the references because the
    // frame is an ordinary (address-taken) local of that method.
    /// <summary>The units a frame keeps for tail calls once there is more than
    /// one (Frame.KeepUnit). Small: a tail loop runs through the same few units
    /// again and again, and each is kept once.</summary>
    internal sealed class UnitSet
    {
        private object[] _items;
        private int _count;
        private HashSet<object>? _many;
        internal UnitSet(object a, object b) { _items = new object[4]; _items[0] = a; _items[1] = b; _count = 2; }
        internal void Add(object unit)
        {
            if (_many != null) { _many.Add(unit); return; }
            for (int i = 0; i < _count; i++) if (ReferenceEquals(_items[i], unit)) return;
            if (_count == _items.Length)
            {
                if (_count >= 16)
                {
                    _many = new HashSet<object>(_items, ReferenceEqualityComparer.Instance) { unit };
                    _items = Array.Empty<object>(); _count = 0;
                    return;
                }
                Array.Resize(ref _items, _count * 2);
            }
            _items[_count++] = unit;
        }
    }

    public unsafe struct Frame
    {
        internal Frame* Prev;
        internal int Depth;   // 1 for the outermost frame on this thread
        // Not readonly: a tail call reuses the frame of the body making it for
        // the function it calls (Retarget).
        // Argument count, plus LentBit: set while a tail call has handed this
        // frame to a callee that records no frame of its own (see TailPrepCore).
        // Kept in the same word so the frame does not grow for it.
        internal int _argc;
        public int Argc => _argc & ~(LentBit | TypedArgsBit | TailRestBit);
        internal bool Lent => (_argc & LentBit) != 0;
        internal const int LentBit = 1 << 30;
        // Set when the arguments are raw values held by the typed call that
        // pushed the frame (_ext then points at its TypedFrameArgs). Every
        // writer of the argument count clears it.
        internal const int TypedArgsBit = 1 << 29;
        // Set when _rest was allocated by Retarget, so the next Retarget of the
        // same length may overwrite it.
        internal const int TailRestBit = 1 << 28;
        public string Name;
        internal LispObject? _a0, _a1, _a2, _a3;
        internal LispObject[]? _rest; // non-null when args came as an array
        // Arguments 5-8 of a fixed-arity call (Invoke5-8). They live in a
        // separate struct next to the frame on the same native stack, so the
        // 0-4 argument frames stay as small as before and no array is
        // allocated. Only read while the frame is linked (see SnapshotFrames).
        internal FrameExt* _ext;
        // Compilation units kept alive for tail calls made while this frame is
        // linked (see KeepUnit): null, one unit holder, or a UnitSet.
        internal object? _unit;

        /// <summary>Keep UNIT (a RetainUnit holder) reachable until this frame
        /// is unlinked. Only ever adds: the frame may be an outer function's,
        /// still needed by a body further down the stack, and a tail call cannot
        /// tell which units those bodies use.</summary>
        internal void KeepUnit(object unit)
        {
            var cur = _unit;
            if (cur == null) _unit = unit;
            else if (!ReferenceEquals(cur, unit))
            {
                if (cur is UnitSet set) set.Add(unit);
                else _unit = new UnitSet(cur, unit);
            }
        }

        /// <summary>Mark this frame as lent to a callee that records no frame of
        /// its own (see TailPrepCore).</summary>
        internal void Lend() => _argc |= LentBit;

        /// <summary>Make this frame stand for the function a tail call is about
        /// to run in place of the one that pushed it. The frame stays where it
        /// is (the Invoke that pushed it still unlinks it), so only its contents
        /// change. Arguments past the fourth go to the FrameExt of the Invoke5-8
        /// that pushed the frame, or else to an array the frame keeps for the
        /// next Retarget of the same length.
        ///
        /// Only the slots below ARGC are written. Arg never reads past Argc, and
        /// each reference stored through the frame pointer costs a checked write
        /// barrier, which is most of what a tail call pays here; a stale slot
        /// only keeps an earlier argument alive until the frame is unlinked.</summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        internal void Retarget(string name, int argc, LispObject? a0, LispObject? a1,
                               LispObject? a2, LispObject? a3, LispObject? a4, LispObject? a5,
                               LispObject? a6, LispObject? a7)
        {
            Name = name;
            if ((_argc & TypedArgsBit) != 0) _ext = null;
            if (argc > 4 && _ext != null)
            {
                _ext->A4 = a4;
                if (argc > 5) _ext->A5 = a5;
                if (argc > 6) _ext->A6 = a6;
                if (argc > 7) _ext->A7 = a7;
            }
            else if (argc > 4)
            {
                var r = (_argc & TailRestBit) != 0 && _rest!.Length == argc ? _rest : new LispObject[argc];
                r[0] = a0!; r[1] = a1!; r[2] = a2!; r[3] = a3!; r[4] = a4!;
                if (argc > 5) r[5] = a5!;
                if (argc > 6) r[6] = a6!;
                if (argc > 7) r[7] = a7!;
                _rest = r; _argc = argc | TailRestBit;
                return;
            }
            _argc = argc;
            if (_rest != null) _rest = null;
            if (argc > 0) _a0 = a0;
            if (argc > 1) _a1 = a1;
            if (argc > 2) _a2 = a2;
            if (argc > 3) _a3 = a3;
        }

        internal void Retarget(string name, LispObject[] all)
        {
            Name = name; _ext = null; _rest = all; _argc = all.Length | TailRestBit;
        }

        public Frame(string name)
        { Prev = null; Depth = 0; Name = name; _argc = 0; _a0 = _a1 = _a2 = _a3 = null; _rest = null; _ext = null; _unit = null; }
        public Frame(string name, LispObject a0)
        { Prev = null; Depth = 0; Name = name; _argc = 1; _a0 = a0; _a1 = _a2 = _a3 = null; _rest = null; _ext = null; _unit = null; }
        public Frame(string name, LispObject a0, LispObject a1)
        { Prev = null; Depth = 0; Name = name; _argc = 2; _a0 = a0; _a1 = a1; _a2 = _a3 = null; _rest = null; _ext = null; _unit = null; }
        public Frame(string name, LispObject a0, LispObject a1, LispObject a2)
        { Prev = null; Depth = 0; Name = name; _argc = 3; _a0 = a0; _a1 = a1; _a2 = a2; _a3 = null; _rest = null; _ext = null; _unit = null; }
        public Frame(string name, LispObject a0, LispObject a1, LispObject a2, LispObject a3)
        { Prev = null; Depth = 0; Name = name; _argc = 4; _a0 = a0; _a1 = a1; _a2 = a2; _a3 = a3; _rest = null; _ext = null; _unit = null; }
        public Frame(string name, int argc, LispObject a0, LispObject a1, LispObject a2, LispObject a3, FrameExt* ext)
        { Prev = null; Depth = 0; Name = name; _argc = argc; _a0 = a0; _a1 = a1; _a2 = a2; _a3 = a3; _rest = null; _ext = ext; _unit = null; }
        public Frame(string name, LispObject[] args)
        { Prev = null; Depth = 0; Name = name; _argc = args.Length; _a0 = _a1 = _a2 = _a3 = null; _rest = args; _ext = null; _unit = null; }
        public Frame(string name, int argc, TypedFrameArgs* raw)
        { Prev = null; Depth = 0; Name = name; _argc = argc | TypedArgsBit; _a0 = _a1 = _a2 = _a3 = null; _rest = null; _ext = (FrameExt*)raw; _unit = null; }

        public LispObject? Arg(int i)
        {
            if (_rest != null) return (uint)i < (uint)_rest.Length ? _rest[i] : null;
            if ((uint)i >= (uint)Argc) return null;
            if ((_argc & TypedArgsBit) != 0) return ((TypedFrameArgs*)_ext)->Box(i);
            return i switch
            {
                0 => _a0, 1 => _a1, 2 => _a2, 3 => _a3,
                4 => _ext->A4, 5 => _ext->A5, 6 => _ext->A6, 7 => _ext->A7,
                _ => null
            };
        }
    }

    /// <summary>Arguments 5-8 of a frame pushed by Invoke5-8.</summary>
    public struct FrameExt
    {
        internal LispObject? A4, A5, A6, A7;
    }

    // Lives in CallTls (see there): read and written on every framed call.
    private static unsafe ref Frame* s_top => ref CallTls.Top;

    /// <summary>Make F the innermost frame. The caller restores the previous head
    /// (F->Prev) in a finally.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static unsafe void Link(Frame* f)
    {
        var p = s_top;
        f->Prev = p;
        f->Depth = p == null ? 1 : p->Depth + 1;
        s_top = f;
    }

    /// <summary>Number of Lisp frames on this thread's call stack. A body's own
    /// frame is already pushed while it runs, so this is that body's depth;
    /// DebugFrames uses it to tie a frame's locals to its backtrace position.</summary>
    internal static unsafe int CallStackDepth => s_top == null ? 0 : s_top->Depth;

    /// <summary>Name of the innermost Lisp frame, or null when there is none
    /// (anonymous callee, or a body reached without going through Invoke).</summary>
    internal static unsafe string? CurrentFrameName => s_top == null || s_top->Depth == 0 ? null : s_top->Name;

    /// <summary>Copy of the live frames, innermost first. A copy of a frame
    /// pushed by Invoke5-8 still points at that call's FrameExt, so the
    /// snapshot is only valid while those frames are live: every caller
    /// consumes it before returning.</summary>
    private static unsafe Frame[] SnapshotFrames()
    {
        var top = s_top;
        if (top == null) return Array.Empty<Frame>();
        var frames = new Frame[top->Depth];
        int i = 0;
        for (var f = top; f != null && i < frames.Length; f = f->Prev) frames[i++] = *f;
        return frames;
    }

    /// <summary>Backtrace as callee-name strings, innermost first. Used by the
    /// programmatic DOTCL:BACKTRACE.</summary>
    internal static string[] GetCallStack()
    {
        var frames = SnapshotFrames();
        if (frames.Length == 0) return Array.Empty<string>();
        var result = new string[frames.Length];
        for (int i = 0; i < frames.Length; i++) result[i] = frames[i].Name;
        return result;
    }

    /// <summary>Backtrace as printed call forms "(NAME arg1 arg2 ...)", innermost
    /// first. Used by the :bt debugger command and DOTCL:PRINT-BACKTRACE. Argument
    /// rendering happens here (off the call hot path) and is bounded/cycle-safe.</summary>
    internal static string[] GetCallStackForms()
    {
        var frames = SnapshotFrames();
        if (frames.Length == 0) return Array.Empty<string>();
        var result = new string[frames.Length];
        for (int i = 0; i < frames.Length; i++) result[i] = FormatFrame(frames[i]);
        return result;
    }

    /// <summary>Backtrace frames as Lisp lists (NAME arg0 arg1 ...), innermost
    /// first, where the args are the ACTUAL captured LispObjects (not printed
    /// strings). Backs DOTCL:BACKTRACE-WITH-ARGS so callers can inspect frame
    /// arguments programmatically (cf. sb-debug:list-backtrace).</summary>
    internal static LispObject[] GetCallStackWithArgs()
    {
        var frames = SnapshotFrames();
        if (frames.Length == 0) return Array.Empty<LispObject>();
        var result = new LispObject[frames.Length];
        for (int i = 0; i < frames.Length; i++)
        {
            var f = frames[i];
            LispObject args = Nil.Instance;
            for (int j = f.Argc - 1; j >= 0; j--)
                args = new Cons(f.Arg(j) ?? Nil.Instance, args);
            result[i] = new Cons(new LispString(f.Name), args);
        }
        return result;
    }

    private static string FormatFrame(Frame f)
    {
        if (f.Argc == 0) return "(" + f.Name + ")";
        var sb = new System.Text.StringBuilder(32);
        sb.Append('(').Append(f.Name);
        for (int i = 0; i < f.Argc; i++)
        {
            sb.Append(' ');
            var a = f.Arg(i);
            sb.Append(a == null ? "?" : Runtime.BacktraceArgString(a));
        }
        return sb.Append(')').ToString();
    }

    // Backward compat: existing Generated.cs uses Invoke(params)
    // Includes stack overflow guard for C#-implemented functions that can recurse via Lisp dispatch
    private static ref int _stackCheckCounter => ref CallTls.StackCheck;
    public unsafe LispObject Invoke(params LispObject[] args)
    {
        if (++_stackCheckCounter % 256 == 0)
        {
            if (!Compat.TryEnsureSufficientExecutionStackWithMargin())
                throw new LispErrorException(new LispStorageCondition(
                    $"Stack overflow in function {Name ?? "anonymous"}"));
            ConditionSystem.CheckInterrupt();
        }
        // Push a debugger frame, exactly as InvokeSlow does for the same
        // args-array shape. This is the entry every non-emitted caller uses,
        // APPLY, and the tree-walk evaluator's own call site, so without it a
        // named callee reached that way was missing from BACKTRACE while the
        // identical call from compiled code (which goes through InvokeN) was
        // listed.
        var f = _func;
        var n = _frameName;
        if (n == null || _selfFramed) { var saved = s_top; try { if (RetainUnit == null) return f != null ? f(args) : CallDirectWithArray(args); if (s_top == null && !_selfFramed) return UnderKeeperArgs(args); var r = f != null ? f(args) : CallDirectWithArray(args); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
        return f != null ? CallFramed(f, n, args) : CallDirectFramed(n, args);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private unsafe LispObject UnderKeeperArgs(params LispObject[] args)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return Invoke(args); } finally { s_top = kf.Prev; }
    }

    private unsafe LispObject CallFramed(Func<LispObject[], LispObject> f, string n, LispObject[] args)
    {
        var fr = new Frame(n, args); Link(&fr); fr._unit = RetainUnit;
        try { return f(args); } finally { s_top = fr.Prev; }
    }

    private unsafe LispObject CallDirectFramed(string n, LispObject[] args)
    {
        var fr = new Frame(n, args); Link(&fr); fr._unit = RetainUnit;
        try { return CallDirectWithArray(args); } finally { s_top = fr.Prev; }
    }

    /// <summary>Invoke without recording a debugger frame. Used where the callee
    /// is scaffolding the debugger user is not asking about: the runtime's call to
    /// *DEBUGGER-HOOK* runs at the depth of the frame that signalled, so the hook
    /// appearing as frame 0 would shift every index the user reads and hide the
    /// signalling frame's variables behind its own.</summary>
    public unsafe LispObject InvokeNoFrame(params LispObject[] args)
    {
        PeriodicStackCheck();
        var f = _func;
        var saved = s_top;
        try
        {
            if (_sfMask != 0 && NoFrameDirect(args) is { } d) return d;
            var r = f != null ? f(args) : CallDirectWithArray(args);
            GC.KeepAlive(this);
            return r;
        }
        finally { s_top = saved; }
    }

    // Borrow 2 is set past the arity check, right before the body is entered.
    private LispObject? NoFrameDirect(LispObject[] args)
    {
        switch (args.Length)
        {
            case 0: if ((_sfMask & 1) != 0 && _func0 is { } f0) { CallTls.Borrow = 2; return f0(); } break;
            case 1: if ((_sfMask & 2) != 0 && _func1 is { } f1) { CallTls.Borrow = 2; return f1(args[0]); } break;
            case 2: if ((_sfMask & 4) != 0 && _func2 is { } f2) { CallTls.Borrow = 2; return f2(args[0], args[1]); } break;
            case 3: if ((_sfMask & 8) != 0 && _func3 is { } f3) { CallTls.Borrow = 2; return f3(args[0], args[1], args[2]); } break;
            case 4: if ((_sfMask & 16) != 0 && _func4 is { } f4) { CallTls.Borrow = 2; return f4(args[0], args[1], args[2], args[3]); } break;
        }
        return null;
    }

    // Direct-param invoke: avoids array allocation when _funcN is set.
    // Fallback paths through _func include periodic stack overflow check
    // to prevent uncatchable .NET StackOverflowException from recursive macros.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void PeriodicStackCheck()
    {
        if (++_stackCheckCounter % 256 == 0)
        {
            if (!Compat.TryEnsureSufficientExecutionStackWithMargin())
                throw new LispErrorException(new LispStorageCondition(
                    $"Stack overflow in function {Name ?? "anonymous"}"));
            ConditionSystem.CheckInterrupt();
        }
    }

    // Each InvokeN fast path calls PeriodicStackCheck before dispatching to
    // _funcN: this is the single choke point for every direct-delegate call
    // (assembler-installed simple functions, FASL closures, C# builtins, and
    // MakeDirectClosure closures: grep confirms nothing calls _funcN directly).
    // Without it, deep non-TCO recursion through the fast path never reaches
    // InvokeSlow's check and dies as an uncatchable .NET StackOverflowException
    // instead of a catchable Lisp "Stack overflow" PROGRAM-ERROR. The check is
    // AggressiveInlining and only a thread-static counter increment on the
    // common path (the real stack probe runs every 256th call).
    /// <summary>Args-array entry for a direct-params closure, which has no _func
    /// wrapper. Same arity check the wrapper used to perform, then spread.</summary>
    private LispObject CallDirectWithArray(LispObject[] args)
    {
        var env = Environment!;
        switch (_directDel)
        {
            case Func<object[], LispObject> d0:
                Runtime.CheckArityExact(StatsName ?? "anonymous", args, 0); return d0(env);
            case Func<object[], LispObject, LispObject> d1:
                Runtime.CheckArityExact(StatsName ?? "anonymous", args, 1); return d1(env, args[0]);
            case Func<object[], LispObject, LispObject, LispObject> d2:
                Runtime.CheckArityExact(StatsName ?? "anonymous", args, 2); return d2(env, args[0], args[1]);
            case Func<object[], LispObject, LispObject, LispObject, LispObject> d3:
                Runtime.CheckArityExact(StatsName ?? "anonymous", args, 3); return d3(env, args[0], args[1], args[2]);
            case Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject> d4:
                Runtime.CheckArityExact(StatsName ?? "anonymous", args, 4);
                return d4(env, args[0], args[1], args[2], args[3]);
            case Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> d5:
                Runtime.CheckArityExact(StatsName ?? "anonymous", args, 5);
                return d5(env, args[0], args[1], args[2], args[3], args[4]);
            case Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> d6:
                Runtime.CheckArityExact(StatsName ?? "anonymous", args, 6);
                return d6(env, args[0], args[1], args[2], args[3], args[4], args[5]);
            case Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> d7:
                Runtime.CheckArityExact(StatsName ?? "anonymous", args, 7);
                return d7(env, args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
            case Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> d8:
                Runtime.CheckArityExact(StatsName ?? "anonymous", args, 8);
                return d8(env, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
        }
        throw new LispErrorException(new LispProgramError(
            "internal: closure has no callable body"));
    }

    // A function that owns a compilation unit (RetainUnit) keeps that unit
    // reachable until its body returns. A function compiled in memory (COMPILE,
    // EVAL, COERCE of a lambda) is often the only owner of its unit, and its body
    // looks the unit up when it builds a closure or loads a unit constant.
    // Optimized code drops THIS as soon as the delegate has been read, so with
    // nothing else holding the function (FUNCALL of a fresh COERCE result) a GC in
    // the middle of the body collected the unit and the next closure in the body
    // failed with "closure compilation unit was collected while still callable".
    //
    //   - A framed call stores the unit in its frame (Frame._unit), which the GC
    //     sees for as long as the frame's Invoke is on the stack. Holding THIS
    //     with GC.KeepAlive in the finally instead cost 5% on a call-heavy loop.
    //   - An unframed call to such a function holds THIS with GC.KeepAlive after
    //     the call. A function without a unit keeps the plain call.
    //   - With no frame on the thread at all, the call goes through UnderKeeperN,
    //     which pushes a frame outside the backtrace for the tail calls the body
    //     makes (see the tail-call section). It is a method of its own so that
    //     its try/finally stays out of the InvokeN fast paths.
    public unsafe LispObject Invoke0()
    {
        if (_entryKey == 1)
        {
            var saved = s_top;
            try { var r = ((delegate*<LispFunction, LispObject>)_entry)(this); GC.KeepAlive(this); return r; }
            finally { s_top = saved; }
        }
        var f0 = _func0;
        if (f0 != null)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null || (_sfMask & 1) != 0) { var saved = s_top; try { if (RetainUnit == null) return f0(); if (s_top == null && (_sfMask & 1) == 0) return UnderKeeper0(); var r = f0(); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n); Link(&fr); fr._unit = RetainUnit;
            try { return f0(); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject> c0)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { var saved = s_top; try { if (RetainUnit == null) return c0(Environment!); if (s_top == null) return UnderKeeper0(); var r = c0(Environment!); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n); Link(&fr); fr._unit = RetainUnit;
            try { return c0(Environment!); } finally { s_top = fr.Prev; }
        }
        return InvokeSlow(Array.Empty<LispObject>());
    }

    /// <summary>InvokeN for ARGC of the loose arguments, with the delegate called in
    /// place when this function records no frame and holds no unit.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal LispObject InvokeArgc(int argc, LispObject a, LispObject? b, LispObject? c, LispObject? d)
    {
        if (_frameName == null && RetainUnit == null && (_sfMask & (1 << argc)) == 0)
        {
            switch (argc)
            {
                case 1: if (_func1 is { } f1) { PeriodicStackCheck(); return f1(a); } break;
                case 2: if (_func2 is { } f2) { PeriodicStackCheck(); return f2(a, b!); } break;
                case 3: if (_func3 is { } f3) { PeriodicStackCheck(); return f3(a, b!, c!); } break;
                default: if (_func4 is { } f4) { PeriodicStackCheck(); return f4(a, b!, c!, d!); } break;
            }
        }
        return argc switch
        {
            1 => Invoke1(a),
            2 => Invoke2(a, b!),
            3 => Invoke3(a, b!, c!),
            _ => Invoke4(a, b!, c!, d!)
        };
    }


    // InvokeN for compiled callers (CallN), without the filter: they repair the
    // frame list in their own handlers, and the filter costs the tail call.
    public unsafe LispObject InvokeFast0()
    {
        var f0 = _func0;
        if (f0 != null)
        {
            if ((_sfMask & 1) != 0) { if (RetainUnit == null) return f0(); var r = f0(); GC.KeepAlive(this); return r; }
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if (RetainUnit == null) return f0(); if (s_top == null) return UnderKeeper0(); var r = f0(); GC.KeepAlive(this); return r; }
            var fr = new Frame(n); Link(&fr); fr._unit = RetainUnit;
            try { return f0(); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject> c0)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if (RetainUnit == null) return c0(Environment!); if (s_top == null) return UnderKeeper0(); var r = c0(Environment!); GC.KeepAlive(this); return r; }
            var fr = new Frame(n); Link(&fr); fr._unit = RetainUnit;
            try { return c0(Environment!); } finally { s_top = fr.Prev; }
        }
        return InvokeSlow(Array.Empty<LispObject>());
    }

    public unsafe LispObject InvokeFast1(LispObject a)
    {
        var f1 = _func1;
        if (f1 != null)
        {
            if ((_sfMask & 2) != 0) { if (RetainUnit == null) return f1(a); var r = f1(a); GC.KeepAlive(this); return r; }
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if (RetainUnit == null) return f1(a); if (s_top == null) return UnderKeeper1(a); var r = f1(a); GC.KeepAlive(this); return r; }
            var fr = new Frame(n, a); Link(&fr); fr._unit = RetainUnit;
            try { return f1(a); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject, LispObject> c1)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if (RetainUnit == null) return c1(Environment!, a); if (s_top == null) return UnderKeeper1(a); var r = c1(Environment!, a); GC.KeepAlive(this); return r; }
            var fr = new Frame(n, a); Link(&fr); fr._unit = RetainUnit;
            try { return c1(Environment!, a); } finally { s_top = fr.Prev; }
        }
        return InvokeSlow(new[] { a });
    }

    public unsafe LispObject InvokeFast2(LispObject a, LispObject b)
    {
        var f2 = _func2;
        if (f2 != null)
        {
            if ((_sfMask & 4) != 0) { if (RetainUnit == null) return f2(a, b); var r = f2(a, b); GC.KeepAlive(this); return r; }
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if (RetainUnit == null) return f2(a, b); if (s_top == null) return UnderKeeper2(a, b); var r = f2(a, b); GC.KeepAlive(this); return r; }
            var fr = new Frame(n, a, b); Link(&fr); fr._unit = RetainUnit;
            try { return f2(a, b); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject, LispObject, LispObject> c2)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if (RetainUnit == null) return c2(Environment!, a, b); if (s_top == null) return UnderKeeper2(a, b); var r = c2(Environment!, a, b); GC.KeepAlive(this); return r; }
            var fr = new Frame(n, a, b); Link(&fr); fr._unit = RetainUnit;
            try { return c2(Environment!, a, b); } finally { s_top = fr.Prev; }
        }
        return InvokeSlow(new[] { a, b });
    }

    public unsafe LispObject InvokeFast3(LispObject a, LispObject b, LispObject c)
    {
        var f3 = _func3;
        if (f3 != null)
        {
            if ((_sfMask & 8) != 0) { if (RetainUnit == null) return f3(a, b, c); var r = f3(a, b, c); GC.KeepAlive(this); return r; }
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if (RetainUnit == null) return f3(a, b, c); if (s_top == null) return UnderKeeper3(a, b, c); var r = f3(a, b, c); GC.KeepAlive(this); return r; }
            var fr = new Frame(n, a, b, c); Link(&fr); fr._unit = RetainUnit;
            try { return f3(a, b, c); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject, LispObject, LispObject, LispObject> c3)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if (RetainUnit == null) return c3(Environment!, a, b, c); if (s_top == null) return UnderKeeper3(a, b, c); var r = c3(Environment!, a, b, c); GC.KeepAlive(this); return r; }
            var fr = new Frame(n, a, b, c); Link(&fr); fr._unit = RetainUnit;
            try { return c3(Environment!, a, b, c); } finally { s_top = fr.Prev; }
        }
        return InvokeSlow(new[] { a, b, c });
    }

    public unsafe LispObject InvokeFast4(LispObject a, LispObject b, LispObject c, LispObject d)
    {
        var f4 = _func4;
        if (f4 != null)
        {
            if ((_sfMask & 16) != 0) { if (RetainUnit == null) return f4(a, b, c, d); var r = f4(a, b, c, d); GC.KeepAlive(this); return r; }
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if (RetainUnit == null) return f4(a, b, c, d); if (s_top == null) return UnderKeeper4(a, b, c, d); var r = f4(a, b, c, d); GC.KeepAlive(this); return r; }
            var fr = new Frame(n, a, b, c, d); Link(&fr); fr._unit = RetainUnit;
            try { return f4(a, b, c, d); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject> c4)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if (RetainUnit == null) return c4(Environment!, a, b, c, d); if (s_top == null) return UnderKeeper4(a, b, c, d); var r = c4(Environment!, a, b, c, d); GC.KeepAlive(this); return r; }
            var fr = new Frame(n, a, b, c, d); Link(&fr); fr._unit = RetainUnit;
            try { return c4(Environment!, a, b, c, d); } finally { s_top = fr.Prev; }
        }
        return InvokeSlow(new[] { a, b, c, d });
    }

    public unsafe LispObject InvokeFast0M(int mode)
    {
        var f0 = _func0;
        if (f0 != null)
        {
            if ((_sfMask & 1) != 0) { if ((_mvModeMask & 1) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return f0(); var r = f0(); GC.KeepAlive(this); return r; }
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if ((_mvModeMask & 1) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return f0(); if (s_top == null) return UnderKeeper0M(mode); var r = f0(); GC.KeepAlive(this); return r; }
            var fr = new Frame(n); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 1) != 0) MultipleValues.SetMode(mode); return f0(); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject> c0)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if ((_mvModeMask & 1) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return c0(Environment!); if (s_top == null) return UnderKeeper0M(mode); var r = c0(Environment!); GC.KeepAlive(this); return r; }
            var fr = new Frame(n); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 1) != 0) MultipleValues.SetMode(mode); return c0(Environment!); } finally { s_top = fr.Prev; }
        }
        return Invoke0();
    }

    public unsafe LispObject InvokeFast1M(LispObject a, int mode)
    {
        var f1 = _func1;
        if (f1 != null)
        {
            if ((_sfMask & 2) != 0) { if ((_mvModeMask & 2) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return f1(a); var r = f1(a); GC.KeepAlive(this); return r; }
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if ((_mvModeMask & 2) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return f1(a); if (s_top == null) return UnderKeeper1M(a, mode); var r = f1(a); GC.KeepAlive(this); return r; }
            var fr = new Frame(n, a); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 2) != 0) MultipleValues.SetMode(mode); return f1(a); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject, LispObject> c1)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if ((_mvModeMask & 2) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return c1(Environment!, a); if (s_top == null) return UnderKeeper1M(a, mode); var r = c1(Environment!, a); GC.KeepAlive(this); return r; }
            var fr = new Frame(n, a); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 2) != 0) MultipleValues.SetMode(mode); return c1(Environment!, a); } finally { s_top = fr.Prev; }
        }
        return Invoke1(a);
    }

    public unsafe LispObject InvokeFast2M(LispObject a, LispObject b, int mode)
    {
        var f2 = _func2;
        if (f2 != null)
        {
            if ((_sfMask & 4) != 0) { if ((_mvModeMask & 4) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return f2(a, b); var r = f2(a, b); GC.KeepAlive(this); return r; }
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if ((_mvModeMask & 4) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return f2(a, b); if (s_top == null) return UnderKeeper2M(a, b, mode); var r = f2(a, b); GC.KeepAlive(this); return r; }
            var fr = new Frame(n, a, b); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 4) != 0) MultipleValues.SetMode(mode); return f2(a, b); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject, LispObject, LispObject> c2)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if ((_mvModeMask & 4) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return c2(Environment!, a, b); if (s_top == null) return UnderKeeper2M(a, b, mode); var r = c2(Environment!, a, b); GC.KeepAlive(this); return r; }
            var fr = new Frame(n, a, b); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 4) != 0) MultipleValues.SetMode(mode); return c2(Environment!, a, b); } finally { s_top = fr.Prev; }
        }
        return Invoke2(a, b);
    }

    public unsafe LispObject InvokeFast3M(LispObject a, LispObject b, LispObject c, int mode)
    {
        var f3 = _func3;
        if (f3 != null)
        {
            if ((_sfMask & 8) != 0) { if ((_mvModeMask & 8) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return f3(a, b, c); var r = f3(a, b, c); GC.KeepAlive(this); return r; }
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if ((_mvModeMask & 8) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return f3(a, b, c); if (s_top == null) return UnderKeeper3M(a, b, c, mode); var r = f3(a, b, c); GC.KeepAlive(this); return r; }
            var fr = new Frame(n, a, b, c); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 8) != 0) MultipleValues.SetMode(mode); return f3(a, b, c); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject, LispObject, LispObject, LispObject> c3)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if ((_mvModeMask & 8) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return c3(Environment!, a, b, c); if (s_top == null) return UnderKeeper3M(a, b, c, mode); var r = c3(Environment!, a, b, c); GC.KeepAlive(this); return r; }
            var fr = new Frame(n, a, b, c); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 8) != 0) MultipleValues.SetMode(mode); return c3(Environment!, a, b, c); } finally { s_top = fr.Prev; }
        }
        return Invoke3(a, b, c);
    }

    public unsafe LispObject InvokeFast4M(LispObject a, LispObject b, LispObject c, LispObject d, int mode)
    {
        var f4 = _func4;
        if (f4 != null)
        {
            if ((_sfMask & 16) != 0) { if ((_mvModeMask & 16) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return f4(a, b, c, d); var r = f4(a, b, c, d); GC.KeepAlive(this); return r; }
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if ((_mvModeMask & 16) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return f4(a, b, c, d); if (s_top == null) return UnderKeeper4M(a, b, c, d, mode); var r = f4(a, b, c, d); GC.KeepAlive(this); return r; }
            var fr = new Frame(n, a, b, c, d); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 16) != 0) MultipleValues.SetMode(mode); return f4(a, b, c, d); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject> c4)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { if ((_mvModeMask & 16) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return c4(Environment!, a, b, c, d); if (s_top == null) return UnderKeeper4M(a, b, c, d, mode); var r = c4(Environment!, a, b, c, d); GC.KeepAlive(this); return r; }
            var fr = new Frame(n, a, b, c, d); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 16) != 0) MultipleValues.SetMode(mode); return c4(Environment!, a, b, c, d); } finally { s_top = fr.Prev; }
        }
        return Invoke4(a, b, c, d);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private unsafe LispObject UnderKeeper0()
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return Invoke0(); } finally { s_top = kf.Prev; }
    }

    public unsafe LispObject Invoke1(LispObject a)
    {
        if (_entryKey == 2)
        {
            var saved = s_top;
            try { var r = ((delegate*<LispFunction, LispObject, LispObject>)_entry)(this, a); GC.KeepAlive(this); return r; }
            finally { s_top = saved; }
        }
        var f1 = _func1;
        if (f1 != null)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null || (_sfMask & 2) != 0) { var saved = s_top; try { if (RetainUnit == null) return f1(a); if (s_top == null && (_sfMask & 2) == 0) return UnderKeeper1(a); var r = f1(a); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n, a); Link(&fr); fr._unit = RetainUnit;
            try { return f1(a); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject, LispObject> c1)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { var saved = s_top; try { if (RetainUnit == null) return c1(Environment!, a); if (s_top == null) return UnderKeeper1(a); var r = c1(Environment!, a); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n, a); Link(&fr); fr._unit = RetainUnit;
            try { return c1(Environment!, a); } finally { s_top = fr.Prev; }
        }
        return InvokeSlow(new[] { a });
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private unsafe LispObject UnderKeeper1(LispObject a)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return Invoke1(a); } finally { s_top = kf.Prev; }
    }

    public unsafe LispObject Invoke2(LispObject a, LispObject b)
    {
        if (_entryKey == 3)
        {
            var saved = s_top;
            try { var r = ((delegate*<LispFunction, LispObject, LispObject, LispObject>)_entry)(this, a, b); GC.KeepAlive(this); return r; }
            finally { s_top = saved; }
        }
        var f2 = _func2;
        if (f2 != null)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null || (_sfMask & 4) != 0) { var saved = s_top; try { if (RetainUnit == null) return f2(a, b); if (s_top == null && (_sfMask & 4) == 0) return UnderKeeper2(a, b); var r = f2(a, b); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n, a, b); Link(&fr); fr._unit = RetainUnit;
            try { return f2(a, b); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject, LispObject, LispObject> c2)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { var saved = s_top; try { if (RetainUnit == null) return c2(Environment!, a, b); if (s_top == null) return UnderKeeper2(a, b); var r = c2(Environment!, a, b); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n, a, b); Link(&fr); fr._unit = RetainUnit;
            try { return c2(Environment!, a, b); } finally { s_top = fr.Prev; }
        }
        return InvokeSlow(new[] { a, b });
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private unsafe LispObject UnderKeeper2(LispObject a, LispObject b)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return Invoke2(a, b); } finally { s_top = kf.Prev; }
    }

    public unsafe LispObject Invoke3(LispObject a, LispObject b, LispObject c)
    {
        if (_entryKey == 4)
        {
            var saved = s_top;
            try { var r = ((delegate*<LispFunction, LispObject, LispObject, LispObject, LispObject>)_entry)(this, a, b, c); GC.KeepAlive(this); return r; }
            finally { s_top = saved; }
        }
        var f3 = _func3;
        if (f3 != null)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null || (_sfMask & 8) != 0) { var saved = s_top; try { if (RetainUnit == null) return f3(a, b, c); if (s_top == null && (_sfMask & 8) == 0) return UnderKeeper3(a, b, c); var r = f3(a, b, c); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n, a, b, c); Link(&fr); fr._unit = RetainUnit;
            try { return f3(a, b, c); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject, LispObject, LispObject, LispObject> c3)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { var saved = s_top; try { if (RetainUnit == null) return c3(Environment!, a, b, c); if (s_top == null) return UnderKeeper3(a, b, c); var r = c3(Environment!, a, b, c); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n, a, b, c); Link(&fr); fr._unit = RetainUnit;
            try { return c3(Environment!, a, b, c); } finally { s_top = fr.Prev; }
        }
        return InvokeSlow(new[] { a, b, c });
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private unsafe LispObject UnderKeeper3(LispObject a, LispObject b, LispObject c)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return Invoke3(a, b, c); } finally { s_top = kf.Prev; }
    }

    public unsafe LispObject Invoke4(LispObject a, LispObject b, LispObject c, LispObject d)
    {
        if (_entryKey == 5)
        {
            var saved = s_top;
            try { var r = ((delegate*<LispFunction, LispObject, LispObject, LispObject, LispObject, LispObject>)_entry)(this, a, b, c, d); GC.KeepAlive(this); return r; }
            finally { s_top = saved; }
        }
        var f4 = _func4;
        if (f4 != null)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null || (_sfMask & 16) != 0) { var saved = s_top; try { if (RetainUnit == null) return f4(a, b, c, d); if (s_top == null && (_sfMask & 16) == 0) return UnderKeeper4(a, b, c, d); var r = f4(a, b, c, d); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n, a, b, c, d); Link(&fr); fr._unit = RetainUnit;
            try { return f4(a, b, c, d); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject> c4)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { var saved = s_top; try { if (RetainUnit == null) return c4(Environment!, a, b, c, d); if (s_top == null) return UnderKeeper4(a, b, c, d); var r = c4(Environment!, a, b, c, d); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n, a, b, c, d); Link(&fr); fr._unit = RetainUnit;
            try { return c4(Environment!, a, b, c, d); } finally { s_top = fr.Prev; }
        }
        return InvokeSlow(new[] { a, b, c, d });
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private unsafe LispObject UnderKeeper4(LispObject a, LispObject b, LispObject c, LispObject d)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return Invoke4(a, b, c, d); } finally { s_top = kf.Prev; }
    }

    public unsafe LispObject Invoke5(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e)
    {
        var f5 = _func5;
        if (f5 != null)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null || (_sfMask & 32) != 0) { var saved = s_top; try { if (RetainUnit == null) return f5(a, b, c, d, e); if (s_top == null && (_sfMask & 32) == 0) return UnderKeeper5(a, b, c, d, e); var r = f5(a, b, c, d, e); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            FrameExt x = default; x.A4 = e;
            var fr = new Frame(n, 5, a, b, c, d, &x); Link(&fr); fr._unit = RetainUnit;
            try { return f5(a, b, c, d, e); } finally { s_top = fr.Prev; }
        }
        return InvokeSlow(new[] { a, b, c, d, e });
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private unsafe LispObject UnderKeeper5(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return Invoke5(a, b, c, d, e); } finally { s_top = kf.Prev; }
    }

    public unsafe LispObject Invoke6(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f)
    {
        var f6 = _func6;
        if (f6 != null)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null || (_sfMask & 64) != 0) { var saved = s_top; try { if (RetainUnit == null) return f6(a, b, c, d, e, f); if (s_top == null && (_sfMask & 64) == 0) return UnderKeeper6(a, b, c, d, e, f); var r = f6(a, b, c, d, e, f); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            FrameExt x = default; x.A4 = e; x.A5 = f;
            var fr = new Frame(n, 6, a, b, c, d, &x); Link(&fr); fr._unit = RetainUnit;
            try { return f6(a, b, c, d, e, f); } finally { s_top = fr.Prev; }
        }
        return InvokeSlow(new[] { a, b, c, d, e, f });
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private unsafe LispObject UnderKeeper6(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return Invoke6(a, b, c, d, e, f); } finally { s_top = kf.Prev; }
    }

    public unsafe LispObject Invoke7(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f, LispObject g)
    {
        var f7 = _func7;
        if (f7 != null)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null || (_sfMask & 128) != 0) { var saved = s_top; try { if (RetainUnit == null) return f7(a, b, c, d, e, f, g); if (s_top == null && (_sfMask & 128) == 0) return UnderKeeper7(a, b, c, d, e, f, g); var r = f7(a, b, c, d, e, f, g); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            FrameExt x = default; x.A4 = e; x.A5 = f; x.A6 = g;
            var fr = new Frame(n, 7, a, b, c, d, &x); Link(&fr); fr._unit = RetainUnit;
            try { return f7(a, b, c, d, e, f, g); } finally { s_top = fr.Prev; }
        }
        return InvokeSlow(new[] { a, b, c, d, e, f, g });
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private unsafe LispObject UnderKeeper7(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f, LispObject g)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return Invoke7(a, b, c, d, e, f, g); } finally { s_top = kf.Prev; }
    }

    public unsafe LispObject Invoke8(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f, LispObject g, LispObject h)
    {
        var f8 = _func8;
        if (f8 != null)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null || (_sfMask & 256) != 0) { var saved = s_top; try { if (RetainUnit == null) return f8(a, b, c, d, e, f, g, h); if (s_top == null && (_sfMask & 256) == 0) return UnderKeeper8(a, b, c, d, e, f, g, h); var r = f8(a, b, c, d, e, f, g, h); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            FrameExt x = default; x.A4 = e; x.A5 = f; x.A6 = g; x.A7 = h;
            var fr = new Frame(n, 8, a, b, c, d, &x); Link(&fr); fr._unit = RetainUnit;
            try { return f8(a, b, c, d, e, f, g, h); } finally { s_top = fr.Prev; }
        }
        return InvokeSlow(new[] { a, b, c, d, e, f, g, h });
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private unsafe LispObject UnderKeeper8(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f, LispObject g, LispObject h)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return Invoke8(a, b, c, d, e, f, g, h); } finally { s_top = kf.Prev; }
    }

    // --- Invoke with a value mode (see MultipleValues.TakeMode) ---
    //
    // Bit N of _mvModeMask is set when the arity-N direct entry (_funcN, or the
    // closure body for a closure of arity N) is a compiled body whose first
    // instruction takes the mode. InvokeNM passes MODE on only to such an entry
    // and only right before calling it, so a mode can never be left behind for
    // some other body to pick up; any other entry is called exactly as InvokeN
    // calls it.
    internal int _mvModeMask;

    /// <summary>The arity-ARITY direct entry reads a value mode on entry.</summary>
    public void MarkMvModeEntry(int arity)
    {
        if ((uint)arity < 32) _mvModeMask |= 1 << arity;
    }

    /// <summary>MARKMVMODEENTRY for a function just built on the stack: returns FN.</summary>
    public static LispObject WithMvModeEntry(LispObject fn, int arity)
    {
        ((LispFunction)fn).MarkMvModeEntry(arity);
        return fn;
    }

    internal bool MvModeEntry(int arity) => (uint)arity < 32 && (_mvModeMask & (1 << arity)) != 0;

    // ---- Tail calls ----
    //
    // A call in tail position in a compiled body does not go through InvokeN:
    // InvokeN runs the callee inside the try/finally that unlinks its debugger
    // frame, so its own native frame stays on the stack for the whole call, and
    // a chain of tail calls grew the stack by one InvokeN frame per call however
    // the body made the call. Instead the body asks TailPrep how to make the
    // call and then makes it with the IL tail. prefix itself, straight into the
    // callee's body delegate:
    //
    //   - When the innermost debugger frame is the calling body's own, the
    //     frame is rewritten to name the callee (Retarget) and no new one is
    //     pushed. The backtrace then shows the function that was tail-called in
    //     place of the one that called it, as SBCL's does.
    //   - When the callee records no frame (a closure, a lambda, a function
    //     compiled with (debug 0)) there is nothing to push either.
    //   - Otherwise (the callee needs a frame and the caller has none to give
    //     it) the call goes through InvokeN as before. The callee then owns a
    //     frame, so the tail calls it makes in turn reuse it: the stack grows
    //     once per such transition, not once per call.
    //
    // Whether the innermost frame is the caller's own is decided by name: the
    // emitter passes the frame name the calling body runs under (null when it
    // cannot say). A body that records no frame of its own (a closure, a lambda,
    // a (debug 0) function) passes TailAnonName instead, and owns the innermost
    // frame when a tail call lent it that frame (Frame.Lent): a tail call to such
    // a callee leaves the frame's name alone, so a backtrace still shows the last
    // named function of the chain, as it shows the named caller of a lambda
    // today, and never a C# builtin's missing name. A wrong guess is only ever
    // cosmetic: the frame always stays linked by the Invoke that pushed it, so
    // only the name and arguments a backtrace shows for it can be off.
    //
    // A callee that owns a compilation unit (RetainUnit: compiled in memory, and
    // possibly reachable from nothing but the call being made) has to stay
    // reachable while its body runs, which InvokeN does by holding the function
    // across the call. A tail call gives up the native frame that held it, so
    // TailPrep adds the unit to the innermost frame instead (Frame.KeepUnit),
    // whoever owns it: that frame's Invoke is below the callee on the native
    // stack and unlinks it only after the callee has returned. Units are only
    // added, never replaced, because the innermost frame may also be holding a
    // unit for a body that is waiting further down. With no frame on the thread
    // at all, the call goes through InvokeN, which then pushes a frame outside
    // the backtrace (UnitKeeperName, Depth 0) for the tail calls after it.

    /// <summary>Name of the frame a call pushes when the thread has no frame at
    /// all and the callee owns a compilation unit: it gives the tail calls the
    /// callee makes a frame to keep units in (Frame.KeepUnit). It has Depth 0,
    /// so it is not part of the backtrace, and no body ever owns it.</summary>
    internal const string UnitKeeperName = "\0unit-keeper";

    /// <summary>What a body that records no frame of its own passes to
    /// TailPrep as its frame name. No frame has an empty name.</summary>
    public const string TailAnonName = "";

    public const int TailKindInvoke = 0, TailKindDirect = 1, TailKindClosure = 2;

    private int TailKind(int arity) => arity switch
    {
            0 => _func0 != null ? TailKindDirect : _directDel is Func<object[], LispObject> ? TailKindClosure : TailKindInvoke,
            1 => _func1 != null ? TailKindDirect : _directDel is Func<object[], LispObject, LispObject> ? TailKindClosure : TailKindInvoke,
            2 => _func2 != null ? TailKindDirect : _directDel is Func<object[], LispObject, LispObject, LispObject> ? TailKindClosure : TailKindInvoke,
            3 => _func3 != null ? TailKindDirect : _directDel is Func<object[], LispObject, LispObject, LispObject, LispObject> ? TailKindClosure : TailKindInvoke,
            4 => _func4 != null ? TailKindDirect : _directDel is Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject> ? TailKindClosure : TailKindInvoke,
            5 => _func5 != null ? TailKindDirect : _directDel is Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> ? TailKindClosure : TailKindInvoke,
            6 => _func6 != null ? TailKindDirect : _directDel is Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> ? TailKindClosure : TailKindInvoke,
            7 => _func7 != null ? TailKindDirect : _directDel is Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> ? TailKindClosure : TailKindInvoke,
            8 => _func8 != null ? TailKindDirect : _directDel is Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> ? TailKindClosure : TailKindInvoke,
            _ => TailKindInvoke,
    };

    /// <summary>Prepare a tail call to CALLEE with N arguments (TailPrepN) from
    /// a body whose frame name is SELF, passing the value mode MODE (-1 for a
    /// call that passes none). Returns how the caller makes the call:
    /// TailKindDirect through TailDirectN, TailKindClosure through TailClosureN
    /// with Environment, TailKindInvoke through InvokeN. For the first two the
    /// frame and the value mode are already set up.
    ///
    /// The common cases, a named function tail-calling another through the frame
    /// it owns and any call to a callee that records no frame, are decided
    /// inline at the call site: frame names are interned (FrameNameOf) and SELF
    /// is a string literal, so owning the frame is a reference comparison.
    /// Everything else goes to TailPrepCore.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe int TailPrep0(LispFunction callee, int mode, string? self)
    {
        if (callee._func0 != null && (callee._sfMask & 1) == 0)
        {
            var top = s_top;
            var name = callee._frameName;
            bool mine = top != null && ReferenceEquals(top->Name, self);
            // A callee with a compilation unit the innermost frame does not keep
            // yet goes the long way (TailPrepCore keeps it there).
            var unit = callee.RetainUnit;
            if (unit != null && (top == null || !ReferenceEquals(top->_unit, unit)))
                return callee.TailPrepCore(0, mode, self, null, null, null, null);
            if (name == null)
            {
                if (mine) top->_argc |= Frame.LentBit;
            }
            else if (mine)
            {
                top->Name = name; top->_argc = 0; top->_ext = null;
                if (top->_rest != null) top->_rest = null;
            }
            else return callee.TailPrepCore(0, mode, self, null, null, null, null);
            if (mode >= 0 && (callee._mvModeMask & 1) != 0) MultipleValues.SetMode(mode);
            ConditionSystem.PollInterrupt();
            return TailKindDirect;
        }
        return callee.TailPrepCore(0, mode, self, null, null, null, null);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe int TailPrep1(LispFunction callee, int mode, string? self, LispObject a0)
    {
        if (callee._func1 != null && (callee._sfMask & 2) == 0)
        {
            var top = s_top;
            var name = callee._frameName;
            bool mine = top != null && ReferenceEquals(top->Name, self);
            // A callee with a compilation unit the innermost frame does not keep
            // yet goes the long way (TailPrepCore keeps it there).
            var unit = callee.RetainUnit;
            if (unit != null && (top == null || !ReferenceEquals(top->_unit, unit)))
                return callee.TailPrepCore(1, mode, self, a0, null, null, null);
            if (name == null)
            {
                if (mine) top->_argc |= Frame.LentBit;
            }
            else if (mine)
            {
                top->Name = name; top->_argc = 1; top->_ext = null;
                if (top->_rest != null) top->_rest = null; top->_a0 = a0;
            }
            else return callee.TailPrepCore(1, mode, self, a0, null, null, null);
            if (mode >= 0 && (callee._mvModeMask & 2) != 0) MultipleValues.SetMode(mode);
            ConditionSystem.PollInterrupt();
            return TailKindDirect;
        }
        return callee.TailPrepCore(1, mode, self, a0, null, null, null);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe int TailPrep2(LispFunction callee, int mode, string? self, LispObject a0, LispObject a1)
    {
        if (callee._func2 != null && (callee._sfMask & 4) == 0)
        {
            var top = s_top;
            var name = callee._frameName;
            bool mine = top != null && ReferenceEquals(top->Name, self);
            // A callee with a compilation unit the innermost frame does not keep
            // yet goes the long way (TailPrepCore keeps it there).
            var unit = callee.RetainUnit;
            if (unit != null && (top == null || !ReferenceEquals(top->_unit, unit)))
                return callee.TailPrepCore(2, mode, self, a0, a1, null, null);
            if (name == null)
            {
                if (mine) top->_argc |= Frame.LentBit;
            }
            else if (mine)
            {
                top->Name = name; top->_argc = 2; top->_ext = null;
                if (top->_rest != null) top->_rest = null; top->_a0 = a0; top->_a1 = a1;
            }
            else return callee.TailPrepCore(2, mode, self, a0, a1, null, null);
            if (mode >= 0 && (callee._mvModeMask & 4) != 0) MultipleValues.SetMode(mode);
            ConditionSystem.PollInterrupt();
            return TailKindDirect;
        }
        return callee.TailPrepCore(2, mode, self, a0, a1, null, null);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe int TailPrep3(LispFunction callee, int mode, string? self, LispObject a0, LispObject a1, LispObject a2)
    {
        if (callee._func3 != null && (callee._sfMask & 8) == 0)
        {
            var top = s_top;
            var name = callee._frameName;
            bool mine = top != null && ReferenceEquals(top->Name, self);
            // A callee with a compilation unit the innermost frame does not keep
            // yet goes the long way (TailPrepCore keeps it there).
            var unit = callee.RetainUnit;
            if (unit != null && (top == null || !ReferenceEquals(top->_unit, unit)))
                return callee.TailPrepCore(3, mode, self, a0, a1, a2, null);
            if (name == null)
            {
                if (mine) top->_argc |= Frame.LentBit;
            }
            else if (mine)
            {
                top->Name = name; top->_argc = 3; top->_ext = null;
                if (top->_rest != null) top->_rest = null; top->_a0 = a0; top->_a1 = a1; top->_a2 = a2;
            }
            else return callee.TailPrepCore(3, mode, self, a0, a1, a2, null);
            if (mode >= 0 && (callee._mvModeMask & 8) != 0) MultipleValues.SetMode(mode);
            ConditionSystem.PollInterrupt();
            return TailKindDirect;
        }
        return callee.TailPrepCore(3, mode, self, a0, a1, a2, null);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe int TailPrep4(LispFunction callee, int mode, string? self, LispObject a0, LispObject a1, LispObject a2, LispObject a3)
    {
        if (callee._func4 != null && (callee._sfMask & 16) == 0)
        {
            var top = s_top;
            var name = callee._frameName;
            bool mine = top != null && ReferenceEquals(top->Name, self);
            // A callee with a compilation unit the innermost frame does not keep
            // yet goes the long way (TailPrepCore keeps it there).
            var unit = callee.RetainUnit;
            if (unit != null && (top == null || !ReferenceEquals(top->_unit, unit)))
                return callee.TailPrepCore(4, mode, self, a0, a1, a2, a3);
            if (name == null)
            {
                if (mine) top->_argc |= Frame.LentBit;
            }
            else if (mine)
            {
                top->Name = name; top->_argc = 4; top->_ext = null;
                if (top->_rest != null) top->_rest = null; top->_a0 = a0; top->_a1 = a1; top->_a2 = a2; top->_a3 = a3;
            }
            else return callee.TailPrepCore(4, mode, self, a0, a1, a2, a3);
            if (mode >= 0 && (callee._mvModeMask & 16) != 0) MultipleValues.SetMode(mode);
            ConditionSystem.PollInterrupt();
            return TailKindDirect;
        }
        return callee.TailPrepCore(4, mode, self, a0, a1, a2, a3);
    }

    /// <summary>TailPrep for 5 to 8 arguments.</summary>
    public static int TailPrep8(LispFunction callee, int arity, int mode, string? self,
                                LispObject? a0, LispObject? a1, LispObject? a2, LispObject? a3,
                                LispObject? a4, LispObject? a5, LispObject? a6, LispObject? a7)
        => callee.TailPrepCore(arity, mode, self, a0, a1, a2, a3, a4, a5, a6, a7);

    /// <summary>The tree-walk evaluator's trampoline is about to run CALLEE's
    /// body in place of OWNER's, OWNER being the function whose frame the chain
    /// runs under. Returns the function the innermost frame stands for afterwards,
    /// or null when CALLEE needs a frame and the innermost one is not OWNER's to
    /// give, so the call has to be an ordinary one.</summary>
    internal static unsafe LispFunction? InterpTailFrame(LispFunction owner, LispFunction callee, LispObject args)
    {
        ConditionSystem.PollInterrupt();
        var name = callee._frameName;
        if (name == null) return owner;
        var top = s_top;
        var mine = owner._frameName;
        if (top == null || mine == null || !ReferenceEquals(top->Name, mine)) return null;
        int argc = 0;
        for (var a = args; a is Cons c; a = c.Cdr) argc++;
        if (argc > 4)
        {
            var all = new LispObject[argc];
            int i = 0;
            for (var a = args; a is Cons c; a = c.Cdr) all[i++] = c.Car;
            top->Retarget(name, all);
        }
        else
        {
            LispObject? a0 = null, a1 = null, a2 = null, a3 = null;
            if (args is Cons c0) { a0 = c0.Car;
                if (c0.Cdr is Cons c1) { a1 = c1.Car;
                    if (c1.Cdr is Cons c2) { a2 = c2.Car;
                        if (c2.Cdr is Cons c3) a3 = c3.Car; } } }
            top->Retarget(name, argc, a0, a1, a2, a3, null, null, null, null);
        }
        return callee;
    }

    private unsafe int TailPrepCore(int arity, int mode, string? self,
                                    LispObject? a0, LispObject? a1, LispObject? a2, LispObject? a3,
                                    LispObject? a4 = null, LispObject? a5 = null,
                                    LispObject? a6 = null, LispObject? a7 = null)
    {
        int kind = TailKind(arity);
        if (kind == TailKindInvoke) return kind;
        ConditionSystem.PollInterrupt();
        var top = s_top;
        var callee = _frameName;
        bool owned = self != null && top != null
            && (self.Length == 0 ? top->Lent : string.Equals(top->Name, self));
        if ((_sfMask & (1 << arity)) != 0)
        {
            if (mode >= 0 && (_mvModeMask & (1 << arity)) != 0) MultipleValues.SetMode(mode);
            if (owned) CallTls.Borrow = 1;
            return kind;
        }
        var unit = RetainUnit;
        if (unit != null && top == null) return TailKindInvoke;
        if (callee != null)
        {
            if (!owned) return TailKindInvoke;
            top->Retarget(callee, arity, a0, a1, a2, a3, a4, a5, a6, a7);
        }
        else if (owned)
            top->Lend();
        if (unit != null) top->KeepUnit(unit);
        if (mode >= 0 && (_mvModeMask & (1 << arity)) != 0) MultipleValues.SetMode(mode);
        return kind;
    }

    // The body delegates a tail call made through TailPrep calls. Only valid
    // after TailPrep has answered TailKindDirect / TailKindClosure for the arity.
    public Func<LispObject> TailDirect0 => _func0!;
    public Func<LispObject, LispObject> TailDirect1 => _func1!;
    public Func<LispObject, LispObject, LispObject> TailDirect2 => _func2!;
    public Func<LispObject, LispObject, LispObject, LispObject> TailDirect3 => _func3!;
    public Func<LispObject, LispObject, LispObject, LispObject, LispObject> TailDirect4 => _func4!;
    public Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> TailDirect5 => _func5!;
    public Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> TailDirect6 => _func6!;
    public Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> TailDirect7 => _func7!;
    public Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> TailDirect8 => _func8!;
    public Func<object[], LispObject> TailClosure0 => Unsafe.As<Func<object[], LispObject>>(_directDel!);
    public Func<object[], LispObject, LispObject> TailClosure1 => Unsafe.As<Func<object[], LispObject, LispObject>>(_directDel!);
    public Func<object[], LispObject, LispObject, LispObject> TailClosure2 => Unsafe.As<Func<object[], LispObject, LispObject, LispObject>>(_directDel!);
    public Func<object[], LispObject, LispObject, LispObject, LispObject> TailClosure3 => Unsafe.As<Func<object[], LispObject, LispObject, LispObject, LispObject>>(_directDel!);
    public Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject> TailClosure4 => Unsafe.As<Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject>>(_directDel!);
    public Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> TailClosure5 => Unsafe.As<Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>>(_directDel!);
    public Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> TailClosure6 => Unsafe.As<Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>>(_directDel!);
    public Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> TailClosure7 => Unsafe.As<Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>>(_directDel!);
    public Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> TailClosure8 => Unsafe.As<Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>>(_directDel!);

    public unsafe LispObject Invoke0M(int mode)
    {
        var f0 = _func0;
        if (f0 != null)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null || (_sfMask & 1) != 0) { var saved = s_top; try { if ((_mvModeMask & 1) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return f0(); if (s_top == null && (_sfMask & 1) == 0) return UnderKeeper0M(mode); var r = f0(); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 1) != 0) MultipleValues.SetMode(mode); return f0(); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject> c0)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { var saved = s_top; try { if ((_mvModeMask & 1) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return c0(Environment!); if (s_top == null) return UnderKeeper0M(mode); var r = c0(Environment!); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 1) != 0) MultipleValues.SetMode(mode); return c0(Environment!); } finally { s_top = fr.Prev; }
        }
        return Invoke0();
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private unsafe LispObject UnderKeeper0M(int mode)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return Invoke0M(mode); } finally { s_top = kf.Prev; }
    }

    public unsafe LispObject Invoke1M(LispObject a, int mode)
    {
        var f1 = _func1;
        if (f1 != null)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null || (_sfMask & 2) != 0) { var saved = s_top; try { if ((_mvModeMask & 2) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return f1(a); if (s_top == null && (_sfMask & 2) == 0) return UnderKeeper1M(a, mode); var r = f1(a); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n, a); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 2) != 0) MultipleValues.SetMode(mode); return f1(a); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject, LispObject> c1)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { var saved = s_top; try { if ((_mvModeMask & 2) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return c1(Environment!, a); if (s_top == null) return UnderKeeper1M(a, mode); var r = c1(Environment!, a); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n, a); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 2) != 0) MultipleValues.SetMode(mode); return c1(Environment!, a); } finally { s_top = fr.Prev; }
        }
        return Invoke1(a);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private unsafe LispObject UnderKeeper1M(LispObject a, int mode)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return Invoke1M(a, mode); } finally { s_top = kf.Prev; }
    }

    public unsafe LispObject Invoke2M(LispObject a, LispObject b, int mode)
    {
        var f2 = _func2;
        if (f2 != null)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null || (_sfMask & 4) != 0) { var saved = s_top; try { if ((_mvModeMask & 4) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return f2(a, b); if (s_top == null && (_sfMask & 4) == 0) return UnderKeeper2M(a, b, mode); var r = f2(a, b); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n, a, b); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 4) != 0) MultipleValues.SetMode(mode); return f2(a, b); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject, LispObject, LispObject> c2)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { var saved = s_top; try { if ((_mvModeMask & 4) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return c2(Environment!, a, b); if (s_top == null) return UnderKeeper2M(a, b, mode); var r = c2(Environment!, a, b); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n, a, b); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 4) != 0) MultipleValues.SetMode(mode); return c2(Environment!, a, b); } finally { s_top = fr.Prev; }
        }
        return Invoke2(a, b);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private unsafe LispObject UnderKeeper2M(LispObject a, LispObject b, int mode)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return Invoke2M(a, b, mode); } finally { s_top = kf.Prev; }
    }

    public unsafe LispObject Invoke3M(LispObject a, LispObject b, LispObject c, int mode)
    {
        var f3 = _func3;
        if (f3 != null)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null || (_sfMask & 8) != 0) { var saved = s_top; try { if ((_mvModeMask & 8) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return f3(a, b, c); if (s_top == null && (_sfMask & 8) == 0) return UnderKeeper3M(a, b, c, mode); var r = f3(a, b, c); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n, a, b, c); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 8) != 0) MultipleValues.SetMode(mode); return f3(a, b, c); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject, LispObject, LispObject, LispObject> c3)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { var saved = s_top; try { if ((_mvModeMask & 8) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return c3(Environment!, a, b, c); if (s_top == null) return UnderKeeper3M(a, b, c, mode); var r = c3(Environment!, a, b, c); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n, a, b, c); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 8) != 0) MultipleValues.SetMode(mode); return c3(Environment!, a, b, c); } finally { s_top = fr.Prev; }
        }
        return Invoke3(a, b, c);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private unsafe LispObject UnderKeeper3M(LispObject a, LispObject b, LispObject c, int mode)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return Invoke3M(a, b, c, mode); } finally { s_top = kf.Prev; }
    }

    public unsafe LispObject Invoke4M(LispObject a, LispObject b, LispObject c, LispObject d, int mode)
    {
        var f4 = _func4;
        if (f4 != null)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null || (_sfMask & 16) != 0) { var saved = s_top; try { if ((_mvModeMask & 16) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return f4(a, b, c, d); if (s_top == null && (_sfMask & 16) == 0) return UnderKeeper4M(a, b, c, d, mode); var r = f4(a, b, c, d); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n, a, b, c, d); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 16) != 0) MultipleValues.SetMode(mode); return f4(a, b, c, d); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject> c4)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { var saved = s_top; try { if ((_mvModeMask & 16) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return c4(Environment!, a, b, c, d); if (s_top == null) return UnderKeeper4M(a, b, c, d, mode); var r = c4(Environment!, a, b, c, d); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            var fr = new Frame(n, a, b, c, d); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 16) != 0) MultipleValues.SetMode(mode); return c4(Environment!, a, b, c, d); } finally { s_top = fr.Prev; }
        }
        return Invoke4(a, b, c, d);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private unsafe LispObject UnderKeeper4M(LispObject a, LispObject b, LispObject c, LispObject d, int mode)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return Invoke4M(a, b, c, d, mode); } finally { s_top = kf.Prev; }
    }

    public unsafe LispObject Invoke5M(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, int mode)
    {
        var f5 = _func5;
        if (f5 != null)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null || (_sfMask & 32) != 0) { var saved = s_top; try { if ((_mvModeMask & 32) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return f5(a, b, c, d, e); if (s_top == null && (_sfMask & 32) == 0) return UnderKeeper5M(a, b, c, d, e, mode); var r = f5(a, b, c, d, e); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            FrameExt x = default; x.A4 = e; var fr = new Frame(n, 5, a, b, c, d, &x); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 32) != 0) MultipleValues.SetMode(mode); return f5(a, b, c, d, e); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> c5)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { var saved = s_top; try { if ((_mvModeMask & 32) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return c5(Environment!, a, b, c, d, e); if (s_top == null) return UnderKeeper5M(a, b, c, d, e, mode); var r = c5(Environment!, a, b, c, d, e); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            FrameExt x = default; x.A4 = e; var fr = new Frame(n, 5, a, b, c, d, &x); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 32) != 0) MultipleValues.SetMode(mode); return c5(Environment!, a, b, c, d, e); } finally { s_top = fr.Prev; }
        }
        return Invoke5(a, b, c, d, e);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private unsafe LispObject UnderKeeper5M(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, int mode)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return Invoke5M(a, b, c, d, e, mode); } finally { s_top = kf.Prev; }
    }

    public unsafe LispObject Invoke6M(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f, int mode)
    {
        var f6 = _func6;
        if (f6 != null)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null || (_sfMask & 64) != 0) { var saved = s_top; try { if ((_mvModeMask & 64) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return f6(a, b, c, d, e, f); if (s_top == null && (_sfMask & 64) == 0) return UnderKeeper6M(a, b, c, d, e, f, mode); var r = f6(a, b, c, d, e, f); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            FrameExt x = default; x.A4 = e; x.A5 = f; var fr = new Frame(n, 6, a, b, c, d, &x); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 64) != 0) MultipleValues.SetMode(mode); return f6(a, b, c, d, e, f); } finally { s_top = fr.Prev; }
        }
        if (_directDel is Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> c6)
        {
            PeriodicStackCheck();
            var n = _frameName;
            if (n == null) { var saved = s_top; try { if ((_mvModeMask & 64) != 0) MultipleValues.SetMode(mode); if (RetainUnit == null) return c6(Environment!, a, b, c, d, e, f); if (s_top == null) return UnderKeeper6M(a, b, c, d, e, f, mode); var r = c6(Environment!, a, b, c, d, e, f); GC.KeepAlive(this); return r; } finally { s_top = saved; } }
            FrameExt x = default; x.A4 = e; x.A5 = f; var fr = new Frame(n, 6, a, b, c, d, &x); Link(&fr); fr._unit = RetainUnit;
            try { if ((_mvModeMask & 64) != 0) MultipleValues.SetMode(mode); return c6(Environment!, a, b, c, d, e, f); } finally { s_top = fr.Prev; }
        }
        return Invoke6(a, b, c, d, e, f);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private unsafe LispObject UnderKeeper6M(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f, int mode)
    {
        var kf = new Frame(UnitKeeperName); Link(&kf); kf.Depth = 0;
        try { return Invoke6M(a, b, c, d, e, f, mode); } finally { s_top = kf.Prev; }
    }

    // InvokeN for code that runs once: the method EVAL builds for a top level
    // form with no loop. Such a method is JIT-compiled with full optimization,
    // which inlines InvokeN (frame push, try/finally, the direct-delegate arms)
    // into it and cost several times the JIT time of the rest of the method,
    // to save one call that runs once. Code that can run more than once calls
    // InvokeN itself.
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    public LispObject InvokeOnce0() => Invoke0();
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    public LispObject InvokeOnce1(LispObject a) => Invoke1(a);
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    public LispObject InvokeOnce2(LispObject a, LispObject b) => Invoke2(a, b);
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    public LispObject InvokeOnce3(LispObject a, LispObject b, LispObject c) => Invoke3(a, b, c);
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    public LispObject InvokeOnce4(LispObject a, LispObject b, LispObject c, LispObject d) => Invoke4(a, b, c, d);
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    public LispObject InvokeOnce5(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e) => Invoke5(a, b, c, d, e);
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    public LispObject InvokeOnce6(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f) => Invoke6(a, b, c, d, e, f);
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    public LispObject InvokeOnce7(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f, LispObject g) => Invoke7(a, b, c, d, e, f, g);
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    public LispObject InvokeOnce8(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, LispObject f, LispObject g, LispObject h) => Invoke8(a, b, c, d, e, f, g, h);

    // Native fixnum invoke: long args avoid boxing, LispObject return is body result.
    //
    // The null arms matter as soon as a call site names a function other than the
    // one it sits in. A call site compiles against what the callee looked like
    // then; the callee is fetched from its symbol on every call (LOAD-SYM-FN
    // caches the Symbol, not the function), so by the time the call runs the
    // function may have been redefined into one with no native entry at all --
    // interpreted, or with arguments that are not declared fixnums. Boxing the
    // arguments and going through the ordinary entry gives the same answer, only
    // slower, which is the property that lets a call site assume a native entry
    // without the assumption being load-bearing for correctness.
    public LispObject InvokeNative1(long a)
    {
        if (_nativeArity != 1) return Invoke1(Fixnum.Make(a));
        if (RetainUnit == null) return Unsafe.As<Func<LispFunction, long, LispObject>>(_nativeDel!)(this, a);
        var r = Unsafe.As<Func<LispFunction, long, LispObject>>(_nativeDel!)(this, a);
        GC.KeepAlive(this);
        return r;
    }
    public LispObject InvokeNative2(long a, long b)
    {
        if (_nativeArity != 2) return Invoke2(Fixnum.Make(a), Fixnum.Make(b));
        if (RetainUnit == null) return Unsafe.As<Func<LispFunction, long, long, LispObject>>(_nativeDel!)(this, a, b);
        var r = Unsafe.As<Func<LispFunction, long, long, LispObject>>(_nativeDel!)(this, a, b);
        GC.KeepAlive(this);
        return r;
    }
    public LispObject InvokeNative3(long a, long b, long c)
    {
        if (_nativeArity != 3) return Invoke3(Fixnum.Make(a), Fixnum.Make(b), Fixnum.Make(c));
        if (RetainUnit == null) return Unsafe.As<Func<LispFunction, long, long, long, LispObject>>(_nativeDel!)(this, a, b, c);
        var r = Unsafe.As<Func<LispFunction, long, long, long, LispObject>>(_nativeDel!)(this, a, b, c);
        GC.KeepAlive(this);
        return r;
    }
    public LispObject InvokeNative4(long a, long b, long c, long d)
    {
        if (_nativeArity != 4) return Invoke4(Fixnum.Make(a), Fixnum.Make(b), Fixnum.Make(c), Fixnum.Make(d));
        if (RetainUnit == null) return Unsafe.As<Func<LispFunction, long, long, long, long, LispObject>>(_nativeDel!)(this, a, b, c, d);
        var r = Unsafe.As<Func<LispFunction, long, long, long, long, LispObject>>(_nativeDel!)(this, a, b, c, d);
        GC.KeepAlive(this);
        return r;
    }

    // Raw-return native invoke: long args in, long out. The callee-side entry
    // that would avoid boxing the result does not exist yet, so these go through
    // the argument-side native entry and turn the result back into a long: a
    // redefined callee may return something that is not a fixnum at all, and that
    // has to reach the caller as a Lisp TYPE-ERROR naming the value, not as an
    // InvalidCastException from the middle of the call sequence.
    public long InvokeNativeRet1(long a) => AsRawFixnum(InvokeNative1(a));
    public long InvokeNativeRet2(long a, long b) => AsRawFixnum(InvokeNative2(a, b));
    public long InvokeNativeRet3(long a, long b, long c) => AsRawFixnum(InvokeNative3(a, b, c));
    public long InvokeNativeRet4(long a, long b, long c, long d) =>
        AsRawFixnum(InvokeNative4(a, b, c, d));

    private long AsRawFixnum(LispObject v) =>
        v is Fixnum f
            ? f.Value
            : throw new LispErrorException(new LispTypeError(
                  $"{StatsName ?? Name ?? "anonymous function"}: declared to return a fixnum, returned {v}",
                  v, Startup.Sym("FIXNUM")));

    // Install a native long->LispObject delegate for the appropriate arity.
    public void SetNativeDelegate(Delegate del)
    {
        // Clear the tag first so no reader pairs a new delegate with an old arity.
        _nativeArity = 0;
        _nativeDel = del;
        switch (del)
        {
            case Func<LispFunction, long, LispObject>: Volatile.Write(ref _nativeArity, 1); break;
            case Func<LispFunction, long, long, LispObject>: Volatile.Write(ref _nativeArity, 2); break;
            case Func<LispFunction, long, long, long, LispObject>: Volatile.Write(ref _nativeArity, 3); break;
            case Func<LispFunction, long, long, long, long, LispObject>: Volatile.Write(ref _nativeArity, 4); break;
            default: throw new ArgumentException($"SetNativeDelegate: unsupported type {del.GetType().Name}");
        }
    }

    // Typed entry (a function whose FTYPE gives every argument and the result a
    // numeric type): Func<LispFunction, T1..Tn, R> with each T and R long or
    // double. It shares the native slot: a function has one body, so it has
    // either a native entry or a typed one, never both. The tag is the
    // signature code (TypedEntrySig.Code), which has bit 0x100 set and so never
    // equals a native arity: InvokeNativeN on a typed function takes its boxed
    // fallback.
    public void SetTypedEntry(Delegate del, int sigCode)
    {
        if ((sigCode & 0x100) == 0)
            throw new ArgumentException($"SetTypedEntry: bad signature code {sigCode}");
        _nativeArity = 0;
        _nativeDel = del;
        Volatile.Write(ref _nativeArity, sigCode);
    }

    /// <summary>The signature code of this function's typed entry, or 0.</summary>
    public int TypedEntryCode => (_nativeArity & 0x100) != 0 ? _nativeArity : 0;

    /// <summary>This function's typed entry when its signature code is CODE, else null.</summary>
    public Delegate? TypedEntryFor(int code) => _nativeArity == code ? _nativeDel : null;

    // Install a typed direct-call delegate for the appropriate arity.
    // Public so FASL-emitted code (in a separate assembly) can bypass the
    // internal field visibility without extra reflection hops.
    public void SetDirectDelegate(Delegate del)
    {
        // A new entry has not been marked as taking a value mode: the caller
        // marks it after installing it (MarkMvModeEntry).
        switch (del)
        {
            case Func<LispObject> f0: _func0 = f0; _mvModeMask &= ~1; break;
            case Func<LispObject, LispObject> f1: _func1 = f1; _mvModeMask &= ~2; break;
            case Func<LispObject, LispObject, LispObject> f2: _func2 = f2; _mvModeMask &= ~4; break;
            case Func<LispObject, LispObject, LispObject, LispObject> f3: _func3 = f3; _mvModeMask &= ~8; break;
            case Func<LispObject, LispObject, LispObject, LispObject, LispObject> f4: _func4 = f4; _mvModeMask &= ~16; break;
            case Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> f5: _func5 = f5; _mvModeMask &= ~32; break;
            case Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> f6: _func6 = f6; _mvModeMask &= ~64; break;
            case Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> f7: _func7 = f7; _mvModeMask &= ~128; break;
            case Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject> f8: _func8 = f8; _mvModeMask &= ~256; break;
            default:
                throw new ArgumentException($"SetDirectDelegate: unsupported delegate type {del.GetType().Name}");
        }
    }

    // --- InvokeSlow call statistics (opt-in diagnostic) ---
    // Counts InvokeSlow entries per (callee name, argc) so the fast-path gap
    // (functions still going through the args-array _func) can be measured.
    // Off by default: the only cost on the hot path is a single branch.
    // Lisp API: dotcl:collect-invoke-stats / dotcl:invoke-slow-stats /
    // dotcl:reset-invoke-slow-stats (registered in Startup.cs).
    internal static bool CollectInvokeStats;
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<(string Name, int Argc), long>
        s_invokeSlowStats = new();

    internal static void ResetInvokeSlowStats() => s_invokeSlowStats.Clear();

    /// <summary>Snapshot of the InvokeSlow counters, sorted by count descending.</summary>
    internal static List<KeyValuePair<(string Name, int Argc), long>> InvokeSlowStatsSnapshot()
    {
        var list = new List<KeyValuePair<(string Name, int Argc), long>>(s_invokeSlowStats.Count);
        foreach (var kv in s_invokeSlowStats) list.Add(kv);
        list.Sort((a, b) => b.Value.CompareTo(a.Value));
        return list;
    }

    private unsafe LispObject InvokeSlow(LispObject[] args)
    {
        if (CollectInvokeStats)
            s_invokeSlowStats.AddOrUpdate((Name ?? StatsName ?? AnonOriginTag(), args.Length), 1,
                                          static (_, c) => c + 1);
        PeriodicStackCheck();
        var f = _func;
        var frameName = _frameName;
        if (frameName == null || _selfFramed)
        {
            var saved = s_top;
            try
            {
                var r = f != null ? f(args) : CallDirectWithArray(args);
                GC.KeepAlive(this);
                return r;
            }
            finally { s_top = saved; }
        }
        return f != null ? CallFramed(f, frameName, args) : CallDirectFramed(frameName, args);
    }

    // Origin tag for anonymous functions in the InvokeSlow statistics. The
    // backing method's name identifies the generation site: DynamicMethod
    // names are chosen per emitter site ("lambda", "lambda_direct",
    // "lambda_closure", "lambda_closure_direct", FASL "closure_N", ...), and
    // C# lambdas carry a compiler-generated name embedding the enclosing
    // method (e.g. "<MakeHandlerCaseFunction>b__1_0"). Digits are stripped so
    // per-instance names (closure_42) collapse into one statistics key.
    // Only called with CollectInvokeStats enabled: no cost otherwise.
    private string AnonOriginTag()
    {
        var m = (_closureFunc ?? (Delegate?)_func ?? _directDel)?.Method;
        if (m == null) return "<anon>";
        var sb = new System.Text.StringBuilder("<anon:");
        foreach (var ch in m.Name)
            if (!char.IsDigit(ch)) sb.Append(ch);
        return sb.Append('>').ToString();
    }

    public (Delegate Delegate, string Label) GetJitDelegate()
    {
        if ((_nativeArity & 0x100) != 0)
            return (_nativeDel!, "typed-" + DotCL.Emitter.TypedEntrySig.FromCode(_nativeArity));
        if (_nativeArity != 0) return (_nativeDel!, "native-" + _nativeArity);
        if (_func1 != null) return (_func1, "func-1");
        if (_func2 != null) return (_func2, "func-2");
        if (_func3 != null) return (_func3, "func-3");
        if (_func4 != null) return (_func4, "func-4");
        if (_func5 != null) return (_func5, "func-5");
        if (_func6 != null) return (_func6, "func-6");
        if (_func7 != null) return (_func7, "func-7");
        if (_func8 != null) return (_func8, "func-8");
        if (_func != null) return (_func, "func");
        if (_directDel != null) return (_directDel, "closure-direct");
        return (RawFunction, "func");
    }

    public override string ToString() =>
        Name != null ? $"#<FUNCTION {Name}>" : "#<FUNCTION>";
}
