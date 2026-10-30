using System;
using System.Runtime.CompilerServices;

#pragma warning disable CS8500

namespace DotCL;

/// <summary>
/// The entry points the code compiled into a dotnet:library .dll calls in place
/// of the runtime's own calling convention (function frames, calls through a
/// function object, value modes, symbol and function-cell lookup, argument
/// checks, function objects and their entries, literal data). The compiler's
/// internals change from release to release; these names and signatures are the
/// ones a library depends on, and are kept.
/// </summary>
public static class LibraryAbi
{
    /// <summary>The debugger frame of a function body. Opaque to the library.</summary>
    public struct Frame
    {
        internal LispFunction.Frame F;
    }

    // --- Frames ---

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Enter(ref Frame frame, string name) => LispFunction.Enter0(ref frame.F, name);

    /// <summary>Enter, for a body whose function SELF may keep its compiled code alive.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Enter(ref Frame frame, string name, LispFunction self) => LispFunction.Enter0U(ref frame.F, name, self);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void EnterUnit(ref Frame frame, LispFunction self) => LispFunction.EnterUnit(ref frame.F, self);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Leave(ref Frame frame) => LispFunction.Leave(ref frame.F);

    /// <summary>At the start of a handler in a body that has a frame.</summary>
    public static void Resume(ref Frame frame) => LispFunction.Resume(ref frame.F);

    /// <summary>At the start of a handler in a body without a frame; MARKER is a
    /// local of the handler's method.</summary>
    public static void ResumeBelow(ref byte marker) => LispFunction.ResumeBelow(ref marker);

    public static void UnlinkIfTop(ref Frame frame) => LispFunction.UnlinkIfTop(ref frame.F);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Poll() => ConditionSystem.PollInterrupt();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool PollPending() => ConditionSystem.PollPending();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool PollSavesFloats() => ConditionSystem.PollSavesFloats();

    public static void PollSaved() => ConditionSystem.PollInterruptSaved();

    public static double[] PollSave1(int n, double a) => ConditionSystem.PollSave1(n, a);

    public static double[] PollSave2(int n, double a, double b) => ConditionSystem.PollSave2(n, a, b);

    public static double[] PollSave3(int n, double a, double b, double c) => ConditionSystem.PollSave3(n, a, b, c);

    public static double[] PollSave4(int n, double a, double b, double c, double d)
        => ConditionSystem.PollSave4(n, a, b, c, d);

    public static void PollSaveD(double[] buffer, int i, double v) => ConditionSystem.PollSaveD(buffer, i, v);

    public static double PollLoadD(double[] buffer, int i) => ConditionSystem.PollLoadD(buffer, i);

    // --- Calls. MODE is the value mode the caller asks for (0: none). ---

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject Call(LispFunction fn, int mode)
        => mode == 0 ? LispFunction.Call0(fn) : LispFunction.Call0M(fn, mode);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject Call(LispFunction fn, LispObject a, int mode)
        => mode == 0 ? LispFunction.Call1(fn, a) : LispFunction.Call1M(fn, a, mode);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject Call(LispFunction fn, LispObject a, LispObject b, int mode)
        => mode == 0 ? LispFunction.Call2(fn, a, b) : LispFunction.Call2M(fn, a, b, mode);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject Call(LispFunction fn, LispObject a, LispObject b, LispObject c, int mode)
        => mode == 0 ? LispFunction.Call3(fn, a, b, c) : LispFunction.Call3M(fn, a, b, c, mode);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject Call(LispFunction fn, LispObject a, LispObject b, LispObject c, LispObject d, int mode)
        => mode == 0 ? LispFunction.Call4(fn, a, b, c, d) : LispFunction.Call4M(fn, a, b, c, d, mode);

    /// <summary>The first N (5 to 8) of A0..A7.</summary>
    public static LispObject Call(LispFunction fn, LispObject? a0, LispObject? a1, LispObject? a2, LispObject? a3,
        LispObject? a4, LispObject? a5, LispObject? a6, LispObject? a7, int n, int mode)
    {
        if (mode != 0)
            switch (n)
            {
                case 5: return fn.Invoke5M(a0!, a1!, a2!, a3!, a4!, mode);
                case 6: return fn.Invoke6M(a0!, a1!, a2!, a3!, a4!, a5!, mode);
            }
        return n switch
        {
            5 => fn.Invoke5(a0!, a1!, a2!, a3!, a4!),
            6 => fn.Invoke6(a0!, a1!, a2!, a3!, a4!, a5!),
            7 => fn.Invoke7(a0!, a1!, a2!, a3!, a4!, a5!, a6!),
            _ => fn.Invoke8(a0!, a1!, a2!, a3!, a4!, a5!, a6!, a7!),
        };
    }

    public static LispObject Call(LispFunction fn, LispObject[] args, int mode)
    {
        if (mode != 0)
            switch (args.Length)
            {
                case 5: return fn.Invoke5M(args[0], args[1], args[2], args[3], args[4], mode);
                case 6: return fn.Invoke6M(args[0], args[1], args[2], args[3], args[4], args[5], mode);
            }
        return args.Length switch
        {
            0 => fn.Invoke0(),
            1 => fn.Invoke1(args[0]),
            2 => fn.Invoke2(args[0], args[1]),
            3 => fn.Invoke3(args[0], args[1], args[2]),
            4 => fn.Invoke4(args[0], args[1], args[2], args[3]),
            5 => fn.Invoke5(args[0], args[1], args[2], args[3], args[4]),
            6 => fn.Invoke6(args[0], args[1], args[2], args[3], args[4], args[5]),
            7 => fn.Invoke7(args[0], args[1], args[2], args[3], args[4], args[5], args[6]),
            8 => fn.Invoke8(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]),
            _ => fn.Invoke(args),
        };
    }

    // --- Tail calls. N is the argument count, MODE the value mode (-1: none). ---

    /// <summary>How to make the tail call: 0 through Call, 1 through TailDirect,
    /// 2 through TailClosure. SELF: the frame name the caller passes on, or null.</summary>
    public static int TailPrep(LispFunction fn, int n, int mode, string? self,
        LispObject? a0, LispObject? a1, LispObject? a2, LispObject? a3,
        LispObject? a4, LispObject? a5, LispObject? a6, LispObject? a7) => n switch
    {
        0 => LispFunction.TailPrep0(fn, mode, self),
        1 => LispFunction.TailPrep1(fn, mode, self, a0!),
        2 => LispFunction.TailPrep2(fn, mode, self, a0!, a1!),
        3 => LispFunction.TailPrep3(fn, mode, self, a0!, a1!, a2!),
        4 => LispFunction.TailPrep4(fn, mode, self, a0!, a1!, a2!, a3!),
        _ => LispFunction.TailPrep8(fn, n, mode, self, a0, a1, a2, a3, a4, a5, a6, a7),
    };

    /// <summary>The body of FN taking N arguments, as a Func of N LispObjects.</summary>
    public static T TailDirect<T>(LispFunction fn, int n) where T : class => Unsafe.As<T>(n switch
    {
        0 => fn.TailDirect0, 1 => fn.TailDirect1, 2 => fn.TailDirect2, 3 => fn.TailDirect3,
        4 => fn.TailDirect4, 5 => fn.TailDirect5, 6 => fn.TailDirect6, 7 => fn.TailDirect7,
        _ => (object)fn.TailDirect8,
    });

    /// <summary>The closure body of FN taking N arguments, as a Func of the
    /// environment and N LispObjects.</summary>
    public static T TailClosure<T>(LispFunction fn, int n) where T : class => Unsafe.As<T>(n switch
    {
        0 => fn.TailClosure0, 1 => fn.TailClosure1, 2 => fn.TailClosure2, 3 => fn.TailClosure3,
        4 => fn.TailClosure4, 5 => fn.TailClosure5, _ => (object)fn.TailClosure6,
    });

    public static object[]? Environment(LispFunction fn) => fn.Environment;

    /// <summary>From a body with a frame: true when FN is entered through TailDirect.</summary>
    public static bool SelfTailEntry(ref Frame frame, LispFunction fn, int n, int mode)
        => LispFunction.SelfTailEntry(ref frame.F, fn, n, mode);

    /// <summary>From a body with a frame, when SelfTailEntry is false: Lend (call
    /// FN, then Leave), or the SELF to pass to TailPrep.</summary>
    public static string? SelfTailPrepare(ref Frame frame, LispFunction fn, string name)
        => LispFunction.SelfTailPrepare(ref frame.F, fn, name);

    public static string Lend => LispFunction.TailLendMarker;

    // --- Multiple values ---

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int TakeMode() => MultipleValues.TakeMode();

    public static void PassMode(int mode) => MultipleValues.PassMode(mode);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void ResetValues() => MultipleValues.Reset();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void ResetValuesOnExit() => MultipleValues.ResetOnExit();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject Primary(LispObject value) => Runtime.UnwrapMv(value);

    // --- Symbols and function cells ---

    /// <summary>A cache cell for the symbol NAME names; SiteSymbol reads it.</summary>
    public static object NameSite(string name) => new Startup.SymSite(name);

    /// <summary>A cache cell for the function name NAME (in PKG when given).</summary>
    public static object SymbolSite(string name, string? pkg) => new Startup.SymFnSite(name, pkg);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Symbol SiteSymbol(object site)
        => site is Startup.SymFnSite f ? f.Resolve() : Unsafe.As<Startup.SymSite>(site).Resolve();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispFunction FunctionOf(Symbol sym) => Emitter.CilAssembler.GetFunctionBySymbol(sym);

    public static LispFunction SetfFunctionOf(Symbol sym) => Emitter.CilAssembler.GetSetfFunctionBySymbol(sym);

    /// <summary>PACKAGE: the package of the definition, or null.</summary>
    public static void DefineFunction(Symbol sym, LispFunction fn, string? package)
    {
        if (package == null) Emitter.CilAssembler.RegisterFunctionOnSymbol(sym, fn);
        else Emitter.CilAssembler.RegisterFunctionOnSymbolGuarded(sym, fn, package);
    }

    // --- Function objects ---

    public static LispFunction Function(Func<LispObject[], LispObject> body, string name, int arity)
        => new LispFunction(body, name, arity);

    public static LispFunction Closure(Func<object[], LispObject[], LispObject> body, object[] env, string name, int arity)
        => new LispFunction(body, env, name, arity);

    public const int EntryDirect = 0, EntryNative = 1, EntrySelfFramed = 2, EntryArity = 3, EntryLeaf = 4, EntryMode = 5;

    /// <summary>Give FN an entry: WHAT is one of the Entry constants, with the
    /// delegate (EntryDirect, EntryNative), the address (EntrySelfFramed,
    /// EntryArity) and the arity (all but the delegates) it takes.</summary>
    public static void SetEntry(LispFunction fn, int what, Delegate? del, IntPtr entry, int arity)
    {
        switch (what)
        {
            case EntryDirect: fn.SetDirectDelegate(del!); break;
            case EntryNative: fn.SetNativeDelegate(del!); break;
            case EntrySelfFramed: fn.SetSelfFramedEntry(entry, arity); break;
            case EntryArity: fn.SetArityEntry(entry, arity); break;
            case EntryLeaf: fn.MarkLeafEntry(arity); break;
            case EntryMode: fn.MarkMvModeEntry(arity); break;
            default: throw new ArgumentOutOfRangeException(nameof(what));
        }
    }

    public static LispFunction DirectClosure(Delegate body, object[] env, string name)
        => LispFunction.MakeDirectClosure(body, env, name);

    /// <summary>Give the closure FN, just made, an entry (EntryArity) or mark the
    /// entry of ARITY as taking a value mode (EntryMode); returns FN.</summary>
    public static LispObject WithEntry(LispObject fn, int what, IntPtr entry, int arity)
        => what == EntryMode ? LispFunction.WithMvModeEntry(fn, arity) : LispFunction.WithClosureEntry(fn, entry, arity);

    /// <summary>The check a closure body with an entry makes on the way in.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void ClosureCheck() => LispFunction.ClosureEntryCheck();

    public static void SetLambdaList(LispFunction fn, string text) => Runtime.SetLambdaListSource(fn, text);

    // --- Arguments passed as an array ---

    public static void CheckArity(string name, LispObject[] args, int n) => Runtime.CheckArityExact(name, args, n);

    public static void CheckArityMin(string name, LispObject[] args, int min) => Runtime.CheckArityMin(name, args, min);

    public static void CheckKeys(string name, LispObject[] args, int keyStart, string[] validKeys)
        => Runtime.CheckNoUnknownKeys(name, args, keyStart, validKeys);

    public static LispObject RestArgs(LispObject[] args, int start) => Runtime.CollectRestArgs(args, start);

    public static LispObject? KeyArg(LispObject[] args, int start, string key) => Runtime.FindKeyArg(args, start, key);

    public static LispObject Absent => AbsentKey.Instance;

    public static bool IsAbsent(LispObject o) => AbsentKey.Is(o);

    // --- Literal data ---

    public static void Preintern(byte[] data) => FaslData.Preintern(data);

    public static byte[][] SplitData(byte[] all) => FaslData.SplitParts(all);

    public static byte[][] SplitData(byte[][] pieces) => FaslData.SplitParts(pieces);

    public static LispObject ReadData(byte[][] parts, int index, LispObject?[] cache, Symbol[]? symbols, LispObject?[]? shared)
        => FaslData.ReadValue(parts, index, cache, symbols, shared);

    public static LispObject ShareData(LispObject v, LispObject?[] slots, int slot) => FaslData.Share(v, slots, slot);

    // --- Non-local exits and handlers ---

    /// <summary>One clause of a HANDLER-CASE or HANDLER-BIND. A cluster is an
    /// array of these.</summary>
    public sealed class Binding : HandlerBinding
    {
        public Binding(LispObject typeSpec, LispFunction handler) : base(typeSpec, handler) { }
        public Binding(LispObject typeSpec, int clause) : base(typeSpec, clause) { }
    }

    /// <summary>A restart of a RESTART-CASE.</summary>
    public sealed class Restart : LispRestart
    {
        public Restart(string name, Func<LispObject[], LispObject> handler, string? description, object tag, bool isBind)
            : base(name, handler, description, tag, isBind) { }
    }

    /// <summary>The values of a form, held across code that publishes values of its own.</summary>
    public struct Held
    {
        internal MvHeld H;
    }

    public static void PushCatch(LispObject tag) => CatchTagStack.Push(tag);

    public static int PushCatchIndex(LispObject tag) => CatchTagStack.PushIndex(tag);

    public static void PopCatch() => CatchTagStack.Pop();

    public static object? EnterCatch(LispObject tag)
    {
        CatchTagStack.Push(tag);
        return null;
    }

    public static void LeaveCatch(object? state) => CatchTagStack.Pop();

    public static bool HasCatch(LispObject tag) => CatchTagStack.HasMatchingCatch(tag);

    /// <summary>The exception a THROW of VALUE to TAG raises.</summary>
    public static Exception MakeThrow(LispObject tag, LispObject value) => Runtime.MakeCatchThrow(tag, value);

    /// <summary>Exception filter of CATCH: 1 when EX is a THROW to TAG.</summary>
    public static int CatchMatches(object ex, LispObject tag) => ControlFlowFilters.CatchTagMatches(ex, tag);

    public static LispObject ThrownValue(object ex) => ((CatchThrowException)ex).Value;

    public static LispObject ThrownTag(object ex) => ((CatchThrowException)ex).Tag;

    public static void PushHandlers(Binding[] cluster) => HandlerClusterStack.PushCluster(cluster);

    public static void PopHandlers() => HandlerClusterStack.PopCluster();

    /// <summary>Exception filter of HANDLER-CASE: the index of the clause of
    /// CLUSTER that handles EX, or -1.</summary>
    public static int HandlerCaseClause(object ex, Binding[] cluster) => ControlFlowFilters.HandlerCaseClause(ex, cluster);

    public static LispObject HandlerCaseCondition(object ex) => ControlFlowFilters.HandlerCaseCondition(ex);

    /// <summary>What an exception filter matches on; see ControlFlowFilters.Transfer.</summary>
    public static object Transfer(object ex) => ControlFlowFilters.Transfer(ex);

    public static object TakeTransfer(object ex) => ControlFlowFilters.TakeTransfer(ex);

    // A library never takes the shortcut that returns a THROW through the
    // function that was called from its CATCH: a THROW is always an exception.
    public static int ClaimThrow(LispFunction self) => -1;

    public static bool CanReturnThrow(LispObject tag, int claimed) => false;

    public static LispObject ReturnThrow(LispObject value) => ThrowReturn.Return(value);

    public static object? ClaimThrowS(LispFunction self) => null;

    public static int ClaimedThrowIndex(object? state) => -1;

    public static bool CanReturnThrowS(LispObject tag, int claimed, object? state) => false;

    public static LispObject ReturnThrowS(LispObject value, object? state) => ThrowReturn.Return(value);

    public static bool IsThrowReturned(LispObject r) => ThrowReturn.IsReturned(r);

    public static LispObject TakeThrown() => ThrowReturn.TakePending();

    // --- Multiple values ---

    public static Held Hold(LispObject primary) => new Held { H = MultipleValues.HoldValues(primary) };

    public static LispObject Restore(Held held, LispObject primary, int mode) => MultipleValues.RestoreValues(held.H, primary, mode);

    public static LispObject Values0() => MultipleValues.Values0();

    public static LispObject PrimaryValue(LispObject v) => MultipleValues.Primary(v);

    public static LispObject PrimaryAlone(LispObject v) => MultipleValues.PrimaryAlone(v);

    public static LispObject Capture(LispObject primary) => MultipleValues.CaptureForBind(primary);

    public static LispObject CapturePair(LispObject primary) => MultipleValues.CapturePair(primary);

    public static LispObject PairNth(LispObject primary, int n) => MultipleValues.PairNth(primary, n);

    public static LispObject Nth(int i) => MultipleValues.BindNthI(i);

    public static LispObject Nth(LispObject i) => MultipleValues.BindNth(i);

    public static LispObject CallBound(LispObject fn, int mode) => MultipleValues.CallBound(fn, mode);

    public static LispObject Values(LispObject a, LispObject b, int mode) => MultipleValues.Values2Mode(a, b, mode);

    public static LispObject Values(LispObject a, LispObject b, LispObject c, int mode)
        => MultipleValues.Values3Mode(a, b, c, mode);

    public static LispObject Values(LispObject a, LispObject b, LispObject c, LispObject d, int mode)
        => MultipleValues.Values4Mode(a, b, c, d, mode);

    public static LispObject Values(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e, int mode)
        => MultipleValues.Values5Mode(a, b, c, d, e, mode);

    public static LispObject Values(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e,
                                    LispObject f, int mode)
        => MultipleValues.Values6Mode(a, b, c, d, e, f, mode);

    public static LispObject Values(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e,
                                    LispObject f, LispObject g, int mode)
        => MultipleValues.Values7Mode(a, b, c, d, e, f, g, mode);

    public static LispObject Values(LispObject a, LispObject b, LispObject c, LispObject d, LispObject e,
                                    LispObject f, LispObject g, LispObject h, int mode)
        => MultipleValues.Values8Mode(a, b, c, d, e, f, g, h, mode);

    public static LispObject Values(LispObject[] values, int mode) => MultipleValues.ValuesArrayMode(values, mode);

    public static int SavedCount() => MultipleValues.SaveCount();

    public static LispObject[]? SavedValues() => MultipleValues.SaveValues();

    public static void RestoreSaved(int count, LispObject[]? values) => MultipleValues.RestoreSaved(count, values);

    public static void SetRestartName(Restart r, Symbol name) => r.NameSymbol = name;

    public static void SetRestartReport(Restart r, LispObject f) => r.ReportFunction = f;

    public static void SetRestartInteractive(Restart r, LispObject f) => r.InteractiveFunction = f;

    public static void SetRestartTest(Restart r, LispObject f) => r.TestFunction = f;

    public static void PushRestarts(Restart[] cluster) => RestartClusterStack.PushCluster(cluster);

    public static void PopRestarts() => RestartClusterStack.PopCluster();

    /// <summary>Exception filter of RESTART-CASE: the index of the tag of TAGS that
    /// EX invokes, or -1.</summary>
    public static int RestartMatches(object ex, object[] tags) => ControlFlowFilters.RestartCaseTag(ex, tags);

    public static LispObject[] RestartArguments(object ex) => ((RestartInvocationException)ex).Arguments;
}
