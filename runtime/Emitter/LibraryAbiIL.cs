using System.Reflection;
using System.Reflection.Emit;

namespace DotCL.Emitter;

internal static class LibraryAbiILExtensions
{
    /// <summary>While a dotnet:library is compiled, the IL of its methods goes
    /// through LibraryAbiIL; otherwise IL is returned unchanged.</summary>
    internal static ILGenerator ForLibrary(this ILGenerator il)
    {
#if NET9_0_OR_GREATER
        if (LibraryAbiIL.Active && il is not LibraryAbiIL && il.GetType().Name != "DynamicILGenerator")
            return new LibraryAbiIL(il);
#endif
        return il;
    }

    /// <summary>The type of a static field that holds a symbol site.</summary>
    internal static System.Type FieldType(System.Type t)
    {
#if NET9_0_OR_GREATER
        if (LibraryAbiIL.Active && (t == typeof(Startup.SymSite) || t == typeof(Startup.SymFnSite)))
            return typeof(object);
#endif
        return t;
    }
}

#if NET9_0_OR_GREATER
/// <summary>
/// An ILGenerator that writes calls to the runtime's calling convention (frames,
/// calls through a function object, value modes, symbol sites, function cells,
/// argument checks, function objects, literal data) as calls to LibraryAbi, so a
/// library depends on that class instead of on the compiler's internals.
/// </summary>
internal sealed class LibraryAbiIL : ILGenerator
{
    [System.ThreadStatic] private static int t_depth;

    internal static bool Active => t_depth > 0;
    // DOTCL_NO_LIBRARY_ABI=1: compile a library as any other code, for comparison.
    private static readonly bool s_off = System.Environment.GetEnvironmentVariable("DOTCL_NO_LIBRARY_ABI") == "1";
    internal static void Begin() { if (!s_off) t_depth++; }
    internal static void End() { if (!s_off) t_depth--; }

    private readonly ILGenerator _in;
    private bool _tail;

    internal LibraryAbiIL(ILGenerator inner) { _in = inner; }

    private enum Kind { Plain, PushNull, DropArgs, CallFixed, CallFixedMode, CallWide, CallWideMode, CallArray, Entry, WithEntry, DropCall, DropCallMode }

    private readonly record struct Redirect(Kind Kind, MethodInfo Target, int N = 0);

    // Built on first use: Active is read by every assembler, but the tables are
    // only needed while a library-ABI module is being written.
    private static Dictionary<MethodBase, Redirect>? _lazy_map;
    private static Dictionary<MethodBase, Redirect> s_map => _lazy_map ??= BuildMap();
    private static MethodInfo? _lazy_absent;
    private static MethodInfo s_absent => _lazy_absent ??= typeof(LibraryAbi).GetProperty(nameof(LibraryAbi.Absent))!.GetGetMethod()!;
    private static FieldInfo? _lazy_absentField;
    private static FieldInfo s_absentField => _lazy_absentField ??= typeof(AbsentKey).GetField(nameof(AbsentKey.Instance))!;

    private static MethodInfo Abi(string name, params Type[] args) =>
        typeof(LibraryAbi).GetMethod(name, args)
        ?? throw new System.InvalidOperationException("LibraryAbi." + name);

    private static Dictionary<MethodBase, Redirect> BuildMap()
    {
        var m = new Dictionary<MethodBase, Redirect>();
        var lf = typeof(LispFunction);
        var lo = typeof(LispObject);
        var frameRef = typeof(LibraryAbi.Frame).MakeByRefType();
        void Plain(MethodBase from, MethodInfo to) => m[from] = new Redirect(Kind.Plain, to);
        MethodInfo M(System.Type t, string name, params System.Type[] args) =>
            t.GetMethod(name, args) ?? throw new System.InvalidOperationException(t.Name + "." + name);

        var enter = Abi(nameof(LibraryAbi.Enter), frameRef, typeof(string));
        for (int n = 0; n <= 4; n++)
            m[lf.GetMethod("Enter" + n)!] = new Redirect(Kind.DropArgs, enter, n);
        var enterU = Abi(nameof(LibraryAbi.Enter), frameRef, typeof(string), lf);
        for (int n = 0; n <= 4; n++)
            m[lf.GetMethod("Enter" + n + "U")!] = new Redirect(Kind.DropArgs, enterU, n);
        Plain(lf.GetMethod("EnterUnit")!, Abi(nameof(LibraryAbi.EnterUnit), frameRef, lf));
        Plain(lf.GetMethod("Leave")!, Abi(nameof(LibraryAbi.Leave), frameRef));
        Plain(lf.GetMethod("Resume")!, Abi(nameof(LibraryAbi.Resume), frameRef));
        Plain(lf.GetMethod("UnlinkIfTop")!, Abi(nameof(LibraryAbi.UnlinkIfTop), frameRef));
        Plain(lf.GetMethod("ResumeBelow")!, Abi(nameof(LibraryAbi.ResumeBelow), typeof(byte).MakeByRefType()));
        Plain(M(typeof(ConditionSystem), "PollInterrupt"), Abi(nameof(LibraryAbi.Poll)));
        Plain(M(typeof(ConditionSystem), "PollPending"), Abi(nameof(LibraryAbi.PollPending)));
        Plain(M(typeof(ConditionSystem), "PollSavesFloats"), Abi(nameof(LibraryAbi.PollSavesFloats)));
        Plain(M(typeof(ConditionSystem), "PollInterruptSaved"), Abi(nameof(LibraryAbi.PollSaved)));
        Plain(M(typeof(ConditionSystem), "PollSave1", typeof(int), typeof(double)), Abi(nameof(LibraryAbi.PollSave1), typeof(int), typeof(double)));
        Plain(M(typeof(ConditionSystem), "PollSave2", typeof(int), typeof(double), typeof(double)), Abi(nameof(LibraryAbi.PollSave2), typeof(int), typeof(double), typeof(double)));
        Plain(M(typeof(ConditionSystem), "PollSave3", typeof(int), typeof(double), typeof(double), typeof(double)), Abi(nameof(LibraryAbi.PollSave3), typeof(int), typeof(double), typeof(double), typeof(double)));
        Plain(M(typeof(ConditionSystem), "PollSave4", typeof(int), typeof(double), typeof(double), typeof(double), typeof(double)), Abi(nameof(LibraryAbi.PollSave4), typeof(int), typeof(double), typeof(double), typeof(double), typeof(double)));
        Plain(M(typeof(ConditionSystem), "PollSaveD", typeof(double[]), typeof(int), typeof(double)),
              Abi(nameof(LibraryAbi.PollSaveD), typeof(double[]), typeof(int), typeof(double)));
        Plain(M(typeof(ConditionSystem), "PollLoadD", typeof(double[]), typeof(int)),
              Abi(nameof(LibraryAbi.PollLoadD), typeof(double[]), typeof(int)));

        for (int n = 0; n <= 8; n++)
        {
            var args = Enumerable.Repeat(lo, n).ToArray();
            var withMode = args.Append(typeof(int)).ToArray();
            if (n <= 4)
            {
                var call = Abi(nameof(LibraryAbi.Call), new[] { lf }.Concat(withMode).ToArray());
                foreach (var name in new[] { "Call" + n })
                    m[M(lf, name, new[] { lf }.Concat(args).ToArray())] = new Redirect(Kind.CallFixed, call);
                m[M(lf, "Call" + n + "M", new[] { lf }.Concat(withMode).ToArray())] = new Redirect(Kind.CallFixedMode, call);
                foreach (var name in new[] { "Invoke", "InvokeFast", "InvokeOnce" })
                    m[M(lf, name + n, args)] = new Redirect(Kind.CallFixed, call);
                foreach (var name in new[] { "Invoke", "InvokeFast" })
                    m[M(lf, name + n + "M", withMode)] = new Redirect(Kind.CallFixedMode, call);
            }
            else
            {
                var call = Abi(nameof(LibraryAbi.Call), new[] { lf }.Concat(Enumerable.Repeat(lo, 8))
                    .Append(typeof(int)).Append(typeof(int)).ToArray());
                foreach (var name in new[] { "Invoke", "InvokeOnce" })
                    m[M(lf, name + n, args)] = new Redirect(Kind.CallWide, call, n);
                if (lf.GetMethod("Invoke" + n + "M", withMode) is { } im)
                    m[im] = new Redirect(Kind.CallWideMode, call, n);
            }
        }
        m[M(lf, "Invoke", typeof(LispObject[]))] =
            new Redirect(Kind.CallArray, Abi(nameof(LibraryAbi.Call), lf, typeof(LispObject[]), typeof(int)));

        Plain(M(typeof(MultipleValues), "TakeMode"), Abi(nameof(LibraryAbi.TakeMode)));
        Plain(M(typeof(MultipleValues), "PassMode", typeof(int)), Abi(nameof(LibraryAbi.PassMode), typeof(int)));
        Plain(M(typeof(MultipleValues), "Reset"), Abi(nameof(LibraryAbi.ResetValues)));
        Plain(M(typeof(MultipleValues), "ResetOnExit"), Abi(nameof(LibraryAbi.ResetValuesOnExit)));
        Plain(M(typeof(Runtime), "UnwrapMv", lo), Abi(nameof(LibraryAbi.Primary), lo));

        // Non-local exits, handlers, and multiple values held or bound.
        var mv = typeof(MultipleValues);
        var binding = typeof(LibraryAbi.Binding);
        var held = typeof(LibraryAbi.Held);
        Plain(M(typeof(CatchTagStack), "Push", lo), Abi(nameof(LibraryAbi.PushCatch), lo));
        Plain(M(typeof(CatchTagStack), "PushIndex", lo), Abi(nameof(LibraryAbi.PushCatchIndex), lo));
        Plain(M(typeof(CatchTagStack), "Pop"), Abi(nameof(LibraryAbi.PopCatch)));
        Plain(M(typeof(CatchTagStack), "Enter", lo), Abi(nameof(LibraryAbi.EnterCatch), lo));
        Plain(M(typeof(CatchTagStack), "Leave", typeof(object)), Abi(nameof(LibraryAbi.LeaveCatch), typeof(object)));
        Plain(M(typeof(CatchTagStack), "HasMatchingCatch", lo), Abi(nameof(LibraryAbi.HasCatch), lo));
        Plain(M(typeof(Runtime), "MakeCatchThrow", lo, lo), Abi(nameof(LibraryAbi.MakeThrow), lo, lo));
        Plain(M(typeof(ControlFlowFilters), "CatchTagMatches", typeof(object), lo), Abi(nameof(LibraryAbi.CatchMatches), typeof(object), lo));
        Plain(typeof(CatchThrowException).GetProperty("Value")!.GetGetMethod()!, Abi(nameof(LibraryAbi.ThrownValue), typeof(object)));
        Plain(typeof(CatchThrowException).GetProperty("Tag")!.GetGetMethod()!, Abi(nameof(LibraryAbi.ThrownTag), typeof(object)));
        Plain(M(typeof(HandlerClusterStack), "PushCluster", typeof(HandlerBinding[])), Abi(nameof(LibraryAbi.PushHandlers), binding.MakeArrayType()));
        var restart = typeof(LibraryAbi.Restart);
        Plain(typeof(LispRestart).GetProperty("NameSymbol")!.GetSetMethod()!, Abi(nameof(LibraryAbi.SetRestartName), restart, typeof(Symbol)));
        Plain(typeof(LispRestart).GetProperty("ReportFunction")!.GetSetMethod()!, Abi(nameof(LibraryAbi.SetRestartReport), restart, lo));
        Plain(typeof(LispRestart).GetProperty("InteractiveFunction")!.GetSetMethod()!, Abi(nameof(LibraryAbi.SetRestartInteractive), restart, lo));
        Plain(typeof(LispRestart).GetProperty("TestFunction")!.GetSetMethod()!, Abi(nameof(LibraryAbi.SetRestartTest), restart, lo));
        Plain(M(typeof(RestartClusterStack), "PushCluster", typeof(LispRestart[])), Abi(nameof(LibraryAbi.PushRestarts), restart.MakeArrayType()));
        Plain(M(typeof(RestartClusterStack), "PopCluster"), Abi(nameof(LibraryAbi.PopRestarts)));
        Plain(M(typeof(ControlFlowFilters), "RestartCaseTag", typeof(object), typeof(object[])), Abi(nameof(LibraryAbi.RestartMatches), typeof(object), typeof(object[])));
        Plain(typeof(RestartInvocationException).GetProperty("Arguments")!.GetGetMethod()!, Abi(nameof(LibraryAbi.RestartArguments), typeof(object)));
        Plain(M(typeof(HandlerClusterStack), "PopCluster"), Abi(nameof(LibraryAbi.PopHandlers)));
        Plain(M(typeof(ControlFlowFilters), "HandlerCaseClause", typeof(object), typeof(HandlerBinding[])),
            Abi(nameof(LibraryAbi.HandlerCaseClause), typeof(object), binding.MakeArrayType()));
        Plain(M(typeof(ControlFlowFilters), "HandlerCaseCondition", typeof(object)), Abi(nameof(LibraryAbi.HandlerCaseCondition), typeof(object)));
        Plain(M(typeof(ControlFlowFilters), "Transfer", typeof(object)), Abi(nameof(LibraryAbi.Transfer), typeof(object)));
        Plain(M(typeof(ControlFlowFilters), "TakeTransfer", typeof(object)), Abi(nameof(LibraryAbi.TakeTransfer), typeof(object)));

        var tr = typeof(ThrowReturn);
        Plain(M(tr, "Claim", lf), Abi(nameof(LibraryAbi.ClaimThrow), lf));
        Plain(M(tr, "CanReturn", lo, typeof(int)), Abi(nameof(LibraryAbi.CanReturnThrow), lo, typeof(int)));
        Plain(M(tr, "Return", lo), Abi(nameof(LibraryAbi.ReturnThrow), lo));
        Plain(M(tr, "ClaimS", lf), Abi(nameof(LibraryAbi.ClaimThrowS), lf));
        Plain(M(tr, "ClaimedIndex", typeof(object)), Abi(nameof(LibraryAbi.ClaimedThrowIndex), typeof(object)));
        Plain(M(tr, "CanReturnS", lo, typeof(int), typeof(object)), Abi(nameof(LibraryAbi.CanReturnThrowS), lo, typeof(int), typeof(object)));
        Plain(M(tr, "ReturnS", lo, typeof(object)), Abi(nameof(LibraryAbi.ReturnThrowS), lo, typeof(object)));
        Plain(M(tr, "IsReturned", lo), Abi(nameof(LibraryAbi.IsThrowReturned), lo));
        Plain(M(tr, "TakePending"), Abi(nameof(LibraryAbi.TakeThrown)));
        for (int n = 0; n <= 4; n++)
        {
            var args = new[] { lf }.Concat(Enumerable.Repeat(lo, n)).ToArray();
            var call = Abi(nameof(LibraryAbi.Call), args.Append(typeof(int)).ToArray());
            m[M(tr, "Forward" + n, args.Append(typeof(int)).ToArray())] = new Redirect(Kind.DropCall, call);
            m[M(tr, "Call" + n, args.Append(typeof(int)).ToArray())] = new Redirect(Kind.DropCall, call);
            m[M(tr, "Call" + n + "S", args.Append(typeof(object)).ToArray())] = new Redirect(Kind.DropCall, call);
            m[M(tr, "Forward" + n + "M", args.Append(typeof(int)).Append(typeof(int)).ToArray())] = new Redirect(Kind.DropCallMode, call);
        }

        Plain(M(mv, "HoldValues", lo), Abi(nameof(LibraryAbi.Hold), lo));
        Plain(M(mv, "RestoreValues", typeof(MvHeld), lo, typeof(int)), Abi(nameof(LibraryAbi.Restore), held, lo, typeof(int)));
        Plain(M(mv, "Values0"), Abi(nameof(LibraryAbi.Values0)));
        Plain(M(mv, "Primary", lo), Abi(nameof(LibraryAbi.PrimaryValue), lo));
        Plain(M(mv, "PrimaryAlone", lo), Abi(nameof(LibraryAbi.PrimaryAlone), lo));
        Plain(M(mv, "CaptureForBind", lo), Abi(nameof(LibraryAbi.Capture), lo));
        Plain(M(mv, "CapturePair", lo), Abi(nameof(LibraryAbi.CapturePair), lo));
        Plain(M(mv, "PairNth", lo, typeof(int)), Abi(nameof(LibraryAbi.PairNth), lo, typeof(int)));
        Plain(M(mv, "BindNthI", typeof(int)), Abi(nameof(LibraryAbi.Nth), typeof(int)));
        Plain(M(mv, "BindNth", lo), Abi(nameof(LibraryAbi.Nth), lo));
        Plain(M(mv, "CallBound", lo, typeof(int)), Abi(nameof(LibraryAbi.CallBound), lo, typeof(int)));
        for (int n = 2; n <= 8; n++)
        {
            var vs = Enumerable.Repeat(lo, n).Append(typeof(int)).ToArray();
            Plain(M(mv, "Values" + n + "Mode", vs), Abi(nameof(LibraryAbi.Values), vs));
        }
        Plain(M(mv, "ValuesArrayMode", typeof(LispObject[]), typeof(int)), Abi(nameof(LibraryAbi.Values), typeof(LispObject[]), typeof(int)));
        Plain(M(mv, "SaveCount"), Abi(nameof(LibraryAbi.SavedCount)));
        Plain(M(mv, "SaveValues"), Abi(nameof(LibraryAbi.SavedValues)));
        Plain(M(mv, "RestoreSaved", typeof(int), typeof(LispObject[])), Abi(nameof(LibraryAbi.RestoreSaved), typeof(int), typeof(LispObject[])));

        var siteSymbol = Abi(nameof(LibraryAbi.SiteSymbol), typeof(object));
        Plain(M(typeof(Startup.SymSite), "Resolve"), siteSymbol);
        Plain(M(typeof(Startup.SymFnSite), "Resolve"), siteSymbol);
        Plain(M(typeof(CilAssembler), "GetFunctionBySymbol", typeof(Symbol)), Abi(nameof(LibraryAbi.FunctionOf), typeof(Symbol)));
        Plain(M(typeof(CilAssembler), "GetSetfFunctionBySymbol", typeof(Symbol)), Abi(nameof(LibraryAbi.SetfFunctionOf), typeof(Symbol)));
        m[M(typeof(CilAssembler), "RegisterFunctionOnSymbol", typeof(Symbol), lf)] =
            new Redirect(Kind.PushNull, Abi(nameof(LibraryAbi.DefineFunction), typeof(Symbol), lf, typeof(string)));
        Plain(M(typeof(CilAssembler), "RegisterFunctionOnSymbolGuarded", typeof(Symbol), lf, typeof(string)),
            Abi(nameof(LibraryAbi.DefineFunction), typeof(Symbol), lf, typeof(string)));

        var entry = Abi(nameof(LibraryAbi.SetEntry), lf, typeof(int), typeof(System.Delegate), typeof(System.IntPtr), typeof(int));
        m[M(lf, "SetDirectDelegate", typeof(System.Delegate))] = new Redirect(Kind.Entry, entry, LibraryAbi.EntryDirect);
        m[M(lf, "SetNativeDelegate", typeof(System.Delegate))] = new Redirect(Kind.Entry, entry, LibraryAbi.EntryNative);
        m[M(lf, "SetSelfFramedEntry", typeof(System.IntPtr), typeof(int))] = new Redirect(Kind.Entry, entry, LibraryAbi.EntrySelfFramed);
        m[M(lf, "SetArityEntry", typeof(System.IntPtr), typeof(int))] = new Redirect(Kind.Entry, entry, LibraryAbi.EntryArity);
        m[M(lf, "MarkLeafEntry", typeof(int))] = new Redirect(Kind.Entry, entry, LibraryAbi.EntryLeaf);
        m[M(lf, "MarkMvModeEntry", typeof(int))] = new Redirect(Kind.Entry, entry, LibraryAbi.EntryMode);
        Plain(M(typeof(Runtime), "SetLambdaListSource", lf, typeof(string)), Abi(nameof(LibraryAbi.SetLambdaList), lf, typeof(string)));

        var arr = typeof(LispObject[]);
        Plain(M(typeof(Runtime), "CheckArityExact", typeof(string), arr, typeof(int)), Abi(nameof(LibraryAbi.CheckArity), typeof(string), arr, typeof(int)));
        Plain(M(typeof(Runtime), "CheckArityMin", typeof(string), arr, typeof(int)), Abi(nameof(LibraryAbi.CheckArityMin), typeof(string), arr, typeof(int)));
        Plain(M(typeof(Runtime), "CheckNoUnknownKeys", typeof(string), arr, typeof(int), typeof(string[])),
            Abi(nameof(LibraryAbi.CheckKeys), typeof(string), arr, typeof(int), typeof(string[])));
        Plain(M(typeof(Runtime), "CollectRestArgs", arr, typeof(int)), Abi(nameof(LibraryAbi.RestArgs), arr, typeof(int)));
        Plain(M(typeof(Runtime), "FindKeyArg", arr, typeof(int), typeof(string)), Abi(nameof(LibraryAbi.KeyArg), arr, typeof(int), typeof(string)));
        Plain(M(typeof(AbsentKey), "Is", lo), Abi(nameof(LibraryAbi.IsAbsent), lo));

        Plain(M(typeof(FaslData), "Preintern", typeof(byte[])), Abi(nameof(LibraryAbi.Preintern), typeof(byte[])));
        Plain(M(typeof(FaslData), "SplitParts", typeof(byte[])), Abi(nameof(LibraryAbi.SplitData), typeof(byte[])));
        Plain(M(typeof(FaslData), "SplitParts", typeof(byte[][])), Abi(nameof(LibraryAbi.SplitData), typeof(byte[][])));
        var readArgs = new[] { typeof(byte[][]), typeof(int), typeof(LispObject[]), typeof(Symbol[]), typeof(LispObject[]) };
        Plain(M(typeof(FaslData), "ReadValue", readArgs), Abi(nameof(LibraryAbi.ReadData), readArgs));
        Plain(M(typeof(FaslData), "Share", lo, typeof(LispObject[]), typeof(int)),
            Abi(nameof(LibraryAbi.ShareData), lo, typeof(LispObject[]), typeof(int)));

        Plain(lf.GetProperty("Environment")!.GetGetMethod()!, Abi(nameof(LibraryAbi.Environment), lf));
        Plain(M(lf, "ClosureEntryCheck"), Abi(nameof(LibraryAbi.ClosureCheck)));
        Plain(M(lf, "MakeDirectClosure", typeof(System.Delegate), typeof(object[]), typeof(string)),
            Abi(nameof(LibraryAbi.DirectClosure), typeof(System.Delegate), typeof(object[]), typeof(string)));
        var withEntry = Abi(nameof(LibraryAbi.WithEntry), lo, typeof(int), typeof(System.IntPtr), typeof(int));
        m[M(lf, "WithClosureEntry", lo, typeof(System.IntPtr), typeof(int))] = new Redirect(Kind.WithEntry, withEntry, LibraryAbi.EntryArity);
        m[M(lf, "WithMvModeEntry", lo, typeof(int))] = new Redirect(Kind.WithEntry, withEntry, LibraryAbi.EntryMode);

        Plain(typeof(Startup.SymSite).GetConstructor(new[] { typeof(string) })!,
            Abi(nameof(LibraryAbi.NameSite), typeof(string)));
        Plain(typeof(Startup.SymFnSite).GetConstructor(new[] { typeof(string), typeof(string) })!,
            Abi(nameof(LibraryAbi.SymbolSite), typeof(string), typeof(string)));
        Plain(lf.GetConstructor(new[] { typeof(System.Func<LispObject[], LispObject>), typeof(string), typeof(int) })!,
            Abi(nameof(LibraryAbi.Function), typeof(System.Func<LispObject[], LispObject>), typeof(string), typeof(int)));
        Plain(lf.GetConstructor(new[] { typeof(System.Func<object[], LispObject[], LispObject>), typeof(object[]), typeof(string), typeof(int) })!,
            Abi(nameof(LibraryAbi.Closure), typeof(System.Func<object[], LispObject[], LispObject>), typeof(object[]), typeof(string), typeof(int)));
        return m;
    }

    private void Flush()
    {
        if (_tail) { _in.Emit(OpCodes.Tailcall); _tail = false; }
    }

    private bool TryRedirect(MethodBase from)
    {
        if (!s_map.TryGetValue(from, out var r)) return false;
        bool tail = _tail;
        _tail = false;
        switch (r.Kind)
        {
            case Kind.PushNull:
                _in.Emit(OpCodes.Ldnull);
                break;
            case Kind.DropArgs:
                for (int i = 0; i < r.N; i++) _in.Emit(OpCodes.Pop);
                break;
            case Kind.CallFixed:
            case Kind.CallArray:
                _in.Emit(OpCodes.Ldc_I4_0);
                break;
            case Kind.DropCall:
                _in.Emit(OpCodes.Pop);
                _in.Emit(OpCodes.Ldc_I4_0);
                break;
            case Kind.DropCallMode:
                _in.Emit(OpCodes.Pop);
                break;
            case Kind.CallWide:
                for (int k = r.N; k < 8; k++) _in.Emit(OpCodes.Ldnull);
                _in.Emit(OpCodes.Ldc_I4, r.N);
                _in.Emit(OpCodes.Ldc_I4_0);
                break;
            case Kind.CallWideMode:
            {
                var mode = Temp(typeof(int), 0);
                _in.EmitLoc(OpCodes.Stloc, mode);
                for (int k = r.N; k < 8; k++) _in.Emit(OpCodes.Ldnull);
                _in.Emit(OpCodes.Ldc_I4, r.N);
                _in.EmitLoc(OpCodes.Ldloc, mode);
                break;
            }
            case Kind.Entry:
                EntryArgs(r.N);
                break;
            case Kind.WithEntry:
            {
                var a = Temp(typeof(int), 0);
                _in.EmitLoc(OpCodes.Stloc, a);
                LocalBuilder? p = null;
                if (r.N == LibraryAbi.EntryArity) _in.EmitLoc(OpCodes.Stloc, p = Temp(typeof(System.IntPtr), 0));
                _in.Emit(OpCodes.Ldc_I4, r.N);
                if (p != null) _in.EmitLoc(OpCodes.Ldloc, p); else { _in.Emit(OpCodes.Ldc_I4_0); _in.Emit(OpCodes.Conv_I); }
                _in.EmitLoc(OpCodes.Ldloc, a);
                break;
            }
        }
        if (tail) _in.Emit(OpCodes.Tailcall);
        _in.Emit(OpCodes.Call, r.Target);
        return true;
    }

    private readonly Dictionary<(System.Type, int), LocalBuilder> _temps = new();

    // Scratch locals, live only within one redirected call.
    private LocalBuilder Temp(System.Type t, int i)
    {
        if (!_temps.TryGetValue((t, i), out var l)) _temps[(t, i)] = l = _in.DeclareLocal(t);
        return l;
    }

    // From (fn, delegate), (fn, entry, arity) or (fn, arity) to the arguments of
    // LibraryAbi.SetEntry (fn, what, delegate, entry, arity).
    private void EntryArgs(int what)
    {
        bool del = what == LibraryAbi.EntryDirect || what == LibraryAbi.EntryNative;
        bool ptr = what == LibraryAbi.EntrySelfFramed || what == LibraryAbi.EntryArity;
        LocalBuilder? d = null, p = null, a = null;
        if (del) _in.EmitLoc(OpCodes.Stloc, d = Temp(typeof(System.Delegate), 0));
        else
        {
            _in.EmitLoc(OpCodes.Stloc, a = Temp(typeof(int), 0));
            if (ptr) _in.EmitLoc(OpCodes.Stloc, p = Temp(typeof(System.IntPtr), 0));
        }
        _in.Emit(OpCodes.Ldc_I4, what);
        if (d != null) _in.EmitLoc(OpCodes.Ldloc, d); else _in.Emit(OpCodes.Ldnull);
        if (p != null) _in.EmitLoc(OpCodes.Ldloc, p); else { _in.Emit(OpCodes.Ldc_I4_0); _in.Emit(OpCodes.Conv_I); }
        if (a != null) _in.EmitLoc(OpCodes.Ldloc, a); else _in.Emit(OpCodes.Ldc_I4_0);
    }

    public override void Emit(OpCode opcode, MethodInfo meth)
    {
        if ((opcode == OpCodes.Call || opcode == OpCodes.Callvirt) && TryRedirect(meth)) return;
        Flush(); _in.Emit(opcode, meth);
    }

    public override void EmitCall(OpCode opcode, MethodInfo methodInfo, System.Type[]? optionalParameterTypes)
    {
        if (optionalParameterTypes == null && (opcode == OpCodes.Call || opcode == OpCodes.Callvirt)
            && TryRedirect(methodInfo)) return;
        Flush(); _in.EmitCall(opcode, methodInfo, optionalParameterTypes);
    }

    private static Dictionary<ConstructorInfo, ConstructorInfo>? _lazy_ctors;
    private static Dictionary<ConstructorInfo, ConstructorInfo> s_ctors => _lazy_ctors ??= BuildCtors();
    private static Dictionary<ConstructorInfo, ConstructorInfo> BuildCtors() => new()
    {
        [typeof(HandlerBinding).GetConstructor(new[] { typeof(LispObject), typeof(LispFunction) })!] =
            typeof(LibraryAbi.Binding).GetConstructor(new[] { typeof(LispObject), typeof(LispFunction) })!,
        [typeof(HandlerBinding).GetConstructor(new[] { typeof(LispObject), typeof(int) })!] =
            typeof(LibraryAbi.Binding).GetConstructor(new[] { typeof(LispObject), typeof(int) })!,
        [typeof(LispRestart).GetConstructors()[0]] = typeof(LibraryAbi.Restart).GetConstructors()[0],
    };

    public override void Emit(OpCode opcode, ConstructorInfo con)
    {
        if (opcode == OpCodes.Newobj && TryRedirect(con)) return;
        Flush();
        if (opcode == OpCodes.Newobj && s_ctors.TryGetValue(con, out var to)) con = to;
        _in.Emit(opcode, con);
    }

    public override void Emit(OpCode opcode, FieldInfo field)
    {
        Flush();
        if (opcode == OpCodes.Ldsfld && field == s_absentField) { _in.Emit(OpCodes.Call, s_absent); return; }
        _in.Emit(opcode, field);
    }

    public override LocalBuilder DeclareLocal(System.Type localType, bool pinned) =>
        _in.DeclareLocal(LibraryType(localType), pinned);

    public override void Emit(OpCode opcode)
    {
        if (opcode == OpCodes.Tailcall) { Flush(); _tail = true; return; }
        Flush(); _in.Emit(opcode);
    }

    public override int ILOffset => _in.ILOffset;
    public override void BeginCatchBlock(System.Type? exceptionType) { Flush(); _in.BeginCatchBlock(exceptionType); }
    public override void BeginExceptFilterBlock() { Flush(); _in.BeginExceptFilterBlock(); }
    public override Label BeginExceptionBlock() { Flush(); return _in.BeginExceptionBlock(); }
    public override void BeginFaultBlock() { Flush(); _in.BeginFaultBlock(); }
    public override void BeginFinallyBlock() { Flush(); _in.BeginFinallyBlock(); }
    public override void BeginScope() => _in.BeginScope();
    public override Label DefineLabel() => _in.DefineLabel();
    public override void Emit(OpCode opcode, byte arg) { Flush(); _in.Emit(opcode, arg); }
    public override void Emit(OpCode opcode, double arg) { Flush(); _in.Emit(opcode, arg); }
    public override void Emit(OpCode opcode, short arg) { Flush(); _in.Emit(opcode, arg); }
    public override void Emit(OpCode opcode, int arg) { Flush(); _in.Emit(opcode, arg); }
    public override void Emit(OpCode opcode, long arg) { Flush(); _in.Emit(opcode, arg); }
    public override void Emit(OpCode opcode, Label label) { Flush(); _in.Emit(opcode, label); }
    public override void Emit(OpCode opcode, Label[] labels) { Flush(); _in.Emit(opcode, labels); }
    public override void Emit(OpCode opcode, LocalBuilder local) { Flush(); _in.Emit(opcode, local); }
    public override void Emit(OpCode opcode, SignatureHelper signature) { Flush(); _in.Emit(opcode, signature); }
    public override void Emit(OpCode opcode, float arg) { Flush(); _in.Emit(opcode, arg); }
    public override void Emit(OpCode opcode, string str) { Flush(); _in.Emit(opcode, str); }
    public override void Emit(OpCode opcode, System.Type cls)
    {
        Flush();
        if (opcode == OpCodes.Castclass && (cls == typeof(CatchThrowException) || cls == typeof(RestartInvocationException))) return;
        _in.Emit(opcode, LibraryType(cls));
    }

    // The type a library names in place of a runtime type it must not name.
    private static System.Type LibraryType(System.Type t) =>
        t == typeof(LispFunction.Frame) ? typeof(LibraryAbi.Frame)
        : t == typeof(HandlerBinding) ? typeof(LibraryAbi.Binding)
        : t == typeof(HandlerBinding[]) ? typeof(LibraryAbi.Binding[])
        : t == typeof(LispRestart) ? typeof(LibraryAbi.Restart)
        : t == typeof(LispRestart[]) ? typeof(LibraryAbi.Restart[])
        : t == typeof(MvHeld) ? typeof(LibraryAbi.Held)
        : t == typeof(CatchThrowException) ? typeof(object)
        : t;
    public override void EmitCalli(OpCode opcode, CallingConventions callingConvention, System.Type? returnType,
        System.Type[]? parameterTypes, System.Type[]? optionalParameterTypes)
    { Flush(); _in.EmitCalli(opcode, callingConvention, returnType, parameterTypes, optionalParameterTypes); }
    public override void EmitCalli(OpCode opcode, System.Runtime.InteropServices.CallingConvention unmanagedCallConv,
        System.Type? returnType, System.Type[]? parameterTypes)
    { Flush(); _in.EmitCalli(opcode, unmanagedCallConv, returnType, parameterTypes); }
    public override void EndExceptionBlock() { Flush(); _in.EndExceptionBlock(); }
    public override void EndScope() => _in.EndScope();
    public override void MarkLabel(Label loc) { Flush(); _in.MarkLabel(loc); }
    public override void UsingNamespace(string usingNamespace) => _in.UsingNamespace(usingNamespace);
    protected override void MarkSequencePointCore(System.Diagnostics.SymbolStore.ISymbolDocumentWriter document,
        int startLine, int startColumn, int endLine, int endColumn)
        => _in.MarkSequencePoint(document, startLine, startColumn, endLine, endColumn);
}
#endif
