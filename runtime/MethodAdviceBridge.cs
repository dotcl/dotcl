using System.Diagnostics;
using System.Reflection;

namespace DotCL;

/// <summary>
/// Runtime scaffolding for the `harmony` contrib (interactive advice on live
/// .NET methods). This class references NO Harmony types: it is only a registry
/// of Lisp closures keyed by the target <see cref="MethodBase"/>, plus universal
/// advice methods whose parameters use Harmony's name-based injection convention
/// (<c>__originalMethod</c> / <c>__instance</c> / <c>__args</c> / <c>__result</c>).
/// The contrib resolves Lib.Harmony via `(require "nuget")`, then hands
/// Harmony the <see cref="MethodInfo"/> of <see cref="Postfix"/> as the patch;
/// Harmony discovers and invokes it by reflection at runtime.
///
/// PoC note: this bridge lives in the runtime for now so the harmony PoC needs
/// no C# build step in the contrib. The intended end state is to emit the same
/// static method from Lisp via `dotnet:define-class` (:static), which does not
/// exist yet; when it does, this file can be deleted and the contrib becomes
/// pure Lisp.
/// </summary>
public static class MethodAdviceBridge
{
    // Keyed by the original MethodBase Harmony reports via __originalMethod.
    // _postHandlers: read-only observers (watch). _patchHandlers: return-value
    // rewriters (patch) — their value replaces the method's result.
    private static readonly Dictionary<MethodBase, LispObject> _postHandlers = new();
    private static readonly Dictionary<MethodBase, LispObject> _patchHandlers = new();
    // _traceHandlers: closures fed (instance args result elapsed-seconds) for timing.
    private static readonly Dictionary<MethodBase, LispObject> _traceHandlers = new();
    private static readonly object _lock = new();

    /// <summary>Register (or replace) the read-only closure run after TARGET returns.</summary>
    public static void RegisterPostfix(MethodBase target, LispObject fn)
    {
        lock (_lock) _postHandlers[target] = fn;
    }

    /// <summary>Register (or replace) the closure whose value replaces TARGET's result.</summary>
    public static void RegisterPatch(MethodBase target, LispObject fn)
    {
        lock (_lock) _patchHandlers[target] = fn;
    }

    /// <summary>Remove the observer closure for TARGET (idempotent).</summary>
    public static bool UnregisterPostfix(MethodBase target)
    {
        lock (_lock) return _postHandlers.Remove(target);
    }

    /// <summary>Remove the rewriter closure for TARGET (idempotent).</summary>
    public static bool UnregisterPatch(MethodBase target)
    {
        lock (_lock) return _patchHandlers.Remove(target);
    }

    /// <summary>Register (or replace) the timing closure run around TARGET.</summary>
    public static void RegisterTrace(MethodBase target, LispObject fn)
    {
        lock (_lock) _traceHandlers[target] = fn;
    }

    /// <summary>Remove the timing closure for TARGET (idempotent).</summary>
    public static bool UnregisterTrace(MethodBase target)
    {
        lock (_lock) return _traceHandlers.Remove(target);
    }

    /// <summary>
    /// Universal Harmony postfix. Harmony injects the arguments by parameter
    /// name. Marshals INSTANCE, the argument vector (as a Lisp list) and the
    /// return value across to Lisp and funcalls the registered closure with
    /// <c>(instance args-list result)</c>. Read-only: the closure's value is
    /// ignored, so this cannot change control flow (that is a later `patch`).
    /// </summary>
    public static void Postfix(MethodBase __originalMethod, object? __instance,
                               object?[]? __args, object? __result)
    {
        LispObject fn;
        lock (_lock)
        {
            if (!_postHandlers.TryGetValue(__originalMethod, out fn!)) return;
        }

        // Build a Lisp list from the boxed argument vector, preserving order.
        LispObject argList = Nil.Instance;
        if (__args != null)
            for (int i = __args.Length - 1; i >= 0; i--)
                argList = new Cons(Runtime.DotNetToLisp(__args[i]), argList);

        var lispArgs = new[]
        {
            Runtime.DotNetToLisp(__instance),
            argList,
            Runtime.DotNetToLisp(__result),
        };
        // Cross the C#->Lisp boundary through the foreign-callback handler so a
        // Lisp error in the advice body is contained rather than crashing the
        // patched application's thread.
        Runtime.InvokeForeignCallback(fn, lispArgs);
    }

    /// <summary>
    /// Universal Harmony postfix that REPLACES the return value: funcalls the
    /// registered closure with <c>(instance args-list result)</c> and writes its
    /// value back through <c>ref __result</c>, converting to the method's actual
    /// return type. This is how `harmony:patch` fixes a method's output in place
    /// (Arthas "redefine"). Value-type results round-trip via boxing.
    /// </summary>
    public static void PostfixReplace(MethodBase __originalMethod, object? __instance,
                                      object?[]? __args, ref object? __result)
    {
        LispObject fn;
        lock (_lock)
        {
            if (!_patchHandlers.TryGetValue(__originalMethod, out fn!)) return;
        }

        LispObject argList = Nil.Instance;
        if (__args != null)
            for (int i = __args.Length - 1; i >= 0; i--)
                argList = new Cons(Runtime.DotNetToLisp(__args[i]), argList);

        var lispArgs = new[]
        {
            Runtime.DotNetToLisp(__instance),
            argList,
            Runtime.DotNetToLisp(__result),
        };
        var replacement = Runtime.InvokeForeignCallback(fn, lispArgs);

        // Convert back to the method's declared return type and box for `ref object`.
        var returnType = (__originalMethod as MethodInfo)?.ReturnType ?? typeof(object);
        if (returnType != typeof(void))
            __result = Runtime.LispToDotNet(replacement, returnType);
    }

    /// <summary>
    /// Harmony prefix half of `trace`: stamps the start time into <c>__state</c>,
    /// which Harmony threads through to <see cref="TracePostfix"/> for the same
    /// call. Only pays the cost when a trace closure is registered for TARGET.
    /// </summary>
    public static void TracePrefix(MethodBase __originalMethod, ref object? __state)
    {
        lock (_lock)
        {
            if (!_traceHandlers.ContainsKey(__originalMethod)) return;
        }
        __state = Stopwatch.GetTimestamp();
    }

    /// <summary>
    /// Harmony postfix half of `trace`: computes the elapsed wall time from the
    /// prefix's <c>__state</c> stamp and funcalls the registered closure with
    /// <c>(instance args-list result elapsed-seconds)</c> (Arthas "trace").
    /// </summary>
    public static void TracePostfix(MethodBase __originalMethod, object? __instance,
                                    object?[]? __args, object? __result, object? __state)
    {
        LispObject fn;
        lock (_lock)
        {
            if (!_traceHandlers.TryGetValue(__originalMethod, out fn!)) return;
        }
        if (__state is not long start) return;   // prefix skipped (no handler at entry)
        double seconds = (Stopwatch.GetTimestamp() - start) / (double)Stopwatch.Frequency;

        LispObject argList = Nil.Instance;
        if (__args != null)
            for (int i = __args.Length - 1; i >= 0; i--)
                argList = new Cons(Runtime.DotNetToLisp(__args[i]), argList);

        var lispArgs = new[]
        {
            Runtime.DotNetToLisp(__instance),
            argList,
            Runtime.DotNetToLisp(__result),
            Runtime.DotNetToLisp(seconds),
        };
        Runtime.InvokeForeignCallback(fn, lispArgs);
    }

    /// <summary>
    /// Demo target for the harmony PoC and its regression test: a plain, jitted
    /// static method in a normal loaded assembly, which Harmony can reliably
    /// patch (Reflection.Emit dynamic-assembly methods are not always
    /// patchable on CoreCLR, so define-class targets are unsuitable for a
    /// deterministic test).
    /// </summary>
    public static int DemoAdd(int a, int b) => a + b;
}
