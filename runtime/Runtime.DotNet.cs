namespace DotCL;

/// <summary>
/// Wraps a .NET object for use in Lisp code.
/// </summary>
public class LispDotNetObject : LispObject
{
    public object Value { get; }
    public Type Type { get; }

    public LispDotNetObject(object value)
    {
        Value = value ?? throw new ArgumentNullException(nameof(value));
        Type = value.GetType();
    }

    public override string ToString()
        => $"#<DOTNET {Type.FullName} {Value}>";
}

/// <summary>
/// A LispObject that carries a type hint for .NET method resolution.
/// </summary>
public class LispDotNetBoxed : LispDotNetObject
{
    public Type HintType { get; }

    public LispDotNetBoxed(object value, Type hintType) : base(value)
    {
        HintType = hintType;
    }

    public override string ToString()
        => $"#<DOTNET-BOXED {HintType.Name} {Value}>";
}

/// <summary>Singleton marker returned by (dotnet:null): marshals to an explicit
/// .NET null, distinct from Lisp NIL (which means false for bool / bool?).</summary>
public sealed class LispDotNetNull : LispObject
{
    public static readonly LispDotNetNull Instance = new();
    private LispDotNetNull() { }
    public override string ToString() => "#<DOTNET-NULL>";
}

public static partial class Runtime
{
    internal static readonly LispObject DotNetNullMarker = LispDotNetNull.Instance;

    public static LispObject DotNetNull(LispObject[] args)
    {
        if (args.Length != 0)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:NULL: takes no arguments"));
        return DotNetNullMarker;
    }

    // Tracks dynamically-defined class names (uppercase simple name → Type) for
    // case-insensitive resolution from Lisp symbols (e.g. symbol Animal → "ANIMAL").
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<string, Type>
        _dotNetDynTypeByUpperName = new(StringComparer.OrdinalIgnoreCase);

    /// <summary>Convert a .NET object to an appropriate LispObject.</summary>
    public static LispObject DotNetToLisp(object? value)
    {
        if (value == null) return Nil.Instance;
        return value switch
        {
            int i => Fixnum.Make(i),
            long l => Fixnum.Make(l),
            // C# type patterns do not implicitly widen, so the small integer types must
            // be matched explicitly or they fall through to LispDotNetObject (boxed),
            // breaking byte[] reads (UTF-8 codecs, binary protocols) and any reflection
            // call returning a byte/short-family scalar. All fit in a 64-bit fixnum
            // except ulong/nuint above long.MaxValue, which promote to a bignum.
            byte b8 => Fixnum.Make(b8),
            sbyte sb => Fixnum.Make(sb),
            short s16 => Fixnum.Make(s16),
            ushort u16 => Fixnum.Make(u16),
            uint u32 => Fixnum.Make(u32),
            ulong u64 => u64 <= long.MaxValue
                ? Fixnum.Make((long)u64)
                : (LispObject)Bignum.MakeInteger((System.Numerics.BigInteger)u64),
            nint ni => Fixnum.Make((long)ni),
            nuint nu => (ulong)nu <= long.MaxValue
                ? Fixnum.Make((long)nu)
                : (LispObject)Bignum.MakeInteger((System.Numerics.BigInteger)(ulong)nu),
            double d => new DoubleFloat(d),
            float f => new DoubleFloat(f),
            // Preserve decimal as a first-class scale-keeping value (not a normalized
            // rational, which would drop trailing zeros / the .NET-specific scale).
            decimal m => new LispDecimal(m),
            string s => new LispString(s),
            char c => LispChar.Make(c),
            bool b => b ? (LispObject)T.Instance : Nil.Instance,
            LispObject lo => lo,
            _ => new LispDotNetObject(value)
        };
    }

    private static readonly System.Numerics.BigInteger DecimalMaxInt = new(decimal.MaxValue);
    private static readonly System.Numerics.BigInteger DecimalMinInt = new(decimal.MinValue);

    /// <summary>Convert an exact rational (num/den, normalized) to System.Decimal, or signal
    /// a Lisp error when it is not exactly representable: the denominator has a prime factor
    /// other than 2 or 5 (e.g. 1/3), or the value needs scale &gt; 28 / a mantissa wider than
    /// 96 bits. Used when marshalling a CL ratio into a decimal-typed .NET parameter.</summary>
    private static decimal RationalToDecimalExact(System.Numerics.BigInteger num, System.Numerics.BigInteger den, LispObject arg)
    {
        var d = den;
        int twos = 0, fives = 0;
        while (d % 2 == 0) { d /= 2; twos++; }
        while (d % 5 == 0) { d /= 5; fives++; }
        if (d != System.Numerics.BigInteger.One)
            throw new LispErrorException(new LispError(
                $"{arg} is not exactly representable as System.Decimal (denominator has a prime factor other than 2 or 5)"));
        int scale = System.Math.Max(twos, fives);
        if (scale > 28)
            throw new LispErrorException(new LispError(
                $"{arg} needs more than 28 decimal places (System.Decimal scale limit)"));
        // Exact: den = 2^twos * 5^fives divides 10^scale.
        var mantissa = num * System.Numerics.BigInteger.Pow(10, scale) / den;
        bool neg = mantissa.Sign < 0;
        var mag = System.Numerics.BigInteger.Abs(mantissa);
        if (mag > (System.Numerics.BigInteger.One << 96) - 1)
            throw new LispErrorException(new LispError(
                $"{arg} exceeds the System.Decimal 96-bit mantissa range"));
        uint lo = (uint)(mag & 0xFFFFFFFF);
        uint mid = (uint)((mag >> 32) & 0xFFFFFFFF);
        uint hi = (uint)((mag >> 64) & 0xFFFFFFFF);
        return new decimal((int)lo, (int)mid, (int)hi, neg, (byte)scale);
    }

    /// <summary>Normalize a Lisp-wrapped awaitable (Task / Task&lt;T&gt; / ValueTask /
    /// ValueTask&lt;T&gt;) to a System.Threading.Tasks.Task. Returns null if ARG does not
    /// wrap an awaitable.</summary>
    private static System.Threading.Tasks.Task? ToTask(LispObject arg)
    {
        if (arg is not LispDotNetObject dno) return null;
        object value = dno.Value;
        var vt = value.GetType();
        if (value is System.Threading.Tasks.ValueTask valueTask)
            return valueTask.AsTask();
        if (vt.IsGenericType
            && vt.GetGenericTypeDefinition() == typeof(System.Threading.Tasks.ValueTask<>))
            return (System.Threading.Tasks.Task)vt.GetMethod("AsTask")!.Invoke(value, null)!;
        return value as System.Threading.Tasks.Task;
    }

    /// <summary>Marshal a COMPLETED task's result to Lisp. NIL for a non-generic /
    /// void task. Caller must ensure the task is RanToCompletion.</summary>
    private static LispObject TaskResultToLisp(System.Threading.Tasks.Task task)
    {
        var rt = task.GetType();
        if (rt.IsGenericType)
        {
            var resultProp = rt.GetProperty("Result");
            if (resultProp != null)
            {
                var res = resultProp.GetValue(task);
                // Task<VoidTaskResult> is the internal shape of a non-generic async
                // Task; its Result is a private placeholder struct → NIL.
                if (res != null && res.GetType().FullName == "System.Threading.Tasks.VoidTaskResult")
                    return Nil.Instance;
                return DotNetToLisp(res);
            }
        }
        return Nil.Instance;
    }

    /// <summary>
    /// (dotnet:await awaitable) => value
    /// Block the current thread until a .NET Task / Task&lt;T&gt; / ValueTask /
    /// ValueTask&lt;T&gt; completes, returning the result marshalled to Lisp (NIL for
    /// a non-generic / void awaitable). A faulted awaitable rethrows its inner
    /// exception (not the wrapping AggregateException), so handler-case sees the
    /// real condition. This is the blocking primitive (step A): it holds the
    /// calling thread, so run it on a worker thread (bordeaux-threads) when the
    /// caller must stay responsive. Non-blocking (async ...) builds on %async-bind.
    /// </summary>
    public static LispObject DotNetAwait(LispObject[] args)
    {
        if (args.Length != 1)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:AWAIT: requires exactly 1 argument (a Task, Task<T>, ValueTask, or ValueTask<T>)"));
        var task = ToTask(args[0]);
        if (task == null)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:AWAIT: not an awaitable .NET object: " + args[0]));

        task.GetAwaiter().GetResult();   // block; unwraps AggregateException to the inner exception
        return TaskResultToLisp(task);
    }

    /// <summary>
    /// (dotcl:%async-return value) => task
    /// Lift a Lisp VALUE into an already-completed Task. The terminal continuation
    /// of an (async ...) block, so the block as a whole always yields a Task.
    /// </summary>
    public static LispObject AsyncReturn(LispObject[] args)
    {
        if (args.Length != 1)
            throw new LispErrorException(new LispProgramError(
                "DOTCL:%ASYNC-RETURN: requires exactly 1 argument"));
        return new LispDotNetObject(System.Threading.Tasks.Task.FromResult(args[0]));
    }

    /// <summary>Run a Lisp 0-arg thunk with a captured dynamic environment installed
    /// (specials + handler clusters), unwinding it afterwards. Used to run a deferred
    /// async cleanup/handler thunk on a continuation thread under the environment that
    /// was in scope where the construct was established.</summary>
    private static LispObject InvokeWithEnv(LispFunction thunk,
        Dictionary<Symbol, LispObject>? dyn, List<HandlerBinding[]>? handlers)
    {
        var baseDepth = DynamicBindings.Depth;
        var hBaseDepth = HandlerClusterStack.Depth;
        try
        {
            DynamicBindings.Restore(dyn);
            HandlerClusterStack.Restore(handlers);
            return thunk.Invoke0();
        }
        finally
        {
            DynamicBindings.TruncateTo(baseDepth);
            HandlerClusterStack.TruncateTo(hBaseDepth);
        }
    }

    /// <summary>Marker returned by an (async handler-case) dispatch when no clause
    /// matched, telling %async-try to propagate the original condition.</summary>
    internal sealed class AsyncDeclineMarker : LispObject
    {
        public static readonly AsyncDeclineMarker Instance = new();
        private AsyncDeclineMarker() { }
        public override string ToString() => "#<ASYNC-DECLINE>";
    }

    /// <summary>Thrown by the filter handler %async-try installs so that a Lisp
    /// condition SIGNALED within an async handler-case body (which would otherwise
    /// reach the debugger) becomes a Task fault that %async-try can dispatch.</summary>
    internal sealed class AsyncSignalException : Exception
    {
        public LispObject Condition { get; }
        public AsyncSignalException(LispObject condition) : base("async signal")
            => Condition = condition;
    }

    // The per-clause handler %async-try pushes: rethrows the matched condition as an
    // AsyncSignalException so it unwinds to %async-try as a fault.
    private static readonly LispFunction AsyncRaiseFn = new LispFunction(
        a => throw new AsyncSignalException(a[0]), "%ASYNC-RAISE", 1);

    /// <summary>(dotcl:%async-decline) => marker. Emitted by the handler-case
    /// dispatch's default (no-match) clause.</summary>
    public static LispObject AsyncDecline(LispObject[] args) => AsyncDeclineMarker.Instance;

    private static Exception UnwrapAggregate(Exception e)
    {
        while (e is AggregateException ae && ae.InnerException != null) e = ae.InnerException;
        return e;
    }

    /// <summary>Build the LispErrorException thrown when a reflected .NET call
    /// faults, preserving the inner CLR exception's type on the condition
    /// (ClrExceptionType) so dotnet:exception-type / dotnet:handler-bind can
    /// dispatch on it. CONTEXT is the "DOTNET:OP Type.Member" prefix. (dotcl/dotcl#45)</summary>
    private static LispErrorException DotNetInvokeError(string context, System.Reflection.TargetInvocationException tie)
    {
        var inner = tie.InnerException;
        return new LispErrorException(new LispError($"{context}: {inner?.Message ?? tie.Message}")
        {
            ClrExceptionType = inner?.GetType()
        });
    }

    private static LispObject ConditionFromException(Exception ex)
    {
        if (ex is AsyncSignalException ase) return ase.Condition;
        if (ex is LispErrorException le) return le.Condition;
        if (ex is HandlerCaseInvocationException hce) return hce.Condition;
        // raw .NET → program-error; append the .NET type + StackTrace only under
        // dotcl:*debug-stacktrace* so ordinary error reports stay clean. Preserve
        // the CLR type so dotnet:exception-type / dotnet:handler-bind can dispatch.
        return new LispProgramError(
            Startup.DebugStacktrace && !string.IsNullOrEmpty(ex.StackTrace)
                ? ex.Message + "\n[.NET " + ex.GetType().Name + "]\n" + ex.StackTrace
                : ex.Message)
        { ClrExceptionType = ex.GetType() };
    }

    private static LispObject InvokeWithEnv1(LispFunction fn, LispObject arg,
        Dictionary<Symbol, LispObject>? dyn, List<HandlerBinding[]>? handlers)
    {
        var baseDepth = DynamicBindings.Depth;
        var hBaseDepth = HandlerClusterStack.Depth;
        try
        {
            DynamicBindings.Restore(dyn);
            HandlerClusterStack.Restore(handlers);
            return fn.Invoke1(arg);
        }
        finally
        {
            DynamicBindings.TruncateTo(baseDepth);
            HandlerClusterStack.TruncateTo(hBaseDepth);
        }
    }

    /// <summary>
    /// (dotcl:%async-try clause-types body-thunk dispatch-fn) => task
    /// Async handler-case. CLAUSE-TYPES is the list of clause type-specifiers. A
    /// filter handler for those types is pushed around BODY-THUNK so a matching Lisp
    /// condition SIGNALED in the body (e.g. via error) unwinds as a fault instead of
    /// reaching the debugger; non-matching signals propagate normally. When the body
    /// Task faults, DISPATCH-FN(condition) (the macro-built typecase over the clauses)
    /// runs the matching clause's Task, or returns (dotcl:%async-decline) so the
    /// original condition propagates. Lisp control-flow exceptions bypass dispatch.
    /// Dispatch runs under the env captured at setup (outside the filter cluster).
    /// </summary>
    public static LispObject AsyncTry(LispObject[] args)
    {
        if (args.Length != 3)
            throw new LispErrorException(new LispProgramError(
                "DOTCL:%ASYNC-TRY: requires exactly 3 arguments (clause-types body-thunk dispatch-fn)"));
        if (args[1] is not LispFunction bodyThunk || args[2] is not LispFunction dispatchFn)
            throw new LispErrorException(new LispProgramError(
                "DOTCL:%ASYNC-TRY: body and dispatch must be functions"));

        // Capture env BEFORE installing the filter cluster, so clause bodies run
        // outside it (a re-signal in a handler must not re-enter our filter).
        var dynSnapshot = DynamicBindings.Snapshot();
        var handlerSnapshot = HandlerClusterStack.Snapshot();
        var tcs = new System.Threading.Tasks.TaskCompletionSource<LispObject>();

        var types = Runtime.ToList(args[0]);
        var cluster = new HandlerBinding[types.Count];
        for (int i = 0; i < types.Count; i++) cluster[i] = new HandlerBinding(types[i], AsyncRaiseFn);

        System.Threading.Tasks.Task bodyTask;
        HandlerClusterStack.PushCluster(cluster);
        try { bodyTask = ToTask(bodyThunk.Invoke0()) ?? throw NotATask(); }
        catch (Exception e)
        {
            HandlerClusterStack.PopCluster();
            DispatchAsyncFault(e, dispatchFn, dynSnapshot, handlerSnapshot, tcs);
            return new LispDotNetObject(tcs.Task);
        }
        HandlerClusterStack.PopCluster();

        bodyTask.ContinueWith(bt =>
        {
            if (bt.IsCanceled) { tcs.SetCanceled(); return; }
            if (!bt.IsFaulted) { tcs.SetResult(((System.Threading.Tasks.Task<LispObject>)bt).Result); return; }
            DispatchAsyncFault(bt.Exception!, dispatchFn, dynSnapshot, handlerSnapshot, tcs);
        });
        return new LispDotNetObject(tcs.Task);
    }

    private static void DispatchAsyncFault(Exception ex, LispFunction dispatchFn,
        Dictionary<Symbol, LispObject>? dyn, List<HandlerBinding[]>? handlers,
        System.Threading.Tasks.TaskCompletionSource<LispObject> tcs)
    {
        var inner = UnwrapAggregate(ex);
        // Control-flow transfers are not conditions — let them propagate.
        if (inner is BlockReturnException || inner is CatchThrowException ||
            inner is GoException || inner is RestartInvocationException)
        { tcs.SetException(inner); return; }

        LispObject result;
        try { result = InvokeWithEnv1(dispatchFn, ConditionFromException(inner), dyn, handlers); }
        catch (Exception he) { tcs.SetException(he); return; }   // a handler clause threw

        if (ReferenceEquals(result, AsyncDeclineMarker.Instance))
        { tcs.SetException(inner); return; }                     // no clause matched → re-raise

        var next = ToTask(result);
        if (next == null) { tcs.SetResult(result); return; }
        next.ContinueWith(nt =>
        {
            if (nt.IsFaulted) tcs.SetException(nt.Exception!.InnerExceptions);
            else if (nt.IsCanceled) tcs.SetCanceled();
            else tcs.SetResult(((System.Threading.Tasks.Task<LispObject>)nt).Result);
        });
    }

    /// <summary>
    /// (dotcl:%async-unwind-protect body-thunk cleanup-thunk) => task
    /// Async unwind-protect. Runs BODY-THUNK (→ Task); once it settles (success,
    /// fault, or cancel) runs CLEANUP-THUNK (→ Task) for effect, then propagates
    /// BODY's original outcome. A fault raised by the cleanup itself supersedes the
    /// body's outcome (as in synchronous CL). The cleanup runs under the dynamic
    /// environment captured where the unwind-protect was established.
    /// </summary>
    public static LispObject AsyncUnwindProtect(LispObject[] args)
    {
        if (args.Length != 2)
            throw new LispErrorException(new LispProgramError(
                "DOTCL:%ASYNC-UNWIND-PROTECT: requires exactly 2 arguments (body-thunk cleanup-thunk)"));
        if (args[0] is not LispFunction bodyThunk || args[1] is not LispFunction cleanupThunk)
            throw new LispErrorException(new LispProgramError(
                "DOTCL:%ASYNC-UNWIND-PROTECT: arguments must be functions"));

        var dynSnapshot = DynamicBindings.Snapshot();
        var handlerSnapshot = HandlerClusterStack.Snapshot();

        System.Threading.Tasks.Task bodyTask;
        try { bodyTask = ToTask(bodyThunk.Invoke0()) ?? throw NotATask(); }
        catch (Exception e)
        {
            // Body failed to even start: still run cleanup, then rethrow.
            var ftcs = new System.Threading.Tasks.TaskCompletionSource<LispObject>();
            RunCleanupThen(cleanupThunk, dynSnapshot, handlerSnapshot, ftcs,
                onCleanupDone: () => ftcs.SetException(e));
            return new LispDotNetObject(ftcs.Task);
        }

        var tcs = new System.Threading.Tasks.TaskCompletionSource<LispObject>();
        bodyTask.ContinueWith(bt =>
            RunCleanupThen(cleanupThunk, dynSnapshot, handlerSnapshot, tcs,
                onCleanupDone: () =>
                {
                    if (bt.IsFaulted) tcs.SetException(bt.Exception!.InnerExceptions);
                    else if (bt.IsCanceled) tcs.SetCanceled();
                    else tcs.SetResult(((System.Threading.Tasks.Task<LispObject>)bt).Result);
                }));
        return new LispDotNetObject(tcs.Task);
    }

    private static LispErrorException NotATask()
        => new LispErrorException(new LispProgramError(
            "DOTCL:%ASYNC-UNWIND-PROTECT: thunk did not return an awaitable"));

    /// <summary>Run CLEANUP (→ Task) under the captured env; on its completion call
    /// onCleanupDone, unless the cleanup itself faults (which then supersedes).</summary>
    private static void RunCleanupThen(LispFunction cleanupThunk,
        Dictionary<Symbol, LispObject>? dyn, List<HandlerBinding[]>? handlers,
        System.Threading.Tasks.TaskCompletionSource<LispObject> tcs, Action onCleanupDone)
    {
        System.Threading.Tasks.Task cleanupTask;
        try { cleanupTask = ToTask(InvokeWithEnv(cleanupThunk, dyn, handlers)) ?? throw NotATask(); }
        catch (Exception ce) { tcs.SetException(ce); return; }   // cleanup error wins
        cleanupTask.ContinueWith(ct =>
        {
            if (ct.IsFaulted) tcs.SetException(ct.Exception!.InnerExceptions);  // cleanup error wins
            else if (ct.IsCanceled) tcs.SetCanceled();
            else onCleanupDone();
        });
    }

    /// <summary>
    /// (dotcl:%async-restart restart-names body-thunk dispatch-fn) => task
    /// Async restart-case. RESTART-NAMES is a Lisp list of restart name strings;
    /// a LispRestart cluster (one per name, each with a fresh tag) is pushed around
    /// BODY-THUNK so find-restart / compute-restarts / invoke-restart see them, even
    /// across an await (the cluster is snapshotted into continuations by %async-bind).
    /// When the body Task faults with a RestartInvocationException whose tag belongs
    /// to this cluster, DISPATCH-FN(name-string, args-list) (the macro-built dispatch
    /// over the clauses) runs the matching clause's Task; its value becomes the
    /// restart-case value. A non-matching restart invocation (an outer restart) or any
    /// other exception propagates. Mirrors %async-try for the restart-case shape.
    /// </summary>
    public static LispObject AsyncRestart(LispObject[] args)
    {
        if (args.Length != 3)
            throw new LispErrorException(new LispProgramError(
                "DOTCL:%ASYNC-RESTART: requires exactly 3 arguments (restart-names body-thunk dispatch-fn)"));
        if (args[1] is not LispFunction bodyThunk || args[2] is not LispFunction dispatchFn)
            throw new LispErrorException(new LispProgramError(
                "DOTCL:%ASYNC-RESTART: body and dispatch must be functions"));

        // Capture env BEFORE pushing our cluster, so clause bodies run outside it
        // (re-invoking the same restart from a clause would be a control error, not
        // a re-entry of our cluster).
        var dynSnapshot = DynamicBindings.Snapshot();
        var handlerSnapshot = HandlerClusterStack.Snapshot();
        var restartSnapshot = RestartClusterStack.Snapshot();
        var tcs = new System.Threading.Tasks.TaskCompletionSource<LispObject>();

        var names = Runtime.ToList(args[0]);
        var cluster = new LispRestart[names.Count];
        // tag → clause name, so a caught RestartInvocationException maps back to the
        // clause to dispatch. Tags are reference-unique per LispRestart.
        var tagNames = new Dictionary<object, string>(names.Count);
        for (int i = 0; i < names.Count; i++)
        {
            string nm = names[i] switch { LispString s => s.Value, Symbol sy => sy.Name, _ => names[i].ToString() ?? "" };
            // Handler is unused for non-bind restarts (invoke-restart throws by tag);
            // supply a thrower for symmetry with the synchronous path.
            var r = new LispRestart(nm, _ => throw new RestartInvocationException(new object(), Array.Empty<LispObject>()));
            cluster[i] = r;
            tagNames[r.Tag] = nm;
        }

        System.Threading.Tasks.Task bodyTask;
        RestartClusterStack.PushCluster(cluster);
        try { bodyTask = ToTask(bodyThunk.Invoke0()) ?? throw NotATask(); }
        catch (Exception e)
        {
            RestartClusterStack.PopCluster();
            DispatchAsyncRestart(e, tagNames, dispatchFn, dynSnapshot, handlerSnapshot, tcs);
            return new LispDotNetObject(tcs.Task);
        }
        RestartClusterStack.PopCluster();

        bodyTask.ContinueWith(bt =>
        {
            if (bt.IsCanceled) { tcs.SetCanceled(); return; }
            if (!bt.IsFaulted) { tcs.SetResult(((System.Threading.Tasks.Task<LispObject>)bt).Result); return; }
            DispatchAsyncRestart(bt.Exception!, tagNames, dispatchFn, dynSnapshot, handlerSnapshot, tcs);
        });
        return new LispDotNetObject(tcs.Task);
    }

    private static void DispatchAsyncRestart(Exception ex, Dictionary<object, string> tagNames,
        LispFunction dispatchFn, Dictionary<Symbol, LispObject>? dyn, List<HandlerBinding[]>? handlers,
        System.Threading.Tasks.TaskCompletionSource<LispObject> tcs)
    {
        var inner = UnwrapAggregate(ex);
        // Only a restart invocation targeting one of OUR restarts is handled here;
        // everything else (outer restart, condition, control transfer) propagates.
        if (inner is not RestartInvocationException rie || !tagNames.TryGetValue(rie.Tag, out var name))
        { tcs.SetException(inner); return; }

        // args list (the invoke-restart arguments) → Lisp list for the dispatch fn.
        LispObject argList = Nil.Instance;
        for (int i = rie.Arguments.Length - 1; i >= 0; i--) argList = new Cons(rie.Arguments[i], argList);

        LispObject result;
        try { result = InvokeWithEnv2(dispatchFn, new LispString(name), argList, dyn, handlers); }
        catch (Exception he) { tcs.SetException(he); return; }

        var next = ToTask(result);
        if (next == null) { tcs.SetResult(result); return; }
        next.ContinueWith(nt =>
        {
            if (nt.IsFaulted) tcs.SetException(nt.Exception!.InnerExceptions);
            else if (nt.IsCanceled) tcs.SetCanceled();
            else tcs.SetResult(((System.Threading.Tasks.Task<LispObject>)nt).Result);
        });
    }

    private static LispObject InvokeWithEnv2(LispFunction fn, LispObject a0, LispObject a1,
        Dictionary<Symbol, LispObject>? dyn, List<HandlerBinding[]>? handlers)
    {
        var baseDepth = DynamicBindings.Depth;
        var hBaseDepth = HandlerClusterStack.Depth;
        try
        {
            DynamicBindings.Restore(dyn);
            HandlerClusterStack.Restore(handlers);
            return fn.Invoke(new[] { a0, a1 });
        }
        finally
        {
            DynamicBindings.TruncateTo(baseDepth);
            HandlerClusterStack.TruncateTo(hBaseDepth);
        }
    }

    /// <summary>
    /// (dotcl:%async-bind awaitable fn) => task
    /// Non-blocking monadic bind for (async ...). When AWAITABLE completes, call FN
    /// (a 1-arg Lisp function = the continuation) with the marshalled result; FN
    /// returns the next Task in the chain. Returns a Task that completes with that
    /// next Task's result. Faults/cancellation propagate. The continuation runs on a
    /// thread-pool thread (ContinueWith); the caller's dynamic (special-variable)
    /// bindings are snapshotted here and re-installed around the continuation so
    /// specials established before the await stay visible across it. Still out of
    /// scope for this cut: handler-case/unwind-protect spanning an await, and
    /// multiple values from an awaited form.
    /// </summary>
    public static LispObject AsyncBind(LispObject[] args)
    {
        if (args.Length != 2)
            throw new LispErrorException(new LispProgramError(
                "DOTCL:%ASYNC-BIND: requires exactly 2 arguments (awaitable continuation)"));
        var task = ToTask(args[0]);
        if (task == null)
            throw new LispErrorException(new LispProgramError(
                "DOTCL:%ASYNC-BIND: first argument is not awaitable: " + args[0]));
        if (args[1] is not LispFunction cont)
            throw new LispErrorException(new LispProgramError(
                "DOTCL:%ASYNC-BIND: second argument is not a function"));

        // Capture the caller's dynamic environment now, while the relevant special
        // bindings and handler clusters are still in scope; the continuation runs on
        // another thread with fresh (ThreadStatic) stacks. Specials and
        // handler-bind clusters (so handlers established around an await stay active
        // when a continuation signals) are re-installed around the continuation.
        var dynSnapshot = DynamicBindings.Snapshot();
        var handlerSnapshot = HandlerClusterStack.Snapshot();
        var restartSnapshot = RestartClusterStack.Snapshot();   // restart-case across await

        var tcs = new System.Threading.Tasks.TaskCompletionSource<LispObject>();
        task.ContinueWith(t =>
        {
            if (t.IsFaulted) { tcs.SetException(t.Exception!.InnerExceptions); return; }
            if (t.IsCanceled) { tcs.SetCanceled(); return; }
            LispObject next;
            var baseDepth = DynamicBindings.Depth;
            var hBaseDepth = HandlerClusterStack.Depth;
            var rBaseDepth = RestartClusterStack.Depth;
            try
            {
                DynamicBindings.Restore(dynSnapshot);    // install captured specials
                HandlerClusterStack.Restore(handlerSnapshot);  // and handler clusters
                RestartClusterStack.Restore(restartSnapshot);  // and restart clusters
                var v = TaskResultToLisp(t);
                next = cont.Invoke1(v);   // continuation returns the next Task
            }
            catch (Exception e) { tcs.SetException(e); return; }
            finally
            {
                DynamicBindings.TruncateTo(baseDepth);
                HandlerClusterStack.TruncateTo(hBaseDepth);
                RestartClusterStack.TruncateTo(rBaseDepth);
            }

            var nextTask = ToTask(next);
            if (nextTask == null) { tcs.SetResult(next); return; }   // lenient: plain value
            nextTask.ContinueWith(nt =>
            {
                if (nt.IsFaulted) tcs.SetException(nt.Exception!.InnerExceptions);
                else if (nt.IsCanceled) tcs.SetCanceled();
                else
                {
                    try { tcs.SetResult(TaskResultToLisp(nt)); }
                    catch (Exception e) { tcs.SetException(e); }
                }
            });
        });
        return new LispDotNetObject(tcs.Task);
    }

    /// <summary>Convert a LispObject to a .NET type based on target parameter type.</summary>
    public static object? LispToDotNet(LispObject arg, Type targetType)
    {
        // (dotnet:null) marker → an explicit .NET null, for any reference or
        // Nullable<T> target (e.g. a null/indeterminate CheckBox.IsChecked).
        if (ReferenceEquals(arg, DotNetNullMarker)) return null;

        // LispDotNetBoxed: use hint type
        if (arg is LispDotNetBoxed boxed)
        {
            if (targetType.IsAssignableFrom(boxed.HintType))
                return boxed.Value;
            return Convert.ChangeType(boxed.Value, targetType);
        }

        // LispDotNetObject: unwrap
        if (arg is LispDotNetObject dno)
        {
            if (targetType.IsAssignableFrom(dno.Type))
                return dno.Value;
            return Convert.ChangeType(dno.Value, targetType);
        }

        // Nullable<T>: marshal to the underlying type so bool? mirrors plain bool
        // (t→true, nil→false) and int?/etc. accept their value. nil maps to null
        // for non-bool nullables (no false analog); (dotnet:null) is the explicit
        // null for all. Without this, nil → Activator.CreateInstance(Nullable<T>)
        // = null, so bool? could never receive false.
        var underlyingType = Nullable.GetUnderlyingType(targetType);
        if (underlyingType != null)
        {
            if (arg is Nil && underlyingType != typeof(bool)) return null;
            return LispToDotNet(arg, underlyingType);
        }

        // Nil → null or false
        if (arg is Nil)
        {
            if (targetType == typeof(bool)) return false;
            if (!targetType.IsValueType) return null;
            return Activator.CreateInstance(targetType);
        }

        // T → true
        if (arg is T && targetType == typeof(bool))
            return true;

        // Fixnum → numeric types
        if (arg is Fixnum fx)
        {
            if (targetType == typeof(int)) return (int)fx.Value;
            if (targetType == typeof(long)) return fx.Value;
            if (targetType == typeof(double)) return (double)fx.Value;
            if (targetType == typeof(float)) return (float)fx.Value;
            if (targetType == typeof(short)) return (short)fx.Value;
            if (targetType == typeof(byte)) return (byte)fx.Value;
            // The remaining small integer types, symmetric with DotNetToLisp's read
            // side: without these a (setf (aref a i) n) into a sbyte[]/ushort[]/uint[]/…
            // (dotnet:make-array store) fails with "Cannot convert Fixnum to SByte".
            if (targetType == typeof(sbyte)) return (sbyte)fx.Value;
            if (targetType == typeof(ushort)) return (ushort)fx.Value;
            if (targetType == typeof(uint)) return (uint)fx.Value;
            if (targetType == typeof(ulong)) return (ulong)fx.Value;
            if (targetType == typeof(nint)) return (nint)fx.Value;
            if (targetType == typeof(nuint)) return (nuint)fx.Value;
            if (targetType == typeof(decimal)) return (decimal)fx.Value;
            if (targetType == typeof(object)) return fx.Value;
        }

        // DoubleFloat → double/float
        if (arg is DoubleFloat df)
        {
            if (targetType == typeof(double)) return df.Value;
            if (targetType == typeof(float)) return (float)df.Value;
            if (targetType == typeof(decimal)) return (decimal)df.Value;
            if (targetType == typeof(object)) return df.Value;
        }

        // SingleFloat → float/double
        if (arg is SingleFloat sf)
        {
            if (targetType == typeof(float)) return sf.Value;
            if (targetType == typeof(double)) return (double)sf.Value;
            if (targetType == typeof(decimal)) return (decimal)sf.Value;
            if (targetType == typeof(object)) return sf.Value;
        }

        // LispDecimal → decimal (exact) / float / double
        if (arg is LispDecimal ld)
        {
            if (targetType == typeof(decimal)) return ld.Value;
            if (targetType == typeof(double)) return (double)ld.Value;
            if (targetType == typeof(float)) return (float)ld.Value;
            if (targetType == typeof(object)) return ld.Value;
        }

        // Bignum / Ratio → decimal: exact-or-throw, so a computed CL real can be passed to
        // a decimal-typed .NET parameter without silent precision loss.
        if (arg is Bignum bnD && targetType == typeof(decimal))
        {
            if (bnD.Value < DecimalMinInt || bnD.Value > DecimalMaxInt)
                throw new LispErrorException(new LispTypeError(
                    "value out of System.Decimal range", arg, Startup.Sym("NUMBER")));
            return (decimal)bnD.Value;
        }
        if (arg is Ratio rtD && targetType == typeof(decimal))
            return RationalToDecimalExact(rtD.Numerator, rtD.Denominator, arg);

        // LispString → string
        if (arg is LispString ls)
        {
            if (targetType == typeof(string)) return ls.Value;
            if (targetType == typeof(object)) return ls.Value;
        }

        // Char-backed LispVector (BASE-STRING / fill-pointered string) → string.
        // CL strings have two runtime representations (LispString and char
        // LispVector); both must marshal to System.String for .NET interop.
        if (arg is LispVector cv && cv.IsCharVector)
        {
            if (targetType == typeof(string)) return cv.ToCharString();
            if (targetType == typeof(object)) return cv.ToCharString();
        }

        // LispFunction → delegate (Func<>, Action<>, EventHandler<>, etc.)
        if (arg is LispFunction fn && typeof(Delegate).IsAssignableFrom(targetType))
            return CreateLispDelegate(fn, targetType);

        // Enum target: accept an integer (underlying value) or a name. Names come
        // as a string/symbol/keyword and go through Enum.Parse (case-insensitive),
        // which also accepts flag combinations like "Tunnel, Bubble". Lets callers
        // pass RoutingStrategies / StringComparison etc. without first fetching the
        // enum field object. An actual wrapped enum value is handled above.
        if (targetType.IsEnum)
        {
            switch (arg)
            {
                case Fixnum efx: return Enum.ToObject(targetType, efx.Value);
                case LispString els: return Enum.Parse(targetType, els.Value, ignoreCase: true);
                case LispVector ecv when ecv.IsCharVector:
                    return Enum.Parse(targetType, ecv.ToCharString(), ignoreCase: true);
                case Symbol esym: return Enum.Parse(targetType, esym.Name, ignoreCase: true);
            }
        }

        // Lisp sequence (list or vector) → typed array T[]. Lets a Lisp list or
        // vector be passed where an array-typed parameter or property is expected
        // (e.g. set_FileTypeFilter with FilePickerFileType[], Patterns with
        // string[]). Each element is marshalled to the element type. An already
        // wrapped .NET array is handled by the LispDotNetObject branch above.
        if (targetType.IsArray && TryLispSequenceItems(arg, out var seqItems))
        {
            var elemType = targetType.GetElementType()!;
            var arr = System.Array.CreateInstance(elemType, seqItems.Count);
            for (int i = 0; i < seqItems.Count; i++)
                arr.SetValue(LispToDotNet(seqItems[i], elemType), i);
            return arr;
        }

        // Interface or base-class target (IComparable, IConvertible, IFormattable,
        // ValueType, …) that a primitive value's natural .NET type satisfies: box at
        // that natural type. Lets dotnet:box / a typed parameter accept e.g. an int
        // where IComparable is wanted. Concrete primitive targets (int,
        // double, string, …) are handled by the branches above.
        {
            object? natural = arg switch
            {
                Fixnum nfx      => nfx.Value,                // long
                DoubleFloat ndf => ndf.Value,                // double
                SingleFloat nsf => nsf.Value,                // float
                LispString nls  => nls.Value,                // string
                LispVector ncv when ncv.IsCharVector => ncv.ToCharString(),
                _ => null
            };
            if (natural != null && targetType.IsAssignableFrom(natural.GetType()))
                return natural;
            // Integers also satisfy 32-bit-specific targets (e.g. IComparable<int>).
            if (arg is Fixnum gfx && targetType.IsAssignableFrom(typeof(int)))
                return (int)gfx.Value;
        }

        // Fallback: pass as object
        if (targetType == typeof(object)) return arg;

        throw new LispErrorException(new LispTypeError(
            $"Cannot convert {arg.GetType().Name} to {targetType.Name}", arg));
    }

    /// <summary>Collect the elements of a Lisp proper list or vector (non-char)
    /// into a flat list. Returns false for anything that isn't a sequence we
    /// marshal to a .NET array (e.g. a string, which is a char vector).</summary>
    private static bool TryLispSequenceItems(LispObject arg, out List<LispObject> items)
    {
        items = new List<LispObject>();
        switch (arg)
        {
            case Nil:
                return true;
            case Cons:
                var cur = arg;
                while (cur is Cons c) { items.Add(c.Car); cur = c.Cdr; }
                return cur is Nil; // proper list only
            case LispVector v when !v.IsCharVector:
                for (int i = 0; i < v.Length; i++) items.Add(v.GetElement(i));
                return true;
            default:
                return false;
        }
    }

    /// <summary>Resolve a Lisp value naming a .NET type: a type-name string,
    /// a symbol, or an already-wrapped System.Type.</summary>
    internal static Type ResolveElementTypeArg(LispObject arg)
    {
        if (arg is LispDotNetObject dno && dno.Value is Type t) return t;
        string typeName = arg switch { LispString ls => ls.Value, _ => arg.ToString() ?? "" };
        return ResolveDotNetType(typeName);
    }

    /// <summary>(dotnet:new-array element-type &rest elements) => T[]
    /// Create a typed .NET array of element-type, filled with the marshalled
    /// elements. element-type is a type-name string/symbol or a resolved
    /// System.Type. e.g. (dotnet:new-array "System.String" "a" "b" "c").
    /// To build from a Lisp list: (apply #'dotnet:new-array "System.String" lst).</summary>
    public static LispObject DotNetNewArray(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:NEW-ARRAY: requires at least 1 argument (element-type)"));
        var elemType = ResolveElementTypeArg(args[0]);
        int n = args.Length - 1;
        var arr = System.Array.CreateInstance(elemType, n);
        for (int i = 0; i < n; i++)
            arr.SetValue(LispToDotNet(args[i + 1], elemType), i);
        return new LispDotNetObject(arr);
    }

    /// <summary>(dotnet:make-array element-type &rest dimensions) => array
    /// Create a typed .NET array of ELEMENT-TYPE with the given DIMENSION sizes,
    /// filled with the element type's default value. One dimension yields a 1-D
    /// array, several yield a multi-dimensional array (Array.CreateInstance).
    /// element-type is a type-name string/symbol or a resolved System.Type.
    /// e.g. (dotnet:make-array "System.Int32" 100) / (dotnet:make-array "System.Single" 10 20).
    /// Read/write elements with (dotnet:invoke arr "get_Item"/"set_Item" idx... [val]).
    /// (dotcl/dotcl#45)</summary>
    public static LispObject DotNetMakeArray(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:MAKE-ARRAY: requires element-type and at least one dimension"));
        var elemType = ResolveElementTypeArg(args[0]);
        var dims = new int[args.Length - 1];
        for (int i = 0; i < dims.Length; i++)
        {
            if (args[i + 1] is Fixnum fi && fi.Value >= 0)
                dims[i] = (int)fi.Value;
            else
                throw new LispErrorException(new LispTypeError(
                    "DOTNET:MAKE-ARRAY: dimension must be a non-negative fixnum", args[i + 1],
                    Startup.Sym("UNSIGNED-BYTE")));
        }
        var arr = System.Array.CreateInstance(elemType, dims);
        return new LispDotNetObject(arr);
    }

    public static LispObject DotNetLoadAssembly(LispObject[] args)
    {
        if (args.Length != 1)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:LOAD-ASSEMBLY: wrong number of arguments: " + args.Length + " (expected 1)"));

        string path = args[0] switch
        {
            LispString ls => ls.Value,
            _ => args[0].ToString() ?? ""
        };

        // If no path separators and no .dll extension, treat as assembly name.
        // Try Assembly.Load first (base runtime), then search shared framework dirs
        // so e.g. "System.Windows.Forms" finds Microsoft.WindowsDesktop.App.
        if (!path.Contains('/') && !path.Contains('\\')
            && !path.EndsWith(".dll", StringComparison.OrdinalIgnoreCase))
        {
            try
            {
                var byName = System.Reflection.Assembly.Load(path);
                return new LispString(byName.FullName ?? path);
            }
            catch { }

            var dllPath = FindSharedFrameworkDll(path);
            if (dllPath != null)
            {
                var byPath = System.Reflection.Assembly.LoadFrom(dllPath);
                return new LispString(byPath.FullName ?? path);
            }

            throw new LispErrorException(new LispProgramError(
                $"DOTNET:LOAD-ASSEMBLY: assembly not found: {path}"));
        }

        // LoadFrom (not Load(bytes)) so transitive references in the same
        // directory resolve automatically. Required for contribs that ship
        // their own lib/ directory with multiple interdependent DLLs (e.g.
        // dotcl-cs loading Roslyn).
        var asm = System.Reflection.Assembly.LoadFrom(System.IO.Path.GetFullPath(path));
        return new LispString(asm.FullName ?? path);
    }

    // Registerable resolver tables, populated from Lisp (e.g. the nuget contrib
    // that reads project.assets.json after `dotnet restore`). The Default ALC's
    // Resolving / ResolvingUnmanagedDll hooks (Startup.cs) consult these so a managed
    // assembly resolves to the exact version-unified path and a native library resolves
    // to its RID-specific path — without baking any NuGet logic into the runtime.
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<string, string>
        _registeredAssemblyPaths = new(StringComparer.OrdinalIgnoreCase);
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<string, string>
        _registeredNativePaths = new(StringComparer.OrdinalIgnoreCase);

    /// <summary>Register managed assembly simple-name → absolute .dll path (dotcl:register-assembly-path).</summary>
    internal static void RegisterAssemblyPath(string name, string path) => _registeredAssemblyPaths[name] = path;
    /// <summary>Register native library name → absolute path (dotcl:register-native-path).</summary>
    internal static void RegisterNativePath(string name, string path) => _registeredNativePaths[name] = path;
    /// <summary>Look up a registered managed assembly path by simple name, or null.</summary>
    internal static string? FindRegisteredAssembly(string name) =>
        _registeredAssemblyPaths.TryGetValue(name, out var p) ? p : null;
    /// <summary>Look up a registered native library path by name, or null.</summary>
    internal static string? FindRegisteredNative(string name) =>
        _registeredNativePaths.TryGetValue(name, out var p) ? p : null;

    // ----- resolve-type memoization + base-directory probe -----------------
    // resolve-type memoizes name -> Type so repeated lookups skip the
    // GetAssemblies scan. The cache is invalidated wholesale whenever the set
    // of loaded assemblies changes (AssemblyLoad fires MarkTypeCacheDirty), so
    // a module reload re-resolves every name once on next use rather than
    // returning a stale Type. Only successful resolutions of stable .NET
    // assembly types are cached — failures and dynamically-defined Lisp types
    // are never cached (a later load / redefinition must be able to change the
    // answer). Cleared strong refs also let a collectible ALC actually unload.
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<string, Type>
        _typeCache = new(StringComparer.Ordinal);
    private static volatile bool _typeCacheDirty;
    // Whether the base-dir probe has already run since the last invalidation.
    // Bounds probing to once per assembly-set change: a name that stays
    // unresolved does not re-scan the directory on every miss.
    private static bool _probedSinceDirty;
    // Reentrancy guard: the probe's own LoadFromAssemblyPath calls fire
    // AssemblyLoad; without this the probe would mark its own cache dirty and
    // thrash. Thread-static because the AssemblyLoad handler runs synchronously
    // on the loading (== resolving) thread.
    [ThreadStatic] private static bool _inTypeResolve;

    /// <summary>Invalidate the resolve-type cache on the next resolution. Called
    /// from the AssemblyLoad hook (Startup) — a new assembly may change what a
    /// name resolves to. No-op while a resolution (incl. its probe) is in flight
    /// so the probe does not invalidate itself.</summary>
    internal static void MarkTypeCacheDirty()
    {
        if (!_inTypeResolve) _typeCacheDirty = true;
    }

    /// <summary>DOTNET:CLEAR-TYPE-CACHE — drop all memoized resolve-type entries.
    /// The next resolve-type re-resolves against the current assembly set.</summary>
    internal static void ClearTypeCache()
    {
        _typeCache.Clear();
        _probedSinceDirty = false;
    }

    /// <summary>Eagerly load every managed assembly sitting in the app base
    /// directory so resolve-type's GetAssemblies scan can see PackageReference
    /// types whose assembly simple-name differs from the type's namespace (so
    /// the namespace-prefix Assembly.Load never finds them). Best-effort; run at
    /// most once per assembly-set change (see _probedSinceDirty).</summary>
    private static void ProbeLoadBaseDir()
    {
        string dir;
        try { dir = AppContext.BaseDirectory; }
        catch { return; }
        if (string.IsNullOrEmpty(dir) || !System.IO.Directory.Exists(dir)) return;
        foreach (var dll in System.IO.Directory.EnumerateFiles(dir, "*.dll"))
        {
            try
            {
#if NETSTANDARD2_0
                System.Reflection.Assembly.LoadFrom(System.IO.Path.GetFullPath(dll));
#else
                System.Runtime.Loader.AssemblyLoadContext.Default
                    .LoadFromAssemblyPath(System.IO.Path.GetFullPath(dll));
#endif
            }
            catch { /* not a managed assembly, or already loaded — ignore */ }
        }
    }

    internal static string? FindSharedFrameworkDll(string assemblyName)
    {
        // RuntimeEnvironment.GetRuntimeDirectory() returns e.g.
        // C:\Program Files\dotnet\shared\Microsoft.NETCore.App\10.0.5\
        // Go up two levels to reach the dotnet root's shared/ directory.
        var runtimeDir = System.Runtime.InteropServices.RuntimeEnvironment
                             .GetRuntimeDirectory();
        var sharedDir = System.IO.Path.GetDirectoryName(
                            System.IO.Path.GetDirectoryName(runtimeDir.TrimEnd('/', '\\')));
        if (sharedDir == null || !System.IO.Directory.Exists(sharedDir)) return null;

        foreach (var fwDir in System.IO.Directory.GetDirectories(sharedDir))
        foreach (var verDir in System.IO.Directory.GetDirectories(fwDir)
                                    .OrderByDescending(d => d))
        {
            var dll = System.IO.Path.Combine(verDir, assemblyName + ".dll");
            if (System.IO.File.Exists(dll)) return dll;
        }
        return null;
    }

    /// <summary>DOTNET:RESOLVE-TYPE — resolve a .NET System.Type by name (searching loaded
    /// assemblies, loading by namespace prefix, and COM ProgIDs on Windows), returning it
    /// wrapped as a .NET object. The result can be inspected or passed anywhere a
    /// System.Type is expected (it unwraps to the Type). Signals an error if not found.
    /// Exposes the previously-internal ResolveDotNetType (dotcl/dotcl#17).</summary>
    public static LispObject DotNetResolveType(LispObject[] args)
    {
        if (args.Length != 1)
            throw new LispErrorException(new LispProgramError(
                $"DOTNET:RESOLVE-TYPE: expected 1 argument, got {args.Length}"));
        if (args[0] is not LispString name)
            throw new LispErrorException(new LispTypeError(
                "DOTNET:RESOLVE-TYPE: type name must be a string", args[0]));
        return new LispDotNetObject(ResolveDotNetType(name.Value));
    }

    /// <summary>DOTNET:CLEAR-TYPE-CACHE — drop all memoized resolve-type entries so
    /// the next resolution re-searches the current assembly set. Returns T.</summary>
    public static LispObject DotNetClearTypeCache(LispObject[] args)
    {
        ClearTypeCache();
        return T.Instance;
    }

    /// <summary>DOTNET:CLASS-FOR-TYPE — return the CLOS class DotCL uses (registering
    /// it lazily on first call) for a .NET type, so user code can obtain a specializer
    /// class object without hand-spelling a class symbol — which is especially awkward
    /// for closed generics, whose auto-derived name is a long assembly-qualified string.
    /// The argument is a System.Type (as from dotnet:resolve-type / dotnet:make-generic-type)
    /// or a type-name string/symbol. The returned class object is usable directly as a
    /// defmethod specializer via read-time #. (like SBCL/CCL accept a class object).
    /// (dotcl/dotcl#50)</summary>
    public static LispObject DotNetClassForType(LispObject[] args)
    {
        if (args.Length != 1)
            throw new LispErrorException(new LispProgramError(
                $"DOTNET:CLASS-FOR-TYPE: expected 1 argument, got {args.Length}"));
        var type = ResolveElementTypeArg(args[0]);
        return EnsureDotNetTypeClass(type);
    }

    /// <summary>Resolve a .NET type by full name, searching loaded assemblies.
    /// Falls back to COM ProgID lookup (Windows only) for names like "Excel.Application".</summary>
    /// <summary>Non-throwing variant of ResolveDotNetType: returns null instead of
    /// signalling when the type can't be resolved. Used where a resolution attempt
    /// is speculative (e.g. trying a name with and without a backtick-arity suffix).</summary>
    internal static Type? TryResolveDotNetType(string typeName)
    {
        try { return ResolveDotNetType(typeName); }
        catch { return null; }
    }

    internal static Type ResolveDotNetType(string typeName)
    {
        // Outermost call owns the dirty-flag check so the probe's own loads
        // (which re-enter via AssemblyLoad, not via this method) don't clear the
        // cache mid-resolution.
        bool outer = !_inTypeResolve;
        if (outer)
        {
            _inTypeResolve = true;
            if (_typeCacheDirty) { ClearTypeCache(); _typeCacheDirty = false; }
        }
        try
        {
            if (_typeCache.TryGetValue(typeName, out var cached)) return cached;
            var t = SearchDotNetType(typeName, out var cacheable);
            if (t == null && !_probedSinceDirty)
            {
                // Miss: a PackageReference type whose assembly is present in the
                // app base dir but not yet loaded. Probe once, then retry.
                _probedSinceDirty = true;
                ProbeLoadBaseDir();
                t = SearchDotNetType(typeName, out cacheable);
            }
            if (t == null)
                throw new LispErrorException(new LispError($"DOTNET: type not found: {typeName}"));
            if (cacheable) _typeCache[typeName] = t;
            return t;
        }
        finally { if (outer) _inTypeResolve = false; }
    }

    /// <summary>The actual type search: loaded assemblies, namespace-prefix load,
    /// mscorlib/netstandard facades, COM ProgID, then dynamically-defined Lisp
    /// types. Returns null (not throwing) on miss. CACHEABLE is false for a
    /// dynamic Lisp type hit — those can be redefined, so they must not be
    /// memoized.</summary>
    private static Type? SearchDotNetType(string typeName, out bool cacheable)
    {
        cacheable = true;
        var type = Type.GetType(typeName);
        if (type != null) return type;

        // Strip ", AssemblyName" suffix before searching loaded assemblies with GetType(),
        // which only accepts unqualified type names.
        string bareTypeName = typeName;
        int commaIdx = typeName.IndexOf(',');
        if (commaIdx > 0) bareTypeName = typeName[..commaIdx].Trim();

        foreach (var asm in AppDomain.CurrentDomain.GetAssemblies())
        {
            type = asm.GetType(bareTypeName);
            if (type != null) return type;
        }

        // Try to load the assembly based on namespace prefix
        // e.g., "System.Net.IPAddress" → try loading "System.Net" assembly
        var parts = bareTypeName.Split('.');
        for (int len = parts.Length - 1; len >= 1; len--)
        {
            var asmName = string.Join(".", parts, 0, len);
            try
            {
                var asm = System.Reflection.Assembly.Load(asmName);
                type = asm.GetType(bareTypeName);
                if (type != null) return type;
            }
            catch { }
        }

        // BCL / framework types whose assembly is not yet loaded and whose name
        // doesn't match its containing assembly (so the namespace-prefix loop
        // above misses it). The mscorlib / netstandard facades type-forward most
        // of the BCL surface, so resolving through them triggers the real
        // assembly load. e.g. "System.Collections.Queue" actually lives in
        // System.Collections.NonGeneric — resolvable as "...Queue, mscorlib".
        // Crucial: this must run BEFORE the COM ProgID fallback, because some
        // legacy types (System.Collections.Queue, ArrayList, ...) are ALSO
        // registered as .NET Framework COM components (mscoree.dll). Activating
        // those via COM throws an uncatchable "Failed to load the runtime" and
        // crashes the process.
        foreach (var facade in new[] { "mscorlib", "netstandard" })
        {
            try
            {
                type = Type.GetType($"{bareTypeName}, {facade}");
                if (type != null) return type;
            }
            catch { }
        }

        // COM ProgID fallback for genuine ProgIDs (e.g. "Excel.Application",
        // "Schedule.Service"). On non-Windows GetTypeFromProgID returns
        // null (does not throw in modern .NET). Skip managed framework
        // namespaces: they never name a wanted COM component, and "System.*"
        // collides with legacy .NET Framework COM registrations that crash on
        // activation.
        if (!bareTypeName.StartsWith("System.", StringComparison.Ordinal))
        {
            try
            {
                var comType = Type.GetTypeFromProgID(typeName);
                if (comType != null) return comType;
            }
            catch { }
        }

        // Fallback: case-insensitive lookup in dynamically-defined types (e.g. Lisp symbol
        // 'Animal uppercased to "ANIMAL" by reader, but dynamic type is named "Animal").
        if (_dotNetDynTypeByUpperName.TryGetValue(typeName, out var dynType))
        {
            cacheable = false;   // dynamic Lisp types can be redefined
            return dynType;
        }

        return null;
    }

    /// <summary>
    /// Compile-time helper for typed-return inference. Given a
    /// receiver type name, an instance method name, and a list of parameter-type
    /// name strings, resolve the method's static return type and return its FullName
    /// as a LispString — but ONLY when a value of that static type is guaranteed to
    /// marshal back (DotNetToLisp) to a LispDotNetObject, so it can serve as the
    /// receiver of a subsequent typed direct callvirt. Returns NIL for void, an
    /// unresolvable type / overload, a natively-marshaled primitive or string, or
    /// any slot type (object / ValueType / Enum / an interface) whose runtime value
    /// could itself be a string or boxed primitive (which would NOT be a
    /// LispDotNetObject). The compiler macro uses this to lower a method chain
    /// (dotnet:invoke (dotnet:invoke r "Inner" ...) "Outer" ...) without an explicit
    /// THE on the inner result. Args: (typeName methodName paramTypeList).
    /// </summary>
    public static LispObject DotNetMethodReturnType(LispObject[] args)
    {
        if (args.Length < 2) return Nil.Instance;
        string? typeName = (args[0] as LispString)?.Value;
        string? methodName = (args[1] as LispString)?.Value;
        if (typeName == null || methodName == null) return Nil.Instance;

        var paramTypeNames = new System.Collections.Generic.List<string>();
        if (args.Length >= 3)
            for (var cur = args[2]; cur is Cons cc; cur = cc.Cdr)
            {
                if (cc.Car is LispString ps) paramTypeNames.Add(ps.Value);
                else return Nil.Instance;
            }

        Type t;
        try { t = ResolveDotNetType(typeName); } catch { return Nil.Instance; }
        var paramTypes = new Type[paramTypeNames.Count];
        for (int i = 0; i < paramTypes.Length; i++)
        {
            try { paramTypes[i] = ResolveDotNetType(paramTypeNames[i]); }
            catch { return Nil.Instance; }
        }

        System.Reflection.MethodInfo? m;
        try { m = t.GetMethod(methodName, paramTypes); }
        catch { return Nil.Instance; }          // ambiguous overload, etc.
        if (m == null) return Nil.Instance;

        var rt = m.ReturnType;
        if (!IsDirectableReturnType(rt) || rt.FullName == null) return Nil.Instance;
        return new LispString(rt.FullName);
    }

    /// <summary>
    /// True when EVERY runtime value of static type RT marshals (DotNetToLisp) to a
    /// LispDotNetObject — the precondition for using such a value as a typed direct
    /// callvirt receiver. Value types are exact (sealed), so any non-primitive
    /// struct / enum qualifies. Reference types are polymorphic: a slot typed as
    /// object / ValueType / Enum / an interface could hold a string or boxed
    /// primitive (which marshal to LispString / Fixnum / …), so those are excluded;
    /// any other concrete or abstract class is safe because string and the boxed
    /// primitives are not assignable to it.
    /// </summary>
    private static bool IsDirectableReturnType(Type rt)
    {
        if (rt == typeof(void)) return false;
        if (rt.IsByRef || rt.IsPointer || rt.IsGenericParameter) return false;
        if (rt.IsInterface) return false;
        if (rt == typeof(object) || rt == typeof(ValueType) || rt == typeof(Enum))
            return false;
        if (rt == typeof(int) || rt == typeof(long) || rt == typeof(double)
            || rt == typeof(float) || rt == typeof(string) || rt == typeof(char)
            || rt == typeof(bool)) return false;
        return true;
    }

    /// <summary>Best-fit conversion when target parameter type is unknown
    /// (InvokeMember path). Default Binder picks the overload from these
    /// runtime types.</summary>
    internal static object? LispToDotNetGeneric(LispObject arg)
    {
        return arg switch
        {
            LispDotNetBoxed b => b.Value,
            LispDotNetObject d => d.Value,
            Nil => null,
            T => true,
            Fixnum fx => (fx.Value >= int.MinValue && fx.Value <= int.MaxValue)
                            ? (object)(int)fx.Value : fx.Value,
            DoubleFloat df => df.Value,
            SingleFloat sf => sf.Value,
            LispString ls => ls.Value,
            // Char-backed LispVector (BASE-STRING / fill-pointered string): CL
            // strings have two representations; marshal both to System.String so
            // overload resolution (e.g. Graphics.DrawString(string,...)) binds.
            LispVector cv when cv.IsCharVector => cv.ToCharString(),
            _ => arg
        };
    }

    private static object?[] LispArgsToDotNetGeneric(LispObject[] lispArgs)
    {
        var result = new object?[lispArgs.Length];
        for (int i = 0; i < lispArgs.Length; i++)
            result[i] = LispToDotNetGeneric(lispArgs[i]);
        return result;
    }

    private const System.Reflection.BindingFlags InstanceReadFlags =
        System.Reflection.BindingFlags.Public
        | System.Reflection.BindingFlags.Instance
        | System.Reflection.BindingFlags.InvokeMethod
        | System.Reflection.BindingFlags.GetProperty
        | System.Reflection.BindingFlags.GetField;

    private const System.Reflection.BindingFlags InstanceWriteFlags =
        System.Reflection.BindingFlags.Public
        | System.Reflection.BindingFlags.Instance
        | System.Reflection.BindingFlags.SetProperty
        | System.Reflection.BindingFlags.SetField;

    private const System.Reflection.BindingFlags StaticReadFlags =
        System.Reflection.BindingFlags.Public
        | System.Reflection.BindingFlags.Static
        | System.Reflection.BindingFlags.InvokeMethod
        | System.Reflection.BindingFlags.GetProperty
        | System.Reflection.BindingFlags.GetField;

    private const System.Reflection.BindingFlags StaticWriteFlags =
        System.Reflection.BindingFlags.Public
        | System.Reflection.BindingFlags.Static
        | System.Reflection.BindingFlags.SetProperty
        | System.Reflection.BindingFlags.SetField;

    // ── dotnet:invoke / dotnet:static method-resolution cache ────────────────────
    // Type.InvokeMember re-runs member-name lookup + default-Binder overload
    // resolution on every call. Cache the resolved MethodInfo keyed by (runtime
    // type OBJECT, member name, arg runtime types) — exactly the inputs the binder
    // uses — so a hot interop loop pays resolution only once and then goes straight
    // to MethodInfo.Invoke (which .NET 8+ backs with a cached fast invoker stub).
    // Pure reflection, no Reflection.Emit, so the fast path is AOT/IL2CPP-safe.
    //
    // Only plain fixed-arity method calls are cached. COM/IDispatch targets (one
    // shared __ComObject type, per-object member set), params / by-ref methods,
    // generic definitions, null args, and property/field access all fall through to
    // the unchanged InvokeMember path — the cache can never alter overload
    // resolution, COM dispatch, or the #24 optional-argument fallback.
    //
    // Keying on the Type OBJECT (not its name) makes class redefinition safe: a
    // redefined type is a new object = a new key, so old entries are never served.
    // A RuntimeType's member set is otherwise immutable, so entries don't go stale
    // (the one runtime-mutation path, Hot Reload via MetadataUpdater.ApplyUpdate, is
    // dev-only; wire a MetadataUpdateHandler to flush this if it ever matters).
    private readonly struct InvokeKey : IEquatable<InvokeKey>
    {
        public readonly Type Owner;
        public readonly string Name;
        public readonly Type[] ArgTypes;
        public InvokeKey(Type owner, string name, Type[] argTypes)
        { Owner = owner; Name = name; ArgTypes = argTypes; }

        public bool Equals(InvokeKey o)
        {
            if (!ReferenceEquals(Owner, o.Owner) || Name != o.Name
                || ArgTypes.Length != o.ArgTypes.Length) return false;
            for (int i = 0; i < ArgTypes.Length; i++)
                if (!ReferenceEquals(ArgTypes[i], o.ArgTypes[i])) return false;
            return true;
        }
        public override bool Equals(object? o) => o is InvokeKey k && Equals(k);
        public override int GetHashCode()
        {
            var h = new HashCode();
            h.Add(Owner);
            h.Add(Name);
            for (int i = 0; i < ArgTypes.Length; i++) h.Add(ArgTypes[i]);
            return h.ToHashCode();
        }
    }

    // null value = "this signature is not cacheable; always use InvokeMember".
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<InvokeKey, System.Reflection.MethodInfo?>
        _invokeMethodCache = new();

    /// <summary>Fast path for dotnet:invoke / dotnet:static plain method calls:
    /// resolve the MethodInfo once (cached) then invoke it directly, skipping
    /// InvokeMember's per-call name lookup and overload resolution. Returns false
    /// (caller falls back to InvokeMember) for anything not safe to cache. A
    /// TargetInvocationException from the invoked method propagates unchanged so the
    /// caller's existing handler wraps it identically.</summary>
    private static bool TryCachedInvoke(
        Type type, string name, object? target, object?[] callArgs, bool isStatic, out object? result)
    {
        result = null;

        // COM IDispatch: every __ComObject shares one Type but resolves members
        // per-object. Caching by Type would serve another object's dispatch.
        if (type.IsCOMObject) return false;

        // A null arg has no runtime type → can't key it or overload-resolve it; let
        // InvokeMember's binder handle null matching.
        var argTypes = new Type[callArgs.Length];
        for (int i = 0; i < callArgs.Length; i++)
        {
            if (callArgs[i] is null) return false;
            argTypes[i] = callArgs[i]!.GetType();
        }

        var key = new InvokeKey(type, name, argTypes);
        if (!_invokeMethodCache.TryGetValue(key, out var method))
        {
            method = ResolveCacheableMethod(type, name, argTypes, isStatic);
            _invokeMethodCache[key] = method;
        }
        if (method is null) return false;

        result = method.Invoke(target, callArgs);
        return true;
    }

    /// <summary>Pick the method InvokeMember's default binder would select, but only
    /// when it is a plain fixed-arity method (no params / by-ref params, not a
    /// generic definition). Returns null otherwise so the caller keeps the
    /// InvokeMember path (which also handles property/field access and #24 optional
    /// defaults).</summary>
    private static System.Reflection.MethodInfo? ResolveCacheableMethod(
        Type type, string name, Type[] argTypes, bool isStatic)
    {
        var flags = System.Reflection.BindingFlags.Public
            | (isStatic ? System.Reflection.BindingFlags.Static : System.Reflection.BindingFlags.Instance);

        System.Collections.Generic.List<System.Reflection.MethodBase>? candidates = null;
        foreach (var m in type.GetMethods(flags))
        {
            if (m.Name != name || m.IsGenericMethodDefinition) continue;
            var ps = m.GetParameters();
            if (ps.Length != argTypes.Length) continue;          // omitted optionals -> InvokeMember
            bool ok = true;
            foreach (var p in ps)
                if (p.ParameterType.IsByRef
                    || System.Attribute.IsDefined(p, typeof(ParamArrayAttribute)))
                { ok = false; break; }                            // ref/out and params -> InvokeMember
            if (!ok) continue;
            (candidates ??= new System.Collections.Generic.List<System.Reflection.MethodBase>()).Add(m);
        }
        if (candidates is null) return null;

        try
        {
            var selected = (System.Reflection.MethodInfo?)Type.DefaultBinder.SelectMethod(
                flags, candidates.ToArray(), argTypes, null);
            // Don't cache a catch-all (object, ...) overload when a more-specific
            // sibling of the same arity exists: the binder reached the object
            // overload only by boxing an integer/pointer arg (e.g. it picks
            // Marshal.ReadIntPtr(object,int) over (IntPtr,int) because Int64 boxes
            // to object but has no implicit conversion to IntPtr). Defer so the
            // specificity-aware path (TryInvokeMostSpecificOverload) selects the
            // typed overload instead of reading/writing a boxed value.
            if (selected != null)
            {
                int selObj = selected.GetParameters().Count(p => p.ParameterType == typeof(object));
                if (selObj > 0 && candidates.Any(c =>
                        !ReferenceEquals(c, selected) &&
                        c.GetParameters().Count(p => p.ParameterType == typeof(object)) < selObj))
                    return null;
            }
            return selected;
        }
        catch (System.Reflection.AmbiguousMatchException) { return null; }
    }

    /// <summary>Fallback for dotnet:invoke / dotnet:static when InvokeMember finds no
    /// matching overload: locate a method NAME whose first parameters take the supplied
    /// args and whose remaining (trailing) parameters are all C# optional, then fill those
    /// with their declared default values. Lets callers omit defaulted parameters
    /// (e.g. SpriteBatch.Begin()) (dotcl/dotcl#24). Returns true and sets RESULT on success.</summary>
    private static bool TryInvokeWithOptionalDefaults(
        Type type, string name, object? target, LispObject[] lispArgs, bool isStatic, out object? result)
    {
        result = null;
        var flags = System.Reflection.BindingFlags.Public
            | (isStatic ? System.Reflection.BindingFlags.Static : System.Reflection.BindingFlags.Instance);
        System.Reflection.MethodInfo? best = null;
        int bestLen = int.MaxValue;
        foreach (var m in type.GetMethods(flags))
        {
            if (m.Name != name || m.IsGenericMethodDefinition) continue;
            var ps = m.GetParameters();
            if (ps.Length <= lispArgs.Length) continue;           // need omitted optional params
            bool tailOptional = true;
            for (int i = lispArgs.Length; i < ps.Length; i++)
                if (!ps[i].IsOptional) { tailOptional = false; break; }
            if (!tailOptional) continue;
            if (ps.Length < bestLen) { best = m; bestLen = ps.Length; }   // closest match
        }
        if (best == null) return false;

        var bps = best.GetParameters();
        var full = new object?[bps.Length];
        try
        {
            for (int i = 0; i < lispArgs.Length; i++)
                full[i] = LispToDotNet(lispArgs[i], bps[i].ParameterType);
        }
        catch { return false; }   // supplied args not convertible to this overload
        for (int i = lispArgs.Length; i < bps.Length; i++)
            full[i] = bps[i].HasDefaultValue ? bps[i].DefaultValue : Type.Missing;

        try { result = best.Invoke(target, full); return true; }
        catch (System.Reflection.TargetInvocationException tie)
        {
            throw DotNetInvokeError($"DOTNET:{(isStatic ? "STATIC" : "INVOKE")} {type.Name}.{name}", tie);
        }
    }

    /// <summary>Pre-empt InvokeMember for the one case its default binder gets
    /// wrong: when a same-arity catch-all <c>(object, ...)</c> overload coexists
    /// with a fully-typed one, InvokeMember may box an integer/pointer arg to the
    /// object overload — e.g. <c>Marshal.WriteIntPtr(object,int,IntPtr)</c> gets
    /// picked over <c>(IntPtr,int,IntPtr)</c> and writes into a boxed value
    /// instead of native memory (corrupting char** builds; dotcl/dotcl FFI
    /// regression). Fires ONLY when an object-param overload is present AND a
    /// 0-object-param overload exists that every arg marshals to; otherwise
    /// returns false and the normal InvokeMember path runs unchanged. Runs after
    /// the cache miss, so well-typed cacheable calls never reach it.</summary>
    private static bool TryInvokeMostSpecificOverload(
        Type type, string name, object? target, LispObject[] lispArgs, bool isStatic, out object? result)
    {
        result = null;
        var flags = System.Reflection.BindingFlags.Public
            | (isStatic ? System.Reflection.BindingFlags.Static : System.Reflection.BindingFlags.Instance);
        List<System.Reflection.MethodInfo> cands = new();
        bool sawObjectOverload = false;
        foreach (var m in type.GetMethods(flags))
        {
            if (m.Name != name || m.IsGenericMethodDefinition) continue;
            var ps = m.GetParameters();
            if (ps.Length != lispArgs.Length) continue;
            bool fixedArity = true;
            bool hasObjectParam = false;
            foreach (var p in ps)
            {
                if (p.ParameterType.IsByRef || System.Attribute.IsDefined(p, typeof(ParamArrayAttribute)))
                { fixedArity = false; break; }
                if (p.ParameterType == typeof(object)) hasObjectParam = true;
            }
            if (!fixedArity) continue;
            if (hasObjectParam) sawObjectOverload = true;
            cands.Add(m);
        }
        // Only intervene in the catch-all-ambiguity case; leave everything else to
        // the binder so this stays a narrow, low-risk correction.
        if (!sawObjectOverload || cands.Count < 2) return false;

        foreach (var m in cands.OrderBy(mi => mi.GetParameters().Count(p => p.ParameterType == typeof(object))))
        {
            // Consider only fully-typed overloads; if none convert, defer to the
            // normal path rather than forcing an object overload here.
            if (m.GetParameters().Any(p => p.ParameterType == typeof(object))) break;
            var ps = m.GetParameters();
            var converted = new object?[ps.Length];
            bool ok = true;
            for (int i = 0; i < ps.Length; i++)
            {
                try { converted[i] = LispToDotNet(lispArgs[i], ps[i].ParameterType); }
                catch { ok = false; break; }
            }
            if (!ok) continue;
            try { result = m.Invoke(target, converted); return true; }
            catch (System.Reflection.TargetInvocationException tie)
            {
                throw DotNetInvokeError($"DOTNET:{(isStatic ? "STATIC" : "INVOKE")} {type.Name}.{name}", tie);
            }
            catch (ArgumentException) { /* not this overload after all — try next typed one */ }
        }
        return false;
    }

    /// <summary>Fallback for dotnet:invoke / dotnet:static when InvokeMember's
    /// default binder finds no overload, because it matches on the args' Lisp
    /// runtime types and can't see conversions like Lisp-list → T[] or
    /// Lisp-fn → delegate. Finds a same-name, same-arity, fixed (no ref/params)
    /// method whose declared parameter types every supplied arg marshals to via
    /// LispToDotNet, and invokes it. Purely additive: only runs after the binder
    /// has already failed. Prefers candidates with an array parameter so the
    /// list→T[] case is deterministic. Returns true and sets RESULT on success.</summary>
    private static bool TryInvokeWithMarshalledArgs(
        Type type, string name, object? target, LispObject[] lispArgs, bool isStatic, out object? result)
    {
        result = null;
        var flags = System.Reflection.BindingFlags.Public
            | (isStatic ? System.Reflection.BindingFlags.Static : System.Reflection.BindingFlags.Instance);
        var candidates = new List<System.Reflection.MethodInfo>();
        foreach (var m in type.GetMethods(flags))
        {
            if (m.Name != name || m.IsGenericMethodDefinition) continue;
            var ps = m.GetParameters();
            if (ps.Length != lispArgs.Length) continue;
            bool ok = true;
            foreach (var p in ps)
                if (p.ParameterType.IsByRef
                    || System.Attribute.IsDefined(p, typeof(ParamArrayAttribute)))
                { ok = false; break; }
            if (ok) candidates.Add(m);
        }
        // Order candidates by specificity so the fallback picks the intended
        // overload, not a catch-all:
        //   1. fewer System.Object parameters first — an obsolete (object, ...)
        //      overload must lose to the typed one. e.g. Marshal.WriteIntPtr has
        //      both (IntPtr,int,IntPtr) and (object,int,IntPtr); since a Lisp
        //      integer marshals to object just as readily as to IntPtr, the object
        //      overload would otherwise win by iteration order and write into a
        //      boxed value instead of native memory (corrupting char** builds).
        //   2. then prefer an array parameter (the list -> T[] case this enables).
        int objParamCount(System.Reflection.MethodInfo m) =>
            m.GetParameters().Count(p => p.ParameterType == typeof(object));
        bool hasArrayParam(System.Reflection.MethodInfo m) =>
            m.GetParameters().Any(p => p.ParameterType.IsArray);
        candidates.Sort((a, b) =>
        {
            int byObj = objParamCount(a) - objParamCount(b);
            if (byObj != 0) return byObj;
            return (hasArrayParam(b) ? 1 : 0) - (hasArrayParam(a) ? 1 : 0);
        });

        foreach (var m in candidates)
        {
            var ps = m.GetParameters();
            var converted = new object?[ps.Length];
            bool converall = true;
            for (int i = 0; i < ps.Length; i++)
            {
                try { converted[i] = LispToDotNet(lispArgs[i], ps[i].ParameterType); }
                catch { converall = false; break; }
            }
            if (!converall) continue;
            try { result = m.Invoke(target, converted); return true; }
            catch (System.Reflection.TargetInvocationException tie)
            {
                throw DotNetInvokeError($"DOTNET:{(isStatic ? "STATIC" : "INVOKE")} {type.Name}.{name}", tie);
            }
            catch (ArgumentException) { /* wrong overload — try next */ }
        }
        return false;
    }

    /// <summary>(dotnet:static "Type" "Member" &rest args)
    /// Read-side entry point for static methods, properties, and fields.
    /// Type.InvokeMember dispatches based on member kind + arg count.</summary>
    public static LispObject DotNetStatic(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:STATIC: requires at least 2 arguments (type-name member-name &rest args)"));

        string typeName = args[0] switch { LispString ls => ls.Value, _ => args[0].ToString() ?? "" };
        string memberName = args[1] switch { LispString ls => ls.Value, _ => args[1].ToString() ?? "" };
        var type = ResolveDotNetType(typeName);
        var callArgs = LispArgsToDotNetGeneric(args.Skip(2).ToArray());

        // See DotNetInvoke: marshal with declared param types first when an arg is
        // nil, so value-type / Nullable<value> params get the natural default
        // (bool/bool? → false) rather than null.
        if (args.Skip(2).Any(a => a is Nil)
            && TryInvokeWithMarshalledArgs(type, memberName, null, args.Skip(2).ToArray(), true, out var pre))
            return DotNetToLisp(pre);

        try
        {
            if (TryCachedInvoke(type, memberName, null, callArgs, true, out var cached))
                return DotNetToLisp(cached);
            // Guard against InvokeMember binding an integer/pointer arg to a
            // catch-all (object, ...) overload when a typed one exists.
            if (TryInvokeMostSpecificOverload(type, memberName, null, args.Skip(2).ToArray(), true, out var spec))
                return DotNetToLisp(spec);
            var result = type.InvokeMember(memberName, StaticReadFlags, null, null, callArgs);
            return DotNetToLisp(result);
        }
        catch (MissingMethodException)
        {
            // No exact overload — retry allowing omitted C# optional parameters (#24).
            if (TryInvokeWithOptionalDefaults(type, memberName, null, args.Skip(2).ToArray(), true, out var r))
                return DotNetToLisp(r);
            // Or retry marshalling each arg to a candidate's declared param types
            // (e.g. Lisp list → T[]), which the binder's runtime-type match misses.
            if (TryInvokeWithMarshalledArgs(type, memberName, null, args.Skip(2).ToArray(), true, out var r2))
                return DotNetToLisp(r2);
            throw;
        }
        catch (System.Reflection.TargetInvocationException tie)
        {
            throw DotNetInvokeError($"DOTNET:STATIC {typeName}.{memberName}", tie);
        }
    }

    /// <summary>(dotnet:%set-static "Type" "Member" &rest indexer-args value)
    /// Write-side entry point. Last argument is the value to assign;
    /// preceding args (if any) are property indexer arguments.</summary>
    public static LispObject DotNetSetStatic(LispObject[] args)
    {
        if (args.Length < 3)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:%SET-STATIC: requires at least 3 arguments (type-name member-name [indexers...] value)"));

        string typeName = args[0] switch { LispString ls => ls.Value, _ => args[0].ToString() ?? "" };
        string memberName = args[1] switch { LispString ls => ls.Value, _ => args[1].ToString() ?? "" };
        var type = ResolveDotNetType(typeName);
        var callArgs = LispArgsToDotNetGeneric(args.Skip(2).ToArray());

        try
        {
            type.InvokeMember(memberName, StaticWriteFlags, null, null, callArgs);
            return args[args.Length - 1];
        }
        catch (System.Reflection.TargetInvocationException tie)
        {
            throw DotNetInvokeError($"DOTNET:%SET-STATIC {typeName}.{memberName}", tie);
        }
    }

    /// <summary>(dotnet:invoke object "Member" &rest args)
    /// Read-side entry point for instance methods, properties, fields, and
    /// COM IDispatch members. Type.InvokeMember on the runtime type routes
    /// transparently for both managed and __ComObject targets.</summary>
    public static LispObject DotNetInvoke(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:INVOKE: requires at least 2 arguments (object member-name &rest args)"));

        if (args[0] is not LispDotNetObject dno)
            throw new LispErrorException(new LispTypeError(
                "DOTNET:INVOKE: first argument must be a .NET object", args[0]));

        var target = dno.Value;
        string memberName = args[1] switch { LispString ls => ls.Value, _ => args[1].ToString() ?? "" };
        var type = target.GetType();
        var callArgs = LispArgsToDotNetGeneric(args.Skip(2).ToArray());

        // C# arrays don't expose get_Item/set_Item as named methods; route to
        // Array.GetValue / Array.SetValue instead.
        if (target is Array arr)
        {
            if (memberName == "get_Item")
            {
                var indices = callArgs.Select(Convert.ToInt32).ToArray();
                return DotNetToLisp(arr.GetValue(indices));
            }
            if (memberName == "set_Item")
            {
                var indices = callArgs.Take(callArgs.Length - 1).Select(Convert.ToInt32).ToArray();
                arr.SetValue(callArgs[callArgs.Length - 1], indices);
                return DotNetToLisp(callArgs[callArgs.Length - 1]);
            }
        }

        // Lisp NIL is ambiguous between .NET null and a value-type default (bool
        // false, etc.). The generic binder path below treats it as null, which is
        // wrong for value-type / Nullable<value> parameters — e.g.
        // (dotnet:invoke cb "set_IsChecked" nil) keeps IsChecked null instead of
        // false. When any arg is nil, first try parameter-type-aware marshalling
        // (LispToDotNet with the declared param type) so nil maps to the param's
        // natural default (bool/bool? → false, ref/string/int? → null).
        // Reference/string params still resolve to null, so this only changes the
        // previously-broken value-type case.
        if (args.Skip(2).Any(a => a is Nil)
            && TryInvokeWithMarshalledArgs(type, memberName, target, args.Skip(2).ToArray(), false, out var pre))
            return DotNetToLisp(pre);

        try
        {
            if (TryCachedInvoke(type, memberName, target, callArgs, false, out var cached))
                return DotNetToLisp(cached);
            var result = type.InvokeMember(memberName, InstanceReadFlags, null, target, callArgs);
            return DotNetToLisp(result);
        }
        catch (MissingMethodException)
        {
            // No exact overload — retry allowing omitted C# optional parameters (#24).
            if (TryInvokeWithOptionalDefaults(type, memberName, target, args.Skip(2).ToArray(), false, out var r))
                return DotNetToLisp(r);
            // Or retry marshalling each arg to a candidate's declared param types
            // (e.g. Lisp list → T[]), which the binder's runtime-type match misses.
            if (TryInvokeWithMarshalledArgs(type, memberName, target, args.Skip(2).ToArray(), false, out var r2))
                return DotNetToLisp(r2);
            // Last resort: an extension method (e.g. LINQ's Enumerable.Where) —
            // a static method elsewhere whose first parameter accepts the receiver.
            try
            {
                if (TryInvokeExtensionMethod(type, memberName, target, args.Skip(2).ToArray(), out var ext))
                    return DotNetToLisp(ext);
            }
            catch (System.Reflection.TargetInvocationException etie)
            {
                throw DotNetInvokeError($"DOTNET:INVOKE {type.Name}.{memberName} (extension)", etie);
            }
            throw;
        }
        catch (System.Reflection.TargetInvocationException tie)
        {
            throw DotNetInvokeError($"DOTNET:INVOKE {type.Name}.{memberName}", tie);
        }
    }

    // --- Extension-method fallback (dotcl/dotcl#45) ---
    // Cache of public static methods marked [Extension] by simple name, scanned
    // lazily across loaded assemblies on first lookup. Fallback-only, so normal
    // instance-method calls pay nothing.
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<string, System.Reflection.MethodInfo[]>
        _extensionMethodsByName = new();

    private static System.Reflection.MethodInfo[] ExtensionMethodsNamed(string name)
        => _extensionMethodsByName.GetOrAdd(name, n =>
        {
            var list = new System.Collections.Generic.List<System.Reflection.MethodInfo>();
            var extAttr = typeof(System.Runtime.CompilerServices.ExtensionAttribute);
            foreach (var asm in AppDomain.CurrentDomain.GetAssemblies())
            {
                Type[] types;
                try { types = asm.GetTypes(); }
                catch { continue; } // ReflectionTypeLoadException etc. — skip assembly
                foreach (var t in types)
                {
                    // Extension methods live in static classes (sealed + abstract).
                    if (!t.IsClass || !t.IsSealed || !t.IsAbstract) continue;
                    if (!t.IsDefined(extAttr, false)) continue;
                    foreach (var m in t.GetMethods(System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Static))
                        if (m.Name == n && m.IsDefined(extAttr, false))
                            list.Add(m);
                }
            }
            return list.ToArray();
        });

    /// <summary>The element type T if T2 is (or implements) IEnumerable&lt;T&gt;,
    /// or the array element type; else null. Used to infer a single generic type
    /// argument of a LINQ-style extension method from the receiver.</summary>
    private static Type? EnumerableElementType(Type t)
    {
        if (t.IsArray) return t.GetElementType();
        if (t.IsGenericType && t.GetGenericTypeDefinition() == typeof(System.Collections.Generic.IEnumerable<>))
            return t.GetGenericArguments()[0];
        foreach (var i in t.GetInterfaces())
            if (i.IsGenericType && i.GetGenericTypeDefinition() == typeof(System.Collections.Generic.IEnumerable<>))
                return i.GetGenericArguments()[0];
        return null;
    }

    /// <summary>Try to invoke METHODNAME as an extension method on TARGET (receiver).
    /// Handles non-generic extension methods and single-type-parameter generic ones
    /// whose type argument can be inferred from the receiver's IEnumerable&lt;T&gt;
    /// (covers Enumerable.Where etc.). Binding mismatches are skipped; an exception
    /// thrown by the resolved method propagates as TargetInvocationException.</summary>
    private static bool TryInvokeExtensionMethod(Type recvType, string methodName, object? target,
        LispObject[] lispArgs, out object? result)
    {
        result = null;
        foreach (var m in ExtensionMethodsNamed(methodName))
        {
            var ps = m.GetParameters();
            if (ps.Length != lispArgs.Length + 1) continue; // +1 for the receiver
            var concrete = m;
            if (m.IsGenericMethodDefinition)
            {
                if (m.GetGenericArguments().Length != 1) continue; // only 1-type-param inference
                var elem = EnumerableElementType(recvType);
                if (elem == null) continue;
                try { concrete = m.MakeGenericMethod(elem); }
                catch { continue; }
                ps = concrete.GetParameters();
            }
            if (!ps[0].ParameterType.IsAssignableFrom(recvType)) continue;
            var callArgs = new object?[ps.Length];
            callArgs[0] = target;
            bool ok = true;
            try
            {
                for (int i = 0; i < lispArgs.Length; i++)
                    callArgs[i + 1] = LispToDotNet(lispArgs[i], ps[i + 1].ParameterType);
            }
            catch { ok = false; }
            if (!ok) continue;
            result = concrete.Invoke(null, callArgs); // method's own throw → TargetInvocationException
            return true;
        }
        return false;
    }

    /// <summary>(dotnet:%set-invoke object "Member" &rest indexer-args value)
    /// Last arg is the value; preceding args are indexer arguments.</summary>
    public static LispObject DotNetSetInvoke(LispObject[] args)
    {
        if (args.Length < 3)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:%SET-INVOKE: requires at least 3 arguments (object member-name [indexers...] value)"));

        if (args[0] is not LispDotNetObject dno)
            throw new LispErrorException(new LispTypeError(
                "DOTNET:%SET-INVOKE: first argument must be a .NET object", args[0]));

        var target = dno.Value;
        string memberName = args[1] switch { LispString ls => ls.Value, _ => args[1].ToString() ?? "" };
        var type = target.GetType();
        var callArgs = LispArgsToDotNetGeneric(args.Skip(2).ToArray());

        try
        {
            type.InvokeMember(memberName, InstanceWriteFlags, null, target, callArgs);
            return args[args.Length - 1];
        }
        catch (System.Reflection.TargetInvocationException tie)
        {
            throw DotNetInvokeError($"DOTNET:%SET-INVOKE {type.Name}.{memberName}", tie);
        }
    }

    public static LispObject DotNetNew(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:NEW: requires at least 1 argument (type-name &rest args)"));

        // Accept a resolved System.Type (e.g. from dotnet:make-generic-type) as the
        // first arg, in addition to a type-name string/symbol.
        Type type;
        if (args[0] is LispDotNetObject tdno && tdno.Value is Type resolvedType)
            type = resolvedType;
        else
        {
            string typeName = args[0] switch { LispString ls => ls.Value, _ => args[0].ToString() ?? "" };
            type = ResolveDotNetType(typeName);
        }

        if (args.Length == 1)
        {
            // A true parameterless ctor (value types always have one).
            if (type.IsValueType || type.GetConstructor(Type.EmptyTypes) != null)
                return new LispDotNetObject(Activator.CreateInstance(type)!);
            // Otherwise fall back to an all-optional ctor, supplying its defaults —
            // C#'s `new T()` does the same (e.g. FluentTheme(Uri? baseUri = null)).
            var optCtor = type.GetConstructors()
                .Where(c => c.GetParameters().Length > 0 && c.GetParameters().All(p => p.IsOptional))
                .OrderBy(c => c.GetParameters().Length)
                .FirstOrDefault();
            if (optCtor != null)
            {
                var ps = optCtor.GetParameters();
                var defaults = new object?[ps.Length];
                for (int i = 0; i < ps.Length; i++)
                    defaults[i] = ps[i].HasDefaultValue ? ps[i].DefaultValue : Type.Missing;
                return new LispDotNetObject(optCtor.Invoke(defaults)!);
            }
            // No usable ctor — let Activator throw its descriptive error.
            return new LispDotNetObject(Activator.CreateInstance(type)!);
        }

        var lispArgs = args.Skip(1).ToArray();
        // A ctor is a candidate when the supplied arg count fits between its
        // required and total param count, with any omitted trailing params all
        // optional (filled with their defaults below). This admits e.g.
        // SolidColorBrush(Color, double opacity = 1) for a single Color arg —
        // without it the only fixed-arity 1-param ctor is (uint), and the Color
        // gets Convert.ChangeType'd to uint → "Object must implement IConvertible"
        // Mirrors TryInvokeWithOptionalDefaults for methods (#24).
        var ctors = type.GetConstructors().Where(c => {
            var ps = c.GetParameters();
            if (lispArgs.Length > ps.Length) return false;
            for (int i = lispArgs.Length; i < ps.Length; i++)
                if (!ps[i].IsOptional) return false;
            return true;
        }).ToArray();

        if (ctors.Length == 0)
            throw new LispErrorException(new LispError(
                $"DOTNET:NEW: no constructor for {type.FullName} with {lispArgs.Length} arguments"));

        // Score constructors like methods for best type match. Prefer the closest
        // arity (fewest filled-in optionals) as a tie-breaker, so a fixed-arity
        // overload beats one that relies on default-filling at equal type score.
        var ctor = ctors.OrderByDescending(c => {
            int score = 0;
            var ps = c.GetParameters();
            for (int i = 0; i < ps.Length && i < lispArgs.Length; i++)
            {
                var pt = ps[i].ParameterType;
                if (lispArgs[i] is Fixnum) { if (pt == typeof(int) || pt == typeof(long)) score += 10; }
                else if (lispArgs[i] is DoubleFloat || lispArgs[i] is SingleFloat) { if (pt == typeof(double)) score += 10; }
                else if (lispArgs[i] is LispString) { if (pt == typeof(string)) score += 10; }
                else if (lispArgs[i] is LispDotNetObject dno)
                {
                    // A wrapped .NET instance (value-type struct or reference type):
                    // prefer the overload whose parameter type the instance is
                    // assignable to (exact match wins over a base/interface match),
                    // e.g. SolidColorBrush(Color) for a Color rather than (uint),
                    // or Bitmap(Stream) for a MemoryStream rather than Bitmap(string).
                    var at = dno is LispDotNetBoxed bx ? bx.HintType : dno.Type;
                    if (pt == at) score += 20;
                    else if (pt.IsAssignableFrom(at)) score += 10;
                }
            }
            return score;
        }).ThenBy(c => c.GetParameters().Length).First();

        var paramTypes = ctor.GetParameters();
        var convertedArgs = new object?[paramTypes.Length];
        for (int i = 0; i < lispArgs.Length; i++)
            convertedArgs[i] = LispToDotNet(lispArgs[i], paramTypes[i].ParameterType);
        for (int i = lispArgs.Length; i < paramTypes.Length; i++)
            convertedArgs[i] = paramTypes[i].HasDefaultValue ? paramTypes[i].DefaultValue : Type.Missing;

        var instance = ctor.Invoke(convertedArgs);
        return new LispDotNetObject(instance);
    }

    /// <summary>(dotnet:%define-class "Full.Name" &optional "Base.Type" field-specs attr-specs method-specs ctor-body property-specs interface-specs event-specs)
    /// Emit a named public class. Shapes: (fields) (attrs),
    /// (methods) (ctor-body: 1-arg Lisp fn called after base.ctor),
    /// (property-specs: list of ("Name" "TypeName") for auto-properties).
    /// method-spec accepts optional 5th element override-flag; when truthy,
    /// the method is emitted as an override of a matching base virtual method.
    /// 8th arg interface-specs is a list of fully qualified interface
    /// type names; each declared, and any method in method-specs whose
    /// name+signature matches an interface method is emitted as the implicit
    /// implementation of that slot.
    /// 9th arg event-specs is a list of ("Name" "DelegateTypeName"); each
    /// emits a private delegate field + public add_/remove_ accessors +
    /// EventBuilder. If a declared interface carries a matching add_/remove_
    /// slot, the accessors are wired up as implicit implementations.
    /// property-specs accepts optional 3rd element (notify-flag); when
    /// truthy, the setter additionally calls OnPropertyChanged with a
    /// PropertyChangedEventArgs carrying the property name. Requires a
    /// matching PropertyChanged event to be declared via event-specs.
    /// Returns the full name as a LispString on success.</summary>
    public static LispObject DotNetDefineClass(LispObject[] args)
    {
#if !DOTCL_EMIT
        // Defining a brand-new .NET type at runtime is inherently emit-based
        // (AssemblyBuilder/TypeBuilder); unavailable on the emit-free runtime.
        throw new LispErrorException(new LispProgramError(
            "DOTNET:%DEFINE-CLASS: defining .NET classes requires the emitting runtime (not available on this build)"));
#else
        if (args.Length < 1 || args.Length > 12)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:%DEFINE-CLASS: requires 1-12 arguments (full-name &optional base-type-name field-specs attr-specs method-specs ctor-body property-specs interface-specs event-specs ctor-param-types base-ctor-arg-indices ctor-specs-list)"));

        string fullName = args[0] switch
        {
            LispString ls => ls.Value,
            _ => args[0].ToString() ?? ""
        };

        Type? baseType = null;
        if (args.Length >= 2 && args[1] != Nil.Instance)
        {
            string baseName = args[1] switch
            {
                LispString ls => ls.Value,
                _ => args[1].ToString() ?? ""
            };
            baseType = ResolveDotNetType(baseName);
        }

        List<(string, Type)>? fields = null;
        if (args.Length >= 3 && args[2] != Nil.Instance)
        {
            fields = new List<(string, Type)>();
            var cur = args[2];
            while (cur is Cons c)
            {
                if (c.Car is not Cons spec)
                    throw new LispErrorException(new LispTypeError(
                        "DOTNET:%DEFINE-CLASS: each field spec must be a (name type-name) list",
                        c.Car));
                var nameObj = spec.Car;
                var typeObj = spec.Cdr is Cons c2 ? c2.Car : Nil.Instance;

                string fname = nameObj switch
                {
                    LispString ls => ls.Value,
                    _ => nameObj.ToString() ?? ""
                };
                string tname = typeObj switch
                {
                    LispString ls => ls.Value,
                    _ => typeObj.ToString() ?? ""
                };
                fields.Add((fname, ResolveDotNetType(tname)));
                cur = c.Cdr;
            }
        }

        List<System.Reflection.Emit.CustomAttributeBuilder>? attrs = null;
        if (args.Length >= 4 && args[3] != Nil.Instance)
        {
            attrs = new List<System.Reflection.Emit.CustomAttributeBuilder>();
            var cur = args[3];
            while (cur is Cons c)
            {
                if (c.Car is not Cons spec)
                    throw new LispErrorException(new LispTypeError(
                        "DOTNET:%DEFINE-CLASS: each attr spec must be a (type-name ctor-args...) list",
                        c.Car));
                var typeObj = spec.Car;
                string tname = typeObj switch
                {
                    LispString ls => ls.Value,
                    _ => typeObj.ToString() ?? ""
                };
                var attrType = ResolveDotNetType(tname);

                // Collect ctor args (rest of spec).
                var ctorLispArgs = new List<LispObject>();
                var acur = spec.Cdr;
                while (acur is Cons ac) { ctorLispArgs.Add(ac.Car); acur = ac.Cdr; }

                // Find a ctor matching argcount.
                var ctors = attrType.GetConstructors()
                    .Where(ci => ci.GetParameters().Length == ctorLispArgs.Count).ToArray();
                if (ctors.Length == 0)
                    throw new LispErrorException(new LispError(
                        $"DOTNET:%DEFINE-CLASS: no constructor on {tname} with {ctorLispArgs.Count} arguments"));
                var ctor = ctors[0];
                var ctorParamTypes = ctor.GetParameters();
                var ctorArgs = new object?[ctorLispArgs.Count];
                for (int i = 0; i < ctorLispArgs.Count; i++)
                    ctorArgs[i] = LispToDotNet(ctorLispArgs[i], ctorParamTypes[i].ParameterType);

                attrs.Add(new System.Reflection.Emit.CustomAttributeBuilder(ctor, ctorArgs));
                cur = c.Cdr;
            }
        }

        List<Emitter.DynamicClassBuilder.MethodSpec>? methods = null;
        if (args.Length >= 5 && args[4] != Nil.Instance)
        {
            methods = new List<Emitter.DynamicClassBuilder.MethodSpec>();
            var cur = args[4];
            while (cur is Cons c)
            {
                if (c.Car is not Cons spec)
                    throw new LispErrorException(new LispTypeError(
                        "DOTNET:%DEFINE-CLASS: each method spec must be a (name return-type (param-types) lambda) list",
                        c.Car));
                // Spec shape: (name return-type (param-types) lambda)
                var nameObj = spec.Car;
                var rest = spec.Cdr;
                if (rest is not Cons r1)
                    throw new LispErrorException(new LispProgramError(
                        "DOTNET:%DEFINE-CLASS: method spec missing return type"));
                var retObj = r1.Car;
                var rest2 = r1.Cdr;
                if (rest2 is not Cons r2)
                    throw new LispErrorException(new LispProgramError(
                        "DOTNET:%DEFINE-CLASS: method spec missing param-types list"));
                var paramListObj = r2.Car;
                var rest3 = r2.Cdr;
                if (rest3 is not Cons r3)
                    throw new LispErrorException(new LispProgramError(
                        "DOTNET:%DEFINE-CLASS: method spec missing lambda"));
                var lambdaObj = r3.Car;

                string mname = nameObj switch
                {
                    LispString ls => ls.Value,
                    _ => nameObj.ToString() ?? ""
                };
                string rname = retObj switch
                {
                    LispString ls => ls.Value,
                    _ => retObj.ToString() ?? ""
                };
                Type rtype = rname == "System.Void" ? typeof(void) : ResolveDotNetType(rname);

                var paramTypes = new List<Type>();
                var pcur = paramListObj;
                while (pcur is Cons pc)
                {
                    var ptObj = pc.Car;
                    string ptname = ptObj switch
                    {
                        LispString ls => ls.Value,
                        _ => ptObj.ToString() ?? ""
                    };
                    paramTypes.Add(ResolveDotNetType(ptname));
                    pcur = pc.Cdr;
                }

                if (lambdaObj is not LispFunction)
                    throw new LispErrorException(new LispTypeError(
                        "DOTNET:%DEFINE-CLASS: method body must be a function",
                        lambdaObj));

                // Optional 5th element: override flag. Nil/absent = false.
                bool isOverride = false;
                LispObject? attrSpecsObj = null;
                if (r3.Cdr is Cons r4)
                {
                    isOverride = r4.Car != Nil.Instance;
                    // Optional 6th element: list of (type-name ctor-args...)
                    // attribute specs, same shape as the class-level attrs list.
                    if (r4.Cdr is Cons r5)
                        attrSpecsObj = r5.Car;
                }

                List<System.Reflection.Emit.CustomAttributeBuilder>? methodAttrs = null;
                if (attrSpecsObj != null && attrSpecsObj != Nil.Instance)
                {
                    methodAttrs = new List<System.Reflection.Emit.CustomAttributeBuilder>();
                    var acur = attrSpecsObj;
                    while (acur is Cons ac)
                    {
                        if (ac.Car is not Cons aspec)
                            throw new LispErrorException(new LispTypeError(
                                "DOTNET:%DEFINE-CLASS: each method attr spec must be a (type-name ctor-args...) list",
                                ac.Car));
                        var atypeObj = aspec.Car;
                        string atname = atypeObj switch
                        {
                            LispString ls => ls.Value,
                            _ => atypeObj.ToString() ?? ""
                        };
                        var attrType = ResolveDotNetType(atname);

                        var actorArgs = new List<LispObject>();
                        var aacur = aspec.Cdr;
                        while (aacur is Cons aac) { actorArgs.Add(aac.Car); aacur = aac.Cdr; }

                        var actors = attrType.GetConstructors()
                            .Where(ci => ci.GetParameters().Length == actorArgs.Count).ToArray();
                        if (actors.Length == 0)
                            throw new LispErrorException(new LispError(
                                $"DOTNET:%DEFINE-CLASS: no constructor on {atname} with {actorArgs.Count} arguments"));
                        var actor = actors[0];
                        var actorParamTypes = actor.GetParameters();
                        var actorArgsArr = new object?[actorArgs.Count];
                        for (int i = 0; i < actorArgs.Count; i++)
                            actorArgsArr[i] = LispToDotNet(actorArgs[i], actorParamTypes[i].ParameterType);

                        methodAttrs.Add(new System.Reflection.Emit.CustomAttributeBuilder(actor, actorArgsArr));
                        acur = ac.Cdr;
                    }
                }

                methods.Add(new Emitter.DynamicClassBuilder.MethodSpec(
                    mname, rtype, paramTypes, lambdaObj, isOverride, methodAttrs));
                cur = c.Cdr;
            }
        }

        LispObject? ctorBody = null;
        if (args.Length >= 6 && args[5] != Nil.Instance)
        {
            if (args[5] is not LispFunction)
                throw new LispErrorException(new LispTypeError(
                    "DOTNET:%DEFINE-CLASS: ctor-body must be a function",
                    args[5]));
            ctorBody = args[5];
        }

        List<(string, Type, bool)>? propertySpecs = null;
        if (args.Length >= 7 && args[6] != Nil.Instance)
        {
            propertySpecs = new List<(string, Type, bool)>();
            var cur = args[6];
            while (cur is Cons c)
            {
                if (c.Car is not Cons spec)
                    throw new LispErrorException(new LispTypeError(
                        "DOTNET:%DEFINE-CLASS: each property spec must be a (name type-name &optional notify) list",
                        c.Car));
                var nameObj = spec.Car;
                var rest = spec.Cdr;
                var typeObj = rest is Cons c2 ? c2.Car : Nil.Instance;
                var notifyObj = (rest is Cons c2a && c2a.Cdr is Cons c3)
                    ? c3.Car : Nil.Instance;

                string pname = nameObj switch
                {
                    LispString ls => ls.Value,
                    _ => nameObj.ToString() ?? ""
                };
                string tname = typeObj switch
                {
                    LispString ls => ls.Value,
                    _ => typeObj.ToString() ?? ""
                };
                bool notify = notifyObj != Nil.Instance;
                propertySpecs.Add((pname, ResolveDotNetType(tname), notify));
                cur = c.Cdr;
            }
        }

        List<Type>? interfaceSpecs = null;
        if (args.Length >= 8 && args[7] != Nil.Instance)
        {
            interfaceSpecs = new List<Type>();
            var cur = args[7];
            while (cur is Cons c)
            {
                var entry = c.Car;
                string tname = entry switch
                {
                    LispString ls => ls.Value,
                    _ => entry.ToString() ?? ""
                };
                interfaceSpecs.Add(ResolveDotNetType(tname));
                cur = c.Cdr;
            }
        }

        List<(string, Type)>? eventSpecs = null;
        if (args.Length >= 9 && args[8] != Nil.Instance)
        {
            eventSpecs = new List<(string, Type)>();
            var cur = args[8];
            while (cur is Cons c)
            {
                if (c.Car is not Cons spec)
                    throw new LispErrorException(new LispTypeError(
                        "DOTNET:%DEFINE-CLASS: each event spec must be a (name delegate-type-name) list",
                        c.Car));
                var nameObj = spec.Car;
                var typeObj = spec.Cdr is Cons c2 ? c2.Car : Nil.Instance;

                string ename = nameObj switch
                {
                    LispString ls => ls.Value,
                    _ => nameObj.ToString() ?? ""
                };
                string tname = typeObj switch
                {
                    LispString ls => ls.Value,
                    _ => typeObj.ToString() ?? ""
                };
                eventSpecs.Add((ename, ResolveDotNetType(tname)));
                cur = c.Cdr;
            }
        }

        List<Type>? userCtorParamTypes = null;
        if (args.Length >= 10 && args[9] != Nil.Instance)
        {
            userCtorParamTypes = new List<Type>();
            var cur = args[9];
            while (cur is Cons c)
            {
                string tname = c.Car switch
                {
                    LispString ls => ls.Value,
                    _ => c.Car.ToString() ?? ""
                };
                userCtorParamTypes.Add(ResolveDotNetType(tname));
                cur = c.Cdr;
            }
        }

        List<int>? baseCtorArgIndices = null;
        if (args.Length >= 11 && args[10] != Nil.Instance)
        {
            baseCtorArgIndices = new List<int>();
            var cur = args[10];
            while (cur is Cons c)
            {
                if (c.Car is not Fixnum fi)
                    throw new LispErrorException(new LispTypeError(
                        "DOTNET:%DEFINE-CLASS: base-ctor-arg-indices must be a list of integers",
                        c.Car));
                baseCtorArgIndices.Add((int)fi.Value);
                cur = c.Cdr;
            }
        }

        // arg 11: ctor-specs-list: list of (lambda param-types base-arg-indices) triples.
        // When non-nil, overrides the single-ctor path (args 5/9/10).
        List<Emitter.DynamicClassBuilder.CtorSpec>? ctorSpecs = null;
        if (args.Length >= 12 && args[11] != Nil.Instance)
        {
            ctorSpecs = new List<Emitter.DynamicClassBuilder.CtorSpec>();
            var cur = args[11];
            while (cur is Cons c)
            {
                if (c.Car is not Cons spec)
                    throw new LispErrorException(new LispTypeError(
                        "DOTNET:%DEFINE-CLASS: each ctor-spec must be a (lambda param-types base-arg-indices) list",
                        c.Car));
                var lambdaObj = spec.Car;
                var rest1 = spec.Cdr;
                var paramTypesObj = rest1 is Cons r1 ? r1.Car : Nil.Instance;
                var baseIndicesObj = (rest1 is Cons r1b && r1b.Cdr is Cons r2b) ? r2b.Car : Nil.Instance;

                if (lambdaObj is not LispFunction && lambdaObj != Nil.Instance)
                    throw new LispErrorException(new LispTypeError(
                        "DOTNET:%DEFINE-CLASS: ctor-spec body must be a function or nil", lambdaObj));

                LispObject? ctorLambda = lambdaObj == Nil.Instance ? null : lambdaObj;

                var paramTypes = new List<Type>();
                var pcur = paramTypesObj;
                while (pcur is Cons pc)
                {
                    string ptname = pc.Car switch
                    {
                        LispString ls => ls.Value,
                        _ => pc.Car.ToString() ?? ""
                    };
                    paramTypes.Add(ResolveDotNetType(ptname));
                    pcur = pc.Cdr;
                }

                var baseIndices = new List<int>();
                var bcur = baseIndicesObj;
                while (bcur is Cons bc)
                {
                    if (bc.Car is not Fixnum bfi)
                        throw new LispErrorException(new LispTypeError(
                            "DOTNET:%DEFINE-CLASS: ctor-spec base-arg-indices must be integers", bc.Car));
                    baseIndices.Add((int)bfi.Value);
                    bcur = bc.Cdr;
                }

                ctorSpecs.Add(new Emitter.DynamicClassBuilder.CtorSpec(
                    ctorLambda,
                    paramTypes.Count > 0 ? (IReadOnlyList<Type>)paramTypes : null,
                    baseIndices.Count > 0 ? (IReadOnlyList<int>)baseIndices : null));
                cur = c.Cdr;
            }
        }

        try
        {
            var type = Emitter.DynamicClassBuilder.DefineMinimalClass(
                fullName, baseType, fields, attrs, methods, ctorBody, propertySpecs,
                interfaceSpecs, eventSpecs, userCtorParamTypes, baseCtorArgIndices,
                ctorSpecs);
            // Register as CLOS class so class-of/type-of/find-class work for instances.
            EnsureDotNetTypeClass(type);
            // Register by uppercase name for resolution from Lisp symbols (e.g. Animal → ANIMAL).
            _dotNetDynTypeByUpperName[type.Name] = type;
            if (type.FullName != null && type.FullName != type.Name)
                _dotNetDynTypeByUpperName[type.FullName] = type;
            return new LispString(type.FullName ?? fullName);
        }
        catch (ArgumentException ae)
        {
            throw new LispErrorException(new LispError(
                $"DOTNET:%DEFINE-CLASS: {ae.Message}"));
        }
#endif
    }

#if DOTCL_EMIT
    // Cache: (selfType, methodName, paramTypeSig) → DynamicMethod for non-virtual base call.
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<(Type, string, string), System.Reflection.Emit.DynamicMethod>
        _baseCallCache = new();
#endif

    /// <summary>(dotnet:call-base self "Method" &rest args)
    /// Call the base class implementation of a virtual method non-virtually
    /// (equivalent to C# base.Method(args)). self must be a dotcl-defined class
    /// instance; base type is self.GetType().BaseType. Requires the emitting runtime
    /// (emits a non-virtual call thunk via DynamicMethod).</summary>
    public static LispObject DotNetCallBase(LispObject[] args)
    {
#if !DOTCL_EMIT
        throw new LispErrorException(new LispProgramError(
            "DOTNET:CALL-BASE: requires the emitting runtime (not available on this build)"));
#else
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:CALL-BASE: requires at least 2 arguments (self method-name &rest args)"));
        if (args[0] is not LispDotNetObject dno)
            throw new LispErrorException(new LispTypeError(
                "DOTNET:CALL-BASE: first argument must be a .NET object", args[0]));

        var target    = dno.Value;
        var selfType  = target.GetType();
        var baseType  = selfType.BaseType
            ?? throw new LispErrorException(new LispError(
                $"DOTNET:CALL-BASE: {selfType.FullName} has no base type"));
        string methodName = args[1] switch { LispString ls => ls.Value, _ => args[1].ToString() ?? "" };
        var callArgs  = LispArgsToDotNetGeneric(args.Skip(2).ToArray());

        // Find best-matching method on base type by name + arg count.
        var candidates = baseType.GetMethods(
            System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance)
            .Where(m => m.Name == methodName && m.GetParameters().Length == callArgs.Length)
            .ToArray();
        if (candidates.Length == 0)
            throw new LispErrorException(new LispError(
                $"DOTNET:CALL-BASE: no method {baseType.Name}.{methodName} with {callArgs.Length} args"));
        var baseMethod = candidates[0];
        var paramInfos = baseMethod.GetParameters();

        // Convert args to expected parameter types.
        var convertedArgs = new object?[callArgs.Length];
        for (int i = 0; i < callArgs.Length; i++)
            convertedArgs[i] = callArgs[i] == null ? null
                : Convert.ChangeType(callArgs[i], paramInfos[i].ParameterType,
                    System.Globalization.CultureInfo.InvariantCulture);

        // Get or create a DynamicMethod that emits `call` (non-virtual) to baseMethod.
        var sig = string.Join(",", paramInfos.Select(p => p.ParameterType.FullName));
        var dm = _baseCallCache.GetOrAdd((selfType, methodName, sig), _ =>
        {
            var pTypes   = new[] { selfType }.Concat(paramInfos.Select(p => p.ParameterType)).ToArray();
            var dynMethod = new System.Reflection.Emit.DynamicMethod(
                "__base_" + methodName, baseMethod.ReturnType, pTypes, selfType, skipVisibility: true);
            var il = dynMethod.GetILGenerator();
            il.Emit(System.Reflection.Emit.OpCodes.Ldarg_0);
            for (int i = 0; i < paramInfos.Length; i++)
                il.Emit(System.Reflection.Emit.OpCodes.Ldarg, i + 1);
            il.Emit(System.Reflection.Emit.OpCodes.Call, baseMethod);
            il.Emit(System.Reflection.Emit.OpCodes.Ret);
            return dynMethod;
        });

        var allArgs = new object?[] { target }.Concat(convertedArgs).ToArray();
        var result  = dm.Invoke(null, allArgs);
        return DotNetToLisp(result);
#endif
    }

    public static LispObject DotNetBox(LispObject[] args)
    {
        if (args.Length != 2)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:BOX: requires 2 arguments (value type-name)"));

        string typeName = args[1] switch { LispString ls => ls.Value, _ => args[1].ToString() ?? "" };
        var type = ResolveDotNetType(typeName);
        var converted = LispToDotNet(args[0], type);
        return new LispDotNetBoxed(converted!, type);
    }

    /// <summary>
    /// <lispdoc>(dotnet:hint-type obj) -- For a value produced by dotnet:box, return its hint type (the user-supplied static type used to choose overloads) as a System.Type. Returns NIL for any object that carries no hint (an ordinary .NET object, or a non-.NET value). Use dotnet:object-type for the actual runtime type. (#31)</lispdoc>
    /// Return the static hint type of a LispDotNetBoxed value (the type given to
    /// dotnet:box, used for overload resolution), or NIL when OBJ carries no hint.
    /// </summary>
    [LispDoc("DOTNET:HINT-TYPE")]
    public static LispObject DotNetHintType(LispObject arg)
        => arg is LispDotNetBoxed boxed ? new LispDotNetObject(boxed.HintType) : Nil.Instance;

    /// <summary>
    /// <lispdoc>(dotnet:exception-type condition) -- For a condition that wraps a raw .NET exception (e.g. caught in handler-case), return the original CLR exception System.Type. Returns NIL for an ordinary Lisp condition. Use with dotnet:exception-typep / dotnet:handler-bind to dispatch on specific .NET exception types. (dotcl/dotcl#45)</lispdoc>
    /// </summary>
    [LispDoc("DOTNET:EXCEPTION-TYPE")]
    public static LispObject DotNetExceptionType(LispObject arg)
        => arg is LispCondition lc && lc.ClrExceptionType is Type ct
            ? new LispDotNetObject(ct) : Nil.Instance;

    /// <summary>
    /// <lispdoc>(dotnet:exception-typep condition type) -- Return T if CONDITION wraps a raw .NET exception whose CLR type is TYPE or a subtype of it (Type.IsAssignableFrom), else NIL. TYPE is a type-name string/symbol or System.Type. This is the matcher dotnet:handler-bind uses. (dotcl/dotcl#45)</lispdoc>
    /// </summary>
    [LispDoc("DOTNET:EXCEPTION-TYPEP")]
    public static LispObject DotNetExceptionTypep(LispObject condition, LispObject typeName)
    {
        if (condition is LispCondition lc && lc.ClrExceptionType is Type ct)
        {
            var want = ResolveElementTypeArg(typeName);
            return want.IsAssignableFrom(ct) ? T.Instance : Nil.Instance;
        }
        return Nil.Instance;
    }

    /// <summary>
    /// <lispdoc>(dotnet:make-generic-type open-type type-args-list) -- Construct a closed generic System.Type from an open generic type definition and a Lisp list of type-argument names. open-type is the open definition name, with or without the CLR backtick-arity suffix (e.g. "System.Collections.Generic.Dictionary`2" or just "System.Collections.Generic.Dictionary" — the arity is inferred from the type-args list). The result is a System.Type usable with dotnet:new, dotnet:static-generic type args, etc. Example: (dotnet:make-generic-type "System.Collections.Generic.Dictionary" '("System.String" "System.Int32")). (dotcl/dotcl#45)</lispdoc>
    /// </summary>
    [LispDoc("DOTNET:MAKE-GENERIC-TYPE")]
    public static LispObject DotNetMakeGenericType(LispObject[] args)
    {
        if (args.Length != 2)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:MAKE-GENERIC-TYPE: requires open-type and type-args-list"));

        string openName = args[0] switch { LispString ls => ls.Value, Symbol sym => sym.Name, _ => args[0].ToString() ?? "" };

        // Parse the type-args list (a Lisp list of type-name strings/symbols).
        var typeArgNames = new System.Collections.Generic.List<string>();
        var cursor = args[1];
        while (cursor is Cons c)
        {
            typeArgNames.Add(c.Car switch { LispString ls => ls.Value, Symbol sym => sym.Name, _ => c.Car.ToString() ?? "" });
            cursor = c.Cdr;
        }
        if (typeArgNames.Count == 0)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:MAKE-GENERIC-TYPE: type-args-list must be a non-empty list of type names"));

        // Resolve the open generic definition. Accept the name with an explicit
        // backtick-arity (Dictionary`2) or without (infer arity from the args).
        Type? openType = TryResolveDotNetType(openName);
        if ((openType == null || !openType.IsGenericTypeDefinition) && !openName.Contains('`'))
            openType = TryResolveDotNetType($"{openName}`{typeArgNames.Count}");
        if (openType == null)
            throw new LispErrorException(new LispError(
                $"DOTNET:MAKE-GENERIC-TYPE: cannot resolve open generic type {openName}"));
        if (!openType.IsGenericTypeDefinition)
            throw new LispErrorException(new LispError(
                $"DOTNET:MAKE-GENERIC-TYPE: {openType.FullName} is not an open generic type definition"));
        if (openType.GetGenericArguments().Length != typeArgNames.Count)
            throw new LispErrorException(new LispError(
                $"DOTNET:MAKE-GENERIC-TYPE: {openType.FullName} expects " +
                $"{openType.GetGenericArguments().Length} type arg(s), got {typeArgNames.Count}"));

        var typeArgs = typeArgNames.Select(ResolveDotNetType).ToArray();
        try
        {
            return new LispDotNetObject(openType.MakeGenericType(typeArgs));
        }
        catch (Exception e)
        {
            throw new LispErrorException(new LispError(
                $"DOTNET:MAKE-GENERIC-TYPE {openType.FullName}: MakeGenericType failed: {e.Message}"));
        }
    }

    /// <summary>
    /// <lispdoc>(dotnet:enum-or enum-type &amp;rest members) -- Combine [Flags] enum members with bitwise OR and return the resulting enum value. enum-type is a type-name string/symbol or System.Type. Each member is a member-name string/symbol (Enum.Parse, case-insensitive), an integer, or an existing enum value of the type. e.g. (dotnet:enum-or "System.IO.FileAccess" "Read" "Write"). (dotcl/dotcl#45)</lispdoc>
    /// </summary>
    [LispDoc("DOTNET:ENUM-OR")]
    public static LispObject DotNetEnumOr(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:ENUM-OR: requires enum-type and at least one member"));
        var type = ResolveElementTypeArg(args[0]);
        if (!type.IsEnum)
            throw new LispErrorException(new LispTypeError(
                $"DOTNET:ENUM-OR: {type.FullName} is not an enum type", args[0]));
        long acc = 0;
        for (int i = 1; i < args.Length; i++)
        {
            switch (args[i])
            {
                case Fixnum fx: acc |= fx.Value; break;
                case LispDotNetObject dno when dno.Value.GetType() == type:
                    acc |= Convert.ToInt64(dno.Value); break;
                default:
                    string name = args[i] switch
                    {
                        LispString ls => ls.Value,
                        Symbol sym    => sym.Name,
                        _             => args[i].ToString() ?? ""
                    };
                    object parsed;
                    try { parsed = Enum.Parse(type, name, ignoreCase: true); }
                    catch (Exception e)
                    {
                        throw new LispErrorException(new LispError(
                            $"DOTNET:ENUM-OR: {name} is not a member of {type.FullName}: {e.Message}"));
                    }
                    acc |= Convert.ToInt64(parsed);
                    break;
            }
        }
        return DotNetToLisp(Enum.ToObject(type, acc));
    }

    /// <summary>
    /// <lispdoc>(dotnet:is-instance-of obj type) -- Return T if OBJ is an instance of TYPE, else NIL. TYPE is a type-name string/symbol or a resolved System.Type. OBJ may be a wrapped .NET object or a plain Lisp value (marshalled to its natural .NET type first, e.g. a string tests against System.String). Replaces manual Type.IsAssignableFrom checks. (dotcl/dotcl#45)</lispdoc>
    /// </summary>
    [LispDoc("DOTNET:IS-INSTANCE-OF")]
    public static LispObject DotNetIsInstanceOf(LispObject[] args)
    {
        if (args.Length != 2)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:IS-INSTANCE-OF: requires 2 arguments (object type)"));
        var type = ResolveElementTypeArg(args[1]);
        var val  = LispToDotNetGeneric(args[0]);
        return (val != null && type.IsInstanceOfType(val)) ? T.Instance : Nil.Instance;
    }

    /// <summary>
    /// <lispdoc>(dotnet:cast obj type) -- Reference cast: verify OBJ is an instance of TYPE (signalling an error if not, like a C# cast) and return it re-wrapped carrying TYPE as its static hint, so subsequent dotnet:invoke / dotnet:new overload resolution treats it as TYPE (e.g. upcast to a base class or interface). TYPE is a type-name string/symbol or a System.Type. For value-type conversions use dotnet:box instead. (dotcl/dotcl#45)</lispdoc>
    /// </summary>
    [LispDoc("DOTNET:CAST")]
    public static LispObject DotNetCast(LispObject[] args)
    {
        if (args.Length != 2)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:CAST: requires 2 arguments (object type)"));
        var type = ResolveElementTypeArg(args[1]);
        var val  = LispToDotNetGeneric(args[0]);
        if (val == null)
            throw new LispErrorException(new LispError("DOTNET:CAST: cannot cast NIL/null"));
        if (!type.IsInstanceOfType(val))
            throw new LispErrorException(new LispError(
                $"DOTNET:CAST: {val.GetType().FullName} is not an instance of {type.FullName}"));
        return new LispDotNetBoxed(val, type);
    }

    /// <summary>
    /// <lispdoc>(dotnet:object-type obj) -- Return the actual runtime type (value.GetType()) of a .NET object as a System.Type, or NIL if OBJ is not a .NET object. For a dotnet:box value this is the boxed value's real type, which may differ from dotnet:hint-type. (#31)</lispdoc>
    /// Return the actual runtime Type of a wrapped .NET object, or NIL otherwise.
    /// </summary>
    [LispDoc("DOTNET:OBJECT-TYPE")]
    public static LispObject DotNetObjectType(LispObject arg)
        => arg is LispDotNetObject dno ? new LispDotNetObject(dno.Type) : Nil.Instance;

    public static LispObject DotNetToStream(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:TO-STREAM: requires at least 1 argument"));

        System.IO.Stream netStream;
        if (args[0] is LispDotNetObject dno && dno.Value is System.IO.Stream s)
            netStream = s;
        else
            throw new LispErrorException(new LispTypeError(
                "DOTNET:TO-STREAM: argument must be a .NET Stream", args[0]));

        // Check for :binary / :bivalent keyword arguments
        bool binary = false, bivalent = false;
        for (int i = 1; i < args.Length - 1; i += 2)
        {
            if (args[i] is Symbol kw && args[i + 1] != Nil.Instance)
            {
                if (kw.Name == "BINARY") binary = true;
                else if (kw.Name == "BIVALENT") bivalent = true;
            }
        }

        if (binary)
            return new LispBinaryStream(netStream);

        // Bivalent: a character stream that also serves read-byte/write-byte over the
        // same NetworkStream (SBCL-style), so byte-oriented protocol code (cl-rpc HTTP/WS)
        // works on a non-binary socket stream. No read-ahead, so char and byte reads
        // stay coordinated; UTF-8 with no BOM.
        if (bivalent)
            return new LispBidirectionalStream(
                new BivalentStreamReader(netStream), new BivalentStreamWriter(netStream));

        // BOM-less UTF-8 (encoderShouldEmitUTF8Identifier: false). Encoding.UTF8 emits a
        // BOM (EF BB BF) on the first write, which corrupts the head of a network/protocol
        // response (e.g. an HTTP/WebSocket reply written through a non-binary to-stream).
        var encoding = new System.Text.UTF8Encoding(false);

        var reader = new System.IO.StreamReader(netStream, encoding, false, 4096, leaveOpen: true);
        var writer = new System.IO.StreamWriter(netStream, encoding, 4096, leaveOpen: true)
        {
            AutoFlush = false
        };

        return new LispBidirectionalStream(reader, writer);
    }

    // --- Delegate marshal ---

    /// <summary>
    /// <lispdoc>(dotnet:make-delegate type-name function) -- Wrap a Lisp function as a .NET delegate. type-name is e.g. "System.Func`2[System.String,System.Boolean]". The delegate can be passed to any .NET method expecting that delegate type. LispFunction arguments are auto-converted via dotnet:call when the target parameter type is a delegate.</lispdoc>
    /// Wrap a Lisp <paramref name="fn"/> as a .NET delegate of <paramref name="delegateType"/>.
    /// Each call to the delegate marshals .NET args → LispObject, calls <paramref name="fn"/>,
    /// then marshals the LispObject result back to the delegate's return type.
    /// </summary>
    [LispDoc("DOTNET:MAKE-DELEGATE")]
    public static LispObject DotNetMakeDelegate(LispObject[] args)
    {
        if (args.Length != 2)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:MAKE-DELEGATE: requires 2 arguments (type-name function)"));

        if (args[1] is not LispFunction fn)
            throw new LispErrorException(new LispTypeError(
                "DOTNET:MAKE-DELEGATE: second argument must be a function", args[1]));

        // Accept a resolved System.Type (e.g. from dotnet:make-generic-type) as the
        // delegate type, in addition to a type-name string/symbol.
        Type delegateType;
        if (args[0] is LispDotNetObject tdno && tdno.Value is Type t0)
            delegateType = t0;
        else
        {
            string typeName = args[0] switch
            {
                LispString ls => ls.Value,
                Symbol s      => s.Name,
                _             => args[0].ToString() ?? ""
            };
            delegateType = ResolveDotNetType(typeName)
                ?? throw new LispErrorException(new LispProgramError(
                    $"DOTNET:MAKE-DELEGATE: cannot resolve type '{typeName}'"));
        }

        if (!typeof(Delegate).IsAssignableFrom(delegateType))
            throw new LispErrorException(new LispProgramError(
                $"DOTNET:MAKE-DELEGATE: '{delegateType.FullName}' is not a delegate type"));

        return new LispDotNetObject(CreateLispDelegate(fn, delegateType));
    }

    /// <summary>
    /// Build a .NET delegate of <paramref name="delegateType"/> that, when invoked,
    /// marshals its arguments to LispObject[], calls <paramref name="fn"/>, and
    /// marshals the return value back to the delegate's return type.
    /// Uses Expression.Lambda — no raw IL required.
    /// </summary>
    internal static Delegate CreateLispDelegate(LispFunction fn, Type delegateType)
    {
        var invokeMethod = delegateType.GetMethod("Invoke")
            ?? throw new InvalidOperationException(
                $"CreateLispDelegate: {delegateType} has no Invoke method");

        var paramInfos  = invokeMethod.GetParameters();
        var returnType  = invokeMethod.ReturnType;

        // Expression parameters matching the delegate signature
        var parameters = paramInfos
            .Select(p => System.Linq.Expressions.Expression.Parameter(p.ParameterType, p.Name))
            .ToArray();

        // Box each arg to object then call DotNetToLisp
        var dotNetToLisp = typeof(Runtime).GetMethod(nameof(DotNetToLisp))!;
        var lispArgs = parameters
            .Select(p => (System.Linq.Expressions.Expression)
                System.Linq.Expressions.Expression.Call(
                    dotNetToLisp,
                    System.Linq.Expressions.Expression.Convert(p, typeof(object))))
            .ToArray();

        // Runtime.InvokeForeignCallback(fn, new LispObject[] { ... }) — wraps
        // fn.Invoke so a Lisp error inside the callback is handled at the boundary
        // (dotcl:*foreign-callback-handler*) instead of crashing the .NET caller.
        var argsArray = System.Linq.Expressions.Expression.NewArrayInit(
            typeof(LispObject), lispArgs);
        var invokeForeign = typeof(Runtime).GetMethod(nameof(InvokeForeignCallback))!;
        var callFn = System.Linq.Expressions.Expression.Call(
            invokeForeign,
            System.Linq.Expressions.Expression.Constant(fn, typeof(LispObject)),
            argsArray);

        System.Linq.Expressions.Expression body;
        if (returnType == typeof(void))
        {
            // Action<…>: discard return value
            body = System.Linq.Expressions.Expression.Block(
                callFn,
                System.Linq.Expressions.Expression.Empty());
        }
        else
        {
            // Func<…,TResult>: marshal LispObject result → TResult
            var lispToDotNet = typeof(Runtime)
                .GetMethod(nameof(LispToDotNet), new[] { typeof(LispObject), typeof(Type) })!;
            var converted = System.Linq.Expressions.Expression.Call(
                lispToDotNet,
                callFn,
                System.Linq.Expressions.Expression.Constant(returnType));
            body = System.Linq.Expressions.Expression.Convert(converted, returnType);
        }

        return System.Linq.Expressions.Expression.Lambda(delegateType, body, parameters)
            .Compile();
    }

    private static Symbol? _foreignCbHandlerSym;
    private static Symbol ForeignCbHandlerSym =>
        _foreignCbHandlerSym ??= Startup.SymInPkg("*FOREIGN-CALLBACK-HANDLER*", "DOTCL");

    /// <summary>
    /// Invoke a Lisp function at a C#→Lisp callback boundary (a delegate built by
    /// CreateLispDelegate, an event handler, or a dotnet:%define-class method
    /// override), keeping a Lisp error from tearing through the host. Such errors
    /// otherwise escape as LispErrorException → TargetInvocationException and crash
    /// the .NET caller (e.g. a MonoGame Game.Run loop calling an overridden Draw).
    ///
    /// On a LispErrorException the condition is handed to dotcl:*foreign-callback-
    /// handler* — a function of one argument (the condition) whose return value
    /// becomes the callback's result. When that variable is NIL (the default) the
    /// condition is reported to *error-output* and NIL is returned, so the marshal
    /// layer yields the return type's default and the host keeps running. Non-Lisp
    /// .NET exceptions are NOT caught here; they propagate as before.
    /// </summary>
    public static LispObject InvokeForeignCallback(LispObject fn, LispObject[] args)
    {
        // Establish a handler-bind for ERROR around the callback (like handler-case)
        // so the condition is intercepted at the SIGNAL point — before the error
        // function would invoke the debugger. That matters because the host caller
        // is typically a non-interactive loop (a 60fps Game.Run, an event handler);
        // entering the debugger there would hang. The handler unwinds to the catch
        // below carrying the ORIGINAL condition.
        var tag = new object();
        var handler = new LispFunction(
            hargs => throw new HandlerCaseInvocationException(
                tag, 0, hargs.Length > 0 ? hargs[0] : Nil.Instance),
            "%FOREIGN-CALLBACK-BOUNDARY", -1);
        HandlerClusterStack.PushCluster(new[] { new HandlerBinding(Startup.Sym("ERROR"), handler) });
        try
        {
            return Funcall(fn, args);
        }
        catch (HandlerCaseInvocationException ex) when (ReferenceEquals(ex.Tag, tag))
        {
            return HandleForeignCallbackError(
                ex.Condition as LispCondition ?? new LispError(ex.Condition.ToString() ?? "error"));
        }
        catch (LispErrorException ex)
        {
            // A LispErrorException that bypassed the handler-bind (e.g. signaled with
            // no ERROR match, or thrown directly) is still handled at the boundary.
            return HandleForeignCallbackError(ex.Condition);
        }
        finally
        {
            HandlerClusterStack.PopCluster();
        }
    }

    private static LispObject HandleForeignCallbackError(LispCondition condition)
    {
        var handler = DynamicBindings.Get(ForeignCbHandlerSym);
        if (handler is not Nil)
        {
            // A user handler decides the callback's result. If it is not actually
            // funcallable or itself errors, fall through to the default report
            // rather than re-crossing the boundary.
            try { return Funcall(handler, condition); }
            catch (LispErrorException) { }
        }
        // Default: report and continue with NIL so the host is not torn down.
        try
        {
            var errOut = DynamicBindings.Get(Startup.SymInPkg("*ERROR-OUTPUT*", "COMMON-LISP"));
            var writer = (errOut as LispOutputStream)?.Writer
                         ?? (errOut as LispBidirectionalStream)?.Writer;
            var msg = $";; Unhandled error in foreign callback: {condition}";
            if (writer != null) writer.WriteLine(msg);
            else Console.Error.WriteLine(msg);
        }
        catch { /* never let reporting itself escape the boundary */ }
        return Nil.Instance;
    }

    /// <summary>
    /// <lispdoc>(dotnet:call-out type-or-obj "Method" &amp;rest in-args) -- Call a .NET method that has out/ref parameters. type-or-obj is a type-name string for static calls, or a .NET object for instance calls. in-args supplies only the non-out parameters. Returns multiple values: the method's return value (T for void), followed by each out/ref parameter value in declaration order. Example: (multiple-value-bind (ok n) (dotnet:call-out "System.Int32" "TryParse" "42") ...)</lispdoc>
    /// Invoke a .NET static or instance method that has <c>out</c>/<c>ref</c> parameters.
    /// Supply only the in (non-out) arguments from Lisp; out positions are filled automatically.
    /// Returns multiple values: return-value (T for void) followed by each out/ref value.
    /// </summary>
    [LispDoc("DOTNET:CALL-OUT")]
    public static LispObject DotNetCallOut(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:CALL-OUT: requires type-or-obj method-name &rest in-args"));

        string memberName = args[1] switch { LispString ls => ls.Value, _ => args[1].ToString() ?? "" };
        var lispInArgs   = args.Skip(2).ToArray();

        System.Reflection.MethodInfo method;
        object? target;
        Type    type;

        if (args[0] is LispDotNetObject dno)
        {
            // Instance call
            target = dno.Value;
            type   = target.GetType();
            method = FindOutMethod(type, memberName, lispInArgs.Length,
                         System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance)
                     ?? throw new LispErrorException(new LispError(
                         $"DOTNET:CALL-OUT: no instance method {type.Name}.{memberName} " +
                         $"with {lispInArgs.Length} in-parameter(s)"));
        }
        else
        {
            // Static call
            string typeName = args[0] switch { LispString ls => ls.Value, _ => args[0].ToString() ?? "" };
            type   = ResolveDotNetType(typeName);
            target = null;
            method = FindOutMethod(type, memberName, lispInArgs.Length,
                         System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Static)
                     ?? throw new LispErrorException(new LispError(
                         $"DOTNET:CALL-OUT: no static method {typeName}.{memberName} " +
                         $"with {lispInArgs.Length} in-parameter(s)"));
        }

        var paramInfos = method.GetParameters();
        var callArgs   = new object?[paramInfos.Length];
        int inIdx = 0;
        for (int i = 0; i < paramInfos.Length; i++)
        {
            var p = paramInfos[i];
            if (p.IsOut || (p.ParameterType.IsByRef && !p.IsIn))
            {
                callArgs[i] = null; // placeholder; filled by .NET on return
            }
            else
            {
                var elemType = p.ParameterType.IsByRef ? p.ParameterType.GetElementType()! : p.ParameterType;
                callArgs[i] = LispToDotNet(lispInArgs[inIdx++], elemType);
            }
        }

        object? returnVal;
        try
        {
            returnVal = method.Invoke(target, callArgs);
        }
        catch (System.Reflection.TargetInvocationException tie)
        {
            throw DotNetInvokeError($"DOTNET:CALL-OUT {type.Name}.{memberName}", tie);
        }

        var mvVals = new System.Collections.Generic.List<LispObject>();
        mvVals.Add(method.ReturnType == typeof(void) ? T.Instance : DotNetToLisp(returnVal));
        for (int i = 0; i < paramInfos.Length; i++)
        {
            if (paramInfos[i].IsOut || paramInfos[i].ParameterType.IsByRef)
                mvVals.Add(DotNetToLisp(callArgs[i]));
        }
        return MultipleValues.Values(mvVals.ToArray());
    }

    private static System.Reflection.MethodInfo? FindOutMethod(
        Type type, string name, int inArgCount, System.Reflection.BindingFlags flags)
    {
        return type.GetMethods(flags)
            .Where(m => m.Name == name)
            .FirstOrDefault(m => {
                var ps = m.GetParameters();
                var inCount = ps.Count(p => !p.IsOut && !(p.ParameterType.IsByRef && !p.IsIn));
                return inCount == inArgCount;
            });
    }

    /// <summary>
    /// <lispdoc>(dotnet:call-out-generic type-or-obj "Method" type-args-list &amp;rest in-args) -- Call a generic .NET method that has out/ref parameters. Combines generic type-argument instantiation (MakeGenericMethod) with out/ref handling: type-or-obj is a type-name string for static calls or a .NET object for instance calls; type-args-list is a Lisp list of type-name strings; in-args supplies only the non-out parameters. Returns multiple values: the method's return value (T for void) followed by each out/ref parameter value in declaration order. Example: (multiple-value-bind (ok day) (dotnet:call-out-generic "System.Enum" "TryParse" '("System.DayOfWeek") "Monday") ...)</lispdoc>
    /// Generic counterpart of dotnet:call-out: resolve an open generic method
    /// definition with explicit type arguments (MakeGenericMethod), then invoke it
    /// handling out/ref parameters like call-out. This is the combined
    /// generic + out/ref path requested in dotcl/dotcl#45.
    /// </summary>
    [LispDoc("DOTNET:CALL-OUT-GENERIC")]
    public static LispObject DotNetCallOutGeneric(LispObject[] args)
    {
        if (args.Length < 3)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:CALL-OUT-GENERIC: requires type-or-obj method-name type-args-list &rest in-args"));

        string memberName = args[1] switch { LispString ls => ls.Value, _ => args[1].ToString() ?? "" };

        // Parse type-args list (a Lisp list of type-name strings).
        var typeArgNames = new System.Collections.Generic.List<string>();
        var cursor = args[2];
        while (cursor is Cons c)
        {
            typeArgNames.Add(c.Car switch { LispString ls => ls.Value, _ => c.Car.ToString() ?? "" });
            cursor = c.Cdr;
        }

        var lispInArgs = args.Skip(3).ToArray();

        object? target;
        Type    type;
        System.Reflection.BindingFlags flags;
        if (args[0] is LispDotNetObject dno)
        {
            target = dno.Value;
            type   = target.GetType();
            flags  = System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance;
        }
        else
        {
            string typeName = args[0] switch { LispString ls => ls.Value, _ => args[0].ToString() ?? "" };
            type   = ResolveDotNetType(typeName);
            target = null;
            flags  = System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Static;
        }

        // Find the open generic method definition: name + generic arity + in-param count.
        // in-param count excludes out and by-ref-out parameters (same rule as call-out),
        // and is computed on the OPEN definition (IsByRef/IsOut are visible there).
        var methodDef = type.GetMethods(flags)
            .Where(m => m.Name == memberName
                     && m.IsGenericMethodDefinition
                     && m.GetGenericArguments().Length == typeArgNames.Count)
            .FirstOrDefault(m => {
                var ps = m.GetParameters();
                var inCount = ps.Count(p => !p.IsOut && !(p.ParameterType.IsByRef && !p.IsIn));
                return inCount == lispInArgs.Length;
            })
            ?? throw new LispErrorException(new LispError(
                $"DOTNET:CALL-OUT-GENERIC: no generic method {type.Name}.{memberName} " +
                $"with {typeArgNames.Count} type arg(s) and {lispInArgs.Length} in-parameter(s)"));

        var concreteTypes = typeArgNames.Select(ResolveDotNetType).ToArray();
        System.Reflection.MethodInfo method;
        try { method = methodDef.MakeGenericMethod(concreteTypes); }
        catch (Exception e)
        {
            throw new LispErrorException(new LispError(
                $"DOTNET:CALL-OUT-GENERIC {type.Name}.{memberName}: MakeGenericMethod failed: {e.Message}"));
        }

        var paramInfos = method.GetParameters();
        var callArgs   = new object?[paramInfos.Length];
        int inIdx = 0;
        for (int i = 0; i < paramInfos.Length; i++)
        {
            var p = paramInfos[i];
            if (p.IsOut || (p.ParameterType.IsByRef && !p.IsIn))
                callArgs[i] = null; // placeholder; filled by .NET on return
            else
            {
                var elemType = p.ParameterType.IsByRef ? p.ParameterType.GetElementType()! : p.ParameterType;
                callArgs[i] = LispToDotNet(lispInArgs[inIdx++], elemType);
            }
        }

        object? returnVal;
        try
        {
            returnVal = method.Invoke(target, callArgs);
        }
        catch (System.Reflection.TargetInvocationException tie)
        {
            throw DotNetInvokeError($"DOTNET:CALL-OUT-GENERIC {type.Name}.{memberName}", tie);
        }

        var mvVals = new System.Collections.Generic.List<LispObject>();
        mvVals.Add(method.ReturnType == typeof(void) ? T.Instance : DotNetToLisp(returnVal));
        for (int i = 0; i < paramInfos.Length; i++)
        {
            if (paramInfos[i].IsOut || paramInfos[i].ParameterType.IsByRef)
                mvVals.Add(DotNetToLisp(callArgs[i]));
        }
        return MultipleValues.Values(mvVals.ToArray());
    }

    /// <summary>
    /// <lispdoc>(dotnet:static-generic "TypeName" "MethodName" type-args-list &amp;rest args) -- Call a generic static method with explicit type arguments. type-args-list is a Lisp list of type-name strings. Uses type-guided conversion so Lisp lambdas are auto-marshaled to the concrete delegate type. Example: (dotnet:static-generic "System.Linq.Enumerable" "Where" '("System.Int32") list (lambda (x) (> x 3)))</lispdoc>
    /// Invoke a generic static method with explicit type arguments.
    /// <paramref name="args"/>[0] = type name, [1] = method name,
    /// [2] = Lisp list of type-arg strings, [3..] = method arguments.
    /// Uses type-guided conversion, so LispFunction args are auto-marshaled to
    /// the concrete delegate types inferred from <c>MakeGenericMethod</c>.
    /// </summary>
    [LispDoc("DOTNET:STATIC-GENERIC")]
    public static LispObject DotNetStaticGeneric(LispObject[] args)
    {
        if (args.Length < 3)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:STATIC-GENERIC: requires type-name method-name type-args-list &rest args"));

        string typeName   = args[0] switch { LispString ls => ls.Value, _ => args[0].ToString() ?? "" };
        string memberName = args[1] switch { LispString ls => ls.Value, _ => args[1].ToString() ?? "" };
        var    type       = ResolveDotNetType(typeName);
        var    lispArgs   = args.Skip(3).ToArray();

        // Parse type-args list
        var typeArgNames = new System.Collections.Generic.List<string>();
        var cursor = args[2];
        while (cursor is Cons c)
        {
            typeArgNames.Add(c.Car switch { LispString ls => ls.Value, _ => c.Car.ToString() ?? "" });
            cursor = c.Cdr;
        }

        // Find the generic method definition matching name + arity
        var methodDef = type.GetMethods(
                System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Static)
            .Where(m => m.Name == memberName
                     && m.IsGenericMethodDefinition
                     && m.GetGenericArguments().Length == typeArgNames.Count
                     && m.GetParameters().Length == lispArgs.Length)
            .FirstOrDefault()
            ?? throw new LispErrorException(new LispError(
                $"DOTNET:STATIC-GENERIC: no generic static method {typeName}.{memberName} " +
                $"with {typeArgNames.Count} type arg(s) and {lispArgs.Length} parameter(s)"));

        var concreteTypes  = typeArgNames.Select(ResolveDotNetType).ToArray();
        var concreteMethod = methodDef.MakeGenericMethod(concreteTypes);
        var paramInfos     = concreteMethod.GetParameters();

        var callArgs = new object?[lispArgs.Length];
        for (int i = 0; i < lispArgs.Length; i++)
            callArgs[i] = LispToDotNet(lispArgs[i], paramInfos[i].ParameterType);

        try
        {
            var result = concreteMethod.Invoke(null, callArgs);
            return DotNetToLisp(result);
        }
        catch (System.Reflection.TargetInvocationException tie)
        {
            throw DotNetInvokeError($"DOTNET:STATIC-GENERIC {typeName}.{memberName}", tie);
        }
    }

    /// <summary>(dotnet:invoke-generic object "Method" '("TypeArg" ...) &rest args)
    /// Instance counterpart of dotnet:static-generic: invoke a generic instance method on
    /// OBJECT, instantiating it with the given type-argument names (MakeGenericMethod)
    /// before the call (e.g. ContentManager.Load&lt;Texture2D&gt;) (dotcl/dotcl#23).</summary>
    public static LispObject DotNetInvokeGeneric(LispObject[] args)
    {
        if (args.Length < 3)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:INVOKE-GENERIC: requires object method-name type-args-list &rest args"));
        if (args[0] is not LispDotNetObject dno)
            throw new LispErrorException(new LispTypeError(
                "DOTNET:INVOKE-GENERIC: first argument must be a .NET object", args[0]));

        var    target     = dno.Value;
        var    type       = target.GetType();
        string memberName = args[1] switch { LispString ls => ls.Value, _ => args[1].ToString() ?? "" };
        var    lispArgs   = args.Skip(3).ToArray();

        // Parse type-args list
        var typeArgNames = new System.Collections.Generic.List<string>();
        var cursor = args[2];
        while (cursor is Cons c)
        {
            typeArgNames.Add(c.Car switch { LispString ls => ls.Value, _ => c.Car.ToString() ?? "" });
            cursor = c.Cdr;
        }

        // Find the generic instance method matching name + type-arg arity + param count
        var methodDef = type.GetMethods(
                System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance)
            .Where(m => m.Name == memberName
                     && m.IsGenericMethodDefinition
                     && m.GetGenericArguments().Length == typeArgNames.Count
                     && m.GetParameters().Length == lispArgs.Length)
            .FirstOrDefault()
            ?? throw new LispErrorException(new LispError(
                $"DOTNET:INVOKE-GENERIC: no generic instance method {type.Name}.{memberName} " +
                $"with {typeArgNames.Count} type arg(s) and {lispArgs.Length} parameter(s)"));

        var concreteTypes  = typeArgNames.Select(ResolveDotNetType).ToArray();
        var concreteMethod = methodDef.MakeGenericMethod(concreteTypes);
        var paramInfos     = concreteMethod.GetParameters();

        var callArgs = new object?[lispArgs.Length];
        for (int i = 0; i < lispArgs.Length; i++)
            callArgs[i] = LispToDotNet(lispArgs[i], paramInfos[i].ParameterType);

        try
        {
            var result = concreteMethod.Invoke(target, callArgs);
            return DotNetToLisp(result);
        }
        catch (System.Reflection.TargetInvocationException tie)
        {
            throw DotNetInvokeError($"DOTNET:INVOKE-GENERIC {type.Name}.{memberName}", tie);
        }
    }
}
