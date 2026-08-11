namespace DotCL;

public class LispCondition : LispObject
{
    private string? _message;
    private System.Func<string>? _messageThunk;

    /// <summary>
    /// The condition's message. May be supplied as a thunk, in which case it is
    /// rendered on first access rather than at construction. WARN and friends build
    /// this by running the caller's format control over the caller's arguments, and
    /// those arguments are arbitrary objects: rendering them when the condition is
    /// created runs the printer under whatever printer variables happened to be in
    /// effect at the signalling site, not the ones in effect where the condition is
    /// finally reported. SBCL's compiler relies on the difference — it binds
    /// *PRINT-CIRCLE* to T around reporting, and the type objects it reports on hold
    /// each other, so rendering early printed a cycle with no circle detection and
    /// never came back.
    /// </summary>
    public string Message
    {
        get
        {
            if (_message == null)
            {
                var thunk = _messageThunk;
                _messageThunk = null;   // render once
                _message = thunk != null ? thunk() : "";
            }
            return _message;
        }
    }
    /// <summary>CL condition type name (used for TYPE-OF). Defaults to "CONDITION".</summary>
    public string ConditionTypeName { get; set; } = "CONDITION";
    /// <summary>Original format control string (for simple-condition-format-control).</summary>
    public LispObject FormatControl { get; set; } = Nil.Instance;
    /// <summary>Original format arguments list (for simple-condition-format-arguments).</summary>
    public LispObject FormatArguments { get; set; } = Nil.Instance;
    /// <summary>Package reference for PACKAGE-ERROR conditions.</summary>
    public LispObject? PackageRef { get; set; }
    /// <summary>Pathname reference for FILE-ERROR conditions.</summary>
    public LispObject? FileErrorPathnameRef { get; set; }
    /// <summary>Stream reference for STREAM-ERROR conditions.</summary>
    public LispObject? StreamErrorStreamRef { get; set; }
    /// <summary>Operation reference for ARITHMETIC-ERROR conditions.</summary>
    public LispObject? OperationRef { get; set; }
    /// <summary>Operands reference for ARITHMETIC-ERROR conditions.</summary>
    public LispObject? OperandsRef { get; set; }
    /// <summary>For a condition wrapping a raw .NET exception: the original CLR
    /// exception type, so dotnet:exception-type / dotnet:handler-bind can dispatch
    /// on the specific .NET type. Null for ordinary Lisp conditions. (dotcl/dotcl#45)</summary>
    public System.Type? ClrExceptionType { get; set; }
    /// <summary>For a condition wrapping a raw .NET exception: the exception
    /// instance itself, exposed via dotnet:exception-object so handlers can read
    /// type-specific detail the message loses — SocketException.SocketErrorCode,
    /// the InnerException chain (an IOException around a socket timeout), etc.
    /// Null for ordinary Lisp conditions.</summary>
    public System.Exception? ClrException { get; set; }
    public LispCondition(string message) => _message = message;
    public LispCondition(System.Func<string> messageThunk) => _messageThunk = messageThunk;
    public override string ToString() => $"#<{ConditionTypeName}: {Message}>";
}

public class LispError : LispCondition
{
    public LispError(string message) : base(message) { ConditionTypeName = "ERROR"; }
    public override string ToString() => $"#<ERROR: {Message}>";
}

public class LispTypeError : LispError
{
    // Settable so MOP slot-access (setf slot-value / slot-makunbound) can reach
    // the DATUM / EXPECTED-TYPE slots of a native (runtime-signaled) type-error.
    public LispObject Datum { get; set; }
    public LispObject ExpectedType { get; set; }

    public LispTypeError(string message, LispObject? datum = null, LispObject? expectedType = null)
        : base(message)
    {
        ConditionTypeName = "TYPE-ERROR";
        Datum = datum ?? Nil.Instance;
        ExpectedType = expectedType ?? Nil.Instance;
    }
}

public class LispProgramError : LispError
{
    public LispProgramError(string message) : base(message) {
        ConditionTypeName = "PROGRAM-ERROR";
    }
    public override string ToString() => $"#<PROGRAM-ERROR: {Message}>";
}

/// <summary>STORAGE-CONDITION (stack or heap exhaustion). Deliberately NOT a
/// LispError: the spec places it under SERIOUS-CONDITION but outside ERROR, so
/// handler-case (error ...) must not catch it — matching SBCL, whose
/// control-stack-exhausted / heap-exhausted-error are pure storage-conditions.</summary>
public class LispStorageCondition : LispCondition
{
    public LispStorageCondition(string message) : base(message)
    {
        ConditionTypeName = "STORAGE-CONDITION";
    }
    public override string ToString() => $"#<STORAGE-CONDITION: {Message}>";
}

public class LispCellError : LispError
{
    public LispObject Name { get; set; }
    public LispCellError(string message, LispObject name) : base(message) { Name = name; }
}

public class LispUndefinedFunction : LispCellError
{
    public LispUndefinedFunction(LispObject name)
        : base($"Undefined function: {name}", name) { ConditionTypeName = "UNDEFINED-FUNCTION"; }
    public override string ToString() => $"#<UNDEFINED-FUNCTION: {Name}>";
}

public class LispUnboundVariable : LispCellError
{
    public LispUnboundVariable(LispObject name)
        : base($"Unbound variable: {name}", name) { ConditionTypeName = "UNBOUND-VARIABLE"; }
    public override string ToString() => $"#<UNBOUND-VARIABLE: {Name}>";
}

public class LispControlError : LispError
{
    public LispControlError(string message) : base(message) { ConditionTypeName = "CONTROL-ERROR"; }
    public override string ToString() => $"#<CONTROL-ERROR: {Message}>";
}

public class LispWarning : LispCondition
{
    public LispWarning(string message) : base(message) { ConditionTypeName = "WARNING"; }
    public LispWarning(System.Func<string> messageThunk) : base(messageThunk) { ConditionTypeName = "WARNING"; }
    public override string ToString() => $"#<WARNING: {Message}>";
}

/// <summary>
/// Wraps a CLOS LispInstance as a LispCondition for the condition system.
/// Used by define-condition which expands to defclass.
/// </summary>
public class LispInstanceCondition : LispCondition
{
    public LispInstance Instance { get; }
    public LispInstanceCondition(LispInstance instance)
        : base(instance.ToString())
    {
        Instance = instance;
        ConditionTypeName = instance.Class.Name.Name;
    }
    public override string ToString() => Instance.ToString();
}

/// <summary>Signaled when the user interrupts evaluation with Ctrl-C.</summary>
public class LispInteractiveInterrupt : LispCondition
{
    public LispInteractiveInterrupt()
        : base("Interactive interrupt") { ConditionTypeName = "INTERACTIVE-INTERRUPT"; }
}

public class LispErrorException : Exception
{
    public LispCondition Condition { get; }
    public LispErrorException(LispCondition condition)
        : base(condition.Message)
    {
        Condition = condition;
        // Per CL spec: error signals the condition through handler-bind before throwing.
        // If a handler does a non-local exit, this constructor never returns.
        HandlerClusterStack.Signal(condition);
    }
}

/// <summary>
/// Thrown by a handler-case clause's handler function in HandlerClusterStack.Signal,
/// to perform the non-local exit back to the handler-case's catch block.
/// </summary>
public class HandlerCaseInvocationException : Exception
{
    public object Tag { get; }
    public int ClauseIndex { get; }
    public LispObject Condition { get; }
    public HandlerCaseInvocationException(object tag, int clauseIndex, LispObject condition)
        : base("handler-case invoked")
    {
        Tag = tag;
        ClauseIndex = clauseIndex;
        Condition = condition;
    }
}

public class LispRestart : LispObject
{
    public string Name { get; }
    public Func<LispObject[], LispObject> Handler { get; }
    public string? Description { get; }
    public object Tag { get; }
    public bool IsBindRestart { get; }
    public LispObject? InteractiveFunction { get; set; }
    public LispObject? ReportFunction { get; set; }
    public Symbol? NameSymbol { get; set; }
    public LispObject? TestFunction { get; set; }

    public LispRestart(string name, Func<LispObject[], LispObject> handler,
                       string? description = null, object? tag = null, bool isBindRestart = false)
    {
        Name = name;
        Handler = handler;
        Description = description;
        Tag = tag ?? new object();
        IsBindRestart = isBindRestart;
    }

    public override string ToString()
    {
        if (ReportFunction != null)
        {
            try
            {
                var stream = new LispStringOutputStream(new System.IO.StringWriter());
                Runtime.Funcall(ReportFunction, stream);
                return stream.GetString();
            }
            catch { }
        }
        return $"#<RESTART {Name}>";
    }
}

/// <summary>
/// A handler binding: Lisp type specifier + handler function.
/// Used by handler-bind for non-unwinding handlers.
/// </summary>
public class HandlerBinding
{
    public LispObject TypeSpec { get; }
    public LispFunction Handler { get; }
    public HandlerBinding(LispObject typeSpec, LispFunction handler)
    {
        TypeSpec = typeSpec;
        Handler = handler;
    }
}

/// <summary>
/// Handler cluster stack: Lisp typep-based handler dispatch.
/// Each cluster is an array of HandlerBindings established by one handler-bind.
/// </summary>
public static class HandlerClusterStack
{
    [ThreadStatic]
    private static List<HandlerBinding[]>? _clusters;

    public static void PushCluster(HandlerBinding[] cluster)
    {
        _clusters ??= new();
        _clusters.Add(cluster);
    }

    public static void PopCluster()
    {
        if (_clusters?.Count > 0)
            _clusters.RemoveAt(_clusters.Count - 1);
    }

    /// <summary>Current cluster-stack depth.</summary>
    public static int Depth => _clusters?.Count ?? 0;

    /// <summary>Shallow copy of the live cluster stack (bottom→top), or null if
    /// empty. Used to carry handler-bind clusters across an async await boundary,
    /// where the continuation runs on a different (ThreadStatic) thread.</summary>
    public static List<HandlerBinding[]>? Snapshot()
        => (_clusters == null || _clusters.Count == 0)
            ? null : new List<HandlerBinding[]>(_clusters);

    /// <summary>Re-install a snapshot on top of the current stack.</summary>
    public static void Restore(List<HandlerBinding[]>? snapshot)
    {
        if (snapshot == null || snapshot.Count == 0) return;
        _clusters ??= new();
        _clusters.AddRange(snapshot);
    }

    /// <summary>Pop clusters down to DEPTH (no-op if already at or below it).</summary>
    public static void TruncateTo(int depth)
    {
        var c = _clusters;
        if (c == null) return;
        if (depth < 0) depth = 0;
        while (c.Count > depth) c.RemoveAt(c.Count - 1);
    }

    /// <summary>
    /// Signal a condition through the handler stack.
    /// Matching handlers are called without unwinding.
    /// If a handler returns normally, it declines and the next handler is tried.
    /// Per CL spec: when calling a handler, that handler's cluster and above are removed
    /// to prevent infinite recursion.
    /// </summary>
    public static void Signal(LispCondition condition)
    {
        if (_clusters == null) return;
        for (int i = _clusters.Count - 1; i >= 0; i--)
        {
            var cluster = _clusters[i];
            foreach (var binding in cluster)
            {
                if (Runtime.IsTruthy(Runtime.Typep(condition, binding.TypeSpec)))
                {
                    // Remove this cluster and above during handler call
                    var saved = new List<HandlerBinding[]>();
                    for (int j = _clusters.Count - 1; j >= i; j--)
                    {
                        saved.Add(_clusters[j]);
                        _clusters.RemoveAt(j);
                    }
                    try
                    {
                        binding.Handler.Invoke(condition);
                        // Handler returned normally → decline, restore and continue
                    }
                    finally
                    {
                        // Restore clusters
                        saved.Reverse();
                        _clusters.AddRange(saved);
                    }
                }
            }
        }
    }
}

/// <summary>
/// Restart cluster stack for restart-case.
/// </summary>
public static class RestartClusterStack
{
    [ThreadStatic]
    private static List<LispRestart[]>? _clusters;

    public static void PushCluster(LispRestart[] cluster)
    {
        _clusters ??= new();
        _clusters.Add(cluster);
    }

    public static void PopCluster()
    {
        if (_clusters?.Count > 0)
            _clusters.RemoveAt(_clusters.Count - 1);
    }

    public static int Depth => _clusters?.Count ?? 0;

    /// <summary>Shallow copy of the live restart-cluster stack (bottom→top), or
    /// null if empty. Carries restart-case clusters across an async await
    /// boundary so find-restart / compute-restarts / invoke-restart see them on
    /// the continuation thread (which has fresh ThreadStatic stacks). Mirrors
    /// HandlerClusterStack.Snapshot.</summary>
    public static List<LispRestart[]>? Snapshot()
        => (_clusters == null || _clusters.Count == 0)
            ? null : new List<LispRestart[]>(_clusters);

    /// <summary>Re-install a snapshot on top of the current stack.</summary>
    public static void Restore(List<LispRestart[]>? snapshot)
    {
        if (snapshot == null || snapshot.Count == 0) return;
        _clusters ??= new();
        _clusters.AddRange(snapshot);
    }

    /// <summary>Pop clusters down to DEPTH (no-op if already at or below it).</summary>
    public static void TruncateTo(int depth)
    {
        var c = _clusters;
        if (c == null) return;
        if (depth < 0) depth = 0;
        while (c.Count > depth) c.RemoveAt(c.Count - 1);
    }

    [ThreadStatic]
    private static List<(LispObject Condition, LispRestart Restart)>? _conditionRestarts;

    public static LispObject GetTopClusterRestarts()
    {
        if (_clusters == null || _clusters.Count == 0) return Nil.Instance;
        var top = _clusters[_clusters.Count - 1];
        LispObject result = Nil.Instance;
        for (int i = top.Length - 1; i >= 0; i--)
            result = new Cons(top[i], result);
        return result;
    }

    public static void AssociateConditionRestarts(LispObject condition, LispObject restartList)
    {
        _conditionRestarts ??= new List<(LispObject, LispRestart)>();
        var current = restartList;
        while (current is Cons c)
        {
            if (c.Car is LispRestart restart)
                _conditionRestarts.Add((condition, restart));
            current = c.Cdr;
        }
    }

    public static void DisassociateConditionRestarts(LispObject condition, LispObject restartList)
    {
        if (_conditionRestarts == null) return;
        var current = restartList;
        while (current is Cons c)
        {
            if (c.Car is LispRestart restart)
                _conditionRestarts.RemoveAll(pair =>
                    ReferenceEquals(pair.Condition, condition) &&
                    ReferenceEquals(pair.Restart, restart));
            current = c.Cdr;
        }
    }

    private static bool IsAssociatedWith(LispRestart restart, LispObject condition)
    {
        if (_conditionRestarts == null) return false;
        return _conditionRestarts.Exists(pair =>
            ReferenceEquals(pair.Condition, condition) &&
            ReferenceEquals(pair.Restart, restart));
    }

    private static bool IsAssociatedWithAny(LispRestart restart)
    {
        if (_conditionRestarts == null) return false;
        return _conditionRestarts.Exists(pair =>
            ReferenceEquals(pair.Restart, restart));
    }

    public static LispRestart? FindRestartByName(string name, LispObject? condition = null)
    {
        if (_clusters == null) return null;
        for (int i = _clusters.Count - 1; i >= 0; i--)
        {
            foreach (var r in _clusters[i])
            {
                if (r.Name == name)
                {
                    if (condition == null || condition is Nil)
                    {
                        // No condition: check test function with nil
                        if (r.TestFunction != null)
                        {
                            var result = Runtime.Funcall(r.TestFunction, condition ?? Nil.Instance);
                            if (result is Nil) continue;
                        }
                        return r;
                    }
                    // Has condition: check test function first
                    if (r.TestFunction != null)
                    {
                        var result = Runtime.Funcall(r.TestFunction, condition);
                        if (result is Nil) continue;
                        return r;
                    }
                    // No test function: use association logic
                    if (IsAssociatedWith(r, condition) || !IsAssociatedWithAny(r))
                        return r;
                }
            }
        }
        return null;
    }

    public static LispRestart? FindRestart(LispObject nameOrRestart, LispObject? condition = null)
    {
        if (nameOrRestart is LispRestart restart)
        {
            if (_clusters == null) return null;
            for (int i = _clusters.Count - 1; i >= 0; i--)
                foreach (var r in _clusters[i])
                    if (ReferenceEquals(r, restart))
                    {
                        if (condition == null || condition is Nil)
                        {
                            if (r.TestFunction != null)
                            {
                                var testResult = Runtime.Funcall(r.TestFunction, condition ?? Nil.Instance);
                                if (testResult is Nil) return null;
                            }
                            return r;
                        }
                        if (r.TestFunction != null)
                        {
                            var testResult = Runtime.Funcall(r.TestFunction, condition);
                            if (testResult is Nil) return null;
                            return r;
                        }
                        if (IsAssociatedWith(r, condition) || !IsAssociatedWithAny(r))
                            return r;
                        return null;
                    }
            return null;
        }
        string name = nameOrRestart switch
        {
            Symbol sym => sym.Name,
            LispString s => s.Value,
            _ => nameOrRestart.ToString() ?? ""
        };
        return FindRestartByName(name, condition);
    }

    public static LispObject ComputeRestarts(LispObject? condition = null)
    {
        if (_clusters == null) return Nil.Instance;
        LispObject result = Nil.Instance;
        // Iterate from oldest cluster (0) to newest (Count-1),
        // and within each cluster from last to first.
        // Prepending with Cons produces: newest-cluster's first restart at head.
        for (int i = 0; i < _clusters.Count; i++)
        {
            var cluster = _clusters[i];
            for (int j = cluster.Length - 1; j >= 0; j--)
            {
                var r = cluster[j];
                if (condition != null && condition is not Nil)
                {
                    // Check test function
                    if (r.TestFunction != null)
                    {
                        var testResult = Runtime.Funcall(r.TestFunction, condition);
                        if (testResult is Nil) continue;
                    }
                    else if (!IsAssociatedWith(r, condition) && IsAssociatedWithAny(r))
                        continue;
                }
                result = new Cons(r, result);
            }
        }
        return result;
    }
}

public class RestartInvocationException : Exception
{
    public object Tag { get; }
    public LispObject[] Arguments { get; }
    public RestartInvocationException(object tag, LispObject[] arguments)
        : base("Restart invoked")
    {
        Tag = tag;
        Arguments = arguments;
    }
}

public static class ConditionSystem
{
    // --- Ctrl-C interrupt delivery ---

    private static volatile bool _interruptRequested = false;

    /// <summary>Request interrupt delivery (called from Console.CancelKeyPress on another thread).</summary>
    public static void RequestInterrupt() => _interruptRequested = true;

    /// <summary>
    /// Check and deliver a pending interrupt. Called periodically from hot paths (LispFunction.Invoke).
    /// When fired, signals INTERACTIVE-INTERRUPT through the condition system.
    /// </summary>
    internal static void CheckInterrupt()
    {
        if (System.Threading.Volatile.Read(ref Runtime.SafepointInterruptsPending) != 0)
            Runtime.RunPendingInterruptsAtSafepoint();
        if (!_interruptRequested) return;
        _interruptRequested = false;
        Error(new LispInteractiveInterrupt());
    }

    /// <summary>Diagnostic counter for PollInterrupt: how many loop back-edge
    /// safepoints have executed. Read from Lisp via dotnet:static; regression
    /// tests use the delta to prove a loop is (or is not) emitting polls.</summary>
    public static long PollCount;

    /// <summary>
    /// Loop back-edge safepoint. The compiler emits a call to this on the
    /// back-edge of compiled loops (tagbody dispatch, TCO self/mutual loops) so
    /// a loop whose body contains no Lisp calls — and therefore never reaches
    /// the periodic check in LispFunction.Invoke — can still be stopped by
    /// Ctrl-C. Bodies declared (optimize (safety 0)) opt out at compile time.
    /// </summary>
    public static void PollInterrupt()
    {
        PollCount++;
        // Tier 2: INTERRUPT-THREAD functions queued for this thread run here.
        // The counter gate keeps the common case to one volatile static read.
        if (System.Threading.Volatile.Read(ref Runtime.SafepointInterruptsPending) != 0)
            Runtime.RunPendingInterruptsAtSafepoint();
        if (!_interruptRequested) return;
        _interruptRequested = false;
        Error(new LispInteractiveInterrupt());
    }

    private static void CheckBreakOnSignals(LispCondition condition)
    {
        var bosSym = Startup.Sym("*BREAK-ON-SIGNALS*");
        if (DynamicBindings.TryGet(bosSym, out var bosVal) && bosVal is not Nil)
        {
            // Bind *break-on-signals* to NIL during check to prevent recursion
            DynamicBindings.Push(bosSym, Nil.Instance);
            try
            {
                if (Runtime.IsTruthy(Runtime.Typep(condition, bosVal)))
                {
                    var breakSym = Startup.Sym("BREAK");
                    if (breakSym.Function is LispFunction breakFn)
                        breakFn.Invoke(new LispString(condition.Message));
                }
            }
            finally
            {
                DynamicBindings.Pop(bosSym);
            }
        }
    }

    public static LispObject Signal(LispCondition condition)
    {
        CheckBreakOnSignals(condition);
        HandlerClusterStack.Signal(condition);
        return Nil.Instance;
    }

    public static LispObject Error(LispCondition condition)
    {
        CheckBreakOnSignals(condition);
        HandlerClusterStack.Signal(condition);
        // Not handled → invoke debugger (per CLHS)
        var invokeDebugger = Startup.Sym("INVOKE-DEBUGGER");
        if (invokeDebugger.Function is LispFunction invDbgFn)
        {
            // No debugger frame: this is ERROR handing control over, not a call
            // the user made. Recording it would put INVOKE-DEBUGGER between
            // *DEBUGGER-HOOK* and the frame that signalled, shifting every index
            // the hook reads (a user (invoke-debugger c) still gets a frame — it
            // goes through the ordinary compiled call path).
            invDbgFn.InvokeNoFrame(condition);
        }
        throw new LispErrorException(condition);
    }

    public static LispObject Warn(LispCondition condition)
    {
        CheckBreakOnSignals(condition);
        // CLHS: warn establishes a MUFFLE-WARNING restart, then signals.
        // If a handler calls muffle-warning, the warning is suppressed.
        //
        // The restart must transfer control, not merely record that it ran: CLHS says
        // WARN's restart causes WARN to return immediately, which unwinds the handler
        // that invoked it. Marking it isBindRestart made MUFFLE-WARNING call this
        // handler in place and return, so the invoking handler kept running and
        // HANDLER-BIND went on to the next applicable clause. Code that muffles a
        // STYLE-WARNING under a handler-bind listing both STYLE-WARNING and WARNING
        // therefore ran the WARNING clause as well — SBCL's compiler does exactly that
        // (COMPILER-STYLE-WARNING-HANDLER muffles, COMPILER-WARNING-HANDLER sets
        // *FAILURE-P*), so every cross-compiled file printed its diagnostics twice and
        // any file with a style warning failed with "FAILURE-P was set".
        // The catch below is what ends the warning now.
        var muffled = false;
        var restart = new LispRestart("MUFFLE-WARNING",
            _ => Nil.Instance);
        RestartClusterStack.PushCluster(new[] { restart });
        try
        {
            HandlerClusterStack.Signal(condition);
        }
        catch (RestartInvocationException rie) when (ReferenceEquals(rie.Tag, restart.Tag))
        {
            muffled = true;
        }
        finally
        {
            RestartClusterStack.PopCluster();
        }
        if (!muffled)
        {
            // Not handled → print warning to *error-output*
            try
            {
                var errSym = Startup.Sym("*ERROR-OUTPUT*");
                if (DynamicBindings.TryGet(errSym, out var errStream) && errStream is LispOutputStream los)
                    los.Writer.WriteLine($"WARNING: {condition.Message}");
                else
                    Console.Error.WriteLine($"WARNING: {condition.Message}");
            }
            catch
            {
                Console.Error.WriteLine($"WARNING: {condition.Message}");
            }
        }
        return Nil.Instance;
    }
}
