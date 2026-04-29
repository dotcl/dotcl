namespace DotCL;

public class LispCondition : LispObject
{
    public string Message { get; }
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
    public LispCondition(string message) => Message = message;
    public override string ToString() => $"#<{ConditionTypeName}: {Message}>";
}

public class LispError : LispCondition
{
    public LispError(string message) : base(message) { ConditionTypeName = "ERROR"; }
    public override string ToString() => $"#<ERROR: {Message}>";
}

public class LispTypeError : LispError
{
    public LispObject Datum { get; }
    public LispObject ExpectedType { get; }

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

public class LispCellError : LispError
{
    public LispObject Name { get; }
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
            invDbgFn.Invoke(condition);
        }
        throw new LispErrorException(condition);
    }

    public static LispObject Warn(LispCondition condition)
    {
        CheckBreakOnSignals(condition);
        // CLHS: warn establishes a MUFFLE-WARNING restart, then signals.
        // If a handler calls muffle-warning, the warning is suppressed.
        var muffled = false;
        var restart = new LispRestart("MUFFLE-WARNING",
            _ => { muffled = true; return Nil.Instance; },
            isBindRestart: true);
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
