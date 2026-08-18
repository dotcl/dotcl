namespace DotCL;

/// <summary>
/// Exception for block/return-from non-local exit.
/// Tag is compared by reference identity (each block creates a unique object).
/// </summary>
public class BlockReturnException : Exception
{
    public object Tag { get; }
    public LispObject Value { get; }

    public BlockReturnException(object tag, LispObject value)
        : base("block return")
    {
        Tag = tag;
        Value = value;
    }
}

/// <summary>
/// Exception for catch/throw non-local exit.
/// Tag is compared by EQ (reference identity for most objects).
/// Uses thread-local caching to avoid repeated allocation.
/// </summary>
public class CatchThrowException : Exception
{
    public LispObject Tag { get; private set; }
    public LispObject Value { get; private set; }

    [ThreadStatic]
    private static CatchThrowException? _cached;

    // The cached instance is built empty and filled by Get before every use.
    private CatchThrowException() : base("catch throw")
    {
        Tag = Nil.Instance;
        Value = Nil.Instance;
    }

    public CatchThrowException(LispObject tag, LispObject value)
        : base("catch throw")
    {
        // Called from CIL newobj — return cached instance via static Get
        Tag = tag;
        Value = value;
    }

    /// <summary>Get a (possibly cached) instance. Avoids allocation in hot loops.</summary>
    public static CatchThrowException Get(LispObject tag, LispObject value)
    {
        var ex = _cached ??= new CatchThrowException();
        ex.Tag = tag;
        ex.Value = value;
        return ex;
    }
}

/// <summary>
/// Exception-filter predicate for CATCH: does this in-flight exception target the
/// catch whose tag is TAG? Compiled CATCH runs it as a CIL filter rather than
/// catching every CatchThrowException and rethrowing the ones meant for an outer
/// catch. A rethrow restarts exception dispatch from inside the handler funclet,
/// which stays live while the exception keeps travelling, so N nested catches cost
/// N stacked dispatches; a filter that answers "not mine" leaves the frame
/// untouched and costs nothing. The tree-walk evaluator establishes one CATCH per
/// interpreted call (every function body is a BLOCK), so a THROW out of deep
/// interpreted recursion used to need stack proportional to the depth it crossed
/// and died as an uncatchable .NET StackOverflowException.
/// Returns int because a CIL filter block yields 1 (handle) / 0 (keep looking).
/// </summary>
public static class ControlFlowFilters
{
    public static int CatchTagMatches(object ex, LispObject tag)
        => ex is CatchThrowException c && ReferenceEquals(c.Tag, tag) ? 1 : 0;

    /// <summary>Filter predicate for BLOCK: does this non-local RETURN-FROM target
    /// the block whose tag is TAG? Same reasoning as CatchTagMatches — a block that
    /// caught every BlockReturnException to rethrow the ones it did not own paid a
    /// stacked dispatch for each level a return crossed.</summary>
    public static int BlockTagMatches(object ex, LispObject tag)
        => ex is BlockReturnException b && ReferenceEquals(b.Tag, tag) ? 1 : 0;

    /// <summary>Filter predicate for TAGBODY: does this non-local GO target the
    /// tagbody whose id is ID? Same reasoning as CatchTagMatches.</summary>
    public static int GoTagbodyMatches(object ex, LispObject id)
        => ex is GoException g && ReferenceEquals(g.TagbodyId, id) ? 1 : 0;

    /// <summary>The condition an in-flight exception presents to HANDLER-CASE, or
    /// null when the exception is not one a handler may see (a Lisp non-local exit
    /// passing through). A raw .NET exception is wrapped, exactly as the old
    /// per-catch code did.</summary>
    private static LispObject? ConditionOf(object ex)
    {
        if (ex is LispErrorException lee) return lee.Condition;
        if (ex is Exception e && !Runtime.IsLispControlFlowException(e))
            return Runtime.WrapDotNetExceptionObj(e);
        return null;
    }

    /// <summary>Filter predicate for HANDLER-CASE: the index of the clause that
    /// takes this exception, or -1 for "not mine, keep unwinding".
    ///
    /// TAG identifies this handler-case instance, so its own
    /// HandlerCaseInvocationException (thrown by the handler function
    /// HandlerClusterStack.Signal called) is recognized by identity and carries the
    /// clause index with it. Anything else is matched by type against SPECS, the
    /// clause type specifiers in clause order — first match wins, as CL requires.
    ///
    /// Running this as a CIL filter is what keeps a signal that crosses N nested
    /// handler-cases from costing N stacked exception dispatches: a frame that
    /// answers -1 is never entered, where catching and rethrowing left a live
    /// handler funclet behind at every level (deep recursion with a handler-case
    /// per level died as an uncatchable .NET StackOverflowException at ~20k
    /// frames, a depth the same recursion survives a hundredfold without one).</summary>
    public static int HandlerCaseClause(object ex, object tag, LispObject[] specs)
    {
        if (ex is HandlerCaseInvocationException hci)
            return ReferenceEquals(hci.Tag, tag)
                   && hci.ClauseIndex >= 0 && hci.ClauseIndex < specs.Length
                ? hci.ClauseIndex : -1;
        var cond = ConditionOf(ex);
        if (cond == null) return -1;
        for (int i = 0; i < specs.Length; i++)
            if (Runtime.IsTruthy(Runtime.Typep(cond, specs[i]))) return i;
        return -1;
    }

    /// <summary>Filter predicate for RESTART-CASE: the index of the clause whose
    /// restart this invocation targets, or -1 for "not mine, keep unwinding".
    ///
    /// TAGS holds one unique object per clause, the same objects the LispRestart
    /// entries carry, so INVOKE-RESTART identifies its clause by reference.
    ///
    /// This runs as a CIL filter for the same reason HANDLER-CASE's does: invoking
    /// a restart established far out crosses every restart-case in between, and
    /// catching the invocation only to rethrow it left a live handler funclet at
    /// each one. The cost was quadratic in the number of levels crossed and the
    /// stack it held eventually killed the process.</summary>
    public static int RestartCaseTag(object ex, object[] tags)
    {
        if (ex is not RestartInvocationException rie) return -1;
        for (int i = 0; i < tags.Length; i++)
            if (ReferenceEquals(rie.Tag, tags[i])) return i;
        return -1;
    }

    /// <summary>The condition object to bind to the clause variable, for an
    /// exception whose filter already answered "mine".</summary>
    public static LispObject HandlerCaseCondition(object ex)
        => ex is HandlerCaseInvocationException hci
            ? hci.Condition
            : ConditionOf(ex) ?? Nil.Instance;
}

/// <summary>
/// Exception for tagbody/go non-local transfer.
/// TagbodyId is compared by reference identity.
/// TargetLabel is the integer index of the target tag within the tagbody.
/// </summary>
public class GoException : Exception
{
    public object TagbodyId { get; }
    public int TargetLabel { get; }

    public GoException(object tagbodyId, int targetLabel)
        : base("go")
    {
        TagbodyId = tagbodyId;
        TargetLabel = targetLabel;
    }
}

/// <summary>
/// Runtime stack of active catch tags. Used by throw to check whether
/// a matching catch exists before throwing CatchThrowException.
/// If no matching catch, throw signals CONTROL-ERROR instead.
/// </summary>
public static class CatchTagStack
{
    [ThreadStatic]
    private static List<LispObject>? _tags;

    public static void Push(LispObject tag)
    {
        _tags ??= new List<LispObject>();
        _tags.Add(tag);
    }

    public static void Pop()
    {
        _tags!.RemoveAt(_tags.Count - 1);
    }

    public static bool HasMatchingCatch(LispObject tag)
    {
        if (_tags == null) return false;
        for (int i = _tags.Count - 1; i >= 0; i--)
        {
            if (ReferenceEquals(_tags[i], tag)) return true;
            // For non-reference types (numbers), use Equals
            if (_tags[i].Equals(tag)) return true;
        }
        return false;
    }
}

/// <summary>
/// Wraps an exception with source location information (file + line).
/// Nested loads produce a chain of LispSourceExceptions forming a stack trace.
/// </summary>
public class LispSourceException : Exception
{
    public string FilePath { get; }
    public int Line { get; }

    public LispSourceException(string filePath, int line, Exception inner)
        : base($"{filePath}:{line}: {GetRootMessage(inner)}", inner)
    {
        FilePath = filePath;
        Line = line;
    }

    private static string GetRootMessage(Exception ex)
    {
        // Walk to the innermost non-LispSourceException for the actual error message
        while (ex.InnerException is LispSourceException lse)
            ex = lse.InnerException!;
        return ex.Message;
    }

    /// <summary>
    /// Build a source location trace like:
    ///   file2.lisp:20: Unbound variable: X
    ///     from file1.lisp:10
    /// Innermost (deepest) location first, outermost last.
    /// </summary>
    public string FormatTrace()
    {
        // Collect chain: this -> inner -> inner.inner -> ...
        var chain = new System.Collections.Generic.List<(string file, int line)>();
        Exception cur = this;
        while (cur is LispSourceException lse)
        {
            chain.Add((lse.FilePath, lse.Line));
            cur = lse.InnerException!;
        }
        // Reverse so innermost is first
        chain.Reverse();
        var sb = new System.Text.StringBuilder();
        sb.AppendLine($"{chain[0].file}:{chain[0].line}: {cur.Message}");
        for (int i = 1; i < chain.Count; i++)
            sb.AppendLine($"  from {chain[i].file}:{chain[i].line}");
        // Under dotcl:*debug-stacktrace*, append the underlying .NET exception's
        // type and stack trace. Without this a raw .NET exception (e.g.
        // ArrayTypeMismatchException) that unwinds past all Lisp handlers loses
        // its origin entirely — the Lisp backtrace is empty because the frames
        // already unwound.
        if (Startup.DebugStacktrace && cur != null && !string.IsNullOrEmpty(cur.StackTrace))
        {
            sb.AppendLine($"[.NET {cur.GetType().Name}]");
            sb.Append(cur.StackTrace);
        }
        return sb.ToString().TrimEnd();
    }

    /// MSBuild canonical diagnostic so IDEs (VS / Rider / VS Code) surface the
    /// error in their Error List with click-to-navigate (dotcl/dotcl#48):
    ///   file(line): error DOTCL: message
    ///     from outer(line)
    /// The innermost (deepest) frame — where compilation actually failed — is the
    /// clickable canonical error; outer frames are informational `from` lines.
    public string FormatMsBuildDiagnostic()
    {
        var chain = new System.Collections.Generic.List<(string file, int line)>();
        Exception cur = this;
        while (cur is LispSourceException lse)
        {
            chain.Add((lse.FilePath, lse.Line));
            cur = lse.InnerException!;
        }
        chain.Reverse();
        var sb = new System.Text.StringBuilder();
        sb.AppendLine($"{chain[0].file}({chain[0].line}): error DOTCL: {cur.Message}");
        for (int i = 1; i < chain.Count; i++)
            sb.AppendLine($"  from {chain[i].file}({chain[i].line})");
        return sb.ToString().TrimEnd();
    }
}
