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

    private CatchThrowException() : base("catch throw") { }

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
        return sb.ToString().TrimEnd();
    }
}
