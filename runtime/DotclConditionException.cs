namespace DotCL;

using System;

/// <summary>
/// A Lisp condition that reached the .NET host: thrown by the non-interactive
/// debugger hook (<see cref="DotclHost.SetThrowingDebuggerHook()"/>) when no
/// Lisp handler took the condition, and by the DotclHost entry points for an
/// error the runtime raised itself while that hook is installed.
///
/// The point of the type is that the condition survives the trip. A host that
/// only reads <see cref="Exception.Message"/> gets the report string it would
/// have printed; a host that wants to do something about it keeps
/// <see cref="Condition"/> -- the live condition object, which can be handed
/// back to Lisp to read its slots, ask what restarts are available, or invoke
/// one. When the condition wraps a .NET exception (a Lisp call that reached a
/// failing .NET method), <see cref="ClrException"/> is that exception, so the
/// host can dispatch on its type instead of matching on text.
///
/// <example>
/// <code>
/// try { DotclHost.EvalString("(risky)"); }
/// catch (DotclConditionException e) when (e.ClrException is System.IO.IOException io)
/// {
///     // the failure came from .NET; io.Message, io.HResult, ... are intact
/// }
/// catch (DotclConditionException e)
/// {
///     DotclHost.Register("host-condition", _ => e.Condition);
///     DotclHost.EvalString("(invoke-restart (find-restart 'continue (host-condition)))");
/// }
/// </code>
/// </example>
/// </summary>
public class DotclConditionException : Exception
{
    /// <summary>
    /// The condition object itself. Hand it back to Lisp to read its slots or to
    /// find and invoke a restart; it is the same object the Lisp side signalled.
    /// </summary>
    public LispObject Condition { get; }

    /// <summary>
    /// The .NET exception this condition wraps, or null for a condition that
    /// originated in Lisp. A failing .NET call reaches Lisp as a condition
    /// carrying the original exception, and this is how a host gets it back
    /// without parsing the message.
    /// </summary>
    public Exception? ClrException { get; }

    /// <summary>
    /// The condition's type name as Lisp spells it -- "SIMPLE-ERROR",
    /// "DIVISION-BY-ZERO", or the name of a DEFINE-CONDITION class.
    /// </summary>
    public string ConditionType { get; }

    /// <summary>Wrap CONDITION. <see cref="Exception.Message"/> becomes the
    /// condition's report string, so a host that logs the message alone sees
    /// what the debugger would have printed.</summary>
    public DotclConditionException(LispObject condition)
        : base(ConditionText.Report(condition))
    {
        Condition = condition;
        ConditionType = ConditionText.TypeName(condition);
        ClrException = (condition as LispCondition)?.ClrException;
    }

    /// <summary>Wrap CONDITION, recording the exception that carried it this far
    /// (a <see cref="LispErrorException"/> the runtime threw without running the
    /// debugger hook) as <see cref="Exception.InnerException"/>.</summary>
    public DotclConditionException(LispObject condition, Exception inner)
        : base(ConditionText.Report(condition), inner)
    {
        Condition = condition;
        ConditionType = ConditionText.TypeName(condition);
        ClrException = (condition as LispCondition)?.ClrException;
    }
}
