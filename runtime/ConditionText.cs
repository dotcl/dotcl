namespace DotCL;

using System;

/// <summary>
/// Human-readable text for a condition: the report, plus the type name, for the
/// places that show a condition to a person (the debugger banner, the
/// non-interactive diagnostic, the REPL's error line, an unhandled WARNING).
///
/// Those places used to read <see cref="LispCondition.Message"/> or call
/// <c>ToString()</c>. Both are wrong for a condition built from a
/// DEFINE-CONDITION class: such a condition is a <see cref="LispInstance"/>
/// wrapped in a <see cref="LispInstanceCondition"/>, its report lives in a
/// PRINT-OBJECT method that only fires when *PRINT-ESCAPE* is NIL, and neither
/// the Message field nor ToString() runs the Lisp printer, so the banner showed
/// "#&lt;MY-ERROR&gt;". That is not only user code: SIMPLE-PACKAGE-ERROR and
/// SIMPLE-STYLE-WARNING are DEFINE-CONDITION classes too, so a build that died
/// inside a library said nothing but its condition class.
///
/// PRINC is the printer entry point that produces a report, so that is what
/// this calls. Reporting must not be able to fail, though: it runs a report
/// function, which is arbitrary user code that can signal, and a report that
/// signals while reporting a failure would either lose the banner or loop. So
/// every call is guarded, and every guard falls back to text that needs no
/// evaluation at all -- the Message field, or "#&lt;TYPE&gt;".
/// </summary>
public static class ConditionText
{
    /// <summary>
    /// Set while a report is being rendered on this thread. A report function
    /// that signals reaches the debugger, which asks for a report again; without
    /// this the second report runs the failing function a second time, and its
    /// failure a third. The nested ask gets the fallback instead.
    /// </summary>
    [ThreadStatic]
    private static bool _reporting;

    /// <summary>
    /// What PRINC prints for CONDITION: the report of a DEFINE-CONDITION class,
    /// the format control of a SIMPLE-* condition, or the message a
    /// runtime-signalled condition carries. Never throws.
    /// </summary>
    public static string Report(LispObject condition)
    {
        var fallback = Fallback(condition);
        if (_reporting) return fallback;
        _reporting = true;
        var depth = HandlerClusterStack.Depth;
        try
        {
            // Stand in for the HANDLER-BIND that reporting would need if it were
            // written in Lisp. Without it an ERROR inside the report reaches
            // INVOKE-DEBUGGER, and the debugger this is reporting for is often
            // the one that exits the process: a script's *DEBUGGER-HOOK* prints
            // and calls exit, so the report's own error replaced the error the
            // user was being told about. The cluster is the outermost one during
            // the report, so a HANDLER-CASE inside the report still wins.
            HandlerClusterStack.PushCluster(new[] {
                new HandlerBinding(Startup.Sym("ERROR"),
                    new LispFunction(_ => throw new ReportFailed(), "%REPORT-GUARD", 1))
            });
            return Runtime.PrincToString(condition) is LispString s ? s.Value : fallback;
        }
        catch (BlockReturnException) { throw; }
        catch (CatchThrowException) { throw; }
        catch (GoException) { throw; }
        catch (RestartInvocationException) { throw; }
        catch (HandlerCaseInvocationException) { throw; }
        catch (Exception)
        {
            // The report function signalled, or the printer could not run at all
            // (this is reachable before the Lisp side is up). Say what type of
            // condition it was and let the caller print its banner.
            return fallback;
        }
        finally
        {
            HandlerClusterStack.TruncateTo(depth);
            _reporting = false;
        }
    }

    /// <summary>Thrown by the report guard to unwind out of a report that
    /// signalled. Never escapes <see cref="Report"/>.</summary>
    private sealed class ReportFailed : Exception { }

    /// <summary>
    /// The condition's type name as Lisp spells it: "SIMPLE-ERROR",
    /// "DIVISION-BY-ZERO", or the name of a DEFINE-CONDITION class. Never throws.
    /// </summary>
    public static string TypeName(LispObject condition)
    {
        if (condition is LispCondition lc) return lc.ConditionTypeName;
        try
        {
            return Runtime.TypeOf(condition) is Symbol s ? s.Name : condition.GetType().Name;
        }
        catch (Exception)
        {
            return condition.GetType().Name;
        }
    }

    /// <summary>"TYPE: report", the one-line form for a diagnostic that has no
    /// room for the two-line banner.</summary>
    public static string Line(LispObject condition)
        => $"{TypeName(condition)}: {Report(condition)}";

    /// <summary>Text that is available without evaluating anything.</summary>
    private static string Fallback(LispObject condition)
        => condition is LispCondition lc ? lc.Message : condition.ToString();
}
