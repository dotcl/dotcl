using System.ComponentModel;
using DotCL;
using ModelContextProtocol.Server;

namespace McpServerDemo;

/// <summary>
/// Minimal MCP tool collection that exposes dotcl as a tool.
/// When an LLM (Claude Desktop / Cursor / etc.) calls <c>lisp_eval</c>,
/// the Lisp form is evaluated inside the in-process dotcl image and the
/// result is returned as a prin1-to-string string.
/// </summary>
[McpServerToolType]
public sealed class DotclTools
{
    // Boot once only (FASL core load takes ~0.3s). The dotcl runtime serialises
    // concurrent evals internally, so no host-side _evalLock is needed.
    private static readonly object _bootLock = new();
    private static bool _booted;

    private static void EnsureBooted()
    {
        if (_booted) return;
        lock (_bootLock)
        {
            if (_booted) return;
            DotclHost.Initialize();
            var core = DotclHost.FindCore()
                ?? throw new InvalidOperationException(
                    "dotcl.core not found next to McpServerDemo.exe. " +
                    "Check that the csproj copies ../../compiler/dotcl.core.");
            DotclHost.LoadCore(core);
            _booted = true;
        }
    }

    [McpServerTool(Name = "lisp_eval"),
     Description(
        "Evaluate a Common Lisp form in the dotcl image and return the " +
        "printed representation of the primary value (via PRIN1-TO-STRING). " +
        "Multiple forms are wrapped in a PROGN — only the last value is returned. " +
        "Side effects (DEFUN / DEFVAR / DEFPARAMETER) persist across calls.")]
    public static string LispEval(
        [Description("Common Lisp source, e.g. \"(+ 1 2)\" or \"(mapcar #'1+ '(1 2 3))\"")]
        string code)
    {
        EnsureBooted();
        try
        {
            // (prin1-to-string (progn <user-code>))
            // progn handles multi-form input; prin1-to-string returns an
            // escaped readable representation back to C#.
            var wrapped = $"(prin1-to-string (progn {code}))";
            var result = DotclHost.EvalString(wrapped);
            return result is LispString ls ? ls.Value
                                           : result?.ToString() ?? "NIL";
        }
        catch (Exception ex)
        {
            return $"ERROR ({ex.GetType().Name}): {ex.Message}";
        }
    }
}
