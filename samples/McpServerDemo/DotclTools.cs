using System.ComponentModel;
using DotCL;
using ModelContextProtocol.Server;

namespace McpServerDemo;

/// <summary>
/// dotcl を MCP 経由の tool として公開する最小 collection。
/// LLM (Claude Desktop / Cursor / etc.) が <c>lisp_eval</c> を tool として
/// 呼ぶと、このプロセス内の dotcl image で Lisp form を評価、結果を
/// prin1-to-string した文字列で返す。
/// </summary>
[McpServerToolType]
public sealed class DotclTools
{
    // boot は 1 回だけ (FASL core load に 0.3s ほどかかるため)。eval の
    // 並行直列化は dotcl runtime が暗黙に行うので host 側 _evalLock 不要 (D870)。
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
            // progn で multi-form 入力に対応、prin1 でエスケープ込みの
            // readable 形式にしてから C# に戻す。
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
