namespace DotCL;

public static class Debugger
{
    [ThreadStatic]
    private static int _nestLevel;

    /// <summary>
    /// Enter the interactive debugger. Never returns normally —
    /// only exits via non-local transfer (restart invocation).
    /// </summary>
    public static LispObject Enter(LispObject condition)
    {
        var condMsg = condition is LispCondition lc ? lc.Message : condition.ToString();
        var condType = condition is LispCondition lc2 ? lc2.ConditionTypeName : condition.GetType().Name;

        Console.Error.WriteLine($"; Debugger entered on {condType}:");
        Console.Error.WriteLine($";   {condMsg}");
        Console.Error.WriteLine(";");

        var restarts = CollectRestarts(condition);
        PrintRestarts(restarts);

        int level = _nestLevel;
        _nestLevel++;
        // Selected backtrace frame, the one :locals reports on and :up / :down
        // walk. Local to this debugger level, so a nested debugger has its own.
        int frame = 0;
        try
        {
            while (true)
            {
                Console.Write($"{level}] ");
                var line = Console.ReadLine();
                if (line == null)
                {
                    // EOF on stdin — try ABORT restart; if none available, throw to escape
                    var abortRestart = RestartClusterStack.FindRestartByName("ABORT", condition);
                    if (abortRestart == null)
                    {
                        throw new LispErrorException(new LispError($"Debugger: stdin closed, no ABORT restart; {condType}: {condMsg}"));
                    }
                    TryInvokeAbort(condition);
                    continue;
                }
                if (string.IsNullOrWhiteSpace(line)) continue;

                var trimmedLine = line.Trim();

                // Restart by number
                if (int.TryParse(trimmedLine, out int idx) && idx >= 0 && idx < restarts.Count)
                {
                    InvokeRestartByIndex(restarts, idx);
                    continue;
                }

                // Commands take at most one argument (":frame 2"), so split off the
                // verb before dispatching and keep the rest for the command.
                var spaceIdx = trimmedLine.IndexOf(' ');
                var verb = (spaceIdx < 0 ? trimmedLine : trimmedLine.Substring(0, spaceIdx))
                    .ToLowerInvariant();
                var cmdArg = spaceIdx < 0 ? "" : trimmedLine.Substring(spaceIdx + 1).Trim();

                switch (verb)
                {
                    case ":abort":
                    case ":q":
                        TryInvokeAbort(condition);
                        continue;
                    case ":continue":
                        TryInvokeContinue(condition);
                        continue;
                    case ":backtrace":
                    case ":bt":
                        PrintBacktrace(frame);
                        continue;
                    case ":frame":
                    case ":f":
                        if (cmdArg.Length == 0)
                            PrintFrame(frame);
                        else if (int.TryParse(cmdArg, out int wanted))
                            SelectFrame(wanted, ref frame);
                        else
                            Console.Error.WriteLine("; :frame expects a frame number.");
                        continue;
                    case ":up":
                    case ":u":
                        SelectFrame(frame + 1, ref frame);
                        continue;
                    case ":down":
                    case ":d":
                        SelectFrame(frame - 1, ref frame);
                        continue;
                    case ":locals":
                    case ":l":
                        PrintFrameLocals(frame);
                        continue;
                    case ":specials":
                    case ":s":
                        PrintFrameSpecials(frame);
                        continue;
                    case ":help":
                    case ":h":
                        PrintHelp();
                        continue;
                    case ":restarts":
                    case ":r":
                        PrintRestarts(restarts);
                        continue;
                }

                // Eval Lisp expression
                try
                {
                    var reader = new Reader(new System.IO.StringReader(trimmedLine));
                    while (reader.TryRead(out var expr))
                    {
                        var result = Runtime.Eval(expr);
                        Console.WriteLine(Runtime.FormatTop(result, true));
                    }
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine($"; Error: {ex.Message}");
                }
            }
        }
        finally
        {
            _nestLevel--;
        }
    }

    private static List<LispRestart> CollectRestarts(LispObject condition)
    {
        var result = new List<LispRestart>();
        var restartList = RestartClusterStack.ComputeRestarts(
            condition is LispCondition ? condition : null);
        var cur = restartList;
        while (cur is Cons c)
        {
            if (c.Car is LispRestart r)
                result.Add(r);
            cur = c.Cdr;
        }
        return result;
    }

    private static void InvokeRestartByIndex(List<LispRestart> restarts, int idx)
    {
        var restart = restarts[idx];
        LispObject[] args = Array.Empty<LispObject>();
        if (restart.InteractiveFunction != null)
        {
            var argList = Runtime.Funcall(restart.InteractiveFunction);
            var argsList = new List<LispObject>();
            var cur = argList;
            while (cur is Cons c) { argsList.Add(c.Car); cur = c.Cdr; }
            args = argsList.ToArray();
        }
        if (restart.IsBindRestart)
            restart.Handler(args);
        else
            throw new RestartInvocationException(restart.Tag, args);
    }

    private static void TryInvokeAbort(LispObject? condition)
    {
        var restart = RestartClusterStack.FindRestartByName("ABORT", condition);
        if (restart == null)
        {
            Console.Error.WriteLine("; No ABORT restart available.");
            return;
        }
        if (restart.IsBindRestart)
            restart.Handler(Array.Empty<LispObject>());
        else
            throw new RestartInvocationException(restart.Tag, Array.Empty<LispObject>());
    }

    private static void TryInvokeContinue(LispObject? condition)
    {
        var restart = RestartClusterStack.FindRestartByName("CONTINUE", condition);
        if (restart == null)
        {
            Console.Error.WriteLine("; No CONTINUE restart available.");
            return;
        }
        if (restart.IsBindRestart)
            restart.Handler(Array.Empty<LispObject>());
        else
            throw new RestartInvocationException(restart.Tag, Array.Empty<LispObject>());
    }

    private static void PrintBacktrace(int current)
    {
        var frames = LispFunction.GetCallStackForms();
        if (frames.Length == 0)
        {
            Console.Error.WriteLine("; (no Lisp frames)");
            return;
        }
        for (int i = 0; i < frames.Length; i++)
            Console.Error.WriteLine($"; {(i == current ? "-->" : "   ")} {i,2}: {frames[i]}");
    }

    /// <summary>Print the selected frame's call form and its locals — what :frame,
    /// :up and :down show after moving.</summary>
    private static void PrintFrame(int idx)
    {
        var frames = LispFunction.GetCallStackForms();
        if (idx < 0 || idx >= frames.Length)
        {
            Console.Error.WriteLine("; (no such frame)");
            return;
        }
        Console.Error.WriteLine($";  {idx,2}: {frames[idx]}");
        PrintFrameLocals(idx);
    }

    private static void SelectFrame(int wanted, ref int current)
    {
        int count = LispFunction.GetCallStack().Length;
        if (count == 0)
        {
            Console.Error.WriteLine("; (no Lisp frames)");
            return;
        }
        if (wanted < 0 || wanted >= count)
        {
            Console.Error.WriteLine($"; (no frame {wanted}; frames are 0..{count - 1})");
            return;
        }
        current = wanted;
        PrintFrame(current);
    }

    /// <summary>Print a frame's lexical variables. They are recorded only for code
    /// compiled with frame-locals mode on, so say so rather than looking broken
    /// when there is nothing to show.</summary>
    private static void PrintFrameLocals(int idx)
    {
        var lines = DebugFrames.FormatLocals(idx);
        if (lines.Length == 0)
        {
            Console.Error.WriteLine("; (no locals recorded for this frame — compile with");
            Console.Error.WriteLine(";  dotcl:*emit-frame-locals* true to record them)");
            return;
        }
        foreach (var line in lines)
            Console.Error.WriteLine($";      {line}");
    }

    /// <summary>Print the special-variable bindings in effect, marking with * the
    /// ones the selected frame (or something it called) established. Needs no
    /// frame-locals mode — the binding stack is always there.</summary>
    private static void PrintFrameSpecials(int idx)
    {
        var lines = DebugFrames.FormatSpecials(idx);
        if (lines.Length == 0)
        {
            Console.Error.WriteLine("; (no special bindings in effect)");
            return;
        }
        Console.Error.WriteLine(";      (* = bound by this frame or its callees)");
        foreach (var line in lines)
            Console.Error.WriteLine($";      {line}");
    }

    private static void PrintHelp()
    {
        Console.Error.WriteLine("; Debugger commands:");
        Console.Error.WriteLine(";   <number>     Invoke restart by index");
        Console.Error.WriteLine(";   :abort, :q   Invoke ABORT restart");
        Console.Error.WriteLine(";   :continue    Invoke CONTINUE restart (if available)");
        Console.Error.WriteLine(";   :bt          Show backtrace (--> marks the selected frame)");
        Console.Error.WriteLine(";   :frame [N], :f  Show frame N (no N: the selected one) and select it");
        Console.Error.WriteLine(";   :up, :u      Select the calling frame");
        Console.Error.WriteLine(";   :down, :d    Select the called frame");
        Console.Error.WriteLine(";   :locals, :l  Show the selected frame's lexical variables");
        Console.Error.WriteLine(";   :specials, :s Show the special (dynamic) bindings in effect");
        Console.Error.WriteLine(";   :restarts, :r Show available restarts");
        Console.Error.WriteLine(";   :help, :h    Show this help");
        Console.Error.WriteLine(";   <expr>       Evaluate a Lisp expression");
    }

    private static void PrintRestarts(List<LispRestart> restarts)
    {
        Console.Error.WriteLine("; Available restarts:");
        for (int i = 0; i < restarts.Count; i++)
        {
            var r = restarts[i];
            var desc = r.Description ?? r.Name;
            Console.Error.WriteLine($";   {i}: [{r.Name}] {desc}");
        }
        Console.Error.WriteLine(";");
    }
}
