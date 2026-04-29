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

        // Show abbreviated backtrace
        try
        {
            var trace = Environment.StackTrace;
            Console.Error.WriteLine("; Backtrace (use :bt to re-display):");
            var lines = trace.Split('\n');
            int shown = 0;
            foreach (var line in lines)
            {
                var trimmed = line.Trim();
                if (trimmed.StartsWith("at DotCL.") && shown < 10)
                {
                    Console.Error.WriteLine($";   {trimmed}");
                    shown++;
                }
            }
            Console.Error.WriteLine(";");
        }
        catch { }

        int level = _nestLevel;
        _nestLevel++;
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

                switch (trimmedLine.ToLowerInvariant())
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
                        PrintBacktrace();
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
                catch (EndOfStreamException) { }
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

    private static void PrintBacktrace()
    {
        var trace = Environment.StackTrace;
        var lines = trace.Split('\n');
        foreach (var line in lines)
        {
            var trimmed = line.Trim();
            if (trimmed.StartsWith("at DotCL."))
                Console.Error.WriteLine($";   {trimmed}");
        }
    }

    private static void PrintHelp()
    {
        Console.Error.WriteLine("; Debugger commands:");
        Console.Error.WriteLine(";   <number>     Invoke restart by index");
        Console.Error.WriteLine(";   :abort, :q   Invoke ABORT restart");
        Console.Error.WriteLine(";   :continue    Invoke CONTINUE restart (if available)");
        Console.Error.WriteLine(";   :bt          Show backtrace");
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
            var desc = r.ToString();
            if (desc.StartsWith("#<RESTART"))
                desc = r.Name;
            Console.Error.WriteLine($";   {i}: [{r.Name}] {desc}");
        }
        Console.Error.WriteLine(";");
    }
}
