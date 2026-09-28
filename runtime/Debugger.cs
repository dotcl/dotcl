namespace DotCL;

/// <summary>
/// What the debugger throws when it has nobody to ask (see
/// <see cref="Debugger.InteractiveRepl"/>). A LispErrorException like any other
/// for everything that catches those; the REPL tells it apart, because a REPL
/// reading from a pipe ends the session on it rather than going on to the next
/// form.
///
/// It is not signalled. It reports on a condition the handlers have already
/// been given; signalling the report as well ran every HANDLER-BIND a second
/// time for the one error, once *DEBUGGER-HOOK* had returned.
/// </summary>
public class DebuggerDeclinedException : LispErrorException
{
    public DebuggerDeclinedException(LispCondition condition) : base(condition, default(NoSignal)) { }
}

public static class Debugger
{
    [ThreadStatic]
    private static int _nestLevel;

    /// <summary>
    /// True once a REPL reading from a terminal is driving this process. Only
    /// then is there somebody to read the debugger's prompt and choose a
    /// restart; a script run started with a file argument, --eval or --load has
    /// nobody, nor does a REPL whose standard input is a pipe or a file, and the
    /// debugger must report and unwind rather than pick a restart on its own.
    /// </summary>
    public static volatile bool InteractiveRepl;

    /// <summary>
    /// Where the debugger reads its lines when there is no line editor: the
    /// reader the REPL reads its own lines from, so the two share one buffer
    /// and one way of reading the terminal. Console.In when unset. On a Unix
    /// terminal the REPL reads the descriptor directly and leaves echoing to
    /// the terminal; Console.ReadLine would switch the terminal to its own mode
    /// and echo the line itself, which a terminal that does not echo (an
    /// editor's shell buffer) shows as the line twice.
    /// </summary>
    public static System.IO.TextReader? Input;

    static string? ReadPlainLine() => (Input ?? Console.In).ReadLine();

    /// <summary>
    /// Enter the interactive debugger. Never returns normally;
    /// only exits via non-local transfer (restart invocation).
    /// </summary>
    public static LispObject Enter(LispObject condition)
    {
        var condMsg = ConditionText.Report(condition);
        var condType = ConditionText.TypeName(condition);

        Console.Error.WriteLine(ReplColor.ForErr("ERROR", $"; Debugger entered on {condType}:"));
        Console.Error.WriteLine(ReplColor.ForErr("ERROR", $";   {condMsg}"));
        Console.Error.WriteLine(";");

        var restarts = CollectRestarts(condition);
        // With the line editor on, the restarts are offered as a menu under
        // the prompt instead (see ReadWithHook); without it, or when no menu
        // can be drawn, they are listed here and chosen by number.
        bool menu = InteractiveRepl && Startup.DebuggerReadHook != null && restarts.Count > 0;
        if (!menu) PrintRestarts(restarts);

        // Nobody is here to answer the prompt. Choosing a restart on the
        // caller's behalf is worse than stopping: the innermost ABORT usually
        // belongs to a library (ASDF establishes one per system it loads), so
        // the script resumes at the next form as if the error had been handled
        // and the process still exits 0.
        if (!InteractiveRepl)
        {
            throw new DebuggerDeclinedException(new LispError(
                $"Debugger: non-interactive session; {condType}: {condMsg}"));
        }

        int level = _nestLevel;
        _nestLevel++;
        // Selected backtrace frame, the one :locals reports on and :up / :down
        // walk. Local to this debugger level, so a nested debugger has its own.
        int frame = 0;
        // Set by :frames: the next read offers the frames as a menu instead of
        // the restarts.
        bool frameMenu = false;
        try
        {
            while (true)
            {
                var prompt = ReplColor.ForOut("DEBUGGER", $"{level}]") + " ";
                string? line;
                if (frameMenu)
                {
                    frameMenu = false;
                    if (!ReadFrameMenu(prompt, ref frame, out line)) continue;
                }
                else if (Startup.DebuggerReadHook is { } hook)
                {
                    var answer = ReadWithHook(hook, prompt,
                        menu ? RestartLabels(restarts) : null, 0, false, out line);
                    if (answer == HookAnswer.Choice)
                    {
                        // The menu only offers restarts that exist.
                        InvokeRestartByIndex(restarts, int.Parse(line!));
                        continue;
                    }
                    if (answer == HookAnswer.NoMenu)
                    {
                        menu = false;
                        PrintRestarts(restarts);
                        continue;
                    }
                    if (answer == HookAnswer.Failed)
                    {
                        menu = false;
                        PrintRestarts(restarts);
                        Console.Write(prompt);
                        line = ReadPlainLine();
                    }
                }
                else
                {
                    Console.Write(prompt);
                    line = ReadPlainLine();
                }
                if (line == null)
                {
                    // EOF on stdin: try ABORT restart; if none available, throw to escape.
                    // Not signalled, like DebuggerDeclinedException: the handlers
                    // have been given CONDITION already.
                    var abortRestart = RestartClusterStack.FindRestartByName("ABORT", condition);
                    if (abortRestart == null)
                    {
                        throw LispErrorException.WithoutSignal(new LispError($"Debugger: stdin closed, no ABORT restart; {condType}: {condMsg}"));
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
                    case ":frames":
                    case ":fr":
                        frameMenu = true;
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
                    case ":source":
                    case ":src":
                        PrintFrameSource(frame);
                        continue;
                    case ":help":
                    case ":h":
                        PrintHelp();
                        continue;
                    case ":restarts":
                    case ":r":
                        // Where there is a menu, bring it back under the next
                        // prompt rather than print the list.
                        if (InteractiveRepl && Startup.DebuggerReadHook != null && restarts.Count > 0)
                            menu = true;
                        else
                            PrintRestarts(restarts);
                        continue;
                }

                // Eval Lisp expression
                try
                {
                    var reader = new Reader(new System.IO.StringReader(trimmedLine));
                    while (reader.TryRead(out var expr))
                    {
                        // The same history variables the top level keeps, as in
                        // SBCL: what you work out here is reachable with * on
                        // the next line, here or after the restart.
                        ReplHistory.BeforeEval(expr);
                        var result = Runtime.Eval(expr);
                        ReplHistory.AfterEval(result);
                        Console.WriteLine(ReplColor.ForOut("RESULT", Runtime.FormatTop(result, true)));
                    }
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine(ReplColor.ForErr("ERROR", $"; Error: {ex.Message}"));
                }
            }
        }
        finally
        {
            _nestLevel--;
        }
    }

    private enum HookAnswer { Line, Choice, NoMenu, Cancel, Failed }

    /// <summary>
    /// Read at the debugger prompt through the line editor's hook. LABELS,
    /// when given, are offered as a menu, SELECTED marked first; FRAMES says
    /// they are backtrace frames rather than restarts. The answer says whether
    /// LINE holds a typed line (null at the end of input), the index of the
    /// chosen label, or nothing: no menu was possible or it was put away
    /// (NoMenu), the frame menu was closed without a choice (Cancel), or the
    /// editor failed and the hook is off from now on (Failed).
    /// </summary>
    private static HookAnswer ReadWithHook(LispFunction hook, string prompt,
        List<string>? labels, int selected, bool frames, out string? line)
    {
        line = null;
        LispObject labelList = Nil.Instance;
        if (labels != null)
            for (int i = labels.Count - 1; i >= 0; i--)
                labelList = new Cons(new LispString(labels[i]), labelList);
        LispObject result;
        try
        {
            result = frames
                ? hook.Invoke(new LispObject[] { new LispString(prompt), labelList,
                                                 Fixnum.Make(selected), Startup.Keyword("FRAMES") })
                : hook.Invoke(new LispObject[] { new LispString(prompt), labelList });
        }
        // Transfers of control are not the editor failing.
        catch (RestartInvocationException) { throw; }
        catch (HandlerCaseInvocationException) { throw; }
        catch (CatchThrowException) { throw; }
        catch (BlockReturnException) { throw; }
        catch (GoException) { throw; }
        catch (LispErrorException ex) when (ex.Condition is LispInteractiveInterrupt) { throw; }
        catch (Exception ex)
        {
            Startup.DebuggerReadHook = null;
            Console.Error.WriteLine($"; line editor failed in the debugger ({ex.Message}); using basic line input");
            return HookAnswer.Failed;
        }
        if (result is MvReturn mv) result = mv.PrimaryValue;
        switch (result)
        {
            case Fixnum f when labels != null && f.Value >= 0 && f.Value < labels.Count:
                line = f.Value.ToString(System.Globalization.CultureInfo.InvariantCulture);
                return HookAnswer.Choice;
            case Symbol { Name: "NO-MENU" }:
                return HookAnswer.NoMenu;
            case Symbol { Name: "CANCEL" }:
                return HookAnswer.Cancel;
            case Nil:
                return HookAnswer.Line;
            case LispString ls:
                line = ls.Value;
                return HookAnswer.Line;
            default:
                line = result.ToString();
                return HookAnswer.Line;
        }
    }

    /// <summary>
    /// :frames. Offer the backtrace as a menu under the prompt, the selected
    /// frame marked, and select the one chosen as :frame N would. Without a
    /// menu (no line editor, or none can be drawn) print the backtrace as :bt
    /// does. True when a line was typed instead, in LINE, for the caller to
    /// handle like any other.
    /// </summary>
    private static bool ReadFrameMenu(string prompt, ref int frame, out string? line)
    {
        line = null;
        var forms = LispFunction.GetCallStackForms();
        if (forms.Length == 0)
        {
            Console.Error.WriteLine("; (no Lisp frames)");
            return false;
        }
        if (Startup.DebuggerReadHook is not { } hook)
        {
            PrintBacktrace(frame);
            return false;
        }
        var labels = new List<string>(forms.Length);
        for (int i = 0; i < forms.Length; i++) labels.Add(FrameLabel(forms, i, frame));
        int start = Math.Max(0, Math.Min(frame, forms.Length - 1));
        switch (ReadWithHook(hook, prompt, labels, start, true, out line))
        {
            case HookAnswer.Choice:
                SelectFrame(int.Parse(line!, System.Globalization.CultureInfo.InvariantCulture), ref frame);
                line = null;
                return false;
            case HookAnswer.Cancel:
                return false;
            case HookAnswer.NoMenu:
            case HookAnswer.Failed:
                PrintBacktrace(frame);
                return false;
            default:
                return true;
        }
    }

    /// <summary>A backtrace line as :bt prints it, without the comment prefix.</summary>
    private static string FrameLabel(string[] forms, int i, int current) =>
        $"{(i == current ? "-->" : "   ")} {i,2}: {forms[i]}";

    private static List<string> RestartLabels(List<LispRestart> restarts)
    {
        var labels = new List<string>(restarts.Count);
        for (int i = 0; i < restarts.Count; i++) labels.Add(RestartLabel(restarts, i));
        return labels;
    }

    private static string RestartLabel(List<LispRestart> restarts, int i)
    {
        var r = restarts[i];
        string text;
        // A report function is user code and may itself signal; the menu must
        // still come up.
        try { text = r.Report(); }
        catch (System.Exception) { text = r.Description ?? r.Name; }
        return $"{i}: [{r.Name}] {text}";
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

    /// <summary>Print the selected frame's call form and its locals: what :frame,
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
        PrintFrameLocation(idx);
        PrintFrameLocals(idx);
    }

    /// <summary>FILE:LINE as the debugger shows it: relative to the current
    /// directory when the file is under it, and painted.</summary>
    private static string LocationText(string file, int line)
    {
        string shown = file;
        try
        {
            // Path.GetRelativePath is not in netstandard2.0; only the "under the
            // current directory" case is wanted anyway.
            var full = System.IO.Path.GetFullPath(file);
            var cwd = System.IO.Path.GetFullPath(System.IO.Directory.GetCurrentDirectory());
            if (!cwd.EndsWith(System.IO.Path.DirectorySeparatorChar.ToString(), StringComparison.Ordinal))
                cwd += System.IO.Path.DirectorySeparatorChar;
            var cmp = Compat.IsWindows() ? StringComparison.OrdinalIgnoreCase : StringComparison.Ordinal;
            if (full.Length > cwd.Length && full.StartsWith(cwd, cmp))
                shown = full.Substring(cwd.Length);
        }
        catch (Exception) { }
        return ReplColor.ForErr("LOCATION", $"{shown}:{line}");
    }

    /// <summary>A generic function's recorded place is the last DEFMETHOD or
    /// DEFGENERIC read for it, which need not be the method running in the
    /// frame; say so.</summary>
    private static string SourceNote(Symbol sym) =>
        sym.Function is GenericFunction ? " (the last DEFMETHOD or DEFGENERIC read)" : "";

    /// <summary>One line under :frame's call form saying where the frame's
    /// function was defined. Nothing at all when that is not known for sure
    /// (not recorded, or several packages define a function of that name):
    /// :source is there to ask.</summary>
    private static void PrintFrameLocation(int idx)
    {
        var names = LispFunction.GetCallStack();
        if (idx < 0 || idx >= names.Length) return;
        foreach (var l in SourceLines(names[idx], true, 0))
            Console.Error.WriteLine(l);
    }

    /// <summary>:source. Where the selected frame's function was defined and the
    /// lines around it.</summary>
    private static void PrintFrameSource(int idx)
    {
        var names = LispFunction.GetCallStack();
        if (idx < 0 || idx >= names.Length)
        {
            Console.Error.WriteLine(names.Length == 0 ? "; (no Lisp frames)" : "; (no such frame)");
            return;
        }
        foreach (var l in SourceLines(names[idx], false, SourceWidth()))
            Console.Error.WriteLine(l);
    }

    /// <summary>
    /// What the debugger prints about the source of a frame whose function is
    /// named NAME. BRIEF: the one line :frame adds, or none when the place is
    /// not known for sure. Otherwise what :source prints: the place and the
    /// lines around it, the candidates when several packages define that name,
    /// or why there is nothing. Locations are recorded per top-level form when
    /// LOAD or COMPILE-FILE reads a source file, so the line is where the
    /// definition starts, not the expression the frame stopped in. Source
    /// lines are cut to WIDTH columns when WIDTH is positive.
    /// </summary>
    internal static List<string> SourceLines(string name, bool brief, int width)
    {
        var result = new List<string>();
        var found = Runtime.DefinitionSourcesForFrame(name);
        if (brief)
        {
            if (found.Count == 1)
                result.Add($";      source: {LocationText(found[0].File, found[0].Line)}{SourceNote(found[0].Sym)}");
            return result;
        }
        if (found.Count == 0)
        {
            result.Add($"; (no source location for {name}: locations are known for");
            result.Add(";  definitions LOAD or COMPILE-FILE read from a source file)");
            return result;
        }
        if (found.Count > 1)
        {
            result.Add($"; {name} is defined in more than one package:");
            foreach (var (sym, file, line) in found)
                result.Add($";   {sym.HomePackage?.Name ?? "#"}::{sym.Name}  {LocationText(file, line)}");
            return result;
        }
        var (s0, f0, l0) = found[0];
        result.Add($"; {name}: {LocationText(f0, l0)}{SourceNote(s0)}");
        string[] lines;
        try { lines = System.IO.File.ReadAllLines(f0); }
        catch (Exception)
        {
            result.Add(";  (the file cannot be read now)");
            return result;
        }
        if (l0 < 1 || l0 > lines.Length)
        {
            result.Add(";  (the file no longer has that line)");
            return result;
        }
        int from = Math.Max(1, l0 - SourceBefore), to = Math.Min(lines.Length, l0 + SourceAfter);
        int digits = to.ToString(System.Globalization.CultureInfo.InvariantCulture).Length;
        for (int n = from; n <= to; n++)
        {
            var head = $"; {(n == l0 ? "-->" : "   ")} {n.ToString(System.Globalization.CultureInfo.InvariantCulture).PadLeft(digits)} | ";
            var text = lines[n - 1].Replace("\t", "        ");
            if (width > 0 && head.Length + text.Length > width - 1)
                text = text.Substring(0, Math.Max(0, width - 1 - head.Length - 3)) + "...";
            result.Add(head + text);
        }
        return result;
    }

    /// <summary>DOTCL::%DEBUGGER-SOURCE-LINES (name &amp;optional brief width):
    /// <see cref="SourceLines"/> as a list of strings, so the text :source and
    /// :frame print is tested without a terminal.</summary>
    public static LispObject SourceLinesForLisp(LispObject[] args)
    {
        if (args.Length < 1 || args[0] is not LispString nm) return Nil.Instance;
        bool brief = args.Length > 1 && args[1] is not Nil;
        int width = args.Length > 2 && args[2] is Fixnum w ? (int)w.Value : 0;
        LispObject list = Nil.Instance;
        var lines = SourceLines(nm.Value, brief, width);
        for (int i = lines.Count - 1; i >= 0; i--) list = new Cons(new LispString(lines[i]), list);
        return list;
    }

    private const int SourceBefore = 2;
    private const int SourceAfter = 4;

    /// <summary>Columns to fit a source line into: the terminal's width when
    /// standard error is one, else 0 (leave the line whole).</summary>
    private static int SourceWidth()
    {
        try
        {
            if (Console.IsErrorRedirected) return 0;
            int w = Console.WindowWidth;
            return w >= 20 ? w : 0;
        }
        catch (Exception) { return 0; }
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
            Console.Error.WriteLine("; (no locals recorded for this frame; compile with");
            Console.Error.WriteLine(";  dotcl:*emit-frame-locals* true to record them)");
            return;
        }
        foreach (var line in lines)
            Console.Error.WriteLine($";      {line}");
    }

    /// <summary>Print the special-variable bindings in effect, marking with * the
    /// ones the selected frame (or something it called) established. Needs no
    /// frame-locals mode: the binding stack is always there.</summary>
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
        Console.Error.WriteLine(";   Up/Down, Enter  Choose from the restart menu (line editor on)");
        Console.Error.WriteLine(";   :abort, :q   Invoke ABORT restart");
        Console.Error.WriteLine(";   :continue    Invoke CONTINUE restart (if available)");
        Console.Error.WriteLine(";   :bt          Show backtrace (--> marks the selected frame)");
        Console.Error.WriteLine(";   :frame [N], :f  Show frame N (no N: the selected one) and select it");
        Console.Error.WriteLine(";   :frames, :fr Choose the frame from a menu (line editor on; else as :bt)");
        Console.Error.WriteLine(";   :up, :u      Select the calling frame");
        Console.Error.WriteLine(";   :down, :d    Select the called frame");
        Console.Error.WriteLine(";   :locals, :l  Show the selected frame's lexical variables");
        Console.Error.WriteLine(";   :specials, :s Show the special (dynamic) bindings in effect");
        Console.Error.WriteLine(";   :source, :src Show where the selected frame's function is defined");
        Console.Error.WriteLine(";   :restarts, :r Show available restarts");
        Console.Error.WriteLine(";   :help, :h    Show this help");
        Console.Error.WriteLine(";   <expr>       Evaluate a Lisp expression");
    }

    private static void PrintRestarts(List<LispRestart> restarts)
    {
        Console.Error.WriteLine("; Available restarts:");
        for (int i = 0; i < restarts.Count; i++)
            Console.Error.WriteLine($";   {RestartLabel(restarts, i)}");
        Console.Error.WriteLine(";");
    }
}
