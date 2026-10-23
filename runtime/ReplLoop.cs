namespace DotCL;

/// <summary>
/// The REPL's read-eval-print loop, for the dotcl command and for a host that
/// runs the REPL on a terminal of its own (dotcl-repl:run-repl).
/// </summary>
public static class ReplLoop
{
    [ThreadStatic] private static System.IO.TextWriter? t_out;
    [ThreadStatic] private static System.IO.TextWriter? t_err;
    [ThreadStatic] private static LispObject? t_outStream;
    [ThreadStatic] private static LispObject? t_errStream;
    [ThreadStatic] private static bool t_color;
    [ThreadStatic] private static LispFunction? t_readHook;
    [ThreadStatic] private static LispFunction? t_debuggerHook;

    /// <summary>Where the REPL and the debugger write what they say: the
    /// host's output inside a hosted REPL, standard output otherwise.</summary>
    public static System.IO.TextWriter Out => t_out ?? Console.Out;

    /// <summary>As <see cref="Out"/>, for errors.</summary>
    public static System.IO.TextWriter Err => t_err ?? Console.Error;

    /// <summary>True on the thread of a hosted REPL.</summary>
    public static bool Hosted => t_out != null;

    /// <summary>Whether a hosted REPL on this thread paints its output.</summary>
    internal static bool HostColor => t_color;

    /// <summary>The line editor's read hook: a hosted REPL's own on its
    /// thread, the process's (<see cref="Startup.ReadlineHook"/>) elsewhere.</summary>
    internal static LispFunction? ReadlineHook => Hosted ? t_readHook : Startup.ReadlineHook;

    /// <summary>As <see cref="ReadlineHook"/>, for the debugger's prompt.</summary>
    internal static LispFunction? DebuggerReadHook => Hosted ? t_debuggerHook : Startup.DebuggerReadHook;

    /// <summary>Turn off the read hook that failed, only for the REPL it belongs to.</summary>
    internal static void DropReadlineHook()
    {
        if (Hosted) t_readHook = null; else Startup.ReadlineHook = null;
    }

    /// <summary>As <see cref="DropReadlineHook"/>, for the debugger's.</summary>
    internal static void DropDebuggerReadHook()
    {
        if (Hosted) t_debuggerHook = null; else Startup.DebuggerReadHook = null;
    }

    /// <summary>
    /// A line from the hosted REPL's line editor, for the debugger once its own
    /// hook has failed: null at the end of input or when the editor fails too.
    /// </summary>
    internal static string? HostReadLine(string prompt)
    {
        if (t_readHook is not { } hook) return null;
        LispObject result;
        try
        {
            result = Debugger.InvokeReadHook(hook, new LispObject[] { new LispString(prompt) });
        }
        catch (Exception ex) when (!IsTransfer(ex))
        {
            t_readHook = null;
            Err.WriteLine($"; readline failed ({ex.Message})");
            return null;
        }
        if (result is MvReturn mv) result = mv.PrimaryValue;
        return result is Nil ? null : (result as LispString)?.Value ?? result.ToString();
    }

    private static bool IsTransfer(Exception ex) => ex is RestartInvocationException
        or HandlerCaseInvocationException or CatchThrowException or BlockReturnException
        or GoException
        || (ex is LispErrorException le && le.Condition is LispInteractiveInterrupt);

    // The host's writer may be shared (Console.Out), so its NewLine is left alone.
    private sealed class LfWriter : System.IO.TextWriter
    {
        private readonly System.IO.TextWriter _inner;
        public LfWriter(System.IO.TextWriter inner) { _inner = inner; CoreNewLine = new[] { '\n' }; }
        public override System.Text.Encoding Encoding => _inner.Encoding;
        public override void Write(char value) => _inner.Write(value);
        public override void Write(string? value) => _inner.Write(value);
        public override void Write(char[] buffer, int index, int count) => _inner.Write(buffer, index, count);
        public override void WriteLine() => _inner.Write('\n');
        public override void WriteLine(string? value) { _inner.Write(value); _inner.Write('\n'); }
        public override void Flush() => _inner.Flush();
    }

    /// <summary>True for the output stream a hosted REPL on this thread was
    /// given (TARGET :ERROR for its error output).</summary>
    internal static bool IsHostStream(LispObject? stream, bool error)
        => stream != null && ReferenceEquals(stream, error ? t_errStream : t_outStream);

    /// <summary>
    /// Run the REPL on this thread, reading lines through READLINE and the
    /// debugger's through DEBUGGERREAD (the line editor's hooks, for this thread
    /// only), writing to the Lisp streams OUTPUT and ERROR-OUTPUT. Returns at
    /// the end of input or when READLINE fails. COLOR paints the prompt and the
    /// values.
    /// </summary>
    public static void RunHosted(LispObject output, LispObject errorOutput, bool color,
        LispFunction readline, LispFunction? debuggerRead)
    {
        var savedOut = t_out; var savedErr = t_err;
        var savedOutStream = t_outStream; var savedErrStream = t_errStream;
        var savedColor = t_color;
        var savedReadHook = t_readHook; var savedDebuggerHook = t_debuggerHook;
        try
        {
            t_readHook = readline;
            t_debuggerHook = debuggerRead;
            var outWriter = new LfWriter(Runtime.GetTextWriter(output));
            var errWriter = new LfWriter(Runtime.GetTextWriter(errorOutput));
            t_out = outWriter;
            t_err = errWriter;
            t_outStream = output;
            t_errStream = errorOutput;
            t_color = color;
            try
            {
                Out.WriteLine("dotcl REPL. Ctrl+D to exit.");
                Run(null, () => "Ctrl+D", null, null);
            }
            finally
            {
                try { outWriter.Flush(); errWriter.Flush(); } catch { }
            }
        }
        finally
        {
            t_out = savedOut; t_err = savedErr;
            t_outStream = savedOutStream; t_errStream = savedErrStream;
            t_color = savedColor;
            t_readHook = savedReadHook; t_debuggerHook = savedDebuggerHook;
        }
    }

    /// <summary>
    /// Prompt, read, evaluate and print until the end of input. Lines come from
    /// the readline hook when there is one, else from STDIN. When the hook fails
    /// it is turned off and FALLBACKINPUT, if given, supplies STDIN from then
    /// on; without it the loop ends. DECLINED runs when the debugger had nobody
    /// to ask (input not a terminal); without it the loop ends.
    /// </summary>
    public static unsafe void Run(System.IO.TextReader? stdin, Func<string> exitKeyHint,
        Func<System.IO.TextReader>? fallbackInput, Action? declined)
    {
        var buffer = new System.Text.StringBuilder();

        while (true)
        {
            // The ABORT restart covers the WHOLE iteration -- prompt, read and
            // eval -- not just the evaluation. Ctrl-C arriving while the REPL
            // waits for input used to reach the debugger with an empty restart
            // cluster: "Available restarts:" followed by nothing, and no way
            // back to the prompt except Ctrl-D (dotcl/dotcl issue 61). The
            // interrupt is delivered at a safepoint on this thread, so the
            // cluster established here is the one it sees.
            var abortTag = new object();
            var abortRestart = new LispRestart("ABORT",
                _ => Nil.Instance,
                description: "Return to top level.",
                tag: abortTag);
            RestartClusterStack.PushCluster(new[] { abortRestart });
            try
            {
            var pkg = DynamicBindings.Get(Startup.Sym("*PACKAGE*")) as Package;
            var pkgName = pkg != null
                ? new[] { pkg.VisibleName }.Concat(pkg.VisibleNicknames).OrderBy(n => n.Length).First()
                : "CL-USER";

            // The continuation prompt is as wide as the primary one looks,
            // which is the plain text: the colour takes no columns.
            var plainPrimary = $"{pkgName}> ";
            var primary = ReplColor.ForOut("PROMPT", $"{pkgName}>") + " ";
            var prompt = buffer.Length == 0 ? primary : new string(' ', plainPrimary.Length);

            string? line;
            if (ReadlineHook is { } readHook)
            {
                // An interrupt that arrives while the editor waits for a key is
                // not an interrupt of any computation: it means "drop what I
                // typed". On a Windows console the editor reads Ctrl+C as a key
                // while it waits (TreatControlCAsInput) and drops the form
                // itself. On Unix the key arrives as SIGINT instead, so the
                // condition is caught here rather than entering the debugger,
                // with the same fresh prompt as the result. The editor also
                // signals it for Ctrl+C on a continuation line, whose earlier
                // lines are in this loop's buffer.
                var interruptTag = new object();
                HandlerClusterStack.PushCluster(new[] {
                    new HandlerBinding(Startup.Sym("INTERACTIVE-INTERRUPT"),
                        new LispFunction(hargs => throw new HandlerCaseInvocationException(
                            interruptTag, 0, hargs.Length > 0 ? hargs[0] : Nil.Instance),
                            "%REPL-INPUT-INTERRUPT", -1))
                });
                try
                {
                    var result = readHook.Invoke(new LispObject[] { new LispString(prompt) });
                    // A reader that ends in READ-LINE returns two values; an
                    // interpreted one hands them back as a single MvReturn.
                    if (result is MvReturn mv) result = mv.PrimaryValue;
                    line = result is Nil ? null : (result as LispString)?.Value ?? result.ToString();
                }
                catch (HandlerCaseInvocationException hce) when (ReferenceEquals(hce.Tag, interruptTag))
                {
                    Out.WriteLine("^C");
                    buffer.Clear();
                    continue;
                }
                // Transfers of control are not the editor failing. ABORT from a
                // debugger entered inside the editor, a THROW, an interrupt: each
                // goes to whoever established it (the handlers below), and the
                // editor stays on for the next prompt.
                catch (RestartInvocationException) { throw; }
                catch (HandlerCaseInvocationException) { throw; }
                catch (CatchThrowException) { throw; }
                catch (BlockReturnException) { throw; }
                catch (GoException) { throw; }
                catch (LispErrorException ex) when (ex.Condition is LispInteractiveInterrupt) { throw; }
                catch (Exception ex)
                {
                    // The line editor failed (e.g. --readline forced on a
                    // non-console where Console.ReadKey/CursorLeft are invalid).
                    // Disable it and fall back to plain line input for the rest
                    // of the session instead of crashing the REPL.
                    DropReadlineHook();
                    stdin = fallbackInput?.Invoke();
                    if (stdin == null)
                    {
                        Err.WriteLine($"; readline failed ({ex.Message})");
                        break;
                    }
                    Err.WriteLine(
                        $"; readline failed ({ex.Message}); falling back to basic line input"
                        + $" ({exitKeyHint()} to exit)");
                    continue;
                }
                finally
                {
                    HandlerClusterStack.PopCluster();
                }
            }
            else
            {
                if (stdin == null) break;
                Out.Write(prompt);
                line = stdin.ReadLine();
                // On a Windows console, Ctrl+C makes the pending ReadLine return
                // null as if the input had ended. Tell the two apart by the
                // interrupt the same key requested, and treat it like the
                // editor's Ctrl+C rather than leaving the REPL.
                if (line == null && ConditionSystem.TakeConsoleInterrupt())
                {
                    Out.WriteLine("^C");
                    buffer.Clear();
                    continue;
                }
            }

            if (line == null)
            {
                // EOF. Drop any pending partial form and exit.
                break;
            }
            if (buffer.Length == 0 && string.IsNullOrWhiteSpace(line)) continue;

            if (buffer.Length > 0) buffer.Append('\n');
            buffer.Append(line);

            // Try to read all forms from the accumulated buffer. Reader signals
            // "more input needed" as a LispError of condition type END-OF-FILE
            // (mid-list, mid-string, after a quote: see
            // Reader.MakeEndOfFileError), which means "keep the buffer and
            // re-prompt with the continuation indent". Anything else is a real
            // syntax error: print and drop the buffer.
            var forms = new List<LispObject>();
            bool incomplete = false;
            bool readError = false;
            try
            {
                var reader = new Reader(new StringReader(buffer.ToString()));
                while (reader.TryRead(out var expr))
                    forms.Add(expr);
            }
            catch (LispErrorException ex) when (
                ex.Condition is LispCondition lc && lc.ConditionTypeName == "END-OF-FILE")
            {
                incomplete = true;
            }
            catch (LispErrorException ex)
            {
                Err.WriteLine(ReplColor.ForErr("ERROR", $"; read error: {ConditionText.Line(ex.Condition)}"));
                readError = true;
            }
            catch (Exception ex)
            {
                Err.WriteLine(ReplColor.ForErr("ERROR", $"; read error: {ex.Message}"));
                readError = true;
            }

            if (incomplete) continue;
            if (readError) { buffer.Clear(); continue; }
            buffer.Clear();

                // An error the runtime signals while evaluating or printing
                // (CAR of a non-list, an undefined function, ...) that nothing
                // handles enters the debugger here, as ERROR does, with the
                // signalling frames still live for :bt and :locals. Only here:
                // the reader above signals END-OF-FILE for every unfinished
                // form, and that must stay a request for more input.
                ConditionSystem.UnhandledErrorsEnterDebugger = true;
                try
                {
                foreach (var form in forms)
                {
                    // The history variables, CLHS 25.1.1. AFTEREVAL runs before
                    // the result is printed, because printing can run Lisp and
                    // so can overwrite the values it reads.
                    ReplHistory.BeforeEval(form);
                    var result = Runtime.Eval(form);
                    ReplHistory.AfterEval(result);
                    Out.WriteLine(ReplColor.ForOut("RESULT", Runtime.FormatTop(result, true)));
                }
                }
                finally
                {
                    ConditionSystem.UnhandledErrorsEnterDebugger = false;
                }
            }
            catch (RestartInvocationException rie) when (ReferenceEquals(rie.Tag, abortTag))
            {
                // ABORT invoked -- drop any partial input and re-prompt.
                buffer.Clear();
            }
            catch (LispErrorException ex) when (ex.Condition is LispInteractiveInterrupt)
            {
                buffer.Clear();
                Err.WriteLine(ReplColor.ForErr("ERROR", "; Interrupted."));
            }
            catch (DebuggerDeclinedException ex)
            {
                // A REPL on a pipe reached the debugger: stop as a script does.
                Err.WriteLine(ReplColor.ForErr("ERROR", $"; {ConditionText.Line(ex.Condition)}"));
                if (declined == null) break;
                declined();
            }
            catch (LispErrorException ex)
            {
                Err.WriteLine(ReplColor.ForErr("ERROR", $"; {ConditionText.Line(ex.Condition)}"));
            }
            catch (Exception ex)
            {
                Err.WriteLine(ReplColor.ForErr("ERROR", $"Error: {ex.Message}"));
                if (Startup.DebugStacktrace) Err.WriteLine(ex.StackTrace);
            }
            finally
            {
                RestartClusterStack.PopCluster();
            }
        }
    }
}
