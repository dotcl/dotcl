namespace DotCL;

/// <summary>
/// What the REPL may assume about the terminal it runs in. The one place that
/// says which terminals take escape sequences, so colour, the line editor and
/// its menus agree about where they are off.
/// </summary>
public static class ReplTerminal
{
    /// <summary>
    /// True for a terminal that says it cannot move the cursor or take any
    /// other escape sequence: TERM=dumb, which is what Emacs's shell and comint
    /// buffers set, as do other editors that show a process's output as text.
    /// </summary>
    public static bool Dumb(string? term) => term == "dumb";

    /// <summary>
    /// Whether the REPL reads through the line editor. The editor draws with
    /// escape sequences and reads single keys, so by default it is on only when
    /// standard input and standard output are both a terminal that takes escape
    /// sequences (on Windows: a console on which virtual terminal processing
    /// could be turned on). --readline and --no-readline (PREF true or false)
    /// override that, except that TERM=dumb turns it off in every case, as it
    /// does colour: the person who set TERM knows better where the text is
    /// going than a command line that may have come from an alias.
    ///
    /// A pure function of its arguments, so the decision is tested without a
    /// terminal.
    /// </summary>
    public static bool LineEditing(bool? pref, string? term, bool inputTerminal, bool outputTerminal)
    {
        if (Dumb(term)) return false;
        if (pref.HasValue) return pref.Value;
        return inputTerminal && outputTerminal;
    }
}

/// <summary>How <c>--color</c> was given: the default is <see cref="Auto"/>.</summary>
public enum ReplColorMode { Auto, Always, Never }

/// <summary>
/// Colour in the REPL: the prompt, the value a form returned, warnings and
/// errors. What a program prints itself is left in the terminal's own colour,
/// so it stays distinguishable from all of those without anything here having
/// to get between a program and its output.
///
/// Off unless the REPL turns it on (<see cref="Configure"/>). A script, an
/// --eval run and the regression suite never see an escape sequence from here.
///
/// Every painted piece of text is followed by a reset, so a colour never leaks
/// into the next write, whichever stream that write goes to: standard output and
/// standard error share one terminal and so one colour state.
/// </summary>
public static class ReplColor
{
    public const string Reset = "\u001b[0m";

    /// <summary>True when text written to standard output is painted.</summary>
    public static volatile bool Out;

    /// <summary>True when text written to standard error is painted.</summary>
    public static volatile bool Err;

    /// <summary>
    /// The escape sequence that starts ROLE, or null for a role that has no
    /// colour. The roles are the kinds of text the REPL tells apart.
    /// </summary>
    public static string? Sgr(string role) => role switch
    {
        "PROMPT" => "\u001b[1;32m",     // bold green: the package name
        "DEBUGGER" => "\u001b[1;31m",   // bold red: the debugger depth
        "SHELL" => "\u001b[1;35m",      // bold magenta: the shell mode prompt
        "COMMAND" => "\u001b[1;36m",    // bold cyan: the comma command prompt
        "RESULT" => "\u001b[36m",       // cyan: what a form returned
        "WARNING" => "\u001b[33m",      // yellow
        "ERROR" => "\u001b[31m",        // red
        "SELECTED" => "\u001b[7m",      // reverse video: the marked row of a menu
        "LOCATION" => "\u001b[1m",      // bold: a file:line the debugger points at
        "MATCH" => "\u001b[7m",         // reverse video: the bracket the one before the cursor closes
        "STRING" => "\u001b[32m",       // green: a string literal in the input line
        "COMMENT" => "\u001b[90m",      // bright black (grey): a comment in the input line
        "KEYWORD" => "\u001b[35m",      // magenta: a keyword in the input line
        _ => null,
    };

    /// <summary>
    /// Parse the value of <c>--color=</c>, or null when it is none of the three.
    /// </summary>
    public static ReplColorMode? ParseMode(string value) => value switch
    {
        "auto" => ReplColorMode.Auto,
        "always" => ReplColorMode.Always,
        "never" => ReplColorMode.Never,
        _ => null,
    };

    /// <summary>
    /// Whether to paint a stream. NO_COLOR (set to anything but the empty
    /// string, as no-color.org defines it) and TERM=dumb turn colour off
    /// whatever the mode says, --color=always included: they describe the
    /// place the text is going, which the person who set them knows better
    /// than a command line that may have come from an alias. Otherwise
    /// <c>always</c> and <c>never</c> mean what they say, and <c>auto</c>
    /// paints only a terminal, so that a log or a pipe never gets escape
    /// sequences mixed into it.
    ///
    /// A pure function of its arguments, so the decision is tested without a
    /// terminal.
    /// </summary>
    public static bool Decide(ReplColorMode mode, string? noColor, string? term, bool terminal)
    {
        if (!string.IsNullOrEmpty(noColor)) return false;
        if (ReplTerminal.Dumb(term)) return false;
        return mode switch
        {
            ReplColorMode.Always => true,
            ReplColorMode.Never => false,
            _ => terminal,
        };
    }

    /// <summary>
    /// Decide for both streams from the environment of this process. The caller
    /// says whether each stream is a terminal that understands escape sequences
    /// (on Windows: a console on which virtual terminal processing could be
    /// turned on).
    /// </summary>
    public static void Configure(ReplColorMode mode, bool stdoutTerminal, bool stderrTerminal)
    {
        var noColor = System.Environment.GetEnvironmentVariable("NO_COLOR");
        var term = System.Environment.GetEnvironmentVariable("TERM");
        Out = Decide(mode, noColor, term, stdoutTerminal);
        Err = Decide(mode, noColor, term, stderrTerminal);
    }

    /// <summary>TEXT painted as ROLE when ENABLED, TEXT itself otherwise.</summary>
    public static string Paint(string role, string text, bool enabled)
    {
        if (!enabled || text.Length == 0) return text;
        var sgr = Sgr(role);
        return sgr == null ? text : sgr + text + Reset;
    }

    /// <summary>TEXT as it should be written to standard output.</summary>
    public static string ForOut(string role, string text) => Paint(role, text, Out);

    /// <summary>TEXT as it should be written to standard error.</summary>
    public static string ForErr(string role, string text) => Paint(role, text, Err);

    /// <summary>
    /// TEXT as it should be written to STREAM: painted only when STREAM is the
    /// process's own standard output or standard error. A stream a program has
    /// bound *ERROR-OUTPUT* to (a string, a file) gets the text as it is.
    /// </summary>
    public static string ForStream(string role, string text, LispObject? stream)
    {
        if (stream != null && ReferenceEquals(stream, Startup.ErrorOutput)) return ForErr(role, text);
        if (stream != null && ReferenceEquals(stream, Startup.StandardOutput)) return ForOut(role, text);
        return text;
    }
}
