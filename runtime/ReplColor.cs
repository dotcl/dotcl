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
    /// The SGR parameters each role is painted with unless DOTCL_COLORS or
    /// <see cref="SetColors"/> says otherwise. Only the 16 basic colours and
    /// attributes, so every terminal theme maps them to something it chose.
    /// </summary>
    static readonly Dictionary<string, string> Defaults = new()
    {
        ["PROMPT"] = "1;32",    // bold green: the package name
        ["DEBUGGER"] = "1;31",  // bold red: the debugger depth
        ["SHELL"] = "1;35",     // bold magenta: the shell mode prompt
        ["COMMAND"] = "1;36",   // bold cyan: the comma command prompt
        ["RESULT"] = "36",      // cyan: what a form returned
        ["WARNING"] = "33",     // yellow
        ["ERROR"] = "31",       // red
        ["SELECTED"] = "7",     // reverse video: the marked row of a menu
        ["LOCATION"] = "1",     // bold: a file:line the debugger points at
        ["MATCH"] = "7",        // reverse video: the bracket the one before the cursor closes
        ["STRING"] = "32",      // green: a string literal in the input line
        // Faint: the terminal's own foreground, dimmed. Not bright black (90),
        // which some themes (Solarized Dark) make the background colour, so
        // the comment disappears.
        ["COMMENT"] = "2",
        ["KEYWORD"] = "35",     // magenta: a keyword in the input line
    };

    static readonly object OverridesLock = new();
    // Role to SGR parameters, "" for a role asked to have no colour. Starts as
    // what DOTCL_COLORS says.
    static Dictionary<string, string>? _overrides;

    static Dictionary<string, string> Overrides
    {
        get
        {
            lock (OverridesLock)
                return _overrides ??= ParseColors(
                    System.Environment.GetEnvironmentVariable("DOTCL_COLORS"));
        }
    }

    /// <summary>
    /// Read a colour specification in the form of GCC_COLORS:
    /// <c>role=params:role=params...</c>, where PARAMS is what goes between
    /// ESC [ and m (<c>1;32</c>). An empty PARAMS means no colour for that role.
    /// A role that is not one of the roles, or PARAMS with anything but digits
    /// and semicolons in it, is skipped without a word: a mistake in an
    /// environment variable must not stop the REPL from starting. Role names
    /// are compared without case; the result has them in upper case.
    /// </summary>
    public static Dictionary<string, string> ParseColors(string? spec)
    {
        var result = new Dictionary<string, string>();
        if (spec == null || spec.Length == 0) return result;
        foreach (var entry in spec.Split(':'))
        {
            int eq = entry.IndexOf('=');
            if (eq <= 0) continue;
            var role = entry.Substring(0, eq).Trim().ToUpperInvariant();
            var value = entry.Substring(eq + 1).Trim();
            if (!Defaults.ContainsKey(role)) continue;
            bool ok = true;
            foreach (var ch in value)
                if (!(ch == ';' || (ch >= '0' && ch <= '9'))) { ok = false; break; }
            if (!ok) continue;
            result[role] = value;
        }
        return result;
    }

    /// <summary>
    /// Apply SPEC (the DOTCL_COLORS form) on top of the colours in effect:
    /// the roles it names change, the others stay. For an init file.
    /// </summary>
    public static void SetColors(string? spec)
    {
        var parsed = ParseColors(spec);
        lock (OverridesLock)
        {
            var next = new Dictionary<string, string>(Overrides);
            foreach (var kv in parsed) next[kv.Key] = kv.Value;
            _overrides = next;
        }
    }

    /// <summary>
    /// The escape sequence that starts ROLE, or null for a role that has no
    /// colour. The roles are the kinds of text the REPL tells apart.
    /// </summary>
    public static string? Sgr(string role)
    {
        if (!Overrides.TryGetValue(role, out var sgr) && !Defaults.TryGetValue(role, out sgr))
            return null;
        return sgr.Length == 0 ? null : "\u001b[" + sgr + "m";
    }

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
