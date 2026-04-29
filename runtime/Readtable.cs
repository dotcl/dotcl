namespace DotCL;

/// <summary>Character syntax types as defined by CLHS 2.1.4.</summary>
public enum SyntaxType
{
    Constituent,
    Whitespace,
    TerminatingMacro,
    NonTerminatingMacro,
    SingleEscape,
    MultipleEscape,
    Invalid
}

/// <summary>Readtable case modes as defined by CLHS 23.1.2.</summary>
public enum ReadtableCase
{
    Upcase,
    Downcase,
    Preserve,
    Invert
}

/// <summary>
/// CL readtable object (CLHS 2.1.4, 23.2).
/// Maps characters to syntax types and reader macro functions.
/// </summary>
public class LispReadtable : LispObject
{
    // Syntax type for each character. Characters not in this map are Constituent by default.
    private readonly Dictionary<char, SyntaxType> _syntaxTypes = new();

    // Reader macro functions: char → C# delegate (stream, char) → LispObject?
    // null return means "no value produced" (re-enter step 1)
    private readonly Dictionary<char, Func<Reader, char, LispObject?>> _macroFunctions = new();

    // Which macro characters are non-terminating
    private readonly HashSet<char> _nonTerminating = new();

    // Original Lisp function objects for user-set macro characters
    // Used by GET-MACRO-CHARACTER to return the function the user passed to SET-MACRO-CHARACTER
    private readonly Dictionary<char, LispObject> _lispMacroFunctions = new();

    // Dispatch tables: dispatching-char → (sub-char → dispatch function(stream, sub-char, numarg))
    private readonly Dictionary<char, Dictionary<char, Func<Reader, char, int, LispObject?>>> _dispatchTables = new();

    public ReadtableCase Case { get; set; } = ReadtableCase.Upcase;

    /// <summary>Get the syntax type of a character.</summary>
    public SyntaxType GetSyntaxType(char ch)
    {
        if (_syntaxTypes.TryGetValue(ch, out var st))
            return st;
        // Default: printable characters are Constituent, control chars are Invalid
        if (ch >= ' ' && ch <= '~') return SyntaxType.Constituent;
        if (ch == '\x7F') return SyntaxType.Invalid; // Rubout/DEL is invalid
        if (ch > '~') return SyntaxType.Constituent; // extended chars
        return SyntaxType.Invalid;
    }

    /// <summary>Set the syntax type of a character.</summary>
    public void SetSyntaxType(char ch, SyntaxType type)
    {
        _syntaxTypes[ch] = type;
    }

    /// <summary>Check if a character is a macro character (terminating or non-terminating).</summary>
    public bool IsMacroCharacter(char ch)
    {
        var st = GetSyntaxType(ch);
        return st == SyntaxType.TerminatingMacro || st == SyntaxType.NonTerminatingMacro;
    }

    /// <summary>Check if a macro character is non-terminating.</summary>
    public bool IsNonTerminating(char ch) => _nonTerminating.Contains(ch);

    /// <summary>Get the reader macro function for a character, or null.</summary>
    public Func<Reader, char, LispObject?>? GetMacroFunction(char ch)
    {
        return _macroFunctions.TryGetValue(ch, out var fn) ? fn : null;
    }

    /// <summary>Set a reader macro function for a character.</summary>
    public void SetMacroCharacter(char ch, Func<Reader, char, LispObject?> fn, bool nonTerminating, LispObject? lispFn = null)
    {
        _macroFunctions[ch] = fn;
        if (lispFn != null)
            _lispMacroFunctions[ch] = lispFn;
        else
            _lispMacroFunctions.Remove(ch);
        SetSyntaxType(ch, nonTerminating ? SyntaxType.NonTerminatingMacro : SyntaxType.TerminatingMacro);
        if (nonTerminating)
            _nonTerminating.Add(ch);
        else
            _nonTerminating.Remove(ch);
    }

    /// <summary>Get the Lisp function object for a macro character, or null if built-in.</summary>
    public LispObject? GetLispMacroFunction(char ch)
    {
        return _lispMacroFunctions.TryGetValue(ch, out var fn) ? fn : null;
    }

    /// <summary>Make a character into a dispatching macro character.</summary>
    public void MakeDispatchMacroCharacter(char ch, bool nonTerminating)
    {
        SetSyntaxType(ch, nonTerminating ? SyntaxType.NonTerminatingMacro : SyntaxType.TerminatingMacro);
        if (nonTerminating) _nonTerminating.Add(ch); else _nonTerminating.Remove(ch);
        if (!_dispatchTables.ContainsKey(ch))
            _dispatchTables[ch] = new Dictionary<char, Func<Reader, char, int, LispObject?>>();
        // Register DispatchMacro as the reader macro function for this dispatching char.
        _macroFunctions[ch] = (reader, c) => reader.DispatchMacro(this, c);
    }

    /// <summary>Get the dispatch table for a dispatching macro character.</summary>
    public Dictionary<char, Func<Reader, char, int, LispObject?>>? GetDispatchTable(char ch)
    {
        return _dispatchTables.TryGetValue(ch, out var table) ? table : null;
    }

    /// <summary>Set a dispatch sub-character function.</summary>
    public void SetDispatchMacroCharacter(char dispChar, char subChar, Func<Reader, char, int, LispObject?> fn)
    {
        if (!_dispatchTables.TryGetValue(dispChar, out var table))
            throw new Exception($"{dispChar} is not a dispatching macro character");
        table[char.ToUpperInvariant(subChar)] = fn;
    }

    /// <summary>Get a dispatch sub-character function.</summary>
    public Func<Reader, char, int, LispObject?>? GetDispatchMacroCharacter(char dispChar, char subChar)
    {
        if (!_dispatchTables.TryGetValue(dispChar, out var table)) return null;
        return table.TryGetValue(char.ToUpperInvariant(subChar), out var fn) ? fn : null;
    }

    /// <summary>
    /// Copy syntax from one character to another (set-syntax-from-char).
    /// Copies syntax type and reader macro function. For dispatching macro chars,
    /// copies the entire dispatch table.
    /// </summary>
    public void CopySyntax(char toChar, char fromChar, LispReadtable fromReadtable)
    {
        var st = fromReadtable.GetSyntaxType(fromChar);
        SetSyntaxType(toChar, st);

        // Copy macro function if any
        var fn = fromReadtable.GetMacroFunction(fromChar);
        if (fn != null)
            _macroFunctions[toChar] = fn;
        else
            _macroFunctions.Remove(toChar);

        // Copy Lisp function object if any
        var lispFn = fromReadtable.GetLispMacroFunction(fromChar);
        if (lispFn != null)
            _lispMacroFunctions[toChar] = lispFn;
        else
            _lispMacroFunctions.Remove(toChar);

        // Copy non-terminating status
        if (fromReadtable._nonTerminating.Contains(fromChar))
            _nonTerminating.Add(toChar);
        else
            _nonTerminating.Remove(toChar);

        // Copy dispatch table if dispatching macro character
        if (fromReadtable._dispatchTables.TryGetValue(fromChar, out var srcTable))
        {
            var newTable = new Dictionary<char, Func<Reader, char, int, LispObject?>>(srcTable);
            _dispatchTables[toChar] = newTable;
            // Re-register macro function to reference this readtable's dispatch table
            // (the copied lambda from fromReadtable captures the wrong readtable reference)
            _macroFunctions[toChar] = (reader, c) => reader.DispatchMacro(this, c);
        }
        else
        {
            _dispatchTables.Remove(toChar);
        }
    }

    /// <summary>Copy all data from source readtable into this readtable (in-place).</summary>
    public void CopyFrom(LispReadtable source)
    {
        Case = source.Case;
        _syntaxTypes.Clear();
        foreach (var kv in source._syntaxTypes) _syntaxTypes[kv.Key] = kv.Value;
        _macroFunctions.Clear();
        foreach (var kv in source._macroFunctions) _macroFunctions[kv.Key] = kv.Value;
        _lispMacroFunctions.Clear();
        foreach (var kv in source._lispMacroFunctions) _lispMacroFunctions[kv.Key] = kv.Value;
        _nonTerminating.Clear();
        foreach (var ch in source._nonTerminating) _nonTerminating.Add(ch);
        _dispatchTables.Clear();
        foreach (var kv in source._dispatchTables)
            _dispatchTables[kv.Key] = new Dictionary<char, Func<Reader, char, int, LispObject?>>(kv.Value);
        // Re-register DispatchMacro closures to reference this readtable (not source)
        foreach (var kv in _dispatchTables)
        {
            var dispChar = kv.Key;
            _macroFunctions[dispChar] = (reader, c) => reader.DispatchMacro(this, c);
        }
    }

    /// <summary>Create a deep copy of this readtable.</summary>
    public LispReadtable Clone()
    {
        var copy = new LispReadtable();
        copy.Case = Case;
        foreach (var kv in _syntaxTypes) copy._syntaxTypes[kv.Key] = kv.Value;
        foreach (var kv in _macroFunctions) copy._macroFunctions[kv.Key] = kv.Value;
        foreach (var kv in _lispMacroFunctions) copy._lispMacroFunctions[kv.Key] = kv.Value;
        foreach (var ch in _nonTerminating) copy._nonTerminating.Add(ch);
        foreach (var kv in _dispatchTables)
            copy._dispatchTables[kv.Key] = new Dictionary<char, Func<Reader, char, int, LispObject?>>(kv.Value);
        // Re-register DispatchMacro closures to reference the copy (not source)
        foreach (var kv in copy._dispatchTables)
        {
            var dispChar = kv.Key;
            copy._macroFunctions[dispChar] = (reader, c) => reader.DispatchMacro(copy, c);
        }
        return copy;
    }

    /// <summary>Apply case conversion to a constituent character per readtable-case.</summary>
    public char ApplyCase(char ch)
    {
        return Case switch
        {
            ReadtableCase.Upcase => char.ToUpperInvariant(ch),
            ReadtableCase.Downcase => char.ToLowerInvariant(ch),
            ReadtableCase.Preserve => ch,
            ReadtableCase.Invert => ch, // Invert is applied after full token is read
            _ => ch
        };
    }

    public override string ToString() => "#<READTABLE>";

    /// <summary>
    /// Create the standard readtable with default CL syntax (CLHS 2.1.4 Figure 2-7).
    /// Reader macro functions are NOT set here — they must be registered after Reader is available.
    /// </summary>
    public static LispReadtable CreateStandard()
    {
        var rt = new LispReadtable();

        // Whitespace (CLHS 2.1.4): Tab, Newline, Linefeed, Page, Return, Space
        rt.SetSyntaxType('\t', SyntaxType.Whitespace);
        rt.SetSyntaxType('\n', SyntaxType.Whitespace);  // Newline/Linefeed
        rt.SetSyntaxType('\f', SyntaxType.Whitespace);  // Page
        rt.SetSyntaxType('\r', SyntaxType.Whitespace);  // Return
        rt.SetSyntaxType(' ', SyntaxType.Whitespace);

        // Single escape
        rt.SetSyntaxType('\\', SyntaxType.SingleEscape);

        // Multiple escape
        rt.SetSyntaxType('|', SyntaxType.MultipleEscape);

        // Terminating macro characters: " ' ( ) , ; `
        rt.SetSyntaxType('"', SyntaxType.TerminatingMacro);
        rt.SetSyntaxType('\'', SyntaxType.TerminatingMacro);
        rt.SetSyntaxType('(', SyntaxType.TerminatingMacro);
        rt.SetSyntaxType(')', SyntaxType.TerminatingMacro);
        rt.SetSyntaxType(',', SyntaxType.TerminatingMacro);
        rt.SetSyntaxType(';', SyntaxType.TerminatingMacro);
        rt.SetSyntaxType('`', SyntaxType.TerminatingMacro);

        // Non-terminating macro character: #
        rt.SetSyntaxType('#', SyntaxType.NonTerminatingMacro);

        // All other printable ASCII are Constituent by default (handled by GetSyntaxType fallback)

        return rt;
    }
}
