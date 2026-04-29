using System.Linq;
using System.Numerics;
using System.Text;

namespace DotCL;

/// <summary>
/// A token character with escape tracking (CLHS 2.2 step 8/9).
/// Escaped characters are not eligible as package markers.
/// </summary>
public struct TokenChar
{
    public char Ch;
    public bool Escaped;
    public TokenChar(char ch, bool escaped) { Ch = ch; Escaped = escaped; }
}

/// <summary>
/// A token accumulated by the Reader (CLHS 2.2 steps 7-9).
/// Tracks which characters were escaped for correct symbol interpretation.
/// </summary>
public class Token
{
    public List<TokenChar> Chars { get; } = new();
    public bool HasEscaped { get; set; }

    public int Length => Chars.Count;

    public override string ToString()
    {
        var sb = new StringBuilder(Chars.Count);
        foreach (var tc in Chars) sb.Append(tc.Ch);
        return sb.ToString();
    }

    /// <summary>
    /// Find the position of the first unescaped colon, or -1.
    /// Per CLHS 2.3.5, only unescaped colons are package markers.
    /// </summary>
    public int FindUnescapedColon()
    {
        for (int i = 0; i < Chars.Count; i++)
            if (Chars[i].Ch == ':' && !Chars[i].Escaped)
                return i;
        return -1;
    }

    /// <summary>Extract substring from token chars.</summary>
    public string Substring(int start, int length)
    {
        var sb = new StringBuilder(length);
        for (int i = start; i < start + length && i < Chars.Count; i++)
            sb.Append(Chars[i].Ch);
        return sb.ToString();
    }

    public string Substring(int start) => Substring(start, Chars.Count - start);
}

public class Reader
{
    private readonly TextReader _input;
    private int _pushedBack;
    private bool _hasPushback;
    private bool _readSuppress;
    private int _line = 1;
    private Dictionary<int, LispObject> _shareLabels = new();
    private Dictionary<int, SharePlaceholder> _sharePlaceholders = new();
    /// <summary>Thread-static backquote nesting level shared across all Readers.
    /// This allows Lisp-defined reader macros (which create new Reader instances
    /// via ReadFromStream) to correctly handle commas inside backquotes.</summary>
    [ThreadStatic]
    internal static int s_backquoteLevel;

    /// <summary>Set to true after Read() if the token was terminated by whitespace.</summary>
    public bool WhitespaceTerminated { get; private set; }

    /// <summary>Character position (number of chars consumed minus pushbacks). Useful for read-from-string.</summary>
    public int Position { get; private set; }

    /// <summary>Line number where the most recently read form started.</summary>
    public int LastFormLine { get; private set; } = 1;

    /// <summary>Lisp stream reference for stream-error-stream in error conditions.</summary>
    public LispObject? LispStreamRef { get; set; }

    /// <summary>The underlying TextReader, exposed for user-defined macro character functions.</summary>
    public TextReader Input => _input;

    public Reader(TextReader input)
    {
        _input = input;
    }

    /// <summary>
    /// Share #n=/#n# label tables with a LispStream, so multiple Reader instances
    /// on the same stream (e.g. C# dispatch macros called from SBCL's Lisp reader)
    /// can resolve share references across Reader lifetimes.
    /// </summary>
    public void AdoptStreamShareTables(LispStream stream)
    {
        if (stream.ShareLabels == null)
        {
            stream.ShareLabels = _shareLabels;
            stream.SharePlaceholders = _sharePlaceholders;
        }
        else
        {
            _shareLabels = stream.ShareLabels;
            _sharePlaceholders = stream.SharePlaceholders!;
        }
    }

    private LispErrorException MakeReaderError(string message)
    {
        var err = new LispError(message);
        err.ConditionTypeName = "READER-ERROR";
        if (LispStreamRef != null) err.StreamErrorStreamRef = LispStreamRef;
        return new LispErrorException(err);
    }

    private LispErrorException MakeEndOfFileError(string message)
    {
        var err = new LispError(message);
        err.ConditionTypeName = "END-OF-FILE";
        if (LispStreamRef != null) err.StreamErrorStreamRef = LispStreamRef;
        return new LispErrorException(err);
    }

    internal int Peek()
    {
        if (_hasPushback) return _pushedBack;
        return _input.Peek();
    }

    /// <summary>Consume one whitespace character if present (for read vs read-preserving-whitespace).</summary>
    public void ConsumeOneWhitespace()
    {
        int ch = Peek();
        if (ch != -1 && char.IsWhiteSpace((char)ch))
            ReadChar();
    }

    internal int ReadChar()
    {
        if (_hasPushback)
        {
            _hasPushback = false;
            if (_pushedBack == '\n') _line++;
            Position++;
            return _pushedBack;
        }
        var ch = _input.Read();
        if (ch == '\n') _line++;
        if (ch != -1) Position++;
        return ch;
    }

    internal void UnreadChar(int ch)
    {
        _pushedBack = ch;
        _hasPushback = true;
        if (ch != -1) Position--;
    }

    /// <summary>Skip whitespace characters based on current readtable syntax types.</summary>
    internal void SkipWhitespace()
    {
        var rt = GetCurrentReadtable();
        while (true)
        {
            int ch = Peek();
            if (ch == -1) return;
            if (rt.GetSyntaxType((char)ch) == SyntaxType.Whitespace)
            {
                ReadChar();
                continue;
            }
            break;
        }
    }

    /// <summary>Check if ch is a terminating character for token accumulation.</summary>
    private bool IsTerminating(int ch)
    {
        if (ch == -1) return true;
        var rt = GetCurrentReadtable();
        var st = rt.GetSyntaxType((char)ch);
        return st == SyntaxType.TerminatingMacro || st == SyntaxType.Whitespace;
    }

    /// <summary>Get the current readtable from *readtable*.</summary>
    private LispReadtable GetCurrentReadtable()
    {
        var rt = DynamicBindings.Get(Startup.Sym("*READTABLE*"));
        if (rt is LispReadtable lrt) return lrt;
        return Startup.StandardReadtable; // fallback
    }

    /// <summary>
    /// Read one form per CLHS 2.2 Reader Algorithm.
    /// Reader macros that produce no value return null; Read() loops until a value appears.
    /// </summary>
    public LispObject Read()
    {
        // Sync *read-suppress* dynamic variable
        var suppressSym = Startup.Sym("*READ-SUPPRESS*");
        bool dynSuppress = DynamicBindings.Get(suppressSym) is not Nil;
        var savedSuppress = _readSuppress;
        if (dynSuppress) _readSuppress = true;
        WhitespaceTerminated = false;
        try
        {
            while (true)
            {
                LastFormLine = _line;
                var obj = ReadStep1();
                if (obj != null)
                {
                    if (_readSuppress) return Nil.Instance;
                    return obj;
                }
            }
        }
        finally
        {
            _readSuppress = savedSuppress;
        }
    }

    public bool TryRead(out LispObject result)
    {
        // Sync *read-suppress* dynamic variable
        var suppressSym = Startup.Sym("*READ-SUPPRESS*");
        bool dynSuppress = DynamicBindings.Get(suppressSym) is not Nil;
        var savedSuppress = _readSuppress;
        if (dynSuppress) _readSuppress = true;
        try
        {
            while (true)
            {
                if (PeekSkipNothing() == -1)
                {
                    result = Nil.Instance;
                    return false;
                }
                LastFormLine = _line;
                var obj = ReadStep1();
                if (obj != null)
                {
                    result = _readSuppress ? Nil.Instance : obj;
                    return true;
                }
            }
        }
        finally
        {
            _readSuppress = savedSuppress;
        }
    }

    /// <summary>Peek after skipping whitespace only; used to detect EOF before reading a required form.</summary>
    private int PeekSkipWhitespaceOnly()
    {
        var rt = GetCurrentReadtable();
        while (true)
        {
            int ch = Peek();
            if (ch == -1) return -1;
            if (rt.GetSyntaxType((char)ch) == SyntaxType.Whitespace)
            {
                ReadChar();
                continue;
            }
            return ch;
        }
    }

    /// <summary>Peek without consuming anything; used only by TryRead for EOF check.</summary>
    private int PeekSkipNothing()
    {
        // Skip whitespace to check for true EOF.
        // We must NOT skip macro characters here — after set-syntax-from-char,
        // any character could have any macro function. Let ReadStep1 handle them.
        while (true)
        {
            int ch = Peek();
            if (ch == -1) return -1;
            var rt = GetCurrentReadtable();
            var st = rt.GetSyntaxType((char)ch);
            if (st == SyntaxType.Whitespace) { ReadChar(); continue; }
            return ch;
        }
    }

    /// <summary>
    /// CLHS 2.2 Step 1: Read a character and dispatch based on syntax type.
    /// Loops internally for whitespace. Returns null ONLY when a reader macro
    /// produces no value (e.g., #-feature skipped form, block comment).
    /// Callers like ReadList that need to handle ')' must check before calling.
    /// </summary>
    private LispObject? ReadStep1()
    {
        var rt = GetCurrentReadtable();

        while (true)
        {
            int ch = Peek();

            // EOF
            if (ch == -1) throw new EndOfStreamException("End of input");

            var st = rt.GetSyntaxType((char)ch);

            switch (st)
            {
                case SyntaxType.Invalid:
                    // Step 2: signal error (suppressed if *read-suppress*)
                    ReadChar();
                    if (_readSuppress)
                    {
                        var token = new Token();
                        token.Chars.Add(new TokenChar((char)ch, false));
                        return AccumulateAndInterpret(rt, token);
                    }
                    throw MakeReaderError($"Invalid character: {(char)ch} (code {ch})");

                case SyntaxType.Whitespace:
                    // Step 3: discard and re-enter step 1
                    ReadChar();
                    continue;

                case SyntaxType.TerminatingMacro:
                case SyntaxType.NonTerminatingMacro:
                {
                    // Step 4: call reader macro function
                    ReadChar();
                    var fn = rt.GetMacroFunction((char)ch);
                    if (fn != null)
                    {
                        var result = fn(this, (char)ch);
                        if (result != null) return result;
                        return null; // no value produced — return to caller
                    }
                    // No function registered — treat as constituent (shouldn't happen for standard chars)
                    var token = new Token();
                    token.Chars.Add(new TokenChar(rt.ApplyCase((char)ch), false));
                    return AccumulateAndInterpret(rt, token);
                }

                case SyntaxType.SingleEscape:
                {
                    // Step 5: read next char as escaped constituent, begin token
                    ReadChar(); // consume '\'
                    int next = ReadChar();
                    if (next == -1) throw MakeEndOfFileError("EOF after single escape");
                    var token = new Token();
                    token.HasEscaped = true;
                    token.Chars.Add(new TokenChar((char)next, true));
                    return AccumulateAndInterpret(rt, token);
                }

                case SyntaxType.MultipleEscape:
                {
                    // Step 6: begin empty token, enter step 9 (odd multiple escapes)
                    // After step 9, continue with step 8 (even multiple escapes)
                    // so that |pkg|::sym is read as a single token.
                    ReadChar(); // consume '|'
                    var token = new Token();
                    token.HasEscaped = true;
                    AccumulateStep9(rt, token);
                    return AccumulateAndInterpret(rt, token);
                }

                case SyntaxType.Constituent:
                {
                    // Step 7: begin token with this constituent
                    ReadChar();
                    // CLHS 2.1.4.2: characters with invalid constituent trait signal reader-error
                    if (!_readSuppress && HasInvalidConstituentTrait((char)ch))
                        throw MakeReaderError($"Character {(char)ch} (code {ch}) has invalid constituent trait");
                    var token = new Token();
                    token.Chars.Add(new TokenChar(rt.ApplyCase((char)ch), false));
                    return AccumulateAndInterpret(rt, token);
                }

                default:
                    ReadChar();
                    throw MakeReaderError($"Unexpected syntax type for character: {(char)ch}");
            }
        }
    }

    /// <summary>
    /// Continue token accumulation from step 8 (even multiple escapes) and then interpret.
    /// </summary>
    private LispObject AccumulateAndInterpret(LispReadtable rt, Token token)
    {
        AccumulateStep8(rt, token);
        if (rt.Case == ReadtableCase.Invert) ApplyInvertCase(token);
        return InterpretToken(token);
    }

    /// <summary>
    /// Apply :INVERT readtable-case to a completed token (CLHS 23.1.2).
    /// If all unescaped alphabetic chars are uppercase, convert them to lowercase.
    /// If all unescaped alphabetic chars are lowercase, convert them to uppercase.
    /// If mixed, leave as-is. Escaped characters are never converted.
    /// </summary>
    private static void ApplyInvertCase(Token token)
    {
        bool hasUpper = false, hasLower = false;
        for (int i = 0; i < token.Chars.Count; i++)
        {
            var tc = token.Chars[i];
            if (!tc.Escaped && char.IsLetter(tc.Ch))
            {
                if (char.IsUpper(tc.Ch)) hasUpper = true;
                else hasLower = true;
            }
        }

        // Mixed case or no alphabetic chars → leave as-is
        if ((hasUpper && hasLower) || (!hasUpper && !hasLower)) return;

        for (int i = 0; i < token.Chars.Count; i++)
        {
            var tc = token.Chars[i];
            if (!tc.Escaped && char.IsLetter(tc.Ch))
            {
                char converted = hasUpper
                    ? char.ToLowerInvariant(tc.Ch)
                    : char.ToUpperInvariant(tc.Ch);
                token.Chars[i] = new TokenChar(converted, false);
            }
        }
    }

    /// <summary>
    /// CLHS 2.2 Step 8: Token accumulation with even multiple escapes.
    /// </summary>
    private void AccumulateStep8(LispReadtable rt, Token token)
    {
        while (true)
        {
            int ch = Peek();
            if (ch == -1) return; // EOF → step 10

            var st = rt.GetSyntaxType((char)ch);

            switch (st)
            {
                case SyntaxType.Constituent:
                    ReadChar();
                    // CLHS 2.1.4.2: characters with invalid constituent trait signal reader-error
                    if (!_readSuppress && HasInvalidConstituentTrait((char)ch))
                        throw MakeReaderError($"Character {(char)ch} (code {ch}) has invalid constituent trait");
                    token.Chars.Add(new TokenChar(rt.ApplyCase((char)ch), false));
                    continue;
                case SyntaxType.NonTerminatingMacro:
                    ReadChar();
                    token.Chars.Add(new TokenChar(rt.ApplyCase((char)ch), false));
                    continue;

                case SyntaxType.SingleEscape:
                    ReadChar(); // consume '\'
                    int next = ReadChar();
                    if (next == -1) throw MakeEndOfFileError("EOF after single escape in token");
                    token.HasEscaped = true;
                    token.Chars.Add(new TokenChar((char)next, true));
                    continue;

                case SyntaxType.MultipleEscape:
                    ReadChar(); // consume '|'
                    token.HasEscaped = true;
                    AccumulateStep9(rt, token);
                    continue; // back to step 8

                case SyntaxType.TerminatingMacro:
                    // Don't consume — unread
                    return; // → step 10

                case SyntaxType.Whitespace:
                    // CLHS 2.2 step 8: whitespace terminates token, is unread
                    // read (not read-preserving-whitespace) will consume it later
                    WhitespaceTerminated = true;
                    return; // → step 10

                case SyntaxType.Invalid:
                    ReadChar();
                    if (_readSuppress)
                    {
                        token.Chars.Add(new TokenChar((char)ch, false));
                        continue;
                    }
                    throw MakeReaderError($"Invalid character in token: {(char)ch}");

                default:
                    return;
            }
        }
    }

    /// <summary>
    /// CLHS 2.2 Step 9: Token accumulation with odd multiple escapes.
    /// All characters are treated as alphabetic (escaped) until matching multiple escape.
    /// </summary>
    private void AccumulateStep9(LispReadtable rt, Token token)
    {
        while (true)
        {
            int ch = Peek();
            if (ch == -1) throw MakeEndOfFileError("EOF inside multiple escape");

            var st = rt.GetSyntaxType((char)ch);

            if (st == SyntaxType.MultipleEscape)
            {
                ReadChar(); // consume closing '|'
                return; // → back to step 8
            }
            else if (st == SyntaxType.SingleEscape)
            {
                ReadChar(); // consume '\'
                int next = ReadChar();
                if (next == -1) throw MakeEndOfFileError("EOF after single escape inside multiple escape");
                token.Chars.Add(new TokenChar((char)next, true));
            }
            else
            {
                // All other characters are treated as alphabetic (escaped, no case conversion)
                ReadChar();
                token.Chars.Add(new TokenChar((char)ch, true));
            }
        }
    }

    /// <summary>
    /// CLHS 2.2 Step 10: Interpret the accumulated token.
    /// Try as number first (if no escaped chars), otherwise as symbol.
    /// </summary>
    private LispObject InterpretToken(Token token)
    {
        if (token.Length == 0)
        {
            if (_readSuppress) return Nil.Instance;
            // Empty escaped token (e.g. ||) is a valid symbol with empty name
            if (token.HasEscaped)
                return ParseSymbol(token);
            throw MakeReaderError("Empty token");
        }

        var tokenStr = token.ToString();

        // CLHS 2.3.3: A token consisting solely of dots (without any escape
        // characters) is illegal except as the consing dot in a list context.
        if (!token.HasEscaped && !_readSuppress)
        {
            bool allDots = true;
            for (int i = 0; i < token.Length; i++)
            {
                if (token.Chars[i].Ch != '.')
                {
                    allDots = false;
                    break;
                }
            }
            if (allDots)
                throw MakeReaderError($"A token consisting solely of dots is illegal: \"{tokenStr}\"");
        }

        // Number interpretation: only if no escaped characters and not suppressed
        if (!token.HasEscaped && !_readSuppress && TryParseNumber(tokenStr, out var number))
            return number;

        // Symbol interpretation
        return ParseSymbol(token);
    }

    /// <summary>Reader macro function for '(' — read a list (CLHS 2.4.1).</summary>
    internal LispObject ReadList(char _triggerChar)
    {
        var items = new List<LispObject>();
        LispObject? dotCdr = null;

        while (true)
        {
            SkipWhitespace();
            int ch = Peek();
            if (ch == -1) throw MakeEndOfFileError("Unexpected end of input in list");

            if (ch == ')')
            {
                ReadChar();
                break;
            }

            // Check for dot — per CLHS, a token of just "." is the consing dot
            // Under *read-suppress*, skip dot detection entirely — treat as regular token
            if (ch == '.' && !_readSuppress)
            {
                ReadChar();
                int next = Peek();
                if (IsTerminating(next))
                {
                    // Consing dot
                    if (items.Count == 0 && !_readSuppress) throw MakeReaderError("Dot at start of list");
                    dotCdr = Read();
                    // Skip any no-value reader macros (e.g., #-feature skipped-form)
                    // before expecting ')'. CLHS allows reader macros to produce no token.
                    while (true)
                    {
                        SkipWhitespace();
                        if (Peek() == ')' || Peek() == -1) break;
                        var extra = ReadStep1();
                        if (extra != null && !_readSuppress)
                            throw MakeReaderError("Expected ')' after dotted pair");
                    }
                    if (Peek() != ')' && !_readSuppress)
                        throw MakeReaderError("Expected ')' after dotted pair");
                    if (Peek() == ')') ReadChar();
                    break;
                }
                else
                {
                    // Dot is start of a token (e.g., .5)
                    UnreadChar('.');
                }
            }

            // Read next element — ReadStep1 may return null for skipped forms (#+ etc.)
            var item = ReadStep1();
            if (item != null)
                items.Add(item);
            // If null, loop back to check for ')' again
        }

        LispObject result = dotCdr ?? Nil.Instance;
        for (int i = items.Count - 1; i >= 0; i--)
            result = new Cons(items[i], result);

        return result;
    }

    /// <summary>Reader macro function for ' (CLHS 2.4.3).</summary>
    internal LispObject ReadQuote(char _triggerChar)
    {
        var quoted = Read();
        return MakeList(Startup.Sym("QUOTE"), quoted);
    }

    /// <summary>Reader macro function for ` (CLHS 2.4.6).</summary>
    internal LispObject ReadBackquote(char _triggerChar)
    {
        s_backquoteLevel++;
        LispObject quoted;
        try { quoted = Read(); }
        finally { s_backquoteLevel--; }
        return ExpandBackquote(quoted);
    }

    /// <summary>
    /// Expand a backquoted form into LIST/CONS/APPEND/QUOTE combinations.
    /// Called at read time so the result contains no QUASIQUOTE/UNQUOTE symbols.
    /// </summary>
    private LispObject ExpandBackquote(LispObject form)
    {
        // Atom → (QUOTE atom)
        if (form is not Cons cons)
            return MakeList(Startup.QUOTE, form);

        // (UNQUOTE x) → x
        if (cons.Car is Symbol sym1 && ReferenceEquals(sym1, Startup.UNQUOTE))
            return ((Cons)cons.Cdr).Car;

        // (UNQUOTE-SPLICING x) or (UNQUOTE-NSPLICING x) at top level is an error
        if (cons.Car is Symbol sym2 && (ReferenceEquals(sym2, Startup.UNQUOTE_SPLICING)
            || ReferenceEquals(sym2, Startup.UNQUOTE_NSPLICING)))
            throw MakeReaderError(",@/,. after backquote in non-list context");

        // List form: process each element
        return ExpandBackquoteList(cons);
    }

    /// <summary>
    /// Expand a backquoted list by processing each element.
    /// Returns an APPEND of LIST/spliced segments.
    /// </summary>
    private LispObject ExpandBackquoteList(Cons form)
    {
        var segments = new List<LispObject>();
        var nspliceIndices = new HashSet<int>();
        LispObject current = form;

        while (current is Cons c)
        {
            var element = c.Car;

            if (element is Cons inner)
            {
                if (inner.Car is Symbol us && (ReferenceEquals(us, Startup.UNQUOTE_SPLICING)
                    || ReferenceEquals(us, Startup.UNQUOTE_NSPLICING)))
                {
                    // ,@x or ,.x → x (spliced)
                    // Treat ,. same as ,@ (use APPEND not NCONC) to avoid
                    // circular list structures from destructive splicing
                    var spliceForm = ((Cons)inner.Cdr).Car;
                    segments.Add(spliceForm);
                }
                else if (inner.Car is Symbol uq && ReferenceEquals(uq, Startup.UNQUOTE))
                {
                    // ,x → (LIST x)
                    segments.Add(MakeList(Startup.Sym("LIST"), ((Cons)inner.Cdr).Car));
                }
                else
                {
                    // Nested list → (LIST (expand-backquote inner))
                    segments.Add(MakeList(Startup.Sym("LIST"), ExpandBackquote(inner)));
                }
            }
            else if (element is Symbol usym && ReferenceEquals(usym, Startup.UNQUOTE))
            {
                // Dot-position unquote: `(a . ,b) — but this shouldn't happen
                // since ReadComma wraps in (UNQUOTE x)
                segments.Add(MakeList(Startup.Sym("LIST"), element));
            }
            else
            {
                // Atom element → (LIST (QUOTE atom))
                segments.Add(MakeList(Startup.Sym("LIST"), MakeList(Startup.QUOTE, element)));
            }

            current = c.Cdr;

            // Check if CDR is (UNQUOTE x) — a dotted-pair unquote like `(a . ,b)
            // The reader produces (a . (UNQUOTE b)) which is a 3-element list,
            // but we must NOT iterate into it — break and let the dotted-tail handler process it.
            if (current is Cons nextC && nextC.Car is Symbol nextSym
                && ReferenceEquals(nextSym, Startup.UNQUOTE))
                break;
        }

        // Handle dotted tail
        if (current is not Nil)
        {
            // Dotted pair tail
            if (current is Cons dc && dc.Car is Symbol dcs && ReferenceEquals(dcs, Startup.UNQUOTE))
            {
                // `(a . ,b) → tail is (UNQUOTE b)
                segments.Add(((Cons)dc.Cdr).Car);
            }
            else
            {
                segments.Add(MakeList(Startup.QUOTE, current));
            }
        }

        // Optimize: if only one segment and no splicing needed
        if (segments.Count == 1 && current is Nil)
            return segments[0];

        // Build (APPEND seg1 seg2 ...) for multiple segments
        // But our Runtime.Append only takes 2 args, so nest them
        if (segments.Count == 0)
            return Nil.Instance;

        var result = segments[segments.Count - 1];
        // If the last segment came from a proper list (nil tail), wrap in append context
        if (current is Nil && segments.Count > 1)
        {
            for (int i = segments.Count - 2; i >= 0; i--)
            {
                var op = nspliceIndices.Contains(i) ? Startup.Sym("NCONC") : Startup.Sym("APPEND");
                result = MakeList(op, segments[i], result);
            }
        }
        else if (current is not Nil)
        {
            // Dotted tail: last segment is the tail value
            for (int i = segments.Count - 2; i >= 0; i--)
            {
                var op = nspliceIndices.Contains(i) ? Startup.Sym("NCONC") : Startup.Sym("APPEND");
                result = MakeList(op, segments[i], result);
            }
        }
        else
        {
            // Single segment, already handled above
        }

        return result;
    }

    /// <summary>Reader macro function for , (CLHS 2.4.7).</summary>
    internal LispObject ReadComma(char _triggerChar)
    {
        if (s_backquoteLevel == 0)
            throw MakeReaderError("Comma is not inside a backquote");
        var ch = Peek();
        if (ch == '@')
        {
            ReadChar();
            var form = Read();
            return MakeList(Startup.UNQUOTE_SPLICING, form);
        }
        if (ch == '.')
        {
            ReadChar();
            var form = Read();
            return MakeList(Startup.UNQUOTE_NSPLICING, form);
        }
        var expr = Read();
        return MakeList(Startup.Sym("UNQUOTE"), expr);
    }

    /// <summary>Reader macro function for " (CLHS 2.4.5).</summary>
    internal LispObject ReadString(char _triggerChar)
    {
        var sb = new StringBuilder();
        var rt = GetCurrentReadtable();

        while (true)
        {
            int ch = ReadChar();
            if (ch == -1) throw MakeEndOfFileError("Unterminated string");
            if (ch == _triggerChar) break;
            if (rt.GetSyntaxType((char)ch) == SyntaxType.SingleEscape)
            {
                // CLHS 2.4.5: single-escape character causes the next character
                // to be included literally, regardless of what it is.
                ch = ReadChar();
                if (ch == -1) throw MakeEndOfFileError("Unterminated string escape");
                sb.Append((char)ch);
            }
            else
            {
                sb.Append((char)ch);
            }
        }

        return new LispString(sb.ToString());
    }

    // ReadHash is no longer needed — '#' is handled via DispatchMacro + dispatch table.

    internal LispObject ReadArrayLiteral(int rank)
    {
        if (_readSuppress)
        {
            Read(); // consume contents
            return new Symbol("NIL");
        }

        var contents = Read() ?? Nil.Instance;

        if (rank == 0)
        {
            // 0-dimensional array: #0A element (scalar)
            return new LispVector(new LispObject[] { contents }, Array.Empty<int>(), "T");
        }

        // For rank 1, handle non-list sequences (strings, vectors, bit-vectors)
        if (rank == 1)
        {
            if (contents is LispString str)
            {
                var chars = str.Value.Select(c => (LispObject)LispChar.Make(c)).ToArray();
                return new LispVector(chars, "T");
            }
            if (contents is LispVector vec)
            {
                // Already a vector; re-wrap as general vector
                var elts = new LispObject[vec.Length];
                for (int j = 0; j < vec.Length; j++)
                    elts[j] = vec[j];
                return new LispVector(elts, "T");
            }
            // Fall through to list handling
        }

        // Compute dimensions by traversing the first-element path
        var dims = new int[rank];
        ComputeArrayDims(contents, dims, 0, rank);

        int total = 1;
        foreach (var d in dims) total *= d;

        var items = new LispObject[total];
        Array.Fill(items, Nil.Instance);
        FlattenArrayContents(contents, items, 0, rank);

        return rank == 1
            ? new LispVector(items, "T")
            : new LispVector(items, dims, "T");
    }

    internal LispObject ReadStructureLiteral()
    {
        if (_readSuppress)
        {
            Read(); // consume the list
            return new Symbol("NIL");
        }

        // Read the list: (type-name :slot1 val1 :slot2 val2 ...)
        var list = Read();
        if (list is not Cons firstCons)
            throw MakeReaderError("#S requires a list argument");

        var typeName = firstCons.Car;
        if (typeName is not Symbol typeSymbol)
            throw MakeReaderError("#S requires a symbol as structure type name");

        // Look up the class to get slot names
        var cls = Runtime.FindClassOrNil(typeSymbol) as LispClass;
        if (cls == null || !cls.IsStructureClass || cls.StructSlotNames == null)
            throw MakeReaderError($"#S: {typeSymbol.Name} is not a known structure type");

        var slotNames = cls.StructSlotNames;
        var slots = new LispObject[slotNames.Length];
        Array.Fill(slots, Nil.Instance);

        // Parse slot-value pairs from the rest of the list
        var rest = firstCons.Cdr;
        var pairs = new List<(string key, LispObject value)>();

        while (rest is Cons pair)
        {
            var key = pair.Car;
            string keyName;
            if (key is Symbol keySym)
                keyName = keySym.Name;
            else if (key is LispString keyStr)
                keyName = keyStr.Value.ToUpperInvariant();
            else if (key is LispChar keyChar)
                keyName = keyChar.Value.ToString().ToUpperInvariant();
            else
                throw MakeReaderError($"#S: invalid slot name: {key}");

            rest = pair.Cdr;
            if (rest is not Cons valueCons)
                throw MakeReaderError($"#S: odd number of arguments after type name");

            pairs.Add((keyName, valueCons.Car));
            rest = valueCons.Cdr;
        }

        // Assign values to slots (first occurrence wins per CLHS)
        var assigned = new bool[slotNames.Length];
        foreach (var (key, value) in pairs)
        {
            if (key == "ALLOW-OTHER-KEYS") continue;

            int idx = -1;
            for (int i = 0; i < slotNames.Length; i++)
            {
                if (slotNames[i].Name == key) { idx = i; break; }
            }

            if (idx >= 0 && !assigned[idx])
            {
                slots[idx] = value;
                assigned[idx] = true;
            }
        }

        return new LispStruct(typeSymbol, slots);
    }

    private static void ComputeArrayDims(LispObject contents, int[] dims, int level, int rank)
    {
        if (level >= rank) return;
        int count = 0;
        LispObject? first = null;
        if (contents is LispVector vec)
        {
            count = vec.Length;
            if (count > 0) first = vec[0];
        }
        else
        {
            var cur = contents;
            while (cur is Cons c) { count++; if (first == null) first = c.Car; cur = c.Cdr; }
        }
        dims[level] = count;
        if (first != null && level + 1 < rank)
            ComputeArrayDims(first, dims, level + 1, rank);
    }

    private static int FlattenArrayContents(LispObject contents, LispObject[] items, int idx, int rank)
    {
        if (rank <= 1)
        {
            if (contents is LispVector vec)
            {
                for (int i = 0; i < vec.Length; i++)
                    if (idx < items.Length) items[idx++] = vec[i];
            }
            else
            {
                var cur = contents;
                while (cur is Cons c) { if (idx < items.Length) items[idx++] = c.Car; cur = c.Cdr; }
            }
        }
        else
        {
            if (contents is LispVector vec)
            {
                for (int i = 0; i < vec.Length; i++)
                    idx = FlattenArrayContents(vec[i], items, idx, rank - 1);
            }
            else
            {
                var cur = contents;
                while (cur is Cons c) { idx = FlattenArrayContents(c.Car, items, idx, rank - 1); cur = c.Cdr; }
            }
        }
        return idx;
    }

    internal LispObject ReadShareLabel(int label)
    {
        // #n= obj — define shared structure label
        if (_readSuppress) { Read(); return Nil.Instance; }
        if (label == -1) throw MakeReaderError("#= requires a numeric label");
        // Pre-register a placeholder so self-references (#n#) during Read() can find it
        SharePlaceholder? ph = null;
        if (!_sharePlaceholders.TryGetValue(label, out ph))
        {
            ph = new SharePlaceholder(label);
            _sharePlaceholders[label] = ph;
        }
        var obj = Read();
        _shareLabels[label] = obj;
        ph.Value = obj;
        _sharePlaceholders.Remove(label);
        // Patch any structures containing this placeholder
        PatchPlaceholders(obj, ph, obj, new HashSet<object>());
        return obj;
    }

    /// <summary>
    /// Walk a structure and replace SharePlaceholder references with the actual object.
    /// </summary>
    private static void PatchPlaceholders(LispObject root, SharePlaceholder ph, LispObject replacement, HashSet<object> visited)
    {
        if (!visited.Add(root)) return;
        if (root is Cons cons)
        {
            if (cons.Car is SharePlaceholder p1 && p1 == ph)
                cons.Car = replacement;
            else
                PatchPlaceholders(cons.Car, ph, replacement, visited);
            if (cons.Cdr is SharePlaceholder p2 && p2 == ph)
                cons.Cdr = replacement;
            else
                PatchPlaceholders(cons.Cdr, ph, replacement, visited);
        }
        else if (root is LispVector vec)
        {
            for (int i = 0; i < vec.Length; i++)
            {
                if (vec[i] is SharePlaceholder p && p == ph)
                    vec[i] = replacement;
                else
                    PatchPlaceholders(vec[i], ph, replacement, visited);
            }
        }
        else if (root is LispStruct st)
        {
            for (int i = 0; i < st.Slots.Length; i++)
            {
                if (st.Slots[i] is SharePlaceholder p && p == ph)
                {
                    st.Slots[i] = replacement;
                }
                else
                    PatchPlaceholders(st.Slots[i], ph, replacement, visited);
            }
        }
        else if (root is LispInstance inst)
        {
            for (int i = 0; i < inst.Slots.Length; i++)
            {
                if (inst.Slots[i] is SharePlaceholder p && p == ph)
                    inst.Slots[i] = replacement;
                else if (inst.Slots[i] != null)
                    PatchPlaceholders(inst.Slots[i]!, ph, replacement, visited);
            }
        }
    }

    internal LispObject ReadShareRef(int label)
    {
        // #n# — reference to shared structure
        if (_readSuppress) return Nil.Instance;
        if (label == -1) throw MakeReaderError("## requires a numeric label");
        if (_shareLabels.TryGetValue(label, out var obj))
            return obj;
        // Forward reference: return placeholder
        if (!_sharePlaceholders.TryGetValue(label, out var ph))
        {
            ph = new SharePlaceholder(label);
            _sharePlaceholders[label] = ph;
        }
        return ph;
    }

    internal LispObject ReadBitVector(int numArg = -1)
    {
        // #*0110... → bit vector (LispVector of Fixnum 0/1)
        // #n*01... → bit vector of length n, fill remaining with last bit
        if (_readSuppress)
        {
            // In suppress mode, consume all constituent characters (not just 0/1)
            while (true)
            {
                int ch = Peek();
                if (ch == -1 || IsTerminating(ch)) break;
                ReadChar();
            }
            return Nil.Instance;
        }
        var bits = new System.Collections.Generic.List<LispObject>();
        while (true)
        {
            int ch = Peek();
            if (ch == '0') { ReadChar(); bits.Add(Fixnum.Make(0)); }
            else if (ch == '1') { ReadChar(); bits.Add(Fixnum.Make(1)); }
            else if (ch != -1 && !IsTerminating(ch) && !char.IsWhiteSpace((char)ch))
            {
                // Non-bit constituent character (e.g., '2') — signal error
                throw MakeReaderError($"Invalid bit character '{(char)ch}' in bit vector");
            }
            else break;
        }
        if (numArg >= 0)
        {
            int length = numArg;
            if (bits.Count > length)
                throw MakeReaderError($"Bit vector has {bits.Count} bits but #n* specified length {length}");
            if (bits.Count < length)
            {
                if (bits.Count == 0)
                    throw MakeReaderError($"Bit vector #n* with length {length} requires at least one bit");
                var fill = bits[bits.Count - 1];
                while (bits.Count < length)
                    bits.Add(fill);
            }
        }
        return new LispVector(bits.ToArray(), "BIT");
    }

    internal LispObject ReadPathnameShorthand()
    {
        // #P"..." → pathname object
        var str = Read();
        if (_readSuppress) return new Symbol("NIL");
        string? nameStr = null;
        if (str is LispString s)
            nameStr = s.Value;
        else if (str is LispVector vec && vec.IsCharVector)
            nameStr = vec.ToCharString();
        if (nameStr != null)
        {
            // Detect logical pathname strings
            if (Runtime.IsLogicalPathnameString(nameStr))
                return LispLogicalPathname.FromLogicalString(nameStr);
            return LispPathname.FromString(nameStr);
        }
        throw MakeReaderError($"#P requires a string argument, got: {str}");
    }

    internal LispObject ReadReadTimeEval()
    {
        // #. form — read and evaluate at read time
        // CLHS 2.4.8.6: signal reader-error if *read-eval* is NIL
        var readEvalSym = Startup.Sym("*READ-EVAL*");
        var readEval = DynamicBindings.Get(readEvalSym);
        if (readEval is Nil)
            throw MakeReaderError("#. is not allowed when *READ-EVAL* is NIL");
        // Signal end-of-file if EOF is encountered
        if (PeekSkipWhitespaceOnly() == -1)
            throw MakeEndOfFileError("EOF after #.");
        var form = Read();
        if (_readSuppress) return new Symbol("NIL"); // suppress mode: return dummy
        // #. only uses the primary value; unwrap MvReturn so it doesn't leak
        // into source data (D641, issue #19).
        return MultipleValues.Primary(Runtime.Eval(form));
    }

    internal LispObject ReadFunctionShorthand()
    {
        // #'name → (FUNCTION name)
        // Signal end-of-file if EOF is encountered
        if (PeekSkipWhitespaceOnly() == -1)
            throw MakeEndOfFileError("EOF after #'");
        var name = Read();
        return MakeList(Startup.Sym("FUNCTION"), name);
    }

    internal LispObject ReadCharacterLiteral()
    {
        // #\x or #\Space etc.
        int first = ReadChar();
        if (first == -1) throw MakeEndOfFileError("Unexpected end of input in character literal");

        // Check if the next character is also a constituent (multi-char name)
        int peeked = Peek();
        bool multiChar = peeked != -1 && !IsTerminating(peeked) && !char.IsWhiteSpace((char)peeked);

        if (multiChar)
        {
            var sb = new StringBuilder();
            sb.Append((char)first);
            while (true)
            {
                int next = Peek();
                if (next == -1 || IsTerminating(next)) break;
                ReadChar();
                sb.Append((char)next);
            }

            var name = sb.ToString();
            if (_readSuppress) return Nil.Instance;

            if (name.Length == 1)
                return LispChar.Make(name[0]);

            // Use Runtime.NameChar for unified character name lookup
            var ch = Runtime.NameChar(name);
            if (ch.HasValue)
                return LispChar.Make(ch.Value);
            throw MakeReaderError($"Unknown character name: {name}");
        }

        return _readSuppress ? (LispObject)Nil.Instance : LispChar.Make((char)first);
    }

    internal LispObject ReadVector(int numArg = -1)
    {
        // #( ... ) → vector
        // #n( ... ) → vector of length n, fill remaining with last element
        var items = new List<LispObject>();
        while (true)
        {
            SkipWhitespace();
            if (Peek() == ')')
            {
                ReadChar();
                break;
            }
            if (Peek() == -1) throw MakeEndOfFileError("Unterminated vector literal");
            var item = ReadStep1();
            if (item != null)
                items.Add(item);
        }
        if (_readSuppress) return Nil.Instance;
        if (numArg >= 0)
        {
            int length = numArg;
            if (items.Count > length)
                throw MakeReaderError($"Vector has {items.Count} elements but #n( specified length {length}");
            if (items.Count < length)
            {
                if (items.Count == 0)
                    throw MakeReaderError($"Cannot fill vector of length {length} with no elements");
                var fill = items[items.Count - 1];
                while (items.Count < length)
                    items.Add(fill);
            }
        }
        return new LispVector(items.ToArray());
    }

    internal LispObject? ReadBlockComment()
    {
        // #| ... |# — nested block comments; produces no value
        int depth = 1;
        while (depth > 0)
        {
            int ch = ReadChar();
            if (ch == -1) throw MakeEndOfFileError("Unterminated block comment");
            if (ch == '#' && Peek() == '|')
            {
                ReadChar();
                depth++;
            }
            else if (ch == '|' && Peek() == '#')
            {
                ReadChar();
                depth--;
            }
        }
        return null;
    }

    internal LispObject ReadUninterned()
    {
        // #:name → uninterned symbol (token already uppercased by ReadToken)
        var token = ReadToken();
        if (GetCurrentReadtable().Case == ReadtableCase.Invert) ApplyInvertCase(token);
        if (_readSuppress) return Nil.Instance;
        // CLHS 2.4.8.5: uninterned symbols must not contain a package prefix
        int colonPos = token.FindUnescapedColon();
        if (colonPos > 0)
            throw MakeReaderError("Uninterned symbol contains a package marker");
        return new Symbol(token.ToString());
    }

    internal LispObject? ReadFeature(bool positive)
    {
        // #+feature form  or  #-feature form
        // CLHS 2.4.8.17: feature expression is read with *package* bound to KEYWORD
        var pkgSym = Startup.Sym("*PACKAGE*");
        DynamicBindings.Push(pkgSym, Package.FindPackage("KEYWORD")!);
        LispObject feature;
        try { feature = Read(); }
        finally { DynamicBindings.Pop(pkgSym); }
        bool hasFeature = EvaluateFeature(feature);
        bool shouldInclude = positive ? hasFeature : !hasFeature;

        if (shouldInclude)
            return Read();

        // Feature not matched — read in suppressed mode (tolerates unknown packages)
        var saved = _readSuppress;
        _readSuppress = true;
        try { Read(); }
        finally { _readSuppress = saved; }
        return null;
    }

    /// <summary>
    /// Recursively evaluate a feature expression:
    ///   symbol        → Startup.HasFeature(name)
    ///   (OR f1 f2 ..) → any sub-feature matches
    ///   (AND f1 f2..) → all sub-features match
    ///   (NOT f)       → sub-feature does not match
    /// </summary>
    private static bool EvaluateFeature(LispObject feature)
    {
        if (feature is Symbol sym)
        {
            // Check the *features* dynamic variable (CLHS 24.1.2.1)
            // Features are compared using EQ (symbol identity)
            var featuresSym = Startup.Sym("*FEATURES*");
            var featuresList = DynamicBindings.Get(featuresSym);
            var cur = featuresList;
            while (cur is Cons c)
            {
                if (ReferenceEquals(c.Car, sym))
                    return true;
                cur = c.Cdr;
            }
            return false;
        }

        if (feature is Cons cons && cons.Car is Symbol op)
        {
            var name = op.Name.ToUpperInvariant();
            switch (name)
            {
                case "OR":
                {
                    LispObject rest = cons.Cdr;
                    while (rest is Cons c)
                    {
                        if (EvaluateFeature(c.Car)) return true;
                        rest = c.Cdr;
                    }
                    return false;
                }
                case "AND":
                {
                    LispObject rest = cons.Cdr;
                    while (rest is Cons c)
                    {
                        if (!EvaluateFeature(c.Car)) return false;
                        rest = c.Cdr;
                    }
                    return true;
                }
                case "NOT":
                {
                    if (cons.Cdr is Cons nc)
                        return !EvaluateFeature(nc.Car);
                    return true; // (NOT) with no arg → true
                }
            }
        }

        // Unknown feature expression form → false
        return false;
    }

    internal LispObject ReadComplex()
    {
        if (_readSuppress)
        {
            Read(); // consume the (real imag) list
            return Nil.Instance;
        }
        // #C(real imag)
        SkipWhitespace();
        if (ReadChar() != '(') throw MakeReaderError("Expected ( after #C");
        var real = Read();
        var imag = Read();
        SkipWhitespace();
        if (ReadChar() != ')') throw MakeReaderError("Expected ) in #C(...)");

        if (real is not Number rn || imag is not Number im)
            throw MakeReaderError("#C requires numeric arguments");

        return Arithmetic.MakeComplexPublic(rn, im);
    }

    internal LispObject ReadRadixNumber(int radix)
    {
        var token = ReadTokenString();
        if (_readSuppress) return Nil.Instance;
        try
        {
            int slashPos = token.IndexOf('/');
            if (slashPos >= 0)
            {
                var numStr = token.Substring(0, slashPos);
                var denStr = token.Substring(slashPos + 1);
                var num = ParseIntegerRadix(numStr, radix);
                var den = ParseIntegerRadix(denStr, radix);
                if (den == 0)
                    throw MakeReaderError($"Zero denominator in ratio: {token}");
                return (Number)Ratio.Make(num, den);
            }
            var value = ParseIntegerRadix(token, radix);
            return Bignum.MakeInteger(value);
        }
        catch
        {
            throw MakeReaderError($"Invalid base-{radix} number: {token}");
        }
    }

    private static BigInteger ParseIntegerRadix(string token, int radix)
    {
        bool negative = false;
        int start = 0;
        if (token.Length > 0 && token[0] == '-') { negative = true; start = 1; }
        else if (token.Length > 0 && token[0] == '+') { start = 1; }

        BigInteger result = 0;
        for (int i = start; i < token.Length; i++)
        {
            int digit = DigitValue(token[i]);
            if (digit < 0 || digit >= radix)
                throw new FormatException($"Invalid digit '{token[i]}' for base {radix}");
            result = result * radix + digit;
        }

        return negative ? -result : result;
    }

    private static int DigitValue(char c)
    {
        if (c >= '0' && c <= '9') return c - '0';
        if (c >= 'a' && c <= 'z') return c - 'a' + 10;
        if (c >= 'A' && c <= 'Z') return c - 'A' + 10;
        return -1;
    }

    private LispObject ReadAtom()
    {
        var token = ReadToken();
        if (GetCurrentReadtable().Case == ReadtableCase.Invert) ApplyInvertCase(token);
        if (token.Length == 0)
        {
            if (_readSuppress) return Nil.Instance;
            throw MakeReaderError("Empty token");
        }

        var tokenStr = token.ToString();

        // Skip number parsing when token has escaped chars;
        // in CL, |123| is a symbol named "123", not the number 123.
        if (!token.HasEscaped && TryParseNumber(tokenStr, out var number))
            return number;

        // It's a symbol
        return ParseSymbol(token);
    }

    /// <summary>
    /// Accumulate a token per CLHS 2.2 steps 7-9.
    /// Returns a Token with per-character escape tracking.
    /// </summary>
    private Token ReadToken()
    {
        var token = new Token();
        bool inEscape = false; // inside |...| (odd multiple escapes = step 9)

        while (true)
        {
            int ch = Peek();

            if (inEscape)
            {
                // Step 9: odd multiple escapes
                if (ch == -1) throw MakeEndOfFileError("Unterminated escape in token");
                if (ch == '|')
                {
                    ReadChar();
                    inEscape = false; // back to step 8
                    continue;
                }
                if (ch == '\\')
                {
                    // Single escape inside multiple escape
                    ReadChar();
                    ch = ReadChar();
                    if (ch == -1) throw MakeEndOfFileError("Unterminated escape in token");
                    token.Chars.Add(new TokenChar((char)ch, true));
                    token.HasEscaped = true;
                    continue;
                }
                ReadChar();
                token.HasEscaped = true;
                token.Chars.Add(new TokenChar((char)ch, true)); // case-preserved, escaped
                continue;
            }

            // Step 8: even multiple escapes
            if (ch == '|')
            {
                ReadChar();
                inEscape = true;
                token.HasEscaped = true; // even empty || counts as escaped
                continue;
            }

            if (ch == '\\')
            {
                // Single escape
                ReadChar();
                ch = ReadChar();
                if (ch == -1) throw MakeEndOfFileError("Unterminated escape in token");
                token.HasEscaped = true;
                token.Chars.Add(new TokenChar((char)ch, true)); // case-preserved, escaped
                continue;
            }

            if (IsTerminating(ch)) break;
            if (ch == -1) break;
            ReadChar();
            // Constituent: apply readtable case (default upcase)
            var currentRt = GetCurrentReadtable();
            token.Chars.Add(new TokenChar(currentRt.ApplyCase((char)ch), false));
        }

        return token;
    }

    /// <summary>Legacy wrapper for callers that only need the string.</summary>
    private string ReadTokenString()
    {
        var token = ReadToken();
        return token.ToString();
    }

    private bool TryParseNumber(string token, out Number result)
    {
        result = null!;
        if (token.Length == 0) return false;

        var upper = token.ToUpperInvariant();
        int readBase = GetReadBase();

        // Trailing dot means integer in CL (e.g., "10." = 10)
        // Per CLHS, trailing dot forces base 10 interpretation
        if (upper.Length > 1 && upper[^1] == '.' && IsAllDigitsInBase(upper[..^1], 10))
        {
            if (BigInteger.TryParse(upper[..^1], out var iv))
            {
                result = (Number)Bignum.MakeInteger(iv);
                return true;
            }
        }

        // Ratio: N/D — uses *read-base*
        int slashPos = upper.IndexOf('/');
        if (slashPos > 0 && slashPos < upper.Length - 1 && !upper[(slashPos + 1)..].Contains('/'))
        {
            var numPart = upper[..slashPos];
            var denPart = upper[(slashPos + 1)..];
            if (IsAllDigitsInBase(numPart, readBase) && IsAllDigitsInBase(denPart, readBase))
            {
                BigInteger num = 0, den = 0;
                bool parsed = false;
                try
                {
                    num = ParseIntegerRadix(numPart, readBase);
                    den = ParseIntegerRadix(denPart, readBase);
                    parsed = true;
                }
                catch { }
                if (parsed)
                {
                    // Throw reader-error OUTSIDE try-catch so handler-case can catch it
                    if (den == 0)
                        throw MakeReaderError($"Zero denominator in ratio: {token}");
                    result = (Number)Ratio.Make(num, den);
                    return true;
                }
            }
        }

        // Integer — uses *read-base* (check before floats so that hex digits
        // like D/E/F are not misinterpreted as float exponent markers)
        if (IsAllDigitsInBase(upper, readBase))
        {
            try
            {
                var bv = ParseIntegerRadix(upper, readBase);
                result = (Number)Bignum.MakeInteger(bv);
                return true;
            }
            catch { }
        }

        // Float with exponent markers: d, f, s, l, e
        // Floats are always base 10 per CLHS
        if (HasExponentMarker(upper))
        {
            if (TryParseFloat(upper, out result))
                return true;
        }

        // Float with decimal point (no exponent marker)
        // Floats are always base 10 per CLHS
        if (upper.Contains('.') && !upper.EndsWith('.'))
        {
            if (double.TryParse(token, System.Globalization.NumberStyles.Float,
                    System.Globalization.CultureInfo.InvariantCulture, out var dv))
            {
                // No exponent marker: use *read-default-float-format*
                result = IsDefaultFloatDouble() ? new DoubleFloat(dv) : new SingleFloat((float)dv);
                return true;
            }
        }

        return false;
    }

    private static bool IsAllDigits(string s)
    {
        return IsAllDigitsInBase(s, 10);
    }

    private static bool IsAllDigitsInBase(string s, int radix)
    {
        if (s.Length == 0) return false;
        int start = (s[0] == '+' || s[0] == '-') ? 1 : 0;
        if (start >= s.Length) return false;
        for (int i = start; i < s.Length; i++)
        {
            int dv = DigitValue(s[i]);
            if (dv < 0 || dv >= radix) return false;
        }
        return true;
    }

    private static int GetReadBase()
    {
        var readBaseSym = Startup.Sym("*READ-BASE*");
        LispObject val;
        if (DynamicBindings.TryGet(readBaseSym, out val))
        {
            if (val is Fixnum f) return (int)f.Value;
        }
        else if (readBaseSym.IsBound && readBaseSym.Value is Fixnum f2)
        {
            return (int)f2.Value;
        }
        return 10;
    }

    private static bool HasExponentMarker(string token)
    {
        for (int i = 1; i < token.Length; i++)
        {
            char c = token[i];
            if (c == 'D' || c == 'F' || c == 'S' || c == 'L' || c == 'E')
            {
                // Must be preceded by a digit or dot
                char prev = token[i - 1];
                if (char.IsDigit(prev) || prev == '.')
                    return true;
            }
        }
        return false;
    }

    private bool TryParseFloat(string token, out Number result)
    {
        result = null!;

        // Find exponent marker and replace with E for parsing
        // CLHS 2.3.1.1: exponent markers determine float type:
        //   S/F → single-float, D/L → double-float, E → *read-default-float-format*
        //   No marker → *read-default-float-format*
        bool hasExplicitMarker = false;
        bool isDouble = false;
        var normalized = new StringBuilder(token.Length);

        for (int i = 0; i < token.Length; i++)
        {
            char c = token[i];
            if ((c == 'D' || c == 'L') && i > 0 && (char.IsDigit(token[i - 1]) || token[i - 1] == '.'))
            {
                hasExplicitMarker = true;
                isDouble = true;
                normalized.Append('E');
            }
            else if ((c == 'F' || c == 'S') && i > 0 && (char.IsDigit(token[i - 1]) || token[i - 1] == '.'))
            {
                hasExplicitMarker = true;
                isDouble = false;
                normalized.Append('E');
            }
            else if (c == 'E' && i > 0 && (char.IsDigit(token[i - 1]) || token[i - 1] == '.'))
            {
                hasExplicitMarker = true;
                // E uses *read-default-float-format*
                isDouble = IsDefaultFloatDouble();
                normalized.Append('E');
            }
            else
            {
                normalized.Append(c);
            }
        }

        // No exponent marker: use *read-default-float-format*
        if (!hasExplicitMarker)
            isDouble = IsDefaultFloatDouble();

        if (double.TryParse(normalized.ToString(), System.Globalization.NumberStyles.Float,
                System.Globalization.CultureInfo.InvariantCulture, out var dv))
        {
            result = isDouble ? new DoubleFloat(dv) : new SingleFloat((float)dv);
            return true;
        }

        return false;
    }

    /// <summary>Check if *read-default-float-format* is double-float or long-float.</summary>
    /// <summary>
    /// Check if a character has the "invalid" constituent trait per CLHS 2.1.4.2 Figure 2-8.
    /// These characters signal reader-error when encountered as constituents.
    /// </summary>
    private static bool HasInvalidConstituentTrait(char ch)
    {
        return ch == '\b'   // Backspace
            || ch == '\t'   // Tab
            || ch == '\n'   // Newline/Linefeed
            || ch == '\f'   // Page
            || ch == '\r'   // Return
            || ch == ' '    // Space
            || ch == '\x7F'; // Rubout/DEL
    }

    private static bool IsDefaultFloatDouble()
    {
        var sym = Startup.Sym("*READ-DEFAULT-FLOAT-FORMAT*");
        var val = DynamicBindings.Get(sym);
        if (val is Symbol s)
        {
            var name = s.Name;
            return name == "DOUBLE-FLOAT" || name == "LONG-FLOAT";
        }
        return false; // default: single-float
    }

    /// <summary>
    /// Interpret a token as a symbol per CLHS 2.3.5.
    /// Only unescaped colons are package markers.
    /// </summary>
    private LispObject ParseSymbol(Token token)
    {
        // Find first unescaped colon
        int colonPos = token.FindUnescapedColon();

        // Keyword :NAME — unescaped colon at position 0
        // ::NAME (double-colon at start) is also treated as keyword :NAME per most impls
        if (colonPos == 0)
        {
            int nameStart = 1;
            if (nameStart < token.Length && token.Chars[nameStart].Ch == ':' && !token.Chars[nameStart].Escaped)
                nameStart = 2; // skip second colon in ::NAME
            var name = token.Substring(nameStart);
            return Startup.Keyword(name);
        }

        // Package-qualified: PKG:NAME or PKG::NAME
        if (colonPos > 0)
        {
            var pkgName = token.Substring(0, colonPos);

            // Check for double colon (internal)
            bool isInternal = colonPos + 1 < token.Length
                && token.Chars[colonPos + 1].Ch == ':'
                && !token.Chars[colonPos + 1].Escaped;

            var symName = isInternal
                ? token.Substring(colonPos + 2)
                : token.Substring(colonPos + 1);

            // Resolve package: check local nicknames of *package* first (CDR 5)
            var starPkgForNick = DynamicBindings.Get(Startup.Sym("*PACKAGE*"));
            var currentPkgForNick = (starPkgForNick is Package curP) ? curP : null;
            var pkg = currentPkgForNick?.FindLocalNickname(pkgName) ?? Package.FindPackage(pkgName);
            if (pkg == null)
            {
                if (_readSuppress)
                    return new Symbol(symName); // dummy uninterned symbol
                throw MakeReaderError($"Package \"{pkgName}\" not found");
            }

            if (_readSuppress)
            {
                return new Symbol(symName); // don't pollute packages in suppressed mode
            }

            if (isInternal)
            {
                var (sym, _) = pkg.Intern(symName);
                return sym;
            }
            else
            {
                // Single colon: external symbol access only (CLHS 2.3.5)
                var (sym, status) = pkg.FindSymbol(symName);
                if (status == SymbolStatus.External)
                    return sym;
                // Not external — signal error per spec (B7)
                if (_readSuppress) return new Symbol(symName);
                throw MakeReaderError($"Symbol \"{symName}\" is not external in package \"{pkgName}\"");
            }
        }

        // No unescaped colon — unqualified symbol in current package
        var tokenStr = token.ToString();
        var starPkg = DynamicBindings.Get(Startup.Sym("*PACKAGE*"));
        var currentPackage = (starPkg is Package curPkg) ? curPkg : (Package.FindPackage("CL-USER") ?? Startup.CL);
        var (symbol, _) = currentPackage.Intern(tokenStr);

        // Return singleton values for NIL and T
        if (ReferenceEquals(symbol, Startup.NIL_SYM)) return Nil.Instance;
        if (ReferenceEquals(symbol, Startup.T_SYM)) return T.Instance;

        return symbol;
    }

    /// <summary>
    /// Dispatch macro handler: reads numeric argument and sub-character,
    /// then calls the registered dispatch function from the readtable.
    /// Called as the reader macro function for dispatching macro characters like #.
    /// </summary>
    internal LispObject? DispatchMacro(LispReadtable rt, char dispChar)
    {
        int ch = ReadChar();
        if (ch == -1) throw MakeEndOfFileError($"EOF after {dispChar}");

        // Read optional numeric argument
        int numArg = -1;
        if (char.IsDigit((char)ch))
        {
            numArg = ch - '0';
            ch = ReadChar();
            while (ch != -1 && char.IsDigit((char)ch))
            {
                unchecked { numArg = numArg * 10 + (ch - '0'); }
                ch = ReadChar();
            }
            if (ch == -1) throw MakeEndOfFileError($"EOF in {dispChar} dispatch");
        }

        var subChar = char.ToUpperInvariant((char)ch);
        var fn = rt.GetDispatchMacroCharacter(dispChar, subChar);
        if (fn != null)
            return fn(this, (char)ch, numArg);

        // CLHS 2.4.8: whitespace and non-constituent sub-characters are always invalid,
        // even in suppress mode (CLHS 2.4.8.20)
        var st = rt.GetSyntaxType((char)ch);
        if (_readSuppress && st != SyntaxType.Whitespace && st != SyntaxType.TerminatingMacro)
        {
            // In suppress mode, undefined but valid sub-characters: consume next token/object and return NIL
            Read();
            return Nil.Instance;
        }
        throw MakeReaderError($"Unknown {dispChar} dispatch character: {(char)ch}");
    }

    /// <summary>Reader macro function for ; — line comment (CLHS 2.4.4).</summary>
    internal LispObject? ReadLineComment(char _triggerChar)
    {
        while (true)
        {
            int ch = ReadChar();
            if (ch == -1 || ch == '\n' || ch == '\r') break;
        }
        return null; // no value produced
    }

    /// <summary>Reader macro function for ) — error (CLHS 2.4.2).</summary>
    internal LispObject? ReadRightParen(char _triggerChar)
    {
        throw MakeReaderError("Unexpected ')'");
    }

    /// <summary>
    /// Register all standard CL reader macro functions on a readtable.
    /// Must be called after Reader class is available.
    /// </summary>
    public static void RegisterStandardMacros(LispReadtable rt)
    {
        // Terminating macro characters
        rt.SetMacroCharacter('(', (reader, ch) => reader.ReadList(ch), false);
        rt.SetMacroCharacter(')', (reader, ch) => reader.ReadRightParen(ch), false);
        rt.SetMacroCharacter('\'', (reader, ch) => reader.ReadQuote(ch), false);
        rt.SetMacroCharacter('"', (reader, ch) => reader.ReadString(ch), false);
        rt.SetMacroCharacter(';', (reader, ch) => reader.ReadLineComment(ch), false);
        rt.SetMacroCharacter('`', (reader, ch) => reader.ReadBackquote(ch), false);
        rt.SetMacroCharacter(',', (reader, ch) => reader.ReadComma(ch), false);

        // Non-terminating dispatching macro character: #
        // MakeDispatchMacroCharacter sets up the dispatch table and registers
        // DispatchMacro as the reader macro function.
        rt.MakeDispatchMacroCharacter('#', true);

        // Register standard # dispatch sub-characters (CLHS 2.4.8)
        rt.SetDispatchMacroCharacter('#', '\\', (r, c, n) => r.ReadCharacterLiteral());
        rt.SetDispatchMacroCharacter('#', '\'', (r, c, n) => r.ReadFunctionShorthand());
        rt.SetDispatchMacroCharacter('#', '(', (r, c, n) => r.ReadVector(n));
        rt.SetDispatchMacroCharacter('#', '*', (r, c, n) => r.ReadBitVector(n));
        rt.SetDispatchMacroCharacter('#', ':', (r, c, n) => r.ReadUninterned());
        rt.SetDispatchMacroCharacter('#', '=', (r, c, n) => r.ReadShareLabel(n));
        rt.SetDispatchMacroCharacter('#', '#', (r, c, n) => r.ReadShareRef(n));
        rt.SetDispatchMacroCharacter('#', '.', (r, c, n) => r.ReadReadTimeEval());
        rt.SetDispatchMacroCharacter('#', '+', (r, c, n) => r.ReadFeature(true));
        rt.SetDispatchMacroCharacter('#', '-', (r, c, n) => r.ReadFeature(false));
        rt.SetDispatchMacroCharacter('#', '|', (r, c, n) => r.ReadBlockComment());
        rt.SetDispatchMacroCharacter('#', 'A', (r, c, n) => r.ReadArrayLiteral(n >= 0 ? n : 1));
        rt.SetDispatchMacroCharacter('#', 'B', (r, c, n) => r.ReadRadixNumber(2));
        rt.SetDispatchMacroCharacter('#', 'C', (r, c, n) => r.ReadComplex());
        rt.SetDispatchMacroCharacter('#', 'O', (r, c, n) => r.ReadRadixNumber(8));
        rt.SetDispatchMacroCharacter('#', 'P', (r, c, n) => r.ReadPathnameShorthand());
        rt.SetDispatchMacroCharacter('#', 'R', (r, c, n) => r.ReadRadixNumber(n));
        rt.SetDispatchMacroCharacter('#', 'S', (r, c, n) => r.ReadStructureLiteral());
        rt.SetDispatchMacroCharacter('#', 'X', (r, c, n) => r.ReadRadixNumber(16));
        // CLHS: #) and #< signal error
        rt.SetDispatchMacroCharacter('#', ')', (r, c, n) =>
            throw r.MakeReaderError("Invalid # dispatch: #)"));
        rt.SetDispatchMacroCharacter('#', '<', (r, c, n) =>
            throw r.MakeReaderError("Invalid # dispatch: #<"));
    }

    private static LispObject MakeList(params LispObject[] elements)
    {
        LispObject result = Nil.Instance;
        for (int i = elements.Length - 1; i >= 0; i--)
            result = new Cons(elements[i], result);
        return result;
    }
}

/// <summary>Placeholder for forward #n# references in shared structure reading.</summary>
public class SharePlaceholder : LispObject
{
    public int Label { get; }
    public LispObject? Value { get; set; }
    public SharePlaceholder(int label) => Label = label;
    public override string ToString() => Value?.ToString() ?? $"#<SHARE-REF {Label}>";
}
