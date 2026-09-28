namespace DotCL;

public sealed class LispString : LispObject
{
    // Copy-on-write backing: at most one of (_str, _chars) is the active source.
    // - LispString(string) starts with _str set and _chars null (no ToCharArray copy).
    // - First mutating access (index set, ToUpperInPlace, RawChars) materializes _chars.
    // This is the dominant LispString allocation pattern (format/printer output) where
    // the result is consumed read-only: making the ToCharArray copy pure waste.
    private string? _str;
    private char[]? _chars;

    public LispString(string value)
    {
        _str = value;
        DotCL.Diagnostics.AllocCounter.Inc("LispString");
    }
    public LispString(char[] chars)
    {
        _chars = chars;
        DotCL.Diagnostics.AllocCounter.Inc("LispString");
    }

    public int Length => _chars?.Length ?? _str!.Length;

    public char this[int index]
    {
        get => _chars is { } c ? c[index] : _str![index];
        set
        {
            EnsureMutable();
            _chars![index] = value;
        }
    }

    public string Value => _str ?? new string(_chars!);

    // Read-only bulk access that does NOT materialize. RAWCHARS is the write
    // accessor: it turns a string-backed LispString into a char[]-backed one
    // permanently, and from then on VALUE has to build a fresh System.String on
    // every read. A read-only scan that reaches for RAWCHARS therefore makes
    // every later STRING= / STRING< / STRING-TRIM on that object allocate --
    // (SEARCH "wor" s) did exactly that, and the cost stayed with S for the rest
    // of the image's life. Bulk readers use this instead.
    internal ReadOnlySpan<char> Chars => _chars is { } c ? c : _str.AsSpan();

    // The two backings, exposed for reading only. A per-element read through
    // LENGTH and then the indexer asks which backing is live twice, and the
    // JIT keeps both questions in the loop: on a 1 MB scan that second test,
    // and the bounds check that goes with it, cost more than the character
    // load. A caller that selects the backing once can read its length and its
    // element off the same object. Neither is a licence to write: a write has
    // to go through the indexer or RAWCHARS so that the copy-on-write
    // invariant (at most one backing live) is maintained.
    internal char[]? CharsOrNull => _chars;
    internal string? StrOrNull => _str;

    // Bulk access for Array.Fill / Array.Copy optimizations: forces materialization
    internal char[] RawChars
    {
        get
        {
            EnsureMutable();
            return _chars!;
        }
    }

    private void EnsureMutable()
    {
        if (_chars == null)
        {
            _chars = _str!.ToCharArray();
            _str = null;
        }
    }

    // In-place mutation methods for NSTRING-* functions
    public void ToUpperInPlace(int start, int end)
    {
        EnsureMutable();
        for (int i = start; i < end; i++)
            _chars![i] = char.ToUpperInvariant(_chars[i]);
    }

    public void ToLowerInPlace(int start, int end)
    {
        EnsureMutable();
        for (int i = start; i < end; i++)
            _chars![i] = char.ToLowerInvariant(_chars[i]);
    }

    public void ToCapitalizeInPlace(int start, int end)
    {
        EnsureMutable();
        // CL capitalize: word boundary starts true; non-alphanumeric sets it true;
        // digits set it false; alphabetic chars: upcase if boundary, else downcase.
        bool wordBoundary = true;
        for (int i = start; i < end; i++)
        {
            char c = _chars![i];
            if (char.IsLetter(c))
            {
                _chars[i] = wordBoundary ? char.ToUpperInvariant(c) : char.ToLowerInvariant(c);
                wordBoundary = false;
            }
            else if (char.IsDigit(c))
            {
                wordBoundary = false;
            }
            else
            {
                wordBoundary = true;
            }
        }
    }

    public override string ToString() => $"\"{EscapeString(Value)}\"";

    private static string EscapeString(string s)
    {
        var sb = new System.Text.StringBuilder(s.Length);
        foreach (var c in s)
        {
            switch (c)
            {
                case '"': sb.Append("\\\""); break;
                case '\\': sb.Append("\\\\"); break;
                default: sb.Append(c); break;
            }
        }
        return sb.ToString();
    }

    public override bool Equals(object? obj) =>
        obj is LispString other && Value == other.Value;

    public override int GetHashCode() => Value.GetHashCode();
}

public sealed class LispChar : LispObject
{
    public char Value { get; }

    private static readonly LispChar[] AsciiCache = new LispChar[128];

    static LispChar()
    {
        for (int i = 0; i < 128; i++)
            AsciiCache[i] = new LispChar((char)i);
    }

    private LispChar(char value)
    {
        Value = value;
        DotCL.Diagnostics.AllocCounter.Inc("LispChar");
    }

    // Every character is one object, not just ASCII: EQ on characters is
    // implementation-dependent in the standard, but every mainstream
    // implementation makes it true for the same character, and libraries rely on
    // it (cl-ppcre's charset compares stored characters with EQ, and missed
    // every non-ASCII member when (code-char 200) was a fresh object each time).
    // Two-level and filled lazily, so a program that never leaves ASCII pays
    // nothing; CompareExchange so two threads cannot publish different objects
    // for one character.
    private static readonly LispChar?[]?[] Pages = new LispChar?[]?[256];

    public static LispChar Make(char value)
    {
        if (value < 128) return AsciiCache[value];
        var page = Pages[value >> 8];
        if (page == null)
        {
            System.Threading.Interlocked.CompareExchange(ref Pages[value >> 8], new LispChar?[256], null);
            page = Pages[value >> 8]!;
        }
        var c = page[value & 0xFF];
        if (c != null) return c;
        System.Threading.Interlocked.CompareExchange(ref page[value & 0xFF], new LispChar(value), null);
        return page[value & 0xFF]!;
    }

    public override string ToString()
    {
        // Use Runtime.CharName for named characters to ensure consistency with char-name.
        // Multi-word UCD names (e.g. "SOFT HYPHEN") are now readable: the reader handles
        // #\SOFT HYPHEN by consuming words until it finds a NameChar match.
        var name = Runtime.CharName(Value);
        if (name != null)
            return $"#\\{name}";
        if (Value > ' ' && Value < 127)
            return $"#\\{Value}";
        return $"#\\U+{(int)Value:X4}";
    }

    public override bool Equals(object? obj) =>
        obj is LispChar other && Value == other.Value;

    public override int GetHashCode() => Value.GetHashCode();
}
