namespace DotCL;

public class LispString : LispObject
{
    // Copy-on-write backing: at most one of (_str, _chars) is the active source.
    // - LispString(string) starts with _str set and _chars null (no ToCharArray copy).
    // - First mutating access (index set, ToUpperInPlace, RawChars) materializes _chars.
    // This is the dominant LispString allocation pattern (format/printer output) where
    // the result is consumed read-only — making the ToCharArray copy pure waste pre-D663.
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

    // Bulk access for Array.Fill / Array.Copy optimizations — forces materialization
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

public class LispChar : LispObject
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

    public static LispChar Make(char value) =>
        value < 128 ? AsciiCache[value] : new LispChar(value);

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
