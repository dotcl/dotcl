namespace IlParity;

/// <summary>A whitespace tokenizer over a flat string.
///
/// The only case whose element type is a character rather than an integer, and
/// the one that asks what a character costs: a scan like this reads one element
/// per iteration and compares it against constants, so a representation that
/// boxes a character pays on every character of the input.
///
/// It returns numbers rather than substrings on purpose -- allocating the
/// tokens would make the case about the allocator instead of about the scan.
/// </summary>
public sealed class Tokenizer
{
    private readonly string _text;
    private int _pos;

    public Tokenizer(string text)
    {
        _text = text;
        _pos = 0;
    }

    private static bool IsSpace(char c) => c == ' ' || c == '\t' || c == '\n';

    /// <summary>The length of the next token, or -1 at end of input.</summary>
    public long NextToken()
    {
        string s = _text;
        int n = s.Length;
        int i = _pos;
        while (i < n && IsSpace(s[i])) i++;
        if (i >= n) { _pos = i; return -1; }
        int start = i;
        while (i < n && !IsSpace(s[i])) i++;
        _pos = i;
        return i - start;
    }

    /// <summary>Token count and the sum of the character codes, in one pass.
    /// The codes are folded in so that a scan that skipped or repeated a
    /// character changes the answer.</summary>
    public long Digest()
    {
        string s = _text;
        int n = s.Length;
        long tokens = 0;
        long sum = 0;
        int i = 0;
        while (i < n)
        {
            while (i < n && IsSpace(s[i])) i++;
            if (i >= n) break;
            tokens++;
            while (i < n && !IsSpace(s[i])) { sum += s[i]; i++; }
        }
        return tokens * 1000003 + sum;
    }

    public static long SelfCheck(long n)
    {
        var sb = new System.Text.StringBuilder();
        for (long i = 0; i < n; i++)
        {
            sb.Append((char)('a' + (int)(i % 26)));
            if (i % 5 == 4) sb.Append(' ');
            if (i % 17 == 16) sb.Append('\t');
        }
        string text = sb.ToString();
        var t = new Tokenizer(text);
        long acc = t.Digest();
        var u = new Tokenizer(text);
        long len;
        long count = 0;
        while ((len = u.NextToken()) >= 0) count += len;
        return acc + count;
    }
}
