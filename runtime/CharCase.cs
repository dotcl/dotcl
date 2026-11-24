using System.Runtime.CompilerServices;

namespace DotCL;

// CLHS case pairs: only characters whose .NET simple mappings are inverses keep case.
internal static class CharCase
{
    private const byte Lower = 1, Upper = 2;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static char Upcase(char c) =>
        c >= 'a' && c <= 'z' ? (char)(c - 32) : c < 128 ? c : (char)Table.Up[c];

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static char Downcase(char c) =>
        c >= 'A' && c <= 'Z' ? (char)(c + 32) : c < 128 ? c : (char)Table.Down[c];

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static char Fold(char c) =>
        c >= 'a' && c <= 'z' ? (char)(c - 32) : c < 128 ? c : (char)Table.Up[c];

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static bool IsUpper(char c) =>
        c < 128 ? c >= 'A' && c <= 'Z' : Table.Kind[c] == Upper;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static bool IsLower(char c) =>
        c < 128 ? c >= 'a' && c <= 'z' : Table.Kind[c] == Lower;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static bool HasCase(char c) =>
        c < 128 ? (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') : Table.Kind[c] != 0;

    // Simple (one char to one char) case mappings used where the reader, the printer and
    // symbol names need the Unicode mapping rather than the CLHS case pairs above. The
    // .NET mapping of a char depends on the host: ICU and the invariant-globalization tables
    // differ in 7 BMP chars, so those are fixed here and every other char is left to .NET.
    // That makes a symbol read, printed and compared the same under ICU, NLS and
    // DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static char SimpleUpper(char c) =>
        c < 128 ? (c >= 'a' && c <= 'z' ? (char)(c - 32) : c)
        : IsHostSensitive(c) ? SimpleUpperSlow(c) : char.ToUpperInvariant(c);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static char SimpleLower(char c) =>
        c < 128 ? (c >= 'A' && c <= 'Z' ? (char)(c + 32) : c)
        : IsHostSensitive(c) ? SimpleLowerSlow(c) : char.ToLowerInvariant(c);

    private static bool IsHostSensitive(char c) =>
        c == '\u017F' || c == '\uA7CE' || c == '\uA7CF' || (c >= '\uA7D2' && c <= '\uA7D5');

    private static char SimpleUpperSlow(char c) => c switch
    {
        '\u017F' => 'S',
        '\uA7CF' => '\uA7CE',
        '\uA7D3' => '\uA7D2',
        '\uA7D5' => '\uA7D4',
        _ => c,
    };

    private static char SimpleLowerSlow(char c) => c switch
    {
        '\uA7CE' => '\uA7CF',
        '\uA7D2' => '\uA7D3',
        '\uA7D4' => '\uA7D5',
        _ => c,
    };

    internal static string SimpleUpper(string s)
    {
        for (int i = 0; i < s.Length; i++)
            if (IsHostSensitive(s[i])) return MapString(s, SimpleUpper);
        return s.ToUpperInvariant();
    }

    internal static string SimpleLower(string s)
    {
        for (int i = 0; i < s.Length; i++)
            if (IsHostSensitive(s[i])) return MapString(s, SimpleLower);
        return s.ToLowerInvariant();
    }

    private static string MapString(string s, System.Func<char, char> f)
    {
        var a = s.ToCharArray();
        for (int i = 0; i < a.Length; i++) a[i] = f(a[i]);
        return new string(a);
    }

    /// <summary>Upper case of an ASCII letter; any other char as it is. For syntax that is
    /// ASCII by definition (format directives), where a host mapping must not turn a
    /// non-ASCII char into a letter.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static char AsciiUpper(char c) => c >= 'a' && c <= 'z' ? (char)(c - 32) : c;

    private static class Table
    {
        internal static readonly ushort[] Up = new ushort[65536];
        internal static readonly ushort[] Down = new ushort[65536];
        internal static readonly byte[] Kind = new byte[65536];

        static Table()
        {
            for (int i = 0; i < 65536; i++)
                Up[i] = Down[i] = (ushort)i;
            for (int i = 128; i < 65536; i++)
            {
                char c = (char)i;
                if (char.IsLower(c))
                {
                    char u = char.ToUpperInvariant(c);
                    if (char.IsUpper(u) && char.ToLowerInvariant(u) == c)
                    {
                        Kind[i] = Lower;
                        Up[i] = u;
                    }
                }
                else if (char.IsUpper(c))
                {
                    char l = char.ToLowerInvariant(c);
                    if (char.IsLower(l) && char.ToUpperInvariant(l) == c)
                    {
                        Kind[i] = Upper;
                        Down[i] = l;
                    }
                }
            }
        }
    }
}
