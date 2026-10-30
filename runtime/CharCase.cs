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
