using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace DotCL;

/// <summary>
/// API-compat helpers for the netstandard2.0 (emit-free) target. The extension
/// methods here are compiled on every target framework but are only *used* on
/// ns2.0: on net5.0+ the framework provides matching instance methods, which the
/// C# overload resolver prefers over extensions, so behaviour is identical.
/// </summary>
internal static class Compat
{
    /// <summary>Equivalent of <c>array[start..]</c> without RuntimeHelpers.GetSubArray
    /// (absent on ns2.0). Used directly via call-site rewrite so it runs on all TFMs.</summary>
    public static T[] SubArray<T>(this T[] array, int start)
    {
        int len = array.Length - start;
        if (len <= 0) return Array.Empty<T>();
        var result = new T[len];
        Array.Copy(array, start, result, 0, len);
        return result;
    }

    // --- BigInteger (GetBitLength / GetByteCount are .NET Core 2.1+ / net5.0+) ---

    /// <summary>Bit length excluding the sign bit, matching BigInteger.GetBitLength().</summary>
    public static long GetBitLength(this BigInteger value)
    {
        if (value.Sign == 0) return 0;
        // For negatives .NET measures the magnitude of (~value) == (-value - 1).
        BigInteger x = value.Sign < 0 ? -value - 1 : value;
        long bits = 0;
        while (x > 0) { x >>= 1; bits++; }
        return bits;
    }

    /// <summary>Upper-bound byte count, matching BigInteger.GetByteCount(isUnsigned).
    /// ns2.0 ToByteArray() is two's-complement little-endian; its length is a safe bound.</summary>
    public static int GetByteCount(this BigInteger value, bool isUnsigned = false)
        => value.ToByteArray().Length;

    // --- Random.NextInt64 (net6.0+) ---

    /// <summary>Non-negative 64-bit random, matching Random.NextInt64().</summary>
    public static long NextInt64(this Random random)
    {
        var buf = new byte[8];
        random.NextBytes(buf);
        return BitConverter.ToInt64(buf, 0) & long.MaxValue;
    }

    // --- Stack<T>.TryPop (net5.0+) ---

    public static bool TryPop<T>(this Stack<T> stack, out T value)
    {
        if (stack.Count == 0) { value = default!; return false; }
        value = stack.Pop();
        return true;
    }

    // --- IDictionary.TryAdd (net5.0+) ---

    public static bool TryAdd<TKey, TValue>(this Dictionary<TKey, TValue> dict, TKey key, TValue value)
    {
        if (dict.ContainsKey(key)) return false;
        dict.Add(key, value);
        return true;
    }

    /// <summary>BigInteger.ToByteArray(isUnsigned, isBigEndian) — net5.0+ instance overload.
    /// On net5.0+ the instance method wins; this extension only binds on ns2.0.</summary>
    public static byte[] ToByteArray(this BigInteger value, bool isUnsigned, bool isBigEndian = false)
    {
        byte[] bytes = value.ToByteArray(); // two's-complement little-endian
        if (isUnsigned && value.Sign >= 0)
        {
            // Trim redundant 0x00 sign bytes the unsigned form omits.
            int len = bytes.Length;
            while (len > 1 && bytes[len - 1] == 0) len--;
            if (len != bytes.Length) Array.Resize(ref bytes, len);
        }
        if (isBigEndian) Array.Reverse(bytes);
        return bytes;
    }

    // --- Static-method routers. Defined for all TFMs; each forwards to the BCL on
    //     net5.0+ and implements the API locally on netstandard2.0. Call sites use
    //     Compat.X unconditionally, keeping behaviour identical across targets. ---

    public static double ScaleB(double x, int n)
    {
#if NETSTANDARD2_0
        // Port of the CoreLib (fdlibm-derived) scalbn.
        const double c1 = 8.98846567431158E+307;  // 2^1023
        const double c2 = 2.2250738585072014E-308; // 2^-1022
        const double c3 = 9007199254740992.0;       // 2^53
        double y = x;
        if (n > 1023)
        {
            y *= c1; n -= 1023;
            if (n > 1023) { y *= c1; n -= 1023; if (n > 1023) n = 1023; }
        }
        else if (n < -1022)
        {
            y *= c2 * c3; n += 1022 - 53;
            if (n < -1022) { y *= c2 * c3; n += 1022 - 53; if (n < -1022) n = -1022; }
        }
        double u = BitConverter.Int64BitsToDouble((long)(0x3ff + n) << 52);
        return y * u;
#else
        return Math.ScaleB(x, n);
#endif
    }

    public static double BitIncrement(double x)
    {
#if NETSTANDARD2_0
        long bits = BitConverter.DoubleToInt64Bits(x);
        if (((bits >> 32) & 0x7FF00000) >= 0x7FF00000)
            return bits == unchecked((long)0xFFF0000000000000) ? double.MinValue : x;
        if (bits == unchecked((long)0x8000000000000000)) return double.Epsilon;
        bits += bits < 0 ? -1 : 1;
        return BitConverter.Int64BitsToDouble(bits);
#else
        return Math.BitIncrement(x);
#endif
    }

    public static double Log2(double x)
#if NETSTANDARD2_0
        => Math.Log(x, 2.0);
#else
        => Math.Log2(x);
#endif

    public static double Acosh(double x)
#if NETSTANDARD2_0
        => Math.Log(x + Math.Sqrt(x * x - 1.0));
#else
        => Math.Acosh(x);
#endif

    public static double Asinh(double x)
#if NETSTANDARD2_0
        => Math.Log(x + Math.Sqrt(x * x + 1.0));
#else
        => Math.Asinh(x);
#endif

    public static double Atanh(double x)
#if NETSTANDARD2_0
        => 0.5 * Math.Log((1.0 + x) / (1.0 - x));
#else
        => Math.Atanh(x);
#endif

    public static long Clamp(long value, long min, long max)
        => value < min ? min : (value > max ? max : value);

    public static double Clamp(double value, double min, double max)
        => value < min ? min : (value > max ? max : value);

    public static long BigMul(long a, long b, out long low)
    {
#if NETSTANDARD2_0
        ulong al = (ulong)a, bl = (ulong)b;
        const ulong mask = 0xFFFFFFFFUL;
        ulong a0 = al & mask, a1 = al >> 32, b0 = bl & mask, b1 = bl >> 32;
        ulong p00 = a0 * b0, p01 = a0 * b1, p10 = a1 * b0, p11 = a1 * b1;
        ulong mid = (p00 >> 32) + (p01 & mask) + (p10 & mask);
        ulong lo = (p00 & mask) | (mid << 32);
        ulong hi = p11 + (p01 >> 32) + (p10 >> 32) + (mid >> 32);
        if (a < 0) hi -= bl;
        if (b < 0) hi -= al;
        low = (long)lo;
        return (long)hi;
#else
        return Math.BigMul(a, b, out low);
#endif
    }

    public static void Fill<T>(T[] array, T value)
    {
#if NETSTANDARD2_0
        for (int i = 0; i < array.Length; i++) array[i] = value;
#else
        Array.Fill(array, value);
#endif
    }

    public static void Fill<T>(T[] array, T value, int startIndex, int count)
    {
#if NETSTANDARD2_0
        for (int i = startIndex; i < startIndex + count; i++) array[i] = value;
#else
        Array.Fill(array, value, startIndex, count);
#endif
    }

    public static int SingleToInt32Bits(float value)
#if NETSTANDARD2_0
        => BitConverter.ToInt32(BitConverter.GetBytes(value), 0);
#else
        => BitConverter.SingleToInt32Bits(value);
#endif

    public static float Int32BitsToSingle(int value)
#if NETSTANDARD2_0
        => BitConverter.ToSingle(BitConverter.GetBytes(value), 0);
#else
        => BitConverter.Int32BitsToSingle(value);
#endif

    public static bool IsNegative(double value)
        => BitConverter.DoubleToInt64Bits(value) < 0;

    public static bool IsAsciiDigit(char c) => c >= '0' && c <= '9';

    public static bool IsWindows()
#if NETSTANDARD2_0
        => RuntimeInformation.IsOSPlatform(OSPlatform.Windows);
#else
        => OperatingSystem.IsWindows();
#endif

    public static bool IsLinux()
#if NETSTANDARD2_0
        => RuntimeInformation.IsOSPlatform(OSPlatform.Linux);
#else
        => OperatingSystem.IsLinux();
#endif

    public static bool IsMacOS()
#if NETSTANDARD2_0
        => RuntimeInformation.IsOSPlatform(OSPlatform.OSX);
#else
        => OperatingSystem.IsMacOS();
#endif

    public static long TickCount64()
#if NETSTANDARD2_0
        => Environment.TickCount & 0xFFFFFFFFL;
#else
        => Environment.TickCount64;
#endif

    public static long GetTotalAllocatedBytes(bool precise = false)
#if NETSTANDARD2_0
        => GC.GetTotalMemory(false);
#else
        => GC.GetTotalAllocatedBytes(precise);
#endif

    public static bool TryEnsureSufficientExecutionStack()
#if NETSTANDARD2_0
        => true; // ns2.0 lacks the probe; best-effort no-op
#else
        => RuntimeHelpers.TryEnsureSufficientExecutionStack();
#endif

    /// <summary>GCCollectionMode.Aggressive is net5.0+; fall back to Forced on ns2.0.</summary>
    public static readonly GCCollectionMode AggressiveGCMode =
#if NETSTANDARD2_0
        GCCollectionMode.Forced;
#else
        GCCollectionMode.Aggressive;
#endif

    // --- char/Span string overloads (net core 2.1+/net5.0+ instance methods).
    //     Instance methods win on net5.0+; these only bind on ns2.0. ---

    public static bool StartsWith(this string s, char value)
        => s.Length > 0 && s[0] == value;

    public static bool EndsWith(this string s, char value)
        => s.Length > 0 && s[s.Length - 1] == value;

    public static bool Contains(this string s, char value)
        => s.IndexOf(value) >= 0;

    public static string[] Split(this string s, char separator, StringSplitOptions options)
        => s.Split(new[] { separator }, options);

    public static int GetHashCode(this string s, StringComparison comparisonType)
    {
        switch (comparisonType)
        {
            case StringComparison.OrdinalIgnoreCase: return StringComparer.OrdinalIgnoreCase.GetHashCode(s);
            case StringComparison.CurrentCulture: return StringComparer.CurrentCulture.GetHashCode(s);
            case StringComparison.CurrentCultureIgnoreCase: return StringComparer.CurrentCultureIgnoreCase.GetHashCode(s);
            case StringComparison.InvariantCulture: return StringComparer.InvariantCulture.GetHashCode(s);
            case StringComparison.InvariantCultureIgnoreCase: return StringComparer.InvariantCultureIgnoreCase.GetHashCode(s);
            default: return StringComparer.Ordinal.GetHashCode(s);
        }
    }

    // --- KeyValuePair deconstruction (net core 2.0+). Enables `foreach (var (k,v) in dict)`. ---

    public static void Deconstruct<TKey, TValue>(this KeyValuePair<TKey, TValue> kvp, out TKey key, out TValue value)
    {
        key = kvp.Key;
        value = kvp.Value;
    }

    // --- ProcessStartInfo.ArgumentList (net core 2.1+). On ns2.0 fall back to a
    //     quoted Arguments string. ---

    public static void AddArg(ProcessStartInfo psi, string arg)
    {
#if NETSTANDARD2_0
        if (psi.Arguments.Length > 0) psi.Arguments += " ";
        psi.Arguments += QuoteArg(arg);
#else
        psi.ArgumentList.Add(arg);
#endif
    }

#if NETSTANDARD2_0
    private static string QuoteArg(string arg)
    {
        if (arg.Length > 0 && arg.IndexOfAny(new[] { ' ', '\t', '"' }) < 0)
            return arg;
        // Minimal Windows-style quoting: wrap in quotes, escape embedded quotes/backslashes.
        var sb = new System.Text.StringBuilder();
        sb.Append('"');
        foreach (char c in arg)
        {
            if (c == '"') sb.Append('\\');
            sb.Append(c);
        }
        sb.Append('"');
        return sb.ToString();
    }
#endif

    /// <summary>BigInteger(byte[], isUnsigned, isBigEndian) ctor (net5.0+).</summary>
    public static BigInteger MakeBigInteger(byte[] value, bool isUnsigned = false, bool isBigEndian = false)
    {
#if NETSTANDARD2_0
        byte[] bytes = (byte[])value.Clone();
        if (isBigEndian) Array.Reverse(bytes); // normalize to little-endian
        if (isUnsigned && bytes.Length > 0 && (bytes[bytes.Length - 1] & 0x80) != 0)
        {
            Array.Resize(ref bytes, bytes.Length + 1);
            bytes[bytes.Length - 1] = 0; // append zero sign byte to force non-negative
        }
        return new BigInteger(bytes);
#else
        return new BigInteger(value, isUnsigned, isBigEndian);
#endif
    }
}
