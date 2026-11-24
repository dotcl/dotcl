namespace IlParity;

/// <summary>Bit tests and sets, an integer division with both results, and a
/// closure over one value: the shapes the Lisp compiler has special paths for
/// (a bit vector element write, LOGBITP, TRUNCATE read with two values, a
/// closure made at run time). The C# side is the plain code for each.</summary>
public static class Bits
{
    public static void Set(ulong[] words, long i) => words[i >> 6] |= 1UL << (int)(i & 63);

    public static long Test(ulong[] words, long i) => (long)((words[i >> 6] >> (int)(i & 63)) & 1UL);

    public static bool LogBit(long x, long k) => ((x >> (int)k) & 1L) != 0;

    public static long DivMod(long a, long b)
    {
        long q = a / b;
        long r = a % b;
        return q * 31 + r;
    }

    public static System.Func<long, long> Adder(long n) => x => x + n;

    public static long SelfCheck(long n)
    {
        var words = new ulong[(n + 63) / 64];
        for (long i = 0; i < n; i++)
            if (i % 3 == 0) Set(words, i);
        long acc = 0;
        for (long i = 0; i < n; i++)
        {
            acc += Test(words, i);
            acc += LogBit(i * 7919, i % 40) ? 3 : 0;
            acc += DivMod(i * 13 + 5, i % 17 + 1);
            acc += Adder(i)(i + 1);
        }
        return acc;
    }
}
