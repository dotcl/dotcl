// bench/csharp-parity/Program.cs -- C# half of the C# parity benchmark.
//
// Each method here computes exactly what the correspondingly named function in
// kernels.lisp computes, over the same input sizes, so the two programs'
// outputs divide into a time ratio per kernel. Output format is shared too:
// one "name<TAB>milliseconds" line per kernel on stdout, minimum of 5 timed
// runs after 1 warmup run.
//
// Build and run:
//   dotnet build -c Release bench/csharp-parity/Parity.csproj
//   bench/csharp-parity/bin/Release/net10.0/parity
//
// Release only. A Debug build measures a different implementation: the JIT
// declines several optimisations in a debuggable assembly, which is the same
// reason the Lisp side insists on the Release runtime.

using System;
using System.Diagnostics;

namespace Parity
{
    public sealed class KPoint
    {
        public long A;
        public long B;

        public KPoint(long a, long b)
        {
            A = a;
            B = b;
        }
    }

    public static class Program
    {
        // --- Kernels ---------------------------------------------------
        //
        // NoInlining on the kernel entry points only. Without it the JIT can
        // hoist a whole call out of the timing loop once it proves the result
        // is unused, and the benchmark then measures nothing at all. The
        // recursive self-calls inside tak and fib are left inlinable, because
        // that is what a normal build of this code would get.

        [System.Runtime.CompilerServices.MethodImpl(
            System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
        public static long Tak(long x, long y, long z)
        {
            if (!(y < x))
            {
                return z;
            }
            return Tak(Tak(x - 1, y, z),
                       Tak(y - 1, z, x),
                       Tak(z - 1, x, y));
        }

        [System.Runtime.CompilerServices.MethodImpl(
            System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
        public static long Fib(long n)
        {
            if (n < 2)
            {
                return n;
            }
            return Fib(n - 1) + Fib(n - 2);
        }

        [System.Runtime.CompilerServices.MethodImpl(
            System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
        public static long FixnumLoop(long n)
        {
            long acc = 0;
            for (long i = 0; i < n; i++)
            {
                acc += i & 255;
            }
            return acc;
        }

        [System.Runtime.CompilerServices.MethodImpl(
            System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
        public static double DoubleLoop(long n)
        {
            double x = 1.0;
            double acc = 0.0;
            for (long i = 0; i < n; i++)
            {
                x = x * 1.0000001;
                acc = acc + 1.0 / x;
            }
            return acc;
        }

        [System.Runtime.CompilerServices.MethodImpl(
            System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
        public static long ArrayWalk(long[] arr, long passes)
        {
            long acc = 0;
            long n = arr.Length;
            for (long p = 0; p < passes; p++)
            {
                for (long i = 0; i < n; i++)
                {
                    acc += arr[i];
                }
            }
            return acc;
        }

        [System.Runtime.CompilerServices.MethodImpl(
            System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
        public static long StructLoop(KPoint p, long n)
        {
            long acc = 0;
            for (long i = 0; i < n; i++)
            {
                acc += p.A + p.B;
                p.A = i;
            }
            return acc;
        }

        [System.Runtime.CompilerServices.MethodImpl(
            System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
        public static long StringWalk(string s, long passes)
        {
            long acc = 0;
            long n = s.Length;
            for (long p = 0; p < passes; p++)
            {
                for (long i = 0; i < n; i++)
                {
                    acc += (int)s[(int)i];
                }
            }
            return acc;
        }


        // The char[] counterpart of StringWalk. On the dotcl side the two
        // string spellings are two different objects reached by two different
        // routes; on this side they are a string and a char[], which is the
        // closest thing C# has to the same distinction. Same body as
        // StringWalk on purpose: the argument is what differs.
        [System.Runtime.CompilerServices.MethodImpl(
            System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
        public static long StringWalkChars(char[] s, long passes)
        {
            long acc = 0;
            long n = s.Length;
            for (long p = 0; p < passes; p++)
            {
                for (long i = 0; i < n; i++)
                {
                    acc += (int)s[(int)i];
                }
            }
            return acc;
        }

        // --- Harness ---------------------------------------------------

        private const int Runs = 5;

        // Every kernel's result is folded in here and printed to stderr at the
        // end. A result that reaches an observable side effect cannot be
        // optimised away, and stderr keeps it off the machine-readable stream.
        private static double _sink;

        // Each kernel carries a repeat count and the reported figure is the
        // time for the whole repeated block, matching kernels.lisp. The counts
        // put every block above roughly 30 ms, where the measurement stops
        // being dominated by tiering and scheduling. Both sides use the same
        // counts, so the ratio is unaffected; only the absolute figures scale.
        private static double Measure(int repeats, Func<double> thunk)
        {
            var sw = Stopwatch.StartNew();
            for (int i = 0; i < repeats; i++)
            {
                _sink += thunk();
            }
            sw.Stop();
            return sw.Elapsed.TotalMilliseconds;
        }

        private static void Report(string name, int repeats, Func<double> thunk)
        {
            Measure(repeats, thunk);
            double best = double.MaxValue;
            for (int i = 0; i < Runs; i++)
            {
                double ms = Measure(repeats, thunk);
                if (ms < best)
                {
                    best = ms;
                }
            }
            Console.Out.WriteLine("{0}\t{1:F3}", name, best);
            Console.Out.Flush();
        }

        // --- Inputs ----------------------------------------------------

        private const int ArraySize = 1024;
        private const long ArrayPasses = 100000;
        private const int StringSize = 1048576;
        private const long StringPasses = 100;

        public static void Main()
        {
            var arr = new long[ArraySize];
            for (int i = 0; i < ArraySize; i++)
            {
                arr[i] = i % 256;
            }

            var chars = new char[StringSize];
            for (int i = 0; i < StringSize; i++)
            {
                chars[i] = (char)(32 + (i % 64));
            }
            string s = new string(chars);

            var point = new KPoint(1, 2);

            Report("tak", 40, () => Tak(24, 16, 8));
            Report("fib", 5, () => Fib(32));
            Report("fixnum-loop", 1, () => FixnumLoop(100000000L));
            Report("double-loop", 1, () => DoubleLoop(50000000L));
            Report("array-walk", 1, () => ArrayWalk(arr, ArrayPasses));
            Report("struct-slots", 5, () => StructLoop(point, 10000000L));
            Report("string-walk", 1, () => StringWalk(s, StringPasses));
            Report("string-walk-vector", 1, () => StringWalkChars(chars, StringPasses));

            Console.Error.WriteLine("sink: {0}", _sink);
        }
    }
}
