// Polyfills for the netstandard2.0 (emit-free) target. These types exist in the
// BCL on net5.0+, so they are compiled only when targeting ns2.0 to avoid clashing
// with the framework-provided definitions.
#if NETSTANDARD2_0
namespace System.Runtime.CompilerServices
{
    /// <summary>Enables C# 9 `init`-only setters and records on netstandard2.0.</summary>
    internal static class IsExternalInit { }
}

namespace System
{
    // Canonical System.Index / System.Range polyfills (matching the .NET runtime
    // reference source). Required for C# range/index syntax (s[1..], s[^1]) which
    // the compiler lowers to constructions of these types. String slicing then uses
    // Substring; no RuntimeHelpers.GetSubArray is needed unless arrays are sliced.
    internal readonly struct Index : IEquatable<Index>
    {
        private readonly int _value;

        public Index(int value, bool fromEnd = false)
        {
            if (value < 0) throw new ArgumentOutOfRangeException(nameof(value), "value must be non-negative");
            _value = fromEnd ? ~value : value;
        }

        private Index(int value) => _value = value;

        public static Index Start => new Index(0);
        public static Index End => new Index(~0);

        public static Index FromStart(int value)
        {
            if (value < 0) throw new ArgumentOutOfRangeException(nameof(value), "value must be non-negative");
            return new Index(value);
        }

        public static Index FromEnd(int value)
        {
            if (value < 0) throw new ArgumentOutOfRangeException(nameof(value), "value must be non-negative");
            return new Index(~value);
        }

        public int Value => _value < 0 ? ~_value : _value;
        public bool IsFromEnd => _value < 0;

        public int GetOffset(int length)
        {
            var offset = _value;
            if (IsFromEnd) offset += length + 1;
            return offset;
        }

        public static implicit operator Index(int value) => FromStart(value);

        public bool Equals(Index other) => _value == other._value;
        public override bool Equals(object value) => value is Index other && _value == other._value;
        public override int GetHashCode() => _value;
        public override string ToString() => IsFromEnd ? "^" + (uint)Value : ((uint)Value).ToString();
    }

    internal readonly struct Range : IEquatable<Range>
    {
        public Index Start { get; }
        public Index End { get; }

        public Range(Index start, Index end)
        {
            Start = start;
            End = end;
        }

        public bool Equals(Range other) => other.Start.Equals(Start) && other.End.Equals(End);
        public override bool Equals(object value) => value is Range r && r.Start.Equals(Start) && r.End.Equals(End);
        public override int GetHashCode() => Start.GetHashCode() * 31 + End.GetHashCode();
        public override string ToString() => Start + ".." + End;

        public static Range StartAt(Index start) => new Range(start, Index.End);
        public static Range EndAt(Index end) => new Range(Index.Start, end);
        public static Range All => new Range(Index.Start, Index.End);

        public (int Offset, int Length) GetOffsetAndLength(int length)
        {
            int start = Start.GetOffset(length);
            int end = End.GetOffset(length);
            if ((uint)end > (uint)length || (uint)start > (uint)end)
                throw new ArgumentOutOfRangeException(nameof(length));
            return (start, end - start);
        }
    }

    /// <summary>Minimal System.HashCode polyfill. Distribution need not match the BCL;
    /// only stability within a process is required for hashtable use.</summary>
    internal struct HashCode
    {
        private int _hash;
        private bool _init;

        private void EnsureInit()
        {
            if (!_init) { _hash = 17; _init = true; }
        }

        public void Add<T>(T value)
        {
            EnsureInit();
            unchecked { _hash = _hash * 31 + (value?.GetHashCode() ?? 0); }
        }

        public int ToHashCode()
        {
            EnsureInit();
            return _hash;
        }

        private static int H(int seed, int h) { unchecked { return seed * 31 + h; } }
        private static int HC<T>(T v) => v?.GetHashCode() ?? 0;

        public static int Combine<T1>(T1 v1) => H(17, HC(v1));
        public static int Combine<T1, T2>(T1 v1, T2 v2) => H(H(17, HC(v1)), HC(v2));
        public static int Combine<T1, T2, T3>(T1 v1, T2 v2, T3 v3) => H(Combine(v1, v2), HC(v3));
        public static int Combine<T1, T2, T3, T4>(T1 v1, T2 v2, T3 v3, T4 v4) => H(Combine(v1, v2, v3), HC(v4));
        public static int Combine<T1, T2, T3, T4, T5>(T1 v1, T2 v2, T3 v3, T4 v4, T5 v5) => H(Combine(v1, v2, v3, v4), HC(v5));
        public static int Combine<T1, T2, T3, T4, T5, T6>(T1 v1, T2 v2, T3 v3, T4 v4, T5 v5, T6 v6) => H(Combine(v1, v2, v3, v4, v5), HC(v6));
        public static int Combine<T1, T2, T3, T4, T5, T6, T7>(T1 v1, T2 v2, T3 v3, T4 v4, T5 v5, T6 v6, T7 v7) => H(Combine(v1, v2, v3, v4, v5, v6), HC(v7));
        public static int Combine<T1, T2, T3, T4, T5, T6, T7, T8>(T1 v1, T2 v2, T3 v3, T4 v4, T5 v5, T6 v6, T7 v7, T8 v8) => H(Combine(v1, v2, v3, v4, v5, v6, v7), HC(v8));
    }

    /// <summary>Single-precision math polyfill (System.MathF is net5.0+).</summary>
    internal static class MathF
    {
        // x * 2^n. Math.Pow is exact for integer powers of two within normal range.
        public static float ScaleB(float x, int n) => (float)(x * Math.Pow(2.0, n));

        public static float BitIncrement(float x)
        {
            int bits = BitConverter.ToInt32(BitConverter.GetBytes(x), 0);
            if ((bits & 0x7F800000) >= 0x7F800000)
            {
                // +Inf or NaN stays; -Inf -> float.MinValue
                return bits == unchecked((int)0xFF800000) ? float.MinValue : x;
            }
            if (bits == unchecked((int)0x80000000)) // -0.0
                return float.Epsilon;
            bits += bits < 0 ? -1 : 1;
            return BitConverter.ToSingle(BitConverter.GetBytes(bits), 0);
        }
    }
}

namespace System.Collections.Generic
{
    using System.Runtime.CompilerServices;

    /// <summary>Reference-identity equality comparer (System.Collections.Generic.ReferenceEqualityComparer
    /// is net5.0+ public). Shadows the inaccessible ns2.0 internal type (CS0436).</summary>
    internal sealed class ReferenceEqualityComparer : IEqualityComparer<object>, IEqualityComparer
    {
        private ReferenceEqualityComparer() { }
        public static ReferenceEqualityComparer Instance { get; } = new ReferenceEqualityComparer();

        public new bool Equals(object x, object y) => ReferenceEquals(x, y);
        public int GetHashCode(object obj) => RuntimeHelpers.GetHashCode(obj);

        bool IEqualityComparer.Equals(object x, object y) => ReferenceEquals(x, y);
        int IEqualityComparer.GetHashCode(object obj) => RuntimeHelpers.GetHashCode(obj);
    }
}

namespace System.Runtime.InteropServices
{
    /// <summary>Native dynamic-load polyfill (System.Runtime.InteropServices.NativeLibrary is net5.0+).</summary>
    internal static class NativeLibrary
    {
        public static IntPtr Load(string libraryPath)
        {
            var h = PlatformLoad(libraryPath);
            if (h == IntPtr.Zero) throw new DllNotFoundException(libraryPath);
            return h;
        }

        public static bool TryLoad(string libraryPath, out IntPtr handle)
        {
            handle = PlatformLoad(libraryPath);
            return handle != IntPtr.Zero;
        }

        public static void Free(IntPtr handle)
        {
            if (handle != IntPtr.Zero) PlatformFree(handle);
        }

        public static IntPtr GetExport(IntPtr handle, string name)
        {
            var p = PlatformGetExport(handle, name);
            if (p == IntPtr.Zero) throw new EntryPointNotFoundException(name);
            return p;
        }

        public static bool TryGetExport(IntPtr handle, string name, out IntPtr address)
        {
            address = PlatformGetExport(handle, name);
            return address != IntPtr.Zero;
        }

        private static bool IsWindows => RuntimeInformation.IsOSPlatform(OSPlatform.Windows);
        private static IntPtr PlatformLoad(string p) => IsWindows ? Win_LoadLibrary(p) : Unix_dlopen(p, 2 /* RTLD_NOW */);
        private static void PlatformFree(IntPtr h) { if (IsWindows) Win_FreeLibrary(h); else Unix_dlclose(h); }
        private static IntPtr PlatformGetExport(IntPtr h, string n) => IsWindows ? Win_GetProcAddress(h, n) : Unix_dlsym(h, n);

        [DllImport("kernel32", EntryPoint = "LoadLibraryA", SetLastError = true, CharSet = CharSet.Ansi)]
        private static extern IntPtr Win_LoadLibrary(string name);
        [DllImport("kernel32", EntryPoint = "FreeLibrary", SetLastError = true)]
        private static extern bool Win_FreeLibrary(IntPtr handle);
        [DllImport("kernel32", EntryPoint = "GetProcAddress", SetLastError = true, CharSet = CharSet.Ansi)]
        private static extern IntPtr Win_GetProcAddress(IntPtr handle, string name);

        [DllImport("libdl", EntryPoint = "dlopen", CharSet = CharSet.Ansi)]
        private static extern IntPtr Unix_dlopen(string path, int flags);
        [DllImport("libdl", EntryPoint = "dlclose")]
        private static extern int Unix_dlclose(IntPtr handle);
        [DllImport("libdl", EntryPoint = "dlsym", CharSet = CharSet.Ansi)]
        private static extern IntPtr Unix_dlsym(IntPtr handle, string name);
    }
}
#endif
