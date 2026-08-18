using System.Collections.Concurrent;
using System.Runtime.CompilerServices;

namespace DotCL.Diagnostics;

/// <summary>
/// Opt-in allocation profiler. Counts how many instances of each tracked
/// LispObject subtype are constructed. Enabled by env var DOTCL_ALLOC_PROF=1.
///
/// When disabled (default), Inc() is a single-branch no-op inline expansion,
/// so the overhead on hot-path constructors is negligible.
///
/// Usage from Lisp:
///   (dotcl:alloc-report)          → prints counts, sorted descending
///   (dotcl:alloc-report-reset)    → zero all counters
///
/// Or set DOTCL_ALLOC_PROF=1 and (dotcl:alloc-report) on exit.
///
/// A per-type count says what is being allocated but not who allocates it.
/// DOTCL_ALLOC_STACK=&lt;type&gt; additionally samples the call stack of every
/// DOTCL_ALLOC_STACK_EVERY-th allocation of that one type (default every 64th)
/// and (dotcl:alloc-stacks) prints the most frequent stacks. Compiled Lisp
/// functions are real .NET methods named after the Lisp function, so the frames
/// read as Lisp names. Capturing a stack is expensive; it is why this is
/// restricted to one type and to a sampling interval.
/// </summary>
public static class AllocCounter
{
    public static readonly bool Enabled =
        System.Environment.GetEnvironmentVariable("DOTCL_ALLOC_PROF") == "1";

    // The single type whose allocations get a sampled stack, or null.
    private static readonly string? _stackType =
        System.Environment.GetEnvironmentVariable("DOTCL_ALLOC_STACK");

    private static readonly long _stackEvery = ReadStackEvery();
    private static readonly int _stackDepth = ReadIntEnv("DOTCL_ALLOC_STACK_DEPTH", 12);

    private static long ReadStackEvery()
    {
        var n = ReadIntEnv("DOTCL_ALLOC_STACK_EVERY", 64);
        return n < 1 ? 1 : n;
    }

    private static int ReadIntEnv(string name, int fallback)
    {
        var s = System.Environment.GetEnvironmentVariable(name);
        return int.TryParse(s, out var n) ? n : fallback;
    }

    // Per-type counter array. GetOrAdd returns a single-element long[]
    // which we then Interlocked.Increment. Keeps allocations O(unique types).
    private static readonly ConcurrentDictionary<string, long[]> _counters = new();

    // Sampled stacks for _stackType, keyed by the rendered frame list.
    private static readonly ConcurrentDictionary<string, long[]> _stacks = new();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Inc(string type)
    {
        if (!Enabled) return;
        IncSlow(type);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static void IncSlow(string type)
    {
        var arr = _counters.GetOrAdd(type, _ => new long[1]);
        var n = System.Threading.Interlocked.Increment(ref arr[0]);
        if (_stackType != null && n % _stackEvery == 0 && type == _stackType)
            RecordStack();
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static void RecordStack()
    {
        // skipFrames=2 drops RecordStack and IncSlow; Inc is inlined away.
        // The constructor that allocated is kept as the first frame, since for
        // an overloaded type it says which constructor ran.
        var st = new System.Diagnostics.StackTrace(2, false);
        var sb = new System.Text.StringBuilder();
        int kept = 0;
        for (int i = 0; i < st.FrameCount && kept < _stackDepth; i++)
        {
            var m = st.GetFrame(i)?.GetMethod();
            if (m == null) continue;
            if (kept > 0) sb.Append(" < ");
            // A DynamicMethod (compiled Lisp) has no declaring type; its name
            // is the Lisp function name.
            var owner = m.DeclaringType?.Name;
            if (owner != null) sb.Append(owner).Append('.');
            sb.Append(m.Name);
            kept++;
        }
        var key = sb.ToString();
        var arr = _stacks.GetOrAdd(key, _ => new long[1]);
        System.Threading.Interlocked.Increment(ref arr[0]);
    }

    public static string? StackType => _stackType;
    public static long StackEvery => _stackEvery;

    public static IReadOnlyList<(string Stack, long Count)> StackSnapshot()
    {
        var result = new List<(string, long)>(_stacks.Count);
        foreach (var kvp in _stacks)
            result.Add((kvp.Key, System.Threading.Interlocked.Read(ref kvp.Value[0])));
        result.Sort((a, b) => b.Item2.CompareTo(a.Item2));
        return result;
    }

    public static IReadOnlyList<(string Type, long Count)> Snapshot()
    {
        var result = new List<(string, long)>(_counters.Count);
        foreach (var kvp in _counters)
            result.Add((kvp.Key, System.Threading.Interlocked.Read(ref kvp.Value[0])));
        result.Sort((a, b) => b.Item2.CompareTo(a.Item2));
        return result;
    }

    public static void Reset()
    {
        foreach (var kvp in _counters)
            System.Threading.Interlocked.Exchange(ref kvp.Value[0], 0);
        _stacks.Clear();
    }
}
