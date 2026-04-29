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
/// </summary>
public static class AllocCounter
{
    public static readonly bool Enabled =
        System.Environment.GetEnvironmentVariable("DOTCL_ALLOC_PROF") == "1";

    // Per-type counter array. GetOrAdd returns a single-element long[]
    // which we then Interlocked.Increment. Keeps allocations O(unique types).
    private static readonly ConcurrentDictionary<string, long[]> _counters = new();

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
        System.Threading.Interlocked.Increment(ref arr[0]);
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
    }
}
