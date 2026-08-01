using System.Collections.Concurrent;
using System.Runtime.CompilerServices;

namespace DotCL.Diagnostics;

/// <summary>
/// Opt-in phase profiler for the compile pipeline. Accumulates elapsed time and
/// a call count per named phase. Enabled by env var DOTCL_PHASE_PROF=1.
///
/// The point is to answer "where does compiling a large file actually go?"
/// without touching the Lisp compiler: COMPILE-FILE's loop crosses the phase
/// boundaries in C# (read a form, compile it to an instruction list, maybe run
/// it at compile time, hand it to the fasl backend), so timing them here splits
/// the total without the direct-delegate problem that makes wrapping Lisp
/// functions unreliable.
///
/// Nesting is not modelled: each site measures its own span, and sites are
/// chosen so they do not contain one another. Sum of phases ≈ wall time.
///
/// Usage from Lisp:
///   (dotcl:phase-report)          → prints phases, slowest first
///   (dotcl:phase-report-reset)    → zero all phases
///
/// When disabled (default) Time() runs the action with one branch of overhead.
/// </summary>
public static class PhaseTimer
{
    public static readonly bool Enabled =
        System.Environment.GetEnvironmentVariable("DOTCL_PHASE_PROF") == "1";

    private sealed class Bucket { public long Ticks; public long Count; }

    private static readonly ConcurrentDictionary<string, Bucket> _phases = new();

    /// <summary>Run ACTION, charging its elapsed time to PHASE.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T Time<T>(string phase, System.Func<T> action)
    {
        if (!Enabled) return action();
        var start = System.Diagnostics.Stopwatch.GetTimestamp();
        try { return action(); }
        finally { Charge(phase, System.Diagnostics.Stopwatch.GetTimestamp() - start); }
    }

    /// <summary>Void form of <see cref="Time{T}"/>.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Time(string phase, System.Action action)
    {
        if (!Enabled) { action(); return; }
        var start = System.Diagnostics.Stopwatch.GetTimestamp();
        try { action(); }
        finally { Charge(phase, System.Diagnostics.Stopwatch.GetTimestamp() - start); }
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static void Charge(string phase, long ticks)
    {
        var b = _phases.GetOrAdd(phase, _ => new Bucket());
        System.Threading.Interlocked.Add(ref b.Ticks, ticks);
        System.Threading.Interlocked.Increment(ref b.Count);
    }

    public static IReadOnlyList<(string Phase, double Seconds, long Count)> Snapshot()
    {
        var freq = (double)System.Diagnostics.Stopwatch.Frequency;
        var rows = new List<(string, double, long)>(_phases.Count);
        foreach (var kvp in _phases)
            rows.Add((kvp.Key, kvp.Value.Ticks / freq, kvp.Value.Count));
        rows.Sort((a, b) => b.Item2.CompareTo(a.Item2));
        return rows;
    }

    public static void Reset() => _phases.Clear();
}
