using System.Diagnostics;
using System.Text;
using System.Threading;

namespace DotCL;

/// <summary>Counts, per method name the assembler resolves for CALL / CALLVIRT,
/// how often generated code names it and how often that call runs. Off unless
/// DOTCL_CALL_COVERAGE names a directory; each process then writes
/// calls-PID.tsv there on exit. Counts are split into code assembled while the
/// core loads and code assembled after.</summary>
public static class CallCoverage
{
    static readonly string? Dir = Environment.GetEnvironmentVariable("DOTCL_CALL_COVERAGE");
    public static readonly bool Enabled = !string.IsNullOrEmpty(Dir);

    static string[] _names = new string[0];
    static Dictionary<string, int>? _ids;
    static long[]? _emits;
    static long[]? _hits;
    static int _phase;

    internal static void Init(IEnumerable<string> names)
    {
        if (!Enabled || _ids != null) return;
        var sorted = new List<string>(names);
        sorted.Sort(StringComparer.Ordinal);
        _names = sorted.ToArray();
        var ids = new Dictionary<string, int>(StringComparer.Ordinal);
        for (int i = 0; i < _names.Length; i++) ids[_names[i]] = i;
        _emits = new long[_names.Length * 2];
        _hits = new long[_names.Length * 2];
        _ids = ids;
        AppDomain.CurrentDomain.ProcessExit += (_, _) => Dump();
    }

    public static void CoreLoaded() => _phase = 1;

    /// <summary>Record that NAME was emitted; the id to pass to Hit, or -1.</summary>
    internal static int Emitted(string name)
    {
        if (_ids == null || !_ids.TryGetValue(name, out var i)) return -1;
        int id = i + _phase * _names.Length;
        Interlocked.Increment(ref _emits![id]);
        return id;
    }

    public static void Hit(int id)
    {
        var h = _hits;
        if (h != null && (uint)id < (uint)h.Length) Interlocked.Increment(ref h[id]);
    }

    static void Dump()
    {
        if (_emits == null || _hits == null) return;
        try
        {
            Directory.CreateDirectory(Dir!);
            int n = _names.Length;
            var sb = new StringBuilder();
            for (int i = 0; i < n; i++)
                sb.Append(_names[i]).Append('\t')
                  .Append(_emits[i]).Append('\t').Append(_emits[i + n]).Append('\t')
                  .Append(_hits[i]).Append('\t').Append(_hits[i + n]).Append('\n');
            var path = Path.Combine(Dir!, "calls-" + Process.GetCurrentProcess().Id + ".tsv");
            File.WriteAllText(path, sb.ToString());
        }
        catch (Exception e)
        {
            Console.Error.WriteLine(";; call coverage: " + e.Message);
        }
    }
}
